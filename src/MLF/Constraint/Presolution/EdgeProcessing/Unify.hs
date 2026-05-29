-- |
-- Module      : MLF.Constraint.Presolution.EdgeProcessing.Unify
-- Description : Edge-local unification helpers for presolution
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Helpers for applying expansions and executing edge-local unification.
module MLF.Constraint.Presolution.EdgeProcessing.Unify
  ( EdgeExpansionInput (..),
    EdgeExpansionResult (..),
    EdgeExpansionApplied (..),
    EdgeExpansionBound (..),
    EdgeExpansionPrepared (..),
    EdgeExpansionExecuted (..),
    EdgeExpansionApplyPlan (..),
    EdgeExpansionInstantiatePlan (..),
    runExpansionUnify,
    executeEdgeExpansionPipeline,
    applyEdgeExpansion,
    prepareEdgeExpansionApply,
    applyGenericEdgeExpansion,
    unifyEdgeExpansionInstantiateArgs,
    freshEdgeExpansionBinderMetas,
    instantiateEdgeExpansionScheme,
    copyEdgeExpansionBinderBounds,
    finishEdgeExpansionInstantiateApply,
    bindEdgeExpansionRoot,
    prepareEdgeExpansionOmega,
    executeEdgeExpansionOmega,
    finishEdgeExpansionUnify,
    setBindParentIfUpper,
  )
where

{- Note [Edge-local omega execution]
Edge-local unification executes the paper's χe operations (Raise, Merge, Weaken)
around the chosen expansion recipe. The execution order here is:

  1. Apply the expansion (copying nodes + binding the expansion root).
  2. Execute Omega base ops *before* structural unification.
  3. Unify expansion structure with the target (unifyStructureEdge).
  4. Unify the edge root with the expansion result (unifyAcyclicEdge).
  5. Execute Omega base ops *after* unification.

The Raise/Merge/Weaken steps are recorded by EdgeUnify while χe runs, and are
later integrated with expansion-derived steps into a per-edge witness (Φ).
This aligns with `papers/these-finale-english.txt` (see also `papers/xmlf.txt`
Fig. 10 for Ω/Φ). TyVar/TyExp special cases remain in presolution; shared
structural decomposition lives in `Unify.Decompose`.
-}

{- Note [Weaken suppression]
Annotation edges suppress OpWeaken steps (see EdgeProcessing + Witness). This
module assumes `baseOps` already reflect that decision; it only executes the
ops it is given. Keep this invariant intact when extending edge-local unification.
-}

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.Base
  ( CopyMap,
    FrontierSet,
    InteriorSet,
    PresolutionError (..),
    PresolutionM,
    bindExpansionArgs,
    edgeInteriorExact,
    emptyTrace,
    getConstraint,
    getCopyMapping,
    lookupCopy,
    pendingWeakenOwnerFromMaybe,
  )
import MLF.Constraint.Presolution.EdgeUnify
  ( EdgeUnifyState (eusOps),
    executeEdgeLocalOmegaOps,
    initEdgeUnifyState,
    mkOmegaExecEnv,
    unifyAcyclicEdge,
    unifyStructureEdge,
  )
import MLF.Constraint.Presolution.Expansion
  ( applyExpansionEdgeTracedWithBinders,
    bindExpansionRootLikeTarget,
    copyBinderBounds,
    instantiateSchemeWithTrace,
  )
import MLF.Constraint.Presolution.Ops
  ( createFreshVar,
    findRoot,
    setBindParentM,
  )
import MLF.Constraint.Presolution.StateAccess (getCanonical)
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Util.Trace (traceBindingM)

-- | Input bundle for edge-expansion unification.
data EdgeExpansionInput = EdgeExpansionInput
  { -- | Gen-node owning this edge
    eeiGenId :: GenNodeId,
    -- | The edge being processed
    eeiEdgeId :: EdgeId,
    -- | Raw type at the source node
    eeiLeftRaw :: TyNode,
    -- | Raw type at the target node
    eeiRightRaw :: TyNode,
    -- | Expansion recipe for this edge
    eeiExpansion :: Expansion,
    -- | Root of the source scheme body computed during edge planning.
    eeiBodyRoot :: NodeId,
    -- | Source binders computed during edge planning.
    eeiBoundVars :: [NodeId],
    -- | Binder-to-instantiation-argument pairs chosen while deciding expansion
    eeiBinderArgs :: [(NodeId, NodeId)]
  }

-- | Result of applying an expansion and running edge-local unification.
data EdgeExpansionResult = EdgeExpansionResult
  { eerTrace :: (CopyMap, InteriorSet, FrontierSet),
    eerExtraOps :: [InstanceOp]
  }

data EdgeExpansionApplied = EdgeExpansionApplied
  { eeaInput :: EdgeExpansionInput,
    eeaBaseOps :: [InstanceOp],
    eeaResultNodeId :: NodeId,
    eeaCopyMap :: CopyMap,
    eeaInterior :: InteriorSet,
    eeaFrontier :: FrontierSet
  }

data EdgeExpansionBound = EdgeExpansionBound
  { eebApplied :: EdgeExpansionApplied,
    eebTargetBinder :: NodeRef,
    eebCopyMapCanon :: IntMap.IntMap NodeId
  }

data EdgeExpansionPrepared = EdgeExpansionPrepared
  { eepBound :: EdgeExpansionBound,
    eepBinderArgs :: [(NodeId, NodeId)],
    eepBinderMetas :: [(NodeId, NodeId)],
    eepInterior :: InteriorSet
  }

data EdgeExpansionExecuted = EdgeExpansionExecuted
  { eexPrepared :: EdgeExpansionPrepared,
    eexExtraOps :: [InstanceOp]
  }

data EdgeExpansionApplyPlan
  = EdgeExpansionApplyReady EdgeExpansionApplied
  | EdgeExpansionApplyGeneric EdgeExpansionInput [InstanceOp]
  | EdgeExpansionApplyInstantiate EdgeExpansionInstantiatePlan

data EdgeExpansionInstantiatePlan = EdgeExpansionInstantiatePlan
  { eeipInput :: EdgeExpansionInput,
    eeipBaseOps :: [InstanceOp],
    eeipArgs :: [NodeId],
    eeipArgUnifications :: [(NodeId, NodeId)]
  }

-- | Apply an expansion and run edge-local unification for a single edge.
runExpansionUnify ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionResult
runExpansionUnify = executeEdgeExpansionPipeline

-- | Fused expansion pipeline that avoids intermediate record allocations.
-- Takes the raw input and base ops, applies the expansion, binds the root,
-- prepares and executes omega ops, and finishes unification -- all without
-- constructing EdgeExpansionBound, EdgeExpansionPrepared, or EdgeExpansionExecuted.
executeEdgeExpansionPipeline ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionResult
executeEdgeExpansionPipeline input baseOps = do
  -- Step 1: Apply expansion (complex branching; keep as a separate call)
  applied <- applyEdgeExpansion input baseOps

  -- Extract fields from applied into local bindings (avoids repeated accessor chains)
  let resNodeId = eeaResultNodeId applied
      copyMap0 = eeaCopyMap applied
      interior0 = eeaInterior applied
      frontier0 = eeaFrontier applied
      target = eeiRightRaw input
      targetNodeId = tnId target
      leftRaw = eeiLeftRaw input
      gid = eeiGenId input
      bas = eeiBinderArgs input

  -- Step 2: Bind expansion root (inlined from bindEdgeExpansionRoot)
  cBeforeBind <- getConstraint
  let targetParent = Binding.lookupBindParent cBeforeBind (typeRef targetNodeId)
  debugBindParents
    ( "processInstEdge: expansion root bind target="
        ++ show targetNodeId
        ++ " parent="
        ++ show targetParent
    )
  targetBinder <- bindExpansionRootLikeTarget resNodeId targetNodeId

  canonical <- getCanonical
  let copyMapCanon =
        IntMap.foldlWithKey'
          (\acc orig copy ->
            IntMap.insert (getNodeId (canonical (NodeId orig))) copy acc)
          IntMap.empty
          (getCopyMapping copyMap0)
  forM_ (IntSet.toList frontier0) $ \nidInt ->
    case IntMap.lookup nidInt copyMapCanon of
      Nothing -> pure ()
      Just copy -> setBindParentIfUpper copy targetBinder

  -- Step 3: Prepare omega (inlined from prepareEdgeExpansionOmega)
  binderMetas <- forM bas $ \(bv, _arg) ->
    case lookupCopy bv copyMap0 of
      Just meta -> pure (bv, meta)
      Nothing ->
        throwError (InternalError ("runExpansionUnify: missing binder-meta copy for " ++ show bv))

  -- Reuse `canonical` from above: no UF mutation has occurred since line 210
  -- (setBindParentIfUpper only modifies constraint, not UF).
  let canonInteriorSet =
        IntSet.fromList
          [ getNodeId (canonical (NodeId i))
          | i <- IntSet.toList interior0
          ]
  interiorExact <- edgeInteriorExact resNodeId
  let interior = IntSet.union canonInteriorSet interiorExact

  -- Step 4: Execute omega (inlined from executeEdgeExpansionOmega)
  eu0 <- initEdgeUnifyState binderMetas interior resNodeId (pendingWeakenOwnerFromMaybe (Just gid))
  let omegaEnv = mkOmegaExecEnv copyMap0
  (_a, eu1) <-
    runStateT
      ( executeEdgeLocalOmegaOps omegaEnv baseOps $ do
          bindExpansionArgs resNodeId bas
          forM_ (IntSet.toList frontier0) $ \nidInt ->
            case IntMap.lookup nidInt copyMapCanon of
              Nothing -> pure ()
              Just copy -> unifyStructureEdge copy (NodeId nidInt)
          unifyStructureEdge resNodeId (tnId target)
          unifyAcyclicEdge (tnId leftRaw) resNodeId
      )
      eu0

  -- Step 5: Finish (inlined from finishEdgeExpansionUnify)
  resRoot <- findRoot resNodeId
  setBindParentIfUpper resRoot targetBinder
  setBindParentIfUpper (tnId leftRaw) targetBinder

  cAfterBind <- getConstraint
  let resParent = Binding.lookupBindParent cAfterBind (typeRef resRoot)
  debugBindParents
    ( "processInstEdge: expansion root bound resRoot="
        ++ show resRoot
        ++ " parent="
        ++ show resParent
        ++ " targetBinder="
        ++ show targetBinder
    )

  -- Reuse `cAfterBind`: debugBindParents does not modify state.
  -- Inline setBindParentIfUpper to avoid re-reading getConstraint.
  case Binding.lookupBindParent cAfterBind (typeRef resRoot) of
    Nothing ->
      when (Binding.isUpper cAfterBind targetBinder (TypeRef resRoot)) $
        setBindParentM (TypeRef resRoot) (targetBinder, BindFlex)
    Just _ -> pure ()

  pure
    EdgeExpansionResult
      { eerTrace = (copyMap0, interior, frontier0),
        eerExtraOps = eusOps eu1
      }

applyEdgeExpansion ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionApplied
applyEdgeExpansion input baseOps = do
  plan <- prepareEdgeExpansionApply input baseOps
  case plan of
    EdgeExpansionApplyReady applied ->
      pure applied
    EdgeExpansionApplyGeneric genericInput genericBaseOps ->
      applyGenericEdgeExpansion genericInput genericBaseOps
    EdgeExpansionApplyInstantiate instantiatePlan -> do
      unifyEdgeExpansionInstantiateArgs instantiatePlan
      binderMetas <- freshEdgeExpansionBinderMetas instantiatePlan
      schemeTrace <- instantiateEdgeExpansionScheme instantiatePlan binderMetas
      boundsTrace <- copyEdgeExpansionBinderBounds instantiatePlan binderMetas
      finishEdgeExpansionInstantiateApply instantiatePlan schemeTrace boundsTrace

prepareEdgeExpansionApply ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionApplyPlan
prepareEdgeExpansionApply input baseOps =
  case eeiLeftRaw input of
    TyExp {}
      | ExpInstantiate args <- eeiExpansion input ->
          prepareEdgeExpansionInstantiateApply input baseOps args
      | otherwise ->
          pure (EdgeExpansionApplyGeneric input baseOps)
    _ ->
      throwError (InternalError ("runExpansionUnify: expected TyExp for edge " ++ show (eeiEdgeId input)))

prepareEdgeExpansionInstantiateApply ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  [NodeId] ->
  PresolutionM p EdgeExpansionApplyPlan
prepareEdgeExpansionInstantiateApply input baseOps args
  | null boundVars =
      if null args
        then do
          applied <- finishEdgeExpansionApply input baseOps bodyRoot emptyTrace
          pure (EdgeExpansionApplyReady applied)
        else throwError $ InstantiateOnNonForall (tnId (eeiLeftRaw input))
  | length boundVars == length args =
      pure $
        EdgeExpansionApplyInstantiate
          EdgeExpansionInstantiatePlan
            { eeipInput = input,
              eeipBaseOps = baseOps,
              eeipArgs = args,
              eeipArgUnifications = []
            }
  | length boundVars == 1 && length args > 1 =
      case args of
        [] -> throwError $ ArityMismatch "applyExpansionEdgeTracedWithBinders" (length boundVars) (length args)
        arg0 : rest ->
          pure $
            EdgeExpansionApplyInstantiate
              EdgeExpansionInstantiatePlan
                { eeipInput = input,
                  eeipBaseOps = baseOps,
                  eeipArgs = [arg0],
                  eeipArgUnifications = [(arg0, arg) | arg <- rest]
                }
  | otherwise =
      throwError $ ArityMismatch "applyExpansionEdgeTracedWithBinders" (length boundVars) (length args)
  where
    boundVars = eeiBoundVars input
    bodyRoot = eeiBodyRoot input

applyGenericEdgeExpansion ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionApplied
applyGenericEdgeExpansion input baseOps =
  let gid = eeiGenId input
      edgeId = eeiEdgeId input
      leftRaw = eeiLeftRaw input
      expn = eeiExpansion input
   in case leftRaw of
        TyExp {tnBody = _bodyId} -> do
          (resNodeId, (copyMap0, interior0, frontier0)) <-
            applyExpansionEdgeTracedWithBinders
              gid
              expn
              leftRaw
              (eeiBodyRoot input)
              (eeiBoundVars input)
          finishEdgeExpansionApply input baseOps resNodeId (copyMap0, interior0, frontier0)
        _ ->
          throwError (InternalError ("runExpansionUnify: expected TyExp for edge " ++ show edgeId))

unifyEdgeExpansionInstantiateArgs :: EdgeExpansionInstantiatePlan -> PresolutionM p ()
unifyEdgeExpansionInstantiateArgs plan =
  forM_ (eeipArgUnifications plan) (uncurry unifyAcyclic)

freshEdgeExpansionBinderMetas :: EdgeExpansionInstantiatePlan -> PresolutionM p [(NodeId, NodeId)]
freshEdgeExpansionBinderMetas plan = do
  let boundVars = eeiBoundVars (eeipInput plan)
  metas <- forM boundVars $ \bv -> do
    meta <- createFreshVar
    pure (bv, meta)
  pure metas

instantiateEdgeExpansionScheme ::
  EdgeExpansionInstantiatePlan ->
  [(NodeId, NodeId)] ->
  PresolutionM p (NodeId, CopyMap, InteriorSet, FrontierSet)
instantiateEdgeExpansionScheme plan binderMetas =
  instantiateSchemeWithTrace (eeiBodyRoot (eeipInput plan)) binderMetas

copyEdgeExpansionBinderBounds ::
  EdgeExpansionInstantiatePlan ->
  [(NodeId, NodeId)] ->
  PresolutionM p (CopyMap, InteriorSet, FrontierSet)
copyEdgeExpansionBinderBounds plan binderMetas =
  copyBinderBounds binderMetas binderArgs
  where
    boundVars = eeiBoundVars (eeipInput plan)
    binderArgs = zip boundVars (eeipArgs plan)

finishEdgeExpansionInstantiateApply ::
  EdgeExpansionInstantiatePlan ->
  (NodeId, CopyMap, InteriorSet, FrontierSet) ->
  (CopyMap, InteriorSet, FrontierSet) ->
  PresolutionM p EdgeExpansionApplied
finishEdgeExpansionInstantiateApply plan (root, cmap0, interior0, frontier0) (cmapB, interiorB, frontierB) =
  finishEdgeExpansionApply
    (eeipInput plan)
    (eeipBaseOps plan)
    root
    (cmap0 <> cmapB, IntSet.union interior0 interiorB, IntSet.union frontier0 frontierB)

finishEdgeExpansionApply ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  NodeId ->
  (CopyMap, InteriorSet, FrontierSet) ->
  PresolutionM p EdgeExpansionApplied
finishEdgeExpansionApply input baseOps resNodeId (copyMap0, interior0, frontier0) = do
  debugBindParents
    ( "processInstEdge: expansion result resNodeId="
        ++ show resNodeId
        ++ " copyMap0="
        ++ show copyMap0
        ++ " frontier0="
        ++ show frontier0
    )
  pure
    EdgeExpansionApplied
      { eeaInput = input,
        eeaBaseOps = baseOps,
        eeaResultNodeId = resNodeId,
        eeaCopyMap = copyMap0,
        eeaInterior = interior0,
        eeaFrontier = frontier0
      }

bindEdgeExpansionRoot :: EdgeExpansionApplied -> PresolutionM p EdgeExpansionBound
bindEdgeExpansionRoot applied = do
  let input = eeaInput applied
      target = eeiRightRaw input
      targetNodeId = tnId target
      resNodeId = eeaResultNodeId applied
      copyMap0 = eeaCopyMap applied
      frontier0 = eeaFrontier applied
  cBeforeBind <- getConstraint
  let targetParent = Binding.lookupBindParent cBeforeBind (typeRef targetNodeId)
  debugBindParents
    ( "processInstEdge: expansion root bind target="
        ++ show targetNodeId
        ++ " parent="
        ++ show targetParent
    )
  targetBinder <- bindExpansionRootLikeTarget resNodeId targetNodeId

  canonical <- getCanonical
  let copyMapCanon =
        IntMap.foldlWithKey'
          (\acc orig copy ->
            IntMap.insert (getNodeId (canonical (NodeId orig))) copy acc)
          IntMap.empty
          (getCopyMapping copyMap0)
  forM_ (IntSet.toList frontier0) $ \nidInt ->
    case IntMap.lookup nidInt copyMapCanon of
      Nothing -> pure ()
      Just copy -> setBindParentIfUpper copy targetBinder
  pure
    EdgeExpansionBound
      { eebApplied = applied,
        eebTargetBinder = targetBinder,
        eebCopyMapCanon = copyMapCanon
      }

prepareEdgeExpansionOmega :: EdgeExpansionBound -> PresolutionM p EdgeExpansionPrepared
prepareEdgeExpansionOmega bound = do
  let applied = eebApplied bound
      input = eeaInput applied
      copyMap0 = eeaCopyMap applied
      interior0 = eeaInterior applied
      resNodeId = eeaResultNodeId applied
      bas = eeiBinderArgs input
  binderMetas <- forM bas $ \(bv, _arg) ->
    case lookupCopy bv copyMap0 of
      Just meta -> pure (bv, meta)
      Nothing ->
        throwError (InternalError ("runExpansionUnify: missing binder-meta copy for " ++ show bv))

  canonInterior <- getCanonical
  let canonInteriorSet =
        IntSet.fromList
          [ getNodeId (canonInterior (NodeId i))
          | i <- IntSet.toList interior0
          ]
  interiorExact <- edgeInteriorExact resNodeId
  let interior = IntSet.union canonInteriorSet interiorExact
  pure
    EdgeExpansionPrepared
      { eepBound = bound,
        eepBinderArgs = bas,
        eepBinderMetas = binderMetas,
        eepInterior = interior
      }

executeEdgeExpansionOmega :: EdgeExpansionPrepared -> PresolutionM p EdgeExpansionExecuted
executeEdgeExpansionOmega prepared = do
  let bound = eepBound prepared
      applied = eebApplied bound
      input = eeaInput applied
      gid = eeiGenId input
      leftRaw = eeiLeftRaw input
      target = eeiRightRaw input
      baseOps = eeaBaseOps applied
      resNodeId = eeaResultNodeId applied
      copyMap0 = eeaCopyMap applied
      frontier0 = eeaFrontier applied
      copyMapCanon = eebCopyMapCanon bound
      bas = eepBinderArgs prepared
      binderMetas = eepBinderMetas prepared
      interior = eepInterior prepared
  eu0 <- initEdgeUnifyState binderMetas interior resNodeId (pendingWeakenOwnerFromMaybe (Just gid))
  let omegaEnv = mkOmegaExecEnv copyMap0
  (_a, eu1) <-
    runStateT
      ( executeEdgeLocalOmegaOps omegaEnv baseOps $ do
          bindExpansionArgs resNodeId bas
          forM_ (IntSet.toList frontier0) $ \nidInt ->
            case IntMap.lookup nidInt copyMapCanon of
              Nothing -> pure ()
              Just copy -> unifyStructureEdge copy (NodeId nidInt)
          unifyStructureEdge resNodeId (tnId target)
          unifyAcyclicEdge (tnId leftRaw) resNodeId
      )
      eu0
  pure
    EdgeExpansionExecuted
      { eexPrepared = prepared,
        eexExtraOps = eusOps eu1
      }

finishEdgeExpansionUnify :: EdgeExpansionExecuted -> PresolutionM p EdgeExpansionResult
finishEdgeExpansionUnify executed = do
  let prepared = eexPrepared executed
      bound = eepBound prepared
      applied = eebApplied bound
      input = eeaInput applied
      leftRaw = eeiLeftRaw input
      resNodeId = eeaResultNodeId applied
      copyMap0 = eeaCopyMap applied
      interior = eepInterior prepared
      frontier0 = eeaFrontier applied
      targetBinder = eebTargetBinder bound
  resRoot <- findRoot resNodeId
  setBindParentIfUpper resRoot targetBinder
  setBindParentIfUpper (tnId leftRaw) targetBinder

  cAfterBind <- getConstraint
  let resParent = Binding.lookupBindParent cAfterBind (typeRef resRoot)
  debugBindParents
    ( "processInstEdge: expansion root bound resRoot="
        ++ show resRoot
        ++ " parent="
        ++ show resParent
        ++ " targetBinder="
        ++ show targetBinder
    )

  -- Reuse `cAfterBind`: debugBindParents does not modify state.
  -- Inline setBindParentIfUpper to avoid re-reading getConstraint.
  case Binding.lookupBindParent cAfterBind (typeRef resRoot) of
    Nothing ->
      when (Binding.isUpper cAfterBind targetBinder (TypeRef resRoot)) $
        setBindParentM (TypeRef resRoot) (targetBinder, BindFlex)
    Just _ -> pure ()

  pure
    EdgeExpansionResult
      { eerTrace = (copyMap0, interior, frontier0),
        eerExtraOps = eexExtraOps executed
      }

-- | Set a binding parent if the parent is upper than the child in the binding tree.
setBindParentIfUpper :: NodeId -> NodeRef -> PresolutionM p ()
setBindParentIfUpper child parent = do
  cBind <- getConstraint
  when (Binding.isUpper cBind parent (TypeRef child)) $
    setBindParentM (TypeRef child) (parent, BindFlex)

-- | Debug binding operations (uses explicit trace config).
debugBindParents :: String -> PresolutionM p ()
debugBindParents msg = do
  cfg <- ask
  traceBindingM cfg msg
