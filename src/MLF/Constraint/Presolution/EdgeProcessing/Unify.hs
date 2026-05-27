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
    runExpansionUnify,
    applyEdgeExpansion,
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
  )
import MLF.Constraint.Presolution.Ops
  ( findRoot,
    setBindParentM,
  )
import MLF.Constraint.Presolution.StateAccess (getCanonical)
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

-- | Apply an expansion and run edge-local unification for a single edge.
runExpansionUnify ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionResult
runExpansionUnify input baseOps = do
  applied <- applyEdgeExpansion input baseOps
  bound <- bindEdgeExpansionRoot applied
  prepared <- prepareEdgeExpansionOmega bound
  executed <- executeEdgeExpansionOmega prepared
  finishEdgeExpansionUnify executed

applyEdgeExpansion ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionApplied
applyEdgeExpansion input baseOps =
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
        _ ->
          throwError (InternalError ("runExpansionUnify: expected TyExp for edge " ++ show edgeId))

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
        IntMap.fromListWith
          const
          [ (getNodeId (canonical (NodeId orig)), copy)
          | (orig, copy) <- IntMap.toList (getCopyMapping copyMap0)
          ]
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

  c1 <- getConstraint
  case Binding.lookupBindParent c1 (typeRef resRoot) of
    Nothing -> setBindParentIfUpper resRoot targetBinder
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
