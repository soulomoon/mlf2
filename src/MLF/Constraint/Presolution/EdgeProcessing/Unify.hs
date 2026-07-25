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
    constructEdgeExpansionInstantiate,
    finishEdgeExpansionInstantiateApply,
    bindEdgeExpansionRoot,
    prepareEdgeExpansionOmega,
    executeEdgeExpansionOmega,
    resolveEdgeUnificationTarget,
    finishEdgeExpansionUnify,
    requireExpansionResultScope,
  )
where

{- Note [Edge-local omega execution]
Edge-local unification executes the paper's χe operations (Raise, Merge, Weaken)
around the chosen expansion recipe. The execution order here is:

  1. Apply the expansion (copying nodes + binding the expansion root).
  2. Execute Omega base ops *before* structural unification.
  3. Unify expansion structure with the target (unifyStructureEdge).
  4. Record the administrative TyExp-wrapper replacement without touching UF.
  5. Execute Omega base ops *after* unification.

The Raise/Merge/Weaken steps are recorded by EdgeUnify while χe runs, and are
later integrated with expansion-derived steps into a per-edge witness (Φ).
This aligns with `papers/these-finale-english.txt` (see also `papers/xmlf.txt`
Fig. 10 for Ω/Φ). TyVar/TyExp special cases remain in presolution; shared
structural decomposition lives in `Unify.Decompose`.
-}

{- Note [Thesis-exact weaken execution]
Witness construction emits the paper's `OpWeaken` for unbounded binders on
every edge, including annotation edges. This module executes the supplied
`baseOps` unchanged; it does not classify or suppress weakenings.
-}

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (gets, runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.BindingUtil (firstGenAncestorFrom)
import MLF.Constraint.Presolution.Base
  ( CopyMap,
    EdgeDestinationInterior (..),
    EdgeExecutionArtifacts (..),
    EdgeSourceInterior (..),
    EdgeTrace (..),
    FrontierSet,
    InteriorNodes (..),
    InteriorSet,
    PresolutionError (..),
    PresolutionM,
    PresolutionState
      ( psEdgeExecutionArtifacts,
        psExpansionResults
      ),
    RawExpansionConstruction,
    edgeInteriorExact,
    emptyRawExpansionConstruction,
    getConstraint,
    getCopyMapping,
    lookupExpansionResultUnder,
    lookupCopy,
    pendingWeakenOwnerFromMaybe,
  )
import MLF.Constraint.Presolution.EdgeUnify
  ( EdgeUnifyState (eusOps),
    constructNondegenerateIdentityTerminalRootAuthority,
    constructUncopiedTerminalRootAuthority,
    executeEdgeLocalOmegaOps,
    initEdgeUnifyStateWithCopyMap,
    mkOmegaExecEnv,
    recordExpansionWrapperResult,
    unifyQuotientTerminalStructureEdge,
    unifyStructureEdge,
    unifyTerminalStructureEdge,
  )
import MLF.Constraint.Presolution.Expansion
  ( applyExpansionEdgeTracedAtTargetWithBinders,
    getExpansion,
  )
import MLF.Constraint.Presolution.Copy
  ( instantiateExpansionWithTraceAtTargetSnapshot,
  )
import MLF.Constraint.Presolution.Ops
  ( createFreshVar,
  )
import MLF.Constraint.Presolution.StateAccess
  ( getBindingSnapshot,
    getCanonical,
  )
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import MLF.Constraint.Presolution.Witness
  ( EdgeWitnessOp (..),
  )
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
    -- | Frozen source-domain I(r), captured by the decision before chi_e can
    -- mutate binding or UF state.
    eeiSourceInterior :: EdgeSourceInterior,
    -- | Source nodes that were locked when the frozen source domain was
    -- captured.  Execution must not rediscover this from the mutated graph.
    eeiLockedSourceNodes :: IntSet.IntSet,
    -- | Frozen source nodes with an all-flexible type-parent path to the
    -- expansion root.  Only these identities can own Raise operations.
    eeiSourceRaiseAuthorityNodes :: IntSet.IntSet,
    -- | Nodes that existed before constructing chi_e.  This freezes the
    -- source/destination domain boundary for edge-local witness emission.
    eeiSourceNodeKeys :: IntSet.IntSet,
    -- | Source binders computed during edge planning.
    eeiBoundVars :: [NodeId],
    -- | Binder-to-instantiation-argument pairs chosen while deciding expansion
    eeiBinderArgs :: [(NodeId, NodeId)],
    -- | Structural equalities selected together with the expansion recipe.
    -- They execute in the same edge-local transaction as chi_e so any
    -- cross-scope Raise is retained in this edge's witness.
    eeiStructuralUnifications :: [(NodeId, NodeId)]
  }

-- | Result of applying an expansion and running edge-local unification.
data EdgeExpansionResult = EdgeExpansionResult
  { eerResultRoot :: NodeId,
    eerCopyMap :: CopyMap,
    eerDestinationInterior :: EdgeDestinationInterior,
    eerFrontier :: FrontierSet,
    eerConstruction :: RawExpansionConstruction,
    eerExtraOps :: [EdgeWitnessOp]
  }

data EdgeExpansionApplied = EdgeExpansionApplied
  { eeaInput :: EdgeExpansionInput,
    eeaBaseOps :: [InstanceOp],
    eeaResultNodeId :: NodeId,
    -- | Frozen producer result when this application reuses an existing chi_e.
    -- Execution still validates the current canonical result, while the trace
    -- keeps the authority that constructed the shared destination graph.
    eeaTraceResultRoot :: Maybe NodeId,
    eeaCopyMap :: CopyMap,
    eeaDestinationInterior :: EdgeDestinationInterior,
    eeaFrontier :: FrontierSet,
    eeaConstruction :: RawExpansionConstruction
  }

data EdgeExpansionBound = EdgeExpansionBound
  { eebApplied :: EdgeExpansionApplied,
    eebTargetBinder :: NodeRef,
    eebAllowedResultOwners :: [NodeRef],
    eebCopyMapCanon :: IntMap.IntMap NodeId
  }

data EdgeExpansionPrepared = EdgeExpansionPrepared
  { eepBound :: EdgeExpansionBound,
    eepBinderMetas :: [(NodeId, NodeId)],
    eepExecutionInterior :: EdgeDestinationInterior
  }

data EdgeExpansionExecuted = EdgeExpansionExecuted
  { eexPrepared :: EdgeExpansionPrepared,
    eexExtraOps :: [EdgeWitnessOp]
  }

data EdgeExpansionApplyPlan
  = EdgeExpansionApplyGeneric EdgeExpansionInput [InstanceOp]
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
runExpansionUnify input baseOps =
  case eeiExpansion input of
    ExpIdentity
      | null baseOps -> executeIdentityEdgeUnify input
      | otherwise ->
          throwError
            (IdentityExpansionHasBaseOps (eeiEdgeId input) baseOps)
    _ -> executeEdgeExpansionPipeline input baseOps

-- | Execute the equalities selected for an identity expansion without moving
-- the shared producer scheme to the destination.  The former shortcut ran
-- these equalities in ordinary presolution; that made a legitimate
-- cross-scope lower-bound repair fail before an edge witness existed.  The
-- generic expansion path is not appropriate either: applying ExpIdentity at
-- the target would reparent the producer's scheme root.
executeIdentityEdgeUnify :: EdgeExpansionInput -> PresolutionM p EdgeExpansionResult
executeIdentityEdgeUnify input = do
  identityConstraint <- getConstraint
  canonicalBefore <- getCanonical
  let bodyRoot = eeiBodyRoot input
      EdgeSourceInterior (InteriorNodes interior) = eeiSourceInterior input
      destinationInterior = EdgeDestinationInterior interior
      edgeKey = getEdgeId (eeiEdgeId input)
      isNonAdministrativeEdge =
        IntSet.notMember edgeKey (cAnnEdges identityConstraint)
          && IntSet.notMember edgeKey (cLetEdges identityConstraint)
      sourceSchemeIsNondegenerate =
        not
          ( IntSet.null
              (IntSet.delete (getNodeId bodyRoot) interior)
          )
      sourceAndTargetHaveDistinctBindingParents =
        fmap
          (Canonicalize.canonicalRef canonicalBefore . fst)
          (Binding.lookupBindParent identityConstraint (typeRef bodyRoot))
          /= fmap
            (Canonicalize.canonicalRef canonicalBefore . fst)
            ( Binding.lookupBindParent
                identityConstraint
                (typeRef (tnId (eeiRightRaw input)))
            )
      -- An identity expansion has no copied root, so its source binding
      -- domain is the construction certificate.  Inst-Elim-Mono turns a
      -- degenerate edge into ordinary unification; that unification still
      -- records a Raise when its operands have distinct binding parents.  A
      -- same-parent singleton (the identity lambda body) is quotient-only.
      -- A nondegenerate domain has an additional frozen interior node and may
      -- construct Figure 15.3.4's terminal authority using Section 10.1.2's
      -- binding reset, even after Var-Abs has flattened both operands into one
      -- gen.
      uncopiedRootAuthorityAllowed =
        isNonAdministrativeEdge
          && ( sourceSchemeIsNondegenerate
                 || sourceAndTargetHaveDistinctBindingParents
             )
  eu0 <-
    initEdgeUnifyStateWithCopyMap
      mempty
      (eeiSourceNodeKeys input)
      bodyRoot
      (eeiSourceInterior input)
      (eeiLockedSourceNodes input)
      (eeiSourceRaiseAuthorityNodes input)
      []
      interior
      bodyRoot
      (pendingWeakenOwnerFromMaybe (Just (eeiGenId input)))
  let terminalTarget = tnId (eeiRightRaw input)
  (_a, eu1) <-
    runStateT
      ( do
          when uncopiedRootAuthorityAllowed $
            if sourceSchemeIsNondegenerate
              then constructNondegenerateIdentityTerminalRootAuthority terminalTarget
              else constructUncopiedTerminalRootAuthority terminalTarget
          -- Every structural pair selected for ExpIdentity denotes
          -- corresponding quotient nodes.  Execute each pair through the
          -- children-first terminal seam: bounds must be unified before a
          -- root merge can make either side rigid (Figure 15.3.4).
          forM_
            (eeiStructuralUnifications input)
            (uncurry unifyQuotientTerminalStructureEdge)
      )
      eu0
  canonical <- getCanonical
  pure
    EdgeExpansionResult
      { eerResultRoot = canonical bodyRoot,
        eerCopyMap = mempty,
        eerDestinationInterior = destinationInterior,
        eerFrontier = IntSet.empty,
        eerConstruction = emptyRawExpansionConstruction,
        eerExtraOps = eusOps eu1
      }

-- | Fused expansion pipeline that avoids intermediate record allocations.
-- Takes the raw input and base ops, constructs the expansion at its target,
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
      EdgeDestinationInterior interior0 = eeaDestinationInterior applied
      frontier0 = eeaFrontier applied
      target = eeiRightRaw input
      targetNodeId = tnId target
      leftRaw = eeiLeftRaw input
      gid = eeiGenId input
      bas = eeiBinderArgs input

  -- Step 2: Read the destination ownership established by expansion
  -- construction (inlined from bindEdgeExpansionRoot).
  cBeforeBind <- getConstraint
  let targetParent = Binding.lookupBindParent cBeforeBind (typeRef targetNodeId)
  debugBindParents
    ( "processInstEdge: expansion root bind target="
        ++ show targetNodeId
        ++ " parent="
        ++ show targetParent
    )
  canonical <- getCanonical
  (targetBinder, allowedResultOwners) <-
    constructedExpansionOwnerCertificate canonical cBeforeBind resNodeId
  let copyMapCanon =
        if IntSet.null frontier0
          then IntMap.empty
          else
            IntMap.foldlWithKey'
              (\acc orig copy ->
                IntMap.insert (getNodeId (canonical (NodeId orig))) copy acc)
              IntMap.empty
              (getCopyMapping copyMap0)
  -- Step 3: Prepare omega (inlined from prepareEdgeExpansionOmega)
  binderMetas <- forM bas $ \(bv, _arg) ->
    case lookupCopy bv copyMap0 of
      Just meta -> pure (bv, meta)
      Nothing ->
        throwError (InternalError ("runExpansionUnify: missing binder-meta copy for " ++ show bv))

  -- Reuse `canonical` from above: preparing the copy map does not mutate UF.
  let canonInteriorSet =
        IntSet.fromList
          [ getNodeId (canonical (NodeId i))
          | i <- IntSet.toList interior0
          ]
  interiorExact <- edgeInteriorExact resNodeId
  let interior = IntSet.union canonInteriorSet interiorExact
  semanticTargetNodeId <- resolveEdgeUnificationTarget targetNodeId

  -- Step 4: Execute omega (inlined from executeEdgeExpansionOmega)
  eu0 <-
    initEdgeUnifyStateWithCopyMap
      copyMap0
      (eeiSourceNodeKeys input)
      (eeiBodyRoot input)
      (eeiSourceInterior input)
      (eeiLockedSourceNodes input)
      (eeiSourceRaiseAuthorityNodes input)
      binderMetas
      interior
      resNodeId
      (pendingWeakenOwnerFromMaybe (Just gid))
  let omegaEnv = mkOmegaExecEnv copyMap0
  (_a, eu1) <-
    runStateT
      ( executeEdgeLocalOmegaOps omegaEnv baseOps $ do
          forM_ (eeiStructuralUnifications input) (uncurry unifyStructureEdge)
          forM_ (IntSet.toList frontier0) $ \nidInt ->
            case IntMap.lookup nidInt copyMapCanon of
              Nothing -> pure ()
              Just copy -> unifyStructureEdge copy (NodeId nidInt)
          unifyTerminalStructureEdge resNodeId semanticTargetNodeId
          recordExpansionWrapperResult (tnId leftRaw) resNodeId
      )
      eu0

  -- Step 5: Finish (inlined from finishEdgeExpansionUnify)
  validatedResultRoot <-
    requireExpansionResultScope resNodeId allowedResultOwners
  let resRoot =
        case eeaTraceResultRoot applied of
          Just producerResultRoot -> producerResultRoot
          Nothing -> validatedResultRoot

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

  pure
    EdgeExpansionResult
      { eerResultRoot = resRoot,
        eerCopyMap = copyMap0,
        eerDestinationInterior = EdgeDestinationInterior interior,
        eerFrontier = frontier0,
        eerConstruction = eeaConstruction applied,
        eerExtraOps = eusOps eu1
      }

applyEdgeExpansion ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  PresolutionM p EdgeExpansionApplied
applyEdgeExpansion input baseOps = do
  mbShared <- reuseDestinationOwnedExpansion input baseOps
  case mbShared of
    Just applied -> pure applied
    Nothing -> do
      plan <- prepareEdgeExpansionApply input baseOps
      case plan of
        EdgeExpansionApplyGeneric genericInput genericBaseOps ->
          applyGenericEdgeExpansion genericInput genericBaseOps
        EdgeExpansionApplyInstantiate instantiatePlan -> do
          unifyEdgeExpansionInstantiateArgs instantiatePlan
          binderMetas <- freshEdgeExpansionBinderMetas instantiatePlan
          schemeTrace <- constructEdgeExpansionInstantiate instantiatePlan binderMetas
          finishEdgeExpansionInstantiateApply instantiatePlan schemeTrace

{- Note [One destination-owned chi_e result per occurrence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An expansion variable may occur in more than one instantiation edge, but the
worklist admits only one destination gen for that variable.  When those edges
share the same TyExp occurrence, Definition 10.3.2 constructs one chi_e graph
at that destination: later edges constrain that graph; they do not construct a
second graph and reconcile the two afterwards.

The expansion-result map identifies an already-constructed graph for one
occurrence.  Its producer trace owns the original-to-copy provenance needed by
Omega.  A later occurrence of the same expansion variable may reuse both only
when its target has the same destination gen and its binder arguments agree.
Reusing one occurrence for a different target still reaches
'recordExpansionResult' and remains a hard conflict.
-}
reuseDestinationOwnedExpansion
  :: EdgeExpansionInput
  -> [InstanceOp]
  -> PresolutionM p (Maybe EdgeExpansionApplied)
reuseDestinationOwnedExpansion input baseOps =
  case eeiLeftRaw input of
    TyExp {tnId = wrapper, tnExpVar = expVar} -> do
      canonical <- getCanonical
      st <- gets id
      constraint <- getConstraint
      mbWrapperResult <-
        either throwError pure $
          lookupExpansionResultUnder canonical wrapper (psExpansionResults st)
      let completeArtifacts =
            [ ( trace,
                witness,
                construction
              )
            | artifacts <- IntMap.elems (psEdgeExecutionArtifacts st)
            , let trace = eeaTrace artifacts
                  witness = eeaWitness artifacts
                  construction = eeaExpansionConstruction artifacts
            ]
          exactOccurrenceProducer result =
            [ (trace, result, construction)
            | (trace, witness, construction) <- completeArtifacts,
              ewLeft witness == wrapper,
              canonical (ewRight witness) == canonical (tnId (eeiRightRaw input)),
              compatibleBinderArgs canonical (eeiBinderArgs input) (etBinderArgs trace),
              canonical (etResultRoot trace) == canonical result
            ]
          sharedExpansionProducer =
            [ (trace, etResultRoot trace, construction)
            | (trace, witness, construction) <- completeArtifacts,
              Just TyExp {tnExpVar = producerExpVar} <-
                [lookupNodeIn (cNodes constraint) (ewLeft witness)],
              producerExpVar == expVar,
              sameDestinationGen constraint (ewRight witness) (tnId (eeiRightRaw input)),
              compatibleBinderArgs canonical (eeiBinderArgs input) (etBinderArgs trace)
            ]
          candidates =
            case mbWrapperResult of
              Just result -> exactOccurrenceProducer result
              Nothing -> sharedExpansionProducer
      case candidates of
        (producerTrace, result, construction) : _ -> do
          -- A recorded trace owns source-domain provenance only.  The
          -- already-built chi_e graph has its own destination domain;
          -- recompute that domain from the current frozen binding view
          -- instead of reusing 'etInterior'.
          destinationInterior <- edgeInteriorExact result
          applied <-
            finishEdgeExpansionApply
              input
              baseOps
              result
              (etCopyMap producerTrace, destinationInterior, IntSet.empty)
              construction
          pure (Just applied {eeaTraceResultRoot = Just result})
        [] -> pure Nothing
    _ -> pure Nothing
  where
    sameDestinationGen constraint left right =
      case
        ( firstGenAncestorFrom (cBindParents constraint) (typeRef left),
          firstGenAncestorFrom (cBindParents constraint) (typeRef right)
        ) of
        (Just leftGen, Just rightGen) -> leftGen == rightGen
        _ -> False

    compatibleBinderArgs canonical current prior =
      length current == length prior
        && and
          ( zipWith
              (\(binder, arg) (priorBinder, priorArg) ->
                canonical binder == canonical priorBinder
                  && canonical arg == canonical priorArg
              )
              current
              prior
          )

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
        then pure (EdgeExpansionApplyGeneric input baseOps)
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
        [] -> throwError $ ArityMismatch "applyExpansionEdgeTracedAtTargetWithBinders" (length boundVars) (length args)
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
      throwError $ ArityMismatch "applyExpansionEdgeTracedAtTargetWithBinders" (length boundVars) (length args)
  where
    boundVars = eeiBoundVars input

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
          ( resNodeId
            , (copyMap0, interior0, frontier0)
            , construction
            ) <-
            applyExpansionEdgeTracedAtTargetWithBinders
              gid
              (tnId (eeiRightRaw input))
              expn
              leftRaw
              (eeiBodyRoot input)
              (eeiBoundVars input)
          finishEdgeExpansionApply
            input
            baseOps
            resNodeId
            (copyMap0, interior0, frontier0)
            construction
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

constructEdgeExpansionInstantiate ::
  EdgeExpansionInstantiatePlan ->
  [(NodeId, NodeId)] ->
  PresolutionM p
    ( NodeId,
      CopyMap,
      InteriorSet,
      FrontierSet,
      RawExpansionConstruction
    )
constructEdgeExpansionInstantiate plan binderMetas = do
  snapshot <- getBindingSnapshot
  let input = eeipInput plan
      binderArgs = zip (eeiBoundVars input) (eeipArgs plan)
  ( (expansionRoot, bodyCopyMap, bodyInterior, bodyFrontier),
    (boundCopyMap, boundInterior, boundFrontier),
    construction
    ) <-
    instantiateExpansionWithTraceAtTargetSnapshot
      snapshot
      (eeiGenId input)
      (tnId (eeiRightRaw input))
      (eeiBodyRoot input)
      binderMetas
      binderArgs
  let schemeTrace =
        ( expansionRoot,
          bodyCopyMap <> boundCopyMap,
          IntSet.union bodyInterior boundInterior,
          IntSet.union bodyFrontier boundFrontier,
          construction
        )
  pure schemeTrace

finishEdgeExpansionInstantiateApply ::
  EdgeExpansionInstantiatePlan ->
  (NodeId, CopyMap, InteriorSet, FrontierSet, RawExpansionConstruction) ->
  PresolutionM p EdgeExpansionApplied
finishEdgeExpansionInstantiateApply plan (root, copyMap, interior, frontier, construction) =
  finishEdgeExpansionApply
    (eeipInput plan)
    (eeipBaseOps plan)
    root
    (copyMap, interior, frontier)
    construction

finishEdgeExpansionApply ::
  EdgeExpansionInput ->
  [InstanceOp] ->
  NodeId ->
  (CopyMap, InteriorSet, FrontierSet) ->
  RawExpansionConstruction ->
  PresolutionM p EdgeExpansionApplied
finishEdgeExpansionApply input baseOps resNodeId (copyMap0, interior0, frontier0) construction = do
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
        eeaTraceResultRoot = Nothing,
        eeaCopyMap = copyMap0,
        eeaDestinationInterior = EdgeDestinationInterior interior0,
        eeaFrontier = frontier0,
        eeaConstruction = construction
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
  canonical <- getCanonical
  (targetBinder, allowedResultOwners) <-
    constructedExpansionOwnerCertificate canonical cBeforeBind resNodeId
  let copyMapCanon =
        if IntSet.null frontier0
          then IntMap.empty
          else
            IntMap.foldlWithKey'
              (\acc orig copy ->
                IntMap.insert (getNodeId (canonical (NodeId orig))) copy acc)
              IntMap.empty
              (getCopyMapping copyMap0)
  pure
    EdgeExpansionBound
      { eebApplied = applied,
        eebTargetBinder = targetBinder,
        eebAllowedResultOwners = allowedResultOwners,
        eebCopyMapCanon = copyMapCanon
      }

prepareEdgeExpansionOmega :: EdgeExpansionBound -> PresolutionM p EdgeExpansionPrepared
prepareEdgeExpansionOmega bound = do
  let applied = eebApplied bound
      input = eeaInput applied
      copyMap0 = eeaCopyMap applied
      EdgeDestinationInterior interior0 = eeaDestinationInterior applied
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
        eepBinderMetas = binderMetas,
        eepExecutionInterior = EdgeDestinationInterior interior
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
      binderMetas = eepBinderMetas prepared
      EdgeDestinationInterior interior = eepExecutionInterior prepared
  semanticTargetNodeId <- resolveEdgeUnificationTarget (tnId target)
  eu0 <-
    initEdgeUnifyStateWithCopyMap
      copyMap0
      (eeiSourceNodeKeys input)
      (eeiBodyRoot input)
      (eeiSourceInterior input)
      (eeiLockedSourceNodes input)
      (eeiSourceRaiseAuthorityNodes input)
      binderMetas
      interior
      resNodeId
      (pendingWeakenOwnerFromMaybe (Just gid))
  let omegaEnv = mkOmegaExecEnv copyMap0
  (_a, eu1) <-
    runStateT
      ( executeEdgeLocalOmegaOps omegaEnv baseOps $ do
          forM_ (eeiStructuralUnifications input) (uncurry unifyStructureEdge)
          forM_ (IntSet.toList frontier0) $ \nidInt ->
            case IntMap.lookup nidInt copyMapCanon of
              Nothing -> pure ()
              Just copy -> unifyStructureEdge copy (NodeId nidInt)
          unifyStructureEdge resNodeId semanticTargetNodeId
          recordExpansionWrapperResult (tnId leftRaw) resNodeId
      )
      eu0
  pure
    EdgeExpansionExecuted
      { eexPrepared = prepared,
        eexExtraOps = eusOps eu1
      }

-- | Resolve an administrative target occurrence to the semantic graph root
-- against which χe must be unified.  Identity wrappers collapse directionally
-- to their body; a non-identity wrapper can be used only after its own
-- destination-scoped expansion result has been constructed.  No TyExp wrapper
-- is ever inserted into semantic union-find.
resolveEdgeUnificationTarget :: NodeId -> PresolutionM p NodeId
resolveEdgeUnificationTarget = go IntSet.empty
  where
    go seen node0 = do
      canonical <- getCanonical
      let node = canonical node0
          key = getNodeId node
      if IntSet.member key seen
        then
          throwError
            (InternalError ("cyclic administrative expansion target: " ++ show node))
        else do
          constraint <- getConstraint
          case lookupNodeIn (cNodes constraint) node of
            Nothing -> throwError (NodeLookupFailed node)
            Just TyExp {tnExpVar = expVar, tnBody = body} -> do
              expansion <- getExpansion expVar
              case expansion of
                ExpIdentity -> go (IntSet.insert key seen) body
                _ -> do
                  expansionResults <- gets psExpansionResults
                  mbResult <-
                    either throwError pure $
                      lookupExpansionResultUnder canonical node expansionResults
                  case mbResult of
                    Just result -> go (IntSet.insert key seen) result
                    Nothing -> throwError (MissingExpansionResult node expVar)
            Just _ -> pure node

finishEdgeExpansionUnify :: EdgeExpansionExecuted -> PresolutionM p EdgeExpansionResult
finishEdgeExpansionUnify executed = do
  let prepared = eexPrepared executed
      bound = eepBound prepared
      applied = eebApplied bound
      resNodeId = eeaResultNodeId applied
      copyMap0 = eeaCopyMap applied
      EdgeDestinationInterior interior = eepExecutionInterior prepared
      frontier0 = eeaFrontier applied
      targetBinder = eebTargetBinder bound
      allowedResultOwners = eebAllowedResultOwners bound
  validatedResultRoot <-
    requireExpansionResultScope resNodeId allowedResultOwners
  let resRoot =
        case eeaTraceResultRoot applied of
          Just producerResultRoot -> producerResultRoot
          Nothing -> validatedResultRoot

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

  pure
    EdgeExpansionResult
      { eerResultRoot = resRoot,
        eerCopyMap = copyMap0,
        eerDestinationInterior = EdgeDestinationInterior interior,
        eerFrontier = frontier0,
        eerConstruction = eeaConstruction applied,
        eerExtraOps = eexExtraOps executed
      }

-- | Verify that Ω only raised the constructed expansion result along the
-- destination owner's original path to the binding root.
--
-- Exact destination ownership is a construction invariant checked by
-- 'requireDestinationOwnedRoot'.  It is deliberately not a post-Ω invariant:
-- solving a frontier equality may collapse a degenerate destination-owned
-- Bottom back into an exterior source node, which legally raises the result to
-- an ancestor scope.  Freezing the original path rejects sideways/downward
-- movement without forbidding that paper-required Raise.
requireExpansionResultScope :: NodeId -> [NodeRef] -> PresolutionM p NodeId
requireExpansionResultScope result allowedOwners0 = do
  constraint <- getConstraint
  canonical <- getCanonical
  quotient <-
    case Binding.quotientBindParentsContextUnder canonical constraint of
      Left err -> throwError (BindingTreeError err)
      Right value -> pure value
  let resultRoot = canonical result
      resultRef = typeRef resultRoot
      bindParents = Binding.qbpBindParents quotient
      allowedOwnerKeys =
        IntSet.fromList
          [ nodeRefKey (Canonicalize.canonicalRef canonical owner)
          | owner <- allowedOwners0
          ]
  _ <-
    case Binding.bindingPathToRootLocal bindParents resultRef of
      Left err -> throwError (BindingTreeError err)
      Right path -> pure path
  case IntMap.lookup (nodeRefKey resultRef) bindParents of
    Just (actualParent, _flag)
      | IntSet.member (nodeRefKey actualParent) allowedOwnerKeys -> pure resultRoot
      | otherwise ->
          throwError $
            InternalError $
              "edge expansion result moved outside its construction-owner ancestor path: "
                ++ show (resultRoot, actualParent, allowedOwners0)
    Nothing ->
      throwError
        (BindingTreeError (MissingBindParent resultRef))

-- | Read the destination owner installed by the edge expansion constructor
-- and freeze its pre-Ω ancestor path.  A missing parent here is an impossible
-- partially-constructed χe result, not an invitation to repair ownership
-- after copied bounds have been installed.
constructedExpansionOwnerCertificate
  :: (NodeId -> NodeId)
  -> Constraint p
  -> NodeId
  -> PresolutionM p (NodeRef, [NodeRef])
constructedExpansionOwnerCertificate canonical constraint expansionRoot = do
  let root = canonical expansionRoot
      rootRef = typeRef root
      canonicalRef = Canonicalize.canonicalRef canonical
  parentRaw <-
    case Binding.lookupBindParent constraint rootRef of
      Just (owner, _flag) -> pure owner
      Nothing ->
        throwError
          ( InternalError
              ( "edge expansion constructor left its root unowned: "
                  ++ show root
              )
          )
  ownerPathRaw <-
    case Binding.bindingPathToRoot constraint parentRaw of
      Left err -> throwError (BindingTreeError err)
      Right path -> pure path
  pure (canonicalRef parentRaw, map canonicalRef ownerPathRaw)

-- | Debug binding operations (uses explicit trace config).
debugBindParents :: String -> PresolutionM p ()
debugBindParents msg = do
  cfg <- ask
  traceBindingM cfg msg
