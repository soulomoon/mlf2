{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module MLF.Constraint.Presolution.TestSupport (
    EdgeWitnessOp(..),
    EdgeArtifact,
    EdgeArtifacts,
    EdgeArtifactsError(..),
    edgeArtifactExpansion,
    edgeArtifactWitness,
    edgeArtifactTrace,
    edgeArtifactExpansionConstruction,
    eaEdgeExpansions,
    eaEdgeWitnesses,
    eaEdgeTraces,
    eaEdgeExpansionConstructions,
    eaIdentityEdges,
    mkEdgeArtifacts,
    emptyEdgeArtifacts,
    lookupEdgeArtifact,
    edgeArtifactsForTest,
    insertEdgeArtifactForTest,
    setEdgeArtifactExpansionForTest,
    setEdgeArtifactWitnessForTest,
    setEdgeArtifactTraceForTest,
    deleteEdgeArtifactForTest,
    setEdgeArtifactsIdentityEdges,
    TranslatabilityIssue(..),
    ExpansionResultMap(..),
    PresolutionState
        ( PresolutionState
        , psConstraint
        , psPresolution
        , psUnionFind
        , psNextNodeId
        , psPendingWeakens
        , psPendingWeakenOwners
        , psWeakenReplayCertificates
        , psBinderCache
        , psGraphVersion
        , psUnionFindVersion
        , psBindParentsVersion
        , psBindingModelCache
        , psEdgeLocalSnapshot
        , psBindingRepairCache
        , psBindingRepairDirty
        , psCachedRootGen
        , psExpansionResults
        , psEdgeExecutionArtifacts
        ),
    EdgeExecutionArtifacts(..),
    EdgeWitnessNonSourceOrigin(..),
    PresolutionUf(..),
    prUnionFind,
    emptyPresolutionStateForTest,
    psEdgeExpansions,
    psEdgeWitnesses,
    psEdgeRaiseAuthorityNodes,
    psEdgeNonSourceOpOrigins,
    psEdgeExpansionConstructions,
    psEdgeTraces,
    emptyExpansionResultMap,
    lookupExpansionResult,
    lookupExpansionResultUnder,
    canonicalizeExpansionResultMap,
    emptyBindingRepairDirty,
    CopyMapping(..),
    CopyMap,
    lookupCopy,
    insertCopy,
    copiedNodes,
    originalNodes,
    InteriorNodes(..),
    EdgeSourceInterior(..),
    sourceInteriorFromList,
    sourceInteriorFromSet,
    fromListInterior,
    toListInterior,
    emptyRawExpansionConstruction,
    runPresolutionM,
    defaultPlanBuilder,
    decideMinimalExpansion,
    processInstEdge,
    runPresolutionLoopWithOperationTimingForTest,
    runIdentityExpansionWithBaseOpsForTest,
    runIdentityStructuralUnificationsForTest,
    edgeExpansionExtraOpsForTest,
    requireExpansionResultScopeForTest,
    validateReplayMapTraceContract,
    unifyAcyclic,
    unifyAcyclicRawWithRaiseTrace,
    unifyAcyclicRawWithRaiseTracePrefer,
    setVarBoundWithRaiseTraceForTest,
    rebindWithBoundRepairTraceForTest,
    unifyStructureForTest,
    recordEdgeExecutionArtifactsForTest,
    singletonEdgeExecutionArtifactsForTest,
    runEdgeUnifyForTest,
    runEdgeStructureUnifyForTest,
    runEdgeTerminalStructureUnifyForTest,
    TerminalRootTransition(..),
    classifyTerminalRootTransitionForTest,
    sourceRaiseAuthorityNodesForTest,
    runEdgeUnifyWithBinderMetasForTest,
    sourceWitnessNodeWithCopyMapForTest,
    runEdgeBoundInstallForTest,
    RebuildBindParentsEnv(..),
    rebuildBindParentsForTest,
    contractExpansionWrapperBindingsForTest,
    instantiateScheme,
    instantiateSchemeWithTrace,
    instantiateSchemeAtTargetWithBoundsForTest,
    copyForallBoundProjectionAtBinderForTest,
    applyExpansionEdgeTracedAtTargetWithBindersForTest,
    mergeExpansions,
    materializeExpansionsForTest,
    bindExpansionArgsForTest,
    instantiationBindersForTest,
    dropVarBindForTest,
    certifyAppliedNonRootWeakenReplay,
    normalizeEdgeWitnessesM,
    ProvenancedNode(..),
    ProvenancedInstanceOp(..),
    normalizeInstanceOpsCoreWithProvenance,
    assertNoStandaloneGraftsWithProvenance,
    validateTerminalRootRaiseMergeForTest,
    rootRaiseMergeTraceAuthority,
    rootWeakenRaiseMergeTraceAuthority,
    validateTranslatablePresolution,
    structuralInterior,
    translatableWeakenedNodes
) where

import Control.Monad (void)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Base
    ( CopyMap
    , CopyMapping(..)
    , EdgeArtifact
    , EdgeArtifacts
    , EdgeArtifactsError(..)
    , EdgeExecutionArtifacts(..)
    , EdgeWitnessNonSourceOrigin(..)
    , PresolutionUf(..)
    , prUnionFind
    , edgeArtifactExpansion
    , edgeArtifactWitness
    , edgeArtifactTrace
    , edgeArtifactExpansionConstruction
    , eaEdgeExpansions
    , eaEdgeWitnesses
    , eaEdgeTraces
    , eaEdgeExpansionConstructions
    , eaIdentityEdges
    , lookupEdgeArtifact
    , edgeArtifactsFromExecutionArtifacts
    , EdgeTrace
    , ExpansionResultMap(..)
    , InteriorNodes(..)
    , EdgeSourceInterior(..)
    , InteriorSet
    , FrontierSet
    , PresolutionPlanBuilder(..)
    , MonadPresolution(bindExpansionArgs)
    , PresolutionM
    , PresolutionError
    , TranslatabilityIssue(..)
    , PresolutionState(..)
    , PendingWeakenOwner
    , mkPresolutionState
    , RawExpansionConstruction
    , emptyExpansionResultMap
    , emptyRawExpansionConstruction
    , canonicalizeExpansionResultMap
    , certifyAppliedNonRootWeakenReplay
    , emptyBindingRepairDirty
    , copiedNodes
    , fromListInterior
    , insertCopy
    , instantiationBindersM
    , lookupCopy
    , lookupExpansionResult
    , lookupExpansionResultUnder
    , originalNodes
    , runPresolutionM
    , sourceInteriorFromList
    , sourceInteriorFromSet
    , toListInterior
    , rootRaiseMergeTraceAuthority
    , rootWeakenRaiseMergeTraceAuthority
    )
import MLF.Constraint.Presolution.Copy
    ( copyForallBoundProjectionAtBinder
    , instantiateScheme
    , instantiateExpansionWithTraceAtTargetSnapshot
    , instantiateSchemeWithTrace
    )
import MLF.Constraint.Presolution.Driver
    ( validateReplayMapTraceContract
    )
import MLF.Constraint.Presolution.EdgeProcessing
    ( processInstEdge
    , runPresolutionLoopWithTiming
    )
import qualified MLF.Constraint.Presolution.EdgeProcessing.Unify as EdgeUnify
import qualified MLF.Constraint.Presolution.EdgeProcessing.Solve as EdgeSolve
import qualified MLF.Constraint.Presolution.EdgeProcessing.Interpreter as EdgeInterpreter
import MLF.Constraint.Presolution.EdgeUnify
    ( runEdgeBoundInstallForTest
    , runEdgeStructureUnifyForTest
    , runEdgeTerminalStructureUnifyForTest
    , runEdgeUnifyForTest
    , runEdgeUnifyWithBinderMetasForTest
    , sourceWitnessNodeWithCopyMapForTest
    )
import MLF.Constraint.Presolution.EdgeUnify.Unify
    ( TerminalRootTransition(..)
    , classifyTerminalRootTransition
    )
import MLF.Constraint.Presolution.Rewrite
    ( RebuildBindParentsEnv(..)
    , contractExpansionWrapperBindings
    , rebuildBindParents
    )
import MLF.Constraint.Presolution.Expansion
    ( applyExpansionEdgeTracedAtTargetWithBinders
    , decideMinimalExpansion
    , mergeExpansions
    )
import MLF.Constraint.Presolution.StateAccess (getBindingSnapshot)
import qualified MLF.Constraint.Presolution.Materialization as Materialization
import MLF.Constraint.Presolution.Plan (buildGeneralizePlans)
import qualified MLF.Constraint.Presolution.Ops as Ops
import MLF.Constraint.Presolution.Unify
    ( unifyAcyclic
    , unifyAcyclicRawWithRaiseTrace
    , unifyAcyclicRawWithRaiseTracePrefer
    )
import MLF.Constraint.Presolution.Validation
    ( structuralInterior
    , translatableWeakenedNodes
    , validateTranslatablePresolution
    )
import MLF.Constraint.Presolution.WitnessNorm (normalizeEdgeWitnessesM)
import MLF.Constraint.Presolution.Witness (EdgeWitnessOp(..))
import qualified MLF.Constraint.Presolution.WitnessValidation as WitnessValidation
import MLF.Constraint.Presolution.WitnessCanon
    ( ProvenancedInstanceOp(..)
    , ProvenancedNode(..)
    , assertNoStandaloneGraftsWithProvenance
    , normalizeInstanceOpsCoreWithProvenance
    )
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , BindingError
    , Constraint
    , EdgeId(..)
    , GenNodeId
    , InstEdge
    , NodeId
    , NodeRef
    , TyNode
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.RootOwnership (emptyRootOwnershipIndex)
import MLF.Constraint.Types.Witness
    ( EdgeWitness
    , Expansion(..)
    , InstanceOp
    , ewEdgeId
    )
import MLF.Util.Timing (defaultTimingConfig, timingProgramOperations)
import MLF.Util.Trace (TraceConfig)

psEdgeExpansions :: PresolutionState p -> IntMap Expansion
psEdgeExpansions =
    IntMap.map eeaExpansion . psEdgeExecutionArtifacts

psEdgeWitnesses :: PresolutionState p -> IntMap EdgeWitness
psEdgeWitnesses =
    IntMap.map eeaWitness . psEdgeExecutionArtifacts

psEdgeRaiseAuthorityNodes :: PresolutionState p -> IntMap IntSet.IntSet
psEdgeRaiseAuthorityNodes =
    IntMap.map eeaRaiseAuthorityNodes . psEdgeExecutionArtifacts

psEdgeNonSourceOpOrigins
    :: PresolutionState p
    -> IntMap (IntMap EdgeWitnessNonSourceOrigin)
psEdgeNonSourceOpOrigins =
    IntMap.map eeaNonSourceOpOrigins . psEdgeExecutionArtifacts

psEdgeExpansionConstructions
    :: PresolutionState p
    -> IntMap RawExpansionConstruction
psEdgeExpansionConstructions =
    IntMap.map eeaExpansionConstruction . psEdgeExecutionArtifacts

psEdgeTraces :: PresolutionState p -> IntMap EdgeTrace
psEdgeTraces =
    IntMap.map eeaTrace . psEdgeExecutionArtifacts

legacyPresolutionStateView
    :: PresolutionState p
    -> ( Constraint p
       , Presolution
       , IntMap NodeId
       , Int
       , IntSet.IntSet
       , IntMap PendingWeakenOwner
       , IntMap [NodeId]
       , IntMap Expansion
       , IntMap EdgeWitness
       , IntMap EdgeTrace
       )
legacyPresolutionStateView st =
    ( psConstraint st
    , psPresolution st
    , psUnionFind st
    , psNextNodeId st
    , psPendingWeakens st
    , psPendingWeakenOwners st
    , psBinderCache st
    , psEdgeExpansions st
    , psEdgeWitnesses st
    , psEdgeTraces st
    )

-- | Compatibility fixture syntax isolated to the test-support module.
--
-- Production construction accepts only complete 'EdgeExecutionArtifacts'.
-- Older low-level specs can still use the compact positional form, but this
-- builder validates identical keys and embedded edge identities before
-- creating packets.
pattern PresolutionState
    :: Constraint p
    -> Presolution
    -> IntMap NodeId
    -> Int
    -> IntSet.IntSet
    -> IntMap PendingWeakenOwner
    -> IntMap [NodeId]
    -> IntMap Expansion
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> PresolutionState p
pattern PresolutionState
    constraint
    presolution
    unionFind
    nextNodeId
    pendingWeakens
    pendingWeakenOwners
    binderCache
    edgeExpansions
    edgeWitnesses
    edgeTraces <-
    ( legacyPresolutionStateView ->
        ( constraint
        , presolution
        , unionFind
        , nextNodeId
        , pendingWeakens
        , pendingWeakenOwners
        , binderCache
        , edgeExpansions
        , edgeWitnesses
        , edgeTraces
        )
      )
  where
    PresolutionState
        constraint
        presolution
        unionFind
        nextNodeId
        pendingWeakens
        pendingWeakenOwners
        binderCache
        edgeExpansions
        edgeWitnesses
        edgeTraces =
            mkPresolutionState
                constraint
                presolution
                unionFind
                nextNodeId
                pendingWeakens
                pendingWeakenOwners
                binderCache
                ( legacyEdgeExecutionArtifactsForTest
                    edgeExpansions
                    edgeWitnesses
                    edgeTraces
                )

{-# COMPLETE PresolutionState #-}

emptyPresolutionStateForTest
    :: Constraint p
    -> Int
    -> PresolutionState p
emptyPresolutionStateForTest constraint nextNodeId =
    mkPresolutionState
        constraint
        (Presolution IntMap.empty)
        IntMap.empty
        nextNodeId
        IntSet.empty
        IntMap.empty
        IntMap.empty
        IntMap.empty

legacyEdgeExecutionArtifactsForTest
    :: IntMap Expansion
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> IntMap EdgeExecutionArtifacts
legacyEdgeExecutionArtifactsForTest expansions witnesses traces
    | expansionKeys /= witnessKeys || witnessKeys /= traceKeys =
        error "PresolutionState test fixture: partial edge execution artifacts"
    | otherwise =
        IntMap.mapWithKey makeArtifact witnesses
  where
    expansionKeys = IntMap.keysSet expansions
    witnessKeys = IntMap.keysSet witnesses
    traceKeys = IntMap.keysSet traces

    makeArtifact edgeKey witness
        | ewEdgeId witness /= EdgeId edgeKey =
            error
                ( "PresolutionState test fixture: witness edge identity mismatch "
                    ++ show (EdgeId edgeKey, ewEdgeId witness)
                )
        | otherwise =
            EdgeExecutionArtifacts
                { eeaExpansion = require "expansion" edgeKey expansions
                , eeaWitness = witness
                , eeaRaiseAuthorityNodes = IntSet.empty
                , eeaNonSourceOpOrigins = IntMap.empty
                , eeaExpansionConstruction = emptyRawExpansionConstruction
                , eeaTrace = require "trace" edgeKey traces
                }

    require label key values =
        case IntMap.lookup key values of
            Just value -> value
            Nothing ->
                error
                    ( "PresolutionState test fixture: missing "
                        ++ label
                        ++ " for "
                        ++ show (EdgeId key)
                    )

defaultPlanBuilder :: TraceConfig -> PresolutionPlanBuilder
defaultPlanBuilder traceCfg = PresolutionPlanBuilder (buildGeneralizePlans traceCfg)

-- | Test-only component-map fixture. Production publishes one complete
-- 'EdgeExecutionArtifacts' packet per edge and never exposes this join.
mkEdgeArtifacts
    :: IntMap Expansion
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> IntMap RawExpansionConstruction
    -> IntSet.IntSet
    -> Either EdgeArtifactsError EdgeArtifacts
mkEdgeArtifacts expansions witnesses traces constructions identityEdges
    | expansionKeys /= witnessKeys
        || witnessKeys /= traceKeys
        || traceKeys /= constructionKeys =
        Left keyMismatch
    | otherwise = do
        executionArtifacts <-
            IntMap.traverseWithKey
                (\edgeKey expansion ->
                    case
                        ( IntMap.lookup edgeKey witnesses
                        , IntMap.lookup edgeKey traces
                        , IntMap.lookup edgeKey constructions
                        )
                    of
                        (Just witness, Just traceInfo, Just construction) ->
                            Right
                                EdgeExecutionArtifacts
                                    { eeaExpansion = expansion
                                    , eeaWitness = witness
                                    , eeaRaiseAuthorityNodes = IntSet.empty
                                    , eeaNonSourceOpOrigins = IntMap.empty
                                    , eeaExpansionConstruction = construction
                                    , eeaTrace = traceInfo
                                    }
                        _ -> Left keyMismatch
                )
                expansions
        edgeArtifactsFromExecutionArtifacts executionArtifacts identityEdges
  where
    expansionKeys = IntMap.keysSet expansions
    witnessKeys = IntMap.keysSet witnesses
    traceKeys = IntMap.keysSet traces
    constructionKeys = IntMap.keysSet constructions
    keyMismatch =
        EdgeArtifactKeyMismatch
            { edgeArtifactExpansionKeys = expansionKeys
            , edgeArtifactWitnessKeys = witnessKeys
            , edgeArtifactTraceKeys = traceKeys
            , edgeArtifactExpansionConstructionKeys = constructionKeys
            }

emptyEdgeArtifacts :: EdgeArtifacts
emptyEdgeArtifacts =
    expectEdgeArtifacts
        (edgeArtifactsFromExecutionArtifacts IntMap.empty IntSet.empty)

setEdgeArtifactsIdentityEdges
    :: IntSet.IntSet
    -> EdgeArtifacts
    -> EdgeArtifacts
setEdgeArtifactsIdentityEdges identityEdges edgeArtifacts =
    expectEdgeArtifacts
        ( mkEdgeArtifacts
            (eaEdgeExpansions edgeArtifacts)
            (eaEdgeWitnesses edgeArtifacts)
            (eaEdgeTraces edgeArtifacts)
            (eaEdgeExpansionConstructions edgeArtifacts)
            identityEdges
        )

edgeArtifactsForTest
    :: IntMap Expansion
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> IntSet.IntSet
    -> EdgeArtifacts
edgeArtifactsForTest expansions witnesses traces identityEdges =
    expectEdgeArtifacts
        ( mkEdgeArtifacts
            expansions
            witnesses
            traces
            (IntMap.map (const emptyRawExpansionConstruction) expansions)
            identityEdges
        )

insertEdgeArtifactForTest
    :: EdgeId
    -> Expansion
    -> EdgeWitness
    -> EdgeTrace
    -> EdgeArtifacts
    -> EdgeArtifacts
insertEdgeArtifactForTest edgeId expansion witness traceInfo =
    insertEdgeArtifactWithConstructionForTest
        edgeId
        expansion
        witness
        traceInfo
        emptyRawExpansionConstruction

insertEdgeArtifactWithConstructionForTest
    :: EdgeId
    -> Expansion
    -> EdgeWitness
    -> EdgeTrace
    -> RawExpansionConstruction
    -> EdgeArtifacts
    -> EdgeArtifacts
insertEdgeArtifactWithConstructionForTest edgeId expansion witness traceInfo construction edgeArtifacts =
    expectEdgeArtifacts
        ( mkEdgeArtifacts
            (IntMap.insert edgeKey expansion (eaEdgeExpansions edgeArtifacts))
            (IntMap.insert edgeKey witness (eaEdgeWitnesses edgeArtifacts))
            (IntMap.insert edgeKey traceInfo (eaEdgeTraces edgeArtifacts))
            ( IntMap.insert
                edgeKey
                construction
                (eaEdgeExpansionConstructions edgeArtifacts)
            )
            (eaIdentityEdges edgeArtifacts)
        )
  where
    edgeKey = getEdgeId edgeId

setEdgeArtifactExpansionForTest
    :: EdgeId
    -> Expansion
    -> EdgeArtifacts
    -> EdgeArtifacts
setEdgeArtifactExpansionForTest edgeId expansion edgeArtifacts =
    case lookupEdgeArtifact edgeId edgeArtifacts of
        Nothing ->
            error
                ( "setEdgeArtifactExpansionForTest: missing edge packet "
                    ++ show edgeId
                )
        Just artifact ->
            insertEdgeArtifactWithConstructionForTest
                edgeId
                expansion
                (edgeArtifactWitness artifact)
                (edgeArtifactTrace artifact)
                (edgeArtifactExpansionConstruction artifact)
                edgeArtifacts

setEdgeArtifactWitnessForTest
    :: EdgeId
    -> EdgeWitness
    -> EdgeArtifacts
    -> EdgeArtifacts
setEdgeArtifactWitnessForTest edgeId witness edgeArtifacts =
    case lookupEdgeArtifact edgeId edgeArtifacts of
        Nothing ->
            error
                ( "setEdgeArtifactWitnessForTest: missing edge packet "
                    ++ show edgeId
                )
        Just artifact ->
            insertEdgeArtifactWithConstructionForTest
                edgeId
                (edgeArtifactExpansion artifact)
                witness
                (edgeArtifactTrace artifact)
                (edgeArtifactExpansionConstruction artifact)
                edgeArtifacts

setEdgeArtifactTraceForTest
    :: EdgeId
    -> EdgeTrace
    -> EdgeArtifacts
    -> EdgeArtifacts
setEdgeArtifactTraceForTest edgeId traceInfo edgeArtifacts =
    case lookupEdgeArtifact edgeId edgeArtifacts of
        Nothing ->
            error
                ( "setEdgeArtifactTraceForTest: missing edge packet "
                    ++ show edgeId
                )
        Just artifact ->
            insertEdgeArtifactWithConstructionForTest
                edgeId
                (edgeArtifactExpansion artifact)
                (edgeArtifactWitness artifact)
                traceInfo
                (edgeArtifactExpansionConstruction artifact)
                edgeArtifacts

deleteEdgeArtifactForTest :: EdgeId -> EdgeArtifacts -> EdgeArtifacts
deleteEdgeArtifactForTest edgeId edgeArtifacts =
    expectEdgeArtifacts
        ( mkEdgeArtifacts
            (IntMap.delete edgeKey (eaEdgeExpansions edgeArtifacts))
            (IntMap.delete edgeKey (eaEdgeWitnesses edgeArtifacts))
            (IntMap.delete edgeKey (eaEdgeTraces edgeArtifacts))
            (IntMap.delete edgeKey (eaEdgeExpansionConstructions edgeArtifacts))
            (eaIdentityEdges edgeArtifacts)
        )
  where
    edgeKey = getEdgeId edgeId

expectEdgeArtifacts
    :: Either EdgeArtifactsError EdgeArtifacts
    -> EdgeArtifacts
expectEdgeArtifacts result =
    case result of
        Left err ->
            error ("invalid test edge artifact packet: " ++ show err)
        Right edgeArtifacts ->
            edgeArtifacts

-- | Test-only view of the total terminal-root binding-flag classifier.  The
-- production mutation paths consume its non-optional result, so 'Nothing'
-- cannot reserve or eliminate edge-local authority state.
classifyTerminalRootTransitionForTest
    :: Maybe BindFlag
    -> Maybe BindFlag
    -> Maybe TerminalRootTransition
classifyTerminalRootTransitionForTest = classifyTerminalRootTransition

-- | Test-only projection of the production constructor for frozen standalone
-- Raise authority.  Tests use it before mutating the binding tree so they
-- cannot manufacture a certificate that construction would never issue.
sourceRaiseAuthorityNodesForTest
    :: NodeId
    -> EdgeSourceInterior
    -> PresolutionM p IntSet.IntSet
sourceRaiseAuthorityNodesForTest = EdgeInterpreter.sourceRaiseAuthorityNodes

validateTerminalRootRaiseMergeForTest
    :: NodeId
    -> (NodeId -> NodeId -> Bool)
    -> (NodeId -> NodeId -> Bool)
    -> [InstanceOp]
    -> Either WitnessValidation.OmegaNormalizeError ()
validateTerminalRootRaiseMergeForTest =
    WitnessValidation.validateTerminalRootRaiseMerge

-- | Test-only entrypoint for checking that the decomposed operation-timing
-- interpreter is semantically identical to the fused edge interpreter.
runPresolutionLoopWithOperationTimingForTest
    :: Bool
    -> String
    -> TraceConfig
    -> [InstEdge]
    -> PresolutionState p
    -> IO (Either PresolutionError ((), PresolutionState p))
runPresolutionLoopWithOperationTimingForTest operationTiming label traceCfg edges =
    runPresolutionLoopWithTiming
        defaultTimingConfig {timingProgramOperations = operationTiming}
        label
        traceCfg
        emptyRootOwnershipIndex
        edges

-- | Test-only assertion seam for the ExpIdentity witness-plan invariant.  The
-- production input record stays private; tests can still prove that identity
-- execution rejects any accidental Omega base operations.
runIdentityExpansionWithBaseOpsForTest
    :: GenNodeId
    -> EdgeId
    -> TyNode
    -> TyNode
    -> NodeId
    -> [InstanceOp]
    -> PresolutionM p ()
runIdentityExpansionWithBaseOpsForTest owner edgeId leftRaw rightRaw bodyRoot baseOps =
    void $
        EdgeUnify.runExpansionUnify
            EdgeUnify.EdgeExpansionInput
                { EdgeUnify.eeiGenId = owner
                , EdgeUnify.eeiEdgeId = edgeId
                , EdgeUnify.eeiLeftRaw = leftRaw
                , EdgeUnify.eeiRightRaw = rightRaw
                , EdgeUnify.eeiExpansion = ExpIdentity
                , EdgeUnify.eeiBodyRoot = bodyRoot
                , EdgeUnify.eeiSourceInterior = EdgeSourceInterior mempty
                , EdgeUnify.eeiLockedSourceNodes = mempty
                , EdgeUnify.eeiSourceRaiseAuthorityNodes = mempty
                , EdgeUnify.eeiSourceNodeKeys = mempty
                , EdgeUnify.eeiBoundVars = []
                , EdgeUnify.eeiBinderArgs = []
                , EdgeUnify.eeiStructuralUnifications = []
                }
            baseOps

-- | Test-only entrypoint for the structural equalities selected by an
-- 'ExpIdentity' decision.  It keeps the private edge-execution input record out
-- of specs while allowing them to exercise the production identity lane.
runIdentityStructuralUnificationsForTest
    :: GenNodeId
    -> EdgeId
    -> TyNode
    -> TyNode
    -> NodeId
    -> EdgeSourceInterior
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> [(NodeId, NodeId)]
    -> PresolutionM p ()
runIdentityStructuralUnificationsForTest
    owner
    edgeId
    leftRaw
    rightRaw
    bodyRoot
    sourceInterior
    sourceRaiseAuthorityNodes
    sourceNodeKeys
    structuralUnifications =
        void $
            EdgeUnify.runExpansionUnify
                EdgeUnify.EdgeExpansionInput
                    { EdgeUnify.eeiGenId = owner
                    , EdgeUnify.eeiEdgeId = edgeId
                    , EdgeUnify.eeiLeftRaw = leftRaw
                    , EdgeUnify.eeiRightRaw = rightRaw
                    , EdgeUnify.eeiExpansion = ExpIdentity
                    , EdgeUnify.eeiBodyRoot = bodyRoot
                    , EdgeUnify.eeiSourceInterior = sourceInterior
                    , EdgeUnify.eeiLockedSourceNodes = IntSet.empty
                    , EdgeUnify.eeiSourceRaiseAuthorityNodes = sourceRaiseAuthorityNodes
                    , EdgeUnify.eeiSourceNodeKeys = sourceNodeKeys
                    , EdgeUnify.eeiBoundVars = []
                    , EdgeUnify.eeiBinderArgs = []
                    , EdgeUnify.eeiStructuralUnifications = structuralUnifications
                    }
                []

edgeExpansionExtraOpsForTest
    :: EdgeUnify.EdgeExpansionResult
    -> [EdgeWitnessOp]
edgeExpansionExtraOpsForTest = EdgeUnify.eerExtraOps

-- | Test-only seam for the post-Omega monotone owner certificate.  Expansion
-- construction itself remains covered by the destination-aware constructor.
requireExpansionResultScopeForTest
    :: NodeId
    -> [NodeRef]
    -> PresolutionM p NodeId
requireExpansionResultScopeForTest = EdgeUnify.requireExpansionResultScope

-- | Test-only access to the final expansion rewrite and ownership transfer.
materializeExpansionsForTest :: PresolutionM p (IntMap NodeId)
materializeExpansionsForTest = Materialization.materializeExpansions

-- | Test-only access to the paper's expansion-destination ownership step.
bindExpansionArgsForTest :: NodeId -> [(NodeId, NodeId)] -> PresolutionM p ()
bindExpansionArgsForTest = bindExpansionArgs

-- | Test-only access to binder discovery so cache invalidation can be checked
-- across an authoritative graph mutation.
instantiationBindersForTest
    :: GenNodeId
    -> NodeId
    -> PresolutionM p (NodeId, [NodeId])
instantiationBindersForTest = instantiationBindersM

-- | Test-only alias for the normal eliminated-variable mutation path.
dropVarBindForTest :: NodeId -> PresolutionM p ()
dropVarBindForTest = Ops.dropVarBind

-- | Test-only seam for the complete atomic destination-aware instantiation.
-- The first trace is the complete semantic copy, including lower bounds
-- reached by binder substitution.  The compatibility second trace is empty;
-- argument-owned auxiliary copies are intentionally private to the constructor.
instantiateSchemeAtTargetWithBoundsForTest
    :: GenNodeId
    -> NodeId
    -> NodeId
    -> [(NodeId, NodeId)]
    -> [(NodeId, NodeId)]
    -> PresolutionM p
        ( (NodeId, CopyMap, InteriorSet, FrontierSet)
        , (CopyMap, InteriorSet, FrontierSet)
        )
instantiateSchemeAtTargetWithBoundsForTest sourceOwner target body binderMetas binderArgs = do
    snapshot <- getBindingSnapshot
    (semanticTrace, compatibilityTrace, _construction) <-
        instantiateExpansionWithTraceAtTargetSnapshot
            snapshot
            sourceOwner
            target
            body
            binderMetas
            binderArgs
    pure (semanticTrace, compatibilityTrace)

-- | Test-only seam for the destination-aware copy used by a
-- 'BoundProjection' in forall introduction.
copyForallBoundProjectionAtBinderForTest
    :: NodeRef
    -> NodeId
    -> [(NodeId, NodeId)]
    -> PresolutionM p NodeId
copyForallBoundProjectionAtBinderForTest destinationBinder boundRoot substitutions = do
    snapshot <- getBindingSnapshot
    copyForallBoundProjectionAtBinder
        snapshot
        destinationBinder
        boundRoot
        substitutions

-- | Test-only access to the complete target-aware expansion interpreter,
-- including ExpCompose recipes whose instantiation is not the outer constructor.
applyExpansionEdgeTracedAtTargetWithBindersForTest
    :: GenNodeId
    -> NodeId
    -> Expansion
    -> TyNode
    -> NodeId
    -> [NodeId]
    -> PresolutionM p
        ( NodeId
        , (CopyMap, InteriorSet, FrontierSet)
        , RawExpansionConstruction
        )
applyExpansionEdgeTracedAtTargetWithBindersForTest =
    applyExpansionEdgeTracedAtTargetWithBinders

-- | Test-only seam for the construction-time variable-bound scope invariant.
setVarBoundWithRaiseTraceForTest
    :: NodeId
    -> Maybe NodeId
    -> PresolutionM p [NodeId]
setVarBoundWithRaiseTraceForTest = Ops.setVarBoundWithRaiseTrace

-- | Test-only seam for atomic binding moves that preserve bound scopes.
rebindWithBoundRepairTraceForTest
    :: NodeRef
    -> (NodeRef, BindFlag)
    -> PresolutionM p [NodeId]
rebindWithBoundRepairTraceForTest = Ops.rebindWithBoundRepairTrace

-- | Test-only access to the expansion-aware structural unifier.  Keep the
-- implementation module private while allowing focused graph regressions.
unifyStructureForTest :: NodeId -> NodeId -> PresolutionM p ()
unifyStructureForTest = EdgeSolve.unifyStructure

-- | Test-only access to the atomic edge execution recorder.  Keeping this
-- seam here lets structural tests exercise duplicate-write rejection
-- without exposing the edge-processing implementation publicly.
recordEdgeExecutionArtifactsForTest
    :: EdgeId
    -> EdgeExecutionArtifacts
    -> PresolutionM p ()
recordEdgeExecutionArtifactsForTest = EdgeSolve.recordEdgeExecutionArtifacts

singletonEdgeExecutionArtifactsForTest
    :: Int
    -> EdgeWitness
    -> EdgeTrace
    -> IntMap EdgeExecutionArtifacts
singletonEdgeExecutionArtifactsForTest edgeKey witness trace =
    IntMap.singleton
        edgeKey
        EdgeExecutionArtifacts
            { eeaExpansion = ExpIdentity
            , eeaWitness = witness
            , eeaRaiseAuthorityNodes = IntSet.empty
            , eeaNonSourceOpOrigins = IntMap.empty
            , eeaExpansionConstruction = emptyRawExpansionConstruction
            , eeaTrace = trace
            }

-- | Test-only access to final bind-parent reconstruction.  Keep the rewrite
-- implementation private while allowing focused ownership regressions.
rebuildBindParentsForTest :: RebuildBindParentsEnv p -> PresolutionM q BindParents
rebuildBindParentsForTest = rebuildBindParents

contractExpansionWrapperBindingsForTest
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError BindParents
contractExpansionWrapperBindingsForTest = contractExpansionWrapperBindings
