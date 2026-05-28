{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing
Description : Edge processing loop for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the edge processing loop for MLF presolution.
It processes instantiation edges in topological order to decide minimal
expansions for expansion variables.

The two-pass architecture delegates to:
* Pass A (Planner): classifies edges into typed plans
* Pass B (Interpreter): executes TyExp-left plans with expansion semantics
* Shared helpers (Solve): unify/solve operations used by both passes
-}
module MLF.Constraint.Presolution.EdgeProcessing (
    runPresolutionLoop,
    runPresolutionLoopWithTiming,
    processInstEdge,
) where

import Control.Exception (evaluate)
import Control.Monad (unless, when)
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
    ( Constraint(..)
    , EdgeId(..)
    , ExpVarId(..)
    , GenNodeId(..)
    , InstEdge(..)
    , NodeId(..)
    )
import MLF.Constraint.Types.Witness (Expansion(..))
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Presolution.Base
    ( MonadPresolution(..)
    , PresolutionError(..)
    , PresolutionM
    , PendingWeakenOwner
    , pendingWeakenOwnerFromMaybe
    , ensureBindingParents
    , requireValidBindingTree
    , runPresolutionM
    , PresolutionState(..)
    , setEdgeLocalSnapshot
    , clearEdgeLocalSnapshot
    )
import MLF.Constraint.Presolution.EdgeUnify
    ( flushPendingWeakensAtOwnerBoundary
    )
import MLF.Constraint.Presolution.EdgeUnify.Omega
    ( pendingWeakenOwners
    )
import MLF.Constraint.Presolution.StateAccess
    ( PresolutionBindingSnapshot(..)
    , bindingSnapshotFindSchemeIntroducer
    , getBindingSnapshot
    , getCanonical
    , getConstraintAndUnionFind
    , getPendingUnifyEdgesM
    , getPendingWeakensM
    , putConstraintAndUnionFind
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan
    ( EdgePlan(..)
    , ResolvedTyExp(..)
    , mkResolvedTyExp
    )
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter
    ( executeEdgePlan
    , executeEdgePlanWithoutTraceCanonicalizationWithOutcome
    , EdgeExecutionDecision (..)
    , EdgeExecutionOutcome (..)
    , EdgeExecutionWitnessContext (..)
    , prepareEdgeExecutionDecision
    , recordEdgeExecutionExpansion
    , unifyEdgeExecutionStructure
    , prepareEdgeExecutionWitness
    , recordEdgeExecutionReplayTrace
    , runEdgeExecutionExpansionUnify
    , recordEdgeExecutionTrace
    , recordEdgeExecutionWitness
    )
import MLF.Constraint.Presolution.EdgeProcessing.Solve
    ( canonicalizeEdgeTraceInteriorsWith
    )
import MLF.Constraint.Presolution.EdgeProcessing.Unify
    ( EdgeExpansionResult
    , applyEdgeExpansion
    , bindEdgeExpansionRoot
    , prepareEdgeExpansionOmega
    , executeEdgeExpansionOmega
    , finishEdgeExpansionUnify
    )
import MLF.Constraint.Presolution.EdgeProcessing.Worklist
    ( EdgeFingerprint(..)
    , EdgePlanSeed(..)
    , EdgeWorkItem(..)
    , EdgeWorklist
    , WorklistInvalidation(..)
    , buildIndexedEdgeWorklist
    , invalidateExpansionsExcept
    , invalidateOwnersExcept
    , invalidateRootsExcept
    , noteInertEdge
    , noteProcessedEdge
    , popEdgeWorkItem
    )
import MLF.Constraint.Presolution.Witness (EdgeWitnessPlan(..))
import MLF.Constraint.Presolution.Ops
    ( findRoot
    , getCanonicalNode
    , getNode
    )
import MLF.Constraint.Solve (repairNonUpperParents)
import MLF.Constraint.Unify.Closure (SolveError, UnifyClosureResult(..), runUnifyClosureWithSeed)
import MLF.Util.Timing
    ( TimingConfig
    , emitProgramOperationDurationIO
    , measureProgramOperationIO
    )
import MLF.Util.Trace (TraceConfig)
import MLF.Constraint.Types.SynthesizedExpVar (isSynthesizedExpVar)
import qualified MLF.Util.UnionFind as UnionFind

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: TraceConfig -> [InstEdge] -> PresolutionM p ()
runPresolutionLoop traceCfg edges = do
    requireValidBindingTree
    drainPendingUnifyClosure traceCfg
    worklist0 <- buildIndexedEdgeWorklist edges
    (mbLastOwner, _worklist) <-
        runScheduledWorklist traceCfg Nothing worklist0
    scheduleWeakensByOwnerBoundary mbLastOwner Nothing Nothing
    assertNoPendingUnifyEdges "after-inst-edge-closure" Nothing
    requireValidBindingTree

data PresolutionLoopCounters = PresolutionLoopCounters
    { plcValidation :: !Word64
    , plcIndex :: !Word64
    , plcPlan :: !Word64
    , plcScheduleWeakens :: !Word64
    , plcExecute :: !Word64
    , plcExecuteDecideExpansion :: !Word64
    , plcExecuteRecordExpansion :: !Word64
    , plcExecuteUnifyStructure :: !Word64
    , plcExecuteWitnessPlan :: !Word64
    , plcExecuteExpansionUnify :: !Word64
    , plcExecuteExpansionApply :: !Word64
    , plcExecuteExpansionBindRoot :: !Word64
    , plcExecuteExpansionPrepareOmega :: !Word64
    , plcExecuteExpansionExecuteOmega :: !Word64
    , plcExecuteExpansionFinish :: !Word64
    , plcExecuteRecordTrace :: !Word64
    , plcExecuteRecordWitness :: !Word64
    , plcCanonicalizeTraceInteriors :: !Word64
    , plcDrainUnifyClosure :: !Word64
    , plcStaleChanged :: !Word64
    , plcReplayHit :: !Word64
    , plcReplayFresh :: !Word64
    , plcReplayNoop :: !Word64
    , plcReplayTraceRebuild :: !Word64
    , plcInvalidatedRootEdges :: !Word64
    , plcInvalidatedOwnerEdges :: !Word64
    , plcInvalidatedExpansionEdges :: !Word64
    , plcInvalidatedEdges :: !Word64
    }

data EdgeMutation = EdgeMutation
    { emRoots :: !IntSet.IntSet
    , emOwners :: !IntSet.IntSet
    , emExpansions :: !IntSet.IntSet
    }
    deriving (Eq, Show)

data EdgeWorkAction
    = EdgeWorkSkip
    | EdgeWorkRefresh !EdgePlan
    | EdgeWorkProcess !EdgeProcessReason !EdgePlan
    deriving (Eq, Show)

data EdgeProcessReason
    = EdgeProcessNormal
    | EdgeProcessStaleChanged
    deriving (Eq, Show)

data EdgeInvalidation = EdgeInvalidation
    { eiRootEdges :: !IntSet.IntSet
    , eiOwnerEdges :: !IntSet.IntSet
    , eiExpansionEdges :: !IntSet.IntSet
    , eiEdges :: !IntSet.IntSet
    }
    deriving (Eq, Show)

{- Note [Processed edge replay]
The indexed worklist can mark already-processed edges stale when a later edge
changes roots, owners, or expansion assignments.  Stale processed edges must go
through planning again so their fingerprint/index entries stay current.  They
do not re-execute from the worklist yet: replay is currently only safe for
direct repeated edge execution, where the final expansion matches the recorded
expansion and trace/witness artifacts are already present.
-}

emptyPresolutionLoopCounters :: PresolutionLoopCounters
emptyPresolutionLoopCounters =
    PresolutionLoopCounters
        { plcValidation = 0
        , plcIndex = 0
        , plcPlan = 0
        , plcScheduleWeakens = 0
        , plcExecute = 0
        , plcExecuteDecideExpansion = 0
        , plcExecuteRecordExpansion = 0
        , plcExecuteUnifyStructure = 0
        , plcExecuteWitnessPlan = 0
        , plcExecuteExpansionUnify = 0
        , plcExecuteExpansionApply = 0
        , plcExecuteExpansionBindRoot = 0
        , plcExecuteExpansionPrepareOmega = 0
        , plcExecuteExpansionExecuteOmega = 0
        , plcExecuteExpansionFinish = 0
        , plcExecuteRecordTrace = 0
        , plcExecuteRecordWitness = 0
        , plcCanonicalizeTraceInteriors = 0
        , plcDrainUnifyClosure = 0
        , plcStaleChanged = 0
        , plcReplayHit = 0
        , plcReplayFresh = 0
        , plcReplayNoop = 0
        , plcReplayTraceRebuild = 0
        , plcInvalidatedRootEdges = 0
        , plcInvalidatedOwnerEdges = 0
        , plcInvalidatedExpansionEdges = 0
        , plcInvalidatedEdges = 0
        }

addPresolutionLoopCounters :: PresolutionLoopCounters -> PresolutionLoopCounters -> PresolutionLoopCounters
addPresolutionLoopCounters a b =
    PresolutionLoopCounters
        { plcValidation = plcValidation a + plcValidation b
        , plcIndex = plcIndex a + plcIndex b
        , plcPlan = plcPlan a + plcPlan b
        , plcScheduleWeakens = plcScheduleWeakens a + plcScheduleWeakens b
        , plcExecute = plcExecute a + plcExecute b
        , plcExecuteDecideExpansion = plcExecuteDecideExpansion a + plcExecuteDecideExpansion b
        , plcExecuteRecordExpansion = plcExecuteRecordExpansion a + plcExecuteRecordExpansion b
        , plcExecuteUnifyStructure = plcExecuteUnifyStructure a + plcExecuteUnifyStructure b
        , plcExecuteWitnessPlan = plcExecuteWitnessPlan a + plcExecuteWitnessPlan b
        , plcExecuteExpansionUnify = plcExecuteExpansionUnify a + plcExecuteExpansionUnify b
        , plcExecuteExpansionApply = plcExecuteExpansionApply a + plcExecuteExpansionApply b
        , plcExecuteExpansionBindRoot = plcExecuteExpansionBindRoot a + plcExecuteExpansionBindRoot b
        , plcExecuteExpansionPrepareOmega = plcExecuteExpansionPrepareOmega a + plcExecuteExpansionPrepareOmega b
        , plcExecuteExpansionExecuteOmega = plcExecuteExpansionExecuteOmega a + plcExecuteExpansionExecuteOmega b
        , plcExecuteExpansionFinish = plcExecuteExpansionFinish a + plcExecuteExpansionFinish b
        , plcExecuteRecordTrace = plcExecuteRecordTrace a + plcExecuteRecordTrace b
        , plcExecuteRecordWitness = plcExecuteRecordWitness a + plcExecuteRecordWitness b
        , plcCanonicalizeTraceInteriors = plcCanonicalizeTraceInteriors a + plcCanonicalizeTraceInteriors b
        , plcDrainUnifyClosure = plcDrainUnifyClosure a + plcDrainUnifyClosure b
        , plcStaleChanged = plcStaleChanged a + plcStaleChanged b
        , plcReplayHit = plcReplayHit a + plcReplayHit b
        , plcReplayFresh = plcReplayFresh a + plcReplayFresh b
        , plcReplayNoop = plcReplayNoop a + plcReplayNoop b
        , plcReplayTraceRebuild = plcReplayTraceRebuild a + plcReplayTraceRebuild b
        , plcInvalidatedRootEdges = plcInvalidatedRootEdges a + plcInvalidatedRootEdges b
        , plcInvalidatedOwnerEdges = plcInvalidatedOwnerEdges a + plcInvalidatedOwnerEdges b
        , plcInvalidatedExpansionEdges = plcInvalidatedExpansionEdges a + plcInvalidatedExpansionEdges b
        , plcInvalidatedEdges = plcInvalidatedEdges a + plcInvalidatedEdges b
        }

edgeProcessReasonCounters :: EdgeProcessReason -> PresolutionLoopCounters
edgeProcessReasonCounters reason =
    case reason of
        EdgeProcessNormal ->
            emptyPresolutionLoopCounters
        EdgeProcessStaleChanged ->
            emptyPresolutionLoopCounters { plcStaleChanged = 1 }

emptyEdgeInvalidation :: EdgeInvalidation
emptyEdgeInvalidation =
    EdgeInvalidation
        { eiRootEdges = IntSet.empty
        , eiOwnerEdges = IntSet.empty
        , eiExpansionEdges = IntSet.empty
        , eiEdges = IntSet.empty
        }

edgeMutationIsEmpty :: EdgeMutation -> Bool
edgeMutationIsEmpty mutation =
    IntSet.null (emRoots mutation)
        && IntSet.null (emOwners mutation)
        && IntSet.null (emExpansions mutation)

edgeInvalidationCounters :: EdgeInvalidation -> PresolutionLoopCounters
edgeInvalidationCounters invalidation =
    emptyPresolutionLoopCounters
        { plcInvalidatedRootEdges = intSetSizeWord64 (eiRootEdges invalidation)
        , plcInvalidatedOwnerEdges = intSetSizeWord64 (eiOwnerEdges invalidation)
        , plcInvalidatedExpansionEdges = intSetSizeWord64 (eiExpansionEdges invalidation)
        , plcInvalidatedEdges = intSetSizeWord64 (eiEdges invalidation)
        }

intSetSizeWord64 :: IntSet.IntSet -> Word64
intSetSizeWord64 = fromIntegral . IntSet.size

runPresolutionLoopWithTiming
    :: TimingConfig
    -> String
    -> TraceConfig
    -> [InstEdge]
    -> PresolutionState p
    -> IO (Either PresolutionError ((), PresolutionState p))
runPresolutionLoopWithTiming timing label traceCfg edges initialState = do
    startResult <- runMeasuredStage traceCfg timing initialState requireValidBindingTree
    case startResult of
        Left err -> pure (Left err)
        Right ((), st1, startNs) -> do
            drainResult <- runMeasuredStage traceCfg timing st1 (drainPendingUnifyClosure traceCfg)
            case drainResult of
                Left err -> pure (Left err)
                Right ((), st2, drainNs) -> do
                    buildResult <-
                        runMeasuredStage traceCfg timing st2 (buildIndexedEdgeWorklist edges)
                    case buildResult of
                        Left err -> pure (Left err)
                        Right (worklist0, st3, indexNs) -> do
                            scheduledResult <-
                                runTimedScheduledEdges
                                    traceCfg
                                    timing
                                    st3
                                    emptyPresolutionLoopCounters
                                        { plcValidation = startNs
                                        , plcIndex = indexNs
                                        , plcDrainUnifyClosure = drainNs
                                        }
                                    Nothing
                                    worklist0
                            case scheduledResult of
                                Left err -> pure (Left err)
                                Right (mbLastOwner, st4, counters3) -> do
                                    boundaryResult <-
                                        runMeasuredStage traceCfg timing st4 $
                                            scheduleWeakensByOwnerBoundary mbLastOwner Nothing Nothing
                                    case boundaryResult of
                                        Left err -> pure (Left err)
                                        Right ((), st5, scheduleNs) -> do
                                            closureResult <-
                                                runMeasuredStage traceCfg timing st5 $
                                                    assertNoPendingUnifyEdges "after-inst-edge-closure" Nothing
                                            case closureResult of
                                                Left err -> pure (Left err)
                                                Right ((), st6, validationNs) -> do
                                                    endResult <- runMeasuredStage traceCfg timing st6 requireValidBindingTree
                                                    case endResult of
                                                        Left err -> pure (Left err)
                                                        Right ((), st7, endNs) -> do
                                                            let counters =
                                                                    counters3
                                                                        { plcValidation =
                                                                            plcValidation counters3 + validationNs + endNs
                                                                        , plcScheduleWeakens =
                                                                            plcScheduleWeakens counters3 + scheduleNs
                                                                        }
                                                            emitPresolutionLoopCounters timing label counters
                                                            pure (Right ((), st7))

runTimedScheduledEdges
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> PresolutionLoopCounters
    -> Maybe GenNodeId
    -> EdgeWorklist
    -> IO (Either PresolutionError (Maybe GenNodeId, PresolutionState p, PresolutionLoopCounters))
runTimedScheduledEdges traceCfg timing st0 counters0 mbActiveOwner worklist0 =
    case popEdgeWorkItem worklist0 of
        Nothing -> pure (Right (mbActiveOwner, st0, counters0))
        Just (item, worklist1) -> runTimedScheduledEdge traceCfg timing st0 counters0 mbActiveOwner item worklist1

runTimedScheduledEdge
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> PresolutionLoopCounters
    -> Maybe GenNodeId
    -> EdgeWorkItem
    -> EdgeWorklist
    -> IO (Either PresolutionError (Maybe GenNodeId, PresolutionState p, PresolutionLoopCounters))
runTimedScheduledEdge traceCfg timing st0 counters0 mbActiveOwner item worklist1 = do
    let edge = ewiEdge item
    assertBeforeResult <-
        runMeasuredStage traceCfg timing st0 $
            assertNoPendingUnifyEdgesOnly "before-inst-edge" (Just edge)
    case assertBeforeResult of
        Left err -> pure (Left err)
        Right ((), st1, assertBeforeNs) -> do
            actionResult <- runTimedInstEdgeAction traceCfg timing st1 item
            case actionResult of
                Left err -> pure (Left err)
                Right (action, st2, planNs) ->
                    case action of
                        EdgeWorkSkip -> do
                            let counters =
                                    counters0
                                        { plcValidation = plcValidation counters0 + assertBeforeNs
                                        , plcPlan = plcPlan counters0 + planNs
                                        }
                                worklist2 = noteInertEdge item worklist1
                            runTimedScheduledEdges traceCfg timing st2 counters mbActiveOwner worklist2
                        EdgeWorkRefresh plan -> do
                            let worklist2 =
                                    noteProcessedEdge edge (Just (edgeFactsFromPlanState (UnionFind.frWith (psUnionFind st2)) st2 plan)) worklist1
                                stageCounters =
                                    emptyPresolutionLoopCounters
                                        { plcValidation = assertBeforeNs
                                        , plcPlan = planNs
                                        }
                                counters =
                                    counters0
                                        `addPresolutionLoopCounters` stageCounters
                                        `addPresolutionLoopCounters` edgeProcessReasonCounters EdgeProcessStaleChanged
                            runTimedScheduledEdges traceCfg timing st2 counters mbActiveOwner worklist2
                        EdgeWorkProcess reason plan ->
                            runTimedProcessedEdge
                                traceCfg
                                timing
                                st1
                                st2
                                counters0
                                mbActiveOwner
                                edge
                                reason
                                plan
                                worklist1
                                assertBeforeNs
                                planNs

runTimedProcessedEdge
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> PresolutionState p
    -> PresolutionLoopCounters
    -> Maybe GenNodeId
    -> InstEdge
    -> EdgeProcessReason
    -> EdgePlan
    -> EdgeWorklist
    -> Word64
    -> Word64
    -> IO (Either PresolutionError (Maybe GenNodeId, PresolutionState p, PresolutionLoopCounters))
runTimedProcessedEdge
    traceCfg
    timing
    stBefore
    stPlanned
    counters0
    mbActiveOwner
    edge
    reason
    plan
    worklist1
    assertBeforeNs
    planNs = do
        let nextOwner = Just (eprSchemeOwnerGen plan)
        scheduleResult <-
            runMeasuredStage traceCfg timing stPlanned $
                scheduleWeakensByOwnerBoundary mbActiveOwner nextOwner (Just edge)
        case scheduleResult of
            Left err -> pure (Left err)
            Right ((), stScheduled, scheduleNs) -> do
                executeResult <-
                    runTimedEdgeExecution traceCfg timing stScheduled reason plan
                case executeResult of
                    Left err -> pure (Left err)
                    Right (stExecuted, executeNs, executeCounters, outcome) -> do
                        let baseCounters =
                                emptyPresolutionLoopCounters
                                    { plcValidation = assertBeforeNs
                                    , plcPlan = planNs
                                    , plcScheduleWeakens = scheduleNs
                                    , plcExecute = executeNs
                                    }
                            countersAfterExecute =
                                counters0
                                    `addPresolutionLoopCounters` baseCounters
                                    `addPresolutionLoopCounters` executeCounters
                                    `addPresolutionLoopCounters` edgeProcessReasonCounters reason
                            replayMutation = edgeMutationFromPlanVersions stBefore stExecuted plan
                        case outcome of
                            EdgeExecutionReplayNoop
                                | edgeMutationIsEmpty replayMutation -> do
                                    let worklistProcessed =
                                            noteProcessedEdge edge (Just (edgeFactsFromPlanState (UnionFind.frWith (psUnionFind stExecuted)) stExecuted plan)) worklist1
                                    runTimedScheduledEdges traceCfg timing stExecuted countersAfterExecute nextOwner worklistProcessed
                            _ ->
                                runTimedPostProcessedEdge
                                    traceCfg
                                    timing
                                    stBefore
                                    stExecuted
                                    countersAfterExecute
                                    nextOwner
                                    edge
                                    plan
                                    worklist1

runTimedPostProcessedEdge
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> PresolutionState p
    -> PresolutionLoopCounters
    -> Maybe GenNodeId
    -> InstEdge
    -> EdgePlan
    -> EdgeWorklist
    -> IO (Either PresolutionError (Maybe GenNodeId, PresolutionState p, PresolutionLoopCounters))
runTimedPostProcessedEdge traceCfg timing stBefore stExecuted counters0 nextOwner edge plan worklist1 = do
    canonicalizeResult <-
        runMeasuredStage traceCfg timing stExecuted $ do
            canonical' <- getCanonical
            canonicalizeEdgeTraceInteriorsWith canonical' (instEdgeId edge)
    case canonicalizeResult of
        Left err -> pure (Left err)
        Right ((), stCanonicalized, canonicalizeNs) -> do
            drainResult <-
                runMeasuredStage traceCfg timing stCanonicalized $
                    drainPendingUnifyClosure traceCfg
            case drainResult of
                Left err -> pure (Left err)
                Right ((), stDrained, drainNs) -> do
                    assertAfterResult <-
                        runMeasuredStage traceCfg timing stDrained $
                            assertNoPendingUnifyEdgesOnly "after-inst-edge-closure" (Just edge)
                    case assertAfterResult of
                        Left err -> pure (Left err)
                        Right ((), stAfter, assertAfterNs) -> do
                            let mutation = edgeMutationFromPlanVersions stBefore stAfter plan
                                worklistProcessed =
                                    noteProcessedEdge edge (Just (edgeFactsFromPlanState (UnionFind.frWith (psUnionFind stAfter)) stAfter plan)) worklist1
                                (invalidation, worklistInvalidated) =
                                    invalidateWorklistAfterMutation edge mutation worklistProcessed
                                stageCounters =
                                    emptyPresolutionLoopCounters
                                        { plcValidation = assertAfterNs
                                        , plcCanonicalizeTraceInteriors = canonicalizeNs
                                        , plcDrainUnifyClosure = drainNs
                                        }
                                counters =
                                    counters0
                                        `addPresolutionLoopCounters` stageCounters
                                        `addPresolutionLoopCounters` edgeInvalidationCounters invalidation
                            runTimedScheduledEdges traceCfg timing stAfter counters nextOwner worklistInvalidated

runTimedInstEdgeAction
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> EdgeWorkItem
    -> IO (Either PresolutionError (EdgeWorkAction, PresolutionState p, Word64))
runTimedInstEdgeAction traceCfg timing st0 item =
    runMeasuredStage traceCfg timing st0 $ do
        canonical <- getCanonical
        prepareInstEdgeActionForWorkItem canonical item

runTimedEdgeExecution
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> EdgeProcessReason
    -> EdgePlan
    -> IO (Either PresolutionError (PresolutionState p, Word64, PresolutionLoopCounters, EdgeExecutionOutcome))
runTimedEdgeExecution traceCfg timing st0 _reason plan = do
    -- Freeze binding snapshot for edge-local execution to avoid quotient rebuilds.
    decisionResult <- runMeasuredStage traceCfg timing st0 $ do
        freezeEdgeLocalBindingSnapshot
        canonical <- getCanonical
        prepareEdgeExecutionDecision canonical plan
    case decisionResult of
        Left err -> pure (Left err)
        Right (decision, st1, decideNs)
            | Just previousTrace <- eedReplayTrace decision -> do
                recordExpansionResult <- runMeasuredStage traceCfg timing st1 (recordEdgeExecutionExpansion decision)
                case recordExpansionResult of
                    Left err -> pure (Left err)
                    Right ((), st2, recordExpansionNs) -> do
                        unifyResult <- runMeasuredStage traceCfg timing st2 (unifyEdgeExecutionStructure decision)
                        case unifyResult of
                            Left err -> pure (Left err)
                            Right ((), st3, unifyNs) -> do
                                witnessPlanResult <- runMeasuredStage traceCfg timing st3 (prepareEdgeExecutionWitness decision)
                                case witnessPlanResult of
                                    Left err -> pure (Left err)
                                    Right (witnessContext, st4, witnessPlanNs) -> do
                                        traceResult <-
                                            runMeasuredStage traceCfg timing st4 $
                                                recordEdgeExecutionReplayTrace witnessContext previousTrace
                                        pure $ case traceResult of
                                            Left err -> Left err
                                            Right ((), st5, traceNs) ->
                                                let counters =
                                                        emptyPresolutionLoopCounters
                                                            { plcExecuteDecideExpansion = decideNs
                                                            , plcExecuteRecordExpansion = recordExpansionNs
                                                            , plcExecuteUnifyStructure = unifyNs
                                                            , plcExecuteWitnessPlan = witnessPlanNs
                                                            , plcExecuteRecordTrace = traceNs
                                                            , plcReplayHit = 1
                                                            , plcReplayTraceRebuild = 1
                                                            }
                                                    totalNs =
                                                        decideNs
                                                            + recordExpansionNs
                                                            + unifyNs
                                                            + witnessPlanNs
                                                            + traceNs
                                                in Right (clearEdgeLocalSnapshot st5, totalNs, counters, EdgeExecutionReplayTraceRebuilt)
            | otherwise -> do
                recordExpansionResult <- runMeasuredStage traceCfg timing st1 (recordEdgeExecutionExpansion decision)
                case recordExpansionResult of
                    Left err -> pure (Left err)
                    Right ((), st2, recordExpansionNs) -> do
                        unifyResult <- runMeasuredStage traceCfg timing st2 (unifyEdgeExecutionStructure decision)
                        case unifyResult of
                            Left err -> pure (Left err)
                            Right ((), st3, unifyNs) -> do
                                witnessPlanResult <- runMeasuredStage traceCfg timing st3 (prepareEdgeExecutionWitness decision)
                                case witnessPlanResult of
                                    Left err -> pure (Left err)
                                    Right (witnessContext, st4, witnessPlanNs) -> do
                                        expansionUnifyResult <-
                                            runTimedEdgeExpansionUnify traceCfg timing st4 witnessContext
                                        case expansionUnifyResult of
                                            Left err -> pure (Left err)
                                            Right (expansionResult, st5, expansionUnifyNs, expansionCounters) -> do
                                                traceResult <-
                                                    runMeasuredStage traceCfg timing st5 $
                                                        recordEdgeExecutionTrace witnessContext expansionResult
                                                case traceResult of
                                                    Left err -> pure (Left err)
                                                    Right ((), st6, traceNs) -> do
                                                        witnessResult <-
                                                            runMeasuredStage traceCfg timing st6 $
                                                                recordEdgeExecutionWitness witnessContext expansionResult
                                                        case witnessResult of
                                                            Left err -> pure (Left err)
                                                            Right ((), st7, witnessNs) -> do
                                                                let counters =
                                                                        emptyPresolutionLoopCounters
                                                                            { plcExecuteDecideExpansion = decideNs
                                                                            , plcExecuteRecordExpansion = recordExpansionNs
                                                                            , plcExecuteUnifyStructure = unifyNs
                                                                            , plcExecuteWitnessPlan = witnessPlanNs
                                                                            , plcExecuteExpansionUnify = expansionUnifyNs
                                                                            , plcExecuteExpansionApply = plcExecuteExpansionApply expansionCounters
                                                                            , plcExecuteExpansionBindRoot = plcExecuteExpansionBindRoot expansionCounters
                                                                            , plcExecuteExpansionPrepareOmega = plcExecuteExpansionPrepareOmega expansionCounters
                                                                            , plcExecuteExpansionExecuteOmega = plcExecuteExpansionExecuteOmega expansionCounters
                                                                            , plcExecuteExpansionFinish = plcExecuteExpansionFinish expansionCounters
                                                                            , plcExecuteRecordTrace = traceNs
                                                                            , plcExecuteRecordWitness = witnessNs
                                                                            , plcReplayFresh = 1
                                                                            }
                                                                    totalNs =
                                                                        decideNs
                                                                            + recordExpansionNs
                                                                            + unifyNs
                                                                            + witnessPlanNs
                                                                            + expansionUnifyNs
                                                                            + traceNs
                                                                            + witnessNs
                                                                pure (Right (clearEdgeLocalSnapshot st7, totalNs, counters, EdgeExecutionFreshOutcome))

runTimedEdgeExpansionUnify
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> EdgeExecutionWitnessContext
    -> IO (Either PresolutionError (EdgeExpansionResult, PresolutionState p, Word64, PresolutionLoopCounters))
runTimedEdgeExpansionUnify traceCfg timing st0 witnessContext
    | eedFinalExpansion (eewDecision witnessContext) == ExpIdentity = do
        result <- runMeasuredStage traceCfg timing st0 (runEdgeExecutionExpansionUnify witnessContext)
        pure $ case result of
            Left err -> Left err
            Right (expansionResult, st1, elapsed) ->
                Right (expansionResult, st1, elapsed, emptyPresolutionLoopCounters)
    | otherwise = do
        let input = eewExpansionInput witnessContext
            baseOps = ewpBaseOps (eewWitnessPlan witnessContext)
        applyResult <- runMeasuredStage traceCfg timing st0 (applyEdgeExpansion input baseOps)
        case applyResult of
            Left err -> pure (Left err)
            Right (applied, st1, applyNs) -> do
                bindResult <- runMeasuredStage traceCfg timing st1 (bindEdgeExpansionRoot applied)
                case bindResult of
                    Left err -> pure (Left err)
                    Right (bound, st2, bindNs) -> do
                        prepareResult <- runMeasuredStage traceCfg timing st2 (prepareEdgeExpansionOmega bound)
                        case prepareResult of
                            Left err -> pure (Left err)
                            Right (prepared, st3, prepareNs) -> do
                                executeResult <- runMeasuredStage traceCfg timing st3 (executeEdgeExpansionOmega prepared)
                                case executeResult of
                                    Left err -> pure (Left err)
                                    Right (executed, st4, executeNs) -> do
                                        finishResult <- runMeasuredStage traceCfg timing st4 (finishEdgeExpansionUnify executed)
                                        pure $ case finishResult of
                                            Left err -> Left err
                                            Right (expansionResult, st5, finishNs) ->
                                                let counters =
                                                        emptyPresolutionLoopCounters
                                                            { plcExecuteExpansionApply = applyNs
                                                            , plcExecuteExpansionBindRoot = bindNs
                                                            , plcExecuteExpansionPrepareOmega = prepareNs
                                                            , plcExecuteExpansionExecuteOmega = executeNs
                                                            , plcExecuteExpansionFinish = finishNs
                                                            }
                                                    totalNs = applyNs + bindNs + prepareNs + executeNs + finishNs
                                                 in Right (expansionResult, st5, totalNs, counters)

runMeasuredStage
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> PresolutionM p a
    -> IO (Either PresolutionError (a, PresolutionState p, Word64))
runMeasuredStage traceCfg timing st action = do
    (result, elapsed) <-
        measureProgramOperationIO timing $
            evaluate (runPresolutionM traceCfg st action)
    pure $ case result of
        Left err -> Left err
        Right (value, st') -> Right (value, st', elapsed)

emitPresolutionLoopCounters :: TimingConfig -> String -> PresolutionLoopCounters -> IO ()
emitPresolutionLoopCounters timing label counters = do
    emitProgramOperationDurationIO timing (label ++ ".validation") (plcValidation counters)
    emitProgramOperationDurationIO timing (label ++ ".index") (plcIndex counters)
    emitProgramOperationDurationIO timing (label ++ ".plan") (plcPlan counters)
    emitProgramOperationDurationIO timing (label ++ ".schedule_weakens") (plcScheduleWeakens counters)
    emitProgramOperationDurationIO timing (label ++ ".execute") (plcExecute counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.decide_expansion") (plcExecuteDecideExpansion counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.record_expansion") (plcExecuteRecordExpansion counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.unify_structure") (plcExecuteUnifyStructure counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.witness_plan") (plcExecuteWitnessPlan counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.expansion_unify") (plcExecuteExpansionUnify counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.expansion_unify.apply_expansion") (plcExecuteExpansionApply counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.expansion_unify.bind_root") (plcExecuteExpansionBindRoot counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.expansion_unify.prepare_omega") (plcExecuteExpansionPrepareOmega counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.expansion_unify.execute_omega") (plcExecuteExpansionExecuteOmega counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.expansion_unify.finish") (plcExecuteExpansionFinish counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.record_trace") (plcExecuteRecordTrace counters)
    emitProgramOperationDurationIO timing (label ++ ".execute.record_witness") (plcExecuteRecordWitness counters)
    emitProgramOperationDurationIO timing (label ++ ".canonicalize_trace_interiors") (plcCanonicalizeTraceInteriors counters)
    emitProgramOperationDurationIO timing (label ++ ".drain_unify_closure") (plcDrainUnifyClosure counters)
    emitProgramOperationDurationIO timing (label ++ ".stale.changed") (plcStaleChanged counters)
    emitProgramOperationDurationIO timing (label ++ ".replay.hit") (plcReplayHit counters)
    emitProgramOperationDurationIO timing (label ++ ".replay.fresh") (plcReplayFresh counters)
    emitProgramOperationDurationIO timing (label ++ ".replay.noop") (plcReplayNoop counters)
    emitProgramOperationDurationIO timing (label ++ ".replay.trace_rebuild") (plcReplayTraceRebuild counters)
    emitProgramOperationDurationIO timing (label ++ ".invalidated.root_edges") (plcInvalidatedRootEdges counters)
    emitProgramOperationDurationIO timing (label ++ ".invalidated.owner_edges") (plcInvalidatedOwnerEdges counters)
    emitProgramOperationDurationIO timing (label ++ ".invalidated.expansion_edges") (plcInvalidatedExpansionEdges counters)
    emitProgramOperationDurationIO timing (label ++ ".invalidated.edges") (plcInvalidatedEdges counters)

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM p ()
processInstEdge edge = do
    requireValidBindingTree
    canonical <- getCanonical
    plan <- prepareInstEdgePlan canonical edge
    executeEdgePlan canonical plan

prepareInstEdgePlan :: (NodeId -> NodeId) -> InstEdge -> PresolutionM p EdgePlan
prepareInstEdgePlan canonical edge = do
    ensureBindingParents
    planEdge canonical edge

prepareInstEdgeActionForWorkItem :: (NodeId -> NodeId) -> EdgeWorkItem -> PresolutionM p EdgeWorkAction
prepareInstEdgeActionForWorkItem canonical item = do
    case (ewiStale item, ewiPlanSeed item, ewiFingerprint item) of
        (True, Just seed, Just previous) -> do
            mbCurrent <- edgeFactsFromSeed canonical (ewiEdge item) seed
            case mbCurrent of
                Just (_, current)
                    | current == previous -> pure EdgeWorkSkip
                    | otherwise -> EdgeWorkRefresh <$> prepareInstEdgePlanForWorkItem canonical item
                Nothing -> EdgeWorkRefresh <$> prepareInstEdgePlanForWorkItem canonical item
        _ -> EdgeWorkProcess EdgeProcessNormal <$> prepareInstEdgePlanForWorkItem canonical item

prepareInstEdgePlanForWorkItem :: (NodeId -> NodeId) -> EdgeWorkItem -> PresolutionM p EdgePlan
prepareInstEdgePlanForWorkItem canonical item =
    case (ewiStale item, ewiPlanSeed item) of
        (False, Just seed) -> prepareInstEdgePlanFromSeed canonical (ewiEdge item) seed
        _ -> prepareInstEdgePlan canonical (ewiEdge item)

prepareInstEdgePlanFromSeed :: (NodeId -> NodeId) -> InstEdge -> EdgePlanSeed -> PresolutionM p EdgePlan
prepareInstEdgePlanFromSeed canonical edge seed = do
    ensureBindingParents
    n1Raw <- getNode (instLeft edge)
    case mkResolvedTyExp n1Raw of
        Just leftTyExp
            | leftTyExp == epsLeftTyExp seed -> do
                bodyRoot <- findRoot (rteBodyId leftTyExp)
                if bodyRoot /= epsBodyRoot seed
                    then planEdge canonical edge
                    else do
                        n2 <- getCanonicalNode (instRight edge)
                        leftRoot <- findRoot (instLeft edge)
                        rightRoot <- findRoot (instRight edge)
                        constraint0 <- getConstraint
                        let edgeId = instEdgeId edge
                            eidInt = getEdgeId edgeId
                            letEdgeTrivial = IntSet.member eidInt (cLetEdges constraint0)
                            allowTrivial =
                                letEdgeTrivial || isSynthesizedExpVar (rteExpVar leftTyExp)
                        pure
                            EdgePlanResolved
                                { eprEdge = edge
                                , eprLeftTyExp = leftTyExp
                                , eprRightNode = n2
                                , eprLeftCanonical = leftRoot
                                , eprRightCanonical = rightRoot
                                , eprAllowTrivial = allowTrivial
                                , eprSchemeOwnerGen = epsSchemeOwnerGen seed
                                }
        _ -> planEdge canonical edge

invalidateWorklistAfterMutation :: InstEdge -> EdgeMutation -> EdgeWorklist -> (EdgeInvalidation, EdgeWorklist)
invalidateWorklistAfterMutation edge mutation worklist0
    | edgeMutationIsEmpty mutation = (emptyEdgeInvalidation, worklist0)
    | otherwise =
        let excluded = IntSet.singleton (getEdgeId (instEdgeId edge))
            (rootInvalidation, worklist1) =
                invalidateRootsExcept excluded (emRoots mutation) worklist0
            (ownerInvalidation, worklist2) =
                invalidateOwnersExcept excluded (emOwners mutation) worklist1
            (expInvalidation, worklist3) =
                invalidateExpansionsExcept excluded (emExpansions mutation) worklist2
            invalidation =
                EdgeInvalidation
                    { eiRootEdges = wiEdges rootInvalidation
                    , eiOwnerEdges = wiEdges ownerInvalidation
                    , eiExpansionEdges = wiEdges expInvalidation
                    , eiEdges =
                        wiEdges rootInvalidation
                            `IntSet.union` wiEdges ownerInvalidation
                            `IntSet.union` wiEdges expInvalidation
                    }
        in (invalidation, worklist3)

edgeFactsFromPlanState :: (NodeId -> NodeId) -> PresolutionState p -> EdgePlan -> (EdgePlanSeed, EdgeFingerprint)
edgeFactsFromPlanState canonical st plan =
    ( EdgePlanSeed
        { epsLeftTyExp = leftTyExp
        , epsLeftRoot = leftRoot
        , epsRightRoot = rightRoot
        , epsBodyRoot = bodyRoot
        , epsSchemeOwnerGen = owner
        , epsExpansionVar = expVar
        }
    , fingerprint
    )
  where
    edge = eprEdge plan
    leftTyExp = eprLeftTyExp plan
    expVar = rteExpVar leftTyExp
    owner = eprSchemeOwnerGen plan
    leftRoot = canonical (instLeft edge)
    rightRoot = canonical (instRight edge)
    bodyRoot = canonical (rteBodyId leftTyExp)
    Presolution assignments = psPresolution st
    currentExpansion =
        IntMap.findWithDefault ExpIdentity (getExpVarId expVar) assignments
    fingerprint =
        EdgeFingerprint
            { efLeftRoot = leftRoot
            , efRightRoot = rightRoot
            , efBodyRoot = bodyRoot
            , efSchemeOwnerGen = owner
            , efExpansionVar = expVar
            , efCurrentExpansion = currentExpansion
            }

edgeFactsFromSeed :: (NodeId -> NodeId) -> InstEdge -> EdgePlanSeed -> PresolutionM p (Maybe (EdgePlanSeed, EdgeFingerprint))
edgeFactsFromSeed canonical edge seed = do
    n1Raw <- getNode (instLeft edge)
    case mkResolvedTyExp n1Raw of
        Just leftTyExp
            | leftTyExp == epsLeftTyExp seed ->
                Just <$> edgeFactsFromTyExp canonical edge leftTyExp
        _ -> pure Nothing

edgeFactsFromTyExp :: (NodeId -> NodeId) -> InstEdge -> ResolvedTyExp -> PresolutionM p (EdgePlanSeed, EdgeFingerprint)
edgeFactsFromTyExp canonical edge leftTyExp = do
    fingerprint <- edgeFingerprintFromTyExp canonical edge leftTyExp
    pure
        ( EdgePlanSeed
            { epsLeftTyExp = leftTyExp
            , epsLeftRoot = efLeftRoot fingerprint
            , epsRightRoot = efRightRoot fingerprint
            , epsBodyRoot = efBodyRoot fingerprint
            , epsSchemeOwnerGen = efSchemeOwnerGen fingerprint
            , epsExpansionVar = efExpansionVar fingerprint
            }
        , fingerprint
        )

edgeFingerprintFromTyExp :: (NodeId -> NodeId) -> InstEdge -> ResolvedTyExp -> PresolutionM p EdgeFingerprint
edgeFingerprintFromTyExp canonical edge leftTyExp = do
    snapshot <- getBindingSnapshot
    owner <- bindingSnapshotFindSchemeIntroducer snapshot (rteBodyId leftTyExp)
    st <- getPresolutionState
    let
        expVar = rteExpVar leftTyExp
        Presolution assignments = psPresolution st
        currentExpansion =
            IntMap.findWithDefault ExpIdentity (getExpVarId expVar) assignments
    pure
        EdgeFingerprint
            { efLeftRoot = canonical (instLeft edge)
            , efRightRoot = canonical (instRight edge)
            , efBodyRoot = canonical (rteBodyId leftTyExp)
            , efSchemeOwnerGen = owner
            , efExpansionVar = expVar
            , efCurrentExpansion = currentExpansion
            }

edgeMutationFromPlanVersions :: PresolutionState p -> PresolutionState p -> EdgePlan -> EdgeMutation
edgeMutationFromPlanVersions before after plan =
    EdgeMutation
        { emRoots = rootsChanged
        , emOwners = ownersChanged
        , emExpansions = expansionsChanged
        }
  where
    graphChanged = psGraphVersion before /= psGraphVersion after
    ufChanged = psUnionFindVersion before /= psUnionFindVersion after
    bindParentsChanged = psBindParentsVersion before /= psBindParentsVersion after

    rootsChanged
        | graphChanged || ufChanged || bindParentsChanged =
            IntSet.fromList
                [ getNodeId (eprLeftCanonical plan)
                , getNodeId (eprRightCanonical plan)
                , getNodeId (rteBodyId (eprLeftTyExp plan))
                ]
        | otherwise = IntSet.empty

    ownersChanged
        | bindParentsChanged = IntSet.singleton (getGenNodeId (eprSchemeOwnerGen plan))
        | otherwise = IntSet.empty

    Presolution beforeAssignments = psPresolution before
    Presolution afterAssignments = psPresolution after
    expVar = rteExpVar (eprLeftTyExp plan)

    expansionsChanged
        | IntMap.lookup (getExpVarId expVar) beforeAssignments
            /= IntMap.lookup (getExpVarId expVar) afterAssignments =
            IntSet.singleton (getExpVarId expVar)
        | otherwise = IntSet.empty

-- | Ensure the binding model cache is populated and freeze it as the edge-local
-- snapshot.  During edge-local execution, UF path-compression bumps
-- 'psUnionFindVersion', which would normally invalidate the cache and force a
-- quotient rebuild.  The frozen snapshot bypasses the version check so the
-- quotient built once before edge execution is reused throughout.
freezeEdgeLocalBindingSnapshot :: PresolutionM p ()
freezeEdgeLocalBindingSnapshot = do
    _ <- getBindingSnapshot
    st <- getPresolutionState
    case psBindingModelCache st of
        Just cached -> putPresolutionState (setEdgeLocalSnapshot cached st)
        Nothing -> pure ()

runScheduledWorklist
    :: TraceConfig
    -> Maybe GenNodeId
    -> EdgeWorklist
    -> PresolutionM p (Maybe GenNodeId, EdgeWorklist)
runScheduledWorklist traceCfg mbActiveOwner worklist0 =
    case popEdgeWorkItem worklist0 of
        Nothing -> pure (mbActiveOwner, worklist0)
        Just (item, worklist1) -> do
            let edge = ewiEdge item
            canonical <- getCanonical
            assertNoPendingUnifyEdgesOnly "before-inst-edge" (Just edge)
            stBefore <- getPresolutionState
            action <- prepareInstEdgeActionForWorkItem canonical item
            case action of
                EdgeWorkSkip ->
                    runScheduledWorklist traceCfg mbActiveOwner (noteInertEdge item worklist1)
                EdgeWorkRefresh plan -> do
                    stRefreshed <- getPresolutionState
                    let worklistRefreshed =
                            noteProcessedEdge edge (Just (edgeFactsFromPlanState canonical stRefreshed plan)) worklist1
                    runScheduledWorklist traceCfg mbActiveOwner worklistRefreshed
                EdgeWorkProcess _reason plan -> do
                    let nextOwner = Just (eprSchemeOwnerGen plan)
                    scheduleWeakensByOwnerBoundary mbActiveOwner nextOwner (Just edge)
                    freezeEdgeLocalBindingSnapshot
                    outcome <- executeEdgePlanWithoutTraceCanonicalizationWithOutcome canonical plan
                    stExecuted <- getPresolutionState
                    putPresolutionState (clearEdgeLocalSnapshot stExecuted)
                    let replayMutation = edgeMutationFromPlanVersions stBefore stExecuted plan
                    case outcome of
                        EdgeExecutionReplayNoop
                            | edgeMutationIsEmpty replayMutation -> do
                                let worklistProcessed =
                                        noteProcessedEdge edge (Just (edgeFactsFromPlanState canonical stExecuted plan)) worklist1
                                runScheduledWorklist traceCfg nextOwner worklistProcessed
                        _ -> do
                            canonical' <- getCanonical
                            canonicalizeEdgeTraceInteriorsWith canonical' (instEdgeId edge)
                            drainPendingUnifyClosure traceCfg
                            assertNoPendingUnifyEdgesOnly "after-inst-edge-closure" (Just edge)
                            stAfter <- getPresolutionState
                            let mutation = edgeMutationFromPlanVersions stBefore stAfter plan
                                worklistProcessed =
                                    noteProcessedEdge edge (Just (edgeFactsFromPlanState (UnionFind.frWith (psUnionFind stAfter)) stAfter plan)) worklist1
                                (_invalidation, worklistInvalidated) =
                                    invalidateWorklistAfterMutation edge mutation worklistProcessed
                            runScheduledWorklist traceCfg nextOwner worklistInvalidated

-- | Boundary scheduler for delayed weakens.
--
-- Flush only when crossing owner groups (or exiting the edge loop) so pending
-- weakens remain edge-local within an owner but cannot leak across boundaries.
scheduleWeakensByOwnerBoundary :: Maybe GenNodeId -> Maybe GenNodeId -> Maybe InstEdge -> PresolutionM p ()
scheduleWeakensByOwnerBoundary mbCurrentOwner mbNextOwner mbEdge =
    when (ownerBoundaryChanged mbCurrentOwner mbNextOwner) $ do
        assertNoPendingUnifyEdgesOnly "owner-boundary-before-weaken-flush" mbEdge
        let boundaryOwner = pendingWeakenOwnerFromMaybe mbCurrentOwner
        flushPendingWeakensAtOwnerBoundary boundaryOwner
        assertNoPendingWeakensOutsideOwnerBoundary
            "owner-boundary-after-weaken-flush"
            boundaryOwner
            mbNextOwner
            mbEdge
  where
    ownerBoundaryChanged :: Maybe GenNodeId -> Maybe GenNodeId -> Bool
    ownerBoundaryChanged (Just ownerA) (Just ownerB) = ownerA /= ownerB
    ownerBoundaryChanged (Just _) Nothing = True
    ownerBoundaryChanged _ _ = False

assertNoPendingWeakensOutsideOwnerBoundary
    :: String
    -> PendingWeakenOwner
    -> Maybe GenNodeId
    -> Maybe InstEdge
    -> PresolutionM p ()
assertNoPendingWeakensOutsideOwnerBoundary phase boundaryOwner mbNextOwner mbEdge = do
    owners <- pendingWeakenOwners
    let allowedAfterBoundary = case mbNextOwner of
            Nothing -> []
            Just nextOwner -> [pendingWeakenOwnerFromMaybe (Just nextOwner)]
        residualOwners = filter (`notElem` allowedAfterBoundary) owners
    unless (null residualOwners) $
        throwPresolutionError $
            InternalError
                ( "presolution boundary violation ("
                    ++ phase
                    ++ ")"
                    ++ edgeCtx
                    ++ ": pending weakens remained after owner-boundary flush for closed owner "
                    ++ show boundaryOwner
                    ++ ", allowed owners after boundary = "
                    ++ show allowedAfterBoundary
                    ++ ", remaining owners = "
                    ++ show owners
                )
  where
    edgeCtx = case mbEdge of
        Nothing -> ""
        Just edge -> " at edge " ++ show edge

drainPendingUnifyClosure :: TraceConfig -> PresolutionM p ()
drainPendingUnifyClosure traceCfg = do
    pendingUnify <- getPendingUnifyEdgesM
    if null pendingUnify
        then pure ()
        else do
            (constraint0, unionFind0) <- getConstraintAndUnionFind
            let cPrepared = repairNonUpperParents constraint0
                closureResult = runUnifyClosureWithSeed traceCfg unionFind0 cPrepared
            closure <- either (throwPresolutionError . closureError) pure closureResult
            putConstraintAndUnionFind (ucConstraint closure) (ucUnionFind closure)

assertNoPendingUnifyEdges :: String -> Maybe InstEdge -> PresolutionM p ()
assertNoPendingUnifyEdges phase mbEdge = do
    pendingUnify <- getPendingUnifyEdgesM
    pendingWeakens <- getPendingWeakensM
    unless (null pendingUnify && IntSet.null pendingWeakens) $
        do
            pendingOwners <- pendingWeakenOwners
            throwPresolutionError $
                InternalError
                    ( "presolution boundary violation ("
                        ++ phase
                        ++ ")"
                        ++ edgeCtx
                        ++ ": pending unify edges = "
                        ++ show pendingUnify
                        ++ ", pending weakens = "
                        ++ show (IntSet.toList pendingWeakens)
                        ++ ", pending weaken owners = "
                        ++ show pendingOwners
                    )
  where
    edgeCtx = case mbEdge of
        Nothing -> ""
        Just edge -> " at edge " ++ show edge

assertNoPendingUnifyEdgesOnly :: String -> Maybe InstEdge -> PresolutionM p ()
assertNoPendingUnifyEdgesOnly phase mbEdge = do
    pendingUnify <- getPendingUnifyEdgesM
    pendingWeakens <- getPendingWeakensM
    unless (null pendingUnify) $
        throwPresolutionError $
            InternalError
                ( "presolution boundary violation ("
                    ++ phase
                    ++ ")"
                    ++ edgeCtx
                    ++ ": pending unify edges = "
                    ++ show pendingUnify
                    ++ ", pending weakens (allowed within source group) = "
                    ++ show (IntSet.toList pendingWeakens)
                )
  where
    edgeCtx = case mbEdge of
        Nothing -> ""
        Just edge -> " at edge " ++ show edge

closureError :: SolveError -> PresolutionError
closureError err =
    InternalError ("presolution runUnifyClosure failed: " ++ show err)
