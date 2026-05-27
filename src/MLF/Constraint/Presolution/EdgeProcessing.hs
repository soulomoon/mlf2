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
import Control.Monad (foldM, unless, when)
import Data.Word (Word64)
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph (GenNodeId, InstEdge(..))
import MLF.Constraint.Types.Witness (Expansion(..))
import MLF.Constraint.Presolution.Base
    ( MonadPresolution(..)
    , PresolutionError(..)
    , PresolutionM
    , PendingWeakenOwner
    , pendingWeakenOwnerFromMaybe
    , ensureBindingParents
    , requireValidBindingTree
    , runPresolutionM
    , PresolutionState
    )
import MLF.Constraint.Presolution.EdgeUnify
    ( flushPendingWeakensAtOwnerBoundary
    )
import MLF.Constraint.Presolution.EdgeUnify.Omega
    ( pendingWeakenOwners
    )
import MLF.Constraint.Presolution.StateAccess
    ( getConstraintAndUnionFind
    , getPendingUnifyEdgesM
    , getPendingWeakensM
    , putConstraintAndUnionFind
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan (EdgePlan(..))
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter
    ( executeEdgePlan
    , EdgeExecutionDecision (..)
    , EdgeExecutionWitnessContext (..)
    , prepareEdgeExecutionDecision
    , recordEdgeExecutionExpansion
    , unifyEdgeExecutionStructure
    , prepareEdgeExecutionWitness
    , runEdgeExecutionExpansionUnify
    , recordEdgeExecutionTrace
    , recordEdgeExecutionWitness
    )
import MLF.Constraint.Presolution.EdgeProcessing.Solve
    ( canonicalizeEdgeTraceInteriorsM
    )
import MLF.Constraint.Presolution.EdgeProcessing.Unify
    ( EdgeExpansionResult
    , applyEdgeExpansion
    , bindEdgeExpansionRoot
    , prepareEdgeExpansionOmega
    , executeEdgeExpansionOmega
    , finishEdgeExpansionUnify
    )
import MLF.Constraint.Presolution.Witness (EdgeWitnessPlan(..))
import MLF.Constraint.Solve (repairNonUpperParents)
import MLF.Constraint.Unify.Closure (SolveError, UnifyClosureResult(..), runUnifyClosureWithSeed)
import MLF.Util.Timing
    ( TimingConfig
    , emitProgramOperationDurationIO
    , measureProgramOperationIO
    )
import MLF.Util.Trace (TraceConfig)

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: TraceConfig -> [InstEdge] -> PresolutionM p ()
runPresolutionLoop traceCfg edges = do
    requireValidBindingTree
    drainPendingUnifyClosure traceCfg
    mbLastOwner <- foldM (runScheduledEdge traceCfg) Nothing edges
    scheduleWeakensByOwnerBoundary mbLastOwner Nothing Nothing
    assertNoPendingUnifyEdges "after-inst-edge-closure" Nothing
    requireValidBindingTree

data PresolutionLoopCounters = PresolutionLoopCounters
    { plcValidation :: !Word64
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
    }

emptyPresolutionLoopCounters :: PresolutionLoopCounters
emptyPresolutionLoopCounters =
    PresolutionLoopCounters
        { plcValidation = 0
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
        }

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
                    scheduledResult <-
                        runTimedScheduledEdges
                            traceCfg
                            timing
                            st2
                            emptyPresolutionLoopCounters
                                { plcValidation = startNs
                                , plcDrainUnifyClosure = drainNs
                                }
                            Nothing
                            edges
                    case scheduledResult of
                        Left err -> pure (Left err)
                        Right (mbLastOwner, st3, counters3) -> do
                            boundaryResult <-
                                runMeasuredStage traceCfg timing st3 $
                                    scheduleWeakensByOwnerBoundary mbLastOwner Nothing Nothing
                            case boundaryResult of
                                Left err -> pure (Left err)
                                Right ((), st4, scheduleNs) -> do
                                    closureResult <-
                                        runMeasuredStage traceCfg timing st4 $
                                            assertNoPendingUnifyEdges "after-inst-edge-closure" Nothing
                                    case closureResult of
                                        Left err -> pure (Left err)
                                        Right ((), st5, validationNs) -> do
                                            endResult <- runMeasuredStage traceCfg timing st5 requireValidBindingTree
                                            case endResult of
                                                Left err -> pure (Left err)
                                                Right ((), st6, endNs) -> do
                                                    let counters =
                                                            counters3
                                                                { plcValidation =
                                                                    plcValidation counters3 + validationNs + endNs
                                                                , plcScheduleWeakens =
                                                                    plcScheduleWeakens counters3 + scheduleNs
                                                                }
                                                    emitPresolutionLoopCounters timing label counters
                                                    pure (Right ((), st6))

runTimedScheduledEdges
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> PresolutionLoopCounters
    -> Maybe GenNodeId
    -> [InstEdge]
    -> IO (Either PresolutionError (Maybe GenNodeId, PresolutionState p, PresolutionLoopCounters))
runTimedScheduledEdges _ _ st counters mbActiveOwner [] =
    pure (Right (mbActiveOwner, st, counters))
runTimedScheduledEdges traceCfg timing st0 counters0 mbActiveOwner (edge : rest) = do
    assertBeforeResult <-
        runMeasuredStage traceCfg timing st0 $
            assertNoPendingUnifyEdgesOnly "before-inst-edge" (Just edge)
    case assertBeforeResult of
        Left err -> pure (Left err)
        Right ((), st1, assertBeforeNs) -> do
            planResult <- runTimedInstEdgePlan traceCfg timing st1 edge
            case planResult of
                Left err -> pure (Left err)
                Right (plan, st2, planNs) -> do
                    let nextOwner = Just (eprSchemeOwnerGen plan)
                    scheduleResult <-
                        runMeasuredStage traceCfg timing st2 $
                            scheduleWeakensByOwnerBoundary mbActiveOwner nextOwner (Just edge)
                    case scheduleResult of
                        Left err -> pure (Left err)
                        Right ((), st3, scheduleNs) -> do
                            executeResult <-
                                runTimedEdgeExecution traceCfg timing st3 plan
                            case executeResult of
                                Left err -> pure (Left err)
                                Right (st4, executeNs, executeCounters) -> do
                                    canonicalizeResult <-
                                        runMeasuredStage traceCfg timing st4 canonicalizeEdgeTraceInteriorsM
                                    case canonicalizeResult of
                                        Left err -> pure (Left err)
                                        Right ((), st5, canonicalizeNs) -> do
                                            drainResult <-
                                                runMeasuredStage traceCfg timing st5 $
                                                    drainPendingUnifyClosure traceCfg
                                            case drainResult of
                                                Left err -> pure (Left err)
                                                Right ((), st6, drainNs) -> do
                                                    assertAfterResult <-
                                                        runMeasuredStage traceCfg timing st6 $
                                                            assertNoPendingUnifyEdgesOnly "after-inst-edge-closure" (Just edge)
                                                    case assertAfterResult of
                                                        Left err -> pure (Left err)
                                                        Right ((), st7, assertAfterNs) -> do
                                                            let counters =
                                                                    counters0
                                                                        { plcValidation =
                                                                            plcValidation counters0 + assertBeforeNs + assertAfterNs
                                                                        , plcPlan = plcPlan counters0 + planNs
                                                                        , plcScheduleWeakens =
                                                                            plcScheduleWeakens counters0 + scheduleNs
                                                                        , plcExecute = plcExecute counters0 + executeNs
                                                                        , plcExecuteDecideExpansion =
                                                                            plcExecuteDecideExpansion counters0 + plcExecuteDecideExpansion executeCounters
                                                                        , plcExecuteRecordExpansion =
                                                                            plcExecuteRecordExpansion counters0 + plcExecuteRecordExpansion executeCounters
                                                                        , plcExecuteUnifyStructure =
                                                                            plcExecuteUnifyStructure counters0 + plcExecuteUnifyStructure executeCounters
                                                                        , plcExecuteWitnessPlan =
                                                                            plcExecuteWitnessPlan counters0 + plcExecuteWitnessPlan executeCounters
                                                                        , plcExecuteExpansionUnify =
                                                                            plcExecuteExpansionUnify counters0 + plcExecuteExpansionUnify executeCounters
                                                                        , plcExecuteExpansionApply =
                                                                            plcExecuteExpansionApply counters0 + plcExecuteExpansionApply executeCounters
                                                                        , plcExecuteExpansionBindRoot =
                                                                            plcExecuteExpansionBindRoot counters0 + plcExecuteExpansionBindRoot executeCounters
                                                                        , plcExecuteExpansionPrepareOmega =
                                                                            plcExecuteExpansionPrepareOmega counters0 + plcExecuteExpansionPrepareOmega executeCounters
                                                                        , plcExecuteExpansionExecuteOmega =
                                                                            plcExecuteExpansionExecuteOmega counters0 + plcExecuteExpansionExecuteOmega executeCounters
                                                                        , plcExecuteExpansionFinish =
                                                                            plcExecuteExpansionFinish counters0 + plcExecuteExpansionFinish executeCounters
                                                                        , plcExecuteRecordTrace =
                                                                            plcExecuteRecordTrace counters0 + plcExecuteRecordTrace executeCounters
                                                                        , plcExecuteRecordWitness =
                                                                            plcExecuteRecordWitness counters0 + plcExecuteRecordWitness executeCounters
                                                                        , plcCanonicalizeTraceInteriors =
                                                                            plcCanonicalizeTraceInteriors counters0 + canonicalizeNs
                                                                        , plcDrainUnifyClosure =
                                                                            plcDrainUnifyClosure counters0 + drainNs
                                                                        }
                                                            runTimedScheduledEdges traceCfg timing st7 counters nextOwner rest

runTimedInstEdgePlan
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> InstEdge
    -> IO (Either PresolutionError (EdgePlan, PresolutionState p, Word64))
runTimedInstEdgePlan traceCfg timing st0 edge =
    runMeasuredStage traceCfg timing st0 (prepareInstEdgePlan edge)

runTimedEdgeExecution
    :: TraceConfig
    -> TimingConfig
    -> PresolutionState p
    -> EdgePlan
    -> IO (Either PresolutionError (PresolutionState p, Word64, PresolutionLoopCounters))
runTimedEdgeExecution traceCfg timing st0 plan = do
    decisionResult <- runMeasuredStage traceCfg timing st0 (prepareEdgeExecutionDecision plan)
    case decisionResult of
        Left err -> pure (Left err)
        Right (decision, st1, decideNs) -> do
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
                                                                        }
                                                                totalNs =
                                                                    decideNs
                                                                        + recordExpansionNs
                                                                        + unifyNs
                                                                        + witnessPlanNs
                                                                        + expansionUnifyNs
                                                                        + traceNs
                                                                        + witnessNs
                                                            pure (Right (st7, totalNs, counters))

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

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM p ()
processInstEdge edge = do
    requireValidBindingTree
    plan <- prepareInstEdgePlan edge
    executeEdgePlan plan

prepareInstEdgePlan :: InstEdge -> PresolutionM p EdgePlan
prepareInstEdgePlan edge = do
    ensureBindingParents
    planEdge edge

runScheduledEdge :: TraceConfig -> Maybe GenNodeId -> InstEdge -> PresolutionM p (Maybe GenNodeId)
runScheduledEdge traceCfg mbActiveOwner edge = do
    assertNoPendingUnifyEdgesOnly "before-inst-edge" (Just edge)
    plan <- prepareInstEdgePlan edge
    let nextOwner = Just (eprSchemeOwnerGen plan)
    scheduleWeakensByOwnerBoundary mbActiveOwner nextOwner (Just edge)
    executeEdgePlan plan
    drainPendingUnifyClosure traceCfg
    assertNoPendingUnifyEdgesOnly "after-inst-edge-closure" (Just edge)
    pure nextOwner

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
