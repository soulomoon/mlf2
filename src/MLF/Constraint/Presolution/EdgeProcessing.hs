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
    processInstEdge,
    -- * Re-exports from Solve
    unifyStructure,
    recordEdgeWitness,
    recordEdgeTrace,
    canonicalizeEdgeTraceInteriorsM,
) where

import Control.Monad (foldM, unless, when)
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types (GenNodeId, InstEdge(..), cUnifyEdges)
import MLF.Constraint.Presolution.Base
    ( MonadPresolution(..)
    , PresolutionError(..)
    , PresolutionM
    , PresolutionState(..)
    , PendingWeakenOwner
    , pendingWeakenOwnerFromMaybe
    , requireValidBindingTree
    )
import MLF.Constraint.Presolution.EdgeUnify
    ( flushPendingWeakensAtOwnerBoundary
    , pendingWeakenOwners
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan (EdgePlan(..))
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter (executeEdgePlan)
import MLF.Constraint.Presolution.EdgeProcessing.Solve (
    unifyStructure,
    recordEdgeWitness,
    recordEdgeTrace,
    canonicalizeEdgeTraceInteriorsM,
    )
import MLF.Constraint.Solve (repairNonUpperParents)
import MLF.Constraint.Unify.Closure (SolveError, UnifyClosureResult(..), runUnifyClosureWithSeed)
import MLF.Util.Trace (TraceConfig)

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: TraceConfig -> [InstEdge] -> PresolutionM ()
runPresolutionLoop traceCfg edges = do
    drainPendingUnifyClosure traceCfg
    mbLastOwner <- foldM (runScheduledEdge traceCfg) Nothing edges
    scheduleWeakensByOwnerBoundary mbLastOwner Nothing Nothing
    assertNoPendingUnifyEdges "after-inst-edge-closure" Nothing

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
    plan <- prepareInstEdgePlan edge
    executeEdgePlan plan

prepareInstEdgePlan :: InstEdge -> PresolutionM EdgePlan
prepareInstEdgePlan edge = do
    requireValidBindingTree
    planEdge edge

runScheduledEdge :: TraceConfig -> Maybe GenNodeId -> InstEdge -> PresolutionM (Maybe GenNodeId)
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
scheduleWeakensByOwnerBoundary :: Maybe GenNodeId -> Maybe GenNodeId -> Maybe InstEdge -> PresolutionM ()
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
    -> PresolutionM ()
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

drainPendingUnifyClosure :: TraceConfig -> PresolutionM ()
drainPendingUnifyClosure traceCfg = do
    st <- getPresolutionState
    if null (cUnifyEdges (psConstraint st))
        then pure ()
        else do
            let cPrepared = repairNonUpperParents (psConstraint st)
                closureResult = runUnifyClosureWithSeed traceCfg (psUnionFind st) cPrepared
            closure <- either (throwPresolutionError . closureError) pure closureResult
            putPresolutionState
                st
                    { psConstraint = ucConstraint closure
                    , psUnionFind = ucUnionFind closure
                    }

assertNoPendingUnifyEdges :: String -> Maybe InstEdge -> PresolutionM ()
assertNoPendingUnifyEdges phase mbEdge = do
    st <- getPresolutionState
    let pendingUnify = cUnifyEdges (psConstraint st)
        pendingWeakens = psPendingWeakens st
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

assertNoPendingUnifyEdgesOnly :: String -> Maybe InstEdge -> PresolutionM ()
assertNoPendingUnifyEdgesOnly phase mbEdge = do
    st <- getPresolutionState
    let pendingUnify = cUnifyEdges (psConstraint st)
        pendingWeakens = psPendingWeakens st
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
