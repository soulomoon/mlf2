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

import Control.Monad (forM_, unless)

import MLF.Constraint.Types (InstEdge, cUnifyEdges)
import MLF.Constraint.Presolution.Base
    ( MonadPresolution(..)
    , PresolutionError(..)
    , PresolutionM
    , PresolutionState(..)
    , requireValidBindingTree
    )
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
    forM_ edges $ \edge -> do
        assertNoPendingUnifyEdges "before-inst-edge" (Just edge)
        processInstEdge edge
        drainPendingUnifyClosure traceCfg
        assertNoPendingUnifyEdges "after-inst-edge-closure" (Just edge)

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
    requireValidBindingTree
    plan <- planEdge edge
    executeEdgePlan plan

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
    let pending = cUnifyEdges (psConstraint st)
    unless (null pending) $
        throwPresolutionError $
            InternalError
                ( "presolution boundary violation ("
                    ++ phase
                    ++ ")"
                    ++ edgeCtx
                    ++ ": pending unify edges = "
                    ++ show pending
                )
  where
    edgeCtx = case mbEdge of
        Nothing -> ""
        Just edge -> " at edge " ++ show edge

closureError :: SolveError -> PresolutionError
closureError err =
    InternalError ("presolution runUnifyClosure failed: " ++ show err)
