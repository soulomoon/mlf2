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

import Control.Monad (forM_)

import MLF.Constraint.Types (InstEdge)
import MLF.Constraint.Presolution.Base (PresolutionM, requireValidBindingTree)
import MLF.Constraint.Presolution.EdgeProcessing.Planner (planEdge)
import MLF.Constraint.Presolution.EdgeProcessing.Interpreter (executeEdgePlan)
import MLF.Constraint.Presolution.EdgeProcessing.Solve (
    unifyStructure,
    recordEdgeWitness,
    recordEdgeTrace,
    canonicalizeEdgeTraceInteriorsM,
    )

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: [InstEdge] -> PresolutionM ()
runPresolutionLoop edges = forM_ edges processInstEdge

-- | Process a single instantiation edge.
processInstEdge :: InstEdge -> PresolutionM ()
processInstEdge edge = do
    requireValidBindingTree
    plan <- planEdge edge
    executeEdgePlan plan
