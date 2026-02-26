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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types (InstEdge, NodeId(..), cUnifyEdges)
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
import MLF.Constraint.Solve (repairNonUpperParents, rewriteConstraintWithUF)
import MLF.Constraint.Unify.Closure (SolveError, UnifyClosureResult(..), runUnifyClosure)
import MLF.Util.Trace (TraceConfig)
import qualified MLF.Util.UnionFind as UnionFind

-- | The main loop processing sorted instantiation edges.
runPresolutionLoop :: TraceConfig -> [InstEdge] -> PresolutionM ()
runPresolutionLoop traceCfg edges = do
    drainPendingUnifyClosure traceCfg
    forM_ edges $ \edge -> do
        processInstEdge edge
        drainPendingUnifyClosure traceCfg

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
            let cCanon =
                    repairNonUpperParents
                        (rewriteConstraintWithUF (psUnionFind st) (psConstraint st))
                closureResult = runUnifyClosure traceCfg cCanon
            closure <- either (throwPresolutionError . closureError) pure closureResult
            let uf' = composeUnionFind (psUnionFind st) (ucUnionFind closure)
            putPresolutionState
                st
                    { psConstraint = ucConstraint closure
                    , psUnionFind = uf'
                    }

composeUnionFind :: IntMap NodeId -> IntMap NodeId -> IntMap NodeId
composeUnionFind ufOld ufNew =
    let oldCanon = UnionFind.frWith ufOld
        newCanon = UnionFind.frWith ufNew
        keys = IntSet.fromList (IntMap.keys ufOld ++ IntMap.keys ufNew)
    in IntMap.fromList
        [ (k, rep)
        | k <- IntSet.toList keys
        , let nid = NodeId k
              rep = newCanon (oldCanon nid)
        , rep /= nid
        ]

closureError :: SolveError -> PresolutionError
closureError err =
    InternalError ("presolution runUnifyClosure failed: " ++ show err)
