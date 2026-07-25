{- |
Module      : MLF.Constraint.Presolution.EdgeUnify
Description : Facade for edge-local unification and ω execution helpers

This module remains the public/export owner for the edge-local unification
helpers used during presolution, while the implementation lives in focused
child modules under @MLF.Constraint.Presolution.EdgeUnify.@
-}
module MLF.Constraint.Presolution.EdgeUnify (
    EdgeUnifyStats(..),
    EdgeUnifyState(..),
    EdgeUnifyM,
    MonadEdgeUnify(..),
    addEdgeUnifyStats,
    emptyEdgeUnifyStats,
    executeEdgeLocalOmegaOps,
    flushPendingWeakensAtOwnerBoundary,
    initEdgeUnifyState,
    initEdgeUnifyStateWithCopyMap,
    initEdgeUnifyStateWithStats,
    mkOmegaExecEnv,
    recordEdgeUnifyStat,
    recordEdgeUnifyStatN,
    runEdgeUnifyForTest,
    runEdgeStructureUnifyForTest,
    runEdgeTerminalStructureUnifyForTest,
    runEdgeUnifyWithBinderMetasForTest,
    sourceWitnessNodeWithCopyMapForTest,
    runEdgeBoundInstallForTest,
    constructNondegenerateIdentityTerminalRootAuthority,
    constructUncopiedTerminalRootAuthority,
    unifyAcyclicEdge,
    recordExpansionWrapperResult,
    unifyStructureEdge,
    unifyQuotientTerminalStructureEdge,
    unifyTerminalStructureEdge,
    unifyUncopiedTerminalStructureEdge
) where

import Control.Monad.State (runStateT)
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Base (
    CopyMap,
    EdgeSourceInterior,
    PendingWeakenOwner(..),
    PresolutionM,
    requireValidBindingTree
    )
import MLF.Constraint.Presolution.EdgeUnify.State (
    EdgeUnifyStats(..),
    EdgeUnifyM,
    EdgeUnifyState(..),
    MonadEdgeUnify(..),
    addEdgeUnifyStats,
    emptyEdgeUnifyStats,
    initEdgeUnifyState,
    initEdgeUnifyStateWithCopyMap,
    initEdgeUnifyStateWithStats,
    mkOmegaExecEnv,
    recordEdgeUnifyStat,
    recordEdgeUnifyStatN,
    sourceWitnessNode
    )
import qualified MLF.Constraint.Presolution.EdgeUnify.Omega as Omega
import MLF.Constraint.Presolution.EdgeUnify.Omega (
    flushPendingWeakensAtOwnerBoundary
    )
import MLF.Constraint.Presolution.EdgeUnify.Unify (
    constructNondegenerateIdentityTerminalRootAuthority,
    constructUncopiedTerminalRootAuthority,
    recordExpansionWrapperResult,
    unifyAcyclicEdge,
    unifyQuotientTerminalStructureEdge,
    unifyStructureEdge,
    unifyTerminalStructureEdge,
    unifyUncopiedTerminalStructureEdge
    )
import MLF.Constraint.Presolution.Witness (edgeWitnessInstanceOp)
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Constraint.Types.Witness (InstanceOp)
import qualified MLF.Witness.OmegaExec as OmegaExec

-- | Testing helper: run a single edge-local unification and return the recorded
-- instance-operation witness slice.
--
-- This bypasses expansion copying and is intended for unit tests that want to
-- assert the precise `OpRaise` targets produced by binding-parent harmonization
-- (including the “no spray” behavior for interior nodes).
runEdgeUnifyForTest
    :: NodeId -- ^ edge root (for ≺ ordering keys)
    -> IntSet.IntSet -- ^ interior nodes (I(r))
    -> NodeId -- ^ left node to unify
    -> NodeId -- ^ right node to unify
    -> PresolutionM p [InstanceOp]
runEdgeUnifyForTest edgeRoot interior n1 n2 = do
    runEdgeUnifyWithBinderMetasForTest edgeRoot interior [] n1 n2

-- | Testing helper for the recursive structural unifier.  Unlike
-- 'runEdgeUnifyForTest', this exercises the post-union variable-bound rules
-- used when matching children of a rigid structure.
runEdgeStructureUnifyForTest
    :: NodeId
    -> IntSet.IntSet
    -> NodeId
    -> NodeId
    -> PresolutionM p [InstanceOp]
runEdgeStructureUnifyForTest edgeRoot interior n1 n2 = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState [] interior edgeRoot PendingWeakenOwnerUnknown
    (_a, eu1) <- runStateT (unifyStructureEdge n1 n2) eu0
    pure (map edgeWitnessInstanceOp (reverse (eusOps eu1)))

-- | Testing helper for the children-first terminal structural seam.  Locked
-- recursive structure must reach the binder-aware root matcher before any
-- recursive occurrence is considered for UF merging or Raise.
runEdgeTerminalStructureUnifyForTest
    :: NodeId
    -> IntSet.IntSet
    -> NodeId
    -> NodeId
    -> PresolutionM p [InstanceOp]
runEdgeTerminalStructureUnifyForTest edgeRoot interior n1 n2 = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState [] interior edgeRoot PendingWeakenOwnerUnknown
    (_a, eu1) <- runStateT (unifyTerminalStructureEdge n1 n2) eu0
    pure (map edgeWitnessInstanceOp (reverse (eusOps eu1)))

-- | Testing helper for the edge-local binder-meta path.  It keeps the
-- binder-meta map test-only while exercising the same witnessed bound install
-- used by production expansion unification.
runEdgeUnifyWithBinderMetasForTest
    :: NodeId
    -> IntSet.IntSet
    -> [(NodeId, NodeId)]
    -> NodeId
    -> NodeId
    -> PresolutionM p [InstanceOp]
runEdgeUnifyWithBinderMetasForTest edgeRoot interior binderMetas n1 n2 = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState binderMetas interior edgeRoot PendingWeakenOwnerUnknown
    (_a, eu1) <- runStateT (unifyAcyclicEdge n1 n2) eu0
    pure (map edgeWitnessInstanceOp (reverse (eusOps eu1)))

-- | Test-only seam for resolving one witness operand from the frozen source
-- domain and exact current-edge copy map, without exposing the implementation
-- state record to tests.
sourceWitnessNodeWithCopyMapForTest
    :: CopyMap
    -> IntSet.IntSet
    -> NodeId
    -> EdgeSourceInterior
    -> NodeId
    -> PresolutionM p (Maybe NodeId)
sourceWitnessNodeWithCopyMapForTest copyMap sourceNodeKeys sourceRoot sourceInterior destination = do
    eu0 <-
        initEdgeUnifyStateWithCopyMap
            copyMap
            sourceNodeKeys
            sourceRoot
            sourceInterior
            IntSet.empty
            IntSet.empty
            []
            IntSet.empty
            destination
            PendingWeakenOwnerUnknown
    fst <$> runStateT (sourceWitnessNode destination) eu0

-- | Testing helper for the witnessed lower-bound installation primitive.
-- Unlike UF harmonization, every Raise returned by this path must belong to
-- the frozen source interior because it has no other witness domain.
runEdgeBoundInstallForTest
    :: NodeId
    -> IntSet.IntSet
    -> NodeId
    -> NodeId
    -> PresolutionM p [InstanceOp]
runEdgeBoundInstallForTest edgeRoot interior target bound = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState [] interior edgeRoot PendingWeakenOwnerUnknown
    (_a, eu1) <- runStateT (setVarBoundM target (Just bound)) eu0
    pure (map edgeWitnessInstanceOp (reverse (eusOps eu1)))

-- | Execute edge-local graph operations as one block around a unification action.
--
-- This preserves existing pre/post ordering while giving callers a single
-- edge-local execution entrypoint.
executeEdgeLocalOmegaOps
    :: OmegaExec.OmegaExecEnv (EdgeUnifyM p)
    -> [InstanceOp]
    -> EdgeUnifyM p a
    -> EdgeUnifyM p a
executeEdgeLocalOmegaOps omegaEnv baseOps action = do
    Omega.executeEdgeLocalOmegaOps omegaEnv baseOps action
