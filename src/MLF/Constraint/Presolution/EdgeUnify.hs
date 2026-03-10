{- |
Module      : MLF.Constraint.Presolution.EdgeUnify
Description : Facade for edge-local unification and ω execution helpers

This module remains the public/export owner for the edge-local unification
helpers used during presolution, while the implementation lives in focused
child modules under @MLF.Constraint.Presolution.EdgeUnify.@
-}
module MLF.Constraint.Presolution.EdgeUnify (
    EdgeUnifyState(..),
    EdgeUnifyM,
    MonadEdgeUnify(..),
    executeEdgeLocalOmegaOps,
    flushPendingWeakensAtOwnerBoundary,
    initEdgeUnifyState,
    mkOmegaExecEnv,
    runEdgeUnifyForTest,
    unifyAcyclicEdge,
    unifyStructureEdge
) where

import Control.Monad.State (runStateT)
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Base (
    PendingWeakenOwner(..),
    PresolutionM,
    requireValidBindingTree
    )
import MLF.Constraint.Presolution.EdgeUnify.State (
    EdgeUnifyM,
    EdgeUnifyState(..),
    MonadEdgeUnify(..),
    initEdgeUnifyState,
    mkOmegaExecEnv
    )
import qualified MLF.Constraint.Presolution.EdgeUnify.Omega as Omega
import MLF.Constraint.Presolution.EdgeUnify.Omega (
    flushPendingWeakensAtOwnerBoundary
    )
import MLF.Constraint.Presolution.EdgeUnify.Unify (
    unifyAcyclicEdge,
    unifyStructureEdge
    )
import MLF.Constraint.Types (InstanceOp, NodeId)
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
    -> PresolutionM [InstanceOp]
runEdgeUnifyForTest edgeRoot interior n1 n2 = do
    requireValidBindingTree
    eu0 <- initEdgeUnifyState [] interior edgeRoot PendingWeakenOwnerUnknown
    (_a, eu1) <- runStateT (unifyAcyclicEdge n1 n2) eu0
    pure (eusOps eu1)

-- | Execute edge-local graph operations as one block around a unification action.
--
-- This preserves existing pre/post ordering while giving callers a single
-- edge-local execution entrypoint.
executeEdgeLocalOmegaOps
    :: OmegaExec.OmegaExecEnv EdgeUnifyM
    -> [InstanceOp]
    -> EdgeUnifyM a
    -> EdgeUnifyM a
executeEdgeLocalOmegaOps omegaEnv baseOps action = do
    Omega.executeEdgeLocalOmegaOps omegaEnv baseOps action
