{- |
Module      : MLF.Constraint.Presolution.EdgeUnify.Omega
Description : Edge-local ω execution and pending-weaken boundary flushing
-}
module MLF.Constraint.Presolution.EdgeUnify.Omega (
    executeEdgeLocalOmegaOps,
    pendingWeakenOwners,
    flushPendingWeakensAtOwnerBoundary
) where

import Control.Monad (forM, forM_)
import Control.Monad.State (modify')
import Data.List (nub, partition)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Presolution.Base (
    MonadPresolution(..),
    PendingWeakenOwner(..),
    PresolutionM,
    PresolutionState(..)
    )
import MLF.Constraint.Presolution.EdgeUnify.State (EdgeUnifyM, applyPendingWeaken)
import MLF.Constraint.Presolution.StateAccess (pendingWeakenOwnerM)
import MLF.Constraint.Types (InstanceOp, NodeId(..), getNodeId)
import qualified MLF.Witness.OmegaExec as OmegaExec

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
    OmegaExec.executeOmegaBaseOpsPre omegaEnv baseOps
    out <- action
    OmegaExec.executeOmegaBaseOpsPost omegaEnv baseOps
    pure out

pendingWeakenOwners :: PresolutionM [PendingWeakenOwner]
pendingWeakenOwners = do
    st <- getPresolutionState
    let pending = psPendingWeakens st
        ownerMap = psPendingWeakenOwners st
    owners <- forM (IntSet.toList pending) $ \nidInt ->
        case IntMap.lookup nidInt ownerMap of
            Just owner -> pure owner
            Nothing -> pendingWeakenOwnerM (NodeId nidInt)
    pure (nub owners)

-- | Flush delayed `Weaken` operations for one owner boundary.
--
-- Owner-boundary scheduling behavior:
-- * pending nodes owned by the given boundary owner are flushed now
-- * all other pending nodes remain queued
flushPendingWeakensAtOwnerBoundary :: PendingWeakenOwner -> PresolutionM ()
flushPendingWeakensAtOwnerBoundary owner =
    flushPendingWeakensWhere shouldFlush
  where
    shouldFlush bucket =
        bucket == owner || bucket == PendingWeakenOwnerUnknown

flushPendingWeakensWhere :: (PendingWeakenOwner -> Bool) -> PresolutionM ()
flushPendingWeakensWhere shouldFlush = do
    st0 <- getPresolutionState
    let pending = psPendingWeakens st0
        ownerMap = psPendingWeakenOwners st0
    tagged <- forM (IntSet.toList pending) $ \nidInt -> do
        let nid = NodeId nidInt
        owner <- case IntMap.lookup nidInt ownerMap of
            Just stamped -> pure stamped
            Nothing -> pendingWeakenOwnerM nid
        pure (nid, owner)
    let (toFlush, toKeep) = partition (shouldFlush . snd) tagged
    forM_ toFlush (applyPendingWeaken . fst)
    modify' $ \st ->
        st
            { psPendingWeakens =
                IntSet.fromList [getNodeId nid | (nid, _owner) <- toKeep]
            , psPendingWeakenOwners =
                IntMap.fromList [(getNodeId nid, owner) | (nid, owner) <- toKeep]
            }
