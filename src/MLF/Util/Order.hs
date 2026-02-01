{- |
Module      : MLF.Util.Order
Description : Leftmost-lowermost ordering helpers (paper ≺)

This module is a small utility for computing the paper’s “leftmost-lowermost”
ordering key (§15.2.4, `papers/these-finale-english.txt`; §3.4, `papers/xmlf.txt`)
over nodes reachable from a root.

It is used by elaboration (Φ/Σ) and presolution to make merge directions and
quantifier ordering follow the paper’s ≺ convention (see Phase 8B in
`plans/merge_raise_merge_plan.txt`).
-}
module MLF.Util.Order (
    OrderKey(..),
    OrderKeyError(..),
    compareOrderKey,
    orderKeysFromRootWith,
    orderKeysFromConstraintWith,
    orderKeysFromRoot,
    orderKeysFromRootRestricted,
    compareNodesByOrderKey,
    sortByOrderKey
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types (Constraint(..), NodeId(..), TyNode(..))
import MLF.Util.OrderKey (OrderKey(..), OrderKeyError(..), compareOrderKey, orderKeysFromRootWith, orderKeysFromRootWithExtra, compareNodesByOrderKey, sortByOrderKey)

-- | Compute best order keys for all nodes reachable from @root@, using the
-- solved graph’s union-find canonicalization.
orderKeysFromRoot :: SolveResult -> NodeId -> IntMap OrderKey
orderKeysFromRoot res root0 =
    orderKeysFromConstraintWith (Solve.frWith (srUnionFind res)) (srConstraint res) root0 Nothing

orderKeysFromRootRestricted :: SolveResult -> NodeId -> IntSet.IntSet -> IntMap OrderKey
orderKeysFromRootRestricted res root0 allowed =
    orderKeysFromConstraintWith (Solve.frWith (srUnionFind res)) (srConstraint res) root0 (Just allowed)

-- | Compute order keys from a raw constraint using term-DAG structure plus
-- instance-bound dependencies.
orderKeysFromConstraintWith
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Maybe IntSet.IntSet
    -> IntMap OrderKey
orderKeysFromConstraintWith canonical constraint root0 mbAllowed =
    let nodes = cNodes constraint
        extraChildren nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Just TyVar{ tnBound = Just bnd } -> [bnd]
                _ -> []
    in orderKeysFromRootWithExtra canonical nodes extraChildren root0 mbAllowed
