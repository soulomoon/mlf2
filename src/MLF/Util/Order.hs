{- |
Module      : MLF.Util.Order
Description : Leftmost-lowermost ordering helpers (paper ≺)

This module is a small utility for computing the paper’s “leftmost-lowermost”
ordering key (§3.4, `papers/xmlf.txt`) over nodes reachable from a root.

It is used by elaboration (Φ/Σ) and presolution to make merge directions and
quantifier ordering follow the paper’s ≺ convention (see Phase 8B in
`plans/merge_raise_merge_plan.txt`).
-}
module MLF.Util.Order (
    OrderKey(..),
    compareOrderKey,
    orderKeysFromRootWith,
    orderKeysFromRoot,
    orderKeysFromRootRestricted,
    compareNodesByOrderKey
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet

import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types (Constraint(..), NodeId)
import MLF.Util.OrderKey (OrderKey(..), compareOrderKey, orderKeysFromRootWith, compareNodesByOrderKey)

-- | Compute best order keys for all nodes reachable from @root@, using the
-- solved graph’s union-find canonicalization.
orderKeysFromRoot :: SolveResult -> NodeId -> IntMap OrderKey
orderKeysFromRoot res root0 =
    orderKeysFromRootWith (Solve.frWith (srUnionFind res)) (cNodes (srConstraint res)) root0 Nothing

orderKeysFromRootRestricted :: SolveResult -> NodeId -> IntSet.IntSet -> IntMap OrderKey
orderKeysFromRootRestricted res root0 allowed =
    orderKeysFromRootWith (Solve.frWith (srUnionFind res)) (cNodes (srConstraint res)) root0 (Just allowed)
