{- |
Module      : MLF.Util.Order
Description : Leftmost-lowermost ordering helpers (paper ≺)

This module is a small utility for computing the paper’s “leftmost-lowermost”
ordering key (§15.2.4, `papers/these-finale-english.txt`; §3.4, `papers/xmlf.txt`)
over nodes reachable from a root.

It is used by elaboration (Φ/Σ) and presolution to make merge directions and
quantifier ordering follow the paper’s ≺ convention (see Phase 8B in
`.kiro/specs/paper-faithfulness-remaining-deltas/`).
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
import qualified Data.IntSet as IntSet

import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
    ( Constraint(..)
    , NodeId(..)
    , TyNode(..)
    , lookupNodeIn
    )
import MLF.Util.OrderKey (OrderKey(..), OrderKeyError(..), compareOrderKey, orderKeysFromRootWith, orderKeysFromRootWithExtra, compareNodesByOrderKey, sortByOrderKey)

-- | Compute best order keys for all nodes reachable from @root@, using the
-- solved graph’s canonicalization.
orderKeysFromRoot :: Solved -> NodeId -> IntMap OrderKey
orderKeysFromRoot solved root0 =
    orderKeysFromConstraintWith (Solved.canonical solved) (Solved.solvedConstraint solved) root0 Nothing

orderKeysFromRootRestricted :: Solved -> NodeId -> IntSet.IntSet -> IntMap OrderKey
orderKeysFromRootRestricted solved root0 allowed =
    orderKeysFromConstraintWith (Solved.canonical solved) (Solved.solvedConstraint solved) root0 (Just allowed)

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
            case lookupNodeIn nodes nid of
                Just TyVar{ tnBound = Just bnd } -> [bnd]
                _ -> []
    in orderKeysFromRootWithExtra canonical nodes extraChildren root0 mbAllowed
