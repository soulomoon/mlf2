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
    compareOrderKey,
    orderKeysFromRootWith,
    orderKeysFromConstraintWith,
    orderKeysFromRoot,
    orderKeysFromRootRestricted,
    compareNodesByOrderKey
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types (Constraint(..), NodeId(..), VarBounds)
import MLF.Util.OrderKey (OrderKey(..), compareOrderKey, orderKeysFromRootWith, orderKeysFromRootWithExtra, compareNodesByOrderKey)

-- | Compute best order keys for all nodes reachable from @root@, using the
-- solved graph’s union-find canonicalization.
orderKeysFromRoot :: SolveResult -> NodeId -> IntMap OrderKey
orderKeysFromRoot res root0 =
    orderKeysFromConstraintWith (Solve.frWith (srUnionFind res)) (srConstraint res) root0 Nothing

orderKeysFromRootRestricted :: SolveResult -> NodeId -> IntSet.IntSet -> IntMap OrderKey
orderKeysFromRootRestricted res root0 allowed =
    orderKeysFromConstraintWith (Solve.frWith (srUnionFind res)) (srConstraint res) root0 (Just allowed)

-- | Compute order keys from a raw constraint.
--
-- Paper alignment: ordering is based on term-DAG paths. Our constraint model
-- stores variable bounds separately, so we thread bounds as extra structural
-- children (they are part of the type syntax), but we do not incorporate
-- binding edges.
orderKeysFromConstraintWith
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Maybe IntSet.IntSet
    -> IntMap OrderKey
orderKeysFromConstraintWith canonical constraint root0 mbAllowed =
    let extraChildren = extraChildrenFromBounds canonical (cVarBounds constraint)
    in orderKeysFromRootWithExtra canonical (cNodes constraint) extraChildren root0 mbAllowed

extraChildrenFromBounds :: (NodeId -> NodeId) -> VarBounds -> (NodeId -> [NodeId])
extraChildrenFromBounds canonical varBounds =
    let boundMap =
            IntMap.fromListWith (flip (++))
                [ (getNodeId vC, [bC])
                | (vid, Just bnd) <- IntMap.toList varBounds
                , let vC = canonical (NodeId vid)
                , let bC = canonical bnd
                , vC /= bC
                ]
    in \parent ->
        IntMap.findWithDefault [] (getNodeId (canonical parent)) boundMap
