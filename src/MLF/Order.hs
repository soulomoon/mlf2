{- |
Module      : MLF.Order
Description : Leftmost-lowermost ordering helpers (paper ≺)

This module is a small utility for computing the paper’s “leftmost-lowermost”
ordering key (§3.4, `papers/xmlf.txt`) over nodes reachable from a root.

It is used by elaboration (Φ/Σ) and presolution to make merge directions and
quantifier ordering follow the paper’s ≺ convention (see Phase 8B in
`merge_raise_merge_plan.txt`).
-}
module MLF.Order (
    OrderKey(..),
    compareOrderKey,
    orderKeysFromRootWith,
    orderKeysFromRoot,
    orderKeysFromRootRestricted,
    compareNodesByOrderKey
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Ord (Down(..))

import qualified MLF.Solve as Solve (frWith)
import MLF.Solve (SolveResult(..))
import MLF.Types

-- | A “best occurrence” position key for a node in a (shared) type DAG.
--
-- Paper intent: order nodes by “lowermost first”, breaking ties by “leftmost”.
-- We encode that as:
--   - deeper depth is better (ordered descending),
--   - path is compared lexicographically (ascending).
data OrderKey = OrderKey
    { okDepth :: !Int
    , okPath :: [Int]
    } deriving (Eq, Show)

compareOrderKey :: OrderKey -> OrderKey -> Ordering
compareOrderKey a b =
    compare (Down (okDepth a), okPath a) (Down (okDepth b), okPath b)

-- | Compute best order keys for nodes reachable from @root@.
--
-- If an allowed-node set is provided, traversal is restricted to that set
-- (with @root@ always treated as allowed).
orderKeysFromRootWith :: (NodeId -> NodeId) -> IntMap TyNode -> NodeId -> Maybe IntSet.IntSet -> IntMap OrderKey
orderKeysFromRootWith canonical nodes root0 mbAllowed =
    go IntMap.empty [(canonical root0, 0, [])]
  where
    root = canonical root0

    allowed :: NodeId -> Bool
    allowed nid =
        nid == root || case mbAllowed of
            Nothing -> True
            Just s -> IntSet.member (getNodeId nid) s

    better :: OrderKey -> OrderKey -> Bool
    better new old = compareOrderKey new old == LT

    go :: IntMap OrderKey -> [(NodeId, Int, [Int])] -> IntMap OrderKey
    go m [] = m
    go m ((nid, depth, path) : rest) =
        let key = OrderKey { okDepth = depth, okPath = path }
            (m', enqueueKids) =
                case IntMap.lookup (getNodeId nid) m of
                    Nothing -> (IntMap.insert (getNodeId nid) key m, True)
                    Just oldKey ->
                        if better key oldKey
                            then (IntMap.insert (getNodeId nid) key m, True)
                            else (m, False)
            rest' =
                if enqueueKids && allowed nid
                    then rest ++ children nid (depth + 1) path
                    else rest
        in go m' rest'

    children :: NodeId -> Int -> [Int] -> [(NodeId, Int, [Int])]
    children nid depth path =
        case IntMap.lookup (getNodeId nid) nodes of
            Nothing -> []
            Just n -> case n of
                TyVar{} -> []
                TyBase{} -> []
                TyArrow{ tnDom = d, tnCod = c } ->
                    [ (canonical d, depth, path ++ [0])
                    , (canonical c, depth, path ++ [1])
                    ]
                TyForall{ tnBody = b } ->
                    [ (canonical b, depth, path ++ [0]) ]
                TyExp{ tnBody = b } ->
                    [ (canonical b, depth, path ++ [0]) ]

-- | Compute best order keys for all nodes reachable from @root@, using the
-- solved graph’s union-find canonicalization.
orderKeysFromRoot :: SolveResult -> NodeId -> IntMap OrderKey
orderKeysFromRoot res root0 =
    orderKeysFromRootWith (Solve.frWith (srUnionFind res)) (cNodes (srConstraint res)) root0 Nothing

orderKeysFromRootRestricted :: SolveResult -> NodeId -> IntSet.IntSet -> IntMap OrderKey
orderKeysFromRootRestricted res root0 allowed =
    orderKeysFromRootWith (Solve.frWith (srUnionFind res)) (cNodes (srConstraint res)) root0 (Just allowed)

-- | Compare two nodes by their best order keys (falling back to NodeId order
-- when a key is missing).
compareNodesByOrderKey :: IntMap OrderKey -> NodeId -> NodeId -> Ordering
compareNodesByOrderKey m a b =
    case (IntMap.lookup (getNodeId a) m, IntMap.lookup (getNodeId b) m) of
        (Just ka, Just kb) ->
            case compareOrderKey ka kb of
                EQ -> compare a b
                other -> other
        (Just _, Nothing) -> LT
        (Nothing, Just _) -> GT
        (Nothing, Nothing) -> compare a b
