{- |
Module      : MLF.Util.OrderKey
Description : Pure leftmost-lowermost ordering keys (paper ≺)

This module contains the *pure* algorithm for computing the paper’s
“leftmost-lowermost” ordering key over nodes reachable from a root in the term
DAG.

It is split out from `MLF.Util.Order` so lower-level modules (notably
`MLF.Binding.Tree`) can use the ordering logic without introducing dependency
cycles through Solve.
 -}
module MLF.Util.OrderKey (
    OrderKey(..),
    compareOrderKey,
    orderKeysFromRootWith,
    compareNodesByOrderKey
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types (NodeId(..), TyNode, structuralChildren)

-- | A “best occurrence” position key for a node in a (shared) type DAG.
--
-- Paper intent: order nodes by the leftmost-lowermost path order (<P) from
-- §15.2.4. This is lexicographic on paths with the empty path considered
-- *greatest* (so deeper paths are smaller only when they share a prefix).
data OrderKey = OrderKey
    { okDepth :: !Int
    , okPath :: [Int]
    } deriving (Eq, Show)

compareOrderKey :: OrderKey -> OrderKey -> Ordering
compareOrderKey a b =
    comparePaths (okPath a) (okPath b)

comparePaths :: [Int] -> [Int] -> Ordering
comparePaths [] [] = EQ
comparePaths [] (_:_) = GT  -- empty path is greatest
comparePaths (_:_) [] = LT
comparePaths (i:is) (j:js) =
    case compare i j of
        EQ -> comparePaths is js
        other -> other

-- | Compute best order keys for nodes reachable from @root@.
--
-- If an allowed-node set is provided, traversal is restricted to that set
-- (with @root@ always treated as allowed).
orderKeysFromRootWith
    :: (NodeId -> NodeId)
    -> IntMap TyNode
    -> NodeId
    -> Maybe IntSet.IntSet
    -> IntMap OrderKey
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
    go m ((nid0, depth, path) : rest) =
        let nid = canonical nid0
            key = OrderKey { okDepth = depth, okPath = path }
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
            Just n ->
                [ (canonical child, depth, path ++ [idx])
                | (idx, child) <- zip [0 ..] (structuralChildren n)
                ]

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
