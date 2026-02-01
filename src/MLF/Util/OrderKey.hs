{- |
Module      : MLF.Util.OrderKey
Description : Pure leftmost-lowermost ordering keys (paper ≺)

This module contains the *pure* algorithm for computing the paper’s
“leftmost-lowermost” ordering key over nodes reachable from a root in the term
DAG (Definition 15.2.7).

It is split out from `MLF.Util.Order` so lower-level modules (notably
`MLF.Binding.Tree`) can use the ordering logic without introducing dependency
cycles through Solve.
 -}
module MLF.Util.OrderKey (
    OrderKey(..),
    OrderKeyError(..),
    compareOrderKey,
    orderKeysFromRootWithExtra,
    orderKeysFromRootWith,
    compareNodesByOrderKey,
    sortByOrderKey
) where

import Control.Monad (forM_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortBy)

import MLF.Constraint.Types (NodeId(..), NodeMap, TyNode, lookupNodeIn, structuralChildren)

-- | Errors that can occur when comparing nodes by order key.
data OrderKeyError
    = MissingOrderKey NodeId
    | EqualKeysDistinctNodes NodeId NodeId
    deriving (Eq, Show)

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
orderKeysFromRootWithExtra
    :: (NodeId -> NodeId)
    -> NodeMap TyNode
    -> (NodeId -> [NodeId])
    -> NodeId
    -> Maybe IntSet.IntSet
    -> IntMap OrderKey
orderKeysFromRootWithExtra canonical nodes extraChildren root0 mbAllowed =
    go IntMap.empty [(canonical root0, 0, [], IntSet.singleton (getNodeId (canonical root0)))]
  where
    root = canonical root0

    allowed :: NodeId -> Bool
    allowed nid =
        nid == root || case mbAllowed of
            Nothing -> True
            Just s -> IntSet.member (getNodeId nid) s

    better :: OrderKey -> OrderKey -> Bool
    better new old = compareOrderKey new old == LT

    go :: IntMap OrderKey -> [(NodeId, Int, [Int], IntSet.IntSet)] -> IntMap OrderKey
    go m [] = m
    go m ((nid0, depth, path, pathNodes) : rest) =
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
                    then rest ++ children nid (depth + 1) path pathNodes
                    else rest
        in go m' rest'

    children :: NodeId -> Int -> [Int] -> IntSet.IntSet -> [(NodeId, Int, [Int], IntSet.IntSet)]
    children nid depth path pathNodes =
        case lookupNodeIn nodes nid of
            Nothing -> []
            Just n ->
                let structural = structuralChildren n
                    extras = extraChildren nid
                    allChildren = structural ++ extras
                in [ (childC, depth, path ++ [idx], IntSet.insert (getNodeId childC) pathNodes)
                   | (idx, child) <- zip [0 ..] allChildren
                   , let childC = canonical child
                   , childC /= nid
                   , not (IntSet.member (getNodeId childC) pathNodes)
                   ]

-- | Compute best order keys for nodes reachable from @root@ using only
-- structural children.
orderKeysFromRootWith
    :: (NodeId -> NodeId)
    -> NodeMap TyNode
    -> NodeId
    -> Maybe IntSet.IntSet
    -> IntMap OrderKey
orderKeysFromRootWith canonical nodes root0 mbAllowed =
    orderKeysFromRootWithExtra canonical nodes (const []) root0 mbAllowed

-- | Compare two nodes by their best order keys.
--
-- Missing keys indicate a bug at the call site: all compared nodes must be
-- reachable from the root used to compute the keys.
compareNodesByOrderKey :: IntMap OrderKey -> NodeId -> NodeId -> Either OrderKeyError Ordering
compareNodesByOrderKey m a b =
    case (IntMap.lookup (getNodeId a) m, IntMap.lookup (getNodeId b) m) of
        (Just ka, Just kb) ->
            case compareOrderKey ka kb of
                EQ ->
                    if a == b
                        then Right EQ
                        else Left (EqualKeysDistinctNodes a b)
                other -> Right other
        (Nothing, _) ->
            Left (MissingOrderKey a)
        (_, Nothing) ->
            Left (MissingOrderKey b)

-- | Sort a list of nodes by their order keys.
--
-- Returns Left if any node is missing an order key or if distinct nodes have
-- equal keys.
sortByOrderKey :: IntMap OrderKey -> [NodeId] -> Either OrderKeyError [NodeId]
sortByOrderKey m nodes = do
    -- Validate all nodes have keys and no duplicates exist
    forM_ nodes $ \nid ->
        case IntMap.lookup (getNodeId nid) m of
            Nothing -> Left (MissingOrderKey nid)
            Just _ -> Right ()
    -- Check for equal keys on distinct nodes by comparing all pairs
    -- (This is O(n^2) but the lists are typically small)
    let indexed = [(nid, IntMap.lookup (getNodeId nid) m) | nid <- nodes]
    forM_ [(a, ka, b, kb) | (a, Just ka) <- indexed, (b, Just kb) <- indexed, a /= b] $
        \(a, ka, b, kb) ->
            if compareOrderKey ka kb == EQ
                then Left (EqualKeysDistinctNodes a b)
                else Right ()
    -- Safe to use sortBy now since we validated
    let cmp x y = case compareNodesByOrderKey m x y of
            Right ord -> ord
            Left _ -> EQ  -- unreachable after validation
    Right (sortBy cmp nodes)
