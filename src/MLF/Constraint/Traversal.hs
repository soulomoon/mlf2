{- |
Module      : MLF.Constraint.Traversal
Description : Shared structural reachability helpers

Several phases need to traverse the term-DAG under some canonicalization
function (typically a union-find representative chase). This module centralizes
that traversal so Normalize/Presolution/Solve agree on the same notion of
reachability.
-}
module MLF.Constraint.Traversal (
    TraversalError(..),
    occursInUnder,
    reachableFromNodes,
    reachableFromUnderLenient,
    reachableFromManyUnderLenient,
    reachableFromWithBounds
) where

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import MLF.Constraint.Types.Graph
    ( NodeId(..)
    , TyNode
    , structuralChildren
    , structuralChildrenWithBounds
    )

data TraversalError
    = MissingNode NodeId
    deriving (Eq, Show)

-- | Check whether @needle@ occurs in the structural subgraph rooted at @start@,
-- following the supplied canonicalization function on every visited node.
--
-- Returns 'MissingNode' when traversal reaches a referenced node id that is not
-- present in the graph.
occursInUnder
    :: (NodeId -> NodeId)
    -> (NodeId -> Maybe TyNode)
    -> NodeId
    -> NodeId
    -> Either TraversalError Bool
occursInUnder canonical lookupNode needle0 start0 =
    go IntSet.empty start0
  where
    needle = canonical needle0

    go :: IntSet -> NodeId -> Either TraversalError Bool
    go visited nid0 =
        let nid = canonical nid0
            key = getNodeId nid
        in if nid == needle
            then Right True
            else if IntSet.member key visited
                then Right False
                else case lookupNode nid of
                    Nothing -> Left (MissingNode nid)
                    Just node ->
                        let visited' = IntSet.insert key visited
                        in anyM (go visited') (structuralChildren node)

    anyM :: (a -> Either TraversalError Bool) -> [a] -> Either TraversalError Bool
    anyM _ [] = Right False
    anyM f (x : xs) = do
        b <- f x
        if b then Right True else anyM f xs

-- | Collect all nodes reachable from @root@ by following 'structuralChildren',
-- rewriting every visited id through the given canonicalization function.
--
-- This is a lenient traversal: missing nodes are treated as leafs, and the
-- returned set always includes @root@ (after canonicalization) even if it is
-- not present in the node map.
reachableFromUnderLenient
    :: (NodeId -> NodeId)
    -> (NodeId -> Maybe TyNode)
    -> NodeId
    -> IntSet
reachableFromUnderLenient canonical lookupNode root0 =
    reachableFromManyUnderLenient canonical lookupNode [root0]

-- | Like 'reachableFromUnderLenient', but accepts multiple roots.
--
-- The returned set is the union of the reachable nodes for each root, with
-- all roots included (after canonicalization), even if they are missing from
-- the node map.
reachableFromManyUnderLenient
    :: (NodeId -> NodeId)
    -> (NodeId -> Maybe TyNode)
    -> [NodeId]
    -> IntSet
reachableFromManyUnderLenient canonical lookupNode roots0 =
    reachableFromNodes canonical children roots0
  where
    children nid =
        case lookupNode nid of
            Nothing -> []
            Just node -> structuralChildren node

reachableFromNodes
    :: (NodeId -> NodeId)
    -> (NodeId -> [NodeId])
    -> [NodeId]
    -> IntSet
reachableFromNodes canonical childrenOf roots0 =
    go IntSet.empty (map canonical roots0)
  where
    go :: IntSet -> [NodeId] -> IntSet
    go visited [] = visited
    go visited (nid0 : rest) =
        let nid = canonical nid0
            key = getNodeId nid
        in if IntSet.member key visited
            then go visited rest
            else
                let visited' = IntSet.insert key visited
                    children = map canonical (childrenOf nid)
                in go visited' (children ++ rest)

-- | Collect all nodes reachable from @root@ by following both structural
-- children and instance bounds.
--
-- This is a lenient traversal: missing nodes are treated as leafs, and the
-- returned set always includes @root@ (after canonicalization) even if it is
-- not present in the node map.
reachableFromWithBounds
    :: (NodeId -> NodeId)
    -> (NodeId -> Maybe TyNode)
    -> NodeId
    -> IntSet
reachableFromWithBounds canonical lookupNode root0 =
    reachableFromNodes canonical children [root0]
  where
    children nid =
        case lookupNode nid of
            Nothing -> []
            Just node -> structuralChildrenWithBounds node
