{- |
Module      : MLF.Util.UnionFind
Description : Shared union-find helpers (NodeId parent map)

Several phases (Normalize/Presolution/Solve) maintain a union-find parent map
`IntMap NodeId`. This module centralizes the common operations:
  * read-only representative chase (no path compression), and
  * pure find with path compression.
-}
module MLF.Util.UnionFind (
    frWith,
    findRootWithCompression
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types (NodeId(..))

-- | Read-only chase to the canonical representative in a union-find map.
--
-- This does not perform path compression.
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith uf nid =
    case IntMap.lookup (getNodeId nid) uf of
        Nothing -> nid
        Just parent
            | parent == nid -> nid
            | otherwise -> frWith uf parent

-- | Find the canonical representative of a node, with path compression.
--
-- Returns the representative and an updated map that includes compression links
-- along the search path.
findRootWithCompression :: IntMap NodeId -> NodeId -> (NodeId, IntMap NodeId)
findRootWithCompression uf0 nid =
    case IntMap.lookup (getNodeId nid) uf0 of
        Nothing -> (nid, uf0)
        Just parent
            | parent == nid -> (nid, uf0)
            | otherwise ->
                let (root, uf1) = findRootWithCompression uf0 parent
                    uf2 = IntMap.insert (getNodeId nid) root uf1
                in (root, uf2)
