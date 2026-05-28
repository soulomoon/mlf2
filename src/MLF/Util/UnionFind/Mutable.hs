{-# LANGUAGE FlexibleContexts #-}
{- |
Module      : MLF.Util.UnionFind.Mutable
Description : ST-based mutable union-find with ranked union and path compression

Backed by 'STUArray' for cache-friendly O(α(n)) amortized operations.
Each node is represented by an 'Int' key.  The array stores parent pointers
(0-based); a node that is its own parent is a root.  A parallel rank array
supports union-by-rank.
-}
module MLF.Util.UnionFind.Mutable (
    MutableUf,
    newUf,
    findRoot,
    unionNodes,
    freezeToMap,
) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.Base (STUArray, newArray, readArray, writeArray)
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (NodeId(..))

-- | A mutable union-find structure parameterized by the ST thread.
data MutableUf s = MutableUf
    { ufParent :: {-# UNPACK #-} !(STUArray s Int Int)
    , ufRank   :: {-# UNPACK #-} !(STUArray s Int Int)
    , ufSize   :: {-# UNPACK #-} !Int
    }

-- | Create a fresh union-find with @n@ elements, each in its own singleton set.
--
-- Nodes are indexed 0 .. n-1.
newUf :: Int -> ST s (MutableUf s)
newUf n = do
    parent <- newArray (0, n - 1) 0
    rank   <- newArray (0, n - 1) 0
    -- Initialize each node as its own parent
    let go i
            | i >= n    = pure ()
            | otherwise = do
                writeArray parent i i
                go (i + 1)
    go 0
    pure MutableUf { ufParent = parent, ufRank = rank, ufSize = n }

-- | Find the canonical representative with in-place path compression.
findRoot :: MutableUf s -> Int -> ST s Int
findRoot uf nid = do
    parent <- readArray (ufParent uf) nid
    if parent == nid
        then pure nid
        else do
            root <- findRoot uf parent
            -- Path compression: point directly to root
            when (parent /= root) $
                writeArray (ufParent uf) nid root
            pure root

-- | Union two elements by rank.  @unionNodes from to@ makes @from@'s root
-- point to @to@'s root.  Returns @True@ if a merge actually happened
-- (the roots were different).
unionNodes :: MutableUf s -> Int -> Int -> ST s Bool
unionNodes uf from to = do
    rootFrom <- findRoot uf from
    rootTo   <- findRoot uf to
    if rootFrom == rootTo
        then pure False
        else do
            rankFrom <- readArray (ufRank uf) rootFrom
            rankTo   <- readArray (ufRank uf) rootTo
            case compare rankFrom rankTo of
                LT -> do
                    writeArray (ufParent uf) rootFrom rootTo
                    pure True
                GT -> do
                    writeArray (ufParent uf) rootTo rootFrom
                    pure True
                EQ -> do
                    writeArray (ufParent uf) rootFrom rootTo
                    writeArray (ufRank uf) rootTo (rankTo + 1)
                    pure True

-- | Freeze the union-find to an 'IntMap NodeId' suitable for the rest of
-- the pure pipeline.  Only entries whose parent differs from the index
-- are included (roots are omitted since they are the identity).
freezeToMap :: MutableUf s -> ST s (IntMap.IntMap NodeId)
freezeToMap uf = do
    let hi = ufSize uf - 1
    let go i acc
            | i > hi    = pure acc
            | otherwise = do
                root <- findRoot uf i
                let acc' = if root == i
                        then acc
                        else IntMap.insert i (NodeId root) acc
                go (i + 1) acc'
    go 0 IntMap.empty
