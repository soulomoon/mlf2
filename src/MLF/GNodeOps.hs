{- |
Module      : MLF.GNodeOps
Description : Shared operations on the binding tree (G-nodes)

This module centralizes small, pure transformations of the binding tree
(`cGNodes` / `GNode.gBinds`) so that multiple phases can maintain the same
invariants without duplicating list/IntMap plumbing.
-}
module MLF.GNodeOps (
    ensureVarBindAtLevel,
    setVarBoundAtLevel,
    lookupVarBoundAtLevel,
    dropVarBindFromAllLevels
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import MLF.Types (GNode(..), GNodeId(..), NodeId(..))

-- | Ensure that a type variable is registered in the bind list at a level.
--
-- If the variable is already present, keep its existing bound.
ensureVarBindAtLevel :: GNodeId -> NodeId -> IntMap GNode -> IntMap GNode
ensureVarBindAtLevel lvl vid gnodes
    | IntMap.null gnodes = gnodes
    | otherwise =
        IntMap.adjust
            (\g -> g { gBinds = ensure (gBinds g) })
            (getGNodeId lvl)
            gnodes
  where
    ensure binds
        | any (\(v, _) -> v == vid) binds = binds
        | otherwise = (vid, Nothing) : binds

-- | Set (or insert) the instance bound of a type variable at a level.
--
-- This also deduplicates multiple entries of the same variable, keeping only
-- the first occurrence (updated) and dropping the rest.
setVarBoundAtLevel :: GNodeId -> NodeId -> Maybe NodeId -> IntMap GNode -> IntMap GNode
setVarBoundAtLevel lvl vid mb gnodes
    | IntMap.null gnodes = gnodes
    | otherwise =
        IntMap.adjust
            (\g -> g { gBinds = set (gBinds g) })
            (getGNodeId lvl)
            gnodes
  where
    set binds =
        let (pre, rest) = break (\(v, _) -> v == vid) binds
        in case rest of
            [] -> (vid, mb) : binds
            (_ : xs) -> pre ++ (vid, mb) : filter (\(v, _) -> v /= vid) xs

-- | Lookup the instance bound of a variable at a particular level.
lookupVarBoundAtLevel :: GNodeId -> NodeId -> IntMap GNode -> Maybe NodeId
lookupVarBoundAtLevel lvl vid gnodes = do
    g <- IntMap.lookup (getGNodeId lvl) gnodes
    mb <- lookup vid (gBinds g)
    mb

-- | Remove a variable from every level's bind list.
--
-- This is useful for elimination steps (Weaken/Merge) where the variable should
-- no longer be considered a quantifiable binder.
dropVarBindFromAllLevels :: NodeId -> IntMap GNode -> IntMap GNode
dropVarBindFromAllLevels vid =
    IntMap.map (\g -> g { gBinds = filter (\(v, _) -> v /= vid) (gBinds g) })
