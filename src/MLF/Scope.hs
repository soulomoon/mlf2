{- |
Module      : MLF.Scope
Description : Scope utilities (G-node ancestry / LCA)

This module provides small, pure helpers for reasoning about scope levels
(`GNodeId`) using the binding tree stored in `Constraint.cGNodes`.

It is useful whenever we need LCA/ancestry information (e.g. for counting how
many paper-style `Raise` steps correspond to moving a binder between two levels).
-}
module MLF.Scope (
    lcaWithCounts
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import MLF.Types (GNode(..), GNodeId(..))

-- | Compute the lowest common ancestor (LCA) of two levels in a `GNode` tree.
--
-- Returns:
--   * the LCA level, and
--   * how many parent steps each input must take to reach that LCA.
--
-- When the two levels do not share an ancestor (e.g. different trees) or the
-- tree is incomplete, returns `Nothing`.
lcaWithCounts :: IntMap GNode -> GNodeId -> GNodeId -> Maybe (GNodeId, Int, Int)
lcaWithCounts gnodes a b = do
    let aAnc = ancestorsWithDist a 0
        aMap = IntMap.fromList [ (getGNodeId lvl, dist) | (lvl, dist) <- aAnc ]
    pick b 0 aMap
  where
    ancestorsWithDist :: GNodeId -> Int -> [(GNodeId, Int)]
    ancestorsWithDist lvl dist =
        (lvl, dist) : case IntMap.lookup (getGNodeId lvl) gnodes >>= gParent of
            Nothing -> []
            Just p -> ancestorsWithDist p (dist + 1)

    pick :: GNodeId -> Int -> IntMap Int -> Maybe (GNodeId, Int, Int)
    pick lvl distB aMap =
        case IntMap.lookup (getGNodeId lvl) aMap of
            Just distA -> Just (lvl, distA, distB)
            Nothing ->
                case IntMap.lookup (getGNodeId lvl) gnodes >>= gParent of
                    Nothing -> Nothing
                    Just p -> pick p (distB + 1) aMap
