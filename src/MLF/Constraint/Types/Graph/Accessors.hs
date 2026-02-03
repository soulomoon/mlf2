{- |
Module      : MLF.Constraint.Types.Graph.Accessors
Description : Accessor utilities for the constraint graph
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides accessor utilities for querying the constraint graph:

* 'maxNodeIdKeyOr0' - Get the maximum NodeId key for fresh ID allocation
-}
module MLF.Constraint.Types.Graph.Accessors (
    -- * Node ID utilities
    maxNodeIdKeyOr0,
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph.NodeEdge (NodeMap (..))

-- | Maximum NodeId key present in a NodeMap, defaulting to 0 for an empty map.
--
-- This is used to initialize fresh NodeId counters in phases that allocate new
-- nodes during rewriting (Normalize, Presolution).
maxNodeIdKeyOr0 :: NodeMap a -> Int
maxNodeIdKeyOr0 (NodeMap nodes) =
    case IntMap.lookupMax nodes of
        Nothing -> 0
        Just (k, _node) -> k
