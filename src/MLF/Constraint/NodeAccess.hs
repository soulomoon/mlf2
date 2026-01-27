{- |
Module      : MLF.Constraint.NodeAccess
Description : Centralized node access operations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides a centralized API for accessing nodes in the constraint graph.
It eliminates duplicate lookup patterns across the codebase and provides consistent
error handling.

= Design Rationale

Previously, 42+ modules duplicated patterns like:
@
case IntMap.lookup (getNodeId nid) (cNodes c) of
    Just node -> ...
    Nothing -> ...
@

This module centralizes these patterns, making it easier to:
- Add logging/debugging hooks
- Change the underlying storage representation
- Ensure consistent error handling
- Reduce code duplication (~200 lines eliminated)

= Usage

For safe lookups that return Maybe:
@
case lookupNode c nid of
    Just node -> ...
    Nothing -> ...
@

For lookups that should always succeed (with error on failure):
@
node <- lookupNodeSafe c nid
@
-}
module MLF.Constraint.NodeAccess (
    -- * Node lookup
    lookupNode,
    lookupNodeSafe,
    lookupGenNode,
    lookupGenNodeSafe,
    -- * Bound lookup
    lookupVarBound,
    lookupVarBoundSafe,
    -- * Bind parent lookup
    lookupBindParent,
    lookupBindParentSafe,
    -- * Batch operations
    lookupNodes,
    lookupNodesWithDefault
) where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (mapMaybe)

import MLF.Constraint.Types (Constraint(..), NodeId(..), GenNodeId(..), TyNode(..), GenNode, NodeRef, BindFlag, nodeRefKey, getNodeId, getGenNodeId)

-- | Look up a type node in the constraint graph.
--
-- Returns 'Nothing' if the node is not present.
lookupNode :: Constraint -> NodeId -> Maybe TyNode
lookupNode c nid = IntMap.lookup (getNodeId nid) (cNodes c)

-- | Look up a type node, returning an error if not found.
--
-- Use this when the node should always exist (e.g., after validation).
lookupNodeSafe :: Constraint -> NodeId -> Either String TyNode
lookupNodeSafe c nid =
    case lookupNode c nid of
        Just node -> Right node
        Nothing -> Left $ "Node not found: " ++ show (getNodeId nid)

-- | Look up a gen node in the constraint graph.
--
-- Returns 'Nothing' if the gen node is not present.
lookupGenNode :: Constraint -> GenNodeId -> Maybe GenNode
lookupGenNode c gid = IntMap.lookup (getGenNodeId gid) (cGenNodes c)

-- | Look up a gen node, returning an error if not found.
lookupGenNodeSafe :: Constraint -> GenNodeId -> Either String GenNode
lookupGenNodeSafe c gid =
    case lookupGenNode c gid of
        Just node -> Right node
        Nothing -> Left $ "Gen node not found: " ++ show (getGenNodeId gid)

-- | Look up the instance bound of a variable.
--
-- Missing entries are treated as ⊥ (returns 'Nothing').
-- Non-variable nodes also return 'Nothing'.
lookupVarBound :: Constraint -> NodeId -> Maybe NodeId
lookupVarBound c v =
    case lookupNode c v of
        Just TyVar{ tnBound = mb } -> mb
        _ -> Nothing

-- | Look up the instance bound of a variable, returning an error if the node
-- is not a variable or doesn't exist.
lookupVarBoundSafe :: Constraint -> NodeId -> Either String (Maybe NodeId)
lookupVarBoundSafe c v =
    case lookupNode c v of
        Just TyVar{ tnBound = mb } -> Right mb
        Just _ -> Left $ "Node is not a variable: " ++ show (getNodeId v)
        Nothing -> Left $ "Node not found: " ++ show (getNodeId v)

-- | Look up the binding parent of a node reference.
--
-- Returns 'Nothing' if the node has no binding parent (i.e., it's a root).
lookupBindParent :: Constraint -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent c ref = IntMap.lookup (nodeRefKey ref) (cBindParents c)

-- | Look up the binding parent, returning an error if not found.
lookupBindParentSafe :: Constraint -> NodeRef -> Either String (NodeRef, BindFlag)
lookupBindParentSafe c ref =
    case lookupBindParent c ref of
        Just parent -> Right parent
        Nothing -> Left $ "No binding parent for node: " ++ show (nodeRefKey ref)

-- | Look up multiple nodes, returning only those that exist.
--
-- Useful for batch operations where some nodes may be missing.
lookupNodes :: Constraint -> [NodeId] -> [TyNode]
lookupNodes c nids = mapMaybe (lookupNode c) nids

-- | Look up multiple nodes, using a default value for missing nodes.
lookupNodesWithDefault :: Constraint -> TyNode -> [NodeId] -> [TyNode]
lookupNodesWithDefault c defaultNode nids =
    map (\nid -> maybe defaultNode id (lookupNode c nid)) nids
