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
    -- * Canonicalization-aware lookups
    lookupNodeCanon,
    lookupGenNodeCanon,
    -- * BindingError-returning lookups
    requireNode,
    requireGenNode,
    -- * Node type predicates
    isVar,
    isForall,
    isArrow,
    isBase,
    isBottom,
    -- * Structural access
    lookupStructuralChildren,
    lookupStructuralChildrenWithBounds,
    -- * Bound lookup
    lookupVarBound,
    lookupVarBoundSafe,
    -- * Bind parent lookup
    lookupBindParent,
    lookupBindParentSafe,
    -- * Batch operations
    lookupNodes,
    lookupNodesWithDefault,
    -- * Collection accessors
    allGenNodes,
    allNodes,
    allNodeIds,
    allGenNodeIds
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.Maybe (mapMaybe)

import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindingError(..)
    , Constraint(..)
    , GenNode
    , GenNodeId(..)
    , NodeId(..)
    , NodeRef
    , TyNode(..)
    , getGenNodeId
    , getNodeId
    , nodeRefKey
    , structuralChildren
    , structuralChildrenWithBounds
    , toListGen
    , toListNode
    )

-- | Look up a type node in the constraint graph.
--
-- Returns 'Nothing' if the node is not present.
lookupNode :: Constraint p -> NodeId -> Maybe TyNode
lookupNode c nid = Graph.lookupNode nid (cNodes c)

-- | Look up a type node, returning an error if not found.
--
-- Use this when the node should always exist (e.g., after validation).
lookupNodeSafe :: Constraint p -> NodeId -> Either String TyNode
lookupNodeSafe c nid =
    case lookupNode c nid of
        Just node -> Right node
        Nothing -> Left $ "Node not found: " ++ show (getNodeId nid)

-- | Look up a gen node in the constraint graph.
--
-- Returns 'Nothing' if the gen node is not present.
lookupGenNode :: Constraint p -> GenNodeId -> Maybe GenNode
lookupGenNode c gid = Graph.lookupGen gid (cGenNodes c)

-- | Look up a gen node, returning an error if not found.
lookupGenNodeSafe :: Constraint p -> GenNodeId -> Either String GenNode
lookupGenNodeSafe c gid =
    case lookupGenNode c gid of
        Just node -> Right node
        Nothing -> Left $ "Gen node not found: " ++ show (getGenNodeId gid)

-- | Look up the instance bound of a variable.
--
-- Missing entries are treated as ⊥ (returns 'Nothing').
-- Non-variable nodes also return 'Nothing'.
lookupVarBound :: Constraint p -> NodeId -> Maybe NodeId
lookupVarBound c v =
    case lookupNode c v of
        Just TyVar{ tnBound = mb } -> mb
        _ -> Nothing

-- | Look up the instance bound of a variable, returning an error if the node
-- is not a variable or doesn't exist.
lookupVarBoundSafe :: Constraint p -> NodeId -> Either String (Maybe NodeId)
lookupVarBoundSafe c v =
    case lookupNode c v of
        Just TyVar{ tnBound = mb } -> Right mb
        Just _ -> Left $ "Node is not a variable: " ++ show (getNodeId v)
        Nothing -> Left $ "Node not found: " ++ show (getNodeId v)

-- | Look up the binding parent of a node reference.
--
-- Returns 'Nothing' if the node has no binding parent (i.e., it's a root).
lookupBindParent :: Constraint p -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent c ref = IntMap.lookup (nodeRefKey ref) (cBindParents c)

-- | Look up the binding parent, returning an error if not found.
lookupBindParentSafe :: Constraint p -> NodeRef -> Either String (NodeRef, BindFlag)
lookupBindParentSafe c ref =
    case lookupBindParent c ref of
        Just parent -> Right parent
        Nothing -> Left $ "No binding parent for node: " ++ show (nodeRefKey ref)

-- | Look up multiple nodes, returning only those that exist.
--
-- Useful for batch operations where some nodes may be missing.
lookupNodes :: Constraint p -> [NodeId] -> [TyNode]
lookupNodes c nids = mapMaybe (lookupNode c) nids

-- | Look up multiple nodes, using a default value for missing nodes.
lookupNodesWithDefault :: Constraint p -> TyNode -> [NodeId] -> [TyNode]
lookupNodesWithDefault c defaultNode nids =
    map (\nid -> maybe defaultNode id (lookupNode c nid)) nids

-- -----------------------------------------------------------------------------
-- Canonicalization-aware lookups
-- -----------------------------------------------------------------------------

-- | Look up a type node with canonicalization.
--
-- Applies the canonical function to the node ID before looking up.
-- This is useful for Presolution modules that use union-find.
lookupNodeCanon :: (NodeId -> NodeId) -> Constraint p -> NodeId -> Maybe TyNode
lookupNodeCanon canonical c nid = lookupNode c (canonical nid)

-- | Look up a gen node with canonicalization.
lookupGenNodeCanon :: (GenNodeId -> GenNodeId) -> Constraint p -> GenNodeId -> Maybe GenNode
lookupGenNodeCanon canonical c gid = lookupGenNode c (canonical gid)

-- -----------------------------------------------------------------------------
-- BindingError-returning lookups
-- -----------------------------------------------------------------------------

-- | Look up a type node, returning a BindingError if not found.
--
-- Use this in contexts where missing nodes indicate an invalid binding tree.
requireNode :: Constraint p -> NodeId -> Either BindingError TyNode
requireNode c nid =
    case lookupNode c nid of
        Just node -> Right node
        Nothing -> Left $ InvalidBindingTree $ "Node not found: " ++ show (getNodeId nid)

-- | Look up a gen node, returning a BindingError if not found.
requireGenNode :: Constraint p -> GenNodeId -> Either BindingError GenNode
requireGenNode c gid =
    case lookupGenNode c gid of
        Just node -> Right node
        Nothing -> Left $ InvalidBindingTree $ "Gen node not found: " ++ show (getGenNodeId gid)

-- -----------------------------------------------------------------------------
-- Node type predicates
-- -----------------------------------------------------------------------------

-- | Check if a node is a type variable.
isVar :: Constraint p -> NodeId -> Bool
isVar c nid = case lookupNode c nid of
    Just TyVar{} -> True
    _ -> False

-- | Check if a node is a forall.
isForall :: Constraint p -> NodeId -> Bool
isForall c nid = case lookupNode c nid of
    Just TyForall{} -> True
    _ -> False

-- | Check if a node is an arrow.
isArrow :: Constraint p -> NodeId -> Bool
isArrow c nid = case lookupNode c nid of
    Just TyArrow{} -> True
    _ -> False

-- | Check if a node is a base type.
isBase :: Constraint p -> NodeId -> Bool
isBase c nid = case lookupNode c nid of
    Just TyBase{} -> True
    _ -> False

-- | Check if a node is bottom.
isBottom :: Constraint p -> NodeId -> Bool
isBottom c nid = case lookupNode c nid of
    Just TyBottom{} -> True
    _ -> False

-- -----------------------------------------------------------------------------
-- Structural access
-- -----------------------------------------------------------------------------

-- | Get structural children of a node.
--
-- Returns Nothing if the node doesn't exist, or Just the children list.
lookupStructuralChildren :: Constraint p -> NodeId -> Maybe [NodeId]
lookupStructuralChildren c nid =
    structuralChildren <$> lookupNode c nid

-- | Get structural children of a node, including bound references.
--
-- Returns Nothing if the node doesn't exist, or Just the children list.
lookupStructuralChildrenWithBounds :: Constraint p -> NodeId -> Maybe [NodeId]
lookupStructuralChildrenWithBounds c nid =
    structuralChildrenWithBounds <$> lookupNode c nid

-- -----------------------------------------------------------------------------
-- Collection accessors
-- -----------------------------------------------------------------------------

-- | Get all gen nodes in the constraint.
--
-- This is a common pattern that appears 27+ times across the codebase.
-- Replaces: @IntMap.elems (cGenNodes c)@
allGenNodes :: Constraint p -> [GenNode]
allGenNodes c = map snd (toListGen (cGenNodes c))

-- | Get all type nodes in the constraint.
--
-- Replaces: @IntMap.elems (cNodes c)@
allNodes :: Constraint p -> [TyNode]
allNodes c = map snd (toListNode (cNodes c))

-- | Get all type node IDs as an IntSet.
--
-- Replaces: @IntSet.fromList (IntMap.keys (cNodes c))@
allNodeIds :: Constraint p -> IntSet
allNodeIds c =
    IntSet.fromList (map (getNodeId . fst) (toListNode (cNodes c)))

-- | Get all gen node IDs as an IntSet.
--
-- Replaces: @IntSet.fromList (IntMap.keys (cGenNodes c))@
allGenNodeIds :: Constraint p -> IntSet
allGenNodeIds c =
    IntSet.fromList (map (getGenNodeId . fst) (toListGen (cGenNodes c)))
