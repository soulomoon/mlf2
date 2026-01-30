{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.StateAccess
Description : Centralized state access operations for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides centralized access patterns for PresolutionM state,
eliminating duplicate boilerplate across presolution submodules.

= Design Rationale

Previously, 30+ locations duplicated patterns like:
@
c0 <- gets psConstraint
uf <- gets psUnionFind
let canonical = UnionFind.frWith uf
@

And binding tree operations with error lifting:
@
case Binding.lookupBindParentUnder canonical c0 ref of
    Left err -> throwError (BindingTreeError err)
    Right result -> ...
@

This module centralizes these patterns, making code more readable and reducing
duplication by ~150-200 lines across presolution submodules.

= Common Patterns

* Getting the canonical function from union-find state
* Getting constraint and canonical together
* Binding tree operations lifted to PresolutionM with error handling
* Node lookups with canonicalization
-}
module MLF.Constraint.Presolution.StateAccess (
    -- * Canonical function access
    getCanonical,
    getConstraintAndCanonical,
    withCanonical,

    -- * Binding tree operations (lifted to PresolutionM)
    lookupBindParentM,
    bindingPathToRootM,
    interiorOfM,
    boundFlexChildrenM,
    boundFlexChildrenAllM,
    orderedBindersM,
    checkBindingTreeM,

    -- * Node lookups with canonicalization
    lookupNodeCanonM,
    lookupGenNodeCanonM,
    getCanonicalNodeM,

    -- * Convenience re-exports
    liftBindingError
) where

import Control.Monad.State (gets)
import Control.Monad.Except (throwError)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), PresolutionState(..))

-- -----------------------------------------------------------------------------
-- Canonical function access
-- -----------------------------------------------------------------------------

-- | Get the canonical function from current union-find state.
--
-- This is the most common pattern in presolution: getting the function
-- that maps node IDs to their canonical representatives.
--
-- Replaces:
-- @
-- uf <- gets psUnionFind
-- let canonical = UnionFind.frWith uf
-- @
getCanonical :: PresolutionM (NodeId -> NodeId)
getCanonical = do
    uf <- gets psUnionFind
    pure (UnionFind.frWith uf)

-- | Get constraint and canonical function together.
--
-- This is the second most common pattern: getting both the constraint
-- and the canonical function for binding tree operations.
--
-- Replaces:
-- @
-- c0 <- gets psConstraint
-- uf <- gets psUnionFind
-- let canonical = UnionFind.frWith uf
-- @
getConstraintAndCanonical :: PresolutionM (Constraint, NodeId -> NodeId)
getConstraintAndCanonical = do
    c <- gets psConstraint
    uf <- gets psUnionFind
    pure (c, UnionFind.frWith uf)

-- | Run an action with canonical function and constraint.
--
-- Useful for pure computations that need both values.
--
-- Example:
-- @
-- result <- withCanonical $ \canonical c ->
--     someComputation canonical c nodeId
-- @
withCanonical :: ((NodeId -> NodeId) -> Constraint -> a) -> PresolutionM a
withCanonical f = do
    (c, canonical) <- getConstraintAndCanonical
    pure (f canonical c)

-- -----------------------------------------------------------------------------
-- Binding tree operations (lifted to PresolutionM)
-- -----------------------------------------------------------------------------

-- | Lift a BindingError to PresolutionError.
--
-- Use this when you need custom error handling for binding operations.
liftBindingError :: Either BindingError a -> PresolutionM a
liftBindingError = \case
    Left err -> throwError (BindingTreeError err)
    Right result -> pure result

-- | Look up the binding parent of a node, using canonical representatives.
--
-- Returns 'Nothing' if the node is a binding root.
--
-- Replaces:
-- @
-- case Binding.lookupBindParentUnder canonical c0 ref of
--     Left err -> throwError (BindingTreeError err)
--     Right result -> ...
-- @
lookupBindParentM :: NodeRef -> PresolutionM (Maybe (NodeRef, BindFlag))
lookupBindParentM ref = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.lookupBindParentUnder canonical c ref

-- | Trace the binding-parent chain from a node to a root.
--
-- Returns the path as a list of NodeRefs, starting with the given node
-- and ending with a root.
bindingPathToRootM :: NodeRef -> PresolutionM [NodeRef]
bindingPathToRootM start = do
    (c, canonical) <- getConstraintAndCanonical
    go canonical c IntSet.empty [start] start
  where
    go canonical c visited path ref
        | IntSet.member (nodeRefKey ref) visited =
            throwError (BindingTreeError (BindingCycleDetected (reverse path)))
        | otherwise = do
            mbParentInfo <- liftBindingError $ Binding.lookupBindParentUnder canonical c ref
            case mbParentInfo of
                Nothing -> pure (reverse path)
                Just (parent, _flag) ->
                    go canonical c
                       (IntSet.insert (nodeRefKey ref) visited)
                       (parent : path)
                       parent

-- | Compute the interior I(r): all nodes transitively bound to r.
--
-- The returned set contains canonical node keys.
interiorOfM :: NodeRef -> PresolutionM IntSet
interiorOfM root = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.interiorOfUnder canonical c root

-- | Get flexibly-bound TyVar children of a binder node.
--
-- This corresponds to Q(n) in the paper, restricted to variable nodes.
boundFlexChildrenM :: NodeRef -> PresolutionM [NodeId]
boundFlexChildrenM binder = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.boundFlexChildrenUnder canonical c binder

-- | Get flexibly-bound children (any node type) of a binder node.
--
-- TyExp is internal and skipped; TyBase/TyBottom are atomic.
boundFlexChildrenAllM :: NodeRef -> PresolutionM [NodeId]
boundFlexChildrenAllM binder = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.boundFlexChildrenAllUnder canonical c binder

-- | Get ordered binders for a binder node (leftmost-lowermost, paper ≺).
orderedBindersM :: NodeRef -> PresolutionM [NodeId]
orderedBindersM binder = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.orderedBinders canonical c binder

-- | Validate binding-tree invariants on the quotient graph.
checkBindingTreeM :: PresolutionM ()
checkBindingTreeM = do
    (c, canonical) <- getConstraintAndCanonical
    liftBindingError $ Binding.checkBindingTreeUnder canonical c

-- -----------------------------------------------------------------------------
-- Node lookups with canonicalization
-- -----------------------------------------------------------------------------

-- | Look up a type node using canonical representative.
--
-- Returns 'Nothing' if the node doesn't exist.
lookupNodeCanonM :: NodeId -> PresolutionM (Maybe TyNode)
lookupNodeCanonM nid = do
    (c, canonical) <- getConstraintAndCanonical
    pure $ NodeAccess.lookupNodeCanon canonical c nid

-- | Look up a gen node using canonical representative.
--
-- Returns 'Nothing' if the gen node doesn't exist.
lookupGenNodeCanonM :: GenNodeId -> PresolutionM (Maybe GenNode)
lookupGenNodeCanonM gid = do
    c <- gets psConstraint
    pure $ NodeAccess.lookupGenNode c gid

-- | Look up a node at its canonical representative, failing if not found.
--
-- This is a common pattern that combines findRoot + lookup.
getCanonicalNodeM :: NodeId -> PresolutionM TyNode
getCanonicalNodeM nid = do
    (c, canonical) <- getConstraintAndCanonical
    let canonNid = canonical nid
    case NodeAccess.lookupNode c canonNid of
        Just node -> pure node
        Nothing -> throwError $ NodeLookupFailed canonNid
