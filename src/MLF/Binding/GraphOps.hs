{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Binding.GraphOps
Description : Paper ω binding-tree operations as pure graph transformations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the paper's ω operations (Raise/Weaken) as pure
transformations on the binding tree. These operations modify only the
binding-edge structure, not the term-DAG.

= Phase Classification

All operations are phase-insensitive.  Raise and Weaken modify binding
flags without relying on normalization or acyclicity invariants.  Every
helper is polymorphic in the phase index @p@.

Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@
§3.1-§3.4)

Note [Raise and Weaken Operations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The paper defines two fundamental operations on the binding tree:

  - Weaken(n): Changes a flexibly bound node to rigidly bound.
    This "locks" the node, preventing further raising.

  - Raise(n): Moves a node's binding edge one step toward the root.
    If parent(n) = p and parent(p) = q, then after Raise(n), parent(n) = q.
    The flag is preserved.

Both operations preserve the binding-tree invariants:
  - Every non-root node has exactly one parent
  - Parent pointers are acyclic
  - Parents are "upper" than children in the term-DAG

Note [Instantiable vs Locked Nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A node is "instantiable" if its entire binding path to the root (including
its own edge) consists of flexible edges. A node is "locked" for the purposes
of `isLocked` if that condition fails, so restricted nodes (their own edge is
rigid) are treated as locked by this predicate.

For ω operations, the thesis distinguishes orange restricted nodes (own edge
rigid), which may be raised, from red locked nodes (own edge flexible with a
rigid ancestor), which may not. Raise therefore checks `nodeKind` directly;
Weaken still requires a flexible edge.
-}
module MLF.Binding.GraphOps (
    -- * Weaken operation
    applyWeaken,
    -- * Raise operations
    applyRaiseStep,
    applyRaiseTo,
    -- * Predicates
    isInstantiable,
    isLocked,
    -- * Utilities
    getBindFlag,
) where

import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness (InstanceOp (..))
import MLF.Binding.Tree
    ( NodeKind(..)
    , bindingPathToRoot
    , isBindingRoot
    , lookupBindParent
    , nodeKind
    , setBindParent
    )

-- | Get the binding flag for a node.
--
-- Returns 'Nothing' if the node is a binding root.
getBindFlag :: Constraint p -> NodeRefTag 'TypeTag -> Maybe BindFlag
getBindFlag c tag = fmap snd (lookupBindParent c (fromNodeRefTag tag))

-- | Check if a node is instantiable (all-flexible binding path).
--
-- A node is instantiable if:
--   1. It is not a binding root (has a parent)
--   2. Its entire binding path to the root consists of flexible edges
--
-- Paper reference: instantiable nodes have no rigid edges on their binding
-- path (full-flexible flag path).
isInstantiable :: Constraint p -> NodeRefTag 'TypeTag -> Either BindingError Bool
isInstantiable c tag = do
    let nid = fromNodeRefTag tag
    -- Check if it's a root (roots cannot be raised)
    if isBindingRoot c nid
        then return False
        else do
            -- Get the path to root and check all flags
            path <- bindingPathToRoot c nid
            -- Check that all edges on the path are flexible
            -- (except the root which has no parent)
            let checkFlags [] = return True
                checkFlags [_] = return True  -- Root has no parent, so no flag to check
                checkFlags (n:rest) = do
                    case lookupBindParent c n of
                        Nothing -> return True  -- Reached root
                        Just (_, BindRigid) -> return False  -- Found rigid edge
                        Just (_, BindFlex) -> checkFlags rest
            checkFlags path

-- | Check if a node is locked (has a rigid edge on its binding path).
--
-- A node is locked if any edge on its binding path to the root is rigid.
-- Note: Raise operations in this module check only *strict* rigid ancestors
-- via `isUnderRigidBinder`, so a restricted node (own edge rigid) is treated
-- as locked here but can still be raised.
isLocked :: Constraint p -> NodeRefTag 'TypeTag -> Either BindingError Bool
isLocked c tag = do
    instantiable <- isInstantiable c tag
    return (not instantiable)

-- | Apply the Weaken operation to a node.
--
-- Weaken(n) changes the binding flag of n from flexible to rigid.
--
-- Preconditions:
--   - n must not be a binding root
--   - n must be flexibly bound
--
-- Returns the updated constraint and the operation that was applied.
--
-- Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@ §3.1)
applyWeaken :: NodeRefTag 'TypeTag -> Constraint p -> Either BindingError (Constraint p, InstanceOp)
applyWeaken tag c = do
    let nid = fromNodeRefTag tag
        nidT = nodeIdFromTypeRef tag
    -- Check that the node has a binding parent
    case lookupBindParent c nid of
        Nothing -> Left $ MissingBindParent nid
        Just (_parent, BindRigid) -> Left $ OperationOnLockedNode nid
        Just (parent, BindFlex) -> do
    -- Change the flag to rigid and remember this binder was weakened.
            let c' = setBindParent nid (parent, BindRigid) c
            let c'' = c' { cWeakenedVars = IntSet.insert (getNodeId nidT) (cWeakenedVars c') }
            return (c'', OpWeaken nidT)

-- | Apply a single Raise step to a node.
--
-- Raise(n) moves n's binding edge one step toward the root:
--   If parent(n) = p and parent(p) = q, then after Raise(n), parent(n) = q.
--   The binding flag is preserved.
--
-- Preconditions:
--   - n must not be a binding root
--   - n must be non-red (instantiable or restricted, but not locked)
--   - parent(n) must not be a root (otherwise there's nowhere to raise to)
--
-- Returns:
--   - Right (c', Just op) if the raise was performed
--   - Right (c, Nothing) if n's parent is already a root (no-op)
--   - Left error if preconditions are violated
--
-- Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@ §3.1)
-- "slide over" semantics
applyRaiseStep :: NodeRefTag 'TypeTag -> Constraint p -> Either BindingError (Constraint p, Maybe InstanceOp)
applyRaiseStep tag c = do
    let nid = fromNodeRefTag tag
        nidT = nodeIdFromTypeRef tag
    -- Check that the node has a binding parent
    case lookupBindParent c nid of
        Nothing -> Left $ MissingBindParent nid
        Just (parent, flag) -> do
            -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` Fig. 10):
            -- operations "under a
            -- rigidly bound node" must be absent from normalized witnesses.
            --
            -- We treat a node as "locked" iff it is *strictly* under a rigid
            -- binding edge. A restricted node (its own edge rigid) is still a
            -- valid Raise target; its translation is the identity instantiation.
            kind <- nodeKind c nid
            case kind of
                NodeLocked -> Left $ OperationOnLockedNode nid
                _ -> do
                    -- Check if parent has a parent (grandparent)
                    case lookupBindParent c parent of
                        Nothing ->
                            -- Parent is a root, can't raise further
                            return (c, Nothing)
                        Just (grandparent, _) -> do
                            -- Move n's binding edge to grandparent, preserving flag
                            let c' = setBindParent nid (grandparent, flag) c
                            return (c', Just (OpRaise nidT))

-- | Raise a node to a specific ancestor binder.
--
-- This walks the binding path once to verify the target is an ancestor,
-- classifies the node once, then directly rebinds the
-- node to the target.  The previous implementation called 'applyRaiseStep'
-- in a loop, which re-walked the path via 'isUnderRigidBinder' at every
-- step (O(R * L) where R = raise count, L = path length).  The new
-- implementation is O(L): one path walk, one node-kind check, one
-- parent update.
--
-- Preconditions:
--   - n must not be a binding root
--   - n must be non-red (instantiable or restricted, but not locked)
--   - target must be an ancestor of n in the binding tree
--
-- Returns the updated constraint and the list of Raise operations applied.
--
-- The child stays type-indexed, but the target remains the mixed 'NodeRef'
-- seam because a valid ancestor can still be either a type binder or the
-- retained gen-root boundary.
--
-- Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@ §3.1)
applyRaiseTo :: NodeRefTag 'TypeTag -> NodeRef -> Constraint p -> Either BindingError (Constraint p, [InstanceOp])
applyRaiseTo tag target c = do
    let nid = fromNodeRefTag tag
        nidT = nodeIdFromTypeRef tag
    -- Walk the binding path once: verify target is an ancestor.
    path <- bindingPathToRoot c nid
    let pathSet = IntSet.fromList $ map nodeRefKey path
    if not (IntSet.member (nodeRefKey target) pathSet)
        then Left $ InvalidBindingTree $
            "Target " ++ show target ++
            " is not an ancestor of " ++ show nid
        else do
            -- Single paper node-kind check on the original path.
            -- Ancestors' binding edges do not change across raise steps
            -- (only nid's own parent pointer is mutated), so one check
            -- is equivalent to checking at every step of the old loop.
            kind <- nodeKind c nid
            case kind of
                NodeLocked -> Left $ OperationOnLockedNode nid
                _ -> do
                    -- Get current binding edge (parent and flag).
                    case lookupBindParent c nid of
                        Nothing -> Left $ MissingBindParent nid
                        Just (parent, flag) ->
                            if parent == target
                                then return (c, [])
                                else do
                                    -- Count raise steps from the path:
                                    -- nodes strictly between nid's parent and
                                    -- the target correspond 1-to-1 with steps.
                                    let numSteps =
                                            length $
                                            takeWhile (\r -> nodeRefKey r /= nodeRefKey target) $
                                            drop 1 path
                                    -- Directly rebind nid to target, preserving flag.
                                    let c' = setBindParent nid (target, flag) c
                                    let ops = replicate numSteps (OpRaise nidT)
                                    return (c', ops)
