{- |
Module      : MLF.Binding.Adjustment
Description : Binding-edge harmonization for unification
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements binding-edge harmonization (paper Raise(n)) on the
paper-style binding tree.

Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@ §3.4)
describes Raise(n) as a binding-edge raising operation. This module provides
'harmonizeBindParentsWithTrace' which
computes the LCA of two nodes' binders and raises each node step-by-step
until both are bound at that LCA.

Note [Binding-Edge Harmonization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When unifying two nodes, their binding parents must be compatible. If they
are bound at different levels in the binding tree, we need to "raise" them
to their lowest common ancestor (LCA).

The harmonization process:
  1. Find the binding parents of both nodes
  2. Compute the LCA of those parents in the binding tree
  3. Raise each node step-by-step until its parent is the LCA
  4. Return the trace of Raise operations performed

This replaces the repo’s earlier “var-level metadata” approach: instead of
adjusting per-variable level fields, we update the binding-edge structure directly.
-}
module MLF.Binding.Adjustment (
    -- * Main API
    harmonizeBindParentsWithTrace,
    harmonizeBindParents,
    harmonizeBindParentsMulti,
    -- * Lower-level operations
    raiseToParent,
    raiseToParentWithCount,
) where

import MLF.Constraint.Types
import MLF.Binding.Tree (isUnderRigidBinder, lookupBindParent, bindingLCA, isBindingRoot)
import MLF.Binding.GraphOps (applyRaiseStep)

expectTypeRef :: NodeRef -> Either BindingError NodeId
expectTypeRef ref = case ref of
    TypeRef nid -> Right nid
    GenRef _ ->
        Left $
            InvalidBindingTree $
                "binding-tree harmonization expects type nodes, got gen node " ++ show ref

-- | Harmonize the binding parents of two nodes by raising them to their LCA.
--
-- This is the main entry point for binding-edge harmonization. It:
--   1. Finds the binding parents of both nodes
--   2. Computes the LCA of those parents
--   3. Raises each node to that LCA
--   4. Returns the updated constraint and the list of raised nodes
--
-- The returned list contains one entry per Raise step executed, with
-- multiplicity (a node raised twice appears twice).
--
-- Note: we no longer treat an empty `cBindParents` map as a “legacy mode”
-- signal. An empty map is a valid binding tree for root-only graphs.
--
-- Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@ §3.4)
harmonizeBindParentsWithTrace
    :: NodeRef -> NodeRef -> Constraint
    -> Either BindingError (Constraint, [NodeId])
harmonizeBindParentsWithTrace n1 n2 c0 = do
    _ <- expectTypeRef n1
    _ <- expectTypeRef n2
    -- Get binding parents for both nodes
    let mbParent1 = lookupBindParent c0 n1
        mbParent2 = lookupBindParent c0 n2

    case (mbParent1, mbParent2) of
        -- Both are roots: nothing to harmonize in the binding tree.
        (Nothing, Nothing) -> return (c0, [])

        -- One is a root: raise the other until its parent is also a root.
        (Nothing, Just (_p2, _)) -> do
            (c2, trace2) <- raiseToRoot n2 c0
            return (c2, trace2)

        (Just (_p1, _), Nothing) -> do
            (c2, trace1) <- raiseToRoot n1 c0
            return (c2, trace1)

        -- Both have parents: find LCA and raise both.
        (Just (p1, _), Just (p2, _)) ->
            case bindingLCA c0 p1 p2 of
                Right lca -> do
                    (c2, trace1) <- raiseToParentWithCount n1 lca c0
                    (c3, trace2) <- raiseToParentWithCount n2 lca c2
                    return (c3, trace1 ++ trace2)
                Left err ->
                    Left err

-- | Harmonize binding parents without returning the trace.
--
-- This is a convenience wrapper around 'harmonizeBindParentsWithTrace'.
-- On error, returns the original constraint unchanged (consistent with
-- 'raiseToParent' and the normalization pipeline's error-deferral strategy).
harmonizeBindParents :: NodeRef -> NodeRef -> Constraint -> Constraint
harmonizeBindParents n1 n2 c =
    case harmonizeBindParentsWithTrace n1 n2 c of
        Right (c', _) -> c'
        Left _ -> c

-- | Harmonize binding parents for multiple nodes simultaneously.
--
-- Given a list of NodeRefs (members of an equivalence class), compute the
-- LCA of all their bind parents and raise every member to that LCA.
-- This is the generalized Rebind from thesis Section 7.6.2: one Rebind
-- per equivalence class instead of one per pair.
--
-- Returns the updated constraint and the combined raise trace.
harmonizeBindParentsMulti
    :: [NodeRef] -> Constraint
    -> Either BindingError (Constraint, [NodeId])
harmonizeBindParentsMulti [] c = Right (c, [])
harmonizeBindParentsMulti [_] c = Right (c, [])
harmonizeBindParentsMulti refs c0 = do
    -- Collect all bind parents
    let parents =
            [ p
            | ref <- refs
            , Just (p, _) <- [lookupBindParent c0 ref]
            ]
        roots = [ref | ref <- refs, Nothing == lookupBindParent c0 ref]
    case (parents, roots) of
        -- All are roots: nothing to do
        ([], _) -> Right (c0, [])
        -- Some are roots: raise all non-roots to root level
        (_, (_:_)) -> do
            let nonRoots = [ref | ref <- refs, Nothing /= lookupBindParent c0 ref]
            go c0 [] nonRoots
        -- All have parents: fold LCA pairwise, then raise all to it
        (p1:pRest, []) -> do
            lca <- foldLCA p1 pRest
            goRaise c0 [] refs lca
  where
    go c trace [] = Right (c, trace)
    go c trace (ref:rest) = do
        (c', t) <- raiseToRoot ref c
        go c' (trace ++ t) rest

    goRaise c trace [] _ = Right (c, trace)
    goRaise c trace (ref:rest) lca = do
        (c', t) <- raiseToParentWithCount ref lca c
        goRaise c' (trace ++ t) rest lca

    foldLCA start [] = Right start
    foldLCA start (p:ps) = do
        lca <- bindingLCA c0 start p
        foldLCA lca ps

-- | Raise a node until its parent is the target.
--
-- Returns the updated constraint and the list of nodes that were raised
-- (with multiplicity).
raiseToParentWithCount
    :: NodeRef -> NodeRef -> Constraint
    -> Either BindingError (Constraint, [NodeId])
raiseToParentWithCount nid target c0 = go c0 []
  where
    go constraint trace = do
        nidT <- expectTypeRef nid
        -- Check current parent
        case lookupBindParent constraint nid of
            Nothing -> 
                -- Node is a root, can't raise
                return (constraint, reverse trace)
            Just (parent, _) ->
                if parent == target
                    then return (constraint, reverse trace)
                    else do
                        -- Check if we can raise
                        locked <- isUnderRigidBinder constraint nid
                        if locked
                            then Left (OperationOnLockedNode nid)
                            else do
                                -- Apply one raise step
                                result <- applyRaiseStep nid constraint
                                case result of
                                    (constraint', Just _) ->
                                        go constraint' (nidT : trace)
                                    (_constraint', Nothing) ->
                                        -- parent is already a root: cannot reach a non-root target
                                        Left (RaiseNotPossible nid)

-- | Raise a node to the target parent (convenience wrapper).
raiseToParent :: NodeRef -> NodeRef -> Constraint -> Constraint
raiseToParent nid target c =
    case raiseToParentWithCount nid target c of
        Right (c', _) -> c'
        Left _ -> c

-- | Raise a node until its parent is a root.
raiseToRoot :: NodeRef -> Constraint -> Either BindingError (Constraint, [NodeId])
raiseToRoot nid c0 = go c0 []
  where
    go constraint trace = do
        nidT <- expectTypeRef nid
        case lookupBindParent constraint nid of
            Nothing -> 
                -- Node is already a root
                return (constraint, reverse trace)
            Just (parent, _) ->
                if isBindingRoot constraint parent
                    then return (constraint, reverse trace)
                else do
                    -- Check if we can raise
                    locked <- isUnderRigidBinder constraint nid
                    if locked
                        then Left (OperationOnLockedNode nid)
                        else do
                            result <- applyRaiseStep nid constraint
                            case result of
                                (constraint', Just _) ->
                                    go constraint' (nidT : trace)
                                (_constraint', Nothing) ->
                                    Left (RaiseNotPossible nid)
