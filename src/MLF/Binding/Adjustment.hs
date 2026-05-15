{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Binding.Adjustment
Description : Binding-edge harmonization for unification
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements binding-edge harmonization (paper Raise(n)) on the
paper-style binding tree.

= Phase Classification

All operations are phase-insensitive.  Harmonization reads and mutates
binding edges without relying on normalization or acyclicity invariants.
Every helper is polymorphic in the phase index @p@.

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

import MLF.Constraint.Types.Graph
import MLF.Binding.Tree (isUnderRigidBinder, lookupBindParent, bindingLCA, isBindingRoot)
import MLF.Binding.GraphOps (applyRaiseStep)

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
    :: NodeRefTag 'TypeTag -> NodeRefTag 'TypeTag -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
harmonizeBindParentsWithTrace n1Tag n2Tag c0 =
    -- Get binding parents for both nodes
    let n1 = fromNodeRefTag n1Tag
        n2 = fromNodeRefTag n2Tag
        mbParent1 = lookupBindParent c0 n1
        mbParent2 = lookupBindParent c0 n2
    in case (mbParent1, mbParent2) of
        -- Both are roots: nothing to harmonize in the binding tree.
        (Nothing, Nothing) -> return (c0, [])

        -- One is a root: raise the other until its parent is also a root.
        (Nothing, Just (_p2, _)) -> do
            (c2, trace2) <- raiseToRoot n2Tag c0
            return (c2, trace2)

        (Just (_p1, _), Nothing) -> do
            (c2, trace1) <- raiseToRoot n1Tag c0
            return (c2, trace1)

        -- Both have parents: find LCA and raise both.
        (Just (p1, _), Just (p2, _)) ->
            case bindingLCA c0 p1 p2 of
                Right lca -> do
                    (c2, trace1) <- raiseToParentWithCount n1Tag lca c0
                    (c3, trace2) <- raiseToParentWithCount n2Tag lca c2
                    return (c3, trace1 ++ trace2)
                Left err ->
                    Left err

-- | Harmonize binding parents without returning the trace.
--
-- This is a convenience wrapper around 'harmonizeBindParentsWithTrace'.
-- On error, returns the original constraint unchanged (consistent with
-- 'raiseToParent' and the normalization pipeline's error-deferral strategy).
harmonizeBindParents :: NodeRefTag 'TypeTag -> NodeRefTag 'TypeTag -> Constraint p -> Constraint p
harmonizeBindParents n1 n2 c =
    case harmonizeBindParentsWithTrace n1 n2 c of
        Right (c', _) -> c'
        Left _ -> c

-- | Harmonize binding parents for multiple nodes simultaneously.
--
-- Given a list of type-node refs (members of an equivalence class), compute the
-- LCA of all their bind parents and raise every member to that LCA.
-- This is the generalized Rebind from thesis Section 7.6.2: one Rebind
-- per equivalence class instead of one per pair.
--
-- Returns the updated constraint and the combined raise trace.
harmonizeBindParentsMulti
    :: [NodeRefTag 'TypeTag] -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
harmonizeBindParentsMulti [] c = Right (c, [])
harmonizeBindParentsMulti [_] c = Right (c, [])
harmonizeBindParentsMulti refs c0 = do
    -- Collect all bind parents
    let parents =
            [ p
            | refTag <- refs
            , let ref = fromNodeRefTag refTag
            , Just (p, _) <- [lookupBindParent c0 ref]
            ]
        roots =
            [ refTag
            | refTag <- refs
            , Nothing == lookupBindParent c0 (fromNodeRefTag refTag)
            ]
    case (parents, roots) of
        -- All are roots: nothing to do
        ([], _) -> Right (c0, [])
        -- Some are roots: raise all non-roots to root level
        (_, (_:_)) -> do
            let nonRoots =
                    [ refTag
                    | refTag <- refs
                    , Nothing /= lookupBindParent c0 (fromNodeRefTag refTag)
                    ]
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
    :: NodeRefTag 'TypeTag -> NodeRef -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
raiseToParentWithCount ref target c0 = do
    let nid = fromNodeRefTag ref
        nidT = nodeIdFromTypeRef ref
    go nid nidT c0 []
  where
    go nid nidT constraint trace =
        case lookupBindParent constraint nid of
            Nothing ->
                return (constraint, reverse trace)
            Just (parent, _) ->
                if parent == target
                    then return (constraint, reverse trace)
                    else do
                        locked <- isUnderRigidBinder constraint nid
                        if locked
                            then Left (OperationOnLockedNode nid)
                            else do
                                result <- applyRaiseStep ref constraint
                                case result of
                                    (constraint', Just _) ->
                                        go nid nidT constraint' (nidT : trace)
                                    (_constraint', Nothing) ->
                                        Left (RaiseNotPossible nid)

-- | Raise a node to the target parent (convenience wrapper).
raiseToParent :: NodeRefTag 'TypeTag -> NodeRef -> Constraint p -> Constraint p
raiseToParent nid target c =
    case raiseToParentWithCount nid target c of
        Right (c', _) -> c'
        Left _ -> c

-- | Raise a node until its parent is a root.
raiseToRoot :: NodeRefTag 'TypeTag -> Constraint p -> Either BindingError (Constraint p, [NodeId])
raiseToRoot ref c0 = do
    let nid = fromNodeRefTag ref
        nidT = nodeIdFromTypeRef ref
    go nid nidT c0 []
  where
    go nid nidT constraint trace =
        case lookupBindParent constraint nid of
            Nothing ->
                return (constraint, reverse trace)
            Just (parent, _) ->
                if isBindingRoot constraint parent
                    then return (constraint, reverse trace)
                    else do
                        locked <- isUnderRigidBinder constraint nid
                        if locked
                            then Left (OperationOnLockedNode nid)
                            else do
                                result <- applyRaiseStep ref constraint
                                case result of
                                    (constraint', Just _) ->
                                        go nid nidT constraint' (nidT : trace)
                                    (_constraint', Nothing) ->
                                        Left (RaiseNotPossible nid)
