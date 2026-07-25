{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Constraint.Inert
Description : Inert and inert-locked node classification (thesis §5.2.2, §15.2.2)

This module provides helpers for computing inert nodes (nodes that do not
expose polymorphism through flexible binding paths) and inert-locked nodes
(inert nodes that are flexibly bound but have a rigid ancestor).

= Phase Classification

All operations in this module are phase-insensitive. Inertness and
inert-locked classification depend only on binding-tree structure and
polymorphic-symbol membership, both of which are maintained at every
pipeline stage. The weakening helpers ('weakenInertLockedNodes',
'weakenInertNodes') mutate binding flags without relying on normalization
or acyclicity invariants. All helpers are polymorphic in @p@.

Paper anchor: `papers/these-finale-english.txt` Definition 5.2.2 (inert nodes)
and Definition 15.2.2 (inert-locked nodes).
-}
module MLF.Constraint.Inert (
    inertNodes,
    inertLockedNodes,
    weakenInertLockedNodes,
    weakenInertNodes
) where

import Control.Monad (foldM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindingError
    , Constraint(..)
    , NodeId(..)
    , NodeMap(..)
    , NodeRef(..)
    , NodeRefTag(..)
    , TyNode(..)
    , getNodeId
    , typeRef
    )

-- | True for nodes that count as "intrinsically polymorphic" anchors when
-- computing inertness.
--
-- Thesis note: Definition 5.2.2 (inert nodes) uses intrinsically polymorphic
-- symbols (⊥ and Poly; see §5.2.1 for the symbol set).
isPolymorphicAnchor :: Constraint p -> TyNode -> Bool
isPolymorphicAnchor _ TyBottom{} = True
isPolymorphicAnchor c TyBase{ tnBaseIdentity = identity } = Set.member identity (cPolySyms c)
isPolymorphicAnchor c TyCon{ tnConIdentity = identity } = Set.member identity (cPolySyms c)
isPolymorphicAnchor _ _ = False

-- | Compute the set of inert nodes.
--
-- Definition 5.2.2 (Inert nodes).
inertNodes :: Constraint p -> Either BindingError IntSet.IntSet
inertNodes c = do
    let nodeMap = getNodeMap (cNodes c)
        -- Single pass: collect anchors AND all node IDs
        (anchors0, allNodes) =
            IntMap.foldlWithKey'
                (\(!anc, !all') nid node ->
                    let all'' = IntSet.insert nid all'
                    in if isPolymorphicAnchor c node || isImplicitBottomAnchor node
                        then (IntSet.insert nid anc, all'')
                        else (anc, all'')
                )
                (IntSet.empty, IntSet.empty)
                nodeMap
        -- Eliminated variables remain in the graph only as witness provenance.
        -- They can carry exposure across a bound chain, but are not themselves
        -- live variables whose instantiability must survive W-normalization.
        nonInert =
            IntSet.difference
                (closeExposedPolymorphism nodeMap anchors0)
                (cEliminatedVars c)
    pure (IntSet.difference allNodes nonInert)
  where
    -- A bounded variable exposes any polymorphism exposed by its lower bound.
    -- In the graph representation that relation crosses two kinds of edge:
    -- flexible binding edges inside the bound, then the variable's `tnBound`
    -- edge.  Close the two relations together; doing either closure only once
    -- misses chains such as beta >= wrapper >= forall a. a -> a.
    closeExposedPolymorphism nodeMap exposed0 =
        let exposedByBinding =
                collectFlexAncestors c (map NodeId (IntSet.toList exposed0))
            exposeBoundVar acc nid node = case node of
                TyVar{ tnBound = Just bnd }
                    | IntSet.member (getNodeId bnd) exposedByBinding ->
                        IntSet.insert nid acc
                _ -> acc
            exposed = IntMap.foldlWithKey' exposeBoundVar exposedByBinding nodeMap
        in if exposed == exposedByBinding
            then exposed
            else closeExposedPolymorphism nodeMap exposed

isImplicitBottomAnchor :: TyNode -> Bool
isImplicitBottomAnchor node = case node of
    TyVar{ tnBound = Nothing } -> True
    _ -> False

collectFlexAncestors :: Constraint p -> [NodeId] -> IntSet.IntSet
collectFlexAncestors c anchors =
    go visited0 anchors
  where
    visited0 = IntSet.fromList (map getNodeId anchors)

    go visited [] = visited
    go visited (nid : rest) =
        case Binding.lookupBindParent c (typeRef nid) of
            Nothing -> go visited rest
            Just (_, BindRigid) -> go visited rest
            Just (parent, BindFlex) ->
                case parent of
                    TypeRef parentN ->
                        let pid = getNodeId parentN
                        in if IntSet.member pid visited
                            then go visited rest
                            else go (IntSet.insert pid visited) (parentN : rest)
                    GenRef _ -> go visited rest

-- | Compute inert-locked nodes: inert nodes that are flexibly bound and have a
-- rigid ancestor (Definition 15.2.2).
inertLockedNodes :: Constraint p -> Either BindingError IntSet.IntSet
inertLockedNodes c = do
    inert <- inertNodes c
    foldM addLocked IntSet.empty (IntSet.toList inert)
  where
    addLocked acc nidInt = do
        let nid = NodeId nidInt
        let checkLocked acc0 = do
                locked <- Binding.isUnderRigidBinder c (typeRef nid)
                pure $ if locked
                    then IntSet.insert nidInt acc0
                    else acc0
        case Binding.lookupBindParent c (typeRef nid) of
            Just (_, BindFlex) -> checkLocked acc
            _ -> pure acc

-- | Weaken inert-locked nodes (flip their binding edge to rigid when flexible).
--
-- Thesis alignment: Lemma 15.2.4 + Corollary 15.2.5 (§15.2.3.2) show we can
-- weaken inert-locked nodes to obtain an inert-equivalent presolution without
-- inert-locked nodes.
weakenInertLockedNodes :: Constraint p -> Either BindingError (Constraint p)
weakenInertLockedNodes c0 = go c0
  where
    go c = do
        locked <- inertLockedNodes c
        if IntSet.null locked
            then pure c
            else do
                c' <- foldM weakenOne c (IntSet.toList locked)
                go c'
    weakenOne c nidInt = do
        let nid = NodeId nidInt
        case Binding.lookupBindParent c (typeRef nid) of
            Nothing -> pure c
            Just (_, BindRigid) -> pure c
            Just _ -> fst <$> GraphOps.applyWeaken (TypeRefTag nid) c

-- | Weaken all inert nodes (flip their binding edge to rigid when flexible).
--
-- Thesis alignment: §15.2.8 applies weakening to all inert nodes; Corollary
-- 15.2.5 ensures the result is still a presolution inert-equivalent to the
-- original.
weakenInertNodes :: Constraint p -> Either BindingError (Constraint p)
weakenInertNodes c0 = do
    inert <- inertNodes c0
    foldM weakenOne c0 (IntSet.toList inert)
  where
    weakenOne c nidInt = do
        let nid = NodeId nidInt
        case Binding.lookupBindParent c (typeRef nid) of
            Nothing -> pure c
            Just (_, BindRigid) -> pure c
            Just _ -> fst <$> GraphOps.applyWeaken (TypeRefTag nid) c
