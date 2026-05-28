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
isPolymorphicAnchor c TyBase{ tnBase = b } = Set.member b (cPolySyms c)
isPolymorphicAnchor c TyCon{ tnCon = con } = Set.member con (cPolySyms c)
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
        anchorSet = closeBoundAnchors nodeMap anchors0
        anchors = map NodeId (IntSet.toList anchorSet)
        nonInert = collectFlexAncestors c anchors
    pure (IntSet.difference allNodes nonInert)
  where
    closeBoundAnchors nodeMap set0 =
        let addBound acc nid node = case node of
                TyVar{ tnBound = Just bnd } ->
                    if IntSet.member (getNodeId bnd) acc
                        then IntSet.insert nid acc
                        else acc
                _ -> acc
            set1 = IntMap.foldlWithKey' addBound set0 nodeMap
        in if set1 == set0 then set0 else closeBoundAnchors nodeMap set1

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
