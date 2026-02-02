{- |
Module      : MLF.Constraint.Inert
Description : Inert and inert-locked node classification (thesis §5.2.2, §15.2.2)

This module provides helpers for computing inert nodes (nodes that do not
expose polymorphism through flexible binding paths) and inert-locked nodes
(inert nodes that are flexibly bound but have a rigid ancestor).

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
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindingError
    , Constraint(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , getNodeId
    , toListNode
    , typeRef
    )

-- | True for nodes that count as "intrinsically polymorphic" anchors when
-- computing inertness.
--
-- Thesis note: Definition 5.2.2 (inert nodes) uses intrinsically polymorphic
-- symbols (⊥ and Poly; see §5.2.1 for the symbol set).
isPolymorphicAnchor :: Constraint -> TyNode -> Bool
isPolymorphicAnchor _ TyBottom{} = True
isPolymorphicAnchor c TyBase{ tnBase = b } = Set.member b (cPolySyms c)
isPolymorphicAnchor _ _ = False

-- | Compute the set of inert nodes.
--
-- Definition 5.2.2 (Inert nodes).
inertNodes :: Constraint -> Either BindingError IntSet.IntSet
inertNodes c = do
    let nodes = cNodes c
        anchors0 =
            [ nid
            | (nid, node) <- toListNode nodes
            , isPolymorphicAnchor c node || isImplicitBottomAnchor node
            ]
        anchorSet0 = IntSet.fromList (map getNodeId anchors0)
        anchorSet = closeBoundAnchors anchorSet0
        anchors = map NodeId (IntSet.toList anchorSet)
        nonInert = collectFlexAncestors c anchors
        allNodes = IntSet.fromList (map (getNodeId . fst) (toListNode nodes))
    pure (IntSet.difference allNodes nonInert)
  where
    closeBoundAnchors set0 =
        let addBound acc (nid, node) = case node of
                TyVar{ tnBound = Just bnd } ->
                    if IntSet.member (getNodeId bnd) acc
                        then IntSet.insert (getNodeId nid) acc
                        else acc
                _ -> acc
            set1 = foldl' addBound set0 (toListNode (cNodes c))
        in if set1 == set0 then set0 else closeBoundAnchors set1

isImplicitBottomAnchor :: TyNode -> Bool
isImplicitBottomAnchor node = case node of
    TyVar{ tnBound = Nothing } -> True
    _ -> False

collectFlexAncestors :: Constraint -> [NodeId] -> IntSet.IntSet
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
inertLockedNodes :: Constraint -> Either BindingError IntSet.IntSet
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
weakenInertLockedNodes :: Constraint -> Either BindingError Constraint
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
            Just _ -> fst <$> GraphOps.applyWeaken (typeRef nid) c

-- | Weaken all inert nodes (flip their binding edge to rigid when flexible).
--
-- Thesis alignment: §15.2.8 applies weakening to all inert nodes; Corollary
-- 15.2.5 ensures the result is still a presolution inert-equivalent to the
-- original.
weakenInertNodes :: Constraint -> Either BindingError Constraint
weakenInertNodes c0 = do
    inert <- inertNodes c0
    foldM weakenOne c0 (IntSet.toList inert)
  where
    weakenOne c nidInt = do
        let nid = NodeId nidInt
        case Binding.lookupBindParent c (typeRef nid) of
            Nothing -> pure c
            Just (_, BindRigid) -> pure c
            Just _ -> fst <$> GraphOps.applyWeaken (typeRef nid) c
