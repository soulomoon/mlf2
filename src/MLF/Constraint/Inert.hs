{- |
Module      : MLF.Constraint.Inert
Description : Inert and inert-locked node classification (thesis §5.2/§15.2)

This module provides helpers for computing inert nodes (nodes that do not
expose polymorphism through flexible binding paths) and inert-locked nodes
(inert nodes that are flexibly bound but have a rigid ancestor).

Paper anchor: `papers/these-finale-english.txt` §5.2.4 (inert nodes) and §15.2.3
inert-locked nodes.
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

import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types (BindFlag(..), BindingError, Constraint(..), NodeId(..), TyNode(..), getNodeId)

-- | True for nodes that count as "intrinsically polymorphic" anchors when
-- computing inertness.
--
-- Thesis note: polymorphic symbols include ⊥ (§5.2.1). We currently treat
-- ⊥ as the only polymorphic constructor in the term-DAG.
isPolymorphicAnchor :: TyNode -> Bool
isPolymorphicAnchor TyBottom{} = True
isPolymorphicAnchor _ = False

-- | Compute the set of inert nodes.
--
-- A node is inert if there is no flex-only binding path from a polymorphic
-- anchor (⊥ / Poly constructors) to that node.
inertNodes :: Constraint -> Either BindingError IntSet.IntSet
inertNodes c = do
    let nodes = cNodes c
        anchors =
            [ NodeId nid
            | (nid, node) <- IntMap.toList nodes
            , isPolymorphicAnchor node
            ]
        nonInert = collectFlexAncestors c anchors
        allNodes = IntSet.fromList (IntMap.keys nodes)
    pure (IntSet.difference allNodes nonInert)

collectFlexAncestors :: Constraint -> [NodeId] -> IntSet.IntSet
collectFlexAncestors c anchors =
    go visited0 anchors
  where
    visited0 = IntSet.fromList (map getNodeId anchors)

    go visited [] = visited
    go visited (nid : rest) =
        case Binding.lookupBindParent c nid of
            Nothing -> go visited rest
            Just (_, BindRigid) -> go visited rest
            Just (parent, BindFlex) ->
                let pid = getNodeId parent
                in if IntSet.member pid visited
                    then go visited rest
                    else go (IntSet.insert pid visited) (parent : rest)

-- | Compute inert-locked nodes: inert nodes that are flexibly bound and have a
-- rigid ancestor (thesis §15.2.2).
inertLockedNodes :: Constraint -> Either BindingError IntSet.IntSet
inertLockedNodes c = do
    inert <- inertNodes c
    foldM addLocked IntSet.empty (IntSet.toList inert)
  where
    addLocked acc nidInt = do
        let nid = NodeId nidInt
        case Binding.lookupBindParent c nid of
            Just (_, BindFlex) -> do
                locked <- Binding.isUnderRigidBinder c nid
                pure $ if locked
                    then IntSet.insert nidInt acc
                    else acc
            _ -> pure acc

-- | Weaken inert-locked nodes (flip their binding edge to rigid when flexible).
--
-- Thesis alignment (§15.2.3.2): weakening inert-locked nodes yields an
-- inert-equivalent presolution without inert-locked nodes.
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
        case Binding.lookupBindParent c nid of
            Nothing -> pure c
            Just (_, BindRigid) -> pure c
            Just _ -> fst <$> GraphOps.applyWeaken nid c

-- | Weaken all inert nodes (flip their binding edge to rigid when flexible).
--
-- Thesis alignment (§15.2.3.2): weakening inert nodes yields an inert-equivalent
-- presolution without inert-locked nodes.
weakenInertNodes :: Constraint -> Either BindingError Constraint
weakenInertNodes c0 = do
    inert <- inertNodes c0
    foldM weakenOne c0 (IntSet.toList inert)
  where
    weakenOne c nidInt = do
        let nid = NodeId nidInt
        case Binding.lookupBindParent c nid of
            Nothing -> pure c
            Just (_, BindRigid) -> pure c
            Just _ -> fst <$> GraphOps.applyWeaken nid c
