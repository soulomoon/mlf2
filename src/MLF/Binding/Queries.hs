{-# LANGUAGE BangPatterns #-}
{- |
Module      : MLF.Binding.Queries
Description : Binding tree query operations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides query operations for binding trees: path tracing,
interior computation, LCA, root detection, and node classification.

Extracted from MLF.Binding.Tree for modularity.
-}
module MLF.Binding.Queries (
    -- * Path operations
    bindingPathToRoot,
    bindingPathToRootWithLookup,
    bindingPathToRootLocal,
    bindingLCA,
    -- * Interior computation
    interiorOf,
    interiorOfUnder,
    -- * Root detection
    bindingRoots,
    isBindingRoot,
    -- * Node classification (paper §3.1)
    NodeKind(..),
    nodeKind,
    isUnderRigidBinder,
    -- * Basic lookup (re-exported for convenience)
    lookupBindParent,
) where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import MLF.Constraint.Types.Graph

import MLF.Binding.NodeRefs (
    allNodeRefs,
    nodeRefExists,
    )
import MLF.Binding.Path (
    bindingPathToRoot,
    bindingPathToRootLocal,
    bindingPathToRootWithLookup,
    validateBindingPathsAcyclic,
    )
-- Import canonicalization functions
import MLF.Binding.Canonicalization (
    withQuotientBindParents,
    )

-- | Look up the binding parent and flag for a node.
--
-- Returns 'Nothing' if the node is a binding root (has no parent).
lookupBindParent :: Constraint p -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent c ref = IntMap.lookup (nodeRefKey ref) (cBindParents c)

-- | Paper node kinds (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1).
--
-- These are derived from binding-edge flags along the binding-parent path.
data NodeKind
    = NodeRoot
    | NodeInstantiable
    | NodeRestricted
    | NodeLocked
    deriving (Eq, Show)

-- | Check if a node is a binding root.
isBindingRoot :: Constraint p -> NodeRef -> Bool
isBindingRoot c ref = not $ IntMap.member (nodeRefKey ref) (cBindParents c)

-- | Compute the set of binding roots (nodes with no binding parent).
bindingRoots :: Constraint p -> [NodeRef]
bindingRoots c = filter (isBindingRoot c) (allNodeRefs c)

-- | Compute the lowest common ancestor of two nodes in the binding tree.
--
-- Returns an error if either node has a cycle in its binding path.
bindingLCA :: Constraint p -> NodeRef -> NodeRef -> Either BindingError NodeRef
bindingLCA c n1 n2 = do
    path1 <- bindingPathToRoot c n1
    path2 <- bindingPathToRoot c n2
    let set1 = IntSet.fromList $ map nodeRefKey path1
        -- Find the first node in path2 that is also in path1
        common = filter (\ref -> IntSet.member (nodeRefKey ref) set1) path2
    case common of
        [] -> Left $ NoCommonAncestor n1 n2
        (lca:_) -> Right lca

-- | Classify a node according to the paper's instantiable/restricted/locked
-- taxonomy (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1).
--
--   - Root: no binding parent.
--   - Restricted: the node's own binding edge is rigid.
--   - Locked: the node's own edge is flexible, but some strict ancestor edge is rigid.
--   - Instantiable: the entire binding path to the root is flexible.
nodeKind :: Constraint p -> NodeRef -> Either BindingError NodeKind
nodeKind c nid = case lookupBindParent c nid of
    Nothing                     -> Right NodeRoot
    Just (_, BindRigid)         -> Right NodeRestricted
    Just (parentRef, BindFlex)  -> do
        underRigid <- isUnderRigidBinderFromParent c nid parentRef
        pure $ if underRigid then NodeLocked else NodeInstantiable

-- | True iff @nid@ is strictly under some rigid binding edge.
--
-- This intentionally does /not/ treat a restricted node as "under rigid" purely
-- because its own binding edge is rigid: only strict ancestors are considered.
--
-- Paper anchor (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4):
-- normalized witnesses do not perform operations under rigidly bound nodes.
isUnderRigidBinder :: Constraint p -> NodeRef -> Either BindingError Bool
isUnderRigidBinder c nid =
    case lookupBindParent c nid of
        Nothing -> Right False
        Just (parentRef, _ownFlag) ->
            isUnderRigidBinderFromParent c nid parentRef

isUnderRigidBinderFromParent :: Constraint p -> NodeRef -> NodeRef -> Either BindingError Bool
isUnderRigidBinderFromParent c nid parent0 =
    go (IntSet.singleton startKey) False (nodeRefKey parent0)
  where
    bindParents = cBindParents c
    startKey = nodeRefKey nid
    lookupParent key = IntMap.lookup key bindParents

    go !seen !hasRigid !key
        | IntSet.member key seen =
            case bindingPathToRootWithLookup lookupParent nid of
                Left err -> Left err
                Right path -> Left (BindingCycleDetected path)
        | otherwise =
            case lookupParent key of
                Nothing -> Right hasRigid
                Just (parentRef, flag) ->
                    let !seen' = IntSet.insert key seen
                        !hasRigid' = hasRigid || flag == BindRigid
                        !parentKey = nodeRefKey parentRef
                    in go
                        seen'
                        hasRigid'
                        parentKey

-- | Compute the interior I(r): all nodes transitively bound to r.
--
-- This includes r itself and all nodes whose binding path passes through r.
interiorOf :: Constraint p -> NodeRef -> Either BindingError IntSet
interiorOf c root = do
    unless (nodeRefExists c root) $
        Left $ InvalidBindingTree $ "interiorOf: root " ++ show root ++ " not in constraint"
    let refs = allNodeRefs c
    validateBindingPathsAcyclic (cBindParents c) refs
    let liveKeys = IntSet.fromList (map nodeRefKey refs)
        childrenByParent =
            IntMap.foldlWithKey'
                (\m childKey (parent, _flag) ->
                    insertChild (nodeRefKey parent) childKey m
                )
                IntMap.empty
                (cBindParents c)
    pure $
        IntSet.intersection
            liveKeys
            (descendantsFromChildren childrenByParent (nodeRefKey root))

-- | Compute the interior I(r) on the quotient graph induced by a canonicalization
-- function.
--
-- This mirrors `checkBindingTreeUnder`: binding edges are first rewritten to
-- canonical representatives (dropping self-edges), and the interior is computed
-- on that rewritten binding-parent relation.
--
-- The returned set contains canonical node ids.
interiorOfUnder
    :: (NodeId -> NodeId)
    -> Constraint p
    -> NodeRef
    -> Either BindingError IntSet
interiorOfUnder canonical c0 root0 =
    withQuotientBindParents "interiorOfUnder" canonical c0 root0 $ \rootC bindParents ->
        let childrenByParent :: IntMap.IntMap IntSet
            childrenByParent =
                IntMap.foldlWithKey'
                    (\m childRootKey (parentRoot, _flag) ->
                        insertChild (nodeRefKey parentRoot) childRootKey m
                    )
                    IntMap.empty
                    bindParents

        in pure (descendantsFromChildren childrenByParent (nodeRefKey rootC))

insertChild :: Int -> Int -> IntMap.IntMap IntSet -> IntMap.IntMap IntSet
insertChild !parentKey !childKey =
    IntMap.alter insertOrAppend parentKey
  where
    insertOrAppend Nothing = Just (IntSet.singleton childKey)
    insertOrAppend (Just children) = Just (IntSet.insert childKey children)

descendantsFromChildren :: IntMap.IntMap IntSet -> Int -> IntSet
descendantsFromChildren childrenByParent rootKey =
    go (IntSet.singleton rootKey) [rootKey]
  where
    go !visited [] = visited
    go !visited (key : rest) =
        let kids = IntMap.findWithDefault IntSet.empty key childrenByParent
            !newKids = IntSet.difference kids visited
            !visited' = IntSet.union visited newKids
            !rest' = IntSet.foldl' (flip (:)) rest newKids
        in go visited' rest'
