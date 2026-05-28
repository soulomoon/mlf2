{- |
Module      : MLF.Binding.Canonicalization
Description : Binding tree canonicalization under union-find
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides functions for canonicalizing binding-parent relations
under a union-find canonicalization function. This is useful when unification
maintains a union-find over nodes: after a merge, the raw `cBindParents`
relation may contain edges for multiple aliases of the same canonical
representative.

Extracted from MLF.Binding.Tree for modularity.
-}
module MLF.Binding.Canonicalization (
    -- * Canonicalization
    QuotientBindParents(..),
    canonicalizeBindParentsUnder,
    quotientBindParentsUnder,
    quotientBindParentsContextUnder,
    quotientChildrenForParent,
    -- * Helpers
    withQuotientBindParents,
    withQuotientBindParentsContext,
) where

import Control.Monad (forM_, unless)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Types.Graph

data QuotientBindParents = QuotientBindParents
    { qbpAllRoots :: !IntSet
    , qbpBindParents :: !BindParents
    , qbpChildrenByParent :: !(IntMap [(Int, (NodeRef, BindFlag))])
    }
    deriving (Eq, Show)

-- | Canonicalize the binding-parent relation under a canonicalization function.
--
-- This drops self-edges (where child and parent canonicalize to the same node),
-- merges duplicate edges by requiring the same parent and taking the max flag,
-- and rejects conflicting parents on the same canonical node.
--
-- This is useful when unification maintains a union-find over nodes: after a
-- merge, the raw `cBindParents` relation may contain edges for multiple aliases
-- of the same canonical representative. Canonicalizing makes subsequent binding
-- operations (like LCA and Raise) well-defined on representatives.
canonicalizeBindParentsUnder
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError BindParents
canonicalizeBindParentsUnder canonical c0 = do
    (_allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    pure bindParents

-- | Rewrite `cBindParents` to a canonicalized binding-parent relation (dropping
-- self-edges), merging duplicates by requiring the same parent and taking the
-- max flag.
--
-- This is the shared core used by 'checkBindingTreeUnder', 'interiorOfUnder',
-- and other quotient-aware binding-tree queries.
--
-- Returns the set of all canonical node keys and the canonicalized bind parents.
quotientBindParentsUnder
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError (IntSet, BindParents)
quotientBindParentsUnder canonical c0 = do
    let bindParents0 = cBindParents c0

        allRoots :: IntSet
        allRoots =
            IntSet.fromList
                [ typeRefKey (canonical (NodeId k))
                | k <- IntMap.keys (getNodeMap (cNodes c0))
                ]
                `IntSet.union` IntSet.fromList
                [ genRefKey (GenNodeId k)
                | k <- IntMap.keys (getGenNodeMap (cGenNodes c0))
                ]

        -- Fuse entries0 + foldME: canonicalize, filter, and insert in one pass.
        -- Union-find canonicalization can transiently create multiple
        -- binding parents for the same canonical node. We resolve this
        -- deterministically by keeping the first parent we saw and
        -- taking the max flag.
        bindParents =
            IntMap.foldlWithKey'
                (\bp childKey (parent0, flag) ->
                    let childRootKey = Canonicalize.canonicalRefKey canonical (nodeRefFromKey childKey)
                        parentRootKey = Canonicalize.canonicalRefKey canonical parent0
                    in if childRootKey == parentRootKey
                          || not (IntSet.member childRootKey allRoots)
                          || not (IntSet.member parentRootKey allRoots)
                        then bp
                        else
                            let parentRoot = Canonicalize.canonicalRef canonical parent0
                            in case IntMap.lookup childRootKey bp of
                                Nothing -> IntMap.insert childRootKey (parentRoot, flag) bp
                                Just (parentOld, flagOld) ->
                                    IntMap.insert childRootKey (parentOld, max flagOld flag) bp
                )
                IntMap.empty
                bindParents0

    -- Sanity: rewritten nodes must correspond to canonical reps of live nodes.
    forM_ (IntMap.keys bindParents) $ \childRootKey ->
        unless (IntSet.member childRootKey allRoots) $
            Left $
                InvalidBindingTree $
                    "quotientBindParentsUnder: binding child " ++ show childRootKey ++ " not in constraint"

    forM_ (IntMap.toList bindParents) $ \(childRootKey, (parentRoot, _flag)) -> do
        let parentRootKey = nodeRefKey parentRoot
        unless (IntSet.member parentRootKey allRoots) $
            Left $
                InvalidBindingTree $
                    "quotientBindParentsUnder: binding parent " ++ show parentRootKey
                        ++ " of node " ++ show childRootKey ++ " not in constraint"

    pure (allRoots, bindParents)

quotientBindParentsContextUnder
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError QuotientBindParents
quotientBindParentsContextUnder canonical c0 = do
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    let childrenByParent =
            IntMap.map reverse $
                IntMap.foldlWithKey'
                    (\m childKey info@(parentRoot, _flag) ->
                        IntMap.insertWith (++) (nodeRefKey parentRoot) [(childKey, info)] m
                    )
                    IntMap.empty
                    bindParents
    pure
        QuotientBindParents
            { qbpAllRoots = allRoots
            , qbpBindParents = bindParents
            , qbpChildrenByParent = childrenByParent
            }

quotientChildrenForParent :: NodeRef -> QuotientBindParents -> [(Int, (NodeRef, BindFlag))]
quotientChildrenForParent parent qbp =
    IntMap.findWithDefault [] (nodeRefKey parent) (qbpChildrenByParent qbp)

-- | Helper: run an action with canonicalized binding parents and validated node.
--
-- This is the shared core for quotient-aware binding-tree operations.
-- It canonicalizes the node, computes the quotient bind parents, validates
-- that the canonical node exists, and runs the given action.
withQuotientBindParents
    :: String  -- ^ Error context for validation failures
    -> (NodeId -> NodeId)
    -> Constraint p
    -> NodeRef
    -> (NodeRef -> BindParents -> Either BindingError a)
    -> Either BindingError a
withQuotientBindParents errCtx canonical c0 ref0 f = do
    let refC = Canonicalize.canonicalRef canonical ref0
    (allRoots, bindParents) <- quotientBindParentsUnder canonical c0
    unless (IntSet.member (nodeRefKey refC) allRoots) $
        Left $ InvalidBindingTree $ errCtx ++ ": node " ++ show refC ++ " not in constraint"
    f refC bindParents

withQuotientBindParentsContext
    :: String  -- ^ Error context for validation failures
    -> (NodeId -> NodeId)
    -> Constraint p
    -> NodeRef
    -> (NodeRef -> QuotientBindParents -> Either BindingError a)
    -> Either BindingError a
withQuotientBindParentsContext errCtx canonical c0 ref0 f = do
    let refC = Canonicalize.canonicalRef canonical ref0
    qbp <- quotientBindParentsContextUnder canonical c0
    unless (IntSet.member (nodeRefKey refC) (qbpAllRoots qbp)) $
        Left $ InvalidBindingTree $ errCtx ++ ": node " ++ show refC ++ " not in constraint"
    f refC qbp
