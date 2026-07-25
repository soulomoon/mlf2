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
    -- | Every raw parent assignment, grouped by canonical child and with its
    -- parent canonicalized.  Unlike 'qbpBindParents', this does not collapse
    -- alias entries or their flags.  Construction-time conflict checks can
    -- therefore reuse the quotient snapshot without rescanning the complete
    -- raw binding-parent map or losing contradictory alias evidence.
    , qbpRawParentAssignments :: !(IntMap [(NodeRef, BindFlag)])
    }
    deriving (Eq, Show)

-- | Canonicalize the binding-parent relation under a canonicalization function.
--
-- This drops self-edges (where child and parent canonicalize to the same node),
-- and merges duplicate alias edges deterministically while taking the max flag.
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
-- self-edges) and merging duplicate aliases deterministically.
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
    (allRoots, bindParents, _rawParentAssignments) <-
        quotientBindParentsDetailsUnder False canonical c0
    pure (allRoots, bindParents)

quotientBindParentsDetailsUnder
    :: Bool
    -> (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError (IntSet, BindParents, IntMap [(NodeRef, BindFlag)])
quotientBindParentsDetailsUnder retainRawAssignments canonical c0 = do
    let bindParents0 = cBindParents c0

        allRoots :: IntSet
        allRoots =
            IntMap.foldlWithKey'
                (\acc k _ -> IntSet.insert (typeRefKey (canonical (NodeId k))) acc)
                (IntMap.foldlWithKey'
                    (\acc k _ -> IntSet.insert (genRefKey (GenNodeId k)) acc)
                    IntSet.empty
                    (getGenNodeMap (cGenNodes c0)))
                (getNodeMap (cNodes c0))

        -- Canonicalize each raw edge once, retaining the exact assignments for
        -- construction-time validation while also building the collapsed
        -- quotient relation.
        -- Union-find canonicalization can transiently create multiple
        -- binding parents for the same canonical node. We resolve this
        -- deterministically by keeping the first parent we saw and
        -- taking the max flag.
        (rawParentAssignments, bindParents) =
            IntMap.foldlWithKey'
                (\(rawAssignments, bp) childKey (parent0, flag) ->
                    let childRootKey = Canonicalize.canonicalRefKey canonical (nodeRefFromKey childKey)
                        parentRoot = Canonicalize.canonicalRef canonical parent0
                        parentRootKey = nodeRefKey parentRoot
                        rawAssignments'
                            | retainRawAssignments =
                                IntMap.insertWith
                                    (++)
                                    childRootKey
                                    [(parentRoot, flag)]
                                    rawAssignments
                            | otherwise = rawAssignments
                    in if childRootKey == parentRootKey
                          || not (IntSet.member childRootKey allRoots)
                          || not (IntSet.member parentRootKey allRoots)
                        then (rawAssignments', bp)
                        else
                            ( rawAssignments'
                            , IntMap.insertWith
                                (\(_, flagNew) (parentOld, flagOld) -> (parentOld, max flagOld flagNew))
                                childRootKey
                                (parentRoot, flag)
                                bp
                            )
                )
                (IntMap.empty, IntMap.empty)
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

    pure (allRoots, bindParents, rawParentAssignments)

quotientBindParentsContextUnder
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError QuotientBindParents
quotientBindParentsContextUnder canonical c0 = do
    (allRoots, bindParents, rawParentAssignments) <-
        quotientBindParentsDetailsUnder True canonical c0
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
            , qbpRawParentAssignments = rawParentAssignments
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
