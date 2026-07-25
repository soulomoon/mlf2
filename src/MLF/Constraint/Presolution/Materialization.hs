{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Materialization
Description : Expansion materialization for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module handles the materialization of expansions during presolution.
After the edge processing loop decides minimal expansions for each expansion
variable, this module applies those expansions to TyExp nodes and records
their replacements.

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - §5 "Presolution"
-}
module MLF.Constraint.Presolution.Materialization (
    materializeExpansions,
    frWith
) where

import Control.Monad (foldM, forM)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (gets, modify')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Util.UnionFind as UnionFind
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.Expansion (getExpansion)

-- | Apply final expansions to all TyExp nodes and record their replacements.
materializeExpansions :: PresolutionM p (IntMap NodeId)
materializeExpansions = do
    (c0, canonical) <- getConstraintAndCanonical
    expansionResults0 <- gets psExpansionResults
    expansionResults <-
        either throwError pure $
            canonicalizeExpansionResultMap canonical expansionResults0
    let exps =
            [ (expNode, eid, expVar, expBody)
            | (_, expNode@TyExp { tnId = eid, tnExpVar = expVar, tnBody = expBody }) <- toListNode (cNodes c0)
            ]
    redirects <- forM exps $ \(_expNode, eid, expVar, expBody) -> do
        expn <- getExpansion expVar
        nid' <- case expn of
            -- Identity expansions are erased by rewriting the wrapper to its body.
            ExpIdentity -> do
                alignIdentityExpansionBinding canonical eid expBody
                pure expBody
            -- For non-identity expansions, `processInstEdge` should already have
            -- materialized and unified the expansion result with the target.
            -- The occurrence-site TyExp wrapper is administrative, so its
            -- replacement is recorded outside semantic union-find.  Reuse that
            -- destination-scoped result here; reapplying at the source scheme
            -- would duplicate χe and give its arguments the wrong owner.
            _ -> do
                case lookupExpansionResult (canonical eid) expansionResults of
                    Just result -> pure (canonical result)
                    Nothing -> throwError (MissingExpansionResult eid expVar)
        pure (getNodeId eid, nid')
    foldM (insertRedirect canonical) IntMap.empty redirects
  where
    insertRedirect
        :: (NodeId -> NodeId)
        -> IntMap NodeId
        -> (Int, NodeId)
        -> PresolutionM q (IntMap NodeId)
    insertRedirect canonical acc (wrapperKey, result) = do
        let wrapper = NodeId wrapperKey
            wrapperClassKey = getNodeId (canonical wrapper)
        acc' <- insertOne wrapper result acc
        insertOne (NodeId wrapperClassKey) result acc'

    insertOne
        :: NodeId
        -> NodeId
        -> IntMap NodeId
        -> PresolutionM q (IntMap NodeId)
    insertOne wrapper result acc =
        let key = getNodeId wrapper
        in case IntMap.lookup key acc of
            Nothing -> pure (IntMap.insert key result acc)
            Just existing
                | existing == result -> pure acc
                | otherwise ->
                    throwError (ExpansionResultConflict wrapper existing result)

-- | Make an erased identity wrapper carry the live binding edge of its body.
--
-- Rewriting is a directed collapse from the wrapper to the body.  The wrapper's
-- use-site edge therefore must not become a competing owner for the body after
-- canonicalization: doing so can pull a result variable back inside the use
-- site after edge unification has already raised it to its principal scope.
alignIdentityExpansionBinding
    :: (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> PresolutionM p ()
alignIdentityExpansionBinding canonical wrapper body = do
    c <- getConstraint
    case identityBodyOwner canonical c wrapper body of
        Just parentInfo ->
            modify' (setBindParentState (typeRef wrapper) parentInfo)
        Nothing ->
            throwError $
                InternalError $
                    "identity expansion body has no recoverable owner: "
                        ++ show (wrapper, body)

-- | Select the body edge that survives erasing an identity wrapper.
--
-- The raw body is semantic provenance that remains available even when
-- union-find chooses the wrapper as the class representative, so its external
-- owner wins before any representative lookup.  A representative lookup is
-- safe only while the body and wrapper remain in distinct classes; otherwise
-- it can observe the wrapper's use-site edge as if it belonged to the body.
--
-- There is one constructive fallback.  In the original binding tree a normal
-- identity wrapper owns its body directly.  Erasing @wrapper -> body@ then
-- contracts @body -> wrapper -> parent@ to @body -> parent@, so the wrapper's
-- raw external parent is the body's owner by structural provenance.  Merely
-- reaching the wrapper class through some third UF member does not establish
-- that provenance and fails closed.
identityBodyOwner
    :: (NodeId -> NodeId)
    -> Constraint p
    -> NodeId
    -> NodeId
    -> Maybe (NodeRef, BindFlag)
identityBodyOwner canonical c wrapper body =
    case rawBodyOwner of
        Just parentInfo -> Just parentInfo
        Nothing ->
            case canonicalBodyOwner of
                Just parentInfo -> Just parentInfo
                Nothing -> directWrapperOwner
  where
    wrapperRoot = canonical wrapper
    bodyRoot = canonical body
    rawBodyParent = Binding.lookupBindParent c (typeRef body)

    rawBodyOwner = rawBodyParent >>= outsideWrapperClass

    canonicalBodyOwner
        | bodyRoot == wrapperRoot = Nothing
        | bodyRoot == body = Nothing
        | otherwise = externalOwnerOf bodyRoot

    externalOwnerOf node =
        Binding.lookupBindParent c (typeRef node) >>= outsideWrapperClass

    directWrapperOwner =
        case (NodeAccess.lookupNode c wrapper, rawBodyParent) of
            ( Just TyExp {tnBody = directBody}
                , Just (TypeRef directParent, _)
                )
                | directBody == body
                , directParent == wrapper -> externalOwnerOf wrapper
            _ -> Nothing

    outsideWrapperClass parentInfo@(parent, _) =
        case parent of
            TypeRef parentNode
                | canonical parentNode == wrapperRoot -> Nothing
            _ -> Just parentInfo

-- | Read-only chase like Solve.frWith
frWith :: IntMap NodeId -> NodeId -> NodeId
frWith = UnionFind.frWith
