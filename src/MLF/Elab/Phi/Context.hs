{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Elab.Phi.Context
Description : Instantiation context computation for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module computes instantiation-context paths from a root node to a target
node, following the paper's definition of instantiation contexts (Figure 10):

  C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C

Contexts navigate only through quantifiers (under binders) and their bounds
(inside-bounds). This is used to translate graph witnesses to xMLF instantiations.

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - Figure 10
* Thesis §15.3 - Instantiation contexts
-}
module MLF.Elab.Phi.Context (
    contextToNodeBound,
    contextToNodeBoundWithOrderKeys
) where

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (elemIndex)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Elab.Types
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve (SolveResult(..), frWith)
import MLF.Reify.Core (namedNodes)

-- | Compute an instantiation-context path from a root node to a target node.
--
-- Paper reference: @papers/these-finale-english.txt@ (see @papers/xmlf.txt@
-- Figure 10) defines instantiation contexts:
--
--   C ::= {·} | ∀(⩾ C) | ∀(α ⩾) C
--
-- i.e. contexts navigate *only* through quantifiers (under binders) and their
-- bounds (inside-bounds). This helper computes the thesis computation context
-- by descending the type structure and choosing the leftmost binder-bound path
-- that contains @target@.
--
-- Returns 'Nothing' when @target@ is not transitively bound to @root@.
contextToNodeBound :: SolveResult -> NodeId -> NodeId -> Either ElabError (Maybe [ContextStep])
contextToNodeBound res root target = do
    let c = srConstraint res
        uf = srUnionFind res
        canonical = frWith uf
        rootC = canonical root
        targetC = canonical target

    if rootC == targetC
        then pure (Just [])
        else do
            let keys = Order.orderKeysFromRoot res rootC
            namedSet <- namedNodes res
            contextToNodeBoundWithOrderKeys canonical keys c namedSet rootC targetC

contextToNodeBoundWithOrderKeys
    :: (NodeId -> NodeId)
    -> IntMap.IntMap Order.OrderKey
    -> Constraint
    -> IntSet.IntSet
    -> NodeId
    -> NodeId
    -> Either ElabError (Maybe [ContextStep])
contextToNodeBoundWithOrderKeys canonical keys c _namedSet root target = do
    let rootC = canonical root
        targetC = canonical target

    if rootC == targetC
        then pure (Just [])
        else do
            let rootNode = NodeAccess.lookupNode c rootC
                needsInsideRoot =
                    case rootNode of
                        Just TyForall{} -> False
                        Just TyExp{} -> False
                        Just TyVar{} -> True
                        Just TyArrow{} -> True
                        Just TyBase{} -> True
                        Just TyBottom{} -> True
                        Nothing -> False
                start =
                    case rootNode of
                        Just TyVar{ tnBound = Just bnd } -> canonical bnd
                        Just TyVar{} -> rootC
                        _ -> rootC
            res <- snd <$> go IntSet.empty IntMap.empty start
            pure $
                if needsInsideRoot
                    then fmap (StepInside :) res
                    else res
  where
    nameFor nid = "t" ++ show (getNodeId nid)

    reachableFromStructural :: NodeId -> IntSet.IntSet
    reachableFromStructural root0 =
        Traversal.reachableFromUnderLenient canonical (NodeAccess.lookupNode c) root0

    dedupeById :: [NodeId] -> [NodeId]
    dedupeById =
        reverse . IntSet.foldl' (\acc i -> NodeId i : acc) [] . IntSet.fromList . map getNodeId

    orderedBindersAt :: NodeId -> Either ElabError [NodeId]
    orderedBindersAt binder0 = do
        let binder = canonical binder0
        binders0 <-
            bindingToElab (Binding.boundFlexChildrenUnder canonical c (typeRef binder))
        let orderRoot =
                case NodeAccess.lookupNode c binder of
                    Just TyForall{ tnBody = body } -> canonical body
                    _ -> binder
            reachable = reachableFromStructural orderRoot
            bindersReachable =
                filter (\nid -> IntSet.member (getNodeId nid) reachable) (dedupeById (map canonical binders0))
            missing =
                [ nid
                | nid <- bindersReachable
                , not (IntMap.member (getNodeId nid) keys)
                ]
        unless (null missing) $
            Left $
                InstantiationError $
                    "contextToNodeBound: missing order keys for " ++ show missing
        case Order.sortByOrderKey keys bindersReachable of
            Left err ->
                Left $
                    InstantiationError $
                        "contextToNodeBound: order key error: " ++ show err
            Right sorted -> pure sorted

    go
        :: IntSet.IntSet
        -> IntMap.IntMap (Maybe [ContextStep])
        -> NodeId
        -> Either ElabError (IntMap.IntMap (Maybe [ContextStep]), Maybe [ContextStep])
    go visiting memo nid0 = do
        let nid = canonical nid0
            key = getNodeId nid
            targetC = canonical target
        if nid == targetC
            then
                let res = Just []
                in Right (IntMap.insert key res memo, res)
            else case IntMap.lookup key memo of
                Just res -> Right (memo, res)
                Nothing ->
                    if IntSet.member key visiting
                        then
                            Left $
                                InstantiationError $
                                    "contextToNodeBound: cycle detected at " ++ show nid
                        else
                            case NodeAccess.lookupNode c nid of
                                Nothing -> Left (MissingNode nid)
                                Just node ->
                                    let visiting' = IntSet.insert key visiting
                                        finish res memo' =
                                            let memo'' = IntMap.insert key res memo'
                                            in Right (memo'', res)
                                    in case node of
                                        TyForall{ tnBody = body } -> do
                                            (memo', res) <- goForall visiting' memo nid body
                                            finish res memo'
                                        TyArrow{ tnDom = dom, tnCod = cod } -> do
                                            (memo', res) <- goChildren visiting' memo [dom, cod]
                                            finish res memo'
                                        TyExp{ tnBody = body } -> do
                                            (memo', res) <- go visiting' memo body
                                            finish res memo'
                                        TyVar{} ->
                                            finish Nothing memo
                                        TyBase{} ->
                                            finish Nothing memo
                                        TyBottom{} ->
                                            finish Nothing memo

    goChildren
        :: IntSet.IntSet
        -> IntMap.IntMap (Maybe [ContextStep])
        -> [NodeId]
        -> Either ElabError (IntMap.IntMap (Maybe [ContextStep]), Maybe [ContextStep])
    goChildren _ memo [] = Right (memo, Nothing)
    goChildren visiting memo (child:rest) = do
        (memo', res) <- go visiting memo child
        case res of
            Just _ -> pure (memo', res)
            Nothing -> goChildren visiting memo' rest

    goForall
        :: IntSet.IntSet
        -> IntMap.IntMap (Maybe [ContextStep])
        -> NodeId
        -> NodeId
        -> Either ElabError (IntMap.IntMap (Maybe [ContextStep]), Maybe [ContextStep])
    goForall visiting memo forallId _body0 = do
        binders <- orderedBindersAt forallId
        let targetC = canonical target
        case elemIndex targetC binders of
            Just i -> do
                let before = take i binders
                    steps = map (StepUnder . nameFor) before
                pure (memo, Just steps)
            Nothing -> do
                let tryBound memoAcc [] = Right (memoAcc, Nothing)
                    tryBound memoAcc (b : bs) =
                        case NodeAccess.lookupNode c b of
                            Just TyVar{ tnBound = Just bnd } -> do
                                let bndC = canonical bnd
                                (memo', res) <- go visiting memoAcc bndC
                                case res of
                                    Just ctx ->
                                        let before = takeWhile (/= b) binders
                                            steps = map (StepUnder . nameFor) before ++ [StepInside] ++ ctx
                                        in pure (memo', Just steps)
                                    Nothing -> tryBound memo' bs
                            Just TyVar{} -> tryBound memoAcc bs
                            Just _ -> tryBound memoAcc bs
                            Nothing -> tryBound memoAcc bs
                (memo', boundRes) <- tryBound memo binders
                case boundRes of
                    Just _ -> pure (memo', boundRes)
                    Nothing -> pure (memo', Nothing)
