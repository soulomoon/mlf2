{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Rewrite
Description : Constraint rewriting helpers for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module provides helper functions for rewriting constraints during
the presolution phase. Extracted from Driver.hs for maintainability.
-}
module MLF.Constraint.Presolution.Rewrite (
    -- * Canonicalization
    canonicalizeExpansion,
    canonicalizeOp,
    canonicalizeStep,
    canonicalizeWitness,
    canonicalizeTrace,
    -- * Bind parent reconstruction
    RebuildBindParentsEnv(..),
    rebuildBindParents,
    -- * Node rewriting
    rewriteNode,
    rewriteVarSet,
    rewriteGenNodes,
) where

import Control.Monad (forM, foldM)
import Control.Monad.Except (throwError)
import Data.Functor.Foldable (cata)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (CopyMapping(..), EdgeTrace(..), PresolutionError(..), PresolutionM, fromListInterior, toListInterior)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Util.Trace (debugBinding)

-- | Canonicalize an expansion through a node ID mapping.
canonicalizeExpansion :: (NodeId -> NodeId) -> Expansion -> Expansion
canonicalizeExpansion canonical = cata alg
  where
    alg layer = case layer of
        ExpIdentityF -> ExpIdentity
        ExpInstantiateF args -> ExpInstantiate (map canonical args)
        ExpForallF levels -> ExpForall levels
        ExpComposeF exps -> ExpCompose exps

-- | Canonicalize an instance operation.
canonicalizeOp :: (NodeId -> NodeId) -> InstanceOp -> InstanceOp
canonicalizeOp canonical = \case
    OpGraft sigma n -> OpGraft (canonical sigma) (canonical n)
    OpMerge a b -> OpMerge (canonical a) (canonical b)
    OpRaise n -> OpRaise (canonical n)
    OpWeaken n -> OpWeaken (canonical n)
    OpRaiseMerge n m -> OpRaiseMerge (canonical n) (canonical m)

-- | Canonicalize an instance step.
canonicalizeStep :: (NodeId -> NodeId) -> InstanceStep -> InstanceStep
canonicalizeStep canonical = \case
    StepOmega op -> StepOmega (canonicalizeOp canonical op)
    StepIntro -> StepIntro

-- | Canonicalize an edge witness.
canonicalizeWitness :: (NodeId -> NodeId) -> EdgeWitness -> EdgeWitness
canonicalizeWitness canonical w =
    let InstanceWitness ops = ewWitness w
    in w
        { ewLeft = canonical (ewLeft w)
        , ewRight = canonical (ewRight w)
        , ewRoot = canonical (ewRoot w)
        , ewSteps = map (canonicalizeStep canonical) (ewSteps w)
        , ewWitness = InstanceWitness (map (canonicalizeOp canonical) ops)
        }

-- | Canonicalize an edge trace.
canonicalizeTrace :: (NodeId -> NodeId) -> EdgeTrace -> EdgeTrace
canonicalizeTrace canonical tr =
    let canonPair (a, b) = (canonical a, canonical b)
        canonInterior =
            fromListInterior
                [ canonical i
                | i <- toListInterior (etInterior tr)
                ]
        canonCopyMap =
            CopyMapping $
                IntMap.fromListWith min
                    [ ( getNodeId (canonical (NodeId k))
                      , canonical v
                      )
                    | (k, v) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                    ]
    in tr
        { etRoot = canonical (etRoot tr)
        , etBinderArgs = map canonPair (etBinderArgs tr)
        , etInterior = canonInterior
        , etCopyMap = canonCopyMap
        }

-- | Rewrite a node through canonicalization.
-- Returns Nothing for TyExp nodes (which are erased).
rewriteNode :: (NodeId -> NodeId) -> TyNode -> Maybe (Int, TyNode)
rewriteNode canonical = \case
    TyExp{} -> Nothing
    TyVar { tnId = nid, tnBound = mb } ->
        let nid' = canonical nid
        in Just (getNodeId nid', TyVar { tnId = nid', tnBound = fmap canonical mb })
    TyBottom { tnId = nid } ->
        let nid' = canonical nid
        in Just (getNodeId nid', TyBottom nid')
    TyArrow { tnId = nid, tnDom = d, tnCod = cod } ->
        let nid' = canonical nid
        in Just (getNodeId nid', TyArrow nid' (canonical d) (canonical cod))
    TyBase { tnId = nid, tnBase = b } ->
        let nid' = canonical nid
        in Just (getNodeId nid', TyBase nid' b)
    TyForall { tnId = nid, tnBody = b } ->
        let nid' = canonical nid
        in Just (getNodeId nid', TyForall nid' (canonical b))

-- | Rewrite a set of variable IDs through canonicalization, keeping only those
-- that still exist as TyVar nodes in the rewritten node map.
rewriteVarSet :: (NodeId -> NodeId) -> IntMap TyNode -> IntSet.IntSet -> IntSet.IntSet
rewriteVarSet canonical nodes0 vars0 =
    IntSet.fromList
        [ getNodeId vC
        | vid <- IntSet.toList vars0
        , let vC = canonical (NodeId vid)
        , case IntMap.lookup (getNodeId vC) nodes0 of
            Just TyVar{} -> True
            _ -> False
        ]

-- | Rewrite gen nodes through canonicalization.
rewriteGenNodes :: (NodeId -> NodeId) -> IntMap TyNode -> GenNodeMap GenNode -> GenNodeMap GenNode
rewriteGenNodes canonical nodes0 gen0 =
    let rewriteOne g =
            let (schemesRev, _seen) =
                    foldl'
                        (\(acc, seen) s ->
                            let s' = canonical s
                                key = getNodeId s'
                            in if IntMap.member key nodes0 && not (IntSet.member key seen)
                                then (s' : acc, IntSet.insert key seen)
                                else (acc, seen)
                        )
                        ([], IntSet.empty)
                        (gnSchemes g)
                schemes' = reverse schemesRev
            in (genNodeKey (gnId g), g { gnSchemes = schemes' })
    in GenNodeMap
        (IntMap.fromListWith const (map rewriteOne (IntMap.elems (getGenNodeMap gen0))))

-- | Environment for rebuilding bind parents.
data RebuildBindParentsEnv = RebuildBindParentsEnv
    { rbpOriginalConstraint :: !Constraint
      -- ^ The original constraint before rewriting
    , rbpNewNodes :: !(IntMap TyNode)
      -- ^ The rewritten nodes (TyExp removed)
    , rbpGenNodes :: !(GenNodeMap GenNode)
      -- ^ The rewritten gen nodes
    , rbpCanonical :: !(NodeId -> NodeId)
      -- ^ Canonicalization function
    , rbpIncomingParents :: !(IntMap IntSet.IntSet)
      -- ^ Map from child node ID to set of structural parent IDs
    }

-- | Rebuild binding parents after constraint rewriting.
--
-- This is the most complex part of constraint rewriting. It must:
-- 1. Canonicalize all binding edges
-- 2. Resolve conflicts when multiple parents map to the same child
-- 3. Ensure all nodes have valid parents in the rewritten graph
-- 4. Fix any parent references that violate the binding tree invariant
rebuildBindParents :: RebuildBindParentsEnv -> PresolutionM BindParents
rebuildBindParents env = do
    let c = rbpOriginalConstraint env
        newNodes = rbpNewNodes env
        genNodes' = rbpGenNodes env
        canonical = rbpCanonical env
        incomingParents = rbpIncomingParents env
        bindingEdges0 = cBindParents c
        cStruct = c { cNodes = NodeMap newNodes, cGenNodes = genNodes' }

        rootGenRef =
            case IntMap.keys (getGenNodeMap genNodes') of
                [] -> Nothing
                gids -> Just (genRef (GenNodeId (minimum gids)))

        genExists gid =
            IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
        typeExists nid = IntMap.member (getNodeId nid) newNodes

        -- Build scheme parent map: child -> gen parent
        schemeParents =
            IntMap.fromListWith const
                [ (getNodeId child, genRef gid)
                | (childKey, (parent0, _flag)) <- IntMap.toList bindingEdges0
                , let childRef0 = nodeRefFromKey childKey
                      childRef = canonicalRef childRef0
                      parentRef = canonicalRef parent0
                , TypeRef child <- [childRef]
                , GenRef gid <- [parentRef]
                ]

        canonicalRef = Canonicalize.canonicalRef canonical

        -- Initial entries from original binding edges
        entries0 =
            [ (childRef, parentRef, flag)
            | (childKey, (parent0, flag)) <- IntMap.toList bindingEdges0
            , let childRef0 = nodeRefFromKey childKey
                  childRef = canonicalRef childRef0
                  parentRef = canonicalRef parent0
            , case childRef of
                TypeRef nid -> typeExists nid
                GenRef gid -> genExists gid
            ]

    -- Choose valid parent for each child
    let chooseBindParent :: NodeRef -> NodeRef -> PresolutionM NodeRef
        chooseBindParent childRef parent0 =
            case childRef of
                GenRef _ ->
                    case parent0 of
                        GenRef gid
                            | genExists gid -> pure parent0
                        _ ->
                            throwError $
                                BindingTreeError $
                                    InvalidBindingTree $
                                        "rebuildBindParents: gen node has non-gen parent "
                                            ++ show parent0
                TypeRef child' -> do
                    let inNodes ref = case ref of
                            TypeRef nid -> typeExists nid
                            GenRef gid -> genExists gid
                        upper parent = case parent of
                            GenRef _ -> True
                            _ -> Binding.isUpper cStruct parent childRef

                        bindingAncestors :: NodeRef -> [NodeRef]
                        bindingAncestors start =
                            case Binding.bindingPathToRoot c start of
                                Left _ -> []
                                Right path -> drop 1 path

                        expBody :: NodeRef -> Maybe NodeRef
                        expBody ref =
                            case ref of
                                TypeRef nid ->
                                    case NodeAccess.lookupNode c nid of
                                        Just TyExp{ tnBody = b } -> Just (TypeRef b)
                                        _ -> Nothing
                                GenRef _ -> Nothing

                        structuralParent :: NodeRef -> Maybe NodeRef
                        structuralParent ref =
                            case ref of
                                GenRef _ -> Nothing
                                TypeRef nid ->
                                    case IntMap.lookup (getNodeId nid) incomingParents of
                                        Nothing -> Nothing
                                        Just ps ->
                                            case IntSet.toList ps of
                                                [] -> Nothing
                                                (p:_) -> Just (TypeRef (NodeId p))

                        candidates =
                            map canonicalRef $
                                [ parent0 ]
                                    ++ maybe [] pure (IntMap.lookup (getNodeId child') schemeParents)
                                    ++ mapMaybe expBody [parent0]
                                    ++ bindingAncestors parent0
                                    ++ bindingAncestors childRef
                                    ++ maybe [] pure (structuralParent childRef)

                        firstValid = \case
                            [] -> Nothing
                            (p:ps) ->
                                if p == childRef || not (inNodes p) || not (upper p)
                                    then firstValid ps
                                    else Just p

                    case firstValid candidates of
                        Just p -> pure p
                        Nothing ->
                            case rootGenRef of
                                Just gref
                                    | gref /= childRef
                                    , inNodes gref
                                    , upper gref ->
                                        pure gref
                                _ ->
                                    throwError $
                                        BindingTreeError $
                                            InvalidBindingTree $
                                                "rebuildBindParents: could not find a valid binding parent for "
                                                    ++ show child'
                                                    ++ " (original parent "
                                                    ++ show parent0
                                                    ++ ")"

    let insertOne :: BindParents -> (Int, (NodeRef, BindFlag)) -> PresolutionM BindParents
        insertOne bp (childKey, (parent, flag)) =
            case IntMap.lookup childKey bp of
                Nothing -> pure (IntMap.insert childKey (parent, flag) bp)
                Just (parent0, flag0)
                    | parent0 == parent ->
                        let flag' = max flag0 flag
                        in pure (IntMap.insert childKey (parent, flag') bp)
                    | otherwise ->
                        let flag' = max flag0 flag
                            childRef = nodeRefFromKey childKey
                            schemeParent =
                                case childRef of
                                    TypeRef nid -> IntMap.lookup (getNodeId nid) schemeParents
                                    GenRef _ -> Nothing
                            pickParent0 =
                                case schemeParent of
                                    Just sp -> sp
                                    Nothing -> parent0
                            pickParent =
                                if Binding.isUpper cStruct pickParent0 childRef
                                    then pickParent0
                                    else parent
                            bp' = IntMap.insert childKey (pickParent, flag') bp
                            msg =
                                "presolution: bind-parent conflict child="
                                    ++ show childRef
                                    ++ " parent0="
                                    ++ show parent0
                                    ++ " parent1="
                                    ++ show parent
                                    ++ " schemeParent="
                                    ++ show schemeParent
                                    ++ " pick="
                                    ++ show pickParent
                        in pure (debugBindParents msg bp')

    entries' <- fmap concat $ forM entries0 $ \(childRef, parent0, flag) -> do
        parent <- chooseBindParent childRef parent0
        pure $ case parent == childRef of
            True -> []
            False -> [(nodeRefKey childRef, (parent, flag))]

    bp0 <- foldM insertOne IntMap.empty entries'

    let inNodesRef ref =
            case ref of
                TypeRef nid -> typeExists nid
                GenRef gid -> genExists gid
        bp0' =
            IntMap.filterWithKey
                (\childKey (parent, _) ->
                    inNodesRef (nodeRefFromKey childKey) && inNodesRef parent
                )
                bp0
        rootGen =
            case IntMap.keys (getGenNodeMap genNodes') of
                [] -> Nothing
                gids -> Just (GenNodeId (minimum gids))

        addMissing bp nidInt = do
            let childRef = typeRef (NodeId nidInt)
                childKey = nodeRefKey childRef
            if IntMap.member childKey bp
                then pure bp
                else do
                    let structuralParent =
                            case IntMap.lookup nidInt incomingParents of
                                Nothing -> Nothing
                                Just ps ->
                                    case IntSet.toList ps of
                                        [] -> Nothing
                                        (p:_) -> Just (typeRef (NodeId p))
                        parent =
                            case IntMap.lookup nidInt schemeParents of
                                Just gp -> Just gp
                                Nothing -> structuralParent
                        parent' =
                            case parent of
                                Just p -> Just p
                                Nothing ->
                                    case rootGen of
                                        Nothing -> Nothing
                                        Just gid -> Just (genRef gid)
                    case parent' of
                        Nothing -> pure bp
                        Just p ->
                            if p == childRef
                                then pure bp
                                else pure (IntMap.insert childKey (p, BindFlex) bp)

    bp1 <- foldM addMissing bp0' (IntMap.keys newNodes)

    let rootGenRefLocal = fmap genRef rootGen
        pickUpperParent childN =
            case IntMap.lookup (getNodeId childN) incomingParents of
                Just ps ->
                    case IntSet.toList ps of
                        (p:_) -> Just (typeRef (NodeId p))
                        [] -> rootGenRefLocal
                Nothing -> rootGenRefLocal
        fixUpper bp =
            IntMap.mapWithKey
                (\childKey (parentRef, flag) ->
                    case nodeRefFromKey childKey of
                        GenRef _ -> (parentRef, flag)
                        TypeRef childN ->
                            case parentRef of
                                GenRef _ -> (parentRef, flag)
                                _ ->
                                    if Binding.isUpper cStruct parentRef (typeRef childN)
                                        then (parentRef, flag)
                                        else
                                            case pickUpperParent childN of
                                                Just pRef ->
                                                    if pRef == typeRef childN
                                                        then
                                                            case rootGenRefLocal of
                                                                Just gref -> (gref, flag)
                                                                Nothing -> (parentRef, flag)
                                                        else (pRef, flag)
                                                Nothing -> (parentRef, flag)
                )
                bp
    pure (fixUpper bp1)

-- | Debug binding operations (uses global trace config).
debugBindParents :: String -> a -> a
debugBindParents = debugBinding
