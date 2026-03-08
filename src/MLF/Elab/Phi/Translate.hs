{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{- |
Module      : MLF.Elab.Phi.Translate
Description : Translate graph witnesses to xMLF instantiations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module translates recorded per-edge graph witnesses to xMLF instantiation
witnesses (φ). It interprets witness operations (Graft, Weaken, Raise, Merge,
RaiseMerge) and produces explicit instantiation terms.

= Architecture

The translation has two main phases:
1. Context computation - compute instantiation-context paths (see "MLF.Elab.Phi.Context")
2. Witness interpretation - interpret witness operations to build instantiation terms

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - Figure 10
* Thesis §15.3 - Witness translation

= Note on Module Structure

This module contains a large function 'phiFromEdgeWitnessWithTrace' with many
local helper functions that share a complex closure. The Omega/Step interpretation
helpers live in "MLF.Elab.Phi.Omega"; the "MLF.Elab.Phi" module re-exports
the public entry points as a facade.
-}
module MLF.Elab.Phi.Translate (
    -- * Translation entry point (requires trace)
    phiFromEdgeWitnessWithTrace,
    canonicalNodeM
) where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types
import MLF.Constraint.Types.Witness (ReplayContract(..), isStrictReplayContract)
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Reify.Core
    ( namedNodes
    , reifyBoundWithNames
    , reifyType
    , reifyTypeWithNamedSetNoFallback
    )
import MLF.Constraint.Presolution (EdgeTrace(..), PresolutionView(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), InteriorNodes(..), copiedNodes)
import qualified MLF.Binding.Tree as Binding
import MLF.Binding.Tree (checkBindingTree, checkNoGenFallback, checkSchemeClosureUnder)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Phi.Env (PhiM, askCanonical)
import MLF.Elab.Phi.Omega (OmegaContext(..), phiWithSchemeOmega)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Elab.Run.Scope (schemeBodyTarget)

-- | Canonicalize a node id using the union-find from the solve result.
canonicalNodeM :: NodeId -> PhiM NodeId
canonicalNodeM nid = do
    canonicalNode <- askCanonical
    pure (canonicalNode nid)

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
type GeneralizeAtWith =
    Maybe GaBindParents
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

phiFromEdgeWitnessWithTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> PresolutionView
    -> Maybe GaBindParents
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith presolutionView mbGaParents mSchemeInfo mTrace ew =
    case mTrace of
        Nothing -> Left (MissingEdgeTrace (ewEdgeId ew))
        Just _ ->
            phiFromEdgeWitnessCore traceCfg generalizeAtWith presolutionView mbGaParents mSchemeInfo mTrace ew

{- Note [Trace-First Copied Set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The copied-node set is built from EdgeTrace.etCopyMap (witness domain),
then canonicalized for alias reconciliation only. The copy map is the
authoritative source of which nodes were copied during presolution
expansion (thesis §10.3). Canonical chasing here reconciles IDs that
were merged during solving but does not introduce new semantic content.
-}

phiFromEdgeWitnessCore
    :: TraceConfig
    -> GeneralizeAtWith
    -> PresolutionView
    -> Maybe GaBindParents
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessCore traceCfg generalizeAtWith presolutionView mbGaParents mSchemeInfo mTrace ew = do
    requireValidBindingTree
    namedSet0 <- namedNodes presolutionView
    case debugPhi
        ("phi ewLeft=" ++ show (ewLeft ew)
            ++ " ewRight=" ++ show (ewRight ew)
        )
        () of
        () -> pure ()
    case debugPhi
        ("phi ewRootType=" ++ show (reifyDebugType (ewRoot ew))
            ++ " ewLeftType=" ++ show (reifyDebugType (ewLeft ew))
            ++ " ewRightType=" ++ show (reifyDebugType (ewRight ew))
        )
        () of
        () -> pure ()
    -- See Note [Trace-First Copied Set]
    let copied =
            case mTrace of
                Nothing -> IntSet.empty
                Just tr ->
                    IntSet.fromList
                        [ getNodeId (canonicalNode nid)
                        | nid <- copiedNodes (etCopyMap tr)
                        ]
        interior =
            case mTrace of
                Nothing -> IntSet.empty
                Just tr ->
                    case etInterior tr of
                        InteriorNodes s -> s
        namedSet1 = IntSet.difference namedSet0 copied
        rootKey = getNodeId (canonicalNode (ewRoot ew))
        namedSet =
            let base =
                    if IntSet.null interior
                        then namedSet1
                        else IntSet.intersection namedSet1 interior
            in IntSet.delete rootKey base
    let introCount = ewForallIntros ew
        ops = getInstanceOps (ewWitness ew)
        _ops0Debug =
            debugPhi
                ("phi ops edge=" ++ show (ewEdgeId ew)
                    ++ " root=" ++ show (ewRoot ew)
                    ++ " right=" ++ show (ewRight ew)
                    ++ " introCount=" ++ show introCount
                    ++ " ops=" ++ show ops
                )
                ops
    let mSchemeInfoReplaySeed =
            case mSchemeInfo of
                Just si ->
                    let schemeArity =
                            case siScheme si of
                                Forall binds _ -> length binds
                    in if schemeArity == 0 && maybe False (\tr -> traceBinderArity tr > 0) mTrace
                        then Nothing
                        else Just si
                Nothing -> Nothing
    case debugPhi
        ("phi scheme replay-subst edge=" ++ show (ewEdgeId ew)
            ++ " subst=" ++ show (fmap siSubst mSchemeInfoReplaySeed)
        )
        () of
        () -> pure ()
    siReplay <-
        case mSchemeInfoReplaySeed of
            Nothing -> do
                si0 <- schemeInfoForRoot (ewRoot ew)
                pure si0
            Just si -> pure si
    let replayContract =
            maybe ReplayContractNone etReplayContract mTrace
    (traceBinderSourcesRaw, traceBinderReplayMapRaw, traceBinderMapDomainRaw) <-
        computeTraceBinderReplayBridge mTrace replayContract siReplay
    let traceBinderSources =
            debugPhi
                ("phi traceBinderSources=" ++ show (IntSet.toList traceBinderSourcesRaw))
                traceBinderSourcesRaw
        traceBinderReplayMap =
            debugPhi
                ("phi traceBinderReplayMap=" ++ show (IntMap.toList traceBinderReplayMapRaw))
                traceBinderReplayMapRaw
        traceBinderMapDomain =
            debugPhi
                ("phi traceBinderMapDomain=" ++ show (IntSet.toList traceBinderMapDomainRaw))
                traceBinderMapDomainRaw
    phiWithSchemeOmega
        (omegaCtx (Just siReplay) traceBinderSources traceBinderReplayMap traceBinderMapDomain replayContract)
        namedSet
        siReplay
        introCount
        ops
  where
    debugPhi :: String -> a -> a
    debugPhi = traceGeneralize traceCfg

    omegaCtx
        :: Maybe SchemeInfo
        -> IntSet.IntSet
        -> IntMap.IntMap NodeId
        -> IntSet.IntSet
        -> ReplayContract
        -> OmegaContext
    omegaCtx mSchemeInfoCtx traceBinderSources traceBinderReplayMap traceBinderMapDomain replayContractCtx =
        OmegaContext
            { ocTraceConfig = traceCfg
            , ocPresolutionView = presolutionView
            , ocReifyBoundWithNames = reifyBoundWithNamesAt
            , ocReifyTypeWithNamedSetNoFallback = reifyTypeWithNamedSetNoFallbackAt
            , ocCopyMap = copyMap
            , ocGaParents = mbGaParents
            , ocTrace = mTrace
            , ocSchemeInfo = mSchemeInfoCtx
            , ocTraceBinderSources = traceBinderSources
            , ocTraceBinderReplayMap = traceBinderReplayMap
            , ocTraceBinderMapDomain = traceBinderMapDomain
            , ocReplayContract = replayContractCtx
            , ocEdgeRoot = ewRoot ew
            , ocEdgeLeft = ewLeft ew
            , ocEdgeRight = ewRight ew
            }

    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        let (constraintCheck, schemeConstraint, schemeCanonical) =
                (constraint, constraint, canonicalNode)
        in case checkBindingTree constraintCheck of
            Left err -> Left (BindingTreeError err)
            Right () ->
                case checkNoGenFallback constraintCheck of
                    Left err -> Left (BindingTreeError err)
                    Right () ->
                        case checkSchemeClosureUnder schemeCanonical schemeConstraint of
                            Left err -> Left (BindingTreeError err)
                            Right () -> Right ()

    canonicalNode :: NodeId -> NodeId
    canonicalNode = pvCanonical presolutionView

    constraint :: Constraint
    constraint = pvConstraint presolutionView

    reifyDebugType :: NodeId -> Either ElabError ElabType
    reifyDebugType = reifyType presolutionView

    reifyBoundWithNamesAt
        :: IntMap.IntMap String
        -> NodeId
        -> Either ElabError ElabType
    reifyBoundWithNamesAt = reifyBoundWithNames presolutionView

    reifyTypeWithNamedSetNoFallbackAt
        :: IntMap.IntMap String
        -> IntSet.IntSet
        -> NodeId
        -> Either ElabError ElabType
    reifyTypeWithNamedSetNoFallbackAt = reifyTypeWithNamedSetNoFallback presolutionView

    computeTraceBinderReplayBridge
        :: Maybe EdgeTrace
        -> ReplayContract
        -> SchemeInfo
        -> Either ElabError (IntSet.IntSet, IntMap.IntMap NodeId, IntSet.IntSet)
    computeTraceBinderReplayBridge mbTrace replayContract siReplay =
        case mbTrace of
            Nothing -> Left (MissingEdgeTrace (ewEdgeId ew))
            Just tr ->
                let traceBinderSourcesInOrder =
                        reverse $
                            snd $
                                foldl'
                                    (\(seen, acc) (binder, _arg) ->
                                        let key = getNodeId binder
                                        in if IntSet.member key seen
                                            then (seen, acc)
                                            else (IntSet.insert key seen, binder : acc)
                                    )
                                    (IntSet.empty, [])
                                    (etBinderArgs tr)
                    traceBinderSourceKeys = map getNodeId traceBinderSourcesInOrder
                    traceBinderSourceSet = IntSet.fromList traceBinderSourceKeys
                    replayMapRaw = etBinderReplayMap tr
                    replayMapDomain = IntSet.fromList (IntMap.keys replayMapRaw)
                    replayBinderDomainRaw =
                        IntSet.fromList (IntMap.keys (siSubst siReplay))
                    targetInReplayDomainRaw replayTarget =
                        IntSet.member (getNodeId replayTarget) replayBinderDomainRaw
                    missingSources =
                        IntSet.toList (IntSet.difference traceBinderSourceSet replayMapDomain)
                    extraSources =
                        IntSet.toList (IntSet.difference replayMapDomain traceBinderSourceSet)
                in if isStrictReplayContract replayContract
                    then
                        if not (null missingSources) || not (null extraSources)
                            then
                                Left $
                                    PhiInvariantError $
                                        unlines
                                            [ "trace binder replay-map domain mismatch"
                                            , "edge: " ++ show (ewEdgeId ew)
                                            , "trace binder sources: " ++ show traceBinderSourceKeys
                                            , "replay-map domain: " ++ show (IntMap.keys replayMapRaw)
                                            , "missing source keys: " ++ show missingSources
                                            , "extra source keys: " ++ show extraSources
                                            ]
                        else
                            let validateTarget sourceKey = do
                                    replayTargetRaw <-
                                        case IntMap.lookup sourceKey replayMapRaw of
                                            Just replayTarget -> Right replayTarget
                                            Nothing ->
                                                Left $
                                                    PhiInvariantError $
                                                        unlines
                                                            [ "trace binder replay-map missing source key after domain validation"
                                                            , "edge: " ++ show (ewEdgeId ew)
                                                            , "source key: " ++ show sourceKey
                                                            ]
                                    if targetInReplayDomainRaw replayTargetRaw
                                        then pure (sourceKey, replayTargetRaw)
                                        else
                                            Left $
                                                PhiInvariantError $
                                                    unlines
                                                        [ "replay-map target outside replay binder domain"
                                                        , "edge: " ++ show (ewEdgeId ew)
                                                        , "source key: " ++ show sourceKey
                                                        , "replay target: " ++ show replayTargetRaw
                                                        , "replay binder domain: " ++ show (IntSet.toList replayBinderDomainRaw)
                                                        ]
                            in case mapM validateTarget traceBinderSourceKeys of
                                Left err -> Left err
                                Right replayEntries ->
                                    Right
                                        ( traceBinderSourceSet
                                        , IntMap.fromList replayEntries
                                        , replayMapDomain
                                        )
                    else
                        if IntMap.null replayMapRaw && null traceBinderSourceKeys
                            then Right (IntSet.empty, IntMap.empty, IntSet.empty)
                            else
                                Left $
                                    PhiInvariantError $
                                        unlines
                                            [ "trace replay artifacts expected empty under ReplayContractNone"
                                            , "edge: " ++ show (ewEdgeId ew)
                                            , "trace binder sources: " ++ show traceBinderSourceKeys
                                            , "replay-map domain: " ++ show (IntMap.keys replayMapRaw)
                                            ]

    traceBinderArity :: EdgeTrace -> Int
    traceBinderArity tr =
        IntSet.size $
            IntSet.fromList
                [ getNodeId binder
                | (binder, _arg) <- etBinderArgs tr
                ]

    schemeInfoForRoot :: NodeId -> Either ElabError SchemeInfo
    schemeInfoForRoot root0 = do
        let rootC = canonicalNode root0
            targetNode =
                case pvLookupVarBound presolutionView rootC of
                    Just bnd ->
                        case pvLookupNode presolutionView bnd of
                            Just TyForall{ tnBody = body } -> canonicalNode body
                            _ -> canonicalNode bnd
                    Nothing -> schemeBodyTarget presolutionView root0
        scopeRoot <- instScopeRoot root0
        (sch, subst) <-
            case mbGaParents of
                Nothing -> generalizeAtWith Nothing scopeRoot targetNode
                Just ga -> generalizeAtWith (Just ga) scopeRoot targetNode
        pure SchemeInfo { siScheme = sch, siSubst = subst }

    instScopeRoot :: NodeId -> Either ElabError NodeRef
    instScopeRoot root0 =
        case mbGaParents of
            Nothing ->
                let rootC = canonicalNode root0
                    owners =
                        [ gnId gen
                        | gen <- NodeAccess.allGenNodes constraint
                        , any (\root -> canonicalNode root == rootC) (gnSchemes gen)
                        ]
                in case owners of
                    gid:_ -> Right (genRef gid)
                    [] -> goScope IntSet.empty (typeRef rootC)
            Just ga ->
                let rootC = canonicalNode root0
                    baseFromTrace =
                        case mTrace of
                            Nothing -> Nothing
                            Just tr ->
                                let traceCopyMap = getCopyMapping (etCopyMap tr)
                                    revMatches =
                                        [ NodeId k
                                        | (k, v) <- IntMap.toList traceCopyMap
                                        , canonicalNode v == rootC
                                        ]
                                in listToMaybe revMatches
                    baseRep =
                        IntMap.lookup (getNodeId rootC) (gaSolvedToBase ga)
                            <|> baseFromTrace
                in case baseRep of
                    Nothing -> goScope IntSet.empty (typeRef rootC)
                    Just baseN ->
                        case bindingPathToRootLocal (gaBindParentsBase ga) (typeRef baseN) of
                            Left _ -> goScope IntSet.empty (typeRef rootC)
                            Right path ->
                                case listToMaybe [gid | GenRef gid <- drop 1 path] of
                                    Just gid -> Right (genRef gid)
                                    Nothing -> goScope IntSet.empty (typeRef rootC)
      where
        goScope visited ref
            | IntSet.member (nodeRefKey ref) visited =
                Right (typeRef (canonicalNode root0))
            | otherwise = do
                mbParent <- bindingToElab (Binding.lookupBindParentUnder canonicalNode constraint ref)
                case mbParent of
                    Nothing -> Right (typeRef (canonicalNode root0))
                    Just (GenRef gid, _) -> Right (genRef gid)
                    Just (TypeRef parent, _) ->
                        goScope (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonicalNode parent))

    copyMap :: IntMap.IntMap NodeId
    copyMap =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr -> getCopyMapping (etCopyMap tr)
