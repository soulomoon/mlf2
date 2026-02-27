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
    -- * Production entry point (requires trace)
    phiFromEdgeWitnessWithTrace,
    -- * Test-only entry point (no trace required)
    phiFromEdgeWitnessNoTrace,
    -- * Legacy alias (deprecated)
    phiFromEdgeWitness,
    canonicalNodeM,
    remapSchemeInfoM
) where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Reify.Core (namedNodes, reifyType)
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), InteriorNodes(..), copiedNodes)
import qualified MLF.Binding.Tree as Binding
import MLF.Binding.Tree (checkBindingTree, checkNoGenFallback, checkSchemeClosureUnder)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Phi.Env (PhiM, askResult)
import MLF.Elab.Phi.IdentityBridge
    ( mkIdentityBridge
    , sourceKeysForNode
    , traceOrderRank
    )
import MLF.Elab.Phi.Omega (OmegaContext(..), phiWithSchemeOmega)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Util.Names (parseNameId)

-- | Canonicalize a node id using the union-find from the solve result.
canonicalNodeM :: NodeId -> PhiM NodeId
canonicalNodeM nid = do
    res <- askResult
    pure $ Solved.canonical res nid

-- | Remap scheme info using the copy mapping from an edge trace.
remapSchemeInfoM :: EdgeTrace -> SchemeInfo -> PhiM SchemeInfo
remapSchemeInfoM tr si = do
    solved <- askResult
    pure (remapSchemeInfoByTrace solved tr si)

-- | Re-key scheme substitutions into the EdgeTrace source-ID domain.
--
-- Source IDs in `etBinderArgs` are authoritative for Phi binder membership.
-- We therefore map each substitution key to the best source-ID candidate using:
--   1) binder IDs in trace order (preferred),
--   2) reverse lookup in `etCopyMap`,
--   3) original key as deterministic fallback.
remapSchemeInfoByTrace :: Solved -> EdgeTrace -> SchemeInfo -> SchemeInfo
remapSchemeInfoByTrace solved tr si =
    let traceCopyMap = getCopyMapping (etCopyMap tr)
        binderOrder = map fst (etBinderArgs tr)
        schemeNames = case siScheme si of
            Forall binds _ -> map fst binds
        -- Build bridge for ranking/dedup
        ib = mkIdentityBridge solved (Just tr) traceCopyMap
        traceBindersInOrder =
            let sourceSeeds =
                    if null binderOrder
                        then map NodeId (IntMap.keys traceCopyMap)
                        else binderOrder
                (_, rev) =
                    foldl'
                        (\(seen, acc) binder ->
                            let k = getNodeId binder
                            in if IntSet.member k seen
                                then (seen, acc)
                                else (IntSet.insert k seen, binder : acc)
                        )
                        (IntSet.empty, [])
                        sourceSeeds
            in reverse rev
        -- Name-based preferred candidates (higher-level, stays in Translate)
        traceBinderByName =
            Map.fromList
                [ (name, binder)
                | (name, binder) <- zip schemeNames traceBindersInOrder
                ]
        -- Use bridge's sourceKeysForNode for candidate ranking
        rankedCandidateKeys nid =
            map NodeId (sourceKeysForNode ib nid)
        remapExisting =
            fst $
                foldl'
                    (\(acc, usedKeys) (k, name) ->
                        let preferredByName = Map.lookup name traceBinderByName
                            candidates = maybe id (:) preferredByName
                                            (rankedCandidateKeys (NodeId k))
                            pickSource =
                                case filter (\n -> not (IntSet.member (getNodeId n) usedKeys)) candidates of
                                    src : _ -> src
                                    [] ->
                                        case candidates of
                                            src : _ -> src
                                            [] -> NodeId k
                            srcKey = getNodeId pickSource
                        in ( IntMap.insert srcKey name acc
                           , IntSet.insert srcKey usedKeys
                           )
                    )
                    (IntMap.empty, IntSet.empty)
                    (IntMap.toList (siSubst si))
        synthFromScheme =
            let (_, pairsRev) =
                    foldl'
                        (\(usedKeys, acc) (name, binder) ->
                            let binderKey = getNodeId binder
                            in if IntSet.member binderKey usedKeys
                                then (usedKeys, acc)
                                else (IntSet.insert binderKey usedKeys, (binderKey, name) : acc)
                        )
                        (IntSet.empty, [])
                        (zip schemeNames traceBindersInOrder)
            in IntMap.fromList (reverse pairsRev)
        subst' =
            if IntMap.null (siSubst si)
                then synthFromScheme
                else remapExisting
    in si { siSubst = subst' }

-- | Hydrate substitutions using traced binder order when remapped substitutions
-- under-cover scheme binders. This preserves source-ID provenance instead of
-- dropping scheme info on arity mismatch.
hydrateSchemeInfoByTrace :: Solved -> EdgeTrace -> SchemeInfo -> SchemeInfo
hydrateSchemeInfoByTrace solved tr si =
    let schemeNames =
            case siScheme si of
                Forall binds _ -> map fst binds
        schemeNameSet =
            Map.fromList [(name, ()) | name <- schemeNames]
        traceCopyMap = getCopyMapping (etCopyMap tr)
        -- Build bridge for ranking/dedup
        ib = mkIdentityBridge solved (Just tr) traceCopyMap
        traceBinderKeys =
            let (_, rev) =
                    foldl'
                        (\(seen, acc) (binder, _arg) ->
                            let key = getNodeId binder
                            in if IntSet.member key seen
                                then (seen, acc)
                                else (IntSet.insert key seen, key : acc)
                        )
                        (IntSet.empty, [])
                        (etBinderArgs tr)
            in reverse rev
        -- Use bridge's traceOrderRank for key ranking
        keyRank key = traceOrderRank ib key
        traceByName = Map.fromList (zip schemeNames traceBinderKeys)
        chooseBetterKey new old =
            if keyRank new < keyRank old
                then new
                else old
        existingByName =
            foldl'
                (\acc (key, name) ->
                    if Map.member name schemeNameSet
                        then Map.insertWith chooseBetterKey name key acc
                        else acc
                )
                Map.empty
                (IntMap.toList (siSubst si))
        uniqueKeys =
            reverse
                . snd
                . foldl'
                    (\(seen, acc) key ->
                        if IntSet.member key seen
                            then (seen, acc)
                            else (IntSet.insert key seen, key : acc)
                    )
                    (IntSet.empty, [])
        (pairsRev, _) =
            foldl'
                (\(acc, usedKeys) name ->
                    let candidates =
                            sortOn keyRank $
                                uniqueKeys
                                    ( maybe [] pure (Map.lookup name existingByName)
                                    ++ maybe [] pure (Map.lookup name traceByName)
                                    )
                        mbPick =
                            listToMaybe
                                [ key
                                | key <- candidates
                                , not (IntSet.member key usedKeys)
                                ]
                    in case mbPick of
                        Nothing -> (acc, usedKeys)
                        Just key ->
                            ( (key, name) : acc
                            , IntSet.insert key usedKeys
                            )
                )
                ([], IntSet.empty)
                schemeNames
    in si { siSubst = IntMap.fromList (reverse pairsRev) }

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
type GeneralizeAtWith =
    Maybe GaBindParents
    -> Solved
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

-- | Strict no-trace path for tests/debugging:
-- fails fast because production Φ requires edge traces.
phiFromEdgeWitnessNoTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> Solved
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessNoTrace _traceCfg _generalizeAtWith _res _mSchemeInfo ew =
    Left (MissingEdgeTrace (ewEdgeId ew))

-- | Legacy alias for 'phiFromEdgeWitnessNoTrace' (deprecated; test/debug-only).
phiFromEdgeWitness
    :: TraceConfig
    -> GeneralizeAtWith
    -> Solved
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitness = phiFromEdgeWitnessNoTrace

{-# DEPRECATED phiFromEdgeWitness "Use phiFromEdgeWitnessWithTrace; the no-trace entrypoint is strict fail-fast." #-}

phiFromEdgeWitnessWithTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> Solved
    -> Maybe GaBindParents
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith res mbGaParents mSchemeInfo mTrace ew =
    case mTrace of
        Nothing -> Left (MissingEdgeTrace (ewEdgeId ew))
        Just _ -> phiFromEdgeWitnessCore traceCfg generalizeAtWith res mbGaParents mSchemeInfo mTrace ew

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
    -> Solved
    -> Maybe GaBindParents
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessCore traceCfg generalizeAtWith res mbGaParents mSchemeInfo mTrace ew = do
    requireValidBindingTree
    namedSet0 <- namedNodes res
    case debugPhi
        ("phi ewLeft=" ++ show (ewLeft ew)
            ++ " ewRight=" ++ show (ewRight ew)
        )
        () of
        () -> pure ()
    case debugPhi
        ("phi ewRootType=" ++ show (reifyType res (ewRoot ew))
            ++ " ewLeftType=" ++ show (reifyType res (ewLeft ew))
            ++ " ewRightType=" ++ show (reifyType res (ewRight ew))
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
    let siSource =
            case mTrace of
                Just tr -> remapAndHydrateSchemeInfo tr siReplay
                Nothing -> siReplay
    case debugPhi
        ("phi scheme source-subst edge=" ++ show (ewEdgeId ew)
            ++ " subst=" ++ show (siSubst siSource)
        )
        () of
        () -> pure ()
    (traceBinderSourcesRaw, traceBinderReplayMapRaw, traceBinderMapDomainRaw) <-
        computeTraceBinderReplayBridge mTrace siReplay siSource
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
        (omegaCtx (Just siReplay) traceBinderSources traceBinderReplayMap traceBinderMapDomain)
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
        -> OmegaContext
    omegaCtx mSchemeInfoCtx traceBinderSources traceBinderReplayMap traceBinderMapDomain =
        OmegaContext
            { ocTraceConfig = traceCfg
            , ocSolved = res
            , ocCopyMap = copyMap
            , ocGaParents = mbGaParents
            , ocTrace = mTrace
            , ocSchemeInfo = mSchemeInfoCtx
            , ocTraceBinderSources = traceBinderSources
            , ocTraceBinderReplayMap = traceBinderReplayMap
            , ocTraceBinderMapDomain = traceBinderMapDomain
            , ocEdgeRoot = ewRoot ew
            , ocEdgeLeft = ewLeft ew
            , ocEdgeRight = ewRight ew
            }

    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        let (constraintCheck, schemeConstraint, schemeCanonical) =
                (Solved.originalConstraint res, Solved.originalConstraint res, canonicalNode)
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
    canonicalNode = Solved.canonical res

    remapSchemeInfo :: EdgeTrace -> SchemeInfo -> SchemeInfo
    remapSchemeInfo tr si = remapSchemeInfoByTrace res tr si

    remapAndHydrateSchemeInfo :: EdgeTrace -> SchemeInfo -> SchemeInfo
    remapAndHydrateSchemeInfo tr =
        hydrateSchemeInfoByTrace res tr . remapSchemeInfo tr

    computeTraceBinderReplayBridge
        :: Maybe EdgeTrace
        -> SchemeInfo
        -> SchemeInfo
        -> Either ElabError (IntSet.IntSet, IntMap.IntMap NodeId, IntSet.IntSet)
    computeTraceBinderReplayBridge mbTrace siReplay siSource =
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
                    traceCopyMap = getCopyMapping (etCopyMap tr)
                    ib = mkIdentityBridge res (Just tr) traceCopyMap
                    sourceNameByKey =
                        IntMap.fromList (IntMap.toList (siSubst siSource))
                    isLiveTyVarTarget replayTarget =
                        case Solved.lookupNode res (canonicalNode replayTarget) of
                            Just TyVar{} -> True
                            _ -> False
                    chooseBetterKey new old =
                        if traceOrderRank ib new < traceOrderRank ib old
                            then new
                            else old
                    replayKeyByName =
                        foldl'
                            (\acc (key, name) ->
                                Map.insertWith chooseBetterKey name key acc
                            )
                            Map.empty
                            (IntMap.toList (siSubst siReplay))
                    dedupKeys =
                        reverse
                            . snd
                            . foldl'
                                (\(seen, acc) key ->
                                    if IntSet.member key seen
                                        then (seen, acc)
                                        else (IntSet.insert key seen, key : acc)
                                    )
                                (IntSet.empty, [])
                    replayKeyBySchemeName =
                        Map.fromList
                            [ (name, NodeId key)
                            | (key, name) <- IntMap.toList (siSubst siReplay)
                            ]
                    replayKeysFromSchemeQuantifiers =
                        dedupKeys
                            [ getNodeId replayTarget
                            | (name, _mbBound) <- case siScheme siReplay of
                                Forall binds _ -> binds
                            , Just replayTarget <-
                                [ Map.lookup name replayKeyBySchemeName
                                    <|> (NodeId <$> parseNameId name)
                                ]
                            , isLiveTyVarTarget replayTarget
                            ]
                    replayKeysInTraceOrder =
                        if null replayKeysFromSchemeQuantifiers
                            then
                                dedupKeys
                                    [ replayKey
                                    | replayKey <- sortOn (traceOrderRank ib) (IntMap.keys (siSubst siReplay))
                                    , isLiveTyVarTarget (NodeId replayKey)
                                    ]
                            else replayKeysFromSchemeQuantifiers
                    positionalReplayMap =
                        IntMap.fromList
                            [ (sourceKey, NodeId replayKey)
                            | (sourceKey, replayKey) <- zip traceBinderSourceKeys replayKeysInTraceOrder
                            ]
                    replayMapRaw = etBinderReplayMap tr
                    replayMapDomain = IntSet.fromList (IntMap.keys replayMapRaw)
                    replayMapCanonical0 =
                        replayMapRaw
                    replayBinderDomainFromSchemeRaw =
                        IntSet.fromList replayKeysInTraceOrder
                    replayBinderDomainFromSchemeCanonical =
                        IntSet.fromList
                            [ getNodeId (canonicalNode (NodeId key))
                            | key <- IntSet.toList replayBinderDomainFromSchemeRaw
                            ]
                    replayBinderDomainFromMapRaw0 =
                        IntSet.fromList
                            [ getNodeId replayTarget0
                            | replayTarget0 <- IntMap.elems replayMapCanonical0
                            , isLiveTyVarTarget replayTarget0
                            ]
                    preferredReplayBinderDomainRaw =
                        if IntSet.null replayBinderDomainFromSchemeRaw
                            then replayBinderDomainFromMapRaw0
                            else replayBinderDomainFromSchemeRaw
                    inPreferredReplayDomainRaw replayTarget =
                        IntSet.member (getNodeId replayTarget) preferredReplayBinderDomainRaw
                    sourceTyVarFallback sourceKey =
                        if IntSet.null replayBinderDomainFromSchemeRaw
                            then
                                let sourceTarget = NodeId sourceKey
                                in if isLiveTyVarTarget sourceTarget
                                    then Just sourceTarget
                                    else Nothing
                            else Nothing
                    sourceNameFallback sourceKey =
                        case IntMap.lookup sourceKey sourceNameByKey of
                            Nothing -> Nothing
                            Just name ->
                                case Map.lookup name replayKeyByName of
                                    Nothing -> Nothing
                                    Just replayKey ->
                                        let replayTarget = NodeId replayKey
                                        in if isLiveTyVarTarget replayTarget
                                            && (IntSet.null replayBinderDomainFromSchemeRaw
                                                || inPreferredReplayDomainRaw replayTarget)
                                            then Just replayTarget
                                            else Nothing
                    positionalFallback sourceKey =
                        case IntMap.lookup sourceKey positionalReplayMap of
                            Just candidate ->
                                if isLiveTyVarTarget candidate
                                    && (IntSet.null replayBinderDomainFromSchemeRaw
                                        || inPreferredReplayDomainRaw candidate)
                                    then Just candidate
                                    else Nothing
                            Nothing -> Nothing
                    replayMapCanonical =
                        IntMap.fromList
                            [ let existing = IntMap.lookup sourceKey replayMapCanonical0
                                  existingValid =
                                      case existing of
                                          Just replayTarget
                                              | IntSet.member (getNodeId replayTarget) preferredReplayBinderDomainRaw ->
                                                  Just replayTarget
                                          _ -> Nothing
                                  defaultReplayFallback =
                                      if IntSet.null replayBinderDomainFromSchemeRaw
                                          then Nothing
                                          else case replayKeysInTraceOrder of
                                              replayKey : _ -> Just (NodeId replayKey)
                                              [] -> Nothing
                                  fallbackTarget =
                                      existingValid
                                          <|> sourceNameFallback sourceKey
                                          <|> positionalFallback sourceKey
                                          <|> sourceTyVarFallback sourceKey
                                          <|> defaultReplayFallback
                                  chosen =
                                      case fallbackTarget of
                                          Just replayTarget -> replayTarget
                                          Nothing ->
                                              case existing of
                                                  Just replayTarget -> replayTarget
                                                  Nothing -> NodeId sourceKey
                              in (sourceKey, chosen)
                            | sourceKey <- traceBinderSourceKeys
                            ]
                    replayBinderDomainFromMapRaw' =
                        IntSet.fromList
                            [ getNodeId replayTarget
                            | replayTarget <- IntMap.elems replayMapCanonical
                            , isLiveTyVarTarget replayTarget
                            ]
                    replayBinderDomainFromMapCanonical' =
                        IntSet.fromList
                            [ getNodeId (canonicalNode replayTarget)
                            | replayTarget <- IntMap.elems replayMapCanonical
                            , isLiveTyVarTarget replayTarget
                            ]
                    replayBinderDomainCanonical =
                        IntSet.union replayBinderDomainFromSchemeCanonical replayBinderDomainFromMapCanonical'
                    targetInReplayDomain replayTarget =
                        if IntSet.null replayBinderDomainFromSchemeRaw
                            then IntSet.member (getNodeId replayTarget) replayBinderDomainFromMapRaw'
                            else IntSet.member (getNodeId replayTarget) replayBinderDomainFromSchemeRaw
                    renderReplayBinderDomain =
                        if IntSet.null replayBinderDomainFromSchemeRaw
                            then IntSet.toList replayBinderDomainCanonical
                            else IntSet.toList replayBinderDomainFromSchemeRaw
                    missingSources =
                        IntSet.toList (IntSet.difference traceBinderSourceSet replayMapDomain)
                    extraSources =
                        IntSet.toList (IntSet.difference replayMapDomain traceBinderSourceSet)
                    invalidTargets =
                        if IntSet.null replayBinderDomainFromSchemeRaw
                            then []
                            else
                                [ (sourceKey, replayTarget)
                                | (sourceKey, replayTarget) <- IntMap.toList replayMapCanonical
                                , not (targetInReplayDomain replayTarget)
                                ]
                in
                    if not (null missingSources) || not (null extraSources)
                        then
                            Left $
                                PhiInvariantError $
                                    unlines
                                        [ "trace binder replay-map domain mismatch"
                                        , "edge: " ++ show (ewEdgeId ew)
                                        , "trace binder sources: " ++ show traceBinderSourceKeys
                                        , "replay-map domain: " ++ show (IntMap.keys replayMapCanonical)
                                        , "missing source keys: " ++ show missingSources
                                        , "extra source keys: " ++ show extraSources
                                        ]
                    else case invalidTargets of
                        (sourceKey, replayTarget) : _ ->
                            Left $
                                PhiInvariantError $
                                    unlines
                                        [ "trace binder replay-map target outside replay binder domain"
                                        , "edge: " ++ show (ewEdgeId ew)
                                        , "source key: " ++ show sourceKey
                                        , "replay target: " ++ show replayTarget
                                        , "replay binder domain: " ++ show renderReplayBinderDomain
                                        ]
                        [] ->
                            Right
                                ( traceBinderSourceSet
                                , replayMapCanonical
                                , replayMapDomain
                                )

    traceBinderArity :: EdgeTrace -> Int
    traceBinderArity tr =
        IntSet.size $
            IntSet.fromList
                [ getNodeId binder
                | (binder, _arg) <- etBinderArgs tr
                ]

    schemeInfoForRoot :: NodeId -> Either ElabError SchemeInfo
    schemeInfoForRoot root0 = do
        scopeRoot <- instScopeRoot root0
        (sch, subst) <-
            case mbGaParents of
                Nothing -> generalizeAtWith Nothing res scopeRoot root0
                Just ga -> generalizeAtWith (Just ga) res scopeRoot root0
        pure SchemeInfo { siScheme = sch, siSubst = subst }

    instScopeRoot :: NodeId -> Either ElabError NodeRef
    instScopeRoot root0 =
        case mbGaParents of
            Nothing ->
                let rootC = canonicalNode root0
                    owners =
                        [ gnId gen
                        | gen <- NodeAccess.allGenNodes (Solved.originalConstraint res)
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
                mbParent <- bindingToElab (Binding.lookupBindParentUnder canonicalNode (Solved.originalConstraint res) ref)
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
