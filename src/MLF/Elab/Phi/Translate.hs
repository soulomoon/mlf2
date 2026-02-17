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
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), InteriorNodes(..), copiedNodes)
import qualified MLF.Binding.Tree as Binding
import MLF.Binding.Tree (checkBindingTree, checkNoGenFallback, checkSchemeClosureUnder)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Phi.Env (PhiM, askCanonical, askResult)
import MLF.Elab.Phi.IdentityBridge (mkIdentityBridge, sourceKeysForNode, traceOrderRank)
import MLF.Elab.Phi.Omega (OmegaContext(..), phiWithSchemeOmega)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

-- | Canonicalize a node id using the union-find from the solve result.
canonicalNodeM :: NodeId -> PhiM NodeId
canonicalNodeM nid = do
    res <- askResult
    pure $ Solve.frWith (srUnionFind res) nid

-- | Remap scheme info using the copy mapping from an edge trace.
remapSchemeInfoM :: EdgeTrace -> SchemeInfo -> PhiM SchemeInfo
remapSchemeInfoM tr si = do
    canonical <- askCanonical
    pure (remapSchemeInfoByTrace canonical tr si)

-- | Re-key scheme substitutions into the EdgeTrace source-ID domain.
--
-- Source IDs in `etBinderArgs` are authoritative for Phi binder membership.
-- We therefore map each substitution key to the best source-ID candidate using:
--   1) binder IDs in trace order (preferred),
--   2) reverse lookup in `etCopyMap`,
--   3) original key as deterministic fallback.
remapSchemeInfoByTrace :: (NodeId -> NodeId) -> EdgeTrace -> SchemeInfo -> SchemeInfo
remapSchemeInfoByTrace canonical tr si =
    let traceCopyMap = getCopyMapping (etCopyMap tr)
        binderOrder = map fst (etBinderArgs tr)
        schemeNames = case siScheme si of
            Forall binds _ -> map fst binds
        -- Build bridge for ranking/dedup
        ib = mkIdentityBridge canonical (Just tr) traceCopyMap
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
hydrateSchemeInfoByTrace :: (NodeId -> NodeId) -> EdgeTrace -> SchemeInfo -> SchemeInfo
hydrateSchemeInfoByTrace canonical tr si =
    let schemeNames =
            case siScheme si of
                Forall binds _ -> map fst binds
        schemeNameSet =
            Map.fromList [(name, ()) | name <- schemeNames]
        traceCopyMap = getCopyMapping (etCopyMap tr)
        -- Build bridge for ranking/dedup
        ib = mkIdentityBridge canonical (Just tr) traceCopyMap
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
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

-- | Test/debug-only: Translate without requiring an EdgeTrace.
-- This bypasses thesis-exact precondition checks and must not be used by
-- production elaboration paths.
phiFromEdgeWitnessNoTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessNoTrace traceCfg generalizeAtWith res mSchemeInfo ew =
    phiFromEdgeWitnessCore traceCfg generalizeAtWith res Nothing mSchemeInfo Nothing ew

-- | Legacy alias for 'phiFromEdgeWitnessNoTrace' (deprecated; test/debug-only).
phiFromEdgeWitness
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitness = phiFromEdgeWitnessNoTrace

{-# DEPRECATED phiFromEdgeWitness "Use phiFromEdgeWitnessNoTrace only in tests/debugging; production code must use phiFromEdgeWitnessWithTrace." #-}

phiFromEdgeWitnessWithTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> Maybe GaBindParents
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith res mbGaParents mSchemeInfo mTrace ew =
    case mTrace of
        Nothing -> Left (MissingEdgeTrace (ewEdgeId ew))
        Just _ -> phiFromEdgeWitnessCore traceCfg generalizeAtWith res mbGaParents mSchemeInfo mTrace ew

phiFromEdgeWitnessCore
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
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
    let InstanceWitness ops = ewWitness ew
        steps0Raw =
            case ewSteps ew of
                [] -> map StepOmega ops
                xs -> xs
        steps0 =
            debugPhi
                ("phi steps edge=" ++ show (ewEdgeId ew)
                    ++ " root=" ++ show (ewRoot ew)
                    ++ " right=" ++ show (ewRight ew)
                    ++ " steps=" ++ show steps0Raw
                )
                steps0Raw
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
    let (traceBinderSourcesRaw, traceBinderReplayMapRaw, traceBinderHintDomainRaw, traceBinderSourceNamesRaw) =
            computeTraceBinderReplayBridge mTrace siReplay siSource
        traceBinderSources =
            debugPhi
                ("phi traceBinderSources=" ++ show (IntSet.toList traceBinderSourcesRaw))
                traceBinderSourcesRaw
        traceBinderReplayMap =
            debugPhi
                ("phi traceBinderReplayMap=" ++ show (IntMap.toList traceBinderReplayMapRaw))
                traceBinderReplayMapRaw
        traceBinderHintDomain =
            debugPhi
                ("phi traceBinderHintDomain=" ++ show (IntSet.toList traceBinderHintDomainRaw))
                traceBinderHintDomainRaw
        traceBinderSourceNames =
            debugPhi
                ("phi traceBinderSourceNames=" ++ show (IntMap.toList traceBinderSourceNamesRaw))
                traceBinderSourceNamesRaw
    targetBinderKeysRaw <- computeTargetBinderKeys siReplay
    let targetBinderKeys =
            debugPhi
                ("phi targetBinderKeys=" ++ show (IntSet.toList targetBinderKeysRaw))
                targetBinderKeysRaw
    phiWithSchemeOmega
        (omegaCtx (Just siReplay) traceBinderSources traceBinderReplayMap traceBinderHintDomain traceBinderSourceNames)
        namedSet
        targetBinderKeys
        siReplay
        steps0
  where
    debugPhi :: String -> a -> a
    debugPhi = traceGeneralize traceCfg

    omegaCtx
        :: Maybe SchemeInfo
        -> IntSet.IntSet
        -> IntMap.IntMap NodeId
        -> IntSet.IntSet
        -> IntMap.IntMap String
        -> OmegaContext
    omegaCtx mSchemeInfoCtx traceBinderSources traceBinderReplayMap traceBinderHintDomain traceBinderSourceNames =
        OmegaContext
            { ocTraceConfig = traceCfg
            , ocResult = res
            , ocCanonicalNode = canonicalNode
            , ocCopyMap = copyMap
            , ocGaParents = mbGaParents
            , ocTrace = mTrace
            , ocSchemeInfo = mSchemeInfoCtx
            , ocTraceBinderSources = traceBinderSources
            , ocTraceBinderReplayMap = traceBinderReplayMap
            , ocTraceBinderHintDomain = traceBinderHintDomain
            , ocTraceBinderSourceNames = traceBinderSourceNames
            , ocEdgeRoot = ewRoot ew
            , ocEdgeLeft = ewLeft ew
            , ocEdgeRight = ewRight ew
            }

    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        let (constraintCheck, schemeConstraint, schemeCanonical) =
                (srConstraint res, srConstraint res, canonicalNode)
        in case checkBindingTree constraintCheck of
            Left err -> Left (BindingTreeError err)
            Right () ->
                case checkNoGenFallback constraintCheck of
                    Left err -> Left (BindingTreeError err)
                    Right () ->
                        case checkSchemeClosureUnder schemeCanonical schemeConstraint of
                            Left GenSchemeFreeVars{} -> Right ()
                            Left err -> Left (BindingTreeError err)
                            Right () -> Right ()

    canonicalNode :: NodeId -> NodeId
    canonicalNode = Solve.frWith (srUnionFind res)

    remapSchemeInfo :: EdgeTrace -> SchemeInfo -> SchemeInfo
    remapSchemeInfo tr si = remapSchemeInfoByTrace canonicalNode tr si

    remapAndHydrateSchemeInfo :: EdgeTrace -> SchemeInfo -> SchemeInfo
    remapAndHydrateSchemeInfo tr =
        hydrateSchemeInfoByTrace canonicalNode tr . remapSchemeInfo tr

    computeTraceBinderReplayBridge
        :: Maybe EdgeTrace
        -> SchemeInfo
        -> SchemeInfo
        -> (IntSet.IntSet, IntMap.IntMap NodeId, IntSet.IntSet, IntMap.IntMap String)
    computeTraceBinderReplayBridge mbTrace siReplay siSource =
        case mbTrace of
            Nothing -> (IntSet.empty, IntMap.empty, IntSet.empty, IntMap.empty)
            Just tr ->
                let schemeNames =
                        case siScheme siReplay of
                            Forall binds _ -> map fst binds
                    traceBinderSourcesInOrder =
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
                    traceBinderSourcesKeys =
                        [ getNodeId binder
                        | binder <- traceBinderSourcesInOrder
                        ]
                    traceBinderSourcesSet =
                        IntSet.fromList traceBinderSourcesKeys
                    hintMapRaw = etBinderReplayHints tr
                    hintDomain = IntSet.fromList (IntMap.keys hintMapRaw)
                    hintPeersByHint =
                        IntMap.fromListWith IntSet.union
                            [ ( getNodeId (canonicalNode replayN)
                              , IntSet.singleton sourceKey
                              )
                            | (sourceKey, replayN) <- IntMap.toList hintMapRaw
                            ]
                    traceCopyMap = getCopyMapping (etCopyMap tr)
                    ib = mkIdentityBridge canonicalNode (Just tr) traceCopyMap
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
                    sourceNameByKey =
                        IntMap.fromList (IntMap.toList (siSubst siSource))
                    replayBinderDomain =
                        IntSet.fromList
                            [ getNodeId (canonicalNode (NodeId key))
                            | key <- IntMap.keys (siSubst siReplay)
                            , case NodeAccess.lookupNode (srConstraint res) (canonicalNode (NodeId key)) of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                    isReplayBinderKey key =
                        IntSet.member
                            (getNodeId (canonicalNode (NodeId key)))
                            replayBinderDomain
                    isUsableReplayKey key =
                        isReplayBinderKey key
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
                    replayFromHint sourceKey =
                        case IntMap.lookup sourceKey hintMapRaw of
                            Nothing -> []
                            Just replayN ->
                                let replayKey = getNodeId (canonicalNode replayN)
                                in [NodeId replayKey | isUsableReplayKey replayKey]
                    replayFromName sourceKey =
                        case IntMap.lookup sourceKey sourceNameByKey of
                            Nothing -> []
                            Just name ->
                                case Map.lookup name replayKeyByName of
                                    Just replayKey | isUsableReplayKey replayKey ->
                                        [NodeId replayKey]
                                    _ -> []
                    sameHintSources sourceKey =
                        case IntMap.lookup sourceKey hintMapRaw of
                            Nothing -> []
                            Just replayN ->
                                IntSet.toList $
                                    IntMap.findWithDefault
                                        (IntSet.singleton sourceKey)
                                        (getNodeId (canonicalNode replayN))
                                        hintPeersByHint
                    replayFromAlias sourceKey =
                        let sourceEqKeys =
                                dedupKeys
                                    ( sourceKey
                                        : sourceKeysForNode ib (NodeId sourceKey)
                                        ++ concat
                                            [ peerKey : sourceKeysForNode ib (NodeId peerKey)
                                            | peerKey <- sameHintSources sourceKey
                                            ]
                                    )
                            directReplay =
                                [ NodeId key
                                | key <- sourceEqKeys
                                , isUsableReplayKey key
                                ]
                            hintedReplay =
                                concatMap replayFromHint sourceEqKeys
                            namedReplay =
                                concatMap replayFromName sourceEqKeys
                        in map canonicalNode (hintedReplay ++ namedReplay ++ directReplay)
                    replayCandidates sourceKey =
                        let candidates =
                                replayFromHint sourceKey
                                    ++ replayFromName sourceKey
                                    ++ replayFromAlias sourceKey
                            dedupNodeIds =
                                reverse
                                    . snd
                                    . foldl'
                                        (\(seen, acc) nid ->
                                            let k = getNodeId nid
                                            in if IntSet.member k seen
                                                then (seen, acc)
                                                else (IntSet.insert k seen, nid : acc)
                                        )
                                        (IntSet.empty, [])
                        in dedupNodeIds candidates
                    replaySeedPairs =
                        [ (getNodeId sourceBinder, replayCandidates (getNodeId sourceBinder))
                        | sourceBinder <- traceBinderSourcesInOrder
                        ]
                    replayKeysInTraceOrder =
                        dedupKeys
                            [ getNodeId (canonicalNode (NodeId replayKey))
                        | replayKey <- sortOn (traceOrderRank ib) (IntMap.keys (siSubst siReplay))
                        , isUsableReplayKey replayKey
                            ]
                    replayPositionalSeed =
                        IntMap.fromList
                            [ (sourceKey, [NodeId replayKey])
                            | (sourceKey, replayKey) <- zip traceBinderSourcesKeys replayKeysInTraceOrder
                            ]
                    replayNameSeed =
                        IntMap.fromList
                            [ (sourceKey, [NodeId replayKey])
                            | ((_, name), sourceKey) <- zip (zip traceBinderSourcesInOrder schemeNames) traceBinderSourcesKeys
                            , Just replayKey <- [Map.lookup name replayKeyByName]
                            , isUsableReplayKey replayKey
                            ]
                    replayAllCandidates =
                        IntMap.fromListWith
                            (\new old -> new ++ old)
                            [ (sourceKey, cands)
                            | (sourceKey, cands) <- replaySeedPairs
                            ]
                    replayAllCandidates' =
                        IntMap.unionsWith (\a b -> a ++ b)
                            [ replayAllCandidates
                            , replayNameSeed
                            , replayPositionalSeed
                            ]
                    replayCandidatesBySource =
                        [ ( sourceKey
                          , map getNodeId (IntMap.findWithDefault [] sourceKey replayAllCandidates')
                          )
                        | sourceKey <- traceBinderSourcesKeys
                        ]
                    replayMap =
                        fst $
                            foldl'
                                (\(acc, used) sourceKey ->
                                    let cands0 = IntMap.findWithDefault [] sourceKey replayAllCandidates'
                                        cands =
                                            [ canonicalNode nid
                                            | nid <- cands0
                                            , isUsableReplayKey (getNodeId (canonicalNode nid))
                                            ]
                                        pickUnused =
                                            listToMaybe
                                                [ nid
                                                | nid <- cands
                                                , not (IntSet.member (getNodeId nid) used)
                                                ]
                                        fallbackHint =
                                            canonicalNode <$>
                                                IntMap.lookup sourceKey hintMapRaw
                                        fallbackRaw =
                                            Just (canonicalNode (NodeId sourceKey))
                                        pick = pickUnused <|> listToMaybe cands <|> fallbackHint <|> fallbackRaw
                                    in case pick of
                                        Nothing ->
                                            ( IntMap.insert sourceKey (NodeId sourceKey) acc
                                            , used
                                            )
                                        Just replayN ->
                                            ( IntMap.insert sourceKey replayN acc
                                            , IntSet.insert (getNodeId replayN) used
                                            )
                                )
                                (IntMap.empty, IntSet.empty)
                                traceBinderSourcesKeys
                    replayMap' =
                        debugPhi
                            ( "phi replayBridge detail: positionalSeed="
                                ++ show (fmap (map getNodeId) replayPositionalSeed)
                                ++ " nameSeed="
                                ++ show (fmap (map getNodeId) replayNameSeed)
                                ++ " candidates="
                                ++ show replayCandidatesBySource
                                ++ " replayMap="
                                ++ show (IntMap.toList replayMap)
                            )
                            replayMap
                in (traceBinderSourcesSet, replayMap', hintDomain, sourceNameByKey)

    computeTargetBinderKeys :: SchemeInfo -> Either ElabError IntSet.IntSet
    computeTargetBinderKeys siForKeys =
        case mTrace of
            -- Keep pre-bridge behavior for trace-free Φ tests/callers:
            -- without an edge trace we should not retain binder targets.
            Nothing -> pure IntSet.empty
            Just _ -> do
                let targetRootC = canonicalNode (ewRight ew)
                    schemeReplayKeys = IntMap.keys (siSubst siForKeys)
                targetBinders <-
                    case bindingToElab (Binding.orderedBinders canonicalNode (srConstraint res) (typeRef targetRootC)) of
                        Right bs -> pure bs
                        Left _ -> pure []
                let targetCanonKeys =
                        IntSet.fromList
                            [ getNodeId (canonicalNode binder)
                            | binder <- targetBinders
                            ]
                    keepFrom replayKeys =
                        IntSet.fromList
                            [ replayKey
                            | replayKey <- replayKeys
                            , IntSet.member
                                (getNodeId (canonicalNode (NodeId replayKey)))
                                targetCanonKeys
                            ]
                    keepKeys = keepFrom schemeReplayKeys
                debugPhi
                    ("phi target binders=" ++ show targetBinders
                        ++ " keep-keys=" ++ show (IntSet.toList keepKeys)
                    )
                    (pure ())
                pure keepKeys

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
                        | gen <- NodeAccess.allGenNodes (srConstraint res)
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
                mbParent <- bindingToElab (Binding.lookupBindParentUnder canonicalNode (srConstraint res) ref)
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
