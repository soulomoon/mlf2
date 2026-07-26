{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
    -- * Checked occurrence endpoint authority
    PhiOccurrenceRole(..),
    PhiEndpointShapeAuthority(..),
    -- * Translation entry point (requires trace)
    phiFromEdgeWitnessWithTrace,
    phiFromEdgeWitnessWithTraceReadModel,
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints,
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor,
    -- * Paper-shaped occurrence computation
    phiOccurrenceFromEdgeWitnessWithTrace,
    phiOccurrenceFromEdgeWitnessWithTraceReadModel
) where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (schemeToType)
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Elab.ReadModel
    ( ElabReadModel
    , PhiReadModel
    , buildElabReadModel
    , buildPhiReadModel
    , ermNamedNodes
    , ermPresolutionView
    , phiReadModelElabReadModel
    )
import MLF.Reify.Core
    ( reifyType
    )
import MLF.Reify.Bound
    ( reifyBoundWithRefsReadModel
    )
import MLF.Reify.Type (reifyTypeWithNamedSetRefsNoFallbackReadModel)
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Constraint.Presolution (EdgeTrace(..), PresolutionView(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), EdgeSourceInterior(..), InteriorNodes(..), copiedNodes)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Phi.Computation
    ( OccurrenceComputation
    , occurrenceComputationInstantiation
    )
import MLF.Elab.Phi.Omega
    ( OmegaContext(..)
    , phiWithSchemeOmegaOccurrence
    )
import MLF.Util.Trace (TraceConfig(..), traceGeneralize)
import MLF.Elab.Run.Scope (schemeBodyTarget)

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
type GeneralizeAtWith (p :: Phase) =
    Maybe (GaBindParents p)
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)

-- | The syntactic role that owns an application occurrence endpoint.  Phi
-- receives a checked producer scheme, but the same edge can be consumed as
-- either the function or the argument of an application.  Retaining that role
-- prevents a closed value occurrence from being selected merely because its
-- strict replay domain is empty.
data PhiOccurrenceRole
    = PhiApplicationFunctionOccurrence
    | PhiApplicationArgumentOccurrence
    deriving (Eq, Show)

-- | Positive endpoint-shape authority supplied by the application
-- constructor.  The checked SchemeInfo remains the producer authority; this
-- record says which endpoint that producer must construct at this occurrence.
-- It carries no ambient Gamma bindings.
data PhiEndpointShapeAuthority = PhiEndpointShapeAuthority
    { pesaOccurrenceRole :: !PhiOccurrenceRole
    , pesaRequiredEndpointType :: !ElabType
    }
    deriving (Eq, Show)

data PhiOuterShape
    = PhiArrowShape
    | PhiMuShape
    | PhiBaseShape SymbolIdentity
    | PhiConstructorShape SymbolIdentity
    | PhiVarApplicationShape
    deriving (Eq)

phiFromEdgeWitnessWithTrace
    :: TraceConfig
    -> GeneralizeAtWith p
    -> PresolutionView p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith presolutionView gaParents mSchemeInfo mTrace ew =
    occurrenceComputationInstantiation
        <$> phiOccurrenceFromEdgeWitnessWithTrace
            traceCfg
            generalizeAtWith
            presolutionView
            gaParents
            mSchemeInfo
            mTrace
            ew

-- | Translate a recorded edge witness into the paper-shaped
-- @phi_R;T(e)@ occurrence computation.
phiOccurrenceFromEdgeWitnessWithTrace
    :: TraceConfig
    -> GeneralizeAtWith p
    -> PresolutionView p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError OccurrenceComputation
phiOccurrenceFromEdgeWitnessWithTrace traceCfg generalizeAtWith presolutionView gaParents mSchemeInfo mTrace ew =
    case mTrace of
        Nothing -> Left (MissingEdgeTrace (ewEdgeId ew))
        Just _ -> do
            readModel <- buildElabReadModel presolutionView
            phiOccurrenceFromEdgeWitnessWithTraceReadModel
                traceCfg
                generalizeAtWith
                readModel
                gaParents
                mSchemeInfo
                mTrace
                ew

phiFromEdgeWitnessWithTraceReadModel
    :: TraceConfig
    -> GeneralizeAtWith p
    -> ElabReadModel p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTraceReadModel traceCfg generalizeAtWith readModel gaParents mSchemeInfo mTrace ew =
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
        traceCfg
        generalizeAtWith
        readModel
        gaParents
        mSchemeInfo
        IntMap.empty
        mTrace
        ew

-- | Read-model translation with exact endpoints already constructed by
-- sibling edges.  The map is occurrence-local and keyed by frozen node id;
-- it is construction evidence for Graft operands, not a solved-graph cache.
phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
    :: TraceConfig
    -> GeneralizeAtWith p
    -> ElabReadModel p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> IntMap.IntMap ElabType
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints traceCfg generalizeAtWith readModel gaParents mSchemeInfo frozenEndpointTypes mTrace ew =
    occurrenceComputationInstantiation
        <$> phiOccurrenceFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
            traceCfg
            generalizeAtWith
            readModel
            gaParents
            mSchemeInfo
            frozenEndpointTypes
            Nothing
            mTrace
            ew

-- | Frozen-endpoint translation with application-owned endpoint-shape
-- authority.  Only application construction should call this entry point;
-- ordinary replay retains the shape-neutral API above.
phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor
    :: TraceConfig
    -> GeneralizeAtWith p
    -> ElabReadModel p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> IntMap.IntMap ElabType
    -> PhiEndpointShapeAuthority
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor traceCfg generalizeAtWith readModel gaParents mSchemeInfo frozenEndpointTypes endpointShapeAuthority mTrace ew =
    occurrenceComputationInstantiation
        <$> phiOccurrenceFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
            traceCfg
            generalizeAtWith
            readModel
            gaParents
            mSchemeInfo
            frozenEndpointTypes
            (Just endpointShapeAuthority)
            mTrace
            ew

-- | Read-model variant of 'phiOccurrenceFromEdgeWitnessWithTrace'.
phiOccurrenceFromEdgeWitnessWithTraceReadModel
    :: TraceConfig
    -> GeneralizeAtWith p
    -> ElabReadModel p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError OccurrenceComputation
phiOccurrenceFromEdgeWitnessWithTraceReadModel traceCfg generalizeAtWith readModel gaParents mSchemeInfo mTrace ew =
    phiOccurrenceFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
        traceCfg
        generalizeAtWith
        readModel
        gaParents
        mSchemeInfo
        IntMap.empty
        Nothing
        mTrace
        ew

phiOccurrenceFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
    :: TraceConfig
    -> GeneralizeAtWith p
    -> ElabReadModel p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> IntMap.IntMap ElabType
    -> Maybe PhiEndpointShapeAuthority
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError OccurrenceComputation
phiOccurrenceFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints traceCfg generalizeAtWith readModel gaParents mSchemeInfo frozenEndpointTypes endpointShapeAuthority mTrace ew =
    case mTrace of
        Nothing -> Left (MissingEdgeTrace (ewEdgeId ew))
        Just _ -> do
            phiReadModel <- buildPhiReadModel readModel
            phiFromEdgeWitnessCore traceCfg generalizeAtWith phiReadModel gaParents mSchemeInfo frozenEndpointTypes endpointShapeAuthority mTrace ew

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
    -> GeneralizeAtWith p
    -> PhiReadModel p
    -> GaBindParents p
    -> Maybe SchemeInfo
    -> IntMap.IntMap ElabType
    -> Maybe PhiEndpointShapeAuthority
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError OccurrenceComputation
phiFromEdgeWitnessCore traceCfg generalizeAtWith phiReadModel gaParents mSchemeInfo frozenEndpointTypes endpointShapeAuthority mTrace ew = do
    let namedSet0 = ermNamedNodes readModel
    case if tcGeneralize traceCfg
        then
            debugPhi
                ("phi ewLeft=" ++ show (ewLeft ew)
                    ++ " ewRight=" ++ show (ewRight ew)
                )
                ()
        else () of
        () -> pure ()
    case if tcGeneralize traceCfg
        then
            debugPhi
                ("phi ewRootType=" ++ show (reifyDebugType (ewRoot ew))
                    ++ " ewLeftType=" ++ show (reifyDebugType (ewLeft ew))
                    ++ " ewRightType=" ++ show (reifyDebugType (ewRight ew))
                )
                ()
        else () of
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
                        EdgeSourceInterior (InteriorNodes s) ->
                            IntSet.fromList
                                [ getNodeId (canonicalNode (NodeId key))
                                | key <- IntSet.toList s
                                ]
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
                    let schemeArity = length (schemeBinderRefs (siScheme si))
                        traceBinderKeys =
                            maybe
                                IntSet.empty
                                (IntSet.fromList . map (getNodeId . fst) . etBinderArgs)
                                mTrace
                        schemeBinderKeys = schemeInfoBinderIdentityKeySet si
                        traceDomainCovered =
                            traceBinderKeys `IntSet.isSubsetOf` schemeBinderKeys
                    in case mTrace of
                        Just tr
                            | isStrictReplayContract (etReplayContract tr) ->
                                case replaySpineBinderArgs si tr of
                                    Just [] -> Just si
                                    Just _
                                        | schemeInfoAlreadyInReplayDomain si tr -> Just si
                                    _ -> Nothing
                        _
                            | (schemeArity == 0 && not (IntSet.null traceBinderKeys))
                                || not traceDomainCovered -> Nothing
                            | otherwise -> Just si
                Nothing -> Nothing
    case debugPhi
        ("phi scheme replay-subst edge=" ++ show (ewEdgeId ew)
            ++ " subst=" ++ show (fmap schemeInfoBinderRefSubst mSchemeInfoReplaySeed)
        )
        () of
        () -> pure ()
    siReplay <-
        case mTrace of
            Just tr
                | isStrictReplayContract (etReplayContract tr) ->
                    case mSchemeInfo of
                        Just supplied
                            | null (schemeSpineBinderRefs (siScheme supplied))
                            , null
                                ( freeTypeVarRefsType
                                    (schemeToType (siScheme supplied))
                                )
                            , replaySpineBinderArgs supplied tr == Just []
                            , suppliedOccurrenceMatchesEndpoint supplied -> do
                                -- A closed monomorphic occurrence already is
                                -- the complete source computation.  The strict
                                -- trace still validates its frozen replay
                                -- domain, but rebuilding the producer root can
                                -- expose an owner-local graph placeholder that
                                -- is absent from this occurrence.  The checked
                                -- producer is selected only when its outer
                                -- type shape also satisfies the application
                                -- endpoint owned by the caller; otherwise the
                                -- producer-root path below reconstructs the
                                -- computation and fails closed.
                                validateStrictReplayTraceDomain supplied tr
                                pure supplied
                        _ -> do
                            sourceSchemeInfo <-
                                case mSchemeInfo of
                                    Just supplied -> pure supplied
                                    Nothing -> schemeInfoForRoot (ewRoot ew)
                            producerSource <- schemeInfoForRoot (ewRoot ew)
                            validateStrictReplayTraceDomain producerSource tr
                            case replaySpineBinderArgs producerSource tr of
                                Just [] -> pure sourceSchemeInfo
                                Just _ ->
                                    case mSchemeInfoReplaySeed of
                                        Just replayReady
                                            | schemeInfoAlreadyInReplayDomain replayReady tr ->
                                                    pure replayReady
                                        _ -> do
                                            -- A reduced consumer presentation may omit
                                            -- producer binders.  Rename the producer's
                                            -- existing binders in place; never infer a
                                            -- spine from the broader frozen trace.
                                            case transportSchemeInfoToReplayDomain producerSource tr of
                                                Just transported -> pure transported
                                                Nothing ->
                                                    Left $
                                                        PhiInvariantError $
                                                            unlines
                                                                [ "strict replay producer scheme cannot be transported without changing its type tree"
                                                                , "edge: " ++ show (ewEdgeId ew)
                                                                , "consumer scheme: " ++ show (schemeToType (siScheme sourceSchemeInfo))
                                                                , "producer scheme: " ++ show (schemeToType (siScheme producerSource))
                                                                , "producer replay refs: " ++ show (schemeSpineBinderRefs (siScheme producerSource))
                                                                , "producer subst: " ++ show (schemeInfoBinderRefSubst producerSource)
                                                                , "trace binder args: " ++ show (etBinderArgs tr)
                                                                , "classified replay spine: " ++ show (replaySpineBinderArgs producerSource tr)
                                                                , "producer replay domain: " ++ show (producerReplayDomain producerSource tr)
                                                                ]
                                Nothing ->
                                    Left $
                                        PhiInvariantError $
                                            unlines
                                                [ "strict replay trace cannot be classified against the producer type tree"
                                                , "edge: " ++ show (ewEdgeId ew)
                                                , "producer scheme: " ++ show (schemeToType (siScheme producerSource))
                                                , "producer subst: " ++ show (schemeInfoBinderRefSubst producerSource)
                                                , "trace binder args: " ++ show (etBinderArgs tr)
                                                , "trace replay map: " ++ show (IntMap.toList (etBinderReplayMap tr))
                                                ]
            _ ->
                case mSchemeInfoReplaySeed of
                    Nothing -> schemeInfoForRoot (ewRoot ew)
                    Just si -> pure si
    let replayContract =
            maybe ReplayContractNone etReplayContract mTrace
    (traceBinderSourcesRaw, traceBinderReplayMapRaw, traceBinderMapDomainRaw) <-
        computeTraceBinderReplayBridge mTrace replayContract siReplay
    replaySpineSourcesRaw <-
        case mTrace of
            Just tr
                | isStrictReplayContract replayContract ->
                    case replaySpineBinderArgs siReplay tr of
                        Just binderArgs ->
                            pure
                                ( IntSet.fromList
                                    [ getNodeId sourceBinder
                                    | (_producerBinder, sourceBinder, _argument) <- binderArgs
                                    ]
                                )
                        Nothing ->
                            Left $
                                PhiInvariantError $
                                    "transported replay scheme lost its producer-spine classification"
            _ -> pure traceBinderSourcesRaw
    let traceBinderSources =
            debugPhi
                ("phi traceBinderSources=" ++ show (IntSet.toList traceBinderSourcesRaw))
                traceBinderSourcesRaw
        replaySpineSources =
            debugPhi
                ("phi replaySpineSources=" ++ show (IntSet.toList replaySpineSourcesRaw))
                replaySpineSourcesRaw
        traceBinderReplayMap =
            debugPhi
                ("phi traceBinderReplayMap=" ++ show (IntMap.toList traceBinderReplayMapRaw))
                traceBinderReplayMapRaw
        traceBinderMapDomain =
            debugPhi
                ("phi traceBinderMapDomain=" ++ show (IntSet.toList traceBinderMapDomainRaw))
                traceBinderMapDomainRaw
    phiWithSchemeOmegaOccurrence
        (omegaCtx (Just siReplay) traceBinderSources replaySpineSources traceBinderReplayMap traceBinderMapDomain replayContract)
        namedSet
        siReplay
        introCount
        ops
  where
    readModel = phiReadModelElabReadModel phiReadModel
    debugPhi :: String -> a -> a
    debugPhi = traceGeneralize traceCfg

    suppliedOccurrenceMatchesEndpoint :: SchemeInfo -> Bool
    suppliedOccurrenceMatchesEndpoint supplied =
        case endpointShapeAuthority of
            Nothing -> True
            Just authority ->
                let suppliedTy = schemeToType (siScheme supplied)
                    endpointTy = pesaRequiredEndpointType authority
                in endpointShapeCompatible
                        (pesaOccurrenceRole authority)
                        suppliedTy
                        endpointTy

    endpointShapeCompatible
        :: PhiOccurrenceRole
        -> ElabType
        -> ElabType
        -> Bool
    endpointShapeCompatible role suppliedTy endpointTy =
        case role of
            PhiApplicationFunctionOccurrence ->
                outerShape suppliedTy == Just PhiArrowShape
                    && outerShape endpointTy == Just PhiArrowShape
            PhiApplicationArgumentOccurrence ->
                case outerShape endpointTy of
                    Nothing -> True
                    Just endpointShape ->
                        outerShape suppliedTy == Just endpointShape

    -- A graph variable or bottom endpoint has not supplied a structural
    -- promise, so it cannot contradict a checked closed producer.  In
    -- particular, do not unfold mu here: a recursive data value whose
    -- representation contains arrows is still not a callable occurrence.
    outerShape :: ElabType -> Maybe PhiOuterShape
    outerShape ty =
        case ty of
            TArrow {} -> Just PhiArrowShape
            TMuRef {} -> Just PhiMuShape
            TBaseWithIdentity identity _ -> Just (PhiBaseShape identity)
            TConWithIdentity identity _ _ ->
                Just (PhiConstructorShape identity)
            TVarAppRef {} -> Just PhiVarApplicationShape
            TForallRef _ _ body -> outerShape body
            TVarRef {} -> Nothing
            TBottom -> Nothing

    omegaCtx mSchemeInfoCtx traceBinderSources replaySpineSources traceBinderReplayMap traceBinderMapDomain replayContractCtx =
        OmegaContext
            { ocTraceConfig = traceCfg
            , ocPresolutionView = presolutionView
            , ocFrozenEndpointTypes = frozenEndpointTypes
            , ocReifyBoundWithRefs = reifyBoundWithRefsAt
            , ocReifyTypeWithNamedSetRefsNoFallback = reifyTypeWithNamedSetRefsNoFallbackAt
            , ocCopyMap = copyMap
            , ocGaParents = gaParents
            , ocTrace = mTrace
            , ocSchemeInfo = mSchemeInfoCtx
            , ocTraceBinderSources = traceBinderSources
            , ocReplaySpineSources = replaySpineSources
            , ocTraceBinderReplayMap = traceBinderReplayMap
            , ocTraceBinderMapDomain = traceBinderMapDomain
            , ocReplayContract = replayContractCtx
            , ocEdgeRoot = ewRoot ew
            , ocEdgeLeft = ewLeft ew
            , ocEdgeRight = ewRight ew
            }

    canonicalNode :: NodeId -> NodeId
    canonicalNode = pvCanonical presolutionView

    presolutionView = ermPresolutionView readModel

    constraint = pvConstraint presolutionView

    reifyDebugType :: NodeId -> Either ElabError ElabType
    reifyDebugType = reifyType presolutionView

    reifyBoundWithRefsAt
        :: IntMap.IntMap TypeBinderRef
        -> NodeId
        -> Either ElabError ElabType
    reifyBoundWithRefsAt = reifyBoundWithRefsReadModel readModel

    reifyTypeWithNamedSetRefsNoFallbackAt
        :: IntMap.IntMap TypeBinderRef
        -> IntSet.IntSet
        -> NodeId
        -> Either ElabError ElabType
    reifyTypeWithNamedSetRefsNoFallbackAt = reifyTypeWithNamedSetRefsNoFallbackReadModel readModel

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
                    replayMapDomain = IntSet.fromAscList (IntMap.keys replayMapRaw)
                    replayBinderDomainRaw =
                        case etReplayDomainBinders tr of
                            replayBinders@(_ : _) ->
                                IntSet.fromList (map getNodeId replayBinders)
                            [] ->
                                schemeInfoBinderIdentityKeySet siReplay
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
                        if IntMap.null replayMapRaw
                            then Right (traceBinderSourceSet, IntMap.empty, IntSet.empty)
                            else
                                Left $
                                    PhiInvariantError $
                                        unlines
                                            [ "trace replay map expected empty under ReplayContractNone"
                                            , "edge: " ++ show (ewEdgeId ew)
                                            , "trace binder sources: " ++ show traceBinderSourceKeys
                                            , "replay-map domain: " ++ show (IntMap.keys replayMapRaw)
                                            ]

    -- Validate the producer-owned replay bridge before transporting any
    -- consumer SchemeInfo into that key space.  Otherwise a malformed replay
    -- target can accidentally become valid merely because reconstruction
    -- manufactures a binder carrying the malformed target identity.
    validateStrictReplayTraceDomain :: SchemeInfo -> EdgeTrace -> Either ElabError ()
    validateStrictReplayTraceDomain sourceSchemeInfo tr = do
        let traceSourceKeys = map (getNodeId . fst) (etBinderArgs tr)
            traceSourceSet = IntSet.fromList traceSourceKeys
            replayMap = etBinderReplayMap tr
            replayMapDomain = IntSet.fromAscList (IntMap.keys replayMap)
            replayDomain =
                case etReplayDomainBinders tr of
                    replayBinders@(_ : _) ->
                        IntSet.fromList (map getNodeId replayBinders)
                    [] -> schemeInfoBinderIdentityKeySet sourceSchemeInfo
            missingSources =
                IntSet.toList (IntSet.difference traceSourceSet replayMapDomain)
            extraSources =
                IntSet.toList (IntSet.difference replayMapDomain traceSourceSet)
        if not (null missingSources) || not (null extraSources)
            then
                Left $
                    PhiInvariantError $
                        unlines
                            [ "trace binder replay-map domain mismatch"
                            , "edge: " ++ show (ewEdgeId ew)
                            , "trace binder sources: " ++ show traceSourceKeys
                            , "replay-map domain: " ++ show (IntMap.keys replayMap)
                            , "missing source keys: " ++ show missingSources
                            , "extra source keys: " ++ show extraSources
                            ]
            else mapM_ (validateTarget replayMap replayDomain) traceSourceKeys
      where
        validateTarget replayMap replayDomain sourceKey =
            case IntMap.lookup sourceKey replayMap of
                Nothing ->
                    Left $
                        PhiInvariantError $
                            "trace binder replay-map missing source key after domain validation: "
                                ++ show sourceKey
                Just replayTarget
                    | IntSet.member (getNodeId replayTarget) replayDomain -> pure ()
                    | otherwise ->
                        Left $
                            PhiInvariantError $
                                unlines
                                    [ "replay-map target outside replay binder domain"
                                    , "edge: " ++ show (ewEdgeId ew)
                                    , "source key: " ++ show sourceKey
                                    , "replay target: " ++ show replayTarget
                                    , "replay binder domain: " ++ show (IntSet.toList replayDomain)
                                    ]

    schemeInfoForRoot :: NodeId -> Either ElabError SchemeInfo
    schemeInfoForRoot root0 = do
        let rootC = canonicalNode root0
        case pvLookupNode presolutionView rootC of
            Just TyForall {} -> do
                ty <- reifyDebugType rootC
                pure (schemeInfoFromRefSubst (schemeFromType ty) IntMap.empty)
            _ -> do
                let targetNode =
                        case pvLookupVarBound presolutionView rootC of
                            Just bnd ->
                                case pvLookupNode presolutionView bnd of
                                    Just TyForall{ tnBody = body } -> canonicalNode body
                                    _ -> canonicalNode bnd
                            Nothing -> schemeBodyTarget presolutionView root0
                scopeRoot <- instScopeRoot root0
                (sch, subst) <-
                    generalizeAtWith (Just gaParents) scopeRoot targetNode
                pure (schemeInfoFromRefSubst sch subst)

    producerReplayDomain :: SchemeInfo -> EdgeTrace -> Maybe [NodeId]
    producerReplayDomain sourceSchemeInfo tr = do
        binderArgs <- replaySpineBinderArgs sourceSchemeInfo tr
        traverse
            (\(_producerBinder, sourceBinder, _argument) ->
                IntMap.lookup
                    (getNodeId sourceBinder)
                    (etBinderReplayMap tr)
            )
            binderArgs

    schemeInfoAlreadyInReplayDomain :: SchemeInfo -> EdgeTrace -> Bool
    schemeInfoAlreadyInReplayDomain schemeInfo tr =
        case replaySpineBinderArgs schemeInfo tr of
            Just bindings@(_ : _) ->
                all
                    (\(producerBinder, sourceBinder, _argument) ->
                        IntMap.lookup
                            (getNodeId sourceBinder)
                            (etBinderReplayMap tr)
                            == Just producerBinder
                    )
                    bindings
            _ -> False

    -- The producer type tree, rather than frozen I(r), owns the quantifier
    -- spine.  `etBinderArgs` is deliberately broader: it can retain a source
    -- interior node solely because Omega needs its construction authority.
    -- Classify only the trace entries that correspond to actual producer
    -- quantifiers, in type-tree order.  A strict trace may legitimately cover
    -- just part of the producer spine: untouched quantifiers keep their
    -- producer identities, while extra trace entries can name interior nodes
    -- solely for Omega construction authority.  Ambiguous classification
    -- still fails closed.
    --
    -- Each result is @(producer binder, frozen source binder, argument)@.
    replaySpineBinderArgs :: SchemeInfo -> EdgeTrace -> Maybe [(NodeId, NodeId, NodeId)]
    replaySpineBinderArgs sourceSchemeInfo tr = do
        producerDomain <- schemeInfoSpineDomain sourceSchemeInfo
        traceEntries <- uniqueTraceEntries (etBinderArgs tr)
        classified <- catMaybes <$> traverse (traceEntryFor traceEntries) producerDomain
        let classifiedSources =
                IntSet.fromList
                    [ getNodeId sourceBinder
                    | (_producerBinder, sourceBinder, _argument) <- classified
                    ]
        if IntSet.size classifiedSources == length classified
            then Just classified
            else Nothing
      where
        replayMap = etBinderReplayMap tr

        traceEntryFor traceEntries producerBinder =
            case retainedProducerEntries traceEntries producerBinder of
                -- The solved/base quotient plus normalized Merge orientation
                -- is the producer-owned construction certificate.  Prefer it
                -- to live union-find equality: multiple frozen source nodes
                -- can project to one producer binder, while OpMerge records
                -- exactly which one survives as the quantified source.
                [entry] -> Just (Just (classifiedEntry producerBinder entry))
                [] ->
                    case
                        [ entry
                        | entry@(sourceBinder, _argument) <- traceEntries
                        , sourceBinder == producerBinder
                            -- EdgeTrace deliberately freezes the
                            -- pre-materialization source identity. SchemeInfo
                            -- is reconstructed from the finalized producer
                            -- tree, so its binder may be the final quotient
                            -- representative of that exact source node.
                            || canonicalNode sourceBinder
                                == canonicalNode producerBinder
                            || producerBaseSource producerBinder == Just sourceBinder
                            || IntMap.lookup (getNodeId sourceBinder) replayMap
                                == Just producerBinder
                        ]
                    of
                        [entry] -> Just (Just (classifiedEntry producerBinder entry))
                        [] -> Just Nothing
                        _ -> Nothing
                -- More than one non-eliminated member of the producer's base
                -- class means the normalized witness did not select a unique
                -- source representative.  Fail closed instead of choosing by
                -- trace order or NodeId.
                _ -> Nothing

        classifiedEntry producerBinder (sourceBinder, argument) =
            (producerBinder, sourceBinder, argument)

        retainedProducerEntries traceEntries producerBinder =
            case producerBaseIdentity producerBinder of
                Just producerBase ->
                    let baseEntries =
                            [ entry
                            | entry@(sourceBinder, _argument) <- traceEntries
                            , producerBaseIdentity sourceBinder == Just producerBase
                            ]
                    in case baseEntries of
                        -- A unique member is the exact source certificate for
                        -- this producer quantifier even when a later,
                        -- cross-base Merge consumes that quantifier as an
                        -- actual instantiation computation.
                        [entry] -> [entry]
                        -- Merge orientation disambiguates only duplicate
                        -- aliases inside this same prepared base class.  A
                        -- cross-base Merge relates distinct producer
                        -- quantifiers and must not erase either classification.
                        _ ->
                            [ entry
                            | entry@(sourceBinder, _argument) <- baseEntries
                            , IntSet.notMember
                                (getNodeId sourceBinder)
                                (sameBaseMergeEliminatedSources producerBase)
                            ]
                Nothing -> []

        producerBaseIdentity binder = do
            case IntMap.lookup (getNodeId binder) (gaSolvedToBase gaParents) of
                Just baseBinder -> Just baseBinder
                Nothing ->
                    case NodeAccess.lookupNode (gaBaseConstraint gaParents) binder of
                        Just _ -> Just binder
                        Nothing -> Nothing

        sameBaseMergeEliminatedSources producerBase =
            IntSet.fromList
                [ getNodeId operated
                | OpMerge operated retained <- getInstanceOps (ewWitness ew)
                , producerBaseIdentity operated == Just producerBase
                , producerBaseIdentity retained == Just producerBase
                ]

        producerBaseSource producerBinder = do
            IntMap.lookup
                (getNodeId producerBinder)
                (gaSolvedToBase gaParents)

        uniqueTraceEntries = go IntMap.empty []
          where
            go _ entries [] = Just (reverse entries)
            go seen entries (entry@(sourceBinder, argument) : rest) =
                let sourceKey = getNodeId sourceBinder
                in case IntMap.lookup sourceKey seen of
                    Nothing ->
                        go
                            (IntMap.insert sourceKey argument seen)
                            (entry : entries)
                            rest
                    Just previousArgument
                        | previousArgument == argument -> go seen entries rest
                        | otherwise -> Nothing

    -- Omega operates on the leading quantifier spine represented by VSpine.
    -- Quantifiers nested in a bound, arrow component, constructor argument, or
    -- recursive body are not members of that spine and must not consume trace
    -- entries here.  A SchemeInfo may represent some leading quantifiers in
    -- its Scheme prefix and retain others at the head of the Scheme body, so
    -- account for both presentations in outermost-first order.
    schemeSpineBinderRefs :: ElabScheme -> [TypeBinderRef]
    schemeSpineBinderRefs scheme =
        map fst (schemeBinderRefs scheme)
            ++ leadingBodyRefs (schemeBody scheme)
      where
        leadingBodyRefs ty =
            case ty of
                TForallRef ref _bound body -> ref : leadingBodyRefs body
                _ -> []

    schemeInfoSpineDomain :: SchemeInfo -> Maybe [NodeId]
    schemeInfoSpineDomain schemeInfo =
        traverse refDomain (schemeSpineBinderRefs (siScheme schemeInfo))
      where
        subst = schemeInfoBinderRefSubst schemeInfo
        refDomain ref =
            typeBinderRefNode ref
                <|> case
                    [ NodeId key
                    | (key, substRef) <- IntMap.toList subst
                    , typeBinderRefsSameIdentity ref substRef
                    ] of
                    [node] -> Just node
                    _ -> Nothing

    -- Transport a producer scheme into the producer-approved replay key space.
    -- This is an injective identity-preserving alpha-renaming of exactly the
    -- replay-covered quantifiers.  Untouched producer quantifiers retain their
    -- identities; trace-only interior nodes never become quantifiers.
    transportSchemeInfoToReplayDomain :: SchemeInfo -> EdgeTrace -> Maybe SchemeInfo
    transportSchemeInfoToReplayDomain supplied tr = do
        classifiedBinderArgs <- replaySpineBinderArgs supplied tr
        sourceSpineDomain <- schemeInfoSpineDomain supplied
        let sourceBinders = schemeBinderRefs (siScheme supplied)
            sourceSpineRefs = schemeSpineBinderRefs (siScheme supplied)
            sourceSubst = schemeInfoBinderRefSubst supplied
        if length sourceSpineRefs == length sourceSpineDomain
            then pure ()
            else Nothing
        renamesWithNodes <-
            traverse
                (\(producerBinder, sourceBinder, _argument) -> do
                    sourceRef <-
                        case
                            [ ref
                            | (ref, domainBinder) <- zip sourceSpineRefs sourceSpineDomain
                            , domainBinder == producerBinder
                            ]
                        of
                            [ref] -> Just ref
                            _ -> Nothing
                    replayBinder <-
                        IntMap.lookup
                            (getNodeId sourceBinder)
                            (etBinderReplayMap tr)
                    let replayRef =
                            typeBinderRefFromIdentity
                                (typeBinderIdentityFromNode replayBinder)
                                (typeBinderRefName sourceRef)
                    pure (producerBinder, replayBinder, sourceRef, replayRef)
                )
                classifiedBinderArgs
        let replayKeys =
                IntSet.fromList
                    [ getNodeId replayBinder
                    | (_producerBinder, replayBinder, _sourceRef, _replayRef) <- renamesWithNodes
                    ]
            touchedSourceRefs =
                [ sourceRef
                | (_producerBinder, _replayBinder, sourceRef, _replayRef) <- renamesWithNodes
                ]
            replayRefs =
                [ replayRef
                | (_producerBinder, _replayBinder, _sourceRef, replayRef) <- renamesWithNodes
                ]
            untouchedRefs =
                filter
                    (\ref ->
                        not
                            ( any
                                (typeBinderRefsSameIdentity ref)
                                touchedSourceRefs
                            )
                    )
                    (typeBinderRefsInType (schemeToType (siScheme supplied)))
            replayCapturesUntouchedIdentity =
                any
                    (\replayRef ->
                        any
                            (typeBinderRefsSameIdentity replayRef)
                            untouchedRefs
                    )
                    replayRefs
        if IntSet.size replayKeys == length renamesWithNodes
            && not replayCapturesUntouchedIdentity
            then pure ()
            else Nothing
        let renames =
                [ (sourceRef, replayRef)
                | (_producerBinder, _replayBinder, sourceRef, replayRef) <- renamesWithNodes
                ]
            renameType = renameTypeBinderIdentities renames
            replayRefFor sourceRef =
                snd
                    <$> listToMaybe
                        [ pair
                        | pair@(candidate, _) <- renames
                        , typeBinderRefsSameIdentity sourceRef candidate
                        ]
            replaySpecs =
                [ ( fromMaybe sourceRef (replayRefFor sourceRef)
                  , fmap (mapBoundType renameType) mbBound
                  )
                | (sourceRef, mbBound) <- sourceBinders
                ]
            replayScheme =
                mkElabSchemeWithRefs
                    replaySpecs
                    (renameType (schemeBody (siScheme supplied)))
            replaySubst =
                IntMap.union
                    ( IntMap.fromList
                        [ (getNodeId replayBinder, replayRef)
                        | (_producerBinder, replayBinder, _sourceRef, replayRef) <- renamesWithNodes
                        ]
                    )
                    (IntMap.map
                        (\sourceRef -> fromMaybe sourceRef (replayRefFor sourceRef))
                        sourceSubst
                    )
        pure (schemeInfoFromRefSubst replayScheme replaySubst)

    -- Apply a binder-identity renaming simultaneously.  Sequential
    -- substitution collapses valid injective swaps such as @a -> b, b -> a@
    -- and can capture later sources before they are visited.
    renameTypeBinderIdentities :: [(TypeBinderRef, TypeBinderRef)] -> ElabType -> ElabType
    renameTypeBinderIdentities renames ty =
        case ty of
            TVarRef ref -> TVarRef (renameRef ref)
            TArrow domain codomain ->
                TArrow
                    (renameTypeBinderIdentities renames domain)
                    (renameTypeBinderIdentities renames codomain)
            TConWithIdentity identity con args ->
                TConWithIdentity
                    identity
                    con
                    (fmap (renameTypeBinderIdentities renames) args)
            TVarAppRef ref args ->
                TVarAppRef
                    (renameRef ref)
                    (fmap (renameTypeBinderIdentities renames) args)
            TBaseWithIdentity {} -> ty
            TForallRef ref mbBound body ->
                TForallRef
                    (renameRef ref)
                    (fmap (mapBoundType (renameTypeBinderIdentities renames)) mbBound)
                    (renameTypeBinderIdentities renames body)
            TMuRef ref body ->
                TMuRef
                    (renameRef ref)
                    (renameTypeBinderIdentities renames body)
            TBottom -> TBottom
      where
        renameRef ref =
            fromMaybe ref $
                snd
                    <$> listToMaybe
                        [ pair
                        | pair@(sourceRef, _replayRef) <- renames
                        , typeBinderRefsSameIdentity sourceRef ref
                        ]

    typeBinderRefsInType :: ElabType -> [TypeBinderRef]
    typeBinderRefsInType ty =
        case ty of
            TVarRef ref -> [ref]
            TArrow domain codomain ->
                typeBinderRefsInType domain ++ typeBinderRefsInType codomain
            TConWithIdentity _identity _con args ->
                concatMap typeBinderRefsInType args
            TVarAppRef ref args ->
                ref : concatMap typeBinderRefsInType args
            TBaseWithIdentity {} -> []
            TForallRef ref mbBound body ->
                ref
                    : maybe [] (typeBinderRefsInType . tyToElab) mbBound
                    ++ typeBinderRefsInType body
            TMuRef ref body -> ref : typeBinderRefsInType body
            TBottom -> []

    instScopeRoot :: NodeId -> Either ElabError NodeRef
    instScopeRoot root0 =
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
                IntMap.lookup (getNodeId rootC) (gaSolvedToBase gaParents)
                    <|> baseFromTrace
        in case baseRep of
            Nothing -> goScope IntSet.empty (typeRef rootC)
            Just baseN ->
                case bindingPathToRootLocal (gaBindParentsBase gaParents) (typeRef baseN) of
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
