{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.WitnessNorm
Description : Witness normalization for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module handles normalization of edge witnesses against the finalized
presolution constraint.
-}
module MLF.Constraint.Presolution.WitnessNorm (
    normalizeEdgeWitnessesM
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (forM, foldM, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe, mapMaybe)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.Validation (translatableWeakenedNodes)
import MLF.Constraint.Presolution.Witness (
    normalizeInstanceOpsCore,
    OmegaNormalizeEnv(OmegaNormalizeEnv, oneRoot),
    validateNormalizedWitness,
    )
import qualified MLF.Constraint.Presolution.Witness as Witness

-- | Normalize edge witnesses against the finalized presolution constraint.
normalizeEdgeWitnessesM :: PresolutionM ()
normalizeEdgeWitnessesM = do
    (c0, canonical) <- getConstraintAndCanonical
    traces <- gets psEdgeTraces
    witnesses0 <- gets psEdgeWitnesses
    let rewriteNodeWith copyMap nid =
            let mapped = IntMap.findWithDefault nid (getNodeId nid) (getCopyMapping copyMap)
                mappedC = canonical mapped
                sourceC = canonical nid
                isLive n =
                    case NodeAccess.lookupNode c0 n of
                        Just _ -> True
                        _ -> False
            in if isLive mappedC
                then mappedC
                else if isLive sourceC
                    then sourceC
                    else mappedC
        weakenedOps =
            IntSet.fromList
                [ getNodeId (canonical (rewriteNodeWith copyMap n))
                | (eid, w0) <- IntMap.toList witnesses0
                , let copyMap = maybe mempty etCopyMap (IntMap.lookup eid traces)
                , OpWeaken n <- getInstanceOps (ewWitness w0)
                ]
        weakened =
            IntSet.union weakenedOps (translatableWeakenedNodes c0)
    witnessResults <- forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
        let mbTrace = IntMap.lookup eid traces
            (edgeRoot, copyMap, binderArgs0, traceInterior) =
                case mbTrace of
                    Nothing -> (ewRoot w0, mempty, [], mempty)
                    Just tr ->
                        ( etRoot tr
                        , etCopyMap tr
                        , etBinderArgs tr
                        , etInterior tr
                        )
            rewriteNode = rewriteNodeWith copyMap
            binderArgs =
                IntMap.fromList
                    [ ( getNodeId (canonical (rewriteNode bv))
                      , canonical (rewriteNode arg)
                      )
                    | (bv, arg) <- binderArgs0
                    ]
            copyToOriginal =
                IntMap.fromListWith min
                    [ (getNodeId copy, NodeId orig)
                    | (orig, copy) <- IntMap.toList (getCopyMapping copyMap)
                    ]
            restoreNode nid =
                IntMap.findWithDefault nid (getNodeId nid) copyToOriginal
            rewriteOp op =
                case op of
                    OpGraft sigma n -> OpGraft (rewriteNode sigma) (rewriteNode n)
                    OpMerge n m -> OpMerge (rewriteNode n) (rewriteNode m)
                    OpRaise n -> OpRaise (rewriteNode n)
                    OpWeaken n -> OpWeaken (rewriteNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (rewriteNode n) (rewriteNode m)
            restoreOp op =
                case op of
                    OpGraft sigma n -> OpGraft (restoreNode sigma) (restoreNode n)
                    OpMerge n m -> OpMerge (restoreNode n) (restoreNode m)
                    OpRaise n -> OpRaise (restoreNode n)
                    OpWeaken n -> OpWeaken (restoreNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (restoreNode n) (restoreNode m)
            isExactTyVar nid =
                case NodeAccess.lookupNode c0 nid of
                    Just TyVar{} -> True
                    _ -> False
            dedupNodes =
                reverse
                    . snd
                    . foldl'
                        (\(seen, acc) nid ->
                            let key = getNodeId nid
                            in if IntSet.member key seen
                                then (seen, acc)
                                else (IntSet.insert key seen, nid : acc)
                        )
                        (IntSet.empty, [])
            sourceEntriesInOrder :: [(NodeId, NodeId)]
            sourceEntriesInOrder =
                reverse $
                    snd $
                        foldl'
                            (\(seen, acc) (sourceBinder, arg) ->
                                let key = getNodeId sourceBinder
                                in if IntSet.member key seen
                                    then (seen, acc)
                                    else (IntSet.insert key seen, (sourceBinder, arg) : acc)
                            )
                            (IntSet.empty, [])
                            binderArgs0
            sourceBindersInOrder = map fst sourceEntriesInOrder
            traceInteriorKeys =
                case traceInterior of
                    InteriorNodes s -> s
        let orderBase = edgeRoot
            orderRoot = orderBase
        interiorExact <-
            if IntSet.null traceInteriorKeys
                then edgeInteriorExact edgeRoot
                else pure traceInteriorKeys
        let interiorNorm =
                -- Rewrite interior through copyMap so it's in the same node-id space
                -- as the rewritten witness steps (thesis-exact I(r) membership).
                IntSet.fromList
                    [ getNodeId (canonical (rewriteNode (NodeId n)))
                    | n <- IntSet.toList interiorExact
                    ]
            sourceReplayCandidates =
                [ sourceBinder
                | sourceBinder <- sourceBindersInOrder
                , isExactTyVar sourceBinder
                ]
                    ++
                [ canonical (rewriteNode sourceBinder)
                | sourceBinder <- sourceBindersInOrder
                , isExactTyVar (canonical (rewriteNode sourceBinder))
                ]
            argReplayCandidates =
                [ canonical (rewriteNode arg)
                | (_sourceBinder, arg) <- binderArgs0
                , isExactTyVar (canonical (rewriteNode arg))
                ]
            interiorReplayCandidates =
                [ canonical (NodeId key)
                | key <- IntSet.toList interiorNorm
                , isExactTyVar (canonical (NodeId key))
                ]
            replayCandidatePool =
                dedupNodes (sourceReplayCandidates ++ argReplayCandidates ++ interiorReplayCandidates)
            assignReplayMap =
                let step (sourceAcc, rewrittenAcc, usedReplay, missingSources) sourceBinder =
                        let sourceKey = getNodeId sourceBinder
                            rewrittenSource = canonical (rewriteNode sourceBinder)
                            rewrittenSourceKey = getNodeId rewrittenSource
                            sourceDirectCandidates =
                                dedupNodes
                                    ( [ sourceBinder
                                      | isExactTyVar sourceBinder
                                      ]
                                        ++ [ rewrittenSource
                                           | isExactTyVar rewrittenSource
                                           ]
                                    )
                            sourceArgCandidates =
                                dedupNodes
                                    [ canonical (rewriteNode arg)
                                    | (sourceBinder', arg) <- binderArgs0
                                    , sourceBinder' == sourceBinder
                                    , isExactTyVar (canonical (rewriteNode arg))
                                    ]
                            sourceCandidates =
                                dedupNodes
                                    ( sourceDirectCandidates
                                        ++ sourceArgCandidates
                                        ++ replayCandidatePool
                                    )
                            mbReplay =
                                case [ cand
                                     | cand <- sourceCandidates
                                     , not (IntSet.member (getNodeId cand) usedReplay)
                                     ] of
                                    replayBinder : _ -> Just replayBinder
                                    [] -> Nothing
                        in case mbReplay of
                            Nothing ->
                                ( sourceAcc
                                , rewrittenAcc
                                , usedReplay
                                , sourceBinder : missingSources
                                )
                            Just replayBinder ->
                                ( IntMap.insert sourceKey replayBinder sourceAcc
                                , IntMap.insert rewrittenSourceKey replayBinder rewrittenAcc
                                , IntSet.insert (getNodeId replayBinder) usedReplay
                                , missingSources
                                )
                    (replayMapSourceRev, replayMapRewrittenRev, _usedReplay, missingRev) =
                        foldl'
                            step
                            (IntMap.empty, IntMap.empty, IntSet.empty, [])
                            sourceBindersInOrder
                    missing = reverse missingRev
                in case missing of
                    [] -> Right (replayMapSourceRev, replayMapRewrittenRev)
                    _ -> Left (Witness.ReplayMapIncomplete missing)
        (replayMapSource, replayMapRewritten) <-
            case assignReplayMap of
                Left err -> throwError (WitnessNormalizationError (EdgeId eid) err)
                Right mapping -> pure mapping
        let interiorWithBinders =
                if IntMap.size binderArgs > 1
                    then IntSet.union interiorNorm (IntSet.fromList (IntMap.keys binderArgs))
                    else interiorNorm
            isAnnEdge =
                IntSet.member eid (cAnnEdges c0)
            replayBindersAtRoot =
                [ canonical b
                | b <- bindersOrdered
                , isExactTyVar (canonical b)
                ]
              where
                bindersOrdered =
                    either (const []) id $
                        Binding.orderedBinders canonical c0 (typeRef (canonical edgeRoot))
            isLiveNode nid =
                case NodeAccess.lookupNode c0 (canonical nid) of
                    Just _ -> True
                    _ -> False
            sourceBinderReplayKey sourceBinder =
                IntMap.lookup
                    (getNodeId (canonical (rewriteNode sourceBinder)))
                    replayMapRewritten
            replayToSourceMap =
                IntMap.fromListWith min
                    [ (getNodeId (canonical replayTarget), canonical (NodeId sourceKey))
                    | (sourceKey, replayTarget) <- IntMap.toList replayMapSource
                    ]
            argToSourceMap =
                IntMap.fromListWith min
                    [ ( getNodeId (canonical (rewriteNode arg))
                      , canonical sourceBinder
                      )
                    | (sourceBinder, arg) <- binderArgs0
                    ]
            sourceBinderFallback =
                case reverse sourceBindersInOrder of
                    sourceBinder : _ -> Just (canonical sourceBinder)
                    [] -> Nothing
            sourceBinderForOpTarget target =
                case IntMap.lookup (getNodeId (canonical target)) replayToSourceMap of
                    Just sourceBinder -> sourceBinder
                    Nothing -> canonical target
            sourceBinderForWeakenTarget target =
                case IntMap.lookup (getNodeId (canonical target)) replayToSourceMap of
                    Just sourceBinder -> sourceBinder
                    Nothing -> fromMaybe (canonical target) sourceBinderFallback
            sourceBinderForOpArg arg =
                case IntMap.lookup (getNodeId (canonical arg)) argToSourceMap of
                    Just sourceBinder -> sourceBinder
                    Nothing -> canonical arg
            replayBinderForOpTarget target =
                case IntMap.lookup (getNodeId (canonical target)) replayMapRewritten of
                    Just replay -> canonical replay
                    Nothing -> canonical target
            candidateArgsForReplay replayBinder =
                dedupNodes
                    [ canonical (rewriteNode arg)
                    | (sourceBinder, arg) <- binderArgs0
                    , Just replayBinder' <- [sourceBinderReplayKey sourceBinder]
                    , canonical replayBinder' == replayBinder
                    ]
            sourceBindersForReplay replayBinder =
                dedupNodes
                    [ sourceBinder
                    | (sourceBinder, _arg) <- binderArgs0
                    , Just replayBinder' <- [sourceBinderReplayKey sourceBinder]
                    , canonical replayBinder' == replayBinder
                    ]
            hasAliasedSourceBinder replayBinder =
                any
                    (\sourceBinder -> canonical (rewriteNode sourceBinder) /= replayBinder)
                    (sourceBindersForReplay replayBinder)
            hasExplicitWeaken binder ops =
                any
                    (\case
                        OpWeaken n -> replayBinderForOpTarget n == binder
                        _ -> False
                    )
                    ops
            graftArgsForBinder binder ops =
                dedupNodes
                    [ canonical arg
                    | OpGraft arg n <- ops
                    , replayBinderForOpTarget n == binder
                    ]
            ambiguousReplayBinders ops =
                let graftBinders =
                        dedupNodes
                            [ replayBinderForOpTarget n
                            | OpGraft _ n <- ops
                            ]
                in [ binder
                    | binder <- graftBinders
                    , not (hasExplicitWeaken binder ops)
                    , length (graftArgsForBinder binder ops) > 1
                    , length (sourceBindersForReplay binder) > 1
                    , hasAliasedSourceBinder binder
                    ]
            graftIndexPositions binder ops =
                [ idx
                | (idx, op) <- zip [0 ..] ops
                , case op of
                    OpGraft _ n -> replayBinderForOpTarget n == binder
                    _ -> False
                ]
            dropBinderGrafts binder =
                filter
                    (\case
                        OpGraft _ n -> replayBinderForOpTarget n /= binder
                        _ -> True
                    )
            synthesizeOneBinder ops binder =
                case graftIndexPositions binder ops of
                    firstIdx : _ -> do
                        let candidates = candidateArgsForReplay binder
                            liveCandidates = filter isLiveNode candidates
                            chosenArg =
                                case liveCandidates of
                                    arg : _ -> Right arg
                                    [] ->
                                        Left
                                            (Witness.DeterministicGraftWeakenSynthesisFailed
                                                binder
                                                candidates
                                            )
                        chosen <- chosenArg
                        let opsDropped = dropBinderGrafts binder ops
                            (prefix, suffix) = splitAt firstIdx opsDropped
                        pure (prefix ++ [OpGraft chosen binder, OpWeaken binder] ++ suffix)
                    [] -> Right ops
            synthesizeAmbiguousAnnotationGraftWeaken ops
                | not isAnnEdge = Right ops
                | null replayBindersAtRoot = Right ops
                | otherwise = do
                    let binders0 = ambiguousReplayBinders ops
                    foldM synthesizeOneBinder ops binders0
        ops0 <- case synthesizeAmbiguousAnnotationGraftWeaken (map rewriteOp (getInstanceOps (ewWitness w0))) of
            Left err ->
                throwError (WitnessNormalizationError (EdgeId eid) err)
            Right ops' -> pure ops'
        let
            orderKeys0 = Order.orderKeysFromConstraintWith canonical c0 (canonical orderRoot) Nothing
            orderKeys = orderKeys0
            env =
                OmegaNormalizeEnv
                    { oneRoot = canonical edgeRoot
                    , Witness.interior = interiorWithBinders
                    , Witness.weakened = weakened
                    , Witness.orderKeys = orderKeys
                    , Witness.canonical = canonical
                    , Witness.constraint = c0
                    , Witness.binderArgs = binderArgs
                    , Witness.binderReplayMap = replayMapRewritten
                    }
        opsNorm <- case normalizeInstanceOpsCore env ops0 of
            Right ops' -> pure ops'
            Left err ->
                throwError (WitnessNormalizationError (EdgeId eid) err)
        let replayBinders = replayBindersAtRoot
            graftTargetKeysNorm =
                IntSet.fromList
                    [ getNodeId (canonical target)
                    | OpGraft _ target <- opsNorm
                    ]
            hasNonPairedWeaken =
                any
                    (\case
                        OpWeaken target ->
                            IntSet.notMember (getNodeId (canonical target)) graftTargetKeysNorm
                        _ -> False
                    )
                    opsNorm
            prunePairedWeaken (OpWeaken target)
                | null replayBinders
                , hasNonPairedWeaken
                , IntSet.notMember (getNodeId (canonical target)) interiorNorm
                , IntSet.member (getNodeId (canonical target)) graftTargetKeysNorm =
                    Nothing
            prunePairedWeaken op = Just op
            opsNormPruned = mapMaybe prunePairedWeaken opsNorm
            dropNoReplayGraftWeaken op =
                case op of
                    OpGraft{} -> Nothing
                    OpWeaken{} -> Nothing
                    _ -> Just op
            stripNoReplayOp op =
                case op of
                    OpGraft{} -> Nothing
                    OpMerge{} -> Nothing
                    OpRaiseMerge{} -> Nothing
                    OpWeaken target
                        | IntSet.member (getNodeId (canonical target)) graftTargetKeysNorm ->
                            Nothing
                    _ -> Just op
            opsNormNoReplay
                | null replayBinders
                , not isAnnEdge =
                    mapMaybe stripNoReplayOp opsNormPruned
                | null replayBinders
                , not hasNonPairedWeaken =
                    mapMaybe dropNoReplayGraftWeaken opsNormPruned
                | otherwise =
                    opsNormPruned
            projectOpTargetsNoReplay op =
                let project = sourceBinderForOpTarget
                in case op of
                    OpGraft sigma n -> OpGraft (sourceBinderForOpArg sigma) (project n)
                    OpMerge n m -> OpMerge (project n) (project m)
                    OpRaise n -> OpRaise (project n)
                    OpWeaken n -> OpWeaken (sourceBinderForWeakenTarget n)
                    OpRaiseMerge n m -> OpRaiseMerge (project n) (project m)
            opsNormProjected
                | null replayBinders = map projectOpTargetsNoReplay opsNormNoReplay
                | otherwise = opsNormPruned
            opsNormFinalized
                | null replayBinders
                , null sourceBindersInOrder =
                    []
                | otherwise =
                    opsNormProjected
            targetKeysUsed :: IntSet.IntSet
            targetKeysUsed =
                IntSet.fromList
                    [ getNodeId (canonical n)
                    | op <- opsNormFinalized
                    , n <- case op of
                        OpGraft _ t -> [t]
                        OpWeaken t -> [t]
                        OpRaise t -> [t]
                        OpMerge a b -> [a, b]
                        OpRaiseMerge a b -> [a, b]
                    ]
            isReplaySourceBinder sourceBinder =
                case IntMap.lookup (getNodeId sourceBinder) replayMapSource of
                    Just replayTarget -> isExactTyVar (canonical replayTarget)
                    Nothing -> False
            sourceReplayTargetKey sourceBinder =
                fmap (getNodeId . canonical) $
                    IntMap.lookup (getNodeId sourceBinder) replayMapSource
            sourceEntryTouchesTargets sourceBinder =
                let sourceKey = getNodeId (canonical (rewriteNode sourceBinder))
                in IntSet.member sourceKey targetKeysUsed
                    || maybe False (`IntSet.member` targetKeysUsed) (sourceReplayTargetKey sourceBinder)
            activeSourceEntriesBaseRaw =
                [ entry
                | entry@(sourceBinder, _arg) <- sourceEntriesInOrder
                , IntMap.member (getNodeId sourceBinder) replayMapSource
                ]
            activeSourceEntriesBase
                | null replayBinders = activeSourceEntriesBaseRaw
                | otherwise =
                    [ entry
                    | entry@(sourceBinder, _arg) <- activeSourceEntriesBaseRaw
                    , isReplaySourceBinder sourceBinder
                    ]
            activeSourceEntries
                -- Non-polymorphic edge: do not emit replay-domain contract fields.
                | null replayBinders = []
                | IntSet.null targetKeysUsed = activeSourceEntriesBase
                | otherwise =
                    let filtered =
                            [ entry
                            | entry@(sourceBinder, _arg) <- activeSourceEntriesBase
                            , sourceEntryTouchesTargets sourceBinder
                            ]
                    in if null filtered
                        then activeSourceEntriesBase
                        else filtered
            nActive = length activeSourceEntries
        when (not (null replayBinders) && length replayBinders < nActive) $
            throwError $
                WitnessNormalizationError (EdgeId eid) $
                    Witness.ReplayMapIncomplete (map fst (drop (length replayBinders) activeSourceEntries))
        let replayPairs = zip activeSourceEntries (take nActive replayBinders)
            replayMapSourceFinal
                | null replayBinders =
                    IntMap.fromList
                        [ (getNodeId sourceBinder, canonical replayBinder)
                        | (sourceBinder, _arg) <- activeSourceEntries
                        , Just replayBinder <- [IntMap.lookup (getNodeId sourceBinder) replayMapSource]
                        ]
                | otherwise =
                    IntMap.fromList
                        [ (getNodeId sourceBinder, replayBinder)
                        | ((sourceBinder, _), replayBinder) <- replayPairs
                        ]
            replayMapValidation =
                IntMap.fromList
                    [ (getNodeId (canonical (rewriteNode sourceBinder)), replayBinder)
                    | (sourceBinder, _arg) <- activeSourceEntries
                    , Just replayBinder <- [IntMap.lookup (getNodeId sourceBinder) replayMapSourceFinal]
                    ]
            activeBinderArgsMap =
                IntMap.fromList
                    [ ( getNodeId (canonical (rewriteNode sourceBinder))
                      , canonical (rewriteNode arg)
                      )
                    | (sourceBinder, arg) <- activeSourceEntries
                    ]
            activeBinderArgs = activeSourceEntries
            envPost =
                env
                    { Witness.binderArgs = activeBinderArgsMap
                    , Witness.binderReplayMap = replayMapValidation
                    }
        -- Validate normalized ops and replay-map contract against the
        -- post-projection replay codomain (producer boundary contract).
        -- IMPORTANT: Validate BEFORE restoring to original node space, since
        -- interiorNorm is in the rewritten node space (matching the normalized ops).
        --
        -- When an edge root has no replay binders, there is no replay codomain
        -- contract to enforce at this boundary; keep legacy permissive behavior
        -- for these edges and defer to downstream constraints.
        when (not (null replayBinders)) $
            case validateNormalizedWitness envPost opsNormFinalized of
                Left valErr -> throwError (WitnessNormalizationError (EdgeId eid) valErr)
                Right () -> pure ()
        let interiorSourceKeys =
                case traceInterior of
                    InteriorNodes s -> s
            normalizeRaiseTarget op =
                case op of
                    OpRaise n
                        | IntSet.member (getNodeId n) interiorSourceKeys ->
                            Just (OpRaise n)
                        | IntSet.member (getNodeId (canonical n)) interiorSourceKeys ->
                            Just (OpRaise (canonical n))
                        | otherwise ->
                            Nothing
                    _ -> Just op
            opsFinal = mapMaybe (normalizeRaiseTarget . restoreOp) opsNormFinalized
            trace' = fmap (\tr -> tr
                { etBinderArgs = activeBinderArgs
                , etBinderReplayMap = replayMapSourceFinal
                }) mbTrace
        pure (eid, w0 { ewForallIntros = ewForallIntros w0, ewWitness = InstanceWitness opsFinal }, trace')
    let witnessMap =
            IntMap.fromList
                [ (eid, witness)
                | (eid, witness, _mbTrace) <- witnessResults
                ]
        traceUpdates =
            IntMap.fromList
                [ (eid, tr)
                | (eid, _witness, Just tr) <- witnessResults
                ]
    modify' $ \st ->
        st
            { psEdgeWitnesses = witnessMap
            , psEdgeTraces = IntMap.union traceUpdates traces
            }
