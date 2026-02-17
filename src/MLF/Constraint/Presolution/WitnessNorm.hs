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
import Control.Monad (forM, foldM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.Validation (translatableWeakenedNodes)
import MLF.Constraint.Presolution.Witness (
    normalizeInstanceStepsFull,
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
                , StepOmega (OpWeaken n) <- ewSteps w0
                ]
        weakened =
            IntSet.union weakenedOps (translatableWeakenedNodes c0)
    witnessResults <- forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
        let mbTrace = IntMap.lookup eid traces
            (edgeRoot, copyMap, binderArgs0, traceInterior, existingHints) =
                case mbTrace of
                    Nothing -> (ewRoot w0, mempty, [], mempty, IntMap.empty)
                    Just tr ->
                        ( etRoot tr
                        , etCopyMap tr
                        , etBinderArgs tr
                        , etInterior tr
                        , etBinderReplayHints tr
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
            rewriteStep step =
                case step of
                    StepOmega op -> StepOmega (rewriteOp op)
                    StepIntro -> StepIntro
            restoreOp op =
                case op of
                    OpGraft sigma n -> OpGraft (restoreNode sigma) (restoreNode n)
                    OpMerge n m -> OpMerge (restoreNode n) (restoreNode m)
                    OpRaise n -> OpRaise (restoreNode n)
                    OpWeaken n -> OpWeaken (restoreNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (restoreNode n) (restoreNode m)
            restoreStep step =
                case step of
                    StepOmega op -> StepOmega (restoreOp op)
                    StepIntro -> StepIntro
            isLiveTyVar nid =
                case NodeAccess.lookupNode c0 (canonical nid) of
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
            sourceBindersInOrder =
                reverse
                    . snd
                    $ foldl'
                        (\(seen, acc) (binder, _arg) ->
                            let key = getNodeId binder
                            in if IntSet.member key seen
                                then (seen, acc)
                                else (IntSet.insert key seen, binder : acc)
                        )
                        (IntSet.empty, [])
                        binderArgs0
            liveReplayPool =
                [ NodeId key
                | key <- IntMap.keys binderArgs
                , isLiveTyVar (NodeId key)
                ]
            chooseReplayHints =
                let go (sourceAcc, rewrittenAcc, used) (i, sourceBinder) =
                        let sourceKey = getNodeId sourceBinder
                            rewrittenSource = canonical (rewriteNode sourceBinder)
                            rewrittenSourceKey = getNodeId rewrittenSource
                            existingSeed =
                                case IntMap.lookup sourceKey existingHints of
                                    Just replayN -> [canonical replayN]
                                    _ -> []
                            directSeed = [rewrittenSource]
                            positionalSeed =
                                case drop i liveReplayPool of
                                    replayN : _ -> [replayN]
                                    [] -> []
                            liveSeeds =
                                [ nid
                                | nid <- existingSeed ++ positionalSeed ++ liveReplayPool ++ directSeed
                                , isLiveTyVar nid
                                ]
                            liveCandidates =
                                dedupNodes liveSeeds
                            fallbackCandidates =
                                dedupNodes (liveCandidates ++ existingSeed ++ directSeed)
                            pick =
                                case [ nid | nid <- liveCandidates, not (IntSet.member (getNodeId nid) used) ] of
                                    nid : _ -> Just nid
                                    [] ->
                                        case liveCandidates of
                                            nid : _ -> Just nid
                                            [] ->
                                                case fallbackCandidates of
                                                    nid : _ -> Just nid
                                                    [] -> Just rewrittenSource
                        in case pick of
                            Nothing -> (sourceAcc, rewrittenAcc, used)
                            Just replayN ->
                                ( IntMap.insert sourceKey replayN sourceAcc
                                , IntMap.insert rewrittenSourceKey replayN rewrittenAcc
                                , IntSet.insert (getNodeId replayN) used
                                )
                in foldl' go (IntMap.empty, IntMap.empty, IntSet.empty) (zip [0 :: Int ..] sourceBindersInOrder)
            (replayHintsSource, replayHintsRewritten, _usedReplayHints) = chooseReplayHints
        let orderBase = edgeRoot
            orderRoot = orderBase
            traceInteriorKeys =
                case traceInterior of
                    InteriorNodes s -> s
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
            interiorWithBinders =
                if IntMap.size binderArgs > 1
                    then IntSet.union interiorNorm (IntSet.fromList (IntMap.keys binderArgs))
                    else interiorNorm
            isAnnEdge =
                IntSet.member eid (cAnnEdges c0)
            isLiveNode nid =
                case NodeAccess.lookupNode c0 (canonical nid) of
                    Just _ -> True
                    _ -> False
            sourceBinderReplayKey sourceBinder =
                IntMap.lookup
                    (getNodeId (canonical (rewriteNode sourceBinder)))
                    replayHintsRewritten
            replayBinderForOpTarget target =
                case IntMap.lookup (getNodeId (canonical target)) replayHintsRewritten of
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
            isOmegaStep = \case
                StepOmega{} -> True
                StepIntro -> False
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
            synthesizeAmbiguousAnnotationGraftWeaken steps
                | not isAnnEdge = Right steps
                | otherwise = go steps
              where
                go [] = Right []
                go (StepIntro : rest) =
                    (StepIntro :) <$> go rest
                go xs =
                    let (omegaSeg, rest) = span isOmegaStep xs
                        ops0 = [op | StepOmega op <- omegaSeg]
                        binders0 = ambiguousReplayBinders ops0
                    in do
                        ops1 <- foldM synthesizeOneBinder ops0 binders0
                        rest' <- go rest
                        pure (map StepOmega ops1 ++ rest')
        steps0 <- case synthesizeAmbiguousAnnotationGraftWeaken (map rewriteStep (ewSteps w0)) of
            Left err ->
                throwError (WitnessNormalizationError (EdgeId eid) err)
            Right steps' -> pure steps'
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
                    , Witness.binderReplayHints = replayHintsRewritten
                    }
        steps <- case normalizeInstanceStepsFull env steps0 of
            Right steps' -> pure steps'
            Left err ->
                throwError (WitnessNormalizationError (EdgeId eid) err)
        -- Validate normalized ops against Fig. 15.3.4 invariants (thesis-exact).
        -- IMPORTANT: Validate BEFORE restoring to original node space, since
        -- interiorNorm is in the rewritten node space (matching the normalized steps).
        let opsRewritten = [op | StepOmega op <- steps]
        case validateNormalizedWitness env opsRewritten of
            Left valErr -> throwError (WitnessNormalizationError (EdgeId eid) valErr)
            Right () -> pure ()
        let interiorSourceKeys =
                case traceInterior of
                    InteriorNodes s -> s
            normalizeRaiseTarget step =
                case step of
                    StepOmega (OpRaise n)
                        | IntSet.member (getNodeId n) interiorSourceKeys ->
                            Just (StepOmega (OpRaise n))
                        | IntSet.member (getNodeId (canonical n)) interiorSourceKeys ->
                            Just (StepOmega (OpRaise (canonical n)))
                        | otherwise ->
                            Nothing
                    _ -> Just step
            stepsFinal = mapMaybe (normalizeRaiseTarget . restoreStep) steps
            ops = [op | StepOmega op <- stepsFinal]
            trace' = fmap (\tr -> tr { etBinderReplayHints = replayHintsSource }) mbTrace
        pure (eid, w0 { ewSteps = stepsFinal, ewWitness = InstanceWitness ops }, trace')
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
