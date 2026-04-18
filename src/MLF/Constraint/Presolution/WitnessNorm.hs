{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Constraint.Presolution.WitnessNorm
-- Description : Witness normalization for presolution
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- This module handles normalization of edge witnesses against the finalized
-- presolution constraint.
module MLF.Constraint.Presolution.WitnessNorm
  ( normalizeEdgeWitnessesM,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.State
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find, nub)
import Data.Maybe (mapMaybe)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.Validation (translatableWeakenedNodes)
import MLF.Constraint.Presolution.Witness
  ( OmegaNormalizeEnv (OmegaNormalizeEnv, oneRoot),
    normalizeInstanceOpsCore,
    validateNormalizedWitness,
  )
import qualified MLF.Constraint.Presolution.Witness as Witness
import MLF.Constraint.Types
import MLF.Constraint.Types.Witness (ReplayContract (..))
import qualified MLF.Util.Order as Order

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
              else
                if isLive sourceC
                  then sourceC
                  else mappedC
      weakenedOps =
        IntSet.fromList
          [ getNodeId (canonical (rewriteNodeWith copyMap n))
            | (eid, w0) <- IntMap.toList witnesses0,
              let copyMap = maybe mempty etCopyMap (IntMap.lookup eid traces),
              OpWeaken n <- getInstanceOps (ewWitness w0)
          ]
      weakened =
        IntSet.union weakenedOps (translatableWeakenedNodes c0)
  witnessResults <- forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
    let mbTrace = IntMap.lookup eid traces
        (edgeRoot, copyMap, binderArgs0, traceInterior) =
          case mbTrace of
            Nothing -> (ewRoot w0, mempty, [], mempty)
            Just tr ->
              ( etRoot tr,
                etCopyMap tr,
                etBinderArgs tr,
                etInterior tr
              )
        rewriteNode = rewriteNodeWith copyMap
        binderArgs =
          IntMap.fromList
            [ ( getNodeId (canonical (rewriteNode bv)),
                canonical (rewriteNode arg)
              )
              | (bv, arg) <- binderArgs0
            ]
        copyToOriginal =
          IntMap.fromListWith
            min
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
            Just TyVar {} -> True
            _ -> False
        isLiveNode nid =
          case NodeAccess.lookupNode c0 (canonical nid) of
            Just _ -> True
            _ -> False
        sourceEntriesInOrder :: [(NodeId, NodeId)]
        sourceEntriesInOrder =
          reverse $
            snd $
              foldl'
                ( \(seen, acc) (sourceBinder, arg) ->
                    let key = getNodeId sourceBinder
                     in if IntSet.member key seen
                          then (seen, acc)
                          else (IntSet.insert key seen, (sourceBinder, arg) : acc)
                )
                (IntSet.empty, [])
                binderArgs0
        sourceBindersInOrder = map fst sourceEntriesInOrder
        sourceBinderKeySet =
          IntSet.fromList
            [ getNodeId sourceBinder
              | sourceBinder <- sourceBindersInOrder
            ]
        traceInteriorKeys =
          case traceInterior of
            InteriorNodes s -> s
        isSchemeRootSourceBinder nid =
          case Binding.lookupBindParent c0 (typeRef nid) of
            Just (GenRef gid, _) ->
              case lookupGen gid (cGenNodes c0) of
                Just gen -> nid `elem` gnSchemes gen
                Nothing -> False
            _ -> False
        isReplayDomainBinder nid =
          let targetC = canonical nid
              restored = restoreNode targetC
           in isExactTyVar targetC
                && (restored == targetC || IntSet.notMember (getNodeId restored) sourceBinderKeySet)
        replayBindersAtRoot =
          [ canonical b
            | b <- bindersOrdered,
              isReplayDomainBinder b
          ]
          where
            bindersOrdered =
              let rootC = canonical edgeRoot
                  orderedUnder nid =
                    either (const []) id $
                      Binding.orderedBinders canonical c0 (typeRef (canonical nid))
                  direct = orderedUnder rootC
               in case NodeAccess.lookupNode c0 rootC of
                    Just TyVar {tnBound = Just bnd} ->
                      let viaBound = orderedUnder bnd
                       in if null direct then viaBound else direct
                    Just TyMu {tnBody = muBody} ->
                      let viaMu = orderedUnder muBody
                       in if null direct then viaMu else direct
                    _ -> direct
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
        nSourceBinders = length sourceBindersInOrder
        initialReplayPairs =
          zip sourceBindersInOrder (take nSourceBinders replayBindersAtRoot)
        replayMapRewritten =
          IntMap.fromList
            [ (getNodeId (canonical (rewriteNode sourceBinder)), replayBinder)
              | (sourceBinder, replayBinder) <- initialReplayPairs
            ]
        sourceReplayBinders =
          nub
            [ binderC
              | sourceBinder <- sourceBindersInOrder,
                let binderC = canonical (rewriteNode sourceBinder),
                isExactTyVar binderC,
                isSchemeRootSourceBinder sourceBinder
            ]
    let interiorWithBinders =
          if IntMap.size binderArgs > 1
            then IntSet.union interiorNorm (IntSet.fromList (IntMap.keys binderArgs))
            else interiorNorm
        isAnnEdge =
          IntSet.member eid (cAnnEdges c0)
        ops0 = map rewriteOp (getInstanceOps (ewWitness w0))
    let orderKeys0 = Order.orderKeysFromConstraintWith canonical c0 (canonical orderRoot) Nothing
        orderKeys = orderKeys0
        env =
          OmegaNormalizeEnv
            { oneRoot = canonical edgeRoot,
              Witness.interior = interiorWithBinders,
              Witness.interiorRaw = interiorNorm,
              Witness.weakened = weakened,
              Witness.orderKeys = orderKeys,
              Witness.canonical = canonical,
              Witness.constraint = c0,
              Witness.binderArgs = binderArgs,
              Witness.binderReplayMap = replayMapRewritten,
              Witness.replayContract = ReplayContractNone,
              Witness.replayDomainBinders = replayBindersAtRoot,
              Witness.isAnnotationEdge = isAnnEdge
            }
    opsNormRaw <- case normalizeInstanceOpsCore env ops0 of
      Right ops' -> pure ops'
      Left err ->
        throwError (WitnessNormalizationError (EdgeId eid) err)
    let opsNorm = opsNormRaw
    let sourceKeySet =
          IntSet.fromList
            [ getNodeId sourceBinder
              | sourceBinder <- sourceBindersInOrder
            ]
        abstractBoundShape nid =
          let go seen current =
                let currentC = canonical current
                    currentKey = getNodeId currentC
                    seen' = IntSet.insert currentKey seen
                 in if IntSet.member currentKey seen
                      then True
                      else case NodeAccess.lookupNode c0 currentC of
                        Just TyVar {tnBound = Nothing} ->
                          True
                        Just TyVar {tnBound = Just bnd} ->
                          go seen' bnd
                        Just TyBase {} ->
                          False
                        Just TyBottom {} ->
                          False
                        Just node ->
                          let children = structuralChildren node
                           in not (null children) && all (go seen') children
                        Nothing ->
                          False
           in case NodeAccess.lookupNode c0 (canonical nid) of
                Just TyVar {tnBound = Just bnd} ->
                  go IntSet.empty bnd
                Just TyMu {tnBody = muBody} ->
                  go IntSet.empty muBody
                _ ->
                  False
        sourceKeySetSeed =
          IntSet.fromList
            [ getNodeId (canonical (rewriteNode sourceBinder))
              | sourceBinder <- sourceBindersInOrder
            ]
        replayKeySetSeed =
          IntSet.fromList
            [ getNodeId replayBinder
              | replayBinder <- replayBindersAtRoot
            ]
        keepFinalizedOp op =
          case op of
            OpWeaken target ->
              let targetC = canonical target
                  targetKey = getNodeId targetC
                  rootKey = getNodeId (canonical edgeRoot)
               in targetKey == rootKey
                    || IntSet.member targetKey sourceKeySetSeed
                    || IntSet.member targetKey replayKeySetSeed
                    || abstractBoundShape target
            _ ->
              True
        opsNormPruned =
          filter keepFinalizedOp opsNorm
        graftTargetKeys =
          IntSet.fromList
            [ getNodeId (restoreNode target)
              | OpGraft _ target <- opsNormFinalized
            ]
        graftTargetCount = IntSet.size graftTargetKeys
        opsNormFinalized
          | null sourceBindersInOrder,
            null opsNormPruned =
              []
          | otherwise =
              opsNormPruned
        isInSourceInterior target =
          IntSet.member (getNodeId (restoreNode target)) traceInteriorKeys
        interiorContainsTyMu =
          any
            ( \nid ->
                case NodeAccess.lookupNode c0 (NodeId nid) of
                  Just TyMu {} -> True
                  _ -> False
            )
            (IntSet.toList interiorWithBinders)
        replayBindersSeededFromInteriorGrafts =
          nub
            [ targetC
              | OpGraft _ target <- opsNormFinalized,
                let targetC = canonical target,
                isExactTyVar targetC,
                abstractBoundShape target,
                isInSourceInterior target,
                restoreNode target /= edgeRoot,
                not (IntSet.member (getNodeId (restoreNode target)) sourceKeySet)
            ]
        sourceEntryForRestored key =
          find
            ( \(sourceBinder, _) ->
                getNodeId sourceBinder == key
            )
            sourceEntriesInOrder
        replayEntriesSeededFromRaiseMerge =
          nub
            [ (sourceEntry, targetC)
              | OpRaiseMerge source target <- opsNormFinalized,
                let sourceKey = getNodeId (restoreNode source),
                Just sourceEntry <- [sourceEntryForRestored sourceKey],
                let targetC = canonical target,
                isExactTyVar targetC,
                not (IntSet.member (getNodeId targetC) sourceKeySet)
            ]
        replayBinders
          | null replayBindersAtRoot
              && length sourceReplayBinders == length sourceEntriesInOrder
              && any
                ( \case
                    OpGraft _ target -> abstractBoundShape target
                    _ -> False
                )
                opsNormFinalized =
              sourceReplayBinders
          | null replayBindersAtRoot
              && length replayBindersSeededFromInteriorGrafts == length sourceEntriesInOrder
              && not (null replayBindersSeededFromInteriorGrafts) =
              replayBindersSeededFromInteriorGrafts
          | null replayBindersAtRoot
              && not (null replayEntriesSeededFromRaiseMerge) =
              nub (map snd replayEntriesSeededFromRaiseMerge)
          | otherwise =
              replayBindersAtRoot
        replayBindersWithBoundedGrafts =
          if null replayBinders
            then []
            else
              nub
                ( replayBinders
                    ++ [ targetC
                         | OpGraft _ target <- opsNormFinalized,
                           let targetC = canonical target,
                           isExactTyVar targetC,
                           abstractBoundShape target
                       ]
                )
        hasReplayCodomain =
          not (null replayBindersWithBoundedGrafts)
        semanticStrictWithReplayCodomain op =
          case op of
            OpWeaken target -> canonical target /= canonical edgeRoot
            OpGraft _ target -> canonical target /= canonical edgeRoot
            OpMerge {} -> True
            OpRaiseMerge {} -> True
            OpRaise {} -> False
        strictWithReplayCodomain =
          hasReplayCodomain
            && ( not (null sourceEntriesInOrder)
                   || any semanticStrictWithReplayCodomain opsNorm
               )
        keepNoReplayProjectedOp op =
          case op of
            OpGraft {} ->
              Nothing
            OpMerge {} ->
              Just op
            OpRaiseMerge n m
              | isLiveNode n && isLiveNode m -> Just op
              | otherwise -> Nothing
            OpRaise target
              | restoreNode target == edgeRoot ->
                  Nothing
              | not (isTypeTreeBound target) ->
                  Nothing
              | otherwise ->
                  Just op
            OpWeaken target
              | not (abstractBoundShape target) ->
                  Nothing
              | IntSet.size sourceKeySet <= 1 ->
                  Nothing
              | not (IntSet.member (getNodeId (restoreNode target)) sourceKeySet) ->
                  Nothing
              | IntSet.member (getNodeId (restoreNode target)) graftTargetKeys ->
                  Nothing
              | graftTargetCount <= 1 ->
                  Nothing
              | restoreNode target == edgeRoot ->
                  Nothing
              | otherwise ->
                  Just op
        opsNoReplayProjected =
          mapMaybe keepNoReplayProjectedOp opsNormFinalized
        disallowedNoReplayOp op =
          case op of
            OpGraft _ target ->
              graftTargetCount <= 1
                && isInSourceInterior target
                && restoreNode target /= edgeRoot
                && not (IntSet.member (getNodeId (restoreNode target)) sourceKeySet)
            OpMerge {} -> True
            OpRaiseMerge n m -> isLiveNode n && isLiveNode m
            _ -> False
        residualNoReplayOp
          | strictWithReplayCodomain =
              Nothing
          | interiorContainsTyMu =
              Nothing
          | otherwise =
              find disallowedNoReplayOp opsNormFinalized
        strictNoReplayContract =
          case residualNoReplayOp of
            Nothing ->
              any
                ( \case
                    OpWeaken target -> restoreNode target /= edgeRoot
                    _ -> False
                )
                opsNoReplayProjected
            Just _ ->
              False
        strictReplayContract =
          strictWithReplayCodomain || strictNoReplayContract
        replayContract =
          if strictReplayContract
            then ReplayContractStrict
            else ReplayContractNone
        opsNormContract
          | strictWithReplayCodomain =
              opsNormFinalized
          | otherwise =
              filter (not . disallowedNoReplayOp) opsNoReplayProjected
        activeSourceEntries
          | not strictWithReplayCodomain = []
          | not (null replayEntriesSeededFromRaiseMerge) =
              map fst replayEntriesSeededFromRaiseMerge
          | otherwise =
              reverse $
                snd $
                  foldl'
                    ( \(seen, acc) entry@(sourceBinder, _arg) ->
                        let key = getNodeId (canonical (rewriteNode sourceBinder))
                         in if IntSet.member key seen
                              then (seen, acc)
                              else (IntSet.insert key seen, entry : acc)
                    )
                    (IntSet.empty, [])
                    sourceEntriesInOrder
        nActive = length activeSourceEntries
        isTypeTreeBound target =
          case Binding.lookupBindParent c0 (typeRef (canonical target)) of
            Just (TypeRef _, _) -> True
            Nothing -> False
            Just _ -> False
    when (strictReplayContract && length replayBindersWithBoundedGrafts < nActive) $
      throwError $
        WitnessNormalizationError (EdgeId eid) $
          Witness.ReplayMapIncomplete (map fst (drop (length replayBindersWithBoundedGrafts) activeSourceEntries))
    let replayPairs =
          if strictReplayContract
            then
              if not (null replayEntriesSeededFromRaiseMerge)
                then replayEntriesSeededFromRaiseMerge
                else zip activeSourceEntries (take nActive replayBindersWithBoundedGrafts)
            else []
    let replayMapSourceFinal =
          IntMap.fromList
            [ (getNodeId sourceBinder, replayBinder)
              | ((sourceBinder, _), replayBinder) <- replayPairs
            ]
        replayMapReplayToSource =
          IntMap.fromList
            [ (getNodeId replayBinder, NodeId sourceBinder)
              | (sourceBinder, replayBinder) <- IntMap.toList replayMapSourceFinal
            ]
        replayMapValidation =
          IntMap.fromList
            [ (getNodeId (canonical (rewriteNode sourceBinder)), replayBinder)
              | (sourceBinder, _arg) <- activeSourceEntries,
                Just replayBinder <- [IntMap.lookup (getNodeId sourceBinder) replayMapSourceFinal]
            ]
        activeBinderArgsMap =
          IntMap.fromList
            [ ( getNodeId (canonical (rewriteNode sourceBinder)),
                canonical (rewriteNode arg)
              )
              | (sourceBinder, arg) <- activeSourceEntries
            ]
        activeBinderArgs
          | strictReplayContract = activeSourceEntries
          | otherwise = []
        envPost =
          env
            { Witness.binderArgs = activeBinderArgsMap,
              Witness.binderReplayMap = replayMapValidation,
              Witness.replayContract = replayContract,
              Witness.replayDomainBinders = replayBindersWithBoundedGrafts
            }
    -- Validate normalized ops and replay-map contract against the finalized
    -- producer replay codomain.
    -- IMPORTANT: Validate BEFORE restoring to original node space, since
    -- interiorNorm is in the rewritten node space (matching the normalized ops).
    case residualNoReplayOp of
      Just op ->
        throwError $
          WitnessNormalizationError (EdgeId eid) $
            Witness.ReplayContractNoneRequiresReplay op
      Nothing -> pure ()
    case validateNormalizedWitness envPost opsNormContract of
      Left valErr -> throwError (WitnessNormalizationError (EdgeId eid) valErr)
      Right () -> pure ()
    let interiorSourceKeys =
          case traceInterior of
            InteriorNodes s -> s
        restoreOpFinal op =
          case op of
            OpWeaken target
              | strictReplayContract,
                restoreNode target /= edgeRoot ->
                  let targetC = canonical target
                   in case IntMap.lookup (getNodeId targetC) replayMapReplayToSource of
                        Just sourceBinder -> OpWeaken sourceBinder
                        Nothing -> OpWeaken (restoreNode target)
            _ -> restoreOp op
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
        opsFinal = mapMaybe (normalizeRaiseTarget . restoreOpFinal) opsNormContract
        trace' =
          fmap
            ( \tr ->
                tr
                  { etBinderArgs = activeBinderArgs,
                    etReplayContract = replayContract,
                    etBinderReplayMap = replayMapSourceFinal,
                    etReplayDomainBinders =
                      if strictReplayContract
                        then replayBindersWithBoundedGrafts
                        else []
                  }
            )
            mbTrace
    pure (eid, w0 {ewForallIntros = ewForallIntros w0, ewWitness = InstanceWitness opsFinal}, trace')
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
      { psEdgeWitnesses = witnessMap,
        psEdgeTraces = IntMap.union traceUpdates traces
      }
