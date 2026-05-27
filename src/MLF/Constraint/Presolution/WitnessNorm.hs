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
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess
  ( PresolutionBindingSnapshot (..),
    bindingSnapshotInteriorOf,
    getBindingSnapshot,
  )
import MLF.Constraint.Presolution.Validation (translatableWeakenedNodes)
import MLF.Constraint.Presolution.Witness
  ( OmegaNormalizeEnv (OmegaNormalizeEnv, oneRoot),
    normalizeInstanceOpsCore,
    validateNormalizedWitness,
  )
import qualified MLF.Constraint.Presolution.Witness as Witness
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
    ( InstanceOp(..)
    , ReplayContract(..)
    , ewEdgeId
    , ewForallIntros
    , ewLeft
    , ewRight
    , ewRoot
    , ewWitness
    , getInstanceOps
    , mkEdgeWitness
    , mkInstanceWitness
    )
import qualified MLF.Util.Order as Order

data WitnessNormCache = WitnessNormCache
  { wncOrderedBinders :: IntMap.IntMap [NodeId],
    wncInteriorExact :: IntMap.IntMap IntSet.IntSet,
    wncOrderKeys :: IntMap.IntMap (IntMap.IntMap Order.OrderKey),
    wncAbstractBoundShapes :: IntMap.IntMap Bool
  }

emptyWitnessNormCache :: WitnessNormCache
emptyWitnessNormCache =
  WitnessNormCache
    { wncOrderedBinders = IntMap.empty,
      wncInteriorExact = IntMap.empty,
      wncOrderKeys = IntMap.empty,
      wncAbstractBoundShapes = IntMap.empty
    }

orderedNubNodes :: [NodeId] -> [NodeId]
orderedNubNodes =
  reverse . snd . foldl' step (IntSet.empty, [])
  where
    step (seen, acc) nid =
      let key = getNodeId nid
       in if IntSet.member key seen
            then (seen, acc)
            else (IntSet.insert key seen, nid : acc)

orderedNubPairs :: [((NodeId, NodeId), NodeId)] -> [((NodeId, NodeId), NodeId)]
orderedNubPairs =
  reverse . snd . foldl' step (IntMap.empty, [])
  where
    step (seen, acc) entry@((sourceBinder, _arg), target) =
      let sourceKey = getNodeId sourceBinder
          targetKey = getNodeId target
          targets = IntMap.findWithDefault IntSet.empty sourceKey seen
       in if IntSet.member targetKey targets
            then (seen, acc)
            else (IntMap.insert sourceKey (IntSet.insert targetKey targets) seen, entry : acc)

opTargets :: InstanceOp -> [NodeId]
opTargets op =
  case op of
    OpGraft sigma n -> [sigma, n]
    OpWeaken n -> [n]
    OpMerge n m -> [n, m]
    OpRaise n -> [n]
    OpRaiseMerge n m -> [n, m]

cachedOrderedBinders ::
  PresolutionBindingSnapshot p ->
  NodeId ->
  StateT WitnessNormCache (PresolutionM p) [NodeId]
cachedOrderedBinders snapshot nid = do
  let c0 = pbsConstraint snapshot
      canonical = pbsCanonical snapshot
  let nidC = canonical nid
      key = getNodeId nidC
  cache <- gets wncOrderedBinders
  case IntMap.lookup key cache of
    Just binders -> pure binders
    Nothing -> do
      binders <-
        case Binding.orderedBindersInQuotient canonical c0 (pbsQuotient snapshot) (typeRef nidC) of
          Left _ -> pure []
          Right ordered -> pure ordered
      modify' $ \st ->
        st {wncOrderedBinders = IntMap.insert key binders (wncOrderedBinders st)}
      pure binders

cachedInteriorExact ::
  PresolutionBindingSnapshot p ->
  NodeId ->
  StateT WitnessNormCache (PresolutionM p) IntSet.IntSet
cachedInteriorExact snapshot root0 = do
  let c0 = pbsConstraint snapshot
      canonical = pbsCanonical snapshot
  let rootC = canonical root0
      key = getNodeId rootC
  cache <- gets wncInteriorExact
  case IntMap.lookup key cache of
    Just interior -> pure interior
    Nothing -> do
      let interiorRootRef = traceInteriorRootRef canonical c0 root0
      raw <- lift (bindingSnapshotInteriorOf snapshot interiorRootRef)
      let interior =
            IntSet.fromList
              [ getNodeId nid
                | refKey <- IntSet.toList raw,
                  TypeRef nid <- [nodeRefFromKey refKey]
              ]
      modify' $ \st ->
        st {wncInteriorExact = IntMap.insert key interior (wncInteriorExact st)}
      pure interior

cachedOrderKeys ::
  Constraint p ->
  (NodeId -> NodeId) ->
  NodeId ->
  StateT WitnessNormCache (PresolutionM p) (IntMap.IntMap Order.OrderKey)
cachedOrderKeys c0 canonical root0 = do
  let rootC = canonical root0
      key = getNodeId rootC
  cache <- gets wncOrderKeys
  case IntMap.lookup key cache of
    Just orderKeys -> pure orderKeys
    Nothing -> do
      let orderKeys = Order.orderKeysFromConstraintWith canonical c0 rootC Nothing
      modify' $ \st ->
        st {wncOrderKeys = IntMap.insert key orderKeys (wncOrderKeys st)}
      pure orderKeys

cachedAbstractBoundShape ::
  Constraint p ->
  IntSet.IntSet ->
  (NodeId -> NodeId) ->
  NodeId ->
  StateT WitnessNormCache (PresolutionM p) Bool
cachedAbstractBoundShape c0 liveNodeKeys canonical nid = do
  let nidC = canonical nid
      key = getNodeId nidC
  cache <- gets wncAbstractBoundShapes
  case IntMap.lookup key cache of
    Just result -> pure result
    Nothing -> do
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
          result =
            if IntSet.notMember key liveNodeKeys
              then False
              else case NodeAccess.lookupNode c0 nidC of
                Just TyVar {tnBound = Just bnd} ->
                  go IntSet.empty bnd
                Just TyMu {tnBody = muBody} ->
                  go IntSet.empty muBody
                _ ->
                  False
      modify' $ \st ->
        st {wncAbstractBoundShapes = IntMap.insert key result (wncAbstractBoundShapes st)}
      pure result

precomputedDescendantsForOps ::
  PresolutionBindingSnapshot p ->
  [InstanceOp] ->
  IntMap.IntMap IntSet.IntSet
precomputedDescendantsForOps snapshot ops =
  IntMap.fromList
    [ (getNodeId (canonical target), descendants)
      | target <- orderedNubNodes (concatMap opCacheTargets ops),
        Just descendants <- [descendantsOf target]
    ]
  where
    canonical = pbsCanonical snapshot
    qbp = pbsQuotient snapshot
    childrenByParent = Binding.qbpChildrenByParent qbp

    descendantsOf target =
      let targetC = canonical target
          rootKey = nodeRefKey (typeRef targetC)
       in if IntSet.notMember rootKey (Binding.qbpAllRoots qbp)
            then Nothing
            else
              let go visited [] = visited
                  go visited (key : rest) =
                    let kids =
                          [ childKey
                          | (childKey, _info) <- IntMap.findWithDefault [] key childrenByParent,
                            not (IntSet.member childKey visited)
                          ]
                        visited' = foldl' (flip IntSet.insert) visited kids
                     in go visited' (kids ++ rest)
                  raw = go (IntSet.singleton rootKey) [rootKey]
               in Just $
                    IntSet.delete (getNodeId targetC) $
                      IntSet.fromList
                        [ getNodeId nid
                        | refKey <- IntSet.toList raw,
                          TypeRef nid <- [nodeRefFromKey refKey]
                        ]

    opCacheTargets op =
      case op of
        OpGraft _ n -> [n]
        OpWeaken n -> [n]
        OpMerge n m -> [n, m]
        OpRaise n -> [n]
        OpRaiseMerge n m -> [n, m]

-- | Normalize edge witnesses against the finalized presolution constraint.
normalizeEdgeWitnessesM :: PresolutionM p ()
normalizeEdgeWitnessesM = do
  snapshot <- getBindingSnapshot
  let c0 = pbsConstraint snapshot
      canonical = pbsCanonical snapshot
  traces <- gets psEdgeTraces
  witnesses0 <- gets psEdgeWitnesses
  let allNodes0 = NodeAccess.allNodes c0
      liveNodeKeys =
        IntSet.fromList [getNodeId (tnId node) | node <- allNodes0]
      tyVarNodeKeys =
        IntSet.fromList
          [ getNodeId (tnId node)
            | node@TyVar {} <- allNodes0
          ]
      tyMuNodeKeys =
        IntSet.fromList
          [ getNodeId (tnId node)
            | node@TyMu {} <- allNodes0
          ]
  let rewriteNodeWith copyMap nid =
        let mapped = IntMap.findWithDefault nid (getNodeId nid) (getCopyMapping copyMap)
            mappedC = canonical mapped
            sourceC = canonical nid
            isLive n = IntSet.member (getNodeId n) liveNodeKeys
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
  witnessResults <- evalStateT (forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
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
          IntSet.member (getNodeId nid) tyVarNodeKeys
        isLiveNode nid =
          IntSet.member (getNodeId (canonical nid)) liveNodeKeys
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
    let rootC = canonical edgeRoot
    directBinders <- cachedOrderedBinders snapshot rootC
    bindersOrdered <-
      case NodeAccess.lookupNode c0 rootC of
        Just TyVar {tnBound = Just bnd} -> do
          viaBound <- cachedOrderedBinders snapshot bnd
          pure (if null directBinders then viaBound else directBinders)
        Just TyMu {tnBody = muBody} -> do
          viaMu <- cachedOrderedBinders snapshot muBody
          pure (if null directBinders then viaMu else directBinders)
        _ -> pure directBinders
    let replayBindersAtRoot =
          [ canonical b
            | b <- bindersOrdered,
              isReplayDomainBinder b
          ]
    let orderBase = edgeRoot
        orderRoot = orderBase
    interiorExact <-
      if IntSet.null traceInteriorKeys
        then cachedInteriorExact snapshot edgeRoot
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
          orderedNubNodes
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
    orderKeys <- cachedOrderKeys c0 canonical orderRoot
    let env =
          OmegaNormalizeEnv
            { oneRoot = canonical edgeRoot,
              Witness.interior = interiorWithBinders,
              Witness.interiorRaw = interiorNorm,
              Witness.weakened = weakened,
              Witness.orderKeys = orderKeys,
              Witness.canonical = canonical,
              Witness.constraint = c0,
              Witness.binderArgs = binderArgs,
              Witness.precomputedDescendants = precomputedDescendantsForOps snapshot ops0,
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
    abstractShapes <-
      IntMap.fromList <$> do
        let targets =
              orderedNubNodes
                [ target
                  | op <- opsNorm,
                    target <- opTargets op
                ]
        forM targets $ \target -> do
          shape <- cachedAbstractBoundShape c0 liveNodeKeys canonical target
          pure (getNodeId (canonical target), shape)
    let sourceKeySet =
          IntSet.fromList
            [ getNodeId sourceBinder
              | sourceBinder <- sourceBindersInOrder
            ]
        abstractBoundShape nid =
          IntMap.findWithDefault False (getNodeId (canonical nid)) abstractShapes
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
            (`IntSet.member` tyMuNodeKeys)
            (IntSet.toList interiorWithBinders)
        replayBindersSeededFromInteriorGrafts =
          orderedNubNodes
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
          orderedNubPairs
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
              orderedNubNodes (map snd replayEntriesSeededFromRaiseMerge)
          | otherwise =
              replayBindersAtRoot
        replayBindersWithBoundedGrafts =
          if null replayBinders
            then []
            else
              orderedNubNodes
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
    let iw = mkInstanceWitness (Witness.validatedInstanceOpsAfterNormalization opsFinal)
        witness' =
            case mkEdgeWitness (ewEdgeId w0) (ewLeft w0) (ewRight w0) (ewRoot w0) (ewForallIntros w0) iw of
                Left err ->
                    error ("normalizeEdgeWitnessesM rebuilt invalid witness: " ++ show err)
                Right witness ->
                    witness
    pure (eid, witness', trace')
    ) emptyWitnessNormCache
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
