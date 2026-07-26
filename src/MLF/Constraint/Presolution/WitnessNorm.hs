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
  ( NormalizedEdgeArtifacts,
    normalizedEdgeArtifacts,
    normalizeEdgeWitnessesM,
  )
where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.State
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
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
  )
import qualified MLF.Constraint.Presolution.Witness as Witness
import qualified MLF.Constraint.Presolution.WitnessValidation as WitnessValidation
import MLF.Constraint.Presolution.WitnessCanon
  ( ProvenancedInstanceOp (..),
    ProvenancedNode (..),
    forgetInstanceOpProvenance,
    normalizeInstanceOpsCoreWithProvenanceBy,
  )
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
    ( InstanceOp(..)
    , ReplayContract(..)
    , ValidatedInstanceOps
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
import qualified MLF.Constraint.Types.Witness.Internal as WitnessInternal
import qualified MLF.Util.Order as Order

data WitnessNormCache = WitnessNormCache
  { wncOrderedBinders :: IntMap.IntMap [NodeId],
    wncInteriorExact :: IntMap.IntMap IntSet.IntSet,
    wncOrderKeys :: IntMap.IntMap (IntMap.IntMap Order.OrderKey),
    wncAbstractBoundShapes :: IntMap.IntMap Bool
  }

-- | Consumer-ready edge artifacts produced only by witness normalization.
--
-- Keeping the constructor private makes final presolution publication consume
-- normalization evidence instead of depending on a preceding state mutation.
newtype NormalizedEdgeArtifacts = NormalizedEdgeArtifacts
  { normalizedEdgeArtifacts :: EdgeArtifacts
  }

data OperandAuthorityDomain
  = FrozenSourceInterior
  | FinalDestinationInterior
  | RetainedFinalDestination
  deriving (Eq, Ord, Show)

data OperandConstructionCertificate
  = WeakenIfTerminalTargetBecomesRigid
  deriving (Eq, Ord, Show)

data OperandProvenance = OperandProvenance
  { operandSourceCandidates :: IntSet.IntSet,
    operandAuthorityDomains :: Set.Set OperandAuthorityDomain,
    operandConstructionCertificates :: Set.Set OperandConstructionCertificate
  }
  deriving (Eq, Show)

instance Semigroup OperandProvenance where
  left <> right =
    OperandProvenance
      { operandSourceCandidates =
          IntSet.union
            (operandSourceCandidates left)
            (operandSourceCandidates right),
        operandAuthorityDomains =
          Set.union
            (operandAuthorityDomains left)
            (operandAuthorityDomains right),
        operandConstructionCertificates =
          Set.union
            (operandConstructionCertificates left)
            (operandConstructionCertificates right)
      }

-- | Source-identity operations obtained only by the deterministic restoration
-- of a provenance-preserving normalized sequence.
--
-- Keeping the constructor private prevents an arbitrary raw operation list
-- from entering the final validation lane.
newtype RestoredInstanceOps =
  RestoredInstanceOps [InstanceOp]

restoreNormalizedInstanceOps
  :: Constraint p
  -> (NodeId -> NodeId)
  -> IntSet.IntSet
  -> IntMap.IntMap NodeId
  -> Bool
  -> NodeId
  -> [ProvenancedInstanceOp OperandProvenance]
  -> Either Witness.OmegaNormalizeError RestoredInstanceOps
restoreNormalizedInstanceOps
  c0
  canonical
  interiorSourceKeys
  replayMapReplayToSource
  strictReplayContract
  sourceRoot
  normalizedOps = do
    restored <- concat <$> traverse restoreOpWithDeferredWeaken normalizedOps
    pure (RestoredInstanceOps (mapMaybe keepSourceInteriorRaise restored))
  where
    restoreOperand operand =
      let provenance = pnProvenance operand
       in if Set.member RetainedFinalDestination (operandAuthorityDomains provenance)
            then Right (canonical (pnNode operand))
            else
              case map NodeId (IntSet.toAscList (operandSourceCandidates provenance)) of
                [source] -> Right source
                sources ->
                  Left
                    ( Witness.AmbiguousOperatedSource
                        (canonical (pnNode operand))
                        sources
                    )

    restoreOp op =
      case op of
        ProvenancedGraft sigma target ->
          OpGraft <$> restoreOperand sigma <*> restoreOperand target
        ProvenancedMerge operated other ->
          OpMerge <$> restoreOperand operated <*> restoreOperand other
        ProvenancedRaise target ->
          OpRaise <$> restoreOperand target
        -- RaiseMerge is the normalized form of Raise(n); Merge(n,m).
        -- Restoring copy-domain identities can turn only the Merge part into
        -- a self-merge; the preceding Raise remains semantic.
        ProvenancedRaiseMerge operated other -> do
          operated' <- restoreOperand operated
          other' <- restoreOperand other
          pure $
            if operated' == other'
              then OpRaise operated'
              else OpRaiseMerge operated' other'
        ProvenancedWeaken target -> do
          operated <- restoreOperand target
          let targetC = canonical (pnNode target)
              replaySource =
                case IntMap.lookup (getNodeId targetC) replayMapReplayToSource of
                  Just sourceBinder -> Just sourceBinder
                  Nothing ->
                    IntMap.lookup
                      (getNodeId (canonical operated))
                      replayMapReplayToSource
          pure $
            case replaySource of
              Just sourceBinder
                | strictReplayContract,
                  operated /= sourceRoot -> OpWeaken sourceBinder
              _ -> OpWeaken operated

    restoreOpWithDeferredWeaken op = do
      restored <- restoreOp op
      let targetIsRigid target =
            let targetNode = canonical (pnNode target)
             in case Binding.lookupBindParent c0 (typeRef targetNode) of
                  Just (_parent, BindRigid) -> True
                  _ -> False
          deferredWeaken =
            case (op, restored) of
              (ProvenancedRaiseMerge operated other, OpRaiseMerge operated' _)
                | Set.member
                    WeakenIfTerminalTargetBecomesRigid
                    (operandConstructionCertificates (pnProvenance operated)),
                  targetIsRigid other ->
                      [OpWeaken operated']
              _ -> []
      pure (restored : deferredWeaken)

    keepSourceInteriorRaise op =
      case op of
        OpRaise node
          | IntSet.member (getNodeId node) interiorSourceKeys ->
              Just op
          | otherwise ->
              Nothing
        _ -> Just op

certifyRestoredInstanceOps
  :: WitnessValidation.NormalizedWitnessValidation
  -> IntSet.IntSet
  -> NodeId
  -> (NodeId -> NodeId -> Bool)
  -> (NodeId -> NodeId -> Bool)
  -> RestoredInstanceOps
  -> Either Witness.OmegaNormalizeError ValidatedInstanceOps
certifyRestoredInstanceOps
  destinationValidation
  sourceReplayDomain
  traceRoot
  flexibleAuthority
  rigidAuthority
  (RestoredInstanceOps ops) =
    destinationValidation `seq` do
      case
          [ operand
          | op <- ops
          , operand <- opTargets op
          , IntSet.notMember (getNodeId operand) sourceReplayDomain
          ]
        of
          operand : _ ->
            Left (Witness.FinalOperandOutsideSourceReplayDomain operand)
          [] -> Right ()
      WitnessValidation.validateTerminalRootRaiseMerge
        traceRoot
        flexibleAuthority
        rigidAuthority
        ops
      pure (WitnessInternal.sealValidatedInstanceOps ops)

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
      interiorRootRef = traceInteriorRootRef canonical c0 root0
      key = nodeRefKey interiorRootRef
  cache <- gets wncInteriorExact
  case IntMap.lookup key cache of
    Just interior -> pure interior
    Nothing -> do
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
normalizeEdgeWitnessesM :: PresolutionM p NormalizedEdgeArtifacts
normalizeEdgeWitnessesM = do
  snapshot <- getBindingSnapshot
  let c0 = pbsConstraint snapshot
      canonical = pbsCanonical snapshot
  artifacts0 <- gets psEdgeExecutionArtifacts
  weakenReplayCertificates <- gets psWeakenReplayCertificates
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
            | artifacts <- IntMap.elems artifacts0,
              let w0 = eeaWitness artifacts
                  copyMap = etCopyMap (eeaTrace artifacts),
            OpWeaken n <- getInstanceOps (ewWitness w0)
          ]
  weakenedByTranslatability <-
    either throwError pure (translatableWeakenedNodes c0)
  let
      weakened =
        IntSet.union weakenedOps weakenedByTranslatability
  witnessResults <- evalStateT (forM (IntMap.toList artifacts0) $ \(eid, artifacts) -> do
    let w0 = eeaWitness artifacts
        trace0 = eeaTrace artifacts
        (sourceRoot, resultRoot, copyMap, binderArgs0, traceInterior, producerReplayBinders) =
          ( etRoot trace0,
            etResultRoot trace0,
            etCopyMap trace0,
            etBinderArgs trace0,
            etInterior trace0,
            etReplayDomainBinders trace0
          )
        rewriteNode = rewriteNodeWith copyMap
        binderArgEntries =
          [ ( getNodeId (canonical (rewriteNode bv)),
              canonical (rewriteNode arg)
            )
            | (bv, arg) <- binderArgs0
          ]
        sourcesByExactCopy =
          IntMap.fromListWith
            IntSet.union
            [ ( getNodeId copy,
                IntSet.singleton orig
              )
              | (orig, copy) <- IntMap.toList (getCopyMapping copyMap)
            ]
        sourcesByDestination =
          IntMap.fromListWith
            IntSet.union
            [ ( copyKey,
                IntSet.singleton orig
              )
              | (orig, copy) <- IntMap.toList (getCopyMapping copyMap),
                copyKey <- [getNodeId copy, getNodeId (canonical copy)]
            ]
        sourceCandidates raw =
          case IntMap.lookup (getNodeId raw) sourcesByExactCopy of
            Just candidates | not (IntSet.null candidates) -> candidates
            _ ->
              let destination = canonical raw
               in case IntMap.lookup (getNodeId destination) sourcesByDestination of
                    Just candidates | not (IntSet.null candidates) -> candidates
                    _ ->
                      -- This helper is used only for an operation tagged as
                      -- destination-origin.  Copy provenance therefore wins
                      -- even when a destination representative happens to
                      -- reuse a frozen source key.
                      IntSet.singleton (getNodeId raw)
        sourceProvenancedNode raw =
          ProvenancedNode
            { pnNode = rewriteNode raw,
              pnProvenance =
                OperandProvenance
                  { operandSourceCandidates = IntSet.singleton (getNodeId raw),
                    operandAuthorityDomains = Set.singleton FrozenSourceInterior,
                    operandConstructionCertificates = Set.empty
                  }
            }
        destinationProvenancedNode raw =
          ProvenancedNode
            { pnNode = canonical raw,
              pnProvenance =
                OperandProvenance
                  { operandSourceCandidates = sourceCandidates raw,
                    operandAuthorityDomains = Set.singleton FinalDestinationInterior,
                    operandConstructionCertificates = Set.empty
                }
            }
        retainedDestinationProvenancedNode raw =
          ProvenancedNode
            { pnNode = canonical raw,
              pnProvenance =
                OperandProvenance
                  { operandSourceCandidates = IntSet.empty,
                    operandAuthorityDomains = Set.singleton RetainedFinalDestination,
                    operandConstructionCertificates = Set.empty
                  }
            }
        flexibleTerminalSourceProvenancedNode raw =
          let sourceNode = sourceProvenancedNode raw
              provenance = pnProvenance sourceNode
           in sourceNode
                { pnProvenance =
                    provenance
                      { operandConstructionCertificates =
                          Set.singleton WeakenIfTerminalTargetBecomesRigid
                      }
                }
        provenancedOpWith provenancedNode op =
          case op of
            OpGraft sigma n ->
              ProvenancedGraft (provenancedNode sigma) (provenancedNode n)
            OpMerge n m ->
              ProvenancedMerge (provenancedNode n) (provenancedNode m)
            OpRaise n -> ProvenancedRaise (provenancedNode n)
            OpWeaken n -> ProvenancedWeaken (provenancedNode n)
            OpRaiseMerge n m ->
              ProvenancedRaiseMerge (provenancedNode n) (provenancedNode m)
        provenancedOpWithOrigin origin op =
          case origin of
            Nothing -> Right (provenancedOpWith sourceProvenancedNode op)
            Just DestinationEdgeOperation ->
              Right (provenancedOpWith destinationProvenancedNode op)
            Just SourceDestinationMergeOperation ->
              case op of
                OpMerge operated other ->
                  Right
                    ( ProvenancedMerge
                        (sourceProvenancedNode operated)
                        (destinationProvenancedNode other)
                    )
                _ ->
                  Left
                    ( InternalError
                        ( "source/destination witness origin attached to non-Merge operation: "
                            ++ show op
                        )
                    )
            Just DestinationSourceGraftOperation ->
              case op of
                OpGraft argument binder ->
                  Right
                    ( ProvenancedGraft
                        (retainedDestinationProvenancedNode argument)
                        (sourceProvenancedNode binder)
                    )
                _ ->
                  Left
                    ( InternalError
                        ( "destination/source witness origin attached to non-Graft operation: "
                            ++ show op
                        )
                    )
            Just FlexibleTerminalSourceOperation ->
              case op of
                OpRaise operated ->
                  Right
                    ( ProvenancedRaise
                        (flexibleTerminalSourceProvenancedNode operated)
                    )
                OpMerge operated other ->
                  Right
                    ( ProvenancedMerge
                        (flexibleTerminalSourceProvenancedNode operated)
                        (sourceProvenancedNode other)
                    )
                _ ->
                  Left
                    ( InternalError
                        ( "flexible terminal witness certificate attached to unsupported operation: "
                            ++ show op
                        )
                    )
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
        rawOps = getInstanceOps (ewWitness w0)
        edgeRaiseAuthoritySourceKeys =
          eeaRaiseAuthorityNodes artifacts
        nonSourceOpOrigins =
          eeaNonSourceOpOrigins artifacts
        edgeWeakenReplayCertificates =
          IntMap.findWithDefault IntMap.empty eid weakenReplayCertificates
        traceInteriorKeys =
          case traceInterior of
            EdgeSourceInterior (InteriorNodes s) -> s
        isReplayDomainBinder nid =
          let targetC = canonical nid
              knownSources =
                IntMap.findWithDefault
                  IntSet.empty
                  (getNodeId targetC)
                  sourcesByDestination
           in isExactTyVar targetC
                && IntSet.null (IntSet.intersection knownSources sourceBinderKeySet)
    forM_ (IntMap.toList edgeWeakenReplayCertificates) $ \(sourceKey, certificate) ->
      let source = NodeId sourceKey
          mbCopied = lookupCopy source copyMap
          rawWitnessContainsWeaken = OpWeaken source `elem` rawOps
          traceContainsSource =
            any ((== source) . fst) sourceEntriesInOrder
          artifactMatches =
            case mbCopied of
              Nothing -> False
              Just copied ->
                weakenReplayCertificateMatches
                  canonical
                  source
                  copied
                  resultRoot
                  certificate
       in when (not rawWitnessContainsWeaken || not traceContainsSource || not artifactMatches) $
            throwError $
              InternalError $
                "invalid construction-time Weaken replay certificate for edge "
                  ++ show (EdgeId eid)
                  ++ ", source "
                  ++ show source
    let certifiedReplayEntries =
          [ (sourceEntry, weakenReplayCertificateReplayBinder certificate)
            | sourceEntry@(sourceBinder, _arg) <- sourceEntriesInOrder,
              Just certificate <-
                [ IntMap.lookup
                    (getNodeId sourceBinder)
                    edgeWeakenReplayCertificates
                ]
          ]
    let binderArgs = IntMap.fromList binderArgEntries
        -- Trace source entries still drive replay-contract completeness below.
        -- Binder arguments are metadata for normalization rules; they never
        -- enlarge the destination-owned I(etResultRoot).
        normalizationBinderArgs =
          IntMap.filterWithKey
            ( \binderKey _arg ->
                IntSet.member binderKey liveNodeKeys
            )
            binderArgs
        sourceRootC = canonical sourceRoot
    directBinders <- cachedOrderedBinders snapshot sourceRootC
    bindersOrdered <-
      case NodeAccess.lookupNode c0 sourceRootC of
        Just TyVar {tnBound = Just bnd} -> do
          viaBound <- cachedOrderedBinders snapshot bnd
          pure (if null directBinders then viaBound else directBinders)
        Just TyMu {tnBody = muBody} -> do
          viaMu <- cachedOrderedBinders snapshot muBody
          pure (if null directBinders then viaMu else directBinders)
        _ -> pure directBinders
    let replayBindersAtRoot =
          if null producerReplayBinders
            then
              [ canonical b
              | b <- bindersOrdered,
                isReplayDomainBinder b
              ]
            else producerReplayBinders
    let orderBase = resultRoot
        orderRoot = orderBase
    interiorNorm <- cachedInteriorExact snapshot resultRoot
    let
        projectedFrozenSourceInterior =
          IntSet.fromList
            [ getNodeId (canonical (rewriteNode (NodeId sourceKey)))
            | sourceKey <- IntSet.toList traceInteriorKeys
            ]
        normalizationInterior =
          IntSet.union interiorNorm projectedFrozenSourceInterior
        nSourceBinders = length sourceBindersInOrder
        initialReplayPairs =
          zip sourceBindersInOrder (take nSourceBinders replayBindersAtRoot)
        replayMapRewritten =
          IntMap.fromList
            [ (getNodeId (canonical (rewriteNode sourceBinder)), replayBinder)
              | (sourceBinder, replayBinder) <- initialReplayPairs
            ]
        sourceReplayBinders =
          -- Source binders live in the trace's source-id domain and may have
          -- disappeared from the finalized graph.  The copy map is the
          -- producer-owned bridge into the replay domain; final binding-tree
          -- ownership cannot reconstruct that provenance.
          orderedNubNodes
            [ binderC
              | sourceBinder <- sourceBindersInOrder,
                Just copiedBinder <-
                  [IntMap.lookup (getNodeId sourceBinder) (getCopyMapping copyMap)],
                let binderC = canonical copiedBinder,
                isExactTyVar binderC
            ]
    let isAnnEdge =
          IntSet.member eid (cAnnEdges c0)
        certifiedWeakenSourceKeys =
          IntSet.fromAscList (IntMap.keys edgeWeakenReplayCertificates)
        certifiedWeakenTargetKeys =
          IntSet.fromList
            [ getNodeId (canonical (weakenReplayCertificateTarget certificate))
            | certificate <- IntMap.elems edgeWeakenReplayCertificates
            ]
        certifiedReplayBinderKeys =
          IntSet.fromList
            ( map getNodeId producerReplayBinders
                ++ [ getNodeId replayBinder
                   | (_sourceEntry, replayBinder) <- certifiedReplayEntries
                   ]
            )
        certifiedWeakenDescendants =
          IntMap.fromListWith
            IntSet.union
            [ ( getNodeId (canonical (weakenReplayCertificateTarget certificate))
              , IntSet.fromList
                  [ getNodeId (canonical (NodeId descendantKey))
                  | descendantKey <-
                      IntSet.toList
                        (weakenReplayCertificateDescendants certificate)
                  ]
              )
            | certificate <- IntMap.elems edgeWeakenReplayCertificates
            ]
    ops0 <-
      either throwError pure $
        traverse
          ( \(index, op) ->
              provenancedOpWithOrigin
                (IntMap.lookup index nonSourceOpOrigins)
                op
          )
          (zip [0 ..] rawOps)
    let operandHasFrozenSourceAuthority operand =
          Set.member
            FrozenSourceInterior
            (operandAuthorityDomains (pnProvenance operand))
        operandIsExplicitSourceBinder operand =
          not $
            IntSet.null $
              IntSet.intersection
                (operandSourceCandidates (pnProvenance operand))
                sourceBinderKeySet
        operandCanBeSourceRoot operand =
          IntSet.member
            (getNodeId sourceRoot)
            (operandSourceCandidates (pnProvenance operand))
        -- A source-child Merge can become a destination self-merge after
        -- chi_e quotients the two construction classes.  It remains semantic
        -- only when the operated source owns a quantifier in S(r); otherwise
        -- there is no Phi binder to eliminate and the result type has already
        -- inlined the equality.  Drop that identity before coalescing checks
        -- whether an exterior Merge has a preceding Raise.
        collapsedUnboundSourceMergeIsIdentity op =
          case op of
            ProvenancedMerge operated other ->
              operandHasFrozenSourceAuthority operated
                && operandHasFrozenSourceAuthority other
                && canonical (pnNode operated) == canonical (pnNode other)
                && not (operandIsExplicitSourceBinder operated)
                && not (operandCanBeSourceRoot operated)
            _ -> False
        ops0ForNormalization =
          filter (not . collapsedUnboundSourceMergeIsIdentity) ops0
        ops0Destination = map forgetInstanceOpProvenance ops0
        precomputedDescendants0 =
          IntMap.unionWith
            IntSet.union
            certifiedWeakenDescendants
            ( precomputedDescendantsForOps
                snapshot
                (map forgetInstanceOpProvenance ops0ForNormalization)
            )
    destinationOrderKeys <- cachedOrderKeys c0 canonical orderRoot
    let projectedSourceRoot = canonical (rewriteNode sourceRoot)
        -- The frozen trace stores operation-time source identities, while the
        -- retained source type tree is indexed by their final quotient
        -- representatives.  Traverse that source tree in the quotient, but
        -- publish a certificate for every exact frozen source identity below.
        -- This is the only lawful bridge from operation authority to <P>:
        -- destination reachability cannot reconstruct a source occurrence.
        sourceOrderInterior =
          IntSet.fromList
            [ getNodeId (canonical (NodeId sourceKey))
            | sourceKey <- IntSet.toList traceInteriorKeys
            ]
        sourceOrderKeys =
          Order.orderKeysFromConstraintWith
            canonical
            c0
            sourceRoot
            (Just sourceOrderInterior)
        anchoredSourceOrderKeys = do
          anchor <-
            IntMap.lookup
              (getNodeId projectedSourceRoot)
              destinationOrderKeys
          pure $
            IntMap.fromListWith preferEarlierOrderKey
              [ ( getNodeId (canonical (rewriteNode sourceNode)),
                  anchorSourceOrderKey anchor sourceKeyOrder
                )
                | sourceKey <- IntSet.toList traceInteriorKeys,
                  let sourceNode = NodeId sourceKey,
                  Just sourceKeyOrder <-
                    [ IntMap.lookup
                        (getNodeId (canonical sourceNode))
                        sourceOrderKeys
                    ]
              ]
        orderKeys =
          case anchoredSourceOrderKeys of
            Nothing -> destinationOrderKeys
            Just sourceCertificate ->
              -- Destination occurrences remain authoritative wherever the
              -- final type tree already supplies one.  The source certificate
              -- fills only projected operation nodes that final reachability
              -- cannot see; it must not reorder the destination root or an
              -- existing destination child that shares its quotient class.
              IntMap.union destinationOrderKeys sourceCertificate
        preferEarlierOrderKey left right =
          case Order.compareOrderKey left right of
            LT -> left
            _ -> right
        anchorSourceOrderKey anchor relative =
          Order.OrderKey
            { Order.okDepth = Order.okDepth anchor + Order.okDepth relative,
              Order.okPath = Order.okPath anchor ++ Order.okPath relative
            }
        exactFrozenSource operand =
          let provenance = pnProvenance operand
           in if Set.member FrozenSourceInterior (operandAuthorityDomains provenance)
                then
                  case IntSet.toList (operandSourceCandidates provenance) of
                    [sourceKey] -> Just (NodeId sourceKey)
                    _ -> Nothing
                else
                  Nothing
        collapsedSourceMergeOperands op =
          case op of
            ProvenancedMerge operated other
              | canonical (pnNode operated) == canonical (pnNode other),
                Just operatedSource <- exactFrozenSource operated,
                Just otherSource <- exactFrozenSource other,
                canonical operatedSource /= canonical otherSource ->
                  Just (operatedSource, otherSource)
            _ -> Nothing
        validateCollapsedSourceMergeOrder op =
          case collapsedSourceMergeOperands op of
            Nothing -> Right ()
            Just (operatedSource, otherSource) ->
              let projected = canonical (rewriteNode operatedSource)
               in if IntMap.notMember (getNodeId projected) orderKeys
                    then Left (Witness.MissingOrderKey projected)
                    else
                      case
                        Order.compareNodesByOrderKey
                          sourceOrderKeys
                          (canonical otherSource)
                          (canonical operatedSource)
                      of
                        Right LT -> Right ()
                        Right _ ->
                          Left
                            ( Witness.MergeDirectionInvalid
                                operatedSource
                                otherSource
                            )
                        Left (Order.MissingOrderKey missing) ->
                          Left (Witness.MissingOrderKey missing)
                        Left (Order.EqualKeysDistinctNodes left right) ->
                          Left (Witness.EqualOrderKeysDistinctNodes left right)
    let env =
          OmegaNormalizeEnv
            { oneRoot = canonical resultRoot,
              Witness.interior = normalizationInterior,
              -- Operation authority is frozen before chi_e mutates the
              -- destination graph.  A Raise may deliberately move its copy
              -- out of the final destination interior, so projecting this
              -- set through the finalized quotient would erase its proof.
              Witness.interiorRaw = traceInteriorKeys,
              Witness.weakened = weakened,
              Witness.orderKeys = orderKeys,
              Witness.canonical = canonical,
              Witness.constraint = c0,
              Witness.binderArgs = normalizationBinderArgs,
              Witness.precomputedDescendants = precomputedDescendants0,
              Witness.certifiedWeakens = certifiedWeakenTargetKeys,
              Witness.certifiedRaises = IntSet.empty,
              Witness.certifiedReplayBinders = certifiedReplayBinderKeys,
              Witness.binderReplayMap = replayMapRewritten,
              Witness.replayContract = ReplayContractNone,
              Witness.replayDomainBinders = replayBindersAtRoot,
              Witness.isAnnotationEdge = isAnnEdge
            }
    let operandInAuthorityInterior operand =
          let provenance = pnProvenance operand
              sourceAuthorized =
                Set.member FrozenSourceInterior (operandAuthorityDomains provenance)
                  && not
                    ( IntSet.null
                        ( IntSet.intersection
                            (operandSourceCandidates provenance)
                            traceInteriorKeys
                        )
                    )
              destinationAuthorized =
                Set.member FinalDestinationInterior (operandAuthorityDomains provenance)
                  && IntSet.member
                    (getNodeId (canonical (pnNode operand)))
                    interiorNorm
           in sourceAuthorized || destinationAuthorized
    opsNormRaw <- case normalizeInstanceOpsCoreWithProvenanceBy operandInAuthorityInterior env ops0ForNormalization of
      Right ops' -> pure ops'
      Left err ->
        throwError (WitnessNormalizationError (EdgeId eid) err)
    forM_ opsNormRaw $ \op ->
      case validateCollapsedSourceMergeOrder op of
        Right () -> pure ()
        Left err ->
          throwError (WitnessNormalizationError (EdgeId eid) err)
    let provenancedOperands op =
          case op of
            ProvenancedGraft sigma target -> [sigma, target]
            ProvenancedMerge operated other -> [operated, other]
            ProvenancedRaise target -> [target]
            ProvenancedWeaken target -> [target]
            ProvenancedRaiseMerge operated other -> [operated, other]
    let checkedSource operand =
          case IntSet.toList (operandSourceCandidates (pnProvenance operand)) of
            [source] -> NodeId source
            _ -> pnNode operand
        isCertifiedReplayWeaken operand =
          IntSet.member
            (getNodeId (checkedSource operand))
            certifiedWeakenSourceKeys
            && IntSet.member
              (getNodeId (canonical (pnNode operand)))
              certifiedWeakenTargetKeys
        opsNorm = map forgetInstanceOpProvenance opsNormRaw
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
        normalizedWeakenTargets =
          IntSet.fromList
            [ getNodeId (canonical target)
              | OpWeaken target <- opsNorm
            ]
        sameWitnessWeakenTargets =
          IntSet.fromList
            [ getNodeId (canonical target)
              | OpWeaken target <- ops0Destination
            ]
        copyTargetIsRigidIdentity target =
          let targetC = canonical (pnNode target)
              restored = canonical (checkedSource target)
           in restored /= targetC
                && IntSet.notMember (getNodeId targetC) normalizedWeakenTargets
                && IntSet.notMember (getNodeId targetC) sameWitnessWeakenTargets
                && case Binding.lookupBindParent c0 (typeRef targetC) of
                  Just (_, BindRigid) -> True
                  _ -> False
        -- Expansion/replay domains may be copied under rigid binding edges.
        -- Raise and merge operations whose operated copy node is already
        -- directly rigid denote identity computations (thesis Figure 15.3.4).
        -- Eliminate them while copy provenance is still available: restoring
        -- only the node id can map that restricted copy onto a source node
        -- locked below a rigid ancestor and turn an identity into an invalid
        -- semantic Raise.
        copyRigidOperationIsIdentity op =
          case op of
            ProvenancedRaise target -> copyTargetIsRigidIdentity target
            ProvenancedMerge operated _ -> copyTargetIsRigidIdentity operated
            ProvenancedRaiseMerge operated _ -> copyTargetIsRigidIdentity operated
            _ -> False
        -- Inst-Elim-Mono permits a collapsed root transition to be erased only
        -- for a degenerate source scheme: its frozen expansion domain contains
        -- the root alone.  For a nondegenerate domain (notably K's outer
        -- lambda-body edge), the final UF equality is the result of executing
        -- RaiseMerge itself and therefore cannot be used to prove that the
        -- construction was identity.  Test the edge/root relation as well as
        -- the destination representative so distinct frozen child operations
        -- that share a representative also remain distinct.
        collapsedRootTransitionIsIdentity op =
          case op of
            ProvenancedRaiseMerge operated other ->
              IntSet.null (IntSet.delete (getNodeId sourceRoot) traceInteriorKeys)
                && checkedSource operated == sourceRoot
                && sourceRoot == resultRoot
                && canonical (ewLeft w0) == canonical (ewRight w0)
                && canonical (pnNode operated) == canonical (pnNode other)
            _ -> False
        keepFinalizedOp op =
          not (copyRigidOperationIsIdentity op)
            && not (collapsedRootTransitionIsIdentity op)
            && case op of
              ProvenancedWeaken target
                | isCertifiedReplayWeaken target -> True
              _ ->
                case forgetInstanceOpProvenance op of
                  OpWeaken target ->
                    let targetC = canonical target
                        targetKey = getNodeId targetC
                        rootKey = getNodeId (canonical resultRoot)
                     in targetKey == rootKey
                          || IntSet.member targetKey sourceKeySetSeed
                          || IntSet.member targetKey replayKeySetSeed
                          || abstractBoundShape target
                  _ ->
                    True
        -- A flexible terminal child is raised and merged while constructing
        -- chi_e so that graph unification can continue.  Phi only needs that
        -- transition when the source child is an explicit binder in S(r).
        -- Otherwise no quantifier exists for the operation to eliminate: the
        -- transition is already inlined in the result type and is therefore
        -- the identity computation.  The construction certificate makes this
        -- distinction before source ids are restored from the final quotient.
        certifiedTerminalTransitionIsInlined op =
          let certified operand =
                Set.member
                  WeakenIfTerminalTargetBecomesRigid
                  (operandConstructionCertificates (pnProvenance operand))
              isExplicitSourceBinder operand =
                not $
                  IntSet.null $
                    IntSet.intersection
                      (operandSourceCandidates (pnProvenance operand))
                      sourceBinderKeySet
           in case op of
                ProvenancedRaise operated ->
                  certified operated && not (isExplicitSourceBinder operated)
                ProvenancedRaiseMerge operated _ ->
                  certified operated && not (isExplicitSourceBinder operated)
                _ -> False
        opsNormPrunedWithProvenance =
          filter
            ( \op ->
                keepFinalizedOp op
                  && not (certifiedTerminalTransitionIsInlined op)
            )
            opsNormRaw
        opsNormPruned =
          map forgetInstanceOpProvenance opsNormPrunedWithProvenance
        graftTargetKeys =
          IntSet.fromList
            [ getNodeId (checkedSource target)
              | ProvenancedGraft _ target <- opsNormFinalizedWithProvenance
            ]
        graftTargetCount = IntSet.size graftTargetKeys
        opsNormFinalizedWithProvenance
          | null sourceBindersInOrder,
            null opsNormPruned =
              []
          | otherwise =
              opsNormPrunedWithProvenance
        opsNormFinalized =
          map forgetInstanceOpProvenance opsNormFinalizedWithProvenance
        isInSourceInterior target =
          IntSet.member (getNodeId (checkedSource target)) traceInteriorKeys
        traceProvesRootRaiseMergeNoReplay operated other =
          checkedSource operated == sourceRoot
            && sourceRoot == etRoot trace0
            && IntSet.size (operandSourceCandidates (pnProvenance operated)) == 1
            && IntSet.size (operandSourceCandidates (pnProvenance other)) == 1
            && isInSourceInterior operated
            && not (isInSourceInterior other)
            && null (etBinderArgs trace0)
            && IntMap.null (etBinderReplayMap trace0)
            && null (etReplayDomainBinders trace0)
            && etReplayContract trace0 == ReplayContractNone
        traceProvesRootWeakenRaiseMerge operated other =
          rootWeakenRaiseMergeTraceAuthority
            (checkedSource operated)
            (checkedSource other)
            trace0
        interiorContainsTyMu =
          any
            (`IntSet.member` tyMuNodeKeys)
            (IntSet.toList interiorNorm)
        replayBindersSeededFromInteriorGrafts =
          orderedNubNodes
            [ targetC
              | ProvenancedGraft _ target <- opsNormFinalizedWithProvenance,
                let targetC = canonical (pnNode target),
                isExactTyVar targetC,
                abstractBoundShape (pnNode target),
                isInSourceInterior target,
                checkedSource target /= sourceRoot,
                not (IntSet.member (getNodeId (checkedSource target)) sourceKeySet)
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
              | ProvenancedRaiseMerge source target <- opsNormFinalizedWithProvenance,
                let sourceKey = getNodeId (checkedSource source),
                Just sourceEntry <- [sourceEntryForRestored sourceKey],
                let targetC = canonical (pnNode target),
                isExactTyVar targetC,
                ( null replayBindersAtRoot
                    || targetC `elem` replayBindersAtRoot
                  ),
                not (IntSet.member (getNodeId targetC) sourceKeySet)
            ]
        legacyReplayBinders
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
        legacyReplayBindersWithBoundedGrafts =
          if null legacyReplayBinders
            then []
            else
              orderedNubNodes
                ( legacyReplayBinders
                    ++ [ targetC
                       | ProvenancedGraft _ target <- opsNormFinalizedWithProvenance,
                         let targetC = canonical (pnNode target),
                         isExactTyVar targetC,
                         abstractBoundShape (pnNode target)
                     ]
                )
        -- A construction certificate proves one source/binder replay pair;
        -- it does not replace replay evidence inferred for the rest of the
        -- expansion.  In particular, a certified standalone Weaken must not
        -- hide an unrelated Graft target that was already tracked by the
        -- legacy replay domain.
        replayBindersWithBoundedGrafts =
          orderedNubNodes
            ( legacyReplayBindersWithBoundedGrafts
                ++ map snd certifiedReplayEntries
            )
        hasLegacyReplayCodomain =
          not (null legacyReplayBindersWithBoundedGrafts)
        hasReplayCodomain =
          not (null replayBindersWithBoundedGrafts)
        semanticStrictWithReplayCodomain op =
          case op of
            OpWeaken target -> canonical target /= canonical resultRoot
            OpGraft _ target -> canonical target /= canonical resultRoot
            OpMerge {} -> True
            OpRaiseMerge {} -> True
            OpRaise {} -> False
        provenancedStrictWithReplayCodomain op =
          case op of
            ProvenancedRaiseMerge operated other
              | traceProvesRootRaiseMergeNoReplay operated other -> False
            _ -> semanticStrictWithReplayCodomain (forgetInstanceOpProvenance op)
        replayOperationsRequireCodomain =
          not (null sourceEntriesInOrder)
            || any
              provenancedStrictWithReplayCodomain
              opsNormFinalizedWithProvenance
        legacyStrictWithReplayCodomain =
          hasLegacyReplayCodomain
            && replayOperationsRequireCodomain
        strictWithReplayCodomain =
          hasReplayCodomain
            && replayOperationsRequireCodomain
        rootTransitionWeakenSourceKeys =
          IntSet.fromList
            [ getNodeId (checkedSource rootWeakened)
              | ( ProvenancedWeaken rootWeakened,
                  ProvenancedRaiseMerge operated other
                  ) <-
                  zip
                    opsNormFinalizedWithProvenance
                    (drop 1 opsNormFinalizedWithProvenance),
                checkedSource rootWeakened == checkedSource operated,
                traceProvesRootWeakenRaiseMerge operated other
            ]
        keepNoReplayProjectedOp provenanced =
          case provenanced of
            ProvenancedGraft {} ->
              Nothing
            ProvenancedMerge {} ->
              Just provenanced
            ProvenancedRaiseMerge n m
              | traceProvesRootRaiseMergeNoReplay n m -> Just provenanced
              | isLiveNode (pnNode n) && isLiveNode (pnNode m) -> Just provenanced
              | otherwise -> Nothing
            ProvenancedRaise target
              | checkedSource target == sourceRoot ->
                  Nothing
              | not (isTypeTreeBound (pnNode target)) ->
                  Nothing
              | otherwise ->
                  Just provenanced
            ProvenancedWeaken target
              | isCertifiedReplayWeaken target ->
                  Just provenanced
              | IntSet.member
                  (getNodeId (checkedSource target))
                  rootTransitionWeakenSourceKeys ->
                  Just provenanced
              | not (abstractBoundShape (pnNode target)) ->
                  Nothing
              | IntSet.size sourceKeySet <= 1 ->
                  Nothing
              | not (IntSet.member (getNodeId (checkedSource target)) sourceKeySet) ->
                  Nothing
              | IntSet.member (getNodeId (checkedSource target)) graftTargetKeys ->
                  Nothing
              | graftTargetCount <= 1 ->
                  Nothing
              | checkedSource target == sourceRoot ->
                  Nothing
              | otherwise ->
                  Just provenanced
        opsNoReplayProjectedWithProvenance =
          mapMaybe keepNoReplayProjectedOp opsNormFinalizedWithProvenance
        disallowedNoReplayProvenanced op =
          case op of
            ProvenancedGraft _ target ->
              graftTargetCount <= 1
                && isInSourceInterior target
                && checkedSource target /= sourceRoot
                && not (IntSet.member (getNodeId (checkedSource target)) sourceKeySet)
            ProvenancedMerge {} -> True
            ProvenancedRaiseMerge n m
              | traceProvesRootRaiseMergeNoReplay n m -> False
              | sourceInteriorValidRaiseMerge op -> False
              | otherwise ->
                  isLiveNode (pnNode n) && isLiveNode (pnNode m)
            _ -> False
        residualNoReplayOpWithProvenance
          | traceProvesNoReplay =
              find
                disallowedNoReplayProvenanced
                opsNormFinalizedWithProvenance
          | strictWithReplayCodomain =
              Nothing
          | interiorContainsTyMu =
              Nothing
          | otherwise =
              find
                disallowedNoReplayProvenanced
                opsNormFinalizedWithProvenance
        traceProvesNoReplay =
          null (etBinderArgs trace0)
            && IntMap.null (etBinderReplayMap trace0)
            && null (etReplayDomainBinders trace0)
            && etReplayContract trace0 == ReplayContractNone
        residualNoReplayOp =
          forgetInstanceOpProvenance <$> residualNoReplayOpWithProvenance
        strictNoReplayContract =
          case residualNoReplayOp of
            Nothing ->
              any
                ( \case
                    ProvenancedWeaken target -> checkedSource target /= sourceRoot
                    _ -> False
                )
                opsNoReplayProjectedWithProvenance
            Just _ ->
              False
        strictReplayContract =
          strictWithReplayCodomain || strictNoReplayContract
        replayContract =
          if strictReplayContract
            then ReplayContractStrict
            else ReplayContractNone
        opsNormContractWithProvenance
          | strictWithReplayCodomain =
              opsNormFinalizedWithProvenance
          | otherwise =
              filter
                (not . disallowedNoReplayProvenanced)
                opsNoReplayProjectedWithProvenance
        opsNormContract =
          map forgetInstanceOpProvenance opsNormContractWithProvenance
        raisesByDestination =
          IntMap.fromListWith
            (++)
            [ ( getNodeId (canonical (pnNode operated))
              , [operated]
              )
            | ProvenancedRaise operated <- opsNormContractWithProvenance
            ]
        hasExactSemanticRaiseAuthority operated =
          let provenance = pnProvenance operated
           in Set.member FrozenSourceInterior (operandAuthorityDomains provenance)
                && case IntSet.toList (operandSourceCandidates provenance) of
                  [sourceKey] ->
                    sourceKey /= getNodeId sourceRoot
                      && IntSet.member sourceKey edgeRaiseAuthoritySourceKeys
                  _ -> False
        -- Validation is destination-indexed after quotienting, so one source
        -- certificate may exempt a destination only when every retained Raise
        -- at that destination has one exact, certified frozen source.  This
        -- prevents a legal source operation from authorizing an unrelated
        -- source that happened to collapse to the same final representative.
        certifiedRaiseTargets =
          IntSet.fromList
            [ destinationKey
            | (destinationKey, operatedNodes) <- IntMap.toList raisesByDestination
            , all hasExactSemanticRaiseAuthority operatedNodes
            ]
        contractWeakenTargets =
          IntSet.fromList
            [ getNodeId (canonical target)
              | OpWeaken target <- opsNormContract
            ]
        protectedRigidRaiseTargets =
          IntSet.fromList
            [ targetKey
              | op <- opsNormContract,
                target <-
                  case op of
                    OpRaise operated -> [operated]
                    OpMerge operated _ -> [operated]
                    OpRaiseMerge operated _ -> [operated]
                    _ -> [],
                let targetC = canonical target
                    targetKey = getNodeId targetC,
                IntSet.member targetKey sameWitnessWeakenTargets,
                Just sources <- [IntMap.lookup targetKey sourcesByDestination],
                any ((/= targetC) . canonical . NodeId) (IntSet.toList sources),
                Just (_, BindRigid) <- [Binding.lookupBindParent c0 (typeRef targetC)]
            ]
        validationWeakens =
          [ OpWeaken target
            | OpWeaken target <- ops0Destination,
              IntSet.member
                (getNodeId (canonical target))
                protectedRigidRaiseTargets,
              IntSet.notMember
                (getNodeId (canonical target))
                contractWeakenTargets
          ]
        sourceInteriorValidRaiseMerge op =
          case op of
            ProvenancedRaiseMerge operated other ->
              IntSet.size (operandSourceCandidates (pnProvenance operated)) == 1
                && operandInAuthorityInterior operated
                && not (operandInAuthorityInterior other)
            _ -> False
        -- The plain validator sees only destination I(r).  A RaiseMerge whose
        -- per-operand construction domains prove operated n inside and other
        -- m outside already satisfies the paper condition; after UF both
        -- copies may legitimately share one destination representative,
        -- which would otherwise look like a spurious
        -- RaiseMergeInsideInterior.  Likewise, a Merge between exact frozen
        -- sources can have distinct, correctly ordered source occurrences but
        -- one destination quotient node.  Its direction was checked above
        -- against the frozen source certificate; erasing provenance and
        -- checking one quotient node against itself again would manufacture a
        -- failure.
        opsForValidation =
          map forgetInstanceOpProvenance
            ( filter
                ( \op ->
                    not (sourceInteriorValidRaiseMerge op)
                      && case collapsedSourceMergeOperands op of
                        Nothing -> True
                        Just _ -> False
                )
                opsNormContractWithProvenance
            )
            ++ validationWeakens
        deduplicateSourceEntries =
          reverse
            . snd
            . foldl'
              ( \(seen, acc) entry@(sourceBinder, _arg) ->
                  let key = getNodeId sourceBinder
                   in if IntSet.member key seen
                        then (seen, acc)
                        else (IntSet.insert key seen, entry : acc)
              )
              (IntSet.empty, [])
        legacyActiveSourceEntries
          | not legacyStrictWithReplayCodomain = []
          | not (null replayEntriesSeededFromRaiseMerge) =
              map fst replayEntriesSeededFromRaiseMerge
          | otherwise =
              deduplicateSourceEntries sourceEntriesInOrder
        activeSourceEntries
          | not strictWithReplayCodomain = []
          | otherwise =
              deduplicateSourceEntries
                (legacyActiveSourceEntries ++ map fst certifiedReplayEntries)
        legacyReplayPairs
          | not legacyStrictWithReplayCodomain = []
          | not (null replayEntriesSeededFromRaiseMerge) =
              replayEntriesSeededFromRaiseMerge
          | otherwise =
              zip
                legacyActiveSourceEntries
                (take (length legacyActiveSourceEntries) legacyReplayBindersWithBoundedGrafts)
        certifiedSourceKeys =
          IntSet.fromList
            [ getNodeId sourceBinder
              | ((sourceBinder, _arg), _replayBinder) <- certifiedReplayEntries
            ]
        legacyReplayPairsWithoutCertifiedSources =
          [ replayPair
            | replayPair@((sourceBinder, _arg), _replayBinder) <- legacyReplayPairs,
              IntSet.notMember
                (getNodeId sourceBinder)
                certifiedSourceKeys
          ]
        -- A certificate replaces inferred evidence only for its exact frozen
        -- source.  Preserve every unrelated pair, then prove the resulting
        -- relation is functional before constructing the replay IntMap.
        replayPairCandidates =
          certifiedReplayEntries ++ legacyReplayPairsWithoutCertifiedSources
        replayTargetsBySource =
          IntMap.fromListWith
            IntSet.union
            [ ( getNodeId sourceBinder,
                IntSet.singleton (getNodeId replayBinder)
              )
              | ((sourceBinder, _arg), replayBinder) <- replayPairCandidates
            ]
        nonFunctionalReplaySources =
          [ (sourceKey, replayTargets)
            | (sourceKey, replayTargets) <- IntMap.toList replayTargetsBySource,
              IntSet.size replayTargets > 1
          ]
        isTypeTreeBound target =
          case Binding.lookupBindParent c0 (typeRef (canonical target)) of
            Just (TypeRef _, _) -> True
            Nothing -> False
            Just _ -> False
    case nonFunctionalReplaySources of
      (sourceKey, replayTargets) : _ ->
        throwError $
          WitnessNormalizationError (EdgeId eid) $
            Witness.ReplayMapSourceNonFunctional
              (NodeId sourceKey)
              (map NodeId (IntSet.toAscList replayTargets))
      [] -> pure ()
    let replayTargetBySource =
          IntMap.mapMaybe
            (fmap (NodeId . fst) . IntSet.minView)
            replayTargetsBySource
        replayPairs
          | not strictReplayContract = []
          | otherwise =
              [ (sourceEntry, replayBinder)
                | sourceEntry@(sourceBinder, _arg) <- activeSourceEntries,
                  Just replayBinder <-
                    [ IntMap.lookup
                        (getNodeId sourceBinder)
                        replayTargetBySource
                    ]
              ]
        missingReplaySources =
          [ sourceEntry
            | sourceEntry@(sourceBinder, _arg) <- activeSourceEntries,
              IntMap.notMember
                (getNodeId sourceBinder)
                replayTargetBySource
          ]
    when (strictReplayContract && not (null missingReplaySources)) $
      throwError $
        WitnessNormalizationError (EdgeId eid) $
          Witness.ReplayMapIncomplete (map fst missingReplaySources)
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
        envPost =
          env
            { Witness.binderArgs = activeBinderArgsMap,
              Witness.certifiedRaises = certifiedRaiseTargets,
              Witness.binderReplayMap = replayMapValidation,
              Witness.replayContract = replayContract,
              Witness.certifiedReplayBinders = certifiedReplayBinderKeys,
              Witness.replayDomainBinders = replayBindersWithBoundedGrafts
            }
    -- Rewritten operations and `etResultRoot` are both in the destination
    -- expansion domain.  Source identities are restored only after this
    -- validation; mixing source operands with the destination root would make
    -- ownership depend on accidental numeric aliases.
    case residualNoReplayOp of
      Just op ->
        throwError $
          WitnessNormalizationError
            (EdgeId eid)
            (Witness.ReplayContractNoneRequiresReplay op)
      Nothing -> pure ()
    validationCertificate <-
      case WitnessValidation.certifyNormalizedWitness envPost opsForValidation of
      Left valErr ->
        let restoreValidationNode node =
              case IntMap.lookup (getNodeId (canonical node)) sourcesByDestination of
                Just sources
                  | [source] <- map NodeId (IntSet.toAscList sources) -> source
                _ -> node
            restoreValidationOp op =
              case op of
                OpGraft sigma target ->
                  OpGraft (restoreValidationNode sigma) (restoreValidationNode target)
                OpMerge operated other ->
                  OpMerge (restoreValidationNode operated) (restoreValidationNode other)
                OpRaise target -> OpRaise (restoreValidationNode target)
                OpWeaken target -> OpWeaken (restoreValidationNode target)
                OpRaiseMerge operated other ->
                  OpRaiseMerge (restoreValidationNode operated) (restoreValidationNode other)
            restoredError =
              case valErr of
                Witness.NotTransitivelyFlexBound op target validationRoot ->
                  Witness.NotTransitivelyFlexBound
                    (restoreValidationOp op)
                    (restoreValidationNode target)
                    (restoreValidationNode validationRoot)
                _ -> valErr
         in throwError (WitnessNormalizationError (EdgeId eid) restoredError)
      Right certificate -> pure certificate
    let interiorSourceKeys =
          case traceInterior of
            EdgeSourceInterior (InteriorNodes s) -> s
    restoredOps <-
      case
          restoreNormalizedInstanceOps
            c0
            canonical
            interiorSourceKeys
            replayMapReplayToSource
            strictReplayContract
            sourceRoot
            opsNormContractWithProvenance
        of
        Left restoreErr ->
          throwError (WitnessNormalizationError (EdgeId eid) restoreErr)
        Right restored -> pure restored
    let provenanceDomain =
          IntSet.unions
            [ operandSourceCandidates (pnProvenance operand)
              | op <- opsNormContractWithProvenance,
                operand <- provenancedOperands op
            ]
        retainedDestinationDomain =
          IntSet.fromList
            [ getNodeId (canonical (pnNode operand))
              | op <- opsNormContractWithProvenance,
                operand <- provenancedOperands op,
                Set.member
                  RetainedFinalDestination
                  (operandAuthorityDomains (pnProvenance operand))
            ]
        replaySourceDomain =
          IntSet.fromList (IntMap.keys replayMapSourceFinal)
        replayTargetDomain =
          IntSet.fromList (map getNodeId replayBindersWithBoundedGrafts)
        finalSourceReplayDomain =
          IntSet.unions
            [ provenanceDomain,
              replaySourceDomain,
              replayTargetDomain,
              retainedDestinationDomain
            ]
        trace' =
          trace0
            { -- The binder/argument bridge is frozen producer provenance.  In
              -- particular, an argument may now have a different final
              -- representative; solved-graph consumers canonicalize it
              -- locally instead of erasing the node chosen while constructing
              -- chi_e.
              etReplayContract = replayContract,
              etBinderReplayMap = replayMapSourceFinal,
              etReplayDomainBinders =
                if strictReplayContract
                  then replayBindersWithBoundedGrafts
                  else []
            }
    validatedOps <-
      case
          certifyRestoredInstanceOps
            validationCertificate
            finalSourceReplayDomain
            sourceRoot
            (\operated exterior ->
                rootRaiseMergeTraceAuthority operated exterior trace'
            )
            (\operated exterior ->
                rootWeakenRaiseMergeTraceAuthority operated exterior trace'
            )
            restoredOps
        of
          Left validationError ->
            throwError
              (WitnessNormalizationError (EdgeId eid) validationError)
          Right certifiedOps -> pure certifiedOps
    let iw = mkInstanceWitness validatedOps
        witness' =
          mkEdgeWitness
            (ewEdgeId w0)
            (ewLeft w0)
            (ewRight w0)
            (ewRoot w0)
            (ewForallIntros w0)
            iw
    pure
      ( eid,
        artifacts
          { eeaWitness = witness',
            eeaTrace = trace'
          }
      )
    ) emptyWitnessNormCache
  let normalizedExecutionArtifacts = IntMap.fromList witnessResults
  publishedArtifacts <-
    case
        edgeArtifactsFromExecutionArtifacts
          normalizedExecutionArtifacts
          IntSet.empty
      of
        Left err -> throwError (InvalidEdgeArtifacts err)
        Right artifacts -> pure artifacts
  modify' $ \st ->
    st
      { psEdgeExecutionArtifacts = normalizedExecutionArtifacts
      }
  pure (NormalizedEdgeArtifacts publishedArtifacts)
