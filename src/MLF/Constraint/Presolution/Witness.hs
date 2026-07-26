{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Constraint.Presolution.Witness
-- Description : Witness (Ω) helpers for presolution
--
-- This module hosts helper routines for constructing and normalizing the
-- instance-operation witnesses that presolution records for later Φ/Σ translation.
--
-- These helpers keep witness assembly isolated while the public presolution
-- entrypoint stays focused on orchestration.
module MLF.Constraint.Presolution.Witness
  ( EdgeWitnessInput (..),
    EdgeWitnessPlan (..),
    EdgeWitnessOp (..),
    EdgeWitnessNonSourceOrigin (..),
    edgeWitnessInstanceOp,
    binderArgsFromExpansion,
    binderArgsFromKnownBinders,
    filterTyVarBinders,
    edgeWitnessPlan,
    edgeWitnessPlanFromBinders,
    buildEdgeWitness,
    buildEdgeTrace,
    integrateEdgeWitnessOps,
    integratePhase2Ops,
    integratePhase2Steps,
    witnessFromExpansion,
    normalizeInstanceOpsCore,
    normalizeInstanceOpsFull,
    coalesceRaiseMergeWithEnv,
    reorderWeakenWithEnv,
    assertNoStandaloneGrafts,
    validateNormalizedWitness,
    OmegaNormalizeEnv (..),
    OmegaNormalizeError (..),
  )
where

import Control.Monad (filterM, foldM)
import Control.Monad.Except (throwError)
import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (mapAccumL, partition, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Ord (Down (..))
import Numeric.Natural (Natural)
import MLF.Constraint.Presolution.Base (CopyMap, EdgeSourceInterior (..), EdgeTrace (..), EdgeWitnessNonSourceOrigin (..), PresolutionError (..), PresolutionM, instantiationBindersM, lookupCopy)
import MLF.Constraint.Presolution.Ops (getCanonicalNode, lookupVarBound)
import MLF.Constraint.Presolution.WitnessCanon
  ( assertNoStandaloneGrafts,
    coalesceRaiseMergeWithEnv,
    normalizeInstanceOpsCore,
    normalizeInstanceOpsFull,
    reorderWeakenWithEnv,
  )
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeEnv (..), OmegaNormalizeError (..), validateNormalizedWitness)
import MLF.Constraint.Types.Graph
  ( EdgeId,
    GenNodeId,
    NodeId,
    TyNode (..),
    getNodeId,
  )
import qualified MLF.Constraint.Types.Witness.Internal as WitnessInternal
import MLF.Constraint.Types.Witness
  ( EdgeWitness,
    Expansion (..),
    ExpansionF (..),
    InstanceOp (..),
    ReplayContract (..),
    forallSpecBinderCount,
    mkEdgeWitness
  )
import MLF.Util.RecursionSchemes (cataM)

-- | Precompute the base forall-intro count and ops for a witness.
data EdgeWitnessPlan = EdgeWitnessPlan
  { ewpForallIntros :: Natural,
    ewpBaseOps :: [EdgeWitnessOp]
  }

-- | The node-id domain in which an operation was emitted.
--
-- Expansion-derived binder operations normally name the frozen source graph,
-- but an instantiation Graft is deliberately mixed: its type argument belongs
-- to the destination graph while its quantified binder belongs to the source.
-- Operations emitted while executing a constructed chi_e normally name its
-- destination copy, except for identity expansion, which executes structural
-- equalities in place.  Carry the domain with each operation so witness
-- normalization never has to guess it from the finalized graph.
data EdgeWitnessOp
  = SourceEdgeWitnessOp InstanceOp
  | DestinationEdgeWitnessOp InstanceOp
  | SourceDestinationEdgeWitnessMerge NodeId NodeId
  | DestinationSourceEdgeWitnessGraft NodeId NodeId
  | FlexibleTerminalSourceEdgeWitnessOp InstanceOp
  deriving (Eq, Show)

edgeWitnessInstanceOp :: EdgeWitnessOp -> InstanceOp
edgeWitnessInstanceOp edgeOp =
  case edgeOp of
    SourceEdgeWitnessOp op -> op
    DestinationEdgeWitnessOp op -> op
    SourceDestinationEdgeWitnessMerge operated other -> OpMerge operated other
    DestinationSourceEdgeWitnessGraft argument binder -> OpGraft argument binder
    FlexibleTerminalSourceEdgeWitnessOp op -> op

-- | Input bundle for building per-edge witness metadata.
data EdgeWitnessInput = EdgeWitnessInput
  { -- | The edge being witnessed
    ewiEdgeId :: EdgeId,
    -- | Source (left) node of the edge
    ewiSrcNode :: NodeId,
    -- | Target (right) node of the edge
    ewiTgtNode :: NodeId,
    -- | Frozen source root selected by edge planning.  This is the identity
    -- named by expansion-derived operations and may differ from the raw
    -- TyExp body's administrative node.
    ewiRoot :: NodeId,
    -- | Nesting depth for forall-intro tracking
    ewiDepth :: Natural
  }

edgeWitnessPlan :: GenNodeId -> NodeId -> TyNode -> Expansion -> PresolutionM p EdgeWitnessPlan
edgeWitnessPlan gid _leftId leftRaw expn = do
  boundVars <-
    if expansionHasInstantiate expn
      then case leftRaw of
        TyExp {tnBody = b} -> do
          (_bodyRoot, binders) <- instantiationBindersM gid b
          pure binders
        _ -> pure []
      else pure []
  (introCount, baseOps) <- witnessOpsFromExpansionWithBinders boundVars expn
  pure EdgeWitnessPlan {ewpForallIntros = introCount, ewpBaseOps = baseOps}

edgeWitnessPlanFromBinders :: [NodeId] -> Expansion -> PresolutionM p EdgeWitnessPlan
edgeWitnessPlanFromBinders binders expn = do
  (introCount, baseOps) <- witnessOpsFromExpansionWithBinders binders expn
  pure EdgeWitnessPlan {ewpForallIntros = introCount, ewpBaseOps = baseOps}

buildEdgeWitness ::
  EdgeWitnessInput ->
  [EdgeWitnessOp] ->
  [EdgeWitnessOp] ->
  PresolutionM p (EdgeWitness, IntMap.IntMap EdgeWitnessNonSourceOrigin)
buildEdgeWitness input baseOps extraOps = do
  let eid = ewiEdgeId input
      left = ewiSrcNode input
      right = ewiTgtNode input
      root = ewiRoot input
      introCount = ewiDepth input
      (ops, nonSourceOpOrigins) =
        integrateTaggedEdgeWitnessOps root baseOps extraOps
      intros = introCount
  let iw = WitnessInternal.mkUncheckedInstanceWitness ops
  pure (mkEdgeWitness eid left right root intros iw, nonSourceOpOrigins)

-- | Integrate source-domain expansion ops with execution-emitted ops while
-- keeping destination-domain indices aligned with the final reordered list.
integrateEdgeWitnessOps ::
  NodeId ->
  [InstanceOp] ->
  [EdgeWitnessOp] ->
  ([InstanceOp], IntMap.IntMap EdgeWitnessNonSourceOrigin)
integrateEdgeWitnessOps root baseOps extraOps =
  integrateTaggedEdgeWitnessOps root (map SourceEdgeWitnessOp baseOps) extraOps

-- | Integrate already domain-tagged expansion operations.  Unlike the public
-- plain-op adapter above, this is the construction path used by edge plans:
-- an 'ExpInstantiate' Graft carries a destination-domain argument and a
-- frozen-source binder from the moment it is produced.
integrateTaggedEdgeWitnessOps ::
  NodeId ->
  [EdgeWitnessOp] ->
  [EdgeWitnessOp] ->
  ([InstanceOp], IntMap.IntMap EdgeWitnessNonSourceOrigin)
integrateTaggedEdgeWitnessOps root baseOps extraOps =
  let taggedOps =
        integratePhase2OpsBy
          (Just root)
          edgeWitnessInstanceOp
          baseOps
          extraOps
      ops = map edgeWitnessInstanceOp taggedOps
      nonSourceOpOrigins =
        IntMap.fromList
          [ (index, origin)
          | (index, taggedOp) <- zip [0 :: Int ..] taggedOps
          , origin <- case taggedOp of
              SourceEdgeWitnessOp _ -> []
              DestinationEdgeWitnessOp _ -> [DestinationEdgeOperation]
              SourceDestinationEdgeWitnessMerge _ _ -> [SourceDestinationMergeOperation]
              DestinationSourceEdgeWitnessGraft _ _ -> [DestinationSourceGraftOperation]
              FlexibleTerminalSourceEdgeWitnessOp _ -> [FlexibleTerminalSourceOperation]
          ]
   in (ops, nonSourceOpOrigins)

buildEdgeTrace ::
  NodeId ->
  EdgeSourceInterior ->
  [(NodeId, NodeId)] ->
  NodeId ->
  CopyMap ->
  PresolutionM p EdgeTrace
buildEdgeTrace sourceRoot sourceInterior bas resultRoot0 copyMap0 = do
  pure
    EdgeTrace
      { etRoot = sourceRoot,
        etResultRoot = resultRoot0,
        etBinderArgs = bas,
        etInterior = sourceInterior,
        etReplayContract = ReplayContractNone,
        etBinderReplayMap = mempty,
        etReplayDomainBinders =
          [ copiedBinder
          | (sourceBinder, _argument) <- bas,
            Just copiedBinder <- [lookupCopy sourceBinder copyMap0]
          ],
        etCopyMap = copyMap0
      }

binderArgsFromExpansion :: GenNodeId -> TyNode -> Expansion -> PresolutionM p [(NodeId, NodeId)]
binderArgsFromExpansion gid leftRaw expn =
  if expansionHasInstantiate expn
    then do
      let instantiationBinders nid = do
            (_bodyRoot, binders) <- instantiationBindersM gid nid
            filterTyVarBinders binders
      binders <- case leftRaw of
        TyExp {tnBody = b} -> instantiationBinders b
        _ -> instantiationBinders (tnId leftRaw)
      binderArgsFromKnownBinders "binderArgsFromExpansion/ExpInstantiate" binders expn
    else pure []

filterTyVarBinders :: [NodeId] -> PresolutionM p [NodeId]
filterTyVarBinders =
  filterM $ \nid -> do
    n <- getCanonicalNode nid
    pure $
      case n of
        TyVar {} -> True
        _ -> False

binderArgsFromKnownBinders :: String -> [NodeId] -> Expansion -> PresolutionM p [(NodeId, NodeId)]
binderArgsFromKnownBinders context binders expn =
  cataM alg expn
 where
  alg :: ExpansionF [(NodeId, NodeId)] -> PresolutionM q [(NodeId, NodeId)]
  alg layer = case layer of
    ExpIdentityF -> pure []
    ExpForallF _ -> pure []
    ExpComposeF es -> pure (concat (NE.toList es))
    ExpInstantiateF args ->
      if length binders /= length args
        then throwError (ArityMismatch context (length binders) (length args))
        else
          pure (zip binders args)

-- | Convert a presolution expansion recipe into a forall-intro count and omega ops.
witnessFromExpansion :: GenNodeId -> NodeId -> TyNode -> Expansion -> PresolutionM p (Natural, [InstanceOp])
witnessFromExpansion gid _root leftRaw expn = do
  boundVars <-
    if expansionHasInstantiate expn
      then case leftRaw of
        TyExp {tnBody = b} -> do
          (_bodyRoot, binders) <- instantiationBindersM gid b
          pure binders
        _ -> pure []
      else pure []
  (introCount, taggedOps) <- witnessOpsFromExpansionWithBinders boundVars expn
  pure (introCount, map edgeWitnessInstanceOp taggedOps)

witnessOpsFromExpansionWithBinders :: [NodeId] -> Expansion -> PresolutionM p (Natural, [EdgeWitnessOp])
witnessOpsFromExpansionWithBinders boundVars expn = do
  let (_hasForall, stepper) = cata witnessAlg expn
  steps <- stepper
  let introCount = fst steps
      ops = snd steps
  pure (introCount, ops)
  where
    witnessAlg ::
      ExpansionF (Bool, PresolutionM p (Natural, [EdgeWitnessOp])) ->
      (Bool, PresolutionM p (Natural, [EdgeWitnessOp]))
    witnessAlg layer = case layer of
      ExpIdentityF ->
        (False, pure (0, []))
      ExpForallF ls ->
        let count = sum (map (fromIntegral . forallSpecBinderCount) (NE.toList ls))
         in (True, pure (count, []))
      ExpInstantiateF args ->
        ( False,
          if length boundVars /= length args
            then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
            else
              do
                let pairs = zip args boundVars
                (grafts, merges, weakens) <- foldM (classify boundVars) ([], [], []) pairs
                pure (0, grafts ++ merges ++ weakens)
        )
      ExpComposeF es ->
        let children = NE.toList es
            hasForall = or (map fst children)
         in ( hasForall,
              do
                results <- mapM snd children
                let totalIntros = sum (map fst results)
                    allOps = concatMap snd results
                pure (totalIntros, allOps)
            )

    classify ::
      [NodeId] -> -- binders at this instantiation site
      ([EdgeWitnessOp], [EdgeWitnessOp], [EdgeWitnessOp]) ->
      (NodeId, NodeId) -> -- (arg, binder)
      PresolutionM p ([EdgeWitnessOp], [EdgeWitnessOp], [EdgeWitnessOp])
    classify binders (gAcc, mAcc, wAcc) (arg, bv) = do
      mbBound <- binderBound bv
      let weakenOp = [SourceEdgeWitnessOp (OpWeaken bv)]
      case mbBound of
        Nothing ->
          -- Unbounded binder: graft then eliminate later via weaken.
          pure
            ( gAcc ++ [DestinationSourceEdgeWitnessGraft arg bv]
            , mAcc
            , wAcc ++ weakenOp
            )
        Just bnd -> do
          isVarBound <- isTyVar bnd
          if isVarBound && bnd `elem` binders
            -- Bounded by an in-scope variable: alias + eliminate via Merge (Fig. 10).
            then pure (gAcc, mAcc ++ [SourceEdgeWitnessOp (OpMerge bv bnd)], wAcc)
            -- Bounded by structure: suppress OpGraft (InstBot can't target a
            -- non-⊥ bound, Def. 15.3.4) but emit OpWeaken to eliminate the
            -- quantifier via InstElim — thesis-exact behavior.
            else pure (gAcc, mAcc, wAcc ++ weakenOp)

    binderBound :: NodeId -> PresolutionM p (Maybe NodeId)
    binderBound bv = do
      n <- getCanonicalNode bv
      case n of
        TyVar {} ->
          lookupVarBound bv
        _ -> pure Nothing

    isTyVar :: NodeId -> PresolutionM p Bool
    isTyVar nid = do
      n <- getCanonicalNode nid
      pure $ case n of
        TyVar {} -> True
        _ -> False

expansionHasInstantiate :: Expansion -> Bool
expansionHasInstantiate =
  cata $ \case
    ExpIdentityF -> False
    ExpForallF _ -> False
    ExpInstantiateF _ -> True
    ExpComposeF es -> or (NE.toList es)

integratePhase2Ops :: [InstanceOp] -> [InstanceOp] -> [InstanceOp]
integratePhase2Ops = integratePhase2OpsBy Nothing id

integratePhase2OpsBy :: Maybe NodeId -> (op -> InstanceOp) -> [op] -> [op] -> [op]
integratePhase2OpsBy mbRoot project baseOps extraOps =
  let isBarrier = \case
        op -> case project op of
          OpRaise {} -> True
          _ -> False

      isGraft = \case
        op -> case project op of
          OpGraft {} -> True
          _ -> False

      isWeaken = \case
        op -> case project op of
          OpWeaken {} -> True
          _ -> False

      isMergeLike = \case
        op -> case project op of
          OpMerge {} -> True
          OpRaiseMerge {} -> True
          _ -> False

      elimBinderByMerge op = case project op of
        OpMerge n _ -> Just n
        OpRaiseMerge n _ -> Just n
        _ -> Nothing

      elimBinder op = case project op of
        OpMerge n _ -> Just n
        OpRaiseMerge n _ -> Just n
        OpWeaken n -> Just n
        _ -> Nothing

      baseMerged =
        IntSet.fromList
          [ getNodeId n
          | op <- baseOps,
            Just n <- [elimBinderByMerge op]
          ]

      (extraRaises, extraOps') =
        partition
          ( \op -> case project op of
              OpRaise {} -> True
              _ -> False
          )
          extraOps

      raisesByBinder0 =
        foldl'
          ( \m op -> case project op of
              OpRaise n -> IntMap.insertWith (++) (getNodeId n) [op] m
              _ -> m
          )
          IntMap.empty
          extraRaises

      extraElimOps =
        [ op
        | op <- extraOps',
          Just n <- [elimBinder op],
          not (IntSet.member (getNodeId n) baseMerged)
        ]

      (beforeBarrier, afterBarrier) = break isBarrier baseOps

      grafts = [op | op <- beforeBarrier, isGraft op]
      weakens = [op | op <- beforeBarrier, isWeaken op]
      mergesBase = [op | op <- beforeBarrier, isMergeLike op]
      others = [op | op <- beforeBarrier, not (isGraft op || isWeaken op || isMergeLike op)]

      takeRaises raisesMap n =
        case IntMap.lookup (getNodeId n) raisesMap of
          Nothing -> ([], raisesMap)
          Just rs -> (rs, IntMap.delete (getNodeId n) raisesMap)

      mergeBlock raisesMap op = case elimBinder op of
        Just n ->
          let (rs, raisesMap') = takeRaises raisesMap n
           in (raisesMap', rs ++ [op])
        Nothing -> (raisesMap, [op])

      (raisesAfterBaseMerges, mergesBaseBlocks) =
        mapAccumL mergeBlock raisesByBinder0 mergesBase

      (raisesAfterExtraMerges, extraElimBlocks) =
        mapAccumL mergeBlock raisesAfterBaseMerges extraElimOps

      mergesAll = mergesBaseBlocks ++ extraElimBlocks

      elimKey op = case project op of
        OpMerge n _ -> getNodeId n
        OpRaiseMerge n _ -> getNodeId n
        _ -> -1

      rootWeakenPresent =
        case mbRoot of
          Nothing -> False
          Just root ->
            any
              (\op -> case project op of
                  OpWeaken target -> target == root
                  _ -> False
              )
              (weakens ++ extraElimOps)

      (terminalRootMergeBlocks, ordinaryMergeBlocks) =
        case (mbRoot, rootWeakenPresent) of
          (Just root, True) ->
            partition
              (\block -> elimKey (last block) == getNodeId root)
              mergesAll
          _ -> ([], mergesAll)

      mergesSorted = concat (sortOn (Down . elimKey . last) ordinaryMergeBlocks)
      terminalRootMerges =
        concat (sortOn (Down . elimKey . last) terminalRootMergeBlocks)
      (raisesAfterWeakens, weakensWithRaises) =
        foldl'
          ( \(raisesMap, acc) op -> case elimBinder op of
              Just n
                | isWeaken op ->
                    let (rs, raisesMap') = takeRaises raisesMap n
                     in (raisesMap', acc ++ rs ++ [op])
              _ -> (raisesMap, acc ++ [op])
          )
          (raisesAfterExtraMerges, [])
          weakens

      leftoverRaises = concat (IntMap.elems raisesAfterWeakens)
   in grafts
        ++ mergesSorted
        ++ others
        ++ leftoverRaises
        ++ weakensWithRaises
        ++ terminalRootMerges
        ++ afterBarrier

-- | Integrate phase-2 ops into a witness. The intro count passes through
-- unchanged; phase-2 ops are merged into the ops list.
integratePhase2Steps :: (Natural, [InstanceOp]) -> [InstanceOp] -> (Natural, [InstanceOp])
integratePhase2Steps (introCount, baseOps) extraOps =
  (introCount, integratePhase2Ops baseOps extraOps)
