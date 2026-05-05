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
    binderArgsFromExpansion,
    edgeWitnessPlan,
    buildEdgeWitness,
    buildEdgeTrace,
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
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.Base (CopyMap, EdgeTrace (..), FrontierSet, InteriorSet, PresolutionError (..), PresolutionM, fromListInterior, instantiationBindersM)
import MLF.Constraint.Presolution.Ops (findRoot, getCanonicalNode, lookupVarBound)
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical, liftBindingError)
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
    NodeRef (TypeRef),
    TyNode (..),
    genRef,
    getNodeId,
    nodeRefFromKey,
  )
import MLF.Constraint.Types.Witness (EdgeWitness (..), Expansion (..), ExpansionF (..), ForallSpec (..), forallSpecBinderCount, InstanceOp (..), InstanceWitness (..), mkEdgeWitness, mkInstanceWitness, ReplayContract (..))
import MLF.Util.RecursionSchemes (cataM)

-- | Precompute the base forall-intro count and ops for a witness.
data EdgeWitnessPlan = EdgeWitnessPlan
  { ewpForallIntros :: Int,
    ewpBaseOps :: [InstanceOp]
  }

-- | Input bundle for building per-edge witness metadata.
data EdgeWitnessInput = EdgeWitnessInput
  { -- | The edge being witnessed
    ewiEdgeId :: EdgeId,
    -- | Source (left) node of the edge
    ewiSrcNode :: NodeId,
    -- | Target (right) node of the edge
    ewiTgtNode :: NodeId,
    -- | Raw type at the source node before canonicalization
    ewiLeftRaw :: TyNode,
    -- | Nesting depth for forall-intro tracking
    ewiDepth :: Int
  }

edgeWitnessPlan :: GenNodeId -> NodeId -> TyNode -> Expansion -> PresolutionM EdgeWitnessPlan
edgeWitnessPlan gid leftId leftRaw expn = do
  let root = case leftRaw of
        TyExp {tnBody = b} -> b
        _ -> leftId
  (introCount, baseOps) <- witnessFromExpansion gid root leftRaw expn
  pure EdgeWitnessPlan {ewpForallIntros = introCount, ewpBaseOps = baseOps}

buildEdgeWitness ::
  EdgeWitnessInput ->
  [InstanceOp] ->
  [InstanceOp] ->
  PresolutionM EdgeWitness
buildEdgeWitness input baseOps extraOps = do
  let eid = ewiEdgeId input
      left = ewiSrcNode input
      right = ewiTgtNode input
      leftRaw = ewiLeftRaw input
      introCount = ewiDepth input
      root = case leftRaw of
        TyExp {tnBody = b} -> b
        _ -> left
      (intros, ops) = integratePhase2Steps (introCount, baseOps) extraOps
  let iw = mkInstanceWitness ops
  case mkEdgeWitness eid left right root intros iw of
    Left err -> throwError (InternalError ("buildEdgeWitness: " ++ show err))
    Right w -> pure w

buildEdgeTrace ::
  EdgeWitnessInput ->
  GenNodeId ->
  Expansion ->
  (CopyMap, InteriorSet, FrontierSet) ->
  PresolutionM EdgeTrace
buildEdgeTrace input gid expn (copyMap0, _interior0, _frontier0) = do
  let left = ewiSrcNode input
      leftRaw = ewiLeftRaw input
  bas <- binderArgsFromExpansion gid leftRaw expn
  let rootSeed = case leftRaw of
        TyExp {tnBody = b} -> b
        _ -> left
  root <- findRoot rootSeed
  (constraint0, canonicalizeNode) <- getConstraintAndCanonical
  let interiorRootRef = genRef gid
  interiorNodesRaw <- do
    s <- liftBindingError $ Binding.interiorOfUnder canonicalizeNode constraint0 interiorRootRef
    pure
      [ nid
      | key <- IntSet.toList s,
        TypeRef nid <- [nodeRefFromKey key]
      ]
  let interiorNodes = fromListInterior (map canonicalizeNode interiorNodesRaw)
  pure
    EdgeTrace
      { etRoot = root,
        etBinderArgs = bas,
        etInterior = interiorNodes,
        etReplayContract = ReplayContractNone,
        etBinderReplayMap = mempty,
        etReplayDomainBinders = [],
        etCopyMap = copyMap0
      }

binderArgsFromExpansion :: GenNodeId -> TyNode -> Expansion -> PresolutionM [(NodeId, NodeId)]
binderArgsFromExpansion gid leftRaw expn = do
  let isTyVarBinder nid = do
        n <- getCanonicalNode nid
        pure $
          case n of
            TyVar {} -> True
            _ -> False
      instantiationBinders nid = do
        (_bodyRoot, binders) <- instantiationBindersM gid nid
        filterM isTyVarBinder binders
  let alg layer = case layer of
        ExpIdentityF -> pure []
        ExpForallF _ -> pure []
        ExpComposeF es -> pure (concat (NE.toList es))
        ExpInstantiateF args ->
          case leftRaw of
            TyExp {tnBody = b} -> do
              binders <- instantiationBinders b
              if null binders
                then pure []
                else
                  if length binders > length args
                    then throwError (ArityMismatch "binderArgsFromExpansion/ExpInstantiate" (length binders) (length args))
                    else pure (zip binders (take (length binders) args))
            _ -> do
              binders <- instantiationBinders (tnId leftRaw)
              if null binders
                then pure []
                else
                  if length binders > length args
                    then throwError (ArityMismatch "binderArgsFromExpansion/ExpInstantiate" (length binders) (length args))
                    else pure (zip binders (take (length binders) args))
  cataM alg expn

-- | Convert a presolution expansion recipe into a forall-intro count and omega ops.
witnessFromExpansion :: GenNodeId -> NodeId -> TyNode -> Expansion -> PresolutionM (Int, [InstanceOp])
witnessFromExpansion gid _root leftRaw expn = do
  let (_hasForall, stepper) = cata (witnessAlg leftRaw) expn
  steps <- stepper
  let introCount = fst steps
      ops = snd steps
  pure (introCount, ops)
  where
    witnessAlg ::
      TyNode ->
      ExpansionF (Bool, PresolutionM (Int, [InstanceOp])) ->
      (Bool, PresolutionM (Int, [InstanceOp]))
    witnessAlg lr layer = case layer of
      ExpIdentityF ->
        (False, pure (0, []))
      ExpForallF ls ->
        let count = sum (map forallSpecBinderCount (NE.toList ls))
         in (True, pure (count, []))
      ExpInstantiateF args ->
        ( False,
          do
            case lr of
              TyExp {tnBody = b} -> do
                (_bodyRoot, boundVars) <- instantiationBindersM gid b
                if null boundVars
                  then pure (0, [])
                  else
                    if length boundVars > length args
                      then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                      else do
                        let args' = take (length boundVars) args
                            pairs = zip args' boundVars
                        (grafts, merges, weakens) <- foldM (classify boundVars) ([], [], []) pairs
                        pure (0, grafts ++ merges ++ weakens)
              _ -> do
                pure (0, [])
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
      ([InstanceOp], [InstanceOp], [InstanceOp]) ->
      (NodeId, NodeId) -> -- (arg, binder)
      PresolutionM ([InstanceOp], [InstanceOp], [InstanceOp])
    classify binders (gAcc, mAcc, wAcc) (arg, bv) = do
      mbBound <- binderBound bv
      let weakenOp = [OpWeaken bv]
      case mbBound of
        Nothing ->
          -- Unbounded binder: graft then eliminate later via weaken.
          pure (gAcc ++ [OpGraft arg bv], mAcc, wAcc ++ weakenOp)
        Just bnd -> do
          isVarBound <- isTyVar bnd
          if isVarBound && bnd `elem` binders
            -- Bounded by an in-scope variable: alias + eliminate via Merge (Fig. 10).
            then pure (gAcc, mAcc ++ [OpMerge bv bnd], wAcc)
            -- Bounded by structure: suppress OpGraft (InstBot can't target a
            -- non-⊥ bound, Def. 15.3.4) but emit OpWeaken to eliminate the
            -- quantifier via InstElim — thesis-exact behavior.
            else pure (gAcc, mAcc, wAcc ++ weakenOp)

    binderBound :: NodeId -> PresolutionM (Maybe NodeId)
    binderBound bv = do
      n <- getCanonicalNode bv
      case n of
        TyVar {} ->
          lookupVarBound bv
        _ -> pure Nothing

    isTyVar :: NodeId -> PresolutionM Bool
    isTyVar nid = do
      n <- getCanonicalNode nid
      pure $ case n of
        TyVar {} -> True
        _ -> False

integratePhase2Ops :: [InstanceOp] -> [InstanceOp] -> [InstanceOp]
integratePhase2Ops baseOps extraOps =
  let isBarrier = \case
        OpRaise {} -> True
        _ -> False

      isGraft = \case
        OpGraft {} -> True
        _ -> False

      isWeaken = \case
        OpWeaken {} -> True
        _ -> False

      isMergeLike = \case
        OpMerge {} -> True
        OpRaiseMerge {} -> True
        _ -> False

      elimBinderByMerge = \case
        OpMerge n _ -> Just n
        OpRaiseMerge n _ -> Just n
        _ -> Nothing

      elimBinder = \case
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
          ( \case
              OpRaise {} -> True
              _ -> False
          )
          extraOps

      raisesByBinder0 =
        foldl'
          ( \m op -> case op of
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

      elimKey op = case op of
        OpMerge n _ -> getNodeId n
        OpRaiseMerge n _ -> getNodeId n
        _ -> -1

      mergesSorted = concat (sortOn (Down . elimKey . last) mergesAll)
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
   in grafts ++ mergesSorted ++ others ++ leftoverRaises ++ weakensWithRaises ++ afterBarrier

-- | Integrate phase-2 ops into a witness. The intro count passes through
-- unchanged; phase-2 ops are merged into the ops list.
integratePhase2Steps :: (Int, [InstanceOp]) -> [InstanceOp] -> (Int, [InstanceOp])
integratePhase2Steps (introCount, baseOps) extraOps =
  (introCount, integratePhase2Ops baseOps extraOps)
