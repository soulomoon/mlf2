{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Witness
Description : Witness (Ω) helpers for presolution

This module hosts helper routines for constructing and normalizing the
instance-operation witnesses that presolution records for later Φ/Σ translation.

These helpers are extracted from `MLF.Constraint.Presolution.Core` so `Core` can
shrink into a driver/wiring layer over time.
-}
module MLF.Constraint.Presolution.Witness (
    binderArgsFromExpansion,
    integratePhase2Ops,
    integratePhase2Steps,
    witnessFromExpansion,
    normalizeInstanceOpsFull,
    coalesceRaiseMergeWithEnv,
    reorderWeakenWithEnv,
    assertNoStandaloneGrafts,
    validateNormalizedWitness,
    OmegaNormalizeEnv(..),
    OmegaNormalizeError(..)
) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import Data.List (mapAccumL, partition, sortOn)
import Data.Ord (Down(..))
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types.Graph
    ( GenNodeId
    , NodeId
    , TyNode(..)
    , getNodeId
    )
import MLF.Constraint.Types.Witness (Expansion(..), ExpansionF(..), ForallSpec(..), InstanceOp(..))
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), instantiationBindersM)
import MLF.Constraint.Presolution.Ops (getCanonicalNode, lookupVarBound)
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeEnv(..), OmegaNormalizeError(..), validateNormalizedWitness)
import MLF.Constraint.Presolution.WitnessCanon (normalizeInstanceOpsFull, coalesceRaiseMergeWithEnv, reorderWeakenWithEnv, assertNoStandaloneGrafts)
import MLF.Util.RecursionSchemes (cataM)

binderArgsFromExpansion :: GenNodeId -> TyNode -> Expansion -> PresolutionM [(NodeId, NodeId)]
binderArgsFromExpansion gid leftRaw expn = do
    let instantiationBinders nid = do
            (_bodyRoot, binders) <- instantiationBindersM gid nid
            pure binders
    let alg layer = case layer of
            ExpIdentityF -> pure []
            ExpForallF _ -> pure []
            ExpComposeF es -> pure (concat (NE.toList es))
            ExpInstantiateF args ->
                case leftRaw of
                    TyExp{ tnBody = b } -> do
                        binders <- instantiationBinders b
                        if null binders
                            then pure []
                        else if length binders > length args
                            then throwError (ArityMismatch "binderArgsFromExpansion/ExpInstantiate" (length binders) (length args))
                            else pure (zip binders (take (length binders) args))
                    _ -> do
                        binders <- instantiationBinders (tnId leftRaw)
                        if null binders
                            then pure []
                        else if length binders > length args
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
    witnessAlg
        :: TyNode
        -> ExpansionF (Bool, PresolutionM (Int, [InstanceOp]))
        -> (Bool, PresolutionM (Int, [InstanceOp]))
    witnessAlg lr layer = case layer of
        ExpIdentityF ->
            (False, pure (0, []))
        ExpForallF ls ->
            let count = sum (map fsBinderCount (NE.toList ls))
            in (True, pure (count, []))
        ExpInstantiateF args ->
            (False, do
                case lr of
                    TyExp{ tnBody = b } -> do
                        (_bodyRoot, boundVars) <- instantiationBindersM gid b
                        if null boundVars
                            then pure (0, [])
                        else if length boundVars > length args
                            then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                            else do
                                let args' = take (length boundVars) args
                                    pairs = zip args' boundVars
                                (grafts, merges, weakens) <- foldM (classify boundVars) ([], [], []) pairs
                                pure (0, grafts ++ merges ++ weakens)
                    _ -> do
                        pure (0, []))
        ExpComposeF es ->
            let children = NE.toList es
                hasForall = or (map fst children)
            in (hasForall, do
                results <- mapM snd children
                let totalIntros = sum (map fst results)
                    allOps = concatMap snd results
                pure (totalIntros, allOps))

    classify
        :: [NodeId] -- binders at this instantiation site
        -> ([InstanceOp], [InstanceOp], [InstanceOp])
        -> (NodeId, NodeId) -- (arg, binder)
        -> PresolutionM ([InstanceOp], [InstanceOp], [InstanceOp])
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
                    -- Bounded by structure: keep witness translatable by not emitting
                    -- a Graft/Weaken pair that would require InstBot under a non-⊥
                    -- bound. This matches the previous effective behavior where Φ
                    -- translation treated these as no-op.
                    else pure (gAcc, mAcc, wAcc)

    binderBound :: NodeId -> PresolutionM (Maybe NodeId)
    binderBound bv = do
        n <- getCanonicalNode bv
        case n of
            TyVar{} ->
                lookupVarBound bv
            _ -> pure Nothing

    isTyVar :: NodeId -> PresolutionM Bool
    isTyVar nid = do
        n <- getCanonicalNode nid
        pure $ case n of
            TyVar{} -> True
            _ -> False

integratePhase2Ops :: [InstanceOp] -> [InstanceOp] -> [InstanceOp]
integratePhase2Ops baseOps extraOps =
    let isBarrier = \case
            OpRaise{} -> True
            _ -> False

        isGraft = \case
            OpGraft{} -> True
            _ -> False

        isWeaken = \case
            OpWeaken{} -> True
            _ -> False

        isMergeLike = \case
            OpMerge{} -> True
            OpRaiseMerge{} -> True
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
                | op <- baseOps
                , Just n <- [elimBinderByMerge op]
                ]

        (extraRaises, extraOps') =
            partition
                (\case
                    OpRaise{} -> True
                    _ -> False
                )
                extraOps

        raisesByBinder0 =
            foldl'
                (\m op -> case op of
                    OpRaise n -> IntMap.insertWith (++) (getNodeId n) [op] m
                    _ -> m
                )
                IntMap.empty
                extraRaises

        extraElimOps =
            [ op
            | op <- extraOps'
            , Just n <- [elimBinder op]
            , not (IntSet.member (getNodeId n) baseMerged)
            ]

        (beforeBarrier, afterBarrier) = break isBarrier baseOps

        grafts = [ op | op <- beforeBarrier, isGraft op ]
        weakens = [ op | op <- beforeBarrier, isWeaken op ]
        mergesBase = [ op | op <- beforeBarrier, isMergeLike op ]
        others = [ op | op <- beforeBarrier, not (isGraft op || isWeaken op || isMergeLike op) ]

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
                (\(raisesMap, acc) op -> case elimBinder op of
                    Just n | isWeaken op ->
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
