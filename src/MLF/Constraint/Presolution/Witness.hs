{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Witness
Description : Witness (Ω) helpers for presolution

This module hosts helper routines for constructing and normalizing the
instance-operation witnesses that presolution records for later Φ/Σ translation.

These helpers are extracted from `MLF.Constraint.Presolution.Core` so `Core` can
shrink into a driver/wiring layer over time.
-}
module MLF.Constraint.Presolution.Witness (
    binderArgsFromExpansion,
    forallIntroSuffixCount,
    integratePhase2Ops,
    normalizeInstanceOps,
    witnessFromExpansion,
    coalesceRaiseMerge
) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (mapAccumL, partition, sortOn)
import Data.Ord (Down(..))
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types (Expansion(..), InstanceOp(..), InstanceWitness(..), NodeId, TyNode(..), getNodeId)
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), orderedBindersM)
import MLF.Constraint.Presolution.Ops (getCanonicalNode, lookupVarBound)

-- | Extract binder→argument pairing information from an expansion recipe.
--
-- For now, this is only defined for instantiation steps at a `TyExp` root.
binderArgsForFirstNonVacuousForall :: NodeId -> PresolutionM [NodeId]
binderArgsForFirstNonVacuousForall nid0 = do
    n <- getCanonicalNode nid0
    case n of
        TyForall{ tnId = forallId, tnBody = inner } -> do
            binders <- orderedBindersM forallId
            if null binders
                then binderArgsForFirstNonVacuousForall inner
                else pure binders
        _ -> throwError (InstantiateOnNonForall (tnId n))

binderArgsFromExpansion :: TyNode -> Expansion -> PresolutionM [(NodeId, NodeId)]
binderArgsFromExpansion leftRaw expn = do
    let go = \case
            ExpIdentity -> pure []
            ExpForall _ -> pure []
            ExpCompose es -> concat <$> mapM go (NE.toList es)
            ExpInstantiate args ->
                case leftRaw of
                    TyExp{ tnBody = b } -> do
                        binders <- binderArgsForFirstNonVacuousForall b
                        if length binders /= length args
                            then throwError (ArityMismatch "binderArgsFromExpansion/ExpInstantiate" (length binders) (length args))
                            else pure (zip binders args)
                    _ -> pure []

    go expn

-- | Convert our presolution expansion recipe into a (coarse) instance-operation witness.
witnessFromExpansion :: NodeId -> TyNode -> Expansion -> PresolutionM InstanceWitness
witnessFromExpansion _root leftRaw expn = do
    ops <- go expn
    pure (InstanceWitness ops)
  where
    go :: Expansion -> PresolutionM [InstanceOp]
    go ExpIdentity = pure []
    go (ExpCompose es) = concat <$> mapM go (NE.toList es)
    go (ExpForall _ls) = do
        -- `papers/xmlf.txt` does not include quantifier-introduction (`O`) in the
        -- witness language Ω (Figure 10). We record these separately on the
        -- `EdgeWitness` and apply them directly when constructing Φ(e).
        pure []
    go (ExpInstantiate args) = do
        -- If the TyExp body is a forall, instantiate its binders in the same order
        -- that `applyExpansion` uses (binding-edge Q(n) via `orderedBinders`).
        case leftRaw of
            TyExp{ tnBody = b } -> do
                boundVars <- binderArgsForFirstNonVacuousForall b
                if length boundVars /= length args
                    then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                    else do
                        let pairs = zip args boundVars
                        (grafts, merges, weakens) <- foldM (classify boundVars) ([], [], []) pairs
                        -- Order:
                        --   • grafts first (update ⊥ bounds)
                        --   • then merges (alias + eliminate)
                        --   • then remaining weakens (eliminate using existing bounds)
                        --
                        -- This is closer to `papers/xmlf.txt` Fig. 10, and avoids emitting
                        -- invalid grafts under non-⊥ bounds (e.g. bounded variables).
                        pure (grafts ++ merges ++ weakens)
            _ -> do
                -- Instantiating a non-TyExp is unexpected in current pipeline; treat as empty.
                pure []
      where
        classify
            :: [NodeId] -- binders at this instantiation site
            -> ([InstanceOp], [InstanceOp], [InstanceOp])
            -> (NodeId, NodeId) -- (arg, binder)
            -> PresolutionM ([InstanceOp], [InstanceOp], [InstanceOp])
        classify binders (gAcc, mAcc, wAcc) (arg, bv) = do
            mbBound <- binderBound bv
            case mbBound of
                Nothing ->
                    -- Unbounded binder: graft then eliminate later via weaken.
                    pure (gAcc ++ [OpGraft arg bv], mAcc, wAcc ++ [OpWeaken bv])
                Just bnd -> do
                    isVarBound <- isTyVar bnd
                    if isVarBound && bnd `elem` binders
                        -- Bounded by an in-scope variable: alias + eliminate via Merge (Fig. 10).
                        then pure (gAcc, mAcc ++ [OpMerge bv bnd], wAcc)
                        -- Bounded by structure: eliminate via Weaken (substitute bound).
                        else pure (gAcc, mAcc, wAcc ++ [OpWeaken bv])

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

forallIntroSuffixCount :: Expansion -> PresolutionM Int
forallIntroSuffixCount expn =
    let parts = flatten expn
        (prefix, suffix) = splitTrailing isForall parts
    in if any isForall prefix
        then throwError (InternalError "forallIntroSuffixCount: ExpForall appears in non-suffix position")
        else pure (sum (map forallCount suffix))
  where
    isForall = \case
        ExpForall{} -> True
        _ -> False

    forallCount = \case
        ExpForall ls -> length (NE.toList ls)
        _ -> 0

    flatten = \case
        ExpCompose es -> concatMap flatten (NE.toList es)
        other -> [other]

    splitTrailing p xs =
        let (sufRev, preRev) = span p (reverse xs)
        in (reverse preRev, reverse sufRev)

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

        extraElims =
            IntSet.fromList
                [ getNodeId n
                | op <- extraElimOps
                , Just n <- [elimBinder op]
                ]

        removeElimOps op = case op of
            OpGraft _ bv -> not (IntSet.member (getNodeId bv) extraElims)
            OpWeaken bv -> not (IntSet.member (getNodeId bv) extraElims)
            _ -> True

        baseOps' = filter removeElimOps baseOps

        (beforeBarrier, afterBarrier) = break isBarrier baseOps'

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

-- | Lightweight normalization of a recorded witness op sequence.
--
-- This is a conservative step toward `papers/xmlf.txt`’s “normalized” Ω language:
--  * ensure a binder is not eliminated twice (Weaken after Merge/RaiseMerge), and
--  * ensure `OpWeaken n` appears after other ops mentioning `n` when possible.
normalizeInstanceOps :: [InstanceOp] -> [InstanceOp]
normalizeInstanceOps ops0 =
    let ops1 = coalesceRaiseMerge ops0
    in go ops1
  where
    isBarrier = \case
        OpRaise{} -> True
        _ -> False

    isWeaken = \case
        OpWeaken{} -> True
        _ -> False

    isMergeLike = \case
        OpMerge{} -> True
        OpRaiseMerge{} -> True
        _ -> False

    elimBinder = \case
        OpMerge n _ -> Just n
        OpRaiseMerge n _ -> Just n
        OpWeaken n -> Just n
        _ -> Nothing

    normalizePreRaise :: [InstanceOp] -> [InstanceOp]
    normalizePreRaise ops =
        let mergeElims =
                IntSet.fromList
                    [ getNodeId n
                    | op <- ops
                    , Just n <- [case op of OpMerge n _ -> Just n; OpRaiseMerge n _ -> Just n; _ -> Nothing]
                    ]

            -- If an op eliminates a binder via Merge/RaiseMerge, do not also emit Weaken/Graft on it.
            opsNoRedundant =
                [ op
                | op <- ops
                , case op of
                    OpWeaken n -> not (IntSet.member (getNodeId n) mergeElims)
                    OpGraft _ n -> not (IntSet.member (getNodeId n) mergeElims)
                    _ -> True
                ]

            opsDedupElims = dedupElims opsNoRedundant

            grafts = [ op | op <- opsDedupElims, case op of OpGraft{} -> True; _ -> False ]
            merges = [ op | op <- opsDedupElims, isMergeLike op ]
            others = [ op | op <- opsDedupElims, not (case op of OpGraft{} -> True; _ -> False) && not (isMergeLike op) && not (isWeaken op) ]
            weakens = [ op | op <- opsDedupElims, isWeaken op ]

            elimKey = \case
                OpMerge n _ -> getNodeId n
                OpRaiseMerge n _ -> getNodeId n
                _ -> -1

            mergesSorted = sortOn (Down . elimKey) merges
        in grafts ++ mergesSorted ++ others ++ weakens

    go :: [InstanceOp] -> [InstanceOp]
    go ops = case break isBarrier ops of
        (chunk, []) -> normalizePreRaise chunk
        (chunk, barrierOp : rest) ->
            normalizePreRaise chunk ++ (barrierOp : go rest)

    dedupElims :: [InstanceOp] -> [InstanceOp]
    dedupElims = goDedup IntSet.empty
      where
        goDedup _ [] = []
        goDedup eliminated (op : rest) =
            case elimBinder op of
                Nothing -> op : goDedup eliminated rest
                Just n ->
                    let k = getNodeId n
                    in if IntSet.member k eliminated
                        then goDedup eliminated rest
                        else op : goDedup (IntSet.insert k eliminated) rest

-- | Coalesce the paper-shaped pattern “Raise(n); Merge(n, m)” into a single
-- `OpRaiseMerge(n, m)` operation.
--
-- Note: ∀-introduction (`O`) is not recorded in Ω; it is stored separately on
-- `EdgeWitness` as `ewForallIntros` and applied directly when constructing Φ(e).
coalesceRaiseMerge :: [InstanceOp] -> [InstanceOp]
coalesceRaiseMerge = go
  where
    go = \case
        (OpRaise n : rest) ->
            let isSameRaise = \case
                    OpRaise n' -> n' == n
                    _ -> False
                (_moreRaises, rest1) = span isSameRaise rest
            in case rest1 of
                (OpMerge n' m : rest2) | n' == n -> OpRaiseMerge n m : go rest2
                _ -> OpRaise n : go rest
        (op : rest) -> op : go rest
        [] -> []
