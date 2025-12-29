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
    integratePhase2Steps,
    witnessFromExpansion,
    normalizeInstanceStepsFull,
    coalesceRaiseMergeWithEnv,
    reorderWeakenWithEnv,
    normalizeInstanceOpsFull,
    validateNormalizedWitness,
    OmegaNormalizeEnv(..),
    OmegaNormalizeError(..)
) where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (mapAccumL, partition, sortBy, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(..))
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types (Constraint, Expansion(..), ForallSpec(..), InstanceOp(..), InstanceStep(..), NodeId, TyNode(..), getNodeId)
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), orderedBindersM)
import MLF.Constraint.Presolution.Ops (getCanonicalNode, lookupVarBound)
import qualified MLF.Binding.Tree as Binding
import MLF.Util.Order (OrderKey, compareNodesByOrderKey)

data OmegaNormalizeEnv = OmegaNormalizeEnv
    { oneRoot :: NodeId
    , interior :: IntSet.IntSet
    , orderKeys :: IntMap.IntMap OrderKey
    , canonical :: NodeId -> NodeId
    , constraint :: Constraint
    , binderArgs :: IntMap.IntMap NodeId
    }

data OmegaNormalizeError
    = OpOutsideInterior InstanceOp
    | MergeDirectionInvalid NodeId NodeId
    | RaiseNotUnderRoot NodeId NodeId
    | RaiseMergeInsideInterior NodeId NodeId
    | OpUnderRigid NodeId
    | MissingOrderKey NodeId
    | MalformedRaiseMerge [InstanceOp]
    deriving (Eq, Show)

validateNormalizedWitness :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError ()
validateNormalizedWitness env ops = do
    mapM_ checkOp ops
    checkWeakenOrdering ops
  where
    rootC = canonical env (oneRoot env)

    canon = canonical env

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    requireInterior op nid =
        if inInterior nid
            then Right ()
            else Left (OpOutsideInterior op)

    mergeKeyNode nid =
        case IntMap.lookup (getNodeId (canon nid)) (binderArgs env) of
            Just arg -> arg
            Nothing -> canon nid

    checkMergeDirection n m = do
        let ord = compareNodesByOrderKey (orderKeys env) (mergeKeyNode m) (mergeKeyNode n)
        case ord of
            LT -> Right ()
            _ -> Left (MergeDirectionInvalid (canon n) (canon m))

    checkOp op =
        case op of
            OpGraft _ n ->
                requireInterior op n
            OpWeaken n ->
                requireInterior op n
            OpMerge n m -> do
                requireInterior op n
                requireInterior op m
                checkMergeDirection n m
            OpRaise n ->
                if inInterior n
                    then Right ()
                    else Left (RaiseNotUnderRoot (canon n) rootC)
            OpRaiseMerge n m -> do
                if not (inInterior n)
                    then Left (OpOutsideInterior op)
                    else if inInterior m
                        then Left (RaiseMergeInsideInterior (canon n) (canon m))
                        else Right ()

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n _ -> [n]
            OpRaise n -> [n]
            OpRaiseMerge n _ -> [n]

    -- Paper alignment (`papers/xmlf.txt` §3.4, condition (5)): “below n” means
    -- strict binding-tree descendants (exclude n itself).
    descendantsOf nid =
        case Binding.interiorOf (constraint env) (canon nid) of
            Left _ -> Left (OpUnderRigid (canon nid))
            Right s ->
                Right (IntSet.delete (getNodeId (canon nid)) s)

    firstOffender descSet rest =
        listToMaybe
            [ canon t
            | op <- rest
            , t <- opTargets op
            , IntSet.member (getNodeId (canon t)) descSet
            ]

    checkWeakenOrdering [] = Right ()
    checkWeakenOrdering (op : rest) =
        case op of
            OpWeaken n -> do
                desc <- descendantsOf n
                case firstOffender desc rest of
                    Nothing -> checkWeakenOrdering rest
                    Just offender -> Left (OpUnderRigid offender)
            _ -> checkWeakenOrdering rest

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

-- | Convert a presolution expansion recipe into interleaved witness steps.
witnessFromExpansion :: NodeId -> TyNode -> Expansion -> PresolutionM [InstanceStep]
witnessFromExpansion _root leftRaw expn = do
    let parts = flatten expn
        suffix = scanr (\e acc -> isForall e || acc) False parts
        flags = case suffix of
            [] -> []
            (_:rest) -> rest
    steps <- mapM (uncurry (go leftRaw)) (zip flags parts)
    pure (concat steps)
  where
    isForall (ExpForall _) = True
    isForall _ = False

    flatten (ExpCompose es) = concatMap flatten (NE.toList es)
    flatten other = [other]

    go :: TyNode -> Bool -> Expansion -> PresolutionM [InstanceStep]
    go _ _ ExpIdentity = pure []
    go _ _ (ExpForall ls) =
        let count = sum (map fsBinderCount (NE.toList ls))
        in pure (replicate count StepIntro)
    go lr suppressWeaken (ExpInstantiate args) = do
        -- If the TyExp body is a forall, instantiate its binders in the same order
        -- that `applyExpansion` uses (binding-edge Q(n) via `orderedBinders`).
        case lr of
            TyExp{ tnBody = b } -> do
                boundVars <- binderArgsForFirstNonVacuousForall b
                if length boundVars /= length args
                    then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                    else do
                        let pairs = zip args boundVars
                        (grafts, merges, weakens) <- foldM (classify suppressWeaken boundVars) ([], [], []) pairs
                        -- Order:
                        --   • grafts first (update ⊥ bounds)
                        --   • then merges (alias + eliminate)
                        --   • then remaining weakens (eliminate using existing bounds)
                        --
                        -- This is closer to `papers/xmlf.txt` Fig. 10, and avoids emitting
                        -- invalid grafts under non-⊥ bounds (e.g. bounded variables).
                        pure (map StepOmega (grafts ++ merges ++ weakens))
            _ -> do
                -- Instantiating a non-TyExp is unexpected in current pipeline; treat as empty.
                pure []
    go lr suppressWeaken (ExpCompose es) =
        concat <$> mapM (go lr suppressWeaken) (NE.toList es)

    classify
        :: Bool
        -> [NodeId] -- binders at this instantiation site
        -> ([InstanceOp], [InstanceOp], [InstanceOp])
        -> (NodeId, NodeId) -- (arg, binder)
        -> PresolutionM ([InstanceOp], [InstanceOp], [InstanceOp])
    classify suppressWeaken binders (gAcc, mAcc, wAcc) (arg, bv) = do
        mbBound <- binderBound bv
        let weakenOp = if suppressWeaken then [] else [OpWeaken bv]
        case mbBound of
            Nothing ->
                -- Unbounded binder: graft then eliminate later via weaken.
                pure (gAcc ++ [OpGraft arg bv], mAcc, wAcc ++ weakenOp)
            Just bnd -> do
                isVarBound <- isTyVar bnd
                if isVarBound && bnd `elem` binders
                    -- Bounded by an in-scope variable: alias + eliminate via Merge (Fig. 10).
                    then pure (gAcc, mAcc ++ [OpMerge bv bnd], wAcc)
                    -- Bounded by structure: eliminate via Weaken (substitute bound).
                    else pure (gAcc, mAcc, wAcc ++ weakenOp)

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

integratePhase2Steps :: [InstanceStep] -> [InstanceOp] -> [InstanceStep]
integratePhase2Steps steps extraOps =
    let (prefix, suffix) = splitOnLastIntro steps
        prefix' = integrateSegments prefix
        suffixOps = integratePhase2Ops (omegaOps suffix) extraOps
    in prefix' ++ map StepOmega suffixOps
  where
    omegaOps xs = [op | StepOmega op <- xs]

    splitOnLastIntro xs =
        case findLastIndex isIntro xs of
            Nothing -> ([], xs)
            Just idx -> splitAt (idx + 1) xs

    findLastIndex p = go 0 Nothing
      where
        go _ acc [] = acc
        go i acc (y:ys) =
            let acc' = if p y then Just i else acc
            in go (i + 1) acc' ys

    isIntro = \case
        StepIntro -> True
        _ -> False

    integrateSegments = go []
      where
        go acc [] = flush acc
        go acc (step : rest) = case step of
            StepOmega op -> go (op : acc) rest
            StepIntro -> flush acc ++ [StepIntro] ++ go [] rest

        flush opsRev =
            if null opsRev
                then []
                else map StepOmega (integratePhase2Ops (reverse opsRev) [])

normalizeInstanceOpsWithFallback :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsWithFallback env ops0 =
    case normalizeInstanceOpsFull env ops0 of
        Right ops' -> Right ops'
        Left (MergeDirectionInvalid _ _) -> do
            ops1 <- coalesceRaiseMergeWithEnv env ops0
            ops2 <- reorderWeakenWithEnv env ops1
            case validateNormalizedWitness env ops2 of
                Left (MergeDirectionInvalid _ _) -> Right ops2
                Left err' -> Left err'
                Right () -> Right ops2
        Left err -> Left err

normalizeInstanceStepsFull :: OmegaNormalizeEnv -> [InstanceStep] -> Either OmegaNormalizeError [InstanceStep]
normalizeInstanceStepsFull env = go []
  where
    go acc [] = flush acc
    go acc (step : rest) = case step of
        StepOmega op -> go (op : acc) rest
        StepIntro -> do
            ops <- flush acc
            rest' <- go [] rest
            pure (ops ++ [StepIntro] ++ rest')

    flush opsRev =
        if null opsRev
            then Right []
            else map StepOmega <$> normalizeInstanceOpsWithFallback env (reverse opsRev)

coalesceRaiseMergeWithEnv :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
coalesceRaiseMergeWithEnv env = go
  where
    canon = canonical env

    sameBinder a b = canon a == canon b

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    isSameRaise n = \case
        OpRaise n' -> sameBinder n n'
        _ -> False

    go = \case
        [] -> Right []
        (OpRaise n : rest) ->
            let (moreRaises, rest1) = span (isSameRaise n) rest
                raises = OpRaise n : moreRaises
            in case rest1 of
                (OpMerge n' m : rest2) | sameBinder n n' ->
                    if inInterior m
                        then do
                            rest' <- go rest1
                            pure (raises ++ rest')
                        else do
                            rest' <- go rest2
                            pure (OpRaiseMerge n m : rest')
                _ -> (OpRaise n :) <$> go rest
        (OpMerge n m : rest)
            | inInterior n && not (inInterior m) -> Left (MalformedRaiseMerge [OpMerge n m])
            | otherwise -> (OpMerge n m :) <$> go rest
        (op : rest) -> (op :) <$> go rest

data WeakenInfo = WeakenInfo
    { wiOp :: InstanceOp
    , wiBinder :: NodeId
    , wiAnchor :: Int
    , wiIndex :: Int
    , wiDesc :: IntSet.IntSet
    }

reorderWeakenWithEnv :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
reorderWeakenWithEnv env ops =
    if null weakenIndexed
        then Right ops
        else do
            infos <- mapM mkWeakenInfo weakenIndexed
            let groups =
                    IntMap.fromListWith (++)
                        [ (wiAnchor info, [info])
                        | info <- infos
                        ]
                orderedGroups = IntMap.map orderWeakenGroup groups
                nonWeakenByIndex =
                    IntMap.fromList
                        [ (idx, op)
                        | (idx, op) <- opsIndexed
                        , not (isWeaken op)
                        ]
                maxIndex = length ops - 1
                output =
                    concat
                        [ maybe [] (: []) (IntMap.lookup idx nonWeakenByIndex)
                            ++ IntMap.findWithDefault [] idx orderedGroups
                        | idx <- [0 .. maxIndex]
                        ]
            Right output
  where
    opsIndexed = zip [0 ..] ops

    weakenIndexed =
        [ (idx, n)
        | (idx, OpWeaken n) <- opsIndexed
        ]

    canon = canonical env

    isWeaken = \case
        OpWeaken{} -> True
        _ -> False

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n _ -> [n]
            OpRaise n -> [n]
            OpRaiseMerge n _ -> [n]

    descendantsOf nid =
        case Binding.interiorOf (constraint env) (canon nid) of
            Left _ -> Left (OpUnderRigid (canon nid))
            Right s ->
                Right (IntSet.delete (getNodeId (canon nid)) s)

    isDescendant descSet nid =
        IntSet.member (getNodeId (canon nid)) descSet

    lastDescendantIndex descSet =
        let hits =
                [ idx
                | (idx, op) <- opsIndexed
                , any (isDescendant descSet) (opTargets op)
                ]
        in case hits of
            [] -> -1
            _ -> maximum hits

    mkWeakenInfo (idx, n) = do
        descSet <- descendantsOf n
        let anchor = max idx (lastDescendantIndex descSet)
        pure
            WeakenInfo
                { wiOp = OpWeaken n
                , wiBinder = canon n
                , wiAnchor = anchor
                , wiIndex = idx
                , wiDesc = descSet
                }

    orderWeakenGroup infos0 = map wiOp (go infos0 [])
      where
        compareReady a b =
            case compareNodesByOrderKey (orderKeys env) (wiBinder a) (wiBinder b) of
                EQ -> compare (wiIndex a) (wiIndex b)
                ord -> ord

        hasDescendant remaining info =
            any
                (\other ->
                    IntSet.member
                        (getNodeId (wiBinder other))
                        (wiDesc info)
                )
                remaining

        go remaining acc =
            case remaining of
                [] -> acc
                _ ->
                    let (ready, blocked) = partition (not . hasDescendant remaining) remaining
                    in if null ready
                        then acc ++ sortBy compareReady remaining
                        else go blocked (acc ++ sortBy compareReady ready)

-- | Normalize Ω by enforcing `papers/xmlf.txt` conditions (1)–(5) only.
normalizeInstanceOpsFull :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsFull env ops0 = do
    ops1 <- canonicalizeOps ops0
    ops2 <- coalesceRaiseMergeWithEnv env ops1
    ops3 <- checkMergeDirection ops2
    ops4 <- reorderWeakenWithEnv env ops3
    validateNormalizedWitness env ops4
    pure ops4
  where
    canon = canonical env

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n _ -> [n]
            OpRaise n -> [n]
            OpRaiseMerge n _ -> [n]

    canonicalizeOps = mapM canonicalizeOp

    canonicalizeOp op = do
        mapM_ checkRigid (opTargets op)
        pure $ case op of
            OpGraft sigma n -> OpGraft (canon sigma) (canon n)
            OpMerge n m -> OpMerge (canon n) (canon m)
            OpRaise n -> OpRaise (canon n)
            OpWeaken n -> OpWeaken (canon n)
            OpRaiseMerge n m -> OpRaiseMerge (canon n) (canon m)

    checkRigid nid =
        case Binding.isUnderRigidBinder (constraint env) (canon nid) of
            Left _ -> Left (OpUnderRigid (canon nid))
            Right True -> Left (OpUnderRigid (canon nid))
            Right False -> Right ()

    mergeKeyNode nid =
        case IntMap.lookup (getNodeId (canon nid)) (binderArgs env) of
            Just arg -> arg
            Nothing -> canon nid

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    checkMergeDirection ops = do
        mapM_ checkOp ops
        pure ops

    checkOp op =
        case op of
            OpMerge n m ->
                if inInterior n && inInterior m
                    then checkDir n m
                    else Right ()
            OpRaiseMerge{} -> Right ()
            _ -> Right ()

    checkDir n m = do
        let ord = compareNodesByOrderKey (orderKeys env) (mergeKeyNode m) (mergeKeyNode n)
        case ord of
            LT -> Right ()
            _ -> Left (MergeDirectionInvalid (canon n) (canon m))
