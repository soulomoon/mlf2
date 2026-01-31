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

import Control.Monad (foldM, zipWithM)
import Control.Monad.Except (throwError)
import Data.Functor.Foldable (ListF(..), ana, cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import Data.List (mapAccumL, partition, sortBy, sortOn)
import Data.Ord (Down(..))
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types (BindFlag(..), Expansion(..), ExpansionF(..), ForallSpec(..), GenNode(..), InstanceOp(..), InstanceStep(..), NodeId, NodeRef(..), TyNode(..), getNodeId, nodeRefFromKey, typeRef)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base (PresolutionM, PresolutionError(..), instantiationBindersM)
import MLF.Constraint.Presolution.Ops (getCanonicalNode, lookupVarBound)
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeEnv(..), OmegaNormalizeError(..), validateNormalizedWitness, compareNodesByOrderKeyM)
import qualified MLF.Binding.Tree as Binding
import MLF.Util.Order (compareNodesByOrderKey)
import MLF.Util.RecursionSchemes (cataM)

binderArgsFromExpansion :: TyNode -> Expansion -> PresolutionM [(NodeId, NodeId)]
binderArgsFromExpansion leftRaw expn = do
    let instantiationBinders nid = do
            (_bodyRoot, binders) <- instantiationBindersM nid
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

-- | Convert a presolution expansion recipe into interleaved witness steps.
witnessFromExpansion :: NodeId -> TyNode -> Expansion -> PresolutionM [InstanceStep]
witnessFromExpansion _root leftRaw expn = do
    let (_hasForall, stepper) = cata (witnessAlg leftRaw) expn
    stepper False
  where
    witnessAlg
        :: TyNode
        -> ExpansionF (Bool, Bool -> PresolutionM [InstanceStep])
        -> (Bool, Bool -> PresolutionM [InstanceStep])
    witnessAlg lr layer = case layer of
        ExpIdentityF ->
            (False, \_ -> pure [])
        ExpForallF ls ->
            let count = sum (map fsBinderCount (NE.toList ls))
            in (True, \_ -> pure (replicate count StepIntro))
        ExpInstantiateF args ->
            (False, \suppressWeaken -> do
                -- If the TyExp body is a forall, instantiate its binders in the same order
                -- that `applyExpansion` uses (binding-edge Q(n) via `orderedBinders`).
                case lr of
                    TyExp{ tnBody = b } -> do
                        (_bodyRoot, boundVars) <- instantiationBindersM b
                        if null boundVars
                            then pure []
                        else if length boundVars > length args
                            then throwError (ArityMismatch "witnessFromExpansion/ExpInstantiate" (length boundVars) (length args))
                            else do
                                let args' = take (length boundVars) args
                                    pairs = zip args' boundVars
                                (grafts, merges, weakens) <- foldM (classify suppressWeaken boundVars) ([], [], []) pairs
                                -- Order:
                                --   • grafts first (update ⊥ bounds)
                                --   • then merges (alias + eliminate)
                                --   • then remaining weakens (eliminate using existing bounds)
                                --
                                -- This is closer to `papers/these-finale-english.txt`
                                -- (see `papers/xmlf.txt` Fig. 10), and avoids emitting
                                -- invalid grafts under non-⊥ bounds (e.g. bounded variables).
                                pure (map StepOmega (grafts ++ merges ++ weakens))
                    _ -> do
                        -- Instantiating a non-TyExp is unexpected in current pipeline; treat as empty.
                        pure [])
        ExpComposeF es ->
            let children = NE.toList es
                childHas = map fst children
                hasForall = or childHas
            in (hasForall, \rightHasForall -> do
                let suffixFlags = drop 1 (scanr (||) rightHasForall childHas)
                steps <- zipWithM (\flag (_, childStep) -> childStep flag) suffixFlags children
                pure (concat steps))

    classify
        :: Bool
        -> [NodeId] -- binders at this instantiation site
        -> ([InstanceOp], [InstanceOp], [InstanceOp])
        -> (NodeId, NodeId) -- (arg, binder)
        -> PresolutionM ([InstanceOp], [InstanceOp], [InstanceOp])
    classify suppressWeaken binders (gAcc, mAcc, wAcc) (arg, bv) = do
        mbBound <- binderBound bv
        argGenBound <- argIsGenBound arg
        let weakenOp =
                if suppressWeaken || argGenBound
                    then []
                    else [OpWeaken bv]
        case mbBound of
            Nothing ->
                -- Unbounded binder: graft then eliminate later via weaken.
                pure (gAcc ++ [OpGraft arg bv], mAcc, wAcc ++ weakenOp)
            Just bnd -> do
                isVarBound <- isTyVar bnd
                if isVarBound && bnd `elem` binders
                    -- Bounded by an in-scope variable: alias + eliminate via Merge (Fig. 10).
                    then pure (gAcc, mAcc ++ [OpMerge bv bnd], wAcc)
                    -- Bounded by structure: graft then eliminate via Weaken.
                    else pure (gAcc ++ [OpGraft arg bv], mAcc, wAcc ++ weakenOp)

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

    argIsGenBound :: NodeId -> PresolutionM Bool
    argIsGenBound nid = do
        (c, canon) <- getConstraintAndCanonical
        let nidC = canon nid
            schemeParents =
                IntMap.fromList
                    [ (getNodeId (canon root), gnId gen)
                    | gen <- NodeAccess.allGenNodes c
                    , root <- gnSchemes gen
                    ]
        case IntMap.lookup (getNodeId nidC) schemeParents of
            Nothing -> pure False
            Just gid ->
                case Binding.lookupBindParentUnder canon c (typeRef nidC) of
                    Left _err -> pure False
                    Right (Just (GenRef gid', BindFlex)) -> pure (gid' == gid)
                    _ -> pure False

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

    flatten = cata alg
      where
        alg layer = case layer of
            ExpComposeF es -> concat (NE.toList es)
            ExpIdentityF -> [ExpIdentity]
            ExpForallF specs -> [ExpForall specs]
            ExpInstantiateF args -> [ExpInstantiate args]

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

    findLastIndex p = cata alg
      where
        alg = \case
            Nil -> Nothing
            Cons y mbTail ->
                case mbTail of
                    Just i -> Just (i + 1)
                    Nothing ->
                        if p y
                            then Just 0
                            else Nothing

    isIntro = \case
        StepIntro -> True
        _ -> False

    integrateSegments segmentSteps =
        let stepper = cata integrateAlg segmentSteps
        in stepper []
      where
        integrateAlg = \case
            Nil -> flush
            Cons step restFn ->
                case step of
                    StepOmega op -> \acc -> restFn (op : acc)
                    StepIntro -> \acc -> flush acc ++ [StepIntro] ++ restFn []

        flush opsRev =
            if null opsRev
                then []
                else map StepOmega (integratePhase2Ops (reverse opsRev) [])

-- | Drop operations that do not touch I(r).
--
-- Thesis alignment: §15.2.2 (Convention after Definition 15.2.1) splits
-- derivations into Iu ; I, keeps only I, and I is defined by touching I(r).
stripExteriorOps :: OmegaNormalizeEnv -> [InstanceOp] -> [InstanceOp]
stripExteriorOps env =
    filter keepOp
  where
    canon = canonical env

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n m -> [n, m]
            OpRaise n -> [n]
            OpRaiseMerge n m -> [n, m]

    touchesInterior op = any inInterior (opTargets op)

    keepOp op = touchesInterior op

normalizeInstanceOpsWithFallback :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsWithFallback env ops0 =
    case normalizeInstanceOpsFull env ops0 of
        Right ops' -> Right ops'
        Left (MergeDirectionInvalid _ _) -> do
            let ops1 = stripExteriorOps env ops0
            ops2 <- coalesceRaiseMergeWithEnv env ops1
            ops3 <- reorderWeakenWithEnv env ops2
            case validateNormalizedWitness env ops3 of
                Left (MergeDirectionInvalid _ _) -> Right ops3
                Left err' -> Left err'
                Right () -> Right ops3
        Left err -> Left err

normalizeInstanceStepsFull :: OmegaNormalizeEnv -> [InstanceStep] -> Either OmegaNormalizeError [InstanceStep]
normalizeInstanceStepsFull env steps =
    let stepper = cata (normalizeAlg env) steps
    in stepper []
  where
    normalizeAlg
        :: OmegaNormalizeEnv
        -> ListF InstanceStep ([InstanceOp] -> Either OmegaNormalizeError [InstanceStep])
        -> ([InstanceOp] -> Either OmegaNormalizeError [InstanceStep])
    normalizeAlg env' = \case
        Nil -> flush env'
        Cons step restFn ->
            case step of
                StepOmega op -> \acc -> restFn (op : acc)
                StepIntro -> \acc -> do
                    ops <- flush env' acc
                    rest' <- restFn []
                    pure (ops ++ [StepIntro] ++ rest')

    flush env' opsRev =
        if null opsRev
            then Right []
            else map StepOmega <$> normalizeInstanceOpsWithFallback env' (reverse opsRev)

coalesceRaiseMergeWithEnv :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
coalesceRaiseMergeWithEnv env ops =
    let stepper = cata (coalesceAlg env) ops
    in stepper Nothing
  where
    canon = canonical env

    sameBinder a b = canon a == canon b

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    flushPending = \case
        Nothing -> Right []
        Just (_n, opsRev) -> Right (reverse opsRev)

    coalesceAlg
        :: OmegaNormalizeEnv
        -> ListF InstanceOp (Maybe (NodeId, [InstanceOp]) -> Either OmegaNormalizeError [InstanceOp])
        -> (Maybe (NodeId, [InstanceOp]) -> Either OmegaNormalizeError [InstanceOp])
    coalesceAlg _ = \case
        Nil -> flushPending
        Cons op restFn ->
            case op of
                OpRaise n ->
                    \pending -> case pending of
                        Just (n', opsRev)
                            | sameBinder n n' -> restFn (Just (n', OpRaise n : opsRev))
                        _ -> do
                            prefix <- flushPending pending
                            rest <- restFn (Just (n, [OpRaise n]))
                            pure (prefix ++ rest)
                OpMerge n m ->
                    \pending ->
                        case pending of
                            Just (n', _opsRev)
                                | sameBinder n n' ->
                                    if inInterior m
                                        then emitMerge pending
                                        else do
                                            rest <- restFn Nothing
                                            pure (OpRaiseMerge n m : rest)
                            _ -> emitMerge pending
                  where
                    emitMerge pending = do
                        if inInterior n && not (inInterior m)
                            then Left (MalformedRaiseMerge [OpMerge n m])
                            else do
                                prefix <- flushPending pending
                                rest <- restFn Nothing
                                pure (prefix ++ [OpMerge n m] ++ rest)
                _ ->
                    \pending -> do
                        prefix <- flushPending pending
                        rest <- restFn Nothing
                        pure (prefix ++ [op] ++ rest)

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
            let missing =
                    [ wiBinder info
                    | info <- infos
                    , not (IntMap.member (getNodeId (wiBinder info)) (orderKeys env))
                    ]
            case missing of
                [] -> pure ()
                (nid:_) -> Left (MissingOrderKey nid)
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
            OpMerge n m -> [n, m]
            OpRaise n -> [n]
            OpRaiseMerge n m -> [n, m]

    descendantsOf nid =
        case Binding.interiorOf (constraint env) (typeRef (canon nid)) of
            Left _ -> Left (OpUnderRigid (canon nid))
            Right s ->
                let typeInterior =
                        IntSet.fromList
                            [ getNodeId t
                            | key <- IntSet.toList s
                            , TypeRef t <- [nodeRefFromKey key]
                            ]
                in Right (IntSet.delete (getNodeId (canon nid)) typeInterior)

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

    orderWeakenGroup infos0 = map wiOp (ana orderAlg ([], infos0))
      where
        compareReady a b =
            case compareNodesByOrderKey (orderKeys env) (wiBinder a) (wiBinder b) of
                Right EQ -> compare (wiIndex a) (wiIndex b)
                Right ord -> ord
                Left _ -> compare (wiIndex a) (wiIndex b)  -- fallback if missing key

        hasDescendant remaining info =
            any
                (\other ->
                    IntSet.member
                        (getNodeId (wiBinder other))
                        (wiDesc info)
                )
                remaining

        orderAlg (queue, remaining) =
            case queue of
                (q:qs) -> Cons q (qs, remaining)
                [] ->
                    case remaining of
                        [] -> Nil
                        _ ->
                            let (ready, blocked) = partition (not . hasDescendant remaining) remaining
                            in if null ready
                                then emitQueue (sortBy compareReady remaining) []
                                else emitQueue (sortBy compareReady ready) blocked

        emitQueue [] _ = Nil
        emitQueue (q:qs) remaining = Cons q (qs, remaining)

-- | Normalize Ω by enforcing `papers/these-finale-english.txt` conditions
-- (see `papers/xmlf.txt` conditions (1)–(5)) only.
normalizeInstanceOpsFull :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsFull env ops0 = do
    let ops1 = stripExteriorOps env ops0
    ops2 <- canonicalizeOps ops1
    ops3 <- coalesceRaiseMergeWithEnv env ops2
    ops4 <- checkMergeDirection ops3
    ops5 <- reorderWeakenWithEnv env ops4
    validateNormalizedWitness env ops5
    pure ops5
  where
    canon = canonical env

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n m -> [n, m]
            OpRaise n -> [n]
            OpRaiseMerge n m -> [n, m]

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
        case Binding.bindingPathToRoot (constraint env) (typeRef (canon nid)) of
            Left _ -> Left (OpUnderRigid (canon nid))
            Right path ->
                let strictAncestors = drop 1 path
                    softened n =
                        case n of
                            TypeRef nId -> IntSet.member (getNodeId (canon nId)) (weakened env)
                            GenRef _ -> False
                    rigidAncestor n =
                        case Binding.lookupBindParent (constraint env) n of
                            Just (_, BindRigid) -> not (softened n)
                            _ -> False
                in if any rigidAncestor strictAncestors
                    then Left (OpUnderRigid (canon nid))
                    else Right ()

    mergeKeyNode nid =
        case IntMap.lookup (getNodeId (canon nid)) (binderArgs env) of
            Just arg ->
                let argC = canon arg
                in if IntMap.member (getNodeId argC) (orderKeys env)
                    then argC
                    else canon nid
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
        ord <- compareNodesByOrderKeyM env (mergeKeyNode m) (mergeKeyNode n)
        case ord of
            LT -> Right ()
            _ -> Left (MergeDirectionInvalid (canon n) (canon m))
