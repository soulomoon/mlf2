{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.WitnessCanon
Description : Canonicalization and normalization for witnesses

This module provides canonicalization and normalization functions for
instance operation witnesses, enforcing the conditions from the MLF thesis.
-}
module MLF.Constraint.Presolution.WitnessCanon (
    normalizeInstanceStepsFull,
    normalizeInstanceStepsStrict,
    normalizeInstanceOpsFull,
    coalesceRaiseMergeWithEnv,
    reorderWeakenWithEnv
) where

import Data.Functor.Foldable (ListF(..), ana, cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (partition, sortBy)

import MLF.Constraint.Types.Graph (NodeId(..), NodeRef(..), getNodeId, nodeRefFromKey, typeRef)
import MLF.Constraint.Types.Witness (InstanceOp(..), InstanceStep(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Util.Order (compareNodesByOrderKey)
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeEnv(..), OmegaNormalizeError(..), validateNormalizedWitness, compareNodesByOrderKeyM)

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

normalizeInstanceStepsWith
    :: (OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp])
    -> OmegaNormalizeEnv
    -> [InstanceStep]
    -> Either OmegaNormalizeError [InstanceStep]
normalizeInstanceStepsWith normalizeOps env steps =
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
            else map StepOmega <$> normalizeOps env' (reverse opsRev)

normalizeInstanceStepsFull :: OmegaNormalizeEnv -> [InstanceStep] -> Either OmegaNormalizeError [InstanceStep]
normalizeInstanceStepsFull = normalizeInstanceStepsWith normalizeInstanceOpsWithFallback

-- | Production strict Ω-segment normalization (no permissive merge-direction fallback).
normalizeInstanceStepsStrict :: OmegaNormalizeEnv -> [InstanceStep] -> Either OmegaNormalizeError [InstanceStep]
normalizeInstanceStepsStrict = normalizeInstanceStepsWith normalizeInstanceOpsFull

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

-- | Normalize Ω by enforcing `papers/these-finale-english.txt` conditions
-- (see `papers/xmlf.txt` conditions (1)–(5)) only.
normalizeInstanceOpsFull :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsFull env ops0 = do
    let ops1 = stripExteriorOps env ops0
    ops2 <- canonicalizeOps ops1
    ops3 <- coalesceRaiseMergeWithEnv env ops2
    let ops3' = dropRedundantOps ops3
    ops4 <- checkMergeDirection ops3'
    ops5 <- reorderWeakenWithEnv env ops4
    let ops6 = dropRedundantOps ops5
    validateNormalizedWitness env ops6
    pure ops6
  where
    canon = canonical env

    canonicalizeOps = pure . map canonicalizeOp

    canonicalizeOp op =
        case op of
            OpGraft sigma n -> OpGraft (canon sigma) (canon n)
            OpMerge n m -> OpMerge (canon n) (canon m)
            OpRaise n -> OpRaise (canon n)
            OpWeaken n -> OpWeaken (canon n)
            OpRaiseMerge n m -> OpRaiseMerge (canon n) (canon m)

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

-- | Drop locally redundant witness operations without changing order.
-- This removes consecutive duplicate raises and self-merges.
dropRedundantOps :: [InstanceOp] -> [InstanceOp]
dropRedundantOps = go Nothing
  where
    go _ [] = []
    go lastRaise (op:rest) =
        case op of
            OpRaise n ->
                case lastRaise of
                    Just n' | n == n' -> go lastRaise rest
                    _ -> op : go (Just n) rest
            OpMerge n m ->
                if n == m
                    then go Nothing rest
                    else op : go Nothing rest
            OpRaiseMerge n m ->
                if n == m
                    then go Nothing rest
                    else op : go Nothing rest
            _ -> op : go Nothing rest
