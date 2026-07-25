{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.WitnessCanon
Description : Canonicalization and normalization for witnesses

This module provides canonicalization and normalization functions for
instance operation witnesses, enforcing the conditions from the MLF thesis.
-}
module MLF.Constraint.Presolution.WitnessCanon (
    ProvenancedNode(..),
    ProvenancedInstanceOp(..),
    forgetInstanceOpProvenance,
    normalizeInstanceOpsCoreWithProvenance,
    normalizeInstanceOpsCoreWithProvenanceBy,
    normalizeInstanceOpsCore,
    normalizeInstanceOpsFull,
    coalesceRaiseMergeWithEnv,
    reorderWeakenWithEnv,
    assertNoStandaloneGrafts,
    assertNoStandaloneGraftsWithProvenance
) where

import Data.Functor.Foldable (ListF(..), ana, cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (nubBy, partition, sortBy)

import MLF.Constraint.Types.Graph (NodeId(..), NodeRef(..), getNodeId, nodeRefFromKey, typeRef)
import MLF.Constraint.Types.Witness (InstanceOp(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Util.Order (compareNodesByOrderKey)
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeEnv(..), OmegaNormalizeError(..), validateNormalizedWitness, compareNodesByOrderKeyM)

-- | A witness operand together with the source-domain evidence that produced
-- its destination-domain node.  Normalization changes only 'pnNode'; it must
-- never discard 'pnProvenance'.
data ProvenancedNode provenance = ProvenancedNode
    { pnNode :: NodeId
    , pnProvenance :: provenance
    }
    deriving (Eq, Show)

-- | An instance operation whose operands retain producer-owned provenance.
--
-- Keeping provenance on each operand (rather than in a reverse node map) is
-- essential when distinct source nodes are copied and later canonicalized to
-- the same destination node.  Such operands remain distinct; coalescing is
-- permitted only when both destination and frozen source provenance agree.
data ProvenancedInstanceOp provenance
    = ProvenancedGraft
        (ProvenancedNode provenance)
        (ProvenancedNode provenance)
    | ProvenancedWeaken (ProvenancedNode provenance)
    | ProvenancedMerge
        (ProvenancedNode provenance)
        (ProvenancedNode provenance)
    | ProvenancedRaise (ProvenancedNode provenance)
    | ProvenancedRaiseMerge
        (ProvenancedNode provenance)
        (ProvenancedNode provenance)
    deriving (Eq, Show)

forgetInstanceOpProvenance :: ProvenancedInstanceOp provenance -> InstanceOp
forgetInstanceOpProvenance op =
    case op of
        ProvenancedGraft sigma n -> OpGraft (pnNode sigma) (pnNode n)
        ProvenancedWeaken n -> OpWeaken (pnNode n)
        ProvenancedMerge n m -> OpMerge (pnNode n) (pnNode m)
        ProvenancedRaise n -> OpRaise (pnNode n)
        ProvenancedRaiseMerge n m -> OpRaiseMerge (pnNode n) (pnNode m)

withoutProvenance :: InstanceOp -> ProvenancedInstanceOp ()
withoutProvenance op =
    case op of
        OpGraft sigma n ->
            ProvenancedGraft (ProvenancedNode sigma ()) (ProvenancedNode n ())
        OpWeaken n -> ProvenancedWeaken (ProvenancedNode n ())
        OpMerge n m ->
            ProvenancedMerge (ProvenancedNode n ()) (ProvenancedNode m ())
        OpRaise n -> ProvenancedRaise (ProvenancedNode n ())
        OpRaiseMerge n m ->
            ProvenancedRaiseMerge (ProvenancedNode n ()) (ProvenancedNode m ())

destinationInInterior :: OmegaNormalizeEnv p -> NodeId -> Bool
destinationInInterior env nid =
    IntSet.member
        (getNodeId (canonical env nid))
        (interior env)

-- | Drop operations that do not touch I(r).
--
-- Thesis alignment: §15.2.2 (Convention after Definition 15.2.1) splits
-- derivations into Iu ; I, keeps only I, and I is defined by touching I(r).
stripExteriorOpsWithProvenanceBy
    :: (ProvenancedNode provenance -> Bool)
    -> OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp provenance]
    -> [ProvenancedInstanceOp provenance]
stripExteriorOpsWithProvenanceBy operandInInterior env =
    filter keepOp
  where
    canon = canonical env

    isCertifiedWeaken nid =
        IntSet.member (getNodeId (canon nid)) (certifiedWeakens env)

    opTargets op =
        case op of
            ProvenancedGraft _ n -> [n]
            ProvenancedWeaken n -> [n]
            ProvenancedMerge n m -> [n, m]
            ProvenancedRaise n -> [n]
            ProvenancedRaiseMerge n m -> [n, m]

    touchesInterior op = any operandInInterior (opTargets op)

    keepOp op =
        touchesInterior op
            || case op of
                ProvenancedWeaken target -> isCertifiedWeaken (pnNode target)
                _ -> False

coalesceRaiseMergeWithEnv :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
coalesceRaiseMergeWithEnv env ops =
    map forgetInstanceOpProvenance
        <$> coalesceRaiseMergeWithProvenanceBy
            (\operand -> destinationInInterior env (pnNode operand))
            env
            (map withoutProvenance ops)

coalesceRaiseMergeWithProvenanceBy
    :: (Eq provenance, Semigroup provenance)
    => (ProvenancedNode provenance -> Bool)
    -> OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp provenance]
    -> Either OmegaNormalizeError [ProvenancedInstanceOp provenance]
coalesceRaiseMergeWithProvenanceBy operandInInterior env ops =
    let stepper = cata (coalesceAlg operandInInterior env) ops
    in stepper Nothing
  where
    canon = canonical env

    sameBinder a b =
        canon (pnNode a) == canon (pnNode b)
            && pnProvenance a == pnProvenance b

    flushPending = \case
        Nothing -> Right []
        Just (_n, opsRev) -> Right (reverse opsRev)

    combineNodeEvidence firstNode rest =
        firstNode
            { pnProvenance =
                foldl'
                    (<>)
                    (pnProvenance firstNode)
                    (map pnProvenance rest)
            }

    coalesceAlg
        :: (Eq provenance, Semigroup provenance)
        => (ProvenancedNode provenance -> Bool)
        -> OmegaNormalizeEnv p
        -> ListF
            (ProvenancedInstanceOp provenance)
            ( Maybe
                ( ProvenancedNode provenance
                , [ProvenancedInstanceOp provenance]
                )
                -> Either OmegaNormalizeError [ProvenancedInstanceOp provenance]
            )
        -> ( Maybe
                ( ProvenancedNode provenance
                , [ProvenancedInstanceOp provenance]
                )
                -> Either OmegaNormalizeError [ProvenancedInstanceOp provenance]
           )
    coalesceAlg isOperandInterior _ = \case
        Nil -> flushPending
        Cons op restFn ->
            case op of
                ProvenancedRaise n ->
                    \pending -> case pending of
                        Just (n', opsRev)
                            | sameBinder n n' ->
                                restFn (Just (n', ProvenancedRaise n : opsRev))
                        _ -> do
                            prefix <- flushPending pending
                            rest <- restFn (Just (n, [ProvenancedRaise n]))
                            pure (prefix ++ rest)
                ProvenancedMerge n m ->
                    \pending ->
                        case pending of
                            Just (n', opsRev)
                                | sameBinder n n' ->
                                    if isOperandInterior m
                                        then emitMerge pending
                                        else do
                                            rest <- restFn Nothing
                                            let raiseNodes =
                                                    [ raiseNode
                                                    | ProvenancedRaise raiseNode <- reverse opsRev
                                                    ]
                                                operated =
                                                    case raiseNodes of
                                                        [] -> n
                                                        firstRaise : otherRaises ->
                                                            combineNodeEvidence
                                                                firstRaise
                                                                (otherRaises ++ [n])
                                            pure (ProvenancedRaiseMerge operated m : rest)
                            _ -> emitMerge pending
                  where
                    emitMerge pending = do
                        if isOperandInterior n && not (isOperandInterior m)
                            then Left (MalformedRaiseMerge [OpMerge (pnNode n) (pnNode m)])
                            else do
                                prefix <- flushPending pending
                                rest <- restFn Nothing
                                pure (prefix ++ [ProvenancedMerge n m] ++ rest)
                _ ->
                    \pending -> do
                        prefix <- flushPending pending
                        rest <- restFn Nothing
                        pure (prefix ++ [op] ++ rest)

data WeakenInfo provenance = WeakenInfo
    { wiOp :: ProvenancedInstanceOp provenance
    , wiBinder :: NodeId
    , wiAnchor :: Int
    , wiIndex :: Int
    , wiDesc :: IntSet.IntSet
    }

reorderWeakenWithEnv :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
reorderWeakenWithEnv env ops =
    map forgetInstanceOpProvenance
        <$> reorderWeakenWithProvenance env (map withoutProvenance ops)

reorderWeakenWithProvenance
    :: Eq provenance
    => OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp provenance]
    -> Either OmegaNormalizeError [ProvenancedInstanceOp provenance]
reorderWeakenWithProvenance env ops =
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
        | (idx, ProvenancedWeaken n) <- opsIndexed
        ]

    canon = canonical env

    rootC = canon (oneRoot env)

    sameSourceNode left right =
        canon (pnNode left) == canon (pnNode right)
            && pnProvenance left == pnProvenance right

    matchingRootRaiseMergeIndexAfter idx weakenNode
        | canon (pnNode weakenNode) /= rootC = Nothing
        | otherwise =
            case
                [ raiseMergeIndex
                | (raiseMergeIndex, ProvenancedRaiseMerge operated _) <- opsIndexed
                , raiseMergeIndex > idx
                , sameSourceNode weakenNode operated
                ]
            of
                [raiseMergeIndex] -> Just raiseMergeIndex
                _ -> Nothing

    isWeaken = \case
        ProvenancedWeaken{} -> True
        _ -> False

    opTargets op =
        case op of
            ProvenancedGraft _ n -> [pnNode n]
            ProvenancedWeaken n -> [pnNode n]
            ProvenancedMerge n m -> [pnNode n, pnNode m]
            ProvenancedRaise n -> [pnNode n]
            ProvenancedRaiseMerge n m -> [pnNode n, pnNode m]

    descendantsOf nid =
        case IntMap.lookup (getNodeId (canon nid)) (precomputedDescendants env) of
            Just typeInterior -> Right typeInterior
            Nothing ->
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
        descSet <- descendantsOf (pnNode n)
        -- Figure 15.3.4's rigid terminal-root lane is a state transition:
        -- Weaken(r) makes r rigid, then RaiseMerge(r,m) translates to the
        -- identity.  Intermediate child operations can separate the two
        -- source operations before delayed-Weaken normalization, while final
        -- destination canonicalization can erase the child relation entirely.
        -- Exact operand provenance is the authority that they are the same
        -- source root: move that Weaken immediately before its unique later
        -- RaiseMerge.  Every other Weaken is still delayed by descendants.
        let anchor
                | Just raiseMergeIndex <-
                    matchingRootRaiseMergeIndexAfter idx n =
                        raiseMergeIndex - 1
                | otherwise = max idx (lastDescendantIndex descSet)
        pure
            WeakenInfo
                { wiOp = ProvenancedWeaken n
                , wiBinder = canon (pnNode n)
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

coalesceDelayedGraftWeakenWithProvenance
    :: Eq provenance
    => OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp provenance]
    -> Either OmegaNormalizeError [ProvenancedInstanceOp provenance]
coalesceDelayedGraftWeakenWithProvenance env = go
  where
    canon = canonical env

    opTargets op =
        case op of
            ProvenancedGraft _ n -> [pnNode n]
            ProvenancedWeaken n -> [pnNode n]
            ProvenancedMerge n m -> [pnNode n, pnNode m]
            ProvenancedRaise n -> [pnNode n]
            ProvenancedRaiseMerge n m -> [pnNode n, pnNode m]

    protectedSetFor binder =
        case IntMap.lookup (getNodeId (canon binder)) (precomputedDescendants env) of
            Just descendants ->
                Just (IntSet.insert (getNodeId (canon binder)) descendants)
            Nothing ->
                case Binding.interiorOf (constraint env) (typeRef (canon binder)) of
                    Left _ -> Nothing
                    Right s ->
                        let descendants =
                                IntSet.fromList
                                    [ getNodeId (canon t)
                                    | key <- IntSet.toList s
                                    , TypeRef t <- [nodeRefFromKey key]
                                    ]
                        in Just (IntSet.insert (getNodeId (canon binder)) descendants)

    touchesProtected protected op =
        any
            (\nodeId -> IntSet.member (getNodeId (canon nodeId)) protected)
            (opTargets op)

    splitDelayedWeaken binder ops =
        case protectedSetFor (pnNode binder) of
            Nothing -> Nothing
            Just protected ->
                let (prefix, suffix) =
                        break
                            (\case
                                ProvenancedWeaken n ->
                                    sameSourceBinder n binder
                                _ -> False
                            )
                            ops
                in case suffix of
                    (weaken@ProvenancedWeaken{} : rest)
                        | all (not . touchesProtected protected) prefix ->
                            Just (prefix, weaken, rest)
                    _ -> Nothing

    sameSourceBinder left right =
        canon (pnNode left) == canon (pnNode right)
            && pnProvenance left == pnProvenance right

    go [] = Right []
    go (op : rest) =
        case op of
            ProvenancedGraft _arg binder ->
                case splitDelayedWeaken binder rest of
                    Just (middle, weaken, restAfterWeaken) -> do
                        suffix <- go (middle ++ restAfterWeaken)
                        pure (op : weaken : suffix)
                    Nothing -> do
                        suffix <- go rest
                        pure (op : suffix)
            _ -> do
                suffix <- go rest
                pure (op : suffix)

assertNoStandaloneGrafts :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError ()
assertNoStandaloneGrafts env =
    assertNoStandaloneGraftsWithProvenance env . map withoutProvenance

assertNoStandaloneGraftsWithProvenance
    :: Eq provenance
    => OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp provenance]
    -> Either OmegaNormalizeError ()
assertNoStandaloneGraftsWithProvenance env = go
  where
    canon = canonical env
    rootC = canon (oneRoot env)
    go [] = Right ()
    go (ProvenancedGraft _ bv : ProvenancedWeaken bv' : rest)
        | sameSourceBinder bv bv' = go rest
    go (ProvenancedGraft _ bv : rest)
        | canon (pnNode bv) == rootC = go rest  -- root grafts don't need weakens
    go (ProvenancedGraft _ bv : _) =
        Left (StandaloneGraftRemaining (canon (pnNode bv)))
    go (_ : rest) = go rest

    sameSourceBinder left right =
        canon (pnNode left) == canon (pnNode right)
            && pnProvenance left == pnProvenance right

-- | Normalize Ω by canonicalization/coalescing/reordering passes only.
-- Validation is applied by 'normalizeInstanceOpsFull' or at call-site boundaries.
normalizeInstanceOpsCore :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsCore env ops0 =
    map forgetInstanceOpProvenance
        <$> normalizeInstanceOpsCoreWithProvenanceBy
            (\operand -> destinationInInterior env (pnNode operand))
            env
            (map withoutProvenance ops0)

-- | Normalize Ω without severing any operand from its source provenance.
-- Canonicalization updates the destination node only.  Coalescing and duplicate
-- elimination additionally require equal frozen source provenance.
normalizeInstanceOpsCoreWithProvenance
    :: OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp IntSet.IntSet]
    -> Either OmegaNormalizeError [ProvenancedInstanceOp IntSet.IntSet]
normalizeInstanceOpsCoreWithProvenance env =
    normalizeInstanceOpsCoreWithProvenanceBy sourceInInterior env
  where
    sourceInInterior operand =
        not $
            IntSet.null $
                IntSet.intersection
                    (pnProvenance operand)
                    (interiorRaw env)

normalizeInstanceOpsCoreWithProvenanceBy
    :: (Eq provenance, Semigroup provenance)
    => (ProvenancedNode provenance -> Bool)
    -> OmegaNormalizeEnv p
    -> [ProvenancedInstanceOp provenance]
    -> Either OmegaNormalizeError [ProvenancedInstanceOp provenance]
normalizeInstanceOpsCoreWithProvenanceBy operandInInterior env ops0 = do
    let ops1 = stripExteriorOpsWithProvenanceBy operandInInterior env ops0
    ops2 <- canonicalizeOps ops1
    ops2' <- rejectAmbiguousGraftWeaken ops2
    ops3 <- coalesceRaiseMergeWithProvenanceBy operandInInterior env ops2'
    let ops3' = dropRedundantOpsWithProvenance ops3
    ops4 <- checkMergeDirection ops3'
    ops5 <- reorderWeakenWithProvenance env ops4
    ops5' <- coalesceDelayedGraftWeakenWithProvenance env ops5
    let ops6 = dropRedundantOpsWithProvenance ops5'
    pure ops6
  where
    canon = canonical env

    canonicalizeOps = pure . map canonicalizeOp

    canonicalizeNode node = node {pnNode = canon (pnNode node)}

    canonicalizeOp op =
        case op of
            ProvenancedGraft sigma n ->
                ProvenancedGraft (canonicalizeNode sigma) (canonicalizeNode n)
            ProvenancedMerge n m ->
                ProvenancedMerge (canonicalizeNode n) (canonicalizeNode m)
            ProvenancedRaise n -> ProvenancedRaise (canonicalizeNode n)
            ProvenancedWeaken n -> ProvenancedWeaken (canonicalizeNode n)
            ProvenancedRaiseMerge n m ->
                ProvenancedRaiseMerge (canonicalizeNode n) (canonicalizeNode m)

    rejectAmbiguousGraftWeaken ops =
        case ambiguousBinders of
            ((binderKey, argSet) : _) ->
                Left
                    (AmbiguousGraftWeaken
                        (NodeId binderKey)
                        (map pnNode argSet)
                    )
            [] -> Right ops
      where
        ambiguousBinders =
            [ (getNodeId (pnNode binder), args)
            | ProvenancedGraft _ binder <- ops
            , any (sameSourceNode binder) weakenedBinders
            , let args =
                    nubBy sameSourceNode
                        [ arg
                        | ProvenancedGraft arg binder' <- ops
                        , sameSourceNode binder binder'
                        ]
            , length args > 1
            ]

        weakenedBinders =
            [ binder
            | ProvenancedWeaken binder <- ops
            ]

        sameSourceNode left right =
            pnNode left == pnNode right
                && pnProvenance left == pnProvenance right

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
            ProvenancedMerge n m ->
                if canon (pnNode n) == canon (pnNode m)
                    && pnProvenance n /= pnProvenance m
                    -- Final destination unification can erase the strict
                    -- source order of two different operands.  The producer's
                    -- frozen provenance keeps the Merge semantic; do not turn
                    -- it into an invalid destination-domain self-merge.
                    then Right ()
                    else if inInterior (pnNode n) && inInterior (pnNode m)
                    then checkDir (pnNode n) (pnNode m)
                    else Right ()
            ProvenancedRaiseMerge{} -> Right ()
            _ -> Right ()

    checkDir n m = do
        ord <- compareNodesByOrderKeyM env (mergeKeyNode m) (mergeKeyNode n)
        case ord of
            LT -> Right ()
            _ -> Left (MergeDirectionInvalid (canon n) (canon m))

-- | Normalize Ω and validate paper invariants (conditions (1)–(5)).
normalizeInstanceOpsFull :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsFull env ops0 = do
    ops <- normalizeInstanceOpsCore env ops0
    validateNormalizedWitness env ops
    pure ops

-- | Drop locally redundant witness operations without changing order.
-- This removes consecutive duplicate raises and self-merges only when their
-- frozen source operands also agree.
dropRedundantOpsWithProvenance
    :: (Eq provenance, Semigroup provenance)
    => [ProvenancedInstanceOp provenance]
    -> [ProvenancedInstanceOp provenance]
dropRedundantOpsWithProvenance = go
  where
    combineNodeEvidence left right =
        left {pnProvenance = pnProvenance left <> pnProvenance right}

    sameSourceNode left right =
        pnNode left == pnNode right
            && pnProvenance left == pnProvenance right

    go [] = []
    go (op : rest) =
        case op of
            ProvenancedRaise n ->
                case rest of
                    ProvenancedRaise n' : rest'
                        | sameSourceNode n n' ->
                            go (ProvenancedRaise (combineNodeEvidence n n') : rest')
                    _ -> op : go rest
            ProvenancedMerge n m
                | sameSourceNode n m -> go rest
                | otherwise -> op : go rest
            ProvenancedRaiseMerge n m
                | sameSourceNode n m -> go (ProvenancedRaise n : rest)
                | otherwise -> op : go rest
            _ -> op : go rest
