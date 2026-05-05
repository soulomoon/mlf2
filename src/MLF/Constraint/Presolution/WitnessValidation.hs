{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.WitnessValidation
Description : Validation for normalized witnesses

This module provides validation functions for checking that normalized
instance operation witnesses satisfy the required invariants from the
MLF thesis (conditions 1-5).
-}
module MLF.Constraint.Presolution.WitnessValidation (
    OmegaNormalizeEnv(..),
    OmegaNormalizeError(..),
    validateNormalizedWitness,
    compareNodesByOrderKeyM
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types.Graph (BindFlag(..), Constraint(..), NodeId(..), NodeRef(..), TyNode(..), getNodeId, nodeRefFromKey, typeRef)
import MLF.Constraint.Types.Witness (InstanceOp(..), ReplayContract(..), isStrictReplayContract)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Util.Order (OrderKey, compareOrderKey)

data OmegaNormalizeEnv p = OmegaNormalizeEnv
    { oneRoot :: NodeId
    , interior :: IntSet.IntSet
    , interiorRaw :: IntSet.IntSet
    , weakened :: IntSet.IntSet
    , orderKeys :: IntMap.IntMap OrderKey
    , canonical :: NodeId -> NodeId
    , constraint :: Constraint p
    , binderArgs :: IntMap.IntMap NodeId
    , binderReplayMap :: IntMap.IntMap NodeId
    , replayContract :: ReplayContract
    , replayDomainBinders :: [NodeId]
    , isAnnotationEdge :: Bool
    }

data OmegaNormalizeError
    = OpOutsideInterior InstanceOp
    | MergeDirectionInvalid NodeId NodeId
    | RaiseNotUnderRoot NodeId NodeId
    | RaiseMergeInsideInterior NodeId NodeId
    | GraftOnNonBottomBound NodeId NodeId
    | OpUnderRigid NodeId
    | DelayedWeakenViolation NodeId NodeId
    | MissingOrderKey NodeId
    | RigidOperationInvalid InstanceOp NodeId
    | RigidOperandMismatch InstanceOp NodeId NodeId
    | NotTransitivelyFlexBound InstanceOp NodeId NodeId
    | MalformedRaiseMerge [InstanceOp]
    | AmbiguousGraftWeaken NodeId [NodeId]
    | DeterministicGraftWeakenSynthesisFailed NodeId [NodeId]
    | ReplayMapIncomplete [NodeId]
    | ReplayMapTargetOutsideReplayDomain NodeId NodeId
    | ReplayMapNonTyVarTarget NodeId NodeId
    | ReplayMapNonInjective NodeId NodeId NodeId
    | ReplayMapExpectedEmpty [NodeId]
    | ReplayContractNoneRequiresReplay InstanceOp
    | StandaloneGraftRemaining NodeId
    deriving (Eq, Show)

compareNodesByOrderKeyM :: OmegaNormalizeEnv p -> NodeId -> NodeId -> Either OmegaNormalizeError Ordering
compareNodesByOrderKeyM env a b =
    case (IntMap.lookup (getNodeId (canon a)) (orderKeys env), IntMap.lookup (getNodeId (canon b)) (orderKeys env)) of
        (Just ka, Just kb) ->
            case compareOrderKey ka kb of
                EQ -> Right (compare (canon a) (canon b))
                other -> Right other
        (Nothing, _) -> Left (MissingOrderKey (canon a))
        (_, Nothing) -> Left (MissingOrderKey (canon b))
  where
    canon = canonical env

validateNormalizedWitness :: OmegaNormalizeEnv p -> [InstanceOp] -> Either OmegaNormalizeError ()
validateNormalizedWitness env ops = do
    validateReplayMapContract
    mapM_ checkOp ops
    checkWeakenOrdering ops
  where
    rootC = canonical env (oneRoot env)

    canon = canonical env

    validateReplayMapContract = do
        let sourceDomain = IntSet.fromList (IntMap.keys (binderArgs env))
            replayMap = binderReplayMap env
            replayDomain = IntSet.fromList (IntMap.keys replayMap)
            missingSources = IntSet.toList (IntSet.difference sourceDomain replayDomain)
            strictContract = isStrictReplayContract (replayContract env)
        if strictContract
            then do
                if null missingSources
                    then Right ()
                    else Left (ReplayMapIncomplete (map NodeId missingSources))
                mapM_ checkReplayTargetStrict (IntMap.toList replayMap)
                case duplicateReplayTarget replayMap of
                    Nothing -> Right ()
                    Just (sourceA, sourceB, target) ->
                        Left (ReplayMapNonInjective sourceA sourceB target)
            else
                if IntMap.null replayMap
                    then Right ()
                    else Left (ReplayMapExpectedEmpty (map NodeId (IntMap.keys replayMap)))

    checkReplayTargetStrict (sourceKey, replayTargetRaw) =
        let inReplayDomain =
                IntSet.member (getNodeId replayTargetRaw) replayBinderDomain
        in if not inReplayDomain
            then Left (ReplayMapTargetOutsideReplayDomain (NodeId sourceKey) replayTargetRaw)
            else if isLiveTyVar replayTargetRaw
                then Right ()
                else Left (ReplayMapNonTyVarTarget (NodeId sourceKey) replayTargetRaw)

    replayBindersForRoot
        | not (null (replayDomainBinders env)) =
            map canon (replayDomainBinders env)
        | otherwise =
            let orderedUnder nid =
                    case Binding.orderedBinders canon (constraint env) (typeRef (canon nid)) of
                        Left _ -> []
                        Right binders -> map canon binders
                direct = orderedUnder rootC
            in case NodeAccess.lookupNode (constraint env) rootC of
                Just TyVar{ tnBound = Just bnd } ->
                    let viaBound = orderedUnder bnd
                    in if null direct then viaBound else direct
                Just TyMu{ tnBody = muBody } ->
                    let viaMu = orderedUnder muBody
                    in if null direct then viaMu else direct
                _ -> direct

    replayBinderDomain =
        IntSet.fromList
            [ getNodeId binder
            | binder <- replayBindersForRoot
            ]

    duplicateReplayTarget replayMap =
        let step (seen, dupFound) (sourceKey, replayTargetRaw)
                | Just _ <- dupFound = (seen, dupFound)
                | otherwise =
                    let replayKey = getNodeId replayTargetRaw
                    in case IntMap.lookup replayKey seen of
                        Nothing ->
                            (IntMap.insert replayKey (NodeId sourceKey) seen, Nothing)
                        Just sourceA ->
                            ( seen
                            , Just (sourceA, NodeId sourceKey, NodeId replayKey)
                            )
            (_, dup) = foldl' step (IntMap.empty, Nothing) (IntMap.toList replayMap)
        in dup

    inInterior nid =
        IntSet.member (getNodeId (canon nid)) (interior env)

    isRigid nid =
        case Binding.lookupBindParent (constraint env) (typeRef (canon nid)) of
            Just (_, BindRigid) -> True
            _ -> False

    requireInterior op nid =
        if inInterior nid
            then Right ()
            else Left (OpOutsideInterior op)

    requireTransitivelyFlexBoundToRoot op nid = go IntSet.empty (canon nid)
      where
        targetC = canon nid
        failNotFlex = Left (NotTransitivelyFlexBound op targetC rootC)

        go seen cur
            | cur == rootC = Right ()
            | IntSet.member (getNodeId cur) seen = failNotFlex
            | otherwise =
                let seen' = IntSet.insert (getNodeId cur) seen
                in case Binding.lookupBindParent (constraint env) (typeRef cur) of
                    Just (TypeRef parent, BindFlex) -> go seen' (canon parent)
                    _ -> failNotFlex

    mergeKeyNode nid =
        case IntMap.lookup (getNodeId (canon nid)) (binderArgs env) of
            Just arg ->
                let argC = canon arg
                in if IntMap.member (getNodeId argC) (orderKeys env)
                    then argC
                    else canon nid
            Nothing -> canon nid

    checkMergeDirection n m = do
        ord <- compareNodesByOrderKeyM env (mergeKeyNode m) (mergeKeyNode n)
        case ord of
            LT -> Right ()
            _ -> Left (MergeDirectionInvalid (canon n) (canon m))

    isBottomNode nid =
        case NodeAccess.lookupNode (constraint env) (canon nid) of
            Just TyBottom{} -> True
            _ -> False

    isLiveTyVar nid =
        case NodeAccess.lookupNode (constraint env) (canon nid) of
            Just TyVar{} -> True
            _ -> False

    requireGraftTarget n =
        let nC = canon n
            trackedByExpansion = IntMap.member (getNodeId nC) (binderArgs env)
        in if nC == rootC
            then Right ()
            else if trackedByExpansion
                then Right ()
                else case NodeAccess.lookupNode (constraint env) nC of
                    Just TyVar{ tnBound = Just bnd }
                        | not (isBottomNode bnd) -> Left (GraftOnNonBottomBound nC (canon bnd))
                    Just TyMu{} -> Right ()
                    _ -> Right ()

    checkOp op =
        do
            case op of
                OpGraft _ n ->
                    requireInterior op n >> requireGraftTarget n
                OpWeaken n ->
                    requireInterior op n
                OpMerge n m -> do
                    if isRigid n
                        then Right ()
                        else if isRigid m
                            then Left (RigidOperandMismatch op (canon n) (canon m))
                            else do
                                requireInterior op n
                                requireInterior op m
                                checkMergeDirection n m
                                requireTransitivelyFlexBoundToRoot op n
                                requireTransitivelyFlexBoundToRoot op m
                OpRaise n ->
                    if isRigid n
                        then Right ()
                        else if not (inInterior n)
                            then Left (RaiseNotUnderRoot (canon n) rootC)
                            else requireTransitivelyFlexBoundToRoot op n
                OpRaiseMerge n m -> do
                    if isRigid n
                        then Right ()
                    else if isRigid m
                        then Left (RigidOperandMismatch op (canon n) (canon m))
                    else if not (inInterior n)
                        then Left (OpOutsideInterior op)
                        else if inInterior m
                            then Left (RaiseMergeInsideInterior (canon n) (canon m))
                            else Right ()

    opTargets op =
        case op of
            OpGraft _ n -> [n]
            OpWeaken n -> [n]
            OpMerge n m -> [n, m]
            OpRaise n -> [n]
            OpRaiseMerge n m -> [n, m]

    -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.4, condition (5)):
    -- "below n" means
    -- strict binding-tree descendants (exclude n itself).
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
                    Just offender -> Left (DelayedWeakenViolation (canon n) offender)
            _ -> checkWeakenOrdering rest
