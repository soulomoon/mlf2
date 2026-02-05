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

import MLF.Constraint.Types.Graph (BindFlag(..), Constraint(..))
import MLF.Constraint.Types.Graph (NodeId, NodeRef(..), getNodeId, nodeRefFromKey, typeRef)
import MLF.Constraint.Types.Witness (InstanceOp(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Util.Order (OrderKey, compareOrderKey)

data OmegaNormalizeEnv = OmegaNormalizeEnv
    { oneRoot :: NodeId
    , interior :: IntSet.IntSet
    , weakened :: IntSet.IntSet
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
    | RigidOperationInvalid InstanceOp NodeId
    | RigidOperandMismatch InstanceOp NodeId NodeId
    | NotTransitivelyFlexBound InstanceOp NodeId NodeId
    | MalformedRaiseMerge [InstanceOp]
    deriving (Eq, Show)

compareNodesByOrderKeyM :: OmegaNormalizeEnv -> NodeId -> NodeId -> Either OmegaNormalizeError Ordering
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

validateNormalizedWitness :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError ()
validateNormalizedWitness env ops = do
    mapM_ checkOp ops
    checkWeakenOrdering ops
  where
    rootC = canonical env (oneRoot env)

    canon = canonical env

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

    checkOp op =
        case op of
            OpGraft _ n ->
                requireInterior op n
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
                    else if inInterior n
                        then Right ()
                        else Left (RaiseNotUnderRoot (canon n) rootC)
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
                    Just offender -> Left (OpUnderRigid offender)
            _ -> checkWeakenOrdering rest
