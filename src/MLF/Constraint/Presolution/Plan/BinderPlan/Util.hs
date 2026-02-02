{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Util
Description : Helper utilities for binder plan construction
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Presolution.Plan.BinderPlan.Util (
    lookupNodeInMap,
    boundRootWith,
    firstSchemeRootAncestorWith
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types

lookupNodeInMap :: IntMap.IntMap TyNode -> NodeId -> Maybe TyNode
lookupNodeInMap nodes nid = IntMap.lookup (getNodeId nid) nodes

-- | Walk through bounds to find the root of a bound chain.
boundRootWith
    :: (NodeId -> Int)
    -> (NodeId -> NodeId)
    -> (Int -> Maybe TyNode)
    -> (NodeId -> Maybe NodeId)
    -> (Int -> Maybe NodeId)
    -> Bool
    -> NodeId
    -> NodeId
boundRootWith keyOf canon lookupNodeByKey lookupBound lookupSchemeRoot followForall start =
    walk IntSet.empty start
  where
    walk visited nid0
        | IntSet.member key visited = nid
        | Just root <- lookupSchemeRoot key = canon root
        | otherwise = case lookupNodeByKey key of
            Just TyForall{ tnBody = b } | followForall -> canon b
            Just TyExp{ tnBody = b } -> walk (insertKey visited) b
            Just TyVar{ tnBound = Just bnd }
                | bndC /= nid -> walk (insertKey visited) bnd
                where bndC = canon bnd
            _ -> case lookupBound nid of
                Just bnd' | canon bnd' /= nid -> walk (insertKey visited) bnd'
                _ -> nid
      where
        nid = canon nid0
        key = keyOf nid
        insertKey = IntSet.insert key

-- | Find the first scheme root ancestor of a node.
firstSchemeRootAncestorWith
    :: (NodeRef -> Maybe NodeRef)
    -> (NodeRef -> Int)
    -> (Int -> Bool)
    -> Int
    -> Maybe Int
firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey startKey =
    case parentOf startRef of
        Just parentRef -> walk (IntSet.singleton (keyOfRef startRef)) parentRef
        Nothing -> Nothing
  where
    startRef = TypeRef (NodeId startKey)
    walk visited ref@(TypeRef _) =
        case () of
            _ | isSchemeRootKey parentKey -> Just parentKey
              | IntSet.member parentKey visited -> Nothing
              | Just parentRef' <- parentOf ref -> walk (IntSet.insert parentKey visited) parentRef'
              | otherwise -> Nothing
          where
            parentKey = keyOfRef ref
    walk _ (GenRef _) = Nothing
