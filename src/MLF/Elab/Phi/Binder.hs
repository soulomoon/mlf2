{- |
Module      : MLF.Elab.Phi.Binder
Description : Binder index/name helpers for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module extracts binder index/name helpers used by Phi translation so the
main Phi facade can stay focused on orchestration.
-}
module MLF.Elab.Phi.Binder (
    isBinderNodeM,
    lookupBinderIndexM,
    binderIndexM,
    binderNameForM
) where

import Control.Monad.Except (MonadError(..))
import Data.List (findIndex)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (catMaybes, fromMaybe)

import MLF.Constraint.Types.Graph (NodeId, getNodeId)
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (splitForalls)
import MLF.Elab.Types (ElabError(..), ElabType)
import MLF.Elab.Phi.Env (PhiM, askCanonical, askCopyMap, askGaParents, askInvCopyMap)

-- | Check if a node is a binder node (member of binderKeys set).
isBinderNodeM :: IntSet.IntSet -> NodeId -> PhiM Bool
isBinderNodeM binderKeys nid = do
    canonical <- askCanonical
    let key = getNodeId (canonical nid)
    pure $ IntSet.member key binderKeys

-- | Look up the index of a binder in the identity list.
-- Returns Nothing if the node is not a binder or not found.
lookupBinderIndexM :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> PhiM (Maybe Int)
lookupBinderIndexM binderKeys ids nid = do
    isBinder <- isBinderNodeM binderKeys nid
    if not isBinder
        then pure Nothing
        else do
            canonical <- askCanonical
            mbGaParents <- askGaParents
            copyMap <- askCopyMap
            invCopyMap <- askInvCopyMap
            let baseKeyFor n =
                    let key0 = getNodeId (canonical n)
                    in case mbGaParents of
                        Just ga -> maybe key0 getNodeId (IntMap.lookup key0 (gaSolvedToBase ga))
                        Nothing -> key0
                nC = canonical nid
                key = getNodeId nC
                candidateKeys = IntSet.fromList $
                    baseKeyFor nid
                    : catMaybes [ baseKeyFor <$> IntMap.lookup key copyMap
                                , baseKeyFor <$> IntMap.lookup key invCopyMap
                                ]
                matches (Just nid') = IntSet.member (baseKeyFor nid') candidateKeys
                matches Nothing = False
            pure $ findIndex matches ids

-- | Get the index of a binder in the identity list.
-- Returns an error if the binder is not found.
binderIndexM :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> PhiM Int
binderIndexM binderKeys ids nid = do
    mbIdx <- lookupBinderIndexM binderKeys ids nid
    case mbIdx of
        Just i -> pure i
        Nothing ->
            throwError $
                InstantiationError $
                    "binder " ++ show nid ++ " not found in identity list " ++ show ids

-- | Get the name of a binder.
binderNameForM
    :: IntSet.IntSet
    -> ElabType
    -> [Maybe NodeId]
    -> NodeId
    -> (NodeId -> Maybe String)
    -> PhiM String
binderNameForM binderKeys ty ids nid lookupBinder = do
    mbIdx <- lookupBinderIndexM binderKeys ids nid
    let (qs, _) = splitForalls ty
        names = map fst qs
    case mbIdx of
        Just i
            | length names /= length ids ->
                throwError (InstantiationError "binderNameFor: binder spine / identity list length mismatch")
            | i >= length names ->
                throwError (InstantiationError "binderNameFor: index out of range")
            | otherwise -> pure (names !! i)
        Nothing ->
            pure $ fromMaybe ("t" ++ show (getNodeId nid)) (lookupBinder nid)
