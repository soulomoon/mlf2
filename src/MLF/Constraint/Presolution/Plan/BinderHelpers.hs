module MLF.Constraint.Presolution.Plan.BinderHelpers (
    isTargetSchemeBinderFor,
    boundMentionsSelfAliasFor
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore

isTargetSchemeBinderFor
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Bool
    -> NodeId
    -> Bool
isTargetSchemeBinderFor canonical constraint target0 targetIsBaseLike v =
    if targetIsBaseLike
        then False
        else
            canonical v == canonical target0
                || case VarStore.lookupVarBound constraint (canonical v) of
                    Just bnd -> canonical bnd == canonical target0
                    Nothing -> False

boundMentionsSelfAliasFor
    :: (NodeId -> NodeId)
    -> Constraint
    -> IntMap.IntMap TyNode
    -> IntMap.IntMap Int
    -> IntSet.IntSet
    -> (NodeId -> IntSet.IntSet)
    -> NodeId
    -> Bool
boundMentionsSelfAliasFor canonical constraint nodes gammaAlias nestedSchemeInteriorSet reachableFromWithBounds v =
    case VarStore.lookupVarBound constraint (canonical v) of
        Just bnd ->
            let reachableBound = reachableFromWithBounds bnd
                binderKey = getNodeId (canonical v)
                mentionsSelf nidInt =
                    let nidC = canonical (NodeId nidInt)
                        keyC = getNodeId nidC
                    in if IntSet.member keyC nestedSchemeInteriorSet
                        then False
                        else case IntMap.lookup keyC nodes of
                            Just TyVar{} ->
                                case IntMap.lookup keyC gammaAlias of
                                    Just repKey -> repKey == binderKey
                                    Nothing -> False
                            _ -> False
            in any mentionsSelf (IntSet.toList reachableBound)
        Nothing -> False
