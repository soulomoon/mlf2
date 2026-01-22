module MLF.Constraint.Presolution.Plan.Util (
    boundRootWith,
    firstSchemeRootAncestorWith
) where

import qualified Data.IntSet as IntSet

import MLF.Constraint.Types

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
    let walkBoundRoot visited nid0 =
            let nid = canon nid0
                key = keyOf nid
            in if IntSet.member key visited
                then nid
                else
                    case lookupSchemeRoot key of
                        Just root -> canon root
                        Nothing ->
                            case lookupNodeByKey key of
                                Just TyForall{ tnBody = b }
                                    | followForall -> canon b
                                Just TyExp{ tnBody = b } ->
                                    walkBoundRoot (IntSet.insert key visited) b
                                Just TyVar{ tnBound = Just bnd } ->
                                    let bndC = canon bnd
                                    in if bndC /= nid
                                        then walkBoundRoot (IntSet.insert key visited) bnd
                                        else nid
                                _ ->
                                    case lookupBound nid of
                                        Just bnd' | canon bnd' /= nid ->
                                            walkBoundRoot (IntSet.insert key visited) bnd'
                                        _ -> nid
    in walkBoundRoot IntSet.empty start

firstSchemeRootAncestorWith
    :: (NodeRef -> Maybe NodeRef)
    -> (NodeRef -> Int)
    -> (Int -> Bool)
    -> Int
    -> Maybe Int
firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey startKey =
    let startRef = TypeRef (NodeId startKey)
        walkParents visited ref =
            case ref of
                GenRef _ -> Nothing
                TypeRef _ ->
                    let parentKey = keyOfRef ref
                    in if IntSet.member parentKey visited
                        then Nothing
                        else if isSchemeRootKey parentKey
                            then Just parentKey
                            else
                                case parentOf ref of
                                    Just parentRef' ->
                                        walkParents (IntSet.insert parentKey visited) parentRef'
                                    Nothing -> Nothing
    in case parentOf startRef of
        Just parentRef -> walkParents (IntSet.singleton (keyOfRef startRef)) parentRef
        Nothing -> Nothing
