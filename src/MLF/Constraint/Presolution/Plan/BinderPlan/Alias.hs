{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Alias
Description : Alias binder helpers
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Presolution.Plan.BinderPlan.Alias (
    boundMentionsSelfAliasFor,
    computeAliasBinders
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import MLF.Util.ElabError (ElabError)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.IntMapUtils as IntMapUtils

-- | Check if a bound mentions a self-alias.
boundMentionsSelfAliasFor
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeMap TyNode
    -> IntMap.IntMap Int
    -> IntSet.IntSet
    -> (NodeId -> IntSet.IntSet)
    -> NodeId
    -> Bool
boundMentionsSelfAliasFor canonical constraint nodes gammaAlias nestedSchemeInteriorSet reachableFromWithBounds v =
    case VarStore.lookupVarBound constraint (canonical v) of
        Just bnd -> any mentionsSelf (IntSet.toList (reachableFromWithBounds bnd))
          where
            binderKey = getNodeId (canonical v)
            mentionsSelf nidInt
                | IntSet.member keyC nestedSchemeInteriorSet = False
                | Just TyVar{} <- lookupNodeIn nodes nidC
                , Just repKey <- IntMap.lookup keyC gammaAlias = repKey == binderKey
                | otherwise = False
              where
                nidC = canonical (NodeId nidInt)
                keyC = getNodeId nidC
        Nothing -> False

computeAliasBinders
    :: (NodeId -> NodeId)
    -> (NodeId -> Int)
    -> Constraint
    -> NodeMap TyNode
    -> BindParents
    -> IntSet.IntSet
    -> NodeRef
    -> (String -> Either ElabError ())
    -> Either ElabError (IntSet.IntSet, [NodeId])
computeAliasBinders canonical canonKey constraint nodes bindParents scopeSchemeRoots scopeRootC traceM =
    let scopeHasStructuralScheme =
            case scopeRootC of
                GenRef gid ->
                    case NodeAccess.lookupGenNode constraint gid of
                        Just gen ->
                            any
                                (\root ->
                                    not (IntSet.member (canonKey root) scopeSchemeRoots)
                                )
                                (gnSchemes gen)
                        Nothing -> False
                _ -> False
        aliasBinderInfo =
            let baseBoundTargets =
                    IntSet.fromList
                        [ getNodeId bndC
                        | (vid, node) <- toListNode nodes
                        , TyVar{} <- [node]
                        , Just bnd <- [VarStore.lookupVarBound constraint vid]
                        , let bndC = canonical bnd
                        , case lookupNodeIn nodes bndC of
                            Just TyBase{} -> True
                            Just TyBottom{} -> True
                            _ -> False
                        ]
            in case scopeRootC of
                GenRef gid
                    | scopeHasStructuralScheme ->
                        let bases =
                                IntSet.fromList
                                    [ key
                                    | child <- IntMapUtils.typeChildrenOfGen bindParents gid
                                    , let key = getNodeId child
                                    , IntSet.member key baseBoundTargets
                                    , case lookupNodeIn nodes (NodeId key) of
                                        Just TyBase{} -> True
                                        Just TyBottom{} -> True
                                        _ -> False
                                    ]
                        in (bases, [ NodeId key | key <- IntSet.toList bases ])
                _ -> (IntSet.empty, [])
    in traceM
        ("generalizeAt: aliasBinderBases="
            ++ show (IntSet.toList (fst aliasBinderInfo))
            ++ " scopeHasStructuralScheme="
            ++ show scopeHasStructuralScheme
        )
        *> pure aliasBinderInfo
