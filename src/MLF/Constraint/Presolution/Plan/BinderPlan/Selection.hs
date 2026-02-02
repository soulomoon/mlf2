{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Selection
Description : Binder selection helpers
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Presolution.Plan.BinderPlan.Selection (
    boundFlexChildrenUnder,
    bindableChildrenUnder,
    bindingScopeGen,
    isQuantifiable,
    boundContainsForall,
    isScopeSchemeRoot,
    hasExplicitBoundFor,
    mkIsBindable,
    bindersForGen,
    bindersForType,
    selectBinders
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.BindingUtil (bindingScopeFor)
import MLF.Constraint.Types
import MLF.Util.ElabError (ElabError)
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.IntMapUtils as IntMapUtils

boundFlexChildrenUnder
    :: (NodeId -> NodeId)
    -> BindParents
    -> (Int -> NodeId -> Bool)
    -> NodeRef
    -> [NodeId]
boundFlexChildrenUnder canonical bindParents isBindable parentRef =
    [ canonical child
    | (childKey, child) <- IntMapUtils.flexTypeChildrenWithKeyOf bindParents parentRef
    , isBindable childKey child
    ]

bindableChildrenUnder
    :: (NodeId -> NodeId)
    -> BindParents
    -> (Int -> NodeId -> Bool)
    -> NodeRef
    -> [NodeId]
bindableChildrenUnder canonical bindParents isBindable parentRef =
    [ canonical child
    | (childKey, child) <- IntMapUtils.typeChildrenWithKeyOf bindParents parentRef
    , isBindable childKey child
    ]

bindingScopeGen :: Constraint -> NodeId -> Maybe GenNodeId
bindingScopeGen constraint child = bindingScopeFor constraint (typeRef child)

isQuantifiable :: (NodeId -> NodeId) -> Constraint -> (Int -> Bool) -> NodeId -> Bool
isQuantifiable canonical constraint isTyVarKey child =
    isTyVarKey (getNodeId child) && not (VarStore.isEliminatedVar constraint (canonical child))

boundContainsForall
    :: (NodeId -> NodeId)
    -> Constraint
    -> (NodeId -> Bool)
    -> NodeId
    -> Bool
boundContainsForall canonical constraint containsForallFrom v =
    case VarStore.lookupVarBound constraint (canonical v) of
        Just bnd -> containsForallFrom bnd
        Nothing -> False

isScopeSchemeRoot
    :: (NodeId -> Int)
    -> IntSet.IntSet
    -> NodeId
    -> Bool
isScopeSchemeRoot canonKey scopeSchemeRoots child =
    IntSet.member (canonKey child) scopeSchemeRoots

hasExplicitBoundFor
    :: (NodeId -> NodeId)
    -> NodeMap TyNode
    -> Constraint
    -> NodeId
    -> Bool
hasExplicitBoundFor canonical nodes constraint v =
    case lookupNodeIn nodes (canonical v) of
        Just TyVar{} -> VarStore.lookupVarBound constraint (canonical v) /= Nothing
        _ -> False

mkIsBindable
    :: IntMap.IntMap BindFlag
    -> (NodeId -> Bool)
    -> (Int -> NodeId -> Bool)
mkIsBindable bindFlags isQuantifiableP key child =
    case IntMap.lookup key bindFlags of
        Just BindFlex -> isQuantifiableP child
        _ -> False

bindersForGen
    :: (NodeId -> NodeId)
    -> BindParents
    -> NodeMap TyNode
    -> Constraint
    -> (Int -> NodeId -> Bool)
    -> ([(NodeId, BindFlag, Maybe TyNode, Bool)] -> String)
    -> [NodeId]
    -> (String -> Either ElabError ())
    -> GenNodeId
    -> Either ElabError [NodeId]
bindersForGen canonical bindParents nodes constraint isBindable renderAllChildren aliasBinderNodes traceM gid = do
    let allChildren = IntMapUtils.typeChildrenWithFlagOf bindParents (GenRef gid)
    traceM
        ("generalizeAt: scopeGen child nodes="
            ++ renderAllChildren
                [ (child, flag, lookupNodeIn nodes child, VarStore.isEliminatedVar constraint (canonical child))
                | (child, flag) <- allChildren
                ]
        )
    traceM
        ("generalizeAt: scopeGen children="
            ++ show allChildren
        )
    let bindableByScope =
            [ canonical child
            | (child, node) <- toListNode nodes
            , TyVar{} <- [node]
            , let childKey = getNodeId child
            , isBindable childKey child
            , bindingScopeFor constraint (typeRef child) == Just gid
            ]
        bindableUnder = bindableChildrenUnder canonical bindParents isBindable (GenRef gid)
    pure (bindableUnder ++ aliasBinderNodes ++ bindableByScope)

bindersForType
    :: (NodeId -> NodeId)
    -> BindParents
    -> (Int -> NodeId -> Bool)
    -> (NodeId -> Int)
    -> IntSet.IntSet
    -> (NodeId -> Bool)
    -> NodeMap TyNode
    -> NodeId
    -> [NodeId]
bindersForType canonical bindParents isBindable canonKey scopeSchemeRoots hasExplicitBoundP nodes scopeRootN =
    let direct = boundFlexChildrenUnder canonical bindParents isBindable (typeRef scopeRootN)
    in case lookupNodeIn nodes scopeRootN of
        Just TyForall{} -> direct
        _ ->
            if IntSet.member (canonKey scopeRootN) scopeSchemeRoots
                then direct
                else [ v | v <- direct, not (hasExplicitBoundP v) ]

selectBinders
    :: (NodeId -> NodeId)
    -> BindParents
    -> NodeMap TyNode
    -> Constraint
    -> (Int -> NodeId -> Bool)
    -> (NodeId -> Int)
    -> IntSet.IntSet
    -> (NodeId -> Bool)
    -> [NodeId]
    -> (String -> Either ElabError ())
    -> Maybe GenNodeId
    -> NodeRef
    -> NodeId
    -> Either ElabError [NodeId]
selectBinders canonical bindParents nodes constraint isBindable canonKey scopeSchemeRoots hasExplicitBoundP aliasBinderNodes traceM scopeGen scopeRootC target0 = do
    binders0 <- case scopeRootC of
        GenRef gid ->
            bindersForGen
                canonical
                bindParents
                nodes
                constraint
                isBindable
                show
                aliasBinderNodes
                traceM
                gid
        TypeRef scopeRootN ->
            pure $
                bindersForType
                    canonical
                    bindParents
                    isBindable
                    canonKey
                    scopeSchemeRoots
                    hasExplicitBoundP
                    nodes
                    scopeRootN
    traceM
        ("generalizeAt: scopeRoot=" ++ show scopeRootC
            ++ " scopeGen=" ++ show scopeGen
            ++ " target=" ++ show target0
            ++ " binders=" ++ show binders0
        )
    pure binders0
