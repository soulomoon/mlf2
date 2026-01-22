module MLF.Constraint.Presolution.Plan.Helpers (
    boundFlexChildrenUnder,
    bindableChildrenUnder,
    isQuantifiable,
    boundContainsForall,
    isScopeSchemeRoot,
    hasExplicitBound,
    mkIsBindable,
    bindingScopeGen,
    bindersForGen,
    bindersForType,
    selectBinders,
    computeAliasBinders
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Plan.BindingUtil (bindingScopeFor)
import MLF.Util.ElabError (ElabError)

boundFlexChildrenUnder
    :: (NodeId -> NodeId)
    -> BindParents
    -> (Int -> NodeId -> Bool)
    -> NodeRef
    -> [NodeId]
boundFlexChildrenUnder canonical bindParents isBindable parentRef =
    [ canonical child
    | (childKey, (parent, flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , flag == BindFlex
    , TypeRef child <- [nodeRefFromKey childKey]
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
    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
    , parent == parentRef
    , TypeRef child <- [nodeRefFromKey childKey]
    , isBindable childKey child
    ]

bindingScopeGen :: Constraint -> NodeId -> Maybe GenNodeId
bindingScopeGen constraint child = bindingScopeFor constraint (typeRef child)

isQuantifiable :: (NodeId -> NodeId) -> Constraint -> (Int -> Bool) -> NodeId -> Bool
isQuantifiable canonical constraint isTyVarKey child =
    if isTyVarKey (getNodeId child)
        then not (VarStore.isEliminatedVar constraint (canonical child))
        else False

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

hasExplicitBound
    :: (NodeId -> NodeId)
    -> IntMap.IntMap TyNode
    -> Constraint
    -> NodeId
    -> Bool
hasExplicitBound canonical nodes constraint v =
    case IntMap.lookup (getNodeId (canonical v)) nodes of
        Just TyVar{} ->
            case VarStore.lookupVarBound constraint (canonical v) of
                Nothing -> False
                Just _ -> True
        _ -> False

mkIsBindable
    :: Bool
    -> IntMap.IntMap BindFlag
    -> (NodeId -> Bool)
    -> (NodeId -> Bool)
    -> (NodeId -> Bool)
    -> (Int -> NodeId -> Bool)
mkIsBindable allowRigidBinders bindFlags isQuantifiableP isScopeSchemeRootP boundContainsForallP =
    \key child ->
        case IntMap.lookup key bindFlags of
            Just BindFlex -> isQuantifiableP child
            Just BindRigid
                | allowRigidBinders || isScopeSchemeRootP child || boundContainsForallP child ->
                    isQuantifiableP child
            _ -> False

bindersForGen
    :: (NodeId -> NodeId)
    -> BindParents
    -> IntMap.IntMap TyNode
    -> Constraint
    -> (Int -> NodeId -> Bool)
    -> ([(NodeId, BindFlag, Maybe TyNode, Bool)] -> String)
    -> [NodeId]
    -> (String -> Either ElabError ())
    -> GenNodeId
    -> Either ElabError [NodeId]
bindersForGen canonical bindParents nodes constraint isBindable renderAllChildren aliasBinderNodes traceM gid = do
    let allChildren =
            [ (child, flag)
            | (childKey, (parent, flag)) <- IntMap.toList bindParents
            , parent == GenRef gid
            , TypeRef child <- [nodeRefFromKey childKey]
            ]
    traceM
        ("generalizeAt: scopeGen child nodes="
            ++ renderAllChildren
                [ (child, flag, IntMap.lookup (getNodeId child) nodes, VarStore.isEliminatedVar constraint (canonical child))
                | (child, flag) <- allChildren
                ]
        )
    traceM
        ("generalizeAt: scopeGen children="
            ++ show allChildren
        )
    let bindableByScope =
            [ canonical child
            | (childKey, node) <- IntMap.toList nodes
            , TyVar{} <- [node]
            , let child = NodeId childKey
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
    -> IntMap.IntMap TyNode
    -> NodeId
    -> [NodeId]
bindersForType canonical bindParents isBindable canonKey scopeSchemeRoots hasExplicitBoundP nodes scopeRootN =
    let direct = boundFlexChildrenUnder canonical bindParents isBindable (typeRef scopeRootN)
    in case IntMap.lookup (getNodeId scopeRootN) nodes of
        Just TyForall{} -> direct
        _ ->
            if IntSet.member (canonKey scopeRootN) scopeSchemeRoots
                then direct
                else [ v | v <- direct, not (hasExplicitBoundP v) ]

selectBinders
    :: (NodeId -> NodeId)
    -> BindParents
    -> IntMap.IntMap TyNode
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

computeAliasBinders
    :: (NodeId -> NodeId)
    -> (NodeId -> Int)
    -> Constraint
    -> IntMap.IntMap TyNode
    -> BindParents
    -> IntSet.IntSet
    -> NodeRef
    -> (String -> Either ElabError ())
    -> Either ElabError (IntSet.IntSet, [NodeId])
computeAliasBinders canonical canonKey constraint nodes bindParents scopeSchemeRoots scopeRootC traceM =
    let scopeHasStructuralScheme =
            case scopeRootC of
                GenRef gid ->
                    case IntMap.lookup (genNodeKey gid) (cGenNodes constraint) of
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
                        | (vidKey, node) <- IntMap.toList nodes
                        , TyVar{} <- [node]
                        , Just bnd <- [VarStore.lookupVarBound constraint (NodeId vidKey)]
                        , let bndC = canonical bnd
                        , case IntMap.lookup (getNodeId bndC) nodes of
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
                                    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
                                    , parent == GenRef gid
                                    , TypeRef child <- [nodeRefFromKey childKey]
                                    , let key = getNodeId child
                                    , IntSet.member key baseBoundTargets
                                    , case IntMap.lookup key nodes of
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
