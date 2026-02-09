{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target.GammaPlan (
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort, sortBy)
import Data.Maybe (listToMaybe)
import MLF.Util.Trace (traceWhen)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..), firstSchemeRootAncestorWith)
import MLF.Constraint.BindingUtil (firstGenAncestorFrom)
import qualified MLF.Util.Order as Order

data GammaPlanInput = GammaPlanInput
    { gpiDebugEnabled :: Bool
    , gpiConstraint :: Constraint
    , gpiNodes :: IntMap.IntMap TyNode
    , gpiCanonical :: NodeId -> NodeId
    , gpiCanonKey :: NodeId -> Int
    , gpiIsTyVarKey :: Int -> Bool
    , gpiBindParents :: BindParents
    , gpiBindParentsGa :: Maybe GaBindParentsInfo
    , gpiScopeGen :: Maybe GenNodeId
    , gpiTarget0 :: NodeId
    , gpiTargetBound :: Maybe NodeId
    , gpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , gpiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , gpiSchemeRootByBody :: IntMap.IntMap NodeId
    , gpiSchemeRootKeySet :: IntSet.IntSet
    , gpiOrderRoot :: NodeId
    , gpiOrderRootBase :: NodeId
    , gpiTypeRoot0 :: NodeId
    , gpiNamedUnderGaInterior :: IntSet.IntSet
    , gpiNestedSchemeInteriorSet :: IntSet.IntSet
    , gpiReachableForBinders0 :: IntSet.IntSet
    , gpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , gpiBindableChildrenUnder :: NodeRef -> [NodeId]
    , gpiAliasBinderNodes :: [NodeId]
    , gpiFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    }

data GammaPlan = GammaPlan
    { gpBaseGammaSet :: IntSet.IntSet
    , gpBaseGammaRep :: IntMap.IntMap Int
    , gpNamedUnderGaSet :: IntSet.IntSet
    , gpSolvedToBasePref :: IntMap.IntMap NodeId
    , gpGammaAlias :: IntMap.IntMap Int
    , gpBaseGammaRepSet :: IntSet.IntSet
    , gpReachableForBinders :: IntSet.IntSet
    , gpGammaKeyFor :: Int -> Int -> Int
    , gpNamedUnderGa :: [NodeId]
    , gpBoundHasNamedOutsideGamma :: Bool
    , gpTypeRootHasNamedOutsideGamma :: Bool
    }

tracePlanEnabled :: Bool -> String -> a -> a
tracePlanEnabled = traceWhen

buildGammaPlan :: GammaPlanInput -> GammaPlan
buildGammaPlan GammaPlanInput{..} =
    let tracePlan = tracePlanEnabled gpiDebugEnabled
        constraint = gpiConstraint
        nodes = gpiNodes
        canonical = gpiCanonical
        canonKey = gpiCanonKey
        isTyVarKey = gpiIsTyVarKey
        bindParents = gpiBindParents
        mbBindParentsGa = gpiBindParentsGa
        scopeGen = gpiScopeGen
        target0 = gpiTarget0
        targetBound = gpiTargetBound
        schemeRootOwnerBase = gpiSchemeRootOwnerBase
        schemeRootOwner = gpiSchemeRootOwner
        schemeRootByBody = gpiSchemeRootByBody
        schemeRootKeySet = gpiSchemeRootKeySet
        orderRoot = gpiOrderRoot
        orderRootBase = gpiOrderRootBase
        typeRoot0 = gpiTypeRoot0
        namedUnderGaInterior = gpiNamedUnderGaInterior
        nestedSchemeInteriorSet = gpiNestedSchemeInteriorSet
        reachableForBinders0 = gpiReachableForBinders0
        reachableFromWithBounds = gpiReachableFromWithBounds
        bindableChildrenUnder' = gpiBindableChildrenUnder
        aliasBinderNodes = gpiAliasBinderNodes
        firstGenAncestorGa = gpiFirstGenAncestor
        namedUnderGaRaw =
            case scopeGen of
                Just gid ->
                    case mbBindParentsGa of
                        Nothing ->
                            IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- bindableChildrenUnder' (GenRef gid) ++ aliasBinderNodes
                                    ]
                        Just ga ->
                            let solvedKids = bindableChildrenUnder' (GenRef gid)
                                baseKids =
                                    [ canonical solvedNid
                                    | (childKey, (parent, flag)) <- IntMap.toList (gbiBindParentsBase ga)
                                    , parent == GenRef gid
                                    , flag == BindFlex
                                    , case lookupNodeIn (cNodes (gbiBaseConstraint ga)) (NodeId childKey) of
                                        Just TyVar{} -> True
                                        _ -> False
                                    , Just solvedNid <- [IntMap.lookup childKey (gbiBaseToSolved ga)]
                                    , case IntMap.lookup (getNodeId (canonical solvedNid)) nodes of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ]
                            in IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- baseKids ++ solvedKids ++ aliasBinderNodes
                                    ]
                Nothing -> []
        (baseGammaSetLocalOut, baseGammaRepLocalOut, namedUnderGaSetLocalOut, solvedToBasePrefLocalOut) =
            case (scopeGen, mbBindParentsGa) of
                (Just gid, Just ga) ->
                    let baseConstraint = gbiBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        baseGammaSetLocalRaw =
                            IntSet.fromList
                                [ childKey
                                | (childKey, (parent, flag)) <- IntMap.toList (gbiBindParentsBase ga)
                                , parent == GenRef gid
                                , flag == BindFlex
                                , case lookupNodeIn baseNodes (NodeId childKey) of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        baseSchemeRootSetLocal =
                            IntSet.fromList
                                [ rootKey
                                | rootKey <- IntMap.keys schemeRootOwnerBase
                                ]
                        firstSchemeRootAncestorBase baseKey =
                            let parentOf ref =
                                    fmap fst (IntMap.lookup (nodeRefKey ref) (gbiBindParentsBase ga))
                                keyOfRef ref = nodeRefKey ref
                                isSchemeRootKey = (`IntSet.member` baseSchemeRootSetLocal)
                            in firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey baseKey
                        keepBaseGamma baseKey =
                            case firstSchemeRootAncestorBase baseKey of
                                Nothing -> True
                                Just rootKey ->
                                    case IntMap.lookup rootKey schemeRootOwnerBase of
                                        Just ownerGid -> ownerGid == gid
                                        Nothing -> True
                        baseGammaSetLocal0 =
                            IntSet.filter keepBaseGamma baseGammaSetLocalRaw
                        baseGammaDirect =
                            IntSet.fromList
                                [ getNodeId baseN
                                | solvedN <- namedUnderGaRaw
                                , Just baseN <- [IntMap.lookup (getNodeId (canonical solvedN)) (gbiSolvedToBase ga)]
                                ]
                        baseGammaSetLocal =
                            IntSet.union baseGammaSetLocal0 baseGammaDirect
                        firstSchemeRootAncestorSolved solvedKey =
                            let parentOf ref =
                                    fmap fst (IntMap.lookup (nodeRefKey ref) bindParents)
                                keyOfRef ref =
                                    case ref of
                                        GenRef gidRef -> genNodeKey gidRef
                                        TypeRef nid -> canonKey nid
                                isSchemeRootKey = (`IntSet.member` schemeRootKeySet)
                            in firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey solvedKey
                        keepSolvedGamma solvedKey =
                            case firstSchemeRootAncestorSolved solvedKey of
                                Nothing -> True
                                Just rootKey ->
                                    case IntMap.lookup rootKey schemeRootOwner of
                                        Just ownerGid -> ownerGid == gid
                                        Nothing -> False
                        solvedGammaSetLocal =
                            case Binding.boundFlexChildren constraint (GenRef gid) of
                                Right kids ->
                                    IntSet.fromList
                                        [ getNodeId (canonical kid)
                                        | kid <- kids
                                        , let key = getNodeId (canonical kid)
                                        , case IntMap.lookup key nodes of
                                            Just TyVar{} -> keepSolvedGamma key
                                            _ -> False
                                        ]
                                Left _ -> IntSet.empty
                        baseToSolved = gbiBaseToSolved ga
                        solvedToBase = gbiSolvedToBase ga
                        keysSolved = Order.orderKeysFromConstraintWith canonical constraint orderRoot Nothing
                        keysBase = Order.orderKeysFromConstraintWith id baseConstraint orderRootBase Nothing
                        sortByKeys keys =
                            let keyMaybe k = IntMap.lookup k keys
                                cmp a b =
                                    case (keyMaybe a, keyMaybe b) of
                                        (Just ka, Just kb) ->
                                            case Order.compareOrderKey ka kb of
                                                EQ -> compare a b
                                                other -> other
                                        (Just _, Nothing) -> LT
                                        (Nothing, Just _) -> GT
                                        _ -> compare a b
                            in sortBy cmp
                        solvedGammaOrdered = sortByKeys keysSolved (IntSet.toList solvedGammaSetLocal)
                        baseGammaOrdered = sortByKeys keysBase (IntSet.toList baseGammaSetLocal)
                        qAlignSolvedToBaseLocal =
                            IntMap.fromList
                                [ (solvedKey, NodeId baseKey)
                                | (solvedKey, baseKey) <- zip solvedGammaOrdered baseGammaOrdered
                                ]
                        alignBaseToSolved =
                            case ( Binding.boundFlexChildren (gbiBaseConstraint ga) (GenRef gid)
                                 , Binding.boundFlexChildrenUnder canonical constraint (GenRef gid)
                                 ) of
                                (Right baseBinders, Right solvedBinders) ->
                                    IntMap.fromList
                                        [ (getNodeId baseB, getNodeId (canonical solvedB))
                                        | (baseB, solvedB) <- zip baseBinders solvedBinders
                                        ]
                                _ -> IntMap.empty
                        solvedToBaseAll =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId (canonical solvedNid), [NodeId baseKey])
                                | (baseKey, solvedNid) <- IntMap.toList baseToSolved
                                ]
                        solvedUnderScope solvedKey =
                            firstGenAncestorGa (typeRef (NodeId solvedKey)) == Just gid
                        preferGamma =
                            IntMap.mapMaybeWithKey
                                (\_solvedKey baseList ->
                                    listToMaybe
                                        [ baseN
                                        | baseN@(NodeId baseKey) <- baseList
                                        , IntSet.member baseKey baseGammaSetLocal
                                        ]
                                )
                                solvedToBaseAll
                        identityGamma =
                            IntMap.fromList
                                [ (baseKey, NodeId baseKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , IntMap.member baseKey nodes
                                ]
                        identityGammaScoped =
                            IntMap.fromList
                                [ (baseKey, NodeId baseKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , IntMap.member baseKey nodes
                                , solvedUnderScope baseKey
                                ]
                        solvedToBasePrefLocal =
                            IntMap.union identityGammaScoped $
                                IntMap.union preferGamma $
                                    IntMap.union solvedToBase $
                                        IntMap.union qAlignSolvedToBaseLocal identityGamma
                        solvedByBasePref =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId baseN, [solvedKey])
                                | (solvedKey, baseN) <- IntMap.toList solvedToBasePrefLocal
                                ]
                        solvedFallback =
                            IntMap.fromListWith
                                min
                                [ (getNodeId baseNid, getNodeId (canonical (NodeId solvedKey)))
                                | (solvedKey, baseNid) <- IntMap.toList solvedToBase
                                ]
                        pickSolved baseKey =
                            if IntMap.member baseKey nodes && solvedUnderScope baseKey
                                then Just baseKey
                                else
                                    case IntMap.lookup baseKey baseToSolved of
                                        Just solvedNid ->
                                            let solvedKey = getNodeId (canonical solvedNid)
                                            in if solvedUnderScope solvedKey
                                                then Just solvedKey
                                                else
                                                    case IntMap.lookup baseKey solvedByBasePref of
                                                        Just solvedKeys ->
                                                            let underScopeKeys = filter solvedUnderScope solvedKeys
                                                                pickFrom keys =
                                                                    case filter isTyVarKey keys of
                                                                        (k:_) -> Just k
                                                                        [] ->
                                                                            case keys of
                                                                                (k:_) -> Just k
                                                                                _ -> Nothing
                                                            in case pickFrom underScopeKeys of
                                                                Just k -> Just k
                                                                Nothing -> pickFrom solvedKeys
                                                        _ ->
                                                            case IntMap.lookup baseKey alignBaseToSolved of
                                                                Just solvedKey' -> Just solvedKey'
                                                                Nothing ->
                                                                    case IntMap.lookup baseKey solvedFallback of
                                                                        Just solvedKey' -> Just solvedKey'
                                                                        Nothing ->
                                                                            if IntMap.member baseKey nodes
                                                                                then Just baseKey
                                                                                else Nothing
                                        Nothing ->
                                            case IntMap.lookup baseKey solvedByBasePref of
                                                Just solvedKeys ->
                                                    let underScopeKeys = filter solvedUnderScope solvedKeys
                                                        pickFrom keys =
                                                            case filter isTyVarKey keys of
                                                                (k:_) -> Just k
                                                                [] ->
                                                                    case keys of
                                                                        (k:_) -> Just k
                                                                        _ -> Nothing
                                                    in case pickFrom underScopeKeys of
                                                        Just k -> Just k
                                                        Nothing -> pickFrom solvedKeys
                                                _ ->
                                                    case IntMap.lookup baseKey alignBaseToSolved of
                                                        Just solvedKey -> Just solvedKey
                                                        Nothing ->
                                                            case IntMap.lookup baseKey solvedFallback of
                                                                Just solvedKey -> Just solvedKey
                                                                Nothing ->
                                                                    if IntMap.member baseKey nodes
                                                                        then Just baseKey
                                                                        else Nothing
                        baseGammaRepLocal =
                            IntMap.fromList
                                [ (baseKey, solvedKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , Just solvedKey <- [pickSolved baseKey]
                                , case IntMap.lookup solvedKey nodes of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        solvedBindersUnderScope =
                            [ canonical child
                            | child <- IntMapUtils.typeChildrenOfGenWithFlag bindParents gid BindFlex
                            , case IntMap.lookup (getNodeId (canonical child)) nodes of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                        isSchemeRootAliasSolved nid =
                            case VarStore.lookupVarBound constraint (canonical nid) of
                                Just bnd ->
                                    let bndC = canonical bnd
                                    in IntSet.member (getNodeId bndC) schemeRootKeySet
                                        || IntMap.member (getNodeId bndC) schemeRootByBody
                                Nothing -> False
                        baseSchemeRootSet =
                            IntSet.fromList
                                [ getNodeId root
                                | gen <- NodeAccess.allGenNodes baseConstraint
                                , root <- gnSchemes gen
                                ]
                        isSchemeRootAliasBase baseKey =
                            case lookupNodeIn baseNodes (NodeId baseKey) of
                                Just TyVar{ tnBound = Just bnd } ->
                                    IntSet.member (getNodeId bnd) baseSchemeRootSet
                                _ -> False
                        baseSchemeAliases =
                            let keys = IntSet.toList baseGammaSetLocal
                            in filter isSchemeRootAliasBase keys
                        solvedSchemeAliases =
                            let keys = map getNodeId solvedBindersUnderScope
                            in filter (\k -> isSchemeRootAliasSolved (NodeId k)) keys
                        scopeAliasOverrides =
                            IntMap.fromList
                                [ (solvedKey, NodeId baseKey)
                                | (solvedKey, baseKey) <- zip solvedSchemeAliases baseSchemeAliases
                                ]
                        alignSolvedToBase =
                            case ( Binding.boundFlexChildren baseConstraint (GenRef gid)
                                 , Binding.boundFlexChildrenUnder canonical constraint (GenRef gid)
                                 ) of
                                (Right baseBinders, Right solvedBinders) ->
                                    IntMap.fromList
                                        [ (getNodeId (canonical solvedB), NodeId (getNodeId baseB))
                                        | (baseB, solvedB) <- zip baseBinders solvedBinders
                                        ]
                                _ -> IntMap.empty
                        solvedBinderKeys =
                            IntSet.fromList (map getNodeId solvedBindersUnderScope)
                        alignPrefer =
                            IntMap.filterWithKey
                                (\k v -> IntSet.member k solvedBinderKeys
                                    && case IntMap.lookup k solvedToBase of
                                        Just baseN
                                            | baseN /= v
                                            , IntSet.member (getNodeId baseN) baseGammaSetLocal
                                            , getNodeId baseN /= k -> False
                                        _ -> True
                                )
                                alignSolvedToBase
                        solvedToBasePrefLocal' =
                            IntMap.union alignPrefer
                                (IntMap.union scopeAliasOverrides solvedToBasePrefLocal)
                        namedUnderGaSetLocal =
                            IntSet.union
                                (IntSet.fromList
                                    [ solvedKey
                                    | solvedKey <- IntMap.elems baseGammaRepLocal
                                    ])
                                namedUnderGaInterior
                    in tracePlan
                        ("generalizeAt: baseGammaSet="
                            ++ show (IntSet.toList baseGammaSetLocal)
                            ++ " baseGammaPick="
                            ++ show
                                [ ( baseKey
                                  , IntMap.findWithDefault [] baseKey solvedByBasePref
                                  , pickSolved baseKey
                                  , IntMap.lookup baseKey nodes
                                  )
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                ]
                            ++ " solvedToBasePref[6]="
                            ++ show (IntMap.lookup 6 solvedToBasePrefLocal')
                            ++ " scopeAliasOverrides="
                            ++ show (IntMap.toList scopeAliasOverrides)
                            ++ " baseGammaRep="
                            ++ show (IntMap.toList baseGammaRepLocal)
                            ++ " namedUnderGaSet="
                            ++ show (IntSet.toList namedUnderGaSetLocal)
                        )
                        (baseGammaSetLocal, baseGammaRepLocal, namedUnderGaSetLocal, solvedToBasePrefLocal')
                (Nothing, Just ga) ->
                    ( IntSet.empty
                    , IntMap.empty
                    , IntSet.union
                        (IntSet.fromList
                            [ getNodeId nid
                            | nid <- namedUnderGaRaw
                            , not (IntSet.member (getNodeId (canonical nid)) nestedSchemeInteriorSet)
                            ])
                        namedUnderGaInterior
                    , gbiSolvedToBase ga
                    )
                _ ->
                    ( IntSet.empty
                    , IntMap.empty
                    , IntSet.union
                        (IntSet.fromList
                            [ getNodeId nid
                            | nid <- namedUnderGaRaw
                            , not (IntSet.member (getNodeId (canonical nid)) nestedSchemeInteriorSet)
                            ])
                        namedUnderGaInterior
                    , IntMap.empty
                    )
        gammaAliasLocal =
            case mbBindParentsGa of
                Just ga ->
                    let baseToSolved = gbiBaseToSolved ga
                        aliasEligible solvedKey =
                            case scopeGen of
                                Nothing -> True
                                Just gid ->
                                    let underSolved =
                                            firstGenAncestorGa (typeRef (NodeId solvedKey)) == Just gid
                                        underBasePref =
                                            case IntMap.lookup solvedKey solvedToBasePrefLocalOut of
                                                Just baseN ->
                                                    firstGenAncestorFrom (gbiBindParentsBase ga) (TypeRef baseN) == Just gid
                                                Nothing -> False
                                        underBaseGamma =
                                            case IntMap.lookup solvedKey solvedToBasePrefLocalOut of
                                                Just baseN -> IntSet.member (getNodeId baseN) baseGammaSetLocalOut
                                                Nothing -> False
                                    in underSolved || underBasePref || underBaseGamma
                        solvedToBaseAll =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId (canonical solvedNid), [baseKey])
                                | (baseKey, solvedNid) <- IntMap.toList baseToSolved
                                ]
                        pickBaseGamma baseKeys =
                            listToMaybe
                                [ baseKey
                                | baseKey <- sort baseKeys
                                , IntSet.member baseKey baseGammaSetLocalOut
                                ]
                        aliasFromBase =
                            IntMap.fromList
                                [ (solvedKey, repKey)
                                | (solvedKey, baseKeys) <- IntMap.toList solvedToBaseAll
                                , aliasEligible solvedKey
                                , Just baseKey <- [pickBaseGamma baseKeys]
                                , Just repKey <- [IntMap.lookup baseKey baseGammaRepLocalOut]
                                ]
                        aliasFromPref =
                            IntMap.fromList
                                [ (solvedKeyC, repKey)
                                | (solvedKey, node) <- IntMap.toList nodes
                                , case node of
                                    TyVar{} -> True
                                    _ -> False
                                , let solvedKeyC = getNodeId (canonical (NodeId solvedKey))
                                , aliasEligible solvedKeyC
                                , Just baseNid <- [IntMap.lookup solvedKeyC solvedToBasePrefLocalOut]
                                , let baseKey = getNodeId baseNid
                                , IntSet.member baseKey baseGammaSetLocalOut
                                , Just repKey <- [IntMap.lookup baseKey baseGammaRepLocalOut]
                                ]
                    in IntMap.union aliasFromBase aliasFromPref
                Nothing -> IntMap.empty
        baseGammaRepSetLocal =
            IntSet.fromList (IntMap.elems baseGammaRepLocalOut)
        reachableForBindersLocal =
            let aliasReachable =
                    [ repKey
                    | (aliasKey, repKey) <- IntMap.toList gammaAliasLocal
                    , IntSet.member aliasKey reachableForBinders0
                    ]
                typeRootC = canonical typeRoot0
                schemeBodyAliasReachable =
                    [ getNodeId (canonical (NodeId vidKey))
                    | (vidKey, node) <- IntMap.toList nodes
                    , TyVar{} <- [node]
                    , case VarStore.lookupVarBound constraint (NodeId vidKey) of
                        Just bnd ->
                            let bndC = canonical bnd
                            in bndC == typeRootC
                        Nothing -> False
                    ]
            in IntSet.union
                reachableForBinders0
                (IntSet.fromList (aliasReachable ++ schemeBodyAliasReachable))
        gammaKeyForLocal binderKey k =
            case IntMap.lookup k gammaAliasLocal of
                Just repKey | repKey == binderKey -> k
                Just repKey -> repKey
                Nothing -> k
        namedUnderGaLocal =
            [ NodeId nid
            | nid <- IntSet.toList namedUnderGaSetLocalOut
            ]
        boundHasNamedOutsideGammaLocal =
            case targetBound of
                Just bnd ->
                    let reachableBound = reachableFromWithBounds bnd
                        targetKey = getNodeId (canonical target0)
                        isNamedOutside nidInt =
                            let nidC = canonical (NodeId nidInt)
                                keyC = getNodeId nidC
                            in case IntMap.lookup keyC nodes of
                                Just TyVar{} ->
                                    if IntSet.member keyC nestedSchemeInteriorSet
                                        then False
                                        else case IntMap.lookup (nodeRefKey (typeRef nidC)) bindParents of
                                            Just (GenRef _, _) ->
                                                not (IntSet.member (gammaKeyForLocal targetKey keyC) namedUnderGaSetLocalOut)
                                            _ -> False
                                _ -> False
                    in any isNamedOutside (IntSet.toList reachableBound)
                Nothing -> False
        typeRootHasNamedOutsideGammaLocal = False
    in GammaPlan
        { gpBaseGammaSet = baseGammaSetLocalOut
        , gpBaseGammaRep = baseGammaRepLocalOut
        , gpNamedUnderGaSet = namedUnderGaSetLocalOut
        , gpSolvedToBasePref = solvedToBasePrefLocalOut
        , gpGammaAlias = gammaAliasLocal
        , gpBaseGammaRepSet = baseGammaRepSetLocal
        , gpReachableForBinders = reachableForBindersLocal
        , gpGammaKeyFor = gammaKeyForLocal
        , gpNamedUnderGa = namedUnderGaLocal
        , gpBoundHasNamedOutsideGamma = boundHasNamedOutsideGammaLocal
        , gpTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGammaLocal
        }
