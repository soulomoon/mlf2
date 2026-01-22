{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target (
    TargetPlanInput(..),
    TargetPlan(..),
    buildTargetPlan,
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan,
    DropPlanInput(..),
    DropPlan(..),
    buildDropPlan,
    TypeRootPlanInput(..),
    TypeRootPlan(..),
    buildTypeRootPlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort, sortBy)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..))
import MLF.Constraint.Presolution.Plan.BindingUtil (firstGenAncestorFrom)
import MLF.Constraint.Presolution.Plan.Util (boundRootWith, firstSchemeRootAncestorWith)
import qualified MLF.Util.Order as Order

data TargetPlanInput = TargetPlanInput
    { tpiConstraint :: Constraint
    , tpiNodes :: IntMap.IntMap TyNode
    , tpiCanonical :: NodeId -> NodeId
    , tpiCanonKey :: NodeId -> Int
    , tpiIsTyVarKey :: Int -> Bool
    , tpiBindFlags :: IntMap.IntMap BindFlag
    , tpiScopeGen :: Maybe GenNodeId
    , tpiScopeRootC :: NodeRef
    , tpiBindParents :: BindParents
    , tpiTarget0 :: NodeId
    , tpiSchemeRootKeySetRaw :: IntSet.IntSet
    , tpiSchemeRootKeySet :: IntSet.IntSet
    , tpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , tpiSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , tpiContainsForallForTarget :: NodeId -> Bool
    , tpiFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , tpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , tpiBindParentsGa :: Maybe GaBindParentsInfo
    }

data TargetPlan = TargetPlan
    { tpTargetBound :: Maybe NodeId
    , tpTargetBoundUnderOtherGen :: Bool
    , tpBoundUnderOtherGen :: Bool
    , tpBoundIsSchemeRoot :: Bool
    , tpBoundIsVar :: Bool
    , tpBoundIsBase :: Bool
    , tpBoundIsStructural :: Bool
    , tpBoundIsChild :: Bool
    , tpBoundIsDirectChild :: Bool
    , tpBoundMentionsTarget :: Bool
    , tpBoundHasForall :: Bool
    , tpBoundHasNestedGen :: Bool
    , tpTargetRigid :: Bool
    , tpTargetIsSchemeRoot :: Bool
    , tpTargetIsSchemeRootForScope :: Bool
    , tpTargetIsTyVar :: Bool
    }

buildTargetPlan :: TargetPlanInput -> TargetPlan
buildTargetPlan TargetPlanInput{..} =
    let constraint = tpiConstraint
        nodes = tpiNodes
        canonical = tpiCanonical
        canonKey = tpiCanonKey
        isTyVarKey = tpiIsTyVarKey
        bindFlags = tpiBindFlags
        scopeGen = tpiScopeGen
        scopeRootC = tpiScopeRootC
        bindParents = tpiBindParents
        target0 = tpiTarget0
        schemeRootKeySetRaw = tpiSchemeRootKeySetRaw
        schemeRootKeySet = tpiSchemeRootKeySet
        schemeRootOwnerBase = tpiSchemeRootOwnerBase
        schemeRootByBodyBase = tpiSchemeRootByBodyBase
        containsForallForTarget = tpiContainsForallForTarget
        firstGenAncestorGa = tpiFirstGenAncestor
        reachableFromWithBounds = tpiReachableFromWithBounds
        mbBindParentsGa = tpiBindParentsGa
        targetBoundLocalFromBase =
            case mbBindParentsGa of
                Just ga ->
                    case IntMap.lookup (getNodeId target0) (gbiSolvedToBase ga) of
                        Just baseN ->
                            case VarStore.lookupVarBound (gbiBaseConstraint ga) baseN of
                                Just bndBase ->
                                    case IntMap.lookup (getNodeId bndBase) (gbiBaseToSolved ga) of
                                        Just solvedB -> Just (canonical solvedB)
                                        Nothing ->
                                            let bndKey = getNodeId bndBase
                                            in if IntMap.member bndKey nodes
                                                then Just (canonical (NodeId bndKey))
                                                else Nothing
                                Nothing -> Nothing
                        Nothing -> Nothing
                Nothing -> Nothing
        targetBoundLocalRaw =
            case (mbBindParentsGa, targetBoundLocalFromBase) of
                (Just _, Just bnd) -> Just bnd
                _ ->
                    case IntMap.lookup (getNodeId target0) nodes of
                        Just TyVar{ tnBound = Just bnd } -> Just bnd
                        _ -> targetBoundLocalFromBase
        targetBoundLocal =
            case targetBoundLocalRaw of
                Just bnd | IntSet.member (getNodeId bnd) schemeRootKeySetRaw -> Just bnd
                Just bnd -> Just (canonical bnd)
                Nothing -> Nothing
        targetBindParentLocal = IntMap.lookup (nodeRefKey (typeRef target0)) bindParents
        targetBoundUnderOtherGenLocal =
            case (scopeGen, targetBindParentLocal) of
                (Just gidScope, Just (GenRef gidTarget, _)) -> gidTarget /= gidScope
                _ -> False
        boundUnderOtherGenLocal =
            case (scopeGen, targetBoundLocal) of
                (Just gidScope, Just bnd) ->
                    case IntMap.lookup (nodeRefKey (typeRef bnd)) bindParents of
                        Just (GenRef gidBound, _) -> gidBound /= gidScope
                        _ -> False
                _ -> False
        boundIsSchemeRootLocal =
            case (scopeRootC, targetBoundLocal) of
                (GenRef gid, Just bnd) ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Nothing -> False
                        Just gen ->
                            IntSet.member
                                (getNodeId (canonical bnd))
                                (IntSet.fromList (map (getNodeId . canonical) (gnSchemes gen)))
                _ -> False
        boundIsVarLocal =
            case targetBoundLocal >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyVar{} -> True
                _ -> False
        boundIsBaseLocal =
            case targetBoundLocal >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyBase{} -> True
                _ -> False
        boundIsStructuralLocal =
            case targetBoundLocal >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                Just TyArrow{} -> True
                Just TyForall{} -> True
                Just TyExp{} -> True
                _ -> False
        boundIsChildLocal =
            case targetBoundLocal of
                Just bnd ->
                    let bndC = canonical bnd
                        quantifiable =
                            case IntMap.lookup (getNodeId bndC) nodes of
                                Just TyVar{} -> True
                                _ -> False
                    in quantifiable
                        && case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                            Just (parentRef, _) -> parentRef == typeRef target0
                            Nothing -> False
                Nothing -> False
        boundIsDirectChildLocal =
            case targetBoundLocal of
                Just bnd ->
                    let bndC = canonical bnd
                    in case IntMap.lookup (nodeRefKey (typeRef bndC)) bindParents of
                        Just (parentRef, _) -> parentRef == typeRef target0
                        Nothing -> False
                Nothing -> False
        boundMentionsTargetLocal =
            case targetBoundLocal of
                Nothing -> False
                Just bnd ->
                    let targetC = canonical target0
                        walkMentions visited nid0 =
                            let nid = canonical nid0
                                key = getNodeId nid
                            in if nid == targetC
                                then True
                                else if IntSet.member key visited
                                    then False
                                    else
                                        case IntMap.lookup key nodes of
                                            Nothing -> False
                                            Just node ->
                                                let visited' = IntSet.insert key visited
                                                    kids = structuralChildrenWithBounds node
                                                in any (walkMentions visited') kids
                    in walkMentions IntSet.empty bnd
        boundHasForallLocal =
            let boundHasForallLocalBase =
                    case (mbBindParentsGa, targetBoundLocal) of
                        (Just ga, Just bnd) ->
                            case IntMap.lookup (getNodeId (canonical bnd)) (gbiSolvedToBase ga) of
                                Just baseBnd ->
                                    IntMap.member (getNodeId baseBnd) schemeRootOwnerBase
                                        || IntMap.member (getNodeId baseBnd) schemeRootByBodyBase
                                Nothing -> False
                        _ -> False
            in case targetBoundLocal of
                Nothing -> False
                Just bnd -> containsForallForTarget bnd || boundHasForallLocalBase
        boundHasNestedGenLocal =
            case (targetBoundLocal, scopeGen) of
                (Just bnd, Just gid) ->
                    let reachableBound = reachableFromWithBounds bnd
                        isNested nidInt =
                            case IntMap.lookup nidInt nodes of
                                Just TyVar{} ->
                                    case firstGenAncestorGa (typeRef (NodeId nidInt)) of
                                        Just gid' -> gid' /= gid
                                        Nothing -> False
                                _ -> False
                    in any isNested (IntSet.toList reachableBound)
                _ -> False
        targetRigidLocal =
            case IntMap.lookup (nodeRefKey (typeRef target0)) bindFlags of
                Just BindRigid -> True
                _ -> False
        targetIsSchemeRootLocal =
            IntSet.member (canonKey target0) schemeRootKeySet
        targetIsSchemeRootForScopeLocal =
            case (scopeGen, mbBindParentsGa) of
                (Just gid, Just ga) ->
                    case IntMap.lookup (canonKey target0) (gbiSolvedToBase ga) of
                        Just baseN ->
                            case IntMap.lookup (getNodeId baseN) schemeRootOwnerBase of
                                Just ownerGid -> ownerGid == gid
                                Nothing -> False
                        Nothing -> False
                (Just gid, Nothing) ->
                    case IntMap.lookup (getGenNodeId gid) (cGenNodes constraint) of
                        Just gen -> any (\root -> canonical root == canonical target0) (gnSchemes gen)
                        Nothing -> False
                _ -> False
        targetIsTyVarLocal = isTyVarKey (canonKey target0)
    in TargetPlan
        { tpTargetBound = targetBoundLocal
        , tpTargetBoundUnderOtherGen = targetBoundUnderOtherGenLocal
        , tpBoundUnderOtherGen = boundUnderOtherGenLocal
        , tpBoundIsSchemeRoot = boundIsSchemeRootLocal
        , tpBoundIsVar = boundIsVarLocal
        , tpBoundIsBase = boundIsBaseLocal
        , tpBoundIsStructural = boundIsStructuralLocal
        , tpBoundIsChild = boundIsChildLocal
        , tpBoundIsDirectChild = boundIsDirectChildLocal
        , tpBoundMentionsTarget = boundMentionsTargetLocal
        , tpBoundHasForall = boundHasForallLocal
        , tpBoundHasNestedGen = boundHasNestedGenLocal
        , tpTargetRigid = targetRigidLocal
        , tpTargetIsSchemeRoot = targetIsSchemeRootLocal
        , tpTargetIsSchemeRootForScope = targetIsSchemeRootForScopeLocal
        , tpTargetIsTyVar = targetIsTyVarLocal
        }

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
    , gpiAllowRigidBinders :: Bool
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
tracePlanEnabled enabled msg value =
    if enabled
        then trace msg value
        else value

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
        allowRigidBinders = gpiAllowRigidBinders
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
                                    , flag == BindFlex || (allowRigidBinders && flag == BindRigid)
                                    , case IntMap.lookup childKey (cNodes (gbiBaseConstraint ga)) of
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
                                , flag == BindFlex || (allowRigidBinders && flag == BindRigid)
                                , case IntMap.lookup childKey baseNodes of
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
                            IntMap.union qAlignSolvedToBaseLocal $
                                IntMap.union identityGammaScoped
                                    (IntMap.union preferGamma (IntMap.union solvedToBase identityGamma))
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
                            | (childKey, (parentRef, flag)) <- IntMap.toList bindParents
                            , parentRef == GenRef gid
                            , flag == BindFlex
                            , TypeRef child <- [nodeRefFromKey childKey]
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
                                | gen <- IntMap.elems (cGenNodes baseConstraint)
                                , root <- gnSchemes gen
                                ]
                        isSchemeRootAliasBase baseKey =
                            case IntMap.lookup baseKey baseNodes of
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
                            IntMap.filterWithKey (\k _ -> IntSet.member k solvedBinderKeys) alignSolvedToBase
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


data DropPlanInput = DropPlanInput
    { dpiAllowDropTarget :: Bool
    , dpiTargetIsSchemeRoot :: Bool
    , dpiNodes :: IntMap.IntMap TyNode
    , dpiTarget0 :: NodeId
    , dpiTargetBound :: Maybe NodeId
    , dpiTargetRigid :: Bool
    , dpiBoundIsBase :: Bool
    , dpiBoundIsStructural :: Bool
    , dpiBoundIsVar :: Bool
    , dpiBoundIsChild :: Bool
    , dpiBoundHasNestedGen :: Bool
    , dpiBoundHasNamedOutsideGamma :: Bool
    , dpiBoundMentionsTarget :: Bool
    , dpiBoundHasForall :: Bool
    , dpiBoundIsSchemeRootAll :: NodeId -> Bool
    , dpiHasExplicitBound :: NodeId -> Bool
    , dpiScopeRootC :: NodeRef
    , dpiCanonKey :: NodeId -> Int
    }

data DropPlan = DropPlan
    { dpDropTarget :: Bool
    , dpSchemeRoots :: IntSet.IntSet
    }

buildDropPlan :: DropPlanInput -> DropPlan
buildDropPlan DropPlanInput{..} =
    let allowDropTarget = dpiAllowDropTarget
        targetIsSchemeRoot = dpiTargetIsSchemeRoot
        nodes = dpiNodes
        target0 = dpiTarget0
        targetBound = dpiTargetBound
        targetRigid = dpiTargetRigid
        boundIsBase = dpiBoundIsBase
        boundIsStructural = dpiBoundIsStructural
        boundIsVar = dpiBoundIsVar
        boundIsChild = dpiBoundIsChild
        boundHasNestedGen = dpiBoundHasNestedGen
        boundHasNamedOutsideGamma = dpiBoundHasNamedOutsideGamma
        boundMentionsTarget = dpiBoundMentionsTarget
        boundHasForall = dpiBoundHasForall
        boundIsSchemeRootAll = dpiBoundIsSchemeRootAll
        hasExplicitBound' = dpiHasExplicitBound
        scopeRootC = dpiScopeRootC
        canonKey = dpiCanonKey
        dropTargetLocal =
            not targetIsSchemeRoot &&
            case IntMap.lookup (getNodeId target0) nodes of
                Just TyVar{} ->
                    case targetBound >>= (\bnd -> IntMap.lookup (getNodeId bnd) nodes) of
                        Just TyVar{} ->
                            allowDropTarget &&
                                targetRigid &&
                                (boundIsBase || boundIsStructural) &&
                                (boundIsVar || boundIsChild) &&
                                not boundHasNestedGen &&
                                not (boundIsSchemeRootAll target0) &&
                                not boundHasNamedOutsideGamma &&
                                case targetBound of
                                    Just bndVar -> not (hasExplicitBound' bndVar)
                                    Nothing -> False
                        Just _ ->
                            allowDropTarget &&
                                boundIsBase &&
                                not boundMentionsTarget &&
                                not boundHasForall &&
                                not boundHasNestedGen &&
                                not boundHasNamedOutsideGamma
                        Nothing -> False
                Just _ -> False
                Nothing -> False
        schemeRootsLocal =
            case scopeRootC of
                GenRef _ | dropTargetLocal -> IntSet.singleton (canonKey target0)
                _ -> IntSet.empty
    in DropPlan
        { dpDropTarget = dropTargetLocal
        , dpSchemeRoots = schemeRootsLocal
        }


data TypeRootPlanInput = TypeRootPlanInput
    { trpiConstraint :: Constraint
    , trpiNodes :: IntMap.IntMap TyNode
    , trpiCanonical :: NodeId -> NodeId
    , trpiCanonKey :: NodeId -> Int
    , trpiIsTyVarKey :: Int -> Bool
    , trpiIsBaseLikeKey :: Int -> Bool
    , trpiBindParents :: BindParents
    , trpiScopeRootC :: NodeRef
    , trpiScopeGen :: Maybe GenNodeId
    , trpiTarget0 :: NodeId
    , trpiTargetBound :: Maybe NodeId
    , trpiTargetIsSchemeRoot :: Bool
    , trpiTargetIsSchemeRootForScope :: Bool
    , trpiTargetIsTyVar :: Bool
    , trpiTargetBoundUnderOtherGen :: Bool
    , trpiBoundUnderOtherGen :: Bool
    , trpiBoundIsDirectChild :: Bool
    , trpiNamedUnderGaSet :: IntSet.IntSet
    , trpiTypeRoot0 :: NodeId
    , trpiTypeRootFromBoundVar :: Maybe NodeId
    , trpiTypeRootHasNamedOutsideGamma :: Bool
    , trpiBoundHasForallForVar :: NodeId -> Bool
    , trpiAllowDropTarget :: Bool
    , trpiDropTarget :: Bool
    , trpiSchemeRootKeySet :: IntSet.IntSet
    , trpiSchemeRootByBody :: IntMap.IntMap NodeId
    , trpiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , trpiLiftToForall :: NodeId -> NodeId
    }

data TypeRootPlan = TypeRootPlan
    { trUseBoundTypeRoot :: Bool
    , trSchemeBodyRoot :: NodeId
    , trTargetInGamma :: Bool
    , trTargetIsBaseLike :: Bool
    , trSchemeBodyChildUnderGen :: Maybe NodeId
    , trTypeRoot0 :: NodeId
    , trTypeRoot :: NodeId
    }

buildTypeRootPlan :: TypeRootPlanInput -> TypeRootPlan
buildTypeRootPlan TypeRootPlanInput{..} =
    let constraint = trpiConstraint
        nodes = trpiNodes
        canonical = trpiCanonical
        canonKey = trpiCanonKey
        isTyVarKey = trpiIsTyVarKey
        isBaseLikeKey = trpiIsBaseLikeKey
        bindParents = trpiBindParents
        scopeRootC = trpiScopeRootC
        scopeGen = trpiScopeGen
        target0 = trpiTarget0
        targetBound = trpiTargetBound
        targetIsSchemeRoot = trpiTargetIsSchemeRoot
        targetIsSchemeRootForScope = trpiTargetIsSchemeRootForScope
        targetIsTyVar = trpiTargetIsTyVar
        targetBoundUnderOtherGen = trpiTargetBoundUnderOtherGen
        boundUnderOtherGen = trpiBoundUnderOtherGen
        boundIsDirectChild = trpiBoundIsDirectChild
        namedUnderGaSet = trpiNamedUnderGaSet
        typeRoot0 = trpiTypeRoot0
        typeRootFromBoundVar = trpiTypeRootFromBoundVar
        typeRootHasNamedOutsideGamma = trpiTypeRootHasNamedOutsideGamma
        boundHasForallForVar = trpiBoundHasForallForVar
        allowDropTarget = trpiAllowDropTarget
        dropTarget = trpiDropTarget
        schemeRootKeySet = trpiSchemeRootKeySet
        schemeRootByBody = trpiSchemeRootByBody
        schemeRootOwner = trpiSchemeRootOwner
        liftToForall = trpiLiftToForall
        useSchemeBodyForScope = False
        useBoundTypeRootLocal =
            not targetIsSchemeRoot &&
            case targetBound of
                Just bnd ->
                    IntMap.member (getNodeId (canonical bnd)) schemeRootByBody
                Nothing -> False
        schemeBodyRootLocal =
            case targetBound of
                Just bnd ->
                    case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                        Just TyForall{ tnBody = b } -> canonical b
                        _ -> canonical bnd
                Nothing -> typeRoot0
        targetInGammaLocal =
            IntSet.member (canonKey target0) namedUnderGaSet
        targetIsBaseLikeLocal =
            isBaseLikeKey (canonKey target0)
        schemeBodyChildUnderGenLocal =
            case scopeRootC of
                GenRef gid | targetIsSchemeRootForScope && targetIsTyVar ->
                    let children =
                            [ canonical child
                            | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents
                            , parentRef == GenRef gid
                            , TypeRef child <- [nodeRefFromKey childKey]
                            , not (isTyVarKey (canonKey child))
                            ]
                    in case children of
                        [child] -> Just child
                        _ -> Nothing
                _ -> Nothing
        boundRootForTypeLocal bnd0 =
            boundRootWith
                getNodeId
                canonical
                (`IntMap.lookup` nodes)
                (VarStore.lookupVarBound constraint)
                (\key -> IntMap.lookup key schemeRootByBody)
                True
                bnd0
        typeRootFromTargetBoundLocal =
            case (allowDropTarget, scopeGen, targetIsTyVar, targetBound) of
                (True, Just _, True, Just bnd) ->
                    let targetUnderScope =
                            case (scopeGen, IntMap.lookup (nodeRefKey (typeRef target0)) bindParents) of
                                (Just gid, Just (GenRef gid', _)) -> gid' == gid
                                _ -> False
                        targetIsSchemeRootAlias =
                            let bndC = canonical bnd
                                bndKey = getNodeId bndC
                            in (IntSet.member bndKey schemeRootKeySet
                                || IntMap.member bndKey schemeRootByBody)
                                && not targetIsSchemeRootForScope
                        root = boundRootForTypeLocal bnd
                        useBoundRoot =
                            not targetUnderScope
                                || targetIsSchemeRootAlias
                                || boundUnderOtherGen
                                || (boundIsDirectChild && not targetIsSchemeRootForScope && not targetInGammaLocal)
                    in if useBoundRoot
                            && canonical root /= canonical target0
                        then Just root
                        else Nothing
                _ -> Nothing
        typeRoot0Local =
            if useSchemeBodyForScope
                then schemeBodyRootLocal
                else
                    case (scopeRootC, targetIsSchemeRootForScope, targetIsTyVar, targetBound) of
                        (GenRef _, True, True, Nothing) ->
                            case schemeBodyChildUnderGenLocal of
                                Just child -> child
                                Nothing -> schemeBodyRootLocal
                        _ ->
                            case (dropTarget || useBoundTypeRootLocal, targetBound) of
                                (True, Just bnd) -> liftToForall bnd
                                _ ->
                                    case typeRootFromTargetBoundLocal of
                                        Just v -> v
                                        Nothing ->
                                            case typeRootFromBoundVar of
                                                Just v
                                                    | targetIsTyVar
                                                        && not (boundHasForallForVar v) -> v
                                                Just v
                                                    | targetIsTyVar
                                                        && targetBoundUnderOtherGen -> v
                                                Just v
                                                    | targetIsTyVar
                                                        && typeRootHasNamedOutsideGamma -> v
                                                _ -> typeRoot0
        typeRootLocal =
            case IntMap.lookup (canonKey typeRoot0Local) nodes of
                Just TyForall{ tnBody = b }
                    | targetIsTyVar
                    , targetIsSchemeRootForScope
                    , Just gid <- scopeGen
                    , Just gidOwner <- IntMap.lookup (canonKey typeRoot0Local) schemeRootOwner
                    , gid == gidOwner ->
                        canonical b
                _ -> typeRoot0Local
    in TypeRootPlan
        { trUseBoundTypeRoot = useBoundTypeRootLocal
        , trSchemeBodyRoot = schemeBodyRootLocal
        , trTargetInGamma = targetInGammaLocal
        , trTargetIsBaseLike = targetIsBaseLikeLocal
        , trSchemeBodyChildUnderGen = schemeBodyChildUnderGenLocal
        , trTypeRoot0 = typeRoot0Local
        , trTypeRoot = typeRootLocal
        }
