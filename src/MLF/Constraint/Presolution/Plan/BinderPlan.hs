{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.BinderPlan (
    GaBindParentsInfo(..),
    BinderPlanInput(..),
    BinderPlan(..),
    buildBinderPlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Debug.Trace (trace)

import MLF.Constraint.Types
import MLF.Types.Elab (ElabType(..))
import MLF.Util.ElabError (ElabError)
import MLF.Constraint.Presolution.Plan.Util (boundRootWith)
import MLF.Constraint.Presolution.Plan.Names (alphaName)
import MLF.Reify.Core (reifyBoundWithNames, reifyBoundWithNamesOnConstraint)
import MLF.Reify.TypeOps (freeTypeVarsFrom)
import MLF.Util.Graph (reachableFrom)
import MLF.Constraint.Solve (SolveResult)
import qualified MLF.Constraint.VarStore as VarStore

data GaBindParentsInfo = GaBindParentsInfo
    { gbiBindParentsBase :: BindParents
    , gbiBaseConstraint :: Constraint
    , gbiBaseToSolved :: IntMap.IntMap NodeId
    , gbiSolvedToBase :: IntMap.IntMap NodeId
    }

data BinderPlanInput = BinderPlanInput
    { bpiDebugEnabled :: Bool
    , bpiConstraint :: Constraint
    , bpiNodes :: IntMap.IntMap TyNode
    , bpiCanonical :: NodeId -> NodeId
    , bpiCanonKey :: NodeId -> Int
    , bpiIsTyVarKey :: Int -> Bool
    , bpiBindParents :: BindParents
    , bpiBindParentsGa :: Maybe GaBindParentsInfo
    , bpiScopeRootC :: NodeRef
    , bpiScopeGen :: Maybe GenNodeId
    , bpiTarget0 :: NodeId
    , bpiTargetBound :: Maybe NodeId
    , bpiTargetIsSchemeRoot :: Bool
    , bpiTargetIsBaseLike :: Bool
    , bpiBoundUnderOtherGen :: Bool
    , bpiBinders0 :: [NodeId]
    , bpiNamedUnderGa :: [NodeId]
    , bpiGammaAlias :: IntMap.IntMap Int
    , bpiBaseGammaSet :: IntSet.IntSet
    , bpiBaseGammaRep :: IntMap.IntMap Int
    , bpiBaseGammaRepSet :: IntSet.IntSet
    , bpiNamedUnderGaSet :: IntSet.IntSet
    , bpiSolvedToBasePref :: IntMap.IntMap NodeId
    , bpiReachable :: IntSet.IntSet
    , bpiReachableForBinders :: IntSet.IntSet
    , bpiReachableType :: IntSet.IntSet
    , bpiReachableTypeStructural :: IntSet.IntSet
    , bpiTypeRoot0 :: NodeId
    , bpiTypeRoot :: NodeId
    , bpiTypeRootFromBoundVar :: Maybe NodeId
    , bpiTypeRootIsForall :: Bool
    , bpiLiftToForall :: NodeId -> NodeId
    , bpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , bpiResForReify :: SolveResult
    , bpiGammaKeyFor :: Int -> Int -> Int
    , bpiNestedSchemeInteriorSet :: IntSet.IntSet
    , bpiBoundIsSchemeRootVar :: NodeId -> Bool
    , bpiBoundIsSchemeRootAll :: NodeId -> Bool
    , bpiIsNestedSchemeBound :: NodeId -> Bool
    , bpiSchemeRootKeySet :: IntSet.IntSet
    , bpiSchemeRootByBody :: IntMap.IntMap NodeId
    , bpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , bpiSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , bpiParseNameId :: String -> Maybe Int
    , bpiAliasBinderBases :: IntSet.IntSet
    , bpiOrderBinderCandidates :: [Int] -> (Int -> Either ElabError [Int]) -> Either ElabError [Int]
    }

data BinderPlan = BinderPlan
    { bpBindersCanon :: [NodeId]
    , bpBinderIds :: [Int]
    , bpOrderedBinderIds :: [Int]
    , bpBinderNames :: [String]
    , bpSubst0 :: IntMap.IntMap String
    , bpNestedSchemeInteriorSet :: IntSet.IntSet
    , bpGammaAlias :: IntMap.IntMap Int
    , bpBaseGammaSet :: IntSet.IntSet
    , bpBaseGammaRep :: IntMap.IntMap Int
    , bpNamedUnderGaSet :: IntSet.IntSet
    , bpSolvedToBasePref :: IntMap.IntMap NodeId
    , bpReachableForBinders :: IntSet.IntSet
    , bpAliasBinderBases :: IntSet.IntSet
    , bpOrderBinders :: [Int] -> Either ElabError [Int]
    }

traceBinderPlanEnabled :: Bool -> String -> a -> a
traceBinderPlanEnabled enabled msg value =
    if enabled then trace msg value else value

traceBinderPlanEnabledM :: Bool -> String -> Either ElabError ()
traceBinderPlanEnabledM enabled msg =
    traceBinderPlanEnabled enabled msg (Right ())

buildBinderPlan :: BinderPlanInput -> Either ElabError BinderPlan
buildBinderPlan BinderPlanInput{..} = do
    let traceGeneralize = traceBinderPlanEnabled bpiDebugEnabled
        traceGeneralizeM = traceBinderPlanEnabledM bpiDebugEnabled
        constraint = bpiConstraint
        nodes = bpiNodes
        canonical = bpiCanonical
        canonKey = bpiCanonKey
        isTyVarKey = bpiIsTyVarKey
        bindParents = bpiBindParents
        mbBindParentsGa = bpiBindParentsGa
        scopeRootC = bpiScopeRootC
        scopeGen = bpiScopeGen
        target0 = bpiTarget0
        targetBound = bpiTargetBound
        targetIsSchemeRoot = bpiTargetIsSchemeRoot
        targetIsBaseLike = bpiTargetIsBaseLike
        boundUnderOtherGen = bpiBoundUnderOtherGen
        binders0 = bpiBinders0
        namedUnderGa = bpiNamedUnderGa
        gammaAlias = bpiGammaAlias
        baseGammaSet = bpiBaseGammaSet
        baseGammaRep = bpiBaseGammaRep
        baseGammaRepSet = bpiBaseGammaRepSet
        namedUnderGaSet = bpiNamedUnderGaSet
        solvedToBasePref = bpiSolvedToBasePref
        reachable = bpiReachable
        reachableForBinders = bpiReachableForBinders
        reachableType = bpiReachableType
        reachableTypeStructural = bpiReachableTypeStructural
        typeRoot0 = bpiTypeRoot0
        typeRoot = bpiTypeRoot
        typeRootFromBoundVar = bpiTypeRootFromBoundVar
        typeRootIsForall = bpiTypeRootIsForall
        liftToForall = bpiLiftToForall
        reachableFromWithBounds = bpiReachableFromWithBounds
        resForReify = bpiResForReify
        gammaKeyFor = bpiGammaKeyFor
        nestedSchemeInteriorSet = bpiNestedSchemeInteriorSet
        boundIsSchemeRootVar = bpiBoundIsSchemeRootVar
        boundIsSchemeRootAll = bpiBoundIsSchemeRootAll
        isNestedSchemeBound = bpiIsNestedSchemeBound
        schemeRootKeySet = bpiSchemeRootKeySet
        schemeRootByBody = bpiSchemeRootByBody
        schemeRootOwnerBase = bpiSchemeRootOwnerBase
        schemeRootByBodyBase = bpiSchemeRootByBodyBase
        parseNameId = bpiParseNameId
        aliasBinderBases = bpiAliasBinderBases
        orderBinderCandidates = bpiOrderBinderCandidates

    let binders0Adjusted =
            let freeVarsFromBound =
                    case targetBound of
                        Just bnd ->
                            [ canonical (NodeId nid)
                            | nid <- IntSet.toList (reachableFromWithBounds bnd)
                            , case IntMap.lookup nid nodes of
                                Just TyVar{} -> not (VarStore.isEliminatedVar constraint (NodeId nid))
                                _ -> False
                            ]
                        Nothing -> []
                targetC = canonical target0
                activeBinders =
                    [ v
                    | v <- binders0
                    , IntSet.member (canonKey v) reachableForBinders
                    ]
                onlyTarget = case activeBinders of
                    [] -> True
                    [v] -> canonical v == targetC
                    _ -> False
                extras =
                    if targetIsSchemeRoot && onlyTarget && not (boundIsSchemeRootAll target0)
                        then freeVarsFromBound
                        else []
            in binders0 ++ extras ++ namedUnderGa
    traceGeneralizeM
        ("generalizeAt: bindersAdjusted=" ++ show binders0Adjusted
            ++ " reachable=" ++ show (IntSet.toList reachableForBinders)
        )
    let isTargetSchemeBinder v =
            if targetIsBaseLike
                then False
                else
                    canonical v == canonical target0
                        || case VarStore.lookupVarBound constraint (canonical v) of
                            Just bnd -> canonical bnd == canonical target0
                            Nothing -> False
    let extraReachable =
            case (scopeGen, IntMap.lookup (getNodeId typeRoot0) nodes) of
                (Just _, _) -> []
                (_, Just TyVar{}) -> []
                (Nothing, _) ->
                    [ canonical v
                    | nid <- IntSet.toList reachable
                    , Just TyVar{} <- [IntMap.lookup nid nodes]
                    , let v = NodeId nid
                    , not (VarStore.isEliminatedVar constraint (canonical v))
                    , not (elem (canonical v) binders0Adjusted)
                    , case IntMap.lookup (nodeRefKey (typeRef (canonical v))) bindParents of
                        Just (GenRef _, _) -> False
                        _ -> True
                    ]
        bindersCandidates = binders0Adjusted ++ extraReachable ++ namedUnderGa
        canonicalBinder v =
            let vC = canonical v
            in case IntMap.lookup (getNodeId vC) nodes of
                Just TyVar{} -> vC
                _ ->
                    case IntMap.lookup (getNodeId v) nodes of
                        Just TyVar{} -> v
                        _ -> vC
        canonicalizeBinder v =
            let vC = canonicalBinder v
                vKey = getNodeId vC
            in case IntMap.lookup vKey gammaAlias of
                Just repKey
                    | IntSet.member vKey baseGammaRepSet -> vC
                    | otherwise ->
                        canonicalBinder (NodeId repKey)
                Nothing -> vC
        normalizedBinders =
            [ canonicalizeBinder v
            | v <- bindersCandidates
            ]
        bindersCandidatesCanonical =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- normalizedBinders
                    ]
    let typeRootForScheme = liftToForall typeRoot
        isSchemeRootAlias v =
            IntSet.member (getNodeId (canonical v)) schemeRootKeySet
                && canonical v /= canonical target0
                && case VarStore.lookupVarBound constraint (canonical v) of
                    Just bnd ->
                        let bndC = canonical bnd
                        in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                    Nothing -> False
        boundIsSchemeBodyAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                    in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                Nothing -> False
        boundIsStructuralAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                        isStruct =
                            case IntMap.lookup (getNodeId bndC) nodes of
                                Just TyArrow{} -> True
                                Just TyForall{} -> True
                                Just TyExp{} -> True
                                _ -> False
                    in (bndC == canonical typeRoot || bndC == canonical typeRootForScheme) && isStruct
                Nothing -> False
        boundIsTypeRootAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    let bndC = canonical bnd
                    in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
                Nothing -> False
        boundHasNamedOutsideGammaFor v =
            case mbBindParentsGa of
                Just ga
                    | Just baseK <- IntMap.lookup (getNodeId (canonical v)) solvedToBasePref ->
                        let baseConstraint = gbiBaseConstraint ga
                            baseNodes = cNodes baseConstraint
                            baseParents = gbiBindParentsBase ga
                            scopeGenBase = scopeGen
                            boundSchemeOwnerBase bnd =
                                case IntMap.lookup (getNodeId bnd) schemeRootOwnerBase of
                                    Just gid -> Just gid
                                    Nothing ->
                                        case IntMap.lookup (getNodeId bnd) schemeRootByBodyBase of
                                            Just root ->
                                                IntMap.lookup (getNodeId root) schemeRootOwnerBase
                                            Nothing -> Nothing
                            allowBoundTraversalBase bnd =
                                case boundSchemeOwnerBase bnd of
                                    Nothing -> True
                                    Just gid ->
                                        case scopeGenBase of
                                            Just scopeGid -> gid == scopeGid
                                            Nothing -> False
                            reachableFromWithBoundsBase root0 =
                                let children nid =
                                        case IntMap.lookup (getNodeId nid) baseNodes of
                                            Nothing -> []
                                            Just node ->
                                                case node of
                                                    TyVar{ tnBound = Just bnd }
                                                        | allowBoundTraversalBase bnd ->
                                                            structuralChildrenWithBounds node
                                                    _ ->
                                                        structuralChildren node
                                in reachableFrom getNodeId id children root0
                            isNamedOutsideBase nidInt =
                                case IntMap.lookup nidInt baseNodes of
                                    Just TyVar{} ->
                                        case IntMap.lookup (nodeRefKey (TypeRef (NodeId nidInt))) baseParents of
                                            Just (GenRef _, _) ->
                                                not (IntSet.member nidInt baseGammaSet)
                                            _ -> False
                                    _ -> False
                        in case VarStore.lookupVarBound baseConstraint baseK of
                            Just bnd ->
                                let reachableBound = reachableFromWithBoundsBase bnd
                                in any isNamedOutsideBase (IntSet.toList reachableBound)
                            Nothing -> False
                _ ->
                    case VarStore.lookupVarBound constraint (canonical v) of
                        Just bnd ->
                            let reachableBound = reachableFromWithBounds bnd
                                binderKey = getNodeId (canonical v)
                                isNamedOutside nidInt =
                                    let nidC = canonical (NodeId nidInt)
                                        keyC = getNodeId nidC
                                    in case IntMap.lookup keyC nodes of
                                        Just TyVar{} ->
                                            if IntSet.member keyC nestedSchemeInteriorSet
                                                then False
                                                else case IntMap.lookup (nodeRefKey (typeRef nidC)) bindParents of
                                                    Just (GenRef _, _) ->
                                                        not (IntSet.member (gammaKeyFor binderKey keyC) namedUnderGaSet)
                                                    _ -> False
                                        _ -> False
                            in any isNamedOutside (IntSet.toList reachableBound)
                        Nothing -> False
        boundMentionsSelfAlias v =
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
        aliasBoundIsBottomOrNone v =
            isSchemeRootAlias v
                && not (isTargetSchemeBinder v)
                && case VarStore.lookupVarBound constraint (canonical v) of
                    Nothing -> True
                    Just bnd ->
                        let bndC = canonical bnd
                        in case (IntMap.lookup (getNodeId bndC) nodes, VarStore.lookupVarBound constraint bndC) of
                            (Just TyVar{}, Nothing) -> True
                            (Just TyBottom{}, _) -> True
                            _ -> False
        boundIsVarAlias v =
            case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd ->
                    case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                        Just TyVar{} -> True
                        _ -> False
                Nothing -> False
        isTypeRootBinder v =
            case typeRootFromBoundVar of
                Just v0 -> canonical v == canonical v0
                Nothing -> False
        aliasBinderIsTrivial v =
            not (isTargetSchemeBinder v)
                && not (isTypeRootBinder v)
                && boundIsVarAlias v
                && not (IntSet.member (getNodeId (canonical v)) reachableTypeStructural)
                && not (boundHasNamedOutsideGammaFor v)
                && not (boundMentionsSelfAlias v)
        aliasBinderIsRedundant v inGamma =
            not (isTargetSchemeBinder v)
                && not (isTypeRootBinder v)
                && (isSchemeRootAlias v || boundIsTypeRootAlias v)
                && not (IntSet.member (getNodeId (canonical v)) reachableType)
                && (not inGamma || boundIsTypeRootAlias v)
                && not (boundIsSchemeBodyAlias v)
    let forallBoundBinders =
            IntSet.fromList
                [ canonKey child
                | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents
                , TypeRef parent <- [parentRef]
                , IntSet.member (canonKey parent) reachableType
                , case IntMap.lookup (canonKey parent) nodes of
                    Just TyForall{} -> True
                    _ -> False
                , IntSet.member (canonKey parent) schemeRootKeySet
                , TypeRef child <- [nodeRefFromKey childKey]
                , isTyVarKey (canonKey child)
                ]
    let binderCandidateKeys =
            IntSet.fromList [ canonKey v | v <- bindersCandidatesCanonical ]
    let binders =
            [ canonicalBinder v
            | v <- bindersCandidatesCanonical
            , let vKey = canonKey v
            , let gammaKey =
                    case IntMap.lookup vKey gammaAlias of
                        Just repKey -> repKey
                        Nothing -> vKey
            , let keepTypeRootBinder =
                    case typeRootFromBoundVar of
                        Just v0 -> canonKey v0 == vKey
                        Nothing -> False
            , let inGamma =
                    case mbBindParentsGa of
                        Just _ ->
                            let keysToCheck = [vKey, gammaKey]
                                inBase =
                                    any
                                        (\k ->
                                            case IntMap.lookup k solvedToBasePref of
                                                Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                                Nothing -> False
                                        )
                                        keysToCheck
                            in inBase || IntSet.member gammaKey namedUnderGaSet
                        Nothing ->
                            IntSet.member gammaKey namedUnderGaSet
                                || IntSet.member gammaKey aliasBinderBases
                                || IntSet.member vKey aliasBinderBases
            , case scopeRootC of
                GenRef _ ->
                    (inGamma || IntSet.member vKey aliasBinderBases)
                        && (IntSet.member vKey reachableForBinders || isTargetSchemeBinder v)
                TypeRef _ -> IntSet.member vKey reachableForBinders
            , case mbBindParentsGa of
                Just _ ->
                    if keepTypeRootBinder
                        then True
                        else
                            case IntMap.lookup vKey solvedToBasePref of
                                Just baseN ->
                                    case IntMap.lookup (getNodeId baseN) baseGammaRep of
                                        Just repKey ->
                                            repKey == vKey
                                                || not (IntSet.member repKey binderCandidateKeys)
                                                || IntSet.member vKey baseGammaRepSet
                                        Nothing -> True
                                Nothing -> True
                Nothing -> True
            , not (IntSet.member (canonKey v) nestedSchemeInteriorSet
                && not (isTargetSchemeBinder v)
                && not (IntSet.member vKey aliasBinderBases))
            , not (isNestedSchemeBound v && not inGamma && not (isTargetSchemeBinder v))
            , not (boundIsSchemeRootVar v && not (isTargetSchemeBinder v) && not (hasExplicitBound v) && not inGamma)
            , not (boundIsSchemeRootAll v
                && not (isTargetSchemeBinder v)
                && not (boundHasNamedOutsideGammaFor v)
                && not (IntSet.member (canonKey v) reachableForBinders)
                )
            , not (isSchemeRootAlias v
                && not (isTargetSchemeBinder v)
                && not (boundHasNamedOutsideGammaFor v)
                && not (boundMentionsSelfAlias v)
                && (not inGamma || boundIsTypeRootAlias v)
                )
            , not (boundIsSchemeBodyAlias v
                && not (isTargetSchemeBinder v)
                && not inGamma
                )
            , not (boundIsStructuralAlias v
                && canonical v /= canonical target0
                && not (boundIsSchemeBodyAlias v)
                )
            , not (aliasBoundIsBottomOrNone v && not inGamma)
            , not (aliasBinderIsTrivial v)
            , not (aliasBinderIsRedundant v inGamma)
            , not (IntSet.member (canonKey v) forallBoundBinders && not typeRootIsForall)
            ]
    traceGeneralizeM
        ("generalizeAt: binder filters="
            ++ show
                [ let vKey = canonKey v
                      gammaKey =
                          case IntMap.lookup vKey gammaAlias of
                              Just repKey -> repKey
                              Nothing -> vKey
                      inGammaDbg =
                          case mbBindParentsGa of
                              Just _ ->
                                  let keysToCheck = [vKey, gammaKey]
                                      inBase =
                                            any
                                                (\k ->
                                                    case IntMap.lookup k solvedToBasePref of
                                                        Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                                        Nothing -> False
                                                )
                                                keysToCheck
                                  in inBase
                              Nothing -> IntSet.member gammaKey namedUnderGaSet
                  in ( v
                     , IntSet.member vKey reachable
                     , isNestedSchemeBound v
                     , boundIsSchemeRootVar v
                     , boundIsSchemeRootAll v
                     , boundIsTypeRootAlias v
                     , boundUnderOtherGen
                     , IntSet.member vKey forallBoundBinders
                     , inGammaDbg
                     , IntMap.lookup (nodeRefKey (typeRef (canonical v))) bindParents
                     )
                | v <- binders0Adjusted
                ]
        )
    let requiredGammaBinders :: [NodeId]
        requiredGammaBinders = []
        bindersWithRequired =
            binders
        binders' =
            traceGeneralize
                ("generalizeAt: bindersFiltered=" ++ show binders
                    ++ " requiredGammaBinders=" ++ show requiredGammaBinders
                    ++ " typeRoot=" ++ show typeRoot
                )
                bindersWithRequired
        bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId v, v)
                    | v <- binders'
                    ]
    let binderIds = map getNodeId bindersCanon
        candidateSet = IntSet.fromList binderIds
        nameForDep k = "t" ++ show k
        substDeps =
            IntMap.fromList
                [ (k, nameForDep k)
                | k <- binderIds
                ]
        substDepsBase =
            case mbBindParentsGa of
                Just _ ->
                    IntMap.fromListWith
                        (\_ old -> old)
                        [ (getNodeId baseN, nameForDep k)
                        | k <- binderIds
                        , Just baseN <- [IntMap.lookup k solvedToBasePref]
                        ]
                Nothing -> IntMap.empty
        nameToId = parseNameId
        substDepsFor k =
            IntMap.union substDeps $
                IntMap.fromList
                    [ (aliasKey, nameForDep binderKey)
                    | (aliasKey, binderKey) <- IntMap.toList gammaAlias
                    , aliasKey /= binderKey
                    , binderKey /= k
                    ]
        boundRootForDeps bnd0 =
            boundRootWith
                getNodeId
                canonical
                (`IntMap.lookup` nodes)
                (VarStore.lookupVarBound constraint)
                (`IntMap.lookup` schemeRootByBody)
                False
                bnd0
        boundDepsForCandidate k =
            let isBaseRep =
                    case IntMap.lookup k solvedToBasePref of
                        Just baseK ->
                            case IntMap.lookup (getNodeId baseK) baseGammaRep of
                                Just repKey -> repKey == k
                                Nothing -> False
                        Nothing -> False
            in case mbBindParentsGa of
                Just ga
                    | Just baseK <- IntMap.lookup k solvedToBasePref
                    , isBaseRep ->
                        let baseConstraint = gbiBaseConstraint ga
                            baseNodes = cNodes baseConstraint
                            boundRootForDepsBase bnd0 =
                                boundRootWith
                                    getNodeId
                                    id
                                    (`IntMap.lookup` baseNodes)
                                    (VarStore.lookupVarBound baseConstraint)
                                    (`IntMap.lookup` schemeRootByBodyBase)
                                    False
                                    bnd0
                        in case VarStore.lookupVarBound baseConstraint baseK of
                            Nothing -> do
                                let boundTy = TVar (nameForDep k)
                                    freeNames = Set.toList (freeTypeVarsFrom Set.empty boundTy)
                                    deps =
                                        [ dep
                                        | name <- freeNames
                                        , Just dep <- [nameToId name]
                                        , dep /= k
                                        , IntSet.member dep candidateSet
                                        ]
                                pure deps
                            Just bnd -> do
                                let bndRoot = boundRootForDepsBase bnd
                                boundTy <- reifyBoundWithNamesOnConstraint baseConstraint substDepsBase bndRoot
                                let bndRootKey = getNodeId bndRoot
                                    freeNames0 = Set.toList (freeTypeVarsFrom Set.empty boundTy)
                                    freeNames =
                                        case (boundTy, IntMap.lookup bndRootKey baseNodes, VarStore.lookupVarBound baseConstraint bndRoot) of
                                            (TBottom, Just TyVar{}, Nothing) ->
                                                [nameForDep bndRootKey]
                                            _ -> freeNames0
                                    deps =
                                        [ dep
                                        | name <- freeNames
                                        , Just dep <- [nameToId name]
                                        , dep /= k
                                        , IntSet.member dep candidateSet
                                        ]
                                traceGeneralizeM
                                    ("generalizeAt: boundDeps k="
                                        ++ show k
                                        ++ " bndRoot="
                                        ++ show bndRoot
                                        ++ " boundTy="
                                        ++ show boundTy
                                        ++ " freeNames="
                                        ++ show freeNames
                                        ++ " deps="
                                        ++ show deps
                                    )
                                pure deps
                _ -> do
                    let subst = substDepsFor k
                    case VarStore.lookupVarBound constraint (canonical (NodeId k)) of
                        Nothing -> do
                            let boundTy = TVar (nameForDep k)
                                freeNames = Set.toList (freeTypeVarsFrom Set.empty boundTy)
                                deps =
                                    [ dep
                                    | name <- freeNames
                                    , Just dep <- [nameToId name]
                                    , dep /= k
                                    , IntSet.member dep candidateSet
                                    ]
                            pure deps
                        Just bnd -> do
                            let bndRoot = boundRootForDeps bnd
                            boundTy <- reifyBoundWithNames resForReify subst bndRoot
                            let bndRootC = canonical bndRoot
                                bndRootKey = getNodeId bndRootC
                                freeNames0 = Set.toList (freeTypeVarsFrom Set.empty boundTy)
                                freeNames =
                                    case (boundTy, IntMap.lookup bndRootKey nodes, VarStore.lookupVarBound constraint bndRootC) of
                                        (TBottom, Just TyVar{}, Nothing) ->
                                            [nameForDep bndRootKey]
                                        _ -> freeNames0
                                deps =
                                    [ dep
                                    | name <- freeNames
                                    , Just dep <- [nameToId name]
                                    , dep /= k
                                    , IntSet.member dep candidateSet
                                    ]
                            traceGeneralizeM
                                ("generalizeAt: boundDeps k="
                                    ++ show k
                                    ++ " bndRoot="
                                    ++ show bndRoot
                                    ++ " boundTy="
                                    ++ show boundTy
                                    ++ " freeNames="
                                    ++ show freeNames
                                    ++ " deps="
                                    ++ show deps
                                )
                            pure deps
        orderBinders candidates =
            orderBinderCandidates
                candidates
                boundDepsForCandidate

    ordered0 <- orderBinders binderIds
    traceGeneralizeM
        ("generalizeAt: binderIds=" ++ show binderIds
            ++ " ordered0=" ++ show ordered0
        )
    let names = zipWith alphaName [0..] ordered0
        subst0 = IntMap.fromList (zip ordered0 names)
    pure BinderPlan
        { bpBindersCanon = bindersCanon
        , bpBinderIds = binderIds
        , bpOrderedBinderIds = ordered0
        , bpBinderNames = names
        , bpSubst0 = subst0
        , bpNestedSchemeInteriorSet = nestedSchemeInteriorSet
        , bpGammaAlias = gammaAlias
        , bpBaseGammaSet = baseGammaSet
        , bpBaseGammaRep = baseGammaRep
        , bpNamedUnderGaSet = namedUnderGaSet
        , bpSolvedToBasePref = solvedToBasePref
        , bpReachableForBinders = reachableForBinders
        , bpAliasBinderBases = aliasBinderBases
        , bpOrderBinders = orderBinders
        }

    where
        hasExplicitBound v =
            case IntMap.lookup (getNodeId (bpiCanonical v)) (bpiNodes) of
                Just TyVar{} ->
                    case VarStore.lookupVarBound (bpiConstraint) (bpiCanonical v) of
                        Nothing -> False
                        Just _ -> True
                _ -> False
