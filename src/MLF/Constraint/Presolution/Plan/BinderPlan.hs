{-# LANGUAGE RecordWildCards #-}
{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan
Description : Plan which variables become quantified binders
Copyright   : (c) 2024
License     : BSD-3-Clause

This module determines which type variables should be generalized as explicit
binders in a polymorphic type scheme. It handles:

* Filtering variables by reachability and scope
* Detecting scheme roots and alias binders
* Ordering binders by their bound dependencies
* Computing binder names (α-conversion)

The binder selection logic is complex because MLF allows variables to have
bounds, and those bounds may contain other variables. We must order binders
topologically so that each binder's bound only refers to previously-introduced
binders.

This module has been consolidated from several smaller modules:
* BinderHelpers - Checking binder properties
* Ordering - Topological ordering of binder candidates
* Util - Bound root computation
* Helpers - Binder selection helpers
-}
module MLF.Constraint.Presolution.Plan.BinderPlan (
    GaBindParentsInfo(..),
    BinderPlanInput(..),
    BinderPlan(..),
    buildBinderPlan,
    -- * Binder helpers (merged from BinderHelpers)
    isTargetSchemeBinderFor,
    boundMentionsSelfAliasFor,
    -- * Binder ordering (merged from Ordering)
    orderBinderCandidates,
    -- * Utility functions (merged from Util)
    boundRootWith,
    firstSchemeRootAncestorWith,
    -- * Binder selection (merged from Helpers)
    boundFlexChildrenUnder,
    bindableChildrenUnder,
    isQuantifiable,
    boundContainsForall,
    isScopeSchemeRoot,
    hasExplicitBoundFor,
    mkIsBindable,
    bindingScopeGen,
    bindersForGen,
    bindersForType,
    selectBinders,
    computeAliasBinders
) where

import Control.Monad (forM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Debug.Trace (trace)

import MLF.Constraint.Types
import MLF.Types.Elab (Ty(..))
import MLF.Util.ElabError (ElabError)
import MLF.Util.Names (alphaName)
import MLF.Reify.Core (reifyBoundWithNames, reifyBoundWithNamesOnConstraint)
import MLF.Reify.TypeOps (freeTypeVarsFrom)
import MLF.Util.Graph (reachableFrom, topoSortBy)
import MLF.Constraint.Solve (SolveResult)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.Order as Order
import MLF.Constraint.BindingUtil (bindingScopeFor)

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
                shouldAddExtras = targetIsSchemeRoot && onlyTarget && not (boundIsSchemeRootAll target0)
                extras = if shouldAddExtras then freeVarsFromBound else []
            in binders0 ++ extras ++ namedUnderGa
    traceGeneralizeM
        ("generalizeAt: bindersAdjusted=" ++ show binders0Adjusted
            ++ " reachable=" ++ show (IntSet.toList reachableForBinders)
        )
    let isTargetSchemeBinder v
            | targetIsBaseLike = False
            | canonical v == canonical target0 = True
            | otherwise =
                case VarStore.lookupVarBound constraint (canonical v) of
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
                Just bnd | bndC == canonical typeRoot || bndC == canonical typeRootForScheme ->
                    case IntMap.lookup (getNodeId bndC) nodes of
                        Just TyArrow{} -> True
                        Just TyForall{} -> True
                        Just TyExp{} -> True
                        _ -> False
                  where
                    bndC = canonical bnd
                _ -> False
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
                _ -> False
        isTypeRootBinder v = maybe False (\v0 -> canonical v == canonical v0) typeRootFromBoundVar
        aliasBinderIsTrivial v =
            all ($ v)
                [ not . isTargetSchemeBinder
                , not . isTypeRootBinder
                , boundIsVarAlias
                , \x -> not (inReachableTypeStructural x)
                , not . boundHasNamedOutsideGammaFor
                , not . boundMentionsSelfAlias
                ]
          where
            inReachableTypeStructural x = IntSet.member (getNodeId (canonical x)) reachableTypeStructural
        aliasBinderIsRedundant v inGamma =
            all ($ v)
                [ not . isTargetSchemeBinder
                , not . isTypeRootBinder
                , \x -> isSchemeRootAlias x || boundIsTypeRootAlias x
                , \x -> not (inReachableType x)
                , \_ -> not inGamma || boundIsTypeRootAlias v
                , not . boundIsSchemeBodyAlias
                ]
          where
            inReachableType x = IntSet.member (getNodeId (canonical x)) reachableType
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
        binders' =
            traceGeneralize
                ("generalizeAt: bindersFiltered=" ++ show binders
                    ++ " requiredGammaBinders=" ++ show requiredGammaBinders
                    ++ " typeRoot=" ++ show typeRoot
                )
                binders
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
            case IntMap.lookup (getNodeId (bpiCanonical v)) bpiNodes of
                Just TyVar{} -> VarStore.lookupVarBound bpiConstraint (bpiCanonical v) /= Nothing
                _ -> False

-- | Check if a variable is a target scheme binder.
isTargetSchemeBinderFor
    :: (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> Bool
    -> NodeId
    -> Bool
isTargetSchemeBinderFor canonical constraint target0 targetIsBaseLike v
    | targetIsBaseLike = False
    | canonical v == canonical target0 = True
    | otherwise =
        case VarStore.lookupVarBound constraint (canonical v) of
            Just bnd -> canonical bnd == canonical target0
            Nothing -> False

-- | Check if a bound mentions a self-alias.
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
        Just bnd -> any mentionsSelf (IntSet.toList (reachableFromWithBounds bnd))
          where
            binderKey = getNodeId (canonical v)
            mentionsSelf nidInt
                | IntSet.member keyC nestedSchemeInteriorSet = False
                | Just TyVar{} <- IntMap.lookup keyC nodes
                , Just repKey <- IntMap.lookup keyC gammaAlias = repKey == binderKey
                | otherwise = False
              where
                nidC = canonical (NodeId nidInt)
                keyC = getNodeId nidC
        Nothing -> False

-- | Order binder candidates topologically by their bound dependencies.
orderBinderCandidates
    :: Bool
    -> Maybe GaBindParentsInfo
    -> (NodeId -> NodeId)
    -> Constraint
    -> NodeId
    -> NodeId
    -> [Int]
    -> (Int -> Either ElabError [Int])
    -> Either ElabError [Int]
orderBinderCandidates debugEnabled mbBindParentsGa canonical constraint root rootBase candidates depsForE =
    let keysSolved = Order.orderKeysFromConstraintWith canonical constraint root Nothing
    in case mbBindParentsGa of
        Nothing -> orderBinderCandidatesSolved keysSolved candidates depsForE
        Just ga -> orderBinderCandidatesBase ga keysSolved rootBase candidates depsForE
  where
    traceOrderingEnabledM enabled msg =
        if enabled then trace msg (Right ()) else Right ()

    orderBinderCandidatesSolved keysSolved candidates' depsForE' = do
        let candidateSet = IntSet.fromList candidates'
            keyMaybe k = IntMap.lookup k keysSolved
            cmpReady a b =
                case (keyMaybe a, keyMaybe b) of
                    (Just ka, Just kb) ->
                        case Order.compareOrderKey ka kb of
                            EQ -> compare a b
                            other -> other
                    _ -> compare a b
            filterDeps k deps = filter (\d -> d /= k && IntSet.member d candidateSet) deps

        depsList <- forM candidates' $ \k -> do
            deps <- depsForE' k
            pure (k, filterDeps k deps)
        let depsFor k = IntMap.findWithDefault [] k (IntMap.fromList depsList)

        topoSortBy "generalizeAt: cycle in binder bound dependencies" cmpReady depsFor candidates'

    orderBinderCandidatesBase ga keysSolved rootBase' candidates' depsForE' = do
        let baseConstraint = gbiBaseConstraint ga
            keysBase = Order.orderKeysFromConstraintWith id baseConstraint rootBase' Nothing
            candidateSet = IntSet.fromList candidates'
            toBase k = IntMap.lookup k (gbiSolvedToBase ga)
            keyBase k = toBase k >>= (\b -> IntMap.lookup (getNodeId b) keysBase)
            keySolved k = IntMap.lookup k keysSolved
            missingKeys =
                [ k
                | k <- candidates'
                , Just baseN <- [toBase k]
                , not (IntMap.member (getNodeId baseN) keysBase)
                ]
            cmpReady a b =
                case (keyBase a, keyBase b) of
                    (Just ka, Just kb) ->
                        case Order.compareOrderKey ka kb of
                            EQ -> compare a b
                            other -> other
                    _ ->
                        case (keySolved a, keySolved b) of
                            (Just sa, Just sb) ->
                                case Order.compareOrderKey sa sb of
                                    EQ -> compare a b
                                    other -> other
                            _ -> compare a b
            filterDeps k deps = filter (\d -> d /= k && IntSet.member d candidateSet) deps

        traceOrderingEnabledM debugEnabled
            ("generalizeAt: missing base order keys (falling back to solved) "
                ++ show (map NodeId missingKeys)
            )

        depsList <- forM candidates' $ \k -> do
            deps <- depsForE' k
            pure (k, filterDeps k deps)
        let depsFor k = IntMap.findWithDefault [] k (IntMap.fromList depsList)

        topoSortBy "generalizeAt: cycle in binder bound dependencies" cmpReady depsFor candidates'

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

-- -----------------------------------------------------------------------------
-- Binder selection helpers (merged from Helpers.hs)
-- -----------------------------------------------------------------------------

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
    -> IntMap.IntMap TyNode
    -> Constraint
    -> NodeId
    -> Bool
hasExplicitBoundFor canonical nodes constraint v =
    case IntMap.lookup (getNodeId (canonical v)) nodes of
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
