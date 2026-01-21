{-# LANGUAGE RecordWildCards #-}

module MLF.Elab.Generalize.ReifyPlan (
    ReifyPlan(..),
    ReifyPlanInput(..),
    buildReifyPlan,
    ReifyBindingEnv(..),
    bindingFor,
    freeTypeVarsType
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Data.Maybe (isNothing)
import qualified Data.Set as Set

import MLF.Constraint.Types
import MLF.Constraint.Solve (SolveResult)
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Binding.Tree as Binding
import MLF.Elab.Generalize.BinderPlan (GaBindParentsInfo(..))
import MLF.Elab.Generalize.Names (alphaName)
import MLF.Elab.TypeOps (freeTypeVarsType)
import MLF.Elab.Reify
    ( reifyBoundWithNames
    , reifyBoundWithNamesOnConstraint
    )
import MLF.Elab.Types

data ReifyPlan = ReifyPlan
    { rpSubst :: IntMap.IntMap String
    , rpSubstBaseByKey :: IntMap.IntMap String
    , rpSubstForBound :: Int -> IntMap.IntMap String
    , rpSubstForBoundBase :: Int -> IntMap.IntMap String
    , rpTypeRootForReify :: NodeId
    , rpSubstForReify :: IntMap.IntMap String
    }

data ReifyPlanInput = ReifyPlanInput
    { rpiCanonical :: NodeId -> NodeId
    , rpiBindParentsGa :: Maybe GaBindParentsInfo
    , rpiExtraNameStart :: Int
    , rpiOrderedExtra :: [Int]
    , rpiSubst0 :: IntMap.IntMap String
    , rpiGammaAlias :: IntMap.IntMap Int
    , rpiNestedSchemeInteriorSet :: IntSet.IntSet
    , rpiBaseGammaRep :: IntMap.IntMap Int
    , rpiAliasBinderBases :: IntSet.IntSet
    , rpiSolvedToBasePref :: IntMap.IntMap NodeId
    , rpiTypeRoot :: NodeId
    }

data ReifyBindingEnv = ReifyBindingEnv
    { rbeConstraint :: Constraint
    , rbeNodes :: IntMap.IntMap TyNode
    , rbeCanonical :: NodeId -> NodeId
    , rbeBindParents :: BindParents
    , rbeScopeGen :: Maybe GenNodeId
    , rbeSchemeRootOwner :: IntMap.IntMap GenNodeId
    , rbeSchemeRootByBody :: IntMap.IntMap NodeId
    , rbeSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , rbeSchemeRootKeySet :: IntSet.IntSet
    , rbeGammaAlias :: IntMap.IntMap Int
    , rbeAliasBinderBases :: IntSet.IntSet
    , rbeSolvedToBasePref :: IntMap.IntMap NodeId
    , rbeNamedUnderGaSet :: IntSet.IntSet
    , rbeBinderSet :: IntSet.IntSet
    , rbeUniqueUnboundedName :: Maybe String
    , rbeResForReify :: SolveResult
    , rbeBindParentsGa :: Maybe GaBindParentsInfo
    , rbeBindingScopeGen :: NodeId -> Maybe GenNodeId
    , rbeHasExplicitBound :: NodeId -> Bool
    , rbeIsTargetSchemeBinder :: NodeId -> Bool
    , rbeBoundMentionsSelfAlias :: NodeId -> Bool
    , rbeContainsForall :: ElabType -> Bool
    , rbeParseNameId :: String -> Maybe Int
    , rbeFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , rbeTraceM :: String -> Either ElabError ()
    }

buildReifyPlan :: ReifyPlanInput -> ReifyPlan
buildReifyPlan ReifyPlanInput{..} =
    let extraNames = zipWith alphaName [rpiExtraNameStart ..] rpiOrderedExtra
        substExtra = IntMap.fromList (zip rpiOrderedExtra extraNames)
        substBaseLocal = IntMap.unions [rpiSubst0, substExtra]
        substAliasesLocal =
            IntMap.fromList
                [ (aliasKey, name)
                | (aliasKey, binderKey) <- IntMap.toList rpiGammaAlias
                , aliasKey /= binderKey
                , not (IntSet.member aliasKey rpiNestedSchemeInteriorSet)
                , Just name <- [IntMap.lookup binderKey substBaseLocal]
                ]
        substAliasesCanonLocal =
            IntMap.fromList
                [ (aliasKeyC, name)
                | (aliasKey, name) <- IntMap.toList substAliasesLocal
                , let aliasKeyC = getNodeId (rpiCanonical (NodeId aliasKey))
                , aliasKeyC /= aliasKey
                , not (IntMap.member aliasKeyC substBaseLocal)
                , not (IntMap.member aliasKeyC substAliasesLocal)
                ]
        substAliasesFromBaseLocal =
            IntMap.fromList
                [ (solvedKey, name)
                | (solvedKey, baseN) <- IntMap.toList rpiSolvedToBasePref
                , let baseKey = getNodeId baseN
                , Just repKey <- [IntMap.lookup baseKey rpiBaseGammaRep]
                , Just name <- [IntMap.lookup repKey substBaseLocal]
                , solvedKey /= repKey
                , not (IntSet.member solvedKey rpiNestedSchemeInteriorSet)
                ]
        substLocal =
            IntMap.unions
                [ substBaseLocal
                , substAliasesLocal
                , substAliasesCanonLocal
                , substAliasesFromBaseLocal
                ]
        aliasBinderKeysLocal = rpiAliasBinderBases
        filterAliasKeysLocal =
            if IntSet.null aliasBinderKeysLocal
                then id
                else IntMap.filterWithKey (\k _ -> not (IntSet.member k aliasBinderKeysLocal))
        substAliasesForLocal _binderKey =
            IntMap.fromList
                [ (aliasKey, name)
                | (aliasKey, binderKey') <- IntMap.toList rpiGammaAlias
                , aliasKey /= binderKey'
                , not (IntSet.member aliasKey rpiNestedSchemeInteriorSet)
                , Just name <- [IntMap.lookup binderKey' substBaseLocal]
                ]
        substForBoundLocal binderKey =
            filterAliasKeysLocal $
                IntMap.union substBaseLocal (substAliasesForLocal binderKey)
        substBaseByKeyLocal =
            case rpiBindParentsGa of
                Just ga ->
                    let fromBaseRep =
                            [ (baseKey, name)
                            | (baseKey, solvedKey) <- IntMap.toList rpiBaseGammaRep
                            , Just name <- [IntMap.lookup solvedKey substBaseLocal]
                            ]
                        fromSolved =
                            [ (getNodeId baseN, name)
                            | (solvedKey, name) <- IntMap.toList substBaseLocal
                            , Just baseN <- [IntMap.lookup solvedKey rpiSolvedToBasePref]
                            ]
                        fromBaseToSolved =
                            [ (baseKey, name)
                            | (baseKey, solvedN) <- IntMap.toList (gbiBaseToSolved ga)
                            , let solvedKey = getNodeId (rpiCanonical solvedN)
                            , Just name <- [IntMap.lookup solvedKey substBaseLocal]
                            ]
                    in IntMap.unions
                        [ IntMap.fromListWith (\_ old -> old) fromBaseRep
                        , IntMap.fromListWith (\_ old -> old) fromSolved
                        , IntMap.fromListWith (\_ old -> old) fromBaseToSolved
                        ]
                Nothing -> IntMap.empty
        substForBoundBaseLocal _binderKey = filterAliasKeysLocal substBaseByKeyLocal
        (typeRootForReifyLocal, substForReifyLocal) =
            case rpiBindParentsGa of
                Just _ ->
                    case IntMap.lookup (getNodeId (rpiCanonical rpiTypeRoot)) rpiSolvedToBasePref of
                        Just baseN
                            | rpiCanonical baseN /= rpiCanonical rpiTypeRoot ->
                                (baseN, substBaseByKeyLocal)
                        _ -> (rpiTypeRoot, substLocal)
                Nothing -> (rpiTypeRoot, substLocal)
    in ReifyPlan
        { rpSubst = substLocal
        , rpSubstBaseByKey = substBaseByKeyLocal
        , rpSubstForBound = substForBoundLocal
        , rpSubstForBoundBase = substForBoundBaseLocal
        , rpTypeRootForReify = typeRootForReifyLocal
        , rpSubstForReify = substForReifyLocal
        }

bindingFor
    :: ReifyBindingEnv
    -> ReifyPlan
    -> (String, Int)
    -> Either ElabError (String, Maybe ElabType)
bindingFor env plan (name, nidInt) = do
    let ReifyBindingEnv
            { rbeConstraint = constraint
            , rbeNodes = nodes
            , rbeCanonical = canonical
            , rbeBindParents = bindParents
            , rbeScopeGen = scopeGen
            , rbeSchemeRootOwner = schemeRootOwner
            , rbeSchemeRootByBody = schemeRootByBody
            , rbeSchemeRootByBodyBase = schemeRootByBodyBase
            , rbeSchemeRootKeySet = schemeRootKeySet
            , rbeGammaAlias = gammaAlias
            , rbeAliasBinderBases = aliasBinderBases
            , rbeSolvedToBasePref = solvedToBasePref
            , rbeNamedUnderGaSet = namedUnderGaSet
            , rbeBinderSet = binderSet
            , rbeUniqueUnboundedName = uniqueUnboundedName
            , rbeResForReify = resForReify
            , rbeBindParentsGa = mbBindParentsGa
            , rbeBindingScopeGen = bindingScopeGen
            , rbeHasExplicitBound = hasExplicitBound
            , rbeIsTargetSchemeBinder = isTargetSchemeBinder
            , rbeBoundMentionsSelfAlias = boundMentionsSelfAlias
            , rbeContainsForall = containsForall
            , rbeParseNameId = parseNameId
            , rbeFirstGenAncestor = firstGenAncestor
            , rbeTraceM = traceGeneralizeM
            } = env
        ReifyPlan
            { rpSubst = subst
            , rpSubstForBound = substForBound
            , rpSubstForBoundBase = substForBoundBase
            } = plan
        canonicalBinder v =
            let vC = canonical v
            in case IntMap.lookup (getNodeId vC) nodes of
                Just TyVar{} -> vC
                _ ->
                    case IntMap.lookup (getNodeId v) nodes of
                        Just TyVar{} -> v
                        _ -> vC
        bNodeC = canonicalBinder (NodeId nidInt)
        binderIsNamed = IntSet.member (getNodeId bNodeC) namedUnderGaSet
        binderKey = getNodeId bNodeC
        substForBound' = substForBound binderKey
        substNameSetForBound = Set.fromList (IntMap.elems substForBound')
        mbBoundNode = VarStore.lookupVarBound constraint bNodeC
        boundIsLocalSchemeRoot =
            case (scopeGen, mbBoundNode) of
                (Just gid, Just bnd) ->
                    case IntMap.lookup (getNodeId (canonical bnd)) schemeRootOwner of
                        Just gid' -> gid' == gid
                        Nothing -> False
                _ -> False
        boundIsLocalSchemeBody =
            case (scopeGen, mbBoundNode) of
                (Just gid, Just bnd) ->
                    case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
                        Just root ->
                            case IntMap.lookup (getNodeId (canonical root)) schemeRootOwner of
                                Just gid' -> gid' == gid
                                Nothing -> False
                        Nothing -> False
                _ -> False
        boundParentIsBinder =
            case mbBoundNode of
                Just bnd ->
                    case IntMap.lookup (nodeRefKey (typeRef (canonical bnd))) bindParents of
                        Just (TypeRef parent, _) -> canonical parent == bNodeC
                        _ -> False
                Nothing -> False
        boundRoot =
            case mbBoundNode of
                Just bnd ->
                    case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
                        Just _root
                            | boundIsLocalSchemeBody -> canonical bnd
                        Just root
                            | canonical root == bNodeC -> canonical bnd
                        Just root -> root
                        Nothing -> canonical bnd
                Nothing -> bNodeC
    traceGeneralizeM
        ("generalizeAt: boundRoot binder="
            ++ show bNodeC
            ++ " boundRoot="
            ++ show boundRoot
            ++ " boundParentIsBinder="
            ++ show boundParentIsBinder
            ++ " boundIsLocalSchemeBody="
            ++ show boundIsLocalSchemeBody
        )
    boundSchemeBinderKeys <- case IntMap.lookup (getNodeId (canonical boundRoot)) schemeRootOwner of
        Just gid
            | Just gid /= scopeGen -> do
                nestedBinders <- bindingToElab (Binding.boundFlexChildrenUnder canonical constraint (genRef gid))
                pure (IntSet.fromList (map (getNodeId . canonical) nestedBinders))
        _ -> pure IntSet.empty
    let substForBoundFiltered =
            if IntSet.null boundSchemeBinderKeys
                then substForBound'
                else IntMap.filterWithKey (\k _ -> not (IntSet.member k boundSchemeBinderKeys)) substForBound'
    let mbBaseRoot =
            if boundIsLocalSchemeBody || boundParentIsBinder
                then Nothing
                else
                    case mbBindParentsGa of
                        Just ga ->
                            let baseConstraint = gbiBaseConstraint ga
                                baseBoundForBinder =
                                    case IntMap.lookup binderKey solvedToBasePref of
                                        Just baseBinder ->
                                            case VarStore.lookupVarBound baseConstraint baseBinder of
                                                Just baseBnd ->
                                                    case IntMap.lookup (getNodeId baseBnd) schemeRootByBodyBase of
                                                        Just baseSchemeRoot ->
                                                            if baseSchemeRoot == baseBinder
                                                                then Just baseBnd
                                                                else Just baseSchemeRoot
                                                        Nothing -> Just baseBnd
                                                Nothing -> Nothing
                                        Nothing -> Nothing
                                fallbackFromBoundRoot =
                                    case IntMap.lookup (getNodeId (canonical boundRoot)) solvedToBasePref of
                                        Just baseRoot ->
                                            case IntMap.lookup (getNodeId baseRoot) schemeRootByBodyBase of
                                                Just baseSchemeRoot ->
                                                    if baseSchemeRoot == baseRoot
                                                        then Just baseRoot
                                                        else Just baseSchemeRoot
                                                Nothing -> Just baseRoot
                                        Nothing -> IntMap.lookup binderKey solvedToBasePref
                            in case baseBoundForBinder of
                                Just _ -> baseBoundForBinder
                                Nothing -> fallbackFromBoundRoot
                        Nothing -> Nothing
    boundTy0 <-
        case (mbBindParentsGa, mbBaseRoot) of
            (Just ga, Just baseRoot) ->
                reifyBoundWithNamesOnConstraint (gbiBaseConstraint ga) (substForBoundBase binderKey) baseRoot
            _ -> reifyBoundWithNames resForReify substForBoundFiltered boundRoot
    let fallbackAliasFor nm =
            case (uniqueUnboundedName, parseNameId nm) of
                (Just fallbackName, Just k)
                    | boundIsLocalSchemeBody
                        && not (Set.member nm substNameSetForBound) ->
                        let nid = canonical (NodeId k)
                        in case bindingScopeGen nid of
                            Just gid | Just gid /= scopeGen -> Just fallbackName
                            Nothing | isNothing scopeGen -> Just fallbackName
                            _ -> Nothing
                _ -> Nothing
        aliasNameFor nm =
            case parseNameId nm of
                Just k ->
                    let keyC = getNodeId (canonical (NodeId k))
                        repKey = IntMap.findWithDefault keyC keyC gammaAlias
                    in case IntMap.lookup repKey substForBound' of
                        Just nm' -> Just nm'
                        Nothing -> fallbackAliasFor nm
                Nothing -> fallbackAliasFor nm
        substAliasTy boundSet ty = case ty of
            TVar v ->
                if Set.member v boundSet
                    then TVar v
                    else case aliasNameFor v of
                            Just v' -> TVar v'
                            Nothing -> TVar v
            TArrow a b -> TArrow (substAliasTy boundSet a) (substAliasTy boundSet b)
            TBase _ -> ty
            TBottom -> ty
            TForall v mb body ->
                let mb' = fmap (substAliasTy boundSet) mb
                    body' = substAliasTy (Set.insert v boundSet) body
                in TForall v mb' body'
        boundTy0' =
            case (boundTy0, mbBoundNode) of
                (TBottom, Just _)
                    | binderIsNamed -> TBottom
                (TBottom, Just bnd) ->
                    let bndC = canonical bnd
                        bndKey = getNodeId bndC
                        nameForBound =
                            case IntMap.lookup bndKey substForBound' of
                                Just nm -> nm
                                Nothing -> "t" ++ show bndKey
                    in case (IntMap.lookup bndKey nodes, VarStore.lookupVarBound constraint bndC) of
                        (Just TyVar{}, Nothing) -> TVar nameForBound
                        _ -> boundTy0
                _ -> boundTy0
        boundTy0'' =
            if boundMentionsSelfAlias bNodeC
                then TBottom
                else boundTy0'
        boundTy0Aliased = substAliasTy Set.empty boundTy0''
        extraBoundNames =
            let isAliasBound nm =
                    case parseNameId nm of
                        Just k ->
                            let keyC = getNodeId (canonical (NodeId k))
                                repKey = IntMap.findWithDefault keyC keyC gammaAlias
                            in IntMap.member repKey substForBound'
                        Nothing -> False
                freeNames = Set.toList (freeTypeVarsType boundTy0Aliased)
            in [ nm
               | nm <- freeNames
               , not (Set.member nm substNameSetForBound)
               , not (isAliasBound nm)
               ]
        boundTy =
            foldr
                (\v acc -> TForall v Nothing acc)
                boundTy0Aliased
                (sort extraBoundNames)
    traceGeneralizeM
        ( "generalizeAt: boundExtras binder="
            ++ show bNodeC
            ++ " extras="
            ++ show extraBoundNames
            ++ " extraInfo="
            ++ show
                [ ( nm
                  , do nid <- parseNameId nm
                       let keyC = getNodeId (canonical (NodeId nid))
                       let baseM = IntMap.lookup keyC solvedToBasePref
                       let aliasM = IntMap.lookup keyC gammaAlias
                       pure (keyC, baseM, aliasM, firstGenAncestor (typeRef (NodeId keyC)))
                  )
                | nm <- extraBoundNames
                ]
        )
    traceGeneralizeM
        ("generalizeAt: boundSelfAlias binder="
            ++ show bNodeC
            ++ " mentionsSelf="
            ++ show (boundMentionsSelfAlias bNodeC)
            ++ " boundTy0="
            ++ show boundTy0
            ++ " boundTy="
            ++ show boundTy
        )
    let boundIsFreeVar =
            case boundTy of
                TVar _ ->
                    case mbBoundNode of
                        Just bnd ->
                            let bndC = canonical bnd
                                bndHasExplicitBound = VarStore.lookupVarBound constraint bndC
                            in case IntMap.lookup (getNodeId bndC) nodes of
                                Just TyVar{} ->
                                    isNothing bndHasExplicitBound
                                        && not (IntMap.member (getNodeId bndC) subst)
                                _ -> False
                        _ -> False
                _ -> False
        boundIsFreeVar' =
            boundIsFreeVar && not binderIsNamed
        boundIsSelfVar =
            case boundTy of
                TVar v -> isNothing mbBoundNode && v == name
                _ -> False
        boundIsSchemeRootNode =
            case mbBoundNode of
                Just bnd -> IntSet.member (getNodeId (canonical bnd)) schemeRootKeySet
                Nothing -> False
        boundAllowed =
            if binderIsNamed
                then True
            else if boundIsLocalSchemeRoot || boundIsLocalSchemeBody
                then isTargetSchemeBinder bNodeC
                else
                    hasExplicitBound bNodeC
                        || boundParentIsBinder
                        || boundIsSchemeRootNode
                        || case mbBoundNode of
                            Just bnd -> IntSet.member (getNodeId (canonical bnd)) binderSet
                            Nothing -> False
                        || containsForall boundTy
        mbBound =
            if IntSet.member (getNodeId bNodeC) aliasBinderBases
                then
                    if boundTy == TBottom
                        then Nothing
                        else Just boundTy
                else if boundTy == TBottom || boundIsFreeVar' || boundIsSelfVar || not boundAllowed
                    then Nothing
                    else Just boundTy
    pure (name, mbBound)
