module MLF.Elab.Generalize (
    GaBindParents(..),
    applyGeneralizePlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Constraint.Presolution.Plan
    ( GeneralizePlan(..)
    , ReifyPlan(..)
    )
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Plan.BinderPlan (BinderPlan(..))
import MLF.Constraint.Presolution.Plan.Context
    ( GaBindParents(..)
    , GeneralizeEnv(..)
    , GeneralizeCtx(..)
    , traceGeneralizeM
    )
import MLF.Util.Names (alphaName)
import MLF.Constraint.Presolution.Plan.Target (TypeRootPlan(..))
import MLF.Constraint.Presolution.Plan.Finalize (FinalizeInput(..), finalizeScheme)
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Constraint.Presolution.Plan.SchemeRoots
    ( SchemeRootInfo(..)
    , SchemeRootsPlan(..)
    , allowBoundTraversalFor
    )
import MLF.Reify.Core
    ( reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    , reifyBoundWithNames
    )
import MLF.Reify.TypeOps (freeTypeVarsFrom)
import MLF.Elab.Types
import MLF.Util.Graph (reachableFromStop)

applyGeneralizePlan
    :: (NodeRef -> NodeId -> Either ElabError ElabScheme)
    -> GeneralizePlan
    -> ReifyPlan
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
applyGeneralizePlan generalizeAtForScheme plan reifyPlanWrapper = do
    let GeneralizePlan
            { gpEnv = env
            , gpContext = ctx
            , gpSchemeRootsPlan = schemeRootsPlan
            , gpTypeRootPlan = typeRootPlan
            , gpBinderPlan = binderPlan
            , gpScopeHasStructuralScheme = scopeHasStructuralScheme
            , gpBinders0 = binders0
            , gpReachableFromWithBounds = reachableFromWithBounds
            , gpBindParents = bindParents
            } = plan
        GeneralizeEnv
            { geConstraint = constraint
            , geNodes = nodes
            , geCanonical = canonical
            , geBindParentsGa = mbBindParentsGa
            } = env
        GeneralizeCtx
            { gcTarget0 = target0
            , gcScopeRootC = scopeRootC
            , gcScopeGen = scopeGen
            , gcFirstGenAncestor = firstGenAncestorGa
            , gcResForReify = resForReify
            , gcBindParentsGaInfo = mbBindParentsGaInfo
            } = ctx
        SchemeRootsPlan
            { srInfo = schemeRootInfo
            , srSchemeRootByBodyBase = schemeRootByBodyBase
            } = schemeRootsPlan
        SchemeRootInfo
            { sriRootKeySet = schemeRootKeySet
            , sriRootOwner = schemeRootOwner
            , sriRootByBody = schemeRootByBody
            } = schemeRootInfo
        TypeRootPlan
            { trTypeRoot = typeRoot
            } = typeRootPlan
        BinderPlan
            { bpBinderNames = binderNames
            , bpOrderedBinderIds = orderedBinders
            , bpGammaAlias = gammaAliasPlan
            , bpNamedUnderGaSet = namedUnderGaSetPlan
            , bpSolvedToBasePref = solvedToBasePrefPlan
            , bpAliasBinderBases = aliasBinderBasesPlan
            } = binderPlan
        ReifyPlan
            { rpPlan = reifyPlan
            , rpTypeRootForReifyAdjusted = typeRootForReifyAdjusted
            , rpSubstForReifyAdjusted = substForReifyAdjusted
            } = reifyPlanWrapper
        Reify.ReifyPlan
            { Reify.rpSubst = subst
            , Reify.rpSubstBaseByKey = substBaseByKey
            , Reify.rpSchemeTypeChoice = schemeTypeChoice
            , Reify.rpBindingScopeGen = bindingScopeGenPlan
            , Reify.rpHasExplicitBound = hasExplicitBoundPlan
            , Reify.rpIsTargetSchemeBinder = isTargetSchemeBinderPlan
            , Reify.rpBoundMentionsSelfAlias = boundMentionsSelfAliasPlan
            , Reify.rpContainsForall = containsForallPlan
            , Reify.rpParseNameId = parseNameIdPlan
            } = reifyPlan
        allowBoundTraversal =
            allowBoundTraversalFor schemeRootsPlan canonical scopeGen target0
        childrenWithBounds nid =
            case IntMap.lookup (getNodeId nid) nodes of
                Nothing -> []
                Just node ->
                    case node of
                        TyVar{ tnBound = Just bnd }
                            | allowBoundTraversal bnd ->
                                structuralChildrenWithBounds node
                        _ -> structuralChildren node
    let unboundedBinderNames =
            [ name
            | (name, nidInt) <- zip binderNames orderedBinders
            , case VarStore.lookupVarBound constraint (canonical (NodeId nidInt)) of
                Nothing -> True
                Just _ -> False
            ]
        uniqueUnboundedName =
            case unboundedBinderNames of
                [nm] -> Just nm
                _ -> Nothing
    let binderSet = IntSet.fromList orderedBinders
        bindingEnv =
            Reify.ReifyBindingEnv
                { Reify.rbeConstraint = constraint
                , Reify.rbeNodes = nodes
                , Reify.rbeCanonical = canonical
                , Reify.rbeBindParents = bindParents
                , Reify.rbeScopeGen = scopeGen
                , Reify.rbeSchemeRootOwner = schemeRootOwner
                , Reify.rbeSchemeRootByBody = schemeRootByBody
                , Reify.rbeSchemeRootByBodyBase = schemeRootByBodyBase
                , Reify.rbeSchemeRootKeySet = schemeRootKeySet
                , Reify.rbeGammaAlias = gammaAliasPlan
                , Reify.rbeAliasBinderBases = aliasBinderBasesPlan
                , Reify.rbeSolvedToBasePref = solvedToBasePrefPlan
                , Reify.rbeNamedUnderGaSet = namedUnderGaSetPlan
                , Reify.rbeBinderSet = binderSet
                , Reify.rbeUniqueUnboundedName = uniqueUnboundedName
                , Reify.rbeResForReify = resForReify
                , Reify.rbeBindParentsGa = mbBindParentsGaInfo
                , Reify.rbeBindingScopeGen = bindingScopeGenPlan
                , Reify.rbeHasExplicitBound = hasExplicitBoundPlan
                , Reify.rbeIsTargetSchemeBinder = isTargetSchemeBinderPlan
                , Reify.rbeBoundMentionsSelfAlias = boundMentionsSelfAliasPlan
                , Reify.rbeContainsForall = containsForallPlan
                , Reify.rbeParseNameId = parseNameIdPlan
                , Reify.rbeFirstGenAncestor = firstGenAncestorGa
                , Reify.rbeTraceM = traceGeneralizeM env
                }
    -- Phase 8: construct per-binder bounds.
    bindings <- mapM (Reify.bindingFor bindingEnv reifyPlan) (zip binderNames orderedBinders)
    reachableType <- Right (reachableFromWithBounds typeRoot)

    -- Phase 9: scheme ownership and type reification.
    let typeRootC = canonical typeRoot
        Reify.SchemeTypeChoice
            { Reify.stcUseSchemeType = useSchemeTypeAdjusted
            , Reify.stcSchemeOwnerFromBody = schemeOwnerFromBody
            , Reify.stcSchemeOwnerFromBodyIsAlias = schemeOwnerFromBodyIsAlias
            , Reify.stcSchemeOwners = schemeOwners
            } = schemeTypeChoice
        ownersByRoot =
            [ gnId gen
            | gen <- IntMap.elems (cGenNodes constraint)
            , any (\root -> canonical root == typeRootC) (gnSchemes gen)
            ]
    let reifyTypeWithAliases bodyRoot substBase binderPairs = do
            let bodyRootC = canonical bodyRoot
                useConstraintReify =
                    case IntMap.lookup (getNodeId bodyRootC) nodes of
                        Just TyVar{} ->
                            case VarStore.lookupVarBound constraint bodyRootC of
                                Just bnd -> getNodeId bnd == getNodeId bodyRoot
                                Nothing -> False
                        _ -> False
                reifyWith substRoot substMap constraintArg resArg =
                    if useConstraintReify
                        then reifyTypeWithNamesNoFallbackOnConstraint constraintArg substMap substRoot
                        else reifyTypeWithNamesNoFallback resArg substMap substRoot
            let reachableWithoutBound bnd =
                    let stopSet = IntSet.singleton (getNodeId (canonical bnd))
                        shouldStop nid = IntSet.member (getNodeId nid) stopSet
                    in reachableFromStop
                        getNodeId
                        canonical
                        childrenWithBounds
                        shouldStop
                        bodyRoot
                aliasEntries =
                    [ (getNodeId (canonical bnd), name)
                    | (b, name) <- binderPairs
                    , Just bnd <- [VarStore.lookupVarBound constraint (canonical b)]
                    , canonical bnd /= bodyRootC
                    , not (IntSet.member (getNodeId (canonical b)) (reachableWithoutBound bnd))
                    ]
            if null aliasEntries
                then reifyWith bodyRoot substBase constraint resForReify
                else do
                    let aliasNodes =
                            IntMap.fromList
                                [ (key, TyVar { tnId = NodeId key, tnBound = Nothing })
                                | (key, _) <- aliasEntries
                                ]
                        constraintAlias =
                            constraint { cNodes = IntMap.union aliasNodes nodes }
                        substAlias =
                            IntMap.union (IntMap.fromList aliasEntries) substBase
                    let resAlias = resForReify { srConstraint = constraintAlias }
                    reifyWith bodyRoot substAlias constraintAlias resAlias

    let reifySchemeType =
            if useSchemeTypeAdjusted
                then do
                    let reachableVars =
                            [ NodeId nid
                            | nid <- IntSet.toList reachableType
                            , nid /= getNodeId typeRootC
                            , case IntMap.lookup nid nodes of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                        hasReachableBinder gid =
                            any
                                (\nid -> firstGenAncestorGa (typeRef nid) == Just gid)
                                reachableVars
                        schemeOwnerCandidates = filter hasReachableBinder schemeOwners
                        schemeScope =
                            case schemeOwnerFromBody of
                                Just gid
                                    | schemeOwnerFromBodyIsAlias
                                    , (owner:_) <- ownersByRoot ->
                                        genRef owner
                                    | otherwise -> genRef gid
                                Nothing ->
                                    case schemeOwnerCandidates of
                                        (gid:_) -> genRef gid
                                        [] ->
                                            case schemeOwners of
                                                (gid:_) -> genRef gid
                                                [] -> typeRef typeRootC
                    if schemeScope == scopeRootC
                        then do
                            traceGeneralizeM env
                                ("generalizeAt: schemeScope equals scopeRootC; skipping recursive scheme-type fallback"
                                    ++ " scopeRootC=" ++ show scopeRootC
                                    ++ " typeRootC=" ++ show typeRootC
                                )
                            reifyTypeWithAliases
                                typeRootForReifyAdjusted
                                substForReifyAdjusted
                                (zip (map NodeId orderedBinders) binderNames)
                        else do
                            sch <- generalizeAtForScheme schemeScope typeRootC
                            pure $
                                case sch of
                                    Forall binds body ->
                                        foldr (\(n, b) t -> TForall n b t) body binds
                else do
                    let explicitSchemeBinders = binders0
                    explicitSchemeTy <-
                        case (null bindings, scopeHasStructuralScheme, explicitSchemeBinders) of
                            (True, True, explicitBinders0@(_:_)) -> do
                                let binderKeys =
                                        IntSet.fromList
                                            [ getNodeId (canonical b)
                                            | b <- explicitBinders0
                                            ]
                                    binderKeysList = IntSet.toList binderKeys
                                    binders = [ NodeId key | key <- binderKeysList ]
                                    names = zipWith alphaName [0..] binderKeysList
                                    substExplicit = IntMap.fromList (zip binderKeysList names)
                                    explicitBodyRoot =
                                        case IntMap.lookup (getNodeId typeRootC) nodes of
                                            Just TyVar{} ->
                                                case VarStore.lookupVarBound constraint (canonical typeRootC) of
                                                    Just bnd -> canonical bnd
                                                    Nothing -> typeRootForReifyAdjusted
                                            _ -> typeRootForReifyAdjusted
                                if null binders
                                    then pure Nothing
                                    else do
                                        bodyTy <-
                                            reifyTypeWithAliases
                                                explicitBodyRoot
                                                substExplicit
                                                (zip binders names)
                                        bounds <-
                                            mapM
                                                (\(b, name) ->
                                                    case VarStore.lookupVarBound constraint (canonical b) of
                                                        Nothing -> pure (name, Nothing)
                                                        Just bnd -> do
                                                            bndTy <-
                                                                reifyBoundWithNames
                                                                    resForReify
                                                                    substExplicit
                                                                    (canonical bnd)
                                                            let selfBound =
                                                                    case bndTy of
                                                                        TVar v -> v == name
                                                                        _ -> False
                                                                mbBound =
                                                                    if bndTy == TBottom || selfBound
                                                                        then Nothing
                                                                        else Just bndTy
                                                            pure (name, mbBound)
                                                )
                                                (zip binders names)
                                        let tyExplicit =
                                                foldr
                                                    (\(n, mb) acc -> TForall n mb acc)
                                                    bodyTy
                                                    bounds
                                        pure (Just tyExplicit)
                            _ -> pure Nothing
                    case explicitSchemeTy of
                        Just ty -> pure ty
                        Nothing ->
                            if scopeHasStructuralScheme && null bindings
                                then
                                    reifyTypeWithNamesNoFallbackOnConstraint
                                        constraint
                                        substForReifyAdjusted
                                        typeRootForReifyAdjusted
                                else
                                    case mbBindParentsGa of
                                        Just ga ->
                                            case IntMap.lookup (getNodeId (canonical typeRoot)) solvedToBasePrefPlan of
                                                Just baseN
                                                    | canonical baseN /= canonical typeRoot -> do
                                                        tyBase <-
                                                            reifyTypeWithNamesNoFallbackOnConstraint
                                                                (gaBaseConstraint ga)
                                                                substBaseByKey
                                                                baseN
                                                        let freeBase = freeTypeVarsFrom Set.empty tyBase
                                                            allowedBase = Set.fromList (IntMap.elems substBaseByKey)
                                                        if Set.isSubsetOf freeBase allowedBase
                                                            then pure tyBase
                                                            else reifyTypeWithAliases
                                                                typeRootForReifyAdjusted
                                                                substForReifyAdjusted
                                                                (zip (map NodeId orderedBinders) binderNames)
                                                _ ->
                                                    reifyTypeWithAliases
                                                        typeRootForReifyAdjusted
                                                        substForReifyAdjusted
                                                        (zip (map NodeId orderedBinders) binderNames)
                                        Nothing ->
                                            reifyTypeWithAliases
                                                typeRootForReifyAdjusted
                                                substForReifyAdjusted
                                                (zip (map NodeId orderedBinders) binderNames)
    ty0Raw <- reifySchemeType
    finalizeScheme FinalizeInput
        { fiEnv = env
        , fiConstraint = constraint
        , fiCanonical = canonical
        , fiBindParents = bindParents
        , fiScopeRootC = scopeRootC
        , fiTypeRoot = typeRoot
        , fiTypeRootC = typeRootC
        , fiScopeGen = scopeGen
        , fiFirstGenAncestorGa = firstGenAncestorGa
        , fiBindParentsGa = mbBindParentsGa
        , fiSolvedToBasePref = solvedToBasePrefPlan
        , fiGammaAlias = gammaAliasPlan
        , fiNamedUnderGaSet = namedUnderGaSetPlan
        , fiOrderedBinders = orderedBinders
        , fiBinderNames = binderNames
        , fiBindings = bindings
        , fiSubst = subst
        , fiTyRaw = ty0Raw
        }
