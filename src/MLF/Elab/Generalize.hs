module MLF.Elab.Generalize (
    GaBindParents(..),
    generalizeAt,
    generalizeAtKeepTarget,
    generalizeAtAllowRigid,
    generalizeAtKeepTargetAllowRigid,
    generalizeAtAllowRigidWithBindParents,
    generalizeAtKeepTargetAllowRigidWithBindParents,
    applyGeneralizePlan
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Presolution.Plan
    ( GeneralizePlan(..)
    , PresolutionEnv(..)
    , ReifyPlan(..)
    , planGeneralizeAt
    , planReify
    )
import MLF.Constraint.Solve (SolveResult(..))
import qualified MLF.Constraint.Solve as Solve
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.FreeNames (freeNamesFrom, freeNamesOf)
import MLF.Elab.Generalize.BinderPlan (BinderPlan(..))
import MLF.Elab.Generalize.BinderHelpers
    ( boundMentionsSelfAliasFor
    , isTargetSchemeBinderFor
    )
import MLF.Elab.Generalize.BindingUtil
    ( bindingPathToRootLocal
    , bindingScopeFor
    , firstGenAncestorFrom
    )
import MLF.Elab.Generalize.Context
    ( GaBindParents(..)
    , GeneralizeEnv(..)
    , GeneralizeCtx(..)
    , traceGeneralize
    , traceGeneralizeM
    )
import MLF.Elab.Generalize.Helpers
    ( bindingScopeGen
    , hasExplicitBound
    )
import MLF.Elab.Generalize.Names (alphaName, parseNameId)
import MLF.Elab.Generalize.Normalize
    ( simplifySchemeBindings
    , promoteArrowAlias
    , isBaseBound
    , isVarBound
    , containsForall
    )
import MLF.Elab.Generalize.Plan
    ( TargetPlan(..)
    , TypeRootPlan(..)
    )
import qualified MLF.Elab.Generalize.ReifyPlan as Reify
import MLF.Elab.Generalize.SchemeRoots
    ( SchemeRootInfo(..)
    , SchemeRootsPlan(..)
    , allowBoundTraversalFor
    )
import qualified MLF.Elab.Generalize.SchemeRoots as SchemeRoots
import MLF.Elab.Reify
    ( reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    , reifyBoundWithNames
    )
import MLF.Elab.TypeOps (stripForallsType)
import MLF.Elab.Types
import MLF.Elab.Util (reachableFromStop)

-- | Generalize a node at the given binding site into a polymorphic scheme.
-- For xMLF, quantified variables can have bounds.
-- Returns the scheme and the substitution used to rename variables.
generalizeAt :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAt = generalizeAtWith True False Nothing

-- | Variant of 'generalizeAt' that keeps the target binder even when it would
-- normally be dropped as an alias wrapper.
generalizeAtKeepTarget :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTarget = generalizeAtWith False True Nothing

-- | Variant of 'generalizeAt' that allows rigid binders to be quantified
-- while still dropping alias targets.
generalizeAtAllowRigid :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtAllowRigid = generalizeAtWith True True Nothing

-- | Variant of 'generalizeAt' that allows rigid binders while keeping
-- alias targets.
generalizeAtKeepTargetAllowRigid :: SolveResult -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTargetAllowRigid = generalizeAtWith False True Nothing

generalizeAtAllowRigidWithBindParents
    :: GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtAllowRigidWithBindParents gaParents =
    generalizeAtWith True True (Just gaParents)

generalizeAtKeepTargetAllowRigidWithBindParents
    :: GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtKeepTargetAllowRigidWithBindParents gaParents =
    generalizeAtWith False True (Just gaParents)

generalizeAtWith
    :: Bool
    -> Bool
    -> Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtWith allowDropTarget allowRigidBinders mbBindParentsGa res scopeRoot targetNode = do
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
        presEnv =
            PresolutionEnv
                { peConstraint = constraint
                , peSolveResult = res
                , peCanonical = canonical
                , peBindParents = cBindParents constraint
                , peAllowDropTarget = allowDropTarget
                , peAllowRigidBinders = allowRigidBinders
                , peBindParentsGa = mbBindParentsGa
                , peScopeRoot = scopeRoot
                , peTargetNode = targetNode
                }
    genPlan <- planGeneralizeAt presEnv
    reifyPlan <- planReify presEnv genPlan
    applyGeneralizePlan genPlan reifyPlan

applyGeneralizePlan
    :: GeneralizePlan
    -> ReifyPlan
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
applyGeneralizePlan plan reifyPlanWrapper = do
    let GeneralizePlan
            { gpEnv = env
            , gpContext = ctx
            , gpSchemeRootsPlan = schemeRootsPlan
            , gpTargetPlan = targetPlan
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
            , geRes = res
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
        TargetPlan
            { tpTargetBound = targetBound
            } = targetPlan
        TypeRootPlan
            { trTargetIsBaseLike = targetIsBaseLike
            , trTypeRoot = typeRoot
            } = typeRootPlan
        BinderPlan
            { bpBinderNames = binderNames
            , bpOrderedBinderIds = orderedBinders
            , bpGammaAlias = gammaAliasPlan
            , bpNestedSchemeInteriorSet = nestedSchemeInteriorSetPlan
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
        isTargetSchemeBinder =
            isTargetSchemeBinderFor canonical constraint target0 targetIsBaseLike
        boundMentionsSelfAlias =
            boundMentionsSelfAliasFor
                canonical
                constraint
                nodes
                gammaAliasPlan
                nestedSchemeInteriorSetPlan
                reachableFromWithBounds
        bindingScopeGen' = bindingScopeGen constraint
        hasExplicitBound' = hasExplicitBound canonical nodes constraint
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
                , Reify.rbeBindingScopeGen = bindingScopeGen'
                , Reify.rbeHasExplicitBound = hasExplicitBound'
                , Reify.rbeIsTargetSchemeBinder = isTargetSchemeBinder
                , Reify.rbeBoundMentionsSelfAlias = boundMentionsSelfAlias
                , Reify.rbeContainsForall = containsForall
                , Reify.rbeParseNameId = parseNameId
                , Reify.rbeFirstGenAncestor = firstGenAncestorGa
                , Reify.rbeTraceM = traceGeneralizeM env
                }
    -- Phase 8: construct per-binder bounds.
    bindings <- mapM (Reify.bindingFor bindingEnv reifyPlan) (zip binderNames orderedBinders)
    reachableType <- Right (reachableFromWithBounds typeRoot)

    -- Phase 9: scheme ownership and type reification.
    let typeRootC = canonical typeRoot
        (schemeOwnerFromBody, schemeOwnerFromBodyIsAlias) =
            SchemeRoots.schemeOwnerFromBody schemeRootsPlan solvedToBasePrefPlan typeRootC
        ownersByRoot =
            [ gnId gen
            | gen <- IntMap.elems (cGenNodes constraint)
            , any (\root -> canonical root == typeRootC) (gnSchemes gen)
            ]
        schemeOwners =
            maybe ownersByRoot (\gid -> gid : ownersByRoot) schemeOwnerFromBody
    typeInScope <-
        case scopeRootC of
            GenRef gid ->
                pure (bindingScopeFor constraint (typeRef typeRootC) == Just gid)
            _ -> pure False
    let typeInScopeAdjusted =
            case (scopeGen, schemeOwnerFromBody) of
                (Just gid, Just owner)
                    | owner /= gid ->
                        False
                _ -> typeInScope
    let useSchemeType =
            case (scopeRootC, scopeGen, schemeOwnerFromBody) of
                (GenRef _, Just gid, Just owner)
                    | owner /= gid -> True
                (GenRef _, Just gid, _) ->
                    not typeInScopeAdjusted &&
                    not (null schemeOwners) &&
                    not (gid `elem` schemeOwners)
                _ -> False
    let typeRootIsTargetBound =
            case targetBound of
                Just bnd -> canonical bnd == canonical typeRoot
                Nothing -> False
    let useSchemeTypeAdjusted =
            case (schemeOwnerFromBody, scopeGen) of
                (Just owner, Just gid)
                    | owner /= gid && not typeRootIsTargetBound -> False
                _ -> useSchemeType
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
                            (sch, _substScheme) <-
                                generalizeAtWith False True mbBindParentsGa res schemeScope typeRootC
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
                                                        let freeBase = freeNamesFrom Set.empty tyBase
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
    let aliasToTypeRootNames =
            [ name
            | (nidInt, name) <- zip orderedBinders binderNames
            , let nid = NodeId nidInt
            , Just bnd <- [VarStore.lookupVarBound constraint (canonical nid)]
            , canonical bnd == canonical typeRoot
            ]
        inlineAliasBinder ty binds = case ty of
            TVar v
                | v `elem` aliasToTypeRootNames ->
                    case lookup v binds of
                        Just (Just bnd)
                            | not (isVarBound bnd)
                            , not (isBaseBound bnd) ->
                                (bnd, filter (\(n, _) -> n /= v) binds)
                        _ -> (ty, binds)
            _ -> (ty, binds)
        (ty0RawAlias, bindingsAlias) = inlineAliasBinder ty0Raw bindings
    -- Phase 10: normalize, rename, and prune bindings + type.
    let canonAllVars ty =
            let (ty', _freeEnv, _n) = go [] [] (0 :: Int) ty
            in ty'
          where
            go boundEnv freeEnv n ty0 = case ty0 of
                TVar v ->
                    case lookup v boundEnv of
                        Just v' -> (TVar v', freeEnv, n)
                        Nothing ->
                            case lookup v freeEnv of
                                Just v' -> (TVar v', freeEnv, n)
                                Nothing ->
                                    let v' = "v" ++ show n
                                    in (TVar v', (v, v') : freeEnv, n + 1)
                TBase b -> (TBase b, freeEnv, n)
                TBottom -> (TBottom, freeEnv, n)
                TArrow a b ->
                    let (a', free1, n1) = go boundEnv freeEnv n a
                        (b', free2, n2) = go boundEnv free1 n1 b
                    in (TArrow a' b', free2, n2)
                TForall v mb body ->
                    let v' = "v" ++ show n
                        n1 = n + 1
                        (mb', free1, n2) =
                            case mb of
                                Nothing -> (Nothing, freeEnv, n1)
                                Just bnd ->
                                    let (bnd', free', n') = go boundEnv freeEnv n1 bnd
                                    in (Just bnd', free', n')
                        (body', free2, n3) = go ((v, v') : boundEnv) free1 n2 body
                    in (TForall v' mb' body', free2, n3)
        replaceAlias boundNorm v = goReplace
          where
            goReplace ty
                | canonAllVars ty == boundNorm = TVar v
                | otherwise =
                    case ty of
                        TArrow a b -> TArrow (goReplace a) (goReplace b)
                        TForall name mb body ->
                            TForall name (fmap goReplace mb) (goReplace body)
                        _ -> ty
        stripAliasForall ty = case ty of
            TForall v (Just bound) body
                | TVar v' <- body
                , v == v' ->
                    stripAliasForall bound
                | otherwise ->
                    TForall v (Just (stripAliasForall bound)) (stripAliasForall body)
            TForall v Nothing body ->
                TForall v Nothing (stripAliasForall body)
            TArrow a b -> TArrow (stripAliasForall a) (stripAliasForall b)
            _ -> ty
        collapseBoundAliases binds ty =
            foldr
                (\(v, mbBound) acc ->
                    case mbBound of
                        Nothing -> acc
                        Just bound ->
                            let boundCore = stripForallsType bound
                            in if isVarBound boundCore
                                then acc
                                else
                                    let boundNorm = canonAllVars boundCore
                                    in if canonAllVars acc == boundNorm
                                        then acc
                                        else replaceAlias boundNorm v acc
                )
                ty
                binds
    let normalizeScheme tyRaw binds =
            let tyAdjusted =
                    case (binds, tyRaw) of
                        ((v, mb):_, TForall v' mb' body)
                            | v == v' && mb == mb' -> body
                        _ -> tyRaw
                tyAliased = stripAliasForall (collapseBoundAliases binds tyAdjusted)
            in traceGeneralize env
                ("generalizeAt: ty0Raw=" ++ show tyAliased
                    ++ " subst=" ++ show subst
                    ++ " bindings=" ++ show binds
                )
                (tyAliased, binds)
        (ty0RawAdjusted, bindingsAdjusted) = normalizeScheme ty0RawAlias bindingsAlias
        nameForId k = "t" ++ show k
        substNames =
            [ (nameForId k, name)
            | (k, name) <- IntMap.toList subst
            ]
        namedBinderNames =
            Set.union
                (Set.fromList
                    [ name
                    | (nidInt, name) <- IntMap.toList subst
                    , IntSet.member nidInt namedUnderGaSetPlan
                    ])
                (Set.fromList [ name | (name, Just _) <- bindingsAdjusted ])
        renameVars = cata alg
          where
            renameFromSubst v = case lookup v substNames of
                Just v' -> v'
                Nothing -> v
            alg ty = case ty of
                TVarF v -> TVar (renameFromSubst v)
                TArrowF a b -> TArrow a b
                TBaseF b -> TBase b
                TBottomF -> TBottom
                TForallF v mb body ->
                    let v' = renameFromSubst v
                    in TForall v' mb body
        ty0 = renameVars ty0RawAdjusted
        inlineBaseBounds = False
        (bindingsNorm0, tyNorm0) =
            simplifySchemeBindings inlineBaseBounds namedBinderNames bindingsAdjusted ty0
        (bindingsNorm, tyNorm) = promoteArrowAlias bindingsNorm0 tyNorm0
        usedNames =
            Set.unions
                ( freeNamesOf tyNorm
                    : [freeNamesOf b | (_, Just b) <- bindingsNorm]
                )
        bindingsFinal =
            filter
                (\(name, _) ->
                    Set.member name usedNames || Set.member name namedBinderNames
                )
                bindingsNorm
        bindingsFinal' =
            let dropRedundant (name, mb) =
                    not (Set.member name usedNames) &&
                    case mb of
                        Nothing -> True
                        Just bnd ->
                            let freeBound = freeNamesFrom Set.empty bnd
                                boundMentionsSelf = Set.member name freeBound
                                boundIsSimple = isVarBound bnd || isBaseBound bnd
                                boundIsBody = bnd == tyNorm
                            in not boundMentionsSelf && (boundIsSimple || boundIsBody)
            in filter (not . dropRedundant) bindingsFinal
        renameTypeVars = cata alg
          where
            renameFromMap v = Map.findWithDefault v v renameMap
            alg ty = case ty of
                TVarF v -> TVar (renameFromMap v)
                TArrowF a b -> TArrow a b
                TBaseF b -> TBase b
                TBottomF -> TBottom
                TForallF v mb body ->
                    let v' = renameFromMap v
                    in TForall v' mb body
        renameMap =
            Map.fromList
                [ (old, alphaName idx 0)
                | (idx, (old, _)) <- zip [0..] bindingsFinal'
                ]
        renameName name = Map.findWithDefault name name renameMap
        bindingsRenamed =
            [ (renameName name, fmap renameTypeVars mb)
            | (name, mb) <- bindingsFinal'
            ]
        tyRenamed = renameTypeVars tyNorm
        _ =
            traceGeneralize env
                ("generalizeAt: tyNorm=" ++ show tyNorm
                    ++ " usedNames=" ++ show (Set.toList usedNames)
                    ++ " bindingsNorm=" ++ show bindingsNorm
                    ++ " bindingsFinal=" ++ show bindingsFinal'
                    ++ " bindingsRenamed=" ++ show bindingsRenamed
                )
                ()
        usedNamesRenamed =
            Set.unions
                ( freeNamesOf tyRenamed
                    : [freeNamesOf b | (_, Just b) <- bindingsRenamed]
                )
        boundNames = Set.fromList (map fst bindingsRenamed)
        allowedNames = Set.fromList (map fst bindingsRenamed)
        missingNamesRaw = Set.toList (Set.difference usedNamesRenamed boundNames)
        aliasAllowed name =
            case parseNameId name of
                Just nid ->
                    let keyC = getNodeId (canonical (NodeId nid))
                        aliasKey = case IntMap.lookup keyC gammaAliasPlan of
                            Just repKey -> repKey
                            Nothing -> keyC
                    in case IntMap.lookup aliasKey subst of
                        Just nm -> Set.member (renameName nm) boundNames
                        Nothing -> False
                Nothing -> False
        missingNamesRaw' = filter (not . aliasAllowed) missingNamesRaw
        missingNames =
            case scopeGen of
                Nothing -> missingNamesRaw'
                Just gid ->
                    let underScope name =
                            case parseNameId name of
                                Just nid ->
                                    let nidRef = NodeId nid
                                        underSolved =
                                            firstGenAncestorGa (typeRef nidRef) == Just gid
                                        underBase =
                                            case mbBindParentsGa of
                                                Just ga ->
                                                    case IntMap.lookup nid solvedToBasePrefPlan of
                                                        Just baseN ->
                                                            firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN) == Just gid
                                                        Nothing -> underSolved
                                                Nothing -> underSolved
                                    in underBase
                                Nothing -> True
                    in filter underScope missingNamesRaw'
        keepNames = map fst bindingsRenamed
        subst' = IntMap.filter (`elem` keepNames) (IntMap.map renameName subst)
        finalizeScheme missing =
            if null missing
                then pure (Forall bindingsRenamed tyRenamed, subst')
                else
                    traceGeneralize env
                        ("generalizeAt: SchemeFreeVars typeRoot="
                            ++ show typeRootC
                            ++ " scopeRoot="
                            ++ show scopeRootC
                            ++ " scopeGen="
                            ++ show scopeGen
                            ++ " missing="
                            ++ show missing
                            ++ " bindingsFinal="
                            ++ show bindingsFinal
                            ++ " usedNames="
                            ++ show (Set.toList usedNames)
                            ++ " boundNames="
                            ++ show (Set.toList boundNames)
                            ++ " allowedNames="
                            ++ show (Set.toList (allowedNames :: Set.Set String))
                            ++ " bindParentsMissing="
                            ++ show
                                [ (name, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParents)
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                ]
                            ++ " missingBasePaths="
                            ++ show
                                [ ( name
                                  , NodeId nid
                                  , mbBase
                                  , mbBasePref
                                  , case mbBase of
                                        Nothing -> []
                                        Just baseN ->
                                            case bindingPathToRootLocal (gaBindParentsBase ga) (TypeRef baseN) of
                                                Right path -> path
                                                Left _ -> []
                                  , case mbBasePref of
                                        Nothing -> []
                                        Just baseN ->
                                            case bindingPathToRootLocal (gaBindParentsBase ga) (TypeRef baseN) of
                                                Right path -> path
                                                Left _ -> []
                                  , case mbBasePref of
                                        Nothing -> Nothing
                                        Just baseN -> firstGenAncestorFrom (gaBindParentsBase ga) (TypeRef baseN)
                                  , case mbBase of
                                        Nothing -> Nothing
                                        Just baseN ->
                                            IntMap.lookup (nodeRefKey (typeRef baseN)) (gaBindParentsBase ga)
                                  )
                                | name <- missing
                                , Just nid <- [parseNameId name]
                                , Just ga <- [mbBindParentsGa]
                                , let mbBase = IntMap.lookup nid (gaSolvedToBase ga)
                                , let mbBasePref = IntMap.lookup nid solvedToBasePrefPlan
                                ]
                        )
                        (Left $ SchemeFreeVars typeRootC missing)
    -- Phase 11: final validation (SchemeFreeVars).
    finalizeScheme missingNames
