{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Elab.Generalize
Description : Apply generalization plans to produce elaborated types
Copyright   : (c) 2024
License     : BSD-3-Clause

This module applies generalization plans (produced by the presolution phase)
to produce elaborated types with explicit polymorphism. It coordinates the
binder naming, scheme reification, and finalization steps.

= Process

1. Receive 'GeneralizePlan' from presolution
2. Build 'BinderPlan' for naming quantified variables
3. Reify the type using 'ReifyPlan'
4. Finalize schemes with proper binders

See 'MLF.Constraint.Presolution.Plan' for the plan generation logic.
-}
module MLF.Elab.Generalize (
    GaBindParents(..),
    applyGeneralizePlan
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MLF.Constraint.Presolution.Plan
    ( GeneralizePlan(..)
    , ReifyPlan(..)
    )
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.IntMapUtils as IntMapUtils
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
    , reifyBoundWithNamesOnConstraint
    )
import MLF.Reify.TypeOps (freeTypeVarsFrom, inlineAliasBoundsWithBy)
import MLF.Elab.Types
import MLF.Util.Graph (reachableFromStop)

-- | Generate a name for a rigid type variable based on its key.
rigidNameFor :: Int -> String
rigidNameFor key = "__rigid" ++ show key

-- | Build a forall type from a list of binders and a body type.
buildForallType :: [(String, Maybe BoundType)] -> ElabType -> ElabType
buildForallType binds body = foldr (\(n, b) t -> TForall n b t) body binds

-- | Inline rigid type variables by substituting them with their bounds.
-- Uses cycle detection to prevent infinite loops when bounds reference each other.
inlineRigidTypes :: Map.Map String ElabType -> ElabType -> ElabType
inlineRigidTypes rigidBounds = go Set.empty
  where
    go seen ty = case ty of
        TVar v ->
            case Map.lookup v rigidBounds of
                Just bound
                    | Set.member v seen -> TVar v
                    | otherwise -> go (Set.insert v seen) bound
                Nothing -> TVar v
        TBase b -> TBase b
        TBottom -> TBottom
        TArrow a b -> TArrow (go seen a) (go seen b)
        TForall v mb body ->
            TForall v (fmap (goBound seen) mb) (go seen body)
    goBound seen = \case
        TArrow a b -> TArrow (go seen a) (go seen b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body -> TForall v (fmap (goBound seen) mb) (go seen body)

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
                Just node@TyVar{ tnBound = Just bnd } | allowBoundTraversal bnd ->
                    structuralChildrenWithBounds node
                Just node -> structuralChildren node
                Nothing -> []
    let uniqueUnboundedName =
            case [ name
                 | (name, nidInt) <- zip binderNames orderedBinders
                 , Nothing <- [lookupCanonicalBound (NodeId nidInt)]
                 ] of
                [nm] -> Just nm
                _ -> Nothing
        lookupCanonicalBound nid = VarStore.lookupVarBound constraint (canonical nid)
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
            | gen <- NodeAccess.allGenNodes constraint
            , root <- gnSchemes gen
            , canonical root == typeRootC
            ]
    -- Thesis §15.2.5: rigid quantification is always inlined (no abstractions for rigid nodes).
    let reifyTypeWithAliases bodyRoot substBase binderPairs =
            if null aliasEntries
                then reifyAndInlineRigid bodyRoot substBaseRigid
                else do
                    let aliasNodes =
                            IntMap.fromList
                                [ (key, TyVar { tnId = NodeId key, tnBound = Nothing })
                                | (key, _) <- aliasEntries
                                ]
                        constraintAlias =
                            constraint { cNodes = NodeMap (IntMap.union aliasNodes nodes) }
                        substAlias =
                            IntMap.union (IntMap.fromList aliasEntries) substBaseRigid
                        resAlias = resForReify { srConstraint = constraintAlias }
                    ty <- reifyWith bodyRoot substAlias constraintAlias resAlias
                    inlineRigid substAlias constraintAlias resAlias ty
          where
            -- Basic setup
            bodyRootC = canonical bodyRoot
            canonicalKey = getNodeId . canonical
            lookupBound nid = VarStore.lookupVarBound constraint (canonical nid)

            -- Determine whether to use constraint-based or result-based reification
            useConstraintReify =
                case IntMap.lookup (getNodeId bodyRootC) nodes of
                    Just TyVar{} | Just bnd <- lookupBound bodyRootC ->
                        getNodeId bnd == getNodeId bodyRoot
                    _ -> False

            -- Reification helpers
            reifyWith substRoot substMap constraintArg resArg
                | useConstraintReify = reifyTypeWithNamesNoFallbackOnConstraint constraintArg substMap substRoot
                | otherwise = reifyTypeWithNamesNoFallback resArg substMap substRoot

            reifyBoundWith substMap constraintArg resArg bndRoot
                | useConstraintReify = reifyBoundWithNamesOnConstraint constraintArg substMap bndRoot
                | otherwise = reifyBoundWithNames resArg substMap bndRoot

            -- Rigid type handling
            isReachableRigidVar nid =
                case IntMap.lookup (canonicalKey nid) nodes of
                    Just TyVar{} ->
                        let cidKey = canonicalKey nid
                        in IntSet.member cidKey (reachableFromWithBounds bodyRoot)
                            && cidKey `IntSet.notMember` binderSet
                    _ -> False

            rigidNodeKeys =
                IntSet.toList $ IntSet.fromList
                    [ canonicalKey nid
                    | nid <- IntMapUtils.rigidTypeChildren bindParents
                    , isReachableRigidVar nid
                    ]

            rigidSubstMap =
                IntMap.fromList
                    [ (key, rigidNameFor key)
                    | key <- rigidNodeKeys
                    ]

            -- Alias handling
            reachableWithoutBound bnd =
                let shouldStop nid = getNodeId nid == getNodeId (canonical bnd)
                in reachableFromStop
                    getNodeId
                    canonical
                    childrenWithBounds
                    shouldStop
                    bodyRoot

            aliasEntries =
                [ (getNodeId (canonical bnd), name)
                | (b, name) <- binderPairs
                , Just bnd <- [lookupBound b]
                , canonical bnd /= bodyRootC
                , canonicalKey b `IntSet.notMember` reachableWithoutBound bnd
                ]

            substBaseRigid = IntMap.union rigidSubstMap substBase

            -- Main reification logic
            reifyAndInlineRigid root substMap = do
                ty <- reifyWith root substMap constraint resForReify
                inlineRigid substMap constraint resForReify ty

            inlineRigid substMap constraintArg resArg ty
                | null rigidNodeKeys = pure ty
                | otherwise = do
                    let computeRigidBound key = do
                            let nid = NodeId key
                                name = rigidNameFor key
                            case lookupBound nid of
                                Nothing -> pure (name, TBottom)
                                Just bnd -> do
                                    bndTy <- reifyBoundWith substMap constraintArg resArg (canonical bnd)
                                    pure (name, bndTy)
                    rigidBounds <- mapM computeRigidBound rigidNodeKeys
                    let rigidMap = Map.fromList rigidBounds
                    pure (inlineRigidTypes rigidMap ty)

    let orderedBinderPairs = zip (map NodeId orderedBinders) binderNames
        reifyTypeWithOrderedBinders =
            reifyTypeWithAliases
                typeRootForReifyAdjusted
                substForReifyAdjusted
                orderedBinderPairs

    let reifySchemeType
            | useSchemeTypeAdjusted = reifySchemeTypeAdjusted
            | otherwise = reifySchemeTypeExplicit
          where
            -- Adjusted scheme type: use scheme ownership to determine scope
            reifySchemeTypeAdjusted = do
                let reachableVars =
                        [ NodeId nid
                        | nid <- IntSet.toList reachableType
                        , nid /= getNodeId typeRootC
                        , Just TyVar{} <- [IntMap.lookup nid nodes]
                        ]
                    hasReachableBinder gid =
                        any (\nid -> firstGenAncestorGa (typeRef nid) == Just gid) reachableVars
                    schemeOwnerCandidates = filter hasReachableBinder schemeOwners
                    schemeScope = case schemeOwnerFromBody of
                        Just _ | schemeOwnerFromBodyIsAlias, (owner:_) <- ownersByRoot -> genRef owner
                        Just gid -> genRef gid
                        Nothing -> case schemeOwnerCandidates ++ schemeOwners of
                            (gid:_) -> genRef gid
                            [] -> typeRef typeRootC
                if schemeScope == scopeRootC
                    then do
                        traceGeneralizeM env
                            ("generalizeAt: schemeScope equals scopeRootC; skipping recursive scheme-type fallback"
                                ++ " scopeRootC=" ++ show scopeRootC
                                ++ " typeRootC=" ++ show typeRootC
                            )
                        reifyTypeWithOrderedBinders
                    else do
                        sch <- generalizeAtForScheme schemeScope typeRootC
                        pure $ case sch of
                            Forall binds body -> buildForallType binds body

            -- Explicit scheme type: use structural scheme if available
            reifySchemeTypeExplicit = do
                explicitSchemeTy <- explicitStructuralSchemeType
                case explicitSchemeTy of
                    Just ty -> pure ty
                    Nothing -> fallbackSchemeType

            explicitStructuralSchemeType
                | null bindings, scopeHasStructuralScheme, explicitBinders0@(_:_) <- binders0 =
                    case explicitSchemePlan explicitBinders0 of
                        Nothing -> pure Nothing
                        Just (binders, names, substExplicit, explicitBodyRoot) -> do
                            bodyTy <- reifyTypeWithAliases explicitBodyRoot substExplicit (zip binders names)
                            bounds <- explicitBounds binders names substExplicit
                            pure (Just (buildForallType bounds bodyTy))
                | otherwise = pure Nothing

            explicitSchemePlan explicitBinders0 =
                let binderKeysList = IntSet.toList $ IntSet.fromList
                        [ getNodeId (canonical b) | b <- explicitBinders0 ]
                    names = zipWith alphaName [0..] binderKeysList
                in case binderKeysList of
                    [] -> Nothing
                    _ -> Just
                        ( map NodeId binderKeysList
                        , names
                        , IntMap.fromList (zip binderKeysList names)
                        , case IntMap.lookup (getNodeId typeRootC) nodes of
                            Just TyVar{} | Just bnd <- lookupCanonicalBound typeRootC ->
                                canonical bnd
                            _ -> typeRootForReifyAdjusted
                        )

            explicitBounds binders names substExplicit =
                let lookupBound nid = VarStore.lookupVarBound constraint (canonical nid)
                    inlineNamedBounds = inlineNamedBoundsFor substExplicit
                    computeBound (b, name) =
                        case lookupBound b of
                            Nothing -> pure (name, Nothing)
                            Just bnd -> do
                                bndTy <- reifyBoundWithNames resForReify substExplicit (canonical bnd)
                                let bndTy' = inlineNamedBounds bndTy
                                    mbBound = case bndTy' of
                                        TBottom -> Nothing
                                        TVar v | v == name -> Nothing
                                        TVar{} -> Nothing
                                        _ -> either (const Nothing) Just (elabToBound bndTy')
                                pure (name, mbBound)
                in mapM computeBound (zip binders names)

            inlineNamedBoundsFor substExplicit =
                -- See Note [Scope-aware bound/alias inlining] in
                -- docs/notes/2026-01-27-elab-changes.md.
                inlineAliasBoundsWithBy
                    False
                    canonical
                    (NodeMap nodes)
                    (VarStore.lookupVarBound constraint)
                    (reifyBoundWithNames resForReify substExplicit)

            fallbackSchemeType
                | scopeHasStructuralScheme && null bindings =
                    reifyTypeWithNamesNoFallbackOnConstraint
                        constraint
                        substForReifyAdjusted
                        typeRootForReifyAdjusted
                | Just ga <- mbBindParentsGa = reifyWithGaBase ga
                | otherwise = reifyTypeWithOrderedBinders

            reifyWithGaBase ga =
                case IntMap.lookup (getNodeId (canonical typeRoot)) solvedToBasePrefPlan of
                    Just baseN | canonical baseN /= canonical typeRoot -> do
                        tyBase <-
                            reifyTypeWithNamesNoFallbackOnConstraint
                                (gaBaseConstraint ga)
                                substBaseByKey
                                baseN
                        let freeBase = freeTypeVarsFrom Set.empty tyBase
                            allowedBase = Set.fromList (IntMap.elems substBaseByKey)
                        if Set.isSubsetOf freeBase allowedBase
                            then pure tyBase
                            else reifyTypeWithOrderedBinders
                    _ -> reifyTypeWithOrderedBinders
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
