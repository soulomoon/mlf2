{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.Ann (
    computeResultTypeFromAnnWithView,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Types.Graph
    ( EdgeId(..)
    , NodeId(..)
    , NodeRef(..)
    , cGenNodes
    , cNodes
    , getEdgeId
    , getNodeId
    , lookupGen
    , lookupNodeIn
    , gnSchemes
    )
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTraceReadModel)
import MLF.Elab.ReadModel (ermNamedNodes)
import MLF.Elab.Types
import MLF.Reify.TypeOps
    ( inlineAliasBoundsWithBy
    , inlineBaseBoundsType
    , resolveBoundBodyConstraint
    )
import MLF.Elab.Run.Annotation (adjustAnnotationInst, annNode)
import MLF.Elab.Run.Debug (debugWhenCondM, debugWhenM)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme, instInsideFromArgsWithBounds)
import MLF.Elab.Run.Scope
    ( bindingScopeRef
    , canonicalizeScopeRef
    )
import MLF.Elab.Run.TypeOps
    ( inlineBoundVarsType
    , inlineBoundVarsTypeForBound
    , simplifyAnnotationType
    )
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.ResultType.Util
    ( containsBoundForall
    , instHasBoundForall
    , instantiateImplicitForalls
    )
import MLF.Elab.Run.ResultType.Types
    ( ResultTypeInputs(..)
    , rtcEdgeTraces
    , rtcEdgeWitnesses
    )
import qualified MLF.Elab.Run.ResultType.View as View

computeResultTypeFromAnnWithView
    :: ResultTypeInputs p
    -> View.ResultTypeView p
    -> AnnExpr
    -> AnnExpr
    -> NodeId
    -> EdgeId
    -> Either ElabError ElabType
computeResultTypeFromAnnWithView ctx view inner innerPre annNodeId eid = do
    let presolutionViewForGen = View.rtvPresolutionViewOverlay view
        canonical = rtcCanonical ctx
        edgeWitnesses = rtcEdgeWitnesses ctx
        edgeTraces = rtcEdgeTraces ctx
        bindParentsGa = rtcBindParentsGa ctx
        planBuilder = rtcPlanBuilder ctx
        c1 = rtcBaseConstraint ctx
        redirects = rtcRedirects ctx
        traceCfg = rtcTraceConfig ctx
        generalizeAtWith mbGa =
            generalizeAtWithBuilder planBuilder mbGa presolutionViewForGen

    let rootPre = annNode inner
        rootC = canonical rootPre
    ew <- case IntMap.lookup (getEdgeId eid) edgeWitnesses of
        Nothing -> Left (ValidationFailed ["missing edge witness for annotation"])
        Just ew' -> Right ew'
    let mTrace = IntMap.lookup (getEdgeId eid) edgeTraces
    let targetC = View.rtvSchemeBodyTarget view rootC
        scopeRootNodePre0 = annNode innerPre
        scopeRootNodePre =
            let solvedToBase = gaSolvedToBase bindParentsGa
            in IntMap.findWithDefault scopeRootNodePre0 (getNodeId targetC) solvedToBase
        scopeRootNodePost = annNode inner
    scopeRootPre <- View.rtvResolveCanonicalScope view scopeRootNodePre
    let scopeRootPost =
            case bindingScopeRef c1 scopeRootNodePost of
                Right ref -> canonicalizeScopeRef presolutionViewForGen redirects ref
                Left _ -> scopeRootPre
        scopeRoot = scopeRootPre
    debugWhenCondM traceCfg (scopeRootPre /= scopeRootPost)
        ("runPipelineElab: ga' mismatch (AAnn) pre="
            ++ show scopeRootPre
            ++ " post="
            ++ show scopeRootPost
            ++ " preNode="
            ++ show scopeRootNodePre
            ++ " postNode="
            ++ show scopeRootNodePost
        )
    (sch0, subst0) <-
        View.rtvGeneralizeTarget view scopeRoot targetC
    let sch = sch0
        subst = subst0
        srcTy = schemeToType sch
        schemeInfo = SchemeInfo { siScheme = sch, siSubst = subst }
        generalizeTargetCached scopeRoot' target'
            | scopeRoot' == scopeRoot && target' == targetC = Right (sch0, subst0)
            | otherwise = View.rtvGeneralizeTarget view scopeRoot' target'
    readModel <- View.rtvReadModel view
    phi0 <- phiFromEdgeWitnessWithTraceReadModel traceCfg generalizeAtWith readModel (Just bindParentsGa) (Just schemeInfo) mTrace ew
    let namedSetSolved = ermNamedNodes readModel
    let annBound = View.rtvLookupVarBound view annNodeId
        annTargetNode0 =
            case annBound of
                Just bnd -> bnd
                Nothing -> annNodeId
        reifyMaybe nid =
            case View.rtvReifyWithNamedSetNoFallback view IntMap.empty namedSetSolved nid of
                Left _ -> Nothing
                Right ty0 -> Just ty0
        targetTyRawFullM =
            reifyMaybe annTargetNode0
        annTargetNode = View.rtvSchemeBodyTarget view annTargetNode0
        targetTyRawM =
            if annTargetNode == annTargetNode0
                then targetTyRawFullM
                else reifyMaybe annTargetNode
        targetTyMatchM =
            fmap
                (inlineBoundVarsType presolutionViewForGen)
                targetTyRawM
        baseConstraint = gaBaseConstraint bindParentsGa
        solvedToBase = gaSolvedToBase bindParentsGa
        toBase nid =
            IntMap.findWithDefault nid (getNodeId nid) solvedToBase
        baseNodesVarOnly = View.rtvBaseVarOnlyNodes view
        inlineAllBoundsType =
            -- See Note [Scope-aware bound/alias inlining] in
            -- docs/notes/2026-01-27-elab-changes.md.
            inlineAliasBoundsWithBy
                False
                toBase
                baseNodesVarOnly
                (VarStore.lookupVarBound baseConstraint)
                (View.rtvReifyBaseWithNamesNoFallback view IntMap.empty)
        targetTyBaseInlineM =
            fmap inlineAllBoundsType targetTyRawM
        annTargetBase =
            IntMap.findWithDefault annTargetNode (getNodeId annTargetNode) solvedToBase
        annGenMaybe =
            case bindingScopeRef c1 annNodeId of
                Right (GenRef gid) -> Just gid
                _ -> Nothing
        annotationExplicit =
            case annGenMaybe of
                Nothing -> False
                Just annGen ->
                    case lookupGen annGen (cGenNodes c1) of
                        Just gen -> not (null (gnSchemes gen))
                        Nothing -> False
        normalizeTarget ty =
            if annotationExplicit
                then ty
                else instantiateImplicitForalls ty
        targetTyMatchNormM =
            fmap normalizeTarget targetTyMatchM
        targetTyBaseM =
            let baseCandidate =
                    case IntMap.lookup (getNodeId annTargetNode) solvedToBase of
                        Just baseN -> Just baseN
                        Nothing ->
                            case lookupNodeIn (cNodes baseConstraint) annTargetNode of
                                Just _ -> Just annTargetNode
                                Nothing -> Nothing
            in case baseCandidate of
                Just baseN ->
                    let baseRoot =
                            resolveBoundBodyConstraint id baseConstraint IntSet.empty baseN
                    in case View.rtvReifyBaseWithNamesNoFallback view IntMap.empty baseRoot of
                        Right ty0 ->
                            let ty1 = inlineBaseBoundsType baseConstraint id ty0
                            in Just (inlineAllBoundsType ty1)
                        Left _ -> Nothing
                Nothing -> Nothing
        targetTyForMatchM =
            case targetTyMatchNormM of
                Just ty -> Just ty
                Nothing ->
                    case targetTyBaseM of
                        Just ty -> Just (normalizeTarget ty)
                        Nothing ->
                            case targetTyBaseInlineM of
                                Just ty -> Just (normalizeTarget ty)
                                Nothing -> targetTyMatchNormM
    debugWhenM traceCfg
        ("runPipelineElab: ann explicit="
            ++ show annotationExplicit
            ++ " annTargetBase="
            ++ show annTargetBase
            ++ " annGen="
            ++ show annGenMaybe
        )
    -- See Note [Skip target-derived instantiation when bounds include foralls]
    -- in docs/notes/2026-01-27-elab-changes.md.
    let phiHasBoundForall = instHasBoundForall phi0
        targetHasBoundForall =
            let hasForallInBounds ty = containsBoundForall ty
                matchHasForall =
                    case targetTyForMatchM of
                        Just ty -> hasForallInBounds ty
                        Nothing -> False
                rawHasForall =
                    if annotationExplicit
                        then maybe False hasForallInBounds targetTyRawFullM
                        else False
            in matchHasForall || rawHasForall || (annotationExplicit && phiHasBoundForall)
    let phiFromTargetArgs =
            case (sch, targetTyForMatchM) of
                (Forall binds body, Just targetTy)
                    | not targetHasBoundForall ->
                        inferInstAppArgsFromScheme binds body targetTy
                _ -> Nothing
        phiFromTarget =
            case (sch, phiFromTargetArgs) of
                (Forall binds _, Just args) ->
                    instInsideFromArgsWithBounds binds (map (inlineBoundVarsTypeForBound presolutionViewForGen) args)
                _ -> Nothing
        phi =
            if annotationExplicit
                then
                    case phiFromTarget of
                        Just inst -> inst
                        Nothing ->
                            if targetHasBoundForall
                                then phi0
                                else InstId
                else
                    case phiFromTarget of
                        Just inst -> inst
                        Nothing ->
                            if targetHasBoundForall
                                then phi0
                                else adjustAnnotationInst phi0
    debugWhenM traceCfg
        ("runPipelineElab: ann targetTy="
            ++ show targetTyMatchM
            ++ " targetTyRaw="
            ++ show targetTyRawM
            ++ " targetHasBoundForall="
            ++ show targetHasBoundForall
            ++ " phiFromTarget="
            ++ show phiFromTargetArgs
        )
    debugWhenM traceCfg
        ("runPipelineElab: ann srcTy="
            ++ pretty srcTy
            ++ " phi0="
            ++ pretty phi0
            ++ " phi="
            ++ pretty phi
        )
    case phiFromTarget of
        Just _ -> do
            ty0 <- applyInstantiation srcTy phi
            pure (simplifyAnnotationType ty0)
        Nothing ->
            if annotationExplicit
                then do
                    -- For explicit annotations, report the (generalized) target type itself.
                    -- This preserves explicit bounds (including nested foralls) rather than
                    -- re-deriving a type solely from the witness-derived instantiation.
                    annScopeRoot <-
                        View.rtvResolveCanonicalScope view annTargetNode
                    (annSch, _substAnn) <-
                        generalizeTargetCached annScopeRoot annTargetNode
                    pure (simplifyAnnotationType (schemeToType annSch))
                else if targetHasBoundForall
                    then do
                        ty0 <- applyInstantiation srcTy phi
                        pure (simplifyAnnotationType ty0)
                    else do
                        annScopeRoot <-
                            View.rtvResolveCanonicalScope view annTargetNode
                        (annSch, _substAnn) <-
                            generalizeTargetCached annScopeRoot annTargetNode
                        pure (simplifyAnnotationType (schemeToType annSch))
