{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.Ann (
    computeResultTypeFromAnn,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types.Graph
    ( EdgeId(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cGenNodes
    , cNodes
    , fromListNode
    , getEdgeId
    , getNodeId
    , lookupGen
    , lookupNodeIn
    , gnSchemes
    , toListNode
    )
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Reify.Core
    ( namedNodes
    , reifyTypeWithNamedSetNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    )
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
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
    , resolveCanonicalScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.TypeOps
    ( inlineBoundVarsType
    , inlineBoundVarsTypeForBound
    , simplifyAnnotationType
    )
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.ResultType.Util
    ( generalizeWithPlan
    , containsBoundForall
    , instHasBoundForall
    , instantiateImplicitForalls
    )
import MLF.Elab.Run.ResultType.Types (ResultTypeContext(..))

-- | Compute result type from an annotation edge.
computeResultTypeFromAnn
    :: ResultTypeContext
    -> AnnExpr      -- ^ inner (post-redirect)
    -> AnnExpr      -- ^ innerPre (pre-redirect)
    -> NodeId       -- ^ annNodeId
    -> EdgeId       -- ^ eid
    -> Either ElabError ElabType
computeResultTypeFromAnn ctx inner innerPre annNodeId eid = do
    let canonical = rtcCanonical ctx
        edgeWitnesses = rtcEdgeWitnesses ctx
        edgeTraces = rtcEdgeTraces ctx
        solvedForGen = rtcSolvedForGen ctx
        bindParentsGa = rtcBindParentsGa ctx
        planBuilder = rtcPlanBuilder ctx
        c1 = rtcBaseConstraint ctx
        redirects = rtcRedirects ctx
        traceCfg = rtcTraceConfig ctx
        generalizeAtWith = generalizeAtWithBuilder planBuilder

    let rootPre = annNode inner
        rootC = canonical rootPre
    ew <- case IntMap.lookup (getEdgeId eid) edgeWitnesses of
        Nothing -> Left (ValidationFailed ["missing edge witness for annotation"])
        Just ew' -> Right ew'
    let mTrace = IntMap.lookup (getEdgeId eid) edgeTraces
    let targetC = schemeBodyTarget solvedForGen rootC
        scopeRootNodePre0 = annNode innerPre
        scopeRootNodePre =
            let solvedToBase = gaSolvedToBase bindParentsGa
            in IntMap.findWithDefault scopeRootNodePre0 (getNodeId targetC) solvedToBase
        scopeRootNodePost = annNode inner
    scopeRootPre <- bindingToElab (resolveCanonicalScope c1 solvedForGen redirects scopeRootNodePre)
    let scopeRootPost =
            case bindingScopeRef (srConstraint solvedForGen) scopeRootNodePost of
                Right ref -> canonicalizeScopeRef solvedForGen redirects ref
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
        generalizeWithPlan planBuilder bindParentsGa solvedForGen scopeRoot targetC
    let sch = sch0
        subst = subst0
        srcTy = schemeToType sch
        schemeInfo = SchemeInfo { siScheme = sch, siSubst = subst }
    phi0 <- phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith solvedForGen (Just bindParentsGa) (Just schemeInfo) mTrace ew
    namedSetSolved <- namedNodes solvedForGen
    let annBound = VarStore.lookupVarBound (srConstraint solvedForGen) annNodeId
        annTargetNode0 =
            case annBound of
                Just bnd -> bnd
                Nothing -> annNodeId
        reifyMaybe nid =
            case reifyTypeWithNamedSetNoFallback solvedForGen IntMap.empty namedSetSolved nid of
                Left _ -> Nothing
                Right ty0 -> Just ty0
        targetTyRawFullM =
            reifyMaybe annTargetNode0
        annTargetNode = schemeBodyTarget solvedForGen annTargetNode0
        targetTyRawM =
            reifyMaybe annTargetNode
        targetTyMatchM =
            fmap
                (inlineBoundVarsType solvedForGen)
                targetTyRawM
        baseConstraint = gaBaseConstraint bindParentsGa
        solvedToBase = gaSolvedToBase bindParentsGa
        toBase nid =
            IntMap.findWithDefault nid (getNodeId nid) solvedToBase
        baseNodesVarOnly =
            fromListNode
                [ (nid, node)
                | (nid, node) <- toListNode (cNodes baseConstraint)
                , isTyVar node
                ]
        isTyVar node = case node of
            TyVar{} -> True
            _ -> False
        inlineAllBoundsType =
            -- See Note [Scope-aware bound/alias inlining] in
            -- docs/notes/2026-01-27-elab-changes.md.
            inlineAliasBoundsWithBy
                False
                toBase
                baseNodesVarOnly
                (VarStore.lookupVarBound baseConstraint)
                (reifyTypeWithNamesNoFallbackOnConstraint baseConstraint IntMap.empty)
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
                    in case reifyTypeWithNamesNoFallbackOnConstraint
                            baseConstraint
                            IntMap.empty
                            baseRoot of
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
                    Just (instInsideFromArgsWithBounds binds (map (inlineBoundVarsTypeForBound solvedForGen) args))
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
                        bindingToElab
                            (resolveCanonicalScope (srConstraint solvedForGen) solvedForGen redirects annTargetNode)
                    (annSch, _substAnn) <-
                        generalizeWithPlan planBuilder bindParentsGa solvedForGen annScopeRoot annTargetNode
                    pure (simplifyAnnotationType (schemeToType annSch))
                else if targetHasBoundForall
                    then do
                        ty0 <- applyInstantiation srcTy phi
                        pure (simplifyAnnotationType ty0)
                    else do
                        annScopeRoot <-
                            bindingToElab
                                (resolveCanonicalScope (srConstraint solvedForGen) solvedForGen redirects annTargetNode)
                        (annSch, _substAnn) <-
                            generalizeWithPlan planBuilder bindParentsGa solvedForGen annScopeRoot annTargetNode
                        pure (simplifyAnnotationType (schemeToType annSch))
