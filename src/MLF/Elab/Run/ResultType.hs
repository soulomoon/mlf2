{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType (
    ResultTypeContext(..),
    computeResultTypeFromAnn,
    computeResultTypeFallback
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionPlanBuilder(..)
    )
import MLF.Constraint.Solve (SolveResult(..), frWith)
import MLF.Constraint.Types
    ( Constraint
    , EdgeId(..)
    , EdgeWitness(..)
    , Expansion(..)
    , GenNode(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cGenNodes
    , cLetEdges
    , cNodes
    , ewRight
    , genNodeKey
    , getEdgeId
    , getNodeId
    , gnId
    , gnSchemes
    , nodeRefFromKey
    )
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
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
    , resolveBaseBoundForInstConstraint
    , resolveBoundBodyConstraint
    )
import MLF.Elab.Run.Annotation (adjustAnnotationInst, annNode)
import MLF.Elab.Run.Debug (debugGaScope, debugGaScopeEnabled, debugWhenCondM, debugWhenM)
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme, instInsideFromArgsWithBounds)
import MLF.Elab.Run.Scope
    ( bindingScopeRef
    , canonicalizeScopeRef
    , preferGenScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.TypeOps
    ( inlineBoundVarsType
    , inlineBoundVarsTypeForBound
    , simplifyAnnotationType
    )

-- | Context for result type computation, bundling shared state.
data ResultTypeContext = ResultTypeContext
    { rtcCanonical :: NodeId -> NodeId
    , rtcEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , rtcEdgeTraces :: IntMap.IntMap EdgeTrace
    , rtcEdgeExpansions :: IntMap.IntMap Expansion
    , rtcSolvedForGen :: SolveResult
    , rtcSolvedClean :: SolveResult
    , rtcBindParentsGa :: GaBindParents
    , rtcPlanBuilder :: PresolutionPlanBuilder
    , rtcBaseConstraint :: Constraint
    , rtcRedirects :: IntMap.IntMap NodeId
    }

-- | Generalize with plan helper
generalizeWithPlan
    :: PresolutionPlanBuilder
    -> GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeWithPlan planBuilder bindParentsGa res scopeRoot targetNode =
    generalizeAtWithBuilder
        planBuilder
        (Just bindParentsGa)
        res
        scopeRoot
        targetNode

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right

-- | Check if a type contains foralls in bounds
containsBoundForall :: ElabType -> Bool
containsBoundForall ty =
    let go t = case t of
            TForall _ mb body ->
                maybe False containsAnyForallBound mb || go body
            TArrow a b -> go a || go b
            _ -> False
        containsAnyForallBound bound = case bound of
            TArrow a b -> go a || go b
            TForall _ _ _ -> True
            _ -> False
    in go ty

-- | Check if an instantiation contains foralls in bounds
instHasBoundForall :: Instantiation -> Bool
instHasBoundForall inst = cata instAlg inst
  where
    instAlg inst0 = case inst0 of
        InstIdF -> False
        InstSeqF a b -> a || b
        InstAppF ty -> containsForallTy ty
        InstBotF ty -> containsForallTy ty
        InstInsideF innerInst -> innerInst
        InstUnderF _ innerInst -> innerInst
        InstIntroF -> False
        InstElimF -> False
        InstAbstrF _ -> False

-- | Instantiate implicit foralls (foralls with bounds)
instantiateImplicitForalls :: ElabType -> ElabType
instantiateImplicitForalls ty0 =
    let go ty = case ty of
            TForall _ (Just _) _ ->
                case applyInstantiation ty InstElim of
                    Right ty' -> go ty'
                    Left _ -> ty
            TForall v mb body ->
                TForall v (fmap goBound mb) (go body)
            TArrow a b -> TArrow (go a) (go b)
            TBase _ -> ty
            TBottom -> ty
            TVar _ -> ty
        goBound bound = case bound of
            TArrow a b -> TArrow (go a) (go b)
            TBase b -> TBase b
            TBottom -> TBottom
            TForall v mb body ->
                TForall v (fmap goBound mb) (go body)
    in go ty0

-- | Compute result type from an annotation edge.
computeResultTypeFromAnn
    :: ResultTypeContext
    -> AnnExpr      -- ^ inner (post-redirect)
    -> AnnExpr      -- ^ innerPre (pre-redirect)
    -> NodeId       -- ^ annNodeId
    -> EdgeId       -- ^ eid
    -> Either String ElabType
computeResultTypeFromAnn ctx inner innerPre annNodeId eid = do
    let canonical = rtcCanonical ctx
        edgeWitnesses = rtcEdgeWitnesses ctx
        edgeTraces = rtcEdgeTraces ctx
        solvedForGen = rtcSolvedForGen ctx
        bindParentsGa = rtcBindParentsGa ctx
        planBuilder = rtcPlanBuilder ctx
        c1 = rtcBaseConstraint ctx
        redirects = rtcRedirects ctx
        generalizeAtWith = generalizeAtWithBuilder planBuilder

    let rootPre = annNode inner
        rootC = canonical rootPre
    ew <- case IntMap.lookup (getEdgeId eid) edgeWitnesses of
        Nothing -> Left "missing edge witness for annotation"
        Just ew' -> Right ew'
    let mTrace = IntMap.lookup (getEdgeId eid) edgeTraces
    let targetC = schemeBodyTarget solvedForGen rootC
        scopeRootNodePre0 = annNode innerPre
        scopeRootNodePre =
            let solvedToBase = gaSolvedToBase bindParentsGa
            in IntMap.findWithDefault scopeRootNodePre0 (getNodeId targetC) solvedToBase
        scopeRootNodePost = annNode inner
    scopeRoot0 <- firstShow (bindingScopeRef c1 scopeRootNodePre)
    let scopeRootBase = preferGenScope c1 scopeRoot0
    let scopeRootPre = canonicalizeScopeRef solvedForGen redirects scopeRootBase
    let scopeRootPost =
            case bindingScopeRef (srConstraint solvedForGen) scopeRootNodePost of
                Right ref -> canonicalizeScopeRef solvedForGen redirects ref
                Left _ -> scopeRootPre
        scopeRoot = scopeRootPre
    debugWhenCondM (scopeRootPre /= scopeRootPost)
        ("runPipelineElab: ga' mismatch (AAnn) pre="
            ++ show scopeRootPre
            ++ " post="
            ++ show scopeRootPost
            ++ " preNode="
            ++ show scopeRootNodePre
            ++ " postNode="
            ++ show scopeRootNodePost
        )
    (sch0, subst0) <- firstShow
        (generalizeWithPlan planBuilder bindParentsGa solvedForGen scopeRoot targetC)
    let sch = sch0
        subst = subst0
        srcTy = schemeToType sch
        schemeInfo = SchemeInfo { siScheme = sch, siSubst = subst }
    phi0 <- firstShow (phiFromEdgeWitnessWithTrace generalizeAtWith solvedForGen (Just bindParentsGa) (Just schemeInfo) mTrace ew)
    namedSetSolved <- firstShow (namedNodes solvedForGen)
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
        baseNodesVarOnly = IntMap.filter isTyVar (cNodes baseConstraint)
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
                    case IntMap.lookup (genNodeKey annGen) (cGenNodes c1) of
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
                            if IntMap.member (getNodeId annTargetNode) (cNodes baseConstraint)
                                then Just annTargetNode
                                else Nothing
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
    debugWhenM
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
    debugWhenM
        ("runPipelineElab: ann targetTy="
            ++ show targetTyMatchM
            ++ " targetTyRaw="
            ++ show targetTyRawM
            ++ " targetHasBoundForall="
            ++ show targetHasBoundForall
            ++ " phiFromTarget="
            ++ show phiFromTargetArgs
        )
    debugWhenM
        ("runPipelineElab: ann srcTy="
            ++ pretty srcTy
            ++ " phi0="
            ++ pretty phi0
            ++ " phi="
            ++ pretty phi
        )
    case phiFromTarget of
        Just _ -> do
            ty0 <- firstShow (applyInstantiation srcTy phi)
            pure (simplifyAnnotationType ty0)
        Nothing ->
            if targetHasBoundForall
                then do
                    ty0 <- firstShow (applyInstantiation srcTy phi)
                    pure (simplifyAnnotationType ty0)
                else do
                    annScopeRoot0 <- firstShow (bindingScopeRef (srConstraint solvedForGen) annTargetNode)
                    let annScopeRootBase = preferGenScope (srConstraint solvedForGen) annScopeRoot0
                        annScopeRoot = canonicalizeScopeRef solvedForGen redirects annScopeRootBase
                    (annSch, _substAnn) <- firstShow
                        (generalizeWithPlan planBuilder bindParentsGa solvedForGen annScopeRoot annTargetNode)
                    let annTy = schemeToType annSch
                    pure (simplifyAnnotationType annTy)

-- | Strip annotations from an AnnExpr
stripAnn :: AnnExpr -> AnnExpr
stripAnn ann0 = case ann0 of
    AAnn inner _ _ -> stripAnn inner
    _ -> ann0

-- | Collect all edge IDs from an AnnExpr
collectEdges :: AnnExpr -> [EdgeId]
collectEdges ann0 = case ann0 of
    AVar _ _ -> []
    ALit _ _ -> []
    ALam _ _ _ body _ -> collectEdges body
    AApp f a funEid argEid _ ->
        funEid : argEid : collectEdges f ++ collectEdges a
    ALet _ _ _ _ _ rhs body _ ->
        collectEdges rhs ++ collectEdges body
    AAnn inner _ eid -> eid : collectEdges inner

-- | Compute result type when there's no direct annotation (fallback path).
computeResultTypeFallback
    :: ResultTypeContext
    -> AnnExpr      -- ^ annCanon (post-redirect)
    -> AnnExpr      -- ^ ann (pre-redirect)
    -> Either String ElabType
computeResultTypeFallback ctx annCanon ann = do
    let canonical = rtcCanonical ctx
        edgeWitnesses = rtcEdgeWitnesses ctx
        edgeTraces = rtcEdgeTraces ctx
        edgeExpansions = rtcEdgeExpansions ctx
        solvedForGen = rtcSolvedForGen ctx
        solvedClean = rtcSolvedClean ctx
        solved = solvedClean
        bindParentsGa = rtcBindParentsGa ctx
        planBuilder = rtcPlanBuilder ctx
        c1 = rtcBaseConstraint ctx
        redirects = rtcRedirects ctx
        generalizeAtWith = generalizeAtWithBuilder planBuilder

    let edgeTraceCounts =
            IntMap.fromListWith
                (+)
                [ (getNodeId (etRoot tr), 1 :: Int)
                | tr <- IntMap.elems edgeTraces
                ]
    let schemeRootSet =
            IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- NodeAccess.allGenNodes (srConstraint solvedForGen)
                , root <- gnSchemes gen
                ]
        isSchemeRoot nid =
            IntSet.member (getNodeId (canonical nid)) schemeRootSet
        letEdges = cLetEdges c1
        isLetEdge (EdgeId eid) = IntSet.member eid letEdges
    let rootForTypeAnn =
            let peel ann0 = case ann0 of
                    ALet _ _ _ _ _ _ bodyAnn nid ->
                        case bodyAnn of
                            AAnn inner target eid
                                | canonical target == canonical nid
                                    && isLetEdge eid ->
                                        peel inner
                                | canonical target == canonical nid
                                    && not (isSchemeRoot target) ->
                                        peel inner
                            _ -> bodyAnn
                    _ -> ann0
            in peel annCanon
        rootForType = annNode rootForTypeAnn
        rootForTypePreAnn =
            let peel ann0 = case ann0 of
                    ALet _ _ _ _ _ _ bodyAnn nid ->
                        case bodyAnn of
                            AAnn inner target eid
                                | target == nid
                                    && isLetEdge eid ->
                                        peel inner
                                | target == nid
                                    && not (isSchemeRoot target) ->
                                        peel inner
                            _ -> bodyAnn
                    _ -> ann0
            in peel ann
        rootForTypePre = annNode rootForTypePreAnn
    case rootForTypeAnn of
        AAnn inner annNodeId eid -> do
            let innerPre =
                    case rootForTypePreAnn of
                        AAnn innerPre0 _ _ -> innerPre0
                        _ -> rootForTypePreAnn
            computeResultTypeFromAnn ctx inner innerPre annNodeId eid
        _ -> do
            let rootC = canonical rootForType
                constraint = srConstraint solved
                nodes = cNodes constraint
                rootInstRoots =
                    [ etRoot tr
                    | EdgeId eid <- collectEdges rootForTypeAnn
                    , Just tr <- [IntMap.lookup eid edgeTraces]
                    ]
                rootHasMultiInst =
                    any
                        (\root -> IntMap.findWithDefault 0 (getNodeId root) edgeTraceCounts > 1)
                        rootInstRoots
                collectInstApps = cata alg
                  where
                    alg inst0 = case inst0 of
                        InstAppF ty -> [ty]
                        InstSeqF a b -> a ++ b
                        InstInsideF phi -> phi
                        InstUnderF _ phi -> phi
                        _ -> []
                baseNodeForTy ty =
                    case ty of
                        TBase base ->
                            listToMaybe
                                [ tnId node
                                | node@TyBase{} <- IntMap.elems nodes
                                , tnBase node == base
                                ]
                        _ -> Nothing
                instAppBasesFromWitness funEid =
                    case IntMap.lookup (getEdgeId funEid) edgeWitnesses of
                        Just ew ->
                            case phiFromEdgeWitnessWithTrace generalizeAtWith solved (Just bindParentsGa) Nothing (IntMap.lookup (getEdgeId funEid) edgeTraces) ew of
                                Right inst ->
                                    IntSet.fromList
                                        [ getNodeId nid
                                        | ty <- collectInstApps inst
                                        , Just nid <- [baseNodeForTy ty]
                                        ]
                                Left _ -> IntSet.empty
                        Nothing -> IntSet.empty
                argBounds arg = do
                    baseC <- resolveBaseBoundForInstConstraint constraint canonical arg
                    case IntMap.lookup (getNodeId baseC) nodes of
                        Just TyBase{} -> Just baseC
                        Just TyBottom{} -> Just baseC
                        _ -> Nothing
                instArgNode tr binder arg =
                    case IntMap.lookup (getNodeId binder) (etCopyMap tr) of
                        Just copyN -> copyN
                        Nothing -> arg
                edgeBaseBounds =
                    IntSet.fromList
                        [ getNodeId baseC
                        | ew <- IntMap.elems edgeWitnesses
                        , Just baseC <- [argBounds (ewRight ew)]
                        ]
                rootArgBaseBounds =
                    let argEdgeBases eid =
                            case IntMap.lookup (getEdgeId eid) edgeTraces of
                                Nothing -> IntSet.empty
                                Just tr ->
                                    IntSet.fromList
                                        [ getNodeId baseC
                                        | (binder, arg) <- etBinderArgs tr
                                        , let argNode =
                                                instArgNode tr binder arg
                                        , Just baseC <- [argBounds argNode]
                                        ]
                    in case stripAnn rootForTypeAnn of
                        AApp _ arg funEid argEid _ ->
                            let fromWitness = instAppBasesFromWitness funEid
                                fromArgEdge = argEdgeBases argEid
                            in if not (IntSet.null fromWitness)
                                then fromWitness
                                else if not (IntSet.null fromArgEdge)
                                    then fromArgEdge
                                    else
                                        case resolveBaseBoundForInstConstraint constraint canonical (annNode arg) of
                                            Just baseC -> IntSet.singleton (getNodeId baseC)
                                            Nothing -> IntSet.empty
                        _ -> IntSet.empty
                instArgBaseBounds =
                    let binderBounds =
                            IntMap.fromListWith IntSet.union
                                [ (getNodeId (canonical binderRoot), IntSet.singleton (getNodeId baseC))
                                | tr <- IntMap.elems edgeTraces
                                , let copyMapInv =
                                        IntMap.fromListWith
                                            min
                                            [ (getNodeId copyN, getNodeId origN)
                                            | (origKey, copyN) <- IntMap.toList (etCopyMap tr)
                                            , let origN = NodeId origKey
                                            ]
                                , (binder, arg) <- etBinderArgs tr
                                , let binderRoot =
                                        case IntMap.lookup (getNodeId binder) copyMapInv of
                                            Just origKey -> NodeId origKey
                                            Nothing -> binder
                                , let argNode = instArgNode tr binder arg
                                , Just baseC <- [argBounds argNode]
                                ]
                    in IntSet.union
                        (IntSet.unions
                        [ bounds
                        | bounds <- IntMap.elems binderBounds
                        , IntSet.size bounds > 1
                        ])
                        edgeBaseBounds
                (rootBounds, instArgRootMultiBase) =
                    let rootBounds' =
                            IntMap.fromListWith IntSet.union
                                [ (getNodeId (canonical (etRoot tr)), IntSet.singleton (getNodeId baseC))
                                | tr <- IntMap.elems edgeTraces
                                , (binder, arg) <- etBinderArgs tr
                                , let argNode = instArgNode tr binder arg
                                , Just baseC <- [argBounds argNode]
                                ]
                        multiBase =
                            any (\s -> IntSet.size s > 1) (IntMap.elems rootBounds')
                                || IntSet.size edgeBaseBounds > 1
                    in (rootBounds', multiBase)

            let rootBaseBounds =
                    IntMap.findWithDefault IntSet.empty (getNodeId rootC) rootBounds
                rootBoundCandidates =
                    if IntSet.null rootBaseBounds
                        then rootArgBaseBounds
                        else rootBaseBounds
            let baseTarget =
                    case IntMap.lookup (getNodeId rootC) nodes of
                        Just TyVar{} ->
                            case resolveBaseBoundForInstConstraint constraint canonical rootC of
                                Just baseC
                                    | IntSet.size rootBoundCandidates == 1
                                        && IntSet.member (getNodeId baseC) rootBoundCandidates
                                        && not rootHasMultiInst
                                        && not instArgRootMultiBase ->
                                        Just baseC
                                    | IntSet.null rootBoundCandidates
                                        && IntSet.null instArgBaseBounds
                                        && not rootHasMultiInst
                                        && not instArgRootMultiBase ->
                                        Just baseC
                                    | IntSet.null rootBaseBounds
                                        && not instArgRootMultiBase
                                        && not rootHasMultiInst
                                        && IntSet.size instArgBaseBounds == 1
                                        && IntSet.member (getNodeId baseC) instArgBaseBounds ->
                                        Just baseC
                                    | otherwise ->
                                        case IntSet.toList rootBoundCandidates of
                                            [baseKey]
                                                | not rootHasMultiInst
                                                    && not instArgRootMultiBase ->
                                                    case IntMap.lookup baseKey nodes of
                                                        Just TyBase{} -> Just (NodeId baseKey)
                                                        Just TyBottom{} -> Just (NodeId baseKey)
                                                        _ -> Nothing
                                            _ -> Nothing
                                _ ->
                                    case IntSet.toList rootBoundCandidates of
                                        [baseKey]
                                            | not rootHasMultiInst
                                                && not instArgRootMultiBase ->
                                                case IntMap.lookup baseKey nodes of
                                                    Just TyBase{} -> Just (NodeId baseKey)
                                                    Just TyBottom{} -> Just (NodeId baseKey)
                                                    _ -> Nothing
                                        _ -> Nothing
                        _ -> Nothing
            let boundTarget =
                    case (baseTarget, IntMap.lookup (getNodeId rootC) nodes) of
                        (Nothing, Just TyVar{ tnBound = Nothing }) ->
                            case IntSet.toList rootBoundCandidates of
                                [baseKey] -> Just (NodeId baseKey)
                                _ -> Nothing
                        _ -> Nothing
            debugWhenM
                (let showBase tr =
                        let bases =
                                [ resolveBaseBoundForInstConstraint constraint canonical argNode
                                | (binder, arg) <- etBinderArgs tr
                                , let argNode =
                                        case IntMap.lookup (getNodeId binder) (etCopyMap tr) of
                                            Just copyN -> copyN
                                            Nothing -> arg
                                ]
                        in (etRoot tr, bases)
                     perEdge = map showBase (IntMap.elems edgeTraces)
                     edgeRightBases =
                        [ (EdgeId eid, ewRight ew, resolveBaseBoundForInstConstraint constraint canonical (ewRight ew))
                        | (eid, ew) <- IntMap.toList edgeWitnesses
                        ]
                     edgeExpansionsList =
                        [ (EdgeId eid, expn)
                        | (eid, expn) <- IntMap.toList edgeExpansions
                        ]
                 in "runPipelineElab: instArgBaseBounds="
                        ++ show instArgBaseBounds
                        ++ " instArgRootMultiBase="
                        ++ show instArgRootMultiBase
                        ++ " rootArgBaseBounds="
                        ++ show rootArgBaseBounds
                        ++ " rootBoundCandidates="
                        ++ show rootBoundCandidates
                        ++ " rootHasMultiInst="
                        ++ show rootHasMultiInst
                        ++ " rootForTypeAnn="
                        ++ case stripAnn rootForTypeAnn of
                            AApp _ _ funEid argEid nid ->
                                "AApp funEid=" ++ show funEid
                                    ++ " argEid=" ++ show argEid
                                    ++ " appNode=" ++ show nid
                            other -> show (annNode other)
                        ++ " baseNames="
                        ++ show
                            [ tnBase node
                            | node@TyBase{} <- IntMap.elems nodes
                            ]
                        ++ " perEdgeBases="
                        ++ show perEdge
                        ++ " edgeRightBases="
                        ++ show edgeRightBases
                        ++ " edgeExpansions="
                        ++ show edgeExpansionsList
                )
            let resFinal =
                    case baseTarget of
                        Just _ -> solvedClean
                        Nothing -> solvedForGen
                resFinalBounded =
                    case boundTarget of
                        Nothing -> resFinal
                        Just baseN ->
                            let nodes' =
                                    IntMap.adjust
                                        (\node ->
                                            case node of
                                                TyVar{ tnId = nid, tnBound = Nothing } ->
                                                    TyVar{ tnId = nid, tnBound = Just (canonical baseN) }
                                                _ -> node
                                        )
                                        (getNodeId rootC)
                                        (cNodes (srConstraint resFinal))
                                constraint' = (srConstraint resFinal) { cNodes = nodes' }
                            in resFinal { srConstraint = constraint' }
            let scopeRootNodePre = rootForTypePre
            scopeRoot0' <- firstShow (bindingScopeRef c1 scopeRootNodePre)
            let scopeRootBase = preferGenScope c1 scopeRoot0'
            let scopeRootPre = canonicalizeScopeRef resFinalBounded redirects scopeRootBase
            let scopeRootPost =
                    case bindingScopeRef (srConstraint resFinalBounded) rootC of
                        Right ref -> canonicalizeScopeRef resFinalBounded redirects ref
                        Left _ -> scopeRootPre
                scopeRoot = scopeRootPre
            debugWhenCondM (scopeRootPre /= scopeRootPost)
                ("runPipelineElab: ga' mismatch pre="
                    ++ show scopeRootPre
                    ++ " post="
                    ++ show scopeRootPost
                    ++ " preNode="
                    ++ show rootForTypePre
                    ++ " postNode="
                    ++ show rootC
                )
            let canonicalFinal = frWith (srUnionFind resFinalBounded)
                rootFinal = canonicalFinal rootC
                nodesFinal = cNodes (srConstraint resFinalBounded)
                rootBound =
                    case IntMap.lookup (getNodeId rootFinal) nodesFinal of
                        Just TyVar{ tnBound = Just bnd } -> Just (canonicalFinal bnd)
                        _ -> Nothing
                rootBoundIsBase =
                    case rootBound of
                        Just bnd ->
                            case IntMap.lookup (getNodeId bnd) nodesFinal of
                                Just TyBase{} -> True
                                Just TyBottom{} -> True
                                _ -> False
                        Nothing -> False
                rootIsSchemeRoot =
                    any
                        (\gen -> any (\root -> canonicalFinal root == rootFinal) (gnSchemes gen))
                        (NodeAccess.allGenNodes (srConstraint resFinalBounded))
                rootIsSchemeAlias =
                    rootIsSchemeRoot
                        && maybe False (const True) rootBound
                rootBoundIsBaseLike = rootBoundIsBase
                boundVarTargetRoot = canonicalFinal (schemeBodyTarget resFinalBounded rootC)
                schemeRootSetFinal =
                    IntSet.fromList
                        [ getNodeId (canonicalFinal root)
                        | gen <- NodeAccess.allGenNodes (srConstraint resFinalBounded)
                        , root <- gnSchemes gen
                        ]
                schemeRootOwnerFinal =
                    IntMap.fromList
                        [ (getNodeId (canonicalFinal root), gnId gen)
                        | gen <- NodeAccess.allGenNodes (srConstraint resFinalBounded)
                        , root <- gnSchemes gen
                        ]
                boundHasForallFrom start0 =
                    let go visited nid0 =
                            let nid = canonicalFinal nid0
                                key = getNodeId nid
                                isNestedSchemeRoot =
                                    case IntMap.lookup key schemeRootOwnerFinal of
                                        Just gid ->
                                            case scopeRoot of
                                                GenRef gidScope -> gid /= gidScope
                                                TypeRef _ -> True
                                        Nothing -> False
                                reportHit label =
                                    if debugGaScopeEnabled
                                        then
                                            let owner = IntMap.lookup key schemeRootOwnerFinal
                                                nodeTag =
                                                    case IntMap.lookup key nodesFinal of
                                                        Just TyVar{} -> "var"
                                                        Just TyArrow{} -> "arrow"
                                                        Just TyForall{} -> "forall"
                                                        Just TyExp{} -> "exp"
                                                        Just TyBase{} -> "base"
                                                        Just TyBottom{} -> "bottom"
                                                        Nothing -> "missing"
                                            in debugGaScope
                                                ("runPipelineElab: boundHasForall hit="
                                                    ++ label
                                                    ++ " node="
                                                    ++ show nid
                                                    ++ " tag="
                                                    ++ nodeTag
                                                    ++ " owner="
                                                    ++ show owner
                                                    ++ " scope="
                                                    ++ show scopeRoot
                                                )
                                                ()
                                        else ()
                            in if IntSet.member key visited
                                then False
                                else if IntSet.member key schemeRootSetFinal
                                    then
                                        case IntMap.lookup key nodesFinal of
                                            Just TyVar{ tnBound = Just bnd } ->
                                                go (IntSet.insert key visited) bnd
                                            _ ->
                                                if isNestedSchemeRoot
                                                    then case reportHit "schemeRoot" of
                                                        () -> True
                                                    else False
                                else
                                    case IntMap.lookup key nodesFinal of
                                        Just TyForall{} ->
                                            case reportHit "TyForall" of
                                                () -> True
                                        Just TyVar{ tnBound = Just bnd } ->
                                            let bndC = canonicalFinal bnd
                                                bndKey = getNodeId bndC
                                                bndNested =
                                                    case IntMap.lookup bndKey schemeRootOwnerFinal of
                                                        Just gid ->
                                                            case scopeRoot of
                                                                GenRef gidScope -> gid /= gidScope
                                                                TypeRef _ -> True
                                                        Nothing -> False
                                            in if IntSet.member bndKey schemeRootSetFinal
                                                then
                                                    case IntMap.lookup bndKey nodesFinal of
                                                        Just TyVar{ tnBound = Just bndInner } ->
                                                            go (IntSet.insert key visited) bndInner
                                                        _ ->
                                                            if bndNested
                                                                then case reportHit "boundSchemeRoot" of
                                                                    () -> True
                                                                else False
                                                else go (IntSet.insert key visited) bndC
                                        Just TyExp{ tnBody = b } ->
                                            go (IntSet.insert key visited) b
                                        Just TyArrow{ tnDom = d, tnCod = c } ->
                                            let visited' = IntSet.insert key visited
                                            in go visited' d || go visited' c
                                        _ -> False
                    in go IntSet.empty start0
                boundVarTarget =
                    case scopeRoot of
                        GenRef gid ->
                            let candidates =
                                    [ ( canonicalFinal child
                                      , canonicalFinal bnd
                                      , canonicalFinal (schemeBodyTarget resFinalBounded bnd)
                                      , boundHasForallFrom bnd
                                      )
                                    | (childKey, (parentRef, _flag)) <- IntMap.toList (cBindParents (srConstraint resFinalBounded))
                                    , parentRef == GenRef gid
                                    , TypeRef child <- [nodeRefFromKey childKey]
                                    , Just bnd <- [VarStore.lookupVarBound (srConstraint resFinalBounded) (canonicalFinal child)]
                                    ]
                                debugCandidates =
                                    if debugGaScopeEnabled
                                        then
                                            debugGaScope
                                                ("runPipelineElab: boundVarTargetRoot="
                                                    ++ show boundVarTargetRoot
                                                    ++ " candidates="
                                                    ++ show
                                                        [ (child, bnd, bndRoot, hasForall)
                                                        | (child, bnd, bndRoot, hasForall) <- candidates
                                                        ]
                                                )
                                                ()
                                        else ()
                            in case debugCandidates of
                                () ->
                                    listToMaybe
                                        [ child
                                        | (child, _bnd, bndRoot, hasForall) <- candidates
                                        , bndRoot == boundVarTargetRoot
                                        , not hasForall
                                        ]
                        _ -> Nothing
                keepTargetFinal =
                    rootHasMultiInst
                        || instArgRootMultiBase
                        || (rootIsSchemeAlias && rootBoundIsBaseLike)
                        || maybe False (const True) boundVarTarget
            let targetC =
                    case baseTarget of
                        Just baseC -> baseC
                        Nothing ->
                            if keepTargetFinal
                                then
                                    case IntMap.lookup (getNodeId rootFinal) nodesFinal of
                                        Just TyVar{} -> rootFinal
                                        _ ->
                                            case boundVarTarget of
                                                Just v -> v
                                                Nothing -> schemeBodyTarget resFinalBounded rootC
                                else schemeBodyTarget resFinalBounded rootC
            let bindParentsGaFinal =
                    case boundTarget of
                        Just baseN ->
                            let baseConstraint = gaBaseConstraint bindParentsGa
                                baseRoot =
                                    IntMap.findWithDefault rootC
                                        (getNodeId rootC)
                                        (gaSolvedToBase bindParentsGa)
                                nodes' =
                                    IntMap.adjust
                                        (\node ->
                                            case node of
                                                TyVar{ tnId = nid, tnBound = Nothing } ->
                                                    TyVar{ tnId = nid, tnBound = Just baseN }
                                                _ -> node
                                        )
                                        (getNodeId baseRoot)
                                        (cNodes baseConstraint)
                                baseConstraint' = baseConstraint { cNodes = nodes' }
                            in bindParentsGa { gaBaseConstraint = baseConstraint' }
                        Nothing -> bindParentsGa
            (sch, _subst) <- firstShow
                (generalizeWithPlan planBuilder bindParentsGaFinal resFinalBounded scopeRoot targetC)
            debugWhenM
                ("runPipelineElab: final scheme="
                    ++ pretty sch
                    ++ " keepTargetBase="
                    ++ show (case baseTarget of { Just _ -> True; Nothing -> False })
                    ++ " targetC="
                    ++ show targetC
                    ++ " scopeRoot="
                    ++ show scopeRoot
                )
            let ty = case sch of
                    Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds
            pure ty
