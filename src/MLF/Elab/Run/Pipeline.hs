module MLF.Elab.Run.Pipeline (
    runPipelineElab,
    runPipelineElabChecked
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , GeneralizePolicy
    , policyDefault
    , policyKeepTarget
    , PresolutionPlanBuilder(..)
    , PresolutionResult(..)
    , computePresolution
    )
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import MLF.Constraint.Types
    ( BoundRef(..)
    , EdgeId(..)
    , EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , GenNode(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    , InstEdge(..)
    , NodeId(..)
    , NodeRef(..)
    , PolySyms
    , TyNode(..)
    , cBindParents
    , cGenNodes
    , cInstEdges
    , cLetEdges
    , cNodes
    , ewRight
    , genNodeKey
    , getEdgeId
    , getNodeId
    , gnId
    , gnSchemes
    , instLeft
    , instRight
    , nodeRefFromKey
    , structuralChildren
    , typeRef
    )
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Elaborate (elaborateWithScope)
import MLF.Elab.Generalize
    ( GaBindParents(..)
    )
import MLF.Util.Names (parseNameId)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Reify.Core (reifyType, reifyTypeWithNamesNoFallbackOnConstraint)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Reify.TypeOps (inlineBaseBoundsType, resolveBaseBoundForInstConstraint)
import MLF.Elab.Run.Annotation (adjustAnnotationInst, annNode, applyRedirectsToAnn, canonicalizeAnn)
import MLF.Elab.Run.Debug (debugGaScope, debugGaScopeEnabled, edgeOrigins)
import MLF.Elab.Run.Generalize
    ( constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    , pruneBindParentsConstraint
    )
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme, instInsideFromArgsWithBounds)
import MLF.Elab.Run.Scope
    ( bindingScopeRef
    , canonicalizeScopeRef
    , letScopeOverrides
    , preferGenScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.TypeOps (inlineBoundVarsType, simplifyAnnotationType)
import MLF.Elab.Run.Util (chaseRedirects)

runPipelineElab :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElab polySyms = runPipelineElabWith (generateConstraints polySyms)

runPipelineElabChecked :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElabChecked polySyms expr = do
    (term, _ty) <- runPipelineElab polySyms expr
    tyChecked <- firstShow (typeCheck term)
    pure (term, tyChecked)

generalizeWithPlan
    :: PresolutionPlanBuilder
    -> GeneralizePolicy
    -> GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeWithPlan planBuilder policy bindParentsGa res scopeRoot targetNode =
    generalizeAtWithBuilder
        planBuilder
        policy
        (Just bindParentsGa)
        res
        scopeRoot
        targetNode

runPipelineElabWith
    :: (Expr -> Either ConstraintError ConstraintResult)
    -> Expr
    -> Either String (ElabTerm, ElabType)
runPipelineElabWith genConstraints expr = do
    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- firstShow (genConstraints expr)
    let c1 = normalize c0
    let debugEdges =
            if debugGaScopeEnabled
                then
                    let targetEid = 3
                        origins = edgeOrigins ann
                        edgeDesc =
                            case IntMap.lookup targetEid origins of
                                Nothing -> "none"
                                Just msg -> msg
                        instEdge =
                            listToMaybe
                                [ (instLeft e, instRight e)
                                | e <- cInstEdges c1
                                , getEdgeId (instEdgeId e) == targetEid
                                ]
                    in debugGaScope
                        ("edge origin: eid="
                            ++ show targetEid
                            ++ " origin="
                            ++ edgeDesc
                            ++ " instEdge="
                            ++ show instEdge
                        )
                        ()
                else ()
    case debugEdges of
        () -> pure ()
    acyc <- firstShow (checkAcyclicity c1)
    pres <- firstShow (computePresolution acyc c1)
    let planBuilder = prPlanBuilder pres
        generalizeAtWith = generalizeAtWithBuilder planBuilder
    solved <- firstShow (solveUnify (prConstraint pres))
    let solvedClean = solved { srConstraint = pruneBindParentsConstraint (srConstraint solved) }
    case validateSolvedGraphStrict solvedClean of
        [] -> do
            let canonicalSolved = frWith (srUnionFind solvedClean)
                adoptNode = canonicalSolved . chaseRedirects (prRedirects pres)
                instCopyNodes =
                    instantiationCopyNodes solvedClean (prRedirects pres) (prEdgeTraces pres)
                instCopyMapFull =
                    let baseNodes = cNodes c1
                        baseBindParents = cBindParents c1
                        baseNamedKeysAll =
                            IntSet.fromList
                                [ childKey
                                | (childKey, (parentRef, _flag)) <- IntMap.toList baseBindParents
                                , case parentRef of
                                    GenRef _ -> True
                                    _ -> False
                                , TypeRef child <- [nodeRefFromKey childKey]
                                , case IntMap.lookup (getNodeId child) baseNodes of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        traceMaps =
                            [ let copyMap0 = etCopyMap tr
                                  rootBase = etRoot tr
                                  baseInteriorSet =
                                      case Binding.interiorOf c1 (typeRef rootBase) of
                                          Right s ->
                                              IntSet.insert
                                                  (getNodeId rootBase)
                                                  (IntSet.fromList
                                                      [ getNodeId nid
                                                      | key <- IntSet.toList s
                                                      , TypeRef nid <- [nodeRefFromKey key]
                                                      ]
                                                  )
                                          Left _ -> IntSet.singleton (getNodeId rootBase)
                                  binderCopyOverrides =
                                      IntMap.fromList
                                          [ (getNodeId (adoptNode copyN), NodeId baseKey)
                                          | (baseKey, copyN) <- IntMap.toList copyMap0
                                          , IntSet.member baseKey baseNamedKeysAll
                                          ]
                                  binderMetaOverrides =
                                      IntMap.fromList
                                          [ (getNodeId (adoptNode meta), binder)
                                          | (binder, _arg) <- etBinderArgs tr
                                          , Just meta <- [IntMap.lookup (getNodeId binder) copyMap0]
                                          ]
                                  invMap =
                                      IntMap.fromListWith
                                          (\_ old -> old)
                                          [ (getNodeId (adoptNode copyN), NodeId baseKey)
                                          | (baseKey, copyN) <- IntMap.toList copyMap0
                                          ]
                                  ensureRoot acc =
                                      let rootCopyKey = getNodeId (adoptNode rootBase)
                                      in IntMap.insertWith (\_ old -> old) rootCopyKey rootBase acc
                                  addInterior acc nidInt =
                                      let baseN = NodeId nidInt
                                          copyKey = getNodeId (adoptNode baseN)
                                      in if IntMap.member copyKey acc
                                          then acc
                                          else if IntMap.member nidInt baseNodes
                                              && IntSet.member nidInt baseInteriorSet
                                              then IntMap.insert copyKey baseN acc
                                          else IntMap.insert copyKey rootBase acc
                              in foldl'
                                    addInterior
                                    (ensureRoot (IntMap.union binderMetaOverrides (IntMap.union binderCopyOverrides invMap)))
                                    (IntSet.toList (etInterior tr))
                            | tr <- IntMap.elems (prEdgeTraces pres)
                            ]
                    in foldl' IntMap.union IntMap.empty traceMaps
                (constraintForGen, bindParentsGa) =
                    constraintForGeneralization solvedClean (prRedirects pres) instCopyNodes instCopyMapFull c1 ann
            let solvedForGen = solvedClean { srConstraint = constraintForGen }
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            let canonNode = canonicalSolved . chaseRedirects (prRedirects pres)
                canonOp op = case op of
                    OpGraft a b -> OpGraft (canonNode a) (canonNode b)
                    OpMerge a b -> OpMerge (canonNode a) (canonNode b)
                    OpRaise n -> OpRaise (canonNode n)
                    OpWeaken n -> OpWeaken (canonNode n)
                    OpRaiseMerge a b -> OpRaiseMerge (canonNode a) (canonNode b)
                canonStep step = case step of
                    StepOmega op -> StepOmega (canonOp op)
                    StepIntro -> StepIntro
                canonWitness w =
                    let InstanceWitness ops = ewWitness w
                    in w
                        { ewLeft = canonNode (ewLeft w)
                        , ewRight = canonNode (ewRight w)
                        , ewRoot = canonNode (ewRoot w)
                        , ewSteps = map canonStep (ewSteps w)
                        , ewWitness = InstanceWitness (map canonOp ops)
                        }
                canonTrace tr =
                    let canonPair (a, b) = (canonNode a, canonNode b)
                        canonInterior =
                            IntSet.fromList
                                [ getNodeId (canonNode (NodeId i))
                                | i <- IntSet.toList (etInterior tr)
                                ]
                        canonCopyMap =
                            IntMap.fromListWith min
                                [ ( getNodeId (canonNode (NodeId k))
                                  , canonNode v
                                  )
                                | (k, v) <- IntMap.toList (etCopyMap tr)
                                ]
                    in tr
                        { etRoot = canonNode (etRoot tr)
                        , etBinderArgs = map canonPair (etBinderArgs tr)
                        , etInterior = canonInterior
                        , etCopyMap = canonCopyMap
                        }
                canonExpansion expn = case expn of
                    ExpIdentity -> ExpIdentity
                    ExpForall specs ->
                        let canonBound bnd = case bnd of
                                BoundNode nid -> BoundNode (canonNode nid)
                                BoundBinder ix -> BoundBinder ix
                            canonSpec spec =
                                spec
                                    { fsBounds =
                                        map
                                            (fmap canonBound)
                                            (fsBounds spec)
                                    }
                        in ExpForall (NE.map canonSpec specs)
                    ExpInstantiate args -> ExpInstantiate (map canonNode args)
                    ExpCompose es -> ExpCompose (NE.map canonExpansion es)
            let annCanon = canonicalizeAnn canonNode ann'
            let edgeWitnesses = IntMap.map canonWitness (prEdgeWitnesses pres)
                edgeTraces = IntMap.map canonTrace (prEdgeTraces pres)
                edgeExpansions = IntMap.map canonExpansion (prEdgeExpansions pres)
            let scopeOverrides = letScopeOverrides c1 (srConstraint solvedForGen) solvedClean (prRedirects pres) annCanon
            term <- firstShow (elaborateWithScope generalizeAtWith solvedClean solvedClean solvedForGen bindParentsGa edgeWitnesses edgeTraces edgeExpansions scopeOverrides annCanon)

            -- Also apply redirects to the root node, as it might have been a TyExp.
            let canonical = frWith (srUnionFind solvedClean)
                typeFromAnn inner innerPre annNodeId eid = do
                    let rootPre = annNode inner
                        rootC = canonical rootPre
                    ew <- case IntMap.lookup (getEdgeId eid) edgeWitnesses of
                        Nothing -> Left "missing edge witness for annotation"
                        Just ew' -> Right ew'
                    let mTrace = IntMap.lookup (getEdgeId eid) edgeTraces
                        constraintGen = srConstraint solvedForGen
                        schemeRootSet =
                            IntSet.fromList
                                [ getNodeId (canonical root)
                                | gen <- IntMap.elems (cGenNodes constraintGen)
                                , root <- gnSchemes gen
                                ]
                        boundHasForallNode nid0 =
                            let go visited nid1 =
                                    let nid = canonical nid1
                                        key = getNodeId nid
                                    in if IntSet.member key visited
                                        then False
                                        else if IntSet.member key schemeRootSet
                                            then True
                                            else
                                                case IntMap.lookup key (cNodes constraintGen) of
                                                    Just TyForall{} -> True
                                                    Just TyVar{ tnBound = Just bnd } ->
                                                        go (IntSet.insert key visited) bnd
                                                    Just TyExp{ tnBody = b } ->
                                                        go (IntSet.insert key visited) b
                                                    Just node ->
                                                        let visited' = IntSet.insert key visited
                                                        in any (go visited') (structuralChildren node)
                                                    Nothing -> False
                            in go IntSet.empty nid0
                        keepTarget =
                            let traceKeep =
                                    case mTrace of
                                        Just tr -> length (etBinderArgs tr) > 1
                                        Nothing -> False
                                boundKeep =
                                    case IntMap.lookup (getNodeId rootC) (cNodes constraintGen) of
                                        Just TyVar{ tnBound = Just bnd } -> boundHasForallNode bnd
                                        _ -> False
                            in traceKeep || boundKeep
                    let targetC = schemeBodyTarget solvedForGen rootC
                        scopeRootNodePre0 = annNode innerPre
                        scopeRootNodePre =
                            let solvedToBase = gaSolvedToBase bindParentsGa
                            in IntMap.findWithDefault scopeRootNodePre0 (getNodeId targetC) solvedToBase
                        scopeRootNodePost = annNode inner
                    scopeRoot0 <- firstShow (bindingScopeRef c1 scopeRootNodePre)
                    let scopeRootBase = preferGenScope c1 scopeRoot0
                    let scopeRootPre = canonicalizeScopeRef solvedForGen (prRedirects pres) scopeRootBase
                    let scopeRootPost =
                            case bindingScopeRef (srConstraint solvedForGen) scopeRootNodePost of
                                Right ref -> canonicalizeScopeRef solvedForGen (prRedirects pres) ref
                                Left _ -> scopeRootPre
                    let _ =
                            if debugGaScopeEnabled && scopeRootPre /= scopeRootPost
                                then
                                    debugGaScope
                                        ("runPipelineElab: ga' mismatch (AAnn) pre="
                                            ++ show scopeRootPre
                                            ++ " post="
                                            ++ show scopeRootPost
                                            ++ " preNode="
                                            ++ show scopeRootNodePre
                                            ++ " postNode="
                                            ++ show scopeRootNodePost
                                        )
                                        ()
                                else ()
                        scopeRoot = scopeRootPre
                    let policy =
                            if keepTarget
                                then policyKeepTarget
                                else policyDefault
                    (sch0, subst0) <- firstShow
                        (generalizeWithPlan planBuilder policy bindParentsGa solvedForGen scopeRoot targetC)
                    let sch = sch0
                        subst = subst0
                        srcTy = schemeToType sch
                        schemeInfo = SchemeInfo { siScheme = sch, siSubst = subst }
                    pure ()
                    phi0 <- firstShow (phiFromEdgeWitnessWithTrace generalizeAtWith solvedForGen (Just bindParentsGa) (Just schemeInfo) mTrace ew)
                    let annBound = VarStore.lookupVarBound (srConstraint solvedForGen) annNodeId
                        annTargetNode0 =
                            case annBound of
                                Just bnd -> bnd
                                Nothing -> annNodeId
                        annTargetNode = schemeBodyTarget solvedForGen annTargetNode0
                        targetTyRawM =
                            case reifyType solvedForGen annTargetNode of
                                Left _ -> Nothing
                                Right ty0 -> Just ty0
                        targetTyMatchM =
                            fmap
                                (inlineBoundVarsType solvedForGen)
                                targetTyRawM
                        baseConstraint = gaBaseConstraint bindParentsGa
                        solvedToBase = gaSolvedToBase bindParentsGa
                        toBase nid =
                            IntMap.findWithDefault nid (getNodeId nid) solvedToBase
                        inlineAllBoundsType =
                            let go boundNames seen ty = case ty of
                                    TVar v
                                        | Set.member v boundNames -> TVar v
                                        | otherwise ->
                                            case parseNameId v of
                                                Just nidInt ->
                                                    let nid = NodeId nidInt
                                                    in if IntSet.member (getNodeId (toBase nid)) seen
                                                        then TVar v
                                                        else
                                                            case VarStore.lookupVarBound baseConstraint (toBase nid) of
                                                                Just bnd ->
                                                                    case reifyTypeWithNamesNoFallbackOnConstraint
                                                                            baseConstraint
                                                                            IntMap.empty
                                                                            bnd of
                                                                        Right ty' ->
                                                                            go boundNames
                                                                                (IntSet.insert (getNodeId (toBase nid)) seen)
                                                                                ty'
                                                                        Left _ -> TVar v
                                                                Nothing -> TVar v
                                                Nothing -> TVar v
                                    TArrow a b -> TArrow (go boundNames seen a) (go boundNames seen b)
                                    TForall v mb body ->
                                        let boundNames' = Set.insert v boundNames
                                        in TForall v (fmap (go boundNames seen) mb) (go boundNames' seen body)
                                    TBase _ -> ty
                                    TBottom -> ty
                            in go Set.empty IntSet.empty
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
                        instantiateImplicitForalls ty0 =
                            let go ty = case ty of
                                    TForall _ (Just _) _ ->
                                        case applyInstantiation ty InstElim of
                                            Right ty' -> go ty'
                                            Left _ -> ty
                                    TForall v mb body ->
                                        TForall v (fmap go mb) (go body)
                                    TArrow a b -> TArrow (go a) (go b)
                                    TBase _ -> ty
                                    TBottom -> ty
                                    TVar _ -> ty
                            in go ty0
                        normalizeTarget ty =
                            if annotationExplicit
                                then ty
                                else instantiateImplicitForalls ty
                        targetTyMatchNormM =
                            fmap normalizeTarget targetTyMatchM
                        targetTyBaseM =
                            let baseCandidate =
                                    case IntMap.lookup (getNodeId annTargetNode) (gaSolvedToBase bindParentsGa) of
                                        Just baseN -> Just baseN
                                        Nothing ->
                                            if IntMap.member (getNodeId annTargetNode) (cNodes baseConstraint)
                                                then Just annTargetNode
                                                else Nothing
                            in case baseCandidate of
                                Just baseN ->
                                    let resolveBoundBodyBase seen nid0 =
                                            let key = getNodeId nid0
                                            in if IntSet.member key seen
                                                then nid0
                                                else case VarStore.lookupVarBound baseConstraint nid0 of
                                                    Just bnd ->
                                                        resolveBoundBodyBase
                                                            (IntSet.insert key seen)
                                                            bnd
                                                    Nothing -> nid0
                                        baseRoot = resolveBoundBodyBase IntSet.empty baseN
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
                    case
                        if debugGaScopeEnabled
                            then
                                debugGaScope
                                    ("runPipelineElab: ann explicit="
                                        ++ show annotationExplicit
                                        ++ " annTargetBase="
                                        ++ show annTargetBase
                                        ++ " annGen="
                                        ++ show annGenMaybe
                                    )
                                    ()
                            else ()
                        of
                        () -> pure ()
                    let containsAnyForall ty =
                            let go t = case t of
                                    TForall _ mb body ->
                                        True || maybe False go mb || go body
                                    TArrow a b -> go a || go b
                                    _ -> False
                            in go ty
                        containsBoundForall ty =
                            let go t = case t of
                                    TForall _ mb body ->
                                        maybe False containsAnyForall mb || go body
                                    TArrow a b -> go a || go b
                                    _ -> False
                            in go ty
                        targetHasBoundForall =
                            case targetTyForMatchM of
                                Just ty -> containsBoundForall ty
                                Nothing -> False
                    let phiFromTargetArgs =
                            case (sch, targetTyForMatchM) of
                                (Forall binds body, Just targetTy)
                                    | not targetHasBoundForall ->
                                        inferInstAppArgsFromScheme binds body targetTy
                                _ -> Nothing
                        phiFromTarget =
                            case (sch, phiFromTargetArgs) of
                                (Forall binds _, Just args) ->
                                    Just (instInsideFromArgsWithBounds binds (map (inlineBoundVarsType solvedForGen) args))
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
                    case
                        if debugGaScopeEnabled
                            then
                                debugGaScope
                                    ("runPipelineElab: ann targetTy="
                                        ++ show targetTyMatchM
                                        ++ " targetTyRaw="
                                        ++ show targetTyRawM
                                        ++ " targetHasBoundForall="
                                        ++ show targetHasBoundForall
                                        ++ " phiFromTarget="
                                        ++ show
                                            phiFromTargetArgs
                                    )
                                    ()
                            else ()
                        of
                        () -> pure ()
                    case
                        if debugGaScopeEnabled
                            then
                                debugGaScope
                                    ("runPipelineElab: ann srcTy="
                                        ++ pretty srcTy
                                        ++ " phi0="
                                        ++ pretty phi0
                                        ++ " phi="
                                        ++ pretty phi
                                    )
                                    ()
                            else ()
                        of
                        () -> pure ()
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
                                        annScopeRoot = canonicalizeScopeRef solvedForGen (prRedirects pres) annScopeRootBase
                                    let policyAnn =
                                            if keepTarget
                                                then policyKeepTarget
                                                else policyDefault
                                    (annSch, _substAnn) <- firstShow
                                        (generalizeWithPlan planBuilder policyAnn bindParentsGa solvedForGen annScopeRoot annTargetNode)
                                    let annTy = schemeToType annSch
                                    pure (simplifyAnnotationType annTy)

            case annCanon of
                AAnn inner annNodeId eid -> do
                    ty <- typeFromAnn inner inner annNodeId eid
                    pure (term, ty)
                _ -> do
                    let edgeTraceCounts =
                            IntMap.fromListWith
                                (+)
                                [ (getNodeId (etRoot tr), 1 :: Int)
                                | tr <- IntMap.elems edgeTraces
                                ]
                        stripAnn ann0 = case ann0 of
                            AAnn inner _ _ -> stripAnn inner
                            _ -> ann0
                        collectEdges ann0 = case ann0 of
                            AVar _ _ -> []
                            ALit _ _ -> []
                            ALam _ _ _ body _ -> collectEdges body
                            AApp f a funEid argEid _ ->
                                funEid : argEid : collectEdges f ++ collectEdges a
                            ALet _ _ _ _ _ rhs body _ ->
                                collectEdges rhs ++ collectEdges body
                            AAnn inner _ eid -> eid : collectEdges inner
                    let schemeRootSet =
                            IntSet.fromList
                                [ getNodeId (canonical root)
                                | gen <- IntMap.elems (cGenNodes (srConstraint solvedForGen))
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
                            ty <- typeFromAnn inner innerPre annNodeId eid
                            pure (term, ty)
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
                                rootArgBaseBounds =
                                    let argBounds arg = do
                                            baseC <- resolveBaseBoundForInstConstraint constraint canonical arg
                                            case IntMap.lookup (getNodeId baseC) nodes of
                                                Just TyBase{} -> Just baseC
                                                Just TyBottom{} -> Just baseC
                                                _ -> Nothing
                                        argEdgeBases eid =
                                            case IntMap.lookup (getEdgeId eid) edgeTraces of
                                                Nothing -> IntSet.empty
                                                Just tr ->
                                                    IntSet.fromList
                                                        [ getNodeId baseC
                                                        | (binder, arg) <- etBinderArgs tr
                                                        , let argNode =
                                                                case IntMap.lookup (getNodeId binder) (etCopyMap tr) of
                                                                    Just copyN -> copyN
                                                                    Nothing -> arg
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
                                    let argBounds arg = do
                                            baseC <- resolveBaseBoundForInstConstraint constraint canonical arg
                                            case IntMap.lookup (getNodeId baseC) nodes of
                                                Just TyBase{} -> Just baseC
                                                Just TyBottom{} -> Just baseC
                                                _ -> Nothing
                                        instArgNode tr binder arg =
                                            case IntMap.lookup (getNodeId binder) (etCopyMap tr) of
                                                Just copyN -> copyN
                                                Nothing -> arg
                                        binderBounds =
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
                                        edgeBaseBounds =
                                            IntSet.fromList
                                                [ getNodeId baseC
                                                | ew <- IntMap.elems edgeWitnesses
                                                , Just baseC <- [argBounds (ewRight ew)]
                                                ]
                                    in IntSet.union
                                        (IntSet.unions
                                        [ bounds
                                        | bounds <- IntMap.elems binderBounds
                                        , IntSet.size bounds > 1
                                        ])
                                        edgeBaseBounds
                                (rootBounds, instArgRootMultiBase) =
                                    let argBounds arg = do
                                            baseC <- resolveBaseBoundForInstConstraint constraint canonical arg
                                            case IntMap.lookup (getNodeId baseC) nodes of
                                                Just TyBase{} -> Just baseC
                                                Just TyBottom{} -> Just baseC
                                                _ -> Nothing
                                        instArgNode tr binder arg =
                                            case IntMap.lookup (getNodeId binder) (etCopyMap tr) of
                                                Just copyN -> copyN
                                                Nothing -> arg
                                        rootBounds' =
                                            IntMap.fromListWith IntSet.union
                                                [ (getNodeId (canonical (etRoot tr)), IntSet.singleton (getNodeId baseC))
                                                | tr <- IntMap.elems edgeTraces
                                                , (binder, arg) <- etBinderArgs tr
                                                , let argNode = instArgNode tr binder arg
                                                , Just baseC <- [argBounds argNode]
                                                ]
                                        edgeBaseBounds =
                                            IntSet.fromList
                                                [ getNodeId baseC
                                                | ew <- IntMap.elems edgeWitnesses
                                                , Just baseC <- [argBounds (ewRight ew)]
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
                            let debugInstArgs =
                                    if debugGaScopeEnabled
                                        then
                                            let showBase tr =
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
                                            in debugGaScope
                                                ("runPipelineElab: instArgBaseBounds="
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
                                                ()
                                        else ()
                            case debugInstArgs of
                                () -> pure ()
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
                            let scopeRootPre = canonicalizeScopeRef resFinalBounded (prRedirects pres) scopeRootBase
                            let scopeRootPost =
                                    case bindingScopeRef (srConstraint resFinalBounded) rootC of
                                        Right ref -> canonicalizeScopeRef resFinalBounded (prRedirects pres) ref
                                        Left _ -> scopeRootPre
                            let _ =
                                    if debugGaScopeEnabled && scopeRootPre /= scopeRootPost
                                        then
                                            debugGaScope
                                                ("runPipelineElab: ga' mismatch pre="
                                                    ++ show scopeRootPre
                                                    ++ " post="
                                                    ++ show scopeRootPost
                                                    ++ " preNode="
                                                    ++ show rootForTypePre
                                                    ++ " postNode="
                                                    ++ show rootC
                                                )
                                                ()
                                else ()
                                scopeRoot = scopeRootPre
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
                                        (IntMap.elems (cGenNodes (srConstraint resFinalBounded)))
                                rootIsSchemeAlias =
                                    rootIsSchemeRoot
                                        && maybe False (const True) rootBound
                                rootBoundIsBaseLike = rootBoundIsBase
                                boundVarTargetRoot = canonicalFinal (schemeBodyTarget resFinalBounded rootC)
                                schemeRootSetFinal =
                                    IntSet.fromList
                                        [ getNodeId (canonicalFinal root)
                                        | gen <- IntMap.elems (cGenNodes (srConstraint resFinalBounded))
                                        , root <- gnSchemes gen
                                        ]
                                schemeRootOwnerFinal =
                                    IntMap.fromList
                                        [ (getNodeId (canonicalFinal root), gnId gen)
                                        | gen <- IntMap.elems (cGenNodes (srConstraint resFinalBounded))
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
                            let policy =
                                    if keepTargetFinal
                                        then policyKeepTarget
                                        else policyDefault
                            (sch, _subst) <- firstShow
                                (generalizeWithPlan planBuilder policy bindParentsGaFinal resFinalBounded scopeRoot targetC)
                            let debugFinal =
                                    if debugGaScopeEnabled
                                        then
                                            debugGaScope
                                                ("runPipelineElab: final scheme="
                                                    ++ pretty sch
                                                    ++ " keepTargetBase="
                                                    ++ show (case baseTarget of { Just _ -> True; Nothing -> False })
                                                    ++ " targetC="
                                                    ++ show targetC
                                                    ++ " scopeRoot="
                                                    ++ show scopeRoot
                                                )
                                                ()
                                        else ()
                            case debugFinal of
                                () -> pure ()
                            let ty = case sch of
                                    Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds
                            pure (term, ty)
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
