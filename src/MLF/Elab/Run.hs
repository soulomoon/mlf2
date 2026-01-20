module MLF.Elab.Run (
    runPipelineElab,
    runPipelineElabChecked,
    applyRedirectsToAnn,
    chaseRedirects
) where

import Data.Functor.Foldable (Recursive (..), cata, para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import MLF.Frontend.Syntax (Expr)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Frontend.ConstraintGen.Types (AnnExprF(..))
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution (EdgeTrace(..), computePresolution, PresolutionResult(..))
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import MLF.Constraint.Types (BindFlag(..), BindingError(..), BoundRef(..), Constraint, EdgeId(..), EdgeWitness(..), Expansion(..), ForallSpec(..), GenNode(..), GenNodeId(..), InstanceOp(..), InstanceStep(..), InstanceWitness(..), InstEdge(..), NodeId(..), NodeRef(..), PolySyms, TyNode(..), cBindParents, cGenNodes, cInstEdges, cLetEdges, cNodes, ewRight, genNodeKey, getEdgeId, getNodeId, gnId, gnSchemes, instLeft, instRight, nodeRefFromKey, nodeRefKey, structuralChildren, typeRef)
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Elaborate (elaborateWithScope)
import MLF.Elab.Generalize
    ( GaBindParents(..)
    , generalizeAtAllowRigidWithBindParents
    , generalizeAtKeepTargetAllowRigidWithBindParents
    )
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Reify (reifyType, reifyTypeWithNamesNoFallbackOnConstraint)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.TypeCheck (typeCheck)
import MLF.Elab.Types
import MLF.Elab.Util (reachableFromStop)

-- | Run the full pipeline (Phases 1–5) then elaborate.
runPipelineElab :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElab polySyms = runPipelineElabWith (generateConstraints polySyms)

runPipelineElabChecked :: PolySyms -> Expr -> Either String (ElabTerm, ElabType)
runPipelineElabChecked polySyms expr = do
    (term, _ty) <- runPipelineElab polySyms expr
    tyChecked <- firstShow (typeCheck term)
    pure (term, tyChecked)

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
            term <- firstShow (elaborateWithScope solvedClean solvedClean solvedForGen bindParentsGa edgeWitnesses edgeTraces edgeExpansions scopeOverrides annCanon)

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
                    (sch0, subst0) <- firstShow
                        (if keepTarget
                            then generalizeAtKeepTargetAllowRigidWithBindParents bindParentsGa solvedForGen scopeRoot targetC
                            else generalizeAtAllowRigidWithBindParents bindParentsGa solvedForGen scopeRoot targetC)
                    let sch = sch0
                        subst = subst0
                        srcTy = schemeToType sch
                        schemeInfo = SchemeInfo { siScheme = sch, siSubst = subst }
                    pure ()
                    phi0 <- firstShow (phiFromEdgeWitnessWithTrace solvedForGen (Just bindParentsGa) (Just schemeInfo) mTrace ew)
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
                                            case parseName v of
                                                Just nid
                                                    | IntSet.member (getNodeId (toBase nid)) seen -> TVar v
                                                    | otherwise ->
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
                                    (annSch, _substAnn) <- firstShow
                                        (if keepTarget
                                            then generalizeAtKeepTargetAllowRigidWithBindParents bindParentsGa solvedForGen annScopeRoot annTargetNode
                                            else generalizeAtAllowRigidWithBindParents bindParentsGa solvedForGen annScopeRoot annTargetNode)
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
                                collectInstApps inst0 = case inst0 of
                                    InstApp ty -> [ty]
                                    InstSeq a b -> collectInstApps a ++ collectInstApps b
                                    InstInside phi -> collectInstApps phi
                                    InstUnder _ phi -> collectInstApps phi
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
                                            case phiFromEdgeWitnessWithTrace solved (Just bindParentsGa) Nothing (IntMap.lookup (getEdgeId funEid) edgeTraces) ew of
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
                                            baseC <- resolveBaseBoundForInst constraint canonical arg
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
                                                        case resolveBaseBoundForInst constraint canonical (annNode arg) of
                                                            Just baseC -> IntSet.singleton (getNodeId baseC)
                                                            Nothing -> IntSet.empty
                                        _ -> IntSet.empty
                                instArgBaseBounds =
                                    let argBounds arg = do
                                            baseC <- resolveBaseBoundForInst constraint canonical arg
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
                                            baseC <- resolveBaseBoundForInst constraint canonical arg
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
                                            case resolveBaseBoundForInst constraint canonical rootC of
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
                                                            [ resolveBaseBoundForInst constraint canonical argNode
                                                            | (binder, arg) <- etBinderArgs tr
                                                            , let argNode =
                                                                    case IntMap.lookup (getNodeId binder) (etCopyMap tr) of
                                                                        Just copyN -> copyN
                                                                        Nothing -> arg
                                                            ]
                                                    in (etRoot tr, bases)
                                                perEdge = map showBase (IntMap.elems edgeTraces)
                                                edgeRightBases =
                                                    [ (EdgeId eid, ewRight ew, resolveBaseBoundForInst constraint canonical (ewRight ew))
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
                            let generalizeFinal =
                                    if keepTargetFinal
                                        then generalizeAtKeepTargetAllowRigidWithBindParents
                                        else generalizeAtAllowRigidWithBindParents
                            (sch, _subst) <- firstShow
                                (generalizeFinal bindParentsGaFinal resFinalBounded scopeRoot targetC)
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

applyRedirectsToAnn :: IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
applyRedirectsToAnn redirects = cata alg
  where
    redir = chaseRedirects redirects
    alg ann = case ann of
        ALitF l nid -> ALit l (redir nid)
        AVarF v nid -> AVar v (redir nid)
        ALamF v pNode x bodyAnn nid ->
            ALam v (redir pNode) x bodyAnn (redir nid)
        AAppF fAnn argAnn funEid argEid nid ->
            AApp fAnn argAnn funEid argEid (redir nid)
        ALetF v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
            ALet v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn (redir nid)
        AAnnF exprAnn nid eid -> AAnn exprAnn (redir nid) eid

canonicalizeAnn :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
canonicalizeAnn canonical = cata alg
  where
    alg ann = case ann of
        ALitF l nid -> ALit l (canonical nid)
        AVarF v nid -> AVar v (canonical nid)
        ALamF v pNode x bodyAnn nid ->
            ALam v (canonical pNode) x bodyAnn (canonical nid)
        AAppF fAnn argAnn funEid argEid nid ->
            AApp fAnn argAnn funEid argEid (canonical nid)
        ALetF v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
            ALet v schemeGenId (canonical schemeRootId) ev rhsGen rhsAnn bodyAnn (canonical nid)
        AAnnF exprAnn nid eid -> AAnn exprAnn (canonical nid) eid

annNode :: AnnExpr -> NodeId
annNode = cata alg
  where
    alg ann = case ann of
        ALitF _ nid -> nid
        AVarF _ nid -> nid
        ALamF _ _ _ _ nid -> nid
        AAppF _ _ _ _ nid -> nid
        ALetF _ _ _ _ _ _ _ nid -> nid
        AAnnF _ nid _ -> nid

pruneBindParentsConstraint :: Constraint -> Constraint
pruneBindParentsConstraint c =
    let liveNodes = cNodes c
        liveGens = cGenNodes c
        liveChild childKey =
            case nodeRefFromKey childKey of
                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        liveParent ref =
            case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        bindParents' =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    liveChild childKey && liveParent parentRef
                )
                (cBindParents c)
    in c { cBindParents = bindParents' }

resolveBaseBoundForInst :: Constraint -> (NodeId -> NodeId) -> NodeId -> Maybe NodeId
resolveBaseBoundForInst constraint canonical start =
    let nodes = cNodes constraint
        go visited nid0 =
            let nid = canonical nid0
                key = getNodeId nid
            in if IntSet.member key visited
                then Nothing
                else
                    case IntMap.lookup key nodes of
                        Just TyBase{} -> Just nid
                        Just TyBottom{} -> Just nid
                        Just TyVar{} ->
                            case VarStore.lookupVarBound constraint nid of
                                Just bnd -> go (IntSet.insert key visited) bnd
                                Nothing -> Nothing
                        _ -> Nothing
    in go IntSet.empty start

inlineBaseBoundsType :: Constraint -> (NodeId -> NodeId) -> ElabType -> ElabType
inlineBaseBoundsType constraint canonical = cata alg
  where
    alg ty = case ty of
        TVarF v ->
            case parseName v of
                Just nid ->
                    case resolveBaseBoundForInst constraint canonical nid of
                        Just baseN ->
                            case IntMap.lookup (getNodeId baseN) (cNodes constraint) of
                                Just TyBase{ tnBase = b } -> TBase b
                                Just TyBottom{} -> TBottom
                                _ -> TVar v
                        Nothing -> TVar v
                Nothing -> TVar v
        TArrowF a b -> TArrow a b
        TForallF v mb body -> TForall v mb body
        TBaseF b -> TBase b
        TBottomF -> TBottom

inlineBoundVarsType :: SolveResult -> ElabType -> ElabType
inlineBoundVarsType res = go IntSet.empty
  where
    constraint = srConstraint res
    canonical = frWith (srUnionFind res)
    resolveBoundBody seen nid0 =
        let nid = canonical nid0
            key = getNodeId nid
        in if IntSet.member key seen
            then nid
            else case VarStore.lookupVarBound constraint nid of
                Just bnd -> resolveBoundBody (IntSet.insert key seen) bnd
                Nothing -> nid
    go seen ty = case ty of
        TVar v ->
            case parseName v of
                Just nid ->
                    let nidC = canonical nid
                        key = getNodeId nidC
                    in if IntSet.member key seen
                        then ty
                        else case VarStore.lookupVarBound constraint nidC of
                            Just bnd ->
                                let bndRoot = resolveBoundBody (IntSet.insert key seen) bnd
                                in case reifyType res bndRoot of
                                    Right t0 ->
                                        let t1 = inlineBaseBoundsType constraint canonical t0
                                        in go (IntSet.insert key seen) t1
                                    Left _ -> ty
                            Nothing -> ty
                Nothing -> ty
        TArrow a b -> TArrow (go seen a) (go seen b)
        TForall v mb body -> TForall v (fmap (go seen) mb) (go seen body)
        TBase _ -> ty
        TBottom -> ty

simplifyAnnotationType :: ElabType -> ElabType
simplifyAnnotationType = go
  where
    go ty = case ty of
        TVar _ -> ty
        TBase _ -> ty
        TBottom -> ty
        TArrow a b -> TArrow (go a) (go b)
        TForall{} ->
            normalizeForalls (stripForalls ty)

    stripForalls = para alg
      where
        alg ty = case ty of
            TForallF v mb bodyPair ->
                let mbOrig = fmap fst mb
                    (binds, inner) = snd bodyPair
                in ((v, mbOrig) : binds, inner)
            TVarF v -> ([], TVar v)
            TArrowF d c -> ([], TArrow (fst d) (fst c))
            TBaseF b -> ([], TBase b)
            TBottomF -> ([], TBottom)

    normalizeForalls (binds0, body0) =
        let binds1 =
                [ (v, fmap go mb)
                | (v, mb) <- binds0
                ]
            body1 = go body0
            (binds2, body2) = mergeBaseBounds binds1 body1
            (binds3, body3) = dropUnusedBinds binds2 body2
            ty = foldr (\(v, b) t -> TForall v b t) body3 binds3
        in inlineAlias ty

    mergeBaseBounds binds body =
        let baseKey bound = case bound of
                TBase b -> Just (Just b)
                TBottom -> Just Nothing
                _ -> Nothing
            usedInBounds =
                Set.unions
                    [ freeVarsType bnd
                    | (_, Just bnd) <- binds
                    ]
            goMerge _ [] body' = ([], body')
            goMerge seen ((v, mb):rest) body' =
                let mb' =
                        case mb of
                            Just (TVar v') | v' == v -> Nothing
                            _ -> mb
                    vUsed = Set.member v usedInBounds
                in case mb' >>= baseKey of
                    Just key ->
                        case Map.lookup key seen of
                            Just (rep, repUsed) ->
                                if repUsed
                                    then
                                        if vUsed
                                            then
                                                let rest' = map (substBind v rep) rest
                                                    body'' = substVar v rep body'
                                                in goMerge seen rest' body''
                                            else
                                                let rest' = map (substBindType v (baseFromKey key)) rest
                                                    body'' = substType v (baseFromKey key) body'
                                                in goMerge seen rest' body''
                                    else
                                        let rest' = map (substBind v rep) rest
                                            body'' = substVar v rep body'
                                            repUsed' = repUsed || vUsed
                                            seen' = Map.insert key (rep, repUsed') seen
                                        in goMerge seen' rest' body''
                            Nothing ->
                                let seen' = Map.insert key (v, vUsed) seen
                                    (rest', body'') = goMerge seen' rest body'
                                in ((v, mb') : rest', body'')
                    Nothing ->
                        let (rest', body'') = goMerge seen rest body'
                        in ((v, mb') : rest', body'')
        in goMerge Map.empty binds body

    baseFromKey key = case key of
        Just b -> TBase b
        Nothing -> TBottom

    dropUnusedBinds binds body =
        let freeInBound = maybe Set.empty freeVarsType
            used = Set.union (freeVarsType body)
                (Set.unions [ freeInBound mb | (_, mb) <- binds ])
            keep (v, mb) = Set.member v used || maybe False (Set.member v . freeVarsType) mb
        in (filter keep binds, body)

    inlineAlias ty = case ty of
        TForall v mb body ->
            let mb' = fmap go mb
                body' = go body
                mb'' = case mb' of
                    Just (TVar v') | v' == v -> Nothing
                    _ -> mb'
            in case (mb'', body') of
                (Just bound, TVar v') | v' == v && inlineAliasBound bound ->
                    bound
                _ -> TForall v mb'' body'
        _ -> ty

    inlineAliasBound bound = case bound of
        TArrow (TVar v1) (TVar v2) -> v1 == v2
        _ -> False

    substBind v v0 (name, mb) =
        let mb' = fmap (substVar v v0) mb
        in (name, mb')

    substBindType v replacement (name, mb) =
        let mb' = fmap (substType v replacement) mb
        in (name, mb')

    substVar v v0 = para alg
      where
        alg ty = case ty of
            TVarF name
                | name == v -> TVar v0
                | otherwise -> TVar name
            TArrowF d c -> TArrow (snd d) (snd c)
            TBaseF b -> TBase b
            TBottomF -> TBottom
            TForallF name mb body
                | name == v -> TForall name (fmap fst mb) (fst body)
                | otherwise -> TForall name (fmap snd mb) (snd body)

    substType v replacement = para alg
      where
        alg ty = case ty of
            TVarF name
                | name == v -> replacement
                | otherwise -> TVar name
            TArrowF d c -> TArrow (snd d) (snd c)
            TBaseF b -> TBase b
            TBottomF -> TBottom
            TForallF name mb body
                | name == v -> TForall name (fmap fst mb) (fst body)
                | otherwise -> TForall name (fmap snd mb) (snd body)

    freeVarsType = freeVarsFrom Set.empty
      where
        freeVarsFrom bound ty = (cata alg ty) bound
        alg ty = case ty of
            TVarF v ->
                \bound' ->
                    if Set.member v bound'
                        then Set.empty
                        else Set.singleton v
            TArrowF a b -> \bound' -> Set.union (a bound') (b bound')
            TBaseF _ -> const Set.empty
            TBottomF -> const Set.empty
            TForallF v mb body ->
                \bound' ->
                    let bound'' = Set.insert v bound'
                        freeBound = maybe Set.empty ($ bound'') mb
                        freeBody = body bound''
                    in Set.union freeBound freeBody

parseName :: String -> Maybe NodeId
parseName name =
    case name of
        ('t':rest) ->
            case reads rest of
                [(n, "")] -> Just (NodeId n)
                _ -> Nothing
        _ -> Nothing

inferInstAppArgsFromScheme :: [(String, Maybe ElabType)] -> ElabType -> ElabType -> Maybe [ElabType]
inferInstAppArgsFromScheme binds body targetTy =
    let binderNames = map fst binds
        binderSet = Set.fromList binderNames
        targetCore = stripForallsType targetTy
        targetForallNames =
            let alg ty = case ty of
                    TForallF v _ body' -> v : body'
                    _ -> []
            in cata alg targetTy
        argsAreIdentity names args =
            and
                [ case arg of
                    TVar v -> v == name || elem v targetForallNames
                    _ -> False
                | (name, arg) <- zip names args
                ]
        fallback =
            case matchType binderSet body targetCore of
                Left _ -> Nothing
                Right subst ->
                    let present = map (\name -> Map.member name subst) binderNames
                        prefixLen = length (takeWhile id present)
                        hasOutOfOrder = or (drop prefixLen present)
                        prefixNames = take prefixLen binderNames
                        args = [ty | name <- prefixNames, Just ty <- [Map.lookup name subst]]
                    in if hasOutOfOrder
                        then Nothing
                        else if argsAreIdentity prefixNames args
                            then Nothing
                            else Just args
        inferFromBound v bound =
            let boundCore = stripForallsType bound
                matchVars = varsInType boundCore
            in case matchType matchVars boundCore targetCore of
                Left _ -> fallback
                Right subst ->
                    let innerVars = Set.difference matchVars binderSet
                        pickInnerArg =
                            case Set.toList innerVars of
                                [inner] -> Map.lookup inner subst
                                _ -> Nothing
                        argFor name =
                            if name == v
                                then
                                    case pickInnerArg of
                                        Just innerArg -> Just innerArg
                                        Nothing -> Just (substTypeSelective binderSet subst boundCore)
                                else Map.lookup name subst
                        argsMaybe = map argFor binderNames
                        present = map (\arg -> case arg of { Just _ -> True; Nothing -> False }) argsMaybe
                        prefixLen = length (takeWhile id present)
                        hasOutOfOrder = or (drop prefixLen present)
                        prefixArgs = take prefixLen argsMaybe
                        args = [ty | Just ty <- prefixArgs]
                        prefixNames = take prefixLen binderNames
                    in if hasOutOfOrder
                        then Nothing
                        else if argsAreIdentity prefixNames args
                            then Nothing
                            else Just args
    in case body of
        TVar v ->
            case lookup v binds of
                Just (Just bound) -> inferFromBound v bound
                _ -> fallback
        _ -> fallback

varsInType :: ElabType -> Set.Set String
varsInType = cata alg
  where
    alg ty = case ty of
        TVarF v -> Set.singleton v
        TArrowF a b -> Set.union a b
        TBaseF _ -> Set.empty
        TBottomF -> Set.empty
        TForallF _ mb body ->
            let varsBound = maybe Set.empty id mb
            in Set.union varsBound body

substTypeSelective :: Set.Set String -> Map.Map String ElabType -> ElabType -> ElabType
substTypeSelective binderSet subst ty0 = (cata alg ty0) Set.empty
  where
    alg ty = case ty of
        TVarF v ->
            \bound ->
                if Set.member v bound || Set.member v binderSet
                    then TVar v
                    else case Map.lookup v subst of
                        Just ty' -> ty'
                        Nothing -> TVar v
        TArrowF a b -> \bound -> TArrow (a bound) (b bound)
        TBaseF b -> const (TBase b)
        TBottomF -> const TBottom
        TForallF v mb body ->
            \bound ->
                let bound' = Set.insert v bound
                    mb' = fmap ($ bound') mb
                    body' = body bound'
                in TForall v mb' body'

instInsideFromArgsWithBounds :: [(String, Maybe ElabType)] -> [ElabType] -> Instantiation
instInsideFromArgsWithBounds binds args = case (binds, args) of
    ([], _) -> InstId
    (_, []) -> InstId
    ((n, mbBound):ns, t:ts) ->
        let rest = instInsideFromArgsWithBounds ns ts
            inst =
                case mbBound of
                    Just bound | containsForallType bound -> InstInside (InstApp t)
                    _ -> InstInside (InstBot t)
        in if rest == InstId
            then inst
            else InstSeq inst (InstUnder n rest)

containsForallType :: ElabType -> Bool
containsForallType = cata alg
  where
    alg ty = case ty of
        TForallF _ _ _ -> True
        TArrowF a b -> a || b
        _ -> False

stripForallsType :: ElabType -> ElabType
stripForallsType = para alg
  where
    alg ty = case ty of
        TForallF _ _ body -> snd body
        TVarF v -> TVar v
        TArrowF (d, _) (c, _) -> TArrow d c
        TBaseF b -> TBase b
        TBottomF -> TBottom

matchType
    :: Set.Set String
    -> ElabType
    -> ElabType
    -> Either ElabError (Map.Map String ElabType)
matchType binderSet = goMatch Map.empty Map.empty
  where
    goMatch env subst tyP tyT = case (tyP, tyT) of
        (TVar a, _) ->
            if a `Set.member` binderSet
                then case Map.lookup a subst of
                    Just ty -> if alphaEqType ty tyT then Right subst else Left (InstantiationError "matchType: mismatch")
                    Nothing -> Right (Map.insert a tyT subst)
                else
                    if alphaEqType tyP tyT
                        then Right subst
                        else Left (InstantiationError "matchType: mismatch")
        (TArrow a b, TArrow a' b') -> do
            subst1 <- goMatch env subst a a'
            goMatch env subst1 b b'
        (TBase b0, TBase b1)
            | b0 == b1 -> Right subst
            | otherwise -> Left (InstantiationError "matchType: base mismatch")
        (TBottom, TBottom) -> Right subst
        (TForall v mb body, TForall v' mb' body') -> do
            subst1 <- maybeMatch env subst mb mb'
            let env' = Map.insert v v' env
            goMatch env' subst1 body body'
        _ -> Left (InstantiationError "matchType: constructor mismatch")

    maybeMatch env subst a b = case (a, b) of
        (Nothing, Nothing) -> Right subst
        (Just x, Just y) -> goMatch env subst x y
        _ -> Left (InstantiationError "matchType: bound mismatch")

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = goEq Map.empty Map.empty
  where
    goEq envL envR t1 t2 = case (t1, t2) of
        (TVar a, TVar b) ->
            case (Map.lookup a envL, Map.lookup b envR) of
                (Just a', Just b') -> a' == b'
                (Nothing, Nothing) -> a == b
                _ -> False
        (TArrow a b, TArrow a' b') -> goEq envL envR a a' && goEq envL envR b b'
        (TBase b0, TBase b1) -> b0 == b1
        (TBottom, TBottom) -> True
        (TForall v mb b, TForall v' mb' b') ->
            let envL' = Map.insert v v' envL
                envR' = Map.insert v' v envR
            in maybeEq envL envR mb mb' && goEq envL' envR' b b'
        _ -> False

    maybeEq envL envR a b = case (a, b) of
        (Nothing, Nothing) -> True
        (Just x, Just y) -> goEq envL envR x y
        _ -> False

-- | Chase redirects through the map until stable or missing
chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects redirects nid =
    let go seen n = case IntMap.lookup (getNodeId n) redirects of
            Just n'
                | n' == n -> n
                | IntSet.member (getNodeId n') seen -> n
                | otherwise -> go (IntSet.insert (getNodeId n') seen) n'
            Nothing -> n
    in go (IntSet.singleton (getNodeId nid)) nid

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right

-- | Annotation instantiation should preserve quantifiers by updating bounds
-- rather than eliminating binders. Drop eliminations and treat applications
-- as inside-bound updates.
adjustAnnotationInst :: Instantiation -> Instantiation
adjustAnnotationInst inst = case inst of
    InstApp ty -> InstInside (InstBot ty)
    InstElim -> InstId
    InstSeq a b ->
        let a' = adjustAnnotationInst a
            b' = adjustAnnotationInst b
        in case (a', b') of
            (InstId, x) -> x
            (x, InstId) -> x
            _ -> InstSeq a' b'
    InstInside a -> InstInside (adjustAnnotationInst a)
    InstUnder v a -> InstUnder v (adjustAnnotationInst a)
    _ -> inst

bindingScopeRef :: Constraint -> NodeId -> Either BindingError NodeRef
bindingScopeRef constraint root = do
    path <- Binding.bindingPathToRoot constraint (typeRef root)
    case listToMaybe [gid | GenRef gid <- drop 1 path] of
        Just gid -> Right (GenRef gid)
        Nothing -> Right (TypeRef root)

preferGenScope :: Constraint -> NodeRef -> NodeRef
preferGenScope constraint ref = case ref of
    GenRef _ -> ref
    TypeRef nid ->
        case Binding.bindingPathToRoot constraint (typeRef nid) of
            Right path ->
                case listToMaybe [gid | GenRef gid <- drop 1 path] of
                    Just gid -> GenRef gid
                    Nothing -> ref
            Left _ -> ref

schemeBodyTarget :: SolveResult -> NodeId -> NodeId
schemeBodyTarget res target =
    let constraint = srConstraint res
        canonical = frWith (srUnionFind res)
        targetC = canonical target
        isSchemeRoot =
            any
                (\gen -> any (\root -> canonical root == targetC) (gnSchemes gen))
                (IntMap.elems (cGenNodes constraint))
        schemeRootByBody =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | gen <- IntMap.elems (cGenNodes constraint)
                , root <- gnSchemes gen
                , Just bnd <- [VarStore.lookupVarBound constraint root]
                , case IntMap.lookup (getNodeId (canonical bnd)) (cNodes constraint) of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
    in case IntMap.lookup (getNodeId targetC) (cNodes constraint) of
        Just TyVar{ tnBound = Just bnd } ->
            let bndC = canonical bnd
                boundIsSchemeBody = IntMap.member (getNodeId bndC) schemeRootByBody
            in if isSchemeRoot || boundIsSchemeBody
                then
                    case IntMap.lookup (getNodeId bndC) (cNodes constraint) of
                        Just TyForall{ tnBody = body } -> canonical body
                        _ -> bndC
                else targetC
        Just TyForall{ tnBody = body } -> canonical body
        _ -> targetC

canonicalizeScopeRef :: SolveResult -> IntMap.IntMap NodeId -> NodeRef -> NodeRef
canonicalizeScopeRef solved redirects scopeRef =
    case scopeRef of
        GenRef gid -> GenRef gid
        TypeRef nid ->
            let canonical = frWith (srUnionFind solved)
            in TypeRef (canonical (chaseRedirects redirects nid))

instantiationCopyNodes :: SolveResult -> IntMap.IntMap NodeId -> IntMap.IntMap EdgeTrace -> IntSet.IntSet
instantiationCopyNodes solved redirects edgeTraces =
    let canonical = frWith (srUnionFind solved)
        adoptNode nid = canonical (chaseRedirects redirects nid)
        collectTrace tr =
            let copyRaw =
                    [ getNodeId node
                    | node <- IntMap.elems (etCopyMap tr)
                    ]
                copyCanon =
                    [ getNodeId (adoptNode node)
                    | node <- IntMap.elems (etCopyMap tr)
                    ]
                interiorRaw =
                    [ nid
                    | nid <- IntSet.toList (etInterior tr)
                    ]
                interiorCanon =
                    [ getNodeId (adoptNode (NodeId nid))
                    | nid <- IntSet.toList (etInterior tr)
                    ]
                rootRaw = getNodeId (etRoot tr)
                rootCanon = getNodeId (adoptNode (etRoot tr))
            in IntSet.fromList (rootRaw : rootCanon : copyRaw ++ copyCanon ++ interiorRaw ++ interiorCanon)
    in IntSet.unions (map collectTrace (IntMap.elems edgeTraces))

constraintForGeneralization :: SolveResult -> IntMap.IntMap NodeId -> IntSet.IntSet -> IntMap.IntMap NodeId -> Constraint -> AnnExpr -> (Constraint, GaBindParents)
constraintForGeneralization solved redirects instCopyNodes instCopyMap base _ann =
    let solvedConstraint = srConstraint solved
        canonical = frWith (srUnionFind solved)
        nodesSolved0 = cNodes solvedConstraint
        schemeRootsBase =
            [ root
            | gen <- IntMap.elems (cGenNodes base)
            , root <- gnSchemes gen
            ]
        restoreSchemeRoot acc root =
            let mbBase =
                    case IntMap.lookup (getNodeId root) (cNodes base) of
                        Just TyVar{ tnBound = mb } ->
                            case mb of
                                Nothing -> Nothing
                                Just bnd ->
                                    case adoptRef (typeRef bnd) of
                                        TypeRef bnd' -> Just bnd'
                                        GenRef _ -> Nothing
                        _ -> Nothing
            in case IntMap.lookup (getNodeId root) acc of
                Nothing ->
                    case mbBase of
                        Just bnd' ->
                            let rootNode = TyVar { tnId = root, tnBound = Just bnd' }
                            in IntMap.insert (getNodeId root) rootNode acc
                        Nothing -> acc
                Just TyVar{ tnId = nid, tnBound = Nothing } ->
                    case mbBase of
                        Just bnd' ->
                            IntMap.insert (getNodeId root) (TyVar { tnId = nid, tnBound = Just bnd' }) acc
                        Nothing -> acc
                Just _ -> acc
        schemeRootsBaseSet =
            IntSet.fromList (map getNodeId schemeRootsBase)
        schemeRootsSolvedSet =
            IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- IntMap.elems (cGenNodes solvedConstraint)
                , root <- gnSchemes gen
                ]
        schemeRootsAllSet =
            IntSet.union schemeRootsBaseSet schemeRootsSolvedSet
        nodesSolved1 = foldl' restoreSchemeRoot nodesSolved0 schemeRootsBase
        nodesSolved =
            let
                nodesSolvedBaseAdjusted =
                    IntMap.foldlWithKey'
                        (\acc key node ->
                            case node of
                                TyVar{ tnBound = Just bndBase } ->
                                    if IntSet.member (getNodeId bndBase) schemeRootsBaseSet
                                        then
                                            case applyRedirectsToRef (typeRef bndBase) of
                                                TypeRef bnd' ->
                                                    IntMap.adjust
                                                        (\n -> case n of
                                                            TyVar{} -> n { tnBound = Just bnd' }
                                                            _ -> n
                                                        )
                                                        key
                                                        acc
                                                GenRef _ -> acc
                                        else acc
                                _ -> acc
                        )
                        nodesSolved1
                        (cNodes base)
                adoptNodeId nid =
                    case adoptRef (typeRef nid) of
                        TypeRef nid' -> nid'
                        GenRef _ -> nid
                restoreNamedVars acc =
                    let baseNodes = cNodes base
                        preferBaseVar new old =
                            case (new, old) of
                                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                                (TyVar{}, TyVar{}) -> new
                                _ -> old
                        insertNamed acc' childKey _parentRef =
                            case IntMap.lookup childKey baseNodes of
                                Just TyVar{ tnId = baseId, tnBound = mb } ->
                                    let baseId' = baseId
                                        mb' = fmap adoptNodeId mb
                                        node' = TyVar { tnId = baseId', tnBound = mb' }
                                    in IntMap.insertWith
                                        preferBaseVar
                                        (getNodeId baseId')
                                        node'
                                        acc'
                                _ -> acc'
                    in IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, _flag) ->
                            insertNamed acc' childKey parentRef
                        )
                        acc
                        (cBindParents base)
                restoreSchemeInteriorVars acc =
                    let
                        baseNodes = cNodes base
                        reachableFromWithBoundsBaseLocal start =
                            let go visited [] = visited
                                go visited (nid0:rest) =
                                    let key = getNodeId nid0
                                    in if IntSet.member key visited
                                        then go visited rest
                                        else
                                            let visited' = IntSet.insert key visited
                                                kids =
                                                    case IntMap.lookup key baseNodes of
                                                        Nothing -> []
                                                        Just node ->
                                                            let boundKids =
                                                                    case node of
                                                                        TyVar{ tnBound = Just bnd } -> [bnd]
                                                                        _ -> []
                                                            in structuralChildren node ++ boundKids
                                            in go visited' (kids ++ rest)
                            in go IntSet.empty [start]
                        schemeInteriorsBase =
                            IntSet.unions
                                [ reachableFromWithBoundsBaseLocal root
                                | gen <- IntMap.elems (cGenNodes base)
                                , root <- gnSchemes gen
                                ]
                        preferBaseVar new old =
                            case (new, old) of
                                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                                (TyVar{}, TyVar{}) -> new
                                _ -> old
                        insertVarFromBase acc' key =
                            if IntSet.member key schemeRootsBaseSet
                                then acc'
                                else
                                    case IntMap.lookup key baseNodes of
                                        Just TyVar{ tnId = baseId, tnBound = mb } ->
                                            let baseId' = adoptNodeId baseId
                                                mb' = fmap adoptNodeId mb
                                                node' = TyVar { tnId = baseId', tnBound = mb' }
                                            in IntMap.insertWith
                                                preferBaseVar
                                                (getNodeId baseId')
                                                node'
                                                acc'
                                        _ -> acc'
                    in IntSet.foldl' insertVarFromBase acc schemeInteriorsBase
                restoreBindParentVars acc =
                    let
                        baseNodes = cNodes base
                        preferBaseVar new old =
                            case (new, old) of
                                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                                (TyVar{}, TyVar{}) -> new
                                _ -> old
                        parentKeys =
                            [ getNodeId parent
                            | (_childKey, (TypeRef parent, _flag)) <- IntMap.toList (cBindParents base)
                            ]
                        childKeys =
                            [ getNodeId child
                            | (childKey, _parent) <- IntMap.toList (cBindParents base)
                            , TypeRef child <- [nodeRefFromKey childKey]
                            ]
                        keys = IntSet.fromList (parentKeys ++ childKeys)
                        insertVarFromBase acc' key =
                            case IntMap.lookup key baseNodes of
                                Just TyVar{ tnId = baseId, tnBound = mb } ->
                                    let baseId' = adoptNodeId baseId
                                        mb' = fmap adoptNodeId mb
                                        node' = TyVar { tnId = baseId', tnBound = mb' }
                                    in IntMap.insertWith
                                        preferBaseVar
                                        (getNodeId baseId')
                                        node'
                                        acc'
                                _ -> acc'
                    in IntSet.foldl' insertVarFromBase acc keys
            in restoreBindParentVars (restoreSchemeInteriorVars (restoreNamedVars nodesSolvedBaseAdjusted))
        genSolved = cGenNodes solvedConstraint
        genMerged = IntMap.union (cGenNodes base) genSolved
        applyRedirectsToRef ref =
            case ref of
                TypeRef nid -> TypeRef (chaseRedirects redirects nid)
                GenRef gid -> GenRef gid
        canonicalRef ref =
            case ref of
                TypeRef nid -> TypeRef (canonical nid)
                GenRef gid -> GenRef gid
        adoptRef = canonicalRef . applyRedirectsToRef
        okRef ref =
            case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) nodesSolved
                GenRef gid -> IntMap.member (genNodeKey gid) genMerged
        upperConstraint =
            solvedConstraint { cNodes = nodesSolved }
        isUpperRef parentRef childRef =
            case parentRef of
                TypeRef _ -> Binding.isUpper upperConstraint parentRef childRef
                GenRef _ -> True
        wasRedirected ref =
            case ref of
                TypeRef nid ->
                    let ref' = applyRedirectsToRef ref
                    in nodeRefKey ref' /= nodeRefKey (TypeRef nid)
                GenRef _ -> False
        bindParentsBase = cBindParents base
        stickyTypeParentsBase =
            IntSet.fromList
                [ childKey
                | (childKey, (parentRef, _flag)) <- IntMap.toList bindParentsBase
                , case parentRef of
                    TypeRef _ -> True
                    _ -> False
                ]
        baseNamedKeys =
            IntSet.fromList
                [ childKey
                | (childKey, _parentRef) <- IntMap.toList bindParentsBase
                , TypeRef child <- [nodeRefFromKey childKey]
                , case IntMap.lookup (getNodeId child) (cNodes base) of
                    Just TyVar{} -> True
                    _ -> False
                ]
        bindParentsSolved = cBindParents solvedConstraint
        insertBindParentBase acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                childKey' = nodeRefKey childRef'
                allowParent =
                    case parentRef of
                        TypeRef _ -> True
                        GenRef _ -> isUpperRef parentRef' childRef'
            in if not (okRef childRef' && okRef parentRef')
                then acc
            else if nodeRefKey childRef' == nodeRefKey parentRef'
                then acc
            else if not allowParent
                then acc
            else IntMap.insertWith (\_ old -> old) childKey' (parentRef', flag) acc

        insertBindParentSolved acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                childKey' = nodeRefKey childRef'
                isSelf = nodeRefKey childRef' == nodeRefKey parentRef'
                existing = IntMap.lookup childKey' acc
                childIsCopy =
                    case childRef' of
                        TypeRef nid -> IntSet.member (getNodeId nid) instCopyNodes
                        GenRef _ -> False
                existingSelf =
                    case existing of
                        Just (parentExisting, _) -> nodeRefKey parentExisting == childKey'
                        Nothing -> False
            in if not (okRef childRef' && okRef parentRef')
                then acc
            else if isSelf
                then acc
            else if not (isUpperRef parentRef' childRef')
                then acc
            else case existing of
                Just _ | childIsCopy && not existingSelf -> acc
                Just _ | childIsCopy ->
                    debugGaScope
                        ("constraintForGeneralization: bind-parent override copy child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                Nothing ->
                    debugGaScope
                        ("constraintForGeneralization: bind-parent fill child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                Just _ | existingSelf ->
                    debugGaScope
                        ("constraintForGeneralization: bind-parent override self-parent child="
                            ++ show childRef'
                            ++ " parent="
                            ++ show parentRef'
                            ++ " redirected="
                            ++ show (wasRedirected childRef)
                        )
                        (IntMap.insert childKey' (parentRef', flag) acc)
                _ -> acc

        bindParentsBase' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentBase acc childKey parentRef flag
                )
                IntMap.empty
                bindParentsBase
        baseToSolved =
            let baseNodes = cNodes base
                baseReachableFromWithBounds start =
                    let go visited [] = visited
                        go visited (nid0:rest) =
                            let key = getNodeId nid0
                            in if IntSet.member key visited
                                then go visited rest
                                else
                                    let visited' = IntSet.insert key visited
                                        kids =
                                            case IntMap.lookup key baseNodes of
                                                Nothing -> []
                                                Just node ->
                                                    let boundKids =
                                                            case node of
                                                                TyVar{ tnBound = Just bnd } -> [bnd]
                                                                _ -> []
                                                    in structuralChildren node ++ boundKids
                                    in go visited' (kids ++ rest)
                    in go IntSet.empty [start]
                schemeInteriorKeys =
                    IntSet.unions
                        [ baseReachableFromWithBounds root
                        | gen <- IntMap.elems (cGenNodes base)
                        , root <- gnSchemes gen
                        ]
                bindParentKeys =
                    IntSet.fromList
                        ( [ getNodeId child
                          | (childKey, _parent) <- IntMap.toList bindParentsBase
                          , TypeRef child <- [nodeRefFromKey childKey]
                          ]
                            ++ [ getNodeId parent
                               | (_childKey, (TypeRef parent, _flag)) <- IntMap.toList bindParentsBase
                               ]
                        )
                baseKeys =
                    IntSet.unions
                        [ IntSet.fromList (IntMap.keys baseNodes)
                        , schemeInteriorKeys
                        , bindParentKeys
                        ]
                adoptNode nid0 =
                    case adoptRef (typeRef (NodeId nid0)) of
                        TypeRef nid' -> nid'
                        GenRef _ -> NodeId nid0
                chooseMapping nid0 =
                    let adopted = adoptNode nid0
                        adoptedKey = getNodeId adopted
                        baseNodeExists = IntMap.member nid0 nodesSolved
                        adoptedExists = IntMap.member adoptedKey nodesSolved
                        baseIsNamed = IntSet.member nid0 baseNamedKeys
                        baseIsEliminated = VarStore.isEliminatedVar solvedConstraint (NodeId nid0)
                    in if baseIsNamed && baseNodeExists && not baseIsEliminated
                        then NodeId nid0
                        else if adoptedExists
                            then adopted
                            else if baseNodeExists
                                then NodeId nid0
                                else adopted
            in IntMap.fromList
                [ (nid0, chooseMapping nid0)
                | nid0 <- IntSet.toList baseKeys
                ]
        solvedToBase0 =
            IntMap.foldlWithKey'
                (\acc baseKey solvedNid ->
                    let solvedKeyC = getNodeId (canonical solvedNid)
                        solvedKeyRaw = getNodeId solvedNid
                        acc' = IntMap.insertWith (\_ old -> old) solvedKeyC (NodeId baseKey) acc
                    in IntMap.insertWith (\_ old -> old) solvedKeyRaw (NodeId baseKey) acc'
                )
                IntMap.empty
                baseToSolved
        solvedToBase =
            let copyOverrides =
                    IntMap.fromList
                        [ (copyKeyC, baseN)
                        | (copyKey, baseN) <- IntMap.toList instCopyMap
                        , let copyKeyC = getNodeId (canonical (NodeId copyKey))
                        , IntMap.member copyKeyC nodesSolved
                        ]
            in IntMap.union copyOverrides solvedToBase0

        bindParents' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentSolved acc childKey parentRef flag
                )
                bindParentsBase'
                bindParentsSolved
        bindParentsWithCopies =
            IntMap.foldlWithKey'
                (\acc copyKey baseN ->
                    let childRef = typeRef (NodeId copyKey)
                        childRef' = adoptRef childRef
                        childKey' = nodeRefKey childRef'
                    in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                        Just (parentRef, flag) ->
                            let parentRef' = adoptRef parentRef
                            in if not (okRef childRef' && okRef parentRef')
                                then acc
                                else if nodeRefKey childRef' == nodeRefKey parentRef'
                                    then acc
                                    else IntMap.insertWith (\_ old -> old) childKey' (parentRef', flag) acc
                        Nothing -> acc
                )
                bindParents'
                instCopyMap
        schemeRootOwners =
            IntMap.fromList
                [ (getNodeId (canonical root), gnId gen)
                | gen <- IntMap.elems genMerged
                , root <- gnSchemes gen
                ]
        schemeRootOwnersBase =
            IntMap.filterWithKey
                (\k _ -> IntSet.member k schemeRootsAllSet)
                schemeRootOwners
        schemeRootOwnersFromBase =
            IntMap.fromList
                [ (getNodeId (canonical solvedRoot), gid)
                | root <- schemeRootsBase
                , Just gid <- [baseFirstGenAncestor (typeRef root)]
                , Just (TypeRef solvedRoot) <- [mapBaseRef (typeRef root)]
                ]
        allSchemeRoots =
            IntSet.fromList
                (IntMap.keys schemeRootOwnersBase)
        reachableFromWithBoundsStop start =
            let stopSet = allSchemeRoots
                shouldStop nid = IntSet.member (getNodeId nid) stopSet
                children nid =
                    case IntMap.lookup (getNodeId nid) nodesSolved of
                        Nothing -> []
                        Just node ->
                            let boundKids =
                                    case node of
                                        TyVar{ tnBound = Just bnd } -> [bnd]
                                        _ -> []
                            in structuralChildren node ++ boundKids
            in reachableFromStop getNodeId canonical children shouldStop start
        reachableFromWithBoundsBaseStop start =
            let baseNodes = cNodes base
                stopSet = schemeRootsBaseSet
                adoptNodeId nid =
                    case adoptRef (typeRef nid) of
                        TypeRef nid' -> nid'
                        GenRef _ -> nid
                startKey = getNodeId start
                go _ acc [] = acc
                go visited acc (nid0:rest) =
                    let key = getNodeId nid0
                    in if IntSet.member key visited
                        then go visited acc rest
                        else
                            let visited' = IntSet.insert key visited
                            in if key /= startKey && IntSet.member key stopSet
                                then go visited' acc rest
                                else
                                    let acc' = IntSet.insert (getNodeId (adoptNodeId nid0)) acc
                                        kids =
                                            case IntMap.lookup key baseNodes of
                                                Nothing -> []
                                                Just node ->
                                                    let boundKids =
                                                            case node of
                                                                TyVar{ tnBound = Just bnd } -> [bnd]
                                                                _ -> []
                                                    in structuralChildren node ++ boundKids
                                    in go visited' acc' (kids ++ rest)
            in go IntSet.empty IntSet.empty [start]
        boundSchemeRoots =
            IntSet.fromList
                [ getNodeId (canonical bnd)
                | node <- IntMap.elems nodesSolved
                , TyVar{ tnBound = Just bnd } <- [node]
                , IntSet.member (getNodeId (canonical bnd)) allSchemeRoots
                ]
        schemeRootOwnersFiltered =
            IntMap.union schemeRootOwnersFromBase schemeRootOwnersBase
        schemeRootByBodySolved =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | gen <- IntMap.elems genMerged
                , root <- gnSchemes gen
                , Just bnd <- [VarStore.lookupVarBound upperConstraint root]
                , case IntMap.lookup (getNodeId (canonical bnd)) nodesSolved of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
        schemeInteriorOwnersFiltered =
            IntMap.fromListWith
                (\_ old -> old)
                [ (nidInt, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFiltered
                , nidInt <- IntSet.toList (reachableFromWithBoundsStop (NodeId rootKey))
                , case IntMap.lookup nidInt nodesSolved of
                    Just TyVar{} -> True
                    _ -> False
                ]
        schemeInteriorOwnersBase =
            IntMap.fromListWith
                (\_ old -> old)
                [ (getNodeId nid, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFiltered
                , nid <- map NodeId (IntSet.toList (reachableFromWithBoundsBaseStop (NodeId rootKey)))
                , okRef (typeRef nid)
                , case IntMap.lookup (getNodeId nid) nodesSolved of
                    Just TyVar{} -> True
                    _ -> False
                ]
        schemeInteriorOwnersFiltered' =
            IntMap.union schemeInteriorOwnersFiltered schemeInteriorOwnersBase
        _ =
            debugGaScope
                ("constraintForGeneralization: schemeRootOwnersFiltered="
                    ++ show (IntMap.toList schemeRootOwnersFiltered)
                )
                ()
        allSchemeRootsFiltered =
            IntSet.fromList (IntMap.keys schemeRootOwnersFiltered)
        bindParentsWithParentOwners =
            let attachParent acc nidInt gid =
                    case IntMap.lookup nidInt nodesSolved of
                        Just TyVar{} ->
                            let childRef = typeRef (NodeId nidInt)
                                childKey = nodeRefKey childRef
                                setChildParent parentRef flag acc' =
                                    if okRef parentRef
                                        then IntMap.insertWith
                                            (\(parentNew, flagNew) (_parentOld, flagOld) ->
                                                (parentNew, max flagNew flagOld)
                                            )
                                            childKey
                                            (parentRef, flag)
                                            acc'
                                        else acc'
                            in case IntMap.lookup nidInt bindParentsWithCopies of
                                Just (GenRef gidParent, flag)
                                    | gidParent /= gid ->
                                        setChildParent (GenRef gid) flag acc
                                Just (TypeRef parentN, _flag) ->
                                    let parentRef = typeRef parentN
                                        parentKey = nodeRefKey parentRef
                                    in if okRef parentRef
                                        then
                                            case IntMap.lookup parentKey acc of
                                                Nothing ->
                                                    IntMap.insert parentKey (GenRef gid, BindFlex) acc
                                                Just (parentExisting, flag)
                                                    | nodeRefKey parentExisting == parentKey ->
                                                        IntMap.insert parentKey (GenRef gid, flag) acc
                                                    | otherwise -> acc
                                        else acc
                                Nothing ->
                                    setChildParent (GenRef gid) BindFlex acc
                                _ -> acc
                        _ -> acc
            in IntMap.foldlWithKey' attachParent bindParentsWithCopies schemeInteriorOwnersFiltered'
        baseFirstGenAncestor ref0 =
            let baseRef =
                    case ref0 of
                        GenRef gid -> GenRef gid
                        TypeRef nid ->
                            case IntMap.lookup (getNodeId nid) solvedToBase of
                                Just baseN -> typeRef baseN
                                Nothing -> typeRef nid
                go visited ref =
                    if IntSet.member (nodeRefKey ref) visited
                        then Nothing
                        else
                            case IntMap.lookup (nodeRefKey ref) bindParentsBase of
                                Nothing -> Nothing
                                Just (parentRef, _) ->
                                    case parentRef of
                                        GenRef gid -> Just gid
                                        TypeRef parentN ->
                                            go
                                                (IntSet.insert (nodeRefKey ref) visited)
                                                (typeRef parentN)
            in go IntSet.empty baseRef
        firstGenAncestorWith bindParents0 ref0 =
            case bindingPathToRootLocal bindParents0 ref0 of
                Left _ -> Nothing
                Right path -> listToMaybe [gid | GenRef gid <- drop 1 path]
        bindingPathToRootLocal bindParents0 start =
            let go visited path key
                    | IntSet.member key visited = Left (BindingTreeError (BindingCycleDetected (reverse path)))
                    | otherwise =
                        case IntMap.lookup key bindParents0 of
                            Nothing -> Right (reverse path)
                            Just (parentRef, _) ->
                                go (IntSet.insert key visited) (parentRef : path) (nodeRefKey parentRef)
            in go IntSet.empty [start] (nodeRefKey start)
        dropSelfParents bp =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    nodeRefKey parentRef /= childKey
                )
                bp
        mapBaseRef ref =
            case ref of
                GenRef gid -> Just (GenRef gid)
                TypeRef nid ->
                    case IntMap.lookup (getNodeId nid) baseToSolved of
                        Just solvedNid -> Just (TypeRef (canonical solvedNid))
                        Nothing ->
                            if IntMap.member (getNodeId nid) nodesSolved
                                then Just (TypeRef (canonical nid))
                                else Nothing
        restoreSchemeRootParentsFromBase bp0 =
            let baseRootAncestor root =
                    firstGenAncestorWith bindParentsBase (typeRef root)
                attachOne acc root =
                    case baseRootAncestor root of
                        Just gid ->
                            case IntMap.lookup (getNodeId root) baseToSolved of
                                Just solvedRoot ->
                                    let childKey = nodeRefKey (typeRef solvedRoot)
                                    in IntMap.insert childKey (GenRef gid, BindFlex) acc
                                Nothing -> acc
                        Nothing -> acc
            in foldl' attachOne bp0 schemeRootsBase
        restoreSchemeRootParentsByBody bp0 =
            let attachOne acc rootKey =
                    let root = NodeId rootKey
                    in case VarStore.lookupVarBound upperConstraint root of
                        Just bnd ->
                            let bodyRef = typeRef (canonical bnd)
                            in case firstGenAncestorWith acc bodyRef of
                                Just gid ->
                                    IntMap.insert (nodeRefKey (typeRef root)) (GenRef gid, BindFlex) acc
                                Nothing -> acc
                        Nothing -> acc
            in foldl' attachOne bp0 (IntSet.toList allSchemeRoots)
        preserveBaseGaPrime bp0 =
            let insertEdge acc childRef parentRef flag =
                    let childKey = nodeRefKey childRef
                    in if not (okRef childRef && okRef parentRef)
                        then acc
                    else if nodeRefKey childRef == nodeRefKey parentRef
                        then acc
                    else if not (isUpperRef parentRef childRef)
                        then acc
                    else IntMap.insert childKey (parentRef, flag) acc
                prefixToGen gid =
                    let go acc [] = reverse acc
                        go acc (ref:rest) =
                            let acc' = ref : acc
                            in case ref of
                                GenRef gid' | gid' == gid -> reverse acc'
                                _ -> go acc' rest
                    in go []
                applyPath acc path =
                    foldl'
                        (\acc' (childRef, parentRef) ->
                            case (mapBaseRef childRef, mapBaseRef parentRef) of
                                (Just childRef', Just parentRef') ->
                                    case IntMap.lookup (nodeRefKey childRef) bindParentsBase of
                                        Just (_parentBase, flag) ->
                                            insertEdge acc' childRef' parentRef' flag
                                        Nothing -> acc'
                                _ -> acc'
                        )
                        acc
                        (zip path (drop 1 path))
                applyForChild acc childKey _ =
                    case nodeRefFromKey childKey of
                        GenRef _ -> acc
                        TypeRef childN ->
                            let baseRef = typeRef childN
                                baseAncestor = firstGenAncestorWith bindParentsBase baseRef
                                solvedRef = mapBaseRef baseRef
                                solvedAncestor =
                                    case solvedRef of
                                        Just ref -> firstGenAncestorWith acc ref
                                        Nothing -> Nothing
                                childIsCopy =
                                    case solvedRef of
                                        Just (TypeRef nid) -> IntMap.member (getNodeId nid) instCopyMap
                                        _ -> False
                            in if childIsCopy
                                then acc
                                else
                                    case (baseAncestor, solvedRef) of
                                        (Just gidBase, Just _) ->
                                            if solvedAncestor == Just gidBase
                                                then acc
                                                else
                                                    case bindingPathToRootLocal bindParentsBase baseRef of
                                                        Left _ -> acc
                                                        Right path ->
                                                            let prefix = prefixToGen gidBase path
                                                            in if null prefix
                                                                then acc
                                                                else applyPath acc prefix
                                        _ -> acc
            in IntMap.foldlWithKey' applyForChild bp0 bindParentsBase
        bindParents'' =
            let
                forceSchemeRootParents bp =
                    foldl'
                        (\acc gen ->
                            let gid = gnId gen
                            in foldl'
                                (\acc' root ->
                                    let rootC = canonical root
                                        rootRef = typeRef rootC
                                        rootKey = nodeRefKey rootRef
                                        accWithRoot =
                                            if okRef rootRef
                                                then
                                                    case IntMap.lookup rootKey acc' of
                                                        Nothing -> IntMap.insert rootKey (GenRef gid, BindFlex) acc'
                                                        Just (parentExisting, flag)
                                                            | parentExisting == GenRef gid -> acc'
                                                            | nodeRefKey parentExisting == rootKey ->
                                                                IntMap.insert rootKey (GenRef gid, flag) acc'
                                                            | otherwise ->
                                                                IntMap.insert rootKey (GenRef gid, flag) acc'
                                                else acc'
                                    in case IntMap.lookup (getNodeId rootC) nodesSolved of
                                        Just TyVar{ tnBound = Just bnd } ->
                                            let bndRef = typeRef bnd
                                                bndKey = nodeRefKey bndRef
                                            in if okRef bndRef
                                                then
                                                    case IntMap.lookup bndKey accWithRoot of
                                                        Nothing ->
                                                            IntMap.insert bndKey (rootRef, BindFlex) accWithRoot
                                                        Just (parentExisting, flag)
                                                            | nodeRefKey parentExisting == bndKey ->
                                                                IntMap.insert bndKey (rootRef, flag) accWithRoot
                                                            | otherwise -> accWithRoot
                                                else accWithRoot
                                        _ -> accWithRoot
                                )
                                acc
                                (gnSchemes gen)
                        )
                        bp
                        (IntMap.elems genMerged)
                restoreBoundParents bp =
                    IntMap.foldlWithKey'
                        (\acc _key node ->
                            case node of
                                TyVar{ tnId = vid, tnBound = Just bnd } ->
                                    let varRef = typeRef (canonical vid)
                                        bndRef = typeRef (canonical bnd)
                                        bndKey = nodeRefKey bndRef
                                    in if okRef varRef && okRef bndRef
                                        then
                                            case IntMap.lookup bndKey acc of
                                                Nothing ->
                                                    IntMap.insert bndKey (varRef, BindFlex) acc
                                                Just (parentExisting, flag)
                                                    | nodeRefKey parentExisting == bndKey ->
                                                        IntMap.insert bndKey (varRef, flag) acc
                                                    | otherwise -> acc
                                        else acc
                                _ -> acc
                        )
                        bp
                        nodesSolved

                addMissingUnderGen bp gid root =
                    let rootC = canonical root
                        interior = reachableFromWithBoundsStop rootC
                    in IntSet.foldl'
                        (\acc nidInt ->
                            let child = NodeId nidInt
                                childKey = nodeRefKey (typeRef child)
                                baseKey =
                                    case IntMap.lookup nidInt solvedToBase of
                                        Just (NodeId key) -> key
                                        Nothing -> nidInt
                                sticky = IntSet.member baseKey stickyTypeParentsBase
                                owner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                                baseOwner = baseFirstGenAncestor (typeRef child)
                                existing = IntMap.lookup childKey acc
                            in if sticky
                                then acc
                                else if IntSet.member nidInt instCopyNodes
                                then acc
                                else if IntMap.member childKey acc
                                then
                                    case existing of
                                        Just (parentExisting, flag)
                                            | nodeRefKey parentExisting == childKey ->
                                                case owner of
                                                    Just gid' | gid' /= gid -> acc
                                                    _ -> IntMap.insert childKey (GenRef gid, flag) acc
                                        _ -> acc
                                else if owner /= Nothing && owner /= Just gid
                                    then acc
                                else if baseOwner /= Nothing && baseOwner /= Just gid
                                    then acc
                                else
                                    case IntMap.lookup nidInt nodesSolved of
                                        Just TyVar{} ->
                                            IntMap.insert childKey (GenRef gid, BindFlex) acc
                                        _ -> acc
                        )
                        bp
                        interior
            in foldl'
                (\bp gen ->
                    let gid = gnId gen
                    in foldl' (\bp' root -> addMissingUnderGen bp' gid root) bp (gnSchemes gen)
                )
                (restoreBoundParents (forceSchemeRootParents bindParentsWithParentOwners))
                (IntMap.elems genMerged)
        bindParentsFinal0 =
            let applyBaseGenParent acc (childKey, (parent, flag)) =
                    case parent of
                        GenRef _ ->
                            case IntMap.lookup childKey acc of
                                Nothing -> IntMap.insert childKey (parent, flag) acc
                                Just (parent0, flag0)
                                    | nodeRefKey parent0 == childKey ->
                                        IntMap.insert childKey (parent, max flag0 flag) acc
                                    | parent0 == parent ->
                                        IntMap.insert childKey (parent0, max flag0 flag) acc
                                    | otherwise -> acc
                        _ -> acc
            in foldl' applyBaseGenParent bindParents'' (IntMap.toList bindParentsBase')
        forceBoundRootParents bp =
            IntSet.foldl'
                (\acc rootKey ->
                    case IntMap.lookup rootKey schemeRootOwnersFiltered of
                        Nothing -> acc
                        Just gid ->
                            let root = NodeId rootKey
                                interior = reachableFromWithBoundsStop root
                                step acc' nidInt =
                                    if IntSet.member nidInt instCopyNodes
                                        then acc'
                                        else case IntMap.lookup nidInt nodesSolved of
                                            Just TyVar{} ->
                                                let child = NodeId nidInt
                                                    childKey = nodeRefKey (typeRef child)
                                                    baseKey =
                                                        case IntMap.lookup nidInt solvedToBase of
                                                            Just (NodeId key) -> key
                                                            Nothing -> nidInt
                                                    sticky = IntSet.member baseKey stickyTypeParentsBase
                                                    owner = IntMap.lookup nidInt schemeRootOwnersFiltered
                                                    baseOwner = baseFirstGenAncestor (typeRef child)
                                                in if sticky
                                                    || (baseOwner /= Nothing && baseOwner /= Just gid)
                                                    then acc'
                                                    else
                                                        (case owner of
                                                            Just gid' | gid' /= gid -> acc'
                                                            _ ->
                                                                    case IntMap.lookup childKey acc' of
                                                                        Just (parentExisting, flag)
                                                                            | nodeRefKey parentExisting == childKey ->
                                                                                IntMap.insert childKey (GenRef gid, flag) acc'
                                                                            | otherwise -> acc'
                                                                        Nothing -> IntMap.insert childKey (GenRef gid, BindFlex) acc'
                                                        )
                                            _ -> acc'
                            in IntSet.foldl'
                                step
                                acc
                                interior
                )
                bp
                boundSchemeRoots
        overrideSchemeInteriorParentsWith schemeOwnerMap gens bp =
            foldl'
                (\acc gen ->
                    let gid = gnId gen
                    in foldl'
                        (\acc' root ->
                            let rootC = canonical root
                                interior = reachableFromWithBoundsStop rootC
                            in IntSet.foldl'
                                (\acc'' nidInt ->
                                    let isInstCopy = IntSet.member nidInt instCopyNodes
                                        ownerFinal = IntMap.lookup nidInt schemeOwnerMap
                                        ownerOk =
                                            case ownerFinal of
                                                Just gid' -> gid' == gid
                                                Nothing -> False
                                    in if isInstCopy && not ownerOk
                                        then acc''
                                        else case IntMap.lookup nidInt nodesSolved of
                                            Just TyVar{} ->
                                                case ownerFinal of
                                                    Just gid' | gid' /= gid -> acc''
                                                    _ ->
                                                        let childRef = typeRef (NodeId nidInt)
                                                            childKey = nodeRefKey childRef
                                                            currentOwner = firstGenAncestorWith acc'' childRef
                                                            shouldOverride =
                                                                case currentOwner of
                                                                    Just gid' -> gid' /= gid
                                                                    Nothing -> True
                                                            baseKey =
                                                                case IntMap.lookup nidInt solvedToBase of
                                                                    Just (NodeId key) -> key
                                                                    Nothing -> nidInt
                                                            sticky = IntSet.member baseKey stickyTypeParentsBase
                                                            baseOwner = baseFirstGenAncestor childRef
                                                            oldOwner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                                                            ownerChanged =
                                                                case (oldOwner, ownerFinal) of
                                                                    (Just oldG, Just newG) -> oldG /= newG
                                                                    _ -> False
                                                            allowFromBase =
                                                                case baseOwner of
                                                                    Nothing -> True
                                                                    Just gidBase ->
                                                                        if gidBase == gid
                                                                            then True
                                                                            else case oldOwner of
                                                                                Just gidOld
                                                                                    | gidOld == gid -> True
                                                                                Just gidOld
                                                                                    | gidOld == gidBase && ownerChanged -> True
                                                                                _ -> False
                                                        in if sticky || not allowFromBase
                                                            then acc''
                                                            else if shouldOverride
                                                                then
                                                                    case IntMap.lookup childKey acc'' of
                                                                        Just (_parentExisting, flag) ->
                                                                            IntMap.insert childKey (GenRef gid, flag) acc''
                                                                        Nothing -> IntMap.insert childKey (GenRef gid, BindFlex) acc''
                                                            else
                                                                case IntMap.lookup childKey acc'' of
                                                                    Just (parentExisting, flag)
                                                                        | nodeRefKey parentExisting == childKey ->
                                                                            IntMap.insert childKey (GenRef gid, flag) acc''
                                                                        | otherwise -> acc''
                                                                    Nothing -> IntMap.insert childKey (GenRef gid, BindFlex) acc''
                                            _ -> acc''
                                )
                                acc'
                                interior
                        )
                        acc
                        (gnSchemes gen)
                )
                bp
                (IntMap.elems gens)
        bindParentsFinal0' =
            overrideSchemeInteriorParentsWith schemeInteriorOwnersFiltered' genMerged (forceBoundRootParents bindParentsFinal0)
        bindParentsFinal =
            foldl'
                (\acc gen ->
                    let gid = gnId gen
                    in foldl'
                        (\acc' root ->
                            let interior = reachableFromWithBoundsStop (canonical root)
                            in IntSet.foldl'
                                (\acc'' nidInt ->
                                    let isInstCopy = IntSet.member nidInt instCopyNodes
                                        owner = IntMap.lookup nidInt schemeInteriorOwnersFiltered'
                                        allowInstCopy = case owner of
                                            Just gid' -> gid' == gid
                                            Nothing -> False
                                    in if isInstCopy && not allowInstCopy
                                        then acc''
                                        else case IntMap.lookup nidInt nodesSolved of
                                            Just TyVar{} ->
                                                let child = NodeId nidInt
                                                    childKey = nodeRefKey (typeRef child)
                                                    baseKey =
                                                        case IntMap.lookup nidInt solvedToBase of
                                                            Just (NodeId key) -> key
                                                            Nothing -> nidInt
                                                    sticky = IntSet.member baseKey stickyTypeParentsBase
                                                    baseOwner = baseFirstGenAncestor (typeRef child)
                                                    baseParent = IntMap.lookup childKey bindParentsBase'
                                                    existing = IntMap.lookup childKey acc''
                                                    shouldPreserve =
                                                        case baseOwner of
                                                            Just gid' -> gid' /= gid
                                                            Nothing ->
                                                                case baseParent of
                                                                    Just (GenRef gid', _) -> gid' /= gid
                                                                    _ ->
                                                                        case existing of
                                                                            Just (GenRef gid', _) -> gid' /= gid
                                                                            _ -> False
                                                in if sticky
                                                    then acc''
                                                    else if owner /= Nothing && owner /= Just gid
                                                    then acc''
                                                    else if shouldPreserve
                                                    then acc''
                                                    else
                                                        case existing of
                                                            Nothing ->
                                                                IntMap.insert
                                                                    childKey
                                                                    (GenRef gid, BindFlex)
                                                                    acc''
                                                            Just (parentExisting, flag)
                                                                | nodeRefKey parentExisting == childKey ->
                                                                    IntMap.insert childKey (GenRef gid, flag) acc''
                                                                | otherwise -> acc''
                                            _ -> acc''
                                )
                                acc'
                                interior
                        )
                        acc
                        (gnSchemes gen)
                )
                bindParentsFinal0'
                (IntMap.elems genMerged)
        rebindSchemeBodyAliases bp =
            IntMap.foldlWithKey'
                (\acc nidInt node ->
                    case node of
                        TyVar{ tnBound = Just bnd }
                            | IntSet.member nidInt instCopyNodes ->
                            let bndC = canonical bnd
                            in case IntMap.lookup (getNodeId bndC) schemeRootByBodySolved of
                                Just root ->
                                    case IntMap.lookup (getNodeId (canonical root)) schemeRootOwnersFiltered of
                                        Just gid ->
                                            let childKey = nodeRefKey (typeRef (NodeId nidInt))
                                            in case IntMap.lookup childKey acc of
                                                Just (GenRef _, _) -> acc
                                                Just (TypeRef _, flag) ->
                                                    IntMap.insert childKey (GenRef gid, flag) acc
                                                Nothing ->
                                                    IntMap.insert childKey (GenRef gid, BindFlex) acc
                                        Nothing -> acc
                                Nothing -> acc
                        _ -> acc
                )
                bp
                nodesSolved
        rootGenId =
            case [gid | GenRef gid <- Binding.bindingRoots base] of
                [gid] -> gid
                _ ->
                    case IntMap.keys genMerged of
                        (k:_) -> GenNodeId k
                        [] -> GenNodeId 0
        attachOrphans bp =
            let constraint0 =
                    solvedConstraint
                        { cNodes = nodesSolved
                        , cGenNodes = genMerged
                        , cBindParents = bp
                        }
                roots = Binding.bindingRoots constraint0
                rootKey = nodeRefKey (GenRef rootGenId)
            in foldl'
                (\acc ref ->
                    if nodeRefKey ref == rootKey
                        then acc
                        else
                            let parent =
                                    case ref of
                                        GenRef _ -> GenRef rootGenId
                                        TypeRef nid ->
                                            case baseFirstGenAncestor (typeRef nid) of
                                                Just gid -> GenRef gid
                                                Nothing -> GenRef rootGenId
                            in IntMap.insert (nodeRefKey ref) (parent, BindFlex) acc
                )
                bp
                roots
        bindParentsFinalRebound = rebindSchemeBodyAliases bindParentsFinal
        bindParentsFinal' =
            let msg =
                    "constraintForGeneralization: scheme interiors="
                        ++ show
                            [ (gnId gen, root, map (\nid -> (nid, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParentsFinal))
                                    [ nidInt
                                    | nidInt <- IntSet.toList (reachableFromWithBoundsStop (canonical root))
                                    , case IntMap.lookup nidInt nodesSolved of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ])
                            | gen <- IntMap.elems genMerged
                            , root <- gnSchemes gen
                            ]
                probeId = NodeId 17
                probeKey = nodeRefKey (typeRef probeId)
                probeSolvedParent = IntMap.lookup probeKey bindParentsSolved
                probeSolvedToBase = IntMap.lookup (getNodeId probeId) solvedToBase
                probeInstCopy = IntSet.member (getNodeId probeId) instCopyNodes
                parentInfo = IntMap.lookup (nodeRefKey (typeRef probeId)) bindParentsFinal
                msgProbe =
                    "constraintForGeneralization: probe bind-parent node="
                        ++ show probeId
                        ++ " parent="
                        ++ show parentInfo
                        ++ " solvedParent="
                        ++ show probeSolvedParent
                        ++ " solvedToBase="
                        ++ show probeSolvedToBase
                        ++ " instCopy="
                        ++ show probeInstCopy
            in debugGaScope msgProbe (debugGaScope msg bindParentsFinalRebound)
        bindParentsFinal'' = attachOrphans bindParentsFinal'
        bindParentsFinalClean = dropSelfParents bindParentsFinal''
        restoreSchemeRootParentsFromOwners bp0 =
            IntMap.foldlWithKey'
                (\acc rootKey gid ->
                    let rootRef = typeRef (NodeId rootKey)
                        rootKeyRef = nodeRefKey rootRef
                    in if okRef rootRef
                        then
                            case IntMap.lookup rootKeyRef acc of
                                Just (_parentExisting, flag) ->
                                    IntMap.insert rootKeyRef (GenRef gid, flag) acc
                                Nothing -> IntMap.insert rootKeyRef (GenRef gid, BindFlex) acc
                        else acc
                )
                bp0
                schemeRootOwnersFiltered
        bindParentsFinalPreserved =
            restoreSchemeRootParentsFromOwners
                (restoreSchemeRootParentsByBody
                    (restoreSchemeRootParentsFromBase (preserveBaseGaPrime bindParentsFinalClean))
                )
        schemeRootsByGen =
            IntMap.fromListWith
                (\a b -> a ++ b)
                [ (genNodeKey gid, [NodeId rootKey])
                | rootKey <- IntSet.toList allSchemeRootsFiltered
                , Just (GenRef gid, _) <- [IntMap.lookup (nodeRefKey (typeRef (NodeId rootKey))) bindParentsFinalPreserved]
                ]
        genMerged' =
            IntMap.mapWithKey
                (\k gen ->
                    let roots =
                            case IntMap.lookup k schemeRootsByGen of
                                Just rs -> rs
                                Nothing -> []
                    in gen { gnSchemes = roots }
                )
                genMerged
        schemeRootOwnersFinal =
            IntMap.fromList
                [ (getNodeId (canonical root), gnId gen)
                | gen <- IntMap.elems genMerged'
                , root <- gnSchemes gen
                ]
        schemeInteriorOwnersFinal =
            IntMap.fromListWith
                (\_ old -> old)
                [ (nidInt, gid)
                | (rootKey, gid) <- IntMap.toList schemeRootOwnersFinal
                , nidInt <- IntSet.toList (reachableFromWithBoundsStop (NodeId rootKey))
                , case IntMap.lookup nidInt nodesSolved of
                    Just TyVar{} -> True
                    _ -> False
                ]
        bindParentsFinalAligned =
            overrideSchemeInteriorParentsWith
                schemeInteriorOwnersFinal
                genMerged'
                bindParentsFinalPreserved
        schemeRootsMerged' =
            [ (gnId gen, map canonical (gnSchemes gen))
            | gen <- IntMap.elems genMerged'
            ]
        constraintForGen =
            let restoreTypeParents acc =
                    IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, flag) ->
                            case (nodeRefFromKey childKey, parentRef) of
                                (TypeRef childBase, TypeRef _parentBase) ->
                                    case (mapBaseRef (typeRef childBase), mapBaseRef parentRef) of
                                        (Just childRef', Just parentRef') ->
                                            let childKey' = nodeRefKey childRef'
                                            in if not (okRef childRef' && okRef parentRef')
                                                then acc'
                                            else if nodeRefKey childRef' == nodeRefKey parentRef'
                                                then acc'
                                            else if not (isUpperRef parentRef' childRef')
                                                then acc'
                                            else
                                                case IntMap.lookup childKey' acc' of
                                                    Nothing -> IntMap.insert childKey' (parentRef', flag) acc'
                                                    Just (parentExisting, _flagExisting)
                                                        | nodeRefKey parentExisting == childKey' ->
                                                            IntMap.insert childKey' (parentRef', flag) acc'
                                                        | otherwise -> acc'
                                        _ -> acc'
                                _ -> acc'
                        )
                        acc
                        bindParentsBase
                bindParentsFinalAligned' = restoreTypeParents bindParentsFinalAligned
                rootGenIdBase =
                    case [gid | GenRef gid <- Binding.bindingRoots base] of
                        [gid] -> gid
                        _ ->
                            case IntMap.keys genMerged of
                                (k:_) -> GenNodeId k
                                [] -> GenNodeId 0
                bindParentsFinalAligned'' =
                    IntMap.foldlWithKey'
                        (\acc copyKey baseN ->
                            let childRef = typeRef (NodeId copyKey)
                                childRef' = adoptRef childRef
                                childKey' = nodeRefKey childRef'
                            in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                                Just (GenRef gid, flag)
                                    | gid == rootGenIdBase
                                    , okRef childRef' ->
                                        IntMap.insert childKey' (GenRef gid, flag) acc
                                _ -> acc
                        )
                        bindParentsFinalAligned'
                        instCopyMap
                pruneBindParents c =
                    let liveNodes = cNodes c
                        liveGens = cGenNodes c
                        liveChild childKey =
                            case nodeRefFromKey childKey of
                                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
                        liveParent ref =
                            case ref of
                                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
                        bindParentsPruned =
                            IntMap.filterWithKey
                                (\childKey (parentRef, _flag) ->
                                    liveChild childKey && liveParent parentRef
                                )
                                (cBindParents c)
                    in c { cBindParents = bindParentsPruned }
                constraint0 = solvedConstraint { cNodes = nodesSolved, cBindParents = bindParentsFinalAligned'', cGenNodes = genMerged' }
                constraint1 = pruneBindParents constraint0
                probeIds = [NodeId 1, NodeId 2, NodeId 3]
                probeInfo =
                    [ ( pid
                      , IntMap.lookup (getNodeId pid) (cNodes constraint1)
                      , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents constraint1)
                      )
                    | pid <- probeIds
                    ]
            in debugGaScope ("constraintForGeneralization: probe nodes " ++ show probeInfo) constraint1
        (qAlignSolvedToBase, qAlignBaseToSolved) =
            let canonicalBase = id
                alignOne (accSolved, accBase) gen =
                    let gid = gnId gen
                    in case
                        ( Binding.boundFlexChildrenUnder canonicalBase base (GenRef gid)
                        , Binding.boundFlexChildrenUnder canonical constraintForGen (GenRef gid)
                        ) of
                            (Right baseBinders, Right solvedBinders) ->
                                foldl'
                                    (\(accSolved', accBase') (solvedB, baseB) ->
                                        let solvedKey = getNodeId (canonical solvedB)
                                            baseKey = getNodeId baseB
                                        in ( IntMap.insertWith (\_ old -> old) solvedKey baseB accSolved'
                                           , IntMap.insertWith (\_ old -> old) baseKey (canonical solvedB) accBase'
                                           )
                                    )
                                    (accSolved, accBase)
                                    (zip solvedBinders baseBinders)
                            _ -> (accSolved, accBase)
            in foldl' alignOne (IntMap.empty, IntMap.empty) (IntMap.elems (cGenNodes base))
        baseToSolvedAligned = IntMap.union baseToSolved qAlignBaseToSolved
        solvedToBaseAligned0 =
            IntMap.foldlWithKey'
                (\acc baseKey solvedNid ->
                    let solvedKeyC = getNodeId (canonical solvedNid)
                        solvedKeyRaw = getNodeId solvedNid
                        acc' = IntMap.insertWith (\_ old -> old) solvedKeyC (NodeId baseKey) acc
                    in IntMap.insertWith (\_ old -> old) solvedKeyRaw (NodeId baseKey) acc'
                )
                IntMap.empty
                baseToSolvedAligned
        solvedToBaseAligned =
            let copyOverrides =
                    IntMap.fromList
                        [ (copyKeyC, baseN)
                        | (copyKey, baseN) <- IntMap.toList instCopyMap
                        , let copyKeyC = getNodeId (canonical (NodeId copyKey))
                        , IntMap.member copyKeyC nodesSolved
                        ]
            in IntMap.union copyOverrides (IntMap.union solvedToBaseAligned0 qAlignSolvedToBase)
    in debugGaScope
            ("constraintForGeneralization: merged gens="
                ++ show (map fst schemeRootsMerged')
                ++ " schemes="
                ++ show (map snd schemeRootsMerged')
            )
            ( constraintForGen
            , GaBindParents
                { gaBindParentsBase = bindParentsBase
                , gaBaseConstraint = base
                , gaBaseToSolved = baseToSolvedAligned
                , gaSolvedToBase = solvedToBaseAligned
                }
            )

letScopeOverrides :: Constraint -> Constraint -> SolveResult -> IntMap.IntMap NodeId -> AnnExpr -> IntMap.IntMap NodeRef
letScopeOverrides base solvedForGen solved redirects ann =
    let canonical = frWith (srUnionFind solved)
        addOverride acc schemeRootId =
            case bindingScopeRef base schemeRootId of
                Right scope0 ->
                    let scope = canonicalizeScopeRef solved redirects scope0
                        schemeRootC = canonical (chaseRedirects redirects schemeRootId)
                        postScope =
                            case bindingScopeRef solvedForGen schemeRootC of
                                Right ref -> canonicalizeScopeRef solved redirects ref
                                Left _ -> scope
                    in if scope == postScope
                        then acc
                        else IntMap.insert (getNodeId schemeRootC) scope acc
                Left _ -> acc
        alg expr = case expr of
            AVarF _ _ -> IntMap.empty
            ALitF _ _ -> IntMap.empty
            ALamF _ _ _ body _ -> body
            AAppF fun arg _ _ _ -> IntMap.union arg fun
            ALetF _ _ schemeRootId _ _ rhs body _ ->
                let baseMap = addOverride IntMap.empty schemeRootId
                in IntMap.union body (IntMap.union rhs baseMap)
            AAnnF inner _ _ -> inner
    in cata alg ann

debugGaScope :: String -> a -> a
debugGaScope msg value =
    if debugGaScopeEnabled
        then trace msg value
        else value

debugGaScopeEnabled :: Bool
debugGaScopeEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_BINDING"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugGaScopeEnabled #-}

edgeOrigins :: AnnExpr -> IntMap.IntMap String
edgeOrigins = fmap (intercalate " | ") . aoMap . cata alg
  where
    tagNode nid = "node=" ++ show nid
    insertEdge eid msg =
        IntMap.insertWith
            (\new old -> old ++ new)
            (getEdgeId eid)
            [msg]
    alg expr = case expr of
        AVarF _ nid -> OriginAcc nid IntMap.empty
        ALitF _ nid -> OriginAcc nid IntMap.empty
        ALamF _ _ _ body nid -> OriginAcc nid (aoMap body)
        AAppF fun arg funEid argEid nid ->
            let msgFun =
                    "AApp fun " ++ tagNode (aoNode fun)
                        ++ " arg=" ++ tagNode (aoNode arg)
                        ++ " app=" ++ show nid
                msgArg =
                    "AApp arg " ++ tagNode (aoNode arg)
                        ++ " fun=" ++ tagNode (aoNode fun)
                        ++ " app=" ++ show nid
                accChildren = IntMap.unionWith (++) (aoMap arg) (aoMap fun)
                acc1 = insertEdge argEid msgArg accChildren
                acc2 = insertEdge funEid msgFun acc1
            in OriginAcc nid acc2
        ALetF _ _ _ _ _ rhs body nid ->
            let accChildren = IntMap.unionWith (++) (aoMap body) (aoMap rhs)
            in OriginAcc nid accChildren
        AAnnF inner target eid ->
            let msg = "AAnn inner=" ++ tagNode (aoNode inner) ++ " target=" ++ show target
                acc1 = insertEdge eid msg (aoMap inner)
            in OriginAcc target acc1

data OriginAcc = OriginAcc
    { aoNode :: NodeId
    , aoMap :: IntMap.IntMap [String]
    }
