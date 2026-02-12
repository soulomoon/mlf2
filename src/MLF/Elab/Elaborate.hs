{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module MLF.Elab.Elaborate (
    ElabConfig(..),
    ElabEnv(..),
    expansionToInst,
    elaborate,
    elaborateWithGen,
    elaborateWithScope,
    elaborateWithEnv
) where

import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, listToMaybe)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Reify.TypeOps
    ( alphaEqType
    , freeTypeVarsType
    , inlineBaseBoundsType
    , matchType
    )

import MLF.Frontend.Syntax (VarName)
import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Legacy (expansionToInst)
import MLF.Elab.TermClosure (closeTermWithSchemeSubstIfNeeded, substInTerm)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType)
import MLF.Elab.Run.Annotation (adjustAnnotationInst)
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.TypeCheck (typeCheck)
import qualified MLF.Elab.TypeCheck as TypeCheck (Env(..), typeCheckWithEnv)
import qualified MLF.Elab.Inst as Inst
import MLF.Reify.Core
    ( namedNodes
    , reifyType
    , reifyTypeWithNamedSetNoFallback
    )
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution
    ( EdgeTrace
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr(..), AnnExprF(..))

type GeneralizeAtWith =
    Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

data ElabConfig = ElabConfig
    { ecTraceConfig :: TraceConfig
    , ecGeneralizeAtWith :: GeneralizeAtWith
    }

data ElabEnv = ElabEnv
    { eeResPhi :: SolveResult
    , eeResReify :: SolveResult
    , eeResGen :: SolveResult
    , eeGaParents :: GaBindParents
    , eeEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , eeEdgeTraces :: IntMap.IntMap EdgeTrace
    , eeEdgeExpansions :: IntMap.IntMap Expansion
    , eeScopeOverrides :: IntMap.IntMap NodeRef
    }

type Env = Map.Map VarName SchemeInfo

data ElabOut = ElabOut
    { elabTerm :: Env -> Either ElabError ElabTerm
    , elabStripped :: Env -> Either ElabError ElabTerm
    }

schemeBodyTarget :: SolveResult -> NodeId -> NodeId
schemeBodyTarget res target =
    let constraint = srConstraint res
        canonical = Solve.frWith (srUnionFind res)
        targetC = canonical target
    in case NodeAccess.lookupNode constraint targetC of
        Just TyVar{ tnBound = Just bnd } ->
            case NodeAccess.lookupNode constraint (canonical bnd) of
                Just TyForall{ tnBody = body } -> canonical body
                _ -> canonical bnd
        Just TyForall{ tnBody = body } -> canonical body
        _ -> targetC

elaborate
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> SolveResult
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborate traceCfg generalizeAtWith resPhi resGen edgeWitnesses edgeTraces edgeExpansions ann =
    let constraint = srConstraint resGen
        keys = map (getNodeId . fst) (toListNode (cNodes constraint))
        baseToSolved =
            IntMap.fromList
                [ (k, NodeId k)
                | k <- keys
                ]
        solvedToBase =
            IntMap.fromList
                [ (k, NodeId k)
                | k <- keys
                ]
        gaParents = GaBindParents
            { gaBindParentsBase = cBindParents constraint
            , gaBaseConstraint = constraint
            , gaBaseToSolved = baseToSolved
            , gaSolvedToBase = solvedToBase
            }
    in elaborateWithGen traceCfg generalizeAtWith resPhi resGen resGen gaParents edgeWitnesses edgeTraces edgeExpansions ann

elaborateWithGen
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> SolveResult
    -> SolveResult
    -> GaBindParents
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborateWithGen traceCfg generalizeAtWith resPhi resReify resGen gaParents edgeWitnesses edgeTraces edgeExpansions ann =
    elaborateWithScope traceCfg generalizeAtWith resPhi resReify resGen gaParents edgeWitnesses edgeTraces edgeExpansions IntMap.empty ann

elaborateWithScope
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> SolveResult
    -> SolveResult
    -> GaBindParents
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> IntMap.IntMap NodeRef
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborateWithScope traceCfg generalizeAtWith resPhi resReify resGen gaParents edgeWitnesses edgeTraces edgeExpansions scopeOverrides ann =
    elaborateWithEnv
        ElabConfig
            { ecTraceConfig = traceCfg
            , ecGeneralizeAtWith = generalizeAtWith
            }
        ElabEnv
            { eeResPhi = resPhi
            , eeResReify = resReify
            , eeResGen = resGen
            , eeGaParents = gaParents
            , eeEdgeWitnesses = edgeWitnesses
            , eeEdgeTraces = edgeTraces
            , eeEdgeExpansions = edgeExpansions
            , eeScopeOverrides = scopeOverrides
            }
        ann

elaborateWithEnv
    :: ElabConfig
    -> ElabEnv
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborateWithEnv config elabEnv ann = do
    namedSetPhi <- namedNodes resPhi
    namedSetReify <- namedNodes resReify
    let ElabOut { elabTerm = runElab } = para (elabAlg namedSetPhi namedSetReify) ann
    runElab Map.empty
  where
    ElabConfig
        { ecTraceConfig = traceCfg
        , ecGeneralizeAtWith = generalizeAtWithRaw
        } = config
    ElabEnv
        { eeResPhi = resPhi
        , eeResReify = resReify
        , eeResGen = resGen
        , eeGaParents = gaParents
        , eeEdgeWitnesses = edgeWitnesses
        , eeEdgeTraces = edgeTraces
        , eeEdgeExpansions = edgeExpansions
        , eeScopeOverrides = scopeOverrides
        } = elabEnv
    canonical = Solve.frWith (srUnionFind resReify)
    scopeRootFromBase root =
        case IntMap.lookup (getNodeId (canonical root)) (gaSolvedToBase gaParents) of
            Nothing -> typeRef root
            Just baseN ->
                case bindingPathToRootLocal (gaBindParentsBase gaParents) (typeRef baseN) of
                    Left _ -> typeRef root
                    Right path ->
                        case listToMaybe [gid | GenRef gid <- drop 1 path] of
                            Just gid -> GenRef gid
                            Nothing -> typeRef root

    generalizeAtWith
        :: Maybe GaBindParents
        -> SolveResult
        -> NodeRef
        -> NodeId
        -> Either ElabError (ElabScheme, IntMap.IntMap String)
    generalizeAtWith mbGa res scopeRoot targetNode =
        case generalizeAtWithRaw mbGa res scopeRoot targetNode of
            Right out -> Right out
            Left (BindingTreeError GenSchemeFreeVars{}) ->
                case mbGa of
                    Just _ ->
                        case generalizeAtWithRaw Nothing res scopeRoot targetNode of
                            Right out -> Right out
                            Left (BindingTreeError GenSchemeFreeVars{}) -> do
                                ty <- reifyType res targetNode
                                pure (schemeFromType ty, IntMap.empty)
                            Left err2 -> Left err2
                    Nothing -> do
                        ty <- reifyType res targetNode
                        pure (schemeFromType ty, IntMap.empty)
            Left err -> Left err

    scopeRootForNode :: NodeId -> NodeRef
    scopeRootForNode nodeId =
        case IntMap.lookup (getNodeId (canonical nodeId)) scopeOverrides of
            Just ref -> ref
            Nothing -> scopeRootFromBase nodeId

    generalizeAtNode :: NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
    generalizeAtNode nodeId =
        let scopeRoot = scopeRootForNode nodeId
            targetC = schemeBodyTarget resGen nodeId
        in case generalizeAtWith (Just gaParents) resGen scopeRoot targetC of
            Right out -> Right out
            Left (BindingTreeError GenSchemeFreeVars{}) ->
                case generalizeAtWith Nothing resGen scopeRoot targetC of
                    Right out -> Right out
                    Left (BindingTreeError GenSchemeFreeVars{}) -> do
                        tyFallback <- reifyNodeTypePreferringBound targetC
                        pure (schemeFromType tyFallback, IntMap.empty)
                    Left err -> Left err
            Left err -> Left err

    reifyNodeTypePreferringBound :: NodeId -> Either ElabError ElabType
    reifyNodeTypePreferringBound nodeId = do
        namedSet <- namedNodes resReify
        let nodeC = canonical nodeId
        case VarStore.lookupVarBound (srConstraint resReify) nodeC of
            Just bnd -> reifyTypeForParam namedSet resReify bnd
            Nothing -> reifyTypeForParam namedSet resReify nodeC

    closeTermForAnnotation :: ElabTerm -> ElabTerm
    closeTermForAnnotation term =
        case typeCheck term of
            Right ty ->
                let freeVars = Set.toList (freeTypeVarsType ty)
                    sch = Forall [(v, Nothing) | v <- freeVars] ty
                in closeTermWithSchemeSubstIfNeeded IntMap.empty sch term
            Left _ -> term

    resolvedLambdaParamNode :: NodeId -> Maybe NodeId
    resolvedLambdaParamNode lamNodeId =
        let lamC = canonical lamNodeId
        in case NodeAccess.lookupNode (srConstraint resReify) lamC of
            Just TyArrow{ tnDom = dom } -> Just dom
            Just TyVar{ tnBound = Just bnd } ->
                case NodeAccess.lookupNode (srConstraint resReify) (canonical bnd) of
                    Just TyArrow{ tnDom = dom } -> Just dom
                    _ -> Nothing
            _ -> Nothing

    hasInformativeVarBound :: NodeId -> Bool
    hasInformativeVarBound nodeId =
        let constraint = srConstraint resReify
            chase seen nid0 =
                let nid = canonical nid0
                    nidKey = getNodeId nid
                in if IntSet.member nidKey seen
                    then False
                    else case NodeAccess.lookupNode constraint nid of
                        Just TyVar{ tnBound = Just bnd } ->
                            let bndC = canonical bnd
                                seen' = IntSet.insert nidKey seen
                            in case NodeAccess.lookupNode constraint bndC of
                                Just TyVar{} -> chase seen' bndC
                                Just _ -> True
                                Nothing -> False
                        _ -> False
        in chase IntSet.empty nodeId

    termStartsWithTypeAbstraction :: ElabTerm -> Bool
    termStartsWithTypeAbstraction term = case term of
        ETyAbs{} -> True
        _ -> False

    coercionDomainTy :: ElabType -> Maybe ElabType
    coercionDomainTy ty0 = case ty0 of
        TForall v (Just bnd) body
            | body == TVar v ->
                Just (tyToElab bnd)
        _ -> Nothing

    mkOut :: (Env -> Either ElabError ElabTerm) -> ElabOut
    mkOut f = ElabOut f f

    elabAlg :: IntSet.IntSet -> IntSet.IntSet -> AnnExprF (AnnExpr, ElabOut) -> ElabOut
    elabAlg namedSetPhi namedSetReify layer = case layer of
        AVarF v _ -> mkOut $ \env ->
            maybe (Left (EnvLookup v)) (const (Right (EVar v))) (Map.lookup v env)
        ALitF lit _ -> mkOut $ \_ -> Right (ELit lit)
        ALamF v paramNode _ (bodyAnn, bodyOut) lamNodeId ->
            let f env = do
                    let mAnnLambda = desugaredAnnLambdaInfo v bodyAnn
                        resolvedParam = resolvedLambdaParamNode lamNodeId
                        paramSource =
                            case mAnnLambda of
                                Just _ -> fromMaybe paramNode resolvedParam
                                Nothing ->
                                    case resolvedParam of
                                        Just resolvedNode
                                            | hasInformativeVarBound resolvedNode -> resolvedNode
                                        _ -> paramNode
                        bodyElabOut =
                            case mAnnLambda of
                                Just (_, innerBodyAnn) ->
                                    para (elabAlg namedSetPhi namedSetReify) innerBodyAnn
                                Nothing -> bodyOut
                    paramTySurface <- reifyNodeTypePreferringBound paramSource
                    (paramTy, paramSchemeInfo) <- case mAnnLambda of
                        Just (annNodeId, _) ->
                            case generalizeAtNode annNodeId of
                                Right (paramSch, _subst) ->
                                    let paramTyRaw = schemeToType paramSch
                                        paramTy0 = fromMaybe paramTyRaw (coercionDomainTy paramTyRaw)
                                    in pure
                                        ( paramTy0
                                        , SchemeInfo
                                            { siScheme = schemeFromType paramTy0
                                            , siSubst = IntMap.empty
                                            }
                                        )
                                Left (BindingTreeError GenSchemeFreeVars{}) -> do
                                    -- Some coercion codomain roots are outside the scheme
                                    -- ownership checks; recover the term-level type directly.
                                    paramTyRaw <- reifyNodeTypePreferringBound annNodeId
                                    let paramTy0 = fromMaybe paramTyRaw (coercionDomainTy paramTyRaw)
                                    pure
                                        ( paramTy0
                                        , SchemeInfo
                                            { siScheme = schemeFromType paramTy0
                                            , siSubst = IntMap.empty
                                            }
                                        )
                                Left err -> Left err
                        Nothing ->
                            pure
                                ( paramTySurface
                                , SchemeInfo
                                    { siScheme = schemeFromType paramTySurface
                                    , siSubst = IntMap.empty
                                    }
                                )
                    let env' = Map.insert v paramSchemeInfo env
                        bodyElab = elabTerm bodyElabOut
                    body' <- bodyElab env'
                    pure (ELam v paramTy body')
            in mkOut f
        AAppF (fAnn, fOut) (aAnn, aOut) funEid argEid _ ->
            let f env = do
                    f' <- elabTerm fOut env
                    a' <- elabTerm aOut env
                    funInst <- reifyInst namedSetReify env fAnn funEid
                    argInst <- reifyInst namedSetReify env aAnn argEid
                    let tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env) Map.empty
                        funInstByFunType =
                            case (funInst, TypeCheck.typeCheckWithEnv tcEnv f') of
                                (InstApp _, Right TForall{}) -> funInst
                                (InstApp _, Right _) -> InstId
                                _ -> funInst
                        sourceVarName annExpr = case annExpr of
                            AVar v _ -> Just v
                            AAnn inner _ _ -> sourceVarName inner
                            _ -> Nothing
                        funInstFromArg =
                            case funInstByFunType of
                                InstId -> do
                                    v <- sourceVarName fAnn
                                    si <- Map.lookup v env
                                    argTy <- either (const Nothing) Just (TypeCheck.typeCheckWithEnv tcEnv a')
                                    args <- inferFunInstArgsForArg (siScheme si) argTy
                                    pure (instSeqApps (map (inlineBoundVarsType resReify) args))
                                _ -> Nothing
                        funInstSeed =
                            case funInstFromArg of
                                Just inst -> inst
                                Nothing -> funInstByFunType
                    let funInst' =
                            case (funInstSeed, sourceAnnIsPolymorphic env aAnn) of
                                (InstApp (TVar _), False) ->
                                    case reifyNodeTypePreferringBound (annNode aAnn) of
                                        Right argTy -> InstApp argTy
                                        Left _ -> funInstSeed
                                (InstApp TForall{}, False) ->
                                    case reifyNodeTypePreferringBound (annNode aAnn) of
                                        Right argTy -> InstApp argTy
                                        Left _ -> funInstSeed
                                _ -> funInstSeed
                        fAppForArgInference = case funInst' of
                            InstId -> f'
                            _ -> ETyInst f' funInst'
                    let argInstFromFun =
                            let argTyChecked = TypeCheck.typeCheckWithEnv tcEnv a'
                                normalizeInferredInst inst0 =
                                    case (inst0, argTyChecked) of
                                        (InstApp argTy, Right (TForall _ (Just bnd) _))
                                            | alphaEqType (tyToElab bnd) argTy ->
                                                InstElim
                                        _ -> inst0
                            in case (sourceVarName aAnn, f') of
                                (Just v, ELam _ paramTy _) -> do
                                    si <- Map.lookup v env
                                    args <- inferInstAppArgs (siScheme si) paramTy
                                    pure (normalizeInferredInst (instSeqApps (map (inlineBoundVarsType resReify) args)))
                                (Just v, _) -> do
                                    si <- Map.lookup v env
                                    case TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference of
                                        Right (TArrow paramTy _) -> do
                                            args <- inferInstAppArgs (siScheme si) paramTy
                                            pure (normalizeInferredInst (instSeqApps (map (inlineBoundVarsType resReify) args)))
                                        _ -> Nothing
                                _ -> Nothing
                    let argInst' =
                            case (sourceAnnIsPolymorphic env aAnn, argInstFromFun) of
                                (True, Just inst) -> inst
                                _ -> argInst
                    let fApp = case funInst' of
                            InstId -> f'
                            _      -> ETyInst f' funInst'
                        aApp = case argInst' of
                            InstId -> a'
                            _      -> ETyInst a' argInst'
                    pure (EApp fApp aApp)
            in mkOut f
        ALetF v schemeGenId schemeRootId _ _rhsScopeGen (rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
            let elaborateLet env = do
                    rhs' <- elabTerm rhsOut env
                    let debugGeneralize = traceGeneralize traceCfg
                    _ <- pure $
                        debugGeneralize
                            ("elaborate let: schemeGenId=" ++ show schemeGenId
                                ++ " schemeRootId=" ++ show schemeRootId
                                ++ " scopeRoot=" ++ show (scopeRootForNode schemeRootId)
                            )
                            ()
                    (sch0, subst0) <- generalizeAtNode schemeRootId
                    let tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env) Map.empty
                        rhsIsApp =
                            case rhsAnn of
                                AApp{} -> True
                                _ -> False
                        rhsIsLam =
                            case rhsAnn of
                                ALam{} -> True
                                _ -> False
                        schemeUnbounded =
                            case sch0 of
                                Forall binds _ -> all ((== Nothing) . snd) binds
                        fallbackEligible = rhsIsApp && schemeUnbounded
                        stripForallsTy ty =
                            case ty of
                                TForall _ _ body -> stripForallsTy body
                                _ -> ty
                        hasIntCodomain ty =
                            case stripForallsTy ty of
                                TArrow _ (TBase (BaseTy "Int")) -> True
                                _ -> False
                        fallbackFromRhsTy rhsTy =
                            let freeVars = Set.toList (freeTypeVarsType rhsTy)
                                rhsSch = Forall [(name, Nothing) | name <- freeVars] rhsTy
                            in if alphaEqType (schemeToType rhsSch) (schemeToType sch0)
                                then Nothing
                                else Just rhsSch
                        rhsTypeChecked = TypeCheck.typeCheckWithEnv tcEnv rhs'
                        fallbackChoiceFromApp =
                            if fallbackEligible
                                then
                                    case rhsTypeChecked of
                                        Right rhsTy | hasIntCodomain rhsTy ->
                                            case fallbackFromRhsTy rhsTy of
                                                Just schApp -> Just (schApp, subst0)
                                                Nothing -> Nothing
                                        Right _ -> Nothing
                                        Left _ -> Nothing
                                else Nothing
                        fallbackChoiceFromLam =
                            if rhsIsLam
                                then
                                    case rhsTypeChecked of
                                        Right rhsTy ->
                                            case fallbackFromRhsTy rhsTy of
                                                Just schLam -> Just (schLam, IntMap.empty)
                                                Nothing -> Nothing
                                        Left _ -> Nothing
                                else Nothing
                        fallbackChoice =
                            case fallbackChoiceFromApp of
                                Just appChoice -> Just appChoice
                                Nothing -> fallbackChoiceFromLam
                        fallbackScheme = fmap fst fallbackChoice
                        sch = maybe sch0 fst fallbackChoice
                        subst = maybe subst0 snd fallbackChoice
                    case debugGeneralize
                        ("elaborate let: scheme=" ++ show sch
                            ++ " subst=" ++ show subst
                            ++ " fallback=" ++ show (fmap schemeToType fallbackScheme)
                        )
                        () of
                        () -> pure ()
                    let env' = Map.insert v SchemeInfo { siScheme = sch, siSubst = subst } env
                        rhsAbs = closeTermWithSchemeSubstIfNeeded subst sch rhs'
                    let bodyElab =
                            case bodyAnn of
                                AAnn _ target _ | canonical target == canonical trivialRoot ->
                                    elabStripped bodyOut
                                _ -> elabTerm bodyOut
                    body' <- bodyElab env'
                    pure (sch, rhsAbs, body')
                f env = do
                    (sch, rhsAbs, body') <- elaborateLet env
                    pure (ELet v sch rhsAbs body')
                fStripped env = do
                    (_, _rhsAbs, body') <- elaborateLet env
                    pure body'
            in ElabOut
                { elabTerm = f
                , elabStripped = fStripped
                }
        AAnnF (exprAnn, exprOut) annNodeId eid ->
            ElabOut
                { elabTerm = \env -> do
                    expr' <- elabTerm exprOut env
                    inst <- reifyInst namedSetReify env exprAnn eid
                    let mSourceSchemeInfo = schemeInfoForAnnExpr env exprAnn
                        sourceHeadBound =
                            case mSourceSchemeInfo of
                                Just SchemeInfo { siScheme = Forall ((_, mbBound) : _) _ } ->
                                    Just (maybe TBottom tyToElab mbBound)
                                _ -> Nothing
                    mExpectedBound <- case generalizeAtNode annNodeId of
                        Right (Forall ((_, Just bnd) : _) _, _) -> pure (Just (tyToElab bnd))
                        Left _ -> pure Nothing
                        _ -> pure Nothing
                    let instAdjusted0 = adjustAnnotationInst inst
                        instAdjusted =
                            case (mExpectedBound, instAdjusted0, sourceHeadBound) of
                                (Just expectedBound, InstInside (InstBot _), _) ->
                                    InstInside (InstBot expectedBound)
                                (Just expectedBound, InstId, Just sourceBound)
                                    | alphaEqType sourceBound TBottom ->
                                        InstInside (InstBot expectedBound)
                                    | alphaEqType sourceBound expectedBound ->
                                        InstId
                                _ -> instAdjusted0
                    exprClosed <-
                        if instAdjusted == InstId
                            then pure $
                                case mSourceSchemeInfo of
                                    Just si -> substInTerm (siSubst si) expr'
                                    Nothing -> expr'
                            else
                                if termStartsWithTypeAbstraction expr' || sourceAnnIsPolymorphic env exprAnn
                                    then pure expr'
                                    else pure (closeTermForAnnotation expr')
                    let instFinal =
                            case instAdjusted of
                                InstId -> InstId
                                _ ->
                                    case typeCheck exprClosed of
                                        Right tyExpr ->
                                            case tyExpr of
                                                TForall{} ->
                                                    case applyInstantiation tyExpr instAdjusted of
                                                        Right tyApplied
                                                            | alphaEqType tyApplied tyExpr -> InstId
                                                        _ -> instAdjusted
                                                _ -> InstId
                                        Left _ -> instAdjusted
                    pure $ case instFinal of
                        InstId -> exprClosed
                        _      -> ETyInst exprClosed instFinal
                , elabStripped = \env -> elabTerm exprOut env
                }

    sourceAnnIsPolymorphic :: Env -> AnnExpr -> Bool
    sourceAnnIsPolymorphic env sourceAnn =
        case sourceAnn of
            AVar v _ ->
                case Map.lookup v env of
                    Just SchemeInfo{ siScheme = Forall binds _ } -> not (null binds)
                    _ -> False
            AAnn inner _ _ -> sourceAnnIsPolymorphic env inner
            _ -> False

    schemeInfoForAnnExpr :: Env -> AnnExpr -> Maybe SchemeInfo
    schemeInfoForAnnExpr env sourceAnn =
        let generalizedSchemeInfo =
                case generalizeAtNode (annNode sourceAnn) of
                    Right (sch, subst) ->
                        Just SchemeInfo
                            { siScheme = sch
                            , siSubst = subst
                            }
                    Left _ -> Nothing
        in case sourceAnn of
            AVar v _ ->
                case Map.lookup v env of
                    Just si -> Just si
                    Nothing -> generalizedSchemeInfo
            _ -> generalizedSchemeInfo

    desugaredAnnLambdaInfo :: VarName -> AnnExpr -> Maybe (NodeId, AnnExpr)
    desugaredAnnLambdaInfo param bodyAnn =
        case bodyAnn of
            ALet letName _ _ _ _ rhsAnn innerBodyAnn _
                | letName == param ->
                    case rhsAnn of
                        AAnn rhsInner annNodeId _ | annRefersToVar param rhsInner ->
                            Just (annNodeId, innerBodyAnn)
                        _ -> Nothing
            _ -> Nothing

    annRefersToVar :: VarName -> AnnExpr -> Bool
    annRefersToVar name exprAnn =
        case exprAnn of
            AVar v _ -> v == name
            AAnn inner _ _ -> annRefersToVar name inner
            _ -> False

    -- | Convert an edge witness to an xMLF instantiation witness.
    --
    -- If the function position is a variable, we use its generalized scheme
    -- (and the NodeId→name substitution from `generalizeAt`) to support Σ(g)
    -- reordering and targeted instantiation.
    reifyInst :: IntSet.IntSet -> Env -> AnnExpr -> EdgeId -> Either ElabError Instantiation
    reifyInst _namedSetReify env funAnn (EdgeId eid) =
        let debugGeneralize :: String -> a -> a
            debugGeneralize = traceGeneralize traceCfg
            dbg =
                debugGeneralize
                    ("reifyInst: edge=" ++ show eid
                        ++ " witness=" ++ show (IntMap.member eid edgeWitnesses)
                        ++ " trace=" ++ show (IntMap.member eid edgeTraces)
                        ++ " exp=" ++ show (IntMap.member eid edgeExpansions)
                    )
                    ()
        in dbg `seq`
        case IntMap.lookup eid edgeWitnesses of
            Nothing ->
                debugGeneralize
                    ("reifyInst: missing witness for edge " ++ show eid)
                    (Right InstId)
            Just ew -> do
                let mTrace = IntMap.lookup eid edgeTraces
                let mSchemeInfo = schemeInfoForAnnExpr env funAnn
                    mSchemeInfo' =
                        case mSchemeInfo of
                            Just si
                                | IntMap.null (siSubst si)
                                , Forall binds _ <- siScheme si
                                , null binds ->
                                    Nothing
                            _ -> mSchemeInfo
                case debugGeneralize
                    ("reifyInst scheme edge=" ++ show eid
                        ++ " source=" ++ show (fmap siSubst mSchemeInfo)
                    )
                    () of
                    () -> pure ()
                phi <- phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith resReify (Just gaParents) mSchemeInfo' mTrace ew
                case debugGeneralize
                    ("reifyInst phi edge=" ++ show eid ++ " phi=" ++ show phi)
                    () of
                    () -> pure ()
                Right phi

    inferInstAppArgs :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferInstAppArgs scheme targetTy =
        let (binds, body) = Inst.splitForalls (schemeToType scheme)
            binderNames = map fst binds
        in case matchType (Set.fromList binderNames) body targetTy of
            Left _ -> Nothing
            Right subst ->
                if all (`Map.member` subst) binderNames
                    then Just [ty | name <- binderNames, Just ty <- [Map.lookup name subst]]
                    else Nothing

    inferFunInstArgsForArg :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferFunInstArgsForArg scheme argTy =
        let (binds, body) = Inst.splitForalls (schemeToType scheme)
            binderNames = map fst binds
            binderSet = Set.fromList binderNames
        in case body of
            TArrow paramTy _ ->
                case matchType binderSet paramTy argTy of
                    Left _ -> Nothing
                    Right subst ->
                        let present = map (`Map.member` subst) binderNames
                            prefixLen = length (takeWhile id present)
                            hasOutOfOrder = or (drop prefixLen present)
                            prefixNames = take prefixLen binderNames
                            args = [ty | name <- prefixNames, Just ty <- [Map.lookup name subst]]
                        in if hasOutOfOrder || null args
                            then Nothing
                            else Just args
            _ -> Nothing

instSeqApps :: [ElabType] -> Instantiation
instSeqApps tys = case map InstApp tys of
    [] -> InstId
    [inst] -> inst
    insts -> foldr1 InstSeq insts

reifyTypeForParam :: IntSet.IntSet -> SolveResult -> NodeId -> Either ElabError ElabType
reifyTypeForParam namedSet res nid = do
    ty <- reifyTypeWithNamedSetNoFallback res IntMap.empty namedSet nid
    let ty' = inlineBaseBounds res ty
    pure (inlineBoundVarsType res ty')

inlineBaseBounds :: SolveResult -> ElabType -> ElabType
inlineBaseBounds res =
    inlineBaseBoundsType
        (srConstraint res)
        (Solve.frWith (srUnionFind res))

annNode :: AnnExpr -> NodeId
annNode ann = case ann of
    ALit _ nid -> nid
    AVar _ nid -> nid
    ALam _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid
