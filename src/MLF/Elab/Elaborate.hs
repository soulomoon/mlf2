{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module MLF.Elab.Elaborate (
    ElabConfig(ElabConfig, ecTraceConfig, ecGeneralizeAtWith),
    ElabEnv(..),
    expansionToInst,
    elaborateWithEnv
) where

import Control.Applicative ((<|>))
import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Reify.TypeOps
    ( alphaEqType
    , freeTypeVarsType
    , inlineBaseBoundsType
    , parseNameId
    )

import MLF.Frontend.Syntax (VarName)
import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Legacy (expansionToInst)
import MLF.Elab.TermClosure (alignTermTypeVarsToScheme, alignTermTypeVarsToTopTyAbs, closeTermWithSchemeSubstIfNeeded)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType, simplifyAnnotationType)
import MLF.Elab.Run.Annotation (adjustAnnotationInst)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import MLF.Elab.Run.Scope (generalizeTargetNode, schemeBodyTarget)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.TypeCheck (typeCheck)
import qualified MLF.Elab.TypeCheck as TypeCheck (Env(..), typeCheckWithEnv)
import qualified MLF.Elab.Inst as Inst
import MLF.Reify.Core
    ( namedNodes
    , reifyTypeWithNamedSetNoFallback
    )
import MLF.Constraint.Presolution
    ( PresolutionView(..)
    , EdgeTrace(..)
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr(..), AnnExprF(..))

type GeneralizeAtWith =
    Maybe GaBindParents
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

data ElabConfig = ElabConfig
    { ecTraceConfig :: TraceConfig
    , ecGeneralizeAtWith :: GeneralizeAtWith
    }

data ElabEnv = ElabEnv
    { eePresolutionView :: PresolutionView
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

elaborateWithEnv
    :: ElabConfig
    -> ElabEnv
    -> AnnExpr
    -> Either ElabError ElabTerm
elaborateWithEnv config elabEnv ann = do
    namedSet <- namedNodes presolutionView
    let namedSetPhi = namedSet
        namedSetReify = namedSet
    let ElabOut { elabTerm = runElab } = para (elabAlg namedSetPhi namedSetReify) ann
    runElab Map.empty
  where
    ElabConfig
        { ecTraceConfig = traceCfg
        , ecGeneralizeAtWith = generalizeAtWithRaw
        } = config
    presolutionView = eePresolutionView elabEnv
    gaParents = eeGaParents elabEnv
    edgeWitnesses = eeEdgeWitnesses elabEnv
    edgeTraces = eeEdgeTraces elabEnv
    edgeExpansions = eeEdgeExpansions elabEnv
    scopeOverrides = eeScopeOverrides elabEnv
    canonical = ChiQuery.chiCanonical presolutionView
    chiLookupNode = ChiQuery.chiLookupNode presolutionView
    chiLookupVarBound = ChiQuery.chiLookupVarBound presolutionView
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
        -> NodeRef
        -> NodeId
        -> Either ElabError (ElabScheme, IntMap.IntMap String)
    generalizeAtWith mbGa scopeRoot targetNode =
        generalizeAtWithRaw mbGa scopeRoot targetNode

    scopeRootForNode :: NodeId -> NodeRef
    scopeRootForNode nodeId =
        case IntMap.lookup (getNodeId (canonical nodeId)) scopeOverrides of
            Just ref -> ref
            Nothing -> scopeRootFromBase nodeId

    generalizeAtNode :: NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
    generalizeAtNode nodeId =
        let scopeRoot = scopeRootForNode nodeId
            targetC = generalizeTargetNode presolutionView nodeId
        in generalizeAtWith (Just gaParents) scopeRoot targetC

    normalizeSchemeSubstPair :: (ElabScheme, IntMap.IntMap String) -> (ElabScheme, IntMap.IntMap String)
    normalizeSchemeSubstPair (schRaw, substRaw) =
        let sch = schemeFromType (schemeToType schRaw)
            subst = normalizeSubstForScheme sch substRaw
        in (sch, subst)

    normalizeSubstForScheme :: ElabScheme -> IntMap.IntMap String -> IntMap.IntMap String
    normalizeSubstForScheme sch substRaw =
        let (binds, _) = Inst.splitForalls (schemeToType sch)
        in foldl'
            (\acc (name, _) ->
                if name `elem` IntMap.elems acc
                    then acc
                    else case parseNameId name of
                        Just nid -> IntMap.insertWith (\_ old -> old) nid name acc
                        Nothing -> acc
            )
            substRaw
            binds

    reifyNodeTypePreferringBound :: NodeId -> Either ElabError ElabType
    reifyNodeTypePreferringBound nodeId = do
        namedSet <- namedNodes presolutionView
        let nodeC = canonical nodeId
        case chiLookupVarBound nodeC of
            Just bnd -> reifyTypeForParam presolutionView namedSet bnd
            Nothing -> reifyTypeForParam presolutionView namedSet nodeC

    closeTermForAnnotation :: ElabTerm -> ElabTerm
    closeTermForAnnotation term =
        case typeCheck term of
            Right ty ->
                let freeVars = Set.toList (freeTypeVarsType ty)
                    sch = Forall [(v, Nothing) | v <- freeVars] ty
                in closeTermWithSchemeSubstIfNeeded IntMap.empty sch term
            Left _ -> term

    stripUnusedTopTyAbs :: ElabTerm -> ElabTerm
    stripUnusedTopTyAbs term =
        case term of
            ETyAbs v mbBound body ->
                let body' = stripUnusedTopTyAbs body
                    term' = ETyAbs v mbBound body'
                in case typeCheck term' of
                    Right (TForall _ _ bodyTy)
                        | v `notElem` freeTypeVarsType bodyTy -> body'
                    _ -> term'
            _ -> term

    resolvedLambdaParamNode :: NodeId -> Maybe NodeId
    resolvedLambdaParamNode lamNodeId =
        let lamC = canonical lamNodeId
        in case chiLookupNode lamC of
            Just TyArrow{ tnDom = dom } -> Just dom
            Just TyVar{ tnBound = Just bnd } ->
                case chiLookupNode (canonical bnd) of
                    Just TyArrow{ tnDom = dom } -> Just dom
                    _ -> Nothing
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
                    paramSource <- case mAnnLambda of
                        Just _ -> pure (fromMaybe paramNode resolvedParam)
                        Nothing ->
                            case resolvedParam of
                                Nothing -> pure paramNode
                                Just resolvedNode ->
                                    case reifyNodeTypePreferringBound resolvedNode of
                                        Right (TVar name)
                                            | isJust (parseNameId name) ->
                                                -- Keep the original param node when the resolved
                                                -- domain is still an unconstrained solver TVar.
                                                pure paramNode
                                        _ -> pure resolvedNode
                    let
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
                                    -- When the generalized scheme is a trivially bounded
                                    -- forall ∀(a:B).a (body is just the binder variable),
                                    -- collapse to the bound type B.  This avoids wrapping
                                    -- monomorphic annotations in bounded foralls, which
                                    -- would produce InstApp on non-⊥ bounds that the
                                    -- strict type checker rejects.
                                    let paramTy0 = case paramSch of
                                            Forall [(name, Just bnd)] bodyTy
                                                | bodyTy == TVar name -> tyToElab bnd
                                            _ -> schemeToType paramSch
                                    in pure
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
                    let funInst' =
                            case (funInstByFunType, sourceAnnIsPolymorphic env aAnn) of
                                (InstApp (TVar _), False) ->
                                    case reifyNodeTypePreferringBound (annNode aAnn) of
                                        Right argTy -> InstApp argTy
                                        Left _ -> funInstByFunType
                                (InstApp TForall{}, False) ->
                                    case reifyNodeTypePreferringBound (annNode aAnn) of
                                        Right argTy -> InstApp argTy
                                        Left _ -> funInstByFunType
                                _ -> funInstByFunType
                        normalizeFunInst inst0 =
                            case TypeCheck.typeCheckWithEnv tcEnv f' of
                                Right fTy -> go 0 inst0
                                  where
                                    go n instN
                                        | n >= (8 :: Int) = instN
                                        | otherwise =
                                            case applyInstantiation fTy instN of
                                                Right TForall{} -> go (n + 1) (InstSeq instN InstElim)
                                                Right _ -> instN
                                                Left _ -> instN
                                Left _ -> inst0
                        funInstNorm = normalizeFunInst funInst'
                        funInstRecovered =
                            let fApp0 = case funInstNorm of
                                    InstId -> f'
                                    _ -> ETyInst f' funInstNorm
                            in case (TypeCheck.typeCheckWithEnv tcEnv (EApp fApp0 a'), sourceVarName fAnn, sourceVarName aAnn, TypeCheck.typeCheckWithEnv tcEnv a') of
                                (Right (TArrow _ TBottom), Just fName, mArgName, Right argTy) ->
                                    case Map.lookup fName env of
                                        Just fSi ->
                                            let argTyPreferred =
                                                    case mArgName >>= (`Map.lookup` env) of
                                                        Just argSi ->
                                                            case Inst.splitForalls (schemeToType (siScheme argSi)) of
                                                                ([], monoTy) -> monoTy
                                                                _ -> argTy
                                                        Nothing -> argTy
                                                (fBinds, fBodyTy) = Inst.splitForalls (schemeToType (siScheme fSi))
                                                fBinderNames = map fst fBinds
                                            in case fBodyTy of
                                                TArrow (TVar headBinder) retTy
                                                    | headBinder `elem` fBinderNames
                                                        && Set.member headBinder (freeTypeVarsType retTy) ->
                                                            normalizeFunInst (InstApp argTyPreferred)
                                                _ -> funInstNorm
                                        Nothing -> funInstNorm
                                _ -> funInstNorm
                        fAppForArgInference = case funInstRecovered of
                            InstId -> f'
                            _ -> ETyInst f' funInstRecovered
                        sourceVarName annExpr = case annExpr of
                            AVar v _ -> Just v
                            AAnn inner _ _ -> sourceVarName inner
                            _ -> Nothing
                    let argInstFromFun =
                            let shouldInlineParamTy =
                                    case (sourceVarName fAnn, sourceVarName aAnn) of
                                        (Just fName, Just argName) -> fName /= argName
                                        _ -> False
                            in case (sourceVarName aAnn, f') of
                                (Just v, ELam _ paramTy _) -> do
                                    si <- Map.lookup v env
                                    let paramTy' =
                                            if shouldInlineParamTy
                                                then inlineBoundVarsType presolutionView paramTy
                                                else paramTy
                                    args <- inferInstAppArgs (siScheme si) paramTy'
                                    pure (instSeqApps args)
                                (Just v, _) -> do
                                    si <- Map.lookup v env
                                    case TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference of
                                        Right (TArrow paramTy _) -> do
                                            let paramTy' =
                                                    if shouldInlineParamTy
                                                        then inlineBoundVarsType presolutionView paramTy
                                                        else paramTy
                                            args <- inferInstAppArgs (siScheme si) paramTy'
                                            pure (instSeqApps args)
                                        _ -> Nothing
                                _ -> Nothing
                    let argInst' =
                            case (sourceAnnIsPolymorphic env aAnn, argInstFromFun) of
                                (True, Just inst) -> inst
                                _ -> argInst
                        -- When the elaborated argument is a forall with a non-⊥
                        -- bound (e.g. an annotation updated the bound), use
                        -- InstElim to eliminate the forall by substituting the
                        -- bound.  This avoids applying InstApp/InstBot to a
                        -- non-⊥ bound which the strict type checker rejects.
                        -- When the argument is already monomorphic, skip the
                        -- instantiation entirely.
                        argInstFinal = case argInst' of
                            InstId -> InstId
                            _ -> case TypeCheck.typeCheckWithEnv tcEnv a' of
                                Right (TForall _ (Just _) _) -> InstElim
                                Right TForall{} -> argInst'
                                _ -> InstId
                    let fApp = case funInstRecovered of
                            InstId -> f'
                            _      -> ETyInst f' funInstRecovered
                        aApp = case argInstFinal of
                            InstId -> a'
                            _      -> ETyInst a' argInstFinal
                    pure (EApp fApp aApp)
            in mkOut f
        ALetF v schemeGenId schemeRootId _ _rhsScopeGen (rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
            let elaborateLet env = do
                    rhs' <- elabTerm rhsOut env
                    let debugGeneralize = traceGeneralize traceCfg
                    _ <- pure $
                        debugGeneralize
                            ("elaborate let(" ++ v ++ "): schemeGenId=" ++ show schemeGenId
                                ++ " schemeRootId=" ++ show schemeRootId
                                ++ " scopeRoot=" ++ show (scopeRootForNode schemeRootId)
                            )
                            ()
                    (sch0Raw, subst0Raw) <- generalizeAtNode schemeRootId
                    let tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env) Map.empty
                        lambdaParamNodes annExpr =
                            case annExpr of
                                ALam _ paramNode _ body _ -> paramNode : lambdaParamNodes body
                                AAnn inner _ _ -> lambdaParamNodes inner
                                _ -> []
                        deriveLambdaBinderSubst scheme0 subst0' =
                            let (binds, _) = Inst.splitForalls (schemeToType scheme0)
                                binderNames = map fst binds
                                binderBounds = map snd binds
                                paramNodes = lambdaParamNodes rhsAnn
                                binderPairs = zip binderNames paramNodes
                                canAugment =
                                    length binderNames == length paramNodes
                                        && all (== Nothing) binderBounds
                            in if canAugment
                                then
                                    foldl'
                                        (\acc (name, paramNode) ->
                                            let key = getNodeId (canonical paramNode)
                                            in IntMap.insertWith (\_ old -> old) key name acc
                                        )
                                        subst0'
                                        binderPairs
                                else subst0'
                        (sch0Norm, subst0Norm) = normalizeSchemeSubstPair (sch0Raw, subst0Raw)
                        sch = schemeFromType (simplifyAnnotationType (schemeToType sch0Norm))
                        subst0 = normalizeSubstForScheme sch (deriveLambdaBinderSubst sch0Norm subst0Norm)
                        subst =
                            let (binds, _) = Inst.splitForalls (schemeToType sch)
                            in if null binds then IntMap.empty else subst0
                        rhsAbs0 = closeTermWithSchemeSubstIfNeeded subst sch rhs'
                        rhsAbs =
                            case sch of
                                Forall binds _ | not (null binds) -> rhsAbs0
                                _ -> stripUnusedTopTyAbs rhsAbs0
                        rhsAbsTyChecked = TypeCheck.typeCheckWithEnv tcEnv rhsAbs
                    case debugGeneralize
                        ("elaborate let(" ++ v ++ "): sch0=" ++ show sch
                            ++ " subst0=" ++ show subst
                            ++ " scheme=" ++ show sch
                            ++ " subst=" ++ show subst
                            ++ " rhsAbsTy=" ++ show rhsAbsTyChecked
                        )
                        () of
                        () -> pure ()
                    let env' = Map.insert v SchemeInfo { siScheme = sch, siSubst = subst } env
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
                    expectedSchemeInfo <- case generalizeAtNode annNodeId of
                        Right pair -> pure (Just pair)
                        Left _ -> pure Nothing
                    let sourceSchemeInfo = sourceAnnSchemeInfo env exprAnn
                        canReuseSourceScheme =
                            case (sourceSchemeInfo, expectedSchemeInfo) of
                                (Just si, Just (schExpected, _substExpected)) ->
                                    alphaEqType (schemeToType (siScheme si)) (schemeToType schExpected)
                                _ -> False
                        requiresExplicitAnnotationInst =
                            case (sourceSchemeInfo, expectedSchemeInfo) of
                                (Just SchemeInfo{ siScheme = srcScheme }, Just (schExpected, _substExpected)) ->
                                    let sourcePoly = case srcScheme of
                                            Forall binds _ -> not (null binds)
                                    in sourcePoly
                                        && not (alphaEqType (schemeToType srcScheme) (schemeToType schExpected))
                                _ -> False
                    inst <-
                        case reifyInst namedSetReify env exprAnn eid of
                            Right inst0 -> pure inst0
                            Left (PhiTranslatabilityError _)
                                | canReuseSourceScheme -> pure InstId
                            Left err -> Left err
                    let expectedSchemeResult = fmap fst expectedSchemeInfo
                    let mExpectedBound = case expectedSchemeResult of
                            Just (Forall ((_, Just bnd) : _) _) -> Just (tyToElab bnd)
                            _ -> Nothing
                    let dropAnnotationElims inst0 = case inst0 of
                            InstElim -> InstId
                            InstSeq a b ->
                                let a' = dropAnnotationElims a
                                    b' = dropAnnotationElims b
                                in case (a', b') of
                                    (InstId, x) -> x
                                    (x, InstId) -> x
                                    _ -> InstSeq a' b'
                            InstInside a -> InstInside (dropAnnotationElims a)
                            InstUnder v a -> InstUnder v (dropAnnotationElims a)
                            _ -> inst0
                        preservesForalls =
                            isJust mExpectedBound
                                || isJust (alignTermTypeVarsToTopTyAbs expr')
                                || case expr' of
                                    ETyAbs{} -> True
                                    _ -> False
                        instAdjusted0 =
                            if preservesForalls
                                then adjustAnnotationInst inst
                                else dropAnnotationElims inst
                        instAdjusted =
                            case (mExpectedBound, instAdjusted0) of
                                (Just expectedBound, InstInside (InstBot _)) ->
                                    InstInside (InstBot expectedBound)
                                _ -> instAdjusted0
                    exprClosed0 <-
                        if instAdjusted == InstId && requiresExplicitAnnotationInst
                            then
                                Left
                                    (PhiTranslatabilityError
                                        [ "AAnnF: missing authoritative instantiation for annotation edge " ++ show eid
                                        ]
                                    )
                        else if instAdjusted == InstId
                            then
                                if canReuseSourceScheme && sourceAnnIsPolymorphic env exprAnn
                                    then pure expr'
                                else case expectedSchemeInfo of
                                    Just (schExpected, substExpected) ->
                                        case expr' of
                                            ETyAbs{} ->
                                                pure (fromMaybe expr' (alignTermTypeVarsToScheme schExpected expr' <|> alignTermTypeVarsToTopTyAbs expr'))
                                            _ -> pure (closeTermWithSchemeSubstIfNeeded substExpected schExpected expr')
                                    Nothing -> pure (fromMaybe expr' (alignTermTypeVarsToTopTyAbs expr'))
                            else
                                let instHasUnder inst0 = case inst0 of
                                        InstUnder{} -> True
                                        InstSeq a b -> instHasUnder a || instHasUnder b
                                        InstInside a -> instHasUnder a
                                        _ -> False
                                in if sourceAnnIsPolymorphic env exprAnn
                                    then pure expr'
                                    else if instHasUnder instAdjusted
                                        then
                                            case expectedSchemeInfo of
                                                Just (schExpected, substExpected) ->
                                                    pure (closeTermWithSchemeSubstIfNeeded substExpected schExpected expr')
                                                Nothing -> pure (closeTermForAnnotation expr')
                                        else pure (closeTermForAnnotation expr')
                    let exprClosed = stripUnusedTopTyAbs exprClosed0
                    let instFinal =
                            case instAdjusted of
                                InstId -> InstId
                                _ ->
                                    case typeCheck exprClosed of
                                        Right tyExpr
                                            | Just expectedScheme <- expectedSchemeResult
                                            , alphaEqType tyExpr (schemeToType expectedScheme) ->
                                                InstId
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

    sourceAnnSchemeInfo :: Env -> AnnExpr -> Maybe SchemeInfo
    sourceAnnSchemeInfo env sourceAnn =
        case sourceAnn of
            AVar v _ -> Map.lookup v env
            AAnn inner _ _ -> sourceAnnSchemeInfo env inner
            _ -> Nothing

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
    reifyInst namedSetReify env funAnn (EdgeId eid) =
        let debugGeneralize :: String -> a -> a
            debugGeneralize = traceGeneralize traceCfg
            schemeInfoForInst annExpr =
                case annExpr of
                    AVar v _ -> Map.lookup v env
                    AAnn inner _ _ -> schemeInfoForInst inner
                    _ -> Nothing
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
                    mExpansion = IntMap.lookup eid edgeExpansions
                let mSchemeInfo = schemeInfoForInst funAnn
                    mSchemeInfo' = mSchemeInfo
                case debugGeneralize
                    ("reifyInst scheme edge=" ++ show eid
                        ++ " source=" ++ show (fmap siSubst mSchemeInfo)
                    )
                    () of
                    () -> pure ()
                phi0 <- phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith presolutionView (Just gaParents) mSchemeInfo' mTrace ew
                let substForPhi = maybe IntMap.empty siSubst mSchemeInfo
                    resolvePhiVar v = do
                        nid <- parseNameId v
                        bnd <- chiLookupVarBound (canonical (NodeId nid))
                        either (const Nothing) Just
                            (reifyTypeWithNamedSetNoFallback presolutionView substForPhi namedSetReify bnd)
                    normalizePhiInst inst = case inst of
                        InstApp (TVar v) -> maybe inst InstApp (resolvePhiVar v)
                        InstBot (TVar v) -> maybe inst InstBot (resolvePhiVar v)
                        _ -> inst
                    phi = normalizePhiInst phi0
                case debugGeneralize
                    ("reifyInst phi edge=" ++ show eid ++ " phi=" ++ show phi)
                    () of
                    () -> pure ()
                instFromAuthority <- case (mExpansion, mSchemeInfo) of
                    (Just (ExpInstantiate args), Just si) -> do
                        let schemeArity =
                                case siScheme si of
                                    Forall binds _ -> length binds
                            targetTy = authoritativeTargetType namedSetReify ew si
                            traceArgs =
                                case mTrace of
                                    Just tr | not (null (etBinderArgs tr)) ->
                                        reifyTraceBinderInstArgs namedSetReify si (map snd (etBinderArgs tr))
                                    _ -> Nothing
                            targetArgs =
                                if schemeArity == 0
                                    then Nothing
                                    else inferAuthoritativeInstArgs namedSetReify ew si
                            authoritativeArgs =
                                case targetArgs of
                                    Just inferred -> Just inferred
                                    Nothing -> traceArgs
                            shouldRefine =
                                instNeedsAuthoritativeRefinement phi
                                    || case targetTy of
                                        Just ty -> not (alphaEqType ty (schemeToType (siScheme si)))
                                        Nothing -> phi == InstId
                        case debugGeneralize
                            ("reifyInst authoritative edge=" ++ show eid
                                ++ " expansionArity=" ++ show (length args)
                                ++ " schemeArity=" ++ show schemeArity
                                ++ " targetTy=" ++ show targetTy
                                ++ " targetArgs=" ++ show targetArgs
                                ++ " traceArgs=" ++ show traceArgs
                                ++ " shouldRefine=" ++ show shouldRefine
                            )
                            () of
                            () -> pure ()
                        case authoritativeArgs of
                            Just (_binds, inferred)
                                | shouldRefine
                                , schemeArity > 0
                                , not (null inferred) ->
                                    let usableLen = min schemeArity (length inferred)
                                    in pure
                                        (Just
                                            ( instSeqApps
                                                (map (inlineBoundVarsType presolutionView) (take usableLen inferred))
                                            )
                                        )
                            _
                                | shouldRefine
                                , schemeArity > 0 ->
                                    Left
                                        (PhiTranslatabilityError
                                            [ "reifyInst: missing authoritative instantiation translation for edge " ++ show eid
                                            , "expansion args=" ++ show args
                                            ]
                                        )
                            _ -> pure Nothing
                    _ -> pure Nothing
                case instFromAuthority of
                    Just inst -> Right inst
                    Nothing -> Right phi

    inferAuthoritativeInstArgs :: IntSet.IntSet -> EdgeWitness -> SchemeInfo -> Maybe ([(String, Maybe BoundType)], [ElabType])
    inferAuthoritativeInstArgs namedSetReify ew si =
        case inferFromNode (ewRight ew) of
            Just args -> Just args
            Nothing -> inferFromNode (ewLeft ew)
      where
        inferFromNode nid =
            case reifyTargetType namedSetReify si nid of
                Right targetTy ->
                    let (binds, body) = Inst.splitForalls (schemeToType (siScheme si))
                    in fmap ((,) binds) (inferInstAppArgsFromScheme binds body targetTy)
                Left _ -> Nothing

    authoritativeTargetType :: IntSet.IntSet -> EdgeWitness -> SchemeInfo -> Maybe ElabType
    authoritativeTargetType namedSetReify ew si =
        case reifyTargetNodeType namedSetReify si (ewRight ew) of
            Right targetTy -> Just targetTy
            Left _ ->
                case reifyTargetNodeType namedSetReify si (ewLeft ew) of
                    Right targetTy -> Just targetTy
                    Left _ -> Nothing

    reifyTraceBinderInstArgs :: IntSet.IntSet -> SchemeInfo -> [NodeId] -> Maybe ([(String, Maybe BoundType)], [ElabType])
    reifyTraceBinderInstArgs namedSetReify si nodes0 =
        let (binds, _) = Inst.splitForalls (schemeToType (siScheme si))
        in fmap ((,) binds) (mapM reifyArg nodes0)
      where
        subst = siSubst si
        reifyArg nid =
            let nidC = canonical nid
                tyE =
                    case chiLookupVarBound nidC of
                        Just bnd -> reifyTypeWithNamedSetNoFallback presolutionView subst namedSetReify bnd
                        Nothing -> reifyTypeWithNamedSetNoFallback presolutionView subst namedSetReify nidC
            in either (const Nothing) Just tyE

    reifyTargetType :: IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
    reifyTargetType namedSetReify si nid =
        let subst = siSubst si
            targetNode = schemeBodyTarget presolutionView nid
        in reifyTypeWithNamedSetNoFallback presolutionView subst namedSetReify targetNode

    reifyTargetNodeType :: IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
    reifyTargetNodeType namedSetReify si nid =
        let subst = siSubst si
            targetNode = canonical nid
        in reifyTypeWithNamedSetNoFallback presolutionView subst namedSetReify targetNode

    inferInstAppArgs :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferInstAppArgs scheme targetTy =
        let (binds, body) = Inst.splitForalls (schemeToType scheme)
        in inferInstAppArgsFromScheme binds body targetTy

    instNeedsAuthoritativeRefinement :: Instantiation -> Bool
    instNeedsAuthoritativeRefinement inst =
        case collectApps inst of
            Just tys -> any isPlaceholderTy tys
            Nothing -> False
      where
        isPlaceholderTy ty = case ty of
            TVar _ -> True
            _ -> False
        collectApps inner = case inner of
            InstId -> Just []
            InstApp ty -> Just [ty]
            InstSeq a b -> (++) <$> collectApps a <*> collectApps b
            _ -> Nothing


instSeqApps :: [ElabType] -> Instantiation
instSeqApps tys = case map InstApp tys of
    [] -> InstId
    [inst] -> inst
    insts -> foldr1 InstSeq insts

reifyTypeForParam :: PresolutionView -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeForParam presolutionView namedSet nid = do
    ty <- reifyTypeWithNamedSetNoFallback presolutionView IntMap.empty namedSet nid
    let ty' = inlineBaseBounds presolutionView ty
    pure (inlineBoundVarsType presolutionView ty')

inlineBaseBounds :: PresolutionView -> ElabType -> ElabType
inlineBaseBounds presolutionView =
    inlineBaseBoundsType
        (ChiQuery.chiConstraint presolutionView)
        (ChiQuery.chiCanonical presolutionView)

annNode :: AnnExpr -> NodeId
annNode ann = case ann of
    ALit _ nid -> nid
    AVar _ nid -> nid
    ALam _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid
