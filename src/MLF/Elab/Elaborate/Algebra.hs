{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
module MLF.Elab.Elaborate.Algebra (
    Env,
    ElabOut(..),
    AlgebraContext(..),
    elabAlg,
    resolvedLambdaParamNode
) where

import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)

import MLF.Constraint.Presolution (PresolutionView)
import MLF.Constraint.Types
    ( NodeId
    , TyNode(..)
    , getNodeId
    )
import MLF.Elab.Elaborate.Annotation
    ( AnnotationContext(..)
    , desugaredAnnLambdaInfo
    , elaborateAnnotationTerm
    , instSeqApps
    , reifyInst
    , sourceAnnIsPolymorphic
    , stripUnusedTopTyAbs
    )
import MLF.Elab.Elaborate.Scope
    ( generalizeAtNode
    , normalizeSchemeSubstPair
    , normalizeSubstForScheme
    , reifyNodeTypePreferringBound
    , scopeRootForNode
    )
import qualified MLF.Elab.Inst as Inst
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType, simplifyAnnotationType)
import MLF.Elab.TermClosure (closeTermWithSchemeSubstIfNeeded)
import qualified MLF.Elab.TypeCheck as TypeCheck (Env(..), typeCheckWithEnv)
import MLF.Elab.Types
    ( ElabError(..)
    , ElabTerm(..)
    , Instantiation(..)
    , SchemeInfo(..)
    , Ty(..)
    , pattern Forall
    , schemeFromType
    , tyToElab
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr(..), AnnExprF(..))
import MLF.Frontend.Syntax (VarName)
import MLF.Reify.TypeOps (freeTypeVarsType, parseNameId)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

type Env = Map.Map VarName SchemeInfo

data ElabOut = ElabOut
    { elabTerm :: Env -> Either ElabError ElabTerm
    , elabStripped :: Env -> Either ElabError ElabTerm
    }

data AlgebraContext = AlgebraContext
    { algPresolutionView :: PresolutionView
    , algTraceConfig :: TraceConfig
    , algCanonical :: NodeId -> NodeId
    , algResolvedLambdaParamNode :: NodeId -> Maybe NodeId
    , algAnnotationContext :: AnnotationContext
    , algNamedSetReify :: IntSet.IntSet
    }

elabAlg :: AlgebraContext -> AnnExprF (AnnExpr, ElabOut) -> ElabOut
elabAlg algebraContext layer =
    case layer of
        AVarF v _ -> mkOut $ \env ->
            maybe (Left (EnvLookup v)) (const (Right (EVar v))) (Map.lookup v env)
        ALitF lit _ -> mkOut $ \_ -> Right (ELit lit)
        ALamF v paramNode _ (bodyAnn, bodyOut) lamNodeId ->
            let f env = do
                    let mAnnLambda = desugaredAnnLambdaInfo v bodyAnn
                        resolvedParam = algResolvedLambdaParamNode algebraContext lamNodeId
                    paramSource <-
                        case mAnnLambda of
                            Just _ -> pure (fromMaybe paramNode resolvedParam)
                            Nothing ->
                                case resolvedParam of
                                    Nothing -> pure paramNode
                                    Just resolvedNode ->
                                        case reifyNodeTypePreferringBound scopeContext resolvedNode of
                                            Right (TVar name)
                                                | isJust (parseNameId name) -> pure paramNode
                                            _ -> pure resolvedNode
                    let bodyElabOut =
                            case mAnnLambda of
                                Just (_, innerBodyAnn) -> para (elabAlg algebraContext) innerBodyAnn
                                Nothing -> bodyOut
                    paramTySurface <- reifyNodeTypePreferringBound scopeContext paramSource
                    (paramTy, paramSchemeInfo) <-
                        case mAnnLambda of
                            Just (annNodeId, _) ->
                                case generalizeAtNode scopeContext annNodeId of
                                    Right (paramScheme, _subst) ->
                                        let paramTy0 = case paramScheme of
                                                Forall [(name, Just bnd)] bodyTy
                                                    | bodyTy == TVar name -> tyToElab bnd
                                                _ -> schemeToType paramScheme
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
                    body' <- elabTerm bodyElabOut env'
                    pure (ELam v paramTy body')
            in mkOut f
        AAppF (fAnn, fOut) (aAnn, aOut) funEid argEid _ ->
            let f env = do
                    f' <- elabTerm fOut env
                    a' <- elabTerm aOut env
                    funInst <- reifyInst annotationContext namedSetReify env fAnn funEid
                    argInst <- reifyInst annotationContext namedSetReify env aAnn argEid
                    let tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env) Map.empty
                        funInstByFunType =
                            case (funInst, TypeCheck.typeCheckWithEnv tcEnv f') of
                                (InstApp _, Right TForall{}) -> funInst
                                (InstApp _, Right _) -> InstId
                                _ -> funInst
                        funInst' =
                            case (funInstByFunType, sourceAnnIsPolymorphic env aAnn) of
                                (InstApp (TVar _), False) ->
                                    case reifyNodeTypePreferringBound scopeContext (annNode aAnn) of
                                        Right argTy -> InstApp argTy
                                        Left _ -> funInstByFunType
                                (InstApp TForall{}, False) ->
                                    case reifyNodeTypePreferringBound scopeContext (annNode aAnn) of
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
                            in case ( TypeCheck.typeCheckWithEnv tcEnv (EApp fApp0 a')
                                    , sourceVarName fAnn
                                    , sourceVarName aAnn
                                    , TypeCheck.typeCheckWithEnv tcEnv a'
                                    ) of
                                (Right (TArrow _ TBottom), Just fName, mArgName, Right argTy) ->
                                    case Map.lookup fName env of
                                        Just fSchemeInfo ->
                                            let argTyPreferred =
                                                    case mArgName >>= (`Map.lookup` env) of
                                                        Just argSchemeInfo ->
                                                            case Inst.splitForalls (schemeToType (siScheme argSchemeInfo)) of
                                                                ([], monoTy) -> monoTy
                                                                _ -> argTy
                                                        Nothing -> argTy
                                                (fBinds, fBodyTy) = Inst.splitForalls (schemeToType (siScheme fSchemeInfo))
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
                        argInstFromFun =
                            let shouldInlineParamTy =
                                    case (sourceVarName fAnn, sourceVarName aAnn) of
                                        (Just fName, Just argName) -> fName /= argName
                                        _ -> False
                            in case (sourceVarName aAnn, f') of
                                (Just vName, ELam _ paramTy _) -> do
                                    schemeInfo <- Map.lookup vName env
                                    let paramTy' =
                                            if shouldInlineParamTy
                                                then inlineBoundVarsType presolutionView paramTy
                                                else paramTy
                                    args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                                    pure (instSeqApps args)
                                (Just vName, _) -> do
                                    schemeInfo <- Map.lookup vName env
                                    case TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference of
                                        Right (TArrow paramTy _) -> do
                                            let paramTy' =
                                                    if shouldInlineParamTy
                                                        then inlineBoundVarsType presolutionView paramTy
                                                        else paramTy
                                            args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                                            pure (instSeqApps args)
                                        _ -> Nothing
                                _ -> Nothing
                        argInst' =
                            case (sourceAnnIsPolymorphic env aAnn, argInstFromFun) of
                                (True, Just inst) -> inst
                                _ -> argInst
                        argInstFinal =
                            case argInst' of
                                InstId -> InstId
                                _ ->
                                    case TypeCheck.typeCheckWithEnv tcEnv a' of
                                        Right (TForall _ (Just _) _) -> InstElim
                                        Right TForall{} -> argInst'
                                        _ -> InstId
                        fApp = case funInstRecovered of
                            InstId -> f'
                            _ -> ETyInst f' funInstRecovered
                        aApp = case argInstFinal of
                            InstId -> a'
                            _ -> ETyInst a' argInstFinal
                    pure (EApp fApp aApp)
            in mkOut f
        ALetF v _schemeGenId schemeRootId _ _rhsScopeGen (rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
            let elaborateLet env = do
                    rhs' <- elabTerm rhsOut env
                    let debugGeneralize = traceGeneralize (algTraceConfig algebraContext)
                    _ <- pure $
                        debugGeneralize
                            ("elaborate let(" ++ v ++ "): schemeRootId=" ++ show schemeRootId
                                ++ " scopeRoot=" ++ show (scopeRootForNode scopeContext schemeRootId)
                            )
                            ()
                    (scheme0Raw, subst0Raw) <- generalizeAtNode scopeContext schemeRootId
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
                        (scheme0Norm, subst0Norm) = normalizeSchemeSubstPair (scheme0Raw, subst0Raw)
                        scheme = schemeFromType (simplifyAnnotationType (schemeToType scheme0Norm))
                        subst0 = normalizeSubstForScheme scheme (deriveLambdaBinderSubst scheme0Norm subst0Norm)
                        subst =
                            let (binds, _) = Inst.splitForalls (schemeToType scheme)
                            in if null binds then IntMap.empty else subst0
                        rhsAbs0 = closeTermWithSchemeSubstIfNeeded subst scheme rhs'
                        rhsAbs =
                            case scheme of
                                Forall binds _ | not (null binds) -> rhsAbs0
                                _ -> stripUnusedTopTyAbs rhsAbs0
                        rhsAbsTyChecked = TypeCheck.typeCheckWithEnv tcEnv rhsAbs
                    case debugGeneralize
                        ("elaborate let(" ++ v ++ "): sch0=" ++ show scheme
                            ++ " subst0=" ++ show subst
                            ++ " scheme=" ++ show scheme
                            ++ " subst=" ++ show subst
                            ++ " rhsAbsTy=" ++ show rhsAbsTyChecked
                        )
                        () of
                        () -> pure ()
                    let env' = Map.insert v SchemeInfo { siScheme = scheme, siSubst = subst } env
                        bodyElab =
                            case bodyAnn of
                                AAnn _ target _ | canonical target == canonical trivialRoot -> elabStripped bodyOut
                                _ -> elabTerm bodyOut
                    body' <- bodyElab env'
                    pure (scheme, rhsAbs, body')
                f env = do
                    (scheme, rhsAbs, body') <- elaborateLet env
                    pure (ELet v scheme rhsAbs body')
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
                    elaborateAnnotationTerm annotationContext namedSetReify env exprAnn annNodeId eid expr'
                , elabStripped = \env -> elabTerm exprOut env
                }
  where
    annotationContext = algAnnotationContext algebraContext
    scopeContext = acScopeContext annotationContext
    presolutionView = algPresolutionView algebraContext
    canonical = algCanonical algebraContext
    namedSetReify = algNamedSetReify algebraContext

    inferInstAppArgs scheme targetTy =
        let (binds, body) = Inst.splitForalls (schemeToType scheme)
        in inferInstAppArgsFromScheme binds body targetTy

    sourceVarName annExpr =
        case annExpr of
            AVar v _ -> Just v
            AAnn inner _ _ -> sourceVarName inner
            _ -> Nothing

mkOut :: (Env -> Either ElabError ElabTerm) -> ElabOut
mkOut f = ElabOut f f

resolvedLambdaParamNode :: (NodeId -> NodeId) -> (NodeId -> Maybe TyNode) -> NodeId -> Maybe NodeId
resolvedLambdaParamNode canonical chiLookupNode lamNodeId =
    let lamC = canonical lamNodeId
    in case chiLookupNode lamC of
        Just TyArrow{ tnDom = dom } -> Just dom
        Just TyVar{ tnBound = Just bnd } ->
            case chiLookupNode (canonical bnd) of
                Just TyArrow{ tnDom = dom } -> Just dom
                _ -> Nothing
        _ -> Nothing

annNode :: AnnExpr -> NodeId
annNode ann =
    case ann of
        ALit _ nid -> nid
        AVar _ nid -> nid
        ALam _ _ _ _ nid -> nid
        AApp _ _ _ _ nid -> nid
        ALet _ _ _ _ _ _ _ nid -> nid
        AAnn _ nid _ -> nid
