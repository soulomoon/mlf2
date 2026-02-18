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
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Reify.TypeOps
    ( alphaEqType
    , freeTypeVarsType
    , inlineBaseBoundsType
    , matchType
    , parseNameId
    , substTypeSimple
    )

import MLF.Frontend.Syntax (VarName)
import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Legacy (expansionToInst)
import MLF.Elab.TermClosure (closeTermWithSchemeSubstIfNeeded, substInTerm)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType, simplifyAnnotationType)
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
    , reifyBoundWithNames
    , reifyTypeWithNamedSetNoFallback
    )
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution
    ( EdgeTrace
    , CopyMapping(..)
    , etBinderArgs
    , etCopyMap
    , lookupCopy
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
    generalizeNeedsFallback :: ElabError -> Bool
    generalizeNeedsFallback err = case err of
        BindingTreeError GenSchemeFreeVars{} -> True
        SchemeFreeVars{} -> True
        _ -> False

    generalizeAtWith mbGa res scopeRoot targetNode =
        case generalizeAtWithRaw mbGa res scopeRoot targetNode of
            Right out -> Right out
            Left err | generalizeNeedsFallback err ->
                case mbGa of
                    Just _ ->
                        case generalizeAtWithRaw Nothing res scopeRoot targetNode of
                            Right out -> Right out
                            Left err2 | generalizeNeedsFallback err2 -> do
                                ty <- reifyType res targetNode
                                pure (schemeFromType ty, IntMap.empty)
                            Left err3 -> Left err3
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
            preferMoreCoherent primary fallback =
                case fallback of
                    Right alt
                        | schemeSubstCoherenceScore alt > schemeSubstCoherenceScore primary -> alt
                    _ -> primary
        in case generalizeAtWith (Just gaParents) resGen scopeRoot targetC of
            Right out ->
                let fallback = generalizeAtWith Nothing resGen scopeRoot targetC
                    out' = preferMoreCoherent out fallback
                in Right out'
            Left err | generalizeNeedsFallback err ->
                case generalizeAtWith Nothing resGen scopeRoot targetC of
                    Right out -> Right out
                    Left err2 | generalizeNeedsFallback err2 -> do
                        tyFallback <- reifyNodeTypePreferringBound targetC
                        pure (schemeFromType tyFallback, IntMap.empty)
                    Left err3 -> Left err3
            Left err -> Left err

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

    schemeFromForallSpine :: ElabType -> ElabScheme
    schemeFromForallSpine ty =
        let (binds, body) = Inst.splitForalls ty
        in Forall binds body

    schemeFromRhsBodyCoherentWithSubst :: ElabType -> (ElabScheme, IntMap.IntMap String)
    schemeFromRhsBodyCoherentWithSubst ty =
        let (binds, body0) = Inst.splitForalls ty
            binderNames = map fst binds
            bodyFvs = Set.toList (freeTypeVarsType body0)
            bodyFvSet = Set.fromList bodyFvs
            externalFvs =
                [ v
                | v <- bodyFvs
                , v `notElem` binderNames
                ]
            unusedBinders =
                [ v
                | v <- binderNames
                , not (Set.member v bodyFvSet)
                ]
            replacements =
                if length externalFvs <= length unusedBinders && not (null externalFvs)
                    then zip externalFvs unusedBinders
                    else []
            body =
                if null replacements
                    then body0
                    else
                        foldl'
                            (\acc (fromV, toV) -> substTypeSimple fromV (TVar toV) acc)
                            body0
                            replacements
            substExtra =
                IntMap.fromList
                    [ (key, toV)
                    | (fromV, toV) <- replacements
                    , Just key <- [parseNameId fromV]
                    ]
        in (Forall binds body, substExtra)

    schemeFromRhsBodyCoherent :: ElabType -> ElabScheme
    schemeFromRhsBodyCoherent ty =
        fst (schemeFromRhsBodyCoherentWithSubst ty)

    schemeFromRhsBodyCoherentSubst :: ElabType -> IntMap.IntMap String
    schemeFromRhsBodyCoherentSubst ty =
        snd (schemeFromRhsBodyCoherentWithSubst ty)

    schemeSubstCoherenceScore :: (ElabScheme, IntMap.IntMap String) -> (Int, Int, Int, Int)
    schemeSubstCoherenceScore pairRaw =
        let (sch, subst) = normalizeSchemeSubstPair pairRaw
            (binds, _) = Inst.splitForalls (schemeToType sch)
            binderNames = Set.fromList (map fst binds)
            binderCount = Set.size binderNames
            substNames = Set.fromList (IntMap.elems subst)
            covered = Set.size (Set.intersection binderNames substNames)
            missing = Set.size (Set.difference binderNames substNames)
        in (covered, binderCount, negate missing, IntMap.size subst)

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

    closeTermWithSchemeSubstNoFreshen :: IntMap.IntMap String -> ElabScheme -> ElabTerm -> ElabTerm
    closeTermWithSchemeSubstNoFreshen subst sch term =
        let termSubst = substInTerm subst term
        in case sch of
            Forall binds _ ->
                foldr
                    (\(name, bound) acc -> ETyAbs name bound acc)
                    termSubst
                    binds

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

    termStartsWithTypeAbstraction :: ElabTerm -> Bool
    termStartsWithTypeAbstraction term = case term of
        ETyAbs{} -> True
        _ -> False

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
                                Left (BindingTreeError GenSchemeFreeVars{}) -> do
                                    -- Some coercion codomain roots are outside the scheme
                                    -- ownership checks; recover the term-level type directly.
                                    paramTy0 <- reifyNodeTypePreferringBound annNodeId
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
                                                then inlineBoundVarsType resGen paramTy
                                                else paramTy
                                    args <- inferInstAppArgs (siScheme si) paramTy'
                                    pure (instSeqApps args)
                                (Just v, _) -> do
                                    si <- Map.lookup v env
                                    case TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference of
                                        Right (TArrow paramTy _) -> do
                                            let paramTy' =
                                                    if shouldInlineParamTy
                                                        then inlineBoundVarsType resGen paramTy
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
                        annIsApp annExpr =
                            case annExpr of
                                AApp{} -> True
                                AAnn inner _ _ -> annIsApp inner
                                _ -> False
                        rhsIsApp = annIsApp rhsAnn
                        annIsLam annExpr =
                            case annExpr of
                                ALam{} -> True
                                AAnn inner _ _ -> annIsLam inner
                                _ -> False
                        rhsIsLam = annIsLam rhsAnn
                        binderCount scheme0 =
                            let (binds, _) = Inst.splitForalls (schemeToType scheme0)
                            in length binds
                        lambdaParamNodes annExpr =
                            case annExpr of
                                ALam _ paramNode _ body _ -> paramNode : lambdaParamNodes body
                                AAnn inner _ _ -> lambdaParamNodes inner
                                _ -> []
                        deriveLambdaBinderSubst scheme0 subst0' =
                            let (binds, _) = Inst.splitForalls (schemeToType scheme0)
                                binderNames = map fst binds
                                paramNodes = lambdaParamNodes rhsAnn
                                binderPairs = zip binderNames paramNodes
                            in foldl'
                                (\acc (name, paramNode) ->
                                    if name `elem` IntMap.elems acc
                                        then acc
                                        else
                                            let key = getNodeId (canonical paramNode)
                                            in IntMap.insertWith (\_ old -> old) key name acc
                                )
                                subst0'
                                binderPairs
                        (sch0Norm, subst0Norm) = normalizeSchemeSubstPair (sch0Raw, subst0Raw)
                        sch0 = sch0Norm
                        subst0 = deriveLambdaBinderSubst sch0Norm subst0Norm
                        schemeUnbounded =
                            case sch0 of
                                Forall binds _ -> all ((== Nothing) . snd) binds
                        fallbackEligible = rhsIsApp && schemeUnbounded
                        containsBottomTy ty =
                            case ty of
                                TVar _ -> False
                                TArrow d c -> containsBottomTy d || containsBottomTy c
                                TCon _ args -> any containsBottomTy args
                                TBase _ -> False
                                TForall _ mb body ->
                                    maybe False (containsBottomTy . tyToElab) mb
                                        || containsBottomTy body
                                TBottom -> True
                        fallbackFromRhsTy rhsTy =
                            let freeVars = Set.toList (freeTypeVarsType rhsTy)
                                rhsSch = Forall [(name, Nothing) | name <- freeVars] rhsTy
                            in if alphaEqType (schemeToType rhsSch) (schemeToType sch0)
                                then Nothing
                                else Just rhsSch
                        fallbackCoherent schCandidate substCandidate =
                            let (binds, _) = Inst.splitForalls (schemeToType schCandidate)
                                binderNames = Set.fromList (map fst binds)
                                substNames = Set.fromList (IntMap.elems substCandidate)
                            in Set.isSubsetOf binderNames substNames
                        rhsSourceVar annExpr =
                            case annExpr of
                                AVar name _ -> Just name
                                AAnn inner _ _ -> rhsSourceVar inner
                                _ -> Nothing
                        rhsAppArgIsVar annExpr =
                            case annExpr of
                                AApp _ argAnn _ _ _ ->
                                    case argAnn of
                                        AVar _ _ -> True
                                        AAnn inner _ _ -> rhsAppArgIsVar inner
                                        _ -> False
                                AAnn inner _ _ -> rhsAppArgIsVar inner
                                _ -> False
                        rhsTypeChecked = TypeCheck.typeCheckWithEnv tcEnv rhs'
                        fallbackChoiceFromVar =
                            case rhsSourceVar rhsAnn of
                                Just sourceName -> do
                                    si <- Map.lookup sourceName env
                                    pure (siScheme si, siSubst si)
                                Nothing -> Nothing
                        fallbackChoiceFromApp =
                            if fallbackEligible
                                then
                                    case rhsTypeChecked of
                                        Right rhsTy ->
                                            case fallbackFromRhsTy rhsTy of
                                                Just schApp -> Just (schApp, subst0)
                                                Nothing -> Nothing
                                        Left _ -> Nothing
                                else Nothing
                        fallbackChoiceFromLam =
                            if rhsIsLam
                                then
                                    case rhsTypeChecked of
                                        Right rhsTy ->
                                            case fallbackFromRhsTy rhsTy of
                                                Just schLam
                                                    | containsBottomTy (schemeToType schLam) -> Nothing
                                                    | binderCount schLam < binderCount sch0 -> Nothing
                                                    | otherwise -> Just (schLam, IntMap.empty)
                                                Nothing -> Nothing
                                        Left _ -> Nothing
                                else Nothing
                        fallbackChoiceRaw =
                            case fallbackChoiceFromApp of
                                Just appChoice -> Just appChoice
                                Nothing ->
                                    case fallbackChoiceFromLam of
                                        Just lamChoice -> Just lamChoice
                                        Nothing -> fallbackChoiceFromVar
                        fallbackChoice =
                            case fallbackChoiceRaw of
                                Just candidateRaw ->
                                    let candidate@(schCandidate, substCandidate) =
                                            normalizeSchemeSubstPair candidateRaw
                                    in if fallbackCoherent schCandidate substCandidate
                                        then Just candidate
                                        else Nothing
                                Nothing -> Nothing
                        fallbackFromVarSelected =
                            case (fallbackChoiceFromApp, fallbackChoiceFromLam, fallbackChoiceFromVar, fallbackChoiceRaw) of
                                (Nothing, Nothing, Just _, Just _) -> True
                                _ -> False
                        fallbackScheme = fmap fst fallbackChoice
                        schChosen = maybe sch0 fst fallbackChoice
                        substChosen = maybe subst0 snd fallbackChoice
                        schSimplified =
                            if fallbackFromVarSelected
                                then schChosen
                                else schemeFromType (simplifyAnnotationType (schemeToType schChosen))
                        (schBase, substBase) = normalizeSchemeSubstPair (schSimplified, substChosen)
                        rhsAbsBase =
                            if
                                fallbackFromVarSelected
                                    || (rhsIsApp && rhsAppArgIsVar rhsAnn)
                                    || (rhsIsLam && isJust fallbackChoice)
                                then rhs'
                                else closeTermWithSchemeSubstIfNeeded substBase schBase rhs'
                        lamAligned =
                            case (rhsIsLam, TypeCheck.typeCheckWithEnv tcEnv rhsAbsBase) of
                                (_isLam, Right rhsTy)
                                    | not (alphaEqType rhsTy (schemeToType schBase)) ->
                                        let shouldAlign = rhsIsLam
                                        in if shouldAlign
                                            then
                                                let (schLam, substLam) =
                                                        normalizeSchemeSubstPair (schemeFromType rhsTy, substBase)
                                                in if fallbackCoherent schLam substLam
                                                    then
                                                        let rhsAbsLam = closeTermWithSchemeSubstIfNeeded substLam schLam rhs'
                                                        in (schLam, substLam, rhsAbsLam, Right rhsTy)
                                                    else (schBase, substBase, rhsAbsBase, Right rhsTy)
                                            else (schBase, substBase, rhsAbsBase, Right rhsTy)
                                (_, Right rhsTy) -> (schBase, substBase, rhsAbsBase, Right rhsTy)
                                (_, Left rhsTyErr) -> (schBase, substBase, rhsAbsBase, Left rhsTyErr)
                        (sch0Final, subst0Final, rhsAbs0Final, rhsAbsTyChecked0) = lamAligned
                        appAligned =
                            case (rhsIsApp, rhsAbsTyChecked0) of
                                (True, Right rhsTy)
                                    | not (alphaEqType rhsTy (schemeToType sch0Final)) ->
                                        let candidateNorm@(schAppNorm, substAppNorm) =
                                                normalizeSchemeSubstPair (schemeFromType rhsTy, subst0Final)
                                            schAppBody = schemeFromRhsBodyCoherent rhsTy
                                            substAppBodyExtra = schemeFromRhsBodyCoherentSubst rhsTy
                                            substAppBody =
                                                normalizeSubstForScheme schAppBody (IntMap.union subst0Final substAppBodyExtra)
                                            candidateBody = (schAppBody, substAppBody)
                                            schAppPreserve = schemeFromForallSpine rhsTy
                                            substAppPreserve = normalizeSubstForScheme schAppPreserve subst0Final
                                            candidatePreserve = (schAppPreserve, substAppPreserve)
                                            bodyCoherent = fallbackCoherent schAppBody substAppBody
                                            normCoherent = fallbackCoherent schAppNorm substAppNorm
                                            preserveCoherent = fallbackCoherent schAppPreserve substAppPreserve
                                            selected =
                                                if bodyCoherent
                                                    then Just candidateBody
                                                    else
                                                        if normCoherent
                                                            then Just candidateNorm
                                                            else
                                                                if preserveCoherent
                                                            then Just candidatePreserve
                                                            else Nothing
                                        in case selected of
                                            Just (schApp, substApp) ->
                                                let (bindsApp, _) = Inst.splitForalls (schemeToType schApp)
                                                    substApp' =
                                                        if null bindsApp
                                                            then IntMap.empty
                                                            else substApp
                                                    rhsAbsApp = closeTermWithSchemeSubstNoFreshen substApp' schApp rhs'
                                                in (schApp, substApp', rhsAbsApp, Right rhsTy)
                                            Nothing -> (sch0Final, subst0Final, rhsAbs0Final, rhsAbsTyChecked0)
                                _ -> (sch0Final, subst0Final, rhsAbs0Final, rhsAbsTyChecked0)
                        (schRawFinal, substRawFinal, rhsAbs, rhsAbsTyChecked) = appAligned
                        substFinal =
                            let (bindsFinal, _) = Inst.splitForalls (schemeToType schRawFinal)
                            in if null bindsFinal then IntMap.empty else substRawFinal
                        sch = schRawFinal
                        subst = substFinal
                    case debugGeneralize
                        ("elaborate let(" ++ v ++ "): sch0=" ++ show sch0
                            ++ " subst0=" ++ show subst0
                            ++ " scheme=" ++ show sch
                            ++ " subst=" ++ show subst
                            ++ " rhsIsApp=" ++ show rhsIsApp
                            ++ " rhsIsLam=" ++ show rhsIsLam
                            ++ " fallback=" ++ show (fmap schemeToType fallbackScheme)
                            ++ " fallback-raw=" ++ show (fmap (schemeToType . fst) fallbackChoiceRaw)
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
                    inst <- reifyInst namedSetReify env exprAnn eid
                    mExpectedBound <- case generalizeAtNode annNodeId of
                        Right (Forall ((_, Just bnd) : _) _, _) -> pure (Just (tyToElab bnd))
                        Left _ -> pure Nothing
                        _ -> pure Nothing
                    let instAdjusted0 = adjustAnnotationInst inst
                        sourceAnnIsVar annExpr =
                            case annExpr of
                                AVar _ _ -> True
                                AAnn inner _ _ -> sourceAnnIsVar inner
                                _ -> False
                        instAdjusted =
                            case (mExpectedBound, instAdjusted0) of
                                (Just expectedBound, InstInside (InstBot _)) ->
                                    InstInside (InstBot expectedBound)
                                (Just expectedBound, InstId)
                                    | not (sourceAnnIsVar exprAnn) ->
                                    InstInside (InstBot expectedBound)
                                _ -> instAdjusted0
                    exprClosed <-
                        if instAdjusted == InstId
                            then pure expr'
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
                let mSchemeInfo = case funAnn of
                        AVar v _ -> Map.lookup v env
                        _ -> Nothing
                    mSchemeInfo' = mSchemeInfo
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
                let allowFallbackFromTrace =
                        case mSchemeInfo of
                            Just si ->
                                case siScheme si of
                                    Forall binds _ -> not (null binds)
                            Nothing -> False
                instFromTrace <- case (allowFallbackFromTrace, phi, mExpansion, mSchemeInfo) of
                    (True, InstId, Just (ExpInstantiate args), mSi) -> do
                        let traceArgNodes =
                                case mTrace of
                                    Just tr | not (null (etBinderArgs tr)) ->
                                        Just (map snd (etBinderArgs tr))
                                    _ -> Nothing
                            argNodesPrimary = fromMaybe args traceArgNodes
                            resolvedPrimary = resolveFallbackArgNodes mTrace argNodesPrimary
                            (argNodes, resolvedArgNodes) =
                                case (traceArgNodes, resolvedPrimary) of
                                    (Just _, Nothing) ->
                                        (args, resolveFallbackArgNodes mTrace args)
                                    _ -> (argNodesPrimary, resolvedPrimary)
                            fallbackOk =
                                fallbackProvenanceArityOk mTrace mSi argNodes
                                    && isJust resolvedArgNodes
                            tracePairCount =
                                case mTrace of
                                    Just tr -> length (etBinderArgs tr)
                                    Nothing -> 0
                            traceBinderArity =
                                case mTrace of
                                    Just tr ->
                                        IntSet.size $
                                            IntSet.fromList
                                                [ getNodeId binder
                                                | (binder, _arg) <- etBinderArgs tr
                                                ]
                                    Nothing -> 0
                            schemeArity =
                                case mSi of
                                    Just si ->
                                        case siScheme si of
                                            Forall binds _ -> length binds
                                    Nothing -> 0
                        let targetArgs =
                                case mSi of
                                    Just si ->
                                        case reifyTargetType namedSetReify ew si of
                                            Right targetTy ->
                                                case inferInstAppArgs (siScheme si) targetTy of
                                                    Just inferred
                                                        | length inferred == length argNodes ->
                                                            Just inferred
                                                    _ -> Nothing
                                            Left _ -> Nothing
                                    Nothing -> Nothing
                        case debugGeneralize
                            ("reifyInst fallback edge=" ++ show eid
                                ++ " argNodes=" ++ show argNodes
                                ++ " fallbackOk=" ++ show fallbackOk
                                ++ " tracePairCount=" ++ show tracePairCount
                                ++ " traceBinderArity=" ++ show traceBinderArity
                                ++ " schemeArity=" ++ show schemeArity
                                ++ " resolvedArgNodes=" ++ show resolvedArgNodes
                                ++ " targetArgs=" ++ show targetArgs
                            )
                            () of
                            () -> pure ()
                        if not fallbackOk
                            then pure Nothing
                            else do
                                let argNodes' = fromMaybe argNodes resolvedArgNodes
                                let substForArgs =
                                        case mSi of
                                            Just si -> siSubst si
                                            Nothing -> IntMap.empty
                                    constraint = srConstraint resReify
                                    reifyArg arg =
                                        let argC = canonical arg
                                        in case VarStore.lookupVarBound constraint argC of
                                            Just bnd -> reifyBoundWithNames resReify substForArgs bnd
                                            Nothing -> reifyTypeWithNamedSetNoFallback resReify substForArgs namedSetReify argC
                                argTys <- case targetArgs of
                                    Just inferred -> pure inferred
                                    Nothing -> mapM reifyArg argNodes'
                                let argTys' = map (inlineBoundVarsType resReify) argTys
                                case debugGeneralize
                                    ("reifyInst fallback edge=" ++ show eid
                                        ++ " argTys=" ++ show argTys
                                        ++ " argTys'=" ++ show argTys'
                                    )
                                    () of
                                    () -> pure ()
                                pure (Just (instSeqApps argTys'))
                    _ -> pure Nothing
                case instFromTrace of
                    Just inst -> Right inst
                    Nothing -> Right phi

    reifyTargetType :: IntSet.IntSet -> EdgeWitness -> SchemeInfo -> Either ElabError ElabType
    reifyTargetType namedSetReify ew si =
        let subst = siSubst si
        in case VarStore.lookupVarBound (srConstraint resReify) (ewRight ew) of
            Just bnd -> reifyTypeWithNamedSetNoFallback resReify subst namedSetReify bnd
            Nothing -> reifyTypeWithNamedSetNoFallback resReify subst namedSetReify (ewRight ew)

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

    fallbackProvenanceArityOk :: Maybe EdgeTrace -> Maybe SchemeInfo -> [NodeId] -> Bool
    fallbackProvenanceArityOk mTrace mSi argNodes =
        case mTrace of
            Just tr
                | not (null (etBinderArgs tr)) ->
                    let pairCount = length (etBinderArgs tr)
                        binderArity =
                            IntSet.size $
                                IntSet.fromList
                                    [ getNodeId binder
                                    | (binder, _arg) <- etBinderArgs tr
                                    ]
                        schemeArityOk =
                            case mSi of
                                Nothing -> True
                                Just si ->
                                    case siScheme si of
                                        Forall binds _ -> length binds == binderArity
                    in pairCount == binderArity
                        && length argNodes == binderArity
                        && schemeArityOk
            _ ->
                case mSi of
                    Nothing -> True
                    Just si ->
                        case siScheme si of
                            Forall binds _ -> length argNodes == length binds

    resolveFallbackArgNodes :: Maybe EdgeTrace -> [NodeId] -> Maybe [NodeId]
    resolveFallbackArgNodes mTrace = mapM (resolveFallbackArgNode mTrace)

    resolveFallbackArgNode :: Maybe EdgeTrace -> NodeId -> Maybe NodeId
    resolveFallbackArgNode mTrace arg =
        listToMaybe
            [ canonical cand
            | cand <- fallbackArgCandidates mTrace arg
            , nodeExists cand
            ]
      where
        nodeExists nid =
            isJust (NodeAccess.lookupNode (srConstraint resReify) (canonical nid))

    fallbackArgCandidates :: Maybe EdgeTrace -> NodeId -> [NodeId]
    fallbackArgCandidates mTrace arg =
        reverse candidatesRev
      where
        key = getNodeId arg
        keyC = getNodeId (canonical arg)
        traceForward nid =
            case mTrace of
                Nothing -> []
                Just tr -> maybe [] pure (lookupCopy nid (etCopyMap tr))
        traceReverseByKey k =
            case mTrace of
                Nothing -> []
                Just tr ->
                    [ NodeId src
                    | (src, dst) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                    , getNodeId dst == k
                    ]
        rawCandidates =
            [arg, canonical arg]
                ++ traceForward arg
                ++ traceForward (canonical arg)
                ++ traceReverseByKey key
                ++ traceReverseByKey keyC
        (candidatesRev, _) =
            foldl'
                (\(acc, seen) nid ->
                    let k = getNodeId nid
                    in if IntSet.member k seen
                        then (acc, seen)
                        else (nid : acc, IntSet.insert k seen)
                )
                ([], IntSet.empty)
                rawCandidates

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
