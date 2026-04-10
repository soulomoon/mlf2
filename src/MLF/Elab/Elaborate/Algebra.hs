{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Elaborate.Algebra
  ( Env,
    ElabOut (..),
    AlgebraContext (..),
    elabAlg,
    resolvedLambdaParamNode,
  )
where

import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import MLF.Constraint.Presolution (PresolutionView)
import MLF.Constraint.Types
  ( NodeId,
    TyNode (..),
    getNodeId,
  )
import MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    instSeqApps,
    reifyInst,
    sourceAnnIsPolymorphic,
    stripUnusedTopTyAbs,
  )
import MLF.Elab.Elaborate.Scope
  ( generalizeAtNode,
    normalizeSchemeSubstPair,
    normalizeSubstForScheme,
    reifyNodeTypePreferringBound,
    scopeRootForNode,
  )
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import qualified MLF.Elab.Inst as Inst
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType, simplifyAnnotationType)
import MLF.Elab.TermClosure (closeTermWithSchemeSubstIfNeeded)
import qualified MLF.Elab.TypeCheck as TypeCheck (Env (..), typeCheckWithEnv)
import MLF.Elab.Types
  ( ElabError (..),
    ElabTerm (..),
    ElabType,
    Instantiation (..),
    SchemeInfo (..),
    Ty (..),
    schemeFromType,
    tyToElab,
    pattern Forall,
  )
import MLF.Frontend.ConstraintGen.Types (AnnExpr (..), AnnExprF (..))
import MLF.Frontend.Syntax (VarName)
import MLF.Reify.TypeOps (alphaEqType, firstNonContractiveRecursiveType, freeTypeVarsType, parseNameId, substTypeCapture)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

type Env = Map.Map VarName SchemeInfo

data ElabOut = ElabOut
  { elabTerm :: Env -> Either ElabError ElabTerm,
    elabStripped :: Env -> Either ElabError ElabTerm
  }

data AlgebraContext = AlgebraContext
  { algPresolutionView :: PresolutionView,
    algTraceConfig :: TraceConfig,
    algCanonical :: NodeId -> NodeId,
    algResolvedLambdaParamNode :: NodeId -> Maybe NodeId,
    algAnnotationContext :: AnnotationContext,
    algNamedSetReify :: IntSet.IntSet
  }

containsMuType :: ElabType -> Bool
containsMuType ty =
  case ty of
    TMu {} -> True
    TArrow dom cod -> containsMuType dom || containsMuType cod
    TCon _ args -> any containsMuType args
    TForall _ mb body -> maybe False containsMuBound mb || containsMuType body
    _ -> False
  where
    containsMuBound bound = case bound of
      TArrow dom cod -> containsMuType dom || containsMuType cod
      TCon _ args -> any containsMuType args
      TForall _ mb body -> maybe False containsMuBound mb || containsMuType body
      TMu {} -> True
      _ -> False

hasContractiveRecursiveWitness :: ElabType -> Bool
hasContractiveRecursiveWitness ty =
  containsMuType ty && isNothing (firstNonContractiveRecursiveType ty)

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
                recursiveParamTyFromEnv annExpr =
                  let isIdentityLikeSchemeType ty =
                        case ty of
                          TForall name Nothing body -> case body of
                            TArrow (TVar dom) (TVar cod) -> dom == name && cod == name
                            _ -> False
                          _ -> False
                      mediatedVarUse expr =
                        case expr of
                          AVar name _ -> name == v
                          AAnn inner _ _ -> mediatedVarUse inner
                          AApp fun arg _ _ _ ->
                            case (fun, arg) of
                              (AVar funName _, innerArg)
                                | Just schemeInfo <- Map.lookup funName env
                                , isIdentityLikeSchemeType (schemeToType (siScheme schemeInfo)) ->
                                    mediatedVarUse innerArg
                              _ -> False
                          _ -> False
                      firstRecursiveDomain expr =
                        case expr of
                          AApp (AVar recurName _) arg _ _ _
                            | mediatedVarUse arg
                            , Just schemeInfo <- Map.lookup recurName env ->
                                case schemeToType (siScheme schemeInfo) of
                                  muTy@(TMu muName muBody)
                                    | hasContractiveRecursiveWitness muTy ->
                                        case substTypeCapture muName muTy muBody of
                                          TArrow dom _ -> Just dom
                                          _ -> Nothing
                                  _ -> Nothing
                          ALam boundName _ _ inner _
                            | boundName == v -> Nothing
                            | otherwise -> firstRecursiveDomain inner
                          AApp fun arg _ _ _ ->
                            case firstRecursiveDomain fun of
                              Just dom -> Just dom
                              Nothing -> firstRecursiveDomain arg
                          ALet boundName _ _ _ _ rhs body _
                            | boundName == v ->
                                firstRecursiveDomain rhs
                            | otherwise ->
                                case firstRecursiveDomain rhs of
                                  Just dom -> Just dom
                                  Nothing -> firstRecursiveDomain body
                          AAnn inner _ _ -> firstRecursiveDomain inner
                          _ -> Nothing
                   in firstRecursiveDomain annExpr
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
            paramTySurface0 <- reifyNodeTypePreferringBound scopeContext paramSource
            let paramTySurface =
                  case paramTySurface0 of
                    TVar name
                      | isJust (parseNameId name) ->
                          fromMaybe paramTySurface0 (recursiveParamTyFromEnv bodyAnn)
                    _ -> paramTySurface0
            (paramTy, paramSchemeInfo) <-
              case mAnnLambda of
                Just (annNodeId, _) ->
                  case generalizeAtNode scopeContext annNodeId of
                    Right (paramScheme, _subst) ->
                      let paramTy0 = case paramScheme of
                            Forall [(name, Just bnd)] bodyTy
                              | bodyTy == TVar name -> tyToElab bnd
                            _ -> schemeToType paramScheme
                          -- If generalizeAtNode returned a bare TVar (over-generalized),
                          -- fall back to reifyNodeTypePreferringBound which resolves
                          -- through the constraint graph's bound/canonical chain.
                          paramTyResolved = case paramTy0 of
                            TVar {} ->
                              case reifyNodeTypePreferringBound scopeContext annNodeId of
                                Right ty@TMu {} -> ty
                                _ -> paramTy0
                            _ -> paramTy0
                       in pure
                            ( paramTyResolved,
                              SchemeInfo
                                { siScheme = schemeFromType paramTyResolved,
                                  siSubst = IntMap.empty
                                }
                            )
                    Left err -> Left err
                Nothing ->
                  pure
                    ( paramTySurface,
                      SchemeInfo
                        { siScheme = schemeFromType paramTySurface,
                          siSubst = IntMap.empty
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
            let tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env) Map.empty
                annHasMuScheme ann =
                  case sourceVarName ann >>= (`Map.lookup` env) of
                    Just schemeInfo ->
                      case schemeToType (siScheme schemeInfo) of
                        TMu {} -> True
                        _ -> False
                    Nothing -> False
                reifyInstWithRecovery ann eid _term =
                  case reifyInst annotationContext namedSetReify env ann eid of
                    Right inst -> Right inst
                    Left err@(PhiTranslatabilityError _)
                      | annHasMuScheme ann -> Right InstId
                      | otherwise -> Left err
                    Left err -> Left err
                reifyInstIfPolymorphic ann eid term
                  | sourceAnnIsPolymorphic env ann =
                      reifyInstWithRecovery ann eid term
                  | otherwise = Right InstId
            funInst <- reifyInstIfPolymorphic fAnn funEid f'
            argInst <- reifyInstIfPolymorphic aAnn argEid a'
            let funInstByFunType =
                  case funInst of
                    inst0@(InstApp _) ->
                      case TypeCheck.typeCheckWithEnv tcEnv f' of
                        Right TForall {} -> inst0
                        Right _ -> InstId
                        Left _ -> inst0
                    inst0@(InstInside (InstBot _)) ->
                      case TypeCheck.typeCheckWithEnv tcEnv f' of
                        Right TForall {} -> inst0
                        Right _ -> InstId
                        Left _ -> inst0
                    inst0@(InstInside (InstApp _)) ->
                      case TypeCheck.typeCheckWithEnv tcEnv f' of
                        Right TForall {} -> inst0
                        Right _ -> InstId
                        Left _ -> inst0
                    inst0@(InstSeq (InstInside (InstBot _)) InstElim) ->
                      case TypeCheck.typeCheckWithEnv tcEnv f' of
                        Right TForall {} -> inst0
                        Right _ -> InstId
                        Left _ -> inst0
                    inst0@(InstSeq (InstInside (InstApp _)) InstElim) ->
                      case TypeCheck.typeCheckWithEnv tcEnv f' of
                        Right TForall {} -> inst0
                        Right _ -> InstId
                        Left _ -> inst0
                    _ -> funInst
                funInst' =
                  case
                      either
                        ( const
                            ( either
                                (const Nothing)
                                Just
                                (reifyNodeTypePreferringBound scopeContext (annNode aAnn))
                            )
                        )
                        Just
                        (TypeCheck.typeCheckWithEnv tcEnv a')
                    of
                    recoveredArg ->
                      case funInstByFunType of
                        inst0@(InstApp ty0) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstInside (InstBot ty0)) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstInside (InstApp ty0)) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstSeq (InstInside (InstBot ty0)) InstElim) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        inst0@(InstSeq (InstInside (InstApp ty0)) InstElim) ->
                          case ty0 of
                            TVar {} -> maybe inst0 InstApp recoveredArg
                            TForall {} -> maybe inst0 InstApp recoveredArg
                            _ -> inst0
                        _ -> funInstByFunType
                normalizeFunInst inst0 =
                  case TypeCheck.typeCheckWithEnv tcEnv f' of
                    Right fTy -> go 0 inst0
                      where
                        go n instN
                          | n >= (8 :: Int) = instN
                          | otherwise =
                              case applyInstantiation fTy instN of
                                Right (TForall _ (Just _) _) -> go (n + 1) (InstSeq instN InstElim)
                                Right TForall {} -> instN
                                Right _ -> instN
                                Left _ -> instN
                    Left _ -> inst0
                funInstNorm = normalizeFunInst funInst'
                funInstRecovered =
                  let fApp0 = case funInstNorm of
                        InstId -> f'
                        _ -> ETyInst f' funInstNorm
                   in case ( TypeCheck.typeCheckWithEnv tcEnv (EApp fApp0 a'),
                             sourceVarName fAnn,
                             sourceVarName aAnn,
                             TypeCheck.typeCheckWithEnv tcEnv a'
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
                        (Left _, _, _, _) ->
                          fromMaybe funInstNorm $
                            case (sourceVarName fAnn, TypeCheck.typeCheckWithEnv tcEnv a') of
                              (Just fName, Right argTy)
                                | let isIdentityLikeSchemeType ty =
                                        case ty of
                                          TForall name Nothing body -> isIdentityLikeBody name body
                                          _ -> False
                                        where
                                          isIdentityLikeBody name body = case body of
                                            TArrow (TVar dom) (TVar cod) -> dom == name && cod == name
                                            _ -> False
                                , hasContractiveRecursiveWitness argTy ->
                                    case Map.lookup fName env of
                                      Just fSchemeInfo
                                        | isIdentityLikeSchemeType (schemeToType (siScheme fSchemeInfo)) ->
                                            let candidate = normalizeFunInst (InstApp argTy)
                                                fAppCandidate = case candidate of
                                                  InstId -> f'
                                                  _ -> ETyInst f' candidate
                                             in case TypeCheck.typeCheckWithEnv tcEnv (EApp fAppCandidate a') of
                                                  Right _ -> Just candidate
                                                  Left _ -> Nothing
                                      _ -> Nothing
                              _ -> Nothing
                        _ -> funInstNorm
                fAppForArgInference = case funInstRecovered of
                  InstId -> f'
                  _ -> ETyInst f' funInstRecovered
                argInstFromFun =
                  let shouldInlineParamTy =
                        case (sourceVarName fAnn, sourceVarName aAnn) of
                          (Just fName, Just argName) -> fName /= argName
                          _ -> False
                      shouldInferArgInst =
                        case (sourceVarName fAnn, sourceVarName aAnn) of
                          (Just fName, Just argName) -> fName /= argName
                          _ -> True
                   in if not shouldInferArgInst
                        then Nothing
                        else case (sourceVarName aAnn, f') of
                          (Just vName, ELam _ paramTy _) -> do
                            schemeInfo <- Map.lookup vName env
                            let paramTy' =
                                  if shouldInlineParamTy
                                    then inlineBoundVarsType presolutionView paramTy
                                    else paramTy
                            do
                              args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                              pure (instSeqApps (map (inlineBoundVarsType presolutionView) args))
                          (Just vName, _) -> do
                            schemeInfo <- Map.lookup vName env
                            case TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference of
                              Right (TArrow paramTy _) -> do
                                let paramTy' =
                                      if shouldInlineParamTy
                                        then inlineBoundVarsType presolutionView paramTy
                                        else paramTy
                                do
                                  args <- inferInstAppArgs (siScheme schemeInfo) paramTy'
                                  pure (instSeqApps (map (inlineBoundVarsType presolutionView) args))
                              _ -> Nothing
                          _ -> Nothing
                argInst' =
                  case (sourceVarName fAnn, sourceVarName aAnn, TypeCheck.typeCheckWithEnv tcEnv fAppForArgInference, argInst) of
                    (Just fName, Just argName, Right (TArrow paramTy _), InstApp argTy)
                      | fName == argName
                      , Just schemeInfo <- Map.lookup fName env
                      , case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False
                      , let instCandidate = InstApp argTy
                      , Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate)
                      , alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstInside (InstBot argTy))
                      | fName == argName
                      , Just schemeInfo <- Map.lookup fName env
                      , case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False
                      , let instCandidate = InstApp argTy
                      , Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate)
                      , alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstInside (InstApp argTy))
                      | fName == argName
                      , Just schemeInfo <- Map.lookup fName env
                      , case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False
                      , let instCandidate = InstApp argTy
                      , Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate)
                      , alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstSeq (InstInside (InstBot argTy)) InstElim)
                      | fName == argName
                      , Just schemeInfo <- Map.lookup fName env
                      , case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False
                      , let instCandidate = InstApp argTy
                      , Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate)
                      , alphaEqType argTy' paramTy ->
                          instCandidate
                    (Just fName, Just argName, Right (TArrow paramTy _), InstSeq (InstInside (InstApp argTy)) InstElim)
                      | fName == argName
                      , Just schemeInfo <- Map.lookup fName env
                      , case siScheme schemeInfo of
                          Forall [(_, Nothing)] _ -> True
                          _ -> False
                      , let instCandidate = InstApp argTy
                      , Right argTy' <- TypeCheck.typeCheckWithEnv tcEnv (ETyInst a' instCandidate)
                      , alphaEqType argTy' paramTy ->
                          instCandidate
                    _ ->
                      case (sourceAnnIsPolymorphic env aAnn, argInstFromFun) of
                        (True, Just inst) -> inst
                        _ -> argInst
                argInstFinal =
                  case argInst' of
                    InstId -> InstId
                    _ ->
                      case TypeCheck.typeCheckWithEnv tcEnv a' of
                        Right (TForall _ (Just _) _) -> InstElim
                        Right TForall {} -> argInst'
                        _ -> InstId
                aApp = case argInstFinal of
                  InstId -> a'
                  _ -> ETyInst a' argInstFinal
                fApp =
                  let fApp0 = case funInstRecovered of
                        InstId -> f'
                        _ -> ETyInst f' funInstRecovered
                      isInternalTyVar ty =
                        case ty of
                          TVar name -> isJust (parseNameId name)
                          _ -> False
                      isIdentityLambdaBody paramName body =
                        case body of
                          EVar bodyName -> bodyName == paramName
                          _ -> False
                   in case (TypeCheck.typeCheckWithEnv tcEnv (EApp fApp0 aApp), fApp0, sourceVarName aAnn, TypeCheck.typeCheckWithEnv tcEnv aApp) of
                        (Left _, ELam paramName paramTy body, Just argName, Right argTy)
                          | isInternalTyVar paramTy
                              && isIdentityLambdaBody paramName body
                              && hasContractiveRecursiveWitness argTy
                              && maybe False (hasContractiveRecursiveWitness . schemeToType . siScheme) (Map.lookup argName env) ->
                              ELam paramName argTy body
                        _ -> fApp0
            app <- insertMuUseSiteCoercions tcEnv fApp aApp
            case
                ( (\go ->
                      sourceAnnIsPolymorphic env aAnn
                        && ( case funInst of
                               InstApp ty -> go ty
                               InstInside (InstBot ty) -> go ty
                               InstInside (InstApp ty) -> go ty
                               InstSeq (InstInside (InstBot ty)) InstElim -> go ty
                               InstSeq (InstInside (InstApp ty)) InstElim -> go ty
                               _ -> False
                            || case argInst of
                                 InstApp ty -> go ty
                                 InstInside (InstBot ty) -> go ty
                                 InstInside (InstApp ty) -> go ty
                                 InstSeq (InstInside (InstBot ty)) InstElim -> go ty
                                 InstSeq (InstInside (InstApp ty)) InstElim -> go ty
                                 _ -> False
                           )
                  )
                    ( let go ty =
                            case ty of
                              TVar name -> isJust (parseNameId name)
                              TArrow dom cod -> go dom && go cod
                              TForall _ _ body -> go body
                              _ -> False
                       in go
                    ),
                  TypeCheck.typeCheckWithEnv tcEnv app
                ) of
                (True, Left tcErr) ->
                  if take (length "TCArgumentMismatch") (show tcErr) == "TCArgumentMismatch"
                        || take (length "TCExpectedArrow") (show tcErr) == "TCExpectedArrow"
                        then
                          Left
                            (PhiTranslatabilityError
                              [ "AAppF: unresolved non-self polymorphic alias instantiation",
                                "function=" ++ show (sourceVarName fAnn),
                                "argument=" ++ show (sourceVarName aAnn),
                                "typeCheck=" ++ show tcErr
                              ]
                            )
                        else Right app
                _ -> Right app
       in mkOut f
    ALetF v _schemeGenId schemeRootId _ _rhsScopeGen (rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
      let elaborateLet env = do
            let debugGeneralize = traceGeneralize (algTraceConfig algebraContext)
            _ <-
              pure $
                debugGeneralize
                  ( "elaborate let("
                      ++ v
                      ++ "): schemeRootId="
                      ++ show schemeRootId
                      ++ " scopeRoot="
                      ++ show (scopeRootForNode scopeContext schemeRootId)
                  )
                  ()
            (scheme0Raw, subst0Raw) <- generalizeAtNode scopeContext schemeRootId
            let lambdaParamNodes annExpr =
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
                            ( \acc (name, paramNode) ->
                                let key = getNodeId (canonical paramNode)
                                 in IntMap.insertWith (\_ old -> old) key name acc
                            )
                            subst0'
                            binderPairs
                        else subst0'
                (scheme0Norm, subst0Norm) = normalizeSchemeSubstPair (scheme0Raw, subst0Raw)
                schemeBase = schemeFromType (simplifyAnnotationType (schemeToType scheme0Norm))
                {- Note [Mu-type annotation override for let schemes]
                   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                   When a let-bound RHS is a lambda with a μ-type annotation on its
                   parameter (e.g. let g = (λx:μα.α→Int. x) in …), the generalization
                   may produce an overly-generic scheme (∀a.∀b. a→b) because the
                   constraint graph's μ-node lives under the lambda scope and is not
                   visible as a binder-bound at the let scope.

                   We detect this case by inspecting the RHS annotation structure for
                   a desugared annotated lambda whose annotation node reifies to TMu.
                   When found, we override the scheme with a monomorphic function type
                   that uses the μ-type as both domain and codomain (identity-like), or
                   more precisely, domain = μ-type and codomain = μ-type when the body
                   simply returns the parameter. -}
                scheme =
                  case (annContainsVar v rhsAnn, blockedAliasMuType (schemeToType schemeBase)) of
                        (True, Just muTy) -> schemeFromType muTy
                        _ -> case
                          let isIdentityLikeSchemeType ty =
                                case ty of
                                  TForall name Nothing body -> isIdentityLikeBody name body
                                  _ -> False
                                where
                                  isIdentityLikeBody name body = case body of
                                    TArrow (TVar dom) (TVar cod) -> dom == name && cod == name
                                    _ -> False
                              muAnnotationTy annExpr =
                                case annExpr of
                                  ALam lamParam _ _ lamBody _ ->
                                    case desugaredAnnLambdaInfo lamParam lamBody of
                                      Just (annNodeId, _) ->
                                        case reifyNodeTypePreferringBound scopeContext annNodeId of
                                          Right annTy@TMu {} -> Just annTy
                                          _ -> Nothing
                                      Nothing -> Nothing
                                  AAnn inner _ _ -> muAnnotationTy inner
                                  _ -> Nothing
                           in case rhsAnn of
                                AApp funAnn argAnn _ _ _
                                  | maybe False (isIdentityLikeSchemeType . schemeToType . siScheme) (sourceVarName funAnn >>= (`Map.lookup` env)) ->
                                      muAnnotationTy argAnn
                                _ -> muAnnotationTy rhsAnn of
                          Just muTy ->
                            {- Note [Selective codomain override for μ-annotated lambdas]
                               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                               The domain is always overridden to the μ-type since
                               the surrounding μ-annotation detection confirms that the lambda parameter has
                               an explicit μ-annotation (e.g. λx:μα.α→Int. x).

                               For the codomain: when the scheme is fully polymorphic
                               (e.g. ∀a.∀b. a→b with both vars quantified), generalization
                               captured the correct parametricity and downstream elaboration
                               handles the μ-type through normal instantiation — so we leave
                               schemeBase intact. When the codomain is a constraint-internal
                               variable (e.g. TVar "t10" that wasn't quantified), generalization
                               lost track of its relationship to the μ-annotated parameter,
                               and we override it to the μ-type. -}
                            let schemeBody = case schemeToType schemeBase of
                                  TForall _ _ inner -> go inner
                                  other -> other
                                  where
                                    go (TForall _ _ inner) = go inner
                                    go t = t
                                quantVars = Set.fromList [n | (n, _) <- fst (Inst.splitForalls (schemeToType schemeBase))]
                                isUnquantifiedTVar (TVar v') = not (Set.member v' quantVars)
                                isUnquantifiedTVar _ = False
                             in case schemeBody of
                                  TArrow _dom cod
                                    | isUnquantifiedTVar cod ->
                                        -- Codomain is an unquantified internal variable:
                                        -- override both domain and codomain to μ.
                                        schemeFromType (TArrow muTy muTy)
                                  _ -> schemeBase
                          Nothing -> schemeBase
                subst0 = normalizeSubstForScheme scheme (deriveLambdaBinderSubst scheme0Norm subst0Norm)
                subst =
                  let (binds, _) = Inst.splitForalls (schemeToType scheme)
                   in if null binds then IntMap.empty else subst0
                schemeInfo = SchemeInfo {siScheme = scheme, siSubst = subst}
                env' = Map.insert v schemeInfo env
                tcEnv = TypeCheck.Env (Map.map (schemeToType . siScheme) env') Map.empty
            rhs' <- elabTerm rhsOut env'
            let rhsAbs0 =
                  let schemeTy = schemeToType scheme
                      rhsMatchesScheme rhsTy =
                        alphaEqType rhsTy schemeTy
                          || case schemeTy of
                            muTy@(TMu muName muBody) ->
                              let expectedBodyTy = substTypeCapture muName muTy muBody
                               in alphaEqType rhsTy expectedBodyTy
                            _ -> False
                   in case (rhs', TypeCheck.typeCheckWithEnv tcEnv rhs') of
                        (EVar _, _) ->
                          closeTermWithSchemeSubstIfNeeded subst scheme rhs'
                        (_, Right rhsTy)
                          | rhsMatchesScheme rhsTy -> rhs'
                        _ -> closeTermWithSchemeSubstIfNeeded subst scheme rhs'
                rhsAbs =
                  let schemeTy = schemeToType scheme
                   in case TypeCheck.typeCheckWithEnv tcEnv rhsAbs0 of
                        rhsAbs0Ty ->
                          case scheme of
                            Forall binds _
                              | not (null binds)
                              , Right rhsTy <- rhsAbs0Ty
                              , alphaEqType rhsTy schemeTy ->
                                  rhsAbs0
                              | not (null binds) ->
                                  case
                                      case (rhsAbs0, rhsAbs0Ty) of
                                        (ETyAbs _ _ body, Right (TForall _ _ bodyTy))
                                          | alphaEqType bodyTy schemeTy ->
                                              body
                                        _ -> stripUnusedTopTyAbs rhsAbs0
                                    of
                                    rhsAbsCandidate ->
                                      case TypeCheck.typeCheckWithEnv tcEnv rhsAbsCandidate of
                                        Right rhsTy
                                          | alphaEqType rhsTy schemeTy ->
                                              rhsAbsCandidate
                                        _ ->
                                          case rhsAbs0Ty of
                                            Left _ -> rhsAbsCandidate
                                            _ -> rhsAbs0
                            _ ->
                              case (rhsAbs0, rhsAbs0Ty) of
                                (ETyAbs _ _ body, Right (TForall _ _ bodyTy))
                                  | alphaEqType bodyTy schemeTy ->
                                      body
                                _ -> stripUnusedTopTyAbs rhsAbs0
                rhsAbsTyChecked = TypeCheck.typeCheckWithEnv tcEnv rhsAbs
            case debugGeneralize
              ( "elaborate let("
                  ++ v
                  ++ "): scheme="
                  ++ show scheme
                  ++ " subst="
                  ++ show subst
                  ++ " rhsAbs="
                  ++ show rhsAbs
                  ++ " rhsAbsTy="
                  ++ show rhsAbsTyChecked
              )
              () of
              () -> pure ()
            let rhsForRoll =
                  case schemeToType scheme of
                    muTy@(TMu muName muBody) ->
                      let expectedBodyTy = substTypeCapture muName muTy muBody
                       in case TypeCheck.typeCheckWithEnv tcEnv rhs' of
                            Right rhsTy
                              | alphaEqType rhsTy expectedBodyTy -> rhs'
                            _ -> rhsAbs
                    _ -> rhsAbs
            let bodyElab =
                  case bodyAnn of
                    AAnn _ target _ | canonical target == canonical trivialRoot -> elabStripped bodyOut
                    _ -> elabTerm bodyOut
            body' <-
              bodyElab
                ( Map.insert
                    v
                    ( case (rhs', TypeCheck.typeCheckWithEnv tcEnv rhs') of
                        (EVar _, Right rhsTy)
                          | not (alphaEqType rhsTy (schemeToType scheme)) ->
                              SchemeInfo
                                { siScheme = schemeFromType rhsTy
                                , siSubst = subst
                                }
                        _ -> schemeInfo
                    )
                    env
                )
            let rhsFinal =
                  case schemeToType scheme of
                    muTy@TMu {} -> ERoll muTy rhsForRoll
                    _ -> rhsAbs
            pure (scheme, rhsFinal, body')
          f env = do
            (scheme, rhsFinal, body') <- elaborateLet env
            pure (ELet v scheme rhsFinal body')
          fStripped env = do
            (scheme, rhsFinal, body') <- elaborateLet env
            if containsFreeVar v rhsFinal
              then pure (ELet v scheme rhsFinal body')
              else pure body'
       in ElabOut
            { elabTerm = f,
              elabStripped = fStripped
            }
    AAnnF (exprAnn, exprOut) annNodeId eid ->
      ElabOut
        { elabTerm = \env -> do
            expr' <- elabTerm exprOut env
            elaborateAnnotationTerm annotationContext namedSetReify env exprAnn annNodeId eid expr',
          elabStripped = \env -> elabTerm exprOut env
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

    insertMuUseSiteCoercions :: TypeCheck.Env -> ElabTerm -> ElabTerm -> Either ElabError ElabTerm
    insertMuUseSiteCoercions tcEnv fTerm aTerm = do
      let fUnrolled =
            case TypeCheck.typeCheckWithEnv tcEnv fTerm of
              Right muTy@TMu {} ->
                case unfoldMuOnce muTy of
                  Just TArrow {} -> EUnroll fTerm
                  _ -> fTerm
              _ -> fTerm
      aCoerced <-
        case TypeCheck.typeCheckWithEnv tcEnv fUnrolled of
          Right (TArrow paramTy _resTy) -> coerceArgForParam tcEnv paramTy aTerm
          _ -> Right aTerm
      pure (EApp fUnrolled aCoerced)

    unfoldMuOnce :: ElabType -> Maybe ElabType
    unfoldMuOnce muTy =
      case muTy of
        TMu name body -> Just (substTypeCapture name muTy body)
        _ -> Nothing

    coerceArgForParam :: TypeCheck.Env -> ElabType -> ElabTerm -> Either ElabError ElabTerm
    coerceArgForParam tcEnv paramTy argTerm =
      case TypeCheck.typeCheckWithEnv tcEnv argTerm of
        Left _ -> Right argTerm
        Right argTy ->
          case paramTy of
            muTy@TMu {} ->
              case unfoldMuOnce muTy of
                Just unfoldedTy
                  | alphaEqType unfoldedTy argTy -> Right (ERoll muTy argTerm)
                _ | shouldRollMuVar muTy argTy -> Right (ERoll muTy argTerm)
                _ -> Right argTerm
            _ ->
              case argTy of
                muTy@TMu {} ->
                  case unfoldMuOnce muTy of
                    Just unfoldedTy
                      | alphaEqType paramTy unfoldedTy -> Right (EUnroll argTerm)
                    _ -> Right argTerm
                _ -> Right argTerm

    containsFreeVar :: VarName -> ElabTerm -> Bool
    containsFreeVar v term =
      case term of
        EVar x -> x == v
        ELit _ -> False
        ELam x _ body
          | x == v -> False
          | otherwise -> containsFreeVar v body
        EApp f a -> containsFreeVar v f || containsFreeVar v a
        ELet x _ rhs body
          | x == v -> containsFreeVar v rhs
          | otherwise -> containsFreeVar v rhs || containsFreeVar v body
        ETyAbs _ _ body -> containsFreeVar v body
        ETyInst e _ -> containsFreeVar v e
        ERoll _ body -> containsFreeVar v body
        EUnroll e -> containsFreeVar v e

    annContainsVar :: VarName -> AnnExpr -> Bool
    annContainsVar v annExpr =
      case annExpr of
        ALit _ _ -> False
        AVar x _ -> x == v
        ALam x _ _ body _
          | x == v -> False
          | otherwise -> annContainsVar v body
        AApp f a _ _ _ -> annContainsVar v f || annContainsVar v a
        ALet x _ _ _ _ rhs body _
          | x == v -> annContainsVar v rhs
          | otherwise -> annContainsVar v rhs || annContainsVar v body
        AAnn inner _ _ -> annContainsVar v inner

    blockedAliasMuType :: ElabType -> Maybe ElabType
    blockedAliasMuType ty =
      case ty of
        TForall a Nothing (TArrow (TVar b) cod)
          | a == b -> Just (TMu a (TArrow (TVar a) cod))
        _ -> Nothing

    shouldRollMuVar :: ElabType -> ElabType -> Bool
    shouldRollMuVar muTy argTy =
      case (muTy, argTy) of
        (TMu _ _, TVar _) -> True
        _ -> False

mkOut :: (Env -> Either ElabError ElabTerm) -> ElabOut
mkOut f = ElabOut f f

resolvedLambdaParamNode :: (NodeId -> NodeId) -> (NodeId -> Maybe TyNode) -> NodeId -> Maybe NodeId
resolvedLambdaParamNode canonical chiLookupNode lamNodeId =
  let lamC = canonical lamNodeId
   in case chiLookupNode lamC of
        Just TyArrow {tnDom = dom} -> Just dom
        Just TyVar {tnBound = Just bnd} ->
          case chiLookupNode (canonical bnd) of
            Just TyArrow {tnDom = dom} -> Just dom
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
