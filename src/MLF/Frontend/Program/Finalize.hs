{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize
  ( finalizeBinding,
  )
where

import Control.Monad (foldM)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Elab.Pipeline
  ( Env (..),
    ExternalEnv,
    renderPipelineError,
    schemeToType,
    typeCheckWithEnv,
  )
import MLF.Elab.Run.Pipeline
  ( PipelineElabDetailedResult (..),
    runPipelineElabDetailedWithEnv,
  )
import MLF.Elab.Types (ElabTerm, ElabType)
import qualified MLF.Elab.Types as X
import MLF.Frontend.Normalize (normalizeExpr, normalizeType)
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeRuntimeTypes,
    inferClassArgument,
    lowerType,
    matchTypes,
    freeTypeVarsSrcType,
    resolveInstanceInfo,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    ConstructorInfo (..),
    DataInfo (..),
    DeferredCaseCall (..),
    DeferredConstructorCall (..),
    DeferredMethodCall (..),
    DeferredProgramObligation (..),
    InstanceInfo (..),
    LoweredBinding (..),
    MethodInfo (..),
    ProgramError (..),
    ValueInfo (..),
  )
import MLF.Frontend.Syntax (Expr (..), SrcBound (..), SrcTy (..), SrcType, SurfaceExpr)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarsType)

finalizeBinding :: ElaborateScope -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBinding scope lowered = do
  PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipeline scope (loweredBindingExternalTypes lowered) (loweredBindingSurfaceExpr lowered)
  (term, actualTy) <-
    finalizeDeferredObligations scope (loweredBindingDeferredObligations lowered) tcEnv term0 actualTy0
  let actualTyForCompare = stripVacuousForalls actualTy
      expectedTyForCompare = stripVacuousForalls (srcTypeToElabType (loweredBindingExpectedType lowered))
      recoveredActualSrcTy = recoverSourceType scope (elabTypeToSrcType actualTyForCompare)
      recoveredActualTy = srcTypeToElabType (lowerType scope recoveredActualSrcTy)
  if alphaEqType actualTyForCompare expectedTyForCompare
    || churchAwareEqType actualTyForCompare expectedTyForCompare
    || alphaEqType recoveredActualTy expectedTyForCompare
    || churchAwareEqType recoveredActualTy expectedTyForCompare
    then
      Right
        CheckedBinding
          { checkedBindingName = loweredBindingName lowered,
            checkedBindingSourceType = loweredBindingExpectedType lowered,
            checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
            checkedBindingTerm = term,
            checkedBindingType = actualTy,
            checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
          }
    else Left (ProgramTypeMismatch recoveredActualSrcTy (loweredBindingExpectedType lowered))

runSurfacePipeline :: ElaborateScope -> Map String SrcType -> SurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipeline scope externalTypes surfaceExpr = do
  let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
  envBindings <- traverse resolveRuntimeType freeVars
  let extEnv :: ExternalEnv
      extEnv =
        Map.fromList
          [ (name, normTy)
            | (name, ty) <- envBindings,
              let normTy = either (error . show) id (normalizeType ty)
          ]
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  either (Left . ProgramPipelineError . renderPipelineError) Right (runPipelineElabDetailedWithEnv Set.empty extEnv normExpr)
  where
    runtimeTypes = externalTypes `Map.union` elaborateScopeRuntimeTypes scope

    resolveRuntimeType name =
      case Map.lookup name runtimeTypes of
        Just ty -> Right (name, ty)
        Nothing -> Left (ProgramUnknownValue name)

finalizeDeferredObligations ::
  ElaborateScope ->
  Map String DeferredProgramObligation ->
  Env ->
  ElabTerm ->
  ElabType ->
  Either ProgramError (ElabTerm, ElabType)
finalizeDeferredObligations _ deferredObligations _ term inferredTy
  | Map.null deferredObligations = Right (term, inferredTy)
finalizeDeferredObligations scope deferredObligations tcEnv term _ = do
  let rewriteEnv = extendTypeCheckEnvWithRuntimeScope scope tcEnv
      constructorObligations = Map.mapMaybe onlyConstructor deferredObligations
      caseObligations = Map.mapMaybe onlyCase deferredObligations
      methodObligations = Map.mapMaybe onlyMethod deferredObligations
  constructorsRewritten <- resolveDeferredConstructors scope rewriteEnv constructorObligations term
  (caseRewriteEnv, casesRewritten) <- resolveDeferredCases scope caseObligations rewriteEnv constructorsRewritten
  rewritten <- resolveDeferredMethods scope methodObligations caseRewriteEnv casesRewritten
  rewrittenTy <-
    fmap (inlineTypeEnvBounds caseRewriteEnv) $
      either
      (Left . ProgramPipelineError . ("deferred program obligation rewrite failed type check: " ++) . show)
      Right
      (typeCheckWithEnv caseRewriteEnv rewritten)
  Right (rewritten, rewrittenTy)
  where
    onlyConstructor = \case
      DeferredConstructor deferred -> Just deferred
      _ -> Nothing

    onlyCase = \case
      DeferredCase deferred -> Just deferred
      _ -> Nothing

    onlyMethod = \case
      DeferredMethod deferred -> Just deferred
      _ -> Nothing

extendTypeCheckEnvWithRuntimeScope :: ElaborateScope -> Env -> Env
extendTypeCheckEnvWithRuntimeScope scope env =
  env
    { termEnv =
        termEnv env
          `Map.union` Map.map srcTypeToElabType (elaborateScopeRuntimeTypes scope)
    }

inlineTypeEnvBounds :: Env -> ElabType -> ElabType
inlineTypeEnvBounds env = go Set.empty
  where
    go seen ty = case ty of
      X.TVar name
        | name `Set.member` seen -> ty
        | otherwise ->
            case Map.lookup name (typeEnv env) of
              Just bound
                | bound /= X.TBottom -> go (Set.insert name seen) bound
              _ -> ty
      X.TArrow dom cod -> X.TArrow (go seen dom) (go seen cod)
      X.TCon con args -> X.TCon con (fmap (go seen) args)
      X.TBase {} -> ty
      X.TBottom -> ty
      X.TForall name mb body ->
        let seen' = Set.insert name seen
         in X.TForall name (fmap (goBound seen') mb) (go seen' body)
      X.TMu name body ->
        let seen' = Set.insert name seen
         in X.TMu name (go seen' body)

    goBound seen bound = case bound of
      X.TArrow dom cod -> X.TArrow (go seen dom) (go seen cod)
      X.TCon con args -> X.TCon con (fmap (go seen) args)
      X.TBase {} -> bound
      X.TBottom -> bound
      X.TForall name mb body ->
        let seen' = Set.insert name seen
         in X.TForall name (fmap (goBound seen') mb) (go seen' body)
      X.TMu name body ->
        let seen' = Set.insert name seen
         in X.TMu name (go seen' body)

resolveDeferredConstructors :: ElaborateScope -> Env -> Map String DeferredConstructorCall -> ElabTerm -> Either ProgramError ElabTerm
resolveDeferredConstructors scope env deferredConstructors = go env
  where
    go env0 term =
      case term of
        X.EVar name ->
          case Map.lookup name deferredConstructors of
            Just deferred
              | deferredConstructorArgCount deferred == 0 ->
                  instantiateConstructorHead env0 (deferredConstructorInfo deferred) []
            _ -> Right term
        X.ELit {} -> Right term
        X.ELam name ty body ->
          let env' = env0 {termEnv = Map.insert name ty (termEnv env0)}
           in X.ELam name ty <$> go env' body
        X.EApp {} -> rewriteApplication env0 term
        X.ELet name scheme rhs body -> do
          let schemeTy = schemeToType scheme
              env' = env0 {termEnv = Map.insert name schemeTy (termEnv env0)}
          X.ELet name scheme <$> go env0 rhs <*> go env' body
        X.ETyAbs name mbBound body ->
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = env0 {typeEnv = Map.insert name boundTy (typeEnv env0)}
           in X.ETyAbs name mbBound <$> go env' body
        X.ETyInst inner inst -> (`X.ETyInst` inst) <$> go env0 inner
        X.ERoll ty body -> X.ERoll ty <$> go env0 body
        X.EUnroll inner -> X.EUnroll <$> go env0 inner

    rewriteApplication env0 term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHead headTerm >>= (`Map.lookup` deferredConstructors) of
            Just deferred -> do
              args' <- mapM (go env0) args
              ctorHead <- instantiateConstructorHead env0 (deferredConstructorInfo deferred) args'
              Right (foldl X.EApp ctorHead args')
            Nothing ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env0 fun <*> go env0 arg
                _ -> Right term

    instantiateConstructorHead env0 ctorInfo args = do
      let runtimeName = ctorRuntimeName ctorInfo
          freeVars = sort (Set.toList (freeTypeVarsSrcType (ctorType ctorInfo)))
      argTypes <- mapM (inferArgSourceType env0) (take (length (ctorArgs ctorInfo)) args)
      let subst =
            foldM
              (\acc (templateTy, actualTy) -> matchTypes acc templateTy actualTy)
              Map.empty
              (zip (ctorArgs ctorInfo) argTypes)
      case subst of
        Nothing -> Right (X.EVar runtimeName)
        Just subst' ->
          Right $
            foldl
              ( \headAcc varName ->
                  case Map.lookup varName subst' of
                    Just ty -> X.ETyInst headAcc (X.InstApp (srcTypeToElabType (lowerType scope ty)))
                    Nothing -> headAcc
              )
              (X.EVar runtimeName)
              freeVars

    inferArgSourceType env0 arg =
      case typeCheckWithEnv env0 arg of
        Right ty -> Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty)))
        Left err -> Left (ProgramPipelineError ("deferred constructor argument type check failed: " ++ show err))

resolveDeferredCases :: ElaborateScope -> Map String DeferredCaseCall -> Env -> ElabTerm -> Either ProgramError (Env, ElabTerm)
resolveDeferredCases scope deferredCases = go
  where
    go env term =
      case term of
        X.EVar {} -> Right (env, term)
        X.ELit {} -> Right (env, term)
        X.ELam name ty body -> do
          let env' = env {termEnv = Map.insert name ty (termEnv env)}
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env bodyEnv, X.ELam name ty body')
        X.EApp {} -> rewriteApplication env term
        X.ELet name scheme rhs body -> do
          let schemeTy = schemeToType scheme
              env' = env {termEnv = Map.insert name schemeTy (termEnv env)}
          (rhsEnv, rhs') <- go env rhs
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env (mergeCaseEnv rhsEnv bodyEnv), X.ELet name scheme rhs' body')
        X.ETyAbs name mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = env {typeEnv = Map.insert name boundTy (typeEnv env)}
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env bodyEnv, X.ETyAbs name mbBound body')
        X.ETyInst inner inst -> do
          (innerEnv, inner') <- go env inner
          Right (innerEnv, X.ETyInst inner' inst)
        X.ERoll ty body -> do
          (bodyEnv, body') <- go env body
          Right (bodyEnv, X.ERoll ty body')
        X.EUnroll inner -> do
          (innerEnv, inner') <- go env inner
          Right (innerEnv, X.EUnroll inner')

    rewriteApplication env term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHead headTerm >>= (`Map.lookup` deferredCases) of
            Just deferred -> do
              (argEnv, args') <- mapAccumCaseEnv env args
              resolveDeferredCaseApplication argEnv deferred args'
            Nothing ->
              case term of
                X.EApp fun arg -> do
                  (funEnv, fun') <- go env fun
                  (argEnv, arg') <- go env arg
                  Right (mergeCaseEnv funEnv argEnv, X.EApp fun' arg')
                _ -> Right (env, term)

    resolveDeferredCaseApplication env deferred args =
      case args of
        scrutinee : handlers
          | length args == deferredCaseExpectedArgCount deferred -> do
              (_scrutineeElabTy, scrutineeRawTy, scrutineeRecoveredTy) <- inferDeferredArgType env scrutinee
              validateCaseScrutineeType (deferredCaseDataInfo deferred) scrutineeRecoveredTy
              let resultTy = srcTypeToElabType (lowerType scope (deferredCaseResultType deferred))
                  caseHead = caseEliminator resultTy scrutinee
                  env' = extendCaseResultEnv (deferredCaseDataInfo deferred) scrutineeRawTy resultTy env
              Right (env', foldl X.EApp caseHead handlers)
        _ -> Left (ProgramCaseOnNonDataType STBottom)

    validateCaseScrutineeType dataInfo scrutineeTy =
      case scrutineeTy of
        STBase name
          | name == dataName dataInfo -> Right ()
        STCon name _
          | name == dataName dataInfo -> Right ()
        other -> Left (ProgramCaseOnNonDataType other)

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty ->
          let rawTy = elabTypeToSrcType (stripVacuousForalls ty)
           in Right (ty, rawTy, recoverSourceType scope rawTy)
        Left err ->
          Left (ProgramPipelineError ("deferred case scrutinee type check failed: " ++ show err))

    caseEliminator resultTy scrutinee =
      X.ETyInst (X.EUnroll scrutinee) (X.InstApp resultTy)

    extendCaseResultEnv dataInfo scrutineeRawTy resultTy env =
      case matchDataInfoEncoding scope dataInfo scrutineeRawTy of
        Just (sourceHeadTy, subst) ->
          let resultName = "$" ++ dataName dataInfo ++ "_result"
              headTy = srcTypeToElabType (lowerType scope sourceHeadTy)
              resultBinding =
                case Map.lookup resultName subst of
                  Just (STVar resultVar) -> Map.singleton resultVar resultTy
                  _ -> Map.empty
              selfAliasBindings =
                case scrutineeRawTy of
                  STMu actualSelf _ ->
                    Map.fromList
                      [ (alias, headTy)
                        | (alias, STVar actualSelf') <- Map.toList subst,
                          actualSelf' == actualSelf,
                          alias /= actualSelf,
                          alias /= resultName,
                          alias `notElem` dataParams dataInfo
                      ]
                  _ -> Map.empty
           in env {typeEnv = selfAliasBindings `Map.union` resultBinding `Map.union` typeEnv env}
        Nothing -> env

    mapAccumCaseEnv env [] = Right (env, [])
    mapAccumCaseEnv env (arg : rest) = do
      (env1, arg') <- go env arg
      (env2, rest') <- mapAccumCaseEnv env1 rest
      Right (env2, arg' : rest')

    mergeCaseEnv base incoming =
      base {typeEnv = typeEnv incoming `Map.union` typeEnv base}

resolveDeferredMethods :: ElaborateScope -> Map String DeferredMethodCall -> Env -> ElabTerm -> Either ProgramError ElabTerm
resolveDeferredMethods scope deferredMethods = go
  where
    go env term =
      case term of
        X.EVar {} -> Right term
        X.ELit {} -> Right term
        X.ELam name ty body -> do
          let env' = env {termEnv = Map.insert name ty (termEnv env)}
          X.ELam name ty <$> go env' body
        X.EApp {} -> rewriteApplication env term
        X.ELet name scheme rhs body -> do
          let schemeTy = schemeToType scheme
              env' = env {termEnv = Map.insert name schemeTy (termEnv env)}
          rhs' <- go env' rhs
          body' <- go env' body
          Right (X.ELet name scheme rhs' body')
        X.ETyAbs name mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = env {typeEnv = Map.insert name boundTy (typeEnv env)}
          X.ETyAbs name mbBound <$> go env' body
        X.ETyInst inner inst ->
          (`X.ETyInst` inst) <$> go env inner
        X.ERoll ty body ->
          X.ERoll ty <$> go env body
        X.EUnroll inner ->
          X.EUnroll <$> go env inner

    rewriteApplication env term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHead headTerm >>= (`Map.lookup` deferredMethods) of
            Just deferred -> do
              args' <- mapM (go env) args
              resolveDeferredApplication env deferred args'
            Nothing ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env fun <*> go env arg
                _ -> Right term

    resolveDeferredApplication env deferred args = do
      let methodInfo = deferredMethodInfo deferred
          requiredArgCount = deferredMethodArgCount deferred
      if length args < requiredArgCount
        then Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
        else do
          argTypes <- mapM (inferDeferredArgType env) (take requiredArgCount args)
          classArgTy <-
            case inferClassArgument (lowerType scope (methodType methodInfo)) (methodParamName methodInfo) argTypes of
              Just ty -> Right ty
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          instanceInfo <- resolveInstanceInfo scope (methodClassName methodInfo) classArgTy
          runtimeName <- concreteMethodRuntimeName instanceInfo methodInfo
          Right (foldl X.EApp (X.EVar runtimeName) args)

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty ->
          Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty)))
        Left err ->
          Left (ProgramPipelineError ("deferred method argument type check failed: " ++ show err))

    concreteMethodRuntimeName instanceInfo methodInfo =
      case Map.lookup (methodName methodInfo) (instanceMethods instanceInfo) of
        Just OrdinaryValue {valueRuntimeName = runtimeName} -> Right runtimeName
        _ -> Left (ProgramUnknownMethod (methodName methodInfo))

collectElabApps :: ElabTerm -> (ElabTerm, [ElabTerm])
collectElabApps = go []
  where
    go args term =
      case term of
        X.EApp fun arg -> go (arg : args) fun
        _ -> (term, args)

deferredPlaceholderHead :: ElabTerm -> Maybe String
deferredPlaceholderHead term =
  case term of
    X.EVar name -> Just name
    X.ETyInst inner _ -> deferredPlaceholderHead inner
    _ -> Nothing

{- Note [recoverSourceType]

When the eMLF pipeline infers a type, it returns raw Church-encoded μ forms
with fresh binder names.  The .mlfp layer still needs named source ADT heads
for diagnostics and instance-head comparisons.  This recovery is deliberately
downstream of lowering: `Program.Elaborate` never invokes the pipeline.
-}
recoverSourceType :: ElaborateScope -> SrcType -> SrcType
recoverSourceType scope = recover
  where
    dataInfos = Map.elems (elaborateScopeDataTypes scope)

    recover ty =
      case lookupHead ty of
        Just headTy -> headTy
        Nothing -> recoverChildren ty

    lookupHead ty =
      case mapMaybeDataHead ty dataInfos of
        (headTy : _) -> Just headTy
        [] -> Nothing

    mapMaybeDataHead ty =
      foldr
        ( \info acc ->
            case recoverDataHead ty info of
              Just headTy -> headTy : acc
              Nothing -> acc
        )
        []

    recoverDataHead ty info =
      fst <$> matchDataInfoEncodingWith recover scope info ty

    recoverChildren ty = case ty of
      STVar {} -> ty
      STBase {} -> ty
      STBottom -> ty
      STArrow dom cod -> STArrow (recover dom) (recover cod)
      STForall name mb body ->
        STForall name (fmap (SrcBound . recover . unSrcBound) mb) (recover body)
      STMu name body -> STMu name (recover body)
      STCon name args -> STCon name (fmap recover args)

matchDataInfoEncoding :: ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncoding = matchDataInfoEncodingWith id

matchDataInfoEncodingWith :: (SrcType -> SrcType) -> ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncodingWith recover scope info ty =
  let params = dataParams info
      templateHead =
        case params of
          [] -> STBase (dataName info)
          p : ps -> STCon (dataName info) (STVar p :| map STVar ps)
      loweredTemplate = lowerType scope templateHead
      matchTemplate template =
        matchRecoverType (Set.fromList params) Map.empty Map.empty template ty
      matched =
        case matchTemplate loweredTemplate of
          Just subst -> Just subst
          Nothing ->
            case loweredTemplate of
              STMu _ body -> matchTemplate body
              _ -> Nothing
   in case matched of
        Just subst ->
          let recoveredArgs = map (\param -> recover (Map.findWithDefault (STVar param) param subst)) params
              recoveredHead =
                case recoveredArgs of
                  [] -> STBase (dataName info)
                  arg : args -> STCon (dataName info) (arg :| args)
           in Just (recoveredHead, subst)
        Nothing -> Nothing

matchRecoverType ::
  Set String ->
  Map String SrcType ->
  Map String String ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchRecoverType params subst renames template actual =
  case template of
    STVar name
      | name `Set.member` params ->
          bindRecoverParam name actual subst
      | Just actualName <- Map.lookup name renames ->
          case actual of
            STVar name' | name' == actualName -> Just subst
            STVar name' -> Just (Map.insert name' (STVar actualName) subst)
            _ -> Nothing
      | otherwise ->
          case actual of
            STVar name' | name' == name -> Just subst
            _ -> Nothing
    STArrow dom cod ->
      case actual of
        STArrow dom' cod' -> do
          subst' <- matchRecoverType params subst renames dom dom'
          matchRecoverType params subst' renames cod cod'
        _ -> Nothing
    STBase name ->
      case actual of
        STBase name' | name == name' -> Just subst
        _ -> Nothing
    STCon name args ->
      case actual of
        STCon name' args'
          | name == name' && length (toListNE args) == length (toListNE args') ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType params acc renames leftTy rightTy)
                subst
                (zip (toListNE args) (toListNE args'))
        _ -> Nothing
    STForall name _mb body ->
      case actual of
        STForall name' _mb' body' ->
          matchRecoverType params subst (Map.insert name name' renames) body body'
        _ ->
          matchRecoverType (Set.insert name params) subst renames body actual
    STMu name body ->
      case actual of
        STMu name' body' ->
          matchRecoverType params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STBottom ->
      case actual of
        STBottom -> Just subst
        _ -> Nothing

bindRecoverParam :: String -> SrcType -> Map String SrcType -> Maybe (Map String SrcType)
bindRecoverParam name actual subst =
  case Map.lookup name subst of
    Nothing -> Just (Map.insert name actual subst)
    Just existing
      | alphaEqType (srcTypeToElabType existing) (srcTypeToElabType actual)
          || churchAwareEqType (srcTypeToElabType existing) (srcTypeToElabType actual) ->
          Just subst
      | otherwise -> Nothing

stripVacuousForalls :: ElabType -> ElabType
stripVacuousForalls (X.TForall v _ body)
  | v `Set.notMember` freeTypeVarsType body = stripVacuousForalls body
stripVacuousForalls (X.TForall v mb body) =
  X.TForall v mb (stripVacuousForalls body)
stripVacuousForalls (X.TArrow dom cod) =
  X.TArrow (stripVacuousForalls dom) (stripVacuousForalls cod)
stripVacuousForalls (X.TMu name body) =
  X.TMu name (stripVacuousForalls body)
stripVacuousForalls ty = ty

surfaceFreeVars :: SurfaceExpr -> Set String
surfaceFreeVars = go Set.empty
  where
    go bound expr = case expr of
      EVar name
        | name `Set.member` bound -> Set.empty
        | otherwise -> Set.singleton name
      ELit _ -> Set.empty
      ELam name body -> go (Set.insert name bound) body
      ELamAnn name _ body -> go (Set.insert name bound) body
      EApp fun arg -> go bound fun `Set.union` go bound arg
      ELet name rhs body -> go (Set.insert name bound) rhs `Set.union` go (Set.insert name bound) body
      EAnn inner _ -> go bound inner
      ECoerceConst _ -> Set.empty

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

elabTypeToSrcType :: X.Ty v -> SrcType
elabTypeToSrcType ty = case ty of
  X.TVar name -> STVar name
  X.TArrow dom cod -> STArrow (elabTypeToSrcType dom) (elabTypeToSrcType cod)
  X.TBase (Graph.BaseTy name) -> STBase name
  X.TCon (Graph.BaseTy name) args ->
    case toListNE (fmap elabTypeToSrcType args) of
      x : xs -> STCon name (x :| xs)
      [] -> STBase name
  X.TForall name mb body ->
    STForall name (fmap (SrcBound . elabTypeToSrcType) mb) (elabTypeToSrcType body)
  X.TMu name body -> STMu name (elabTypeToSrcType body)
  X.TBottom -> STBottom

srcTypeToElabType :: SrcTy n v -> ElabType
srcTypeToElabType ty = case ty of
  STVar name -> X.TVar name
  STArrow dom cod -> X.TArrow (srcTypeToElabType dom) (srcTypeToElabType cod)
  STBase name -> X.TBase (Graph.BaseTy name)
  STCon name args -> X.TCon (Graph.BaseTy name) (fmap srcTypeToElabType args)
  STForall name mb body -> X.TForall name (mb >>= srcBoundToElabBound) (srcTypeToElabType body)
  STMu name body -> X.TMu name (srcTypeToElabType body)
  STBottom -> X.TBottom

srcBoundToElabBound :: SrcBound n -> Maybe X.BoundType
srcBoundToElabBound (SrcBound boundTy) =
  case srcTypeToElabType boundTy of
    X.TVar {} -> Nothing
    X.TBottom -> Nothing
    X.TArrow dom cod -> Just (X.TArrow dom cod)
    X.TBase base -> Just (X.TBase base)
    X.TCon con args -> Just (X.TCon con args)
    X.TForall name mb body -> Just (X.TForall name mb body)
    X.TMu name body -> Just (X.TMu name body)
