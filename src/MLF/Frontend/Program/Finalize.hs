{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize
  ( finalizeBinding,
  )
where

import Data.List (find, sort)
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
    runPipelineElabWithEnv,
    schemeToType,
    typeCheckWithEnv,
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
    resolveInstanceInfo,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    DataInfo (..),
    DeferredMethodCall (..),
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
  (term0, actualTy0, tcEnv) <- runSurfacePipeline scope (loweredBindingExternalTypes lowered) (loweredBindingSurfaceExpr lowered)
  (term, actualTy) <- finalizeDeferredMethods scope (loweredBindingDeferredMethods lowered) tcEnv term0 actualTy0
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

runSurfacePipeline :: ElaborateScope -> Map String SrcType -> SurfaceExpr -> Either ProgramError (ElabTerm, ElabType, Env)
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
      tcEnv =
        Env
          { termEnv = Map.map srcTypeToElabType extEnv,
            typeEnv = Map.empty
          }
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  (term, inferredTy) <- either (Left . ProgramPipelineError . renderPipelineError) Right (runPipelineElabWithEnv Set.empty extEnv normExpr)
  pure (term, inferredTy, tcEnv)
  where
    runtimeTypes = externalTypes `Map.union` elaborateScopeRuntimeTypes scope

    resolveRuntimeType name =
      case Map.lookup name runtimeTypes of
        Just ty -> Right (name, ty)
        Nothing -> Left (ProgramUnknownValue name)

finalizeDeferredMethods ::
  ElaborateScope ->
  Map String DeferredMethodCall ->
  Env ->
  ElabTerm ->
  ElabType ->
  Either ProgramError (ElabTerm, ElabType)
finalizeDeferredMethods _ deferredMethods _ term inferredTy
  | Map.null deferredMethods = Right (term, inferredTy)
finalizeDeferredMethods scope deferredMethods tcEnv term _ = do
  let rewriteEnv = extendTypeCheckEnvWithRuntimeScope scope tcEnv
  rewritten <- resolveDeferredMethods scope deferredMethods rewriteEnv term
  rewrittenTy <-
    either
      (Left . ProgramPipelineError . ("deferred method rewrite failed type check: " ++) . show)
      Right
      (typeCheckWithEnv rewriteEnv rewritten)
  Right (rewritten, rewrittenTy)

extendTypeCheckEnvWithRuntimeScope :: ElaborateScope -> Env -> Env
extendTypeCheckEnvWithRuntimeScope scope env =
  env
    { termEnv =
        termEnv env
          `Map.union` Map.map srcTypeToElabType (elaborateScopeRuntimeTypes scope)
    }

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
    nullaryHeads :: [(ElabType, SrcType)]
    nullaryHeads =
      [ (srcTypeToElabType (lowerType scope (STBase name)), STBase name)
        | (name, info) <- Map.toList (elaborateScopeDataTypes scope),
          null (dataParams info)
      ]

    recover ty =
      case lookupHead ty of
        Just headTy -> headTy
        Nothing -> recoverChildren ty

    lookupHead ty =
      let elabTy = srcTypeToElabType ty
       in case find (\(lowered, _) -> alphaEqType elabTy lowered || churchAwareEqType elabTy lowered) nullaryHeads of
            Just (_, headTy) -> Just headTy
            Nothing -> Nothing

    recoverChildren ty = case ty of
      STVar {} -> ty
      STBase {} -> ty
      STBottom -> ty
      STArrow dom cod -> STArrow (recover dom) (recover cod)
      STForall name mb body ->
        STForall name (fmap (SrcBound . recover . unSrcBound) mb) (recover body)
      STMu name body -> STMu name (recover body)
      STCon name args -> STCon name (fmap recover args)

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
