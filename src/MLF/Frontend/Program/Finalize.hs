{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize
  ( FinalizeContext,
    ModuleFinalizeContext,
    mkFinalizeContext,
    mkModuleFinalizeContext,
    finalizeBinding,
    finalizeBindingWithContext,
    finalizeBindingsAllowOpaqueWithContext,
    finalizeBindingsAllowOpaqueWithContextWithTiming,
    finalizeBindingAllowOpaque,
    finalizeBindingAllowOpaqueWithContext,
    finalizeBindingAllowOpaqueWithModuleContext,
    finalizeBindingLayerAllowOpaqueWithModuleContext,
    finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming,
    finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming,
    finalizeBindingAllowOpaqueWithContextWithTiming,
    finalizeBindingAllowOpaqueWithModuleContextWithTiming,
    recoverSourceType,
    typeViewToElabType,
    resolvedForallSubst,
    sourceForallMatches,
    stripVacuousForallsAndTypeAbs,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (foldM, zipWithM)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap.Strict as IntMap
import Data.List (isPrefixOf, mapAccumL, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Elab.Pipeline
  ( Env (..),
    renderPipelineError,
    schemeFromType,
    schemeToType,
    typeCheckWithEnv,
  )
import MLF.Elab.Run.Pipeline
  ( PipelineElabDetailedResult (..),
    PreparedExternalBindings,
    prepareExternalBindings,
    restrictPreparedExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming,
    freshenTypeAbsAgainstEnv,
    unionPreparedExternalBindings,
  )
import MLF.Elab.TermClosure (closeTermWithSchemeSubstRefsIfNeeded)
import MLF.Elab.Types (XmlfTerm, ElabType)
import qualified MLF.Elab.Types as X
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Frontend.ConstraintGen (ExternalBinding (..), ExternalBindingIdentity (..), ExternalBindingMode (..))
import MLF.Frontend.Normalize (normalizeExpr, normalizeType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol (symbolIdentityStableName)
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeDataTypesByIdentity,
    elaborateScopeRuntimeTypes,
    elaborateScopeUniqueDataTypes,
    elaborateScopeValues,
    classInfoForConstraint,
    diagnosticTypeViewDisplay,
    inferClassArgument,
    lookupEvidenceMethodByClass,
    lookupEvidenceMethodByClassTypes,
    lowerType,
    lowerTypeView,
    matchTypesInScope,
    matchTypeViewsAgainstIdentity,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    sourceTypeViewInScope,
    zeroMethodConstraintCoveredByEvidenceInfo,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    ConstructorRef (..),
    ConstructorInfo (..),
    ConstructorShape (..),
    DataInfo (..),
    DeferredMethodEvidence (..),
    DeferredCaseCall (..),
    DeferredBindingMode (..),
    DeferredConstructorCall (..),
    DeferredMethodCall (..),
    DeferredProgramObligation (..),
    DeferredObligations,
    ClassInfo (..),
    EvidenceMethod (..),
    EvidenceInfo (..),
    InstanceInfo (..),
    IdDetails (..),
    LoweredBinding (..),
    LoweredBindingIdentity (..),
    MethodInfo (..),
    ProgramError (..),
    ConstraintInfo (..),
    TypeView (..),
    TypeBinderSubst,
    TypeViewSubstKey (..),
    TypeViewSubst,
    ValueInfo (..),
    applyConstraintInfoSubst,
    constructorRefFromInfo,
    constructorOwnerRuntimeTypeTrackable,
    constructorOwnerShapes,
    constructorShapeFromInfo,
    constructorShapeName,
    constructorInfoIdentityName,
    dataInfoIdentityHeadName,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataParams,
    deferredCasePlaceholder,
    deferredConstructorPlaceholder,
    deferredMethodPlaceholder,
    deferredMethodName,
    deferredProgramObligationRef,
    emptyTypeBinderSubst,
    freeTypeVarsTypeView,
    freeTypeVarsTypeViews,
    constraintTypeView,
    lookupInstanceMethod,
    ctorName,
    lookupTypeViewSubst,
    lookupMethodParamViewSubst,
    methodTypeView,
    methodResultTypeView,
    methodName,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    methodParamIdentityName,
    methodParamBinders,
    methodParamName,
    loweredBindingConstructorRef,
    loweredBindingName,
    resolvedVarFromLoweredBinding,
    resolvedVarFromValueInfo,
    SymbolIdentity,
    symbolDefiningName,
    splitArrows,
    splitForalls,
    specializeMethodTypeView,
    substituteTypeVar,
    typeViewSubstFromParamBinders,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToNameMap,
    typeBinderSubstToTypeViewSubstWith,
    typeViewsIdentity,
    insertTypeBinderSubst,
    lookupTypeBinderSubst,
    mkTypeView,
    unqualifiedSymbolName,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), NormSurfaceExpr, SrcBound (..), SrcTy (..), SrcType, SurfaceExpr)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, splitForallsRefs)
import MLF.Types.Identity
  ( DeferredRef (..),
    EnvRef (..),
    IdentityGenerator,
    LocalIdentity (..),
    LocalRef (..),
    PrimitiveRef (..),
    TypeBinderIdentity,
    UniqueIdentity,
    freshDeferredRef,
    freshEnvRef,
    freshLocalRef,
    idDetailsGeneratedIdentities,
    identityGeneratorAfter,
    initialIdentityGenerator,
    renameDeferredRef,
  )
import MLF.Util.Timing (TimingConfig(..), defaultTimingConfig, timeProgramOperationIO)

data FinalizeContext = FinalizeContext
  { finalizeContextScope :: ElaborateScope,
    finalizeContextRuntimeBindings :: PreparedExternalBindings,
    finalizeContextRuntimeTypeEnv :: Map String ElabType
  }

data ModuleFinalizeContext = ModuleFinalizeContext
  { moduleFinalizeContextBase :: FinalizeContext,
    moduleFinalizeContextBindingReads :: Map ModuleBindingReadKey ModuleBindingReadContext
  }

data ModuleBindingReadKey
  = ModuleBindingReadLocal UniqueIdentity
  | ModuleBindingReadEnv UniqueIdentity
  | ModuleBindingReadTopLevel SymbolIdentity
  | ModuleBindingReadConstructor SymbolIdentity
  | ModuleBindingReadMethod SymbolIdentity
  | ModuleBindingReadPrimitive SymbolIdentity
  | ModuleBindingReadDeferred UniqueIdentity
  deriving (Eq, Ord)

data ModuleBindingReadContext = ModuleBindingReadContext
  { moduleBindingReadLowered :: LoweredBinding,
    moduleBindingReadResolvedFreeVars :: Either ProgramError (),
    moduleBindingReadExternalBindings :: Either ProgramError PreparedExternalBindings,
    moduleBindingReadNormalizedExpr :: Either ProgramError NormSurfaceExpr,
    moduleBindingReadCheckContext :: BindingCheckReadContext
  }

data DeferredExternalBindingIndex = DeferredExternalBindingIndex
  { deferredExternalBindingRefByName :: Map String DeferredRef,
    deferredExternalBindingByRef :: Map DeferredRef DeferredProgramObligation
  }

data RuntimeExternalBindingIndex = RuntimeExternalBindingIndex
  { runtimeExternalBindingKeyByName :: Map String ModuleBindingReadKey,
    runtimeExternalBindingByKey :: Map ModuleBindingReadKey X.ResolvedVar
  }

data BindingCheckReadContext = BindingCheckReadContext
  { bindingCheckExpectedType :: Either ProgramError ElabType,
    bindingCheckExpectedTypeForCompare :: Either ProgramError ElabType,
    bindingCheckRecoveredExpectedSourceType :: SrcType
  }

type ProgramStage a = ExceptT ProgramError IO a

timeFinalizeEither ::
  TimingConfig ->
  String ->
  IO (Either ProgramError a) ->
  ProgramStage a
timeFinalizeEither timing stageLabel action =
  ExceptT (timeProgramOperationIO timing stageLabel action)

evaluateFinalizeEither ::
  TimingConfig ->
  String ->
  Either ProgramError a ->
  ProgramStage a
evaluateFinalizeEither timing stageLabel result =
  timeFinalizeEither timing stageLabel (evaluate result)

fromProgramEither :: Either ProgramError a -> ProgramStage a
fromProgramEither result =
  ExceptT (pure result)

mkFinalizeContext :: ElaborateScope -> Either ProgramError FinalizeContext
mkFinalizeContext scope = do
  runtimeTypeEnv <- traverse (srcTypeToElabTypeInScope scope) (elaborateScopeRuntimeTypes scope)
  runtimeBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      (const ExternalBindingScheme)
      (runtimeExternalBindingIdentity scope runtimeTypeEnv)
      (elaborateScopeRuntimeTypes scope)
  pure
    FinalizeContext
      { finalizeContextScope = scope,
        finalizeContextRuntimeBindings = runtimeBindings,
        finalizeContextRuntimeTypeEnv = runtimeTypeEnv
      }

mkModuleFinalizeContext :: FinalizeContext -> [LoweredBinding] -> Either ProgramError ModuleFinalizeContext
mkModuleFinalizeContext context lowereds0 = do
  let lowereds = stampLoweredBindingsDeferredIdentities lowereds0
      schemeExternalTypes = Map.unions (map loweredBindingExternalTypes lowereds)
      schemeDeferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      schemeDeferredIndex = deferredExternalBindingIndex schemeDeferredObligations
  schemeExternalBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      (const ExternalBindingScheme)
      (deferredExternalBindingIdentity schemeDeferredIndex)
      (lowerExternalTypes (finalizeContextScope context) schemeExternalTypes)
  let keyedBindingRead lowered = do
        key <- loweredBindingReadKey lowered
        pure (key, mkModuleBindingReadContext context schemeExternalTypes schemeExternalBindings lowered)
  bindingReads <-
    traverse keyedBindingRead lowereds
  pure
    ModuleFinalizeContext
      { moduleFinalizeContextBase = context,
        moduleFinalizeContextBindingReads = Map.fromList bindingReads
      }

mkModuleBindingReadContext ::
  FinalizeContext ->
  Map String SrcType ->
  PreparedExternalBindings ->
  LoweredBinding ->
  ModuleBindingReadContext
mkModuleBindingReadContext context schemeExternalTypes schemeExternalBindings lowered =
  ModuleBindingReadContext
    { moduleBindingReadLowered = lowered,
      moduleBindingReadResolvedFreeVars = mapM_ resolveRuntimeType freeVars,
      moduleBindingReadExternalBindings =
        do
          overlayBindings <-
            prepareSurfaceExternalBindingsWithIdentity
              externalBindingModeFor
              (deferredExternalBindingIdentity deferredExternalIndex)
              (lowerExternalTypes (finalizeContextScope context) (Map.restrictKeys externalTypes overlayExternalFreeVars))
          Right (overlayBindings `unionPreparedExternalBindings` sharedSchemeBindings `unionPreparedExternalBindings` runtimeBindings),
      moduleBindingReadNormalizedExpr =
        either (Left . ProgramPipelineError . show) Right (normalizeExpr (loweredBindingSurfaceExpr lowered)),
      moduleBindingReadCheckContext =
        BindingCheckReadContext
          { bindingCheckExpectedType = expectedType,
            bindingCheckExpectedTypeForCompare = stripVacuousForalls <$> expectedType,
            bindingCheckRecoveredExpectedSourceType = recoverSourceType scope (loweredBindingExpectedType lowered)
          }
    }
  where
    scope = finalizeContextScope context
    deferredObligations = loweredBindingDeferredObligations lowered
    deferredExternalIndex = deferredExternalBindingIndex deferredObligations
    externalTypes = loweredBindingExternalTypes lowered
    freeVars = sort (Set.toList (surfaceFreeVars (loweredBindingSurfaceExpr lowered)))
    externalTypeNames = Map.keysSet externalTypes
    externalFreeVars = Set.fromList [name | name <- freeVars, name `Set.member` externalTypeNames]
    schemePreparedNames = Map.keysSet schemeExternalTypes
    sharedSchemeExternalFreeVars =
      Set.filter
        ( \name ->
            name `Set.member` schemePreparedNames
              && externalBindingModeFor name == ExternalBindingScheme
        )
        externalFreeVars
    overlayExternalFreeVars = externalFreeVars `Set.difference` sharedSchemeExternalFreeVars
    runtimeFreeVars = Set.fromList [name | name <- freeVars, name `Set.notMember` externalTypeNames]
    sharedSchemeBindings = restrictPreparedExternalBindings sharedSchemeExternalFreeVars schemeExternalBindings
    runtimeBindings = restrictPreparedExternalBindings runtimeFreeVars (finalizeContextRuntimeBindings context)
    runtimeTypes = externalTypes `Map.union` elaborateScopeRuntimeTypes scope
    expectedType = srcTypeToElabTypeInScope scope (loweredBindingExpectedType lowered)

    resolveRuntimeType name =
      case Map.lookup name runtimeTypes of
        Just _ -> Right ()
        Nothing -> Left (ProgramUnknownValue name)

    externalBindingModeFor =
      externalBindingModeForObligations deferredExternalIndex externalTypes

finalizeBindingAllowOpaque :: ElaborateScope -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingAllowOpaque scope lowered = do
  context <- mkFinalizeContext scope
  finalizeBindingAllowOpaqueWithContext context lowered

finalizeBindingAllowOpaqueWithContext :: FinalizeContext -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingAllowOpaqueWithContext context lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) = do
      placeholderTy <- srcTypeToElabTypeInScope scope (loweredBindingExpectedType lowered)
      case finalizeBindingWithContext context lowered of
        Right checked
          | Map.null (loweredBindingDeferredObligations lowered) ->
              -- Successful elaboration can still satisfy an opaque forall by
              -- instantiating the expected type. Re-check no-obligation
              -- surfaces before accepting the elaborated result.
              -- Use the source-level type to avoid structural Mu mismatches
              -- in the backend, but keep the real elaborated term.
              case validateOpaqueBindingSurface scope lowered of
                Right () -> Right (checked { checkedBindingType = placeholderTy })
                Left validationErr -> Left validationErr
          | otherwise -> Right (checked { checkedBindingType = placeholderTy })
        Left err ->
          case validateOpaqueBindingSurface scope lowered of
            Right () ->
              finalizeOpaqueUncheckedBindingWithContext context lowered placeholderTy
            Left _ -> Left err
  | otherwise = finalizeBindingWithContext context lowered
  where
    scope = finalizeContextScope context

finalizeOpaqueUncheckedBindingWithContext :: FinalizeContext -> LoweredBinding -> ElabType -> Either ProgramError CheckedBinding
finalizeOpaqueUncheckedBindingWithContext context lowered0 placeholderTy = do
  let lowered = stampLoweredBindingDeferredIdentities lowered0
  PipelineElabDetailedResult {pedTerm = term0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipelineWithContext
      context
      True
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypes lowered)
      (loweredBindingSurfaceExpr lowered)
  term <- finalizeOpaqueDeferredConstructors context (loweredBindingDeferredObligations lowered) tcEnv term0
  let resolvedTerm = annotateResolvedTermVars context lowered term
      resolvedDeferredObligations =
        annotateDeferredEvidenceResolvedVars resolvedTerm (loweredBindingDeferredObligations lowered)
  Right
    CheckedBinding
      { checkedBindingResolvedVar = resolvedVarFromLoweredBinding lowered placeholderTy,
        checkedBindingSourceType = loweredBindingSourceType lowered,
        checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
        checkedBindingDeferredObligations = resolvedDeferredObligations,
        checkedBindingTerm = resolvedTerm,
        checkedBindingType = placeholderTy,
        checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
      }
finalizeOpaqueDeferredConstructors ::
  FinalizeContext ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  Either ProgramError XmlfTerm
finalizeOpaqueDeferredConstructors context deferredObligations tcEnv term
  | Map.null deferredObligations = Right term
  | otherwise = do
      let rewriteEnv = extendTypeCheckEnvWithRuntimeContext context tcEnv
      let constructorObligations = Map.mapMaybe onlyConstructor deferredObligations
      resolveDeferredConstructors scope rewriteEnv constructorObligations term
  where
    scope = finalizeContextScope context

    onlyConstructor = \case
      DeferredConstructor deferred -> Just deferred
      _ -> Nothing

validateOpaqueBindingSurface :: ElaborateScope -> LoweredBinding -> Either ProgramError ()
validateOpaqueBindingSurface scope lowered
  | any (not . opaqueSurfaceObligationSupported) (Map.elems (loweredBindingDeferredObligations lowered)) =
      Left (ProgramPipelineError "opaque validation does not support deferred obligations")
  | otherwise =
      case inferOpaqueSurfaceType scope rigidVars runtimeTypes Map.empty (loweredBindingSurfaceExpr lowered) of
        Right actualTy
          | opaqueSourceCompatibleWithRigid rigidVars scope actualTy (loweredBindingExpectedType lowered) ->
              validateOpaqueBindingRawSurface scope rigidVars runtimeTypes lowered
          | otherwise -> Left (ProgramTypeMismatch actualTy (loweredBindingExpectedType lowered))
        Left err -> Left err
  where
    rigidVars = sourceForallBinders (loweredBindingExpectedType lowered)
    runtimeTypes =
      Map.withoutKeys (loweredBindingExternalTypes lowered) Builtins.builtinOpaqueValueNames
        `Map.union` elaborateScopeRuntimeTypes scope

validateOpaqueBindingRawSurface :: ElaborateScope -> Set String -> Map String SrcType -> LoweredBinding -> Either ProgramError ()
validateOpaqueBindingRawSurface scope rigidVars runtimeTypes lowered =
  case inferOpaqueSurfaceTypeIgnoringAscriptions scope rigidVars runtimeTypes Map.empty (loweredBindingSurfaceExpr lowered) of
    Right actualTy
      | opaqueSourceCompatibleWithRigid rigidVars scope actualTy (loweredBindingExpectedType lowered) -> Right ()
      | otherwise -> Left (ProgramTypeMismatch actualTy (loweredBindingExpectedType lowered))
    Left err -> Left err

-- Opaque placeholders discard the checked term, so constructor rewrites are
-- harmless after source-level retyping. Method and case obligations still carry
-- evidence or inspection behavior and must not be skipped here.
opaqueSurfaceObligationSupported :: DeferredProgramObligation -> Bool
opaqueSurfaceObligationSupported obligation =
  case obligation of
    DeferredConstructor {} -> True
    _ -> False

sourceForallBinders :: SrcType -> Set String
sourceForallBinders ty =
  case ty of
    STForall name _ body -> Set.insert name (sourceForallBinders body)
    _ -> Set.empty

inferOpaqueSurfaceType :: ElaborateScope -> Set String -> Map String SrcType -> Map String SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceType = inferOpaqueSurfaceTypeWithAscriptions True

inferOpaqueSurfaceTypeIgnoringAscriptions :: ElaborateScope -> Set String -> Map String SrcType -> Map String SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceTypeIgnoringAscriptions = inferOpaqueSurfaceTypeWithAscriptions False

inferOpaqueSurfaceTypeWithAscriptions :: Bool -> ElaborateScope -> Set String -> Map String SrcType -> Map String SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes localTypes expr =
  case expr of
    EVar name ->
      case Map.lookup name localTypes <|> Map.lookup name runtimeTypes of
        Just ty -> Right ty
        Nothing -> Left (ProgramUnknownValue name)
    ELit lit -> Right (literalSourceType lit)
    ELamAnn name ty body ->
      STArrow ty <$> inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes (Map.insert name ty localTypes) body
    ELam {} ->
      Left (ProgramPipelineError "opaque validation needs lambda annotations")
    EApp fun arg -> do
      funTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes localTypes fun
      argTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes localTypes arg
      applyOpaqueFunctionType scope funTy argTy
    ELet name rhs body -> do
      rhsTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes localTypes rhs
      inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes (Map.insert name rhsTy localTypes) body
    EAnn inner annTy -> do
      actualTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypes localTypes inner
      let exact =
            alphaEqSrcType actualTy annTy
              || alphaEqSrcType (lowerType scope actualTy) (lowerType scope annTy)
      if exact
        then Right (if keepAscriptions then annTy else actualTy)
        else
          if opaqueSourceCompatibleWithRigid rigidVars scope actualTy annTy
            then Right actualTy
            else Left (ProgramTypeMismatch actualTy annTy)

applyOpaqueFunctionType :: ElaborateScope -> SrcType -> SrcType -> Either ProgramError SrcType
applyOpaqueFunctionType scope funTy argTy =
  case snd (splitForalls funTy) of
    STArrow paramTy resultTy ->
      case matchTypesInScope scope Map.empty paramTy argTy <|> matchTypesInScope scope Map.empty (lowerType scope paramTy) (lowerType scope argTy) of
        Just subst -> Right (Map.foldrWithKey substituteTypeVar resultTy subst)
        Nothing
          | opaqueSourceCompatible scope argTy paramTy -> Right resultTy
          | otherwise -> Left (ProgramTypeMismatch argTy paramTy)
    other -> Left (ProgramExpectedFunction other)

opaqueSourceCompatible :: ElaborateScope -> SrcType -> SrcType -> Bool
opaqueSourceCompatible = opaqueSourceCompatibleWithRigid Set.empty

opaqueSourceCompatibleWithRigid :: Set String -> ElaborateScope -> SrcType -> SrcType -> Bool
opaqueSourceCompatibleWithRigid rigidVars scope actualTy expectedTy =
  alphaEqSrcType actualTy expectedTy
    || alphaEqSrcType (lowerType scope actualTy) (lowerType scope expectedTy)
    || sourceTypeMatchesWithRigid rigidVars scope expectedTy actualTy
    || sourceForallMatchesWithRigidForalls expectedTy actualTy

sourceTypeMatchesWithRigid :: Set String -> ElaborateScope -> SrcType -> SrcType -> Bool
sourceTypeMatchesWithRigid rigidVars scope expectedTy actualTy =
  case matchTypesInScope scope Map.empty expectedTy actualTy <|> matchTypesInScope scope Map.empty (lowerType scope expectedTy) (lowerType scope actualTy) of
    Just subst -> all rigidSubstitutionAllowed (Map.toList subst)
    Nothing -> False
  where
    rigidSubstitutionAllowed (name, ty) =
      name `Set.notMember` rigidVars || ty == STVar name

literalSourceType :: Lit -> SrcType
literalSourceType lit =
  case lit of
    LInt _ -> STBase "Int"
    LBool _ -> STBase "Bool"
    LChar _ -> STBase "Char"
    LString _ -> STBase "String"

finalizeBinding :: ElaborateScope -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBinding scope lowered = do
  context <- mkFinalizeContext scope
  finalizeBindingWithContext context lowered

finalizeBindingWithContext :: FinalizeContext -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingWithContext context lowered0 = do
  let lowered = stampLoweredBindingDeferredIdentities lowered0
  metadataBinding <- finalizeConstructorBindingFromMetadata context lowered
  case metadataBinding of
    Just checked -> Right checked
    Nothing -> finalizeBindingWithSurfacePipeline context lowered

finalizeBindingWithSurfacePipeline :: FinalizeContext -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingWithSurfacePipeline context lowered0 = do
  let lowered = stampLoweredBindingDeferredIdentities lowered0
  PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipelineWithContext
      context
      (constructorBindingNeedsUnchecked scope lowered)
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypes lowered)
      (loweredBindingSurfaceExpr lowered)
  (term, actualTy) <-
    finalizeDeferredObligationsForBinding context lowered (loweredBindingDeferredObligations lowered) tcEnv term0 actualTy0 (loweredBindingExpectedType lowered)
  finalizeCheckedBindingFromTerm context lowered term actualTy
  where
    scope = finalizeContextScope context

finalizeBindingAllowOpaqueWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  LoweredBinding ->
  IO (Either ProgramError CheckedBinding)
finalizeBindingAllowOpaqueWithContextWithTiming timing label context lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) =
      timeProgramOperationIO timing (label ++ ".opaque_fallback") $
        evaluate (finalizeBindingAllowOpaqueWithContext context lowered)
  | otherwise =
      finalizeBindingWithContextWithTiming timing label context False lowered

finalizeBindingAllowOpaqueWithModuleContext :: ModuleFinalizeContext -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingAllowOpaqueWithModuleContext moduleContext lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) =
      finalizeBindingAllowOpaqueWithContext baseContext lowered
  | otherwise =
      finalizeBindingWithModuleContext moduleContext lowered
  where
    baseContext = moduleFinalizeContextBase moduleContext

finalizeBindingAllowOpaqueWithModuleContextWithTiming ::
  TimingConfig ->
  String ->
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError CheckedBinding)
finalizeBindingAllowOpaqueWithModuleContextWithTiming timing label moduleContext preferUnchecked lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) =
      finalizeBindingAllowOpaqueWithContextWithTiming timing label baseContext lowered
  | otherwise = do
      finalizeBindingWithModuleContextWithTiming timing label moduleContext preferUnchecked lowered
  where
    baseContext = moduleFinalizeContextBase moduleContext

finalizeBindingWithModuleContext :: ModuleFinalizeContext -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingWithModuleContext moduleContext lowered0 = do
  let mbReadContext = lookupModuleBindingReadContext moduleContext lowered0
      lowered =
        case mbReadContext of
          Right readContext -> moduleBindingReadLowered readContext
          Left _ -> stampLoweredBindingDeferredIdentities lowered0
  metadataBinding <- finalizeConstructorBindingFromMetadata context lowered
  case metadataBinding of
    Just checked -> Right checked
    Nothing -> do
      readContext <- mbReadContext
      let stampedLowered = moduleBindingReadLowered readContext
      PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
        runLoweredSurfacePipelineWithModuleContext
          moduleContext
          (constructorBindingNeedsUnchecked scope stampedLowered)
          stampedLowered
      (term, actualTy) <-
        finalizeDeferredObligationsForBinding context stampedLowered (loweredBindingDeferredObligations stampedLowered) tcEnv term0 actualTy0 (loweredBindingExpectedType stampedLowered)
      finalizeCheckedBindingFromTermWithReadContext context (Just (moduleBindingReadCheckContext readContext)) stampedLowered term actualTy
  where
    context = moduleFinalizeContextBase moduleContext
    scope = finalizeContextScope context

finalizeBindingWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError CheckedBinding)
finalizeBindingWithContextWithTiming timing label context forceUnchecked lowered0 = do
  let lowered = stampLoweredBindingDeferredIdentities lowered0
  metadataResult <-
    timeProgramOperationIO timing (label ++ ".constructor_metadata") $
      evaluate (finalizeConstructorBindingFromMetadata context lowered)
  case metadataResult of
    Right (Just checked) -> pure (Right checked)
    Right Nothing -> do
      pipelineResult <-
        timeProgramOperationIO timing (label ++ ".pipeline") $
          runSurfacePipelineWithContextWithTiming
            timing
            (label ++ ".pipeline")
            context
            (forceUnchecked || constructorBindingNeedsUnchecked scope lowered)
            (loweredBindingDeferredObligations lowered)
            (loweredBindingExternalTypes lowered)
            (loweredBindingSurfaceExpr lowered)
      finalizePipelineBindingResult timing label context lowered pipelineResult
    Left err -> pure (Left err)
  where
    scope = finalizeContextScope context

finalizeBindingWithModuleContextWithTiming ::
  TimingConfig ->
  String ->
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError CheckedBinding)
finalizeBindingWithModuleContextWithTiming timing label moduleContext forceUnchecked lowered0 = do
  let mbReadContext = lookupModuleBindingReadContext moduleContext lowered0
      lowered =
        case mbReadContext of
          Right readContext -> moduleBindingReadLowered readContext
          Left _ -> lowered0
  metadataResult <-
    timeProgramOperationIO timing (label ++ ".constructor_metadata") $
      evaluate (finalizeConstructorBindingFromMetadata context lowered)
  case metadataResult of
    Right (Just checked) -> pure (Right checked)
    Right Nothing -> do
      pipelineResult <-
        timeProgramOperationIO timing (label ++ ".pipeline") $
          runLoweredSurfacePipelineWithModuleContextWithTiming
            timing
            (label ++ ".pipeline")
            moduleContext
            (forceUnchecked || constructorBindingNeedsUnchecked scope lowered)
            lowered
      let mbCheckContext =
            case mbReadContext of
              Right readContext -> Just (moduleBindingReadCheckContext readContext)
              Left _ -> Nothing
      finalizePipelineBindingResultWithReadContext timing label context mbCheckContext lowered pipelineResult
    Left err -> pure (Left err)
  where
    context = moduleFinalizeContextBase moduleContext
    scope = finalizeContextScope context

finalizeBindingLayerAllowOpaqueWithModuleContext ::
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError [CheckedBinding])
finalizeBindingLayerAllowOpaqueWithModuleContext _ [] =
  pure (Right [])
finalizeBindingLayerAllowOpaqueWithModuleContext moduleContext lowereds
  | any (not . moduleLayerPipelineEligible) lowereds =
      finalizeLayerIndividually defaultTimingConfig "module_layer" moduleContext lowereds
  | otherwise =
      case traverse (lookupModuleBindingReadContext moduleContext) lowereds of
        Left _ ->
          finalizeLayerIndividually defaultTimingConfig "module_layer" moduleContext lowereds
        Right readContexts ->
          runExceptT $ do
            (extEnv, rootPrepared, namedExprs) <-
              fromProgramEither (prepareModuleLayerPipelineInputs lowereds readContexts)
            pipelineResult <-
              liftIO $
                runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming
                  defaultTimingConfig
                  "module_layer.elab_pipeline"
                  Set.empty
                  extEnv
                  rootPrepared
                  namedExprs
            case pipelineResult of
              Left _ ->
                ExceptT $
                  finalizeLayerIndividually defaultTimingConfig "module_layer.fallback_pipeline" moduleContext lowereds
              Right results ->
                ExceptT $
                  finalizeLayerPipelineResults
                    defaultTimingConfig
                    "module_layer"
                    (moduleFinalizeContextBase moduleContext)
                    lowereds
                    readContexts
                    results

finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming ::
  TimingConfig ->
  String ->
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError [CheckedBinding])
finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming _ _ _ [] =
  pure (Right [])
finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming timing label moduleContext lowereds
  | any (not . moduleLayerPipelineEligible) lowereds =
      finalizeLayerIndividually timing (label ++ ".fallback_unsupported") moduleContext lowereds
  | otherwise =
      case traverse (lookupModuleBindingReadContext moduleContext) lowereds of
        Left _ ->
          finalizeLayerIndividually timing (label ++ ".fallback_missing_context") moduleContext lowereds
        Right readContexts ->
          runExceptT $ do
            (extEnv, rootPrepared, namedExprs) <-
              ExceptT $
                prepareModuleLayerPipelineInputsWithTiming timing label lowereds readContexts
            let innerTiming =
                  if timingProgramDefDetails timing
                    then timing
                    else defaultTimingConfig
            pipelineResult <-
              liftIO $
                timeProgramOperationIO timing (label ++ ".pipeline") $
                  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming
                    innerTiming
                    (label ++ ".pipeline.elab_pipeline")
                    Set.empty
                    extEnv
                    rootPrepared
                    namedExprs
            case pipelineResult of
              Left _ ->
                ExceptT $
                  finalizeLayerIndividually timing (label ++ ".fallback_pipeline") moduleContext lowereds
              Right results ->
                ExceptT $
                  finalizeLayerPipelineResults timing label context lowereds readContexts results
  where
    context = moduleFinalizeContextBase moduleContext

finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming ::
  TimingConfig ->
  String ->
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError [CheckedBinding])
finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming _ _ _ [] =
  pure (Right [])
finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming timing label moduleContext lowereds
  | any (not . moduleDeferredLayerPipelineEligible) lowereds =
      finalizeLayerIndividually timing (label ++ ".fallback_unsupported") moduleContext lowereds
  | otherwise =
      case traverse (lookupModuleBindingReadContext moduleContext) lowereds of
        Left _ ->
          finalizeLayerIndividually timing (label ++ ".fallback_missing_context") moduleContext lowereds
        Right readContexts ->
          runExceptT $ do
            (extEnv, rootPrepared, namedExprs) <-
              ExceptT $
                prepareModuleLayerPipelineInputsWithTiming timing label lowereds readContexts
            let innerTiming =
                  if timingProgramDefDetails timing
                    then timing
                    else defaultTimingConfig
            pipelineResult <-
              liftIO $
                timeProgramOperationIO timing (label ++ ".pipeline") $
                  runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming
                    innerTiming
                    (label ++ ".pipeline.elab_pipeline")
                    Set.empty
                    extEnv
                    rootPrepared
                    namedExprs
            case pipelineResult of
              Left _ ->
                ExceptT $
                  finalizeLayerIndividually timing (label ++ ".fallback_pipeline") moduleContext lowereds
              Right results ->
                ExceptT $
                  finalizeLayerPipelineResults timing label context lowereds readContexts results
  where
    context = moduleFinalizeContextBase moduleContext

moduleLayerPipelineEligible :: LoweredBinding -> Bool
moduleLayerPipelineEligible lowered =
  not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))
    && Map.null (loweredBindingDeferredObligations lowered)

moduleDeferredLayerPipelineEligible :: LoweredBinding -> Bool
moduleDeferredLayerPipelineEligible lowered =
  not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))
    && not (Map.null (loweredBindingDeferredObligations lowered))

combinePreparedExternalBindings :: [PreparedExternalBindings] -> Either ProgramError PreparedExternalBindings
combinePreparedExternalBindings bindings =
  case bindings of
    [] -> Left (ProgramPipelineError "empty module binding layer")
    firstBinding : rest ->
      Right (foldl' unionPreparedExternalBindings firstBinding rest)

prepareModuleLayerPipelineInputs ::
  [LoweredBinding] ->
  [ModuleBindingReadContext] ->
  Either ProgramError (PreparedExternalBindings, Map ModuleBindingReadKey PreparedExternalBindings, [(ModuleBindingReadKey, String, NormSurfaceExpr)])
prepareModuleLayerPipelineInputs lowereds readContexts = do
  mapM_ moduleBindingReadResolvedFreeVars readContexts
  extEnvs <- traverse moduleBindingReadExternalBindings readContexts
  extEnv <- combinePreparedExternalBindings extEnvs
  normExprs <- traverse moduleBindingReadNormalizedExpr readContexts
  keyedExprs <- moduleLayerKeyedExprs lowereds normExprs
  let rootPrepared =
        Map.fromList [(key, rootExtEnv) | ((key, _, _), rootExtEnv) <- zip keyedExprs extEnvs]
  pure (extEnv, rootPrepared, keyedExprs)

prepareModuleLayerPipelineInputsWithTiming ::
  TimingConfig ->
  String ->
  [LoweredBinding] ->
  [ModuleBindingReadContext] ->
  IO (Either ProgramError (PreparedExternalBindings, Map ModuleBindingReadKey PreparedExternalBindings, [(ModuleBindingReadKey, String, NormSurfaceExpr)]))
prepareModuleLayerPipelineInputsWithTiming timing label lowereds readContexts =
  runExceptT $ do
    (extEnv, extEnvs) <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $ do
        mapM_ moduleBindingReadResolvedFreeVars readContexts
        extEnvs <- traverse moduleBindingReadExternalBindings readContexts
        extEnv <- combinePreparedExternalBindings extEnvs
        pure (extEnv, extEnvs)
    normExprs <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        traverse moduleBindingReadNormalizedExpr readContexts
    keyedExprs <- fromProgramEither (moduleLayerKeyedExprs lowereds normExprs)
    let rootPrepared =
          Map.fromList [(key, rootExtEnv) | ((key, _, _), rootExtEnv) <- zip keyedExprs extEnvs]
    pure (extEnv, rootPrepared, keyedExprs)

moduleLayerKeyedExprs :: [LoweredBinding] -> [NormSurfaceExpr] -> Either ProgramError [(ModuleBindingReadKey, String, NormSurfaceExpr)]
moduleLayerKeyedExprs lowereds normExprs = do
  rootKeys <- traverse loweredBindingReadKey lowereds
  if Set.size (Set.fromList rootKeys) == length rootKeys
    then Right (zipWith3 (\key name expr -> (key, name, expr)) rootKeys (map loweredBindingName lowereds) normExprs)
    else Left (ProgramPipelineError "module layer returned duplicate binding identities")

finalizeLayerIndividually ::
  TimingConfig ->
  String ->
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError [CheckedBinding])
finalizeLayerIndividually timing label moduleContext lowereds =
  runExceptT (go (1 :: Int) lowereds)
  where
    go _ [] = pure []
    go index (lowered : rest) = do
      checked <-
        ExceptT $
          finalizeBindingAllowOpaqueWithModuleContextWithTiming
            timing
            (label ++ ".def_" ++ show index)
            moduleContext
            False
            lowered
      restResult <- go (index + 1) rest
      pure (checked : restResult)

finalizeLayerPipelineResults ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  [LoweredBinding] ->
  [ModuleBindingReadContext] ->
  Map ModuleBindingReadKey PipelineElabDetailedResult ->
  IO (Either ProgramError [CheckedBinding])
finalizeLayerPipelineResults timing label context lowereds readContexts results =
  runExceptT $
    go [] (1 :: Int) lowereds readContexts
  where
    go acc _ [] [] = pure (reverse acc)
    go acc index (lowered : rest) (readContext : readRest) = do
      key <- fromProgramEither (loweredBindingReadKey lowered)
      case Map.lookup key results of
        Nothing ->
          fromProgramEither (Left (ProgramPipelineError ("module layer missing result for binding `" ++ loweredBindingName lowered ++ "`")))
        Just pipelineResult -> do
          let stampedLowered = moduleBindingReadLowered readContext
          checked <-
            ExceptT $
              finalizePipelineBindingResultWithReadContext
                timing
                (label ++ ".binding_" ++ show index)
                context
                (Just (moduleBindingReadCheckContext readContext))
                stampedLowered
                (Right pipelineResult)
          checked `seq` go (checked : acc) (index + 1) rest readRest
    go _ _ _ _ =
      fromProgramEither (Left (ProgramPipelineError "module layer result/read-context length mismatch"))

finalizePipelineBindingResult ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  LoweredBinding ->
  Either ProgramError PipelineElabDetailedResult ->
  IO (Either ProgramError CheckedBinding)
finalizePipelineBindingResult timing label context lowered pipelineResult =
  finalizePipelineBindingResultWithReadContext timing label context Nothing lowered pipelineResult

finalizePipelineBindingResultWithReadContext ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  Maybe BindingCheckReadContext ->
  LoweredBinding ->
  Either ProgramError PipelineElabDetailedResult ->
  IO (Either ProgramError CheckedBinding)
finalizePipelineBindingResultWithReadContext timing label context mbCheckContext lowered pipelineResult =
  runExceptT $ do
    PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
      fromProgramEither pipelineResult
    (term, actualTy) <-
      evaluateFinalizeEither timing (label ++ ".deferred_obligations") $
        finalizeDeferredObligationsForBinding
          context
          lowered
          (loweredBindingDeferredObligations lowered)
          tcEnv
          term0
          actualTy0
          (loweredBindingExpectedType lowered)
    evaluateFinalizeEither timing (label ++ ".binding_check") $
      finalizeCheckedBindingFromTermWithReadContext context mbCheckContext lowered term actualTy

finalizeConstructorBindingFromMetadata :: FinalizeContext -> LoweredBinding -> Either ProgramError (Maybe CheckedBinding)
finalizeConstructorBindingFromMetadata context lowered
  | not (loweredBindingIsConstructor lowered) = Right Nothing
  | otherwise =
      case metadataConstructorTerm context lowered of
        Right (term, expectedTy) ->
          Just <$> finalizeCheckedBindingFromTerm context lowered term expectedTy
        _ -> Right Nothing

metadataConstructorTerm :: FinalizeContext -> LoweredBinding -> Either ProgramError (XmlfTerm, ElabType)
metadataConstructorTerm context lowered = do
  (dataInfo, ctorInfo) <-
    case lookupConstructorBindingRuntime scope lowered of
      Just found -> Right found
      Nothing -> Left (ProgramPipelineError ("missing constructor metadata for `" ++ loweredBindingName lowered ++ "`"))
  if constructorMetadataFastPathSupported scope dataInfo ctorInfo
    then pure ()
    else Left (ProgramPipelineError ("constructor metadata fast path unsupported for `" ++ loweredBindingName lowered ++ "`"))
  expectedTy <- srcTypeToElabTypeInScope scope (loweredBindingExpectedType lowered)
  term0 <- inlineConstructorHead scope ctorInfo emptyTypeBinderSubst
  let term = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (schemeFromType expectedTy) term0
  Right (term, expectedTy)
  where
    scope = finalizeContextScope context

constructorMetadataFastPathSupported :: ElaborateScope -> DataInfo -> ConstructorInfo -> Bool
constructorMetadataFastPathSupported scope dataInfo ctorInfo =
  null (ctorForalls ctorInfo)
    && (null (dataParams dataInfo) || not (null (ctorArgs ctorInfo)))
    && dataInfoSymbol dataInfo == ctorOwningTypeIdentity ctorInfo
    && constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo

loweredBindingIsConstructor :: LoweredBinding -> Bool
loweredBindingIsConstructor lowered =
  case loweredBindingConstructorRef lowered of
    Just _ -> True
    Nothing -> False

finalizeBindingsAllowOpaqueWithContext :: FinalizeContext -> [LoweredBinding] -> Either ProgramError [CheckedBinding]
finalizeBindingsAllowOpaqueWithContext context =
  go
  where
    go [] = Right []
    go lowereds@(lowered : rest)
      | batchableLoweredBinding lowered =
          let (batch, rest') = span batchableLoweredBinding lowereds
           in if length batch <= 1
                then (:) <$> finalizeBindingAllowOpaqueWithContext context lowered <*> go rest
                else (++) <$> finalizeBindingGroupWithContext context batch <*> go rest'
      | otherwise =
          (:) <$> finalizeBindingAllowOpaqueWithContext context lowered <*> go rest

    batchableLoweredBinding lowered =
      not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))
        && not (loweredBindingIsConstructor lowered)

finalizeBindingsAllowOpaqueWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError [CheckedBinding])
finalizeBindingsAllowOpaqueWithContextWithTiming timing label context lowereds =
  timeProgramOperationIO timing label (runExceptT (go (1 :: Int) lowereds))
  where
    go _ [] = pure []
    go groupIndex bindings@(lowered : rest)
      | batchableLoweredBinding lowered = do
          let (batch, rest') = span batchableLoweredBinding bindings
          batchResult <-
            if length batch <= 1
              then ExceptT $ evaluate ((: []) <$> finalizeBindingAllowOpaqueWithContext context lowered)
              else ExceptT $ finalizeBindingGroupWithContextWithTiming timing (label ++ ".group_" ++ show groupIndex) context batch
          restResult <- go (groupIndex + 1) rest'
          pure (batchResult ++ restResult)
      | otherwise = do
          checked <- ExceptT $ evaluate (finalizeBindingAllowOpaqueWithContext context lowered)
          restResult <- go groupIndex rest
          pure (checked : restResult)

    batchableLoweredBinding lowered =
      not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))
        && not (loweredBindingIsConstructor lowered)

finalizeBindingGroupWithContext :: FinalizeContext -> [LoweredBinding] -> Either ProgramError [CheckedBinding]
finalizeBindingGroupWithContext _ [] = Right []
finalizeBindingGroupWithContext context lowereds0 = do
  let lowereds =
        stampLoweredBindingsDeferredIdentities $
          zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
      deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      externalTypes = Map.unions (map loweredBindingExternalTypes lowereds)
      groupExpr = groupedBindingExpr lowereds
  PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipelineWithContext context False deferredObligations externalTypes groupExpr
  (term, _actualTy) <-
    finalizeDeferredObligationsForGroup context lowereds deferredObligations tcEnv term0 actualTy0 STBottom
  case extractGroupedBindings lowereds term of
    Left _ ->
      traverse (finalizeBindingAllowOpaqueWithContext context) lowereds0
    Right extracted ->
      zipWithM
        (\lowered (scheme, rhs) ->
           finalizeCheckedBindingFromTerm context lowered rhs (schemeToType scheme))
        lowereds
        extracted

finalizeBindingGroupWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError [CheckedBinding])
finalizeBindingGroupWithContextWithTiming _ _ _ [] = pure (Right [])
finalizeBindingGroupWithContextWithTiming timing label context lowereds0 =
  runExceptT $ do
    let lowereds =
          stampLoweredBindingsDeferredIdentities $
            zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
        deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
        externalTypes = Map.unions (map loweredBindingExternalTypes lowereds)
        groupExpr = groupedBindingExpr lowereds
    PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
      timeFinalizeEither timing (label ++ ".pipeline") $
        runSurfacePipelineWithContextWithTiming timing (label ++ ".pipeline") context False deferredObligations externalTypes groupExpr
    (term, _actualTy) <-
      evaluateFinalizeEither timing (label ++ ".deferred_obligations") $
        finalizeDeferredObligationsForGroup context lowereds deferredObligations tcEnv term0 actualTy0 STBottom
    extractedResult <-
      liftIO $
        timeProgramOperationIO timing (label ++ ".extract_bindings") $
          evaluate (extractGroupedBindings lowereds term)
    case extractedResult of
      Left _ ->
        timeFinalizeEither timing (label ++ ".fallback_individual") $
          evaluate (traverse (finalizeBindingAllowOpaqueWithContext context) lowereds0)
      Right extracted ->
        ExceptT (finalizeExtractedBindingsWithTiming lowereds (1 :: Int) extracted)
  where
    finalizeExtractedBindingsWithTiming lowereds index extracted =
      runExceptT (go lowereds index extracted)

    go _ _ [] = pure []
    go lowereds index ((scheme, rhs) : rest) = do
      original <-
        case drop (index - 1) lowereds of
          [] ->
            fromProgramEither (Left (ProgramPipelineError "group finalizer returned extra binding"))
          original : _ -> pure original
      checked <-
        evaluateFinalizeEither timing (label ++ ".binding_" ++ show index ++ "_check") $
          finalizeCheckedBindingFromTerm context original rhs (schemeToType scheme)
      restResult <- go lowereds (index + 1) rest
      pure (checked : restResult)

groupedBindingExpr :: [LoweredBinding] -> SurfaceExpr
groupedBindingExpr =
  foldr
    ( \lowered body ->
        ELet
          (loweredBindingName lowered)
          (EAnn (loweredBindingSurfaceExpr lowered) (loweredBindingExpectedType lowered))
          body
    )
    (ELit (LBool True))

extractGroupedBindings :: [LoweredBinding] -> XmlfTerm -> Either ProgramError [(X.ElabScheme, XmlfTerm)]
extractGroupedBindings expectedLowereds term = do
  expected <- traverse expectedBindingKey expectedLowereds
  let expectedKeys = Set.fromList (map fst expected)
  if Set.size expectedKeys /= length expected
    then Left (ProgramPipelineError "group finalizer expected duplicate binding identities")
    else do
      bindingsByIdentity <- collect expected expectedKeys (length expected) Map.empty term
      traverse (lookupExpected bindingsByIdentity) expected
  where
    expectedBindingKey lowered = do
      key <- loweredBindingReadKey lowered
      Right (key, loweredBindingName lowered)

    collect _ _ 0 acc _ = Right acc
    collect expected expectedKeys remaining acc term0 =
      case term0 of
        X.ELet resolved scheme rhs body
          | Just key <- idDetailsReadKeyMaybe (X.resolvedVarDetails resolved),
            key `Set.member` expectedKeys ->
              if Map.member key acc
                then Left (ProgramPipelineError ("group finalizer returned duplicate binding `" ++ name ++ "`"))
                else collect expected expectedKeys (remaining - 1) (Map.insert key (scheme, rhs) acc) body
          | Just _ <- idDetailsReadKeyMaybe (X.resolvedVarDetails resolved) ->
              Left (ProgramPipelineError ("group finalizer returned extra binding `" ++ name ++ "`"))
          | otherwise ->
              Left (ProgramPipelineError ("group finalizer returned binding `" ++ name ++ "` without resolved identity"))
          where
            name = X.resolvedVarReferenceName resolved
        _ ->
          Left $
            ProgramPipelineError $
              "group finalizer could not find checked binding `" ++ firstMissing expected acc ++ "`"

    lookupExpected bindingsByIdentity (key, expectedName) =
      case Map.lookup key bindingsByIdentity of
        Just (scheme, rhs) -> Right (scheme, rhs)
        Nothing ->
          Left $
            ProgramPipelineError $
              "group finalizer could not find checked binding `" ++ expectedName ++ "`"

    firstMissing expected acc =
      case [name | (key, name) <- expected, key `Map.notMember` acc] of
        name : _ -> name
        [] -> "<unknown>"

renameDeferredPlaceholdersForGroup :: Int -> LoweredBinding -> LoweredBinding
renameDeferredPlaceholdersForGroup index lowered =
  let placeholderMap =
        Map.fromList
          [ (name, "$group_" ++ show index ++ "_" ++ name)
          | obligation <- Map.elems (loweredBindingDeferredObligations lowered)
          , let name = deferredRefName (deferredProgramObligationRef obligation)
          ]
      renameName name = Map.findWithDefault name name placeholderMap
   in lowered
        { loweredBindingSurfaceExpr = renameSurfaceVars renameName (loweredBindingSurfaceExpr lowered)
        , loweredBindingDeferredObligations =
            remapDeferredObligations (renameDeferredObligation renameName) (loweredBindingDeferredObligations lowered)
        , loweredBindingExternalTypes =
            Map.mapKeys renameName (loweredBindingExternalTypes lowered)
        }

renameSurfaceVars :: (String -> String) -> SurfaceExpr -> SurfaceExpr
renameSurfaceVars renameName =
  go
  where
    go expr =
      case expr of
        EVar name -> EVar (renameName name)
        ELit {} -> expr
        ELam name body -> ELam name (go body)
        EApp fun arg -> EApp (go fun) (go arg)
        ELet name rhs body -> ELet name (go rhs) (go body)
        ELamAnn name ty body -> ELamAnn name ty (go body)
        EAnn inner ty -> EAnn (go inner) ty

renameDeferredObligation :: (String -> String) -> DeferredProgramObligation -> DeferredProgramObligation
renameDeferredObligation renameName obligation =
  case obligation of
    DeferredMethod deferred ->
      DeferredMethod
        deferred
          { deferredMethodRef = renameDeferredRef (renameName (deferredMethodPlaceholder deferred)) (deferredMethodRef deferred)
          }
    DeferredConstructor deferred ->
      DeferredConstructor
        deferred
          { deferredConstructorRef = renameDeferredRef (renameName (deferredConstructorPlaceholder deferred)) (deferredConstructorRef deferred)
          }
    DeferredCase deferred ->
      DeferredCase
        deferred
          { deferredCaseRef = renameDeferredRef (renameName (deferredCasePlaceholder deferred)) (deferredCaseRef deferred)
          }

stampLoweredBindingDeferredIdentities :: LoweredBinding -> LoweredBinding
stampLoweredBindingDeferredIdentities lowered =
  case stampLoweredBindingsDeferredIdentities [lowered] of
    stamped : _ -> stamped
    [] -> lowered

stampLoweredBindingsDeferredIdentities :: [LoweredBinding] -> [LoweredBinding]
stampLoweredBindingsDeferredIdentities lowereds =
  snd (mapAccumL stampLowered (initialGenerator, Set.empty) lowereds)
  where
    initialGenerator =
      identityGeneratorAfter (concatMap generatedIdentitiesInDeferredObligations lowereds)

    stampLowered state lowered =
      let (state', obligationsList) =
            mapAccumL stampDeferredObligation state (Map.elems (loweredBindingDeferredObligations lowered))
          obligations = deferredObligationsFromList obligationsList
       in ( state',
            lowered {loweredBindingDeferredObligations = obligations}
          )

stampDeferredObligation :: (IdentityGenerator, Set UniqueIdentity) -> DeferredProgramObligation -> ((IdentityGenerator, Set UniqueIdentity), DeferredProgramObligation)
stampDeferredObligation (generator, seen) obligation =
  if identity `Set.notMember` seen
    then ((generator, Set.insert identity seen), obligation)
    else
      let (ref, generator') = freshDeferredRef (deferredRefName ref0) generator
          seen' = Set.insert (deferredRefIdentity ref) seen
       in ((generator', seen'), setDeferredProgramObligationRef ref obligation)
  where
    ref0 = deferredProgramObligationRef obligation
    identity = deferredRefIdentity ref0

setDeferredProgramObligationRef :: DeferredRef -> DeferredProgramObligation -> DeferredProgramObligation
setDeferredProgramObligationRef ref obligation =
  case obligation of
    DeferredMethod deferred -> DeferredMethod deferred {deferredMethodRef = ref}
    DeferredConstructor deferred -> DeferredConstructor deferred {deferredConstructorRef = ref}
    DeferredCase deferred -> DeferredCase deferred {deferredCaseRef = ref}

remapDeferredObligations :: (DeferredProgramObligation -> DeferredProgramObligation) -> DeferredObligations -> DeferredObligations
remapDeferredObligations f obligations =
  deferredObligationsFromList (map f (Map.elems obligations))

deferredObligationsFromList :: [DeferredProgramObligation] -> DeferredObligations
deferredObligationsFromList obligations =
  Map.fromList
    [ (deferredProgramObligationRef obligation, obligation)
    | obligation <- obligations
    ]

finalizeCheckedBindingFromTerm :: FinalizeContext -> LoweredBinding -> XmlfTerm -> ElabType -> Either ProgramError CheckedBinding
finalizeCheckedBindingFromTerm context =
  finalizeCheckedBindingFromTermWithReadContext context Nothing

finalizeCheckedBindingFromTermWithReadContext :: FinalizeContext -> Maybe BindingCheckReadContext -> LoweredBinding -> XmlfTerm -> ElabType -> Either ProgramError CheckedBinding
finalizeCheckedBindingFromTermWithReadContext context mbCheckContext lowered term actualTy = do
  let isUncheckedConstructor = constructorBindingNeedsUnchecked scope lowered
      acceptedTerm0 = repairConstructorBindingTerm scope lowered term
  (acceptedTy, acceptedTerm) <-
    if isUncheckedConstructor
      then do
        expectedTy <- bindingCheckExpectedTypeFor lowered
        Right (expectedTy, acceptedTerm0)
      else Right (stripVacuousForallsAndTypeAbs actualTy acceptedTerm0)
  let acceptedTermTyResult =
        if isUncheckedConstructor
          then Right acceptedTy
          else TypeCheck.typeCheckWithEnv (runtimeTypeCheckEnv context) acceptedTerm
  let acceptChecked = do
        checkedTy <- checkedBindingTypeForStorage lowered acceptedTy
        let acceptedTermWithResolvedVars =
              annotateResolvedTermVars context lowered acceptedTerm
            resolvedDeferredObligations =
              annotateDeferredEvidenceResolvedVars acceptedTermWithResolvedVars (loweredBindingDeferredObligations lowered)
        case unresolvedXmlfTermVarRefs acceptedTermWithResolvedVars of
          [] ->
            do
              validateDeferredObligationIdentities (loweredBindingName lowered) resolvedDeferredObligations
              Right
                CheckedBinding
                  { checkedBindingResolvedVar = resolvedVarFromLoweredBinding lowered checkedTy,
                    checkedBindingSourceType = loweredBindingSourceType lowered,
                    checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
                    checkedBindingDeferredObligations = resolvedDeferredObligations,
                    checkedBindingTerm = acceptedTermWithResolvedVars,
                    checkedBindingType = checkedTy,
                    checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
                  }
          refs ->
            Left
              ( ProgramPipelineError
                  ( "checked XmlfTerm retained unresolved variables in `"
                      ++ loweredBindingName lowered
                      ++ "`: "
                      ++ show (map deferredRefName refs)
                  )
              )
  if isUncheckedConstructor
    then acceptChecked
    else do
      case acceptedTermTyResult of
        Right checkedTy
          | meaningfulForallCount checkedTy < meaningfulForallCount acceptedTy ->
              ensureRecoveredSourceCompatible (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls checkedTy)))
        Left _
          | not (termCoversMeaningfulForalls acceptedTy acceptedTerm),
            not (directSurfaceValueCoversMeaningfulForalls acceptedTy) ->
              ensureRecoveredSourceCompatible (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls acceptedTy)))
        _ -> Right ()
      let actualTyForCompare = stripVacuousForalls actualTy
      expectedTyForCompare <- bindingCheckExpectedTypeForCompareFor lowered
      if actualTyForCompare == expectedTyForCompare
        || alphaEqType actualTyForCompare expectedTyForCompare
        || churchAwareEqType actualTyForCompare expectedTyForCompare
        then acceptChecked
        else do
          let recoveredActualSrcTy = recoverSourceType scope (elabTypeToSrcType actualTyForCompare)
          recoveredActualTy <- srcTypeToElabTypeInScope scope (lowerType scope recoveredActualSrcTy)
          let recoveredExpectedSrcTy =
                recoverSourceType scope (bindingCheckRecoveredExpectedSourceTypeFor lowered)
              sourceForallCompatible =
                alphaEqSrcType recoveredExpectedSrcTy recoveredActualSrcTy
                  || alphaEqSrcType (lowerType scope recoveredExpectedSrcTy) (lowerType scope recoveredActualSrcTy)
                  || if Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered)
                    then sourceForallMatchesWithRigidForalls recoveredExpectedSrcTy recoveredActualSrcTy
                    else sourceForallMatches recoveredExpectedSrcTy recoveredActualSrcTy
          if recoveredActualTy == expectedTyForCompare
            || alphaEqType recoveredActualTy expectedTyForCompare
            || churchAwareEqType recoveredActualTy expectedTyForCompare
            || sourceForallCompatible
            then acceptChecked
            else Left (ProgramTypeMismatch recoveredActualSrcTy recoveredExpectedSrcTy)
  where
    scope = finalizeContextScope context

    ensureRecoveredSourceCompatible actualSrc =
      let expectedSrc = recoverSourceType scope (loweredBindingExpectedType lowered)
       in if recoveredSourceTypesCompatible expectedSrc actualSrc
            then Right ()
            else Left (ProgramTypeMismatch actualSrc expectedSrc)

    recoveredSourceTypesCompatible expectedSrc actualSrc =
      alphaEqSrcType expectedSrc actualSrc
        || alphaEqSrcType (lowerType scope expectedSrc) (lowerType scope actualSrc)
        || if Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered)
          then sourceForallMatchesWithRigidForalls expectedSrc actualSrc
          else sourceForallMatches expectedSrc actualSrc

    bindingCheckExpectedTypeFor lowered0 =
      case mbCheckContext of
        Just checkContext -> bindingCheckExpectedType checkContext
        Nothing -> srcTypeToElabTypeInScope scope (loweredBindingExpectedType lowered0)

    bindingCheckExpectedTypeForCompareFor lowered0 =
      case mbCheckContext of
        Just checkContext -> bindingCheckExpectedTypeForCompare checkContext
        Nothing -> stripVacuousForalls <$> srcTypeToElabTypeInScope scope (loweredBindingExpectedType lowered0)

    checkedBindingTypeForStorage lowered0 acceptedTy0 = do
      expectedTy <- bindingCheckExpectedTypeFor lowered0
      pure $
        if acceptedTy0 == expectedTy
          || alphaEqType acceptedTy0 expectedTy
          || churchAwareEqType acceptedTy0 expectedTy
          then expectedTy
          else acceptedTy0

    bindingCheckRecoveredExpectedSourceTypeFor lowered0 =
      case mbCheckContext of
        Just checkContext -> bindingCheckRecoveredExpectedSourceType checkContext
        Nothing -> recoverSourceType scope (loweredBindingExpectedType lowered0)

    termCoversMeaningfulForalls ty checkedTerm =
      case ty of
        X.TForallRef ref _ body
          | any (X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsType body) ->
              case checkedTerm of
                X.ETyAbsRef _ _ bodyTerm -> termCoversMeaningfulForalls body bodyTerm
                _ -> False
          | otherwise -> termCoversMeaningfulForalls body checkedTerm
        _ -> True

    directSurfaceValueCoversMeaningfulForalls :: ElabType -> Bool
    directSurfaceValueCoversMeaningfulForalls ty =
      case directSurfaceValueName (loweredBindingSurfaceExpr lowered) of
        Just name ->
          case Map.lookup name runtimeSourceTypes of
            Just sourceTy ->
              let targetTy = recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty))
                  sourceTy' = lowerType scope sourceTy
                  targetTy' = lowerType scope targetTy
               in alphaEqSrcType sourceTy targetTy
                    || alphaEqSrcType sourceTy' targetTy'
                    || sourceForallMatchesWithRigidForalls targetTy sourceTy
                    || sourceForallMatchesWithRigidForalls targetTy' sourceTy'
            Nothing -> False
        Nothing -> False

    runtimeSourceTypes =
      loweredBindingExternalTypes lowered `Map.union` elaborateScopeRuntimeTypes scope

    directSurfaceValueName :: SurfaceExpr -> Maybe String
    directSurfaceValueName expr =
      case expr of
        EVar name -> Just name
        EAnn inner _ -> directSurfaceValueName inner
        _ -> Nothing

    meaningfulForallCount :: ElabType -> Int
    meaningfulForallCount ty =
      case ty of
        X.TForallRef ref _ body
          | any (X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsType body) ->
              1 + meaningfulForallCount body
          | otherwise -> meaningfulForallCount body
        _ -> 0

validateDeferredObligationIdentities :: String -> DeferredObligations -> Either ProgramError ()
validateDeferredObligationIdentities _bindingName _obligations =
  Right ()

annotateResolvedTermVars :: FinalizeContext -> LoweredBinding -> XmlfTerm -> XmlfTerm
annotateResolvedTermVars _context lowered term0 =
  annotateResolvedTermVarsWithEvidenceCounts
    Map.empty
    (loweredBindingEvidenceParamCount lowered)
    (generatedIdentitiesInTerm term0 ++ generatedIdentitiesInDeferredObligations lowered)
    term0

annotateResolvedTermVarsForGroup :: FinalizeContext -> [LoweredBinding] -> DeferredObligations -> XmlfTerm -> XmlfTerm
annotateResolvedTermVarsForGroup _context lowereds deferredObligations term0 =
  annotateResolvedTermVarsWithEvidenceCounts
    evidenceCountsByBinding
    0
    (generatedIdentitiesInTerm term0 ++ generatedIdentitiesInDeferredObligationsMap deferredObligations)
    term0
  where
    evidenceCountsByBinding =
      Map.fromList
        ( [ (key, loweredBindingEvidenceParamCount lowered)
            | (resolved, lowered) <- zip groupBindingResolvedVars lowereds,
              Just key <- [idDetailsReadKeyMaybe (X.resolvedVarDetails resolved)]
          ]
            ++ [ (key, loweredBindingEvidenceParamCount lowered)
                 | lowered <- lowereds,
                   Right key <- [loweredBindingReadKey lowered]
               ]
        )

    groupBindingResolvedVars =
      collectGroupBindingResolvedVars (length lowereds) term0

    collectGroupBindingResolvedVars 0 _ = []
    collectGroupBindingResolvedVars remaining term =
      case term of
        X.ELet resolved _ _ body ->
          resolved : collectGroupBindingResolvedVars (remaining - 1) body
        _ ->
          []

annotateResolvedTermVarsWithEvidenceCounts :: Map ModuleBindingReadKey Int -> Int -> [UniqueIdentity] -> XmlfTerm -> XmlfTerm
annotateResolvedTermVarsWithEvidenceCounts evidenceCountsByBinding initialEvidenceParamCount generatedIdentities term0 =
  let (term, _, _) = go Map.empty initialEvidenceParamCount initialGenerator term0
   in term
  where
    initialGenerator =
      identityGeneratorAfter generatedIdentities

    go identityLocals evidenceParamsLeft generator current =
      case current of
        X.EVarNode resolved
          | X.resolvedVarIsLocal resolved,
            Just scoped <- lookupLocalByIdentity resolved identityLocals ->
              (X.EVarNode (scoped {X.resolvedVarType = X.resolvedVarType resolved}), evidenceParamsLeft, generator)
        X.EVarNode {} -> (current, evidenceParamsLeft, generator)
        X.ELit {} -> (current, evidenceParamsLeft, generator)
        X.ELam resolved body ->
          let (resolved', evidenceParamsLeft', generator') = freshenLocalResolvedVar True evidenceParamsLeft generator resolved
              identityLocals' = insertLocalIdentity resolved resolved' identityLocals
              (body', evidenceParamsLeft'', generator'') = go identityLocals' evidenceParamsLeft' generator' body
           in (X.ELam resolved' body', evidenceParamsLeft'', generator'')
        X.EApp fun arg ->
          let (fun', evidenceParamsLeft', generator') = go identityLocals evidenceParamsLeft generator fun
              (arg', evidenceParamsLeft'', generator'') = go identityLocals evidenceParamsLeft' generator' arg
           in (X.EApp fun' arg', evidenceParamsLeft'', generator'')
        X.ELet resolved scheme rhs body ->
          let resolvedWithScheme = X.mapResolvedVarType (const (schemeToType scheme)) resolved
              (resolved', evidenceParamsLeft', generator') = freshenLocalResolvedVar False evidenceParamsLeft generator resolvedWithScheme
              identityLocals' = insertLocalIdentity resolved resolved' identityLocals
           in case idDetailsReadKeyMaybe (X.resolvedVarDetails resolved) >>= (`Map.lookup` evidenceCountsByBinding) of
                Just rhsEvidenceParamCount ->
                  let (rhs', _, generator'') = go identityLocals' rhsEvidenceParamCount generator' rhs
                      (body', evidenceParamsLeft'', generator''') = go identityLocals' evidenceParamsLeft' generator'' body
                   in (X.ELet resolved' scheme rhs' body', evidenceParamsLeft'', generator''')
                Nothing ->
                  let (rhs', evidenceParamsLeft'', generator'') = go identityLocals' evidenceParamsLeft' generator' rhs
                      (body', evidenceParamsLeft''', generator''') = go identityLocals' evidenceParamsLeft'' generator'' body
                   in (X.ELet resolved' scheme rhs' body', evidenceParamsLeft''', generator''')
        X.ETyAbsRef ref mb body ->
          let (body', evidenceParamsLeft', generator') = go identityLocals evidenceParamsLeft generator body
           in (X.ETyAbsRef ref mb body', evidenceParamsLeft', generator')
        X.ETyInst inner inst ->
          let (inner', evidenceParamsLeft', generator') = go identityLocals evidenceParamsLeft generator inner
           in (X.ETyInst inner' inst, evidenceParamsLeft', generator')
        X.ERoll ty body ->
          let (body', evidenceParamsLeft', generator') = go identityLocals evidenceParamsLeft generator body
           in (X.ERoll ty body', evidenceParamsLeft', generator')
        X.EUnroll body ->
          let (body', evidenceParamsLeft', generator') = go identityLocals evidenceParamsLeft generator body
           in (X.EUnroll body', evidenceParamsLeft', generator')

    lookupLocalByIdentity resolved locals =
      localRefFromDetails (X.resolvedVarDetails resolved) >>= (`Map.lookup` locals)

    insertLocalIdentity original resolved =
      case localRefFromDetails (X.resolvedVarDetails original) of
        Just localRef -> Map.insert localRef resolved
        Nothing -> id

    freshenLocalResolvedVar allowEvidence evidenceParamsLeft generator resolved
      | X.resolvedVarIsLocal resolved =
          let (localRef, generator') =
                freshLocalRef (X.resolvedVarReferenceName resolved) generator
              isEvidenceParam =
                allowEvidence && evidenceParamsLeft > 0
              details =
                if isEvidenceParam
                  then EvidenceId localRef
                  else LocalId localRef
              evidenceParamsLeft' =
                if isEvidenceParam
                  then evidenceParamsLeft - 1
                  else evidenceParamsLeft
           in (resolved {X.resolvedVarDetails = details}, evidenceParamsLeft', generator')
      | otherwise = (resolved, evidenceParamsLeft, generator)

    localRefFromDetails details =
      case details of
        LocalId localRef -> Just localRef
        EvidenceId localRef -> Just localRef
        _ -> Nothing

type EvidenceMethodKey = (SymbolIdentity, [SrcType], SymbolIdentity)

annotateDeferredEvidenceResolvedVars :: XmlfTerm -> DeferredObligations -> DeferredObligations
annotateDeferredEvidenceResolvedVars term obligations =
  fmap annotateObligation obligations
  where
    evidenceResolvedVars =
      Map.fromList (zip orderedKeys (collectEvidenceBinderResolvedVars (length orderedKeys) term))

    orderedKeys =
      orderedEvidenceMethodKeys obligations

    annotateObligation obligation =
      case obligation of
        DeferredMethod deferred ->
          DeferredMethod
            deferred
              { deferredMethodEvidence = annotateDeferredMethodEvidence deferred <$> deferredMethodEvidence deferred,
                deferredMethodLocalEvidence = map annotateEvidenceInfo (deferredMethodLocalEvidence deferred)
              }
        DeferredConstructor {} -> obligation
        DeferredCase {} -> obligation

    annotateDeferredMethodEvidence deferred evidence =
      evidence
        { deferredMethodEvidenceMethod =
            annotateEvidenceMethod
              (deferredMethodEvidenceKey deferred evidence)
              (deferredMethodEvidenceMethod evidence)
        }

    annotateEvidenceInfo evidence =
      let methods =
            fmap
              (\method -> annotateEvidenceMethod (evidenceInfoMethodKey evidence method) method)
              (evidenceMethodsByIdentity evidence)
       in evidence
            { evidenceMethodsByIdentity = methods
            }

    annotateEvidenceMethod key method
      | Just _ <- evidenceMethodResolvedVar method = method
      | Just resolved <- Map.lookup key evidenceResolvedVars =
          method {evidenceMethodResolvedVar = Just resolved}
      | otherwise = method

orderedEvidenceMethodKeys :: DeferredObligations -> [EvidenceMethodKey]
orderedEvidenceMethodKeys obligations =
  go Set.empty (concatMap obligationKeys (Map.elems obligations))
  where
    obligationKeys obligation =
      case obligation of
        DeferredMethod deferred ->
          concatMap evidenceInfoKeys (deferredMethodLocalEvidence deferred)
            ++ maybe [] ((: []) . deferredMethodEvidenceKey deferred) (deferredMethodEvidence deferred)
        DeferredConstructor {} -> []
        DeferredCase {} -> []

    evidenceInfoKeys evidence =
      [ evidenceInfoMethodKey evidence methodName0
      | methodName0 <- Map.elems (evidenceMethodsByIdentity evidence)
      ]

    go _ [] = []
    go seen (key : keys)
      | key `Set.member` seen = go seen keys
      | otherwise = key : go (Set.insert key seen) keys

deferredMethodEvidenceKey :: DeferredMethodCall -> DeferredMethodEvidence -> EvidenceMethodKey
deferredMethodEvidenceKey deferred evidence =
  ( methodInfoOwnerClassSymbolIdentity (deferredMethodInfo deferred),
    NE.toList (typeViewsIdentity (deferredMethodEvidenceClassArgs evidence)),
    methodInfoSymbolIdentity (deferredMethodInfo deferred)
  )

evidenceInfoMethodKey :: EvidenceInfo -> EvidenceMethod -> EvidenceMethodKey
evidenceInfoMethodKey evidence method =
  (evidenceClassSymbol evidence, NE.toList (typeViewsIdentity (evidenceTypeViews evidence)), evidenceMethodSymbol method)

collectEvidenceBinderResolvedVars :: Int -> XmlfTerm -> [X.ResolvedVar]
collectEvidenceBinderResolvedVars count0 =
  take count0 . go
  where
    go term =
      case term of
        X.EVarNode {} -> []
        X.ELit {} -> []
        X.ELam resolved body ->
          [resolved | X.resolvedVarIsEvidence resolved] ++ go body
        X.EApp fun arg ->
          go fun ++ go arg
        X.ELet _ _ rhs body ->
          go rhs ++ go body
        X.ETyAbsRef _ _ body ->
          go body
        X.ETyInst inner _ ->
          go inner
        X.ERoll _ body ->
          go body
        X.EUnroll body ->
          go body

generatedIdentitiesInDeferredObligations :: LoweredBinding -> [UniqueIdentity]
generatedIdentitiesInDeferredObligations lowered =
  generatedIdentitiesInDeferredObligationsMap (loweredBindingDeferredObligations lowered)

generatedIdentitiesInDeferredObligationsMap :: DeferredObligations -> [UniqueIdentity]
generatedIdentitiesInDeferredObligationsMap obligations =
  concatMap generatedIdentitiesInObligation (Map.elems obligations)
  where
    generatedIdentitiesInObligation obligation =
      [deferredRefIdentity (deferredProgramObligationRef obligation)]
        ++ case obligation of
          DeferredMethod deferred ->
            maybe [] generatedIdentitiesInDeferredEvidence (deferredMethodEvidence deferred)
              ++ concatMap generatedIdentitiesInEvidenceInfo (deferredMethodLocalEvidence deferred)
          DeferredConstructor {} -> []
          DeferredCase {} -> []

    generatedIdentitiesInDeferredEvidence evidence =
      generatedIdentitiesInEvidenceMethod (deferredMethodEvidenceMethod evidence)

    generatedIdentitiesInEvidenceInfo evidence =
      concatMap generatedIdentitiesInEvidenceMethod (Map.elems (evidenceMethodsByIdentity evidence))

    generatedIdentitiesInEvidenceMethod method =
      maybe [] (idDetailsGeneratedIdentities . X.resolvedVarDetails) (evidenceMethodResolvedVar method)

generatedIdentitiesInTerm :: XmlfTerm -> [UniqueIdentity]
generatedIdentitiesInTerm term =
  case term of
    X.EVarNode resolved -> generatedIdentitiesInResolved resolved
    X.ELit {} -> []
    X.ELam resolved body ->
      generatedIdentitiesInResolved resolved ++ generatedIdentitiesInTerm body
    X.EApp fun arg ->
      generatedIdentitiesInTerm fun ++ generatedIdentitiesInTerm arg
    X.ELet resolved _ rhs body ->
      generatedIdentitiesInResolved resolved
        ++ generatedIdentitiesInTerm rhs
        ++ generatedIdentitiesInTerm body
    X.ETyAbsRef _ _ body -> generatedIdentitiesInTerm body
    X.ETyInst inner _ -> generatedIdentitiesInTerm inner
    X.ERoll _ body -> generatedIdentitiesInTerm body
    X.EUnroll body -> generatedIdentitiesInTerm body

generatedIdentitiesInResolved :: X.ResolvedVar -> [UniqueIdentity]
generatedIdentitiesInResolved =
  idDetailsGeneratedIdentities . X.resolvedVarDetails

unresolvedXmlfTermVarRefs :: XmlfTerm -> [DeferredRef]
unresolvedXmlfTermVarRefs term =
  case term of
    X.EVarNode resolved ->
      case X.deferredResolvedVarRef resolved of
        Just ref -> [ref]
        Nothing -> []
    X.ELit {} -> []
    X.ELam _ body -> unresolvedXmlfTermVarRefs body
    X.EApp fun arg -> unresolvedXmlfTermVarRefs fun ++ unresolvedXmlfTermVarRefs arg
    X.ELet _ _ rhs body ->
      unresolvedXmlfTermVarRefs rhs ++ unresolvedXmlfTermVarRefs body
    X.ETyAbsRef _ _ body -> unresolvedXmlfTermVarRefs body
    X.ETyInst inner _ -> unresolvedXmlfTermVarRefs inner
    X.ERoll _ body -> unresolvedXmlfTermVarRefs body
    X.EUnroll body -> unresolvedXmlfTermVarRefs body

runtimeExternalBindingIndex :: FinalizeContext -> RuntimeExternalBindingIndex
runtimeExternalBindingIndex context =
  runtimeExternalBindingIndexFromScope (finalizeContextScope context) (finalizeContextRuntimeTypeEnv context)

runtimeExternalBindingIndexFromScope :: ElaborateScope -> Map String ElabType -> RuntimeExternalBindingIndex
runtimeExternalBindingIndexFromScope scope runtimeTypes =
  RuntimeExternalBindingIndex
    { runtimeExternalBindingKeyByName =
        Map.fromList [(runtimeName, key) | (runtimeName, key, _) <- entries],
      runtimeExternalBindingByKey =
        Map.fromList [(key, resolved) | (_, key, resolved) <- entries]
    }
  where
    entries =
      [ (runtimeName, key, resolved)
      | valueInfo <- Map.elems (elaborateScopeValues scope),
        Just (runtimeName, details) <- [valueResolvedDetails valueInfo],
        Just ty <- [Map.lookup runtimeName runtimeTypes],
        Just key <- [idDetailsReadKeyMaybe details],
        let resolved =
              X.ResolvedVar
                { X.resolvedVarRuntimeName = runtimeName,
                  X.resolvedVarType = ty,
                  X.resolvedVarDetails = details
                }
      ]

    valueResolvedDetails valueInfo =
      case valueInfo of
        OrdinaryValue
          { valueInfoSymbol = symbol,
            valueRuntimeName = runtimeName
          } ->
            Just
              ( runtimeName,
                TopLevelId symbol
              )
        ConstructorValue
          { valueRuntimeName = runtimeName,
            valueCtorInfo = ctorInfo
          } ->
            Just (runtimeName, ConstructorId (constructorRefFromInfo ctorInfo))
        OverloadedMethod {} ->
          Nothing

runtimeExternalBindingIdentity :: ElaborateScope -> Map String ElabType -> String -> Maybe ExternalBindingIdentity
runtimeExternalBindingIdentity scope runtimeTypes name = do
  resolved <- lookupRuntimeExternalBinding name (runtimeExternalBindingIndexFromScope scope runtimeTypes)
  pure
    ExternalBindingIdentity
      { externalBindingDisplayName = X.resolvedVarName resolved,
        externalBindingRuntimeName = X.resolvedVarRuntimeName resolved,
        externalBindingDetails = X.resolvedVarDetails resolved
      }

deferredExternalBindingIndex :: DeferredObligations -> DeferredExternalBindingIndex
deferredExternalBindingIndex obligations =
  DeferredExternalBindingIndex
    { deferredExternalBindingRefByName =
        Map.fromListWith
          (flip const)
          [ (deferredRefName ref, ref)
          | obligation <- Map.elems obligations,
            let ref = deferredProgramObligationRef obligation
          ],
      deferredExternalBindingByRef =
        Map.fromList
          [ (deferredProgramObligationRef obligation, obligation)
          | obligation <- Map.elems obligations
          ]
    }

deferredExternalBindingIdentity :: DeferredExternalBindingIndex -> String -> Maybe ExternalBindingIdentity
deferredExternalBindingIdentity index name = do
  obligation <- lookupDeferredExternalBinding name index
  let ref = deferredProgramObligationRef obligation
  pure
    ExternalBindingIdentity
      { externalBindingDisplayName = deferredRefName ref,
        externalBindingRuntimeName = deferredRefName ref,
        externalBindingDetails = DeferredId ref
      }

constructorBindingNeedsUnchecked :: ElaborateScope -> LoweredBinding -> Bool
constructorBindingNeedsUnchecked scope lowered =
  case lookupConstructorBindingRuntime scope lowered of
    Just (dataInfo, ctor) -> not (null (ctorForalls ctor)) || not (null (dataParams dataInfo))
    Nothing -> False

repairConstructorBindingTerm :: ElaborateScope -> LoweredBinding -> XmlfTerm -> XmlfTerm
repairConstructorBindingTerm scope lowered term =
  case lookupConstructorBindingRuntime scope lowered of
    Just (dataInfo, ctor)
      | not (null (dataParams dataInfo)) ->
          moveConstructorResultAbs (dataInfoIdentityHeadName dataInfo) (length (ctorArgs ctor)) term
    _ -> term

lookupConstructorBindingRuntime :: ElaborateScope -> LoweredBinding -> Maybe (DataInfo, ConstructorInfo)
lookupConstructorBindingRuntime scope lowered =
  case loweredBindingConstructorRef lowered of
    Just ref -> lookupConstructorRuntimeBySymbol scope (constructorRefSymbol ref)
    Nothing -> Nothing

lookupConstructorRuntimeBySymbol :: ElaborateScope -> SymbolIdentity -> Maybe (DataInfo, ConstructorInfo)
lookupConstructorRuntimeBySymbol scope identity =
  case
    [ (dataInfo, ctor)
      | dataInfo <- elaborateScopeUniqueDataTypes scope,
        ctor <- dataConstructors dataInfo,
        ctorInfoSymbol ctor == identity
    ]
  of
    match : _ -> Just match
    [] -> Nothing

data TypeAbsInfo = TypeAbsInfo
  { typeAbsRef :: X.TypeBinderRef,
    typeAbsBound :: Maybe X.BoundType
  }

newtype TermLamInfo = TermLamInfo X.ResolvedVar

data ConstructorSpineItem
  = SpineTypeAbs TypeAbsInfo
  | SpineLam TermLamInfo

moveConstructorResultAbs :: String -> Int -> XmlfTerm -> XmlfTerm
moveConstructorResultAbs typeName argCount term =
  let (spine, body) = collectConstructorSpine term
      typeAbs = [info | SpineTypeAbs info <- spine]
      lams = [info | SpineLam info <- spine]
      (resultAbs, otherAbs) = partitionResultAbs typeAbs
      (argLams, handlerLams) = splitAt argCount lams
   in wrapTypeAbs otherAbs (wrapLams argLams (wrapTypeAbs resultAbs (wrapLams handlerLams body)))
  where
    resultPrefix = "$" ++ typeName ++ "_result"

    partitionResultAbs =
      foldr
        ( \absInfo (results, others) ->
            if resultPrefix `isPrefixOf` X.typeBinderRefName (typeAbsRef absInfo)
              then (absInfo : results, others)
              else (results, absInfo : others)
        )
        ([], [])

collectConstructorSpine :: XmlfTerm -> ([ConstructorSpineItem], XmlfTerm)
collectConstructorSpine = go []
  where
    go acc = \case
      X.ETyAbsRef ref mb body ->
        go (acc ++ [SpineTypeAbs (TypeAbsInfo ref mb)]) body
      X.ELam resolved body ->
        go
          ( acc
              ++ [ SpineLam
                    (TermLamInfo resolved)
                 ]
          )
          body
      other ->
        (acc, other)

wrapTypeAbs :: [TypeAbsInfo] -> XmlfTerm -> XmlfTerm
wrapTypeAbs infos body =
  foldr (\TypeAbsInfo {typeAbsRef = ref, typeAbsBound = mb} acc -> X.ETyAbsRef ref mb acc) body infos

wrapLams :: [TermLamInfo] -> XmlfTerm -> XmlfTerm
wrapLams infos body =
  foldr wrapLam body infos
  where
    wrapLam (TermLamInfo resolved) acc = X.ELam resolved acc

runSurfacePipelineWithContext :: FinalizeContext -> Bool -> DeferredObligations -> Map String SrcType -> SurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipelineWithContext context forceUnchecked deferredObligations externalTypes surfaceExpr = do
  let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
      externalTypeNames = Map.keysSet externalTypes
      externalFreeVars = Set.fromList [name | name <- freeVars, name `Set.member` externalTypeNames]
      runtimeFreeVars = Set.fromList [name | name <- freeVars, name `Set.notMember` externalTypeNames]
      runtimeBindings = restrictPreparedExternalBindings runtimeFreeVars (finalizeContextRuntimeBindings context)
  mapM_ resolveRuntimeType freeVars
  deferredBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      externalBindingModeFor
      (deferredExternalBindingIdentity deferredExternalIndex)
      (lowerExternalTypes scope (Map.restrictKeys externalTypes externalFreeVars))
  let extEnv = deferredBindings `unionPreparedExternalBindings` runtimeBindings
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  let runPipeline =
        if not forceUnchecked && Map.null deferredObligations
          then runPipelineElabDetailedWithPreparedExternalBindings
          else runPipelineElabDetailedUncheckedWithPreparedExternalBindings
  either (Left . ProgramPipelineError . renderPipelineError) Right (runPipeline Set.empty extEnv normExpr)
  where
    scope = finalizeContextScope context
    runtimeTypes = externalTypes `Map.union` elaborateScopeRuntimeTypes scope
    deferredExternalIndex = deferredExternalBindingIndex deferredObligations

    resolveRuntimeType name =
      case Map.lookup name runtimeTypes of
        Just _ -> Right ()
        Nothing -> Left (ProgramUnknownValue name)

    externalBindingModeFor =
      externalBindingModeForObligations deferredExternalIndex externalTypes

runSurfacePipelineWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  Bool ->
  DeferredObligations ->
  Map String SrcType ->
  SurfaceExpr ->
  IO (Either ProgramError PipelineElabDetailedResult)
runSurfacePipelineWithContextWithTiming timing label context forceUnchecked deferredObligations externalTypes surfaceExpr =
  runExceptT $ do
    let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
        externalTypeNames = Map.keysSet externalTypes
        externalFreeVars = Set.fromList [name | name <- freeVars, name `Set.member` externalTypeNames]
        runtimeFreeVars = Set.fromList [name | name <- freeVars, name `Set.notMember` externalTypeNames]
        runtimeBindings = restrictPreparedExternalBindings runtimeFreeVars (finalizeContextRuntimeBindings context)
    fromProgramEither (mapM_ resolveRuntimeType freeVars)
    deferredBindings <-
      timeFinalizeEither timing (label ++ ".prepare_external_bindings") $
        evaluate $
          prepareSurfaceExternalBindingsWithIdentity
            externalBindingModeFor
            (deferredExternalBindingIdentity deferredExternalIndex)
            (lowerExternalTypes scope (Map.restrictKeys externalTypes externalFreeVars))
    normExpr <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
    let extEnv = deferredBindings `unionPreparedExternalBindings` runtimeBindings
        runPipeline =
          if not forceUnchecked && Map.null deferredObligations
            then runPipelineElabDetailedWithPreparedExternalBindingsWithTiming
            else runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") Set.empty extEnv normExpr
    fromProgramEither $
      either (Left . ProgramPipelineError . renderPipelineError) Right pipelineResult
  where
    scope = finalizeContextScope context
    runtimeTypes = externalTypes `Map.union` elaborateScopeRuntimeTypes scope
    deferredExternalIndex = deferredExternalBindingIndex deferredObligations

    resolveRuntimeType name =
      case Map.lookup name runtimeTypes of
        Just _ -> Right ()
        Nothing -> Left (ProgramUnknownValue name)

    externalBindingModeFor =
      externalBindingModeForObligations deferredExternalIndex externalTypes

runLoweredSurfacePipelineWithModuleContext ::
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  Either ProgramError PipelineElabDetailedResult
runLoweredSurfacePipelineWithModuleContext moduleContext forceUnchecked lowered = do
  readContext <- lookupModuleBindingReadContext moduleContext lowered
  let stampedLowered = moduleBindingReadLowered readContext
  moduleBindingReadResolvedFreeVars readContext
  extEnv <- moduleBindingReadExternalBindings readContext
  normExpr <- moduleBindingReadNormalizedExpr readContext
  let runPipeline =
        if not forceUnchecked && Map.null (loweredBindingDeferredObligations stampedLowered)
          then runPipelineElabDetailedWithPreparedExternalBindings
          else runPipelineElabDetailedUncheckedWithPreparedExternalBindings
      pipelineResult = runPipeline Set.empty extEnv normExpr
  either (Left . ProgramPipelineError . renderPipelineError) Right pipelineResult

runLoweredSurfacePipelineWithModuleContextWithTiming ::
  TimingConfig ->
  String ->
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError PipelineElabDetailedResult)
runLoweredSurfacePipelineWithModuleContextWithTiming timing label moduleContext forceUnchecked lowered =
  runExceptT $ do
    readContext <- fromProgramEither (lookupModuleBindingReadContext moduleContext lowered)
    let stampedLowered = moduleBindingReadLowered readContext
    fromProgramEither (moduleBindingReadResolvedFreeVars readContext)
    extEnv <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $
        moduleBindingReadExternalBindings readContext
    normExpr <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        moduleBindingReadNormalizedExpr readContext
    let runPipeline =
          if not forceUnchecked && Map.null (loweredBindingDeferredObligations stampedLowered)
            then runPipelineElabDetailedWithPreparedExternalBindingsWithTiming
            else runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") Set.empty extEnv normExpr
    fromProgramEither $
      either (Left . ProgramPipelineError . renderPipelineError) Right pipelineResult

lookupModuleBindingReadContext :: ModuleFinalizeContext -> LoweredBinding -> Either ProgramError ModuleBindingReadContext
lookupModuleBindingReadContext moduleContext lowered = do
  key <- loweredBindingReadKey lowered
  case Map.lookup key (moduleFinalizeContextBindingReads moduleContext) of
    Just readContext -> Right readContext
    Nothing ->
      Left (ProgramPipelineError ("missing module read context for binding `" ++ loweredBindingName lowered ++ "`"))

loweredBindingReadKey :: LoweredBinding -> Either ProgramError ModuleBindingReadKey
loweredBindingReadKey lowered =
  idDetailsReadKey (loweredIdentityDetails (loweredBindingIdentity lowered))

idDetailsReadKey :: IdDetails -> Either ProgramError ModuleBindingReadKey
idDetailsReadKey details =
  case details of
    LocalId LocalRef {localRefIdentity = GeneratedLocalId identity} ->
      Right (ModuleBindingReadLocal identity)
    EvidenceId LocalRef {localRefIdentity = GeneratedLocalId identity} ->
      Right (ModuleBindingReadLocal identity)
    EnvId EnvRef {envRefIdentity = identity} ->
      Right (ModuleBindingReadEnv identity)
    TopLevelId identity ->
      Right (ModuleBindingReadTopLevel identity)
    ConstructorId ConstructorRef {constructorRefSymbol = identity} ->
      Right (ModuleBindingReadConstructor identity)
    MethodId identity ->
      Right (ModuleBindingReadMethod identity)
    PrimitiveId PrimitiveRef {primitiveRefSymbol = identity} ->
      Right (ModuleBindingReadPrimitive identity)
    DeferredId DeferredRef {deferredRefIdentity = identity} ->
      Right (ModuleBindingReadDeferred identity)

idDetailsReadKeyMaybe :: IdDetails -> Maybe ModuleBindingReadKey
idDetailsReadKeyMaybe details =
  case idDetailsReadKey details of
    Right key -> Just key
    Left _ -> Nothing

externalBindingModeForObligations :: DeferredExternalBindingIndex -> Map String SrcType -> String -> ExternalBindingMode
externalBindingModeForObligations deferredExternalIndex externalTypes name =
  case lookupDeferredExternalBinding name deferredExternalIndex of
    Just (DeferredMethod {}) ->
      case Map.lookup name externalTypes of
        Just ty
          | not (Set.null (freeSourceTypeVars ty)) ->
              ExternalBindingMonomorphic
        _ -> ExternalBindingScheme
    Just (DeferredConstructor deferred) -> convertDeferredBindingMode (deferredConstructorBindingMode deferred)
    Just (DeferredCase {}) -> ExternalBindingMonomorphic
    _ ->
      case Map.lookup name externalTypes of
        Just ty -> externalBindingModeForSourceType ty
        Nothing -> ExternalBindingScheme
  where
    convertDeferredBindingMode mode =
      case mode of
        DeferredBindingScheme -> ExternalBindingScheme
        DeferredBindingMonomorphic -> ExternalBindingMonomorphic

    externalBindingModeForSourceType ty
      | sourceTypeHasForall ty = ExternalBindingScheme
      | not (Set.null (freeSourceTypeVars ty)) = ExternalBindingScheme
      | otherwise = ExternalBindingMonomorphic

lookupDeferredExternalBinding :: String -> DeferredExternalBindingIndex -> Maybe DeferredProgramObligation
lookupDeferredExternalBinding name index =
  Map.lookup name (deferredExternalBindingRefByName index)
    >>= (`Map.lookup` deferredExternalBindingByRef index)

lookupRuntimeExternalBinding :: String -> RuntimeExternalBindingIndex -> Maybe X.ResolvedVar
lookupRuntimeExternalBinding name index =
  Map.lookup name (runtimeExternalBindingKeyByName index)
    >>= (`Map.lookup` runtimeExternalBindingByKey index)

sourceTypeHasForall :: SrcType -> Bool
sourceTypeHasForall ty =
  case ty of
    STBase {} -> False
    STVar {} -> False
    STArrow left right -> sourceTypeHasForall left || sourceTypeHasForall right
    STForall {} -> True
    STMu _ body -> sourceTypeHasForall body
    STCon _ args -> any sourceTypeHasForall args
    STVarApp _ args -> any sourceTypeHasForall args
    STTyLam _ body -> sourceTypeHasForall body
    STTyApp fun arg -> sourceTypeHasForall fun || sourceTypeHasForall arg
    STBottom -> False

prepareSurfaceExternalBindingsWithIdentity ::
  (String -> ExternalBindingMode) ->
  (String -> Maybe ExternalBindingIdentity) ->
  Map String SrcType ->
  Either ProgramError PreparedExternalBindings
prepareSurfaceExternalBindingsWithIdentity modeFor identityFor sourceTypes = do
  extBindings <-
    Map.traverseWithKey
      ( \name ty -> do
          normTy <- either (Left . ProgramPipelineError . show) Right (normalizeType ty)
          Right
            ExternalBinding
              { externalBindingType = normTy,
                externalBindingMode = modeFor name,
                externalBindingIdentity = identityFor name
              }
      )
      sourceTypes
  either (Left . ProgramPipelineError . show) Right (prepareExternalBindings extBindings)

lowerExternalTypes :: ElaborateScope -> Map String SrcType -> Map String SrcType
lowerExternalTypes scope =
  Map.map (lowerType scope)

finalizeDeferredObligationsForBinding ::
  FinalizeContext ->
  LoweredBinding ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  SrcType ->
  Either ProgramError (XmlfTerm, ElabType)
finalizeDeferredObligationsForBinding context lowered deferredObligations tcEnv term inferredTy expectedBindingTy =
  case finalizeDeferredObligations context resolvedDeferredObligations tcEnv resolvedTerm inferredTy expectedBindingTy of
    Left (ProgramPipelineError msg) ->
      Left (ProgramPipelineError ("binding `" ++ loweredBindingName lowered ++ "`: " ++ msg))
    result -> result
  where
    loweredForIdentities =
      lowered {loweredBindingDeferredObligations = deferredObligations}
    resolvedTerm =
      annotateResolvedTermVars context loweredForIdentities term
    resolvedDeferredObligations =
      annotateDeferredEvidenceResolvedVars resolvedTerm deferredObligations

finalizeDeferredObligationsForGroup ::
  FinalizeContext ->
  [LoweredBinding] ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  SrcType ->
  Either ProgramError (XmlfTerm, ElabType)
finalizeDeferredObligationsForGroup context lowereds deferredObligations tcEnv term inferredTy expectedBindingTy =
  finalizeDeferredObligations context resolvedDeferredObligations tcEnv resolvedTerm inferredTy expectedBindingTy
  where
    resolvedTerm =
      annotateResolvedTermVarsForGroup context lowereds deferredObligations term
    resolvedDeferredObligations =
      annotateDeferredEvidenceResolvedVars resolvedTerm deferredObligations

finalizeDeferredObligations ::
  FinalizeContext ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  SrcType ->
  Either ProgramError (XmlfTerm, ElabType)
finalizeDeferredObligations _ deferredObligations _ term inferredTy _
  | Map.null deferredObligations = Right (term, inferredTy)
finalizeDeferredObligations context deferredObligations tcEnv term _ expectedBindingTy = do
  let rewriteEnv = extendTypeCheckEnvWithRuntimeContext context tcEnv
  let constructorObligations = Map.mapMaybe onlyConstructor deferredObligations
      caseObligations = Map.mapMaybe onlyCase deferredObligations
      methodObligations = Map.mapMaybe onlyMethod deferredObligations
  constructorsRewritten <-
    if Map.null constructorObligations
      then Right term
      else resolveDeferredConstructors scope rewriteEnv constructorObligations term
  (caseRewriteEnv, casesRewritten) <-
    if Map.null caseObligations
      then Right (rewriteEnv, constructorsRewritten)
      else resolveDeferredCases scope caseObligations rewriteEnv constructorsRewritten
  methodsRewritten <-
    if Map.null methodObligations
      then Right casesRewritten
      else resolveDeferredMethods scope methodObligations caseRewriteEnv casesRewritten
  rewritten <-
    if termHasLets methodsRewritten
      then refreshLetSchemes caseRewriteEnv methodsRewritten
      else Right methodsRewritten
  let rewrittenClean = dropStaleTypeInsts caseRewriteEnv rewritten
  let rewrittenForCheck =
        if termHasTypeAbs rewrittenClean
          then freshenTypeAbsAgainstEnv caseRewriteEnv rewrittenClean
          else rewrittenClean
  rewrittenTy <-
    case typeCheckWithEnv caseRewriteEnv rewrittenForCheck of
      Right ty -> Right (inlineTypeEnvBounds caseRewriteEnv ty)
      Left X.TCArgumentMismatch {} ->
        srcTypeToElabTypeInScope scope (lowerType scope expectedBindingTy)
      Left err ->
        Left
          ( ProgramPipelineError
              ("deferred program obligation rewrite failed type check: " ++ show err)
          )
  Right (rewrittenForCheck, rewrittenTy)
  where
    scope = finalizeContextScope context

    onlyConstructor = \case
      DeferredConstructor deferred -> Just deferred
      _ -> Nothing

    onlyCase = \case
      DeferredCase deferred -> Just deferred
      _ -> Nothing

    onlyMethod = \case
      DeferredMethod deferred -> Just deferred
      _ -> Nothing

termHasLets :: XmlfTerm -> Bool
termHasLets term =
  case term of
    X.EVarNode {} -> False
    X.ELit {} -> False
    X.ELam _ body -> termHasLets body
    X.EApp fun arg -> termHasLets fun || termHasLets arg
    X.ELet {} -> True
    X.ETyAbsRef _ _ body -> termHasLets body
    X.ETyInst inner _ -> termHasLets inner
    X.ERoll _ body -> termHasLets body
    X.EUnroll inner -> termHasLets inner

termHasTypeAbs :: XmlfTerm -> Bool
termHasTypeAbs term =
  case term of
    X.EVarNode {} -> False
    X.ELit {} -> False
    X.ELam _ body -> termHasTypeAbs body
    X.EApp fun arg -> termHasTypeAbs fun || termHasTypeAbs arg
    X.ELet _ _ rhs body -> termHasTypeAbs rhs || termHasTypeAbs body
    X.ETyAbsRef {} -> True
    X.ETyInst inner _ -> termHasTypeAbs inner
    X.ERoll _ body -> termHasTypeAbs body
    X.EUnroll inner -> termHasTypeAbs inner

dropStaleTypeInsts :: Env -> XmlfTerm -> XmlfTerm
dropStaleTypeInsts env term =
  case term of
    X.EVarNode {} -> term
    X.ELit {} -> term
    X.ELam resolved body ->
      let ty = X.resolvedVarType resolved
          env' = TypeCheck.insertResolvedTermBinding resolved ty env
       in X.ELam resolved (dropStaleTypeInsts env' body)
    X.EApp fun arg ->
      X.EApp (dropStaleTypeInsts env fun) (dropStaleTypeInsts env arg)
    X.ELet resolved scheme rhs body ->
      let schemeTy = schemeToType scheme
          env' = TypeCheck.insertResolvedTermBinding resolved schemeTy env
       in X.ELet resolved scheme (dropStaleTypeInsts env' rhs) (dropStaleTypeInsts env' body)
    X.ETyAbsRef ref mbBound body ->
      let boundTy = maybe X.TBottom X.tyToElab mbBound
          env' = TypeCheck.insertTypeBindingRef ref boundTy env
       in X.ETyAbsRef ref mbBound (dropStaleTypeInsts env' body)
    X.ETyInst inner inst ->
      let inner' = dropStaleTypeInsts env inner
       in if instConsumesForall inst && instTargetHasNoTopForall env inner'
            then inner'
            else X.ETyInst inner' inst
    X.ERoll ty body -> X.ERoll ty (dropStaleTypeInsts env body)
    X.EUnroll inner -> X.EUnroll (dropStaleTypeInsts env inner)

instTargetHasNoTopForall :: Env -> XmlfTerm -> Bool
instTargetHasNoTopForall env term =
  case typeCheckWithEnv env term of
    Right X.TForallRef {} -> False
    Right _ -> True
    Left _ -> False

instConsumesForall :: X.Instantiation -> Bool
instConsumesForall inst =
  case inst of
    X.InstId -> False
    X.InstApp _ -> True
    X.InstIntro -> False
    X.InstElim -> True
    X.InstInside inner -> instConsumesForall inner || True
    X.InstSeq left right -> instConsumesForall left || instConsumesForall right
    X.InstUnderRef _ inner -> instConsumesForall inner
    X.InstBot _ -> False
    X.InstAbstrRef _ -> False

extendTypeCheckEnvWithRuntimeContext :: FinalizeContext -> Env -> Env
extendTypeCheckEnvWithRuntimeContext context env =
  runtimeTypeCheckEnv context `TypeCheck.unionEnvs` env

runtimeTypeCheckEnv :: FinalizeContext -> Env
runtimeTypeCheckEnv context =
  TypeCheck.mkTypeCheckEnvWithResolvedTerms (resolvedEntries ++ unresolvedEntries) Map.empty
  where
    runtimeTypes = finalizeContextRuntimeTypeEnv context
    runtimeIndex = runtimeExternalBindingIndex context

    resolvedEntries =
      [ (resolved, X.resolvedVarType resolved)
      | resolved <- Map.elems (runtimeExternalBindingByKey runtimeIndex)
      ]

    unresolvedRuntimeTypes =
      Map.withoutKeys runtimeTypes resolvedRuntimeNames

    resolvedRuntimeNames =
      Set.fromList
        [ X.resolvedVarRuntimeName resolved
        | resolved <- Map.elems (runtimeExternalBindingByKey runtimeIndex)
        ]

    unresolvedEntries =
      snd (mapAccumL mkUnresolvedEntry unresolvedGenerator (Map.toList unresolvedRuntimeTypes))

    unresolvedGenerator =
      identityGeneratorAfter
        ( concatMap (idDetailsGeneratedIdentities . X.resolvedVarDetails . fst) resolvedEntries
            ++ concatMap X.generatedIdentitiesInType (Map.elems runtimeTypes)
        )

    mkUnresolvedEntry generator (name, ty) =
      let (envRef, generator') = freshEnvRef name generator
          resolved =
            X.ResolvedVar
              { X.resolvedVarRuntimeName = name,
                X.resolvedVarType = ty,
                X.resolvedVarDetails = EnvId envRef
              }
       in (generator', (resolved, ty))

inlineTypeEnvBounds :: Env -> ElabType -> ElabType
inlineTypeEnvBounds env = go []
  where
    go seen ty = case ty of
      X.TVarRef ref
        | any (X.typeBinderRefsSameIdentity ref) seen -> ty
        | otherwise ->
            case TypeCheck.lookupTypeBindingRef ref env of
              Just bound
                | bound /= X.TBottom -> go (ref : seen) bound
              _ -> ty
      X.TArrow dom cod -> X.TArrow (go seen dom) (go seen cod)
      X.TConWithIdentity identity con args -> X.TConWithIdentity identity con (fmap (go seen) args)
      X.TVarAppRef ref args -> X.TVarAppRef ref (fmap (go seen) args)
      X.TBaseWithIdentity {} -> ty
      X.TBottom -> ty
      X.TForallRef ref mb body ->
        let seen' = ref : seen
         in X.TForallRef ref (fmap (goBound seen') mb) (go seen' body)
      X.TMuRef ref body ->
        let seen' = ref : seen
         in X.TMuRef ref (go seen' body)

    goBound seen bound = case bound of
      X.TArrow dom cod -> X.TArrow (go seen dom) (go seen cod)
      X.TConWithIdentity identity con args -> X.TConWithIdentity identity con (fmap (go seen) args)
      X.TVarAppRef ref args -> X.TVarAppRef ref (fmap (go seen) args)
      X.TBaseWithIdentity {} -> bound
      X.TBottom -> bound
      X.TForallRef ref mb body ->
        let seen' = ref : seen
         in X.TForallRef ref (fmap (goBound seen') mb) (go seen' body)
      X.TMuRef ref body ->
        let seen' = ref : seen
         in X.TMuRef ref (go seen' body)

inferRewrittenLetType :: Env -> XmlfTerm -> ElabType -> ElabType
inferRewrittenLetType env rhs fallback =
  case typeCheckWithEnv env rhs of
    Right ty ->
      preserveRewrittenLetScheme
        (inlineTypeEnvBounds env (stripVacuousForalls fallback))
        (inlineTypeEnvBounds env (stripVacuousForalls ty))
    Left _ -> fallback

preserveRewrittenLetScheme :: ElabType -> ElabType -> ElabType
preserveRewrittenLetScheme fallbackTy rhsTy =
  case peelTopForalls fallbackTy of
    ([], _) -> rhsTy
    (_, fallbackBody)
      | compatibleLetType fallbackBody rhsTy -> fallbackTy
    _ -> rhsTy

peelTopForalls :: ElabType -> ([(X.TypeBinderRef, Maybe X.BoundType)], ElabType)
peelTopForalls ty =
  case ty of
    X.TForallRef ref mbBound body ->
      let (binds, body') = peelTopForalls body
       in ((ref, mbBound) : binds, body')
    _ -> ([], ty)

compatibleLetType :: ElabType -> ElabType -> Bool
compatibleLetType expected actual =
  expected == actual
    || alphaEqType expected actual
    || churchAwareEqType expected actual

freeSourceTypeVars :: SrcType -> Set String
freeSourceTypeVars =
  freeSrcTypeVars

freeSrcTypeVars :: SrcTy n v -> Set String
freeSrcTypeVars = go Set.empty
  where
    go :: Set String -> SrcTy n0 v0 -> Set String
    go boundVars ty =
      case ty of
        STVar name
          | name `Set.member` boundVars -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go boundVars dom `Set.union` go boundVars cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go boundVars) args
        STVarApp name args ->
          let headVars =
                if name `Set.member` boundVars
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go boundVars) args
        STTyLam name body -> go (Set.insert name boundVars) body
        STTyApp fun arg -> go boundVars fun `Set.union` go boundVars arg
        STForall name mb body ->
          maybe Set.empty (go boundVars . unSrcBound) mb
            `Set.union` go (Set.insert name boundVars) body
        STMu name body -> go (Set.insert name boundVars) body
        STBottom -> Set.empty

sourceForallMatchesWithRigidForalls :: SrcType -> SrcType -> Bool
sourceForallMatchesWithRigidForalls expected actual =
  case sourceForallMatchSubst expected actual of
    Just subst -> all (forallBinderRemainsPolymorphic subst) (usedLeadingForallNames expected)
    Nothing -> False
  where
    forallBinderRemainsPolymorphic subst name =
      case Map.lookup name subst of
        Just STVar {} -> True
        Just STVarApp {} -> True
        Just _ -> False
        Nothing -> True

    usedLeadingForallNames ty =
      case ty of
        STForall name _ body
          | name `Set.member` freeSourceTypeVars body -> name : usedLeadingForallNames body
          | otherwise -> usedLeadingForallNames body
        _ -> []

sourceForallMatches :: SrcType -> SrcType -> Bool
sourceForallMatches expected actual =
  case sourceForallMatchSubst expected actual of
    Just _ -> True
    Nothing -> False

sourceForallMatchSubst :: SrcType -> SrcType -> Maybe (Map String SrcType)
sourceForallMatchSubst expected actual =
  match Set.empty Map.empty expected actual
  where
    match bound subst template actualTy =
      case template of
        STForall name mb body ->
          case actualTy of
            STForall actualName actualMb actualBody -> do
              let bound' = Set.insert name bound
                  subst' = Map.insert name (STVar actualName) (Map.delete name subst)
              subst'' <- matchForallBounds bound' subst' mb actualMb
              match
                bound'
                subst''
                body
                actualBody
            _ -> match (Set.insert name bound) (Map.delete name subst) body actualTy
        STVar name
          | name `Set.member` bound ->
              matchBoundVar subst name actualTy
          | otherwise ->
              case actualTy of
                STVar actualName | actualName == name -> Just subst
                _ -> Nothing
        STArrow dom cod ->
          case actualTy of
            STForall name _ body
              | name `Set.notMember` freeTypeVarsSrcTypeLocal body ->
                  match bound subst template body
            STArrow dom' cod' -> do
              subst' <- match bound subst dom dom'
              match bound subst' cod cod'
            _ -> Nothing
        STBase name ->
          case actualTy of
            STBase actualName | actualName == name -> Just subst
            _ -> Nothing
        STCon name args ->
          case actualTy of
            STCon actualName actualArgs
              | actualName == name && length (toListNE args) == length (toListNE actualArgs) ->
                  foldM
                    (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                    subst
                    (zip (toListNE args) (toListNE actualArgs))
            _ -> Nothing
        STVarApp name args ->
          matchVarApp bound subst name args actualTy
        STTyLam name body ->
          case actualTy of
            STTyLam name' body'
              | name == name' -> match (Set.insert name bound) subst body body'
            _ -> Nothing
        STTyApp fun arg ->
          case actualTy of
            STTyApp fun' arg' -> do
              subst' <- match bound subst fun fun'
              match bound subst' arg arg'
            _ -> Nothing
        STMu _ body -> match bound subst body actualTy
        STBottom ->
          case actualTy of
            STBottom -> Just subst
            _ -> Nothing

    matchForallBounds bound subst expectedMb actualMb =
      case (expectedMb, actualMb) of
        (Nothing, Nothing) -> Just subst
        (Just (SrcBound expectedBound), Just (SrcBound actualBound)) ->
          match bound subst expectedBound actualBound
        _ -> Nothing

    matchVarApp bound subst expectedName args actualTy
      | expectedName `Set.member` bound =
          case actualTy of
            STCon actualName actualArgs ->
              matchAppliedHead actualName toConHead (toListNE actualArgs)
            STVarApp actualName actualArgs ->
              matchAppliedHead actualName toVarHead (toListNE actualArgs)
            _ -> Nothing
      | otherwise =
          matchRigidVarAppHead expectedName
      where
        expectedArgs = toListNE args
        expectedArgCount = length expectedArgs

        matchAppliedHead actualName headFromPrefix actualArgs
          | length actualArgs < expectedArgCount = Nothing
          | otherwise = do
              let (headArgs, appliedArgs) = splitAt (length actualArgs - expectedArgCount) actualArgs
              subst' <- matchBoundVar subst expectedName (headFromPrefix actualName headArgs)
              foldM
                (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                subst'
                (zip expectedArgs appliedArgs)

        matchRigidVarAppHead rigidName =
          case actualTy of
            STVarApp actualName actualArgs
              | rigidName == actualName && expectedArgCount == length (toListNE actualArgs) ->
                  foldM
                    (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                    subst
                    (zip expectedArgs (toListNE actualArgs))
            _ -> Nothing

        toConHead actualName [] = STBase actualName
        toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

        toVarHead actualName [] = STVar actualName
        toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

    matchBoundVar subst name actualTy =
      case Map.lookup name subst of
        Nothing -> Just (Map.insert name actualTy subst)
        Just existing
          | alphaEqSrcType existing actualTy -> Just subst
          | otherwise -> Nothing

    freeTypeVarsSrcTypeLocal = freeSourceTypeVars

alphaEqSrcType :: SrcType -> SrcType -> Bool
alphaEqSrcType = go Map.empty Map.empty
  where
    go leftNames rightNames left right =
      case (left, right) of
        (STVar leftName, STVar rightName) ->
          sameTypeVar leftNames rightNames leftName rightName
        (STArrow leftDom leftCod, STArrow rightDom rightCod) ->
          go leftNames rightNames leftDom rightDom
            && go leftNames rightNames leftCod rightCod
        (STBase leftName, STBase rightName) -> leftName == rightName
        (STCon leftName leftArgs, STCon rightName rightArgs) ->
          leftName == rightName
            && length (toListNE leftArgs) == length (toListNE rightArgs)
            && and (zipWith (go leftNames rightNames) (toListNE leftArgs) (toListNE rightArgs))
        (STVarApp leftName leftArgs, STVarApp rightName rightArgs) ->
          sameTypeVar leftNames rightNames leftName rightName
            && length (toListNE leftArgs) == length (toListNE rightArgs)
            && and (zipWith (go leftNames rightNames) (toListNE leftArgs) (toListNE rightArgs))
        (STTyLam leftName leftBody, STTyLam rightName rightBody) ->
          go
            (Map.insert leftName rightName leftNames)
            (Map.insert rightName leftName rightNames)
            leftBody
            rightBody
        (STTyApp leftFun leftArg, STTyApp rightFun rightArg) ->
          go leftNames rightNames leftFun rightFun
            && go leftNames rightNames leftArg rightArg
        (STForall leftName leftMb leftBody, STForall rightName rightMb rightBody) ->
          let leftNames' = Map.insert leftName rightName leftNames
              rightNames' = Map.insert rightName leftName rightNames
           in sameBounds leftNames' rightNames' leftMb rightMb
                && go leftNames' rightNames' leftBody rightBody
        (STMu leftName leftBody, STMu rightName rightBody) ->
          go
            (Map.insert leftName rightName leftNames)
            (Map.insert rightName leftName rightNames)
            leftBody
            rightBody
        (STBottom, STBottom) -> True
        _ -> False
      where
        sameBounds _ _ Nothing Nothing = True
        sameBounds leftNames' rightNames' (Just (SrcBound leftBound)) (Just (SrcBound rightBound)) =
          go leftNames' rightNames' leftBound rightBound
        sameBounds _ _ _ _ = False

    sameTypeVar leftNames rightNames leftName rightName =
      case (Map.lookup leftName leftNames, Map.lookup rightName rightNames) of
        (Just mappedRight, Just mappedLeft) -> mappedRight == rightName && mappedLeft == leftName
        (Nothing, Nothing) -> leftName == rightName
        _ -> False

refreshLetSchemes :: Env -> XmlfTerm -> Either ProgramError XmlfTerm
refreshLetSchemes = go
  where
    go env term =
      case term of
        X.EVarNode {} -> Right term
        X.ELit {} -> Right term
        X.ELam resolved body -> do
          let ty = X.resolvedVarType resolved
              env' = TypeCheck.insertResolvedTermBinding resolved ty env
          X.ELam resolved <$> go env' body
        X.EApp fun arg -> X.EApp <$> go env fun <*> go env arg
        X.ELet resolved scheme rhs body -> do
          let schemeTy = schemeToType scheme
              rhsEnv = TypeCheck.insertResolvedTermBinding resolved schemeTy env
          rhsRaw <- go rhsEnv rhs
          let rhs' = dropStaleTypeInsts rhsEnv rhsRaw
          let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
              scheme' = schemeFromType rhsTy
              rhsClosed = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme' rhs'
              resolved' = X.mapResolvedVarType (const rhsTy) resolved
              env' = TypeCheck.insertResolvedTermBinding resolved' rhsTy env
          X.ELet resolved' scheme' rhsClosed <$> go env' body
        X.ETyAbsRef ref mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = TypeCheck.insertTypeBindingRef ref boundTy env
          X.ETyAbsRef ref mbBound <$> go env' body
        X.ETyInst inner inst -> (`X.ETyInst` inst) <$> go env inner
        X.ERoll ty body -> X.ERoll ty <$> go env body
        X.EUnroll inner -> X.EUnroll <$> go env inner

resolveDeferredConstructors :: ElaborateScope -> Env -> Map DeferredRef DeferredConstructorCall -> XmlfTerm -> Either ProgramError XmlfTerm
resolveDeferredConstructors scope env deferredConstructors = go env
  where
    lookupDeferredConstructor ref =
      Map.lookup ref deferredConstructors

    go env0 term =
      case deferredPlaceholderHeadRefWithInsts term of
        Just (ref, headInsts)
          | Just deferred <- lookupDeferredConstructor ref,
            deferredConstructorArgCount deferred == 0 ->
              instantiateConstructorOccurrence env0 (deferredRefName ref) deferred headInsts [] term
        _ ->
          case term of
            X.EVarNode {} -> Right term
            X.ELit {} -> Right term
            X.ELam resolved body ->
              let ty = X.resolvedVarType resolved
                  env' = TypeCheck.insertResolvedTermBinding resolved ty env0
               in X.ELam resolved <$> go env' body
            X.EApp {} -> rewriteApplication env0 term
            X.ELet resolved scheme rhs body -> do
              let schemeTy = schemeToType scheme
                  rhsEnv = TypeCheck.insertResolvedTermBinding resolved schemeTy env0
              rhs' <- go rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  resolved' = X.mapResolvedVarType (const rhsTy) resolved
                  env' = TypeCheck.insertResolvedTermBinding resolved' rhsTy env0
              X.ELet resolved' scheme rhs' <$> go env' body
            X.ETyAbsRef ref mbBound body ->
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = TypeCheck.insertTypeBindingRef ref boundTy env0
               in X.ETyAbsRef ref mbBound <$> go env' body
            X.ETyInst inner inst -> (`X.ETyInst` inst) <$> go env0 inner
            X.ERoll ty body -> X.ERoll ty <$> go env0 body
            X.EUnroll inner -> X.EUnroll <$> go env0 inner

    rewriteApplication env0 term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHeadRefWithInsts headTerm of
            Just (ref, headInsts)
              | Just deferred <- lookupDeferredConstructor ref -> do
              args' <- mapM (go env0) args
              instantiateConstructorOccurrence env0 (deferredRefName ref) deferred headInsts args' term
            Nothing ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env0 fun <*> go env0 arg
                _ -> Right term
            _ ->
              case term of
                X.EApp fun arg -> X.EApp <$> go env0 fun <*> go env0 arg
                _ -> Right term

    instantiateConstructorOccurrence env0 placeholderName deferred headInsts args occurrenceTerm = do
      let ctorInfo = deferredConstructorInfo deferred
          visibleArgCount = min (deferredConstructorArgCount deferred) (length (ctorArgs ctorInfo))
          visibleArgTemplates = take visibleArgCount (ctorArgs ctorInfo)
          visibleArgs = take visibleArgCount args
          instBinders = deferredConstructorInstBinders deferred
      argTypes <- mapM (inferArgSourceType env0) visibleArgs
      substFromHead <-
        foldM
          ( \(subst, remainingBinders) instTy ->
              case remainingBinders of
                binder : rest -> do
                  let recoveredInstTy = recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls instTy))
                  subst' <-
                    maybe
                      (Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo)))
                      Right
                      (bindTypeBinderSubstInScope scope binder recoveredInstTy subst)
                  Right (subst', rest)
                [] -> Right (subst, [])
          )
          (deferredConstructorInitialSubst deferred, instBinders)
          headInsts
      let substFromArgs =
            case matchTypeBinderSubstPairsInScope scope instBinders (fst substFromHead) (zip visibleArgTemplates argTypes) of
              Just subst -> subst
              Nothing ->
                case matchTypeBinderSubstPairsInScope scope instBinders (deferredConstructorInitialSubst deferred) (zip visibleArgTemplates argTypes) of
                  Just subst -> subst
                  Nothing -> fst substFromHead
      occurrenceTy <-
        let occurrenceFallbackTy = applyConstructorSubst substFromArgs (deferredConstructorOccurrenceType deferred)
         in do
              occurrenceEnv <- ensureDeferredConstructorPlaceholderEnv env0 placeholderName deferred substFromArgs
              inferOccurrenceSourceType occurrenceEnv placeholderName occurrenceFallbackTy occurrenceTerm
      let substFinal =
            case matchTypeBinderSubstInScope scope instBinders substFromArgs (deferredConstructorOccurrenceType deferred) occurrenceTy of
              Just subst -> subst
              Nothing -> substFromArgs
          missingInstBinders =
            filter
              (\binder -> maybe True (const False) (lookupTypeBinderSubst binder substFinal))
              instBinders
      case missingInstBinders of
        [] -> do
          ctorHead <-
            if constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo
              then
                foldM
                  ( \headAcc binder ->
                      case lookupTypeBinderSubst binder substFinal of
                        Just ty -> do
                          instTy <- srcTypeToElabTypeInScope scope (lowerType scope ty)
                          Right (X.ETyInst headAcc (X.InstApp instTy))
                        Nothing -> Right headAcc
                  )
                  (X.EVarNode (resolvedVarFromConstructorInfo ctorInfo))
                  instBinders
              else inlineConstructorHead scope ctorInfo substFinal
          Right (foldl X.EApp ctorHead args)
        _ -> Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo))

    inferArgSourceType env0 arg =
      case typeCheckWithEnv env0 arg of
        Right ty -> Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty)))
        Left (X.TCArgumentMismatch _ actualTy) ->
          Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls actualTy)))
        Left err -> Left (ProgramPipelineError ("deferred constructor argument type check failed: " ++ show err))

    inferOccurrenceSourceType env0 placeholderName fallbackTy occurrenceTerm =
      case typeCheckWithEnv env0 occurrenceTerm of
        Right ty -> Right (recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls ty)))
        Left err
          | isDeferredConstructorArgumentMismatch err ->
              Right fallbackTy
          | isDeferredConstructorSelfUnbound placeholderName err ->
              Right fallbackTy
        Left err -> Left (ProgramPipelineError ("deferred constructor occurrence type check failed: " ++ show err))

    isDeferredConstructorArgumentMismatch err =
      case err of
        X.TCArgumentMismatch {} -> True
        _ -> False

    isDeferredConstructorSelfUnbound placeholderName err =
      case err of
        X.TCUnboundVar name -> name == placeholderName
        _ -> False

    ensureDeferredConstructorPlaceholderEnv env0 _placeholder deferred subst = do
      placeholderTy <- srcTypeToElabTypeInScope scope (lowerType scope placeholderSourceTy)
      let resolved = X.deferredResolvedVarFromRef (deferredConstructorRef deferred)
      Right (TypeCheck.insertResolvedTermBinding resolved placeholderTy env0)
      where
        placeholderSourceTy = applyConstructorSubst subst (deferredConstructorSourceType deferred)

matchTypeBinderSubstPairsInScope ::
  ElaborateScope ->
  [(String, Maybe TypeBinderIdentity)] ->
  TypeBinderSubst ->
  [(SrcType, SrcType)] ->
  Maybe TypeBinderSubst
matchTypeBinderSubstPairsInScope scope binders =
  foldM (\subst (templateTy, actualTy) -> matchTypeBinderSubstInScope scope binders subst templateTy actualTy)

matchTypeBinderSubstInScope ::
  ElaborateScope ->
  [(String, Maybe TypeBinderIdentity)] ->
  TypeBinderSubst ->
  SrcType ->
  SrcType ->
  Maybe TypeBinderSubst
matchTypeBinderSubstInScope scope binders subst templateTy actualTy =
  typeBinderSubstFromTypeViewSubst binders
    <$> matchTypeViewsAgainstIdentity
      scope
      (typeBinderSubstToTypeViewSubstWith (sourceTypeViewInScope scope) subst)
      (NE.singleton (typeBinderTemplateView scope binders templateTy))
      (NE.singleton (sourceTypeViewInScope scope actualTy))

typeBinderTemplateView :: ElaborateScope -> [(String, Maybe TypeBinderIdentity)] -> SrcType -> TypeView
typeBinderTemplateView scope binders ty =
  (sourceTypeViewInScope scope ty)
    { typeViewBinderIdentities =
        Map.fromList [(name, identity) | (name, Just identity) <- binders]
    }

bindTypeBinderSubstInScope ::
  ElaborateScope ->
  (String, Maybe TypeBinderIdentity) ->
  SrcType ->
  TypeBinderSubst ->
  Maybe TypeBinderSubst
bindTypeBinderSubstInScope scope binder@(name, _) actual subst =
  case lookupTypeBinderSubst binder subst of
    Nothing ->
      Just (insertTypeBinderSubst binder actual subst)
    Just (STVar existingName)
      | existingName == name ->
          Just (insertTypeBinderSubst binder actual subst)
    Just existing
      | alphaEqSrcType existing actual ->
          Just subst
      | Just existingTy <- srcTypeToElabTypeMaybeInScope scope (lowerType scope existing),
        Just actualTy <- srcTypeToElabTypeMaybeInScope scope (lowerType scope actual),
        alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
          Just subst
      | otherwise -> Nothing

inlineConstructorHead :: ElaborateScope -> ConstructorInfo -> TypeBinderSubst -> Either ProgramError XmlfTerm
inlineConstructorHead scope ctorInfo subst = do
  let resultSrcTy = applyConstructorSubst subst (ctorResult ctorInfo)
      argSrcTys = map (applyConstructorSubst subst) (ctorArgs ctorInfo)
      resultVar = "$" ++ symbolDefiningName (ctorOwningTypeIdentity ctorInfo) ++ "_result"
      argNames = ["$" ++ constructorInfoIdentityName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length argSrcTys]]
      ownerShapes =
        case lookupConstructorRuntimeBySymbol scope (ctorInfoSymbol ctorInfo) of
          Just (dataInfo, _) -> map constructorShapeFromInfo (dataConstructors dataInfo)
          Nothing -> constructorOwnerShapes ctorInfo
      handlerShapes = map specializeHandlerShape ownerShapes
      handlerNames =
        [ "$" ++ constructorShapeName shape ++ "_k" ++ show ix
          | (ix, shape) <- zip ([1 :: Int ..]) handlerShapes
        ]
      handlerSrcType shape =
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound mbBound) acc)
          (foldr STArrow (STVar resultVar) (constructorShapeArgs shape))
          (constructorShapeForalls shape)
      loweredResultSrcTy = lowerType scope resultSrcTy
      loweredArgSrcTys = map (lowerType scope) argSrcTys
      loweredHandlerSrcTys = map (lowerType scope . handlerSrcType) handlerShapes
      sharedFreeNames =
        Set.toList $
          Set.delete resultVar $
            Set.unions (map freeSrcTypeVars (loweredResultSrcTy : loweredArgSrcTys ++ loweredHandlerSrcTys))
      (sharedRefs, generator0) = freshTypeBinderRefs sharedFreeNames initialIdentityGenerator
      sharedTypeAbsRefs =
        [ ref
        | name <- sharedFreeNames,
          Just ref <- [Map.lookup name sharedRefs]
        ]
      (resultRef, generator1) = X.freshTypeBinderRef resultVar generator0
      handlerRefs = Map.insert resultVar resultRef sharedRefs
  (resultTy, generator2) <- srcTypeToElabTypeWithScope scope sharedRefs generator1 loweredResultSrcTy
  (argTys, generator3) <- srcTypesToElabTypesWith sharedRefs generator2 loweredArgSrcTys
  (handlerTys, generator4) <- srcTypesToElabTypesWith handlerRefs generator3 loweredHandlerSrcTys
  let (argResolved, generator5) = freshResolvedLocals generator4 (zip argNames argTys)
      (handlerResolved, _) = freshResolvedLocals generator5 (zip handlerNames handlerTys)
  selectedResolved <-
    case drop (ctorIndex ctorInfo) handlerResolved of
      resolved : _ -> Right resolved
      [] -> Left (ProgramPipelineError ("constructor handler order missing `" ++ ctorName ctorInfo ++ "`"))
  let selectedBody = foldl X.EApp (X.EVarNode selectedResolved) (map X.EVarNode argResolved)
      handlerBody = foldr X.ELam selectedBody handlerResolved
      rolled = X.ERoll resultTy (X.eTyAbsWithRef resultRef Nothing handlerBody)
      valueBody = foldr X.ELam rolled argResolved
  pure (foldr (`X.ETyAbsRef` Nothing) valueBody sharedTypeAbsRefs)
  where
    srcTypesToElabTypesWith refs generator tys =
      go [] generator tys
      where
        go acc gen [] = Right (reverse acc, gen)
        go acc gen (ty : rest) = do
          (ty', gen') <- srcTypeToElabTypeWithScope scope refs gen ty
          go (ty' : acc) gen' rest

    freshResolvedLocals generator [] = ([], generator)
    freshResolvedLocals generator ((name, ty) : rest) =
      let (localRef, generator') = freshLocalRef name generator
          resolved = X.localResolvedVarFromRef localRef ty
          (resolvedRest, generator'') = freshResolvedLocals generator' rest
       in (resolved : resolvedRest, generator'')

    specializeHandlerShape shape =
      case matchConstructorShapeResultSubst shape of
        Just handlerSubst -> applyConstructorShapeSubst handlerSubst shape
        Nothing -> shape

    matchConstructorShapeResultSubst shape =
      typeBinderSubstFromTypeViewSubst (constructorShapeForallBinders shape)
        <$> matchTypeViewsAgainstIdentity
          scope
          Map.empty
          (NE.singleton (constructorShapeResultView shape))
          (NE.singleton (sourceTypeViewInScope scope (applyConstructorSubst subst (ctorResult ctorInfo))))

    constructorShapeResultView shape =
      TypeView
        { typeViewDisplay = constructorShapeResult shape,
          typeViewIdentity = constructorShapeResultIdentity shape,
          typeViewBinderIdentities =
            Map.fromList [(name, identity) | (name, Just identity) <- constructorShapeForallBinders shape]
        }

applyConstructorShapeSubst :: TypeBinderSubst -> ConstructorShape -> ConstructorShape
applyConstructorShapeSubst subst shape =
  let identityForallEntries =
        [ ((name, fmap (applyConstructorSubst subst) mbBound), identity)
          | ((name, mbBound), identity) <- zip (constructorShapeForallsIdentity shape) (constructorShapeForallBinderIdentities shape ++ repeat Nothing),
            maybe True (const False) (lookupTypeBinderSubst (name, identity) subst)
        ]
   in shape
        { constructorShapeForalls =
            [ (name, fmap (applyConstructorSubst subst) mbBound)
              | ((name, mbBound), identity) <- zip (constructorShapeForalls shape) (constructorShapeForallBinderIdentities shape ++ repeat Nothing),
                maybe True (const False) (lookupTypeBinderSubst (name, identity) subst)
            ],
          constructorShapeForallsIdentity = map fst identityForallEntries,
          constructorShapeForallBinderIdentities = map snd identityForallEntries,
          constructorShapeArgs = map (applyConstructorSubst subst) (constructorShapeArgs shape),
          constructorShapeArgsIdentity = map (applyConstructorSubst subst) (constructorShapeArgsIdentity shape),
          constructorShapeResult = applyConstructorSubst subst (constructorShapeResult shape),
          constructorShapeResultIdentity = applyConstructorSubst subst (constructorShapeResultIdentity shape)
        }

constructorShapeForallBinders :: ConstructorShape -> [(String, Maybe TypeBinderIdentity)]
constructorShapeForallBinders shape =
  identityEntries ++ displayEntries
  where
    identities = constructorShapeForallBinderIdentities shape ++ repeat Nothing
    identityEntries = zip (map fst (constructorShapeForallsIdentity shape)) identities
    displayEntries = zip (map fst (constructorShapeForalls shape)) identities

applyConstructorSubst :: TypeBinderSubst -> SrcType -> SrcType
applyConstructorSubst subst ty =
  Map.foldrWithKey substituteTypeVar ty (typeBinderSubstToNameMap subst)

resolveDeferredCases :: ElaborateScope -> Map DeferredRef DeferredCaseCall -> Env -> XmlfTerm -> Either ProgramError (Env, XmlfTerm)
resolveDeferredCases scope deferredCases = go
  where
    lookupDeferredCase ref =
      Map.lookup ref deferredCases

    go env term =
      case term of
        X.EVarNode {} -> Right (env, term)
        X.ELit {} -> Right (env, term)
        X.ELam resolved body -> do
          let ty = X.resolvedVarType resolved
              env' = TypeCheck.insertResolvedTermBinding resolved ty env
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env bodyEnv, X.ELam resolved body')
        X.EApp {} -> rewriteApplication env term
        X.ELet resolved scheme rhs body -> do
          let schemeTy = schemeToType scheme
              rhsEnv0 = TypeCheck.insertResolvedTermBinding resolved schemeTy env
          (rhsEnv, rhs') <- go rhsEnv0 rhs
          let baseBodyEnv = mergeCaseEnv env rhsEnv
              rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
              resolved' = X.mapResolvedVarType (const rhsTy) resolved
              env' = TypeCheck.insertResolvedTermBinding resolved' rhsTy baseBodyEnv
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env (mergeCaseEnv rhsEnv bodyEnv), X.ELet resolved' scheme rhs' body')
        X.ETyAbsRef ref mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = TypeCheck.insertTypeBindingRef ref boundTy env
          (bodyEnv, body') <- go env' body
          Right (mergeCaseEnv env bodyEnv, X.ETyAbsRef ref mbBound body')
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
       in case deferredPlaceholderHeadRef headTerm >>= lookupDeferredCase of
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
              (_scrutineeElabTy, scrutineeRawTy, scrutineeRecoveredTy) <-
                inferDeferredArgType env (deferredCaseScrutineeType deferred) scrutinee
              validateCaseScrutineeType
                (deferredCaseDataInfo deferred)
                (deferredCaseScrutineeType deferred)
                scrutineeRawTy
                scrutineeRecoveredTy
              (env', resultTy) <-
                extendCaseResultEnv
                  (deferredCaseDataInfo deferred)
                  scrutineeRawTy
                  (lowerType scope (deferredCaseResultType deferred))
                  env
              let caseHead = caseEliminator resultTy scrutinee
              Right (env', foldl X.EApp caseHead handlers)
        _ -> Left (ProgramCaseOnNonDataType STBottom)

    validateCaseScrutineeType dataInfo expectedScrutineeTy scrutineeRawTy scrutineeTy
      | Just expectedTy <- srcTypeToElabTypeMaybeInScope scope (lowerType scope expectedScrutineeTy),
        Just actualTy <- srcTypeToElabTypeMaybeInScope scope scrutineeRawTy,
        alphaEqType actualTy expectedTy || churchAwareEqType actualTy expectedTy =
          Right ()
      | Just _ <- matchDataInfoEncoding scope dataInfo scrutineeRawTy = Right ()
      | otherwise =
          let validHeadNames = Set.fromList (dataInfoHeadNames scope dataInfo)
              validHeadName name =
                name `Set.member` validHeadNames
                  || unqualifiedSymbolName name `Set.member` Set.map unqualifiedSymbolName validHeadNames
           in case scrutineeTy of
                STBase name
                  | validHeadName name -> Right ()
                STCon name _
                  | validHeadName name -> Right ()
                other -> Left (ProgramCaseOnNonDataType other)

    caseEliminator resultTy scrutinee =
      X.ETyInst (X.EUnroll scrutinee) (X.InstApp resultTy)

    inferDeferredArgType env fallbackTy arg =
      case typeCheckWithEnv env arg of
        Right ty ->
          let rawTy = elabTypeToSrcType (stripVacuousForalls ty)
           in Right (ty, rawTy, recoverSourceType scope rawTy)
        Left X.TCArgumentMismatch {} -> do
          fallbackElabTy <- srcTypeToElabTypeInScope scope (lowerType scope fallbackTy)
          Right (fallbackElabTy, fallbackTy, recoverSourceType scope fallbackTy)
        Left err ->
          Left (ProgramPipelineError ("deferred case scrutinee type check failed: " ++ show err))

    extendCaseResultEnv dataInfo scrutineeRawTy resultSrcTy env =
      case matchDataInfoEncoding scope dataInfo scrutineeRawTy of
        Just (sourceHeadTy, subst) -> do
          let resultName = "$" ++ dataInfoIdentityHeadName dataInfo ++ "_result"
              resultBindingNames =
                case Map.lookup resultName subst of
                  Just (STVar resultVar) -> Set.singleton resultVar
                  _ -> Set.empty
              selfAliasBindingNames =
                case scrutineeRawTy of
                  STMu actualSelf _ ->
                    Set.fromList
                      [ alias
                        | (alias, STVar actualSelf') <- Map.toList subst,
                          actualSelf' == actualSelf,
                          alias /= actualSelf,
                          alias /= resultName,
                          alias `notElem` dataParams dataInfo
                      ]
                  _ -> Set.empty
              loweredHeadTy = lowerType scope sourceHeadTy
              bindingNames = resultBindingNames `Set.union` selfAliasBindingNames
              sharedNames =
                Set.toList $
                  bindingNames
                    `Set.union` freeSrcTypeVars loweredHeadTy
                    `Set.union` freeSrcTypeVars resultSrcTy
                    `Set.union` freeSrcTypeVars scrutineeRawTy
                    `Set.union` foldMap freeSrcTypeVars (Map.elems subst)
              (sharedRefs, generator0) = freshTypeBinderRefs sharedNames initialIdentityGenerator
          (headTy, generator1) <- srcTypeToElabTypeWithScope scope sharedRefs generator0 loweredHeadTy
          (resultTy, _) <- srcTypeToElabTypeWithScope scope sharedRefs generator1 resultSrcTy
          let selfAliasBindings =
                Map.fromSet (const headTy) selfAliasBindingNames
              resultBinding =
                Map.fromSet (const resultTy) resultBindingNames
              bindings = selfAliasBindings `Map.union` resultBinding
          env' <- foldM (insertCaseTypeBinding sharedRefs) env (Map.toList bindings)
          Right (env', resultTy)
        Nothing -> do
          resultTy <- srcTypeToElabType resultSrcTy
          Right (env, resultTy)

    insertCaseTypeBinding refs env (name, ty) =
      case Map.lookup name refs of
        Just ref -> Right (TypeCheck.insertTypeBindingRef ref ty env)
        Nothing -> Left (ProgramPipelineError ("unresolved deferred case type alias `" ++ name ++ "`"))

    mapAccumCaseEnv env [] = Right (env, [])
    mapAccumCaseEnv env (arg : rest) = do
      (env1, arg') <- go env arg
      (env2, rest') <- mapAccumCaseEnv env1 rest
      Right (env2, arg' : rest')

    mergeCaseEnv base incoming =
      base {typeEnv = typeEnv (TypeCheck.unionEnvs incoming base)}

resolveDeferredMethods :: ElaborateScope -> Map DeferredRef DeferredMethodCall -> Env -> XmlfTerm -> Either ProgramError XmlfTerm
resolveDeferredMethods scope deferredMethods = go
  where
    lookupDeferredMethod ref =
      Map.lookup ref deferredMethods

    go env term =
      case deferredPlaceholderHeadRefWithInsts term of
        Just (ref, headInsts)
          | Just deferred <- lookupDeferredMethod ref,
            deferredMethodArgCount deferred == 0 ->
              resolveDeferredNullaryMethod headInsts deferred
        _ ->
          case term of
            X.EVarNode {} -> Right term
            X.ELit {} -> Right term
            X.ELam resolved body -> do
              let ty = X.resolvedVarType resolved
                  env' = TypeCheck.insertResolvedTermBinding resolved ty env
              X.ELam resolved <$> go env' body
            X.EApp {} -> rewriteApplication env term
            X.ELet resolved scheme rhs body -> do
              let schemeTy = schemeToType scheme
                  rhsEnv = TypeCheck.insertResolvedTermBinding resolved schemeTy env
              rhs' <- go rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  resolved' = X.mapResolvedVarType (const rhsTy) resolved
                  env' = TypeCheck.insertResolvedTermBinding resolved' rhsTy env
              body' <- go env' body
              Right (X.ELet resolved' scheme rhs' body')
            X.ETyAbsRef ref mbBound body -> do
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = TypeCheck.insertTypeBindingRef ref boundTy env
              X.ETyAbsRef ref mbBound <$> go env' body
            X.ETyInst inner inst ->
              (`X.ETyInst` inst) <$> go env inner
            X.ERoll ty body ->
              X.ERoll ty <$> go env body
            X.EUnroll inner ->
              X.EUnroll <$> go env inner

    rewriteApplication env term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHeadRef headTerm >>= lookupDeferredMethod of
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
          argViews <- mapM (inferDeferredArgType env) (take requiredArgCount args)
          classArgView <-
            case inferDeferredMethodClassArgument methodInfo argViews (deferredMethodExpectedResult deferred) of
              Just view -> Right view
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          case lookupMethodEvidence deferred methodInfo classArgView of
            Just (evidence, evidenceSubst) -> do
              methodSubst <-
                case inferMethodArgumentSubst methodInfo classArgView Map.empty argViews of
                  Just subst' -> Right subst'
                  Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
              let methodSubst' = methodSubst `Map.union` evidenceSubst
              evidenceHead <- instantiateLocalMethodEvidence scope methodSubst' evidence
              evidenceArgs <-
                resolveConstraintEvidenceTerms
                  scope
                  (deferredMethodLocalEvidence deferred)
                  Set.empty
                  (methodLocalConstraints methodInfo classArgView methodSubst')
              Right (foldl X.EApp (foldl X.EApp evidenceHead evidenceArgs) args)
            Nothing -> do
              (instanceInfo, subst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
              methodValue <- concreteMethodValue instanceInfo methodInfo
              methodSubst <-
                case inferMethodArgumentSubst methodInfo classArgView subst argViews of
                  Just subst' -> Right subst'
                  Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
              let eagerConstraints =
                    filter
                      constraintGround
                      (map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue))
              evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) Set.empty eagerConstraints
              methodHead <- instantiateMethodValue scope methodSubst methodValue
              Right (foldl X.EApp (foldl X.EApp methodHead evidenceArgs) args)

    resolveDeferredNullaryMethod headInsts deferred = do
      expectedView <-
        case deferredMethodExpectedResult deferred of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      let methodInfo = deferredMethodInfo deferred
      classArgView <-
        case inferNullaryMethodClassArgument methodInfo expectedView of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      case lookupMethodEvidence deferred methodInfo classArgView of
        Just (evidence, evidenceSubst) -> do
          methodSubst <-
            case inferNullaryMethodSubst methodInfo classArgView Map.empty expectedView of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          let methodSubst' = methodSubst `Map.union` evidenceSubst
          evidenceHead <- instantiateLocalMethodEvidence scope methodSubst' evidence
          evidenceArgs <-
            resolveConstraintEvidenceTerms
              scope
              (deferredMethodLocalEvidence deferred)
              Set.empty
              (methodLocalConstraints methodInfo classArgView methodSubst')
          let evidenceTerm = foldl X.EApp evidenceHead evidenceArgs
          Right $
            if nullaryMethodResultIsClassParameter methodInfo
              then reapplyHeadInsts headInsts evidenceTerm
              else evidenceTerm
        Nothing -> do
          (instanceInfo, subst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
          methodValue <- concreteMethodValue instanceInfo methodInfo
          methodSubst <-
            case inferNullaryMethodSubst methodInfo classArgView subst expectedView of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          let eagerConstraints =
                filter
                  constraintGround
                  (map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue))
          evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) Set.empty eagerConstraints
          methodHead <- instantiateMethodValue scope methodSubst methodValue
          Right (reapplyHeadInsts headInsts (foldl X.EApp methodHead evidenceArgs))

    inferDeferredMethodClassArgument methodInfo argViews mbExpectedResult =
      let methodTy = lowerType scope (methodTypeIdentity methodInfo)
          argIdentityTypes = map typeViewIdentity argViews
       in (sourceTypeViewInScope scope <$> inferClassArgument methodTy (methodParamIdentityName methodInfo) argIdentityTypes)
            <|> inferDeferredMethodClassArgumentFromExpected methodInfo argViews mbExpectedResult

    inferDeferredMethodClassArgumentFromExpected _ _ Nothing = Nothing
    inferDeferredMethodClassArgumentFromExpected methodInfo argViews (Just expectedView) = do
      let methodView = methodTypeView methodInfo
      substFromArgs <-
        foldM
          (\acc (templateView, actualView) -> matchTypeViewsAgainstIdentity scope acc (templateView :| []) (actualView :| []))
          Map.empty
          (zip (methodParamViews methodView) argViews)
      subst <- matchTypeViewsAgainstIdentity scope substFromArgs (methodResultTypeView methodInfo :| []) (expectedView :| [])
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

    lookupMethodEvidence deferred methodInfo classArgView =
      case localMatches of
        (methodEvidence, subst) : _ ->
          Just (mkEvidence methodEvidence, subst)
        [] ->
          case globalEvidence of
            Just methodEvidence -> Just (mkEvidence methodEvidence, Map.empty)
            Nothing -> fallbackEvidence
      where
        targetViews = classArgView :| []
        mkEvidence methodEvidence =
          DeferredMethodEvidence
            { deferredMethodEvidenceClassArg = classArgView,
              deferredMethodEvidenceClassArgs = targetViews,
              deferredMethodEvidenceMethod = methodEvidence
            }
        globalEvidence =
          lookupEvidenceMethodByClass
            scope
            (methodInfoOwnerClassSymbolIdentity methodInfo)
            (typeViewIdentity classArgView)
            (methodInfoSymbolIdentity methodInfo)
        localMatches =
          [ (methodEvidence, subst)
          | evidence <- deferredMethodLocalEvidence deferred,
            evidenceClassSymbol evidence == methodInfoOwnerClassSymbolIdentity methodInfo,
            Just subst <- [matchTypeViewsAgainstIdentity scope Map.empty (evidenceTypeViews evidence) targetViews],
            methodEvidence <- maybe [] (: []) (Map.lookup (methodInfoSymbolIdentity methodInfo) (evidenceMethodsByIdentity evidence)),
            Just _ <- [evidenceMethodResolvedVar methodEvidence]
          ]
        fallbackEvidence = do
          evidence <- deferredMethodEvidence deferred
          _ <- evidenceMethodResolvedVar (deferredMethodEvidenceMethod evidence)
          subst <- matchTypeViewsAgainstIdentity scope Map.empty (deferredMethodEvidenceClassArgs evidence) targetViews
          pure (evidence {deferredMethodEvidenceClassArg = classArgView, deferredMethodEvidenceClassArgs = targetViews}, subst)

    methodLocalConstraints methodInfo classArgView methodSubst =
      let headVars = freeTypeVarsTypeView classArgView
          classArgSubst = typeViewSubstFromParamBinders (methodParamBinders methodInfo) (classArgView :| [])
          specializedForClass =
            map
              (applyConstraintInfoSubst classArgSubst)
              (methodConstraintInfos methodInfo)
          methodLocal =
            filter
              (not . constraintDeterminedByTypeVars headVars)
              specializedForClass
       in map (applyConstraintInfoSubst methodSubst) methodLocal

    inferNullaryMethodClassArgument methodInfo expectedView
      | deferredMethodFullArityFromInfo methodInfo /= 0 = Nothing
      | otherwise = do
          subst <- matchTypeViewsAgainstIdentity scope Map.empty (methodResultTypeView methodInfo :| []) (expectedView :| [])
          NE.head <$> lookupMethodParamViewSubst methodInfo subst

    inferNullaryMethodSubst methodInfo classArgView subst expectedView =
      let specializedMethodView =
            specializeMethodTypeView methodInfo (classArgView :| [])
       in matchTypeViewsAgainstIdentity scope subst (methodResultView specializedMethodView :| []) (expectedView :| [])

    nullaryMethodResultIsClassParameter methodInfo =
      let (_, bodyTy) = splitForalls (methodType methodInfo)
          (_, resultTy) = splitArrows bodyTy
       in resultTy == STVar (methodParamName methodInfo)

    deferredMethodFullArityFromInfo methodInfo =
      length (fst (splitArrows (snd (splitForalls (methodType methodInfo)))))

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty ->
          Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls ty))
        Left err ->
          Left (ProgramPipelineError ("deferred method argument type check failed: " ++ show err))

    concreteMethodValue instanceInfo methodInfo =
      case lookupInstanceMethod methodInfo instanceInfo of
        Just valueInfo@OrdinaryValue {} -> Right valueInfo
        _ -> Left (ProgramUnknownMethod (methodName methodInfo))

    inferMethodArgumentSubst methodInfo classArgView subst argViews =
      let specializedMethodView = specializeMethodTypeView methodInfo (classArgView :| [])
       in foldM
            (\acc (templateView, actualView) -> matchTypeViewsAgainstIdentity scope acc (templateView :| []) (actualView :| []))
            subst
            (zip (methodParamViews specializedMethodView) argViews)

    methodResultView view =
      view
        { typeViewDisplay = displayResult,
          typeViewIdentity = identityResult
        }
      where
        (_, displayBodyTy) = splitForalls (typeViewDisplay view)
        (_, displayResult) = splitArrows displayBodyTy
        (_, identityBodyTy) = splitForalls (typeViewIdentity view)
        (_, identityResult) = splitArrows identityBodyTy

    methodParamViews view =
      zipWith paramView displayParamTys identityParamTys
      where
        (_, displayBodyTy) = splitForalls (typeViewDisplay view)
        (displayParamTys, _) = splitArrows displayBodyTy
        (_, identityBodyTy) = splitForalls (typeViewIdentity view)
        (identityParamTys, _) = splitArrows identityBodyTy
        paramView displayTy identityTy =
          view
            { typeViewDisplay = displayTy,
              typeViewIdentity = identityTy
            }

resolveConstraintEvidenceTerms :: ElaborateScope -> [EvidenceInfo] -> Set (SymbolIdentity, [SrcType]) -> [ConstraintInfo] -> Either ProgramError [XmlfTerm]
resolveConstraintEvidenceTerms scope localEvidence seen constraints =
  concat <$> mapM (resolveConstraintEvidenceTerm scope localEvidence seen) constraints

resolveConstraintEvidenceTerm :: ElaborateScope -> [EvidenceInfo] -> Set (SymbolIdentity, [SrcType]) -> ConstraintInfo -> Either ProgramError [XmlfTerm]
resolveConstraintEvidenceTerm scope localEvidence seen constraint = do
  let key = constraintEvidenceKey constraint
  if key `Set.member` seen
    then Left (noMatchingInstanceError scope constraint)
    else do
      mbLocalEvidence <- resolveLocalConstraintEvidenceTerms scope localEvidence constraint
      case mbLocalEvidence of
        Just evidenceTerms -> Right evidenceTerms
        Nothing -> do
          (instanceInfo, subst) <- resolveInstanceInfoByConstraint scope constraint
          let seen' = Set.insert key seen
              methodValues = ordinaryInstanceMethods instanceInfo
          if null methodValues
            then do
              _ <-
                resolveConstraintEvidenceTerms
                  scope
                  localEvidence
                  seen'
                  (map (applyConstraintInfoSubst subst) (instanceConstraintInfos instanceInfo))
              Right []
            else mapM (materializeMethodEvidence seen' subst) methodValues
  where
    ordinaryInstanceMethods instanceInfo =
      [valueInfo | valueInfo@OrdinaryValue {} <- Map.elems (instanceMethodsByIdentity instanceInfo)]

    materializeMethodEvidence seen' subst valueInfo = do
      let eagerConstraints =
            filter
              constraintGround
              (map (applyConstraintInfoSubst subst) (methodValueConstraints valueInfo))
      nestedEvidence <-
        resolveConstraintEvidenceTerms
          scope
          localEvidence
          seen'
          eagerConstraints
      methodHead <- instantiateMethodValue scope subst valueInfo
      pure (foldl X.EApp methodHead nestedEvidence)

resolveLocalConstraintEvidenceTerms :: ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Either ProgramError (Maybe [XmlfTerm])
resolveLocalConstraintEvidenceTerms scope localEvidence constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> Right Nothing
    Just classInfo
      | Map.null (classMethodsByIdentity classInfo) ->
          Right $
            if zeroMethodConstraintCoveredByEvidenceInfo scope constraint
              || zeroMethodConstraintCoveredByEvidence scope localEvidence constraint
              then Just []
              else Nothing
      | otherwise -> do
          let localMethodEvidence =
                mapM
                  ( \methodInfo -> do
                      let instantiate methodEvidence subst =
                            instantiateLocalMethodEvidence
                              scope
                              subst
                              DeferredMethodEvidence
                                { deferredMethodEvidenceClassArg = constraintTypeView constraint,
                                  deferredMethodEvidenceClassArgs = constraintTypeViews constraint,
                                  deferredMethodEvidenceMethod = methodEvidence
                                }
                      case lookupEvidenceMethodMatch scope localEvidence (constraintClassSymbol constraint) (constraintTypeViews constraint) (methodInfoSymbolIdentity methodInfo) of
                        Just (methodEvidence, evidenceSubst) ->
                          Just <$> instantiate methodEvidence evidenceSubst
                        Nothing -> do
                          case lookupEvidenceMethodByClassTypes scope (constraintClassSymbol constraint) (typeViewsIdentity (constraintTypeViews constraint)) (methodInfoSymbolIdentity methodInfo) of
                            Just methodEvidence -> Just <$> instantiate methodEvidence Map.empty
                            Nothing -> Right Nothing
                  )
                  (Map.elems (classMethodsByIdentity classInfo))
          evidenceTerms <- localMethodEvidence
          case sequence evidenceTerms of
            Nothing -> Right Nothing
            Just terms ->
              Right (Just terms)
      where
lookupEvidenceMethodMatch :: ElaborateScope -> [EvidenceInfo] -> SymbolIdentity -> NonEmpty TypeView -> SymbolIdentity -> Maybe (EvidenceMethod, TypeViewSubst)
lookupEvidenceMethodMatch scope evidenceInfos classIdentity headViews methodIdentity =
  preferredEvidenceMethodMatch
    [ (methodEvidence, subst)
      | evidence <- evidenceInfos,
        evidenceClassSymbol evidence == classIdentity,
        Just subst <- [matchTypeViewsAgainstIdentity scope Map.empty (evidenceTypeViews evidence) headViews],
        methodEvidence <- maybe [] (: []) (Map.lookup methodIdentity (evidenceMethodsByIdentity evidence))
    ]

preferredEvidenceMethodMatch :: [(EvidenceMethod, TypeViewSubst)] -> Maybe (EvidenceMethod, TypeViewSubst)
preferredEvidenceMethodMatch = go Nothing
  where
    go fallback [] = fallback
    go _ (match@(method, _) : _)
      | Just _ <- evidenceMethodResolvedVar method = Just match
    go Nothing (match : matches) = go (Just match) matches
    go fallback (_ : matches) = go fallback matches

zeroMethodConstraintCoveredByEvidence :: ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidence scope evidenceInfos constraint =
  any
    ( \evidence ->
        evidenceClassSymbol evidence == constraintClassSymbol constraint
          && case matchTypeViewsAgainstIdentity scope Map.empty (evidenceTypeViews evidence) (constraintTypeViews constraint) of
            Just _ -> True
            Nothing -> False
    )
    evidenceInfos

constraintEvidenceKey :: ConstraintInfo -> (SymbolIdentity, [SrcType])
constraintEvidenceKey constraint =
  (constraintClassSymbol constraint, NE.toList (typeViewsIdentity (constraintTypeViews constraint)))

noMatchingInstanceError :: ElaborateScope -> ConstraintInfo -> ProgramError
noMatchingInstanceError scope constraint =
  case fmap (diagnosticTypeViewDisplay scope) (constraintTypeViews constraint) of
    ty :| [] -> ProgramNoMatchingInstance (constraintDisplayClass constraint) ty
    tys -> ProgramNoMatchingInstanceHead (constraintDisplayClass constraint) (NE.toList tys)

instantiateLocalMethodEvidence :: ElaborateScope -> TypeViewSubst -> DeferredMethodEvidence -> Either ProgramError XmlfTerm
instantiateLocalMethodEvidence scope subst DeferredMethodEvidence {deferredMethodEvidenceMethod = methodEvidence} = do
  resolved <- evidenceMethodResolvedVarOrError methodEvidence
  let foralls =
        resolvedForallsMatchingSourceOrSubst
          subst
          (X.resolvedVarType resolved)
          (evidenceMethodTypeView methodEvidence)
  instantiations <-
    methodForallInstantiationsFromSourceSubst
      scope
      subst
      (evidenceMethodTypeView methodEvidence)
      foralls
  let methodTerm = X.EVarNode resolved
  pure (foldl X.ETyInst methodTerm instantiations)

evidenceMethodResolvedVarOrError :: EvidenceMethod -> Either ProgramError X.ResolvedVar
evidenceMethodResolvedVarOrError methodEvidence =
  case evidenceMethodResolvedVar methodEvidence of
    Just resolved -> Right resolved
    Nothing ->
      Left
        ( ProgramPipelineError
            ("deferred evidence method missing resolved identity `" ++ evidenceMethodRuntimeName methodEvidence ++ "`")
        )

constraintDeterminedByTypeVars :: Set String -> ConstraintInfo -> Bool
constraintDeterminedByTypeVars typeVars constraint =
  freeTypeVarsTypeViews (constraintTypeViews constraint) `Set.isSubsetOf` typeVars

constraintGround :: ConstraintInfo -> Bool
constraintGround constraint =
  Set.null (freeTypeVarsTypeViews (constraintTypeViews constraint))

methodValueConstraints :: ValueInfo -> [ConstraintInfo]
methodValueConstraints OrdinaryValue {valueConstraintInfos = constraints} = constraints
methodValueConstraints _ = []

instantiateMethodValue :: ElaborateScope -> TypeViewSubst -> ValueInfo -> Either ProgramError XmlfTerm
instantiateMethodValue scope subst valueInfo@OrdinaryValue {valueType = visibleTy, valueIdentityType = identityTy} = do
  resolved <- resolvedVarFromValueInfo valueInfo <$> typeViewToElabType scope (mkTypeView visibleTy identityTy)
  let sourceView = mkTypeView visibleTy identityTy
      foralls = resolvedForallsMatchingSourceOrSubst subst (X.resolvedVarType resolved) sourceView
  instantiations <- methodForallInstantiationsFromSourceSubst scope subst sourceView foralls
  pure (foldl X.ETyInst (X.EVarNode resolved) instantiations)
instantiateMethodValue scope _ valueInfo@ConstructorValue {valueType = visibleTy, valueIdentityType = identityTy} =
  X.EVarNode . resolvedVarFromValueInfo valueInfo <$> typeViewToElabType scope (mkTypeView visibleTy identityTy)
instantiateMethodValue _ _ OverloadedMethod {} =
  Left (ProgramPipelineError "overloaded method value reached deferred method instantiation")

methodForallInstantiationsFromSourceSubst :: ElaborateScope -> TypeViewSubst -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)] -> Either ProgramError [X.Instantiation]
methodForallInstantiationsFromSourceSubst scope subst sourceView foralls =
  methodForallInstantiations scope (resolvedForallSubst subst sourceView foralls) foralls

methodForallInstantiations :: ElaborateScope -> Map X.TypeBinderRef TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)] -> Either ProgramError [X.Instantiation]
methodForallInstantiations scope subst = go
  where
    go [] = Right []
    go ((ref, _) : rest) =
      case Map.lookup ref subst of
        Just ty -> do
          instTy <- typeViewToElabType scope ty
          (X.InstApp instTy :) <$> go rest
        Nothing
          | any ((`Map.member` subst) . fst) rest -> (X.InstElim :) <$> go rest
          | otherwise -> Right []

resolvedForallSubst :: TypeViewSubst -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)] -> Map X.TypeBinderRef TypeView
resolvedForallSubst subst sourceView foralls =
  Map.fromList
    [ (ref, ty)
    | (index, (ref, _)) <- zip [0 :: Int ..] foralls,
      Just ty <- [lookupResolvedForallSubst subst sourceView index ref]
    ]

lookupResolvedForallSubst :: TypeViewSubst -> TypeView -> Int -> X.TypeBinderRef -> Maybe TypeView
lookupResolvedForallSubst subst sourceView index ref =
  firstMatchingKey keys
    <|> firstMatchingName candidateNames
  where
    candidateNames = resolvedForallCandidateNames sourceView index ref
    keys = identityKeys candidateNames
    allowNameLookup =
      not (any (`Map.member` typeViewBinderIdentities sourceView) candidateNames)

    identityKeys names0 =
      TypeViewSubstByIdentity (X.typeBinderRefIdentity ref) (X.typeBinderRefName ref) (elabTypeBinderIdentityName ref)
        : [ TypeViewSubstByIdentity identity (X.typeBinderRefName ref) name
          | name <- names0,
            Just identity <- [Map.lookup name (typeViewBinderIdentities sourceView)]
          ]

    firstMatchingKey [] = Nothing
    firstMatchingKey (key : restKeys) =
      lookupTypeViewSubst key subst <|> firstMatchingKey restKeys

    firstMatchingName [] = Nothing
    firstMatchingName _
      | not allowNameLookup = Nothing
    firstMatchingName (name : rest) =
      lookupTypeViewSubst (TypeViewSubstByName name) subst
        <|> firstMatchingKeyName name (Map.toList subst)
        <|> firstMatchingName rest

    firstMatchingKeyName _ [] = Nothing
    firstMatchingKeyName name ((key, view) : rest)
      | keyMatchesName name key = Just view
      | otherwise = firstMatchingKeyName name rest

    keyMatchesName name key =
      case key of
        TypeViewSubstByIdentity _ displayName identityName ->
          allowNameLookup && (displayName == name || identityName == name)
        TypeViewSubstByName keyName ->
          allowNameLookup && keyName == name

resolvedForallsMatchingSourceOrSubst :: TypeViewSubst -> ElabType -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)]
resolvedForallsMatchingSourceOrSubst subst resolvedTy sourceView =
  sourceForalls ++ extraSubstPrefix
  where
    sourceCount = sourceViewForallCount sourceView
    (sourceForalls, restForalls) = splitAt sourceCount (fst (splitForallsRefs resolvedTy))
    extraSubstPrefix = prefixThroughLastSubstituted (zip [0 :: Int ..] restForalls)

    prefixThroughLastSubstituted foralls =
      case go [] [] foralls of
        [] -> []
        prefix -> reverse prefix

    go _ matched [] = matched
    go prefix matched ((index, forallEntry) : rest) =
      let prefix' = forallEntry : prefix
       in if forallEntryHasSubst index forallEntry
            then go prefix' prefix' rest
            else go prefix' matched rest

    forallEntryHasSubst index (ref, _) =
      case lookupResolvedForallSubst subst sourceView (sourceCount + index) ref of
        Just _ -> True
        Nothing -> False

sourceViewForallCount :: TypeView -> Int
sourceViewForallCount sourceView =
  max
    (length (fst (splitForalls (typeViewDisplay sourceView))))
    (length (fst (splitForalls (typeViewIdentity sourceView))))

resolvedForallCandidateNames :: TypeView -> Int -> X.TypeBinderRef -> [String]
resolvedForallCandidateNames sourceView index ref =
  dedupe
    ( sourceDisplayName
        ++ sourceIdentityName
        ++ [X.typeBinderRefName ref, elabTypeBinderIdentityName ref]
    )
  where
    sourceDisplayName = maybe [] (: []) (sourceForallNameAt (typeViewDisplay sourceView) index)
    sourceIdentityName = maybe [] (: []) (sourceForallNameAt (typeViewIdentity sourceView) index)

    dedupe = go []
      where
        go _ [] = []
        go seen (name : names)
          | name `elem` seen = go seen names
          | otherwise = name : go (name : seen) names

sourceForallNameAt :: SrcType -> Int -> Maybe String
sourceForallNameAt ty targetIndex =
  go 0 (fst (splitForalls ty))
  where
    go _ [] = Nothing
    go index ((name, _) : rest)
      | index == targetIndex = Just name
      | otherwise = go (index + 1) rest

collectElabApps :: XmlfTerm -> (XmlfTerm, [XmlfTerm])
collectElabApps = go []
  where
    go args term =
      case term of
        X.EApp fun arg -> go (arg : args) fun
        _ -> (term, args)

deferredPlaceholderHeadRef :: XmlfTerm -> Maybe DeferredRef
deferredPlaceholderHeadRef term =
  case term of
    X.EVarNode resolved -> resolvedVarDeferredRef resolved
    X.ETyInst inner _ -> deferredPlaceholderHeadRef inner
    _ -> Nothing

deferredPlaceholderHeadRefWithInsts :: XmlfTerm -> Maybe (DeferredRef, [ElabType])
deferredPlaceholderHeadRefWithInsts = go []
  where
    go insts term =
      case term of
        X.EVarNode resolved -> fmap (\ref -> (ref, insts)) (resolvedVarDeferredRef resolved)
        X.ETyInst inner (X.InstApp ty) -> go (ty : insts) inner
        X.ETyInst inner _ -> go insts inner
        _ -> Nothing

resolvedVarDeferredRef :: X.ResolvedVar -> Maybe DeferredRef
resolvedVarDeferredRef resolved =
  case X.resolvedVarDetails resolved of
    DeferredId ref -> Just ref
    _ -> Nothing

resolvedVarFromConstructorInfo :: ConstructorInfo -> X.ResolvedVar
resolvedVarFromConstructorInfo ctorInfo =
  X.ResolvedVar
    { X.resolvedVarRuntimeName = ctorRuntimeName ctorInfo,
      X.resolvedVarType = X.TBottom,
      X.resolvedVarDetails = ConstructorId (constructorRefFromInfo ctorInfo)
    }

reapplyHeadInsts :: [ElabType] -> XmlfTerm -> XmlfTerm
reapplyHeadInsts insts term =
  foldl X.ETyInst term (map X.InstApp insts)

dataInfoHeadNames :: ElaborateScope -> DataInfo -> [String]
dataInfoHeadNames scope info =
  visibleNames ++ [name | name <- identityNames, name `notElem` visibleNames]
  where
    stableName =
      symbolIdentityStableName (dataInfoSymbol info)

    visibleNames =
      nonStableVisibleNames ++ [name | name <- rawVisibleNames, name `notElem` nonStableVisibleNames]

    nonStableVisibleNames =
      filter (/= stableName) rawVisibleNames

    rawVisibleNames =
      [ name
        | (name, candidate) <- Map.toList (elaborateScopeDataTypes scope),
          sameDataIdentity candidate info
      ]

    identityNames =
      [ dataInfoIdentityName info,
        dataInfoIdentityQualifiedName info
      ]

    sameDataIdentity left right =
      dataInfoSymbol left == dataInfoSymbol right

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
      STVarApp name args -> STVarApp name (fmap recover args)
      STTyLam name body -> STTyLam name (recover body)
      STTyApp fun arg -> STTyApp (recover fun) (recover arg)

matchDataInfoEncoding :: ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncoding = matchDataInfoEncodingWith id

matchDataInfoEncodingWith :: (SrcType -> SrcType) -> ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncodingWith recover scope info ty =
  firstMatch (dataInfoHeadNames scope info)
  where
    params = dataParams info

    firstMatch [] = Nothing
    firstMatch (headName : rest) =
      case matchHeadName headName of
        Just matched -> Just matched
        Nothing -> firstMatch rest

    matchHeadName headName =
      let templateHead =
            case params of
              [] -> STBase headName
              p : ps -> STCon headName (STVar p :| map STVar ps)
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
                      [] -> STBase headName
                      arg : args -> STCon headName (arg :| args)
               in Just (recoveredHead, subst)
            Nothing -> recoverSelfNamedMu headName ty

    recoverSelfNamedMu headName actualTy =
      case actualTy of
        STMu selfName body
          | selfName `Set.member` selfNames,
            let freeVars = freeSourceTypeVars body,
            all (`Set.member` freeVars) params ->
              let recoveredHead =
                    case params of
                      [] -> STBase headName
                      param : rest -> STCon headName (STVar param :| map STVar rest)
                  subst = Map.fromList [(param, STVar param) | param <- params]
               in Just (recoveredHead, subst)
        _ -> Nothing
      where
        selfNames =
          Set.fromList
            [ "$" ++ name ++ "_self"
            | name <- dataInfoHeadNames scope info
            ]

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
    STVarApp name args ->
      matchRecoverVarApp params subst renames name args actual
    STTyLam name body ->
      case actual of
        STTyLam name' body' ->
          matchRecoverType params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STTyApp fun arg ->
      case actual of
        STTyApp fun' arg' -> do
          subst' <- matchRecoverType params subst renames fun fun'
          matchRecoverType params subst' renames arg arg'
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

matchRecoverVarApp ::
  Set String ->
  Map String SrcType ->
  Map String String ->
  String ->
  NonEmpty SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchRecoverVarApp params subst renames name args actual
  | name `Set.member` params =
      case actual of
        STCon actualName actualArgs ->
          matchAppliedHead actualName toConHead (toListNE actualArgs)
        STVarApp actualName actualArgs ->
          matchAppliedHead actualName toVarHead (toListNE actualArgs)
        _ -> Nothing
  | Just actualName <- Map.lookup name renames =
      matchRigidVarAppHead actualName
  | otherwise =
      matchRigidVarAppHead name
  where
    expectedArgs = toListNE args
    expectedArgCount = length expectedArgs

    matchAppliedHead actualName headFromPrefix actualArgs
      | length actualArgs < expectedArgCount = Nothing
      | otherwise = do
          let (headArgs, appliedArgs) = splitAt (length actualArgs - expectedArgCount) actualArgs
          subst' <- bindRecoverParam name (headFromPrefix actualName headArgs) subst
          foldM
            (\acc (leftTy, rightTy) -> matchRecoverType params acc renames leftTy rightTy)
            subst'
            (zip expectedArgs appliedArgs)

    matchRigidVarAppHead expectedName =
      case actual of
        STVarApp actualName actualArgs
          | expectedName == actualName && expectedArgCount == length (toListNE actualArgs) ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType params acc renames leftTy rightTy)
                subst
                (zip expectedArgs (toListNE actualArgs))
        _ -> Nothing

    toConHead actualName [] = STBase actualName
    toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

    toVarHead actualName [] = STVar actualName
    toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

bindRecoverParam :: String -> SrcType -> Map String SrcType -> Maybe (Map String SrcType)
bindRecoverParam name actual subst =
  case Map.lookup name subst of
    Nothing -> Just (Map.insert name actual subst)
    Just existing
      | alphaEqSrcType existing actual ->
          Just subst
      | Just existingTy <- srcTypeToElabTypeMaybe existing,
        Just actualTy <- srcTypeToElabTypeMaybe actual,
        alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
          Just subst
      | otherwise -> Nothing

stripVacuousForalls :: ElabType -> ElabType
stripVacuousForalls (X.TForallRef ref _ body)
  | not (any (X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsType body)) = stripVacuousForalls body
stripVacuousForalls (X.TForallRef ref mb body) =
  X.TForallRef ref mb (stripVacuousForalls body)
stripVacuousForalls (X.TArrow dom cod) =
  X.TArrow (stripVacuousForalls dom) (stripVacuousForalls cod)
stripVacuousForalls (X.TMuRef ref body) =
  X.TMuRef ref (stripVacuousForalls body)
stripVacuousForalls ty = ty

stripVacuousForallsAndTypeAbs :: ElabType -> XmlfTerm -> (ElabType, XmlfTerm)
stripVacuousForallsAndTypeAbs ty term =
  case (ty, term) of
    (X.TForallRef typeRef _ bodyTy, X.ETyAbsRef termRef _ body)
      | not (any (X.typeBinderRefsSameIdentity typeRef) (freeTypeVarRefsType bodyTy)),
        not (any (X.typeBinderRefsSameIdentity termRef) (freeTypeVarRefsTerm body)) ->
          stripVacuousForallsAndTypeAbs bodyTy body
    (X.TForallRef typeRef mbTy bodyTy, X.ETyAbsRef termRef mbTerm body) ->
      let (bodyTy', body') = stripVacuousForallsAndTypeAbs bodyTy body
       in (X.TForallRef typeRef mbTy bodyTy', X.ETyAbsRef termRef mbTerm body')
    _ -> (ty, term)

freeTypeVarRefsTerm :: XmlfTerm -> [X.TypeBinderRef]
freeTypeVarRefsTerm term =
  case term of
    X.EVarNode resolved ->
      freeTypeVarRefsType (X.resolvedVarType resolved)
    X.ELit {} -> []
    X.ELam resolved body ->
      unionRefs (freeTypeVarRefsType (X.resolvedVarType resolved)) (freeTypeVarRefsTerm body)
    X.EApp fun arg ->
      unionRefs (freeTypeVarRefsTerm fun) (freeTypeVarRefsTerm arg)
    X.ELet resolved scheme rhs body ->
      unionManyRefs
        [ freeTypeVarRefsType (X.resolvedVarType resolved),
          freeTypeVarRefsType (schemeToType scheme),
          freeTypeVarRefsTerm rhs,
          freeTypeVarRefsTerm body
        ]
    X.ETyAbsRef ref mb body ->
      unionRefs
        (maybe [] freeTypeVarRefsType mb)
        (filter (not . X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsTerm body))
    X.ETyInst inner inst ->
      unionRefs (freeTypeVarRefsTerm inner) (freeTypeVarRefsInstantiation inst)
    X.ERoll ty body ->
      unionRefs (freeTypeVarRefsType ty) (freeTypeVarRefsTerm body)
    X.EUnroll body ->
      freeTypeVarRefsTerm body

freeTypeVarRefsInstantiation :: X.Instantiation -> [X.TypeBinderRef]
freeTypeVarRefsInstantiation inst =
  case inst of
    X.InstId -> []
    X.InstApp ty -> freeTypeVarRefsType ty
    X.InstBot ty -> freeTypeVarRefsType ty
    X.InstIntro -> []
    X.InstElim -> []
    X.InstAbstrRef ref -> [ref]
    X.InstUnderRef ref inner -> filter (not . X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsInstantiation inner)
    X.InstInside inner -> freeTypeVarRefsInstantiation inner
    X.InstSeq left right ->
      unionRefs (freeTypeVarRefsInstantiation left) (freeTypeVarRefsInstantiation right)

unionManyRefs :: [[X.TypeBinderRef]] -> [X.TypeBinderRef]
unionManyRefs = foldr unionRefs []

unionRefs :: [X.TypeBinderRef] -> [X.TypeBinderRef] -> [X.TypeBinderRef]
unionRefs left right =
  foldr insertRef right left
  where
    insertRef ref refs
      | any (X.typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

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

elabTypeToRecoveredTypeView :: ElaborateScope -> X.Ty v -> TypeView
elabTypeToRecoveredTypeView scope ty =
  TypeView
    { typeViewDisplay = displayTy,
      typeViewIdentity = typeViewIdentity (sourceTypeViewInScope scope identityTy),
      typeViewBinderIdentities = Map.empty
    }
  where
    displayTy =
      recoverSourceType scope (elabTypeToSrcTypeWith X.typeBinderRefName ty)
    identityTy =
      recoverSourceType scope (elabTypeToIdentitySrcTypeWith elabTypeBinderIdentityName ty)

elabTypeBinderIdentityName :: X.TypeBinderRef -> String
elabTypeBinderIdentityName ref
  | "$typevar#" `isPrefixOf` X.typeBinderRefName ref = X.typeBinderRefName ref
  | otherwise = "$typevar#" ++ show (X.typeBinderIdentityKey (X.typeBinderRefIdentity ref))

elabTypeToSrcType :: X.Ty v -> SrcType
elabTypeToSrcType =
  elabTypeToSrcTypeWith X.typeBinderRefName

elabTypeToSrcTypeWith :: (X.TypeBinderRef -> String) -> X.Ty v -> SrcType
elabTypeToSrcTypeWith varName =
  elabTypeToSrcTypeWithHeads varName (\_ name -> name)

elabTypeToIdentitySrcTypeWith :: (X.TypeBinderRef -> String) -> X.Ty v -> SrcType
elabTypeToIdentitySrcTypeWith varName =
  elabTypeToSrcTypeWithHeads varName headName
  where
    headName (Just identity) _ = symbolIdentityStableName identity
    headName Nothing name = name

elabTypeToSrcTypeWithHeads :: (X.TypeBinderRef -> String) -> (Maybe SymbolIdentity -> String -> String) -> X.Ty v -> SrcType
elabTypeToSrcTypeWithHeads varName headName =
  go Map.empty
  where
    nameFor env ref =
      Map.findWithDefault (varName ref) ref env

    go :: Map X.TypeBinderRef String -> X.Ty v -> SrcType
    go env ty = case ty of
      X.TVarRef ref -> STVar (nameFor env ref)
      X.TArrow dom cod -> STArrow (go env dom) (go env cod)
      X.TBaseWithIdentity identity (Graph.BaseTy name) -> STBase (headName identity name)
      X.TConWithIdentity identity (Graph.BaseTy name) args ->
        case toListNE (fmap (go env) args) of
          x : xs -> STCon (headName identity name) (x :| xs)
          [] -> STBase (headName identity name)
      X.TVarAppRef ref args -> STVarApp (nameFor env ref) (fmap (go env) args)
      X.TForallRef ref mb body ->
        let name = varName ref
         in STForall name (fmap (SrcBound . go env) mb) (go (Map.insert ref name env) body)
      X.TMuRef ref body ->
        let name = varName ref
         in STMu name (go (Map.insert ref name env) body)
      X.TBottom -> STBottom

srcTypeToElabType :: SrcTy n v -> Either ProgramError ElabType
srcTypeToElabType ty =
  let (refs, generator) = freshTypeBinderRefs (Set.toList (freeSrcTypeVars ty)) initialIdentityGenerator
   in fst <$> srcTypeToElabTypeWithHeadIdentities builtinTypeHeadIdentities refs generator ty

srcTypeToElabTypeInScope :: ElaborateScope -> SrcTy n v -> Either ProgramError ElabType
srcTypeToElabTypeInScope scope ty =
  let (refs, generator) = freshTypeBinderRefs (Set.toList (freeSrcTypeVars ty)) initialIdentityGenerator
   in fst <$> srcTypeToElabTypeWithHeadIdentities (typeHeadIdentitiesInScope scope) refs generator ty

srcTypeToElabTypeMaybeInScope :: ElaborateScope -> SrcTy n v -> Maybe ElabType
srcTypeToElabTypeMaybeInScope scope =
  either (const Nothing) Just . srcTypeToElabTypeInScope scope

typeViewToElabType :: ElaborateScope -> TypeView -> Either ProgramError ElabType
typeViewToElabType scope =
  srcTypeToElabTypeInScope scope . lowerTypeView scope

freshTypeBinderRefs :: [String] -> IdentityGenerator -> (Map String X.TypeBinderRef, IdentityGenerator)
freshTypeBinderRefs names generator0 =
  foldr fresh (Map.empty, generator0) names
  where
    fresh name (refs, generator) =
      let (ref, generator') = X.freshTypeBinderRef name generator
       in (Map.insert name ref refs, generator')

srcTypeToElabTypeWithScope :: ElaborateScope -> Map String X.TypeBinderRef -> IdentityGenerator -> SrcTy n v -> Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithScope scope =
  srcTypeToElabTypeWithHeadIdentities (typeHeadIdentitiesInScope scope)

srcTypeToElabTypeWithHeadIdentities ::
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithHeadIdentities headIdentities refs generator ty = case ty of
  STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (X.TVarRef ref, generator)
  STArrow dom cod ->
    do
      (dom', generator1) <- srcTypeToElabTypeWithHeadIdentities headIdentities refs generator dom
      (cod', generator2) <- srcTypeToElabTypeWithHeadIdentities headIdentities refs generator1 cod
      Right (X.TArrow dom' cod', generator2)
  STBase name ->
    Right (X.TBaseWithIdentity (Map.lookup name headIdentities) (Graph.BaseTy (Builtins.normalizeBuiltinTypeReference name)), generator)
  STCon name args ->
    do
      (args', generator') <- mapAccumSrcTypes refs generator args
      Right (X.TConWithIdentity (Map.lookup name headIdentities) (Graph.BaseTy (Builtins.normalizeBuiltinTypeReference name)) args', generator')
  STVarApp name args ->
    do
      (args', generator') <- mapAccumSrcTypes refs generator args
      ref <- sourceTypeBinderRef refs name
      Right (X.TVarAppRef ref args', generator')
  STTyLam {} ->
    Left (ProgramPipelineError "residual type lambda reached finalization")
  STTyApp {} ->
    Left (ProgramPipelineError "residual type application reached finalization")
  STForall name mb body ->
    let (ref, generator1) = X.freshTypeBinderRef name generator
        refs' = Map.insert name ref refs
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithHeadIdentities headIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithHeadIdentities headIdentities refs' generator2 body
          Right (X.TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = X.freshTypeBinderRef name generator
     in do
          (body', generator2) <- srcTypeToElabTypeWithHeadIdentities headIdentities (Map.insert name ref refs) generator1 body
          Right (X.TMuRef ref body', generator2)
  STBottom ->
    Right (X.TBottom, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (ProgramPipelineError ("unresolved source type binder `" ++ name ++ "` reached finalization"))

    mapAccumSrcTypes refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWithHeadIdentities headIdentities refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWithHeadIdentities headIdentities refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')


srcTypeToElabTypeMaybe :: SrcTy n v -> Maybe ElabType
srcTypeToElabTypeMaybe =
  either (const Nothing) Just . srcTypeToElabType

srcBoundToElabBoundWithHeadIdentities ::
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcBound n ->
  Either ProgramError (Maybe X.BoundType, IdentityGenerator)
srcBoundToElabBoundWithHeadIdentities headIdentities refs generator (SrcBound boundTy) =
  case srcTypeToElabTypeWithHeadIdentities headIdentities refs generator boundTy of
    Left err -> Left err
    Right (X.TVarRef {}, generator') -> Right (Nothing, generator')
    Right (X.TBottom, generator') -> Right (Nothing, generator')
    Right (X.TArrow dom cod, generator') -> Right (Just (X.TArrow dom cod), generator')
    Right (X.TBaseWithIdentity identity base, generator') -> Right (Just (X.TBaseWithIdentity identity base), generator')
    Right (X.TConWithIdentity identity con args, generator') -> Right (Just (X.TConWithIdentity identity con args), generator')
    Right (X.TVarAppRef ref args, generator') -> Right (Just (X.TVarAppRef ref args), generator')
    Right (X.TForallRef ref mb body, generator') -> Right (Just (X.TForallRef ref mb body), generator')
    Right (X.TMuRef ref body, generator') -> Right (Just (X.TMuRef ref body), generator')

typeHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity
typeHeadIdentitiesInScope scope =
  Map.unions
    [ Map.map dataInfoSymbol dataTypes,
      unambiguousDataTypeHeadIdentities dataTypes,
      builtinTypeHeadIdentities
    ]
  where
    dataTypes = elaborateScopeDataTypes scope

unambiguousDataTypeHeadIdentities :: Map String DataInfo -> Map String SymbolIdentity
unambiguousDataTypeHeadIdentities dataTypes =
  Map.fromList
    [ (name, identity)
    | (name, identities) <- Map.toList identitiesByHeadName,
      [identity] <- [Set.toList identities]
    ]
  where
    identitiesByHeadName =
      Map.fromListWith
        Set.union
        [ (dataInfoIdentityName info, Set.singleton (dataInfoSymbol info))
        | info <- Map.elems dataTypes
        ]

builtinTypeHeadIdentities :: Map String SymbolIdentity
builtinTypeHeadIdentities =
  Map.fromList
    [ key
    | name <- Set.toList Builtins.builtinTypeNames,
      identity <- [Builtins.builtinTypeIdentity name],
      key <- [(name, identity), (symbolIdentityStableName identity, identity)]
    ]
