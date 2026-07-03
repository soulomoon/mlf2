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
    sourceForallMatchesInScope,
    stripVacuousForallsAndTypeAbs,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (filterM, foldM, zipWithM)
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
    extendPreparedExternalBindingTypeIdentities,
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
import MLF.Frontend.ConstraintGen (ExternalBinding (..), ExternalBindingIdentity, ExternalBindingMode (..), externalBindingIdentityFromDetails)
import MLF.Frontend.Normalize (normalizeExpr, normalizeType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol (lookupSymbolIdentityAlias, symbolIdentityAliasMap, symbolIdentityAliasNames, symbolIdentityStableName)
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeDataTypesByIdentity,
    elaborateScopeRuntimeTypeViews,
    elaborateScopeRuntimeTypes,
    elaborateScopeUniqueDataTypes,
    elaborateScopeValues,
    classInfoForConstraint,
    constructorBindingSourceTypeView,
    constructorTypeView,
    diagnosticTypeViewDisplay,
    lookupEvidenceMethodByClass,
    lookupEvidenceMethodByClassTypes,
    lowerType,
    lowerTypeView,
    matchTypesInScope,
    matchMethodTypeViews,
    matchTypeViewsAgainstIdentity,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    sourceTypeBinderIdentitiesInScope,
    sourceTypeViewInScope,
    zeroMethodConstraintCoveredByEvidenceInfo,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    ConstructorForallBinder (..),
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
    LoweredResolvedLocalIdentity (..),
    MethodInfo (..),
    ProgramError (..),
    ConstraintInfo (..),
    TypeView (..),
    TypeBinderSubst,
    TypeViewSubst,
    ValueInfo (..),
    applyConstraintInfoSubst,
    applyTypeBinderSubst,
    constructorRefFromInfo,
    constructorRefSymbol,
    constructorOwnerRuntimeTypeTrackable,
    constructorOwnerShapes,
    constructorShapeFromInfo,
    constructorShapeForalls,
    constructorShapeForallsIdentity,
    constructorShapeArgs,
    constructorShapeArgsIdentity,
    constructorShapeName,
    constructorShapeResult,
    constructorShapeResultIdentity,
    constructorInfoIdentityName,
    dataInfoIdentityHeadName,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataParams,
    dataParamBinders,
    deferredCasePlaceholder,
    deferredConstructorPlaceholder,
    deferredMethodPlaceholder,
    deferredMethodName,
    deferredProgramObligationRef,
    emptyTypeBinderSubst,
    freeTypeBinderIdentitiesTypeViews,
    constraintTypeView,
    lookupInstanceMethod,
    ctorName,
    ctorForalls,
    ctorArgs,
    ctorResult,
    lookupTypeViewSubst,
    lookupMethodParamViewSubst,
    methodType,
    methodTypeView,
    methodResultTypeView,
    methodName,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    methodParamBinderIdentities,
    mergeTypeBinderIdentityMaps,
    mergeSymbolIdentityMaps,
    loweredBindingConstructorRef,
    loweredIdentityDetails,
    loweredBindingName,
    resolvedVarFromLoweredBinding,
    resolvedVarFromValueInfo,
    ordinaryValueTypeView,
    SymbolIdentity,
    symbolDefiningName,
    splitArrows,
    splitForalls,
    specializeMethodTypeView,
    typeViewBinderIdentityForAlias,
    substituteTypeVar,
    typeViewSubstFromParamIdentities,
    typeViewHeadIdentityForAlias,
    typeViewVarPairs,
    typeHeadNamesSrcType,
    typeViewSubstKeyForIdentity,
    typeParamBinderIdentity,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToTypeViewSubstWith,
    typeBinderAliasIdentityMap,
    typeViewsIdentity,
    lookupTypeBinderSubstByIdentity,
    insertTypeBinderSubstWithIdentity,
    unqualifiedSymbolName,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), NormSurfaceExpr, SrcBound (..), SrcTy (..), SrcType, SurfaceExpr, typeParamName)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, freshNameLike, splitForallsRefs, substTypeCaptureRef)
import MLF.Types.Identity
  ( DeferredRef,
    deferredRefIdentity,
    deferredRefName,
    envRefIdentity,
    IdentityGenerator,
    LocalIdentity (..),
    LocalRef,
    localRefGeneratedIdentities,
    localRefIdentity,
    primitiveRefSymbol,
    TypeBinderIdentity,
    UniqueIdentity,
    freshDeferredRef,
    freshEnvRef,
    freshLocalRef,
    idDetailsGeneratedIdentities,
    identityGeneratorAfter,
    renameDeferredRef,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
    typeBinderIdentityStableName,
    uniqueIdentityStableName,
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
  = ModuleBindingReadLocal LocalIdentity
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
  let runtimeTypeViews = elaborateScopeRuntimeTypeViews scope
      runtimeSourceTypes = Map.map typeViewDisplay runtimeTypeViews
  runtimeTypeEnv <- traverse (srcTypeToElabTypeInScope scope) runtimeSourceTypes
  let runtimeIndex = runtimeExternalBindingIndexFromScope scope runtimeTypeEnv
  runtimeBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      scope
      -- Runtime siblings already carry resolved SchemeInfo in the prepared
      -- elaboration/typecheck env. Function-shaped constrained siblings must not
      -- get a second graph scheme binder identity space.
      (externalBindingModeForRuntime runtimeSourceTypes)
      (runtimeExternalBindingIdentity runtimeIndex)
      runtimeTypeViews
  pure
    FinalizeContext
      { finalizeContextScope = scope,
        finalizeContextRuntimeBindings = runtimeBindings,
        finalizeContextRuntimeTypeEnv = runtimeTypeEnv
      }

mkModuleFinalizeContext :: FinalizeContext -> [LoweredBinding] -> Either ProgramError ModuleFinalizeContext
mkModuleFinalizeContext context lowereds0 = do
  mapM_ validateLoweredBindingDeferredObligations lowereds0
  let lowereds = stampLoweredBindingsDeferredIdentities lowereds0
      schemeExternalTypeViews = Map.unions (map loweredBindingExternalTypeViews lowereds)
      schemeExternalTypes = Map.map typeViewDisplay schemeExternalTypeViews
      schemeDeferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      schemeDeferredIndex = deferredExternalBindingIndex schemeDeferredObligations
  schemeExternalBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      (finalizeContextScope context)
      (const ExternalBindingScheme)
      (deferredExternalBindingIdentity schemeDeferredIndex)
      (lowerExternalTypeViews (finalizeContextScope context) schemeExternalTypeViews)
  let keyedBindingRead lowered = do
        key <- loweredBindingReadKey lowered
        pure (key, mkModuleBindingReadContext context schemeExternalTypes schemeExternalBindings lowered)
  bindingReads <-
    traverse keyedBindingRead lowereds
  if Set.size (Set.fromList (map fst bindingReads)) == length bindingReads
    then pure ()
    else Left (ProgramPipelineError "module finalize context received duplicate binding identities")
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
              (finalizeContextScope context)
              externalBindingModeFor
              (deferredExternalBindingIdentity deferredExternalIndex)
              (lowerExternalTypeViews (finalizeContextScope context) (Map.restrictKeys externalTypeViews0 overlayExternalFreeVars))
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
    externalTypeViews0 = loweredBindingExternalTypeViews lowered
    externalTypes = Map.map typeViewDisplay externalTypeViews0
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
    expectedType = loweredExpectedTypeToElabType scope lowered

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
  validateDeferredObligationIdentities (loweredBindingName lowered0) (loweredBindingDeferredObligations lowered0)
  let lowered = stampLoweredBindingDeferredIdentities lowered0
  PipelineElabDetailedResult {pedTerm = term0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipelineWithContext
      context
      True
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypeViews lowered)
      (loweredBindingSurfaceExpr lowered)
  term <- finalizeOpaqueDeferredConstructors context (loweredBindingDeferredObligations lowered) tcEnv term0
  let resolvedTerm = annotateResolvedTermVars context lowered term
      resolvedDeferredObligations =
        annotateDeferredEvidenceResolvedVars resolvedTerm (loweredBindingDeferredObligations lowered)
  let sourceTypeView =
        sourceTypeViewForLoweredBinding context lowered
  validateDeferredObligationIdentities (loweredBindingName lowered) resolvedDeferredObligations
  Right
    CheckedBinding
      { checkedBindingResolvedVar = resolvedVarFromLoweredBinding lowered placeholderTy,
        checkedBindingSourceTypeView = sourceTypeView,
        checkedBindingSurfaceExpr = loweredBindingSurfaceExpr lowered,
        checkedBindingDeferredObligations = resolvedDeferredObligations,
        checkedBindingTerm = resolvedTerm,
        checkedBindingType = placeholderTy,
        checkedBindingExportedAsMain = loweredBindingExportedAsMain lowered
      }

sourceTypeViewForLoweredBinding :: FinalizeContext -> LoweredBinding -> TypeView
sourceTypeViewForLoweredBinding context lowered =
  case loweredBindingSourceTypeView lowered of
    Just view -> view
    Nothing ->
      case lookupConstructorBindingRuntime scope lowered of
        Just (_, ctorInfo) -> constructorBindingSourceTypeView scope ctorInfo
        Nothing -> sourceTypeViewInScope scope (loweredBindingSourceType lowered)
  where
    scope = finalizeContextScope context

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
      Map.withoutKeys (Map.map typeViewDisplay (loweredBindingExternalTypeViews lowered)) Builtins.builtinOpaqueValueNames
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
            alphaEqSrcTypeInScope scope actualTy annTy
              || alphaEqSrcTypeInScope scope (lowerType scope actualTy) (lowerType scope annTy)
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
  alphaEqSrcTypeInScope scope actualTy expectedTy
    || alphaEqSrcTypeInScope scope (lowerType scope actualTy) (lowerType scope expectedTy)
    || sourceTypeMatchesWithRigid rigidVars scope expectedTy actualTy
    || sourceForallMatchesWithRigidForallsInScope scope expectedTy actualTy

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
  validateDeferredObligationIdentities (loweredBindingName lowered0) (loweredBindingDeferredObligations lowered0)
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
      (loweredBindingExternalTypeViews lowered)
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
  validateLoweredBindingDeferredObligations lowered0
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
  case validateDeferredObligationIdentities (loweredBindingName lowered0) (loweredBindingDeferredObligations lowered0) of
    Left err -> pure (Left err)
    Right () -> do
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
                (loweredBindingExternalTypeViews lowered)
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
  case validateLoweredBindingDeferredObligations lowered0 of
    Left err -> pure (Left err)
    Right () -> do
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
  term0 <- inlineConstructorHead scope Map.empty (constructorBindingQuantifiedOwnerParams lowered dataInfo) ctorInfo emptyTypeBinderSubst
  let term = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (schemeFromType expectedTy) term0
  Right (term, expectedTy)
  where
    scope = finalizeContextScope context

constructorMetadataFastPathSupported :: ElaborateScope -> DataInfo -> ConstructorInfo -> Bool
constructorMetadataFastPathSupported scope dataInfo ctorInfo =
  null (ctorForalls ctorInfo)
    && dataInfoSymbol dataInfo == ctorOwningTypeIdentity ctorInfo
    && constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo

constructorBindingQuantifiedOwnerParams :: LoweredBinding -> DataInfo -> [(String, TypeBinderIdentity)]
constructorBindingQuantifiedOwnerParams lowered dataInfo =
  filter quantifiedOwnerParam (dataParamBinders dataInfo)
  where
    quantifiedNames =
      Set.fromList (map fst (fst (splitForalls (loweredBindingExpectedType lowered))))

    quantifiedOwnerParam (name, identity) =
      name `Set.member` quantifiedNames
        || typeBinderIdentityStableName identity `Set.member` quantifiedNames

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
  mapM_ validateLoweredBindingDeferredObligations lowereds0
  let lowereds =
        stampLoweredBindingsDeferredIdentities $
          zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
      deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
      groupExpr = groupedBindingExpr lowereds
  PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
    runSurfacePipelineWithContext context False deferredObligations externalTypeViews0 groupExpr
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
    fromProgramEither (mapM_ validateLoweredBindingDeferredObligations lowereds0)
    let lowereds =
          stampLoweredBindingsDeferredIdentities $
            zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
        deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
        externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
        groupExpr = groupedBindingExpr lowereds
    PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} <-
      timeFinalizeEither timing (label ++ ".pipeline") $
        runSurfacePipelineWithContextWithTiming timing (label ++ ".pipeline") context False deferredObligations externalTypeViews0 groupExpr
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
        , loweredBindingExternalTypeViews =
            Map.mapKeys renameName (loweredBindingExternalTypeViews lowered)
        }

renameSurfaceVars :: (String -> String) -> SurfaceExpr -> SurfaceExpr
renameSurfaceVars renameName =
  go Set.empty
  where
    go bound expr =
      case expr of
        EVar name
          | name `Set.member` bound -> EVar name
          | otherwise -> EVar (renameName name)
        ELit {} -> expr
        ELam name body -> ELam name (go (Set.insert name bound) body)
        EApp fun arg -> EApp (go bound fun) (go bound arg)
        ELet name rhs body ->
          let bound' = Set.insert name bound
           in ELet name (go bound' rhs) (go bound' body)
        ELamAnn name ty body -> ELamAnn name ty (go (Set.insert name bound) body)
        EAnn inner ty -> EAnn (go bound inner) ty

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
      identityGeneratorAfter (concatMap generatedIdentitiesInLoweredBinding lowereds)

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
        let sourceTypeView =
              sourceTypeViewForLoweredBinding context lowered
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
                    checkedBindingSourceTypeView = sourceTypeView,
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
                alphaEqSrcTypeInScope scope recoveredExpectedSrcTy recoveredActualSrcTy
                  || alphaEqSrcTypeInScope scope (lowerType scope recoveredExpectedSrcTy) (lowerType scope recoveredActualSrcTy)
                  || if Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered)
                    then sourceForallMatchesWithRigidForallsInScope scope recoveredExpectedSrcTy recoveredActualSrcTy
                    else sourceForallMatchesInScope scope recoveredExpectedSrcTy recoveredActualSrcTy
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
      alphaEqSrcTypeInScope scope expectedSrc actualSrc
        || alphaEqSrcTypeInScope scope (lowerType scope expectedSrc) (lowerType scope actualSrc)
        || if Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered)
          then sourceForallMatchesWithRigidForallsInScope scope expectedSrc actualSrc
          else sourceForallMatchesInScope scope expectedSrc actualSrc

    bindingCheckExpectedTypeFor lowered0 =
      case mbCheckContext of
        Just checkContext -> bindingCheckExpectedType checkContext
        Nothing -> loweredExpectedTypeToElabType scope lowered0

    bindingCheckExpectedTypeForCompareFor lowered0 =
      case mbCheckContext of
        Just checkContext -> bindingCheckExpectedTypeForCompare checkContext
        Nothing -> stripVacuousForalls <$> loweredExpectedTypeToElabType scope lowered0

    checkedBindingTypeForStorage lowered0 acceptedTy0 = do
      expectedTy <- bindingCheckExpectedTypeFor lowered0
      let recoveredActualSrcTy =
            recoverSourceType scope (elabTypeToSrcType (stripVacuousForalls acceptedTy0))
          recoveredExpectedSrcTy =
            recoverSourceType scope (bindingCheckRecoveredExpectedSourceTypeFor lowered0)
          keepExpected =
            acceptedTy0 == expectedTy
              || alphaEqType acceptedTy0 expectedTy
              || churchAwareEqType acceptedTy0 expectedTy
              || recoveredSourceTypesCompatible recoveredExpectedSrcTy recoveredActualSrcTy
      pure $
        if keepExpected
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
               in alphaEqSrcTypeInScope scope sourceTy targetTy
                    || alphaEqSrcTypeInScope scope sourceTy' targetTy'
                    || sourceForallMatchesWithRigidForallsInScope scope targetTy sourceTy
                    || sourceForallMatchesWithRigidForallsInScope scope targetTy' sourceTy'
            Nothing -> False
        Nothing -> False

    runtimeSourceTypes =
      Map.map typeViewDisplay (loweredBindingExternalTypeViews lowered) `Map.union` elaborateScopeRuntimeTypes scope

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
validateDeferredObligationIdentities bindingName obligations =
  mapM_ validateEntry (Map.toList obligations)
  where
    validateEntry (expectedRef, obligation)
      | expectedRef == actualRef =
          Right ()
      | otherwise =
          Left
            ( ProgramPipelineError
                ( "checked binding `"
                    ++ bindingName
                    ++ "` has mismatched deferred obligation identity: map key "
                    ++ deferredRefLabel expectedRef
                    ++ ", payload "
                    ++ deferredRefLabel actualRef
                )
            )
      where
        actualRef = deferredProgramObligationRef obligation

    deferredRefLabel ref =
      deferredRefName ref ++ "#" ++ uniqueIdentityStableName (deferredRefIdentity ref)

validateLoweredBindingDeferredObligations :: LoweredBinding -> Either ProgramError ()
validateLoweredBindingDeferredObligations lowered =
  validateDeferredObligationIdentities (loweredBindingName lowered) (loweredBindingDeferredObligations lowered)

annotateResolvedTermVars :: FinalizeContext -> LoweredBinding -> XmlfTerm -> XmlfTerm
annotateResolvedTermVars _context lowered term0 =
  annotateResolvedTermVarsWithEvidenceCounts
    Map.empty
    (loweredBindingEvidenceParamCount lowered)
    (loweredBindingResolvedLocalIdentities lowered)
    (X.generatedIdentitiesInTerm term0 ++ generatedIdentitiesInDeferredObligations lowered)
    term0

annotateResolvedTermVarsForGroup :: FinalizeContext -> [LoweredBinding] -> DeferredObligations -> XmlfTerm -> XmlfTerm
annotateResolvedTermVarsForGroup _context lowereds deferredObligations term0 =
  annotateResolvedTermVarsWithEvidenceCounts
    evidenceCountsByBinding
    0
    resolvedLocalIdentities
    (X.generatedIdentitiesInTerm term0 ++ generatedIdentitiesInDeferredObligationsMap deferredObligations)
    term0
  where
    resolvedLocalIdentities =
      concatMap loweredBindingResolvedLocalIdentities lowereds

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

annotateResolvedTermVarsWithEvidenceCounts :: Map ModuleBindingReadKey Int -> Int -> [LoweredResolvedLocalIdentity] -> [UniqueIdentity] -> XmlfTerm -> XmlfTerm
annotateResolvedTermVarsWithEvidenceCounts evidenceCountsByBinding initialEvidenceParamCount resolvedLocalIdentities generatedIdentities term0 =
  let (term, _, _) = go Map.empty initialEvidenceParamCount initialGenerator term0
   in term
  where
    initialGenerator =
      identityGeneratorAfter
        ( generatedIdentities
            ++ concatMap generatedIdentitiesInLoweredResolvedLocalIdentity resolvedLocalIdentities
        )

    resolvedLocalIdentityOverrides =
      collectResolvedLocalIdentityOverrides resolvedLocalIdentities term0

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
      X.resolvedVarLocalRef resolved >>= (`Map.lookup` locals)

    insertLocalIdentity original resolved =
      case X.resolvedVarLocalRef original of
        Just localRef -> Map.insert localRef resolved
        Nothing -> id

    freshenLocalResolvedVar allowEvidence evidenceParamsLeft generator resolved
      | X.resolvedVarIsLocal resolved =
          let isEvidenceParam =
                allowEvidence && evidenceParamsLeft > 0
              mbResolvedLocalRef =
                if isEvidenceParam
                  then Nothing
                  else Map.lookup (X.resolvedVarIdentityKey resolved) resolvedLocalIdentityOverrides
              (localRef, generator') =
                case mbResolvedLocalRef of
                  Just ref -> (ref, generator)
                  Nothing -> freshLocalRef (X.resolvedVarReferenceName resolved) generator
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

collectResolvedLocalIdentityOverrides :: [LoweredResolvedLocalIdentity] -> XmlfTerm -> Map X.ResolvedTermIdentityKey LocalRef
collectResolvedLocalIdentityOverrides resolvedLocalIdentities =
  fst . go resolvedLocalIdentities
  where
    go overrides term =
      case term of
        X.EVarNode {} -> (Map.empty, overrides)
        X.ELit {} -> (Map.empty, overrides)
        X.ELam resolved body ->
          let (entry, overrides') = resolvedLocalEntry resolved overrides
              (bodyEntries, overrides'') = go overrides' body
           in (entry <> bodyEntries, overrides'')
        X.EApp fun arg ->
          let (funEntries, overrides') = go overrides fun
              (argEntries, overrides'') = go overrides' arg
           in (funEntries <> argEntries, overrides'')
        X.ELet resolved _ rhs body ->
          let (entry, overrides') = resolvedLocalEntry resolved overrides
              (rhsEntries, overrides'') = go overrides' rhs
              (bodyEntries, overrides''') = go overrides'' body
           in (entry <> rhsEntries <> bodyEntries, overrides''')
        X.ETyAbsRef _ _ body ->
          go overrides body
        X.ETyInst inner _ ->
          go overrides inner
        X.ERoll _ body ->
          go overrides body
        X.EUnroll body ->
          go overrides body

    resolvedLocalEntry resolved overrides
      | X.resolvedVarIsLocal resolved,
        let runtimeName = X.resolvedVarRuntimeName resolved,
        (before, match : after) <- break ((== runtimeName) . loweredResolvedLocalRuntimeName) overrides =
          ( Map.singleton (X.resolvedVarIdentityKey resolved) (loweredResolvedLocalRef match),
            before ++ after
          )
      | otherwise =
          (Map.empty, overrides)

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

generatedIdentitiesInLoweredBinding :: LoweredBinding -> [UniqueIdentity]
generatedIdentitiesInLoweredBinding lowered =
  idDetailsGeneratedIdentities (loweredIdentityDetails (loweredBindingIdentity lowered))
    ++ maybe [] generatedIdentitiesInTypeView (loweredBindingSourceTypeView lowered)
    ++ maybe [] generatedIdentitiesInTypeView (loweredBindingExpectedTypeView lowered)
    ++ concatMap generatedIdentitiesInLoweredResolvedLocalIdentity (loweredBindingResolvedLocalIdentities lowered)
    ++ generatedIdentitiesInDeferredObligations lowered
    ++ concatMap generatedIdentitiesInTypeView (Map.elems (loweredBindingExternalTypeViews lowered))

generatedIdentitiesInLoweredResolvedLocalIdentity :: LoweredResolvedLocalIdentity -> [UniqueIdentity]
generatedIdentitiesInLoweredResolvedLocalIdentity =
  localRefGeneratedIdentities . loweredResolvedLocalRef

generatedIdentitiesInTypeView :: TypeView -> [UniqueIdentity]
generatedIdentitiesInTypeView view =
  concatMap symbolGeneratedIdentities (Map.elems (typeViewHeadIdentities view))
    ++ concatMap typeBinderGeneratedIdentities (Map.elems (typeViewBinderIdentities view))

generatedIdentitiesInDeferredObligations :: LoweredBinding -> [UniqueIdentity]
generatedIdentitiesInDeferredObligations lowered =
  generatedIdentitiesInDeferredObligationsMap (loweredBindingDeferredObligations lowered)

generatedIdentitiesInDeferredObligationsMap :: DeferredObligations -> [UniqueIdentity]
generatedIdentitiesInDeferredObligationsMap obligations =
  concatMap generatedIdentitiesInObligation (Map.elems obligations)
  where
    generatedIdentitiesInObligation obligation =
      idDetailsGeneratedIdentities (DeferredId (deferredProgramObligationRef obligation))
        ++ case obligation of
          DeferredMethod deferred ->
            generatedIdentitiesInMethodInfo (deferredMethodInfo deferred)
              ++ maybe [] generatedIdentitiesInTypeView (deferredMethodExpectedResult deferred)
              ++ maybe [] generatedIdentitiesInDeferredEvidence (deferredMethodEvidence deferred)
              ++ concatMap generatedIdentitiesInEvidenceInfo (deferredMethodLocalEvidence deferred)
          DeferredConstructor deferred ->
            generatedIdentitiesInConstructorInfo (deferredConstructorInfo deferred)
              ++ concatMap symbolGeneratedIdentities (Map.elems (deferredConstructorTypeHeadIdentities deferred))
              ++ concatMap (typeBinderGeneratedIdentities . snd) (deferredConstructorInstBinders deferred)
          DeferredCase deferred ->
            generatedIdentitiesInDataInfo (deferredCaseDataInfo deferred)

    generatedIdentitiesInDeferredEvidence evidence =
      generatedIdentitiesInTypeView (deferredMethodEvidenceClassArg evidence)
        ++ foldMap generatedIdentitiesInTypeView (deferredMethodEvidenceClassArgs evidence)
        ++ generatedIdentitiesInEvidenceMethod (deferredMethodEvidenceMethod evidence)

    generatedIdentitiesInEvidenceInfo evidence =
      symbolGeneratedIdentities (evidenceClassSymbol evidence)
        ++ foldMap generatedIdentitiesInTypeView (evidenceTypeViews evidence)
        ++ concatMap generatedIdentitiesInEvidenceMethod (Map.elems (evidenceMethodsByIdentity evidence))

    generatedIdentitiesInEvidenceMethod method =
      symbolGeneratedIdentities (evidenceMethodSymbol method)
        ++ maybe [] generatedIdentitiesInResolvedVar (evidenceMethodResolvedVar method)
        ++ generatedIdentitiesInTypeView (evidenceMethodTypeView method)

    generatedIdentitiesInResolvedVar resolved =
      idDetailsGeneratedIdentities (X.resolvedVarDetails resolved)
        ++ X.generatedIdentitiesInType (X.resolvedVarType resolved)

    generatedIdentitiesInMethodInfo info =
      symbolGeneratedIdentities (methodInfoSymbol info)
        ++ generatedIdentitiesInTypeView (methodTypeViewRaw info)
        ++ concatMap generatedIdentitiesInConstraintInfo (methodConstraintInfos info)
        ++ foldMap typeBinderGeneratedIdentities (methodParamBinderIdentities info)

    generatedIdentitiesInConstraintInfo info =
      symbolGeneratedIdentities (constraintClassSymbol info)
        ++ foldMap generatedIdentitiesInTypeView (constraintTypeViews info)

    generatedIdentitiesInConstructorInfo info =
      symbolGeneratedIdentities (ctorInfoSymbol info)
        ++ symbolGeneratedIdentities (ctorOwningTypeIdentity info)
        ++ concatMap generatedIdentitiesInConstructorForallBinder (ctorForallBinderInfo info)
        ++ concatMap generatedIdentitiesInConstructorShape (ctorOwnerConstructors info)

    generatedIdentitiesInConstructorShape shape =
      symbolGeneratedIdentities (constructorShapeSymbol shape)
        ++ concatMap generatedIdentitiesInConstructorForallBinder (constructorShapeForallBinderInfo shape)

    generatedIdentitiesInConstructorForallBinder =
      typeBinderGeneratedIdentities . constructorForallIdentity

    generatedIdentitiesInDataInfo info =
      symbolGeneratedIdentities (dataInfoSymbol info)
        ++ concatMap (maybe [] typeBinderGeneratedIdentities . typeParamBinderIdentity) (dataTypeParams info)
        ++ concatMap generatedIdentitiesInConstructorInfo (dataConstructors info)

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
        Map.fromList
          [ (runtimeName, key)
          | (runtimeName, keys) <- Map.toList keysByRuntimeName
          , [key] <- [Set.toList keys]
          ],
      runtimeExternalBindingByKey =
        Map.fromList
          [ (key, resolved)
          | (key, resolved : rest) <- Map.toList resolvedByKey,
            all (sameRuntimeExternalBinding resolved) rest
          ]
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

    keysByRuntimeName =
      Map.fromListWith
        Set.union
        [ (runtimeName, Set.singleton key)
        | (runtimeName, key, _) <- entries
        ]

    resolvedByKey =
      Map.fromListWith
        (++)
        [ (key, [resolved])
        | (_, key, resolved) <- entries
        ]

    sameRuntimeExternalBinding left right =
      X.resolvedVarRuntimeName left == X.resolvedVarRuntimeName right
        && X.resolvedVarType left == X.resolvedVarType right
        && X.resolvedVarDetails left == X.resolvedVarDetails right

runtimeExternalBindingIdentity :: RuntimeExternalBindingIndex -> String -> Maybe ExternalBindingIdentity
runtimeExternalBindingIdentity index name = do
  resolved <- lookupRuntimeExternalBindingByName name index
  pure (externalBindingIdentityFromDetails (X.resolvedVarRuntimeName resolved) (X.resolvedVarDetails resolved))

deferredExternalBindingIndex :: DeferredObligations -> DeferredExternalBindingIndex
deferredExternalBindingIndex obligations =
  DeferredExternalBindingIndex
    { deferredExternalBindingRefByName =
        Map.fromList
          [ (name, ref)
          | (name, [ref]) <- Map.toList refsByName
          ],
      deferredExternalBindingByRef =
        Map.fromList
          [ (deferredProgramObligationRef obligation, obligation)
          | obligation <- Map.elems obligations
          ]
    }
  where
    refsByName =
      Map.fromListWith
        (++)
        [ (deferredRefName ref, [ref])
        | obligation <- Map.elems obligations,
          let ref = deferredProgramObligationRef obligation
        ]

deferredExternalBindingIdentity :: DeferredExternalBindingIndex -> String -> Maybe ExternalBindingIdentity
deferredExternalBindingIdentity index name = do
  obligation <- lookupDeferredExternalBinding name index
  let ref = deferredProgramObligationRef obligation
  pure (externalBindingIdentityFromDetails (deferredRefName ref) (DeferredId ref))

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

runSurfacePipelineWithContext :: FinalizeContext -> Bool -> DeferredObligations -> Map String TypeView -> SurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipelineWithContext context forceUnchecked deferredObligations externalTypeViews0 surfaceExpr = do
  let freeVars = sort (Set.toList (surfaceFreeVars surfaceExpr))
      externalTypeNames = Map.keysSet externalTypes
      externalFreeVars = Set.fromList [name | name <- freeVars, name `Set.member` externalTypeNames]
      runtimeFreeVars = Set.fromList [name | name <- freeVars, name `Set.notMember` externalTypeNames]
      runtimeBindings = restrictPreparedExternalBindings runtimeFreeVars (finalizeContextRuntimeBindings context)
  mapM_ resolveRuntimeType freeVars
  deferredBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      scope
      externalBindingModeFor
      (deferredExternalBindingIdentity deferredExternalIndex)
      (lowerExternalTypeViews scope (Map.restrictKeys externalTypeViews0 externalFreeVars))
  let extEnv = deferredBindings `unionPreparedExternalBindings` runtimeBindings
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  let runPipeline =
        if not forceUnchecked && Map.null deferredObligations
          then runPipelineElabDetailedWithPreparedExternalBindings
          else runPipelineElabDetailedUncheckedWithPreparedExternalBindings
  either (Left . ProgramPipelineError . renderPipelineError) Right (runPipeline Set.empty extEnv normExpr)
  where
    scope = finalizeContextScope context
    externalTypes = Map.map typeViewDisplay externalTypeViews0
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
  Map String TypeView ->
  SurfaceExpr ->
  IO (Either ProgramError PipelineElabDetailedResult)
runSurfacePipelineWithContextWithTiming timing label context forceUnchecked deferredObligations externalTypeViews0 surfaceExpr =
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
            scope
            externalBindingModeFor
            (deferredExternalBindingIdentity deferredExternalIndex)
            (lowerExternalTypeViews scope (Map.restrictKeys externalTypeViews0 externalFreeVars))
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
    externalTypes = Map.map typeViewDisplay externalTypeViews0
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
    LocalId ref ->
      Right (ModuleBindingReadLocal (localRefIdentity ref))
    EvidenceId ref ->
      Right (ModuleBindingReadLocal (localRefIdentity ref))
    EnvId ref ->
      Right (ModuleBindingReadEnv (envRefIdentity ref))
    TopLevelId identity ->
      Right (ModuleBindingReadTopLevel identity)
    ConstructorId ref ->
      Right (ModuleBindingReadConstructor (constructorRefSymbol ref))
    MethodId identity ->
      Right (ModuleBindingReadMethod identity)
    PrimitiveId ref ->
      Right (ModuleBindingReadPrimitive (primitiveRefSymbol ref))
    DeferredId ref ->
      Right (ModuleBindingReadDeferred (deferredRefIdentity ref))

idDetailsReadKeyMaybe :: IdDetails -> Maybe ModuleBindingReadKey
idDetailsReadKeyMaybe details =
  case idDetailsReadKey details of
    Right key -> Just key
    Left _ -> Nothing

externalBindingModeForObligations :: DeferredExternalBindingIndex -> Map String SrcType -> String -> ExternalBindingMode
externalBindingModeForObligations deferredExternalIndex externalTypes name =
  case lookupDeferredExternalBinding name deferredExternalIndex of
    Just (DeferredMethod {}) -> ExternalBindingScheme
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

externalBindingModeForRuntime :: Map String SrcType -> String -> ExternalBindingMode
externalBindingModeForRuntime runtimeSourceTypes name =
  case Map.lookup name runtimeSourceTypes of
    Just ty
      | sourceTypeHasForall ty && sourceArrowCountAfterForalls ty >= 2 ->
          ExternalBindingMonomorphic
      | otherwise ->
          ExternalBindingScheme
    Nothing ->
      ExternalBindingScheme

sourceArrowCountAfterForalls :: SrcType -> Int
sourceArrowCountAfterForalls ty =
  case ty of
    STForall _ _ body -> sourceArrowCountAfterForalls body
    STArrow _ cod -> 1 + sourceArrowCountAfterForalls cod
    _ -> 0

lookupDeferredExternalBinding :: String -> DeferredExternalBindingIndex -> Maybe DeferredProgramObligation
lookupDeferredExternalBinding name index =
  Map.lookup name (deferredExternalBindingRefByName index)
    >>= (`Map.lookup` deferredExternalBindingByRef index)

lookupRuntimeExternalBindingByName :: String -> RuntimeExternalBindingIndex -> Maybe X.ResolvedVar
lookupRuntimeExternalBindingByName name index =
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
  ElaborateScope ->
  (String -> ExternalBindingMode) ->
  (String -> Maybe ExternalBindingIdentity) ->
  Map String TypeView ->
  Either ProgramError PreparedExternalBindings
prepareSurfaceExternalBindingsWithIdentity scope modeFor identityFor sourceTypeViews = do
  extBindings <-
    Map.traverseWithKey
      ( \name view -> do
          normTy <- either (Left . ProgramPipelineError . show) Right (normalizeType (typeViewDisplay view))
          Right
            ExternalBinding
              { externalBindingType = normTy,
                externalBindingMode = modeFor name,
                externalBindingIdentity = identityFor name,
                externalBindingTypeHeadIdentities =
                  mergeSymbolIdentityMaps
                    [ typeHeadIdentitiesInScope scope,
                      typeViewHeadIdentities view
                    ],
                externalBindingTypeBinderIdentities =
                  mergeTypeBinderIdentityMaps
                    [ typeViewBinderIdentities view,
                      sourceTypeBinderIdentitiesInScope scope normTy
                    ]
              }
      )
      sourceTypeViews
  either (Left . ProgramPipelineError . show) (Right . addScopeIdentities) (prepareExternalBindings extBindings)
  where
    addScopeIdentities =
      extendPreparedExternalBindingTypeIdentities
        (typeHeadIdentitiesInScope scope)
        Map.empty

lowerExternalTypeViews :: ElaborateScope -> Map String TypeView -> Map String TypeView
lowerExternalTypeViews scope =
  Map.map lowerView
  where
    lowerView view =
      view
        { typeViewDisplay = loweredDisplay,
          typeViewIdentity = loweredIdentity,
          typeViewHeadIdentities =
            mergeSymbolIdentityMaps
              [ typeHeadIdentitiesInScope scope,
                typeViewHeadIdentities view
              ],
          typeViewBinderIdentities =
            mergeTypeBinderIdentityMaps
              [ typeViewBinderIdentities view,
                sourceTypeBinderIdentitiesInScope scope loweredDisplay,
                sourceTypeBinderIdentitiesInScope scope loweredIdentity
              ]
        }
      where
        loweredDisplay = lowerType scope (typeViewDisplay view)
        loweredIdentity = lowerType scope (typeViewIdentity view)

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

sourceForallMatchesWithRigidForallsInScope :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceForallMatchesWithRigidForallsInScope scope expected actual =
  case sourceForallMatchSubstInScope scope expected actual of
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

sourceForallMatchesInScope :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceForallMatchesInScope scope expected actual =
  case sourceForallMatchSubstInScope scope expected actual of
    Just _ -> True
    Nothing -> False

sourceForallMatchSubst :: SrcType -> SrcType -> Maybe (Map String SrcType)
sourceForallMatchSubst expected actual =
  sourceForallMatchSubstWith (==) alphaEqSrcType expected actual

sourceForallMatchSubstInScope :: ElaborateScope -> SrcType -> SrcType -> Maybe (Map String SrcType)
sourceForallMatchSubstInScope scope expected actual =
  sourceForallMatchSubstWith
    (sourceTypeHeadMatchesInScope scope)
    (alphaEqSrcTypeInScope scope)
    expected
    actual

sourceForallMatchSubstWith ::
  (String -> String -> Bool) ->
  (SrcType -> SrcType -> Bool) ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
sourceForallMatchSubstWith sameTypeHead sameType expected actual =
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
            STBase actualName | sameTypeHead name actualName -> Just subst
            _ -> Nothing
        STCon name args ->
          case actualTy of
            STCon actualName actualArgs
              | sameTypeHead name actualName && length (toListNE args) == length (toListNE actualArgs) ->
                  foldM
                    (\acc (leftTy, rightTy) -> match bound acc leftTy rightTy)
                    subst
                    (zip (toListNE args) (toListNE actualArgs))
            _ -> Nothing
        STVarApp name args ->
          matchVarApp bound subst name args actualTy
        STTyLam name body ->
          case actualTy of
            STTyLam name' body' ->
              let bound' = Set.insert name bound
                  subst' = Map.insert name (STVar name') (Map.delete name subst)
               in match bound' subst' body body'
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
              | sameTypeHead rigidName actualName && expectedArgCount == length (toListNE actualArgs) ->
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
          | sameType existing actualTy -> Just subst
          | otherwise -> Nothing

    freeTypeVarsSrcTypeLocal = freeSourceTypeVars

sourceTypeHeadMatchesInScope :: ElaborateScope -> String -> String -> Bool
sourceTypeHeadMatchesInScope scope expected actual =
  case matchTypesInScope scope Map.empty (STBase expected) (STBase actual) of
    Just _ -> True
    Nothing -> False

alphaEqSrcType :: SrcType -> SrcType -> Bool
alphaEqSrcType =
  alphaEqSrcTypeWith (==)

alphaEqSrcTypeInScope :: ElaborateScope -> SrcType -> SrcType -> Bool
alphaEqSrcTypeInScope scope =
  alphaEqSrcTypeWith (sourceTypeHeadMatchesInScope scope)

alphaEqSrcTypeWith :: (String -> String -> Bool) -> SrcType -> SrcType -> Bool
alphaEqSrcTypeWith sameTypeHead = go Map.empty Map.empty
  where
    go leftNames rightNames left right =
      case (left, right) of
        (STVar leftName, STVar rightName) ->
          sameTypeVar leftNames rightNames leftName rightName
        (STArrow leftDom leftCod, STArrow rightDom rightCod) ->
          go leftNames rightNames leftDom rightDom
            && go leftNames rightNames leftCod rightCod
        (STBase leftName, STBase rightName) -> sameTypeHead leftName rightName
        (STCon leftName leftArgs, STCon rightName rightArgs) ->
          sameTypeHead leftName rightName
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
      argViews <- mapM (inferArgTypeView env0) visibleArgs
      (substFromHead, _remainingHeadBinders, headInstViews) <-
        foldM
          ( \(subst, remainingBinders, views) instTy ->
              case remainingBinders of
                binder : rest -> do
                  let recoveredInstView = elabTypeToRecoveredTypeView scope (stripVacuousForalls instTy)
                  subst' <-
                    maybe
                      (Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo)))
                      Right
                      (bindTypeBinderSubstViewInScope scope binder recoveredInstView subst)
                  Right (subst', rest, recoveredInstView : views)
                [] -> Right (subst, [], views)
          )
          (deferredConstructorInitialSubst deferred, instBinders, [])
          headInsts
      let substFromArgs =
            case matchTypeBinderSubstViewPairsInScope scope instBinders substFromHead (zip visibleArgTemplates argViews) of
              Just subst -> subst
              Nothing ->
                case matchTypeBinderSubstViewPairsInScope scope instBinders (deferredConstructorInitialSubst deferred) (zip visibleArgTemplates argViews) of
                  Just subst -> subst
                  Nothing -> substFromHead
          argHeadIdentities =
            mergeSymbolIdentityMaps (deferredConstructorTypeHeadIdentities deferred : map typeViewHeadIdentities argViews ++ map typeViewHeadIdentities headInstViews)
      occurrenceView <-
        let occurrenceFallbackTy = applyConstructorSubst substFromArgs (deferredConstructorOccurrenceType deferred)
         in do
              occurrenceEnv <- ensureDeferredConstructorPlaceholderEnv argHeadIdentities env0 placeholderName deferred substFromArgs
              inferOccurrenceTypeView occurrenceEnv placeholderName occurrenceFallbackTy occurrenceTerm
      let substFinal =
            case matchTypeBinderSubstViewInScope scope instBinders substFromArgs (deferredConstructorOccurrenceType deferred) occurrenceView of
              Just subst -> subst
              Nothing -> substFromArgs
          constructorHeadIdentities =
            mergeSymbolIdentityMaps [argHeadIdentities, typeViewHeadIdentities occurrenceView]
          missingInstBinders =
            filter
              (\(_, identity) -> maybe True (const False) (lookupTypeBinderSubstByIdentity identity substFinal))
              instBinders
      case missingInstBinders of
        [] -> do
          ctorHead <-
            if constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo
              then
                foldM
                  ( \headAcc (_, identity) ->
                      case lookupTypeBinderSubstByIdentity identity substFinal of
                        Just ty -> do
                          instTy <- srcTypeToElabTypeInScopeWithHeadIdentities scope constructorHeadIdentities (lowerType scope ty)
                          Right (X.ETyInst headAcc (X.InstApp instTy))
                        Nothing -> Right headAcc
                  )
                  (X.EVarNode (resolvedVarFromConstructorInfo ctorInfo))
                  instBinders
              else inlineConstructorHead scope constructorHeadIdentities [] ctorInfo substFinal
          Right (foldl X.EApp ctorHead args)
        _ ->
          Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo))

    inferArgTypeView env0 arg =
      case typeCheckWithEnv env0 arg of
        Right ty -> Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls ty))
        Left (X.TCArgumentMismatch _ actualTy) ->
          Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls actualTy))
        Left err -> Left (ProgramPipelineError ("deferred constructor argument type check failed: " ++ show err))

    inferOccurrenceTypeView env0 placeholderName fallbackTy occurrenceTerm =
      case typeCheckWithEnv env0 occurrenceTerm of
        Right ty -> Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls ty))
        Left err
          | isDeferredConstructorArgumentMismatch err ->
              Right (sourceTypeViewInScope scope fallbackTy)
          | isDeferredConstructorSelfUnbound placeholderName err ->
              Right (sourceTypeViewInScope scope fallbackTy)
        Left err -> Left (ProgramPipelineError ("deferred constructor occurrence type check failed: " ++ show err))

    isDeferredConstructorArgumentMismatch err =
      case err of
        X.TCArgumentMismatch {} -> True
        _ -> False

    isDeferredConstructorSelfUnbound placeholderName err =
      case err of
        X.TCUnboundVar name -> name == placeholderName
        _ -> False

    ensureDeferredConstructorPlaceholderEnv headIdentities env0 _placeholder deferred subst = do
      placeholderTy <- srcTypeToElabTypeInScopeWithHeadIdentities scope headIdentities (lowerType scope placeholderSourceTy)
      let resolved = X.deferredResolvedVarFromRef (deferredConstructorRef deferred)
      Right (TypeCheck.insertResolvedTermBinding resolved placeholderTy env0)
      where
        placeholderSourceTy = applyConstructorSubst subst (deferredConstructorSourceType deferred)

matchTypeBinderSubstViewPairsInScope ::
  ElaborateScope ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  [(SrcType, TypeView)] ->
  Maybe TypeBinderSubst
matchTypeBinderSubstViewPairsInScope scope binders =
  foldM (\subst (templateTy, actualView) -> matchTypeBinderSubstViewInScope scope binders subst templateTy actualView)

matchTypeBinderSubstViewInScope ::
  ElaborateScope ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  SrcType ->
  TypeView ->
  Maybe TypeBinderSubst
matchTypeBinderSubstViewInScope scope binders subst templateTy actualView =
  typeBinderSubstFromTypeViewSubst binders
    <$> matchTypeViewsAgainstIdentity
      scope
      (typeBinderSubstToTypeViewSubstWith (sourceTypeViewInScope scope) subst)
      (NE.singleton (typeBinderTemplateView scope binders templateTy))
      (NE.singleton actualView)

typeBinderTemplateView :: ElaborateScope -> [(String, TypeBinderIdentity)] -> SrcType -> TypeView
typeBinderTemplateView scope binders ty =
  view
    { typeViewBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ typeViewBinderIdentities view,
            typeBinderAliasIdentityMap binders
          ]
    }
  where
    view = sourceTypeViewInScope scope ty

bindTypeBinderSubstViewInScope ::
  ElaborateScope ->
  (String, TypeBinderIdentity) ->
  TypeView ->
  TypeBinderSubst ->
  Maybe TypeBinderSubst
bindTypeBinderSubstViewInScope scope (name, identity) actualView subst =
  case lookupTypeBinderSubstByIdentity identity subst of
    Nothing ->
      Just (insertTypeBinderSubstWithIdentity identity name actual subst)
    Just (STVar existingName)
      | existingName == name ->
          Just (insertTypeBinderSubstWithIdentity identity name actual subst)
    Just existing
      | alphaEqSrcTypeInScope scope existing actual ->
          Just subst
      | Just existingTy <- srcTypeToElabTypeMaybeInScope scope (lowerType scope existing),
        Right actualTy <- typeViewToElabType scope actualView,
        alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
          Just subst
      | otherwise -> Nothing
  where
    actual =
      typeViewIdentity actualView

inlineConstructorHead :: ElaborateScope -> Map String SymbolIdentity -> [(String, TypeBinderIdentity)] -> ConstructorInfo -> TypeBinderSubst -> Either ProgramError XmlfTerm
inlineConstructorHead scope extraHeadIdentities ownerParamBinders ctorInfo subst = do
  let resultSrcTy = applyConstructorSubst subst (ctorResult ctorInfo)
      argSrcTys = map (applyConstructorSubst subst) (ctorArgs ctorInfo)
      resultVar = "$" ++ symbolDefiningName (ctorOwningTypeIdentity ctorInfo) ++ "_result"
      argNames = ["$" ++ constructorInfoIdentityName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length argSrcTys]]
      ownerShapes =
        case lookupConstructorRuntimeBySymbol scope (ctorInfoSymbol ctorInfo) of
          Just (dataInfo, _) -> dataInfoConstructorOwnerShapes dataInfo
          Nothing -> constructorOwnerShapes ctorInfo
      handlerShapeMatches =
        [ (shape, matchConstructorShapeResultSubst shape)
        | shape <- ownerShapes
        ]
      handlerShapes =
        [ maybe shape (`applyConstructorShapeSubst` shape) handlerSubst
        | (shape, handlerSubst) <- handlerShapeMatches
        ]
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
      headIdentities =
        Map.union extraHeadIdentities (typeHeadIdentitiesInScope scope)
      ownerParamRefsByAlias =
        Map.fromList
          [ (alias, ref)
          | (name, identity) <- ownerParamBinders,
            let ref = X.typeBinderRefFromIdentity identity name,
            alias <- [name, typeBinderIdentityStableName identity]
          ]
      ownerParamRefs =
        [ X.typeBinderRefFromIdentity identity name
        | (name, identity) <- ownerParamBinders
        ]
      missingSharedFreeNames =
        filter (`Map.notMember` ownerParamRefsByAlias) sharedFreeNames
      (freshSharedRefs, generator0) =
        freshTypeBinderRefsAfterHeadAndOwnerParamIdentities headIdentities ownerParamBinders missingSharedFreeNames
      sharedRefs =
        Map.union ownerParamRefsByAlias freshSharedRefs
      sharedTypeAbsRefs =
        [ ref
        | name <- sharedFreeNames,
          Just ref <- [Map.lookup name sharedRefs]
        ]
      topTypeAbsRefs =
        ownerParamRefs ++ filter (not . refIdentityIn ownerParamRefs) sharedTypeAbsRefs
      (resultRef, generator1) = X.sourceTypeBinderRefForName resultVar generator0
      handlerRefs = Map.insert resultVar resultRef sharedRefs
  (resultTy, generator2) <- srcTypeToElabTypeWithHeadIdentities headIdentities sharedRefs generator1 loweredResultSrcTy
  (argTys, generator3) <- srcTypesToElabTypesWith headIdentities sharedRefs generator2 loweredArgSrcTys
  (handlerTys, generator4) <- srcTypesToElabTypesWith headIdentities handlerRefs generator3 loweredHandlerSrcTys
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
  pure (foldr (`X.ETyAbsRef` Nothing) valueBody topTypeAbsRefs)
  where
    refIdentityIn refs ref =
      any (X.typeBinderRefsSameIdentity ref) refs

    freshTypeBinderRefsAfterHeadAndOwnerParamIdentities headIdentities ownerBinders names =
      freshTypeBinderRefs names generator0
      where
        generator0 =
          identityGeneratorAfter
            ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
                ++ concatMap (typeBinderGeneratedIdentities . snd) ownerBinders
            )

    srcTypesToElabTypesWith headIdentities refs generator tys =
      go [] generator tys
      where
        go acc gen [] = Right (reverse acc, gen)
        go acc gen (ty : rest) = do
          (ty', gen') <- srcTypeToElabTypeWithHeadIdentities headIdentities refs gen ty
          go (ty' : acc) gen' rest

    freshResolvedLocals generator [] = ([], generator)
    freshResolvedLocals generator ((name, ty) : rest) =
      let (localRef, generator') = freshLocalRef name generator
          resolved = X.localResolvedVarFromRef localRef ty
          (resolvedRest, generator'') = freshResolvedLocals generator' rest
       in (resolved : resolvedRest, generator'')

    dataInfoConstructorOwnerShapes dataInfo =
      case dataConstructors dataInfo of
        firstCtor : _
          | not (null (ctorOwnerConstructors firstCtor)) ->
              ctorOwnerConstructors firstCtor
        _ ->
          map constructorShapeFromInfo (dataConstructors dataInfo)

    matchConstructorShapeResultSubst shape =
      let templateView = constructorShapeResultMatchView shape
          actualView = sourceTypeViewWithHeadIdentities (applyConstructorSubst subst (ctorResult ctorInfo))
       in typeBinderSubstFromTypeViewSubst (constructorShapeForallBinders shape)
            <$> matchTypeViewsAgainstIdentity
              scope
              Map.empty
              (NE.singleton templateView)
              (NE.singleton actualView)

    sourceTypeViewWithHeadIdentities ty =
      baseView
        { typeViewIdentity = identityTypeWithHeadIdentities ty,
          typeViewHeadIdentities =
            mergeSymbolIdentityMaps
              [ typeViewHeadIdentities baseView,
                extraHeadIdentities
              ]
        }
      where
        baseView =
          sourceTypeViewInScope scope ty

    identityTypeWithHeadIdentities =
      go
      where
        headIdentities =
          Map.union extraHeadIdentities (typeHeadIdentitiesInScope scope)

        go ty =
          case ty of
            STVar {} -> ty
            STArrow dom cod -> STArrow (go dom) (go cod)
            STBase name -> STBase (headName name)
            STCon name args -> STCon (headName name) (fmap go args)
            STVarApp name args -> STVarApp name (fmap go args)
            STTyLam name body -> STTyLam name (go body)
            STTyApp fun arg -> STTyApp (go fun) (go arg)
            STForall name mb body -> STForall name (fmap (SrcBound . go . unSrcBound) mb) (go body)
            STMu name body -> STMu name (go body)
            STBottom -> STBottom

        headName name =
          case lookupSymbolIdentityAlias headIdentities name of
            Just identity -> symbolIdentityStableName identity
            Nothing -> name

    constructorShapeResultMatchView shape =
      TypeView
        { typeViewDisplay = constructorShapeResult shape,
          typeViewIdentity = constructorShapeResultIdentity shape,
          typeViewHeadIdentities =
            mergeSymbolIdentityMaps
              [ typeViewHeadIdentities (constructorShapeTypeView shape),
                typeViewHeadIdentities (sourceTypeViewInScope scope (constructorShapeResultIdentity shape))
              ],
          typeViewBinderIdentities =
            mergeTypeBinderIdentityMaps
              [ typeBinderAliasIdentityMap (constructorShapeForallBinders shape)
              ]
        }

applyConstructorShapeSubst :: TypeBinderSubst -> ConstructorShape -> ConstructorShape
applyConstructorShapeSubst subst shape =
  let keptBinders =
        filter keepBinder (constructorShapeForallBinderInfo shape)
      keepBinder binder =
        maybe True (const False) (lookupTypeBinderSubstByIdentity (constructorForallIdentity binder) subst)
      keptOwnerParams =
        filter keepOwnerParam (constructorShapeOwnerTypeParams shape)
      keepOwnerParam param =
        case typeParamBinderIdentity param of
          Just identity -> maybe True (const False) (lookupTypeBinderSubstByIdentity identity subst)
          Nothing -> True
      substForalls foralls =
        [ (name, fmap (applyConstructorSubst subst) mbBound)
          | ((name, mbBound), binder) <- zip foralls (constructorShapeForallBinderInfo shape),
            keepBinder binder
        ]
      displayForalls = substForalls (constructorShapeForalls shape)
      identityForalls = substForalls (constructorShapeForallsIdentity shape)
      shapeType foralls args result =
        foldr
          (\(name, mbBound) acc -> STForall name (fmap SrcBound mbBound) acc)
          (foldr STArrow result args)
          foralls
   in shape
        { constructorShapeTypeView =
            (constructorShapeTypeView shape)
              { typeViewDisplay =
                  shapeType
                    displayForalls
                    (map (applyConstructorSubst subst) (constructorShapeArgs shape))
                    (applyConstructorSubst subst (constructorShapeResult shape)),
                typeViewIdentity =
                  shapeType
                    identityForalls
                    (map (applyConstructorSubst subst) (constructorShapeArgsIdentity shape))
                    (applyConstructorSubst subst (constructorShapeResultIdentity shape))
                  },
          constructorShapeForallBinderInfo = keptBinders,
          constructorShapeOwnerTypeParams = keptOwnerParams
        }

constructorShapeForallBinders :: ConstructorShape -> [(String, TypeBinderIdentity)]
constructorShapeForallBinders shape =
  ownerEntries ++ forallEntries
  where
    forallEntries =
      [ (constructorForallDisplayName binder, constructorForallIdentity binder)
      | binder <- constructorShapeForallBinderInfo shape
      ]
    ownerParams = constructorShapeOwnerTypeParams shape
    ownerEntries =
      [ (typeParamName param, identity)
      | param <- ownerParams,
        let identity = requiredOwnerParamIdentity param
      ]

    requiredOwnerParamIdentity param =
      case typeParamBinderIdentity param of
        Just identity -> identity
        Nothing -> error ("checked constructor owner parameter `" ++ typeParamName param ++ "` is missing identity")

applyConstructorSubst :: TypeBinderSubst -> SrcType -> SrcType
applyConstructorSubst subst ty =
  applyTypeBinderSubst subst ty

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
              (sharedRefs, generator0) =
                freshTypeBinderRefsAfterHeadIdentities (typeHeadIdentitiesInScope scope) sharedNames
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
resolveDeferredMethods scope deferredMethods env0 term0 = do
  evidenceTypeOverrides <- localEvidenceTypeOverrides scope deferredMethods
  go evidenceTypeOverrides env0 term0
  where
    lookupDeferredMethod ref =
      Map.lookup ref deferredMethods

    go evidenceTypeOverrides env term =
      case deferredPlaceholderHeadRefWithInsts term of
        Just (ref, headInsts)
          | Just deferred <- lookupDeferredMethod ref,
            deferredMethodArgCount deferred == 0 ->
              resolveDeferredNullaryMethod headInsts deferred
        _ ->
          case term of
            X.EVarNode resolved -> Right (X.EVarNode (applyEvidenceTypeOverride evidenceTypeOverrides resolved))
            X.ELit {} -> Right term
            X.ELam resolved body -> do
              let resolved' = applyEvidenceTypeOverride evidenceTypeOverrides resolved
                  ty = X.resolvedVarType resolved'
                  env' = TypeCheck.insertResolvedTermBinding resolved ty env
              X.ELam resolved' <$> go evidenceTypeOverrides env' body
            X.EApp {} -> rewriteApplication evidenceTypeOverrides env term
            X.ELet resolved scheme rhs body -> do
              let resolved0 = applyEvidenceTypeOverride evidenceTypeOverrides resolved
                  schemeTy = schemeToType scheme
                  rhsEnv = TypeCheck.insertResolvedTermBinding resolved0 schemeTy env
              rhs' <- go evidenceTypeOverrides rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  resolved' = X.mapResolvedVarType (const rhsTy) resolved0
                  env' = TypeCheck.insertResolvedTermBinding resolved' rhsTy env
              body' <- go evidenceTypeOverrides env' body
              Right (X.ELet resolved' scheme rhs' body')
            X.ETyAbsRef ref mbBound body -> do
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = TypeCheck.insertTypeBindingRef ref boundTy env
              X.ETyAbsRef ref mbBound <$> go evidenceTypeOverrides env' body
            X.ETyInst inner inst ->
              (`X.ETyInst` inst) <$> go evidenceTypeOverrides env inner
            X.ERoll ty body ->
              X.ERoll ty <$> go evidenceTypeOverrides env body
            X.EUnroll inner ->
              X.EUnroll <$> go evidenceTypeOverrides env inner

    rewriteApplication evidenceTypeOverrides env term =
      let (headTerm, args) = collectElabApps term
       in case deferredPlaceholderHeadRef headTerm >>= lookupDeferredMethod of
            Just deferred -> do
              args' <- mapM (go evidenceTypeOverrides env) args
              resolveDeferredApplication env deferred args'
            Nothing ->
              case term of
                X.EApp fun arg -> X.EApp <$> go evidenceTypeOverrides env fun <*> go evidenceTypeOverrides env arg
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
              methodLocalConstraintInfos <- methodLocalConstraints methodInfo classArgView methodSubst'
              evidenceArgs <-
                resolveConstraintEvidenceTerms
                  scope
                  (deferredMethodLocalEvidence deferred)
                  Set.empty
                  methodLocalConstraintInfos
              Right (foldl X.EApp (foldl X.EApp evidenceHead evidenceArgs) args)
            Nothing -> do
              (instanceInfo, subst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
              methodValue <- concreteMethodValue instanceInfo methodInfo
              methodSubst <-
                case inferMethodArgumentSubst methodInfo classArgView subst argViews of
                  Just subst' -> Right subst'
                  Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
              let eagerConstraints =
                    map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue)
              eagerConstraints' <- filterConstraintGround eagerConstraints
              evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) Set.empty eagerConstraints'
              methodHead <- instantiateMethodValueWithAliasViews scope [methodTypeView methodInfo] methodSubst methodValue
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
          methodLocalConstraintInfos <- methodLocalConstraints methodInfo classArgView methodSubst'
          evidenceArgs <-
            resolveConstraintEvidenceTerms
              scope
              (deferredMethodLocalEvidence deferred)
              Set.empty
              methodLocalConstraintInfos
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
                map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue)
          eagerConstraints' <- filterConstraintGround eagerConstraints
          evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) Set.empty eagerConstraints'
          methodHead <- instantiateMethodValueWithAliasViews scope [methodTypeView methodInfo] methodSubst methodValue
          Right (reapplyHeadInsts headInsts (foldl X.EApp methodHead evidenceArgs))

    inferDeferredMethodClassArgument methodInfo argViews mbExpectedResult =
      inferDeferredMethodClassArgumentFromArgs methodInfo argViews
        <|> inferDeferredMethodClassArgumentFromExpected methodInfo argViews mbExpectedResult

    inferDeferredMethodClassArgumentFromArgs methodInfo argViews = do
      let methodView = methodTypeView methodInfo
      subst <-
        foldM
          (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (templateView :| []) (actualView :| []))
          Map.empty
          (zip (methodParamViews methodView) argViews)
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

    inferDeferredMethodClassArgumentFromExpected _ _ Nothing = Nothing
    inferDeferredMethodClassArgumentFromExpected methodInfo argViews (Just expectedView) = do
      let methodView = methodTypeView methodInfo
      substFromArgs <-
        foldM
          (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (templateView :| []) (actualView :| []))
          Map.empty
          (zip (methodParamViews methodView) argViews)
      subst <- matchMethodTypeViews scope substFromArgs (methodResultTypeView methodInfo :| []) (expectedView :| [])
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
            Just subst <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) targetViews],
            methodEvidence <- maybe [] (: []) (Map.lookup (methodInfoSymbolIdentity methodInfo) (evidenceMethodsByIdentity evidence)),
            Just _ <- [evidenceMethodResolvedVar methodEvidence]
          ]
        fallbackEvidence = do
          evidence <- deferredMethodEvidence deferred
          _ <- evidenceMethodResolvedVar (deferredMethodEvidenceMethod evidence)
          subst <- matchMethodTypeViews scope Map.empty (deferredMethodEvidenceClassArgs evidence) targetViews
          pure (evidence {deferredMethodEvidenceClassArg = classArgView, deferredMethodEvidenceClassArgs = targetViews}, subst)

    methodLocalConstraints methodInfo classArgView methodSubst = do
      headVars <- freeTypeBinderIdentitiesTypeViewsOrError (classArgView :| [])
      methodLocal <-
        filterM
          (fmap not . constraintDeterminedByTypeBinderIdentities headVars)
          specializedForClass
      pure (map (applyConstraintInfoSubst methodSubst) methodLocal)
      where
        classArgSubst =
          typeViewSubstFromParamIdentities
            (methodParamBinderIdentities methodInfo)
            (classArgView :| [])
        specializedForClass =
          map
            (applyConstraintInfoSubst classArgSubst)
            (methodConstraintInfos methodInfo)

    inferNullaryMethodClassArgument methodInfo expectedView
      | deferredMethodFullArityFromInfo methodInfo /= 0 = Nothing
      | otherwise = do
          subst <- matchMethodTypeViews scope Map.empty (methodResultTypeView methodInfo :| []) (expectedView :| [])
          NE.head <$> lookupMethodParamViewSubst methodInfo subst

    inferNullaryMethodSubst methodInfo classArgView subst expectedView =
      let specializedMethodView =
            specializeMethodTypeView methodInfo (classArgView :| [])
       in matchMethodTypeViews scope subst (methodResultView specializedMethodView :| []) (expectedView :| [])

    nullaryMethodResultIsClassParameter methodInfo =
      case typeViewIdentity resultView of
        STVar name ->
          typeViewBinderIdentityForAlias resultView name
            == Just (NE.head (methodParamBinderIdentities methodInfo))
        _ -> False
      where
        resultView = methodResultTypeView methodInfo

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
            (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (templateView :| []) (actualView :| []))
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
            map (applyConstraintInfoSubst subst) (methodValueConstraints valueInfo)
      eagerConstraints' <- filterConstraintGround eagerConstraints
      nestedEvidence <-
        resolveConstraintEvidenceTerms
          scope
          localEvidence
          seen'
          eagerConstraints'
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
        Just subst <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) headViews],
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
          && case matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) (constraintTypeViews constraint) of
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

localEvidenceTypeOverrides :: ElaborateScope -> Map DeferredRef DeferredMethodCall -> Either ProgramError (Map LocalRef ElabType)
localEvidenceTypeOverrides scope deferredMethods =
  do
    entries <- mapM typeOverrideEntry methods
    let reservedTypes = map snd entries
    pure $
      Map.fromList
        [ (ref, freshenElabTypeBindersAgainstTypes reservedTypes ty)
        | (ref, ty) <- entries
        ]
  where
    methods =
      [ method
      | deferred <- Map.elems deferredMethods,
        method <- deferredEvidenceMethods deferred,
        Just _ <- [evidenceMethodResolvedVar method >>= X.resolvedVarLocalRef]
      ]

    deferredEvidenceMethods deferred =
      concatMap (Map.elems . evidenceMethodsByIdentity) (deferredMethodLocalEvidence deferred)
        ++ maybe [] ((: []) . deferredMethodEvidenceMethod) (deferredMethodEvidence deferred)

    typeOverrideEntry method = do
      resolved <- evidenceMethodResolvedVarOrError method
      ty <- typeViewToElabType scope (evidenceMethodTypeView method)
      case X.resolvedVarLocalRef resolved of
        Just ref -> Right (ref, ty)
        Nothing -> Left (ProgramPipelineError ("deferred evidence method is not a local binder `" ++ evidenceMethodRuntimeName method ++ "`"))

applyEvidenceTypeOverride :: Map LocalRef ElabType -> X.ResolvedVar -> X.ResolvedVar
applyEvidenceTypeOverride overrides resolved =
  case X.resolvedVarLocalRef resolved >>= (`Map.lookup` overrides) of
    Just ty -> X.mapResolvedVarType (const ty) resolved
    Nothing -> resolved

evidenceMethodResolvedVarWithMetadataType :: ElaborateScope -> EvidenceMethod -> Either ProgramError X.ResolvedVar
evidenceMethodResolvedVarWithMetadataType scope methodEvidence = do
  resolved <- evidenceMethodResolvedVarOrError methodEvidence
  ty <- typeViewToElabType scope (evidenceMethodTypeView methodEvidence)
  Right (X.mapResolvedVarType (const ty) resolved)

instantiateLocalMethodEvidence :: ElaborateScope -> TypeViewSubst -> DeferredMethodEvidence -> Either ProgramError XmlfTerm
instantiateLocalMethodEvidence scope subst DeferredMethodEvidence {deferredMethodEvidenceMethod = methodEvidence} = do
  resolved <- evidenceMethodResolvedVarWithMetadataType scope methodEvidence
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
  let resolved' =
        freshenResolvedVarTypeAgainstInstantiations (instantiationTypes instantiations) resolved
      methodTerm = X.EVarNode resolved'
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

constraintDeterminedByTypeBinderIdentities :: Set TypeBinderIdentity -> ConstraintInfo -> Either ProgramError Bool
constraintDeterminedByTypeBinderIdentities typeVars constraint =
  (`Set.isSubsetOf` typeVars) <$> freeTypeBinderIdentitiesTypeViewsOrError (constraintTypeViews constraint)

constraintGround :: ConstraintInfo -> Either ProgramError Bool
constraintGround constraint =
  Set.null <$> freeTypeBinderIdentitiesTypeViewsOrError (constraintTypeViews constraint)

filterConstraintGround :: [ConstraintInfo] -> Either ProgramError [ConstraintInfo]
filterConstraintGround =
  filterM constraintGround

freeTypeBinderIdentitiesTypeViewsOrError :: NonEmpty TypeView -> Either ProgramError (Set TypeBinderIdentity)
freeTypeBinderIdentitiesTypeViewsOrError views =
  case freeTypeBinderIdentitiesTypeViews views of
    Right identities -> Right identities
    Left name ->
      Left $
        ProgramPipelineError
          ("finalize resolved type variable `" ++ name ++ "` is missing binder identity")

methodValueConstraints :: ValueInfo -> [ConstraintInfo]
methodValueConstraints OrdinaryValue {valueConstraintInfos = constraints} = constraints
methodValueConstraints _ = []

instantiateMethodValue :: ElaborateScope -> TypeViewSubst -> ValueInfo -> Either ProgramError XmlfTerm
instantiateMethodValue scope =
  instantiateMethodValueWithAliasViews scope []

instantiateMethodValueWithAliasViews :: ElaborateScope -> [TypeView] -> TypeViewSubst -> ValueInfo -> Either ProgramError XmlfTerm
instantiateMethodValueWithAliasViews scope aliasViews subst valueInfo@OrdinaryValue {} = do
  let sourceView = ordinaryValueTypeView valueInfo
      substViews = sourceView : aliasViews
  resolved <- resolvedVarFromValueInfo valueInfo <$> typeViewToElabType scope sourceView
  let foralls = resolvedForallsMatchingSourceOrAliasSubst subst substViews (X.resolvedVarType resolved) sourceView
  instantiations <- methodForallInstantiationsFromAliasSubst scope substViews subst sourceView foralls
  let resolved' =
        freshenResolvedVarTypeAgainstInstantiations (instantiationTypes instantiations) resolved
  pure (foldl X.ETyInst (X.EVarNode resolved') instantiations)
instantiateMethodValueWithAliasViews scope _ _ valueInfo@ConstructorValue {} =
  X.EVarNode . resolvedVarFromValueInfo valueInfo <$> typeViewToElabType scope (constructorTypeView scope (valueCtorInfo valueInfo))
instantiateMethodValueWithAliasViews _ _ _ OverloadedMethod {} =
  Left (ProgramPipelineError "overloaded method value reached deferred method instantiation")

instantiationTypes :: [X.Instantiation] -> [ElabType]
instantiationTypes =
  concatMap go
  where
    go inst =
      case inst of
        X.InstApp ty -> [ty]
        X.InstBot ty -> [ty]
        X.InstSeq left right -> go left ++ go right
        X.InstInside inner -> go inner
        X.InstUnderRef _ inner -> go inner
        _ -> []

freshenResolvedVarTypeAgainstInstantiations :: [ElabType] -> X.ResolvedVar -> X.ResolvedVar
freshenResolvedVarTypeAgainstInstantiations instTys resolved
  | null instTys = resolved
  | otherwise = X.mapResolvedVarType (const (freshenElabTypeBindersAgainstTypes instTys ty0)) resolved
  where
    ty0 = X.resolvedVarType resolved

freshenElabTypeBindersAgainstTypes :: [ElabType] -> ElabType -> ElabType
freshenElabTypeBindersAgainstTypes reservedTys ty
  | null reservedRefs = ty
  | otherwise = ty'
  where
    reservedRefs = foldMap freeTypeVarRefsType reservedTys
    reservedNames =
      Set.fromList (map X.typeBinderRefName reservedRefs)
    generator0 =
      X.identityGeneratorAfterType (foldr X.TArrow ty reservedTys)
    (ty', _) =
      freshenTypeBindersAgainstRefs reservedRefs reservedNames generator0 ty

freshenTypeBindersAgainstRefs :: [X.TypeBinderRef] -> Set String -> IdentityGenerator -> ElabType -> (ElabType, IdentityGenerator)
freshenTypeBindersAgainstRefs reservedRefs reservedNames generator0 =
  go generator0
  where
    binderCollides ref =
      any (X.typeBinderRefsSameIdentity ref) reservedRefs

    go generator ty =
      case ty of
        X.TVarRef {} ->
          (ty, generator)
        X.TArrow dom cod ->
          let (dom', generator1) = go generator dom
              (cod', generator2) = go generator1 cod
           in (X.TArrow dom' cod', generator2)
        X.TConWithIdentity identity con args ->
          let (args', generator') = freshenNonEmpty generator args
           in (X.TConWithIdentity identity con args', generator')
        X.TVarAppRef ref args ->
          let (args', generator') = freshenNonEmpty generator args
           in (X.TVarAppRef ref args', generator')
        X.TBaseWithIdentity {} ->
          (ty, generator)
        X.TForallRef ref mbBound body ->
          let (mbBound', generator1) =
                freshenMaybeBound generator mbBound
              (ref', bodyForFreshening, generator2) =
                if binderCollides ref
                  then
                    let usedNames =
                          Set.unions
                            [ reservedNames,
                              Set.fromList (map X.typeBinderRefName (freeTypeVarRefsType body)),
                              maybe Set.empty (Set.fromList . map X.typeBinderRefName . freeTypeVarRefsType) mbBound,
                              Set.singleton (X.typeBinderRefName ref)
                            ]
                        freshName = freshNameLike (X.typeBinderRefName ref) usedNames
                        (freshRef, generator') = X.freshTypeBinderRef freshName generator1
                     in (freshRef, substTypeCaptureRef ref (X.TVarRef freshRef) body, generator')
                  else (ref, body, generator1)
              (body', generator3) = go generator2 bodyForFreshening
           in (X.TForallRef ref' mbBound' body', generator3)
        X.TMuRef ref body ->
          let (ref', bodyForFreshening, generator1) =
                if binderCollides ref
                  then
                    let usedNames =
                          Set.unions
                            [ reservedNames,
                              Set.fromList (map X.typeBinderRefName (freeTypeVarRefsType body)),
                              Set.singleton (X.typeBinderRefName ref)
                            ]
                        freshName = freshNameLike (X.typeBinderRefName ref) usedNames
                        (freshRef, generator') = X.freshTypeBinderRef freshName generator
                     in (freshRef, substTypeCaptureRef ref (X.TVarRef freshRef) body, generator')
                  else (ref, body, generator)
              (body', generator2) = go generator1 bodyForFreshening
           in (X.TMuRef ref' body', generator2)
        X.TBottom ->
          (ty, generator)

    freshenMaybeBound generator =
      \case
        Nothing -> (Nothing, generator)
        Just bound ->
          let (bound', generator') = go generator (X.tyToElab bound)
           in case X.elabToBound bound' of
                Right bound'' -> (Just bound'', generator')
                Left _ -> (Just bound, generator')

    freshenNonEmpty generator (arg :| args) =
      let (arg', generator1) = go generator arg
          (argsRev, generator') =
            foldl
              ( \(acc, gen) item ->
                  let (item', gen') = go gen item
                   in (item' : acc, gen')
              )
              ([], generator1)
              args
       in (arg' :| reverse argsRev, generator')

methodForallInstantiationsFromSourceSubst :: ElaborateScope -> TypeViewSubst -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)] -> Either ProgramError [X.Instantiation]
methodForallInstantiationsFromSourceSubst scope subst sourceView foralls =
  methodForallInstantiations scope (resolvedForallSubst subst sourceView foralls) foralls

methodForallInstantiationsFromAliasSubst :: ElaborateScope -> [TypeView] -> TypeViewSubst -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)] -> Either ProgramError [X.Instantiation]
methodForallInstantiationsFromAliasSubst scope aliasViews subst sourceView foralls =
  methodForallInstantiations scope (resolvedForallSubstWithAliasViews subst aliasViews sourceView foralls) foralls

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
  resolvedForallSubstWithAliasViews subst [sourceView] sourceView foralls

resolvedForallSubstWithAliasViews :: TypeViewSubst -> [TypeView] -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)] -> Map X.TypeBinderRef TypeView
resolvedForallSubstWithAliasViews subst aliasViews sourceView foralls =
  Map.fromList
    [ (ref, ty)
    | (index, (ref, _)) <- zip [0 :: Int ..] foralls,
      Just ty <- [lookupResolvedForallSubst subst aliasViews sourceView index ref]
    ]

lookupResolvedForallSubst :: TypeViewSubst -> [TypeView] -> TypeView -> Int -> X.TypeBinderRef -> Maybe TypeView
lookupResolvedForallSubst subst aliasViews sourceView index ref =
  firstMatchingKey keys
  where
    candidateNames = resolvedForallCandidateNames sourceView index ref
    keys = identityKeys candidateNames

    identityKeys names0 =
      typeViewSubstKeyForIdentity (X.typeBinderRefIdentity ref)
        : [ typeViewSubstKeyForIdentity identity
          | view <- aliasViews,
            name <- names0,
            Just identity <- [typeViewBinderIdentityForAlias view name]
          ]

    firstMatchingKey [] = Nothing
    firstMatchingKey (key : restKeys) =
      lookupTypeViewSubst key subst <|> firstMatchingKey restKeys

resolvedForallsMatchingSourceOrSubst :: TypeViewSubst -> ElabType -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)]
resolvedForallsMatchingSourceOrSubst subst resolvedTy sourceView =
  resolvedForallsMatchingSourceOrAliasSubst subst [sourceView] resolvedTy sourceView

resolvedForallsMatchingSourceOrAliasSubst :: TypeViewSubst -> [TypeView] -> ElabType -> TypeView -> [(X.TypeBinderRef, Maybe X.BoundType)]
resolvedForallsMatchingSourceOrAliasSubst subst aliasViews resolvedTy sourceView =
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
      case lookupResolvedForallSubst subst aliasViews sourceView (sourceCount + index) ref of
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
    X.EVarNode resolved -> X.deferredResolvedVarRef resolved
    X.ETyInst inner _ -> deferredPlaceholderHeadRef inner
    _ -> Nothing

deferredPlaceholderHeadRefWithInsts :: XmlfTerm -> Maybe (DeferredRef, [ElabType])
deferredPlaceholderHeadRefWithInsts = go []
  where
    go insts term =
      case term of
        X.EVarNode resolved -> fmap (\ref -> (ref, insts)) (X.deferredResolvedVarRef resolved)
        X.ETyInst inner (X.InstApp ty) -> go (ty : insts) inner
        X.ETyInst inner _ -> go insts inner
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
            matchRecoverType scope (Set.fromList params) Map.empty Map.empty template ty
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
  ElaborateScope ->
  Set String ->
  Map String SrcType ->
  Map String String ->
  SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchRecoverType scope params subst renames template actual =
  case template of
    STVar name
      | name `Set.member` params ->
          bindRecoverParam scope name actual subst
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
          subst' <- matchRecoverType scope params subst renames dom dom'
          matchRecoverType scope params subst' renames cod cod'
        _ -> Nothing
    STBase name ->
      case actual of
        STBase name' | recoverTypeHeadMatches scope name name' -> Just subst
        _ -> Nothing
    STCon name args ->
      case actual of
        STCon name' args'
          | recoverTypeHeadMatches scope name name' && length (toListNE args) == length (toListNE args') ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType scope params acc renames leftTy rightTy)
                subst
                (zip (toListNE args) (toListNE args'))
        _ -> Nothing
    STVarApp name args ->
      matchRecoverVarApp scope params subst renames name args actual
    STTyLam name body ->
      case actual of
        STTyLam name' body' ->
          matchRecoverType scope params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STTyApp fun arg ->
      case actual of
        STTyApp fun' arg' -> do
          subst' <- matchRecoverType scope params subst renames fun fun'
          matchRecoverType scope params subst' renames arg arg'
        _ -> Nothing
    STForall name _mb body ->
      case actual of
        STForall name' _mb' body' ->
          matchRecoverType scope params subst (Map.insert name name' renames) body body'
        _ ->
          matchRecoverType scope (Set.insert name params) subst renames body actual
    STMu name body ->
      case actual of
        STMu name' body' ->
          matchRecoverType scope params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STBottom ->
      case actual of
        STBottom -> Just subst
        _ -> Nothing

recoverTypeHeadMatches :: ElaborateScope -> String -> String -> Bool
recoverTypeHeadMatches scope expected actual =
  case matchTypesInScope scope Map.empty (STBase expected) (STBase actual) of
    Just _ -> True
    Nothing -> False

matchRecoverVarApp ::
  ElaborateScope ->
  Set String ->
  Map String SrcType ->
  Map String String ->
  String ->
  NonEmpty SrcType ->
  SrcType ->
  Maybe (Map String SrcType)
matchRecoverVarApp scope params subst renames name args actual
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
          subst' <- bindRecoverParam scope name (headFromPrefix actualName headArgs) subst
          foldM
            (\acc (leftTy, rightTy) -> matchRecoverType scope params acc renames leftTy rightTy)
            subst'
            (zip expectedArgs appliedArgs)

    matchRigidVarAppHead expectedName =
      case actual of
        STVarApp actualName actualArgs
          | recoverTypeHeadMatches scope expectedName actualName && expectedArgCount == length (toListNE actualArgs) ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType scope params acc renames leftTy rightTy)
                subst
                (zip expectedArgs (toListNE actualArgs))
        _ -> Nothing

    toConHead actualName [] = STBase actualName
    toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

    toVarHead actualName [] = STVar actualName
    toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

bindRecoverParam :: ElaborateScope -> String -> SrcType -> Map String SrcType -> Maybe (Map String SrcType)
bindRecoverParam scope name actual subst =
  case Map.lookup name subst of
    Nothing -> Just (Map.insert name actual subst)
    Just existing
      | alphaEqSrcTypeInScope scope existing actual ->
          Just subst
      | Just existingTy <- srcTypeToElabTypeMaybeInScope scope existing,
        Just actualTy <- srcTypeToElabTypeMaybeInScope scope actual,
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
      typeViewIdentity = typeViewIdentity identityView,
      typeViewHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities identityView,
            elabTypeHeadIdentities ty
          ],
      typeViewBinderIdentities = elabTypeBinderIdentities ty
    }
  where
    identityView =
      sourceTypeViewInScope scope identityTy
    displayTy =
      recoverSourceType scope (elabTypeToSrcTypeWith X.typeBinderRefName ty)
    identityTy =
      recoverSourceType scope (elabTypeToIdentitySrcTypeWith elabTypeBinderIdentityName ty)

elabTypeHeadIdentities :: X.Ty v -> Map String SymbolIdentity
elabTypeHeadIdentities =
  go
  where
    go :: X.Ty a -> Map String SymbolIdentity
    go ty =
      case ty of
        X.TVarRef {} -> Map.empty
        X.TArrow dom cod -> mergeSymbolIdentityMaps [go dom, go cod]
        X.TBaseWithIdentity identity base -> identityHead base identity
        X.TConWithIdentity identity base args -> mergeSymbolIdentityMaps (identityHead base identity : map go (NE.toList args))
        X.TVarAppRef _ args -> mergeSymbolIdentityMaps (map go (NE.toList args))
        X.TForallRef _ mb body -> mergeSymbolIdentityMaps [maybe Map.empty go mb, go body]
        X.TMuRef _ body -> go body
        X.TBottom -> Map.empty

    identityHead _ Nothing =
      Map.empty
    identityHead (Graph.BaseTy name) (Just identity) =
      Map.fromList
        [ (alias, identity)
        | alias <- name : symbolIdentityAliasNames identity,
          not (null alias)
        ]

elabTypeBinderIdentities :: X.Ty v -> Map String TypeBinderIdentity
elabTypeBinderIdentities =
  go
  where
    go :: X.Ty a -> Map String TypeBinderIdentity
    go ty =
      case ty of
        X.TVarRef ref -> binder ref
        X.TArrow dom cod -> go dom <> go cod
        X.TBaseWithIdentity {} -> Map.empty
        X.TConWithIdentity _ _ args -> foldMap go args
        X.TVarAppRef ref args -> binder ref <> foldMap go args
        X.TForallRef ref mb body -> binder ref <> maybe Map.empty go mb <> go body
        X.TMuRef ref body -> binder ref <> go body
        X.TBottom -> Map.empty

    binder ref =
      typeBinderAliasIdentityMap [(X.typeBinderRefName ref, X.typeBinderRefIdentity ref)]

elabTypeBinderIdentityName :: X.TypeBinderRef -> String
elabTypeBinderIdentityName ref =
  typeBinderIdentityStableName (X.typeBinderRefIdentity ref)

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
  let (refs, generator) = freshTypeBinderRefsAfterHeadIdentities builtinTypeHeadIdentities (Set.toList (freeSrcTypeVars ty))
   in fst <$> srcTypeToElabTypeWithHeadIdentities builtinTypeHeadIdentities refs generator ty

srcTypeToElabTypeInScope :: ElaborateScope -> SrcTy n v -> Either ProgramError ElabType
srcTypeToElabTypeInScope scope ty =
  srcTypeToElabTypeInScopeWithHeadIdentities scope Map.empty ty

srcTypeToElabTypeInScopeWithHeadIdentities :: ElaborateScope -> Map String SymbolIdentity -> SrcTy n v -> Either ProgramError ElabType
srcTypeToElabTypeInScopeWithHeadIdentities scope extraHeadIdentities ty =
  let headIdentities = Map.union extraHeadIdentities (typeHeadIdentitiesInScope scope)
      (refs, generator) = sourceTypeBinderRefsInScope headIdentities scope Map.empty ty
   in fst <$> srcTypeToElabTypeWithHeadIdentities headIdentities refs generator ty

srcTypeToElabTypeMaybeInScope :: ElaborateScope -> SrcTy n v -> Maybe ElabType
srcTypeToElabTypeMaybeInScope scope =
  either (const Nothing) Just . srcTypeToElabTypeInScope scope

typeViewToElabType :: ElaborateScope -> TypeView -> Either ProgramError ElabType
typeViewToElabType scope view =
  fst <$> srcTypeToElabTypeWithHeadIdentities headIdentities refs generator ty
  where
    ty =
      lowerTypeView scope view

    headIdentities =
      Map.union (typeViewHeadIdentityLookupAliases view) (typeHeadIdentitiesInScope scope)

    (refs, generator) =
      typeViewBinderRefs headIdentities view ty

loweredExpectedTypeToElabType :: ElaborateScope -> LoweredBinding -> Either ProgramError ElabType
loweredExpectedTypeToElabType scope lowered =
  case loweredBindingExpectedTypeView lowered of
    Just view -> typeViewToElabType scope view
    Nothing -> srcTypeToElabTypeInScope scope (loweredBindingExpectedType lowered)

typeViewBinderRefs :: Map String SymbolIdentity -> TypeView -> SrcType -> (Map String X.TypeBinderRef, IdentityGenerator)
typeViewBinderRefs headIdentities view ty =
  (Map.union knownRefs freshRefs, generator)
  where
    baseBinderIdentities =
      typeViewBinderIdentities view

    binderIdentities =
      mergeTypeBinderIdentityMaps [baseBinderIdentities, pairedBinderIdentities]

    pairedBinderIdentities =
      Map.fromList
        [ (identityName, identity)
        | (identityName, _) <- Map.toList (typeViewVarPairs view),
          Just identity <- [typeViewBinderIdentityForAlias view identityName]
        ]

    knownRefs =
      typeBinderIdentityRefs binderIdentities

    generator0 =
      identityGeneratorAfter
        ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
            ++ concatMap typeBinderGeneratedIdentities (Map.elems baseBinderIdentities)
        )

    missingFreeNames =
      filter (`Map.notMember` knownRefs) (Set.toList (freeSrcTypeVars ty))

    (freshRefs, generator) =
      freshTypeBinderRefs missingFreeNames generator0

sourceTypeBinderRefsInScope :: Map String SymbolIdentity -> ElaborateScope -> Map String X.TypeBinderRef -> SrcTy n v -> (Map String X.TypeBinderRef, IdentityGenerator)
sourceTypeBinderRefsInScope headIdentities scope baseRefs ty =
  (knownRefs `Map.union` baseRefs `Map.union` freshRefs, generator)
  where
    binderIdentities =
      sourceTypeBinderIdentitiesInScope scope ty

    knownRefs =
      typeBinderIdentityRefs binderIdentities

    generator0 =
      identityGeneratorAfter
        ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
            ++ concatMap typeBinderGeneratedIdentities (Map.elems binderIdentities)
            ++ concatMap (typeBinderGeneratedIdentities . X.typeBinderRefIdentity) (Map.elems baseRefs)
        )

    missingFreeNames =
      filter (`Map.notMember` knownRefs) (Set.toList (freeSrcTypeVars ty))

    (freshRefs, generator) =
      freshTypeBinderRefs missingFreeNames generator0

freshTypeBinderRefsAfterHeadIdentities :: Map String SymbolIdentity -> [String] -> (Map String X.TypeBinderRef, IdentityGenerator)
freshTypeBinderRefsAfterHeadIdentities headIdentities names =
  freshTypeBinderRefs names generator0
  where
    generator0 =
      identityGeneratorAfter (concatMap symbolGeneratedIdentities (Map.elems headIdentities))

typeViewHeadIdentityLookupAliases :: TypeView -> Map String SymbolIdentity
typeViewHeadIdentityLookupAliases view =
  mergeSymbolIdentityMaps [typeViewHeadIdentities view, aliases, pairedAliases]
  where
    aliases =
      symbolIdentityAliasMap (Map.elems (typeViewHeadIdentities view))

    pairedAliases =
      Map.fromList
        [ (name, identity)
        | name <- Set.toList mentionedHeadNames,
          Just identity <- [typeViewHeadIdentityForAlias view name]
        ]

    mentionedHeadNames =
      typeHeadNamesSrcType (typeViewIdentity view)
        <> typeHeadNamesSrcType (typeViewDisplay view)

typeBinderIdentityRefs :: Map String TypeBinderIdentity -> Map String X.TypeBinderRef
typeBinderIdentityRefs identities =
  Map.fromList
    [ (name, refFor identity)
    | (name, identity) <- Map.toList identities
    ]
  where
    refFor identity =
      X.typeBinderRefFromIdentity identity (preferredName identity)

    preferredName identity =
      case Map.lookup identity namesByIdentity of
        Just names ->
          case filter (/= typeBinderIdentityStableName identity) (sort names) of
            name : _ -> name
            [] -> typeBinderIdentityStableName identity
        Nothing -> typeBinderIdentityStableName identity

    namesByIdentity =
      Map.fromListWith
        (++)
        [ (identity, [name])
        | (name, identity) <- Map.toList identities
        ]

freshTypeBinderRefs :: [String] -> IdentityGenerator -> (Map String X.TypeBinderRef, IdentityGenerator)
freshTypeBinderRefs names generator0 =
  foldr fresh (Map.empty, generator0) names
  where
    fresh name (refs, generator) =
      let (ref, generator') = X.sourceTypeBinderRefForName name generator
       in (Map.insert name ref refs, generator')

srcTypeToElabTypeWithScope :: ElaborateScope -> Map String X.TypeBinderRef -> IdentityGenerator -> SrcTy n v -> Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithScope scope refs generator ty =
  srcTypeToElabTypeWithHeadIdentities headIdentities refs' generator ty
  where
    headIdentities =
      typeHeadIdentitiesInScope scope

    refs' =
      fst (sourceTypeBinderRefsInScope headIdentities scope refs ty)

srcTypeToElabTypeWithHeadIdentities ::
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithHeadIdentities =
  srcTypeToElabTypeWithHeadIdentitiesBound Set.empty

srcTypeToElabTypeWithHeadIdentitiesBound ::
  Set String ->
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithHeadIdentitiesBound boundNames headIdentities refs generator ty = case ty of
  STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (X.TVarRef ref, generator)
  STArrow dom cod ->
    do
      (dom', generator1) <- go refs generator dom
      (cod', generator2) <- go refs generator1 cod
      Right (X.TArrow dom' cod', generator2)
  STBase name ->
    Right (X.TBaseWithIdentity (sourceTypeHeadIdentity name) (Graph.BaseTy (Builtins.normalizeBuiltinTypeReference name)), generator)
  STCon name args ->
    do
      (args', generator') <- mapAccumSrcTypes refs generator args
      Right (X.TConWithIdentity (sourceTypeHeadIdentity name) (Graph.BaseTy (Builtins.normalizeBuiltinTypeReference name)) args', generator')
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
    let (ref, generator1) = sourceTypeBinderRefOrFresh (Set.member name boundNames) refs name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithHeadIdentitiesBound boundNames headIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithHeadIdentitiesBound boundNames' headIdentities refs' generator2 body
          Right (X.TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFresh (Set.member name boundNames) refs name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithHeadIdentitiesBound boundNames' headIdentities refs' generator1 body
          Right (X.TMuRef ref body', generator2)
  STBottom ->
    Right (X.TBottom, generator)
  where
    go =
      srcTypeToElabTypeWithHeadIdentitiesBound boundNames headIdentities

    sourceTypeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name

    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (ProgramPipelineError ("unresolved source type binder `" ++ name ++ "` reached finalization"))

    sourceTypeBinderRefOrFresh shadowed env name gen =
      if shadowed
        then X.sourceTypeBinderRefForName name gen
        else
          case Map.lookup name env of
            Just ref -> (ref, gen)
            Nothing -> X.sourceTypeBinderRefForName name gen

    mapAccumSrcTypes refs0 generator0 (arg :| args) = do
      (arg', generator1) <- go refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- go refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')


srcBoundToElabBoundWithHeadIdentitiesBound ::
  Set String ->
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcBound n ->
  Either ProgramError (Maybe X.BoundType, IdentityGenerator)
srcBoundToElabBoundWithHeadIdentitiesBound boundNames headIdentities refs generator (SrcBound boundTy) =
  case srcTypeToElabTypeWithHeadIdentitiesBound boundNames headIdentities refs generator boundTy of
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
unambiguousDataTypeHeadIdentities =
  symbolIdentityAliasMap . map dataInfoSymbol . Map.elems

builtinTypeHeadIdentities :: Map String SymbolIdentity
builtinTypeHeadIdentities =
  Map.fromList
    [ (name, Builtins.builtinTypeIdentity name)
    | name <- Set.toList Builtins.builtinTypeNames
    ]
