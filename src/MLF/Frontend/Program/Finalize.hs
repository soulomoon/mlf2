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
    recoverSourceTypeMetadataLight,
    elabTypeToRecoveredTypeView,
    typeViewToElabType,
    srcTypeToElabTypeInScope,
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
import Data.List (find, sort)
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
    restrictPreparedExternalBindingsByKeys,
    extendPreparedExternalBindingTypeIdentities,
    reservePreparedExternalBindingIdentities,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming,
    freshenTypeAbsAgainstEnv,
    unionPreparedExternalBindings,
  )
import MLF.Elab.TermClosure (closeTermWithSchemeSubstRefsIfNeeded, renameTermTypeVars)
import MLF.Elab.Types (XmlfTerm, ElabType)
import qualified MLF.Elab.Types as X
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Frontend.ConstraintGen
  ( BindingKey (..),
    bindingKeyForTermReference,
    ExternalBinding (..),
    ExternalBindingIdentity,
    ExternalBindingMode (..),
    externalBindingDetails,
    externalBindingIdentityFromDetails,
    externalBindingIdentityFromDeferredRef,
  )
import MLF.Frontend.Normalize (normalizeExpr, normalizeType)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol (lookupSymbolIdentityAlias, lookupSymbolIdentityExact, sameSymbolIdentity, symbolIdentityAliasMap, symbolIdentityAliasMapWith, symbolIdentityStableName, symbolUniqueIdentity)
import MLF.Frontend.Program.Elaborate
  ( ElaborateScope,
    elaborateScopeDataTypes,
    elaborateScopeDataTypesByIdentity,
    elaborateScopeRuntimeTypeViews,
    elaborateScopeUniqueDataTypes,
    elaborateScopeValueInfos,
    elaborateScopeValueRuntimeAliases,
    classInfoForConstraint,
    constructorBindingSourceTypeView,
    constructorBindingUsesStructuralPlaceholder,
    constructorStructuralArgs,
    constructorStructuralHandlerType,
    constructorTypeView,
    diagnosticTypeViewDisplay,
    lookupEvidenceMethodByClassViews,
    lowerType,
    lowerTypeView,
    lowerTypeViewWithIdentities,
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
    ClassApplicationKey,
    TypeView,
    typeViewBinderIdentities,
    typeViewDisplay,
    typeViewHeadIdentities,
    typeViewIdentity,
    TypeBinderSubst,
    TypeViewSubst,
    ValueInfo (..),
    applyConstraintInfoSubst,
    applyTypeBinderSubst,
    applyTypeViewSubst,
    constructorRefFromInfo,
    constructorRefSymbol,
    constructorOwnerRuntimeTypeTrackable,
    constructorOwnerShapes,
    constructorShapeFromInfo,
    constructorShapeForalls,
    constructorShapeArgs,
    constructorShapeName,
    constructorShapeResultView,
    constructorShapeResultIdentity,
    constructorInfoIdentityName,
    dataInfoIdentityHeadName,
    dataInfoHeadIdentityLookupAliases,
    dataInfoIdentityName,
    dataInfoIdentityQualifiedName,
    dataParams,
    dataParamBinders,
    deferredCasePlaceholder,
    deferredConstructorPlaceholder,
    deferredMethodPlaceholder,
    deferredMethodName,
    deferredProgramObligationRef,
    deferredProgramObligationGeneratedIdentities,
    emptyTypeBinderSubst,
    freeTypeBinderIdentitiesTypeViews,
    constraintTypeView,
    constraintClassApplicationKey,
    lookupInstanceMethod,
    ctorName,
    ctorArgs,
    ctorResult,
    lookupTypeViewSubst,
    lookupMethodParamViewSubst,
    methodType,
    methodTypeView,
    methodParamTypeViews,
    methodResultTypeView,
    methodResultTypeViewFrom,
    methodName,
    methodInfoOwnerClassSymbolIdentity,
    methodInfoSymbolIdentity,
    methodParamBinderIdentities,
    mergeTypeBinderIdentityMaps,
    mergeSymbolIdentityMaps,
    loweredBindingConstructorRef,
    loweredBindingIdentityGeneratedIdentities,
    loweredIdentityDetails,
    loweredBindingName,
    resolvedVarFromLoweredBinding,
    resolvedVarFromValueInfo,
    ordinaryValueTypeView,
    SymbolIdentity,
    splitArrows,
    splitForalls,
    specializeMethodTypeView,
    specializeQuantifiedTypeView,
    typeViewBinderIdentityForAlias,
    typeViewBinderIdentityAliasEntries,
    typeViewIsBareBinderIdentity,
    substituteTypeVar,
    typeViewSubstFromParamIdentities,
    typeViewHeadIdentityLookupAliases,
    typeViewGeneratedIdentities,
    typeViewMergeBinderIdentities,
    typeViewMergeHeadIdentities,
    typeViewWithIdentityMaps,
    typeParamBinderIdentity,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToTypeViewSubst,
    typeBinderAliasIdentityMap,
    uniqueEvidenceMethodMatch,
    valueInfoRuntimeDetails,
    lookupTypeBinderSubstByIdentity,
    lookupTypeBinderSubstViewByIdentity,
    insertTypeBinderSubstViewWithIdentity,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), NormSurfaceExpr, SrcBound (..), SrcTy (..), SrcType, SurfaceExpr, TermReference (..), termReferenceName, typeParamName)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, freshNameLike, matchTypeRefs, splitForallsRefs, substTypeCaptureRef)
import MLF.Types.Identity
  ( DeferredRef,
    deferredRefIdentity,
    deferredRefName,
    IdentityGenerator,
    LocalRef,
    localRefGeneratedIdentities,
    TypeBinderIdentity,
    StructuralTypeBinderRole (StructuralSelfBinder),
    UniqueIdentity,
    freshLocalRef,
    idDetailsAliasMapWith,
    idDetailsAliasNamesWith,
    idDetailsRuntimeName,
    identityGeneratorAfter,
    renameDeferredRef,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
    typeBinderIdentityStructural,
    typeBinderIdentityAliasMap,
    typeBinderIdentityStableName,
    uniqueIdentityStableName,
  )
import MLF.Util.Timing (TimingConfig(..), defaultTimingConfig, timeProgramOperationIO)

data FinalizeContext = FinalizeContext
  { finalizeContextScope :: ElaborateScope,
    finalizeContextRuntimeBindings :: PreparedExternalBindings,
    finalizeContextRuntimeSourceTypes :: Map String SrcType,
    finalizeContextRuntimeTypeEnv :: Map String ElabType,
    finalizeContextRuntimeBindingIndex :: RuntimeExternalBindingIndex
  }

data ModuleFinalizeContext = ModuleFinalizeContext
  { moduleFinalizeContextBase :: FinalizeContext,
    moduleFinalizeContextBindingReads :: Map ModuleBindingReadKey ModuleBindingReadContext
  }

type ModuleBindingReadKey = X.ResolvedTermIdentityKey

data ModuleBindingReadContext = ModuleBindingReadContext
  { moduleBindingReadLowered :: LoweredBinding,
    moduleBindingReadResolvedFreeVars :: Either ProgramError (),
    moduleBindingReadExternalBindings :: Either ProgramError PreparedExternalBindings,
    moduleBindingReadNormalizedExpr :: Either ProgramError NormSurfaceExpr,
    moduleBindingReadCheckContext :: BindingCheckReadContext
  }

data DeferredExternalBindingIndex = DeferredExternalBindingIndex
  { deferredExternalBindingByRef :: Map DeferredRef DeferredProgramObligation,
    deferredExternalBindingByKey :: Map ModuleBindingReadKey DeferredProgramObligation,
    deferredExternalBindingRefByAlias :: Map String DeferredRef
  }

data RuntimeExternalBindingIndex = RuntimeExternalBindingIndex
  { runtimeExternalBindingByKey :: Map ModuleBindingReadKey X.ResolvedVar,
    runtimeExternalBindingKeyByAlias :: Map String ModuleBindingReadKey
  }

data BindingCheckReadContext = BindingCheckReadContext
  { bindingCheckExpectedType :: Either ProgramError ElabType,
    bindingCheckExpectedTypeForCompare :: Either ProgramError ElabType,
    bindingCheckRecoveredExpectedSourceType :: Either ProgramError SrcType
  }

data SurfaceBindingReference = SurfaceBindingReference
  { surfaceBindingReferenceKey :: BindingKey,
    surfaceBindingReferenceDisplayName :: String
  }
  deriving (Eq, Ord, Show)

surfaceBindingReferenceFromTermReference :: TermReference -> SurfaceBindingReference
surfaceBindingReferenceFromTermReference reference =
  SurfaceBindingReference
    { surfaceBindingReferenceKey = bindingKeyForTermReference reference,
      surfaceBindingReferenceDisplayName = termReferenceName reference
    }

data SurfaceExternalBindingInput = SurfaceExternalBindingInput
  { surfaceExternalBindingInputName :: String,
    surfaceExternalBindingInputView :: TypeView,
    surfaceExternalBindingInputMode :: ExternalBindingMode,
    surfaceExternalBindingInputIdentity :: ExternalBindingIdentity
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
  let runtimeTypeViews = runtimeTypeViewsWithVisibleConstructors scope
      runtimeSourceTypes = Map.map typeViewDisplay runtimeTypeViews
  runtimeTypeEnv <- traverse (typeViewToElabType scope) runtimeTypeViews
  let runtimeIndex = runtimeExternalBindingIndexFromScope scope runtimeTypeEnv
      runtimeSourceTypesWithAliases = runtimeSourceTypesWithIdentityAliases runtimeSourceTypes runtimeIndex
  runtimeBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      scope
      -- Runtime siblings already carry resolved SchemeInfo in the prepared
      -- elaboration/typecheck env. Function-shaped constrained siblings must not
      -- get a second graph scheme binder identity space.
      (externalBindingModeForRuntime scope runtimeSourceTypes runtimeIndex)
      (runtimeExternalBindingIdentityByAlias runtimeIndex)
      runtimeTypeViews
  pure
    FinalizeContext
      { finalizeContextScope = scope,
        finalizeContextRuntimeBindings = runtimeBindings,
        finalizeContextRuntimeSourceTypes = runtimeSourceTypesWithAliases,
        finalizeContextRuntimeTypeEnv = runtimeTypeEnv,
        finalizeContextRuntimeBindingIndex = runtimeIndex
      }

runtimeTypeViewsWithVisibleConstructors :: ElaborateScope -> Map String TypeView
runtimeTypeViewsWithVisibleConstructors scope =
  elaborateScopeRuntimeTypeViews scope `Map.union` uniqueConstructorViews
  where
    uniqueConstructorViews =
      Map.fromList
        [ (alias, view)
        | (alias, view : rest) <- Map.toList constructorViewsByAlias,
          all (== view) rest
        ]

    constructorViewsByAlias =
      Map.fromListWith
        (++)
        [ (alias, [constructorTypeView scope ctor])
        | valueInfo@ConstructorValue {valueCtorInfo = ctor} <- elaborateScopeValueInfos scope,
          alias <- elaborateScopeValueRuntimeAliases scope valueInfo
        ]

mkModuleFinalizeContext :: FinalizeContext -> [LoweredBinding] -> Either ProgramError ModuleFinalizeContext
mkModuleFinalizeContext context lowereds0 = do
  validateLoweredBindingsDeferredObligations lowereds0
  let lowereds = lowereds0
      schemeExternalTypeViews = Map.unions (map loweredBindingExternalTypeViews lowereds)
      schemeExternalTypes = Map.map typeViewDisplay schemeExternalTypeViews
      schemeDeferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      schemeDeferredIndex = deferredExternalBindingIndex schemeDeferredObligations
  schemeExternalBindings <-
    prepareSurfaceExternalBindingsWithIdentity
      (finalizeContextScope context)
      (const ExternalBindingScheme)
      (externalBindingIdentityFromIndexes (finalizeContextRuntimeBindingIndex context) schemeDeferredIndex)
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
      moduleBindingReadResolvedFreeVars = mapM_ resolveRuntimeType freeReferences,
      moduleBindingReadExternalBindings =
        do
          overlayBindings <-
            prepareSurfaceExternalBindingsForReferences
              scope
              runtimeSourceTypes
              runtimeIndex
              deferredExternalIndex
              (lowerExternalTypeViews scope externalTypeViews0)
              overlayExternalReferences
          Right (overlayBindings `unionPreparedExternalBindings` sharedSchemeBindings `unionPreparedExternalBindings` runtimeBindings),
      moduleBindingReadNormalizedExpr =
        either (Left . ProgramPipelineError . show) Right (normalizeExpr (loweredBindingSurfaceExpr lowered)),
      moduleBindingReadCheckContext =
        BindingCheckReadContext
          { bindingCheckExpectedType = expectedType,
            bindingCheckExpectedTypeForCompare = stripVacuousForalls <$> expectedType,
            bindingCheckRecoveredExpectedSourceType = recoverElabSourceType scope <$> expectedType
          }
    }
  where
    scope = finalizeContextScope context
    deferredObligations = loweredBindingDeferredObligations lowered
    deferredExternalIndex = deferredExternalBindingIndex deferredObligations
    externalTypeViews0 = loweredBindingExternalTypeViews lowered
    externalTypes = Map.map typeViewDisplay externalTypeViews0
    runtimeSourceTypes = finalizeContextRuntimeSourceTypes context
    runtimeIndex = finalizeContextRuntimeBindingIndex context
    freeReferences = surfaceFreeBindingReferences (loweredBindingSurfaceExpr lowered)
    externalReferences =
      [ reference
      | reference <- freeReferences,
        Just _ <- [surfaceBindingReferenceValue runtimeIndex deferredExternalIndex externalTypeViews0 reference]
      ]
    externalReferenceKeys = surfaceBindingReferenceKeys externalReferences
    runtimeReferences =
      filter ((`Set.notMember` externalReferenceKeys) . surfaceBindingReferenceKey) freeReferences
    sharedSchemeExternalReferences =
      [ reference
      | reference <- externalReferences,
        Just _ <- [surfaceBindingReferenceValue runtimeIndex deferredExternalIndex schemeExternalTypes reference],
        surfaceBindingReferenceMode scope runtimeSourceTypes runtimeIndex deferredExternalIndex externalTypes reference == ExternalBindingScheme
      ]
    sharedSchemeKeys = surfaceBindingReferenceKeys sharedSchemeExternalReferences
    overlayExternalReferences =
      filter ((`Set.notMember` sharedSchemeKeys) . surfaceBindingReferenceKey) externalReferences
    sharedSchemeBindings = restrictPreparedExternalBindingsByKeys sharedSchemeKeys schemeExternalBindings
    runtimeBindings =
      restrictPreparedExternalBindingsByKeys
        (surfaceBindingReferenceKeys runtimeReferences)
        (finalizeContextRuntimeBindings context)
    expectedType = loweredExpectedTypeToElabType scope lowered

    resolveRuntimeType reference =
      case surfaceBindingReferenceSourceType context externalTypes deferredExternalIndex reference of
        Just _ -> Right ()
        Nothing -> Left (ProgramUnknownValue (surfaceBindingReferenceDisplayName reference))

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
              case validateOpaqueBindingSurface context lowered of
                Right () -> Right (setCheckedBindingType placeholderTy checked)
                Left validationErr -> Left validationErr
          | otherwise -> Right (setCheckedBindingType placeholderTy checked)
        Left err ->
          case validateOpaqueBindingSurface context lowered of
            Right () ->
              finalizeOpaqueUncheckedBindingWithContext context lowered placeholderTy
            Left _ -> Left err
  | otherwise = finalizeBindingWithContext context lowered
  where
    scope = finalizeContextScope context

setCheckedBindingType :: ElabType -> CheckedBinding -> CheckedBinding
setCheckedBindingType ty checked =
  checked
    { checkedBindingResolvedVar = X.mapResolvedVarType (const ty) (checkedBindingResolvedVar checked),
      checkedBindingTerm =
        alignLeadingTypeAbsRefsToType ty
          (mapBindingOccurrences (checkedBindingResolvedVar checked) ty (checkedBindingTerm checked)),
      checkedBindingType = ty
    }

mapBindingOccurrences :: X.ResolvedVar -> ElabType -> XmlfTerm -> XmlfTerm
mapBindingOccurrences target ty =
  go
  where
    go term =
      case term of
        X.EVarNode resolved ->
          X.EVarNode (update resolved)
        X.ELit lit ->
          X.ELit lit
        X.ELam resolved body ->
          X.ELam (update resolved) (go body)
        X.EApp fun arg ->
          X.EApp (go fun) (go arg)
        X.ELet resolved scheme rhs body ->
          X.ELet (update resolved) scheme (go rhs) (go body)
        X.ETyAbsRef ref mbBound body ->
          X.ETyAbsRef ref mbBound (go body)
        X.ETyInst inner inst ->
          X.ETyInst (go inner) inst
        X.ERoll rollTy body ->
          X.ERoll rollTy (go body)
        X.EUnroll body ->
          X.EUnroll (go body)

    update resolved
      | X.resolvedVarSameIdentity target resolved =
          X.mapResolvedVarType (const ty) resolved
      | otherwise = resolved

finalizeOpaqueUncheckedBindingWithContext :: FinalizeContext -> LoweredBinding -> ElabType -> Either ProgramError CheckedBinding
finalizeOpaqueUncheckedBindingWithContext context lowered0 placeholderTy = do
  validateDeferredObligationIdentities (loweredBindingName lowered0) (loweredBindingDeferredObligations lowered0)
  let lowered = lowered0
  pipelineResult <-
    runSurfacePipelineWithContext
      context
      [lowered]
      True
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypeViews lowered)
      (loweredBindingSurfaceExpr lowered)
  let PipelineElabDetailedResult {pedTerm = term0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  term <- finalizeOpaqueDeferredConstructors context (loweredBindingDeferredObligations lowered) tcEnv term0
  let resolvedTerm =
        alignLeadingTypeAbsRefsToType placeholderTy
          . TypeCheck.canonicalizeResolvedTermTypes (runtimeTypeCheckEnv context)
          $ term
      resolvedDeferredObligations =
        annotateDeferredEvidenceResolvedVars resolvedTerm (loweredBindingDeferredObligations lowered)
  let sourceTypeView =
        sourceTypeViewForLoweredBinding context lowered
  acceptResolvedCheckedBinding
    lowered
    sourceTypeView
    resolvedDeferredObligations
    resolvedTerm
    placeholderTy

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

validateOpaqueBindingSurface :: FinalizeContext -> LoweredBinding -> Either ProgramError ()
validateOpaqueBindingSurface context lowered
  | any (not . opaqueSurfaceObligationSupported) (Map.elems (loweredBindingDeferredObligations lowered)) =
      Left (ProgramPipelineError "opaque validation does not support deferred obligations")
  | otherwise =
      case inferOpaqueSurfaceType scope rigidVars runtimeTypeFor Map.empty (loweredBindingSurfaceExpr lowered) of
        Right actualTy
          | opaqueSourceCompatibleWithRigid rigidVars scope actualTy (loweredBindingExpectedType lowered) ->
              validateOpaqueBindingRawSurface scope rigidVars runtimeTypeFor lowered
          | otherwise -> Left (ProgramTypeMismatch actualTy (loweredBindingExpectedType lowered))
        Left err -> Left err
  where
    scope = finalizeContextScope context
    rigidVars = sourceForallBinders (loweredBindingExpectedType lowered)
    runtimeTypeFor =
      opaqueRuntimeSourceType context lowered

opaqueRuntimeSourceType :: FinalizeContext -> LoweredBinding -> BindingKey -> String -> Maybe SrcType
opaqueRuntimeSourceType context lowered key name =
  surfaceBindingReferenceSourceType
    context
    externalTypes
    deferredExternalIndex
    (SurfaceBindingReference key name)
  where
    externalTypes =
      Map.withoutKeys
        (Map.map typeViewDisplay (loweredBindingExternalTypeViews lowered))
        Builtins.builtinOpaqueValueNames
    deferredExternalIndex =
      deferredExternalBindingIndex (loweredBindingDeferredObligations lowered)

metadataLightSurfaceFreeVarSourceType :: FinalizeContext -> Map String SrcType -> DeferredExternalBindingIndex -> String -> Maybe SrcType
metadataLightSurfaceFreeVarSourceType context externalTypes deferredExternalIndex name =
  runtimeExternalBindingSourceTypeByAlias
    scope
    (finalizeContextRuntimeSourceTypes context)
    (finalizeContextRuntimeBindingIndex context)
    name
    <|> deferredExternalBindingSourceTypeByAlias
      externalTypes
      deferredExternalIndex
      name
  where
    scope = finalizeContextScope context

validateOpaqueBindingRawSurface :: ElaborateScope -> Set String -> (BindingKey -> String -> Maybe SrcType) -> LoweredBinding -> Either ProgramError ()
validateOpaqueBindingRawSurface scope rigidVars runtimeTypeFor lowered =
  case inferOpaqueSurfaceTypeIgnoringAscriptions scope rigidVars runtimeTypeFor Map.empty (loweredBindingSurfaceExpr lowered) of
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

inferOpaqueSurfaceType :: ElaborateScope -> Set String -> (BindingKey -> String -> Maybe SrcType) -> Map BindingKey SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceType = inferOpaqueSurfaceTypeWithAscriptions True

inferOpaqueSurfaceTypeIgnoringAscriptions :: ElaborateScope -> Set String -> (BindingKey -> String -> Maybe SrcType) -> Map BindingKey SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceTypeIgnoringAscriptions = inferOpaqueSurfaceTypeWithAscriptions False

inferOpaqueSurfaceTypeWithAscriptions :: Bool -> ElaborateScope -> Set String -> (BindingKey -> String -> Maybe SrcType) -> Map BindingKey SrcType -> SurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypeFor localTypes expr =
  case expr of
    EVarNode reference -> inferReference (bindingKeyForTermReference reference) (termReferenceName reference)
    ELit lit -> Right (literalSourceType lit)
    ELamAnnNode reference ty body ->
      STArrow ty
        <$> inferOpaqueSurfaceTypeWithAscriptions
          keepAscriptions
          scope
          rigidVars
          runtimeTypeFor
          (Map.insert (bindingKeyForTermReference reference) ty localTypes)
          body
    ELamNode {} ->
      Left (ProgramPipelineError "opaque validation needs lambda annotations")
    EApp fun arg -> do
      funTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypeFor localTypes fun
      argTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypeFor localTypes arg
      applyOpaqueFunctionType scope funTy argTy
    ELetNode reference rhs body -> do
      rhsTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypeFor localTypes rhs
      inferOpaqueSurfaceTypeWithAscriptions
        keepAscriptions
        scope
        rigidVars
        runtimeTypeFor
        (Map.insert (bindingKeyForTermReference reference) rhsTy localTypes)
        body
    EAnn inner annTy -> do
      actualTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope rigidVars runtimeTypeFor localTypes inner
      let exact =
            alphaEqSrcTypeInScope scope actualTy annTy
              || alphaEqSrcTypeInScope scope (lowerType scope actualTy) (lowerType scope annTy)
      if exact
        then Right (if keepAscriptions then annTy else actualTy)
        else
          if opaqueSourceCompatibleWithRigid rigidVars scope actualTy annTy
            then Right actualTy
            else Left (ProgramTypeMismatch actualTy annTy)
  where
    inferReference key name =
      case Map.lookup key localTypes <|> runtimeTypeFor key name of
        Just ty -> Right ty
        Nothing -> Left (ProgramUnknownValue name)

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
  let lowered = lowered0
  metadataBinding <- finalizeConstructorBindingFromMetadata context lowered
  case metadataBinding of
    Just checked -> Right checked
    Nothing -> finalizeBindingWithSurfacePipeline context lowered

finalizeBindingWithSurfacePipeline :: FinalizeContext -> LoweredBinding -> Either ProgramError CheckedBinding
finalizeBindingWithSurfacePipeline context lowered0 = do
  let lowered = lowered0
  pipelineResult <-
    runSurfacePipelineWithContext
      context
      [lowered]
      False
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypeViews lowered)
      (loweredBindingSurfaceExpr lowered)
  let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  (term, actualTy) <-
    finalizeDeferredObligationsForBinding context lowered (loweredBindingDeferredObligations lowered) tcEnv term0 actualTy0 (loweredBindingExpectedType lowered)
  finalizeCheckedBindingFromTerm context lowered term actualTy

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
          Left _ -> lowered0
  metadataBinding <- finalizeConstructorBindingFromMetadata context lowered
  case metadataBinding of
    Just checked -> Right checked
    Nothing -> do
      readContext <- mbReadContext
      let stampedLowered = moduleBindingReadLowered readContext
      pipelineResult <-
        runLoweredSurfacePipelineWithModuleContext
          moduleContext
          False
          stampedLowered
      let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
            pipelineResult
      (term, actualTy) <-
        finalizeDeferredObligationsForBinding context stampedLowered (loweredBindingDeferredObligations stampedLowered) tcEnv term0 actualTy0 (loweredBindingExpectedType stampedLowered)
      finalizeCheckedBindingFromTermWithReadContext context (Just (moduleBindingReadCheckContext readContext)) stampedLowered term actualTy
  where
    context = moduleFinalizeContextBase moduleContext

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
      let lowered = lowered0
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
                [lowered]
                forceUnchecked
                (loweredBindingDeferredObligations lowered)
                (loweredBindingExternalTypeViews lowered)
                (loweredBindingSurfaceExpr lowered)
          finalizePipelineBindingResult timing label context lowered pipelineResult
        Left err -> pure (Left err)

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
                forceUnchecked
                lowered
          let mbCheckContext =
                case mbReadContext of
                  Right readContext -> Just (moduleBindingReadCheckContext readContext)
                  Left _ -> Nothing
          finalizePipelineBindingResultWithReadContext timing label context mbCheckContext lowered pipelineResult
        Left err -> pure (Left err)
  where
    context = moduleFinalizeContextBase moduleContext

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
  extEnv0 <- combinePreparedExternalBindings extEnvs
  let extEnv = extendPreparedWithLoweredTypeIdentities lowereds extEnv0
      rootExtEnvs =
        zipWith
          (\lowered extEnvForRoot -> extendPreparedWithLoweredTypeIdentities [lowered] extEnvForRoot)
          lowereds
          extEnvs
  normExprs <- traverse moduleBindingReadNormalizedExpr readContexts
  keyedExprs <- moduleLayerKeyedExprs lowereds normExprs
  let rootPrepared =
        Map.fromList [(key, rootExtEnv) | ((key, _, _), rootExtEnv) <- zip keyedExprs rootExtEnvs]
  pure (extEnv, rootPrepared, keyedExprs)

prepareModuleLayerPipelineInputsWithTiming ::
  TimingConfig ->
  String ->
  [LoweredBinding] ->
  [ModuleBindingReadContext] ->
  IO (Either ProgramError (PreparedExternalBindings, Map ModuleBindingReadKey PreparedExternalBindings, [(ModuleBindingReadKey, String, NormSurfaceExpr)]))
prepareModuleLayerPipelineInputsWithTiming timing label lowereds readContexts =
  runExceptT $ do
    (extEnv, rootExtEnvs) <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $ do
        mapM_ moduleBindingReadResolvedFreeVars readContexts
        extEnvs <- traverse moduleBindingReadExternalBindings readContexts
        extEnv0 <- combinePreparedExternalBindings extEnvs
        let extEnv = extendPreparedWithLoweredTypeIdentities lowereds extEnv0
            rootExtEnvs =
              zipWith
                (\lowered extEnvForRoot -> extendPreparedWithLoweredTypeIdentities [lowered] extEnvForRoot)
                lowereds
                extEnvs
        pure (extEnv, rootExtEnvs)
    normExprs <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        traverse moduleBindingReadNormalizedExpr readContexts
    keyedExprs <- fromProgramEither (moduleLayerKeyedExprs lowereds normExprs)
    let rootPrepared =
          Map.fromList [(key, rootExtEnv) | ((key, _, _), rootExtEnv) <- zip keyedExprs rootExtEnvs]
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
    pipelineResult0 <- fromProgramEither pipelineResult
    let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
          pipelineResult0
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
  | otherwise = do
      (term, expectedTy) <- metadataConstructorTerm context lowered
      let resolvedTerm =
            alignLeadingTypeAbsRefsToType expectedTy
              . TypeCheck.canonicalizeResolvedTermTypes (runtimeTypeCheckEnv context)
              $ term
          resolvedDeferredObligations =
            annotateDeferredEvidenceResolvedVars resolvedTerm (loweredBindingDeferredObligations lowered)
      Just
        <$> acceptResolvedCheckedBinding
          lowered
          (sourceTypeViewForLoweredBinding context lowered)
          resolvedDeferredObligations
          resolvedTerm
          expectedTy

metadataConstructorTerm :: FinalizeContext -> LoweredBinding -> Either ProgramError (XmlfTerm, ElabType)
metadataConstructorTerm context lowered = do
  (dataInfo, ctorInfo) <-
    case lookupConstructorBindingRuntime scope lowered of
      Just found -> Right found
      Nothing -> Left (ProgramPipelineError ("missing constructor metadata for `" ++ loweredBindingName lowered ++ "`"))
  if sameSymbolIdentity (dataInfoSymbol dataInfo) (ctorOwningTypeIdentity ctorInfo)
    then pure ()
    else Left (ProgramPipelineError ("inconsistent constructor metadata for `" ++ loweredBindingName lowered ++ "`"))
  expectedTy <- loweredExpectedTypeToElabType scope lowered
  let constructorHeadIdentities =
        maybe Map.empty typeViewHeadIdentities $
          loweredBindingExpectedTypeView lowered <|> loweredBindingSourceTypeView lowered
  term0 <- inlineConstructorHead ConstructorBindingTerm scope constructorHeadIdentities (constructorBindingQuantifiedOwnerParams lowered dataInfo) ctorInfo emptyTypeBinderSubst
  let term = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (schemeFromType expectedTy) term0
  Right (term, expectedTy)
  where
    scope = finalizeContextScope context

constructorBindingQuantifiedOwnerParams :: LoweredBinding -> DataInfo -> [(String, TypeBinderIdentity)]
constructorBindingQuantifiedOwnerParams lowered dataInfo =
  filter quantifiedOwnerParam (dataParamBinders dataInfo)
  where
    quantifiedNames =
      Set.fromList (map fst (fst (splitForalls (loweredBindingExpectedType lowered))))

    quantifiedOwnerParam (name, identity) =
      name `Set.member` quantifiedNames
        || expectedViewQuantifies identity

    expectedViewQuantifies identity =
      case loweredBindingExpectedTypeView lowered of
        Just view ->
          any
            (\name -> typeViewBinderIdentityForAlias view name == Just identity)
            quantifiedNames
        Nothing -> False

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
  validateLoweredBindingsDeferredObligations lowereds0
  let lowereds =
        zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
      deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
      groupExpr = groupedBindingExpr lowereds
  pipelineResult <-
    runSurfacePipelineWithContext context lowereds False deferredObligations externalTypeViews0 groupExpr
  let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
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
    fromProgramEither (validateLoweredBindingsDeferredObligations lowereds0)
    let lowereds =
          zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
        deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
        externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
        groupExpr = groupedBindingExpr lowereds
    pipelineResult <-
      timeFinalizeEither timing (label ++ ".pipeline") $
        runSurfacePipelineWithContextWithTiming timing (label ++ ".pipeline") context lowereds False deferredObligations externalTypeViews0 groupExpr
    let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
          pipelineResult
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
        EResolvedLet
          (loweredIdentityDetails (loweredBindingIdentity lowered))
          (loweredBindingName lowered)
          (EAnn (loweredBindingSurfaceExpr lowered) (groupedBindingAnnotationType lowered))
          body
    )
    (ELit (LBool True))

groupedBindingAnnotationType :: LoweredBinding -> SrcType
groupedBindingAnnotationType lowered =
  maybe
    (loweredBindingSourceType lowered)
    typeViewIdentity
    (loweredBindingExpectedTypeView lowered)

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
        EVarNode reference ->
          case reference of
            MetadataLightTermReference name
              | name `Set.member` bound -> expr
              | otherwise -> EVar (renameName name)
            ResolvedTermReference (DeferredId ref) _ ->
              let renamedRef = renameDeferredRef (renameName (deferredRefName ref)) ref
               in EResolvedVar (DeferredId renamedRef) (deferredRefName renamedRef)
            ResolvedTermReference {} -> expr
        ELit {} -> expr
        ELamNode reference body ->
          ELamNode reference (go (Set.insert (termReferenceName reference) bound) body)
        EApp fun arg -> EApp (go bound fun) (go bound arg)
        ELetNode reference rhs body ->
          let bound' = Set.insert (termReferenceName reference) bound
           in ELetNode reference (go bound' rhs) (go bound' body)
        ELamAnnNode reference ty body ->
          ELamAnnNode reference ty (go (Set.insert (termReferenceName reference) bound) body)
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

acceptResolvedCheckedBinding :: LoweredBinding -> TypeView -> DeferredObligations -> XmlfTerm -> ElabType -> Either ProgramError CheckedBinding
acceptResolvedCheckedBinding lowered sourceTypeView resolvedDeferredObligations resolvedTerm checkedTy =
  case unresolvedXmlfTermVarRefs resolvedTerm of
    [] -> do
      validateDeferredObligationIdentities (loweredBindingName lowered) resolvedDeferredObligations
      Right
        CheckedBinding
          { checkedBindingResolvedVar = resolvedVarFromLoweredBinding lowered checkedTy,
            checkedBindingSourceTypeView = sourceTypeView,
            checkedBindingDeferredObligations = resolvedDeferredObligations,
            checkedBindingTerm = resolvedTerm,
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

finalizeCheckedBindingFromTermWithReadContext :: FinalizeContext -> Maybe BindingCheckReadContext -> LoweredBinding -> XmlfTerm -> ElabType -> Either ProgramError CheckedBinding
finalizeCheckedBindingFromTermWithReadContext context mbCheckContext lowered term actualTy = do
  let (acceptedTy, acceptedTerm) = stripVacuousForallsAndTypeAbs actualTy term
      acceptedTermTyResult = TypeCheck.typeCheckWithEnv (runtimeTypeCheckEnv context) acceptedTerm
  let acceptChecked = do
        checkedTy <- checkedBindingTypeForStorage lowered acceptedTy
        let sourceTypeView =
              sourceTypeViewForLoweredBinding context lowered
        let acceptedTermWithResolvedVars =
              alignLeadingTypeAbsRefsToType checkedTy
                . TypeCheck.canonicalizeResolvedTermTypes (runtimeTypeCheckEnv context)
                $ acceptedTerm
            resolvedDeferredObligations =
              annotateDeferredEvidenceResolvedVars acceptedTermWithResolvedVars (loweredBindingDeferredObligations lowered)
        acceptResolvedCheckedBinding
          lowered
          sourceTypeView
          resolvedDeferredObligations
          acceptedTermWithResolvedVars
          checkedTy
  case acceptedTermTyResult of
    Right checkedTy
      | meaningfulForallCount checkedTy < meaningfulForallCount acceptedTy ->
          ensureRecoveredTypeCompatible (stripVacuousForalls checkedTy)
    Left _
      | not (termCoversMeaningfulForalls acceptedTy acceptedTerm),
        not (directSurfaceValueCoversMeaningfulForalls acceptedTy) ->
          ensureRecoveredTypeCompatible (stripVacuousForalls acceptedTy)
    _ -> Right ()
  let actualTyForCompare = stripVacuousForalls actualTy
  expectedTyForCompare <- bindingCheckExpectedTypeForCompareFor lowered
  compatible <- checkedOrRecoveredTypesCompatible expectedTyForCompare actualTyForCompare
  if compatible
    then acceptChecked
    else do
      let recoveredActualSrcTy = recoverElabSourceType scope actualTyForCompare
      recoveredExpectedSrcTy <- bindingCheckRecoveredExpectedSourceTypeFor lowered
      Left (ProgramTypeMismatch recoveredActualSrcTy recoveredExpectedSrcTy)
  where
    scope = finalizeContextScope context

    ensureRecoveredTypeCompatible candidateTy = do
      expectedTy <- bindingCheckExpectedTypeForCompareFor lowered
      compatible <- checkedOrRecoveredTypesCompatible expectedTy candidateTy
      if compatible
        then Right ()
        else do
          expectedSrc <- bindingCheckRecoveredExpectedSourceTypeFor lowered
          Left (ProgramTypeMismatch (recoverElabSourceType scope candidateTy) expectedSrc)

    checkedOrRecoveredTypesCompatible expectedTy candidateTy
      | checkedTypesCompatible expectedTy candidateTy = Right True
      | otherwise = do
          recoveredExpectedTy <- recoveredTypeForComparison expectedTy
          recoveredActualTy <- recoveredTypeForComparison candidateTy
          Right
            ( checkedTypesCompatible expectedTy recoveredActualTy
                || checkedTypesCompatible recoveredExpectedTy candidateTy
                || checkedTypesCompatible recoveredExpectedTy recoveredActualTy
            )

    recoveredTypeForComparison =
      typeViewToElabType scope . elabTypeToRecoveredTypeView scope

    checkedTypesCompatible expectedTy candidateTy =
      alphaEqType expectedTy candidateTy
        || churchAwareEqType expectedTy candidateTy
        || checkedForallMatches expectedTy candidateTy

    checkedForallMatches expectedTy candidateTy =
      case splitForallsRefs expectedTy of
        ([], _) -> False
        (foralls, body) ->
          case matchTypeRefs (map fst foralls) body candidateTy of
            Right subst
              | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) ->
                  all substitutionRemainsPolymorphic (Map.elems subst)
              | otherwise -> True
            Left _ -> False

    substitutionRemainsPolymorphic ty =
      case ty of
        X.TVarRef {} -> True
        X.TVarAppRef {} -> True
        _ -> False

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
      keepExpected <- checkedOrRecoveredTypesCompatible expectedTy acceptedTy0
      pure $
        if keepExpected
          then expectedTy
          else acceptedTy0

    bindingCheckRecoveredExpectedSourceTypeFor lowered0 =
      case mbCheckContext of
        Just checkContext -> bindingCheckRecoveredExpectedSourceType checkContext
        Nothing -> recoverElabSourceType scope <$> loweredExpectedTypeToElabType scope lowered0

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
      case directSurfaceValueReference (loweredBindingSurfaceExpr lowered) of
        Just reference ->
          case directSurfaceValueSourceType reference of
            Just sourceTy ->
              let targetTy = recoverElabSourceType scope (stripVacuousForalls ty)
                  sourceTy' = lowerType scope sourceTy
                  targetTy' = lowerType scope targetTy
               in alphaEqSrcTypeInScope scope sourceTy targetTy
                    || alphaEqSrcTypeInScope scope sourceTy' targetTy'
                    || sourceForallMatchesWithRigidForallsInScope scope targetTy sourceTy
                    || sourceForallMatchesWithRigidForallsInScope scope targetTy' sourceTy'
            Nothing -> False
        Nothing -> False

    directSurfaceValueSourceType =
      surfaceBindingReferenceSourceType
        context
        bindingExternalSourceTypes
        bindingDeferredExternalIndex

    bindingDeferredExternalIndex =
      deferredExternalBindingIndex (loweredBindingDeferredObligations lowered)

    bindingExternalSourceTypes =
      Map.map typeViewDisplay (loweredBindingExternalTypeViews lowered)

    directSurfaceValueReference :: SurfaceExpr -> Maybe SurfaceBindingReference
    directSurfaceValueReference expr =
      case expr of
        EVarNode reference -> Just (surfaceBindingReferenceFromTermReference reference)
        EAnn inner _ -> directSurfaceValueReference inner
        _ -> Nothing

    meaningfulForallCount :: ElabType -> Int
    meaningfulForallCount ty =
      case ty of
        X.TForallRef ref _ body
          | any (X.typeBinderRefsSameIdentity ref) (freeTypeVarRefsType body) ->
              1 + meaningfulForallCount body
          | otherwise -> meaningfulForallCount body
        _ -> 0

alignLeadingTypeAbsRefsToType :: ElabType -> XmlfTerm -> XmlfTerm
alignLeadingTypeAbsRefsToType expectedTy term =
  case (expectedTy, term) of
    (X.TForallRef targetRef _ targetBody, X.ETyAbsRef termRef mbBound body)
      | X.typeBinderRefsSameIdentity targetRef termRef ->
          X.ETyAbsRef termRef mbBound (alignLeadingTypeAbsRefsToType targetBody body)
      | otherwise ->
          X.ETyAbsRef
            targetRef
            mbBound
            (alignLeadingTypeAbsRefsToType targetBody (renameTermTypeVars [(termRef, targetRef)] body))
    _ -> term

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

validateLoweredBindingsDeferredObligations :: [LoweredBinding] -> Either ProgramError ()
validateLoweredBindingsDeferredObligations lowereds = do
  mapM_ validateLoweredBindingDeferredObligations lowereds
  if Set.size (Set.fromList refs) == length refs
    then Right ()
    else
      Left
        ( ProgramPipelineError
            "lowered bindings contain duplicate deferred identities; lowering must allocate them from the module identity supply"
        )
  where
    refs = concatMap (Map.keys . loweredBindingDeferredObligations) lowereds

annotateDeferredEvidenceResolvedVars :: XmlfTerm -> DeferredObligations -> DeferredObligations
annotateDeferredEvidenceResolvedVars term obligations =
  fmap annotateObligation obligations
  where
    evidenceResolvedVars = collectEvidenceBinderResolvedVars term

    annotateObligation obligation =
      case obligation of
        DeferredMethod deferred ->
          DeferredMethod
            deferred
              { deferredMethodEvidence = annotateDeferredMethodEvidence <$> deferredMethodEvidence deferred,
                deferredMethodLocalEvidence = map annotateEvidenceInfo (deferredMethodLocalEvidence deferred)
              }
        DeferredConstructor {} -> obligation
        DeferredCase {} -> obligation

    annotateDeferredMethodEvidence evidence =
      evidence
        { deferredMethodEvidenceMethod =
            annotateEvidenceMethod (deferredMethodEvidenceMethod evidence)
        }

    annotateEvidenceInfo evidence =
      let methods =
            fmap annotateEvidenceMethod (evidenceMethodsByIdentity evidence)
       in evidence
            { evidenceMethodsByIdentity = methods
            }

    annotateEvidenceMethod method =
      case evidenceMethodResolvedVar method of
        Just existing ->
          case find (X.resolvedVarSameIdentity existing) evidenceResolvedVars of
            Just resolved ->
              method {evidenceMethodResolvedVar = Just (mergeEvidenceResolvedVar method resolved)}
            Nothing -> method
        Nothing -> method

    mergeEvidenceResolvedVar method resolved =
      case evidenceMethodResolvedVar method of
        Just existing
          | X.resolvedVarSameIdentity existing resolved -> resolved
          | otherwise -> existing
        Nothing -> resolved

collectEvidenceBinderResolvedVars :: XmlfTerm -> [X.ResolvedVar]
collectEvidenceBinderResolvedVars = go
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
  loweredBindingIdentityGeneratedIdentities (loweredBindingIdentity lowered)
    ++ maybe [] typeViewGeneratedIdentities (loweredBindingSourceTypeView lowered)
    ++ maybe [] typeViewGeneratedIdentities (loweredBindingExpectedTypeView lowered)
    ++ concatMap generatedIdentitiesInLoweredResolvedLocalIdentity (loweredBindingResolvedLocalIdentities lowered)
    ++ concatMap generatedIdentitiesInLoweredResolvedLocalIdentity (loweredBindingResolvedEvidenceIdentities lowered)
    ++ generatedIdentitiesInDeferredObligations lowered
    ++ concatMap typeViewGeneratedIdentities (Map.elems (loweredBindingExternalTypeViews lowered))

generatedIdentitiesInLoweredResolvedLocalIdentity :: LoweredResolvedLocalIdentity -> [UniqueIdentity]
generatedIdentitiesInLoweredResolvedLocalIdentity identity =
  localRefGeneratedIdentities (loweredResolvedLocalRuntimeRef identity)
    ++ localRefGeneratedIdentities (loweredResolvedLocalRef identity)

generatedIdentitiesInDeferredObligations :: LoweredBinding -> [UniqueIdentity]
generatedIdentitiesInDeferredObligations lowered =
  generatedIdentitiesInDeferredObligationsMap (loweredBindingDeferredObligations lowered)

generatedIdentitiesInDeferredObligationsMap :: DeferredObligations -> [UniqueIdentity]
generatedIdentitiesInDeferredObligationsMap obligations =
  concatMap deferredProgramObligationGeneratedIdentities (Map.elems obligations)

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

runtimeExternalBindingIndexFromScope :: ElaborateScope -> Map String ElabType -> RuntimeExternalBindingIndex
runtimeExternalBindingIndexFromScope scope runtimeTypes =
  RuntimeExternalBindingIndex
    { runtimeExternalBindingByKey = bindingsByKey,
      runtimeExternalBindingKeyByAlias = keysByUniqueAlias
    }
  where
    bindingsByKey =
      Map.fromList
        [ (key, resolved)
        | (key, resolved : rest) <- Map.toList resolvedByKey,
          all (sameRuntimeExternalBinding resolved) rest
        ]

    keysByUniqueAlias =
      Map.fromList
        [ (alias, key)
        | (alias, keys) <- Map.toList keysByAlias,
          [key] <- [Set.toList keys],
          Map.member key bindingsByKey
        ]

    entries =
      [ (alias, key, resolved)
      | (alias, ty) <- Map.toList runtimeTypes,
        Just details <- [Map.lookup alias runtimeDetailsByAlias],
        Just key <- [idDetailsReadKeyMaybe details],
        let resolved =
              X.ResolvedVar
                { X.resolvedVarType = ty,
                  X.resolvedVarDetails = details
                }
      ]

    runtimeDetailsByAlias =
      Map.fromList
        [ (alias, details)
        | (alias, details : rest) <- Map.toList runtimeDetailsCandidatesByAlias,
          all (== details) rest
        ]

    runtimeDetailsCandidatesByAlias =
      Map.fromListWith
        (++)
        [ (alias, [details])
        | valueInfo <- elaborateScopeValueInfos scope,
          Just details <- [valueInfoRuntimeDetails valueInfo],
          alias <- elaborateScopeValueRuntimeAliases scope valueInfo
        ]

    keysByAlias =
      Map.fromListWith
        Set.union
        [ (alias, Set.singleton key)
        | (runtimeName, key, resolved) <- entries,
          alias <- idDetailsAliasNamesWith runtimeName (X.resolvedVarDetails resolved)
        ]

    resolvedByKey =
      Map.fromListWith
        (++)
        [ (key, [resolved])
        | (_, key, resolved) <- entries
        ]

    sameRuntimeExternalBinding left right =
      X.resolvedVarDetails left == X.resolvedVarDetails right
        && ( alphaEqType (X.resolvedVarType left) (X.resolvedVarType right)
               || churchAwareEqType (X.resolvedVarType left) (X.resolvedVarType right)
           )

runtimeExternalBindingResolvedByAlias :: RuntimeExternalBindingIndex -> String -> Maybe X.ResolvedVar
runtimeExternalBindingResolvedByAlias index name = do
  key <- Map.lookup name (runtimeExternalBindingKeyByAlias index)
  Map.lookup key (runtimeExternalBindingByKey index)

runtimeExternalBindingResolvedByKey :: RuntimeExternalBindingIndex -> ModuleBindingReadKey -> Maybe X.ResolvedVar
runtimeExternalBindingResolvedByKey index key =
  Map.lookup key (runtimeExternalBindingByKey index)

runtimeExternalBindingIdentityByAlias :: RuntimeExternalBindingIndex -> String -> Maybe ExternalBindingIdentity
runtimeExternalBindingIdentityByAlias index name = do
  resolved <- runtimeExternalBindingResolvedByAlias index name
  pure (externalBindingIdentityFromDetails (X.resolvedVarDetails resolved))

runtimeExternalBindingSourceTypeByAlias :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> String -> Maybe SrcType
runtimeExternalBindingSourceTypeByAlias scope runtimeSourceTypes index name = do
  resolved <- runtimeExternalBindingResolvedByAlias index name
  runtimeExternalBindingResolvedSourceType scope runtimeSourceTypes resolved

runtimeExternalBindingSourceTypeByKey :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> ModuleBindingReadKey -> Maybe SrcType
runtimeExternalBindingSourceTypeByKey scope runtimeSourceTypes index key = do
  resolved <- runtimeExternalBindingResolvedByKey index key
  runtimeExternalBindingResolvedSourceType scope runtimeSourceTypes resolved

runtimeExternalBindingResolvedSourceType :: ElaborateScope -> Map String SrcType -> X.ResolvedVar -> Maybe SrcType
runtimeExternalBindingResolvedSourceType scope runtimeSourceTypes resolved =
  lookupUniqueAliasValue
    runtimeSourceTypes
    (idDetailsRuntimeName (X.resolvedVarDetails resolved))
    (X.resolvedVarDetails resolved)
    <|> pure (recoverElabSourceType scope (X.resolvedVarType resolved))

runtimeSourceTypesWithIdentityAliases :: Map String SrcType -> RuntimeExternalBindingIndex -> Map String SrcType
runtimeSourceTypesWithIdentityAliases runtimeSourceTypes index =
  runtimeSourceTypes `Map.union` Map.mapMaybe uniqueAlias aliasEntries
  where
    aliasEntries =
      Map.fromListWith
        (++)
        [ (alias, [(X.resolvedVarIdentityKey resolved, ty)])
        | (runtimeName, key) <- Map.toList (runtimeExternalBindingKeyByAlias index),
          Just resolved <- [Map.lookup key (runtimeExternalBindingByKey index)],
          Just ty <- [lookupUniqueAliasValue runtimeSourceTypes runtimeName (X.resolvedVarDetails resolved)],
          alias <- idDetailsAliasNamesWith runtimeName (X.resolvedVarDetails resolved)
        ]

    uniqueAlias entries =
      case (Set.toList (Set.fromList (map fst entries)), entries) of
        ([_], (_, ty) : rest)
          | all ((== ty) . snd) rest -> Just ty
        _ -> Nothing

deferredExternalBindingIndex :: DeferredObligations -> DeferredExternalBindingIndex
deferredExternalBindingIndex obligations =
  DeferredExternalBindingIndex
    { deferredExternalBindingByRef =
        Map.fromList
          [ (deferredProgramObligationRef obligation, obligation)
          | obligation <- Map.elems obligations
          ],
      deferredExternalBindingByKey =
        Map.fromList
          [ (X.idDetailsIdentityKey (DeferredId ref), obligation)
          | obligation <- Map.elems obligations,
            let ref = deferredProgramObligationRef obligation
          ],
      deferredExternalBindingRefByAlias =
        Map.fromList
          [ (alias, ref)
          | (alias, DeferredId ref) <- Map.toList refAliases
          ]
    }
  where
    refAliases =
      idDetailsAliasMapWith
        [ (deferredRefName ref, DeferredId ref)
        | obligation <- Map.elems obligations,
          let ref = deferredProgramObligationRef obligation
        ]

deferredExternalBindingIdentityByAlias :: DeferredExternalBindingIndex -> String -> Maybe ExternalBindingIdentity
deferredExternalBindingIdentityByAlias index name = do
  obligation <- lookupDeferredExternalBindingByAlias name index
  let ref = deferredProgramObligationRef obligation
  pure (externalBindingIdentityFromDeferredRef ref)

deferredExternalBindingIdentityByKey :: DeferredExternalBindingIndex -> ModuleBindingReadKey -> Maybe ExternalBindingIdentity
deferredExternalBindingIdentityByKey index key = do
  obligation <- Map.lookup key (deferredExternalBindingByKey index)
  pure (externalBindingIdentityFromDeferredRef (deferredProgramObligationRef obligation))

deferredExternalBindingSourceTypeByAlias :: Map String SrcType -> DeferredExternalBindingIndex -> String -> Maybe SrcType
deferredExternalBindingSourceTypeByAlias sourceTypes index name = do
  obligation <- lookupDeferredExternalBindingByAlias name index
  let ref = deferredProgramObligationRef obligation
  lookupUniqueAliasValue sourceTypes (deferredRefName ref) (DeferredId ref)

deferredExternalBindingSourceTypeByKey :: Map String SrcType -> DeferredExternalBindingIndex -> ModuleBindingReadKey -> Maybe SrcType
deferredExternalBindingSourceTypeByKey sourceTypes index key = do
  obligation <- Map.lookup key (deferredExternalBindingByKey index)
  let ref = deferredProgramObligationRef obligation
  lookupUniqueAliasValue sourceTypes (deferredRefName ref) (DeferredId ref)

externalBindingIdentityByKey :: RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> ModuleBindingReadKey -> Maybe ExternalBindingIdentity
externalBindingIdentityByKey runtimeIndex deferredIndex key =
  deferredExternalBindingIdentityByKey deferredIndex key
    <|> (externalBindingIdentityFromDetails . X.resolvedVarDetails <$> runtimeExternalBindingResolvedByKey runtimeIndex key)

externalBindingIdentityFromIndexes :: RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> String -> Maybe ExternalBindingIdentity
externalBindingIdentityFromIndexes runtimeIndex deferredIndex name =
  deferredExternalBindingIdentityByAlias deferredIndex name <|> runtimeExternalBindingIdentityByAlias runtimeIndex name

surfaceBindingReferenceKeys :: [SurfaceBindingReference] -> Set BindingKey
surfaceBindingReferenceKeys =
  Set.fromList . map surfaceBindingReferenceKey

surfaceBindingReferenceValue :: Eq a => RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> Map String a -> SurfaceBindingReference -> Maybe a
surfaceBindingReferenceValue runtimeIndex deferredIndex values reference =
  case surfaceBindingReferenceKey reference of
    MetadataLightBindingKey name -> Map.lookup name values
    ResolvedBindingKey key -> do
      identity <- externalBindingIdentityByKey runtimeIndex deferredIndex key
      let details = externalBindingDetails identity
      lookupUniqueAliasValue values (idDetailsRuntimeName details) details

surfaceBindingReferenceIdentity :: RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> SurfaceBindingReference -> Maybe ExternalBindingIdentity
surfaceBindingReferenceIdentity runtimeIndex deferredIndex reference =
  case surfaceBindingReferenceKey reference of
    MetadataLightBindingKey name ->
      externalBindingIdentityFromIndexes runtimeIndex deferredIndex name
    ResolvedBindingKey key ->
      externalBindingIdentityByKey runtimeIndex deferredIndex key

surfaceBindingReferenceSourceType :: FinalizeContext -> Map String SrcType -> DeferredExternalBindingIndex -> SurfaceBindingReference -> Maybe SrcType
surfaceBindingReferenceSourceType context externalTypes deferredIndex reference =
  case surfaceBindingReferenceKey reference of
    MetadataLightBindingKey name ->
      metadataLightSurfaceFreeVarSourceType context externalTypes deferredIndex name
    ResolvedBindingKey key ->
      runtimeExternalBindingSourceTypeByKey scope runtimeTypes runtimeIndex key
        <|> deferredExternalBindingSourceTypeByKey externalTypes deferredIndex key
  where
    scope = finalizeContextScope context
    runtimeTypes = finalizeContextRuntimeSourceTypes context
    runtimeIndex = finalizeContextRuntimeBindingIndex context

surfaceBindingReferenceMode :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> Map String SrcType -> SurfaceBindingReference -> ExternalBindingMode
surfaceBindingReferenceMode scope runtimeTypes runtimeIndex deferredIndex externalTypes reference =
  case surfaceBindingReferenceKey reference of
    MetadataLightBindingKey name ->
      externalBindingModeForObligations scope runtimeTypes runtimeIndex deferredIndex externalTypes name
    ResolvedBindingKey key ->
      externalBindingModeForResolvedKey scope runtimeTypes runtimeIndex deferredIndex key

surfaceExternalBindingInputForReference :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> Map String TypeView -> SurfaceBindingReference -> Either ProgramError (Maybe SurfaceExternalBindingInput)
surfaceExternalBindingInputForReference scope runtimeTypes runtimeIndex deferredIndex externalTypeViews reference =
  case surfaceBindingReferenceValue runtimeIndex deferredIndex externalTypeViews reference of
    Nothing -> Right Nothing
    Just view ->
      case surfaceBindingReferenceIdentity runtimeIndex deferredIndex reference of
        Nothing -> Left (ProgramUnknownValue (surfaceBindingReferenceDisplayName reference))
        Just identity ->
          Right $
            Just
              SurfaceExternalBindingInput
                { surfaceExternalBindingInputName =
                    case surfaceBindingReferenceKey reference of
                      MetadataLightBindingKey name -> name
                      ResolvedBindingKey {} -> idDetailsRuntimeName (externalBindingDetails identity),
                  surfaceExternalBindingInputView = view,
                  surfaceExternalBindingInputMode =
                    surfaceBindingReferenceMode
                      scope
                      runtimeTypes
                      runtimeIndex
                      deferredIndex
                      (Map.map typeViewDisplay externalTypeViews)
                      reference,
                  surfaceExternalBindingInputIdentity = identity
                }

prepareSurfaceExternalBindingsForReferences :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> Map String TypeView -> [SurfaceBindingReference] -> Either ProgramError PreparedExternalBindings
prepareSurfaceExternalBindingsForReferences scope runtimeTypes runtimeIndex deferredIndex externalTypeViews references = do
  inputs <- mapM (surfaceExternalBindingInputForReference scope runtimeTypes runtimeIndex deferredIndex externalTypeViews) references
  prepareSurfaceExternalBindingInputs scope [input | Just input <- inputs]

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
        sameSymbolIdentity (ctorInfoSymbol ctor) identity
    ]
  of
    [match] -> Just match
    _ -> Nothing

prepareSurfacePipelineExternalBindings :: FinalizeContext -> DeferredObligations -> Map String TypeView -> SurfaceExpr -> Either ProgramError PreparedExternalBindings
prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews surfaceExpr = do
  mapM_ resolveReference references
  externalBindings <-
    prepareSurfaceExternalBindingsForReferences
      scope
      runtimeSourceTypes
      runtimeIndex
      deferredIndex
      (lowerExternalTypeViews scope externalTypeViews)
      externalReferences
  let runtimeBindings =
        restrictPreparedExternalBindingsByKeys
          (surfaceBindingReferenceKeys runtimeReferences)
          (finalizeContextRuntimeBindings context)
  pure (externalBindings `unionPreparedExternalBindings` runtimeBindings)
  where
    scope = finalizeContextScope context
    runtimeSourceTypes = finalizeContextRuntimeSourceTypes context
    runtimeIndex = finalizeContextRuntimeBindingIndex context
    deferredIndex = deferredExternalBindingIndex deferredObligations
    externalTypes = Map.map typeViewDisplay externalTypeViews
    references = surfaceFreeBindingReferences surfaceExpr
    externalReferences =
      [ reference
      | reference <- references,
        Just _ <- [surfaceBindingReferenceValue runtimeIndex deferredIndex externalTypeViews reference]
      ]
    externalKeys = surfaceBindingReferenceKeys externalReferences
    runtimeReferences =
      filter ((`Set.notMember` externalKeys) . surfaceBindingReferenceKey) references

    resolveReference reference =
      case surfaceBindingReferenceSourceType context externalTypes deferredIndex reference of
        Just _ -> Right ()
        Nothing -> Left (ProgramUnknownValue (surfaceBindingReferenceDisplayName reference))

runSurfacePipelineWithContext :: FinalizeContext -> [LoweredBinding] -> Bool -> DeferredObligations -> Map String TypeView -> SurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipelineWithContext context lowereds forceUnchecked deferredObligations externalTypeViews0 surfaceExpr = do
  extEnv0 <- prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews0 surfaceExpr
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  let extEnv = extendPreparedWithLoweredTypeIdentities lowereds extEnv0
      runPipeline =
        if not forceUnchecked && Map.null deferredObligations
          then runPipelineElabDetailedWithPreparedExternalBindings
          else runPipelineElabDetailedUncheckedWithPreparedExternalBindings
  either (Left . ProgramPipelineError . renderPipelineError) Right (runPipeline Set.empty extEnv normExpr)

runSurfacePipelineWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  [LoweredBinding] ->
  Bool ->
  DeferredObligations ->
  Map String TypeView ->
  SurfaceExpr ->
  IO (Either ProgramError PipelineElabDetailedResult)
runSurfacePipelineWithContextWithTiming timing label context lowereds forceUnchecked deferredObligations externalTypeViews0 surfaceExpr =
  runExceptT $ do
    extEnv0 <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $
        prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews0 surfaceExpr
    normExpr <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
    let extEnv = extendPreparedWithLoweredTypeIdentities lowereds extEnv0
        runPipeline =
          if not forceUnchecked && Map.null deferredObligations
            then runPipelineElabDetailedWithPreparedExternalBindingsWithTiming
            else runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") Set.empty extEnv normExpr
    fromProgramEither $
      either (Left . ProgramPipelineError . renderPipelineError) Right pipelineResult

extendPreparedWithLoweredTypeIdentities :: [LoweredBinding] -> PreparedExternalBindings -> PreparedExternalBindings
extendPreparedWithLoweredTypeIdentities lowereds prepared =
  reservePreparedExternalBindingIdentities generatedIdentities
    (extendPreparedExternalBindingTypeIdentities headIdentities binderIdentities prepared)
  where
    views =
      [ view
      | lowered <- lowereds,
        Just view <- [loweredBindingExpectedTypeView lowered <|> loweredBindingSourceTypeView lowered]
      ]
    headIdentities =
      mergeSymbolIdentityMaps (map typeViewHeadIdentities views)
    binderIdentities =
      mergeTypeBinderIdentityMaps (map typeViewBinderIdentities views)
    generatedIdentities =
      concatMap generatedIdentitiesInLoweredBinding lowereds

runLoweredSurfacePipelineWithModuleContext ::
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  Either ProgramError PipelineElabDetailedResult
runLoweredSurfacePipelineWithModuleContext moduleContext forceUnchecked lowered = do
  readContext <- lookupModuleBindingReadContext moduleContext lowered
  let stampedLowered = moduleBindingReadLowered readContext
  moduleBindingReadResolvedFreeVars readContext
  extEnv0 <- moduleBindingReadExternalBindings readContext
  normExpr <- moduleBindingReadNormalizedExpr readContext
  let extEnv = extendPreparedWithLoweredTypeIdentities [stampedLowered] extEnv0
      runPipeline =
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
    extEnv0 <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $
        moduleBindingReadExternalBindings readContext
    normExpr <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        moduleBindingReadNormalizedExpr readContext
    let extEnv = extendPreparedWithLoweredTypeIdentities [stampedLowered] extEnv0
        runPipeline =
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
idDetailsReadKey =
  Right . X.idDetailsIdentityKey

idDetailsReadKeyMaybe :: IdDetails -> Maybe ModuleBindingReadKey
idDetailsReadKeyMaybe =
  Just . X.idDetailsIdentityKey

externalBindingModeForObligations ::
  ElaborateScope ->
  Map String SrcType ->
  RuntimeExternalBindingIndex ->
  DeferredExternalBindingIndex ->
  Map String SrcType ->
  String ->
  ExternalBindingMode
externalBindingModeForObligations scope runtimeSourceTypes runtimeIndex deferredExternalIndex externalTypes name =
  case lookupDeferredExternalBindingByAlias name deferredExternalIndex of
    Just (DeferredMethod {}) -> ExternalBindingScheme
    Just (DeferredConstructor deferred) -> convertDeferredBindingMode (deferredConstructorBindingMode deferred)
    Just (DeferredCase {}) -> ExternalBindingMonomorphic
    _ ->
      case
        runtimeExternalBindingSourceTypeByAlias scope runtimeSourceTypes runtimeIndex name
          <|> deferredExternalBindingSourceTypeByAlias externalTypes deferredExternalIndex name
      of
        Just ty -> externalBindingModeForSourceType ty
        Nothing -> ExternalBindingScheme

externalBindingModeForResolvedKey :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> ModuleBindingReadKey -> ExternalBindingMode
externalBindingModeForResolvedKey scope runtimeSourceTypes runtimeIndex deferredExternalIndex key =
  case Map.lookup key (deferredExternalBindingByKey deferredExternalIndex) of
    Just (DeferredMethod {}) -> ExternalBindingScheme
    Just (DeferredConstructor deferred) -> convertDeferredBindingMode (deferredConstructorBindingMode deferred)
    Just (DeferredCase {}) -> ExternalBindingMonomorphic
    Nothing ->
      case runtimeExternalBindingSourceTypeByKey scope runtimeSourceTypes runtimeIndex key of
        Just ty -> externalBindingModeForSourceType ty
        Nothing -> ExternalBindingScheme

convertDeferredBindingMode :: DeferredBindingMode -> ExternalBindingMode
convertDeferredBindingMode mode =
  case mode of
    DeferredBindingScheme -> ExternalBindingScheme
    DeferredBindingMonomorphic -> ExternalBindingMonomorphic

externalBindingModeForSourceType :: SrcType -> ExternalBindingMode
externalBindingModeForSourceType ty
  | sourceTypeHasForall ty = ExternalBindingScheme
  | not (Set.null (freeSourceTypeVars ty)) = ExternalBindingScheme
  | otherwise = ExternalBindingMonomorphic

externalBindingModeForRuntime :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> String -> ExternalBindingMode
externalBindingModeForRuntime scope runtimeSourceTypes runtimeIndex name =
  case runtimeExternalBindingSourceTypeByAlias scope runtimeSourceTypes runtimeIndex name of
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

lookupDeferredExternalBindingByAlias :: String -> DeferredExternalBindingIndex -> Maybe DeferredProgramObligation
lookupDeferredExternalBindingByAlias name index =
  Map.lookup name (deferredExternalBindingRefByAlias index)
    >>= lookupDeferredExternalBindingByRef index

lookupDeferredExternalBindingByRef :: DeferredExternalBindingIndex -> DeferredRef -> Maybe DeferredProgramObligation
lookupDeferredExternalBindingByRef index ref =
  Map.lookup ref (deferredExternalBindingByRef index)

lookupUniqueAliasValue :: Eq a => Map String a -> String -> IdDetails -> Maybe a
lookupUniqueAliasValue values runtimeName details =
  lookupUniqueValue (idDetailsAliasNamesWith runtimeName details) values

lookupUniqueValue :: Eq a => [String] -> Map String a -> Maybe a
lookupUniqueValue aliases values =
  case valuesForAliases of
    [] -> Nothing
    value : rest
      | all (== value) rest -> Just value
      | otherwise -> Nothing
  where
    valuesForAliases =
      [ value
      | alias <- aliases,
        Just value <- [Map.lookup alias values]
      ]

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
  prepareSurfaceExternalBindingInputs
    scope
    [ SurfaceExternalBindingInput name view (modeFor name) identity
    | (name, view) <- Map.toList sourceTypeViews,
      Just identity <- [identityFor name]
    ]

prepareSurfaceExternalBindingInputs :: ElaborateScope -> [SurfaceExternalBindingInput] -> Either ProgramError PreparedExternalBindings
prepareSurfaceExternalBindingInputs scope inputs = do
  extBindings <- fmap Map.fromList (mapM prepareInput inputs)
  either (Left . ProgramPipelineError . show) (Right . addScopeIdentities) (prepareExternalBindings extBindings)
  where
    prepareInput input = do
      let name = surfaceExternalBindingInputName input
          view = surfaceExternalBindingInputView input
      normTy <- either (Left . ProgramPipelineError . show) Right (normalizeType (typeViewDisplay view))
      pure
        ( name,
          ExternalBinding
            { externalBindingType = normTy,
              externalBindingMode = surfaceExternalBindingInputMode input,
              externalBindingIdentity = Just (surfaceExternalBindingInputIdentity input),
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

    addScopeIdentities =
      extendPreparedExternalBindingTypeIdentities
        (typeHeadIdentitiesInScope scope)
        Map.empty

lowerExternalTypeViews :: ElaborateScope -> Map String TypeView -> Map String TypeView
lowerExternalTypeViews scope =
  Map.map (lowerTypeViewWithIdentities scope)

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
    resolvedTerm = term
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
finalizeDeferredObligationsForGroup context _lowereds deferredObligations tcEnv term inferredTy expectedBindingTy =
  finalizeDeferredObligations context resolvedDeferredObligations tcEnv resolvedTerm inferredTy expectedBindingTy
  where
    resolvedTerm = term
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
  structurallyTyped <-
    lowerResolvedTermTypesForCheckedIR scope methodsRewritten
  rewritten <-
    if termHasLets structurallyTyped
      then refreshLetSchemes caseRewriteEnv structurallyTyped
      else Right structurallyTyped
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

lowerResolvedTermTypesForCheckedIR :: ElaborateScope -> XmlfTerm -> Either ProgramError XmlfTerm
lowerResolvedTermTypesForCheckedIR scope = go
  where
    go term =
      case term of
        X.EVarNode resolved ->
          X.EVarNode <$> lowerResolved resolved
        X.ELit {} -> Right term
        X.ELam resolved body ->
          X.ELam <$> lowerResolved resolved <*> go body
        X.EApp fun arg ->
          X.EApp <$> go fun <*> go arg
        X.ELet resolved scheme rhs body ->
          X.ELet
            <$> lowerResolved resolved
            <*> lowerScheme scheme
            <*> go rhs
            <*> go body
        X.ETyAbsRef ref mbBound body ->
          X.ETyAbsRef ref <$> traverse lowerBound mbBound <*> go body
        X.ETyInst inner inst ->
          X.ETyInst <$> go inner <*> lowerInstantiation inst
        X.ERoll ty body ->
          X.ERoll <$> lowerCheckedType ty <*> go body
        X.EUnroll inner ->
          X.EUnroll <$> go inner

    lowerResolved resolved = do
      ty <- lowerCheckedType (X.resolvedVarType resolved)
      Right (X.mapResolvedVarType (const ty) resolved)

    lowerScheme scheme =
      schemeFromType <$> lowerCheckedType (schemeToType scheme)

    lowerBound bound = do
      lowered <- lowerCheckedType (X.tyToElab bound)
      case X.elabToBound lowered of
        Right loweredBound -> Right loweredBound
        Left err ->
          Left
            ( ProgramPipelineError
                ("checked-IR structural bound conversion failed: " ++ show err)
            )

    lowerInstantiation inst =
      case inst of
        X.InstId -> Right X.InstId
        X.InstApp ty -> X.InstApp <$> lowerCheckedType ty
        X.InstBot ty -> X.InstBot <$> lowerCheckedType ty
        X.InstIntro -> Right X.InstIntro
        X.InstElim -> Right X.InstElim
        X.InstAbstrRef ref -> Right (X.InstAbstrRef ref)
        X.InstUnderRef ref inner -> X.InstUnderRef ref <$> lowerInstantiation inner
        X.InstInside inner -> X.InstInside <$> lowerInstantiation inner
        X.InstSeq left right -> X.InstSeq <$> lowerInstantiation left <*> lowerInstantiation right

    lowerCheckedType =
      typeViewToElabType scope . elabTypeToRecoveredTypeView scope

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
  TypeCheck.mkTypeCheckEnvWithResolvedTerms resolvedEntries Map.empty
  where
    runtimeIndex = finalizeContextRuntimeBindingIndex context

    resolvedEntries =
      [ (resolved, X.resolvedVarType resolved)
      | resolved <- Map.elems (runtimeExternalBindingByKey runtimeIndex)
      ]

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
                      (_, binderIdentity) = binder
                  case lookupTypeBinderSubstViewByIdentity binderIdentity subst of
                    -- The initial substitution was selected from the
                    -- identity-bearing source occurrence.  A later xMLF head
                    -- instantiation is a fresh graph representation of that
                    -- same choice, so it must not replace the source identity.
                    Just sourceView ->
                      Right (subst, rest, sourceView : views)
                    Nothing -> do
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
            mergeSymbolIdentityMaps
              ( typeViewHeadIdentities (deferredConstructorSourceTypeView deferred)
                  : typeViewHeadIdentities (deferredConstructorOccurrenceTypeView deferred)
                  : map typeViewHeadIdentities argViews
                  ++ map typeViewHeadIdentities headInstViews
              )
      occurrenceView <-
        let occurrenceFallbackView =
              applyConstructorViewSubst substFromArgs (deferredConstructorOccurrenceTypeView deferred)
         in do
              occurrenceEnv <- ensureDeferredConstructorPlaceholderEnv env0 placeholderName deferred substFromArgs
              inferOccurrenceTypeView occurrenceEnv occurrenceFallbackView occurrenceTerm
      let substFinal =
            case matchTypeBinderSubstTypeViewInScope scope instBinders substFromArgs (deferredConstructorOccurrenceTypeView deferred) occurrenceView of
              Just subst -> subst
              Nothing -> substFromArgs
          constructorHeadIdentities =
            mergeSymbolIdentityMaps [argHeadIdentities, typeViewHeadIdentities occurrenceView]
          missingInstBinders =
            filter
              (\(_, identity) -> maybe True (const False) (lookupTypeBinderSubstViewByIdentity identity substFinal))
              instBinders
      case missingInstBinders of
        [] -> do
          ctorHead <-
            if constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo
              then
                foldM
                  ( \headAcc (_, identity) ->
                      case lookupTypeBinderSubstViewByIdentity identity substFinal of
                        Just view -> do
                          instTy <- typeViewToElabType scope view
                          Right (X.ETyInst headAcc (X.InstApp instTy))
                        Nothing -> Right headAcc
                  )
                  (X.EVarNode (resolvedVarFromConstructorInfo ctorInfo))
                  instBinders
              else inlineConstructorHead ConstructorOccurrenceTerm scope constructorHeadIdentities [] ctorInfo substFinal
          Right (foldl X.EApp ctorHead args)
        _ ->
          Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo))

    inferArgTypeView env0 arg =
      case typeCheckWithEnv env0 arg of
        Right ty -> Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls ty))
        Left (X.TCArgumentMismatch _ actualTy) ->
          Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls actualTy))
        Left err -> Left (ProgramPipelineError ("deferred constructor argument type check failed: " ++ show err))

    inferOccurrenceTypeView env0 fallbackView occurrenceTerm =
      case typeCheckWithEnv env0 occurrenceTerm of
        Right ty -> Right (elabTypeToRecoveredTypeView scope (stripVacuousForalls ty))
        Left err
          | isDeferredConstructorArgumentMismatch err ->
              Right fallbackView
        Left err -> Left (ProgramPipelineError ("deferred constructor occurrence type check failed: " ++ show err))

    isDeferredConstructorArgumentMismatch err =
      case err of
        X.TCArgumentMismatch {} -> True
        _ -> False

    ensureDeferredConstructorPlaceholderEnv env0 _placeholder deferred subst = do
      placeholderTy <- typeViewToElabType scope placeholderSourceView
      let resolved = X.deferredResolvedVarFromRef (deferredConstructorRef deferred)
      Right (TypeCheck.insertResolvedTermBinding resolved placeholderTy env0)
      where
        placeholderSourceView =
          applyConstructorViewSubst subst (deferredConstructorSourceTypeView deferred)

    applyConstructorViewSubst subst =
      applyTypeViewSubst (typeBinderSubstToTypeViewSubst subst)

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
  matchTypeBinderSubstTypeViewInScope
    scope
    binders
    subst
    (typeBinderTemplateView scope binders templateTy)
    actualView

matchTypeBinderSubstTypeViewInScope ::
  ElaborateScope ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  TypeView ->
  TypeView ->
  Maybe TypeBinderSubst
matchTypeBinderSubstTypeViewInScope scope binders subst templateView actualView =
  typeBinderSubstFromTypeViewSubst binders
    <$> matchTypeViewsAgainstIdentity
      scope
      (typeBinderSubstToTypeViewSubst subst)
      (NE.singleton (templateViewWithBinders binders templateView))
      (NE.singleton actualView)

templateViewWithBinders :: [(String, TypeBinderIdentity)] -> TypeView -> TypeView
templateViewWithBinders binders view =
  typeViewMergeBinderIdentities (typeBinderAliasIdentityMap binders) view

typeBinderTemplateView :: ElaborateScope -> [(String, TypeBinderIdentity)] -> SrcType -> TypeView
typeBinderTemplateView scope binders ty =
  templateViewWithBinders binders view
  where
    view = sourceTypeViewInScope scope ty

bindTypeBinderSubstViewInScope ::
  ElaborateScope ->
  (String, TypeBinderIdentity) ->
  TypeView ->
  TypeBinderSubst ->
  Maybe TypeBinderSubst
bindTypeBinderSubstViewInScope scope (name, identity) actualView subst =
  case lookupTypeBinderSubstViewByIdentity identity subst of
    Nothing ->
      Just (insertTypeBinderSubstViewWithIdentity identity name actualView subst)
    Just existingView
      | typeViewIsBareBinderIdentity identity existingView ->
          Just (insertTypeBinderSubstViewWithIdentity identity name actualView subst)
      | semanticTypeViewsMatch existingView actualView ->
          Just subst
      | otherwise -> Nothing
  where
    semanticTypeViewsMatch left right =
      case (typeViewToElabType scope left, typeViewToElabType scope right) of
        (Right leftTy, Right rightTy) ->
          alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy
        _ -> False

data ConstructorTermPurpose
  = ConstructorBindingTerm
  | ConstructorOccurrenceTerm

inlineConstructorHead :: ConstructorTermPurpose -> ElaborateScope -> Map String SymbolIdentity -> [(String, TypeBinderIdentity)] -> ConstructorInfo -> TypeBinderSubst -> Either ProgramError XmlfTerm
inlineConstructorHead purpose scope extraHeadIdentities ownerParamBinders ctorInfo subst = do
  let resultSrcTy = applyConstructorSubst subst (ctorResult ctorInfo)
      resultVar = "$" ++ symbolIdentityStableName (ctorOwningTypeIdentity ctorInfo) ++ "_result"
      useStructuralTypes =
        case purpose of
          ConstructorBindingTerm -> constructorBindingUsesStructuralPlaceholder scope ctorInfo
          ConstructorOccurrenceTerm -> False
      argSrcTys
        | useStructuralTypes = constructorStructuralArgs ctorInfo
        | otherwise = map (applyConstructorSubst subst) (ctorArgs ctorInfo)
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
      handlerSrcType shape
        | useStructuralTypes = constructorStructuralHandlerType resultVar shape
        | otherwise =
            foldr
              (\(name, mbBound) acc -> STForall name (fmap SrcBound mbBound) acc)
              (foldr STArrow (STVar resultVar) (constructorShapeArgs shape))
              (constructorShapeForalls shape)
      loweredResultSrcTy = lowerTypeView scope (sourceTypeViewWithHeadIdentities resultSrcTy)
      loweredArgSrcTys = map (lowerTypeView scope . sourceTypeViewWithHeadIdentities) argSrcTys
      loweredHandlerSrcTys = map (lowerTypeView scope . sourceTypeViewWithHeadIdentities . handlerSrcType) handlerShapes
      structuralBinderIdentities =
        mergeTypeBinderIdentityMaps
          (map (sourceTypeBinderIdentitiesInScope scope) (loweredResultSrcTy : loweredArgSrcTys ++ loweredHandlerSrcTys))
      sharedFreeNames =
        Set.toList $
          Set.delete resultVar $
            Set.unions (map freeSrcTypeVars (loweredResultSrcTy : loweredArgSrcTys ++ loweredHandlerSrcTys))
      headIdentities =
        mergeSymbolIdentityMaps [extraHeadIdentities, typeHeadIdentitiesInScope scope]
      ownerParamRefsByAlias =
        Map.mapWithKey ownerParamRefForAlias (typeBinderIdentityAliasMap ownerParamBinders)
      ownerParamRefForAlias alias identity =
        X.typeBinderRefFromIdentity identity (Map.findWithDefault alias identity ownerParamNamesByIdentity)
      ownerParamNamesByIdentity =
        Map.fromList [(identity, name) | (name, identity) <- ownerParamBinders]
      structuralRefsByAlias =
        typeBinderIdentityRefs structuralBinderIdentities
      ownerParamRefs =
        [ X.typeBinderRefFromIdentity identity name
        | (name, identity) <- ownerParamBinders
        ]
      knownRefs =
        ownerParamRefsByAlias `Map.union` structuralRefsByAlias
      missingSharedFreeNames =
        filter (`Map.notMember` knownRefs) sharedFreeNames
      (freshSharedRefs, generator0) =
        freshTypeBinderRefsAfterHeadOwnerAndStructuralIdentities headIdentities ownerParamBinders structuralBinderIdentities missingSharedFreeNames
      sharedRefs =
        knownRefs `Map.union` freshSharedRefs
      sharedTypeAbsRefs =
        [ ref
        | name <- sharedFreeNames,
          Just ref <- [Map.lookup name sharedRefs]
        ]
      topTypeAbsRefs =
        ownerParamRefs ++ filter (not . refIdentityIn ownerParamRefs) sharedTypeAbsRefs
      mbResultRef = Map.lookup resultVar sharedRefs
      handlerRefs = maybe sharedRefs (\resultRef -> Map.insert resultVar resultRef sharedRefs) mbResultRef
  resultRef <-
    case mbResultRef of
      Just ref -> Right ref
      Nothing -> Left (ProgramPipelineError ("constructor handler result binder `" ++ resultVar ++ "` is missing identity"))
  (resultTy, generator2) <- srcTypeToElabTypeWithHeadIdentities headIdentities sharedRefs generator0 loweredResultSrcTy
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

    freshTypeBinderRefsAfterHeadOwnerAndStructuralIdentities headIdentities ownerBinders structuralBinders names =
      freshTypeBinderRefs names generator0
      where
        generator0 =
          identityGeneratorAfter
            ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
                ++ concatMap (typeBinderGeneratedIdentities . snd) ownerBinders
                ++ concatMap typeBinderGeneratedIdentities (Map.elems structuralBinders)
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
      sourceTypeViewInScopeWithHeadIdentities scope extraHeadIdentities ty

    constructorShapeResultMatchView shape =
      typeViewWithIdentityMaps
        ( mergeSymbolIdentityMaps
            [ typeViewHeadIdentities (constructorShapeTypeView shape),
              typeViewHeadIdentities (sourceTypeViewInScope scope (constructorShapeResultIdentity shape))
            ]
        )
        (typeBinderAliasIdentityMap (constructorShapeForallBinders shape))
        (constructorShapeResultView shape)

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
   in shape
        { constructorShapeTypeView =
            specializeQuantifiedTypeView
              (typeBinderSubstToTypeViewSubst subst)
              (constructorShapeTypeView shape),
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
              scrutineeTy <- inferDeferredArgType env scrutinee
              validateCaseScrutineeType
                (deferredCaseDataInfo deferred)
                (deferredCaseScrutineeTypeView deferred)
                scrutineeTy
              (env', resultTy) <-
                extendCaseResultEnv
                  (deferredCaseDataInfo deferred)
                  scrutineeTy
                  (deferredCaseResultTypeView deferred)
                  env
              let caseHead = caseEliminator resultTy scrutinee
              Right (env', foldl X.EApp caseHead handlers)
        _ -> Left (ProgramCaseOnNonDataType STBottom)

    validateCaseScrutineeType dataInfo expectedView actualTy =
      case caseDataInfoForElabType actualTy of
        Just actualInfo
          | sameSymbolIdentity (dataInfoSymbol actualInfo) (dataInfoSymbol dataInfo) ->
              case typeViewToElabType scope expectedView of
                Right expectedTy
                  | alphaEqType actualTy expectedTy || churchAwareEqType actualTy expectedTy ->
                      Right ()
                _ ->
                  case caseActualSourceTypeView dataInfo actualTy of
                    Just actualView ->
                      case
                        matchTypeViewsAgainstIdentity
                          scope
                          Map.empty
                          (NE.singleton expectedView)
                          (NE.singleton actualView)
                      of
                        Just _ -> Right ()
                        Nothing -> caseTypeMismatch actualTy
                    _ -> caseTypeMismatch actualTy
        _ -> caseTypeMismatch actualTy

    caseDataInfoForElabType :: X.Ty v -> Maybe DataInfo
    caseDataInfoForElabType ty =
      case ty of
        X.TBaseWithIdentity (Just identity) _ ->
          lookupSymbolIdentityExact identity (elaborateScopeDataTypesByIdentity scope)
        X.TConWithIdentity (Just identity) _ _ ->
          lookupSymbolIdentityExact identity (elaborateScopeDataTypesByIdentity scope)
        X.TMuRef ref _ -> do
          (unique, StructuralSelfBinder) <- typeBinderIdentityStructural (X.typeBinderRefIdentity ref)
          find
            ((== unique) . symbolUniqueIdentity . dataInfoSymbol)
            (elaborateScopeUniqueDataTypes scope)
        X.TForallRef _ _ body -> caseDataInfoForElabType body
        _ -> Nothing

    caseActualSourceTypeView dataInfo actualTy =
      case actualTy of
        X.TBaseWithIdentity (Just _) _ ->
          Just (rawElabTypeView actualTy)
        X.TConWithIdentity (Just _) _ _ ->
          Just (rawElabTypeView actualTy)
        X.TForallRef _ _ body ->
          caseActualSourceTypeView dataInfo body
        X.TMuRef {} -> do
          (sourceHeadTy, _) <-
            matchDataInfoEncodingForElabType scope dataInfo actualTy
          let rawView = rawElabTypeView actualTy
              sourceView =
                sourceTypeViewInScopeWithHeadIdentities
                  scope
                  ( mergeSymbolIdentityMaps
                      [ dataInfoHeadIdentityLookupAliases dataInfo,
                        typeViewHeadIdentities rawView
                      ]
                  )
                  sourceHeadTy
          Just
            (typeViewMergeBinderIdentities (typeViewBinderIdentities rawView) sourceView)
        _ -> Nothing

    rawElabTypeView ty =
      typeViewMergeBinderIdentities (elabTypeBinderIdentities ty) sourceView
      where
        sourceView =
          sourceTypeViewInScopeWithHeadIdentities
            scope
            (elabTypeHeadIdentities ty)
            (elabTypeToSrcType ty)

    caseTypeMismatch ty =
      Left (ProgramCaseOnNonDataType (elabTypeToSrcType ty))

    caseEliminator resultTy scrutinee =
      X.ETyInst (X.EUnroll scrutinee) (X.InstApp resultTy)

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty -> Right (stripVacuousForalls ty)
        Left (X.TCArgumentMismatch _ actualTy) ->
          Right (stripVacuousForalls actualTy)
        Left err ->
          Left (ProgramPipelineError ("deferred case scrutinee type check failed: " ++ show err))

    extendCaseResultEnv dataInfo scrutineeTy resultView env =
      case matchDataInfoEncodingForElabType scope dataInfo scrutineeTy of
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
              resultSrcTy = lowerTypeView scope resultView
              bindingNames = resultBindingNames `Set.union` selfAliasBindingNames
              sharedNames =
                Set.toList $
                  bindingNames
                    `Set.union` freeSrcTypeVars loweredHeadTy
                    `Set.union` freeSrcTypeVars resultSrcTy
                    `Set.union` freeSrcTypeVars scrutineeRawTy
                    `Set.union` foldMap freeSrcTypeVars (Map.elems subst)
              headIdentities =
                mergeSymbolIdentityMaps
                  [ dataInfoHeadIdentityLookupAliases dataInfo,
                    typeViewHeadIdentityLookupAliases resultView,
                    elabTypeHeadIdentities scrutineeTy,
                    typeHeadIdentitiesInScope scope
                  ]
              binderIdentities =
                mergeTypeBinderIdentityMaps
                  [ typeViewBinderIdentities resultView,
                    elabTypeBinderIdentities scrutineeTy
                  ]
              knownRefs = typeBinderIdentityRefs binderIdentities
              missingSharedNames = filter (`Map.notMember` knownRefs) sharedNames
              generatorSeed =
                identityGeneratorAfter
                  ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
                      ++ concatMap typeBinderGeneratedIdentities (Map.elems binderIdentities)
                  )
              (freshRefs, generator0) =
                freshTypeBinderRefs missingSharedNames generatorSeed
              sharedRefs = knownRefs `Map.union` freshRefs
          (headTy, generator1) <-
            srcTypeToElabTypeWithScopedHeadIdentities scope headIdentities sharedRefs generator0 loweredHeadTy
          (resultTy, _) <-
            srcTypeToElabTypeWithScopedHeadIdentities scope headIdentities sharedRefs generator1 resultSrcTy
          let selfAliasBindings =
                Map.fromSet (const headTy) selfAliasBindingNames
              resultBinding =
                Map.fromSet (const resultTy) resultBindingNames
              bindings = selfAliasBindings `Map.union` resultBinding
          env' <- foldM (insertCaseTypeBinding sharedRefs) env (Map.toList bindings)
          Right (env', resultTy)
        Nothing -> do
          resultTy <- typeViewToElabType scope resultView
          Right (env, resultTy)
      where
        -- Structural matching is a lowering boundary only.  The owner was
        -- already selected from the carried nominal or structural identity.
        scrutineeRawTy = elabTypeToSrcType scrutineeTy

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
                  []
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
              evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) [] eagerConstraints'
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
              []
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
          evidenceArgs <- resolveConstraintEvidenceTerms scope (deferredMethodLocalEvidence deferred) [] eagerConstraints'
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
          (zip (methodParamTypeViews methodView) argViews)
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

    inferDeferredMethodClassArgumentFromExpected _ _ Nothing = Nothing
    inferDeferredMethodClassArgumentFromExpected methodInfo argViews (Just expectedView) = do
      let methodView = methodTypeView methodInfo
      substFromArgs <-
        foldM
          (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (templateView :| []) (actualView :| []))
          Map.empty
          (zip (methodParamTypeViews methodView) argViews)
      subst <- matchMethodTypeViews scope substFromArgs (methodResultTypeView methodInfo :| []) (expectedView :| [])
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

    lookupMethodEvidence deferred methodInfo classArgView =
      case uniqueEvidenceMethodMatch localMatches of
        Just (methodEvidence, subst) ->
          Just (mkEvidence methodEvidence, subst)
        Nothing ->
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
          lookupEvidenceMethodByClassViews
            scope
            (methodInfoOwnerClassSymbolIdentity methodInfo)
            targetViews
            (methodInfoSymbolIdentity methodInfo)
        localMatches =
          [ (methodEvidence, subst)
          | evidence <- deferredMethodLocalEvidence deferred,
            sameSymbolIdentity (evidenceClassSymbol evidence) (methodInfoOwnerClassSymbolIdentity methodInfo),
            Just subst <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) targetViews],
            methodEvidence <- maybe [] (: []) (lookupSymbolIdentityExact (methodInfoSymbolIdentity methodInfo) (evidenceMethodsByIdentity evidence)),
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
       in matchMethodTypeViews scope subst (methodResultTypeViewFrom specializedMethodView :| []) (expectedView :| [])

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
            (zip (methodParamTypeViews specializedMethodView) argViews)

resolveConstraintEvidenceTerms :: ElaborateScope -> [EvidenceInfo] -> [ClassApplicationKey] -> [ConstraintInfo] -> Either ProgramError [XmlfTerm]
resolveConstraintEvidenceTerms scope localEvidence seen constraints =
  concat <$> mapM (resolveConstraintEvidenceTerm scope localEvidence seen) constraints

resolveConstraintEvidenceTerm :: ElaborateScope -> [EvidenceInfo] -> [ClassApplicationKey] -> ConstraintInfo -> Either ProgramError [XmlfTerm]
resolveConstraintEvidenceTerm scope localEvidence seen constraint = do
  let key = constraintEvidenceKey constraint
  if key `elem` seen
    then Left (noMatchingInstanceError scope constraint)
    else do
      mbLocalEvidence <- resolveLocalConstraintEvidenceTerms scope localEvidence constraint
      case mbLocalEvidence of
        Just evidenceTerms -> Right evidenceTerms
        Nothing -> do
          (instanceInfo, subst) <- resolveInstanceInfoByConstraint scope constraint
          let seen' = key : seen
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
                          case lookupEvidenceMethodByClassViews scope (constraintClassSymbol constraint) (constraintTypeViews constraint) (methodInfoSymbolIdentity methodInfo) of
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
        sameSymbolIdentity (evidenceClassSymbol evidence) classIdentity,
        Just subst <- [matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) headViews],
        methodEvidence <- maybe [] (: []) (lookupSymbolIdentityExact methodIdentity (evidenceMethodsByIdentity evidence))
    ]

preferredEvidenceMethodMatch :: [(EvidenceMethod, TypeViewSubst)] -> Maybe (EvidenceMethod, TypeViewSubst)
preferredEvidenceMethodMatch matches =
  case uniqueEvidenceMethodMatch resolvedMatches of
    Just match -> Just match
    Nothing
      | not (null resolvedMatches) -> Nothing
      | otherwise -> uniqueEvidenceMethodMatch matches
  where
    resolvedMatches =
      [ match
      | match@(method, _) <- matches,
        Just _ <- [evidenceMethodResolvedVar method]
      ]

zeroMethodConstraintCoveredByEvidence :: ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidence scope evidenceInfos constraint =
  any
    ( \evidence ->
        sameSymbolIdentity (evidenceClassSymbol evidence) (constraintClassSymbol constraint)
          && case matchMethodTypeViews scope Map.empty (evidenceTypeViews evidence) (constraintTypeViews constraint) of
            Just _ -> True
            Nothing -> False
    )
    evidenceInfos

constraintEvidenceKey :: ConstraintInfo -> ClassApplicationKey
constraintEvidenceKey =
  constraintClassApplicationKey

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
      (X.typeBinderRefIdentity ref)
        : [ identity
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
    { X.resolvedVarType = X.TBottom,
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
      sameSymbolIdentity (dataInfoSymbol left) (dataInfoSymbol right)

{- Note [recoverElabSourceType]

When the eMLF pipeline infers a type, it returns raw Church-encoded μ forms
with fresh binder names.  The .mlfp layer still needs named source ADT heads
for diagnostics and instance-head comparisons.  This recovery is deliberately
downstream of lowering: `Program.Elaborate` never invokes the pipeline.
-}
data StructuralOwnerRecovery
  = RecoverStructuralOwnerFromIdentities
      (Map String TypeBinderIdentity)
      (Map String SymbolIdentity)
  | RecoverStructuralOwnerMetadataLight

-- Matching happens after the nominal owner has been selected. Checked
-- substitutions are still keyed by the carried binder identity; the display
-- name is retained only for the final SrcType adapter used by diagnostics and
-- the deferred-case environment. Metadata-light fixtures opt into name keys.
data RecoverBinderKey
  = RecoverBinderIdentity TypeBinderIdentity String
  | RecoverBinderMetadataLight String

instance Eq RecoverBinderKey where
  RecoverBinderIdentity left _ == RecoverBinderIdentity right _ = left == right
  RecoverBinderMetadataLight left == RecoverBinderMetadataLight right = left == right
  _ == _ = False

instance Ord RecoverBinderKey where
  compare left right =
    case (left, right) of
      (RecoverBinderIdentity leftIdentity _, RecoverBinderIdentity rightIdentity _) ->
        compare leftIdentity rightIdentity
      (RecoverBinderMetadataLight leftName, RecoverBinderMetadataLight rightName) ->
        compare leftName rightName
      (RecoverBinderIdentity {}, RecoverBinderMetadataLight {}) -> LT
      (RecoverBinderMetadataLight {}, RecoverBinderIdentity {}) -> GT

recoverBinderDisplayName :: RecoverBinderKey -> String
recoverBinderDisplayName key =
  case key of
    RecoverBinderIdentity _ name -> name
    RecoverBinderMetadataLight name -> name

data RecoverHeadIdentityContext = RecoverHeadIdentityContext
  { recoverExpectedHeadIdentities :: Map String SymbolIdentity,
    recoverActualHeadIdentities :: Map String SymbolIdentity
  }

recoverCombinedHeadIdentities :: RecoverHeadIdentityContext -> Map String SymbolIdentity
recoverCombinedHeadIdentities context =
  mergeSymbolIdentityMaps
    [ recoverExpectedHeadIdentities context,
      recoverActualHeadIdentities context
    ]

-- | Explicit adapter for metadata-light fixtures that have no binder identity
-- sidecar. Checked production recovery must use 'recoverElabSourceType'.
recoverSourceTypeMetadataLight :: ElaborateScope -> SrcType -> SrcType
recoverSourceTypeMetadataLight =
  recoverSourceTypeWith RecoverStructuralOwnerMetadataLight

recoverElabSourceType :: ElaborateScope -> X.Ty v -> SrcType
recoverElabSourceType scope ty =
  recoverSourceTypeWith
    ( RecoverStructuralOwnerFromIdentities
        (elabTypeBinderIdentities ty)
        (elabTypeHeadIdentities ty)
    )
    scope
    (elabTypeToSrcType ty)

recoverSourceTypeWith :: StructuralOwnerRecovery -> ElaborateScope -> SrcType -> SrcType
recoverSourceTypeWith ownerRecovery scope = recover
  where
    dataInfos = elaborateScopeUniqueDataTypes scope

    recover ty =
      case lookupHead ty of
        Just headTy -> headTy
        Nothing -> recoverChildren ty

    lookupHead ty =
      case mapMaybeDataHead ty (candidateDataInfos ty) of
        [headTy] -> Just headTy
        _ -> Nothing

    candidateDataInfos ty =
      case ownerRecovery of
        RecoverStructuralOwnerFromIdentities binderIdentities _ ->
          case ty of
            STMu selfName _ ->
              maybe [] (: []) (structuralOwnerDataInfo binderIdentities selfName)
            _ -> []
        RecoverStructuralOwnerMetadataLight ->
          case ty of
            STMu selfName _ -> filter (metadataLightStructuralOwnerMatches selfName) dataInfos
            _ -> dataInfos

    metadataLightStructuralOwnerMatches selfName info =
      selfName
        `Set.member` Set.fromList
          [ "$" ++ name ++ "_self"
          | name <- dataInfoHeadNames scope info
          ]

    structuralOwnerDataInfo binderIdentities selfName = do
      binderIdentity <- Map.lookup selfName binderIdentities
      (ownerUnique, StructuralSelfBinder) <- typeBinderIdentityStructural binderIdentity
      find
        ((== ownerUnique) . symbolUniqueIdentity . dataInfoSymbol)
        dataInfos

    mapMaybeDataHead ty =
      foldr
        ( \info acc ->
            case recoverDataHead ty info of
              Just headTy -> headTy : acc
              Nothing -> acc
        )
        []

    recoverDataHead ty info =
      fst <$> matchDataInfoEncodingWithRecovery recover ownerRecovery scope info ty

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

matchDataInfoEncodingForElabType :: ElaborateScope -> DataInfo -> X.Ty v -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncodingForElabType scope info ty = do
  (ownerUnique, StructuralSelfBinder) <-
    case ty of
      X.TMuRef ref _ -> typeBinderIdentityStructural (X.typeBinderRefIdentity ref)
      _ -> Nothing
  if ownerUnique == symbolUniqueIdentity (dataInfoSymbol info)
    then
      matchDataInfoEncodingWithRecovery
        id
        ( RecoverStructuralOwnerFromIdentities
            (elabTypeBinderIdentities ty)
            (elabTypeHeadIdentities ty)
        )
        scope
        info
        (elabTypeToSrcType ty)
    else Nothing

matchDataInfoEncodingWithRecovery :: (SrcType -> SrcType) -> StructuralOwnerRecovery -> ElaborateScope -> DataInfo -> SrcType -> Maybe (SrcType, Map String SrcType)
matchDataInfoEncodingWithRecovery recover ownerRecovery scope info ty =
  firstMatch (dataInfoHeadNames scope info)
  where
    params = dataParams info
    baseHeadIdentities =
      RecoverHeadIdentityContext
        { recoverExpectedHeadIdentities = dataInfoHeadIdentityLookupAliases info,
          recoverActualHeadIdentities =
            case ownerRecovery of
              RecoverStructuralOwnerFromIdentities _ actualHeadIdentities -> actualHeadIdentities
              _ -> Map.empty
        }

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
          headIdentities =
            baseHeadIdentities
              { recoverExpectedHeadIdentities =
                  mergeSymbolIdentityMaps
                    [ recoverExpectedHeadIdentities baseHeadIdentities,
                      typeViewHeadIdentities (sourceTypeViewInScope scope loweredTemplate)
                    ]
              }
          templateBinderIdentities =
            mergeTypeBinderIdentityMaps
              [ typeBinderAliasIdentityMap (dataParamBinders info),
                typeBinderAliasIdentityMap constructorForallBinders,
                sourceTypeBinderIdentitiesInScope scope loweredTemplate
              ]
          constructorForallBinders =
            [ (constructorForallDisplayName binder, constructorForallIdentity binder)
            | ctor <- dataConstructors info,
              binder <- ctorForallBinderInfo ctor
            ]
          recoverBinderKey name =
            case ownerRecovery of
              RecoverStructuralOwnerFromIdentities {} ->
                RecoverBinderIdentity <$> Map.lookup name templateBinderIdentities <*> pure name
              RecoverStructuralOwnerMetadataLight ->
                Just (RecoverBinderMetadataLight name)
          recoverParams =
            Map.fromList
              [ (param, key)
              | param <- params,
                Just key <- [recoverBinderKey param]
              ]
          matchTemplate template =
            matchRecoverType ownerRecovery scope headIdentities recoverBinderKey recoverParams Map.empty Map.empty template ty
          matched =
            case matchTemplate loweredTemplate of
              Just subst -> Just subst
              Nothing ->
                case loweredTemplate of
                  STMu _ body -> matchTemplate body
                  _ -> Nothing
        in case matched of
            Just subst ->
              let recoveredArg param =
                    recover $
                      case recoverBinderKey param >>= (`Map.lookup` subst) of
                        Just arg -> arg
                        Nothing -> STVar param
                  recoveredArgs = map recoveredArg params
                  recoveredHead =
                    case recoveredArgs of
                      [] -> STBase headName
                      arg : args -> STCon headName (arg :| args)
                  namedSubst =
                    Map.fromList
                      [ (recoverBinderDisplayName key, matchedTy)
                      | (key, matchedTy) <- Map.toList subst
                      ]
               in Just (recoveredHead, namedSubst)
            Nothing ->
              case ownerRecovery of
                RecoverStructuralOwnerMetadataLight -> recoverSelfMu headName ty
                _ -> Nothing

    recoverSelfMu headName actualTy =
      case actualTy of
        STMu selfName body
          | structuralOwnerMatches selfName,
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
        structuralOwnerMatches selfName =
          case ownerRecovery of
            RecoverStructuralOwnerFromIdentities binderIdentities _ ->
              case Map.lookup selfName binderIdentities >>= typeBinderIdentityStructural of
                Just (ownerUnique, StructuralSelfBinder) ->
                  ownerUnique == symbolUniqueIdentity (dataInfoSymbol info)
                _ -> False
            RecoverStructuralOwnerMetadataLight ->
              selfName `Set.member` selfNames

        selfNames =
          Set.fromList
            [ "$" ++ name ++ "_self"
            | name <- dataInfoHeadNames scope info
            ]

matchRecoverType ::
  StructuralOwnerRecovery ->
  ElaborateScope ->
  RecoverHeadIdentityContext ->
  (String -> Maybe RecoverBinderKey) ->
  Map String RecoverBinderKey ->
  Map RecoverBinderKey SrcType ->
  Map String String ->
  SrcType ->
  SrcType ->
  Maybe (Map RecoverBinderKey SrcType)
matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst renames template actual =
  case template of
    STVar name
      | Just key <- Map.lookup name params ->
          bindRecoverParam ownerRecovery scope headIdentities key actual subst
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
          subst' <- matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst renames dom dom'
          matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst' renames cod cod'
        _ -> Nothing
    STBase name ->
      case actual of
        STBase name' | recoverTypeHeadMatches ownerRecovery scope headIdentities name name' -> Just subst
        _ -> Nothing
    STCon name args ->
      case actual of
        STCon name' args'
          | recoverTypeHeadMatches ownerRecovery scope headIdentities name name' && length (toListNE args) == length (toListNE args') ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params acc renames leftTy rightTy)
                subst
                (zip (toListNE args) (toListNE args'))
        _ -> Nothing
    STVarApp name args ->
      matchRecoverVarApp ownerRecovery scope headIdentities recoverBinderKey params subst renames name args actual
    STTyLam name body ->
      case actual of
        STTyLam name' body' ->
          matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STTyApp fun arg ->
      case actual of
        STTyApp fun' arg' -> do
          subst' <- matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst renames fun fun'
          matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst' renames arg arg'
        _ -> Nothing
    STForall name _mb body ->
      case actual of
        STForall name' _mb' body' ->
          matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst (Map.insert name name' renames) body body'
        _ -> do
          key <- recoverBinderKey name
          matchRecoverType ownerRecovery scope headIdentities recoverBinderKey (Map.insert name key params) subst renames body actual
    STMu name body ->
      case actual of
        STMu name' body' ->
          matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STBottom ->
      case actual of
        STBottom -> Just subst
        _ -> Nothing

recoverTypeHeadMatches :: StructuralOwnerRecovery -> ElaborateScope -> RecoverHeadIdentityContext -> String -> String -> Bool
recoverTypeHeadMatches ownerRecovery scope headIdentities expected actual =
  case (resolveExpectedHead expected, resolveActualHead actual) of
    (Just expectedIdentity, Just actualIdentity) -> sameSymbolIdentity expectedIdentity actualIdentity
    (Nothing, Nothing) ->
      case ownerRecovery of
        RecoverStructuralOwnerMetadataLight -> expected == actual
        _ -> False
    _ -> False
  where
    scopedHeadIdentities =
      typeHeadIdentitiesInScope scope

    resolveExpectedHead name =
      lookupSymbolIdentityAlias (recoverExpectedHeadIdentities headIdentities) name
        <|> metadataLightFallback name

    resolveActualHead name =
      lookupSymbolIdentityAlias (recoverActualHeadIdentities headIdentities) name
        <|> metadataLightFallback name

    metadataLightFallback name =
      case ownerRecovery of
        RecoverStructuralOwnerMetadataLight ->
          lookupSymbolIdentityAlias scopedHeadIdentities name
            <|> Builtins.builtinTypeHeadIdentity name
        _ -> Nothing

matchRecoverVarApp ::
  StructuralOwnerRecovery ->
  ElaborateScope ->
  RecoverHeadIdentityContext ->
  (String -> Maybe RecoverBinderKey) ->
  Map String RecoverBinderKey ->
  Map RecoverBinderKey SrcType ->
  Map String String ->
  String ->
  NonEmpty SrcType ->
  SrcType ->
  Maybe (Map RecoverBinderKey SrcType)
matchRecoverVarApp ownerRecovery scope headIdentities recoverBinderKey params subst renames name args actual
  | Just key <- Map.lookup name params =
      case actual of
        STCon actualName actualArgs ->
          matchAppliedHead key actualName toConHead (toListNE actualArgs)
        STVarApp actualName actualArgs ->
          matchAppliedHead key actualName toVarHead (toListNE actualArgs)
        _ -> Nothing
  | Just actualName <- Map.lookup name renames =
      matchRigidVarAppHead actualName
  | otherwise =
      matchRigidVarAppHead name
  where
    expectedArgs = toListNE args
    expectedArgCount = length expectedArgs

    matchAppliedHead key actualName headFromPrefix actualArgs
      | length actualArgs < expectedArgCount = Nothing
      | otherwise = do
          let (headArgs, appliedArgs) = splitAt (length actualArgs - expectedArgCount) actualArgs
          subst' <- bindRecoverParam ownerRecovery scope headIdentities key (headFromPrefix actualName headArgs) subst
          foldM
            (\acc (leftTy, rightTy) -> matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params acc renames leftTy rightTy)
            subst'
            (zip expectedArgs appliedArgs)

    matchRigidVarAppHead expectedName =
      case actual of
        STVarApp actualName actualArgs
          | recoverTypeHeadMatches ownerRecovery scope headIdentities expectedName actualName && expectedArgCount == length (toListNE actualArgs) ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType ownerRecovery scope headIdentities recoverBinderKey params acc renames leftTy rightTy)
                subst
                (zip expectedArgs (toListNE actualArgs))
        _ -> Nothing

    toConHead actualName [] = STBase actualName
    toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

    toVarHead actualName [] = STVar actualName
    toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

bindRecoverParam :: StructuralOwnerRecovery -> ElaborateScope -> RecoverHeadIdentityContext -> RecoverBinderKey -> SrcType -> Map RecoverBinderKey SrcType -> Maybe (Map RecoverBinderKey SrcType)
bindRecoverParam ownerRecovery scope headIdentities key actual subst =
  case Map.lookup key subst of
    Nothing -> Just (Map.insert key actual subst)
    Just existing
      | metadataLightRecovery,
        alphaEqSrcTypeInScope scope existing actual ->
          Just subst
      | Just existingTy <- srcTypeToElabTypeMaybeInScopeWithHeadIdentities scope combinedHeadIdentities existing,
        Just actualTy <- srcTypeToElabTypeMaybeInScopeWithHeadIdentities scope combinedHeadIdentities actual,
        metadataLightRecovery || (checkedHeadsComplete existing && checkedHeadsComplete actual),
        alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
          Just subst
      | otherwise -> Nothing
  where
    combinedHeadIdentities =
      if metadataLightRecovery
        then recoverCombinedHeadIdentities headIdentities
        else recoverActualHeadIdentities headIdentities

    metadataLightRecovery =
      case ownerRecovery of
        RecoverStructuralOwnerMetadataLight -> True
        _ -> False

    checkedHeadsComplete =
      go
      where
        go ty =
          case ty of
            STVar {} -> True
            STArrow dom cod -> go dom && go cod
            STBase name -> hasIdentity name
            STCon name args -> hasIdentity name && all go args
            STVarApp _ args -> all go args
            STTyLam _ body -> go body
            STTyApp fun arg -> go fun && go arg
            STForall _ mb body -> maybe True (go . unSrcBound) mb && go body
            STMu _ body -> go body
            STBottom -> True

        hasIdentity name =
          case lookupSymbolIdentityAlias (recoverActualHeadIdentities headIdentities) name of
            Just _ -> True
            Nothing -> False

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

surfaceFreeBindingReferences :: SurfaceExpr -> [SurfaceBindingReference]
surfaceFreeBindingReferences = Set.toAscList . go Set.empty
  where
    go :: Set BindingKey -> SurfaceExpr -> Set SurfaceBindingReference
    go bound expr = case expr of
      EVarNode reference ->
        freeReference bound reference
      ELit _ -> Set.empty
      ELamNode reference body -> go (Set.insert (bindingKeyForTermReference reference) bound) body
      ELamAnnNode reference _ body -> go (Set.insert (bindingKeyForTermReference reference) bound) body
      EApp fun arg -> go bound fun `Set.union` go bound arg
      ELetNode reference rhs body ->
        let bound' = Set.insert (bindingKeyForTermReference reference) bound
         in go bound' rhs `Set.union` go bound' body
      EAnn inner _ -> go bound inner

    freeReference bound reference
      | key `Set.member` bound = Set.empty
      | otherwise = Set.singleton (surfaceBindingReferenceFromTermReference reference)
      where
        key = bindingKeyForTermReference reference

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

elabTypeToRecoveredTypeView :: ElaborateScope -> X.Ty v -> TypeView
elabTypeToRecoveredTypeView scope ty =
  typeViewWithIdentityMaps
    (mergeSymbolIdentityMaps [typeViewHeadIdentities recoveredView, headIdentities])
    (elabTypeBinderIdentities ty)
    recoveredView
  where
    recoveredView =
      sourceTypeViewInScopeWithHeadIdentities scope headIdentities displayTy
    displayTy =
      recoverElabSourceType scope ty
    headIdentities =
      elabTypeHeadIdentities ty

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
      symbolIdentityAliasMapWith [(identity, [name])]

elabTypeBinderIdentities :: X.Ty v -> Map String TypeBinderIdentity
elabTypeBinderIdentities =
  go
  where
    go :: X.Ty a -> Map String TypeBinderIdentity
    go ty =
      case ty of
        X.TVarRef ref -> binder ref
        X.TArrow dom cod -> mergeTypeBinderIdentityMaps [go dom, go cod]
        X.TBaseWithIdentity {} -> Map.empty
        X.TConWithIdentity _ _ args -> mergeTypeBinderIdentityMaps (map go (NE.toList args))
        X.TVarAppRef ref args -> mergeTypeBinderIdentityMaps (binder ref : map go (NE.toList args))
        X.TForallRef ref mb body -> mergeTypeBinderIdentityMaps [binder ref, maybe Map.empty go mb, go body]
        X.TMuRef ref body -> mergeTypeBinderIdentityMaps [binder ref, go body]
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

srcTypeToElabTypeInScope :: ElaborateScope -> SrcTy n v -> Either ProgramError ElabType
srcTypeToElabTypeInScope scope ty =
  srcTypeToElabTypeInScopeWithHeadIdentities scope Map.empty ty

srcTypeToElabTypeInScopeWithHeadIdentities :: ElaborateScope -> Map String SymbolIdentity -> SrcTy n v -> Either ProgramError ElabType
srcTypeToElabTypeInScopeWithHeadIdentities scope extraHeadIdentities ty =
  let headIdentities = mergeSymbolIdentityMaps [extraHeadIdentities, typeHeadIdentitiesInScope scope]
      (refs, generator) = sourceTypeBinderRefsInScope headIdentities scope Map.empty ty
   in fst <$> srcTypeToElabTypeWithScopedHeadIdentities scope headIdentities refs generator ty

srcTypeToElabTypeMaybeInScopeWithHeadIdentities :: ElaborateScope -> Map String SymbolIdentity -> SrcTy n v -> Maybe ElabType
srcTypeToElabTypeMaybeInScopeWithHeadIdentities scope headIdentities =
  either (const Nothing) Just . srcTypeToElabTypeInScopeWithHeadIdentities scope headIdentities

sourceTypeViewInScopeWithHeadIdentities :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> TypeView
sourceTypeViewInScopeWithHeadIdentities scope extraHeadIdentities ty =
  typeViewMergeHeadIdentities extraHeadIdentities baseView
  where
    baseView =
      sourceTypeViewInScope scope ty

typeViewToElabType :: ElaborateScope -> TypeView -> Either ProgramError ElabType
typeViewToElabType scope view =
  fst <$> srcTypeToElabTypeWithScopedHeadIdentities scope headIdentities refs generator ty
  where
    ty =
      lowerTypeView scope view

    headIdentities =
      mergeSymbolIdentityMaps [typeViewHeadIdentityLookupAliases view, typeHeadIdentitiesInScope scope]

    (refs, generator) =
      typeViewBinderRefs headIdentities viewWithStructuralBinders ty

    viewWithStructuralBinders =
      typeViewMergeBinderIdentities (sourceTypeBinderIdentitiesInScope scope ty) view

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
      mergeTypeBinderIdentityMaps
        [ Map.singleton alias identity
        | (alias, identity) <- typeViewBinderIdentityAliasEntries view
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

srcTypeToElabTypeWithHeadIdentities ::
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithHeadIdentities =
  srcTypeToElabTypeWithHeadIdentitiesBound Set.empty

srcTypeToElabTypeWithScopedHeadIdentities ::
  ElaborateScope ->
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithScopedHeadIdentities scope =
  srcTypeToElabTypeWithHeadIdentityResolverBound Set.empty resolveHead
  where
    scopedHeadIdentities =
      dataTypeHeadIdentitiesInScope scope

    fallbackHeadIdentities =
      typeHeadIdentitiesInScope scope

    resolveHead name =
      lookupSymbolIdentityAlias scopedHeadIdentities name
        <|> Builtins.builtinTypeHeadIdentity name
        <|> lookupSymbolIdentityAlias fallbackHeadIdentities name

srcTypeToElabTypeWithHeadIdentityResolverBound ::
  Set String ->
  (String -> Maybe SymbolIdentity) ->
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithHeadIdentityResolverBound boundNames resolveHead headIdentities refs generator ty = case ty of
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
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithHeadIdentityResolverBound boundNames resolveHead headIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithHeadIdentityResolverBound boundNames' resolveHead headIdentities refs' generator2 body
          Right (X.TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFresh (Set.member name boundNames) refs name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithHeadIdentityResolverBound boundNames' resolveHead headIdentities refs' generator1 body
          Right (X.TMuRef ref body', generator2)
  STBottom ->
    Right (X.TBottom, generator)
  where
    go =
      srcTypeToElabTypeWithHeadIdentityResolverBound boundNames resolveHead headIdentities

    sourceTypeHeadIdentity name =
      resolveHead name <|> lookupSymbolIdentityAlias headIdentities name

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

srcTypeToElabTypeWithHeadIdentitiesBound ::
  Set String ->
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcTy n v ->
  Either ProgramError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithHeadIdentitiesBound boundNames headIdentities =
  srcTypeToElabTypeWithHeadIdentityResolverBound boundNames sourceTypeHeadIdentity headIdentities
  where
    sourceTypeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name

srcBoundToElabBoundWithHeadIdentityResolverBound ::
  Set String ->
  (String -> Maybe SymbolIdentity) ->
  Map String SymbolIdentity ->
  Map String X.TypeBinderRef ->
  IdentityGenerator ->
  SrcBound n ->
  Either ProgramError (Maybe X.BoundType, IdentityGenerator)
srcBoundToElabBoundWithHeadIdentityResolverBound boundNames resolveHead headIdentities refs generator (SrcBound boundTy) =
  case srcTypeToElabTypeWithHeadIdentityResolverBound boundNames resolveHead headIdentities refs generator boundTy of
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
  mergeSymbolIdentityMaps
    [ dataTypeHeadIdentitiesInScope scope,
      builtinTypeHeadIdentities
    ]

dataTypeHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity
dataTypeHeadIdentitiesInScope scope =
  mergeSymbolIdentityMaps
    [ Map.map dataInfoSymbol dataTypes,
      unambiguousDataTypeHeadIdentities dataTypes
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
