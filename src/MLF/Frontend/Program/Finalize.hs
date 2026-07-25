{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize
  ( FinalizeContext,
    ModuleFinalizeContext,
    mkFinalizeContext,
    mkFinalizeContextWithTiming,
    mkModuleFinalizeContext,
    finalizeBinding,
    finalizeBindingWithContext,
    finalizeBindingsAllowOpaqueWithContext,
    finalizeBindingsAllowOpaqueWithContextFromSupply,
    finalizeBindingsAllowOpaqueWithContextWithTiming,
    finalizeBindingsAllowOpaqueWithContextWithTimingFromSupply,
    finalizeBindingAllowOpaque,
    finalizeBindingAllowOpaqueWithContext,
    finalizeBindingAllowOpaqueWithContextFromSupply,
    finalizeBindingAllowOpaqueWithModuleContext,
    finalizeBindingLayerAllowOpaqueWithModuleContext,
    finalizeBindingLayerAllowOpaqueWithModuleContextWithTiming,
    finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply,
    finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTiming,
    finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply,
    finalizeBindingAllowOpaqueWithContextWithTiming,
    finalizeBindingAllowOpaqueWithContextWithTimingFromSupply,
    finalizeBindingAllowOpaqueWithModuleContextWithTiming,
    finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply,
    elabTypeToRecoveredTypeView,
    typeViewToElabType,
    srcTypeToElabTypeInScope,
    resolvedForallSubst,
    sourceForallMatchesInScope,
    consumeDeferredConstructorHeadInstantiationsForTest,
    consumeDeferredMethodHeadInstantiationsForTest,
    constructLocalOccurrencesForSchemeForTest,
    dropStaleTypeInstsForTest,
    normalizeCheckedTypeRedexesForTest,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (foldM, zipWithM)
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
    PipelineError (..),
    renderPipelineError,
    schemeFromType,
    schemeToType,
    typeCheckWithEnv,
  )
import MLF.Elab.Run.Pipeline
  ( PipelineElabDetailedResult (..),
    PreparedExternalBindings,
    prepareExternalBindingsWithTypeIdentities,
    preparedSourceTypeBinderIdentityCandidates,
    restrictPreparedExternalBindingsByKeys,
    extendPreparedExternalBindingTypeIdentityCandidates,
    preferPreparedExternalBindingTypeIdentities,
    reservePreparedExternalBindingIdentities,
    runPipelineElabDetailedResolvedWithPreparedExternalBindings,
    runPipelineElabDetailedResolvedWithPreparedExternalBindingsFromSupply,
    runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTimingFromSupply,
    runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTimingFromSupply,
    runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTimingFromSupply,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsFromSupply,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTimingFromSupply,
    unionPreparedExternalBindings,
  )
import MLF.Elab.SourceBinder (typeBinderDeclarationRefs)
import MLF.Elab.TermClosure (closeTermWithSchemeSubstRefsIfNeeded, renameTermTypeVars)
import MLF.Elab.Elaborate.Annotation (constructExactTermAtType)
import MLF.Elab.Types (XmlfTerm, ElabType)
import qualified MLF.Elab.Types as X
import qualified MLF.Elab.TypeCheck as TypeCheck
import qualified MLF.Elab.Reduce as Reduce
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
    alphaEqTypesInScope,
    alphaEqTypesWithHeadIdentitiesInScope,
    elaborateScopeDataTypes,
    elaborateScopeDataTypesByIdentity,
    elaborateScopeRuntimeTypeViews,
    elaborateScopeUniqueDataTypes,
    elaborateScopeValueInfos,
    elaborateScopeValueRuntimeAliases,
    classInfoForConstraint,
    constructorBindingSourceTypeView,
    constructorTypeView,
    diagnosticTypeViewDisplay,
    lookupEvidenceMethodByClassViews,
    lowerType,
    lowerTypeView,
    lowerTypeViewWithIdentities,
    lowerTypeViewsWithIdentities,
    matchTypesWithHeadIdentitiesInScope,
    matchMethodTypeViews,
    matchTypeViewsAgainstIdentity,
    matchTypeViewsAgainstIdentityRefiningBottom,
    rigidEvidenceTypeViewsMatch,
    resolveInstanceInfoByConstraint,
    resolveMethodInstanceInfoByTypeView,
    sourceTypeBinderIdentitiesInScope,
    sourceTypeHeadIdentitiesInScope,
    requireTypeViewFromSourceTypeInScope,
    zeroMethodConstraintCoveredByEvidenceInfo,
  )
import MLF.Frontend.Program.Finalize.IdentitySupply
  ( freshTypeBinderRefs,
    freshTypeBinderRefsWithSupply,
    freshenElabTypeBindersAgainstTypesFromSupply,
  )
import MLF.Frontend.Program.Finalize.DeferredConstruction
  ( projectDeferredConstructorConstructionRoutes,
  )
import MLF.Frontend.Program.Types
  ( CheckedBinding (..),
    checkedBindingsIdentityGenerator,
    ConstructorForallBinder (..),
    ConstructorInfo (..),
    ConstructorShape (..),
    ctorForallBinderInfo,
    constructorShapeForallBinderInfo,
    DataInfo (..),
    DeferredMethodEvidence (..),
    DeferredCaseCall (..),
    DeferredBindingMode (..),
    DeferredConstructorCall (..),
    DeferredMethodCall (..),
    deferredMethodResolutionArgCount,
    deferredMethodTotalArgCount,
    DeferredProgramObligation (..),
    DeferredObligations,
    ClassInfo (..),
    EvidenceMethod (..),
    EvidenceInfo (..),
    InstanceInfo (..),
    IdDetails (..),
    LoweredBinding (..),
    LoweredBindingIdentity,
    loweredIdentityRuntimeName,
    loweredBindingSourceType,
    loweredBindingExpectedType,
    LoweredResolvedLocalIdentity (..),
    MethodInfo (..),
    ProgramError (..),
    ProgramSourceTypeShape (..),
    ConstraintInfo (..),
    ClassApplicationKey,
    TypeView,
    typeViewBinderIdentities,
    typeViewBinderIdentityAliasEntries,
    typeViewDisplay,
    typeViewForallBinderViews,
    typeViewHeadIdentities,
    typeViewIdentity,
    typeViewFromElabType,
    typeViewOverlayDisplay,
    typeViewToResolved,
    resolvedSourceTypeToElabType,
    TypeBinderSubst,
    TypeViewSubst,
    ValueInfo (..),
    applyConstraintInfoSubst,
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
    constructorInfoArgViews,
    constructorInfoIdentityName,
    constructorInfoResultView,
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
    lookupTypeViewSubst,
    lookupMethodParamViewSubst,
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
    splitForalls,
    specializeMethodTypeView,
    specializeQuantifiedTypeView,
    typeViewBinderIdentityForAlias,
    typeViewIsBareBinderIdentity,
    substituteTypeVar,
    typeViewSubstFromParamIdentities,
    typeViewHeadIdentityLookupAliases,
    typeViewGeneratedIdentities,
    typeViewMergeBinderIdentityAliases,
    mapTypeViewDisplayHeadNames,
    typeViewWithDisplay,
    typeViewWithIdentityAliases,
    checkedTypeParamIdentity,
    checkedTypeParamName,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToTypeViewSubst,
    typeBinderAliasIdentityMap,
    uniqueEvidenceMethod,
    valueInfoRuntimeDetails,
    lookupTypeBinderSubstViewByIdentity,
    insertTypeBinderSubstView,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), ResolvedNormSurfaceExpr, SrcBound (..), SrcTy (..), SrcType, ResolvedSurfaceExpr, TermReference (..), TermReferencePhase (ResolvedTermReferences), resolvedTermReferenceDetails, termReferenceName)
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarRefsType, freshNameLike, implicitForallClosureMatches, matchTypeRefs, splitForallsRefs, substTypeCaptureRef)
import MLF.Types.Identity
  ( DeferredRef,
    deferredRefIdentity,
    deferredRefName,
    IdentityGenerator,
    localRefGeneratedIdentities,
    TypeBinderIdentity,
    StructuralTypeBinderRole (StructuralSelfBinder),
    UniqueIdentity,
    freshLocalRef,
    idDetailsAliasMapWith,
    idDetailsAliasNamesWith,
    idDetailsRuntimeName,
    advanceIdentityGeneratorPastMany,
    identityGeneratorAfter,
    renameDeferredRef,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
    typeBinderIdentityNode,
    typeBinderIdentityStructural,
    typeBinderIdentityAliasMap,
    typeBinderIdentityStableName,
    uniqueIdentityStableName,
  )
import MLF.Util.Timing (TimingConfig(..), defaultTimingConfig, timeProgramDetailIO, timeProgramOperationIO)

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
    moduleBindingReadNormalizedExpr :: Either ProgramError ResolvedNormSurfaceExpr,
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

surfaceBindingReferenceFromTermReference :: TermReference 'ResolvedTermReferences -> SurfaceBindingReference
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
  let runtimeTypeViews =
        lowerTypeViewsWithIdentities scope (runtimeTypeViewsWithVisibleConstructors scope)
      runtimeSourceTypes = Map.map typeViewDisplay runtimeTypeViews
  runtimeTypeEnv <- traverse resolvedTypeViewToElabType runtimeTypeViews
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

mkFinalizeContextWithTiming :: TimingConfig -> String -> ElaborateScope -> IO (Either ProgramError FinalizeContext)
mkFinalizeContextWithTiming timing label scope = do
  runtimeTypeViews <-
    timeProgramDetailIO timing (label ++ ".runtime-type-views") $
      evaluate $
        lowerTypeViewsWithIdentities scope (runtimeTypeViewsWithVisibleConstructors scope)
  let runtimeSourceTypes = Map.map typeViewDisplay runtimeTypeViews
  runtimeTypeEnvResult <-
    timeProgramDetailIO timing (label ++ ".runtime-type-env") $
      evaluate (traverse resolvedTypeViewToElabType runtimeTypeViews)
  case runtimeTypeEnvResult of
    Left err -> pure (Left err)
    Right runtimeTypeEnv -> do
      let runtimeIndex = runtimeExternalBindingIndexFromScope scope runtimeTypeEnv
          runtimeSourceTypesWithAliases = runtimeSourceTypesWithIdentityAliases runtimeSourceTypes runtimeIndex
      runtimeBindingsResult <-
        timeProgramDetailIO timing (label ++ ".runtime-bindings") $
          evaluate $
            prepareSurfaceExternalBindingsWithIdentity
              scope
              (externalBindingModeForRuntime scope runtimeSourceTypes runtimeIndex)
              (runtimeExternalBindingIdentityByAlias runtimeIndex)
              runtimeTypeViews
      pure $ do
        runtimeBindings <- runtimeBindingsResult
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
        [ (alias, [constructorBindingSourceTypeView scope ctor])
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
        either
          (Left . ProgramPipelineError . show)
          Right
          (normalizeExpr (checkedBindingSurfaceExpr context lowered)),
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
        surfaceBindingReferenceMode scope runtimeSourceTypes runtimeIndex deferredExternalIndex reference == ExternalBindingScheme
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
      case finalizeBindingWithContext context lowered of
        Right checked
          | Map.null (loweredBindingDeferredObligations lowered) ->
              -- Successful elaboration can still satisfy an opaque forall by
              -- instantiating the expected type. Re-check no-obligation
              -- surfaces before accepting the elaborated result.
              case validateOpaqueBindingSurface context lowered of
                Right () -> Right checked
                Left validationErr -> Left validationErr
          | otherwise -> Right checked
        Left err ->
          case validateOpaqueBindingSurface context lowered of
            Right () -> do
              placeholderTy <- loweredExpectedTypeToElabType scope lowered
              finalizeOpaqueUncheckedBindingWithContext context lowered placeholderTy
            Left validationErr ->
              Left (classifyOpaqueAnnotationFailure lowered err validationErr)
  | otherwise = finalizeBindingWithContext context lowered
  where
    scope = finalizeContextScope context

finalizeBindingAllowOpaqueWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> LoweredBinding -> Either ProgramError (CheckedBinding, IdentityGenerator)
finalizeBindingAllowOpaqueWithContextFromSupply generator context lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) = do
      case finalizeBindingWithContextFromSupply generator context lowered of
        Right (checked, generator')
          | Map.null (loweredBindingDeferredObligations lowered) ->
              case validateOpaqueBindingSurface context lowered of
                Right () -> Right (checked, generator')
                Left validationErr -> Left validationErr
          | otherwise -> Right (checked, generator')
        Left err ->
          case validateOpaqueBindingSurface context lowered of
            Right () -> do
              placeholderTy <- loweredExpectedTypeToElabType scope lowered
              finalizeOpaqueUncheckedBindingWithContextFromSupply generator context lowered placeholderTy
            Left validationErr ->
              Left (classifyOpaqueAnnotationFailure lowered err validationErr)
  | otherwise = finalizeBindingWithContextFromSupply generator context lowered
  where
    scope = finalizeContextScope context

finalizeOpaqueUncheckedBindingWithContext :: FinalizeContext -> LoweredBinding -> ElabType -> Either ProgramError CheckedBinding
finalizeOpaqueUncheckedBindingWithContext context lowered0 placeholderTy = do
  validateDeferredObligationIdentities (loweredBindingIdentity lowered0) (loweredBindingDeferredObligations lowered0)
  let lowered = lowered0
  pipelineResult <-
    runSurfacePipelineWithContext
      context
      [lowered]
      True
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypeViews lowered)
      (checkedBindingSurfaceExpr context lowered)
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
        loweredBindingSourceTypeView lowered
  acceptResolvedCheckedBinding
    (runtimeTypeCheckEnv context)
    lowered
    sourceTypeView
    resolvedDeferredObligations
    resolvedTerm
    placeholderTy

finalizeOpaqueUncheckedBindingWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> LoweredBinding -> ElabType -> Either ProgramError (CheckedBinding, IdentityGenerator)
finalizeOpaqueUncheckedBindingWithContextFromSupply generator context lowered0 placeholderTy = do
  validateDeferredObligationIdentities (loweredBindingIdentity lowered0) (loweredBindingDeferredObligations lowered0)
  let lowered = lowered0
  pipelineResult <-
    runSurfacePipelineWithContextFromSupply
      generator
      context
      [lowered]
      True
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypeViews lowered)
      (checkedBindingSurfaceExpr context lowered)
  let PipelineElabDetailedResult {pedTerm = term0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  term <- finalizeOpaqueDeferredConstructors context (loweredBindingDeferredObligations lowered) tcEnv term0
  let resolvedTerm =
        alignLeadingTypeAbsRefsToType placeholderTy
          . TypeCheck.canonicalizeResolvedTermTypes (runtimeTypeCheckEnv context)
          $ term
      resolvedDeferredObligations =
        annotateDeferredEvidenceResolvedVars resolvedTerm (loweredBindingDeferredObligations lowered)
      sourceTypeView = loweredBindingSourceTypeView lowered
  checked <-
    acceptResolvedCheckedBinding
      (runtimeTypeCheckEnv context)
      lowered
      sourceTypeView
      resolvedDeferredObligations
      resolvedTerm
      placeholderTy
  pure
    ( checked,
      checkedBindingsIdentityGenerator (pedIdentityGenerator pipelineResult) [checked]
    )

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
      case inferOpaqueSurfaceType scope headIdentities rigidVars runtimeTypeFor Map.empty (loweredBindingSurfaceExpr lowered) of
        Right actualTy
          | opaqueSourceCompatibleWithRigid scope headIdentities rigidVars actualTy (loweredBindingExpectedType lowered) ->
              validateOpaqueBindingRawSurface scope rigidVars runtimeTypeFor lowered
          | otherwise -> Left (ProgramTypeMismatch actualTy (loweredBindingExpectedType lowered))
        Left err -> Left err
  where
    scope = finalizeContextScope context
    headIdentities = opaqueBindingHeadIdentities scope lowered
    rigidVars = sourceForallBinders (loweredBindingExpectedType lowered)
    runtimeTypeFor =
      opaqueRuntimeSourceType context lowered

-- | Opaque builtins are intentionally validated at the source surface when
-- their ordinary eMLF construction fails.  If that independent check proves
-- the source expression cannot inhabit its annotation, the public failure is
-- a type mismatch, not a compiler invariant.  Retain the pipeline error as a
-- structured cause so the internal construction evidence is not discarded.
classifyOpaqueAnnotationFailure :: LoweredBinding -> ProgramError -> ProgramError -> ProgramError
classifyOpaqueAnnotationFailure lowered pipelineError validationError =
  case validationError of
    ProgramTypeMismatch mismatchActual mismatchExpected ->
      ProgramTypeMismatchWithCause mismatchActual mismatchExpected pipelineError
    _
      | Just shape <- opaqueSurfaceTypeShape (loweredBindingSurfaceExpr lowered),
        not (sourceTypeAcceptsShape shape expected) ->
          ProgramTypeShapeMismatchWithCause shape expected pipelineError
      | otherwise -> pipelineError
  where
    expected = loweredBindingExpectedType lowered

-- | A deliberately small source-level proof.  A bare lambda has an arrow
-- type regardless of the unknown parameter and result types, so this records
-- only that outer shape instead of inventing metavariables or reusing the
-- declaration's expected type as its inferred source type.
opaqueSurfaceTypeShape :: ResolvedSurfaceExpr -> Maybe ProgramSourceTypeShape
opaqueSurfaceTypeShape expr =
  case expr of
    ELamNode {} -> Just ProgramSourceArrowShape
    _ -> Nothing

-- | Check only the source type's outer shape.  Leading foralls do not change
-- the shape of their body; nominal data encodings produced by 'lowerType' are
-- intentionally irrelevant at this source boundary.
sourceTypeAcceptsShape :: ProgramSourceTypeShape -> SrcType -> Bool
sourceTypeAcceptsShape shape ty =
  case ty of
    STForall _ _ body -> sourceTypeAcceptsShape shape body
    STArrow {} -> shape == ProgramSourceArrowShape
    _ -> False

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

validateOpaqueBindingRawSurface :: ElaborateScope -> Set String -> (BindingKey -> String -> Maybe SrcType) -> LoweredBinding -> Either ProgramError ()
validateOpaqueBindingRawSurface scope rigidVars runtimeTypeFor lowered =
  case inferOpaqueSurfaceTypeIgnoringAscriptions scope headIdentities rigidVars runtimeTypeFor Map.empty (loweredBindingSurfaceExpr lowered) of
    Right actualTy
      | opaqueSourceCompatibleWithRigid scope headIdentities rigidVars actualTy (loweredBindingExpectedType lowered) -> Right ()
      | otherwise -> Left (ProgramTypeMismatch actualTy (loweredBindingExpectedType lowered))
    Left err -> Left err
  where
    headIdentities = opaqueBindingHeadIdentities scope lowered

opaqueBindingHeadIdentities :: ElaborateScope -> LoweredBinding -> Map String SymbolIdentity
opaqueBindingHeadIdentities scope lowered =
  mergeSymbolIdentityMaps
    ( symbolIdentityAliasMap scopeTypeIdentities
        : typeViewHeadIdentities (loweredBindingSourceTypeView lowered)
        : typeViewHeadIdentities (loweredBindingExpectedTypeView lowered)
        : map typeViewHeadIdentities (Map.elems (loweredBindingExternalTypeViews lowered))
    )
  where
    scopeTypeIdentities =
      Map.keys (elaborateScopeDataTypesByIdentity scope)
        ++ map Builtins.builtinTypeIdentity (Set.toList Builtins.builtinTypeNames)

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

inferOpaqueSurfaceType :: ElaborateScope -> Map String SymbolIdentity -> Set String -> (BindingKey -> String -> Maybe SrcType) -> Map BindingKey SrcType -> ResolvedSurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceType = inferOpaqueSurfaceTypeWithAscriptions True

inferOpaqueSurfaceTypeIgnoringAscriptions :: ElaborateScope -> Map String SymbolIdentity -> Set String -> (BindingKey -> String -> Maybe SrcType) -> Map BindingKey SrcType -> ResolvedSurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceTypeIgnoringAscriptions = inferOpaqueSurfaceTypeWithAscriptions False

inferOpaqueSurfaceTypeWithAscriptions :: Bool -> ElaborateScope -> Map String SymbolIdentity -> Set String -> (BindingKey -> String -> Maybe SrcType) -> Map BindingKey SrcType -> ResolvedSurfaceExpr -> Either ProgramError SrcType
inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope headIdentities rigidVars runtimeTypeFor localTypes expr =
  case expr of
    EVarNode reference -> inferReference (bindingKeyForTermReference reference) (termReferenceName reference)
    ELit lit -> Right (literalSourceType lit)
    ELamAnnNode reference ty body ->
      STArrow ty
        <$> inferOpaqueSurfaceTypeWithAscriptions
          keepAscriptions
          scope
          headIdentities
          rigidVars
          runtimeTypeFor
          (Map.insert (bindingKeyForTermReference reference) ty localTypes)
          body
    EExactLamNode reference ty body ->
      STArrow ty
        <$> inferOpaqueSurfaceTypeWithAscriptions
          keepAscriptions
          scope
          headIdentities
          rigidVars
          runtimeTypeFor
          (Map.insert (bindingKeyForTermReference reference) ty localTypes)
          body
    ELamNode {} ->
      Left (ProgramPipelineError "opaque validation needs lambda annotations")
    EApp fun arg -> do
      funTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope headIdentities rigidVars runtimeTypeFor localTypes fun
      argTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope headIdentities rigidVars runtimeTypeFor localTypes arg
      applyOpaqueFunctionType scope headIdentities funTy argTy
    ELetNode reference rhs body -> do
      rhsTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope headIdentities rigidVars runtimeTypeFor localTypes rhs
      inferOpaqueSurfaceTypeWithAscriptions
        keepAscriptions
        scope
        headIdentities
        rigidVars
        runtimeTypeFor
        (Map.insert (bindingKeyForTermReference reference) rhsTy localTypes)
        body
    EAnn inner annTy -> do
      actualTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope headIdentities rigidVars runtimeTypeFor localTypes inner
      let exact =
            alphaEqTypesWithHeadIdentitiesInScope scope headIdentities actualTy annTy
              || alphaEqTypesWithHeadIdentitiesInScope scope headIdentities (lowerType scope actualTy) (lowerType scope annTy)
      if exact
        then Right (if keepAscriptions then annTy else actualTy)
        else
          if opaqueSourceCompatibleWithRigid scope headIdentities rigidVars actualTy annTy
            then Right actualTy
            else Left (ProgramTypeMismatch actualTy annTy)
    EExactAnn inner annTy _ -> do
      actualTy <- inferOpaqueSurfaceTypeWithAscriptions keepAscriptions scope headIdentities rigidVars runtimeTypeFor localTypes inner
      let exact =
            alphaEqTypesWithHeadIdentitiesInScope scope headIdentities actualTy annTy
              || alphaEqTypesWithHeadIdentitiesInScope scope headIdentities (lowerType scope actualTy) (lowerType scope annTy)
      if exact
        then Right annTy
        else Left (ProgramTypeMismatch actualTy annTy)
  where
    inferReference key name =
      case Map.lookup key localTypes <|> runtimeTypeFor key name of
        Just ty -> Right ty
        Nothing -> Left (ProgramUnknownValue name)

applyOpaqueFunctionType :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> SrcType -> Either ProgramError SrcType
applyOpaqueFunctionType scope headIdentities funTy argTy =
  case snd (splitForalls funTy) of
    STArrow paramTy resultTy ->
      case matchTypes paramTy argTy <|> matchTypes (lowerType scope paramTy) (lowerType scope argTy) of
        Just subst -> Right (Map.foldrWithKey substituteTypeVar resultTy subst)
        Nothing
          | opaqueSourceCompatible scope headIdentities argTy paramTy -> Right resultTy
          | otherwise -> Left (ProgramTypeMismatch argTy paramTy)
    other -> Left (ProgramExpectedFunction other)
  where
    matchTypes =
      matchTypesWithHeadIdentitiesInScope scope headIdentities Map.empty

opaqueSourceCompatible :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> SrcType -> Bool
opaqueSourceCompatible scope headIdentities =
  opaqueSourceCompatibleWithRigid scope headIdentities Set.empty

opaqueSourceCompatibleWithRigid :: ElaborateScope -> Map String SymbolIdentity -> Set String -> SrcType -> SrcType -> Bool
opaqueSourceCompatibleWithRigid scope headIdentities rigidVars actualTy expectedTy =
  alphaEq actualTy expectedTy
    || alphaEq (lowerType scope actualTy) (lowerType scope expectedTy)
    || sourceTypeMatchesWithRigid scope headIdentities rigidVars expectedTy actualTy
    || sourceForallMatchesWithRigidForallsAndHeadIdentitiesInScope scope headIdentities expectedTy actualTy
  where
    alphaEq =
      alphaEqTypesWithHeadIdentitiesInScope scope headIdentities

sourceTypeMatchesWithRigid :: ElaborateScope -> Map String SymbolIdentity -> Set String -> SrcType -> SrcType -> Bool
sourceTypeMatchesWithRigid scope headIdentities rigidVars expectedTy actualTy =
  case matchTypes expectedTy actualTy <|> matchTypes (lowerType scope expectedTy) (lowerType scope actualTy) of
    Just subst -> all rigidSubstitutionAllowed (Map.toList subst)
    Nothing -> False
  where
    matchTypes =
      matchTypesWithHeadIdentitiesInScope scope headIdentities Map.empty

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
  validateDeferredObligationIdentities (loweredBindingIdentity lowered0) (loweredBindingDeferredObligations lowered0)
  let lowered = lowered0
  metadataBinding <- finalizeConstructorBindingFromMetadata context lowered
  case metadataBinding of
    Just checked -> Right checked
    Nothing -> finalizeBindingWithSurfacePipeline context lowered

finalizeBindingWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> LoweredBinding -> Either ProgramError (CheckedBinding, IdentityGenerator)
finalizeBindingWithContextFromSupply generator context lowered0 = do
  validateDeferredObligationIdentities (loweredBindingIdentity lowered0) (loweredBindingDeferredObligations lowered0)
  let lowered = lowered0
  metadataBinding <- finalizeConstructorBindingFromMetadataFromSupply generator context lowered
  case metadataBinding of
    Just finalized -> Right finalized
    Nothing -> finalizeBindingWithSurfacePipelineFromSupply generator context lowered

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
      (checkedBindingSurfaceExpr context lowered)
  let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  (term, actualTy) <-
    finalizeDeferredObligationsForBinding context lowered (loweredBindingDeferredObligations lowered) tcEnv term0 actualTy0
  finalizeCheckedBindingFromTerm context lowered term actualTy

finalizeBindingWithSurfacePipelineFromSupply :: IdentityGenerator -> FinalizeContext -> LoweredBinding -> Either ProgramError (CheckedBinding, IdentityGenerator)
finalizeBindingWithSurfacePipelineFromSupply generator context lowered0 = do
  let lowered = lowered0
  pipelineResult <-
    runSurfacePipelineWithContextFromSupply
      generator
      context
      [lowered]
      False
      (loweredBindingDeferredObligations lowered)
      (loweredBindingExternalTypeViews lowered)
      (checkedBindingSurfaceExpr context lowered)
  let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  (term, actualTy, deferredGenerator) <-
    finalizeDeferredObligationsForBindingFromSupply
      (pedIdentityGenerator pipelineResult)
      context
      lowered
      (loweredBindingDeferredObligations lowered)
      tcEnv
      term0
      actualTy0
  checked <- finalizeCheckedBindingFromTerm context lowered term actualTy
  pure
    ( checked,
      checkedBindingsIdentityGenerator deferredGenerator [checked]
    )

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

finalizeBindingAllowOpaqueWithContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  FinalizeContext ->
  LoweredBinding ->
  IO (Either ProgramError (CheckedBinding, IdentityGenerator))
finalizeBindingAllowOpaqueWithContextWithTimingFromSupply timing label generator context lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) =
      timeProgramOperationIO timing (label ++ ".opaque_fallback") $
        evaluate (finalizeBindingAllowOpaqueWithContextFromSupply generator context lowered)
  | otherwise =
      finalizeBindingWithContextWithTimingFromSupply timing label generator context False lowered

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

finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError (CheckedBinding, IdentityGenerator))
finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply timing label generator moduleContext preferUnchecked lowered
  | Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered) =
      finalizeBindingAllowOpaqueWithContextWithTimingFromSupply timing label generator baseContext lowered
  | otherwise =
      finalizeBindingWithModuleContextWithTimingFromSupply timing label generator moduleContext preferUnchecked lowered
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
        finalizeDeferredObligationsForBinding context stampedLowered (loweredBindingDeferredObligations stampedLowered) tcEnv term0 actualTy0
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
  case validateDeferredObligationIdentities (loweredBindingIdentity lowered0) (loweredBindingDeferredObligations lowered0) of
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
                (checkedBindingSurfaceExpr context lowered)
          finalizePipelineBindingResult timing label context lowered pipelineResult
        Left err -> pure (Left err)

finalizeBindingWithContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  FinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError (CheckedBinding, IdentityGenerator))
finalizeBindingWithContextWithTimingFromSupply timing label generator context forceUnchecked lowered0 = do
  case validateDeferredObligationIdentities (loweredBindingIdentity lowered0) (loweredBindingDeferredObligations lowered0) of
    Left err -> pure (Left err)
    Right () -> do
      let lowered = lowered0
      metadataResult <-
        timeProgramOperationIO timing (label ++ ".constructor_metadata") $
          evaluate (finalizeConstructorBindingFromMetadataFromSupply generator context lowered)
      case metadataResult of
        Right (Just finalized) -> pure (Right finalized)
        Right Nothing -> do
          pipelineResult <-
            timeProgramOperationIO timing (label ++ ".pipeline") $
              runSurfacePipelineWithContextWithTimingFromSupply
                timing
                (label ++ ".pipeline")
                generator
                context
                [lowered]
                forceUnchecked
                (loweredBindingDeferredObligations lowered)
                (loweredBindingExternalTypeViews lowered)
                (checkedBindingSurfaceExpr context lowered)
          case pipelineResult of
            Left err -> pure (Left err)
            Right pipelineResult0 ->
              finalizePipelineBindingResultWithReadContextFromSupply
                timing
                label
                context
                Nothing
                (pedIdentityGenerator pipelineResult0)
                lowered
                pipelineResult0
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

finalizeBindingWithModuleContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError (CheckedBinding, IdentityGenerator))
finalizeBindingWithModuleContextWithTimingFromSupply timing label generator moduleContext forceUnchecked lowered0 = do
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
          evaluate (finalizeConstructorBindingFromMetadataFromSupply generator context lowered)
      case metadataResult of
        Right (Just finalized) -> pure (Right finalized)
        Right Nothing -> do
          pipelineResult <-
            timeProgramOperationIO timing (label ++ ".pipeline") $
              runLoweredSurfacePipelineWithModuleContextWithTimingFromSupply
                timing
                (label ++ ".pipeline")
                generator
                moduleContext
                forceUnchecked
                lowered
          let mbCheckContext =
                case mbReadContext of
                  Right readContext -> Just (moduleBindingReadCheckContext readContext)
                  Left _ -> Nothing
          case pipelineResult of
            Left err -> pure (Left err)
            Right pipelineResult0 ->
              finalizePipelineBindingResultWithReadContextFromSupply
                timing
                label
                context
                mbCheckContext
                (pedIdentityGenerator pipelineResult0)
                lowered
                pipelineResult0
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
                runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTiming
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
                  runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTiming
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

finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply _ _ generator _ [] =
  pure (Right ([], generator))
finalizeBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply timing label generator moduleContext lowereds
  | any (not . moduleLayerPipelineEligible) lowereds =
      finalizeLayerIndividuallyWithTimingFromSupply timing (label ++ ".fallback_unsupported") generator moduleContext lowereds
  | otherwise =
      case traverse (lookupModuleBindingReadContext moduleContext) lowereds of
        Left _ ->
          finalizeLayerIndividuallyWithTimingFromSupply timing (label ++ ".fallback_missing_context") generator moduleContext lowereds
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
                  runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTimingFromSupply
                    innerTiming
                    (label ++ ".pipeline.elab_pipeline")
                    generator
                    Set.empty
                    extEnv
                    rootPrepared
                    namedExprs
            case pipelineResult of
              Left _ ->
                ExceptT $
                  finalizeLayerIndividuallyWithTimingFromSupply timing (label ++ ".fallback_pipeline") generator moduleContext lowereds
              Right results ->
                ExceptT $
                  finalizeLayerPipelineResultsWithSupply timing label context generator lowereds readContexts results
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
                  runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming
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

finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply _ _ generator _ [] =
  pure (Right ([], generator))
finalizeDeferredBindingLayerAllowOpaqueWithModuleContextWithTimingFromSupply timing label generator moduleContext lowereds
  | any (not . moduleDeferredLayerPipelineEligible) lowereds =
      finalizeLayerIndividuallyWithTimingFromSupply timing (label ++ ".fallback_unsupported") generator moduleContext lowereds
  | otherwise =
      case traverse (lookupModuleBindingReadContext moduleContext) lowereds of
        Left _ ->
          finalizeLayerIndividuallyWithTimingFromSupply timing (label ++ ".fallback_missing_context") generator moduleContext lowereds
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
                  runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTimingFromSupply
                    innerTiming
                    (label ++ ".pipeline.elab_pipeline")
                    generator
                    Set.empty
                    extEnv
                    rootPrepared
                    namedExprs
            case pipelineResult of
              Left _ ->
                ExceptT $
                  finalizeLayerIndividuallyWithTimingFromSupply timing (label ++ ".fallback_pipeline") generator moduleContext lowereds
              Right results ->
                ExceptT $
                  finalizeLayerPipelineResultsWithSupply timing label context generator lowereds readContexts results
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
  Either ProgramError (PreparedExternalBindings, Map ModuleBindingReadKey PreparedExternalBindings, [(ModuleBindingReadKey, String, ResolvedNormSurfaceExpr)])
prepareModuleLayerPipelineInputs lowereds readContexts = do
  mapM_ moduleBindingReadResolvedFreeVars readContexts
  extEnvs <- traverse moduleBindingReadExternalBindings readContexts
  extEnv0 <- combinePreparedExternalBindings extEnvs
  let extEnv = extendPreparedWithLoweredTypeIdentities lowereds extEnv0
      rootExtEnvs =
        zipWith
          (\lowered extEnvForRoot -> preferPreparedWithLoweredTypeIdentities [lowered] extEnvForRoot)
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
  IO (Either ProgramError (PreparedExternalBindings, Map ModuleBindingReadKey PreparedExternalBindings, [(ModuleBindingReadKey, String, ResolvedNormSurfaceExpr)]))
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
                (\lowered extEnvForRoot -> preferPreparedWithLoweredTypeIdentities [lowered] extEnvForRoot)
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

moduleLayerKeyedExprs :: [LoweredBinding] -> [ResolvedNormSurfaceExpr] -> Either ProgramError [(ModuleBindingReadKey, String, ResolvedNormSurfaceExpr)]
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

finalizeLayerIndividuallyWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  ModuleFinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeLayerIndividuallyWithTimingFromSupply timing label generator0 moduleContext lowereds =
  runExceptT (go generator0 (1 :: Int) [] lowereds)
  where
    go generator _ acc [] = pure (reverse acc, generator)
    go generator index acc (lowered : rest) = do
      (checked, generator') <-
        ExceptT $
          finalizeBindingAllowOpaqueWithModuleContextWithTimingFromSupply
            timing
            (label ++ ".def_" ++ show index)
            generator
            moduleContext
            False
            lowered
      go generator' (index + 1) (checked : acc) rest

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

finalizeLayerPipelineResultsWithSupply ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  IdentityGenerator ->
  [LoweredBinding] ->
  [ModuleBindingReadContext] ->
  Map ModuleBindingReadKey PipelineElabDetailedResult ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeLayerPipelineResultsWithSupply timing label context generator lowereds readContexts results = do
  runExceptT $ do
    let pipelineGenerator =
          case Map.lookupMax results of
            Nothing -> generator
            Just (_, result) -> pedIdentityGenerator result
    go pipelineGenerator [] (1 :: Int) lowereds readContexts
  where
    go currentGenerator acc _ [] [] =
      pure (reverse acc, currentGenerator)
    go currentGenerator acc index (lowered : rest) (readContext : readRest) = do
      key <- fromProgramEither (loweredBindingReadKey lowered)
      pipelineResult <-
        case Map.lookup key results of
          Nothing ->
            fromProgramEither
              (Left (ProgramPipelineError ("module layer missing result for binding `" ++ loweredBindingName lowered ++ "`")))
          Just result -> pure result
      let stampedLowered = moduleBindingReadLowered readContext
      (checked, nextGenerator) <-
        ExceptT $
          finalizePipelineBindingResultWithReadContextFromSupply
            timing
            (label ++ ".binding_" ++ show index)
            context
            (Just (moduleBindingReadCheckContext readContext))
            currentGenerator
            stampedLowered
            pipelineResult
      checked `seq` go nextGenerator (checked : acc) (index + 1) rest readRest
    go _ _ _ _ _ =
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
    evaluateFinalizeEither timing (label ++ ".binding_check") $
      finalizeCheckedBindingFromTermWithReadContext context mbCheckContext lowered term actualTy

finalizePipelineBindingResultWithReadContextFromSupply ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  Maybe BindingCheckReadContext ->
  IdentityGenerator ->
  LoweredBinding ->
  PipelineElabDetailedResult ->
  IO (Either ProgramError (CheckedBinding, IdentityGenerator))
finalizePipelineBindingResultWithReadContextFromSupply timing label context mbCheckContext generator lowered pipelineResult0 =
  runExceptT $ do
    let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
          pipelineResult0
    (term, actualTy, deferredGenerator) <-
      evaluateFinalizeEither timing (label ++ ".deferred_obligations") $
        finalizeDeferredObligationsForBindingFromSupply
          generator
          context
          lowered
          (loweredBindingDeferredObligations lowered)
          tcEnv
          term0
          actualTy0
    checked <-
      evaluateFinalizeEither timing (label ++ ".binding_check") $
        finalizeCheckedBindingFromTermWithReadContext context mbCheckContext lowered term actualTy
    pure
      ( checked,
        checkedBindingsIdentityGenerator deferredGenerator [checked]
      )

finalizeConstructorBindingFromMetadata :: FinalizeContext -> LoweredBinding -> Either ProgramError (Maybe CheckedBinding)
finalizeConstructorBindingFromMetadata context lowered
  | not (loweredBindingIsConstructor lowered) = Right Nothing
  | otherwise = do
      (term, expectedTy) <- metadataConstructorTerm context lowered
      Just <$> acceptMetadataConstructorBinding context lowered term expectedTy

finalizeConstructorBindingFromMetadataFromSupply ::
  IdentityGenerator ->
  FinalizeContext ->
  LoweredBinding ->
  Either ProgramError (Maybe (CheckedBinding, IdentityGenerator))
finalizeConstructorBindingFromMetadataFromSupply generator context lowered
  | not (loweredBindingIsConstructor lowered) = Right Nothing
  | otherwise = do
      (term, expectedTy, generatorAfterTerm) <-
        metadataConstructorTermFromSupply generator context lowered
      checked <- acceptMetadataConstructorBinding context lowered term expectedTy
      Right
        ( Just
            ( checked,
              checkedBindingsIdentityGenerator generatorAfterTerm [checked]
            )
        )

acceptMetadataConstructorBinding ::
  FinalizeContext ->
  LoweredBinding ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError CheckedBinding
acceptMetadataConstructorBinding context lowered term expectedTy =
  acceptResolvedCheckedBinding
    (runtimeTypeCheckEnv context)
    lowered
    (loweredBindingSourceTypeView lowered)
    resolvedDeferredObligations
    resolvedTerm
    expectedTy
  where
    resolvedTerm =
      alignLeadingTypeAbsRefsToType expectedTy
        . TypeCheck.canonicalizeResolvedTermTypes (runtimeTypeCheckEnv context)
        $ term
    resolvedDeferredObligations =
      annotateDeferredEvidenceResolvedVars resolvedTerm (loweredBindingDeferredObligations lowered)

metadataConstructorTerm :: FinalizeContext -> LoweredBinding -> Either ProgramError (XmlfTerm, ElabType)
metadataConstructorTerm context lowered = do
  (term, expectedTy, _) <- metadataConstructorTermWithSupply Nothing context lowered
  Right (term, expectedTy)

metadataConstructorTermFromSupply ::
  IdentityGenerator ->
  FinalizeContext ->
  LoweredBinding ->
  Either ProgramError (XmlfTerm, ElabType, IdentityGenerator)
metadataConstructorTermFromSupply generator context lowered = do
  (term, expectedTy, mbGeneratorAfterTerm) <-
    metadataConstructorTermWithSupply (Just generator) context lowered
  case mbGeneratorAfterTerm of
    Just generatorAfterTerm -> Right (term, expectedTy, generatorAfterTerm)
    Nothing ->
      Left
        ( ProgramPipelineError
            "supplied constructor metadata finalization discarded its identity supply"
        )

metadataConstructorTermWithSupply ::
  Maybe IdentityGenerator ->
  FinalizeContext ->
  LoweredBinding ->
  Either ProgramError (XmlfTerm, ElabType, Maybe IdentityGenerator)
metadataConstructorTermWithSupply mbGenerator context lowered = do
  (dataInfo, ctorInfo) <-
    case lookupConstructorBindingRuntime scope lowered of
      Just found -> Right found
      Nothing -> Left (ProgramPipelineError ("missing constructor metadata for `" ++ loweredBindingName lowered ++ "`"))
  if sameSymbolIdentity (dataInfoSymbol dataInfo) (ctorOwningTypeIdentity ctorInfo)
    then pure ()
    else Left (ProgramPipelineError ("inconsistent constructor metadata for `" ++ loweredBindingName lowered ++ "`"))
  expectedTy <- loweredExpectedTypeToElabType scope lowered
  let constructorHeadIdentities =
        mergeSymbolIdentityMaps
          [ typeViewHeadIdentities (loweredBindingExpectedTypeView lowered),
            typeViewHeadIdentities (loweredBindingSourceTypeView lowered)
          ]
  (term0, mbGeneratorAfterTerm) <-
    inlineConstructorHeadWithSupply
      ConstructorBindingTerm
      mbGenerator
      scope
      constructorHeadIdentities
      (constructorBindingQuantifiedOwnerParams lowered dataInfo)
      ctorInfo
      emptyTypeBinderSubst
  let term = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (schemeFromType expectedTy) term0
  Right (term, expectedTy, mbGeneratorAfterTerm)
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
      any
        (\name -> typeViewBinderIdentityForAlias (loweredBindingExpectedTypeView lowered) name == Just identity)
        quantifiedNames

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
      | groupedOrdinaryRootEligible context lowered =
          let (batch, rest') = span (groupedOrdinaryRootEligible context) lowereds
           in if length batch <= 1
                then (:) <$> finalizeBindingAllowOpaqueWithContext context lowered <*> go rest
                else (++) <$> finalizeBindingGroupWithContext context batch <*> go rest'
      | otherwise =
          (:) <$> finalizeBindingAllowOpaqueWithContext context lowered <*> go rest

finalizeBindingsAllowOpaqueWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> [LoweredBinding] -> Either ProgramError ([CheckedBinding], IdentityGenerator)
finalizeBindingsAllowOpaqueWithContextFromSupply generator0 context =
  go generator0
  where
    go generator [] = Right ([], generator)
    go generator lowereds@(lowered : rest)
      | groupedOrdinaryRootEligible context lowered =
          let (batch, rest') = span (groupedOrdinaryRootEligible context) lowereds
           in if length batch <= 1
                then do
                  (checked, generator') <- finalizeBindingAllowOpaqueWithContextFromSupply generator context lowered
                  (checkedRest, generator'') <- go generator' rest
                  pure (checked : checkedRest, generator'')
                else do
                  (checkedBatch, generator') <- finalizeBindingGroupWithContextFromSupply generator context batch
                  (checkedRest, generator'') <- go generator' rest'
                  pure (checkedBatch ++ checkedRest, generator'')
      | otherwise = do
          (checked, generator') <- finalizeBindingAllowOpaqueWithContextFromSupply generator context lowered
          (checkedRest, generator'') <- go generator' rest
          pure (checked : checkedRest, generator'')

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
      | groupedOrdinaryRootEligible context lowered = do
          let (batch, rest') = span (groupedOrdinaryRootEligible context) bindings
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

finalizeBindingsAllowOpaqueWithContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  FinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeBindingsAllowOpaqueWithContextWithTimingFromSupply timing label generator0 context lowereds =
  timeProgramOperationIO timing label (runExceptT (go generator0 (1 :: Int) lowereds))
  where
    go generator _ [] = pure ([], generator)
    go generator groupIndex bindings@(lowered : rest)
      | groupedOrdinaryRootEligible context lowered = do
          let (batch, rest') = span (groupedOrdinaryRootEligible context) bindings
          (checkedBatch, generator') <-
            if length batch <= 1
              then do
                (checked, generator1) <-
                  ExceptT $
                    finalizeBindingAllowOpaqueWithContextWithTimingFromSupply
                      timing
                      (label ++ ".binding_" ++ show groupIndex)
                      generator
                      context
                      lowered
                pure ([checked], generator1)
              else
                ExceptT $
                  finalizeBindingGroupWithContextWithTimingFromSupply
                    timing
                    (label ++ ".group_" ++ show groupIndex)
                    generator
                    context
                    batch
          (checkedRest, generator'') <- go generator' (groupIndex + 1) rest'
          pure (checkedBatch ++ checkedRest, generator'')
      | otherwise = do
          (checked, generator') <-
            ExceptT $
              finalizeBindingAllowOpaqueWithContextWithTimingFromSupply
                timing
                (label ++ ".binding_" ++ show groupIndex)
                generator
                context
                lowered
          (checkedRest, generator'') <- go generator' groupIndex rest
          pure (checked : checkedRest, generator'')

-- A grouped expression has one ordinary root.  Only a direct polymorphic
-- alias can safely share that root: every other grouped RHS is wrapped in
-- compiler-owned exact authority, whose construction Gamma belongs to that
-- exact producer rather than to the enclosing group.  Select this ownership
-- before constraint construction instead of discovering the mismatch after a
-- grouped pipeline has already installed another producer's binders.
groupedOrdinaryRootEligible :: FinalizeContext -> LoweredBinding -> Bool
groupedOrdinaryRootEligible context lowered =
  not (Builtins.srcTypeMentionsOpaqueBuiltin (loweredBindingSourceType lowered))
    && not (loweredBindingIsConstructor lowered)
    && directPolymorphicAlias context lowered

finalizeBindingGroupWithContext :: FinalizeContext -> [LoweredBinding] -> Either ProgramError [CheckedBinding]
finalizeBindingGroupWithContext _ [] = Right []
finalizeBindingGroupWithContext context lowereds0 = do
  validateLoweredBindingsDeferredObligations lowereds0
  let lowereds =
        zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
      deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
      groupExpr = groupedBindingExpr context lowereds
  pipelineResult <-
    runSurfacePipelineWithContext context lowereds False deferredObligations externalTypeViews0 groupExpr
  let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  (term, _actualTy) <-
    finalizeDeferredObligationsForGroup context deferredObligations tcEnv term0 actualTy0
  case extractGroupedBindings lowereds term of
    Left _ ->
      traverse (finalizeBindingAllowOpaqueWithContext context) lowereds0
    Right extracted ->
      zipWithM
        (\lowered (scheme, rhs) ->
           finalizeCheckedBindingFromTerm context lowered rhs (schemeToType scheme))
        lowereds
        extracted

finalizeBindingGroupWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> [LoweredBinding] -> Either ProgramError ([CheckedBinding], IdentityGenerator)
finalizeBindingGroupWithContextFromSupply generator _ [] = Right ([], generator)
finalizeBindingGroupWithContextFromSupply generator context lowereds0 = do
  validateLoweredBindingsDeferredObligations lowereds0
  let lowereds =
        zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
      deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
      externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
      groupExpr = groupedBindingExpr context lowereds
  pipelineResult <-
    runSurfacePipelineWithContextFromSupply
      generator
      context
      lowereds
      False
      deferredObligations
      externalTypeViews0
      groupExpr
  let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
        pipelineResult
  (term, _actualTy, deferredGenerator) <-
    finalizeDeferredObligationsForGroupFromSupply
      (pedIdentityGenerator pipelineResult)
      context
      deferredObligations
      tcEnv
      term0
      actualTy0
  case extractGroupedBindings lowereds term of
    Left _ ->
      finalizeBindingsIndividuallyWithContextFromSupply generator context lowereds0
    Right extracted -> do
      checked <-
        zipWithM
          (\lowered (scheme, rhs) ->
             finalizeCheckedBindingFromTerm context lowered rhs (schemeToType scheme))
          lowereds
          extracted
      pure
        ( checked,
          checkedBindingsIdentityGenerator deferredGenerator checked
        )

finalizeBindingsIndividuallyWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> [LoweredBinding] -> Either ProgramError ([CheckedBinding], IdentityGenerator)
finalizeBindingsIndividuallyWithContextFromSupply generator0 context =
  go generator0 []
  where
    go generator acc [] = Right (reverse acc, generator)
    go generator acc (lowered : rest) = do
      (checked, generator') <- finalizeBindingAllowOpaqueWithContextFromSupply generator context lowered
      go generator' (checked : acc) rest

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
        groupExpr = groupedBindingExpr context lowereds
    pipelineResult <-
      timeFinalizeEither timing (label ++ ".pipeline") $
        runSurfacePipelineWithContextWithTiming timing (label ++ ".pipeline") context lowereds False deferredObligations externalTypeViews0 groupExpr
    let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
          pipelineResult
    (term, _actualTy) <-
      evaluateFinalizeEither timing (label ++ ".deferred_obligations") $
        finalizeDeferredObligationsForGroup context deferredObligations tcEnv term0 actualTy0
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

finalizeBindingGroupWithContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  FinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeBindingGroupWithContextWithTimingFromSupply _ _ generator _ [] =
  pure (Right ([], generator))
finalizeBindingGroupWithContextWithTimingFromSupply timing label generator context lowereds0 =
  runExceptT $ do
    fromProgramEither (validateLoweredBindingsDeferredObligations lowereds0)
    let lowereds =
          zipWith renameDeferredPlaceholdersForGroup [(1 :: Int) ..] lowereds0
        deferredObligations = Map.unions (map loweredBindingDeferredObligations lowereds)
        externalTypeViews0 = Map.unions (map loweredBindingExternalTypeViews lowereds)
        groupExpr = groupedBindingExpr context lowereds
    pipelineResult <-
      timeFinalizeEither timing (label ++ ".pipeline") $
        runSurfacePipelineWithContextWithTimingFromSupply
          timing
          (label ++ ".pipeline")
          generator
          context
          lowereds
          False
          deferredObligations
          externalTypeViews0
          groupExpr
    let PipelineElabDetailedResult {pedTerm = term0, pedType = actualTy0, pedTypeCheckEnv = tcEnv} =
          pipelineResult
    (term, _actualTy, deferredGenerator) <-
      evaluateFinalizeEither timing (label ++ ".deferred_obligations") $
        finalizeDeferredObligationsForGroupFromSupply
          (pedIdentityGenerator pipelineResult)
          context
          deferredObligations
          tcEnv
          term0
          actualTy0
    extractedResult <-
      liftIO $
        timeProgramOperationIO timing (label ++ ".extract_bindings") $
          evaluate (extractGroupedBindings lowereds term)
    case extractedResult of
      Left _ ->
        ExceptT $
          finalizeBindingsIndividuallyWithContextWithTimingFromSupply
            timing
            (label ++ ".fallback_individual")
            generator
            context
            lowereds0
      Right extracted -> do
        checked <- ExceptT (finalizeExtractedBindingsWithTiming lowereds (1 :: Int) extracted)
        pure
          ( checked,
            checkedBindingsIdentityGenerator deferredGenerator checked
          )
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

finalizeBindingsIndividuallyWithContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  FinalizeContext ->
  [LoweredBinding] ->
  IO (Either ProgramError ([CheckedBinding], IdentityGenerator))
finalizeBindingsIndividuallyWithContextWithTimingFromSupply timing label generator0 context lowereds =
  runExceptT (go generator0 (1 :: Int) [] lowereds)
  where
    go generator _ acc [] = pure (reverse acc, generator)
    go generator index acc (lowered : rest) = do
      (checked, generator') <-
        ExceptT $
          finalizeBindingAllowOpaqueWithContextWithTimingFromSupply
            timing
            (label ++ ".binding_" ++ show index)
            generator
            context
            lowered
      go generator' (index + 1) (checked : acc) rest

groupedBindingExpr :: FinalizeContext -> [LoweredBinding] -> ResolvedSurfaceExpr
groupedBindingExpr context =
  foldr
    ( \lowered body ->
        EResolvedLet
          (loweredIdentityDetails (loweredBindingIdentity lowered))
          (loweredBindingName lowered)
          (groupedBindingRhs lowered)
          body
    )
    (ELit (LBool True))
  where
    groupedBindingRhs lowered
      | directPolymorphicAlias context lowered = loweredBindingSurfaceExpr lowered
      | otherwise = exactBindingSurfaceExpr lowered

-- | Give the ordinary single-binding pipeline the same construction authority
-- as the grouped pipeline.  The declaration's checked producer type must
-- shape constraint generation and subterm generalization, rather than being
-- consulted only after an independently inferred term fails type checking.
checkedBindingSurfaceExpr :: FinalizeContext -> LoweredBinding -> ResolvedSurfaceExpr
checkedBindingSurfaceExpr context lowered
  | directPolymorphicAlias context lowered = loweredBindingSurfaceExpr lowered
  | otherwise = exactBindingSurfaceExpr lowered

exactBindingSurfaceExpr :: LoweredBinding -> ResolvedSurfaceExpr
exactBindingSurfaceExpr lowered =
  EExactAnn
    (loweredBindingSurfaceExpr lowered)
    (groupedBindingAnnotationType lowered)
    (typeViewToResolved (loweredBindingExpectedTypeView lowered))

-- Var-Let already represents a direct polymorphic alias by reusing the
-- producer scheme.  Wrapping that no-op alias in compiler exact authority
-- hides the bare variable from ConstraintGen's Var-Let construction and
-- instead builds an edge between the producer's external gen and a sibling
-- exact target.  Skip the wrapper only when the authoritative producer and
-- expected schemes are semantically the same; genuine specialization must
-- still go through EExactAnn.
directPolymorphicAlias :: FinalizeContext -> LoweredBinding -> Bool
directPolymorphicAlias context lowered =
  case bareDirectSurfaceValueReference (loweredBindingSurfaceExpr lowered) of
    Nothing -> False
    Just reference ->
      case deferredReferenceIsScheme reference of
        Just isScheme -> isScheme
        Nothing ->
          maybe
            False
            sourceSchemeMatches
            ( lookupUniqueAliasValue
                (loweredBindingExternalTypeViews lowered)
                (termReferenceName reference)
                (resolvedTermReferenceDetails reference)
            )
  where
    scope = finalizeContextScope context

    sourceSchemeMatches sourceView =
      externalBindingModeForSourceType sourceTy == ExternalBindingScheme
        && ( alphaEqTypesInScope scope sourceTy expectedTy
               || alphaEqTypesInScope
                    scope
                    (lowerType scope sourceTy)
                    (lowerType scope expectedTy)
           )
      where
        sourceTy = typeViewIdentity sourceView
        expectedTy = typeViewIdentity (loweredBindingExpectedTypeView lowered)

    -- A deferred constructor's mode is fixed when its source occurrence is
    -- lowered.  Do not rediscover scheme-ness from the placeholder SrcType:
    -- a monomorphic specialization may still carry a forall-shaped template,
    -- while a whole-scheme constructor alias is authoritative even when later
    -- lowering changes its display shape.
    deferredReferenceIsScheme reference =
      case resolvedTermReferenceDetails reference of
        DeferredId ref ->
          Just $
            case Map.lookup ref (loweredBindingDeferredObligations lowered) of
              Just (DeferredConstructor deferred) ->
                deferredConstructorBindingMode deferred == DeferredBindingScheme
              _ -> False
        _ -> Nothing

bareDirectSurfaceValueReference :: ResolvedSurfaceExpr -> Maybe (TermReference 'ResolvedTermReferences)
bareDirectSurfaceValueReference expr =
  case expr of
    EVarNode reference -> Just reference
    _ -> Nothing

groupedBindingAnnotationType :: LoweredBinding -> SrcType
groupedBindingAnnotationType =
  typeViewIdentity . loweredBindingExpectedTypeView

{- Note [Compiler-owned exact annotations for grouped bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The grouped finalizer already owns every binding's authoritative producer
scheme in `loweredBindingExpectedTypeView`. Its wrapper is therefore not user
surface syntax `(e : sigma)` and must not be lowered through the thesis' kappa
coercion, whose flexible codomain intentionally permits subsumption.

`EExactAnn` carries the producer scheme through normalization, desugaring, and
constraint generation. Phase 6 then constructs the corresponding xMLF binder
spine from that scheme. This is the annotation analogue of `EExactLamNode`:
compiler metadata is construction authority, while user `EAnn` remains kappa.

A bare direct reference to an already-polymorphic producer is the Var-Let case,
not a new producer check. When its source and expected schemes are semantically
equal, `groupedBindingExpr` leaves it bare so constraint generation can reuse
the source scheme. A specialization or any non-variable RHS retains exact
authority.
-}

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

renameSurfaceVars :: (String -> String) -> ResolvedSurfaceExpr -> ResolvedSurfaceExpr
renameSurfaceVars renameName =
  go
  where
    go expr =
      case expr of
        EVarNode (ResolvedTermReference (DeferredId ref) _) ->
          let renamedRef = renameDeferredRef (renameName (deferredRefName ref)) ref
           in EResolvedVar (DeferredId renamedRef) (deferredRefName renamedRef)
        EVarNode {} -> expr
        ELit {} -> expr
        ELamNode reference body ->
          ELamNode reference (go body)
        EApp fun arg -> EApp (go fun) (go arg)
        ELetNode reference rhs body ->
          ELetNode reference (go rhs) (go body)
        ELamAnnNode reference ty body ->
          ELamAnnNode reference ty (go body)
        EExactLamNode reference ty body ->
          EExactLamNode reference ty (go body)
        EAnn inner ty -> EAnn (go inner) ty
        EExactAnn inner ty exactTy -> EExactAnn (go inner) ty exactTy

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

-- Deferred constructor selection can make a provisional root forall
-- redundant after the selected constructor has instantiated the same binder
-- inside its structural result type.  Collapse only that impossible duplicate
-- declaration: ordinary vacuous foralls, including bounded producer slots,
-- remain explicit.
collapseDuplicateLeadingForallConstruction :: ElabType -> XmlfTerm -> (ElabType, XmlfTerm)
collapseDuplicateLeadingForallConstruction ty term =
  case (ty, term) of
    (X.TForallRef typeRef mbTypeBound bodyTy, X.ETyAbsRef termRef mbTermBound bodyTerm)
      | X.typeBinderRefsSameIdentity typeRef termRef,
        mbTypeBound == mbTermBound,
        any (X.typeBinderRefsSameIdentity typeRef) (typeBinderDeclarationRefs bodyTy) ->
          collapseDuplicateLeadingForallConstruction bodyTy bodyTerm
      | X.typeBinderRefsSameIdentity typeRef termRef ->
          let (bodyTy', bodyTerm') =
                collapseDuplicateLeadingForallConstruction bodyTy bodyTerm
           in ( X.TForallRef typeRef mbTypeBound bodyTy',
                X.ETyAbsRef termRef mbTermBound bodyTerm'
              )
    _ -> (ty, term)

acceptResolvedCheckedBinding :: Env -> LoweredBinding -> TypeView -> DeferredObligations -> XmlfTerm -> ElabType -> Either ProgramError CheckedBinding
acceptResolvedCheckedBinding typeCheckEnv lowered sourceTypeView resolvedDeferredObligations resolvedTerm checkedTy =
  case unresolvedXmlfTermVarRefs resolvedTerm of
    [] -> do
      validateDeferredObligationIdentities (loweredBindingIdentity lowered) resolvedDeferredObligations
      case TypeCheck.typeCheckWithEnv typeCheckEnv resolvedTerm of
        Left err ->
          Left
            ( ProgramPipelineError
                ( "checked XmlfTerm failed final typecheck in `"
                    ++ loweredBindingName lowered
                    ++ "`: "
                    ++ show err
                )
            )
        Right inferredTy
          | alphaEqType inferredTy checkedTy -> pure ()
          | otherwise ->
              Left
                ( ProgramPipelineError
                    ( "checked XmlfTerm inferred type differs from stored type in `"
                        ++ loweredBindingName lowered
                        ++ "`: inferred "
                        ++ show inferredTy
                        ++ ", stored "
                        ++ show checkedTy
                    )
                )
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
  let (acceptedTy, acceptedTerm) =
        collapseDuplicateLeadingForallConstruction actualTy term
      acceptedTermTyResult = TypeCheck.typeCheckWithEnv (runtimeTypeCheckEnv context) acceptedTerm
  let acceptChecked = do
        checkedTy <- checkedBindingTypeForStorage lowered acceptedTy
        acceptedTermForStorage <-
          if checkedResultTypeSpecializes checkedTy acceptedTy
            then
              case
                  constructExactTermAtType
                    (runtimeTypeCheckEnv context)
                    acceptedTy
                    checkedTy
                    acceptedTerm
                of
                  Right specialized -> Right specialized
                  Left err ->
                    Left
                      ( ProgramPipelineError
                          ( "checked binding `"
                              ++ loweredBindingName lowered
                              ++ "` could not specialize its principal construction to the declared type: "
                              ++ show err
                          )
                      )
            else Right acceptedTerm
        let sourceTypeView =
              loweredBindingSourceTypeView lowered
        let closedAcceptedTerm =
              -- A top-level binding body is elaborated with its declared
              -- type binders in scope.  Discharge that scope into explicit
              -- xMLF type abstractions before constructing CheckedBinding;
              -- TAbs must enclose any evidence lambdas that mention those
              -- binders (thesis Fig. 15.3.4 and the xMLF TAbs rule).
              closeTermWithSchemeSubstRefsIfNeeded
                IntMap.empty
                (schemeFromType checkedTy)
                acceptedTermForStorage
            acceptedTermWithResolvedVars =
              alignLeadingTypeAbsRefsToType checkedTy
                . normalizeCheckedTypeRedexes
                . alignLeadingTypeAbsRefsToType checkedTy
                . TypeCheck.canonicalizeResolvedTermTypes (runtimeTypeCheckEnv context)
                $ closedAcceptedTerm
        -- Deferred rewriting can leave the completed producer more general
        -- than the pipeline's projected result type (for example, a vacuous
        -- local-Gamma forall around a case computation).  The checked term is
        -- the construction authority here: specialize its inferred type to
        -- the stored contract explicitly, before CheckedBinding is created.
        checkedTerm <-
          case TypeCheck.typeCheckWithEnv (runtimeTypeCheckEnv context) acceptedTermWithResolvedVars of
            Right inferredTy
              | alphaEqType inferredTy checkedTy ->
                  Right acceptedTermWithResolvedVars
              | otherwise ->
                  case
                      constructExactTermAtType
                        (runtimeTypeCheckEnv context)
                        inferredTy
                        checkedTy
                        acceptedTermWithResolvedVars
                    of
                      Right specialized -> Right specialized
                      Left err ->
                        Left
                          ( ProgramPipelineError
                              ( "checked binding `"
                                  ++ loweredBindingName lowered
                                  ++ "` could not construct its inferred principal term at the stored type: "
                                  ++ show err
                              )
                          )
            Left _ -> Right acceptedTermWithResolvedVars
        let resolvedDeferredObligations =
              annotateDeferredEvidenceResolvedVars checkedTerm (loweredBindingDeferredObligations lowered)
        acceptResolvedCheckedBinding
          (runtimeTypeCheckEnv context)
          lowered
          sourceTypeView
          resolvedDeferredObligations
          checkedTerm
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
        || checkedResultTypeSpecializes expectedTy candidateTy
        || implicitForallClosureMatches expectedTy candidateTy

    -- Generalization can construct a bounded result scheme explicitly, for
    -- example @Nat -> forall beta >= Bool. beta@.  A binding annotation may
    -- select the lower-bound instance (@Nat -> Bool@); preserve that declared
    -- type while the checked term carries the corresponding explicit coercion.
    -- Only result positions are traversed: specializing a function parameter
    -- would change the function's calling contract.
    checkedResultTypeSpecializes expectedTy candidateTy =
      case (expectedTy, candidateTy) of
        ( X.TForallRef expectedRef expectedBound expectedBody,
          X.TForallRef candidateRef candidateBound candidateBody
          )
            | forallBoundsAgree expectedBound candidateBound ->
                checkedResultTypeSpecializes
                  expectedBody
                  (substTypeCaptureRef candidateRef (X.TVarRef expectedRef) candidateBody)
        (X.TArrow expectedDom expectedCod, X.TArrow candidateDom candidateCod) ->
          checkedTypesCompatible expectedDom candidateDom
            && checkedResultTypeSpecializes expectedCod candidateCod
        (_, X.TForallRef candidateRef Nothing candidateBody)
          | not
              ( any
                  (X.typeBinderRefsSameIdentity candidateRef)
                  (freeTypeVarRefsType candidateBody)
              ) ->
              -- An unbounded but vacuous result binder is still an explicit
              -- xMLF boundary.  The declared monomorphic result selects its
              -- body through InstElim; record that construction relation here
              -- so storage retains the declared contract.
              checkedTypesCompatible expectedTy candidateBody
        (_, X.TForallRef ref (Just bound) body) ->
          case matchTypeRefs [ref] body expectedTy of
            Right substitution ->
              case Map.lookup ref substitution of
                Just instanceTy ->
                  alphaEqType (X.tyToElab bound) instanceTy
                    || churchAwareEqType (X.tyToElab bound) instanceTy
                Nothing -> False
            Left _ -> False
        _ -> False
      where
        forallBoundsAgree left right =
          case (left, right) of
            (Nothing, Nothing) -> True
            (Just leftBound, Just rightBound) ->
              alphaEqType (X.tyToElab leftBound) (X.tyToElab rightBound)
                || churchAwareEqType (X.tyToElab leftBound) (X.tyToElab rightBound)
            _ -> False

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
        if keepExpected && not (implicitForallClosureMatches expectedTy acceptedTy0)
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
               in alphaEqTypesInScope scope sourceTy targetTy
                    || alphaEqTypesInScope scope sourceTy' targetTy'
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

    directSurfaceValueReference :: ResolvedSurfaceExpr -> Maybe SurfaceBindingReference
    directSurfaceValueReference expr =
      case expr of
        EVarNode reference -> Just (surfaceBindingReferenceFromTermReference reference)
        EAnn inner _ -> directSurfaceValueReference inner
        EExactAnn inner _ _ -> directSurfaceValueReference inner
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

validateDeferredObligationIdentities :: LoweredBindingIdentity -> DeferredObligations -> Either ProgramError ()
validateDeferredObligationIdentities bindingIdentity obligations =
  mapM_ validateEntry (Map.toList obligations)
  where
    validateEntry (expectedRef, obligation)
      | expectedRef /= actualRef =
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
      | DeferredCase deferred <- obligation
      , deferredCaseBindingIdentity deferred /= bindingIdentity =
          Left
            ( ProgramPipelineError
                ( "checked binding `"
                    ++ bindingName
                    ++ "` has deferred case owned by binding `"
                    ++ loweredIdentityRuntimeName (deferredCaseBindingIdentity deferred)
                    ++ "`"
                )
            )
      | otherwise =
          Right ()
      where
        actualRef = deferredProgramObligationRef obligation

    bindingName = loweredIdentityRuntimeName bindingIdentity

    deferredRefLabel ref =
      deferredRefName ref ++ "#" ++ uniqueIdentityStableName (deferredRefIdentity ref)

validateLoweredBindingDeferredObligations :: LoweredBinding -> Either ProgramError ()
validateLoweredBindingDeferredObligations lowered =
  validateDeferredObligationIdentities (loweredBindingIdentity lowered) (loweredBindingDeferredObligations lowered)

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
  fmap annotateObligation
    (projectDeferredConstructorConstructionRoutes term obligations)
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
      case find (X.resolvedVarSameIdentity existing) evidenceResolvedVars of
        Just resolved ->
          method {evidenceMethodResolvedVar = mergeEvidenceResolvedVar existing resolved}
        Nothing -> method
      where
        existing = evidenceMethodResolvedVar method

    mergeEvidenceResolvedVar existing resolved
      | X.resolvedVarSameIdentity existing resolved = resolved
      | otherwise = existing

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
    ++ typeViewGeneratedIdentities (loweredBindingSourceTypeView lowered)
    ++ typeViewGeneratedIdentities (loweredBindingExpectedTypeView lowered)
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
          let details = valueInfoRuntimeDetails valueInfo,
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
    ResolvedBindingKey key -> do
      identity <- externalBindingIdentityByKey runtimeIndex deferredIndex key
      let details = externalBindingDetails identity
      lookupUniqueAliasValue values (idDetailsRuntimeName details) details

surfaceBindingReferenceIdentity :: RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> SurfaceBindingReference -> Maybe ExternalBindingIdentity
surfaceBindingReferenceIdentity runtimeIndex deferredIndex reference =
  case surfaceBindingReferenceKey reference of
    ResolvedBindingKey key ->
      externalBindingIdentityByKey runtimeIndex deferredIndex key

surfaceBindingReferenceSourceType :: FinalizeContext -> Map String SrcType -> DeferredExternalBindingIndex -> SurfaceBindingReference -> Maybe SrcType
surfaceBindingReferenceSourceType context externalTypes deferredIndex reference =
  case surfaceBindingReferenceKey reference of
    ResolvedBindingKey key ->
      runtimeExternalBindingSourceTypeByKey scope runtimeTypes runtimeIndex key
        <|> deferredExternalBindingSourceTypeByKey externalTypes deferredIndex key
  where
    scope = finalizeContextScope context
    runtimeTypes = finalizeContextRuntimeSourceTypes context
    runtimeIndex = finalizeContextRuntimeBindingIndex context

surfaceBindingReferenceMode :: ElaborateScope -> Map String SrcType -> RuntimeExternalBindingIndex -> DeferredExternalBindingIndex -> SurfaceBindingReference -> ExternalBindingMode
surfaceBindingReferenceMode scope runtimeTypes runtimeIndex deferredIndex reference =
  case surfaceBindingReferenceKey reference of
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
                    idDetailsRuntimeName (externalBindingDetails identity),
                  surfaceExternalBindingInputView = view,
                  surfaceExternalBindingInputMode =
                    surfaceBindingReferenceMode
                      scope
                      runtimeTypes
                      runtimeIndex
                      deferredIndex
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

prepareSurfacePipelineExternalBindings :: FinalizeContext -> DeferredObligations -> Map String TypeView -> ResolvedSurfaceExpr -> Either ProgramError PreparedExternalBindings
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

-- | A compiler-owned exact wrapper may publish a checked producer scheme, but
-- it must not first resolve a raw source annotation by choosing one of two
-- same-spelled free binders.  Reject that ambiguity before constraint
-- construction; otherwise the exact wrapper can accidentally turn a distinct
-- ambient binder into the declaration's binder and the public type mismatch is
-- lost behind an elaboration invariant failure.
validateExactSurfaceBinderAuthority ::
  PreparedExternalBindings ->
  LoweredBinding ->
  Either ProgramError ()
validateExactSurfaceBinderAuthority prepared lowered
  | null conflictingAliases = Right ()
  | otherwise =
      Left
        ( ProgramTypeMismatch
            (typeViewIdentity sourceView)
            (typeViewDisplay expectedView)
        )
  where
    sourceView = loweredBindingSourceTypeView lowered
    expectedView = loweredBindingExpectedTypeView lowered
    annotationAliases = surfaceAnnotationFreeTypeBinderAliases (loweredBindingSurfaceExpr lowered)
    inheritedCandidates = preparedSourceTypeBinderIdentityCandidates prepared
    rootCandidates =
      Map.fromListWith
        Set.union
        [ (alias, Set.singleton identity)
        | identities <-
            [ typeViewBinderIdentities sourceView,
              typeViewBinderIdentities expectedView
            ],
          (alias, identity) <- Map.toList identities
        ]
    conflictingAliases =
      [ alias
      | alias <- Set.toList annotationAliases,
        Just ownedIdentities <- [Map.lookup alias rootCandidates],
        let inheritedIdentities = Map.findWithDefault Set.empty alias inheritedCandidates,
        Set.size ownedIdentities /= 1
          || not (inheritedIdentities `Set.isSubsetOf` ownedIdentities)
      ]

surfaceAnnotationFreeTypeBinderAliases :: ResolvedSurfaceExpr -> Set String
surfaceAnnotationFreeTypeBinderAliases expr =
  case expr of
    EVarNode {} -> Set.empty
    ELit {} -> Set.empty
    ELamNode _ body -> surfaceAnnotationFreeTypeBinderAliases body
    ELamAnnNode _ ty body ->
      freeSrcTypeVars ty `Set.union` surfaceAnnotationFreeTypeBinderAliases body
    EExactLamNode _ ty body ->
      freeSrcTypeVars ty `Set.union` surfaceAnnotationFreeTypeBinderAliases body
    EApp fun arg ->
      surfaceAnnotationFreeTypeBinderAliases fun
        `Set.union` surfaceAnnotationFreeTypeBinderAliases arg
    ELetNode _ rhs body ->
      surfaceAnnotationFreeTypeBinderAliases rhs
        `Set.union` surfaceAnnotationFreeTypeBinderAliases body
    EAnn inner ty ->
      freeSrcTypeVars ty `Set.union` surfaceAnnotationFreeTypeBinderAliases inner
    EExactAnn inner ty _ ->
      freeSrcTypeVars ty `Set.union` surfaceAnnotationFreeTypeBinderAliases inner

runSurfacePipelineWithContext :: FinalizeContext -> [LoweredBinding] -> Bool -> DeferredObligations -> Map String TypeView -> ResolvedSurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipelineWithContext context lowereds forceUnchecked deferredObligations externalTypeViews0 surfaceExpr = do
  extEnv0 <- prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews0 surfaceExpr
  mapM_ (validateExactSurfaceBinderAuthority extEnv0) lowereds
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  let extEnv = preferPreparedWithLoweredTypeIdentities lowereds extEnv0
      runPipeline =
        if not forceUnchecked && Map.null deferredObligations
          then runPipelineElabDetailedResolvedWithPreparedExternalBindings
          else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindings
  either
    (Left . programErrorFromPipelineFailure context lowereds)
    Right
    (runPipeline Set.empty extEnv normExpr)

runSurfacePipelineWithContextFromSupply :: IdentityGenerator -> FinalizeContext -> [LoweredBinding] -> Bool -> DeferredObligations -> Map String TypeView -> ResolvedSurfaceExpr -> Either ProgramError PipelineElabDetailedResult
runSurfacePipelineWithContextFromSupply generator context lowereds forceUnchecked deferredObligations externalTypeViews0 surfaceExpr = do
  extEnv0 <- prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews0 surfaceExpr
  mapM_ (validateExactSurfaceBinderAuthority extEnv0) lowereds
  normExpr <- either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
  let extEnv = preferPreparedWithLoweredTypeIdentities lowereds extEnv0
      runPipeline =
        if not forceUnchecked && Map.null deferredObligations
          then runPipelineElabDetailedResolvedWithPreparedExternalBindingsFromSupply
          else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsFromSupply
  either
    (Left . programErrorFromPipelineFailure context lowereds)
    Right
    (runPipeline generator Set.empty extEnv normExpr)

runSurfacePipelineWithContextWithTiming ::
  TimingConfig ->
  String ->
  FinalizeContext ->
  [LoweredBinding] ->
  Bool ->
  DeferredObligations ->
  Map String TypeView ->
  ResolvedSurfaceExpr ->
  IO (Either ProgramError PipelineElabDetailedResult)
runSurfacePipelineWithContextWithTiming timing label context lowereds forceUnchecked deferredObligations externalTypeViews0 surfaceExpr =
  runExceptT $ do
    extEnv0 <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $
        prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews0 surfaceExpr
    fromProgramEither (mapM_ (validateExactSurfaceBinderAuthority extEnv0) lowereds)
    normExpr <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
    let extEnv = preferPreparedWithLoweredTypeIdentities lowereds extEnv0
        runPipeline =
          if not forceUnchecked && Map.null deferredObligations
            then runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTiming
            else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTiming
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") Set.empty extEnv normExpr
    fromProgramEither $
      either
        (Left . programErrorFromPipelineFailure context lowereds)
        Right
        pipelineResult

runSurfacePipelineWithContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  FinalizeContext ->
  [LoweredBinding] ->
  Bool ->
  DeferredObligations ->
  Map String TypeView ->
  ResolvedSurfaceExpr ->
  IO (Either ProgramError PipelineElabDetailedResult)
runSurfacePipelineWithContextWithTimingFromSupply timing label generator context lowereds forceUnchecked deferredObligations externalTypeViews0 surfaceExpr =
  runExceptT $ do
    extEnv0 <-
      evaluateFinalizeEither timing (label ++ ".prepare_external_bindings") $
        prepareSurfacePipelineExternalBindings context deferredObligations externalTypeViews0 surfaceExpr
    fromProgramEither (mapM_ (validateExactSurfaceBinderAuthority extEnv0) lowereds)
    normExpr <-
      evaluateFinalizeEither timing (label ++ ".normalize_surface") $
        either (Left . ProgramPipelineError . show) Right (normalizeExpr surfaceExpr)
    let extEnv = preferPreparedWithLoweredTypeIdentities lowereds extEnv0
        runPipeline =
          if not forceUnchecked && Map.null deferredObligations
            then runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTimingFromSupply
            else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTimingFromSupply
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") generator Set.empty extEnv normExpr
    fromProgramEither $
      either
        (Left . programErrorFromPipelineFailure context lowereds)
        Right
        pipelineResult

-- | Project a typed pipeline failure onto the Program boundary.  Compiler
-- exact annotations for opaque result types can fail during Phase 6 before
-- the ordinary checked-type comparison runs.  Prefer an independent surface
-- mismatch when one is available.  Otherwise a bare lambda still proves an
-- arrow outer shape, which is enough to reject a non-arrow annotation without
-- inventing its parameter/result types.  Keep the pipeline failure as the
-- structured cause rather than exposing a Phi/construction invariant as the
-- public classification.
programErrorFromPipelineFailure :: FinalizeContext -> [LoweredBinding] -> PipelineError -> ProgramError
programErrorFromPipelineFailure context lowereds pipelineError =
  case (pipelineError, lowereds) of
    (PipelineElabError {}, [lowered])
      | let expected = loweredBindingExpectedType lowered,
        Builtins.srcTypeMentionsOpaqueBuiltin expected ->
          classifyLoweredOpaqueFailure lowered expected
    _ -> rawPipelineError
  where
    rawPipelineError = ProgramPipelineError (renderPipelineError pipelineError)

    classifyLoweredOpaqueFailure lowered expected =
      case validateOpaqueBindingSurface context lowered of
        Left (ProgramTypeMismatch actual mismatchExpected) ->
          ProgramTypeMismatchWithCause actual mismatchExpected rawPipelineError
        _
          | Just shape <- opaqueSurfaceTypeShape (loweredBindingSurfaceExpr lowered),
            not (sourceTypeAcceptsShape shape expected) ->
              ProgramTypeShapeMismatchWithCause shape expected rawPipelineError
          | otherwise -> rawPipelineError

extendPreparedWithLoweredTypeIdentities :: [LoweredBinding] -> PreparedExternalBindings -> PreparedExternalBindings
extendPreparedWithLoweredTypeIdentities lowereds prepared =
  reservePreparedExternalBindingIdentities generatedIdentities
    ( extendPreparedExternalBindingTypeIdentityCandidates
        (map typeViewHeadIdentities views)
        (map typeViewBinderIdentities views)
        prepared
    )
  where
    views =
      concatMap
        (\lowered -> [loweredBindingExpectedTypeView lowered, loweredBindingSourceTypeView lowered])
        lowereds
    generatedIdentities =
      concatMap generatedIdentitiesInLoweredBinding lowereds

preferPreparedWithLoweredTypeIdentities :: [LoweredBinding] -> PreparedExternalBindings -> PreparedExternalBindings
preferPreparedWithLoweredTypeIdentities lowereds prepared =
  reservePreparedExternalBindingIdentities generatedIdentities
    ( preferPreparedExternalBindingTypeIdentities
        (mergeSymbolIdentityMaps (map typeViewHeadIdentities views))
        (mergeTypeBinderIdentityMaps (map typeViewBinderIdentities views))
        prepared
    )
  where
    views =
      concatMap
        (\lowered -> [loweredBindingExpectedTypeView lowered, loweredBindingSourceTypeView lowered])
        lowereds
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
  let extEnv = preferPreparedWithLoweredTypeIdentities [stampedLowered] extEnv0
      runPipeline =
        if not forceUnchecked && Map.null (loweredBindingDeferredObligations stampedLowered)
          then runPipelineElabDetailedResolvedWithPreparedExternalBindings
          else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindings
      pipelineResult = runPipeline Set.empty extEnv normExpr
  either
    (Left . programErrorFromPipelineFailure context [stampedLowered])
    Right
    pipelineResult
  where
    context = moduleFinalizeContextBase moduleContext

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
    let extEnv = preferPreparedWithLoweredTypeIdentities [stampedLowered] extEnv0
        runPipeline =
          if not forceUnchecked && Map.null (loweredBindingDeferredObligations stampedLowered)
            then runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTiming
            else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTiming
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") Set.empty extEnv normExpr
    fromProgramEither $
      either
        (Left . programErrorFromPipelineFailure context [stampedLowered])
        Right
        pipelineResult
  where
    context = moduleFinalizeContextBase moduleContext

runLoweredSurfacePipelineWithModuleContextWithTimingFromSupply ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  ModuleFinalizeContext ->
  Bool ->
  LoweredBinding ->
  IO (Either ProgramError PipelineElabDetailedResult)
runLoweredSurfacePipelineWithModuleContextWithTimingFromSupply timing label generator moduleContext forceUnchecked lowered =
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
    let extEnv = preferPreparedWithLoweredTypeIdentities [stampedLowered] extEnv0
        runPipeline =
          if not forceUnchecked && Map.null (loweredBindingDeferredObligations stampedLowered)
            then runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTimingFromSupply
            else runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTimingFromSupply
    pipelineResult <-
      liftIO $
        runPipeline timing (label ++ ".elab_pipeline") generator Set.empty extEnv normExpr
    fromProgramEither $
      either
        (Left . programErrorFromPipelineFailure context [stampedLowered])
        Right
        pipelineResult
  where
    context = moduleFinalizeContextBase moduleContext

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
  either
    (Left . ProgramPipelineError . show)
    Right
    (prepareExternalBindingsWithTypeIdentities scopeHeadIdentities Map.empty extBindings)
  where
    scopeHeadIdentities = typeHeadIdentitiesInScope scope

    prepareInput input = do
      let name = surfaceExternalBindingInputName input
          view = surfaceExternalBindingInputView input
      normTy <- either (Left . ProgramPipelineError . show) Right (normalizeType (typeViewDisplay view))
      pure
        ( name,
          ExternalBinding
            { externalBindingType = normTy,
              externalBindingMode = surfaceExternalBindingInputMode input,
              externalBindingIdentity = surfaceExternalBindingInputIdentity input,
              externalBindingTypeHeadIdentities =
                typeViewHeadIdentities view,
              externalBindingTypeBinderIdentities =
                mergeTypeBinderIdentityMaps
                  [ typeViewBinderIdentities view,
                    sourceTypeBinderIdentitiesInScope scope normTy
                  ]
            }
        )

lowerExternalTypeViews :: ElaborateScope -> Map String TypeView -> Map String TypeView
lowerExternalTypeViews = lowerTypeViewsWithIdentities

finalizeDeferredObligationsForBinding ::
  FinalizeContext ->
  LoweredBinding ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError (XmlfTerm, ElabType)
finalizeDeferredObligationsForBinding context lowered deferredObligations tcEnv term inferredTy =
  case finalizeDeferredObligations context resolvedDeferredObligations tcEnv resolvedTerm inferredTy expectedBindingTy of
    Left (ProgramPipelineError msg) ->
      Left (ProgramPipelineError ("binding `" ++ loweredBindingName lowered ++ "`: " ++ msg))
    result -> result
  where
    resolvedTerm = term
    resolvedDeferredObligations =
      annotateDeferredEvidenceResolvedVars resolvedTerm deferredObligations
    expectedBindingTy =
      loweredExpectedTypeToElabType (finalizeContextScope context) lowered

finalizeDeferredObligationsForBindingFromSupply ::
  IdentityGenerator ->
  FinalizeContext ->
  LoweredBinding ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError (XmlfTerm, ElabType, IdentityGenerator)
finalizeDeferredObligationsForBindingFromSupply generator context lowered deferredObligations tcEnv term inferredTy =
  case finalizeDeferredObligationsFromSupply generator context resolvedDeferredObligations tcEnv resolvedTerm inferredTy expectedBindingTy of
    Left (ProgramPipelineError msg) ->
      Left (ProgramPipelineError ("binding `" ++ loweredBindingName lowered ++ "`: " ++ msg))
    result -> result
  where
    resolvedTerm = term
    resolvedDeferredObligations =
      annotateDeferredEvidenceResolvedVars resolvedTerm deferredObligations
    expectedBindingTy =
      loweredExpectedTypeToElabType (finalizeContextScope context) lowered

finalizeDeferredObligationsForGroup ::
  FinalizeContext ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError (XmlfTerm, ElabType)
finalizeDeferredObligationsForGroup context deferredObligations tcEnv term inferredTy =
  finalizeDeferredObligations context resolvedDeferredObligations tcEnv resolvedTerm inferredTy (Right X.TBottom)
  where
    resolvedTerm = term
    resolvedDeferredObligations =
      annotateDeferredEvidenceResolvedVars resolvedTerm deferredObligations

finalizeDeferredObligationsForGroupFromSupply ::
  IdentityGenerator ->
  FinalizeContext ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError (XmlfTerm, ElabType, IdentityGenerator)
finalizeDeferredObligationsForGroupFromSupply generator context deferredObligations tcEnv term inferredTy =
  finalizeDeferredObligationsFromSupply generator context resolvedDeferredObligations tcEnv resolvedTerm inferredTy (Right X.TBottom)
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
  Either ProgramError ElabType ->
  Either ProgramError (XmlfTerm, ElabType)
finalizeDeferredObligations context deferredObligations tcEnv term inferredTy expectedBindingTy = do
  (term', ty, _) <-
    finalizeDeferredObligationsWithSupply
      Nothing
      context
      deferredObligations
      tcEnv
      term
      inferredTy
      expectedBindingTy
  pure (term', ty)

finalizeDeferredObligationsFromSupply ::
  IdentityGenerator ->
  FinalizeContext ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError ElabType ->
  Either ProgramError (XmlfTerm, ElabType, IdentityGenerator)
finalizeDeferredObligationsFromSupply generator context deferredObligations tcEnv term inferredTy expectedBindingTy = do
  (term', ty, mbGenerator) <-
    finalizeDeferredObligationsWithSupply
      (Just generator)
      context
      deferredObligations
      tcEnv
      term
      inferredTy
      expectedBindingTy
  case mbGenerator of
    Just generator' -> Right (term', ty, generator')
    Nothing -> Left (ProgramPipelineError "deferred obligation finalization lost its identity supply")

finalizeDeferredObligationsWithSupply ::
  Maybe IdentityGenerator ->
  FinalizeContext ->
  DeferredObligations ->
  Env ->
  XmlfTerm ->
  ElabType ->
  Either ProgramError ElabType ->
  Either ProgramError (XmlfTerm, ElabType, Maybe IdentityGenerator)
finalizeDeferredObligationsWithSupply mbGenerator _ deferredObligations _ term inferredTy _
  | Map.null deferredObligations = Right (term, inferredTy, mbGenerator)
finalizeDeferredObligationsWithSupply mbGenerator context deferredObligations tcEnv term _ expectedBindingTy = do
  let rewriteEnv = extendTypeCheckEnvWithRuntimeContext context tcEnv
  let constructorObligations = Map.mapMaybe onlyConstructor deferredObligations
      caseObligations = Map.mapMaybe onlyCase deferredObligations
      methodObligations = Map.mapMaybe onlyMethod deferredObligations
  (constructorsRewritten, mbGenerator1) <-
    if Map.null constructorObligations
      then Right (term, mbGenerator)
      else resolveDeferredConstructorsWithSupply mbGenerator scope rewriteEnv constructorObligations term
  (caseRewriteEnv, casesRewritten, mbGenerator2) <-
    if Map.null caseObligations
      then Right (rewriteEnv, constructorsRewritten, mbGenerator1)
      else resolveDeferredCasesWithSupply mbGenerator1 scope caseObligations rewriteEnv constructorsRewritten
  (methodsRewritten, mbGenerator3) <-
    if Map.null methodObligations
      then Right (casesRewritten, mbGenerator2)
      else resolveDeferredMethodsWithSupply mbGenerator2 scope methodObligations caseRewriteEnv casesRewritten
  structurallyTyped <-
    lowerResolvedTermTypesForCheckedIR scope methodsRewritten
  rewritten <-
    if termHasLets structurallyTyped
      then refreshLetSchemes caseRewriteEnv structurallyTyped
      else Right structurallyTyped
  let rewrittenClean = dropStaleTypeInsts caseRewriteEnv rewritten
  -- Deferred rewriters receive the authoritative identity supply and rewrite
  -- environment, so any abstractions they introduce must already be fresh by
  -- construction.  Freshening the completed term again can retarget a
  -- prepared Hyp to a same-named sibling binder after the compiler-exact
  -- boundary has validated it.
  let rewrittenForCheck = rewrittenClean
      mbGenerator' = mbGenerator3
  rewrittenTy <-
    case typeCheckWithEnv caseRewriteEnv rewrittenForCheck of
      Right ty -> Right (inlineTypeEnvBounds caseRewriteEnv ty)
      Left X.TCArgumentMismatch {} ->
        expectedBindingTy
      Left err ->
        Left
          ( ProgramPipelineError
              ( "deferred program obligation rewrite failed type check: "
                  ++ show err
                  ++ "; before stale-inst cleanup: "
                  ++ show (typeCheckWithEnv caseRewriteEnv rewritten)
                  ++ "; before let-scheme refresh: "
                  ++ show (typeCheckWithEnv caseRewriteEnv structurallyTyped)
              )
          )
  Right (rewrittenForCheck, rewrittenTy, mbGenerator')
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

{- Note [Normalize type redexes before checked-IR acceptance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Final closure can place source type abstractions outside vacuous
construction-local abstraction/elimination pairs.  A vacuous pair is a
proof-only redex, not part of the source ABI, so it may be removed before the
checked term is published.

Normalize only explicit type beta-redexes, recursively.  Value-level lets and
applications remain untouched, so CheckedBinding keeps the source evaluation
structure while its type evidence is closed by construction.  This is also an
identity-publication boundary: retain a reduction only when it introduces no
generated identity absent from the original redex.

An explicit @Hyp(alpha)@ under @Lambda(alpha > tau)@ is different: it is the
construction proving that a concrete argument has been raised to the local
application endpoint.  Reducing that pair would erase the exact construction
and publish only its post-substitution normal form.  Preserve such a redex
whole; the binder remains scoped by its own abstraction and backend reduction
may consume it after CheckedBinding has recorded the proof.
-}
normalizeCheckedTypeRedexes :: XmlfTerm -> XmlfTerm
normalizeCheckedTypeRedexes =
  reduceHere . descend
  where
    descend term =
      case term of
        X.EVarNode {} -> term
        X.ELit {} -> term
        X.ELam resolved body ->
          X.ELam resolved (normalizeCheckedTypeRedexes body)
        X.EApp fun arg ->
          X.EApp
            (normalizeCheckedTypeRedexes fun)
            (normalizeCheckedTypeRedexes arg)
        X.ELet resolved scheme rhs body ->
          X.ELet
            resolved
            scheme
            (normalizeCheckedTypeRedexes rhs)
            (normalizeCheckedTypeRedexes body)
        X.ETyAbsRef ref mbBound body ->
          X.ETyAbsRef ref mbBound (normalizeCheckedTypeRedexes body)
        X.ETyInst inner inst ->
          X.ETyInst (normalizeCheckedTypeRedexes inner) inst
        X.ERoll ty body ->
          X.ERoll ty (normalizeCheckedTypeRedexes body)
        X.EUnroll inner ->
          X.EUnroll (normalizeCheckedTypeRedexes inner)

    reduceHere term =
      if checkedTypeRedexCarriesExplicitHyp term
        then term
        else
          case Reduce.reduceLeadingTypeInstantiationRedexes term of
            Just reduced
              | generatedIdentitiesIn reduced
                  `Set.isSubsetOf` generatedIdentitiesIn term ->
                  normalizeCheckedTypeRedexes reduced
            Nothing ->
              term
            _ ->
              term

    generatedIdentitiesIn =
      Set.fromList . X.generatedIdentitiesInTerm

checkedTypeRedexCarriesExplicitHyp :: XmlfTerm -> Bool
checkedTypeRedexCarriesExplicitHyp term =
  case term of
    X.ETyInst (X.ETyAbsRef ref _ body) _ ->
      termContainsExplicitHypFor ref body
    _ -> False

termContainsExplicitHypFor :: X.TypeBinderRef -> XmlfTerm -> Bool
termContainsExplicitHypFor target =
  goTerm
  where
    goTerm term =
      case term of
        X.EVarNode {} -> False
        X.ELit {} -> False
        X.ELam _ body -> goTerm body
        X.EApp fun arg -> goTerm fun || goTerm arg
        X.ELet _ _ rhs body -> goTerm rhs || goTerm body
        X.ETyAbsRef ref _ body
          | X.typeBinderRefsSameIdentity target ref -> False
          | otherwise -> goTerm body
        X.ETyInst inner inst -> goTerm inner || goInst inst
        X.ERoll _ body -> goTerm body
        X.EUnroll inner -> goTerm inner

    goInst inst =
      case inst of
        X.InstId -> False
        X.InstApp _ -> False
        X.InstBot _ -> False
        X.InstIntro -> False
        X.InstElim -> False
        X.InstAbstrRef ref ->
          X.typeBinderRefsSameIdentity target ref
        X.InstUnderRef ref inner
          | X.typeBinderRefsSameIdentity target ref -> False
          | otherwise -> goInst inner
        X.InstInside inner -> goInst inner
        X.InstSeq left right -> goInst left || goInst right

-- | Test seam for the identity-monotone checked-IR normalizer.
normalizeCheckedTypeRedexesForTest :: XmlfTerm -> XmlfTerm
normalizeCheckedTypeRedexesForTest =
  normalizeCheckedTypeRedexes

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
       in if instIsRedundantAtTarget env inner' inst
            then inner'
            else X.ETyInst inner' inst
    X.ERoll ty body -> X.ERoll ty (dropStaleTypeInsts env body)
    X.EUnroll inner -> X.EUnroll (dropStaleTypeInsts env inner)

-- | Owner-local regression seam.  Production callers should continue to use
-- the enclosing deferred-rewrite pipeline so the environment is authoritative.
dropStaleTypeInstsForTest :: Env -> XmlfTerm -> XmlfTerm
dropStaleTypeInstsForTest =
  dropStaleTypeInsts

-- | Reconstruct occurrences of one local binding from its finalized scheme.
-- The local identity is the authority: display/runtime spelling is irrelevant.
-- A monomorphic scheme cannot support a leading forall elimination, so remove
-- that computation while publishing the finalized type on the occurrence.
constructLocalOccurrencesForScheme :: Env -> X.ResolvedVar -> ElabType -> XmlfTerm -> XmlfTerm
constructLocalOccurrencesForScheme env binding schemeTy =
  go
  where
    schemeIsMonomorphic =
      case operationalTargetType env schemeTy of
        X.TForallRef {} -> False
        _ -> True

    isBoundOccurrence =
      X.resolvedVarSameIdentity binding

    canonicalOccurrence =
      X.mapResolvedVarType (const schemeTy)

    go term =
      case term of
        X.EVarNode resolved
          | isBoundOccurrence resolved ->
              X.EVarNode (canonicalOccurrence resolved)
          | otherwise -> term
        X.ELit {} -> term
        X.ELam resolved body
          | isBoundOccurrence resolved -> term
          | otherwise -> X.ELam resolved (go body)
        X.EApp fun arg -> X.EApp (go fun) (go arg)
        X.ELet resolved scheme rhs body
          | isBoundOccurrence resolved -> term
          | otherwise -> X.ELet resolved scheme (go rhs) (go body)
        X.ETyAbsRef ref mbBound body ->
          X.ETyAbsRef ref mbBound (go body)
        X.ETyInst inner inst ->
          let inner' = go inner
           in case (schemeIsMonomorphic, inst, inner') of
                (True, X.InstElim, X.EVarNode resolved)
                  | isBoundOccurrence resolved ->
                      X.EVarNode (canonicalOccurrence resolved)
                _ -> X.ETyInst inner' inst
        X.ERoll ty body -> X.ERoll ty (go body)
        X.EUnroll inner -> X.EUnroll (go inner)

-- | Owner-local regression seam for local-scheme construction.
constructLocalOccurrencesForSchemeForTest :: Env -> X.ResolvedVar -> ElabType -> XmlfTerm -> XmlfTerm
constructLocalOccurrencesForSchemeForTest =
  constructLocalOccurrencesForScheme

-- A deferred rewrite can replace a provisional bottom with its final concrete
-- type.  A previously valid @InstBot concrete@ may then sit either directly at
-- that type or inside a forall whose bound is already concrete.  Prove that
-- such a computation is now identity from the checked source type before
-- removing it; every other malformed computation remains for the enclosing
-- typecheck to reject.
instIsRedundantAtTarget :: Env -> XmlfTerm -> X.Instantiation -> Bool
instIsRedundantAtTarget env term inst =
  case typeCheckWithEnv env term of
    Right actualTy -> instIsIdentityAtType env actualTy inst
    Left _ -> False

instIsIdentityAtType :: Env -> ElabType -> X.Instantiation -> Bool
instIsIdentityAtType env actualTy inst =
  case inst of
    X.InstId -> True
    X.InstBot targetTy ->
      alphaEqType (operationalTargetType env actualTy) targetTy
    X.InstInside inner ->
      case operationalTargetType env actualTy of
        X.TForallRef _ mbBound _ ->
          instIsIdentityAtType
            env
            (maybe X.TBottom X.tyToElab mbBound)
            inner
        _ -> False
    _ -> False

operationalTargetType :: Env -> ElabType -> ElabType
operationalTargetType env = chase []
  where
    chase seen ty@(X.TVarRef ref)
      | any (X.typeBinderRefsSameIdentity ref) seen = ty
      | otherwise =
          case TypeCheck.lookupTypeBindingRef ref env of
            Just bound -> chase (ref : seen) bound
            Nothing -> ty
    chase _ ty = ty

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
    Right ty
      | letTermConstructsBoundedForallSlot fallback rhs,
        compatibleLetType fallbackTy rhsTy ->
          fallbackTy
      | otherwise ->
          preserveRewrittenLetScheme
            (stripVacuousForalls fallbackTy)
            (stripVacuousForalls rhsTy)
      where
        fallbackTy = inlineTypeEnvBounds env fallback
        rhsTy = inlineTypeEnvBounds env ty
    Left _ -> fallback

-- A bounded forall is an operational producer/consumer slot even when its
-- binder is absent from the body.  If the rewritten RHS explicitly constructs
-- that exact slot, keep the declared scheme so later InstElim nodes continue
-- to meet a forall rather than a prematurely erased arrow.
letTermConstructsBoundedForallSlot :: ElabType -> XmlfTerm -> Bool
letTermConstructsBoundedForallSlot = go
  where
    go (X.TForallRef typeRef mbTypeBound bodyTy) (X.ETyAbsRef termRef mbTermBound bodyTerm)
      | X.typeBinderRefsSameIdentity typeRef termRef =
          case (mbTypeBound, mbTermBound) of
            (Just _, Just _) -> True
            _ -> go bodyTy bodyTerm
    go _ _ = False

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
sourceForallMatchesWithRigidForallsInScope scope =
  sourceForallMatchesWithRigidForallsAndHeadIdentitiesInScope scope Map.empty

sourceForallMatchesWithRigidForallsAndHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> SrcType -> Bool
sourceForallMatchesWithRigidForallsAndHeadIdentitiesInScope scope headIdentities expected actual =
  case sourceForallMatchSubstWithHeadIdentitiesInScope scope headIdentities expected actual of
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

sourceForallMatchesInScope :: ElaborateScope -> SrcType -> SrcType -> Bool
sourceForallMatchesInScope scope expected actual =
  case sourceForallMatchSubstInScope scope expected actual of
    Just _ -> True
    Nothing -> False

sourceForallMatchSubstInScope :: ElaborateScope -> SrcType -> SrcType -> Maybe (Map String SrcType)
sourceForallMatchSubstInScope scope =
  sourceForallMatchSubstWithHeadIdentitiesInScope scope Map.empty

sourceForallMatchSubstWithHeadIdentitiesInScope :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> SrcType -> Maybe (Map String SrcType)
sourceForallMatchSubstWithHeadIdentitiesInScope scope headIdentities expected actual =
  sourceForallMatchSubstWith
    (\expectedHead actualHead -> alphaEq (STBase expectedHead) (STBase actualHead))
    alphaEq
    expected
    actual
  where
    alphaEq =
      alphaEqTypesWithHeadIdentitiesInScope scope headIdentities

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
              rhsClosed0 = closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme' rhs'
              (resolved', rhsClosed, bodyForScheme, env') =
                constructLocalLetAtScheme env resolved rhsTy rhsClosed0 body
          X.ELet resolved' scheme' rhsClosed <$> go env' bodyForScheme
        X.ETyAbsRef ref mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = TypeCheck.insertTypeBindingRef ref boundTy env
          X.ETyAbsRef ref mbBound <$> go env' body
        X.ETyInst inner inst -> (`X.ETyInst` inst) <$> go env inner
        X.ERoll ty body -> X.ERoll ty <$> go env body
        X.EUnroll inner -> X.EUnroll <$> go env inner

constructLocalLetAtScheme ::
  Env ->
  X.ResolvedVar ->
  ElabType ->
  XmlfTerm ->
  XmlfTerm ->
  (X.ResolvedVar, XmlfTerm, XmlfTerm, Env)
constructLocalLetAtScheme env resolved schemeTy rhs body =
  (resolved', rhs', body', env')
  where
    resolved' = X.mapResolvedVarType (const schemeTy) resolved
    env' = TypeCheck.insertResolvedTermBinding resolved' schemeTy env
    rhs' = constructLocalOccurrencesForScheme env' resolved' schemeTy rhs
    body' = constructLocalOccurrencesForScheme env' resolved' schemeTy body

resolveDeferredConstructors :: ElaborateScope -> Env -> Map DeferredRef DeferredConstructorCall -> XmlfTerm -> Either ProgramError XmlfTerm
resolveDeferredConstructors scope env deferredConstructors term =
  fst <$> resolveDeferredConstructorsWithSupply Nothing scope env deferredConstructors term

resolveDeferredConstructorsWithSupply :: Maybe IdentityGenerator -> ElaborateScope -> Env -> Map DeferredRef DeferredConstructorCall -> XmlfTerm -> Either ProgramError (XmlfTerm, Maybe IdentityGenerator)
resolveDeferredConstructorsWithSupply mbGenerator scope env deferredConstructors = go False mbGenerator env
  where
    lookupDeferredConstructor ref =
      Map.lookup ref deferredConstructors

    go completionAlreadyConsumed generator env0 term =
      case deferredPlaceholderHeadRefWithInsts term of
        Just (ref, headInsts)
          | Just deferred <- lookupDeferredConstructor ref,
            deferredConstructorArgCount deferred == 0 ->
              instantiateConstructorOccurrence
                generator
                env0
                (deferredRefName ref)
                deferred
                headInsts
                []
                term
        _ ->
          case term of
            X.EVarNode {} -> Right (term, generator)
            X.ELit {} -> Right (term, generator)
            X.ELam resolved body ->
              let ty = X.resolvedVarType resolved
                  env' = TypeCheck.insertResolvedTermBinding resolved ty env0
               in do
                    (body', generator') <- go False generator env' body
                    Right (X.ELam resolved body', generator')
            X.EApp {} -> rewriteApplication generator env0 term
            X.ELet resolved scheme rhs body -> do
              let schemeTy = schemeToType scheme
                  rhsEnv = TypeCheck.insertResolvedTermBinding resolved schemeTy env0
              (rhs', generator1) <- go False generator rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  scheme' = schemeFromType rhsTy
                  (resolved', rhsForScheme, bodyForScheme, env') =
                    constructLocalLetAtScheme env0 resolved rhsTy rhs' body
              (body', generator2) <- go False generator1 env' bodyForScheme
              Right (X.ELet resolved' scheme' rhsForScheme body', generator2)
            X.ETyAbsRef ref mbBound body ->
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = TypeCheck.insertTypeBindingRef ref boundTy env0
               in do
                    let originalBodyTy = typeCheckWithEnv env' body
                    (body', generator') <- go completionAlreadyConsumed generator env' body
                    -- See Note [Consume completion binders closed by deferred construction].
                    let rewrittenBodyTy = typeCheckWithEnv env' body'
                        abstraction = X.ETyAbsRef ref mbBound body'
                        binderOccursIn =
                          any
                            (X.typeBinderRefsSameIdentity ref)
                            . freeTypeVarRefsType
                        compilerOwnedCompletionBinder =
                          case typeBinderIdentityNode (X.typeBinderRefIdentity ref) of
                            Just _ -> True
                            Nothing -> False
                        rewriteClosedBinder =
                          case rewrittenBodyTy of
                            Right rewrittenTy
                              | body' /= body,
                                not completionAlreadyConsumed,
                                not (binderOccursIn rewrittenTy) ->
                                  compilerOwnedCompletionBinder
                                    || case originalBodyTy of
                                      Right originalTy -> binderOccursIn originalTy
                                      Left _ -> False
                            _ -> False
                        consumedAbstraction =
                          X.ETyInst
                            abstraction
                            ( case mbBound of
                                Just bound -> X.InstApp (X.tyToElab bound)
                                Nothing -> X.InstElim
                            )
                    Right
                      ( if rewriteClosedBinder
                          then consumedAbstraction
                          else abstraction
                      , generator'
                      )
            X.ETyInst inner inst -> do
              -- The existing computation owns the contiguous leading
              -- abstraction spine below it.  Do not independently consume a
              -- graph completion binder from that same spine.
              (inner', generator') <- go True generator env0 inner
              Right (X.ETyInst inner' inst, generator')
            X.ERoll ty body -> do
              (body', generator') <- go False generator env0 body
              Right (X.ERoll ty body', generator')
            X.EUnroll inner -> do
              (inner', generator') <- go False generator env0 inner
              Right (X.EUnroll inner', generator')

    {- Note [Consume completion binders closed by deferred construction]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Constructor placeholders are elaborated under the completion binders
    needed by their unresolved occurrence type.  Resolving a placeholder can
    determine one of those variables, leaving the rewritten body independent
    of a binder that was meaningful before construction.  In that case the
    construction must also consume the abstraction: bounded binders use their
    derived [bound] instantiation and unbounded binders use N.

    A graph-identity binder is compiler-owned completion state and can already
    be vacuous before the placeholder is resolved: its bound records the
    construction endpoint rather than a source ABI slot.  Consume such a
    binder only when this subtree was actually rewritten and its result is
    independent afterwards.  For source-owned binders, retain the stricter
    before-and-after dependency check.  This preserves source-vacuous foralls;
    globally stripping vacuous type abstractions would conflate their ABI with
    a binder made redundant specifically by deferred construction.  An
    enclosing ETyInst already owns its contiguous leading abstraction spine;
    consuming any binder in that spine again would compose the old computation
    against an already-consumed body.
    -}

    rewriteApplication generator env0 term =
      let (headTerm, args) = Reduce.collectApplicationSpineThroughHeadTypeRedexes term
       in case deferredPlaceholderHeadRefWithInsts headTerm of
            Just (ref, headInsts)
              | Just deferred <- lookupDeferredConstructor ref -> do
              (args', generator') <- mapAccumTerms generator env0 args
              instantiateConstructorOccurrence
                generator'
                env0
                (deferredRefName ref)
                deferred
                headInsts
                args'
                term
            Nothing ->
              case term of
                X.EApp fun arg -> rewriteOrdinaryApplication generator env0 fun arg
                _ -> Right (term, generator)
            _ ->
              case term of
                X.EApp fun arg -> rewriteOrdinaryApplication generator env0 fun arg
                _ -> Right (term, generator)

    rewriteOrdinaryApplication generator env0 fun arg = do
      (fun', generator1) <- go False generator env0 fun
      (arg', generator2) <- go False generator1 env0 arg
      Right (X.EApp fun' arg', generator2)

    mapAccumTerms generator _ [] = Right ([], generator)
    mapAccumTerms generator env0 (item : rest) = do
      (item', generator1) <- go False generator env0 item
      (rest', generator2) <- mapAccumTerms generator1 env0 rest
      Right (item' : rest', generator2)

    instantiateConstructorOccurrence generator _ _ deferred headInsts args _
      | deferredConstructorBindingMode deferred == DeferredBindingScheme =
          preserveConstructorScheme generator deferred headInsts args
    instantiateConstructorOccurrence generator env0 placeholderName deferred headInsts args occurrenceTerm = do
      let ctorInfo = deferredConstructorInfo deferred
          instBinders = deferredConstructorInstBinders deferred
      constructorArgTemplates <-
        zipWithM
          (constructorArgumentTemplateView instBinders)
          (ctorArgs ctorInfo)
          (constructorInfoArgViews ctorInfo)
      let visibleArgCount = min (deferredConstructorArgCount deferred) (length constructorArgTemplates)
          visibleArgTemplates = take visibleArgCount constructorArgTemplates
          visibleArgs = take visibleArgCount args
      argViews <- mapM (inferArgTypeView env0) visibleArgs
      (substFromHead, headInstViews) <-
        consumeDeferredConstructorHeadInstantiations
          scope
          (ctorName ctorInfo)
          instBinders
          (deferredConstructorInitialSubst deferred)
          headInsts
      substFromArgs <-
        maybe
          (Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo)))
          Right
          (matchTypeBinderSubstViewPairsInScope scope instBinders substFromHead (zip visibleArgTemplates argViews))
      let argHeadIdentities =
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
              inferOccurrenceTypeView
                occurrenceEnv
                deferred
                substFromArgs
                occurrenceFallbackView
                occurrenceTerm
      substFinal <-
        maybe
          (Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo)))
          Right
          (matchTypeBinderSubstTypeViewInScope scope instBinders substFromArgs (deferredConstructorOccurrenceTypeView deferred) occurrenceView)
      let constructorHeadIdentities =
            mergeSymbolIdentityMaps [argHeadIdentities, typeViewHeadIdentities occurrenceView]
          missingInstBinders =
            filter
              (\(_, identity) -> maybe True (const False) (lookupTypeBinderSubstViewByIdentity identity substFinal))
              instBinders
      case missingInstBinders of
        [] -> do
          resolvedConstructor <- resolvedVarFromConstructorInfo scope ctorInfo
          (ctorHead, generator') <-
            if constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo
              then
                do
                  headTerm <-
                    foldM
                      ( \headAcc (_, identity) ->
                          case lookupTypeBinderSubstViewByIdentity identity substFinal of
                            Just view -> do
                              instTy <- typeViewToElabType scope view
                              Right (X.ETyInst headAcc (X.InstApp instTy))
                            Nothing -> Right headAcc
                      )
                      (X.EVarNode resolvedConstructor)
                      instBinders
                  Right (headTerm, generator)
              else
                inlineConstructorHeadWithSupply
                  ConstructorOccurrenceTerm
                  generator
                  scope
                  constructorHeadIdentities
                  []
                  ctorInfo
                  substFinal
          Right (foldl X.EApp ctorHead args, generator')
        _ ->
          Left (ProgramAmbiguousConstructorUse (ctorName ctorInfo))

    preserveConstructorScheme generator deferred headInsts args
      | not (null headInsts) || not (null args) =
          Left
            ( ProgramPipelineError
                ( "whole-scheme deferred constructor `"
                    ++ ctorName ctorInfo
                    ++ "` acquired a monomorphic application during finalization"
                )
            )
      | constructorOwnerRuntimeTypeTrackable (elaborateScopeDataTypesByIdentity scope) ctorInfo = do
          resolvedConstructor <- resolvedVarFromConstructorInfo scope ctorInfo
          Right (X.EVarNode resolvedConstructor, generator)
      | otherwise =
          inlineConstructorHeadWithSupply
            ConstructorOccurrenceTerm
            generator
            scope
            constructorHeadIdentities
            []
            ctorInfo
            emptyTypeBinderSubst
      where
        ctorInfo = deferredConstructorInfo deferred
        constructorHeadIdentities =
          mergeSymbolIdentityMaps
            [ typeViewHeadIdentities (deferredConstructorSourceTypeView deferred),
              typeViewHeadIdentities (deferredConstructorOccurrenceTypeView deferred)
            ]

    inferArgTypeView env0 arg =
      case resolvedConstructorApplicationResultView arg of
        Just view -> Right view
        Nothing ->
          case typeCheckWithEnv env0 arg of
            Right ty ->
              Right
                ( elabTypeToRecoveredTypeView
                    scope
                    (stripVacuousForalls (inlineTypeEnvBounds env0 ty))
                )
            Left (X.TCArgumentMismatch _ actualTy) ->
              Right
                ( elabTypeToRecoveredTypeView
                    scope
                    (stripVacuousForalls (inlineTypeEnvBounds env0 actualTy))
                )
            Left err -> Left (ProgramPipelineError ("deferred constructor argument type check failed: " ++ show err))

    {- Note [Recover constructor results from explicit type applications]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    A structural Church encoding can omit an owner parameter from the
    operational ElabType even though that parameter remains semantically
    relevant to the nominal result.  Once a resolved constructor head is
    fully term-applied, its identity and ordered InstApp spine are positive
    authority for that result.  Reconstruct the nominal TypeView from the
    complete binding forall spine (owner parameters plus constructor-local
    foralls); ctorForallBinderInfo alone omits owner parameters.

    Partial applications and non-InstApp computations deliberately fall back
    to ordinary type checking instead of guessing from shape or display names.
    -}
    resolvedConstructorApplicationResultView arg = do
      let (headTerm, termArgs) =
            Reduce.collectApplicationSpineThroughHeadTypeRedexes arg
      (constructorIdentity, instTys) <-
        resolvedConstructorHeadWithInsts headTerm
      (_, ctorInfo) <-
        lookupConstructorRuntimeBySymbol scope constructorIdentity
      let binders =
            typeViewForallBinderViews
              (constructorBindingSourceTypeView scope ctorInfo)
      if length termArgs < length (ctorArgs ctorInfo)
        || length instTys < length binders
        then Nothing
        else
          let instViews =
                map
                  ( elabTypeToRecoveredTypeView scope
                      . stripVacuousForalls
                  )
                  (take (length binders) instTys)
              subst =
                foldl
                  ( \acc (binder, view) ->
                      insertTypeBinderSubstView
                        (constructorBindingForallIdentity binder)
                        view
                        acc
                  )
                  emptyTypeBinderSubst
                  (zip binders instViews)
           in Just
                ( applyTypeViewSubst
                    (typeBinderSubstToTypeViewSubst subst)
                    (constructorInfoResultView ctorInfo)
                )
      where
        constructorBindingForallIdentity (_, identity, _) =
          identity

    resolvedConstructorHeadWithInsts = collect []
      where
        collect insts headTerm =
          case headTerm of
            X.EVarNode resolved ->
              case X.resolvedVarDetails resolved of
                ConstructorId ref ->
                  Just (constructorRefSymbol ref, insts)
                _ -> Nothing
            X.ETyInst inner inst -> do
              currentInsts <- orderedInstAppTypes inst
              collect (currentInsts ++ insts) inner
            _ -> Nothing

    inferOccurrenceTypeView env0 deferred subst fallbackView occurrenceTerm =
      case typeCheckWithEnv env0 occurrenceTerm of
        Right ty ->
          let occurrenceTy =
                stripVacuousForalls (inlineTypeEnvBounds env0 ty)
           in Right
                ( if deferredConstructorSubstIsConcrete deferred subst
                    && deferredConstructorOccurrenceOwnerMatches deferred occurrenceTy
                    then fallbackView
                    else elabTypeToRecoveredTypeView scope occurrenceTy
                )
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

    constructorArgumentTemplateView binders sourceTy identityView =
      case typeViewWithDisplay sourceTy viewWithBinderAliases of
        Right view -> Right view
        Left err ->
          Left
            ( ProgramPipelineError
                ( "constructor argument TypeView lost identity structure: "
                    ++ show err
                )
            )
      where
        viewWithBinderAliases =
          typeViewMergeBinderIdentityAliases
            (typeBinderAliasIdentityMap binders)
            identityView

    applyConstructorViewSubst subst =
      applyTypeViewSubst (typeBinderSubstToTypeViewSubst subst)

    deferredConstructorSubstIsConcrete deferred subst =
      all binderIsConcrete (deferredConstructorInstBinders deferred)
      where
        binderIsConcrete (_, identity) =
          case lookupTypeBinderSubstViewByIdentity identity subst of
            Just view ->
              typeViewIdentity view /= STBottom
                && not (typeViewIsBareBinderIdentity identity view)
            Nothing -> False

    deferredConstructorOccurrenceOwnerMatches deferred =
      ownerMatches
      where
        ownerIdentity =
          ctorOwningTypeIdentity (deferredConstructorInfo deferred)
        ownerUnique = symbolUniqueIdentity ownerIdentity

        ownerMatches ty =
          case ty of
            X.TBaseWithIdentity identity _ ->
              sameSymbolIdentity identity ownerIdentity
            X.TConWithIdentity identity _ _ ->
              sameSymbolIdentity identity ownerIdentity
            X.TMuRef ref _ ->
              case typeBinderIdentityStructural (X.typeBinderRefIdentity ref) of
                Just (unique, StructuralSelfBinder) -> unique == ownerUnique
                _ -> False
            X.TForallRef _ _ body -> ownerMatches body
            _ -> False

matchTypeBinderSubstViewPairsInScope ::
  ElaborateScope ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  [(TypeView, TypeView)] ->
  Maybe TypeBinderSubst
matchTypeBinderSubstViewPairsInScope scope binders =
  foldM (\subst (templateView, actualView) -> matchTypeBinderSubstTypeViewInScope scope binders subst templateView actualView)

matchTypeBinderSubstTypeViewInScope ::
  ElaborateScope ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  TypeView ->
  TypeView ->
  Maybe TypeBinderSubst
matchTypeBinderSubstTypeViewInScope scope binders subst templateView actualView =
  typeBinderSubstFromTypeViewSubst
    <$> matchTypeViewsAgainstIdentityRefiningBottom
      scope
      (typeBinderSubstToTypeViewSubst subst)
      (NE.singleton (templateViewWithBinders binders templateView))
      (NE.singleton actualView)

templateViewWithBinders :: [(String, TypeBinderIdentity)] -> TypeView -> TypeView
templateViewWithBinders binders view =
  typeViewMergeBinderIdentityAliases (typeBinderAliasIdentityMap binders) view

bindTypeBinderSubstViewInScope ::
  ElaborateScope ->
  (String, TypeBinderIdentity) ->
  TypeView ->
  TypeBinderSubst ->
  Maybe TypeBinderSubst
bindTypeBinderSubstViewInScope scope (_, identity) actualView subst =
  case lookupTypeBinderSubstViewByIdentity identity subst of
    Nothing ->
      Just (insertTypeBinderSubstView identity actualView subst)
    Just existingView
      | typeViewIsBareBinderIdentity identity existingView ->
          Just (insertTypeBinderSubstView identity actualView subst)
      | semanticTypeViewsMatchInScope scope existingView actualView ->
          Just subst
      | otherwise -> Nothing

semanticTypeViewsMatchInScope :: ElaborateScope -> TypeView -> TypeView -> Bool
semanticTypeViewsMatchInScope scope left right =
  case (typeViewToElabType scope left, typeViewToElabType scope right) of
    (Right leftTy, Right rightTy) ->
      alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy
    _ -> False

consumeDeferredConstructorHeadInstantiations ::
  ElaborateScope ->
  String ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  [ElabType] ->
  Either ProgramError (TypeBinderSubst, [TypeView])
consumeDeferredConstructorHeadInstantiations scope constructorName binders subst0 instTys =
  finish
    <$> foldM
      consume
      (subst0, binders, [])
      instTys
  where
    consume (subst, remainingBinders, views) instTy =
      case remainingBinders of
        binder@(_, binderIdentity) : rest -> do
          let recoveredInstView =
                elabTypeToRecoveredTypeView
                  scope
                  (stripVacuousForalls instTy)
          subst' <-
            maybe
              (Left (ProgramAmbiguousConstructorUse constructorName))
              Right
              (bindTypeBinderSubstViewInScope scope binder recoveredInstView subst)
          selectedView <-
            maybe
              (Left (ProgramAmbiguousConstructorUse constructorName))
              Right
              (lookupTypeBinderSubstViewByIdentity binderIdentity subst')
          Right (subst', rest, selectedView : views)
        [] ->
          Left (ProgramAmbiguousConstructorUse constructorName)

    finish (subst, _, views) =
      (subst, reverse views)

-- | Owner-local seam for the identity-aware constructor-head consumer.
consumeDeferredConstructorHeadInstantiationsForTest ::
  ElaborateScope ->
  String ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  [ElabType] ->
  Either ProgramError TypeBinderSubst
consumeDeferredConstructorHeadInstantiationsForTest scope constructorName binders subst instTys =
  fst
    <$> consumeDeferredConstructorHeadInstantiations
      scope
      constructorName
      binders
      subst
      instTys

data ConstructorTermPurpose
  = ConstructorBindingTerm
  | ConstructorOccurrenceTerm

inlineConstructorHeadWithSupply :: ConstructorTermPurpose -> Maybe IdentityGenerator -> ElaborateScope -> Map String SymbolIdentity -> [(String, TypeBinderIdentity)] -> ConstructorInfo -> TypeBinderSubst -> Either ProgramError (XmlfTerm, Maybe IdentityGenerator)
inlineConstructorHeadWithSupply purpose mbGenerator scope extraHeadIdentities ownerParamBinders ctorInfo subst = do
  let viewSubst = typeBinderSubstToTypeViewSubst subst
      resultView = applyTypeViewSubst viewSubst (constructorInfoResultView ctorInfo)
      argumentViews = map (applyTypeViewSubst viewSubst) (constructorInfoArgViews ctorInfo)
      resultVar = "$" ++ symbolIdentityStableName (ctorOwningTypeIdentity ctorInfo) ++ "_result"
      argNames = ["$" ++ constructorInfoIdentityName ctorInfo ++ "_arg" ++ show ix | ix <- [1 .. length argumentViews]]
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
      loweredResultSrcTy = lowerTypeView scope resultView
      loweredArgSrcTys = map (lowerTypeView scope) argumentViews
      loweredHandlerSrcTys = map (lowerType scope . handlerSrcType) handlerShapes
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
        Map.mapWithKey ownerParamRefForAlias (typeBinderIdentityAliasMap ownerParamAliasEntries)
      ownerParamRefForAlias alias identity =
        X.typeBinderRefFromIdentity identity (Map.findWithDefault alias identity ownerParamNamesByIdentity)
      ownerParamNamesByIdentity =
        Map.fromList [(identity, name) | (name, identity) <- ownerParamBinders]
      ownerParamIdentitySet =
        Set.fromList (map snd ownerParamBinders)
      -- Constructor metadata may still display an owner parameter under the
      -- declaration spelling after another owner surface has renamed it.  The
      -- carried identity makes those spellings aliases of the same binder;
      -- install every such alias before computing free names so we never
      -- allocate a second top-level forall for that parameter.
      ownerParamAliasEntries =
        ownerParamBinders
          ++ [ (alias, identity)
             | (alias, identity) <- typeViewBinderIdentityAliasEntries (constructorTypeView scope ctorInfo),
               Set.member identity ownerParamIdentitySet
             ]
      selectedHandlerShape =
        case drop (ctorIndex ctorInfo) handlerShapes of
          shape : _ -> Just shape
          [] -> Nothing
      constructorLocalBinders =
        [ (constructorForallDisplayName binder, constructorForallIdentity binder)
        | shape <- maybe [] (: []) selectedHandlerShape,
          binder <- constructorShapeForallBinderInfo shape
        ]
      constructorLocalRefsByAlias =
        Map.mapWithKey constructorLocalRefForAlias (typeBinderIdentityAliasMap constructorLocalBinders)
      constructorLocalRefForAlias alias identity =
        X.typeBinderRefFromIdentity identity (Map.findWithDefault alias identity constructorLocalNamesByIdentity)
      constructorLocalNamesByIdentity =
        Map.fromList [(identity, name) | (name, identity) <- constructorLocalBinders]
      structuralRefsByAlias =
        typeBinderIdentityRefs structuralBinderIdentities
      ownerParamRefs =
        [ X.typeBinderRefFromIdentity identity name
        | (name, identity) <- ownerParamBinders
        ]
      constructorLocalRefs =
        [ X.typeBinderRefFromIdentity identity name
        | (name, identity) <- constructorLocalBinders
        ]
      knownRefs =
        ownerParamRefsByAlias
          `Map.union` constructorLocalRefsByAlias
          `Map.union` structuralRefsByAlias
      missingSharedFreeNames =
        filter (`Map.notMember` knownRefs) sharedFreeNames
      (freshSharedRefs, generator0) =
        freshTypeBinderRefsFromSupply headIdentities ownerParamBinders structuralBinderIdentities missingSharedFreeNames
      sharedRefs =
        knownRefs `Map.union` freshSharedRefs
      sharedTypeAbsRefs =
        [ ref
        | name <- sharedFreeNames,
          Just ref <- [Map.lookup name sharedRefs]
        ]
      topTypeAbsRefs =
        foldl insertRefByIdentity [] (ownerParamRefs ++ purposeLocalRefs ++ sharedTypeAbsRefs)
      purposeLocalRefs =
        case purpose of
          ConstructorBindingTerm -> constructorLocalRefs
          ConstructorOccurrenceTerm -> []
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
      (handlerResolved, generator6) = freshResolvedLocals generator5 (zip handlerNames handlerTys)
  (selectedResolved, selectedShape) <-
    case drop (ctorIndex ctorInfo) (zip handlerResolved handlerShapes) of
      selected : _ -> Right selected
      [] -> Left (ProgramPipelineError ("constructor handler order missing `" ++ ctorName ctorInfo ++ "`"))
  selectedForallRefs <-
    traverse
      ( \binder ->
          case
            find
              ( (== constructorForallIdentity binder)
                  . X.typeBinderRefIdentity
              )
              (Map.elems sharedRefs)
          of
            Just ref -> Right ref
            Nothing ->
              Left
                ( ProgramPipelineError
                    ( "constructor handler forall `"
                        ++ constructorForallDisplayName binder
                        ++ "` is missing its enclosing constructor abstraction"
                    )
                )
      )
      (constructorShapeForallBinderInfo selectedShape)
  let selectedHead =
        foldl
          ( \headTerm ref ->
              X.ETyInst
                headTerm
                (X.InstSeq (X.InstInside (X.instAbstrWithRef ref)) X.InstElim)
          )
          (X.EVarNode selectedResolved)
          selectedForallRefs
      selectedBody = foldl X.EApp selectedHead (map X.EVarNode argResolved)
      handlerBody = foldr X.ELam selectedBody handlerResolved
      rolled = X.ERoll resultTy (X.eTyAbsWithRef resultRef Nothing handlerBody)
      valueBody = foldr X.ELam rolled argResolved
  pure
    ( foldr (`X.ETyAbsRef` Nothing) valueBody topTypeAbsRefs,
      generator6 <$ mbGenerator
    )
  where
    insertRefByIdentity refs ref
      | refIdentityIn refs ref = refs
      | otherwise = refs ++ [ref]

    refIdentityIn refs ref =
      any (X.typeBinderRefsSameIdentity ref) refs

    freshTypeBinderRefsFromSupply headIdentities ownerBinders structuralBinders names =
      freshTypeBinderRefsWithSupply mbGenerator occupiedIdentities names
      where
        occupiedIdentities =
          concatMap symbolGeneratedIdentities (Map.elems headIdentities)
            ++ concatMap (typeBinderGeneratedIdentities . snd) ownerBinders
            ++ concatMap typeBinderGeneratedIdentities (Map.elems structuralBinders)

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
          actualView =
            applyTypeViewSubst
              (typeBinderSubstToTypeViewSubst subst)
              (constructorInfoResultView ctorInfo)
       in typeBinderSubstFromTypeViewSubst
            <$> matchTypeViewsAgainstIdentity
              scope
              Map.empty
              (NE.singleton templateView)
              (NE.singleton actualView)

    constructorShapeResultMatchView shape =
      typeViewWithIdentityAliases
        ( mergeSymbolIdentityMaps
            [ typeViewHeadIdentities (constructorShapeTypeView shape),
              typeHeadIdentitiesInScope scope
            ]
        )
        (typeBinderAliasIdentityMap (constructorShapeForallBinders shape))
        (constructorShapeResultView shape)

applyConstructorShapeSubst :: TypeBinderSubst -> ConstructorShape -> ConstructorShape
applyConstructorShapeSubst subst shape =
  let keptOwnerParams =
        filter keepOwnerParam (constructorShapeOwnerTypeParams shape)
      keepOwnerParam param =
        maybe True (const False) (lookupTypeBinderSubstViewByIdentity (checkedTypeParamIdentity param) subst)
   in shape
        { constructorShapeTypeView =
            specializeQuantifiedTypeView
              (typeBinderSubstToTypeViewSubst subst)
              (constructorShapeTypeView shape),
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
      [ (checkedTypeParamName param, checkedTypeParamIdentity param)
      | param <- ownerParams
      ]

resolveDeferredCasesWithSupply :: Maybe IdentityGenerator -> ElaborateScope -> Map DeferredRef DeferredCaseCall -> Env -> XmlfTerm -> Either ProgramError (Env, XmlfTerm, Maybe IdentityGenerator)
resolveDeferredCasesWithSupply mbGenerator scope deferredCases env0 term0 =
  go mbGenerator env0 term0
  where
    lookupDeferredCase ref =
      Map.lookup ref deferredCases

    go generator env term =
      case term of
        X.EVarNode {} -> Right (env, term, generator)
        X.ELit {} -> Right (env, term, generator)
        X.ELam resolved body -> do
          let ty = X.resolvedVarType resolved
              env' = TypeCheck.insertResolvedTermBinding resolved ty env
          (bodyEnv, body', generator') <- go generator env' body
          Right (mergeCaseEnv env bodyEnv, X.ELam resolved body', generator')
        X.EApp {} -> rewriteApplication generator env term
        X.ELet resolved scheme rhs body -> do
          let schemeTy = schemeToType scheme
              rhsEnv0 = TypeCheck.insertResolvedTermBinding resolved schemeTy env
          (rhsEnv, rhs', generator1) <- go generator rhsEnv0 rhs
          let baseBodyEnv = mergeCaseEnv env rhsEnv
              rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
              scheme' = schemeFromType rhsTy
              (resolved', rhsForScheme, bodyForScheme, env') =
                constructLocalLetAtScheme baseBodyEnv resolved rhsTy rhs' body
          (bodyEnv, body', generator2) <- go generator1 env' bodyForScheme
          Right (mergeCaseEnv env (mergeCaseEnv rhsEnv bodyEnv), X.ELet resolved' scheme' rhsForScheme body', generator2)
        X.ETyAbsRef ref mbBound body -> do
          let boundTy = maybe X.TBottom X.tyToElab mbBound
              env' = TypeCheck.insertTypeBindingRef ref boundTy env
          (bodyEnv, body', generator') <- go generator env' body
          Right (mergeCaseEnv env bodyEnv, X.ETyAbsRef ref mbBound body', generator')
        X.ETyInst inner inst ->
          case Reduce.reduceLeadingTypeInstantiationRedexes term of
            Just reduced -> go generator env reduced
            Nothing -> do
              (innerEnv, inner', generator') <- go generator env inner
              Right (innerEnv, X.ETyInst inner' inst, generator')
        X.ERoll ty body -> do
          (bodyEnv, body', generator') <- go generator env body
          Right (bodyEnv, X.ERoll ty body', generator')
        X.EUnroll inner -> do
          (innerEnv, inner', generator') <- go generator env inner
          Right (innerEnv, X.EUnroll inner', generator')

    rewriteApplication generator env term =
      let (headTerm, args) = Reduce.collectApplicationSpineThroughHeadTypeRedexes term
       in case deferredCasePlaceholderHeadRefWithInsts headTerm of
            Just (ref, headInsts)
              | Just deferred <- lookupDeferredCase ref -> do
              (argEnv, args', generator1) <- mapAccumCaseEnv generator env args
              resolveDeferredCaseApplication generator1 argEnv headTerm headInsts deferred args'
            _ ->
              case term of
                X.EApp fun arg -> do
                  (funEnv, fun', generator1) <- go generator env fun
                  (argEnv, arg', generator2) <- go generator1 env arg
                  Right (mergeCaseEnv funEnv argEnv, X.EApp fun' arg', generator2)
                _ -> Right (env, term, generator)

    resolveDeferredCaseApplication generator env headTerm headInsts deferred args =
      case splitAt (deferredCaseExpectedArgCount deferred) args of
        (caseArgs, residualArgs)
          | length caseArgs == deferredCaseExpectedArgCount deferred ->
              resolveCompleteCaseApplication
                generator
                env
                headTerm
                headInsts
                deferred
                caseArgs
                residualArgs
        _ ->
          Left
            ( ProgramDeferredCaseArityMismatch
                deferred
                (length args)
            )

    resolveCompleteCaseApplication generator env headTerm headInsts deferred caseArgs residualArgs =
      case caseArgs of
        scrutinee : handlers
          | length caseArgs == deferredCaseExpectedArgCount deferred -> do
              resultTy <- deferredCaseOccurrenceResultType env headTerm headInsts deferred
              scrutineeTy <- inferDeferredArgType env scrutinee
              validateCaseScrutineeType
                env
                deferred
                scrutineeTy
              (env', generator') <-
                extendCaseResultEnvWithSupply
                  generator
                  (deferredCaseDataInfo deferred)
                  scrutineeTy
                  (deferredCaseResultTypeView deferred)
                  resultTy
                  env
              let caseHead = caseEliminator resultTy scrutinee
              caseHeadTy <-
                case typeCheckWithEnv env' caseHead of
                  Right ty -> Right ty
                  Left err ->
                    Left
                      ( ProgramPipelineError
                          ( "deferred case eliminator failed type check: "
                              ++ show err
                              ++ "; scrutinee type="
                              ++ show scrutineeTy
                          )
                      )
              handlerTys <- caseHandlerTypes (length handlers) caseHeadTy
              closedHandlers <- zipWithM (closeCaseHandler env') handlerTys handlers
              let resolvedCase = foldl X.EApp caseHead closedHandlers
              Right (env', foldl X.EApp resolvedCase residualArgs, generator')
        _ ->
          Left
            ( ProgramDeferredCaseArityMismatch
                deferred
                (length caseArgs)
            )

    deferredCaseOccurrenceResultType env headTerm headInsts deferred = do
      occurrenceHeadTy <-
        case typeCheckWithEnv env headTerm of
          Right ty -> Right (inlineTypeEnvBounds env ty)
          Left err ->
            Left
              ( ProgramPipelineError
                  ( "deferred case occurrence head failed type check: "
                      ++ show err
                      ++ "; ordered head applications="
                      ++ show (length headInsts)
                  )
              )
      peelCaseArguments
        (deferredCaseExpectedArgCount deferred)
        occurrenceHeadTy

    peelCaseArguments remaining ty
      | remaining <= 0 = Right ty
      | otherwise =
          case ty of
            X.TArrow _ restTy ->
              peelCaseArguments (remaining - 1) restTy
            _ ->
              Left
                ( ProgramPipelineError
                    ( "deferred case occurrence head exposes only "
                        ++ show (deferredHandlerArity ty)
                        ++ " value-argument arrows"
                    )
                )

    caseHandlerTypes remaining ty
      | remaining <= 0 = Right []
      | otherwise =
          case ty of
            X.TArrow handlerTy restTy ->
              (handlerTy :) <$> caseHandlerTypes (remaining - 1) restTy
            _ ->
              Left
                ( ProgramPipelineError
                    ( "deferred case eliminator exposes only "
                        ++ show (deferredHandlerArity ty)
                        ++ " handler arrows"
                    )
                )

    deferredHandlerArity :: ElabType -> Int
    deferredHandlerArity ty =
      case ty of
        X.TArrow _ rest -> 1 + deferredHandlerArity rest
        _ -> 0

    closeCaseHandler env handlerTy handler =
      let canonicalizedHandler =
            TypeCheck.canonicalizeResolvedTermTypes env handler
          canonicalHandler =
            dropStaleTypeInsts env canonicalizedHandler
          closed =
            closeTermWithSchemeSubstRefsIfNeeded
              IntMap.empty
              (schemeFromType handlerTy)
              canonicalHandler
       in case typeCheckWithEnv env closed of
            Right actualTy
              | alphaEqType actualTy handlerTy -> Right closed
            Right actualTy ->
              Left
                ( ProgramPipelineError
                    ( "deferred case handler has type "
                        ++ show actualTy
                        ++ ", expected "
                        ++ show handlerTy
                    )
                )
            Left err ->
              Left
                ( ProgramPipelineError
                    ( "deferred case handler failed type check: "
                        ++ show err
                    )
                )

    validateCaseScrutineeType env deferred actualTy =
      case typeViewToElabType scope expectedView of
        Right expectedTy
          | actualOwnerCompatible,
            alphaEqType resolvedActualTy expectedTy
              || churchAwareEqType resolvedActualTy expectedTy ->
              Right ()
        _ ->
          case caseDataInfoForElabType resolvedActualTy of
            Just actualInfo
              | sameSymbolIdentity (dataInfoSymbol actualInfo) (dataInfoSymbol dataInfo) ->
                  case caseActualSourceTypeView dataInfo resolvedActualTy of
                    Just actualView ->
                      case
                        matchTypeViewsAgainstIdentity
                          scope
                          Map.empty
                          (NE.singleton expectedView)
                          (NE.singleton actualView)
                      of
                        Just _ -> Right ()
                        Nothing -> caseTypeMismatch deferred expectedView resolvedActualTy
                    _ -> caseTypeMismatch deferred expectedView resolvedActualTy
            _ -> caseTypeMismatch deferred expectedView resolvedActualTy
      where
        dataInfo = deferredCaseDataInfo deferred
        expectedView = deferredCaseScrutineeTypeView deferred
        resolvedActualTy = resolveTypeBinding [] actualTy

        resolveTypeBinding seen ty =
          case ty of
            X.TVarRef ref
              | not (any (X.typeBinderRefsSameIdentity ref) seen) ->
                  case TypeCheck.lookupTypeBindingRef ref env of
                    Just bound
                      | bound /= X.TBottom ->
                          resolveTypeBinding (ref : seen) bound
                    _ -> ty
            _ -> ty

        actualOwnerCompatible =
          case caseDataInfoForElabType resolvedActualTy of
            Just actualInfo ->
              sameSymbolIdentity (dataInfoSymbol actualInfo) (dataInfoSymbol dataInfo)
            Nothing -> True

    caseDataInfoForElabType :: X.Ty v -> Maybe DataInfo
    caseDataInfoForElabType ty =
      case ty of
        X.TBaseWithIdentity identity _ ->
          lookupSymbolIdentityExact identity (elaborateScopeDataTypesByIdentity scope)
        X.TConWithIdentity identity _ _ ->
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
        X.TBaseWithIdentity {} ->
          Just (rawElabTypeView actualTy)
        X.TConWithIdentity {} ->
          Just (rawElabTypeView actualTy)
        X.TForallRef _ _ body ->
          caseActualSourceTypeView dataInfo body
        X.TMuRef {} -> do
          (sourceHeadTy, _) <-
            matchDataInfoEncodingForElabType scope dataInfo actualTy
          let rawView = rawElabTypeView actualTy
              sourceView =
                requireTypeViewFromSourceTypeInScope
                  scope
                  ( mergeSymbolIdentityMaps
                      [ dataInfoHeadIdentityLookupAliases dataInfo,
                        typeViewHeadIdentities rawView
                      ]
                  )
                  (typeViewBinderIdentities rawView)
                  sourceHeadTy
          Just sourceView
        _ -> Nothing

    rawElabTypeView ty =
      requireTypeViewFromSourceTypeInScope
        scope
        (elabTypeHeadIdentities ty)
        (elabTypeBinderIdentities ty)
        (elabTypeToSrcType ty)

    caseTypeMismatch deferred expectedView ty =
      let actualTy = lowerType scope (recoverElabSourceType scope ty)
       in if actualTy == STBottom
            then
              Left
                ( ProgramDeferredCaseBottomScrutinee
                    deferred
                    (typeViewDisplay expectedView)
                )
            else Left (ProgramCaseOnNonDataType actualTy)

    caseEliminator resultTy scrutinee =
      X.ETyInst (X.EUnroll scrutinee) (X.InstApp resultTy)

    inferDeferredArgType env arg =
      case typeCheckWithEnv env arg of
        Right ty -> Right (stripVacuousForalls ty)
        Left (X.TCArgumentMismatch _ actualTy) ->
          Right (stripVacuousForalls actualTy)
        Left err ->
          Left (ProgramPipelineError ("deferred case scrutinee type check failed: " ++ show err))

    extendCaseResultEnvWithSupply generator dataInfo scrutineeTy resultView resultTy env = do
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
              bindingNames = resultBindingNames `Set.union` selfAliasBindingNames
              sharedNames =
                Set.toList $
                  bindingNames
                    `Set.union` freeSrcTypeVars loweredHeadTy
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
              occupiedIdentities =
                concatMap symbolGeneratedIdentities (Map.elems headIdentities)
                  ++ concatMap typeBinderGeneratedIdentities (Map.elems binderIdentities)
              (freshRefs, generator0) =
                freshTypeBinderRefsWithSupply generator occupiedIdentities missingSharedNames
              sharedRefs = knownRefs `Map.union` freshRefs
          (headTy, generator1) <-
            srcTypeToElabTypeWithScopedHeadIdentities scope headIdentities sharedRefs generator0 loweredHeadTy
          let selfAliasBindings =
                Map.fromSet (const headTy) selfAliasBindingNames
              resultBinding =
                Map.fromSet (const resultTy) resultBindingNames
              bindings = selfAliasBindings `Map.union` resultBinding
          env' <- foldM (insertCaseTypeBinding sharedRefs) env (Map.toList bindings)
          Right (env', generator1 <$ generator)
        Nothing -> Right (env, generator)
      where
        -- Structural matching is a lowering boundary only.  The owner was
        -- already selected from the carried nominal or structural identity.
        scrutineeRawTy = elabTypeToSrcType scrutineeTy

    insertCaseTypeBinding refs env (name, ty) =
      case Map.lookup name refs of
        Just ref -> Right (TypeCheck.insertTypeBindingRef ref ty env)
        Nothing -> Left (ProgramPipelineError ("unresolved deferred case type alias `" ++ name ++ "`"))

    mapAccumCaseEnv generator env [] = Right (env, [], generator)
    mapAccumCaseEnv generator env (arg : rest) = do
      (env1, arg', generator1) <- go generator env arg
      (env2, rest', generator2) <- mapAccumCaseEnv generator1 env1 rest
      Right (env2, arg' : rest', generator2)

    mergeCaseEnv base incoming =
      base {typeEnv = typeEnv (TypeCheck.unionEnvs incoming base)}

resolveDeferredMethodsWithSupply :: Maybe IdentityGenerator -> ElaborateScope -> Map DeferredRef DeferredMethodCall -> Env -> XmlfTerm -> Either ProgramError (XmlfTerm, Maybe IdentityGenerator)
resolveDeferredMethodsWithSupply mbGenerator scope deferredMethods env0 term0 =
  go mbGenerator env0 term0
  where
    lookupDeferredMethod ref =
      Map.lookup ref deferredMethods

    go generator env term =
      case deferredPlaceholderHeadRefWithInsts term of
        Just (ref, headInsts)
          | Just deferred <- lookupDeferredMethod ref,
            deferredMethodTotalArgCount deferred == 0 ->
              resolveDeferredNullaryMethod generator headInsts deferred
        _ ->
          case term of
            X.EVarNode {} -> Right (term, generator)
            X.ELit {} -> Right (term, generator)
            X.ELam resolved body -> do
              let ty = X.resolvedVarType resolved
                  env' = TypeCheck.insertResolvedTermBinding resolved ty env
              (body', generator') <- go generator env' body
              Right (X.ELam resolved body', generator')
            X.EApp {} -> rewriteApplication generator env term
            X.ELet resolved scheme rhs body -> do
              let schemeTy = schemeToType scheme
                  rhsEnv = TypeCheck.insertResolvedTermBinding resolved schemeTy env
              (rhs', generator1) <- go generator rhsEnv rhs
              let rhsTy = inferRewrittenLetType rhsEnv rhs' schemeTy
                  scheme' = schemeFromType rhsTy
                  (resolved', rhsForScheme, bodyForScheme, env') =
                    constructLocalLetAtScheme env resolved rhsTy rhs' body
              (body', generator2) <- go generator1 env' bodyForScheme
              Right (X.ELet resolved' scheme' rhsForScheme body', generator2)
            X.ETyAbsRef ref mbBound body -> do
              let boundTy = maybe X.TBottom X.tyToElab mbBound
                  env' = TypeCheck.insertTypeBindingRef ref boundTy env
              (body', generator') <- go generator env' body
              Right (X.ETyAbsRef ref mbBound body', generator')
            X.ETyInst inner inst -> do
              (inner', generator') <- go generator env inner
              Right (X.ETyInst inner' inst, generator')
            X.ERoll ty body -> do
              (body', generator') <- go generator env body
              Right (X.ERoll ty body', generator')
            X.EUnroll inner -> do
              (inner', generator') <- go generator env inner
              Right (X.EUnroll inner', generator')

    rewriteApplication generator env term =
      let (headTerm, args) = Reduce.collectApplicationSpineThroughHeadTypeRedexes term
       in case deferredPlaceholderHeadRefWithInsts headTerm of
            Just (ref, headInsts)
              | Just deferred <- lookupDeferredMethod ref -> do
                  (args', generator') <- mapAccumTerms generator env args
                  resolveDeferredApplication generator' env deferred headInsts args'
            _ ->
              case term of
                X.EApp fun arg -> do
                  (fun', generator1) <- go generator env fun
                  (arg', generator2) <- go generator1 env arg
                  Right (X.EApp fun' arg', generator2)
                _ -> Right (term, generator)

    mapAccumTerms generator _ [] = Right ([], generator)
    mapAccumTerms generator env (item : rest) = do
      (item', generator1) <- go generator env item
      (rest', generator2) <- mapAccumTerms generator1 env rest
      Right (item' : rest', generator2)

    resolveDeferredApplication generator env deferred headInsts args = do
      let methodInfo = deferredMethodInfo deferred
          requiredArgCount = deferredMethodResolutionArgCount deferred
      if length args < requiredArgCount
        then Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
        else do
          headSubst <-
            consumeDeferredMethodHeadInstantiations
              scope
              deferred
              headInsts
          let availableMethodArgCount =
                min
                  (deferredMethodTotalArgCount deferred)
                  (length args)
          argViews <-
            mapM
              (inferDeferredArgType env)
              (take availableMethodArgCount args)
          classArgView <-
            case inferDeferredMethodClassArgument methodInfo headSubst argViews (deferredMethodExpectedResult deferred) of
              Just view -> Right view
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          case lookupMethodEvidence deferred methodInfo classArgView of
            Just evidence -> do
              methodSubst <-
                case inferMethodApplicationSubst methodInfo classArgView headSubst argViews (deferredMethodExpectedResult deferred) of
                  Just subst' -> Right subst'
                  Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
              (evidenceHead, generator1) <- instantiateLocalMethodEvidenceWithSupply generator scope methodSubst evidence
              let methodLocalConstraintInfos = methodLocalConstraints methodInfo classArgView methodSubst
              (evidenceArgs, generator2) <-
                resolveConstraintEvidenceTermsWithSupply
                  generator1
                  scope
                  (deferredMethodLocalEvidence deferred)
                  []
                  methodLocalConstraintInfos
              Right (foldl X.EApp (foldl X.EApp evidenceHead evidenceArgs) args, generator2)
            Nothing -> do
              (instanceInfo, instanceSubst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
              methodValue <- concreteMethodValue instanceInfo methodInfo
              mergedSubst <-
                case mergeTypeViewSubstsInScope scope headSubst instanceSubst of
                  Just subst' -> Right subst'
                  Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
              methodSubst <-
                case inferMethodApplicationSubst methodInfo classArgView mergedSubst argViews (deferredMethodExpectedResult deferred) of
                  Just subst' -> Right subst'
                  Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
              let eagerConstraints =
                    map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue)
              let eagerConstraints' = filterConstraintGround eagerConstraints
              (evidenceArgs, generator1) <-
                resolveConstraintEvidenceTermsWithSupply generator scope (deferredMethodLocalEvidence deferred) [] eagerConstraints'
              (methodHead, generator2) <-
                instantiateMethodValueWithAliasViewsWithSupply generator1 scope [methodTypeView methodInfo] methodSubst methodValue
              Right (foldl X.EApp (foldl X.EApp methodHead evidenceArgs) args, generator2)

    resolveDeferredNullaryMethod generator headInsts deferred = do
      expectedView <-
        case deferredMethodExpectedResult deferred of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      let methodInfo = deferredMethodInfo deferred
      headSubst <-
        consumeDeferredMethodHeadInstantiations
          scope
          deferred
          headInsts
      classArgView <-
        case inferNullaryMethodClassArgument deferred headSubst expectedView of
          Just view -> Right view
          Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
      case lookupMethodEvidence deferred methodInfo classArgView of
        Just evidence -> do
          methodSubst <-
            case inferNullaryMethodSubst methodInfo classArgView headSubst expectedView of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          (evidenceHead, generator1) <- instantiateLocalMethodEvidenceWithSupply generator scope methodSubst evidence
          let methodLocalConstraintInfos = methodLocalConstraints methodInfo classArgView methodSubst
          (evidenceArgs, generator2) <-
            resolveConstraintEvidenceTermsWithSupply
              generator1
              scope
              (deferredMethodLocalEvidence deferred)
              []
              methodLocalConstraintInfos
          Right (foldl X.EApp evidenceHead evidenceArgs, generator2)
        Nothing -> do
          (instanceInfo, instanceSubst) <- resolveMethodInstanceInfoByTypeView scope methodInfo classArgView
          methodValue <- concreteMethodValue instanceInfo methodInfo
          mergedSubst <-
            case mergeTypeViewSubstsInScope scope headSubst instanceSubst of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          methodSubst <-
            case inferNullaryMethodSubst methodInfo classArgView mergedSubst expectedView of
              Just subst' -> Right subst'
              Nothing -> Left (ProgramAmbiguousMethodUse (deferredMethodName deferred))
          let eagerConstraints =
                map (applyConstraintInfoSubst methodSubst) (methodValueConstraints methodValue)
          let eagerConstraints' = filterConstraintGround eagerConstraints
          (evidenceArgs, generator1) <-
            resolveConstraintEvidenceTermsWithSupply generator scope (deferredMethodLocalEvidence deferred) [] eagerConstraints'
          (methodHead, generator2) <-
            instantiateMethodValueWithAliasViewsWithSupply generator1 scope [methodTypeView methodInfo] methodSubst methodValue
          Right (foldl X.EApp methodHead evidenceArgs, generator2)

    inferDeferredMethodClassArgument methodInfo subst argViews mbExpectedResult =
      inferDeferredMethodClassArgumentFromArgs methodInfo subst argViews
        <|> inferDeferredMethodClassArgumentFromExpected methodInfo subst argViews mbExpectedResult

    inferDeferredMethodClassArgumentFromArgs methodInfo initialSubst argViews = do
      let methodView = methodTypeView methodInfo
      subst <-
        foldM
          (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (templateView :| []) (actualView :| []))
          initialSubst
          (zip (methodParamTypeViews methodView) argViews)
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

    inferDeferredMethodClassArgumentFromExpected _ _ _ Nothing = Nothing
    inferDeferredMethodClassArgumentFromExpected methodInfo initialSubst argViews (Just expectedView) = do
      let methodView = methodTypeView methodInfo
      substFromArgs <-
        foldM
          (\acc (templateView, actualView) -> matchMethodTypeViews scope acc (templateView :| []) (actualView :| []))
          initialSubst
          (zip (methodParamTypeViews methodView) argViews)
      subst <- matchMethodTypeViews scope substFromArgs (methodResultTypeView methodInfo :| []) (expectedView :| [])
      NE.head <$> lookupMethodParamViewSubst methodInfo subst

    lookupMethodEvidence deferred methodInfo classArgView =
      case uniqueEvidenceMethod localMatches of
        Just methodEvidence ->
          Just (mkEvidence methodEvidence)
        Nothing ->
          case globalEvidence of
            Just methodEvidence -> Just (mkEvidence methodEvidence)
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
          [ methodEvidence
          | evidence <- deferredMethodLocalEvidence deferred,
            sameSymbolIdentity (evidenceClassSymbol evidence) (methodInfoOwnerClassSymbolIdentity methodInfo),
            rigidEvidenceTypeViewsMatch scope (evidenceTypeViews evidence) targetViews,
            methodEvidence <- maybe [] (: []) (lookupSymbolIdentityExact (methodInfoSymbolIdentity methodInfo) (evidenceMethodsByIdentity evidence))
          ]
        fallbackEvidence = do
          evidence <- deferredMethodEvidence deferred
          if rigidEvidenceTypeViewsMatch scope (deferredMethodEvidenceClassArgs evidence) targetViews
            then pure (evidence {deferredMethodEvidenceClassArg = classArgView, deferredMethodEvidenceClassArgs = targetViews})
            else Nothing

    methodLocalConstraints methodInfo classArgView methodSubst =
      map (applyConstraintInfoSubst methodSubst) methodLocal
      where
        headVars = freeTypeBinderIdentitiesTypeViews (classArgView :| [])
        methodLocal =
          filter
            (not . constraintDeterminedByTypeBinderIdentities headVars)
            specializedForClass
        classArgSubst =
          typeViewSubstFromParamIdentities
            (methodParamBinderIdentities methodInfo)
            (classArgView :| [])
        specializedForClass =
          map
            (applyConstraintInfoSubst classArgSubst)
            (methodConstraintInfos methodInfo)

    inferNullaryMethodClassArgument deferred initialSubst expectedView
      | deferredMethodTotalArgCount deferred /= 0 = Nothing
      | otherwise = do
          subst <- matchMethodTypeViews scope initialSubst (methodResultTypeView methodInfo :| []) (expectedView :| [])
          NE.head <$> lookupMethodParamViewSubst methodInfo subst
      where
        methodInfo = deferredMethodInfo deferred

    inferNullaryMethodSubst methodInfo classArgView subst expectedView =
      let specializedMethodView =
            specializeMethodTypeView methodInfo (classArgView :| [])
       in matchMethodTypeViews scope subst (methodResultTypeViewFrom specializedMethodView :| []) (expectedView :| [])

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

    inferMethodApplicationSubst methodInfo classArgView subst argViews mbExpectedResult = do
      substFromArgs <-
        inferMethodArgumentSubst
          methodInfo
          classArgView
          subst
          argViews
      case mbExpectedResult of
        Nothing -> Just substFromArgs
        Just expectedView ->
          let specializedMethodView =
                specializeMethodTypeView methodInfo (classArgView :| [])
           in matchMethodTypeViews
                scope
                substFromArgs
                (methodResultTypeViewFrom specializedMethodView :| [])
                (expectedView :| [])

consumeDeferredMethodHeadInstantiations ::
  ElaborateScope ->
  DeferredMethodCall ->
  [ElabType] ->
  Either ProgramError TypeViewSubst
consumeDeferredMethodHeadInstantiations scope deferred =
  go Map.empty (deferredMethodInstBinders deferred)
  where
    methodInfo = deferredMethodInfo deferred

    go subst _ [] =
      Right subst
    go _ [] (_ : _) =
      Left (ProgramAmbiguousMethodUse (methodName methodInfo))
    go subst ((_, identity) : binders) (instTy : instTys) = do
      let instView =
            elabTypeToRecoveredTypeView
              scope
              (stripVacuousForalls instTy)
      subst' <-
        maybe
          (Left (ProgramAmbiguousMethodUse (methodName methodInfo)))
          Right
          (bindTypeViewSubstInScope scope identity instView subst)
      go subst' binders instTys

bindTypeViewSubstInScope ::
  ElaborateScope ->
  TypeBinderIdentity ->
  TypeView ->
  TypeViewSubst ->
  Maybe TypeViewSubst
bindTypeViewSubstInScope scope identity actualView subst =
  case lookupTypeViewSubst identity subst of
    Nothing ->
      Just (Map.insert identity actualView subst)
    Just existingView
      | typeViewIsBareBinderIdentity identity existingView ->
          Just (Map.insert identity actualView subst)
      | semanticTypeViewsMatchInScope scope existingView actualView ->
          Just subst
      | otherwise ->
          Nothing

mergeTypeViewSubstsInScope ::
  ElaborateScope ->
  TypeViewSubst ->
  TypeViewSubst ->
  Maybe TypeViewSubst
mergeTypeViewSubstsInScope scope initialSubst incomingSubst =
  foldM
    (\subst (identity, view) -> bindTypeViewSubstInScope scope identity view subst)
    initialSubst
    (Map.toList incomingSubst)

-- | Owner-local seam for ordered method-head construction.
consumeDeferredMethodHeadInstantiationsForTest ::
  ElaborateScope ->
  DeferredMethodCall ->
  [ElabType] ->
  Either ProgramError TypeViewSubst
consumeDeferredMethodHeadInstantiationsForTest =
  consumeDeferredMethodHeadInstantiations

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
      let eagerConstraints' = filterConstraintGround eagerConstraints
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
                      let instantiate methodEvidence =
                            instantiateLocalMethodEvidence
                              scope
                              Map.empty
                              DeferredMethodEvidence
                                { deferredMethodEvidenceClassArg = constraintTypeView constraint,
                                  deferredMethodEvidenceClassArgs = constraintTypeViews constraint,
                                  deferredMethodEvidenceMethod = methodEvidence
                                }
                      case lookupEvidenceMethodMatch scope localEvidence (constraintClassSymbol constraint) (constraintTypeViews constraint) (methodInfoSymbolIdentity methodInfo) of
                        Just methodEvidence ->
                          Just <$> instantiate methodEvidence
                        Nothing -> do
                          case lookupEvidenceMethodByClassViews scope (constraintClassSymbol constraint) (constraintTypeViews constraint) (methodInfoSymbolIdentity methodInfo) of
                            Just methodEvidence -> Just <$> instantiate methodEvidence
                            Nothing -> Right Nothing
                  )
                  (Map.elems (classMethodsByIdentity classInfo))
          evidenceTerms <- localMethodEvidence
          case sequence evidenceTerms of
            Nothing -> Right Nothing
            Just terms ->
              Right (Just terms)

resolveConstraintEvidenceTermsWithSupply :: Maybe IdentityGenerator -> ElaborateScope -> [EvidenceInfo] -> [ClassApplicationKey] -> [ConstraintInfo] -> Either ProgramError ([XmlfTerm], Maybe IdentityGenerator)
resolveConstraintEvidenceTermsWithSupply mbGenerator scope localEvidence seen constraints =
  case mbGenerator of
    Nothing -> do
      terms <- resolveConstraintEvidenceTerms scope localEvidence seen constraints
      Right (terms, Nothing)
    Just generator -> do
      (terms, generator') <- resolveConstraintEvidenceTermsFromSupply generator scope localEvidence seen constraints
      Right (terms, Just generator')

resolveConstraintEvidenceTermsFromSupply :: IdentityGenerator -> ElaborateScope -> [EvidenceInfo] -> [ClassApplicationKey] -> [ConstraintInfo] -> Either ProgramError ([XmlfTerm], IdentityGenerator)
resolveConstraintEvidenceTermsFromSupply generator _ _ _ [] =
  Right ([], generator)
resolveConstraintEvidenceTermsFromSupply generator scope localEvidence seen (constraint : constraints) = do
  (terms, generator1) <-
    resolveConstraintEvidenceTermFromSupply generator scope localEvidence seen constraint
  (rest, generator2) <-
    resolveConstraintEvidenceTermsFromSupply generator1 scope localEvidence seen constraints
  Right (terms ++ rest, generator2)

resolveConstraintEvidenceTermFromSupply :: IdentityGenerator -> ElaborateScope -> [EvidenceInfo] -> [ClassApplicationKey] -> ConstraintInfo -> Either ProgramError ([XmlfTerm], IdentityGenerator)
resolveConstraintEvidenceTermFromSupply generator scope localEvidence seen constraint = do
  let key = constraintEvidenceKey constraint
  if key `elem` seen
    then Left (noMatchingInstanceError scope constraint)
    else do
      (mbLocalEvidence, generator1) <-
        resolveLocalConstraintEvidenceTermsFromSupply generator scope localEvidence constraint
      case mbLocalEvidence of
        Just evidenceTerms -> Right (evidenceTerms, generator1)
        Nothing -> do
          (instanceInfo, subst) <- resolveInstanceInfoByConstraint scope constraint
          let seen' = key : seen
              methodValues = ordinaryInstanceMethods instanceInfo
          if null methodValues
            then do
              (_, generator2) <-
                resolveConstraintEvidenceTermsFromSupply
                  generator1
                  scope
                  localEvidence
                  seen'
                  (map (applyConstraintInfoSubst subst) (instanceConstraintInfos instanceInfo))
              Right ([], generator2)
            else materializeMethodEvidenceTerms generator1 seen' subst methodValues
  where
    ordinaryInstanceMethods instanceInfo =
      [valueInfo | valueInfo@OrdinaryValue {} <- Map.elems (instanceMethodsByIdentity instanceInfo)]

    materializeMethodEvidenceTerms generator0 _ _ [] =
      Right ([], generator0)
    materializeMethodEvidenceTerms generator0 seen' subst (valueInfo : rest) = do
      let eagerConstraints =
            map (applyConstraintInfoSubst subst) (methodValueConstraints valueInfo)
          eagerConstraints' = filterConstraintGround eagerConstraints
      (nestedEvidence, generator1) <-
        resolveConstraintEvidenceTermsFromSupply
          generator0
          scope
          localEvidence
          seen'
          eagerConstraints'
      (methodHead, generator2) <-
        instantiateMethodValueFromSupply generator1 scope subst valueInfo
      (restTerms, generator3) <-
        materializeMethodEvidenceTerms generator2 seen' subst rest
      Right (foldl X.EApp methodHead nestedEvidence : restTerms, generator3)

resolveLocalConstraintEvidenceTermsFromSupply :: IdentityGenerator -> ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Either ProgramError (Maybe [XmlfTerm], IdentityGenerator)
resolveLocalConstraintEvidenceTermsFromSupply generator scope localEvidence constraint =
  case classInfoForConstraint scope constraint of
    Nothing -> Right (Nothing, generator)
    Just classInfo
      | Map.null (classMethodsByIdentity classInfo) ->
          Right
            ( if zeroMethodConstraintCoveredByEvidenceInfo scope constraint
                || zeroMethodConstraintCoveredByEvidence scope localEvidence constraint
                then Just []
                else Nothing,
              generator
            )
      | otherwise ->
          collectLocalMethodEvidence generator (Map.elems (classMethodsByIdentity classInfo))
  where
    collectLocalMethodEvidence generator0 [] =
      Right (Just [], generator0)
    collectLocalMethodEvidence generator0 (methodInfo : rest) =
      case matchingMethodEvidence methodInfo of
        Nothing -> Right (Nothing, generator0)
        Just methodEvidence -> do
          (term, generator1) <-
            instantiateLocalMethodEvidenceFromSupply
              generator0
              scope
              Map.empty
              DeferredMethodEvidence
                { deferredMethodEvidenceClassArg = constraintTypeView constraint,
                  deferredMethodEvidenceClassArgs = constraintTypeViews constraint,
                  deferredMethodEvidenceMethod = methodEvidence
                }
          (mbRest, generator2) <- collectLocalMethodEvidence generator1 rest
          Right ((term :) <$> mbRest, generator2)

    matchingMethodEvidence methodInfo =
      lookupEvidenceMethodMatch
        scope
        localEvidence
        (constraintClassSymbol constraint)
        (constraintTypeViews constraint)
        (methodInfoSymbolIdentity methodInfo)
        <|> lookupEvidenceMethodByClassViews
          scope
          (constraintClassSymbol constraint)
          (constraintTypeViews constraint)
          (methodInfoSymbolIdentity methodInfo)

lookupEvidenceMethodMatch :: ElaborateScope -> [EvidenceInfo] -> SymbolIdentity -> NonEmpty TypeView -> SymbolIdentity -> Maybe EvidenceMethod
lookupEvidenceMethodMatch scope evidenceInfos classIdentity headViews methodIdentity =
  uniqueEvidenceMethod
    [ methodEvidence
      | evidence <- evidenceInfos,
        sameSymbolIdentity (evidenceClassSymbol evidence) classIdentity,
        rigidEvidenceTypeViewsMatch scope (evidenceTypeViews evidence) headViews,
        methodEvidence <- maybe [] (: []) (lookupSymbolIdentityExact methodIdentity (evidenceMethodsByIdentity evidence))
    ]

zeroMethodConstraintCoveredByEvidence :: ElaborateScope -> [EvidenceInfo] -> ConstraintInfo -> Bool
zeroMethodConstraintCoveredByEvidence scope evidenceInfos constraint =
  any
    ( \evidence ->
        sameSymbolIdentity (evidenceClassSymbol evidence) (constraintClassSymbol constraint)
          && rigidEvidenceTypeViewsMatch
            scope
            (evidenceTypeViews evidence)
            (constraintTypeViews constraint)
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

instantiateLocalMethodEvidence :: ElaborateScope -> TypeViewSubst -> DeferredMethodEvidence -> Either ProgramError XmlfTerm
instantiateLocalMethodEvidence scope subst DeferredMethodEvidence {deferredMethodEvidenceMethod = methodEvidence} = do
  let resolved = evidenceMethodResolvedVar methodEvidence
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

instantiateLocalMethodEvidenceWithSupply :: Maybe IdentityGenerator -> ElaborateScope -> TypeViewSubst -> DeferredMethodEvidence -> Either ProgramError (XmlfTerm, Maybe IdentityGenerator)
instantiateLocalMethodEvidenceWithSupply mbGenerator scope subst evidence =
  case mbGenerator of
    Nothing -> do
      term <- instantiateLocalMethodEvidence scope subst evidence
      Right (term, Nothing)
    Just generator -> do
      (term, generator') <- instantiateLocalMethodEvidenceFromSupply generator scope subst evidence
      Right (term, Just generator')

instantiateLocalMethodEvidenceFromSupply :: IdentityGenerator -> ElaborateScope -> TypeViewSubst -> DeferredMethodEvidence -> Either ProgramError (XmlfTerm, IdentityGenerator)
instantiateLocalMethodEvidenceFromSupply generator scope subst DeferredMethodEvidence {deferredMethodEvidenceMethod = methodEvidence} = do
  let resolved = evidenceMethodResolvedVar methodEvidence
      foralls =
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
  let (resolved', generator') =
        freshenResolvedVarTypeAgainstInstantiationsFromSupply
          generator
          (instantiationTypes instantiations)
          resolved
      methodTerm = X.EVarNode resolved'
  Right (foldl X.ETyInst methodTerm instantiations, generator')

constraintDeterminedByTypeBinderIdentities :: Set TypeBinderIdentity -> ConstraintInfo -> Bool
constraintDeterminedByTypeBinderIdentities typeVars constraint =
  freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint)
    `Set.isSubsetOf` typeVars

constraintGround :: ConstraintInfo -> Bool
constraintGround constraint =
  Set.null (freeTypeBinderIdentitiesTypeViews (constraintTypeViews constraint))

filterConstraintGround :: [ConstraintInfo] -> [ConstraintInfo]
filterConstraintGround =
  filter constraintGround

methodValueConstraints :: ValueInfo -> [ConstraintInfo]
methodValueConstraints OrdinaryValue {valueConstraintInfos = constraints} = constraints
methodValueConstraints _ = []

instantiateMethodValue :: ElaborateScope -> TypeViewSubst -> ValueInfo -> Either ProgramError XmlfTerm
instantiateMethodValue scope =
  instantiateMethodValueWithAliasViews scope []

instantiateMethodValueFromSupply :: IdentityGenerator -> ElaborateScope -> TypeViewSubst -> ValueInfo -> Either ProgramError (XmlfTerm, IdentityGenerator)
instantiateMethodValueFromSupply generator scope =
  instantiateMethodValueWithAliasViewsFromSupply generator scope []

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

instantiateMethodValueWithAliasViewsWithSupply :: Maybe IdentityGenerator -> ElaborateScope -> [TypeView] -> TypeViewSubst -> ValueInfo -> Either ProgramError (XmlfTerm, Maybe IdentityGenerator)
instantiateMethodValueWithAliasViewsWithSupply mbGenerator scope aliasViews subst valueInfo =
  case mbGenerator of
    Nothing -> do
      term <- instantiateMethodValueWithAliasViews scope aliasViews subst valueInfo
      Right (term, Nothing)
    Just generator -> do
      (term, generator') <-
        instantiateMethodValueWithAliasViewsFromSupply generator scope aliasViews subst valueInfo
      Right (term, Just generator')

instantiateMethodValueWithAliasViewsFromSupply :: IdentityGenerator -> ElaborateScope -> [TypeView] -> TypeViewSubst -> ValueInfo -> Either ProgramError (XmlfTerm, IdentityGenerator)
instantiateMethodValueWithAliasViewsFromSupply generator scope aliasViews subst valueInfo@OrdinaryValue {} = do
  let sourceView = ordinaryValueTypeView valueInfo
      substViews = sourceView : aliasViews
  resolved <- resolvedVarFromValueInfo valueInfo <$> typeViewToElabType scope sourceView
  let foralls = resolvedForallsMatchingSourceOrAliasSubst subst substViews (X.resolvedVarType resolved) sourceView
  instantiations <- methodForallInstantiationsFromAliasSubst scope substViews subst sourceView foralls
  let (resolved', generator') =
        freshenResolvedVarTypeAgainstInstantiationsFromSupply
          generator
          (instantiationTypes instantiations)
          resolved
  Right (foldl X.ETyInst (X.EVarNode resolved') instantiations, generator')
instantiateMethodValueWithAliasViewsFromSupply generator scope _ _ valueInfo@ConstructorValue {} = do
  term <- X.EVarNode . resolvedVarFromValueInfo valueInfo <$> typeViewToElabType scope (constructorTypeView scope (valueCtorInfo valueInfo))
  Right (term, advanceIdentityGeneratorPastMany (X.generatedIdentitiesInTerm term) generator)
instantiateMethodValueWithAliasViewsFromSupply _ _ _ _ OverloadedMethod {} =
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

freshenResolvedVarTypeAgainstInstantiationsFromSupply :: IdentityGenerator -> [ElabType] -> X.ResolvedVar -> (X.ResolvedVar, IdentityGenerator)
freshenResolvedVarTypeAgainstInstantiationsFromSupply generator instTys resolved =
  (X.mapResolvedVarType (const ty') resolved, generator')
  where
    (ty', generator') =
      freshenElabTypeBindersAgainstTypesFromSupply generator instTys (X.resolvedVarType resolved)

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
sourceViewForallCount =
  length . typeViewForallBinderViews

resolvedForallCandidateNames :: TypeView -> Int -> X.TypeBinderRef -> [String]
resolvedForallCandidateNames sourceView index ref =
  dedupe
    ( sourceDisplayName
        ++ sourceIdentityName
        ++ [X.typeBinderRefName ref, elabTypeBinderIdentityName ref]
    )
  where
    sourceDisplayName = maybe [] (pure . forallDisplayName) sourceBinder
    sourceIdentityName = maybe [] (pure . typeBinderIdentityStableName . forallIdentity) sourceBinder
    sourceBinder = atIndex index (typeViewForallBinderViews sourceView)

    forallDisplayName (name, _, _) = name
    forallIdentity (_, identity, _) = identity

    atIndex target = go 0
      where
        go _ [] = Nothing
        go current (value : rest)
          | current == target = Just value
          | otherwise = go (current + 1) rest

    dedupe = go []
      where
        go _ [] = []
        go seen (name : names)
          | name `elem` seen = go seen names
          | otherwise = name : go (name : seen) names

deferredPlaceholderHeadRefWithInsts :: XmlfTerm -> Maybe (DeferredRef, [ElabType])
deferredPlaceholderHeadRefWithInsts = go []
  where
    go insts term =
      case term of
        X.EVarNode resolved -> fmap (\ref -> (ref, insts)) (X.deferredResolvedVarRef resolved)
        X.ETyInst inner inst -> do
          currentInsts <- orderedInstAppTypes inst
          go (currentInsts ++ insts) inner
        _ -> Nothing

orderedInstAppTypes :: X.Instantiation -> Maybe [ElabType]
orderedInstAppTypes inst =
  case inst of
    X.InstApp ty -> Just [ty]
    X.InstSeq left right ->
      (++) <$> orderedInstAppTypes left <*> orderedInstAppTypes right
    _ -> Nothing

-- Deferred case occurrences can carry several ordered type applications in
-- one composed computation.  Accept only an all-InstApp computation spine:
-- other computations retain their position and are handled by ordinary
-- traversal instead of being silently consumed by case reconstruction.
deferredCasePlaceholderHeadRefWithInsts :: XmlfTerm -> Maybe (DeferredRef, [ElabType])
deferredCasePlaceholderHeadRefWithInsts = go []
  where
    go insts term =
      case term of
        X.EVarNode resolved ->
          fmap (\ref -> (ref, insts)) (X.deferredResolvedVarRef resolved)
        X.ETyInst inner inst -> do
          currentInsts <- orderedInstAppTypes inst
          go (currentInsts ++ insts) inner
        _ -> Nothing

resolvedVarFromConstructorInfo :: ElaborateScope -> ConstructorInfo -> Either ProgramError X.ResolvedVar
resolvedVarFromConstructorInfo scope ctorInfo = do
  constructorTy <-
    typeViewToElabType
      scope
      (constructorBindingSourceTypeView scope ctorInfo)
  pure
    X.ResolvedVar
      { X.resolvedVarType = constructorTy,
        X.resolvedVarDetails = ConstructorId (constructorRefFromInfo ctorInfo)
      }

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
data StructuralOwnerRecovery = StructuralOwnerRecovery
  { recoverOwnerBinderIdentities :: Map String TypeBinderIdentity,
    recoverOwnerHeadIdentities :: Map String SymbolIdentity
  }

-- Matching happens after the nominal owner has been selected. Checked
-- substitutions are still keyed by the carried binder identity; the display
-- name is retained only for the final SrcType adapter used by diagnostics and
-- the deferred-case environment.
data RecoverBinderKey = RecoverBinderIdentity TypeBinderIdentity String

instance Eq RecoverBinderKey where
  RecoverBinderIdentity left _ == RecoverBinderIdentity right _ = left == right

instance Ord RecoverBinderKey where
  compare (RecoverBinderIdentity leftIdentity _) (RecoverBinderIdentity rightIdentity _) =
    compare leftIdentity rightIdentity

recoverBinderDisplayName :: RecoverBinderKey -> String
recoverBinderDisplayName key =
  case key of
    RecoverBinderIdentity _ name -> name

data RecoverHeadIdentityContext = RecoverHeadIdentityContext
  { recoverExpectedHeadIdentities :: Map String SymbolIdentity,
    recoverActualHeadIdentities :: Map String SymbolIdentity
  }

recoverElabSourceType :: ElaborateScope -> X.Ty v -> SrcType
recoverElabSourceType scope ty =
  recoverSourceTypeWith
    StructuralOwnerRecovery
      { recoverOwnerBinderIdentities = elabTypeBinderIdentities ty,
        recoverOwnerHeadIdentities = elabTypeHeadIdentities ty
      }
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
      case ty of
        STMu selfName _ ->
          maybe [] (: []) (structuralOwnerDataInfo (recoverOwnerBinderIdentities ownerRecovery) selfName)
        _ -> []

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
        StructuralOwnerRecovery
          { recoverOwnerBinderIdentities = elabTypeBinderIdentities ty,
            recoverOwnerHeadIdentities = elabTypeHeadIdentities ty
          }
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
          recoverActualHeadIdentities = recoverOwnerHeadIdentities ownerRecovery
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
                      sourceTypeHeadIdentitiesInScope scope loweredTemplate
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
            RecoverBinderIdentity <$> Map.lookup name templateBinderIdentities <*> pure name
          recoverParams =
            Map.fromList
              [ (param, key)
              | param <- params,
                Just key <- [recoverBinderKey param]
              ]
          matchTemplate template =
            matchRecoverType scope headIdentities recoverBinderKey recoverParams Map.empty Map.empty template ty
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
                  -- The matching alias selects a template; it does not own the
                  -- recovered type. Keep the selected nominal owner in the
                  -- reconstructed head and choose a visible spelling later.
                  recoveredHeadName = symbolIdentityStableName (dataInfoSymbol info)
                  recoveredHead =
                    case recoveredArgs of
                      [] -> STBase recoveredHeadName
                      arg : args -> STCon recoveredHeadName (arg :| args)
                  namedSubst =
                    Map.fromList
                      [ (recoverBinderDisplayName key, matchedTy)
                      | (key, matchedTy) <- Map.toList subst
                      ]
               in Just (recoveredHead, namedSubst)
            Nothing -> Nothing

matchRecoverType ::
  ElaborateScope ->
  RecoverHeadIdentityContext ->
  (String -> Maybe RecoverBinderKey) ->
  Map String RecoverBinderKey ->
  Map RecoverBinderKey SrcType ->
  Map String String ->
  SrcType ->
  SrcType ->
  Maybe (Map RecoverBinderKey SrcType)
matchRecoverType scope headIdentities recoverBinderKey params subst renames template actual =
  case template of
    STVar name
      | Just key <- Map.lookup name params ->
          bindRecoverParam scope headIdentities key actual subst
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
          subst' <- matchRecoverType scope headIdentities recoverBinderKey params subst renames dom dom'
          matchRecoverType scope headIdentities recoverBinderKey params subst' renames cod cod'
        _ -> Nothing
    STBase name ->
      case actual of
        STBase name' | recoverTypeHeadMatches headIdentities name name' -> Just subst
        _ -> Nothing
    STCon name args ->
      case actual of
        STCon name' args'
          | recoverTypeHeadMatches headIdentities name name' && length (toListNE args) == length (toListNE args') ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType scope headIdentities recoverBinderKey params acc renames leftTy rightTy)
                subst
                (zip (toListNE args) (toListNE args'))
        _ -> Nothing
    STVarApp name args ->
      matchRecoverVarApp scope headIdentities recoverBinderKey params subst renames name args actual
    STTyLam name body ->
      case actual of
        STTyLam name' body' ->
          matchRecoverType scope headIdentities recoverBinderKey params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STTyApp fun arg ->
      case actual of
        STTyApp fun' arg' -> do
          subst' <- matchRecoverType scope headIdentities recoverBinderKey params subst renames fun fun'
          matchRecoverType scope headIdentities recoverBinderKey params subst' renames arg arg'
        _ -> Nothing
    STForall name _mb body ->
      case actual of
        STForall name' _mb' body' ->
          matchRecoverType scope headIdentities recoverBinderKey params subst (Map.insert name name' renames) body body'
        _ -> do
          key <- recoverBinderKey name
          matchRecoverType scope headIdentities recoverBinderKey (Map.insert name key params) subst renames body actual
    STMu name body ->
      case actual of
        STMu name' body' ->
          matchRecoverType scope headIdentities recoverBinderKey params subst (Map.insert name name' renames) body body'
        _ -> Nothing
    STBottom ->
      case actual of
        STBottom -> Just subst
        _ -> Nothing

recoverTypeHeadMatches :: RecoverHeadIdentityContext -> String -> String -> Bool
recoverTypeHeadMatches headIdentities expected actual =
  case (resolveExpectedHead expected, resolveActualHead actual) of
    (Just expectedIdentity, Just actualIdentity) -> sameSymbolIdentity expectedIdentity actualIdentity
    _ -> False
  where
    resolveExpectedHead name =
      lookupSymbolIdentityAlias (recoverExpectedHeadIdentities headIdentities) name

    resolveActualHead name =
      lookupSymbolIdentityAlias (recoverActualHeadIdentities headIdentities) name

matchRecoverVarApp ::
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
matchRecoverVarApp scope headIdentities recoverBinderKey params subst renames name args actual
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
          subst' <- bindRecoverParam scope headIdentities key (headFromPrefix actualName headArgs) subst
          foldM
            (\acc (leftTy, rightTy) -> matchRecoverType scope headIdentities recoverBinderKey params acc renames leftTy rightTy)
            subst'
            (zip expectedArgs appliedArgs)

    matchRigidVarAppHead expectedName =
      case actual of
        STVarApp actualName actualArgs
          | recoverTypeHeadMatches headIdentities expectedName actualName && expectedArgCount == length (toListNE actualArgs) ->
              foldM
                (\acc (leftTy, rightTy) -> matchRecoverType scope headIdentities recoverBinderKey params acc renames leftTy rightTy)
                subst
                (zip expectedArgs (toListNE actualArgs))
        _ -> Nothing

    toConHead actualName [] = STBase actualName
    toConHead actualName (arg : rest) = STCon actualName (arg :| rest)

    toVarHead actualName [] = STVar actualName
    toVarHead actualName (arg : rest) = STVarApp actualName (arg :| rest)

bindRecoverParam :: ElaborateScope -> RecoverHeadIdentityContext -> RecoverBinderKey -> SrcType -> Map RecoverBinderKey SrcType -> Maybe (Map RecoverBinderKey SrcType)
bindRecoverParam scope headIdentities key actual subst =
  case Map.lookup key subst of
    Nothing -> Just (Map.insert key actual subst)
    Just existing
      | Just existingTy <- srcTypeToElabTypeMaybeInScopeWithHeadIdentities scope combinedHeadIdentities existing,
        Just actualTy <- srcTypeToElabTypeMaybeInScopeWithHeadIdentities scope combinedHeadIdentities actual,
        checkedHeadsComplete existing && checkedHeadsComplete actual,
        alphaEqType existingTy actualTy || churchAwareEqType existingTy actualTy ->
          Just subst
      | otherwise -> Nothing
  where
    combinedHeadIdentities = recoverActualHeadIdentities headIdentities

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

surfaceFreeBindingReferences :: ResolvedSurfaceExpr -> [SurfaceBindingReference]
surfaceFreeBindingReferences = Set.toAscList . go Set.empty
  where
    go :: Set BindingKey -> ResolvedSurfaceExpr -> Set SurfaceBindingReference
    go bound expr = case expr of
      EVarNode reference ->
        freeReference bound reference
      ELit _ -> Set.empty
      ELamNode reference body -> go (Set.insert (bindingKeyForTermReference reference) bound) body
      ELamAnnNode reference _ body -> go (Set.insert (bindingKeyForTermReference reference) bound) body
      EExactLamNode reference _ body -> go (Set.insert (bindingKeyForTermReference reference) bound) body
      EApp fun arg -> go bound fun `Set.union` go bound arg
      ELetNode reference rhs body ->
        let bound' = Set.insert (bindingKeyForTermReference reference) bound
         in go bound' rhs `Set.union` go bound' body
      EAnn inner _ -> go bound inner
      EExactAnn inner _ _ -> go bound inner

    freeReference bound reference
      | key `Set.member` bound = Set.empty
      | otherwise = Set.singleton (surfaceBindingReferenceFromTermReference reference)
      where
        key = bindingKeyForTermReference reference

toListNE :: NonEmpty a -> [a]
toListNE (x :| xs) = x : xs

elabTypeToRecoveredTypeView :: ElaborateScope -> X.Ty v -> TypeView
elabTypeToRecoveredTypeView scope ty =
  -- Recovery can replace a Church encoding with its nominal source head.
  -- Overlay stable heads before choosing display names so same-spelled data
  -- types never need to be resolved from an ambiguous string.
  canonicalizeRecoveredTypeView scope $
    typeViewOverlayDisplay recoveredSubtree recoveredTy semanticView
  where
    recoveredTy =
      recoverElabSourceType scope ty
    semanticView =
      typeViewWithIdentityAliases
        headIdentities
        binderIdentities
        (typeViewFromElabType ty)
    recoveredSubtree sourceTy =
      requireTypeViewFromSourceTypeInScope
        scope
        headIdentities
        (binderIdentities `Map.union` recoveredDataParamBinderIdentities scope headIdentities sourceTy)
        sourceTy
    headIdentities =
      mergeSymbolIdentityMaps
        [ elabTypeHeadIdentities ty,
          sourceTypeHeadIdentitiesInScope scope recoveredTy
        ]
    binderIdentities =
      elabTypeBinderIdentities ty
        `Map.union` mergeTypeBinderIdentityMaps
          [ recoveredElabTypeBinderIdentities recoveredTy ty,
            recoveredDataParamBinderIdentities scope headIdentities recoveredTy
          ]

canonicalizeRecoveredTypeView :: ElaborateScope -> TypeView -> TypeView
canonicalizeRecoveredTypeView scope =
  mapTypeViewDisplayHeadNames canonicalName
  where
    canonicalName identity displayName =
      case lookupDataInfo identity of
        Just dataInfo -> recoveredDataDisplayName scope dataInfo
        Nothing -> displayName

    lookupDataInfo identity =
      lookupSymbolIdentityExact identity dataTypesByIdentity
        <|> case
          [ dataInfo
          | dataInfo <- Map.elems (elaborateScopeDataTypes scope),
            sameSymbolIdentity (dataInfoSymbol dataInfo) identity
          ] of
          dataInfo : rest
            | all (sameSymbolIdentity (dataInfoSymbol dataInfo) . dataInfoSymbol) rest -> Just dataInfo
          _ -> Nothing

    dataTypesByIdentity =
      elaborateScopeDataTypesByIdentity scope

recoveredDataDisplayName :: ElaborateScope -> DataInfo -> String
recoveredDataDisplayName scope dataInfo
  | visibleAs definingName = definingName
  | visibleAs qualifiedName = qualifiedName
  | otherwise =
      case
        [ name
        | (name, candidate) <- Map.toList (elaborateScopeDataTypes scope),
          sameSymbolIdentity (dataInfoSymbol candidate) identity,
          case name of
            '$' : _ -> False
            _ -> True
        ] of
        name : _ -> name
        [] -> qualifiedName
  where
    identity = dataInfoSymbol dataInfo
    definingName = dataInfoIdentityName dataInfo
    qualifiedName = dataInfoIdentityQualifiedName dataInfo

    visibleAs name =
      case Map.lookup name (elaborateScopeDataTypes scope) of
        Just candidate -> sameSymbolIdentity (dataInfoSymbol candidate) identity
        Nothing -> False

recoveredDataParamBinderIdentities :: ElaborateScope -> Map String SymbolIdentity -> SrcType -> Map String TypeBinderIdentity
recoveredDataParamBinderIdentities scope headIdentities =
  go
  where
    go sourceTy =
      case sourceTy of
        STVar {} -> Map.empty
        STArrow dom cod -> mergeTypeBinderIdentityMaps [go dom, go cod]
        STBase {} -> Map.empty
        STCon name args ->
          mergeTypeBinderIdentityMaps
            (dataParamIdentities name (NE.toList args) : map go (NE.toList args))
        STVarApp _ args -> mergeTypeBinderIdentityMaps (map go (NE.toList args))
        STTyLam _ body -> go body
        STTyApp fun arg -> mergeTypeBinderIdentityMaps [go fun, go arg]
        STForall _ mbBound body ->
          mergeTypeBinderIdentityMaps [foldMap (go . unSrcBound) mbBound, go body]
        STMu _ body -> go body
        STBottom -> Map.empty

    dataParamIdentities name args =
      case dataInfoForHead name of
        Just dataInfo ->
          mergeTypeBinderIdentityMaps
            (zipWith bindArgument (dataParamBinders dataInfo) args)
        Nothing -> Map.empty

    dataInfoForHead name = do
      identity <- lookupSymbolIdentityAlias headIdentities name
      lookupSymbolIdentityExact identity (elaborateScopeDataTypesByIdentity scope)

    bindArgument (_, identity) argument =
      case argument of
        STVar name -> typeBinderAliasIdentityMap [(name, identity)]
        STVarApp name _ -> typeBinderAliasIdentityMap [(name, identity)]
        _ -> Map.empty

recoveredElabTypeBinderIdentities :: SrcType -> X.Ty v -> Map String TypeBinderIdentity
recoveredElabTypeBinderIdentities =
  go
  where
    go :: SrcType -> X.Ty a -> Map String TypeBinderIdentity
    go sourceTy elabTy =
      case (sourceTy, elabTy) of
        (STVar name, X.TVarRef ref) -> binder name ref
        (STArrow sourceDom sourceCod, X.TArrow elabDom elabCod) ->
          mergeTypeBinderIdentityMaps [go sourceDom elabDom, go sourceCod elabCod]
        (STBase {}, X.TBaseWithIdentity {}) -> Map.empty
        (STCon _ sourceArgs, X.TConWithIdentity _ _ elabArgs) ->
          mergeTypeBinderIdentityMaps (zipWith go (NE.toList sourceArgs) (NE.toList elabArgs))
        (STVarApp name sourceArgs, X.TVarAppRef ref elabArgs) ->
          mergeTypeBinderIdentityMaps
            (binder name ref : zipWith go (NE.toList sourceArgs) (NE.toList elabArgs))
        (STForall name sourceBound sourceBody, X.TForallRef ref elabBound elabBody) ->
          mergeTypeBinderIdentityMaps
            [ binder name ref,
              maybe Map.empty (uncurry go) (zipBounds sourceBound elabBound),
              go sourceBody elabBody
            ]
        (STMu name sourceBody, X.TMuRef ref elabBody) ->
          mergeTypeBinderIdentityMaps [binder name ref, go sourceBody elabBody]
        (STBottom, X.TBottom) -> Map.empty
        _ -> Map.empty

    binder name ref =
      typeBinderAliasIdentityMap
        [ (name, X.typeBinderRefIdentity ref),
          (X.typeBinderRefName ref, X.typeBinderRefIdentity ref)
        ]

    zipBounds (Just (SrcBound sourceBound)) (Just elabBound) =
      Just (sourceBound, elabBound)
    zipBounds _ _ = Nothing

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

    identityHead (Graph.BaseTy name) identity =
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

elabTypeToSrcTypeWithHeads :: (X.TypeBinderRef -> String) -> (SymbolIdentity -> String -> String) -> X.Ty v -> SrcType
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

typeViewToElabType :: ElaborateScope -> TypeView -> Either ProgramError ElabType
typeViewToElabType scope view =
  resolvedTypeViewToElabType (lowerTypeViewWithIdentities scope view)

resolvedTypeViewToElabType :: TypeView -> Either ProgramError ElabType
resolvedTypeViewToElabType =
  either (Left . ProgramPipelineError) Right
    . resolvedSourceTypeToElabType
    . typeViewToResolved

loweredExpectedTypeToElabType :: ElaborateScope -> LoweredBinding -> Either ProgramError ElabType
loweredExpectedTypeToElabType scope =
  typeViewToElabType scope . loweredBindingExpectedTypeView

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
  STBase name -> do
    identity <- sourceTypeHeadIdentity name
    Right (X.TBaseWithIdentity identity (Graph.BaseTy (Builtins.normalizeBuiltinTypeReference name)), generator)
  STCon name args ->
    do
      (args', generator') <- mapAccumSrcTypes refs generator args
      identity <- sourceTypeHeadIdentity name
      Right (X.TConWithIdentity identity (Graph.BaseTy (Builtins.normalizeBuiltinTypeReference name)) args', generator')
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
      case resolveHead name <|> lookupSymbolIdentityAlias headIdentities name of
        Just identity -> Right identity
        Nothing -> Left (ProgramPipelineError ("unresolved source type head `" ++ name ++ "` reached finalization"))

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
