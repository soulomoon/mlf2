{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Pipeline
  ( runPipelineElab,
    runPipelineElabWithConfig,
    runPipelineElabWithEnv,
    runPipelineElabWithConfigAndEnv,
    PipelineElabDetailedResult (..),
    PreparedExternalBindings,
    prepareExternalBindings,
    restrictPreparedExternalBindings,
    unionPreparedExternalBindings,
    runPipelineElabDetailedWithEnv,
    runPipelineElabDetailedWithConfigAndEnv,
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedWithConfigAndExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedUncheckedWithExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming,
    freshenTypeAbsAgainstEnv,
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, rtsSupportsBoundThreads, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities)
import MLF.Constraint.Acyclicity (breakCyclesAndCheckAcyclicity)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Presolution (computePresolution, computePresolutionWithTiming, computePresolutionWithTimingAndRootOwnership)
import MLF.Constraint.RootOwnership
  ( ModuleRootId (..),
    RootOwnershipIndex (..),
    ownersForEdge,
    rootOwnershipOwnedEdgeCount,
    rootOwnershipOwnedEdgeCounts,
    rootOwnershipOwnedExpVarCount,
    rootOwnershipOwnedGenCount,
    rootOwnershipOwnedNodeCount,
    rootOwnershipRootCount,
    rootOwnershipSharedEdgeCount,
    ownersForGen,
    ownersForNode,
  )
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindFlag,
    Constraint (..),
    EdgeId (..),
    GenNode,
    GenNodeId (..),
    InstEdge (..),
    NodeId (..),
    NodeMap (..),
    NodeRef,
    PolySyms,
    TyNode,
    UnifyEdge (..),
    fromListGen,
    fromListNode,
    nodeRefKey,
    toListGen,
    toListNode,
  )
import MLF.Constraint.Types.Phase (Phase (Presolved, Raw))
import MLF.Elab.Elaborate (ElabConfig, ElabEnv, elaborateWithEnv)
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.PipelineConfig (PipelineConfig (..), defaultPipelineConfig)
import MLF.Elab.PipelineError
  ( PipelineError (..),
    fromConstraintError,
    fromCycleError,
    fromElabError,
    fromPresolutionError,
    fromSolveError,
    fromTypeCheckError,
  )
import MLF.Elab.Run.Generalize.Prepare
  ( PreparedGeneralizationArtifact,
    PreparedRootGeneralization (..),
    canonicalizePreparedAnn,
    computePreparedResultType,
    computePreparedResultTypeWithRootGeneralization,
    generalizePreparedRoot,
    generalizePreparedRootDetailed,
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    preparedAnnotated,
    preparedElaborationConfig,
    preparedElaborationEnv,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    stripPreparedWitnesslessAuthoritativeAnn,
  )
import MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstIfNeeded,
    preserveRetainedChildAuthoritativeResult,
    substInTerm,
  )
import MLF.Elab.TypeCheck (typeCheckWithEnv)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen
  ( AnnExpr (..),
    ConstraintError (..),
    ConstraintResult (..),
    ExternalBinding (..),
    ExternalBindingMode (..),
    ExternalBindings,
    ExternalEnv,
    ModuleConstraintRoot (..),
    ModuleConstraintResult (..),
    generateConstraintsWithExternalBindings,
    generateModuleConstraintsWithExternalBindings,
  )
import MLF.Frontend.Syntax (NormSrcType, NormSurfaceExpr, StructBound, VarName)
import qualified MLF.Frontend.Syntax as Surface
import MLF.Reify.TypeOps (freeTypeVarsType, freshNameLike, substTypeCapture)
import MLF.Util.Timing (TimingConfig, emitProgramOperationMetricIO, timeProgramOperationIO, timingProgramOperations)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data PipelineElabDetailedResult = PipelineElabDetailedResult
  { pedTerm :: ElabTerm,
    pedType :: ElabType,
    pedRootAnn :: AnnExpr,
    pedTypeCheckEnv :: TypeCheck.Env
  }

data PreparedExternalBindings = PreparedExternalBindings
  { pebBindings :: ExternalBindings,
    pebSchemeInfos :: Map.Map VarName SchemeInfo,
    pebTypeCheckEnv :: TypeCheck.Env
  }

data ModuleBatchSharedContext = ModuleBatchSharedContext
  { mbscPreparedExternalBindings :: !PreparedExternalBindings,
    mbscFrozenExternalSchemeTemplates :: !(Map.Map VarName FrozenExternalSchemeTemplate),
    mbscRootTemplateInstantiations :: !(Map.Map VarName RootTemplateInstantiation)
  }

data FrozenExternalSchemeTemplate = FrozenExternalSchemeTemplate
  { festName :: !VarName,
    festMode :: !ExternalBindingMode,
    festSourceType :: !NormSrcType,
    festHasSchemeInfo :: !Bool
  }

data RootTemplateInstantiation = RootTemplateInstantiation
  { rtiRootName :: !VarName,
    rtiTemplateNames :: ![VarName],
    rtiTemplateCount :: !Int,
    rtiMissingTemplateCount :: !Int
  }

data ModuleBatchPlan p = ModuleBatchPlan
  { mbpRoots :: [(VarName, ModuleConstraintRoot)],
    mbpPartitions :: [(VarName, RootPartition p)],
    mbpSharedEdgeCount :: !Int,
    mbpUnknownEdgeCount :: !Int
  }

data RootPartition p = RootPartition
  { rpRootId :: !ModuleRootId,
    rpRootName :: !VarName,
    rpConstraint :: Constraint p,
    rpAnnotated :: !AnnExpr,
    rpAnnSourceTypes :: !(IntMap.IntMap NormSrcType),
    rpPreparedExternalBindings :: !PreparedExternalBindings,
    rpOwnedEdgeCount :: !Int,
    rpExternalSchemeUseCount :: !Int
  }

data RootPartitionBucket = RootPartitionBucket
  { rpbNodes :: ![(NodeId, TyNode)],
    rpbGens :: ![(GenNodeId, GenNode)],
    rpbInstEdges :: ![InstEdge],
    rpbUnifyEdges :: ![UnifyEdge],
    rpbBindParents :: !(IntMap.IntMap (NodeRef, BindFlag)),
    rpbNodeKeys :: !IntSet.IntSet,
    rpbGenKeys :: !IntSet.IntSet,
    rpbEdgeKeys :: !IntSet.IntSet
  }

data RootFinalizationContext p = RootFinalizationContext
  { rfcPartition :: !(RootPartition p),
    rfcPreparedExternalBindings :: !PreparedExternalBindings,
    rfcSharedContext :: !(Maybe ModuleBatchSharedContext),
    rfcTemplateInstantiation :: !(Maybe RootTemplateInstantiation)
  }

validateDirectRecursiveAnnotations :: NormSurfaceExpr -> Either ConstraintError ()
validateDirectRecursiveAnnotations = goExpr
  where
    goExpr expr =
      case expr of
        Surface.EVar _ -> Right ()
        Surface.ELit _ -> Right ()
        Surface.ELam _ body -> goExpr body
        Surface.EApp fun arg -> goExpr fun >> goExpr arg
        Surface.ELet _ rhs body -> goExpr rhs >> goExpr body
        Surface.ELamAnn _ annTy body -> validateAnn annTy >> goExpr body
        Surface.EAnn inner annTy -> goExpr inner >> validateAnn annTy
        Surface.ECoerceConst _ -> Right ()

    validateAnn annTy =
      case directNonContractiveMu annTy of
        Just badTy -> Left (RecursiveAnnotationNotSupported badTy)
        Nothing -> Right ()

    directNonContractiveMu annTy =
      case annTy of
        Surface.STMu v body
          | not (muBodyContractive v body) -> Just annTy
        _ -> Nothing

    muBodyContractive needle = bodyType False False
      where
        bodyType guarded shadowed ty =
          case ty of
            Surface.STVar v -> shadowed || v /= needle || guarded
            Surface.STArrow dom cod -> bodyType True shadowed dom && bodyType True shadowed cod
            Surface.STBase _ -> True
            Surface.STCon _ args -> all (bodyType True shadowed) args
            Surface.STVarApp v args ->
              (shadowed || v /= needle || guarded) && all (bodyType True shadowed) args
            Surface.STTyLam v body ->
              bodyType guarded (shadowed || v == needle) body
            Surface.STTyApp fun arg ->
              bodyType guarded shadowed fun && bodyType guarded shadowed arg
            Surface.STForall v mb body ->
              let shadowed' = shadowed || v == needle
                  boundOk = maybe True (bodyBound guarded shadowed' . Surface.unNormBound) mb
               in boundOk && bodyType guarded shadowed' body
            Surface.STMu v body ->
              let shadowed' = shadowed || v == needle
               in bodyType guarded shadowed' body
            Surface.STBottom -> True

        bodyBound guarded shadowed bound =
          case bound of
            Surface.STArrow dom cod -> bodyType True shadowed dom && bodyType True shadowed cod
            Surface.STBase _ -> True
            Surface.STCon _ args -> all (bodyType True shadowed) args
            Surface.STVarApp v args ->
              (shadowed || v /= needle || guarded) && all (bodyType True shadowed) args
            Surface.STTyLam v body ->
              bodyType guarded (shadowed || v == needle) body
            Surface.STTyApp fun arg ->
              bodyType guarded shadowed fun && bodyType guarded shadowed arg
            Surface.STForall v mb body ->
              let shadowed' = shadowed || v == needle
                  boundOk = maybe True (bodyBound guarded shadowed' . Surface.unNormBound) mb
               in boundOk && bodyType guarded shadowed' body
            Surface.STMu v body ->
              let shadowed' = shadowed || v == needle
               in bodyType guarded shadowed' body
            Surface.STBottom -> True

runPipelineElab :: PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElab = runPipelineElabWithConfig defaultPipelineConfig

runPipelineElabWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithConfig config polySyms expr =
  detailedPair <$> runPipelineElabWith FinalCheckInPipeline (pcTraceConfig config) polySyms Map.empty expr

-- | Run the pipeline with an external environment of type assumptions
-- for free variables, avoiding the ELamAnn wrapping approach.
runPipelineElabWithEnv :: PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithEnv = runPipelineElabWithConfigAndEnv defaultPipelineConfig

runPipelineElabWithConfigAndEnv :: PipelineConfig -> PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError (ElabTerm, ElabType)
runPipelineElabWithConfigAndEnv config polySyms extEnv expr =
  detailedPair <$> runPipelineElabDetailedWithConfigAndEnv config polySyms extEnv expr

runPipelineElabDetailedWithEnv :: PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithEnv = runPipelineElabDetailedWithConfigAndEnv defaultPipelineConfig

runPipelineElabDetailedWithConfigAndEnv :: PipelineConfig -> PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithConfigAndEnv config polySyms extEnv =
  runPipelineElabDetailedWithConfigAndExternalBindings config polySyms (schemeExternalBindings extEnv)

runPipelineElabDetailedWithExternalBindings :: PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithExternalBindings =
  runPipelineElabDetailedWithConfigAndExternalBindings defaultPipelineConfig

runPipelineElabDetailedWithConfigAndExternalBindings :: PipelineConfig -> PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithConfigAndExternalBindings config polySyms extBindings =
  runPipelineElabWith FinalCheckInPipeline (pcTraceConfig config) polySyms extBindings

runPipelineElabDetailedUncheckedWithExternalBindings :: PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedUncheckedWithExternalBindings polySyms extBindings =
  runPipelineElabWith FinalCheckAfterDeferredRewrite (pcTraceConfig defaultPipelineConfig) polySyms extBindings

runPipelineElabDetailedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithPreparedExternalBindings =
  runPipelineElabWithPrepared FinalCheckInPipeline (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithPreparedWithTiming timing label FinalCheckInPipeline (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedUncheckedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedUncheckedWithPreparedExternalBindings =
  runPipelineElabWithPrepared FinalCheckAfterDeferredRewrite (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithPreparedWithTiming timing label FinalCheckAfterDeferredRewrite (pcTraceConfig defaultPipelineConfig)

schemeExternalBindings :: ExternalEnv -> ExternalBindings
schemeExternalBindings =
  Map.map (\srcTy -> ExternalBinding {externalBindingType = srcTy, externalBindingMode = ExternalBindingScheme})

prepareExternalBindings :: ExternalBindings -> Either ConstraintError PreparedExternalBindings
prepareExternalBindings extBindings = do
  schemeInfos <- traverse externalBindingSchemeInfo extBindings
  pure
    PreparedExternalBindings
      { pebBindings = extBindings,
        pebSchemeInfos = schemeInfos,
        pebTypeCheckEnv = typeCheckEnvFromSchemeInfos schemeInfos
      }

restrictPreparedExternalBindings :: Set.Set VarName -> PreparedExternalBindings -> PreparedExternalBindings
restrictPreparedExternalBindings names prepared =
  let schemeInfos = Map.restrictKeys (pebSchemeInfos prepared) names
   in PreparedExternalBindings
        { pebBindings = Map.restrictKeys (pebBindings prepared) names,
          pebSchemeInfos = schemeInfos,
          pebTypeCheckEnv = restrictTypeCheckEnv names (pebTypeCheckEnv prepared)
        }

unionPreparedExternalBindings :: PreparedExternalBindings -> PreparedExternalBindings -> PreparedExternalBindings
unionPreparedExternalBindings preferred fallback =
  let schemeInfos = pebSchemeInfos preferred `Map.union` pebSchemeInfos fallback
   in PreparedExternalBindings
        { pebBindings = pebBindings preferred `Map.union` pebBindings fallback,
          pebSchemeInfos = schemeInfos,
          pebTypeCheckEnv = unionTypeCheckEnv (pebTypeCheckEnv preferred) (pebTypeCheckEnv fallback)
        }

typeCheckEnvFromSchemeInfos :: Map.Map VarName SchemeInfo -> TypeCheck.Env
typeCheckEnvFromSchemeInfos schemeInfos =
  TypeCheck.Env
    { TypeCheck.termEnv = Map.map (schemeToType . siScheme) schemeInfos,
      TypeCheck.typeEnv = Map.empty
    }

restrictTypeCheckEnv :: Set.Set VarName -> TypeCheck.Env -> TypeCheck.Env
restrictTypeCheckEnv names env =
  env
    { TypeCheck.termEnv = Map.restrictKeys (TypeCheck.termEnv env) names
    }

unionTypeCheckEnv :: TypeCheck.Env -> TypeCheck.Env -> TypeCheck.Env
unionTypeCheckEnv preferred fallback =
  TypeCheck.Env
    { TypeCheck.termEnv = TypeCheck.termEnv preferred `Map.union` TypeCheck.termEnv fallback,
      TypeCheck.typeEnv = TypeCheck.typeEnv preferred `Map.union` TypeCheck.typeEnv fallback
    }

detailedPair :: PipelineElabDetailedResult -> (ElabTerm, ElabType)
detailedPair result = (pedTerm result, pedType result)

data PipelineFinalCheckMode
  = FinalCheckInPipeline
  | FinalCheckAfterDeferredRewrite
  deriving (Eq, Show)

runPipelineElabWith ::
  PipelineFinalCheckMode ->
  TraceConfig ->
  PolySyms ->
  ExternalBindings ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWith finalCheckMode traceCfg polySyms extBindings expr = do
  extPrepared <- fromConstraintError (prepareExternalBindings extBindings)
  runPipelineElabWithPrepared finalCheckMode traceCfg polySyms extPrepared expr

runPipelineElabWithPrepared ::
  PipelineFinalCheckMode ->
  TraceConfig ->
  PolySyms ->
  PreparedExternalBindings ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithPrepared finalCheckMode traceCfg polySyms extPrepared =
  runPipelineElabWithPreparedGenerated
    finalCheckMode
    traceCfg
    extPrepared
    (generateConstraintsWithExternalBindings polySyms (pebBindings extPrepared))

runPipelineElabWithPreparedGenerated ::
  PipelineFinalCheckMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  (NormSurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithPreparedGenerated finalCheckMode traceCfg extPrepared generateConstraints expr = do
  () <- fromConstraintError (validateDirectRecursiveAnnotations expr)
  let initialSchemeInfos = pebSchemeInfos extPrepared
  ConstraintResult {crConstraint = c0, crAnnotated = ann, crAnnSourceTypes = annSourceTypes, crInitialEnv = _initialBindings} <-
    fromConstraintError (generateConstraints expr)
  let c1 = normalize c0
  (cAcyclic, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)
  pres <- fromPresolutionError (computePresolution traceCfg acyc cAcyclic)
  prepared <-
    fromSolveError $
      prepareGeneralizationArtifact traceCfg cAcyclic pres ann
  -- Build authoritative SchemeInfo map and TypeCheck.Env from external
  -- source types.  We derive schemes directly from the caller-supplied
  -- NormSrcType values (which preserve lowerType naming) instead of
  -- re-generalizing through the constraint graph, which would produce
  -- graph-internal variable names that conflict with constructor types.
  let initialTcEnv = pebTypeCheckEnv extPrepared
      annCanon = preparedAnnotated prepared
      elabConfig = preparedElaborationConfig traceCfg prepared
      elabEnv = preparedElaborationEnv annSourceTypes initialSchemeInfos prepared
  term <- fromElabError (elaborateWithEnv elabConfig elabEnv annCanon)
  case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
    () -> pure ()
  let authoritativeAnnCanon = authoritativeRootAnn term annCanon
      authoritativeAnnPre = authoritativeRootAnn term ann
      (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
        stripPreparedWitnesslessAuthoritativeAnn prepared authoritativeAnnCanon authoritativeAnnPre
  (rootScheme, rootSubst) <-
    fromElabError $
      generalizePreparedRoot prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal
  let termSubst = substInTerm rootSubst term

  let retainedChildAuthoritativeCandidate =
        case preserveRetainedChildAuthoritativeResult termSubst of
          Just _ -> True
          Nothing -> False
      termClosed0 =
        if retainedChildAuthoritativeCandidate
          then closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
          else case typeCheckWithEnv initialTcEnv termSubst of
            Right ty | null (freeTypeVarsType ty) -> termSubst
            Right ty ->
              case rootScheme of
                Forall binds _
                  | null binds ->
                      let freeBinds =
                            [ (name, Nothing)
                              | name <- Set.toList (freeTypeVarsType ty)
                            ]
                          freeScheme = Forall freeBinds ty
                       in closeTermWithSchemeSubstIfNeeded IntMap.empty freeScheme termSubst
                _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
            Left _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
      termClosed =
        case preserveRetainedChildAuthoritativeResult termClosed0 of
          Just termAdjusted -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme termAdjusted
          Nothing -> termClosed0
  let termClosedFresh = freshenTypeAbsAgainstEnv initialTcEnv termClosed
      uncheckedAuthoritative =
        pure
          PipelineElabDetailedResult
            { pedTerm = termClosedFresh,
              pedType = schemeToType rootScheme,
              pedRootAnn = authoritativeAnnCanonFinal,
              pedTypeCheckEnv = initialTcEnv
            }
      checkedAuthoritative = do
        tyChecked <-
          case typeCheckWithEnv initialTcEnv termClosedFresh of
            Right ty -> pure ty
            Left err -> fromTypeCheckError (Left err)
        pure
          PipelineElabDetailedResult
            { pedTerm = termClosedFresh,
              pedType = tyChecked,
              pedRootAnn = authoritativeAnnCanonFinal,
              pedTypeCheckEnv = initialTcEnv
            }
      authoritativeResult =
        case finalCheckMode of
          FinalCheckInPipeline -> checkedAuthoritative
          FinalCheckAfterDeferredRewrite -> uncheckedAuthoritative

  -- Keep result-type reconstruction for diagnostics, but report the
  -- type-checker result as authoritative.
  case finalCheckMode of
    FinalCheckAfterDeferredRewrite -> authoritativeResult
    FinalCheckInPipeline -> do
      _ <- fromElabError (computePreparedResultType prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal)
      authoritativeResult

runPipelineElabWithPreparedWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  PolySyms ->
  PreparedExternalBindings ->
  NormSurfaceExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedWithTiming timing label finalCheckMode traceCfg polySyms extPrepared =
  runPipelineElabWithPreparedGeneratedWithTiming
    timing
    label
    finalCheckMode
    traceCfg
    extPrepared
    (generateConstraintsWithExternalBindings polySyms (pebBindings extPrepared))

runPipelineElabWithPreparedGeneratedWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  (NormSurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedGeneratedWithTiming timing label finalCheckMode traceCfg extPrepared generateConstraints expr = do
  validationResult <-
    timeProgramOperationIO timing (label ++ ".validate_annotations") $
      evaluate (fromConstraintError (validateDirectRecursiveAnnotations expr))
  case validationResult of
    Left err -> pure (Left err)
    Right () -> do
      let initialSchemeInfos = pebSchemeInfos extPrepared
      constraintResult <-
        timeProgramOperationIO timing (label ++ ".generate_constraints") $
          evaluate (fromConstraintError (generateConstraints expr))
      case constraintResult of
        Left err -> pure (Left err)
        Right ConstraintResult {crConstraint = c0, crAnnotated = ann, crAnnSourceTypes = annSourceTypes, crInitialEnv = _initialBindings} -> do
          normalizeResult <-
            timeProgramOperationIO timing (label ++ ".constraint_normalize") $
              evaluate (normalize c0)
          acycResult <-
            timeProgramOperationIO timing (label ++ ".acyclicity") $
              evaluate (fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult))
          case acycResult of
            Left err -> pure (Left err)
            Right (cAcyclic, acyc) -> do
              presResult <-
                timeProgramOperationIO timing (label ++ ".presolution") $
                  fromPresolutionError <$> computePresolutionWithTiming timing (label ++ ".presolution") traceCfg acyc cAcyclic
              case presResult of
                Left err -> pure (Left err)
                Right pres -> do
                  preparedResult <-
                    timeProgramOperationIO timing (label ++ ".prepare_generalization") $
                      evaluate (fromSolveError (prepareGeneralizationArtifact traceCfg cAcyclic pres ann))
                  case preparedResult of
                    Left err -> pure (Left err)
                    Right prepared -> do
                      let initialTcEnv = pebTypeCheckEnv extPrepared
                          annCanon = preparedAnnotated prepared
                          elabConfig = preparedElaborationConfig traceCfg prepared
                          elabEnv = preparedElaborationEnv annSourceTypes initialSchemeInfos prepared
                      termResult <-
                        timeProgramOperationIO timing (label ++ ".elaborate") $
                          evaluate (fromElabError (elaborateWithEnv elabConfig elabEnv annCanon))
                      case termResult of
                        Left err -> pure (Left err)
                        Right term -> do
                          case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
                            () -> pure ()
                          let authoritativeAnnCanon = authoritativeRootAnn term annCanon
                              authoritativeAnnPre = authoritativeRootAnn term ann
                              (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
                                stripPreparedWitnesslessAuthoritativeAnn prepared authoritativeAnnCanon authoritativeAnnPre
                          rootResult <-
                            timeProgramOperationIO timing (label ++ ".generalize_root") $
                              evaluate (fromElabError (generalizePreparedRoot prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal))
                          case rootResult of
                            Left err -> pure (Left err)
                            Right (rootScheme, rootSubst) -> do
                              termSubst <-
                                timeProgramOperationIO timing (label ++ ".subst_root") $
                                  evaluate (substInTerm rootSubst term)
                              termClosed <-
                                timeProgramOperationIO timing (label ++ ".close_term") $
                                  evaluate (closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst)
                              termClosedFresh <-
                                timeProgramOperationIO timing (label ++ ".freshen_type_abs") $
                                  evaluate (freshenTypeAbsAgainstEnv initialTcEnv termClosed)
                              let uncheckedAuthoritative =
                                    Right
                                      PipelineElabDetailedResult
                                        { pedTerm = termClosedFresh,
                                          pedType = schemeToType rootScheme,
                                          pedRootAnn = authoritativeAnnCanonFinal,
                                          pedTypeCheckEnv = initialTcEnv
                                        }
                              authoritativeResult <-
                                case finalCheckMode of
                                  FinalCheckInPipeline ->
                                    timeProgramOperationIO timing (label ++ ".final_typecheck") $
                                      evaluate $ do
                                        tyChecked <-
                                          case typeCheckWithEnv initialTcEnv termClosedFresh of
                                            Right ty -> pure ty
                                            Left err -> fromTypeCheckError (Left err)
                                        pure
                                          PipelineElabDetailedResult
                                            { pedTerm = termClosedFresh,
                                              pedType = tyChecked,
                                              pedRootAnn = authoritativeAnnCanonFinal,
                                              pedTypeCheckEnv = initialTcEnv
                                            }
                                  FinalCheckAfterDeferredRewrite ->
                                    pure uncheckedAuthoritative
                              case finalCheckMode of
                                FinalCheckAfterDeferredRewrite -> pure authoritativeResult
                                FinalCheckInPipeline -> do
                                  resultTypeResult <-
                                    timeProgramOperationIO timing (label ++ ".result_type_reconstruction") $
                                      evaluate (fromElabError (computePreparedResultType prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal))
                                  pure $ do
                                    _ <- resultTypeResult
                                    authoritativeResult

runPipelineElabWithPreparedConstraintWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  Constraint 'Raw ->
  AnnExpr ->
  IntMap.IntMap NormSrcType ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedConstraintWithTiming timing label finalCheckMode traceCfg extPrepared c0 ann annSourceTypes = do
  let initialSchemeInfos = pebSchemeInfos extPrepared
  normalizeResult <-
    timeProgramOperationIO timing (label ++ ".constraint_normalize") $
      evaluate (normalize c0)
  acycResult <-
    timeProgramOperationIO timing (label ++ ".acyclicity") $
      evaluate (fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult))
  case acycResult of
    Left err -> pure (Left err)
    Right (cAcyclic, acyc) -> do
      presResult <-
        timeProgramOperationIO timing (label ++ ".presolution") $
          fromPresolutionError <$> computePresolutionWithTiming timing (label ++ ".presolution") traceCfg acyc cAcyclic
      case presResult of
        Left err -> pure (Left err)
        Right pres -> do
          preparedResult <-
            timeProgramOperationIO timing (label ++ ".prepare_generalization") $
              evaluate (fromSolveError (prepareGeneralizationArtifact traceCfg cAcyclic pres ann))
          case preparedResult of
            Left err -> pure (Left err)
            Right prepared -> do
              readContextResult <-
                timeProgramOperationIO timing (label ++ ".root_finalization.prepare_read_context") $
                  evaluate (fromElabError (preparedReadContextReady prepared))
              resultTypeReadContextResult <-
                timeProgramOperationIO timing (label ++ ".root_finalization.result_type_read_context") $
                  evaluate (fromElabError (preparedResultTypeViewReady prepared))
              case (readContextResult, resultTypeReadContextResult) of
                (Left err, _) -> pure (Left err)
                (_, Left err) -> pure (Left err)
                (Right (), Right ()) -> do
                  let initialTcEnv = pebTypeCheckEnv extPrepared
                      annCanon = preparedAnnotated prepared
                      elabConfig = preparedElaborationConfig traceCfg prepared
                      elabEnv = preparedElaborationEnv annSourceTypes initialSchemeInfos prepared
                  termResult <-
                    timeProgramOperationIO timing (label ++ ".elaborate") $
                      evaluate (fromElabError (elaborateWithEnv elabConfig elabEnv annCanon))
                  case termResult of
                    Left err -> pure (Left err)
                    Right term -> do
                      case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
                        () -> pure ()
                      let authoritativeAnnCanon = authoritativeRootAnn term annCanon
                          authoritativeAnnPre = authoritativeRootAnn term ann
                          (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
                            stripPreparedWitnesslessAuthoritativeAnn prepared authoritativeAnnCanon authoritativeAnnPre
                      rootResult <-
                        timeProgramOperationIO timing (label ++ ".generalize_root") $
                          evaluate (fromElabError (generalizePreparedRoot prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal))
                      case rootResult of
                        Left err -> pure (Left err)
                        Right (rootScheme, rootSubst) -> do
                          termSubst <-
                            timeProgramOperationIO timing (label ++ ".subst_root") $
                              evaluate (substInTerm rootSubst term)
                          termClosed <-
                            timeProgramOperationIO timing (label ++ ".close_term") $
                              evaluate (closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst)
                          termClosedFresh <-
                            timeProgramOperationIO timing (label ++ ".freshen_type_abs") $
                              evaluate (freshenTypeAbsAgainstEnv initialTcEnv termClosed)
                          let uncheckedAuthoritative =
                                Right
                                  PipelineElabDetailedResult
                                    { pedTerm = termClosedFresh,
                                      pedType = schemeToType rootScheme,
                                      pedRootAnn = authoritativeAnnCanonFinal,
                                      pedTypeCheckEnv = initialTcEnv
                                    }
                          authoritativeResult <-
                            case finalCheckMode of
                              FinalCheckInPipeline ->
                                timeProgramOperationIO timing (label ++ ".final_typecheck") $
                                  evaluate $ do
                                    tyChecked <-
                                      case typeCheckWithEnv initialTcEnv termClosedFresh of
                                        Right ty -> pure ty
                                        Left err -> fromTypeCheckError (Left err)
                                    pure
                                      PipelineElabDetailedResult
                                        { pedTerm = termClosedFresh,
                                          pedType = tyChecked,
                                          pedRootAnn = authoritativeAnnCanonFinal,
                                          pedTypeCheckEnv = initialTcEnv
                                        }
                              FinalCheckAfterDeferredRewrite ->
                                pure uncheckedAuthoritative
                          case finalCheckMode of
                            FinalCheckAfterDeferredRewrite -> pure authoritativeResult
                            FinalCheckInPipeline -> do
                              resultTypeResult <-
                                timeProgramOperationIO timing (label ++ ".result_type_reconstruction") $
                                  evaluate (fromElabError (computePreparedResultType prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal))
                              pure $ do
                                _ <- resultTypeResult
                                authoritativeResult

runPipelineElabDetailedModuleWithPreparedExternalBindingsWithTiming ::
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  [(VarName, NormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map VarName PipelineElabDetailedResult))
runPipelineElabDetailedModuleWithPreparedExternalBindingsWithTiming =
  runPipelineElabDetailedModuleWithPreparedExternalBindingsModeWithTiming FinalCheckInPipeline

runPipelineElabDetailedModuleDeferFinalCheckWithPreparedExternalBindingsWithTiming ::
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  [(VarName, NormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map VarName PipelineElabDetailedResult))
runPipelineElabDetailedModuleDeferFinalCheckWithPreparedExternalBindingsWithTiming =
  runPipelineElabDetailedModuleWithPreparedExternalBindingsModeWithTiming FinalCheckAfterDeferredRewrite

runPipelineElabDetailedModuleWithPreparedExternalBindingsModeWithTiming ::
  PipelineFinalCheckMode ->
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  [(VarName, NormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map VarName PipelineElabDetailedResult))
runPipelineElabDetailedModuleWithPreparedExternalBindingsModeWithTiming finalCheckMode timing label polySyms extPrepared rootPrepared namedExprs = do
  let traceCfg = pcTraceConfig defaultPipelineConfig
  validationResult <-
    timeProgramOperationIO timing (label ++ ".validate_annotations") $
      evaluate (mapM_ (fromConstraintError . validateDirectRecursiveAnnotations . snd) namedExprs)
  case validationResult of
    Left err -> pure (Left err)
    Right () -> do
      constraintResult <-
        timeProgramOperationIO timing (label ++ ".generate_constraints") $
          evaluate (fromConstraintError (generateModuleConstraintsWithExternalBindings polySyms (pebBindings extPrepared) namedExprs))
      case constraintResult of
        Left err -> pure (Left err)
        Right ModuleConstraintResult {mcrConstraint = c0, mcrRoots = roots, mcrAnnSourceTypes = annSourceTypes, mcrRootOwnership = rootOwnership} -> do
          emitModuleBatchGraphMetrics timing (label ++ ".graph") c0 rootOwnership roots annSourceTypes extPrepared rootPrepared
          let batchPlan = buildModuleBatchPlan c0 rootOwnership roots annSourceTypes extPrepared rootPrepared
          mbSharedContext <-
            if timingProgramOperations timing
              then do
                let sharedContext = buildModuleBatchSharedContext extPrepared (mbpPartitions batchPlan)
                _ <-
                  timeProgramOperationIO timing (label ++ ".batch_context.prepare_templates") $
                    evaluate (moduleBatchSharedTemplatePayloadMeasure sharedContext)
                _ <-
                  timeProgramOperationIO timing (label ++ ".batch_context.instantiate_templates") $
                    evaluate (moduleBatchSharedInstantiationPayloadMeasure sharedContext)
                emitModuleBatchSharedContextMetrics timing (label ++ ".batch_context") sharedContext
                pure (Just sharedContext)
              else pure Nothing
          emitModuleBatchPlanMetrics timing (label ++ ".partition") batchPlan
          if moduleBatchPlanRootLocalEligible batchPlan
            then runModuleBatchPlanRootLocalWithTiming timing (label ++ ".partitioned_roots") finalCheckMode traceCfg mbSharedContext batchPlan
            else runModuleBatchPlanGlobalWithTiming timing label finalCheckMode traceCfg extPrepared rootPrepared c0 rootOwnership roots annSourceTypes

runModuleBatchPlanGlobalWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  Constraint 'Raw ->
  RootOwnershipIndex ->
  Map.Map VarName ModuleConstraintRoot ->
  IntMap.IntMap NormSrcType ->
  IO (Either PipelineError (Map.Map VarName PipelineElabDetailedResult))
runModuleBatchPlanGlobalWithTiming timing label finalCheckMode traceCfg extPrepared rootPrepared c0 rootOwnership roots annSourceTypes = do
  normalizeResult <-
    timeProgramOperationIO timing (label ++ ".constraint_normalize") $
      evaluate (normalize c0)
  acycResult <-
    timeProgramOperationIO timing (label ++ ".acyclicity") $
      evaluate (fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult))
  case acycResult of
    Left err -> pure (Left err)
    Right (cAcyclic, acyc) -> do
      presResult <-
        timeProgramOperationIO timing (label ++ ".presolution") $
          fromPresolutionError <$> computePresolutionWithTimingAndRootOwnership timing (label ++ ".presolution") traceCfg rootOwnership acyc cAcyclic
      case presResult of
        Left err -> pure (Left err)
        Right pres -> do
          preparedResult <-
            timeProgramOperationIO timing (label ++ ".prepare_generalization") $
              evaluate $
                fromSolveError $
                  prepareGeneralizationArtifactForRoots
                    traceCfg
                    cAcyclic
                    pres
                    [mcrAnnotated root | root <- Map.elems roots]
          case preparedResult of
            Left err -> pure (Left err)
            Right prepared ->
              let elabConfig = preparedElaborationConfig traceCfg prepared
               in
              timeProgramOperationIO timing (label ++ ".roots") $
                finishPreparedPipelineRootsWithTiming
                  timing
                  (label ++ ".roots")
                  finalCheckMode
                  traceCfg
                  extPrepared
                  rootPrepared
                  prepared
                  elabConfig
                  annSourceTypes
                  (Map.toList roots)

runModuleBatchPlanRootLocalWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  Maybe ModuleBatchSharedContext ->
  ModuleBatchPlan 'Raw ->
  IO (Either PipelineError (Map.Map VarName PipelineElabDetailedResult))
runModuleBatchPlanRootLocalWithTiming timing label finalCheckMode traceCfg mbSharedContext plan =
  timeProgramOperationIO timing label $
    case mbpPartitions plan of
      [] -> pure (Right Map.empty)
      [_] -> goSequential (1 :: Int) Map.empty (mbpPartitions plan)
      partitions -> goConcurrent partitions
  where
    goSequential _ acc [] =
      pure (Right acc)
    goSequential index acc ((name, partition) : rest) = do
      result <-
        runRootFinalizationContextWithTiming
          timing
          (label ++ ".root_" ++ show index)
          finalCheckMode
          traceCfg
          (mkRootFinalizationContext name partition)
      case result of
        Left err -> pure (Left err)
        Right out -> goSequential (index + 1) (Map.insert name out acc) rest

    goConcurrent partitions = do
      ensureConcurrentCapabilities (length partitions)
      workers <-
        mapM
          ( \(index, (name, partition)) -> do
              done <- newEmptyMVar
              _ <-
                forkIO $
                  try
                    ( runRootFinalizationContextWithTiming
                        timing
                        (label ++ ".root_" ++ show index)
                        finalCheckMode
                        traceCfg
                        (mkRootFinalizationContext name partition)
                    )
                    >>= putMVar done
              pure (name, done)
          )
          (zip [(1 :: Int) ..] partitions)
      settled <- mapM (\(name, done) -> (\result -> (name, result)) <$> takeMVar done) workers
      case [ex | (_, Left ex) <- settled] of
        ex : _ -> throwIO (ex :: SomeException)
        [] ->
          case [err | (_, Right (Left err)) <- settled] of
            err : _ -> pure (Left err)
            [] ->
              pure $
                Right $
                  Map.fromList
                    [ (name, out)
                    | (name, Right (Right out)) <- settled
                    ]

    mkRootFinalizationContext name partition =
      RootFinalizationContext
        { rfcPartition = partition,
          rfcPreparedExternalBindings = rpPreparedExternalBindings partition,
          rfcSharedContext = mbSharedContext,
          rfcTemplateInstantiation =
            Map.lookup name . mbscRootTemplateInstantiations =<< mbSharedContext
        }

    ensureConcurrentCapabilities workerCount =
      if rtsSupportsBoundThreads && workerCount > 1
        then do
          processorCount <- getNumProcessors
          currentCapabilities <- getNumCapabilities
          let targetCapabilities = max 1 (min workerCount processorCount)
          if currentCapabilities < targetCapabilities
            then setNumCapabilities targetCapabilities
            else pure ()
        else pure ()

runRootFinalizationContextWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  RootFinalizationContext 'Raw ->
  IO (Either PipelineError PipelineElabDetailedResult)
runRootFinalizationContextWithTiming
  timing
  label
  finalCheckMode
  traceCfg
  RootFinalizationContext
    { rfcPartition = partition,
      rfcPreparedExternalBindings = extPrepared,
      rfcSharedContext = mbSharedContext,
      rfcTemplateInstantiation = mbInstantiation
    } = do
    case (mbSharedContext, mbInstantiation) of
      (Just sharedContext, Just instantiation) -> do
        emitProgramOperationMetricIO timing (label ++ ".batch_context.frozen_templates") (fromIntegral (moduleBatchSharedTemplateCount sharedContext))
        emitProgramOperationMetricIO timing (label ++ ".batch_context.template_uses") (fromIntegral (rtiTemplateCount instantiation))
        emitProgramOperationMetricIO timing (label ++ ".batch_context.missing_templates") (fromIntegral (rtiMissingTemplateCount instantiation))
      _ -> pure ()
    runPipelineElabWithPreparedConstraintWithTiming
      timing
      label
      finalCheckMode
      traceCfg
      extPrepared
      (rpConstraint partition)
      (rpAnnotated partition)
      (rpAnnSourceTypes partition)

moduleBatchPlanRootLocalEligible :: ModuleBatchPlan p -> Bool
moduleBatchPlanRootLocalEligible plan =
  mbpSharedEdgeCount plan == 0
    && mbpUnknownEdgeCount plan == 0
    && not (null (mbpPartitions plan))

buildModuleBatchPlan ::
  Constraint 'Raw ->
  RootOwnershipIndex ->
  Map.Map VarName ModuleConstraintRoot ->
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  ModuleBatchPlan 'Raw
buildModuleBatchPlan constraint rootOwnership roots annSourceTypes extPrepared rootPrepared =
  ModuleBatchPlan
    { mbpRoots = orderedRoots,
      mbpPartitions = partitions,
      mbpSharedEdgeCount = rootOwnershipSharedEdgeCount rootOwnership,
      mbpUnknownEdgeCount =
        length
          [ ()
          | edge <- cInstEdges constraint,
            IntSet.null (ownersForEdge rootOwnership (getEdgeId (instEdgeId edge)))
          ]
    }
  where
    orderedRoots = Map.toList roots
    partitionBuckets = buildRootPartitionBuckets constraint rootOwnership orderedRoots
    partitions =
        [ ( name,
            buildRootPartitionFromBucket
              constraint
              annSourceTypes
              extPrepared
              rootPrepared
              name
              root
              (IntMap.findWithDefault emptyRootPartitionBucket (getModuleRootId (mcrRootId root)) partitionBuckets)
          )
        | (name, root) <- orderedRoots
        ]

buildRootPartitionBuckets ::
  Constraint 'Raw ->
  RootOwnershipIndex ->
  [(VarName, ModuleConstraintRoot)] ->
  IntMap.IntMap RootPartitionBucket
buildRootPartitionBuckets constraint rootOwnership orderedRoots =
  bucketBindParents
    $ bucketUnifyEdges
    $ bucketInstEdges
    $ bucketGens
    $ bucketNodes initialBuckets
  where
    initialBuckets =
      IntMap.fromList
        [ (getModuleRootId (mcrRootId root), emptyRootPartitionBucket)
        | (_name, root) <- orderedRoots
        ]

    bucketNodes buckets0 =
      foldl'
        ( \buckets (nid, node) ->
            insertForOwners
              (ownersForNode rootOwnership (getNodeId nid))
              (addBucketNode nid node)
              buckets
        )
        buckets0
        (toListNode (cNodes constraint))

    bucketGens buckets0 =
      foldl'
        ( \buckets (gid, genNode) ->
            insertForOwners
              (ownersForGen rootOwnership (getGenNodeId gid))
              (addBucketGen gid genNode)
              buckets
        )
        buckets0
        (toListGen (cGenNodes constraint))

    bucketInstEdges buckets0 =
      foldl'
        ( \buckets edge ->
            insertForOwners
              (ownersForEdge rootOwnership (getEdgeId (instEdgeId edge)))
              (addBucketInstEdge edge)
              buckets
        )
        buckets0
        (cInstEdges constraint)

    bucketUnifyEdges buckets0 =
      foldl'
        ( \buckets edge ->
            let ownerRoots =
                  IntSet.intersection
                    (ownersForNode rootOwnership (getNodeId (uniLeft edge)))
                    (ownersForNode rootOwnership (getNodeId (uniRight edge)))
             in insertForOwners ownerRoots (addBucketUnifyEdge edge) buckets
        )
        buckets0
        (cUnifyEdges constraint)

    bucketBindParents buckets0 =
      IntMap.foldlWithKey'
        ( \buckets childKey bindParent@(parent, _) ->
            let ownerRoots =
                  IntSet.intersection
                    (ownersForRefKey rootOwnership childKey)
                    (ownersForRefKey rootOwnership (nodeRefKey parent))
             in insertForOwners ownerRoots (addBucketBindParent childKey bindParent) buckets
        )
        buckets0
        (cBindParents constraint)

emptyRootPartitionBucket :: RootPartitionBucket
emptyRootPartitionBucket =
  RootPartitionBucket
    { rpbNodes = [],
      rpbGens = [],
      rpbInstEdges = [],
      rpbUnifyEdges = [],
      rpbBindParents = IntMap.empty,
      rpbNodeKeys = IntSet.empty,
      rpbGenKeys = IntSet.empty,
      rpbEdgeKeys = IntSet.empty
    }

insertForOwners ::
  IntSet.IntSet ->
  (RootPartitionBucket -> RootPartitionBucket) ->
  IntMap.IntMap RootPartitionBucket ->
  IntMap.IntMap RootPartitionBucket
insertForOwners owners updateBucket buckets =
  IntSet.foldl' (\acc rootKey -> IntMap.adjust updateBucket rootKey acc) buckets owners

addBucketNode :: NodeId -> TyNode -> RootPartitionBucket -> RootPartitionBucket
addBucketNode nid node bucket =
  bucket
    { rpbNodes = (nid, node) : rpbNodes bucket,
      rpbNodeKeys = IntSet.insert (getNodeId nid) (rpbNodeKeys bucket)
    }

addBucketGen :: GenNodeId -> GenNode -> RootPartitionBucket -> RootPartitionBucket
addBucketGen gid genNode bucket =
  bucket
    { rpbGens = (gid, genNode) : rpbGens bucket,
      rpbGenKeys = IntSet.insert (getGenNodeId gid) (rpbGenKeys bucket)
    }

addBucketInstEdge :: InstEdge -> RootPartitionBucket -> RootPartitionBucket
addBucketInstEdge edge bucket =
  bucket
    { rpbInstEdges = edge : rpbInstEdges bucket,
      rpbEdgeKeys = IntSet.insert (getEdgeId (instEdgeId edge)) (rpbEdgeKeys bucket)
    }

addBucketUnifyEdge :: UnifyEdge -> RootPartitionBucket -> RootPartitionBucket
addBucketUnifyEdge edge bucket =
  bucket {rpbUnifyEdges = edge : rpbUnifyEdges bucket}

addBucketBindParent :: Int -> (NodeRef, BindFlag) -> RootPartitionBucket -> RootPartitionBucket
addBucketBindParent childKey bindParent bucket =
  bucket {rpbBindParents = IntMap.insert childKey bindParent (rpbBindParents bucket)}

ownersForRefKey :: RootOwnershipIndex -> Int -> IntSet.IntSet
ownersForRefKey rootOwnership key
  | even key = ownersForNode rootOwnership (key `div` 2)
  | otherwise = ownersForGen rootOwnership ((key - 1) `div` 2)

buildModuleBatchSharedContext :: PreparedExternalBindings -> [(VarName, RootPartition p)] -> ModuleBatchSharedContext
buildModuleBatchSharedContext extPrepared partitions =
  ModuleBatchSharedContext
    { mbscPreparedExternalBindings = extPrepared,
      mbscFrozenExternalSchemeTemplates = templates,
      mbscRootTemplateInstantiations =
        Map.fromList
          [ (name, buildRootTemplateInstantiation name (rpPreparedExternalBindings partition) templates)
          | (name, partition) <- partitions
          ]
    }
  where
    templates =
      Map.mapMaybeWithKey freezeExternalSchemeTemplate (pebBindings extPrepared)
    schemeInfoNames = Map.keysSet (pebSchemeInfos extPrepared)

    freezeExternalSchemeTemplate name ExternalBinding {externalBindingType = srcTy, externalBindingMode = mode} =
      case mode of
        ExternalBindingScheme ->
          Just
            FrozenExternalSchemeTemplate
              { festName = name,
                festMode = mode,
                festSourceType = srcTy,
                festHasSchemeInfo = name `Set.member` schemeInfoNames
              }
        ExternalBindingMonomorphic -> Nothing

buildRootTemplateInstantiation ::
  VarName ->
  PreparedExternalBindings ->
  Map.Map VarName FrozenExternalSchemeTemplate ->
  RootTemplateInstantiation
buildRootTemplateInstantiation rootName prepared templates =
  RootTemplateInstantiation
    { rtiRootName = rootName,
      rtiTemplateNames = templateNames,
      rtiTemplateCount = length templateNames,
      rtiMissingTemplateCount = missingTemplateCount
    }
  where
    externalNames = Map.keys (pebBindings prepared)
    templateNames = filter (`Map.member` templates) externalNames
    missingTemplateCount =
      length
        [ ()
        | name <- externalNames,
          name `Map.notMember` templates
        ]

moduleBatchSharedTemplateCount :: ModuleBatchSharedContext -> Int
moduleBatchSharedTemplateCount =
  Map.size . mbscFrozenExternalSchemeTemplates

moduleBatchSharedInstantiationCount :: ModuleBatchSharedContext -> Int
moduleBatchSharedInstantiationCount =
  sum . map rtiTemplateCount . Map.elems . mbscRootTemplateInstantiations

moduleBatchSharedMissingTemplateCount :: ModuleBatchSharedContext -> Int
moduleBatchSharedMissingTemplateCount =
  sum . map rtiMissingTemplateCount . Map.elems . mbscRootTemplateInstantiations

moduleBatchSharedTemplatePayloadMeasure :: ModuleBatchSharedContext -> Int
moduleBatchSharedTemplatePayloadMeasure sharedContext =
  sum (map measureTemplate (Map.elems (mbscFrozenExternalSchemeTemplates sharedContext)))
  where
    measureTemplate template =
      festSourceType template `seq`
        length (festName template)
          + modeTag (festMode template)
          + if festHasSchemeInfo template then 1 else 0

    modeTag mode =
      case mode of
        ExternalBindingScheme -> 1
        ExternalBindingMonomorphic -> 0

moduleBatchSharedInstantiationPayloadMeasure :: ModuleBatchSharedContext -> Int
moduleBatchSharedInstantiationPayloadMeasure sharedContext =
  sum (map measureInstantiation (Map.elems (mbscRootTemplateInstantiations sharedContext)))
  where
    measureInstantiation instantiation =
      length (rtiRootName instantiation)
        + length (rtiTemplateNames instantiation)
        + rtiTemplateCount instantiation
        + rtiMissingTemplateCount instantiation

emitModuleBatchSharedContextMetrics :: TimingConfig -> String -> ModuleBatchSharedContext -> IO ()
emitModuleBatchSharedContextMetrics timing label sharedContext = do
  emitProgramOperationMetricIO timing (label ++ ".template_count") (fromIntegral (moduleBatchSharedTemplateCount sharedContext))
  emitProgramOperationMetricIO timing (label ++ ".template_instantiations") (fromIntegral (moduleBatchSharedInstantiationCount sharedContext))
  emitProgramOperationMetricIO timing (label ++ ".missing_templates") (fromIntegral (moduleBatchSharedMissingTemplateCount sharedContext))
  emitProgramOperationMetricIO timing (label ++ ".prepared_external_bindings") (fromIntegral (Map.size (pebBindings (mbscPreparedExternalBindings sharedContext))))
  mapM_
    ( \(index, instantiation) -> do
        emitProgramOperationMetricIO timing (label ++ ".root_" ++ show index ++ ".template_uses") (fromIntegral (rtiTemplateCount instantiation))
        emitProgramOperationMetricIO timing (label ++ ".root_" ++ show index ++ ".missing_templates") (fromIntegral (rtiMissingTemplateCount instantiation))
    )
    (zip [(1 :: Int) ..] (Map.elems (mbscRootTemplateInstantiations sharedContext)))

buildRootPartitionFromBucket ::
  Constraint 'Raw ->
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  VarName ->
  ModuleConstraintRoot ->
  RootPartitionBucket ->
  RootPartition 'Raw
buildRootPartitionFromBucket constraint annSourceTypes extPrepared rootPrepared name root bucket =
  RootPartition
    { rpRootId = rootId,
      rpRootName = name,
      rpConstraint = partitionConstraint,
      rpAnnotated = mcrAnnotated root,
      rpAnnSourceTypes = IntMap.restrictKeys annSourceTypes (rpbNodeKeys bucket),
      rpPreparedExternalBindings = rootExtPrepared,
      rpOwnedEdgeCount = IntSet.size (rpbEdgeKeys bucket),
      rpExternalSchemeUseCount = Map.size (pebSchemeInfos rootExtPrepared)
    }
  where
    rootId = mcrRootId root
    rootExtPrepared = Map.findWithDefault extPrepared name rootPrepared
    partitionConstraint =
      constraint
        { cNodes =
            fromListNode (rpbNodes bucket),
          cInstEdges = reverse (rpbInstEdges bucket),
          cUnifyEdges = reverse (rpbUnifyEdges bucket),
          cBindParents = rpbBindParents bucket,
          cEliminatedVars = cEliminatedVars constraint `IntSet.intersection` rpbNodeKeys bucket,
          cWeakenedVars = cWeakenedVars constraint `IntSet.intersection` rpbNodeKeys bucket,
          cAnnEdges = cAnnEdges constraint `IntSet.intersection` rpbEdgeKeys bucket,
          cLetEdges = cLetEdges constraint `IntSet.intersection` rpbEdgeKeys bucket,
          cGenNodes =
            fromListGen (rpbGens bucket)
        }

emitModuleBatchPlanMetrics :: TimingConfig -> String -> ModuleBatchPlan p -> IO ()
emitModuleBatchPlanMetrics timing label plan = do
  emitProgramOperationMetricIO timing (label ++ ".roots") (fromIntegral (length (mbpRoots plan)))
  emitProgramOperationMetricIO timing (label ++ ".shared_edges") (fromIntegral (mbpSharedEdgeCount plan))
  emitProgramOperationMetricIO timing (label ++ ".unknown_edges") (fromIntegral (mbpUnknownEdgeCount plan))
  emitProgramOperationMetricIO timing (label ++ ".root_local_enabled") (if moduleBatchPlanRootLocalEligible plan then 1 else 0)
  mapM_
    ( \(index, (_name, partition)) -> do
        emitProgramOperationMetricIO timing (label ++ ".root_" ++ show index ++ ".owned_edges") (fromIntegral (rpOwnedEdgeCount partition))
        emitProgramOperationMetricIO timing (label ++ ".root_" ++ show index ++ ".external_scheme_uses") (fromIntegral (rpExternalSchemeUseCount partition))
    )
    (zip [(1 :: Int) ..] (mbpPartitions plan))

emitModuleBatchGraphMetrics ::
  TimingConfig ->
  String ->
  Constraint p ->
  RootOwnershipIndex ->
  Map.Map VarName ModuleConstraintRoot ->
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  IO ()
emitModuleBatchGraphMetrics timing label constraint rootOwnership roots annSourceTypes extPrepared rootPrepared = do
  emitProgramOperationMetricIO timing (label ++ ".roots") (fromIntegral (Map.size roots))
  emitProgramOperationMetricIO timing (label ++ ".nodes") (fromIntegral (IntMap.size (getNodeMap (cNodes constraint))))
  emitProgramOperationMetricIO timing (label ++ ".inst_edges") (fromIntegral (length (cInstEdges constraint)))
  emitProgramOperationMetricIO timing (label ++ ".unify_edges") (fromIntegral (length (cUnifyEdges constraint)))
  emitProgramOperationMetricIO timing (label ++ ".bind_parents") (fromIntegral (IntMap.size (cBindParents constraint)))
  emitProgramOperationMetricIO timing (label ++ ".annotation_roots") (fromIntegral (IntMap.size annSourceTypes))
  emitProgramOperationMetricIO timing (label ++ ".external_scheme_unique") (fromIntegral (Map.size (pebSchemeInfos extPrepared)))
  emitProgramOperationMetricIO timing (label ++ ".external_scheme_uses") (fromIntegral (sum (map (Map.size . pebSchemeInfos) (Map.elems rootPrepared))))
  emitProgramOperationMetricIO timing (label ++ ".owned_roots") (fromIntegral (rootOwnershipRootCount rootOwnership))
  emitProgramOperationMetricIO timing (label ++ ".owned_nodes") (fromIntegral (rootOwnershipOwnedNodeCount rootOwnership))
  emitProgramOperationMetricIO timing (label ++ ".owned_gens") (fromIntegral (rootOwnershipOwnedGenCount rootOwnership))
  emitProgramOperationMetricIO timing (label ++ ".owned_exp_vars") (fromIntegral (rootOwnershipOwnedExpVarCount rootOwnership))
  emitProgramOperationMetricIO timing (label ++ ".owned_edges") (fromIntegral (rootOwnershipOwnedEdgeCount rootOwnership))
  emitProgramOperationMetricIO timing (label ++ ".shared_edges") (fromIntegral (rootOwnershipSharedEdgeCount rootOwnership))
  mapM_
    ( \(rootId, edgeCount) ->
        emitProgramOperationMetricIO timing (label ++ ".root_" ++ show rootId ++ ".owned_edges") (fromIntegral edgeCount)
    )
    (IntMap.toAscList (rootOwnershipOwnedEdgeCounts rootOwnership))

finishPreparedPipelineRootsWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  Map.Map VarName PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  IntMap.IntMap NormSrcType ->
  [(VarName, ModuleConstraintRoot)] ->
  IO (Either PipelineError (Map.Map VarName PipelineElabDetailedResult))
finishPreparedPipelineRootsWithTiming timing label finalCheckMode traceCfg extPrepared rootPrepared prepared elabConfig annSourceTypes roots =
  go (1 :: Int) Map.empty roots
  where
    go _ acc [] =
      pure (Right acc)
    go index acc ((name, root) : rest) = do
      let rootExtPrepared =
            Map.findWithDefault extPrepared name rootPrepared
          elabEnv =
            preparedElaborationEnv
              annSourceTypes
              (pebSchemeInfos rootExtPrepared)
              prepared
          rootLabel = label ++ ".root_" ++ show index
      result <-
        finishPreparedPipelineRootWithTiming
          timing
          rootLabel
          finalCheckMode
          traceCfg
          rootExtPrepared
          prepared
          elabConfig
          elabEnv
          (mcrAnnotated root)
      case result of
        Left err -> pure (Left err)
        Right out -> go (index + 1) (Map.insert name out acc) rest

finishPreparedPipelineRootWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  ElabEnv 'Presolved ->
  AnnExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
finishPreparedPipelineRootWithTiming timing label finalCheckMode traceCfg extPrepared prepared elabConfig elabEnv annPre = do
  let initialTcEnv = pebTypeCheckEnv extPrepared
      annCanon = canonicalizePreparedAnn prepared annPre
  termResult <-
    timeProgramOperationIO timing (label ++ ".elaborate") $
      evaluate (fromElabError (elaborateWithEnv elabConfig elabEnv annCanon))
  case termResult of
    Left err -> pure (Left err)
    Right term -> do
      case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
        () -> pure ()
      let authoritativeAnnCanon = authoritativeRootAnn term annCanon
          authoritativeAnnPre = authoritativeRootAnn term annPre
          (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
            stripPreparedWitnesslessAuthoritativeAnn prepared authoritativeAnnCanon authoritativeAnnPre
      rootResult <-
        timeProgramOperationIO timing (label ++ ".generalize_root") $
          evaluate (fromElabError (generalizePreparedRootDetailed prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal))
      case rootResult of
        Left err -> pure (Left err)
        Right rootGeneralization -> do
          let rootScheme = prgScheme rootGeneralization
              rootSubst = prgSubst rootGeneralization
          termSubst <-
            timeProgramOperationIO timing (label ++ ".subst_root") $
              evaluate (substInTerm rootSubst term)
          termClosed <-
            timeProgramOperationIO timing (label ++ ".close_term") $
              evaluate (closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst)
          termClosedFresh <-
            timeProgramOperationIO timing (label ++ ".freshen_type_abs") $
              evaluate (freshenTypeAbsAgainstEnv initialTcEnv termClosed)
          let uncheckedAuthoritative =
                Right
                  PipelineElabDetailedResult
                    { pedTerm = termClosedFresh,
                      pedType = schemeToType rootScheme,
                      pedRootAnn = authoritativeAnnCanonFinal,
                      pedTypeCheckEnv = initialTcEnv
                    }
          authoritativeResult <-
            case finalCheckMode of
              FinalCheckInPipeline ->
                timeProgramOperationIO timing (label ++ ".final_typecheck") $
                  evaluate $ do
                    tyChecked <-
                      case typeCheckWithEnv initialTcEnv termClosedFresh of
                        Right ty -> pure ty
                        Left err -> fromTypeCheckError (Left err)
                    pure
                      PipelineElabDetailedResult
                        { pedTerm = termClosedFresh,
                          pedType = tyChecked,
                          pedRootAnn = authoritativeAnnCanonFinal,
                          pedTypeCheckEnv = initialTcEnv
                        }
              FinalCheckAfterDeferredRewrite ->
                pure uncheckedAuthoritative
          case finalCheckMode of
            FinalCheckAfterDeferredRewrite -> pure authoritativeResult
            FinalCheckInPipeline -> do
              resultTypeResult <-
                timeProgramOperationIO timing (label ++ ".result_type_reconstruction") $
                  evaluate (fromElabError (computePreparedResultTypeWithRootGeneralization prepared rootGeneralization authoritativeAnnCanonFinal authoritativeAnnPreFinal))
              pure $ do
                _ <- resultTypeResult
                authoritativeResult

closePipelineTerm :: TypeCheck.Env -> IntMap.IntMap String -> ElabScheme -> ElabTerm -> ElabTerm -> ElabTerm
closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst =
  let retainedChildAuthoritativeCandidate =
        case preserveRetainedChildAuthoritativeResult termSubst of
          Just _ -> True
          Nothing -> False
      termClosed0 =
        if retainedChildAuthoritativeCandidate
          then closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
          else case typeCheckWithEnv initialTcEnv termSubst of
            Right ty | null (freeTypeVarsType ty) -> termSubst
            Right ty ->
              case rootScheme of
                Forall binds _
                  | null binds ->
                      let freeBinds =
                            [ (name, Nothing)
                              | name <- Set.toList (freeTypeVarsType ty)
                            ]
                          freeScheme = Forall freeBinds ty
                       in closeTermWithSchemeSubstIfNeeded IntMap.empty freeScheme termSubst
                _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
            Left _ -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme term
   in case preserveRetainedChildAuthoritativeResult termClosed0 of
        Just termAdjusted -> closeTermWithSchemeSubstIfNeeded rootSubst rootScheme termAdjusted
        Nothing -> termClosed0

freshenTypeAbsAgainstEnv :: TypeCheck.Env -> ElabTerm -> ElabTerm
freshenTypeAbsAgainstEnv env term0 = pruneVacuousLeadingTyAbsAgainstEnv env (go reserved term0)
  where
    reserved =
      Set.unions
        ( map freeTypeVarsType (Map.elems (TypeCheck.termEnv env))
            ++ [Set.fromList (Map.keys (TypeCheck.typeEnv env))]
        )

    go used term = case term of
      ETyAbs name mb body ->
        let usedForBinder = Set.union used (maybe Set.empty freeTypeVarsType mb)
            (name', bodyForName) =
              if Set.member name usedForBinder
                then
                  let fresh = freshNameLike name usedForBinder
                      bodyRenamed = renameTypeVarInTerm name fresh body
                   in (fresh, bodyRenamed)
                else (name, body)
            usedBody = Set.insert name' usedForBinder
            body' = go usedBody bodyForName
         in ETyAbs name' mb body'
      ELam v ty body ->
        ELam v ty (go (Set.union used (freeTypeVarsType ty)) body)
      EApp f a -> EApp (go used f) (go used a)
      ELet v sch rhs body ->
        let used' = Set.union used (freeTypeVarsType (schemeToType sch))
         in ELet v sch (go used' rhs) (go used' body)
      ETyInst t inst -> ETyInst (go used t) inst
      ERoll ty body -> ERoll ty (go used body)
      EUnroll body -> EUnroll (go used body)
      _ -> term

pruneVacuousLeadingTyAbsAgainstEnv :: TypeCheck.Env -> ElabTerm -> ElabTerm
pruneVacuousLeadingTyAbsAgainstEnv env term = case term of
  ETyAbs name mb body ->
    let env' =
          env
            { TypeCheck.typeEnv =
                Map.insert name (maybe TBottom tyToElab mb) (TypeCheck.typeEnv env)
            }
        body' = pruneVacuousLeadingTyAbsAgainstEnv env' body
     in case typeCheckWithEnv env' body' of
          Right bodyTy
            | name `Set.notMember` freeTypeVarsType bodyTy,
              not (containsRecursiveType bodyTy) ->
                case mb of
                  Nothing -> pruneVacuousLeadingTyAbsAgainstEnv env body'
                  Just _ ->
                    case Set.toList (freeTypeVarsType bodyTy `Set.difference` freeTypeVarsTypeCheckEnv env) of
                      [freeName] ->
                        let bodyRenamed = renameTypeVarInTerm freeName name body'
                         in case typeCheckWithEnv env' bodyRenamed of
                              Right renamedTy
                                | name `Set.member` freeTypeVarsType renamedTy ->
                                    ETyAbs name mb bodyRenamed
                              _ -> pruneVacuousLeadingTyAbsAgainstEnv env body'
                      _ -> pruneVacuousLeadingTyAbsAgainstEnv env body'
          _ -> ETyAbs name mb body'
  _ -> term

freeTypeVarsTypeCheckEnv :: TypeCheck.Env -> Set.Set String
freeTypeVarsTypeCheckEnv env =
  Set.unions
    ( map freeTypeVarsType (Map.elems (TypeCheck.termEnv env))
        ++ map freeTypeVarsType (Map.elems (TypeCheck.typeEnv env))
        ++ [Set.fromList (Map.keys (TypeCheck.typeEnv env))]
    )

containsRecursiveType :: ElabType -> Bool
containsRecursiveType ty = case ty of
  TMu _ _ -> True
  TArrow dom cod -> containsRecursiveType dom || containsRecursiveType cod
  TCon _ args -> any containsRecursiveType args
  TVarApp _ args -> any containsRecursiveType args
  TForall _ mb body -> maybe False containsRecursiveBound mb || containsRecursiveType body
  _ -> False
  where
    containsRecursiveBound bound = case bound of
      TArrow dom cod -> containsRecursiveType dom || containsRecursiveType cod
      TCon _ args -> any containsRecursiveType args
      TVarApp _ args -> any containsRecursiveType args
      TForall _ mb body -> maybe False containsRecursiveBound mb || containsRecursiveType body
      TMu _ _ -> True
      _ -> False

renameTypeVarInTerm :: String -> String -> ElabTerm -> ElabTerm
renameTypeVarInTerm old new term =
  let ty' = TVar new
      renameTy = substTypeCapture old ty'
      renameBound = mapBoundType renameTy
      renameScheme sch = schemeFromType (renameTy (schemeToType sch))
      renameName v
        | v == old = new
        | otherwise = v
      renameInst inst = case inst of
        InstId -> InstId
        InstApp ty -> InstApp (renameTy ty)
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstInside inner -> InstInside (renameInst inner)
        InstSeq a b -> InstSeq (renameInst a) (renameInst b)
        InstUnder v inner -> InstUnder (renameName v) (renameInst inner)
        InstBot ty -> InstBot (renameTy ty)
        InstAbstr v -> InstAbstr (renameName v)
   in case term of
        EVar _ -> term
        ELit _ -> term
        ELam v ty body -> ELam v (renameTy ty) (renameTypeVarInTerm old new body)
        EApp f a -> EApp (renameTypeVarInTerm old new f) (renameTypeVarInTerm old new a)
        ELet v sch rhs body -> ELet v (renameScheme sch) (renameTypeVarInTerm old new rhs) (renameTypeVarInTerm old new body)
        ETyAbs v mb body
          | v == old -> ETyAbs v (fmap renameBound mb) body
          | otherwise -> ETyAbs v (fmap renameBound mb) (renameTypeVarInTerm old new body)
        ETyInst t inst -> ETyInst (renameTypeVarInTerm old new t) (renameInst inst)
        ERoll ty body -> ERoll (renameTy ty) (renameTypeVarInTerm old new body)
        EUnroll body -> EUnroll (renameTypeVarInTerm old new body)

authoritativeRootAnn :: ElabTerm -> AnnExpr -> AnnExpr
authoritativeRootAnn term annExpr =
  case (stripLeadingTyAbs term, annExpr) of
    (term0, AAnn inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (term0, AUnfold inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (ELet termName _ _ bodyTerm, ALet annName _ _ _ _ _ bodyAnn _)
      | termName == annName ->
          authoritativeRootAnn bodyTerm bodyAnn
    (EApp (ELam param _ (EVar bodyVar)) argTerm, AApp _ argAnn _ _ _)
      | param == bodyVar ->
          authoritativeRootAnn argTerm argAnn
    (EVar termName, AApp _ argAnn _ _ _)
      | annProducesVar termName argAnn ->
          authoritativeRootAnn (EVar termName) argAnn
    _ -> annExpr

shouldStripAuthoritativeAnn :: ElabTerm -> Bool
shouldStripAuthoritativeAnn term =
  case term of
    ELet {} -> True
    EVar {} -> True
    EApp (ELam param _ (EVar bodyVar)) _ -> param == bodyVar
    _ -> False

annProducesVar :: Surface.VarName -> AnnExpr -> Bool
annProducesVar termName = go
  where
    go annExpr =
      case annExpr of
        AVar annName _ -> annName == termName
        AAnn inner _ _ -> go inner
        AUnfold inner _ _ -> go inner
        _ -> False

stripLeadingTyAbs :: ElabTerm -> ElabTerm
stripLeadingTyAbs term =
  case term of
    ETyAbs _ _ body -> stripLeadingTyAbs body
    _ -> term

{- Note [srcTypeToElabType in Pipeline]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Local copy of the NormSrcType → ElabType conversion used to build
   authoritative SchemeInfo for external environment bindings.  The
   canonical copy lives in MLF.Elab.Elaborate.Algebra (internal) and
   MLF.Frontend.Program.Elaborate (also internal, not exported).
   We keep this local to avoid widening production facades. -}

externalBindingSchemeInfo :: ExternalBinding -> Either ConstraintError SchemeInfo
externalBindingSchemeInfo ExternalBinding {externalBindingType = srcTy} = do
  ty <- srcTypeToElabType srcTy
  pure SchemeInfo {siScheme = schemeFromType ty, siSubst = IntMap.empty}

srcTypeToElabType :: NormSrcType -> Either ConstraintError ElabType
srcTypeToElabType ty = case ty of
  Surface.STVar name -> Right (TVar name)
  Surface.STArrow dom cod -> TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod
  Surface.STBase name -> Right (TBase (BaseTy name))
  Surface.STCon name args -> TCon (BaseTy name) <$> traverse srcTypeToElabType args
  Surface.STVarApp name args -> TVarApp name <$> traverse srcTypeToElabType args
  Surface.STTyLam {} ->
    Left (InternalConstraintError "residual type lambda reached elaboration")
  Surface.STTyApp {} ->
    Left (InternalConstraintError "residual type application reached elaboration")
  Surface.STForall name mb body ->
    TForall name
      <$> maybe (Right Nothing) srcBoundToElabBound mb
      <*> srcTypeToElabType body
  Surface.STMu name body -> TMu name <$> srcTypeToElabType body
  Surface.STBottom -> Right TBottom
  where
    srcBoundToElabBound :: Surface.SrcBound 'Surface.NormN -> Either ConstraintError (Maybe BoundType)
    srcBoundToElabBound (Surface.SrcBound boundTy) = structBoundToElabBound boundTy

    structBoundToElabBound :: StructBound -> Either ConstraintError (Maybe BoundType)
    structBoundToElabBound bTy = case bTy of
      Surface.STArrow dom cod -> Just <$> (TArrow <$> srcTypeToElabType dom <*> srcTypeToElabType cod)
      Surface.STBase name -> Right (Just (TBase (BaseTy name)))
      Surface.STCon name args -> Just . TCon (BaseTy name) <$> traverse srcTypeToElabType args
      Surface.STVarApp name args -> Just . TVarApp name <$> traverse srcTypeToElabType args
      Surface.STTyLam {} ->
        Left (InternalConstraintError "residual type lambda reached elaboration")
      Surface.STTyApp {} ->
        Left (InternalConstraintError "residual type application reached elaboration")
      Surface.STForall name mb body ->
        Just
          <$> ( TForall name
                  <$> maybe (Right Nothing) srcBoundToElabBound mb
                  <*> srcTypeToElabType body
              )
      Surface.STMu name body -> Just . TMu name <$> srcTypeToElabType body
      Surface.STBottom -> Right Nothing
