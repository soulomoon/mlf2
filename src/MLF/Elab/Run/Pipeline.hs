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
    preparedExternalTypeCheckEnv,
    extendPreparedExternalBindingTypeIdentities,
    restrictPreparedExternalBindings,
    unionPreparedExternalBindings,
    runPipelineElabDetailedWithEnv,
    runPipelineElabDetailedWithConfigAndEnv,
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedWithConfigAndExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedUncheckedWithExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming,
    freshenTypeAbsAgainstEnv,
    authoritativeRootAnn,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, rtsSupportsBoundThreads, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Foldable (Recursive (project))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
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
import MLF.Elab.Elaborate (ElabConfig, ElabEnv (..), elaborateWithEnv)
import MLF.Elab.Elaborate.Algebra (Env, mkEnvWithResolvedBindings)
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
    computePreparedResultTypeWithRootGeneralization,
    generalizePreparedRootDetailed,
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    preparedAnnotated,
    preparedElaborationConfig,
    preparedElaborationEnvWithInitialEnv,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    stripPreparedWitnesslessAuthoritativeAnn,
  )
import MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstRefsIfNeeded,
    preserveRetainedChildAuthoritativeResult,
    substInTermRefs,
  )
import MLF.Elab.TypeCheck (typeCheckWithEnv)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen
  ( AnnExpr (..),
    ConstraintError (..),
    ConstraintResult (..),
    ExternalBinding (..),
    ExternalBindingIdentity,
    externalBindingRuntimeName,
    externalBindingDetails,
    ExternalBindingMode (..),
    ExternalBindings,
    ExternalEnv,
    ModuleConstraintRoot (..),
    ModuleConstraintResult (..),
    generateConstraintsWithExternalBindings,
    generateModuleConstraintsKeyedWithExternalBindings,
  )
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (mergeSymbolIdentityMaps, mergeTypeBinderIdentityMaps)
import MLF.Frontend.Symbol (SymbolIdentity, lookupSymbolIdentityAlias, symbolIdentityAliasMap, symbolUniqueIdentity)
import MLF.Frontend.Syntax (NormSrcType, NormSurfaceExpr, StructBound, VarName)
import qualified MLF.Frontend.Syntax as Surface
import MLF.Reify.TypeOps (freeTypeVarAliasNamesType, freeTypeVarRefsType, freshNameLike, substTypeCaptureRef)
import MLF.Util.Timing
  ( TimingConfig,
    emitProgramOperationMetricIO,
    timeProgramOperationIO,
    timeProgramOperationWithSuffixIO,
    whenProgramOperationsIO,
  )
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Types.Identity
  ( EnvRef,
    IdDetails (..),
    IdentityGenerator,
    freshEnvRef,
    idDetailsAliasNames,
    idDetailsGeneratedIdentities,
    identityGeneratorAfter,
    localRefMatchesNodeId,
    typeBinderIdentityFromStructural,
    symbolGeneratedIdentities,
    StructuralTypeBinderRole (..),
    typeBinderGeneratedIdentities,
  )

data PipelineElabDetailedResult = PipelineElabDetailedResult
  { pedTerm :: XmlfTerm,
    pedType :: ElabType,
    pedRootAnn :: AnnExpr,
    pedTypeCheckEnv :: TypeCheck.Env
  }

data PreparedExternalBindings = PreparedExternalBindings
  { pebBindings :: ExternalBindings,
    pebSchemeInfos :: Map.Map VarName SchemeInfo,
    pebElaborationBindings :: Map.Map VarName (SchemeInfo, ResolvedVar),
    pebTypeCheckEnv :: TypeCheck.Env,
    pebSourceTypeHeadIdentities :: Map.Map String SymbolIdentity,
    pebSourceTypeBinderIdentities :: Map.Map String TypeBinderIdentity
  }

preparedExternalTypeCheckEnv :: PreparedExternalBindings -> TypeCheck.Env
preparedExternalTypeCheckEnv = pebTypeCheckEnv

preparedExternalElaborationEnv :: PreparedExternalBindings -> Env
preparedExternalElaborationEnv =
  mkEnvWithResolvedBindings . pebElaborationBindings

preparedElaborationEnvWithExternalIdentities ::
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabEnv 'Presolved
preparedElaborationEnvWithExternalIdentities annSourceTypes extPrepared artifact =
  (preparedElaborationEnvWithInitialEnv annSourceTypes (preparedExternalElaborationEnv extPrepared) artifact)
    { eeSourceTypeHeadIdentities = headIdentities,
      eeSourceTypeBinderIdentities = binderIdentities
    }
  where
    (headIdentities, binderIdentities) =
      preparedSourceTypeIdentityMaps extPrepared

preparedSourceTypeIdentityMaps ::
  PreparedExternalBindings ->
  (Map.Map String SymbolIdentity, Map.Map String TypeBinderIdentity)
preparedSourceTypeIdentityMaps prepared =
  ( pebSourceTypeHeadIdentities prepared,
    pebSourceTypeBinderIdentities prepared
  )

externalBindingsSourceTypeIdentityMaps ::
  ExternalBindings ->
  (Map.Map String SymbolIdentity, Map.Map String TypeBinderIdentity)
externalBindingsSourceTypeIdentityMaps extBindings =
  (headIdentities, binderIdentities)
  where
    bindings = Map.elems extBindings

    headIdentities =
      mergeSymbolIdentityMaps (map externalBindingTypeHeadIdentities bindings)

    binderIdentities =
      mergeTypeBinderIdentityMaps (structuralTypeBinderIdentitiesFromHeads headIdentities : map externalBindingTypeBinderIdentities bindings)

structuralTypeBinderIdentitiesFromHeads :: Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity
structuralTypeBinderIdentitiesFromHeads headIdentities =
  mergeTypeBinderIdentityMaps $
    [ Map.fromList
        [ ("$" ++ headName ++ "_self", typeBinderIdentityFromStructural (symbolUniqueIdentity identity) StructuralSelfBinder),
          ("$" ++ headName ++ "_result", typeBinderIdentityFromStructural (symbolUniqueIdentity identity) StructuralResultBinder)
        ]
    | (headName, identity) <- Map.toList (symbolIdentityAliasMap (Map.elems headIdentities))
    ]

data ModuleBatchPlan key p = ModuleBatchPlan
  { mbpRoots :: [(key, PreparedExternalBindings, ModuleConstraintRoot)],
    mbpPartitions :: [(key, RootPartition p)],
    mbpSharedEdgeCount :: !Int,
    mbpUnknownEdgeCount :: !Int
  }

data RootPartition p = RootPartition
  { rpRootId :: !ModuleRootId,
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
    rfcPreparedExternalBindings :: !PreparedExternalBindings
  }

type PipelineStage a = ExceptT PipelineError IO a

timePipelineValueSuffix ::
  TimingConfig ->
  String ->
  String ->
  IO a ->
  PipelineStage a
timePipelineValueSuffix timing label suffix action =
  liftIO (timeProgramOperationWithSuffixIO timing label suffix action)

timePipelineEither ::
  TimingConfig ->
  String ->
  IO (Either PipelineError a) ->
  PipelineStage a
timePipelineEither timing stageLabel action =
  ExceptT (timeProgramOperationIO timing stageLabel action)

timePipelineEitherSuffix ::
  TimingConfig ->
  String ->
  String ->
  IO (Either PipelineError a) ->
  PipelineStage a
timePipelineEitherSuffix timing label suffix action =
  ExceptT (timeProgramOperationWithSuffixIO timing label suffix action)

evaluatePipelineEitherSuffix ::
  TimingConfig ->
  String ->
  String ->
  Either PipelineError a ->
  PipelineStage a
evaluatePipelineEitherSuffix timing label suffix result =
  timePipelineEitherSuffix timing label suffix (evaluate result)

evaluatePipelineAttemptSuffix ::
  TimingConfig ->
  String ->
  String ->
  Either PipelineError a ->
  PipelineStage (Either PipelineError a)
evaluatePipelineAttemptSuffix timing label suffix result =
  timePipelineValueSuffix timing label suffix (evaluate result)

fromPipelineEither :: Either PipelineError a -> PipelineStage a
fromPipelineEither result =
  ExceptT (pure result)

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

runPipelineElab :: PolySyms -> NormSurfaceExpr -> Either PipelineError (XmlfTerm, ElabType)
runPipelineElab = runPipelineElabWithConfig defaultPipelineConfig

runPipelineElabWithConfig :: PipelineConfig -> PolySyms -> NormSurfaceExpr -> Either PipelineError (XmlfTerm, ElabType)
runPipelineElabWithConfig config polySyms expr =
  detailedPair <$> runPipelineElabWith FinalCheckInPipeline (resultTypeDiagnosticsFromConfig config) (pcTraceConfig config) polySyms Map.empty expr

-- | Run the pipeline with an external environment of type assumptions
-- for free variables, avoiding the ELamAnn wrapping approach.
runPipelineElabWithEnv :: PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError (XmlfTerm, ElabType)
runPipelineElabWithEnv = runPipelineElabWithConfigAndEnv defaultPipelineConfig

runPipelineElabWithConfigAndEnv :: PipelineConfig -> PolySyms -> ExternalEnv -> NormSurfaceExpr -> Either PipelineError (XmlfTerm, ElabType)
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
  runPipelineElabWith FinalCheckInPipeline (resultTypeDiagnosticsFromConfig config) (pcTraceConfig config) polySyms extBindings

runPipelineElabDetailedUncheckedWithExternalBindings :: PolySyms -> ExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedUncheckedWithExternalBindings polySyms extBindings =
  runPipelineElabWith FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig) polySyms extBindings

runPipelineElabDetailedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedWithPreparedExternalBindings =
  runPipelineElabWithPrepared FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig) (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithPreparedWithTiming timing label FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig) (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedUncheckedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedUncheckedWithPreparedExternalBindings =
  runPipelineElabWithPrepared FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithPreparedWithTiming timing label FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

schemeExternalBindings :: ExternalEnv -> ExternalBindings
schemeExternalBindings =
  Map.map
    ( \srcTy ->
        ExternalBinding
          { externalBindingType = srcTy,
            externalBindingMode = ExternalBindingScheme,
            externalBindingIdentity = Nothing,
            externalBindingTypeHeadIdentities = Map.empty,
            externalBindingTypeBinderIdentities = Map.empty
          }
    )

prepareExternalBindings :: ExternalBindings -> Either ConstraintError PreparedExternalBindings
prepareExternalBindings extBindings0 = do
  let extBindings = externalBindingsWithIdentityAliases extBindings0
      initialGenerator = identityGeneratorAfter (externalBindingsGeneratedIdentities extBindings0)
  (schemeGenerator, schemeInfos0) <- externalBindingSchemeInfos initialGenerator extBindings0
  let schemeInfos = externalBindingSchemeInfoAliases extBindings0 extBindings schemeInfos0
  let (elaborationBindings, typeCheckEnv0) = externalBindingPreparedEnvs schemeGenerator extBindings schemeInfos
      (headIdentities, binderIdentities) = externalBindingsSourceTypeIdentityMaps extBindings
  pure
    PreparedExternalBindings
      { pebBindings = extBindings,
        pebSchemeInfos = schemeInfos,
        pebElaborationBindings = elaborationBindings,
        pebTypeCheckEnv = typeCheckEnv0,
        pebSourceTypeHeadIdentities = headIdentities,
        pebSourceTypeBinderIdentities = binderIdentities
      }

externalBindingsWithIdentityAliases :: ExternalBindings -> ExternalBindings
externalBindingsWithIdentityAliases extBindings =
  extBindings `Map.union` Map.filterWithKey (\name _ -> Map.notMember name extBindings) uniqueAliases
  where
    uniqueAliases =
      Map.fromList
        [ (alias, binding)
        | (alias, binding : rest) <- Map.toList aliasesByName,
          all (== binding) rest
        ]

    aliasesByName =
      Map.fromListWith
        (++)
        [ (alias, [binding])
        | (name, binding) <- Map.toList extBindings,
          alias <- externalBindingAliases name binding
        ]

externalBindingSchemeInfoAliases :: ExternalBindings -> ExternalBindings -> Map.Map VarName SchemeInfo -> Map.Map VarName SchemeInfo
externalBindingSchemeInfoAliases originalBindings extBindings schemeInfos =
  schemeInfos `Map.union` Map.filterWithKey (\name _ -> Map.notMember name schemeInfos) uniqueAliases
  where
    uniqueAliases =
      Map.fromList
        [ (alias, schemeInfo)
        | (alias, (binding, schemeInfo) : rest) <- Map.toList aliasesByName,
          all ((== binding) . fst) rest
        ]

    aliasesByName =
      Map.fromListWith
        (++)
        [ (alias, [(binding, schemeInfo)])
        | (name, binding) <- Map.toList originalBindings,
          Just schemeInfo <- [Map.lookup name schemeInfos],
          alias <- externalBindingAliases name binding,
          Map.member alias extBindings
        ]

externalBindingAliases :: VarName -> ExternalBinding -> [VarName]
externalBindingAliases name binding =
  case externalBindingIdentity binding of
    Just identity ->
      Set.toList $
        Set.fromList $
          idDetailsAliasNames name (externalBindingDetails identity)
            ++ idDetailsAliasNames (externalBindingRuntimeName identity) (externalBindingDetails identity)
    Nothing -> [name]

extendPreparedExternalBindingTypeIdentities ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  PreparedExternalBindings ->
  PreparedExternalBindings
extendPreparedExternalBindingTypeIdentities headIdentities binderIdentities prepared =
  let heads =
        mergeSymbolIdentityMaps
          [pebSourceTypeHeadIdentities prepared, headIdentities]
      binders =
        mergeTypeBinderIdentityMaps
          [ pebSourceTypeBinderIdentities prepared,
            binderIdentities,
            structuralTypeBinderIdentitiesFromHeads heads
          ]
   in prepared
        { pebSourceTypeHeadIdentities = heads,
          pebSourceTypeBinderIdentities = binders
        }

restrictPreparedExternalBindings :: Set.Set VarName -> PreparedExternalBindings -> PreparedExternalBindings
restrictPreparedExternalBindings names prepared =
  let schemeInfos = Map.restrictKeys (pebSchemeInfos prepared) names
      elaborationBindings = Map.restrictKeys (pebElaborationBindings prepared) names
   in PreparedExternalBindings
        { pebBindings = Map.restrictKeys (pebBindings prepared) names,
          pebSchemeInfos = schemeInfos,
          pebElaborationBindings = elaborationBindings,
          pebTypeCheckEnv = restrictTypeCheckEnv elaborationBindings (pebTypeCheckEnv prepared),
          pebSourceTypeHeadIdentities = pebSourceTypeHeadIdentities prepared,
          pebSourceTypeBinderIdentities = pebSourceTypeBinderIdentities prepared
        }

unionPreparedExternalBindings :: PreparedExternalBindings -> PreparedExternalBindings -> PreparedExternalBindings
unionPreparedExternalBindings preferred fallback =
  let schemeInfos = pebSchemeInfos preferred `Map.union` pebSchemeInfos fallback
      heads =
        mergeSymbolIdentityMaps
          [pebSourceTypeHeadIdentities preferred, pebSourceTypeHeadIdentities fallback]
      binders =
        mergeTypeBinderIdentityMaps
          [ pebSourceTypeBinderIdentities preferred,
            pebSourceTypeBinderIdentities fallback,
            structuralTypeBinderIdentitiesFromHeads heads
          ]
   in PreparedExternalBindings
        { pebBindings = pebBindings preferred `Map.union` pebBindings fallback,
          pebSchemeInfos = schemeInfos,
          pebElaborationBindings = pebElaborationBindings preferred `Map.union` pebElaborationBindings fallback,
          pebTypeCheckEnv = unionTypeCheckEnv (pebTypeCheckEnv preferred) (pebTypeCheckEnv fallback),
          pebSourceTypeHeadIdentities = heads,
          pebSourceTypeBinderIdentities = binders
        }

externalBindingPreparedEnvs :: IdentityGenerator -> ExternalBindings -> Map.Map VarName SchemeInfo -> (Map.Map VarName (SchemeInfo, ResolvedVar), TypeCheck.Env)
externalBindingPreparedEnvs generator0 extBindings schemeInfos =
  (Map.fromList elaborationEntries, TypeCheck.mkTypeCheckEnvWithResolvedTerms typeCheckEntries Map.empty)
  where
    (_, elaborationEntries, typeCheckEntries) =
      foldl bindingEntry (generator0, [], []) (Map.toList schemeInfos)

    bindingEntry (generator, elabAcc, tcAcc) (name, schemeInfo) =
      let ty = schemeToType (siScheme schemeInfo)
       in case Map.lookup name extBindings >>= externalBindingIdentity of
            Just identity ->
              let resolved = resolvedExternalBindingVar identity schemeInfo
               in ( generator,
                    (name, (schemeInfo, resolved)) : elabAcc,
                    (resolved, ty) : tcAcc
                  )
            Nothing ->
              let (envRef, generator') = freshEnvRef name generator
                  resolved = resolvedGeneratedExternalBindingVar envRef name schemeInfo
               in ( generator',
                    (name, (schemeInfo, resolved)) : elabAcc,
                    (resolved, ty) : tcAcc
                  )

externalBindingsGeneratedIdentities :: ExternalBindings -> [UniqueIdentity]
externalBindingsGeneratedIdentities extBindings =
  [ identity
  | ExternalBinding {externalBindingIdentity = Just externalIdentity} <- Map.elems extBindings,
    identity <- idDetailsGeneratedIdentities (externalBindingDetails externalIdentity)
  ]
    ++ [ identity
       | ExternalBinding {externalBindingTypeBinderIdentities = binderIdentities} <- Map.elems extBindings,
         identity <- concatMap typeBinderGeneratedIdentities (Map.elems binderIdentities)
       ]
    ++ [ identity
       | ExternalBinding {externalBindingTypeHeadIdentities = headIdentities} <- Map.elems extBindings,
         identity <- concatMap symbolGeneratedIdentities (Map.elems headIdentities)
       ]

resolvedExternalBindingVar :: ExternalBindingIdentity -> SchemeInfo -> ResolvedVar
resolvedExternalBindingVar identity schemeInfo =
  ResolvedVar
    { resolvedVarRuntimeName = externalBindingRuntimeName identity,
      resolvedVarType = schemeToType (siScheme schemeInfo),
      resolvedVarDetails = externalBindingDetails identity
    }

resolvedGeneratedExternalBindingVar :: EnvRef -> VarName -> SchemeInfo -> ResolvedVar
resolvedGeneratedExternalBindingVar envRef name schemeInfo =
  ResolvedVar
    { resolvedVarRuntimeName = name,
      resolvedVarType = schemeToType (siScheme schemeInfo),
      resolvedVarDetails = EnvId envRef
    }

restrictTypeCheckEnv :: Map.Map VarName (SchemeInfo, ResolvedVar) -> TypeCheck.Env -> TypeCheck.Env
restrictTypeCheckEnv bindings env =
  TypeCheck.restrictResolvedTermBindings allowed env
  where
    allowed =
      [ resolved
      | (_, (_, resolved)) <- Map.toList bindings
      ]

unionTypeCheckEnv :: TypeCheck.Env -> TypeCheck.Env -> TypeCheck.Env
unionTypeCheckEnv preferred fallback =
  TypeCheck.unionEnvs preferred fallback

detailedPair :: PipelineElabDetailedResult -> (XmlfTerm, ElabType)
detailedPair result = (pedTerm result, pedType result)

data PipelineFinalCheckMode
  = FinalCheckInPipeline
  | FinalCheckAfterDeferredRewrite
  deriving (Eq, Show)

data ResultTypeDiagnosticsMode
  = ResultTypeDiagnosticsEnabled
  | ResultTypeDiagnosticsDisabled
  deriving (Eq, Show)

resultTypeDiagnosticsFromConfig :: PipelineConfig -> ResultTypeDiagnosticsMode
resultTypeDiagnosticsFromConfig config =
  if pcResultTypeDiagnostics config
    then ResultTypeDiagnosticsEnabled
    else ResultTypeDiagnosticsDisabled

shouldRunResultTypeDiagnostics :: PipelineFinalCheckMode -> ResultTypeDiagnosticsMode -> Bool
shouldRunResultTypeDiagnostics finalCheckMode diagnosticsMode =
  finalCheckMode == FinalCheckInPipeline && diagnosticsMode == ResultTypeDiagnosticsEnabled

runPipelineElabWith ::
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PolySyms ->
  ExternalBindings ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWith finalCheckMode diagnosticsMode traceCfg polySyms extBindings expr = do
  extPrepared <- fromConstraintError (prepareExternalBindings extBindings)
  runPipelineElabWithPrepared finalCheckMode diagnosticsMode traceCfg polySyms extPrepared expr

runPipelineElabWithPrepared ::
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PolySyms ->
  PreparedExternalBindings ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithPrepared finalCheckMode diagnosticsMode traceCfg polySyms extPrepared =
  runPipelineElabWithPreparedGenerated
    finalCheckMode
    diagnosticsMode
    traceCfg
    extPrepared
    (generateConstraintsWithExternalBindings polySyms (pebBindings extPrepared))

runPipelineElabWithPreparedGenerated ::
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  (NormSurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithPreparedGenerated finalCheckMode diagnosticsMode traceCfg extPrepared generateConstraints expr = do
  () <- fromConstraintError (validateDirectRecursiveAnnotations expr)
  ConstraintResult {crConstraint = c0, crAnnotated = ann, crAnnSourceTypes = annSourceTypes, crInitialEnv = _initialBindings} <-
    fromConstraintError (generateConstraints expr)
  let c1 = normalize c0
  (cAcyclic, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)
  pres <- fromPresolutionError (computePresolution traceCfg acyc cAcyclic)
  prepared <-
    fromSolveError $
      prepareGeneralizationArtifact traceCfg cAcyclic pres ann
  -- Use external schemes and identities from prepared bindings instead of
  -- re-generalizing through the constraint graph, which would produce
  -- graph-internal variable names that conflict with constructor types.
  let initialTcEnv = pebTypeCheckEnv extPrepared
      annCanon = preparedAnnotated prepared
      elabConfig = preparedElaborationConfig traceCfg prepared
      elabEnv =
        preparedElaborationEnvWithExternalIdentities annSourceTypes extPrepared prepared
  term <- fromElabError (elaborateWithEnv elabConfig elabEnv annCanon)
  case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
    () -> pure ()
  let authoritativeAnnCanon = authoritativeRootAnn term annCanon
      authoritativeAnnPre = authoritativeRootAnn term ann
      (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
        stripPreparedWitnesslessAuthoritativeAnn prepared authoritativeAnnCanon authoritativeAnnPre
  rootGeneralization <-
    fromElabError $
      generalizePreparedRootDetailed prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal
  let rootScheme0 = prgScheme rootGeneralization
      rootSubst = prgSubst rootGeneralization
      rootScheme = siScheme (schemeInfoFromRefSubst rootScheme0 rootSubst)
  let termSubst = substInTermRefs rootSubst term

  let termClosed = closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst
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

  -- Result-type reconstruction is an opt-in diagnostic cross-check; the final
  -- typechecker result stays authoritative on the default hot path.
  if shouldRunResultTypeDiagnostics finalCheckMode diagnosticsMode
    then do
      _ <-
        fromElabError
          ( computePreparedResultTypeWithRootGeneralization
              prepared
              rootGeneralization
              authoritativeAnnCanonFinal
              authoritativeAnnPreFinal
          )
      authoritativeResult
    else authoritativeResult

runPipelineElabWithPreparedWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PolySyms ->
  PreparedExternalBindings ->
  NormSurfaceExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedWithTiming timing label finalCheckMode diagnosticsMode traceCfg polySyms extPrepared =
  runPipelineElabWithPreparedGeneratedWithTiming
    timing
    label
    finalCheckMode
    diagnosticsMode
    traceCfg
    extPrepared
    (generateConstraintsWithExternalBindings polySyms (pebBindings extPrepared))

runPipelineElabWithPreparedGeneratedWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  (NormSurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedGeneratedWithTiming timing label finalCheckMode diagnosticsMode traceCfg extPrepared generateConstraints expr =
  runExceptT $ do
    evaluatePipelineEitherSuffix timing label ".validate_annotations" $
      fromConstraintError (validateDirectRecursiveAnnotations expr)
    ConstraintResult {crConstraint = c0, crAnnotated = ann, crAnnSourceTypes = annSourceTypes, crInitialEnv = _initialBindings} <-
      evaluatePipelineEitherSuffix timing label ".generate_constraints" $
        fromConstraintError (generateConstraints expr)
    normalizeResult <-
      timePipelineValueSuffix timing label ".constraint_normalize" $
        evaluate (normalize c0)
    (cAcyclic, acyc) <-
      evaluatePipelineEitherSuffix timing label ".acyclicity" $
        fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult)
    pres <-
      let presolutionLabel = label ++ ".presolution"
       in timePipelineEither timing presolutionLabel $
            fromPresolutionError <$> computePresolutionWithTiming timing presolutionLabel traceCfg acyc cAcyclic
    prepared <-
      evaluatePipelineEitherSuffix timing label ".prepare_generalization" $
        fromSolveError (prepareGeneralizationArtifact traceCfg cAcyclic pres ann)
    let annCanon = preparedAnnotated prepared
        elabConfig = preparedElaborationConfig traceCfg prepared
        elabEnv =
          preparedElaborationEnvWithExternalIdentities annSourceTypes extPrepared prepared
    finishPreparedPipelineRootStage
      timing
      label
      finalCheckMode
      diagnosticsMode
      traceCfg
      extPrepared
      prepared
      elabConfig
      elabEnv
      annCanon
      ann

runPipelineElabWithPreparedConstraintWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  Constraint 'Raw ->
  AnnExpr ->
  IntMap.IntMap NormSrcType ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedConstraintWithTiming timing label finalCheckMode diagnosticsMode traceCfg extPrepared c0 ann annSourceTypes =
  runExceptT $ do
    normalizeResult <-
      timePipelineValueSuffix timing label ".constraint_normalize" $
        evaluate (normalize c0)
    (cAcyclic, acyc) <-
      evaluatePipelineEitherSuffix timing label ".acyclicity" $
        fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult)
    pres <-
      let presolutionLabel = label ++ ".presolution"
       in timePipelineEither timing presolutionLabel $
            fromPresolutionError <$> computePresolutionWithTiming timing presolutionLabel traceCfg acyc cAcyclic
    prepared <-
      evaluatePipelineEitherSuffix timing label ".prepare_generalization" $
        fromSolveError (prepareGeneralizationArtifact traceCfg cAcyclic pres ann)
    readContextResult <-
      evaluatePipelineAttemptSuffix timing label ".root_finalization.prepare_read_context" $
        fromElabError (preparedReadContextReady prepared)
    resultTypeReadContextResult <-
      evaluatePipelineAttemptSuffix timing label ".root_finalization.result_type_read_context" $
        fromElabError (preparedResultTypeViewReady prepared)
    case (readContextResult, resultTypeReadContextResult) of
      (Left err, _) -> fromPipelineEither (Left err)
      (_, Left err) -> fromPipelineEither (Left err)
      (Right (), Right ()) -> pure ()
    let annCanon = preparedAnnotated prepared
        elabConfig = preparedElaborationConfig traceCfg prepared
        elabEnv =
          preparedElaborationEnvWithExternalIdentities annSourceTypes extPrepared prepared
    finishPreparedPipelineRootStage
      timing
      label
      finalCheckMode
      diagnosticsMode
      traceCfg
      extPrepared
      prepared
      elabConfig
      elabEnv
      annCanon
      ann

runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, NormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming =
  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTiming FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig)

runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, NormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming =
  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTiming FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled

runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTiming ::
  (Ord key) =>
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, NormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTiming finalCheckMode diagnosticsMode timing label polySyms extPrepared rootPrepared keyedNamedExprs =
  runExceptT $ do
    let traceCfg = pcTraceConfig defaultPipelineConfig
        namedExprs = [(name, expr) | (_, name, expr) <- keyedNamedExprs]
        rootPreparedForKey key =
          Map.findWithDefault extPrepared key rootPrepared
        rootPreparedSchemeUseCount =
          sum
            [ Map.size (pebSchemeInfos prepared)
            | (key, _, _) <- keyedNamedExprs,
              Just prepared <- [Map.lookup key rootPrepared]
            ]
    evaluatePipelineEitherSuffix timing label ".validate_annotations" $
      mapM_ (fromConstraintError . validateDirectRecursiveAnnotations . snd) namedExprs
    ModuleConstraintResult {mcrConstraint = c0, mcrRoots = roots, mcrAnnSourceTypes = annSourceTypes, mcrRootOwnership = rootOwnership} <-
      evaluatePipelineEitherSuffix timing label ".generate_constraints" $
        fromConstraintError (generateModuleConstraintsKeyedWithExternalBindings polySyms (pebBindings extPrepared) keyedNamedExprs)
    liftIO $
      whenProgramOperationsIO timing $
        emitModuleBatchGraphMetrics timing (label ++ ".graph") c0 rootOwnership roots annSourceTypes extPrepared rootPreparedSchemeUseCount
    let batchPlan = buildModuleBatchPlan rootPreparedForKey c0 rootOwnership roots annSourceTypes
    liftIO $
      whenProgramOperationsIO timing $
        emitModuleBatchPlanMetrics timing (label ++ ".partition") batchPlan
    if moduleBatchPlanRootLocalEligible batchPlan
      then
        ExceptT $
          runModuleBatchPlanRootLocalWithTiming timing (label ++ ".partitioned_roots") finalCheckMode diagnosticsMode traceCfg batchPlan
      else
        ExceptT $
          runModuleBatchPlanGlobalWithTiming timing label finalCheckMode diagnosticsMode traceCfg c0 rootOwnership (mbpRoots batchPlan) annSourceTypes

runModuleBatchPlanGlobalWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  Constraint 'Raw ->
  RootOwnershipIndex ->
  [(key, PreparedExternalBindings, ModuleConstraintRoot)] ->
  IntMap.IntMap NormSrcType ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runModuleBatchPlanGlobalWithTiming timing label finalCheckMode diagnosticsMode traceCfg c0 rootOwnership roots annSourceTypes =
  runExceptT $ do
    normalizeResult <-
      timePipelineValueSuffix timing label ".constraint_normalize" $
        evaluate (normalize c0)
    (cAcyclic, acyc) <-
      evaluatePipelineEitherSuffix timing label ".acyclicity" $
        fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult)
    pres <-
      let presolutionLabel = label ++ ".presolution"
       in timePipelineEither timing presolutionLabel $
            fromPresolutionError <$> computePresolutionWithTimingAndRootOwnership timing presolutionLabel traceCfg rootOwnership acyc cAcyclic
    prepared <-
      evaluatePipelineEitherSuffix timing label ".prepare_generalization" $
        fromSolveError $
          prepareGeneralizationArtifactForRoots
            traceCfg
            cAcyclic
            pres
            [mcrAnnotated root | (_, _, root) <- roots]
    let elabConfig = preparedElaborationConfig traceCfg prepared
        rootsLabel = label ++ ".roots"
    timePipelineEither timing rootsLabel $
      finishPreparedPipelineRootsWithTiming
        timing
        rootsLabel
        finalCheckMode
        diagnosticsMode
        traceCfg
        prepared
        elabConfig
        annSourceTypes
        roots

runModuleBatchPlanRootLocalWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  ModuleBatchPlan key 'Raw ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runModuleBatchPlanRootLocalWithTiming timing label finalCheckMode diagnosticsMode traceCfg plan =
  timeProgramOperationIO timing label $
    case mbpPartitions plan of
      [] -> pure (Right Map.empty)
      [_] -> runExceptT (goSequential (1 :: Int) Map.empty (mbpPartitions plan))
      partitions -> goConcurrent partitions
  where
    goSequential _ acc [] =
      pure acc
    goSequential index acc ((key, partition) : rest) = do
      out <-
        ExceptT $
          runRootFinalizationContextWithTiming
            timing
            (rootTimingLabel label index)
            finalCheckMode
            diagnosticsMode
            traceCfg
            (mkRootFinalizationContext partition)
      goSequential (index + 1) (Map.insert key out acc) rest

    goConcurrent partitions = do
      ensureConcurrentCapabilities (length partitions)
      workers <-
        mapM
          ( \(index, (key, partition)) -> do
              done <- newEmptyMVar
              _ <-
                forkIO $
                  try
                    ( runRootFinalizationContextWithTiming
                        timing
                        (rootTimingLabel label index)
                        finalCheckMode
                        diagnosticsMode
                        traceCfg
                        (mkRootFinalizationContext partition)
                    )
                    >>= putMVar done
              pure (key, done)
          )
          (zip [(1 :: Int) ..] partitions)
      settled <- mapM (\(key, done) -> (\result -> (key, result)) <$> takeMVar done) workers
      case [ex | (_, Left ex) <- settled] of
        ex : _ -> throwIO (ex :: SomeException)
        [] ->
          case [err | (_, Right (Left err)) <- settled] of
            err : _ -> pure (Left err)
            [] ->
              pure $
                Right $
                  Map.fromList
                    [ (key, out)
                    | (key, Right (Right out)) <- settled
                    ]

    mkRootFinalizationContext partition =
      RootFinalizationContext
        { rfcPartition = partition,
          rfcPreparedExternalBindings = rpPreparedExternalBindings partition
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

rootTimingLabel :: String -> Int -> String
rootTimingLabel label index =
  label ++ ".root_" ++ show index

runRootFinalizationContextWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  RootFinalizationContext 'Raw ->
  IO (Either PipelineError PipelineElabDetailedResult)
runRootFinalizationContextWithTiming
  timing
  label
  finalCheckMode
  diagnosticsMode
  traceCfg
  RootFinalizationContext
    { rfcPartition = partition,
      rfcPreparedExternalBindings = extPrepared
    } =
    runPipelineElabWithPreparedConstraintWithTiming
      timing
      label
      finalCheckMode
      diagnosticsMode
      traceCfg
      extPrepared
      (rpConstraint partition)
      (rpAnnotated partition)
      (rpAnnSourceTypes partition)

moduleBatchPlanRootLocalEligible :: ModuleBatchPlan key p -> Bool
moduleBatchPlanRootLocalEligible plan =
  mbpSharedEdgeCount plan == 0
    && mbpUnknownEdgeCount plan == 0
    && not (null (mbpPartitions plan))

buildModuleBatchPlan ::
  (key -> PreparedExternalBindings) ->
  Constraint 'Raw ->
  RootOwnershipIndex ->
  Map.Map key ModuleConstraintRoot ->
  IntMap.IntMap NormSrcType ->
  ModuleBatchPlan key 'Raw
buildModuleBatchPlan rootPrepared constraint rootOwnership roots annSourceTypes =
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
    orderedRoots =
      [ (key, rootPrepared key, root)
      | (key, root) <- Map.toList roots
      ]
    partitionBuckets = buildRootPartitionBuckets constraint rootOwnership [root | (_, _, root) <- orderedRoots]
    partitions =
      [ ( key,
          buildRootPartitionFromBucket
            constraint
            annSourceTypes
            rootExtPrepared
            root
            (IntMap.findWithDefault emptyRootPartitionBucket (getModuleRootId (mcrRootId root)) partitionBuckets)
        )
      | (key, rootExtPrepared, root) <- orderedRoots
      ]

buildRootPartitionBuckets ::
  Constraint 'Raw ->
  RootOwnershipIndex ->
  [ModuleConstraintRoot] ->
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
        | root <- orderedRoots
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

buildRootPartitionFromBucket ::
  Constraint 'Raw ->
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  ModuleConstraintRoot ->
  RootPartitionBucket ->
  RootPartition 'Raw
buildRootPartitionFromBucket constraint annSourceTypes rootExtPrepared root bucket =
  RootPartition
    { rpRootId = rootId,
      rpConstraint = partitionConstraint,
      rpAnnotated = mcrAnnotated root,
      rpAnnSourceTypes = IntMap.restrictKeys annSourceTypes (rpbNodeKeys bucket),
      rpPreparedExternalBindings = rootExtPrepared,
      rpOwnedEdgeCount = IntSet.size (rpbEdgeKeys bucket),
      rpExternalSchemeUseCount = Map.size (pebSchemeInfos rootExtPrepared)
    }
  where
    rootId = mcrRootId root
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

emitModuleBatchPlanMetrics :: TimingConfig -> String -> ModuleBatchPlan key p -> IO ()
emitModuleBatchPlanMetrics timing label plan =
  whenProgramOperationsIO timing $ do
    emitProgramOperationMetricIO timing (label ++ ".roots") (fromIntegral (length (mbpRoots plan)))
    emitProgramOperationMetricIO timing (label ++ ".shared_edges") (fromIntegral (mbpSharedEdgeCount plan))
    emitProgramOperationMetricIO timing (label ++ ".unknown_edges") (fromIntegral (mbpUnknownEdgeCount plan))
    emitProgramOperationMetricIO timing (label ++ ".root_local_enabled") (if moduleBatchPlanRootLocalEligible plan then 1 else 0)
    mapM_
      ( \(index, (_, partition)) -> do
          let rootLabel = rootTimingLabel label index
          emitProgramOperationMetricIO timing (rootLabel ++ ".owned_edges") (fromIntegral (rpOwnedEdgeCount partition))
          emitProgramOperationMetricIO timing (rootLabel ++ ".external_scheme_uses") (fromIntegral (rpExternalSchemeUseCount partition))
      )
      (zip [(1 :: Int) ..] (mbpPartitions plan))

emitModuleBatchGraphMetrics ::
  TimingConfig ->
  String ->
  Constraint p ->
  RootOwnershipIndex ->
  Map.Map key ModuleConstraintRoot ->
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  Int ->
  IO ()
emitModuleBatchGraphMetrics timing label constraint rootOwnership roots annSourceTypes extPrepared rootPreparedSchemeUseCount =
  whenProgramOperationsIO timing $ do
    emitProgramOperationMetricIO timing (label ++ ".roots") (fromIntegral (Map.size roots))
    emitProgramOperationMetricIO timing (label ++ ".nodes") (fromIntegral (IntMap.size (getNodeMap (cNodes constraint))))
    emitProgramOperationMetricIO timing (label ++ ".inst_edges") (fromIntegral (length (cInstEdges constraint)))
    emitProgramOperationMetricIO timing (label ++ ".unify_edges") (fromIntegral (length (cUnifyEdges constraint)))
    emitProgramOperationMetricIO timing (label ++ ".bind_parents") (fromIntegral (IntMap.size (cBindParents constraint)))
    emitProgramOperationMetricIO timing (label ++ ".annotation_roots") (fromIntegral (IntMap.size annSourceTypes))
    emitProgramOperationMetricIO timing (label ++ ".external_scheme_unique") (fromIntegral (Map.size (pebSchemeInfos extPrepared)))
    emitProgramOperationMetricIO timing (label ++ ".external_scheme_uses") (fromIntegral rootPreparedSchemeUseCount)
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
  (Ord key) =>
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  IntMap.IntMap NormSrcType ->
  [(key, PreparedExternalBindings, ModuleConstraintRoot)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
finishPreparedPipelineRootsWithTiming timing label finalCheckMode diagnosticsMode traceCfg prepared elabConfig annSourceTypes roots =
  runExceptT (go (1 :: Int) Map.empty roots)
  where
    go _ acc [] =
      pure acc
    go index acc ((key, rootExtPrepared, root) : rest) = do
      let elabEnv =
            preparedElaborationEnvWithExternalIdentities annSourceTypes rootExtPrepared prepared
          rootLabel = rootTimingLabel label index
      out <-
        ExceptT $
          finishPreparedPipelineRootWithTiming
            timing
            rootLabel
            finalCheckMode
            diagnosticsMode
            traceCfg
            rootExtPrepared
            prepared
            elabConfig
            elabEnv
            (mcrAnnotated root)
      go (index + 1) (Map.insert key out acc) rest

finishPreparedPipelineRootWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  ElabEnv 'Presolved ->
  AnnExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
finishPreparedPipelineRootWithTiming timing label finalCheckMode diagnosticsMode traceCfg extPrepared prepared elabConfig elabEnv annPre =
  runExceptT $
    finishPreparedPipelineRootStage
      timing
      label
      finalCheckMode
      diagnosticsMode
      traceCfg
      extPrepared
      prepared
      elabConfig
      elabEnv
      (canonicalizePreparedAnn prepared annPre)
      annPre

finishPreparedPipelineRootStage ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  ElabEnv 'Presolved ->
  AnnExpr ->
  AnnExpr ->
  PipelineStage PipelineElabDetailedResult
finishPreparedPipelineRootStage timing label finalCheckMode diagnosticsMode traceCfg extPrepared prepared elabConfig elabEnv annCanon annPre = do
  let initialTcEnv = pebTypeCheckEnv extPrepared
  term <-
    evaluatePipelineEitherSuffix timing label ".elaborate" $
      fromElabError (elaborateWithEnv elabConfig elabEnv annCanon)
  case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
    () -> pure ()
  let authoritativeAnnCanon = authoritativeRootAnn term annCanon
      authoritativeAnnPre = authoritativeRootAnn term annPre
      (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
        stripPreparedWitnesslessAuthoritativeAnn prepared authoritativeAnnCanon authoritativeAnnPre
  rootGeneralization <-
    evaluatePipelineEitherSuffix timing label ".generalize_root" $
      fromElabError (generalizePreparedRootDetailed prepared authoritativeAnnCanonFinal authoritativeAnnPreFinal)
  let rootScheme0 = prgScheme rootGeneralization
      rootSubst = prgSubst rootGeneralization
      rootScheme = siScheme (schemeInfoFromRefSubst rootScheme0 rootSubst)
  termSubst <-
    timePipelineValueSuffix timing label ".subst_root" $
      evaluate (substInTermRefs rootSubst term)
  termClosed <-
    timePipelineValueSuffix timing label ".close_term" $
      evaluate (closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst)
  termClosedFresh <-
    timePipelineValueSuffix timing label ".freshen_type_abs" $
      evaluate (freshenTypeAbsAgainstEnv initialTcEnv termClosed)
  let uncheckedAuthoritative =
        PipelineElabDetailedResult
          { pedTerm = termClosedFresh,
            pedType = schemeToType rootScheme,
            pedRootAnn = authoritativeAnnCanonFinal,
            pedTypeCheckEnv = initialTcEnv
          }
  authoritativeResult <-
    case finalCheckMode of
      FinalCheckInPipeline ->
        timePipelineValueSuffix timing label ".final_typecheck" $
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
        pure (Right uncheckedAuthoritative)
  if shouldRunResultTypeDiagnostics finalCheckMode diagnosticsMode
    then do
      _ <-
        evaluatePipelineEitherSuffix timing label ".result_type_reconstruction" $
          fromElabError (computePreparedResultTypeWithRootGeneralization prepared rootGeneralization authoritativeAnnCanonFinal authoritativeAnnPreFinal)
      fromPipelineEither authoritativeResult
    else fromPipelineEither authoritativeResult

closePipelineTerm :: TypeCheck.Env -> IntMap.IntMap TypeBinderRef -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst =
  let retainedChildAuthoritativeCandidate =
        case preserveRetainedChildAuthoritativeResult termSubst of
          Just _ -> True
          Nothing -> False
      termClosed0 =
        if retainedChildAuthoritativeCandidate
          then closeTermWithSchemeSubstRefsIfNeeded rootSubst rootScheme term
          else case typeCheckWithEnv initialTcEnv termSubst of
            Right ty ->
              let freeTyVarRefs = freeTypeVarRefsType ty
               in if null freeTyVarRefs
                    then termSubst
                    else
                      if null (schemeBinderRefs rootScheme)
                        then
                          let freeBinds =
                                [ (ref, Nothing)
                                  | ref <- freeTyVarRefs
                                ]
                              freeScheme = mkElabSchemeWithRefs freeBinds ty
                           in closeTermWithSchemeSubstRefsIfNeeded IntMap.empty freeScheme termSubst
                        else closeTermWithSchemeSubstRefsIfNeeded rootSubst rootScheme term
            Left _ -> closeTermWithSchemeSubstRefsIfNeeded rootSubst rootScheme term
   in case preserveRetainedChildAuthoritativeResult termClosed0 of
        Just termAdjusted ->
          closeRetainedChildAuthoritativeTerm initialTcEnv rootSubst rootScheme termAdjusted
        Nothing
          | retainedChildAuthoritativeCandidate ->
              closeRetainedChildAuthoritativeTerm initialTcEnv rootSubst rootScheme termClosed0
        Nothing -> termClosed0

closeRetainedChildAuthoritativeTerm :: TypeCheck.Env -> IntMap.IntMap TypeBinderRef -> ElabScheme -> XmlfTerm -> XmlfTerm
closeRetainedChildAuthoritativeTerm initialTcEnv rootSubst rootScheme termAdjusted =
  let closed = closeTermWithSchemeSubstRefsIfNeeded rootSubst rootScheme termAdjusted
   in if retainedChildCanUseRepresentativeScheme rootScheme
        then case retainedChildRepresentativeTerm initialTcEnv closed of
          Just representativeClosed -> representativeClosed
          Nothing
            | retainedChildIdentityRootScheme rootScheme ->
                case retainedChildRepresentativeTerm initialTcEnv termAdjusted of
                  Just representativeAdjusted -> representativeAdjusted
                  Nothing -> closed
          Nothing -> closed
        else closed

retainedChildCanUseRepresentativeScheme :: ElabScheme -> Bool
retainedChildCanUseRepresentativeScheme rootScheme =
  null (schemeBinderRefs rootScheme) || retainedChildIdentityRootScheme rootScheme

retainedChildIdentityRootScheme :: ElabScheme -> Bool
retainedChildIdentityRootScheme rootScheme = case schemeToType rootScheme of
  TForallRef ref Nothing (TVarRef bodyRef) -> typeBinderRefsSameIdentity ref bodyRef
  _ -> False

retainedChildRepresentativeTerm :: TypeCheck.Env -> XmlfTerm -> Maybe XmlfTerm
retainedChildRepresentativeTerm initialTcEnv term =
  case typeCheckWithEnv initialTcEnv term of
    Right ty
      | containsRecursiveType ty,
        countLeadingUnboundedForalls ty == 0 ->
          let representativeScheme = retainedChildRepresentativeScheme term ty
              representativeClosed =
                closeTermWithSchemeSubstRefsIfNeeded IntMap.empty representativeScheme term
           in case typeCheckWithEnv initialTcEnv representativeClosed of
                Right representativeTy
                  | countLeadingUnboundedForalls representativeTy == 2 ->
                      Just representativeClosed
                _ -> Nothing
    _ -> Nothing

retainedChildRepresentativeScheme :: XmlfTerm -> ElabType -> ElabScheme
retainedChildRepresentativeScheme term ty =
  let generator0 = identityGeneratorAfterTerm term
      used0 = freeTypeVarAliasNamesType ty
      (outerRef, generator1) = freshTypeBinderRefFromNames used0 generator0
      used1 = typeBinderRefAliasNames outerRef `Set.union` used0
      (innerRef, _generator2) = freshTypeBinderRefFromNames used1 generator1
   in mkElabSchemeWithRefs [(outerRef, Nothing), (innerRef, Nothing)] ty

countLeadingUnboundedForalls :: ElabType -> Int
countLeadingUnboundedForalls ty = case ty of
  TForallRef _ Nothing body -> 1 + countLeadingUnboundedForalls body
  _ -> 0

freshenTypeAbsAgainstEnv :: TypeCheck.Env -> XmlfTerm -> XmlfTerm
freshenTypeAbsAgainstEnv env term0 =
  let summary = summarizePipelineTypeCheckEnv env
      visibleRefs = pipelineVisibleTypeVarRefs summary
      seedTerm = foldr (`ETyAbsRef` Nothing) term0 visibleRefs
      generator0 = identityGeneratorAfterTerm seedTerm
      (term1, _) = go generator0 (pipelineFreshenReservedTypeVars summary) visibleRefs term0
   in pruneVacuousLeadingTyAbsAgainstEnv summary env term1
  where
    go generator used visibleRefs term = case term of
      ETyAbsRef ref mb body ->
        let name = typeBinderRefName ref
            usedForBinder = Set.union used (maybe Set.empty freeTypeVarAliasNamesType mb)
            refInScope =
              any (typeBinderRefsSameIdentity ref) visibleRefs
            needsFreshening =
              refInScope || Set.member name usedForBinder
            (ref', generator', bodyForName) =
              if needsFreshening
                then
                  let fresh = freshNameLike name usedForBinder
                      (freshRef, generator1) =
                        if refInScope
                          then freshTypeBinderRef fresh generator
                          else (renameTypeBinderRef fresh ref, generator)
                      bodyRenamed = renameTypeVarInTerm ref freshRef body
                   in (freshRef, generator1, bodyRenamed)
                else (ref, generator, body)
            usedBody = typeBinderRefAliasNames ref' `Set.union` usedForBinder
            visibleBody = unionTypeRefs [ref'] visibleRefs
            (body', generator'') = go generator' usedBody visibleBody bodyForName
         in (ETyAbsRef ref' mb body', generator'')
      ELam resolved body ->
        let ty = resolvedVarType resolved
            used' = Set.union used (freeTypeVarAliasNamesType ty)
            visibleRefs' = unionTypeRefs (freeTypeVarRefsType ty) visibleRefs
            (body', generator') = go generator used' visibleRefs' body
         in (ELam resolved body', generator')
      EApp f a ->
        let (f', generator') = go generator used visibleRefs f
            (a', generator'') = go generator' used visibleRefs a
         in (EApp f' a', generator'')
      ELet resolved sch rhs body ->
        let ty = schemeToType sch
            used' = Set.union used (freeTypeVarAliasNamesType ty)
            visibleRefs' = unionTypeRefs (freeTypeVarRefsType ty) visibleRefs
            (rhs', generator') = go generator used' visibleRefs' rhs
            (body', generator'') = go generator' used' visibleRefs' body
         in (ELet resolved sch rhs' body', generator'')
      ETyInst t inst ->
        let (t', generator') = go generator used visibleRefs t
         in (ETyInst t' inst, generator')
      ERoll ty body ->
        let (body', generator') = go generator used visibleRefs body
         in (ERoll ty body', generator')
      EUnroll body ->
        let (body', generator') = go generator used visibleRefs body
         in (EUnroll body', generator')
      _ -> (term, generator)

data PipelineTypeCheckEnvSummary = PipelineTypeCheckEnvSummary
  { ptcesTermFreeVars :: FreeVarCounts,
    ptcesTypeFreeVars :: FreeVarCounts,
    ptcesTypeRefs :: [TypeBinderRef]
  }

newtype FreeVarCounts = FreeVarCounts [(TypeBinderRef, Int)]

summarizePipelineTypeCheckEnv :: TypeCheck.Env -> PipelineTypeCheckEnvSummary
summarizePipelineTypeCheckEnv env =
  PipelineTypeCheckEnvSummary
    { ptcesTermFreeVars = freeVarCountsFromTypes (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv env))),
      ptcesTypeFreeVars = freeVarCountsFromTypes (Map.elems (TypeCheck.typeEnv env)),
      ptcesTypeRefs = Map.keys (TypeCheck.typeEnv env)
    }

insertPipelineTypeSummary :: TypeBinderRef -> ElabType -> TypeCheck.Env -> PipelineTypeCheckEnvSummary -> PipelineTypeCheckEnvSummary
insertPipelineTypeSummary ref ty env summary =
  summary
    { ptcesTypeFreeVars =
        replaceTypeFreeVars (TypeCheck.lookupTypeBindingRef ref env) ty (ptcesTypeFreeVars summary),
      ptcesTypeRefs = unionTypeRefs [ref] (ptcesTypeRefs summary)
    }

pipelineFreshenReservedTypeVars :: PipelineTypeCheckEnvSummary -> Set.Set String
pipelineFreshenReservedTypeVars summary =
  freeVarCountsNames (ptcesTermFreeVars summary)
    `Set.union` typeVarRefAliasNames (ptcesTypeRefs summary)

typeVarRefAliasNames :: [TypeBinderRef] -> Set.Set String
typeVarRefAliasNames =
  Set.unions . map typeBinderRefAliasNames

pipelineVisibleTypeVarRefs :: PipelineTypeCheckEnvSummary -> [TypeBinderRef]
pipelineVisibleTypeVarRefs summary =
  unionTypeRefs
    (freeVarCountsRefs (ptcesTermFreeVars summary))
    ( unionTypeRefs
        (freeVarCountsRefs (ptcesTypeFreeVars summary))
        (ptcesTypeRefs summary)
    )

freeVarCountsFromTypes :: [ElabType] -> FreeVarCounts
freeVarCountsFromTypes =
  foldl' (\counts ty -> insertFreeVarRefs (freeTypeVarRefsType ty) counts) emptyFreeVarCounts

emptyFreeVarCounts :: FreeVarCounts
emptyFreeVarCounts = FreeVarCounts []

freeVarCountsRefs :: FreeVarCounts -> [TypeBinderRef]
freeVarCountsRefs (FreeVarCounts counts) = map fst counts

freeVarCountsNames :: FreeVarCounts -> Set.Set String
freeVarCountsNames =
  typeVarRefAliasNames . freeVarCountsRefs

replaceTypeFreeVars :: Maybe ElabType -> ElabType -> FreeVarCounts -> FreeVarCounts
replaceTypeFreeVars oldTy newTy =
  insertFreeVarRefs (freeTypeVarRefsType newTy)
    . maybe id (deleteFreeVarRefs . freeTypeVarRefsType) oldTy

insertFreeVarRefs :: [TypeBinderRef] -> FreeVarCounts -> FreeVarCounts
insertFreeVarRefs refs (FreeVarCounts counts) =
  FreeVarCounts (foldl' insertOne counts refs)
  where
    insertOne [] ref = [(ref, 1)]
    insertOne ((existing, count) : rest) ref
      | typeBinderRefsSameIdentity existing ref = (existing, count + 1) : rest
      | otherwise = (existing, count) : insertOne rest ref

deleteFreeVarRefs :: [TypeBinderRef] -> FreeVarCounts -> FreeVarCounts
deleteFreeVarRefs refs (FreeVarCounts counts) =
  FreeVarCounts (foldl' deleteOne counts refs)
  where
    deleteOne [] _ = []
    deleteOne ((existing, count) : rest) ref
      | typeBinderRefsSameIdentity existing ref =
          let count' = count - 1
           in if count' <= 0 then rest else (existing, count') : rest
      | otherwise = (existing, count) : deleteOne rest ref

unionTypeRefs :: [TypeBinderRef] -> [TypeBinderRef] -> [TypeBinderRef]
unionTypeRefs left right =
  foldr insertRef right left
  where
    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

pruneVacuousLeadingTyAbsAgainstEnv :: PipelineTypeCheckEnvSummary -> TypeCheck.Env -> XmlfTerm -> XmlfTerm
pruneVacuousLeadingTyAbsAgainstEnv summary env term = case term of
  ETyAbsRef ref mb body ->
    let boundTy = maybe TBottom tyToElab mb
        summary' = insertPipelineTypeSummary ref boundTy env summary
        env' = TypeCheck.insertTypeBindingRef ref boundTy env
        body' = pruneVacuousLeadingTyAbsAgainstEnv summary' env' body
     in case typeCheckWithEnv env' body' of
          Right bodyTy
            | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType bodyTy)),
              not (containsRecursiveType bodyTy) ->
                case mb of
                  Nothing -> pruneVacuousLeadingTyAbsAgainstEnv summary env body'
                  Just _ ->
                    case
                      [ freeRef
                        | freeRef <- freeTypeVarRefsType bodyTy,
                          not (any (typeBinderRefsSameIdentity freeRef) (pipelineVisibleTypeVarRefs summary))
                      ]
                    of
                      [freeRef] ->
                        let bodyRenamed = renameTypeVarInTerm freeRef ref body'
                         in case typeCheckWithEnv env' bodyRenamed of
                              Right renamedTy
                                | any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType renamedTy) ->
                                    ETyAbsRef ref mb bodyRenamed
                              _ -> pruneVacuousLeadingTyAbsAgainstEnv summary env body'
                      _ -> pruneVacuousLeadingTyAbsAgainstEnv summary env body'
          _ -> ETyAbsRef ref mb body'
  _ -> term

containsRecursiveType :: ElabType -> Bool
containsRecursiveType ty = case ty of
  TMuRef {} -> True
  TArrow dom cod -> containsRecursiveType dom || containsRecursiveType cod
  TConWithIdentity _ _ args -> any containsRecursiveType args
  TVarAppRef _ args -> any containsRecursiveType args
  TForallRef _ mb body -> maybe False containsRecursiveBound mb || containsRecursiveType body
  _ -> False
  where
    containsRecursiveBound bound = case bound of
      TArrow dom cod -> containsRecursiveType dom || containsRecursiveType cod
      TConWithIdentity _ _ args -> any containsRecursiveType args
      TVarAppRef _ args -> any containsRecursiveType args
      TForallRef _ mb body -> maybe False containsRecursiveBound mb || containsRecursiveType body
      TMuRef {} -> True
      _ -> False

renameTypeVarInTerm :: TypeBinderRef -> TypeBinderRef -> XmlfTerm -> XmlfTerm
renameTypeVarInTerm oldRef newRef term =
  let renameTy = substTypeCaptureRef oldRef (TVarRef newRef)
      renameBound = mapBoundType renameTy
      renameScheme sch = schemeFromType (renameTy (schemeToType sch))
      renameRef ref
        | typeBinderRefsSameIdentity ref oldRef = newRef
        | otherwise = ref
      renameInst inst = case project inst of
        InstIdF -> InstId
        InstAppF ty -> InstApp (renameTy ty)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstInsideF inner -> InstInside (renameInst inner)
        InstSeqF a b -> InstSeq (renameInst a) (renameInst b)
        InstUnderFRef ref inner -> instUnderWithRef (renameRef ref) (renameInst inner)
        InstBotF ty -> InstBot (renameTy ty)
        InstAbstrFRef ref -> instAbstrWithRef (renameRef ref)
   in case project term of
        EVarNodeF resolved -> EVarNode (mapResolvedVarType renameTy resolved)
        ELitF lit -> ELit lit
        ELamF resolved body ->
          ELam
            (mapResolvedVarType renameTy resolved)
            (renameTypeVarInTerm oldRef newRef body)
        EAppF f a -> EApp (renameTypeVarInTerm oldRef newRef f) (renameTypeVarInTerm oldRef newRef a)
        ELetF resolved sch rhs body ->
          ELet
            (mapResolvedVarType renameTy resolved)
            (renameScheme sch)
            (renameTypeVarInTerm oldRef newRef rhs)
            (renameTypeVarInTerm oldRef newRef body)
        ETyAbsFRef ref mb body
          | typeBinderRefsSameIdentity ref oldRef -> eTyAbsWithRef ref (fmap renameBound mb) body
          | otherwise -> eTyAbsWithRef ref (fmap renameBound mb) (renameTypeVarInTerm oldRef newRef body)
        ETyInstF t inst -> ETyInst (renameTypeVarInTerm oldRef newRef t) (renameInst inst)
        ERollF ty body -> ERoll (renameTy ty) (renameTypeVarInTerm oldRef newRef body)
        EUnrollF body -> EUnroll (renameTypeVarInTerm oldRef newRef body)

authoritativeRootAnn :: XmlfTerm -> AnnExpr -> AnnExpr
authoritativeRootAnn term annExpr =
  case (stripLeadingTyAbs term, annExpr) of
    (term0, AAnn inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (term0, AUnfold inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (ELet resolved _ _ bodyTerm, ALet _ _ schemeRootId _ _ _ bodyAnn _)
      | resolvedVarMatchesAnnNode resolved schemeRootId ->
          authoritativeRootAnn bodyTerm bodyAnn
    (EApp (ELam param (EVarNode bodyVar)) argTerm, AApp _ argAnn _ _ _)
      | sameResolvedLocalVar param bodyVar ->
          authoritativeRootAnn argTerm argAnn
    (EVarNode resolved, AApp _ argAnn _ _ _)
      | annProducesResolvedVar resolved argAnn ->
          authoritativeRootAnn (EVarNode resolved) argAnn
    _ -> annExpr

shouldStripAuthoritativeAnn :: XmlfTerm -> Bool
shouldStripAuthoritativeAnn term =
  case term of
    ELet {} -> True
    EVarNode {} -> True
    EApp (ELam param (EVarNode bodyVar)) _ ->
      sameResolvedLocalVar param bodyVar
    _ -> False

annProducesResolvedVar :: ResolvedVar -> AnnExpr -> Bool
annProducesResolvedVar resolved = go
  where
    go annExpr =
      case annExpr of
        AVar _ nodeId -> resolvedVarMatchesAnnNode resolved nodeId
        AAnn inner _ _ -> go inner
        AUnfold inner _ _ -> go inner
        _ -> False

resolvedVarMatchesAnnNode :: ResolvedVar -> NodeId -> Bool
resolvedVarMatchesAnnNode resolved nodeId =
  case resolvedVarDetails resolved of
    LocalId ref -> localRefMatchesNodeId ref nodeId
    EvidenceId ref -> localRefMatchesNodeId ref nodeId
    _ -> False

sameResolvedLocalVar :: ResolvedVar -> ResolvedVar -> Bool
sameResolvedLocalVar left right =
  resolvedVarIsLocal left
    && resolvedVarIsLocal right
    && resolvedVarSameIdentity left right

stripLeadingTyAbs :: XmlfTerm -> XmlfTerm
stripLeadingTyAbs term =
  case term of
    ETyAbsRef _ _ body -> stripLeadingTyAbs body
    _ -> term

{- Note [srcTypeToElabType in Pipeline]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Local copy of the NormSrcType → ElabType conversion used to build
   authoritative SchemeInfo for external environment bindings.  The
   canonical copy lives in MLF.Elab.Elaborate.Algebra (internal) and
   MLF.Frontend.Program.Elaborate (also internal, not exported).
   We keep this local to avoid widening production facades. -}

externalBindingSchemeInfos :: IdentityGenerator -> ExternalBindings -> Either ConstraintError (IdentityGenerator, Map.Map VarName SchemeInfo)
externalBindingSchemeInfos generator0 extBindings =
  foldM addBinding (generator0, Map.empty) (Map.toList extBindings)
  where
    addBinding (generator, acc) (name, binding) = do
      (schemeInfo, generator') <- externalBindingSchemeInfoWithGenerator generator binding
      pure (generator', Map.insert name schemeInfo acc)

externalBindingSchemeInfoWithGenerator :: IdentityGenerator -> ExternalBinding -> Either ConstraintError (SchemeInfo, IdentityGenerator)
externalBindingSchemeInfoWithGenerator generator0 ExternalBinding {externalBindingType = srcTy, externalBindingTypeHeadIdentities = headIdentities, externalBindingTypeBinderIdentities = binderIdentities} = do
  (scheme, generator) <- srcTypeToElabSchemeWithFresh headIdentities binderIdentities generator0 srcTy
  pure (schemeInfoFromRefSubst scheme IntMap.empty, generator)

srcTypeToElabSchemeWithFresh :: Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> IdentityGenerator -> NormSrcType -> Either ConstraintError (ElabScheme, IdentityGenerator)
srcTypeToElabSchemeWithFresh headIdentities binderIdentities generator0 srcTy = do
  let (refs, generator1) = sourceTypeBinderRefsFromIdentities binderIdentities (Set.toList (freeSrcTypeVars srcTy)) generator0
  (ty, generator2) <- srcTypeToElabTypeWith headIdentities binderIdentities refs generator1 srcTy
  let explicitScheme = schemeFromType ty
      explicitRefs = map fst (schemeBinderRefs explicitScheme)
      freeBinds =
        [ (ref, Nothing)
        | ref <- Map.elems refs,
          any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType (schemeBody explicitScheme)),
          not (any (typeBinderRefsSameIdentity ref) explicitRefs)
        ]
  pure (mkElabSchemeWithRefs (freeBinds ++ schemeBinderRefs explicitScheme) (schemeBody explicitScheme), generator2)

freeSrcTypeVars :: Surface.SrcTy n v -> Set.Set String
freeSrcTypeVars ty =
  go Set.empty ty
  where
    go :: Set.Set String -> Surface.SrcTy n0 v0 -> Set.Set String
    go bound srcTy =
      case srcTy of
        Surface.STVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        Surface.STArrow dom cod -> go bound dom `Set.union` go bound cod
        Surface.STBase {} -> Set.empty
        Surface.STCon _ args -> foldMap (go bound) args
        Surface.STVarApp name args ->
          let headVars =
                if name `Set.member` bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        Surface.STTyLam name body -> go (Set.insert name bound) body
        Surface.STTyApp fun arg -> go bound fun `Set.union` go bound arg
        Surface.STForall name mb body ->
          maybe Set.empty (go bound . Surface.unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        Surface.STMu name body -> go (Set.insert name bound) body
        Surface.STBottom -> Set.empty

srcTypeToElabTypeWith :: Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> Map.Map String TypeBinderRef -> IdentityGenerator -> NormSrcType -> Either ConstraintError (ElabType, IdentityGenerator)
srcTypeToElabTypeWith =
  srcTypeToElabTypeWithBound Set.empty

srcTypeToElabTypeWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ConstraintError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator ty = case ty of
  Surface.STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (TVarRef ref, generator)
  Surface.STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator1 cod
    Right (TArrow dom' cod', generator2)
  Surface.STBase name -> Right (TBaseWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name), generator)
  Surface.STCon name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    Right (TConWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name) args', generator')
  Surface.STVarApp name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    ref <- sourceTypeBinderRef refs name
    Right (TVarAppRef ref args', generator')
  Surface.STTyLam {} ->
    Left (InternalConstraintError "residual type lambda reached elaboration")
  Surface.STTyApp {} ->
    Left (InternalConstraintError "residual type application reached elaboration")
  Surface.STForall name mb body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWith boundNames refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator2 body
          Right (TForallRef ref mb' body', generator3)
  Surface.STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities (Map.insert name ref refs) generator1 body
          Right (TMuRef ref body', generator2)
  Surface.STBottom -> Right (TBottom, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InternalConstraintError ("unresolved source type binder `" ++ name ++ "` reached pipeline external binding preparation"))

    sourceTypeHeadIdentity name =
      lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name

    srcTypesToElabTypesWith boundNames' refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

    srcBoundToElabBoundWith :: Set.Set String -> Map.Map String TypeBinderRef -> IdentityGenerator -> Surface.SrcBound 'Surface.NormN -> Either ConstraintError (Maybe BoundType, IdentityGenerator)
    srcBoundToElabBoundWith boundNames' refs' generator0 (Surface.SrcBound boundTy) = structBoundToElabBoundWith boundNames' refs' generator0 boundTy

    structBoundToElabBoundWith :: Set.Set String -> Map.Map String TypeBinderRef -> IdentityGenerator -> StructBound -> Either ConstraintError (Maybe BoundType, IdentityGenerator)
    structBoundToElabBoundWith boundNames' refs' generator0 bTy = case bTy of
      Surface.STArrow dom cod -> do
        (dom', generator1) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator0 dom
        (cod', generator2) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator1 cod
        Right (Just (TArrow dom' cod'), generator2)
      Surface.STBase name -> Right (Just (TBaseWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name)), generator0)
      Surface.STCon name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        Right (Just (TConWithIdentity (sourceTypeHeadIdentity name) (builtinBaseTy name) args'), generator1)
      Surface.STVarApp name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        ref <- sourceTypeBinderRef refs' name
        Right (Just (TVarAppRef ref args'), generator1)
      Surface.STTyLam {} ->
        Left (InternalConstraintError "residual type lambda reached elaboration")
      Surface.STTyApp {} ->
        Left (InternalConstraintError "residual type application reached elaboration")
      Surface.STForall name mb body ->
        let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames') binderIdentities name generator0
            refs'' = Map.insert name ref refs'
            boundNames'' = Set.insert name boundNames'
         in do
              (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWith boundNames' refs' generator1) mb
              (body', generator3) <- srcTypeToElabTypeWithBound boundNames'' headIdentities binderIdentities refs'' generator2 body
              Right (Just (TForallRef ref mb' body'), generator3)
      Surface.STMu name body ->
        let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames') binderIdentities name generator0
            boundNames'' = Set.insert name boundNames'
         in do
              (body', generator2) <- srcTypeToElabTypeWithBound boundNames'' headIdentities binderIdentities (Map.insert name ref refs') generator1 body
              Right (Just (TMuRef ref body'), generator2)
      Surface.STBottom -> Right (Nothing, generator0)

builtinBaseTy :: String -> BaseTy
builtinBaseTy =
  BaseTy . Builtins.normalizeBuiltinTypeReference
