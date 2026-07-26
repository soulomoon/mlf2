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
    prepareExternalBindingsWithTypeIdentities,
    preparedExternalTypeCheckEnv,
    preparedSourceTypeIdentityMaps,
    preparedSourceTypeBinderIdentityCandidates,
    extendPreparedExternalBindingTypeIdentities,
    extendPreparedExternalBindingTypeIdentityCandidates,
    preferPreparedExternalBindingTypeIdentities,
    reservePreparedExternalBindingIdentities,
    restrictPreparedExternalBindings,
    restrictPreparedExternalBindingsByKeys,
    unionPreparedExternalBindings,
    runPipelineElabDetailedWithEnv,
    runPipelineElabDetailedWithConfigAndEnv,
    runPipelineElabDetailedWithExternalBindings,
    runPipelineElabDetailedWithConfigAndExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindings,
    runPipelineElabDetailedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedWithPreparedExternalBindings,
    runPipelineElabDetailedResolvedWithPreparedExternalBindingsFromSupply,
    runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTimingFromSupply,
    runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTimingFromSupply,
    runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTimingFromSupply,
    runPipelineElabDetailedUncheckedWithExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindings,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsFromSupply,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTiming,
    runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTimingFromSupply,
    freshenTypeAbsAgainstEnv,
    freshenTypeAbsAgainstEnvFromSupply,
    authoritativeRootAnn,
    closePipelineTerm,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent
  ( forkIO,
    newEmptyMVar,
    putMVar,
    rtsSupportsBoundThreads,
    takeMVar,
  )
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
import MLF.Constraint.Presolution
  ( PresolutionResult,
    computePresolution,
    computePresolutionWithTiming,
    computePresolutionWithTimingAndRootOwnership,
  )
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
import MLF.Constraint.Types.Phase (Phase (Acyclic, Presolved, Raw))
import MLF.Elab.Elaborate (ElabConfig, ElabEnv (..), elaborateWithEnvDetailed)
import MLF.Elab.Elaborate.Algebra
  ( CompilerExactResultBoundCertificate,
    ElaboratedTerm (..),
    Env,
    alignEnvToConstructionBinderRenames,
    alignEnvToCompilerExactBinderRenames,
    extendEnvTypeScopeWithAliases,
    mkEnvWithResolvedBindings,
    projectCompilerExactResultBoundCertificates,
    withEnvConstructedLambdaParamTypes,
    withEnvLocalGammaClosures,
  )
import MLF.Elab.Elaborate.Annotation
  ( AuthorizedElaborationRoot,
    authorizedElaborationResultAnn,
    elaborateClosedExactAnnotationTermAtType,
    sourceTypeToElabTypeWithIdentitiesFromSupply,
  )
import MLF.Elab.Generalize
  ( CompilerExactResultStage (..),
    subtermGeneralizationConstructionBinderRenames,
    subtermGeneralizationCompilerExactBinderRenames,
    subtermGeneralizationCompilerExactBoundary,
  )
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
    PreparedRootConstructionScope,
    PreparedRootClosure (..),
    PreparedRootGeneralization (..),
    preparedRootClosureScheme,
    preparedRootConstructionScopeAliases,
    preparedRootConstructionScopeBinders,
    preparedRootConstructionScopeLocalGammaClosures,
    applyPreparedCompilerExactRootBinderIdentities,
    applyPreparedRootBinderIdentities,
    applyPreparedRootSourceTypeBinderIdentities,
    authorizePreparedAnn,
    computePreparedResultTypeWithRootGeneralization,
    completePreparedCompilerExactSubtermResults,
    generalizePreparedRootDetailed,
    generalizePreparedRootDetailedWithConstructionResult,
    prepareOrdinaryRootConstructionScope,
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    withPreparedResolvedTermSchemes,
    preparedElaborationConfig,
    preparedElaborationEnvWithInitialEnv,
    preparedIdentityGenerator,
    preparedCompilerExactExpectedType,
    applyPreparedTermSourceBinderAliases,
    preparedCompilerExactSourceResultBinderRoutes,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    stripPreparedWitnesslessAuthoritativeAnn,
  )
import MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstRefsIfNeeded,
    constructTermWithSchemeSubstRefs,
    constructTermWithSchemeSubstRefsByBinderRoutes,
    preserveRetainedChildAuthoritativeResult,
    substInTermRefs,
  )
import MLF.Elab.TypeCheck (typeCheckWithEnv)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen
  ( AnnExpr (..),
    BindingKey (..),
    ConstraintError (..),
    ConstraintResult (..),
    ExternalBinding (..),
    ExternalBindingIdentity,
    externalBindingIdentityFromDetails,
    externalBindingRuntimeName,
    externalBindingDetails,
    ExternalBindingMode (..),
    ExternalBindings,
    ExternalEnv,
    ModuleConstraintRoot (..),
    ModuleConstraintResult (..),
    generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply,
    generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply,
    generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply,
    generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply,
  )
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (mergeSymbolIdentityMaps, mergeTypeBinderIdentityMaps)
import MLF.Frontend.Symbol (SymbolIdentity, lookupSymbolIdentityAlias, symbolIdentityAliasMap, symbolUniqueIdentity)
import MLF.Frontend.Syntax (NormSrcType, NormSurfaceExpr, NormSurfaceExprOf, ResolvedNormSurfaceExpr, ResolvedSrcType, StructBound, VarName)
import qualified MLF.Frontend.Syntax as Surface
import MLF.Reify.TypeOps (alphaEqType, churchAwareEqType, freeTypeVarAliasNamesType, freeTypeVarRefsType, freshNameLike, matchTypeRefs, substTypeCaptureRef)
import MLF.Util.Timing
  ( TimingConfig,
    emitProgramOperationMetricIO,
    timeProgramOperationIO,
    timeProgramOperationWithSuffixIO,
    whenProgramOperationsIO,
  )
import MLF.Util.Trace (TraceConfig, traceGeneralize)
import MLF.Types.Identity
  ( IdDetails (..),
    IdentityGenerator,
    advanceIdentityGeneratorPastMany,
    freshEnvRef,
    idDetailsAliasNamesWith,
    idDetailsGeneratedIdentities,
    idDetailsSameIdentity,
    identityGeneratorAfter,
    typeBinderIdentityAliasMap,
    typeBinderIdentityFromStructural,
    symbolGeneratedIdentities,
    StructuralTypeBinderRole (..),
    typeBinderGeneratedIdentities,
  )

data PipelineElabDetailedResult = PipelineElabDetailedResult
  { pedTerm :: XmlfTerm,
    pedType :: ElabType,
    pedRootAnn :: AnnExpr,
    pedTypeCheckEnv :: TypeCheck.Env,
    pedIdentityGenerator :: IdentityGenerator
  }

data PreparedExternalBinding = PreparedExternalBinding
  { preparedBindingSource :: ExternalBinding,
    preparedBindingSchemeInfo :: SchemeInfo
  }
  deriving (Eq)

data PreparedExternalBindings = PreparedExternalBindings
  { pebBindingsByAlias :: Map.Map VarName PreparedExternalBinding,
    -- Keep the complete candidate set for type heads for the same reason as
    -- source binders below.  Once an ambiguous spelling has been collapsed out
    -- of a plain map, a later extension can otherwise make an unrelated third
    -- identity appear authoritative.
    pebSourceTypeHeadIdentityCandidates :: SymbolIdentityCandidates,
    -- Keep every binder identity proposed for an alias.  A plain
    -- @Map String TypeBinderIdentity@ is a lossy representation here: merging
    -- two prepared environments first removes an ambiguous alias, after which
    -- extending the result can accidentally make a third identity look
    -- authoritative.  The resolved map consumed by constraint generation is
    -- therefore always derived from this candidate set.
    pebSourceTypeBinderIdentityCandidates :: TypeBinderIdentityCandidates,
    pebReservedIdentities :: [UniqueIdentity]
  }

type TypeBinderIdentityCandidates = Map.Map String (Set.Set TypeBinderIdentity)

type SymbolIdentityCandidates = Map.Map String (Set.Set SymbolIdentity)

symbolIdentityCandidatesFromMaps :: [Map.Map String SymbolIdentity] -> SymbolIdentityCandidates
symbolIdentityCandidatesFromMaps maps =
  Map.fromListWith
    Set.union
    [ (name, Set.singleton identity)
    | identities <- maps,
      (name, identity) <- Map.toList identities
    ]

mergeSymbolIdentityCandidates :: [SymbolIdentityCandidates] -> SymbolIdentityCandidates
mergeSymbolIdentityCandidates =
  Map.unionsWith Set.union

resolvedSymbolIdentityCandidates :: SymbolIdentityCandidates -> Map.Map String SymbolIdentity
resolvedSymbolIdentityCandidates =
  Map.mapMaybe uniqueIdentity
  where
    uniqueIdentity identities =
      case Set.toList identities of
        [identity] -> Just identity
        _ -> Nothing

typeBinderIdentityCandidatesFromMaps :: [Map.Map String TypeBinderIdentity] -> TypeBinderIdentityCandidates
typeBinderIdentityCandidatesFromMaps maps =
  Map.fromListWith
    Set.union
    [ (name, Set.singleton identity)
    | identities <- maps,
      (name, identity) <- Map.toList identities
    ]

mergeTypeBinderIdentityCandidates :: [TypeBinderIdentityCandidates] -> TypeBinderIdentityCandidates
mergeTypeBinderIdentityCandidates =
  Map.unionsWith Set.union

resolvedTypeBinderIdentityCandidates :: TypeBinderIdentityCandidates -> Map.Map String TypeBinderIdentity
resolvedTypeBinderIdentityCandidates =
  Map.mapMaybe uniqueIdentity
  where
    uniqueIdentity identities =
      case Set.toList identities of
        [identity] -> Just identity
        _ -> Nothing

preparedSourceTypeBinderIdentities :: PreparedExternalBindings -> Map.Map String TypeBinderIdentity
preparedSourceTypeBinderIdentities =
  resolvedTypeBinderIdentityCandidates . pebSourceTypeBinderIdentityCandidates

preparedSourceTypeHeadIdentities :: PreparedExternalBindings -> Map.Map String SymbolIdentity
preparedSourceTypeHeadIdentities =
  resolvedSymbolIdentityCandidates . pebSourceTypeHeadIdentityCandidates

preparedExternalTypeCheckEnv :: PreparedExternalBindings -> TypeCheck.Env
preparedExternalTypeCheckEnv =
  typeCheckEnvFromElaborationBindings . preparedExternalElaborationBindings

preparedExternalIdentityGenerator :: [PreparedExternalBindings] -> IdentityGenerator
preparedExternalIdentityGenerator prepared =
  identityGeneratorAfter (concatMap preparedExternalGeneratedIdentities prepared)

preparedExternalIdentityGeneratorFrom :: IdentityGenerator -> [PreparedExternalBindings] -> IdentityGenerator
preparedExternalIdentityGeneratorFrom generator prepared =
  advanceIdentityGeneratorPastMany
    (concatMap preparedExternalGeneratedIdentities prepared)
    generator

preparedExternalGeneratedIdentities :: PreparedExternalBindings -> [UniqueIdentity]
preparedExternalGeneratedIdentities prepared =
  pebReservedIdentities prepared
    ++ concat
      [ idDetailsGeneratedIdentities (resolvedVarDetails resolved)
          ++ generatedIdentitiesInType (resolvedVarType resolved)
      | (_, resolved) <- Map.elems (preparedExternalElaborationBindings prepared)
      ]
    ++ concatMap
      symbolGeneratedIdentities
      (concatMap Set.toList (Map.elems (pebSourceTypeHeadIdentityCandidates prepared)))
    ++ concatMap
      typeBinderGeneratedIdentities
      (concatMap Set.toList (Map.elems (pebSourceTypeBinderIdentityCandidates prepared)))

preparedExternalElaborationEnv :: PreparedExternalBindings -> Env
preparedExternalElaborationEnv =
  mkEnvWithResolvedBindings . preparedExternalElaborationBindings

preparedExternalElaborationBindings :: PreparedExternalBindings -> Map.Map VarName (SchemeInfo, ResolvedVar)
preparedExternalElaborationBindings =
  Map.map preparedExternalElaborationBinding . pebBindingsByAlias

-- | Exact source schemes keyed by the resolved identity carried by each
-- occurrence. Alias spellings may duplicate an entry, but they cannot select
-- a different declaration once the identity key is fixed.
preparedExternalSchemesByIdentity ::
  PreparedExternalBindings ->
  Map.Map ResolvedTermIdentityKey SchemeInfo
preparedExternalSchemesByIdentity prepared =
  Map.fromList
    [ ( idDetailsIdentityKey
          ( externalBindingDetails
              (externalBindingIdentity (preparedBindingSource binding))
          ),
        preparedBindingSchemeInfo binding
      )
    | binding <- Map.elems (pebBindingsByAlias prepared)
    ]

preparedExternalElaborationBinding :: PreparedExternalBinding -> (SchemeInfo, ResolvedVar)
preparedExternalElaborationBinding prepared =
  (schemeInfo, resolvedExternalBindingVar (externalBindingIdentity binding) schemeInfo)
  where
    binding = preparedBindingSource prepared
    schemeInfo = preparedBindingSchemeInfo prepared

preparedExternalSourceBindings :: PreparedExternalBindings -> ExternalBindings
preparedExternalSourceBindings =
  Map.map preparedBindingSource . pebBindingsByAlias

preparedElaborationEnvWithSourceBinderAliases ::
  Map.Map String TypeBinderIdentity ->
  IntMap.IntMap NormSrcType ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabEnv 'Presolved
preparedElaborationEnvWithSourceBinderAliases sourceBinderAliases annSourceTypes extPrepared artifact =
  (preparedElaborationEnvWithInitialEnv annSourceTypes (preparedExternalElaborationEnv extPrepared) artifact)
    { eeSourceTypeHeadIdentities = headIdentities,
      eeSourceTypeBinderIdentities =
        sourceBinderAliases `Map.union` binderIdentities
    }
  where
    (headIdentities, binderIdentities) =
      preparedSourceTypeIdentityMaps extPrepared

preparedSourceTypeIdentityMaps ::
  PreparedExternalBindings ->
  (Map.Map String SymbolIdentity, Map.Map String TypeBinderIdentity)
preparedSourceTypeIdentityMaps prepared =
  ( preparedSourceTypeHeadIdentities prepared,
    preparedSourceTypeBinderIdentities prepared
  )

-- | Retain the complete identity domain at boundaries that must reject an
-- ambiguous source spelling instead of selecting whichever singleton remains
-- after projection to the ordinary identity map.
preparedSourceTypeBinderIdentityCandidates ::
  PreparedExternalBindings ->
  Map.Map String (Set.Set TypeBinderIdentity)
preparedSourceTypeBinderIdentityCandidates =
  pebSourceTypeBinderIdentityCandidates

preparedAnnotationExpectedTypesWithSourceBinderAliases
  :: IdentityGenerator
  -> Map.Map String TypeBinderIdentity
  -> PreparedExternalBindings
  -> IntMap.IntMap NormSrcType
  -> Either ElabError (IntMap.IntMap ElabType, IdentityGenerator)
preparedAnnotationExpectedTypesWithSourceBinderAliases identityGenerator sourceBinderAliases prepared sourceTypes =
  -- 'IntMap.toAscList' makes lexical identity allocation independent of map
  -- construction history while the accumulator makes distinct annotation
  -- occurrences consume distinct identities.
  foldM convertOne (IntMap.empty, identityGenerator) (IntMap.toAscList sourceTypes)
  where
    (headIdentities, binderIdentities) =
      preparedSourceTypeIdentityMaps prepared
    allBinderIdentities =
      sourceBinderAliases `Map.union` binderIdentities

    convertOne (expectedTypes, generator) (nodeKey, sourceType) = do
      (expectedType, generator') <-
        sourceTypeToElabTypeWithIdentitiesFromSupply
          generator
          headIdentities
          allBinderIdentities
          sourceType
      pure (IntMap.insert nodeKey expectedType expectedTypes, generator')

preparedAnnotationExpectedTypesForRoots
  :: IdentityGenerator
  -> [(key, PreparedExternalBindings, ModuleConstraintRoot)]
  -> IntMap.IntMap NormSrcType
  -> Either ElabError (IntMap.IntMap ElabType, IdentityGenerator)
preparedAnnotationExpectedTypesForRoots identityGenerator roots sourceTypes =
  -- Module roots already arrive in the stable order chosen by the batch plan.
  -- Thread one supply across them so root-local and global finalization assign
  -- the same lexical identities.
  foldM convertRoot (IntMap.empty, identityGenerator) roots
  where
    convertRoot (expectedTypes, generator) (_, prepared, root) = do
      (rootExpectedTypes, generator') <-
        preparedAnnotationExpectedTypesWithSourceBinderAliases
          generator
          (mcrSourceTypeBinderAliases root)
          prepared
          (IntMap.restrictKeys sourceTypes (annotationSourceTypeNodeKeys (mcrAnnotated root)))
      pure (IntMap.union expectedTypes rootExpectedTypes, generator')

-- Source annotation types are recorded only at coercion codomains and exact
-- lambda parameters.  Collect those raw graph keys per definition so a
-- multi-root batch converts each type with that definition's resolved binder
-- identities; merging spelling maps first would conflate unrelated @a@s.
annotationSourceTypeNodeKeys :: AnnExpr -> IntSet.IntSet
annotationSourceTypeNodeKeys ann =
  case ann of
    AResolvedVar {} -> IntSet.empty
    ALit {} -> IntSet.empty
    ALam _ _ paramNode _ body _ _ ->
      IntSet.insert (getNodeId paramNode) (annotationSourceTypeNodeKeys body)
    AApp fun arg _ _ _ ->
      annotationSourceTypeNodeKeys fun
        `IntSet.union` annotationSourceTypeNodeKeys arg
    ALet _ _ _ _ _ _ rhs body _ ->
      annotationSourceTypeNodeKeys rhs
        `IntSet.union` annotationSourceTypeNodeKeys body
    AExactAnn inner _ sourceTypeNode _ ->
      IntSet.insert (getNodeId sourceTypeNode) (annotationSourceTypeNodeKeys inner)
    AAnn inner sourceTypeNode _ ->
      IntSet.insert (getNodeId sourceTypeNode) (annotationSourceTypeNodeKeys inner)
    ALetScope inner _ _ -> annotationSourceTypeNodeKeys inner
    AUnfold inner _ _ -> annotationSourceTypeNodeKeys inner

externalBindingsSourceTypeIdentityCandidates ::
  ExternalBindings ->
  (SymbolIdentityCandidates, TypeBinderIdentityCandidates)
externalBindingsSourceTypeIdentityCandidates extBindings =
  (headIdentityCandidates, binderIdentityCandidates)
  where
    bindings = Map.elems extBindings

    headIdentityCandidates =
      symbolIdentityCandidatesFromMaps (map externalBindingTypeHeadIdentities bindings)

    headIdentities =
      resolvedSymbolIdentityCandidates headIdentityCandidates

    binderIdentityCandidates =
      typeBinderIdentityCandidatesFromMaps
        (structuralTypeBinderIdentitiesFromHeads headIdentities : map externalBindingTypeBinderIdentities bindings)

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
    mbpIdentityGenerator :: IdentityGenerator,
    mbpSharedEdgeCount :: !Int,
    mbpUnknownEdgeCount :: !Int
  }

data RootPartition p = RootPartition
  { rpRootId :: !ModuleRootId,
    rpConstraint :: Constraint p,
    rpAnnotated :: !AnnExpr,
    rpAnnSourceTypes :: !(IntMap.IntMap NormSrcType),
    rpExactProducerTypes :: !(IntMap.IntMap ResolvedSrcType),
    rpSourceTypeBinderIdentities :: !(IntMap.IntMap TypeBinderIdentity),
    rpSourceTypeBinderAliases :: !(Map.Map String TypeBinderIdentity),
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

data RootPresolutionContext = RootPresolutionContext
  { rpcFinalizationContext :: !(RootFinalizationContext 'Raw),
    rpcAcyclicConstraint :: !(Constraint 'Acyclic),
    rpcPresolution :: !PresolutionResult
  }

data PreparedRootFinalizationContext = PreparedRootFinalizationContext
  { prfcFinalizationContext :: !(RootFinalizationContext 'Raw),
    prfcPreparedGeneralization :: !PreparedGeneralizationArtifact
  }

data DeferredRootExactAnnotation = DeferredRootExactAnnotation
  { dreaEdgeId :: !EdgeId
  }

data RootElaborationPlan
  = OrdinaryRootElaborationPlan
      { repAuthorizedElaborationRoot :: !AuthorizedElaborationRoot,
        repElaborationCanonicalAnn :: !AnnExpr,
        repElaborationPrecanonicalAnn :: !AnnExpr
      }
  | ExactRootElaborationPlan
      { repAuthorizedElaborationRoot :: !AuthorizedElaborationRoot,
        repElaborationCanonicalAnn :: !AnnExpr,
        repElaborationPrecanonicalAnn :: !AnnExpr,
        repResultCanonicalAnn :: !AnnExpr,
        repResultPrecanonicalAnn :: !AnnExpr,
        repExactAnnotation :: !DeferredRootExactAnnotation
      }

-- | Construction-time root authority.  Ordinary roots carry only the Gamma
-- needed for strict Hyp checking; they cannot expose a reusable final result
-- generalization before elaboration has selected the authoritative result.
-- Compiler-exact roots are the one case whose result authority is fixed in
-- advance, so they retain a complete provisional generalization for entering
-- the exact construction Gamma.  Local-binder placement is still finalized
-- from owner certificates after elaboration.
data PreparedRootConstruction
  = PreparedOrdinaryRootConstruction !PreparedRootConstructionScope
  | PreparedExactRootConstruction
      !DeferredRootExactAnnotation
      !PreparedRootGeneralization

data PreparedRootExactness
  = PreparedOrdinaryRoot
  | PreparedExactRoot !DeferredRootExactAnnotation

rootConstructionScope :: PreparedRootConstruction -> PreparedRootConstructionScope
rootConstructionScope construction =
  case construction of
    PreparedOrdinaryRootConstruction scope -> scope
    PreparedExactRootConstruction _ rootGeneralization ->
      prgConstructionScope rootGeneralization

preparedRootConstructionDiagnostic :: PreparedRootConstruction -> String
preparedRootConstructionDiagnostic construction =
  case construction of
    PreparedOrdinaryRootConstruction{} -> "<ordinary root>"
    PreparedExactRootConstruction _ preparedRoot ->
      show
        ( prgScopeRoot preparedRoot,
          prgTarget preparedRoot,
          prgScheme preparedRoot
        )

-- | Everything a root can compute after packet preparation without consuming
-- the generated-identity supply.  In the root-local module path these values
-- are produced concurrently; closing and collision freshening are deliberately
-- kept out of this record because both can allocate type-binder identities.
data PreparedPipelineRootStage = PreparedPipelineRootStage
  { pprsPreparedGeneralization :: !PreparedGeneralizationArtifact,
    pprsInitialTypeCheckEnv :: !TypeCheck.Env,
    pprsRootGeneralization :: !PreparedRootGeneralization,
    pprsRootScheme :: !ElabScheme,
    pprsRootSubstitution :: !(IntMap.IntMap TypeBinderRef),
    pprsElaboratedTerm :: !XmlfTerm,
    pprsSubstitutedTerm :: !XmlfTerm,
    pprsCompilerExactResultBoundCertificates :: ![CompilerExactResultBoundCertificate],
    pprsCompilerExactRootBinderRoutes :: ![(TypeBinderRef, TypeBinderRef)],
    pprsAuthoritativeCanonicalAnn :: !AnnExpr,
    pprsAuthoritativePrecanonicalAnn :: !AnnExpr,
    pprsRootExactness :: !PreparedRootExactness
  }

-- | A root after every identity-allocating finalization step has consumed the
-- authoritative supply.  Canonicalization, checking, and diagnostics can now
-- run concurrently without changing identity assignment.
data FreshenedPipelineRootStage = FreshenedPipelineRootStage
  { fprsPreparedRoot :: !PreparedPipelineRootStage,
    fprsClosedTerm :: !XmlfTerm,
    fprsAuthoritativeResultType :: !ElabType,
    fprsIdentityGenerator :: !IdentityGenerator
  }

type PipelineStage a = ExceptT PipelineError IO a

rootElaborationPlan ::
  AuthorizedElaborationRoot ->
  AnnExpr ->
  Either ElabError RootElaborationPlan
rootElaborationPlan authorizedRoot annPre =
  case annCanon of
    AExactAnn innerCanon _ _ edgeId ->
      case annPre of
        AExactAnn innerPre _ _ _ ->
          Right
            ExactRootElaborationPlan
              { repAuthorizedElaborationRoot = authorizedRoot,
                repElaborationCanonicalAnn = innerCanon,
                repElaborationPrecanonicalAnn = innerPre,
                repResultCanonicalAnn = annCanon,
                repResultPrecanonicalAnn = annPre,
                repExactAnnotation =
                  DeferredRootExactAnnotation
                    { dreaEdgeId = edgeId }
              }
        _ ->
          Left
            ( ValidationFailed
                [ "canonical root exact annotation lost its source counterpart",
                  "  canonical: " ++ show annCanon,
                  "  source: " ++ show annPre
                ]
            )
    _ ->
      Right
        OrdinaryRootElaborationPlan
          { repAuthorizedElaborationRoot = authorizedRoot,
            repElaborationCanonicalAnn = annCanon,
            repElaborationPrecanonicalAnn = annPre
          }
  where
    annCanon = authorizedElaborationResultAnn authorizedRoot

-- | Prepare only the authority available before term construction.  Ordinary
-- roots receive a construction-only Γ.  A compiler-owned exact root fixes its
-- endpoint authority in advance and therefore carries a complete provisional
-- plan, but pending local-Gamma placement is finalized from post-elaboration
-- owner certificates in both cases.  This implements Figure 15.3.5's
-- ordering: local Hyp computations are checked under their enclosing Γ before
-- Λ(Γ) is wrapped around the completed term.
prepareRootConstruction
  :: PreparedGeneralizationArtifact
  -> RootElaborationPlan
  -> Either ElabError PreparedRootConstruction
prepareRootConstruction prepared rootPlan =
  case rootPlan of
    OrdinaryRootElaborationPlan{} ->
      PreparedOrdinaryRootConstruction
        <$> prepareOrdinaryRootConstructionScope
          prepared
          (repElaborationCanonicalAnn rootPlan)
          (repElaborationPrecanonicalAnn rootPlan)
    ExactRootElaborationPlan
      { repResultCanonicalAnn = resultCanonicalAnn,
        repResultPrecanonicalAnn = resultPrecanonicalAnn,
        repExactAnnotation = exactAnnotation
      } -> do
      rootGeneralization0 <-
        generalizePreparedRootDetailed
          prepared
          resultCanonicalAnn
          resultPrecanonicalAnn
      sourceProjectedRoot <-
        applyPreparedRootSourceTypeBinderIdentities
          rootGeneralization0
      rootGeneralization <-
        applyPreparedCompilerExactRootBinderIdentities
          prepared
          (dreaEdgeId exactAnnotation)
          sourceProjectedRoot
          (prgSubst sourceProjectedRoot)
      pure
        ( PreparedExactRootConstruction
            exactAnnotation
            rootGeneralization
        )

-- | Install the construction inputs that a nested 'AExactAnn' would install
-- itself.  Root planning deliberately elaborates the exact annotation's
-- child, so the root path must enter the packet's identity quotient and seed
-- lambda domains from the identity-bearing exact contract before recursive
-- construction begins.  Source-annotation routes remain occurrence-owned;
-- replacing them with root Gamma aliases would collapse distinct authorities
-- that merely share a solved graph representative.
elabEnvWithRootConstructionScope
  :: PreparedRootConstruction
  -> AnnExpr
  -> ElabEnv p
  -> Either ElabError (ElabEnv p)
elabEnvWithRootConstructionScope rootConstruction constructionAnn elabEnv = do
  alignedInitialEnv <-
    case rootConstruction of
      PreparedOrdinaryRootConstruction{} ->
        pure (eeInitialTermEnv elabEnv)
      PreparedExactRootConstruction exactAnnotation _ -> do
        packets <- eeSubtermGeneralizations elabEnv
        envInPacketConstruction <-
          alignEnvToConstructionBinderRenames
            [ rename
            | packet <- Map.elems packets
            , subtermGeneralizationCompilerExactBoundary packet
                == Just (dreaEdgeId exactAnnotation)
            , rename <- subtermGeneralizationConstructionBinderRenames packet
            ]
            (eeInitialTermEnv elabEnv)
        alignEnvToCompilerExactBinderRenames
          [ rename
          | packet <- Map.elems packets
          , subtermGeneralizationCompilerExactBoundary packet
              == Just (dreaEdgeId exactAnnotation)
          , rename <- subtermGeneralizationCompilerExactBinderRenames packet
          ]
          envInPacketConstruction
  let scopedInitialEnv =
        withEnvLocalGammaClosures
          (preparedRootConstructionScopeLocalGammaClosures constructionScope)
          ( extendEnvTypeScopeWithAliases
              constructionAliases
              (preparedRootConstructionScopeBinders constructionScope)
              alignedInitialEnv
          )
  constructionInitialEnv <-
    case rootConstruction of
      PreparedOrdinaryRootConstruction{} -> pure scopedInitialEnv
      PreparedExactRootConstruction exactAnnotation _ -> do
        exactTypes <- eeExactProducerTypes elabEnv
        expectedType <-
          case IntMap.lookup (getEdgeId (dreaEdgeId exactAnnotation)) exactTypes of
            Just ty -> pure ty
            Nothing ->
              Left
                ( ValidationFailed
                    [ "exact root construction has no prepared expected type"
                    , "  edge: " ++ show (dreaEdgeId exactAnnotation)
                    ]
                )
        pure
          ( withEnvConstructedLambdaParamTypes
              expectedType
              constructionAnn
              scopedInitialEnv
          )
  pure elabEnv {eeInitialTermEnv = constructionInitialEnv}
  where
    constructionScope = rootConstructionScope rootConstruction
    constructionAliases =
      preparedRootConstructionScopeAliases constructionScope

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

validateDirectRecursiveAnnotations :: NormSurfaceExprOf references -> Either ConstraintError ()
validateDirectRecursiveAnnotations = goExpr
  where
    goExpr expr =
      case expr of
        Surface.EVarNode _ -> Right ()
        Surface.ELit _ -> Right ()
        Surface.ELamNode _ body -> goExpr body
        Surface.EApp fun arg -> goExpr fun >> goExpr arg
        Surface.ELetNode _ rhs body -> goExpr rhs >> goExpr body
        Surface.ELamAnnNode _ annTy body -> validateAnn annTy >> goExpr body
        Surface.EAnn inner annTy -> goExpr inner >> validateAnn annTy
        Surface.EExactAnn inner annTy _ -> goExpr inner >> validateAnn annTy
        Surface.EExactLamNode _ annTy body -> validateAnn annTy >> goExpr body
        Surface.ECoerceConst _ -> Right ()
        Surface.EExactCoerceConst _ _ -> Right ()

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

runPipelineElabDetailedResolvedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedResolvedWithPreparedExternalBindings =
  runPipelineElabWithResolvedPrepared FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig) (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedWithPreparedExternalBindingsFromSupply :: IdentityGenerator -> PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedResolvedWithPreparedExternalBindingsFromSupply =
  runPipelineElabWithResolvedPreparedFromSupply FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig) (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithResolvedPreparedWithTiming timing label FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig) (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTimingFromSupply :: TimingConfig -> String -> IdentityGenerator -> PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedResolvedWithPreparedExternalBindingsWithTimingFromSupply timing label =
  runPipelineElabWithResolvedPreparedWithTimingFromSupply timing label FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig) (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedUncheckedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedUncheckedWithPreparedExternalBindings =
  runPipelineElabWithPrepared FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> NormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedUncheckedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithPreparedWithTiming timing label FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindings :: PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindings =
  runPipelineElabWithResolvedPrepared FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsFromSupply :: IdentityGenerator -> PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> Either PipelineError PipelineElabDetailedResult
runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsFromSupply =
  runPipelineElabWithResolvedPreparedFromSupply FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTiming :: TimingConfig -> String -> PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTiming timing label =
  runPipelineElabWithResolvedPreparedWithTiming timing label FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTimingFromSupply :: TimingConfig -> String -> IdentityGenerator -> PolySyms -> PreparedExternalBindings -> ResolvedNormSurfaceExpr -> IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabDetailedResolvedUncheckedWithPreparedExternalBindingsWithTimingFromSupply timing label =
  runPipelineElabWithResolvedPreparedWithTimingFromSupply timing label FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled (pcTraceConfig defaultPipelineConfig)

schemeExternalBindings :: ExternalEnv -> ExternalBindings
schemeExternalBindings bindings =
  snd (Map.mapAccumWithKey resolveOne (identityGeneratorAfter []) bindings)
  where
    resolveOne generator name srcTy =
      let (ref, generator') = freshEnvRef name generator
       in ( generator',
            ExternalBinding
              { externalBindingType = srcTy,
                externalBindingMode = ExternalBindingScheme,
                externalBindingIdentity = externalBindingIdentityFromDetails (EnvId ref),
                externalBindingTypeHeadIdentities = Map.empty,
                externalBindingTypeBinderIdentities = Map.empty
              }
          )

prepareExternalBindings :: ExternalBindings -> Either ConstraintError PreparedExternalBindings
prepareExternalBindings =
  prepareExternalBindingsWithTypeIdentities Map.empty Map.empty

prepareExternalBindingsWithTypeIdentities ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  ExternalBindings ->
  Either ConstraintError PreparedExternalBindings
prepareExternalBindingsWithTypeIdentities sharedHeadIdentities sharedBinderIdentities extBindings0 = do
  let initialGenerator =
        identityGeneratorAfter
          ( externalBindingsGeneratedIdentities extBindings0
              ++ concatMap symbolGeneratedIdentities (Map.elems sharedHeadIdentities)
              ++ concatMap typeBinderGeneratedIdentities (Map.elems sharedBinderIdentities)
          )
  (_, bindings0) <-
    prepareExternalBindingEntries
      sharedHeadIdentities
      sharedBinderIdentities
      initialGenerator
      extBindings0
  let bindings = preparedBindingsWithIdentityAliases bindings0
      extBindings = Map.map preparedBindingSource bindings
  let (bindingHeadIdentityCandidates, bindingBinderIdentityCandidates) =
        externalBindingsSourceTypeIdentityCandidates extBindings
      headIdentityCandidates =
        mergeSymbolIdentityCandidates
          [ bindingHeadIdentityCandidates,
            symbolIdentityCandidatesFromMaps [sharedHeadIdentities]
          ]
      headIdentities =
        resolvedSymbolIdentityCandidates headIdentityCandidates
      binderIdentityCandidates =
        mergeTypeBinderIdentityCandidates
          [ bindingBinderIdentityCandidates,
            typeBinderIdentityCandidatesFromMaps
              ( sharedBinderIdentities
                  : structuralTypeBinderIdentitiesFromHeads headIdentities
                  : map externalBindingTypeBinderIdentities (Map.elems extBindings0)
              )
          ]
  pure
    PreparedExternalBindings
      { pebBindingsByAlias = bindings,
        pebSourceTypeHeadIdentityCandidates = headIdentityCandidates,
        pebSourceTypeBinderIdentityCandidates = binderIdentityCandidates,
        pebReservedIdentities = externalBindingsGeneratedIdentities extBindings0
      }

preparedBindingsWithIdentityAliases :: Map.Map VarName PreparedExternalBinding -> Map.Map VarName PreparedExternalBinding
preparedBindingsWithIdentityAliases bindings =
  bindings `Map.union` Map.filterWithKey (\name _ -> Map.notMember name bindings) uniqueAliases
  where
    uniqueAliases =
      Map.fromList
        [ (alias, prepared)
        | (alias, prepared : rest) <- Map.toList aliasesByName,
          all (== prepared) rest
        ]

    aliasesByName =
      Map.fromListWith
        (++)
        [ (alias, [prepared])
        | (name, prepared) <- Map.toList bindings,
          alias <- externalBindingAliases name (preparedBindingSource prepared)
        ]

externalBindingAliases :: VarName -> ExternalBinding -> [VarName]
externalBindingAliases name binding =
  Set.toList $
    Set.fromList $
      idDetailsAliasNamesWith name (externalBindingDetails identity)
        ++ idDetailsAliasNamesWith (externalBindingRuntimeName identity) (externalBindingDetails identity)
  where
    identity = externalBindingIdentity binding

extendPreparedExternalBindingTypeIdentities ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  PreparedExternalBindings ->
  PreparedExternalBindings
extendPreparedExternalBindingTypeIdentities headIdentities binderIdentities prepared =
  extendPreparedExternalBindingTypeIdentityCandidates
    [headIdentities]
    [binderIdentities]
    prepared

extendPreparedExternalBindingTypeIdentityCandidates ::
  [Map.Map String SymbolIdentity] ->
  [Map.Map String TypeBinderIdentity] ->
  PreparedExternalBindings ->
  PreparedExternalBindings
extendPreparedExternalBindingTypeIdentityCandidates headIdentityMaps binderIdentityMaps prepared =
  let headIdentityCandidates =
        mergeSymbolIdentityCandidates
          [ pebSourceTypeHeadIdentityCandidates prepared,
            symbolIdentityCandidatesFromMaps headIdentityMaps
          ]
      heads =
        resolvedSymbolIdentityCandidates headIdentityCandidates
      binderIdentityCandidates =
        mergeTypeBinderIdentityCandidates
          [ pebSourceTypeBinderIdentityCandidates prepared,
            typeBinderIdentityCandidatesFromMaps
              (structuralTypeBinderIdentitiesFromHeads heads : binderIdentityMaps)
          ]
   in prepared
        { pebSourceTypeHeadIdentityCandidates = headIdentityCandidates,
          pebSourceTypeBinderIdentityCandidates = binderIdentityCandidates
        }

-- | Install identities owned by the current definition root ahead of
-- same-spelled candidates inherited from its external environment.  The
-- external candidate sets remain intact for every other alias; only names
-- whose semantic identity is carried by this root are authoritative here.
-- This is deliberately distinct from
-- 'extendPreparedExternalBindingTypeIdentityCandidates': module-wide
-- preparation must retain cross-root ambiguity, while constraint generation
-- for one root must not lose its own binder merely because another root also
-- displays it as @a@.
preferPreparedExternalBindingTypeIdentities ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  PreparedExternalBindings ->
  PreparedExternalBindings
preferPreparedExternalBindingTypeIdentities headIdentities binderIdentities prepared =
  prepared
    { pebSourceTypeHeadIdentityCandidates =
        authoritativeHeadCandidates
          `Map.union` pebSourceTypeHeadIdentityCandidates prepared,
      pebSourceTypeBinderIdentityCandidates =
        authoritativeBinderCandidates
          `Map.union` inheritedBinderCandidates
    }
  where
    authoritativeHeadCandidates =
      symbolIdentityCandidatesFromMaps [headIdentities]
    resolvedHeads =
      resolvedSymbolIdentityCandidates
        (authoritativeHeadCandidates `Map.union` pebSourceTypeHeadIdentityCandidates prepared)
    authoritativeBinderCandidates =
      typeBinderIdentityCandidatesFromMaps [binderIdentities]
    inheritedBinderCandidates =
      mergeTypeBinderIdentityCandidates
        [ pebSourceTypeBinderIdentityCandidates prepared,
          typeBinderIdentityCandidatesFromMaps
            [structuralTypeBinderIdentitiesFromHeads resolvedHeads]
        ]

reservePreparedExternalBindingIdentities :: [UniqueIdentity] -> PreparedExternalBindings -> PreparedExternalBindings
reservePreparedExternalBindingIdentities identities prepared =
  prepared {pebReservedIdentities = identities ++ pebReservedIdentities prepared}

restrictPreparedExternalBindings :: Set.Set VarName -> PreparedExternalBindings -> PreparedExternalBindings
restrictPreparedExternalBindings names prepared =
  PreparedExternalBindings
    { pebBindingsByAlias = Map.restrictKeys (pebBindingsByAlias prepared) names,
      pebSourceTypeHeadIdentityCandidates = pebSourceTypeHeadIdentityCandidates prepared,
      pebSourceTypeBinderIdentityCandidates = pebSourceTypeBinderIdentityCandidates prepared,
      pebReservedIdentities = pebReservedIdentities prepared
    }

-- | Restrict resolved callers by semantic identity. The outer map remains
-- name-indexed for storage, but selection never uses that spelling.
restrictPreparedExternalBindingsByKeys :: Set.Set BindingKey -> PreparedExternalBindings -> PreparedExternalBindings
restrictPreparedExternalBindingsByKeys bindingKeys prepared =
  restrictPreparedExternalBindings selectedNames prepared
  where
    selectedNames =
      Map.keysSet $
        Map.filter bindingSelected (pebBindingsByAlias prepared)

    bindingSelected preparedBinding =
      ResolvedBindingKey
        ( idDetailsIdentityKey
            (externalBindingDetails (externalBindingIdentity (preparedBindingSource preparedBinding)))
        )
        `Set.member` bindingKeys

unionPreparedExternalBindings :: PreparedExternalBindings -> PreparedExternalBindings -> PreparedExternalBindings
unionPreparedExternalBindings preferred fallback =
  let headIdentityCandidates =
        mergeSymbolIdentityCandidates
          [ pebSourceTypeHeadIdentityCandidates preferred,
            pebSourceTypeHeadIdentityCandidates fallback
          ]
      heads =
        resolvedSymbolIdentityCandidates headIdentityCandidates
      binderIdentityCandidates =
        mergeTypeBinderIdentityCandidates
          [ pebSourceTypeBinderIdentityCandidates preferred,
            pebSourceTypeBinderIdentityCandidates fallback,
            typeBinderIdentityCandidatesFromMaps
              [structuralTypeBinderIdentitiesFromHeads heads]
          ]
   in PreparedExternalBindings
        { pebBindingsByAlias = pebBindingsByAlias preferred `Map.union` pebBindingsByAlias fallback,
          pebSourceTypeHeadIdentityCandidates = headIdentityCandidates,
          pebSourceTypeBinderIdentityCandidates = binderIdentityCandidates,
          pebReservedIdentities = pebReservedIdentities preferred ++ pebReservedIdentities fallback
        }

typeCheckEnvFromElaborationBindings :: Map.Map VarName (SchemeInfo, ResolvedVar) -> TypeCheck.Env
typeCheckEnvFromElaborationBindings elaborationBindings =
  TypeCheck.mkTypeCheckEnvWithResolvedTerms
    [ (resolved, schemeToType (siScheme schemeInfo))
    | (schemeInfo, resolved) <- Map.elems elaborationBindings
    ]
    Map.empty

externalBindingsGeneratedIdentities :: ExternalBindings -> [UniqueIdentity]
externalBindingsGeneratedIdentities extBindings =
  [ identity
  | ExternalBinding {externalBindingIdentity = externalIdentity} <- Map.elems extBindings,
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
    { resolvedVarType = schemeToType (siScheme schemeInfo),
      resolvedVarDetails = externalBindingDetails identity
    }

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
    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
        (preparedExternalIdentityGenerator [extPrepared])
        polySyms
        (preparedSourceTypeHeadIdentities extPrepared)
        (preparedSourceTypeBinderIdentities extPrepared)
        (preparedExternalSourceBindings extPrepared)
    )

runPipelineElabWithResolvedPrepared ::
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PolySyms ->
  PreparedExternalBindings ->
  ResolvedNormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithResolvedPrepared finalCheckMode diagnosticsMode traceCfg polySyms extPrepared =
  runPipelineElabWithResolvedPreparedFromSupply
    finalCheckMode
    diagnosticsMode
    traceCfg
    (preparedExternalIdentityGenerator [extPrepared])
    polySyms
    extPrepared

runPipelineElabWithResolvedPreparedFromSupply ::
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  IdentityGenerator ->
  PolySyms ->
  PreparedExternalBindings ->
  ResolvedNormSurfaceExpr ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithResolvedPreparedFromSupply finalCheckMode diagnosticsMode traceCfg suppliedGenerator polySyms extPrepared =
  runPipelineElabWithPreparedGenerated
    finalCheckMode
    diagnosticsMode
    traceCfg
    extPrepared
    ( generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply
        (preparedExternalIdentityGeneratorFrom suppliedGenerator [extPrepared])
        polySyms
        (preparedSourceTypeHeadIdentities extPrepared)
        (preparedSourceTypeBinderIdentities extPrepared)
        (preparedExternalSourceBindings extPrepared)
    )

runPipelineElabWithPreparedGenerated ::
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  (NormSurfaceExprOf references -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExprOf references ->
  Either PipelineError PipelineElabDetailedResult
runPipelineElabWithPreparedGenerated finalCheckMode diagnosticsMode traceCfg extPrepared generateConstraints expr = do
  () <- fromConstraintError (validateDirectRecursiveAnnotations expr)
  ConstraintResult
    { crConstraint = c0,
      crAnnotated = ann,
      crIdentityGenerator = packetIdentityGenerator,
      crAnnSourceTypes = annSourceTypes,
      crExactProducerTypes = exactProducerTypes,
      crSourceTypeBinderIdentities = sourceTypeBinderIdentities,
      crSourceTypeBinderAliases = sourceTypeBinderAliases,
      crInitialEnv = _initialBindings
    } <-
    fromConstraintError (generateConstraints expr)
  let c1 = normalize c0
  (cAcyclic, acyc) <- fromCycleError (breakCyclesAndCheckAcyclicity c1)
  pres <- fromPresolutionError (computePresolution traceCfg acyc cAcyclic)
  (annExpectedTypes, preparationIdentityGenerator) <-
    fromElabError
      ( preparedAnnotationExpectedTypesWithSourceBinderAliases
          packetIdentityGenerator
          sourceTypeBinderAliases
          extPrepared
          annSourceTypes
      )
  prepared0 <-
    fromSolveError $
      prepareGeneralizationArtifact
        traceCfg
        preparationIdentityGenerator
        exactProducerTypes
        annExpectedTypes
        sourceTypeBinderIdentities
        cAcyclic
        pres
        ann
  -- Use external schemes and identities from prepared bindings instead of
  -- re-generalizing through the constraint graph, which would produce
  -- graph-internal variable names that conflict with constructor types.
  let prepared =
        withPreparedResolvedTermSchemes
          (preparedExternalSchemesByIdentity extPrepared)
          prepared0
      initialTcEnv = preparedExternalTypeCheckEnv extPrepared
      elabConfig = preparedElaborationConfig traceCfg prepared
      elabEnv =
        preparedElaborationEnvWithSourceBinderAliases
          sourceTypeBinderAliases
          annSourceTypes
          extPrepared
          prepared
  authorizedRoot <-
    fromElabError (authorizePreparedAnn prepared ann)
  rootPlan <- fromElabError (rootElaborationPlan authorizedRoot ann)
  preparedRootConstruction <-
    fromElabError
      ( prepareRootConstruction
          prepared
          rootPlan
      )
  constructionElabEnv <-
    fromElabError
      ( elabEnvWithRootConstructionScope
          preparedRootConstruction
          (repElaborationCanonicalAnn rootPlan)
          elabEnv
      )
  elaboration <-
    case
        elaborateWithEnvDetailed
          elabConfig
          constructionElabEnv
          (repAuthorizedElaborationRoot rootPlan)
      of
        Right elaborated -> pure elaborated
        Left err ->
          fromElabError
            ( Left
                ( ValidationFailed
                    [ "root construction elaboration failed",
                      "  cause=" ++ show err,
                      "  prepared exact root="
                        ++ preparedRootConstructionDiagnostic preparedRootConstruction,
                      "  construction Gamma binders="
                        ++ show
                          ( preparedRootConstructionScopeBinders
                              (rootConstructionScope preparedRootConstruction)
                          )
                    ]
                )
            )
  let term = elaboratedTerm elaboration
      mbOwnerFinalConstruction =
        elaboratedOwnerFinalConstruction elaboration
      localGammaConstructionCertificates =
        elaboratedLocalGammaConstructionCertificates elaboration
      compilerExactResultBoundCertificates =
        elaboratedCompilerExactResultBoundCertificates elaboration
  case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
    () -> pure ()
  let sourceAuthoritativeAnnCanon =
        authoritativeRootAnn term (repElaborationCanonicalAnn rootPlan)
      sourceAuthoritativeAnnPre =
        authoritativeRootAnn term (repElaborationPrecanonicalAnn rootPlan)
      (sourceAuthoritativeAnnCanonFinal, sourceAuthoritativeAnnPreFinal) =
        stripPreparedWitnesslessAuthoritativeAnn
          prepared
          sourceAuthoritativeAnnCanon
          sourceAuthoritativeAnnPre
      (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
        case rootPlan of
          ExactRootElaborationPlan
            { repResultCanonicalAnn = resultCanon,
              repResultPrecanonicalAnn = resultPre
            } ->
              (resultCanon, resultPre)
          OrdinaryRootElaborationPlan{} ->
            (sourceAuthoritativeAnnCanonFinal, sourceAuthoritativeAnnPreFinal)
  -- Root construction is provisional until the source constructor has
  -- published its owner/local-Gamma certificates.  This is true for an exact
  -- root too: its endpoint is fixed in advance, but placing a pending local
  -- Gamma binder at the root would expose it as ambient and suppress the
  -- lambda that must construct it.  Finalize ownership now, then perform the
  -- source projection exactly once on this post-elaboration result.
  rootGeneralization0 <-
    fromElabError $
      generalizePreparedRootDetailedWithConstructionResult
        prepared
        (repElaborationCanonicalAnn rootPlan)
        (repElaborationPrecanonicalAnn rootPlan)
        authoritativeAnnCanonFinal
        mbOwnerFinalConstruction
        localGammaConstructionCertificates
  rootGeneralization <-
    fromElabError
      ( applyPreparedRootSourceTypeBinderIdentities
          rootGeneralization0
      )
  let rootSubst0 = prgSubst rootGeneralization
  rootSubstAliased <-
    fromElabError
      (applyPreparedTermSourceBinderAliases prepared rootSubst0 term)
  rootGeneralizationFinal <-
    case preparedRootConstruction of
      PreparedExactRootConstruction deferred _ ->
        fromElabError
          ( applyPreparedCompilerExactRootBinderIdentities
              prepared
              (dreaEdgeId deferred)
              rootGeneralization
              rootSubstAliased
          )
      PreparedOrdinaryRootConstruction{} ->
        fromElabError
          ( applyPreparedRootBinderIdentities
              rootSubstAliased
              rootGeneralization
          )
  let
      rootScheme = prgScheme rootGeneralizationFinal
      rootSubst = prgSubst rootGeneralizationFinal
      termSubst = substInTermRefs rootSubst term
      projectedCompilerExactResultBoundCertificates =
        projectCompilerExactResultBoundCertificates
          rootSubst
          compilerExactResultBoundCertificates
  compilerExactRootBinderRoutes <-
    case preparedRootConstruction of
      PreparedOrdinaryRootConstruction{} -> pure []
      PreparedExactRootConstruction deferred _ ->
        fromElabError
          ( preparedCompilerExactSourceResultBinderRoutes
              prepared
              (dreaEdgeId deferred)
          )

  preparedGenerator <-
    fromElabError (preparedIdentityGenerator prepared)
  let (termClosed, closedGenerator) =
        closePipelineTermFromSupply
          preparedGenerator
          ( case preparedRootConstruction of
              PreparedOrdinaryRootConstruction{} -> PreparedOrdinaryRoot
              PreparedExactRootConstruction deferred _ -> PreparedExactRoot deferred
          )
          compilerExactRootBinderRoutes
          initialTcEnv
          rootSubst
          (prgClosure rootGeneralizationFinal)
          term
          termSubst
  (termExact, authoritativeResultType) <-
    case preparedRootConstruction of
      PreparedOrdinaryRootConstruction{} ->
        pure (termClosed, schemeToType rootScheme)
      PreparedExactRootConstruction deferred _ -> do
        sourceCompletedTerm <-
          fromElabError
            ( completePreparedCompilerExactSubtermResults
                prepared
                projectedCompilerExactResultBoundCertificates
                CompleteBeforeCompilerExact
                (dreaEdgeId deferred)
                termClosed
            )
        expectedExactType <-
          fromElabError
            ( preparedCompilerExactExpectedType
                prepared
                (dreaEdgeId deferred)
            )
        let closedExpectedExactType =
              closePreparedExactExpectedType
                (prgClosure rootGeneralizationFinal)
                expectedExactType
        exactTerm <-
          case
              elaborateClosedExactAnnotationTermAtType
                initialTcEnv
                closedExpectedExactType
                (dreaEdgeId deferred)
                sourceCompletedTerm
            of
              Left err ->
                fromElabError
                  ( Left
                      ( ValidationFailed
                          [ "root compiler-exact construction failed"
                          , "  cause: " ++ show err
                          ]
                      )
                  )
              Right result -> pure result
        completedTerm <-
          fromElabError
            ( completePreparedCompilerExactSubtermResults
                prepared
                projectedCompilerExactResultBoundCertificates
                CompleteAfterCompilerExact
                (dreaEdgeId deferred)
                exactTerm
            )
        exactTy <-
          case typeCheckWithEnv initialTcEnv completedTerm of
            Right ty -> pure ty
            Left err ->
              fromElabError
                ( Left
                    ( ValidationFailed
                        [ "root compiler-exact completion failed"
                        , "  cause: " ++ show err
                        ]
                    )
                )
        pure (completedTerm, exactTy)
  let (termClosedFresh0, freshenedGenerator) =
        case preparedRootConstruction of
          PreparedExactRootConstruction{} -> (termExact, closedGenerator)
          PreparedOrdinaryRootConstruction{} ->
            freshenTypeAbsAgainstEnvFromSupply closedGenerator initialTcEnv termExact
      termClosedFresh =
        case preparedRootConstruction of
          PreparedExactRootConstruction{} -> termClosedFresh0
          PreparedOrdinaryRootConstruction{} ->
            TypeCheck.canonicalizeResolvedTermTypes initialTcEnv termClosedFresh0
  let resultGenerator resultType =
        advanceIdentityGeneratorPastMany
          (generatedIdentitiesInTerm termClosedFresh ++ generatedIdentitiesInType resultType)
          freshenedGenerator
      uncheckedAuthoritative =
        let resultType = authoritativeResultType
         in pure
              PipelineElabDetailedResult
                { pedTerm = termClosedFresh,
                  pedType = resultType,
                  pedRootAnn = authoritativeAnnCanonFinal,
                  pedTypeCheckEnv = initialTcEnv,
                  pedIdentityGenerator = resultGenerator resultType
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
              pedTypeCheckEnv = initialTcEnv,
              pedIdentityGenerator = resultGenerator tyChecked
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
    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
        (preparedExternalIdentityGenerator [extPrepared])
        polySyms
        (preparedSourceTypeHeadIdentities extPrepared)
        (preparedSourceTypeBinderIdentities extPrepared)
        (preparedExternalSourceBindings extPrepared)
    )

runPipelineElabWithResolvedPreparedWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PolySyms ->
  PreparedExternalBindings ->
  ResolvedNormSurfaceExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithResolvedPreparedWithTiming timing label finalCheckMode diagnosticsMode traceCfg polySyms extPrepared =
  runPipelineElabWithResolvedPreparedWithTimingFromSupply
    timing
    label
    finalCheckMode
    diagnosticsMode
    traceCfg
    (preparedExternalIdentityGenerator [extPrepared])
    polySyms
    extPrepared

runPipelineElabWithResolvedPreparedWithTimingFromSupply ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  IdentityGenerator ->
  PolySyms ->
  PreparedExternalBindings ->
  ResolvedNormSurfaceExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithResolvedPreparedWithTimingFromSupply timing label finalCheckMode diagnosticsMode traceCfg suppliedGenerator polySyms extPrepared =
  runPipelineElabWithPreparedGeneratedWithTiming
    timing
    label
    finalCheckMode
    diagnosticsMode
    traceCfg
    extPrepared
    ( generateConstraintsWithResolvedExternalBindingsAndTypeIdentitiesFromSupply
        (preparedExternalIdentityGeneratorFrom suppliedGenerator [extPrepared])
        polySyms
        (preparedSourceTypeHeadIdentities extPrepared)
        (preparedSourceTypeBinderIdentities extPrepared)
        (preparedExternalSourceBindings extPrepared)
    )

runPipelineElabWithPreparedGeneratedWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  PreparedExternalBindings ->
  (NormSurfaceExprOf references -> Either ConstraintError (ConstraintResult 'Raw)) ->
  NormSurfaceExprOf references ->
  IO (Either PipelineError PipelineElabDetailedResult)
runPipelineElabWithPreparedGeneratedWithTiming timing label finalCheckMode diagnosticsMode traceCfg extPrepared generateConstraints expr =
  runExceptT $ do
    evaluatePipelineEitherSuffix timing label ".validate_annotations" $
      fromConstraintError (validateDirectRecursiveAnnotations expr)
    ConstraintResult
      { crConstraint = c0,
        crAnnotated = ann,
        crIdentityGenerator = packetIdentityGenerator,
        crAnnSourceTypes = annSourceTypes,
        crExactProducerTypes = exactProducerTypes,
        crSourceTypeBinderIdentities = sourceTypeBinderIdentities,
        crSourceTypeBinderAliases = sourceTypeBinderAliases,
        crInitialEnv = _initialBindings
      } <-
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
            fromPresolutionError
              <$> computePresolutionWithTiming timing presolutionLabel traceCfg acyc cAcyclic
    prepared0 <-
      evaluatePipelineEitherSuffix timing label ".prepare_generalization" $
        do
          (annExpectedTypes, preparationIdentityGenerator) <-
            fromElabError
              ( preparedAnnotationExpectedTypesWithSourceBinderAliases
                  packetIdentityGenerator
                  sourceTypeBinderAliases
                  extPrepared
                  annSourceTypes
              )
          fromSolveError
            ( prepareGeneralizationArtifact
              traceCfg
              preparationIdentityGenerator
              exactProducerTypes
              annExpectedTypes
              sourceTypeBinderIdentities
              cAcyclic
              pres
              ann
            )
    let prepared =
          withPreparedResolvedTermSchemes
            (preparedExternalSchemesByIdentity extPrepared)
            prepared0
        elabConfig = preparedElaborationConfig traceCfg prepared
        elabEnv =
          preparedElaborationEnvWithSourceBinderAliases
            sourceTypeBinderAliases
            annSourceTypes
            extPrepared
            prepared
    authorizedRoot <-
      evaluatePipelineEitherSuffix timing label ".authorize_elaboration_root" $
        fromElabError (authorizePreparedAnn prepared ann)
    identityGenerator <-
      evaluatePipelineEitherSuffix timing label ".identity_supply" $
        fromElabError (preparedIdentityGenerator prepared)
    finishPreparedPipelineRootStage
      timing
      label
      finalCheckMode
      diagnosticsMode
      traceCfg
      identityGenerator
      extPrepared
      prepared
      elabConfig
      elabEnv
      authorizedRoot
      ann

prepareRootPresolutionContextWithTiming ::
  TimingConfig ->
  String ->
  TraceConfig ->
  RootFinalizationContext 'Raw ->
  IO (Either PipelineError RootPresolutionContext)
prepareRootPresolutionContextWithTiming timing label traceCfg finalizationContext =
  runExceptT $ do
    normalizeResult <-
      timePipelineValueSuffix timing label ".constraint_normalize" $
        evaluate (normalize (rpConstraint partition))
    (cAcyclic, acyc) <-
      evaluatePipelineEitherSuffix timing label ".acyclicity" $
        fromCycleError (breakCyclesAndCheckAcyclicity normalizeResult)
    pres <-
      let presolutionLabel = label ++ ".presolution"
       in timePipelineEither timing presolutionLabel $
            fromPresolutionError
              <$> computePresolutionWithTiming timing presolutionLabel traceCfg acyc cAcyclic
    pure
      RootPresolutionContext
        { rpcFinalizationContext = finalizationContext,
          rpcAcyclicConstraint = cAcyclic,
          rpcPresolution = pres
        }
  where
    partition = rfcPartition finalizationContext

-- | Allocate packet identities only after every independent root has completed
-- presolution.  The caller invokes this function in stable root order and
-- threads the returned supply to the next root, so worker scheduling cannot
-- change which generated identities appear in a root's checked IR.
prepareRootGeneralizationContextWithTiming
  :: TimingConfig
  -> String
  -> TraceConfig
  -> IdentityGenerator
  -> RootPresolutionContext
  -> IO (Either PipelineError (PreparedRootFinalizationContext, IdentityGenerator))
prepareRootGeneralizationContextWithTiming timing label traceCfg identityGenerator presolutionContext =
  timeProgramOperationWithSuffixIO timing label ".prepare_generalization" $
    evaluate $ do
      (annExpectedTypes, preparationIdentityGenerator) <-
        fromElabError
          ( preparedAnnotationExpectedTypesWithSourceBinderAliases
              identityGenerator
              (rpSourceTypeBinderAliases partition)
              (rfcPreparedExternalBindings finalizationContext)
              (rpAnnSourceTypes partition)
          )
      prepared0 <-
        fromSolveError
          ( prepareGeneralizationArtifact
              traceCfg
              preparationIdentityGenerator
              (rpExactProducerTypes partition)
              annExpectedTypes
              (rpSourceTypeBinderIdentities partition)
              (rpcAcyclicConstraint presolutionContext)
              (rpcPresolution presolutionContext)
              (rpAnnotated partition)
          )
      let prepared =
            withPreparedResolvedTermSchemes
              ( preparedExternalSchemesByIdentity
                  (rfcPreparedExternalBindings finalizationContext)
              )
              prepared0
      identityGenerator' <-
        fromElabError (preparedIdentityGenerator prepared)
      pure
        ( PreparedRootFinalizationContext
            { prfcFinalizationContext = finalizationContext,
              prfcPreparedGeneralization = prepared
            },
          identityGenerator'
        )
  where
    finalizationContext = rpcFinalizationContext presolutionContext
    partition = rfcPartition finalizationContext

prepareRootFinalizationStageWithTiming
  :: TimingConfig
  -> String
  -> TraceConfig
  -> PreparedRootFinalizationContext
  -> IO (Either PipelineError PreparedPipelineRootStage)
prepareRootFinalizationStageWithTiming timing label traceCfg preparedContext =
  runExceptT $ do
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
    let elabConfig = preparedElaborationConfig traceCfg prepared
        elabEnv =
          preparedElaborationEnvWithSourceBinderAliases
            (rpSourceTypeBinderAliases partition)
            annSourceTypes
            extPrepared
            prepared
    authorizedRoot <-
      evaluatePipelineEitherSuffix timing label ".authorize_elaboration_root" $
        fromElabError (authorizePreparedAnn prepared ann)
    preparePipelineRootStage
      timing
      label
      traceCfg
      extPrepared
      prepared
      elabConfig
      elabEnv
      authorizedRoot
      ann
  where
    finalizationContext = prfcFinalizationContext preparedContext
    partition = rfcPartition finalizationContext
    extPrepared = rfcPreparedExternalBindings finalizationContext
    prepared = prfcPreparedGeneralization preparedContext
    ann = rpAnnotated partition
    annSourceTypes = rpAnnSourceTypes partition

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

runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, ResolvedNormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTiming =
  runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTiming FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTimingFromSupply ::
  (Ord key) =>
  TimingConfig ->
  String ->
  IdentityGenerator ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, ResolvedNormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsWithTimingFromSupply =
  runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTimingFromSupply FinalCheckInPipeline (resultTypeDiagnosticsFromConfig defaultPipelineConfig)

runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, ResolvedNormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTiming =
  runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTiming FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled

runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTimingFromSupply ::
  (Ord key) =>
  TimingConfig ->
  String ->
  IdentityGenerator ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, ResolvedNormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedResolvedModuleKeyedDeferFinalCheckWithPreparedExternalBindingsWithTimingFromSupply =
  runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTimingFromSupply FinalCheckAfterDeferredRewrite ResultTypeDiagnosticsDisabled

runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTiming ::
  (Ord key) =>
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, ResolvedNormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTiming =
  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTimingGenerated
    generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply
    Nothing

runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTimingFromSupply ::
  (Ord key) =>
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TimingConfig ->
  String ->
  IdentityGenerator ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, ResolvedNormSurfaceExpr)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedResolvedModuleKeyedWithPreparedExternalBindingsModeWithTimingFromSupply finalCheckMode diagnosticsMode timing label identityGenerator =
  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTimingGenerated
    generateModuleConstraintsKeyedWithResolvedExternalBindingsAndTypeIdentitiesFromSupply
    (Just identityGenerator)
    finalCheckMode
    diagnosticsMode
    timing
    label

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
runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTiming =
  runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTimingGenerated
    generateModuleConstraintsKeyedWithExternalBindingsAndTypeIdentitiesFromSupply
    Nothing

runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTimingGenerated ::
  (Ord key) =>
  ( IdentityGenerator ->
    PolySyms ->
    Map.Map String SymbolIdentity ->
    Map.Map String TypeBinderIdentity ->
    Map.Map key (Map.Map String TypeBinderIdentity) ->
    ExternalBindings ->
    [(key, VarName, NormSurfaceExprOf references)] ->
    Either ConstraintError (ModuleConstraintResult key 'Raw)
  ) ->
  Maybe IdentityGenerator ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TimingConfig ->
  String ->
  PolySyms ->
  PreparedExternalBindings ->
  Map.Map key PreparedExternalBindings ->
  [(key, VarName, NormSurfaceExprOf references)] ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runPipelineElabDetailedModuleKeyedWithPreparedExternalBindingsModeWithTimingGenerated generateModuleConstraints suppliedGenerator finalCheckMode diagnosticsMode timing label polySyms extPrepared rootPrepared keyedNamedExprs =
  runExceptT $ do
    let traceCfg = pcTraceConfig defaultPipelineConfig
        preparedEnvironments = extPrepared : Map.elems rootPrepared
        namedExprs = [(name, expr) | (_, name, expr) <- keyedNamedExprs]
        rootPreparedForKey key =
          Map.findWithDefault extPrepared key rootPrepared
        rootPreparedSchemeUseCount =
          sum
            [ Map.size (pebBindingsByAlias prepared)
            | (key, _, _) <- keyedNamedExprs,
              Just prepared <- [Map.lookup key rootPrepared]
            ]
        sourceTypeHeadIdentities =
          resolvedSymbolIdentityCandidates $
            mergeSymbolIdentityCandidates
              ( pebSourceTypeHeadIdentityCandidates extPrepared
                  : map pebSourceTypeHeadIdentityCandidates (Map.elems rootPrepared)
              )
        sourceTypeBinderIdentityAliases =
          preparedSourceTypeBinderIdentities extPrepared
        rootSourceTypeBinderIdentityAliases =
          Map.map preparedSourceTypeBinderIdentities rootPrepared
    evaluatePipelineEitherSuffix timing label ".validate_annotations" $
      mapM_ (fromConstraintError . validateDirectRecursiveAnnotations . snd) namedExprs
    ModuleConstraintResult
      { mcrConstraint = c0,
        mcrRoots = roots,
        mcrIdentityGenerator = packetIdentityGenerator,
        mcrAnnSourceTypes = annSourceTypes,
        mcrExactProducerTypes = exactProducerTypes,
        mcrSourceTypeBinderIdentities = sourceTypeBinderIdentities,
        mcrRootOwnership = rootOwnership
      } <-
      evaluatePipelineEitherSuffix timing label ".generate_constraints" $
        fromConstraintError
          ( generateModuleConstraints
              ( case suppliedGenerator of
                  Nothing -> preparedExternalIdentityGenerator preparedEnvironments
                  Just generator -> preparedExternalIdentityGeneratorFrom generator preparedEnvironments
              )
              polySyms
              sourceTypeHeadIdentities
              sourceTypeBinderIdentityAliases
              rootSourceTypeBinderIdentityAliases
              (preparedExternalSourceBindings extPrepared)
              keyedNamedExprs
          )
    liftIO $
      whenProgramOperationsIO timing $
        emitModuleBatchGraphMetrics timing (label ++ ".graph") c0 rootOwnership roots annSourceTypes extPrepared rootPreparedSchemeUseCount
    let batchPlan =
          buildModuleBatchPlan
            rootPreparedForKey
            packetIdentityGenerator
            c0
            rootOwnership
            roots
            annSourceTypes
            exactProducerTypes
            sourceTypeBinderIdentities
    liftIO $
      whenProgramOperationsIO timing $
        emitModuleBatchPlanMetrics timing (label ++ ".partition") batchPlan
    if moduleBatchPlanRootLocalEligible batchPlan
      then
        ExceptT $
          runModuleBatchPlanRootLocalWithTiming timing (label ++ ".partitioned_roots") finalCheckMode diagnosticsMode traceCfg batchPlan
      else
        ExceptT $
          runModuleBatchPlanGlobalWithTiming
            timing
            label
            finalCheckMode
            diagnosticsMode
            traceCfg
            packetIdentityGenerator
            c0
            rootOwnership
            (mbpRoots batchPlan)
            annSourceTypes
            exactProducerTypes
            sourceTypeBinderIdentities

runModuleBatchPlanGlobalWithTiming ::
  (Ord key) =>
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  IdentityGenerator ->
  Constraint 'Raw ->
  RootOwnershipIndex ->
  [(key, PreparedExternalBindings, ModuleConstraintRoot)] ->
  IntMap.IntMap NormSrcType ->
  IntMap.IntMap ResolvedSrcType ->
  IntMap.IntMap TypeBinderIdentity ->
  IO (Either PipelineError (Map.Map key PipelineElabDetailedResult))
runModuleBatchPlanGlobalWithTiming timing label finalCheckMode diagnosticsMode traceCfg packetIdentityGenerator c0 rootOwnership roots annSourceTypes exactProducerTypes sourceTypeBinderIdentities =
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
            fromPresolutionError
              <$> computePresolutionWithTimingAndRootOwnership
                timing
                presolutionLabel
                traceCfg
                rootOwnership
                acyc
                cAcyclic
    prepared0 <-
      evaluatePipelineEitherSuffix timing label ".prepare_generalization" $
        do
          (annExpectedTypes, preparationIdentityGenerator) <-
            fromElabError
              ( preparedAnnotationExpectedTypesForRoots
                  packetIdentityGenerator
                  roots
                  annSourceTypes
              )
          fromSolveError $
            prepareGeneralizationArtifactForRoots
              traceCfg
              preparationIdentityGenerator
              exactProducerTypes
              annExpectedTypes
              sourceTypeBinderIdentities
              cAcyclic
              pres
              [mcrAnnotated root | (_, _, root) <- roots]
    let prepared =
          withPreparedResolvedTermSchemes
            ( Map.unions
                [ preparedExternalSchemesByIdentity externalBindings
                | (_, externalBindings, _) <- roots
                ]
            )
            prepared0
        elabConfig = preparedElaborationConfig traceCfg prepared
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
runModuleBatchPlanRootLocalWithTiming timing label finalCheckMode diagnosticsMode traceCfg plan = do
  timeProgramOperationIO timing label $ do
    let rootContexts =
          [ ( key,
              rootTimingLabel label index,
              mkRootFinalizationContext partition
            )
          | (index, (key, partition)) <-
              zip [(1 :: Int) ..] (mbpPartitions plan)
          ]
        presolutionActions =
          [ ( key,
              fmap (fmap (\context -> (rootLabel, context))) $
                prepareRootPresolutionContextWithTiming
                  timing
                  rootLabel
                  traceCfg
                  finalizationContext
            )
          | (key, rootLabel, finalizationContext) <- rootContexts
          ]
    presolutionResult <- runRootStage presolutionActions
    case presolutionResult of
      Left err -> pure (Left err)
      Right presolutionContexts -> do
        preparedResult <-
          prepareRootsInOrder
            (mbpIdentityGenerator plan)
            presolutionContexts
        case preparedResult of
          Left err -> pure (Left err)
          Right (preparedContexts, finalPacketGenerator) -> do
            preparedFinalizationResult <-
              runRootStage
                [ ( key,
                    fmap (fmap (\preparedRoot -> (rootLabel, preparedRoot))) $
                      prepareRootFinalizationStageWithTiming
                        timing
                        rootLabel
                        traceCfg
                        preparedContext
                  )
                | (key, (rootLabel, preparedContext)) <- preparedContexts
                ]
            case preparedFinalizationResult of
              Left err -> pure (Left err)
              Right preparedRoots -> do
                freshenedResult <-
                  freshenRootsInOrder finalPacketGenerator preparedRoots
                case freshenedResult of
                  Left err -> pure (Left err)
                  Right freshenedRoots ->
                    fmap (fmap Map.fromList) $
                      runRootStage
                        [ ( key,
                            runExceptT $
                              finishFreshenedPipelineRootStage
                                timing
                                rootLabel
                                finalCheckMode
                                diagnosticsMode
                                freshenedRoot
                          )
                        | (key, (rootLabel, freshenedRoot)) <- freshenedRoots
                        ]
  where
    runRootStage [] = pure (Right [])
    runRootStage [(key, action)] =
      fmap (fmap (\result -> [(key, result)])) action
    runRootStage actions = do
      ensureConcurrentCapabilities (length actions)
      workers <-
        mapM
          ( \(key, action) -> do
              done <- newEmptyMVar
              _ <-
                forkIO $
                  try action >>= putMVar done
              pure (key, done)
          )
          actions
      settled <- mapM (\(key, done) -> (\result -> (key, result)) <$> takeMVar done) workers
      case [ex | (_, Left ex) <- settled] of
        ex : _ -> throwIO (ex :: SomeException)
        [] ->
          case [err | (_, Right (Left err)) <- settled] of
            err : _ -> pure (Left err)
            [] ->
              pure
                ( Right
                    [ (key, out)
                    | (key, Right (Right out)) <- settled
                    ]
                )

    prepareRootsInOrder identityGenerator [] = pure (Right ([], identityGenerator))
    prepareRootsInOrder identityGenerator ((key, (rootLabel, presolutionContext)) : rest) = do
      preparedResult <-
        prepareRootGeneralizationContextWithTiming
          timing
          rootLabel
          traceCfg
          identityGenerator
          presolutionContext
      case preparedResult of
        Left err -> pure (Left err)
        Right (preparedContext, identityGenerator') -> do
          restResult <- prepareRootsInOrder identityGenerator' rest
          pure
            ( do
                (preparedRest, finalGenerator) <- restResult
                pure
                  ( (key, (rootLabel, preparedContext)) : preparedRest
                  , finalGenerator
                  )
            )

    freshenRootsInOrder _ [] = pure (Right [])
    freshenRootsInOrder identityGenerator ((key, (rootLabel, preparedRoot)) : rest) = do
      result <-
        runExceptT $
          closeAndFreshenPipelineRootStage
            timing
            rootLabel
            identityGenerator
            preparedRoot
      case result of
        Left err -> pure (Left err)
        Right freshenedRoot -> do
          restResult <-
            freshenRootsInOrder
              (fprsIdentityGenerator freshenedRoot)
              rest
          pure (fmap ((key, (rootLabel, freshenedRoot)) :) restResult)

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

moduleBatchPlanRootLocalEligible :: ModuleBatchPlan key p -> Bool
moduleBatchPlanRootLocalEligible plan =
  mbpSharedEdgeCount plan == 0
    && mbpUnknownEdgeCount plan == 0
    && not (null (mbpPartitions plan))

buildModuleBatchPlan ::
  (key -> PreparedExternalBindings) ->
  IdentityGenerator ->
  Constraint 'Raw ->
  RootOwnershipIndex ->
  Map.Map key ModuleConstraintRoot ->
  IntMap.IntMap NormSrcType ->
  IntMap.IntMap ResolvedSrcType ->
  IntMap.IntMap TypeBinderIdentity ->
  ModuleBatchPlan key 'Raw
buildModuleBatchPlan rootPrepared packetIdentityGenerator constraint rootOwnership roots annSourceTypes exactProducerTypes sourceTypeBinderIdentities =
  ModuleBatchPlan
    { mbpRoots = orderedRoots,
      mbpPartitions = partitions,
      mbpIdentityGenerator = packetIdentityGenerator,
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
            exactProducerTypes
            sourceTypeBinderIdentities
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
  IntMap.IntMap ResolvedSrcType ->
  IntMap.IntMap TypeBinderIdentity ->
  PreparedExternalBindings ->
  ModuleConstraintRoot ->
  RootPartitionBucket ->
  RootPartition 'Raw
buildRootPartitionFromBucket constraint annSourceTypes exactProducerTypes sourceTypeBinderIdentities rootExtPrepared root bucket =
  RootPartition
    { rpRootId = rootId,
      rpConstraint = partitionConstraint,
      rpAnnotated = mcrAnnotated root,
      rpAnnSourceTypes = IntMap.restrictKeys annSourceTypes (rpbNodeKeys bucket),
      rpExactProducerTypes =
        IntMap.restrictKeys exactProducerTypes (rpbEdgeKeys bucket),
      rpSourceTypeBinderIdentities =
        IntMap.restrictKeys sourceTypeBinderIdentities (rpbNodeKeys bucket),
      rpSourceTypeBinderAliases = mcrSourceTypeBinderAliases root,
      rpPreparedExternalBindings = rootExtPrepared,
      rpOwnedEdgeCount = IntSet.size (rpbEdgeKeys bucket),
      rpExternalSchemeUseCount = Map.size (pebBindingsByAlias rootExtPrepared)
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
    emitProgramOperationMetricIO timing (label ++ ".external_scheme_unique") (fromIntegral (Map.size (pebBindingsByAlias extPrepared)))
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
  runExceptT $ do
    identityGenerator <-
      evaluatePipelineEitherSuffix timing label ".identity_supply" $
        fromElabError (preparedIdentityGenerator prepared)
    go identityGenerator (1 :: Int) Map.empty roots
  where
    go _ _ acc [] =
      pure acc
    go identityGenerator index acc ((key, rootExtPrepared, root) : rest) = do
      let elabEnv =
            preparedElaborationEnvWithSourceBinderAliases
              (mcrSourceTypeBinderAliases root)
              annSourceTypes
              rootExtPrepared
              prepared
          rootLabel = rootTimingLabel label index
      out <-
        ExceptT $
          finishPreparedPipelineRootWithTiming
            timing
            rootLabel
            finalCheckMode
            diagnosticsMode
            traceCfg
            identityGenerator
            rootExtPrepared
            prepared
            elabConfig
            elabEnv
            (mcrAnnotated root)
      go (pedIdentityGenerator out) (index + 1) (Map.insert key out acc) rest

finishPreparedPipelineRootWithTiming ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  IdentityGenerator ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  ElabEnv 'Presolved ->
  AnnExpr ->
  IO (Either PipelineError PipelineElabDetailedResult)
finishPreparedPipelineRootWithTiming timing label finalCheckMode diagnosticsMode traceCfg identityGenerator extPrepared prepared elabConfig elabEnv annPre =
  runExceptT $ do
    authorizedRoot <-
      ExceptT
        ( pure
            ( fromElabError
                (authorizePreparedAnn prepared annPre)
            )
        )
    finishPreparedPipelineRootStage
      timing
      label
      finalCheckMode
      diagnosticsMode
      traceCfg
      identityGenerator
      extPrepared
      prepared
      elabConfig
      elabEnv
      authorizedRoot
      annPre

finishPreparedPipelineRootStage ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  TraceConfig ->
  IdentityGenerator ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  ElabEnv 'Presolved ->
  AuthorizedElaborationRoot ->
  AnnExpr ->
  PipelineStage PipelineElabDetailedResult
finishPreparedPipelineRootStage timing label finalCheckMode diagnosticsMode traceCfg identityGenerator extPrepared prepared elabConfig elabEnv authorizedRoot annPre = do
  preparedRoot <-
    preparePipelineRootStage
      timing
      label
      traceCfg
      extPrepared
      prepared
      elabConfig
      elabEnv
      authorizedRoot
      annPre
  freshenedRoot <-
    closeAndFreshenPipelineRootStage
      timing
      label
      identityGenerator
      preparedRoot
  finishFreshenedPipelineRootStage
    timing
    label
    finalCheckMode
    diagnosticsMode
    freshenedRoot

preparePipelineRootStage ::
  TimingConfig ->
  String ->
  TraceConfig ->
  PreparedExternalBindings ->
  PreparedGeneralizationArtifact ->
  ElabConfig 'Presolved ->
  ElabEnv 'Presolved ->
  AuthorizedElaborationRoot ->
  AnnExpr ->
  PipelineStage PreparedPipelineRootStage
preparePipelineRootStage timing label traceCfg extPrepared prepared elabConfig elabEnv authorizedRoot annPre = do
  let initialTcEnv = preparedExternalTypeCheckEnv extPrepared
  rootPlan <-
    evaluatePipelineEitherSuffix timing label ".plan_root_exact" $
      fromElabError (rootElaborationPlan authorizedRoot annPre)
  preparedRootConstruction <-
    evaluatePipelineEitherSuffix timing label ".generalize_root_construction" $
      fromElabError
        ( prepareRootConstruction
            prepared
            rootPlan
        )
  constructionElabEnv <-
    evaluatePipelineEitherSuffix timing label ".install_root_construction" $
      fromElabError
        ( elabEnvWithRootConstructionScope
            preparedRootConstruction
            (repElaborationCanonicalAnn rootPlan)
            elabEnv
        )
  elaboration <-
    evaluatePipelineEitherSuffix timing label ".elaborate" $
      case
          elaborateWithEnvDetailed
            elabConfig
            constructionElabEnv
            (repAuthorizedElaborationRoot rootPlan)
        of
          Right elaborated -> pure elaborated
          Left err ->
            fromElabError
              ( Left
                  ( ValidationFailed
                      [ "root construction elaboration failed",
                        "  cause=" ++ show err,
                        "  prepared exact root="
                          ++ preparedRootConstructionDiagnostic preparedRootConstruction,
                        "  construction Gamma binders="
                          ++ show
                            ( preparedRootConstructionScopeBinders
                                (rootConstructionScope preparedRootConstruction)
                            )
                      ]
                  )
              )
  let term = elaboratedTerm elaboration
      mbOwnerFinalConstruction =
        elaboratedOwnerFinalConstruction elaboration
      localGammaConstructionCertificates =
        elaboratedLocalGammaConstructionCertificates elaboration
      compilerExactResultBoundCertificates =
        elaboratedCompilerExactResultBoundCertificates elaboration
  case traceGeneralize traceCfg ("pipeline elaborated term=" ++ show term) () of
    () -> pure ()
  let sourceAuthoritativeAnnCanon =
        authoritativeRootAnn term (repElaborationCanonicalAnn rootPlan)
      sourceAuthoritativeAnnPre =
        authoritativeRootAnn term (repElaborationPrecanonicalAnn rootPlan)
      (sourceAuthoritativeAnnCanonFinal, sourceAuthoritativeAnnPreFinal) =
        stripPreparedWitnesslessAuthoritativeAnn
          prepared
          sourceAuthoritativeAnnCanon
          sourceAuthoritativeAnnPre
      (authoritativeAnnCanonFinal, authoritativeAnnPreFinal) =
        case rootPlan of
          ExactRootElaborationPlan
            { repResultCanonicalAnn = resultCanon,
              repResultPrecanonicalAnn = resultPre
            } ->
              (resultCanon, resultPre)
          OrdinaryRootElaborationPlan{} ->
            (sourceAuthoritativeAnnCanonFinal, sourceAuthoritativeAnnPreFinal)
  rootGeneralization0 <-
    evaluatePipelineEitherSuffix timing label ".generalize_root" $
      fromElabError
        ( generalizePreparedRootDetailedWithConstructionResult
            prepared
            (repElaborationCanonicalAnn rootPlan)
            (repElaborationPrecanonicalAnn rootPlan)
            authoritativeAnnCanonFinal
            mbOwnerFinalConstruction
            localGammaConstructionCertificates
        )
  rootGeneralization <-
    evaluatePipelineEitherSuffix timing label ".project_root_source_binders" $
      fromElabError
        ( applyPreparedRootSourceTypeBinderIdentities
            rootGeneralization0
        )
  let rootSubst0 = prgSubst rootGeneralization
  rootSubstAliased <-
    evaluatePipelineEitherSuffix timing label ".project_term_source_binders" $
      fromElabError
        (applyPreparedTermSourceBinderAliases prepared rootSubst0 term)
  rootGeneralizationFinal <-
    case preparedRootConstruction of
      PreparedExactRootConstruction deferred _ ->
        evaluatePipelineEitherSuffix timing label ".project_root_exact_binders" $
          fromElabError
            ( applyPreparedCompilerExactRootBinderIdentities
                prepared
                (dreaEdgeId deferred)
                rootGeneralization
                rootSubstAliased
            )
      PreparedOrdinaryRootConstruction{} ->
        evaluatePipelineEitherSuffix timing label ".project_root_term_binders" $
          fromElabError
            ( applyPreparedRootBinderIdentities
                rootSubstAliased
                rootGeneralization
            )
  let
      rootScheme = prgScheme rootGeneralizationFinal
      rootSubst = prgSubst rootGeneralizationFinal
  termSubst <-
    timePipelineValueSuffix timing label ".subst_root" $
      evaluate (substInTermRefs rootSubst term)
  compilerExactRootBinderRoutes <-
    case preparedRootConstruction of
      PreparedOrdinaryRootConstruction{} -> pure []
      PreparedExactRootConstruction deferred _ ->
        evaluatePipelineEitherSuffix timing label ".exact_root_binder_routes" $
          fromElabError
            ( preparedCompilerExactSourceResultBinderRoutes
                prepared
                (dreaEdgeId deferred)
            )
  pure
    PreparedPipelineRootStage
      { pprsPreparedGeneralization = prepared,
        pprsInitialTypeCheckEnv = initialTcEnv,
        pprsRootGeneralization = rootGeneralizationFinal,
        pprsRootScheme = rootScheme,
        pprsRootSubstitution = rootSubst,
        pprsElaboratedTerm = term,
        pprsSubstitutedTerm = termSubst,
        pprsCompilerExactResultBoundCertificates =
          projectCompilerExactResultBoundCertificates
            rootSubst
            compilerExactResultBoundCertificates,
        pprsCompilerExactRootBinderRoutes =
          compilerExactRootBinderRoutes,
        pprsAuthoritativeCanonicalAnn = authoritativeAnnCanonFinal,
        pprsAuthoritativePrecanonicalAnn = authoritativeAnnPreFinal,
        pprsRootExactness =
          case preparedRootConstruction of
            PreparedOrdinaryRootConstruction{} -> PreparedOrdinaryRoot
            PreparedExactRootConstruction deferred _ -> PreparedExactRoot deferred
      }

closeAndFreshenPipelineRootStage ::
  TimingConfig ->
  String ->
  IdentityGenerator ->
  PreparedPipelineRootStage ->
  PipelineStage FreshenedPipelineRootStage
closeAndFreshenPipelineRootStage timing label identityGenerator preparedRoot = do
  (termClosed, closedGenerator) <-
    timePipelineValueSuffix timing label ".close_term" $
      evaluate
        ( closePipelineTermFromSupply
            identityGenerator
            (pprsRootExactness preparedRoot)
            (pprsCompilerExactRootBinderRoutes preparedRoot)
            initialTcEnv
            rootSubst
            rootClosure
            term
            termSubst
        )
  (termConstructed, authoritativeResultType) <-
    case pprsRootExactness preparedRoot of
      PreparedOrdinaryRoot -> pure (termClosed, schemeToType rootScheme)
      PreparedExactRoot deferred ->
        evaluatePipelineEitherSuffix timing label ".construct_root_exact" $ do
          sourceCompletedTerm <-
            fromElabError
              ( completePreparedCompilerExactSubtermResults
                  prepared
                  (pprsCompilerExactResultBoundCertificates preparedRoot)
                  CompleteBeforeCompilerExact
                  (dreaEdgeId deferred)
                  termClosed
              )
          expectedExactType <-
            fromElabError
              ( preparedCompilerExactExpectedType
                  prepared
                  (dreaEdgeId deferred)
              )
          let closedExpectedExactType =
                closePreparedExactExpectedType
                  rootClosure
                  expectedExactType
          exactTerm <-
            case
                elaborateClosedExactAnnotationTermAtType
                  initialTcEnv
                  closedExpectedExactType
                  (dreaEdgeId deferred)
                  sourceCompletedTerm
              of
                Left err ->
                  fromElabError
                    ( Left
                        ( ValidationFailed
                            [ "root compiler-exact construction failed"
                            , "  cause: " ++ show err
                            ]
                        )
                    )
                Right result -> pure result
          completedTerm <-
            fromElabError
              ( completePreparedCompilerExactSubtermResults
                  prepared
                  (pprsCompilerExactResultBoundCertificates preparedRoot)
                  CompleteAfterCompilerExact
                  (dreaEdgeId deferred)
                  exactTerm
              )
          exactTy <-
            case typeCheckWithEnv initialTcEnv completedTerm of
              Right ty -> pure ty
              Left err ->
                fromElabError
                  ( Left
                      ( ValidationFailed
                          [ "root compiler-exact completion failed"
                          , "  cause: " ++ show err
                          ]
                      )
                  )
          pure (completedTerm, exactTy)
  (termClosedFresh0, freshenedGenerator) <-
    case pprsRootExactness preparedRoot of
      PreparedExactRoot{} -> pure (termConstructed, closedGenerator)
      PreparedOrdinaryRoot ->
        timePipelineValueSuffix timing label ".freshen_type_abs" $
          evaluate
            (freshenTypeAbsAgainstEnvFromSupply closedGenerator initialTcEnv termConstructed)
  let reservedGenerator =
        advanceIdentityGeneratorPastMany
          ( generatedIdentitiesInTerm termClosedFresh0
              ++ generatedIdentitiesInType (schemeToType rootScheme)
          )
          freshenedGenerator
  pure
    FreshenedPipelineRootStage
      { fprsPreparedRoot = preparedRoot,
        fprsClosedTerm = termClosedFresh0,
        fprsAuthoritativeResultType = authoritativeResultType,
        fprsIdentityGenerator = reservedGenerator
      }
  where
    initialTcEnv = pprsInitialTypeCheckEnv preparedRoot
    prepared = pprsPreparedGeneralization preparedRoot
    rootSubst = pprsRootSubstitution preparedRoot
    rootScheme = pprsRootScheme preparedRoot
    rootClosure = prgClosure (pprsRootGeneralization preparedRoot)
    term = pprsElaboratedTerm preparedRoot
    termSubst = pprsSubstitutedTerm preparedRoot

finishFreshenedPipelineRootStage ::
  TimingConfig ->
  String ->
  PipelineFinalCheckMode ->
  ResultTypeDiagnosticsMode ->
  FreshenedPipelineRootStage ->
  PipelineStage PipelineElabDetailedResult
finishFreshenedPipelineRootStage timing label finalCheckMode diagnosticsMode freshenedRoot = do
  termClosedFresh <-
    case pprsRootExactness preparedRoot of
      PreparedExactRoot{} -> pure termClosedFresh0
      PreparedOrdinaryRoot ->
        timePipelineValueSuffix timing label ".canonicalize_term_types" $
          evaluate (TypeCheck.canonicalizeResolvedTermTypes initialTcEnv termClosedFresh0)
  let resultGenerator resultType =
        advanceIdentityGeneratorPastMany
          (generatedIdentitiesInTerm termClosedFresh ++ generatedIdentitiesInType resultType)
          identityGenerator
      uncheckedResultType = fprsAuthoritativeResultType freshenedRoot
      uncheckedAuthoritative =
        PipelineElabDetailedResult
          { pedTerm = termClosedFresh,
            pedType = uncheckedResultType,
            pedRootAnn = authoritativeAnnCanonFinal,
            pedTypeCheckEnv = initialTcEnv,
            pedIdentityGenerator = resultGenerator uncheckedResultType
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
                  pedTypeCheckEnv = initialTcEnv,
                  pedIdentityGenerator = resultGenerator tyChecked
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
  where
    preparedRoot = fprsPreparedRoot freshenedRoot
    prepared = pprsPreparedGeneralization preparedRoot
    initialTcEnv = pprsInitialTypeCheckEnv preparedRoot
    rootGeneralization = pprsRootGeneralization preparedRoot
    authoritativeAnnCanonFinal = pprsAuthoritativeCanonicalAnn preparedRoot
    authoritativeAnnPreFinal = pprsAuthoritativePrecanonicalAnn preparedRoot
    termClosedFresh0 = fprsClosedTerm freshenedRoot
    identityGenerator = fprsIdentityGenerator freshenedRoot

closePipelineTerm :: TypeCheck.Env -> IntMap.IntMap TypeBinderRef -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
closePipelineTerm initialTcEnv rootSubst rootScheme term termSubst =
  fst
    ( closePipelineTermFromSupply
        (identityGeneratorAfter closeInputs)
        PreparedOrdinaryRoot
        []
        initialTcEnv
        rootSubst
        (PreparedWholeRootClosure [] rootScheme)
        term
        termSubst
    )
  where
    closeInputs =
      generatedIdentitiesInTerm term
        ++ generatedIdentitiesInTerm termSubst
        ++ generatedIdentitiesInType (schemeToType rootScheme)
        ++ pipelineTypeCheckEnvGeneratedIdentities initialTcEnv

-- | A compiler-exact root is checked after its prepared closure has already
-- emitted the root type abstractions.  Close the source-owned expected type
-- with exactly that prepared binder spine before comparing the two endpoints;
-- a post-hoc rule that merely peels matching foralls would lose both Gamma
-- ownership and bound authority.
closePreparedExactExpectedType :: PreparedRootClosure -> ElabType -> ElabType
closePreparedExactExpectedType rootClosure expectedType =
  foldr closeIfFree expectedType (schemeBinderRefs rootClosureScheme)
  where
    rootClosureScheme = preparedRootClosureScheme rootClosure

    closeIfFree (ref, mbBound) ty
      | any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType ty) =
          TForallRef ref mbBound ty
      | otherwise = ty

closePipelineTermFromSupply :: IdentityGenerator -> PreparedRootExactness -> [(TypeBinderRef, TypeBinderRef)] -> TypeCheck.Env -> IntMap.IntMap TypeBinderRef -> PreparedRootClosure -> XmlfTerm -> XmlfTerm -> (XmlfTerm, IdentityGenerator)
closePipelineTermFromSupply suppliedGenerator rootExactness exactRootBinderRoutes initialTcEnv rootSubst rootClosure term termSubst =
  let termSubstType = typeCheckWithEnv initialTcEnv termSubst
      rootClosureScheme = preparedRootClosureScheme rootClosure
      rootType = schemeToType rootClosureScheme
      generator0 =
        advanceIdentityGeneratorPastMany
          ( generatedIdentitiesInTerm term
              ++ generatedIdentitiesInTerm termSubst
              ++ generatedIdentitiesInType rootType
              ++ pipelineTypeCheckEnvGeneratedIdentities initialTcEnv
          )
          suppliedGenerator
      retainedChildAuthoritativeCandidate =
        case rootClosure of
          PreparedLocalRootClosure{} -> False
          PreparedWholeRootClosure{} ->
            case preserveRetainedChildAuthoritativeResult termSubst of
              Just _ -> True
              Nothing -> False
      termClosed0 =
        case (rootExactness, rootClosure) of
          (PreparedExactRoot{}, _) ->
            -- Exact source binders are outer construction authority. Reuse
            -- only an abstraction with the same identity; a distinct leading
            -- abstraction is a local Gamma that must remain inside the exact
            -- root rather than being positionally renamed to it.
            constructTermWithSchemeSubstRefsByBinderRoutes
              exactRootBinderRoutes
              rootSubst
              rootClosureScheme
              term
          -- A local root closure has already constructed the leading forall
          -- spine stored in the scheme body.  Its explicit scheme binders are
          -- the distinct outer/root-owned spine, so emit exactly those here.
          -- The generic "if needed" closer cannot infer this split from the
          -- finished term: it would align the first local abstraction with the
          -- first root binder and then decline to add the missing outer binder.
          (PreparedOrdinaryRoot, PreparedLocalRootClosure{}) ->
            constructTermWithSchemeSubstRefs rootSubst rootClosureScheme term
          (PreparedOrdinaryRoot, PreparedWholeRootClosure{}) ->
            if retainedChildAuthoritativeCandidate
              then closeTermWithSchemeSubstRefsIfNeeded rootSubst rootClosureScheme term
              else if rootSchemeHasPolymorphicBound rootClosureScheme
                then closeTermWithSchemeSubstRefsIfNeeded rootSubst rootClosureScheme termSubst
              else
                case termSubstType of
                  Right ty
                    | ty == rootType
                        || alphaEqType ty rootType
                        || churchAwareEqType ty rootType ->
                        -- Type equality aligns the published endpoint with the
                        -- prepared scheme, but it does not prove that the term
                        -- constructed the scheme's Lambda spine.  In
                        -- particular, an application can return a forall
                        -- directly.  At a publication boundary that value must
                        -- still be abstracted to the scheme body and wrapped
                        -- with the prepared binders.  Preserve an existing
                        -- explicit spine, including source-owned binder
                        -- identities, and construct only when it is absent.
                        if termConstructsRootForallSpine rootClosureScheme termSubst
                          then termSubst
                          else
                            closeTermWithSchemeSubstRefsIfNeeded
                              rootSubst
                              rootClosureScheme
                              termSubst
                    | not (null (freeTypeVarRefsType ty)),
                      termConstructsRootForallSubsequence
                        rootClosureScheme
                        termSubst ->
                        closeTermWithSchemeSubstRefsIfNeeded
                          rootSubst
                          rootClosureScheme
                          term
                    | not (null (freeTypeVarRefsType ty)),
                      rootSchemeMatchesOpenTerm rootClosureScheme ty ->
                        closeTermWithSchemeSubstRefsIfNeeded rootSubst rootClosureScheme term
                    | otherwise -> termSubst
                  Left _ -> closeTermWithSchemeSubstRefsIfNeeded rootSubst rootClosureScheme term
   in case rootClosure of
        PreparedLocalRootClosure{} -> (termClosed0, generator0)
        PreparedWholeRootClosure{} ->
          case preserveRetainedChildAuthoritativeResult termClosed0 of
            Just termAdjusted ->
              closeRetainedChildAuthoritativeTermFromSupply generator0 initialTcEnv rootSubst rootClosureScheme termAdjusted
            Nothing
              | retainedChildAuthoritativeCandidate ->
                  closeRetainedChildAuthoritativeTermFromSupply generator0 initialTcEnv rootSubst rootClosureScheme termClosed0
            Nothing -> (termClosed0, generator0)

rootSchemeMatchesOpenTerm :: ElabScheme -> ElabType -> Bool
rootSchemeMatchesOpenTerm scheme ty =
  case
      matchTypeRefs
        (map fst (schemeBinderRefs scheme))
        (schemeBody scheme)
        ty
    of
      Right _ -> True
      Left _ -> False

-- | A term whose result path already spells the root's forall spine owns that
-- construction, even when an explicit source annotation chose binder
-- identities different from the graph presentation.  A bare producer whose
-- type merely happens to be forall-shaped does not: closing it must emit the
-- root Lambda/applications explicitly so the term and published scheme share
-- one operational binder identity.
termConstructsRootForallSpine :: ElabScheme -> XmlfTerm -> Bool
termConstructsRootForallSpine scheme =
  go (length (schemeBinderRefs scheme))
  where
    go 0 _ = True
    go remaining term =
      case term of
        ETyAbsRef _ _ body -> go (remaining - 1) body
        ELet resolved _ rhs body ->
          case body of
            EVarNode occurrence
              | resolvedVarSameIdentity resolved occurrence ->
                  go remaining rhs
            _ -> go remaining body
        _ -> False

-- | A source constructor can own a later binder in the root scheme while an
-- inferred existential remains root-owned.  The paper's
-- @exists beta. forall alpha. ...@ annotation is the representative case:
-- annotation elaboration constructs @Lambda alpha@, then root closure must
-- insert @Lambda beta@ before it.  Full binder identity, not the number or
-- spelling of abstractions, is the construction proof.
termConstructsRootForallSubsequence :: ElabScheme -> XmlfTerm -> Bool
termConstructsRootForallSubsequence scheme term =
  case constructedRefs term of
    [] -> False
    refs -> refs `isIdentitySubsequenceOf` map fst (schemeBinderRefs scheme)
  where
    constructedRefs current =
      case current of
        ETyAbsRef ref _ body -> ref : constructedRefs body
        ELet resolved _ rhs body ->
          case body of
            EVarNode occurrence
              | resolvedVarSameIdentity resolved occurrence ->
                  constructedRefs rhs
            _ -> constructedRefs body
        _ -> []

    [] `isIdentitySubsequenceOf` _ = True
    _ `isIdentitySubsequenceOf` [] = False
    refs@(ref : rest) `isIdentitySubsequenceOf` (candidate : candidates)
      | typeBinderRefsSameIdentity ref candidate =
          rest `isIdentitySubsequenceOf` candidates
      | otherwise =
          refs `isIdentitySubsequenceOf` candidates

rootSchemeHasPolymorphicBound :: ElabScheme -> Bool
rootSchemeHasPolymorphicBound =
  any (maybe False (containsForall . tyToElab) . snd) . schemeBinderRefs
  where
    containsForall ty =
      case ty of
        TForallRef {} -> True
        TArrow dom cod -> containsForall dom || containsForall cod
        TConWithIdentity _ _ args -> any containsForall args
        TVarAppRef _ args -> any containsForall args
        TMuRef _ body -> containsForall body
        _ -> False

closeRetainedChildAuthoritativeTermFromSupply :: IdentityGenerator -> TypeCheck.Env -> IntMap.IntMap TypeBinderRef -> ElabScheme -> XmlfTerm -> (XmlfTerm, IdentityGenerator)
closeRetainedChildAuthoritativeTermFromSupply generator initialTcEnv rootSubst rootScheme termAdjusted =
  let closed = closeTermWithSchemeSubstRefsIfNeeded rootSubst rootScheme termAdjusted
   in if retainedChildCanUseRepresentativeScheme rootScheme
        then case retainedChildRepresentativeTermFromSupply generator initialTcEnv closed of
          Just representativeClosed -> representativeClosed
          Nothing
            | retainedChildIdentityRootScheme rootScheme ->
                case retainedChildRepresentativeTermFromSupply generator initialTcEnv termAdjusted of
                  Just representativeAdjusted -> representativeAdjusted
                  Nothing -> (closed, generator)
          Nothing -> (closed, generator)
        else (closed, generator)

retainedChildCanUseRepresentativeScheme :: ElabScheme -> Bool
retainedChildCanUseRepresentativeScheme rootScheme =
  null (schemeBinderRefs rootScheme) || retainedChildIdentityRootScheme rootScheme

retainedChildIdentityRootScheme :: ElabScheme -> Bool
retainedChildIdentityRootScheme rootScheme = case schemeToType rootScheme of
  TForallRef ref Nothing (TVarRef bodyRef) -> typeBinderRefsSameIdentity ref bodyRef
  _ -> False

retainedChildRepresentativeTermFromSupply :: IdentityGenerator -> TypeCheck.Env -> XmlfTerm -> Maybe (XmlfTerm, IdentityGenerator)
retainedChildRepresentativeTermFromSupply generator initialTcEnv term =
  case typeCheckWithEnv initialTcEnv term of
    Right ty
      | containsRecursiveType ty,
        countLeadingUnboundedForalls ty == 0 ->
          let (representativeScheme, generator') =
                retainedChildRepresentativeSchemeFromSupply generator term ty
              representativeClosed =
                closeTermWithSchemeSubstRefsIfNeeded IntMap.empty representativeScheme term
           in case typeCheckWithEnv initialTcEnv representativeClosed of
                Right representativeTy
                  | countLeadingUnboundedForalls representativeTy == 2 ->
                      Just (representativeClosed, generator')
                _ -> Nothing
    _ -> Nothing

retainedChildRepresentativeSchemeFromSupply :: IdentityGenerator -> XmlfTerm -> ElabType -> (ElabScheme, IdentityGenerator)
retainedChildRepresentativeSchemeFromSupply suppliedGenerator term ty =
  let generator0 =
        advanceIdentityGeneratorPastMany
          (generatedIdentitiesInTerm term ++ generatedIdentitiesInType ty)
          suppliedGenerator
      used0 = freeTypeVarAliasNamesType ty
      (outerRef, generator1) = freshTypeBinderRefFromNames used0 generator0
      used1 = typeBinderRefAliasNames outerRef `Set.union` used0
      (innerRef, generator2) = freshTypeBinderRefFromNames used1 generator1
   in (mkElabSchemeWithRefs [(outerRef, Nothing), (innerRef, Nothing)] ty, generator2)

countLeadingUnboundedForalls :: ElabType -> Int
countLeadingUnboundedForalls ty = case ty of
  TForallRef _ Nothing body -> 1 + countLeadingUnboundedForalls body
  _ -> 0

freshenTypeAbsAgainstEnv :: TypeCheck.Env -> XmlfTerm -> XmlfTerm
freshenTypeAbsAgainstEnv env term0 =
  fst
    ( freshenTypeAbsAgainstEnvFromSupply
        (identityGeneratorAfterTerm seedTerm)
        env
        term0
    )
  where
    summary = summarizePipelineTypeCheckEnv env
    visibleRefs = pipelineVisibleTypeVarRefs summary
    seedTerm = foldr (`ETyAbsRef` Nothing) term0 visibleRefs

freshenTypeAbsAgainstEnvFromSupply :: IdentityGenerator -> TypeCheck.Env -> XmlfTerm -> (XmlfTerm, IdentityGenerator)
freshenTypeAbsAgainstEnvFromSupply suppliedGenerator env term0 =
  let summary = summarizePipelineTypeCheckEnv env
      visibleRefs = pipelineVisibleTypeVarRefs summary
      seedTerm = foldr (`ETyAbsRef` Nothing) term0 visibleRefs
      generator0 =
        advanceIdentityGeneratorPastMany
          ( generatedIdentitiesInTerm seedTerm
              ++ pipelineTypeCheckEnvGeneratedIdentities env
          )
          suppliedGenerator
   in go generator0 (pipelineFreshenReservedTypeVars summary) visibleRefs env term0
  where
    go generator used visibleRefs tcEnv term = case term of
      ETyAbsRef ref mb body ->
        let name = typeBinderRefName ref
            usedForBinder = Set.union used (maybe Set.empty freeTypeVarAliasNamesType mb)
            refInScope =
              any (typeBinderRefsSameIdentity ref) visibleRefs
            -- Exact identity determines lexical capture.  A display-name or
            -- stable-alias collision still needs alpha-renaming for the
            -- identity-erasing xMLF projection, but it must retain the graph
            -- binder identity.  Only an exact in-scope identity collision
            -- allocates a fresh identity below.
            needsFreshening = refInScope || Set.member name usedForBinder
            (ref', generator', bodyForName) =
              if needsFreshening
                then
                  let fresh = freshNameLike name usedForBinder
                      (freshRef, generator1) =
                        if refInScope
                          then freshTypeBinderRef fresh generator
                          else (renameTypeBinderRef fresh ref, generator)
                      bodyRenamed = renameTypeVarInTermAgainstEnv tcEnv ref freshRef body
                   in (freshRef, generator1, bodyRenamed)
                else (ref, generator, body)
            usedBody = typeBinderRefAliasNames ref' `Set.union` usedForBinder
            visibleBody = unionTypeRefs [ref'] visibleRefs
            tcEnv' = TypeCheck.insertTypeBindingRef ref' (maybe TBottom tyToElab mb) tcEnv
            (body', generator'') = go generator' usedBody visibleBody tcEnv' bodyForName
         in (ETyAbsRef ref' mb body', generator'')
      ELam resolved body ->
        let ty = resolvedVarType resolved
            used' = Set.union used (freeTypeVarAliasNamesType ty)
            visibleRefs' = unionTypeRefs (freeTypeVarRefsType ty) visibleRefs
            tcEnv' = TypeCheck.insertResolvedTermBinding resolved ty tcEnv
            (body', generator') = go generator used' visibleRefs' tcEnv' body
         in (ELam resolved body', generator')
      EApp f a ->
        let (f', generator') = go generator used visibleRefs tcEnv f
            (a', generator'') = go generator' used visibleRefs tcEnv a
         in (EApp f' a', generator'')
      ELet resolved sch rhs body ->
        let ty = schemeToType sch
            used' = Set.union used (freeTypeVarAliasNamesType ty)
            visibleRefs' = unionTypeRefs (freeTypeVarRefsType ty) visibleRefs
            tcEnv' = TypeCheck.insertResolvedTermBinding resolved ty tcEnv
            (rhs', generator') = go generator used' visibleRefs' tcEnv' rhs
            (body', generator'') = go generator' used' visibleRefs' tcEnv' body
         in (ELet resolved sch rhs' body', generator'')
      ETyInst t inst ->
        let (t', generator') = go generator used visibleRefs tcEnv t
         in (ETyInst t' inst, generator')
      ERoll ty body ->
        let (body', generator') = go generator used visibleRefs tcEnv body
         in (ERoll ty body', generator')
      EUnroll body ->
        let (body', generator') = go generator used visibleRefs tcEnv body
         in (EUnroll body', generator')
      _ -> (term, generator)

renameTypeVarInTermAgainstEnv :: TypeCheck.Env -> TypeBinderRef -> TypeBinderRef -> XmlfTerm -> XmlfTerm
renameTypeVarInTermAgainstEnv env oldRef newRef = go env
  where
    renameTy = substTypeCaptureRef oldRef (TVarRef newRef)
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

    go tcEnv term = case project term of
      EVarNodeF resolved ->
        case TypeCheck.lookupResolvedTermEnvEntry (TypeCheck.resolvedTermEnv tcEnv) resolved of
          Just (_, ty) -> EVarNode (mapResolvedVarType (const ty) resolved)
          Nothing -> EVarNode (mapResolvedVarType renameTy resolved)
      ELitF lit -> ELit lit
      ELamF resolved body ->
        let resolved' = mapResolvedVarType renameTy resolved
            tcEnv' = TypeCheck.insertResolvedTermBinding resolved' (resolvedVarType resolved') tcEnv
         in ELam resolved' (go tcEnv' body)
      EAppF f a -> EApp (go tcEnv f) (go tcEnv a)
      ELetF resolved sch rhs body ->
        let sch' = renameScheme sch
            schTy = schemeToType sch'
            resolved' = mapResolvedVarType (const schTy) resolved
            tcEnv' = TypeCheck.insertResolvedTermBinding resolved' schTy tcEnv
         in ELet resolved' sch' (go tcEnv' rhs) (go tcEnv' body)
      ETyAbsFRef ref mb body
        | typeBinderRefsSameIdentity ref oldRef -> eTyAbsWithRef ref (fmap renameBound mb) body
        | otherwise ->
            let mb' = fmap renameBound mb
                tcEnv' = TypeCheck.insertTypeBindingRef ref (maybe TBottom tyToElab mb') tcEnv
             in eTyAbsWithRef ref mb' (go tcEnv' body)
      ETyInstF t inst -> ETyInst (go tcEnv t) (renameInst inst)
      ERollF ty body -> ERoll (renameTy ty) (go tcEnv body)
      EUnrollF body -> EUnroll (go tcEnv body)

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

pipelineTypeCheckEnvGeneratedIdentities :: TypeCheck.Env -> [UniqueIdentity]
pipelineTypeCheckEnvGeneratedIdentities env =
  concat
    [ idDetailsGeneratedIdentities (resolvedVarDetails resolved)
        ++ generatedIdentitiesInType ty
    | (resolved, ty) <- TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv env)
    ]
    ++ concatMap generatedIdentitiesInType (Map.elems (TypeCheck.typeEnv env))
    ++ concatMap
      (typeBinderGeneratedIdentities . typeBinderRefIdentity)
      (Map.keys (TypeCheck.typeEnv env))

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

insertFreeVarRefs :: [TypeBinderRef] -> FreeVarCounts -> FreeVarCounts
insertFreeVarRefs refs (FreeVarCounts counts) =
  FreeVarCounts (foldl' insertOne counts refs)
  where
    insertOne [] ref = [(ref, 1)]
    insertOne ((existing, count) : rest) ref
      | typeBinderRefsSameIdentity existing ref = (existing, count + 1) : rest
      | otherwise = (existing, count) : insertOne rest ref

unionTypeRefs :: [TypeBinderRef] -> [TypeBinderRef] -> [TypeBinderRef]
unionTypeRefs left right =
  foldr insertRef right left
  where
    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

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

authoritativeRootAnn :: XmlfTerm -> AnnExpr -> AnnExpr
authoritativeRootAnn term annExpr =
  case (stripLeadingTyAbs term, annExpr) of
    (term0, AAnn inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (term0, AUnfold inner _ _)
      | shouldStripAuthoritativeAnn term0 ->
          authoritativeRootAnn term0 inner
    (term0, ALetScope inner _ _) ->
      authoritativeRootAnn term0 inner
    (term0, ALet _ binderDetails _ _ _ _ _ bodyAnn _) ->
      case term0 of
        ELet resolved _ _ bodyTerm
          | idDetailsSameIdentity binderDetails (resolvedVarDetails resolved) ->
              authoritativeRootAnn bodyTerm bodyAnn
        ELet {} -> annExpr
        _ -> authoritativeRootAnn term0 bodyAnn
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
        AResolvedVar details _ _ ->
          idDetailsSameIdentity details (resolvedVarDetails resolved)
        AAnn inner _ _ -> go inner
        AUnfold inner _ _ -> go inner
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

prepareExternalBindingEntries ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  IdentityGenerator ->
  ExternalBindings ->
  Either ConstraintError (IdentityGenerator, Map.Map VarName PreparedExternalBinding)
prepareExternalBindingEntries sharedHeadIdentities sharedBinderIdentities generator0 extBindings = do
  (generator, preparedBindings, _) <- foldM addBinding (generator0, Map.empty, Map.empty) (Map.toList extBindings)
  pure (generator, preparedBindings)
  where
    addBinding (generator, acc, identitySchemes) (name, binding) =
      case Map.lookup (externalBindingKey binding) identitySchemes of
        Just (cachedBinding, schemeInfo, inferredBinderIdentities)
          | cachedBinding /= binding ->
              Left
                ( InternalConstraintError
                    "external bindings with one semantic identity carry conflicting type payloads"
                )
          | otherwise ->
              let binding' = attachInferredBinderIdentities binding inferredBinderIdentities
               in pure
                    ( generator,
                      Map.insert name (PreparedExternalBinding binding' schemeInfo) acc,
                      identitySchemes
                    )
        Nothing -> do
          (schemeInfo, inferredBinderIdentities, generator') <-
            externalBindingSchemeInfoWithGenerator
              generator
              binding
                { externalBindingTypeHeadIdentities =
                    mergeSymbolIdentityMaps
                      [ externalBindingTypeHeadIdentities binding,
                        sharedHeadIdentities
                      ],
                  externalBindingTypeBinderIdentities =
                    mergeTypeBinderIdentityMaps
                      [ externalBindingTypeBinderIdentities binding,
                        sharedBinderIdentities
                      ]
                }
          let binding' = attachInferredBinderIdentities binding inferredBinderIdentities
              identitySchemes' =
                Map.insert
                  (externalBindingKey binding)
                  (binding, schemeInfo, inferredBinderIdentities)
                  identitySchemes
          pure
            ( generator',
              Map.insert name (PreparedExternalBinding binding' schemeInfo) acc,
              identitySchemes'
            )

    attachInferredBinderIdentities binding inferred =
      binding
        { externalBindingTypeBinderIdentities =
            mergeTypeBinderIdentityMaps
              [ externalBindingTypeBinderIdentities binding,
                inferred
              ]
        }

    externalBindingKey =
      idDetailsIdentityKey
        . externalBindingDetails
        . externalBindingIdentity

externalBindingSchemeInfoWithGenerator :: IdentityGenerator -> ExternalBinding -> Either ConstraintError (SchemeInfo, Map.Map String TypeBinderIdentity, IdentityGenerator)
externalBindingSchemeInfoWithGenerator generator0 ExternalBinding {externalBindingType = srcTy, externalBindingTypeHeadIdentities = headIdentities, externalBindingTypeBinderIdentities = binderIdentities} = do
  (scheme, inferredBinderIdentities, generator) <- srcTypeToElabSchemeWithFresh headIdentities binderIdentities generator0 srcTy
  pure (schemeInfoFromRefSubst scheme IntMap.empty, inferredBinderIdentities, generator)

srcTypeToElabSchemeWithFresh :: Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> IdentityGenerator -> NormSrcType -> Either ConstraintError (ElabScheme, Map.Map String TypeBinderIdentity, IdentityGenerator)
srcTypeToElabSchemeWithFresh headIdentities binderIdentities generator0 srcTy = do
  let freeNames = freeSrcTypeVarsInOrder srcTy
      (refs, generator1) = sourceTypeBinderRefsFromIdentities binderIdentities freeNames generator0
  (ty, generator2) <- srcTypeToElabTypeWith headIdentities binderIdentities refs generator1 srcTy
  let explicitScheme = schemeFromType ty
      explicitRefs = map fst (schemeBinderRefs explicitScheme)
      freeBinds =
        [ (ref, Nothing)
        | name <- freeNames,
          Just ref <- [Map.lookup name refs],
          any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType (schemeBody explicitScheme)),
          not (any (typeBinderRefsSameIdentity ref) explicitRefs)
        ]
      inferredBinderIdentities =
        mergeTypeBinderIdentityMaps
          [ Map.fromList
              [ (name, typeBinderRefIdentity ref)
              | name <- freeNames,
                Just ref <- [Map.lookup name refs]
              ],
            sourceDeclarationBinderIdentities ty
          ]
  pure
    ( mkElabSchemeWithRefs (freeBinds ++ schemeBinderRefs explicitScheme) (schemeBody explicitScheme),
      inferredBinderIdentities,
      generator2
    )

-- | Recover the exact identities allocated for binder declarations while
-- preparing an external scheme.  Constraint generation internalizes the
-- source type again, so these declaration-copy identities must travel on the
-- external binding rather than being rediscovered from graph roots later.
--
-- Ambiguous display aliases are deliberately removed by
-- 'typeBinderIdentityAliasMap'; each identity's stable alias remains exact.
sourceDeclarationBinderIdentities :: ElabType -> Map.Map String TypeBinderIdentity
sourceDeclarationBinderIdentities =
  typeBinderIdentityAliasMap
    . map
      ( \ref ->
          (typeBinderRefName ref, typeBinderRefIdentity ref)
      )
    . declarationBinderRefs
  where
    declarationBinderRefs ty =
      case ty of
        TVarRef {} -> []
        TArrow dom cod ->
          declarationBinderRefs dom ++ declarationBinderRefs cod
        TBaseWithIdentity {} -> []
        TConWithIdentity _ _ args ->
          foldMap declarationBinderRefs args
        TVarAppRef _ args ->
          foldMap declarationBinderRefs args
        TForallRef ref mbBound body ->
          ref
            : foldMap (declarationBinderRefs . tyToElab) mbBound
              ++ declarationBinderRefs body
        TMuRef ref body ->
          ref : declarationBinderRefs body
        TBottom -> []

freeSrcTypeVarsInOrder :: Surface.SrcTy n v -> [String]
freeSrcTypeVarsInOrder = orderedNub . collect Set.empty
  where
    collect :: Set.Set String -> Surface.SrcTy n0 v0 -> [String]
    collect bound srcTy =
      case srcTy of
        Surface.STVar name
          | name `Set.member` bound -> []
          | otherwise -> [name]
        Surface.STArrow dom cod -> collect bound dom ++ collect bound cod
        Surface.STBase {} -> []
        Surface.STCon _ args -> foldMap (collect bound) args
        Surface.STVarApp name args ->
          [name | name `Set.notMember` bound] ++ foldMap (collect bound) args
        Surface.STTyLam name body -> collect (Set.insert name bound) body
        Surface.STTyApp fun arg -> collect bound fun ++ collect bound arg
        Surface.STForall name mb body ->
          maybe [] (collect bound . Surface.unSrcBound) mb
            ++ collect (Set.insert name bound) body
        Surface.STMu name body -> collect (Set.insert name bound) body
        Surface.STBottom -> []

    orderedNub = reverse . snd . foldl add (Set.empty, [])
    add (seen, acc) name
      | name `Set.member` seen = (seen, acc)
      | otherwise = (Set.insert name seen, name : acc)

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
  Surface.STBase name -> do
    identity <- sourceTypeHeadIdentity name
    Right (TBaseWithIdentity identity (builtinBaseTy name), generator)
  Surface.STCon name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    identity <- sourceTypeHeadIdentity name
    Right (TConWithIdentity identity (builtinBaseTy name) args', generator')
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
      case lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name of
        Just identity -> Right identity
        Nothing -> Left (InternalConstraintError ("unresolved source type head `" ++ name ++ "` reached pipeline external binding preparation"))

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
      Surface.STBase name -> do
        identity <- sourceTypeHeadIdentity name
        Right (Just (TBaseWithIdentity identity (builtinBaseTy name)), generator0)
      Surface.STCon name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        identity <- sourceTypeHeadIdentity name
        Right (Just (TConWithIdentity identity (builtinBaseTy name) args'), generator1)
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
