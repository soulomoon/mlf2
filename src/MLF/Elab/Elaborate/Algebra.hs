{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Elaborate.Algebra
  ( Env,
    OwnerFinalConstruction (..),
    LocalGammaConstruction (..),
    LocalGammaConstructionCertificate (..),
    localGammaConstructionBinders,
    localGammaEmittedBinders,
    localGammaConsumedBinders,
    CompilerExactResultBoundCertificate (..),
    ElaboratedTerm (..),
    ElabOut (..),
    elabTerm,
    AlgebraContext (..),
    elabAlg,
    mkEnv,
    mkEnvWithResolvedBindings,
    lookupSchemeInfoForResolved,
    typeCheckEnvFrom,
    extendEnvTypeScope,
    extendEnvTypeScopeWithAliases,
    alignEnvToConstructionBinderRenames,
    alignEnvToCompilerExactBinderRenames,
    withEnvConstructedLambdaParamTypes,
    withEnvLocalGammaClosures,
    completeCompilerExactSubtermResults,
    completeCompilerExactSubtermResultsWithBounds,
    projectCompilerExactResultBoundCertificates,
    freshenSchemeInfoAgainstEnv,
    freshenSchemeInfoAgainstEnvWithRepresentative,
    resolvedLambdaParamNode,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, unless, when)
import Data.Functor.Foldable (para)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find, mapAccumL, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import qualified Data.Set as Set
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import MLF.Constraint.Presolution.Base
  ( EdgeArtifacts,
    edgeArtifactTrace,
    edgeArtifactWitness,
    lookupEdgeArtifact,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( AmbientGammaAuthority (..),
    GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    placeCurrentGammaRequirementsAt,
  )
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindFlag (..),
    cBindParents,
    cGraftedEdges,
    cNodes,
    EdgeId (..),
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    getEdgeId,
    getNodeId,
    lookupNodeIn,
    nodeRefKey,
    typeRef,
  )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Constraint.Types.Witness
  ( InstanceOp (..),
    ReplayContract (..),
    ewLeft,
    ewRight,
    ewWitness,
    getInstanceOps,
  )
import MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    AnnotationBoundaryRole (..),
    acIdentityEdges,
    annBinderKey,
    annExprReferenceKey,
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    elaborateExactAnnotationTerm,
    reifyInstWithFrozenEndpointsFromCheckedSource,
    reifyInstWithFrozenEndpointsFromCheckedSourceInConstructionGamma,
    reifyInstFromSourceSchemeInConstructionGamma,
    sourceSchemeInfoForConstruction,
  )
import MLF.Elab.Elaborate.Annotation.Construction (checkedArgumentClosedTopology)
import MLF.Elab.Elaborate.Algebra.BodyConsumerRoute
  ( BodyConsumerRoute (..),
    selectBodyConsumerRouteWithPacket,
    validateBodyConsumerRoute,
  )
import MLF.Elab.Elaborate.Algebra.ConstructionGamma
  ( FrozenEndpointCertificate (..),
    FrozenEndpointCertificates,
    ConstructionBinderBoundProvenance (..),
    PendingLocalResultSourcePacket,
    NestedApplicationResidualCertificate,
    LambdaParamBoundaryAuthority (..),
    LambdaParamBoundaryInstallation (..),
    OrdinaryGammaPacketConstruction (..),
    BodyConsumerBoundRefinementCertificate,
    bodyConsumerBoundRefinementTargetsAny,
    authorizeBodyConsumerDeclaration,
    authorizedBodyConsumerDeclarationBound,
    authorizedBodyConsumerRoute,
    buildAmbientGammaAuthorities,
    bodyConsumerRouteProjectionProvenance,
    certifiedSourcePacketOperatedOccurrenceRenames,
    certifiedSourcePacketOccurrenceRenames,
    certifiedSourceOccurrenceRenames,
    certifiedSourceOccurrenceRoutes,
    constructOrdinaryGammaPacket,
    constructionProtectedIdentities,
    constructionRefAlreadyInGamma,
    constructionRouteBoundCompatible,
    directAmbientGammaAuthorityProvenance,
    directSourceBinderConstructionRename,
    frozenEndpointCertificateTypes,
    inheritNestedApplicationResidualReplayAuthority,
    inferExactTransportArguments,
    installLambdaParamBoundary,
    lambdaBodyConstructionRenames,
    lambdaParamConstructionRenames,
    lambdaParamLocalGammaRenames,
    lambdaParamProtectedIdentities,
    localGammaConstructionProvenance,
    localGammaConstructionRoutes,
    mkApplicationPendingLocalResultSourcePacket,
    nestedApplicationResidualFromLocalGamma,
    nestedApplicationResidualFromZeroLocalConstruction,
    requirementNeedsLocalConstruction,
    lookupRefByIdentityOrRepresentative,
    mergeConstructionBinderBoundsByProvenance,
    mergeConstructionSourceBinderRefs,
    completeUnboundedForallSpecializesTo,
    mkValidatedBodyConsumerProjection,
    operationalEndpointTypesAgree,
    projectValidatedAmbientConsumerBoundWithCertificate,
    publishFrozenEndpointCertificate,
    renameBodyConsumerBoundRefinementCertificate,
    protectedBoundaryOccurrenceRenames,
    selectTermSourcePacketOccurrenceRenames,
    selectDirectAmbientGammaAuthority,
    selectDirectLocalApplicationArgumentTopology,
    selectLocalGammaClosureOwnerLane,
    selectBoundaryConstructionRoute,
    sourceBinderAuthorityRefs,
    transparentResultResolvedByEnclosingScheme,
    validatedBodyConsumerLeadingElimination,
    validatedBodyConsumerProjectionSourceConstructionRenames,
    validatedBodyConsumerProjectionSpecialization,
  )
import MLF.Elab.Elaborate.Scope
  ( ScopeContext (..),
    generalizeAtNode,
    normalizeSchemeSubstPair,
    normalizeSubstForScheme,
    reifyNodeTypePreferringBound,
    reifyTargetNodeType,
    scopeRootForBoundary,
    scopeRootForNode,
    scopeTypeBinderIdentityRepresentative,
  )
import MLF.Elab.Generalize
  ( PreparedSubtermGeneralization,
    CompilerExactResultStage (..),
    GaBindParents (..),
    GammaPacketAuthority (..),
    gaConstructionRouteNodes,
    LocalGammaConstructor (..),
    LocalGammaEdgeOwnership (..),
    LocalGammaClosure (..),
    LocalGammaOwner (..),
    RootRaiseMergeAuthority (..),
    SubtermGeneralizations,
    directApplicationClosureOwnsEdges,
    freshenSchemeInfoBinderNamesAgainst,
    generalizationRequirementsForRootEdges,
    generalizationRequirementsForRootEdgesInConstruction,
    inheritDescendantGammaRequirements,
    placeSubtermGeneralizationBindersWithRoutes,
    publishTopologyConsumerRoutes,
    directLetBoundaryEdge,
    lgfChildren,
    lgfDirectEdgeSources,
    lgfOwner,
    localGammaDirectApplicationEdgeOwners,
    localGammaFrame,
    rootRaiseMergeAuthorityFor,
    rootRaiseMergeExteriorOwnedByScope,
    selectLocalGammaEdgeOwnership,
    localGammaOwnerScope,
    retainedDescendantGammaClosures,
    subtermGeneralizationCompilerExactBoundary,
    subtermGeneralizationCompilerExactCompletionRef,
    subtermGeneralizationCompilerExactExistingRef,
    subtermGeneralizationCompilerExactResultRef,
    subtermGeneralizationCompilerExactResultStage,
    subtermGeneralizationCompilerExactBinderRenames,
    subtermGeneralizationConstructionBinderRenames,
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationConsumerIdentity,
    subtermConsumerAuthorityEnclosingOwner,
    subtermConsumerAuthorityIsTopology,
    subtermConsumerAuthorityIsRootGamma,
    scaConsumerIdentity,
    scaEdgeId,
    subtermGeneralizationResultAbstractionRef,
    subtermGeneralizationConstructionResultAbstractionRef,
    subtermGeneralizationGammaBoundScheme,
    subtermGeneralizationGammaAuthority,
    subtermGeneralizationLocalConsumerClosure,
    subtermGeneralizationOperatedSchemeInfo,
    subtermGeneralizationOwnsGammaForEdge,
    subtermGeneralizationSchemeInfo,
    subtermGeneralizationConsumerConstructionSchemeInfo,
    subtermGeneralizationsOwnedBy,
    subtermResultOwnershipConsumerClosedLocally,
    subtermResultOwnershipFor,
    subtermResultOwnershipHasTransparentPath,
    subtermResultOwnershipLocalSourceDeclarationRefs,
    subtermResultOwnershipPacket,
  )
import MLF.Elab.Inst
  ( applyInstantiation,
    composeInst,
    instForLeadingTypeArgument,
    schemeToType,
  )
import MLF.Elab.Phi
  ( PhiEndpointShapeAuthority (..),
    PhiOccurrenceRole (..),
  )
import MLF.Elab.Run.Instantiation
  ( inferInstAppArgsFromSchemeRefsExact,
    residualTopologyAgreesExact,
  )
import MLF.Elab.Run.Generalize.Types
  ( DirectApplicationAmbientGammaClaim (..),
    DirectApplicationGammaClaim (..),
    LocalGammaConstruction (..),
    LocalGammaConstructionCertificate (..),
    localGammaConstructionBinders,
    localGammaConsumedBinders,
    localGammaEmittedBinders,
  )
import MLF.Elab.Run.Generalize.Common
  ( nodeMapToIntMap,
    reachableFromWithBounds,
  )
import qualified MLF.Elab.Reduce as Reduce
import MLF.Elab.Run.Scope
  ( ApplicationConstructionScopes (..),
    applicationGeneralizationScopeForRequirements,
    generalizeTargetNode,
    resolveApplicationConstructionScopes,
  )
import MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    selectUniqueCandidateBy,
  )
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeWithContext, simplifyAnnotationType)
import MLF.Elab.SourceBinder
  ( orderSourceProjectedSchemeBinders,
    resolveConstructionSourceBindersInSchemeInfoExcept,
    resolveSourceBinderAliasesInType,
    sourceBinderConstructionRenames,
  )
import MLF.Elab.TermClosure
  ( closeTermWithSchemeSubstRefsIfNeeded,
    constructTermWithSchemeSubstRefs,
    renameTermTypeBinderRefPayloads,
    renameTermTypeVars,
    renameTypeBinderRefPayloads,
    refreshLocalResolvedVarType,
    substInTermRefs,
  )
import qualified MLF.Elab.TypeCheck as TypeCheck (Env (..), checkInstantiation, lookupTypeBindingRef, mkTypeCheckEnvWithResolvedTerms, resolvedTermEnvEntries, restrictResolvedTermBindings, typeCheckWithEnv)
import MLF.Elab.Types
  ( BoundType,
    ambientSchemeClosureAuthority,
    ElabScheme,
    ElabError (..),
    XmlfTerm (..),
    ElabType,
    Instantiation (..),
    ResolvedVar (..),
    SchemeInfo (..),
    Ty (..),
    sourceTypeBinderRefsFromIdentities,
    sourceTypeBinderRefOrFreshInScope,
    generatedIdentitiesInType,
    generatedIdentitiesInTerm,
    elabToBound,
    mapBoundType,
    mapResolvedVarType,
    mkElabSchemeWithRefs,
    schemeBinderRefs,
    schemeBody,
    schemeInfoFromRefSubst,
    schemeInfoBinderRefSubst,
    renameTypeBinderRef,
    TypeBinderRef,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefAliasNames,
    typeBinderRefIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    typeBinderRefsSameIdentityAndName,
    validateSchemeClosure,
    localResolvedVarFromRef,
    resolvedVarRuntimeName,
    resolvedVarSameIdentity,
    resolvedVarType,
    schemeClosureFreeRefs,
    schemeFromType,
    tyToElab,
  )
import MLF.Frontend.ConstraintGen.Types
  ( AnnExpr (..),
    AnnExprF (..),
    BindingKey (..),
    InstantiationSite (..),
    InstantiationTargetTopology (..),
    instantiationSiteEdgeId,
  )
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (resolvedSourceTypeToElabType)
import MLF.Frontend.Symbol (SymbolIdentity, lookupSymbolIdentityAlias)
import MLF.Frontend.Syntax (NormSrcType, ResolvedSrcType, SrcBound (..), SrcNorm (..), SrcTy (..), StructBound, VarName)
import MLF.Reify.TypeOps
  ( alphaEqType,
    alphaEqTypePreservingStructuralBinders,
    churchAwareEqType,
    firstNonContractiveRecursiveType,
    freeTypeVarRefsType,
    freeTypeVarsType,
    freshNameLike,
    matchTypeRefs,
    splitForallsRefs,
    substTypeCaptureRef,
  )
import MLF.Types.Identity
  ( EnvRef,
    IdDetails (..),
    IdentityGenerator,
    ResolvedTermIdentityKey,
    TypeBinderIdentity,
    UniqueIdentity,
    freshEnvRef,
    freshLocalRef,
    idDetailsIdentityKey,
    idDetailsGeneratedIdentities,
    idDetailsIsDiscard,
    idDetailsIsEvidence,
    idDetailsRenameLocal,
    identityGeneratorAfter,
    symbolGeneratedIdentities,
    typeBinderIdentityGeneratedUnique,
    typeBinderIdentityStructural,
    typeBinderIdentityStableName,
    typeBinderGeneratedIdentities,
  )
import MLF.Util.Names (alphaName)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

data EnvBinding = EnvBinding
  { ebSchemeInfo :: SchemeInfo,
    ebSchemeType :: ElabType,
    ebIdentityDetails :: IdDetails,
    ebRuntimeName :: VarName,
    ebTransparentMediator :: Maybe TransparentMediatorKind,
    ebAliasTarget :: Maybe BindingKey,
    ebExplicitRecursiveParam :: Bool
  }

data TransparentMediatorKind
  = DirectIdentityMediator
  | EtaTransparentMediator !Int
  deriving (Eq)

-- | A source-proven application that reaches its result through a transparent
-- eta mediator.  The two endpoints stay together because the same proof fixes
-- both sides of Figure 15.3.5: the argument topology used by the two child
-- computations and the result bound consumed by the enclosing Gamma.
data MediatedApplicationConstruction = MediatedApplicationConstruction
  { macArgumentType :: !ElabType,
    macResultType :: !ElabType
  }

data IdentityWrapperAlias
  = IdentityWrapperRoot
  | IdentityWrapperMediator

data ConstructionGammaCoverage
  = RequiredGammaOnly
  | OwnerLocalAndRequiredGamma
  | CompleteSchemeAndRequiredGamma

-- | One owner-local Gamma construction, including both the binders emitted by
-- this boundary and exact ambient routes inherited by its requirements.
data ConstructionGammaPlan = ConstructionGammaPlan
  { cgpBinders :: ![(TypeBinderRef, Maybe BoundType)],
    cgpAmbientAliases :: !(IntMap.IntMap TypeBinderRef)
  }
  deriving (Eq, Show)

-- | Graph routing produced for one construction Gamma.  Every requirement
-- needs replay aliases, but only a route backed by a binder emitted here or by
-- an exact ambient declaration is a construction capability.  Keep those maps
-- distinct so prepared substitution metadata cannot accidentally suppress a
-- source-derived application endpoint.
data ConstructionGammaAliases = ConstructionGammaAliases
  { cgaRoutingAliases :: !(IntMap.IntMap TypeBinderRef)
  , cgaAuthorityAliases :: !(IntMap.IntMap TypeBinderRef)
  }
  deriving (Eq, Show)

data ConsumerBoundOwnership
  = ConsumerBoundOwnedLocally !TypeBinderRef
  | ConsumerBoundOwnedByEnclosingGamma !TypeBinderRef !ElabType
  | ConsumerBoundOwnedByRootGamma !TypeBinderRef
  | ConsumerBoundOwnedByRequiredGamma !EdgeId

data LambdaConsumerConstructionPlan = LambdaConsumerConstructionPlan
  { lccSourceType :: !ElabType
  , lccSchemeInfo :: !(Maybe SchemeInfo)
  , lccExpectedEnclosingBound :: !(Maybe ElabType)
  , lccEnclosingOwnedBinderRefs :: ![TypeBinderRef]
  , lccConsumerWithoutLocalBinder :: !(Maybe PacketConsumerWithoutLocalBinder)
  }

-- | The exact semantic-to-construction route for the Gamma consumer of one
-- lambda body edge.  The semantic endpoint is always the requirement's
-- exterior graph identity; the construction endpoint must come from the
-- direct alias installed for that same exterior.  Keeping the edge, owner,
-- and exterior with both refs prevents later construction from treating a
-- quotient representative or a type-shaped peer as consumer authority.
-- | Move a completed lambda-consumer plan into the identity domain selected
-- by its exact body packet.  The plan is materialized after the child has been
-- checked, so an enclosing Gamma bound can still carry the source identity
-- that the child packet has just quotiented to its construction identity.
-- Apply that proved quotient before constructing the outgoing computation;
-- otherwise the resulting Hyp is checked against a stale source-domain bound.
applyLambdaConsumerConstructionPlanRefRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LambdaConsumerConstructionPlan
  -> LambdaConsumerConstructionPlan
applyLambdaConsumerConstructionPlanRefRenames renames plan =
  plan
    { lccSourceType =
        applyTypeVarRefRenames renames (lccSourceType plan)
    , lccSchemeInfo =
        applySchemeInfoRefRenames renames
          <$> lccSchemeInfo plan
    , lccExpectedEnclosingBound =
        applyTypeVarRefRenames renames
          <$> lccExpectedEnclosingBound plan
    , lccEnclosingOwnedBinderRefs =
        map (applyRefRenames renames) (lccEnclosingOwnedBinderRefs plan)
    , lccConsumerWithoutLocalBinder =
        alignConsumerWithoutLocalBinder
          <$> lccConsumerWithoutLocalBinder plan
    }
  where
    alignConsumerWithoutLocalBinder consumer =
      case consumer of
        PacketConsumerInheritedFree ref ->
          PacketConsumerInheritedFree (applyRefRenames renames ref)
        PacketConsumerEliminatedAtAmbientBound ref bound ->
          PacketConsumerEliminatedAtAmbientBound
            (applyRefRenames renames ref)
            (applyTypeVarRefRenames renames bound)

-- | Proof that a packet-owned consumer needs no binder at the current lambda.
-- The distinction is construction-relevant: an inherited free occurrence is
-- already part of the current scheme, whereas a vacuous packet consumer is
-- introduced only by the outgoing Hyp and obtains its bound from ambient
-- Gamma.  Treating both as a generic "missing binder" loses that ownership
-- evidence and makes valid ground-result packets indistinguishable from an
-- accidentally dropped declaration.
data PacketConsumerWithoutLocalBinder
  = PacketConsumerInheritedFree !TypeBinderRef
  | PacketConsumerEliminatedAtAmbientBound !TypeBinderRef !ElabType

data StructuralRecursiveCandidate
  = StructuralRecursiveCandidateFromHelper ElabType
  | StructuralRecursiveCandidateFromDirectCarrier ElabType

type StructuralRecursiveCandidateSelection = CandidateSelection StructuralRecursiveCandidate

pattern NoStructuralRecursiveCandidate :: StructuralRecursiveCandidateSelection
pattern NoStructuralRecursiveCandidate = NoCandidateSelection

pattern UniqueStructuralRecursiveCandidate :: StructuralRecursiveCandidate -> StructuralRecursiveCandidateSelection
pattern UniqueStructuralRecursiveCandidate candidate = UniqueCandidateSelection candidate

pattern AmbiguousStructuralRecursiveCandidate :: StructuralRecursiveCandidateSelection
pattern AmbiguousStructuralRecursiveCandidate = AmbiguousCandidateSelection

{-# COMPLETE NoStructuralRecursiveCandidate, UniqueStructuralRecursiveCandidate, AmbiguousStructuralRecursiveCandidate #-}

-- | An enclosing constructor can pass a type downward for two distinct
-- purposes.  A checking expectation guides recursive construction but owns no
-- operational xMLF endpoint.  An exact construction expectation is backed by
-- the source constructor (for example an annotation) or by a prepared
-- consumer that owns the outgoing computation.  Keeping that distinction in
-- the environment prevents an inferred graph placeholder from being promoted
-- later into an 'ApplicationResultEndpoint'.
data ExpectedTermEndpoint
  = CheckingExpectedTerm !ElabType
  | ExactConstructionExpectedTerm !ElabType
  deriving (Show)

expectedTermEndpointType :: ExpectedTermEndpoint -> ElabType
expectedTermEndpointType endpoint =
  case endpoint of
    CheckingExpectedTerm ty -> ty
    ExactConstructionExpectedTerm ty -> ty

exactConstructionExpectedType :: ExpectedTermEndpoint -> Maybe ElabType
exactConstructionExpectedType endpoint =
  case endpoint of
    CheckingExpectedTerm _ -> Nothing
    ExactConstructionExpectedTerm ty -> Just ty

mapExpectedTermEndpoint
  :: (ElabType -> ElabType)
  -> ExpectedTermEndpoint
  -> ExpectedTermEndpoint
mapExpectedTermEndpoint f endpoint =
  case endpoint of
    CheckingExpectedTerm ty -> CheckingExpectedTerm (f ty)
    ExactConstructionExpectedTerm ty ->
      ExactConstructionExpectedTerm (f ty)

projectExpectedTermEndpoint
  :: (ElabType -> Maybe ElabType)
  -> ExpectedTermEndpoint
  -> Maybe ExpectedTermEndpoint
projectExpectedTermEndpoint project endpoint =
  case project (expectedTermEndpointType endpoint) of
    Nothing -> Nothing
    Just projectedTy ->
      Just (mapExpectedTermEndpoint (const projectedTy) endpoint)

-- | Select one downward expectation without allowing list order to assign
-- construction authority.  Exact endpoints are source/packet certificates:
-- they dominate checking-only guidance, and two different exact certificates
-- are an invariant failure.  Checking expectations remain advisory, so when
-- there is no exact certificate the first available checking view is enough.
selectExpectedTermEndpoint
  :: (ElabType -> ElabType -> Bool)
  -> String
  -> [Maybe ExpectedTermEndpoint]
  -> Either ElabError (Maybe ExpectedTermEndpoint)
selectExpectedTermEndpoint typesAgree owner candidates =
  case exactTypes of
    exactTy : remainingExactTypes
      | all (typesAgree exactTy) remainingExactTypes ->
          pure (Just (ExactConstructionExpectedTerm exactTy))
      | otherwise ->
          Left
            ( ValidationFailed
                [ owner ++ " has conflicting exact construction endpoints"
                , "  exact endpoints: " ++ show exactTypes
                ]
            )
    [] ->
      pure
        ( case checkingTypes of
            checkingTy : _ -> Just (CheckingExpectedTerm checkingTy)
            [] -> Nothing
        )
  where
    endpoints = catMaybes candidates
    exactTypes =
      [ ty
      | ExactConstructionExpectedTerm ty <- endpoints
      ]
    checkingTypes =
      [ ty
      | CheckingExpectedTerm ty <- endpoints
      ]

data Env = Env
  { envBindingsByIdentity :: Map.Map EnvBindingIdentityKey EnvBinding,
    envTypeBindings :: Map.Map TypeBinderRef ElabType,
    envConstructionGammaAliases :: IntMap.IntMap TypeBinderRef,
    -- | Exact Gamma bounds refined from a source-mediated application before
    -- its body is constructed.  This is a capability map, not a cache: only
    -- these declarations may override the packet's provisional opened bound
    -- when an application selects its active consumer endpoint.
    envSourceRefinedGammaBounds :: Map.Map TypeBinderRef ElabType,
    -- | Parameter types fixed by the construction that owns an enclosing
    -- RHS.  In particular, a recursive let publishes a contractive @mu@
    -- scheme before its RHS is elaborated; unfolding that scheme fixes the
    -- leading lambda domains.  Carrying those domains by binder identity
    -- lets the lambda be built with the recursive carrier directly, instead
    -- of first assigning a graph placeholder and trying to equate it with
    -- the carrier at an application use site.
    envConstructedLambdaParamTypes :: Map.Map EnvBindingIdentityKey ElabType,
    -- | Every source-to-graph quotient that is active while constructing the
    -- current term.  Compiler-exact routes are a subset of these routes, but
    -- source-expected packet routes are construction-only: they must align
    -- local terms and bounds without claiming authority to republish the
    -- source ABI.
    envConstructionBinderRenames :: [(TypeBinderRef, TypeBinderRef)],
    -- | The accumulated quotient from compiler-exact source binders to the
    -- graph identities that construct the current term.  Packet ownership is
    -- nested, but its identity domain is lexical: descendants must retain all
    -- enclosing routes after 'envActiveSubtermConstruction' advances to the
    -- innermost packet.
    envCompilerExactBinderRenames :: [(TypeBinderRef, TypeBinderRef)],
    envActiveSubtermConstruction :: Maybe PreparedSubtermGeneralization,
    envLocalGammaClosures :: IntMap.IntMap LocalGammaClosure,
    -- | Checked sibling-edge endpoints in the frozen witness domain.  These
    -- certificates let a later Graft consume the exact constructed type
    -- without re-reading a coarser final graph representative.
    envFrozenEndpointCertificates :: FrozenEndpointCertificates,
    -- | Expected endpoint supplied by the enclosing constructor, retaining
    -- whether it is checking-only or exact construction authority.
    envExpectedTermEndpoint :: Maybe ExpectedTermEndpoint,
    -- | When the current term is itself the source of one of Figure 15.3.5's
    -- application edges, retain which edge occurrence consumes it.  An
    -- application in either position must publish its prepared Gamma even
    -- when the resulting forall is vacuous; only an argument occurrence may
    -- publish a bound-matching exact specialization as derived @InstApp@.
    envApplicationSourceOccurrence :: Maybe ApplicationOccurrence
  }

type EnvBindingIdentityKey = ResolvedTermIdentityKey

data ElabOut = ElabOut
  { elabDetailed :: Env -> Either ElabError ElaboratedTerm,
    elabStripped :: Env -> Either ElabError XmlfTerm
  }

-- | Construction-time evidence published by the source constructor that
-- owns a completed local Gamma.  The checked type is captured by that owner
-- before root completion; root generalization may use it to select among its
-- already planned binders, but must not infer new equalities from it.
data OwnerFinalConstruction = OwnerFinalConstruction
  { ofcOwner :: !LocalGammaOwner,
    ofcConstructedType :: !ElabType,
    ofcLocallyEmittedBinderRefs :: ![TypeBinderRef],
    -- | Graph-node provenance for locally emitted binders.  A local
    -- constructor may publish a copied construction identity rather than the
    -- root planner's candidate identity; this route proves which planned
    -- graph occurrence that emitted binder discharges without quotienting the
    -- two identities globally.
    ofcLocalBinderRoutes :: !(IntMap.IntMap TypeBinderRef),
    ofcUsedAmbientBinderRefs :: ![TypeBinderRef],
    -- | Exact provisional ambient declarations completed while checking this
    -- owner's body.  Root planning consumes only these proof-bearing
    -- refinements; it never rediscovers a bound from the final term.
    ofcBodyConsumerBoundRefinements :: ![BodyConsumerBoundRefinementCertificate]
  }
  deriving (Eq, Show)

-- | The exact bound constructed for a compiler-exact source result by the
-- lambda that owns its outgoing Hyp.  Packet preparation necessarily happens
-- before Typ(body) is known and may therefore retain only a provisional
-- opened bound.  Carry the checked construction result forward with its
-- lexical owner, boundary edge, and binder identity so the exact boundary can
-- install the declaration without rediscovering it from the completed term.
data CompilerExactResultBoundCertificate =
  CompilerExactResultBoundCertificate
    { cerbcOwner :: !ResolvedTermIdentityKey,
      cerbcBoundary :: !EdgeId,
      cerbcResultRef :: !TypeBinderRef,
      cerbcBound :: !ElabType
    }
  deriving (Eq, Show)

-- | Internal detailed elaboration result.  The public elaborator keeps its
-- term-only projection; the pipeline consumes the optional owner certificate
-- in the same pass so construction is never repeated merely to recover type
-- metadata.
data ElaboratedTerm = ElaboratedTerm
  { elaboratedTerm :: !XmlfTerm,
    elaboratedOwnerFinalConstruction :: !(Maybe OwnerFinalConstruction),
    elaboratedLocalGammaConstructionCertificates :: ![LocalGammaConstructionCertificate],
    elaboratedCompilerExactResultBoundCertificates :: ![CompilerExactResultBoundCertificate]
  }
  deriving (Eq, Show)

-- | Select the one source-owned result certificate for a nested application.
-- A non-empty local Gamma is the stronger construction and therefore wins
-- when present.  Otherwise the nested application must publish an exact
-- zero-local final construction for the same owner.  The smart constructors
-- reject malformed local spines, wrong owners, and zero-local claims that
-- secretly carry local binders or routes.
selectNestedApplicationResidualCertificate
  :: LocalGammaOwner
  -> ElaboratedTerm
  -> Either ElabError NestedApplicationResidualCertificate
selectNestedApplicationResidualCertificate expectedOwner elaboration =
  case localMatches of
    [certificate] ->
      nestedApplicationResidualFromLocalGamma
        expectedOwner
        certificate
    [] ->
      case elaboratedOwnerFinalConstruction elaboration of
        Just ownerConstruction ->
          nestedApplicationResidualFromZeroLocalConstruction
            expectedOwner
            (ofcOwner ownerConstruction)
            (ofcConstructedType ownerConstruction)
            (ofcLocallyEmittedBinderRefs ownerConstruction)
            (ofcLocalBinderRoutes ownerConstruction)
        Nothing ->
          selectionFailure
            [ "  local Gamma matches: []",
              "  final construction: Nothing"
            ]
    matches ->
      selectionFailure
        [ "  local Gamma matches: " ++ show matches,
          "  final construction: "
            ++ show (elaboratedOwnerFinalConstruction elaboration)
        ]
  where
    localMatches =
      filter
        ( \certificate ->
            lgccOwner certificate == expectedOwner
              && not
                ( null
                    ( localGammaConstructionBinders
                        (lgccConstruction certificate)
                    )
                )
        )
        (elaboratedLocalGammaConstructionCertificates elaboration)

    selectionFailure details =
      Left
        ( ValidationFailed
            ( [ "nested application result route has no unique source construction certificate",
                "  owner: " ++ show expectedOwner
              ]
                ++ details
            )
        )

elabTerm :: ElabOut -> Env -> Either ElabError XmlfTerm
elabTerm output env = elaboratedTerm <$> elabDetailed output env

-- | The complete xMLF computation owned by one source instantiation edge.
-- This is deliberately private: callers consume the source, computation, and
-- destination together, so none of the three can be repaired independently
-- from a later type-check result.
data EdgeComputation = EdgeComputation
  { edgeComputationSource :: !ElabType,
    edgeComputationInstantiation :: !Instantiation,
    edgeComputationTarget :: !ElabType,
    edgeComputationFrozenEndpoint :: !(Maybe FrozenEndpointCertificate)
  }

frozenEndpointTypes :: Env -> IntMap.IntMap ElabType
frozenEndpointTypes =
  frozenEndpointCertificateTypes . envFrozenEndpointCertificates

-- | Keep invariant failures useful even for deeply nested Church encodings.
-- A complete 'show' of one application endpoint can be tens of kilobytes and
-- hide the actual failing computation behind outer validation context.
diagnosticShown :: Show a => a -> String
diagnosticShown value
  | renderedLength <= 2 * retainedLength = rendered
  | otherwise =
      take retainedLength rendered
        ++ "...<"
        ++ show renderedLength
        ++ " chars>..."
        ++ drop (renderedLength - retainedLength) rendered
  where
    rendered = show value
    renderedLength = length rendered
    retainedLength = 320

diagnosticElabType :: ElabType -> String
diagnosticElabType = diagnosticShown

diagnosticOptionalElabType :: Maybe ElabType -> String
diagnosticOptionalElabType =
  maybe "Nothing" (("Just " ++) . diagnosticElabType)

diagnosticExpectedTermEndpoint :: Maybe ExpectedTermEndpoint -> String
diagnosticExpectedTermEndpoint =
  maybe "Nothing" $ \case
    CheckingExpectedTerm ty ->
      "Just CheckingExpectedTerm " ++ diagnosticElabType ty
    ExactConstructionExpectedTerm ty ->
      "Just ExactConstructionExpectedTerm "
        ++ diagnosticElabType ty

publishFrozenEndpoint
  :: Env
  -> Maybe FrozenEndpointCertificate
  -> Either ElabError Env
publishFrozenEndpoint env Nothing = pure env
publishFrozenEndpoint env (Just certificate) =
  pure
    env
      { envFrozenEndpointCertificates =
          publishFrozenEndpointCertificate
            certificate
            (envFrozenEndpointCertificates env)
      }

data EdgeEndpointAuthority
  = ReplayComputedEndpoint
  | ApplicationFunctionEndpoint ElabType
  | ApplicationResultEndpoint ElabType
  | ExactApplicationFunctionEndpoint ElabType ElabType
  | ExactTransportEndpoint ElabType
  deriving (Show)

data FunctionResultEndpointPlan
  = RetainResultUndeterminedReplay
  | SpecializeFunctionResult [ElabType]

-- | Decide from the checked source scheme whether a parent result endpoint can
-- construct this function occurrence.  A result may either determine all
-- relevant source binders, or leave a binder that occurs only in the argument
-- domain for the shared application endpoint to determine.  With neither
-- proof, the parent endpoint is not an 'ApplicationResultEndpoint'; ordinary
-- replay and argument-domain construction must handle the occurrence.
functionResultEndpointPlan
  :: (ElabType -> ElabType -> Bool)
  -> ElabType
  -> ElabType
  -> Maybe FunctionResultEndpointPlan
functionResultEndpointPlan endpointTypesAgree sourceTy resultEndpoint = do
  let sourceScheme = schemeFromType sourceTy
      sourceBinders = map fst (schemeBinderRefs sourceScheme)
      mentions binderRef ty =
        any
          (typeBinderRefsSameIdentity binderRef)
          (freeTypeVarRefsType ty)
  (sourceDomain, sourceResult) <-
    case schemeBody sourceScheme of
      TArrow domainTy codomainTy -> Just (domainTy, codomainTy)
      _ -> Nothing
  if
      any
        ( \binderRef ->
            mentions binderRef sourceDomain
              && not (mentions binderRef sourceResult)
        )
        sourceBinders
    then Just RetainResultUndeterminedReplay
    else
      case sourceBinders of
        []
          | endpointTypesAgree sourceResult resultEndpoint ->
              Just (SpecializeFunctionResult [])
          | otherwise ->
              -- With no quantified identity there is no N/application
              -- computation that can transport between two merely
              -- Church-equivalent recursive presentations.
              Nothing
        _ ->
          do
            arguments <-
              inferInstAppArgsFromSchemeRefsExact
                (schemeBinderRefs sourceScheme)
                sourceResult
                resultEndpoint
            -- The inference helper deliberately returns the longest
            -- determined source-spine prefix.  Result-directed construction
            -- is authoritative only when that prefix consumes the complete
            -- forall spine; otherwise applying it leaves a residual forall
            -- where the application requires an arrow.
            guard (length arguments == length sourceBinders)
            pure (SpecializeFunctionResult arguments)

data ApplicationOccurrence
  = ApplicationFunctionOccurrence
  | ApplicationArgumentOccurrence

-- | The computation that consumes a leading forall at an application
-- argument boundary.  Omega records bounded elimination with the primitive
-- @N@, but Figure 15.3.5 applies the Gamma-closed argument at that explicit
-- bound.  Publish that derived application as @[bound]@; an unbounded forall
-- has no bound argument to publish and retains @N@.
applicationArgumentEliminationFor :: ElabType -> Instantiation
applicationArgumentEliminationFor sourceTy =
  case sourceTy of
    TForallRef _ (Just bound) _ -> InstApp (tyToElab bound)
    _ -> InstElim

normalizeScopedType :: ScopeContext p -> ElabType -> ElabType
normalizeScopedType scopeContext =
  inlineBoundVarsTypeWithContext (scInlineBoundVarsContext scopeContext)

scopedTypesAgree :: ScopeContext p -> ElabType -> ElabType -> Bool
scopedTypesAgree scopeContext left right =
  let left' = normalizeScopedType scopeContext left
      right' = normalizeScopedType scopeContext right
   in alphaEqType left' right' || churchAwareEqType left' right'

-- | Equality at an operational xMLF endpoint.  Unlike the broader semantic
-- comparison above, this deliberately preserves the explicit forall and
-- flexible-bound ABI.  A term of type @forall a. tau@ cannot be passed where
-- @tau@ is required, and a term of type @sigma@ cannot be passed where
-- @alpha >= sigma@ is required, until the edge computation constructs the
-- corresponding type application or Hyp.
scopedEndpointTypesAgree :: ScopeContext p -> ElabType -> ElabType -> Bool
scopedEndpointTypesAgree _scopeContext =
  operationalEndpointTypesAgree

data AlgebraContext (p :: Phase) = AlgebraContext
  { algPresolutionView :: PresolutionView p,
    algTraceConfig :: TraceConfig,
    algCanonical :: NodeId -> NodeId,
    algResolvedLambdaParamNode :: NodeId -> Maybe NodeId,
    algAnnotationContext :: AnnotationContext p,
    algNamedSetReify :: IntSet.IntSet,
    -- | Identity-bearing expected types keyed by the source annotation edge
    -- that owns them.  Result nodes may be identified during solving; source
    -- coercion occurrences may not.
    algAnnotationExpectedTypesByEdge :: IntMap.IntMap ElabType,
    -- | Source types for compiler-owned exact lambda parameters, which have no
    -- annotation edge and therefore retain their node-keyed authority.
    algExactLambdaParamSourceTypes :: IntMap.IntMap NormSrcType,
    algSourceTypeHeadIdentities :: Map.Map String SymbolIdentity,
    algSourceTypeBinderIdentities :: Map.Map String TypeBinderIdentity,
    algSubtermGeneralizations :: SubtermGeneralizations,
    algExactProducerTypes :: IntMap.IntMap ElabType,
    algCompilerExactConstructionRefs :: IntMap.IntMap (IntMap.IntMap TypeBinderRef)
  }

-- | Free Hyp references carried by a Phi computation. References introduced
-- by 'InstUnderRef' are lexical and therefore do not name construction Gamma.
freeInstantiationAbstractionRefs :: Instantiation -> [TypeBinderRef]
freeInstantiationAbstractionRefs inst =
  case inst of
    InstId -> []
    InstApp _ -> []
    InstBot _ -> []
    InstIntro -> []
    InstElim -> []
    InstAbstrRef ref -> [ref]
    InstInside inner -> freeInstantiationAbstractionRefs inner
    InstSeq left right ->
      unionRefs
        (freeInstantiationAbstractionRefs left)
        (freeInstantiationAbstractionRefs right)
    InstUnderRef ref inner ->
      filter
        (not . typeBinderRefsSameIdentity ref)
        (freeInstantiationAbstractionRefs inner)
  where
    unionRefs left right = foldr insertRef right left
    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

-- | Rewrite only free Hyp references through the construction Gamma's
-- identity routes.  A reference introduced by 'InstUnderRef' is lexical to
-- that computation and must not be captured by an outward alias.
canonicalizeFreeInstantiationHypRefs
  :: (TypeBinderRef -> Maybe TypeBinderRef)
  -> Instantiation
  -> Instantiation
canonicalizeFreeInstantiationHypRefs lookupAlias = go []
  where
    go bound inst =
      case inst of
        InstId -> InstId
        InstApp ty -> InstApp ty
        InstBot ty -> InstBot ty
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstAbstrRef ref
          | any (typeBinderRefsSameIdentity ref) bound -> InstAbstrRef ref
          | otherwise -> InstAbstrRef (fromMaybe ref (lookupAlias ref))
        InstUnderRef ref inner ->
          InstUnderRef ref (go (ref : bound) inner)
        InstInside inner -> InstInside (go bound inner)
        InstSeq left right -> InstSeq (go bound left) (go bound right)

-- | Select the prepared construction-Gamma capability for one application
-- occurrence edge.  Owner-selected and direct edge-local planning run
-- independently, so their shared routing metadata is merged only after
-- validating exact identity/bound agreement.  Required binders are retained
-- only when their positive edge certificate names this occurrence.
applicationReplayRequirementsForEdge
  :: EdgeId
  -> GeneralizationRequirements
  -> GeneralizationRequirements
  -> Either ElabError GeneralizationRequirements
applicationReplayRequirementsForEdge edgeId ownerRequirements edgeLocalRequirements = do
  sourceBinderRefs <-
    foldM
      insertSourceBinderRoute
      (grSourceBinderRefs ownerRequirements)
      (IntMap.toList (grSourceBinderRefs edgeLocalRequirements))
  ambientGammaAuthorities <-
    foldM
      insertAmbientGammaAuthority
      (grAmbientGammaAuthorities ownerRequirements)
      (IntMap.toList (grAmbientGammaAuthorities edgeLocalRequirements))
  requiredGammaBinders <-
    foldM
      insertRequiredGammaBinder
      []
      ( filter requirementNamesEdge
          ( grRequiredGammaBinders ownerRequirements
              ++ grRequiredGammaBinders edgeLocalRequirements
          )
      )
  pure
    ownerRequirements
      { grRequiredGammaBinders = requiredGammaBinders,
        grSourceBinderRefs = sourceBinderRefs,
        grAmbientBinderRefs =
          foldr
            insertAmbientBinderRef
            (grAmbientBinderRefs ownerRequirements)
            (grAmbientBinderRefs edgeLocalRequirements),
        grAmbientGammaAuthorities = ambientGammaAuthorities,
        grLocallyClosedGammaNodes =
          IntSet.union
            (grLocallyClosedGammaNodes ownerRequirements)
            (grLocallyClosedGammaNodes edgeLocalRequirements)
      }
  where
    requirementNamesEdge requirement =
      edgeId `elem` rgbEdgeIds requirement

    insertSourceBinderRoute routes (nodeKey, incomingRef) =
      case IntMap.lookup nodeKey routes of
        Nothing -> pure (IntMap.insert nodeKey incomingRef routes)
        Just existingRef
          | typeBinderRefsSameIdentity existingRef incomingRef -> pure routes
          | otherwise ->
              replayRequirementsConflict
                [ "source-binder route disagreement"
                , "  graph node: " ++ show (NodeId nodeKey)
                , "  owner-selected ref: " ++ show existingRef
                , "  edge-local ref: " ++ show incomingRef
                ]

    insertAmbientGammaAuthority authorities (nodeKey, incomingAuthority) =
      case IntMap.lookup nodeKey authorities of
        Nothing -> pure (IntMap.insert nodeKey incomingAuthority authorities)
        Just existingAuthority
          | typeBinderRefsSameIdentity
              (agaExactRef existingAuthority)
              (agaExactRef incomingAuthority)
          , alphaEqType
              (agaBound existingAuthority)
              (agaBound incomingAuthority) ->
              pure authorities
          | otherwise ->
              replayRequirementsConflict
                [ "ambient Gamma authority disagreement"
                , "  graph node: " ++ show (NodeId nodeKey)
                , "  owner-selected authority: " ++ show existingAuthority
                , "  edge-local authority: " ++ show incomingAuthority
                ]

    insertAmbientBinderRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    insertRequiredGammaBinder requirements incoming =
      case find (sameExterior incoming) requirements of
        Nothing -> pure (requirements ++ [incoming])
        Just existing
          | rgbPlacement existing /= rgbPlacement incoming ->
              replayRequirementsConflict
                [ "required Gamma placement disagreement"
                , "  exterior: " ++ show (rgbExteriorNode incoming)
                , "  owner-selected placement: " ++ show (rgbPlacement existing)
                , "  edge-local placement: " ++ show (rgbPlacement incoming)
                ]
          | not
              ( alphaEqType
                  (rgbOperatedType existing)
                  (rgbOperatedType incoming)
              ) ->
              replayRequirementsConflict
                [ "required Gamma bound disagreement"
                , "  exterior: " ++ show (rgbExteriorNode incoming)
                , "  owner-selected bound: " ++ show (rgbOperatedType existing)
                , "  edge-local bound: " ++ show (rgbOperatedType incoming)
                ]
          | otherwise ->
              pure (map (mergeMatchingExterior incoming) requirements)

    sameExterior left right =
      rgbExteriorNode left == rgbExteriorNode right

    mergeMatchingExterior incoming existing
      | sameExterior incoming existing =
          existing
            { rgbEdgeIds =
                foldl
                  appendDistinct
                  (rgbEdgeIds existing)
                  (NonEmpty.toList (rgbEdgeIds incoming)),
              rgbResultRoots =
                foldl
                  appendDistinct
                  (rgbResultRoots existing)
                  (NonEmpty.toList (rgbResultRoots incoming))
            }
      | otherwise = existing

    appendDistinct :: Eq a => NonEmpty a -> a -> NonEmpty a
    appendDistinct values value
      | value `elem` values = values
      | otherwise = values <> NonEmpty.singleton value

    replayRequirementsConflict :: [String] -> Either ElabError a
    replayRequirementsConflict details =
      Left
        ( ValidationFailed
            ( "application replay requirements conflict"
                : ("  edge: " ++ show edgeId)
                : details
            )
        )

mkEdgeComputation ::
  AlgebraContext p ->
  TypeCheck.Env ->
  (IdDetails -> Maybe SchemeInfo) ->
  (TypeBinderRef -> Maybe ElabType) ->
  IntMap.IntMap TypeBinderRef ->
  IntMap.IntMap ElabType ->
  GeneralizationRequirements ->
  String ->
  ApplicationOccurrence ->
  EdgeEndpointAuthority ->
  AnnExpr ->
  InstantiationSite ->
  ElabType ->
  ElabType ->
  Either ElabError EdgeComputation
mkEdgeComputation algebraContext edgeTypeEnv resolvedLookup typeBindingLookup constructionAliases exactFrozenEndpoints replayRequirements owner occurrence endpointAuthority sourceAnn site sourceTy targetTy = do
  replayAuthority <- edgeAuthority
  let frozenSourceTy =
        reifyNodeTypePreferringBound scopeContext sourceNode
      replayAppliesToCheckedSource =
        case frozenSourceTy of
          Right replaySourceTy ->
            exactEndpointTypesAgree replaySourceTy sourceTy
          Left _ -> True
  rawInstUncanonicalized <-
    case replayAuthority of
      ReplayEdgeAuthority
        | replayAppliesToCheckedSource
            || frozenEndpointsCoverEveryGraft
            || strictCheckedRoutesCoverEveryGraft ->
        case
          reifyInstWithFrozenEndpointsFromCheckedSourceInConstructionGamma
            annotationContext
            namedSetReify
            constructionAliases
            replayRequirements
            PhiEndpointShapeAuthority
              { pesaOccurrenceRole =
                  case occurrence of
                    ApplicationFunctionOccurrence ->
                      PhiApplicationFunctionOccurrence
                    ApplicationArgumentOccurrence ->
                      PhiApplicationArgumentOccurrence
              , pesaRequiredEndpointType = targetTy
              }
            resolvedLookup
            exactFrozenEndpoints
            sourceTy
            sourceAnn
            edgeId
        of
          Left err ->
            Left
              ( ValidationFailed
                  [ "application occurrence edge replay failed under its prepared construction Gamma"
                  , "  edge: " ++ show edgeId
                  , "  owner: " ++ owner
                  , "  requirements: " ++ show replayRequirements
                  , "  cause: " ++ show err
                  ]
              )
          Right inst -> pure inst
        | otherwise -> pure InstId
      IdentityEdgeAuthority -> pure InstId
      GraftedEdgeAuthority -> pure InstId
  let rawInst0 =
        canonicalizeFreeInstantiationHypRefs gammaAliasLookup rawInstUncanonicalized
  rawInstScoped <-
    case filter (isNothing . typeBindingLookup) (freeInstantiationAbstractionRefs rawInst0) of
      [] -> pure rawInst0
      unscopedRefs
        | not replayAppliesToCheckedSource ->
            -- The checked occurrence and frozen graph source are different
            -- presentations, while the replay requires a Hyp not owned by
            -- this occurrence's prepared Gamma.  Preserve the checked
            -- occurrence and let its exact application endpoint construct
            -- the transport.
            pure InstId
        | authoritativeTypesAgree sourceTy targetTy -> pure InstId
        | otherwise ->
            failEdge
              [ "edge computation uses Hyp identities outside its prepared Γ",
                "unscoped refs=" ++ show unscopedRefs,
                "source type=" ++ show sourceTy,
                "target type=" ++ show targetTy,
                "raw instantiation=" ++ show rawInst0,
                "artifact=" ++ show edgeArtifact
              ]
  rawInst <-
    sourceSpecializeOutgoingHyp sourceTy rawInstScoped
  let rawApplied = TypeCheck.checkInstantiation edgeTypeEnv sourceTy rawInst
  (inst0, authoritativeTargetTy) <-
    case endpointAuthority of
      ReplayComputedEndpoint ->
        case (replayAuthority, rawApplied) of
          -- A deferred constructor/method can have a more precise checked
          -- source type than the frozen graph placeholder used to build its
          -- witness.  Such a witness is not a computation on the checked
          -- occurrence: retain the occurrence identity here and let the
          -- application construction specialize the other side to this exact
          -- endpoint.  If the frozen and checked sources agree, rejection is
          -- a real witness/Γ invariant failure and must remain visible.
          (ReplayEdgeAuthority, Left rawError) ->
            failEdge
              [ "edge replay is not admissible in its prepared Gamma",
                "source type=" ++ show sourceTy,
                "frozen source type=" ++ show frozenSourceTy,
                "raw instantiation=" ++ show rawInst,
                "typecheck error=" ++ show rawError
              ]
          (_, Right replayTarget) ->
            -- Identity/grafted authority certifies that Ω contributes no
            -- computation.  At the ordinary replay endpoint that means the
            -- occurrence retains its source presentation exactly.
            case replayAuthority of
              IdentityEdgeAuthority -> pure (rawInst, sourceTy)
              GraftedEdgeAuthority -> pure (rawInst, sourceTy)
              ReplayEdgeAuthority -> pure (rawInst, replayTarget)
          (_, Left rawError) ->
            failEdge
              [ "identity/grafted computation failed type checking",
                "source type=" ++ show sourceTy,
                "raw instantiation=" ++ show rawInst,
                "typecheck error=" ++ show rawError
              ]
      ApplicationFunctionEndpoint argumentEndpoint ->
        case rawApplied of
          Left rawError ->
            constructFunctionAtEndpoint (show rawError) argumentEndpoint
          Right appliedTy@(TArrow replayDomain _)
            | exactEndpointTypesAgree replayDomain argumentEndpoint ->
                pure (rawInst, appliedTy)
          Right (TArrow _ replayCodomain) -> do
            let applicationTarget = TArrow argumentEndpoint replayCodomain
            transported <-
              transportEdgeComputation sourceTy applicationTarget
            pure (transported, applicationTarget)
          Right _ -> do
            -- Figure 15.3.5 owns the shared application endpoint
            -- independently of Ω replay.  An identity/grafted graph edge can
            -- therefore still require a source-scheme InstApp to expose its
            -- arrow.  Infer that application from the domain alone; the
            -- specialized source scheme, not a partially reified graph
            -- codomain, owns the resulting codomain.
            constructFunctionAtEndpoint
              "replay did not expose an arrow"
              argumentEndpoint
      ApplicationResultEndpoint resultEndpoint ->
        case rawApplied of
          Right appliedTy@(TArrow _ codomainTy)
            | exactEndpointTypesAgree codomainTy resultEndpoint ->
                -- Ω already translated the occurrence edge through the
                -- source function's result context.  Preserve that exact
                -- paper computation before attempting source-forall
                -- specialization: the latter only sees the checked scheme
                -- spine and cannot reconstruct a deep result operation.  A
                pure (rawInst, appliedTy)
          _ ->
            constructFunctionAtResultEndpoint
              replayAuthority
              frozenSourceTy
              replayAppliesToCheckedSource
              rawInst
              rawApplied
              resultEndpoint
      ExactApplicationFunctionEndpoint argumentEndpoint resultEndpoint -> do
        let applicationTarget = TArrow argumentEndpoint resultEndpoint
        case rawApplied of
          Right appliedTy
            | exactEndpointTypesAgree appliedTy applicationTarget ->
                pure (rawInst, applicationTarget)
          _ -> do
            -- The result certificate is the stronger source-spine
            -- discriminator.  Specialize it first and retain the exact
            -- domain produced by that source computation; the caller then
            -- transports the argument occurrence to the same domain.  Trying
            -- to match the whole provisional graph arrow in one step can
            -- choose source binders from its domain that contradict the
            -- checked result (notably @apply g g@), while a bare graph
            -- argument endpoint cannot determine @a@ in @IO a@ at all.
            case
                constructFunctionAtResultEndpoint
                  replayAuthority
                  frozenSourceTy
                  replayAppliesToCheckedSource
                  rawInst
                  rawApplied
                  resultEndpoint
              of
                Right
                  resultDirected@(
                    _,
                    TArrow resultDirectedDomain resultDirectedCodomain
                    )
                    | exactEndpointTypesAgree
                        resultDirectedCodomain
                        resultEndpoint
                    , resultDirectedDomain /= TBottom ->
                        pure resultDirected
                _ ->
                  case
                      constructFunctionAtEndpoint
                        "exact result did not determine the source function"
                        argumentEndpoint
                    of
                      Right
                        domainDirected@(
                          _,
                          TArrow _ domainDirectedCodomain
                          )
                          | exactEndpointTypesAgree
                              domainDirectedCodomain
                              resultEndpoint ->
                              pure domainDirected
                      _ -> do
                        transported <-
                          transportEdgeComputation sourceTy applicationTarget
                        pure (transported, applicationTarget)
      ExactTransportEndpoint exactEndpoint ->
        case rawApplied of
          Left _ -> do
            transported <-
              transportEdgeComputation sourceTy exactEndpoint
            pure (transported, exactEndpoint)
          Right appliedTy
            | exactEndpointTypesAgree appliedTy exactEndpoint ->
                pure (rawInst, exactEndpoint)
          Right _ -> do
            transported <-
              transportEdgeComputation sourceTy exactEndpoint
            pure (transported, exactEndpoint)
  let inst =
        case occurrence of
          ApplicationArgumentOccurrence ->
            canonicalApplicationArgumentInstantiation sourceTy inst0
          ApplicationFunctionOccurrence -> inst0
  case inst of
    InstId
      | not (authoritativeTypesAgree sourceTy authoritativeTargetTy) ->
          failEdge
            [ "identity computation has unequal endpoints",
              "authority=" ++ show replayAuthority,
              "site source=" ++ show sourceNode,
              "site target=" ++ show targetNode,
              "site=" ++ show site,
              "source node=" ++ show (pvLookupNode presolutionView sourceNode),
              "target node=" ++ show (pvLookupNode presolutionView targetNode),
              "source bound=" ++ show (pvLookupVarBound presolutionView sourceNode),
              "target bound=" ++ show (pvLookupVarBound presolutionView targetNode),
              "source node type=" ++ show (reifyNodeTypePreferringBound scopeContext sourceNode),
              "artifact=" ++ show edgeArtifact,
              "source type=" ++ show sourceTy,
              "target type=" ++ show authoritativeTargetTy
            ]
    _ -> pure ()
  appliedTy <-
    case TypeCheck.checkInstantiation edgeTypeEnv sourceTy inst of
      Left err ->
        failEdge
          [ "edge computation does not apply to its source",
            "authority=" ++ show replayAuthority,
            "site=" ++ show site,
            "artifact=" ++ show edgeArtifact,
            "source type=" ++ show sourceTy,
            "instantiation=" ++ show inst,
            "apply error=" ++ show err
          ]
      Right ty -> pure ty
  if authoritativeTypesAgree appliedTy authoritativeTargetTy
    then
      pure
        EdgeComputation
          { edgeComputationSource = sourceTy,
            edgeComputationInstantiation = inst,
            edgeComputationTarget = authoritativeTargetTy,
            edgeComputationFrozenEndpoint =
              (\traceInfo ->
                FrozenEndpointCertificate
                  { fecProducerEdge = edgeId,
                    fecResultRoot = etResultRoot traceInfo,
                    fecEndpointType = authoritativeTargetTy
                  }
              )
                <$> (edgeArtifactTrace <$> edgeArtifact)
          }
    else
      failEdge
        [ "edge computation result does not equal its destination",
          "authority=" ++ show replayAuthority,
          "site source=" ++ show sourceNode,
          "site target=" ++ show targetNode,
          "artifact=" ++ show edgeArtifact,
          "source type=" ++ show sourceTy,
          "instantiation=" ++ show inst,
          "applied type=" ++ show appliedTy,
          "target type=" ++ show authoritativeTargetTy
        ]
  where
    gammaAliasLookup ref = do
      node <- typeBinderRefNode ref
      IntMap.lookup
        (getNodeId node)
        constructionAliases

    annotationContext = algAnnotationContext algebraContext
    scopeContext = acScopeContext annotationContext
    presolutionView = algPresolutionView algebraContext
    namedSetReify = algNamedSetReify algebraContext
    canonical = algCanonical algebraContext
    edgeId@(EdgeId edgeKey) = instantiationSiteEdgeId site
    sourceNode = canonical (instantiationSiteSource site)
    targetNode = canonical (instantiationSiteTarget site)
    edgeArtifact =
      lookupEdgeArtifact edgeId (acEdgeArtifacts annotationContext)
    identityEdges = acIdentityEdges annotationContext
    graftedEdges = cGraftedEdges (pvConstraint (algPresolutionView algebraContext))
    frozenEndpointsCoverEveryGraft =
      case edgeArtifact of
        Nothing -> False
        Just artifact ->
          let edgeWitness = edgeArtifactWitness artifact
           in case
                [ operated
                  | OpGraft operated _ <-
                      getInstanceOps (ewWitness edgeWitness)
                ]
              of
                [] -> False
                operatedNodes ->
                  all
                    (\operated -> IntMap.member (getNodeId operated) exactFrozenEndpoints)
                    operatedNodes
    strictCheckedRoutesCoverEveryGraft =
      case edgeArtifact of
        Just artifact ->
          let edgeWitness = edgeArtifactWitness artifact
              traceInfo = edgeArtifactTrace artifact
           in etReplayContract traceInfo == ReplayContractStrict
                && case
                    [ sourceBinder
                      | OpGraft _operated sourceBinder <-
                          getInstanceOps (ewWitness edgeWitness)
                    ]
                  of
                    [] -> False
                    sourceBinders ->
                      all
                        (strictCheckedRouteExists traceInfo)
                        sourceBinders
        Nothing -> False
    strictCheckedRouteExists traceInfo sourceBinder =
      case
          ( [ traceSource
              | (traceSource, _argument) <- etBinderArgs traceInfo
              , traceSource == sourceBinder
            ],
            IntMap.lookup sourceKey (acSourceBinderRefs annotationContext)
          )
        of
          ([_], Just sourceRef) ->
            case
                filter
                  (typeBinderRefsSameIdentity sourceRef)
                  checkedSourceBinders
              of
                [_] ->
                  case IntMap.lookup sourceKey (etBinderReplayMap traceInfo) of
                    Just replayTarget ->
                      not (null (etReplayDomainBinders traceInfo))
                        && replayTarget `elem` etReplayDomainBinders traceInfo
                    Nothing -> False
                _ -> False
          _ -> False
      where
        sourceKey = getNodeId sourceBinder
    checkedSourceBinders =
      map fst (schemeBinderRefs (schemeFromType sourceTy))
    dataAuthority
      | IntSet.member edgeKey graftedEdges = GraftedEdgeAuthority
      | IntSet.member edgeKey identityEdges = IdentityEdgeAuthority
      | otherwise = ReplayEdgeAuthority

    edgeAuthority =
      case edgeArtifact of
        Just artifact ->
          validateReplayWitness (edgeArtifactWitness artifact)
        Nothing ->
          case dataAuthority of
            ReplayEdgeAuthority ->
              failEdge ["ordinary edge is missing its replay artifact packet"]
            authority -> pure authority

    validateReplayWitness witness
      | canonical (ewLeft witness) /= sourceNode =
          failEdge
            [ "witness source does not match the construction site",
              "site source=" ++ show sourceNode,
              "witness source=" ++ show (canonical (ewLeft witness))
            ]
      | canonical (ewRight witness) /= targetNode =
          failEdge
            [ "witness destination does not match the construction site",
              "site target=" ++ show targetNode,
              "witness target=" ++ show (canonical (ewRight witness))
            ]
      | otherwise = pure ReplayEdgeAuthority

    edgeContext =
      owner
        ++ " edge "
        ++ show edgeId
        ++ " authority="
        ++ diagnosticEndpointAuthority endpointAuthority
    failEdge :: [String] -> Either ElabError a
    failEdge details =
      Left
        ( PhiInvariantError
            (unlines (edgeContext : details))
        )

    endpointTypesAgree = scopedTypesAgree scopeContext
    exactEndpointTypesAgree = scopedEndpointTypesAgree scopeContext
    authoritativeTypesAgree =
      case endpointAuthority of
        ReplayComputedEndpoint -> endpointTypesAgree
        ApplicationFunctionEndpoint {} -> exactEndpointTypesAgree
        ApplicationResultEndpoint {} -> exactEndpointTypesAgree
        ExactApplicationFunctionEndpoint {} -> exactEndpointTypesAgree
        ExactTransportEndpoint {} -> exactEndpointTypesAgree

    diagnosticEndpointAuthority authority =
      case authority of
        ReplayComputedEndpoint -> "ReplayComputedEndpoint"
        ApplicationFunctionEndpoint argumentEndpoint ->
          "ApplicationFunctionEndpoint "
            ++ diagnosticElabType argumentEndpoint
        ApplicationResultEndpoint resultEndpoint ->
          "ApplicationResultEndpoint "
            ++ diagnosticElabType resultEndpoint
        ExactApplicationFunctionEndpoint argumentEndpoint resultEndpoint ->
          "ExactApplicationFunctionEndpoint "
            ++ diagnosticElabType argumentEndpoint
            ++ " => "
            ++ diagnosticElabType resultEndpoint
        ExactTransportEndpoint exactEndpoint ->
          "ExactTransportEndpoint "
            ++ diagnosticElabType exactEndpoint

    -- Figure 15.3.5 applies the argument occurrence computation to the
    -- Gamma-closed argument term.  When that term starts with a bounded
    -- abstraction, spell the bound-matching application as the derived
    -- @[bound]@ computation.  It is definitionally
    -- @InstSeq (InstInside (InstBot bound)) InstElim@, but retaining
    -- 'InstApp' records that the parent application consumes the child's
    -- explicit bound; function-side bounded elimination remains the thesis
    -- primitive N.
    canonicalApplicationArgumentInstantiation source inst =
      case inst of
        InstElim -> applicationArgumentEliminationFor source
        InstSeq InstElim rest ->
          InstSeq (applicationArgumentEliminationFor source) rest
        _ -> inst

    -- A root RaiseMerge translates to @alpha ; Hyp@.  Replay can recover only
    -- the terminal Hyp when the frozen operated root is a graph occurrence
    -- variable, while the checked source occurrence retains its exact
    -- scheme.  The construction Gamma's bound is the positive
    -- authority for alpha: specialize the checked source to that exact bound
    -- before applying the unchanged Hyp.
    --
    -- This is determined from the typed computation itself.  Only a terminal
    -- free Hyp is eligible; nested/lexically-bound abstractions keep their
    -- original computation, and failure to construct the prefix remains an
    -- edge invariant error rather than a path or syntax fallback.
    sourceSpecializeOutgoingHyp source inst =
      case splitOutgoingHyp inst of
        Nothing -> pure inst
        Just (InstId, hypRef)
          | TVarRef sourceRef <- source
          , typeBinderRefsSameIdentity sourceRef hypRef ->
              -- Hyp(alpha) is the introduction from alpha's bound to alpha.
              -- An edge whose checked source is already that exact alpha has
              -- no introduction to perform.  In particular, an unbounded
              -- construction-Gamma declaration is represented by Bottom in
              -- the type-check environment; attempting to specialize alpha
              -- to that sentinel would turn an identity edge into an
              -- impossible alpha -> Bottom transport.
              pure InstId
        Just (prefix, hypRef) -> do
          prefixTarget <-
            case TypeCheck.checkInstantiation edgeTypeEnv source prefix of
              Right ty -> pure ty
              Left err ->
                failEdge
                  [ "outgoing Hyp prefix does not apply to the checked source"
                  , "source type=" ++ show source
                  , "prefix=" ++ show prefix
                  , "Hyp ref=" ++ show hypRef
                  , "typecheck error=" ++ show err
                  ]
          case typeBindingLookup hypRef of
            Nothing -> pure inst
            Just hypBound
              | exactEndpointTypesAgree prefixTarget hypBound ->
                  pure inst
              | otherwise -> do
                  specialization <-
                    case constructInstantiation prefixTarget hypBound of
                      Right constructed -> pure constructed
                      Left cause ->
                        failEdge
                          [ "outgoing Hyp prefix cannot be specialized to its prepared bound"
                          , "source type=" ++ show source
                          , "raw instantiation=" ++ show inst
                          , "prefix=" ++ show prefix
                          , "prefix target=" ++ show prefixTarget
                          , "Hyp ref=" ++ show hypRef
                          , "Hyp bound=" ++ show hypBound
                          , "cause=" ++ show cause
                          ]
                  pure
                    ( composeInst
                        (composeInst prefix specialization)
                        (InstAbstrRef hypRef)
                    )

    splitOutgoingHyp inst =
      case inst of
        InstAbstrRef ref -> Just (InstId, ref)
        InstSeq prefix suffix -> do
          (suffixPrefix, ref) <- splitOutgoingHyp suffix
          pure (composeInst prefix suffixPrefix, ref)
        _ -> Nothing

    transportEdgeComputation source target =
      constructInstantiation source target

    constructFunctionAtEndpoint replayError argumentEndpoint = do
      transported <-
        constructFunctionInstantiation sourceTy argumentEndpoint
      transportedTy <-
        case TypeCheck.checkInstantiation edgeTypeEnv sourceTy transported of
          Left err ->
            failEdge
              [ "application function transport does not type check",
                "source type=" ++ show sourceTy,
                "raw replay error=" ++ replayError,
                "instantiation=" ++ show transported,
                "typecheck error=" ++ show err
              ]
          Right ty -> pure ty
      pure (transported, transportedTy)

    -- Construct a function occurrence from the result endpoint supplied by
    -- its parent application.  Matching the source arrow codomain first can
    -- determine quantifiers that do not occur in the current argument's
    -- unreduced source presentation.  For example, in @apply g g@ the parent
    -- requires the child @apply g@ at @sigma-id -> sigma-id@; that endpoint
    -- uniquely selects both quantifiers of @apply@ before the first @g@ is
    -- transported to its function domain.
    constructFunctionAtResultEndpoint
      replayAuthority
      frozenSourceTy
      replayAppliesToCheckedSource
      rawInst
      rawApplied
      resultEndpoint = do
      let sourceScheme = schemeFromType sourceTy
      (sourceDomain, sourceResult) <-
        case schemeBody sourceScheme of
          TArrow domainTy codomainTy -> pure (domainTy, codomainTy)
          bodyTy ->
            failEdge
              [ "application function source scheme does not expose an arrow body"
              , "source type=" ++ show sourceTy
              , "scheme body=" ++ show bodyTy
              ]
      case
          functionResultEndpointPlan
            exactEndpointTypesAgree
            sourceTy
            resultEndpoint
        of
        Just RetainResultUndeterminedReplay ->
          -- The result endpoint cannot choose a source binder that occurs
          -- only in the argument domain.  Retain the replayed occurrence so
          -- the caller constructs that binder from the shared argument
          -- endpoint.  Defaulting it to Bottom here would make a result such
          -- as @Foo@ incorrectly specialize @forall a. a -> Foo@ to
          -- @Bottom -> Foo@ before its argument is considered.
          case rawApplied of
            Right appliedTy -> pure (rawInst, appliedTy)
            Left rawError ->
              failEdge
                [ "result-undetermined function replay does not apply"
                , "source type=" ++ show sourceTy
                , "source domain=" ++ show sourceDomain
                , "source result=" ++ show sourceResult
                , "expected result=" ++ show resultEndpoint
                , "raw instantiation=" ++ show rawInst
                , "typecheck error=" ++ show rawError
                ]
        Just (SpecializeFunctionResult arguments) -> do
          (inst, appliedTy) <- applyArguments sourceTy arguments
          case appliedTy of
            TArrow _ codomainTy
              | exactEndpointTypesAgree codomainTy resultEndpoint ->
                  pure (inst, appliedTy)
            _ ->
              failEdge
                [ "result-directed application specialization did not expose the expected arrow"
                , "source type=" ++ show sourceTy
                , "arguments=" ++ show arguments
                , "applied type=" ++ show appliedTy
                , "expected result=" ++ show resultEndpoint
                ]
        Nothing ->
          failEdge
            [ "cannot specialize the source function at the expected application result"
            , "source type=" ++ show sourceTy
            , "source domain=" ++ show sourceDomain
            , "source result=" ++ show sourceResult
            , "expected result=" ++ show resultEndpoint
            , "raw instantiation=" ++ show rawInst
            , "raw application=" ++ show rawApplied
            , "replay authority=" ++ show replayAuthority
            , "frozen source=" ++ show frozenSourceTy
            , "replay applies to checked source="
                ++ show replayAppliesToCheckedSource
            , "artifact=" ++ show edgeArtifact
            ]

    constructInstantiation source target
      | exactEndpointTypesAgree source target = pure InstId
      | TVarRef targetRef <- target,
        Just targetBound <- typeBindingLookup targetRef,
        exactEndpointTypesAgree source targetBound =
          -- Inst-Hyp is the construction owned by the exact application
          -- Gamma: under @targetRef > source@, the argument occurrence is
          -- transported from the published bound to that binder.  In
          -- particular, a bounded result such as @alpha > Bool@ keeps
          -- @alpha@ as the shared Figure 15.3.5 endpoint instead of forcing
          -- the function occurrence back down to @Bool@.
          pure (InstAbstrRef targetRef)
      | TBottom <- source = pure (InstBot target)
      | otherwise = do
          args <-
            case inferArgumentsFromTarget sourceScheme' target of
                Nothing ->
                  failEdge
                    [ "cannot transport the source scheme to the exact application endpoint",
                      "source type=" ++ show source,
                      "target type=" ++ show target,
                      "artifact=" ++ show edgeArtifact,
                      "solved source node=" ++ show (pvLookupNode presolutionView sourceNode),
                      "solved target node=" ++ show (pvLookupNode presolutionView targetNode),
                      "solved source bound=" ++ show (pvLookupVarBound presolutionView sourceNode),
                      "solved target bound=" ++ show (pvLookupVarBound presolutionView targetNode),
                      "solved target binding="
                        ++ show
                          ( IntMap.lookup
                              (nodeRefKey (typeRef targetNode))
                              (cBindParents (pvConstraint presolutionView))
                          ),
                      "source binding path="
                        ++ show
                          ( bindingPathToRootLocal
                              (gaBindParentsBase (scGaParents scopeContext))
                              (typeRef sourceNode)
                          ),
                      "target binding path="
                        ++ show
                          ( bindingPathToRootLocal
                              (gaBindParentsBase (scGaParents scopeContext))
                              (typeRef targetNode)
                          ),
                      "target binding links="
                        ++ show
                          [ (ref, IntMap.lookup (nodeRefKey ref) bindParents)
                          | ref <-
                              either
                                (const [])
                                id
                                ( bindingPathToRootLocal
                                    bindParents
                                    (typeRef targetNode)
                                )
                          ]
                    ]
                Just inferred -> pure inferred
          (inst0, applied0) <- applyArguments source args
          (inst, applied) <-
            eliminateVacuousForallsToEndpoint
              target
              inst0
              applied0
          if exactEndpointTypesAgree applied target
            then pure inst
            else
              failEdge
                [ "application computation transport does not reach the exact endpoint",
                  "source type=" ++ show source,
                  "arguments=" ++ show args,
                  "applied type=" ++ show applied,
                  "target type=" ++ show target
                ]
      where
        sourceScheme' = schemeFromType source
        bindParents = gaBindParentsBase (scGaParents scopeContext)

        inferArgumentsFromTarget sourceScheme endpointTy =
          inferExactTransportArguments
            exactEndpointTypesAgree
            sourceScheme
            endpointTy

        -- Exact transport first chooses every source binder whose identity is
        -- determined by the destination.  Any leading binders left after that
        -- inference are necessarily positional, and may be vacuous.  Figure
        -- 15.3.5 still requires their explicit N computations before the
        -- occurrence reaches its shared application endpoint.  Construct
        -- those steps from the quantified type itself; do not wait for the
        -- completed application to fail type checking and strip them later.
        eliminateVacuousForallsToEndpoint endpoint = go
          where
            go prefix current
              | exactEndpointTypesAgree current endpoint =
                  pure (prefix, current)
              | TForallRef ref _ body <- current
              , not
                  ( any
                      (typeBinderRefsSameIdentity ref)
                      (freeTypeVarRefsType body)
                  ) = do
                  next <-
                    case TypeCheck.checkInstantiation edgeTypeEnv current InstElim of
                      Right ty -> pure ty
                      Left err ->
                        failEdge
                          [ "vacuous application binder elimination does not type check",
                            "current type=" ++ show current,
                            "target type=" ++ show endpoint,
                            "prefix=" ++ show prefix,
                            "typecheck error=" ++ show err
                          ]
                  go (composeInst prefix InstElim) next
              | otherwise = pure (prefix, current)

    constructFunctionInstantiation source argumentEndpoint = do
      case schemeBody sourceScheme' of
        TArrow {} -> pure ()
        bodyTy ->
          failEdge
            [ "application function source scheme does not expose an arrow body",
              "source type=" ++ show source,
              "scheme body=" ++ show bodyTy
            ]
      -- This constructor has exact authority only for the shared argument
      -- endpoint.  Specialize binders determined by that domain first, and
      -- consume a remaining bounded prefix with the paper's N computation.
      -- In particular, a result-only flexible binder must not be specialized
      -- from the provisional graph codomain: its bound is the only source
      -- construction available until an exact result certificate exists.
      (initialArgs, initialInst, initiallyApplied) <-
        constructAtArgumentDomain [] InstId source
      (residualArgs, inst, applied) <-
        case initiallyApplied of
          TArrow {} -> pure ([], initialInst, initiallyApplied)
          _ ->
            case
                inferResidualResultArguments initiallyApplied targetTy
                  <|> inferResidualResultArguments
                    initiallyApplied
                    (normalizeScopedType scopeContext targetTy)
              of
                Just inferredResidualArgs -> do
                  (residualInst, residualApplied) <-
                    applyArguments initiallyApplied inferredResidualArgs
                  pure
                    ( inferredResidualArgs,
                      composeInst initialInst residualInst,
                      residualApplied
                    )
                Nothing -> pure ([], initialInst, initiallyApplied)
      let args = initialArgs ++ residualArgs
      case applied of
        TArrow domainTy _
          | exactEndpointTypesAgree domainTy argumentEndpoint -> pure inst
        _ ->
          failEdge
            [ "application function specialization did not expose the exact argument domain",
              "source type=" ++ show source,
              "arguments=" ++ show args,
              "applied type=" ++ show applied,
              "argument endpoint=" ++ show argumentEndpoint,
              "application target=" ++ show targetTy
            ]
      where
        sourceScheme' = schemeFromType source
        -- Once domain construction reaches an unbounded result-only binder,
        -- the complete application topology may determine it.  For example,
        -- specializing @forall a b. IO a -> (a -> IO b) -> IO b@ at @IO Unit@
        -- can still choose @b@ from an exact residual result.  Bounded
        -- result-only binders have already taken their canonical N step above.
        inferResidualResultArguments partiallyApplied applicationTarget =
          let residualScheme = schemeFromType partiallyApplied
              residualBinders = schemeBinderRefs residualScheme
           in case residualBinders of
                [] -> Nothing
                _ -> do
                  terminalArgs <-
                    inferInstAppArgsFromSchemeRefsExact
                      residualBinders
                      (terminalArrowResult (schemeBody residualScheme))
                      (terminalArrowResult applicationTarget)
                  case applyArguments partiallyApplied terminalArgs of
                    Right (_, appliedResidual)
                      | residualTopologyAgreesExact
                          (normalizeScopedType scopeContext appliedResidual)
                          ( normalizeScopedType
                              scopeContext
                              (residualApplicationTarget applicationTarget)
                          ) ->
                          Just terminalArgs
                    _ -> Nothing
          where
            residualApplicationTarget target =
              case target of
                TArrow _ codomain ->
                  TArrow argumentEndpointForInference codomain
                _ -> target
        terminalArrowResult =
          \case
            TArrow _ codomain -> terminalArrowResult codomain
            result -> result
        -- The caller selected this occurrence endpoint from the application
        -- site's constructed edge computation.  Its binder identity is the
        -- authority used to infer InstApp; normalizing it through a flexible
        -- bound can select a different graph binder and construct a function
        -- domain that no longer equals the argument occurrence.
        argumentEndpointForInference =
          argumentEndpoint

        constructAtArgumentDomain arguments prefix current =
          case current of
            TArrow {} -> pure (arguments, prefix, current)
            _ ->
              let currentScheme = schemeFromType current
               in case schemeBody currentScheme of
                    TArrow currentDomain _
                      | Just inferred@(_ : _) <-
                          inferInstAppArgsFromSchemeRefsExact
                            (schemeBinderRefs currentScheme)
                            currentDomain
                            argumentEndpointForInference -> do
                          (domainInst, domainApplied) <-
                            applyArguments current inferred
                          constructAtArgumentDomain
                            (arguments ++ inferred)
                            (composeInst prefix domainInst)
                            domainApplied
                    _ ->
                      case current of
                        TForallRef _ (Just _) _ -> do
                          next <-
                            case
                                TypeCheck.checkInstantiation
                                  edgeTypeEnv
                                  current
                                  InstElim
                              of
                                Right ty -> pure ty
                                Left err ->
                                  failEdge
                                    [ "application function bounded-prefix elimination does not type check"
                                    , "current type=" ++ show current
                                    , "prefix=" ++ show prefix
                                    , "typecheck error=" ++ show err
                                    ]
                          constructAtArgumentDomain
                            arguments
                            (composeInst prefix InstElim)
                            next
                        _ -> pure (arguments, prefix, current)

    applyArguments source =
      foldM applyArgument (InstId, source)

    applyArgument (prefix, current) argument = do
      step <- leadingArgumentInstantiation current argument
      current' <-
        case TypeCheck.checkInstantiation edgeTypeEnv current step of
          Left err ->
            failEdge
              [ "application transport argument does not apply",
                "current type=" ++ show current,
                "argument=" ++ show argument,
                "step=" ++ show step,
                "apply error=" ++ show err
              ]
          Right ty -> pure ty
      let prefix' =
            case prefix of
              InstId -> step
              _ -> InstSeq prefix step
      pure (prefix', current')

    leadingArgumentInstantiation current argument =
      case current of
        TForallRef _ Nothing _ -> pure (InstApp argument)
        TForallRef _ (Just bound) _
          | let boundTy = tyToElab bound,
            exactEndpointTypesAgree argument boundTy -> pure InstElim
          | otherwise -> do
              inside <- constructInstantiation (tyToElab bound) argument
              pure (InstSeq (InstInside inside) InstElim)
        _ ->
          failEdge
            [ "application transport has no leading quantifier",
              "current type=" ++ show current,
              "argument=" ++ show argument
            ]

data EdgeAuthority
  = ReplayEdgeAuthority
  | IdentityEdgeAuthority
  | GraftedEdgeAuthority
  deriving (Eq, Show)

-- | Reify destination types from the application topology retained by
-- constraint generation.  Its allocation fields own the original
-- @arrow(domain, codomain)@ construction; the prepared fields project those
-- endpoints through redirects, copying, and canonicalization.  Do not recover
-- either relation from the post-normalization base graph, an elaborated term,
-- or a type-checking failure.
applicationDestinationTypes ::
  AlgebraContext p ->
  IntMap.IntMap TypeBinderRef ->
  InstantiationSite ->
  InstantiationSite ->
  NodeId ->
  Either ElabError (ElabType, ElabType)
applicationDestinationTypes algebraContext constructionAliases funSite argSite appNode = do
  namedRoutes <-
    foldM
      insertNamedRoute
      IntMap.empty
      (IntMap.toList constructionAliases)
  let namedSet =
        IntSet.fromList (IntMap.keys namedRoutes)
      namedSchemeInfo =
        schemeInfoFromRefSubst
          (schemeFromType TBottom)
          namedRoutes
      reifyPreparedType node =
        let canonicalKey =
              getNodeId (scCanonical scopeContext node)
         in if IntMap.member canonicalKey namedRoutes
              then
                -- A construction alias is an exact named S' declaration;
                -- retaining its identity is the point of this projection.
                reifyTargetNodeType
                  scopeContext
                  namedSet
                  namedSchemeInfo
                  node
              else
                -- Unnamed graph destinations still follow their solved
                -- lower bound.  Treating every topology node as named would
                -- turn a grafted literal endpoint back into a fresh TVar.
                reifyNodeTypePreferringBound scopeContext node
  (domainAllocated, domainPrepared, codomainPrepared) <-
    case instantiationSiteTargetTopology funSite of
      ArrowInstantiationTarget
        { instantiationArrowAllocatedDomain = domain0,
          instantiationArrowDomain = domain,
          instantiationArrowCodomain = codomain
        } -> pure (domain0, domain, codomain)
      AtomicInstantiationTarget ->
        failTopology ["function edge site does not retain an arrow target topology"]
  -- The retained allocation topology is the construction-time authority.
  -- Normalization may graft a more precise child into the arrow node before
  -- this acyclic base is captured, so re-reading that transformed node here
  -- would compare two different phases and reject a valid application.
  if domainAllocated == argTargetAllocated
    then pure ()
    else
      failTopology
        [ "function arrow domain is not the argument edge allocation target",
          "arrow domain=" ++ show domainAllocated,
          "argument target=" ++ show argTargetAllocated
        ]
  domainTy <- reifyPreparedType domainPrepared
  argumentTy <- reifyPreparedType (instantiationSiteTarget argSite)
  codomainTy <- reifyPreparedType codomainPrepared
  applicationResultTy <- reifyPreparedType appNode
  if scopedTypesAgree scopeContext domainTy argumentTy
    then pure ()
    else
      failTopology
        [ "projected arrow domain does not equal projected argument destination",
          "domain type=" ++ show domainTy,
          "argument type=" ++ show argumentTy
        ]
  if scopedTypesAgree scopeContext codomainTy applicationResultTy
    then pure ()
    else
      failTopology
        [ "projected arrow codomain does not equal projected application result",
          "codomain type=" ++ show codomainTy,
          "application result type=" ++ show applicationResultTy
        ]
  pure (argumentTy, codomainTy)
  where
    scopeContext = acScopeContext (algAnnotationContext algebraContext)
    argTargetAllocated = instantiationSiteAllocatedTarget argSite
    insertNamedRoute routes (nodeKey, outwardRef) =
      let canonicalKey =
            getNodeId
              (scCanonical scopeContext (NodeId nodeKey))
       in case IntMap.lookup canonicalKey routes of
            Nothing -> pure (IntMap.insert canonicalKey outwardRef routes)
            Just existing
              | typeBinderRefsSameIdentity existing outwardRef -> pure routes
              | otherwise ->
                  failTopology
                    [ "construction aliases collapse to different exact S' identities",
                      "graph node=" ++ show (NodeId nodeKey),
                      "canonical node=" ++ show (NodeId canonicalKey),
                      "first identity=" ++ show existing,
                      "second identity=" ++ show outwardRef
                    ]
    failTopology details =
      Left
        ( PhiInvariantError
            (unlines ("AAppF: invalid construction-time topology" : details))
        )

containsMuType :: ElabType -> Bool
containsMuType ty =
  case ty of
    TMuRef {} -> True
    TArrow dom cod -> containsMuType dom || containsMuType cod
    TConWithIdentity _ _ args -> any containsMuType args
    TVarAppRef _ args -> any containsMuType args
    TForallRef _ mb body -> maybe False containsMuBound mb || containsMuType body
    _ -> False
  where
    containsMuBound bound = case bound of
      TArrow dom cod -> containsMuType dom || containsMuType cod
      TConWithIdentity _ _ args -> any containsMuType args
      TVarAppRef _ args -> any containsMuType args
      TForallRef _ mb body -> maybe False containsMuBound mb || containsMuType body
      TMuRef {} -> True
      _ -> False

hasContractiveRecursiveWitness :: ElabType -> Bool
hasContractiveRecursiveWitness ty =
  containsMuType ty && isNothing (firstNonContractiveRecursiveType ty)

isSingleBinderIdentityScheme :: SchemeInfo -> Bool
isSingleBinderIdentityScheme schemeInfo =
  case (schemeBinderRefs (siScheme schemeInfo), schemeBody (siScheme schemeInfo)) of
    ([(binderRef, Nothing)], TArrow (TVarRef domRef) (TVarRef codRef)) ->
      typeBinderRefsSameIdentity binderRef domRef && typeBinderRefsSameIdentity binderRef codRef
    _ -> False

containsInternalTypeVar :: ElabType -> Bool
containsInternalTypeVar = any isInternalTypeBinderRef . freeTypeVarRefsType

{- Note [Internal graph variables are free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A graph-backed 'TypeBinderRef' is not necessarily unresolved: generalized
schemes deliberately retain graph identities for their bound variables.  The
recovery predicates in this module must therefore inspect only free type
variables.  Traversing every 'TVarRef' misclassifies @forall a. a -> a@ as an
internal scheme and repeatedly rebuilds identity wrappers around it.
-}

{- Note [Recursive identity applications use the argument type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An unannotated identity lambda is valid at every monomorphic parameter type.
At an application such as @(\y. y) k@, the checked type of @k@ is therefore
the authoritative parameter type.  Presolution can retain two closed,
non-equivalent mu representations across the application edge even though the
edge itself needs no xMLF instantiation.  Choosing the lambda-domain replay in
that case constructs an ill-typed application.

When the head is syntactically an identity lambda and the argument carries a
contractive recursive type, AApp constructs the lambda binder and its matching
occurrences with the argument type.  This is valid by the identity-lambda rule
and avoids treating bound graph identities as unresolved-variable evidence.
-}

isInternalTypeBinderRef :: TypeBinderRef -> Bool
isInternalTypeBinderRef ref =
  isJust (typeBinderRefNode ref)

mkEnvBinding :: VarName -> IdDetails -> SchemeInfo -> Maybe TransparentMediatorKind -> EnvBinding
mkEnvBinding name details schemeInfo transparentMediator =
  EnvBinding
    { ebSchemeInfo = schemeInfo,
      ebSchemeType = schemeToType (siScheme schemeInfo),
      ebIdentityDetails = details,
      ebRuntimeName = name,
      ebTransparentMediator = transparentMediator,
      ebAliasTarget = Nothing,
      ebExplicitRecursiveParam = False
    }

mkEnvValueBinding :: VarName -> EnvRef -> SchemeInfo -> Maybe TransparentMediatorKind -> EnvBinding
mkEnvValueBinding name envRef =
  mkEnvBinding name (EnvId envRef)

resolvedTermBinderDetails :: VarName -> IdDetails -> IdDetails
resolvedTermBinderDetails runtimeAlias =
  idDetailsRenameLocal runtimeAlias

mkLocalEnvBinding :: VarName -> IdDetails -> SchemeInfo -> Maybe TransparentMediatorKind -> EnvBinding
mkLocalEnvBinding = mkEnvBinding

localBinderIsDiscard :: IdDetails -> Bool
localBinderIsDiscard = idDetailsIsDiscard

resolvedLocalBinder :: IdDetails -> ElabType -> ResolvedVar
resolvedLocalBinder details ty =
  ResolvedVar
    { resolvedVarType = ty,
      resolvedVarDetails = details
    }

mkLocalLam :: IdDetails -> ElabType -> XmlfTerm -> XmlfTerm
mkLocalLam details ty =
  ELam (resolvedLocalBinder details ty)

mkLocalLet :: IdDetails -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
mkLocalLet details scheme =
  ELet (resolvedLocalBinder details (schemeToType scheme)) scheme

mkEnv :: Map.Map VarName SchemeInfo -> Env
mkEnv schemeInfos =
  mkEnvFromBindings (map snd bindings)
  where
    (_, bindings) =
      mapAccumL mkBinding initialGenerator (Map.toList schemeInfos)

    initialGenerator =
      identityGeneratorAfter $
        concatMap (generatedIdentitiesInType . schemeToType . siScheme) (Map.elems schemeInfos)

    mkBinding generator (name, schemeInfo) =
      let (envRef, generator') = freshEnvRef name generator
       in (generator', (name, mkEnvValueBinding name envRef schemeInfo Nothing))

mkEnvWithResolvedBindings :: Map.Map VarName (SchemeInfo, ResolvedVar) -> Env
mkEnvWithResolvedBindings schemeInfos =
  mkEnvFromBindings (Map.elems bindings)
  where
    bindings =
      Map.map
        ( \(schemeInfo, resolved) ->
            mkEnvBinding (resolvedVarRuntimeName resolved) (resolvedVarDetails resolved) schemeInfo Nothing
        )
        schemeInfos

mkEnvFromBindings :: [EnvBinding] -> Env
mkEnvFromBindings bindings =
  Env
    { envBindingsByIdentity = envBindingIdentityIndex bindings
    , envTypeBindings = Map.empty
    , envConstructionGammaAliases = IntMap.empty
    , envSourceRefinedGammaBounds = Map.empty
    , envConstructedLambdaParamTypes = Map.empty
    , envConstructionBinderRenames = []
    , envCompilerExactBinderRenames = []
    , envActiveSubtermConstruction = Nothing
    , envLocalGammaClosures = IntMap.empty
    , envFrozenEndpointCertificates = IntMap.empty
    , envExpectedTermEndpoint = Nothing
    , envApplicationSourceOccurrence = Nothing
    }

envBindingIdentityIndex :: [EnvBinding] -> Map.Map EnvBindingIdentityKey EnvBinding
envBindingIdentityIndex bindings =
  Map.fromList
    [ (envBindingIdentityKey binding, binding)
    | binding <- bindings
    ]

envGeneratedIdentities :: Env -> [UniqueIdentity]
envGeneratedIdentities env =
  concatMap envBindingGeneratedIdentities (authoritativeEnvBindings env)
  where
    envBindingGeneratedIdentities binding =
      idDetailsGeneratedIdentities (ebIdentityDetails binding)
        ++ generatedIdentitiesInType (ebSchemeType binding)

typeCheckEnvFrom :: Env -> TypeCheck.Env
typeCheckEnvFrom env =
  TypeCheck.mkTypeCheckEnvWithResolvedTerms
    [ (resolvedEnvBindingVar binding, ebSchemeType binding)
    | binding <- authoritativeEnvBindings env
    ]
    (envTypeBindings env)

-- | Project only the resolved-term lookup payloads used by a boundary-local
-- xMLF check.  An occurrence route is not a new construction quotient, so
-- leave Gamma binders, aliases, and accumulated construction routes
-- untouched.  The caller supplies the same ordered composition used for the
-- term: graph occurrence to source, followed by any exact owner construction.
projectResolvedOccurrenceLookupTypes
  :: [(TypeBinderRef, TypeBinderRef)]
  -> Env
  -> Env
projectResolvedOccurrenceLookupTypes requestedRenames env =
  env
    { envBindingsByIdentity =
        Map.map projectBinding (envBindingsByIdentity env)
    }
  where
    projectBinding binding =
      binding
        { ebSchemeType =
            applyTypeVarRefRenames
              requestedRenames
              (ebSchemeType binding)
        }

extendEnvTypeScope :: [(TypeBinderRef, Maybe BoundType)] -> Env -> Env
extendEnvTypeScope binders env =
  env
    { envTypeBindings =
        foldl insertBinding (envTypeBindings env) binders
    }
  where
    constructionRenames = envConstructionBinderRenames env

    insertBinding bindings (ref, mbBound) =
      let ref' = applyRefRenames constructionRenames ref
          bound' =
            applyTypeVarRefRenames constructionRenames
              (maybe TBottom tyToElab mbBound)
       in Map.insert ref' bound' $
            Map.filterWithKey
              (\existing _ -> not (typeBinderRefsSameIdentity existing ref'))
              bindings

-- | Extend the construction environment with both the outward scheme
-- binders and every graph-local identity that the scheme substitution maps
-- to those binders.  Omega computations are constructed before the final
-- term-wide substitution is applied, so their Hyp nodes still mention the
-- graph identity even when the published scheme already prefers a source or
-- generated identity.  Both names denote one prepared Gamma obligation; the
-- alias bridge is construction-local and disappears when the term is
-- substituted and closed.
extendEnvTypeScopeWithAliases
  :: IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Env
  -> Env
extendEnvTypeScopeWithAliases subst binders env =
  envWithAliases
  where
    constructionSubst =
      IntMap.map
        (applyRefRenames (envConstructionBinderRenames env))
        subst
    envWithBinders = extendEnvTypeScope binders env
    bindingsWithBinders = envTypeBindings envWithBinders
    graphAliases =
      [ (graphRef, outwardType)
      | (nodeKey, outwardRef) <- IntMap.toList constructionSubst,
        Just outwardType <-
          [find
            (typeBinderRefsSameIdentity outwardRef . fst)
            (Map.toList bindingsWithBinders)
            >>= (pure . snd)],
        let graphRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId nodeKey))
                ("t" ++ show nodeKey),
        not (typeBinderRefsSameIdentity graphRef outwardRef)
      ]
    envWithAliases =
      envWithBinders
        { envTypeBindings =
            foldl'
              insertAlias
              bindingsWithBinders
              graphAliases
        , envConstructionGammaAliases =
            IntMap.union
              constructionSubst
              (envConstructionGammaAliases envWithBinders)
        }

    insertAlias bindings (graphRef, outwardType) =
      Map.insert graphRef outwardType $
        Map.filterWithKey
          (\existing _ -> not (typeBinderRefsSameIdentity existing graphRef))
          bindings

-- | Project only construction-authoritative scheme routes into the term
-- environment.  A scheme substitution can retain graph routes used solely to
-- replay Omega, so its mere presence is not permission to change a captured
-- term binding.  A route becomes construction authority exactly when its
-- outward ref is declared by this scheme or is already an exact ambient Gamma
-- binder.  Entering those routes before recursively constructing the body
-- keeps captured variable types in the same identity domain as the scheme that
-- will close the lambda.
schemeConstructionAuthorityRenames
  :: IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> [(TypeBinderRef, TypeBinderRef)]
schemeConstructionAuthorityRenames directSourceBinderRefs env schemeInfo =
  [ (graphRef, outwardRef)
  | (nodeKey, outwardRef) <-
      IntMap.toList (schemeInfoBinderRefSubst schemeInfo)
  , let graphRef =
          typeBinderRefFromIdentity
            (typeBinderIdentityFromNode (NodeId nodeKey))
            ("t" ++ show nodeKey)
  , not (typeBinderRefsSameIdentity graphRef outwardRef)
  , any
      (typeBinderRefsSameIdentity outwardRef)
      declaredRefs
      || envOwnsExactTypeBinderRef env outwardRef
      || isJust
        ( directSourceBinderConstructionRename
            directSourceBinderRefs
            nodeKey
            outwardRef
        )
  , routeBoundCompatible graphRef outwardRef
  ]
  where
    declaredBinders = schemeBinderRefs (siScheme schemeInfo)
    declaredRefs = map fst declaredBinders

    routeBoundCompatible graphRef outwardRef =
      constructionRouteBoundCompatible
        (envConstructionBinderRenames env)
        graphRef
        outwardRef
        (lookupCurrentBound graphRef)
        (lookupOutwardBound outwardRef)

    lookupCurrentBound ref =
      let currentRef =
            applyRefRenames
              (envConstructionBinderRenames env)
              ref
       in snd
            <$> find
              (typeBinderRefsSameIdentity currentRef . fst)
              (Map.toList (envTypeBindings env))

    lookupOutwardBound outwardRef =
      case
          find
            (typeBinderRefsSameIdentity outwardRef . fst)
            declaredBinders
        of
          Just (_, mbBound) ->
            Just (maybe TBottom tyToElab mbBound)
          Nothing ->
            snd
              <$> find
                (typeBinderRefsSameIdentity outwardRef . fst)
                (Map.toList (envTypeBindings env))

-- | Install the exact nested-construction proofs prepared for this root.
-- They are consulted only by the matching edge/owner/consumer closure check;
-- no binder or alias is manufactured from this map.
withEnvLocalGammaClosures
  :: IntMap.IntMap LocalGammaClosure
  -> Env
  -> Env
withEnvLocalGammaClosures closures env =
  env {envLocalGammaClosures = closures}

-- | Install the construction domain owned by one compiler-exact edge.  Most
-- delayed packet results are abstracted by 'completeCompilerExactSubtermResults'.
-- An enclosing-owned marker has no completion action, but that alone does not
-- make its packet-local result an ambient binder: a lambda/application below
-- this exact boundary may still own the corresponding type abstraction.
--
-- Bridge the packet construction identity only when its prepared consumer is
-- already present in the explicit ambient type scope.  The consumer authority
-- and packet substitution select the identities; the prepared complete Gamma
-- bound supplies their type.  Thus an exact edge can refine a real lexical
-- binder without pre-opening a result that its child must construct locally.
-- Absence of a prepared result bound remains an error rather than an
-- invitation to manufacture a bottom-bound scope.
--
-- The resulting environment remains edge-local, so sibling exact occurrences
-- cannot observe either these routes or these type-scope bindings.
installCompilerExactConstructionRefs
  :: EdgeId
  -> SubtermGeneralizations
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> Either ElabError Env
installCompilerExactConstructionRefs edgeId packets edgeRefs env = do
  aliases <-
    foldM
      insertRoute
      (envConstructionGammaAliases env)
      (IntMap.toList edgeRefs)
  enclosingBinders <-
    fmap catMaybes
      ( traverse
          preparedEnclosingBinder
          [ (owner, packet)
          | (owner, packet) <- Map.toList packets
          , subtermGeneralizationCompilerExactBoundary packet == Just edgeId
          ]
      )
  foldM
    installPreparedBinder
    env {envConstructionGammaAliases = aliases}
    enclosingBinders
  where
    insertRoute aliases (nodeKey, outwardRef) =
      case IntMap.lookup nodeKey aliases of
        Nothing -> pure (IntMap.insert nodeKey outwardRef aliases)
        Just existingRef
          | typeBinderRefsSameIdentity existingRef outwardRef -> pure aliases
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "compiler exact edge conflicts with the enclosing construction route"
                    , "  edge: " ++ show edgeId
                    , "  graph node: " ++ show (NodeId nodeKey)
                    , "  enclosing identity: " ++ show existingRef
                    , "  exact identity: " ++ show outwardRef
                    ]
                )

    preparedEnclosingBinder (owner, packet) =
      case
          ( subtermGeneralizationCompilerExactResultRef packet
          , subtermGeneralizationCompilerExactCompletionRef packet
          )
        of
          (Just packetResultRef, Nothing) -> do
            validatePacketResultBound owner packetResultRef packet
            consumerAuthority <-
              case subtermGeneralizationConsumerAuthority packet of
                Just authority -> pure authority
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact enclosing result has no consumer authority"
                        , "  edge: " ++ show edgeId
                        , "  owner: " ++ show owner
                        , "  result: " ++ show packetResultRef
                        ]
                    )
            consumerRef <-
              case subtermGeneralizationResultAbstractionRef packet of
                Just ref -> pure ref
                Nothing ->
                  missingConsumerRoute owner packetResultRef
                    "has no semantic consumer reference"
            constructionRef <-
              case subtermGeneralizationConstructionResultAbstractionRef packet of
                Just ref -> pure ref
                Nothing ->
                  missingConsumerRoute owner packetResultRef
                    "has no construction consumer reference"
            unless
              ( typeBinderRefIdentity consumerRef
                  == scaConsumerIdentity consumerAuthority
              )
              ( Left
                  ( ValidationFailed
                      [ "compiler exact enclosing result changed consumer identity"
                      , "  edge: " ++ show edgeId
                      , "  owner: " ++ show owner
                      , "  result: " ++ show packetResultRef
                      , "  authority consumer: "
                          ++ show (scaConsumerIdentity consumerAuthority)
                      , "  semantic consumer: " ++ show consumerRef
                      ]
                  )
              )
            let consumerDomainRefs =
                  uniqueRefs
                    ( consumerRef
                        : constructionRef
                        : routedRefsFor consumerRef
                          ++ routedRefsFor constructionRef
                    )
                ambientBindings =
                  [ binding
                  | binding@(existingRef, _) <-
                      Map.toList (envTypeBindings env)
                  , any
                      (typeBinderRefsSameIdentity existingRef)
                      consumerDomainRefs
                  , envOwnsExactTypeBinderRef env existingRef
                  ]
            case ambientBindings of
              [] -> pure Nothing
              _ -> do
                let preparedBound =
                      schemeToType
                        (subtermGeneralizationGammaBoundScheme packet)
                mapM_
                  ( validateExisting
                      owner
                      packetResultRef
                      constructionRef
                      preparedBound
                  )
                  ambientBindings
                pure
                  ( Just
                      ( owner
                      , packetResultRef
                      , consumerRef
                      , constructionRef
                      , preparedBound
                      )
                  )
          (Just _, Just _) -> pure Nothing
          (Nothing, _) ->
            Left
              ( ValidationFailed
                  [ "compiler exact packet has no prepared result binder"
                  , "  edge: " ++ show edgeId
                  , "  owner: " ++ show owner
                  ]
              )

    validatePacketResultBound owner resultRef packet =
      case
          [ mbBound
          | (binderRef, mbBound) <-
              schemeBinderRefs
                (siScheme (subtermGeneralizationSchemeInfo packet))
          , typeBinderRefsSameIdentity binderRef resultRef
          ]
        of
          [Just _] -> pure ()
          [Nothing] ->
            missingPreparedBound owner resultRef
              "has no operated bound"
          [] ->
            missingPreparedBound owner resultRef
              "is absent from its prepared packet"
          _ ->
            missingPreparedBound owner resultRef
              "occurs more than once in its prepared packet"

    missingPreparedBound owner resultRef reason =
      Left
        ( ValidationFailed
            [ "compiler exact enclosing result " ++ reason
            , "  edge: " ++ show edgeId
            , "  owner: " ++ show owner
            , "  result: " ++ show resultRef
            ]
        )

    missingConsumerRoute owner resultRef reason =
      Left
        ( ValidationFailed
            [ "compiler exact enclosing result " ++ reason
            , "  edge: " ++ show edgeId
            , "  owner: " ++ show owner
            , "  result: " ++ show resultRef
            ]
        )

    routedRefsFor ref =
      case typeBinderRefNode ref of
        Nothing -> []
        Just node ->
          maybeToList
            ( IntMap.lookup
                (getNodeId node)
                (envConstructionGammaAliases env)
            )
            ++ maybeToList
              (IntMap.lookup (getNodeId node) edgeRefs)

    uniqueRefs = foldr insertUniqueRef []

    insertUniqueRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    installPreparedBinder
      scopedEnv
      (owner, packetResultRef, consumerRef, constructionRef, preparedBound) =
      foldM
        (installOne owner packetResultRef preparedBound)
        scopedEnv
        installRefs
      where
        packetConstructionRef =
          [ packetResultRef
          | any
              (typeBinderRefsSameIdentity packetResultRef)
              [consumerRef, constructionRef]
          ]
        constructionDomainRefs =
          uniqueRefs
            ( consumerRef
                : constructionRef
                : packetConstructionRef
                  ++ routedRefsFor consumerRef
                  ++ routedRefsFor constructionRef
            )
        installRefs =
          uniqueRefs
            ( constructionDomainRefs
                ++ concatMap graphAliasesFor constructionDomainRefs
            )

    graphAliasesFor resultRef =
      [ graphRef
      | (nodeKey, outwardRef) <- IntMap.toList edgeRefs
      , typeBinderRefsSameIdentity outwardRef resultRef
      , let graphRef =
              typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId nodeKey))
                ("t" ++ show nodeKey)
      , not (typeBinderRefsSameIdentity graphRef resultRef)
      ]

    installOne owner resultRef preparedBound scopedEnv ref = do
      mapM_ (validateExisting owner resultRef ref preparedBound) sameIdentityBindings
      pure
        scopedEnv
          { envTypeBindings =
              Map.insert ref preparedBound $
                Map.filterWithKey
                  (\existing _ -> not (typeBinderRefsSameIdentity existing ref))
                  (envTypeBindings scopedEnv)
          }
      where
        sameIdentityBindings =
          [ (existingRef, existingBound)
          | (existingRef, existingBound) <- Map.toList (envTypeBindings scopedEnv)
          , typeBinderRefsSameIdentity existingRef ref
          ]

    validateExisting owner resultRef installedRef preparedBound (existingRef, existingBound)
      | existingBound == TBottom = pure ()
      | alphaEqType existingBound preparedBound
          || churchAwareEqType existingBound preparedBound = pure ()
      | otherwise =
          Left
            ( ValidationFailed
                [ "compiler exact enclosing result conflicts with its prepared Gamma bound"
                , "  edge: " ++ show edgeId
                , "  owner: " ++ show owner
                , "  result: " ++ show resultRef
                , "  installed identity: " ++ show installedRef
                , "  existing identity: " ++ show existingRef
                , "  existing bound: " ++ show existingBound
                , "  prepared bound: " ++ show preparedBound
                ]
            )

-- | Exact graph-node routes for binders already present in a construction
-- environment.  Explicit aliases win, while a graph-backed type-scope binder
-- supplies its own identity route when no alias was necessary.
envConstructionIdentityRoutes :: Env -> IntMap.IntMap TypeBinderRef
envConstructionIdentityRoutes env =
  IntMap.union
    (envConstructionGammaAliases env)
    ( IntMap.fromList
        [ (getNodeId node, ref)
        | ref <- Map.keys (envTypeBindings env),
          Just node <- [typeBinderRefNode ref]
        ]
    )

-- | Enter the parameter boundary chosen by construction.  Exact source
-- evidence parameters own their complete type and therefore remove a graph
-- root alias instead of treating that root as one type-variable occurrence.
-- Ordinary constructed parameters retain the historical Gamma refinement.
installConstructedLambdaParamBound
  :: NodeId
  -> LambdaParamBoundaryAuthority
  -> Env
  -> Either ElabError (ElabType, Set.Set TypeBinderIdentity, Env)
installConstructedLambdaParamBound paramNode authority env = do
  installation <-
    installLambdaParamBoundary
      paramNode
      authority
      (envConstructionGammaAliases env)
      (envTypeBindings env)
  pure
    ( lambdaParamBoundaryType installation,
      lambdaParamBoundaryLocalBinderIdentities installation,
      env
        { envConstructionGammaAliases =
            lambdaParamBoundaryGammaAliases installation
        , envTypeBindings =
            lambdaParamBoundaryTypeBindings installation
        }
    )

-- | Publish source-owned construction aliases at both their concrete graph
-- node and solved representative.  Descendant Γ preparation can reify a
-- sibling copy of the same lexical binder, so the representative route must
-- exist before its operated bound is built.  A representative that already
-- belongs to a distinct lexical source binder is deliberately left ambiguous;
-- conflicting concrete routes are still rejected.
constructionSourceBinderRefs
  :: (NodeId -> NodeId)
  -> Env
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
constructionSourceBinderRefs representative env sourceRefs = do
  let constructionSourceRefs =
        IntMap.map
          (applyRefRenames (envConstructionBinderRenames env))
          sourceRefs
  mergeConstructionSourceBinderRefs
    representative
    constructionSourceRefs
    (envConstructionGammaAliases env)

-- | Freeze only direct construction aliases whose outward identities have an
-- exact type binding in the current environment.  This is the authority used
-- before source-ABI alignment: graph-local inherited Gamma routes are not
-- source binder aliases and must remain in their own certificate channel.
ambientGammaAuthoritiesForEnv
  :: Env
  -> Either ElabError (IntMap.IntMap AmbientGammaAuthority)
ambientGammaAuthoritiesForEnv env =
  buildAmbientGammaAuthorities
    (envConstructionGammaAliases env)
    (envTypeBindings env)

-- | Select the complete owner-local Γ constructed for one Figure 15.3.5
-- boundary.  Ordinary binders produced by generalizing at that exact owner and
-- explicit root-RaiseMerge binders are one scope: both must be in scope before
-- the child computations are constructed.  The scheme has already been
-- generalized against the outer environment, so every remaining binder is
-- local to this boundary.  Bound dependencies follow it in scheme order.
constructionGammaBinders
  :: ConstructionGammaCoverage
  -> ScopeContext p
  -> LocalGammaOwner
  -> Maybe PendingLocalResultSourcePacket
  -> Maybe SchemeInfo
  -> Env
  -> GeneralizationRequirements
  -> SchemeInfo
  -> Either ElabError ConstructionGammaPlan
constructionGammaBinders coverage scopeContext owner pendingResultSourcePacket ownerEmissionScheme0 env requirements0 schemeInfo0 = do
  constructionIdentityRoutes <- boundaryConstructionIdentityRoutes
  requirementBoundRenames <-
    either
      (Left . ValidationFailed . pure)
      Right
      ( sourceBinderConstructionRenames
          (scopeTypeBinderIdentityRepresentative scopeContext)
          relevantRequirementSourceBinderRefs
          constructionIdentityRoutes
      )
  requiredRefs <-
    traverse
      (requirementRefForOwner requirementBoundRenames)
      requiredBinders
  let EnvFreeTypeBinderRefs environmentRefList =
        envFreeTypeBinderRefs env
  ownerConstructionPlans <-
    traverse
      (localClosureBindersFor requirementBoundRenames)
      requiredBinders
  ambientCandidateAuthorities <-
    traverse
      (uncurry (ambientBoundFor (const True)))
      (zip requiredBinders requiredRefs)
  let ownerConstructionBinders = concatMap snd ownerConstructionPlans
      ownerLocalRequirementRefs =
        [ ref
        | ( (ownedByBoundary, _),
            (requirement, (ref, mbAmbientAuthority))
            ) <- zip
              ownerConstructionPlans
              (zip requiredBinders (zip requiredRefs ambientCandidateAuthorities)),
          requirementNeedsLocalConstruction
            ownedByBoundary
            (rgbPlacement requirement)
            ( maybe
                False
                ( \authority ->
                    requiredBoundSatisfiedBy
                      ref
                      ( expectedBoundForRef
                          requirementBoundRenames
                          requirement
                          ref
                      )
                      (agaBound authority)
                )
                mbAmbientAuthority
            )
        ]
  let schemeConstructionBinders =
        [ ( ref
          , fmap
              (mapBoundType (applyBinderBoundRenames ref requirementBoundRenames))
              mbBound
          )
        | (ref, mbBound) <- schemeBinders
        ]
  -- Every binder explicitly declared by this exact construction boundary
  -- shadows an ambient ref in the same solved graph class.  Representative
  -- equality is valid only after excluding those owner-local declarations;
  -- otherwise their exact S(operated) bounds would be replaced by TBottom.
  -- An ordinary local-Gamma closure has no prebuilt owner scheme, but it is
  -- still the proof that this boundary owns the requirement.  Its ref shadows
  -- a provisional enclosing packet slot only when that slot does not already
  -- satisfy S(operated); an already-constructed ambient bound remains owned by
  -- its enclosing Gamma and must not be abstracted a second time.
  let localConstructionRefs =
        ownerLocalRequirementRefs
          ++ map fst ownerConstructionBinders
          ++ map fst schemeConstructionBinders
      refAlreadyInGamma ref =
        constructionRefAlreadyInGamma
          (scopeTypeBinderIdentityRepresentative scopeContext)
          localConstructionRefs
          environmentRefList
          ref
          || ( not
                 ( any
                     (typeBinderRefsSameIdentity ref)
                     localConstructionRefs
                 )
                 && any
                   ( \(requiredRef, mbAuthority) ->
                       typeBinderRefsSameIdentity ref requiredRef
                         && isJust mbAuthority
                   )
                   (zip requiredRefs ambientCandidateAuthorities)
             )
  requirementConstructionBinders <-
    concat
      <$> traverse
        (uncurry (requirementConstructionBinder requirementBoundRenames refAlreadyInGamma))
        (zip requiredBinders requiredRefs)
  let declaredConstructionBinders =
        ownerConstructionBinders
          ++ schemeConstructionBinders
          ++ requirementConstructionBinders
      undeclaredDependencyRefs =
        foldr insertDistinctRef []
          [ dependency
          | (_, Just bound) <- declaredConstructionBinders
          , dependency <- freeTypeVarRefsType (tyToElab bound)
          , not (refAlreadyInGamma dependency)
          , not
              ( any
                  (typeBinderRefsSameIdentity dependency . fst)
                  declaredConstructionBinders
              )
          ]
      dependencyBinders =
        [ (dependency, Nothing)
        | dependency <- undeclaredDependencyRefs
        , dependencyHasUnboundedBinderAuthority dependency
        ]
  candidateBinders <-
    case
        mergeConstructionBinderBoundsByProvenance
          ("application construction Gamma " ++ show owner)
          -- The generalized scheme has already placed descendant packets and
          -- allocated copied bound identities.  A requirement binder is the
          -- construction fallback for a quotient with no syntactic binder; it
          -- is exact-endpoint evidence, not authority to replace that prepared
          -- local declaration with the graph's raw operated type.
          ( map
              (\binder -> (ConstructionLocalGammaBound, binder))
              ( dependencyBinders
                  ++ ownerConstructionBinders
                  ++ schemeConstructionBinders
              )
              ++ map
                (\binder -> (ConstructionExactEndpoint, binder))
                requirementConstructionBinders
          )
      of
        Right binders -> pure binders
        Left cause ->
          Left
            ( ValidationFailed
                [ "construction Gamma binder sources cannot be merged"
                , "  owner-construction binders: " ++ show ownerConstructionBinders
                , "  scheme-construction binders: " ++ show schemeConstructionBinders
                , "  requirement-construction binders: " ++ show requirementConstructionBinders
                , "  cause: " ++ show cause
                ]
            )
  ambientRequiredAuthorities <-
    traverse
      (uncurry (ambientBoundFor refAlreadyInGamma))
      (zip requiredBinders requiredRefs)
  let ordinaryOwnerRefs =
        case coverage of
          RequiredGammaOnly -> []
          OwnerLocalAndRequiredGamma ->
            [ ref
            | (ref, _) <- schemeBinders,
              binderOwnedByBoundary ref
            ]
          -- An active consumer packet carries the completed type expected of
          -- this exact source boundary.  Once that certificate has selected
          -- the application, every leading binder left in its aligned scheme
          -- is precisely the local Gamma_g prefix from Figure 15.3.5.
          CompleteSchemeAndRequiredGamma ->
            map fst schemeBinders
      localRoots =
        [ ref
        | ref <- requiredRefs ++ ordinaryOwnerRefs,
          not (refAlreadyInGamma ref)
        ]
      localRefs = dependencyClosure refAlreadyInGamma candidateBinders localRoots
      selected =
        [ binder
        | binder@(ref, _) <- candidateBinders,
          refMember ref localRefs
        ]
      selectedRefs = map fst selected
      missingRoots =
        [ ref
        | ref <- localRoots,
          not (refMember ref selectedRefs)
        ]
      missingDependencies =
        [ dependency
        | (_, Just bound) <- selected,
          dependency <- freeTypeVarRefsType (tyToElab bound),
          not (refAlreadyInGamma dependency),
          not (refMember dependency selectedRefs)
        ]
      requiredBounds =
        [ ( requirement,
            ref,
            expectedBoundForRef requirementBoundRenames requirement ref,
            selectedBoundFor ref selected <|> (agaBound <$> ambientAuthority)
          )
        | ((requirement, ref), ambientAuthority) <-
            zip (zip requiredBinders requiredRefs) ambientRequiredAuthorities
        ]
      missingAmbientBounds =
        [ (ref, expectedBound)
        | (_, ref, expectedBound, Nothing) <- requiredBounds,
          refAlreadyInGamma ref
        ]
      mismatchedBounds =
        [ (ref, expectedBound, actualBound)
        | (_, ref, expectedBound, Just actualBound) <- requiredBounds,
          not (requiredBoundSatisfiedBy ref expectedBound actualBound)
        ]
  if null missingRoots
      && null missingDependencies
      && null missingAmbientBounds
      && null mismatchedBounds
    then do
      ambientAliases <-
        foldM
          insertAmbientAlias
          IntMap.empty
          [ (getNodeId node, agaExactRef authority)
          | (requirement, Just authority) <-
              zip requiredBinders ambientRequiredAuthorities,
            node <-
              rgbOperatedRoot requirement
                : rgbExteriorNode requirement
                : NonEmpty.toList (rgbResultRoots requirement)
          ]
      pure
        ConstructionGammaPlan
          { cgpBinders = selected,
            cgpAmbientAliases = ambientAliases
          }
    else
      Left
        ( ValidationFailed
            [ "construction Γ requirements are not closed by the prepared scheme",
              "  required refs: " ++ show requiredRefs,
              "  exact owner: " ++ show owner,
              "  selected refs: " ++ show selectedRefs,
              "  missing roots: " ++ show missingRoots,
              "  missing dependencies: " ++ show missingDependencies,
              "  missing ambient bound authorities: " ++ show missingAmbientBounds,
              "  mismatched bounds: " ++ show mismatchedBounds,
              "  scheme: " ++ show (siScheme schemeInfo)
            ]
        )
  where
    constructionBinderRenames =
      envConstructionBinderRenames env

    dependencyHasUnboundedBinderAuthority ref =
      any authorityNodeIsUnbounded (graphBinderAuthorityNodes ref)
        || any authorityNodeIsUnbounded (sourceSidecarLiveBinderNodes ref)
      where
        presolutionView = scPresolutionView scopeContext
        authorityNodeIsUnbounded node =
          pvLookupVarBound presolutionView node == Nothing

    graphBinderAuthorityNodes ref =
      [ pvCanonical presolutionView node
      | node <- maybeToList (typeBinderRefNode ref)
      , case pvLookupNode presolutionView node of
          Just TyVar {} -> True
          _ -> False
      ]
      where
        -- A graph identity carries its declaration node directly.  This is
        -- the ordinary authority path for inferred binders that have no
        -- source-level identity sidecar.
        presolutionView = scPresolutionView scopeContext

    sourceSidecarLiveBinderNodes ref =
      foldr insertDistinctNode []
        [ pvCanonical presolutionView (NodeId nodeKey)
        | (nodeKey, sourceRef) <-
            IntMap.toList (grSourceBinderRefs requirements)
        , typeBinderRefsSameIdentity ref sourceRef
        , case pvLookupNode presolutionView (NodeId nodeKey) of
            Just TyVar {} -> True
            -- Constraint generation deliberately records both the lexical
            -- variable node and the forall/mu node that owns it.  Only the
            -- variable entry is a construction dependency binder.
            Just _ -> False
            Nothing -> False
        ]
      where
        -- A source-projected ref intentionally carries its lexical identity,
        -- not a graph-node identity.  Recover its graph authorities from the
        -- sidecar keys; asking 'typeBinderRefNode' of the projected ref loses
        -- precisely the provenance that the sidecar exists to preserve.
        presolutionView = scPresolutionView scopeContext

    insertDistinctNode node nodes
      | node `elem` nodes = nodes
      | otherwise = node : nodes

    insertDistinctRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    ownerEmissionScheme =
      applySchemeInfoRefRenames constructionBinderRenames
        <$> ownerEmissionScheme0
    alignedSourceBinderRefs =
      IntMap.map
        (applyRefRenames constructionBinderRenames)
        (grSourceBinderRefs requirements0)
    alignedAmbientGammaAuthorities =
      IntMap.map
        ( \authority ->
            authority
              { agaExactRef =
                  applyRefRenames
                    constructionBinderRenames
                    (agaExactRef authority)
              , agaBound =
                  applyTypeVarRefRenames
                    constructionBinderRenames
                    (agaBound authority)
              }
        )
        (grAmbientGammaAuthorities requirements0)
    requirements =
      requirements0
        { grRequiredGammaBinders =
            map alignRequirement (grRequiredGammaBinders requirements0)
        , grSourceBinderRefs = alignedSourceBinderRefs
        , grAmbientGammaAuthorities = alignedAmbientGammaAuthorities
        }
    alignRequirement requirement =
      requirement
        { rgbOperatedType =
            applyTypeVarRefRenames
              constructionBinderRenames
              (rgbOperatedType requirement)
        }
    schemeInfo =
      applySchemeInfoRefRenames constructionBinderRenames schemeInfo0
    requiredBinders = grRequiredGammaBinders requirements
    -- The operated requirement is not the only source-identity consumer.  A
    -- pending packet can carry a free lexical result ref that its local Gamma
    -- consumer will bind under the graph construction identity.  Include both
    -- owner views when building the one source-to-construction quotient.
    requiredSourceRefs =
      concatMap
        (freeTypeVarRefsType . rgbOperatedType)
        requiredBinders
        ++ concatMap
          (freeTypeVarRefsType . schemeToType . siScheme)
          pendingOwnerSchemeInfos
        ++ maybe
          []
          (freeTypeVarRefsType . schemeToType . siScheme)
          ownerEmissionScheme
    pendingOwnerSchemeInfos =
      [ ownerSchemeInfo
      | requirement <- requiredBinders
      , edgeId <- NonEmpty.toList (rgbEdgeIds requirement)
      , Just closure <-
          [IntMap.lookup (getEdgeId edgeId) (envLocalGammaClosures env)]
      , lgcOwner closure == owner
      , Just ownerSchemeInfo <- [lgcOwnerPendingScheme closure]
      ]
    relevantRequirementSourceBinderRefs =
      IntMap.filter
        (\sourceRef -> any (typeBinderRefsSameIdentity sourceRef) requiredSourceRefs)
        (grSourceBinderRefs requirements)
    ownerScope = localGammaOwnerScope owner
    -- A generalized binder may be placed in the leading body spine when it
    -- constructs the result of this exact source boundary.  It is still part
    -- of the boundary's Lambda(Gamma) prefix: child computations need it in
    -- scope and the completed term must emit the matching ETyAbsRef.  Looking
    -- only at the outer 'ElabScheme' list leaves such a result forall in the
    -- expected type while asking the children to construct that closed
    -- scheme directly.
    schemeBinders =
      fst (splitForallsRefs (schemeToType (siScheme schemeInfo)))
    subst = schemeInfoBinderRefSubst schemeInfo
    liveBindParents = cBindParents (pvConstraint (scPresolutionView scopeContext))
    boundaryConstructionIdentityRoutes =
      foldM
        insertBoundaryConstructionRoute
        (envConstructionIdentityRoutes env)
        ( binderIdentityRoutes schemeBinders
            ++ IntMap.toList subst
            ++ maybe
              []
              ( \ownerSchemeInfo ->
                  binderIdentityRoutes
                    (schemeBinderRefs (siScheme ownerSchemeInfo))
                    ++ IntMap.toList
                      (schemeInfoBinderRefSubst ownerSchemeInfo)
              )
              ownerEmissionScheme
        )

    -- An explicit graph-backed forall is itself an identity route even when
    -- generalization did not need a separate substitution entry for it.  Make
    -- that construction fact available to source-binder projection before
    -- comparing S(operated); otherwise an ordinary source annotation can stay
    -- in the generated source domain while its owner binder is already in the
    -- graph construction domain.
    binderIdentityRoutes binders =
      [ (getNodeId node, ref)
      | (ref, _) <- binders
      , Just node <- [typeBinderRefNode ref]
      ]

    insertBoundaryConstructionRoute routes (nodeKey, routedRef) =
      case IntMap.lookup nodeKey routes of
        Nothing -> pure (IntMap.insert nodeKey routedRef routes)
        Just existing ->
          case
              selectBoundaryConstructionRoute
                ( IntMap.lookup
                    nodeKey
                    (envConstructionIdentityRoutes env)
                )
                boundaryLocalRefs
                existing
                routedRef
            of
            Just selected
              | typeBinderRefsSameIdentity selected existing ->
                  pure routes
              | otherwise ->
                  pure (IntMap.insert nodeKey selected routes)
            Nothing ->
              Left
                ( ValidationFailed
                    [ "construction boundary has conflicting identity routes"
                    , "  graph node: " ++ show (NodeId nodeKey)
                    , "  established authority: "
                        ++ show
                          ( IntMap.lookup
                              nodeKey
                              (envConstructionIdentityRoutes env)
                          )
                    , "  enclosing route: " ++ show existing
                    , "  local route: " ++ show routedRef
                    , "  owner: " ++ show owner
                    , "  scheme binders: " ++ show schemeBinders
                    , "  scheme: " ++ show (siScheme schemeInfo)
                    , "  scheme substitution: " ++ show subst
                    , "  owner emission scheme: " ++ show (siScheme <$> ownerEmissionScheme)
                    , "  enclosing construction aliases: " ++ show (envConstructionGammaAliases env)
                    ]
                )

    boundaryLocalRefs =
      leadingConstructionRefs schemeInfo
        ++ maybe
          []
          leadingConstructionRefs
          ownerEmissionScheme

    -- Root closure can place an owner-local forall in the leading body spine
    -- instead of the outer 'ElabScheme' binder list.  Both representations
    -- are declarations owned by this exact boundary and therefore shadow an
    -- enclosing route for the same graph node.  Non-leading nested foralls
    -- remain lexical to their own subterm and are deliberately excluded.
    leadingConstructionRefs =
      map fst
        . fst
        . splitForallsRefs
        . schemeToType
        . siScheme
    localClosureBindersFor boundRenames requirement = do
      mbClosure <-
        selectLocalGammaClosureOwnerLane
          owner
          (envLocalGammaClosures env)
          requirement
      case mbClosure of
        Nothing -> pure (False, [])
        Just closure
          | edgeKeySet (lgcEdgeIds closure)
              /= edgeKeySet (rgbEdgeIds requirement) ->
              localClosureFailure
                requirement
                "local Gamma closure does not cover the complete required edge set"
                [closure]
          | lgcExteriorNode closure /= rgbExteriorNode requirement ->
              localClosureFailure
                requirement
                "local Gamma closure exterior disagrees with the requirement"
                [closure]
          | lgcConsumerIdentity closure
              /= typeBinderIdentityFromNode (rgbExteriorNode requirement) ->
              localClosureFailure
                requirement
                "local Gamma closure consumer disagrees with the exterior identity"
                [closure]
          | lgcOwner closure /= owner ->
              localClosureFailure
                requirement
                "local Gamma closure belongs to a different source constructor"
                [closure]
          | not
              ( directEdgeLocalClosureOwnsRequirement closure requirement
                  || rootRaiseMergeExteriorOwnedByScope
                    (scGaParents scopeContext)
                    (localGammaOwnerScope owner)
                    (lgcExteriorNode closure)
              ) ->
              localClosureFailure
                requirement
                "local Gamma closure has neither direct edge-local nor flexible-scope ownership"
                [closure]
          | otherwise ->
              fmap
                ((,) True)
                ( validateOwnerConstructionBinders
                    boundRenames
                    requirement
                    closure
                )

    directEdgeLocalClosureOwnsRequirement closure requirement =
      case coverage of
        RequiredGammaOnly ->
          directApplicationClosureOwnsEdges
            closure
            (rgbEdgeIds requirement)
        OwnerLocalAndRequiredGamma -> False
        CompleteSchemeAndRequiredGamma -> False

    validateOwnerConstructionBinders boundRenames requirement closure =
      case lgcOwnerPendingScheme closure of
        -- An ordinary syntactic owner has no prebuilt packet binder.  It must
        -- still select and emit the requirement through this local planner.
        Nothing -> pure []
        Just recordedOwnerSchemeInfo -> do
          actualOwnerSchemeInfo <-
            case ownerEmissionScheme of
              Just actual -> pure actual
              Nothing ->
                Left
                  ( ValidationFailed
                      [ "local Gamma packet proof has no actual owner emission"
                      , "  edges: " ++ show (lgcEdgeIds closure)
                      , "  owner: " ++ show (lgcOwner closure)
                      , "  recorded scheme: " ++ show recordedOwnerSchemeInfo
                      ]
                  )
          let alignedRecordedGraphSchemeInfo =
                applySchemeInfoRefRenames
                  boundRenames
                  ( applySchemeInfoRefRenames
                      constructionBinderRenames
                      recordedOwnerSchemeInfo
                  )
              alignedActualSchemeInfo =
                applySchemeInfoRefRenames
                  boundRenames
                  actualOwnerSchemeInfo
          recordedGraphConsumerRef <-
            routedOwnerConsumer
              "recorded graph packet"
              closure
              alignedRecordedGraphSchemeInfo
          actualConsumerRef <-
            routedOwnerConsumer
              "actual owner emission"
              closure
              alignedActualSchemeInfo
          let recordedSchemeAlignedToOwner =
                if
                    typeBinderRefsSameIdentity
                      recordedGraphConsumerRef
                      actualConsumerRef
                  then alignedRecordedGraphSchemeInfo
                  else
                    -- Both schemes route the closure's exact exterior key.
                    -- The pending packet records the earlier construction
                    -- identity; the actual owner emission fixes its final
                    -- binder identity.  Join those typed routes here before
                    -- source projection instead of comparing their graph
                    -- representatives or display names.
                    applySchemeInfoRefRenames
                      [(recordedGraphConsumerRef, actualConsumerRef)]
                      alignedRecordedGraphSchemeInfo
          alignedRecordedSchemeInfo <-
            either
              ( \cause ->
                  Left
                    ( ValidationFailed
                        [ "local Gamma pending scheme has inconsistent source-binder provenance"
                        , "  edges: " ++ show (lgcEdgeIds closure)
                        , "  owner: " ++ show (lgcOwner closure)
                        , "  cause: " ++ cause
                        ]
                    )
              )
              Right
              ( resolveConstructionSourceBindersInSchemeInfoExcept
                  ( Set.singleton
                      (typeBinderRefIdentity actualConsumerRef)
                  )
                  (scopeTypeBinderIdentityRepresentative scopeContext)
                  (grSourceBinderRefs requirements)
                  recordedSchemeAlignedToOwner
              )
          recordedConsumerRef <-
            routedOwnerConsumer
              "recorded packet"
              closure
              alignedRecordedSchemeInfo
          if not
              ( typeBinderRefsSameIdentity
                  recordedConsumerRef
                  actualConsumerRef
              )
            then
              Left
                ( ValidationFailed
                    [ "local Gamma packet proof and actual owner route disagree"
                    , "  edges: " ++ show (lgcEdgeIds closure)
                    , "  owner: " ++ show (lgcOwner closure)
                    , "  recorded consumer: " ++ show recordedConsumerRef
                    , "  actual consumer: " ++ show actualConsumerRef
                    , "  recorded representative: " ++ show (consumerRepresentative recordedConsumerRef)
                    , "  actual representative: " ++ show (consumerRepresentative actualConsumerRef)
                    , "  recorded scheme: " ++ show (siScheme alignedRecordedSchemeInfo)
                    , "  recorded substitution: " ++ show (siSubstRefs alignedRecordedSchemeInfo)
                    , "  actual scheme: " ++ show (siScheme alignedActualSchemeInfo)
                    , "  actual substitution: " ++ show (siSubstRefs alignedActualSchemeInfo)
                    , "  source binder routes: " ++ show (grSourceBinderRefs requirements)
                    , "  source-to-construction renames: " ++ show boundRenames
                    ]
                )
            else do
              pendingOwnerBinders <-
                validatePendingOwnerConsumer
                  closure
                  alignedRecordedSchemeInfo
                  recordedConsumerRef
              actualOwnerBinders <-
                validateOwnerConsumerBinder
                  "actual owner emission"
                  boundRenames
                  requirement
                  closure
                  alignedActualSchemeInfo
                  actualConsumerRef
              pure (pendingOwnerBinders ++ actualOwnerBinders)

    consumerRepresentative ref =
      scopeTypeBinderIdentityRepresentative scopeContext
        <$> typeBinderRefNode ref

    -- The prepared closure records only the constructor's pending consumer
    -- route.  Its empty bound is deliberate: the recursively checked child is
    -- the sole authority that materializes S'(operated) in the actual owner
    -- emission.  Comparing an earlier completed packet presentation with the
    -- current exact requirement would reintroduce two competing bound owners.
    validatePendingOwnerConsumer closure ownerSchemeInfo routedConsumerRef =
      case matchingBinders of
        [(_, Nothing)] ->
          pure nonConsumerBinders
        [(_, Just staleBound)] ->
          Left
            ( ValidationFailed
                [ "local Gamma pending consumer is already materialized"
                , "  edges: " ++ show (lgcEdgeIds closure)
                , "  owner: " ++ show (lgcOwner closure)
                , "  consumer: " ++ show routedConsumerRef
                , "  stale bound: " ++ show (tyToElab staleBound)
                , "  pending scheme: " ++ show (siScheme ownerSchemeInfo)
                ]
            )
        [] ->
          Left
            ( ValidationFailed
                [ "local Gamma pending scheme does not emit its proved consumer slot"
                , "  edges: " ++ show (lgcEdgeIds closure)
                , "  owner: " ++ show (lgcOwner closure)
                , "  consumer: " ++ show routedConsumerRef
                , "  pending scheme: " ++ show (siScheme ownerSchemeInfo)
                ]
            )
        matches ->
          Left
            ( ValidationFailed
                [ "local Gamma pending scheme emits its consumer slot more than once"
                , "  edges: " ++ show (lgcEdgeIds closure)
                , "  owner: " ++ show (lgcOwner closure)
                , "  matching binders: " ++ show matches
                ]
            )
      where
        ownerBinders =
          fst (splitForallsRefs (schemeToType (siScheme ownerSchemeInfo)))
        (matchingBinders, nonConsumerBinders) =
          partition
            (typeBinderRefsSameIdentity routedConsumerRef . fst)
            ownerBinders

    routedOwnerConsumer role closure ownerSchemeInfo =
      case
          IntMap.lookup
            (getNodeId (lgcExteriorNode closure))
            (siSubstRefs ownerSchemeInfo)
      of
        Just ref -> pure ref
        Nothing ->
          Left
            ( ValidationFailed
                [ "local Gamma packet has no substitution route for its proved exterior"
                , "  role: " ++ role
                , "  edges: " ++ show (lgcEdgeIds closure)
                , "  owner: " ++ show (lgcOwner closure)
                , "  exterior: " ++ show (lgcExteriorNode closure)
                , "  owner substitution: " ++ show (siSubstRefs ownerSchemeInfo)
                ]
            )

    validateOwnerConsumerBinder role boundRenames requirement closure ownerSchemeInfo routedConsumerRef =
      case matchingBinders of
        [binder@(_, mbBound)] ->
          let expectedBound =
                expectedBoundForRef
                  boundRenames
                  requirement
                  routedConsumerRef
              actualBound = maybe TBottom tyToElab mbBound
           in if
                requiredBoundSatisfiedBy
                  routedConsumerRef
                  expectedBound
                  actualBound
                then pure [binder]
                else
                  Left
                    ( ValidationFailed
                        [ "local Gamma owner binder disagrees with S(operated)"
                        , "  role: " ++ role
                        , "  edges: " ++ show (lgcEdgeIds closure)
                        , "  owner: " ++ show (lgcOwner closure)
                        , "  consumer: " ++ show routedConsumerRef
                        , "  expected bound: " ++ show expectedBound
                        , "  actual bound: " ++ show actualBound
                        , "  source binder routes: " ++ show relevantRequirementSourceBinderRefs
                        , "  source-to-construction renames: " ++ show boundRenames
                        , "  owner scheme: " ++ show (siScheme ownerSchemeInfo)
                        ]
                    )
        []
          -- Root closure can own this forall while the local packet scheme
          -- refers to it freely.  Accept that placement only when the exact
          -- routed identity is already in the construction Γ and is present
          -- in the owner payload.  The enclosing required-bound check below
          -- still proves that the ambient binding satisfies S(operated).
          | envContainsFreeTypeBinderRef
              (scopeTypeBinderIdentityRepresentative scopeContext)
              (envFreeTypeBinderRefs env)
              routedConsumerRef
          , any
              (typeBinderRefsSameIdentity routedConsumerRef)
              (freeTypeVarRefsType (schemeToType (siScheme ownerSchemeInfo))) ->
              pure []
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "local Gamma packet does not emit its proved consumer binder"
                    , "  role: " ++ role
                    , "  edges: " ++ show (lgcEdgeIds closure)
                    , "  owner: " ++ show (lgcOwner closure)
                    , "  consumer identity: " ++ show (lgcConsumerIdentity closure)
                    , "  routed consumer: " ++ show routedConsumerRef
                    , "  owner binders: " ++ show ownerBinders
                    , "  owner scheme: " ++ show (siScheme ownerSchemeInfo)
                    ]
                )
        matches ->
          Left
            ( ValidationFailed
                [ "local Gamma packet emits its consumer binder more than once"
                , "  role: " ++ role
                , "  edges: " ++ show (lgcEdgeIds closure)
                , "  owner: " ++ show (lgcOwner closure)
                , "  matching binders: " ++ show matches
                ]
            )
      where
        -- Root closure may move an owner-local forall from the scheme binder
        -- list into the leading body spine.  Both forms are explicit emitted
        -- binders, so validate the complete leading construction type rather
        -- than only the outer 'ElabScheme' field.
        ownerBinders =
          fst (splitForallsRefs (schemeToType (siScheme ownerSchemeInfo)))
        matchingBinders =
          [ binder
          | binder@(ref, _) <- ownerBinders
          , typeBinderRefsSameIdentity ref routedConsumerRef
          ]

    localClosureFailure requirement reason closures =
      Left
        ( ValidationFailed
            [ reason,
              "  exact owner: " ++ show owner,
              "  requirement: " ++ show requirement,
              "  closures: " ++ show closures
            ]
        )

    edgeKeySet =
      IntSet.fromList . map getEdgeId . NonEmpty.toList

    -- A construction quotient may route a source binder to the exterior that
    -- this very Gamma entry declares.  That route is valid in the entry's
    -- body, after the declaration has entered scope, but never in its bound:
    -- xMLF requires @alpha@ not to occur free in @sigma@ for
    -- @Gamma, alpha > sigma@.  Keep the source dependency in the bound so the
    -- dependency closure emits (or inherits) its declaration before alpha.
    -- This is the identity-bearing analogue of capture avoidance; turning the
    -- dependency into alpha would manufacture the invalid F-bound
    -- @alpha > sigma[alpha/beta]@.
    applyBinderBoundRenames targetRef =
      applyTypeVarRefRenames
        . filter
          ( \(_, outwardRef) ->
              not (typeBinderRefsSameIdentity targetRef outwardRef)
          )

    expectedBoundForRef boundRenames requirement targetRef =
      case applyBinderBoundRenames targetRef boundRenames (rgbOperatedType requirement) of
        TVarRef _ -> TVarRef targetRef
        expectedBound -> expectedBound

    -- A 'RequiredGammaBinder' is the typed construction fact for its local
    -- abstraction.  Generalization can quotient its exterior and result to
    -- the target itself, in which case the resulting scheme substitution has
    -- the authoritative binder identity but no syntactic forall yet.  Build
    -- that binder here from the requirement, instead of rejecting the lawful
    -- quotient and asking a later term/type comparison to recover it.
    requirementConstructionBinder boundRenames refAlreadyInGamma requirement ref
      | refAlreadyInGamma ref = pure []
      | otherwise = do
          mbBound <- requirementConstructionBound ref expectedBound
          pure [(ref, mbBound)]
      where
        expectedBound = expectedBoundForRef boundRenames requirement ref

    requirementConstructionBound ref expectedBound =
      case expectedBound of
        TBottom -> pure Nothing
        TVarRef operatedRef
          | typeBinderRefsSameIdentity ref operatedRef -> pure Nothing
        _ ->
          either
            ( \cause ->
                Left
                  ( ValidationFailed
                      [ "construction Gamma requirement has no structural bound"
                      , "  binder: " ++ show ref
                      , "  expected bound: " ++ show expectedBound
                      , "  cause: " ++ cause
                      ]
                  )
            )
            (pure . Just)
            (elabToBound expectedBound)

    -- Generalization may quotient a required exterior directly to the
    -- lexical variable that is S(operated).  In that case the requirement is
    -- already closed by identity: the ambient variable is the RaiseMerge
    -- result, not a new Gamma binder whose lexical bound must become @a@.
    -- All other routes still require the prepared or ambient bound to agree.
    requiredBoundSatisfiedBy ref expectedBound actualBound =
      case expectedBound of
        TVarRef operatedRef
          | typeBinderRefsSameIdentity ref operatedRef -> True
        _ ->
          alphaEqType expectedBound actualBound
            || churchAwareEqType expectedBound actualBound
            || completeUnboundedForallSpecializesTo
              actualBound
              expectedBound

    findBinder ref = find (typeBinderRefsSameIdentity ref . fst)

    selectedBoundFor ref binders =
      maybe TBottom tyToElab . snd <$> findBinder ref binders

    insertAmbientAlias aliases (nodeKey, exactRef) =
      case IntMap.lookup nodeKey aliases of
        Nothing -> pure (IntMap.insert nodeKey exactRef aliases)
        Just existing
          | typeBinderRefsSameIdentity existing exactRef -> pure aliases
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "construction Gamma requirements select different exact ambient declarations"
                    , "  graph node: " ++ show (NodeId nodeKey)
                    , "  first ambient declaration: " ++ show existing
                    , "  second ambient declaration: " ++ show exactRef
                    ]
                )

    ambientBoundFor refAlreadyInGamma requirement ref
      | not (refAlreadyInGamma ref) = pure Nothing
      | otherwise = do
          preparedAuthority <- directPreparedAmbientAuthority ref requirement
          case preparedAuthority of
            Just authority -> pure (Just authority)
            Nothing -> selectFirstAmbientGroup candidateGroups
      where
        bindings = Map.toList (envTypeBindings env)
        directOperatedRefs =
          case rgbOperatedType requirement of
            TVarRef operatedRef -> [operatedRef]
            _ -> []
        requiredRefsForLookup = [ref]
        routedExteriorRefs =
          routedRefsForNodes [rgbExteriorNode requirement]
        routedResultRefs =
          routedRefsForNodes (NonEmpty.toList (rgbResultRoots requirement))
        routedOperatedRootRefs =
          routedRefsForNodes [rgbOperatedRoot requirement]
        routedRefsForNodes nodes =
          [ routedRef
          | node <- nodes
          , Just routedRef <-
              [ IntMap.lookup
                  (getNodeId node)
                  (envConstructionGammaAliases env)
              ]
          ]

        exactBindingCandidates lookupRefs =
          [ (candidateRef, bound)
          | (candidateRef, bound) <- bindings,
            any
              (`typeBinderRefsSameIdentity` candidateRef)
              lookupRefs
          ]
        representativeBindingCandidates lookupRefs =
          [ (candidateRef, bound)
          | (candidateRef, bound) <- bindings,
            any
              (`sameGraphRepresentative` candidateRef)
              lookupRefs
          ]
        exactFreeCandidates lookupRefs =
          [ (candidateRef, TBottom)
          | candidateRef <- ambientFreeRefs,
            any
              (`typeBinderRefsSameIdentity` candidateRef)
              lookupRefs
          ]
        representativeFreeCandidates lookupRefs =
          [ (candidateRef, TBottom)
          | candidateRef <- ambientFreeRefs,
            any
              (`sameGraphRepresentative` candidateRef)
              lookupRefs
          ]
        candidateGroups =
          [ ( "exact required type binding",
              exactBindingCandidates requiredRefsForLookup
            ),
            ( "exact required free binder",
              exactFreeCandidates requiredRefsForLookup
            ),
            -- The exterior is the declaration consumed by this Gamma
            -- requirement. Operated and result roots are producer routes;
            -- they may legitimately retain different exact ambient
            -- identities until this construction publishes their common
            -- outward alias. Select each role independently instead of
            -- treating all three endpoint routes as declarations of one
            -- pre-existing identity.
            ( "exact exterior construction route type binding",
              exactBindingCandidates routedExteriorRefs
            ),
            ( "exact exterior construction route free binder",
              exactFreeCandidates routedExteriorRefs
            ),
            ( "exact result construction route type binding",
              exactBindingCandidates routedResultRefs
            ),
            ( "exact result construction route free binder",
              exactFreeCandidates routedResultRefs
            ),
            ( "exact operated-root construction route type binding",
              exactBindingCandidates routedOperatedRootRefs
            ),
            ( "exact operated-root construction route free binder",
              exactFreeCandidates routedOperatedRootRefs
            ),
            ( "exact operated type binding",
              exactBindingCandidates directOperatedRefs
            ),
            ( "exact operated free binder",
              exactFreeCandidates directOperatedRefs
            ),
            ( "representative required type binding",
              representativeBindingCandidates requiredRefsForLookup
            ),
            ( "representative required free binder",
              representativeFreeCandidates requiredRefsForLookup
            ),
            ( "representative exterior construction route type binding",
              representativeBindingCandidates routedExteriorRefs
            ),
            ( "representative exterior construction route free binder",
              representativeFreeCandidates routedExteriorRefs
            ),
            ( "representative result construction route type binding",
              representativeBindingCandidates routedResultRefs
            ),
            ( "representative result construction route free binder",
              representativeFreeCandidates routedResultRefs
            ),
            ( "representative operated-root construction route type binding",
              representativeBindingCandidates routedOperatedRootRefs
            ),
            ( "representative operated-root construction route free binder",
              representativeFreeCandidates routedOperatedRootRefs
            ),
            ( "representative operated type binding",
              representativeBindingCandidates directOperatedRefs
            ),
            ( "representative operated free binder",
              representativeFreeCandidates directOperatedRefs
            )
          ]

        -- A term binding such as @y : alpha@ puts @alpha@ in the ambient
        -- paper Gamma even when this owner has no explicit local type-binding
        -- entry for it.  In that case the only bound authority available at
        -- this boundary is the unbounded one.  Treating the missing map entry
        -- as absence from Gamma makes a local RaiseMerge requirement shadow
        -- @alpha@ and later emits a second @Lambda alpha@ around a term that
        -- still refers to @y@.  That construction is ill-scoped by design.
        EnvFreeTypeBinderRefs ambientFreeRefs = envFreeTypeBinderRefs env

        selectFirstAmbientGroup [] = pure Nothing
        selectFirstAmbientGroup ((_, []) : rest) =
          selectFirstAmbientGroup rest
        selectFirstAmbientGroup ((role, candidates) : _) =
          uniqueAmbientAuthority role candidates

        uniqueAmbientAuthority _ [] = pure Nothing
        uniqueAmbientAuthority role ((exactRef, exactBound) : rest)
          | any
              (not . typeBinderRefsSameIdentity exactRef . fst)
              rest =
              Left
                ( ValidationFailed
                    [ "construction Gamma has multiple exact ambient declarations for one representative"
                    , "  lookup role: " ++ role
                    , "  required ref: " ++ show ref
                    , "  requirement: " ++ show requirement
                    , "  routed requirement refs: "
                        ++ show routedRequirementRefPairs
                    , "  construction aliases: "
                        ++ show (envConstructionGammaAliases env)
                    , "  first exact declaration: " ++ show exactRef
                    , "  other candidates: " ++ show rest
                    ]
                )
          | all
              ( \(_, candidateBound) ->
                  alphaEqType exactBound candidateBound
                    || churchAwareEqType exactBound candidateBound
              )
              rest =
              pure
                ( Just
                    AmbientGammaAuthority
                      { agaExactRef = exactRef,
                        agaBound = exactBound
                      }
                )
          | otherwise =
              Left
                ( ValidationFailed
                    [ "construction Gamma has conflicting ambient bounds for one identity"
                    , "  required ref: " ++ show ref
                    , "  exact ambient declaration: " ++ show exactRef
                    , "  exact ambient bound: " ++ show exactBound
                    , "  conflicting candidates: " ++ show rest
                    ]
                )

        sameGraphRepresentative left right =
          case (typeBinderRefNode left, typeBinderRefNode right) of
            (Just leftNode, Just rightNode) ->
              scopeTypeBinderIdentityRepresentative scopeContext leftNode
                == scopeTypeBinderIdentityRepresentative scopeContext rightNode
            _ -> False

        routedRequirementRefPairs =
          [ (node, routedRef)
          | node <-
              rgbOperatedRoot requirement
                : rgbExteriorNode requirement
                : NonEmpty.toList (rgbResultRoots requirement)
          , Just routedRef <-
              [ IntMap.lookup
                  (getNodeId node)
                  (envConstructionGammaAliases env)
              ]
          ]

    directPreparedAmbientAuthority requiredRef requirement =
      case
          selectDirectAmbientGammaAuthority
            requiredRef
            directAuthorities
        of
        Right authority -> pure authority
        Left (ValidationFailed messages) ->
          Left
            ( ValidationFailed
                ( "one construction requirement has conflicting direct ambient Gamma authorities"
                    : ("  requirement: " ++ show requirement)
                    : messages
                )
            )
        Left err -> Left err
      where
        directAuthorities =
          foldr insertDistinctAuthority []
            [ (authorityProvenance node authority, authority)
            | node <-
                rgbOperatedRoot requirement
                  : rgbExteriorNode requirement
                  : NonEmpty.toList (rgbResultRoots requirement)
            , Just authority <-
                [ IntMap.lookup
                    (getNodeId node)
                    (grAmbientGammaAuthorities requirements)
                ]
            ]

        authorityProvenance =
          directAmbientGammaAuthorityProvenance
            constructionBinderRenames
            owner
            pendingResultSourcePacket
            (envLocalGammaClosures env)
            requirement

        insertDistinctAuthority candidate authorities
          | any (sameDirectAuthority candidate) authorities = authorities
          | otherwise = candidate : authorities

        sameDirectAuthority (leftProvenance, left) (rightProvenance, right) =
          leftProvenance == rightProvenance
            && sameAmbientAuthority left right

        sameAmbientAuthority left right =
          typeBinderRefsSameIdentity
            (agaExactRef left)
            (agaExactRef right)
            && ( alphaEqType (agaBound left) (agaBound right)
                   || churchAwareEqType (agaBound left) (agaBound right)
               )

    requirementRefForOwner boundRenames requirement =
      case ownerConstructionRequirementRef boundRenames requirement of
        Just ref -> pure ref
        Nothing -> requirementRef requirement

    ownerConstructionRequirementRef boundRenames requirement =
      case
          [ closure
          | edgeId <- NonEmpty.toList (rgbEdgeIds requirement),
            Just closure <-
              [IntMap.lookup (getEdgeId edgeId) (envLocalGammaClosures env)]
          ]
      of
        closure : rest
          | length rest + 1 == NonEmpty.length (rgbEdgeIds requirement),
            all (== closure) rest,
            lgcOwner closure == owner,
            edgeKeySet (lgcEdgeIds closure)
              == edgeKeySet (rgbEdgeIds requirement),
            Just ownerSchemeInfo <- lgcOwnerPendingScheme closure ->
              IntMap.lookup
                (getNodeId (lgcExteriorNode closure))
                ( siSubstRefs
                    (applySchemeInfoRefRenames boundRenames ownerSchemeInfo)
                )
        _ -> Nothing

    binderOwnedByBoundary ref =
      any nodeOwnedByBoundary (binderRouteNodes ref)

    binderRouteNodes ref =
      case typeBinderRefNode ref of
        -- A graph identity carries its declaration node.  Other substitution
        -- routes can be descendant/result aliases and must not transfer
        -- ownership to this boundary merely because they share the ref.
        Just ownerNode -> [ownerNode]
        Nothing ->
          [ NodeId nodeKey
          | (nodeKey, routedRef) <- IntMap.toList subst,
            typeBinderRefsSameIdentity ref routedRef
          ]

    -- Binder ownership is a declaration fact from the live binding tree, not
    -- a quotient-class fact.  Canonicalizing the declaration before following
    -- its bind-parent path can jump past the exact gen node that owns
    -- @Lambda(Gamma_g)@.  In particular, an application's result binder may
    -- already have been merged with its enclosing consumer while its original
    -- node is still flexibly bound to the application's gen.  Keep that exact
    -- node for both ordinary and root-RaiseMerge ownership checks.
    nodeOwnedByBoundary node =
      flexiblyOwnedBy liveBindParents ownerScope node
        || rootRaiseMergeExteriorOwnedByScope
          (scGaParents scopeContext)
          ownerScope
          node

    flexiblyOwnedBy bindParents expectedOwnerScope node =
      go IntSet.empty (typeRef node)
      where
        go seen child
          | IntSet.member childKey seen = False
          | otherwise =
              case IntMap.lookup childKey bindParents of
                Just (parent, BindFlex)
                  | parent == expectedOwnerScope -> True
                  | TypeRef {} <- parent ->
                      go (IntSet.insert childKey seen) parent
                _ -> False
          where
            childKey = nodeRefKey child

    requirementRef requirement =
      case resultRootRef <|> IntMap.lookup (getNodeId (rgbExteriorNode requirement)) subst
        of
          Just ref -> pure (gammaConsumerRefForRoute requirement ref)
          Nothing ->
            Left
              ( ValidationFailed
                  [ "construction Γ requirement has no prepared binder substitution",
                    "  requirement: " ++ show requirement,
                    "  substitution: " ++ show subst
                  ]
              )
      where
        resultRootRef =
          foldr
            (\resultRoot fallback ->
                IntMap.lookup (getNodeId resultRoot) subst <|> fallback
            )
            Nothing
            (rgbResultRoots requirement)

    dependencyClosure refAlreadyInGamma binders = go
      where
        go refs =
          let dependencies =
                [ dependency
                | (ref, Just bound) <- binders,
                  refMember ref refs,
                  dependency <- freeTypeVarRefsType (tyToElab bound),
                  not (refAlreadyInGamma dependency),
                  any (typeBinderRefsSameIdentity dependency . fst) binders
                ]
              refs' = foldr insertRef refs dependencies
           in if length refs' == length refs then refs else go refs'

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Close only the part of a prepared construction Gamma that survives in
-- the constructed term or its result type.  Edge replay is allowed to use a
-- wider Gamma while constructing children, but an obligation that cancels
-- inside the boundary is not a free dependency of the completed xMLF term.
-- Selecting the bound-dependency closure here prevents a vacuous outer
-- forall such as @forall (a >= Int). Int@ without rewriting the completed
-- type after construction.
constructionGammaCompletionBinders
  :: [(TypeBinderRef, Maybe BoundType)]
  -> XmlfTerm
  -> ElabType
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
constructionGammaCompletionBinders binders term resultTy =
  orderConstructionGammaBinders
    "construction Gamma completion"
    [ binder
    | binder@(ref, _) <- binders,
      refMember ref selectedRefs
    ]
    resultTy
  where
    liveRoots =
      foldr insertRef (freeTypeVarRefsType resultTy) (Reduce.freeTypeVarRefsTerm term)
    selectedRefs = dependencyClosure liveRoots

    dependencyClosure refs =
      let dependencies =
            [ dependency
            | (ref, Just bound) <- binders,
              refMember ref refs,
              dependency <- freeTypeVarRefsType (tyToElab bound),
              any (typeBinderRefsSameIdentity dependency . fst) binders
            ]
          refs' = foldr insertRef refs dependencies
       in if length refs' == length refs
            then refs
            else dependencyClosure refs'

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Put a completed Gamma spine in lexical dependency order before emitting
-- its type abstractions.  Construction sources preserve their own stable
-- order, but merging independently prepared sources can place a dependent
-- binder before a binder named by its bound.  Reject cycles at this typed
-- construction boundary rather than discovering an out-of-scope variable
-- after the term has been built.
orderConstructionGammaBinders
  :: String
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
orderConstructionGammaBinders role binders resultTy =
  either
    (Left . ValidationFailed . pure)
    (Right . schemeBinderRefs)
    ( orderSourceProjectedSchemeBinders
        role
        (mkElabSchemeWithRefs binders resultTy)
    )

-- | Combine independently prepared construction-binder sources before they
-- are emitted.  A local Gamma closure and its pending owner scheme can both
-- carry the same declaration; that is one construction authority, not two
-- nested type abstractions.  Conversely, disagreeing bounds for one identity
-- are an invalid owner packet and must fail before term construction.
mergeConstructionBinderSources
  :: String
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
mergeConstructionBinderSources context first second = do
  first' <- validateSource "ordinary Gamma" first
  second' <- validateSource "prepared owner scheme" second
  foldM mergeBinder first' second'
  where
    validateSource source = foldM (insertSourceBinder source) []

    insertSourceBinder source accumulated binder@(ref, mbBound) =
      case find (typeBinderRefsSameIdentity ref . fst) accumulated of
        Nothing -> pure (accumulated ++ [binder])
        Just (_, existingBound) ->
          Left
            ( ValidationFailed
                [ "one construction source repeats a binder identity"
                , "  context: " ++ context
                , "  source: " ++ source
                , "  binder: " ++ show ref
                , "  first bound: " ++ show existingBound
                , "  second bound: " ++ show mbBound
                ]
            )

    mergeBinder accumulated binder@(ref, mbBound) =
      case find (typeBinderRefsSameIdentity ref . fst) accumulated of
        Nothing -> pure (accumulated ++ [binder])
        Just (_, existingBound)
          | constructionBoundsAgree existingBound mbBound -> pure accumulated
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "construction sources disagree on one binder bound"
                    , "  context: " ++ context
                    , "  binder: " ++ show ref
                    , "  first bound: " ++ show existingBound
                    , "  second bound: " ++ show mbBound
                    ]
                )

    constructionBoundsAgree left right =
      let leftTy = maybe TBottom tyToElab left
          rightTy = maybe TBottom tyToElab right
       in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

-- | Resolve a packet consumer through the construction scheme that owns it.
-- A quotient may publish the graph exterior only in 'siSubstRefs' while the
-- emitted forall already carries the outward source identity.  Both are one
-- typed route; consumers must not compare only the pre-quotient graph identity
-- against the final binder list.
schemeConsumerBinderCandidates
  :: TypeBinderIdentity
  -> SchemeInfo
  -> [(TypeBinderRef, Maybe BoundType)]
schemeConsumerBinderCandidates consumerIdentity schemeInfo =
  [ binder
  | binder@(ref, _) <- schemeBinderRefs (siScheme schemeInfo)
  , any (typeBinderRefsSameIdentity ref) consumerRefs
  ]
  where
    consumerRefs = schemeConsumerConstructionRefs consumerIdentity schemeInfo

-- | Resolve a prepared Gamma consumer in the construction domain of the
-- scheme currently being emitted.  Packet preparation and an enclosing
-- constructor can legitimately choose different graph representatives for
-- the same consumer.  The current scheme substitution is the typed quotient
-- that joins those domains; consulting the packet's earlier completed view
-- would retain a stale representative after that quotient.
schemeConsumerConstructionRefs
  :: TypeBinderIdentity
  -> SchemeInfo
  -> [TypeBinderRef]
schemeConsumerConstructionRefs consumerIdentity schemeInfo =
  directConsumerRef : maybe [] pure routedConsumerRef
  where
    directConsumerRef = directSchemeConsumerRef consumerIdentity
    routedConsumerRef = do
      consumerNode <- typeBinderRefNode directConsumerRef
      IntMap.lookup
        (getNodeId consumerNode)
        (schemeInfoBinderRefSubst schemeInfo)

schemeConsumerConstructionRef
  :: TypeBinderIdentity
  -> SchemeInfo
  -> TypeBinderRef
schemeConsumerConstructionRef consumerIdentity schemeInfo =
  fromMaybe directConsumerRef $ do
    consumerNode <- typeBinderRefNode directConsumerRef
    IntMap.lookup
      (getNodeId consumerNode)
      (schemeInfoBinderRefSubst schemeInfo)
  where
    directConsumerRef = directSchemeConsumerRef consumerIdentity

directSchemeConsumerRef :: TypeBinderIdentity -> TypeBinderRef
directSchemeConsumerRef consumerIdentity =
  typeBinderRefFromIdentity consumerIdentity "$consumer"

-- | Materialize the bound of a prepared consumer at the constructor that owns
-- its outgoing edge.  Packet preparation fixes the consumer identity and its
-- lexical placement before term elaboration; the recursively constructed child
-- fixes the exact @Typ(a)@ source.  Joining those two authorities here makes
-- the local Gamma correct before either Hyp or the enclosing type abstraction
-- is emitted.
materializeConsumerBound
  :: ScopeContext p
  -> ConsumerBoundOwnership
  -> TypeBinderIdentity
  -> ElabType
  -> SchemeInfo
  -> Either ElabError (SchemeInfo, ElabType)
materializeConsumerBound scopeContext ownership consumerIdentity sourceTy schemeInfo =
  case ownership of
    ConsumerBoundOwnedByEnclosingGamma _ _ ->
      -- The exact enclosing owner is already proved here.  Its bound is
      -- checked only after the body computation and local binder spine have
      -- been constructed, when the complete lambda type exists.
      pure (schemeInfo, sourceTy)
    ConsumerBoundOwnedByRootGamma _ ->
      -- The definition/root construction owns this exterior.  The completed
      -- child supplies its bound here; root packet placement emits the binder
      -- once at the outer scheme boundary.
      pure (schemeInfo, sourceTy)
    ConsumerBoundOwnedByRequiredGamma _ ->
      -- The packet's typed edge ownership authorizes the ordinary lambda
      -- construction below to emit this binder from the same source type.
      pure (schemeInfo, sourceTy)
    ConsumerBoundOwnedLocally ownedRef ->
      case matchingBindersFor ownedRef of
        [(consumerRef, Nothing)]
          | TVarRef sourceRef <- sourceTy
          , typeBinderRefsSameIdentity sourceRef consumerRef ->
              -- The source already is the flexible consumer.  Turning that
              -- fact into @a >= a@ would manufacture an invalid self-bound;
              -- the unbounded abstraction is the construction itself.
              pure (schemeInfo, sourceTy)
          | TVarRef sourceRef <- sourceTy ->
              -- Section 15.6.2 represents a peer-variable bound as an alias,
              -- not as an xMLF binder @consumer >= source@ (bare variables
              -- are deliberately outside 'BoundType').  Quotient the pending
              -- consumer through the checked child identity now; the same
              -- substitution later rewrites the outgoing Hyp to the ambient
              -- source, where it becomes the identity computation.
              pure
                ( applySchemeInfoRefRenames
                    [(consumerRef, sourceRef)]
                    schemeInfo
                , sourceTy
                )
          | otherwise -> do
              sourceBound <- checkedSourceBound
              pure
                ( schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        [ if typeBinderRefsSameIdentity ref consumerRef
                            then (ref, Just sourceBound)
                            else binder
                        | binder@(ref, _) <- binders
                        ]
                        (schemeBody scheme)
                    )
                    (schemeInfoBinderRefSubst schemeInfo)
                , sourceTy
                )
        [(consumerRef, Just existingBound)]
          | boundAgrees (tyToElab existingBound) ->
              pure (schemeInfo, tyToElab existingBound)
          | otherwise ->
              incompatibleBound
                "prepared local Gamma"
                consumerRef
                (tyToElab existingBound)
        [] ->
          Left
            ( ValidationFailed
                [ "prepared lambda packet has no local consumer binder"
                , "  consumer: " ++ show consumerIdentity
                ]
            )
        _ ->
          Left
            ( ValidationFailed
                [ "prepared lambda packet binds its consumer identity more than once"
                , "  consumer: " ++ show consumerIdentity
                , "  matches: " ++ show (matchingBindersFor ownedRef)
                ]
            )
  where
    scheme = siScheme schemeInfo
    binders = schemeBinderRefs scheme
    matchingBindersFor ownedRef =
      [ binder
      | binder@(ref, _) <- binders
      , typeBinderRefsSameIdentity ref ownedRef
      ]

    boundAgrees existingBound =
      case sourceTy of
        TVarRef sourceRef
          | typeBinderRefIdentity sourceRef == consumerIdentity -> True
        _ -> scopedTypesAgree scopeContext existingBound sourceTy

    checkedSourceBound = do
      sourceBound <-
        either
          ( \cause ->
              Left
                ( ValidationFailed
                    [ "lambda-body consumer source is not a valid Gamma bound"
                    , "  consumer: " ++ show consumerIdentity
                    , "  source: " ++ show sourceTy
                    , "  cause: " ++ cause
                    ]
                )
          )
          Right
          (elabToBound sourceTy)
      pure sourceBound

    incompatibleBound ownerLabel ref existingBound =
      Left
        ( ValidationFailed
            [ "lambda consumer source contradicts its prepared Gamma bound"
            , "  owner: " ++ ownerLabel
            , "  consumer: " ++ show consumerIdentity
            , "  binder: " ++ show ref
            , "  prepared bound: " ++ show existingBound
            , "  constructed source: " ++ show sourceTy
            ]
        )

prepareLambdaConsumerConstruction
  :: ScopeContext p
  -> Env
  -> LocalGammaOwner
  -> Maybe PreparedSubtermGeneralization
  -> Maybe SchemeInfo
  -> EdgeId
  -> ElabType
  -> Either ElabError LambdaConsumerConstructionPlan
prepareLambdaConsumerConstruction scopeContext enclosingEnv owner mbPacket mbSchemeInfo bodyEdge sourceTy = do
  ( schemeInfo
    , constructionSourceTy
    , expectedEnclosingBound
    , consumerWithoutLocalBinder
    ) <-
    case (mbPacket, mbSchemeInfo) of
      (Just packet, Just preparedSchemeInfo) ->
        case subtermGeneralizationConsumerIdentity packet of
          Nothing -> pure (Just preparedSchemeInfo, sourceTy, Nothing, Nothing)
          Just consumerIdentity -> do
            (localSchemeInfo, packetConsumerWithoutLocalBinder) <-
              materializeOwnedPacketGamma
                packet
                preparedSchemeInfo
            ownership <-
              consumerBoundOwnership
                enclosingEnv
                owner
                packet
                bodyEdge
                consumerIdentity
                localSchemeInfo
            (materializedSchemeInfo, materializedSourceTy) <-
              materializeConsumerBound
                scopeContext
                ownership
                consumerIdentity
                sourceTy
                localSchemeInfo
            inheritedSchemeInfo <-
              inheritAmbientConsumerBinder
                consumerIdentity
                materializedSourceTy
                materializedSchemeInfo
            let expectedBound =
                  case ownership of
                    ConsumerBoundOwnedByEnclosingGamma _ bound ->
                      case
                          subtermGeneralizationConsumerAuthority packet
                            >>= subtermConsumerAuthorityEnclosingOwner
                        of
                          Just enclosingOwner
                            | lgoConstructor enclosingOwner == LocalLambdaGamma ->
                                Just (subtractAmbientLeadingBinders bound)
                          _ -> Nothing
                    _ -> Nothing
            pure
              ( Just inheritedSchemeInfo
              , materializedSourceTy
              , expectedBound
              , packetConsumerWithoutLocalBinder
              )
      (Nothing, Nothing) -> pure (Nothing, sourceTy, Nothing, Nothing)
      _ ->
        Left
          ( ValidationFailed
              [ "prepared lambda packet and construction scheme disagree"
              , "  packet: " ++ show mbPacket
              , "  scheme: " ++ show mbSchemeInfo
              ]
          )
  pure
    LambdaConsumerConstructionPlan
      { lccSourceType = constructionSourceTy
      , lccSchemeInfo = schemeInfo
      , lccExpectedEnclosingBound = expectedEnclosingBound
      , lccEnclosingOwnedBinderRefs =
          maybe [] freeTypeVarRefsType expectedEnclosingBound
      , lccConsumerWithoutLocalBinder = consumerWithoutLocalBinder
      }
  where
    -- The enclosing packet publishes its complete scheme, while this child
    -- lambda is constructed after that Gamma has already opened any shared
    -- leading binders.  Apply Gen(Gamma, tau)'s environment subtraction to
    -- the expected endpoint as well as to the child's construction scheme;
    -- otherwise an ordinary rank-1 @\x -> f x@ is incorrectly required to
    -- recreate the enclosing @forall a b@ prefix.
    subtractAmbientLeadingBinders ty =
      case ty of
        TForallRef ref _ body
          | envOwnsExactTypeBinderRef enclosingEnv ref ->
              subtractAmbientLeadingBinders body
        _ -> ty

    -- Materializing a peer-variable packet consumer can quotient its pending
    -- binder directly to the exact identity already owned by the enclosing
    -- Gamma.  In that case Gen(Gamma, tau) leaves the occurrence free: keeping
    -- the renamed, unbounded binder would shadow the enclosing declaration and
    -- make the already-constructed Hyp observe Bottom instead of its bound.
    inheritAmbientConsumerBinder consumerIdentity constructionSourceTy schemeInfo
      | TVarRef sourceRef <- constructionSourceTy
      , envOwnsExactTypeBinderRef enclosingEnv sourceRef
      , let consumerRef =
              schemeConsumerConstructionRef consumerIdentity schemeInfo
      , typeBinderRefsSameIdentity consumerRef sourceRef =
          case
              partition
                (typeBinderRefsSameIdentity sourceRef . fst)
                (schemeBinderRefs (siScheme schemeInfo))
            of
              ([], _) -> pure schemeInfo
              ([(_, Nothing)], inheritedBinders) ->
                pure
                  ( schemeInfoFromRefSubst
                      ( mkElabSchemeWithRefs
                          inheritedBinders
                          (schemeBody (siScheme schemeInfo))
                      )
                      (schemeInfoBinderRefSubst schemeInfo)
                  )
              (matches, _) ->
                Left
                  ( ValidationFailed
                      [ "ambient packet consumer is not one pending binder"
                      , "  consumer: " ++ show consumerIdentity
                      , "  source: " ++ show sourceRef
                      , "  matches: " ++ show matches
                      ]
                  )
      | otherwise = pure schemeInfo

    -- A packet can both be consumed by an enclosing Gamma and own a distinct
    -- root-RaiseMerge consumer at this lambda-body edge.  The latter is the
    -- result abstraction used by T(e), so its bound must be installed from the
    -- recursively checked child before that instantiation is validated.  The
    -- enclosing consumer is handled afterwards and remains a proof about the
    -- completed lambda, not about the lambda body.
    materializeOwnedPacketGamma packet schemeInfo =
      case subtermGeneralizationGammaAuthority packet of
        Just gammaAuthority
          | gpaEdgeId gammaAuthority == bodyEdge -> do
              case map fst
                  ( schemeConsumerBinderCandidates
                      (gpaConsumerIdentity gammaAuthority)
                      schemeInfo
                  ) of
                [gammaRef] ->
                  do
                    materializedSchemeInfo <-
                      fst
                        <$> materializeConsumerBound
                          scopeContext
                          (ConsumerBoundOwnedLocally gammaRef)
                          (gpaConsumerIdentity gammaAuthority)
                          sourceTy
                          schemeInfo
                    pure (materializedSchemeInfo, Nothing)
                [] -> do
                  resolution <-
                    resolvePacketConsumerWithoutLocalBinder
                      gammaAuthority
                      packet
                      schemeInfo
                  case resolution of
                    PacketConsumerInheritedFree _ ->
                      pure (schemeInfo, Just resolution)
                    PacketConsumerEliminatedAtAmbientBound _ _ ->
                      pure (schemeInfo, Just resolution)
                refs ->
                  Left
                    ( ValidationFailed
                        [ "packet-owned local Gamma has multiple construction binders"
                        , "  edge: " ++ show bodyEdge
                        , "  consumer: " ++ show (gpaConsumerIdentity gammaAuthority)
                        , "  binders: " ++ show refs
                        ]
                    )
        _ -> pure (schemeInfo, Nothing)

    resolvePacketConsumerWithoutLocalBinder gammaAuthority packet schemeInfo =
      case inheritedFreeRefs of
        [inheritedRef] ->
          pure (PacketConsumerInheritedFree inheritedRef)
        []
          | not (null currentConsumerOccurrences) ->
              missingConsumer
                [ "the current scheme retains the consumer without exact ambient ownership"
                , "  current consumer occurrences: " ++ show currentConsumerOccurrences
                ]
          | otherwise ->
              eliminatedConsumerAtAmbientBound
        refs ->
          missingConsumer
            [ "the current scheme has multiple exact ambient consumer routes"
            , "  inherited routes: " ++ show refs
            ]
      where
        consumerIdentity = gpaConsumerIdentity gammaAuthority
        constructionConsumerRefs =
          uniqueIdentityRefs
            (schemeConsumerConstructionRefs consumerIdentity schemeInfo)
        currentSchemeRefs =
          freeTypeVarRefsType (schemeToType (siScheme schemeInfo))
        currentConsumerOccurrences =
          [ occurrence
          | occurrence <- currentSchemeRefs
          , any (typeBinderRefsSameIdentity occurrence) constructionConsumerRefs
          ]
        inheritedFreeRefs =
          uniqueIdentityRefs
            [ occurrence
            | occurrence <- currentConsumerOccurrences
            , envOwnsExactTypeBinderRef enclosingEnv occurrence
            ]

        eliminatedConsumerAtAmbientBound = do
          let constructionConsumerRef =
                schemeConsumerConstructionRef consumerIdentity schemeInfo
              completedPacketInfo = subtermGeneralizationSchemeInfo packet
              completedPacketScheme = siScheme completedPacketInfo
              completedConsumerCandidates =
                schemeConsumerBinderCandidates
                  consumerIdentity
                  completedPacketInfo
          (completedConsumerRef, completedBound) <-
            case completedConsumerCandidates of
              [(consumerRef, Just bound)] -> pure (consumerRef, tyToElab bound)
              [(_, Nothing)] ->
                missingConsumer
                  [ "the eliminated packet consumer has no completed bound" ]
              [] ->
                missingConsumer
                  [ "the completed packet has no consumer declaration" ]
              candidates ->
                missingConsumer
                  [ "the completed packet has multiple consumer declarations"
                  , "  declarations: " ++ show candidates
                  ]
          let completedConsumerRefs =
                uniqueIdentityRefs
                  ( completedConsumerRef
                      : schemeConsumerConstructionRefs
                          consumerIdentity
                          completedPacketInfo
                  )
              completedPayloadRefs =
                freeTypeVarRefsType (schemeBody completedPacketScheme)
                  ++ concatMap
                    (maybe [] (freeTypeVarRefsType . tyToElab) . snd)
                    (schemeBinderRefs completedPacketScheme)
              completedConsumerUses =
                [ occurrence
                | occurrence <- completedPayloadRefs
                , any
                    (typeBinderRefsSameIdentity occurrence)
                    completedConsumerRefs
                ]
              completedSchemeWithoutConsumer =
                mkElabSchemeWithRefs
                  [ binder
                  | binder@(ref, _) <- schemeBinderRefs completedPacketScheme
                  , not
                      (typeBinderRefsSameIdentity ref completedConsumerRef)
                  ]
                  (schemeBody completedPacketScheme)
              completedTypeWithoutConsumer =
                schemeToType completedSchemeWithoutConsumer
              currentConstructionType =
                schemeToType (siScheme schemeInfo)
              completedConsumerRouteKeys =
                [ nodeKey
                | (nodeKey, routedRef) <-
                    IntMap.toList
                      (schemeInfoBinderRefSubst completedPacketInfo)
                , typeBinderRefsSameIdentity routedRef completedConsumerRef
                ]
              currentRouteMismatches =
                [ (nodeKey, IntMap.lookup nodeKey (schemeInfoBinderRefSubst schemeInfo))
                | nodeKey <- completedConsumerRouteKeys
                , maybe
                    True
                    (not . typeBinderRefsSameIdentity constructionConsumerRef)
                    (IntMap.lookup nodeKey (schemeInfoBinderRefSubst schemeInfo))
                ]
          unless (null completedConsumerUses) $
            missingConsumer
              [ "the completed packet consumer is not vacuous"
              , "  packet consumer uses: " ++ show completedConsumerUses
              ]
          unless
            (alphaEqType completedTypeWithoutConsumer currentConstructionType)
            ( missingConsumer
                [ "erasing the vacuous completed consumer does not produce the current construction scheme"
                , "  erased completed type: " ++ show completedTypeWithoutConsumer
                , "  current construction type: " ++ show currentConstructionType
                ]
            )
          consumerNode <-
            case typeBinderRefNode (directSchemeConsumerRef consumerIdentity) of
              Just node -> pure node
              Nothing ->
                missingConsumer
                  [ "the packet Gamma consumer has no graph exterior route" ]
          unless
            ( getNodeId consumerNode `elem` completedConsumerRouteKeys
                && null currentRouteMismatches
            )
            ( missingConsumer
                [ "the completed-to-current consumer quotient is incomplete"
                , "  completed consumer: " ++ show completedConsumerRef
                , "  current consumer: " ++ show constructionConsumerRef
                , "  completed route keys: " ++ show completedConsumerRouteKeys
                , "  current route mismatches: " ++ show currentRouteMismatches
                ]
            )
          unless
            (envOwnsExactTypeBinderRef enclosingEnv constructionConsumerRef)
            ( missingConsumer
                [ "the eliminated consumer is only an alias in ambient Gamma"
                , "  construction consumer: " ++ show constructionConsumerRef
                ]
            )
          ambientBound <-
            case
                [ bound
                | (ambientRef, bound) <- Map.toList (envTypeBindings enclosingEnv)
                , typeBinderRefsSameIdentity constructionConsumerRef ambientRef
                ]
            of
              [bound] -> pure bound
              [] ->
                missingConsumer
                  [ "the eliminated consumer has no exact ambient bound"
                  , "  construction consumer: " ++ show constructionConsumerRef
                  ]
              bounds ->
                missingConsumer
                  [ "the eliminated consumer has multiple exact ambient bounds"
                  , "  construction consumer: " ++ show constructionConsumerRef
                  , "  ambient bounds: " ++ show bounds
                  ]
          unless (scopedTypesAgree scopeContext completedBound ambientBound) $
            missingConsumer
              [ "the completed packet bound disagrees with ambient Gamma"
              , "  completed packet bound: " ++ show completedBound
              , "  ambient bound: " ++ show ambientBound
              ]
          unless (sourceTy == ambientBound || alphaEqType sourceTy ambientBound) $
            missingConsumer
              [ "the recursively checked source cannot construct the eliminated consumer"
              , "  source type: " ++ show sourceTy
              , "  ambient bound: " ++ show ambientBound
              ]
          pure
            ( PacketConsumerEliminatedAtAmbientBound
                constructionConsumerRef
                ambientBound
            )

        missingConsumer :: [String] -> Either ElabError a
        missingConsumer details =
          Left
            ( ValidationFailed
                ( [ "packet-owned local Gamma has no construction binder or valid elimination proof"
                  , "  edge: " ++ show bodyEdge
                  , "  consumer: " ++ show consumerIdentity
                  , "  scheme: " ++ show (siScheme schemeInfo)
                  , "  scheme substitution: " ++ show (schemeInfoBinderRefSubst schemeInfo)
                  , "  packet scheme: " ++ show (siScheme (subtermGeneralizationSchemeInfo packet))
                  , "  construction consumer refs: " ++ show constructionConsumerRefs
                  , "  enclosing free binders: " ++ show enclosingFreeBinderRefs
                  ]
                    ++ details
                )
            )

    uniqueIdentityRefs = foldr insertUnique []
      where
        insertUnique ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

    EnvFreeTypeBinderRefs enclosingFreeBinderRefs =
      envFreeTypeBinderRefs enclosingEnv

-- | Select the one packet whose recorded consumer is owned by this exact
-- lambda-body construction.  Edge equality alone is insufficient: a nested
-- recursive helper can expose a descendant packet at its parent's body while
-- retaining the descendant lambda as its lexical owner.  Conversely, owner
-- equality alone can select a different edge of the same lambda.  Require both
-- pieces of identity-bearing authority before expected-endpoint selection or
-- a terminal Hyp may use the packet.
selectExactLambdaBodyPacket
  :: LocalGammaOwner
  -> EdgeId
  -> [Maybe PreparedSubtermGeneralization]
  -> Either ElabError (Maybe PreparedSubtermGeneralization)
selectExactLambdaBodyPacket owner edgeId candidatePackets =
  case
      selectUniqueCandidateBy
        (==)
        (filter ownsExactBodyConstruction (catMaybes candidatePackets))
    of
      NoCandidateSelection -> pure Nothing
      UniqueCandidateSelection packet -> pure (Just packet)
      AmbiguousCandidateSelection ->
        Left
          ( ValidationFailed
              [ "lambda body has multiple exact construction packets"
              , "  owner: " ++ show owner
              , "  edge: " ++ show edgeId
              ]
          )
  where
    ownsExactBodyConstruction packet =
      case subtermGeneralizationConsumerAuthority packet of
        Just authority ->
          scaEdgeId authority == edgeId
            && subtermConsumerAuthorityEnclosingOwner authority == Just owner
        Nothing -> False

-- | Identity-bearing evidence that an application constructs the declared
-- bound of an active packet consumer.  Keep the ref and bound together: the
-- ref routes the enclosing Hyp, while the bound is the application's exact
-- result endpoint before that Hyp is emitted.
data ActiveApplicationConsumerAuthority = ActiveApplicationConsumerAuthority
  { activeApplicationConsumerRef :: TypeBinderRef,
    activeApplicationConsumerBound :: ElabType
  }
  deriving (Show)

-- | Prove that an application is the exact source boundary for the completed
-- bound of an active packet-owned Gamma consumer.  Figure 15.3.5 constructs
-- that bound as @Lambda(Gamma_g)(a1 a2)@ before the enclosing edge discharges
-- the consumer with Hyp.  The returned certificate is deliberately not a
-- graph target: its ref routes the later Hyp and its declared bound determines
-- the application result by construction.
--
-- The exact lexical owner and either the completed packet scheme or its
-- already-open ambient bound are the authority for this choice.  The complete
-- scheme case is the boundary that will emit the binder itself; the bound case
-- requires that binder to be ambient already.  In particular, nested
-- application children merely inherit the active packet; their different
-- expected endpoint cannot select this target.
activeApplicationConsumerSchemeAuthority
  :: ScopeContext p
  -> Env
  -> NodeRef
  -> Either ElabError (Maybe ActiveApplicationConsumerAuthority)
activeApplicationConsumerSchemeAuthority scopeContext env applicationScope =
  case
      ( envActiveSubtermConstruction env
      , envExpectedTermEndpoint env >>= exactConstructionExpectedType
      )
    of
    (Just packet, Just expectedTy)
      | activeConsumerOwnsConstruction packet applicationScope
      , Just graphRef <- subtermGeneralizationResultAbstractionRef packet ->
          let constructionRenames = envConstructionBinderRenames env
              constructionRefs =
                uniqueIdentityRefs
                  ( map
                      (applyRefRenames constructionRenames)
                      ( graphRef
                          : maybeToList
                            (subtermGeneralizationConstructionResultAbstractionRef packet)
                      )
                  )
              completedPacketInfo =
                applySchemeInfoRefRenames
                  constructionRenames
                  (subtermGeneralizationSchemeInfo packet)
              completedPacketTy =
                schemeToType (siScheme completedPacketInfo)
              expectedIsCompletedPacket =
                scopedEndpointTypesAgree
                  scopeContext
                  expectedTy
                  completedPacketTy
              ambientRefs = envGeneralizationAmbientTypeBinderRefs env
              matchingConsumerBinders =
                [ (constructionRef, constructionBound)
                | (constructionRef, Just packetBound) <-
                    schemeBinderRefs (siScheme completedPacketInfo)
                , any
                    (typeBinderRefsSameIdentity constructionRef)
                    constructionRefs
                , let packetBoundTy = tyToElab packetBound
                      constructionBound =
                        case
                            find
                              ( typeBinderRefsSameIdentity constructionRef
                                  . fst
                              )
                              (Map.toList (envSourceRefinedGammaBounds env))
                          of
                            Just (_, refinedBound)
                              | isExactSchemeProjection
                                  refinedBound
                                  packetBoundTy ->
                                  refinedBound
                            _ -> packetBoundTy
                , expectedIsCompletedPacket
                    || scopedEndpointTypesAgree
                      scopeContext
                      expectedTy
                      constructionBound
                , expectedIsCompletedPacket
                    || any
                      (typeBinderRefsSameIdentity constructionRef)
                      ambientRefs
                ]
           in case matchingConsumerBinders of
                (constructionRef, constructionBound) : rest
                  | all
                      ( \(otherRef, otherBound) ->
                          typeBinderRefsSameIdentity constructionRef otherRef
                            && scopedEndpointTypesAgree
                              scopeContext
                              constructionBound
                              otherBound
                      )
                      rest ->
                      -- This exact packet binder is only an ownership
                      -- certificate.  Its quotient representative is routing
                      -- data for the enclosing Hyp, not the application node
                      -- to generalize.  Returning the identity-bearing ref
                      -- prevents the application boundary from selecting or
                      -- emitting a canonical representative by accident.
                      pure
                        ( Just
                            ActiveApplicationConsumerAuthority
                              { activeApplicationConsumerRef = constructionRef,
                                activeApplicationConsumerBound = constructionBound
                              }
                        )
                _ -> pure Nothing
    _ -> pure Nothing
  where
    -- Packet-owned Gamma has the strongest lexical owner.  In its absence,
    -- only the two consumer authorities that construct their own result
    -- endpoint may select S': topology carries an exact local owner, while a
    -- root consumer is scoped by the active construction environment itself.
    activeConsumerOwnsConstruction packet scope =
      case subtermGeneralizationGammaAuthority packet of
        -- The active environment carries this packet only along the recursive
        -- source-construction path.  The Gamma certificate itself is the
        -- authority; its owner gen names where the pending slot was created,
        -- not necessarily the descendant application gen that fills it.
        Just _ -> True
        Nothing ->
          case subtermGeneralizationConsumerAuthority packet of
            Just authority
              | subtermConsumerAuthorityIsTopology authority ->
                  maybe
                    False
                    ((== scope) . localGammaOwnerScope)
                    (subtermConsumerAuthorityEnclosingOwner authority)
              | subtermConsumerAuthorityIsRootGamma authority -> True
            _ -> False

    uniqueIdentityRefs = foldr insertUnique []
      where
        insertUnique ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

consumerBoundOwnership
  :: Env
  -> LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> EdgeId
  -> TypeBinderIdentity
  -> SchemeInfo
  -> Either ElabError ConsumerBoundOwnership
consumerBoundOwnership enclosingEnv owner packet bodyEdge consumerIdentity schemeInfo =
  case subtermGeneralizationConsumerAuthority packet of
    Nothing -> missingOwner
    Just authority
      | scaConsumerIdentity authority /= consumerIdentity ->
          Left
            ( ValidationFailed
                [ "prepared lambda packet consumer authority changed identity"
                , "  packet consumer: " ++ show consumerIdentity
                , "  authority consumer: " ++ show (scaConsumerIdentity authority)
                ]
            )
      | Just enclosingOwner <-
          subtermConsumerAuthorityEnclosingOwner authority ->
          if subtermConsumerAuthorityIsTopology authority
            then topologyOwnership
            else
              case
                  subtermGeneralizationLocalConsumerClosure
                    (envLocalGammaClosures enclosingEnv)
                    packet
                of
                  Just closure
                    | enclosingOwner == lgcOwner closure ->
                        enclosingClosureOwnership
                  _ -> missingOwner
      | subtermConsumerAuthorityIsRootGamma authority ->
          rootGammaOwnership
      | scaEdgeId authority == bodyEdge
      , subtermGeneralizationOwnsGammaForEdge bodyEdge packet ->
          packetOwnership
      | otherwise -> missingOwner
  where
    localCandidates =
      schemeConsumerBinderCandidates consumerIdentity schemeInfo
    directConsumerRef =
      typeBinderRefFromIdentity consumerIdentity "$consumer"
    packetOwnership =
      case localCandidates of
        [(localRef, _)] -> pure (ConsumerBoundOwnedLocally localRef)
        [] -> pure (ConsumerBoundOwnedByRequiredGamma bodyEdge)
        _ ->
          Left
            ( ValidationFailed
                [ "prepared lambda packet binds its consumer identity more than once"
                , "  consumer: " ++ show consumerIdentity
                , "  matches: " ++ show localCandidates
                ]
            )

    topologyOwnership =
      case localCandidates of
        [(localRef, _)] ->
          pure (ConsumerBoundOwnedLocally localRef)
        [] ->
          Left
            ( ValidationFailed
                [ "topology consumer has no construction-time local binder"
                , "  consumer: " ++ show consumerIdentity
                , "  edge: " ++ show bodyEdge
                , "  scheme: " ++ show (siScheme schemeInfo)
                , "  scheme substitution: "
                    ++ show (schemeInfoBinderRefSubst schemeInfo)
                ]
            )
        _ ->
          Left
            ( ValidationFailed
                [ "topology consumer occurs more than once in its prepared construction scheme"
                , "  consumer: " ++ show consumerIdentity
                , "  matches: " ++ show localCandidates
                ]
            )

    rootGammaOwnership =
      case localCandidates of
        [] ->
          pure (ConsumerBoundOwnedByRootGamma directConsumerRef)
        _ ->
          Left
            ( ValidationFailed
                [ "root-owned consumer unexpectedly occurs in its local construction scheme"
                , "  consumer: " ++ show consumerIdentity
                , "  matches: " ++ show localCandidates
                ]
            )

    enclosingClosureOwnership =
      let expectedBound =
            schemeToType
              (subtermGeneralizationGammaBoundScheme packet)
          enclosingRef =
            typeBinderRefFromIdentity
              consumerIdentity
              "$consumer"
       in pure
            ( ConsumerBoundOwnedByEnclosingGamma
                enclosingRef
                expectedBound
            )

    missingOwner =
      Left
        ( ValidationFailed
            [ "lambda consumer has neither a local, enclosing, nor required Gamma owner"
            , "  consumer: " ++ show consumerIdentity
            , "  body edge: " ++ show bodyEdge
            , "  owner: " ++ show owner
            , "  consumer authority: "
                ++ show (subtermGeneralizationConsumerAuthority packet)
            , "  body-edge closure: "
                ++ show
                  ( IntMap.lookup
                      (getEdgeId bodyEdge)
                      (envLocalGammaClosures enclosingEnv)
                  )
            ]
        )

-- | Select the outward identity owned by one Gamma construction.  Structural
-- self/result identities belong to their enclosing mu/forall reconstruction;
-- they can occur in a generalized substitution, but they are never the
-- flexible exterior introduced by Figure 15.3.5.  Preserve the frozen graph
-- exterior in that case so two recursive occurrences cannot collapse onto one
-- process-wide structural identity.
gammaConsumerRefForRoute
  :: RequiredGammaBinder
  -> TypeBinderRef
  -> TypeBinderRef
gammaConsumerRefForRoute requirement routedRef =
  case typeBinderIdentityStructural (typeBinderRefIdentity routedRef) of
    Just _ ->
      typeBinderRefFromIdentity
        (typeBinderIdentityFromNode (rgbExteriorNode requirement))
        (typeBinderIdentityStableName (typeBinderIdentityFromNode (rgbExteriorNode requirement)))
    Nothing -> routedRef

-- | Retain every graph-local route named by an explicit Γ requirement.  The
-- generalized scheme can publish the result root under a different outward
-- binder, but Ω replay can still name either side of the terminal RaiseMerge.
-- The operated, exterior, and result routes are therefore one prepared Gamma
-- obligation and must enter scope together before either application child is
-- elaborated.
constructionGammaAliases
  :: (NodeId -> [NodeId])
  -> IntMap.IntMap TypeBinderRef
  -> GeneralizationRequirements
  -> ConstructionGammaPlan
  -> SchemeInfo
  -> Either ElabError ConstructionGammaAliases
constructionGammaAliases constructionRouteNodes constructionSourceRoutes requirements constructionPlan schemeInfo = do
  routedPairs <- concat <$> traverse aliasesFor requiredBinders
  routingAliases <- foldM insertAlias IntMap.empty routedPairs
  authorityPairs <- concat <$> traverse authorityAliasesFor requiredBinders
  authorityAliases <- foldM insertAlias IntMap.empty authorityPairs
  pure
    ConstructionGammaAliases
      { cgaRoutingAliases = routingAliases
      , cgaAuthorityAliases = authorityAliases
      }
  where
    requiredBinders = grRequiredGammaBinders requirements
    subst = schemeInfoBinderRefSubst schemeInfo
    constructionBinders = cgpBinders constructionPlan
    ambientAliases = cgpAmbientAliases constructionPlan

    aliasesFor requirement = do
      outwardRef <-
        case emittedExteriorRefs requirement of
          [] ->
            case ambientRequirementRefs requirement of
              [] -> preparedOutwardRef requirement
              [ambientRef] -> pure ambientRef
              ambientRefs ->
                Left
                  ( ValidationFailed
                      [ "construction Gamma requirement routes to multiple exact ambient declarations"
                      , "  requirement: " ++ show requirement
                      , "  ambient declarations: " ++ show ambientRefs
                      ]
                  )
          [emittedRef] -> pure emittedRef
          emittedRefs ->
            Left
              ( ValidationFailed
                  [ "construction Gamma emits one exterior under multiple binder identities"
                  , "  requirement: " ++ show requirement
                  , "  emitted binders: " ++ show emittedRefs
                  ]
              )
      pure
        (requirementRoutingAliasPairs requirement outwardRef)

    authorityAliasesFor requirement =
      case emittedExteriorRefs requirement of
        [emittedRef] ->
          pure (requirementAuthorityAliasPairs requirement emittedRef)
        [] ->
          case ambientRequirementRefs requirement of
            [ambientRef] ->
              pure (requirementAuthorityAliasPairs requirement ambientRef)
            [] -> pure []
            ambientRefs ->
              Left
                ( ValidationFailed
                    [ "construction Gamma authority has multiple exact ambient declarations"
                    , "  requirement: " ++ show requirement
                    , "  ambient declarations: " ++ show ambientRefs
                    ]
                )
        emittedRefs ->
          Left
            ( ValidationFailed
                [ "construction Gamma authority has multiple emitted exterior binders"
                , "  requirement: " ++ show requirement
                , "  emitted binders: " ++ show emittedRefs
                ]
            )

    requirementRoutingAliasPairs requirement outwardRef =
      filter
        ( \pair ->
            IntSet.notMember
              (fst pair)
              (structuredOperatedDependencyKeys requirement)
        )
        ( requirementAuthorityAliasPairs requirement outwardRef
            ++ [ (getNodeId operatedNode, outwardRef)
               | operatedRef <-
                  maybeToList (rgbExactOperatedOccurrenceRef requirement)
               , Just operatedNode <- [typeBinderRefNode operatedRef]
               ]
            ++ [ (getNodeId sourceNode, outwardRef)
               | routeNode <- requirementRouteNodes requirement
               , Just sourceRef <-
                  [ IntMap.lookup
                      (getNodeId routeNode)
                      constructionSourceRoutes
                  ]
               , Just sourceNode <- [typeBinderRefNode sourceRef]
               ]
            ++ [ (getNodeId operatedNode, outwardRef)
               | operatedRef <-
                  freeTypeVarRefsType (rgbOperatedType requirement)
               , any
                  (typeBinderRefsSameIdentity operatedRef)
                  (IntMap.elems constructionSourceRoutes)
               , Just operatedNode <- [typeBinderRefNode operatedRef]
               ]
        )

    -- A structured S'(operated) may mention graph variables that are
    -- dependencies of the flexible exterior.  Routing one of those nodes to
    -- the exterior would construct an illegal self-bound and collapse two
    -- declarations (for example @forall f. forall c > f Int. ...@ into two
    -- declarations of @c@).  The separate authority map still retains the
    -- requirement topology; only the occurrence-routing map excludes these
    -- dependency nodes.  A bare variable endpoint remains the exact consumer
    -- capability and is routed normally.
    structuredOperatedDependencyKeys requirement =
      case rgbOperatedType requirement of
        TVarRef _ -> IntSet.empty
        operatedType ->
          IntSet.fromList
            [ getNodeId node
            | ref <- freeTypeVarRefsType operatedType
            , Just node <- [typeBinderRefNode ref]
            ]

    requirementAuthorityAliasPairs requirement outwardRef =
      [ (getNodeId node, outwardRef)
      | directNode <- directRequirementNodes requirement
      , node <- directNode : constructionRouteNodes directNode
      ]

    requirementRouteNodes requirement =
      concatMap
        (\directNode -> directNode : constructionRouteNodes directNode)
        (directRequirementNodes requirement)

    directRequirementNodes requirement =
      rgbOperatedRoot requirement
        : rgbExteriorNode requirement
        : NonEmpty.toList (rgbResultRoots requirement)

    preparedOutwardRef requirement =
      case
          foldr
            (\resultRoot fallback ->
              IntMap.lookup (getNodeId resultRoot) subst <|> fallback
            )
            (IntMap.lookup (getNodeId (rgbExteriorNode requirement)) subst)
            (rgbResultRoots requirement)
        of
          Just ref -> pure (gammaConsumerRefForRoute requirement ref)
          Nothing ->
            Left
              ( ValidationFailed
                  [ "construction Γ aliases have no prepared outward binder"
                  , "  requirement: " ++ show requirement
                  , "  substitution: " ++ show subst
                  , "  ambient routes: " ++ show ambientAliases
                  ]
              )

    ambientRequirementRefs requirement =
      foldr insertDistinctRef []
        [ ambientRef
        | node <-
            rgbOperatedRoot requirement
              : rgbExteriorNode requirement
              : NonEmpty.toList (rgbResultRoots requirement)
        , Just ambientRef <-
            [IntMap.lookup (getNodeId node) ambientAliases]
        ]

    emittedExteriorRefs requirement =
      foldr insertDistinctRef []
        [ ref
        | (ref, _) <- constructionBinders
        , typeBinderRefNode ref == Just (rgbExteriorNode requirement)
        ]

    insertDistinctRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    insertAlias aliases (nodeKey, outwardRef) =
      case IntMap.lookup nodeKey aliases of
        Nothing -> pure (IntMap.insert nodeKey outwardRef aliases)
        Just existing
          | typeBinderRefsSameIdentity existing outwardRef -> pure aliases
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "construction Γ routes disagree on their outward binder"
                    , "  graph node: " ++ show (NodeId nodeKey)
                    , "  first binder: " ++ show existing
                    , "  second binder: " ++ show outwardRef
                    ]
                )

-- | Carry only the aliases whose outward identity is published by this
-- scheme, replacing their presentation with the scheme's authoritative ref.
-- A surrounding let construction can contain aliases for sibling packets;
-- those are intentionally left with their owners.
alignConstructionAliasesToScheme
  :: IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
alignConstructionAliasesToScheme aliases schemeInfo =
  foldM alignOne (schemeInfoBinderRefSubst schemeInfo) (IntMap.toList aliases)
  where
    schemeRefs = map fst (schemeBinderRefs (siScheme schemeInfo))
    substitutionRefs = IntMap.elems (schemeInfoBinderRefSubst schemeInfo)
    authoritativeRefs = schemeRefs ++ substitutionRefs

    alignOne subst (nodeKey, aliasRef) =
      case filter (typeBinderRefsSameIdentity aliasRef) authoritativeRefs of
        [] -> pure subst
        schemeRef : _ ->
          case IntMap.lookup nodeKey subst of
            Nothing -> pure (IntMap.insert nodeKey schemeRef subst)
            Just existing
              | typeBinderRefsSameIdentity existing schemeRef -> pure subst
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "construction Gamma alias conflicts with the exact owner substitution"
                        , "  graph node: " ++ show (NodeId nodeKey)
                        , "  existing ref: " ++ show existing
                        , "  exact owner ref: " ++ show schemeRef
                        ]
                    )

-- | Select the edge-local RaiseMerge authorities that actually belong to the
-- current application's Γ.  The frozen binding tree owns placement: ordinary
-- named nodes are direct flexible children of the gen, while the structural-
-- recursive extension can retain the exterior below an entirely flexible
-- mu/alias shell.  In both cases the nearest gen owns the Hyp binder.
applicationRequirementEdges
  :: ScopeContext p
  -> EdgeArtifacts
  -> IntMap.IntMap ElabType
  -> (AnnExpr -> Either ElabError (Maybe SchemeInfo))
  -> EdgeId
  -> NodeId
  -> AnnExpr
  -> Either ElabError [(EdgeId, Maybe ElabType)]
applicationRequirementEdges scopeContext edgeArtifacts exactProducerTypes sourceSchemeFor boundaryEdge applicationNode applicationAnn = do
  applicationScope <-
    scopeRootForBoundary scopeContext boundaryEdge applicationNode
  ownedRequirementEdgesForOwnerWithSources
    scopeContext
    edgeArtifacts
    exactProducerTypes
    sourceSchemeFor
    ( LocalGammaOwner
        { lgoConstructor = LocalApplicationGamma,
          lgoBoundaryEdge = boundaryEdge,
          lgoTermNode = applicationNode,
          lgoScope = applicationScope
        }
    )
    applicationAnn

-- | Select application obligations together with the source expression that
-- produced each edge.  A source scheme recovered from resolved identities is
-- authoritative when the frozen expansion root is an occurrence variable,
-- including one whose graph-local lower bound has already been installed: it
-- is the bottom-up @S'(operated)@ used to construct the
-- root-RaiseMerge Gamma binder, not a later transport repair.
ownedRequirementEdgesForOwnerWithSources
  :: ScopeContext p
  -> EdgeArtifacts
  -> IntMap.IntMap ElabType
  -> (AnnExpr -> Either ElabError (Maybe SchemeInfo))
  -> LocalGammaOwner
  -> AnnExpr
  -> Either ElabError [(EdgeId, Maybe ElabType)]
ownedRequirementEdgesForOwnerWithSources scopeContext edgeArtifacts exactProducerTypes sourceSchemeFor owner ann = do
  ownedSources <-
    localOwnerInstantiationEdgeSources
      scopeContext
      edgeArtifacts
      owner
      ann
  owned <-
    requirementEdgesForSources
      scopeContext
      edgeArtifacts
      exactProducerTypes
      sourceSchemeFor
      ownedSources
  pure
    ( IntMap.elems
        ( IntMap.fromList
            [ (getEdgeId edgeId, requirement)
            | requirement@(edgeId, _) <- owned
            ]
        )
    )

-- | Attach the exact operated endpoint, when source construction is the only
-- authority that can provide it, without making any ownership decision for
-- the edge.  Local-owner selection and edge-local Figure 15.3.5 preparation
-- deliberately share this projection so they cannot disagree about
-- @S'(operated)@ while retaining distinct placement proofs.
requirementEdgesForSources
  :: ScopeContext p
  -> EdgeArtifacts
  -> IntMap.IntMap ElabType
  -> (AnnExpr -> Either ElabError (Maybe SchemeInfo))
  -> [(EdgeId, AnnExpr)]
  -> Either ElabError [(EdgeId, Maybe ElabType)]
requirementEdgesForSources scopeContext edgeArtifacts exactProducerTypes sourceSchemeFor =
  traverse edgeRequirement
  where
    edgeRequirement (edgeId, sourceAnn) = do
      sourceType <- sourceTypeForOperatedOccurrence edgeId sourceAnn
      pure
        ( edgeId,
          sourceType
            <|> IntMap.lookup (getEdgeId edgeId) exactProducerTypes
        )

    sourceTypeForOperatedOccurrence edgeId sourceAnn = do
      authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
      case authority of
        Just rootAuthority
          | frozenOperatedRootIsOccurrence rootAuthority ->
              fmap (schemeToType . siScheme) <$> sourceSchemeFor sourceAnn
        _ -> pure Nothing

    -- A resolved producer scheme is the exact type constructed by an
    -- occurrence variable, even when constraint generation has already put a
    -- graph-local lower bound on that occurrence.  The lower bound can retain
    -- flexible/structural graph aliases that are semantically equivalent but
    -- are not the xMLF endpoint accepted by Inst-Hyp.  Carry the checked
    -- source scheme into Gamma construction so the Hyp is well-typed by
    -- construction.  A structural root (arrow, forall, mu, ...) remains its
    -- own S'(operated) authority and must not be replaced by an enclosing
    -- source scheme.
    frozenOperatedRootIsOccurrence rootAuthority =
      case
          lookupNodeIn
            (cNodes (gaBaseConstraint (scGaParents scopeContext)))
            (rrmaOperatedRoot rootAuthority)
        of
          Just TyVar {} -> True
          _ -> False

-- | Select the ordinary Gamma_g edge set owned by one exact source
-- constructor.  The shared typed selector reserves sources named directly by
-- an AApp frame for its edge-local lane; every remaining edge uses the nearest
-- constructor whose frozen scope flexibly owns the exterior.  Scope equality
-- alone is insufficient because nested lambda/application/let nodes may share
-- one gen while constructing distinct local Gammas.
localOwnerInstantiationEdgeSources
  :: ScopeContext p
  -> EdgeArtifacts
  -> LocalGammaOwner
  -> AnnExpr
  -> Either ElabError [(EdgeId, AnnExpr)]
localOwnerInstantiationEdgeSources scopeContext edgeArtifacts expectedOwner ann = do
  directApplicationOwners <-
    localGammaDirectApplicationEdgeOwners
      (scopeRootForBoundary scopeContext)
      ann
  go directApplicationOwners [] ann
  where
    go directApplicationOwners localOwners expr = do
      frame <-
        localGammaFrame
          (scopeRootForBoundary scopeContext)
          expr
      let localOwners' =
            maybe localOwners (: localOwners) (lgfOwner frame)
      direct <-
        fmap concat
          ( traverse
              (claimedBy directApplicationOwners localOwners')
              (lgfDirectEdgeSources frame)
          )
      descendants <-
        fmap concat
          ( traverse
              (go directApplicationOwners localOwners')
              (lgfChildren frame)
          )
      pure (direct ++ descendants)

    claimedBy directApplicationOwners localOwners edgeSource@(edgeId, _) = do
      authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
      pure
        [ edgeSource
        | Just rootAuthority <- [authority]
        , Just (FlexibleExteriorEdgeOwnership owner) <-
            [ selectLocalGammaEdgeOwnership
                directApplicationOwners
                edgeId
                localOwners
                (ownsExterior rootAuthority)
            ]
        , owner == expectedOwner
        ]

    ownsExterior rootAuthority owner =
      rootRaiseMergeExteriorOwnedByScope
        (scGaParents scopeContext)
        (localGammaOwnerScope owner)
        (rrmaExterior rootAuthority)

ownedRequirementEdgesForOwner
  :: ScopeContext p
  -> EdgeArtifacts
  -> IntMap.IntMap ElabType
  -> LocalGammaOwner
  -> AnnExpr
  -> Either ElabError [(EdgeId, Maybe ElabType)]
ownedRequirementEdgesForOwner scopeContext edgeArtifacts exactProducerTypes owner ann = do
  ownedSources <-
    localOwnerInstantiationEdgeSources
      scopeContext
      edgeArtifacts
      owner
      ann
  pure
    ( IntMap.elems
        ( IntMap.fromList
            [ ( getEdgeId edgeId,
                ( edgeId,
                  IntMap.lookup (getEdgeId edgeId) exactProducerTypes
                )
              )
            | (edgeId, _) <- ownedSources
            ]
        )
    )

-- | Publish direct-edge ownership only after the application has constructed
-- and typechecked its complete Gamma.  The annotated application frame is the
-- occurrence authority for the edge set; the typed requirement supplies the
-- exact exterior/operated/result routes; and the final construction supplies
-- either the one local binder identity reached by every route or the one
-- pre-existing ambient identity/bound selected by its authority aliases.
--
-- Keeping this as a checked constructor prevents a later root planner from
-- turning "this application has some local binder" into ownership of an
-- unrelated or only partially overlapping requirement.
buildDirectApplicationGammaClaims
  :: ScopeContext p
  -> LocalGammaOwner
  -> AnnExpr
  -> [RequiredGammaBinder]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> Map.Map TypeBinderRef ElabType
  -> EnvFreeTypeBinderRefs
  -> Either
      ElabError
      ( NonEmpty EdgeId,
        [DirectApplicationGammaClaim],
        [DirectApplicationAmbientGammaClaim]
      )
buildDirectApplicationGammaClaims scopeContext owner ann requirements constructionBinders routes ambientRoutes ambientBindings (EnvFreeTypeBinderRefs ambientFreeRefs) = do
  frame <-
    localGammaFrame
      (scopeRootForBoundary scopeContext)
      ann
  unless
    (lgfOwner frame == Just owner)
    ( claimFailure
        [ "annotated application frame disagrees with the certificate owner"
        , "  frame owner: " ++ show (lgfOwner frame)
        ]
    )
  unless
    (lgoConstructor owner == LocalApplicationGamma)
    (claimFailure ["certificate owner is not an application"])
  directSourceEdges <-
    case NonEmpty.nonEmpty (map fst (lgfDirectEdgeSources frame)) of
      Just edgeIds -> pure edgeIds
      Nothing ->
        claimFailure
          ["application frame has no direct source edge"]
  let directEdgeKeys =
        IntSet.fromList
          (map getEdgeId (NonEmpty.toList directSourceEdges))
  unless
    (IntSet.size directEdgeKeys == NonEmpty.length directSourceEdges)
    ( claimFailure
        [ "application frame repeats one direct source edge"
        , "  direct edges: " ++ show directSourceEdges
        ]
    )
  claimResults <-
    traverse (buildClaim directEdgeKeys) requirements
  let claims = mapMaybe fst claimResults
      ambientClaims = mapMaybe snd claimResults
      allClaimEdgeSets =
        map dagcEdgeIds claims
          ++ map daagcEdgeIds ambientClaims
  unless
    (null (overlappingClaimEdgeSets allClaimEdgeSets))
    ( claimFailure
        [ "direct requirements overlap one source edge"
        , "  overlapping claim edges: "
            ++ show (overlappingClaimEdgeSets allClaimEdgeSets)
        ]
    )
  pure (directSourceEdges, claims, ambientClaims)
  where
    buildClaim directEdgeKeys requirement = do
      let claimEdgeIds = rgbEdgeIds requirement
          claimEdgeKeys =
            IntSet.fromList
              (map getEdgeId (NonEmpty.toList claimEdgeIds))
          routeNodes =
            rgbExteriorNode requirement
              : rgbOperatedRoot requirement
              : NonEmpty.toList (rgbResultRoots requirement)
          routedRefs =
            [ routedRef
            | node <- routeNodes
            , Just routedRef <-
                [IntMap.lookup (getNodeId node) routes]
            ]
      unless
        ( IntSet.size claimEdgeKeys == NonEmpty.length claimEdgeIds
            && claimEdgeKeys `IntSet.isSubsetOf` directEdgeKeys
        )
        ( claimFailure
            [ "requirement is not a complete subset of the application's direct source edges"
            , "  requirement: " ++ show requirement
            , "  direct edges: "
                ++ show (map EdgeId (IntSet.toList directEdgeKeys))
            ]
        )
      mbRoutedRef <-
        case routedRefs of
          [] -> pure Nothing
          firstRef : remainingRefs
            | length routedRefs == length routeNodes
            , all (typeBinderRefsSameIdentity firstRef) remainingRefs ->
                pure (Just firstRef)
          _ ->
            claimFailure
              [ "requirement endpoints do not all route to one constructed binder"
              , "  requirement: " ++ show requirement
              , "  routes: " ++ show routes
              ]
      case mbRoutedRef of
        Nothing -> do
          (ambientRef, ambientBound) <-
            exactAmbientAuthority requirement routeNodes
          -- Publish positive zero-local evidence.  Later root preparation
          -- must not infer ambient ownership merely from a missing route.
          pure
            ( Nothing
            , Just
                DirectApplicationAmbientGammaClaim
                  { daagcEdgeIds = claimEdgeIds
                  , daagcExteriorNode = rgbExteriorNode requirement
                  , daagcOperatedRoot = rgbOperatedRoot requirement
                  , daagcConstructionResultRoots =
                      rgbResultRoots requirement
                  , daagcOperatedType = rgbOperatedType requirement
                  , daagcAmbientRef = ambientRef
                  , daagcAmbientBound = ambientBound
                  }
            )
        Just routedRef -> do
          constructedBound <-
            case
                [ mbBound
                | (constructionRef, mbBound) <- constructionBinders
                , typeBinderRefsSameIdentity constructionRef routedRef
                ]
              of
                [mbBound] -> pure mbBound
                matches ->
                  claimFailure
                    [ "routed requirement does not name exactly one construction binder"
                    , "  requirement: " ++ show requirement
                    , "  routed ref: " ++ show routedRef
                    , "  matching bounds: " ++ show matches
                    ]
          unless
            (boundsAgree constructedBound (rgbOperatedType requirement))
            ( claimFailure
                [ "constructed binder bound disagrees with S(operated)"
                , "  requirement: " ++ show requirement
                , "  constructed bound: " ++ show constructedBound
                ]
            )
          pure
            ( Just
                DirectApplicationGammaClaim
                  { dagcEdgeIds = claimEdgeIds
                  , dagcExteriorNode = rgbExteriorNode requirement
                  , dagcOperatedRoot = rgbOperatedRoot requirement
                  , dagcConstructionResultRoots = rgbResultRoots requirement
                  , dagcOperatedType = rgbOperatedType requirement
                  , dagcBinderRef = routedRef
                  , dagcConstructedBound = constructedBound
                  }
            , Nothing
            )

    exactAmbientAuthority requirement routeNodes = do
      routedAmbientRefs <-
        traverse
          ( \node ->
              case IntMap.lookup (getNodeId node) ambientRoutes of
                Just ref -> pure ref
                Nothing ->
                  claimFailure
                    [ "zero-local requirement has no exact ambient route"
                    , "  requirement: " ++ show requirement
                    , "  missing route node: " ++ show node
                    , "  ambient routes: " ++ show ambientRoutes
                    ]
          )
          routeNodes
      ambientRef <-
        case routedAmbientRefs of
          firstRef : remainingRefs
            | all
                (typeBinderRefsSameIdentity firstRef)
                remainingRefs ->
                pure firstRef
          _ ->
            claimFailure
              [ "zero-local requirement routes to multiple ambient declarations"
              , "  requirement: " ++ show requirement
              , "  routed ambient refs: " ++ show routedAmbientRefs
              ]
      when
        ( any
            (typeBinderRefsSameIdentity ambientRef . fst)
            constructionBinders
        )
        ( claimFailure
            [ "zero-local requirement names an application-local binder"
            , "  requirement: " ++ show requirement
            , "  ambient ref: " ++ show ambientRef
            ]
        )
      let matchingExplicitBounds =
            [ bound
            | (boundRef, bound) <- Map.toList ambientBindings
            , typeBinderRefsSameIdentity boundRef ambientRef
            ]
          matchingFreeRefs =
            foldr insertUniqueRef []
              [ freeRef
              | freeRef <- ambientFreeRefs
              , typeBinderRefsSameIdentity freeRef ambientRef
              ]
      ambientBound <-
        case (matchingExplicitBounds, matchingFreeRefs) of
            ([bound], _) -> pure bound
            ([], [_]) -> pure TBottom
            _ ->
              claimFailure
                [ "zero-local requirement has no unique exact ambient declaration"
                , "  requirement: " ++ show requirement
                , "  ambient ref: " ++ show ambientRef
                , "  matching explicit bounds: "
                    ++ show matchingExplicitBounds
                , "  matching free refs: " ++ show matchingFreeRefs
                ]
      unless
        (ambientBoundSatisfies ambientRef ambientBound (rgbOperatedType requirement))
        ( claimFailure
            [ "zero-local ambient declaration disagrees with S(operated)"
            , "  requirement: " ++ show requirement
            , "  ambient ref: " ++ show ambientRef
            , "  ambient bound: " ++ show ambientBound
            ]
          )
      pure (ambientRef, ambientBound)
      where
        insertUniqueRef ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

    ambientBoundSatisfies ambientRef ambientBound operatedType =
      case operatedType of
        TVarRef operatedRef
          | typeBinderRefsSameIdentity ambientRef operatedRef -> True
        _ ->
          alphaEqType ambientBound operatedType
            || churchAwareEqType ambientBound operatedType

    boundsAgree mbBound operatedType =
      let constructedType = maybe TBottom tyToElab mbBound
       in alphaEqType constructedType operatedType
            || churchAwareEqType constructedType operatedType

    overlappingClaimEdgeSets claimEdgeSets =
      [ claimEdges
      | (index, claimEdges) <- zip [0 :: Int ..] claimEdgeSets
      , any (claimEdgesOverlap claimEdges) (drop (index + 1) claimEdgeSets)
      ]

    claimEdgesOverlap left right =
      not
        ( IntSet.null
            ( IntSet.intersection
                (edgeKeySet left)
                (edgeKeySet right)
            )
        )

    edgeKeySet =
      IntSet.fromList . map getEdgeId . NonEmpty.toList

    claimFailure :: [String] -> Either ElabError a
    claimFailure details =
      Left
        ( ValidationFailed
            ( [ "invalid direct application Gamma requirement claim"
              , "  owner: " ++ show owner
              , "  annotation: " ++ show ann
              ]
                ++ details
            )
        )

annotationInstantiationEdges :: AnnExpr -> [EdgeId]
annotationInstantiationEdges = map fst . annotationInstantiationEdgeSources

annotationInstantiationEdgeSources :: AnnExpr -> [(EdgeId, AnnExpr)]
annotationInstantiationEdgeSources ann =
  case ann of
    AResolvedVar {} -> []
    ALit {} -> []
    ALam _ _ _ _ body bodyEdge _ ->
      (bodyEdge, body) : annotationInstantiationEdgeSources body
    AApp fun arg funSite argSite _ ->
      (instantiationSiteEdgeId funSite, fun)
        : (instantiationSiteEdgeId argSite, arg)
        : (annotationInstantiationEdgeSources fun ++ annotationInstantiationEdgeSources arg)
    ALet _ _ _ _ _ _ rhs body _ ->
      annotationInstantiationEdgeSources rhs ++ annotationInstantiationEdgeSources body
    AExactAnn inner _ _ edgeId ->
      (edgeId, inner) : annotationInstantiationEdgeSources inner
    AAnn inner _ edgeId ->
      (edgeId, inner) : annotationInstantiationEdgeSources inner
    ALetScope inner _ edgeId ->
      (edgeId, inner) : annotationInstantiationEdgeSources inner
    AUnfold inner _ edgeId ->
      (edgeId, inner) : annotationInstantiationEdgeSources inner

authoritativeEnvBindings :: Env -> [EnvBinding]
authoritativeEnvBindings env =
  Map.elems $
    Map.fromList
      [ (envBindingIdentityKey binding, binding)
      | binding <- Map.elems (envBindingsByIdentity env)
      ]

lookupEnvBindingForDetails :: IdDetails -> Env -> Maybe EnvBinding
lookupEnvBindingForDetails details env =
  Map.lookup (envBindingDetailsKey details) (envBindingsByIdentity env)

insertEnvBinding :: EnvBinding -> Env -> Env
insertEnvBinding binding env =
  env
    { envBindingsByIdentity =
        Map.insert (envBindingIdentityKey binding) binding (envBindingsByIdentity env)
    }

insertEnvBindingIdentityAlias :: IdDetails -> EnvBinding -> Env -> Env
insertEnvBindingIdentityAlias aliasDetails binding env =
  env
    { envBindingsByIdentity =
        Map.insert
          (envBindingDetailsKey aliasDetails)
          binding
          (envBindingsByIdentity env)
    }

-- | Enter the graph-identity domain that constructs the current packet.
-- Source-expected and compiler-exact packets both contribute routes here;
-- only the latter also carry authority to publish a source ABI.
alignEnvToConstructionBinderRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> Env
  -> Either ElabError Env
alignEnvToConstructionBinderRenames renames env
  | null renames = pure env
  | otherwise = do
      accumulatedRenames <-
        foldM
          insertConstructionBinderRename
          (envConstructionBinderRenames env)
          renames
      typeBindings <-
        foldM
          insertTypeBinding
          Map.empty
          (Map.toList (envTypeBindings env))
      sourceRefinedGammaBounds <-
        foldM
          insertTypeBinding
          Map.empty
          (Map.toList (envSourceRefinedGammaBounds env))
      pure
        env
          { envBindingsByIdentity =
              Map.map alignBinding (envBindingsByIdentity env)
          , envTypeBindings = typeBindings
          , envConstructionGammaAliases =
              IntMap.map
                (applyRefRenames renames)
                (envConstructionGammaAliases env)
          , envSourceRefinedGammaBounds = sourceRefinedGammaBounds
          , envConstructedLambdaParamTypes =
              Map.map
                (applyTypeVarRefRenames renames)
                (envConstructedLambdaParamTypes env)
          , envConstructionBinderRenames = accumulatedRenames
          }
  where
    insertConstructionBinderRename accumulated rename@(oldRef, newRef) =
      case find (typeBinderRefsSameIdentity oldRef . fst) accumulated of
        Nothing -> pure (accumulated ++ [rename])
        Just (_, existingRef)
          | typeBinderRefsSameIdentity existingRef newRef -> pure accumulated
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "construction binder quotient routes one source binder to multiple graph identities"
                    , "  source binder: " ++ show oldRef
                    , "  first target: " ++ show existingRef
                    , "  second target: " ++ show newRef
                    ]
                )

    alignBinding binding =
      let schemeInfo =
            applySchemeInfoRefRenames renames (ebSchemeInfo binding)
       in binding
            { ebSchemeInfo = schemeInfo
            , ebSchemeType = schemeToType (siScheme schemeInfo)
            }

    insertTypeBinding bindings (ref, bound) =
      let ref' = applyRefRenames renames ref
          bound' = applyTypeVarRefRenames renames bound
       in case Map.lookup ref' bindings of
            Nothing -> pure (Map.insert ref' bound' bindings)
            Just existingBound
              | alphaEqType existingBound bound'
                  || churchAwareEqType existingBound bound' ->
                  pure bindings
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "construction binder quotient merges incompatible Gamma bounds"
                        , "  binder: " ++ show ref'
                        ]
                    )

-- | Enter a strictly nested construction domain.  A parent packet may have
-- already routed @source -> parent@ when its direct child proves
-- @source -> child@.  Those are ordered lexical transitions, not two sibling
-- answers for one quotient.  Rebase the child route to @parent -> child@,
-- apply it to the already-aligned environment, and compose every accumulated
-- target to the final identity.  Conflicting routes presented by one layer are
-- still rejected by 'alignEnvToConstructionBinderRenames'.
alignEnvToNestedConstructionBinderRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> Env
  -> Either ElabError Env
alignEnvToNestedConstructionBinderRenames requestedRenames env = do
  rebasedRenames <-
    traverse
      (\(oldRef, newRef) ->
        (,) <$> resolveCurrent oldRef <*> resolveCurrent newRef
      )
      requestedRenames
  let effectiveRenames =
        [ rename
        | rename@(oldRef, newRef) <- rebasedRenames
        , not (typeBinderRefsSameIdentity oldRef newRef)
        ]
  aligned <- alignEnvToConstructionBinderRenames effectiveRenames env
  normalizedAccumulated <-
    traverse
      (\(oldRef, targetRef) ->
        do
          finalTargetRef <- resolveRef effectiveRenames [] targetRef
          pure (oldRef, finalTargetRef)
      )
      (envConstructionBinderRenames aligned)
  pure
    aligned
      { envConstructionBinderRenames = normalizedAccumulated
      }
  where
    resolveCurrent =
      resolveRef (envConstructionBinderRenames env) []

    resolveRef renames seen ref
      | any (typeBinderRefsSameIdentity ref) seen =
          Left
            ( ValidationFailed
                [ "construction binder quotient contains a rename cycle"
                , "  cycle at: " ++ show ref
                , "  renames: " ++ show renames
                ]
            )
      | otherwise =
          case
              snd
                <$> find
                  (typeBinderRefsSameIdentity ref . fst)
                  renames
            of
              Just nextRef
                | not (typeBinderRefsSameIdentity ref nextRef) ->
                    resolveRef renames (ref : seen) nextRef
              _ -> pure ref

-- | Enter a compiler-exact construction domain without allowing a nested
-- exact packet to replace a source binder already owned by the ambient
-- Gamma.  The accepted routes are construction routes too, but retaining the
-- exact subset separately preserves the stronger publication authority.
alignEnvToCompilerExactBinderRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> Env
  -> Either ElabError Env
alignEnvToCompilerExactBinderRenames requestedRenames env = do
  env' <- alignEnvToConstructionBinderRenames renames env
  exactRenames <-
    foldM
      insertCompilerExactBinderRename
      (envCompilerExactBinderRenames env)
      renames
  pure env' {envCompilerExactBinderRenames = exactRenames}
  where
    renames =
      [ rename
      | rename@(sourceRef, _) <- requestedRenames
      , not
          ( any
              (typeBinderRefsSameIdentity sourceRef)
              (Map.keys (envTypeBindings env))
          )
      ]

    insertCompilerExactBinderRename accumulated rename@(oldRef, newRef) =
      case find (typeBinderRefsSameIdentity oldRef . fst) accumulated of
        Nothing -> pure (accumulated ++ [rename])
        Just (_, existingRef)
          | typeBinderRefsSameIdentity existingRef newRef -> pure accumulated
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "compiler-exact binder quotient routes one source binder to multiple graph identities"
                    , "  source binder: " ++ show oldRef
                    , "  first target: " ++ show existingRef
                    , "  second target: " ++ show newRef
                    ]
                )

lookupEnvBindingForKey :: BindingKey -> Env -> Maybe EnvBinding
lookupEnvBindingForKey (ResolvedBindingKey identityKey) env =
  Map.lookup identityKey (envBindingsByIdentity env)

lookupSchemeInfoForKey :: BindingKey -> Env -> Maybe SchemeInfo
lookupSchemeInfoForKey key env =
  ebSchemeInfo <$> lookupEnvBindingForKey key env

lookupSchemeInfoForResolved :: ResolvedVar -> Env -> Maybe SchemeInfo
lookupSchemeInfoForResolved resolved env =
  ebSchemeInfo <$> Map.lookup (envBindingDetailsKey (resolvedVarDetails resolved)) (envBindingsByIdentity env)

envBindingIdentityKey :: EnvBinding -> EnvBindingIdentityKey
envBindingIdentityKey =
  envBindingDetailsKey . ebIdentityDetails

envBindingDetailsKey :: IdDetails -> EnvBindingIdentityKey
envBindingDetailsKey =
  idDetailsIdentityKey

resolvedEnvBindingVar :: EnvBinding -> ResolvedVar
resolvedEnvBindingVar binding =
  ResolvedVar
    { resolvedVarType = ebSchemeType binding,
      resolvedVarDetails = ebIdentityDetails binding
    }

type LocalVarKey = ResolvedVar

localVarKeyMatchesReference :: LocalVarKey -> ResolvedVar -> Bool
localVarKeyMatchesReference = resolvedVarSameIdentity

-- | Close the existential part of a source annotation against the enclosing
-- paper Gamma, before its explicit universal spine.  This is the pseudo-type
-- order from §12.3.2: @exists beta. forall alpha. ...@.  A solved existential
-- already owned by the ambient Gamma remains free instead of being
-- re-generalized at the annotation boundary.
sourceAnnotationSchemeAgainstEnv :: Env -> ElabType -> ElabScheme
sourceAnnotationSchemeAgainstEnv env ty =
  let sourceScheme = schemeFromType ty
      existentialBinders =
        [ (ref, Nothing)
        | ref <- freeTypeVarRefsInOccurrenceOrder ty
        , not (envOwnsExactTypeBinderRef env ref)
        ]
   in mkElabSchemeWithRefs
        (existentialBinders ++ schemeBinderRefs sourceScheme)
        (schemeBody sourceScheme)

sourceSchemePairForAnnotation :: Env -> AlgebraContext p -> ScopeContext p -> NodeId -> EdgeId -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap TypeBinderRef))
sourceSchemePairForAnnotation env algebraContext scopeContext nodeId edgeId =
  case
      IntMap.lookup
        (getEdgeId edgeId)
        (algAnnotationExpectedTypesByEdge algebraContext)
    of
    Just expectedTy -> do
      let annotationScheme ty =
            normalizeSchemeSubstPair
              (sourceAnnotationSchemeAgainstEnv env ty, IntMap.empty)
          fallback = annotationScheme expectedTy
      pure $
        case reifyNodeTypePreferringBound scopeContext nodeId of
          Right ty@TMuRef {} -> Just (annotationScheme ty)
          _ -> Just fallback
    Nothing -> pure Nothing

sourceSchemePairForExactLambdaParamNode :: AlgebraContext p -> ScopeContext p -> NodeId -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap TypeBinderRef))
sourceSchemePairForExactLambdaParamNode algebraContext scopeContext nodeId =
  case
      IntMap.lookup
        (getNodeId (scCanonical scopeContext nodeId))
        (algExactLambdaParamSourceTypes algebraContext)
    of
    Just srcTy -> do
      ty <- srcTypeToElabType algebraContext srcTy
      pure (Just (schemeFromType ty, IntMap.empty))
    Nothing -> pure Nothing

sourceSchemePairForOuterAnnotation :: Env -> AlgebraContext p -> ScopeContext p -> AnnExpr -> Either ElabError (Maybe (ElabScheme, IntMap.IntMap TypeBinderRef))
sourceSchemePairForOuterAnnotation env algebraContext scopeContext annExpr =
  case annExpr of
    AAnn _ annNodeId edgeId -> sourceSchemePairForAnnotation env algebraContext scopeContext annNodeId edgeId
    AExactAnn _ _ annNodeId edgeId -> sourceSchemePairForAnnotation env algebraContext scopeContext annNodeId edgeId
    ALetScope inner _ _ -> sourceSchemePairForOuterAnnotation env algebraContext scopeContext inner
    AUnfold (AAnn _ annNodeId edgeId) _ _ -> sourceSchemePairForAnnotation env algebraContext scopeContext annNodeId edgeId
    AUnfold (AExactAnn _ _ annNodeId edgeId) _ _ -> sourceSchemePairForAnnotation env algebraContext scopeContext annNodeId edgeId
    _ -> pure Nothing

-- | Find the exact boundary that owns one delayed subterm result.  Only the
-- result-producing path is relevant to a surrounding let scheme: lambda bodies
-- and let bodies retain the enclosing expression's result, while application
-- children and let RHSs do not.
compilerExactBoundarySubject
  :: EdgeId
  -> AnnExpr
  -> Maybe (AnnExpr, ResolvedSrcType)
compilerExactBoundarySubject exactEdge annExpr =
  case annExpr of
    AExactAnn inner exactTy _ edge
      | edge == exactEdge -> Just (inner, exactTy)
      | otherwise -> compilerExactBoundarySubject exactEdge inner
    AAnn inner _ _ -> compilerExactBoundarySubject exactEdge inner
    ALam _ _ _ _ body _ _ -> compilerExactBoundarySubject exactEdge body
    ALet _ _ _ _ _ _ _ body _ -> compilerExactBoundarySubject exactEdge body
    ALetScope inner _ _ -> compilerExactBoundarySubject exactEdge inner
    AUnfold inner _ _ -> compilerExactBoundarySubject exactEdge inner
    _ -> Nothing

-- | Compose a transparent RHS lambda's completed local Gamma into the scheme
-- published by its enclosing let.  The source tree proves that the lambda is
-- on the RHS result path; the prepared packet proves the exact result binder
-- and graph routes.  Performing the quotient here makes the ELet scheme and
-- the RHS type abstractions share one identity by construction.
composeTransparentRhsCompletedGamma
  :: String
  -> (NodeId -> NodeId)
  -> AnnExpr
  -> SubtermGeneralizations
  -> SchemeInfo
  -> Either ElabError (Set.Set TypeBinderIdentity, SchemeInfo)
composeTransparentRhsCompletedGamma binding representative rhsAnn packets baseSchemeInfo =
  case subtermResultOwnershipFor rhsAnn packets of
    Nothing -> pure (Set.empty, baseSchemeInfo)
    Just ownership
      | not (subtermResultOwnershipHasTransparentPath ownership) ->
          pure (Set.empty, baseSchemeInfo)
      | otherwise ->
          let packet = subtermResultOwnershipPacket ownership
           in case subtermGeneralizationGammaAuthority packet of
                Nothing -> pure (Set.empty, baseSchemeInfo)
                Just _ -> composePacket packet
  where
    composePacket packet = do
      resultRef <-
        case subtermGeneralizationConstructionResultAbstractionRef packet of
          Nothing ->
            Left
              ( ValidationFailed
                  [ "transparent RHS Gamma packet has no result abstraction"
                  , "  binding: " ++ binding
                  ]
              )
          Just ref -> pure ref
      let packetSchemeInfo = subtermGeneralizationSchemeInfo packet
          packetBinders = schemeBinderRefs (siScheme packetSchemeInfo)
          baseBinders = schemeBinderRefs (siScheme baseSchemeInfo)
      constructionBinders <-
        mergeCompletedBinders baseBinders packetBinders
      mbResultBinder <-
        case
            [ binder
            | binder@(ref, _) <- constructionBinders
            , typeBinderRefsSameIdentity ref resultRef
            ]
          of
            [binder] -> pure (Just binder)
            []
              | transparentResultResolvedByEnclosingScheme
                  representative
                  packetSchemeInfo
                  resultRef
                  baseSchemeInfo ->
                  pure Nothing
            [] ->
              Left
                ( ValidationFailed
                    [ "transparent RHS Gamma result is absent from its completed construction scheme"
                    , "  binding: " ++ binding
                    , "  result: " ++ show resultRef
                    , "  packet scheme: " ++ show (siScheme packetSchemeInfo)
                    , "  packet operated scheme: "
                        ++ show (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                    , "  packet Gamma bound: "
                        ++ show (subtermGeneralizationGammaBoundScheme packet)
                    , "  enclosing scheme: " ++ show (siScheme baseSchemeInfo)
                    ]
                )
            binders ->
              Left
                ( ValidationFailed
                    [ "transparent RHS Gamma result is declared more than once"
                    , "  binding: " ++ binding
                    , "  result: " ++ show resultRef
                    , "  declarations: " ++ show binders
                    ]
                )
      case mbResultBinder of
        Nothing ->
          -- The enclosing closed/routed scheme, not this packet, owns the
          -- result completion.  Retain that exact identity as construction
          -- evidence so the RHS constructor consumes its provisional Gamma
          -- abstraction instead of publishing it outside the let.
          pure
            ( Set.singleton (typeBinderRefIdentity resultRef)
            , baseSchemeInfo
            )
        Just resultBinder -> do
          let requiredConstructionBinders =
                packetBinderDependencyClosure constructionBinders resultBinder
              requiredConstructionRefs = map fst requiredConstructionBinders
              retainedPacketBinders =
                [ binder
                | binder@(ref, _) <- packetBinders
                , refMember ref requiredConstructionRefs
                ]
              packetOwnedRoutes =
                IntMap.filter
                  (\ref -> refMember ref requiredConstructionRefs)
                  (schemeInfoBinderRefSubst packetSchemeInfo)
              baseRoutes = schemeInfoBinderRefSubst baseSchemeInfo
          baseRenames <-
            sharedConstructionRouteRenames
              ("transparent RHS Gamma for let " ++ binding)
              packetOwnedRoutes
              baseRoutes
          let renamedBase = applySchemeInfoRefRenames baseRenames baseSchemeInfo
          mergedBinders <-
            mergeCompletedBinders
              (schemeBinderRefs (siScheme renamedBase))
              retainedPacketBinders
          let mergedRoutes =
                IntMap.union
                  packetOwnedRoutes
                  (schemeInfoBinderRefSubst renamedBase)
              mergedSchemeInfo =
                schemeInfoFromRefSubst
                  ( mkElabSchemeWithRefs
                      mergedBinders
                      (schemeBody (siScheme renamedBase))
                  )
                  mergedRoutes
          ordered <-
            orderConstructionSchemeInfoBinders
              ("transparent RHS Gamma for let " ++ binding)
              mergedSchemeInfo
          pure
            ( Set.fromList (map (typeBinderRefIdentity . fst) requiredConstructionBinders)
            , ordered
            )

    packetBinderDependencyClosure packetBinders resultBinder =
      [ binder
      | binder@(ref, _) <- packetBinders
      , refMember ref requiredRefs
      ]
      where
        requiredRefs = close [fst resultBinder]
        close refs =
          let dependencies =
                [ dependency
                | (ref, Just bound) <- packetBinders
                , refMember ref refs
                , dependency <- freeTypeVarRefsType (tyToElab bound)
                , any (typeBinderRefsSameIdentity dependency . fst) packetBinders
                ]
              refs' = foldr insertRef refs dependencies
           in if length refs' == length refs then refs else close refs'

    mergeCompletedBinders baseBinders packetBinders =
      foldM mergeBinder [] (baseBinders ++ packetBinders)
      where
        mergeBinder merged binder@(ref, incomingBound) =
          case break (typeBinderRefsSameIdentity ref . fst) merged of
            (_, []) -> pure (merged ++ [binder])
            (before, (existingRef, existingBound) : after) -> do
              mergedBinder <-
                mergeSameIdentityBinder
                  existingRef
                  existingBound
                  ref
                  incomingBound
              pure (before ++ (mergedBinder : after))

        mergeSameIdentityBinder existingRef existingBound incomingRef incomingBound =
          case (existingBound, incomingBound) of
            (Nothing, Nothing) -> pure (existingRef, Nothing)
            (Just _, Nothing) -> pure (existingRef, existingBound)
            (Nothing, Just _) -> pure (incomingRef, incomingBound)
            (Just existing, Just incoming)
              | boundsAgree existing incoming ->
                  pure (incomingRef, incomingBound)
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "transparent RHS Gamma supplies conflicting completed bounds"
                        , "  binding: " ++ binding
                        , "  binder: " ++ show incomingRef
                        , "  let bound: " ++ show existingBound
                        , "  packet bound: " ++ show incomingBound
                        ]
                    )

        boundsAgree left right =
          let leftTy = tyToElab left
              rightTy = tyToElab right
           in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Join two prepared identity views through their shared graph-node routes.
-- The preferred view owns the outward construction identity; the existing
-- view is renamed into it.  This is used both while composing a transparent
-- RHS packet and while aligning the source-declared let ABI to that completed
-- packet.  Deriving both from one function prevents those two consumers from
-- publishing different identities for the same graph occurrence.
sharedConstructionRouteRenames
  :: String
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError [(TypeBinderRef, TypeBinderRef)]
sharedConstructionRouteRenames context preferredRoutes existingRoutes =
  foldM insertRename [] rawRenames
  where
    rawRenames =
      [ (existingRef, preferredRef)
      | (nodeKey, preferredRef) <- IntMap.toList preferredRoutes
      , Just existingRef <- [IntMap.lookup nodeKey existingRoutes]
      , not (typeBinderRefsSameIdentity existingRef preferredRef)
      ]

    insertRename renames rename@(existingRef, preferredRef) =
      case
          [ existingTarget
          | (priorExisting, existingTarget) <- renames
          , typeBinderRefsSameIdentity priorExisting existingRef
          ]
        of
          [] -> pure (renames ++ [rename])
          targets
            | all (typeBinderRefsSameIdentity preferredRef) targets ->
                pure renames
            | otherwise ->
                Left
                  ( ValidationFailed
                      [ "one construction binder routes to multiple outward identities"
                      , "  context: " ++ context
                      , "  existing binder: " ++ show existingRef
                      , "  outward binders: " ++ show (preferredRef : targets)
                      ]
                  )

-- | Lift a body-level compiler-exact packet through its resolved lambda owner.
-- The exact source type owns every enclosing arrow.  The packet owns the one
-- codomain transition from the operated type to its pending Gamma result.  By
-- composing those authorities before term elaboration, ALet never has to infer
-- a replacement scheme from a completed term or a type-checking failure.
compilerExactOwnerSchemeInfo
  :: ScopeContext p
  -> ResolvedTermIdentityKey
  -> PreparedSubtermGeneralization
  -> AnnExpr
  -> ElabType
  -> Either ElabError SchemeInfo
compilerExactOwnerSchemeInfo scopeContext owner packet exactSubject exactType = do
  resultRef <-
    case subtermGeneralizationCompilerExactResultRef packet of
      Just ref -> pure ref
      Nothing ->
        Left
          ( ValidationFailed
              [ "compiler exact packet has no delayed Gamma result"
              , "  owner: " ++ show owner
              ]
          )
  resultOperatedType <-
    case
        [ mbBound
        | (binderRef, mbBound) <-
            schemeBinderRefs
              (siScheme (subtermGeneralizationSchemeInfo packet))
        , typeBinderRefsSameIdentity binderRef resultRef
        ]
      of
        [Just bound] -> pure (tyToElab bound)
        [Nothing] ->
          Left
            ( ValidationFailed
                [ "compiler exact delayed Gamma result has no operated bound"
                , "  owner: " ++ show owner
                , "  result: " ++ show resultRef
                ]
            )
        [] ->
          Left
            ( ValidationFailed
                [ "compiler exact delayed Gamma result is absent from its packet"
                , "  owner: " ++ show owner
                , "  result: " ++ show resultRef
                ]
            )
        _ ->
          Left
            ( ValidationFailed
                [ "compiler exact delayed Gamma result occurs more than once in its packet"
                , "  owner: " ++ show owner
                , "  result: " ++ show resultRef
                ]
            )
  liftedType <- liftAtOwner resultRef resultOperatedType exactSubject exactType
  let (exactBinders, liftedBody) = splitForallsRefs liftedType
      packetSchemeInfo = subtermGeneralizationSchemeInfo packet
      packetBinders = schemeBinderRefs (siScheme packetSchemeInfo)
  (mergedBinders, packetRefRenames) <-
    mergeBinderSpines packetSchemeInfo liftedBody exactBinders packetBinders
  let constructionSubst =
        IntMap.map
          (applyRefRenames packetRefRenames)
          (schemeInfoBinderRefSubst packetSchemeInfo)
      operatedSubstRaw =
        IntMap.map
          (applyRefRenames packetRefRenames)
          ( schemeInfoBinderRefSubst
              (subtermGeneralizationOperatedSchemeInfo packet)
          )
  operatedSubst <-
    IntMap.traverseWithKey
      (projectOperatedRoute mergedBinders constructionSubst)
      operatedSubstRaw
  exactSubst <-
    foldM
      (\subst (nodeKey, operatedRef) ->
        case IntMap.lookup nodeKey subst of
          Nothing -> pure (IntMap.insert nodeKey operatedRef subst)
          Just constructionRef
            | typeBinderRefsSameIdentity constructionRef operatedRef -> pure subst
            | otherwise ->
                Left
                  ( ValidationFailed
                      [ "compiler exact construction and operated packet disagree on a binder route"
                      , "  owner: " ++ show owner
                      , "  graph node: " ++ show (NodeId nodeKey)
                      , "  construction ref: " ++ show constructionRef
                      , "  operated ref: " ++ show operatedRef
                      ]
                  )
      )
      constructionSubst
      (IntMap.toList operatedSubst)
  pure
    ( schemeInfoFromRefSubst
        (mkElabSchemeWithRefs mergedBinders liftedBody)
        exactSubst
    )
  where
    typesAgree left right =
      scopedTypesAgree scopeContext left right
        || alphaEqType left right
        || churchAwareEqType left right

    -- The construction packet has already published the outward binder for
    -- each graph class.  Quotient every operated route through that same
    -- authority before the substitutions are merged.  A graph-backed ref is
    -- authoritative only when it is an actual binder of the merged scheme;
    -- arbitrary aliases in the same solved class are not enough.
    projectOperatedRoute mergedBinders constructionSubst nodeKey operatedRef = do
      case typeBinderRefNode operatedRef of
        Just operatedNode
          | routeRepresentative operatedNode /= routeRepresentative (NodeId nodeKey) ->
              Left
                ( ValidationFailed
                    [ "compiler exact operated route crosses graph classes"
                    , "  owner: " ++ show owner
                    , "  graph node: " ++ show (NodeId nodeKey)
                    , "  operated ref: " ++ show operatedRef
                    ]
                )
        _ -> pure ()
      constructionCandidates <-
        concat
          <$> traverse
            publishedCandidate
            [ (NodeId constructionKey, constructionRef)
            | (constructionKey, constructionRef) <- IntMap.toList constructionSubst
            , routeRepresentative (NodeId constructionKey) == routeRepresentative (NodeId nodeKey)
            ]
      case constructionCandidates of
        [] -> pure operatedRef
        candidate : rest
          | all (typeBinderRefsSameIdentity candidate) rest -> pure candidate
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "compiler exact graph class routes to multiple construction binder identities"
                    , "  owner: " ++ show owner
                    , "  operated graph node: " ++ show (NodeId nodeKey)
                    , "  operated ref: " ++ show operatedRef
                    , "  candidates: " ++ show constructionCandidates
                    ]
                )
      where
        routeRepresentative =
          scopeTypeBinderIdentityRepresentative scopeContext

        publishedCandidate (constructionNode, constructionRef) =
          case typeBinderRefNode constructionRef of
            Nothing -> pure [constructionRef]
            Just refNode
              | routeRepresentative refNode /= routeRepresentative constructionNode ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact construction route crosses graph classes"
                        , "  owner: " ++ show owner
                        , "  graph node: " ++ show constructionNode
                        , "  construction ref: " ++ show constructionRef
                        ]
                    )
              | otherwise ->
                  case
                      [ publishedRef
                      | (publishedRef, _) <- mergedBinders
                      , typeBinderRefsSameIdentity constructionRef publishedRef
                      ]
                    of
                      [] -> pure []
                      [publishedRef] -> pure [publishedRef]
                      publishedRefs ->
                        Left
                          ( ValidationFailed
                              [ "compiler exact construction repeats one published binder identity"
                              , "  owner: " ++ show owner
                              , "  construction ref: " ++ show constructionRef
                              , "  published refs: " ++ show publishedRefs
                              ]
                          )

    liftAtOwner resultRef resultOperatedType expr ty =
      case ty of
        TForallRef ref mbBound body ->
          TForallRef ref mbBound
            <$> liftAtOwner resultRef resultOperatedType expr body
        _ ->
          case expr of
            AAnn inner _ _ -> liftAtOwner resultRef resultOperatedType inner ty
            AExactAnn inner _ _ _ -> liftAtOwner resultRef resultOperatedType inner ty
            ALet _ _ _ _ _ _ _ body _ -> liftAtOwner resultRef resultOperatedType body ty
            ALetScope inner _ _ -> liftAtOwner resultRef resultOperatedType inner ty
            AUnfold inner _ _ -> liftAtOwner resultRef resultOperatedType inner ty
            ALam _ details _ _ body _ _ ->
              case ty of
                TArrow domain codomain
                  | idDetailsIdentityKey details == owner ->
                      if typesAgree codomain resultOperatedType
                        then pure (TArrow domain (TVarRef resultRef))
                        else
                          Left
                            ( ValidationFailed
                                [ "compiler exact owner codomain does not equal its operated packet"
                                , "  owner: " ++ show owner
                                , "  exact codomain: " ++ show codomain
                                , "  operated result type: " ++ show resultOperatedType
                                , "  packet scheme: "
                                    ++ show (siScheme (subtermGeneralizationSchemeInfo packet))
                                , "  exact type: " ++ show exactType
                                ]
                            )
                  | otherwise ->
                      TArrow domain
                        <$> liftAtOwner resultRef resultOperatedType body codomain
                _ ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact owner path does not match the exact source type"
                        , "  owner: " ++ show owner
                        , "  expression: " ++ show expr
                        , "  type: " ++ show ty
                        ]
                    )
            _ ->
              Left
                ( ValidationFailed
                    [ "compiler exact result owner is absent from the exact subject path"
                    , "  owner: " ++ show owner
                    , "  expression: " ++ show expr
                    ]
                )

    mergeBinderSpines packetSchemeInfo liftedBody exactBinders packetBinders = do
      exactBinders' <- traverse preferPacketBinder exactBinders
      let exactRefs = map fst exactBinders'
          packetOnlyRaw =
            [ binder
            | binder@(ref, _) <- packetBinders
            , not (any (typeBinderRefsSameIdentity ref) exactRefs)
            ]
          packetOnly =
            retainLivePacketBinders
              ( freeTypeVarRefsType liftedBody
                  ++ concatMap (maybe [] (freeTypeVarRefsType . tyToElab) . snd) exactBinders'
              )
              packetOnlyRaw
          droppedPacketBinders =
            [ binder
            | binder@(ref, _) <- packetOnlyRaw
            , not (any (typeBinderRefsSameIdentity ref . fst) packetOnly)
            , any
                (typeBinderRefsSameIdentity ref)
                (IntMap.elems (schemeInfoBinderRefSubst packetSchemeInfo))
            ]
          exactOnlyBinders =
            [ binder
            | binder@(ref, _) <- exactBinders'
            , not (any (typeBinderRefsSameIdentity ref . fst) packetBinders)
            ]
          packetPayloadRefs =
            freeTypeVarRefsType (schemeBody (siScheme packetSchemeInfo))
              ++ concatMap
                (maybe [] (freeTypeVarRefsType . tyToElab) . snd)
                packetBinders
          carriedExactBinders =
            [ binder
            | binder@(ref, _) <- exactOnlyBinders
            , any (typeBinderRefsSameIdentity ref) packetPayloadRefs
            ]
          exactCandidates
            | length carriedExactBinders == length droppedPacketBinders =
                carriedExactBinders
            | length exactOnlyBinders == length droppedPacketBinders =
                exactOnlyBinders
            | otherwise = []
      packetRefRenames <-
        if null droppedPacketBinders
          then pure []
          else
            if length exactCandidates /= length droppedPacketBinders
              then
                Left
                  ( ValidationFailed
                      [ "compiler exact source cannot account for dropped packet binders"
                      , "  owner: " ++ show owner
                      , "  exact-only binders: " ++ show exactOnlyBinders
                      , "  dropped packet binders: " ++ show droppedPacketBinders
                      ]
                  )
              else traverse alignDroppedBinder (zip droppedPacketBinders exactCandidates)
      pure (exactBinders' ++ packetOnly, packetRefRenames)
      where
        alignDroppedBinder ((packetRef, packetBound), (exactRef, exactBound))
          | boundsAgree packetBound exactBound = pure (packetRef, exactRef)
          | otherwise =
              Left
                ( ValidationFailed
                    [ "compiler exact source and dropped packet binder disagree on their bound"
                    , "  owner: " ++ show owner
                    , "  packet binder: " ++ show packetRef
                    , "  packet bound: " ++ show packetBound
                    , "  exact binder: " ++ show exactRef
                    , "  exact bound: " ++ show exactBound
                    ]
                )

        retainLivePacketBinders initialLive binders =
          fst (foldr retain ([], initialLive) binders)

        retain binder@(ref, mbBound) (kept, liveRefs)
          | any (typeBinderRefsSameIdentity ref) liveRefs =
              ( binder : kept
              , maybe liveRefs
                  ((++ liveRefs) . freeTypeVarRefsType . tyToElab)
                  mbBound
              )
          | otherwise = (kept, liveRefs)

        preferPacketBinder exactBinder@(exactRef, exactBound) =
          case
              [ packetBinder
              | packetBinder@(packetRef, _) <- packetBinders
              , typeBinderRefsSameIdentity exactRef packetRef
              ]
            of
              [] -> pure exactBinder
              [packetBinder@(_, packetBound)]
                | boundsAgree exactBound packetBound -> pure packetBinder
                | otherwise ->
                    Left
                      ( ValidationFailed
                          [ "compiler exact source and packet disagree on a shared binder bound"
                          , "  binder: " ++ show exactRef
                          , "  exact bound: " ++ show exactBound
                          , "  packet bound: " ++ show packetBound
                          ]
                      )
              _ ->
                Left
                  ( ValidationFailed
                      [ "compiler exact packet repeats a source binder identity"
                      , "  binder: " ++ show exactRef
                      ]
                  )

        boundsAgree Nothing Nothing = True
        boundsAgree (Just left) (Just right) =
          typesAgree (tyToElab left) (tyToElab right)
        boundsAgree _ _ = False

lookupAliasTarget :: BindingKey -> Env -> Maybe BindingKey
lookupAliasTarget key env = lookupEnvBindingForKey key env >>= ebAliasTarget

resolveAliasKey :: Env -> BindingKey -> BindingKey
resolveAliasKey env key =
  case lookupAliasTarget key env of
    Just target -> resolveAliasKey env target
    Nothing -> key

isTransparentMediatorKey :: BindingKey -> Env -> Bool
isTransparentMediatorKey key env =
  maybe False (isJust . ebTransparentMediator) (lookupEnvBindingForKey key env)

transparentMediatorArity :: Env -> AnnExpr -> Maybe Int
transparentMediatorArity env ann = do
  key <- directAnnReferenceKey (stripAnnExpr ann)
  binding <- lookupEnvBindingForKey key env
  case ebTransparentMediator binding of
    Just DirectIdentityMediator -> Just 0
    Just (EtaTransparentMediator arity) -> Just arity
    Nothing -> Nothing

-- | Display names already owned by lexical Gamma, excluding any construction
-- identities that the current boundary has proved it owns itself.
freeTypeVarsEnvSchemesExceptIdentities
  :: Set.Set TypeBinderIdentity
  -> Env
  -> Set.Set String
freeTypeVarsEnvSchemesExceptIdentities excludedIdentities env =
  Set.unions
    [ typeBinderRefAliasNames ref
    | ref <- envFreeRefs
    , Set.notMember (typeBinderRefIdentity ref) excludedIdentities
    ]
  where
    envFreeRefs =
      Map.keys (envTypeBindings env)
        ++ concatMap
          (freeTypeVarRefsType . ebSchemeType)
          (authoritativeEnvBindings env)

canonicalBinderRefPayloadRenames
  :: Env
  -> Set.Set String
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
canonicalBinderRefPayloadRenames env additionallyReserved localRefs =
  [ (oldRef, newRef)
  | (oldRef, newRef) <- renames0
  , not (typeBinderRefsSameIdentityAndName oldRef newRef)
  ]
  where
    localIdentities = Set.fromList (map typeBinderRefIdentity localRefs)
    reservedNames =
      Set.union
        additionallyReserved
        (freeTypeVarsEnvSchemesExceptIdentities localIdentities env)
    (_, renames0) =
      mapAccumL allocateCanonicalName (reservedNames, 0 :: Int) localRefs

    allocateCanonicalName (used, nextIndex) ref =
      let (chosenName, followingIndex) = nextAlphaName used nextIndex
          renamedRef = renameTypeBinderRef chosenName ref
          used' = Set.union used (typeBinderRefAliasNames renamedRef)
       in ((used', followingIndex), (ref, renamedRef))

    nextAlphaName used index
      | Set.member candidate used = nextAlphaName used (index + 1)
      | otherwise = (candidate, index + 1)
      where
        candidate = alphaName index 0

renamePublishedRef
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeBinderRef
  -> TypeBinderRef
renamePublishedRef renames ref =
  fromMaybe ref
    ( snd
        <$> find
          (typeBinderRefsSameIdentity ref . fst)
          renames
    )

renameOwnerFinalConstructionBinderRefPayloads
  :: [(TypeBinderRef, TypeBinderRef)]
  -> OwnerFinalConstruction
  -> OwnerFinalConstruction
renameOwnerFinalConstructionBinderRefPayloads renames ownerCertificate =
  ownerCertificate
    { ofcConstructedType =
        renameTypeBinderRefPayloads
          renames
          (ofcConstructedType ownerCertificate)
    , ofcLocallyEmittedBinderRefs =
        map (renamePublishedRef renames) (ofcLocallyEmittedBinderRefs ownerCertificate)
    , ofcLocalBinderRoutes =
        IntMap.map (renamePublishedRef renames) (ofcLocalBinderRoutes ownerCertificate)
    , ofcUsedAmbientBinderRefs =
        map (renamePublishedRef renames) (ofcUsedAmbientBinderRefs ownerCertificate)
    , ofcBodyConsumerBoundRefinements =
        map
          (renameBodyConsumerBoundRefinementCertificate renames)
          (ofcBodyConsumerBoundRefinements ownerCertificate)
    }

renameLocalGammaConstructionCertificateBinderRefPayloads
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaConstructionCertificate
  -> LocalGammaConstructionCertificate
renameLocalGammaConstructionCertificateBinderRefPayloads renames localCertificate =
  localCertificate
    { lgccConstructedType =
        renameTypeBinderRefPayloads
          renames
          (lgccConstructedType localCertificate)
    , lgccConstruction =
        renameConstruction (lgccConstruction localCertificate)
    , lgccDirectApplicationGammaClaims =
        map
          renameDirectApplicationGammaClaim
          (lgccDirectApplicationGammaClaims localCertificate)
    , lgccDirectApplicationAmbientGammaClaims =
        map
          renameDirectApplicationAmbientGammaClaim
          (lgccDirectApplicationAmbientGammaClaims localCertificate)
    , lgccAmbientDeclarationAuthorities =
        map
          ( \authority ->
              authority
                { agaExactRef =
                    renamePublishedRef
                      renames
                      (agaExactRef authority)
                , agaBound =
                    renameTypeBinderRefPayloads
                      renames
                      (agaBound authority)
                }
          )
          (lgccAmbientDeclarationAuthorities localCertificate)
    , lgccLocalBinderRoutes =
        IntMap.map (renamePublishedRef renames) (lgccLocalBinderRoutes localCertificate)
    , lgccSourceBinderAuthorities =
        IntMap.map
          (renamePublishedRef renames)
          (lgccSourceBinderAuthorities localCertificate)
    , lgccUsedAmbientBinderRefs =
        map (renamePublishedRef renames) (lgccUsedAmbientBinderRefs localCertificate)
    }
  where
    renameConstruction construction =
      case construction of
        LocalGammaEmitted emitted consumed ->
          LocalGammaEmitted
            (fmap renameBinder emitted)
            (map renameBinder consumed)
        LocalGammaConsumed consumed ->
          LocalGammaConsumed (fmap renameBinder consumed)
        LocalGammaAmbient ->
          LocalGammaAmbient

    renameBinder (ref, mbBound) =
      ( renamePublishedRef renames ref
      , fmap
          (mapBoundType (renameTypeBinderRefPayloads renames))
          mbBound
      )

    renameDirectApplicationGammaClaim claim =
      claim
        { dagcOperatedType =
            renameTypeBinderRefPayloads
              renames
              (dagcOperatedType claim)
        , dagcBinderRef =
            renamePublishedRef renames (dagcBinderRef claim)
        , dagcConstructedBound =
            fmap
              (mapBoundType (renameTypeBinderRefPayloads renames))
              (dagcConstructedBound claim)
        }

    renameDirectApplicationAmbientGammaClaim claim =
      claim
        { daagcOperatedType =
            renameTypeBinderRefPayloads
              renames
              (daagcOperatedType claim)
        , daagcAmbientRef =
            renamePublishedRef renames (daagcAmbientRef claim)
        , daagcAmbientBound =
            renameTypeBinderRefPayloads
              renames
              (daagcAmbientBound claim)
        }

renameElaboratedTermBinderRefPayloads
  :: [(TypeBinderRef, TypeBinderRef)]
  -> ElaboratedTerm
  -> ElaboratedTerm
renameElaboratedTermBinderRefPayloads renames detailed =
  detailed
    { elaboratedTerm =
        renameTermTypeBinderRefPayloads
          renames
          (elaboratedTerm detailed)
    , elaboratedOwnerFinalConstruction =
        renameOwnerFinalConstructionBinderRefPayloads renames
          <$> elaboratedOwnerFinalConstruction detailed
    , elaboratedLocalGammaConstructionCertificates =
        map
          (renameLocalGammaConstructionCertificateBinderRefPayloads renames)
          (elaboratedLocalGammaConstructionCertificates detailed)
    , elaboratedCompilerExactResultBoundCertificates =
        map renameCompilerExactResultBoundCertificate
          (elaboratedCompilerExactResultBoundCertificates detailed)
    }
  where
    renameCompilerExactResultBoundCertificate certificate =
      certificate
        { cerbcResultRef =
            renamePublishedRef renames (cerbcResultRef certificate)
        , cerbcBound =
            renameTypeBinderRefPayloads renames (cerbcBound certificate)
        }

canonicalizePublishedSchemePair
  :: Env
  -> (ElabScheme, IntMap.IntMap TypeBinderRef)
  -> ( (ElabScheme, IntMap.IntMap TypeBinderRef)
     , [(TypeBinderRef, TypeBinderRef)]
     )
canonicalizePublishedSchemePair env (scheme, subst) =
  ( ( mkElabSchemeWithRefs
        [ ( renamePublishedRef renames ref
          , fmap
              (mapBoundType (renameTypeBinderRefPayloads renames))
              mbBound
          )
        | (ref, mbBound) <- schemeBinderRefs scheme
        ]
        (renameTypeBinderRefPayloads renames (schemeBody scheme))
    , IntMap.map (renamePublishedRef renames) subst
    )
  , renames
  )
  where
    renames =
      canonicalBinderRefPayloadRenames
        env
        Set.empty
        (map fst (schemeBinderRefs scheme))

-- | Publish a checked local constructor with canonical paper-order binder
-- names.  This is one atomic presentation change over the term and every
-- certificate that describes it; identities and ownership are unchanged.
canonicalizePublishedOwnerConstruction
  :: Env
  -> OwnerFinalConstruction
  -> ElaboratedTerm
  -> Either ElabError (ElaboratedTerm, OwnerFinalConstruction)
canonicalizePublishedOwnerConstruction env certificate elaboration =
  if Set.size localIdentities /= length localRefs
    then
      Left
        ( ValidationFailed
            [ "owner construction publishes one binder identity more than once"
            , "  owner: " ++ show (ofcOwner certificate)
            , "  binders: " ++ show localRefs
            ]
        )
    else
      pure
        ( renameElaboratedTermBinderRefPayloads renames elaboration
        , renameOwnerFinalConstructionBinderRefPayloads renames certificate
        )
  where
    localRefs = ofcLocallyEmittedBinderRefs certificate
    localIdentities = Set.fromList (map typeBinderRefIdentity localRefs)
    ambientAliases =
      Set.unions
        [ typeBinderRefAliasNames ref
        | ref <- ofcUsedAmbientBinderRefs certificate
        , Set.notMember (typeBinderRefIdentity ref) localIdentities
        ]
    renames =
      canonicalBinderRefPayloadRenames env ambientAliases localRefs

freeTypeVarRefsInOccurrenceOrder :: ElabType -> [TypeBinderRef]
freeTypeVarRefsInOccurrenceOrder ty0 = reverse (snd (goType [] [] [] ty0))
  where
    refMember ref =
      any (typeBinderRefsSameIdentity ref)

    addRef bound seen acc ref
      | refMember ref bound = (seen, acc)
      | refMember ref seen = (seen, acc)
      | otherwise = (ref : seen, ref : acc)

    goType bound seen acc ty =
      case ty of
        TVarRef ref -> addRef bound seen acc ref
        TArrow dom cod ->
          let (seen', acc') = goType bound seen acc dom
           in goType bound seen' acc' cod
        TConWithIdentity _ _ args ->
          foldl' (\(seen', acc') arg -> goType bound seen' acc' arg) (seen, acc) args
        TVarAppRef ref args ->
          let (seen', acc') = addRef bound seen acc ref
           in foldl' (\(seen'', acc'') arg -> goType bound seen'' acc'' arg) (seen', acc') args
        TForallRef ref mb body ->
          let (seen', acc') =
                maybe (seen, acc) (\boundTy -> goType bound seen acc (tyToElab boundTy)) mb
           in goType (ref : bound) seen' acc' body
        TMuRef ref body -> goType (ref : bound) seen acc body
        TBaseWithIdentity _ _ -> (seen, acc)
        TBottom -> (seen, acc)

{- Note [Let generalization is identity-relative]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The scheme reconstructed for an inferred let can contain a binder whose
identity is already free in the lexical environment.  That binder represents
an outer type variable, even when its current display name is stale.  Keeping
it quantified changes @Gen(Gamma, tau)@ from

    forall (ftv(tau) \ ftv(Gamma)). tau

into an accidental re-generalization of the outer variable.

Apply the environment subtraction by 'TypeBinderIdentity' before doing the
separate display-name freshening below.  Graph identities are compared through
the solved-to-base identity representative, then the candidate type is aligned
to Gamma's authoritative, complete 'TypeBinderRef' before its quantifier is
removed.  Global redirect/UF canonicalization is insufficient here because
distinct solved copies of one lexical binder can remain in separate UF sets.
The base projection retains that source-identity provenance.  A same-named
binder with a distinct identity is still local and must remain quantified.  The
aligned ref remains in the scheme body and graph-ref substitution: only
ownership of its quantifier changes.
-}
-- | The type-binder identities already owned by the ambient paper Gamma.
-- Explicit type bindings carry their bounds directly.  A free identity in an
-- authoritative term binding is equally part of Gamma: for example, the
-- parameter @x : alpha@ proves that @alpha@ is in scope while elaborating the
-- lambda body.  Keeping both sources in one view lets Gen(Gamma, tau) and
-- boundaries that inherit the full Gamma use the same scope authority.
newtype EnvFreeTypeBinderRefs = EnvFreeTypeBinderRefs [TypeBinderRef]

envFreeTypeBinderRefs :: Env -> EnvFreeTypeBinderRefs
envFreeTypeBinderRefs env =
  EnvFreeTypeBinderRefs
    ( Map.keys (envTypeBindings env)
        ++ concatMap
          freeTypeVarRefsType
          (Map.elems (envTypeBindings env))
        ++ concatMap
          (freeTypeVarRefsType . ebSchemeType)
          (authoritativeEnvBindings env)
    )

-- | Exact explicit type-binder authority opened by the construction currently
-- being elaborated.  Lambda construction deliberately uses this narrower view:
-- packet aliases installed in 'envTypeBindings' describe the type scope opened
-- at that boundary, while its source/parameter packet rules own the separate
-- decision about which enclosing term-binding identities may be retained.
envAmbientTypeBinderRefs :: Env -> [TypeBinderRef]
envAmbientTypeBinderRefs = Map.keys . envTypeBindings

-- | The full paper Gamma inherited by a term boundary which introduces no type
-- binder of its own.  Applications (Figure 15.3.5) and let generalization both
-- inherit every type identity free in an authoritative enclosing term binding;
-- those identities must be supplied to construction generalization up front.
-- Lambdas intentionally keep their narrower explicit-type-binding view because
-- their parameter packet owns the separate decision about which term-binding
-- identities may escape.
envGeneralizationAmbientTypeBinderRefs :: Env -> [TypeBinderRef]
envGeneralizationAmbientTypeBinderRefs env =
  case envFreeTypeBinderRefs env of
    EnvFreeTypeBinderRefs refs -> refs

lookupEnvFreeTypeBinderRef
  :: (NodeId -> NodeId)
  -> EnvFreeTypeBinderRefs
  -> TypeBinderRef
  -> Maybe TypeBinderRef
lookupEnvFreeTypeBinderRef representative (EnvFreeTypeBinderRefs refs) ref =
  lookupRefByIdentityOrRepresentative representative refs ref

envContainsFreeTypeBinderRef
  :: (NodeId -> NodeId)
  -> EnvFreeTypeBinderRefs
  -> TypeBinderRef
  -> Bool
envContainsFreeTypeBinderRef representative refs =
  isJust . lookupEnvFreeTypeBinderRef representative refs

-- | Whether the ambient construction owns this exact outward identity, rather
-- than merely retaining its graph ref as an alias key for another binder.
-- 'extendEnvTypeScopeWithAliases' deliberately installs both forms in
-- 'envTypeBindings'; consulting that map alone would make a packet-local graph
-- exterior look ambient after it has been routed to a structural/source ref.
envOwnsExactTypeBinderRef :: Env -> TypeBinderRef -> Bool
envOwnsExactTypeBinderRef env ref =
  case envFreeTypeBinderRefs env of
    EnvFreeTypeBinderRefs ambientRefs ->
      any (typeBinderRefsSameIdentity ref) ambientRefs
        && case typeBinderRefNode ref of
          Just node ->
            maybe
              True
              (typeBinderRefsSameIdentity ref)
              (IntMap.lookup (getNodeId node) (envConstructionGammaAliases env))
          Nothing -> True

generalizeSchemeInfoAgainstEnvExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> Env
  -> SchemeInfo
  -> SchemeInfo
generalizeSchemeInfoAgainstEnvExcept protectedIdentities identityRepresentative env schemeInfo =
  let environmentRefs = envFreeTypeBinderRefs env
      scheme0 = siScheme schemeInfo
      binds0 = schemeBinderRefs scheme0
      -- Construction routes are an alias sidecar, not declarations in Gamma.
      -- In particular, compiler-exact edges install routes without publishing
      -- binders.  A route may therefore choose the ref used after subtraction
      -- only when the ambient environment owns that exact outward identity.
      authoritativeEnvRef ref =
        case typeBinderRefNode ref of
          Just node
            | Just constructionRef <-
                IntMap.lookup
                  (getNodeId node)
                  (envConstructionIdentityRoutes env),
              envOwnsExactTypeBinderRef env constructionRef ->
                Just constructionRef
          _ ->
            lookupEnvFreeTypeBinderRef
              identityRepresentative
              environmentRefs
              ref
      alignments =
        [ (ref, envRef)
          | (ref, _) <- binds0,
            Set.notMember (typeBinderRefIdentity ref) protectedIdentities,
            Just envRef <- [authoritativeEnvRef ref]
        ]
      binds =
        [ (ref, fmap (mapBoundType (applyTypeVarRefRenames alignments)) mbBound)
          | (ref, mbBound) <- binds0,
            not (any (typeBinderRefsSameIdentity ref . fst) alignments)
        ]
      body = applyTypeVarRefRenames alignments (schemeBody scheme0)
      -- A generalized binder can represent several solved graph occurrences
      -- that belong to distinct ambient construction identities.  Once Gen
      -- removes that binder, rewrite each substitution entry from its exact
      -- graph-node route; applying the binder-wide body rename to every entry
      -- would collapse those distinct aliases again.
      subst =
        IntMap.mapWithKey
          alignSubstitutionEntry
          (schemeInfoBinderRefSubst schemeInfo)
      alignSubstitutionEntry nodeKey ref =
        case find (typeBinderRefsSameIdentity ref . fst) alignments of
          Just (_, envRef) ->
            fromMaybe
              envRef
              (IntMap.lookup nodeKey (envConstructionIdentityRoutes env))
          Nothing -> applyRefRenames alignments ref
   in if null alignments
        then schemeInfo
        else
          schemeInfoFromRefSubst
            (mkElabSchemeWithRefs binds body)
            subst

-- | Align source-owned generated binders with the outward binders already
-- selected by the enclosing construction Gamma before computing Gen(Gamma,
-- tau).  Both maps retain the graph-node owner, so their composition is the
-- construction-time proof that the two generated identities denote one
-- lexical binder.
alignSchemeInfoToConstructionGamma
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
alignSchemeInfoToConstructionGamma =
  alignSchemeInfoToConstructionGammaExcept Set.empty

alignSchemeInfoToConstructionGammaExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
alignSchemeInfoToConstructionGammaExcept protectedIdentities identityRepresentative sourceBinderRefs env schemeInfo0 = do
  sourceRenames0 <-
    either
      (Left . ValidationFailed . pure)
      Right
      ( sourceBinderConstructionRenames
          identityRepresentative
          relevantSourceBinderRefs
          constructionIdentityRoutes
      )
  let -- A source forall declared by this scheme is lexical to the scheme,
      -- even if one of its instantiated graph occurrences later feeds an
      -- enclosing Gamma consumer.  Only free source refs can be captured by
      -- the enclosing construction environment.
      sourceRenames =
        [ rename
        | rename@(sourceRef, outwardRef) <- sourceRenames0
        , not
            ( any
                (typeBinderRefsSameIdentity sourceRef)
                lexicalBinderRefs
            )
        , Set.notMember
            (typeBinderRefIdentity outwardRef)
            protectedIdentities
        ]
  constructionAliasRenames <-
    foldM addConstructionAliasRename [] schemeRefs
  sourceIdentityRenames <-
    foldM addSourceIdentityRename [] schemeRefs
  pure
    ( applySchemeInfoRefRenames
        (constructionAliasRenames ++ sourceIdentityRenames ++ sourceRenames)
        schemeInfo
    )
  where
    -- Scheme substitutions describe graph occurrences, not quantifier
    -- ownership.  A free scheme reference can therefore survive from an
    -- earlier generalization even though its individual occurrence nodes now
    -- have distinct, exact routes in the enclosing construction Gamma.  Keep
    -- genuinely local quantifiers intact, but normalize every free occurrence
    -- by its node-key authority before deriving reference-wide renames.
    schemeInfo =
      schemeInfoFromRefSubst
        (siScheme schemeInfo0)
        ( IntMap.mapWithKey
            normalizeAmbientOccurrence
            (schemeInfoBinderRefSubst schemeInfo0)
        )

    normalizeAmbientOccurrence nodeKey ref
      | Set.member (typeBinderRefIdentity ref) protectedIdentities = ref
      | refIsLexical ref = ref
      | otherwise =
          fromMaybe
            ref
            (IntMap.lookup nodeKey constructionIdentityRoutes)

    lexicalBinderRefs = map fst (schemeBinderRefs (siScheme schemeInfo0))

    schemeRefs =
      map fst (schemeBinderRefs (siScheme schemeInfo))
        ++ freeTypeVarRefsType (schemeToType (siScheme schemeInfo))
        ++ IntMap.elems (schemeInfoBinderRefSubst schemeInfo)

    -- This alignment owns one scheme, not the whole source sidecar.  Asking
    -- an unrelated source binder to choose among this scheme's construction
    -- routes turns harmless sibling aliases into a false ambiguity.
    relevantSourceBinderRefs =
      IntMap.filterWithKey
        ( \nodeKey sourceRef ->
            IntSet.member nodeKey schemeRouteNodeKeys
              || any
                (typeBinderRefsSameIdentity sourceRef)
                schemeRefs
        )
        sourceBinderRefs

    -- A SchemeInfo substitution key is the proof that its graph-local value
    -- and the source sidecar value at that key describe one occurrence. Keep
    -- that route even though the two identities have not yet been joined.
    schemeRouteNodeKeys =
      IntSet.fromList
        [ getNodeId node
        | schemeRef <- schemeRefs
        , node <- schemeRouteNodes schemeRef
        ]

    constructionIdentityRoutes = envConstructionIdentityRoutes env

    addConstructionAliasRename renames ref =
      if Set.member (typeBinderRefIdentity ref) protectedIdentities
          || refIsLexical ref
        then pure renames
        else
          addConstructionAliasRenameUnprotected renames ref

    addConstructionAliasRenameUnprotected renames ref =
      do
        mbOutwardRef <-
          uniqueSchemeRefIdentityRoute
            "construction Gamma"
            constructionIdentityRoutes
            ref
        pure
          ( case mbOutwardRef of
              Just outwardRef
                | not (typeBinderRefsSameIdentityAndName ref outwardRef),
                  not (hasRenameFor ref renames) ->
                    renames ++ [(ref, outwardRef)]
              _ -> renames
          )

    addSourceIdentityRename renames ref =
      if ( Set.member (typeBinderRefIdentity ref) protectedIdentities
            && not (hasDirectSourceDeclarationRoute ref)
         )
          || refIsLexical ref
        then pure renames
        else
          addSourceIdentityRenameUnprotected renames ref

    -- A protected graph identity normally belongs to a construction-local
    -- Gamma consumer and must not be captured through a solved sibling.  A
    -- source sidecar entry at that binder's own graph node is different: it
    -- is the occurrence-level declaration proof for the binder itself.
    hasDirectSourceDeclarationRoute ref =
      case typeBinderRefNode ref of
        Nothing -> False
        Just node ->
          case IntMap.lookup (getNodeId node) sourceBinderRefs of
            Just sourceRef ->
              isJust
                ( typeBinderIdentityGeneratedUnique
                    (typeBinderRefIdentity sourceRef)
                )
                && not (typeBinderRefsSameIdentity ref sourceRef)
            Nothing -> False

    addSourceIdentityRenameUnprotected renames ref =
      do
        mbOutwardRef <-
          uniqueSchemeRefIdentityRoute
            "construction Gamma"
            constructionIdentityRoutes
            ref
        mbSourceRef <-
          uniqueSchemeRefIdentityRoute
            "source binder"
            sourceBinderRefs
            ref
        pure
          ( case (mbOutwardRef, mbSourceRef) of
              (Nothing, Just sourceRef)
                | isJust
                    ( typeBinderIdentityGeneratedUnique
                        (typeBinderRefIdentity sourceRef)
                    ),
                  not (typeBinderRefsSameIdentityAndName ref sourceRef),
                  not (hasRenameFor ref renames) ->
                    renames ++ [(ref, sourceRef)]
              _ -> renames
          )

    -- A generalized reference can be fresh even though its substitution
    -- still records the graph nodes from which it was constructed.  Route
    -- through both facts: consulting only the fresh ref's own node loses an
    -- already-selected enclosing Gamma alias after quotienting.
    uniqueSchemeRefIdentityRoute role routes ref = do
      candidates <-
        catMaybes
          <$> traverse
            (uniqueIdentityRoute role routes)
            (schemeRouteNodes ref)
      case foldr insertDistinctRef [] candidates of
        [] -> pure Nothing
        [candidate] -> pure (Just candidate)
        distinct ->
          Left
            ( ValidationFailed
                [ role ++ " scheme reference has multiple identity routes"
                , "  scheme reference: " ++ show ref
                , "  scheme route nodes: " ++ show (schemeRouteNodes ref)
                , "  routes: " ++ show distinct
                , "  scheme: " ++ show (siScheme schemeInfo)
                , "  scheme substitution: " ++ show (schemeInfoBinderRefSubst schemeInfo)
                , "  source binder refs: " ++ show sourceBinderRefs
                , "  construction aliases: " ++ show (envConstructionGammaAliases env)
                ]
            )

    schemeRouteNodes ref =
      foldr insertDistinctNode []
        ( maybeToList (typeBinderRefNode ref)
            ++ [ NodeId nodeKey
               | (nodeKey, routedRef) <-
                   IntMap.toList (schemeInfoBinderRefSubst schemeInfo)
               , typeBinderRefsSameIdentity ref routedRef
               ]
        )

    insertDistinctNode node nodes
      | node `elem` nodes = nodes
      | otherwise = node : nodes

    uniqueIdentityRoute role routes node =
      case IntMap.lookup (getNodeId node) routes of
        Just direct -> pure (Just direct)
        Nothing ->
          case foldr insertDistinctRef [] representativeCandidates of
            [] -> pure Nothing
            [ref] -> pure (Just ref)
            refs ->
              Left
                ( ValidationFailed
                    [ role ++ " representative has multiple identity routes",
                      "  graph node: " ++ show node,
                      "  representative: "
                        ++ show (identityRepresentative node),
                      "  routes: " ++ show refs
                    ]
                )
      where
        representativeCandidates =
          [ routedRef
          | (nodeKey, routedRef) <- IntMap.toList routes,
            identityRepresentative (NodeId nodeKey)
              == identityRepresentative node
          ]

    insertDistinctRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    hasRenameFor ref =
      any (typeBinderRefsSameIdentity ref . fst)

    refIsLexical ref =
      isNothing (typeBinderRefNode ref)
        && any (typeBinderRefsSameIdentity ref) lexicalBinderRefs

generalizeSchemeInfoAgainstConstructionEnv
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
generalizeSchemeInfoAgainstConstructionEnv =
  generalizeSchemeInfoAgainstConstructionEnvExcept Set.empty

generalizeSchemeInfoAgainstConstructionEnvExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
generalizeSchemeInfoAgainstConstructionEnvExcept excludedIdentities identityRepresentative sourceBinderRefs env schemeInfo = do
  generalizeSchemeInfoAgainstConstructionEnvPreserving
    Set.empty
    excludedIdentities
    identityRepresentative
    sourceBinderRefs
    env
    schemeInfo

generalizeSchemeInfoAgainstConstructionEnvPreserving
  :: Set.Set TypeBinderIdentity
  -> Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
generalizeSchemeInfoAgainstConstructionEnvPreserving lexicalIdentities excludedIdentities identityRepresentative sourceBinderRefs env schemeInfo = do
  aligned <-
    alignSchemeInfoToConstructionGammaExcept
      protectedIdentities
      identityRepresentative
      sourceBinderRefs
      env
      schemeInfo
  orderConstructionSchemeInfoBinders
    "generalize against construction Gamma"
    ( generalizeSchemeInfoAgainstEnvExcept
        protectedIdentities
        identityRepresentative
        env
        aligned
    )
  where
    protectedIdentities =
      lambdaParamProtectedIdentities
        lexicalIdentities
        (envOwnsExactTypeBinderRef env)
        (envConstructionIdentityRoutes env)
        schemeInfo
        excludedIdentities

freshenSchemeInfoAgainstConstructionEnv
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
freshenSchemeInfoAgainstConstructionEnv identityRepresentative sourceBinderRefs env schemeInfo = do
  freshenSchemeInfoAgainstConstructionEnvExcept
    Set.empty
    identityRepresentative
    sourceBinderRefs
    env
    schemeInfo

freshenSchemeInfoAgainstConstructionEnvExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> Env
  -> SchemeInfo
  -> Either ElabError SchemeInfo
freshenSchemeInfoAgainstConstructionEnvExcept excludedIdentities identityRepresentative sourceBinderRefs env schemeInfo = do
  aligned <-
    alignSchemeInfoToConstructionGammaExcept
      protectedIdentities
      identityRepresentative
      sourceBinderRefs
      env
      schemeInfo
  let generalized =
        generalizeSchemeInfoAgainstEnvExcept
          protectedIdentities
          identityRepresentative
          env
          aligned
  ordered <-
    orderConstructionSchemeInfoBinders
      "freshen against construction Gamma"
      generalized
  pure
    ( freshenSchemeInfoBinderNamesAgainst
        (freeTypeVarsEnvSchemesExceptIdentities protectedIdentities env)
        ordered
    )
  where
    protectedIdentities =
      constructionProtectedIdentities
        (envOwnsExactTypeBinderRef env)
        (envConstructionIdentityRoutes env)
        schemeInfo
        excludedIdentities

orderConstructionSchemeInfoBinders
  :: String
  -> SchemeInfo
  -> Either ElabError SchemeInfo
orderConstructionSchemeInfoBinders role schemeInfo = do
  orderedScheme <-
    either
      (Left . ValidationFailed . pure)
      Right
      (orderSourceProjectedSchemeBinders role (siScheme schemeInfo))
  pure
    ( schemeInfoFromRefSubst
        orderedScheme
        (schemeInfoBinderRefSubst schemeInfo)
    )

applySchemeInfoRefRenames :: [(TypeBinderRef, TypeBinderRef)] -> SchemeInfo -> SchemeInfo
applySchemeInfoRefRenames renames schemeInfo =
  schemeInfoFromRefSubst
    (mkElabSchemeWithRefs binds' body')
    subst'
  where
    scheme0 = siScheme schemeInfo
    binderRefs = map fst (schemeBinderRefs scheme0)
    (binderRenames, externalRenames) =
      partition
        (\(oldRef, _) -> any (typeBinderRefsSameIdentity oldRef) binderRefs)
        renames
    binds' = foldl' retainRenamedBinder [] renamedBinds
    renamedBinds = renameBinds [] (schemeBinderRefs scheme0)
    body' = applyTypeVarRefRenames renames (schemeBody scheme0)
    subst' = IntMap.map (applyRefRenames renames) (schemeInfoBinderRefSubst schemeInfo)

    retainRenamedBinder retained binder@(ref, mbBound) =
      case find (typeBinderRefsSameIdentity ref . fst) retained of
        Just (_, existingBound)
          | equivalentRenamedBounds existingBound mbBound -> retained
        _ -> retained ++ [binder]

    equivalentRenamedBounds left right =
      let leftTy = maybe TBottom tyToElab left
          rightTy = maybe TBottom tyToElab right
       in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

    renameBinds _ [] = []
    renameBinds previous ((ref, mbBound) : rest) =
      let ref' = applyRefRenames binderRenames ref
          -- Construction aliases that are free in this scheme apply to every
          -- binder bound.  Alpha-renames for scheme binders apply only after
          -- their binder has entered scope.  Treating both as the latter left
          -- S'(operated) source identities unchanged in packet bounds.
          boundRenames = externalRenames ++ previous
          mbBound' =
            fmap
              (mapBoundType (applyTypeVarRefRenames boundRenames))
              mbBound
          previous'
            | typeBinderRefsSameIdentityAndName ref ref' = previous
            | otherwise = previous ++ [(ref, ref')]
       in (ref', mbBound') : renameBinds previous' rest

freshenSchemeInfoAgainstEnv :: Env -> SchemeInfo -> SchemeInfo
freshenSchemeInfoAgainstEnv = freshenSchemeInfoAgainstEnvWithRepresentative id

freshenSchemeInfoAgainstEnvWithRepresentative :: (NodeId -> NodeId) -> Env -> SchemeInfo -> SchemeInfo
freshenSchemeInfoAgainstEnvWithRepresentative identityRepresentative env schemeInfo0 =
  freshenSchemeInfoAgainstEnvWithRepresentativeExcept
    Set.empty
    identityRepresentative
    env
    schemeInfo0

freshenSchemeInfoAgainstEnvWithRepresentativeExcept
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> Env
  -> SchemeInfo
  -> SchemeInfo
freshenSchemeInfoAgainstEnvWithRepresentativeExcept excludedIdentities identityRepresentative env schemeInfo0 =
  let protectedIdentities =
        constructionProtectedIdentities
          (envOwnsExactTypeBinderRef env)
          (envConstructionIdentityRoutes env)
          schemeInfo0
          excludedIdentities
      schemeInfo =
        generalizeSchemeInfoAgainstEnvExcept
          protectedIdentities
          identityRepresentative
          env
          schemeInfo0
      reservedNames =
        freeTypeVarsEnvSchemesExceptIdentities protectedIdentities env
      scheme0 = siScheme schemeInfo
      binds = schemeBinderRefs scheme0
      body0 = schemeBody scheme0
      binderNames = map (typeBinderRefName . fst) binds
      binderDomain = Set.fromList binderNames
      renames = reverse (snd (foldl' (chooseFreshBinder binderDomain) (reservedNames, []) (map fst binds)))
      refRenames =
        [ (oldRef, renameTypeBinderRef newName oldRef)
          | (oldRef, newName) <- renames,
            typeBinderRefName oldRef /= newName
        ]
   in if null refRenames
        then schemeInfo
        else
          let binds' = renameSchemeBinds refRenames binds
              body' = applyTypeVarRefRenames refRenames body0
              scheme' = mkElabSchemeWithRefs binds' body'
              subst' = IntMap.map (applyRefRenames refRenames) (schemeInfoBinderRefSubst schemeInfo)
           in schemeInfoFromRefSubst scheme' subst'
  where
    chooseFreshBinder binderDomain (used, acc) binder =
      let name = typeBinderRefName binder
          name' =
            if Set.member name used
              then freshNameLike name (Set.union used binderDomain)
              else name
       in (Set.insert name' used, (binder, name') : acc)

    renameSchemeBinds renames0 = go []
      where
        go _ [] = []
        go prev ((ref, mbBound) : restBinds) =
          let ref' = applyRefRenames renames0 ref
              mbBound' = fmap (mapBoundType (applyTypeVarRefRenames prev)) mbBound
              prev'
                | typeBinderRefsSameIdentityAndName ref ref' = prev
                | otherwise = prev ++ [(ref, ref')]
           in (ref', mbBound') : go prev' restBinds

applyRefRenames :: [(TypeBinderRef, TypeBinderRef)] -> TypeBinderRef -> TypeBinderRef
applyRefRenames [] ref = ref
applyRefRenames ((oldRef, newRef) : rest) ref
  | typeBinderRefsSameIdentity oldRef ref = newRef
  | otherwise = applyRefRenames rest ref

applyTypeVarRefRenames :: [(TypeBinderRef, TypeBinderRef)] -> ElabType -> ElabType
applyTypeVarRefRenames renames0 ty0 =
  foldl'
    ( \ty (oldRef, newRef) ->
        if typeBinderRefsSameIdentityAndName oldRef newRef
          then ty
          else substTypeCaptureRef oldRef (TVarRef newRef) ty
    )
    ty0
    renames0

structuralRecursiveCandidateType :: StructuralRecursiveCandidate -> ElabType
structuralRecursiveCandidateType candidate =
  case candidate of
    StructuralRecursiveCandidateFromHelper ty -> ty
    StructuralRecursiveCandidateFromDirectCarrier ty -> ty

selectStructuralRecursiveCandidate :: [StructuralRecursiveCandidate] -> StructuralRecursiveCandidateSelection
selectStructuralRecursiveCandidate =
  selectUniqueCandidateBy
    ( \existing candidate ->
        alphaEqType
          (structuralRecursiveCandidateType existing)
          (structuralRecursiveCandidateType candidate)
    )

schemeHasForwardBoundReference :: ElabType -> Bool
schemeHasForwardBoundReference schemeTy =
  let (binds, _) = splitForallsRefs schemeTy
      go [] = False
      go ((_, mbBound) : rest) =
        let laterRefs = map fst rest
            boundMentionsLater =
              case mbBound of
                Just bound ->
                  any
                    (\laterRef -> any (typeBinderRefsSameIdentity laterRef) (freeTypeVarRefsType bound))
                    laterRefs
                Nothing -> False
         in boundMentionsLater || go rest
   in go binds

schemeTypeHasExplicitBound :: ElabType -> Bool
schemeTypeHasExplicitBound schemeTy =
  let (binds, _) = splitForallsRefs schemeTy
   in any (isJust . snd) binds

stripAnnExpr :: AnnExpr -> AnnExpr
stripAnnExpr annExpr =
  case annExpr of
    AAnn inner _ _ -> stripAnnExpr inner
    AExactAnn inner _ _ _ -> stripAnnExpr inner
    ALetScope inner _ _ -> stripAnnExpr inner
    AUnfold inner _ _ -> stripAnnExpr inner
    _ -> annExpr

-- | Materialize the leading lambda domains already fixed by a let's published
-- scheme.  The RHS is constructed under that scheme's Gamma, so its resolved
-- lambda binders must use the corresponding arrow domains directly instead of
-- first inventing graph-local variables that are repaired after elaboration.
-- Iso-recursive schemes are unfolded once at this boundary before the same
-- structural walk.
--
-- The map is keyed by resolved binder identity, so the authority cannot leak
-- to an unrelated nested lambda.  Source annotations remain authoritative in
-- 'ALamF'; this view supplies the missing owner information only when the
-- lambda itself is unannotated.
constructedLambdaParamTypes
  :: Env
  -> ElabType
  -> AnnExpr
  -> Map.Map EnvBindingIdentityKey ElabType
constructedLambdaParamTypes env constructionTy rhsAnn =
  case stripLeadingForalls constructionTy of
    muTy@(TMuRef muRef muBody)
      | hasContractiveRecursiveWitness muTy ->
          go Map.empty (substTypeCaptureRef muRef muTy muBody) rhsAnn
    ty -> go Map.empty ty rhsAnn
  where
    stripLeadingForalls ty =
      case ty of
        TForallRef _ _ body -> stripLeadingForalls body
        _ -> ty

    go mediatorAliases expectedTy annExpr =
      case annExpr of
        ALam _ details _ _ body _ _ ->
          case expectedTy of
            TArrow domainTy codomainTy ->
              Map.insert
                (envBindingDetailsKey details)
                domainTy
                (go mediatorAliases codomainTy body)
            _ -> Map.empty
        AApp fun arg _ _ _
          | isJust (transparentKind mediatorAliases fun) ->
              -- A validated identity/eta mediator preserves the complete type
              -- of its first argument.  Carry the published let scheme through
              -- that exact resolved occurrence so a nested lambda receives its
              -- domain before its body is elaborated.
              go mediatorAliases expectedTy arg
        ALet _ details _ _ _ _ rhs body _ ->
          let key = annBinderKey details
              mediatorAliases' =
                case transparentKind mediatorAliases rhs of
                  Just kind -> Map.insert key kind mediatorAliases
                  Nothing -> Map.delete key mediatorAliases
           in go mediatorAliases' expectedTy body
        AAnn inner _ _ -> go mediatorAliases expectedTy inner
        AExactAnn inner _ _ _ -> go mediatorAliases expectedTy inner
        ALetScope inner _ _ -> go mediatorAliases expectedTy inner
        -- An explicit source unfold changes the expected type before the
        -- lambda and therefore owns its own construction boundary.
        AUnfold {} -> Map.empty
        _ -> Map.empty

    transparentKind mediatorAliases annExpr =
      transparentMediatorKindAnn annExpr
        <|> do
          key <- directAnnReferenceKey (stripAnnExpr annExpr)
          Map.lookup key mediatorAliases
            <|> (lookupEnvBindingForKey key env >>= ebTransparentMediator)

-- | Seed lambda domains from an already prepared construction contract.  The
-- map is identity-keyed and therefore remains valid through source wrappers;
-- existing, more local construction entries retain precedence.
withEnvConstructedLambdaParamTypes :: ElabType -> AnnExpr -> Env -> Env
withEnvConstructedLambdaParamTypes constructionTy annExpr env =
  env
    { envConstructedLambdaParamTypes =
        Map.union
          (envConstructedLambdaParamTypes env)
          (constructedLambdaParamTypes env constructionTy annExpr)
    }

directAnnReferenceKey :: AnnExpr -> Maybe BindingKey
directAnnReferenceKey annExpr =
  case annExpr of
    AResolvedVar details _ _ -> Just (ResolvedBindingKey (idDetailsIdentityKey details))
    _ -> Nothing

stripLeadingTyAbs :: XmlfTerm -> XmlfTerm
stripLeadingTyAbs term =
  case term of
    ETyAbsRef _ _ body -> stripLeadingTyAbs body
    _ -> term

-- | Complete one prepared stage of the Gamma result abstractions delayed at a
-- compiler-owned exact boundary.  Source-ABI results are consumed before
-- exact validation; packet-only results are consumed afterwards.  Packet
-- owner identity, exact-edge provenance, and the prepared stage select the
-- insertion points; term shape and inferred types are never recovery oracles.
completeCompilerExactSubtermResults
  :: CompilerExactResultStage
  -> EdgeId
  -> SubtermGeneralizations
  -> XmlfTerm
  -> Either ElabError XmlfTerm
completeCompilerExactSubtermResults =
  completeCompilerExactSubtermResultsWithBounds []

-- | Apply the same graph-to-outward substitution used to close a root term to
-- its construction certificates.  Identity matching, rather than binder
-- spelling, determines each route.
projectCompilerExactResultBoundCertificates
  :: IntMap.IntMap TypeBinderRef
  -> [CompilerExactResultBoundCertificate]
  -> [CompilerExactResultBoundCertificate]
projectCompilerExactResultBoundCertificates subst =
  map projectCertificate
  where
    renames =
      [ ( typeBinderRefFromIdentity
            (typeBinderIdentityFromNode (NodeId nodeKey))
            ("t" ++ show nodeKey)
        , outwardRef
        )
      | (nodeKey, outwardRef) <- IntMap.toList subst
      ]

    projectCertificate certificate =
      certificate
        { cerbcResultRef =
            renamePublishedRef renames (cerbcResultRef certificate)
        , cerbcBound =
            renameTypeBinderRefPayloads renames (cerbcBound certificate)
        }

completeCompilerExactSubtermResultsWithBounds
  :: [CompilerExactResultBoundCertificate]
  -> CompilerExactResultStage
  -> EdgeId
  -> SubtermGeneralizations
  -> XmlfTerm
  -> Either ElabError XmlfTerm
completeCompilerExactSubtermResultsWithBounds resultBoundCertificates stage exactEdge packets term = do
  (term', completedOwners) <- go term
  let expectedPackets =
        Map.filter
          (\packet ->
            subtermGeneralizationCompilerExactBoundary packet == Just exactEdge
              && isJust (subtermGeneralizationCompilerExactResultRef packet)
              && isJust (subtermGeneralizationCompilerExactCompletionRef packet)
              && subtermGeneralizationCompilerExactResultStage packet == Just stage
          )
          packets
      expectedOwners = Map.keysSet expectedPackets
      missingOwners = Set.toList (Set.difference expectedOwners completedOwners)
      unexpectedOwners = Set.toList (Set.difference completedOwners expectedOwners)
  case (missingOwners, unexpectedOwners) of
    ([], []) -> do
      delayedBinders <- traverse resultBinder (Map.toList expectedPackets)
      foldM installDelayedBinder term' delayedBinders
    _ ->
      Left
        ( ValidationFailed
            [ "compiler exact boundary did not consume exactly its delayed Gamma results"
            , "  exact edge: " ++ show exactEdge
            , "  missing owners: " ++ show missingOwners
            , "  unexpected owners: " ++ show unexpectedOwners
            ]
        )
  where
    go current =
      case current of
        EVarNode{} -> pure (current, Set.empty)
        ELit{} -> pure (current, Set.empty)
        ELam resolved body -> do
          (body', completed) <- go body
          let owner = idDetailsIdentityKey (resolvedVarDetails resolved)
          case Map.lookup owner packets of
            Just packet
              | subtermGeneralizationCompilerExactBoundary packet == Just exactEdge
              , subtermGeneralizationCompilerExactResultStage packet == Just stage
              , Just completionRef <- subtermGeneralizationCompilerExactCompletionRef packet ->
                  if Set.member owner completed
                    then duplicateOwner owner
                    else do
                      pure
                        ( ELam
                            resolved
                            ( if completionAlreadyApplied
                                  packet
                                  body'
                                then body'
                                else
                                  ETyInst
                                    body'
                                    (InstAbstrRef completionRef)
                            )
                        , Set.insert owner completed
                        )
            _ -> pure (ELam resolved body', completed)
        EApp fun arg -> do
          (fun', funCompleted) <- go fun
          (arg', argCompleted) <- go arg
          completed <- mergeCompleted funCompleted argCompleted
          pure (EApp fun' arg', completed)
        ELet resolved scheme rhs body -> do
          (rhs', rhsCompleted) <- go rhs
          (body', bodyCompleted) <- go body
          completed <- mergeCompleted rhsCompleted bodyCompleted
          pure
            ( ELet resolved scheme rhs' body'
            , completed
            )
        ETyAbsRef ref bound body -> do
          (body', completed) <- go body
          pure (ETyAbsRef ref bound body', completed)
        ETyInst body inst -> do
          (body', completed) <- go body
          pure (ETyInst body' inst, completed)
        ERoll ty body -> do
          (body', completed) <- go body
          pure (ERoll ty body', completed)
        EUnroll body -> do
          (body', completed) <- go body
          pure (EUnroll body', completed)

    resultBinder (owner, packet) = do
      packetResultRef <-
        case subtermGeneralizationCompilerExactResultRef packet of
          Just ref -> pure ref
          Nothing ->
            Left
              ( ValidationFailed
                  [ "compiler exact packet has no delayed Gamma result"
                  , "  exact edge: " ++ show exactEdge
                  ]
              )
      existingRef <-
        case subtermGeneralizationCompilerExactExistingRef packet of
          Just ref -> pure ref
          Nothing ->
            Left
              ( ValidationFailed
                  [ "compiler exact packet has no source construction binder"
                  , "  exact edge: " ++ show exactEdge
                  , "  packet result: " ++ show packetResultRef
                  ]
              )
      completionRef <-
        case subtermGeneralizationCompilerExactCompletionRef packet of
          Just ref -> pure ref
          Nothing ->
            Left
              ( ValidationFailed
                  [ "compiler exact packet has no semantic completion result"
                  , "  exact edge: " ++ show exactEdge
                  , "  packet result: " ++ show packetResultRef
                  ]
              )
      preparedBound <-
        case
          [ mbBound
          | (binderRef, mbBound) <-
              schemeBinderRefs
                (siScheme (subtermGeneralizationSchemeInfo packet))
          , typeBinderRefsSameIdentity binderRef packetResultRef
          ]
        of
          [Just bound] -> pure (Just bound)
          [Nothing] ->
            Left
              ( ValidationFailed
                  [ "compiler exact result abstraction has no operated bound"
                  , "  exact edge: " ++ show exactEdge
                  , "  packet result: " ++ show packetResultRef
                  , "  completion result: " ++ show completionRef
                  ]
              )
          [] ->
            Left
              ( ValidationFailed
                  [ "compiler exact result abstraction is absent from its packet"
                  , "  exact edge: " ++ show exactEdge
                  , "  packet result: " ++ show packetResultRef
                  , "  completion result: " ++ show completionRef
                  ]
              )
          _ ->
            Left
              ( ValidationFailed
                  [ "compiler exact result abstraction occurs more than once in its packet"
                  , "  exact edge: " ++ show exactEdge
                  , "  packet result: " ++ show packetResultRef
                  , "  completion result: " ++ show completionRef
                  ]
              )
      let matchingCertificates =
            [ certificate
            | certificate <- resultBoundCertificates
            , cerbcOwner certificate == owner
            , cerbcBoundary certificate == exactEdge
            , any
                (typeBinderRefsSameIdentity (cerbcResultRef certificate))
                [packetResultRef, existingRef, completionRef]
            ]
      (resultBound, resultBoundIsConstructed) <-
        case (stage, matchingCertificates) of
          (CompleteBeforeCompilerExact, [certificate]) -> do
            constructedBound <-
              case elabToBound (cerbcBound certificate) of
                Right bound -> pure bound
                Left cause ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact source result has a non-bound construction certificate"
                        , "  exact edge: " ++ show exactEdge
                        , "  owner: " ++ show owner
                        , "  result: " ++ show packetResultRef
                        , "  constructed bound: " ++ show (cerbcBound certificate)
                        , "  cause: " ++ cause
                        ]
                    )
            pure (Just constructedBound, True)
          (CompleteBeforeCompilerExact, []) ->
            pure (preparedBound, False)
          (CompleteBeforeCompilerExact, certificates) ->
            Left
              ( ValidationFailed
                  [ "compiler exact source result has multiple construction certificates"
                  , "  exact edge: " ++ show exactEdge
                  , "  owner: " ++ show owner
                  , "  result: " ++ show packetResultRef
                  , "  certificates: " ++ show certificates
                  ]
              )
          (CompleteAfterCompilerExact, []) ->
            pure (preparedBound, False)
          (CompleteAfterCompilerExact, certificates) ->
            Left
              ( ValidationFailed
                  [ "packet-only compiler exact result received a source-bound construction certificate"
                  , "  exact edge: " ++ show exactEdge
                  , "  owner: " ++ show owner
                  , "  result: " ++ show packetResultRef
                  , "  certificates: " ++ show certificates
                  ]
              )
      pure
        ( existingRef
        , completionRef
        , resultBound
        , resultBoundIsConstructed
        )

    -- A surrounding exact-root scheme may already publish the delayed graph
    -- identity.  For a source-owned result, the lambda constructor has also
    -- materialized Typ(body) before this boundary runs.  That constructed
    -- declaration is stronger than the packet's provisional opened bound, so
    -- retain it when the exact identity is already present.  A packet-only
    -- result still has no declaration before this boundary and therefore uses
    -- the prepared packet bound.  Keeping the prepared spine position also
    -- preserves dependencies between interleaved root and packet quantifiers.
    installDelayedBinder current delayed@(existingRef, completionRef, delayedBound, resultBoundIsConstructed) =
      case strengthenLeadingBinder current of
        Right (True, strengthened) -> pure strengthened
        Right (False, unchanged) ->
          pure (insertAfterLeadingTypeAbstractions delayed unchanged)
        Left err -> Left err
      where
        strengthenLeadingBinder term0 =
          case term0 of
            ETyAbsRef ref mbBound body
              | typeBinderRefsSameIdentity ref existingRef
                  || typeBinderRefsSameIdentity ref completionRef -> do
                  bound <-
                    mergeDelayedBound
                      resultBoundIsConstructed
                      ref
                      mbBound
                      delayedBound
                  let body' =
                        if typeBinderRefsSameIdentity ref completionRef
                          then body
                          else renameTermTypeVars [(ref, completionRef)] body
                  pure (True, ETyAbsRef completionRef bound body')
              | otherwise -> do
                  (found, body') <- strengthenLeadingBinder body
                  pure (found, ETyAbsRef ref mbBound body')
            ETyInst body inst -> do
              (found, body') <- strengthenLeadingBinder body
              pure (found, ETyInst body' inst)
            _ -> pure (False, term0)

        mergeDelayedBound constructedBound ref existing incoming =
          case (existing, incoming) of
            (Nothing, Just _) -> pure incoming
            (Just existingBound, Just incomingBound)
              | alphaEqType (tyToElab existingBound) (tyToElab incomingBound)
                  || churchAwareEqType
                      (tyToElab existingBound)
                      (tyToElab incomingBound) ->
                  pure existing
              | constructedBound ->
                  pure incoming
              | stage == CompleteBeforeCompilerExact ->
                  pure existing
            _ ->
              Left
                ( ValidationFailed
                    [ "compiler exact completion conflicts with an existing result binder"
                    , "  exact edge: " ++ show exactEdge
                    , "  result: " ++ show ref
                    , "  existing bound: " ++ show existing
                    , "  prepared bound: " ++ show incoming
                    ]
                )

    -- A compiler-exact packet carries the identity quotient from the source
    -- construction binder to its semantic completion binder.  If the
    -- producer already ends in Hyp(existing), installing the delayed binder
    -- below will rename that exact action to Hyp(completion); adding another
    -- Hyp(completion) here would construct the result twice.
    completionAlreadyApplied packet body =
      case body of
        ETyInst _ (InstAbstrRef appliedRef) ->
          any
            (typeBinderRefsSameIdentity appliedRef)
            ( maybeToList
                (subtermGeneralizationCompilerExactExistingRef packet)
                ++ maybeToList
                  (subtermGeneralizationCompilerExactCompletionRef packet)
            )
        _ -> False

    insertAfterLeadingTypeAbstractions delayed current =
      case current of
        ETyAbsRef ref mbBound body ->
          ETyAbsRef
            ref
            mbBound
            (insertAfterLeadingTypeAbstractions delayed body)
        _ ->
          let (_, ref, mbBound, _) = delayed
           in ETyAbsRef ref mbBound current

    mergeCompleted left right =
      case Set.toList (Set.intersection left right) of
        [] -> pure (Set.union left right)
        duplicate : _ -> duplicateOwner duplicate

    duplicateOwner :: ResolvedTermIdentityKey -> Either ElabError a
    duplicateOwner owner =
      Left
        ( ValidationFailed
            [ "compiler exact boundary encountered a duplicate delayed Gamma owner"
            , "  exact edge: " ++ show exactEdge
            , "  owner: " ++ show owner
            ]
        )

annAppSpine :: AnnExpr -> (AnnExpr, [AnnExpr])
annAppSpine annExpr =
  let go args expr =
        case stripAnnExpr expr of
          AApp fun arg _ _ _ -> go (arg : args) fun
          other -> (other, args)
   in go [] annExpr

transparentMediatorSignatureFor :: BindingKey -> AnnExpr -> Maybe ([(IdDetails, NodeId)], AnnExpr)
transparentMediatorSignatureFor rootParam = transparentMediatorBody rootParam Map.empty []
  where
    transparentMediatorBody root aliases etaParams expr =
      case stripAnnExpr expr of
        ALam _param mbDetails paramNode _ body _ _
          | let paramKey = annBinderKey mbDetails,
            paramKey == root
              || Map.member paramKey aliases
              || paramKey `elem` map (annBinderKey . fst) etaParams ->
              Nothing
          | otherwise ->
              transparentMediatorBody
                root
                aliases
                (etaParams ++ [(mbDetails, paramNode)])
                body
        ALet _boundName mbDetails _ _ _ _ rhs body _
          | let boundKey = annBinderKey mbDetails,
            boundKey == root
              || Map.member boundKey aliases
              || boundKey `elem` map (annBinderKey . fst) etaParams ->
              Nothing
          | Just origin <- transparentMediatorAliasOrigin root aliases etaParams rhs ->
              transparentMediatorBody root (Map.insert (annBinderKey mbDetails) origin aliases) etaParams body
          | otherwise ->
              Nothing
        other ->
          let (funExpr, argExprs) = annAppSpine other
           in if transparentMediatorHead root aliases funExpr
                && length argExprs == length etaParams
                && and
                  ( zipWith
                      (transparentMediatorArg aliases)
                      argExprs
                      (map (annBinderKey . fst) etaParams)
                  )
                then Just (etaParams, other)
                else Nothing

    transparentMediatorAliasOrigin root aliases etaParams rhs =
      case directAnnReferenceKey (stripAnnExpr rhs) of
        Just key
          | resolvedMediatorKey aliases key
              `elem` (root : map (annBinderKey . fst) etaParams) ->
              Just (resolvedMediatorKey aliases key)
        _ -> Nothing

    transparentMediatorHead root aliases expr =
      maybe False ((== root) . resolvedMediatorKey aliases) (directAnnReferenceKey (stripAnnExpr expr))

    transparentMediatorArg aliases expr expectedParam =
      maybe False ((== expectedParam) . resolvedMediatorKey aliases) (directAnnReferenceKey (stripAnnExpr expr))

    resolvedMediatorKey aliases key =
      case Map.lookup key aliases of
        Just origin -> origin
        Nothing -> key

transparentMediatorKindAnn :: AnnExpr -> Maybe TransparentMediatorKind
transparentMediatorKindAnn annExpr =
  case stripAnnExpr annExpr of
    ALam _rootParam mbDetails _ _ body _ _ ->
      case transparentMediatorSignatureFor (annBinderKey mbDetails) body of
        Just ([], _) -> Just DirectIdentityMediator
        Just (etaParams, _) -> Just (EtaTransparentMediator (length etaParams))
        Nothing -> Nothing
    _ -> Nothing

-- | Recognize a transparent mediator together with the arguments already
-- supplied to it.  @EtaTransparentMediator n@ has one distinguished function
-- subject followed by @n@ eta arguments.  When an application node is about
-- to supply the final eta argument, the function-side spine therefore has
-- exactly @n@ arguments and its first one is the source expression whose
-- application the mediator preserves.
--
-- This is identity-bearing: a let alias is accepted only through the resolved
-- 'BindingKey' stored in 'EnvBinding'.  Display names never participate.
transparentMediatorSpine
  :: Env
  -> AnnExpr
  -> Maybe (TransparentMediatorKind, Int, Maybe AnnExpr)
transparentMediatorSpine env ann = do
  let (headAnn, suppliedArgs) = annAppSpine ann
  kind <-
    transparentMediatorKindAnn headAnn
      <|> do
        key <- directAnnReferenceKey (stripAnnExpr headAnn)
        binding <- lookupEnvBindingForKey key env
        ebTransparentMediator binding
  let etaArity =
        case kind of
          DirectIdentityMediator -> 0
          EtaTransparentMediator arity -> arity
      suppliedCount = length suppliedArgs
  guard (suppliedCount <= etaArity)
  pure
    ( kind
    , etaArity - suppliedCount
    , case suppliedArgs of
        subject : _ -> Just subject
        [] -> Nothing
    )

transparentMediatorCompletionSubject :: Env -> AnnExpr -> Maybe AnnExpr
transparentMediatorCompletionSubject env functionAnn = do
  (_, remainingArity, mbSubject) <-
    transparentMediatorSpine env functionAnn
  guard (remainingArity == 0)
  mbSubject

-- | Specialize an exact source scheme at one exact application endpoint.
-- This is the source-side counterpart of the edge computation: quantified
-- arguments are inferred from the declared arrow and every step is checked by
-- the xMLF type checker.  When the parent owns the residual result endpoint,
-- use the complete arrow as the construction target.  This lets the argument
-- determine domain-only binders while the result determines result-only
-- binders, as required for partially applied @__io_bind@.
--
-- A bounded source binder can itself contain a quantified lower bound.  Its
-- construction is therefore @Inside phi ; N@, where @phi@ specializes that
-- lower bound to the inferred argument.  Treating the inferred argument as a
-- bare @InstApp@ would skip the paper's bound computation and fail precisely
-- when a Prelude method carries a dependent bounded spine.
sourceSchemeResultAt
  :: TypeCheck.Env
  -> ElabType
  -> Maybe ElabType
  -> SchemeInfo
  -> Maybe ElabType
sourceSchemeResultAt typeEnv argumentTy mbResultTy schemeInfo =
  snd
    <$> sourceSchemeApplicationAt
      typeEnv
      argumentTy
      mbResultTy
      schemeInfo

-- | Construct both endpoints of the source arrow selected at an application.
-- Keeping the domain together with the result is significant for structural
-- recursive types: the source scheme may validate a graph-domain projection
-- while its checked arrow retains the nominal structural binder identities.
-- Returning only the result would allow the caller to combine those exact
-- result identities with an unrelated graph-domain argument.
sourceSchemeApplicationAt
  :: TypeCheck.Env
  -> ElabType
  -> Maybe ElabType
  -> SchemeInfo
  -> Maybe (ElabType, ElabType)
sourceSchemeApplicationAt typeEnv argumentTy mbResultTy schemeInfo =
  specialize (schemeToType (siScheme schemeInfo))
  where
    typesAgree left right =
      alphaEqType left right || churchAwareEqType left right

    specialize currentTy =
      let currentScheme = schemeFromType currentTy
          currentBinders = schemeBinderRefs currentScheme
          currentBody = schemeBody currentScheme
          targetTy =
            maybe
              argumentTy
              (TArrow argumentTy)
              mbResultTy
          inferenceBody =
            case mbResultTy of
              Just _ -> currentBody
              Nothing ->
                case currentBody of
                  TArrow currentDomain _ -> currentDomain
                  _ -> currentBody
          inferred =
            inferInstAppArgsFromSchemeRefsExact
              currentBinders
              inferenceBody
              targetTy
       in case schemeBody currentScheme of
            TArrow {} ->
              case inferred of
                  Just arguments -> do
                    appliedTy <-
                      foldM applySourceArgument currentTy arguments
                    exposeAppliedArrow appliedTy
                  Nothing -> eliminateBoundedPrefix currentTy
            _ -> eliminateBoundedPrefix currentTy

    exposeAppliedArrow appliedTy =
      case appliedTy of
        TForallRef _ (Just _) _ -> do
          eliminatedTy <-
            either
              (const Nothing)
              Just
              ( TypeCheck.checkInstantiation
                  typeEnv
                  appliedTy
                  InstElim
              )
          exposeAppliedArrow eliminatedTy
        TForallRef _ Nothing _ -> Nothing
        TArrow appliedDomain appliedResult
          | typesAgree appliedDomain argumentTy
          , maybe
              True
              (typesAgree appliedResult)
              mbResultTy ->
              Just (appliedDomain, appliedResult)
        _ -> Nothing

    eliminateBoundedPrefix currentTy =
      case currentTy of
        TForallRef _ (Just _) _ -> do
          eliminatedTy <-
            either
              (const Nothing)
              Just
              ( TypeCheck.checkInstantiation
                  typeEnv
                  currentTy
                  InstElim
              )
          specialize eliminatedTy
        _ -> Nothing

    applySourceArgument currentTy argumentTy' = do
      instantiation <-
        sourceArgumentInstantiation currentTy argumentTy'
      either
        (const Nothing)
        Just
        ( TypeCheck.checkInstantiation
            typeEnv
            currentTy
            instantiation
        )

    sourceArgumentInstantiation currentTy argumentTy' =
      case currentTy of
        TForallRef _ Nothing _ ->
          Just (InstApp argumentTy')
        TForallRef _ (Just bound) _
          | typesAgree argumentTy' (tyToElab bound) ->
              Just InstElim
          | otherwise -> do
              inside <-
                constructSourceInstantiation
                  (tyToElab bound)
                  argumentTy'
              pure (InstSeq (InstInside inside) InstElim)
        _ -> Nothing

    constructSourceInstantiation sourceTy targetTy
      | typesAgree sourceTy targetTy =
          Just InstId
      | TBottom <- sourceTy =
          Just (InstBot targetTy)
      | otherwise = do
          let sourceScheme = schemeFromType sourceTy
              sourceBinders = schemeBinderRefs sourceScheme
          arguments <-
            inferInstAppArgsFromSchemeRefsExact
              sourceBinders
              (schemeBody sourceScheme)
              targetTy
          (instantiation, appliedTy) <-
            foldM
              applyNestedSourceArgument
              (InstId, sourceTy)
              arguments
          guard (typesAgree appliedTy targetTy)
          pure instantiation

    applyNestedSourceArgument
      (prefix, currentTy)
      argumentTy' = do
        step <- sourceArgumentInstantiation currentTy argumentTy'
        appliedTy <-
          either
            (const Nothing)
            Just
            ( TypeCheck.checkInstantiation
                typeEnv
                currentTy
                step
            )
        pure (composeInst prefix step, appliedTy)

-- | Recover the exact source application hidden by a completed eta mediator.
-- For @apply g g@, the function-side spine proves that the result is the source
-- application @g g@; the two independently published source schemes then fix
-- both its argument and result to the closed sigma-id type.  No graph result or
-- failed type check is used as an oracle.
mediatedApplicationConstruction
  :: Env
  -> (AnnExpr -> Either ElabError (Maybe SchemeInfo))
  -> AnnExpr
  -> Either ElabError (Maybe MediatedApplicationConstruction)
mediatedApplicationConstruction env sourceSchemeFor applicationAnn =
  case stripAnnExpr applicationAnn of
    AApp functionAnn argumentAnn _ _ _ ->
      case transparentMediatorCompletionSubject env functionAnn of
        Nothing -> pure Nothing
        Just subjectAnn -> do
          mbSubjectSchemeInfo <- sourceSchemeFor subjectAnn
          mbArgumentSchemeInfo <- sourceSchemeFor argumentAnn
          pure $ do
            subjectSchemeInfo <- mbSubjectSchemeInfo
            argumentSchemeInfo <- mbArgumentSchemeInfo
            let argumentTy =
                  schemeToType (siScheme argumentSchemeInfo)
            guard (null (freeTypeVarRefsType argumentTy))
            resultTy <-
              sourceSchemeResultAt
                (typeCheckEnvFrom env)
                argumentTy
                Nothing
                subjectSchemeInfo
            guard (null (freeTypeVarRefsType resultTy))
            pure
              MediatedApplicationConstruction
                { macArgumentType = argumentTy
                , macResultType = resultTy
                }
    _ -> pure Nothing

-- | A source scheme may be observed by the solver through an opened graph
-- projection.  Prove that relationship by exact binder inference before a
-- source-owned closed scheme replaces the provisional projection.
isExactSchemeProjection :: ElabType -> ElabType -> Bool
isExactSchemeProjection closedTy projectedTy =
  alphaEqType closedTy projectedTy
    || case schemeBinderRefs closedScheme of
      [] -> False
      binders ->
        case
            inferInstAppArgsFromSchemeRefsExact
              binders
              (schemeBody closedScheme)
              projectedTy
          of
            Just arguments ->
              length arguments == length binders
                && all isGraphProjectionArgument arguments
            Nothing -> False
  where
    closedScheme = schemeFromType closedTy
    isGraphProjectionArgument argumentTy =
      case argumentTy of
        TVarRef ref -> isJust (typeBinderRefNode ref)
        _ -> False

-- | Refine the exact packet-owned result declaration before the lambda body is
-- elaborated.  The source mediator certificate is stronger than the solver's
-- opened graph projection, but it may replace that projection only after the
-- packet proves which binder this lambda owns and exact scheme inference proves
-- that the old bound is an opening of the closed source scheme.
refineMediatedLambdaConsumerScheme
  :: EdgeId
  -> PreparedSubtermGeneralization
  -> SchemeInfo
  -> MediatedApplicationConstruction
  -> Either ElabError (SchemeInfo, Maybe (TypeBinderRef, ElabType))
refineMediatedLambdaConsumerScheme bodyEdge packet schemeInfo construction =
  case matchingBinders of
    [] -> pure (schemeInfo, Nothing)
    [(resultRef, mbExistingBound)] -> do
      case mbExistingBound of
        Nothing -> pure ()
        Just existingBound
          | isExactSchemeProjection resultTy (tyToElab existingBound) ->
              pure ()
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "source-proven mediated result disagrees with its packet Gamma bound"
                    , "  body edge: " ++ show bodyEdge
                    , "  result binder: " ++ show resultRef
                    , "  packet bound: " ++ show (tyToElab existingBound)
                    , "  source result: " ++ show resultTy
                    ]
                )
      resultBound <-
        case elabToBound resultTy of
          Right bound -> pure bound
          Left cause ->
            Left
              ( ValidationFailed
                  [ "source-proven mediated result is not a Gamma bound"
                  , "  body edge: " ++ show bodyEdge
                  , "  result binder: " ++ show resultRef
                  , "  source result: " ++ show resultTy
                  , "  cause: " ++ cause
                  ]
              )
      let refinedScheme =
            mkElabSchemeWithRefs
              [ if typeBinderRefsSameIdentity ref resultRef
                  then (ref, Just resultBound)
                  else binder
              | binder@(ref, _) <- schemeBinderRefs (siScheme schemeInfo)
              ]
              (schemeBody (siScheme schemeInfo))
      pure
        ( schemeInfoFromRefSubst
            refinedScheme
            (schemeInfoBinderRefSubst schemeInfo)
        , Just (resultRef, resultTy)
        )
    _ ->
      Left
        ( ValidationFailed
            [ "source-proven mediated result selects multiple packet Gamma binders"
            , "  body edge: " ++ show bodyEdge
            , "  candidates: " ++ show matchingBinders
            ]
        )
  where
    resultTy = macResultType construction
    constructionRefs =
      maybeToList
        (subtermGeneralizationConstructionResultAbstractionRef packet)
        ++ maybeToList
          (subtermGeneralizationResultAbstractionRef packet)
        ++ maybe
          []
          (\consumerIdentity ->
            schemeConsumerConstructionRefs consumerIdentity schemeInfo
          )
          (subtermGeneralizationConsumerIdentity packet)
    matchingBinders =
      [ binder
      | binder@(ref, _) <- schemeBinderRefs (siScheme schemeInfo)
      , any (typeBinderRefsSameIdentity ref) constructionRefs
      ]

installRefinedTypeBinding
  :: (TypeBinderRef, ElabType)
  -> Env
  -> Either ElabError Env
installRefinedTypeBinding (refinedRef, refinedTy) env
  | matchingRefs@(_ : _) <-
      filter
        (typeBinderRefsSameIdentity refinedRef)
        (Map.keys (envTypeBindings env)) =
      pure
        env
          { envTypeBindings =
              Map.mapWithKey
                (\ref ty ->
                  if typeBinderRefsSameIdentity ref refinedRef
                    then refinedTy
                    else ty
                )
                (envTypeBindings env)
          , envSourceRefinedGammaBounds =
              foldr
                (\ref -> Map.insert ref refinedTy)
                (envSourceRefinedGammaBounds env)
                matchingRefs
          }
  | otherwise =
      Left
        ( ValidationFailed
            [ "refined packet Gamma binder is absent from the lambda construction environment"
            , "  result binder: " ++ show refinedRef
            , "  environment binders: " ++ show (Map.keys (envTypeBindings env))
            ]
        )

elabAlg :: AlgebraContext p -> AnnExprF (AnnExpr, ElabOut) -> ElabOut
elabAlg algebraContext layer =
  case layer of
    AResolvedVarF details v _ -> mkOut $ \env ->
      maybe
        (Left (EnvLookup v))
        (Right . EVarNode . resolvedEnvBindingVar)
        (lookupEnvBindingForDetails details env)
    ALitF lit _ -> mkOut $ \_ -> Right (ELit lit)
    ALamF v mbBinderDetails paramNode lambdaScopeGen (bodyAnn, bodyOut) bodyEid lamNodeId ->
      let binderDetails = resolvedTermBinderDetails v mbBinderDetails
          f env = do
            let mAnnLambda = desugaredAnnLambdaInfo mbBinderDetails bodyAnn
                resolvedParam = algResolvedLambdaParamNode algebraContext lamNodeId
                constructedParamTy =
                  Map.lookup
                    (envBindingDetailsKey binderDetails)
                    (envConstructedLambdaParamTypes env)
                exactEvidenceParamSourceTy =
                  if idDetailsIsEvidence binderDetails
                    then
                      IntMap.lookup
                        (getNodeId (scCanonical scopeContext paramNode))
                        (algExactLambdaParamSourceTypes algebraContext)
                    else Nothing
                -- The solved arrow is the construction authority for a lambda.
                -- Its domain may legitimately reify as a bare internal variable;
                -- that does not make the original allocation node authoritative
                -- again.  Falling back here can disconnect the emitted binder
                -- from an enclosing application's retained arrow topology.
                paramSource = fromMaybe paramNode resolvedParam
                preparedLambdaGeneralization =
                  Map.lookup
                    (idDetailsIdentityKey binderDetails)
                    (algSubtermGeneralizations algebraContext)
                preparedBodyPackets =
                  Map.elems
                    ( subtermGeneralizationsOwnedBy
                        bodyAnn
                        (algSubtermGeneralizations algebraContext)
                    )
                preparedBodyResultOwnership =
                  subtermResultOwnershipFor
                    bodyAnn
                    (algSubtermGeneralizations algebraContext)
                preparedPacketParamTy = do
                  packet <- preparedLambdaGeneralization
                  let packetSchemeInfo =
                        subtermGeneralizationConsumerConstructionSchemeInfo packet
                  case schemeBody (siScheme packetSchemeInfo) of
                    TArrow domainTy _ -> Just domainTy
                    _ -> do
                      packetRef <-
                        IntMap.lookup
                          (getNodeId (scCanonical scopeContext paramSource))
                          (schemeInfoBinderRefSubst packetSchemeInfo)
                      if
                          any
                            (typeBinderRefsSameIdentity packetRef . fst)
                            (schemeBinderRefs (siScheme packetSchemeInfo))
                        then Just (TVarRef packetRef)
                        else Nothing
            exactEvidenceParamTy <-
              traverse
                (srcTypeToElabType algebraContext)
                exactEvidenceParamSourceTy
            let paramBoundaryAuthority =
                  case exactEvidenceParamTy of
                    Just exactTy ->
                      Just
                        ( ExactSourceLambdaParamBoundary
                            exactTy
                            constructedParamTy
                        )
                    Nothing ->
                      ConstructedLambdaParamBoundary
                        <$> constructedParamTy
            ( installedParamTy
              , paramBoundaryProtectedIdentities
              , envAfterConstructedParam
              ) <-
              let consumedEnv =
                    env
                      { envConstructedLambdaParamTypes =
                          Map.delete
                            (envBindingDetailsKey binderDetails)
                            (envConstructedLambdaParamTypes env)
                      }
               in case paramBoundaryAuthority of
                    Just authority -> do
                      (paramTy, localBinderIdentities, installedEnv) <-
                        installConstructedLambdaParamBound
                          paramSource
                          authority
                          consumedEnv
                      pure
                        ( Just paramTy
                        , localBinderIdentities
                        , installedEnv
                        )
                    Nothing ->
                      pure (Nothing, Set.empty, consumedEnv)
            preparedBodyGeneralization <-
              case preparedBodyPackets of
                [] -> pure Nothing
                [packet] -> pure (Just packet)
                packets ->
                  Left
                    ( ValidationFailed
                        [ "lambda body has multiple direct construction packets"
                        , "  owner: " ++ show (idDetailsIdentityKey binderDetails)
                        , "  packets: " ++ show (map subtermGeneralizationSchemeInfo packets)
                        ]
                    )
            let currentLambdaIsTransparent =
                  isJust
                    ( transparentMediatorKindAnn
                        ( ALam
                            v
                            mbBinderDetails
                            paramNode
                            lambdaScopeGen
                            bodyAnn
                            bodyEid
                            lamNodeId
                        )
                    )
                preparedConstructionPackets =
                  [ (packet, True)
                  | packet <- maybeToList preparedLambdaGeneralization
                  ]
                    ++ [ (packet, False)
                       | packet <- maybeToList preparedBodyGeneralization
                       , currentLambdaIsTransparent
                       ]
                directSourceBinderRefs =
                  IntMap.restrictKeys
                    (acSourceBinderRefs annotationContext)
                    (acDirectSourceBinderKeys annotationContext)
                -- A direct child packet can publish a construction binder in
                -- its completed scheme even when it has no compiler-exact
                -- source rename.  Enter each proved lexical layer before
                -- constructing the child so an enclosing lambda parameter and
                -- every nested occurrence use one identity-bearing type.
                -- Parent and child routes compose in order.  The parent's
                -- packet-local source quotient is active here; a direct
                -- child's quotient is not active until that child reaches its
                -- own boundary, so the parent admits only declaration-backed
                -- routes from the child's completed scheme.
                enterConstructionPacket currentEnv (packet, includePacketRoutes) = do
                  let constructionInfo =
                        subtermGeneralizationConsumerConstructionSchemeInfo packet
                      localSourceDeclarationRefs =
                        case preparedBodyResultOwnership of
                          Just ownership
                            | not includePacketRoutes
                            , subtermResultOwnershipPacket ownership == packet ->
                                subtermResultOwnershipLocalSourceDeclarationRefs
                                  directSourceBinderRefs
                                  ownership
                          _ -> []
                      packetRenames0 =
                        lambdaParamConstructionRenames
                          paramBoundaryProtectedIdentities
                          ( if includePacketRoutes
                              then
                                subtermGeneralizationConstructionBinderRenames
                                  packet
                              else
                                schemeConstructionAuthorityRenames
                                  directSourceBinderRefs
                                  currentEnv
                                  constructionInfo
                          )
                      -- The direct child owns these source declarations at
                      -- its exact lambda boundary.  Their sidecar identities
                      -- justify the completed packet's declaration shape,
                      -- but entering either the binder or its graph quotient
                      -- here would put the same variable in the enclosing
                      -- Gamma before the child emits its ETyAbsRef.
                      packetRenames =
                        [ rename
                        | rename@(_, outwardRef) <- packetRenames0
                        , not
                            ( any
                                (typeBinderRefsSameIdentity outwardRef)
                                localSourceDeclarationRefs
                            )
                        ]
                      constructionBinders =
                        [ binder
                        | binder@(binderRef, _) <-
                            schemeBinderRefs (siScheme constructionInfo)
                        , not
                            ( any
                                (typeBinderRefsSameIdentity binderRef)
                                localSourceDeclarationRefs
                            )
                        ]
                  alignedEnv <-
                    alignEnvToNestedConstructionBinderRenames
                      packetRenames
                      currentEnv
                  -- A transparent parent is the lexical construction scope
                  -- for its direct child's completed scheme.  Non-identity
                  -- routes alone cannot install a packet whose outward
                  -- binder deliberately keeps its graph identity, so enter
                  -- the packet's exact declarations here.  They remain
                  -- ambient to the lambda's own Gen(Gamma, tau) and are
                  -- closed once, by the enclosing let construction; exact
                  -- source declarations owned by that lambda were excluded
                  -- above.
                  pure $
                    if includePacketRoutes
                      then alignedEnv
                      else
                        extendEnvTypeScopeWithAliases
                          (schemeInfoBinderRefSubst constructionInfo)
                          constructionBinders
                          alignedEnv
            envAtLambdaBoundary <-
              foldM
                enterConstructionPacket
                envAfterConstructedParam
                preparedConstructionPackets
            let bodyElabOut =
                  case mAnnLambda of
                    Just (_, _, _, innerBodyAnn) -> para (elabAlg algebraContext) innerBodyAnn
                    Nothing -> bodyOut
            let preservedAnnotationTyRaw =
                  case mAnnLambda of
                    Just (_, _, annotationEdge, _) ->
                      IntMap.lookup
                        (getEdgeId annotationEdge)
                        (algAnnotationExpectedTypesByEdge algebraContext)
                    Nothing -> Nothing
            let enclosingConstructionBinderRenames =
                  envConstructionBinderRenames envAtLambdaBoundary
                locallyOwnedConsumerIdentities packet =
                  Set.fromList
                    [ consumerIdentity
                    | consumerIdentity <-
                        maybeToList
                          (subtermGeneralizationConsumerIdentity packet)
                    , let consumerRef =
                            typeBinderRefFromIdentity
                              consumerIdentity
                              (typeBinderIdentityStableName consumerIdentity)
                    , not
                        ( envOwnsExactTypeBinderRef
                            envAtLambdaBoundary
                            consumerRef
                        )
                    ]
                -- The completed packet is the preparation-time ownership
                -- proof for its declared quantifiers. Packet-local
                -- declarations shadow representative-equivalent ambient
                -- aliases, but an exact identity already free in the
                -- enclosing Gamma is ambient and must still be subtracted by
                -- Gen(Gamma, tau). A pending consumer is deliberately not
                -- admitted by this set.
                packetOwnedDeclarationIdentities packet =
                  Set.fromList
                    [ typeBinderRefIdentity ref
                    | (ref, _) <-
                        schemeBinderRefs
                          ( siScheme
                              ( applySchemeInfoRefRenames
                                  enclosingConstructionBinderRenames
                                  (subtermGeneralizationSchemeInfo packet)
                              )
                          )
                    , not
                        ( envOwnsExactTypeBinderRef
                            envAtLambdaBoundary
                            ref
                        )
                    ]
                preservedAnnotationTy =
                  applyTypeVarRefRenames enclosingConstructionBinderRenames
                    <$> preservedAnnotationTyRaw
            paramTySurface0 <-
              case exactEvidenceParamTy of
                Just exactTy -> pure exactTy
                Nothing ->
                  case preservedAnnotationTy of
                    Just sourceTy -> pure sourceTy
                    Nothing ->
                      -- A compiler-exact endpoint is the construction contract
                      -- for this lambda spine.  Its domain must therefore win
                      -- over a provisional graph-derived parameter slot (which
                      -- is commonly still Bottom for an opened forall).  Merely
                      -- validating and repairing that slot at the enclosing
                      -- exact annotation loses the binder identity before the
                      -- lambda is built.
                      case
                          envExpectedTermEndpoint envAtLambdaBoundary
                            >>= exactConstructionExpectedType
                        of
                          Just (TArrow expectedDomain _) -> pure expectedDomain
                          _ ->
                            case installedParamTy of
                              Just constructionTy ->
                                pure
                                  ( applyTypeVarRefRenames
                                      enclosingConstructionBinderRenames
                                      constructionTy
                                  )
                              Nothing ->
                                case preparedPacketParamTy of
                                  Just packetTy ->
                                    pure
                                      ( applyTypeVarRefRenames
                                          enclosingConstructionBinderRenames
                                          packetTy
                                      )
                                  Nothing ->
                                    case
                                        expectedTermEndpointType
                                          <$> envExpectedTermEndpoint envAtLambdaBoundary
                                      of
                                      Just (TArrow expectedDomain _) -> pure expectedDomain
                                      _ -> reifyNodeTypePreferringBound scopeContext paramSource
            let paramTySurface = paramTySurface0
            (_paramTyUnaligned, paramSchemeInfoUnaligned) <-
              case mAnnLambda of
                Just (_, annNodeId, _, _) ->
                  -- First, check if we have the original source annotation type
                  -- preserved from constraint generation.  This is the exact type
                  -- the user wrote (after lowering), which presolution may have
                  -- corrupted (e.g. stripping TForall inside a μ body).
                  case preservedAnnotationTy of
                    Just preservedTy -> do
                      pure
                        ( preservedTy,
                          schemeInfoFromRefSubst (schemeFromType preservedTy) IntMap.empty
                        )
                    Nothing ->
                      case generalizeAtNode scopeContext annNodeId of
                        Right (paramScheme, _subst) ->
                          let paramTy0 = case (schemeBinderRefs paramScheme, schemeBody paramScheme) of
                                ([(ref, Just bnd)], TVarRef bodyRef)
                                  | typeBinderRefsSameIdentity ref bodyRef -> tyToElab bnd
                                _ -> schemeToType paramScheme
                              -- If generalizeAtNode returned a bare TVar (over-generalized)
                              -- or a base type that disagrees with the constraint graph's
                              -- solved μ type, fall back to reifyNodeTypePreferringBound.
                              -- This handles the case where ELamAnn's desugared
                              -- annotation-let picks up the body's result type (e.g. Bool)
                              -- instead of the actual annotation type (e.g. μ Nat).
                              paramTyResolved = case paramTy0 of
                                TVarRef {} ->
                                  case reifyNodeTypePreferringBound scopeContext annNodeId of
                                    Right ty@TMuRef {} -> ty
                                    _ -> paramTy0
                                TBaseWithIdentity {} ->
                                  case reifyNodeTypePreferringBound scopeContext annNodeId of
                                    Right ty@TMuRef {} -> ty
                                    _ -> paramTy0
                                _
                                  | TMuRef {} <- paramTySurface,
                                    Just unfoldedSurface <- unfoldMuOnce paramTySurface,
                                    (alphaEqType unfoldedSurface paramTy0 || churchAwareEqType unfoldedSurface paramTy0) ->
                                      paramTySurface
                                _ -> paramTy0
                           in pure
                                ( paramTyResolved,
                                  schemeInfoFromRefSubst (schemeFromType paramTyResolved) IntMap.empty
                                )
                        Left (SchemeFreeVars _ _) ->
                          pure
                            ( paramTySurface,
                              schemeInfoFromRefSubst (schemeFromType paramTySurface) IntMap.empty
                            )
                        Left err -> Left err
                Nothing ->
                  case exactEvidenceParamTy of
                    Just exactTy ->
                      pure
                        ( exactTy,
                          schemeInfoFromRefSubst (schemeFromType exactTy) IntMap.empty
                        )
                    Nothing ->
                      pure
                        ( paramTySurface,
                          schemeInfoFromRefSubst (schemeFromType paramTySurface) IntMap.empty
                        )
            preparedLambdaSchemeInfo0 <-
              traverse
                ( \packet ->
                    freshenSchemeInfoAgainstConstructionEnvExcept
                      ( Set.union
                          (packetOwnedDeclarationIdentities packet)
                          (locallyOwnedConsumerIdentities packet)
                      )
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      (acSourceBinderRefs annotationContext)
                      envAtLambdaBoundary
                      ( applySchemeInfoRefRenames
                          enclosingConstructionBinderRenames
                          (subtermGeneralizationConsumerConstructionSchemeInfo packet)
                      )
                )
                preparedLambdaGeneralization
            envAtLambdaConstructionBoundary <-
              case preparedLambdaSchemeInfo0 of
                Just schemeInfo ->
                  alignEnvToNestedConstructionBinderRenames
                    ( schemeConstructionAuthorityRenames
                        directSourceBinderRefs
                        envAtLambdaBoundary
                        schemeInfo
                    )
                    envAtLambdaBoundary
                Nothing -> pure envAtLambdaBoundary
            let constructionBinderRenames =
                  envConstructionBinderRenames
                    envAtLambdaConstructionBoundary
                -- Method-local forall declarations remain protected below,
                -- but a free evidence identity may already have been routed
                -- by the enclosing Figure 15.3.5 construction before this
                -- lambda is entered.  Snapshot that outer proof before
                -- adding the lambda/body packets and apply it to the
                -- parameter in the same direction as its occurrences.
                -- Otherwise the body is constructed at the outward Gamma
                -- identity while the evidence binder retains the pre-route
                -- source identity.
                exactEvidenceOuterConstructionRenames =
                  case exactEvidenceParamTy of
                    Nothing -> []
                    Just exactTy ->
                      [ rename
                      | rename@(sourceRef, _) <-
                          envConstructionBinderRenames envAfterConstructedParam
                      , any
                          (typeBinderRefsSameIdentity sourceRef)
                          (freeTypeVarRefsType exactTy)
                      ]
                envWithLambdaTypeScope0 =
                  case preparedLambdaSchemeInfo0 of
                    Just schemeInfo ->
                      extendEnvTypeScopeWithAliases
                        (schemeInfoBinderRefSubst schemeInfo)
                        (schemeBinderRefs (siScheme schemeInfo))
                        envAtLambdaConstructionBoundary
                    Nothing -> envAtLambdaConstructionBoundary
            let paramSchemeInfoForConstruction =
                  applySchemeInfoRefRenames
                    ( exactEvidenceOuterConstructionRenames
                        ++ lambdaParamConstructionRenames
                          paramBoundaryProtectedIdentities
                          constructionBinderRenames
                    )
                    paramSchemeInfoUnaligned
            paramSchemeInfo <-
              -- The packet's own result consumer is not part of the
              -- environment used to type the lambda parameter.  In
              -- particular, the source forall in @id : forall a. a -> a@
              -- remains the parameter authority; a peer graph result created
              -- for the lambda body must not capture that source identity
              -- merely because both nodes share a solved representative.
              generalizeSchemeInfoAgainstConstructionEnvPreserving
                paramBoundaryProtectedIdentities
                ( maybe
                    Set.empty
                    locallyOwnedConsumerIdentities
                    preparedLambdaGeneralization
                )
                (scopeTypeBinderIdentityRepresentative scopeContext)
                (acSourceBinderRefs annotationContext)
                envWithLambdaTypeScope0
                paramSchemeInfoForConstruction
            let paramTy = schemeToType (siScheme paramSchemeInfo)
                -- The installed boundary protects both declarations and free
                -- exact-source identities while recursively constructing the
                -- body.  Only its leading forall declarations, however, are
                -- immune to the exact body packet's outgoing quotient.  A
                -- free identity is an occurrence route (for example
                -- Generated7 -> Graph0), not a binder shadow.
                paramBoundaryLexicalBinderIdentities =
                  Set.fromList
                    [ typeBinderRefIdentity ref
                    | (ref, _) <-
                        schemeBinderRefs
                          (schemeFromType paramTy)
                    ]
                paramBinding =
                  (mkLocalEnvBinding v binderDetails paramSchemeInfo Nothing)
                    { ebExplicitRecursiveParam =
                        isJust mAnnLambda && hasContractiveRecursiveWitness paramTy
                    }
                installLambdaParamBinding lambdaEnv =
                  let envWithLambdaParam0 =
                        insertEnvBinding paramBinding lambdaEnv
                   in case mAnnLambda of
                        Just (mediatorDetails, _, _, _) ->
                          insertEnvBindingIdentityAlias
                            mediatorDetails
                            paramBinding
                            envWithLambdaParam0
                        Nothing -> envWithLambdaParam0
                envWithLambdaParamInitial =
                  installLambdaParamBinding envWithLambdaTypeScope0
                lambdaActiveConsumerAuthority =
                  ( preparedLambdaGeneralization
                      >>= subtermGeneralizationConsumerAuthority
                  )
                    <|> ( envActiveSubtermConstruction env
                            >>= subtermGeneralizationConsumerAuthority
                        )
                lambdaEnclosingConsumerAuthority =
                  envActiveSubtermConstruction env
                    >>= subtermGeneralizationConsumerAuthority
                lambdaActiveConsumerIdentity =
                  scaConsumerIdentity <$> lambdaActiveConsumerAuthority
                lambdaResolvedLookup details =
                  ebSchemeInfo
                    <$> lookupEnvBindingForDetails
                      details
                      envWithLambdaParamInitial
                lambdaSourceSchemeFor =
                  sourceSchemeInfoForConstruction
                    lambdaActiveConsumerAuthority
                    lambdaEnclosingConsumerAuthority
                    annotationContext
                    (algNamedSetReify algebraContext)
                    lambdaResolvedLookup
                lambdaConstructionBodyAnn =
                  case mAnnLambda of
                    Just (_, _, _, innerBodyAnn) -> innerBodyAnn
                    Nothing -> bodyAnn
                lambdaScope = GenRef lambdaScopeGen
                lambdaOwner =
                  LocalGammaOwner
                    { lgoConstructor = LocalLambdaGamma,
                      lgoBoundaryEdge = bodyEid,
                      lgoTermNode = lamNodeId,
                      lgoScope = lambdaScope
                    }
            mediatedLambdaConstruction <-
              mediatedApplicationConstruction
                envWithLambdaParamInitial
                lambdaSourceSchemeFor
                lambdaConstructionBodyAnn
            exactLambdaBodyPacket <-
              selectExactLambdaBodyPacket
                lambdaOwner
                bodyEid
                [ preparedLambdaGeneralization
                , preparedBodyGeneralization
                ]
            let exactLambdaBodyPacketConstructionRenames =
                  maybe
                    []
                    subtermGeneralizationConstructionBinderRenames
                    exactLambdaBodyPacket
            (preparedLambdaSchemeInfo, mbRefinedGammaBinding) <-
              case
                  ( preparedLambdaGeneralization
                  , exactLambdaBodyPacket
                  , preparedLambdaSchemeInfo0
                  , mediatedLambdaConstruction
                  )
                of
                  ( Just packet
                    , Just exactPacket
                    , Just schemeInfo
                    , Just construction
                    )
                      | packet == exactPacket -> do
                          (refinedSchemeInfo, mbRefinedBinding) <-
                            refineMediatedLambdaConsumerScheme
                              bodyEid
                              packet
                              schemeInfo
                              construction
                          pure (Just refinedSchemeInfo, mbRefinedBinding)
                  _ -> pure (preparedLambdaSchemeInfo0, Nothing)
            envWithLambdaTypeScope <-
              case mbRefinedGammaBinding of
                Just refinedBinding ->
                  installRefinedTypeBinding
                    refinedBinding
                    envWithLambdaTypeScope0
                Nothing -> pure envWithLambdaTypeScope0
            let envWithLambdaParam =
                  installLambdaParamBinding envWithLambdaTypeScope
                mediatedBodyResultTy =
                  case (mediatedLambdaConstruction, mbRefinedGammaBinding) of
                    (Just construction, Just _) ->
                      Just (macResultType construction)
                    _ -> Nothing
                lambdaEdgeArtifacts = acEdgeArtifacts annotationContext
                preparedPacketBodyExpectedEndpoint packet =
                  selectExpectedTermEndpoint
                    (scopedTypesAgree scopeContext)
                    ("lambda packet body edge " ++ show bodyEid)
                    [ ExactConstructionExpectedTerm
                        <$> ( mediatedBodyResultTy
                                <|> packetResultExpectedType packet
                            )
                    , CheckingExpectedTerm
                        <$> topologyBodyCheckExpectedType packet
                    , CheckingExpectedTerm
                        <$> packetLambdaCodomain packet
                    ]
                -- S'(operated) is an independent expectation for checking the
                -- recursively constructed child.  It is not the consumer's
                -- Gamma bound: only the checked child source can complete that
                -- pending declaration in 'materializeConsumerBound'.
                topologyBodyCheckExpectedType packet = do
                  authority <- subtermGeneralizationConsumerAuthority packet
                  if not (subtermConsumerAuthorityIsTopology authority)
                    then Nothing
                    else do
                      let consumerIdentity = scaConsumerIdentity authority
                          directRef =
                            typeBinderRefFromIdentity
                              consumerIdentity
                              (typeBinderIdentityStableName consumerIdentity)
                          constructionInfo =
                            subtermGeneralizationConsumerConstructionSchemeInfo packet
                          constructionRef =
                            case typeBinderRefNode directRef of
                              Just consumerNode ->
                                IntMap.findWithDefault
                                  directRef
                                  (getNodeId consumerNode)
                                  (schemeInfoBinderRefSubst constructionInfo)
                              Nothing -> directRef
                      if typeBinderRefsSameIdentity directRef constructionRef
                        then
                          Just
                            ( applyTypeVarRefRenames
                                constructionBinderRenames
                                ( schemeToType
                                    ( siScheme
                                        (subtermGeneralizationOperatedSchemeInfo packet)
                                    )
                                )
                            )
                        else Nothing
                packetResultExpectedType packet = do
                  resultRef <- subtermGeneralizationResultAbstractionRef packet
                  mbBound <-
                    lookupBinderBound
                      resultRef
                      (subtermGeneralizationSchemeInfo packet)
                      <|> lookupBinderBound
                        resultRef
                        ( subtermGeneralizationConsumerConstructionSchemeInfo
                            packet
                        )
                  bound <- mbBound
                  pure (tyToElab bound)
                packetLambdaCodomain packet =
                  case schemeBody constructionScheme of
                    TArrow _ codomainTy@(TVarRef codomainRef) ->
                      case lookupBinderBound codomainRef constructionInfo of
                        Just (Just codomainBound) -> Just (tyToElab codomainBound)
                        _ -> Just codomainTy
                    TArrow _ codomainTy -> Just codomainTy
                    _ -> Nothing
                  where
                    constructionInfo =
                      subtermGeneralizationConsumerConstructionSchemeInfo packet
                    constructionScheme = siScheme constructionInfo
                expectedLambdaCodomain = go False
                  where
                    go unfoldedMu expectedTy =
                      case expectedTy of
                        TArrow _ codomainTy -> Just codomainTy
                        TForallRef _ _ bodyTy -> go unfoldedMu bodyTy
                        muTy@TMuRef {}
                          | not unfoldedMu
                          , hasContractiveRecursiveWitness muTy ->
                              unfoldMuOnce muTy >>= go True
                        _ -> Nothing
                lookupBinderBound ref schemeInfo =
                  snd
                    <$> find
                      (typeBinderRefsSameIdentity ref . fst)
                      (schemeBinderRefs (siScheme schemeInfo))
            preparedPacketBodyEndpoint <-
              case exactLambdaBodyPacket of
                Nothing -> pure Nothing
                Just packet -> preparedPacketBodyExpectedEndpoint packet
            preparedLambdaBodyExpectedEndpoint <-
              selectExpectedTermEndpoint
                (scopedTypesAgree scopeContext)
                ("lambda body edge " ++ show bodyEid)
                [ preparedPacketBodyEndpoint
                , envExpectedTermEndpoint envAtLambdaConstructionBoundary
                    >>= projectExpectedTermEndpoint
                      expectedLambdaCodomain
                ]
            annotationConstructionRenames <-
              case preservedAnnotationTy of
                Nothing -> pure []
                Just annotationTy ->
                  let annotationIdentities =
                        Set.fromList
                          (map typeBinderRefIdentity (freeTypeVarRefsType annotationTy))
                      relevantSourceBinderRefs =
                        IntMap.filter
                          ( (`Set.member` annotationIdentities)
                              . typeBinderRefIdentity
                          )
                          (acSourceBinderRefs annotationContext)
                   in either
                        (Left . ValidationFailed . pure)
                        Right
                        ( sourceBinderConstructionRenames
                            (scopeTypeBinderIdentityRepresentative scopeContext)
                            relevantSourceBinderRefs
                            (envConstructionIdentityRoutes envWithLambdaTypeScope0)
                        )
            let preservedAnnotationConstructionTy =
                  applyTypeVarRefRenames annotationConstructionRenames
                    <$> preservedAnnotationTy
            case preservedAnnotationConstructionTy of
              Just annotationConstructionTy
                | not
                    ( alphaEqType annotationConstructionTy paramTy
                        || churchAwareEqType annotationConstructionTy paramTy
                    ) ->
                    Left
                      ( ValidationFailed
                          [ "annotated lambda parameter changed during construction"
                          , "  annotation type: " ++ show preservedAnnotationTy
                          , "  construction-domain annotation type: "
                              ++ show annotationConstructionTy
                          , "  constructed parameter type: " ++ show constructedParamTy
                          , "  parameter surface type: " ++ show paramTySurface
                          , "  parameter scheme: " ++ show (siScheme paramSchemeInfo)
                          , "  prepared lambda scheme: "
                              ++ show (siScheme <$> preparedLambdaSchemeInfo)
                          , "  construction aliases: "
                              ++ show
                                ( envConstructionGammaAliases
                                    envAtLambdaConstructionBoundary
                                )
                          , "  source-to-construction renames: "
                              ++ show annotationConstructionRenames
                          ]
                      )
              _ -> pure ()
            envForBody <-
              case preparedLambdaGeneralization of
                Just packet ->
                  ( \preparedEnv ->
                      preparedEnv
                        { envExpectedTermEndpoint =
                            preparedLambdaBodyExpectedEndpoint
                        }
                    )
                    <$> alignEnvToConstructionBinderRenames
                      ( lambdaParamConstructionRenames
                          paramBoundaryProtectedIdentities
                          (subtermGeneralizationConstructionBinderRenames packet)
                      )
                      envWithLambdaParam
                        { envActiveSubtermConstruction = Just packet
                        }
                Nothing ->
                  pure
                    envWithLambdaParam
                      { envExpectedTermEndpoint =
                          preparedLambdaBodyExpectedEndpoint
                      }
            bodyRootRaiseMergeAuthority <-
              rootRaiseMergeAuthorityFor
                lambdaEdgeArtifacts
                bodyEid
            let bodyRootConstructionTy = do
                  authority <- bodyRootRaiseMergeAuthority
                  let exteriorNode = rrmaExterior authority
                      directExteriorBound =
                        snd
                          <$> find
                            ( (== Just exteriorNode)
                                . typeBinderRefNode
                                . fst
                            )
                            (Map.toList (envTypeBindings envForBody))
                      routedExteriorBound = do
                        routedRef <-
                          IntMap.lookup
                            (getNodeId exteriorNode)
                            (envConstructionGammaAliases envForBody)
                        snd
                          <$> find
                            ( typeBinderRefsSameIdentity routedRef
                                . fst
                            )
                            (Map.toList (envTypeBindings envForBody))
                  constructedTy <-
                    directExteriorBound <|> routedExteriorBound
                  guard (constructedTy /= TBottom)
                  pure constructedTy
                -- Figure 15.3.5 constructs the recursively elaborated body
                -- under S'(operated).  A root RaiseMerge certificate fixes
                -- which exterior owns that endpoint, and the already
                -- installed non-Bottom Gamma bound supplies its complete
                -- source-domain type.  Seed leading body lambdas before
                -- elaborating them; otherwise an exact evidence parameter can
                -- remain in its source identity domain while the body first
                -- invents graph-local parameter identities and only tries to
                -- reconcile them after type checking.
                envForConstructedBody =
                  maybe
                    envForBody
                    (\constructedTy ->
                      let bodyParamTypes =
                            constructedLambdaParamTypes
                              envForBody
                              constructedTy
                              bodyAnn
                       in envForBody
                            { envConstructedLambdaParamTypes =
                                -- This exact body edge is nested inside every
                                -- construction contract already in the map,
                                -- so its identity-keyed domains are the more
                                -- local authority.  The root helper uses the
                                -- opposite bias because entries present before
                                -- entering a root are already more local.
                                Map.union
                                  bodyParamTypes
                                  (envConstructedLambdaParamTypes envForBody)
                            }
                    )
                    bodyRootConstructionTy
                preparedBodyOccurrenceRoutes =
                  maybe
                    IntMap.empty
                    schemeInfoBinderRefSubst
                    preparedLambdaSchemeInfo0
                bodyOccurrenceRoutes =
                  certifiedSourceOccurrenceRoutes
                    (acSourceBinderRefs annotationContext)
                    preparedBodyOccurrenceRoutes
                bodyOccurrenceRenames =
                  certifiedSourceOccurrenceRenames
                    (acSourceBinderRefs annotationContext)
                    preparedBodyOccurrenceRoutes
            bodyElaborationRaw <-
              elabDetailed bodyElabOut envForConstructedBody
            let bodyRawOwnerConstruction =
                  elaboratedOwnerFinalConstruction bodyElaborationRaw
                bodyRawOwnerLocalRefs =
                  maybe
                    []
                    ofcLocallyEmittedBinderRefs
                    bodyRawOwnerConstruction
                bodyRawCertificateLocalRefs =
                  [ ref
                  | certificate <-
                      elaboratedLocalGammaConstructionCertificates
                        bodyElaborationRaw
                  , (ref, _) <-
                      localGammaEmittedBinders
                        (lgccConstruction certificate)
                  ]
                bodyRawOwnerUsedRefs =
                  maybe
                    []
                    ( \ownerConstruction ->
                        freeTypeVarRefsType
                          (ofcConstructedType ownerConstruction)
                          ++ ofcUsedAmbientBinderRefs ownerConstruction
                          ++ IntMap.elems
                            (ofcLocalBinderRoutes ownerConstruction)
                    )
                    bodyRawOwnerConstruction
                bodyRawUsedRefs =
                  bodyRawTermUsedRefs ++ bodyRawOwnerUsedRefs
                bodyRawTermUsedRefs =
                  Reduce.freeTypeVarRefsTerm
                    (elaboratedTerm bodyElaborationRaw)
                bodyRawLocalRefs =
                  bodyRawOwnerLocalRefs
                    ++ bodyRawCertificateLocalRefs
                bodyBoundaryOccurrenceRenames =
                  protectedBoundaryOccurrenceRenames
                    paramBoundaryProtectedIdentities
                    (envConstructionGammaAliases envForBody)
                    bodyRawUsedRefs
                    bodyRawLocalRefs
                -- The protected packet projection deliberately retains exact
                -- generated source identities.  Enter those identities at
                -- the child boundary too, but only when the exact body packet
                -- independently certifies its operated graph occurrence and
                -- the direct node-key sidecar certifies that same source.  The
                -- packet can supply that proof either through an explicit
                -- source-to-construction route or through its paired completed
                -- and operated substitutions at one concrete node.  The
                -- source may be owned by an enclosing evidence lambda rather
                -- than this lambda's parameter, so current-parameter
                -- protection is not an additional authority requirement.
                -- Select only the inverse applications from the term
                -- occurrence itself: a graph ref appearing only in owner
                -- metadata must not rewrite a source-oriented child.  The
                -- complete certificate set still fixes the quotient's source
                -- orientation and suppresses every exact reverse edge below.
                -- Packet-local declarations and certificates are updated
                -- atomically when an inverse is selected.  This makes the
                -- checked body and S'(operated) inhabit the same source domain
                -- before the consumer route is
                -- validated; no representative or type-shape recovery is
                -- involved.
                bodyDirectSourceOccurrenceRenames =
                  selectTermSourcePacketOccurrenceRenames
                    bodyRawTermUsedRefs
                    bodyCertifiedSourceOccurrenceRenames
                bodyCertifiedSourceOccurrenceRenames =
                  case exactLambdaBodyPacket of
                    Nothing -> []
                    Just packet ->
                      certifiedSourcePacketOccurrenceRenames
                        (acSourceBinderRefs annotationContext)
                        (subtermGeneralizationConstructionBinderRenames packet)
                        ++ certifiedSourcePacketOperatedOccurrenceRenames
                          (acSourceBinderRefs annotationContext)
                          ( schemeInfoBinderRefSubst
                              (subtermGeneralizationSchemeInfo packet)
                          )
                          ( schemeInfoBinderRefSubst
                              (subtermGeneralizationOperatedSchemeInfo packet)
                          )
                        ++ certifiedSourcePacketOperatedOccurrenceRenames
                          (acSourceBinderRefs annotationContext)
                          ( schemeInfoBinderRefSubst
                              ( subtermGeneralizationConsumerConstructionSchemeInfo
                                  packet
                              )
                          )
                          ( schemeInfoBinderRefSubst
                              (subtermGeneralizationOperatedSchemeInfo packet)
                          )
                -- Orient the exact packet quotient once, before applying it.
                -- A Graph15 child takes the certified Graph15 -> Generated15
                -- edge; a Generated15 child is already at that endpoint.  In
                -- both cases the matching Generated15 -> Graph15 edge is
                -- absent, so construction cannot undo or reverse the source
                -- projection.
                bodyExactLambdaConstructionRenames =
                  lambdaBodyConstructionRenames
                    paramBoundaryLexicalBinderIdentities
                    bodyCertifiedSourceOccurrenceRenames
                    exactLambdaBodyPacketConstructionRenames
                -- Enter the exact body-edge quotient atomically.  The term and
                -- both construction-certificate channels describe one child;
                -- projecting only the term would leave owner-final metadata in
                -- the graph occurrence domain and make later construction
                -- rediscover a conflicting identity.  This traversal also
                -- crosses matching type abstractions.  Capture-avoiding term
                -- substitution would rename the abstraction but deliberately
                -- stop at its body, separating the declaration from its exact
                -- packet-owned occurrences.
                bodyElaboration =
                  renameElaboratedTermBinderRefPayloads
                    ( bodyBoundaryOccurrenceRenames
                        ++ bodyDirectSourceOccurrenceRenames
                        ++ bodyExactLambdaConstructionRenames
                    )
                    bodyElaborationRaw
                -- The recursive child is returned in its graph-occurrence
                -- domain. First recover exact source occurrences certified by
                -- the prepared lambda scheme, then enter the owner/edge exact
                -- body's validated construction quotient. Apply both routes
                -- to the term before deriving its type or replaying T(e);
                -- rewriting only the inferred type would leave a differently
                -- typed Xmlf term behind the construction boundary.
                bodyRaw =
                  substInTermRefs
                    bodyOccurrenceRoutes
                    (elaboratedTerm bodyElaboration)
                bodyOwnerLocalRefs =
                  maybe
                    []
                    ofcLocallyEmittedBinderRefs
                    (elaboratedOwnerFinalConstruction bodyElaboration)
                -- The recursively elaborated result is the construction
                -- authority for declarations emitted by its exact leading
                -- lambda.  Match the certificate back to the source owner
                -- (constructor, edge, node, and scope), then intersect its
                -- emitted refs with the direct source sidecar.  Those exact
                -- identities remain useful as occurrence routes, but this
                -- enclosing lambda must not use them as declaration
                -- authority for its own Gen(Gamma, tau).
                bodyResultOwnedSourceDeclarationRefs =
                  case
                      ( resultLambdaOwner bodyAnn
                      , elaboratedOwnerFinalConstruction bodyElaboration
                      )
                    of
                    (Just sourceOwner, Just ownerConstruction)
                      | sourceOwner == ofcOwner ownerConstruction ->
                          [ emittedRef
                          | emittedRef <-
                              ofcLocallyEmittedBinderRefs ownerConstruction
                          , any
                              (typeBinderRefsSameIdentity emittedRef)
                              ( IntMap.elems
                                  ( IntMap.restrictKeys
                                      (acSourceBinderRefs annotationContext)
                                      (acDirectSourceBinderKeys annotationContext)
                                  )
                              )
                          ]
                    _ -> []
                resultLambdaOwner ann =
                  case ann of
                    ALam _ _ _ scope _ edge node ->
                      Just
                        LocalGammaOwner
                          { lgoConstructor = LocalLambdaGamma
                          , lgoBoundaryEdge = edge
                          , lgoTermNode = node
                          , lgoScope = GenRef scope
                          }
                    ALet _ _ _ _ _ _ _ resultBody _ ->
                      resultLambdaOwner resultBody
                    AExactAnn inner _ _ _ -> resultLambdaOwner inner
                    AAnn inner _ _ -> resultLambdaOwner inner
                    ALetScope inner _ _ -> resultLambdaOwner inner
                    AUnfold {} -> Nothing
                    _ -> Nothing
                bodySourceTcEnv0 =
                  typeCheckEnvFrom
                    ( projectResolvedOccurrenceLookupTypes
                        ( bodyBoundaryOccurrenceRenames
                            ++ bodyOccurrenceRenames
                            ++ bodyDirectSourceOccurrenceRenames
                            ++ bodyExactLambdaConstructionRenames
                        )
                        envForConstructedBody
                    )
                bodyStripped = stripUnusedTopTyAbsWithEnv bodySourceTcEnv0 bodyRaw
                bodySourceTcEnv =
                  restrictTypeCheckEnvToFreeTermBindings
                    bodyStripped
                    bodySourceTcEnv0
                      { TypeCheck.typeEnv =
                          Map.filterWithKey
                            (\ref _ ->
                              not
                                ( any
                                    (typeBinderRefsSameIdentity ref)
                                    bodyOwnerLocalRefs
                                )
                            )
                            (TypeCheck.typeEnv bodySourceTcEnv0)
                      }
            bodySourceTy <-
              case TypeCheck.typeCheckWithEnv bodySourceTcEnv bodyStripped of
                Right ty -> pure ty
                Left err ->
                  Left
                    ( PhiInvariantError
                        ( unlines
                            [ "ALamF: lambda body source is not typable before its outgoing computation"
                            , "lambda owner=" ++ show (idDetailsIdentityKey binderDetails)
                            , "lambda parameter type=" ++ show paramTy
                            , "exact evidence parameter type="
                                ++ show exactEvidenceParamTy
                            , "exact evidence protected identities="
                                ++ show paramBoundaryProtectedIdentities
                            , "outer construction renames="
                                ++ show
                                  ( envConstructionBinderRenames
                                      envAfterConstructedParam
                                  )
                            , "boundary construction renames="
                                ++ show constructionBinderRenames
                            , "selected exact-evidence outer renames="
                                ++ show exactEvidenceOuterConstructionRenames
                            , "unaligned parameter scheme="
                                ++ show (siScheme paramSchemeInfoUnaligned)
                            , "construction parameter scheme="
                                ++ show
                                  (siScheme paramSchemeInfoForConstruction)
                            , "final parameter scheme="
                                ++ show (siScheme paramSchemeInfo)
                            , "lambda body root RaiseMerge authority="
                                ++ show
                                  ( rootRaiseMergeAuthorityFor
                                      lambdaEdgeArtifacts
                                      bodyEid
                                  )
                            , "lambda body expected endpoint="
                                ++ show preparedLambdaBodyExpectedEndpoint
                            , "lambda body root construction type="
                                ++ show bodyRootConstructionTy
                            , "constructed lambda parameters before body edge="
                                ++ show
                                  (envConstructedLambdaParamTypes envForBody)
                            , "constructed lambda parameters inside body edge="
                                ++ show
                                  ( envConstructedLambdaParamTypes
                                      envForConstructedBody
                                  )
                            , "protected body-boundary occurrence renames="
                                ++ show bodyBoundaryOccurrenceRenames
                            , "body construction Gamma aliases="
                                ++ show
                                  (envConstructionGammaAliases envForBody)
                            , "body construction type bindings="
                                ++ show (envTypeBindings envForBody)
                            , "body owner final construction="
                                ++ show
                                  ( elaboratedOwnerFinalConstruction
                                      bodyElaboration
                                  )
                            , "body local Gamma certificates="
                                ++ show
                                  ( elaboratedLocalGammaConstructionCertificates
                                      bodyElaboration
                                  )
                            , "body=" ++ show bodyStripped
                            , "typecheck=" ++ show err
                            ]
                        )
                    )
            envAtLambdaOutgoingConstructionBoundary <-
              alignEnvToNestedConstructionBinderRenames
                bodyExactLambdaConstructionRenames
                envAtLambdaConstructionBoundary
            let outgoingConstructionBinderRenames =
                  [ rename
                  | rename@(_, outwardRef) <-
                      envConstructionBinderRenames
                        envAtLambdaOutgoingConstructionBoundary
                  , not
                      ( any
                          (typeBinderRefsSameIdentity outwardRef)
                          bodyResultOwnedSourceDeclarationRefs
                      )
                  ]
            lambdaConsumerConstruction0 <-
              prepareLambdaConsumerConstruction
                scopeContext
                envAtLambdaConstructionBoundary
                lambdaOwner
                preparedLambdaGeneralization
                preparedLambdaSchemeInfo
                bodyEid
                bodySourceTy
            let lambdaConsumerConstruction =
                  applyLambdaConsumerConstructionPlanRefRenames
                    outgoingConstructionBinderRenames
                    lambdaConsumerConstruction0
            let preparedLambdaConstructionSchemeInfo =
                  lccSchemeInfo lambdaConsumerConstruction
            let envWithLambdaTypeScopeForConstruction =
                  case preparedLambdaConstructionSchemeInfo of
                    Just schemeInfo ->
                      extendEnvTypeScopeWithAliases
                        (schemeInfoBinderRefSubst schemeInfo)
                        (schemeBinderRefs (siScheme schemeInfo))
                        envAtLambdaOutgoingConstructionBoundary
                    Nothing -> envAtLambdaOutgoingConstructionBoundary
                paramSchemeInfoForOutgoingConstruction =
                  applySchemeInfoRefRenames
                    outgoingConstructionBinderRenames
                    (ebSchemeInfo paramBinding)
                paramBindingForOutgoingConstruction =
                  paramBinding
                    { ebSchemeInfo = paramSchemeInfoForOutgoingConstruction
                    , ebSchemeType =
                        schemeToType
                          (siScheme paramSchemeInfoForOutgoingConstruction)
                    }
                envWithLambdaParamForConstruction0 =
                  insertEnvBinding
                    paramBindingForOutgoingConstruction
                    envWithLambdaTypeScopeForConstruction
                envWithLambdaParamForConstruction =
                  case mAnnLambda of
                    Just (mediatorDetails, _, _, _) ->
                      insertEnvBindingIdentityAlias
                        mediatorDetails
                        paramBindingForOutgoingConstruction
                        envWithLambdaParamForConstruction0
                    Nothing -> envWithLambdaParamForConstruction0
            ( ordinaryLambdaConstructionBinders0,
              envWithOrdinaryLambdaConstruction0,
              ordinaryLambdaBodyConsumerRoute,
              ordinaryLambdaLocalBinderRoutes,
              ordinaryLambdaGeneralizationRequirements
              ) <- do
              sourceBinderRefs <-
                constructionSourceBinderRefs
                  (scopeTypeBinderIdentityRepresentative scopeContext)
                  envWithLambdaTypeScopeForConstruction
                  (acSourceBinderRefs annotationContext)
              ownedEdges <-
                ownedRequirementEdgesForOwner
                  scopeContext
                  lambdaEdgeArtifacts
                  (algExactProducerTypes algebraContext)
                  lambdaOwner
                  ( ALam
                      v
                      mbBinderDetails
                      paramNode
                      lambdaScopeGen
                      bodyAnn
                      bodyEid
                      lamNodeId
                  )
              let requirementEdges =
                    [ ( edgeId
                      , applyTypeVarRefRenames outgoingConstructionBinderRenames
                          <$> if edgeId == bodyEid
                            then Just (lccSourceType lambdaConsumerConstruction)
                            else mbOperatedType
                      )
                    | (edgeId, mbOperatedType) <- ownedEdges
                    ]
              -- The prepared lambda scheme is opened above so its graph
              -- aliases can align and reify the completed construction.  It
              -- is not an enclosing Gamma: using that opened environment for
              -- ownership would let a derived exterior suppress the very
              -- Figure 15.3.5 abstraction this lambda must construct.
              ambientGammaAuthorities <-
                ambientGammaAuthoritiesForEnv
                  envAtLambdaOutgoingConstructionBoundary
              let lambdaAmbientBinderRefs =
                    envAmbientTypeBinderRefs
                      envAtLambdaOutgoingConstructionBoundary
              sourceBinderOwnershipRefs0 <-
                sourceBinderAuthorityRefs
                  (scopeTypeBinderIdentityRepresentative scopeContext)
                  (acDirectSourceBinderKeys annotationContext)
                  ( IntMap.map
                      ( applyRefRenames
                          (envConstructionBinderRenames envAtLambdaOutgoingConstructionBoundary)
                      )
                      (acSourceBinderRefs annotationContext)
                  )
                  ambientGammaAuthorities
              let sourceBinderOwnershipRefs =
                    IntMap.filter
                      ( \sourceRef ->
                          not
                            ( any
                                (typeBinderRefsSameIdentity sourceRef)
                                bodyResultOwnedSourceDeclarationRefs
                            )
                      )
                      sourceBinderOwnershipRefs0
                  -- A direct result lambda emits these source declarations
                  -- itself, so they are not ownership authorities for this
                  -- enclosing Gamma.  Its exact source endpoint can still use
                  -- the declarations, however, while the frozen operated
                  -- packet remains in the corresponding graph-occurrence
                  -- domain.  Retain those concrete sidecar routes only long
                  -- enough to transport that packet to the exact endpoint;
                  -- do not publish them in the resulting ownership view.
                  childOwnedSourceAlignmentRefs =
                    IntMap.filter
                      ( \sourceRef ->
                          any
                            (typeBinderRefsSameIdentity sourceRef)
                            bodyResultOwnedSourceDeclarationRefs
                      )
                      sourceBinderRefs
                  sourceBinderAlignmentRefs =
                    IntMap.union
                      childOwnedSourceAlignmentRefs
                      sourceBinderOwnershipRefs
              requirementsWithAlignedPackets <-
                generalizationRequirementsForRootEdgesInConstruction
                  lambdaActiveConsumerIdentity
                  lambdaAmbientBinderRefs
                  ambientGammaAuthorities
                  (scopeTypeBinderIdentityRepresentative scopeContext)
                  (scCanonical scopeContext)
                  (scGaParents scopeContext)
                  (scPresolutionView scopeContext)
                  lambdaEdgeArtifacts
                  sourceBinderAlignmentRefs
                  (algSubtermGeneralizations algebraContext)
                  requirementEdges
              let requirements =
                    requirementsWithAlignedPackets
                      { grSourceBinderRefs = sourceBinderOwnershipRefs
                      }
              case grRequiredGammaBinders requirements of
                [] ->
                  pure
                    ( [],
                      envWithLambdaParamForConstruction,
                      Nothing,
                      IntMap.empty,
                      requirements
                    )
                _ -> do
                  let lambdaTarget =
                        generalizeTargetNode
                          (scPresolutionView scopeContext)
                          (scCanonical scopeContext lamNodeId)
                  (lambdaSchemeRaw, lambdaSubstRaw) <-
                    case
                        scGeneralizeAtWithRequirements scopeContext
                          requirements
                          (Just (scGaParents scopeContext))
                          lambdaScope
                          lambdaTarget
                      of
                        Right generalized -> pure generalized
                        Left cause ->
                          Left
                            ( ValidationFailed
                                [ "lambda construction generalization failed"
                                , "  lambda=" ++ show (idDetailsIdentityKey binderDetails)
                                , "  target=" ++ show lambdaTarget
                                , "  scope=" ++ show lambdaScope
                                , "  requirements=" ++ show requirements
                                , "  ambient binder refs="
                                    ++ show lambdaAmbientBinderRefs
                                , "  ambient Gamma authorities="
                                    ++ show ambientGammaAuthorities
                                , "  cause=" ++ show cause
                                ]
                            )
                  lambdaSchemeInfoAligned <-
                    alignSchemeInfoToConstructionGamma
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      sourceBinderRefs
                      envWithLambdaTypeScopeForConstruction
                      (schemeInfoFromRefSubst lambdaSchemeRaw lambdaSubstRaw)
                  lambdaSchemeInfoOrdered <-
                    orderConstructionSchemeInfoBinders
                      "lambda packet placement"
                      lambdaSchemeInfoAligned
                  let lambdaPlacementPackets =
                        subtermGeneralizationsOwnedBy
                          bodyAnn
                          (algSubtermGeneralizations algebraContext)
                  lambdaSchemeInfoBeforePlacement <-
                    publishTopologyConsumerRoutes
                      ( gaConstructionRouteNodes
                          (scCanonical scopeContext)
                          (scGaParents scopeContext)
                      )
                      lambdaPlacementPackets
                      lambdaSchemeInfoOrdered
                  lambdaSchemePlaced <-
                    placeSubtermGeneralizationBindersWithRoutes
                      (schemeInfoBinderRefSubst lambdaSchemeInfoBeforePlacement)
                      lambdaPlacementPackets
                      (siScheme lambdaSchemeInfoBeforePlacement)
                  let placedLambdaRefs =
                        map fst (schemeBinderRefs lambdaSchemePlaced)
                      lambdaSubstPlaced =
                        IntMap.map
                          (\ref ->
                              fromMaybe
                                ref
                                (find (typeBinderRefsSameIdentity ref) placedLambdaRefs)
                          )
                          (schemeInfoBinderRefSubst lambdaSchemeInfoBeforePlacement)
                  lambdaSchemeInfo <-
                    generalizeSchemeInfoAgainstConstructionEnv
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      sourceBinderRefs
                      envWithLambdaTypeScopeForConstruction
                      ( schemeInfoFromRefSubst
                          lambdaSchemePlaced
                          lambdaSubstPlaced
                      )
                  gammaPlan <-
                    constructionGammaBinders
                      RequiredGammaOnly
                      scopeContext
                      lambdaOwner
                      Nothing
                      preparedLambdaConstructionSchemeInfo
                      envAtLambdaOutgoingConstructionBoundary
                      requirements
                      lambdaSchemeInfo
                  gammaAliases <-
                    constructionGammaAliases
                      ( gaConstructionRouteNodes
                          (scCanonical scopeContext)
                          (scGaParents scopeContext)
                      )
                      sourceBinderAlignmentRefs
                      requirements
                      gammaPlan
                      lambdaSchemeInfo
                  let binders = cgpBinders gammaPlan
                      aliases = cgaRoutingAliases gammaAliases
                      schemeSubst = schemeInfoBinderRefSubst lambdaSchemeInfo
                      constructionSubst = IntMap.union aliases schemeSubst
                      envWithConstruction =
                        extendEnvTypeScopeWithAliases
                          constructionSubst
                          binders
                          envWithLambdaParamForConstruction
                  bodyConsumerRoute <-
                    selectBodyConsumerRouteWithPacket
                      lambdaOwner
                      bodyEid
                      exactLambdaBodyPacket
                      requirements
                      aliases
                  -- A compiler-exact source forall has no graph node of its
                  -- own, so it cannot contribute a declaration route.  Keep
                  -- its direct source sidecar as declaration authority while
                  -- leaving it out of the graph-route map consumed by the
                  -- construction environment.
                  (localBinderRoutes, _) <-
                    localGammaConstructionProvenance
                      ("lambda " ++ show lambdaOwner)
                      binders
                      [constructionSubst]
                      ( IntMap.restrictKeys
                          sourceBinderRefs
                          (acDirectSourceBinderKeys annotationContext)
                      )
                  pure
                    ( binders,
                      envWithConstruction,
                      bodyConsumerRoute,
                      localBinderRoutes,
                      requirements
                    )
            authorizedOrdinaryLambdaBodyConsumerRoute <-
              case ordinaryLambdaBodyConsumerRoute of
                Nothing -> pure Nothing
                Just consumerRoute ->
                  authorizeBodyConsumerDeclaration
                    ( envConstructionGammaAliases
                        envWithOrdinaryLambdaConstruction0
                    )
                    ( envConstructionBinderRenames
                        envWithOrdinaryLambdaConstruction0
                    )
                    ordinaryLambdaConstructionBinders0
                    (envTypeBindings envAtLambdaOutgoingConstructionBoundary)
                    consumerRoute
            let authorizedOrdinaryLambdaBodyConsumerRef =
                  bcrConstructionRef
                    . authorizedBodyConsumerRoute
                    <$> authorizedOrdinaryLambdaBodyConsumerRoute
                structuredBodyConsumerDependencyIdentities =
                  case
                      authorizedBodyConsumerRoute
                        <$> authorizedOrdinaryLambdaBodyConsumerRoute
                    of
                      Just route
                        | not (bareTypeVariable (bcrOperatedType route)) ->
                            Set.fromList
                              ( typeBinderRefIdentity
                                  (bcrConstructionRef route)
                                  : [ typeBinderRefIdentity ref
                                    | ref <-
                                        freeTypeVarRefsType
                                          (bcrOperatedType route)
                                    , Just node <- [typeBinderRefNode ref]
                                    , Just routedRef <-
                                        [ IntMap.lookup
                                            (getNodeId node)
                                            ( envConstructionGammaAliases
                                                envWithOrdinaryLambdaConstruction0
                                            )
                                        ]
                                    , typeBinderRefsSameIdentity
                                        routedRef
                                        (bcrConstructionRef route)
                                    ]
                              )
                      _ -> Set.empty
                bareTypeVariable ty =
                  case ty of
                    TVarRef _ -> True
                    _ -> False
                ordinaryBodyConsumerOperatesOnOwnDeclaration =
                  case
                      authorizedBodyConsumerRoute
                        <$> authorizedOrdinaryLambdaBodyConsumerRoute
                    of
                      Just route
                        | TVarRef constructionOperatedRef <-
                            bcrConstructionOperatedType route ->
                            typeBinderRefsSameIdentity
                              constructionOperatedRef
                              (bcrConstructionRef route)
                      _ -> False
                ordinaryBodyConsumerOccurrenceRenames =
                  case
                      authorizedBodyConsumerRoute
                        <$> authorizedOrdinaryLambdaBodyConsumerRoute
                    of
                      Just route
                        | TVarRef operatedRef <- bcrOperatedType route
                        , TVarRef constructionOperatedRef <-
                            bcrConstructionOperatedType route
                        , provisionalBodyConsumerOccurrence operatedRef
                        , typeBinderRefsSameIdentity
                            constructionOperatedRef
                            (bcrConstructionRef route)
                        , not
                            ( typeBinderRefsSameIdentity
                                operatedRef
                                constructionOperatedRef
                            ) ->
                            [(operatedRef, constructionOperatedRef)]
                      _ -> []
                provisionalBodyConsumerOccurrence operatedRef =
                  case preparedLambdaConstructionSchemeInfo of
                    Just schemeInfo ->
                      case
                          find
                            ( typeBinderRefsSameIdentity operatedRef
                                . fst
                            )
                            (schemeBinderRefs (siScheme schemeInfo))
                        of
                          Just (_, Nothing) -> True
                          Just (_, Just _) -> False
                          Nothing -> provisionalAmbientOccurrence operatedRef
                    Nothing -> provisionalAmbientOccurrence operatedRef
                provisionalAmbientOccurrence operatedRef =
                  case
                      find
                        ( typeBinderRefsSameIdentity operatedRef
                            . fst
                        )
                        ( Map.toList
                            ( envTypeBindings
                                envAtLambdaOutgoingConstructionBoundary
                            )
                        )
                    of
                      Just (_, TBottom) -> True
                      _ -> False
            preparedBodyEdgeOperatedForConstruction <-
              case exactLambdaBodyPacket of
                Nothing -> pure Nothing
                Just packet -> do
                  bodyPacketSourceBinderRefs <-
                    constructionSourceBinderRefs
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      envWithOrdinaryLambdaConstruction0
                      (acSourceBinderRefs annotationContext)
                  case
                      generalizeSchemeInfoAgainstConstructionEnvPreserving
                        ( Set.union
                            paramBoundaryProtectedIdentities
                            structuredBodyConsumerDependencyIdentities
                        )
                        Set.empty
                        (scopeTypeBinderIdentityRepresentative scopeContext)
                        bodyPacketSourceBinderRefs
                        envWithOrdinaryLambdaConstruction0
                        (subtermGeneralizationOperatedSchemeInfo packet)
                    of
                      Right schemeInfo ->
                        -- The exact packet and source sidecar have already
                        -- selected one source-oriented occurrence quotient
                        -- for the recursively checked body.  Project S'(e)
                        -- through that same certificate before it becomes a
                        -- Gamma bound.  Otherwise representative equality can
                        -- admit Graph0 here while the checked source remains
                        -- Generated7.
                        pure
                          ( Just
                              ( applySchemeInfoRefRenames
                                  ( bodyCertifiedSourceOccurrenceRenames
                                      ++ envConstructionBinderRenames
                                        envWithOrdinaryLambdaConstruction0
                                  )
                                  schemeInfo
                              )
                          )
                      Left cause ->
                        Left
                          ( ValidationFailed
                              [ "lambda body-edge operated packet cannot enter construction Gamma"
                              , "  lambda=" ++ show (idDetailsIdentityKey binderDetails)
                              , "  body edge=" ++ show bodyEid
                              , "  operated packet="
                                  ++ show
                                    (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                              , "  cause=" ++ show cause
                              ]
                          )
            let preparedBodyOperatedForConstruction = do
                  bodyPacket <- preparedBodyGeneralization
                  selectedPacket <- exactLambdaBodyPacket
                  projectedInfo <- preparedBodyEdgeOperatedForConstruction
                  if bodyPacket == selectedPacket
                    then Just projectedInfo
                    else Nothing
            ( ordinaryLambdaConstructionBinders,
              envWithOrdinaryLambdaConstruction,
              envForCompletedLambdaConstruction,
              validatedBodyConsumerProjection,
              bodyConsumerBoundRefinementCertificate
              ) <-
              projectBodyPacketConsumerIntoLambdaGamma
                lambdaOwner
                bodyEid
                bodySourceTy
                exactLambdaBodyPacket
                preparedBodyOperatedForConstruction
                authorizedOrdinaryLambdaBodyConsumerRoute
                ordinaryLambdaGeneralizationRequirements
                ordinaryLambdaConstructionBinders0
                envWithOrdinaryLambdaConstruction0
                envAtLambdaOutgoingConstructionBoundary
            let bodySourceConstructionRenames =
                  maybe
                    []
                    validatedBodyConsumerProjectionSourceConstructionRenames
                    validatedBodyConsumerProjection
                bodyConsumerOccurrenceRenames =
                  ordinaryBodyConsumerOccurrenceRenames
                    ++ bodySourceConstructionRenames
                bodyAtOrdinaryConsumer =
                  renameTermTypeBinderRefPayloads
                    bodyConsumerOccurrenceRenames
                    bodyStripped
                bodySourceTyAtOrdinaryConsumer =
                  applyTypeVarRefRenames
                    bodyConsumerOccurrenceRenames
                    bodySourceTy
                bodyTcEnv =
                  typeCheckEnvFrom
                    ( projectResolvedOccurrenceLookupTypes
                        bodyConsumerOccurrenceRenames
                        envWithOrdinaryLambdaConstruction
                    )
            bodyEdgeTranslationRaw <-
              let reifyBodyEdge =
                    case
                        ( exactLambdaBodyPacket
                        , preparedBodyEdgeOperatedForConstruction
                        )
                      of
                      (Just _, Just projectedOperated)
                        | operationalEndpointTypesAgree
                              bodySourceTyAtOrdinaryConsumer
                              (schemeToType (siScheme projectedOperated)) ->
                              -- Gen(Gamma,tau) has already constructed the
                              -- child's operated endpoint in this lambda's
                              -- identity domain.  The outgoing Gamma result,
                              -- when present, is appended below from its
                              -- prepared consumer authority; replaying the
                              -- pre-projection graph witness here would open
                              -- the same binders a second time.
                              pure InstId
                      (Just _, Just projectedOperated) ->
                        -- Replay is entering the already constructed Gamma,
                        -- so its occurrence capability must be the operated
                        -- packet after that same construction projection.
                        -- Feeding the pre-projection packet back into replay
                        -- would reintroduce graph-local identities at keys
                        -- whose exact outward routes are already fixed.
                        reifyInstFromSourceSchemeInConstructionGamma
                          annotationContext
                          namedSetReify
                          ( envConstructionGammaAliases
                              envWithOrdinaryLambdaConstruction
                          )
                          ordinaryLambdaGeneralizationRequirements
                          projectedOperated
                          bodyAnn
                          bodyEid
                      (Just preparedBody, _) ->
                        reifyInstFromSourceSchemeInConstructionGamma
                          annotationContext
                          namedSetReify
                          ( envConstructionGammaAliases
                              envWithOrdinaryLambdaConstruction
                          )
                          ordinaryLambdaGeneralizationRequirements
                          (subtermGeneralizationOperatedSchemeInfo preparedBody)
                          bodyAnn
                          bodyEid
                      (Nothing, _) ->
                        -- The body has already passed the local xMLF checker,
                        -- so its checked type is the edge's source
                        -- construction.  Graph re-generalization here can
                        -- expose a sibling result variable that is not part
                        -- of that source (notably for self-application).
                        reifyInstWithFrozenEndpointsFromCheckedSource
                          annotationContext
                          namedSetReify
                          (\details -> ebSchemeInfo <$> lookupEnvBindingForDetails details envWithOrdinaryLambdaConstruction)
                          IntMap.empty
                          bodySourceTyAtOrdinaryConsumer
                          bodyAnn
                          bodyEid
                  computation =
                    case
                        ( exactLambdaBodyPacket
                        , ordinaryBodyConsumerOperatesOnOwnDeclaration
                        )
                      of
                      (Nothing, True) ->
                        -- The exact checked occurrence has already been
                        -- routed to the declaration introduced by this
                        -- construction Gamma.  Figure 15.3.5 contributes
                        -- epsilon here; emitting Hyp again would try to
                        -- abstract a value whose type is already that
                        -- declaration.
                        pure InstId
                      _ ->
                        case
                          subtermResultOwnershipFor
                            ( ALam
                                v
                                mbBinderDetails
                                paramNode
                                lambdaScopeGen
                                bodyAnn
                                bodyEid
                                lamNodeId
                            )
                            (algSubtermGeneralizations algebraContext)
                        of
                        -- Only an owner-matched packet whose construction is
                        -- closed locally owns a direct Hyp. Ordinary Gamma
                        -- packets still consume the edge-local T(e); the mere
                        -- presence of a result binder is not authority to
                        -- replace that translation.
                          Just ownership
                            | subtermResultOwnershipConsumerClosedLocally ownership
                            , Just exactPacket <- exactLambdaBodyPacket
                            , subtermResultOwnershipPacket ownership == exactPacket
                            , Just resultRef <-
                                subtermGeneralizationResultAbstractionRef
                                  (subtermResultOwnershipPacket ownership) ->
                                pure (InstAbstrRef resultRef)
                          _ -> reifyBodyEdge
               in case computation of
                    Right inst -> pure inst
                    Left cause ->
                      Left
                        ( ValidationFailed
                            [ "lambda body edge computation failed"
                            , "  lambda=" ++ show (idDetailsIdentityKey binderDetails)
                            , "  edge=" ++ show bodyEid
                            , "  cause=" ++ show cause
                            ]
                        )
            let packetBodySemanticResultRef =
                  exactLambdaBodyPacket
                    >>= subtermGeneralizationResultAbstractionRef
                packetBodyConstructionResultRef =
                  applyRefRenames outgoingConstructionBinderRenames
                    <$> ( exactLambdaBodyPacket
                            >>= subtermGeneralizationConstructionResultAbstractionRef
                        )
                packetBodyRouteDomainRefs =
                  maybeToList packetBodySemanticResultRef
                    ++ maybeToList
                      ( applyRefRenames outgoingConstructionBinderRenames
                          <$> packetBodySemanticResultRef
                      )
                    ++ maybeToList
                      ( exactLambdaBodyPacket
                          >>= subtermGeneralizationConstructionResultAbstractionRef
                      )
            packetBodyResultRoute <-
              case
                  ( packetBodySemanticResultRef
                  , packetBodyConstructionResultRef
                  )
                of
                  (Just semanticRef, Just constructionRef)
                    | typeBinderRefsSameIdentity semanticRef constructionRef ->
                        pure Nothing
                    | otherwise ->
                        case typeBinderRefNode semanticRef of
                          Nothing ->
                            Left
                              ( ValidationFailed
                                  [ "packet result identity changed without a graph route"
                                  , "  semantic result: " ++ show semanticRef
                                  , "  construction result: " ++ show constructionRef
                                  ]
                              )
                          Just semanticNode ->
                            case
                                IntMap.lookup
                                  (getNodeId semanticNode)
                                  ( envConstructionGammaAliases
                                      envWithOrdinaryLambdaConstruction
                                  )
                              of
                                Just existingRef
                                  | not
                                      ( typeBinderRefsSameIdentity
                                          existingRef
                                          constructionRef
                                      ) ->
                                      Left
                                        ( ValidationFailed
                                            [ "packet result route conflicts with the lambda construction Gamma"
                                            , "  graph result: " ++ show semanticRef
                                            , "  packet construction result: "
                                                ++ show constructionRef
                                            , "  lambda construction result: "
                                                ++ show existingRef
                                            ]
                                        )
                                _ -> pure (Just constructionRef)
                  (Nothing, Nothing) -> pure Nothing
                  refs ->
                    Left
                      ( ValidationFailed
                          [ "packet result has only one side of its semantic/construction route"
                          , "  result refs: " ++ show refs
                          ]
                      )
            let constructionAliasForBodyResult ref =
                  case packetBodyResultRoute of
                    Just constructionRef
                      | any
                          (typeBinderRefsSameIdentity ref)
                          packetBodyRouteDomainRefs ->
                          Just constructionRef
                    _ -> do
                      node <- typeBinderRefNode ref
                      IntMap.lookup
                        (getNodeId node)
                        (envConstructionGammaAliases envWithOrdinaryLambdaConstruction)
                canonicalBodyEdgeTranslation =
                  canonicalizeFreeInstantiationHypRefs
                    (\ref -> do
                      node <- typeBinderRefNode ref
                      IntMap.lookup
                        (getNodeId node)
                        (envConstructionGammaAliases envWithOrdinaryLambdaConstruction)
                    )
                    bodyEdgeTranslationRaw
                packetBodyResultAbstractionRef =
                  packetBodyConstructionResultRef
                -- A packet result that is absent from the exact enclosing
                -- lambda codomain is a construction-local waypoint, not a
                -- published body result.  Decide that before edge
                -- specialization: otherwise an existing or synthesized
                -- terminal Hyp reaches the packet specializer as though the
                -- checked body still had to construct that provisional slot.
                dischargedPacketBodyResultRef = do
                  resultRef <- packetBodyResultAbstractionRef
                  expectedBound <-
                    lccExpectedEnclosingBound lambdaConsumerConstruction
                  expectedBodyTy <- expectedLambdaCodomain expectedBound
                  guard
                    ( scopedEndpointTypesAgree
                        scopeContext
                        bodySourceTyAtOrdinaryConsumer
                        expectedBodyTy
                    )
                  guard
                    ( not
                        ( any
                            (typeBinderRefsSameIdentity resultRef)
                            (freeTypeVarRefsType expectedBodyTy)
                        )
                    )
                  pure resultRef
                retainPublishedBodyResultRef mbRef = do
                  ref <- mbRef
                  guard
                    ( maybe
                        True
                        (not . typeBinderRefsSameIdentity ref)
                        dischargedPacketBodyResultRef
                    )
                  pure ref
                retainedOrdinaryBodyResultRef =
                  if ordinaryBodyConsumerOperatesOnOwnDeclaration
                    then Nothing
                    else
                      retainPublishedBodyResultRef
                        authorizedOrdinaryLambdaBodyConsumerRef
                retainedPacketBodyResultRef =
                  retainPublishedBodyResultRef
                    packetBodyResultAbstractionRef
                authorizedBodyResultRefs =
                  maybeToList retainedOrdinaryBodyResultRef
                    ++ maybeToList retainedPacketBodyResultRef
                -- A raw graph replay can end in Hyp for a descendant packet
                -- whose binder happens to be visible while this lambda is
                -- checked.  Visibility is not construction authority.  Keep
                -- the prefix, but retain the terminal Hyp only when this
                -- exact lambda-body selector (or its ordinary Gamma plan)
                -- owns that same result identity.
                bodyEdgeTranslation =
                  case splitOutgoingGammaResult canonicalBodyEdgeTranslation of
                    Just (beforeResult, terminalRef)
                      | not
                          ( any
                              (typeBinderRefsSameIdentity terminalRef)
                              authorizedBodyResultRefs
                          ) ->
                            beforeResult
                    _ -> canonicalBodyEdgeTranslation
                bodyResultAbstractionRef =
                  retainedOrdinaryBodyResultRef
                    <|> retainedPacketBodyResultRef
                bodyEdgeWithResultRaw =
                  case (bodyEdgeTranslation, bodyResultAbstractionRef) of
                    (InstId, Just consumerRef) -> InstAbstrRef consumerRef
                    _ -> bodyEdgeTranslation
                -- Packet result selection can introduce a pre-quotient Hyp
                -- after the raw edge was canonicalized.  Route that completed
                -- computation through the Gamma constructed for this lambda.
                bodyEdgeWithResult =
                  canonicalizeFreeInstantiationHypRefs
                    constructionAliasForBodyResult
                    bodyEdgeWithResultRaw
            packetSpecializedBodyInst <-
              specializeCompletedBodyPacketForEdge
                outgoingConstructionBinderRenames
                bodyTcEnv
                bodySourceTyAtOrdinaryConsumer
                exactLambdaBodyPacket
                validatedBodyConsumerProjection
                bodyEid
                bodyEdgeWithResult
            let ordinaryBodyConsumerBound = do
                  consumerRef <- authorizedOrdinaryLambdaBodyConsumerRef
                  consumerBound <- TypeCheck.lookupTypeBindingRef consumerRef bodyTcEnv
                  pure (consumerRef, consumerBound)
                ordinaryGammaOwnsBodyPacket =
                  case preparedBodyGeneralization of
                    Nothing -> True
                    Just bodyPacket ->
                      maybe False (== bodyPacket) exactLambdaBodyPacket
                ordinaryGammaPacketConstruction = do
                  if ordinaryGammaOwnsBodyPacket
                    then
                      constructOrdinaryGammaPacket
                        bodySourceTyAtOrdinaryConsumer
                        ordinaryBodyConsumerBound
                        packetSpecializedBodyInst
                    else Nothing
            ( bodyForOutgoingComputation,
              bodySourceTyForOutgoingComputation,
              bodyTypeEnvForOutgoingComputation,
              bodyInstFromChild
              ) <-
              case ordinaryGammaPacketConstruction of
                Nothing ->
                  pure
                    ( bodyAtOrdinaryConsumer,
                      bodySourceTyAtOrdinaryConsumer,
                      bodyTcEnv,
                      packetSpecializedBodyInst
                    )
                Just construction -> do
                  let packetBinderRefs =
                        map fst (schemeBinderRefs (ogpcScheme construction))
                      closedBody =
                        foldr
                          (\(ref, mbBound) body -> ETyAbsRef ref mbBound body)
                          bodyAtOrdinaryConsumer
                          (schemeBinderRefs (ogpcScheme construction))
                      packetBodyTcEnv =
                        (restrictTypeCheckEnvToFreeTermBindings closedBody bodyTcEnv)
                          { TypeCheck.typeEnv =
                              Map.filterWithKey
                                (\ref _ ->
                                    not
                                      ( any
                                          (typeBinderRefsSameIdentity ref)
                                          packetBinderRefs
                                      )
                                )
                                (TypeCheck.typeEnv bodyTcEnv)
                          }
                      expectedClosedTy = schemeToType (ogpcScheme construction)
                  closedBodyTy <-
                    case TypeCheck.typeCheckWithEnv packetBodyTcEnv closedBody of
                      Right ty -> pure ty
                      Left cause ->
                        Left
                          ( PhiInvariantError
                              ( unlines
                                  [ "ordinary Gamma packet construction is not typable"
                                  , "lambda owner=" ++ show lambdaOwner
                                  , "body edge=" ++ show bodyEid
                                  , "body source="
                                      ++ show bodySourceTyAtOrdinaryConsumer
                                  , "consumer bound=" ++ show ordinaryBodyConsumerBound
                                  , "bound-to-source binder routes="
                                      ++ show (ogpcBoundBinderRoutes construction)
                                  , "packet body type bindings="
                                      ++ show (TypeCheck.typeEnv packetBodyTcEnv)
                                  , "packet body term bindings="
                                      ++ show
                                        ( TypeCheck.resolvedTermEnvEntries
                                            (TypeCheck.resolvedTermEnv packetBodyTcEnv)
                                        )
                                  , "closed body=" ++ show closedBody
                                  , "typecheck=" ++ show cause
                                  ]
                              )
                          )
                  unless
                    ( alphaEqType closedBodyTy expectedClosedTy
                        || churchAwareEqType closedBodyTy expectedClosedTy
                    )
                    ( Left
                        ( PhiInvariantError
                            ( unlines
                                [ "ordinary Gamma packet construction has the wrong endpoint"
                                , "lambda owner=" ++ show lambdaOwner
                                , "body edge=" ++ show bodyEid
                                , "constructed type=" ++ show closedBodyTy
                                , "expected Gamma bound=" ++ show expectedClosedTy
                                ]
                            )
                        )
                    )
                  pure
                    ( closedBody,
                      closedBodyTy,
                      packetBodyTcEnv,
                      ogpcInstantiation construction
                    )
            let ordinaryBodyInst =
                  case (exactLambdaBodyPacket, bodyInstFromChild) of
                    (Just packet, InstAbstrRef exteriorRef)
                      | subtermGeneralizationOwnsGammaForEdge bodyEid packet
                      , Just exteriorNode <- typeBinderRefNode exteriorRef
                      , Just aliasRef <- IntMap.lookup (getNodeId exteriorNode) (siSubstRefs (subtermGeneralizationSchemeInfo packet))
                      , not (typeBinderRefsSameIdentity exteriorRef aliasRef) -> InstId
                    _ -> bodyInstFromChild
            bodyInstCandidate <-
              case exactLambdaBodyPacket of
                Just packet
                      | Just _ <- subtermGeneralizationCompilerExactBoundary packet
                      , Just _ <- subtermGeneralizationCompilerExactResultRef packet
                      , Nothing <- subtermGeneralizationCompilerExactCompletionRef packet ->
                      -- An enclosing-result marker says that this packet's
                      -- local result Hyp is discharged by the enclosing
                      -- RaiseMerge consumer.  The outgoing computation here
                      -- is therefore that consumer's already-constructed Hyp,
                      -- not the packet-local result ref recorded only to
                      -- suppress a second completion at the exact boundary.
                      pure ordinaryBodyInst
                      | Just exactEdge <- subtermGeneralizationCompilerExactBoundary packet
                      , Just expectedRef <- subtermGeneralizationCompilerExactResultRef packet ->
                      let constructionExpectedRef =
                            applyRefRenames outgoingConstructionBinderRenames expectedRef
                       in
                      case splitOutgoingGammaResult ordinaryBodyInst of
                        Just (beforeResult, actualRef)
                          | typeBinderRefsSameIdentity actualRef constructionExpectedRef ->
                              -- Section 15.3.8 checks the operated producer
                              -- before the enclosing Gamma result is consumed.
                              -- The owning exact boundary completes this exact
                              -- action after construction.  When the checked
                              -- body already is the bound of the selected
                              -- outgoing Gamma result, the earlier graph
                              -- replay and its final Hyp are both discharged.
                              -- Otherwise retain the prefix that reaches that
                              -- bound.
                              let bodyAlreadyAtResultBound =
                                    case TypeCheck.lookupTypeBindingRef constructionExpectedRef bodyTypeEnvForOutgoingComputation of
                                      Just resultBound ->
                                        alphaEqType bodySourceTyForOutgoingComputation resultBound
                                          || churchAwareEqType bodySourceTyForOutgoingComputation resultBound
                                      Nothing -> False
                               in pure
                                    ( if bodyAlreadyAtResultBound
                                        then InstId
                                        else beforeResult
                                    )
                          | otherwise ->
                              Left
                                ( ValidationFailed
                                    [ "compiler exact packet selected a different Gamma result abstraction"
                                    , "  exact edge: " ++ show exactEdge
                                    , "  expected result: " ++ show expectedRef
                                    , "  construction-domain expected result: "
                                        ++ show constructionExpectedRef
                                    , "  actual result: " ++ show actualRef
                                    , "  construction binder renames: "
                                        ++ show outgoingConstructionBinderRenames
                                    , "  packet scheme: "
                                        ++ show (siScheme (subtermGeneralizationSchemeInfo packet))
                                    , "  packet operated scheme: "
                                        ++ show (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                                    , "  packet construction scheme: "
                                        ++ show (siScheme (subtermGeneralizationConsumerConstructionSchemeInfo packet))
                                    , "  lambda body source: " ++ show bodySourceTyForOutgoingComputation
                                    , "  lambda body computation: " ++ show ordinaryBodyInst
                                    , "  lambda body Gamma: " ++ show (envTypeBindings envWithOrdinaryLambdaConstruction)
                                    ]
                                )
                        Nothing ->
                          Left
                            ( ValidationFailed
                                [ "compiler exact packet has no outgoing Gamma result abstraction"
                                , "  exact edge: " ++ show exactEdge
                                , "  lambda owner: " ++ show (idDetailsIdentityKey binderDetails)
                                , "  lambda body edge: " ++ show bodyEid
                                , "  owns body Gamma: " ++ show (subtermGeneralizationOwnsGammaForEdge bodyEid packet)
                                , "  expected result: " ++ show expectedRef
                                , "  edge translation: " ++ show bodyEdgeTranslation
                                , "  packet-specialized edge: " ++ show bodyInstFromChild
                                , "  selected result abstraction: " ++ show bodyResultAbstractionRef
                                , "  body instantiation: " ++ show ordinaryBodyInst
                                , "  packet: " ++ show packet
                                ]
                            )
                _ -> pure ordinaryBodyInst
            let
                -- Some packet presentations retain the body result under
                -- their graph identity until after child specialization.
                -- Discharge that final Hyp only when the enclosing lambda
                -- contract proves the checked body's exact published
                -- codomain and the terminal identity is absent from it.
                expectedEnclosingBodyType =
                  lccExpectedEnclosingBound lambdaConsumerConstruction
                    >>= expectedLambdaCodomain
                provisionalEnclosingBodyConsumer terminalRef = do
                  expectedBodyTy <- expectedEnclosingBodyType
                  provisionalConsumerRef <-
                    find
                      (typeBinderRefsSameIdentity terminalRef)
                      ( catMaybes
                          [ authorizedOrdinaryLambdaBodyConsumerRef
                          , packetBodyResultAbstractionRef
                          ]
                      )
                  guard
                    ( scopedEndpointTypesAgree
                        scopeContext
                        bodySourceTyForOutgoingComputation
                        expectedBodyTy
                    )
                  guard
                    ( not
                        ( any
                            (typeBinderRefsSameIdentity terminalRef)
                            (freeTypeVarRefsType expectedBodyTy)
                        )
                    )
                  pure provisionalConsumerRef
            bodyInst <-
              case splitOutgoingGammaResult bodyInstCandidate of
                Just (beforeProvisionalConsumer, terminalRef)
                  | Just _ <-
                      provisionalEnclosingBodyConsumer terminalRef ->
                      -- The checked body already has the required
                      -- operational endpoint. Preserve every construction
                      -- step before the terminal packet/ordinary Hyp; only
                      -- that unpublished consumer is discharged here.
                      pure beforeProvisionalConsumer
                _ ->
                    case lccConsumerWithoutLocalBinder lambdaConsumerConstruction of
                      Just (PacketConsumerEliminatedAtAmbientBound eliminatedRef _) ->
                        case splitOutgoingGammaResult bodyInstCandidate of
                          Just (beforeResult, actualRef)
                            | typeBinderRefsSameIdentity eliminatedRef actualRef ->
                                -- Eq-Free has removed this packet's vacuous result
                                -- declaration from the current construction scheme.
                                -- Retain any computation that reaches its ambient
                                -- bound, but do not emit the terminal Hyp that would
                                -- recreate the eliminated result as a free variable.
                                pure beforeResult
                          Nothing
                            | bodyInstCandidate == InstId -> pure InstId
                          _ ->
                            Left
                              ( ValidationFailed
                                  [ "vacuous packet consumer has a non-eliminable outgoing computation"
                                  , "  consumer: " ++ show eliminatedRef
                                  , "  source type: " ++ show bodySourceTyForOutgoingComputation
                                  , "  computation: " ++ show bodyInstCandidate
                                  ]
                              )
                      _ ->
                        case directBodyGammaInstantiation
                              bodyTypeEnvForOutgoingComputation
                              bodySourceTyForOutgoingComputation
                              ( exactLambdaBodyPacket
                                  >>= subtermGeneralizationCompilerExactResultRef
                              )
                              bodyResultAbstractionRef of
                          Just directInst -> pure directInst
                          Nothing ->
                            case bodyInstCandidate of
                              InstId -> pure InstId
                              inst@(InstAbstrRef resultRef)
                                -- Hyp(a) constructs a result of type a from the bound of a.
                                -- When the source already has type a, the lambda-body edge
                                -- is the identity computation; emitting Hyp(a) would be
                                -- ill-scoped even after the enclosing Gamma is abstracted.
                                | TVarRef sourceRef <- bodySourceTyForOutgoingComputation
                                , typeBinderRefsSameIdentity sourceRef resultRef ->
                                    pure InstId
                                | otherwise -> pure inst
                              inst -> pure inst
            bodyResultTy <-
              validateBodyInstantiationResult
                [ "ordinary construction binders=" ++ show ordinaryLambdaConstructionBinders
                , "prepared construction scheme=" ++ show preparedLambdaConstructionSchemeInfo
                , "prepared lambda exact boundary="
                    ++ show
                      (preparedLambdaGeneralization >>= subtermGeneralizationCompilerExactBoundary)
                , "prepared body exact boundary="
                    ++ show
                      (preparedBodyGeneralization >>= subtermGeneralizationCompilerExactBoundary)
                , "prepared body consumer authority="
                    ++ show
                      (preparedBodyGeneralization >>= subtermGeneralizationConsumerAuthority)
                , "prepared body operated scheme="
                    ++ show
                      ( siScheme . subtermGeneralizationOperatedSchemeInfo
                          <$> preparedBodyGeneralization
                      )
                , "exact lambda-body packet="
                    ++ show exactLambdaBodyPacket
                , "body edge translation=" ++ show bodyEdgeTranslation
                , "body edge with result=" ++ show bodyEdgeWithResult
                , "body inst from child=" ++ show bodyInstFromChild
                , "body source before packet construction=" ++ show bodySourceTy
                , "body source for outgoing computation="
                    ++ show bodySourceTyForOutgoingComputation
                , "ordinary Gamma packet construction="
                    ++ show ordinaryGammaPacketConstruction
                , "ordinary body consumer=" ++ show authorizedOrdinaryLambdaBodyConsumerRef
                , "ordinary body consumer bound=" ++ show ordinaryBodyConsumerBound
                , "selected body result=" ++ show bodyResultAbstractionRef
                , "selected outgoing Gamma result="
                    ++ show (snd <$> splitOutgoingGammaResult bodyInst)
                , "exact body endpoint="
                    ++ show
                      ( preparedLambdaBodyExpectedEndpoint
                          >>= exactConstructionExpectedType
                      )
                , "expected enclosing bound=" ++ show (lccExpectedEnclosingBound lambdaConsumerConstruction)
                , "construction type bindings=" ++ show (envTypeBindings envWithOrdinaryLambdaConstruction)
                ]
                bodyTypeEnvForOutgoingComputation
                bodySourceTyForOutgoingComputation
                bodyInst
            preparedLambdaSchemeBinders <-
              case
                  ( preparedLambdaConstructionSchemeInfo
                  , preparedLambdaGeneralization >>= subtermGeneralizationCompilerExactResultRef
                  , preparedLambdaGeneralization >>= subtermGeneralizationCompilerExactCompletionRef
                  )
                of
                (Nothing, _, _) -> pure []
                (Just lambdaSchemeInfo, Nothing, _) ->
                  pure (schemeBinderRefs (siScheme lambdaSchemeInfo))
                -- A source- or packet-owned compiler-exact result is
                -- abstracted at the exact boundary by
                -- 'completeCompilerExactSubtermResults'.  Exclude that one
                -- binder from the local lambda so the two constructors do
                -- not publish the same abstraction twice.
                (Just lambdaSchemeInfo, Just delayedResultRef, Just _) ->
                  case
                      partition
                        (typeBinderRefsSameIdentity delayedResultRef . fst)
                        (schemeBinderRefs (siScheme lambdaSchemeInfo))
                    of
                      ([_], retainedBinders) -> pure retainedBinders
                      ([], retainedBinders) -> pure retainedBinders
                      _ ->
                        Left
                          ( ValidationFailed
                              [ "compiler exact result binder occurs more than once in its lambda construction scheme"
                              , "  lambda owner: " ++ show (idDetailsIdentityKey binderDetails)
                              , "  result: " ++ show delayedResultRef
                              , "  scheme: " ++ show (siScheme lambdaSchemeInfo)
                              ]
                          )
                -- An enclosing-owned marker has no exact-boundary completion
                -- action.  Its packet can still own a non-vacuous local Gamma
                -- binder (for example, a case branch returning the graph
                -- result variable).  Retain the construction binder here;
                -- inherited enclosing binders have already been removed by
                -- 'inheritAmbientConsumerBinder'.
                (Just lambdaSchemeInfo, Just _, Nothing) ->
                  pure (schemeBinderRefs (siScheme lambdaSchemeInfo))
            let -- An enclosing-Gamma consumer publishes the exact completed
                -- lambda bound before this constructor emits anything.  Refs
                -- free in that bound are owned by the enclosing constructor:
                -- the current lambda may use their prepared routes, but must
                -- not abstract them around captured term bindings.  Doing so
                -- would turn @\f -> \x -> f x@ into a scope-capturing inner
                -- @Lambda b@ instead of letting the outer lambda emit @b@.
                locallyOwnedPreparedLambdaSchemeBinders =
                  [ binder
                  | binder@(ref, _) <- preparedLambdaSchemeBinders
                  , not
                      ( any
                          (typeBinderRefsSameIdentity ref)
                          ( lccEnclosingOwnedBinderRefs
                              lambdaConsumerConstruction
                          )
                      )
                  ]
            packetResultCompletionBinders <-
              case
                  ( preparedLambdaGeneralization
                  , splitOutgoingGammaResult bodyInst
                  )
                of
                (Just packet, Just (_, resultRef))
                  | not
                      ( any
                          (typeBinderRefsSameIdentity resultRef)
                          (lccEnclosingOwnedBinderRefs lambdaConsumerConstruction)
                      ) -> do
                      let rawConstructionInfo =
                            applySchemeInfoRefRenames
                              outgoingConstructionBinderRenames
                              ( subtermGeneralizationConsumerConstructionSchemeInfo
                                  packet
                              )
                          packetCompletedConstructionInfo =
                            applySchemeInfoRefRenames
                              outgoingConstructionBinderRenames
                              (subtermGeneralizationSchemeInfo packet)
                          completedConstructionInfo =
                            -- Packet preparation cannot know Typ(body), so
                            -- its nominally completed scheme can still carry
                            -- the pending unbounded declaration.  The
                            -- recursively checked body was joined with that
                            -- declaration by 'materializeConsumerBound'
                            -- above; consume that construction result here.
                            case preparedLambdaConstructionSchemeInfo of
                              Just materializedInfo
                                | not
                                    ( null
                                        (resultBindersFor materializedInfo)
                                    ) ->
                                    materializedInfo
                              _ -> packetCompletedConstructionInfo
                          routedRefIn schemeInfo ref =
                            fromMaybe ref $ do
                              node <- typeBinderRefNode ref
                              IntMap.lookup
                                (getNodeId node)
                                (schemeInfoBinderRefSubst schemeInfo)
                          sameConstructionRoute schemeInfo left right =
                            any
                              (uncurry typeBinderRefsSameIdentity)
                              [ (left, right)
                              , (routedRefIn schemeInfo left, right)
                              , (left, routedRefIn schemeInfo right)
                              , ( routedRefIn schemeInfo left
                                , routedRefIn schemeInfo right
                                )
                              ]
                          resultBindersFor schemeInfo =
                            [ binder
                            | binder@(candidateRef, _) <-
                                schemeBinderRefs (siScheme schemeInfo)
                            , sameConstructionRoute
                                schemeInfo
                                resultRef
                                candidateRef
                            ]
                          rawResultBinders =
                            resultBindersFor rawConstructionInfo
                          completedResultBinders =
                            resultBindersFor completedConstructionInfo
                          routedResultRef =
                            routedRefIn rawConstructionInfo resultRef
                          completedElsewhere =
                            any
                              ( typeBinderRefsSameIdentity routedResultRef
                                  . routedRefIn rawConstructionInfo
                              )
                              ( maybeToList
                                  ( subtermGeneralizationCompilerExactCompletionRef
                                      packet
                                  )
                              )
                          exactCompletionMustOwnDeclaration =
                            subtermGeneralizationCompilerExactResultStage packet
                              == Just CompleteAfterCompilerExact
                          materializePendingResultBound binder@(ref, mbRawBound)
                            -- The source-owned declaration is already part of
                            -- the exact producer ABI.  Publish the bound that
                            -- this lambda constructed from its checked body,
                            -- even when packet preparation retained a
                            -- non-empty provisional opening such as
                            -- Bottom -> Bottom.
                            | subtermGeneralizationCompilerExactResultStage packet
                                == Just CompleteBeforeCompilerExact =
                                completedResultBound ref
                            | isJust mbRawBound =
                                pure binder
                            | otherwise =
                                completedResultBound ref
                          completedResultBound ref =
                            case completedResultBinders of
                              [(_, Just completedBound)] ->
                                pure (ref, Just completedBound)
                              [(_, Nothing)] ->
                                missingCompletedResultBound
                                  "has an unbounded completed declaration"
                              [] ->
                                missingCompletedResultBound
                                  "is absent from the completed packet"
                              _ ->
                                missingCompletedResultBound
                                  "has multiple completed declarations"
                          missingCompletedResultBound reason =
                            Left
                              ( ValidationFailed
                                  [ "lambda packet result " ++ reason
                                  , "  lambda owner: " ++ show lambdaOwner
                                  , "  result: " ++ show resultRef
                                  , "  raw declarations: "
                                      ++ show rawResultBinders
                                  , "  completed declarations: "
                                      ++ show completedResultBinders
                                  , "  completed packet scheme: "
                                      ++ show
                                        (siScheme completedConstructionInfo)
                                  ]
                              )
                      case rawResultBinders of
                        [] -> pure []
                        [_]
                          | completedElsewhere
                          , exactCompletionMustOwnDeclaration ->
                              pure []
                        [binder] ->
                          pure <$> materializePendingResultBound binder
                        _ ->
                          Left
                            ( ValidationFailed
                                [ "lambda packet result has multiple construction declarations"
                                , "  lambda owner: " ++ show lambdaOwner
                                , "  result: " ++ show resultRef
                                , "  construction-domain result: "
                                    ++ show routedResultRef
                                , "  declarations: " ++ show rawResultBinders
                                , "  packet construction scheme: "
                                    ++ show (siScheme rawConstructionInfo)
                                ]
                            )
                _ -> pure []
            let packetResultCompletionProvenance =
                  case
                      ( lccExpectedEnclosingBound lambdaConsumerConstruction
                      , preparedLambdaGeneralization
                          >>= subtermGeneralizationCompilerExactBoundary
                      , (\(_, _, annotationEdge, _) -> annotationEdge)
                          <$> mAnnLambda
                      , preservedAnnotationTy
                      )
                    of
                    (Just _, _, _, _) -> ConstructionExactEndpoint
                    (Nothing, Just exactEdge, Just annotationEdge, _)
                      | exactEdge == annotationEdge ->
                          ConstructionSourceAnnotationEndpoint
                    (Nothing, Just _, _, _) ->
                      ConstructionExactEndpoint
                    (Nothing, Nothing, _, Just _) ->
                      ConstructionSourceAnnotationEndpoint
                    (Nothing, Nothing, _, Nothing) ->
                      ConstructionLocalGammaBound
                -- 'materializeConsumerBound' has already joined the packet's
                -- consumer identity with the recursively checked body source.
                -- That is the Figure 15.3.5 local Gamma declaration.  A
                -- preserved parameter annotation or an enclosing exact
                -- endpoint can carry another bound for the same identity,
                -- but only as entry/consumption evidence; neither may replace
                -- the checked local declaration by list precedence.  Compare
                -- exact-edge identity with the desugared parameter annotation
                -- edge: an enclosing-result marker deliberately has no
                -- completion stage, but its boundary identity still proves
                -- that the packet view is an outer exact endpoint.
                lambdaPacketResultCandidates =
                  map
                    (\binder -> (ConstructionLocalGammaBound, binder))
                    locallyOwnedPreparedLambdaSchemeBinders
                    ++ map
                      (\binder -> (packetResultCompletionProvenance, binder))
                      packetResultCompletionBinders
            preparedLambdaCompletionBinders <-
              mergeConstructionBinderBoundsByProvenance
                ("lambda packet result " ++ show lambdaOwner)
                lambdaPacketResultCandidates
            lambdaConstructionCandidates0 <-
              mergeConstructionBinderSources
                ("lambda " ++ show lambdaOwner)
                ordinaryLambdaConstructionBinders
                preparedLambdaCompletionBinders
            let
                -- Gen(Gamma, tau) never re-emits an identity already free in
                -- the enclosing lexical Gamma.  Ordinary edge replay may
                -- still list that identity because it needed the slot while
                -- constructing the body; treating the slot as a declaration
                -- here would build @Lambda c. ...@ around an evidence helper
                -- whose captured type already mentions @c@.
                lambdaConstructionCandidates =
                  [ binder
                  | binder <- lambdaConstructionCandidates0
                  , not (enclosingOwnsCompletedCandidate binder)
                  ]
                enclosingOwnsCompletedCandidate (ref, mbBound) =
                  any
                    (typeBinderRefsSameIdentity ref)
                    enclosingTermSchemeFreeRefs
                    || ( envOwnsExactTypeBinderRef
                          envAtLambdaOutgoingConstructionBoundary
                          ref
                          && case
                            [ ambientBound
                            | (ambientRef, ambientBound) <-
                                Map.toList
                                  ( envTypeBindings
                                      envAtLambdaOutgoingConstructionBoundary
                                  )
                            , typeBinderRefsSameIdentity ref ambientRef
                            ]
                          of
                            [] -> False
                            ambientBounds ->
                              all
                                (operationalEndpointTypesAgree candidateBound)
                                ambientBounds
                       )
                  where
                    candidateBound = maybe TBottom tyToElab mbBound
                enclosingTermSchemeFreeRefs =
                  concatMap
                    (freeTypeVarRefsType . ebSchemeType)
                    ( authoritativeEnvBindings
                        envAtLambdaOutgoingConstructionBoundary
                    )
                lambdaPayloadRenames =
                  ordinaryBodyConsumerOccurrenceRenames
                    ++ lambdaParamLocalGammaRenames
                      paramNode
                      paramTy
                      (map fst ordinaryLambdaConstructionBinders)
                      outgoingConstructionBinderRenames
                      ordinaryLambdaLocalBinderRoutes
                paramTyForConstruction =
                  applyTypeVarRefRenames lambdaPayloadRenames paramTy
                bodyResultTyForConstruction =
                  applyTypeVarRefRenames lambdaPayloadRenames bodyResultTy
                instantiatedBody0 =
                  case bodyInst of
                    InstId -> bodyForOutgoingComputation
                    _ -> ETyInst bodyForOutgoingComputation bodyInst
                instantiatedBody =
                  renameTermTypeVars lambdaPayloadRenames instantiatedBody0
                lambdaTerm =
                  mkLocalLam
                    binderDetails
                    paramTyForConstruction
                    instantiatedBody
                compilerExactSourceResultBoundCertificate = do
                  packet <- preparedLambdaGeneralization
                  guard
                    ( subtermGeneralizationCompilerExactResultStage packet
                        == Just CompleteBeforeCompilerExact
                    )
                  exactBoundary <-
                    subtermGeneralizationCompilerExactBoundary packet
                  resultRef <-
                    subtermGeneralizationCompilerExactResultRef packet
                  pure
                    CompilerExactResultBoundCertificate
                      { cerbcOwner = idDetailsIdentityKey binderDetails
                      , cerbcBoundary = exactBoundary
                      , cerbcResultRef = resultRef
                      , cerbcBound =
                          applyTypeVarRefRenames
                            lambdaPayloadRenames
                            bodySourceTyForOutgoingComputation
                      }
            lambdaSchemeBinders <-
              constructionGammaCompletionBinders
                lambdaConstructionCandidates
                lambdaTerm
                (TArrow paramTyForConstruction bodyResultTyForConstruction)
            let lambdaConstructionScheme =
                  mkElabSchemeWithRefs
                    lambdaSchemeBinders
                    (TArrow paramTyForConstruction bodyResultTyForConstruction)
                lambdaConstructionTy =
                  schemeToType lambdaConstructionScheme
                completedLambda =
                  foldr
                    (\(ref, mbBound) body -> ETyAbsRef ref mbBound body)
                    lambdaTerm
                    lambdaSchemeBinders
            completedLambdaBeforeConstructionQuotient <-
              case lccExpectedEnclosingBound lambdaConsumerConstruction of
                Nothing -> pure completedLambda
                Just expectedBound ->
                  let expectedConstructionBound =
                        applyTypeVarRefRenames
                          outgoingConstructionBinderRenames
                          expectedBound
                      validatedLeadingElimination = do
                        projection <- validatedBodyConsumerProjection
                        packet <- exactLambdaBodyPacket
                        let constructionOperatedType =
                              applyTypeVarRefRenames
                                outgoingConstructionBinderRenames
                                ( schemeToType
                                    ( siScheme
                                        ( subtermGeneralizationOperatedSchemeInfo
                                            packet
                                        )
                                    )
                                )
                        validatedBodyConsumerLeadingElimination
                          constructionOperatedType
                          lambdaConstructionTy
                          expectedConstructionBound
                          projection
                   in case
                        maybe
                          ( leadingBoundSpecialization
                              lambdaConstructionTy
                              expectedConstructionBound
                          )
                          Right
                          validatedLeadingElimination
                      of
                        Right InstId -> pure completedLambda
                        Right specialization ->
                          -- The local constructor has already emitted the
                          -- flexible Gamma binders needed to check its body.
                          -- If the enclosing Gamma consumes their completed
                          -- bounds, construct that endpoint now with the
                          -- corresponding leading N eliminations instead of
                          -- accepting a mismatched type and repairing it later.
                          pure (ETyInst completedLambda specialization)
                        Left cause ->
                          Left
                            ( ValidationFailed
                                [ "completed lambda construction cannot reach its enclosing Gamma bound"
                                , "  lambda owner: " ++ show lambdaOwner
                                , "  lambda binder: " ++ show (idDetailsIdentityKey binderDetails)
                                , "  lambda parameter type: " ++ show paramTyForConstruction
                                , "  published expected bound: " ++ show expectedBound
                                , "  construction-domain expected bound: " ++ show expectedConstructionBound
                                , "  initial prepared scheme: " ++ show (siScheme <$> preparedLambdaSchemeInfo)
                                , "  raw packet construction scheme: "
                                    ++ show
                                      ( siScheme
                                          . subtermGeneralizationConsumerConstructionSchemeInfo
                                          <$> preparedLambdaGeneralization
                                      )
                                , "  packet consumer authority: "
                                    ++ show
                                      ( preparedLambdaGeneralization
                                          >>= subtermGeneralizationConsumerAuthority
                                      )
                                , "  packet Gamma authority: "
                                    ++ show
                                      ( preparedLambdaGeneralization
                                          >>= subtermGeneralizationGammaAuthority
                                      )
                                , "  packet construction binder renames: "
                                    ++ show
                                      ( maybe
                                          []
                                          subtermGeneralizationConstructionBinderRenames
                                          preparedLambdaGeneralization
                                      )
                                , "  boundary construction aliases: "
                                    ++ show
                                      ( envConstructionGammaAliases
                                          envAtLambdaOutgoingConstructionBoundary
                                      )
                                , "  prepared construction scheme: " ++ show (siScheme <$> preparedLambdaConstructionSchemeInfo)
                                , "  constructed type: " ++ show lambdaConstructionTy
                                , "  construction candidates: " ++ show lambdaConstructionCandidates
                                , "  constructed binders: " ++ show lambdaSchemeBinders
                                , "  body source type: " ++ show bodySourceTyForOutgoingComputation
                                , "  raw body term: " ++ show bodyRaw
                                , "  stripped body term: " ++ show bodyStripped
                                , "  child Gamma certificates: "
                                    ++ show
                                      ( elaboratedLocalGammaConstructionCertificates
                                          bodyElaboration
                                      )
                                , "  body instantiation: " ++ show bodyInst
                                , "  body result type: " ++ show bodyResultTy
                                , "  cause: " ++ show cause
                                ]
                            )
            -- The outgoing construction environment has already validated
            -- this exact binder quotient.  Apply that same quotient to the
            -- completed lambda and its child certificates as one value before
            -- the final xMLF check.  In particular, an evidence binder is
            -- outside the recursively returned body, so projecting only the
            -- body leaves its binder payload in the source identity domain
            -- while its occurrences and Gamma are in the construction
            -- domain.  No term name or type shape participates here.
            let completedLambdaConstruction =
                  renameElaboratedTermBinderRefPayloads
                    outgoingConstructionBinderRenames
                    ElaboratedTerm
                      { elaboratedTerm =
                          completedLambdaBeforeConstructionQuotient
                      , elaboratedOwnerFinalConstruction = Nothing
                      , elaboratedLocalGammaConstructionCertificates =
                          elaboratedLocalGammaConstructionCertificates
                            bodyElaboration
                      , elaboratedCompilerExactResultBoundCertificates =
                          elaboratedCompilerExactResultBoundCertificates
                            bodyElaboration
                            ++ maybeToList
                              compilerExactSourceResultBoundCertificate
                      }
                completedLambdaForEnclosing =
                  elaboratedTerm completedLambdaConstruction
                lambdaSchemeBinderRefsForConstruction =
                  map
                    (renamePublishedRef outgoingConstructionBinderRenames . fst)
                    lambdaSchemeBinders
                completedLambdaTcEnv =
                  restrictTypeCheckEnvToFreeTermBindings
                    completedLambdaForEnclosing
                    (typeCheckEnvFrom envForCompletedLambdaConstruction)
            case
                TypeCheck.typeCheckWithEnv
                  completedLambdaTcEnv
                  completedLambdaForEnclosing
              of
                Right completedLambdaTy ->
                  let ownerUsedAmbientBinderRefs =
                        ownerFinalAmbientBinderRefs
                          completedLambdaTcEnv
                          lambdaSchemeBinderRefsForConstruction
                          completedLambdaForEnclosing
                          completedLambdaTy
                      ownerFinalConstruction =
                        OwnerFinalConstruction
                          { ofcOwner = lambdaOwner,
                            ofcConstructedType = completedLambdaTy,
                            ofcLocallyEmittedBinderRefs =
                              lambdaSchemeBinderRefsForConstruction,
                            ofcLocalBinderRoutes =
                              IntMap.filter
                                ( \routedRef ->
                                    any
                                      ( typeBinderRefsSameIdentity
                                          ( renamePublishedRef
                                              outgoingConstructionBinderRenames
                                              routedRef
                                          )
                                      )
                                      lambdaSchemeBinderRefsForConstruction
                                )
                                ( IntMap.unions
                                    [ ordinaryLambdaLocalBinderRoutes,
                                      envConstructionGammaAliases
                                        envWithOrdinaryLambdaConstruction,
                                      maybe
                                        IntMap.empty
                                        schemeInfoBinderRefSubst
                                        preparedLambdaConstructionSchemeInfo
                                    ]
                                ),
                            ofcUsedAmbientBinderRefs =
                              ownerUsedAmbientBinderRefs,
                            ofcBodyConsumerBoundRefinements =
                              filter
                                ( bodyConsumerBoundRefinementTargetsAny
                                    ownerUsedAmbientBinderRefs
                                )
                                ( maybeToList
                                    bodyConsumerBoundRefinementCertificate
                                )
                          }
                   in pure
                        ( renameElaboratedTermBinderRefPayloads
                            outgoingConstructionBinderRenames
                            ( completedLambdaConstruction
                                { elaboratedOwnerFinalConstruction =
                                    Just ownerFinalConstruction
                                }
                            )
                        )
                Left cause ->
                  Left
                    ( PhiInvariantError
                        ( unlines
                            [ "completed lambda is not typable in its enclosing construction environment"
                            , "lambda owner=" ++ show lambdaOwner
                            , "lambda construction candidates=" ++ show lambdaConstructionCandidates
                            , "lambda completion binders=" ++ show lambdaSchemeBinders
                            , "prepared lambda consumer="
                                ++ show
                                  ( preparedLambdaGeneralization
                                      >>= subtermGeneralizationConsumerIdentity
                                  )
                            , "prepared lambda Gamma consumer="
                                ++ show
                                  ( gpaConsumerIdentity
                                      <$> ( preparedLambdaGeneralization
                                              >>= subtermGeneralizationGammaAuthority
                                          )
                                  )
                            , "initial prepared lambda scheme=" ++ show (siScheme <$> preparedLambdaSchemeInfo)
                            , "prepared lambda scheme=" ++ show (siScheme <$> preparedLambdaConstructionSchemeInfo)
                            , "ordinary construction binders=" ++ show ordinaryLambdaConstructionBinders
                            , "enclosing construction aliases=" ++ show (envConstructionGammaAliases envAtLambdaOutgoingConstructionBoundary)
                            , "enclosing construction binder renames=" ++ show (envConstructionBinderRenames envAtLambdaOutgoingConstructionBoundary)
                            , "enclosing type bindings=" ++ show (envTypeBindings envAtLambdaOutgoingConstructionBoundary)
                            , "free enclosing term bindings="
                                ++ show
                                  ( TypeCheck.resolvedTermEnvEntries
                                      (TypeCheck.resolvedTermEnv completedLambdaTcEnv)
                                  )
                            , "completed lambda=" ++ show completedLambdaForEnclosing
                            , "typecheck=" ++ show cause
                            ]
                        )
                    )
          validateBodyInstantiationResult constructionContext bodyTypeEnv bodySourceTy inst =
            case TypeCheck.checkInstantiation bodyTypeEnv bodySourceTy inst of
              Right resultTy -> pure resultTy
              Left err ->
                Left
                  ( PhiInvariantError
                      ( unlines
                          ( [ "ALamF: lambda-body computation is not admissible in its constructed Gamma"
                            , "lambda owner=" ++ show (idDetailsIdentityKey binderDetails)
                            , "lambda scope=" ++ show (GenRef lambdaScopeGen)
                            , "lambda body edge=" ++ show bodyEid
                            , "source type=" ++ show bodySourceTy
                            , "outgoing Gamma result="
                                ++ show (snd <$> splitOutgoingGammaResult inst)
                            , "typecheck=" ++ show err
                            ]
                              ++ constructionContext
                          )
                      )
                  )
          -- A child packet is elaborated at its completed scheme, whereas the
          -- enclosing body edge starts at the packet's operated endpoint.
          -- Construct the consumer bound only through the exact
          -- semantic-exterior route selected above.  This covers both
          -- forall-opening and identity-only projection; neither a failed
          -- type check nor a type-shaped peer is allowed to nominate the
          -- consumer after the fact.
          projectBodyPacketConsumerIntoLambdaGamma owner edgeId sourceTy mbPacket mbProjectedInfo mbConsumerRoute requirements binders env ambientEnv =
            case (mbPacket, mbProjectedInfo, mbConsumerRoute) of
              (Just packet, Just projectedInfo, Just declarationAuthority) -> do
                let consumerRoute =
                      authorizedBodyConsumerRoute declarationAuthority
                validateBodyConsumerRoute
                  owner
                  edgeId
                  packet
                  (envConstructionGammaAliases env)
                  consumerRoute
                routeRequirement <-
                  case
                      [ requirement
                      | requirement <-
                          grRequiredGammaBinders requirements
                      , edgeId
                          `elem` NonEmpty.toList
                            (rgbEdgeIds requirement)
                      , rgbExteriorNode requirement
                          == bcrExteriorNode consumerRoute
                      , operationalEndpointTypesAgree
                          (rgbOperatedType requirement)
                          (bcrOperatedType consumerRoute)
                      ]
                    of
                      [requirement] -> pure requirement
                      matches ->
                        projectionFailure
                          consumerRoute
                          "selected route has no unique exact construction requirement"
                          ["  matching requirements: " ++ show matches]
                publishedBound <-
                  case
                      TypeCheck.lookupTypeBindingRef
                        (bcrConstructionRef consumerRoute)
                        (typeCheckEnvFrom env)
                    of
                      Just bound -> pure bound
                      Nothing ->
                        projectionFailure
                          consumerRoute
                          "construction consumer has no exact Gamma binding"
                          []
                unless
                  ( alphaEqType
                      (authorizedBodyConsumerDeclarationBound declarationAuthority)
                      publishedBound
                      || churchAwareEqType
                        (authorizedBodyConsumerDeclarationBound declarationAuthority)
                        publishedBound
                  )
                  ( projectionFailure
                      consumerRoute
                      "construction consumer binding disagrees with its requirement"
                      [ "  requirement bound: "
                          ++ show
                            ( authorizedBodyConsumerDeclarationBound
                                declarationAuthority
                            )
                      , "  published bound: " ++ show publishedBound
                      ]
                  )
                let projectedOperatedTy =
                      schemeToType (siScheme projectedInfo)
                validatedProjection <-
                  mkValidatedBodyConsumerProjection
                    (acSourceBinderRefs annotationContext)
                    (envConstructionGammaAliases env)
                    (envConstructionBinderRenames env)
                    consumerRoute
                    sourceTy
                    projectedOperatedTy
                let operatesOnOwnDeclaration =
                      case
                          ( bcrConstructionOperatedType consumerRoute,
                            projectedOperatedTy
                          )
                        of
                          (TVarRef operatedRef, TVarRef projectedRef) ->
                            all
                              ( \ref ->
                                  typeBinderRefsSameIdentity
                                    ref
                                    (bcrConstructionRef consumerRoute)
                              )
                              [operatedRef, projectedRef]
                          _ -> False
                if operatesOnOwnDeclaration
                  then
                    -- A named unbounded node has declaration @a > bottom@
                    -- while its operated endpoint remains @a@.  The checked
                    -- body already inhabits that exact identity, so this is
                    -- the epsilon computation: preserve the declaration and
                    -- publish only the occurrence projection.  Constructing
                    -- @a > a@ would be ill-founded in xMLF Gamma.
                    pure
                      ( binders,
                        env,
                        ambientEnv,
                        Just validatedProjection,
                        Nothing
                      )
                  else do
                    projectedBound <-
                      either
                        ( \cause ->
                            projectionFailure
                              consumerRoute
                              "environment-projected packet is not a Gamma bound"
                              [ "  projected operated type: "
                                  ++ show projectedOperatedTy
                              , "  cause: " ++ cause
                              ]
                        )
                        Right
                        (elabToBound projectedOperatedTy)
                    let projectionProvenance =
                          bodyConsumerRouteProjectionProvenance
                            (envConstructionBinderRenames ambientEnv)
                            owner
                            (envLocalGammaClosures ambientEnv)
                            routeRequirement
                            declarationAuthority
                            projectedOperatedTy
                            (envTypeBindings ambientEnv)
                    ( projectedAmbientBindings,
                      bodyConsumerBoundRefinementCertificate
                      ) <-
                      projectValidatedAmbientConsumerBoundWithCertificate
                        projectionProvenance
                        declarationAuthority
                        validatedProjection
                        (envTypeBindings ambientEnv)
                    let refinesConsumer ref =
                          typeBinderRefsSameIdentity
                            ref
                            (bcrConstructionRef consumerRoute)
                            || typeBinderRefsSameIdentity
                              ref
                              (bcrSemanticRef consumerRoute)
                        refinedBinders =
                          [ if refinesConsumer ref
                              then (ref, Just projectedBound)
                              else binder
                          | binder@(ref, _) <- binders
                          ]
                        refinedEnv =
                          env
                            { envTypeBindings =
                                Map.mapWithKey
                                  ( \ref bound ->
                                      if refinesConsumer ref
                                        then projectedOperatedTy
                                        else bound
                                  )
                                  (envTypeBindings env)
                            }
                        refinedAmbientEnv =
                          ambientEnv
                            { envTypeBindings = projectedAmbientBindings
                            }
                    pure
                      ( refinedBinders,
                        refinedEnv,
                        refinedAmbientEnv,
                        Just validatedProjection,
                        bodyConsumerBoundRefinementCertificate
                      )
              _ -> pure (binders, env, ambientEnv, Nothing, Nothing)
            where
              projectionFailure
                :: BodyConsumerRoute
                -> String
                -> [String]
                -> Either ElabError a
              projectionFailure consumerRoute detail context =
                Left
                  ( ValidationFailed
                      ( [ "invalid lambda-body Gamma consumer projection"
                        , "  detail: " ++ detail
                        , "  owner: " ++ show owner
                        , "  edge: " ++ show edgeId
                        , "  route: " ++ show consumerRoute
                        ]
                          ++ context
                      )
                  )

          specializeCompletedBodyPacketForEdge constructionBinderRenames bodyTypeEnv sourceTy mbPacket mbValidatedProjection edgeId edgeTranslation =
            case (mbPacket, edgeTranslation) of
              (Just packet, _)
                | Just (edgePrefix, resultRef) <-
                    splitOutgoingGammaResult edgeTranslation
                , edgePrefix /= InstId
                , Just authority <- subtermGeneralizationConsumerAuthority packet
                , scaEdgeId authority == edgeId
                , scaConsumerIdentity authority == typeBinderRefIdentity resultRef
                , let operatedPacketTy =
                        applyTypeVarRefRenames
                          constructionBinderRenames
                          ( schemeToType
                              (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                          )
                , Right consumedPrefix <-
                    leadingBoundSpecialization operatedPacketTy sourceTy
                , consumedPrefix == edgePrefix ->
                    -- The checked child already publishes the body view of
                    -- this operated packet.  The edge witness still records
                    -- the N prefix that opens the packet forall, followed by
                    -- its outgoing Hyp.  Matching the complete prefix against
                    -- the identity-bearing packet scheme proves that N has
                    -- been consumed exactly once; retain only the Hyp rather
                    -- than applying N again to the body endpoint.
                    pure (InstAbstrRef resultRef)
              (Just packet, InstAbstrRef resultRef)
                | Just authority <- subtermGeneralizationConsumerAuthority packet
                , scaEdgeId authority == edgeId
                , scaConsumerIdentity authority == typeBinderRefIdentity resultRef -> do
                    edgeSourceTy <-
                      case TypeCheck.lookupTypeBindingRef resultRef bodyTypeEnv of
                        Just bound -> pure bound
                        Nothing ->
                          Left
                            ( ValidationFailed
                                [ "lambda body packet consumer is absent from its construction Gamma"
                                , "  edge: " ++ show edgeId
                                , "  consumer: " ++ show resultRef
                                ]
                            )
                    let constructionEdgeSourceTy =
                          applyTypeVarRefRenames
                            constructionBinderRenames
                            edgeSourceTy
                    specialization <-
                      case
                          mbValidatedProjection
                            >>= validatedBodyConsumerProjectionSpecialization
                              resultRef
                              edgeSourceTy
                        of
                        Just projectedSpecialization ->
                          -- The validated route has already installed the
                          -- checked source endpoint at this exact consumer.
                          -- Reapplying the packet's source-to-graph
                          -- construction route here would undo that
                          -- projection (Generated7 -> Graph0 in the imported
                          -- overlap case).
                          pure projectedSpecialization
                        Nothing ->
                          case leadingBoundSpecialization sourceTy constructionEdgeSourceTy of
                            Right inst -> pure inst
                            Left err ->
                              Left
                                ( ValidationFailed
                                    [ "completed lambda body packet cannot reach its operated edge source"
                                    , "  edge: " ++ show edgeId
                                    , "  packet scheme: " ++ show (siScheme (subtermGeneralizationSchemeInfo packet))
                                    , "  packet operated scheme: " ++ show (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                                    , "  validated body-consumer projection: "
                                        ++ show mbValidatedProjection
                                    , "  packet construction routes: " ++ show (subtermGeneralizationConstructionBinderRenames packet)
                                    , "  completed source: " ++ show sourceTy
                                    , "  published operated source: " ++ show edgeSourceTy
                                    , "  construction-domain operated source: " ++ show constructionEdgeSourceTy
                                    , "  outgoing edge translation: " ++ show edgeTranslation
                                    , "  cause: " ++ show err
                                    ]
                                )
                    pure (composeInst specialization edgeTranslation)
              _ -> pure edgeTranslation

          leadingBoundSpecialization sourceTy targetTy
            | alphaEqType sourceTy targetTy = Right InstId
            | TForallRef {} <- sourceTy =
                case
                    inferInstAppArgsFromSchemeRefsExact
                      (schemeBinderRefs sourceScheme)
                      (schemeBody sourceScheme)
                      targetTy
                  of
                    Just arguments@(_ : _) ->
                      specializeAtInferredArguments arguments
                    _ -> eliminateLeadingBound
            | otherwise =
                Left
                  ( InstantiationError
                      ( "leading-bound specialization endpoint mismatch: "
                          ++ show sourceTy
                          ++ " /= "
                          ++ show targetTy
                      )
                  )
            where
              sourceScheme = schemeFromType sourceTy

              specializeAtInferredArguments arguments = do
                (inst, appliedTy) <- applyLeadingArguments sourceTy arguments
                rest <- leadingBoundSpecialization appliedTy targetTy
                pure (composeInst inst rest)

              eliminateLeadingBound = do
                nextTy <- applyInstantiation sourceTy InstElim
                rest <- leadingBoundSpecialization nextTy targetTy
                pure (composeInst InstElim rest)

              applyLeadingArguments currentTy [] = pure (InstId, currentTy)
              applyLeadingArguments currentTy (argumentTy : remaining) = do
                let inst = instForLeadingTypeArgument currentTy argumentTy
                nextTy <- applyInstantiation currentTy inst
                (rest, resultTy) <-
                  applyLeadingArguments nextTy remaining
                pure (composeInst inst rest, resultTy)

          -- The child-packet specialization is composed before the outgoing
          -- Gamma Hyp.  Keep that prefix while identifying the one final
          -- result abstraction owned by the enclosing exact boundary.
          splitOutgoingGammaResult inst =
            case inst of
              InstAbstrRef ref -> Just (InstId, ref)
              InstSeq prefix suffix -> do
                (suffixPrefix, ref) <- splitOutgoingGammaResult suffix
                pure (composeInst prefix suffixPrefix, ref)
              _ -> Nothing

          -- When the completed lambda body already has the bound of the
          -- packet-selected result, Figure 15.3.5 constructs it with one
          -- direct Hyp. Replaying the earlier operated-view Phi would add and
          -- eliminate quantifiers that the enclosing lexical Gamma has
          -- already opened (the K row is the canonical example).
          directBodyGammaInstantiation bodyTypeEnv bodySourceTy mbDelayedExactResult mbResultRef =
            case (mbDelayedExactResult, mbResultRef) of
              (Just _, _) -> Nothing
              (Nothing, Just resultRef)
                | TVarRef sourceRef <- bodySourceTy
                , typeBinderRefsSameIdentity sourceRef resultRef ->
                    Nothing
              (Nothing, Just resultRef) ->
                case TypeCheck.lookupTypeBindingRef resultRef bodyTypeEnv of
                  Just bound
                    | alphaEqType bodySourceTy bound ->
                        Just (InstAbstrRef resultRef)
                  _ -> Nothing
              (Nothing, Nothing) -> Nothing
       in ElabOut
            { elabDetailed = f,
              elabStripped = \env -> elaboratedTerm <$> f env
            }
    AAppF (fAnn, fOut) (aAnn, aOut) funSite argSite appNodeId ->
      let f env = do
            let applicationEdgeArtifacts = acEdgeArtifacts annotationContext
                applicationActiveConsumerAuthority =
                  envActiveSubtermConstruction env
                    >>= subtermGeneralizationConsumerAuthority
                applicationActiveConsumerIdentity =
                  scaConsumerIdentity <$> applicationActiveConsumerAuthority
                applicationResolvedLookup details =
                  ebSchemeInfo
                    <$> lookupEnvBindingForDetails
                      details
                      env
                applicationSourceSchemeFor =
                  sourceSchemeInfoForConstruction
                    applicationActiveConsumerAuthority
                    applicationActiveConsumerAuthority
                    annotationContext
                    (algNamedSetReify algebraContext)
                    applicationResolvedLookup
                applicationAnn =
                  AApp fAnn aAnn funSite argSite appNodeId
            requirementEdges <-
              applicationRequirementEdges
                scopeContext
                applicationEdgeArtifacts
                (algExactProducerTypes algebraContext)
                applicationSourceSchemeFor
                (instantiationSiteEdgeId funSite)
                appNodeId
                applicationAnn
            directRequirementEdges0 <-
              requirementEdgesForSources
                scopeContext
                applicationEdgeArtifacts
                (algExactProducerTypes algebraContext)
                applicationSourceSchemeFor
                [ (instantiationSiteEdgeId funSite, fAnn)
                , (instantiationSiteEdgeId argSite, aAnn)
                ]
            let ownerSelectedEdgeKeys =
                  IntSet.fromList
                    [ getEdgeId edgeId
                    | (edgeId, _) <- requirementEdges
                    ]
                directEdgeKeys =
                  IntSet.fromList
                    [ getEdgeId edgeId
                    | (edgeId, _) <- directRequirementEdges0
                    ]
                overlappingOwnerAndDirectEdges =
                  IntSet.intersection
                    ownerSelectedEdgeKeys
                    directEdgeKeys
            unless
              (IntSet.null overlappingOwnerAndDirectEdges)
              ( Left
                  ( ValidationFailed
                      [ "application direct and ordinary Gamma ownership overlap"
                      , "  application node: " ++ show appNodeId
                      , "  function boundary: "
                          ++ show (instantiationSiteEdgeId funSite)
                      , "  overlapping edges: "
                          ++ show
                            ( map
                                EdgeId
                                (IntSet.toList overlappingOwnerAndDirectEdges)
                            )
                      ]
                  )
              )
            let -- Γ_e for each direct application edge is a
                -- computation-scope obligation, not ordinary Gamma_g
                -- ownership.  The sticky source-owner selector makes the
                -- two sets disjoint by construction.
                directRequirementEdges = directRequirementEdges0
                constructionDirectRequirementEdges =
                  [ ( edgeId
                    , applyTypeVarRefRenames
                        (envConstructionBinderRenames env)
                        <$> mbOperatedType
                    )
                  | (edgeId, mbOperatedType) <- directRequirementEdges
                  ]
            let constructionRequirementEdges =
                  [ ( edgeId
                    , applyTypeVarRefRenames
                        (envConstructionBinderRenames env)
                        <$> mbOperatedType
                    )
                  | (edgeId, mbOperatedType) <- requirementEdges
                  ]
            applicationSourceBinderRefs <-
              constructionSourceBinderRefs
                (scopeTypeBinderIdentityRepresentative scopeContext)
                env
                (acSourceBinderRefs annotationContext)
            applicationAmbientGammaAuthorities <-
              ambientGammaAuthoritiesForEnv env
            let applicationTarget =
                  generalizeTargetNode
                    (scPresolutionView scopeContext)
                    (scCanonical scopeContext appNodeId)
            applicationScopes <-
              resolveApplicationConstructionScopes
                (scCanonical scopeContext)
                (scGaParents scopeContext)
                (scScopeOverrides scopeContext)
                (instantiationSiteEdgeId funSite)
                appNodeId
                applicationTarget
            let applicationScope =
                  applicationOccurrenceScope applicationScopes
                applicationTargetScope =
                  applicationTargetGeneralizationScope applicationScopes
                applicationOwner =
                  LocalGammaOwner
                    { lgoConstructor = LocalApplicationGamma,
                      lgoBoundaryEdge = instantiationSiteEdgeId funSite,
                      lgoTermNode = appNodeId,
                      lgoScope = applicationScope
                    }
                applicationReachableNodes =
                  reachableFromWithBounds
                    ( nodeMapToIntMap
                        (cNodes (pvConstraint (scPresolutionView scopeContext)))
                    )
                    applicationTarget
            retainedDescendantClosures <-
              retainedDescendantGammaClosures
                (scopeRootForBoundary scopeContext)
                applicationOwner
                applicationReachableNodes
                (envLocalGammaClosures env)
                applicationAnn
            let inheritedRequirementEdges =
                  [ ( edgeId
                    , applyTypeVarRefRenames
                        (envConstructionBinderRenames env)
                        <$> IntMap.lookup
                          (getEdgeId edgeId)
                          (algExactProducerTypes algebraContext)
                    )
                  | closure <- retainedDescendantClosures
                  , edgeId <- NonEmpty.toList (lgcEdgeIds closure)
                  ]
            applicationRequirements0 <-
              generalizationRequirementsForRootEdgesInConstruction
                applicationActiveConsumerIdentity
                ( envGeneralizationAmbientTypeBinderRefs
                    env
                )
                applicationAmbientGammaAuthorities
                (scopeTypeBinderIdentityRepresentative scopeContext)
                (scCanonical scopeContext)
                (scGaParents scopeContext)
                (scPresolutionView scopeContext)
                applicationEdgeArtifacts
                applicationSourceBinderRefs
                (algSubtermGeneralizations algebraContext)
                constructionRequirementEdges
            applicationRequirementsBeforeNested <-
              if null retainedDescendantClosures
                then pure applicationRequirements0
                else do
                  descendantRequirements <-
                    generalizationRequirementsForRootEdgesInConstruction
                      applicationActiveConsumerIdentity
                      ( envGeneralizationAmbientTypeBinderRefs
                          env
                      )
                      applicationAmbientGammaAuthorities
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      (scCanonical scopeContext)
                      (scGaParents scopeContext)
                      (scPresolutionView scopeContext)
                      applicationEdgeArtifacts
                      applicationSourceBinderRefs
                      (algSubtermGeneralizations algebraContext)
                      inheritedRequirementEdges
                  inheritDescendantGammaRequirements
                    retainedDescendantClosures
                    (grRequiredGammaBinders descendantRequirements)
                    applicationRequirements0
            let applicationRequirementsAtOccurrence =
                  placeCurrentGammaRequirementsAt
                    applicationScope
                    applicationRequirementsBeforeNested
            mbNestedArgumentOwner <-
              case stripAnnExpr aAnn of
                AApp _ _ nestedFunSite _ nestedNode -> do
                  nestedScope <-
                    scopeRootForBoundary
                      scopeContext
                      (instantiationSiteEdgeId nestedFunSite)
                      nestedNode
                  pure
                    ( Just
                        LocalGammaOwner
                          { lgoConstructor = LocalApplicationGamma
                          , lgoBoundaryEdge =
                              instantiationSiteEdgeId nestedFunSite
                          , lgoTermNode = nestedNode
                          , lgoScope = nestedScope
                          }
                    )
                _ -> pure Nothing
            let nestedOwnerNeedsResidual owner =
                  any
                    ( \requirement ->
                        lgoTermNode owner
                          `elem` NonEmpty.toList (rgbResultRoots requirement)
                          && case
                              IntMap.lookup
                                (getNodeId (lgoTermNode owner))
                                ( grAmbientGammaAuthorities
                                    applicationRequirementsAtOccurrence
                                )
                            of
                              Just authority ->
                                agaBound authority == TBottom
                                  && rgbOperatedType requirement /= TBottom
                              Nothing -> False
                    )
                    (grRequiredGammaBinders applicationRequirementsAtOccurrence)
            mbNestedArgumentCertificate <-
              case mbNestedArgumentOwner of
                Just nestedOwner
                  | nestedOwnerNeedsResidual nestedOwner -> do
                      preliminaryArgument <-
                        elabDetailed
                          aOut
                          env
                            { envApplicationSourceOccurrence =
                                Just ApplicationArgumentOccurrence
                            , envExpectedTermEndpoint = Nothing
                            }
                      nestedCertificate <-
                        selectNestedApplicationResidualCertificate
                          nestedOwner
                          preliminaryArgument
                      pure
                        (Just (nestedOwner, nestedCertificate))
                _ ->
                  pure Nothing
            edgeLocalRequirementsBeforeNested0 <-
              generalizationRequirementsForRootEdgesInConstruction
                applicationActiveConsumerIdentity
                ( envGeneralizationAmbientTypeBinderRefs
                    env
                )
                applicationAmbientGammaAuthorities
                (scopeTypeBinderIdentityRepresentative scopeContext)
                (scCanonical scopeContext)
                (scGaParents scopeContext)
                (scPresolutionView scopeContext)
                applicationEdgeArtifacts
                applicationSourceBinderRefs
                (algSubtermGeneralizations algebraContext)
                constructionDirectRequirementEdges
            let edgeLocalRequirementsAtOccurrence =
                  placeCurrentGammaRequirementsAt
                    applicationScope
                    edgeLocalRequirementsBeforeNested0
            (applicationRequirements, edgeLocalRequirements0) <-
              case mbNestedArgumentCertificate of
                Just (nestedOwner, nestedCertificate) ->
                  inheritNestedApplicationResidualReplayAuthority
                    nestedOwner
                    nestedCertificate
                    applicationRequirementsAtOccurrence
                    edgeLocalRequirementsAtOccurrence
                Nothing ->
                  pure
                    ( applicationRequirementsAtOccurrence
                    , edgeLocalRequirementsAtOccurrence
                    )
            let applicationGeneralizationScope =
                  applicationGeneralizationScopeForRequirements
                    applicationScopes
                    applicationRequirements
            let requirementRawRef requirement =
                  typeBinderRefFromIdentity
                    (typeBinderIdentityFromNode (rgbExteriorNode requirement))
                    ( typeBinderIdentityStableName
                        (typeBinderIdentityFromNode (rgbExteriorNode requirement))
                    )
                insertApplicationSeedRoute routes (nodeKey, ref) =
                  case IntMap.lookup nodeKey routes of
                    Nothing -> pure (IntMap.insert nodeKey ref routes)
                    Just existing
                      | typeBinderRefsSameIdentity existing ref -> pure routes
                      | otherwise ->
                          Left
                            ( ValidationFailed
                                [ "application expected scheme has conflicting construction routes"
                                , "  graph node: " ++ show (NodeId nodeKey)
                                , "  first ref: " ++ show existing
                                , "  second ref: " ++ show ref
                                ]
                            )
                requirementOwnedByDifferentClosure requirement =
                  any
                    ( \edgeId ->
                        maybe
                          False
                          ((/= applicationOwner) . lgcOwner)
                          ( IntMap.lookup
                              (getEdgeId edgeId)
                              (envLocalGammaClosures env)
                          )
                    )
                    (NonEmpty.toList (rgbEdgeIds requirement))
                -- A root/enclosing construction may already have installed
                -- this exact Hyp.  Keep that requirement in the edge-local
                -- plan: 'constructionGammaBinders' will prove it is ambient
                -- and retain the exact declaration as an S' route without
                -- re-emitting a binder.  Dropping it here would preserve only
                -- the fact that some representative is scoped and lose the
                -- declaration identity needed by application construction.
                --
                -- A prepared local-closure certificate still assigns the
                -- whole merged edge requirement to one exact source
                -- constructor.  A nested application may use that edge, but
                -- it cannot replace the enclosing constructor's positive
                -- ownership proof with its edge-local Figure 15.3.5 fallback.
                edgeLocalRequirements =
                  edgeLocalRequirements0
                    { grRequiredGammaBinders =
                        filter
                          ( \requirement ->
                              not
                                ( requirementOwnedByDifferentClosure
                                    requirement
                                )
                          )
                          (grRequiredGammaBinders edgeLocalRequirements0)
                    }
            functionReplayRequirements <-
              applicationReplayRequirementsForEdge
                (instantiationSiteEdgeId funSite)
                applicationRequirements
                edgeLocalRequirements
            argumentReplayRequirements <-
              applicationReplayRequirementsForEdge
                (instantiationSiteEdgeId argSite)
                applicationRequirements
                edgeLocalRequirements
            mbActiveConsumerAuthority <-
              activeApplicationConsumerSchemeAuthority
                scopeContext
                env
                applicationScope
            applicationSchemeInfo <-
              if null (grRequiredGammaBinders applicationRequirements)
                then pure Nothing
                else do
                  -- The active consumer proves that this application must
                  -- construct the consumer's bound; it is not the result
                  -- node generalized by this application.  Figure 15.3.5
                  -- emits Gamma_g for the application's own result and
                  -- the enclosing edge subsequently applies Hyp for the
                  -- consumer.  Selecting the consumer node here would
                  -- re-emit that ambient binder and leave its bound
                  -- unopened.
                  (applicationSchemeRaw, applicationSubstRaw) <-
                    case
                        ( mbActiveConsumerAuthority
                        , envExpectedTermEndpoint env
                            >>= exactConstructionExpectedType
                        )
                      of
                        -- An exact endpoint is already backed by a source
                        -- constructor or an active prepared consumer.  Use
                        -- that typed scheme as the application's S'
                        -- authority; re-generalizing a solved result node can
                        -- expose its bound binder as a free variable after
                        -- RaiseMerge. Required root-Gamma binders are still
                        -- constructed below from 'applicationRequirements'.
                        (_, Just expectedTy) -> do
                          let expectedScheme = schemeFromType expectedTy
                              expectedBinderRoutes =
                                [ (getNodeId node, ref)
                                | (ref, _) <- schemeBinderRefs expectedScheme
                                , Just node <- [typeBinderRefNode ref]
                                ]
                              requirementRoutes =
                                [ ( getNodeId (rgbExteriorNode requirement)
                                  , requirementRawRef requirement
                                  )
                                | requirement <-
                                    grRequiredGammaBinders applicationRequirements
                                ]
                          expectedSubst <-
                            foldM
                              insertApplicationSeedRoute
                              IntMap.empty
                              (expectedBinderRoutes ++ requirementRoutes)
                          pure (expectedScheme, expectedSubst)
                        _ ->
                          case
                              scGeneralizeAtWithRequirements scopeContext
                                applicationRequirements
                                (Just (scGaParents scopeContext))
                                applicationGeneralizationScope
                                applicationTarget
                            of
                              Right generalized -> pure generalized
                              Left cause ->
                                Left
                                  ( ValidationFailed
                                      [ "application construction generalization failed"
                                      , "  application: " ++ show appNodeId
                                      , "  occurrence scope: "
                                          ++ show applicationScope
                                      , "  target generalization scope: "
                                          ++ show applicationTargetScope
                                      , "  selected construction scope: "
                                          ++ show applicationGeneralizationScope
                                      , "  target: " ++ show applicationTarget
                                      , "  expected result: "
                                          ++ show (envExpectedTermEndpoint env)
                                      , "  active packet: "
                                          ++ show
                                            ( siScheme
                                                . subtermGeneralizationConsumerConstructionSchemeInfo
                                                <$> envActiveSubtermConstruction env
                                            )
                                      , "  requirements: " ++ show applicationRequirements
                                      , "  cause: " ++ show cause
                                      ]
                                  )
                  applicationSchemeInfoAligned <-
                    alignSchemeInfoToConstructionGamma
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      applicationSourceBinderRefs
                      env
                      (schemeInfoFromRefSubst applicationSchemeRaw applicationSubstRaw)
                  applicationSchemeInfoOrdered <-
                    orderConstructionSchemeInfoBinders
                      "application packet placement"
                      applicationSchemeInfoAligned
                  let applicationPlacementPackets =
                        subtermGeneralizationsOwnedBy
                          applicationAnn
                          (algSubtermGeneralizations algebraContext)
                  applicationSchemeInfoBeforePlacement <-
                    publishTopologyConsumerRoutes
                      ( gaConstructionRouteNodes
                          (scCanonical scopeContext)
                          (scGaParents scopeContext)
                      )
                      applicationPlacementPackets
                      applicationSchemeInfoOrdered
                  applicationSchemePlaced <-
                    placeSubtermGeneralizationBindersWithRoutes
                      (schemeInfoBinderRefSubst applicationSchemeInfoBeforePlacement)
                      applicationPlacementPackets
                      (siScheme applicationSchemeInfoBeforePlacement)
                  let placedApplicationRefs =
                        map fst (schemeBinderRefs applicationSchemePlaced)
                      applicationSubstPlaced =
                        IntMap.map
                          ( \ref ->
                              fromMaybe
                                ref
                                (find (typeBinderRefsSameIdentity ref) placedApplicationRefs)
                          )
                          (schemeInfoBinderRefSubst applicationSchemeInfoBeforePlacement)
                  applicationSchemeInfo0 <-
                    generalizeSchemeInfoAgainstConstructionEnv
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      applicationSourceBinderRefs
                      env
                      ( schemeInfoFromRefSubst
                          applicationSchemePlaced
                          applicationSubstPlaced
                      )
                  pure (Just applicationSchemeInfo0)
            applicationPendingResultSourcePacket <-
              traverse
                ( mkApplicationPendingLocalResultSourcePacket
                    applicationOwner
                    (instantiationSiteEdgeId argSite)
                    (grRequiredGammaBinders applicationRequirements)
                )
                applicationSchemeInfo
            let edgeLocalRoutePairs requirement =
                  [ (getNodeId node, requirementRawRef requirement)
                  | node <-
                      rgbOperatedRoot requirement
                        : rgbExteriorNode requirement
                        : NonEmpty.toList (rgbResultRoots requirement)
                  ]
                insertEdgeLocalRoute routes (nodeKey, ref) =
                  case IntMap.lookup nodeKey routes of
                    Nothing -> pure (IntMap.insert nodeKey ref routes)
                    Just existing
                      | typeBinderRefsSameIdentity existing ref -> pure routes
                      | otherwise ->
                          Left
                            ( ValidationFailed
                                [ "edge-local application Gamma has conflicting routes"
                                , "  graph node: " ++ show (NodeId nodeKey)
                                , "  first ref: " ++ show existing
                                , "  second ref: " ++ show ref
                                ]
                            )
                mergeApplicationAlias routes (nodeKey, ref) =
                  case IntMap.lookup nodeKey routes of
                    Nothing -> pure (IntMap.insert nodeKey ref routes)
                    Just existing
                      | typeBinderRefsSameIdentity existing ref -> pure routes
                      | otherwise ->
                          Left
                            ( ValidationFailed
                                [ "application Gamma sources disagree on one route"
                                , "  graph node: " ++ show (NodeId nodeKey)
                                , "  owner-selected ref: " ++ show existing
                                , "  edge-local ref: " ++ show ref
                                ]
                            )
            edgeLocalSubst <-
              foldM
                insertEdgeLocalRoute
                IntMap.empty
                ( concatMap
                    edgeLocalRoutePairs
                    (grRequiredGammaBinders edgeLocalRequirements)
                )
            let edgeLocalSchemeInfo =
                  schemeInfoFromRefSubst
                    (schemeFromType TBottom)
                    edgeLocalSubst
            -- The direct argument lane owns a different S' packet from the
            -- owner-selected application scheme.  Build its provisional
            -- result certificate from the same exact routes that the
            -- edge-local Gamma planner consumes; reusing the owner packet
            -- can leave a valid argument exterior classified as ambient.
            edgeLocalPendingResultSourcePacket <-
              if null (grRequiredGammaBinders edgeLocalRequirements)
                then pure Nothing
                else
                  Just
                    <$> mkApplicationPendingLocalResultSourcePacket
                      applicationOwner
                      (instantiationSiteEdgeId argSite)
                      (grRequiredGammaBinders edgeLocalRequirements)
                      edgeLocalSchemeInfo
            (ownerSelectedGammaPlan, ownerSelectedGammaAliases) <-
              case applicationSchemeInfo of
                Nothing ->
                  pure
                    ( ConstructionGammaPlan
                        { cgpBinders = []
                        , cgpAmbientAliases = IntMap.empty
                        }
                    , ConstructionGammaAliases
                        { cgaRoutingAliases = IntMap.empty
                        , cgaAuthorityAliases = IntMap.empty
                        }
                    )
                Just schemeInfo -> do
                  gammaPlan <-
                    constructionGammaBinders
                      ( case mbActiveConsumerAuthority of
                          Just _ -> CompleteSchemeAndRequiredGamma
                          Nothing -> OwnerLocalAndRequiredGamma
                      )
                      scopeContext
                      applicationOwner
                      applicationPendingResultSourcePacket
                      Nothing
                      env
                      applicationRequirements
                      schemeInfo
                  gammaAliases <-
                    constructionGammaAliases
                      ( gaConstructionRouteNodes
                          (scCanonical scopeContext)
                          (scGaParents scopeContext)
                      )
                      applicationSourceBinderRefs
                      applicationRequirements
                      gammaPlan
                      schemeInfo
                  pure (gammaPlan, gammaAliases)
            let ownerSelectedBinders = cgpBinders ownerSelectedGammaPlan
                ownerSelectedAliases =
                  cgaRoutingAliases ownerSelectedGammaAliases
                ownerSelectedAuthorityAliases =
                  cgaAuthorityAliases ownerSelectedGammaAliases
            let edgeLocalPlanningEnv =
                  extendEnvTypeScopeWithAliases
                    ownerSelectedAliases
                    ownerSelectedBinders
                    env
            edgeLocalGammaPlan <-
              constructionGammaBinders
                RequiredGammaOnly
                scopeContext
                applicationOwner
                edgeLocalPendingResultSourcePacket
                Nothing
                edgeLocalPlanningEnv
                edgeLocalRequirements
                edgeLocalSchemeInfo
            edgeLocalGammaAliases <-
              constructionGammaAliases
                ( gaConstructionRouteNodes
                    (scCanonical scopeContext)
                    (scGaParents scopeContext)
                )
                applicationSourceBinderRefs
                edgeLocalRequirements
                edgeLocalGammaPlan
                edgeLocalSchemeInfo
            let edgeLocalBinders = cgpBinders edgeLocalGammaPlan
                edgeLocalAliases =
                  cgaRoutingAliases edgeLocalGammaAliases
                edgeLocalAuthorityAliases =
                  cgaAuthorityAliases edgeLocalGammaAliases
            applicationBinders <-
              mergeConstructionBinderSources
                "application owner and edge-local Gamma"
                ownerSelectedBinders
                edgeLocalBinders
            applicationAliases0 <-
              foldM
                mergeApplicationAlias
                ownerSelectedAliases
                (IntMap.toList edgeLocalAliases)
            applicationAuthorityAliases <-
              foldM
                mergeApplicationAlias
                ownerSelectedAuthorityAliases
                (IntMap.toList edgeLocalAuthorityAliases)
            -- Only owner/edge Gamma construction may name an application
            -- destination.  The generalized scheme substitution is routing
            -- metadata for reification and completion; promoting all of it to
            -- exact Gamma authority turns a bounded graph result back into a
            -- provisional type variable and forces a repair after EApp.
            let applicationAliases = applicationAliases0
            let applicationBinderForExpectedRef expectedRef =
                  find
                    (typeBinderRefsSameIdentity expectedRef . fst)
                    applicationBinders
                    <|> do
                      expectedNode <- typeBinderRefNode expectedRef
                      routedRef <-
                        IntMap.lookup
                          (getNodeId expectedNode)
                          applicationAliases
                      find
                        (typeBinderRefsSameIdentity routedRef . fst)
                        applicationBinders
                    <|> let alignedRef =
                              applyRefRenames
                                (envConstructionBinderRenames env)
                                expectedRef
                         in find
                              (typeBinderRefsSameIdentity alignedRef . fst)
                              applicationBinders
                -- Figure 15.3.5 constructs an application under Γ_g and
                -- closes that Gamma only after both edge computations have
                -- been emitted.  A parent can therefore supply the complete
                -- expected scheme @forall (alpha > sigma). tau@ while this
                -- boundary has already prepared the exact local binder for
                -- @alpha@.  Open only those leading quantifiers whose
                -- identity route ends at a binder owned by this application;
                -- the recursively constructed result endpoint is then
                -- @tau@ in that local identity domain.
                --
                -- The binder route and its bound are positive construction
                -- evidence.  A same-shaped forall with no route is left
                -- intact, and a routed binder with a different bound is an
                -- invariant failure rather than an inferred coercion.
                openExpectedApplicationResult expectedTy =
                  case expectedTy of
                    TForallRef expectedRef expectedBound body
                      | Just (constructionRef, constructionBound) <-
                          applicationBinderForExpectedRef expectedRef -> do
                          let expectedBoundTy =
                                maybe TBottom tyToElab expectedBound
                              constructionBoundTy =
                                maybe TBottom tyToElab constructionBound
                          unless
                            ( scopedTypesAgree
                                scopeContext
                                expectedBoundTy
                                constructionBoundTy
                            )
                            ( Left
                                ( ValidationFailed
                                    [ "application expected scheme disagrees with its prepared Gamma binder"
                                    , "  application: " ++ show appNodeId
                                    , "  expected binder: " ++ show expectedRef
                                    , "  construction binder: " ++ show constructionRef
                                    , "  expected bound: " ++ show expectedBoundTy
                                    , "  construction bound: " ++ show constructionBoundTy
                                    ]
                                )
                            )
                          openExpectedApplicationResult
                            ( applyTypeVarRefRenames
                                [(expectedRef, constructionRef)]
                                body
                            )
                    _ -> pure expectedTy
            applicationExpectedResultTy <-
              traverse
                openExpectedApplicationResult
                (envExpectedTermEndpoint env >>= exactConstructionExpectedType)
            let normalizeApplicationType =
                  normalizeScopedType
                    (acScopeContext (algAnnotationContext algebraContext))
                -- An active consumer owns the outgoing Hyp from the
                -- application's constructed result to its flexible result
                -- variable.  The application itself must therefore be built
                -- at that variable's bound.  Requiring the function
                -- occurrence to manufacture the flexible result would apply
                -- Hyp before EApp and turn an arrow into an abstract type.
                -- The enclosing edge emits the single outgoing Hyp after the
                -- application has been constructed.
                applicationConstructionResultTy =
                  case mbActiveConsumerAuthority of
                    Just authority ->
                      Just
                        ( normalizeApplicationType
                            (activeApplicationConsumerBound authority)
                        )
                    Nothing -> applicationExpectedResultTy
            let envForApplication =
                  if null applicationBinders && IntMap.null applicationAliases
                    then env
                    else
                      extendEnvTypeScopeWithAliases
                        applicationAliases
                        applicationBinders
                        env
                applicationArgumentOwner =
                  unlines
                    [ "application argument at "
                        ++ show appNodeId
                        ++ " in "
                        ++ show applicationScope
                    , "application binders=" ++ show applicationBinders
                    , "application aliases=" ++ show applicationAliases
                    ]
                applicationFunctionOwner =
                  unlines
                    [ "application function at "
                        ++ show appNodeId
                        ++ " in "
                        ++ show applicationScope
                    , "application scheme="
                        ++ show (siScheme <$> applicationSchemeInfo)
                    , "application scheme substitution="
                        ++ show
                          ( schemeInfoBinderRefSubst
                              <$> applicationSchemeInfo
                          )
                    , "application binders=" ++ show applicationBinders
                    , "application aliases=" ++ show applicationAliases
                    , "application required Gamma binders="
                        ++ show
                          ( grRequiredGammaBinders
                              applicationRequirements
                          )
                    , "application source-binder ref count="
                        ++ show
                          ( IntMap.size
                              (grSourceBinderRefs applicationRequirements)
                          )
                    , "edge-local required Gamma binders="
                        ++ show
                          ( grRequiredGammaBinders
                              edgeLocalRequirements
                          )
                    , "edge-local source-binder ref count="
                        ++ show
                          ( IntMap.size
                              (grSourceBinderRefs edgeLocalRequirements)
                          )
                    , "application expected result="
                        ++ diagnosticOptionalElabType
                          applicationExpectedResultTy
                    , "active packet="
                        ++ show
                          ( (\packet ->
                              ( siScheme (subtermGeneralizationSchemeInfo packet)
                              , subtermGeneralizationGammaBoundScheme packet
                              , siScheme
                                  (subtermGeneralizationOperatedSchemeInfo packet)
                              , siScheme
                                  ( subtermGeneralizationConsumerConstructionSchemeInfo
                                      packet
                                  )
                              , subtermGeneralizationConsumerAuthority packet
                              , subtermGeneralizationGammaAuthority packet
                              , subtermGeneralizationResultAbstractionRef packet
                              , subtermGeneralizationConstructionResultAbstractionRef packet
                              )
                            )
                              <$> envActiveSubtermConstruction env
                          )
                    , "ambient application refs="
                        ++ show (envGeneralizationAmbientTypeBinderRefs env)
                    ]
            let instantiationSourceEnv sourceEnv occurrence ann expectedTy =
                  sourceEnv
                    { envApplicationSourceOccurrence =
                        case stripAnnExpr ann of
                          AApp {} -> Just occurrence
                          _ -> Nothing
                    , envExpectedTermEndpoint = expectedTy
                    }
            argElaboration <-
              elabDetailed
                aOut
                ( instantiationSourceEnv
                    envForApplication
                    ApplicationArgumentOccurrence
                    aAnn
                    Nothing
                )
            case mbNestedArgumentCertificate of
              Nothing -> pure ()
              Just (nestedOwner, _) -> do
                actualCertificate <-
                  selectNestedApplicationResidualCertificate
                    nestedOwner
                    argElaboration
                (actualRequirements, actualEdgeLocalRequirements) <-
                  inheritNestedApplicationResidualReplayAuthority
                    nestedOwner
                    actualCertificate
                    applicationRequirementsAtOccurrence
                    edgeLocalRequirementsAtOccurrence
                unless
                  ( actualRequirements == applicationRequirements
                      && actualEdgeLocalRequirements
                        == edgeLocalRequirements0
                  )
                  ( Left
                      ( ValidationFailed
                          [ "nested application residual Gamma authority changed under the enclosing construction"
                          , "  owner: " ++ show nestedOwner
                          , "  preliminary requirements: "
                              ++ show applicationRequirements
                          , "  constructed requirements: "
                              ++ show actualRequirements
                          , "  preliminary edge-local requirements: "
                              ++ show edgeLocalRequirements0
                          , "  constructed edge-local requirements: "
                              ++ show actualEdgeLocalRequirements
                          ]
                      )
                  )
            let argTerm = elaboratedTerm argElaboration
                edgeTypeEnv = typeCheckEnvFrom envForApplication
                typeCheckLocal = TypeCheck.typeCheckWithEnv edgeTypeEnv
                -- Both children must meet at the same explicit xMLF
                -- endpoint.  Inlining a flexible bound here would equate
                -- @alpha@ with its lower bound and retain a now-redundant Hyp
                -- on a monomorphic argument.
                applicationTypesAgree = alphaEqType
                resolvedLookup details =
                  ebSchemeInfo <$> lookupEnvBindingForDetails details envForApplication
                typeBindingLookup ref =
                  snd
                    <$> find
                      (typeBinderRefsSameIdentity ref . fst)
                      (Map.toList (envTypeBindings envForApplication))
                constructionAliases =
                  envConstructionGammaAliases envForApplication
                requireType label term =
                  case typeCheckLocal term of
                    Right ty -> pure ty
                    Left err ->
                      Left
                        ( PhiInvariantError
                            ( unlines
                                [ "AAppF: " ++ label ++ " is not typable before edge application",
                                  "term=" ++ diagnosticShown term,
                                  "typecheck=" ++ diagnosticShown err
                                ]
                            )
                        )
                instantiate term computation =
                  case edgeComputationInstantiation computation of
                    InstId -> term
                    inst -> ETyInst term inst
                applicationFunctionSource term sourceTy =
                  case sourceTy of
                    TForallRef ref (Just _) body
                      | not
                          ( any
                              (typeBinderRefsSameIdentity ref)
                              (freeTypeVarRefsType body)
                          ) -> do
                          eliminatedTy <-
                            case
                                TypeCheck.checkInstantiation
                                  edgeTypeEnv
                                  sourceTy
                                  InstElim
                              of
                                Right ty -> pure ty
                                Left err ->
                                  failApplication
                                    [ "bounded function-source elimination is not admissible"
                                    , "source type=" ++ show sourceTy
                                    , "typecheck=" ++ show err
                                    ]
                          -- A vacuous bounded forall can sit in front of a
                          -- recursive function carrier.  Figure 15.3.5 must
                          -- construct its N elimination before the recursive
                          -- unroll; merely inspecting or stripping the forall
                          -- would leave the emitted term at the wrong type.
                          applicationFunctionSource
                            (ETyInst term InstElim)
                            eliminatedTy
                    muTy@(TMuRef muRef muBody) ->
                      case substTypeCaptureRef muRef muTy muBody of
                        unfoldedTy@TArrow {} ->
                          -- Iso-recursive application exposes the arrow with
                          -- an explicit xMLF unroll.  The edge computation is
                          -- then constructed on that unfolded occurrence; it
                          -- never treats @mu a. tau@ and @[mu a. tau/a]tau@
                          -- as equi-recursively equal.
                          pure (EUnroll term, unfoldedTy)
                        _ -> pure (term, sourceTy)
                    _ -> pure (term, sourceTy)
                failApplication :: [String] -> Either ElabError a
                failApplication details =
                  Left
                    ( PhiInvariantError
                        ( unlines
                            ( [ "AAppF: invalid constructed application"
                              , "function annotation="
                                  ++ show (annNode fAnn, annExprReferenceKey fAnn)
                              , "argument annotation="
                                  ++ show (annNode aAnn, annExprReferenceKey aAnn)
                              , "expected application type="
                                  ++ diagnosticExpectedTermEndpoint
                                    (envExpectedTermEndpoint env)
                              , "construction result endpoint="
                                  ++ diagnosticOptionalElabType
                                    applicationExpectedResultTy
                              , "active construction packet="
                                  ++ show
                                    ( (\packet ->
                                        ( subtermGeneralizationConsumerAuthority packet
                                        , subtermGeneralizationGammaAuthority packet
                                        , subtermGeneralizationResultAbstractionRef packet
                                        )
                                      )
                                        <$> envActiveSubtermConstruction env
                                    )
                              ]
                                ++ details
                            )
                        )
                    )
            argSourceTy0 <- requireType "argument source" argTerm
            let applicationMediatorArity =
                  transparentMediatorArity envForApplication fAnn
                    <|> case transparentMediatorKindAnn fAnn of
                      Just DirectIdentityMediator -> Just 0
                      Just (EtaTransparentMediator arity) -> Just arity
                      Nothing -> Nothing
                (argTermForTopology, argSourceTy) =
                  case (applicationMediatorArity, argSourceTy0) of
                    (Just arity, muTy@(TMuRef muRef muBody))
                      | arity > 0
                      , let unfoldedTy = substTypeCaptureRef muRef muTy muBody
                      , TArrow {} <- unfoldedTy ->
                          -- A validated eta mediator consumes a function.  An
                          -- iso-recursive function value reaches that domain
                          -- only through its explicit one-step unroll; make
                          -- that term/type pair the occurrence source before
                          -- constructing either edge computation.
                          (EUnroll argTerm, unfoldedTy)
                    _ -> (argTerm, argSourceTy0)
            argumentSourceSchemeInfo <-
              applicationSourceSchemeFor aAnn
            functionSourceSchemeInfo <-
              applicationSourceSchemeFor fAnn
            mediatedSourceConstruction <-
              mediatedApplicationConstruction
                envForApplication
                applicationSourceSchemeFor
                applicationAnn
            (argTopologyTy, graphResultTopologyTy) <-
              applicationDestinationTypes
                algebraContext
                applicationAliases
                funSite
                argSite
                appNodeId
            let sourceProjectedTopology =
                  resolveSourceBinderAliasesInType
                    (scopeTypeBinderIdentityRepresentative scopeContext)
                    applicationSourceBinderRefs
                    argTopologyTy
                closedArgumentTopology =
                  checkedArgumentClosedTopology
                    argumentSourceSchemeInfo
                    argSourceTy
                    sourceProjectedTopology
                checkedArgumentAtBound boundTy
                  | alphaEqType argSourceTy boundTy =
                      Just argSourceTy
                  | otherwise = do
                      eliminatedArgument <-
                        eliminateCheckedBoundedPrefix argSourceTy
                      guard
                        (not (alphaEqType eliminatedArgument argSourceTy))
                      guard (alphaEqType eliminatedArgument boundTy)
                      pure eliminatedArgument
                argumentTopologyIndependentOfActiveConsumer topologyRef =
                  maybe
                    True
                    ( \authority ->
                        not
                          ( typeBinderRefsSameIdentity
                              topologyRef
                              (activeApplicationConsumerRef authority)
                          )
                    )
                    mbActiveConsumerAuthority
                localBoundArgumentTopology =
                  case
                      ( mbActiveConsumerAuthority
                      , sourceProjectedTopology
                      )
                    of
                      (_, TVarRef topologyRef) -> do
                        guard
                          ( argumentTopologyIndependentOfActiveConsumer
                              topologyRef
                          )
                        (_, Just topologyBound) <-
                          find
                            ( typeBinderRefsSameIdentity topologyRef
                                . fst
                            )
                            applicationBinders
                        let boundTy = tyToElab topologyBound
                        checkedArgument <- checkedArgumentAtBound boundTy
                        guard (not (applicationResultDependsOn topologyRef))
                        pure checkedArgument
                      _ -> Nothing
                directLocalArgumentBinder =
                  case sourceProjectedTopology of
                    TVarRef topologyRef -> do
                      guard
                        ( argumentTopologyIndependentOfActiveConsumer
                            topologyRef
                        )
                      (_, Just topologyBound) <-
                        find
                          ( typeBinderRefsSameIdentity topologyRef
                              . fst
                          )
                          applicationBinders
                      guard
                        ( alphaEqType
                            argSourceTy
                            (tyToElab topologyBound)
                        )
                      -- This application owns the exact flexible declaration,
                      -- and the checked argument is already at its bound.
                      -- Construct both occurrence computations at the
                      -- declaration itself.  If it is absent from the final
                      -- result, the completed local Gamma is eliminated below;
                      -- specializing the children directly to the bound would
                      -- erase the Figure 15.3.5 construction certificate.
                      pure (topologyRef, tyToElab topologyBound)
                    _ -> Nothing
                directLocalArgumentTopology =
                  TVarRef . fst <$> directLocalArgumentBinder
                ambientBoundArgumentTopology =
                  case
                      ( mbActiveConsumerAuthority
                      , sourceProjectedTopology
                      )
                    of
                      (_, TVarRef topologyRef) -> do
                        guard
                          ( argumentTopologyIndependentOfActiveConsumer
                              topologyRef
                          )
                        authority <-
                          find
                            ( typeBinderRefsSameIdentity topologyRef
                                . agaExactRef
                            )
                            ( IntMap.elems
                                applicationAmbientGammaAuthorities
                            )
                        let boundTy = agaBound authority
                        guard (boundTy /= TBottom)
                        checkedArgument <- checkedArgumentAtBound boundTy
                        guard (not (applicationResultDependsOn topologyRef))
                        -- This is the ambient analogue of
                        -- 'localBoundArgumentTopology'.  The exact Gamma
                        -- declaration, together with an explicit leading N
                        -- computation when the checked occurrence is bounded,
                        -- proves that the occurrence reaches the bound.  When
                        -- the result does not retain the flexible declaration,
                        -- Figure 15.3.5 constructs both children there instead
                        -- of feeding an underdetermined graph variable into
                        -- the function occurrence.
                        pure checkedArgument
                      _ -> Nothing
                -- Retaining a construction-Gamma endpoint is determined by
                -- the complete prepared result dependency, not only by the
                -- graph result after its flexible bounds have been inlined.
                -- In the K/factory application that graph view is already
                -- @forall d. d -> Int@, while the application construction
                -- scheme still records @forall d. d -> a@ under @a >= Int@.
                -- Collapsing @a@ merely because the reduced view no longer
                -- mentions it specializes the function side too early.
                --
                -- Following the already prepared Gamma bounds keeps @a@ as
                -- the shared Figure 15.3.5 endpoint.  The argument occurrence
                -- constructs @Int <= a@ with Hyp(a), and the function replay
                -- therefore reaches the exact bound required by Hyp(b).
                resultTopologyDependsOn soughtRef = go Set.empty
                  where
                    go visited ty =
                      any (refDepends visited) (freeTypeVarRefsType ty)

                    refDepends visited ref
                      | typeBinderRefsSameIdentity soughtRef ref = True
                      | Set.member refIdentity visited = False
                      | otherwise =
                          maybe
                            False
                            (go (Set.insert refIdentity visited))
                            (typeBindingLookup ref)
                      where
                        refIdentity = typeBinderRefIdentity ref
                applicationResultDependsOn topologyRef =
                  resultTopologyDependsOn topologyRef graphResultTopologyTy
                    || maybe
                      False
                      ( resultTopologyDependsOn topologyRef
                          . schemeBody
                          . siScheme
                      )
                      applicationSchemeInfo
                retainedLocalBoundArgumentForResult =
                  case (mbActiveConsumerAuthority, sourceProjectedTopology) of
                    (Nothing, TVarRef topologyRef) ->
                      case
                          find
                            (typeBinderRefsSameIdentity topologyRef . fst)
                            applicationBinders
                        of
                          Just (_, Just topologyBound) ->
                            alphaEqType argSourceTy (tyToElab topologyBound)
                              && applicationResultDependsOn topologyRef
                          _ -> False
                    _ -> False
                constructionClosedArgumentTopology = do
                  guard (isNothing mbActiveConsumerAuthority)
                  topologyBinders <-
                    either
                      (const Nothing)
                      Just
                      ( constructionGammaCompletionBinders
                          applicationBinders
                          argTermForTopology
                          sourceProjectedTopology
                      )
                  let closedTopology =
                        foldr
                          (\(ref, mbBound) body -> TForallRef ref mbBound body)
                          sourceProjectedTopology
                          topologyBinders
                  guard (not (null topologyBinders))
                  guard (alphaEqType closedTopology argSourceTy)
                  pure closedTopology
                sourceDeclaredArgumentTopology = do
                  sourceSchemeInfo <- argumentSourceSchemeInfo
                  let sourceScheme = siScheme sourceSchemeInfo
                      declaredSourceTy = schemeToType sourceScheme
                  guard (null (schemeBinderRefs sourceScheme))
                  guard (alphaEqType declaredSourceTy argSourceTy)
                  guard
                    ( alphaEqType argSourceTy sourceProjectedTopology
                        || sourceProjectedTopology == TBottom
                    )
                  -- A monomorphic source declaration fixes the occurrence
                  -- endpoint without an InstApp choice.  Preserve it when
                  -- the graph agrees, or when the graph contributes only the
                  -- underdetermined Bottom topology.  In the latter case the
                  -- source declaration is the positive construction
                  -- certificate; passing Bottom into a recursively
                  -- constructed function child would manufacture an
                  -- endpoint no source term inhabits.
                  pure argSourceTy
                freezeCertifiedResultTopology ty =
                  case ty of
                    TArrow domainTy codomainTy ->
                      let (domainFrozen, frozenDomainTy) =
                            freezeCertifiedResultTopology domainTy
                          (codomainFrozen, frozenCodomainTy) =
                            freezeCertifiedResultTopology codomainTy
                       in ( domainFrozen || codomainFrozen
                          , TArrow frozenDomainTy frozenCodomainTy
                          )
                    TMuRef ref _ ->
                      case
                          typeBinderRefNode ref
                            >>= ( \node ->
                                    IntMap.lookup
                                      (getNodeId node)
                                      (frozenEndpointTypes envForApplication)
                                )
                        of
                          Just frozenTy -> (True, frozenTy)
                          Nothing -> (False, ty)
                    TVarRef ref ->
                      case
                          typeBinderRefNode ref
                            >>= ( \node ->
                                    IntMap.lookup
                                      (getNodeId node)
                                      (frozenEndpointTypes envForApplication)
                                )
                        of
                          Just frozenTy -> (True, frozenTy)
                          Nothing -> (False, ty)
                    _ -> (False, ty)
                (hasFrozenResultTopology, frozenGraphResultTopologyTy) =
                  freezeCertifiedResultTopology graphResultTopologyTy
                applicationConstructionResultHasConcreteGammaBound =
                  maybe
                    False
                    ( any
                        ( \ref ->
                            maybe
                              False
                              (/= TBottom)
                              (typeBindingLookup ref)
                        )
                        . freeTypeVarRefsType
                    )
                    applicationConstructionResultTy
                sourceViewOfGammaBound boundTy =
                  case boundTy of
                    TForallRef _ (Just _) _ ->
                      case applyInstantiation boundTy InstElim of
                        Right next -> sourceViewOfGammaBound next
                        Left _ -> boundTy
                    _ -> boundTy
                specializeConstructionGammaBounds resultTy =
                  foldl
                    ( \specialized ref ->
                        case typeBindingLookup ref of
                          Just boundTy
                            | boundTy /= TBottom ->
                                substTypeCaptureRef
                                  ref
                                  (sourceViewOfGammaBound boundTy)
                                  specialized
                          _ -> specialized
                    )
                    resultTy
                    (freeTypeVarRefsType resultTy)
                applicationConstructionResultForSource = do
                  resultTy <- applicationConstructionResultTy
                  let gammaSpecializedResult =
                        if applicationConstructionResultHasConcreteGammaBound
                          then specializeConstructionGammaBounds resultTy
                          else resultTy
                      (_, frozenResult) =
                        freezeCertifiedResultTopology gammaSpecializedResult
                  -- A sibling edge publishes its checked destination before
                  -- the function child is elaborated.  Resolve those graph
                  -- references here, while constructing the source arrow,
                  -- instead of first treating them as exact variables and
                  -- repairing the application after EApp.
                  Just frozenResult
                -- A checked source specialization must always agree with the
                -- exact result selected for this application.  Retain the
                -- independent graph-result check when that graph endpoint has
                -- itself been frozen, or when no active consumer owns the
                -- outgoing result computation.  With an active consumer and
                -- an unfrozen bare graph variable, however, Figure 15.3.5
                -- deliberately constructs the application at the consumer's
                -- bound; requiring the source result to equal that provisional
                -- variable would discard the positive consumer certificate
                -- and force a repair only after EApp.
                validatedSourceResult sourceResult = do
                  targetResult <-
                    applicationConstructionResultForSource
                      <|> do
                        guard hasFrozenResultTopology
                        pure frozenGraphResultTopologyTy
                  guard
                    ( sourceResultAgreesAfterLocalElimination
                        sourceResult
                        targetResult
                        || sourceRefinesProvisionalEndpoint
                          targetResult
                          sourceResult
                    )
                  guard
                    ( not
                        ( hasFrozenResultTopology
                            || isNothing mbActiveConsumerAuthority
                        )
                        || sourceResultAgreesAfterLocalElimination
                          sourceResult
                          frozenGraphResultTopologyTy
                        || sourceRefinesProvisionalEndpoint
                          frozenGraphResultTopologyTy
                          sourceResult
                    )
                  pure sourceResult
                  where
                    sourceResultAgreesAfterLocalElimination source target =
                      residualTopologyAgreesExact source target
                        || case directLocalArgumentBinder of
                          Just (localRef, localBound) ->
                            residualTopologyAgreesExact
                              (substTypeCaptureRef localRef localBound source)
                              target
                          Nothing -> False
                sourceRefinesProvisionalEndpoint provisional certified =
                  not (null provisionalRefs)
                    && not (topologyContainsBottom certified)
                    && all
                      (not . provisionalApplicationRef)
                      (freeTypeVarRefsType certified)
                    && case
                        matchTypeRefs
                          provisionalRefs
                          provisional
                          certified
                      of
                        Right substitution ->
                          all
                            (`Map.member` substitution)
                            provisionalRefs
                        Left _ -> False
                  where
                    provisionalRefs =
                      filter
                        provisionalApplicationRef
                        (freeTypeVarRefsType provisional)
                provisionalApplicationRef ref =
                  case typeBinderRefNode ref of
                    Nothing -> False
                    Just node ->
                      let key = getNodeId node
                       in provisionalTypeBinding ref
                            && IntMap.notMember
                              key
                              (frozenEndpointTypes envForApplication)
                            && IntMap.notMember
                              key
                              applicationAuthorityAliases
                            && IntMap.notMember
                              key
                              applicationAmbientGammaAuthorities
                            && IntMap.notMember
                              key
                              (grSourceBinderRefs applicationRequirements)
                            && not
                              ( any
                                  ( typeBinderRefsSameIdentity ref
                                      . fst
                                  )
                                  applicationBinders
                              )
                  where
                    provisionalTypeBinding candidate =
                      case typeBindingLookup candidate of
                        Nothing -> True
                        Just TBottom -> True
                        Just _ -> False
                eliminateCheckedBoundedPrefix current =
                  case current of
                    TForallRef _ (Just _) _ ->
                      case applyInstantiation current InstElim of
                        Right next -> eliminateCheckedBoundedPrefix next
                        Left _ -> Nothing
                    _ -> Just current
                boundedSourceArgument = do
                  boundedArgument <-
                    eliminateCheckedBoundedPrefix argSourceTy
                  guard (not (alphaEqType boundedArgument argSourceTy))
                  pure boundedArgument
                boundedSourceApplicationConstruction = do
                  boundedArgument <- boundedSourceArgument
                  functionSchemeInfo <- functionSourceSchemeInfo
                  (sourceDomain, sourceResult) <-
                    sourceSchemeApplicationAt
                      edgeTypeEnv
                      boundedArgument
                      applicationConstructionResultForSource
                      functionSchemeInfo
                  validatedResult <- validatedSourceResult sourceResult
                  pure
                    ( preserveCertifiedStructuralEndpoint
                        sourceDomain
                        boundedArgument
                    , validatedResult
                    )
                sourceConstructedArgumentTopology = do
                  argumentSchemeInfo <- argumentSourceSchemeInfo
                  functionSchemeInfo <- functionSourceSchemeInfo
                  let closedSourceTy =
                        schemeToType (siScheme argumentSchemeInfo)
                  guard
                    ( not
                        ( null
                            (schemeBinderRefs (siScheme argumentSchemeInfo))
                        )
                    )
                  guard (alphaEqType closedSourceTy argSourceTy)
                  -- The checked argument owns this closed type tree, while
                  -- successful source specialization proves that the function
                  -- consumes that exact endpoint.  Together they construct
                  -- the paper's self-application endpoint directly, without
                  -- first opening it to a provisional graph variable.
                  _ <-
                      sourceSchemeApplicationAt
                        edgeTypeEnv
                        closedSourceTy
                        applicationConstructionResultForSource
                        functionSchemeInfo
                  pure closedSourceTy
                exactArgumentTopology =
                  (macArgumentType <$> mediatedSourceConstruction)
                    <|> selectDirectLocalApplicationArgumentTopology
                      applicationConstructionResultForSource
                      localBoundArgumentTopology
                      directLocalArgumentTopology
                    <|> closedArgumentTopology
                    <|> sourceDeclaredArgumentTopology
                    <|> (fst <$> boundedSourceApplicationConstruction)
                    <|> sourceConstructedArgumentTopology
                    <|> constructionClosedArgumentTopology
                    <|> localBoundArgumentTopology
                    <|> ambientBoundArgumentTopology
                topologyContainsBottom ty =
                  case ty of
                    TBottom -> True
                    TArrow domainTy codomainTy ->
                      topologyContainsBottom domainTy
                        || topologyContainsBottom codomainTy
                    TConWithIdentity _ _ args ->
                      any topologyContainsBottom args
                    TVarAppRef _ args ->
                      any topologyContainsBottom args
                    TForallRef _ mbBound bodyTy ->
                      maybe
                        False
                        (topologyContainsBottom . tyToElab)
                        mbBound
                        || topologyContainsBottom bodyTy
                    TMuRef _ bodyTy -> topologyContainsBottom bodyTy
                    TVarRef {} -> False
                    TBaseWithIdentity {} -> False
                sharedArgumentTopology =
                  fromMaybe sourceProjectedTopology exactArgumentTopology
                sourceSpecializationArgumentTopology =
                  case directLocalArgumentBinder of
                    Just (_, localBound) -> localBound
                    Nothing -> sharedArgumentTopology
                graphResultHasConstructionAuthority =
                  all
                    ( \graphResultRef ->
                        case typeBinderRefNode graphResultRef of
                          Nothing -> False
                          Just graphResultNode ->
                            IntMap.member
                              (getNodeId graphResultNode)
                              applicationAuthorityAliases
                    )
                    (freeTypeVarRefsType graphResultTopologyTy)
                sourceConstructedApplication =
                  ( (\construction ->
                        ( macArgumentType construction
                        , macResultType construction
                        )
                    )
                      <$> mediatedSourceConstruction
                  )
                    <|> boundedSourceApplicationConstruction
                    <|> if
                          graphResultHasConstructionAuthority
                            && isNothing directLocalArgumentBinder
                      then Nothing
                      else do
                        functionSchemeInfo <- functionSourceSchemeInfo
                        let constructSourceApplication argumentTy = do
                              (sourceDomain, sourceResult) <-
                                sourceSchemeApplicationAt
                                  edgeTypeEnv
                                  argumentTy
                                  applicationConstructionResultForSource
                                  functionSchemeInfo
                                  <|> do
                                    targetResult <-
                                      applicationConstructionResultForSource
                                    sourceApplication <-
                                      sourceSchemeApplicationAt
                                        edgeTypeEnv
                                        argumentTy
                                        Nothing
                                        functionSchemeInfo
                                    guard
                                      ( sourceRefinesProvisionalEndpoint
                                          targetResult
                                          (snd sourceApplication)
                                      )
                                    pure sourceApplication
                              -- A source declaration proves how its own
                              -- function specializes, but it does not by
                              -- itself own this application's graph result.
                              -- Admit that residual only when the
                              -- parent/frozen construction endpoint validates
                              -- it and its complete residual topology still
                              -- agrees with the graph result.  Terminal-result
                              -- agreement alone can otherwise route a
                              -- structural ADT eliminator through an unrelated
                              -- graph-mu spine.
                              validatedResult <-
                                validatedSourceResult sourceResult
                              pure
                                ( preserveCertifiedStructuralEndpoint
                                    sourceDomain
                                    argumentTy
                                , validatedResult
                                )
                        constructSourceApplication
                          sourceSpecializationArgumentTopology
                          <|> do
                            guard
                              ( sourceRefinesProvisionalEndpoint
                                  sourceSpecializationArgumentTopology
                                  argSourceTy
                              )
                            constructSourceApplication argSourceTy
                sourceConstructedResultTy =
                  snd <$> sourceConstructedApplication
                sourceExactConstruction = do
                  (sourceDomain, sourceResult) <-
                    sourceConstructedApplication
                  let certifiedResult =
                        case applicationConstructionResultForSource of
                          Just parentResult
                            | sourceRefinesProvisionalEndpoint
                                parentResult
                                sourceResult ->
                                -- A parent endpoint made only from unowned
                                -- Bottom-backed graph variables is a checking
                                -- shape, not a competing construction
                                -- certificate.  The checked source
                                -- specialization fills those holes here, so
                                -- retain its identity-bearing residual rather
                                -- than publishing the provisional variables
                                -- as an exact endpoint.
                                sourceResult
                            | not
                                ( alphaEqTypePreservingStructuralBinders
                                    sourceResult
                                    parentResult
                                ) ->
                                -- Source specialization proves the exact
                                -- domain and that its residual Church
                                -- representation reaches the parent's result.
                                -- It does not replace that parent certificate
                                -- with the provisional representation returned
                                -- by the graph source (for example
                                -- @mu r. Bottom -> Bottom@ for @Unit@).
                                parentResult
                          _ -> sourceResult
                  pure (Just sourceDomain, certifiedResult)
                preserveCertifiedStructuralEndpoint provisional certified
                  | applicationTypesAgree provisional certified
                  , not
                      ( alphaEqTypePreservingStructuralBinders
                          provisional
                          certified
                      ) =
                      -- The checked argument owns the structural-data
                      -- endpoint.  A source specialization may alpha-rename
                      -- ordinary binders, but it must not replace that owner
                      -- with a provisional graph-mu binder in the emitted
                      -- InstApp sequence.
                      certified
                  | otherwise = provisional
                parentExactConstruction =
                  (\resultTy -> (Nothing, resultTy))
                    <$> applicationConstructionResultTy
                parentExactConstructionForSource =
                  (\resultTy -> (Nothing, resultTy))
                    <$> applicationConstructionResultForSource
                applicationExactConstruction
                  | isJust directLocalArgumentBinder =
                      sourceExactConstruction
                        <|> do
                          -- A local argument binder that disappears from the
                          -- result still shares its abstract domain between
                          -- the two children, but it does not make an
                          -- independently constructed parent result
                          -- provisional.  Supplying that result now lets a
                          -- nested function child choose all of its source
                          -- quantifiers before EApp; the local Gamma is still
                          -- closed and consumed below.
                          guard (not retainedLocalBoundArgumentForResult)
                          parentExactConstructionForSource
                  | retainedLocalBoundArgumentForResult = Nothing
                  | applicationConstructionResultHasConcreteGammaBound =
                      sourceExactConstruction
                        -- The enclosing application owns the Gamma binder,
                        -- while this child must be constructed at its direct
                        -- lower bound.  Reusing the unspecialized parent
                        -- result here would manufacture an abstract endpoint
                        -- and ask transport to repair it after EApp.
                        <|> parentExactConstructionForSource
                  | otherwise =
                      -- A checked source specialization carries the exact
                      -- structural identities of its result.  It has already
                      -- been validated against the complete parent/frozen
                      -- result topology above, so retain that construction
                      -- before falling back to the graph-domain presentation.
                      -- Preferring the latter would discard the positive
                      -- source certificate and later ask an application edge
                      -- to transport structurally equal but identity-distinct
                      -- recursive types.
                      sourceExactConstruction <|> parentExactConstruction
                applicationExactSourceDomain =
                  fst =<< applicationExactConstruction
                applicationExactResultTy =
                  snd <$> applicationExactConstruction
                resultTopologyTy =
                  fromMaybe
                    preparedTopologyTy
                    applicationExactResultTy
                  where
                    -- A checked constructor or active consumer owns the
                    -- application's exact result endpoint.  The generalized
                    -- scheme is only the fallback topology when no such
                    -- construction certificate exists; preferring its body
                    -- would make children construct a provisional graph result
                    -- and repair it with Hyp after EApp.
                    --
                    -- A retained local bounded argument is the exception:
                    -- its prepared result still depends on that same Gamma
                    -- binder.  Construct the application at the packet body,
                    -- close the binder, and consume its N step below.  Pushing
                    -- the enclosing annotation endpoint through the child
                    -- first would ask the function occurrence to turn the
                    -- flexible result into its lower bound, which is not an
                    -- xMLF computation.
                    preparedTopologyTy =
                      case (applicationBinders, applicationSchemeInfo) of
                        (_ : _, Just schemeInfo) ->
                          schemeBody (siScheme schemeInfo)
                        _ -> graphResultTopologyTy
                argumentEndpointAuthority
                  | Just sourceDomain <- applicationExactSourceDomain =
                      ExactTransportEndpoint sourceDomain
                  | Just checkedTopology <- exactArgumentTopology =
                      ExactTransportEndpoint checkedTopology
                  | otherwise = ReplayComputedEndpoint
                applicationFunctionOwnerWithTopology =
                  applicationFunctionOwner
                    ++ unlines
                      [ "argument source="
                          ++ diagnosticElabType argSourceTy
                      , "argument graph topology="
                          ++ diagnosticElabType argTopologyTy
                      , "argument source-projected topology="
                          ++ diagnosticElabType sourceProjectedTopology
                      , "argument source-binder routes="
                          ++ show
                            [ ( ref
                              , typeBinderRefNode ref
                                  >>= ( \node ->
                                          IntMap.lookup
                                            (getNodeId node)
                                            ( grSourceBinderRefs
                                                applicationRequirements
                                            )
                                      )
                              )
                            | ref <- freeTypeVarRefsType sourceProjectedTopology
                            ]
                    , "graph result topology="
                        ++ diagnosticElabType graphResultTopologyTy
                    , "graph result source-binder routes="
                        ++ show
                          [ ( ref
                            , typeBinderRefNode ref
                                >>= ( \node ->
                                        IntMap.lookup
                                          (getNodeId node)
                                          ( grSourceBinderRefs
                                              applicationRequirements
                                          )
                                    )
                            )
                          | ref <- freeTypeVarRefsType graphResultTopologyTy
                          ]
                      , "closed argument topology="
                          ++ diagnosticOptionalElabType closedArgumentTopology
                      , "bounded source application topology="
                          ++ diagnosticOptionalElabType
                            (fst <$> boundedSourceApplicationConstruction)
                      , "source-constructed argument topology="
                          ++ diagnosticOptionalElabType
                            sourceConstructedArgumentTopology
                      , "construction-closed argument topology="
                          ++ diagnosticOptionalElabType
                            constructionClosedArgumentTopology
                      , "local-bound argument topology="
                          ++ diagnosticOptionalElabType
                            localBoundArgumentTopology
                      , "source-declared argument topology="
                          ++ diagnosticOptionalElabType
                            sourceDeclaredArgumentTopology
                      , "ambient-bound argument topology="
                          ++ diagnosticOptionalElabType
                            ambientBoundArgumentTopology
                      , "active consumer ref="
                          ++ show
                            ( activeApplicationConsumerRef
                                <$> mbActiveConsumerAuthority
                            )
                      , "topology independent of active consumer="
                          ++ show
                            ( case sourceProjectedTopology of
                                TVarRef topologyRef ->
                                  argumentTopologyIndependentOfActiveConsumer
                                    topologyRef
                                _ -> False
                            )
                      , "ambient topology authority="
                          ++ show
                            ( case sourceProjectedTopology of
                                TVarRef topologyRef ->
                                  find
                                    ( typeBinderRefsSameIdentity topologyRef
                                        . agaExactRef
                                    )
                                    ( IntMap.elems
                                        applicationAmbientGammaAuthorities
                                    )
                                _ -> Nothing
                            )
                      , "argument after leading bounded N="
                          ++ diagnosticOptionalElabType boundedSourceArgument
                      , "function source scheme info="
                          ++ show
                            ( (\schemeInfo ->
                                ( map fst
                                    (schemeBinderRefs (siScheme schemeInfo))
                                , diagnosticElabType
                                    (schemeBody (siScheme schemeInfo))
                                )
                              )
                                <$> functionSourceSchemeInfo
                            )
                      , "bounded source result before validation="
                          ++ diagnosticOptionalElabType
                            (snd <$> boundedSourceApplicationConstruction)
                      , "application construction result="
                          ++ diagnosticOptionalElabType
                            applicationConstructionResultTy
                      , "source-normalized construction result="
                          ++ diagnosticOptionalElabType
                            applicationConstructionResultForSource
                      , "frozen graph result topology="
                          ++ diagnosticElabType frozenGraphResultTopologyTy
                      , "has frozen graph result topology="
                          ++ show hasFrozenResultTopology
                      , "argument topology retained by result="
                          ++ show
                            ( case sourceProjectedTopology of
                                TVarRef topologyRef ->
                                  applicationResultDependsOn topologyRef
                                _ -> False
                            )
                      , "selected exact argument topology="
                          ++ diagnosticOptionalElabType exactArgumentTopology
                      , "source-specialization argument topology="
                          ++ diagnosticElabType
                            sourceSpecializationArgumentTopology
                      ]
            argReplayComputation <-
              mkEdgeComputation
                algebraContext
                edgeTypeEnv
                resolvedLookup
                typeBindingLookup
                constructionAliases
                (frozenEndpointTypes envForApplication)
                argumentReplayRequirements
                applicationArgumentOwner
                ApplicationArgumentOccurrence
                argumentEndpointAuthority
                aAnn
                argSite
                argSourceTy
                argTopologyTy
            let -- Figure 15.3.5 feeds the function child the shared S'(n1)
                -- endpoint, not the argument's pre-edge source T(a2).  The
                -- retained topology owns its polymorphic structure (notably
                -- the sigma-id argument of the paper's g g), while the source
                -- sidecar projects free graph references into the checked
                -- occurrence's lexical identity domain.  The independently
                -- checked argument source proves that a polymorphic topology
                -- is a value endpoint; otherwise a leading graph forall may
                -- only be a stale generalized presentation of an occurrence
                -- whose computation has already eliminated it.
                expectedFunctionDomain =
                  case applicationExactSourceDomain of
                    Just sourceDomain -> sourceDomain
                    Nothing ->
                      case exactArgumentTopology of
                        Just exactTopology ->
                          -- The selector above returns only a source, Gamma, or
                          -- mediator-backed construction certificate.  Preserve
                          -- that identity-bearing endpoint verbatim: normalizing
                          -- an exact ambient variable through its flexible bound
                          -- can turn a lexical @c@ into Bottom and make a nested
                          -- application construct an arrow no source term has.
                          exactTopology
                        Nothing ->
                          let topologyDomain =
                                normalizeApplicationType sharedArgumentTopology
                              computedDomain =
                                normalizeApplicationType
                                  (edgeComputationTarget argReplayComputation)
                              checkedArgumentSource =
                                normalizeApplicationType argSourceTy
                           in case (topologyDomain, computedDomain) of
                                (_, _)
                                  | topologyContainsBottom topologyDomain ->
                                      computedDomain
                                (_, _)
                                  | applicationTypesAgree
                                      checkedArgumentSource
                                      topologyDomain ->
                                      topologyDomain
                                (TForallRef {}, TForallRef {}) ->
                                  topologyDomain
                                (TForallRef {}, _) ->
                                  -- A leading quantifier is a value-level endpoint
                                  -- only when the argument occurrence computation
                                  -- actually constructs one.  The graph topology may
                                  -- retain a generalized presentation that the
                                  -- checked argument has already eliminated.
                                  computedDomain
                                _ -> topologyDomain
                expectedFunctionTy =
                  -- The retained application topology is sufficient to check
                  -- the recursively constructed function child even when no
                  -- parent owns an exact result certificate.  This remains a
                  -- 'CheckingExpectedTerm' below: only
                  -- 'applicationExactResultTy' may confer exact construction
                  -- authority from either the parent consumer or the checked
                  -- source application.  Without either certificate the graph
                  -- codomain remains checking-only guidance.
                  Just
                    ( TArrow
                        expectedFunctionDomain
                        resultTopologyTy
                    )
                expectedFunctionEndpoint =
                  if identityWrapperAnn fAnn
                    then
                      -- The identity constructor's checked argument is the
                      -- complete source proof for both sides of its arrow.
                      -- A parent result expectation belongs to the outgoing
                      -- application computation; pushing it into the lambda
                      -- would make the function child and the identity branch
                      -- below use different construction authorities.
                      Just
                        ( ExactConstructionExpectedTerm
                            (TArrow argSourceTy argSourceTy)
                        )
                    else
                      case applicationExactResultTy of
                        Just exactResultTy ->
                          -- The argument occurrence computation has already
                          -- constructed the shared Figure 15.3.5 endpoint.
                          -- Together with the parent's exact result
                          -- certificate it owns the complete function
                          -- endpoint, independently of whether a transparent
                          -- source mediator supplied that result.  Publish
                          -- that arrow before recursively constructing a
                          -- nested function application so all source
                          -- quantifiers are selected by one endpoint.
                          Just
                            ( ExactConstructionExpectedTerm
                                (TArrow expectedFunctionDomain exactResultTy)
                            )
                        Nothing -> CheckingExpectedTerm <$> expectedFunctionTy
            envForFunction <-
              publishFrozenEndpoint
                envForApplication
                (edgeComputationFrozenEndpoint argReplayComputation)
            funElaboration <-
              case
                  elabDetailed
                    fOut
                    ( instantiationSourceEnv
                        envForFunction
                        ApplicationFunctionOccurrence
                        fAnn
                        expectedFunctionEndpoint
                    )
                of
                  Right elaboration -> pure elaboration
                  Left cause ->
                    Left
                      ( ValidationFailed
                          [ "application function failed under its constructed result plan"
                          , "  application: " ++ show appNodeId
                          , "  application binders: " ++ show applicationBinders
                          , "  application aliases: " ++ show applicationAliases
                          , "  graph result topology: "
                              ++ diagnosticElabType graphResultTopologyTy
                          , "  source result endpoint: "
                              ++ diagnosticOptionalElabType
                                sourceConstructedResultTy
                          , "  parent exact construction result: "
                              ++ diagnosticOptionalElabType
                                applicationConstructionResultTy
                          , "  selected exact construction result: "
                              ++ diagnosticOptionalElabType
                                applicationExactResultTy
                          , "  selected result topology: "
                              ++ diagnosticElabType resultTopologyTy
                          , "  expected function type: "
                              ++ diagnosticOptionalElabType expectedFunctionTy
                          , "  cause: " ++ show cause
                          ]
                      )
            let funTerm = elaboratedTerm funElaboration
                childLocalGammaCertificates =
                  elaboratedLocalGammaConstructionCertificates funElaboration
                    ++ elaboratedLocalGammaConstructionCertificates argElaboration
                childCompilerExactResultBoundCertificates =
                  elaboratedCompilerExactResultBoundCertificates funElaboration
                    ++ elaboratedCompilerExactResultBoundCertificates argElaboration
            (funTermForTopology, funComputation, argComputation) <-
              if identityWrapperAnn fAnn
                then do
                  -- A syntactic identity wrapper is valid at the exact
                  -- checked argument source.  A replay-only flexible target
                  -- such as @alpha >= Int@ is an outgoing edge obligation,
                  -- not the value endpoint of @(\\x. x) 1@.  Constructing both
                  -- children at @alpha@ would manufacture a local
                  -- @Lambda(alpha >= Int)@ whose binder disappears from the
                  -- rigid root scheme.  The identity constructor instead
                  -- discharges that edge at its producer and builds the
                  -- application directly at @Int@.
                  argComputation <-
                    mkEdgeComputation
                      algebraContext
                      edgeTypeEnv
                      resolvedLookup
                      typeBindingLookup
                      constructionAliases
                      (frozenEndpointTypes envForFunction)
                      argumentReplayRequirements
                      applicationArgumentOwner
                      ApplicationArgumentOccurrence
                      (ExactTransportEndpoint argSourceTy)
                      aAnn
                      argSite
                      argSourceTy
                      argTopologyTy
                  let argumentTargetTy = edgeComputationTarget argComputation
                      funTermForTopology =
                        alignLeadingLambdasToType
                          (TArrow argumentTargetTy argumentTargetTy)
                          funTerm
                  funSourceTy <- requireType "function source" funTermForTopology
                  funComputation <-
                    mkEdgeComputation
                      algebraContext
                      edgeTypeEnv
                      resolvedLookup
                      typeBindingLookup
                      constructionAliases
                      (frozenEndpointTypes envForFunction)
                      functionReplayRequirements
                      applicationFunctionOwnerWithTopology
                      ApplicationFunctionOccurrence
                      ( ExactApplicationFunctionEndpoint
                          argumentTargetTy
                          argumentTargetTy
                      )
                      fAnn
                      funSite
                      funSourceTy
                      (TArrow argumentTargetTy argumentTargetTy)
                  pure (funTermForTopology, funComputation, argComputation)
                else do
                  -- Figure 15.3.5 constructs both occurrence computations
                  -- independently and requires their endpoints to share one
                  -- alpha.  A reduced environment presentation can leave one
                  -- replay endpoint in the graph-source domain.  Transport
                  -- exactly that side; a monomorphic side cannot be changed,
                  -- so the construction normally has a unique direction.
                  let argRaw = argReplayComputation
                      argumentTarget = edgeComputationTarget argRaw
                  funSourceTy0 <- requireType "function source" funTerm
                  (funTermForSource, funSourceTy) <-
                    applicationFunctionSource funTerm funSourceTy0
                  let functionEndpointAuthority =
                        case applicationExactResultTy of
                          Just resultEndpoint ->
                            -- The checked argument computation and the parent
                            -- result certificate jointly determine the whole
                            -- function arrow.  Construct against that arrow
                            -- directly; result-only or domain-only inference
                            -- can otherwise leave a later source quantifier
                            -- outside the arrow (for example @b@ in
                            -- @__io_bind@).
                            ExactApplicationFunctionEndpoint
                              expectedFunctionDomain
                              resultEndpoint
                          Nothing -> ReplayComputedEndpoint
                      functionEndpointAt argumentEndpoint =
                        case applicationExactResultTy of
                          Just resultEndpoint ->
                            -- Domain transport must retain the result
                            -- certificate that selected 'funRaw'.  Rebuilding
                            -- only from the argument endpoint can otherwise
                            -- specialize the source domain correctly while
                            -- reverting its codomain to the provisional graph
                            -- result.
                            ExactApplicationFunctionEndpoint
                              argumentEndpoint
                              resultEndpoint
                          Nothing ->
                            ApplicationFunctionEndpoint argumentEndpoint
                  funRaw <-
                    mkEdgeComputation
                      algebraContext
                      edgeTypeEnv
                      resolvedLookup
                      typeBindingLookup
                      constructionAliases
                      (frozenEndpointTypes envForFunction)
                      functionReplayRequirements
                      applicationFunctionOwnerWithTopology
                      ApplicationFunctionOccurrence
                      functionEndpointAuthority
                      fAnn
                      funSite
                      funSourceTy
                      (TArrow sharedArgumentTopology resultTopologyTy)
                  let sourceFunctionScheme = schemeFromType funSourceTy
                      sourceFunctionBinderRefs =
                        map fst (schemeBinderRefs sourceFunctionScheme)
                      sourceClosedFunctionDomain =
                        case schemeBody sourceFunctionScheme of
                          TArrow sourceDomain _ ->
                            let projectedDomain =
                                  resolveSourceBinderAliasesInType
                                    (scopeTypeBinderIdentityRepresentative scopeContext)
                                    applicationSourceBinderRefs
                                    sourceDomain
                                dependsOnSourceBinder =
                                  any
                                    ( \freeRef ->
                                        any
                                          (typeBinderRefsSameIdentity freeRef)
                                          sourceFunctionBinderRefs
                                    )
                                    (freeTypeVarRefsType projectedDomain)
                             in if
                                  not dependsOnSourceBinder
                                    && null (freeTypeVarRefsType projectedDomain)
                                  then Just projectedDomain
                                  else Nothing
                          _ -> Nothing
                      -- A closed source domain is stronger than a graph replay
                      -- presentation only when both occurrence computations
                      -- can be constructed at that exact endpoint and no
                      -- complete result endpoint is already available.  This
                      -- is a positive source/edge certificate: it specializes
                      -- monomorphic consumers such as @useInt id@ without
                      -- manufacturing a local Gamma, while @g g@ is excluded
                      -- because its source domain depends on its source forall.
                      --
                      -- With a complete result certificate, reconstructing
                      -- from the domain alone is strictly weaker: it can
                      -- default a result-only source binder to Bottom and
                      -- overwrite the exact computation already selected
                      -- above (for example @mix[Bool]@ with @mix[Bottom]@).
                      sourceClosedConstruction = do
                        guard (isNothing applicationExactResultTy)
                        closedDomain <- sourceClosedFunctionDomain
                        argumentComputation <-
                          either
                            (const Nothing)
                            Just
                            ( mkEdgeComputation
                                algebraContext
                                edgeTypeEnv
                                resolvedLookup
                                typeBindingLookup
                                constructionAliases
                                (frozenEndpointTypes envForFunction)
                                argumentReplayRequirements
                                applicationArgumentOwner
                                ApplicationArgumentOccurrence
                                (ExactTransportEndpoint closedDomain)
                                aAnn
                                argSite
                                argSourceTy
                                sharedArgumentTopology
                            )
                        functionComputation <-
                          either
                            (const Nothing)
                            Just
                            ( mkEdgeComputation
                                algebraContext
                                edgeTypeEnv
                                resolvedLookup
                                typeBindingLookup
                                constructionAliases
                                (frozenEndpointTypes envForFunction)
                                functionReplayRequirements
                                applicationFunctionOwnerWithTopology
                                ApplicationFunctionOccurrence
                                (ApplicationFunctionEndpoint closedDomain)
                                fAnn
                                funSite
                                funSourceTy
                                (TArrow sharedArgumentTopology resultTopologyTy)
                            )
                        case edgeComputationTarget functionComputation of
                          TArrow functionDomain _
                            | applicationTypesAgree functionDomain closedDomain
                            , applicationTypesAgree
                                (edgeComputationTarget argumentComputation)
                                closedDomain ->
                                Just
                                  ( funTermForSource
                                  , functionComputation
                                  , argumentComputation
                                  )
                          _ -> Nothing
                  case edgeComputationTarget funRaw of
                    _
                      | Just construction <- sourceClosedConstruction ->
                          pure construction
                    TArrow functionDomain _ ->
                      if applicationTypesAgree functionDomain argumentTarget
                        then pure (funTermForSource, funRaw, argRaw)
                        else do
                          let argumentAtFunction =
                                do
                                  computation <-
                                    mkEdgeComputation
                                      algebraContext
                                      edgeTypeEnv
                                      resolvedLookup
                                      typeBindingLookup
                                      constructionAliases
                                      (frozenEndpointTypes envForFunction)
                                      argumentReplayRequirements
                                      applicationArgumentOwner
                                      ApplicationArgumentOccurrence
                                      (ExactTransportEndpoint functionDomain)
                                      aAnn
                                      argSite
                                      argSourceTy
                                      sharedArgumentTopology
                                  if applicationTypesAgree (edgeComputationTarget computation) functionDomain
                                    then pure computation
                                    else
                                      Left
                                        ( PhiInvariantError
                                            "argument transport did not reach the function domain"
                                        )
                              functionAtArgument =
                                do
                                  computation <-
                                    mkEdgeComputation
                                      algebraContext
                                      edgeTypeEnv
                                      resolvedLookup
                                      typeBindingLookup
                                      constructionAliases
                                      (frozenEndpointTypes envForFunction)
                                      functionReplayRequirements
                                      applicationFunctionOwnerWithTopology
                                      ApplicationFunctionOccurrence
                                      ( functionEndpointAt
                                          (normalizeApplicationType argumentTarget)
                                      )
                                      fAnn
                                      funSite
                                      funSourceTy
                                      (TArrow sharedArgumentTopology resultTopologyTy)
                                  case edgeComputationTarget computation of
                                    TArrow domainTy _
                                      | applicationTypesAgree domainTy argumentTarget -> pure computation
                                    _ ->
                                      Left
                                        ( PhiInvariantError
                                            "function transport did not reach the argument endpoint"
                                        )
                              functionAtLeadingArgumentEndpoint =
                                exposeLeadingArgumentEndpoint argRaw
                                where
                                  exposeLeadingArgumentEndpoint argumentComputation =
                                    case edgeComputationTarget argumentComputation of
                                      currentTarget@TForallRef {} -> do
                                        let elimination =
                                              applicationArgumentEliminationFor
                                                currentTarget
                                            composedInstantiation =
                                              composeInst
                                                ( edgeComputationInstantiation
                                                    argumentComputation
                                                )
                                                elimination
                                        exposedTarget <-
                                          case
                                              TypeCheck.checkInstantiation
                                                edgeTypeEnv
                                                currentTarget
                                                elimination
                                            of
                                              Right ty -> pure ty
                                              Left err ->
                                                Left
                                                  ( PhiInvariantError
                                                      ( unlines
                                                          [ "leading application argument elimination is not admissible"
                                                          , "argument endpoint=" ++ show currentTarget
                                                          , "elimination=" ++ show elimination
                                                          , "typecheck=" ++ show err
                                                          ]
                                                      )
                                                  )
                                        composedTarget <-
                                          case
                                              TypeCheck.checkInstantiation
                                                edgeTypeEnv
                                                ( edgeComputationSource
                                                    argumentComputation
                                                )
                                                composedInstantiation
                                            of
                                              Right ty -> pure ty
                                              Left err ->
                                                Left
                                                  ( PhiInvariantError
                                                      ( unlines
                                                          [ "composed application argument elimination is not admissible"
                                                          , "argument source="
                                                              ++ show
                                                                ( edgeComputationSource
                                                                    argumentComputation
                                                                )
                                                          , "instantiation="
                                                              ++ show composedInstantiation
                                                          , "typecheck=" ++ show err
                                                          ]
                                                      )
                                                  )
                                        if
                                            applicationTypesAgree
                                              composedTarget
                                              exposedTarget
                                          then pure ()
                                          else
                                            Left
                                              ( PhiInvariantError
                                                  "composed argument elimination has the wrong endpoint"
                                              )
                                        let exposedArgumentComputation =
                                              argumentComputation
                                                { edgeComputationInstantiation =
                                                    composedInstantiation
                                                , edgeComputationTarget =
                                                    exposedTarget
                                                }
                                            constructFunction = do
                                              computation <-
                                                mkEdgeComputation
                                                  algebraContext
                                                  edgeTypeEnv
                                                  resolvedLookup
                                                  typeBindingLookup
                                                  constructionAliases
                                                  (frozenEndpointTypes envForFunction)
                                                  functionReplayRequirements
                                                  applicationFunctionOwnerWithTopology
                                                  ApplicationFunctionOccurrence
                                                  (functionEndpointAt exposedTarget)
                                                  fAnn
                                                  funSite
                                                  funSourceTy
                                                  (TArrow sharedArgumentTopology resultTopologyTy)
                                              case edgeComputationTarget computation of
                                                TArrow domainTy _
                                                  | applicationTypesAgree
                                                      domainTy
                                                      exposedTarget ->
                                                      pure
                                                        ( computation
                                                        , exposedArgumentComputation
                                                        )
                                                _ ->
                                                  Left
                                                    ( PhiInvariantError
                                                        "function transport did not reach the exposed argument endpoint"
                                                    )
                                        case constructFunction of
                                          Right constructed -> pure constructed
                                          Left cause ->
                                            case exposedTarget of
                                              TForallRef {} ->
                                                exposeLeadingArgumentEndpoint
                                                  exposedArgumentComputation
                                              _ -> Left cause
                                      _ ->
                                        Left
                                          ( PhiInvariantError
                                              "argument endpoint has no leading forall to eliminate"
                                          )
                              functionMatchesTopology =
                                applicationTypesAgree
                                  functionDomain
                                  sharedArgumentTopology
                              argumentMatchesTopology =
                                applicationTypesAgree
                                  argumentTarget
                                  sharedArgumentTopology
                              closedConsumerResult =
                                applicationExactResultTy
                              resultMatchesClosedConsumer computation =
                                case (closedConsumerResult, edgeComputationTarget computation) of
                                  (Just expectedTy, TArrow _ codomainTy) ->
                                    applicationTypesAgree codomainTy expectedTy
                                  _ -> False
                          case (argumentAtFunction, functionAtArgument) of
                            (Right argComputation, Left _) ->
                              pure (funTermForSource, funRaw, argComputation)
                            (Left _, Right funComputation) ->
                              pure (funTermForSource, funComputation, argRaw)
                            (Right argComputation, Right funComputation)
                              | resultMatchesClosedConsumer funRaw
                                  && not (resultMatchesClosedConsumer funComputation) ->
                                  pure (funTermForSource, funRaw, argComputation)
                              | resultMatchesClosedConsumer funComputation
                                  && not (resultMatchesClosedConsumer funRaw) ->
                                  pure (funTermForSource, funComputation, argRaw)
                              | functionMatchesTopology && not argumentMatchesTopology ->
                                  pure (funTermForSource, funRaw, argComputation)
                              | argumentMatchesTopology && not functionMatchesTopology ->
                                  pure (funTermForSource, funComputation, argRaw)
                              | otherwise ->
                                  failApplication
                                    [ "application endpoint transport is ambiguous",
                                      "function domain=" ++ show functionDomain,
                                      "argument endpoint=" ++ show argumentTarget,
                                      "topology domain=" ++ show sharedArgumentTopology
                                    ]
                            (Left argumentError, Left functionError) ->
                              case functionAtLeadingArgumentEndpoint of
                                Right (funComputation, argComputation) ->
                                  pure
                                    ( funTermForSource
                                    , funComputation
                                    , argComputation
                                    )
                                Left leadingEndpointError ->
                                  failApplication
                                    [ "neither occurrence computation can be transported to the shared application domain",
                                      "function domain=" ++ show functionDomain,
                                      "argument endpoint=" ++ show argumentTarget,
                                      "argument transport=" ++ show argumentError,
                                      "function transport=" ++ show functionError,
                                      "leading argument endpoint transport="
                                        ++ show leadingEndpointError
                                    ]
                    replayTarget -> do
                      -- A non-arrow function replay has not yet consumed the
                      -- source scheme's leading quantifiers.  Its body still
                      -- owns the prospective arrow domain.  Construct the
                      -- argument at that domain when possible, then instantiate
                      -- the function at the same endpoint.  This matters for a
                      -- vacuous source quantifier: replay may introduce
                      -- @forall a. alpha@ on the argument edge, but eliminating
                      -- the function's @a@ exposes @alpha@, not that quantified
                      -- presentation.
                      let sourceScheme = schemeFromType funSourceTy
                          sourceBinderRefs = map fst (schemeBinderRefs sourceScheme)
                          domainDependsOnSourceBinder sourceDomain =
                            any
                              ( \freeRef ->
                                  any
                                    (typeBinderRefsSameIdentity freeRef)
                                    sourceBinderRefs
                              )
                              (freeTypeVarRefsType sourceDomain)
                          -- Figure 15.3.5 requires the function and argument
                          -- computations to meet at one value endpoint.  A
                          -- let-generalized argument can retain a leading
                          -- flexible forall after edge replay, while the
                          -- function's value domain has an arrow (or another
                          -- rigid head) below its own source quantifiers.  The
                          -- whole forall is not such a value endpoint: expose
                          -- the first endpoint that the source domain can
                          -- specialize to by constructing the paper's N
                          -- computation on the argument occurrence.
                          --
                          -- This selection is structural.  It does not try a
                          -- term, type-check it, and repair the result: exact
                          -- source-scheme inference decides whether the
                          -- current endpoint is consumable, and each otherwise
                          -- admissible step is the leading-forall elimination
                          -- rule itself.
                          exposeArgumentEndpoint sourceDomain = go
                            where
                              sourceBinders = schemeBinderRefs sourceScheme

                              go computation
                                | Just _ <-
                                    inferInstAppArgsFromSchemeRefsExact
                                      sourceBinders
                                      (schemeBody sourceScheme)
                                      ( TArrow
                                          (edgeComputationTarget computation)
                                          resultTopologyTy
                                      )
                                      <|> inferInstAppArgsFromSchemeRefsExact
                                        sourceBinders
                                        sourceDomain
                                        (edgeComputationTarget computation) =
                                    pure computation
                                | TForallRef {} <- edgeComputationTarget computation = do
                                    eliminatedTarget <-
                                      case
                                          TypeCheck.checkInstantiation
                                            edgeTypeEnv
                                            (edgeComputationTarget computation)
                                            InstElim
                                        of
                                          Left err ->
                                            failApplication
                                              [ "canonical application argument elimination is not admissible",
                                                "source domain=" ++ show sourceDomain,
                                                "argument endpoint="
                                                  ++ show (edgeComputationTarget computation),
                                                "typecheck=" ++ show err
                                              ]
                                          Right ty -> pure ty
                                    let eliminatedComputation =
                                          computation
                                            { edgeComputationInstantiation =
                                                composeInst
                                                  (edgeComputationInstantiation computation)
                                                  InstElim,
                                              edgeComputationTarget = eliminatedTarget
                                            }
                                    composedTarget <-
                                      case
                                          TypeCheck.checkInstantiation
                                            edgeTypeEnv
                                            (edgeComputationSource computation)
                                            (edgeComputationInstantiation eliminatedComputation)
                                        of
                                          Left err ->
                                            failApplication
                                              [ "canonical application argument computation does not apply",
                                                "source domain=" ++ show sourceDomain,
                                                "argument source="
                                                  ++ show (edgeComputationSource computation),
                                                "argument instantiation="
                                                  ++ show
                                                    ( edgeComputationInstantiation
                                                        eliminatedComputation
                                                    ),
                                                "typecheck=" ++ show err
                                              ]
                                          Right ty -> pure ty
                                    if applicationTypesAgree composedTarget eliminatedTarget
                                      then go eliminatedComputation
                                      else
                                        failApplication
                                          [ "canonical application argument computation has the wrong endpoint",
                                            "source domain=" ++ show sourceDomain,
                                            "constructed endpoint=" ++ show composedTarget,
                                            "expected endpoint=" ++ show eliminatedTarget
                                          ]
                                | otherwise =
                                    failApplication
                                      [ "application argument has no endpoint consumable by the function source domain",
                                        "source domain=" ++ show sourceDomain,
                                        "argument endpoint="
                                          ++ show (edgeComputationTarget computation)
                                      ]
                          constructAtArgument argComputation = do
                            let constructedArgumentTarget =
                                  edgeComputationTarget argComputation
                            funComputation <-
                              mkEdgeComputation
                                algebraContext
                                edgeTypeEnv
                                resolvedLookup
                                typeBindingLookup
                                constructionAliases
                                (frozenEndpointTypes envForFunction)
                                functionReplayRequirements
                                applicationFunctionOwnerWithTopology
                                ApplicationFunctionOccurrence
                                (functionEndpointAt constructedArgumentTarget)
                                fAnn
                                funSite
                                funSourceTy
                                (TArrow sharedArgumentTopology resultTopologyTy)
                            case edgeComputationTarget funComputation of
                              TArrow domainTy _
                                | applicationTypesAgree domainTy constructedArgumentTarget ->
                                    pure (funTermForSource, funComputation, argComputation)
                                | otherwise ->
                                    failApplication
                                      [ "function computation did not retain the constructed argument endpoint",
                                        "function domain=" ++ show domainTy,
                                        "argument destination=" ++ show constructedArgumentTarget
                                      ]
                              transportedTarget ->
                                failApplication
                                  [ "function computation with a non-arrow replay endpoint did not transport to the argument endpoint",
                                    "function site=" ++ show funSite,
                                    "argument site=" ++ show argSite,
                                    "replay endpoint=" ++ show replayTarget,
                                    "transported endpoint=" ++ show transportedTarget,
                                    "argument replay endpoint=" ++ show argumentTarget,
                                    "application topology endpoint=" ++ show sharedArgumentTopology
                                  ]
                          constructArgumentAtExactEndpoint endpoint =
                            mkEdgeComputation
                              algebraContext
                              edgeTypeEnv
                              resolvedLookup
                              typeBindingLookup
                              constructionAliases
                              (frozenEndpointTypes envForFunction)
                              argumentReplayRequirements
                              applicationArgumentOwner
                              ApplicationArgumentOccurrence
                              (ExactTransportEndpoint endpoint)
                              aAnn
                              argSite
                              argSourceTy
                              sharedArgumentTopology
                      argComputation <-
                        case schemeBody sourceScheme of
                          TArrow sourceDomain _
                            | domainDependsOnSourceBinder sourceDomain
                            , Just _ <-
                                inferInstAppArgsFromSchemeRefsExact
                                  (schemeBinderRefs sourceScheme)
                                  sourceDomain
                                  (edgeComputationTarget argRaw) ->
                                -- The occurrence edge already publishes the
                                -- shared Gamma endpoint (for example Unit ->
                                -- alpha via Hyp).  Retain that computation
                                -- before considering the concrete source.
                                pure argRaw
                            | domainDependsOnSourceBinder sourceDomain
                            , Just _ <-
                                inferInstAppArgsFromSchemeRefsExact
                                  (schemeBinderRefs sourceScheme)
                                  sourceDomain
                                  argSourceTy ->
                                -- The checked occurrence itself determines
                                -- the source binder arguments.  Construct at
                                -- that exact endpoint instead of asking a
                                -- still-bare graph variable to recover them.
                                constructArgumentAtExactEndpoint argSourceTy
                            | domainDependsOnSourceBinder sourceDomain ->
                                exposeArgumentEndpoint sourceDomain argRaw
                            | otherwise ->
                                constructArgumentAtExactEndpoint sourceDomain
                          _ ->
                            failApplication
                              [ "function source scheme does not expose an arrow body",
                                "function source=" ++ show funSourceTy
                              ]
                      constructAtArgument argComputation
            (parameterTy, resultTy) <-
              case edgeComputationTarget funComputation of
                TArrow domainTy codomainTy -> pure (domainTy, codomainTy)
                targetTy ->
                  failApplication
                    [ "function edge destination is not an arrow",
                      "destination=" ++ show targetTy
                    ]
            let funApplied =
                  instantiate funTermForTopology funComputation
                argApplied = instantiate argTermForTopology argComputation
            if
                applicationTypesAgree
                  parameterTy
                  (edgeComputationTarget argComputation)
              then pure ()
              else
                failApplication
                  [ "function domain does not equal argument edge destination",
                    "function source=" ++ show (edgeComputationSource funComputation),
                    "function instantiation=" ++ show (edgeComputationInstantiation funComputation),
                    "function domain=" ++ show parameterTy,
                    "argument source=" ++ show (edgeComputationSource argComputation),
                    "argument instantiation=" ++ show (edgeComputationInstantiation argComputation),
                    "argument destination=" ++ show (edgeComputationTarget argComputation)
                  ]
            let app = EApp funApplied argApplied
            checkedTy <-
              requireType
                ( unlines
                    [ "constructed application"
                    , "application node=" ++ show appNodeId
                    , "argument site=" ++ show argSite
                    , "argument annotation=" ++ show (annNode aAnn, annExprReferenceKey aAnn)
                    , "type-binder scope size=" ++ show (Map.size (envTypeBindings envForApplication))
                    , "application Γ scheme="
                        ++ diagnosticShown
                          (siScheme <$> applicationSchemeInfo)
                    , "function instantiation="
                        ++ diagnosticShown
                          (edgeComputationInstantiation funComputation)
                    , "argument instantiation="
                        ++ diagnosticShown
                          (edgeComputationInstantiation argComputation)
                    , "argument source="
                        ++ diagnosticElabType
                          (edgeComputationSource argComputation)
                    , "argument target="
                        ++ diagnosticElabType
                          (edgeComputationTarget argComputation)
                    ]
                )
                app
            if operationalEndpointTypesAgree checkedTy resultTy
              then pure ()
              else
                failApplication
                  [ "type checker disagrees with the constructed edge topology",
                    "checked type=" ++ diagnosticElabType checkedTy,
                    "function codomain=" ++ diagnosticElabType resultTy
                  ]
            completionBinders <-
              if isJust (envApplicationSourceOccurrence env)
                then
                  orderConstructionGammaBinders
                    "application source occurrence"
                    applicationBinders
                    checkedTy
                else
                  constructionGammaCompletionBinders
                    applicationBinders
                    app
                    checkedTy
            let completedApplication =
                  foldr
                    (\(ref, mbBound) body -> ETyAbsRef ref mbBound body)
                    app
                    completionBinders
                expectedApplicationType =
                  foldr
                    (\(ref, mbBound) body -> TForallRef ref mbBound body)
                    checkedTy
                    completionBinders
            completedType <-
              case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom env) completedApplication of
                Right ty -> pure ty
                Left err ->
                  failApplication
                    [ "completed Figure 15.3.5 application is not typable in its outer environment",
                      "application Γ scheme=" ++ show (siScheme <$> applicationSchemeInfo),
                      "completed term=" ++ show completedApplication,
                      "typecheck=" ++ show err
                    ]
            if applicationTypesAgree completedType expectedApplicationType
              then pure ()
              else
                failApplication
                  [ "completed Figure 15.3.5 application does not have its prepared scheme"
                  , "checked type=" ++ show completedType
                  , "prepared type=" ++ show expectedApplicationType
                  ]
            let sourceCompletionEndpoint = do
                  guard
                    ( retainedLocalBoundArgumentForResult
                        || isJust directLocalArgumentTopology
                    )
                  sourceFunctionSchemeInfo <- functionSourceSchemeInfo
                  sourceSchemeResultAt
                    edgeTypeEnv
                    argSourceTy
                    applicationConstructionResultTy
                    sourceFunctionSchemeInfo
                consumeCompletedGammaAtBounds targetTy =
                  go completionBinders completedApplication completedType
                  where
                    go remainingBinders termAtGamma tyAtGamma
                      | applicationTypesAgree tyAtGamma targetTy =
                          Just (termAtGamma, tyAtGamma, remainingBinders)
                    go
                      ((expectedRef, Just expectedBound) : remainingBinders)
                      termAtGamma
                      tyAtGamma =
                        case tyAtGamma of
                          TForallRef actualRef (Just actualBound) _
                            | typeBinderRefsSameIdentity expectedRef actualRef
                            , alphaEqType
                                (tyToElab expectedBound)
                                (tyToElab actualBound) -> do
                                nextTy <-
                                  either
                                    (const Nothing)
                                    Just
                                    ( TypeCheck.checkInstantiation
                                        (typeCheckEnvFrom env)
                                        tyAtGamma
                                        InstElim
                                    )
                                go
                                  remainingBinders
                                  (ETyInst termAtGamma InstElim)
                                  nextTy
                          _ -> Nothing
                    go _ _ _ = Nothing
                sourceCompletedApplication =
                  sourceCompletionEndpoint
                    >>= consumeCompletedGammaAtBounds
            ( completedApplicationForResult
              , completedTypeForResult
              , emittedCompletionBinders
              ) <-
                case sourceCompletedApplication of
                  Nothing ->
                    pure
                      ( completedApplication
                      , completedType
                      , completionBinders
                      )
                  Just constructed@(termAtResult, tyAtResult, _) ->
                    case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom env) termAtResult of
                      Right checkedResultTy
                        | applicationTypesAgree checkedResultTy tyAtResult ->
                            pure constructed
                      checked ->
                        failApplication
                          [ "source-directed Gamma elimination is not typable"
                          , "source result endpoint="
                              ++ show sourceCompletionEndpoint
                          , "constructed term=" ++ show termAtResult
                          , "constructed type=" ++ show tyAtResult
                          , "typecheck=" ++ show checked
                          ]
            finalApplication <-
              case
                  ( applicationMediatorArity
                  , argSourceTy0
                  , emittedCompletionBinders
                  )
                of
                    (Just arity, muTy@(TMuRef muRef muBody), [])
                      | arity > 0
                      , let unfoldedTy = substTypeCaptureRef muRef muTy muBody
                      , applicationTypesAgree completedTypeForResult unfoldedTy -> do
                          let rolledApplication =
                                ERoll muTy completedApplicationForResult
                          case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom env) rolledApplication of
                            Right rolledTy
                              | applicationTypesAgree rolledTy muTy ->
                                  pure rolledApplication
                            checked ->
                              failApplication
                                [ "eta-mediated recursive application cannot be rolled to its occurrence source"
                                , "recursive source=" ++ show muTy
                                , "unfolded result=" ++ show completedTypeForResult
                                , "typecheck=" ++ show checked
                                ]
                    _ -> pure completedApplicationForResult
            finalApplicationType <-
              case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom env) finalApplication of
                Right ty -> pure ty
                Left err ->
                  failApplication
                    [ "final Figure 15.3.5 application is not typable"
                    , "application=" ++ show finalApplication
                    , "typecheck=" ++ show err
                    ]
            case applicationExactResultTy of
              Just exactResultTy
                | null emittedCompletionBinders
                , not
                    ( scopedEndpointTypesAgree
                        scopeContext
                        finalApplicationType
                        exactResultTy
                    ) ->
                    failApplication
                      [ "final application did not retain its exact construction result"
                      , "application=" ++ show finalApplication
                      , "final type=" ++ show finalApplicationType
                      , "exact result=" ++ show exactResultTy
                      ]
              _ -> pure ()
            (localBinderRoutes, sourceBinderAuthorities) <-
              case NonEmpty.nonEmpty applicationBinders of
                Nothing -> pure (IntMap.empty, IntMap.empty)
                Just _ ->
                  localGammaConstructionProvenance
                    ("application " ++ show applicationOwner)
                    applicationBinders
                    [ applicationAliases,
                      maybe
                        IntMap.empty
                        schemeInfoBinderRefSubst
                        applicationSchemeInfo
                    ]
                    applicationSourceBinderRefs
            let binderWasEmitted (ref, _) =
                  any
                    (typeBinderRefsSameIdentity ref . fst)
                    emittedCompletionBinders
                consumedBinders =
                  filter (not . binderWasEmitted) applicationBinders
                construction =
                  case
                      ( NonEmpty.nonEmpty emittedCompletionBinders
                      , NonEmpty.nonEmpty applicationBinders
                      )
                    of
                      (Just emittedBinders, _) ->
                        LocalGammaEmitted emittedBinders consumedBinders
                      (Nothing, Just preparedBinders) ->
                        LocalGammaConsumed preparedBinders
                      (Nothing, Nothing) ->
                        LocalGammaAmbient
            ( directApplicationSourceEdgeIds
              , directApplicationGammaClaims
              , directApplicationAmbientGammaClaims
              ) <-
                buildDirectApplicationGammaClaims
                  scopeContext
                  applicationOwner
                  applicationAnn
                  (grRequiredGammaBinders edgeLocalRequirements)
                  (localGammaConstructionBinders construction)
                  localBinderRoutes
                  applicationAuthorityAliases
                  (envTypeBindings env)
                  (envFreeTypeBinderRefs env)
            let publishLocalCertificate =
                  not (null applicationBinders)
                    || not (null directApplicationAmbientGammaClaims)
                applicationUsedAmbientBinderRefs =
                  ownerFinalAmbientBinderRefs
                    (typeCheckEnvFrom env)
                    (map fst applicationBinders)
                    finalApplication
                    finalApplicationType
                localCertificate =
                  [ LocalGammaConstructionCertificate
                      { lgccOwner = applicationOwner,
                        lgccConstructedType = finalApplicationType,
                        lgccConstruction = construction,
                        lgccDirectApplicationSourceEdgeIds =
                          directApplicationSourceEdgeIds,
                        lgccDirectApplicationGammaClaims =
                          directApplicationGammaClaims,
                        lgccDirectApplicationAmbientGammaClaims =
                          directApplicationAmbientGammaClaims,
                        lgccAmbientDeclarationAuthorities =
                          [ AmbientGammaAuthority
                              (daagcAmbientRef claim)
                              (daagcAmbientBound claim)
                          | claim <-
                              directApplicationAmbientGammaClaims
                          ],
                        lgccLocalBinderRoutes = localBinderRoutes,
                        lgccSourceBinderAuthorities = sourceBinderAuthorities,
                        lgccUsedAmbientBinderRefs =
                          applicationUsedAmbientBinderRefs
                      }
                  | publishLocalCertificate
                  ]
            let zeroLocalFinalConstruction =
                  case applicationBinders of
                    [] ->
                      Just
                        OwnerFinalConstruction
                          { ofcOwner = applicationOwner,
                            ofcConstructedType = finalApplicationType,
                            ofcLocallyEmittedBinderRefs = [],
                            ofcLocalBinderRoutes = IntMap.empty,
                            ofcUsedAmbientBinderRefs =
                              applicationUsedAmbientBinderRefs,
                            ofcBodyConsumerBoundRefinements = []
                          }
                    _ -> Nothing
            pure
              ElaboratedTerm
                { elaboratedTerm = finalApplication,
                  -- A zero-local application still owns a positive checked
                  -- result construction.  Publish that exact source owner so
                  -- an enclosing application can refine a provisional
                  -- result route without inventing an empty local Gamma
                  -- certificate.
                  elaboratedOwnerFinalConstruction =
                    zeroLocalFinalConstruction,
                  elaboratedLocalGammaConstructionCertificates =
                    childLocalGammaCertificates ++ localCertificate,
                  elaboratedCompilerExactResultBoundCertificates =
                      childCompilerExactResultBoundCertificates
                }
       in ElabOut
            { elabDetailed = f,
              elabStripped = \env -> elaboratedTerm <$> f env
            }
    ALetF v mbBinderDetails schemeGenId schemeRootId expVarId rhsScopeGen (rhsAnn, rhsOut) (bodyAnn, bodyOut) trivialRoot ->
      let binderKey = annBinderKey mbBinderDetails
          binderDetails = resolvedTermBinderDetails v mbBinderDetails
          currentLetAnn =
            ALet
              v
              mbBinderDetails
              schemeGenId
              schemeRootId
              expVarId
              rhsScopeGen
              rhsAnn
              bodyAnn
              trivialRoot
          letEdgeArtifacts = acEdgeArtifacts annotationContext
          prepareLetConstructionGamma env = do
            letBoundary <- directLetBoundaryEdge bodyAnn
            letScope <-
              scopeRootForBoundary scopeContext letBoundary trivialRoot
            let letOwner =
                  LocalGammaOwner
                    { lgoConstructor = LocalLetGamma,
                      lgoBoundaryEdge = letBoundary,
                      lgoTermNode = trivialRoot,
                      lgoScope = letScope
                    }
            sourceBinderRefs <-
              constructionSourceBinderRefs
                (scopeTypeBinderIdentityRepresentative scopeContext)
                env
                (acSourceBinderRefs annotationContext)
            requirementEdges <-
              ownedRequirementEdgesForOwner
                scopeContext
                letEdgeArtifacts
                (algExactProducerTypes algebraContext)
                letOwner
                currentLetAnn
            requirements0 <-
              generalizationRequirementsForRootEdges
                (scopeTypeBinderIdentityRepresentative scopeContext)
                (scCanonical scopeContext)
                (scGaParents scopeContext)
                (scPresolutionView scopeContext)
                letEdgeArtifacts
                sourceBinderRefs
                (algSubtermGeneralizations algebraContext)
                requirementEdges
            let insertAmbientRef ref refs
                  | any (typeBinderRefsSameIdentity ref) refs = refs
                  | otherwise = ref : refs
                requirements =
                  requirements0
                    { grAmbientBinderRefs =
                        foldr
                          insertAmbientRef
                          (grAmbientBinderRefs requirements0)
                          (envGeneralizationAmbientTypeBinderRefs env)
                    }
            schemeInfo <-
              case grRequiredGammaBinders requirements of
                [] -> pure Nothing
                _ -> do
                  let letTarget =
                        generalizeTargetNode
                          (scPresolutionView scopeContext)
                          (scCanonical scopeContext trivialRoot)
                  (schemeRaw, substRaw) <-
                    case
                        scGeneralizeAtWithRequirements scopeContext
                          requirements
                          (Just (scGaParents scopeContext))
                          letScope
                          letTarget
                      of
                        Right result -> pure result
                        Left cause ->
                          Left
                            ( ValidationFailed
                                [ "let construction Gamma failed before term construction"
                                , "  binding: " ++ v
                                , "  let scope: " ++ show letScope
                                , "  let target: " ++ show letTarget
                                , "  requirements: " ++ show requirements
                                , "  cause: " ++ show cause
                                ]
                            )
                  generalizedSchemeInfo <-
                    generalizeSchemeInfoAgainstConstructionEnv
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      sourceBinderRefs
                      env
                      (schemeInfoFromRefSubst schemeRaw substRaw)
                  pure (Just generalizedSchemeInfo)
            (binders, aliases) <-
              case schemeInfo of
                Nothing -> pure ([], IntMap.empty)
                Just info -> do
                  gammaPlan <-
                    constructionGammaBinders
                      RequiredGammaOnly
                      scopeContext
                      letOwner
                      Nothing
                      Nothing
                      env
                      requirements
                      info
                  gammaAliases <-
                    constructionGammaAliases
                      ( gaConstructionRouteNodes
                          (scCanonical scopeContext)
                          (scGaParents scopeContext)
                      )
                      sourceBinderRefs
                      requirements
                      gammaPlan
                      info
                  pure (cgpBinders gammaPlan, cgaRoutingAliases gammaAliases)
            let letSchemeSubst =
                  case schemeInfo of
                    Nothing -> IntMap.empty
                    Just info ->
                      schemeInfoBinderRefSubst info
                envForLet =
                  case schemeInfo of
                    Nothing -> env
                    Just _ ->
                      extendEnvTypeScopeWithAliases
                        (IntMap.union aliases letSchemeSubst)
                        binders
                        env
            pure
              ( letOwner,
                letScope,
                requirements,
                schemeInfo,
                binders,
                aliases,
                sourceBinderRefs,
                envForLet
              )
          withLetConstructionGammaDetailed env build = do
            ( letOwner,
              letScope,
              requirements,
              schemeInfo,
              binders,
              aliases,
              _,
              envForLet
              ) <-
              prepareLetConstructionGamma env
            elaboration <- build letScope requirements aliases envForLet
            let term = elaboratedTerm elaboration
            termTy <-
              case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom envForLet) term of
                Right ty -> pure ty
                Left err ->
                  Left
                    ( PhiInvariantError
                        ( unlines
                            [ "ALetF: term is not typable under its prepared construction Γ",
                              "binding=" ++ v,
                              "scope=" ++ show letScope,
                              "Γ scheme=" ++ show (siScheme <$> schemeInfo),
                              "term=" ++ show term,
                              "typecheck=" ++ show err
                            ]
                        )
                    )
            completionBinders <-
              constructionGammaCompletionBinders binders term termTy
            let completed =
                  foldr
                    (\(ref, mbBound) body -> ETyAbsRef ref mbBound body)
                    term
                    completionBinders
                expectedTy =
                  foldr
                    (\(ref, mbBound) body -> TForallRef ref mbBound body)
                    termTy
                    completionBinders
            completedTy <-
              case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom env) completed of
                Right ty -> pure ty
                Left err ->
                  Left
                    ( PhiInvariantError
                        ( unlines
                            [ "ALetF: completed construction Γ is not typable in its outer environment",
                              "binding=" ++ v,
                              "scope=" ++ show letScope,
                              "Γ scheme=" ++ show (siScheme <$> schemeInfo),
                              "typecheck=" ++ show err
                            ]
                        )
                    )
            if scopedTypesAgree scopeContext completedTy expectedTy
              then do
                localBinderRoutes <-
                  localGammaConstructionRoutes
                    ("let " ++ show letOwner)
                    completionBinders
                    [ aliases
                    , maybe
                        IntMap.empty
                        schemeInfoBinderRefSubst
                        schemeInfo
                    ]
                let letOwnerFinalConstruction =
                      OwnerFinalConstruction
                        { ofcOwner = letOwner
                        , ofcConstructedType = completedTy
                        , ofcLocallyEmittedBinderRefs = map fst completionBinders
                        , ofcLocalBinderRoutes = localBinderRoutes
                        , ofcUsedAmbientBinderRefs =
                            ownerFinalAmbientBinderRefs
                              (typeCheckEnvFrom env)
                              (map fst completionBinders)
                              completed
                              completedTy
                        , ofcBodyConsumerBoundRefinements = []
                        }
                    ownerFinalConstruction =
                      case
                          ( completionBinders
                          , elaboratedOwnerFinalConstruction elaboration
                          )
                        of
                        ([], Just childCertificate)
                          | not
                              ( null
                                  (ofcLocallyEmittedBinderRefs childCertificate)
                              )
                          , scopedTypesAgree
                              scopeContext
                              (ofcConstructedType childCertificate)
                              completedTy ->
                              -- A transparent let that emits no Gamma of its
                              -- own must not erase the exact child constructor
                              -- that emitted the result binder.  Preserve that
                              -- owner certificate through the wrapper so root
                              -- closure validates the binder at its real
                              -- lambda/application owner.
                              childCertificate
                        _ -> letOwnerFinalConstruction
                pure
                  elaboration
                    { elaboratedTerm = completed
                    , elaboratedOwnerFinalConstruction =
                        Just ownerFinalConstruction
                    }
              else
                Left
                  ( PhiInvariantError
                      ( unlines
                          [ "ALetF: completed construction Γ has the wrong type",
                            "binding=" ++ v,
                            "scope=" ++ show letScope,
                            "checked type=" ++ show completedTy,
                            "expected type=" ++ show expectedTy
                          ]
                      )
                  )
          withLetConstructionGamma env build =
            elaboratedTerm
              <$> withLetConstructionGammaDetailed
                env
                ( \letScope requirements aliases envForLet -> do
                    term <- build letScope requirements aliases envForLet
                    pure
                      ElaboratedTerm
                        { elaboratedTerm = term,
                          elaboratedOwnerFinalConstruction = Nothing,
                          elaboratedLocalGammaConstructionCertificates = [],
                          elaboratedCompilerExactResultBoundCertificates = []
                        }
                )
          elaborateLet letConstructionScope letConstructionRequirements constructionAliases env = do
            -- The let publishes Typ(rhs), not the union of every Gamma below
            -- the RHS.  Nested lambdas and applications can share this let's
            -- GenRef while still owning distinct local construction Gammas;
            -- their requirements are consumed at those exact constructors.
            -- Scanning all descendant edges here would steal a child binder
            -- and incorrectly publish it in the let scheme.
            let allSubtermPackets =
                  algSubtermGeneralizations algebraContext
                rhsSubtermOwnership =
                  subtermResultOwnershipFor rhsAnn allSubtermPackets
                ownedSubtermPackets =
                  case rhsSubtermOwnership of
                    Just ownership
                      | subtermResultOwnershipHasTransparentPath ownership ->
                          subtermGeneralizationsOwnedBy rhsAnn allSubtermPackets
                    _ -> Map.empty
                -- The result lambda installs an enclosing/topology consumer
                -- while constructing its own Gamma.  Its enclosing let must
                -- publish the completed lambda scheme, not attempt to place
                -- that same packet at a second, absent let-scheme consumer.
                -- Root- and packet-owned consumers still require the ordinary
                -- placement below.
                letSchemePlacementPackets =
                  case rhsSubtermOwnership of
                    Just ownership
                      | subtermResultOwnershipConsumerClosedLocally ownership ->
                          Map.empty
                    _ -> ownedSubtermPackets
                requiredConsumerIdentities =
                  map
                    (typeBinderIdentityFromNode . rgbExteriorNode)
                    (grRequiredGammaBinders letConstructionRequirements)
                letRequiresConstructionGamma =
                  not (null (grRequiredGammaBinders letConstructionRequirements))
                letRequiresScopedGeneralization =
                  letRequiresConstructionGamma
                    || not
                      (null (grAmbientBinderRefs letConstructionRequirements))
                generalizeAtLetConstructionBoundary nodeId =
                  let target =
                        generalizeTargetNode
                          (scPresolutionView scopeContext)
                          (canonical nodeId)
                   in scGeneralizeAtWithRequirements scopeContext
                        letConstructionRequirements
                        (Just (scGaParents scopeContext))
                        letConstructionScope
                        target
            exactConstructionAuthority <-
              case
                  [ (owner, packet, exactEdge)
                  | (owner, packet) <- Map.toList ownedSubtermPackets
                  , Just exactEdge <- [subtermGeneralizationCompilerExactBoundary packet]
                  , maybe
                      True
                      (`elem` requiredConsumerIdentities)
                      (subtermGeneralizationConsumerIdentity packet)
                  ]
                of
                  [] -> pure Nothing
                  [(owner, packet, exactEdge)] ->
                    case compilerExactBoundarySubject exactEdge rhsAnn of
                      Just (subject, exactTy) ->
                        pure (Just (owner, packet, subject, exactTy))
                      Nothing ->
                        Left
                          ( ValidationFailed
                              [ "compiler-exact packet has no matching boundary on its let RHS path"
                              , "  binding: " ++ v
                              , "  owner: " ++ show owner
                              , "  exact edge: " ++ show exactEdge
                              ]
                          )
                  packets ->
                    Left
                      ( ValidationFailed
                          [ "one let construction Gamma selects multiple compiler-exact packets"
                          , "  binding: " ++ v
                          , "  consumers: " ++ show requiredConsumerIdentities
                          , "  packet schemes: "
                              ++ show
                                [ siScheme (subtermGeneralizationSchemeInfo packet)
                                | (_, packet, _) <- packets
                                ]
                          ]
                      )
            let debugGeneralize = traceGeneralize (algTraceConfig algebraContext)
                splitMediatorArrows arity ty
                  | arity <= (0 :: Int) = Just ([], ty)
                  | otherwise =
                      case ty of
                        TArrow domain codomain -> do
                          (domains, resultTy) <-
                            splitMediatorArrows (arity - 1) codomain
                          pure (domain : domains, resultTy)
                        _ -> Nothing
                -- A source-proven eta mediator and its prepared eta-parameter
                -- packet jointly determine the complete let contract.  In
                -- particular, if the packet constructs @forall a b. a -> b@,
                -- the enclosing mediator constructs
                -- @forall a b. (a -> b) -> a -> b@.  Select that contract
                -- before asking root generalization to rediscover the nested
                -- lambda's locally owned binders.
                preparedTransparentMediatorConstruction = do
                  (rootDetails, etaParams) <-
                    case stripAnnExpr rhsAnn of
                      ALam _rootName details _ _ mediatorBody _ _ -> do
                        (params, _) <-
                          transparentMediatorSignatureFor
                            (annBinderKey details)
                            mediatorBody
                        case params of
                          [] -> Nothing
                          _ : _ -> Just (details, params)
                      _ -> Nothing
                  packet <-
                    case
                      [ candidate
                      | (details, _) <- etaParams
                      , Just candidate <-
                          [ Map.lookup
                              (idDetailsIdentityKey details)
                              (algSubtermGeneralizations algebraContext)
                          ]
                      ]
                    of
                      [candidate] -> Just candidate
                      _ -> Nothing
                  let operatedInfo =
                        subtermGeneralizationOperatedSchemeInfo packet
                      (operatedBinders, rootParamTy) =
                        splitForallsRefs (schemeToType (siScheme operatedInfo))
                  (etaParamTys, resultTy) <-
                    splitMediatorArrows (length etaParams) rootParamTy
                  let mediatorScheme =
                        mkElabSchemeWithRefs
                          operatedBinders
                          ( TArrow
                              rootParamTy
                              (foldr TArrow resultTy etaParamTys)
                          )
                      mediatorInfo =
                        schemeInfoFromRefSubst
                          mediatorScheme
                          (schemeInfoBinderRefSubst operatedInfo)
                  pure
                    ( rootDetails
                    , etaParams
                    , rootParamTy
                    , etaParamTys
                    , mediatorInfo
                    )
                transparentMediatorSourceKey annExpr =
                  case annExprReferenceKey annExpr of
                    Just sourceKey -> Just sourceKey
                    Nothing ->
                      case stripAnnExpr annExpr of
                        AApp funAnn argAnn _ _ _
                          | maybe False (`isTransparentMediatorKey` env) (annExprReferenceKey funAnn) ->
                              transparentMediatorSourceKey argAnn
                        _ -> Nothing
                peelTransparentMediatorSubject annExpr =
                  case stripAnnExpr annExpr of
                    AApp funAnn argAnn _ _ _
                      | maybe False (`isTransparentMediatorKey` env) (annExprReferenceKey funAnn) ->
                          peelTransparentMediatorSubject argAnn
                    _ -> annExpr
                aliasSourceKey = transparentMediatorSourceKey rhsAnn
                aliasSourceSchemeInfo = aliasSourceKey >>= (`lookupSchemeInfoForKey` env)
                containsRecursiveSelfAppToParam selfKey paramKey annExpr =
                  case annExpr of
                    AResolvedVar _ _ _ -> False
                    ALit _ _ -> False
                    AApp fun arg _ _ _ ->
                      case directAnnReferenceKey fun of
                        Just recurKey
                          | recurKey == selfKey -> annContainsReference paramKey arg
                        _ ->
                          containsRecursiveSelfAppToParam selfKey paramKey fun
                            || containsRecursiveSelfAppToParam selfKey paramKey arg
                    ALam _boundName mbDetails _ _ body _ _
                      | let boundKey = annBinderKey mbDetails,
                        boundKey == selfKey || boundKey == paramKey -> False
                      | otherwise -> containsRecursiveSelfAppToParam selfKey paramKey body
                    ALet _boundName mbDetails _ _ _ _ rhs body _
                      | let boundKey = annBinderKey mbDetails,
                        boundKey == selfKey || boundKey == paramKey ->
                          containsRecursiveSelfAppToParam selfKey paramKey rhs
                      | otherwise ->
                          containsRecursiveSelfAppToParam selfKey paramKey rhs
                            || containsRecursiveSelfAppToParam selfKey paramKey body
                    AAnn inner _ _ -> containsRecursiveSelfAppToParam selfKey paramKey inner
                    AExactAnn inner _ _ _ -> containsRecursiveSelfAppToParam selfKey paramKey inner
                    ALetScope inner _ _ -> containsRecursiveSelfAppToParam selfKey paramKey inner
                    AUnfold inner _ _ -> containsRecursiveSelfAppToParam selfKey paramKey inner
                hasNestedRecursiveSelfAppToParam selfKey paramKey annExpr =
                  case annExpr of
                    AResolvedVar _ _ _ -> False
                    ALit _ _ -> False
                    AApp fun arg _ _ _ ->
                      containsRecursiveSelfAppToParam selfKey paramKey arg
                        || hasNestedRecursiveSelfAppToParam selfKey paramKey fun
                        || hasNestedRecursiveSelfAppToParam selfKey paramKey arg
                    ALam _boundName mbDetails _ _ body _ _
                      | let boundKey = annBinderKey mbDetails,
                        boundKey == selfKey || boundKey == paramKey -> False
                      | otherwise -> hasNestedRecursiveSelfAppToParam selfKey paramKey body
                    ALet _boundName mbDetails _ _ _ _ rhs body _
                      | let boundKey = annBinderKey mbDetails,
                        boundKey == selfKey || boundKey == paramKey ->
                          hasNestedRecursiveSelfAppToParam selfKey paramKey rhs
                      | otherwise ->
                          hasNestedRecursiveSelfAppToParam selfKey paramKey rhs
                            || hasNestedRecursiveSelfAppToParam selfKey paramKey body
                    AAnn inner _ _ -> hasNestedRecursiveSelfAppToParam selfKey paramKey inner
                    AExactAnn inner _ _ _ -> hasNestedRecursiveSelfAppToParam selfKey paramKey inner
                    ALetScope inner _ _ -> hasNestedRecursiveSelfAppToParam selfKey paramKey inner
                    AUnfold inner _ _ -> hasNestedRecursiveSelfAppToParam selfKey paramKey inner
                recursiveArrowCarrier ownerNode extraUsedNames codTy =
                  let usedNames = Set.union extraUsedNames (freeTypeVarsType codTy)
                      pickFreshMuName idx =
                        let candidate =
                              if idx == (0 :: Int)
                                then "a"
                                else "a" ++ show idx
                         in if Set.member candidate usedNames
                              then pickFreshMuName (idx + 1)
                              else candidate
                      muName = pickFreshMuName 0
                      -- The inferred fixed point belongs to this raw syntax
                      -- occurrence.  Deriving its binder identity from that
                      -- owner makes sibling constructions distinct without a
                      -- locally threaded freshness supply that cannot account
                      -- for the rest of the elaboration pipeline.
                      muRef =
                        typeBinderRefFromIdentity
                          (typeBinderIdentityFromNode ownerNode)
                          muName
                      EnvFreeTypeBinderRefs ambientRefs =
                        envFreeTypeBinderRefs env
                      -- Reification can expose the result node of the
                      -- recursive call as a free graph placeholder.  The
                      -- self-application test above is the construction
                      -- certificate for the recursive equation; it permits
                      -- those otherwise-unowned result positions to refer to
                      -- this carrier's μ binder.  Exact identities already
                      -- owned by the ambient paper Γ remain untouched.
                      unownedCodomainRefs =
                        schemeClosureFreeRefs
                          (ambientSchemeClosureAuthority ambientRefs)
                          (schemeFromType codTy)
                      ownedCodTy =
                        foldl'
                          ( \ty ref ->
                              substTypeCaptureRef ref (TVarRef muRef) ty
                          )
                          codTy
                          unownedCodomainRefs
                   in TMuRef muRef (TArrow (TVarRef muRef) ownedCodTy)
                recursiveFixedPointCarrier ownerNode extraUsedNames =
                  let pickFreshMuName idx =
                        let candidate =
                              if idx == (0 :: Int)
                                then "a"
                                else "a" ++ show idx
                         in if Set.member candidate extraUsedNames
                              then pickFreshMuName (idx + 1)
                              else candidate
                      muName = pickFreshMuName 0
                      muRef =
                        typeBinderRefFromIdentity
                          (typeBinderIdentityFromNode ownerNode)
                          muName
                   in TMuRef muRef (TArrow (TVarRef muRef) (TVarRef muRef))
                -- The returned-helper equation is still open when its
                -- result reifies to a bare graph variable.  That variable
                -- is the identity-preserving form of the old bottom
                -- placeholder, not a solved codomain.  Close the equation
                -- at its structural owner instead of publishing a carrier
                -- whose roll body cannot inhabit its unfolding.
                openNestedRecursiveResult resultTy =
                  case resultTy of
                    TBottom -> True
                    TVarRef ref -> isInternalTypeBinderRef ref
                    _ -> False
                inferredRecursiveCarrierTyFor selfKey extraUsedNames annExpr =
                  case annExpr of
                    ALam _lamParam mbDetails _ _ lamBody _ _ ->
                      let paramKey = annBinderKey mbDetails
                          ownerNode = annNode annExpr
                       in if containsRecursiveSelfAppToParam selfKey paramKey lamBody
                            then do
                              resultTy <-
                                either
                                  (const Nothing)
                                  Just
                                  ( reifyNodeTypePreferringBound
                                      scopeContext
                                      (annNode lamBody)
                                  )
                              pure $
                                if openNestedRecursiveResult resultTy
                                    && hasNestedRecursiveSelfAppToParam
                                      selfKey
                                      paramKey
                                      lamBody
                                  then
                                    recursiveFixedPointCarrier
                                      ownerNode
                                      extraUsedNames
                                  else
                                    recursiveArrowCarrier
                                      ownerNode
                                      extraUsedNames
                                      resultTy
                            else Nothing
                    AAnn inner _ _ ->
                      inferredRecursiveCarrierTyFor selfKey extraUsedNames inner
                    AExactAnn inner _ _ _ ->
                      inferredRecursiveCarrierTyFor selfKey extraUsedNames inner
                    ALetScope inner _ _ ->
                      inferredRecursiveCarrierTyFor selfKey extraUsedNames inner
                    AUnfold inner _ _ ->
                      inferredRecursiveCarrierTyFor selfKey extraUsedNames inner
                    _ -> Nothing
                recursiveCarrierTyFor selfKey extraUsedNames annExpr =
                  let inferredCarrier =
                        inferredRecursiveCarrierTyFor
                          selfKey
                          extraUsedNames
                          annExpr
                   in case
                        reifyNodeTypePreferringBound
                          scopeContext
                          (annNode annExpr)
                      of
                        Right carrierTy
                          | hasContractiveRecursiveWitness carrierTy ->
                              case inferredCarrier of
                                Just inferredTy
                                  | shouldPreferInferredRecursiveCarrier
                                      carrierTy
                                      inferredTy ->
                                      Just inferredTy
                                _ -> Just carrierTy
                        _ -> inferredCarrier
                shouldPreferInferredRecursiveCarrier carrierTy inferredTy =
                  (isBottomRecursiveCarrier carrierTy && isFixedPointRecursiveCarrier inferredTy)
                    || hasInternalRecursiveCodomain carrierTy
                      && not (hasInternalRecursiveCodomain inferredTy)
                isBottomRecursiveCarrier carrierTy =
                  case carrierTy of
                    TMuRef _ (TArrow _ TBottom) -> True
                    _ -> False
                isFixedPointRecursiveCarrier carrierTy =
                  case carrierTy of
                    TMuRef muRef (TArrow (TVarRef domRef) (TVarRef codRef)) ->
                      typeBinderRefsSameIdentity muRef domRef
                        && typeBinderRefsSameIdentity muRef codRef
                    _ -> False
                hasInternalRecursiveCodomain carrierTy =
                  case carrierTy of
                    TMuRef muRef (TArrow (TVarRef domRef) codTy) ->
                      typeBinderRefsSameIdentity muRef domRef
                        && internalOnlyType codTy
                    _ -> False
                internalOnlyType ty =
                  case ty of
                    TVarRef ref -> isInternalTypeBinderRef ref
                    TArrow dom cod -> internalOnlyType dom && internalOnlyType cod
                    TConWithIdentity _ _ args ->
                      not (null args) && all internalOnlyType args
                    TForallRef _ mb body ->
                      maybe True internalOnlyBound mb && internalOnlyType body
                    TMuRef _ body -> internalOnlyType body
                    _ -> False
                internalOnlyBound bound =
                  case bound of
                    TArrow dom cod -> internalOnlyType dom && internalOnlyType cod
                    TConWithIdentity _ _ args ->
                      not (null args) && all internalOnlyType args
                    TForallRef _ mb body ->
                      maybe True internalOnlyBound mb && internalOnlyType body
                    TMuRef _ body -> internalOnlyType body
                    _ -> False
                returnedRecursiveHelperArrowTypes annExpr =
                  case annExpr of
                    ALam _ _ _ _ lamBody _ _ ->
                      returnedRecursiveHelperSignatureTypes lamBody
                    AAnn inner _ _ ->
                      returnedRecursiveHelperArrowTypes inner
                    AExactAnn inner _ _ _ ->
                      returnedRecursiveHelperArrowTypes inner
                    ALetScope inner _ _ ->
                      returnedRecursiveHelperArrowTypes inner
                    AUnfold inner _ _ ->
                      returnedRecursiveHelperArrowTypes inner
                    _ -> ([], [])
                returnedRecursiveHelperSignatureTypes lamBody =
                  case lamBody of
                    ALet _helperName mbHelperDetails _ _ _ _ helperRhs@(ALam _helperParam mbParamDetails _ _ _ _ _) helperBody _
                      | let helperKey = annBinderKey mbHelperDetails,
                        annExprReferenceKey helperBody == Just helperKey ->
                          let helperParamKey = annBinderKey mbParamDetails
                           in returnedHelperCandidates
                                helperKey
                                (Just (helperKey, helperParamKey))
                                helperRhs
                    ALet _helperName mbHelperDetails _ _ _ _ helperRhs helperBody _
                      | let helperKey = annBinderKey mbHelperDetails,
                        annExprReferenceKey helperBody == Just helperKey ->
                          returnedHelperCandidates
                            helperKey
                            Nothing
                            helperRhs
                    AAnn inner _ _ ->
                      returnedRecursiveHelperSignatureTypes inner
                    AExactAnn inner _ _ _ ->
                      returnedRecursiveHelperSignatureTypes inner
                    ALetScope inner _ _ ->
                      returnedRecursiveHelperSignatureTypes inner
                    AUnfold inner _ _ ->
                      returnedRecursiveHelperSignatureTypes inner
                    _ -> ([], [])
                returnedHelperCandidates helperKey mbHelperKeys helperRhs =
                  case
                    recursiveCarrierTyFor
                      helperKey
                      Set.empty
                      helperRhs
                  of
                    Nothing -> ([], [annNode helperRhs])
                    Just helperTy ->
                      let mbHelper =
                            (\(helperName, helperParam) ->
                                (helperName, helperParam, helperTy)
                            )
                              <$> mbHelperKeys
                          (outerDomTys, unresolvedCallArguments) =
                            recursiveCallArgumentTypesFor
                              binderKey
                              mbHelper
                              helperRhs
                       in case outerDomTys of
                            [] -> ([], unresolvedCallArguments)
                            _ ->
                              ( map (`TArrow` helperTy) outerDomTys
                              , unresolvedCallArguments
                              )
                recursiveCallArgumentTypesFor selfKey mbHelper annExpr =
                  case annExpr of
                    AApp fun arg _ _ _
                      | Just recurKey <- directAnnReferenceKey fun,
                        recurKey == selfKey ->
                          appendRecursiveCallEvidence
                            (recursiveCallArgumentEvidence mbHelper arg)
                            (recursiveCallArgumentTypesFor selfKey mbHelper arg)
                    AApp fun arg _ _ _ ->
                      appendRecursiveCallEvidence
                        (recursiveCallArgumentTypesFor selfKey mbHelper fun)
                        (recursiveCallArgumentTypesFor selfKey mbHelper arg)
                    ALam _boundName mbDetails _ _ body _ _
                      | annBinderKey mbDetails == selfKey -> ([], [])
                      | otherwise ->
                          recursiveCallArgumentTypesFor selfKey mbHelper body
                    ALet _boundName mbDetails _ _ _ _ rhs body _
                      | annBinderKey mbDetails == selfKey ->
                          recursiveCallArgumentTypesFor selfKey mbHelper rhs
                      | otherwise ->
                          appendRecursiveCallEvidence
                            (recursiveCallArgumentTypesFor selfKey mbHelper rhs)
                            (recursiveCallArgumentTypesFor selfKey mbHelper body)
                    AAnn inner _ _ ->
                      recursiveCallArgumentTypesFor selfKey mbHelper inner
                    AExactAnn inner _ _ _ ->
                      recursiveCallArgumentTypesFor selfKey mbHelper inner
                    ALetScope inner _ _ ->
                      recursiveCallArgumentTypesFor selfKey mbHelper inner
                    AUnfold inner _ _ ->
                      recursiveCallArgumentTypesFor selfKey mbHelper inner
                    _ -> ([], [])
                recursiveCallArgumentEvidence mbHelper arg =
                  case
                    helperRecursiveSelfAppResultTy mbHelper arg
                      <|> either
                        (const Nothing)
                        Just
                        ( reifyNodeTypePreferringBound
                            scopeContext
                            (annNode arg)
                        )
                  of
                    Just argTy -> ([argTy], [])
                    Nothing -> ([], [annNode arg])
                appendRecursiveCallEvidence
                  (leftTypes, leftUnresolved)
                  (rightTypes, rightUnresolved) =
                    ( leftTypes ++ rightTypes
                    , leftUnresolved ++ rightUnresolved
                    )
                helperRecursiveSelfAppResultTy mbHelper argExpr =
                  case mbHelper of
                    Just (helperName, helperParam, helperTy)
                      | isRecursiveSelfAppToParam helperName helperParam argExpr ->
                          recursiveSelfAppResultTy helperTy
                    _ -> Nothing
                isRecursiveSelfAppToParam helperKey helperParamKey annExpr =
                  case annExpr of
                    AApp fun arg _ _ _
                      | Just recurKey <- directAnnReferenceKey fun,
                        recurKey == helperKey -> annContainsReference helperParamKey arg
                    AAnn inner _ _ -> isRecursiveSelfAppToParam helperKey helperParamKey inner
                    AExactAnn inner _ _ _ -> isRecursiveSelfAppToParam helperKey helperParamKey inner
                    ALetScope inner _ _ -> isRecursiveSelfAppToParam helperKey helperParamKey inner
                    AUnfold inner _ _ -> isRecursiveSelfAppToParam helperKey helperParamKey inner
                    _ -> False
                recursiveSelfAppResultTy helperTy =
                  case helperTy of
                    TForallRef _ _ bodyTy -> recursiveSelfAppResultTy bodyTy
                    TArrow _ codTy -> Just codTy
                    muTy@TMuRef {} ->
                      case unfoldMuOnce muTy of
                        Just (TArrow _ codTy) -> Just codTy
                        _ -> Nothing
                    _ -> Nothing
                mediatedMuSubject = peelTransparentMediatorSubject rhsAnn
                (returnedHelperCandidateTypes, unresolvedHelperNodes) =
                  returnedRecursiveHelperArrowTypes mediatedMuSubject
                recursiveCarrierCandidate =
                  recursiveCarrierTyFor
                    binderKey
                    Set.empty
                    mediatedMuSubject
                structuralRecursiveCandidateSelection =
                  if null unresolvedHelperNodes
                    then
                      selectStructuralRecursiveCandidate $
                        map
                          StructuralRecursiveCandidateFromHelper
                          returnedHelperCandidateTypes
                          ++ maybe
                            []
                            (pure . StructuralRecursiveCandidateFromDirectCarrier)
                            recursiveCarrierCandidate
                    else AmbiguousStructuralRecursiveCandidate
            letResultSourceScheme <- sourceSchemePairForExactLambdaParamNode algebraContext scopeContext (canonical trivialRoot)
            schemeRootSourceScheme <- sourceSchemePairForExactLambdaParamNode algebraContext scopeContext schemeRootId
            rhsOuterSourceScheme <- sourceSchemePairForOuterAnnotation env algebraContext scopeContext rhsAnn
            let authoritativeSourceSchemePair =
                  rhsOuterSourceScheme <|> letResultSourceScheme <|> schemeRootSourceScheme
                schemeRootRef =
                  typeBinderRefFromIdentity
                    (typeBinderIdentityFromNode schemeRootId)
                    ("t" ++ show (getNodeId schemeRootId))
                inheritedGammaBinding =
                  find
                    (\(ref, bound) ->
                        typeBinderRefsSameIdentity schemeRootRef ref
                          && bound /= TBottom
                    )
                    (Map.toList (envTypeBindings env))
                directRhsConstructionNode = annNode (stripAnnExpr rhsAnn)
                -- A direct identity producer preserves the exact type of its
                -- resolved argument.  Both halves of this certificate are
                -- source-owned before the independent RHS is elaborated: the
                -- lambda body refers to its own binder, and the argument
                -- identity selects one already checked lexical scheme.
                -- This is therefore a valid construction endpoint for the
                -- application result even though the enclosing let-body
                -- expectation belongs to a sibling occurrence and must be
                -- cleared.  No graph result shape or representative is used.
                -- A direct identity producer fixes its result to the exact
                -- type of its argument before the application graph is
                -- generalized.  Resolved variables carry that authority in
                -- the lexical environment; a desugared annotated identity
                -- lambda carries it in the source annotation edge.
                independentIdentityProducerArgumentType argAnn =
                  ( do
                      argumentKey <- annExprReferenceKey argAnn
                      argumentBinding <- lookupEnvBindingForKey argumentKey env
                      pure (ebSchemeType argumentBinding)
                  )
                    <|> case stripAnnExpr argAnn of
                      ALam _ mbParamDetails _ _ lamBody _ _ -> do
                        (mediatorDetails, _, annotationEdge, innerBodyAnn) <-
                          desugaredAnnLambdaInfo mbParamDetails lamBody
                        guard
                          ( annExprReferenceKey innerBodyAnn
                              == Just (annBinderKey mediatorDetails)
                          )
                        annotationTy <-
                          IntMap.lookup
                            (getEdgeId annotationEdge)
                            (algAnnotationExpectedTypesByEdge algebraContext)
                        pure (TArrow annotationTy annotationTy)
                      _ -> Nothing
                independentIdentityProducerHead funAnn =
                  identityWrapperAnn funAnn
                    || case transparentMediatorSpine env funAnn of
                      Just (DirectIdentityMediator, 0, Nothing) -> True
                      _ -> False
                independentRhsProducerEndpoint = do
                  (funAnn, argAnn) <-
                    case stripAnnExpr rhsAnn of
                      AApp fun arg _ _ _ -> Just (fun, arg)
                      _ -> Nothing
                  guard (independentIdentityProducerHead funAnn)
                  argumentTy <-
                    independentIdentityProducerArgumentType argAnn
                  pure
                    ( ExactConstructionExpectedTerm
                        argumentTy
                    )
                ownerMatchesDirectRhs owner =
                  lgoTermNode owner == directRhsConstructionNode
                directRhsOwnerFinalConstruction elaboration =
                  case
                      [ certificate
                      | certificate <- elaboratedLocalGammaConstructionCertificates elaboration
                      , ownerMatchesDirectRhs (lgccOwner certificate)
                      ]
                    of
                      [] ->
                        pure
                          ( case elaboratedOwnerFinalConstruction elaboration of
                              Just certificate
                                | ownerMatchesDirectRhs (ofcOwner certificate) ->
                                    Just certificate
                              _ -> Nothing
                          )
                      [certificate] ->
                        case localGammaEmittedBinders (lgccConstruction certificate) of
                          [] -> pure Nothing
                          emittedBinders ->
                            pure
                              ( Just
                                  OwnerFinalConstruction
                                    { ofcOwner = lgccOwner certificate
                                    , ofcConstructedType = lgccConstructedType certificate
                                    , ofcLocallyEmittedBinderRefs =
                                        map fst emittedBinders
                                    , ofcLocalBinderRoutes = lgccLocalBinderRoutes certificate
                                    , ofcUsedAmbientBinderRefs =
                                        lgccUsedAmbientBinderRefs certificate
                                    , ofcBodyConsumerBoundRefinements = []
                                    }
                              )
                      certificates ->
                        Left
                          ( ValidationFailed
                              [ "direct let RHS published multiple construction certificates"
                              , "  binding: " ++ v
                              , "  RHS node: " ++ show directRhsConstructionNode
                              , "  certificates: " ++ show certificates
                              ]
                          )
                rhsOwnsPostEnvConstruction =
                  case stripAnnExpr rhsAnn of
                    ALam {} -> True
                    AApp {} -> True
                    _ -> False
                prepareExactConstructionSchemeInfo =
                  case exactConstructionAuthority of
                    Nothing -> pure Nothing
                    Just (owner, packet, exactSubject, exactSourceType) -> do
                      exactType <-
                        either
                          (Left . InstantiationError)
                          Right
                          (resolvedSourceTypeToElabType exactSourceType)
                      exactInfoUnaligned <-
                        compilerExactOwnerSchemeInfo
                          scopeContext
                          owner
                          packet
                          exactSubject
                          exactType
                      exactInfo0 <-
                        alignSchemeInfoToConstructionGamma
                          (scopeTypeBinderIdentityRepresentative scopeContext)
                          (acSourceBinderRefs annotationContext)
                          env
                          exactInfoUnaligned
                      let ownedExactPackets =
                            Map.elems
                              ( subtermGeneralizationsOwnedBy
                                  exactSubject
                                  (algSubtermGeneralizations algebraContext)
                              )
                          ownedExactGammaEdges =
                            [ (edgeId, ownedPacket)
                            | edgeId <- annotationInstantiationEdges exactSubject
                            , ownedPacket <- ownedExactPackets
                            , subtermGeneralizationOwnsGammaForEdge edgeId ownedPacket
                            ]
                          aliasesForOwnedExactEdge (edgeId, ownedPacket) = do
                            requirements <-
                              generalizationRequirementsForRootEdges
                                (scopeTypeBinderIdentityRepresentative scopeContext)
                                (scCanonical scopeContext)
                                (scGaParents scopeContext)
                                (scPresolutionView scopeContext)
                                letEdgeArtifacts
                                (acSourceBinderRefs annotationContext)
                                (algSubtermGeneralizations algebraContext)
                                [ ( edgeId
                                  , Just
                                      ( schemeToType
                                          ( siScheme
                                              ( subtermGeneralizationOperatedSchemeInfo
                                                  ownedPacket
                                              )
                                          )
                                      )
                                  )
                                ]
                            cgaRoutingAliases
                              <$> constructionGammaAliases
                                ( gaConstructionRouteNodes
                                    (scCanonical scopeContext)
                                    (scGaParents scopeContext)
                                )
                                (acSourceBinderRefs annotationContext)
                                requirements
                                ConstructionGammaPlan
                                  { cgpBinders = []
                                  , cgpAmbientAliases = IntMap.empty
                                  }
                                (subtermGeneralizationSchemeInfo ownedPacket)
                          mergeAliasMaps aliases incoming =
                            foldM
                              (\current (nodeKey, outwardRef) ->
                                case IntMap.lookup nodeKey current of
                                  Nothing -> pure (IntMap.insert nodeKey outwardRef current)
                                  Just existing
                                    | typeBinderRefsSameIdentity existing outwardRef -> pure current
                                    | otherwise ->
                                        Left
                                          ( ValidationFailed
                                              [ "exact construction edges disagree on one Gamma alias"
                                              , "  graph node: " ++ show (NodeId nodeKey)
                                              , "  first ref: " ++ show existing
                                              , "  second ref: " ++ show outwardRef
                                              ]
                                          )
                              )
                              aliases
                              (IntMap.toList incoming)
                      exactAliasMaps <-
                        traverse aliasesForOwnedExactEdge ownedExactGammaEdges
                      exactAliases <- foldM mergeAliasMaps IntMap.empty exactAliasMaps
                      exactSubst <-
                        alignConstructionAliasesToScheme
                          (IntMap.union exactAliases constructionAliases)
                          exactInfo0
                      pure
                        ( Just
                            (schemeInfoFromRefSubst (siScheme exactInfo0) exactSubst)
                        )
            exactConstructionSchemeInfo <-
              prepareExactConstructionSchemeInfo
            ( rhsConstructionElaborationBeforeScheme
              , rhsOwnerFinalConstructionBeforeScheme
              ) <-
              if not (annContainsReference binderKey rhsAnn)
                  && rhsOwnsPostEnvConstruction
                  && isNothing rhsOuterSourceScheme
                  && isNothing inheritedGammaBinding
                  && isNothing exactConstructionSchemeInfo
                  && isNothing preparedTransparentMediatorConstruction
                then
                  -- A non-recursive let RHS is not the result occurrence of
                  -- the enclosing body.  Pre-elaboration discovers only the
                  -- RHS's own Figure 15.3.5 construction, so inheriting the
                  -- body's active consumer (or its expected result) would
                  -- specialize the RHS at a sibling endpoint. Keep lexical
                  -- bindings/Gamma in scope and retain only an exact endpoint
                  -- constructed by this RHS's own producer certificate.
                  case
                      elabDetailed
                        rhsOut
                        env
                          { envActiveSubtermConstruction = Nothing
                          , envExpectedTermEndpoint =
                              independentRhsProducerEndpoint
                          , envApplicationSourceOccurrence = Nothing
                          }
                    of
                    Right elaboration -> do
                      mbCertificate <-
                        directRhsOwnerFinalConstruction elaboration
                      case mbCertificate of
                        Nothing -> pure (Just elaboration, Nothing)
                        Just certificate -> do
                          (canonicalElaboration, canonicalCertificate) <-
                            canonicalizePublishedOwnerConstruction
                              env
                              certificate
                              elaboration
                          pure
                            ( Just canonicalElaboration
                            , Just canonicalCertificate
                            )
                    Left cause ->
                      Left
                        ( ValidationFailed
                            [ "non-recursive let RHS failed before scheme construction"
                            , "  binding: " ++ v
                            , "  RHS node: " ++ show directRhsConstructionNode
                            , "  cause: " ++ show cause
                            ]
                        )
                else pure (Nothing, Nothing)
            let rhsOwnerConstructionSchemeInfo =
                  ( \certificate ->
                      schemeInfoFromRefSubst
                        (schemeFromType (ofcConstructedType certificate))
                        (ofcLocalBinderRoutes certificate)
                  )
                    <$> rhsOwnerFinalConstructionBeforeScheme
                rhsConstructionTypeBeforeScheme = do
                  elaboration <- rhsConstructionElaborationBeforeScheme
                  either
                    (const Nothing)
                    Just
                    ( TypeCheck.typeCheckWithEnv
                        (typeCheckEnvFrom env)
                        (elaboratedTerm elaboration)
                    )
                rhsClosedConstructionSchemeInfo = do
                  rhsTy <- rhsConstructionTypeBeforeScheme
                  -- This seam is deliberately monomorphic: a closed checked
                  -- result (notably a recursive carrier or a discarded
                  -- recursive call result) is already the complete Typ(rhs)
                  -- construction.  A type with free refs still needs the
                  -- prepared generalization plan, and a leading forall still
                  -- needs its identity-bearing substitution routes.
                  guard (null (freeTypeVarRefsType rhsTy))
                  guard
                    ( null
                        (schemeBinderRefs (schemeFromType rhsTy))
                    )
                  pure
                    ( schemeInfoFromRefSubst
                        (schemeFromType rhsTy)
                        IntMap.empty
                    )
                withLetSchemeConstructionContext construction =
                  case construction of
                    Right result -> pure result
                    Left cause ->
                      Left
                        ( ValidationFailed
                            [ "let scheme construction has no closed authority"
                            , "  binding: " ++ v
                            , "  scheme root: " ++ show schemeRootId
                            , "  scoped generalization: "
                                ++ show letRequiresScopedGeneralization
                            , "  source scheme: "
                                ++ show (fst <$> authoritativeSourceSchemePair)
                            , "  alias source: " ++ show aliasSourceKey
                            , "  alias scheme: "
                                ++ show (siScheme <$> aliasSourceSchemeInfo)
                            , "  preconstructed RHS type: "
                                ++ show rhsConstructionTypeBeforeScheme
                            , "  cause: " ++ show cause
                            ]
                        )
            let structuralRecursiveSchemePair = do
                  guard (annContainsReference binderKey rhsAnn)
                  candidate <-
                    case structuralRecursiveCandidateSelection of
                      UniqueStructuralRecursiveCandidate uniqueCandidate ->
                        Just uniqueCandidate
                      NoStructuralRecursiveCandidate -> Nothing
                      AmbiguousStructuralRecursiveCandidate -> Nothing
                  let candidateTy = structuralRecursiveCandidateType candidate
                      candidateScheme = schemeFromType candidateTy
                      EnvFreeTypeBinderRefs ambientRefs =
                        envFreeTypeBinderRefs env
                  guard (hasContractiveRecursiveWitness candidateTy)
                  pure
                    ( validateSchemeClosure
                        ("structural recursive scheme for let " ++ v)
                        (ambientSchemeClosureAuthority ambientRefs)
                        candidateScheme
                    , IntMap.empty
                    )
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
            (scheme0Raw, subst0Raw, usesSourceSchemeDirectly) <-
              case inheritedGammaBinding of
                Just (_, gammaBound) ->
                  pure
                    ( schemeFromType gammaBound,
                      IntMap.empty,
                      False
                    )
                Nothing
                  | Just (sourceScheme, sourceSubst) <- rhsOuterSourceScheme ->
                      -- A source annotation on the RHS owns the recursive
                      -- binding ABI.  The surrounding construction Gamma is
                      -- still installed by 'withLetConstructionGamma', but it
                      -- must not replace a declared monomorphic scheme with a
                      -- graph-generalized contravariant one such as
                      -- @forall a >= Nat b >= Nat. a -> b -> Bool@.  No xMLF
                      -- computation can turn parameters of those abstract
                      -- types back into the declared @Nat@ arguments used by
                      -- the recursive body.
                      pure (sourceScheme, sourceSubst, True)
                Nothing
                  | Just exactInfo <- exactConstructionSchemeInfo ->
                      -- A compiler-exact RHS packet already owns the complete
                      -- result construction, including the routes for every
                      -- Gamma binder used by that endpoint.  Use that
                      -- construction as the intermediate scheme too: asking
                      -- the solved let root to rediscover it first can expose
                      -- an operated packet binder as a free variable before
                      -- the exact scheme is selected below.
                      pure
                        ( siScheme exactInfo
                        , schemeInfoBinderRefSubst exactInfo
                        , False
                        )
                Nothing
                  | Just constructedInfo <- rhsOwnerConstructionSchemeInfo ->
                      -- A non-recursive RHS constructor has already emitted
                      -- and checked its Figure 15.3.5 Lambda(Gamma) prefix in
                      -- the current outer environment. Its type and graph
                      -- routes are therefore the let scheme construction;
                      -- root reification must not rediscover those local
                      -- binders as free references.
                      pure
                        ( siScheme constructedInfo
                        , schemeInfoBinderRefSubst constructedInfo
                        , False
                        )
                Nothing
                  | Just constructedInfo <- rhsClosedConstructionSchemeInfo ->
                      -- The independently elaborated RHS has already produced
                      -- a closed monomorphic endpoint.  Use that construction
                      -- before graph generalization; otherwise a stale local
                      -- result node can escape even though no binder remains
                      -- to be generalized.
                      pure
                        ( siScheme constructedInfo
                        , schemeInfoBinderRefSubst constructedInfo
                        , False
                        )
                Nothing
                  | Just (scheme, substRefs) <- authoritativeSourceSchemePair ->
                      -- Source ABI authority is independent of whether this
                      -- let also owns a scoped Gamma.  Select it before graph
                      -- generalization so a local graph placeholder can never
                      -- become a free variable merely because the scoped path
                      -- was chosen first.
                      pure (scheme, substRefs, True)
                Nothing
                  | Just (_, _, _, _, mediatorInfo) <-
                      preparedTransparentMediatorConstruction ->
                      -- The mediator packet is the complete construction
                      -- contract for its lambda spine.  Its locally owned
                      -- binders must enter the let scheme before any root
                      -- reification is attempted.
                      pure
                        ( siScheme mediatorInfo
                        , schemeInfoBinderRefSubst mediatorInfo
                        , False
                        )
                Nothing
                  | Just aliasInfo <- aliasSourceSchemeInfo ->
                      -- A transparent alias preserves its lexical owner's
                      -- identity-bearing scheme.  Re-generalizing the alias
                      -- root would replace that positive authority with the
                      -- current graph representative.
                      pure
                        ( siScheme aliasInfo
                        , schemeInfoBinderRefSubst aliasInfo
                        , False
                        )
                Nothing
                  | AmbiguousStructuralRecursiveCandidate <-
                      structuralRecursiveCandidateSelection
                  , annContainsReference binderKey rhsAnn ->
                      Left
                        ( ValidationFailed
                            [ "recursive let has incompatible structural equations"
                            , "  binding: " ++ v
                            , "  RHS: " ++ show rhsAnn
                            ]
                        )
                Nothing
                  | Just (schemeConstruction, substRefs) <- structuralRecursiveSchemePair -> do
                      -- A unique structural recursive equation owns its
                      -- contractive mu construction.  Publish that construction
                      -- before graph generalization can expose a helper-local
                      -- placeholder as a free outer reference.  Ambiguous
                      -- candidates are rejected explicitly above.
                      scheme <- schemeConstruction
                      pure (scheme, substRefs, False)
                Nothing
                  | letRequiresScopedGeneralization -> do
                      (scheme, substRefs) <-
                        withLetSchemeConstructionContext
                          (generalizeAtLetConstructionBoundary schemeRootId)
                      pure (scheme, substRefs, False)
                Nothing -> do
                  (scheme, substRefs) <-
                    withLetSchemeConstructionContext
                      (generalizeAtNode scopeContext schemeRootId)
                  pure (scheme, substRefs, False)
            let lambdaParamNodes annExpr =
                  case annExpr of
                    ALam _ _ paramNode _ body _ _ -> paramNode : lambdaParamNodes body
                    AAnn inner _ _ -> lambdaParamNodes inner
                    AExactAnn inner _ _ _ -> lambdaParamNodes inner
                    ALetScope inner _ _ -> lambdaParamNodes inner
                    AUnfold inner _ _ -> lambdaParamNodes inner
                    _ -> []
                deriveLambdaBinderSubst scheme0 subst0' =
                  let (binds, _) = splitForallsRefs (schemeToType scheme0)
                      binderRefs = map fst (schemeBinderRefs scheme0)
                      binderNames = map typeBinderRefName binderRefs
                      binderBounds = map snd binds
                      paramNodes = lambdaParamNodes rhsAnn
                      binderPairs = zip binderRefs paramNodes
                      canAugment =
                        length binderNames == length paramNodes
                          && all (== Nothing) binderBounds
                   in if canAugment
                        then
                          foldl'
                            ( \acc (ref, paramNode) ->
                                let key = getNodeId (canonical paramNode)
                                 in IntMap.insertWith (\_ old -> old) key ref acc
                            )
                            subst0'
                            binderPairs
                        else subst0'
                normalizedSchemePair =
                  normalizeSchemeSubstPair (scheme0Raw, subst0Raw)
                selectedLetSchemePlacementPackets
                  | isJust structuralRecursiveSchemePair = Map.empty
                  | otherwise = letSchemePlacementPackets
                ( (scheme0Norm, subst0NormRaw)
                  , letSchemePresentationRenames
                  ) =
                    if usesSourceSchemeDirectly
                      then (normalizedSchemePair, [])
                      else canonicalizePublishedSchemePair env normalizedSchemePair
            letSchemeInfoBeforePlacement <-
              if usesSourceSchemeDirectly
                then
                  pure
                    (schemeInfoFromRefSubst scheme0Norm subst0NormRaw)
                else
                  publishTopologyConsumerRoutes
                    ( gaConstructionRouteNodes
                        (scCanonical scopeContext)
                        (scGaParents scopeContext)
                    )
                    selectedLetSchemePlacementPackets
                    (schemeInfoFromRefSubst scheme0Norm subst0NormRaw)
            let subst0Norm =
                  schemeInfoBinderRefSubst letSchemeInfoBeforePlacement
            scheme0Placed <-
              if usesSourceSchemeDirectly
                then pure scheme0Norm
                else
                  placeSubtermGeneralizationBindersWithRoutes
                    subst0Norm
                    selectedLetSchemePlacementPackets
                    (siScheme letSchemeInfoBeforePlacement)
            let scheme0Ty = schemeToType scheme0Placed
                schemeBase =
                  -- A source-owned annotation defines an ordered xMLF
                  -- forall ABI.  Simplifying it may remove a vacuous binder
                  -- while use sites still carry the matching positional N
                  -- computation, so only graph-inferred schemes may be
                  -- canonicalized this way.
                  if isJust authoritativeSourceSchemePair
                    then scheme0Placed
                    else schemeFromType (simplifyAnnotationType scheme0Ty)
            {- Note [Mu-type annotation override for let schemes]
               ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               When a let-bound RHS is a lambda with a μ-type annotation on its
               parameter (e.g. let g = (λx:μα.α→Int. x) in …), the generalization
               may produce an overly-generic scheme (∀a.∀b. a→b) because the
               constraint graph's μ-node lives under the lambda scope and is not
               visible as a binder-bound at the let scope.

               We detect this case by inspecting the RHS annotation structure for
               a desugared annotated lambda whose annotation node reifies to a
               contractive TMu witness. When found, we override the scheme with a
               monomorphic function type that uses the witnessed μ-type as both
               domain and codomain (identity-like), or more precisely, domain =
               μ-type and codomain = μ-type when the body simply returns the
               parameter. -}
            inferredScheme <-
              let firstNonContractiveMuAnnotation annExpr =
                    case annExpr of
                      ALam _lamParam mbDetails _ _ lamBody _ _ ->
                        case desugaredAnnLambdaInfo mbDetails lamBody of
                          Just (_, annNodeId, _, _) ->
                            case reifyNodeTypePreferringBound scopeContext annNodeId of
                              Right annTy ->
                                firstNonContractiveRecursiveType annTy
                              _ -> Nothing
                          Nothing -> Nothing
                      AAnn inner _ _ -> firstNonContractiveMuAnnotation inner
                      AExactAnn inner _ _ _ -> firstNonContractiveMuAnnotation inner
                      ALetScope inner _ _ -> firstNonContractiveMuAnnotation inner
                      AUnfold inner _ _ -> firstNonContractiveMuAnnotation inner
                      _ -> Nothing
                  muAnnotationTy annExpr =
                    case annExpr of
                      ALam _lamParam mbDetails _ _ lamBody _ _ ->
                        case desugaredAnnLambdaInfo mbDetails lamBody of
                          Just (_, annNodeId, _, _) ->
                            case reifyNodeTypePreferringBound scopeContext annNodeId of
                              Right annTy@TMuRef {}
                                | hasContractiveRecursiveWitness annTy -> Just annTy
                              _ -> Nothing
                          Nothing -> Nothing
                      AAnn inner _ _ -> muAnnotationTy inner
                      AExactAnn inner _ _ _ -> muAnnotationTy inner
                      ALetScope inner _ _ -> muAnnotationTy inner
                      AUnfold inner _ _ -> muAnnotationTy inner
                      _ -> Nothing
                  muAnnotatedIdentityBody annExpr =
                    case annExpr of
                      ALam _lamParam mbDetails _ _ lamBody _ _ ->
                        case desugaredAnnLambdaInfo mbDetails lamBody of
                          Just (mediatorDetails, _, _, innerBodyAnn) ->
                            annExprReferenceKey innerBodyAnn == Just (annBinderKey mediatorDetails)
                          Nothing -> False
                      AAnn inner _ _ -> muAnnotatedIdentityBody inner
                      AExactAnn inner _ _ _ -> muAnnotatedIdentityBody inner
                      ALetScope inner _ _ -> muAnnotatedIdentityBody inner
                      AUnfold inner _ _ -> muAnnotatedIdentityBody inner
                      _ -> False
                  overrideMuAnnotatedCodomain muTy =
                    let stripForalls ty =
                          case ty of
                            TForallRef _ _ inner -> stripForalls inner
                            other -> other
                        strippedSchemeBody = stripForalls (schemeToType schemeBase)
                        quantRefs = map fst (fst (splitForallsRefs (schemeToType schemeBase)))
                        isUnquantifiedTVar (TVarRef ref) =
                          not (any (typeBinderRefsSameIdentity ref) quantRefs)
                        isUnquantifiedTVar _ = False
                     in case strippedSchemeBody of
                          TArrow _dom cod
                            | isUnquantifiedTVar cod ->
                                -- Codomain is an unquantified internal variable:
                                -- override both domain and codomain to μ.
                                schemeFromType (TArrow muTy muTy)
                          _ -> schemeBase
               in case inheritedGammaBinding of
                    Just (_, gammaBound) ->
                      pure (schemeFromType gammaBound)
                    Nothing
                      | usesSourceSchemeDirectly ->
                          -- Recursive-carrier recovery repairs graph-inferred
                          -- schemes only.  Once an explicit RHS annotation
                          -- has selected the binding ABI above, revisiting the
                          -- annotated term here can only replace that ABI with
                          -- another inference result.
                          pure schemeBase
                    Nothing
                      | isJust rhsOwnerConstructionSchemeInfo ->
                          pure schemeBase
                    Nothing ->
                      case aliasSourceSchemeInfo of
                        Just aliasInfo ->
                          pure (siScheme aliasInfo)
                        Nothing ->
                          case (structuralRecursiveCandidateSelection, annContainsReference binderKey rhsAnn, blockedAliasMuType (schemeToType schemeBase)) of
                            -- A returned recursive helper determines the
                            -- enclosing function's arrow topology before the
                            -- RHS is elaborated.  Keeping an unrelated root
                            -- mu carrier here and replacing it after checking
                            -- would invalidate the explicit roll/unroll
                            -- computations already emitted for recursive
                            -- occurrences.
                            (UniqueStructuralRecursiveCandidate (StructuralRecursiveCandidateFromHelper candidateTy), True, _) ->
                              pure (schemeFromType candidateTy)
                            (UniqueStructuralRecursiveCandidate (StructuralRecursiveCandidateFromDirectCarrier candidateTy), True, Just muTy)
                              | shouldPreferInferredRecursiveCarrier muTy candidateTy ->
                                  pure (schemeFromType candidateTy)
                            (UniqueStructuralRecursiveCandidate (StructuralRecursiveCandidateFromDirectCarrier candidateTy), True, Nothing)
                              | not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                                  pure (schemeFromType candidateTy)
                            (AmbiguousStructuralRecursiveCandidate, True, _) ->
                              Left
                                ( ValidationFailed
                                    [ "recursive let reached inference with incompatible structural equations"
                                    , "  binding: " ++ v
                                    ]
                                )
                            (NoStructuralRecursiveCandidate, True, Just muTy) ->
                              pure (schemeFromType muTy)
                            _ ->
                              case firstNonContractiveMuAnnotation mediatedMuSubject of
                                Just badTy ->
                                  Left (InstantiationError ("non-contractive recursive annotation: " ++ show badTy))
                                Nothing ->
                                  pure $
                                    case muAnnotationTy mediatedMuSubject of
                                      Just muTy ->
                                        {- Note [Selective codomain override for μ-annotated lambdas]
                                           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                           The domain is always overridden to the μ-type since
                                           the surrounding μ-annotation detection confirms that the lambda parameter has
                                           an explicit contractive μ-annotation (e.g. λx:μα.α→Int. x).

                                           For the codomain: when the scheme is fully polymorphic
                                           (e.g. ∀a.∀b. a→b with both vars quantified), generalization
                                           captured the correct parametricity and downstream elaboration
                                           handles the μ-type through normal instantiation — so we leave
                                           schemeBase intact. When the codomain is a constraint-internal
                                           variable (e.g. TVar "t10" that wasn't quantified), generalization
                                           lost track of its relationship to the μ-annotated parameter,
                                           and we override it to the μ-type. -}
                                        if muAnnotatedIdentityBody mediatedMuSubject
                                          then schemeFromType (TArrow muTy muTy)
                                          else overrideMuAnnotatedCodomain muTy
                                      Nothing ->
                                        case recursiveCarrierCandidate of
                                          Just carrierTy
                                            | annContainsReference binderKey rhsAnn,
                                              not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                                                schemeFromType carrierTy
                                          Nothing
                                            | not (hasContractiveRecursiveWitness (schemeToType schemeBase)) ->
                                                schemeBase
                                          _ -> schemeBase
            let inferredSubst0 =
                  normalizeSubstForScheme
                    inferredScheme
                    (deriveLambdaBinderSubst scheme0Norm subst0Norm)
                inferredSubst =
                  case inheritedGammaBinding of
                    Just _ -> inferredSubst0
                    Nothing
                      | isJust rhsOwnerConstructionSchemeInfo -> inferredSubst0
                    Nothing ->
                      case aliasSourceSchemeInfo of
                        Just aliasInfo -> schemeInfoBinderRefSubst aliasInfo
                        Nothing ->
                          let (binds, _) = splitForallsRefs (schemeToType inferredScheme)
                           in if null binds then IntMap.empty else inferredSubst0
                inferredSchemeInfo0 =
                  schemeInfoFromRefSubst inferredScheme inferredSubst
            (transparentRhsGammaIdentities, inferredSchemeInfo) <-
              case exactConstructionSchemeInfo of
                Just _ -> pure (Set.empty, inferredSchemeInfo0)
                Nothing
                  | isJust rhsOwnerConstructionSchemeInfo ->
                      pure (Set.empty, inferredSchemeInfo0)
                Nothing
                  | isJust structuralRecursiveSchemePair ->
                      -- A closed structural recursive scheme owns Typ(rhs)
                      -- before descendant packet placement.  Its lambda and
                      -- application constructors still emit their own local
                      -- Gamma; composing that packet into the enclosing let
                      -- would republish a helper-local result binder with the
                      -- packet's provisional graph bound.
                      pure (Set.empty, inferredSchemeInfo0)
                Nothing ->
                  composeTransparentRhsCompletedGamma
                    v
                    (scopeTypeBinderIdentityRepresentative scopeContext)
                    rhsAnn
                    (algSubtermGeneralizations algebraContext)
                    inferredSchemeInfo0
            let selectedSchemeInfo =
                  fromMaybe inferredSchemeInfo exactConstructionSchemeInfo
                scheme = siScheme selectedSchemeInfo
                subst =
                  normalizeSubstForScheme
                    scheme
                    (schemeInfoBinderRefSubst selectedSchemeInfo)
            schemeInfo <-
              freshenSchemeInfoAgainstConstructionEnvExcept
                transparentRhsGammaIdentities
                (scopeTypeBinderIdentityRepresentative scopeContext)
                (acSourceBinderRefs annotationContext)
                env
                ( applySchemeInfoRefRenames
                    (envConstructionBinderRenames env)
                    (schemeInfoFromRefSubst scheme subst)
                )
            authoritativeSourceSchemeInfo <-
              case authoritativeSourceSchemePair of
                _ | not usesSourceSchemeDirectly -> pure Nothing
                Just (schemeSrc, substSrc) -> do
                  sourceSchemeInfo <-
                    freshenSchemeInfoAgainstConstructionEnv
                      (scopeTypeBinderIdentityRepresentative scopeContext)
                      (acSourceBinderRefs annotationContext)
                      env
                      ( applySchemeInfoRefRenames
                          (envConstructionBinderRenames env)
                          (schemeInfoFromRefSubst schemeSrc substSrc)
                      )
                  sourceToConstructionRenames <-
                    sharedConstructionRouteRenames
                      ("source ABI for let " ++ v)
                      (schemeInfoBinderRefSubst schemeInfo)
                      (schemeInfoBinderRefSubst sourceSchemeInfo)
                  pure
                    ( Just
                        ( applySchemeInfoRefRenames
                            sourceToConstructionRenames
                            sourceSchemeInfo
                        )
                    )
                Nothing -> pure Nothing
            let envSchemeInfoForRhs =
                  fromMaybe schemeInfo authoritativeSourceSchemeInfo
            rhsConstructionSubst <-
              foldM
                (\aliases (nodeKey, constructionRef) ->
                  case IntMap.lookup nodeKey aliases of
                    Nothing -> pure (IntMap.insert nodeKey constructionRef aliases)
                    Just sourceRef
                      | typeBinderRefsSameIdentity sourceRef constructionRef ->
                          pure aliases
                      | otherwise ->
                          Left
                            ( ValidationFailed
                                [ "source and exact construction schemes disagree on an RHS binder route"
                                , "  binding: " ++ v
                                , "  graph node: " ++ show (NodeId nodeKey)
                                , "  source ref: " ++ show sourceRef
                                , "  construction ref: " ++ show constructionRef
                                ]
                            )
                )
                (schemeInfoBinderRefSubst envSchemeInfoForRhs)
                (IntMap.toList (schemeInfoBinderRefSubst schemeInfo))
            let
                transparentMediator =
                  transparentMediatorKindAnn rhsAnn
                    <|> (aliasSourceKey >>= (`lookupEnvBindingForKey` env) >>= ebTransparentMediator)
                envBindingFor bindingSchemeInfo =
                  case aliasSourceKey of
                    Just sourceKey ->
                      (mkLocalEnvBinding v binderDetails bindingSchemeInfo transparentMediator)
                        { ebAliasTarget = Just (resolveAliasKey env sourceKey)
                        }
                    Nothing -> mkLocalEnvBinding v binderDetails bindingSchemeInfo transparentMediator
                tcEnvBase = typeCheckEnvFrom env
                typeCheckBase = TypeCheck.typeCheckWithEnv tcEnvBase
                env' = insertEnvBinding (envBindingFor envSchemeInfoForRhs) env
                -- Figure 15.3.5 constructs the RHS computations under the
                -- let-bound scheme's Gamma and only then closes them with
                -- the matching type abstractions.  In particular, a
                -- recursive derived helper can use a Hyp computation for a
                -- captured class parameter while elaborating its RHS; the
                -- binder is not in scope merely because the recursive term
                -- binding already carries the polymorphic scheme.
                envForRhs0 =
                  extendEnvTypeScopeWithAliases
                    rhsConstructionSubst
                    (schemeBinderRefs (siScheme envSchemeInfoForRhs))
                    env'
                envForRhs =
                  let constructedParamTypes =
                        constructedLambdaParamTypes
                          envForRhs0
                          (schemeToType (siScheme envSchemeInfoForRhs))
                          rhsAnn
                   in envForRhs0
                        { envConstructedLambdaParamTypes =
                            Map.union
                              constructedParamTypes
                              (envConstructedLambdaParamTypes envForRhs0)
                        , envExpectedTermEndpoint =
                            if localBinderIsDiscard binderDetails
                              then Nothing
                              else
                                independentRhsProducerEndpoint
                                  <|> Just
                                    -- A compiler-exact owner scheme is just
                                    -- as construction-authoritative as a
                                    -- source-declared scheme.  Preserve that
                                    -- endpoint role through the RHS lambda
                                    -- spine so its opened forall variables
                                    -- never become provisional Bottom
                                    -- parameter annotations.
                                    ( ( if usesSourceSchemeDirectly
                                            || isJust exactConstructionSchemeInfo
                                          then ExactConstructionExpectedTerm
                                          else CheckingExpectedTerm
                                      )
                                        (schemeBody (siScheme envSchemeInfoForRhs))
                                    )
                        }
                tcEnv = typeCheckEnvFrom env'
                typeCheckLet = TypeCheck.typeCheckWithEnv tcEnv
                transparentMediatorTerm = do
                  (rootDetails, etaParams, rootParamTy, etaParamTys, _) <-
                    preparedTransparentMediatorConstruction
                  let rootResolved = resolvedLocalBinder rootDetails rootParamTy
                      etaResolved =
                        zipWith
                          (\(details, _) paramTy -> resolvedLocalBinder details paramTy)
                          etaParams
                          etaParamTys
                      applied =
                        foldl'
                          EApp
                          (EVarNode rootResolved)
                          (map EVarNode etaResolved)
                  pure (ELam rootResolved (foldr ELam applied etaResolved))
            rhsElaborationRaw <-
              case (transparentMediator, transparentMediatorTerm) of
                (_, Just constructed) ->
                  pure
                    ElaboratedTerm
                      { elaboratedTerm = constructed,
                        elaboratedOwnerFinalConstruction = Nothing,
                        elaboratedLocalGammaConstructionCertificates = [],
                        elaboratedCompilerExactResultBoundCertificates = []
                      }
                _
                  | Just elaboration <- rhsConstructionElaborationBeforeScheme
                  , isJust rhsOwnerConstructionSchemeInfo ->
                      pure elaboration
                _ ->
                  case elabDetailed rhsOut envForRhs of
                    Right elaboration -> pure elaboration
                    Left cause ->
                      Left
                        ( ValidationFailed
                            [ "let RHS failed under its selected construction scheme"
                            , "  binding: " ++ v
                            , "  scheme: " ++ show (siScheme envSchemeInfoForRhs)
                            , "  cause: " ++ show cause
                            ]
                        )
            let rhsElaboration =
                  renameElaboratedTermBinderRefPayloads
                    letSchemePresentationRenames
                    rhsElaborationRaw
            let rhs' = elaboratedTerm rhsElaboration
                rhsLocalGammaCertificates =
                  elaboratedLocalGammaConstructionCertificates rhsElaboration
                rhsCompilerExactResultBoundCertificates =
                  elaboratedCompilerExactResultBoundCertificates rhsElaboration
                closeFreeVarsToScheme ty =
                  let (binds, body) = splitForallsRefs ty
                      boundRefs = map fst binds
                      extraBinds =
                        [ (ref, Nothing)
                          | ref <- freeTypeVarRefsInOccurrenceOrder body,
                            not (any (typeBinderRefsSameIdentity ref) boundRefs)
                        ]
                   in mkElabSchemeWithRefs (binds ++ extraBinds) body
                splitArrowN n ty
                  | n <= (0 :: Int) = Just ([], ty)
                  | otherwise =
                      case ty of
                        TArrow dom cod -> do
                          (doms, resultTy) <- splitArrowN (n - 1) cod
                          pure (dom : doms, resultTy)
                        _ -> Nothing
                collectLeadingLambdaParams term =
                  case term of
                    ELam resolved body ->
                      let (params, core) = collectLeadingLambdaParams body
                       in (resolved : params, core)
                    -- Lambda-body edges can place the paper's explicit
                    -- coercion around the next eta lambda.  The source-side
                    -- transparent-mediator proof already authorizes rebuilding
                    -- this whole spine, so the coercion is not a lexical
                    -- boundary for collecting its parameters.
                    ETyInst inner _ -> collectLeadingLambdaParams inner
                    ELet _ sch (EVarNode _) body
                      | null (schemeBinderRefs sch) ->
                          collectLeadingLambdaParams body
                    _ -> ([], term)
                collapsedIdentityWrapperScheme ty =
                  case splitForallsRefs ty of
                    (binders, TArrow domTy codTy) ->
                      let boundRefs = map fst binders
                          isUnboundInternal ref =
                            isInternalTypeBinderRef ref
                              && not (any (typeBinderRefsSameIdentity ref) boundRefs)
                       in codTy == TBottom
                            || any isUnboundInternal (freeTypeVarRefsType codTy)
                            -- A syntactic identity wrapper has one carrier.
                            -- Independent generalized domain/codomain binders
                            -- are already evidence that graph sharing was lost.
                            || not (alphaEqType domTy codTy)
                    -- A lambda cannot construct a non-arrow scheme.  When
                    -- contextual graph sharing has collapsed the let root to
                    -- the enclosing result (for example Bool), the checked
                    -- identity RHS remains the construction authority.
                    _ -> True
                rebuildTransparentMediatorTerm rootName etaParams resultTy =
                  let rootParamTy = foldr TArrow resultTy (map resolvedVarType etaParams)
                      (rootRef, _) =
                        freshLocalRef
                          rootName
                          ( identityGeneratorAfter $
                              envGeneratedIdentities env
                                ++ generatedIdentitiesInType rootParamTy
                                ++ concatMap (generatedIdentitiesInTerm . EVarNode) etaParams
                          )
                      rootResolved = localResolvedVarFromRef rootRef rootParamTy
                      mediatorBody =
                        foldr
                          ELam
                          (foldl' EApp (EVarNode rootResolved) (map EVarNode etaParams))
                          etaParams
                   in (rootParamTy, ELam rootResolved mediatorBody)
                rhsAliasTerm = stripUnusedTopTyAbsWithEnv tcEnvBase rhs'
                rhsTransparentMediatorTerm = stripLeadingTyAbs rhsAliasTerm
                rhsConstructionTy =
                  TypeCheck.typeCheckWithEnv
                    ( restrictTypeCheckEnvToFreeTermBindings
                        rhs'
                        (typeCheckEnvFrom envForRhs)
                    )
                    rhs'
                identityWrapperMediatorExpr aliases expr =
                  case annExprReferenceKey (stripAnnExpr expr) of
                    Just key ->
                      case Map.lookup key aliases of
                        Just IdentityWrapperMediator -> True
                        Just IdentityWrapperRoot -> False
                        Nothing ->
                          case lookupEnvBindingForKey (resolveAliasKey env key) env of
                            Just binding ->
                              isJust (ebTransparentMediator binding)
                                || isSingleBinderIdentityScheme (ebSchemeInfo binding)
                            Nothing -> False
                    Nothing ->
                      case stripAnnExpr expr of
                        ALam _param mbDetails _ _ body _ _ ->
                          identityWrapperBody (annBinderKey mbDetails) Map.empty body
                        ALet _boundName mbDetails _ _ _ _ rhs body _
                          | let boundKey = annBinderKey mbDetails,
                            Map.member boundKey aliases -> False
                          | Just origin <- identityWrapperAliasOrigin (annBinderKey mbDetails) aliases rhs ->
                              identityWrapperMediatorExpr (Map.insert (annBinderKey mbDetails) origin aliases) body
                          | otherwise -> False
                        _ -> False
                identityWrapperHead root aliases expr =
                  case annExprReferenceKey (stripAnnExpr expr) of
                    Just key ->
                      case Map.lookup key aliases of
                        Just IdentityWrapperMediator -> True
                        Just IdentityWrapperRoot -> False
                        Nothing
                          | key == root -> False
                          | otherwise ->
                              case lookupEnvBindingForKey (resolveAliasKey env key) env of
                                Just binding ->
                                  isJust (ebTransparentMediator binding)
                                    || isSingleBinderIdentityScheme (ebSchemeInfo binding)
                                Nothing -> False
                    Nothing -> False
                identityWrapperExpr root aliases expr =
                  case annExprReferenceKey (stripAnnExpr expr) of
                    Just key ->
                      case Map.lookup key aliases of
                        Just IdentityWrapperRoot -> True
                        Just IdentityWrapperMediator -> False
                        Nothing -> key == root
                    Nothing ->
                      case stripAnnExpr expr of
                        ALet _boundName mbDetails _ _ _ _ rhs body _
                          | let boundKey = annBinderKey mbDetails,
                            boundKey == root || Map.member boundKey aliases ->
                              False
                          | Just origin <- identityWrapperAliasOrigin root aliases rhs ->
                              identityWrapperExpr root (Map.insert (annBinderKey mbDetails) origin aliases) body
                          | otherwise ->
                              False
                        other ->
                          let (funExpr, argExprs) = annAppSpine other
                           in case argExprs of
                                [argExpr] ->
                                  identityWrapperHead root aliases funExpr
                                    && identityWrapperExpr root aliases argExpr
                                _ -> False
                identityWrapperAliasOrigin root aliases rhsExpr =
                  if identityWrapperExpr root aliases rhsExpr
                    then Just IdentityWrapperRoot
                    else
                      if identityWrapperMediatorExpr aliases rhsExpr
                        then Just IdentityWrapperMediator
                        else Nothing
                identityWrapperBody root aliases expr =
                  identityWrapperExpr root aliases expr
                schemeNeedsStructuralRecovery schemeTy =
                  not (schemeTypeHasExplicitBound schemeTy)
                    && (containsInternalTypeVar schemeTy || schemeHasForwardBoundReference schemeTy)
                rhsTransparentMediatorOverride =
                  if isJust transparentMediator
                    then case rhsTransparentMediatorTerm of
                      ELam rootResolved body ->
                        let rootParamTy = resolvedVarType rootResolved
                            (etaParams, _core) = collectLeadingLambdaParams body
                            etaParamTys = map resolvedVarType etaParams
                         in case splitArrowN (length etaParams) rootParamTy of
                              Just (_expectedEtaParamTys, resultTy)
                                | not (null etaParams),
                                  let (structuralRootParamTy, structuralMediatorTerm) =
                                        rebuildTransparentMediatorTerm v etaParams resultTy,
                                  let rhsScheme =
                                        closeFreeVarsToScheme
                                          (TArrow structuralRootParamTy (foldr TArrow resultTy etaParamTys)),
                                  let candidateSubst =
                                        case splitForallsRefs (schemeToType rhsScheme) of
                                          ([], _) -> IntMap.empty
                                          _ -> normalizeSubstForScheme rhsScheme subst,
                                  let candidateSchemeInfo =
                                        schemeInfoFromRefSubst rhsScheme candidateSubst,
                                  let rhsClosed =
                                        closeTermWithSchemeSubstRefsIfNeeded
                                          (schemeInfoBinderRefSubst candidateSchemeInfo)
                                          (siScheme candidateSchemeInfo)
                                          structuralMediatorTerm,
                                  let candidateSchemeAdmitsRhs =
                                        case typeCheckBase rhsClosed of
                                          Right rhsTy -> alphaEqType rhsTy (schemeToType rhsScheme)
                                          Left _ -> False,
                                  candidateSchemeAdmitsRhs
                                    || containsInternalTypeVar (schemeToType scheme)
                                    || schemeHasForwardBoundReference (schemeToType scheme)
                                    || not (alphaEqType (schemeToType scheme) (schemeToType rhsScheme)) ->
                                    Just
                                      ( structuralMediatorTerm,
                                        candidateSchemeInfo
                                      )
                              _ -> Nothing
                      _ -> Nothing
                    else Nothing
                rhsIdentityWrapperOverride =
                  case (stripAnnExpr rhsAnn, rhsTransparentMediatorTerm) of
                    (ALam _rootParam mbRootDetails _ _ body _ _, ELam rootResolved _)
                      | let rootKey = annBinderKey mbRootDetails,
                        rootKey == ResolvedBindingKey (idDetailsIdentityKey (resolvedVarDetails rootResolved)),
                        identityWrapperBody rootKey Map.empty body ->
                          let generalizedSchemeTy = schemeToType scheme
                              rootParamTy =
                                case splitForallsRefs generalizedSchemeTy of
                                  (_, TArrow domTy _) -> domTy
                                  _ -> resolvedVarType rootResolved
                              rootResolved' = mapResolvedVarType (const rootParamTy) rootResolved
                              rhsTerm = ELam rootResolved' (EVarNode rootResolved')
                              rhsScheme = closeFreeVarsToScheme (TArrow rootParamTy rootParamTy)
                              candidateSubst =
                                case splitForallsRefs (schemeToType rhsScheme) of
                                  ([], _) -> IntMap.empty
                                  _ -> normalizeSubstForScheme rhsScheme subst
                              candidateSchemeInfo =
                                schemeInfoFromRefSubst rhsScheme candidateSubst
                              rhsClosed =
                                closeTermWithSchemeSubstRefsIfNeeded
                                  (schemeInfoBinderRefSubst candidateSchemeInfo)
                                  (siScheme candidateSchemeInfo)
                                  rhsTerm
                              candidateSchemeAdmitsRhs =
                                case typeCheckBase rhsClosed of
                                  Right rhsTy -> alphaEqType rhsTy (schemeToType rhsScheme)
                                  Left _ -> False
                              generalizedSchemeNeedsRecovery =
                                schemeNeedsStructuralRecovery generalizedSchemeTy
                                  || collapsedIdentityWrapperScheme generalizedSchemeTy
                                  -- Transparent result ownership is a positive
                                  -- construction plan, even when the enclosing
                                  -- inferred scheme itself is already closed.
                                  || not (Set.null transparentRhsGammaIdentities)
                           in if candidateSchemeAdmitsRhs && generalizedSchemeNeedsRecovery
                                then
                                  Just
                                    ( rhsTerm,
                                      candidateSchemeInfo
                                    )
                                else Nothing
                    _ -> Nothing
                rhsAliasOverride =
                  case (rhsAliasTerm, rhsAliasTy) of
                    (EVarNode _, Right rhsTy)
                      | not (alphaEqType rhsTy (schemeToType scheme)) ->
                          -- An alias occurrence already carries the scheme of
                          -- its lexical owner.  Any free type variables in
                          -- that type belong to Gamma; closing them here would
                          -- re-generalize a lambda-bound parameter (for
                          -- example a recursive deriving handler's @List a@)
                          -- into an unrelated @forall a. List a@.  Figure
                          -- 15.3.5 therefore publishes the carried type as a
                          -- degenerate scheme and leaves explicit foralls
                          -- already present on the aliased value intact.
                          let rhsScheme = schemeFromType rhsTy
                              rhsSubst =
                                case splitForallsRefs rhsTy of
                                  ([], _) -> IntMap.empty
                                  _ -> subst
                           in Just (rhsAliasTerm, schemeInfoFromRefSubst rhsScheme rhsSubst)
                    _ -> Nothing
                -- With no source annotation, the checked RHS is the owner of
                -- an already-constructed contractive μ carrier.  Retaining an
                -- unfolded generalized arrow here would force Phase 7 to
                -- rediscover information that elaboration has already proved.
                rhsRecursiveCarrierOverride =
                  case rhsAliasTy of
                    Right rhsTy
                      | isNothing authoritativeSourceSchemePair,
                        hasContractiveRecursiveWitness rhsTy,
                        not (hasContractiveRecursiveWitness (schemeToType scheme)),
                        not (alphaEqType rhsTy (schemeToType scheme)) ->
                          Just
                            ( rhsAliasTerm,
                              schemeInfoFromRefSubst (schemeFromType rhsTy) IntMap.empty
                            )
                    _ -> Nothing
                -- A non-recursive let is constructed from the checked RHS.
                -- The graph plan may retain an unbounded copy placeholder as
                -- its nominal scheme root even though the RHS has already
                -- produced a closed ground or structurally polymorphic type.
                -- That checked type is the construction result; wrapping it
                -- in an otherwise admissible vacuous flexible binder would
                -- publish a less-principal scheme.  Generalize the checked
                -- type itself before the ELet node is built.
                rhsCheckedConstructionOverride =
                  case rhsConstructionTy of
                    Right rhsTy
                      | isNothing authoritativeSourceSchemePair,
                        not (annContainsReference binderKey rhsAnn) ->
                          case
                              constructionGammaCompletionBinders
                                (schemeBinderRefs (siScheme envSchemeInfoForRhs))
                                rhs'
                                rhsTy
                            of
                              Left _ -> Nothing
                              Right rhsBinders ->
                                let rhsScheme =
                                      mkElabSchemeWithRefs rhsBinders rhsTy
                                    rhsSchemeInfo =
                                      schemeInfoFromRefSubst
                                        rhsScheme
                                        (normalizeSubstForScheme rhsScheme subst)
                                    closedWith info term =
                                      closeTermWithSchemeSubstRefsIfNeeded
                                        (schemeInfoBinderRefSubst info)
                                        (siScheme info)
                                        term
                                    admits info term =
                                      case typeCheckBase (closedWith info term) of
                                        Right closedTy ->
                                          alphaEqType closedTy (schemeToType (siScheme info))
                                        Left _ -> False
                                 in if alphaEqType rhsTy (schemeToType (siScheme schemeInfo))
                                      then Nothing
                                      else
                                        if admits rhsSchemeInfo rhs'
                                          then Just (rhs', rhsSchemeInfo)
                                          else Nothing
                    _ -> Nothing
                effectiveRhsOverride =
                  case rhsTransparentMediatorOverride of
                    Just overrideInfo -> Just overrideInfo
                    Nothing ->
                      case rhsIdentityWrapperOverride of
                        Just overrideInfo -> Just overrideInfo
                        Nothing ->
                          case rhsAliasOverride of
                            Just overrideInfo -> Just overrideInfo
                            Nothing ->
                              case rhsRecursiveCarrierOverride of
                                Just overrideInfo -> Just overrideInfo
                                Nothing -> rhsCheckedConstructionOverride
                effectiveSchemeInfoRaw =
                  let inputInfo =
                        case effectiveRhsOverride of
                          Just (_, overrideInfo) -> overrideInfo
                          Nothing -> schemeInfo
                   in freshenSchemeInfoAgainstEnvWithRepresentativeExcept
                        transparentRhsGammaIdentities
                        (scopeTypeBinderIdentityRepresentative scopeContext)
                        env
                        inputInfo
                effectiveRhsTermRaw =
                  case effectiveRhsOverride of
                    Just (overrideTerm, _) -> overrideTerm
                    Nothing -> rhs'
                ( (effectiveScheme, effectiveSubstRefs)
                  , effectiveSchemePresentationRenames
                  ) =
                    if usesSourceSchemeDirectly
                      then
                        ( ( siScheme effectiveSchemeInfoRaw
                          , schemeInfoBinderRefSubst effectiveSchemeInfoRaw
                          )
                        , []
                        )
                      else
                        canonicalizePublishedSchemePair
                          env
                          ( siScheme effectiveSchemeInfoRaw
                          , schemeInfoBinderRefSubst effectiveSchemeInfoRaw
                          )
                effectiveSchemeInfo =
                  schemeInfoFromRefSubst
                    effectiveScheme
                    effectiveSubstRefs
                effectiveRhsTerm =
                  renameTermTypeBinderRefPayloads
                    effectiveSchemePresentationRenames
                    effectiveRhsTermRaw
                effectiveRhsTy = typeCheckLet effectiveRhsTerm
                authoritativeEnvSchemeInfo =
                  fromMaybe
                    ( freshenSchemeInfoAgainstEnvWithRepresentativeExcept
                        transparentRhsGammaIdentities
                        (scopeTypeBinderIdentityRepresentative scopeContext)
                        env
                        schemeInfo
                    )
                    authoritativeSourceSchemeInfo
                effectiveSubst = effectiveSubstRefs
                envSchemeInfoForBody =
                  if usesSourceSchemeDirectly
                    then authoritativeEnvSchemeInfo
                    else effectiveSchemeInfo
                envForBody = insertEnvBinding (envBindingFor envSchemeInfoForBody) env
                tcEnvForBody = typeCheckEnvFrom envForBody
                typeCheckForBody = TypeCheck.typeCheckWithEnv tcEnvForBody
                rhsAliasTy = typeCheckBase rhsAliasTerm
            compilerExactClosedRhs <-
              case (exactConstructionSchemeInfo, effectiveRhsOverride) of
                (Just _, Nothing) -> do
                  let splitLeadingTypeAbstractions term =
                        case term of
                          ETyAbsRef ref mbBound body ->
                            let (rest, core) = splitLeadingTypeAbstractions body
                             in ((ref, mbBound) : rest, core)
                          _ -> ([], term)
                      (existingBinders, exactCore) =
                        splitLeadingTypeAbstractions effectiveRhsTerm
                      schemeBinders = schemeBinderRefs effectiveScheme
                      unknownExisting =
                        [ ref
                        | (ref, _) <- existingBinders
                        , not
                            ( any
                                (typeBinderRefsSameIdentity ref . fst)
                                schemeBinders
                            )
                        ]
                      exactClosed =
                        foldr
                          (\(ref, mbBound) body -> ETyAbsRef ref mbBound body)
                          exactCore
                          schemeBinders
                      exactSchemeTy = schemeToType effectiveScheme
                  case unknownExisting of
                    _ : _ ->
                      Left
                        ( ValidationFailed
                            [ "compiler exact RHS carries an abstraction outside its constructed scheme"
                            , "  binding: " ++ v
                            , "  unknown abstractions: " ++ show unknownExisting
                            ]
                        )
                    [] ->
                      case typeCheckLet exactClosed of
                        Right checkedTy
                          | alphaEqType checkedTy exactSchemeTy
                              || churchAwareEqType checkedTy exactSchemeTy ->
                              pure (Just exactClosed)
                        checked ->
                          Left
                            ( PhiInvariantError
                                ( unlines
                                    [ "ALetF: compiler exact construction does not inhabit its prepared full-owner scheme"
                                    , "binding=" ++ v
                                    , "scheme=" ++ show effectiveScheme
                                    , "existing abstractions=" ++ show existingBinders
                                    , "typecheck=" ++ show checked
                                    ]
                                )
                            )
                _ -> pure Nothing
            let rhsAbs0 =
                  let schemeTy = schemeToType effectiveScheme
                      rhsMatchesScheme rhsTy =
                        alphaEqType rhsTy schemeTy
                          || case schemeTy of
                            muTy@(TMuRef muRef muBody) ->
                              let expectedBodyTy = substTypeCaptureRef muRef muTy muBody
                               in alphaEqType rhsTy expectedBodyTy
                            _ -> False
                   in case compilerExactClosedRhs of
                        Just exactClosed -> exactClosed
                        Nothing ->
                          case (effectiveRhsTerm, effectiveRhsTy) of
                            (EVarNode _, _) ->
                              closeTermWithSchemeSubstRefsIfNeeded effectiveSubstRefs effectiveScheme effectiveRhsTerm
                            (_, Right rhsTy)
                              | rhsMatchesScheme rhsTy ->
                                  -- The independently constructed RHS already
                                  -- inhabits the selected let scheme.  Preserve
                                  -- that computation even when the scheme has a
                                  -- forall spine: rebuilding it as
                                  -- @Lambda alpha. rhs[alpha]@ would replace
                                  -- the paper's direct @omega[N] id@
                                  -- construction with an unnecessary
                                  -- eta/Hyp wrapper.
                                  effectiveRhsTerm
                            _ ->
                              -- The selected let scheme is the construction
                              -- Gamma under which the RHS was elaborated.
                              -- Publish that Gamma directly instead of asking
                              -- the finished open term whether wrapping happens
                              -- to type-check: the latter can mistake a prepared
                              -- ambient binder for a binder that will remain in
                              -- scope after this let boundary.
                              constructTermWithSchemeSubstRefs
                                effectiveSubstRefs
                                effectiveScheme
                                effectiveRhsTerm
                rhsAbs =
                  let schemeTy = schemeToType effectiveScheme
                      rhsAbs0Ty = typeCheckLet rhsAbs0
                      rhsAbsBase =
                        if not (null (schemeBinderRefs effectiveScheme))
                          then
                            case rhsAbs0Ty of
                              Right rhsTy
                                | alphaEqType rhsTy schemeTy -> rhsAbs0
                              _ ->
                                case case (rhsAbs0, rhsAbs0Ty) of
                                  (ETyAbsRef _ _ body, Right (TForallRef _ _ bodyTy))
                                    | alphaEqType bodyTy schemeTy ->
                                        body
                                  _ -> stripUnusedTopTyAbsWithEnv tcEnv rhsAbs0 of
                                  rhsAbsCandidate ->
                                    case typeCheckLet rhsAbsCandidate of
                                      Right rhsTy
                                        | alphaEqType rhsTy schemeTy ->
                                            rhsAbsCandidate
                                      _ ->
                                        case rhsAbs0Ty of
                                          Left _ -> rhsAbsCandidate
                                          _ -> rhsAbs0
                          else
                            case (rhsAbs0, rhsAbs0Ty) of
                              (ETyAbsRef _ _ body, Right (TForallRef _ _ bodyTy))
                                | alphaEqType bodyTy schemeTy ->
                                    body
                              _ -> stripUnusedTopTyAbsWithEnv tcEnv rhsAbs0
                      rhsAbsAligned =
                        let withTyAbs = addMissingLeadingTyAbsAlongType tcEnv schemeTy rhsAbsBase
                            aligned = alignLeadingLambdasToType schemeTy withTyAbs
                         in if localBinderIsDiscard binderDetails
                              then
                                let stripped = stripUnusedTopTyAbsWithEnv tcEnv aligned
                                 in case typeCheckLet stripped of
                                      Right _ -> stripped
                                      Left _ -> aligned
                              else aligned
                      rhsAbsBaseTy = typeCheckLet rhsAbsBase
                      rhsAbsAlignedTy = typeCheckLet rhsAbsAligned
                   in case rhsAbsBaseTy of
                        Right rhsTy
                          | alphaEqType rhsTy schemeTy -> rhsAbsBase
                        _ ->
                          case rhsAbsAlignedTy of
                            Right rhsTy
                              | alphaEqType rhsTy schemeTy -> rhsAbsAligned
                            _ -> rhsAbsBase
                rhsAbsTyChecked = typeCheckLet rhsAbs
            case debugGeneralize
              ( "elaborate let("
                  ++ v
                  ++ "): scheme="
                  ++ show effectiveScheme
                  ++ " subst="
                  ++ show effectiveSubst
                  ++ " rhsAbs="
                  ++ show rhsAbs
                  ++ " rhsAbsTy="
                  ++ show rhsAbsTyChecked
              )
              () of
              () -> pure ()
            let effectiveRhsTyForBody = typeCheckForBody effectiveRhsTerm
                rhsAbsTyForBody = typeCheckForBody rhsAbs
            let bodyEnv =
                  case rhsAliasOverride of
                    Just (_, aliasInfo) -> insertEnvBinding (envBindingFor aliasInfo) env
                    Nothing ->
                      insertEnvBinding
                        ( envBindingFor
                            ( case (effectiveRhsTerm, effectiveRhsTy) of
                                (EVarNode _, Right rhsTy)
                                  | not (alphaEqType rhsTy (schemeToType effectiveScheme)) ->
                                      schemeInfoFromRefSubst (schemeFromType rhsTy) effectiveSubst
                                _ -> effectiveSchemeInfo
                            )
                        )
                        env
            bodyElaboration <-
              case elabDetailed bodyOut bodyEnv of
                Right elaboration -> pure elaboration
                Left cause ->
                  Left
                    ( ValidationFailed
                        [ "let body failed under its constructed binding"
                        , "  binding: " ++ v
                        , "  effective scheme: " ++ show effectiveScheme
                        , "  effective substitution: " ++ show effectiveSubst
                        , "  cause: " ++ show cause
                        ]
                    )
            let body' = elaboratedTerm bodyElaboration
                bodyLocalGammaCertificates =
                  elaboratedLocalGammaConstructionCertificates bodyElaboration
                bodyCompilerExactResultBoundCertificates =
                  elaboratedCompilerExactResultBoundCertificates bodyElaboration
            rhsFinal <-
              case rhsAliasOverride of
                Just (aliasTerm, _) -> Right aliasTerm
                Nothing ->
                  case schemeToType effectiveScheme of
                    muTy@(TMuRef muRef muBody) ->
                      case effectiveRhsTyForBody of
                        Right rhsTy
                          | alphaEqType rhsTy muTy -> Right effectiveRhsTerm
                        _ ->
                          case rhsAbsTyForBody of
                            Right rhsTy
                              | alphaEqType rhsTy muTy -> Right rhsAbs
                            _ ->
                              let expectedBodyTy = substTypeCaptureRef muRef muTy muBody
                                  rhsRollAligned = alignLeadingLambdasToType expectedBodyTy rhsAbs
                                  candidates =
                                    [ (effectiveRhsTerm, effectiveRhsTyForBody),
                                      (rhsAbs, rhsAbsTyForBody),
                                      (rhsRollAligned, typeCheckForBody rhsRollAligned)
                                    ]
                                  -- A recursively prepared RHS can close a
                                  -- local bounded construction Gamma before
                                  -- reaching the recursive carrier, for
                                  -- example
                                  --
                                  --   forall (a >= Int). mu r. r -> Int -> a
                                  --
                                  -- The roll body is the explicit N
                                  -- specialization of that checked term, not
                                  -- the still-abstract forall.  Consume only
                                  -- leading bounded binders and re-check every
                                  -- generated ETyInst; unbounded choices remain
                                  -- unavailable at this boundary.
                                  candidateAtExpectedBody term checked =
                                    case checked of
                                      Right ty
                                        | alphaEqType ty expectedBodyTy ->
                                            Just term
                                      Right (TForallRef _ (Just _) _) ->
                                        let specialized = ETyInst term InstElim
                                         in candidateAtExpectedBody
                                              specialized
                                              (typeCheckForBody specialized)
                                      _ -> Nothing
                                  matchingBody =
                                    foldr
                                      ( \(candidateTerm, candidateTy) rest ->
                                          candidateAtExpectedBody
                                            candidateTerm
                                            candidateTy
                                            <|> rest
                                      )
                                      Nothing
                                      candidates
                               in case matchingBody of
                                    Just rollBody ->
                                      Right (ERoll muTy rollBody)
                                    Nothing ->
                                      Left
                                        ( PhiTranslatabilityError
                                            [ "ALetF: recursive scheme has no admissible roll body",
                                              "binding=" ++ v,
                                              "recursiveType=" ++ show muTy,
                                              "expectedBodyType=" ++ show expectedBodyTy,
                                              "candidateTypes=" ++ show (map snd candidates)
                                            ]
                                        )
                    _ -> Right rhsAbs
            let rhsFinalTy = typeCheckForBody rhsFinal
            case rhsFinalTy of
              Right _ -> pure ()
              Left rhsError ->
                Left
                  ( PhiInvariantError
                      ( unlines
                          [ "ALetF: finalized RHS is not typable under its published scheme"
                          , "binding=" ++ v
                          , "scheme=" ++ show effectiveScheme
                          , "substitution=" ++ show effectiveSubst
                          , "term=" ++ show rhsFinal
                          , "typecheck=" ++ show rhsError
                          ]
                      )
                  )
            let schemeFinal0 =
                  case rhsFinalTy of
                    Right rhsTy
                      | localBinderIsDiscard binderDetails ->
                          schemeFromType rhsTy
                      | annExprReferenceKey bodyAnn == Just binderKey,
                        lambdaAnn rhsAnn,
                        -- Recursive occurrences were elaborated against the
                        -- construction-time scheme.  Changing that scheme
                        -- here would retag variables underneath already
                        -- emitted roll/unroll computations.  Only a
                        -- non-recursive returned lambda may publish its
                        -- checked RHS type at this late boundary.
                        not (annContainsReference binderKey rhsAnn),
                        isNothing rhsOuterSourceScheme,
                        isNothing letResultSourceScheme,
                        isNothing schemeRootSourceScheme,
                        not (alphaEqType rhsTy (schemeToType effectiveScheme)) ->
                          schemeFromType rhsTy
                    _ ->
                      case rhsAliasOverride of
                        Just (_, aliasInfo) -> siScheme aliasInfo
                        Nothing -> effectiveScheme
                EnvFreeTypeBinderRefs ambientClosureRefs =
                  envFreeTypeBinderRefs env
            schemeFinal <-
              validateSchemeClosure
                ("ALetF binding " ++ show v)
                (ambientSchemeClosureAuthority ambientClosureRefs)
                schemeFinal0
            let
                finalTy = schemeToType schemeFinal
                finalResolved = resolvedLocalBinder binderDetails finalTy
                rhsFinal' = refreshLocalResolvedVarType finalResolved finalTy rhsFinal
                body'' = refreshLocalResolvedVarType finalResolved finalTy body'
            pure
              ( schemeFinal,
                rhsFinal',
                body'',
                rhsLocalGammaCertificates ++ bodyLocalGammaCertificates,
                elaboratedOwnerFinalConstruction bodyElaboration,
                rhsCompilerExactResultBoundCertificates
                  ++ bodyCompilerExactResultBoundCertificates
              )
          unusedIdentityWrapperBinding =
            not (annContainsReference binderKey bodyAnn) && identityWrapperAnn rhsAnn
          f env =
            withLetConstructionGammaDetailed env $ \letConstructionScope letConstructionRequirements constructionAliases envForLet ->
              if unusedIdentityWrapperBinding
                then elabDetailed bodyOut envForLet
                else do
                  ( scheme
                    , rhsFinal
                    , body'
                    , localGammaCertificates
                    , bodyOwnerFinalConstruction
                    , compilerExactResultBoundCertificates
                    ) <-
                    elaborateLet
                      letConstructionScope
                      letConstructionRequirements
                      constructionAliases
                      envForLet
                  let finalResolved = resolvedLocalBinder binderDetails (schemeToType scheme)
                      completedLet
                        | isJust (annExprReferenceKey rhsAnn)
                        , not (containsFreeVar finalResolved body') = body'
                        | otherwise = mkLocalLet binderDetails scheme rhsFinal body'
                  pure
                    ElaboratedTerm
                      { elaboratedTerm = completedLet,
                        -- Let is result-transparent.  Preserve the exact
                        -- child constructor until the surrounding let Gamma
                        -- proves that it emitted a binder of its own; the
                        -- outer wrapper will replace this certificate only in
                        -- that case.
                        elaboratedOwnerFinalConstruction =
                          bodyOwnerFinalConstruction,
                        elaboratedLocalGammaConstructionCertificates =
                          localGammaCertificates,
                        elaboratedCompilerExactResultBoundCertificates =
                          compilerExactResultBoundCertificates
                      }
          fStripped env =
            withLetConstructionGamma env $ \letConstructionScope letConstructionRequirements constructionAliases envForLet ->
              if unusedIdentityWrapperBinding
                then elabTerm bodyOut envForLet
                else do
                  ( scheme
                    , rhsFinal
                    , body'
                    , _localGammaCertificates
                    , _bodyOwnerFinalConstruction
                    , _compilerExactResultBoundCertificates
                    ) <-
                    elaborateLet
                      letConstructionScope
                      letConstructionRequirements
                      constructionAliases
                      envForLet
                  let finalResolved = resolvedLocalBinder binderDetails (schemeToType scheme)
                  if isJust (annExprReferenceKey rhsAnn) && not (containsFreeVar finalResolved body')
                    then pure body'
                    else
                      if containsFreeVar finalResolved rhsFinal
                        then pure (mkLocalLet binderDetails scheme rhsFinal body')
                        else pure body'
       in ElabOut
            { elabDetailed = f,
              elabStripped = fStripped
            }
    AExactAnnF (_exprAnn, exprOut) _exactTy annNodeId eid ->
      ElabOut
        { elabDetailed = \env -> do
            expectedTy <-
              case IntMap.lookup (getEdgeId eid) (algExactProducerTypes algebraContext) of
                Just ty -> pure ty
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact annotation has no prepared contract"
                        , "  edge: " ++ show eid
                        ]
                    )
            edgeConstructionRefs <-
              case IntMap.lookup (getEdgeId eid) (algCompilerExactConstructionRefs algebraContext) of
                Just refs -> pure refs
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact annotation has no prepared construction plan"
                        , "  edge: " ++ show eid
                        ]
                    )
            envInExactIdentityDomain <-
              alignEnvToCompilerExactBinderRenames
                ( compilerExactBinderRenamesForEdge
                    eid
                    (algSubtermGeneralizations algebraContext)
                )
                env
            envForExact <-
              installCompilerExactConstructionRefs
                eid
                (algSubtermGeneralizations algebraContext)
                edgeConstructionRefs
                envInExactIdentityDomain
            exprElaboration <-
              elabDetailed
                exprOut
                envForExact
                  { envExpectedTermEndpoint =
                      Just (ExactConstructionExpectedTerm expectedTy)
                  }
            let expr' = elaboratedTerm exprElaboration
            -- The prepared packet fixes staging as well as ownership.  A
            -- result binder already present in the exact source ABI must be
            -- constructed before validation.  A packet-only result is
            -- constructed afterwards, so the exact boundary validates the
            -- operated producer rather than immediately specializing away
            -- the packet's newly published Hyp result.
            sourceCompletedTerm <-
              completeCompilerExactSubtermResultsWithBounds
                (elaboratedCompilerExactResultBoundCertificates exprElaboration)
                CompleteBeforeCompilerExact
                eid
                (algSubtermGeneralizations algebraContext)
                expr'
            exactTerm <-
              elaborateExactAnnotationTerm
                ( case envApplicationSourceOccurrence env of
                    Just ApplicationArgumentOccurrence ->
                      AnnotationApplicationArgumentBoundary
                    _ -> AnnotationProducerBoundary
                )
                annotationContext
                (typeCheckEnvFrom envForExact)
                expectedTy
                edgeConstructionRefs
                annNodeId
                eid
                sourceCompletedTerm
            completedTerm <-
              completeCompilerExactSubtermResultsWithBounds
                (elaboratedCompilerExactResultBoundCertificates exprElaboration)
                CompleteAfterCompilerExact
                eid
                (algSubtermGeneralizations algebraContext)
                exactTerm
            case TypeCheck.typeCheckWithEnv (typeCheckEnvFrom envForExact) completedTerm of
              Right _ ->
                pure
                  ElaboratedTerm
                    { elaboratedTerm = completedTerm,
                      elaboratedOwnerFinalConstruction = Nothing,
                      elaboratedLocalGammaConstructionCertificates =
                        elaboratedLocalGammaConstructionCertificates exprElaboration,
                      elaboratedCompilerExactResultBoundCertificates =
                        elaboratedCompilerExactResultBoundCertificates exprElaboration
                    }
              Left checkError ->
                Left
                  ( ValidationFailed
                      [ "compiler exact completion is not typable in its prepared Gamma"
                      , "  exact edge: " ++ show eid
                      , "  error: " ++ show checkError
                      , "  pending results: "
                          ++ show
                            [ ( owner
                              , subtermGeneralizationCompilerExactResultRef packet
                              , subtermGeneralizationCompilerExactCompletionRef packet
                              )
                            | (owner, packet) <- Map.toList (algSubtermGeneralizations algebraContext)
                            , subtermGeneralizationCompilerExactBoundary packet == Just eid
                            ]
                      , "  Gamma refs: " ++ show (Map.keys (envTypeBindings envForExact))
                      ]
                  ),
          elabStripped = \env -> do
            edgeConstructionRefs <-
              case IntMap.lookup (getEdgeId eid) (algCompilerExactConstructionRefs algebraContext) of
                Just refs -> pure refs
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "compiler exact annotation has no prepared construction plan"
                        , "  edge: " ++ show eid
                        ]
                    )
            envForExact <-
              installCompilerExactConstructionRefs
                eid
                (algSubtermGeneralizations algebraContext)
                edgeConstructionRefs
                env
            elabTerm exprOut envForExact
        }
    AAnnF (exprAnn, exprOut) annNodeId eid ->
      ElabOut
        { elabDetailed = \env -> do
            annotationSourceTy <-
              case
                  IntMap.lookup
                    (getEdgeId eid)
                    (acAnnotationExpectedTypesByEdge annotationContext)
                of
                  Just ty ->
                    pure
                      ( applyTypeVarRefRenames
                          (envConstructionBinderRenames env)
                          ty
                      )
                  Nothing ->
                    Left
                      ( ValidationFailed
                          [ "source annotation has no prepared construction type"
                          , "  edge: " ++ show eid
                          ]
                      )
            -- A source annotation guides a lambda that cannot synthesize its
            -- own polymorphic parameter/result spine.  An inferable producer
            -- (notably an application) must instead be elaborated at the type
            -- selected by its graph edge, and only then coerced by this
            -- annotation edge.  Pre-entering the target forall for
            -- @omega id : forall a. a -> a@ would construct the pointwise
            -- @Lambda a. omega[forall(>a); N] id@ instead of the paper's
            -- canonical explicit-bound elimination @omega[N] id@.
            let annotationSourceScheme =
                  sourceAnnotationSchemeAgainstEnv env annotationSourceTy
                annotationChildEnv =
                  case
                      ( lambdaAnn exprAnn
                      , null (schemeBinderRefs annotationSourceScheme)
                      )
                    of
                    (True, _) ->
                      extendEnvTypeScope
                        (schemeBinderRefs annotationSourceScheme)
                        env
                          { envExpectedTermEndpoint =
                              Just
                                ( ExactConstructionExpectedTerm
                                    (schemeBody annotationSourceScheme)
                                )
                          }
                    (False, True) ->
                      -- A monomorphic source annotation is the exact endpoint
                      -- for an inferable producer as well. This fixes the
                      -- result parameter of applications such as
                      -- @__io_bind ... : IO Unit@ during construction.
                      env
                        { envExpectedTermEndpoint =
                            Just
                              ( ExactConstructionExpectedTerm
                                  annotationSourceTy
                              )
                        }
                    (False, False) ->
                      -- Quantified inferable producers must synthesize before
                      -- the annotation edge computes them to the source
                      -- scheme. Entering that scheme here would turn
                      -- @omega[N] id@ into a pointwise forall.
                      env
                        { envExpectedTermEndpoint = Nothing
                        }
            exprElaboration <-
              elabDetailed
                exprOut
                annotationChildEnv
            annotatedTerm <-
              elaborateAnnotationTerm
                ( case envApplicationSourceOccurrence env of
                    Just ApplicationArgumentOccurrence ->
                      AnnotationApplicationArgumentBoundary
                    _ -> AnnotationProducerBoundary
                )
                annotationContext
                namedSetReify
                (\details -> ebSchemeInfo <$> lookupEnvBindingForDetails details env)
                (typeCheckEnvFrom annotationChildEnv)
                (envConstructionBinderRenames annotationChildEnv)
                (envConstructionIdentityRoutes annotationChildEnv)
                exprAnn
                annNodeId
                eid
                (elaboratedTerm exprElaboration)
            pure
              ElaboratedTerm
                { elaboratedTerm = annotatedTerm,
                  elaboratedOwnerFinalConstruction = Nothing,
                  elaboratedLocalGammaConstructionCertificates =
                    elaboratedLocalGammaConstructionCertificates exprElaboration,
                  elaboratedCompilerExactResultBoundCertificates =
                    elaboratedCompilerExactResultBoundCertificates exprElaboration
                },
          elabStripped = \env -> elabTerm exprOut env
        }
    ALetScopeF (_exprAnn, exprOut) _resultNodeId _eid -> exprOut
    AUnfoldF (_exprAnn, exprOut) _unfoldNodeId _eid ->
      ElabOut
        { elabDetailed = \env -> do
            exprElaboration <- elabDetailed exprOut env
            pure
              ElaboratedTerm
                { elaboratedTerm = EUnroll (elaboratedTerm exprElaboration),
                  elaboratedOwnerFinalConstruction = Nothing,
                  elaboratedLocalGammaConstructionCertificates =
                    elaboratedLocalGammaConstructionCertificates exprElaboration,
                  elaboratedCompilerExactResultBoundCertificates =
                    elaboratedCompilerExactResultBoundCertificates exprElaboration
                },
          elabStripped = \env -> do
            expr' <- elabTerm exprOut env
            pure (EUnroll expr')
        }
  where
    annotationContext = algAnnotationContext algebraContext
    scopeContext = acScopeContext annotationContext
    canonical = algCanonical algebraContext
    namedSetReify = algNamedSetReify algebraContext
    compilerExactBinderRenamesForEdge exactEdge packets =
      concat
        [ subtermGeneralizationCompilerExactBinderRenames packet
        | packet <- Map.elems packets
        , subtermGeneralizationCompilerExactBoundary packet == Just exactEdge
        ]
    lambdaAnn annExpr =
      case stripAnnExpr annExpr of
        ALam {} -> True
        _ -> False

    identityWrapperAnn annExpr =
      case annExpr of
        ALam _param mbDetails _ _ body _ _ ->
          annExprReferenceKey body == Just (annBinderKey mbDetails)
        AAnn inner _ _ -> identityWrapperAnn inner
        AExactAnn inner _ _ _ -> identityWrapperAnn inner
        ALetScope inner _ _ -> identityWrapperAnn inner
        AUnfold inner _ _ -> identityWrapperAnn inner
        _ -> False

    unfoldMuOnce :: ElabType -> Maybe ElabType
    unfoldMuOnce muTy =
      case muTy of
        TMuRef ref body -> Just (substTypeCaptureRef ref muTy body)
        _ -> Nothing

    containsFreeVar :: LocalVarKey -> XmlfTerm -> Bool
    containsFreeVar v term =
      case term of
        EVarNode resolved -> localVarKeyMatchesReference v resolved
        ELit _ -> False
        ELam resolved body
          | localVarKeyMatchesReference v resolved -> False
          | otherwise -> containsFreeVar v body
        EApp f a -> containsFreeVar v f || containsFreeVar v a
        ELet resolved _ rhs body
          | localVarKeyMatchesReference v resolved -> containsFreeVar v rhs
          | otherwise -> containsFreeVar v rhs || containsFreeVar v body
        ETyAbsRef _ _ body -> containsFreeVar v body
        ETyInst e _ -> containsFreeVar v e
        ERoll _ body -> containsFreeVar v body
        EUnroll e -> containsFreeVar v e

    restrictTypeCheckEnvToFreeTermBindings :: XmlfTerm -> TypeCheck.Env -> TypeCheck.Env
    restrictTypeCheckEnvToFreeTermBindings term env =
      TypeCheck.restrictResolvedTermBindings
        [ resolved
        | (resolved, _) <-
            TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv env)
        , containsFreeVar resolved term
        ]
        env

    alignLeadingLambdasToType :: ElabType -> XmlfTerm -> XmlfTerm
    alignLeadingLambdasToType ty term =
      case (ty, term) of
        (TForallRef targetRef _ bodyTy, ETyAbsRef termRef mb body) ->
          let bodyTy' = substTypeCaptureRef targetRef (TVarRef termRef) bodyTy
           in ETyAbsRef termRef mb (alignLeadingLambdasToType bodyTy' body)
        (TArrow dom cod, ELam resolved body) ->
          let body' = alignLeadingLambdasToType cod body
           in ELam (mapResolvedVarType (const dom) resolved) (refreshLocalResolvedVarType resolved dom body')
        _ -> term

    stripUnusedTopTyAbsWithEnv :: TypeCheck.Env -> XmlfTerm -> XmlfTerm
    stripUnusedTopTyAbsWithEnv tcEnv term =
      case term of
        ETyAbsRef ref mbBound body ->
          let body' = stripUnusedTopTyAbsWithEnv tcEnv body
              term' = ETyAbsRef ref mbBound body'
           in if any (typeBinderRefsSameIdentity ref) (Reduce.freeTypeVarRefsTerm body')
                then term'
                else body'
        -- Administrative lets do not change the result-position ABI.  A
        -- vacuous type abstraction in their tail is still a leading result
        -- abstraction, and must be removed before an enclosing lambda fixes
        -- its arrow codomain.  Waiting for the whole let to type check creates
        -- a cycle: the enclosing application cannot type check until this
        -- result abstraction has been placed correctly.
        ELet resolved scheme rhs body ->
          ELet resolved scheme rhs (stripUnusedTopTyAbsWithEnv tcEnv body)
        _ -> term

    addMissingLeadingTyAbsAlongType :: TypeCheck.Env -> ElabType -> XmlfTerm -> XmlfTerm
    addMissingLeadingTyAbsAlongType tcEnv targetTy term =
      let initialReserved =
            Set.unions
              ( Set.union (typeAbsNamesInTerm term) (typeVarNamesInTerm term)
                  : map freeTypeVarsType (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv tcEnv)))
                  ++ [Set.fromList (map typeBinderRefName (Map.keys (TypeCheck.typeEnv tcEnv))), forallBinderNames targetTy]
              )
       in go initialReserved targetTy term
      where
        go reserved targetTy' term' =
          case targetTy' of
            TForallRef targetRef mbBound targetBody ->
              case stripUnusedTopTyAbsWithEnv tcEnv term' of
                ETyAbsRef termRef termBound body
                  | typeBinderRefsSameIdentity targetRef termRef ->
                      ETyAbsRef termRef termBound (go (Set.insert (typeBinderRefName termRef) reserved) targetBody body)
                term'' ->
                  let targetName = typeBinderRefName targetRef
                      (targetRef', targetBody') =
                        if Set.member targetName reserved
                          then
                            let fresh = freshNameLike targetName reserved
                                freshTargetRef = renameTypeBinderRef fresh targetRef
                             in (freshTargetRef, substTypeCaptureRef targetRef (TVarRef freshTargetRef) targetBody)
                          else (targetRef, targetBody)
                      reserved' = Set.insert (typeBinderRefName targetRef') reserved
                      body' = go reserved' targetBody' term''
                   in ETyAbsRef targetRef' mbBound body'
            TArrow dom cod ->
              case term' of
                ELam resolved body ->
                  let body' = go reserved cod body
                   in ELam (mapResolvedVarType (const dom) resolved) (refreshLocalResolvedVarType resolved dom body')
                _ -> term'
            _ -> term'

        forallBinderNames ty =
          case ty of
            TForallRef ref _ body -> Set.insert (typeBinderRefName ref) (forallBinderNames body)
            _ -> Set.empty

    typeAbsNamesInTerm :: XmlfTerm -> Set.Set String
    typeAbsNamesInTerm term =
      case term of
        ETyAbsRef ref _ body -> Set.insert (typeBinderRefName ref) (typeAbsNamesInTerm body)
        ELam _ body -> typeAbsNamesInTerm body
        EApp f a -> Set.union (typeAbsNamesInTerm f) (typeAbsNamesInTerm a)
        ELet _ _ rhs body -> Set.union (typeAbsNamesInTerm rhs) (typeAbsNamesInTerm body)
        ETyInst body _ -> typeAbsNamesInTerm body
        ERoll _ body -> typeAbsNamesInTerm body
        EUnroll body -> typeAbsNamesInTerm body
        _ -> Set.empty

    typeVarNamesInTerm :: XmlfTerm -> Set.Set String
    typeVarNamesInTerm term =
      case term of
        ETyAbsRef ref mb body ->
          Set.insert (typeBinderRefName ref) (maybe Set.empty freeTypeVarsType mb `Set.union` typeVarNamesInTerm body)
        ELam resolved body -> Set.union (freeTypeVarsType (resolvedVarType resolved)) (typeVarNamesInTerm body)
        EApp f a -> Set.union (typeVarNamesInTerm f) (typeVarNamesInTerm a)
        ELet _ sch rhs body -> Set.unions [freeTypeVarsType (schemeToType sch), typeVarNamesInTerm rhs, typeVarNamesInTerm body]
        ETyInst body inst -> Set.union (typeVarNamesInTerm body) (goInst inst)
        ERoll ty body -> Set.union (freeTypeVarsType ty) (typeVarNamesInTerm body)
        EUnroll body -> typeVarNamesInTerm body
        _ -> Set.empty
      where
        goInst inst =
          case inst of
            InstId -> Set.empty
            InstApp ty -> freeTypeVarsType ty
            InstIntro -> Set.empty
            InstElim -> Set.empty
            InstInside inner -> goInst inner
            InstSeq a b -> Set.union (goInst a) (goInst b)
            InstUnderRef _ inner -> goInst inner
            InstBot ty -> freeTypeVarsType ty
            InstAbstrRef _ -> Set.empty

    annContainsReference :: BindingKey -> AnnExpr -> Bool
    annContainsReference key annExpr =
      case annExpr of
        ALit _ _ -> False
        AResolvedVar details _ _ -> ResolvedBindingKey (idDetailsIdentityKey details) == key
        ALam _name mbDetails _ _ body _ _
          | annBinderKey mbDetails == key -> False
          | otherwise -> annContainsReference key body
        AApp fun arg _ _ _ -> annContainsReference key fun || annContainsReference key arg
        ALet _name mbDetails _ _ _ _ rhs body _
          | annBinderKey mbDetails == key -> annContainsReference key rhs
          | otherwise -> annContainsReference key rhs || annContainsReference key body
        AAnn inner _ _ -> annContainsReference key inner
        AExactAnn inner _ _ _ -> annContainsReference key inner
        ALetScope inner _ _ -> annContainsReference key inner
        AUnfold inner _ _ -> annContainsReference key inner

    blockedAliasMuType :: ElabType -> Maybe ElabType
    blockedAliasMuType ty =
      case ty of
        TForallRef ref Nothing (TArrow (TVarRef domRef) cod)
          | typeBinderRefsSameIdentity ref domRef -> Just (TMuRef ref (TArrow (TVarRef ref) cod))
        _ -> Nothing

plainElaboration
  :: (Env -> Either ElabError XmlfTerm)
  -> Env
  -> Either ElabError ElaboratedTerm
plainElaboration f env =
  (\term -> ElaboratedTerm term Nothing [] []) <$> f env

-- | Record only ambient identities that the completed owner actually uses.
-- Free refs in the term cover type applications and computations that may be
-- absent from the result type; free refs in the checked type cover the owner
-- result itself.  Closing through ambient bounds preserves indirect
-- dependencies such as the lexical parameter in K, while locally emitted
-- abstractions are excluded by construction.
ownerFinalAmbientBinderRefs
  :: TypeCheck.Env
  -> [TypeBinderRef]
  -> XmlfTerm
  -> ElabType
  -> [TypeBinderRef]
ownerFinalAmbientBinderRefs typeEnv localRefs term constructedTy =
  close initialRefs
  where
    initialRefs =
      distinctRefs
        ( filter (not . isLocalRef)
            ( Reduce.freeTypeVarRefsTerm term
                ++ freeTypeVarRefsType constructedTy
            )
        )

    close refs =
      let dependencies =
            [ dependency
            | ref <- refs
            , Just bound <- [TypeCheck.lookupTypeBindingRef ref typeEnv]
            , dependency <- freeTypeVarRefsType bound
            , not (isLocalRef dependency)
            ]
          refs' = distinctRefs (refs ++ dependencies)
       in if length refs' == length refs then refs else close refs'

    isLocalRef ref =
      any (typeBinderRefsSameIdentity ref) localRefs

    distinctRefs = foldr insertDistinct []
    insertDistinct ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

mkOut :: (Env -> Either ElabError XmlfTerm) -> ElabOut
mkOut f = ElabOut (plainElaboration f) f

resolvedLambdaParamNode :: (NodeId -> NodeId) -> (NodeId -> Maybe TyNode) -> NodeId -> Maybe NodeId
resolvedLambdaParamNode canonical lookupNode lamNodeId =
  let lamC = canonical lamNodeId
   in case lookupNode lamC of
        Just TyArrow {tnDom = dom} -> Just dom
        Just TyVar {tnBound = Just bnd} ->
          case lookupNode (canonical bnd) of
            Just TyArrow {tnDom = dom} -> Just dom
            _ -> Nothing
        _ -> Nothing

{- Note [srcTypeToElabType in Algebra]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Local copy of the NormSrcType → ElabType conversion.  The canonical copy lives
in MLF.Frontend.Program.Elaborate but is not exported (production surface is
kept narrow).  We need this conversion in ALamF to recover the original source
annotation type that presolution may have stripped (e.g. TForall inside a μ
body).  Keeping it local avoids widening a production facade for a single
internal consumer.
-}

-- | Convert a normalized source type to its elaboration-level equivalent.
srcTypeToElabType :: AlgebraContext p -> NormSrcType -> Either ElabError ElabType
srcTypeToElabType algebraContext ty =
  let (refs, generator) =
        sourceTypeBinderRefsFromIdentities
          (algSourceTypeBinderIdentities algebraContext)
          (Set.toList (freeSrcTypeVars ty))
          (sourceTypeIdentityGenerator algebraContext ty)
   in fmap fst (srcTypeToElabTypeWith algebraContext refs generator ty)

sourceTypeIdentityGenerator :: AlgebraContext p -> NormSrcType -> IdentityGenerator
sourceTypeIdentityGenerator algebraContext ty =
  identityGeneratorAfter
    ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
        ++ concatMap typeBinderGeneratedIdentities (Map.elems (algSourceTypeBinderIdentities algebraContext))
    )
  where
    headIdentities =
      Map.union
        (algSourceTypeHeadIdentities algebraContext)
        (Builtins.builtinSourceTypeHeadIdentities ty)

freeSrcTypeVars :: SrcTy n v -> Set.Set String
freeSrcTypeVars ty =
  go Set.empty ty
  where
    go :: Set.Set String -> SrcTy n0 v0 -> Set.Set String
    go bound srcTy =
      case srcTy of
        STVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go bound dom `Set.union` go bound cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go bound) args
        STVarApp name args ->
          let headVars =
                if name `Set.member` bound
                  then Set.empty
                  else Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STTyLam name body -> go (Set.insert name bound) body
        STTyApp fun arg -> go bound fun `Set.union` go bound arg
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body -> go (Set.insert name bound) body
        STBottom -> Set.empty

srcTypeToElabTypeWith :: AlgebraContext p -> Map.Map String TypeBinderRef -> IdentityGenerator -> NormSrcType -> Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWith =
  srcTypeToElabTypeWithBound Set.empty

srcTypeToElabTypeWithBound ::
  Set.Set String ->
  AlgebraContext p ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithBound boundNames algebraContext refs generator ty = case ty of
  STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (TVarRef ref, generator)
  STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames algebraContext refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames algebraContext refs generator1 cod
    Right (TArrow dom' cod', generator2)
  STBase name -> do
    identity <- sourceTypeHeadIdentity name
    Right (TBaseWithIdentity identity (builtinBaseTy name), generator)
  STCon name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    identity <- sourceTypeHeadIdentity name
    Right (TConWithIdentity identity (builtinBaseTy name) args', generator')
  STVarApp name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    ref <- sourceTypeBinderRef refs name
    Right (TVarAppRef ref args', generator')
  STTyLam {} ->
    Left (InstantiationError "residual type lambda reached elaboration")
  STTyApp {} ->
    Left (InstantiationError "residual type application reached elaboration")
  STForall name mb body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) (algSourceTypeBinderIdentities algebraContext) name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWith boundNames refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs' generator2 body
          Right (TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) (algSourceTypeBinderIdentities algebraContext) name generator
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithBound boundNames' algebraContext (Map.insert name ref refs) generator1 body
          Right (TMuRef ref body', generator2)
  STBottom -> Right (TBottom, generator)
  where
    sourceTypeHeadIdentity name =
      case lookupSymbolIdentityAlias (algSourceTypeHeadIdentities algebraContext) name <|> Builtins.builtinTypeHeadIdentity name of
        Just identity -> Right identity
        Nothing -> Left (InstantiationError ("unresolved source type head `" ++ name ++ "` reached algebra elaboration"))

    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InstantiationError ("unresolved source type binder `" ++ name ++ "` reached algebra elaboration"))

    srcTypesToElabTypesWith boundNames' refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWithBound boundNames' algebraContext refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

    srcBoundToElabBoundWith :: Set.Set String -> Map.Map String TypeBinderRef -> IdentityGenerator -> SrcBound 'NormN -> Either ElabError (Maybe BoundType, IdentityGenerator)
    srcBoundToElabBoundWith boundNames' refs' generator0 (SrcBound boundTy) = structBoundToElabBoundWith boundNames' refs' generator0 boundTy

    structBoundToElabBoundWith :: Set.Set String -> Map.Map String TypeBinderRef -> IdentityGenerator -> StructBound -> Either ElabError (Maybe BoundType, IdentityGenerator)
    structBoundToElabBoundWith boundNames' refs' generator0 bTy = case bTy of
      STArrow dom cod -> do
        (dom', generator1) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs' generator0 dom
        (cod', generator2) <- srcTypeToElabTypeWithBound boundNames' algebraContext refs' generator1 cod
        Right (Just (TArrow dom' cod'), generator2)
      STBase name -> do
        identity <- sourceTypeHeadIdentity name
        Right (Just (TBaseWithIdentity identity (builtinBaseTy name)), generator0)
      STCon name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        identity <- sourceTypeHeadIdentity name
        Right (Just (TConWithIdentity identity (builtinBaseTy name) args'), generator1)
      STVarApp name args -> do
        (args', generator1) <- srcTypesToElabTypesWith boundNames' refs' generator0 args
        ref <- sourceTypeBinderRef refs' name
        Right (Just (TVarAppRef ref args'), generator1)
      STTyLam {} ->
        Left (InstantiationError "residual type lambda reached elaboration")
      STTyApp {} ->
        Left (InstantiationError "residual type application reached elaboration")
      STForall name mb body ->
        let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames') (algSourceTypeBinderIdentities algebraContext) name generator0
            refs'' = Map.insert name ref refs'
            boundNames'' = Set.insert name boundNames'
         in do
              (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWith boundNames' refs' generator1) mb
              (body', generator3) <- srcTypeToElabTypeWithBound boundNames'' algebraContext refs'' generator2 body
              Right (Just (TForallRef ref mb' body'), generator3)
      STMu name body ->
        let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames') (algSourceTypeBinderIdentities algebraContext) name generator0
            boundNames'' = Set.insert name boundNames'
         in do
              (body', generator2) <- srcTypeToElabTypeWithBound boundNames'' algebraContext (Map.insert name ref refs') generator1 body
              Right (Just (TMuRef ref body'), generator2)
      STBottom -> Right (Nothing, generator0)

builtinBaseTy :: String -> BaseTy
builtinBaseTy =
  BaseTy . Builtins.normalizeBuiltinTypeReference

annNode :: AnnExpr -> NodeId
annNode ann =
  case ann of
    ALit _ nid -> nid
    AResolvedVar _ _ nid -> nid
    ALam _ _ _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid
    AExactAnn _ _ nid _ -> nid
    ALetScope _ nid _ -> nid
    AUnfold _ nid _ -> nid
