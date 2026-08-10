{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Generalize.Prepare.Internal (
    PreparedGeneralizationArtifact(..),
    PreparedRootGeneralization(..),
    preparedRootCertifiedTermBinderRenames,
    PreparedRootClosure(..),
    preparedRootClosureScheme,
    preparedRootClosureAmbientBinderRefs,
    prepareRootClosureScheme,
    prepareRootClosureSchemeWithAmbient,
    PreparedRootConstructionScope,
    preparedRootConstructionScopeBinders,
    preparedRootConstructionScopeAliases,
    preparedRootConstructionScopeBinderRenames,
    preparedRootConstructionScopeLocalGammaClosures,
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    withPreparedResolvedTermSchemes,
    prepareElaborationExpansionConstructionPlacements,
    identityTopologyAncestryFailures,
    exactApplicationClosureOwnsRequirement,
    applicationCertificateOwnsRootRequirement,
    applicationCertificateOwnsAmbientRootRequirement,
    applicationCertificateDirectClaimOwnsPlanningRequirement,
    applicationCertificateCompletesProvisionalResultRequirement,
    applicationCertificateCompletesExactResultRequirement,
    applicationCertificateTransfersRootRequirementOwnership,
    applicationCertificateDischargesRootClosure,
    applicationCertificateDischargesLocalGammaClosure,
    rootRequirementOwnershipAllowsLocalGammaClosure,
    validateLocalApplicationCertificates,
    unclaimedEdgesOutsideLocalGammaClosures,
    placeNestedRootRequirements,
    preparedAnnotated,
    authorizePreparedAnn,
    selectPreparedRootScopeAuthority,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    preparedIdentityGenerator,
    applyPreparedTermSourceBinderAliases,
    preparedCompilerExactSourceResultBinderRoutes,
    insertPreparedTermSourceBinderAlias,
    completePreparedCompilerExactSubtermResults,
    preparedCompilerExactExpectedType,
    preparedCompilerExactDeclarationRefs,
    preparedElaborationConfig,
    preparedElaborationEnv,
    preparedElaborationEnvWithInitialEnv,
    stripPreparedWitnesslessAuthoritativeAnn,
    generalizePreparedRoot,
    generalizePreparedRootDetailed,
    generalizePreparedRootDetailedWithConstructionAnn,
    generalizePreparedRootDetailedWithConstructionResult,
    prepareOrdinaryRootConstructionScope,
    prepareRootConstructionScope,
    prepareRootConstructionScopeWithRequirementEvidence,
    requiredGammaBinderClosedLocally,
    requiredGammaBinderConstructionRef,
    applyPreparedRootSourceTypeBinderIdentities,
    applyPreparedRootBinderIdentities,
    applyPreparedCompilerExactRootBinderIdentities,
    prepareCompilerExactRootBinderSubst,
    applyPreparedRootBinderSubst,
    quotientPreparedRootClosureIdentities,
    projectPreparedRootFreeSourceDeclarationCopies,
    reconcileRootSourceBinderAliases,
    projectPreparedSourceBinderSubstExceptWithLocalKeys,
    CompilerExactEdgePlan(..),
    prepareCompilerExactEdgePlans,
    prepareAnnotationExpectedTypesByEdge,
    alignSourceExpectedOperatedType,
    computePreparedResultType,
    computePreparedResultTypeWithRootGeneralization,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (find, minimumBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Control.Monad (filterM, foldM, guard, unless)
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Canonicalizer (Canonicalizer, canonicalizeNode)
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionPlanBuilder(..)
    )
import MLF.Constraint.Presolution.Base
    ( EdgeArtifacts
    , EdgeArtifactsError
    , PresolutionResult(..)
    , edgeArtifactExpansion
    , edgeArtifactTrace
    , edgeArtifactWitness
    , eaEdgeExpansionConstructions
    , eaEdgeTraces
    , eaEdgeWitnesses
    , mapEdgeArtifacts
    , lookupEdgeArtifact
    , getCopyMapping
    )
import MLF.Constraint.Presolution.Construction
    ( RawExpansionConstruction
    , rawExpansionConstructionArgumentKeys
    , rawExpansionConstructionParents
    , rawExpansionConstructionSemanticMetaKeys
    )
import MLF.Constraint.Presolution.Plan.Requirements
    ( AmbientGammaAuthority(..)
    , GeneralizationRequirements(..)
    , RequiredGammaBinder(..)
    , RequiredGammaPlacement(..)
    )
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Constraint.Presolution.Plan.Context
    ( SolvedToBaseResolution(..)
    , resolveGaSolvedToBase
    )
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Solve (SolveError)
import qualified MLF.Constraint.Solve as Solve
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindParents
    , Constraint
    , EdgeId(..)
    , GenNode(..)
    , GenNodeId(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(TyVar)
    , cBindParents
    , cNodes
    , getEdgeId
    , getNodeId
    , genRef
    , lookupNodeIn
    , nodeRefKey
    , nodeRefFromKey
    , toListNode
    , toPresolvedConstraint
    , typeRef
    )
import MLF.Constraint.Types.Phase (Phase(Acyclic, Presolved))
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
import MLF.Constraint.Types.Witness
    ( Expansion(..)
    , ReplayContract(..)
    , ewForallIntros
    , ewLeft
    , ewRight
    , ewRoot
    , ewWitness
    , getInstanceOps
    )
import MLF.Elab.Elaborate (ElabConfig(..), ElabEnv(..))
import MLF.Elab.Elaborate.Algebra
    ( CompilerExactResultBoundCertificate
    , Env
    , OwnerFinalConstruction(..)
    , ofcCarriedResultBinderRefs
    , ofcConstructedBinderSpine
    , ofcConstructedBinderRoutes
    , ofcLocallyEmittedBinderRefs
    , completeCompilerExactSubtermResultsWithBounds
    , mkEnv
    , renameOwnerFinalConstructionBinderRefPayloads
    )
import MLF.Elab.Elaborate.Annotation
    ( AuthorizedElaborationRoot
    , ElaborationEdgeAuthority
    , annBinderKey
    , annExprReferenceKey
    , authorizedElaborationRoots
    , desugaredAnnLambdaInfo
    , elaborationAnnotationExpectedTypesByEdge
    , elaborationEdgeArtifacts
    , mkElaborationEdgeAuthority
    )
import MLF.Elab.Elaborate.Algebra.ConstructionGamma
    ( bodyConsumerBoundRefinementCompletedTopologyEndpoint
    , bodyConsumerBoundRefinementConsumedDependencies
    , bodyConsumerBoundRefinementConsumesAny
    , bodyConsumerBoundRefinementTargetsAny
    , completeUnboundedForallSpecializesTo
    , exactIdentityForallClosureOf
    , operationalEndpointTypesAgree
    , projectCertifiedBodyConsumerBoundsIfPresent
    , projectCertifiedBodyConsumerRootScheme
    )
import MLF.Elab.Generalize
    ( CompilerExactResultStage(..)
    , administrativeLambdaBody
    , GaBindParents(..)
    , gaConstructionRouteNodes
    , GammaPacketAuthority(..)
    , LocalGammaConstructor(..)
    , LocalGammaEdgeOwnership(..)
    , LocalGammaClosure(..)
    , LocalGammaOwner(..)
    , PreparedSubtermGeneralization
    , RootEdgeExactEndpoint(..)
    , RootRaiseMergeAuthority(..)
    , SubtermPacketPlacement(..)
    , SubtermGeneralizations
    , SubtermResultOwnership
    , mergeSubtermGeneralizations
    , pairSubtermGeneralizationRoots
    , placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy
    , placedSubtermBinderScheme
    , placedSubtermCopiedBinderRoutes
    , placedSubtermConstructedConsumerIdentities
    , publishPlacedSubtermConstructionBinderOrder
    , publishRootRaiseMergePacketResultRoute
    , publishTopologyConsumerRoutes
    , prepareSubtermGeneralizationPacket
    , prepareRootRaiseMergeScheme
    , prepareRootRaiseMergeSchemeAtEdge
    , directApplicationClosureOwnsEdges
    , rootRaiseMergeAuthorityFor
    , rootRaiseMergeAuthorityForExpression
    , rootRaiseMergeExteriorOwnedByScope
    , lgfChildren
    , lgfDirectEdgeSources
    , lgfOwner
    , localGammaDirectApplicationEdgeOwners
    , localGammaPreparedEnclosingEdgeOwners
    , localGammaFrame
    , localGammaOwnerOccursIn
    , localGammaOwnerScope
    , selectLocalGammaEdgeOwnership
    , mkIdentityTopologyConsumerAuthority
    , generalizationRequirementsForEnclosingRootExactEdges
    , subtermGeneralizationsOwnedBy
    , scaConsumerIdentity
    , scaEdgeId
    , subtermConsumerAuthorityEnclosingOwner
    , subtermConsumerAuthorityIsTopology
    , subtermGeneralizationConsumerAuthority
    , subtermGeneralizationConsumerIdentity
    , subtermGeneralizationConsumerConstructionSchemeInfo
    , subtermGeneralizationConstructionResultAbstractionRef
    , subtermGeneralizationExactConsumerSpecialization
    , subtermGeneralizationCompilerExactBoundary
    , subtermGeneralizationCompilerExactCompletionRef
    , subtermGeneralizationCompilerExactExistingRef
    , subtermGeneralizationCompilerExactResultRef
    , subtermGeneralizationCompilerExactResultStage
    , subtermGeneralizationConstructionBinderRenames
    , subtermGeneralizationInheritedGammaRoutes
    , subtermGeneralizationLocalResultAuthority
    , subtermGeneralizationOpaqueResultConstruction
    , subtermGeneralizationSourceOwnerConsumerCompletion
    , subtermGeneralizationSourceLambdaResultConstruction
    , subtermGeneralizationGammaAuthority
    , subtermGeneralizationGammaBoundScheme
    , subtermGeneralizationSchemeInfo
    , subtermGeneralizationOwnsGammaEdge
    , subtermGeneralizationOwnsGammaForEdge
    , subtermGeneralizationLocalConsumerClosure
    , subtermResultOwnershipFor
    , subtermResultOwnershipConsumerClosedLocally
    , subtermResultOwnershipHasTransparentPath
    , subtermResultOwnershipLambdaArity
    , subtermResultOwnershipLambdaNode
    , subtermResultOwnershipPacket
    , withCompilerExactSourceSubtermResult
    , withCompilerExactPacketSubtermResult
    , withCompilerExactDescendantSubtermResult
    , withCompilerExactEnclosingSubtermResult
    , withCompilerExactBinderRenames
    , withConstructionBinderRenames
    , withExactConsumerSpecialization
    , withSourceOwnerConsumerCompletion
    , withSourceOwnerFinalConsumerCompletion
    , withOpaqueResultConstruction
    , withPlacedCopiedBinderRoutes
    , withInheritedGammaRoutes
    , withSourceLambdaParameter
    )
import MLF.Elab.Inst (applyInstantiation, schemeToType)
import MLF.Elab.ReadModel (ElabReadModel, buildElabReadModel)
import MLF.Elab.SourceBinder
    ( orderSourceProjectedSchemeBinders
    , resolveConstructionSourceBindersInSchemeInfoExcept
    , resolveSourceBinderAliasesInType
    , sourceBinderAliasSubstitution
    , sourceBinderConstructionRenames
    , sourceBinderConstructionRenamesRetainingAmbiguousSources
    , typeBinderDeclarationRefs
    )
import qualified MLF.Elab.Reduce as Reduce
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Run.Annotation
    ( alignAnnInstantiationSites
    , annNode
    , redirectAndCanonicalizeAnn
    )
import MLF.Elab.Run.Generalize
    ( GeneralizeAtView
    , constraintForGeneralization
    , generalizeAtWithBuilderRequired
    , generalizeAtWithBuilderRequiredCertified
    , generalizeAtWithBuilderRequiredResultCertified
    , instantiationCopyNodes
    )
import MLF.Elab.Run.Instantiation
    ( planExactBinderSpine
    , resolvedSourceApplicationArgumentEndpoint
    )
import MLF.Elab.Run.Generalize.Types
    ( DirectApplicationAmbientGammaClaim(..)
    , DirectApplicationGammaClaim(..)
    , ExpansionConstructionPlacementConflict(..)
    , ExpansionConstructionPlacements
    , LocalGammaConstruction(..)
    , LocalGammaConstructionCertificate(..)
    , expansionConstructionPlacementsFromProjectedLists
    , localGammaConstructionCertificateResidualType
    , localGammaConstructionBinders
    , localGammaConsumedBinders
    , localGammaEmittedBinders
    , sourceBinderAuthorityConstructionRef
    , sourceBinderAuthoritySidecarRef
    )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType
    ( ResultTypeInputs(..)
    , ResultTypeView
    , buildResultTypeView
    , computeResultTypeFallbackWithView
    , computeResultTypeFromAnnWithView
    , mkResultTypeInputs
    )
import qualified MLF.Elab.Run.ResultType.View as View
import MLF.Elab.Run.Scope
    ( ConstructionScopes
    , constructionScopes
    , generalizeTargetNode
    , resolveCanonicalScope
    , resolveConstructionScopeForBoundary
    , schemeBodyTarget
    )
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeWithCanonical)
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , chaseRedirects
    , makeCanonicalizer
    )
import MLF.Reify.TypeOps
    ( alphaEqType
    , churchAwareEqType
    , freeTypeVarRefsType
    , matchChurchAwareTypeRefs
    , matchTypeRefs
    , splitForallsRefs
    , substTypeCaptureRef
    , substTypeSimpleRef
    )
import qualified MLF.Reify.Core as TypeReify
import MLF.Elab.Types
    ( ElabError(..)
    , BoundType
    , ElabScheme
    , ElabType
    , Instantiation(InstApp)
    , XmlfTerm
    , SchemeClosureAuthority
    , SchemeInfo(..)
    , Ty(TArrow, TBaseWithIdentity, TBottom, TConWithIdentity, TForallRef, TMuRef, TVarAppRef, TVarRef)
    , TypeBinderRef
    , idDetailsIdentityKey
    , bindingToElab
    , elabToBound
    , mkElabSchemeWithRefs
    , ambientSchemeClosureAuthority
    , mapBoundType
    , schemeBinderRefs
    , schemeBody
    , schemeFromType
    , schemeInfoFromRefSubst
    , rebuildSchemeInfoFromRefSubst
    , typeBinderRefFromIdentity
    , typeBinderIdentityFromNode
    , typeBinderRefIdentity
    , typeBinderRefName
    , typeBinderRefNode
    , typeBinderRefsSameIdentity
    , typeBinderRefsSameIdentityAndName
    , tyToElab
    , validateSchemeClosure
    )
import MLF.Frontend.ConstraintGen
    ( AnnExpr(..)
    , InstantiationSite(..)
    , InstantiationTargetTopology(..)
    )
import MLF.Frontend.Program.Types (resolvedSourceTypeToElabType)
import MLF.Frontend.Syntax (NormSrcType, ResolvedSrcType, VarName)
import MLF.Types.Identity
    ( IdentityGenerator
    , ResolvedTermIdentityKey
    , TypeBinderIdentity
    , typeBinderIdentityGeneratedUnique
    , typeBinderIdentityStableName
    , typeBinderIdentityStructural
    )
import MLF.Util.Trace (TraceConfig)

{- Note [Prepared generalization artifact]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generalization preparation is elaboration-owned because it aligns the solved
presolution output with source annotations, result-type reconstruction, and the
root generalization call.  Keeping the alignment here avoids letting
MLF.Elab.Run.Pipeline rebuild copy maps, redirects, scope overrides, and
canonical edge artifacts as unrelated local values.

The artifact's normal API exposes the few capabilities current consumers need,
while hiding both the raw record fields and the mechanics that produce them:

* the directional phase bridge from the acyclic base graph to the prepared
  presolved phase, retained on `GaBindParents` instead of duplicated on the
  outer artifact;
* instantiation copy-node and base-copy-map recovery from edge traces;
* the redirect plus union-find canonicalizer used for annotations and edge
  artifacts;
* the constraint-for-generalization rewrite plus finalized presolution view;
* let-scope override comparison between the acyclic base graph and the
  generalization graph;
* the result-type-ready adapter, so downstream consumers do not reconstruct
  redirects, canonical edge artifacts, base maps, or the owner-local phase
  bridge by deconstructing this artifact.

Result-type reconstruction still expects the thesis base graph in the same
phantom phase as the prepared view, but that graph already lives on
`pgaBindParentsGa.gaBaseConstraint`. The result-type adapter is assembled here,
and the artifact keeps the phase bridge owner-local to `GaBindParents` instead
of duplicating the base graph on the outer record.
-}
data PreparedGeneralizationArtifact = PreparedGeneralizationArtifact
    { pgaPresolutionView :: PresolutionView 'Presolved
    , pgaBindParentsGa :: GaBindParents 'Presolved
    , pgaExpansionConstructionPlacements :: ExpansionConstructionPlacements
    , pgaGeneralizeAt :: GeneralizeAtView 'Presolved
    , pgaResultTypeInputs :: ResultTypeInputs 'Presolved
    , pgaReadModel :: Either ElabError (ElabReadModel 'Presolved)
    , pgaBaseReadModel :: Either ElabError (ElabReadModel 'Presolved)
    , pgaResultTypeView :: Either ElabError (ResultTypeView 'Presolved)
    , pgaElaborationEdgeAuthority :: ElaborationEdgeAuthority
    , pgaExactProducerTypes :: Either ElabError (IntMap.IntMap ElabType)
    , pgaAnnotationSourceNodeKeys :: IntSet.IntSet
    , pgaScopeOverrides :: ConstructionScopes
    , pgaRootScopeOverrides :: [(AnnExpr, ConstructionScopes)]
    , pgaSubtermGeneralizations :: Either ElabError SubtermGeneralizations
    , pgaIdentityGenerator :: Either ElabError IdentityGenerator
    , pgaAnnotated :: AnnExpr
    , pgaAuthorizedElaborationRoots
        :: [(AnnExpr, AuthorizedElaborationRoot)]
    , pgaAnnNodeCanonical :: NodeId -> NodeId
    , pgaCanonical :: NodeId -> NodeId
    , pgaPlanBuilder :: PresolutionPlanBuilder
    , pgaSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    -- Direct declaration keys are a strict subset of the expanded source
    -- carrier. Solved/copy aliases can share a source identity with a fresh
    -- Gamma consumer without owning that consumer's identity.
    , pgaDirectSourceBinderKeys :: IntSet.IntSet
    , pgaCompilerExactEdgePlans :: Either ElabError (IntMap.IntMap CompilerExactEdgePlan)
    , pgaRedirects :: IntMap.IntMap NodeId
    -- Exact source schemes keyed by resolved term identity. These are
    -- installed by the pipeline after graph preparation and are consulted
    -- only for occurrence-owned application endpoints.
    , pgaResolvedTermSchemes :: Map.Map ResolvedTermIdentityKey SchemeInfo
    }

preparedEdgeArtifacts :: PreparedGeneralizationArtifact -> EdgeArtifacts
preparedEdgeArtifacts =
    elaborationEdgeArtifacts . pgaElaborationEdgeAuthority

preparedAnnotationExpectedTypesByEdge
    :: PreparedGeneralizationArtifact
    -> IntMap.IntMap ElabType
preparedAnnotationExpectedTypesByEdge =
    elaborationAnnotationExpectedTypesByEdge
        . pgaElaborationEdgeAuthority

withPreparedResolvedTermSchemes
    :: Map.Map ResolvedTermIdentityKey SchemeInfo
    -> PreparedGeneralizationArtifact
    -> PreparedGeneralizationArtifact
withPreparedResolvedTermSchemes schemes artifact =
    artifact {pgaResolvedTermSchemes = schemes}

-- | Complete construction authority for one compiler-owned exact edge.
-- Keeping the finalized contract and its graph routes together makes an
-- explicit empty trace different from a missing plan and prevents routes from
-- one exact occurrence being inferred from, or installed at, a sibling edge.
data CompilerExactEdgePlan = CompilerExactEdgePlan
    { ceepExpectedType :: !ElabType
    , ceepConstructionRefs :: !(IntMap.IntMap TypeBinderRef)
    -- Exact declaration identities paired with graph occurrences by source
    -- provenance. These are occurrence-selection evidence, not ambient Gamma
    -- aliases: one solved graph key can serve another lexical role elsewhere.
    , ceepDeclarationRefs :: !(IntMap.IntMap TypeBinderRef)
    }
    deriving (Eq, Show)

data PreparedRootGeneralization = PreparedRootGeneralization
    { prgScopeRoot :: NodeRef
    , prgTarget :: NodeId
    , prgScheme :: ElabScheme
    -- Root-owned binders only. Any local-constructor-owned result forall is
    -- reconstructed in this scheme's body, so closing it preserves the
    -- constructor's exact ETyAbs placement.
    , prgClosure :: PreparedRootClosure
    , prgSubst :: IntMap.IntMap TypeBinderRef
    -- The complete source-identity carrier used to construct this root,
    -- including edge-local compiler-exact routes.  Keeping it on the root
    -- artifact prevents final projection from falling back to the artifact's
    -- global carrier and losing identities introduced only by this exact
    -- occurrence.
    , prgSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    -- Direct source-declaration keys, including compiler-exact routes owned by
    -- this root. Expanded source aliases are deliberately excluded.
    , prgDirectSourceBinderKeys :: IntSet.IntSet
    -- The exact type scope available while constructing the root term.  This
    -- deliberately excludes locally owned result binders and every graph
    -- alias that resolves to one of them; those become available only when
    -- the recorded source constructor emits its ETyAbs.
    , prgConstructionScope :: PreparedRootConstructionScope
    -- Identities introduced by Figure 15.3.5 Gamma construction. A solved
    -- alias of a lexical source binder may feed such a consumer, but that
    -- does not make the fresh consumer the lexical binder itself.
    , prgConstructedGammaIdentities :: !(Set.Set TypeBinderIdentity)
    -- Graph-domain references occurring in the checked owner construction
    -- for which the owner certificate positively proves a root/local
    -- construction route.  Keep the sources rather than a precomputed target:
    -- later source/compiler-exact projection may refine 'prgSubst', and the
    -- finished term must enter that final quotient atomically with the root
    -- closure.
    , prgCertifiedTermBinderRefs :: ![TypeBinderRef]
    }

preparedRootCertifiedTermBinderRenames
    :: PreparedRootGeneralization
    -> [(TypeBinderRef, TypeBinderRef)]
preparedRootCertifiedTermBinderRenames rootGeneralization =
    [ (graphRef, outwardRef)
    | graphRef <- prgCertifiedTermBinderRefs rootGeneralization
    , Just graphNode <- [typeBinderRefNode graphRef]
    , Just outwardRef <-
        [ IntMap.lookup
            (getNodeId graphNode)
            (prgSubst rootGeneralization)
        ]
    , not
        ( typeBinderRefsSameIdentityAndName
            graphRef
            outwardRef
        )
    ]

-- | Source-tree and post-environment authority for quantifiers already
-- emitted by the elaborated root term.  Keeping the proof components in one
-- record avoids a constructor cross-product as new local constructors gain
-- exact construction certificates.
data PreparedLocalRootAuthority = PreparedLocalRootAuthority
    { plraPacketOwnership :: !(Maybe SubtermResultOwnership)
    , plraGammaClosures :: ![LocalGammaClosure]
    , plraApplicationCertificates :: ![LocalGammaConstructionCertificate]
    , plraAmbientBinderRefs :: ![TypeBinderRef]
    , plraScheme :: !ElabScheme
    }

preparedLocalRootAuthorityScheme
    :: PreparedLocalRootAuthority
    -> ElabScheme
preparedLocalRootAuthorityScheme authority =
    plraScheme authority

replacePreparedLocalRootAuthorityScheme
    :: ElabScheme
    -> PreparedLocalRootAuthority
    -> PreparedLocalRootAuthority
replacePreparedLocalRootAuthorityScheme scheme authority =
    authority {plraScheme = scheme}

-- | Prepared closure authority.  A local constructor couples the closure
-- scheme to the source-tree proof for every forall already emitted inside the
-- term, preventing final root closure from wrapping those binders again.
data PreparedRootClosure
    = PreparedWholeRootClosure
        ![TypeBinderRef]
        !ElabScheme
    -- | A topology packet was placed by moving its exact planned dependency
    -- spine into an enclosing flexible consumer bound.  The non-empty
    -- identity list is the placement proof that final term closure must
    -- construct that consumer result; it is not inferred from the final
    -- scheme's shape.
    | PreparedTopologyPacketRootClosure
        !(NonEmpty.NonEmpty TypeBinderIdentity)
        !PreparedRootClosure
    | PreparedLocalRootClosure
        !PreparedLocalRootAuthority
        !ElabScheme
    -- | Root and checked producer binders are interleaved by lexical bound
    -- dependency.  The identity list names the binders already constructed
    -- by the producer; final publication opens those exact foralls and emits
    -- only the missing positions in the full scheme spine.
    | PreparedInterleavedLocalRootClosure
        !PreparedLocalRootAuthority
        ![TypeBinderRef]
        !ElabScheme

-- | Construction proof for the result consumed by one lambda-body edge.
-- A terminal root RaiseMerge and a validated identity-topology bridge have
-- different Gamma obligations; representing them as constructors prevents a
-- caller from combining an arbitrary identity with an unrelated Boolean
-- "requires Gamma" flag.
data PreparedLambdaBodyConsumer
    = PreparedRootRaiseMergeBodyConsumer
        !EdgeId
        !RootRaiseMergeAuthority
    | PreparedIdentityTopologyBodyConsumer
        !EdgeId
        !GenNodeId
        !NodeId
        !GenNodeId
        !NodeId
        !NodeId
    deriving (Eq, Show)

preparedLambdaBodyConsumerEdge
    :: PreparedLambdaBodyConsumer
    -> EdgeId
preparedLambdaBodyConsumerEdge consumer =
    case consumer of
        PreparedRootRaiseMergeBodyConsumer edgeId _ -> edgeId
        PreparedIdentityTopologyBodyConsumer edgeId _ _ _ _ _ -> edgeId

preparedLambdaBodyConsumerIdentity
    :: PreparedLambdaBodyConsumer
    -> TypeBinderIdentity
preparedLambdaBodyConsumerIdentity consumer =
    case consumer of
        PreparedRootRaiseMergeBodyConsumer _ authority ->
            typeBinderIdentityFromNode (rrmaExterior authority)
        PreparedIdentityTopologyBodyConsumer _ _ _ _ _ resultRoot ->
            typeBinderIdentityFromNode resultRoot

preparedLambdaBodyConsumerRequiresGamma
    :: PreparedLambdaBodyConsumer
    -> Bool
preparedLambdaBodyConsumerRequiresGamma consumer =
    case consumer of
        PreparedRootRaiseMergeBodyConsumer {} -> True
        PreparedIdentityTopologyBodyConsumer {} -> False

preparedRootClosureScheme :: PreparedRootClosure -> ElabScheme
preparedRootClosureScheme closure =
    case closure of
        PreparedWholeRootClosure _ scheme -> scheme
        PreparedTopologyPacketRootClosure _ inner ->
            preparedRootClosureScheme inner
        PreparedLocalRootClosure _ scheme -> scheme
        PreparedInterleavedLocalRootClosure _ _ scheme -> scheme

preparedRootClosureSchemeAuthority
    :: PreparedRootClosure
    -> SchemeClosureAuthority
preparedRootClosureSchemeAuthority =
    ambientSchemeClosureAuthority
        . preparedRootClosureAmbientBinderRefs

preparedRootClosureAmbientBinderRefs
    :: PreparedRootClosure
    -> [TypeBinderRef]
preparedRootClosureAmbientBinderRefs closure =
    case closure of
        PreparedWholeRootClosure ambientRefs _ -> ambientRefs
        PreparedTopologyPacketRootClosure _ inner ->
            preparedRootClosureAmbientBinderRefs inner
        PreparedLocalRootClosure authority _ ->
            plraAmbientBinderRefs authority
        PreparedInterleavedLocalRootClosure authority _ _ ->
            plraAmbientBinderRefs authority

preparedRootClosureLocallyConstructedBinderRefs
    :: PreparedRootClosure
    -> [TypeBinderRef]
preparedRootClosureLocallyConstructedBinderRefs closure =
    case closure of
        PreparedWholeRootClosure {} -> []
        PreparedTopologyPacketRootClosure _ inner ->
            preparedRootClosureLocallyConstructedBinderRefs inner
        PreparedLocalRootClosure authority _ ->
            map fst
                ( schemeBinderRefs
                    (preparedLocalRootAuthorityScheme authority)
                )
        PreparedInterleavedLocalRootClosure _ localRefs _ ->
            localRefs

validatePreparedRootClosure
    :: String
    -> PreparedRootClosure
    -> Either ElabError PreparedRootClosure
validatePreparedRootClosure role closure = do
    _ <-
        validateSchemeClosure
            role
            (preparedRootClosureSchemeAuthority closure)
            (preparedRootClosureScheme closure)
    pure closure

-- | A closed construction-scope plan.  Keeping binders and graph aliases in
-- one value prevents callers from pairing the root-only binder spine with the
-- full result substitution, which would pre-bind a descendant packet result.
data PreparedRootConstructionScope = PreparedRootConstructionScope
    { prcsBinders :: [(TypeBinderRef, Maybe BoundType)]
    , prcsAliases :: IntMap.IntMap TypeBinderRef
    -- Source-to-graph routes whose target declaration is emitted by a
    -- descendant constructor.  They are construction identity provenance,
    -- not ambient Gamma aliases: installing the route must not put the
    -- descendant binder in scope before its lambda/application emits it.
    , prcsBinderRenames :: [(TypeBinderRef, TypeBinderRef)]
    -- Exact outward identities owned by descendant constructors.  Keep the
    -- requirement-selected construction endpoint, not only the source
    -- exterior recorded by 'LocalGammaClosure', so source projection cannot
    -- re-admit a routed local consumer as ambient root Gamma.
    , prcsLocallyClosedBinderRefs :: [TypeBinderRef]
    -- The edge-owned proof is retained so a later source-identity projection
    -- can rebuild the scope without admitting a locally closed Gamma alias.
    , prcsLocallyClosedGammas :: IntMap.IntMap LocalGammaClosure
    -- Post-environment application certificates have no surviving edge
    -- witness.  Retain their exact graph routes independently so source
    -- projection cannot re-admit a binder already emitted by the AApp.
    , prcsLocallyClosedApplicationNodes :: IntSet.IntSet
    }

preparedRootConstructionScopeBinders
    :: PreparedRootConstructionScope
    -> [(TypeBinderRef, Maybe BoundType)]
preparedRootConstructionScopeBinders = prcsBinders

preparedRootConstructionScopeAliases
    :: PreparedRootConstructionScope
    -> IntMap.IntMap TypeBinderRef
preparedRootConstructionScopeAliases = prcsAliases

preparedRootConstructionScopeBinderRenames
    :: PreparedRootConstructionScope
    -> [(TypeBinderRef, TypeBinderRef)]
preparedRootConstructionScopeBinderRenames = prcsBinderRenames

preparedRootConstructionScopeLocalGammaClosures
    :: PreparedRootConstructionScope
    -> IntMap.IntMap LocalGammaClosure
preparedRootConstructionScopeLocalGammaClosures = prcsLocallyClosedGammas

emptyPreparedRootConstructionScope :: PreparedRootConstructionScope
emptyPreparedRootConstructionScope =
    PreparedRootConstructionScope
        { prcsBinders = []
        , prcsAliases = IntMap.empty
        , prcsBinderRenames = []
        , prcsLocallyClosedBinderRefs = []
        , prcsLocallyClosedGammas = IntMap.empty
        , prcsLocallyClosedApplicationNodes = IntSet.empty
        }

data PacketExpectedType
    = CompilerExactExpectedType
        !EdgeId
        !ElabType
        ![(TypeBinderRef, Maybe BoundType)]
    | SourceExpectedType
        !ElabType
        ![(TypeBinderRef, Maybe BoundType)]
    -- | The exact source/canonical let identity returned by the current
    -- result path.  Unlike a general source annotation, this is positive
    -- construction authority for the already-built RHS packet and can seed
    -- S'(operated) without consulting the pending RaiseMerge result.
    | ReturnedBindingSourceExpectedType
        !ElabType
        ![(TypeBinderRef, Maybe BoundType)]

data SourceConstructionOrigin
    = DirectSourceConstruction
    | ExactReturnedBindingConstruction !ResolvedTermIdentityKey
    | EnclosedReturnedBindingConstruction !ResolvedTermIdentityKey
    deriving (Eq, Show)

data ReturnedBindingResolution
    = PreserveReturnedBindings
    | ResolveOwnerBodyReturnedBindings
    deriving (Eq, Show)

data SourceConstructionResult = SourceConstructionResult
    { scrType :: !ElabType
    , scrOrigin :: !SourceConstructionOrigin
    , scrReturnedLambdaParameters :: ![RequiredLambdaParameter]
    }

encloseSourceConstructionOrigin
    :: SourceConstructionOrigin
    -> SourceConstructionOrigin
encloseSourceConstructionOrigin origin =
    case origin of
        DirectSourceConstruction -> DirectSourceConstruction
        ExactReturnedBindingConstruction bindingKey ->
            EnclosedReturnedBindingConstruction bindingKey
        EnclosedReturnedBindingConstruction bindingKey ->
            EnclosedReturnedBindingConstruction bindingKey

isExactReturnedBindingConstruction :: SourceConstructionOrigin -> Bool
isExactReturnedBindingConstruction origin =
    case origin of
        ExactReturnedBindingConstruction {} -> True
        DirectSourceConstruction -> False
        EnclosedReturnedBindingConstruction {} -> False

-- | Source-tree authority that one administrative packet is the complete
-- construction for a particular nested lambda.  The graph can erase the
-- outer arrow when an unused parameter and a body result are represented by
-- separate flexible nodes; carrying the paired lambda/parameter nodes lets
-- packet preparation reconstruct Figure 15.3.5's arrow at the only boundary
-- that owns it.
data RequiredLambdaParameter = RequiredLambdaParameter
    { rlpParameterNode :: !NodeId
    , rlpLambdaNode :: !NodeId
    -- Nothing is reserved for a genuinely unbounded graph parameter.  A
    -- structured source parameter carries its exact identity-bearing type so
    -- packet construction can form the lambda arrow without rediscovering
    -- that type from the completed body.
    , rlpStructuredParameterType :: !(Maybe ElabType)
    -- A bare source-domain binder is occurrence-local authority that the
    -- paired graph parameter denotes this exact semantic identity.  Install
    -- that route before packet generalization; source-annotation forall
    -- binders are lexical and therefore deliberately absent from the global
    -- free-source-binder sidecar.
    , rlpSourceParameterRef :: !(Maybe TypeBinderRef)
    }
    deriving (Eq, Show)

-- | A result action proved while preparing one compiler-exact packet.  The
-- packet-owned form has no separately selectable completion identity, so the
-- later stored authority cannot represent an After-stage identity change.
data CompilerExactPacketResult
    = SourceOwnedCompilerExactPacketResult
        !EdgeId
        !TypeBinderRef
        !TypeBinderRef
    | PacketOwnedCompilerExactPacketResult
        !EdgeId
        !TypeBinderRef
    | DescendantOwnedCompilerExactPacketResult
        !EdgeId
        !TypeBinderRef

-- | A composed administrative-lambda packet can retain the result binder
-- already owned by a deeper packet in the same source lambda spine.  The
-- deeper packet is the construction boundary whose body actually has the
-- operated result type; repeating its delayed Hyp at an enclosing lambda
-- would apply that Hyp to an arrow instead.  Resolve that ownership while the
-- descendant packet and exact edge are both explicit, before attaching a
-- completion action to the enclosing packet.
descendantOwnsExactResult
    :: CompilerExactPacketResult
    -> SubtermGeneralizations
    -> Bool
descendantOwnsExactResult candidate descendants =
    any ownsCandidate (Map.elems descendants)
  where
    (exactEdge, candidateRef) =
        case candidate of
            SourceOwnedCompilerExactPacketResult edge resultRef _ ->
                (edge, resultRef)
            PacketOwnedCompilerExactPacketResult edge resultRef ->
                (edge, resultRef)
            DescendantOwnedCompilerExactPacketResult edge resultRef ->
                (edge, resultRef)

    ownsCandidate packet =
        subtermGeneralizationCompilerExactBoundary packet == Just exactEdge
            && case subtermGeneralizationCompilerExactCompletionRef packet of
                Just descendantRef ->
                    typeBinderRefsSameIdentity descendantRef candidateRef
                Nothing -> False

packetExpectedType :: PacketExpectedType -> ElabType
packetExpectedType expected =
    case expected of
        CompilerExactExpectedType _ ty _ -> ty
        SourceExpectedType ty _ -> ty
        ReturnedBindingSourceExpectedType ty _ -> ty

packetOperatedExpectedType :: PacketExpectedType -> ElabType
packetOperatedExpectedType expected =
    case expected of
        CompilerExactExpectedType _ ty enclosingBinders ->
            compilerExactOperatedType enclosingBinders ty
        SourceExpectedType ty _ -> ty
        ReturnedBindingSourceExpectedType ty _ -> ty

packetExpectedAmbientBinders
    :: PacketExpectedType
    -> [(TypeBinderRef, Maybe BoundType)]
packetExpectedAmbientBinders expected =
    case expected of
        CompilerExactExpectedType _ _ enclosingBinders -> enclosingBinders
        SourceExpectedType _ enclosingBinders -> enclosingBinders
        ReturnedBindingSourceExpectedType _ enclosingBinders ->
            enclosingBinders

packetExpectedTypeIsSource :: PacketExpectedType -> Bool
packetExpectedTypeIsSource expected =
    case expected of
        SourceExpectedType {} -> True
        ReturnedBindingSourceExpectedType {} -> True
        CompilerExactExpectedType {} -> False

sourcePacketExpectedType :: ElabType -> PacketExpectedType
sourcePacketExpectedType =
    sourcePacketExpectedTypeWith SourceExpectedType

returnedBindingSourcePacketExpectedType :: ElabType -> PacketExpectedType
returnedBindingSourcePacketExpectedType =
    sourcePacketExpectedTypeWith ReturnedBindingSourceExpectedType

sourcePacketExpectedTypeWith
    :: ( ElabType
        -> [(TypeBinderRef, Maybe BoundType)]
        -> PacketExpectedType
       )
    -> ElabType
    -> PacketExpectedType
sourcePacketExpectedTypeWith constructor ty =
    constructor ty ambientBinders
  where
    -- A source annotation owns its leading forall spine and installs those
    -- binders while constructing an annotated lambda.  Packet preparation
    -- sees the lambda body before the annotation wrapper emits that spine, so
    -- carry the declarations as lexical ambient authority alongside genuinely
    -- free source binders.
    (sourceBinders, _) = splitForallsRefs ty
    ambientBinders =
        sourceBinders
            ++ [ (ref, Nothing)
               | ref <- distinctTypeBinderRefs (freeTypeVarRefsType ty)
               , not
                    ( any
                        (typeBinderRefsSameIdentity ref . fst)
                        sourceBinders
                    )
               ]

compilerExactOperatedType
    :: [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ElabType
compilerExactOperatedType enclosingBinders ty =
    case ty of
        TVarRef ref ->
            case find (typeBinderRefsSameIdentity ref . fst) enclosingBinders of
                Just (_, Just bound) -> tyToElab bound
                Just (_, Nothing) -> TBottom
                Nothing -> ty
        _ -> ty

resolvedExactExpectedType
    :: ResolvedSrcType
    -> Either ElabError ElabType
resolvedExactExpectedType exactType =
    either (Left . InstantiationError) Right
        (resolvedSourceTypeToElabType exactType)

-- | Collect every descendant instantiation edge whose terminal root
-- RaiseMerge introduces a binder directly in the supplied Gamma.  A binder
-- can be needed only by a nested occurrence computation and therefore be
-- absent from the owner's result type; limiting requirements to the owner's
-- outer lambda-body edge leaves that Hyp unbound during construction.
--
-- The binding-tree parent is the ownership authority.  Descendant edges
-- owned by a nested gen node remain the responsibility of that nested term.
generalizationRequirementsForOwnedScope
    :: (NodeId -> NodeId)
    -> (NodeId -> NodeId)
    -> GaBindParents 'Presolved
    -> NodeRef
    -> PresolutionView 'Presolved
    -> EdgeArtifacts
    -> IntMap.IntMap ElabType
    -> IntMap.IntMap TypeBinderRef
    -> SubtermGeneralizations
    -> [(EdgeId, Maybe RootEdgeExactEndpoint)]
    -> Maybe ElabType
    -> AnnExpr
    -> Either ElabError GeneralizationRequirements
generalizationRequirementsForOwnedScope =
    generalizationRequirementsForScopeEdges
        []
        (pure . annotationInstantiationEdges)

-- | The complete root-boundary planning result.  Requirements alone are not
-- enough: an edge omitted because a nested constructor owns its Gamma must
-- remain distinguishable from an accidentally missing requirement when the
-- finalized root scheme is validated.  Application-local route values stay
-- in their construction certificates.  Boundary planning consumes only their
-- keys through 'grLocallyClosedGammaNodes', so a later root projection cannot
-- reinterpret a local consumer route as a root substitution.
data RootBoundaryRequirements = RootBoundaryRequirements
    { rbrRequirements :: !GeneralizationRequirements
    , rbrLocallyClosedGammas :: !(IntMap.IntMap LocalGammaClosure)
    , rbrInheritedGammaRoutes :: !Reify.InheritedGammaRoutes
    }

data RootBoundaryEdges = RootBoundaryEdges
    { rbeUnclaimedEdges :: ![EdgeId]
    , rbeLocallyClosedGammas :: !(IntMap.IntMap LocalGammaClosure)
    , rbeInheritedGammaRoutes :: !(IntMap.IntMap Reify.InheritedGammaRoutes)
    }

-- | Keep each source edge at most once, and never route an edge back to the
-- root planner after an exact local-constructor lane has claimed it.  The
-- duplicate arises when an application site and the transparent annotation
-- around its operand both name the same paper edge.
unclaimedEdgesOutsideLocalGammaClosures
    :: IntMap.IntMap LocalGammaClosure
    -> [EdgeId]
    -> [EdgeId]
unclaimedEdgesOutsideLocalGammaClosures locallyClosedGammas =
    foldl retain []
  where
    retain retained edgeId
        | IntMap.member (getEdgeId edgeId) locallyClosedGammas = retained
        | edgeId `elem` retained = retained
        | otherwise = retained ++ [edgeId]

-- | Prove that a locally claimed application requirement is stronger than a
-- provisional root slot for the same exterior.  The typed source owner,
-- complete merged edge set, exterior identity, and either its flexible
-- binding-tree scope or its exact direct-application edge provenance must all
-- agree.  An application's occurrence identity is its function edge, but
-- Figure 15.3.5 gives that same constructor ownership of both its function
-- and argument edges; requiring the occurrence edge to equal the claimed edge
-- would leak every argument-side Gamma back to the root.
exactApplicationClosureOwnsRequirement
    :: GaBindParents 'Presolved
    -> [RequiredGammaBinder]
    -> LocalGammaClosure
    -> RequiredGammaBinder
    -> Bool
exactApplicationClosureOwnsRequirement ga rootRequirements closure requirement =
    lgoConstructor owner == LocalApplicationGamma
        && lgcExteriorNode closure == rgbExteriorNode requirement
        && lgcConsumerIdentity closure
            == typeBinderIdentityFromNode (rgbExteriorNode requirement)
        && edgeKeySet (lgcEdgeIds closure)
            == edgeKeySet (rgbEdgeIds requirement)
        && rgbOperatedType requirement /= TBottom
        && not (null matchingRootRequirements)
        && all ((== TBottom) . rgbOperatedType) matchingRootRequirements
        && ( directApplicationClosureOwnsEdges
                closure
                (rgbEdgeIds requirement)
                || rootRaiseMergeExteriorOwnedByScope
                    ga
                    (localGammaOwnerScope owner)
                    (lgcExteriorNode closure)
           )
  where
    owner = lgcOwner closure
    matchingRootRequirements =
        filter
            ((== lgcExteriorNode closure) . rgbExteriorNode)
            rootRequirements
    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | Prove that a post-environment application certificate has already
-- constructed one root requirement.  The root edge need not be the
-- application's source edge: an enclosing constructor argument can carry the
-- same root-RaiseMerge exterior.  Ownership is exact only when every endpoint
-- in the typed root requirement routes to one constructed binder, that
-- binder's bound is S(operated), and either the application's lexical scope
-- owns the required placement or a validated per-requirement direct-edge claim
-- names this exact source occurrence.  A mixed direct/flexible edge set must
-- prove both partitions.  A consumed binder discharges the
-- obligation without leaving a forall for root reconstruction; an emitted
-- binder is reconstructed from the certificate later.
applicationCertificateOwnsRootRequirement
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateOwnsRootRequirement rootScope certificate requirement =
    lgoConstructor owner == LocalApplicationGamma
        && case directClaimsForRequirement certificate requirement of
            []
                | requirementUsesDirectApplicationSource
                    certificate
                    requirement ->
                    False
                | otherwise ->
                    applicationCertificateOwnerOwnsPlacement
                        rootScope
                        certificate
                        requirement
                        && routedConstructionBoundAgrees
                            certificate
                            requirement
            [claim] ->
                directApplicationGammaClaimOwnsRequirement
                    certificate
                    claim
                    requirement
                    && ( requirementHasOnlyDirectApplicationSources
                            certificate
                            requirement
                            || applicationCertificateOwnerOwnsPlacement
                                rootScope
                                certificate
                                requirement
                       )
            _ -> False
  where
    owner = lgccOwner certificate

requirementUsesDirectApplicationSource
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
requirementUsesDirectApplicationSource certificate requirement =
    not
        ( IntSet.null
            ( IntSet.intersection
                (edgeKeySet (lgccDirectApplicationSourceEdgeIds certificate))
                (edgeKeySet (rgbEdgeIds requirement))
            )
        )
  where
    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

requirementHasOnlyDirectApplicationSources
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
requirementHasOnlyDirectApplicationSources certificate requirement =
    requirementKeys `IntSet.isSubsetOf` directSourceKeys
  where
    directSourceKeys =
        edgeKeySet (lgccDirectApplicationSourceEdgeIds certificate)
    requirementKeys =
        edgeKeySet (rgbEdgeIds requirement)
    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | Select claims by the occurrence fields that cannot change between root
-- planner views.  Operated type/root and route evidence are validated after
-- selection so a malformed direct claim cannot silently fall back to the
-- ordinary scope-owned path.
directClaimsForRequirement
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> [DirectApplicationGammaClaim]
directClaimsForRequirement certificate requirement =
    [ claim
    | claim <- lgccDirectApplicationGammaClaims certificate
    , edgeKeySet (dagcEdgeIds claim)
        == directRequirementEdgeKeys
    , dagcExteriorNode claim == rgbExteriorNode requirement
    ]
  where
    directRequirementEdgeKeys =
        IntSet.intersection
            (edgeKeySet (lgccDirectApplicationSourceEdgeIds certificate))
            (edgeKeySet (rgbEdgeIds requirement))
    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

ambientDirectClaimsForRequirement
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> [DirectApplicationAmbientGammaClaim]
ambientDirectClaimsForRequirement certificate requirement =
    [ claim
    | claim <- lgccDirectApplicationAmbientGammaClaims certificate
    , edgeKeySet (daagcEdgeIds claim)
        == directRequirementEdgeKeys
    , daagcExteriorNode claim == rgbExteriorNode requirement
    ]
  where
    directRequirementEdgeKeys =
        IntSet.intersection
            (edgeKeySet (lgccDirectApplicationSourceEdgeIds certificate))
            (edgeKeySet (rgbEdgeIds requirement))
    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | Discharge a direct requirement proved ambient by the checked application.
-- The positive claim names the complete source occurrence and the exact
-- ambient declaration/bound selected during construction.  Its validity
-- requires every frozen endpoint to remain absent from local routes, one
-- matching declaration authority, and every free identity of the operated
-- type and declaration bound certified either by that application's local
-- Gamma construction or by its ambient-use set.
-- A later planner may rename result endpoints, but it cannot add a routed
-- local binder or a flexible source edge to this zero-local proof.
applicationCertificateOwnsAmbientRootRequirement
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateOwnsAmbientRootRequirement certificate requirement =
    lgoConstructor (lgccOwner certificate) == LocalApplicationGamma
        && requirementHasOnlyDirectApplicationSources certificate requirement
        && case ambientDirectClaimsForRequirement certificate requirement of
            [claim] ->
                daagcOperatedRoot claim == rgbOperatedRoot requirement
                    && constructionEndpointProvides
                        (daagcOperatedType claim)
                        (rgbOperatedType requirement)
                    && ambientDirectApplicationGammaClaimConstructionValid
                        certificate
                        claim
                    && all
                        ( \node ->
                            IntMap.notMember
                                (getNodeId node)
                                (lgccLocalBinderRoutes certificate)
                        )
                        (requirementRouteNodes requirement)
            _ -> False

ambientDirectApplicationGammaClaimConstructionValid
    :: LocalGammaConstructionCertificate
    -> DirectApplicationAmbientGammaClaim
    -> Bool
ambientDirectApplicationGammaClaimConstructionValid certificate claim =
    ambientDeclarationAuthoritiesMatchClaims certificate
        && all
        ( \node ->
            IntMap.notMember
                (getNodeId node)
                (lgccLocalBinderRoutes certificate)
        )
        (ambientDirectClaimRouteNodes claim)
        && not
            ( any
                (typeBinderRefsSameIdentity (daagcAmbientRef claim) . fst)
                ( localGammaConstructionBinders
                    (lgccConstruction certificate)
                )
            )
        && ambientClaimBoundSatisfies
            (daagcAmbientRef claim)
            (daagcAmbientBound claim)
            (daagcOperatedType claim)
        && all
            claimDependencyIsCertified
            ( freeTypeVarRefsType (daagcOperatedType claim)
                ++ freeTypeVarRefsType (daagcAmbientBound claim)
            )
  where
    claimDependencyIsCertified freeRef =
        any
            (typeBinderRefsSameIdentity freeRef)
            (certificateAvailableAmbientBinderRefs certificate)
            || any
                (typeBinderRefsSameIdentity freeRef . fst)
                ( localGammaConstructionBinders
                    (lgccConstruction certificate)
                )

    -- The application constructor may consume an ambient declaration's
    -- leading forall spine at this occurrence.  Reconstruct that same exact
    -- xMLF plan here rather than treating the declaration and operated
    -- endpoint as equal.  TBottom is deliberately excluded: it denotes a
    -- free ambient type variable here, not a polymorphic term scheme.
    ambientClaimBoundSatisfies ambientRef ambientBound operatedType =
        case operatedType of
            TVarRef operatedRef
                | typeBinderRefsSameIdentity ambientRef operatedRef -> True
            _ ->
                typesEquivalent ambientBound operatedType
                    || case ambientBound of
                        TForallRef {} ->
                            isJust
                                ( planExactBinderSpine
                                    typesEquivalent
                                    ambientBound
                                    operatedType
                                )
                        _ -> False

certificateAvailableAmbientBinderRefs
    :: LocalGammaConstructionCertificate
    -> [TypeBinderRef]
certificateAvailableAmbientBinderRefs certificate =
    lgccUsedAmbientBinderRefs certificate
        ++ map fst (lgccEnclosingTypeAbsBinders certificate)

ambientDeclarationAuthoritiesMatchClaims
    :: LocalGammaConstructionCertificate
    -> Bool
ambientDeclarationAuthoritiesMatchClaims certificate =
    all hasOneAuthority claims
        && all hasOneClaim authorities
  where
    claims =
        lgccDirectApplicationAmbientGammaClaims certificate
    authorities =
        lgccAmbientDeclarationAuthorities certificate

    hasOneAuthority claim =
        case filter (`authorityMatchesClaim` claim) authorities of
            [_] -> True
            _ -> False

    hasOneClaim authority =
        case filter (authorityMatchesClaim authority) claims of
            [_] -> True
            _ -> False

    authorityMatchesClaim authority claim =
        typeBinderRefsSameIdentity
            (agaExactRef authority)
            (daagcAmbientRef claim)
            && typesEquivalent
                (agaBound authority)
                (daagcAmbientBound claim)

applicationCertificateDischargesRootRequirement
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateDischargesRootRequirement rootScope certificate requirement =
    applicationCertificateOwnsRootRequirement
        rootScope
        certificate
        requirement
        || applicationCertificateOwnsAmbientRootRequirement
            certificate
            requirement

directApplicationGammaClaimOwnsRequirement
    :: LocalGammaConstructionCertificate
    -> DirectApplicationGammaClaim
    -> RequiredGammaBinder
    -> Bool
directApplicationGammaClaimOwnsRequirement certificate claim requirement =
    dagcOperatedRoot claim == rgbOperatedRoot requirement
        && directClaimEndpointProvides
            claim
            (rgbOperatedType requirement)
        && directApplicationGammaClaimConstructionValid
            certificate
            claim
        && routesAllReachRef
            certificate
            (requirementRouteNodes requirement)
            (dagcBinderRef claim)
        && directClaimBoundProvides
            claim
            (rgbOperatedType requirement)

-- | A direct application claim owns the source occurrence independently of a
-- later planner's pre-environment type and result-route projection.  The
-- certificate must still prove S(operated) against its post-environment type
-- and every route in its construction-time claim.  Edge set, exterior, and
-- operated root remain exact, so the specialized claim cannot discharge a
-- different source occurrence.
applicationCertificateDirectClaimOwnsPlanningRequirement
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateDirectClaimOwnsPlanningRequirement certificate requirement =
    lgoConstructor (lgccOwner certificate) == LocalApplicationGamma
        && requirementHasOnlyDirectApplicationSources certificate requirement
        && case directClaimsForRequirement certificate requirement of
            [claim] ->
                dagcOperatedRoot claim == rgbOperatedRoot requirement
                    && directApplicationGammaClaimConstructionValid
                        certificate
                        claim
            _ -> False

-- | Discharge a provisional downstream slot when a checked application has
-- already constructed the exact result occurrence carrying that exterior.
-- Alias/let frames can retain a frozen @Bottom@ requirement whose operated
-- node is downstream of the application, so it cannot equal the direct
-- source claim's operated root.  A mixed direct/structural requirement is
-- stricter: the claim's direct edges must be part of that requirement and
-- every requirement route node must reach the same constructed binder.  In
-- both cases one validated non-bottom direct claim must name the same
-- exterior and complete result-node set, and the application term node must
-- be that result occurrence.  No representative or type-shape relation
-- participates.
applicationCertificateCompletesProvisionalResultRequirement
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateCompletesProvisionalResultRequirement certificate requirement =
    lgoConstructor owner == LocalApplicationGamma
        && rgbOperatedType requirement == TBottom
        && case matchingClaims of
            [claim] ->
                directApplicationGammaClaimConstructionValid
                    certificate
                    claim
                    && dagcOperatedType claim /= TBottom
                    && resultRoutesReachClaim claim
                    && mixedDirectRoutesAreComplete claim
            _ -> False
  where
    owner = lgccOwner certificate
    requirementUsesDirect =
        requirementUsesDirectApplicationSource certificate requirement
    requirementResultKeys =
        IntSet.fromList
            (map getNodeId (NonEmpty.toList (rgbResultRoots requirement)))
    matchingClaims =
        [ claim
        | claim <- lgccDirectApplicationGammaClaims certificate
        , dagcExteriorNode claim == rgbExteriorNode requirement
        , IntSet.fromList
            ( map
                getNodeId
                (NonEmpty.toList (dagcConstructionResultRoots claim))
            )
            == requirementResultKeys
        , IntSet.member
            (getNodeId (lgoTermNode owner))
            requirementResultKeys
        ]

    resultRoutesReachClaim claim =
        routesAllReachRef
            certificate
            ( rgbExteriorNode requirement
                : NonEmpty.toList (rgbResultRoots requirement)
            )
            (dagcBinderRef claim)

    mixedDirectRoutesAreComplete claim
        | not requirementUsesDirect = True
        | otherwise =
            edgeKeySet (dagcEdgeIds claim)
                `IntSet.isSubsetOf` edgeKeySet (rgbEdgeIds requirement)
                && routesAllReachRef
                    certificate
                    (requirementRouteNodes requirement)
                    (dagcBinderRef claim)

    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | Discharge an exact downstream result requirement from the checked
-- construction that produced that result.  A later planner view can move the
-- operated root from the direct source occurrence to the application result,
-- but it cannot change the direct source partition, exterior, complete result
-- occurrence, endpoint, route, or construction scope.
--
-- Unlike 'applicationCertificateCompletesProvisionalResultRequirement', this
-- proof is only for a non-bottom endpoint.  Every requirement route must
-- therefore reach the constructed binder, and that binder's checked bound must
-- construct the exact endpoint.  These positive conditions distinguish a
-- genuine source-to-result transfer from two same-shaped Gamma obligations.
applicationCertificateCompletesExactResultRequirement
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateCompletesExactResultRequirement rootScope certificate requirement =
    lgoConstructor owner == LocalApplicationGamma
        && rgbOperatedType requirement /= TBottom
        && applicationCertificateOwnerOwnsPlacement
            rootScope
            certificate
            requirement
        && case directClaimsForRequirement certificate requirement of
            [claim] ->
                directApplicationGammaClaimConstructionValid
                    certificate
                    claim
                    && directClaimEndpointProvides
                        claim
                        (rgbOperatedType requirement)
                    && directClaimBoundProvides
                        claim
                        (rgbOperatedType requirement)
                    && resultOccurrenceMatches claim
                    && routesAllReachRef
                        certificate
                        (requirementRouteNodes requirement)
                        (dagcBinderRef claim)
            _ -> False
  where
    owner = lgccOwner certificate
    requirementResultKeys =
        IntSet.fromList
            (map getNodeId (NonEmpty.toList (rgbResultRoots requirement)))

    resultOccurrenceMatches claim =
        IntSet.fromList
            ( map
                getNodeId
                (NonEmpty.toList (dagcConstructionResultRoots claim))
            )
            == requirementResultKeys
            && IntSet.member
                (getNodeId (lgoTermNode owner))
                requirementResultKeys

directApplicationGammaClaimConstructionValid
    :: LocalGammaConstructionCertificate
    -> DirectApplicationGammaClaim
    -> Bool
directApplicationGammaClaimConstructionValid certificate claim =
    routesAllReachRef
        certificate
        (directClaimRouteNodes claim)
        (dagcBinderRef claim)
        && case constructionBinderForRef certificate (dagcBinderRef claim) of
            Just constructedBound ->
                boundsEquivalent
                    constructedBound
                    (dagcConstructedBound claim)
            Nothing -> False

routedConstructionBoundAgrees
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
routedConstructionBoundAgrees certificate requirement =
    case routedRequirementRef certificate requirement of
        Just routedRef ->
            maybe
                False
                ( \constructedBound ->
                    boundMatchesType
                        constructedBound
                        (rgbOperatedType requirement)
                )
                (constructionBinderForRef certificate routedRef)
        Nothing -> False

routedRequirementRef
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Maybe TypeBinderRef
routedRequirementRef certificate requirement =
    case
        [ routedRef
        | node <- requirementRouteNodes requirement
        , Just routedRef <-
            [IntMap.lookup (getNodeId node) routes]
        ]
    of
        routedRef : rest
            | length rest + 1 == length (requirementRouteNodes requirement)
            , all
                (typeBinderRefsSameIdentity routedRef)
                rest ->
                Just routedRef
        _ -> Nothing
  where
    routes = lgccLocalBinderRoutes certificate

constructionBinderForRef
    :: LocalGammaConstructionCertificate
    -> TypeBinderRef
    -> Maybe (Maybe BoundType)
constructionBinderForRef certificate routedRef =
    case
        [ mbBound
        | (constructionRef, mbBound) <-
            localGammaConstructionBinders (lgccConstruction certificate)
        , typeBinderRefsSameIdentity constructionRef routedRef
        ]
    of
        [mbBound] -> Just mbBound
        _ -> Nothing

routesAllReachRef
    :: LocalGammaConstructionCertificate
    -> [NodeId]
    -> TypeBinderRef
    -> Bool
routesAllReachRef certificate nodes expectedRef =
    all
        ( \node ->
            maybe
                False
                (typeBinderRefsSameIdentity expectedRef)
                ( IntMap.lookup
                    (getNodeId node)
                    (lgccLocalBinderRoutes certificate)
                )
        )
        nodes

requirementRouteNodes :: RequiredGammaBinder -> [NodeId]
requirementRouteNodes requirement =
    rgbExteriorNode requirement
        : rgbOperatedRoot requirement
        : NonEmpty.toList (rgbResultRoots requirement)

directClaimRouteNodes :: DirectApplicationGammaClaim -> [NodeId]
directClaimRouteNodes claim =
    dagcExteriorNode claim
        : dagcOperatedRoot claim
        : NonEmpty.toList (dagcConstructionResultRoots claim)

ambientDirectClaimRouteNodes
    :: DirectApplicationAmbientGammaClaim
    -> [NodeId]
ambientDirectClaimRouteNodes claim =
    daagcExteriorNode claim
        : daagcOperatedRoot claim
        : NonEmpty.toList (daagcConstructionResultRoots claim)

typesEquivalent :: ElabType -> ElabType -> Bool
typesEquivalent left right =
    alphaEqType left right || churchAwareEqType left right

boundsEquivalent :: Maybe BoundType -> Maybe BoundType -> Bool
boundsEquivalent left right =
    typesEquivalent
        (maybe TBottom tyToElab left)
        (maybe TBottom tyToElab right)

boundMatchesType :: Maybe BoundType -> ElabType -> Bool
boundMatchesType mbBound ty =
    typesEquivalent (maybe TBottom tyToElab mbBound) ty

-- | Relate a checked application declaration to a later exact endpoint.  A
-- source declaration provides that endpoint only when it is already equal or
-- exact scheme inference consumes its complete unbounded forall spine.
constructionEndpointProvides :: ElabType -> ElabType -> Bool
constructionEndpointProvides source endpoint =
    typesEquivalent source endpoint
        || completeUnboundedForallSpecializesTo source endpoint

boundProvidesType :: Maybe BoundType -> ElabType -> Bool
boundProvidesType mbBound =
    constructionEndpointProvides (maybe TBottom tyToElab mbBound)

-- | A direct application may consume a child value only after that child has
-- emitted the exact forall spine which closes identities still free in the
-- frozen graph endpoint.  The direct claim is the positive construction
-- authority for that transition; the closure relation alone is not an
-- instantiation rule and is never used for an ambient or shape-only claim.
directClaimEndpointProvides
    :: DirectApplicationGammaClaim
    -> ElabType
    -> Bool
directClaimEndpointProvides claim endpoint =
    constructionEndpointProvides
        (dagcOperatedType claim)
        endpoint
        || exactIdentityForallClosureOf
            (dagcOperatedType claim)
            endpoint

directClaimBoundProvides
    :: DirectApplicationGammaClaim
    -> ElabType
    -> Bool
directClaimBoundProvides claim endpoint =
    boundProvidesType (dagcConstructedBound claim) endpoint
        || let constructedBound =
                    maybe TBottom tyToElab (dagcConstructedBound claim)
           in typesEquivalent constructedBound (dagcOperatedType claim)
                && exactIdentityForallClosureOf
                    constructedBound
                    endpoint

-- | Transfer an exact post-environment ownership proof from the canonical
-- root-requirement view to another planner view of the same Gamma binder.
-- Result endpoints can differ because one view includes edge-local closure
-- inputs, but the complete source-edge set, semantic exterior, operated root,
-- and S(operated) bound must remain identical.  Ordinary transfer retains the
-- certified route and exact placement as before.  A direct application
-- transfer must select that same per-requirement claim; alternate result
-- endpoints remain planner routing data and are not reinterpreted as a second
-- construction proof.  A mixed direct/flexible requirement additionally
-- retains ordinary placement for its flexible remainder.
-- The certificate's own application edge is independent and need not equal
-- this set.  Rechecking every alternate result endpoint against the
-- application route would leave the same certified obligation alive in one
-- planner view after removing its closure in another; ignoring the planner
-- edge set would instead let one certified obligation erase another
-- same-shaped edge occurrence.
applicationCertificateTransfersRootRequirementOwnership
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> RequiredGammaBinder
    -> Bool
applicationCertificateTransfersRootRequirementOwnership rootScope certificate certified requirement =
    applicationCertificateOwnsRootRequirement
        rootScope
        certificate
        certified
        && requirementEdgeKeys requirement
            == requirementEdgeKeys certified
        && rgbExteriorNode requirement == rgbExteriorNode certified
        && rgbOperatedRoot requirement == rgbOperatedRoot certified
        && equivalentOperatedTypes
            (rgbOperatedType requirement)
            (rgbOperatedType certified)
        && transfersSameClaim
  where
    requirementEdgeKeys =
        IntSet.fromList . map getEdgeId . NonEmpty.toList . rgbEdgeIds

    equivalentOperatedTypes left right =
        constructionEndpointProvides left right
            || constructionEndpointProvides right left

    transfersSameClaim =
        case directClaimsForRequirement certificate certified of
            [] ->
                rgbPlacement requirement == rgbPlacement certified
                    && applicationCertificateOwnerOwnsPlacement
                        rootScope
                        certificate
                        requirement
            [certifiedClaim] ->
                case directClaimsForRequirement certificate requirement of
                    [requirementClaim] ->
                        requirementClaim == certifiedClaim
                            && ( requirementHasOnlyDirectApplicationSources
                                    certificate
                                    requirement
                                    || ( rgbPlacement requirement
                                            == rgbPlacement certified
                                            && applicationCertificateOwnerOwnsPlacement
                                                rootScope
                                                certificate
                                                requirement
                                       )
                               )
                    _ -> False
            _ -> False

applicationCertificateOwnerOwnsPlacement
    :: NodeRef
    -> LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateOwnerOwnsPlacement rootScope certificate requirement =
    case rgbPlacement requirement of
        RequiredGammaAtCurrentScope ->
            localGammaOwnerScope (lgccOwner certificate) == rootScope
        RequiredGammaAtConstructionScope scope ->
            localGammaOwnerScope (lgccOwner certificate) == scope
        RequiredGammaAtNestedScope _ -> False

-- | Remove a provisional structural closure only when a checked application
-- certificate proves that it constructed the complete typed root obligation
-- for that exact edge set and exterior.  The closure's owner can be an
-- enclosing lambda: structural planning runs before application elaboration
-- and therefore records the nearest lexical owner, while the application's
-- requirement-specific non-empty construction route is the stronger
-- authority.
--
-- A 'LocalGammaAmbient' certificate is deliberately insufficient here.  Its
-- direct claim proves that this application emitted no local Gamma for the
-- occurrence; the declaration can still be owned by the enclosing structural
-- closure.  Removing that closure would turn relative ambientness into root
-- construction ownership and discard the exact declaration provenance needed
-- to complete its bound.
applicationCertificateDischargesRootClosure
    :: NodeRef
    -> [RequiredGammaBinder]
    -> LocalGammaClosure
    -> LocalGammaConstructionCertificate
    -> Bool
applicationCertificateDischargesRootClosure rootScope rootRequirements closure certificate =
    lgcConsumerIdentity closure
        == typeBinderIdentityFromNode (lgcExteriorNode closure)
        && any ownsMatchingRequirement rootRequirements
  where
    ownsMatchingRequirement requirement =
        rgbExteriorNode requirement == lgcExteriorNode closure
            && edgeKeySet (rgbEdgeIds requirement)
                == edgeKeySet (lgcEdgeIds closure)
            && applicationCertificateOwnsRootRequirement
                rootScope
                certificate
                requirement
    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | Decide the root-vs-local ownership layer after application certificates
-- have been validated.  Only root requirements still unowned by an exact
-- application certificate block a structural closure for the same exterior.
-- This ordering matters for 'LocalGammaAmbient': the direct planning
-- requirement is discharged because the application found an exact ambient
-- declaration, while the enclosing lambda/let closure remains the
-- declaration's construction provenance.
rootRequirementOwnershipAllowsLocalGammaClosure
    :: GaBindParents 'Presolved
    -> NodeRef
    -> [LocalGammaConstructionCertificate]
    -> [RequiredGammaBinder]
    -> [RequiredGammaBinder]
    -> LocalGammaClosure
    -> Bool
rootRequirementOwnershipAllowsLocalGammaClosure ga rootScope certificates rootRequirements constructionRequirements closure =
    not rootStillOwnsExterior
        || any
            ( exactApplicationClosureOwnsRequirement
                ga
                rootRequirements
                closure
            )
            constructionRequirements
  where
    rootStillOwnsExterior =
        any
            ( \requirement ->
                rgbExteriorNode requirement == lgcExteriorNode closure
                    && not
                        ( any
                            ( \certificate ->
                                applicationCertificateDischargesRootRequirement
                                    rootScope
                                    certificate
                                    requirement
                            )
                            certificates
                        )
            )
            rootRequirements

-- | Collect the obligations needed for the complete root result scheme while
-- retaining exact proof of which ones are constructed locally. A local
-- constructor on the result path contributes its S'(operated) bound to the
-- scheme, but 'prepareRequiredRootConstructionScope' excludes that binder
-- from the root construction Gamma and final closure preserves its local
-- 'ETyAbsRef' placement.
generalizationRequirementsForRootBoundary
    :: (EdgeId -> NodeId -> Either ElabError NodeRef)
    -> (NodeId -> NodeId)
    -> (NodeId -> NodeId)
    -> GaBindParents 'Presolved
    -> NodeRef
    -> PresolutionView 'Presolved
    -> EdgeArtifacts
    -> IntMap.IntMap ElabType
    -- Root-planning source refs exclude declarations owned by nested source
    -- annotations.  Certificates still need the complete prepared sidecar to
    -- validate those exact local source occurrences.
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> SubtermGeneralizations
    -> SubtermGeneralizations
    -> [(EdgeId, Maybe ElabType)]
    -> Maybe ElabType
    -> [LocalGammaConstructionCertificate]
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError RootBoundaryRequirements
generalizationRequirementsForRootBoundary scopeForBoundary identityRepresentative constructionCanonical ga ownerScope presolutionView edgeArtifacts exactProducerTypes certificateSourceBinderRefs sourceBinderRefs allSubtermPackets subtermPackets explicitEdges expectedType localApplicationCertificates certificateOwnerAnn ann = do
    localApplicationRoutes <-
        validateLocalApplicationCertificatesWithRepresentative
            identityRepresentative
            scopeForBoundary
            certificateOwnerAnn
            certificateSourceBinderRefs
            localApplicationCertificates
    boundaryEdges <-
        rootBoundaryInstantiationEdges
            scopeForBoundary
            ga
            edgeArtifacts
            (Map.elems allSubtermPackets)
            ann
    let explicitProducerEdges =
            [ (edgeId, RootEdgeExactProducer <$> mbEndpoint)
            | (edgeId, mbEndpoint) <- explicitEdges
            ]
        -- An exact endpoint refines S'(operated); it does not override the
        -- syntax-owned constructor selected for that edge.  The root-owned
        -- baseline must therefore exclude every edge already claimed by a
        -- local closure.
        rootExplicitProducerEdges =
            [ edge
            | edge@(edgeId, _) <- explicitProducerEdges
            , IntMap.notMember
                (getEdgeId edgeId)
                (rbeLocallyClosedGammas boundaryEdges)
            ]
    rootRequirements <-
        generalizationRequirementsForScopeEdges
            []
            (const (pure (rbeUnclaimedEdges boundaryEdges)))
            identityRepresentative
            constructionCanonical
            ga
            ownerScope
            presolutionView
            edgeArtifacts
            exactProducerTypes
            sourceBinderRefs
            subtermPackets
            rootExplicitProducerEdges
            expectedType
            ann
    let resultLocalEdges =
            [ EdgeId edgeKey
            | (edgeKey, closure) <-
                IntMap.toList (rbeLocallyClosedGammas boundaryEdges)
            , localGammaOwnerOnResultPath (lgcOwner closure) ann
            ]
        resultLocalEdgeKeys =
            IntSet.fromList (map getEdgeId resultLocalEdges)
        -- A locally claimed exact edge contributes to the root result only
        -- when its constructor is on that result path.  Other exact
        -- application arguments are closed inside the checked term and must
        -- not be replayed as root Gamma requirements.
        resultExplicitProducerEdges =
            [ edge
            | edge@(edgeId, _) <- explicitProducerEdges
            , IntMap.notMember
                (getEdgeId edgeId)
                (rbeLocallyClosedGammas boundaryEdges)
                || IntSet.member
                    (getEdgeId edgeId)
                    resultLocalEdgeKeys
            ]
    requirements0 <-
        generalizationRequirementsForScopeEdges
            resultLocalEdges
            ( const
                ( pure
                    ( rbeUnclaimedEdges boundaryEdges
                        ++ resultLocalEdges
                    )
                )
            )
            identityRepresentative
            constructionCanonical
            ga
            ownerScope
            presolutionView
            edgeArtifacts
            exactProducerTypes
            sourceBinderRefs
            subtermPackets
            resultExplicitProducerEdges
            expectedType
            ann
    let closureMatchesRequirement closure requirement =
            lgcExteriorNode closure == rgbExteriorNode requirement
                && IntSet.fromList
                    (map getEdgeId (NonEmpty.toList (lgcEdgeIds closure)))
                    == IntSet.fromList
                        (map getEdgeId (NonEmpty.toList (rgbEdgeIds requirement)))
        closureHasEnclosingConsumerAuthority closure =
            any packetHasEnclosingAuthority (Map.elems allSubtermPackets)
          where
            packetHasEnclosingAuthority packet =
                case subtermGeneralizationConsumerAuthority packet of
                    Just authority ->
                        scaConsumerIdentity authority
                            == lgcConsumerIdentity closure
                            && isJust
                                (subtermConsumerAuthorityEnclosingOwner authority)
                            && not
                                (subtermConsumerAuthorityIsTopology authority)
                    Nothing -> False
        closureHasCompleteDirectApplicationProvenance closure =
            directApplicationClosureOwnsEdges
                closure
                (lgcEdgeIds closure)
        -- If any non-bottom edge for an exterior remains root-owned after
        -- applying the post-environment application certificates, the root
        -- Gamma must introduce that binder for every descendant use.  A
        -- nested edge cannot make that shared exterior local by itself.  A
        -- zero-local application claim removes only its exact direct planning
        -- requirement from that root-owned set; it does not remove the
        -- enclosing structural closure that made the declaration ambient to
        -- the application.  The one exception for a genuinely root-owned
        -- provisional slot is an exact application occurrence whose complete
        -- edge/exterior/scope proof carries a non-bottom requirement.  In
        -- that case the local constructor is the stronger authority and must
        -- shadow the placeholder.  A solved edge can also leave a precomputed
        -- closure after its exterior has collapsed to a rigid result, so
        -- every other closure still needs an exact requirement or
        -- enclosing-consumer proof.
        locallyClosedGammas =
            IntMap.filter
                ( \closure ->
                    not (applicationDischargesClosure closure)
                        && ( any
                                (closureMatchesRequirement closure)
                                (grRequiredGammaBinders requirements0)
                                || closureHasEnclosingConsumerAuthority closure
                                || closureHasCompleteDirectApplicationProvenance closure
                           )
                        && rootRequirementOwnershipAllowsLocalGammaClosure
                            ga
                            ownerScope
                            localApplicationCertificates
                            (grRequiredGammaBinders rootRequirements)
                            (grRequiredGammaBinders requirements0)
                            closure
                )
                (rbeLocallyClosedGammas boundaryEdges)
        locallyClosedApplicationNodes = IntMap.keysSet localApplicationRoutes
        exactApplicationOwnsRequirement requirement =
            any
                ( \certificate ->
                    applicationCertificateDischargesRootRequirement
                        ownerScope
                        certificate
                        requirement
                )
                localApplicationCertificates
        applicationOwnsPlanningRequirement requirement =
            any
                ( \certificate ->
                    applicationCertificateDischargesRootRequirement
                        ownerScope
                        certificate
                        requirement
                        || applicationCertificateDirectClaimOwnsPlanningRequirement
                            certificate
                            requirement
                        || applicationCertificateCompletesProvisionalResultRequirement
                            certificate
                            requirement
                        || applicationCertificateCompletesExactResultRequirement
                            ownerScope
                            certificate
                            requirement
                        || any
                            ( \certifiedRequirement ->
                                applicationCertificateTransfersRootRequirementOwnership
                                    ownerScope
                                    certificate
                                    certifiedRequirement
                                    requirement
                            )
                            (grRequiredGammaBinders rootRequirements)
                )
                localApplicationCertificates
        applicationDischargesClosure closure =
            any
                ( applicationCertificateDischargesRootClosure
                    ownerScope
                    (grRequiredGammaBinders rootRequirements)
                    closure
                )
                localApplicationCertificates
        rootOwnedPlanningRequirements =
            filter
                (not . applicationOwnsPlanningRequirement)
                (grRequiredGammaBinders requirements0)
        rootOwnedPlanningRouteNodes =
            IntSet.fromList
                [ getNodeId routeNode
                | requirement <- rootOwnedPlanningRequirements
                , routeNode <- requirementRouteNodes requirement
                ]
        locallyClosedApplicationPlanningNodes =
            IntSet.difference
                locallyClosedApplicationNodes
                rootOwnedPlanningRouteNodes
        -- Compare structural closures only with requirements still owned by
        -- the root after exact application evidence.  In particular, an
        -- ambient direct claim proves that its application found an enclosing
        -- declaration; it removes the duplicate direct planning obligation,
        -- while the enclosing closure remains the provenance of that
        -- declaration.
        unownedRootExteriorNodes =
            IntSet.fromList
                [ getNodeId (rgbExteriorNode requirement)
                | requirement <- grRequiredGammaBinders rootRequirements
                , not (exactApplicationOwnsRequirement requirement)
                ]
        conflictingApplicationNodes =
            IntSet.intersection
                unownedRootExteriorNodes
                locallyClosedApplicationNodes
        requirementsWithApplicationCertificates =
            requirements0
                { -- A validated post-environment AApp certificate is positive
                  -- evidence that this requirement's binder was constructed at
                  -- the application boundary.  Keep the node keys as closure
                  -- authority, but do not feed the same requirement back to
                  -- the root binder planner.  Emitted binders are reconstructed
                  -- from the certificate by
                  -- 'prepareRootClosureSchemeWithAmbient'; consumed binders
                  -- leave no root forall.
                  grRequiredGammaBinders = rootOwnedPlanningRequirements
                , grLocallyClosedGammaNodes =
                    IntSet.union
                        (grLocallyClosedGammaNodes requirements0)
                        locallyClosedApplicationPlanningNodes
                }
    unless
        (IntSet.null conflictingApplicationNodes)
        ( Left
            ( ValidationFailed
                [ "an emitted application Gamma is also required by the root scope"
                , "  conflicting nodes: "
                    ++ show (map NodeId (IntSet.toList conflictingApplicationNodes))
                , "  application owners: "
                    ++ show (map lgccOwner localApplicationCertificates)
                ]
            )
        )
    retainedInheritedGammaRoutes <-
        foldM
            Reify.mergeInheritedGammaRoutes
            Reify.emptyInheritedGammaRoutes
            [ routes
            | edgeKey <- IntMap.keys locallyClosedGammas
            , Just routes <-
                [IntMap.lookup edgeKey (rbeInheritedGammaRoutes boundaryEdges)]
            ]
    placedRequirements <-
        placeNestedRootRequirements
            ga
            ownerScope
            locallyClosedGammas
            requirementsWithApplicationCertificates
    pure
        RootBoundaryRequirements
            { -- Edge-only local ownership remains in the complete
              -- requirements and is placed by its closure.  Post-environment
              -- application ownership is stronger: its checked certificate
              -- either reconstructs the emitted binder in
              -- 'prepareRootClosureSchemeWithAmbient' or proves that it was
              -- consumed, so the root planner must not see that requirement a
              -- second time.
              rbrRequirements = placedRequirements
            , rbrLocallyClosedGammas = locallyClosedGammas
            , rbrInheritedGammaRoutes =
                retainedInheritedGammaRoutes
            }

-- | Validate the exact evidence returned by AApp after lexical-environment
-- subtraction.  Unlike the retired graph heuristic, this accepts a local
-- endpoint only when that concrete source occurrence published a non-empty
-- emitted/consumed partition and either a graph route or an exact
-- source-sidecar authority for every prepared binder.  A zero-local
-- certificate is accepted only when its exact direct ambient claim proves
-- which source occurrence had no local route.
validateLocalApplicationCertificates
    :: (EdgeId -> NodeId -> Either ElabError NodeRef)
    -> AnnExpr
    -> IntMap.IntMap TypeBinderRef
    -> [LocalGammaConstructionCertificate]
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
validateLocalApplicationCertificates scopeForBoundary ann sourceBinderRefs certificates = do
    validateLocalApplicationCertificatesWithRepresentative
        id
        scopeForBoundary
        ann
        sourceBinderRefs
        certificates

validateLocalApplicationCertificatesWithRepresentative
    :: (NodeId -> NodeId)
    -> (EdgeId -> NodeId -> Either ElabError NodeRef)
    -> AnnExpr
    -> IntMap.IntMap TypeBinderRef
    -> [LocalGammaConstructionCertificate]
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
validateLocalApplicationCertificatesWithRepresentative identityRepresentative scopeForBoundary ann sourceBinderRefs certificates = do
    unless
        (null duplicateOwners)
        ( Left
            ( ValidationFailed
                [ "duplicate post-environment application Gamma certificate owners"
                , "  owners: " ++ show duplicateOwners
                , "  annotation: " ++ show ann
                ]
            )
        )
    foldM validateCertificate IntMap.empty certificates
  where
    duplicateOwners =
        [ owner
        | (index, owner) <-
            zip [0 :: Int ..] (map lgccOwner certificates)
        , owner `elem` map lgccOwner (drop (index + 1) certificates)
        ]

    validateCertificate accumulated certificate = do
        let owner = lgccOwner certificate
            construction = lgccConstruction certificate
            constructionBinders =
                localGammaConstructionBinders construction
            emittedBinders =
                localGammaEmittedBinders construction
            consumedBinders =
                localGammaConsumedBinders construction
            constructionRefs = map fst constructionBinders
            consumedRefs = map fst consumedBinders
            routes = lgccLocalBinderRoutes certificate
            sourceAuthorities = lgccSourceBinderAuthorities certificate
            usedSourceAuthorities =
                lgccUsedSourceBinderAuthorities certificate
            sourceConstructionRefs =
                map
                    sourceBinderAuthorityConstructionRef
                    (IntMap.elems sourceAuthorities)
            graphRouted ref =
                any
                    (typeBinderRefsSameIdentity ref)
                    (IntMap.elems routes)
            sourceAuthorized ref =
                any
                    (typeBinderRefsSameIdentity ref)
                    sourceConstructionRefs
            externalAmbientRefs =
                lgccUsedAmbientBinderRefs certificate
            enclosingTypeAbsBinders =
                lgccEnclosingTypeAbsBinders certificate
            ambientRefs =
                certificateAvailableAmbientBinderRefs certificate
            ownerOccurrences = applicationOwnerOccurrences owner ann
            occurrenceCount = length ownerOccurrences
            duplicateConstructionRefs =
                [ ref
                | (index, ref) <- zip [0 :: Int ..] constructionRefs
                , any
                    (typeBinderRefsSameIdentity ref)
                    (drop (index + 1) constructionRefs)
                ]
            unauthorizedConstructionRefs =
                [ ref
                | ref <- constructionRefs
                , not (graphRouted ref)
                , not (sourceAuthorized ref)
                ]
            foreignRouteRefs =
                [ routedRef
                | routedRef <- IntMap.elems routes
                , not
                    ( any
                        (typeBinderRefsSameIdentity routedRef)
                        constructionRefs
                    )
                ]
            foreignSourceAuthorityRefs =
                [ constructionRef
                | authority <- IntMap.elems sourceAuthorities
                , let constructionRef =
                        sourceBinderAuthorityConstructionRef authority
                , not
                    ( any
                        (typeBinderRefsSameIdentity constructionRef)
                        constructionRefs
                    )
                ]
            graphSourceAuthorityOverlap =
                [ constructionRef
                | constructionRef <- constructionRefs
                , graphRouted constructionRef
                , sourceAuthorized constructionRef
                ]
            invalidSourceAuthorities =
                [ ( NodeId nodeKey
                  , sourceBinderAuthoritySidecarRef authority
                  , sourceBinderAuthorityConstructionRef authority
                  , IntMap.lookup nodeKey sourceBinderRefs
                  )
                | (nodeKey, authority) <- IntMap.toList sourceAuthorities
                , case IntMap.lookup nodeKey sourceBinderRefs of
                    Just currentSourceRef ->
                        not
                            ( typeBinderRefsSameIdentity
                                (sourceBinderAuthoritySidecarRef authority)
                                currentSourceRef
                            )
                    Nothing -> True
                ]
            invalidUsedSourceAuthorities =
                [ ( NodeId nodeKey
                  , sourceBinderAuthoritySidecarRef authority
                  , sourceBinderAuthorityConstructionRef authority
                  , IntMap.lookup nodeKey sourceBinderRefs
                  )
                | (nodeKey, authority) <-
                    IntMap.toList usedSourceAuthorities
                , case IntMap.lookup nodeKey sourceBinderRefs of
                    Just currentSourceRef ->
                        not
                            ( typeBinderRefsSameIdentity
                                (sourceBinderAuthoritySidecarRef authority)
                                currentSourceRef
                            )
                    Nothing -> True
                ]
            foreignUsedSourceAuthorities =
                [ constructionRef
                | authority <- IntMap.elems usedSourceAuthorities
                , let constructionRef =
                        sourceBinderAuthorityConstructionRef authority
                , not
                    ( any
                        (typeBinderRefsSameIdentity constructionRef)
                        ambientRefs
                    )
                ]
            duplicateAmbientRefs =
                [ ref
                | (index, ref) <- zip [0 :: Int ..] ambientRefs
                , any
                    (typeBinderRefsSameIdentity ref)
                    (drop (index + 1) ambientRefs)
                ]
            invalidEnclosingBoundDependencies =
                [ (enclosingRef, dependency)
                | (binderIndex, (enclosingRef, Just bound)) <-
                    zip [0 :: Int ..] enclosingTypeAbsBinders
                , dependency <- freeTypeVarRefsType (tyToElab bound)
                , not
                    ( any
                        (typeBinderRefsSameIdentity dependency)
                        ( externalAmbientRefs
                            ++ constructionRefs
                            ++ map
                                fst
                                (take binderIndex enclosingTypeAbsBinders)
                        )
                    )
                ]
            localAmbientOverlap =
                [ ambientRef
                | ambientRef <- ambientRefs
                , any
                    (typeBinderRefsSameIdentity ambientRef)
                    constructionRefs
                ]
            (constructedBinders, _) =
                splitForallsRefs (lgccConstructedType certificate)
            constructedPrefix = take (length emittedBinders) constructedBinders
            prefixMismatch =
                length constructedPrefix /= length emittedBinders
                    || not
                        ( and
                            ( zipWith
                                bindersAgree
                                emittedBinders
                                constructedPrefix
                            )
                        )
            constructedRefs =
                typeBinderDeclarationRefs (lgccConstructedType certificate)
                    ++ freeTypeVarRefsType (lgccConstructedType certificate)
            unconsumedRefs =
                [ consumedRef
                | consumedRef <- consumedRefs
                , any
                    (typeBinderRefsSameIdentity consumedRef)
                    constructedRefs
                ]
            uncoveredConstructedFreeRefs =
                [ freeRef
                | freeRef <-
                    freeTypeVarRefsType (lgccConstructedType certificate)
                , not
                    ( any
                        (typeBinderRefsSameIdentity freeRef)
                        ambientRefs
                    )
                ]
        unless
            (lgoConstructor owner == LocalApplicationGamma)
            (certificateFailure certificate "owner is not an application constructor")
        unless
            (occurrenceCount == 1)
            ( certificateFailure
                certificate
                ( "owner occurrence count is "
                    ++ show occurrenceCount
                    ++ ", expected exactly one"
                )
            )
        expectedScope <-
            scopeForBoundary
                (lgoBoundaryEdge owner)
                (lgoTermNode owner)
        unless
            (expectedScope == localGammaOwnerScope owner)
            ( certificateFailure
                certificate
                ( "owner scope disagrees with prepared boundary: "
                    ++ show expectedScope
                )
            )
        ownerOccurrence <-
            case ownerOccurrences of
                [occurrence] -> pure occurrence
                _ ->
                    certificateFailure
                        certificate
                        "cannot recover the unique source application occurrence"
        ownerFrame <-
            localGammaFrame
                scopeForBoundary
                ownerOccurrence
        unless
            (lgfOwner ownerFrame == Just owner)
            ( certificateFailure
                certificate
                ( "source application frame disagrees with certificate owner: "
                    ++ show (lgfOwner ownerFrame)
                )
            )
        let directFrameEdges =
                map fst (lgfDirectEdgeSources ownerFrame)
            directEdgeKeys =
                IntSet.fromList
                    (map getEdgeId directFrameEdges)
            certificateDirectEdges =
                NonEmpty.toList
                    (lgccDirectApplicationSourceEdgeIds certificate)
            certificateDirectEdgeKeys =
                IntSet.fromList
                    (map getEdgeId certificateDirectEdges)
            directClaims =
                lgccDirectApplicationGammaClaims certificate
            ambientDirectClaims =
                lgccDirectApplicationAmbientGammaClaims certificate
            directClaimEdgeSets =
                map dagcEdgeIds directClaims
                    ++ map daagcEdgeIds ambientDirectClaims
            overlappingDirectClaimEdgeSets =
                [ claimEdges
                | (index, claimEdges) <-
                    zip [0 :: Int ..] directClaimEdgeSets
                , any
                    (directClaimEdgeSetsOverlap claimEdges)
                    (drop (index + 1) directClaimEdgeSets)
                ]
        unless
            ( length certificateDirectEdges
                == IntSet.size certificateDirectEdgeKeys
                && certificateDirectEdgeKeys == directEdgeKeys
            )
            ( certificateFailure
                certificate
                ( "direct source-edge universe disagrees with the exact application frame: "
                    ++ show certificateDirectEdges
                    ++ " /= "
                    ++ show directFrameEdges
                )
            )
        unless
            (null overlappingDirectClaimEdgeSets)
            ( certificateFailure
                certificate
                ( "direct requirement claims overlap one source edge: "
                    ++ show overlappingDirectClaimEdgeSets
                )
            )
        unless
            (ambientDeclarationAuthoritiesMatchClaims certificate)
            ( certificateFailure
                certificate
                "ambient declaration authorities do not correspond one-to-one with direct ambient claims"
            )
        mapM_
            (validateDirectClaim certificate directEdgeKeys)
            directClaims
        mapM_
            (validateAmbientDirectClaim certificate directEdgeKeys)
            ambientDirectClaims
        unless
            ( construction /= LocalGammaAmbient
                || ( null constructionBinders
                        && IntMap.null routes
                        && IntMap.null sourceAuthorities
                        && null directClaims
                        && not (null ambientDirectClaims)
                   )
            )
            ( certificateFailure
                certificate
                "ambient construction lacks an exact zero-local direct claim"
            )
        unless
            (null duplicateConstructionRefs)
            ( certificateFailure
                certificate
                ( "duplicate construction binders: "
                    ++ show duplicateConstructionRefs
                )
            )
        unless
            (null unauthorizedConstructionRefs)
            ( certificateFailure
                certificate
                ( "construction binders have neither graph nor source authority: "
                    ++ show unauthorizedConstructionRefs
                )
            )
        unless
            (null foreignRouteRefs)
            ( certificateFailure
                certificate
                ( "routes mention binders not constructed here: "
                    ++ show foreignRouteRefs
                )
            )
        unless
            (null foreignSourceAuthorityRefs)
            ( certificateFailure
                certificate
                ( "source authorities mention binders not constructed here: "
                    ++ show foreignSourceAuthorityRefs
                )
            )
        unless
            (null graphSourceAuthorityOverlap)
            ( certificateFailure
                certificate
                ( "construction binders claim both graph and source authority: "
                    ++ show graphSourceAuthorityOverlap
                )
            )
        unless
            (null invalidSourceAuthorities)
            ( certificateFailure
                certificate
                ( "source binder authorities disagree with the prepared source sidecar: "
                    ++ show invalidSourceAuthorities
                )
            )
        unless
            (null invalidUsedSourceAuthorities)
            ( certificateFailure
                certificate
                ( "used source binder authorities disagree with the prepared source sidecar: "
                    ++ show invalidUsedSourceAuthorities
                )
            )
        unless
            (null foreignUsedSourceAuthorities)
            ( certificateFailure
                certificate
                ( "used source binder authorities do not name ambient dependencies: "
                    ++ show foreignUsedSourceAuthorities
                )
            )
        unless
            (null duplicateAmbientRefs)
            ( certificateFailure
                certificate
                ("duplicate ambient binders: " ++ show duplicateAmbientRefs)
            )
        unless
            (null localAmbientOverlap)
            ( certificateFailure
                certificate
                ("local and ambient binders overlap: " ++ show localAmbientOverlap)
            )
        unless
            (not prefixMismatch)
            ( certificateFailure
                certificate
                ( "constructed type does not start with the emitted binder prefix: "
                    ++ show constructedPrefix
                )
            )
        unless
            (null invalidEnclosingBoundDependencies)
            ( certificateFailure
                certificate
                ( "enclosing let-RHS type abstractions have unscoped bound dependencies: "
                    ++ show invalidEnclosingBoundDependencies
                )
            )
        unless
            (null unconsumedRefs)
            ( certificateFailure
                certificate
                ( "consumed binders still occur in the constructed type: "
                    ++ show unconsumedRefs
                )
            )
        unless
            (null uncoveredConstructedFreeRefs)
            ( certificateFailure
                certificate
                ( "constructed type uses ambient binders omitted from the certificate: "
                    ++ show uncoveredConstructedFreeRefs
                )
            )
        foldM (insertRoute certificate) accumulated (IntMap.toList routes)

    insertRoute _certificate accumulated (nodeKey, emittedRef) =
        case IntMap.lookup nodeKey accumulated of
            Nothing -> pure (IntMap.insert nodeKey emittedRef accumulated)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef emittedRef -> pure accumulated
                | otherwise ->
                    -- Route values are constructor-local binder identities.
                    -- Distinct, occurrence-validated applications may share
                    -- one solved graph node while constructing different
                    -- local binders.  Root planning consumes only this map's
                    -- keys as local-closure evidence; the exact per-owner
                    -- route remains in each certificate.
                    pure accumulated

    certificateFailure
        :: LocalGammaConstructionCertificate
        -> String
        -> Either ElabError a
    certificateFailure certificate detail =
        Left
            ( ValidationFailed
                [ "invalid post-environment application Gamma certificate"
                , "  detail: " ++ detail
                , "  certificate: " ++ show certificate
                , "  annotation: " ++ show ann
                ]
            )

    bindersAgree (leftRef, leftBound) (rightRef, rightBound) =
        typeBinderRefsSameIdentity leftRef rightRef
            && equivalentBounds leftBound rightBound

    equivalentBounds left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

    validateDirectClaim certificate directEdgeKeys claim = do
        let claimEdgeList = NonEmpty.toList (dagcEdgeIds claim)
            claimEdgeKeys =
                IntSet.fromList (map getEdgeId claimEdgeList)
        unless
            ( length claimEdgeList == IntSet.size claimEdgeKeys
                && claimEdgeKeys `IntSet.isSubsetOf` directEdgeKeys
            )
            ( certificateFailure
                certificate
                ( "direct claim names an edge outside its source application frame: "
                    ++ show claim
                )
            )
        unless
            ( routesAllReachRef
                certificate
                (directClaimRouteNodes claim)
                (dagcBinderRef claim)
            )
            ( certificateFailure
                certificate
                ( "direct claim endpoints do not route to its constructed binder: "
                    ++ show claim
                )
            )
        unless
            ( case constructionBinderForRef certificate (dagcBinderRef claim) of
                Just constructedBound ->
                    boundsEquivalent
                        constructedBound
                        (dagcConstructedBound claim)
                        && projectedBoundProvidesType
                            constructedBound
                            (dagcOperatedType claim)
                Nothing -> False
            )
            ( certificateFailure
                certificate
                ( "direct claim ref/bound disagrees with the construction: "
                    ++ show claim
                )
            )

    projectedBoundProvidesType mbBound ty =
        let projectedBound =
                resolveSourceBinderAliasesInType
                    identityRepresentative
                    sourceBinderRefs
                    (maybe TBottom tyToElab mbBound)
            projectedType =
                resolveSourceBinderAliasesInType
                    identityRepresentative
                    sourceBinderRefs
                    ty
        in constructionEndpointProvides projectedBound projectedType

    validateAmbientDirectClaim certificate directEdgeKeys claim = do
        let claimEdgeList = NonEmpty.toList (daagcEdgeIds claim)
            claimEdgeKeys =
                IntSet.fromList (map getEdgeId claimEdgeList)
        unless
            ( length claimEdgeList == IntSet.size claimEdgeKeys
                && claimEdgeKeys `IntSet.isSubsetOf` directEdgeKeys
            )
            ( certificateFailure
                certificate
                ( "ambient direct claim names an edge outside its source application frame: "
                    ++ show claim
                )
            )
        unless
            (ambientDirectApplicationGammaClaimConstructionValid certificate claim)
            ( certificateFailure
                certificate
                ( "ambient direct claim has invalid declaration authority, a local route, or an uncertified bound dependency: "
                    ++ show claim
                )
            )

    directClaimEdgeSetsOverlap left right =
        not
            ( IntSet.null
                ( IntSet.intersection
                    (edgeKeySet left)
                    (edgeKeySet right)
                )
            )
      where
        edgeKeySet =
            IntSet.fromList . map getEdgeId . NonEmpty.toList

    applicationOwnerOccurrences :: LocalGammaOwner -> AnnExpr -> [AnnExpr]
    applicationOwnerOccurrences owner = go
      where
        go expr =
            [expr | ownerHere expr]
                ++ case expr of
                    AResolvedVar{} -> []
                    ALit{} -> []
                    ALam _ _ _ _ body _ _ -> go body
                    AApp fun arg _ _ _ -> go fun ++ go arg
                    ALet _ _ _ _ _ _ rhs body _ -> go rhs ++ go body
                    AExactAnn inner _ _ _ -> go inner
                    AAnn inner _ _ -> go inner
                    ALetScope inner _ _ -> go inner
                    AUnfold inner _ _ -> go inner

        ownerHere expr =
            case expr of
                AApp _ _ funSite _ applicationNode
                    | lgoBoundaryEdge owner == instantiationSiteEdgeId funSite
                    , lgoTermNode owner == applicationNode -> True
                _ -> False

-- | Attach the exact Figure 15.3.5 owner while construction evidence is still
-- available.  A term-local closure is the strongest authority.  Its source
-- constructor can be at the current scope, below it, or above a deeper result
-- scope: the last case is an exact construction-scope placement, not a nested
-- placement.  Unrelated scopes remain invalid.
--
-- If no term constructor owns the edge, the frozen binding tree supplies the
-- structural owner: the first gen reached through the exterior's flexible
-- path.  That gen must be the current construction scope or a proven
-- descendant of it.  A parentless exterior has no such path; it belongs to
-- the current construction only when it is itself an exact result endpoint
-- of the root RaiseMerge.  Stamp that proof as an exact construction-scope
-- placement so later planners validate an already-constructed placement
-- instead of repairing a root fallback.
placeNestedRootRequirements
    :: GaBindParents 'Presolved
    -> NodeRef
    -> IntMap.IntMap LocalGammaClosure
    -> GeneralizationRequirements
    -> Either ElabError GeneralizationRequirements
placeNestedRootRequirements ga currentScope locallyClosed requirements = do
    placed <- traverse placeRequirement (grRequiredGammaBinders requirements)
    pure
        requirements
            { grRequiredGammaBinders = placed
            }
  where
    placeRequirement requirement =
        case presentClosures of
            [] -> placeFromFrozenExterior requirement
            closures@(closure : rest)
                | length closures /= length requirementEdges ->
                    placementFailure
                        "only part of a root Gamma requirement is owned by a nested constructor"
                        requirement
                        closures
                | any (/= closure) rest ->
                    placementFailure
                        "one root Gamma requirement is split across nested constructors"
                        requirement
                        closures
                | edgeKeySet (lgcEdgeIds closure) /= edgeKeySet (rgbEdgeIds requirement) ->
                    placementFailure
                        "nested constructor does not own the complete root Gamma requirement"
                        requirement
                        closures
                | lgcExteriorNode closure /= rgbExteriorNode requirement ->
                    placementFailure
                        "nested constructor exterior disagrees with the root Gamma requirement"
                        requirement
                        closures
                | lgcConsumerIdentity closure
                    /= typeBinderIdentityFromNode (rgbExteriorNode requirement) ->
                    placementFailure
                        "nested constructor consumer disagrees with the root Gamma requirement"
                        requirement
                        closures
                | otherwise ->
                    placeFromLocalClosure requirement closure
      where
        requirementEdges = NonEmpty.toList (rgbEdgeIds requirement)
        presentClosures =
            [ closure
            | edgeId <- requirementEdges
            , Just closure <- [closureForEdge edgeId]
            ]

    closureForEdge edgeId =
        IntMap.lookup (getEdgeId edgeId) locallyClosed

    placeFromLocalClosure requirement closure = do
        currentOwner <- currentScopeGenOwner requirement
        let closureOwner = localGammaOwnerScope (lgcOwner closure)
        placement <-
            if closureOwner == currentOwner
                then pure (RequiredGammaAtNestedScope closureOwner)
                else do
                    closureOwnerPath <-
                        bindingPathToRootLocal bindParents closureOwner
                    currentOwnerPath <-
                        bindingPathToRootLocal bindParents currentOwner
                    let closureIsNested =
                            currentOwner `elem` drop 1 closureOwnerPath
                        closureContainsCurrent =
                            closureOwner `elem` drop 1 currentOwnerPath
                    case () of
                        _
                            | closureIsNested ->
                                pure (RequiredGammaAtNestedScope closureOwner)
                            | closureContainsCurrent ->
                                pure
                                    ( RequiredGammaAtConstructionScope
                                        closureOwner
                                    )
                            | otherwise ->
                                localClosurePlacementFailure
                                    "local Gamma owner is unrelated to the current construction scope"
                                    requirement
                                    closureOwner
                                    currentOwner
                                    closureOwnerPath
                                    currentOwnerPath
        pure requirement {rgbPlacement = placement}

    placeFromFrozenExterior requirement = do
        currentOwner <- currentScopeGenOwner requirement
        if parentlessResultExterior requirement
            then
                pure
                    requirement
                        { rgbPlacement =
                            RequiredGammaAtConstructionScope currentOwner
                        }
            else do
                exteriorOwner <- nearestFlexibleExteriorOwner requirement
                if exteriorOwner == currentOwner
                    then
                        pure
                            requirement
                                { rgbPlacement = RequiredGammaAtCurrentScope
                                }
                    else do
                        ownerPath <-
                            bindingPathToRootLocal bindParents exteriorOwner
                        let ownerIsContained =
                                case ownerPath of
                                    _ : ancestors -> currentOwner `elem` ancestors
                                    [] -> False
                        if ownerIsContained
                            then
                                pure
                                    requirement
                                        { rgbPlacement =
                                            RequiredGammaAtNestedScope exteriorOwner
                                        }
                            else
                                structuralPlacementFailure
                                    "frozen root Gamma owner is not contained by the current construction scope"
                                    requirement
                                    exteriorOwner
                                    currentOwner
                                    ownerPath

    parentlessResultExterior requirement =
        IntMap.notMember
            (nodeRefKey (typeRef exterior))
            bindParents
            && exterior `elem` rgbResultRoots requirement
      where
        exterior = rgbExteriorNode requirement

    nearestFlexibleExteriorOwner requirement =
        go IntSet.empty (typeRef (rgbExteriorNode requirement))
      where
        go seen child
            | IntSet.member childKey seen =
                structuralPathFailure
                    "frozen root Gamma exterior path is cyclic"
                    requirement
            | otherwise =
                case IntMap.lookup childKey bindParents of
                    Just (parent@GenRef{}, BindFlex) -> pure parent
                    Just (parent@TypeRef{}, BindFlex) ->
                        go (IntSet.insert childKey seen) parent
                    Just (_, BindRigid) ->
                        structuralPathFailure
                            "frozen root Gamma exterior crosses a rigid binding"
                            requirement
                    Nothing ->
                        structuralPathFailure
                            "frozen root Gamma exterior has no owning gen"
                            requirement
          where
            childKey = nodeRefKey child

    currentScopeGenOwner requirement =
        case currentScope of
            owner@GenRef{} -> pure owner
            TypeRef{} -> do
                path <- bindingPathToRootLocal bindParents currentScope
                case [owner | owner@GenRef{} <- drop 1 path] of
                    owner : _ -> pure owner
                    [] ->
                        structuralPathFailure
                            "current root construction scope has no owning gen"
                            requirement

    bindParents = gaBindParentsBase ga

    edgeKeySet = IntSet.fromList . map getEdgeId . NonEmpty.toList

    placementFailure reason requirement closures =
        Left
            ( ValidationFailed
                [ reason
                , "  requirement: " ++ show requirement
                , "  closures: " ++ show closures
                ]
            )

    structuralPathFailure reason requirement =
        Left
            ( ValidationFailed
                [ reason
                , "  requirement: " ++ show requirement
                , "  current construction scope: " ++ show currentScope
                , "  exterior path: "
                    ++ show
                        ( bindingPathToRootLocal
                            bindParents
                            (typeRef (rgbExteriorNode requirement))
                        )
                ]
            )

    structuralPlacementFailure reason requirement exteriorOwner currentOwner ownerPath =
        Left
            ( ValidationFailed
                [ reason
                , "  requirement: " ++ show requirement
                , "  frozen exterior owner: " ++ show exteriorOwner
                , "  current construction owner: " ++ show currentOwner
                , "  owner path: " ++ show ownerPath
                ]
            )

    localClosurePlacementFailure reason requirement closureOwner currentOwner closureOwnerPath currentOwnerPath =
        Left
            ( ValidationFailed
                [ reason
                , "  requirement: " ++ show requirement
                , "  local Gamma owner: " ++ show closureOwner
                , "  current construction owner: " ++ show currentOwner
                , "  local Gamma owner path: " ++ show closureOwnerPath
                , "  current construction owner path: " ++ show currentOwnerPath
                ]
            )

generalizationRequirementsForScopeEdges
    :: [EdgeId]
    -> (AnnExpr -> Either ElabError [EdgeId])
    -> (NodeId -> NodeId)
    -> (NodeId -> NodeId)
    -> GaBindParents 'Presolved
    -> NodeRef
    -> PresolutionView 'Presolved
    -> EdgeArtifacts
    -> IntMap.IntMap ElabType
    -> IntMap.IntMap TypeBinderRef
    -> SubtermGeneralizations
    -> [(EdgeId, Maybe RootEdgeExactEndpoint)]
    -> Maybe ElabType
    -> AnnExpr
    -> Either ElabError GeneralizationRequirements
generalizationRequirementsForScopeEdges localEdges edgeSelector identityRepresentative constructionCanonical ga ownerScope presolutionView edgeArtifacts exactProducerTypes sourceBinderRefs subtermPackets explicitEdges expectedType ann = do
    (rootEdge, exactOperatedType) <- rootLambdaBodyRequirement expectedType ann
    selectedEdges <- edgeSelector ann
    ownedEdges <- filterM ownsEdge selectedEdges
    let inferredEdges =
            [ (edgeId, RootEdgeExactProducer <$> exactFor edgeId)
            | edgeId <- ownedEdges
            ]
        exactFor edgeId
            | Just edgeId == rootEdge =
                IntMap.lookup (getEdgeId edgeId) exactProducerTypes
                    <|> exactOperatedType
            | otherwise = IntMap.lookup (getEdgeId edgeId) exactProducerTypes
        edgesById =
            IntMap.fromListWith preferExact
                [ (getEdgeId edgeId, (edgeId, mbExact))
                | (edgeId, mbExact) <- inferredEdges ++ explicitEdges
                ]
    case
        generalizationRequirementsForEnclosingRootExactEdges
            identityRepresentative
            constructionCanonical
            ga
            presolutionView
            edgeArtifacts
            sourceBinderRefs
            subtermPackets
            (IntMap.elems edgesById)
      of
        Right requirements -> pure requirements
        Left cause ->
            Left
                ( ValidationFailed
                    [ "scope-edge Gamma requirement planning failed"
                    , "  owner scope: " ++ show ownerScope
                    , "  local edges: " ++ show localEdges
                    , "  selected edges: " ++ show selectedEdges
                    , "  owned edges: " ++ show ownedEdges
                    , "  inferred edges: " ++ show inferredEdges
                    , "  explicit edges: " ++ show explicitEdges
                    , "  merged edges: " ++ show (IntMap.elems edgesById)
                    , "  subterm packet authorities: "
                        ++ show
                            [ ( key
                              , subtermGeneralizationConsumerAuthority packet
                              , subtermGeneralizationGammaAuthority packet
                              )
                            | (key, packet) <- Map.toList subtermPackets
                            ]
                    , "  expected type: " ++ show expectedType
                    , "  annotated term: " ++ show ann
                    , "  cause: " ++ show cause
                    ]
                )
  where
    rootLambdaBodyRequirement expected expr =
        case expr of
            AExactAnn inner _ _ edgeId -> do
                exactType <-
                    case IntMap.lookup (getEdgeId edgeId) exactProducerTypes of
                        Just ty -> pure ty
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "compiler exact lambda boundary has no edge plan"
                                    , "  edge: " ++ show edgeId
                                    ]
                                )
                rootLambdaBodyRequirement (Just exactType) inner
            AAnn inner _ _ -> rootLambdaBodyRequirement Nothing inner
            ALam _ _ _ _ _ edgeId _ ->
                pure (Just edgeId, expected >>= exactLambdaBodyOperatedType)
            ALetScope inner _ _ -> rootLambdaBodyRequirement expected inner
            AUnfold inner _ _ -> rootLambdaBodyRequirement Nothing inner
            _ -> pure (Nothing, Nothing)

    ownsEdge edgeId = do
        if edgeId `elem` localEdges
            then pure True
            else do
                authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
                pure $
                    case authority of
                        Just rootAuthority ->
                            rootRaiseMergeExteriorOwnedByScope
                                ga
                                ownerScope
                                (rrmaExterior rootAuthority)
                        Nothing -> False

    preferExact left@(_, Just _) _ = left
    preferExact _ right = right

annotationInstantiationEdges :: AnnExpr -> [EdgeId]
annotationInstantiationEdges ann =
    case ann of
        AResolvedVar{} -> []
        ALit{} -> []
        ALam _ _ _ _ body bodyEdge _ ->
            bodyEdge : annotationInstantiationEdges body
        AApp fun arg funSite argSite _ ->
            instantiationSiteEdgeId funSite
                : instantiationSiteEdgeId argSite
                : (annotationInstantiationEdges fun ++ annotationInstantiationEdges arg)
        ALet _ _ _ _ _ _ rhs body _ ->
            annotationInstantiationEdges rhs ++ annotationInstantiationEdges body
        AExactAnn inner _ _ edgeId ->
            edgeId : annotationInstantiationEdges inner
        AAnn inner _ edgeId ->
            edgeId : annotationInstantiationEdges inner
        ALetScope inner _ edgeId ->
            edgeId : annotationInstantiationEdges inner
        AUnfold inner _ edgeId ->
            edgeId : annotationInstantiationEdges inner

-- Applications defer the two instantiation computations named directly by
-- their source frame to the AApp edge-local construction lane; that edge
-- identity is Figure 15.3.5 occurrence provenance even when the frozen
-- exterior remains under an enclosing graph scope.  Inherited
-- application/let/lambda edges still require flexible scope ownership.
-- Nested construction boundaries accumulate ownership: a
-- descendant edge closed by any local application or let is no longer a
-- root-boundary obligation.  This mirrors 'withLetConstructionGamma'; failing
-- to record a let scope here makes the root install the same Gamma a second
-- time around the whole term.
rootBoundaryInstantiationEdges
    :: (EdgeId -> NodeId -> Either ElabError NodeRef)
    -> GaBindParents 'Presolved
    -> EdgeArtifacts
    -> [PreparedSubtermGeneralization]
    -> AnnExpr
    -> Either ElabError RootBoundaryEdges
rootBoundaryInstantiationEdges scopeForBoundary ga edgeArtifacts packets ann = do
    directApplicationOwners <-
        localGammaDirectApplicationEdgeOwners
            scopeForBoundary
            ann
    preparedEnclosingOwners <-
        localGammaPreparedEnclosingEdgeOwners packets
    boundary <-
        go
            directApplicationOwners
            preparedEnclosingOwners
            []
            ann
    groupBoundaryClosures boundary
  where
    go directApplicationOwners preparedEnclosingOwners localOwners expr = do
        frame <- localGammaFrame scopeForBoundary expr
        let localOwners' =
                maybe localOwners (: localOwners) (lgfOwner frame)
        collect
            directApplicationOwners
            preparedEnclosingOwners
            localOwners'
            (map fst (lgfDirectEdgeSources frame))
            (lgfChildren frame)

    collect directApplicationOwners preparedEnclosingOwners localOwners edges children = do
        edgeClaims <-
            traverse
                (\edgeId -> do
                    mbClosure <-
                        claimedClosure
                            directApplicationOwners
                            preparedEnclosingOwners
                            localOwners
                            edgeId
                    pure (edgeId, mbClosure)
                )
                edges
        descendants <-
            traverse
                ( go
                    directApplicationOwners
                    preparedEnclosingOwners
                    localOwners
                )
                children
        directClosures <-
            foldM
                insertClosure
                IntMap.empty
                [ (getEdgeId edgeId, closure)
                | (edgeId, Just (closure, _)) <- edgeClaims
                ]
        let directInheritedGammaRoutes =
                IntMap.fromList
                    [ (getEdgeId edgeId, routes)
                    | (edgeId, Just (_, routes)) <- edgeClaims
                    ]
        locallyClosedGammas <-
            foldM
                mergeClosureMaps
                directClosures
                (map rbeLocallyClosedGammas descendants)
        inheritedGammaRoutes <-
            foldM
                mergeRouteMaps
                directInheritedGammaRoutes
                (map rbeInheritedGammaRoutes descendants)
        let unclaimedCandidates =
                [ edgeId
                | (edgeId, Nothing) <- edgeClaims
                ]
                    ++ concatMap rbeUnclaimedEdges descendants
            -- One source edge can be named both by its AApp
            -- instantiation site and by the transparent annotation wrapper
            -- around that operand.  Edge identity is unique in the paper, so
            -- a direct local claim is authoritative over the duplicate
            -- wrapper visit.  Keeping both would feed the same edge back to
            -- the root planner after selecting its edge-local AApp lane.
            unclaimedEdges =
                unclaimedEdgesOutsideLocalGammaClosures
                    locallyClosedGammas
                    unclaimedCandidates
        pure
            RootBoundaryEdges
                { rbeUnclaimedEdges = unclaimedEdges
                , rbeLocallyClosedGammas = locallyClosedGammas
                , rbeInheritedGammaRoutes = inheritedGammaRoutes
                }

    claimedClosure directApplicationOwners preparedEnclosingOwners localOwners edgeId = do
        authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
        case authority of
            Just rootAuthority -> do
                mbOwner <- closureOwner rootAuthority
                case mbOwner of
                    Nothing -> pure Nothing
                    Just
                        ( owner
                          , directApplicationEdges
                          , forwardedResultEdges
                          ) -> do
                        ownerScheme <- packetOwnerScheme owner
                        inheritedRoutes <-
                            packetInheritedGammaRoutes rootAuthority
                        pure
                            ( Just
                                ( LocalGammaClosure
                                    { lgcEdgeIds = NonEmpty.singleton edgeId
                                    , lgcDirectApplicationEdgeIds =
                                        directApplicationEdges
                                    , lgcForwardedResultEdgeIds =
                                        forwardedResultEdges
                                    , lgcExteriorNode = rrmaExterior rootAuthority
                                    , lgcConsumerIdentity =
                                        typeBinderIdentityFromNode
                                            (rrmaExterior rootAuthority)
                                    , lgcOwner = owner
                                    , lgcOwnerPendingScheme = ownerScheme
                                    }
                                , inheritedRoutes
                                )
                            )
            _ -> pure Nothing
      where
        packetOwnerScheme owner =
            case ownerPackets owner of
                [] -> pure Nothing
                [packet] ->
                    pure
                        ( Just
                            (subtermGeneralizationConsumerConstructionSchemeInfo packet)
                        )
                matches ->
                    Left
                        ( ValidationFailed
                            [ "multiple prepared packets claim one local Gamma closure"
                            , "  edge: " ++ show edgeId
                            , "  owner: " ++ show owner
                            , "  packet count: " ++ show (length matches)
                            ]
                        )

        ownerPackets owner =
            [ packet
            | packet <- packets
            , lgoConstructor owner == LocalLambdaGamma
            , Just packetAuthority <-
                [subtermGeneralizationGammaAuthority packet]
            , gpaEdgeId packetAuthority == edgeId
            , genRef (gpaOwnerGen packetAuthority)
                == localGammaOwnerScope owner
            ]

        packetInheritedGammaRoutes rootAuthority =
            foldM
                Reify.mergeInheritedGammaRoutes
                Reify.emptyInheritedGammaRoutes
                [ subtermGeneralizationInheritedGammaRoutes packet
                | packet <- packets
                , Just consumerAuthority <-
                    [subtermGeneralizationConsumerAuthority packet]
                , scaEdgeId consumerAuthority == edgeId
                , scaConsumerIdentity consumerAuthority
                    == typeBinderIdentityFromNode
                        (rrmaExterior rootAuthority)
                ]

        closureOwner rootAuthority = do
            ownership <-
                selectLocalGammaEdgeOwnership
                    directApplicationOwners
                    preparedEnclosingOwners
                    edgeId
                    localOwners
                    ownsExterior
            case ownership of
                Just (DirectApplicationEdgeOwnership owner) ->
                    -- Figure 15.3.5 sends each of an application's two
                    -- direct instantiation computations through its
                    -- edge-local construction lane.  The syntax-owned edge
                    -- is stable occurrence proof even when the frozen
                    -- exterior remains bound on an enclosing gen.
                    pure (Just (owner, [edgeId], []))
                Just (PreparedEnclosingEdgeOwnership owner) ->
                    pure (Just (owner, [], [edgeId]))
                Just (FlexibleExteriorEdgeOwnership owner) ->
                    pure (Just (owner, [], []))
                Nothing -> pure Nothing
          where
            ownsExterior owner =
                rootRaiseMergeExteriorOwnedByScope
                    ga
                    (localGammaOwnerScope owner)
                    (rrmaExterior rootAuthority)

    mergeClosureMaps closures incoming =
        foldM insertClosure closures (IntMap.toList incoming)

    mergeRouteMaps routes incoming =
        foldM insertRoutes routes (IntMap.toList incoming)

    insertRoutes routes (edgeKey, incoming) =
        case IntMap.lookup edgeKey routes of
            Nothing -> pure (IntMap.insert edgeKey incoming routes)
            Just existing -> do
                merged <-
                    Reify.mergeInheritedGammaRoutes existing incoming
                pure (IntMap.insert edgeKey merged routes)

    insertClosure closures (edgeKey, closure) =
        case IntMap.lookup edgeKey closures of
            Nothing -> pure (IntMap.insert edgeKey closure closures)
            Just existing
                | existing == closure ->
                    -- A transparent operand wrapper can revisit the exact
                    -- direct application edge.  Sticky direct ownership
                    -- makes both visits produce the same proof; retain it
                    -- once instead of reporting two constructors.
                    pure closures
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "one instantiation edge is claimed by multiple local Gamma constructors"
                            , "  edge: " ++ show (EdgeId edgeKey)
                            , "  first: " ++ show existing
                            , "  second: " ++ show closure
                            ]
                        )

    groupBoundaryClosures boundary = do
        groups <-
            foldM
                insertClosureGroup
                []
                (IntMap.elems (rbeLocallyClosedGammas boundary))
        pure
            boundary
                { rbeLocallyClosedGammas =
                    IntMap.fromList
                        [ (getEdgeId edgeId, closure)
                        | closure <- groups
                        , edgeId <- NonEmpty.toList (lgcEdgeIds closure)
                        ]
                }

    insertClosureGroup groups closure =
        case break (sameClosureGroup closure) groups of
            (_, []) -> pure (groups ++ [closure])
            (before, existing : after) -> do
                let (laterMatches, laterUnmatched) =
                        List.partition
                            (sameClosureGroup closure)
                            after
                merged <-
                    foldM
                        mergeClosureGroup
                        existing
                        (closure : laterMatches)
                pure
                    ( before
                        ++ [merged]
                        ++ laterUnmatched
                    )

    mergeClosureGroup existing incoming = do
        pendingOwnerScheme <-
            mergePendingOwnerSchemes existing incoming
        let ownersDiffer = lgcOwner existing /= lgcOwner incoming
            (ownerClosure, forwardedClosure) =
                preferDirectApplicationOwner existing incoming
            forwardedEdges =
                lgcForwardedResultEdgeIds ownerClosure
                    ++ lgcForwardedResultEdgeIds forwardedClosure
                    ++ [ edgeId
                       | ownersDiffer
                       , edgeId <-
                            NonEmpty.toList
                                (lgcEdgeIds forwardedClosure)
                       ]
        pure
            ownerClosure
                { lgcEdgeIds =
                    foldl
                        appendEdgeId
                        (lgcEdgeIds ownerClosure)
                        (NonEmpty.toList (lgcEdgeIds forwardedClosure))
                , lgcDirectApplicationEdgeIds =
                    foldl
                        appendDirectApplicationEdgeId
                        (lgcDirectApplicationEdgeIds ownerClosure)
                        (lgcDirectApplicationEdgeIds forwardedClosure)
                , lgcForwardedResultEdgeIds =
                    foldl
                        appendDirectApplicationEdgeId
                        []
                        forwardedEdges
                , lgcOwnerPendingScheme = pendingOwnerScheme
                }

    -- Grouping has already proved that both visits denote one exact
    -- owner/exterior/consumer obligation.  A missing packet scheme is
    -- therefore absence of information, not evidence for a competing
    -- construction.  Join the partial observations and reject only two
    -- genuinely different constructed schemes.
    mergePendingOwnerSchemes existing incoming =
        case
            ( lgcOwnerPendingScheme existing
            , lgcOwnerPendingScheme incoming
            )
        of
            (Nothing, pending) -> pure pending
            (pending, Nothing) -> pure pending
            (Just firstScheme, Just secondScheme)
                | firstScheme == secondScheme -> pure (Just firstScheme)
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "one local Gamma obligation has conflicting pending owner schemes"
                            , "  owner: " ++ show (lgcOwner incoming)
                            , "  exterior: " ++ show (lgcExteriorNode incoming)
                            , "  first scheme: " ++ show firstScheme
                            , "  second scheme: " ++ show secondScheme
                            ]
                        )

    sameClosureGroup left right =
        lgcExteriorNode left == lgcExteriorNode right
            && lgcConsumerIdentity left == lgcConsumerIdentity right
            && ( lgcOwner left == lgcOwner right
                    || transparentDirectApplicationGroup left right
               )

    -- A result-transparent wrapper can contribute another edge to the exact
    -- exterior constructed by a direct application.  While the annotated
    -- tree is still present, prove that both owners are on that same result
    -- path and retain the application as the single construction owner.
    -- This prevents one paper Gamma requirement from being split merely
    -- because @let x = rhs in x@ introduced an administrative scope.
    transparentDirectApplicationGroup left right =
        lgoTermNode (lgcOwner left) == lgoTermNode (lgcOwner right)
            && directApplicationOwner left
                /= directApplicationOwner right
            && localGammaOwnerOnResultPath (lgcOwner left) ann
            && localGammaOwnerOnResultPath (lgcOwner right) ann

    preferDirectApplicationOwner left right
        | directApplicationOwner right
        , not (directApplicationOwner left) = (right, left)
        | otherwise = (left, right)

    directApplicationOwner closure =
        lgoConstructor (lgcOwner closure) == LocalApplicationGamma
            && not (null (lgcDirectApplicationEdgeIds closure))

    appendEdgeId edgeIds edgeId
        | edgeId `elem` edgeIds = edgeIds
        | otherwise = edgeIds <> NonEmpty.singleton edgeId

    appendDirectApplicationEdgeId edgeIds edgeId
        | edgeId `elem` edgeIds = edgeIds
        | otherwise = edgeIds ++ [edgeId]

exactLambdaBodyOperatedType :: ElabType -> Maybe ElabType
exactLambdaBodyOperatedType ty = do
    (codomain, enclosingBinders) <- exactLambdaBodyWithBindings [] ty
    pure (compilerExactOperatedType enclosingBinders codomain)

exactLambdaBodyWithBindings
    :: [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> Maybe (ElabType, [(TypeBinderRef, Maybe BoundType)])
exactLambdaBodyWithBindings enclosingBinders ty =
    case ty of
        TForallRef ref mbBound body ->
            exactLambdaBodyWithBindings
                (appendBinder (ref, mbBound) enclosingBinders)
                body
        TArrow _ codomain -> Just (codomain, enclosingBinders)
        _ -> Nothing
  where
    appendBinder binder@(ref, _) binders
        | any (typeBinderRefsSameIdentity ref . fst) binders = binders
        | otherwise = binders ++ [binder]

compilerExactLambdaBodyExpected
    :: EdgeId
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> Maybe PacketExpectedType
compilerExactLambdaBodyExpected exactEdge enclosingBinders ty =
    do
        (codomain, enclosingBinders') <-
            exactLambdaBodyWithBindings enclosingBinders ty
        pure
            ( CompilerExactExpectedType
                exactEdge
                codomain
                enclosingBinders'
            )

packetLambdaBodyType :: PacketExpectedType -> Maybe PacketExpectedType
packetLambdaBodyType expected =
    case expected of
        CompilerExactExpectedType exactEdge ty enclosingBinders ->
            compilerExactLambdaBodyExpected exactEdge enclosingBinders ty
        SourceExpectedType ty enclosingBinders -> do
            (codomain, enclosingBinders') <-
                exactLambdaBodyWithBindings enclosingBinders ty
            pure (SourceExpectedType codomain enclosingBinders')
        ReturnedBindingSourceExpectedType ty enclosingBinders -> do
            (codomain, enclosingBinders') <-
                exactLambdaBodyWithBindings enclosingBinders ty
            pure
                ( ReturnedBindingSourceExpectedType
                    codomain
                    enclosingBinders'
                )

packetLambdaParameterType :: PacketExpectedType -> Maybe ElabType
packetLambdaParameterType expected =
    case snd (splitForallsRefs (packetExpectedType expected)) of
        TArrow domain _ -> Just domain
        _ -> Nothing

compilerExactLambdaParameterDeclarationIdentities
    :: Maybe PacketExpectedType
    -> Set.Set TypeBinderIdentity
compilerExactLambdaParameterDeclarationIdentities mbExpected =
    case mbExpected of
        Just expected@CompilerExactExpectedType{} ->
            Set.fromList
                ( map
                    typeBinderRefIdentity
                    ( maybe
                        []
                        typeBinderDeclarationRefs
                        (packetLambdaParameterType expected)
                    )
                )
        _ -> Set.empty

constructPacketOperatedScheme
    :: (NodeId -> NodeId)
    -> (NodeId -> NodeId)
    -> Either ElabError (ResultTypeView 'Presolved)
    -> IntMap.IntMap TypeBinderRef
    -> Maybe SchemeInfo
    -> Maybe PacketExpectedType
    -> SchemeInfo
    -> Either
        ElabError
        ( SchemeInfo
        , [(TypeBinderRef, TypeBinderRef)]
        , [(TypeBinderRef, TypeBinderRef)]
        )
constructPacketOperatedScheme representative constructionCanonical alignmentView sourceBinderRefs mbConstruction mbExpected inferred =
    case mbExpected of
        Just expected@(CompilerExactExpectedType exactEdge _ enclosingBinders) -> do
            -- The exact source boundary owns the operated type, while the
            -- inferred packet owns the graph-to-binder projection used by
            -- its construction.  Match the inferred leading binder spine to
            -- the exact boundary while both authorities are present, then
            -- construct the operated type directly in the graph identity
            -- domain.  Keeping the exact payload with free source identities
            -- and hoping a later root substitution repairs it would leave
            -- bounds open under the ambient construction Gamma.
            constructionRenames <-
                either
                    (Left . ValidationFailed . pure)
                    Right
                    ( sourceBinderConstructionRenamesRetainingAmbiguousSources
                        representative
                        relevantSourceBinderRefs
                        (siSubstRefs inferred)
                    )
            view <- alignmentView
            let (inferredBinders, inferredBodyRaw) =
                    splitForallsRefs (schemeToType (siScheme inferred))
                inferredBody =
                    inlineBoundVarsTypeWithCanonical
                        constructionCanonical
                        (View.rtvPresolutionViewOverlay view)
                        inferredBodyRaw
            (operatedType, binderRenames) <-
                alignCompilerExactOperatedType
                    representative
                    (siSubstRefs inferred)
                    exactEdge
                    enclosingBinders
                    inferredBinders
                    inferredBodyRaw
                    inferredBody
                    constructionRenames
                    (packetOperatedExpectedType expected)
            pure
                ( schemeInfoFromRefSubst
                    (schemeFromType operatedType)
                    (siSubstRefs inferred)
                , binderRenames
                , binderRenames
                )
          where
            exactFreeRefs =
                freeTypeVarRefsType (packetOperatedExpectedType expected)
            liveEnclosingRefs =
                [ exactRef
                | (exactRef, _) <- enclosingBinders
                , any (typeBinderRefsSameIdentity exactRef) exactFreeRefs
                ]
            relevantSourceBinderRefs =
                IntMap.filter
                    (\sourceRef ->
                        any
                            (typeBinderRefsSameIdentity sourceRef)
                            liveEnclosingRefs
                    )
                    sourceBinderRefs
        Just (SourceExpectedType sourceType _) ->
            alignSource sourceType
        Just (ReturnedBindingSourceExpectedType sourceType _) ->
            alignSource sourceType
        _ -> pure (inferred, [], [])
  where
    alignSource sourceType = do
        (aligned, constructionRoutes) <-
            alignSourceExpectedOperatedType
                representative
                sourceBinderRefs
                (fromMaybe inferred mbConstruction)
                inferred
                sourceType
        -- This quotient closes a packet-local source forall in the
        -- construction domain.  It is not a compiler-exact lexical
        -- publication route: recording it as one would instantiate
        -- the newly retained packet binder and make it free again.
        pure (aligned, constructionRoutes, [])

-- | Project a source-owned expected endpoint into the packet construction
-- domain while both views are still present.  A later source forall can be
-- free in the operated endpoint even though the inferred packet has already
-- materialized its graph representative as a local binder.  The shared graph
-- representative is the construction proof for that quotient; leaving the
-- source occurrence untouched would publish an open Gamma bound and force a
-- later repair.
alignSourceExpectedOperatedType
    :: (NodeId -> NodeId)
    -> IntMap.IntMap TypeBinderRef
    -> SchemeInfo
    -> SchemeInfo
    -> ElabType
    -> Either ElabError (SchemeInfo, [(TypeBinderRef, TypeBinderRef)])
alignSourceExpectedOperatedType representative sourceBinderRefs construction inferred sourceType = do
    declarationRoutes <-
        sourceExpectedDeclarationBinderRoutes
            sourceBinderRefs
            inferred
            sourceType
            (schemeToType (siScheme inferred))
    routes <-
        foldM
            insertSourceRoute
            declarationRoutes
            (distinctTypeBinderRefs (freeTypeVarRefsType sourceType))
    let alignedType = renameCompilerExactBinderRefs routes sourceType
        routeList =
            [ (sourceRef, constructionRef)
            | sourceRef <-
                distinctTypeBinderRefs
                    ( typeBinderDeclarationRefs sourceType
                        ++ freeTypeVarRefsType sourceType
                    )
            , Just constructionRef <-
                [Map.lookup (typeBinderRefIdentity sourceRef) routes]
            , not (typeBinderRefsSameIdentity sourceRef constructionRef)
            ]
    pure
        ( schemeInfoFromRefSubst
            (schemeFromType alignedType)
            (siSubstRefs inferred)
        , routeList
        )
  where
    constructionBinders = map fst (schemeBinderRefs (siScheme construction))

    insertSourceRoute routes sourceRef =
        case find (typeBinderRefsSameIdentity sourceRef) constructionBinders of
            Just constructionRef ->
                insertCompilerExactBinderIdentityRef
                    routes
                    (typeBinderRefIdentity sourceRef, constructionRef)
            Nothing ->
                case representativeCandidates sourceRef of
                    [] -> pure routes
                    [constructionRef] ->
                        insertCompilerExactBinderIdentityRef
                            routes
                            (typeBinderRefIdentity sourceRef, constructionRef)
                    candidates ->
                        Left
                            ( ValidationFailed
                                [ "source expected binder has multiple packet construction representatives"
                                , "  source binder: " ++ show sourceRef
                                , "  candidates: " ++ show candidates
                                ]
                            )

    representativeCandidates sourceRef =
        case typeBinderRefNode sourceRef of
            Nothing -> []
            Just sourceNode ->
                distinctTypeBinderRefs
                    [ constructionRef
                    | constructionRef <- constructionBinders
                    , Just constructionNode <- [typeBinderRefNode constructionRef]
                    , representative constructionNode == representative sourceNode
                    ]

-- | Pair declaration identities in two already-aligned type constructions.
-- Callers establish alpha-equivalence (or a stronger occurrence-specific
-- construction proof) before consuming these pairs.
pairedTypeDeclarationRefs
    :: ElabType
    -> ElabType
    -> [(TypeBinderRef, TypeBinderRef)]
pairedTypeDeclarationRefs source target =
    case (source, target) of
        (TArrow sourceDomain sourceCodomain, TArrow targetDomain targetCodomain) ->
            pairedTypeDeclarationRefs sourceDomain targetDomain
                ++ pairedTypeDeclarationRefs sourceCodomain targetCodomain
        (TConWithIdentity _ _ sourceArgs, TConWithIdentity _ _ targetArgs) ->
            concat
                ( zipWith
                    pairedTypeDeclarationRefs
                    (NonEmpty.toList sourceArgs)
                    (NonEmpty.toList targetArgs)
                )
        (TVarAppRef _ sourceArgs, TVarAppRef _ targetArgs) ->
            concat
                ( zipWith
                    pairedTypeDeclarationRefs
                    (NonEmpty.toList sourceArgs)
                    (NonEmpty.toList targetArgs)
                )
        (TForallRef sourceRef sourceBound sourceBody, TForallRef targetRef targetBound targetBody) ->
            (sourceRef, targetRef)
                : pairedBounds sourceBound targetBound
                    ++ pairedTypeDeclarationRefs sourceBody targetBody
        (TMuRef sourceRef sourceBody, TMuRef targetRef targetBody) ->
            (sourceRef, targetRef)
                : pairedTypeDeclarationRefs sourceBody targetBody
        _ -> []
  where
    pairedBounds (Just sourceBound) (Just targetBound) =
        pairedTypeDeclarationRefs
            (tyToElab sourceBound)
            (tyToElab targetBound)
    pairedBounds _ _ = []

-- | Pair source-variable occurrences with their graph occurrences in two
-- structurally aligned type constructions.
pairedTypeOccurrenceRefs
    :: ElabType
    -> ElabType
    -> [(TypeBinderRef, TypeBinderRef)]
pairedTypeOccurrenceRefs source target =
    go source target
  where
    go sourceTy targetTy =
        case (sourceTy, targetTy) of
            (TVarRef sourceRef, TVarRef targetRef) ->
                [(sourceRef, targetRef)]
            (TArrow sourceDomain sourceCodomain, TArrow targetDomain targetCodomain) ->
                go sourceDomain targetDomain
                    ++ go sourceCodomain targetCodomain
            (TConWithIdentity _ _ sourceArgs, TConWithIdentity _ _ targetArgs) ->
                concat
                    ( zipWith
                        go
                        (NonEmpty.toList sourceArgs)
                        (NonEmpty.toList targetArgs)
                    )
            (TVarAppRef sourceRef sourceArgs, TVarAppRef targetRef targetArgs) ->
                (sourceRef, targetRef)
                    : concat
                        ( zipWith
                            go
                            (NonEmpty.toList sourceArgs)
                            (NonEmpty.toList targetArgs)
                        )
            (TForallRef _ sourceBound sourceBody, TForallRef _ targetBound targetBody) ->
                pairedBounds sourceBound targetBound
                    ++ go sourceBody targetBody
            (TMuRef _ sourceBody, TMuRef _ targetBody) ->
                go sourceBody targetBody
            _ -> []

    pairedBounds (Just sourceBound) (Just targetBound) =
        go (tyToElab sourceBound) (tyToElab targetBound)
    pairedBounds _ _ = []

-- | Pair only lexically free source occurrences.  A source annotation owns
-- its declarations locally, while its free variables remain available to the
-- enclosing Gamma construction.
pairedFreeTypeOccurrenceRefs
    :: ElabType
    -> ElabType
    -> [(TypeBinderRef, TypeBinderRef)]
pairedFreeTypeOccurrenceRefs source target =
    [ pair
    | pair@(sourceRef, _) <- pairedTypeOccurrenceRefs source target
    , any (typeBinderRefsSameIdentity sourceRef) sourceFreeRefs
    ]
  where
    sourceFreeRefs = freeTypeVarRefsType source

alignedTypeOccurrencePairs
    :: ElabType
    -> ElabType
    -> Maybe [(TypeBinderRef, TypeBinderRef)]
alignedTypeOccurrencePairs source target =
    if annotationProducerTypesAgree source alignedTarget
        then Just pairs
        else Nothing
  where
    pairs = pairedTypeOccurrenceRefs source target
    alignedTarget = alignTargetOccurrences pairs target

    -- The annotation edge may construct any expected component from bottom.
    -- Every non-bottom component must already agree after the exact
    -- occurrence projection; this is the structural part of the edge-owned
    -- coercion used to publish the route.
    annotationProducerTypesAgree expected producer
        | alphaEqType expected producer
            || churchAwareEqType expected producer = True
    annotationProducerTypesAgree _ TBottom = True
    annotationProducerTypesAgree
        (TArrow expectedDomain expectedCodomain)
        (TArrow producerDomain producerCodomain) =
            annotationProducerTypesAgree expectedDomain producerDomain
                && annotationProducerTypesAgree expectedCodomain producerCodomain
    annotationProducerTypesAgree
        (TConWithIdentity expectedIdentity expectedConstructor expectedArgs)
        (TConWithIdentity producerIdentity producerConstructor producerArgs) =
            expectedIdentity == producerIdentity
                && expectedConstructor == producerConstructor
                && NonEmpty.length expectedArgs == NonEmpty.length producerArgs
                && and
                    ( zipWith
                        annotationProducerTypesAgree
                        (NonEmpty.toList expectedArgs)
                        (NonEmpty.toList producerArgs)
                    )
    annotationProducerTypesAgree
        (TVarAppRef expectedRef expectedArgs)
        (TVarAppRef producerRef producerArgs) =
            typeBinderRefsSameIdentity expectedRef producerRef
                && NonEmpty.length expectedArgs == NonEmpty.length producerArgs
                && and
                    ( zipWith
                        annotationProducerTypesAgree
                        (NonEmpty.toList expectedArgs)
                        (NonEmpty.toList producerArgs)
                    )
    annotationProducerTypesAgree _ _ = False

-- | Establish an exact source/graph alignment for free occurrences.  Free
-- identities are intentionally not alpha-equivalent, so first project the
-- structurally paired graph occurrences to their source identities and only
-- then ask alpha-equivalence to validate the complete construction.
alignedFreeTypeOccurrencePairs
    :: ElabType
    -> ElabType
    -> Maybe [(TypeBinderRef, TypeBinderRef)]
alignedFreeTypeOccurrencePairs source target =
    alignedOccurrencePairsWith
        pairedFreeTypeOccurrenceRefs
        source
        target

alignedOccurrencePairsWith
    :: (ElabType -> ElabType -> [(TypeBinderRef, TypeBinderRef)])
    -> ElabType
    -> ElabType
    -> Maybe [(TypeBinderRef, TypeBinderRef)]
alignedOccurrencePairsWith pairOccurrences source target =
    if alphaEqType source alignedTarget
        then Just pairs
        else Nothing
  where
    pairs = pairOccurrences source target
    alignedTarget = alignTargetOccurrences pairs target

alignTargetOccurrences
    :: [(TypeBinderRef, TypeBinderRef)]
    -> ElabType
    -> ElabType
alignTargetOccurrences pairs target =
    foldl'
        ( \ty (sourceRef, targetRef) ->
            substTypeCaptureRef
                targetRef
                (TVarRef sourceRef)
                ty
        )
        target
        pairs

-- | Recover the alpha-renaming already chosen by the inferred construction
-- for source-local forall and mu declarations.  These binders are not free in
-- the source expected type, so the ordinary source-to-Gamma route cannot see
-- them.  Alpha-equivalence validates the declaration correspondence, but does
-- not create identity authority: every non-trivial route must also join the
-- exact source sidecar and inferred substitution at one graph occurrence.
sourceExpectedDeclarationBinderRoutes
    :: IntMap.IntMap TypeBinderRef
    -> SchemeInfo
    -> ElabType
    -> ElabType
    -> Either ElabError (Map.Map TypeBinderIdentity TypeBinderRef)
sourceExpectedDeclarationBinderRoutes sourceBinderRefs inferred sourceType inferredType
    | alphaEqType sourceType inferredType =
        foldM
            insertDeclarationRoute
            Map.empty
            (pairedTypeDeclarationRefs sourceType inferredType)
    | otherwise = pure Map.empty
  where
    insertDeclarationRoute routes (sourceRef, inferredRef)
        | typeBinderRefsSameIdentity sourceRef inferredRef = pure routes
        | otherwise =
            case exactInferredRefs sourceRef of
                [] -> pure routes
                [authorizedRef]
                    | typeBinderRefsSameIdentity authorizedRef inferredRef ->
                        insertCompilerExactBinderIdentityRef
                            routes
                            (typeBinderRefIdentity sourceRef, inferredRef)
                    | otherwise ->
                        Left
                            ( ValidationFailed
                                [ "source-local declaration sidecar disagrees with its alpha-corresponding inferred binder"
                                , "  source binder: " ++ show sourceRef
                                , "  alpha-corresponding binder: " ++ show inferredRef
                                , "  sidecar route: " ++ show authorizedRef
                                ]
                            )
                authorizedRefs ->
                    Left
                        ( ValidationFailed
                            [ "source-local declaration has multiple inferred occurrence routes"
                            , "  source binder: " ++ show sourceRef
                            , "  inferred routes: " ++ show authorizedRefs
                            ]
                        )

    exactInferredRefs sourceRef =
        distinctTypeBinderRefs
            [ inferredRef
            | (graphKey, candidateSourceRef) <- IntMap.toList sourceBinderRefs
            , typeBinderRefsSameIdentity candidateSourceRef sourceRef
            , Just inferredRef <- [IntMap.lookup graphKey (siSubstRefs inferred)]
            ]

-- | Build the quotient between a compiler-exact source binder and the
-- inferred graph binder that represents it at this packet boundary.  The
-- exact annotation supplies the source binder spine.  Prefer the source-
-- binder sidecar and inferred scheme substitution, which share the graph node
-- that proves its construction identity.  An explicit-empty exact trace has
-- no such route, so the inferred and exact operated bodies may instead supply
-- one unique identity-bearing structural correspondence.  Ambiguous routes
-- are rejected here.  When no inferred binder is a candidate, the exact
-- enclosing binder remains the construction authority and needs no quotient;
-- inventing a graph route would conflate two identity domains.
alignCompilerExactOperatedType
    :: (NodeId -> NodeId)
    -> IntMap.IntMap TypeBinderRef
    -> EdgeId
    -> [(TypeBinderRef, Maybe BoundType)]
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ElabType
    -> [(TypeBinderRef, TypeBinderRef)]
    -> ElabType
    -> Either ElabError (ElabType, [(TypeBinderRef, TypeBinderRef)])
alignCompilerExactOperatedType representative inferredSubst exactEdge enclosingBinders inferredBinders inferredBodyRaw inferredBodyNormalized constructionRenames exactOperatedType =
    case liveExactRefs of
        [] -> pure (exactOperatedType, [])
        _ -> do
            quotient <-
                foldM
                    insertExactRoute
                    Map.empty
                    liveExactRefs
            let operatedType =
                    renameCompilerExactBinderRefs quotient exactOperatedType
            pure
                ( operatedType
                , [ (exactRef, inferredRef)
                  | exactRef <- liveExactRefs
                  , Just inferredRef <-
                        [ Map.lookup
                            (typeBinderRefIdentity exactRef)
                            quotient
                        ]
                  ]
                )
  where
    exactFreeRefs = freeTypeVarRefsType exactOperatedType
    liveExactRefs =
        [ exactRef
        | (exactRef, _) <- enclosingBinders
        , any (typeBinderRefsSameIdentity exactRef) exactFreeRefs
        ]

    insertExactRoute routes exactRef =
        case distinctTypeBinderRefs (explicitInferredRefs exactRef) of
            [inferredRef] ->
                insertCompilerExactBinderIdentityRef
                    routes
                    (typeBinderRefIdentity exactRef, inferredRef)
            [] -> do
                let successfulMatches =
                        [ matches
                        | Right matches <- structuralMatches
                        ]
                    candidates =
                        distinctTypeBinderRefs
                            ( concatMap
                                (`structuralInferredRefs` exactRef)
                                successfulMatches
                                ++ concatMap
                                    (`churchStructuralInferredRefs` exactRef)
                                    successfulChurchMatches
                            )
                case candidates of
                    [inferredRef] ->
                        insertCompilerExactBinderIdentityRef
                            routes
                            (typeBinderRefIdentity exactRef, inferredRef)
                    [] ->
                        -- The compiler-exact spine already binds this source
                        -- identity.  A structural match that maps other
                        -- binders is not evidence that this one needs a graph
                        -- representative, so retain the exact identity.
                        pure routes
                    _ -> noUniqueRoute exactRef candidates
            candidates -> noUniqueRoute exactRef candidates

    inferredBinderRefs = map fst inferredBinders

    noUniqueRoute exactRef candidates =
        Left
            ( ValidationFailed
                [ "compiler-exact enclosing binder has no unique inferred construction route"
                , "  edge: " ++ show exactEdge
                , "  exact binder: " ++ show exactRef
                , "  candidate count: " ++ show (length candidates)
                ]
            )

    explicitInferredRefs exactRef =
        [ inferredRef
        | (sourceRef, inferredRef) <- constructionRenames
        , typeBinderRefsSameIdentity sourceRef exactRef
        ]

    structuralMatches =
        [ matchTypeRefs inferredBinderRefs inferredBodyRaw exactOperatedType
        , matchTypeRefs inferredBinderRefs inferredBodyNormalized exactOperatedType
        ]

    churchStructuralMatches =
        [ matchChurchAwareTypeRefs
            liveExactRefs
            exactOperatedType
            inferredBodyNormalized
        ]

    successfulChurchMatches =
        [ matches
        | Right matches <- churchStructuralMatches
        ]

    structuralInferredRefs inferredMatches exactRef =
        [ inferredRef
        | (inferredRef, TVarRef matchedExactRef) <- Map.toList inferredMatches
        , typeBinderRefsSameIdentity matchedExactRef exactRef
        ]

    churchStructuralInferredRefs exactMatches exactRef =
        case Map.lookup exactRef exactMatches of
            Just (TVarRef inferredRef) -> projectToInferredBinder inferredRef
            _ -> []

    projectToInferredBinder inferredRef =
        distinctTypeBinderRefs
            (directBinder ++ representativeBinders)
      where
        constructionBinders =
            distinctTypeBinderRefs
                (inferredBinderRefs ++ IntMap.elems inferredSubst)

        directBinder =
            [ candidate
            | candidate <- constructionBinders
            , typeBinderRefsSameIdentity candidate inferredRef
            ]

        representativeBinders =
            case typeBinderRefNode inferredRef of
                Nothing -> []
                Just inferredNode ->
                    [ candidate
                    | candidate <- constructionBinders
                    , Just candidateNode <- [typeBinderRefNode candidate]
                    , representative candidateNode == representative inferredNode
                    ]

mergeCompilerExactConstructionBinderRefs
    :: IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
mergeCompilerExactConstructionBinderRefs existing projected =
    foldM insertProjected existing (IntMap.toList projected)
  where
    insertProjected refs (graphKey, sourceRef) =
        case IntMap.lookup graphKey refs of
            Nothing -> pure (IntMap.insert graphKey sourceRef refs)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef sourceRef ->
                    pure refs
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "compiler exact construction binder conflicts with existing source identity"
                            , "  graph node: " ++ show (NodeId graphKey)
                            , "  existing source binder: " ++ show existingRef
                            , "  exact source binder: " ++ show sourceRef
                                ]
                        )

-- | Enter a nested source-binder domain.  Solving may reuse one graph node
-- for binders from two nested exact annotations, but only the innermost source
-- identity is active while its annotated producer is prepared.  The
-- edge-local maps have already rejected ambiguity within each domain, so
-- left-biased union is lexical shadowing rather than conflict recovery.
enterCompilerExactConstructionBinderRefs
    :: IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
enterCompilerExactConstructionBinderRefs inherited local =
    IntMap.union local inherited

insertCompilerExactBinderIdentityRef
    :: Map.Map TypeBinderIdentity TypeBinderRef
    -> (TypeBinderIdentity, TypeBinderRef)
    -> Either ElabError (Map.Map TypeBinderIdentity TypeBinderRef)
insertCompilerExactBinderIdentityRef refs (exactIdentity, sourceRef) =
    case Map.lookup exactIdentity refs of
        Just existingSourceRef
            | typeBinderRefsSameIdentity existingSourceRef sourceRef ->
                pure refs
            | otherwise ->
                Left
                    ( ValidationFailed
                        [ "compiler exact binder maps to multiple source identities"
                        , "  exact binder: " ++ show exactIdentity
                        , "  first source binder: " ++ show existingSourceRef
                        , "  second source binder: " ++ show sourceRef
                        ]
                    )
        Nothing ->
            case
                find
                    (\(otherIdentity, otherSourceRef) ->
                        otherIdentity /= exactIdentity
                            && typeBinderRefsSameIdentity otherSourceRef sourceRef
                    )
                    (Map.toList refs)
            of
                Just (otherIdentity, _) ->
                    Left
                        ( ValidationFailed
                            [ "compiler exact source binder maps from multiple exact binders"
                            , "  source binder: " ++ show sourceRef
                            , "  first exact binder: " ++ show otherIdentity
                            , "  second exact binder: " ++ show exactIdentity
                            ]
                        )
                Nothing -> pure (Map.insert exactIdentity sourceRef refs)

resultTypeViewWithBoundOverlays
    :: GaBindParents 'Presolved
    -> Either ElabError (ResultTypeView 'Presolved)
    -> IntMap.IntMap NodeId
    -> Either ElabError (ResultTypeView 'Presolved)
resultTypeViewWithBoundOverlays bindParentsGa resultTypeView targetBoundOverlays = do
    view0 <- resultTypeView
    pure
        ( IntMap.foldlWithKey'
            (\current sourceKey solvedBound ->
                let withBaseOverlay =
                        View.rtvWithBoundOverlay
                            (NodeId sourceKey)
                            solvedBound
                            current
                in case IntMap.lookup sourceKey (gaBaseToSolved bindParentsGa) of
                    Nothing -> withBaseOverlay
                    Just liveNode ->
                        View.rtvWithBoundOverlay
                            liveNode
                            solvedBound
                            withBaseOverlay
            )
            view0
            targetBoundOverlays
        )

-- | Build compiler-exact contracts solely from the edge's frozen
-- binder-to-argument trace.  The exact type remains untouched until this edge
-- plan exists; no whole-tree alias pass or structural type matching is allowed
-- to invent a route.  A trace with no binder arguments still produces a plan,
-- while an absent trace is rejected.
prepareCompilerExactEdgePlans
    :: IntMap.IntMap ElabType
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap CompilerExactEdgePlan)
prepareCompilerExactEdgePlans rawExactTypes edgeTraces sourceBinderRefs =
    IntMap.traverseWithKey prepareEdge rawExactTypes
  where
    prepareEdge edgeKey rawExactType = do
        traceInfo <-
            case IntMap.lookup edgeKey edgeTraces of
                Just trace -> pure trace
                Nothing -> Left (MissingEdgeTrace (EdgeId edgeKey))
        (constructionRefs, exactBinderRefs) <-
            foldM
                (prepareBinderRoute traceInfo rawExactType (EdgeId edgeKey))
                (IntMap.empty, Map.empty)
                (etBinderArgs traceInfo)
        expectedType <-
            applyCompilerExactBinderQuotient exactBinderRefs rawExactType
        pure
            CompilerExactEdgePlan
                { ceepExpectedType = expectedType
                , ceepConstructionRefs = constructionRefs
                , ceepDeclarationRefs =
                    exactDeclarationRefs constructionRefs expectedType
                }

    -- Recursive declarations are not edge arguments, so retain their exact
    -- identity provenance in a separate lane. The boundary later selects only
    -- the graph owner occurring at the corresponding recursive position. Use
    -- the post-quotient type because imported structural owners can become
    -- exact only through this edge's frozen binder route.
    exactDeclarationRefs constructionRefs expectedType =
        IntMap.unions
            [ IntMap.map
                (const exactRef)
                ( IntMap.union
                    ( Map.findWithDefault
                        IntMap.empty
                        (typeBinderRefIdentity exactRef)
                        sourceBinderRefsByIdentity
                    )
                    ( IntMap.filter
                        (typeBinderRefsSameIdentity exactRef)
                        constructionRefs
                    )
                )
            | exactRef <- declarationRefs
            ]
      where
        declarationRefs =
            distinctTypeBinderRefs
                (typeBinderDeclarationRefs expectedType)

    -- This index is shared by every exact edge. Building it once avoids an
    -- edge-by-edge scan of the whole source-provenance table in large modules.
    sourceBinderRefsByIdentity =
        IntMap.foldlWithKey'
            (\refsByIdentity graphKey sourceRef ->
                Map.insertWith
                    IntMap.union
                    (typeBinderRefIdentity sourceRef)
                    (IntMap.singleton graphKey sourceRef)
                    refsByIdentity
            )
            Map.empty
            sourceBinderRefs

    prepareBinderRoute traceInfo rawExactType edgeId (constructionRefs, exactBinderRefs) (producerBinder, argumentNode) =
        case selectArgumentRef rawExactType argumentNode of
            Nothing -> pure (constructionRefs, exactBinderRefs)
            Just argumentRef -> do
                let outwardRef =
                        IntMap.findWithDefault
                            argumentRef
                            (getNodeId producerBinder)
                            sourceBinderRefs
                    constructionNodes =
                        producerBinder
                            : maybe
                                []
                                pure
                                (IntMap.lookup (getNodeId producerBinder) (etBinderReplayMap traceInfo))
                constructionRefs' <-
                    foldM
                        (insertConstructionRoute edgeId outwardRef)
                        constructionRefs
                        constructionNodes
                exactBinderRefs' <-
                    foldM
                        insertCompilerExactBinderIdentityRef
                        exactBinderRefs
                        [ (typeBinderRefIdentity exactRef, outwardRef)
                        | exactRef <- exactRefsForRoute rawExactType producerBinder argumentNode argumentRef
                        ]
                pure (constructionRefs', exactBinderRefs')

    selectArgumentRef rawExactType argumentNode =
        case IntMap.lookup (getNodeId argumentNode) sourceBinderRefs of
            Just ref -> Just ref
            Nothing -> listToMaybe (typeRefsAtNode argumentNode rawExactType)

    exactRefsForRoute rawExactType producerBinder argumentNode argumentRef =
        distinctTypeBinderRefs
            [ ref
            | ref <- allTypeRefs rawExactType
            , typeBinderRefsSameIdentity ref argumentRef
                || typeBinderRefNode ref == Just producerBinder
                || typeBinderRefNode ref == Just argumentNode
            ]

    insertConstructionRoute edgeId outwardRef refs graphNode =
        case IntMap.lookup (getNodeId graphNode) refs of
            Nothing -> pure (IntMap.insert (getNodeId graphNode) outwardRef refs)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef outwardRef -> pure refs
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "compiler exact edge maps one construction binder to multiple identities"
                            , "  edge: " ++ show edgeId
                            , "  graph binder: " ++ show graphNode
                            , "  first identity: " ++ show existingRef
                            , "  second identity: " ++ show outwardRef
                            ]
                        )

distinctTypeBinderRefs :: [TypeBinderRef] -> [TypeBinderRef]
distinctTypeBinderRefs = foldr insertDistinct []
  where
    insertDistinct ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

typeRefsAtNode :: NodeId -> ElabType -> [TypeBinderRef]
typeRefsAtNode target = filter hasTargetNode . allTypeRefs
  where
    hasTargetNode ref = typeBinderRefNode ref == Just target

allTypeRefs :: ElabType -> [TypeBinderRef]
allTypeRefs ty =
    case ty of
        TVarRef ref -> [ref]
        TArrow domain codomain -> allTypeRefs domain ++ allTypeRefs codomain
        TConWithIdentity _ _ args -> concatMap allTypeRefs args
        TVarAppRef ref args -> ref : concatMap allTypeRefs args
        TBaseWithIdentity{} -> []
        TForallRef ref mbBound body ->
            ref : maybe [] allBoundTypeRefs mbBound ++ allTypeRefs body
        TMuRef ref body -> ref : allTypeRefs body
        TBottom -> []

allBoundTypeRefs :: BoundType -> [TypeBinderRef]
allBoundTypeRefs ty =
    case ty of
        TArrow domain codomain -> allTypeRefs domain ++ allTypeRefs codomain
        TConWithIdentity _ _ args -> concatMap allTypeRefs args
        TVarAppRef ref args -> ref : concatMap allTypeRefs args
        TBaseWithIdentity{} -> []
        TForallRef ref mbBound body ->
            ref : maybe [] allBoundTypeRefs mbBound ++ allTypeRefs body
        TMuRef ref body -> ref : allTypeRefs body
        TBottom -> []

-- | Select the result abstraction that turns the compiler-exact operated
-- type into this packet's completed construction.  The proof is contained in
-- the prepared scheme itself: the result must be a packet-owned binder whose
-- bound is the exact operated type and whose identity occurs in the scheme
-- body.  A vacuous quantifier therefore cannot become a pending term action.
compilerExactPacketResult
    :: Maybe PacketExpectedType
    -> SchemeInfo
    -> SchemeInfo
    -> Either ElabError (Maybe CompilerExactPacketResult)
compilerExactPacketResult mbExpected packet operatedPacket =
    case mbExpected of
        Just (CompilerExactExpectedType exactEdge expectedType enclosingBinders) ->
            case resultSpineCandidates constructionType operatedType of
                [] -> pure Nothing
                [packetResultRef] -> do
                    mbSourceCompletionRef <-
                        sourceCompletionRef
                            exactEdge
                            packetResultRef
                            constructionType
                            expectedType
                            enclosingBinders
                    pure
                        ( case mbSourceCompletionRef of
                            Just completionRef ->
                                Just
                                    ( SourceOwnedCompilerExactPacketResult
                                        exactEdge
                                        packetResultRef
                                        completionRef
                                    )
                            Nothing ->
                                Just
                                    ( PacketOwnedCompilerExactPacketResult
                                        exactEdge
                                        packetResultRef
                                    )
                        )
                resultRefs ->
                    Left
                        ( ValidationFailed
                            [ "compiler exact packet has multiple result abstractions"
                            , "  exact edge: " ++ show exactEdge
                            , "  operated type: " ++ show operatedType
                            , "  candidates: " ++ show resultRefs
                            , "  packet: " ++ show packet
                            ]
                        )
        _ -> pure Nothing
  where
    constructionType = schemeBody (siScheme packet)
    operatedType = schemeToType (siScheme operatedPacket)

    resultSpineCandidates construction operated
        | alphaEqType construction operated
            || churchAwareEqType construction operated = []
    resultSpineCandidates (TVarRef ref) operated =
        case findBinderBound ref of
            Just bound
                | alphaEqType (tyToElab bound) operated
                    || churchAwareEqType (tyToElab bound) operated -> [ref]
            _ -> []
    resultSpineCandidates (TArrow constructionDomain constructionCodomain) (TArrow operatedDomain operatedCodomain)
        | alphaEqType constructionDomain operatedDomain
            || churchAwareEqType constructionDomain operatedDomain =
            resultSpineCandidates constructionCodomain operatedCodomain
    resultSpineCandidates _ _ = []

    findBinderBound ref =
        snd
            =<< find
                (typeBinderRefsSameIdentity ref . fst)
                (schemeBinderRefs (siScheme packet))

    sourceCompletionRef exactEdge packetResultRef construction expectedType enclosingBinders =
        case expectedResultRef construction expectedType of
            Just expectedRef ->
                case
                    foldr
                        insertDistinctRef
                        []
                        [ enclosingRef
                        | (enclosingRef, _) <- enclosingBinders
                        , typeBinderRefsSameIdentity expectedRef enclosingRef
                        ]
                of
                    [] -> pure Nothing
                    [sourceRef] -> pure (Just sourceRef)
                    sourceRefs ->
                        Left
                            ( ValidationFailed
                                [ "compiler exact result position names multiple source binders"
                                , "  exact edge: " ++ show exactEdge
                                , "  expected result: " ++ show expectedRef
                                , "  source binders: " ++ show sourceRefs
                                ]
                            )
            Nothing -> pure Nothing

      where
        expectedResultRef constructionTy expectedTy =
            case (constructionTy, expectedTy) of
                (TVarRef constructionRef, TVarRef expectedRef)
                    | typeBinderRefsSameIdentity constructionRef packetResultRef ->
                        Just expectedRef
                (TArrow constructionDomain constructionCodomain, TArrow expectedDomain expectedCodomain)
                    | alphaEqType constructionDomain expectedDomain
                        || churchAwareEqType constructionDomain expectedDomain ->
                        expectedResultRef constructionCodomain expectedCodomain
                _ -> Nothing

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

prepareGeneralizationArtifact
    :: TraceConfig
    -> IdentityGenerator
    -> IntMap.IntMap ResolvedSrcType
    -> IntMap.IntMap ElabType
    -> IntMap.IntMap TypeBinderIdentity
    -> Constraint 'Acyclic
    -> PresolutionResult
    -> AnnExpr
    -> Either SolveError PreparedGeneralizationArtifact
prepareGeneralizationArtifact traceCfg identityGenerator exactProducerSourceTypes annExpectedTypes sourceBinderIdentities acyclicBase pres ann =
    prepareGeneralizationArtifactForRoots
        traceCfg
        identityGenerator
        exactProducerSourceTypes
        annExpectedTypes
        sourceBinderIdentities
        acyclicBase
        pres
        [ann]

edgeArtifactsSolveError :: EdgeArtifactsError -> SolveError
edgeArtifactsSolveError err =
    Solve.ValidationFailed
        ["invalid presolution edge artifact packet: " ++ show err]

prepareGeneralizationArtifactForRoots
    :: TraceConfig
    -> IdentityGenerator
    -> IntMap.IntMap ResolvedSrcType
    -> IntMap.IntMap ElabType
    -> IntMap.IntMap TypeBinderIdentity
    -> Constraint 'Acyclic
    -> PresolutionResult
    -> [AnnExpr]
    -> Either SolveError PreparedGeneralizationArtifact
prepareGeneralizationArtifactForRoots traceCfg identityGenerator exactProducerSourceTypes annExpectedTypes sourceBinderIdentities acyclicBase pres anns0 = do
    let anns =
            case anns0 of
                [] -> [snapshotAnnFallback]
                _ -> anns0
        snapshotAnnFallback =
            error "prepareGeneralizationArtifactForRoots: empty annotation roots"
        annForGeneralization =
            case anns of
                firstAnn : _ -> firstAnn
                [] -> snapshotAnnFallback
    let preRewrite = snapshotConstraint pres
    (solvedClean, presolutionViewClean) <-
        Finalize.finalizeSnapshotArtifacts preRewrite (snapshotUnionFind pres)
    let canonNode =
            makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        rawEdgeArtifacts = prEdgeArtifacts pres
    traceCopyArtifacts <-
        prepareTraceCopyArtifacts
            acyclicBase
            presolutionViewClean
            (prRedirects pres)
            canonNode
            rawEdgeArtifacts
    let acyclicBaseForGeneralization = toPresolvedConstraint acyclicBase
        planBuilder = prPlanBuilder pres
        TraceCopyArtifacts
            { tcaInstCopyNodes = instCopyNodes
            , tcaInstCopyMapFull = instCopyMapFull
            , tcaExpansionConstructionPlacements = expansionConstructionPlacements
            } = traceCopyArtifacts
        (constraintForGen, bindParentsGa) =
            constraintForGeneralization
                traceCfg
                presolutionViewClean
                (prRedirects pres)
                instCopyNodes
                instCopyMapFull
                expansionConstructionPlacements
                acyclicBaseForGeneralization
                annForGeneralization
    presolutionViewForGen <-
        Finalize.finalizePresolutionViewFromSnapshot
            constraintForGen
            (Solved.canonicalMap solvedClean)
    edgeArtifacts <-
        first edgeArtifactsSolveError
            ( mapEdgeArtifacts
                (canonicalizeExpansion canonNode)
                (canonicalizeWitness canonNode)
                (canonicalizeTrace canonNode)
                rawEdgeArtifacts
            )
    let annNodeCanonical = canonicalizeNode canonNode
        constructionIdentityRepresentative node =
            case resolveGaSolvedToBase bindParentsGa (annNodeCanonical node) of
                SolvedToBaseMapped baseNode -> baseNode
                SolvedToBaseSameDomain baseNode -> baseNode
                SolvedToBaseMissing -> annNodeCanonical node
        prepareAnn ann =
            alignAnnInstantiationSites
                edgeArtifacts
                ( redirectAndCanonicalizeAnn
                    annNodeCanonical
                    (prRedirects pres)
                    ann
                )
    annCanons <-
        first
            ( \err ->
                Solve.ValidationFailed
                    [ "application instantiation-site preparation failed"
                    , "  cause: " ++ show err
                    ]
            )
            (traverse prepareAnn anns)
    let
        annCanon =
            case annCanons of
                firstAnn : _ -> firstAnn
                [] -> snapshotAnnFallback
        generalizeAtWithView mbGa =
            generalizeAtWithBuilderRequired
                planBuilder
                preparedSourceRequirements
                mbGa
                presolutionViewForGen
        resultTypeInputs =
            mkResultTypeInputs
                (pvCanonical presolutionViewForGen)
                edgeArtifacts
                presolutionViewForGen
                bindParentsGa
                planBuilder
                acyclicBaseForGeneralization
                (prRedirects pres)
                traceCfg
        readModel = buildElabReadModel presolutionViewForGen
        baseReadModel =
            buildElabReadModel
                (Finalize.presolutionViewFromSnapshot acyclicBaseForGeneralization IntMap.empty)
        resultTypeInputsWithReadModels =
            resultTypeInputs
                { rtcReadModel = Just readModel
                , rtcBaseReadModel = Just baseReadModel
                }
        resultTypeView = buildResultTypeView resultTypeInputsWithReadModels
        sourceBinderRefsDirect =
            IntMap.map
                (\identity ->
                    typeBinderRefFromIdentity
                        identity
                        (typeBinderIdentityStableName identity)
                )
                sourceBinderIdentities
        sourceBinderRefs =
            expandPreparedSourceBinderRefs
                bindParentsGa
                annNodeCanonical
                sourceBinderRefsDirect
        preparedSourceRequirements =
            GeneralizationRequirements
                { grRequiredGammaBinders = []
                , grSourceBinderRefs = sourceBinderRefs
                , grAmbientBinderRefs = []
                , grTermUsedRootBinderRefs = []
                , grAmbientGammaAuthorities = IntMap.empty
                , grLocallyClosedGammaNodes = IntSet.empty
                }
        rawExactProducerTypes =
            traverse resolvedExactExpectedType exactProducerSourceTypes
        compilerExactEdgePlans = do
            rawTypes <- rawExactProducerTypes
            prepareCompilerExactEdgePlans
                rawTypes
                (eaEdgeTraces edgeArtifacts)
                sourceBinderRefs
        exactProducerTypes =
            IntMap.map ceepExpectedType <$> compilerExactEdgePlans
    (annotationExpectedTypesByEdge, annotationSourceNodeKeys) <-
        case prepareAnnotationExpectedTypesByEdge annExpectedTypes anns of
            Right preparedTypes -> pure preparedTypes
            Left (ValidationFailed messages) ->
                Left (Solve.ValidationFailed messages)
            Left err ->
                Left (Solve.ValidationFailed [show err])
    elaborationEdgeAuthority <-
        case
            mkElaborationEdgeAuthority
                annNodeCanonical
                annotationExpectedTypesByEdge
                edgeArtifacts
                annCanons
        of
            Right authority -> pure authority
            Left (ValidationFailed messages) ->
                Left (Solve.ValidationFailed messages)
            Left err ->
                Left (Solve.ValidationFailed [show err])
    scopeOverrideParts <-
        case
            traverse
                ( constructionScopes
                    acyclicBaseForGeneralization
                    constraintForGen
                    presolutionViewClean
                    (prRedirects pres)
                )
                anns
        of
            Left err -> Left (Solve.BindingTreeError err)
            Right parts -> Right parts
    let rootScopeOverrides = zip anns scopeOverrideParts
        scopeOverrides = mconcat scopeOverrideParts
    let
        subtermPreparation =
            do
                producerTypes <- exactProducerTypes
                exactEdgePlans <- compilerExactEdgePlans
                case
                    ( prepareSubtermGeneralizations
                        identityGenerator
                        constructionIdentityRepresentative
                        annNodeCanonical
                        acyclicBaseForGeneralization
                        presolutionViewForGen
                        rawEdgeArtifacts
                        edgeArtifacts
                        producerTypes
                        annExpectedTypes
                        (prRedirects pres)
                        bindParentsGa
                        sourceBinderRefs
                        (IntMap.map ceepConstructionRefs exactEdgePlans)
                        resultTypeView
                        ( zipWith3
                            (\scopes source canon -> (scopes, source, canon))
                            scopeOverrideParts
                            anns
                            annCanons
                        )
                    )
                  of
                    Right preparedPackets -> pure preparedPackets
                    Left cause ->
                        Left
                            ( ValidationFailed
                                [ "subterm packet preparation failed"
                                , "  roots: " ++ show annCanons
                                , "  cause: " ++ show cause
                                ]
                            )
        subtermGeneralizations = fst <$> subtermPreparation
        preparedGenerator = snd <$> subtermPreparation
    pure
        PreparedGeneralizationArtifact
            { pgaPresolutionView = presolutionViewForGen
            , pgaBindParentsGa = bindParentsGa
            , pgaExpansionConstructionPlacements = expansionConstructionPlacements
            , pgaGeneralizeAt = generalizeAtWithView
            , pgaResultTypeInputs = resultTypeInputsWithReadModels
            , pgaReadModel = readModel
            , pgaBaseReadModel = baseReadModel
            , pgaResultTypeView = resultTypeView
            , pgaElaborationEdgeAuthority = elaborationEdgeAuthority
            , pgaExactProducerTypes = exactProducerTypes
            , pgaAnnotationSourceNodeKeys = annotationSourceNodeKeys
            , pgaScopeOverrides = scopeOverrides
            , pgaRootScopeOverrides = rootScopeOverrides
            , pgaSubtermGeneralizations = subtermGeneralizations
            , pgaIdentityGenerator = preparedGenerator
            , pgaAnnotated = annCanon
            , pgaAuthorizedElaborationRoots =
                zip anns (authorizedElaborationRoots elaborationEdgeAuthority)
            , pgaAnnNodeCanonical = annNodeCanonical
            , pgaCanonical = pvCanonical presolutionViewForGen
            , pgaPlanBuilder = planBuilder
            , pgaSourceBinderRefs = sourceBinderRefs
            , pgaDirectSourceBinderKeys =
                IntMap.keysSet sourceBinderRefsDirect
            , pgaCompilerExactEdgePlans = compilerExactEdgePlans
            , pgaRedirects = prRedirects pres
            , pgaResolvedTermSchemes = Map.empty
            }

-- | Project semantic source-binder identities into every live/copy node that
-- can reify that binder during preparation.  The source sidecar is keyed in
-- the acyclic base graph, while subterm packets are built from the solved
-- generalization graph; delaying this projection until root closure leaves
-- nested packets with graph-local aliases.
--
-- Direct base entries remain authoritative.  For derived aliases we retain a
-- reference only when every path to that key agrees on semantic identity; a
-- solved class that merged distinct source binders deliberately keeps its
-- graph identity instead of selecting one by traversal order.
expandPreparedSourceBinderRefs
    :: GaBindParents 'Presolved
    -> (NodeId -> NodeId)
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
expandPreparedSourceBinderRefs ga canonical directRefs =
    IntMap.union directRefs (IntMap.mapMaybe id projectedCandidates)
  where
    classCandidates =
        IntMap.fromListWith mergeCandidate
            [ (classKeyForBase baseKey, Just ref)
            | (baseKey, ref) <- IntMap.toList directRefs
            ]

    classRefs = IntMap.mapMaybe id classCandidates

    projectedCandidates =
        IntMap.fromListWith mergeCandidate
            ( constructionRouteCandidates
                ++ liveProjectionCandidates
                ++ baseAliasCandidates
                ++ copyCandidates
            )

    -- Annotation redirects and repeated base/solved copies form one certified
    -- construction route, not merely the single hops represented by the maps
    -- below.  Publish the source identity at every node in that route before a
    -- nested packet generalizes an operated bound.  Conflicting source
    -- identities still collapse to 'Nothing' through 'mergeCandidate'.
    constructionRouteCandidates =
        [ (getNodeId routeNode, Just ref)
        | (baseKey, ref) <- IntMap.toList directRefs
        , routeNode <-
            gaConstructionRouteNodes
                canonical
                ga
                (NodeId baseKey)
        ]

    liveProjectionCandidates =
        concat
            [ let solvedNode =
                        IntMap.findWithDefault
                            (NodeId baseKey)
                            baseKey
                            (gaBaseToSolved ga)
                  canonicalSolved = canonical solvedNode
              in [ (getNodeId solvedNode, Just ref)
                 , (getNodeId canonicalSolved, Just ref)
                 ]
            | (baseKey, ref) <- IntMap.toList directRefs
            ]

    baseAliasCandidates =
        [ (baseKey, Just ref)
        | (baseKey, _) <- IntMap.toList (gaBaseToSolved ga)
        , Just ref <- [IntMap.lookup (classKeyForBase baseKey) classRefs]
        ]

    copyCandidates =
        concat
            [ [ (solvedKey, Just ref)
              , (getNodeId (canonical (NodeId solvedKey)), Just ref)
              ]
            | (solvedKey, baseNode) <- IntMap.toList (gaSolvedToBase ga)
            , Just ref <- [refForBaseNode baseNode]
            ]

    refForBaseNode baseNode =
        IntMap.lookup (getNodeId baseNode) directRefs
            <|> IntMap.lookup (classKeyForBase (getNodeId baseNode)) classRefs

    classKeyForBase baseKey =
        getNodeId
            ( canonical
                ( IntMap.findWithDefault
                    (NodeId baseKey)
                    baseKey
                    (gaBaseToSolved ga)
                )
            )

    mergeCandidate (Just left) (Just right)
        | typeBinderRefsSameIdentity left right = Just left
    mergeCandidate _ _ = Nothing

-- | Validate the binding-tree half of an identity-topology consumer
-- certificate. The body source must be owned by a strictly nested Gen scope,
-- every hop to the lambda scope must be flexible, and the allocated result
-- must be the lambda scope's distinct direct flexible child.
identityTopologyAncestryFailures
    :: BindParents
    -> GenNodeId
    -> NodeId
    -> NodeId
    -> [String]
identityTopologyAncestryFailures bindParents sourceScopeRoot sourceBodyRoot resultRoot =
    directResultFailures ++ bodyFailures
  where
    expect condition message = [message | not condition]

    directResultFailures =
        expect
            ( IntMap.lookup (nodeRefKey (typeRef resultRoot)) bindParents
                == Just (GenRef sourceScopeRoot, BindFlex)
            )
            "lambda result is not a direct flexible child of the lambda scope"

    bodyFailures =
        case bindingPathToRootLocal bindParents (typeRef sourceBodyRoot) of
            Left err ->
                ["body binding ancestry is invalid: " ++ show err]
            Right path ->
                case break (== GenRef sourceScopeRoot) path of
                    (_, []) ->
                        [ "body binding ancestry does not reach the lambda scope"
                        , "body binding path: " ++ show path
                        ]
                    (beforeScope, _ : _) ->
                        let scopedPath = beforeScope ++ [GenRef sourceScopeRoot]
                            descendantGens =
                                [ gid
                                | GenRef gid <- drop 1 beforeScope
                                ]
                            invalidHops =
                                [ ( child
                                  , parent
                                  , IntMap.lookup (nodeRefKey child) bindParents
                                  )
                                | (child, parent) <- zip scopedPath (drop 1 scopedPath)
                                , IntMap.lookup (nodeRefKey child) bindParents
                                    /= Just (parent, BindFlex)
                                ]
                         in concat
                                [ expect
                                    (not (null descendantGens))
                                    "body binding ancestry has no nested generalization scope"
                                , expect
                                    (null invalidHops)
                                    ("body binding ancestry is not entirely flexible: " ++ show invalidHops)
                                , expect
                                    (sourceBodyRoot /= resultRoot)
                                    "body source and lambda result are the same raw node"
                                ]

{- Note [Prepared subterm generalization packets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Section 15.1.2 of the thesis introduces the quantifier for the inner K
abstraction at that abstraction, not at the program root.  Constraint
generation deliberately applies Var-Abs and can therefore leave nested lambda
parameters as siblings under one graph gen node.  Re-running root closure over
the finished term cannot recover lexical ownership from that flattened spine.

Preparation instead computes a scheme packet at the Var-Abs boundary while the
source tree still supplies the lexical parameter nodes.  The ordinary case is
the nested abstraction illustrated by K; an arbitrary application or let body
does not otherwise acquire a separate abstraction scheme.  A root RaiseMerge
on the lambda-body edge is the exception: its exterior identity is direct
witness authority for a required Gamma entry, so that body receives a packet
regardless of its syntactic shape.  Constructing the packet here prevents the
algebra from emitting @!b@ without the corresponding binder.  Such a packet
records the exact body edge it constructs, so elaboration does not replay that
edge after closure and consume the newly introduced abstraction twice.

Binders corresponding to enclosing lambda parameters are removed from a nested
abstraction's packet; they remain free there and are captured by their owning
outer packet.  The algebra consumes each packet while constructing the parent
lambda, so the resulting type abstraction is introduced at the paper-prescribed
subterm boundary.
-}

prepareSubtermGeneralizations
    :: IdentityGenerator
    -> (NodeId -> NodeId)
    -> (NodeId -> NodeId)
    -> Constraint 'Presolved
    -> PresolutionView 'Presolved
    -> EdgeArtifacts
    -> EdgeArtifacts
    -> IntMap.IntMap ElabType
    -> IntMap.IntMap ElabType
    -> IntMap.IntMap NodeId
    -> GaBindParents 'Presolved
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap (IntMap.IntMap TypeBinderRef)
    -> Either ElabError (ResultTypeView 'Presolved)
    -> [(ConstructionScopes, AnnExpr, AnnExpr)]
    -> Either ElabError (SubtermGeneralizations, IdentityGenerator)
prepareSubtermGeneralizations identityGenerator identityRepresentative constructionCanonical baseConstraint presolutionView rawEdgeArtifacts edgeArtifacts exactProducerTypes annExpectedTypes redirects bindParentsGa sourceBinderRefs compilerExactConstructionRefs resultTypeView scopedRoots =
    foldM collectRoot (Map.empty, identityGenerator) scopedRoots
  where
    sources = [source | (_, source, _) <- scopedRoots]
    canons = [canon | (_, _, canon) <- scopedRoots]

    preparedLocalGammaClosures scopeOverrides canon =
        rbeLocallyClosedGammas
            <$> rootBoundaryInstantiationEdges
                (packetScopeRootForBoundary scopeOverrides)
                bindParentsGa
                edgeArtifacts
                []
                canon

    packetScopeRootForBoundary scopeOverrides edgeId fallbackNode =
        resolveConstructionScopeForBoundary
            constructionCanonical
            bindParentsGa
            scopeOverrides
            edgeId
            fallbackNode

    expectedEndpointsAgree left right =
        let leftType = packetExpectedType left
            rightType = packetExpectedType right
         in alphaEqType leftType rightType
                || churchAwareEqType leftType rightType

    collectRoot (packets, generator) (scopeOverrides, source, canon) = do
        localGammaClosures <-
            preparedLocalGammaClosures scopeOverrides canon
        (rootPackets, generator') <-
            collect
                localGammaClosures
                sourceBinderRefs
                annotationBoundOverlays
                Nothing
                generator
                source
                canon
        packets' <- mergeSubtermGeneralizations packets rootPackets
        pure (packets', generator')

    collect localGammaClosures localSourceBinderRefs boundOverlays expectedType generator source canon =
        case (source, canon) of
            ( ALam _ sourceDetails _sourceParam sourceScopeRoot sourceBody sourceBodyEdge _
              , ALam _ canonDetails canonParam canonScopeRoot canonBody _ canonLambdaNode
              ) -> do
                lambdaBodySourceBinderRefs <-
                    installExpectedLambdaParameterSourceRef
                        canonLambdaNode
                        canonParam
                        expectedType
                        localSourceBinderRefs
                let outerExpectedBodyType = expectedType >>= packetLambdaBodyType
                sourceExpectedBodyType <-
                    case sourceBody of
                        AAnn _ sourceAnnNode _ ->
                            case IntMap.lookup (getNodeId sourceAnnNode) annExpectedTypes of
                                Just sourceType ->
                                    pure (Just (sourcePacketExpectedType sourceType))
                                Nothing ->
                                    Left
                                        ( ValidationFailed
                                            [ "lambda body source annotation has no prepared identity-bearing type"
                                            , "  annotation node: " ++ show sourceAnnNode
                                            ]
                                        )
                        _ -> pure Nothing
                expectedBodyType <-
                    case (sourceExpectedBodyType, outerExpectedBodyType) of
                        (Just sourceExpected, Just outerExpected)
                            | expectedEndpointsAgree sourceExpected outerExpected ->
                                -- The nearest source annotation owns the
                                -- lambda-body endpoint. Retain its authority
                                -- while preparing the parent packet so an
                                -- enclosing exact wrapper cannot manufacture
                                -- a duplicate result carrier.
                                pure (Just sourceExpected)
                            | otherwise ->
                                Left
                                    ( ValidationFailed
                                        [ "lambda body source annotation disagrees with its enclosing exact endpoint"
                                        , "  source endpoint: " ++ show (packetExpectedType sourceExpected)
                                        , "  enclosing endpoint: " ++ show (packetExpectedType outerExpected)
                                        ]
                                    )
                        (Just sourceExpected, Nothing) -> pure (Just sourceExpected)
                        (Nothing, mbOuterExpected) -> pure mbOuterExpected
                (descendants, generatorAfterDescendants) <-
                    collect
                        localGammaClosures
                        lambdaBodySourceBinderRefs
                        boundOverlays
                        expectedBodyType
                        generator
                        sourceBody
                        canonBody
                let enclosingConsumerOwner =
                        LocalGammaOwner
                            { lgoConstructor = LocalLambdaGamma
                            , lgoBoundaryEdge = sourceBodyEdge
                            , lgoTermNode = canonLambdaNode
                            , lgoScope = GenRef canonScopeRoot
                            }
                    consumedByStrictDescendant sourceRoot packet =
                        case subtermGeneralizationConsumerAuthority packet of
                            Just authority
                                | not
                                    ( subtermConsumerAuthorityIsTopology
                                        authority
                                    )
                                , Just owner <-
                                    subtermConsumerAuthorityEnclosingOwner
                                        authority ->
                                    lgoTermNode owner /= annNode sourceRoot
                                        && localGammaOwnerOccursIn
                                            owner
                                            sourceRoot
                            _ -> False
                    prepareBodyPacket ownerKey currentConstructionOwner availableSourceBinderRefs exactParameterDeclarationIdentities mbRequiredLambdaParam mbEnclosingParam mbConsumer mbConsumerOwner mbBodyGammaAuthority packetGammaAuthority packetTopologyAuthority expectedPacketType sourcePacketRoot canonPacketRoot = do
                        packetSourceBinderRefs <-
                            installRequiredLambdaParameterSourceRef
                                mbRequiredLambdaParam
                                availableSourceBinderRefs
                        let resultOwnership =
                                subtermResultOwnershipFor
                                    canonPacketRoot
                                    descendants
                            resultOwnedDescendants =
                                case resultOwnership of
                                    Just ownership
                                        | subtermResultOwnershipHasTransparentPath
                                            ownership ->
                                            subtermGeneralizationsOwnedBy
                                            canonPacketRoot
                                            descendants
                                    _ -> Map.empty
                            -- Crossing a lambda makes the result path opaque:
                            -- the enclosing packet must not claim ownership of
                            -- the descendant packet.  The completed
                            -- administrative packet still constructs the
                            -- exact graph carrier at that crossed lambda,
                            -- however.  Retain that positive construction
                            -- fact so Gen can leave the carrier open only
                            -- until the packet is composed into its result.
                            opaqueResultConstructions =
                                case resultOwnership of
                                    Just ownership
                                        | not
                                            ( subtermResultOwnershipHasTransparentPath
                                                ownership
                                            )
                                        , let packet =
                                                subtermResultOwnershipPacket
                                                    ownership
                                        , Just (certifiedLambdaNode, constructedType) <-
                                            subtermGeneralizationSourceLambdaResultConstruction
                                                packet
                                        , certifiedLambdaNode
                                            == subtermResultOwnershipLambdaNode
                                                ownership ->
                                            [ ( typeBinderRefFromIdentity
                                                    ( typeBinderIdentityFromNode
                                                        certifiedLambdaNode
                                                    )
                                                    ( "t"
                                                        ++ show
                                                            ( getNodeId
                                                                certifiedLambdaNode
                                                            )
                                                    )
                                              , constructedType
                                              , packet
                                              )
                                            ]
                                    _ -> []
                            explicitlyOwnedDescendants =
                                Map.filter
                                    ( \packet ->
                                        case
                                            subtermGeneralizationConsumerAuthority packet
                                                >>= subtermConsumerAuthorityEnclosingOwner
                                        of
                                            Just owner ->
                                                owner == currentConstructionOwner
                                            Nothing -> False
                                    )
                                    descendants
                            -- Result transparency alone cannot see through a
                            -- let-bound variable back to its RHS packet.  The
                            -- packet's exact enclosing-owner capability still
                            -- proves which lambda must place it, so retain both
                            -- independently established ownership routes.
                            ownedDescendants =
                                Map.union
                                    explicitlyOwnedDescendants
                                    resultOwnedDescendants
                            -- Result ownership and scope-edge requirements are
                            -- alternative constructions of a descendant
                            -- packet.  A transparent result is already in
                            -- 'ownedDescendants'; an opaque result is composed
                            -- through 'opaqueResultConstructions'.  In either
                            -- case, exposing the same transitive packets to
                            -- Gen would consume their completed declarations a
                            -- second time.  Only when the source result walk
                            -- has no ownership certificate may a consumer-only
                            -- packet supply exact S'(operated) evidence for an
                            -- enclosing scope edge.  Packets with their own
                            -- Gamma remain closed by that nested construction.
                            requirementDescendants =
                                Map.union
                                    ownedDescendants
                                    consumerOnlyDescendants
                            consumerOnlyDescendants =
                                case resultOwnership of
                                    Just _ -> Map.empty
                                    Nothing ->
                                        Map.filter
                                            ( isNothing
                                                . subtermGeneralizationGammaAuthority
                                            )
                                            descendants
                            descendantsPlacedByCurrentConstruction =
                                Map.filter
                                    ( not
                                        . consumedByStrictDescendant
                                            sourcePacketRoot
                                    )
                                    ownedDescendants
                            mbConsumerIdentity =
                                preparedLambdaBodyConsumerIdentity <$> mbConsumer
                            consumerRequiresGamma =
                                maybe False preparedLambdaBodyConsumerRequiresGamma mbConsumer
                            consumerPlacement consumer mbGammaAuthority =
                                case (consumer, mbConsumerOwner, mbGammaAuthority) of
                                    (PreparedRootRaiseMergeBodyConsumer consumerEdge _, Just consumerOwner, Nothing) ->
                                        pure
                                            ( EnclosingConsumerPacket
                                                (preparedLambdaBodyConsumerIdentity consumer)
                                                consumerEdge
                                                consumerOwner
                                            )
                                    (PreparedRootRaiseMergeBodyConsumer consumerEdge _, Just consumerOwner, Just gammaAuthority) ->
                                        pure
                                            ( EnclosingConsumerGammaPacket
                                                (preparedLambdaBodyConsumerIdentity consumer)
                                                consumerEdge
                                                consumerOwner
                                                gammaAuthority
                                            )
                                    (PreparedRootRaiseMergeBodyConsumer consumerEdge _, Nothing, Nothing) ->
                                        pure
                                            ( RootConsumerPacket
                                                (preparedLambdaBodyConsumerIdentity consumer)
                                                consumerEdge
                                            )
                                    (PreparedRootRaiseMergeBodyConsumer consumerEdge _, Nothing, Just gammaAuthority) ->
                                        pure
                                            ( RootConsumerGammaPacket
                                                (preparedLambdaBodyConsumerIdentity consumer)
                                                consumerEdge
                                                gammaAuthority
                                            )
                                    (PreparedIdentityTopologyBodyConsumer consumerEdge topologySourceScopeRoot sourceBodyRoot boundaryScopeRoot boundaryBodyRoot frozenResultRoot, Just consumerOwner, Nothing) -> do
                                        topologyAuthority <-
                                            mkIdentityTopologyConsumerAuthority
                                                (gaRestoredSchemeRootTargets bindParentsGa)
                                                consumerEdge
                                                topologySourceScopeRoot
                                                sourceBodyRoot
                                                boundaryScopeRoot
                                                boundaryBodyRoot
                                                frozenResultRoot
                                                consumerOwner
                                        pure
                                            (TopologyConsumerPacket topologyAuthority)
                                    (PreparedIdentityTopologyBodyConsumer consumerEdge topologySourceScopeRoot sourceBodyRoot boundaryScopeRoot boundaryBodyRoot frozenResultRoot, Just consumerOwner, Just gammaAuthority) -> do
                                        topologyAuthority <-
                                            mkIdentityTopologyConsumerAuthority
                                                (gaRestoredSchemeRootTargets bindParentsGa)
                                                consumerEdge
                                                topologySourceScopeRoot
                                                sourceBodyRoot
                                                boundaryScopeRoot
                                                boundaryBodyRoot
                                                frozenResultRoot
                                                consumerOwner
                                        pure
                                            ( TopologyConsumerGammaPacket
                                                topologyAuthority
                                                gammaAuthority
                                            )
                                    (PreparedIdentityTopologyBodyConsumer consumerEdge _ _ _ _ _, Nothing, _) ->
                                        Left
                                            ( ValidationFailed
                                                [ "topology consumer lost its lexical owner"
                                                , "  edge: " ++ show consumerEdge
                                                , "  consumer: " ++ show (preparedLambdaBodyConsumerIdentity consumer)
                                                ]
                                            )
                            exactGammaOperatedType = do
                                gammaAuthority <- mbBodyGammaAuthority
                                sourceType <-
                                    IntMap.lookup
                                        (getEdgeId (gpaEdgeId gammaAuthority))
                                        exactProducerTypes
                                pure (sourcePacketExpectedType sourceType)
                        sourceConstruction <-
                            sourceConstructionResultTypeWithPackets
                                descendants
                                sourcePacketRoot
                                canonPacketRoot
                        let sourceConstructionType = scrType <$> sourceConstruction
                            sourceConstructionReturnsBinding =
                                maybe
                                    False
                                    ( isExactReturnedBindingConstruction
                                        . scrOrigin
                                    )
                                    sourceConstruction
                        sourceConsumerExpectedType <-
                            case
                                ( expectedPacketType
                                , mbConsumer
                                , sourceConstructionType
                                )
                            of
                                ( Nothing
                                  , Just _
                                  , Just computedType
                                  ) ->
                                        let consumerRefs =
                                                maybeToList mbConsumerIdentity
                                            computedRefs =
                                                typeBinderDeclarationRefs computedType
                                                    ++ freeTypeVarRefsType computedType
                                            retainsConsumer =
                                                any
                                                    ( \consumerIdentity ->
                                                        any
                                                            ( (== consumerIdentity)
                                                                . typeBinderRefIdentity
                                                            )
                                                            computedRefs
                                                    )
                                                    consumerRefs
                                         in pure
                                                ( if retainsConsumer
                                                    then Nothing
                                                    else
                                                        Just
                                                            ( ( if sourceConstructionReturnsBinding
                                                                    then returnedBindingSourcePacketExpectedType
                                                                    else sourcePacketExpectedType
                                                              )
                                                                computedType
                                                            )
                                                )
                                _ -> pure Nothing
                        let operatedExpectedType =
                                expectedPacketType
                                    <|> sourceConsumerExpectedType
                                    <|> exactGammaOperatedType
                        sourceOwnerConstructionType <-
                            case mbConsumerOwner of
                                Just owner ->
                                    sourceConstructionTypeForOwner
                                        descendants
                                        owner
                                        ( if owner == enclosingConsumerOwner
                                            then expectedType
                                            else Nothing
                                        )
                                        ( sourceConstructionType
                                            <|> ( packetOperatedExpectedType
                                                    <$> operatedExpectedType
                                                )
                                        )
                                Nothing -> pure Nothing
                        sourceOwnerBodyConstructionType <-
                            case mbConsumerOwner of
                                Just owner ->
                                    sourceBodyConstructionTypeForOwner
                                        descendants
                                        owner
                                Nothing -> pure Nothing
                        let
                            eligibleSourceConsumerExpectedType mbComputedType =
                                case
                                    ( mbConsumerIdentity
                                    , mbComputedType
                                    )
                                  of
                                    (Just consumerIdentity, Just computedType)
                                        | not
                                            ( any
                                                ( (== consumerIdentity)
                                                    . typeBinderRefIdentity
                                                )
                                                ( typeBinderDeclarationRefs
                                                    computedType
                                                    ++ freeTypeVarRefsType
                                                        computedType
                                                )
                                            ) ->
                                                Just
                                                    ( sourcePacketExpectedType
                                                        computedType
                                                    )
                                    _ -> Nothing
                            sourceOwnerConsumerExpectedType =
                                eligibleSourceConsumerExpectedType
                                    sourceOwnerConstructionType
                            sourceOwnerBodyConsumerExpectedType =
                                eligibleSourceConsumerExpectedType
                                    sourceOwnerBodyConstructionType
                            sourceBodyConsumerExpectedType =
                                eligibleSourceConsumerExpectedType
                                    ( if isNothing expectedPacketType
                                        then sourceConstructionType
                                        else Nothing
                                    )
                            operatedDeclarationIdentities =
                                Set.union
                                    exactParameterDeclarationIdentities
                                    ( case operatedExpectedType of
                                        Just expected ->
                                            Set.fromList
                                                ( map
                                                    typeBinderRefIdentity
                                                    ( expectedAmbientDeclarations expected
                                                        ++ typeBinderDeclarationRefs
                                                            (packetOperatedExpectedType expected)
                                                    )
                                                )
                                        Nothing -> Set.empty
                                    )
                            expectedAmbientDeclarations expected =
                                case expected of
                                    CompilerExactExpectedType{} ->
                                        map fst
                                            (packetExpectedAmbientBinders expected)
                                    _ -> []
                            structuredParameterDeclarationIdentities =
                                maybe
                                    Set.empty
                                    ( Set.fromList
                                        . map typeBinderRefIdentity
                                        . typeBinderDeclarationRefs
                                    )
                                    ( mbRequiredLambdaParam
                                        >>= rlpStructuredParameterType
                                    )
                            descendantConsumerDeclarationIdentities =
                                Set.fromList
                                    [ consumerIdentity
                                    | packet <-
                                        Map.elems
                                            descendantsPlacedByCurrentConstruction
                                    , Just authority <-
                                        [subtermGeneralizationConsumerAuthority packet]
                                    , not
                                        ( subtermConsumerAuthorityIsTopology
                                            authority
                                        )
                                    , isJust
                                        ( subtermConsumerAuthorityEnclosingOwner
                                            authority
                                        )
                                    , consumerIdentity <-
                                        maybeToList
                                            ( subtermGeneralizationConsumerIdentity
                                                packet
                                            )
                                    ]
                            packetConstructionAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( packetAmbientBinderRefs
                                        packetSourceBinderRefs
                                        operatedExpectedType
                                        mbEnclosingParam
                                        ++ concatMap
                                            opaqueResultConstructionAmbientRefs
                                            opaqueResultConstructions
                                    )
                            descendantTermUsedDeclarationIdentities =
                                packetTermUsedBinderIdentities
                                    packetSourceBinderRefs
                                    packetConstructionAmbientBinderRefs
                                    ownedDescendants
                            -- An enclosing-consumer packet is positive
                            -- declaration ownership for the child Gamma slot.
                            -- Preserve that exact identity until the child is
                            -- placed in the parent packet; a source sidecar
                            -- alias at the same graph occurrence cannot turn
                            -- the local declaration into an ambient capture.
                            packetProjectionProtectedIdentities =
                                Set.unions
                                    [ structuredParameterDeclarationIdentities
                                    , operatedDeclarationIdentities
                                    , descendantConsumerDeclarationIdentities
                                    , descendantTermUsedDeclarationIdentities
                                    ]
                        ( _bodyTarget
                          , bodyPacketRaw0
                          , operatedPacketRaw0
                          , inheritedGammaRoutes
                          ) <-
                            case
                                generalizeBody
                                    (localGammaOwnerScope currentConstructionOwner)
                                    mbRequiredLambdaParam
                                    mbEnclosingParam
                                    packetSourceBinderRefs
                                    requirementDescendants
                                    mbBodyGammaAuthority
                                    operatedExpectedType
                                    boundOverlays
                                    ownedDescendants
                                    descendantsPlacedByCurrentConstruction
                                    opaqueResultConstructions
                                    sourcePacketRoot
                                    canonPacketRoot
                              of
                                Right generalized -> pure generalized
                                Left cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "subterm body packet planning failed"
                                            , "  packet owner key: "
                                                ++ show ownerKey
                                            , "  construction owner: "
                                                ++ show currentConstructionOwner
                                            , "  body consumer: " ++ show mbConsumer
                                            , "  body consumer owner: "
                                                ++ show mbConsumerOwner
                                            , "  body Gamma authority: "
                                                ++ show mbBodyGammaAuthority
                                            , "  packet Gamma authority: "
                                                ++ show packetGammaAuthority
                                            , "  expected packet type: "
                                                ++ show
                                                    ( packetExpectedType
                                                        <$> expectedPacketType
                                                    )
                                            , "  source construction: "
                                                ++ show
                                                    ( (\construction -> (scrType construction, scrOrigin construction))
                                                        <$> sourceConstruction
                                                    )
                                            , "  operated expected type: "
                                                ++ show
                                                    ( packetExpectedType
                                                        <$> operatedExpectedType
                                                    )
                                            , "  all descendant authorities: "
                                                ++ show
                                                    [ ( key
                                                      , subtermGeneralizationConsumerAuthority packet
                                                      , subtermGeneralizationGammaAuthority packet
                                                      , siScheme
                                                            ( subtermGeneralizationSchemeInfo
                                                                packet
                                                            )
                                                      , subtermGeneralizationGammaBoundScheme
                                                            packet
                                                      )
                                                    | (key, packet) <-
                                                        Map.toList descendants
                                                    ]
                                            , "  owned descendant authorities: "
                                                ++ show
                                                    [ ( key
                                                      , subtermGeneralizationConsumerAuthority packet
                                                      , subtermGeneralizationGammaAuthority packet
                                                      , siScheme
                                                            ( subtermGeneralizationSchemeInfo
                                                                packet
                                                            )
                                                      , subtermGeneralizationGammaBoundScheme
                                                            packet
                                                      )
                                                    | (key, packet) <-
                                                        Map.toList ownedDescendants
                                                    ]
                                            , "  result ownership: "
                                                ++ show resultOwnership
                                            , "  opaque result constructions: "
                                                ++ show
                                                    [ ( carrierRef
                                                      , constructedType
                                                      , subtermGeneralizationSourceLambdaResultConstruction
                                                            packet
                                                      )
                                                    | ( carrierRef
                                                        , constructedType
                                                        , packet
                                                        ) <- opaqueResultConstructions
                                                    ]
                                            , "  cause: " ++ show cause
                                            ]
                                        )
                        -- Lambda restoration can introduce the graph carrier
                        -- for an otherwise-unused source parameter.  Resolve
                        -- source ownership only after that carrier exists, so
                        -- the packet cannot quantify both the graph alias and
                        -- its generated source identity.
                        bodyPacketConstruction0 <-
                            either
                                ( \cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "subterm construction packet has inconsistent source-binder provenance"
                                            , "  cause: " ++ cause
                                            ]
                                        )
                                )
                                Right
                                ( resolveConstructionSourceBindersInSchemeInfoExcept
                                    packetProjectionProtectedIdentities
                                    identityRepresentative
                                    packetSourceBinderRefs
                                    bodyPacketRaw0
                                )
                        bodyPacketConstruction <-
                            publishTopologyConsumerRoutes
                                (gaConstructionRouteNodes constructionCanonical bindParentsGa)
                                ownedDescendants
                                bodyPacketConstruction0
                        let sourceOwnerCompletedDescendantBound
                                packet
                                targetRef
                                packetBound
                                _constructedBound =
                                    directCompletion packet targetRef
                                        <|> sharedCompletion
                                            packet
                                            targetRef
                                            packetBound

                            directCompletion packet targetRef = do
                                (_, completedEndpoint) <-
                                    certifiedTransition targetRef packet
                                pure completedEndpoint

                            sharedCompletion packet targetRef packetBound = do
                                authority <-
                                    subtermGeneralizationConsumerAuthority packet
                                owner <-
                                    subtermConsumerAuthorityEnclosingOwner
                                        authority
                                guard (owner == currentConstructionOwner)
                                guard
                                    ( typeBinderRefsSameIdentity
                                        targetRef
                                        ( typeBinderRefFromIdentity
                                            (scaConsumerIdentity authority)
                                            "$consumer"
                                        )
                                    )
                                firstTransition : remainingTransitions <-
                                    pure
                                        ( mapMaybe
                                            (certifiedTransition targetRef)
                                            ( Map.elems
                                                descendantsPlacedByCurrentConstruction
                                            )
                                        )
                                let (frozenOperatedType, completedEndpoint) =
                                        firstTransition
                                guard
                                    ( all
                                        ( \(otherFrozen, otherCompleted) ->
                                            endpointTypesAgree
                                                frozenOperatedType
                                                otherFrozen
                                                && endpointTypesAgree
                                                    completedEndpoint
                                                    otherCompleted
                                        )
                                        remainingTransitions
                                    )
                                guard
                                    ( endpointTypesAgree
                                        packetBound
                                        frozenOperatedType
                                    )
                                pure completedEndpoint

                            certifiedTransition targetRef packet = do
                                ( authority
                                  , certifiedOwner
                                  , frozenOperatedType
                                  , completedEndpoint
                                  ) <-
                                    subtermGeneralizationSourceOwnerConsumerCompletion
                                        packet
                                guard
                                    ( certifiedOwner
                                        == currentConstructionOwner
                                    )
                                guard
                                    ( typeBinderRefsSameIdentity
                                        targetRef
                                        ( typeBinderRefFromIdentity
                                            (scaConsumerIdentity authority)
                                            "$consumer"
                                        )
                                    )
                                pure
                                    ( frozenOperatedType
                                    , completedEndpoint
                                    )

                            endpointTypesAgree left right =
                                alphaEqType left right
                                    || churchAwareEqType left right
                        bodyBinderPlacement <-
                            case
                                placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy
                                    sourceOwnerCompletedDescendantBound
                                    (siSubstRefs bodyPacketConstruction)
                                    descendantsPlacedByCurrentConstruction
                                    (siScheme bodyPacketConstruction)
                            of
                                Right placement -> pure placement
                                Left cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "subterm descendant placement lost its enclosing construction consumer"
                                            , "  raw body packet: "
                                                ++ show bodyPacketRaw0
                                            , "  projected body packet: "
                                                ++ show bodyPacketConstruction
                                            , "  protected identities: "
                                                ++ show
                                                    packetProjectionProtectedIdentities
                                            , "  source binder refs: "
                                                ++ show packetSourceBinderRefs
                                            , "  descendants: "
                                                ++ show
                                                    descendantsPlacedByCurrentConstruction
                                            , "  cause: " ++ show cause
                                            ]
                                        )
                        operatedPacketWithConsumerRoutes <-
                            publishTopologyConsumerRoutes
                                (gaConstructionRouteNodes constructionCanonical bindParentsGa)
                                ownedDescendants
                                operatedPacketRaw0
                        operatedSchemeOrdered <-
                            either
                                (\cause -> Left (ValidationFailed [cause]))
                                Right
                                ( orderSourceProjectedSchemeBinders
                                    "subterm operated packet"
                                    (siScheme operatedPacketWithConsumerRoutes)
                                )
                        let operatedViewDescendants =
                                Map.filter
                                    ( not
                                        . consumedByConstructionOnlyConsumer
                                    )
                                    descendantsPlacedByCurrentConstruction
                            consumedByConstructionOnlyConsumer packet =
                                case
                                    ( subtermGeneralizationConsumerAuthority
                                        packet
                                    , subtermGeneralizationConsumerIdentity
                                        packet
                                    )
                                of
                                    (Just authority, Just consumerIdentity) ->
                                        not
                                            ( subtermConsumerAuthorityIsTopology
                                                authority
                                            )
                                            && isJust
                                                ( subtermConsumerAuthorityEnclosingOwner
                                                    authority
                                                )
                                            && schemeDeclares
                                                consumerIdentity
                                                (siScheme bodyPacketConstruction)
                                            && not
                                                ( schemeDeclares
                                                    consumerIdentity
                                                    operatedSchemeOrdered
                                                )
                                    _ -> False
                            schemeDeclares identity =
                                any
                                    ( (== identity)
                                        . typeBinderRefIdentity
                                    )
                                    . typeBinderDeclarationRefs
                                    . schemeToType
                        operatedScheme <-
                            case
                                placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy
                                    sourceOwnerCompletedDescendantBound
                                    (siSubstRefs operatedPacketWithConsumerRoutes)
                                    operatedViewDescendants
                                    operatedSchemeOrdered
                            of
                                Right placement ->
                                    pure
                                        ( placedSubtermBinderScheme
                                            placement
                                        )
                                Left cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "subterm operated-view placement lost its enclosing construction consumer"
                                            , "  raw operated packet: "
                                                ++ show operatedPacketRaw0
                                            , "  routed operated packet: "
                                                ++ show
                                                    operatedPacketWithConsumerRoutes
                                            , "  completed body placement: "
                                                ++ show bodyBinderPlacement
                                            , "  protected identities: "
                                                ++ show
                                                    packetProjectionProtectedIdentities
                                            , "  descendants: "
                                                ++ show
                                                    operatedViewDescendants
                                            , "  cause: " ++ show cause
                                            ]
                                        )
                        let bodySchemePlaced =
                                placedSubtermBinderScheme bodyBinderPlacement
                            bodyPlacedCopiedBinderRoutes =
                                placedSubtermCopiedBinderRoutes
                                    bodyBinderPlacement
                            bodyPacketPlaced =
                                publishPlacedSubtermConstructionBinderOrder
                                    descendantsPlacedByCurrentConstruction
                                    ( rebuildSchemeInfoFromRefSubst
                                        bodyPacketConstruction
                                        bodySchemePlaced
                                        (siSubstRefs bodyPacketConstruction)
                                    )
                        -- Descendant placement composes complete packet bounds
                        -- and can therefore add graph carriers that were not
                        -- present in the parent's raw scheme.  Re-project the
                        -- composed packet before it becomes closure authority.
                        bodyPacket <-
                            either
                                ( \cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "composed subterm construction packet has inconsistent source-binder provenance"
                                            , "  cause: " ++ cause
                                            ]
                                        )
                                )
                                Right
                                ( resolveConstructionSourceBindersInSchemeInfoExcept
                                    packetProjectionProtectedIdentities
                                    identityRepresentative
                                    packetSourceBinderRefs
                                    bodyPacketPlaced
                                )
                        (operatedPacket, constructionBinderRenames, exactBinderRenames) <-
                            constructPacketOperatedScheme
                                identityRepresentative
                                constructionCanonical
                                (resultTypeViewWithOverlays boundOverlays)
                                packetSourceBinderRefs
                                (Just bodyPacket)
                                operatedExpectedType
                                ( publishPlacedSubtermConstructionBinderOrder
                                    operatedViewDescendants
                                    ( rebuildSchemeInfoFromRefSubst
                                        operatedPacketWithConsumerRoutes
                                        operatedScheme
                                        (siSubstRefs operatedPacketWithConsumerRoutes)
                                    )
                                )
                        constructionPacket0 <-
                            completeAdministrativeSourceConstruction
                                mbRequiredLambdaParam
                                operatedExpectedType
                                ( maybeToList mbConsumerIdentity
                                    ++ map
                                        gpaConsumerIdentity
                                        (maybeToList packetGammaAuthority)
                                )
                                constructionBinderRenames
                                bodyPacket
                                operatedPacket
                        let constructionPacket = constructionPacket0
                            bodyScheme = siScheme constructionPacket
                        let needsPacket =
                                isJust mbConsumer
                                    || isJust packetTopologyAuthority
                                    || not (Map.null ownedDescendants)
                                    || not (null (schemeBinderRefs bodyScheme))
                                    || not (null constructionBinderRenames)
                                    || not (null exactBinderRenames)
                        exactResultCandidate <-
                            compilerExactPacketResult
                                expectedPacketType
                                constructionPacket
                                operatedPacket
                        let exactResult =
                                case exactResultCandidate of
                                    Just
                                        ( candidate@(PacketOwnedCompilerExactPacketResult
                                                exactEdge
                                                packetResultRef
                                            )
                                        )
                                        | descendantOwnsExactResult
                                            candidate
                                            ownedDescendants ->
                                            Just
                                                ( DescendantOwnedCompilerExactPacketResult
                                                    exactEdge
                                                    packetResultRef
                                                )
                                    _ -> exactResultCandidate
                        if not needsPacket
                            then pure (descendants, generatorAfterDescendants)
                            else do
                                basePlacement <-
                                    case (mbConsumer, packetGammaAuthority) of
                                        (Just consumer, Just gammaAuthority) ->
                                            if gpaEdgeId gammaAuthority == sourceBodyEdge
                                                && consumerRequiresGamma
                                                then
                                                    if gpaConsumerIdentity gammaAuthority
                                                        == preparedLambdaBodyConsumerIdentity consumer
                                                        then pure (GammaPacket gammaAuthority)
                                                        else
                                                            Left
                                                                ( ValidationFailed
                                                                    [ "one Gamma edge carries conflicting consumer identities"
                                                                    , "  edge: " ++ show sourceBodyEdge
                                                                    , "  enclosing consumer: " ++ show (preparedLambdaBodyConsumerIdentity consumer)
                                                                    , "  packet consumer: " ++ show (gpaConsumerIdentity gammaAuthority)
                                                                    ]
                                                                )
                                                else
                                                    consumerPlacement
                                                        consumer
                                                        (Just gammaAuthority)
                                        (Just consumer, Nothing) ->
                                            consumerPlacement consumer Nothing
                                        (Nothing, Just gammaAuthority) ->
                                            pure (GammaPacket gammaAuthority)
                                        (Nothing, Nothing) ->
                                            pure DirectPacket
                                let placement =
                                        maybe
                                            basePlacement
                                            (WithLocalTopologyResult basePlacement)
                                            packetTopologyAuthority
                                (preparedBodyPacket, generator') <-
                                    prepareSubtermGeneralizationPacket
                                        generatorAfterDescendants
                                        placement
                                        constructionPacket
                                        operatedPacket
                                preparedBodyPacketWithPlacedCopies <-
                                    withPlacedCopiedBinderRoutes
                                        bodyPlacedCopiedBinderRoutes
                                        preparedBodyPacket
                                -- A source existential can occur only in this
                                -- packet's Gamma bound and therefore be absent
                                -- from the parent's result graph.  Its exact
                                -- generated identity plus the source sidecar
                                -- is lexical capability: retain it with the
                                -- packet now, before descendant placement
                                -- would otherwise try to quotient it to the
                                -- consumer and manufacture a self-bound forall.
                                let preparedPacketGammaFreeRefs =
                                        freeTypeVarRefsType
                                            ( schemeToType
                                                ( subtermGeneralizationGammaBoundScheme
                                                    preparedBodyPacketWithPlacedCopies
                                                )
                                            )
                                    packetSourceLexicalRefs =
                                        [ packetRef
                                        | packetRef <- preparedPacketGammaFreeRefs
                                        , any
                                            (typeBinderRefsSameIdentity packetRef)
                                            (IntMap.elems packetSourceBinderRefs)
                                        ]
                                    expectedAmbientRefs =
                                        distinctTypeBinderRefs
                                            [ ambientRef
                                            | ambientRef <-
                                                map fst
                                                    ( maybe
                                                        []
                                                        packetExpectedAmbientBinders
                                                        expectedPacketType
                                                    )
                                                ++ packetSourceLexicalRefs
                                            , any
                                                (typeBinderRefsSameIdentity ambientRef)
                                                preparedPacketGammaFreeRefs
                                            ]
                                    expectedAmbientRoutes =
                                        Reify.inheritedGammaRoutesFromLexicalRefs
                                            expectedAmbientRefs
                                preparedBodyPacketWithExpectedAmbientRoutes <-
                                    withInheritedGammaRoutes
                                        expectedAmbientRoutes
                                        preparedBodyPacketWithPlacedCopies
                                preparedBodyPacketWithInheritedRoutes <-
                                    withInheritedGammaRoutes
                                        inheritedGammaRoutes
                                        preparedBodyPacketWithExpectedAmbientRoutes
                                let preparedBodyPacketWithConstructionRenames =
                                        withConstructionBinderRenames
                                            constructionBinderRenames
                                            preparedBodyPacketWithInheritedRoutes
                                preparedBodyPacketWithBinderRenames <-
                                    withCompilerExactBinderRenames
                                        exactBinderRenames
                                        preparedBodyPacketWithConstructionRenames
                                preparedBodyPacketWithAdministrativeParameter <-
                                    case mbRequiredLambdaParam of
                                        Nothing ->
                                            pure preparedBodyPacketWithBinderRenames
                                        Just requiredParam ->
                                            withSourceLambdaParameter
                                                ( gaConstructionRouteNodes
                                                    constructionCanonical
                                                    bindParentsGa
                                                )
                                                (rlpLambdaNode requiredParam)
                                                (rlpParameterNode requiredParam)
                                                ( rlpStructuredParameterType
                                                    requiredParam
                                                )
                                                preparedBodyPacketWithBinderRenames
                                preparedBodyPacketWithOpaqueResult <-
                                    case
                                        ( opaqueResultConstructions
                                        , sourceConstructionType
                                        )
                                    of
                                        (_ : _, Just constructedType) ->
                                            withOpaqueResultConstruction
                                                [ (carrierRef, producerPacket)
                                                | (carrierRef, _, producerPacket) <-
                                                    opaqueResultConstructions
                                                ]
                                                constructedType
                                                preparedBodyPacketWithAdministrativeParameter
                                        _ ->
                                            pure
                                                preparedBodyPacketWithAdministrativeParameter
                                preparedBodyPacketWithExactConsumer <-
                                    let expectedEndpoints =
                                            [ exactConsumerEndpoint
                                                        mbRequiredLambdaParam
                                                        expected
                                                        operatedPacket
                                                   | Just expected <-
                                                        [ sourceBodyConsumerExpectedType
                                                        , operatedExpectedType
                                                        ]
                                                   ]
                                                ++ [ packetOperatedExpectedType expected
                                                   | Just expected <-
                                                        [sourceOwnerConsumerExpectedType]
                                                   ]
                                     in
                                    case
                                        ( mbConsumer
                                        , mbConsumerOwner
                                        , expectedEndpoints
                                        )
                                      of
                                        ( Just PreparedRootRaiseMergeBodyConsumer {}
                                          , Just _
                                          , _ : _
                                          ) ->
                                            pure
                                                ( withExactConsumerSpecialization
                                                    expectedEndpoints
                                                    preparedBodyPacketWithOpaqueResult
                                                )
                                        ( Just PreparedIdentityTopologyBodyConsumer {}
                                          , Just _
                                          , _ : _
                                          ) ->
                                            pure
                                                ( withExactConsumerSpecialization
                                                    expectedEndpoints
                                                    preparedBodyPacketWithOpaqueResult
                                                )
                                        _ ->
                                            pure preparedBodyPacketWithOpaqueResult
                                preparedBodyPacketWithSourceOwnerCompletion <-
                                    case
                                        ( mbConsumerOwner
                                        , sourceOwnerBodyConsumerExpectedType
                                            <|> sourceBodyConsumerExpectedType
                                        )
                                      of
                                        (Just owner, Just expected)
                                            | isNothing
                                                ( subtermGeneralizationExactConsumerSpecialization
                                                    preparedBodyPacketWithExactConsumer
                                                )
                                            , isNothing
                                                ( subtermGeneralizationOpaqueResultConstruction
                                                    preparedBodyPacketWithExactConsumer
                                                )
                                            , isNothing
                                                ( subtermGeneralizationGammaAuthority
                                                    preparedBodyPacketWithExactConsumer
                                                ) ->
                                                withSourceOwnerConsumerCompletion
                                                    owner
                                                    ( packetOperatedExpectedType
                                                        expected
                                                    )
                                                    preparedBodyPacketWithExactConsumer
                                        _ ->
                                            pure
                                                preparedBodyPacketWithExactConsumer
                                preparedBodyPacketWithSourceOwnerFinalCompletion <-
                                    case
                                        ( mbConsumerOwner
                                        , mbConsumerIdentity
                                        , sourceOwnerConsumerExpectedType
                                        )
                                      of
                                        ( Just owner
                                          , Just consumerIdentity
                                          , Just expected
                                          )
                                            | lgoConstructor owner
                                                /= LocalLambdaGamma
                                            , consumerIdentity
                                                == typeBinderIdentityFromNode
                                                    (lgoTermNode owner)
                                            , isNothing
                                                ( subtermGeneralizationOpaqueResultConstruction
                                                    preparedBodyPacketWithSourceOwnerCompletion
                                                )
                                            , isNothing
                                                ( subtermGeneralizationGammaAuthority
                                                    preparedBodyPacketWithSourceOwnerCompletion
                                                ) ->
                                                withSourceOwnerFinalConsumerCompletion
                                                    owner
                                                    ( packetOperatedExpectedType
                                                        expected
                                                    )
                                                    preparedBodyPacketWithSourceOwnerCompletion
                                        _ ->
                                            pure
                                                preparedBodyPacketWithSourceOwnerCompletion
                                preparedBodyPacket' <-
                                    case exactResult of
                                        Just
                                            ( SourceOwnedCompilerExactPacketResult
                                                exactEdge
                                                packetResultRef
                                                sourceResultRef
                                              ) ->
                                                withCompilerExactSourceSubtermResult
                                                    exactEdge
                                                    packetResultRef
                                                    sourceResultRef
                                                    preparedBodyPacketWithSourceOwnerFinalCompletion
                                        Just
                                            ( PacketOwnedCompilerExactPacketResult
                                                exactEdge
                                                packetResultRef
                                              ) ->
                                                case mbConsumerIdentity of
                                                    Just _ ->
                                                        withCompilerExactEnclosingSubtermResult
                                                            exactEdge
                                                            packetResultRef
                                                            preparedBodyPacketWithSourceOwnerFinalCompletion
                                                    Nothing ->
                                                        withCompilerExactPacketSubtermResult
                                                            exactEdge
                                                            packetResultRef
                                                            preparedBodyPacketWithSourceOwnerFinalCompletion
                                        Just
                                            ( DescendantOwnedCompilerExactPacketResult
                                                exactEdge
                                                packetResultRef
                                              ) ->
                                                withCompilerExactDescendantSubtermResult
                                                    exactEdge
                                                    packetResultRef
                                                    preparedBodyPacketWithSourceOwnerFinalCompletion
                                        Nothing ->
                                            pure
                                                preparedBodyPacketWithSourceOwnerFinalCompletion
                                packets <-
                                    mergeSubtermGeneralizations
                                        ( Map.singleton
                                            ownerKey
                                            preparedBodyPacket'
                                        )
                                        (Map.delete ownerKey descendants)
                                pure (packets, generator')
                case
                    ( administrativeLambdaBody sourceDetails sourceBody
                    , administrativeLambdaBody canonDetails canonBody
                    )
                  of
                    (Just sourceNestedLambda, Just canonNestedLambda) -> do
                        mbConsumer <-
                            consumerForEdge
                                sourceScopeRoot
                                (annNode sourceBody)
                                canonScopeRoot
                                (annNode canonBody)
                                sourceBodyEdge
                        consumerOwner <-
                            case mbConsumer of
                                Nothing -> pure (Just enclosingConsumerOwner)
                                Just consumer ->
                                    consumerOwnerForEdge
                                        localGammaClosures
                                        enclosingConsumerOwner
                                        consumer
                        ( sourceNestedScope
                          , sourceNestedBody
                          , sourceNestedBodyEdge
                          , nestedParameterDetails
                          , nestedOwnerKey
                          , nestedParamNode
                          , nestedScope
                          , nestedBody
                          , nestedBodyEdge
                          , nestedLambdaNode
                          ) <-
                            case (sourceNestedLambda, canonNestedLambda) of
                                ( ALam _ _ _ sourceScope innerSourceBody sourceEdge _
                                  , ALam _ nestedDetails nestedParam canonScope innerCanonBody canonEdge canonNode
                                  )
                                    | sourceEdge == canonEdge ->
                                        pure
                                            ( sourceScope
                                            , innerSourceBody
                                            , sourceEdge
                                            , nestedDetails
                                            , idDetailsIdentityKey nestedDetails
                                            , nestedParam
                                            , canonScope
                                            , innerCanonBody
                                            , canonEdge
                                            , canonNode
                                            )
                                    | otherwise ->
                                        Left
                                            ( ValidationFailed
                                                [ "administrative lambda body edge changed during preparation"
                                                , "  source edge: " ++ show sourceEdge
                                                , "  canonical edge: " ++ show canonEdge
                                                ]
                                            )
                                _ ->
                                    Left
                                        (ValidationFailed
                                            [ "administrative lambda body did not retain its lambda owner"
                                            , "  source body: " ++ show sourceNestedLambda
                                            , "  canonical body: " ++ show canonNestedLambda
                                            ])
                        let nestedOwner =
                                LocalGammaOwner
                                    { lgoConstructor = LocalLambdaGamma
                                    , lgoBoundaryEdge = nestedBodyEdge
                                    , lgoTermNode = nestedLambdaNode
                                    , lgoScope = GenRef nestedScope
                                    }
                        nestedBodyConsumer <-
                            consumerForEdge
                                sourceNestedScope
                                (annNode sourceNestedBody)
                                nestedScope
                                (annNode nestedBody)
                                sourceNestedBodyEdge
                        nestedBodyConsumerOwner <-
                            case nestedBodyConsumer of
                                Nothing -> pure (Just nestedOwner)
                                Just consumer ->
                                    consumerOwnerForEdge
                                        localGammaClosures
                                        nestedOwner
                                        consumer
                        let directNestedGammaAuthority = do
                                consumer <- nestedBodyConsumer
                                guard
                                    ( preparedLambdaBodyConsumerRequiresGamma
                                        consumer
                                    )
                                guard
                                    (nestedBodyConsumerOwner == Just nestedOwner)
                                pure
                                    ( GammaPacketAuthority
                                        (preparedLambdaBodyConsumerEdge consumer)
                                        sourceNestedScope
                                        (preparedLambdaBodyConsumerIdentity consumer)
                                    )
                            inheritedNestedGammaAuthority =
                                case Map.lookup nestedOwnerKey descendants of
                                    Just packet
                                        | subtermGeneralizationOwnsGammaForEdge nestedBodyEdge packet ->
                                            subtermGeneralizationGammaAuthority packet
                                    _ -> Nothing
                        directNestedTopologyAuthority <-
                            case
                                ( nestedBodyConsumer
                                , nestedBodyConsumerOwner
                                )
                              of
                                ( Just
                                    ( PreparedIdentityTopologyBodyConsumer
                                        edgeId
                                        topologySourceScope
                                        topologySourceBody
                                        boundaryScope
                                        boundaryBody
                                        frozenResult
                                      )
                                  , Just topologyOwner
                                  )
                                    | topologyOwner == nestedOwner ->
                                        Just
                                            <$> mkIdentityTopologyConsumerAuthority
                                                (gaRestoredSchemeRootTargets bindParentsGa)
                                                edgeId
                                                topologySourceScope
                                                topologySourceBody
                                                boundaryScope
                                                boundaryBody
                                                frozenResult
                                                topologyOwner
                                _ -> pure Nothing

                        packetGammaAuthority <-
                            case
                                ( directNestedGammaAuthority
                                , inheritedNestedGammaAuthority
                                )
                              of
                                (Just direct, Just inherited)
                                    | direct == inherited -> pure (Just direct)
                                    | otherwise ->
                                        Left
                                            ( ValidationFailed
                                                [ "administrative lambda has conflicting direct and inherited Gamma authority"
                                                , "  owner: " ++ show nestedOwner
                                                , "  direct: " ++ show direct
                                                , "  inherited: " ++ show inherited
                                                ]
                                            )
                                (Just direct, Nothing) -> pure (Just direct)
                                (Nothing, Just inherited) -> pure (Just inherited)
                                (Nothing, Nothing) -> pure Nothing
                        requiredLambdaParameter <-
                            prepareRequiredLambdaParameter
                                nestedParameterDetails
                                nestedParamNode
                                nestedLambdaNode
                                sourceNestedBody
                                expectedBodyType
                        nestedExpectedBodyType <-
                            administrativeLambdaBodyExpectedType
                                sourceNestedBody
                                expectedBodyType
                        nestedLambdaSourceBinderRefs <-
                            expectedLambdaSpineSourceBinderRefs
                                nestedExpectedBodyType
                                sourceNestedBody
                                nestedBody
                        let nestedLambdaConstructionBinderRefs =
                                expandPreparedSourceBinderRefs
                                    bindParentsGa
                                    constructionCanonical
                                    nestedLambdaSourceBinderRefs
                        nestedConstructionBinderRefs <-
                            sourceResultConstructionBinderRefs
                                sourceBody
                        nestedSourceBinderRefs0 <-
                            mergeCompilerExactConstructionBinderRefs
                                lambdaBodySourceBinderRefs
                                nestedConstructionBinderRefs
                        nestedSourceBinderRefs <-
                            mergeCompilerExactConstructionBinderRefs
                                nestedSourceBinderRefs0
                                nestedLambdaConstructionBinderRefs
                        prepareBodyPacket
                            nestedOwnerKey
                            nestedOwner
                            nestedSourceBinderRefs
                            ( compilerExactLambdaParameterDeclarationIdentities
                                expectedBodyType
                            )
                            (Just requiredLambdaParameter)
                            (Just canonParam)
                            mbConsumer
                            consumerOwner
                            packetGammaAuthority
                            packetGammaAuthority
                            directNestedTopologyAuthority
                            nestedExpectedBodyType
                            sourceNestedLambda
                            canonNestedLambda
                    (Nothing, Nothing) -> do
                        mbConsumer <-
                            consumerForEdge
                                sourceScopeRoot
                                (annNode sourceBody)
                                canonScopeRoot
                                (annNode canonBody)
                                sourceBodyEdge
                        case mbConsumer of
                            Nothing -> pure (descendants, generatorAfterDescendants)
                            Just consumer -> do
                                consumerOwner <-
                                    consumerOwnerForEdge
                                        localGammaClosures
                                        enclosingConsumerOwner
                                        consumer
                                let consumerIdentity =
                                        preparedLambdaBodyConsumerIdentity consumer
                                    consumerEdge =
                                        preparedLambdaBodyConsumerEdge consumer
                                    mbGammaAuthority =
                                        if preparedLambdaBodyConsumerRequiresGamma consumer
                                            && consumerOwner == Just enclosingConsumerOwner
                                            then
                                                Just
                                                    ( GammaPacketAuthority
                                                        consumerEdge
                                                        sourceScopeRoot
                                                        consumerIdentity
                                                    )
                                            else Nothing
                                prepareBodyPacket
                                    (idDetailsIdentityKey canonDetails)
                                    enclosingConsumerOwner
                                    lambdaBodySourceBinderRefs
                                    ( compilerExactLambdaParameterDeclarationIdentities
                                        expectedType
                                    )
                                    Nothing
                                    Nothing
                                    (Just consumer)
                                    consumerOwner
                                    mbGammaAuthority
                                    mbGammaAuthority
                                    Nothing
                                    expectedBodyType
                                    sourceBody
                                    canonBody
                    _ ->
                        Left
                            ( ValidationFailed
                                [ "prepared administrative lambda-body shape changed after validation"
                                , "  source body: " ++ show sourceBody
                                , "  canonical body: " ++ show canonBody
                                ]
                            )
            (AApp sourceFun sourceArg _ _ _, AApp canonFun canonArg _ _ _) -> do
                (funPackets, generatorAfterFun) <-
                    collect localGammaClosures localSourceBinderRefs boundOverlays Nothing generator sourceFun canonFun
                (argPackets, generatorAfterArg) <-
                    collect localGammaClosures localSourceBinderRefs boundOverlays Nothing generatorAfterFun sourceArg canonArg
                packets <- mergeSubtermGeneralizations funPackets argPackets
                pure (packets, generatorAfterArg)
            (ALet _ _ _ _ _ _ sourceRhs sourceBody _, ALet _ _ _ _ _ _ canonRhs canonBody _) -> do
                (rhsPackets, generatorAfterRhs) <-
                    collect localGammaClosures localSourceBinderRefs boundOverlays Nothing generator sourceRhs canonRhs
                (bodyPackets, generatorAfterBody) <-
                    collect localGammaClosures localSourceBinderRefs boundOverlays expectedType generatorAfterRhs sourceBody canonBody
                packets <- mergeSubtermGeneralizations rhsPackets bodyPackets
                pure (packets, generatorAfterBody)
            (AExactAnn sourceInner sourceExactType _ sourceEdge, AExactAnn canonInner canonExactType _ canonEdge)
                | sourceEdge == canonEdge
                , sourceExactType == canonExactType -> do
                    exactType <-
                        case IntMap.lookup (getEdgeId sourceEdge) exactProducerTypes of
                            Just ty -> pure ty
                            Nothing ->
                                Left
                                    ( ValidationFailed
                                        [ "compiler exact subterm has no prepared contract"
                                        , "  edge: " ++ show sourceEdge
                                        ]
                                    )
                    exactBinderRefs <-
                        case IntMap.lookup (getEdgeId sourceEdge) compilerExactConstructionRefs of
                            Just refs -> pure refs
                            Nothing ->
                                Left
                                    ( ValidationFailed
                                        [ "compiler exact subterm has no prepared construction route"
                                        , "  edge: " ++ show sourceEdge
                                        ]
                                    )
                    let exactBoundOverlays =
                            IntMap.union
                                (edgeBoundOverlays sourceEdge)
                                boundOverlays
                    let localSourceBinderRefs' =
                            enterCompilerExactConstructionBinderRefs
                                localSourceBinderRefs
                                exactBinderRefs
                    collect
                        localGammaClosures
                        localSourceBinderRefs'
                        exactBoundOverlays
                        (Just (CompilerExactExpectedType sourceEdge exactType []))
                        generator
                        sourceInner
                        canonInner
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "prepared exact-annotation authority changed after validation"
                            , "  source edge: " ++ show sourceEdge
                            , "  canonical edge: " ++ show canonEdge
                            , "  source type: " ++ show sourceExactType
                            , "  canonical type: " ++ show canonExactType
                            ]
                        )
            (AAnn sourceInner sourceAnnNode _, AAnn canonInner _ _) -> do
                sourceExpectedType <-
                    case IntMap.lookup (getNodeId sourceAnnNode) annExpectedTypes of
                        Just ty -> pure ty
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "missing identity-bearing source annotation type during packet preparation"
                                    , "  annotation node: " ++ show sourceAnnNode
                                            , "  available annotation nodes: " ++ show (map NodeId (IntMap.keys annExpectedTypes))
                                            ]
                                        )
                annotationConstructionBinderRefs <-
                    sourceAnnotationConstructionBinderRefs
                        sourceAnnNode
                        sourceExpectedType
                annotationSourceBinderRefs <-
                    mergeCompilerExactConstructionBinderRefs
                        localSourceBinderRefs
                        annotationConstructionBinderRefs
                collect
                    localGammaClosures
                    annotationSourceBinderRefs
                    boundOverlays
                    (Just (sourcePacketExpectedType sourceExpectedType))
                    generator
                    sourceInner
                    canonInner
            (ALetScope sourceInner _ _, ALetScope canonInner _ _) ->
                collect localGammaClosures localSourceBinderRefs boundOverlays expectedType generator sourceInner canonInner
            (AUnfold sourceInner _ _, AUnfold canonInner _ _) ->
                collect localGammaClosures localSourceBinderRefs boundOverlays Nothing generator sourceInner canonInner
            (AResolvedVar {}, AResolvedVar {}) -> pure (Map.empty, generator)
            (ALit {}, ALit {}) -> pure (Map.empty, generator)
            _ ->
                Left
                    ( ValidationFailed
                        [ "prepared subterm annotation shape changed after validation"
                        , "  source: " ++ show source
                        , "  canonical: " ++ show canon
                        ]
                    )

    -- A topology consumer is prepared before its child term is checked.  It
    -- may nevertheless have an exact result endpoint when the authoritative
    -- source path itself proves that endpoint: lets and let scopes preserve
    -- their body result, while applying a syntactic lambda returns the result
    -- of that exact lambda body.  The path must terminate at an edge-owned
    -- source annotation (or compiler-exact annotation); graph reification is
    -- deliberately not a fallback here because solving may already have
    -- identified the child result with the pending topology consumer.
    sourceConstructionResultType
        :: AnnExpr
        -> Either ElabError (Maybe ElabType)
    sourceConstructionResultType ann =
        case ann of
            AAnn _ sourceAnnNode _ ->
                pure
                    ( IntMap.lookup
                        (getNodeId sourceAnnNode)
                        annExpectedTypes
                    )
            AExactAnn _ _ _ sourceEdge ->
                pure
                    ( IntMap.lookup
                        (getEdgeId sourceEdge)
                        exactProducerTypes
                    )
            ALam _ details parameterNode _ body _ lambdaNode -> do
                mbBodyType <- sourceConstructionResultType body
                case mbBodyType of
                    Nothing -> pure Nothing
                    Just bodyType -> do
                        requiredParameter <-
                            prepareRequiredLambdaParameter
                                details
                                parameterNode
                                lambdaNode
                                body
                                Nothing
                        pure
                            ( Just
                                ( sourceLambdaConstructionType
                                    requiredParameter
                                    bodyType
                                )
                            )
            ALet _ _ _ _ _ _ _ body _ ->
                sourceConstructionResultType body
            ALetScope inner _ _ ->
                sourceConstructionResultType inner
            AApp fun _ _ _ _ ->
                directlyAppliedLambdaResultType fun
            _ -> pure Nothing

    -- Recover a complete source result while reusing administrative packets
    -- that have already been constructed bottom-up.  The source tree owns
    -- annotation identities and lambda syntax; the canonical tree owns the
    -- packet key and exact graph nodes.  Walking them in lockstep therefore
    -- restores every crossed Var-Abs layer without guessing from a reified
    -- arrow or treating an opaque descendant as an enclosing-owned packet.
    sourceConstructionResultTypeWithPackets
        :: SubtermGeneralizations
        -> AnnExpr
        -> AnnExpr
        -> Either ElabError (Maybe SourceConstructionResult)
    sourceConstructionResultTypeWithPackets packets =
        sourceConstructionResultTypeWithPacketsFrom
            PreserveReturnedBindings
            packets
            Map.empty

    sourceOwnerBodyConstructionResultTypeWithPackets
        :: SubtermGeneralizations
        -> AnnExpr
        -> AnnExpr
        -> Either ElabError (Maybe SourceConstructionResult)
    sourceOwnerBodyConstructionResultTypeWithPackets packets =
        sourceConstructionResultTypeWithPacketsFrom
            ResolveOwnerBodyReturnedBindings
            packets
            Map.empty

    sourceConstructionResultTypeWithPacketsFrom
        :: ReturnedBindingResolution
        -> SubtermGeneralizations
        -> Map.Map ResolvedTermIdentityKey ElabType
        -> AnnExpr
        -> AnnExpr
        -> Either ElabError (Maybe SourceConstructionResult)
    sourceConstructionResultTypeWithPacketsFrom returnedBindingResolution packets lexicalTypes source canon =
        case (source, canon) of
            ( AAnn _ sourceAnnNode _
              , AAnn {}
              ) ->
                pure
                    ( fmap
                        ( \ty ->
                            SourceConstructionResult
                                ty
                                DirectSourceConstruction
                                []
                        )
                        ( IntMap.lookup
                            (getNodeId sourceAnnNode)
                            annExpectedTypes
                        )
                    )
            ( AExactAnn _ _ _ sourceEdge
              , AExactAnn {}
              ) ->
                pure
                    ( fmap
                        ( \ty ->
                            SourceConstructionResult
                                ty
                                DirectSourceConstruction
                                []
                        )
                        ( IntMap.lookup
                            (getEdgeId sourceEdge)
                            exactProducerTypes
                        )
                    )
            ( ALam _ sourceDetails _ _ sourceBody _ _
              , ALam _ canonDetails canonParam _ canonBody _ canonLambdaNode
              ) -> do
                let mbPacket =
                        Map.lookup
                            (idDetailsIdentityKey canonDetails)
                            packets
                requiredParameter <-
                    prepareRequiredLambdaParameter
                        sourceDetails
                        canonParam
                        canonLambdaNode
                        sourceBody
                        Nothing
                mbBodyConstruction <-
                    sourceConstructionResultTypeWithPacketsFrom
                        returnedBindingResolution
                        packets
                        ( Map.delete
                            (idDetailsIdentityKey canonDetails)
                            lexicalTypes
                        )
                        sourceBody
                        canonBody
                case mbPacket >>= subtermGeneralizationSourceLambdaResultConstruction of
                    Just (certifiedLambdaNode, constructedType)
                        | certifiedLambdaNode == canonLambdaNode ->
                            pure
                                ( Just
                                    ( case mbBodyConstruction of
                                        Just bodyConstruction
                                            | returnedBindingResolution
                                                == ResolveOwnerBodyReturnedBindings ->
                                            sourceLambdaConstructionFromBody
                                                requiredParameter
                                                bodyConstruction
                                        _ ->
                                            SourceConstructionResult
                                                constructedType
                                                DirectSourceConstruction
                                                [requiredParameter]
                                    )
                                )
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "source lambda packet names a different construction node"
                                    , "  source identity: "
                                        ++ show
                                            ( idDetailsIdentityKey
                                                sourceDetails
                                            )
                                    , "  canonical identity: "
                                        ++ show
                                            ( idDetailsIdentityKey
                                                canonDetails
                                            )
                                    , "  source lambda node: "
                                        ++ show canonLambdaNode
                                    , "  certified lambda node: "
                                        ++ show certifiedLambdaNode
                                    ]
                                )
                    Nothing ->
                        pure
                            ( sourceLambdaConstructionFromBody
                                requiredParameter
                                <$> mbBodyConstruction
                            )
            ( ALet _ sourceDetails _ _ _ _ sourceRhs sourceBody _
              , ALet _ canonDetails _ _ _ _ canonRhs canonBody _
              )
                | idDetailsIdentityKey sourceDetails
                    == idDetailsIdentityKey canonDetails -> do
                    mbRhsType <-
                        case (sourceRhs, canonRhs) of
                            (ALam {}, ALam {}) ->
                                lambdaValueConstructionTypeWithPackets
                                    returnedBindingResolution
                                    packets
                                    lexicalTypes
                                    sourceRhs
                                    canonRhs
                            _
                                | returnedBindingResolution
                                    == ResolveOwnerBodyReturnedBindings ->
                                    sourceConstructionResultTypeWithPacketsFrom
                                        returnedBindingResolution
                                        packets
                                        lexicalTypes
                                        sourceRhs
                                        canonRhs
                            _ -> pure Nothing
                    let bodyTypes =
                            maybe
                                lexicalTypes
                                ( \rhsConstruction ->
                                    Map.insert
                                        (idDetailsIdentityKey canonDetails)
                                        (scrType rhsConstruction)
                                        lexicalTypes
                                )
                                mbRhsType
                    mbBodyConstruction <-
                        sourceConstructionResultTypeWithPacketsFrom
                            returnedBindingResolution
                            packets
                            bodyTypes
                            sourceBody
                            canonBody
                    case
                        ( returnedBindingResolution
                        , mbRhsType
                        , mbBodyConstruction
                        )
                      of
                        ( ResolveOwnerBodyReturnedBindings
                          , Just rhsConstruction
                          , Just
                                bodyConstruction@SourceConstructionResult
                                    { scrOrigin =
                                        ExactReturnedBindingConstruction returnedKey
                                    }
                          )
                            | returnedKey
                                == idDetailsIdentityKey canonDetails ->
                                if alphaEqType
                                    (scrType rhsConstruction)
                                    (scrType bodyConstruction)
                                    || churchAwareEqType
                                        (scrType rhsConstruction)
                                        (scrType bodyConstruction)
                                  then pure (Just rhsConstruction)
                                  else
                                    Left
                                        ( ValidationFailed
                                            [ "owner-body let return disagrees with its source RHS construction"
                                            , "  binding: "
                                                ++ show returnedKey
                                            , "  RHS construction: "
                                                ++ show (scrType rhsConstruction)
                                            , "  returned construction: "
                                                ++ show (scrType bodyConstruction)
                                            ]
                                        )
                        _ -> pure mbBodyConstruction
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "source construction let identity changed during canonicalization"
                            , "  source identity: "
                                ++ show (idDetailsIdentityKey sourceDetails)
                            , "  canonical identity: "
                                ++ show (idDetailsIdentityKey canonDetails)
                            ]
                        )
            ( ALetScope sourceInner _ _
              , ALetScope canonInner _ _
              ) ->
                sourceConstructionResultTypeWithPacketsFrom
                    returnedBindingResolution
                    packets
                    lexicalTypes
                    sourceInner
                    canonInner
            ( AApp sourceFun sourceArg _ _ _
              , AApp canonFun canonArg canonFunSite _ canonResultNode
              ) -> do
                mbResult <-
                    directlyAppliedLambdaResultTypeWithPackets
                        returnedBindingResolution
                        packets
                        lexicalTypes
                        sourceFun
                        canonFun
                        sourceArg
                        canonArg
                -- Application consumes the function value.  A result learned
                -- only by following an exact returned binding is therefore
                -- no longer source authority for the application's codomain;
                -- that endpoint belongs to the application construction.
                case mbResult of
                    Just result
                        | scrOrigin result == DirectSourceConstruction ->
                            pure (Just result)
                    _ ->
                        exactApplicationResultConstruction
                            canonFunSite
                            canonResultNode
            ( AResolvedVar sourceDetails _ _
              , AResolvedVar canonDetails _ _
              )
                | idDetailsIdentityKey sourceDetails
                    == idDetailsIdentityKey canonDetails ->
                    pure
                        ( fmap
                            ( \ty ->
                                SourceConstructionResult
                                    ty
                                    ( ExactReturnedBindingConstruction
                                        (idDetailsIdentityKey canonDetails)
                                    )
                                    []
                            )
                            ( Map.lookup
                                (idDetailsIdentityKey canonDetails)
                                lexicalTypes
                            )
                        )
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "source construction variable identity changed during canonicalization"
                            , "  source identity: "
                                ++ show (idDetailsIdentityKey sourceDetails)
                            , "  canonical identity: "
                                ++ show (idDetailsIdentityKey canonDetails)
                            ]
                        )
            (ALit {}, ALit {}) -> pure Nothing
            ( AUnfold sourceInner _ _
              , AUnfold canonInner _ _
              ) ->
                sourceConstructionResultTypeWithPacketsFrom
                    returnedBindingResolution
                    packets
                    lexicalTypes
                    sourceInner
                    canonInner
            _ ->
                Left
                    ( ValidationFailed
                        [ "source construction result traversal changed shape"
                        , "  source: " ++ show source
                        , "  canonical: " ++ show canon
                        ]
                    )
      where
        -- The packet summary may have floated the body's declarations across
        -- the arrow while preparing an enclosing Gamma.  When the paired
        -- source/canonical walk can construct the body itself, the source
        -- Var-Abs layer is the stronger lexical authority: rebuild that layer
        -- around the body so a returned forall remains beneath the arrow.
        -- Reuse the packet summary only when no body construction is
        -- available.
        sourceLambdaConstructionFromBody required bodyConstruction =
            SourceConstructionResult
                ( sourceLambdaConstructionType
                    required
                    (scrType bodyConstruction)
                )
                ( encloseSourceConstructionOrigin
                    (scrOrigin bodyConstruction)
                )
                ( required
                    : scrReturnedLambdaParameters bodyConstruction
                )

    -- The canonical AApp constructor records its exact codomain node before
    -- solving.  When source traversal cannot reduce the function syntax (for
    -- example, a let-bound annotated function), recover only a complete,
    -- closed type from that occurrence's own codomain/direct-bound chain.
    -- This is a no-fallback construction query: an open carrier or unrelated
    -- contextual alias remains unavailable rather than being guessed from
    -- the final root type.
    exactApplicationResultConstruction
        :: InstantiationSite
        -> NodeId
        -> Either ElabError (Maybe SourceConstructionResult)
    exactApplicationResultConstruction funSite resultNode =
        case instantiationSiteTargetTopology funSite of
            ArrowInstantiationTarget{instantiationArrowCodomain = codomain} -> do
                    view <- resultTypeView
                    let codomainC = constructionCanonical codomain
                        resultNodeC = constructionCanonical resultNode
                        candidateNodes =
                            distinctNodes
                                ( [ codomainC
                                  , resultNodeC
                                  , View.rtvSchemeBodyTarget view codomainC
                                  , View.rtvSchemeBodyTarget view resultNodeC
                                  ]
                                    ++ maybeToList
                                        (View.rtvDirectBoundTarget view codomainC)
                                    ++ maybeToList
                                        (View.rtvDirectBoundTarget view resultNodeC)
                                )
                        closedTypes =
                            [ ty
                            | node <- candidateNodes
                            , Right ty <- [View.rtvReifyNoFallback view node]
                            , ty /= TBottom
                            , null (freeTypeVarRefsType ty)
                            ]
                    case closedTypes of
                        [] -> pure Nothing
                        firstType : remainingTypes
                            | all (constructionTypesAgree firstType) remainingTypes ->
                                pure
                                    ( Just
                                        ( SourceConstructionResult
                                            firstType
                                            DirectSourceConstruction
                                            []
                                        )
                                    )
                            | otherwise ->
                                Left
                                    ( ValidationFailed
                                        [ "exact application codomain has conflicting closed constructions"
                                        , "  application edge: "
                                            ++ show
                                                (instantiationSiteEdgeId funSite)
                                        , "  result node: " ++ show resultNodeC
                                        , "  candidate nodes: "
                                            ++ show candidateNodes
                                        , "  candidate types: "
                                            ++ show closedTypes
                                        ]
                                    )
            _ -> pure Nothing
      where
        distinctNodes = foldr insertNode []

        insertNode node nodes
            | node `elem` nodes = nodes
            | otherwise = node : nodes

        constructionTypesAgree left right =
            alphaEqType left right || churchAwareEqType left right

    -- A context that preserves one exact source lambda value supplies a
    -- stronger construction boundary than an arbitrary inferred endpoint.
    -- A returned let connects the RHS lambda to its resolved occurrence; a
    -- direct syntactic application connects the argument lambda to the
    -- function parameter.  In either case the ordinary lambda-body packet may
    -- be paired with the source parameter to reconstruct the complete Var-Abs
    -- result without consulting the pending graph result.
    lambdaValueConstructionTypeWithPackets
        :: ReturnedBindingResolution
        -> SubtermGeneralizations
        -> Map.Map ResolvedTermIdentityKey ElabType
        -> AnnExpr
        -> AnnExpr
        -> Either ElabError (Maybe SourceConstructionResult)
    lambdaValueConstructionTypeWithPackets returnedBindingResolution packets lexicalTypes source canon =
        case (source, canon) of
            ( ALam _ sourceDetails _ _ sourceBody _ _
              , ALam _ canonDetails canonParam _ _ _ canonLambdaNode
              )
                | idDetailsIdentityKey sourceDetails
                    == idDetailsIdentityKey canonDetails
                , Just packet <-
                    Map.lookup
                        (idDetailsIdentityKey canonDetails)
                        packets
                , Nothing <-
                    subtermGeneralizationSourceLambdaResultConstruction
                        packet
                , alphaEqType
                    ( schemeToType
                        ( siScheme
                            (subtermGeneralizationSchemeInfo packet)
                        )
                    )
                    ( schemeToType
                        (subtermGeneralizationGammaBoundScheme packet)
                    ) -> do
                    requiredParameter <-
                        prepareRequiredLambdaParameter
                            sourceDetails
                            canonParam
                            canonLambdaNode
                            sourceBody
                            Nothing
                    constructedType <-
                        sourceLambdaValueConstructionTypeFromPacket
                            requiredParameter
                            packet
                    pure
                        ( Just
                            ( SourceConstructionResult
                                constructedType
                                DirectSourceConstruction
                                [requiredParameter]
                            )
                        )
            _ ->
                sourceConstructionResultTypeWithPacketsFrom
                    returnedBindingResolution
                    packets
                    lexicalTypes
                    source
                    canon

    directlyAppliedLambdaResultTypeWithPackets
        returnedBindingResolution
        packets
        lexicalTypes
        sourceFun
        canonFun
        sourceArg
        canonArg =
        case (sourceFun, canonFun) of
            ( ALam _ sourceDetails _ _ sourceBody _ _
              , ALam _ canonDetails _ _ canonBody _ _
              )
                | sourceBinderKey == canonBinderKey -> do
                    mbArgumentConstruction <-
                        case (sourceArg, canonArg) of
                            ( ALam _ argumentDetails _ _ argumentBody _ _
                              , ALam {}
                              )
                                | isJust
                                    ( desugaredAnnLambdaInfo
                                        argumentDetails
                                        argumentBody
                                    )
                                    || annExprReferenceKey sourceBody
                                        == Just (annBinderKey sourceDetails) ->
                                lambdaValueConstructionTypeWithPackets
                                    returnedBindingResolution
                                    packets
                                    lexicalTypes
                                    sourceArg
                                    canonArg
                            (ALam {}, ALam {}) -> pure Nothing
                            _ ->
                                sourceConstructionResultTypeWithPacketsFrom
                                    returnedBindingResolution
                                    packets
                                    lexicalTypes
                                    sourceArg
                                    canonArg
                    let bodyLexicalTypes =
                            maybe
                                lexicalTypes
                                ( \argumentConstruction ->
                                    Map.insert
                                        canonBinderKey
                                        (scrType argumentConstruction)
                                        lexicalTypes
                                )
                                mbArgumentConstruction
                    mbBodyConstruction <-
                        sourceConstructionResultTypeWithPacketsFrom
                            returnedBindingResolution
                            packets
                            bodyLexicalTypes
                            sourceBody
                            canonBody
                    pure
                        ( fmap
                            ( \bodyConstruction ->
                                case scrOrigin bodyConstruction of
                                    ExactReturnedBindingConstruction returnedKey
                                        | returnedKey == canonBinderKey ->
                                            bodyConstruction
                                                { scrOrigin =
                                                    DirectSourceConstruction
                                                }
                                    _ -> bodyConstruction
                            )
                            mbBodyConstruction
                        )
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "directly applied lambda identity changed during canonicalization"
                            , "  source identity: " ++ show sourceBinderKey
                            , "  canonical identity: " ++ show canonBinderKey
                            ]
                        )
              where
                sourceBinderKey = idDetailsIdentityKey sourceDetails
                canonBinderKey = idDetailsIdentityKey canonDetails
            ( ALetScope sourceInner _ _
              , ALetScope canonInner _ _
              ) ->
                directlyAppliedLambdaResultTypeWithPackets
                    returnedBindingResolution
                    packets
                    lexicalTypes
                    sourceInner
                    canonInner
                    sourceArg
                    canonArg
            _ -> pure Nothing

    -- Recover the complete source construction named by a prepared local
    -- Gamma owner.  The owner is recorded in the canonical tree, while source
    -- annotations retain the identity-bearing endpoint needed to certify the
    -- exact xMLF binder-spine computation.  Traverse the already shape-checked
    -- source/canonical trees in lockstep instead of guessing that endpoint
    -- from a projected packet body.
    sourceConstructionTypeForOwner
        :: SubtermGeneralizations
        -> LocalGammaOwner
        -> Maybe PacketExpectedType
        -> Maybe ElabType
        -> Either ElabError (Maybe ElabType)
    sourceConstructionTypeForOwner packets owner mbExpectedOwnerType mbOperatedType =
        sourceConstructionTypeAtOwner owner ownerConstructionType
      where
        ownerConstructionType sourceOwner canonOwner =
            case (sourceOwner, canonOwner, mbOperatedType) of
                ( ALam _ sourceDetails _ _ sourceBody _ _
                  , ALam _ _ canonParam _ _ _ canonLambdaNode
                  , Just operatedType
                  ) -> do
                    requiredParameter <-
                        prepareRequiredLambdaParameter
                            sourceDetails
                            canonParam
                            canonLambdaNode
                            sourceBody
                            mbExpectedOwnerType
                    pure
                        ( Just
                            ( sourceLambdaConstructionType
                                requiredParameter
                                operatedType
                            )
                        )
                _ ->
                    fmap (fmap scrType)
                        ( sourceConstructionResultTypeWithPacketsFrom
                            ResolveOwnerBodyReturnedBindings
                            packets
                            Map.empty
                            sourceOwner
                            canonOwner
                        )

    -- Recover the source construction of a local lambda owner's body.  This
    -- endpoint differs from the whole owner construction when a returned
    -- forall remains beneath a lambda arrow.  Existing descendant packets
    -- and source identities determine it before recursive elaboration; no
    -- checked final type is used to commute the binder through the arrow.
    sourceBodyConstructionTypeForOwner
        :: SubtermGeneralizations
        -> LocalGammaOwner
        -> Either ElabError (Maybe ElabType)
    sourceBodyConstructionTypeForOwner packets owner =
        sourceConstructionTypeAtOwner owner ownerBodyConstructionType
      where
        ownerBodyConstructionType sourceOwner canonOwner =
            case (sourceOwner, canonOwner) of
                ( ALam _ _ _ _ sourceBody _ _
                  , ALam _ _ _ _ canonBody _ _
                  ) -> do
                    mbConstruction <-
                        sourceOwnerBodyConstructionResultTypeWithPackets
                            packets
                            sourceBody
                            canonBody
                    case mbConstruction of
                        Nothing -> pure Nothing
                        Just construction ->
                            case openedReturnedLambdaEndpoint construction of
                                Just endpoint -> pure (Just endpoint)
                                Nothing
                                    | required : _ <-
                                        scrReturnedLambdaParameters construction
                                    , isJust
                                        (rlpStructuredParameterType required) ->
                                        -- A structured source parameter owns
                                        -- only its value arrow; there is no
                                        -- Var-Abs declaration to open at the
                                        -- enclosing owner boundary.
                                        pure (Just (scrType construction))
                                Nothing
                                    | _ : _ <-
                                        scrReturnedLambdaParameters
                                            construction ->
                                    Left
                                        ( ValidationFailed
                                            [ "source owner body construction cannot open its returned lambda endpoint"
                                            , "  owner: " ++ show owner
                                            , "  construction type: "
                                                ++ show (scrType construction)
                                            , "  construction origin: "
                                                ++ show (scrOrigin construction)
                                            , "  returned lambda parameters: "
                                                ++ show
                                                    ( scrReturnedLambdaParameters
                                                        construction
                                                    )
                                            ]
                                        )
                                Nothing -> pure Nothing
                _ -> pure Nothing

        openedReturnedLambdaEndpoint construction = do
            required : _ <-
                pure (scrReturnedLambdaParameters construction)
            guard (isNothing (rlpStructuredParameterType required))
            let requiredRef = requiredLambdaParameterRef required
            case scrType construction of
                TForallRef binderRef Nothing _
                    | typeBinderRefsSameIdentity binderRef requiredRef -> do
                        opened <-
                            either
                                (const Nothing)
                                Just
                                ( applyInstantiation
                                    (scrType construction)
                                    (InstApp (TVarRef requiredRef))
                                )
                        let (_, openedBody) = splitForallsRefs opened
                        case openedBody of
                            TArrow (TVarRef domainRef) _
                                | typeBinderRefsSameIdentity
                                    domainRef
                                    requiredRef ->
                                    Just opened
                            _ -> Nothing
                _ -> Nothing

    sourceConstructionTypeAtOwner
        :: LocalGammaOwner
        -> (AnnExpr -> AnnExpr -> Either ElabError (Maybe ElabType))
        -> Either ElabError (Maybe ElabType)
    sourceConstructionTypeAtOwner owner ownerConstructionType = do
        rootPairs <- pairSubtermGeneralizationRoots sources canons
        matches <-
            concat
                <$> traverse
                    ( uncurry
                        ( collectOwnerConstructionTypes
                            owner
                            ownerConstructionType
                        )
                    )
                    rootPairs
        case matches of
            [] ->
                Left
                    ( ValidationFailed
                        [ "prepared local Gamma owner has no paired source construction"
                        , "  owner: " ++ show owner
                        , "  available canonical owners: "
                            ++ show (concatMap localOwnerKeysIn canons)
                        ]
                    )
            [ownerType] -> pure ownerType
            ownerTypes ->
                Left
                    ( ValidationFailed
                        [ "one local Gamma owner names multiple source constructions"
                        , "  owner: " ++ show owner
                        , "  source constructions: " ++ show ownerTypes
                        ]
                    )

    collectOwnerConstructionTypes
        :: LocalGammaOwner
        -> (AnnExpr -> AnnExpr -> Either ElabError (Maybe ElabType))
        -> AnnExpr
        -> AnnExpr
        -> Either ElabError [Maybe ElabType]
    collectOwnerConstructionTypes owner ownerConstructionType source canon
        | localOwnerMatches owner canon =
            (: []) <$> ownerConstructionType source canon
        | otherwise =
            case (source, canon) of
                (ALam _ _ _ _ sourceBody _ _, ALam _ _ _ _ canonBody _ _) ->
                    collectOwnerConstructionTypes
                        owner
                        ownerConstructionType
                        sourceBody
                        canonBody
                (AApp sourceFun sourceArg _ _ _, AApp canonFun canonArg _ _ _) ->
                    (++)
                        <$> collectOwnerConstructionTypes
                            owner
                            ownerConstructionType
                            sourceFun
                            canonFun
                        <*> collectOwnerConstructionTypes
                            owner
                            ownerConstructionType
                            sourceArg
                            canonArg
                (ALet _ _ _ _ _ _ sourceRhs sourceBody _, ALet _ _ _ _ _ _ canonRhs canonBody _) ->
                    (++)
                        <$> collectOwnerConstructionTypes
                            owner
                            ownerConstructionType
                            sourceRhs
                            canonRhs
                        <*> collectOwnerConstructionTypes
                            owner
                            ownerConstructionType
                            sourceBody
                            canonBody
                (AExactAnn sourceInner _ _ _, AExactAnn canonInner _ _ _) ->
                    collectOwnerConstructionTypes
                        owner
                        ownerConstructionType
                        sourceInner
                        canonInner
                (AAnn sourceInner _ _, AAnn canonInner _ _) ->
                    collectOwnerConstructionTypes
                        owner
                        ownerConstructionType
                        sourceInner
                        canonInner
                (ALetScope sourceInner _ _, ALetScope canonInner _ _) ->
                    collectOwnerConstructionTypes
                        owner
                        ownerConstructionType
                        sourceInner
                        canonInner
                (AUnfold sourceInner _ _, AUnfold canonInner _ _) ->
                    collectOwnerConstructionTypes
                        owner
                        ownerConstructionType
                        sourceInner
                        canonInner
                (AResolvedVar {}, AResolvedVar {}) -> pure []
                (ALit {}, ALit {}) -> pure []
                _ ->
                    Left
                        ( ValidationFailed
                            [ "source construction owner traversal changed shape"
                            , "  owner: " ++ show owner
                            , "  source: " ++ show source
                            , "  canonical: " ++ show canon
                            ]
                        )
    localOwnerMatches :: LocalGammaOwner -> AnnExpr -> Bool
    localOwnerMatches owner ann =
        case ann of
            ALam _ _ _ lambdaScope _ bodyEdge lambdaNode ->
                lgoConstructor owner == LocalLambdaGamma
                    && lgoBoundaryEdge owner == bodyEdge
                    && lgoTermNode owner == lambdaNode
                    && lgoScope owner == GenRef lambdaScope
            AApp _ _ funSite _ applicationNode ->
                lgoConstructor owner == LocalApplicationGamma
                    && lgoBoundaryEdge owner
                        == instantiationSiteEdgeId funSite
                    && lgoTermNode owner == applicationNode
            ALet _ _ _ _ _ _ _ body resultNode ->
                lgoConstructor owner == LocalLetGamma
                    && case body of
                        ALetScope _ _ edgeId ->
                            lgoBoundaryEdge owner == edgeId
                                && lgoTermNode owner == resultNode
                        _ -> False
            _ -> False

    localOwnerKeysIn
        :: AnnExpr
        -> [(LocalGammaConstructor, EdgeId, NodeId, Maybe NodeRef)]
    localOwnerKeysIn ann =
        ownerKey
            ++ case ann of
                AResolvedVar {} -> []
                ALit {} -> []
                ALam _ _ _ _ body _ _ ->
                    localOwnerKeysIn body
                AApp fun argument _ _ _ ->
                    localOwnerKeysIn fun ++ localOwnerKeysIn argument
                ALet _ _ _ _ _ _ rhs body _ ->
                    localOwnerKeysIn rhs ++ localOwnerKeysIn body
                AExactAnn inner _ _ _ -> localOwnerKeysIn inner
                AAnn inner _ _ -> localOwnerKeysIn inner
                ALetScope inner _ _ -> localOwnerKeysIn inner
                AUnfold inner _ _ -> localOwnerKeysIn inner
      where
        ownerKey =
            case ann of
                ALam _ _ _ lambdaScope _ bodyEdge lambdaNode ->
                    [ ( LocalLambdaGamma
                      , bodyEdge
                      , lambdaNode
                      , Just (GenRef lambdaScope)
                      )
                    ]
                AApp _ _ funSite _ applicationNode ->
                    [ ( LocalApplicationGamma
                      , instantiationSiteEdgeId funSite
                      , applicationNode
                      , Nothing
                      )
                    ]
                ALet _ _ _ _ _ _ _ body resultNode ->
                    case body of
                        ALetScope _ _ edgeId ->
                            [ ( LocalLetGamma
                              , edgeId
                              , resultNode
                              , Nothing
                              )
                            ]
                        _ -> []
                _ -> []

    -- A source annotation owns both an identity-bearing expected type and the
    -- exact graph root produced by kappa construction.  Reify that occurrence
    -- in the acyclic base graph, align free occurrences first, pair the
    -- remaining alpha-corresponding declarations, and then project the
    -- resulting carrier through the prepared copy map.  This publishes, for
    -- example, both the original annotation binder and a Var-Abs copy under
    -- one source identity before packet generalization.
    sourceAnnotationConstructionBinderRefs
        :: NodeId
        -> ElabType
        -> Either ElabError (IntMap.IntMap TypeBinderRef)
    sourceAnnotationConstructionBinderRefs sourceAnnNode sourceExpectedType = do
        graphType <-
            TypeReify.reifyTypeWithRefsNoFallbackOnConstraint
                baseConstraint
                IntMap.empty
                sourceAnnNode
        case
            alignedFreeTypeOccurrencePairs
                sourceExpectedType
                graphType
          of
            Nothing -> pure IntMap.empty
            Just freeOccurrencePairs -> do
                directRefs <-
                    foldM
                        insertAnnotationDeclarationRoute
                        IntMap.empty
                        ( freeOccurrencePairs
                            ++ pairedTypeDeclarationRefs
                                sourceExpectedType
                                graphType
                        )
                pure
                    ( expandPreparedSourceBinderRefs
                        bindParentsGa
                        constructionCanonical
                        directRefs
                    )

    insertAnnotationDeclarationRoute refs (sourceRef, graphRef) =
        case typeBinderRefNode graphRef of
            Nothing ->
                Left
                    ( ValidationFailed
                        [ "source annotation graph declaration has no node identity"
                        , "  source binder: " ++ show sourceRef
                        , "  graph binder: " ++ show graphRef
                        ]
                    )
            Just graphNode ->
                case IntMap.lookup (getNodeId graphNode) refs of
                    Nothing ->
                        pure
                            ( IntMap.insert
                                (getNodeId graphNode)
                                sourceRef
                                refs
                            )
                    Just existing
                        | typeBinderRefsSameIdentity existing sourceRef ->
                            pure refs
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "source annotation graph declaration maps to multiple source identities"
                                    , "  graph binder: " ++ show graphRef
                                    , "  first source binder: " ++ show existing
                                    , "  second source binder: " ++ show sourceRef
                                    ]
                                )

    -- Follow only constructors whose result is definitionally the selected
    -- child result.  The returned map is therefore occurrence-local
    -- construction authority, never a whole-subtree name lookup.
    sourceResultConstructionBinderRefs
        :: AnnExpr
        -> Either ElabError (IntMap.IntMap TypeBinderRef)
    sourceResultConstructionBinderRefs ann =
        case ann of
            AAnn _ sourceAnnNode _ ->
                case IntMap.lookup (getNodeId sourceAnnNode) annExpectedTypes of
                    Just sourceExpectedType ->
                        sourceAnnotationConstructionBinderRefs
                            sourceAnnNode
                            sourceExpectedType
                    Nothing ->
                        Left
                            ( ValidationFailed
                                [ "source result annotation has no identity-bearing expected type"
                                , "  annotation node: " ++ show sourceAnnNode
                                ]
                            )
            AExactAnn _ _ _ sourceEdge ->
                case
                    IntMap.lookup
                        (getEdgeId sourceEdge)
                        compilerExactConstructionRefs
                of
                    Just refs -> pure refs
                    Nothing ->
                        Left
                            ( ValidationFailed
                                [ "compiler exact result has no prepared construction route"
                                , "  edge: " ++ show sourceEdge
                                ]
                            )
            ALet _ _ _ _ _ _ _ body _ ->
                sourceResultConstructionBinderRefs body
            ALetScope inner _ _ ->
                sourceResultConstructionBinderRefs inner
            ALam _ _ _ _ body _ _ ->
                sourceResultConstructionBinderRefs body
            AApp fun _ _ _ _ ->
                directlyAppliedLambdaResultConstructionBinderRefs fun
            _ -> pure IntMap.empty

    directlyAppliedLambdaResultConstructionBinderRefs fun =
        case fun of
            ALam _ _ _ _ body _ _ ->
                sourceResultConstructionBinderRefs body
            ALetScope inner _ _ ->
                directlyAppliedLambdaResultConstructionBinderRefs inner
            _ -> pure IntMap.empty

    -- An administrative packet generalizes the nested lambda body; its
    -- parameter is restored separately by 'constructPacketBodyScheme'.
    -- Strip the same Var-Abs prefix from an enclosing expectation and compare
    -- it with the exact source construction while both authorities are
    -- present.
    administrativeLambdaBodyExpectedType
        :: AnnExpr
        -> Maybe PacketExpectedType
        -> Either ElabError (Maybe PacketExpectedType)
    administrativeLambdaBodyExpectedType nestedBody mbOuterExpected = do
        mbSourceBodyType <- sourceConstructionResultType nestedBody
        let mbSourceExpected =
                sourcePacketExpectedType <$> mbSourceBodyType
            mbOuterBodyExpected =
                mbOuterExpected >>= packetLambdaBodyType
        case (mbSourceExpected, mbOuterBodyExpected) of
            (Just sourceExpected, Just outerExpected)
                | expectedEndpointsAgree sourceExpected outerExpected ->
                    pure (Just sourceExpected)
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "administrative lambda body source construction disagrees with its enclosing endpoint"
                            , "  source body: "
                                ++ show (packetExpectedType sourceExpected)
                            , "  enclosing body: "
                                ++ show (packetExpectedType outerExpected)
                            ]
                        )
            (Just sourceExpected, Nothing) ->
                pure (Just sourceExpected)
            (Nothing, Just outerExpected) ->
                pure (Just outerExpected)
            (Nothing, Nothing) ->
                pure Nothing

    -- Var-Abs constructs an unbounded parameter together with its forall.
    -- A structured parameter is already constructed by source/compiler
    -- authority and contributes only the arrow domain.
    sourceLambdaConstructionType
        :: RequiredLambdaParameter
        -> ElabType
        -> ElabType
    sourceLambdaConstructionType required bodyType =
        case rlpStructuredParameterType required of
            Just _ ->
                TArrow
                    (requiredLambdaParameterType required)
                    bodyType
            Nothing ->
                let parameterRef = requiredLambdaParameterRef required
                 in TForallRef
                        parameterRef
                        Nothing
                        (TArrow (TVarRef parameterRef) bodyType)

    -- A body application can own a topology result abstraction even when its
    -- complete consumer-facing packet has projected that result out.  When
    -- the exact source lambda is used as a value, reconstruct the declaration
    -- from the packet's pending construction slot and S'(operated), while the
    -- structured source parameter is still available.  This is Section 15.3.8
    -- for @lambda (g : sigma-id). g g@:
    --
    --   forall (result >= sigma-id). sigma-id -> result
    --
    -- In particular, the forall inside @sigma-id@ is lexical to the bound; it
    -- cannot be left free beside the identical source parameter type.
    sourceLambdaValueConstructionTypeFromPacket
        :: RequiredLambdaParameter
        -> PreparedSubtermGeneralization
        -> Either ElabError ElabType
    sourceLambdaValueConstructionTypeFromPacket required packet =
        case rlpStructuredParameterType required of
            Nothing -> pure ordinaryConstruction
            Just parameterType ->
                case pendingTopologyResultDeclaration of
                    Nothing -> pure ordinaryConstruction
                    Just (resultRef, constructionBinders) -> do
                        completedBoundType <-
                            completeTopologyResultBound
                                parameterType
                                rawGammaBoundType
                        completedBound <-
                            case elabToBound completedBoundType of
                                Right bound -> pure bound
                                Left cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "source lambda topology result has an invalid completed bound"
                                            , "  lambda: " ++ show (rlpLambdaNode required)
                                            , "  result: " ++ show resultRef
                                            , "  bound: " ++ show completedBoundType
                                            , "  cause: " ++ cause
                                            ]
                                        )
                        pure
                            ( schemeToType
                                ( mkElabSchemeWithRefs
                                    [ if typeBinderRefsSameIdentity ref resultRef
                                        then (ref, Just completedBound)
                                        else binding
                                    | binding@(ref, _) <- constructionBinders
                                    ]
                                    ( TArrow
                                        parameterType
                                        (TVarRef resultRef)
                                    )
                                )
                            )
      where
        packetSchemeInfo = subtermGeneralizationSchemeInfo packet
        ordinaryConstruction =
            sourceLambdaConstructionType
                required
                (schemeToType (siScheme packetSchemeInfo))
        rawGammaBoundType =
            schemeToType
                (subtermGeneralizationGammaBoundScheme packet)
        completeType = schemeToType (siScheme packetSchemeInfo)
        completeRefs =
            typeBinderDeclarationRefs completeType
                ++ freeTypeVarRefsType completeType
        pendingTopologyResultDeclaration = do
            authority <- subtermGeneralizationConsumerAuthority packet
            guard (subtermConsumerAuthorityIsTopology authority)
            constructionResultRef <-
                subtermGeneralizationConstructionResultAbstractionRef packet
            guard
                ( not
                    ( any
                        (typeBinderRefsSameIdentity constructionResultRef)
                        completeRefs
                    )
                )
            let constructionBinders =
                    schemeBinderRefs
                        ( siScheme
                            ( subtermGeneralizationConsumerConstructionSchemeInfo
                                packet
                            )
                        )
                matching =
                    [ binding
                    | binding@(ref, Nothing) <- constructionBinders
                    , typeBinderRefsSameIdentity ref constructionResultRef
                    ]
            case matching of
                [(resultRef, Nothing)] ->
                    pure (resultRef, constructionBinders)
                _ -> Nothing

        completeTopologyResultBound parameterType rawBound
            | constructionEndpointProvides parameterType rawBound =
                pure parameterType
            | null parameterLexicalLeaks = pure rawBound
            | otherwise =
                Left
                    ( ValidationFailed
                        [ "source lambda topology result leaks a parameter-local binder"
                        , "  lambda: " ++ show (rlpLambdaNode required)
                        , "  parameter type: " ++ show parameterType
                        , "  raw result bound: " ++ show rawBound
                        , "  leaked binders: " ++ show parameterLexicalLeaks
                        ]
                    )
          where
            parameterDeclarations = typeBinderDeclarationRefs parameterType
            parameterLexicalLeaks =
                [ freeRef
                | freeRef <- freeTypeVarRefsType rawBound
                , any
                    (typeBinderRefsSameIdentity freeRef)
                    parameterDeclarations
                ]

    requiredLambdaParameterType :: RequiredLambdaParameter -> ElabType
    requiredLambdaParameterType required =
        fromMaybe
            (TVarRef (requiredLambdaParameterRef required))
            (rlpStructuredParameterType required)

    requiredLambdaParameterRef required =
        fromMaybe
            ( typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (rlpParameterNode required))
                ("t" ++ show (getNodeId (rlpParameterNode required)))
            )
            (rlpSourceParameterRef required)

    -- Complete the paper's Var-Abs construction while the exact source body
    -- endpoint and its lambda-parameter certificate are both available.  The
    -- graph packet may contain the source body's leading foralls only in
    -- erased form; reintroduce that already-validated endpoint beneath the
    -- exact arrow instead of asking root closure to repair a free binder.
    completeAdministrativeSourceConstruction
        :: Maybe RequiredLambdaParameter
        -> Maybe PacketExpectedType
        -> [TypeBinderIdentity]
        -> [(TypeBinderRef, TypeBinderRef)]
        -> SchemeInfo
        -> SchemeInfo
        -> Either ElabError SchemeInfo
    completeAdministrativeSourceConstruction
        mbRequired
        mbExpected
        consumerIdentities
        constructionRenames
        construction
        operated =
            case (mbRequired, mbExpected) of
                (Just required, Just expected)
                    | packetExpectedTypeIsSource expected
                    , isNothing (rlpStructuredParameterType required)
                    , not (null operatedDeclarationRefs) ->
                    complete required
                _ -> pure construction
      where
        complete required =
            case constructionBodyCore of
                TArrow existingDomain rawCodomain
                    | not
                        ( constructionTypesAgree
                            existingDomain
                            parameterType
                        ) ->
                        Left
                            ( ValidationFailed
                                [ "administrative source construction has a conflicting parameter domain"
                                , "  lambda: " ++ show (rlpLambdaNode required)
                                , "  parameter: "
                                    ++ show (rlpParameterNode required)
                                , "  certified domain: " ++ show parameterType
                                , "  construction domain: "
                                    ++ show existingDomain
                                ]
                            )
                    | constructionTypesAgree rawCodomain operatedType ->
                        pure construction
                    | constructionTypesAgree
                        rawCodomain
                        (operatedCodomainUnderConstructionSpine required) ->
                        pure construction
                    | otherwise ->
                        -- 'SourceExpectedType' and 'RequiredLambdaParameter'
                        -- pair this exact source/canonical lambda occurrence
                        -- with its checked body endpoint.  That construction
                        -- capability, rather than the provisional graph
                        -- codomain's shape, decides the Var-Abs result.  The
                        -- graph may split the source result across a free
                        -- occurrence and a vacuous bounded carrier, so neither
                        -- component is required to resemble the complete
                        -- source codomain on its own.
                        rebuildCompletedConstruction
                  where
                    rebuildCompletedConstruction =
                        pure
                            ( rebuildSchemeInfoFromRefSubst
                                construction
                                ( mkElabSchemeWithRefs
                                    (retainedConstructionBinders required)
                                    ( foldr
                                        (uncurry TForallRef)
                                        ( TArrow
                                            parameterType
                                            ( operatedCodomainUnderConstructionSpine
                                                required
                                            )
                                        )
                                        constructionBodyLeadingBinders
                                    )
                                )
                                (siSubstRefs construction)
                            )
                other ->
                    Left
                        ( ValidationFailed
                            [ "administrative source construction lost its certified lambda arrow"
                            , "  lambda: " ++ show (rlpLambdaNode required)
                            , "  parameter: "
                                ++ show (rlpParameterNode required)
                            , "  construction body: " ++ show other
                            ]
                        )
          where
            parameterType = requiredLambdaParameterType required

        (constructionBodyLeadingBinders, constructionBodyCore) =
            splitForallsRefs (schemeBody (siScheme construction))

        -- Body-target generalization owns these leading declarations before
        -- Var-Abs constructs the current arrow.  Compare and rebuild beneath
        -- that exact spine; moving or duplicating it would change which
        -- lambda owns the inner parameter/result abstractions.
        operatedCodomainUnderConstructionSpine required =
            stripConstructionBodySpine
                constructionBodyLeadingBinders
                (nestedOperatedType required)

        stripConstructionBodySpine [] ty = ty
        stripConstructionBodySpine
            ((constructionRef, _) : remaining)
            (TForallRef operatedRef _ operatedBody)
                | typeBinderRefsSameIdentity constructionRef operatedRef =
                    stripConstructionBodySpine remaining operatedBody
        stripConstructionBodySpine _ ty = ty

        operatedType =
            schemeToType (siScheme operated)
        (operatedLeadingBinders, operatedBodyWithoutLeadingForalls) =
            splitForallsRefs operatedType
        nestedOperatedType required =
            case parameterBinderInOperated required of
                Nothing -> operatedType
                Just _ ->
                    case operatedBodyWithoutLeadingForalls of
                        TArrow domain codomain
                            | constructionTypesAgree
                                domain
                                (requiredLambdaParameterType required) ->
                                foldr
                                    (\(ref, mbBound) body ->
                                        TForallRef ref mbBound body
                                    )
                                    codomain
                                    ( nonParameterOperatedBinders
                                        required
                                    )
                        _ -> operatedType
        parameterBinderInOperated required =
            find
                ( typeBinderRefsSameIdentity
                    (requiredLambdaParameterRef required)
                    . fst
                )
                operatedLeadingBinders
        nonParameterOperatedBinders required =
            filter
                ( not
                    . typeBinderRefsSameIdentity
                        (requiredLambdaParameterRef required)
                    . fst
                )
                operatedLeadingBinders
        operatedDeclarationRefs =
            typeBinderDeclarationRefs operatedType
        movedConstructionRefs required =
            [ operatedRef
            | operatedRef <- operatedDeclarationRefs
            , not
                ( typeBinderRefsSameIdentity
                    operatedRef
                    (requiredLambdaParameterRef required)
                )
            ]
                ++ [ constructionRef
                   | (sourceRef, constructionRef) <- constructionRenames
                   , any
                        (typeBinderRefsSameIdentity sourceRef . fst)
                        (nonParameterOperatedBinders required)
                   ]
                ++ [ constructionRef
                   | consumerIdentity <- consumerIdentities
                   , (constructionRef, _) <-
                        schemeBinderRefs (siScheme construction)
                   , typeBinderRefIdentity constructionRef
                        == consumerIdentity
                   ]
        retainedConstructionBinders required =
            if
                any
                    ( typeBinderRefsSameIdentity parameterRef
                        . fst
                    )
                    retained
                then retained
                else (parameterRef, Nothing) : retained
          where
            parameterRef = requiredLambdaParameterRef required
            retained =
                [ binder
                | binder@(constructionRef, _) <-
                    schemeBinderRefs (siScheme construction)
                , not
                    ( any
                        (typeBinderRefsSameIdentity constructionRef)
                        (movedConstructionRefs required)
                    )
                ]
        constructionTypesAgree left right =
            alphaEqType left right || churchAwareEqType left right
    exactConsumerEndpoint
        :: Maybe RequiredLambdaParameter
        -> PacketExpectedType
        -> SchemeInfo
        -> ElabType
    exactConsumerEndpoint mbRequired expected operated =
        case (mbRequired, expected) of
            (Just required, sourceExpected)
                | packetExpectedTypeIsSource sourceExpected ->
                sourceLambdaConstructionType
                    required
                    (schemeToType (siScheme operated))
            _ ->
                packetOperatedExpectedType expected

    directlyAppliedLambdaResultType
        :: AnnExpr
        -> Either ElabError (Maybe ElabType)
    directlyAppliedLambdaResultType fun =
        case fun of
            ALam _ _ _ _ body _ _ ->
                sourceConstructionResultType body
            ALetScope inner _ _ ->
                directlyAppliedLambdaResultType inner
            _ -> pure Nothing

    -- A compiler-owned exact annotation copies the source graph and then
    -- constrains that copy to the producer type.  When a copied unbounded
    -- source node becomes concrete, that solved target is the construction
    -- bound for nested Var-Abs packets inside this annotation.  Preserve the
    -- relation locally to the annotated subtree; applying it globally would
    -- conflate independent instantiations of the same source scheme.
    edgeBoundOverlays (EdgeId edgeKey) =
        IntMap.map
            (chaseRedirects redirects)
            (edgeCopyMapping (EdgeId edgeKey))

    resultTypeViewWithOverlays =
        resultTypeViewWithBoundOverlays bindParentsGa resultTypeView

    edgeCopyMapping (EdgeId edgeKey) =
        case IntMap.lookup edgeKey (eaEdgeTraces edgeArtifacts) of
            Nothing -> IntMap.empty
            Just traceInfo -> getCopyMapping (etCopyMap traceInfo)

    -- Root partitioning can place the compiler-owned annotation node outside
    -- the retained expression tree while keeping its edge in the partition.
    -- Recover only unanimous, annotation-owned copy refinements here.  A
    -- source node instantiated to different targets has no single lexical
    -- construction bound, so it is deliberately omitted instead of choosing
    -- one traversal result.
    annotationBoundOverlays =
        IntMap.mapMaybe singletonTarget annotationTargets
      where
        annotationTargets =
            IntMap.fromListWith IntSet.union
                [ (sourceKey, IntSet.singleton (getNodeId solvedBound))
                | (edgeKey, traceInfo) <- IntMap.toList (eaEdgeTraces edgeArtifacts)
                , IntSet.member edgeKey exactAnnotationEdgeKeys
                , (sourceKey, copiedNode) <-
                    IntMap.toList (getCopyMapping (etCopyMap traceInfo))
                , let solvedBound = chaseRedirects redirects copiedNode
                ]

        singletonTarget targets =
            case IntSet.toList targets of
                [targetKey] -> Just (NodeId targetKey)
                _ -> Nothing

    exactAnnotationEdgeKeys =
        IntMap.keysSet exactProducerTypes
            `IntSet.union` IntSet.unions (map collectExactAnnotationEdges sources)

    -- A terminal root RaiseMerge names its exterior binder directly.  When
    -- the witness is identity, the lambda edge still has an allocated result
    -- variable that consumes the completed child scheme during Γ placement.
    -- Keep that topology as construction evidence so an identity T(e) does
    -- not erase the preceding φR abstraction.
    consumerForEdge
        sourceScopeRoot
        sourceBodyRoot
        boundaryScopeRoot
        boundaryBodyRoot
        edgeId = do
        rootAuthority <-
            rootRaiseMergeAuthorityFor edgeArtifacts edgeId
        case rootAuthority of
            Just authority ->
                pure
                    ( Just
                        (PreparedRootRaiseMergeBodyConsumer edgeId authority)
                    )
            Nothing -> topologyConsumer
      where
        rawArtifact = lookupEdgeArtifact edgeId rawEdgeArtifacts
        directFlexOwner node =
            IntMap.lookup
                (nodeRefKey (typeRef node))
                (cBindParents baseConstraint)
                == Just (GenRef sourceScopeRoot, BindFlex)

        -- Identity T(e) is construction authority only when it bridges a
        -- descendant body result into the exact flexible result allocated by
        -- this lambda.  If the body node is already directly owned by the
        -- lambda (the ordinary @\x -> f x@ case), no additional consumer is
        -- introduced.
        topologyConsumer =
            case rawArtifact of
                Just artifact
                    | let traceInfo = edgeArtifactTrace artifact
                          resultRoot = etResultRoot traceInfo
                    , directFlexOwner resultRoot
                    , not (directFlexOwner sourceBodyRoot) -> do
                        let witness = edgeArtifactWitness artifact
                            expansion = edgeArtifactExpansion artifact
                        let failures =
                                concat
                                    [ expect (ewLeft witness == resultRoot) "witness left is not the frozen result root"
                                    , expect (ewRight witness == resultRoot) "witness right is not the frozen result root"
                                    , expect (ewRoot witness == resultRoot) "witness root is not the frozen result root"
                                    , expect (ewForallIntros witness == 0) "identity topology has forall introductions"
                                    , expect (null (getInstanceOps (ewWitness witness))) "identity topology has instance operations"
                                    , expect (expansion == ExpIdentity) "identity topology has a non-identity expansion"
                                    , expect (etRoot traceInfo == sourceBodyRoot) "trace root is not the frozen body source"
                                    , expect (etResultRoot traceInfo == resultRoot) "trace result differs from the frozen result root"
                                    , expect (constructionCanonical sourceBodyRoot == constructionCanonical resultRoot) "body source and result are not one solved identity class"
                                    , expect
                                        ( IntMap.lookup
                                            (getNodeId sourceBodyRoot)
                                            (gaRestoredSchemeRootTargets bindParentsGa)
                                            == Just boundaryBodyRoot
                                        )
                                        "frozen body source is not restored to the paired source-lambda body root"
                                    , expect (null (etBinderArgs traceInfo)) "identity topology has binder arguments"
                                    , expect (IntMap.null (etBinderReplayMap traceInfo)) "identity topology has a binder replay map"
                                    , expect (null (etReplayDomainBinders traceInfo)) "identity topology has a replay domain"
                                    , expect (IntMap.null (getCopyMapping (etCopyMap traceInfo))) "identity topology has a copy map"
                                    , expect (etReplayContract traceInfo == ReplayContractNone) "identity topology has a replay contract"
                                    , identityTopologyAncestryFailures
                                        (cBindParents baseConstraint)
                                        sourceScopeRoot
                                        sourceBodyRoot
                                        resultRoot
                                    , expect (isRawTyVar sourceBodyRoot) "frozen body source is not a base-graph type variable"
                                    , expect (isRawTyVar resultRoot) "frozen result is not a base-graph type variable"
                                    , expect (isSolvedTyVar resultRoot) "frozen result is not a solved type variable"
                                    ]
                        if null failures
                            then
                                pure
                                    ( Just
                                        ( PreparedIdentityTopologyBodyConsumer
                                            edgeId
                                            sourceScopeRoot
                                            sourceBodyRoot
                                            boundaryScopeRoot
                                            boundaryBodyRoot
                                            resultRoot
                                        )
                                    )
                            else topologyFailure resultRoot failures
                _ -> pure Nothing

        expect condition message = [message | not condition]

        isRawTyVar node =
            case lookupNodeIn (cNodes baseConstraint) node of
                Just TyVar {} -> True
                _ -> False

        isSolvedTyVar node =
            case pvLookupNode presolutionView node of
                Just TyVar {} -> True
                _ -> False

        topologyFailure resultRoot failures =
            Left
                ( ValidationFailed
                    ( [ "lambda identity-topology consumer failed construction validation"
                      , "  edge: " ++ show edgeId
                      , "  lambda scope: " ++ show sourceScopeRoot
                      , "  body source: " ++ show sourceBodyRoot
                      , "  paired boundary scope: " ++ show boundaryScopeRoot
                      , "  paired boundary body: " ++ show boundaryBodyRoot
                      , "  result root: " ++ show resultRoot
                      ]
                        ++ map ("  " ++) failures
                    )
                )

    consumerOwnerForEdge closures fallbackOwner consumer =
        case consumer of
            PreparedIdentityTopologyBodyConsumer {} ->
                pure (Just fallbackOwner)
            PreparedRootRaiseMergeBodyConsumer edgeId _ ->
                let consumerIdentity = preparedLambdaBodyConsumerIdentity consumer
                 in case IntMap.lookup (getEdgeId edgeId) closures of
                        Just closure
                            | edgeId `elem` lgcEdgeIds closure
                            , consumerIdentity == lgcConsumerIdentity closure ->
                                pure (Just (lgcOwner closure))
                            | otherwise ->
                                Left
                                    ( ValidationFailed
                                        [ "prepared consumer disagrees with its local Gamma closure"
                                        , "  edge: " ++ show edgeId
                                        , "  consumer: " ++ show consumerIdentity
                                        , "  closure: " ++ show closure
                                        ]
                                    )
                        Nothing -> pure Nothing

    collectExactAnnotationEdges ann =
        case ann of
            AResolvedVar {} -> IntSet.empty
            ALit {} -> IntSet.empty
            ALam _ _ _ _ body _ _ -> collectExactAnnotationEdges body
            AApp fun arg _ _ _ ->
                collectExactAnnotationEdges fun
                    `IntSet.union` collectExactAnnotationEdges arg
            ALet _ _ _ _ _ _ rhs body _ ->
                collectExactAnnotationEdges rhs
                    `IntSet.union` collectExactAnnotationEdges body
            AExactAnn inner _ _ eid ->
                IntSet.insert (getEdgeId eid) (collectExactAnnotationEdges inner)
            AAnn inner _ _ -> collectExactAnnotationEdges inner
            ALetScope inner _ _ -> collectExactAnnotationEdges inner
            AUnfold inner _ _ -> collectExactAnnotationEdges inner

    prepareRequiredLambdaParameter
        nestedDetails
        parameterNode
        lambdaNode
        nestedBody
        mbExpectedLambdaType = do
            sourceParameterType <-
                case desugaredAnnLambdaInfo nestedDetails nestedBody of
                    Just (_, annotationNode, _, _) ->
                        case
                            IntMap.lookup
                                (getNodeId annotationNode)
                                annExpectedTypes
                        of
                            Just parameterTy -> pure (Just parameterTy)
                            Nothing ->
                                Left
                                    ( ValidationFailed
                                        [ "administrative annotated lambda lost its exact parameter type"
                                        , "  lambda: " ++ show lambdaNode
                                        , "  parameter: " ++ show parameterNode
                                        , "  annotation: " ++ show annotationNode
                                        ]
                                    )
                    Nothing -> pure Nothing
            let expectedParameterType =
                    mbExpectedLambdaType >>= packetLambdaParameterType
            structuredParameterType <-
                case (sourceParameterType, expectedParameterType) of
                    (Just sourceTy, Just expectedTy)
                        | alphaEqType sourceTy expectedTy
                            || churchAwareEqType sourceTy expectedTy ->
                            pure (Just sourceTy)
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "administrative lambda parameter disagrees with its expected arrow domain"
                                    , "  lambda: " ++ show lambdaNode
                                    , "  parameter: " ++ show parameterNode
                                    , "  source parameter type: " ++ show sourceTy
                                    , "  expected parameter type: " ++ show expectedTy
                                    ]
                                )
                    (Just sourceTy, Nothing) -> pure (Just sourceTy)
                    (Nothing, Just expectedTy) ->
                        -- The enclosing source/compiler-exact endpoint is the
                        -- positive construction authority for this
                        -- administrative lambda.  Prefer its complete domain
                        -- to a base-graph copy whose recursive self edge may
                        -- already have been expanded to the full mu owner.
                        pure (Just expectedTy)
                    (Nothing, Nothing) ->
                        case
                            lookupNodeIn
                                (cNodes baseConstraint)
                                parameterNode
                        of
                            Just (TyVar _ Nothing) -> pure Nothing
                            Just (TyVar _ (Just boundNode)) ->
                                Just
                                    <$> reifyBaseParameterType boundNode
                            Just _ ->
                                Just
                                    <$> reifyBaseParameterType parameterNode
                            Nothing ->
                                Left
                                    ( ValidationFailed
                                        [ "administrative lambda parameter is absent from the base graph"
                                        , "  lambda: " ++ show lambdaNode
                                        , "  parameter: " ++ show parameterNode
                                        ]
                                    )
            pure
                RequiredLambdaParameter
                    { rlpParameterNode = parameterNode
                    , rlpLambdaNode = lambdaNode
                    , rlpStructuredParameterType = structuredParameterType
                    , rlpSourceParameterRef =
                        case structuredParameterType of
                            Just (TVarRef ref) -> Just ref
                            _ -> Nothing
                    }
      where
        reifyBaseParameterType node = do
            view <- resultTypeView
            View.rtvReifyBaseNoFallback view node

    -- Pairing the source and canonical lambda trees with the source expected
    -- arrow determines this route before graph generalization runs.  This is
    -- stronger than accepting alpha-equivalent domains afterwards: the exact
    -- lambda occurrence, parameter node, and source binder identity travel in
    -- one construction capability.
    installExpectedLambdaParameterSourceRef
        lambdaNode
        parameterNode
        mbExpected
        refs =
            case mbExpected >>= packetLambdaParameterType of
                Just (TVarRef sourceRef) ->
                    insertLambdaParameterSourceRef
                        lambdaNode
                        parameterNode
                        sourceRef
                        refs
                _ -> pure refs

    -- A parent administrative packet is prepared after its descendant lambda
    -- packets, but it generalizes the whole exact lambda spine.  Carry every
    -- source-owned parameter route from that spine into the parent packet now,
    -- while source/canonical lockstep and the annotation endpoint are both
    -- available.  This is the Var-Abs construction certificate for occurrences
    -- in an enclosing RaiseMerge bound; recovering the routes from the solved
    -- quotient later would conflate distinct source parameters.
    expectedLambdaSpineSourceBinderRefs
        mbExpected
        sourceLambda
        canonLambda =
            case (mbExpected, sourceLambda, canonLambda) of
                ( Just expected
                  , AAnn sourceInner _ _
                  , AAnn canonInner _ _
                  ) ->
                    expectedLambdaSpineSourceBinderRefs
                        (Just expected)
                        sourceInner
                        canonInner
                ( Just expected
                  , ALam _ _ _ _ sourceBody _ _
                  , ALam _ _ canonParam _ canonBody _ canonLambdaNode
                  ) -> do
                    currentRefs <-
                        installExpectedLambdaParameterSourceRef
                            canonLambdaNode
                            canonParam
                            (Just expected)
                            IntMap.empty
                    nestedRefs <-
                        expectedLambdaSpineSourceBinderRefs
                            (packetLambdaBodyType expected)
                            sourceBody
                            canonBody
                    mergeCompilerExactConstructionBinderRefs
                        currentRefs
                        nestedRefs
                _ -> pure IntMap.empty

    installRequiredLambdaParameterSourceRef mbRequired refs =
        case mbRequired of
            Nothing -> pure refs
            Just required ->
                case rlpSourceParameterRef required of
                    Nothing -> pure refs
                    Just sourceRef ->
                        insertLambdaParameterSourceRef
                            (rlpLambdaNode required)
                            (rlpParameterNode required)
                            sourceRef
                            refs

    insertLambdaParameterSourceRef
        lambdaNode
        parameterNode
        sourceRef
        refs =
            let parameterKey = getNodeId parameterNode
             in case IntMap.lookup parameterKey refs of
                    Nothing ->
                        pure
                            ( IntMap.insert
                                parameterKey
                                sourceRef
                                refs
                            )
                    Just existingRef
                        | typeBinderRefsSameIdentity
                            existingRef
                            sourceRef ->
                            pure refs
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "source-owned lambda parameter has conflicting identity routes"
                                    , "  lambda: " ++ show lambdaNode
                                    , "  parameter: " ++ show parameterNode
                                    , "  prepared source ref: "
                                        ++ show sourceRef
                                    , "  existing source ref: "
                                        ++ show existingRef
                                    ]
                                )

    -- Construct the packet's binder scope while the source-owned lambda
    -- capability is still available.  The returned scheme is locally closed
    -- exactly once: an unused nested parameter is introduced by its own
    -- Var-Abs packet, while enclosing/source binders remain ambient and can
    -- never enter this packet's quantifier spine.
    constructPacketBodyScheme
        mbRequiredLambdaParam
        availableSourceBinderRefs
        ambientBinderRefs
        packet = do
            packetWithRequiredParam <-
                case mbRequiredLambdaParam of
                    Nothing -> pure packet
                    Just requiredParam ->
                        ensureRequiredLambdaParameter
                            availableSourceBinderRefs
                            requiredParam
                            packet
            let subst = siSubstRefs packetWithRequiredParam
                locallyRequiredParameterRef = do
                    required <- mbRequiredLambdaParam
                    guard
                        (isNothing (rlpStructuredParameterType required))
                    pure
                        ( fromMaybe
                            ( requiredLambdaParameterRef required
                            )
                            ( refForNode
                                subst
                                (rlpParameterNode required)
                            )
                        )
                retainedBinders =
                    [ binding
                    | binding@(ref, _) <-
                        schemeBinderRefs (siScheme packetWithRequiredParam)
                    , maybe
                        False
                        (typeBinderRefsSameIdentity ref)
                        locallyRequiredParameterRef
                        || not
                            ( any
                                (typeBinderRefsSameIdentity ref)
                                ambientBinderRefs
                            )
                    ]
            pure
                ( schemeInfoFromRefSubst
                    ( mkElabSchemeWithRefs
                        retainedBinders
                        (schemeBody (siScheme packetWithRequiredParam))
                    )
                    subst
                )

    ensureRequiredLambdaParameter
        availableSourceBinderRefs
        requiredParam
        packet =
        case rlpStructuredParameterType requiredParam of
            Just parameterTy -> do
                packetInSourceDomain <-
                    projectStructuredParameterSourceBinders
                        parameterTy
                        packet
                ensureStructuredLambdaParameter
                    parameterTy
                    packetInSourceDomain
            Nothing ->
                ensureUnboundedLambdaParameter packet
      where
        -- The source annotation and the graph packet can name the same
        -- parameter through different identities.  Project the complete
        -- packet through only those sidecar routes named by the structured
        -- parameter before constructing its arrow.  Only free parameter
        -- identities can be shared with the packet: declarations inside the
        -- complete parameter type are lexical to that type and must not
        -- capture a fresh result binder (the paper's @g g@ needs distinct
        -- source-parameter and result-forall identities).
        projectStructuredParameterSourceBinders parameterTy packet0 =
            let parameterRefs =
                    distinctTypeBinderRefs
                        (freeTypeVarRefsType parameterTy)
                parameterSourceBinderRefs =
                    IntMap.filter
                        ( \sourceRef ->
                            any
                                (typeBinderRefsSameIdentity sourceRef)
                                parameterRefs
                        )
                        availableSourceBinderRefs
             in if IntMap.null parameterSourceBinderRefs
                    then pure packet0
                    else
                        either
                            ( \cause ->
                                Left
                                    ( ValidationFailed
                                        [ "source-owned lambda parameter projection failed"
                                        , "  lambda: "
                                            ++ show
                                                (rlpLambdaNode requiredParam)
                                        , "  parameter: "
                                            ++ show
                                                (rlpParameterNode requiredParam)
                                        , "  parameter type: "
                                            ++ show parameterTy
                                        , "  cause: " ++ cause
                                        ]
                                    )
                            )
                            Right
                            ( resolveConstructionSourceBindersInSchemeInfoExcept
                                Set.empty
                                identityRepresentative
                                parameterSourceBinderRefs
                                packet0
                            )

        ensureStructuredLambdaParameter parameterTy packet0 =
            let schemeInfo = siScheme packet0
                body = schemeBody schemeInfo
                rebuild completedBody =
                    packet0
                        { siScheme =
                            mkElabSchemeWithRefs
                                (schemeBinderRefs schemeInfo)
                                completedBody
                        }
             in pure
                    ( rebuild
                        ( restoreStructuredParameterDomain
                            parameterTy
                            body
                        )
                    )

        ensureUnboundedLambdaParameter packet0 =
            let binders = schemeBinderRefs (siScheme packet0)
                subst = siSubstRefs packet0
                body = schemeBody (siScheme packet0)
                paramNode = rlpParameterNode requiredParam
                paramRef =
                    fromMaybe
                        ( typeBinderRefFromIdentity
                            (typeBinderIdentityFromNode paramNode)
                            ("t" ++ show (getNodeId paramNode))
                        )
                        (refForNode subst paramNode)
                paramAlreadyBound =
                    any (typeBinderRefsSameIdentity paramRef . fst) binders
                bindersWithParam
                    | paramAlreadyBound = binders
                    | otherwise = (paramRef, Nothing) : binders
                rebuild completedBody =
                    schemeInfoFromRefSubst
                        (mkElabSchemeWithRefs bindersWithParam completedBody)
                        (IntMap.insert (getNodeId paramNode) paramRef subst)
           in pure
                ( rebuild
                    (restoreMissingParameterDomain paramRef body)
                )

    restoreMissingParameterDomain paramRef ty =
        case ty of
            TForallRef ref mbBound body ->
                TForallRef
                    ref
                    mbBound
                    (restoreMissingParameterDomain paramRef body)
            _ -> TArrow (TVarRef paramRef) ty

    restoreStructuredParameterDomain parameterTy ty =
        case ty of
            TForallRef ref mbBound body ->
                TForallRef
                    ref
                    mbBound
                    (restoreStructuredParameterDomain parameterTy body)
            _ -> TArrow parameterTy ty

    generalizeBody ownerScope mbRequiredLambdaParam mbEnclosingParam localSourceBinderRefs requirementDescendants mbAuthority expectedType boundOverlays ownedDescendants placementDescendants opaqueResultConstructions sourceBody canonBody = do
        baseScopeRoot <-
            case mbAuthority of
                Nothing -> pure ownerScope
                Just authority -> pure (genRef (gpaOwnerGen authority))
        let (generalizationSource, generalizationCanon) =
                case (mbRequiredLambdaParam, sourceBody, canonBody) of
                    ( Just required
                      , ALam _ _ _ _ sourceLambdaBody _ _
                      , ALam _ _ _ _ canonLambdaBody _ canonLambdaNode
                      )
                        | canonLambdaNode == rlpLambdaNode required ->
                            (sourceLambdaBody, canonLambdaBody)
                    _ -> (sourceBody, canonBody)
            -- An administrative Var-Abs packet owns the lambda constructor,
            -- but Gen operates on S'(body).  Generalizing the whole lambda
            -- target and only afterwards restoring its parameter can feed the
            -- pending result carrier back into its own bound.  Select the body
            -- target while the paired lambda/parameter certificate is present;
            -- 'constructPacketBodyScheme' constructs the arrow exactly once.
            baseTarget =
                schemeBodyTarget
                    presolutionView
                    (annNode generalizationCanon)
            expectedElabType = packetExpectedType <$> expectedType
            ambientBinderRefs =
                distinctTypeBinderRefs
                    ( packetAmbientBinderRefs
                        localSourceBinderRefs
                        expectedType
                        mbEnclosingParam
                        ++ concatMap
                            opaqueResultConstructionAmbientRefs
                            opaqueResultConstructions
                    )
            descendantTermUsedBinderRefs =
                distinctTypeBinderRefs
                    [ ref
                    | ref <-
                        administrativeConsumerRefs
                            ++ packetTermUsedBinderRefs
                                localSourceBinderRefs
                                ambientBinderRefs
                                ownedDescendants
                    , not
                        ( any
                            (typeBinderRefsSameIdentity ref)
                            opaqueResultBinderRefs
                        )
                    ]
            -- Once an administrative packet generalizes S'(body), a
            -- descendant topology result need not occur in that body type.
            -- Its prepared consumer capability is nevertheless positive
            -- declaration ownership for this exact parent construction.  Feed
            -- that identity to Gen explicitly so placement receives the
            -- pending binder without recovering it from the whole lambda.
            administrativeConsumerRefs =
                case mbRequiredLambdaParam of
                    Nothing -> []
                    Just _ ->
                        [ ref
                        | packet <- Map.elems placementDescendants
                        , any
                            subtermConsumerAuthorityIsTopology
                            ( maybeToList
                                ( subtermGeneralizationConsumerAuthority
                                    packet
                                )
                                ++ maybeToList
                                    ( subtermGeneralizationLocalResultAuthority
                                        packet
                                    )
                            )
                        , ref <-
                            maybeToList
                                ( subtermGeneralizationConstructionResultAbstractionRef
                                    packet
                                )
                        , constructionRefHasLiveRoute ref
                        ]
            -- A descendant capability can become term-used authority only
            -- when its graph occurrence is still live at this Gen target (or
            -- an exact source sidecar keeps the solved structural carrier
            -- live).  Merely retaining a packet is not enough: requesting a
            -- dead result identity would ask binder planning to manufacture a
            -- declaration with no route in the solved graph.
            constructionRefHasLiveRoute ref =
                case typeBinderRefNode ref of
                    Nothing -> False
                    Just node ->
                        case
                            pvLookupNode
                                presolutionView
                                (constructionCanonical node)
                        of
                            Just TyVar{} -> True
                            Just _ ->
                                IntMap.member
                                    (getNodeId node)
                                    localSourceBinderRefs
                                    || IntMap.member
                                        (getNodeId (constructionCanonical node))
                                        localSourceBinderRefs
                            Nothing -> False
            -- A crossed lambda packet closes the declarations in its exact
            -- source construction before that construction replaces the
            -- enclosing graph carrier.  Such a declaration can occur free in
            -- the packet's consumer-facing Gamma bound, but it is not free in
            -- the enclosing term construction.  Generalizing it again at the
            -- parent would duplicate the same identity on both sides of the
            -- restored lambda arrow.
            opaqueResultBinderRefs =
                concatMap
                    (typeBinderDeclarationRefs . opaqueResultType)
                    opaqueResultConstructions
            opaqueResultType (_, packetType, _) = packetType
        case mbAuthority of
            Nothing -> do
                requirements0 <-
                    generalizationRequirementsForOwnedScope
                        identityRepresentative
                        constructionCanonical
                        bindParentsGa
                        baseScopeRoot
                        presolutionView
                        edgeArtifacts
                        exactProducerTypes
                        localSourceBinderRefs
                        requirementDescendants
                        []
                        expectedElabType
                        generalizationSource
                let requirements =
                        requirements0
                            { grAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( ambientBinderRefs
                                        ++ grAmbientBinderRefs requirements0
                                    )
                            , grTermUsedRootBinderRefs =
                                distinctTypeBinderRefs
                                    ( descendantTermUsedBinderRefs
                                        ++ grTermUsedRootBinderRefs requirements0
                                    )
                            }
                (target, schemeRaw, subst, inheritedGammaRoutes) <-
                    generalizeTarget
                        boundOverlays
                        baseScopeRoot
                        baseTarget
                        requirements
                schemeInfoRaw <-
                    composeOpaqueResultConstructions
                        ambientBinderRefs
                        opaqueResultConstructions
                        (schemeInfoFromRefSubst schemeRaw subst)
                descendantOwnsRootRaiseMerge <-
                    rootRaiseMergeClosedByDescendant
                        requirements
                        ownedDescendants
                        generalizationSource
                schemeInfoPrepared <-
                    if descendantOwnsRootRaiseMerge
                        then pure schemeInfoRaw
                        else
                            case
                                prepareRootRaiseMergeScheme
                                    edgeArtifacts
                                    generalizationSource
                                    requirements
                                    schemeInfoRaw
                            of
                                Right prepared -> pure prepared
                                Left cause ->
                                    Left
                                        ( ValidationFailed
                                            [ "root packet preparation has no local or descendant construction"
                                            , "  descendant packets: "
                                                ++ show
                                                    [ ( subtermGeneralizationGammaAuthority packet
                                                      , subtermGeneralizationConsumerAuthority packet
                                                      )
                                                    | packet <- Map.elems ownedDescendants
                                                    ]
                                            , "  cause: " ++ show cause
                                            ]
                                        )
                bodySchemeInfo <-
                    constructPacketBodyScheme
                        mbRequiredLambdaParam
                        localSourceBinderRefs
                        ambientBinderRefs
                        schemeInfoPrepared
                pure
                    ( target
                    , bodySchemeInfo
                    , schemeInfoPrepared
                    , inheritedGammaRoutes
                    )
            Just gammaAuthority -> do
                let authorityEdge = gpaEdgeId gammaAuthority
                    authorityScope = genRef (gpaOwnerGen gammaAuthority)
                    -- A Gamma packet must construct its Hyp before Gen can
                    -- compose an opaque result carrier.  Only this branch has
                    -- such a pending Hyp: a direct packet first generalizes
                    -- its ordinary result and composes the opaque construction
                    -- afterwards.  Keeping the certificate branch-local makes
                    -- it impossible to publish a descendant operated endpoint
                    -- as an enclosing direct-packet requirement.
                    opaqueResultOperatedEdges =
                        [ ( scaEdgeId authority
                          , Just
                                ( RootEdgeExactOperated
                                    constructedType
                                )
                          )
                        | (_, constructedType, packet) <-
                            opaqueResultConstructions
                        , Just (sourceLambdaNode, certifiedType) <-
                            [ subtermGeneralizationSourceLambdaResultConstruction
                                packet
                            ]
                        , operationalEndpointTypesAgree
                            certifiedType
                            constructedType
                        , Just authority <-
                            [subtermGeneralizationConsumerAuthority packet]
                        , Just enclosingOwner <-
                            [ subtermConsumerAuthorityEnclosingOwner
                                authority
                            ]
                        , scaEdgeId authority
                            == lgoBoundaryEdge enclosingOwner
                        , sourceLambdaNode /= lgoTermNode enclosingOwner
                        , Just rootAuthority <-
                            [ either
                                (const Nothing)
                                id
                                ( rootRaiseMergeAuthorityFor
                                    edgeArtifacts
                                    (scaEdgeId authority)
                                )
                            ]
                        , rrmaOperatedRoot rootAuthority
                            == sourceLambdaNode
                        , null
                            ( schemeBinderRefs
                                (schemeFromType constructedType)
                            )
                        ]
                authority <-
                    rootRaiseMergeAuthorityFor edgeArtifacts authorityEdge
                        >>= maybe
                            ( Left
                                ( ValidationFailed
                                    [ "lambda-body packet lost its root RaiseMerge authority"
                                    , "  edge: " ++ show authorityEdge
                                    ]
                                )
                            )
                            Right
                operatedRequirements0 <-
                    generalizationRequirementsForOwnedScope
                        identityRepresentative
                        constructionCanonical
                        bindParentsGa
                        authorityScope
                        presolutionView
                        edgeArtifacts
                        exactProducerTypes
                        localSourceBinderRefs
                        requirementDescendants
                        opaqueResultOperatedEdges
                        expectedElabType
                        generalizationSource
                let operatedRequirements =
                        operatedRequirements0
                            { grAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( ambientBinderRefs
                                        ++ grAmbientBinderRefs operatedRequirements0
                                    )
                            , grTermUsedRootBinderRefs =
                                distinctTypeBinderRefs
                                    ( descendantTermUsedBinderRefs
                                        ++ grTermUsedRootBinderRefs
                                            operatedRequirements0
                                    )
                            }
                ( _operatedTarget
                  , operatedSchemeRaw
                  , operatedSubst
                  , operatedInheritedGammaRoutes
                  ) <-
                    case sourceConstructedOperatedScheme operatedRequirements expectedType of
                        Just constructed ->
                            pure
                                ( rrmaOperatedRoot authority
                                , siScheme constructed
                                , siSubstRefs constructed
                                , Reify.emptyInheritedGammaRoutes
                                )
                        Nothing ->
                            generalizeTarget
                                boundOverlays
                                authorityScope
                                (rrmaOperatedRoot authority)
                                operatedRequirements
                -- This is the open operated view used to construct this
                -- packet's own root RaiseMerge.  The construction below
                -- closes every quantifier that view depends on; only that
                -- completed packet is a valid S'(operated) bound for an
                -- enclosing consumer.  Validate the completed construction,
                -- not this intermediate open view.
                operatedSchemeInfoRaw <-
                    composeOpaqueResultConstructions
                        ambientBinderRefs
                        opaqueResultConstructions
                        ( schemeInfoFromRefSubst
                            operatedSchemeRaw
                            operatedSubst
                        )
                (operatedSchemeInfo, _constructionBinderRenames, _exactBinderRenames) <-
                    constructPacketOperatedScheme
                        identityRepresentative
                        constructionCanonical
                        (resultTypeViewWithOverlays boundOverlays)
                        localSourceBinderRefs
                        Nothing
                        expectedType
                        operatedSchemeInfoRaw
                requirements0 <-
                    generalizationRequirementsForOwnedScope
                        identityRepresentative
                        constructionCanonical
                        bindParentsGa
                        authorityScope
                        presolutionView
                        edgeArtifacts
                        exactProducerTypes
                        localSourceBinderRefs
                        requirementDescendants
                        ( ( authorityEdge
                          , Just
                              ( RootEdgeExactProducer
                                  (schemeToType (siScheme operatedSchemeInfo))
                              )
                          )
                            : opaqueResultOperatedEdges
                        )
                        Nothing
                        generalizationSource
                let requirements =
                        requirements0
                            { grAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( ambientBinderRefs
                                        ++ grAmbientBinderRefs requirements0
                                    )
                            , grTermUsedRootBinderRefs =
                                distinctTypeBinderRefs
                                    ( descendantTermUsedBinderRefs
                                        ++ grTermUsedRootBinderRefs requirements0
                                    )
                            }
                ( target
                  , schemeRaw
                  , subst
                  , constructionInheritedGammaRoutes
                  ) <-
                    generalizeTarget
                        boundOverlays
                        authorityScope
                        baseTarget
                        requirements
                constructionSchemeInfo <-
                    composeOpaqueResultConstructions
                        ambientBinderRefs
                        opaqueResultConstructions
                        (schemeInfoFromRefSubst schemeRaw subst)
                schemeInfoPrepared <-
                    case
                        prepareRootRaiseMergeSchemeAtEdge
                            edgeArtifacts
                            authorityEdge
                            requirements
                            constructionSchemeInfo
                    of
                        Right prepared -> pure prepared
                        Left err ->
                            Left
                                ( ValidationFailed
                                    [ "root RaiseMerge construction packet validation failed"
                                    , "  operated scheme: " ++ show operatedSchemeInfo
                                    , "  construction scheme: " ++ show constructionSchemeInfo
                                    , "  requirements: " ++ show requirements
                                    , "  cause: " ++ show err
                                    ]
                                )
                bodySchemeInfo <-
                    constructPacketBodyScheme
                        mbRequiredLambdaParam
                        localSourceBinderRefs
                        ambientBinderRefs
                        schemeInfoPrepared
                inheritedGammaRoutes <-
                    Reify.mergeInheritedGammaRoutes
                        operatedInheritedGammaRoutes
                        constructionInheritedGammaRoutes
                pure
                    ( target
                    , bodySchemeInfo
                    , operatedSchemeInfo
                    , inheritedGammaRoutes
                    )
      where
        -- A checked source endpoint is already Figure 15.3.5's S'(operated)
        -- when no additional Gamma declaration is owned by this
        -- generalization.  Construct that scheme directly while its source
        -- identities are present.  Re-generalizing the pending RaiseMerge's
        -- live operated node would make its result feed back into its own
        -- bound and can manufacture a binder dependency cycle.
        sourceConstructedOperatedScheme requirements mbExpected =
            case mbExpected of
                Just (ReturnedBindingSourceExpectedType sourceType _)
                    | hasNoAdditionalGamma ->
                        constructed sourceType
                Just (SourceExpectedType sourceType _)
                    | hasNoAdditionalGamma
                    , null (freeTypeVarRefsType sourceType) ->
                        -- A closed source/application endpoint carries no
                        -- lexical identity that Gen still has to place.  It
                        -- is therefore the complete S'(operated), just like
                        -- an exact returned binding.  Open source endpoints
                        -- deliberately stay on the ordinary route so their
                        -- source-binder authorities remain explicit.
                        constructed sourceType
                _ -> Nothing
          where
            hasNoAdditionalGamma =
                null (grRequiredGammaBinders requirements)
                    && null (grTermUsedRootBinderRefs requirements)
                    && IntMap.null
                        (grAmbientGammaAuthorities requirements)

            constructed sourceType =
                Just
                    ( schemeInfoFromRefSubst
                        (schemeFromType sourceType)
                        IntMap.empty
                    )

        generalizeTarget targetBoundOverlays scopeRoot target requirements = do
            view <- resultTypeViewWithOverlays targetBoundOverlays
            (schemeRaw, subst, inheritedGammaRoutes) <-
                case
                    View.rtvGeneralizeTargetWithRequirementsCertified
                        requirements
                        view
                        scopeRoot
                        target
                of
                    Right generalized -> pure generalized
                    Left cause ->
                        Left
                            ( ValidationFailed
                                [ "subterm packet generalization failed"
                                , "  scope: " ++ show scopeRoot
                                , "  target: " ++ show target
                                , "  source: " ++ show sourceBody
                                , "  expected endpoint: "
                                    ++ show (packetExpectedType <$> expectedType)
                                , "  ambient binders: "
                                    ++ show (grAmbientBinderRefs requirements)
                                , "  requirements: " ++ show requirements
                                , "  target node/bound/type: "
                                    ++ show
                                        ( View.rtvLookupNode view target
                                        , View.rtvLookupVarBound view target
                                        , View.rtvReifyNoFallback view target
                                        )
                                , "  required operated node/bound/types: "
                                    ++ show
                                        [ ( rgbOperatedRoot requirement
                                          , View.rtvLookupNode
                                                view
                                                (rgbOperatedRoot requirement)
                                          , View.rtvLookupVarBound
                                                view
                                                (rgbOperatedRoot requirement)
                                          , View.rtvReifyNoFallback
                                                view
                                                (rgbOperatedRoot requirement)
                                          )
                                        | requirement <-
                                            grRequiredGammaBinders requirements
                                        ]
                                , "  required operated occurrence routes: "
                                    ++ show
                                        [ ( occurrenceRef
                                          , [ ( routeNode
                                              , IntMap.lookup
                                                    (getNodeId routeNode)
                                                    (grSourceBinderRefs requirements)
                                              )
                                            | routeNode <-
                                                maybe
                                                    []
                                                    ( gaConstructionRouteNodes
                                                        constructionCanonical
                                                        bindParentsGa
                                                    )
                                                    (typeBinderRefNode occurrenceRef)
                                            ]
                                          )
                                        | requirement <-
                                            grRequiredGammaBinders requirements
                                        , occurrenceRef <-
                                            freeTypeVarRefsType
                                                (rgbOperatedType requirement)
                                        ]
                                , "  cause: " ++ show cause
                                ]
                            )
            pure (target, schemeRaw, subst, inheritedGammaRoutes)

        -- A packet for an enclosing lambda can have a source annotation as
        -- its body while the annotation's immediate lambda has already
        -- constructed its own root RaiseMerge in a descendant packet.  The
        -- outer packet must preserve that completed child, not reclaim the
        -- child's edge merely because 'rootLambdaBodyEdge' can see through
        -- the transparent annotation wrapper.
        rootRaiseMergeClosedByDescendant requirements descendants expr = do
            mbRootAuthority <-
                rootRaiseMergeAuthorityForExpression edgeArtifacts expr
            case mbRootAuthority of
                Nothing -> pure False
                Just (edgeId, authority)
                    | any
                        (elem edgeId . NonEmpty.toList . rgbEdgeIds)
                        (grRequiredGammaBinders requirements) ->
                        pure False
                    | otherwise ->
                        case
                            [ packet
                            | packet <- Map.elems descendants
                            , subtermGeneralizationConsumerIdentity packet
                                == Just
                                    ( typeBinderIdentityFromNode
                                        (rrmaExterior authority)
                                    )
                            , subtermGeneralizationOwnsGammaForEdge
                                edgeId
                                packet
                                || case
                                    subtermGeneralizationConsumerAuthority packet
                                        >>= subtermConsumerAuthorityEnclosingOwner
                                of
                                    Just owner ->
                                        lgoBoundaryEdge owner == edgeId
                                    Nothing -> False
                            ]
                        of
                            [] -> pure False
                            [_] -> pure True
                            packets ->
                                Left
                                    ( ValidationFailed
                                        [ "multiple descendant packets own one root RaiseMerge"
                                        , "  edge: " ++ show edgeId
                                        , "  authority: " ++ show authority
                                        , "  packets: " ++ show packets
                                        ]
                                    )

    opaqueResultConstructionAmbientRefs (carrierRef, packetType, _) =
        carrierRef
            : typeBinderDeclarationRefs packetType
                ++ freeTypeVarRefsType packetType

    -- An opaque result path crosses a lambda whose graph result remains a
    -- bare carrier in the enclosing presolution.  The descendant's completed
    -- administrative packet is the construction of that carrier.  Admit the
    -- carrier and the packet's already-declared binder spine as ambient while
    -- Gen exposes the open result, then compose the completed packet
    -- immediately.  Otherwise Gen can quantify one declaration from the
    -- opened carrier bound while leaving a later declaration free, splitting
    -- a single certified packet across lexical owners.  This is the
    -- type-level counterpart of emitting the nested Var-Abs term: no free
    -- carrier is published and no later elaboration repair has to rediscover
    -- the relation.
    composeOpaqueResultConstructions
        ambientBinderRefs
        constructions
        initialSchemeInfo
            | null constructions = pure initialSchemeInfo
            | otherwise = do
                completed0 <-
                    foldM composeOne initialSchemeInfo constructions
                completed <-
                    foldM
                        closeDeclaredCarrierBound
                        completed0
                        constructions
                let carrierRefs =
                        [ carrierRef
                        | (carrierRef, _, _) <- constructions
                        ]
                    survivingCarriers =
                        [ freeRef
                        | freeRef <-
                            freeTypeVarRefsType
                                (schemeToType (siScheme completed))
                        , any
                            (typeBinderRefsSameIdentity freeRef)
                            carrierRefs
                        ]
                    enclosingAmbientRefs =
                        [ ref
                        | ref <- ambientBinderRefs
                        , not
                            ( any
                                (typeBinderRefsSameIdentity ref)
                                carrierRefs
                            )
                        ]
                case survivingCarriers of
                    [] -> do
                        checkedScheme <-
                            validateSchemeClosure
                                "opaque descendant result composition"
                                ( ambientSchemeClosureAuthority
                                    enclosingAmbientRefs
                                )
                                (siScheme completed)
                        pure completed {siScheme = checkedScheme}
                    _ ->
                        Left
                            ( ValidationFailed
                                [ "opaque descendant result composition left its graph carrier free"
                                , "  carriers: " ++ show carrierRefs
                                , "  surviving carriers: "
                                    ++ show survivingCarriers
                                , "  completed scheme: "
                                    ++ show (siScheme completed)
                                ]
                            )
      where
        -- Gen can retain an opaque carrier as a bounded declaration instead
        -- of leaving a free occurrence for 'composeOne'.  Its bound is then
        -- the opened descendant endpoint: leading source foralls have become
        -- free dependencies of the bound.  The source-lambda construction
        -- certificate closes that exact endpoint before the declaration is
        -- published, keeping the dependency lexical to the packet.
        closeDeclaredCarrierBound
            current
            construction@(carrierRef, packetType, _packet) = do
                validateSourceLambdaConstruction construction
                case matchingOuterBinders of
                    [] -> do
                        mbBody <- closeLeadingBinder (schemeBody currentScheme)
                        case mbBody of
                            Nothing -> pure current
                            Just body ->
                                pure
                                    current
                                        { siScheme =
                                            mkElabSchemeWithRefs
                                                (schemeBinderRefs currentScheme)
                                                body
                                        }
                    [(_, mbBound)] -> do
                        completedBound <- completeCarrierBound mbBound
                        pure
                            current
                                { siScheme =
                                    mkElabSchemeWithRefs
                                        [ if typeBinderRefsSameIdentity
                                                ref
                                                carrierRef
                                            then (ref, Just completedBound)
                                            else binder
                                        | binder@(ref, _) <-
                                            schemeBinderRefs currentScheme
                                        ]
                                        (schemeBody currentScheme)
                                }
                    matches ->
                        carrierFailure
                            "opaque carrier has duplicate outer declarations"
                            ["  declarations: " ++ show matches]
              where
                currentScheme = siScheme current
                matchingOuterBinders =
                    filter
                        (typeBinderRefsSameIdentity carrierRef . fst)
                        (schemeBinderRefs currentScheme)

                closeLeadingBinder ty =
                    case ty of
                        TForallRef ref mbBound body
                            | typeBinderRefsSameIdentity ref carrierRef -> do
                                completedBound <- completeCarrierBound mbBound
                                pure
                                    ( Just
                                        ( TForallRef
                                            ref
                                            (Just completedBound)
                                            body
                                        )
                                    )
                            | otherwise -> do
                                mbBody <- closeLeadingBinder body
                                pure
                                    ( TForallRef ref mbBound
                                        <$> mbBody
                                    )
                        _ -> pure Nothing

                completeCarrierBound mbBound = do
                    currentBound <-
                        case mbBound of
                            Nothing ->
                                carrierFailure
                                    "opaque carrier was generalized without its opened packet bound"
                                    []
                            Just bound -> pure (tyToElab bound)
                    unless
                        ( operationalEndpointTypesAgree
                            packetType
                            currentBound
                            || exactIdentityForallClosureOf
                                packetType
                                currentBound
                        )
                        ( carrierFailure
                            "opaque carrier bound is not the certified opened descendant endpoint"
                            ["  current bound: " ++ show currentBound]
                        )
                    case elabToBound packetType of
                        Right bound -> pure bound
                        Left cause ->
                            carrierFailure
                                "opaque descendant construction is not a legal carrier bound"
                                ["  cause: " ++ cause]

                carrierFailure
                    :: String
                    -> [String]
                    -> Either ElabError a
                carrierFailure detail context =
                    Left
                        ( ValidationFailed
                            ( [ "cannot close an opaque descendant carrier declaration"
                              , "  detail: " ++ detail
                              , "  carrier: " ++ show carrierRef
                              , "  constructed type: " ++ show packetType
                              , "  enclosing scheme: " ++ show currentScheme
                              ]
                                ++ context
                            )
                        )

        validateSourceLambdaConstruction (carrierRef, packetType, packet) =
            case subtermGeneralizationSourceLambdaResultConstruction packet of
                Just (lambdaNode, constructedType)
                    | typeBinderRefNode carrierRef == Just lambdaNode
                    , constructedType == packetType -> pure ()
                authority ->
                    Left
                        ( ValidationFailed
                            [ "opaque descendant carrier lacks its exact source-lambda construction"
                            , "  carrier: " ++ show carrierRef
                            , "  constructed type: " ++ show packetType
                            , "  authority: " ++ show authority
                            ]
                        )

        composeOne current (carrierRef, packetType, packet)
            | not
                ( any
                    (typeBinderRefsSameIdentity carrierRef)
                    ( freeTypeVarRefsType
                        (schemeToType (siScheme current))
                    )
                ) = pure current
            | any
                (typeBinderRefsSameIdentity carrierRef . fst)
                (schemeBinderRefs (siScheme current)) =
                    Left
                        ( ValidationFailed
                            [ "opaque descendant result carrier was generalized as a local binder"
                            , "  carrier: " ++ show carrierRef
                            , "  enclosing scheme: "
                                ++ show (siScheme current)
                            ]
                        )
            | any
                (typeBinderRefsSameIdentity carrierRef)
                (freeTypeVarRefsType packetType) =
                    Left
                        ( ValidationFailed
                            [ "opaque descendant result packet recursively retains its carrier"
                            , "  carrier: " ++ show carrierRef
                            , "  packet type: " ++ show packetType
                            ]
                        )
            | otherwise = do
                mergedSubst <-
                    mergeExactRouteMaps
                        "opaque descendant substitution"
                        (dropCarrierRoutes (siSubstRefs current))
                        (packetConstructionRoutes (siSubstRefs packetInfo))
                mergedSourceOrder <-
                    mergeExactRouteMaps
                        "opaque descendant source-order routes"
                        (dropCarrierRoutes (siSourceBinderOrderRefs current))
                        ( packetConstructionRoutes
                            (siSourceBinderOrderRefs packetInfo)
                        )
                mergedConstructionOrder <-
                    mergeExactRouteMaps
                        "opaque descendant construction-order routes"
                        ( dropCarrierRoutes
                            (siConstructionBinderOrderRefs current)
                        )
                        ( packetConstructionRoutes
                            (siConstructionBinderOrderRefs packetInfo)
                        )
                let replaceCarrier =
                        substTypeCaptureRef carrierRef packetType
                    packetDeclaredRefs =
                        typeBinderDeclarationRefs packetType
                    retainedCurrentBinders =
                        [ binder
                        | binder@(ref, _) <-
                            schemeBinderRefs (siScheme current)
                        , not
                            ( any
                                (typeBinderRefsSameIdentity ref)
                                packetDeclaredRefs
                            )
                        ]
                    completedScheme =
                        mkElabSchemeWithRefs
                            [ ( ref
                              , fmap
                                    (mapBoundType replaceCarrier)
                                    mbBound
                              )
                            | (ref, mbBound) <- retainedCurrentBinders
                            ]
                            (replaceCarrier (schemeBody (siScheme current)))
                    rebuilt =
                        schemeInfoFromRefSubst
                            completedScheme
                            mergedSubst
                    retainOrder orderRefs =
                        IntMap.mapMaybeWithKey
                            ( \nodeKey orderRef ->
                                case
                                    IntMap.lookup
                                        nodeKey
                                        (siSubstRefs rebuilt)
                                of
                                    Just rebuiltRef
                                        | typeBinderRefsSameIdentity
                                            orderRef
                                            rebuiltRef ->
                                                Just rebuiltRef
                                    _ -> Nothing
                            )
                            orderRefs
                pure
                    rebuilt
                        { siSourceBinderOrderRefs =
                            retainOrder mergedSourceOrder
                        , siConstructionBinderOrderRefs =
                            retainOrder mergedConstructionOrder
                        }
          where
            packetInfo = subtermGeneralizationSchemeInfo packet
            packetTypeRefs =
                typeBinderDeclarationRefs packetType
                    ++ freeTypeVarRefsType packetType
            carrierKey = getNodeId <$> typeBinderRefNode carrierRef
            dropCarrierRoutes =
                IntMap.filterWithKey
                    ( \nodeKey routedRef ->
                        Just nodeKey /= carrierKey
                            && not
                                ( typeBinderRefsSameIdentity
                                    routedRef
                                    carrierRef
                                )
                    )
            packetConstructionRoutes =
                IntMap.filter
                    ( \routedRef ->
                        any
                            (typeBinderRefsSameIdentity routedRef)
                            packetTypeRefs
                    )
                    . dropCarrierRoutes

        mergeExactRouteMaps role left right =
            foldM insertRoute left (IntMap.toList right)
          where
            insertRoute routes (nodeKey, incomingRef) =
                case IntMap.lookup nodeKey routes of
                    Nothing ->
                        pure (IntMap.insert nodeKey incomingRef routes)
                    Just existingRef
                        | typeBinderRefsSameIdentity
                            existingRef
                            incomingRef ->
                                pure routes
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ role
                                        ++ " disagree at one graph key"
                                    , "  graph key: " ++ show nodeKey
                                    , "  enclosing route: "
                                        ++ show existingRef
                                    , "  descendant route: "
                                        ++ show incomingRef
                                    ]
                                )

    -- A packet may mention binders introduced by an enclosing source forall
    -- or lambda.  Preserve both the semantic source identity and every graph
    -- occurrence that projects to it: reification can legitimately choose
    -- either domain before source-identity projection runs.
    packetAmbientBinderRefs availableSourceBinderRefs mbExpected mbEnclosingParam =
        distinctTypeBinderRefs
            ( expectedRefs
                ++ expectedGraphRefs
                ++ enclosingParamRefs
            )
      where
        expectedRefs =
            maybe [] (map fst . packetExpectedAmbientBinders) mbExpected
        expectedGraphRefs =
            [ graphRefForKey nodeKey
            | (nodeKey, sourceRef) <- IntMap.toList availableSourceBinderRefs
            , any (typeBinderRefsSameIdentity sourceRef) expectedRefs
            ]
        enclosingParamRefs =
            case mbEnclosingParam of
                Nothing -> []
                Just paramNode ->
                    graphRefForKey (getNodeId paramNode)
                        : maybeToList
                            (IntMap.lookup (getNodeId paramNode) availableSourceBinderRefs)
        graphRefForKey nodeKey =
            typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId nodeKey))
                ("t" ++ show nodeKey)

    -- A descendant packet can use a source existential only in the bound it
    -- contributes to its enclosing Gamma.  That dependency is absent from
    -- the parent's result graph, so publish its exact graph occurrence to
    -- Gen as a term-used root before constructing the parent scheme.  This
    -- lets ordinary binder planning place the existential outside the
    -- consumer declaration; placement never has to manufacture a forall
    -- after the fact.
    packetTermUsedBinderRefs availableSourceBinderRefs ambientRefs packets =
        distinctTypeBinderRefs
            ( directGraphRefs ++ sourceOccurrenceGraphRefs )
      where
        packetFreeRefs = packetGammaFreeRefs packets
        directGraphRefs =
            [ freeRef
            | freeRef <- packetFreeRefs
            , isJust (typeBinderRefNode freeRef)
            , not (hasAmbientAuthority freeRef)
            ]
        sourceOccurrenceGraphRefs =
            [ graphRefForKey nodeKey
            | (nodeKey, sourceRef) <-
                IntMap.toList availableSourceBinderRefs
            , any (typeBinderRefsSameIdentity sourceRef) packetFreeRefs
            , not (hasAmbientAuthority sourceRef)
            ]
        hasAmbientAuthority ref =
            any (typeBinderRefsSameIdentity ref) ambientRefs
        graphRefForKey nodeKey =
            typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (NodeId nodeKey))
                ("t" ++ show nodeKey)

    packetTermUsedBinderIdentities availableSourceBinderRefs ambientRefs packets =
        Set.fromList
            ( map
                typeBinderRefIdentity
                ( packetTermUsedBinderRefs
                    availableSourceBinderRefs
                    ambientRefs
                    packets
                )
                ++ [ typeBinderRefIdentity sourceRef
                   | sourceRef <- IntMap.elems availableSourceBinderRefs
                   , any
                        (typeBinderRefsSameIdentity sourceRef)
                        (packetGammaFreeRefs packets)
                   , not
                        ( any
                            (typeBinderRefsSameIdentity sourceRef)
                            ambientRefs
                        )
                   ]
            )

    packetGammaFreeRefs =
        concatMap
            ( freeTypeVarRefsType
                . schemeToType
                . subtermGeneralizationGammaBoundScheme
            )
            . Map.elems

    refForNode subst node =
        IntMap.lookup (getNodeId node) subst
            <|> find
                (\ref -> typeBinderRefNode ref == Just node)
                (IntMap.elems subst)

preparedAnnotated :: PreparedGeneralizationArtifact -> AnnExpr
preparedAnnotated = pgaAnnotated

-- | Select the canonical root that was sealed during preparation.  Compare the
-- complete source tree and return the stored capability; never canonicalize a
-- new tree against an existing authority after the fact.
authorizePreparedAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError AuthorizedElaborationRoot
authorizePreparedAnn artifact sourceAnn =
    case
        find
            ((== sourceAnn) . fst)
            (pgaAuthorizedElaborationRoots artifact)
    of
        Just (_, authorizedRoot) -> Right authorizedRoot
        Nothing ->
            Left
                ( ValidationFailed
                    [ "annotated root does not belong to the prepared edge authority"
                    , "  root: " ++ show sourceAnn
                    ]
                )

-- | Restrict occurrence-sensitive construction scopes to one prepared root.
-- Multi-root module checking shares solved type representatives across roots,
-- but lexical Gamma ownership does not cross a root boundary.  Selecting the
-- root's scope authority before construction prevents a canonical base node
-- (for example the built-in String node) from combining unrelated let scopes.
selectPreparedRootScopeAuthority
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError PreparedGeneralizationArtifact
selectPreparedRootScopeAuthority artifact sourceAnn =
    case
        [ scopes
        | (preparedAnn, scopes) <- pgaRootScopeOverrides artifact
        , preparedAnn == sourceAnn
        ]
    of
        [scopes] ->
            pure artifact {pgaScopeOverrides = scopes}
        [] ->
            Left
                ( ValidationFailed
                    [ "annotated root has no prepared construction-scope authority"
                    , "  root: " ++ show sourceAnn
                    ]
                )
        scopes ->
            Left
                ( ValidationFailed
                    [ "annotated root has multiple prepared construction-scope authorities"
                    , "  root: " ++ show sourceAnn
                    , "  authorities: " ++ show scopes
                    ]
                )

preparedReadContextReady :: PreparedGeneralizationArtifact -> Either ElabError ()
preparedReadContextReady artifact = do
    _ <- pgaReadModel artifact
    _ <- pgaBaseReadModel artifact
    pure ()

preparedResultTypeViewReady :: PreparedGeneralizationArtifact -> Either ElabError ()
preparedResultTypeViewReady artifact = do
    _ <- pgaResultTypeView artifact
    pure ()

preparedIdentityGenerator
    :: PreparedGeneralizationArtifact
    -> Either ElabError IdentityGenerator
preparedIdentityGenerator = pgaIdentityGenerator

-- | Extend the root substitution with every free producer reference whose
-- source identity was fixed during preparation.  Root generalization only
-- needs entries that occur in its scheme; the producer term can additionally
-- contain graph aliases in lambda parameter types.  Merge those aliases here,
-- before closure/exact validation.  A root-local graph placeholder at the
-- exact node of a direct source declaration is projected to that source
-- identity; every other disagreement remains an invariant failure.
applyPreparedTermSourceBinderAliases
    :: PreparedGeneralizationArtifact
    -> PreparedRootGeneralization
    -> XmlfTerm
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
applyPreparedTermSourceBinderAliases artifact rootGeneralization term =
    foldM
        ( insertPreparedTermSourceBinderAlias
            (prgConstructedGammaIdentities rootGeneralization)
            (prgDirectSourceBinderKeys rootGeneralization)
            rootSourceBinderRefs
        )
        (prgSubst rootGeneralization)
        (IntMap.toList sourceAliases)
  where
    rootSourceBinderRefs = prgSourceBinderRefs rootGeneralization
    sourceAliases =
        sourceBinderAliasSubstitution
            (pgaCanonical artifact)
            rootSourceBinderRefs
            (Reduce.freeTypeVarRefsTerm term)

-- | Publish the source-result quotient prepared for one compiler-exact edge.
-- The packet result is the binder emitted by the checked source constructor;
-- the source result is the exact annotation binder selected at that result
-- position.  Keeping this route explicit lets root closure reuse that
-- construction without guessing from quantifier position or bound shape.
preparedCompilerExactSourceResultBinderRoutes
    :: PreparedGeneralizationArtifact
    -> EdgeId
    -> Either ElabError [(TypeBinderRef, TypeBinderRef)]
preparedCompilerExactSourceResultBinderRoutes artifact exactEdge = do
    packets <- pgaSubtermGeneralizations artifact
    let matchingPackets =
            [ packet
            | packet <- Map.elems packets
            , subtermGeneralizationCompilerExactBoundary packet
                == Just exactEdge
            , subtermGeneralizationCompilerExactResultStage packet
                == Just CompleteBeforeCompilerExact
            ]
    foldM insertRoute [] matchingPackets
  where
    insertRoute routes packet =
        case
            ( subtermGeneralizationCompilerExactResultRef packet
            , subtermGeneralizationCompilerExactExistingRef packet
            )
        of
            (Just packetResultRef, Just sourceResultRef) ->
                case
                    find
                        ( typeBinderRefsSameIdentity packetResultRef
                            . fst
                        )
                        routes
                of
                    Nothing ->
                        pure ((packetResultRef, sourceResultRef) : routes)
                    Just (_, existingSourceRef)
                        | typeBinderRefsSameIdentity
                            existingSourceRef
                            sourceResultRef ->
                            pure routes
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "compiler exact packet result has conflicting source routes"
                                    , "  exact edge: " ++ show exactEdge
                                    , "  packet result: " ++ show packetResultRef
                                    , "  first source result: "
                                        ++ show existingSourceRef
                                    , "  second source result: "
                                        ++ show sourceResultRef
                                    ]
                                )
            _ ->
                Left
                    ( ValidationFailed
                        [ "source-owned compiler exact packet has no complete result route"
                        , "  exact edge: " ++ show exactEdge
                        , "  packet: " ++ show packet
                        ]
                    )


-- | Merge one free term alias into the root substitution.  A conflicting
-- graph placeholder may adopt a generated source identity either at the exact
-- key of a direct source declaration or when the free term occurrence and the
-- root substitution name the same graph identity.  The latter is the missing
-- construction bridge for a solved/copy occurrence: the expanded sidecar
-- alone is not declaration authority, but the checked producer occurrence
-- proves that this root binder closes that exact source occurrence.
--
-- If Figure 15.3.5 has already constructed the root binder, that construction
-- still wins over an expanded source alias: there the alias proves only that
-- the source occurrence can feed the fresh consumer.
insertPreparedTermSourceBinderAlias
    :: Set.Set TypeBinderIdentity
    -> IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> (Int, TypeBinderRef)
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
insertPreparedTermSourceBinderAlias protectedIdentities directSourceKeys sourceBinderRefs subst (nodeKey, sourceRef) =
    case IntMap.lookup nodeKey subst of
        Nothing -> pure (IntMap.insert nodeKey sourceRef subst)
        Just existing
            | typeBinderRefsSameIdentity existing sourceRef -> pure subst
            | typeBinderRefNode existing == Just (NodeId nodeKey)
            , IntSet.member nodeKey directSourceKeys
            , Just directSourceRef <- IntMap.lookup nodeKey sourceBinderRefs
            , typeBinderRefsSameIdentity directSourceRef sourceRef
            , isJust
                ( typeBinderIdentityGeneratedUnique
                    (typeBinderRefIdentity sourceRef)
                ) ->
                pure (IntMap.insert nodeKey sourceRef subst)
            | Set.member
                (typeBinderRefIdentity existing)
                protectedIdentities ->
                pure subst
            | typeBinderRefNode existing == Just (NodeId nodeKey)
            , Just routedSourceRef <- IntMap.lookup nodeKey sourceBinderRefs
            , typeBinderRefsSameIdentity routedSourceRef sourceRef
            , isJust
                ( typeBinderIdentityGeneratedUnique
                    (typeBinderRefIdentity sourceRef)
                ) ->
                pure (IntMap.insert nodeKey sourceRef subst)
            | otherwise ->
                Left
                    ( ValidationFailed
                        [ "prepared root and source-binder substitutions disagree"
                        , "  graph node: " ++ show (NodeId nodeKey)
                        , "  root binder: " ++ show existing
                        , "  source binder: " ++ show sourceRef
                        , "  direct source keys: "
                            ++ show (IntSet.toList directSourceKeys)
                        , "  protected identities: "
                            ++ show (Set.toList protectedIdentities)
                        ]
                    )

-- | Consume the delayed Gamma result actions owned by one compiler-exact
-- boundary.  The prepared artifact keeps packet selection and provenance
-- private; root finalization receives only the term construction operation.
completePreparedCompilerExactSubtermResults
    :: PreparedGeneralizationArtifact
    -> [CompilerExactResultBoundCertificate]
    -> CompilerExactResultStage
    -> EdgeId
    -> XmlfTerm
    -> Either ElabError XmlfTerm
completePreparedCompilerExactSubtermResults artifact resultBoundCertificates stage exactEdge term = do
    packets <- pgaSubtermGeneralizations artifact
    completeCompilerExactSubtermResultsWithBounds
        resultBoundCertificates
        stage
        exactEdge
        packets
        term

-- | Resolve one root compiler-exact contract through the same edge-scoped
-- graph-to-source identity proof used by its binder plan.  The prepared
-- producer-type table is the canonical contract authority; re-reading a
-- precanonical annotation here could recreate graph identities that were
-- already projected to semantic source identities during preparation.
preparedCompilerExactExpectedType
    :: PreparedGeneralizationArtifact
    -> EdgeId
    -> Either ElabError ElabType
preparedCompilerExactExpectedType artifact exactEdge = do
    plansByEdge <- pgaCompilerExactEdgePlans artifact
    case IntMap.lookup (getEdgeId exactEdge) plansByEdge of
        Just plan -> pure (ceepExpectedType plan)
        Nothing ->
            Left
                ( ValidationFailed
                    [ "prepared compiler-exact edge has no edge-local plan"
                    , "  edge: " ++ show exactEdge
                    ]
                )

-- | Return the exact edge's occurrence-sensitive declaration provenance.
-- Unlike construction refs, this map must not be installed as a Gamma alias;
-- its graph key is consumed only after the boundary has identified the
-- corresponding recursive declaration in the checked producer type.
preparedCompilerExactDeclarationRefs
    :: PreparedGeneralizationArtifact
    -> EdgeId
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
preparedCompilerExactDeclarationRefs artifact exactEdge = do
    plansByEdge <- pgaCompilerExactEdgePlans artifact
    case IntMap.lookup (getEdgeId exactEdge) plansByEdge of
        Just plan -> pure (ceepDeclarationRefs plan)
        Nothing ->
            Left
                ( ValidationFailed
                    [ "prepared compiler-exact edge has no declaration plan"
                    , "  edge: " ++ show exactEdge
                    ]
                )

-- | Apply only the exact-contract side of a compiler-exact identity proof.
-- Unlike a free-reference substitution, this quotient renames lexical forall
-- and mu declarations together with their bounds and occurrences.  The route
-- is keyed by full binder identity, never by the numeric node id of the
-- current presolution graph.
applyCompilerExactBinderQuotient
    :: Map.Map TypeBinderIdentity TypeBinderRef
    -> ElabType
    -> Either ElabError ElabType
applyCompilerExactBinderQuotient exactBinderRefs ty = do
    let renamed = renameCompilerExactBinderRefs exactBinderRefs ty
    schemeToType
        <$> quotientPreparedBinderIdentities
            "compiler-exact contract"
            (schemeFromType renamed)

renameCompilerExactBinderRefs
    :: Map.Map TypeBinderIdentity TypeBinderRef
    -> ElabType
    -> ElabType
renameCompilerExactBinderRefs exactBinderRefs = renameType
  where
    renameRef ref =
        Map.findWithDefault
            ref
            (typeBinderRefIdentity ref)
            exactBinderRefs

    renameType current =
        case current of
            TVarRef ref -> TVarRef (renameRef ref)
            TArrow domain codomain ->
                TArrow (renameType domain) (renameType codomain)
            TConWithIdentity identity constructor args ->
                TConWithIdentity identity constructor (fmap renameType args)
            TVarAppRef ref args ->
                TVarAppRef (renameRef ref) (fmap renameType args)
            TBaseWithIdentity identity base ->
                TBaseWithIdentity identity base
            TForallRef ref mbBound body ->
                TForallRef
                    (renameRef ref)
                    (fmap (mapBoundType renameType) mbBound)
                    (renameType body)
            TMuRef ref body ->
                TMuRef (renameRef ref) (renameType body)
            TBottom -> TBottom

preparedIdentityRepresentative
    :: PreparedGeneralizationArtifact
    -> NodeId
    -> NodeId
preparedIdentityRepresentative artifact node =
    case
        resolveGaSolvedToBase
            (pgaBindParentsGa artifact)
            (pgaAnnNodeCanonical artifact node)
    of
        SolvedToBaseMapped baseNode -> baseNode
        SolvedToBaseSameDomain baseNode -> baseNode
        SolvedToBaseMissing -> pgaAnnNodeCanonical artifact node

preparedElaborationConfig :: TraceConfig -> PreparedGeneralizationArtifact -> ElabConfig 'Presolved
preparedElaborationConfig traceCfg artifact =
    ElabConfig
        { ecTraceConfig = traceCfg
        , ecGeneralizeAtWith =
            \mbGa scope target ->
                generalizeAtWithBuilderRequired
                    (pgaPlanBuilder artifact)
                    GeneralizationRequirements
                        { grRequiredGammaBinders = []
                        , grSourceBinderRefs = pgaSourceBinderRefs artifact
                        , grAmbientBinderRefs = []
                        , grTermUsedRootBinderRefs = []
                        , grAmbientGammaAuthorities = IntMap.empty
                        , grLocallyClosedGammaNodes = IntSet.empty
                        }
                    mbGa
                    (pgaPresolutionView artifact)
                    scope
                    target
        , ecGeneralizeAtWithRequirements =
            \requirements mbGa scope target ->
                generalizeAtWithBuilderRequired
                    (pgaPlanBuilder artifact)
                    requirements
                    mbGa
                    (pgaPresolutionView artifact)
                    scope
                    target
        , ecGeneralizeAtWithResultCertificate =
            \request requirements mbGa scope target ->
                generalizeAtWithBuilderRequiredResultCertified
                    (pgaPlanBuilder artifact)
                    request
                    requirements
                    mbGa
                    (pgaPresolutionView artifact)
                    scope
                    target
        }

-- | Index source annotation authority by the edge that owns the coercion.
-- Graph solving is allowed to identify two annotation result nodes, so a
-- canonical-node map cannot retain occurrence-specific source types.  Edge
-- identities remain stable in the annotated term and are the construction
-- key consumed by annotation elaboration.
prepareAnnotationExpectedTypesByEdge
    :: IntMap.IntMap ElabType
    -> [AnnExpr]
    -> Either ElabError (IntMap.IntMap ElabType, IntSet.IntSet)
prepareAnnotationExpectedTypesByEdge expectedTypes =
    foldM collectRoot (IntMap.empty, IntSet.empty)
  where
    collectRoot acc ann = collect ann acc

    collect ann acc =
        case ann of
            AResolvedVar {} -> pure acc
            ALit {} -> pure acc
            ALam _ _ _ _ body _ _ -> collect body acc
            AApp fun arg _ _ _ -> do
                afterFun <- collect fun acc
                collect arg afterFun
            ALet _ _ _ _ _ _ rhs body _ -> do
                afterRhs <- collect rhs acc
                collect body afterRhs
            -- Both source kappa annotations and compiler-exact annotations
            -- carry a source-facing expected type at their own edge.
            AExactAnn inner _ sourceNode edgeId ->
                collectAnnotation inner sourceNode edgeId acc
            AAnn inner sourceNode edgeId ->
                collectAnnotation inner sourceNode edgeId acc
            ALetScope inner _ _ -> collect inner acc
            AUnfold inner _ _ -> collect inner acc

    collectAnnotation inner sourceNode edgeId (typesByEdge, sourceNodeKeys) = do
        expectedType <-
            case IntMap.lookup (getNodeId sourceNode) expectedTypes of
                Just ty -> pure ty
                Nothing ->
                    Left
                        ( ValidationFailed
                            [ "missing identity-bearing source annotation type"
                            , "  edge: " ++ show edgeId
                            , "  source node: " ++ show sourceNode
                            ]
                        )
        let edgeKey = getEdgeId edgeId
        typesByEdge' <-
            case IntMap.lookup edgeKey typesByEdge of
                Nothing ->
                    pure (IntMap.insert edgeKey expectedType typesByEdge)
                Just existingType ->
                    Left
                        ( ValidationFailed
                            [ "one annotation edge is owned by multiple source occurrences"
                            , "  edge: " ++ show edgeId
                            , "  first expected type: " ++ show existingType
                            , "  second expected type: " ++ show expectedType
                            ]
                        )
        collect
            inner
            ( typesByEdge'
            , IntSet.insert (getNodeId sourceNode) sourceNodeKeys
            )

preparedElaborationEnv
    :: IntMap.IntMap NormSrcType
    -> Map.Map VarName SchemeInfo
    -> PreparedGeneralizationArtifact
    -> ElabEnv 'Presolved
preparedElaborationEnv annSourceTypes initialTermEnv artifact =
    preparedElaborationEnvWithInitialEnv annSourceTypes (mkEnv initialTermEnv) artifact

preparedElaborationEnvWithInitialEnv
    :: IntMap.IntMap NormSrcType
    -> Env
    -> PreparedGeneralizationArtifact
    -> ElabEnv 'Presolved
preparedElaborationEnvWithInitialEnv annSourceTypes initialTermEnv artifact =
    ElabEnv
        { eePresolutionView = pgaPresolutionView artifact
        , eeCanonical = pgaAnnNodeCanonical artifact
        , eeReadModel = pgaReadModel artifact
        , eeGaParents = pgaBindParentsGa artifact
        , eeExactProducerTypes = pgaExactProducerTypes artifact
        , eeCompilerExactConstructionRefs =
            IntMap.map ceepConstructionRefs
                <$> pgaCompilerExactEdgePlans artifact
        , eeCompilerExactDeclarationRefs =
            IntMap.map ceepDeclarationRefs
                <$> pgaCompilerExactEdgePlans artifact
        , eeScopeOverrides = pgaScopeOverrides artifact
        , eeExactLambdaParamSourceTypes =
            preparedExactLambdaParamSourceTypes artifact annSourceTypes
        , eeSourceTypeHeadIdentities = Map.empty
        , eeSourceTypeBinderIdentities = Map.empty
        , eeSourceBinderRefs = pgaSourceBinderRefs artifact
        , eeDirectSourceBinderKeys = pgaDirectSourceBinderKeys artifact
        , eeSubtermGeneralizations = pgaSubtermGeneralizations artifact
        , eeInitialTermEnv = initialTermEnv
        }

-- | Retain only source types attached to compiler-owned exact lambda
-- parameter nodes.  These are occurrence authorities: quotient-equivalent
-- nodes from another lexical scope must not acquire the parameter's source
-- type merely because solving merged their graph classes.
preparedExactLambdaParamSourceTypes
    :: PreparedGeneralizationArtifact
    -> IntMap.IntMap NormSrcType
    -> IntMap.IntMap NormSrcType
preparedExactLambdaParamSourceTypes artifact annSourceTypes =
    IntMap.withoutKeys
        annSourceTypes
        (pgaAnnotationSourceNodeKeys artifact)

stripPreparedWitnesslessAuthoritativeAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> (AnnExpr, AnnExpr)
stripPreparedWitnesslessAuthoritativeAnn artifact =
    stripWitnesslessAuthoritativeAnnWith
        (eaEdgeWitnesses (preparedEdgeArtifacts artifact))

-- | Publish the exact endpoint of a resolved application argument before root
-- Gamma planning. Both the function and argument schemes come from their
-- resolved identities; the function's closed source domain selects the
-- endpoint and the argument's complete checked forall instantiation proves
-- that this occurrence constructs it. A domain that still depends on a
-- function-source binder (the paper's @g g@ case) is deliberately left to the
-- ordinary application construction.
preparedExactApplicationArgumentEdges
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError [(EdgeId, Maybe ElabType)]
preparedExactApplicationArgumentEdges artifact = collect
  where
    collect ann =
        case ann of
            AResolvedVar{} -> pure []
            ALit{} -> pure []
            ALam _ _ _ _ body _ _ -> collect body
            AApp fun arg _ argSite _ -> do
                here <-
                    case exactArgumentEndpoint fun arg of
                        Just endpoint ->
                            pure
                                [ ( instantiationSiteEdgeId argSite
                                  , Just endpoint
                                  )
                                ]
                        Nothing -> pure []
                funEdges <- collect fun
                argEdges <- collect arg
                mergeExactEdges (here ++ funEdges ++ argEdges)
            ALet _ _ _ _ _ _ rhs body _ -> do
                rhsEdges <- collect rhs
                bodyEdges <- collect body
                mergeExactEdges (rhsEdges ++ bodyEdges)
            AExactAnn inner _ _ _ -> collect inner
            AAnn inner _ _ -> collect inner
            ALetScope inner _ _ -> collect inner
            AUnfold inner _ _ -> collect inner

    exactArgumentEndpoint fun arg = do
        functionSchemeInfo <- resolvedOccurrenceScheme fun
        argumentSchemeInfo <- resolvedOccurrenceScheme arg
        resolvedSourceApplicationArgumentEndpoint
            TypeCheck.emptyEnv
            functionSchemeInfo
            argumentSchemeInfo

    resolvedOccurrenceScheme ann = do
        details <- resolvedOccurrenceDetails ann
        Map.lookup
            (idDetailsIdentityKey details)
            (pgaResolvedTermSchemes artifact)

    resolvedOccurrenceDetails ann =
        case ann of
            AResolvedVar details _ _ -> Just details
            AExactAnn inner _ _ _ -> resolvedOccurrenceDetails inner
            AAnn inner _ _ -> resolvedOccurrenceDetails inner
            ALetScope inner _ _ -> resolvedOccurrenceDetails inner
            AUnfold inner _ _ -> resolvedOccurrenceDetails inner
            _ -> Nothing

    mergeExactEdges =
        fmap IntMap.elems
            . foldM insertExactEdge IntMap.empty

    insertExactEdge edges requirement@(edgeId, mbEndpoint) =
        case IntMap.lookup (getEdgeId edgeId) edges of
            Nothing ->
                pure
                    ( IntMap.insert
                        (getEdgeId edgeId)
                        requirement
                        edges
                    )
            Just (_, existingEndpoint)
                | endpointsAgree existingEndpoint mbEndpoint -> pure edges
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "resolved application edge has conflicting exact endpoints"
                            , "  edge: " ++ show edgeId
                            , "  first endpoint: " ++ show existingEndpoint
                            , "  second endpoint: " ++ show mbEndpoint
                            ]
                        )

    endpointsAgree Nothing Nothing = True
    endpointsAgree (Just left) (Just right) =
        alphaEqType left right || churchAwareEqType left right
    endpointsAgree _ _ = False

-- | Reuse a complete local Figure 15.3.5 construction instead of asking the
-- root binder planner to rediscover it through the already-merged result
-- node.  Every requirement must be certified local and every owning closure
-- must publish its pending scheme; a mixed or incomplete set deliberately
-- falls back to ordinary root planning.
preparedLocalGammaConstructionSeed
    :: GaBindParents 'Presolved
    -> IntMap.IntMap LocalGammaClosure
    -> GeneralizationRequirements
    -> Either ElabError (Maybe SchemeInfo)
preparedLocalGammaConstructionSeed ga closures requirements
    | all
        requirementIsNested
        requiredBinders = do
        mbSchemes <- traverse pendingSchemeFor requiredBinders
        case sequence mbSchemes of
            Just schemes@(_ : _) -> Just <$> mergePendingSchemes schemes
            _ -> pure Nothing
    | otherwise = pure Nothing
  where
    requiredBinders = grRequiredGammaBinders requirements

    requirementIsNested requirement =
        case rgbPlacement requirement of
            RequiredGammaAtNestedScope _ -> True
            _ -> False

    pendingSchemeFor requirement = do
        closedLocally <-
            requiredGammaBinderClosedLocally
                ga
                closures
                requirement
        if not closedLocally
            then pure Nothing
            else
                case requirementClosures requirement of
                    [] -> pure Nothing
                    firstClosure : remainingClosures
                        | all (== firstClosure) remainingClosures
                        , lgcExteriorNode firstClosure
                            == rgbExteriorNode requirement ->
                            pure (lgcOwnerPendingScheme firstClosure)
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "one locally constructed Gamma requirement has conflicting owner closures"
                                    , "  requirement: " ++ show requirement
                                    , "  closures: "
                                        ++ show
                                            ( firstClosure
                                                : remainingClosures
                                            )
                                    ]
                                )

    requirementClosures requirement =
        [ closure
        | edgeId <- NonEmpty.toList (rgbEdgeIds requirement)
        , Just closure <- [IntMap.lookup (getEdgeId edgeId) closures]
        ]

    mergePendingSchemes schemes = do
        (binders, subst) <-
            foldM
                mergePendingScheme
                ([], IntMap.empty)
                schemes
        pure
            ( schemeInfoFromRefSubst
                (mkElabSchemeWithRefs binders TBottom)
                subst
            )

    mergePendingScheme (binders, subst) schemeInfo = do
        binders' <-
            foldM
                insertPendingBinder
                binders
                ( fst
                    ( splitForallsRefs
                        (schemeToType (siScheme schemeInfo))
                    )
                )
        subst' <-
            foldM
                insertPendingRoute
                subst
                (IntMap.toList (siSubstRefs schemeInfo))
        pure (binders', subst')

    insertPendingBinder binders incoming@(incomingRef, incomingBound) =
        case break (typeBinderRefsSameIdentity incomingRef . fst) binders of
            (_, []) -> pure (binders ++ [incoming])
            (_, (_, existingBound) : _)
                | existingBound == incomingBound -> pure binders
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "one local Gamma construction publishes conflicting bounds for one identity"
                            , "  binder: " ++ show incomingRef
                            , "  first bound: " ++ show existingBound
                            , "  second bound: " ++ show incomingBound
                            ]
                        )

    insertPendingRoute subst (nodeKey, incomingRef) =
        case IntMap.lookup nodeKey subst of
            Nothing -> pure (IntMap.insert nodeKey incomingRef subst)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef incomingRef ->
                    pure subst
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "local Gamma constructions disagree at one graph occurrence"
                            , "  graph key: " ++ show nodeKey
                            , "  first binder: " ++ show existingRef
                            , "  second binder: " ++ show incomingRef
                            ]
                        )

-- | Prepare only the Gamma needed while elaborating an ordinary root.  Unlike
-- 'generalizePreparedRootDetailed', this does not select the root result type,
-- compose descendant result packets, validate a final root RaiseMerge, or
-- produce a scheme that later closure may reuse.  Its target is one required
-- Gamma exterior, used solely as a stable anchor that lets the binder planner
-- order every required exterior and its bound dependencies.
--
-- Keeping this construction artifact separate is essential for ordinary
-- roots: their authoritative result annotation is known only after term
-- elaboration, while strict Hyp checking needs the edge-owned Gamma already in
-- scope during elaboration.
prepareOrdinaryRootConstructionScope
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError PreparedRootConstructionScope
prepareOrdinaryRootConstructionScope artifact authoritativeAnnCanon sourceScopeAnnPre = do
    subtermPackets <-
        case pgaSubtermGeneralizations artifact of
            Right packets -> pure packets
            Left cause ->
                Left
                    ( ValidationFailed
                        [ "ordinary root construction could not prepare subterm packets"
                        , "  root: " ++ show authoritativeAnnCanon
                        , "  cause: " ++ show cause
                        ]
                    )
    exactProducerTypes <- pgaExactProducerTypes artifact
    annotationConstructionBinders <-
        preparedTransparentRootSourceAnnotationBinders
            artifact
            authoritativeAnnCanon
    exactApplicationEdges <-
        preparedExactApplicationArgumentEdges
            artifact
            authoritativeAnnCanon
    sourceBinderRefs0 <-
        preparedSourceBinderRefsForAnn artifact authoritativeAnnCanon
    sourceAnnotationOccurrenceRefs <-
        preparedSourceAnnotationOccurrenceRefsForAnn
            artifact
            sourceScopeAnnPre
    let sourceBinderRefs =
            enterSourceAnnotationOccurrenceRefs
                sourceBinderRefs0
                sourceAnnotationOccurrenceRefs
    directSourceBinderKeys <-
        preparedDirectSourceBinderKeysForAnn artifact authoritativeAnnCanon
    rootScope <- preparedRootSchemeScope artifact (annNode sourceScopeAnnPre)
    let ownedSubtermPackets =
            subtermGeneralizationsOwnedBy authoritativeAnnCanon subtermPackets
    rootBoundary <-
        generalizationRequirementsForRootBoundary
            (preparedScopeRootForBoundary artifact)
            (preparedIdentityRepresentative artifact)
            (pgaAnnNodeCanonical artifact)
            (pgaBindParentsGa artifact)
            rootScope
            (pgaPresolutionView artifact)
            (preparedEdgeArtifacts artifact)
            exactProducerTypes
            sourceBinderRefs
            sourceBinderRefs
            subtermPackets
            ownedSubtermPackets
            exactApplicationEdges
            Nothing
            []
            authoritativeAnnCanon
            authoritativeAnnCanon
    let annotationConstructionRefs =
            map fst annotationConstructionBinders
        completeRequirements0 = rbrRequirements rootBoundary
        completeRequirements =
            completeRequirements0
                { grAmbientBinderRefs =
                    distinctTypeBinderRefs
                        ( annotationConstructionRefs
                            ++ grAmbientBinderRefs completeRequirements0
                        )
                }
    case grRequiredGammaBinders completeRequirements of
        [] ->
            pure
                emptyPreparedRootConstructionScope
                    { prcsLocallyClosedGammas =
                        rbrLocallyClosedGammas rootBoundary
                    }
        firstRequirement : _ -> do
            let constructionAnchor =
                    pgaAnnNodeCanonical artifact
                        (rgbExteriorNode firstRequirement)
            mbLocalConstructionSeed <-
                preparedLocalGammaConstructionSeed
                    (pgaBindParentsGa artifact)
                    (rbrLocallyClosedGammas rootBoundary)
                    completeRequirements
            ( constructionScheme
              , constructionSubst0
              , constructionInheritedGammaRoutes
              ) <-
                case mbLocalConstructionSeed of
                    Just localConstruction ->
                        pure
                            ( siScheme localConstruction
                            , siSubstRefs localConstruction
                            , Reify.emptyInheritedGammaRoutes
                            )
                    Nothing ->
                        case
                            generalizeAtWithBuilderRequiredCertified
                                (pgaPlanBuilder artifact)
                                completeRequirements
                                (Just (pgaBindParentsGa artifact))
                                (pgaPresolutionView artifact)
                                rootScope
                                constructionAnchor
                        of
                            Right result -> pure result
                            Left cause ->
                                Left
                                    ( ValidationFailed
                                        [ "ordinary root construction Gamma could not be planned"
                                        , "  root scope: " ++ show rootScope
                                        , "  construction anchor: " ++ show constructionAnchor
                                        , "  requirements: " ++ show completeRequirements
                                        , "  source annotation occurrence refs: "
                                            ++ show sourceAnnotationOccurrenceRefs
                                        , "  locally closed Gamma: "
                                            ++ show (rbrLocallyClosedGammas rootBoundary)
                                        , "  cause: " ++ show cause
                                        ]
                                    )
            constructionSubst <-
                projectPreparedSourceBinderSubstExceptWithLocalKeys
                    ( Set.fromList
                        [ typeBinderIdentityFromNode
                            (rgbExteriorNode requirement)
                        | requirement <-
                            grRequiredGammaBinders completeRequirements
                        ]
                    )
                    directSourceBinderKeys
                    ( IntSet.fromList
                        [ getNodeId (lgcExteriorNode closure)
                        | closure <-
                            IntMap.elems
                                (rbrLocallyClosedGammas rootBoundary)
                        ]
                    )
                    sourceBinderRefs
                    constructionSubst0
            constructionBinderRenames <-
                either
                    ( \cause ->
                        Left
                            ( ValidationFailed
                                [ "ordinary root construction has inconsistent source-binder provenance"
                                , "  cause: " ++ cause
                                ]
                            )
                    )
                    Right
                    ( sourceBinderConstructionRenames
                        (preparedIdentityRepresentative artifact)
                        sourceBinderRefs
                        constructionSubst0
                    )
            inheritedGammaRoutes <-
                Reify.mergeInheritedGammaRoutes
                    (rbrInheritedGammaRoutes rootBoundary)
                    constructionInheritedGammaRoutes
            constructionDependencySubst <-
                overlayInheritedGammaDependencyRoutes
                    inheritedGammaRoutes
                    constructionSubst
            constructionScope <-
                prepareRequiredRootConstructionScope
                    (pgaPresolutionView artifact)
                    (pgaBindParentsGa artifact)
                    annotationConstructionBinders
                    (rbrLocallyClosedGammas rootBoundary)
                    inheritedGammaRoutes
                    completeRequirements
                    constructionScheme
                    constructionDependencySubst
            pure
                constructionScope
                    { prcsBinderRenames =
                        constructionBinderRenames
                    }

generalizePreparedRoot
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
generalizePreparedRoot artifact authoritativeAnnCanon authoritativeAnnPre = do
    detailed <- generalizePreparedRootDetailed artifact authoritativeAnnCanon authoritativeAnnPre
    pure (prgScheme detailed, prgSubst detailed)

generalizePreparedRootDetailed
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError PreparedRootGeneralization
generalizePreparedRootDetailed artifact authoritativeAnnCanon sourceScopeAnnPre =
    generalizePreparedRootDetailedWithConstructionAnn
        artifact
        authoritativeAnnCanon
        sourceScopeAnnPre
        authoritativeAnnCanon

-- | Build the final root scheme with construction and result authority kept
-- distinct.  The original root owns the lexical scope and every edge Gamma
-- not already closed by a local application construction.  The post-
-- elaboration authoritative result owns only result selection, packet
-- placement, and root RaiseMerge validation.  This prevents a projected result
-- annotation from dropping construction Gamma, without duplicating an
-- application's local 'ETyAbsRef' binders or letting the original
-- pre-elaboration shape dictate the final result scheme.
generalizePreparedRootDetailedWithConstructionAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError PreparedRootGeneralization
generalizePreparedRootDetailedWithConstructionAnn artifact constructionAnnCanon constructionAnnPre authoritativeResultAnnCanon =
    generalizePreparedRootDetailedWithConstructionResult
        artifact
        constructionAnnCanon
        constructionAnnPre
        authoritativeResultAnnCanon
        Nothing
        []

-- | Complete an enclosing-root RaiseMerge requirement from the exact checked
-- construction that owns its exterior declaration.  The current owner may
-- have emitted that declaration locally, or it may carry the exact child
-- declaration through a result-transparent boundary.  Both paths retain
-- binder identity and graph-occurrence routes before the final type is
-- checked, so they can supply @S'(operated)@ without re-reading the mutable
-- graph or recovering a declaration from type shape.
completeOwnerFinalRootGammaRequirements
    :: Maybe OwnerFinalConstruction
    -> GeneralizationRequirements
    -> Either ElabError GeneralizationRequirements
completeOwnerFinalRootGammaRequirements Nothing requirements =
    pure requirements
completeOwnerFinalRootGammaRequirements (Just certificate) requirements = do
    completedRequirements <-
        traverse
            completeRequirement
            (grRequiredGammaBinders requirements)
    pure
        requirements
            { grRequiredGammaBinders = completedRequirements
            }
  where
    owner = ofcOwner certificate
    ownerEdge = lgoBoundaryEdge owner
    ownerScope = localGammaOwnerScope owner

    completeRequirement requirement
        | not (ownerPlacementMatches requirement) =
            pure requirement
        | otherwise = do
            carriedBinders <- matchingCarriedConstructionBinders requirement
            let localBinders
                    | ownerEdge
                        `elem` NonEmpty.toList (rgbEdgeIds requirement) =
                        matchingLocalConstructionBinders requirement
                    | otherwise = []
                constructionBinders =
                    distinctConstructionBinders
                        (localBinders ++ carriedBinders)
            case constructionBinders of
                [] -> pure requirement
                [(_, constructionBound)] ->
                    pure
                        requirement
                            { rgbOperatedType =
                                maybe TBottom tyToElab constructionBound
                            }
                matches ->
                    completionFailure
                        requirement
                        [ "owner certificate contains conflicting exterior declarations"
                        , "  matching binders: " ++ show matches
                        ]

    ownerPlacementMatches requirement =
        case rgbPlacement requirement of
            RequiredGammaAtConstructionScope scope -> scope == ownerScope
            RequiredGammaAtNestedScope scope -> scope == ownerScope
            RequiredGammaAtCurrentScope -> False

    matchingLocalConstructionBinders requirement =
        [ binder
        | binder@(ref, _) <- ofcLocallyEmittedBinders certificate
        , typeBinderRefsSameIdentity
            ref
            (requirementConstructionRef requirement)
        ]

    matchingCarriedConstructionBinders requirement =
        case routedRefs of
            [] -> pure []
            firstRef : remainingRefs
                | all
                    (typeBinderRefsSameIdentity firstRef)
                    remainingRefs ->
                    case
                        [ binder
                        | binder@(ref, _) <-
                            ofcCarriedResultBinders certificate
                        , typeBinderRefsSameIdentity ref firstRef
                        ]
                    of
                        [] ->
                            completionFailure
                                requirement
                                [ "owner certificate carried route has no declaration"
                                , "  routed ref: " ++ show firstRef
                                , "  carried binders: "
                                    ++ show
                                        (ofcCarriedResultBinders certificate)
                                ]
                        matches -> pure matches
                | otherwise ->
                    completionFailure
                        requirement
                        [ "owner certificate has conflicting carried-result routes"
                        , "  routed refs: " ++ show routedRefs
                        , "  carried routes: "
                            ++ show
                                (ofcCarriedResultBinderRoutes certificate)
                        ]
      where
        routedRefs =
            foldr insertDistinctRef []
                [ routedRef
                | routeNode <-
                    [ rgbExteriorNode requirement
                    , rgbOperatedRoot requirement
                    ]
                , routedRef <-
                    maybeToList
                        ( IntMap.lookup
                            (getNodeId routeNode)
                            (ofcCarriedResultBinderRoutes certificate)
                        )
                ]

    distinctConstructionBinders =
        foldl insertConstructionBinder []

    insertConstructionBinder binders incoming@(incomingRef, incomingBound) =
        case
            find
                (typeBinderRefsSameIdentity incomingRef . fst)
                binders
        of
            Nothing -> binders ++ [incoming]
            Just (_, existingBound)
                | constructionBoundsAgree existingBound incomingBound ->
                    binders
                | otherwise -> binders ++ [incoming]

    constructionBoundsAgree left right =
        operationalEndpointTypesAgree
            (maybe TBottom tyToElab left)
            (maybe TBottom tyToElab right)

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

    requirementConstructionRef requirement =
        fromMaybe
            ( typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (rgbExteriorNode requirement))
                ("t" ++ show (getNodeId (rgbExteriorNode requirement)))
            )
            ( IntMap.lookup
                (getNodeId (rgbExteriorNode requirement))
                (ofcLocalBinderRoutes certificate)
            )

    completionFailure
        :: RequiredGammaBinder
        -> [String]
        -> Either ElabError a
    completionFailure requirement details =
        Left
            ( ValidationFailed
                ( [ "owner-final construction cannot complete root RaiseMerge requirement"
                  , "  owner: " ++ show owner
                  , "  requirement: " ++ show requirement
                  ]
                    ++ details
                )
            )

generalizePreparedRootDetailedWithConstructionResult
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> AnnExpr
    -> Maybe OwnerFinalConstruction
    -> [LocalGammaConstructionCertificate]
    -> Either ElabError PreparedRootGeneralization
generalizePreparedRootDetailedWithConstructionResult artifact constructionAnnCanon constructionAnnPre authoritativeResultAnnCanon mbOwnerFinalConstruction localApplicationCertificates = do
    subtermPackets <- pgaSubtermGeneralizations artifact
    exactProducerTypes <- pgaExactProducerTypes artifact
    constructionAnnotationBinders <-
        preparedTransparentRootSourceAnnotationBinders
            artifact
            constructionAnnCanon
    resultAnnotationBinders <-
        preparedTransparentRootSourceAnnotationBinders
            artifact
            authoritativeResultAnnCanon
    constructionExactApplicationEdges <-
        preparedExactApplicationArgumentEdges
            artifact
            constructionAnnCanon
    resultExactApplicationEdges <-
        preparedExactApplicationArgumentEdges
            artifact
            authoritativeResultAnnCanon
    constructionSourceBinderRefs0 <-
        preparedSourceBinderRefsForAnn artifact constructionAnnCanon
    constructionSourceAnnotationOccurrenceRefs <-
        preparedSourceAnnotationOccurrenceRefsForAnn
            artifact
            constructionAnnPre
    let constructionSourceBinderRefs =
            enterSourceAnnotationOccurrenceRefs
                constructionSourceBinderRefs0
                constructionSourceAnnotationOccurrenceRefs
    resultSourceBinderRefs0 <-
        preparedSourceBinderRefsForAnn artifact authoritativeResultAnnCanon
    constructionDirectSourceBinderKeys <-
        preparedDirectSourceBinderKeysForAnn artifact constructionAnnCanon
    resultDirectSourceBinderKeys0 <-
        preparedDirectSourceBinderKeysForAnn
            artifact
            authoritativeResultAnnCanon
    let resultSourceBinderRefs =
            enterCompilerExactConstructionBinderRefs
                constructionSourceBinderRefs
                resultSourceBinderRefs0
        -- Occurrence-local routes from a nested source annotation are needed
        -- while constructing its packets, but are not root publication
        -- authority.  Keep the root carrier on the filtered inherited/exact
        -- maps so final closure cannot reuse a child-owned forall identity.
        resultRootProjectionSourceBinderRefs =
            enterCompilerExactConstructionBinderRefs
                constructionSourceBinderRefs0
                resultSourceBinderRefs0
        constructionCertificateSourceBinderRefs =
            enterCompilerExactConstructionBinderRefs
                (pgaSourceBinderRefs artifact)
                constructionSourceBinderRefs
        resultCertificateSourceBinderRefs =
            enterCompilerExactConstructionBinderRefs
                constructionCertificateSourceBinderRefs
                resultSourceBinderRefs
    let resultDirectSourceBinderKeys =
            IntSet.union
                constructionDirectSourceBinderKeys
                resultDirectSourceBinderKeys0
    -- Only a packet reached through result-transparent wrappers can own a
    -- forall emitted at this expression root.  A packet below another lambda
    -- or unfold is already closed by that nested constructor; admitting its
    -- binder spine here would demand declarations that correctly do not occur
    -- in the root scheme.
    let mbResultOwnership =
            case
                subtermResultOwnershipFor
                    authoritativeResultAnnCanon
                    subtermPackets
            of
                Just ownership
                    | subtermResultOwnershipHasTransparentPath ownership ->
                        Just ownership
                _ -> Nothing
        ownedSubtermPackets =
            subtermGeneralizationsOwnedBy authoritativeResultAnnCanon subtermPackets
        constructionOwnedSubtermPackets =
            subtermGeneralizationsOwnedBy constructionAnnCanon subtermPackets
    rootScope <-
        preparedRootSchemeScope artifact (annNode constructionAnnPre)
    constructionBoundary <-
        generalizationRequirementsForRootBoundary
            (preparedScopeRootForBoundary artifact)
            (preparedIdentityRepresentative artifact)
            (pgaAnnNodeCanonical artifact)
            (pgaBindParentsGa artifact)
            rootScope
            (pgaPresolutionView artifact)
            (preparedEdgeArtifacts artifact)
            exactProducerTypes
            constructionCertificateSourceBinderRefs
            constructionSourceBinderRefs
            subtermPackets
            constructionOwnedSubtermPackets
            constructionExactApplicationEdges
            Nothing
            localApplicationCertificates
            constructionAnnCanon
            constructionAnnCanon
    let resultLocalApplicationCertificates =
            [ certificate
            | certificate <- localApplicationCertificates
            , localGammaOwnerOnResultPath
                (lgccOwner certificate)
                constructionAnnCanon
            ]
        resultBoundaryApplicationCertificates =
            [ certificate
            | certificate <- localApplicationCertificates
            , localGammaOwnerOccursIn
                (lgccOwner certificate)
                authoritativeResultAnnCanon
            ]
    -- A checked application may discharge direct requirements anywhere in
    -- the authoritative result subtree, including an argument position.  A
    -- certificate from an eliminated let RHS remains valid in the complete
    -- construction frame, but it cannot discharge a requirement in the
    -- projected result frame where that occurrence is absent.
    resultBoundary <-
        generalizationRequirementsForRootBoundary
            (preparedScopeRootForBoundary artifact)
            (preparedIdentityRepresentative artifact)
            (pgaAnnNodeCanonical artifact)
            (pgaBindParentsGa artifact)
            rootScope
            (pgaPresolutionView artifact)
            (preparedEdgeArtifacts artifact)
            exactProducerTypes
            resultCertificateSourceBinderRefs
            resultSourceBinderRefs
            subtermPackets
            ownedSubtermPackets
            resultExactApplicationEdges
            Nothing
            resultBoundaryApplicationCertificates
            constructionAnnCanon
            authoritativeResultAnnCanon
    let withAnnotationAmbient binders requirements =
            requirements
                { grAmbientBinderRefs =
                    distinctTypeBinderRefs
                        ( map fst binders
                            ++ grAmbientBinderRefs requirements
                        )
                }
        constructionRequirements0 =
            withAnnotationAmbient
                constructionAnnotationBinders
                (rbrRequirements constructionBoundary)
        rootOwnerFinalConstruction = do
            certificate <- mbOwnerFinalConstruction
            guard
                ( any
                    ( \owner ->
                        localGammaOwnerOnResultPath
                            owner
                            authoritativeResultAnnCanon
                            || localGammaOwnerOnResultPath
                                owner
                                constructionAnnCanon
                    )
                    ( ofcOwner certificate
                        : ofcTransparentResultOwners certificate
                    )
                )
            pure certificate
        locallyRefinedRootBinderRefs certificate =
            [ ref
            | ref <- ofcLocallyEmittedBinderRefs certificate
            , any
                (bodyConsumerBoundRefinementTargetsAny [ref])
                (ofcBodyConsumerBoundRefinements certificate)
            ]
        ownerLocalBinderRoutes =
            case rootOwnerFinalConstruction of
                Nothing -> IntMap.empty
                Just certificate ->
                    IntMap.filter
                        ( \routedRef ->
                            any
                                (typeBinderRefsSameIdentity routedRef)
                                (locallyRefinedRootBinderRefs certificate)
                        )
                        (ofcLocalBinderRoutes certificate)
    constructionSourceBinderRefsWithOwner <-
        mergeCompilerExactConstructionBinderRefs
            (grSourceBinderRefs constructionRequirements0)
            ownerLocalBinderRoutes
    let constructionRequirements =
            constructionRequirements0
                { grSourceBinderRefs =
                    constructionSourceBinderRefsWithOwner
                }
        termUsedRootBinderRefs =
            distinctTypeBinderRefs
                ( applicationUsedNonAmbientRootBinderRefs
                    ++ resultOwnershipConsumerBinderRefs
                    ++ case rootOwnerFinalConstruction of
                        Just certificate ->
                            locallyEmittedRootBinderCandidates certificate
                                ++ usedNonAmbientRootBinderRefs certificate
                        Nothing -> []
                )
        -- A transparent let or annotation does not change the result
        -- boundary owned by its terminal lambda.  If that lambda has already
        -- consumed a topology packet, the packet's consumer identity is a
        -- term-used declaration of the enclosing root even though the lambda
        -- has not emitted the flexible result binder yet.  Feed that exact
        -- source-tree authority to Gen(Gamma,tau); otherwise an unused let can
        -- make the paper's @g g@ result binder disappear before packet
        -- placement has a chance to attach its completed bound.
        resultOwnershipConsumerBinderRefs =
            case mbResultOwnership of
                Just ownership
                    | subtermResultOwnershipConsumerClosedLocally ownership
                    , let packet = subtermResultOwnershipPacket ownership
                    , Just authority <-
                        subtermGeneralizationConsumerAuthority packet
                    , subtermConsumerAuthorityIsTopology authority ->
                        [ typeBinderRefFromIdentity
                            (scaConsumerIdentity authority)
                            ( typeBinderIdentityStableName
                                (scaConsumerIdentity authority)
                            )
                        ]
                _ -> []
        -- A result-transparent application certificate is post-environment
        -- evidence for the exact free declarations used by its checked term
        -- and result.  Feed those identities into Gen(Gamma,tau) before root
        -- reification; validating them only after the root scheme is built
        -- would discover a missing forall too late.  Established incoming or
        -- authority-owned declarations remain ambient and are not re-emitted.
        applicationUsedNonAmbientRootBinderRefs =
            [ ref
            | certificate <- resultLocalApplicationCertificates
            , ref <- lgccUsedAmbientBinderRefs certificate
            , Just _ <- [typeBinderRefNode ref]
            , not
                ( refOccursIn
                    ref
                    (grAmbientBinderRefs constructionRequirements)
                )
            , not
                ( refOccursIn
                    ref
                    ( map agaExactRef
                        (lgccAmbientDeclarationAuthorities certificate)
                    )
                )
            , not
                ( refOccursIn
                    ref
                    ( map agaExactRef
                        ( IntMap.elems
                            ( grAmbientGammaAuthorities
                                constructionRequirements
                            )
                        )
                    )
                )
            ]
        locallyEmittedRootBinderCandidates certificate =
            routedCandidates ++ directlyAddressedCandidates
          where
            localRefs = locallyRefinedRootBinderRefs certificate
            routedCandidates =
                [ typeBinderRefFromIdentity
                    (typeBinderIdentityFromNode (NodeId graphKey))
                    ("t" ++ show graphKey)
                | (graphKey, routedRef) <-
                    IntMap.toList ownerLocalBinderRoutes
                , refOccursIn routedRef localRefs
                ]
            directlyAddressedCandidates =
                [ ref
                | ref <- localRefs
                , Just _ <- [typeBinderRefNode ref]
                , not
                    ( refOccursIn
                        ref
                        (IntMap.elems ownerLocalBinderRoutes)
                    )
                ]
        usedNonAmbientRootBinderRefs certificate =
            [ ref
            | ref <- ofcUsedAmbientBinderRefs certificate
            , Just _ <- [typeBinderRefNode ref]
            , not
                ( refOccursIn
                    ref
                    (grAmbientBinderRefs constructionRequirements)
                )
            , not
                ( refOccursIn
                    ref
                    ( map agaExactRef
                        (ofcAmbientDeclarationAuthorities certificate)
                    )
                )
            , not
                ( refOccursIn
                    ref
                    ( map agaExactRef
                        ( IntMap.elems
                            ( grAmbientGammaAuthorities
                                constructionRequirements
                            )
                        )
                    )
                )
            ]
        rootGeneralizationRequirements0 =
            constructionRequirements
                { grTermUsedRootBinderRefs = termUsedRootBinderRefs
                }
        resultRequirements0 =
            ( withAnnotationAmbient
                resultAnnotationBinders
                (rbrRequirements resultBoundary)
            )
                { grTermUsedRootBinderRefs = termUsedRootBinderRefs
                }
        sourceAnnotationExpectedType =
            transparentRootSourceAnnotationExpectedType
                authoritativeResultAnnCanon
        refOccursIn ref =
            any (typeBinderRefsSameIdentity ref)
        resultLocalApplicationRouteKeys =
            IntSet.unions
                ( map
                    (IntMap.keysSet . lgccLocalBinderRoutes)
                    resultLocalApplicationCertificates
                )
        rawResultLocalGammaClosures =
            foldr insertDistinctClosure []
                [ closure
                | closure <-
                    IntMap.elems
                        (rbrLocallyClosedGammas constructionBoundary)
                , localGammaOwnerOnResultPath
                    (lgcOwner closure)
                    constructionAnnCanon
                , not
                    ( any
                        (applicationCertificateDischargesLocalGammaClosure closure)
                        resultLocalApplicationCertificates
                    )
                ]
        resultLocalGammaClosures =
            case sourceAnnotationExpectedType of
                Just expectedType ->
                    -- The checked source annotation has already consumed or
                    -- preserved child Gamma computations internal to its
                    -- coercion. A local constructor whose exterior is routed
                    -- to a binder visible in the annotation result remains
                    -- the owner of that binder, however; moving it back to
                    -- the root would duplicate the constructor's Lambda.
                    filter
                        (sourceAnnotationRetainsLocalGamma expectedType)
                        rawResultLocalGammaClosures
                Nothing -> rawResultLocalGammaClosures
        sourceAnnotationRetainsLocalGamma expectedType closure =
            annotationRetainsClosure
                || ownerConstructionRetainsClosure
          where
            annotationRetainsClosure =
                any
                    ( \candidateRef ->
                        any
                            (typeBinderRefsSameIdentity candidateRef)
                            expectedRefs
                    )
                    closureResultRefs
            ownerConstructionRetainsClosure =
                case rootOwnerFinalConstruction of
                    Nothing -> False
                    Just certificate ->
                        any
                            ( bodyConsumerBoundRefinementTargetsAny
                                closureResultRefs
                            )
                            (ofcBodyConsumerBoundRefinements certificate)
                            && any
                                ( isJust
                                    . ownerFinalConstructionLocalRefFor
                                        certificate
                                )
                                closureResultRefs
            expectedRefs =
                typeBinderDeclarationRefs expectedType
                    ++ freeTypeVarRefsType expectedType
            exteriorNode = lgcExteriorNode closure
            constructionRouteNodes =
                exteriorNode
                    : gaConstructionRouteNodes
                        (pgaAnnNodeCanonical artifact)
                        (pgaBindParentsGa artifact)
                        exteriorNode
            consumerRef =
                typeBinderRefFromIdentity
                    (lgcConsumerIdentity closure)
                    ( typeBinderIdentityStableName
                        (lgcConsumerIdentity closure)
                    )
            closureResultRefs =
                consumerRef
                    : [ sourceRef
                      | routeNode <- constructionRouteNodes
                      , sourceRef <-
                            maybeToList
                                ( IntMap.lookup
                                    (getNodeId routeNode)
                                    resultSourceBinderRefs
                                )
                      ]
        hasResultTopologyConstruction =
            resultOwnershipHasTopologyConstruction
                || any
                    ( \packet ->
                        case
                            subtermGeneralizationConsumerAuthority packet
                                >>= subtermConsumerAuthorityEnclosingOwner
                        of
                            Just owner ->
                                maybe
                                    False
                                    subtermConsumerAuthorityIsTopology
                                    (subtermGeneralizationConsumerAuthority packet)
                                    && localGammaOwnerOnResultPath
                                        owner
                                        constructionAnnCanon
                            Nothing -> False
                    )
                    (Map.elems ownedSubtermPackets)
        -- An exact application can publish one child as its authoritative
        -- result even though an arbitrary application is not a transparent
        -- source-tree frame.  Result ownership was selected from that
        -- already-certified result annotation, so its exact lambda
        -- edge/node/scope proof is sufficient to retain the topology
        -- construction without teaching the generic result-path walker that
        -- applications are transparent.
        resultOwnershipHasTopologyConstruction =
            case mbResultOwnership of
                Just ownership
                    | subtermResultOwnershipConsumerClosedLocally ownership
                    , Just authority <-
                        subtermGeneralizationConsumerAuthority
                            (subtermResultOwnershipPacket ownership) ->
                        subtermConsumerAuthorityIsTopology authority
                _ -> False
        hasResultLocalConstruction =
            hasResultTopologyConstruction
                || not (null unresolvedResultLocalGammaClosures)
                || any
                    ( not
                        . null
                        . localGammaEmittedBinders
                        . lgccConstruction
                    )
                    resultLocalApplicationCertificates
        -- A result-path closure is historical once the exact checked owner
        -- proves that it neither emitted nor retained the closure's consumer.
        -- Count only unresolved closures when deciding whether root planning
        -- must reconstruct a local Gamma.  Otherwise a transparent wrapper can
        -- hide a closed owner-final result behind stale graph reification and
        -- force root projection to replay a declaration that the owner already
        -- consumed.
        unresolvedResultLocalGammaClosures =
            [ closure
            | closure <- resultLocalGammaClosures
            , not (ownerFinalDischargesUnprojectedClosure closure)
            ]
        ownerFinalDischargesUnprojectedClosure closure =
            case rootOwnerFinalConstruction of
                Nothing -> False
                Just certificate ->
                    let consumerIdentity = lgcConsumerIdentity closure
                        consumerRef =
                            typeBinderRefFromIdentity
                                consumerIdentity
                                (typeBinderIdentityStableName consumerIdentity)
                    in ownerFinalConstructionAuthorizesResultOwner
                        certificate
                        (lgcOwner closure)
                        && isNothing
                            ( ownerFinalConstructionLocalRefFor
                                certificate
                                consumerRef
                            )
                        && not
                            ( any
                                (typeBinderRefsSameIdentity consumerRef)
                                ( freeTypeVarRefsType
                                    (ofcConstructedType certificate)
                                )
                            )
        ownerFinalHasExactApplicationConstruction certificate =
            case
                [ applicationCertificate
                | applicationCertificate <-
                    resultLocalApplicationCertificates
                , lgccOwner applicationCertificate
                    == ofcOwner certificate
                , lgccConstructedType applicationCertificate
                    == ofcConstructedType certificate
                , localGammaEmittedBinders
                    (lgccConstruction applicationCertificate)
                    == ofcLocallyEmittedBinders certificate
                , emittedApplicationRoutes applicationCertificate
                    == ofcLocalBinderRoutes certificate
                ]
            of
                [_] -> True
                _ -> False
          where
            emittedRefs =
                ofcLocallyEmittedBinderRefs certificate
            emittedApplicationRoutes applicationCertificate =
                IntMap.filter
                    ( \routedRef ->
                        any
                            (typeBinderRefsSameIdentity routedRef)
                            emittedRefs
                    )
                    (lgccLocalBinderRoutes applicationCertificate)
        constructedGammaIdentities =
            Set.fromList
                ( [ typeBinderIdentityFromNode (rgbExteriorNode requirement)
                  | requirement <-
                      grRequiredGammaBinders constructionRequirements
                  ]
                    ++ [ typeBinderRefIdentity ref
                       | certificate <- resultLocalApplicationCertificates
                       , (ref, _) <-
                            localGammaEmittedBinders
                                (lgccConstruction certificate)
                       ]
                )
        ownerFinalRootSchemeWith rootBinders certificate =
            mkElabSchemeWithRefs
                ( mergeBinders
                    (rootBinders ++ existingBinders ++ missingApplicationBinders)
                )
                (schemeBody certificateScheme)
          where
            certificateScheme = schemeFromType (ofcConstructedType certificate)
            existingBinders = schemeBinderRefs certificateScheme
            existingRefs = map fst existingBinders
            certificateFreeRefs =
                freeTypeVarRefsType (schemeToType certificateScheme)
            missingApplicationBinders =
                foldr insertMissing []
                    [ binder
                    | applicationCertificate <- resultLocalApplicationCertificates
                    , binder@(ref, _) <-
                        localGammaEmittedBinders
                            (lgccConstruction applicationCertificate)
                    , any (typeBinderRefsSameIdentity ref) certificateFreeRefs
                    ]
            insertMissing binder@(ref, _) binders
                | any (typeBinderRefsSameIdentity ref) existingRefs = binders
                | any (typeBinderRefsSameIdentity ref . fst) binders = binders
                | otherwise = binder : binders
            mergeBinders =
                foldl
                    ( \binders binder@(ref, _) ->
                        if any (typeBinderRefsSameIdentity ref . fst) binders
                            then binders
                            else binders ++ [binder]
                    )
                    []
        ownerFinalRootScheme =
            ownerFinalRootSchemeWith []
        ownerFinalRootSchemeFromOwnedDeclarations rootBinders certificate = do
            completedRootBinders <- traverse completeRootBinder rootBinders
            if null ownedAmbientRefs
                && null ownedSourceRefs
                && completedRootBinders == rootBinders
                then pure Nothing
                else if any null ownedAmbientAuthorityMatches
                    then pure Nothing
                    else do
                        authorities <-
                            traverse
                                uniqueAuthority
                                (zip ownedAmbientRefs ownedAmbientAuthorityMatches)
                        ambientBinders <- traverse ambientBinder authorities
                        sourceBinders <- traverse sourceBinder ownedSourceRefs
                        orderedScheme <-
                            either
                                (Left . ValidationFailed . pure)
                                Right
                                ( orderSourceProjectedSchemeBinders
                                    "owner-final root ambient construction"
                                    ( ownerFinalRootSchemeWith
                                        ( completedRootBinders
                                            ++ sourceBinders
                                            ++ ambientBinders
                                        )
                                        certificate
                                    )
                                )
                        let remainingRefs =
                                [ ref
                                | ref <-
                                    freeTypeVarRefsType
                                        (schemeToType orderedScheme)
                                , not
                                    ( any
                                        (typeBinderRefsSameIdentity ref)
                                        incomingAmbientRefs
                                    )
                                ]
                        case remainingRefs of
                            [] -> pure (Just orderedScheme)
                            _ ->
                                Left
                                    ( ValidationFailed
                                        [ "owner-final root ambient construction remained open"
                                        , "  owner: " ++ show (ofcOwner certificate)
                                        , "  remaining refs: " ++ show remainingRefs
                                        , "  scheme: " ++ show orderedScheme
                                        ]
                                    )
          where
            usedAmbientRefs =
                distinctTypeBinderRefs
                    (ofcUsedAmbientBinderRefs certificate)
            incomingAmbientRefs =
                grAmbientBinderRefs constructionRequirements
            sourceOwnedRefs =
                distinctTypeBinderRefs
                    ( map
                        sourceBinderAuthorityConstructionRef
                        ( IntMap.elems
                            (ofcUsedSourceBinderAuthorities certificate)
                        )
                    )
            rootBinderRefs = map fst rootBinders
            ownedSourceRefs =
                [ ref
                | ref <- sourceOwnedRefs
                , any
                    (typeBinderRefsSameIdentity ref)
                    usedAmbientRefs
                , not
                    ( any
                        (typeBinderRefsSameIdentity ref)
                        (rootBinderRefs ++ incomingAmbientRefs)
                    )
                ]
            ownedAmbientRefs =
                [ ref
                | ref <- usedAmbientRefs
                , not
                    ( any
                        (typeBinderRefsSameIdentity ref)
                        (rootBinderRefs ++ incomingAmbientRefs)
                    )
                , not
                    ( any
                        (typeBinderRefsSameIdentity ref)
                        sourceOwnedRefs
                    )
                ]

            sourceBinder ref = do
                case typeBinderIdentityGeneratedUnique
                    (typeBinderRefIdentity ref) of
                    Nothing ->
                        authorityFailure
                            ref
                            [ "source-owned root declaration is not a generated annotation existential"
                            ]
                    Just _ -> pure ()
                let matchingAuthorities =
                        [ authority
                        | (nodeKey, authority) <-
                            IntMap.toList
                                (ofcUsedSourceBinderAuthorities certificate)
                        , typeBinderRefsSameIdentity
                            ref
                            ( sourceBinderAuthorityConstructionRef
                                authority
                            )
                        , Just currentSidecarRef <-
                            [ IntMap.lookup
                                nodeKey
                                constructionCertificateSourceBinderRefs
                            ]
                        , typeBinderRefsSameIdentity
                            currentSidecarRef
                            (sourceBinderAuthoritySidecarRef authority)
                        ]
                case matchingAuthorities of
                    [] ->
                        authorityFailure
                            ref
                            [ "source-owned root declaration has no live source-sidecar authority"
                            , "  certificate authorities: "
                                ++ show
                                    (ofcUsedSourceBinderAuthorities certificate)
                            , "  root source sidecars: "
                                ++ show constructionCertificateSourceBinderRefs
                            ]
                    _ -> pure (ref, Nothing)

            completeRootBinder binder@(rootRef, rootBound) =
                case
                    [ authority
                    | authority <-
                        ofcAmbientDeclarationAuthorities certificate
                    , typeBinderRefsSameIdentity
                        rootRef
                        (agaExactRef authority)
                    ]
                of
                    [] -> pure binder
                    [authority] -> do
                        (_, authorityBound) <- ambientBinder authority
                        case (rootBound, authorityBound) of
                            (Nothing, Nothing) -> pure binder
                            (Nothing, Just completedBound) ->
                                pure (rootRef, Just completedBound)
                            (Just _, Nothing) ->
                                -- The graph can finish a declaration after
                                -- the local owner observed it as pending.
                                -- Preserve that newer positive bound.
                                pure binder
                            (Just plannedBound, Just completedBound)
                                | operationalEndpointTypesAgree
                                    (tyToElab plannedBound)
                                    (tyToElab completedBound) ->
                                    pure binder
                                | otherwise ->
                                    authorityFailure
                                        rootRef
                                        [ "root and owner-final bounds disagree"
                                        , "  root bound: "
                                            ++ show (tyToElab plannedBound)
                                        , "  owner-final bound: "
                                            ++ show (tyToElab completedBound)
                                        ]
                    authorities ->
                        authorityFailure
                            rootRef
                            [ "multiple exact ambient declarations complete one root binder"
                            , "  declarations: " ++ show authorities
                            ]

            ownedAmbientAuthorityMatches =
                [ [ authority
                  | authority <-
                        ofcAmbientDeclarationAuthorities certificate
                  , typeBinderRefsSameIdentity
                        usedRef
                        (agaExactRef authority)
                  ]
                | usedRef <- ownedAmbientRefs
                ]

            uniqueAuthority (usedRef, authorities) =
                case authorities of
                    [authority] -> pure authority
                    _ ->
                        authorityFailure
                            usedRef
                            [ "multiple exact ambient declarations were published"
                            , "  declarations: " ++ show authorities
                            ]

            ambientBinder authority =
                case agaBound authority of
                    TBottom ->
                        pure (agaExactRef authority, Nothing)
                    bound ->
                        case elabToBound bound of
                            Right boundTy ->
                                pure
                                    ( agaExactRef authority
                                    , Just boundTy
                                    )
                            Left cause ->
                                authorityFailure
                                    (agaExactRef authority)
                                    [ "declaration is not a legal Gamma bound"
                                    , "  bound: " ++ show bound
                                    , "  cause: " ++ cause
                                    ]

            authorityFailure
                :: TypeBinderRef
                -> [String]
                -> Either ElabError a
            authorityFailure ref details =
                Left
                    ( ValidationFailed
                        ( [ "owner-final root construction cannot adopt an ambient identity"
                          , "  owner: " ++ show (ofcOwner certificate)
                          , "  binder: " ++ show ref
                          ]
                            ++ details
                        )
                    )
        ownerFinalFreeRefsOwnedByRoot rootScheme certificate =
            not (null certificateFreeRefs)
                && all certifiedRootRef certificateFreeRefs
                && all ownedByRoot certificateAmbientRefs
          where
            rootRefs = map fst (schemeBinderRefs rootScheme)
            certificateFreeRefs =
                freeTypeVarRefsType (ofcConstructedType certificate)
            certificateAmbientRefs =
                ofcUsedAmbientBinderRefs certificate
            ownedByRoot ref =
                any (typeBinderRefsSameIdentity ref) rootRefs
            certifiedRootRef ref =
                ownedByRoot ref
                    && any
                        (typeBinderRefsSameIdentity ref)
                        certificateAmbientRefs
    rootGeneralizationRequirements <-
        completeOwnerFinalRootGammaRequirements
            rootOwnerFinalConstruction
            rootGeneralizationRequirements0
    resultRequirements <-
        completeOwnerFinalRootGammaRequirements
            rootOwnerFinalConstruction
            resultRequirements0
    let rootTarget =
            case grRequiredGammaBinders resultRequirements of
                []
                    | resultOwnershipHasTopologyConstruction ->
                        -- The exact result-owner packet names a lambda
                        -- codomain construction, while the generic prepared
                        -- scheme-body target can denote an enclosing exact
                        -- application's already-completed result.  Generalize
                        -- the certified source owner itself so the consumer
                        -- occurs in the constructed codomain; merely adding a
                        -- vacuous root binder would lose the paper's InstAbstr
                        -- result action.
                        generalizeTargetNode
                            (pgaPresolutionView artifact)
                            (annNode authoritativeResultAnnCanon)
                    | otherwise ->
                        preparedSchemeBodyTarget
                            artifact
                            (annNode authoritativeResultAnnCanon)
                _ ->
                    generalizeTargetNode
                        (pgaPresolutionView artifact)
                        (annNode authoritativeResultAnnCanon)
        selectRootBinderClosure binders initialRefs =
            [ binder
            | binder@(ref, _) <- binders
            , refMember ref closedRefs
            ]
          where
            closedRefs = close (distinctTypeBinderRefs initialRefs)
            close refs =
                let dependencies =
                        [ dependency
                        | (ref, Just bound) <- binders
                        , refMember ref refs
                        , dependency <- freeTypeVarRefsType (tyToElab bound)
                        , any
                            (typeBinderRefsSameIdentity dependency . fst)
                            binders
                        ]
                    refs' =
                        distinctTypeBinderRefs (refs ++ dependencies)
                in if length refs' == length refs
                    then refs
                    else close refs'
            refMember ref = any (typeBinderRefsSameIdentity ref)
    ownerFinalOwnedRootScheme <-
        case rootOwnerFinalConstruction of
            Just certificate ->
                ownerFinalRootSchemeFromOwnedDeclarations [] certificate
            _ -> pure Nothing
    sourceAnnotationRootScheme <-
        case sourceAnnotationExpectedType of
            Nothing -> pure Nothing
            Just expectedType
                | Just certificate <- rootOwnerFinalConstruction
                , Just ownedAmbientScheme <-
                    ownerFinalOwnedRootScheme
                , alphaEqType
                    (ofcConstructedType certificate)
                    expectedType ->
                    pure
                        ( Just
                            ( ownedAmbientScheme
                            , ofcConstructedBinderRoutes certificate
                            )
                        )
            Just expectedType -> do
                (graphScheme, graphSubst) <-
                    generalizeAtWithBuilderRequired
                        (pgaPlanBuilder artifact)
                        rootGeneralizationRequirements
                        (Just (pgaBindParentsGa artifact))
                        (pgaPresolutionView artifact)
                        rootScope
                        rootTarget
                let expectedRefs =
                        distinctTypeBinderRefs
                            ( typeBinderDeclarationRefs expectedType
                                ++ freeTypeVarRefsType expectedType
                            )
                    expectedSubst =
                        IntMap.filter
                            ( \sourceRef ->
                                any
                                    (typeBinderRefsSameIdentity sourceRef)
                                    expectedRefs
                            )
                            (pgaSourceBinderRefs artifact)
                    annotationSubst =
                        IntMap.union graphSubst expectedSubst
                routedAnnotationScheme <-
                    applyPreparedRootBinderSubst
                        "source annotation result"
                        annotationSubst
                        (schemeFromType expectedType)
                let annotationBinders =
                        schemeBinderRefs routedAnnotationScheme
                    rootedFreeRefs =
                        freeTypeVarRefsType
                            (schemeToType routedAnnotationScheme)
                    constructionUsedRefs =
                        concatMap
                            ( \ref ->
                                ref
                                    : case typeBinderRefNode ref of
                                        Just node ->
                                            maybeToList
                                                ( IntMap.lookup
                                                    (getNodeId node)
                                                    graphSubst
                                                )
                                        Nothing -> []
                            )
                            termUsedRootBinderRefs
                    rootedGraphBinders =
                        selectRootBinderClosure
                            (schemeBinderRefs graphScheme)
                            (rootedFreeRefs ++ constructionUsedRefs)
                    rootedBinders =
                        map preferAnnotationBinder rootedGraphBinders
                            ++ [ binder
                               | binder@(ref, _) <- annotationBinders
                               , not
                                    ( any
                                        (typeBinderRefsSameIdentity ref . fst)
                                        rootedGraphBinders
                                    )
                               ]
                    preferAnnotationBinder graphBinder@(graphRef, _) =
                        fromMaybe
                            graphBinder
                            ( find
                                ( typeBinderRefsSameIdentity graphRef
                                    . fst
                                )
                                annotationBinders
                            )
                    -- Free annotation refs owned by this root's graph
                    -- generalization acquire the graph-planned binder (and
                    -- any bound dependencies).  The remaining refs retain
                    -- their source-side identity as ambient authority; they
                    -- are recorded by 'inheritedRootRefs' below and must not
                    -- be manufactured into unrelated root binders merely
                    -- because the annotation mentions them.
                    rootedAnnotationScheme =
                        mkElabSchemeWithRefs
                            rootedBinders
                            (schemeBody routedAnnotationScheme)
                pure
                    ( Just
                        ( rootedAnnotationScheme
                        , annotationSubst
                        )
                    )
    sourceAnnotationOwnerAmbientRootScheme <-
        case
            ( sourceAnnotationRootScheme
            , rootOwnerFinalConstruction
            )
        of
            (Just (annotationScheme, annotationSubst), Just certificate)
                | hasResultLocalConstruction -> do
                    mbCombinedScheme <-
                        ownerFinalRootSchemeFromOwnedDeclarations
                            (schemeBinderRefs annotationScheme)
                            certificate
                    pure
                        ( fmap
                            ( \combinedScheme ->
                                let annotationBinderRefs =
                                        map fst
                                            (schemeBinderRefs annotationScheme)
                                    adoptedAmbientRefs =
                                        [ ref
                                        | (ref, _) <-
                                            schemeBinderRefs combinedScheme
                                        , not
                                            ( any
                                                (typeBinderRefsSameIdentity ref)
                                                annotationBinderRefs
                                            )
                                        ]
                                    adoptedAmbientRoutes =
                                        IntMap.mapMaybeWithKey
                                            ( \nodeKey _ ->
                                                case
                                                    IntMap.lookup
                                                        nodeKey
                                                        constructionSourceBinderRefs
                                                of
                                                    Just sourceRef
                                                        | any
                                                            ( typeBinderRefsSameIdentity
                                                                sourceRef
                                                            )
                                                            adoptedAmbientRefs ->
                                                            Just sourceRef
                                                    _ -> Nothing
                                            )
                                            annotationSubst
                                in ( combinedScheme
                                   , IntMap.union
                                        adoptedAmbientRoutes
                                        annotationSubst
                                   )
                            )
                            mbCombinedScheme
                        )
            _ -> pure Nothing
    (scheme, subst) <-
        case
            ( sourceAnnotationRootScheme
            , rootOwnerFinalConstruction
            , ownerFinalOwnedRootScheme
            , sourceAnnotationOwnerAmbientRootScheme
            )
        of
            (Just annotationScheme, _, _, _)
                | not hasResultLocalConstruction ->
                -- A source annotation has already constructed and checked
                -- its edge-owned expected type once all internal Gamma
                -- computations have their own construction authority.  That
                -- expected type owns its forall declarations; rebuilding only
                -- the graph body would expose those declarations as free
                -- variables and then try to recover them after the fact.
                pure annotationScheme
            (_, Just certificate, _, _)
                | null
                    (freeTypeVarRefsType (ofcConstructedType certificate))
                , not hasResultLocalConstruction
                    || ownerFinalHasExactApplicationConstruction certificate
                ->
                    -- The exact source constructor on the transparent result
                    -- path has already emitted and checked its complete
                    -- Figure 15.3.5 Lambda(Gamma) prefix.  A surrounding let
                    -- may preserve a lambda/application certificate, so the
                    -- authority is the recorded owner path rather than the
                    -- wrapper constructor kind.  Reifying the
                    -- pre-construction graph here would expose those locally
                    -- owned binders as residual free refs.  When the owner is
                    -- itself an application with a non-empty local Gamma, the
                    -- matching post-environment application certificate is
                    -- the positive proof that the same forall prefix is
                    -- already inside this checked result.
                    pure
                        ( ownerFinalRootScheme certificate
                        , ofcConstructedBinderRoutes certificate
                        )
            (_, _, _, Just combinedScheme) ->
                -- A source annotation can own the ordinary outer forall
                -- while the checked local constructor uses an additional
                -- exact Gamma declaration.  Construct both declarations in
                -- one ordered root spine: the annotation supplies its
                -- binders, and the owner certificate supplies every
                -- remaining declaration and bound.  The direct graph-key
                -- route is retained in the paired substitution, so local
                -- closure consumes the declaration that is actually emitted
                -- here rather than leaving an unscoped Hyp behind.
                pure combinedScheme
            (_, Just certificate, Just ownedAmbientScheme, _) ->
                -- The checked result-path owner used a declaration that was
                -- ambient to its local construction but is absent from the
                -- root's incoming Gamma.  Its owner-final certificate carries
                -- that exact declaration and bound, so construct the root
                -- forall now.  This is the positive counterpart of
                -- Gen(Gamma,tau): no residual free variable or failed graph
                -- reification is inspected to invent the binder.
                pure
                    ( ownedAmbientScheme
                    , ofcConstructedBinderRoutes certificate
                    )
            _ -> do
                generalized@(graphScheme, graphSubst) <-
                    generalizeAtWithBuilderRequired
                        (pgaPlanBuilder artifact)
                        rootGeneralizationRequirements
                        (Just (pgaBindParentsGa artifact))
                        (pgaPresolutionView artifact)
                        rootScope
                        rootTarget
                graphOwnerAmbientScheme <-
                    case rootOwnerFinalConstruction of
                        Just certificate ->
                            ownerFinalRootSchemeFromOwnedDeclarations
                                (schemeBinderRefs graphScheme)
                                certificate
                        Nothing -> pure Nothing
                case
                    ( graphOwnerAmbientScheme
                    , rootOwnerFinalConstruction
                    ) of
                    (Just ownerScheme, Just certificate) -> do
                        -- The graph plan owns the root binders while the
                        -- checked result-path owner owns the exact body and
                        -- any additional ambient declarations.  Compose both
                        -- their declarations and their exact graph routes
                        -- now.  Keeping only the graph substitution would
                        -- put a nested owner forall in the result type while
                        -- dropping the route by which its root RaiseMerge
                        -- reaches that declaration.
                        mergedRoutes <-
                            mergeCompilerExactConstructionBinderRefs
                                graphSubst
                                (ofcConstructedBinderRoutes certificate)
                        pure (ownerScheme, mergedRoutes)
                    (Nothing, Just certificate)
                        | not hasResultLocalConstruction
                        , ownerFinalFreeRefsOwnedByRoot
                            graphScheme
                            certificate ->
                            -- The source constructor has already checked the
                            -- exact result body.  Preserve the graph-planned
                            -- root forall spine, but take the body from that
                            -- certificate so a solved rigid occurrence cannot
                            -- be reintroduced as a free graph identity.
                            pure
                                ( ownerFinalRootSchemeWith
                                    (schemeBinderRefs graphScheme)
                                    certificate
                                , graphSubst
                                )
                    _ -> pure generalized
    let unvalidatedRootSchemeInfo0 = schemeInfoFromRefSubst scheme subst
    mRootRaiseMergeAuthority <-
        rootRaiseMergeAuthorityForExpression
            (preparedEdgeArtifacts artifact)
            authoritativeResultAnnCanon
    let rootRaiseMergeClosedLocally =
            case mRootRaiseMergeAuthority of
                Just (edgeId, authority) ->
                    case
                        IntMap.lookup
                            (getEdgeId edgeId)
                            (rbrLocallyClosedGammas constructionBoundary)
                    of
                        Just closure ->
                            edgeId `elem` lgcEdgeIds closure
                                && lgcExteriorNode closure
                                    == rrmaExterior authority
                                && lgcConsumerIdentity closure
                                    == typeBinderIdentityFromNode
                                        (rrmaExterior authority)
                                && rootRaiseMergeExteriorOwnedByScope
                                    (pgaBindParentsGa artifact)
                                    (localGammaOwnerScope (lgcOwner closure))
                                    (lgcExteriorNode closure)
                        Nothing -> False
                Nothing -> False
    unvalidatedRootSchemeInfo <-
        if rootRaiseMergeClosedLocally
            then pure unvalidatedRootSchemeInfo0
            else
                case mRootRaiseMergeAuthority of
                    Just (edgeId, authority) ->
                        case
                            publishRootRaiseMergePacketResultRoute
                                edgeId
                                authority
                                ownedSubtermPackets
                                unvalidatedRootSchemeInfo0
                          of
                            Right published -> pure published
                            Left cause ->
                                Left
                                    ( ValidationFailed
                                        [ "root RaiseMerge route publication failed during root construction"
                                        , "  construction source routes: "
                                            ++ show constructionSourceBinderRefs
                                        , "  result source routes: "
                                            ++ show resultSourceBinderRefs
                                        , "  root requirements: "
                                            ++ show rootGeneralizationRequirements
                                        , "  owner-final construction: "
                                            ++ show rootOwnerFinalConstruction
                                        , "  result ownership: "
                                            ++ show mbResultOwnership
                                        , "  cause: " ++ show cause
                                        ]
                                    )
                    Nothing -> pure unvalidatedRootSchemeInfo0
    rootSchemeInfo0 <-
        if rootRaiseMergeClosedLocally
            then
                -- The edge-local result substitution belongs to the nested
                -- constructor packet, not to this root view. Local Gamma and
                -- exact-result packets retain their owner-specific construction
                -- paths.
                pure unvalidatedRootSchemeInfo
            else
                case
                    prepareRootRaiseMergeScheme
                        (preparedEdgeArtifacts artifact)
                        authoritativeResultAnnCanon
                        resultRequirements
                        unvalidatedRootSchemeInfo
                of
                    Right prepared -> pure prepared
                    Left err ->
                        Left
                            ( ValidationFailed
                                [ "prepared root RaiseMerge validation failed"
                                , "  construction annotation: " ++ show constructionAnnCanon
                                , "  authoritative result annotation: " ++ show authoritativeResultAnnCanon
                                , "  packets: " ++ show ownedSubtermPackets
                                , "  construction requirements: " ++ show constructionRequirements
                                , "  result requirements: " ++ show resultRequirements
                                , "  root scope: " ++ show rootScope
                                , "  root target: " ++ show rootTarget
                                , "  root target node: " ++ show (pvLookupNode (pgaPresolutionView artifact) rootTarget)
                                , "  root target bound: " ++ show (pvLookupVarBound (pgaPresolutionView artifact) rootTarget)
                                , "  root scheme: " ++ show unvalidatedRootSchemeInfo
                                , "  owner-final construction: "
                                    ++ show mbOwnerFinalConstruction
                                , "  root RaiseMerge operated graph state: "
                                    ++ show
                                        ( case mRootRaiseMergeAuthority of
                                            Nothing -> Nothing
                                            Just (_, authority) ->
                                                let operatedRoot =
                                                        rrmaOperatedRoot authority
                                                    ga = pgaBindParentsGa artifact
                                                 in Just
                                                      ( operatedRoot
                                                      , lookupNodeIn
                                                          ( cNodes
                                                              (gaBaseConstraint ga)
                                                          )
                                                          operatedRoot
                                                      , pvLookupNode
                                                          (pgaPresolutionView artifact)
                                                          operatedRoot
                                                      , pgaCanonical artifact operatedRoot
                                                      , IntMap.lookup
                                                          (getNodeId operatedRoot)
                                                          (gaBaseToSolved ga)
                                                      , IntMap.lookup
                                                          (getNodeId operatedRoot)
                                                          (gaSolvedToBase ga)
                                                      )
                                        )
                                , "  cause: " ++ show err
                                ]
                            )
    -- Application-local routes certify the consumer binder emitted inside
    -- AApp.  They are not aliases for the root scheme: the same graph key can
    -- still carry an ambient source dependency in the root substitution.
    -- Keep only their keys as local-closure ownership evidence for
    -- 'prepareRootConstructionScope' below.
    let rootSchemeInfo = rootSchemeInfo0
    let schemeNormalized = siScheme rootSchemeInfo
        rootSubst = siSubstRefs rootSchemeInfo
    let locallyClosedGammas = rbrLocallyClosedGammas constructionBoundary
        applicationDischargedConsumerIdentities =
            [ typeBinderRefIdentity constructionRef
            | certificate <- resultLocalApplicationCertificates
            , (constructionRef, _) <-
                localGammaConstructionBinders
                    (lgccConstruction certificate)
            ]
        applicationAlreadyPlacedPacket packet =
            case subtermGeneralizationConsumerIdentity packet of
                Just consumerIdentity ->
                    consumerIdentity
                        `elem` applicationDischargedConsumerIdentities
                Nothing -> False
        localGammaAlreadyPlacedPacket packet =
            isJust
                ( subtermGeneralizationLocalConsumerClosure
                    locallyClosedGammas
                    packet
                )
                || case subtermGeneralizationConsumerAuthority packet of
                    Just authority ->
                        isJust
                            ( subtermConsumerAuthorityEnclosingOwner
                                authority
                            )
                            && not
                                ( subtermConsumerAuthorityIsTopology
                                    authority
                                )
                    Nothing -> False
        rootSchemeRetainsPacketConsumer packet =
            case subtermGeneralizationConsumerIdentity packet of
                Just consumerIdentity ->
                    any
                        ( (== consumerIdentity)
                            . typeBinderRefIdentity
                            . fst
                        )
                        (schemeBinderRefs schemeNormalized)
                Nothing -> False
        completeTopologyPacket packetKey packet =
            case rootOwnerFinalConstruction of
                Nothing -> pure (packet, Nothing)
                Just certificate ->
                    case
                        [ endpoint
                        | refinement <-
                            ofcBodyConsumerBoundRefinements certificate
                        , Just endpoint <-
                            [ bodyConsumerBoundRefinementCompletedTopologyEndpoint
                                packet
                                refinement
                            ]
                        ]
                    of
                        [] -> pure (packet, Nothing)
                        [endpoint]
                            -- Gen(Gamma,tau) has retained the exact consumer
                            -- declaration supplied by transparent result
                            -- ownership.  Ordinary packet placement below now
                            -- owns attaching the certified endpoint as its
                            -- flexible bound.  Exact specialization is only
                            -- the construction for a consumer eliminated from
                            -- the root scheme.
                            | rootSchemeRetainsPacketConsumer packet ->
                                pure (packet, Just endpoint)
                            | otherwise ->
                            let completedPacket =
                                    withExactConsumerSpecialization
                                        [endpoint]
                                        packet
                             in case
                                    subtermGeneralizationExactConsumerSpecialization
                                        completedPacket
                                of
                                    Just (_, storedEndpoint, _, _)
                                        | storedEndpoint == endpoint ->
                                            pure (completedPacket, Nothing)
                                    specialization ->
                                        Left
                                            ( ValidationFailed
                                                [ "post-construction topology endpoint has no exact packet specialization"
                                                , "  packet key: "
                                                    ++ show packetKey
                                                , "  certified endpoint: "
                                                    ++ show endpoint
                                                , "  specialization: "
                                                    ++ show specialization
                                                , "  root scheme: "
                                                    ++ show schemeNormalized
                                                , "  root substitution: "
                                                    ++ show rootSubst
                                                , "  result ownership: "
                                                    ++ show mbResultOwnership
                                                ]
                                            )
                        endpoints ->
                            Left
                                ( ValidationFailed
                                    [ "identity-topology packet has multiple post-construction endpoints"
                                    , "  packet key: " ++ show packetKey
                                    , "  endpoints: " ++ show endpoints
                                    ]
                                )
    completedTopologyPacketResults <-
        Map.traverseWithKey completeTopologyPacket ownedSubtermPackets
    let
        completedTopologyPackets =
            fmap fst completedTopologyPacketResults
        completedTopologyEndpoints =
            Map.mapMaybe snd completedTopologyPacketResults
        -- Local lambda/let Gamma owners and AApp place their descendant
        -- packets before Gen(Gamma, tau).  The former retain an edge/scope
        -- closure proof; the latter publish an exact post-environment
        -- emitted/consumed certificate.  A packet accepted by either
        -- authority has therefore already been discharged by that source
        -- constructor.  Trying to place it again at the root would require a
        -- second consumer or duplicate an emitted binder.  Other
        -- consumer-only packets remain root-owned.
        descendantPackets =
            Map.filter
                ( \packet ->
                    not (subtermGeneralizationOwnsGammaEdge packet)
                        && not (localGammaAlreadyPlacedPacket packet)
                        && not (applicationAlreadyPlacedPacket packet)
                )
                completedTopologyPackets
    rootPlacementConstructionRenames <-
        either
            ( \cause ->
                Left
                    ( ValidationFailed
                        [ "root packet placement has inconsistent source-binder construction provenance"
                        , "  cause: " ++ cause
                        ]
                    )
            )
            Right
            ( sourceBinderConstructionRenamesRetainingAmbiguousSources
                (preparedIdentityRepresentative artifact)
                resultCertificateSourceBinderRefs
                rootSubst
            )
    descendantPacketsForPlacement <-
        Map.traverseWithKey
            (enterRootPlacementConstructionRenames rootPlacementConstructionRenames)
            descendantPackets
    let completedPlacementEndpoints =
            [ (placementPacket, endpoint)
            | (packetKey, endpoint) <-
                Map.toList completedTopologyEndpoints
            , Just placementPacket <-
                [Map.lookup packetKey descendantPacketsForPlacement]
            ]
        completedPlacementEndpointFor packet =
            snd
                <$> find
                    ((== packet) . fst)
                    completedPlacementEndpoints
    placementSubst <-
        projectPreparedSourceBinderSubstExceptWithLocalKeys
            constructedGammaIdentities
            resultDirectSourceBinderKeys
            resultLocalApplicationRouteKeys
            resultSourceBinderRefs
            rootSubst
    schemeForPlacement <-
        applyPreparedRootBinderSubst
            "result packet placement"
            placementSubst
            schemeNormalized
    placementInfo <-
        publishTopologyConsumerRoutes
            ( gaConstructionRouteNodes
                (pgaAnnNodeCanonical artifact)
                (pgaBindParentsGa artifact)
            )
            descendantPacketsForPlacement
            (schemeInfoFromRefSubst schemeForPlacement placementSubst)
    rootBinderPlacement <-
        case
            placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy
                (\packet _targetRef _packetBound _constructedBound ->
                    completedPlacementEndpointFor packet
                )
                (siSubstRefs placementInfo)
                descendantPacketsForPlacement
                (siScheme placementInfo)
        of
            Right placement -> pure placement
            Left cause ->
                Left
                    ( ValidationFailed
                        [ "root descendant packet placement failed"
                        , "  result source binder refs: "
                            ++ show resultSourceBinderRefs
                        , "  raw root substitution: " ++ show rootSubst
                        , "  placement substitution: "
                            ++ show (siSubstRefs placementInfo)
                        , "  candidate source/construction renames: "
                            ++ show
                                ( sourceBinderConstructionRenames
                                    (preparedIdentityRepresentative artifact)
                                    resultSourceBinderRefs
                                    rootSubst
                                )
                        , "  certificate source/construction renames: "
                            ++ show
                                ( sourceBinderConstructionRenames
                                    (preparedIdentityRepresentative artifact)
                                    resultCertificateSourceBinderRefs
                                    rootSubst
                                )
                        , "  result certificate source binder refs: "
                            ++ show resultCertificateSourceBinderRefs
                        , "  packet-local source/construction renames: "
                            ++ show
                                [ sourceBinderConstructionRenames
                                    (preparedIdentityRepresentative artifact)
                                    ( siSubstRefs
                                        (subtermGeneralizationSchemeInfo packet)
                                    )
                                    rootSubst
                                | packet <- Map.elems descendantPackets
                                ]
                        , "  packet construction route nodes: "
                            ++ show
                                [ [ ( NodeId graphKey
                                    , gaConstructionRouteNodes
                                        (pgaAnnNodeCanonical artifact)
                                        (pgaBindParentsGa artifact)
                                        (NodeId graphKey)
                                    )
                                  | graphKey <-
                                        IntMap.keys
                                            ( siSubstRefs
                                                ( subtermGeneralizationSchemeInfo
                                                    packet
                                                )
                                            )
                                  ]
                                | packet <- Map.elems descendantPackets
                                ]
                        , "  root owner-final construction: "
                            ++ show rootOwnerFinalConstruction
                        , "  result direct source binder keys: "
                            ++ show resultDirectSourceBinderKeys
                        , "  cause: " ++ show cause
                        ]
                    )
    let schemePlaced =
            placedSubtermBinderScheme rootBinderPlacement
        topologyConstructedConsumers =
            placedSubtermConstructedConsumerIdentities
                rootBinderPlacement
    -- Packet placement may freshen an enclosing consumer's display payload
    -- after allocating the descendant packet name.  Update every graph route
    -- by binder identity before any later source projection can reattach the
    -- stale pre-placement payload.
    let placedRootRefs = map fst (schemeBinderRefs schemePlaced)
        placedRootSubst =
            IntMap.map
                alignPlacedRootRef
                (siSubstRefs placementInfo)
        alignPlacedRootRef ref =
            fromMaybe
                ref
                (find (typeBinderRefsSameIdentity ref) placedRootRefs)
    let schemeConstructed = schemePlaced
        constructedSubst = placedRootSubst
    schemeConstructedUnique0 <-
        quotientPreparedBinderIdentities
            "constructed root"
            schemeConstructed
    let locallyConstructedRootKeys =
            IntSet.union
                resultLocalApplicationRouteKeys
                ( IntSet.fromList
                    [ getNodeId (lgcExteriorNode closure)
                    | closure <- resultLocalGammaClosures
                    ]
                )
        schemeConstructedUnique =
            projectPreparedRootFreeSourceDeclarationCopies
                constructedGammaIdentities
                resultDirectSourceBinderKeys
                locallyConstructedRootKeys
                resultSourceBinderRefs
                schemeConstructedUnique0
        -- Packet placement records a consumer when it installs a previously
        -- missing bound.  A transparent result owner can instead make
        -- Gen(Gamma,tau) construct that exact declaration and bound before
        -- placement runs.  Retain both positive construction lanes, but only
        -- when the identity supplied as term-used result authority actually
        -- survives as a completed declaration in the constructed root spine.
        retainedTopologyConsumers =
            Set.union
                topologyConstructedConsumers
                ( Set.fromList
                    [ typeBinderRefIdentity ownershipRef
                    | ownershipRef <- resultOwnershipConsumerBinderRefs
                    , any
                        ( \(candidateRef, mbBound) ->
                            isJust mbBound
                                && typeBinderRefsSameIdentity
                                    ownershipRef
                                    candidateRef
                        )
                        (schemeBinderRefs schemeConstructedUnique)
                    ]
                )
    schemeConstructedAtResult <-
        constructRetainedTopologyResultScheme
            retainedTopologyConsumers
            mbResultOwnership
            rootOwnerFinalConstruction
            schemeConstructedUnique
    ( requirementConstructionScope
      , exactLocalConstructionRefs
      ) <-
        case grRequiredGammaBinders constructionRequirements of
            [] ->
                pure
                    ( emptyPreparedRootConstructionScope
                        { prcsLocallyClosedGammas = locallyClosedGammas
                        }
                    , []
                    )
            firstRequirement : _ -> do
                let constructionAnchor =
                        pgaAnnNodeCanonical artifact
                            (rgbExteriorNode firstRequirement)
                    locallyConstructedRequirementKeys =
                        IntSet.unions
                            [ resultLocalApplicationRouteKeys
                            , IntSet.fromList
                                [ getNodeId (lgcExteriorNode closure)
                                | closure <-
                                    IntMap.elems locallyClosedGammas
                                ]
                            ]
                mbLocalConstructionSeed <-
                    preparedLocalGammaConstructionSeed
                        (pgaBindParentsGa artifact)
                        locallyClosedGammas
                        constructionRequirements
                ( constructionScheme
                  , constructionSubst0
                  , constructionGeneralizationInheritedRoutes
                  ) <-
                    case mbLocalConstructionSeed of
                        Just localConstruction ->
                            pure
                                ( siScheme localConstruction
                                , siSubstRefs localConstruction
                                , Reify.emptyInheritedGammaRoutes
                                )
                        Nothing ->
                            generalizeAtWithBuilderRequiredCertified
                                (pgaPlanBuilder artifact)
                                constructionRequirements
                                (Just (pgaBindParentsGa artifact))
                                (pgaPresolutionView artifact)
                                rootScope
                                constructionAnchor
                constructionSubst <-
                    projectPreparedSourceBinderSubstExceptWithLocalKeys
                        constructedGammaIdentities
                        constructionDirectSourceBinderKeys
                        locallyConstructedRequirementKeys
                        constructionSourceBinderRefs
                        constructionSubst0
                constructionInheritedGammaRoutes <-
                    Reify.mergeInheritedGammaRoutes
                        (rbrInheritedGammaRoutes constructionBoundary)
                        constructionGeneralizationInheritedRoutes
                constructionDependencySubst <-
                    overlayInheritedGammaDependencyRoutes
                        constructionInheritedGammaRoutes
                        constructionSubst
                prepareRequiredRootConstructionScopeDetailed
                    (pgaPresolutionView artifact)
                    (pgaBindParentsGa artifact)
                    constructionAnnotationBinders
                    locallyClosedGammas
                    constructionInheritedGammaRoutes
                    constructionRequirements
                    constructionScheme
                    constructionDependencySubst
    let inheritedRootRefs =
            foldr insertDistinctRootRef []
                ( [ sourceRef
                  | sourceRef <- IntMap.elems resultSourceBinderRefs
                  , refMember sourceRef certifiedRootAmbientRefs
                  ]
                    ++ [ dependencyRef
                       | (dependencyRef, _) <-
                            prcsBinders requirementConstructionScope
                       , refMember dependencyRef certifiedRootAmbientRefs
                       ]
                )
        freeRootRefs =
            freeTypeVarRefsType (schemeToType schemeConstructedAtResult)
        constructedRootBinderRefs =
            map fst (schemeBinderRefs schemeConstructedAtResult)
        certifiedTermBinderRefs =
            case mbOwnerFinalConstruction of
                Nothing -> []
                Just certificate ->
                    [ graphRef
                    | (graphKey, projectedRef) <-
                        IntMap.toList constructedSubst
                    , refMember projectedRef constructedRootBinderRefs
                    , let graphIdentity =
                            typeBinderIdentityFromNode (NodeId graphKey)
                    , let graphRef =
                            typeBinderRefFromIdentity
                                graphIdentity
                                (typeBinderIdentityStableName graphIdentity)
                    , ownerCertificateOwnsBinderRef certificate graphRef
                    ]
        certifiedRootAmbientRefs =
            freeRootRefs
                ++ maybe
                    []
                    ofcUsedAmbientBinderRefs
                    mbOwnerFinalConstruction
        refMember ref = any (typeBinderRefsSameIdentity ref)
        insertDistinctRootRef ref refs
            | any (typeBinderRefsSameIdentity ref) refs = refs
            | otherwise = ref : refs
        ownerCertificateOwnsBinderRef certificate ref =
            refMember
                ref
                ( ofcLocallyEmittedBinderRefs certificate
                    ++ ofcCarriedResultBinderRefs certificate
                    ++ IntMap.elems
                        (ofcConstructedBinderRoutes certificate)
                    ++ ofcUsedAmbientBinderRefs certificate
                    ++ map
                        agaExactRef
                        (ofcAmbientDeclarationAuthorities certificate)
                )
                || any
                    ( bodyConsumerBoundRefinementTargetsAny [ref]
                    )
                    (ofcBodyConsumerBoundRefinements certificate)
    baseRootClosure <-
        case
            prepareRootClosureSchemeWithSourceAuthorities
                inheritedRootRefs
                constructionSourceBinderRefs
                constructionCertificateSourceBinderRefs
                retainedTopologyConsumers
                mbResultOwnership
                resultLocalGammaClosures
                resultLocalApplicationCertificates
                constructedSubst
                schemeConstructedAtResult
                mbOwnerFinalConstruction
        of
            Right closure -> pure closure
            Left (ValidationFailed messages) ->
                Left
                    ( ValidationFailed
                        ( messages
                            ++ [ "  inherited root refs: "
                                    ++ show inheritedRootRefs
                               , "  construction ambient refs: "
                                    ++ show
                                        ( grAmbientBinderRefs
                                            constructionRequirements
                                        )
                               , "  source annotation root scheme: "
                                    ++ show sourceAnnotationRootScheme
                               , "  owner-final root declaration scheme: "
                                    ++ show ownerFinalOwnedRootScheme
                               , "  topology-constructed consumers: "
                                    ++ show topologyConstructedConsumers
                               , "  retained topology consumers: "
                                    ++ show retainedTopologyConsumers
                               , "  result ownership consumer refs: "
                                    ++ show resultOwnershipConsumerBinderRefs
                               , "  application-used root refs: "
                                    ++ show applicationUsedNonAmbientRootBinderRefs
                               , "  term-used root refs: "
                                    ++ show termUsedRootBinderRefs
                               , "  constructed Gamma identities: "
                                    ++ show constructedGammaIdentities
                               , "  root owner-final construction: "
                                    ++ show rootOwnerFinalConstruction
                               , "  result has topology construction: "
                                    ++ show hasResultTopologyConstruction
                               , "  result local Gamma closures: "
                                    ++ show resultLocalGammaClosures
                               , "  result local application certificates: "
                                    ++ show resultLocalApplicationCertificates
                               , "  constructed root scheme: "
                                    ++ show schemeConstructedAtResult
                               ]
                        )
                    )
            Left cause -> Left cause
    let rootClosure =
            case
                NonEmpty.nonEmpty
                    (Set.toList retainedTopologyConsumers)
            of
                Nothing -> baseRootClosure
                Just consumers ->
                    PreparedTopologyPacketRootClosure
                        consumers
                        baseRootClosure
    refinedRequirementConstructionBinders <-
        case mbOwnerFinalConstruction of
            Nothing -> pure (prcsBinders requirementConstructionScope)
            Just certificate ->
                projectCertifiedBodyConsumerBoundsIfPresent
                    (ofcBodyConsumerBoundRefinements certificate)
                    (prcsBinders requirementConstructionScope)
    rootConstructionScope <-
        prepareRootConstructionScopeWithRequirementEvidence
            locallyClosedGammas
            resultLocalApplicationRouteKeys
            exactLocalConstructionRefs
            refinedRequirementConstructionBinders
            (prcsAliases requirementConstructionScope)
            rootClosure
            constructedSubst
    pure
        PreparedRootGeneralization
            { prgScopeRoot = rootScope
            , prgTarget = rootTarget
            -- Root closure can materialize an identity-routed local Gamma
            -- binder that was quotiented out of the incoming spine.  Publish
            -- that construction-complete scheme to result-type consumers too;
            -- otherwise term closure and the prepared result view disagree on
            -- whether the local forall exists.
            , prgScheme = preparedRootClosureScheme rootClosure
            , prgClosure = rootClosure
            , prgSubst = constructedSubst
            , prgSourceBinderRefs = resultRootProjectionSourceBinderRefs
            , prgDirectSourceBinderKeys = resultDirectSourceBinderKeys
            , prgConstructionScope = rootConstructionScope
            , prgConstructedGammaIdentities = constructedGammaIdentities
            , prgCertifiedTermBinderRefs = certifiedTermBinderRefs
            }
  where
    -- A source annotation declaration is local evidence, not a root ambient
    -- binder.  When root generalization has already constructed a graph-domain
    -- copy of that declaration, compose the certificate-only source sidecar
    -- with the root substitution and publish the resulting quotient on exactly
    -- those descendant packets whose Gamma bound depends on the source ref.
    -- Packet placement can then consume one identity domain without promoting
    -- the source declaration itself into the root scope.
    enterRootPlacementConstructionRenames boundaryRenames packetKey packet = do
        mergedRenames <-
            foldM
                insertBoundaryRename
                (subtermGeneralizationConstructionBinderRenames packet)
                relevantBoundaryRenames
        pure (withConstructionBinderRenames mergedRenames packet)
      where
        packetFreeRefs =
            freeTypeVarRefsType
                ( schemeToType
                    (subtermGeneralizationGammaBoundScheme packet)
                )
        relevantBoundaryRenames =
            [ rename
            | rename@(sourceRef, _) <- boundaryRenames
            , any (typeBinderRefsSameIdentity sourceRef) packetFreeRefs
            ]

        insertBoundaryRename renames incoming@(sourceRef, constructionRef) =
            case
                [ existingConstructionRef
                | (existingSourceRef, existingConstructionRef) <- renames
                , typeBinderRefsSameIdentity existingSourceRef sourceRef
                ]
            of
                [] -> pure (renames ++ [incoming])
                existingConstructionRef : _
                    | typeBinderRefsSameIdentity
                        existingConstructionRef
                        constructionRef ->
                        pure renames
                    | otherwise ->
                        Left
                            ( ValidationFailed
                                [ "root packet placement has conflicting construction routes for one source binder"
                                , "  packet: " ++ show packetKey
                                , "  source binder: " ++ show sourceRef
                                , "  packet route: "
                                    ++ show existingConstructionRef
                                , "  root route: " ++ show constructionRef
                                ]
                            )

    -- Re-open a topology result that the exact source lambda consumed before
    -- an enclosing transparent/result-certified wrapper generalized it.  The
    -- ownership proof supplies the source lambda arity and packet; root
    -- placement supplies the surviving consumer identity; and the finalized
    -- owner certificate supplies the self-contained completed endpoint.  The
    -- only rewrite is therefore the certified lambda codomain, performed
    -- before root closure or final term construction.
    constructRetainedTopologyResultScheme
        :: Set.Set TypeBinderIdentity
        -> Maybe SubtermResultOwnership
        -> Maybe OwnerFinalConstruction
        -> ElabScheme
        -> Either ElabError ElabScheme
    constructRetainedTopologyResultScheme
        retainedConsumers
        mbOwnership
        mbCertificate
        scheme =
            case mbOwnership of
                Just ownership
                    | subtermResultOwnershipConsumerClosedLocally ownership
                    , let packet = subtermResultOwnershipPacket ownership
                    , Just authority <-
                        subtermGeneralizationConsumerAuthority packet
                    , subtermConsumerAuthorityIsTopology authority
                    , Set.member
                        (scaConsumerIdentity authority)
                        retainedConsumers -> do
                        certificate <-
                            maybe
                                (constructionFailure "result owner has no final construction certificate" [])
                                pure
                                mbCertificate
                        endpoint <-
                            case
                                [ completedEndpoint
                                | refinement <-
                                    ofcBodyConsumerBoundRefinements certificate
                                , Just completedEndpoint <-
                                    [ bodyConsumerBoundRefinementCompletedTopologyEndpoint
                                        packet
                                        refinement
                                    ]
                                ]
                            of
                                [completedEndpoint] -> pure completedEndpoint
                                endpoints ->
                                    constructionFailure
                                        "result topology consumer has no unique completed endpoint"
                                        ["  endpoints: " ++ show endpoints]
                        consumerRef <-
                            case
                                [ ref
                                | (ref, Just _) <- schemeBinderRefs scheme
                                , typeBinderRefIdentity ref
                                    == scaConsumerIdentity authority
                                ]
                            of
                                [ref] -> pure ref
                                refs ->
                                    constructionFailure
                                        "result topology consumer has no unique completed root declaration"
                                        ["  declarations: " ++ show refs]
                        body <-
                            publishAtLambdaCodomain
                                (subtermResultOwnershipLambdaArity ownership)
                                consumerRef
                                endpoint
                                (schemeBody scheme)
                        pure
                            ( mkElabSchemeWithRefs
                                (schemeBinderRefs scheme)
                                body
                            )
                _ -> pure scheme
          where
            publishAtLambdaCodomain
                :: Int
                -> TypeBinderRef
                -> ElabType
                -> ElabType
                -> Either ElabError ElabType
            publishAtLambdaCodomain remaining consumerRef endpoint ty
                | remaining <= 0 =
                    case ty of
                        TVarRef presentRef
                            | typeBinderRefsSameIdentity
                                presentRef
                                consumerRef ->
                                pure ty
                        _
                            | operationalEndpointTypesAgree ty endpoint ->
                                pure (TVarRef consumerRef)
                            | otherwise ->
                                constructionFailure
                                    "certified lambda codomain is neither its completed endpoint nor its retained consumer"
                                    [ "  codomain: " ++ show ty
                                    , "  completed endpoint: " ++ show endpoint
                                    ]
                | TForallRef ref mbBound body <- ty =
                    TForallRef ref mbBound
                        <$> publishAtLambdaCodomain
                            remaining
                            consumerRef
                            endpoint
                            body
                | TArrow domain codomain <- ty =
                    TArrow domain
                        <$> publishAtLambdaCodomain
                            (remaining - 1)
                            consumerRef
                            endpoint
                            codomain
                | otherwise =
                    constructionFailure
                        "certified result owner type ended before its source lambda codomain"
                        [ "  remaining lambda arity: " ++ show remaining
                        , "  owner type: " ++ show ty
                        ]

            constructionFailure
                :: String
                -> [String]
                -> Either ElabError a
            constructionFailure detail context =
                Left
                    ( ValidationFailed
                        ( [ "cannot construct retained topology result at its source lambda"
                          , "  detail: " ++ detail
                          , "  result ownership: " ++ show mbOwnership
                          , "  retained consumers: " ++ show retainedConsumers
                          , "  root scheme: " ++ show scheme
                          ]
                            ++ context
                        )
                    )

    transparentRootSourceAnnotationExpectedType ann =
        case ann of
            AAnn _ _ edgeId ->
                IntMap.lookup
                    (getEdgeId edgeId)
                    (preparedAnnotationExpectedTypesByEdge artifact)
            ALetScope inner _ _ ->
                transparentRootSourceAnnotationExpectedType inner
            _ -> Nothing

    insertDistinctClosure closure closures
        | closure `elem` closures = closures
        | otherwise = closure : closures

-- | Decide result placement while the authoritative annotated tree is still
-- available.  Annotation and let-result frames preserve the result path;
-- crossing a different lambda/application or an unfold does not.
localGammaOwnerOnResultPath :: LocalGammaOwner -> AnnExpr -> Bool
localGammaOwnerOnResultPath owner = go
  where
    go ann
        | ownerMatches ann = True
        | otherwise =
            case ann of
                AAnn inner _ _ -> go inner
                AExactAnn inner _ _ _ -> go inner
                ALetScope inner _ _ -> go inner
                ALet _ binderDetails _ _ _ _ rhs body _
                    | annExprReferenceKey body
                        == Just (annBinderKey binderDetails) ->
                        go rhs
                    | otherwise ->
                        go body
                _ -> False

    ownerMatches ann =
        case ann of
            ALam _ _ _ lambdaScope _ bodyEdge lambdaNode ->
                lgoConstructor owner == LocalLambdaGamma
                    && lgoBoundaryEdge owner == bodyEdge
                    && lgoTermNode owner == lambdaNode
                    && localGammaOwnerScope owner == GenRef lambdaScope
            AApp _ _ funSite _ applicationNode ->
                lgoConstructor owner == LocalApplicationGamma
                    && lgoBoundaryEdge owner == instantiationSiteEdgeId funSite
                    && lgoTermNode owner == applicationNode
            ALet _ _ _ _ _ _ _ body resultNode ->
                lgoConstructor owner == LocalLetGamma
                    && ( case body of
                            ALetScope _ _ edgeId -> edgeId == lgoBoundaryEdge owner
                            _ -> False
                       )
                    && lgoTermNode owner == resultNode
            _ -> False

-- | A post-environment application certificate discharges its own pending
-- closure only when it comes from the same source occurrence and either
-- carries the exact exterior graph route to a non-empty emitted/consumed
-- binder or carries an exact identity-and-bound ambient claim for that direct
-- occurrence.  A closure selected from an application's direct lane
-- additionally requires the exact per-requirement edge/exterior claim; merely
-- sharing a consumer identity or scope is insufficient after graph
-- quotienting.
applicationCertificateDischargesLocalGammaClosure
    :: LocalGammaClosure
    -> LocalGammaConstructionCertificate
    -> Bool
applicationCertificateDischargesLocalGammaClosure closure certificate =
    lgccOwner certificate == lgcOwner closure
        && ( ( exteriorRouteIsConstructed
                && directOccurrenceIsCertified
             )
                || ambientOccurrenceIsCertified
           )
  where
    exteriorRouteIsConstructed =
        case
            IntMap.lookup
                (getNodeId (lgcExteriorNode closure))
                (lgccLocalBinderRoutes certificate)
        of
            Just routedRef ->
                any
                    (typeBinderRefsSameIdentity routedRef . fst)
                    ( localGammaConstructionBinders
                        (lgccConstruction certificate)
                    )
            Nothing -> False

    directOccurrenceIsCertified =
        case NonEmpty.nonEmpty (lgcDirectApplicationEdgeIds closure) of
            Nothing -> True
            Just directEdges ->
                case
                    [ claim
                    | claim <-
                        lgccDirectApplicationGammaClaims certificate
                    , edgeKeySet (dagcEdgeIds claim)
                        == edgeKeySet directEdges
                    , dagcExteriorNode claim
                        == lgcExteriorNode closure
                    ]
                of
                    [claim] ->
                        directApplicationGammaClaimConstructionValid
                            certificate
                            claim
                    _ -> False

    ambientOccurrenceIsCertified =
        case NonEmpty.nonEmpty (lgcDirectApplicationEdgeIds closure) of
            Nothing -> False
            Just directEdges ->
                case
                    [ claim
                    | claim <-
                        lgccDirectApplicationAmbientGammaClaims certificate
                    , edgeKeySet (daagcEdgeIds claim)
                        == edgeKeySet directEdges
                    , daagcExteriorNode claim == lgcExteriorNode closure
                    ]
                of
                    [claim] ->
                        ambientDirectApplicationGammaClaimConstructionValid
                            certificate
                            claim
                    _ -> False

    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | A checked owner can also prove that one planned local Gamma slot was
-- discharged without emitting a binder for that slot.  The same owner may
-- still emit an independent binder elsewhere in its completed construction;
-- discharge is therefore decided per closure, not from whether the whole
-- certificate is empty.  The root substitution and owner routes are composed
-- before checking absence so an exterior routed through a planned binder is
-- not mistaken for a consumed slot.
ownerFinalConstructionDischargesLocalGammaClosure
    :: IntMap.IntMap TypeBinderRef
    -> LocalGammaClosure
    -> OwnerFinalConstruction
    -> Bool
ownerFinalConstructionDischargesLocalGammaClosure fullSubst closure certificate =
    discharged
  where
    discharged =
        ownerFinalConstructionAuthorizesResultOwner
            certificate
            (lgcOwner closure)
            && all
                (isNothing . ownerFinalConstructionLocalRefFor certificate)
                localClosureRefs
            && not (any (`refMember` constructedFreeRefs) localClosureRefs)
    consumerIdentity = lgcConsumerIdentity closure
    consumerRef =
        typeBinderRefFromIdentity
            consumerIdentity
            (typeBinderIdentityStableName consumerIdentity)
    closureRefs =
        consumerRef
            : maybeToList
                ( IntMap.lookup
                    (getNodeId (lgcExteriorNode closure))
                    fullSubst
                )
    localClosureRefs = closureRefs
    constructedFreeRefs =
        freeTypeVarRefsType (ofcConstructedType certificate)
    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Prepare the scheme that may be closed at the expression root. The full
-- result scheme retains both root- and lambda-owned binders; this view moves
-- the latter into the body type because the annotated lambda constructs their
-- ETyAbs itself. Packet identity is checked here, while preparation can still
-- reject an invalid plan, rather than falling back during final term closure.
prepareRootClosureScheme
    :: Maybe SubtermResultOwnership
    -> [LocalGammaClosure]
    -> [LocalGammaConstructionCertificate]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Maybe OwnerFinalConstruction
    -> Either ElabError PreparedRootClosure
prepareRootClosureScheme =
    prepareRootClosureSchemeWithAmbient [] IntMap.empty

-- | Prepare a root closure while retaining inherited source-binder authority
-- that was proved before graph-to-source projection. These binders remain
-- free in the local result by Gen(Gamma, tau); the root must neither reject
-- them nor manufacture a duplicate forall.
prepareRootClosureSchemeWithAmbient
    :: [TypeBinderRef]
    -> IntMap.IntMap TypeBinderRef
    -> Maybe SubtermResultOwnership
    -> [LocalGammaClosure]
    -> [LocalGammaConstructionCertificate]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Maybe OwnerFinalConstruction
    -> Either ElabError PreparedRootClosure
prepareRootClosureSchemeWithAmbient ambientRootRefs sourceBinderRefs =
    prepareRootClosureSchemeWithSourceAuthorities
        ambientRootRefs
        sourceBinderRefs
        sourceBinderRefs
        Set.empty

prepareRootClosureSchemeWithSourceAuthorities
    :: [TypeBinderRef]
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Set.Set TypeBinderIdentity
    -> Maybe SubtermResultOwnership
    -> [LocalGammaClosure]
    -> [LocalGammaConstructionCertificate]
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Maybe OwnerFinalConstruction
    -> Either ElabError PreparedRootClosure
prepareRootClosureSchemeWithSourceAuthorities ambientRootRefs sourceBinderRefs certificateSourceBinderRefs retainedRootConsumers mbOwnership localGammaClosures localApplicationCertificates fullSubst fullScheme0 mbOwnerFinalConstruction = do
    let refinementLocalGammaClosures =
            filter
                ( \closure ->
                    not
                        ( any
                            (applicationCertificateDischargesLocalGammaClosure closure)
                            localApplicationCertificates
                        )
                )
                localGammaClosures
        pendingLocalGammaClosures =
            filter
                ( \closure ->
                    not
                        ( maybe
                            False
                            ( ownerFinalConstructionDischargesLocalGammaClosure
                                fullSubst
                                closure
                            )
                            mbProjectedOwnerFinalConstruction
                        )
                )
                refinementLocalGammaClosures
    mapM_
        validateOwnerFinalCertificateRoutes
        mbProjectedOwnerFinalConstruction
    gammaBinders <- traverse gammaBinder pendingLocalGammaClosures
    mapM_ validateApplicationCertificateRoutes localApplicationCertificates
    applicationConstructedSourceBinders <-
        fmap concat
            (traverse applicationConstructedSourceSuffix localApplicationCertificates)
    let gammaBinderRefs = map fst gammaBinders
        applicationBinders =
            concatMap
                (localGammaEmittedBinders . lgccConstruction)
                localApplicationCertificates
        applicationBinderRefs = map fst applicationBinders
        -- A completed local owner can introduce a source binder which the
        -- root planner observes only as a free identity in the result body.
        -- Materialize that exact checked binder before partitioning the root
        -- spine.  Requiring a free occurrence keeps unused owner binders out
        -- of the root plan; requiring matching local authority prevents an
        -- unrelated construction certificate from closing it.
        ownerEmittedFreeBinders =
            case mbProjectedOwnerFinalConstruction of
                Just certificate
                    | ownerHasLocalAuthority ->
                        [ binder
                        | binder@(emittedRef, _) <-
                            ofcLocallyEmittedBinders certificate
                        , any
                            (typeBinderRefsSameIdentity emittedRef)
                            ownerConstructionBodyRefs
                        ]
                _ -> []
        ownerConstructionBodyRefs =
            freeTypeVarRefsType (schemeToType fullScheme)
                ++ case mbProjectedOwnerFinalConstruction of
                    Just certificate ->
                        concat
                            [ bodyConsumerBoundRefinementConsumedDependencies
                                refinement
                            | refinement <-
                                ofcBodyConsumerBoundRefinements certificate
                            , bodyConsumerBoundRefinementConsumesAny
                                rootConstructionBinderRefs
                                refinement
                            ]
                    Nothing -> []
        ownerEmittedFreeBinderRefs = map fst ownerEmittedFreeBinders
        -- An application may emit its prepared Gamma and return a leading
        -- source forall constructed by its checked argument.  That residual
        -- binder is below the application's emitted prefix, so the root must
        -- not reconstruct it outside the application.  Materialize only the
        -- consecutive residual binders that have an exact source-sidecar
        -- route from a free graph occurrence.  Graph-owned residual foralls
        -- remain with the ordinary root planner.  This is an intentional
        -- binding substitution: the declaration is installed immediately
        -- below and owns the projected occurrence.
        applicationConstructedSourceProjectedScheme =
            mkElabSchemeWithRefs
                [ (ref, fmap (mapBoundType projectApplicationConstructedType) mbBound)
                | (ref, mbBound) <- schemeBinderRefs fullScheme
                ]
                (projectApplicationConstructedType (schemeBody fullScheme))
        projectApplicationConstructedType ty0 =
            foldl
                ( \ty (graphRef, constructedRef) ->
                    substTypeSimpleRef
                        graphRef
                        (TVarRef constructedRef)
                        ty
                )
                ty0
                applicationConstructedSourceRoutes
        applicationConstructedSourceRoutes =
            [ (graphRef, constructedRef)
            | (nodeKey, sourceRef) <-
                IntMap.toList certificateSourceBinderRefs
            , constructedRef <- map fst applicationConstructedSourceBinders
            , typeBinderRefsSameIdentity sourceRef constructedRef
            , let graphIdentity =
                    typeBinderIdentityFromNode (NodeId nodeKey)
            , let graphRef =
                    typeBinderRefFromIdentity
                        graphIdentity
                        (typeBinderIdentityStableName graphIdentity)
            , any
                (typeBinderRefsSameIdentity graphRef)
                applicationConstructionBodyFreeRefs
            ]
        applicationConstructionBodyFreeRefs =
            freeTypeVarRefsType (schemeToType fullScheme)
        existingFullBinders =
            schemeBinderRefs applicationConstructedSourceProjectedScheme
        missingLocalBinders =
            foldl
                (insertMissingGammaBinder existingFullBinders)
                []
                ( gammaBinders
                    ++ applicationBinders
                    ++ ownerEmittedFreeBinders
                    ++ applicationConstructedSourceBinders
                )
        unrefinedConstructedFullScheme =
            mkElabSchemeWithRefs
                (existingFullBinders ++ missingLocalBinders)
                (schemeBody applicationConstructedSourceProjectedScheme)
        packetBinderRefs =
            case mbOwnership of
                Nothing -> []
                Just ownership ->
                    [ packetRef
                    | (packetRef, _) <-
                        schemeBinderRefs
                            ( siScheme
                                ( subtermGeneralizationSchemeInfo
                                    (subtermResultOwnershipPacket ownership)
                                )
                            )
                    , any
                        (typeBinderRefsSameIdentity packetRef . fst)
                        (schemeBinderRefs fullScheme)
                    ]
        pendingLocalRefs = packetBinderRefs ++ gammaBinderRefs
        ownerHasLocalAuthority =
            case mbProjectedOwnerFinalConstruction of
                Just certificate ->
                    ownerFinalConstructionMatchesLocalAuthority
                        mbOwnership
                        refinementLocalGammaClosures
                        certificate
                Nothing -> False
        -- Packet ownership and LocalGammaClosure describe where a binder is
        -- expected to be constructed; they are not evidence that an
        -- ETyAbsRef was actually emitted.  Exact-root preparation runs before
        -- term elaboration, so only the checked owner certificate may move
        -- these pending binders below the root closure.  Application
        -- certificates already are post-elaboration emission evidence.
        certifiedPendingLocalRefs =
            case mbProjectedOwnerFinalConstruction of
                Just certificate
                    | ownerHasLocalAuthority ->
                        [ pendingRef
                        | pendingRef <- pendingLocalRefs
                        , isJust
                            ( ownerFinalConstructionLocalRefFor
                                certificate
                                pendingRef
                            )
                        ]
                _ -> []
        -- The completed owner is also authoritative for a planned binder
        -- whose exact route ends at one of its emitted abstractions.  This is
        -- independent of whether another local Gamma slot owned by the same
        -- constructor was consumed (the g g construction exercises both
        -- facts at once).
        ownerRoutedPlannedRefs =
            case mbProjectedOwnerFinalConstruction of
                Just certificate
                    | ownerHasLocalAuthority ->
                        [ plannedRef
                        | (plannedRef, _) <- schemeBinderRefs fullScheme
                        , isJust
                            ( ownerFinalConstructionLocalRefFor
                                certificate
                                plannedRef
                            )
                        ]
                _ -> []
        -- A checked owner can also return a polymorphic result whose leading
        -- forall was constructed by a descendant rather than emitted by this
        -- owner's own completion Gamma.  Keep such a binder local only when
        -- its declared bound depends, transitively, on an exact binder emitted
        -- by this certificate.  For example, if the owner emits @a@ and
        -- returns @forall b >= tau[a]. rho@, moving @b@ outside @a@ would
        -- manufacture an ill-scoped bound.  Merely occurring in the checked
        -- result spine is not ownership evidence: a transparent owner with no
        -- emitted binders must leave ordinary root foralls to the root.
        ownerConstructedResultBinders =
            case mbProjectedOwnerFinalConstruction of
                Just certificate
                    | ownerHasLocalAuthority ->
                        schemeBinderRefs
                            (schemeFromType (ofcConstructedType certificate))
                _ -> []
        ownerConstructedLocalSpine =
            foldl selectOwnerConstructedLocal [] ownerConstructedResultBinders
        selectOwnerConstructedLocal selected binder@(ref, mbBound)
            | any
                (typeBinderRefsSameIdentity ref)
                ownerEmittedConstructionRefs =
                selected ++ [binder]
            | any
                ( \dependency ->
                    any
                        (typeBinderRefsSameIdentity dependency . fst)
                        selected
                )
                ( maybe
                    []
                    (freeTypeVarRefsType . tyToElab)
                    mbBound
                ) =
                selected ++ [binder]
            | otherwise = selected
        ownerEmittedConstructionRefs =
            maybe
                []
                ofcLocallyEmittedBinderRefs
                mbProjectedOwnerFinalConstruction
        ownerConstructedResultBinderRefs =
            [ constructedRef
            | (constructedRef, _) <- ownerConstructedLocalSpine
            , any
                (typeBinderRefsSameIdentity constructedRef . fst)
                (schemeBinderRefs unrefinedConstructedFullScheme)
            ]
        ownerConsumedLocalRef ref =
            case mbProjectedOwnerFinalConstruction of
                Just certificate ->
                    any
                        (bodyConsumerBoundRefinementConsumesAny [ref])
                        (ofcBodyConsumerBoundRefinements certificate)
                Nothing -> False
        ownerDischargedLocalRef ref =
            case mbProjectedOwnerFinalConstruction of
                Just certificate ->
                    any
                        ( \closure ->
                            ownerFinalConstructionDischargesLocalGammaClosure
                                fullSubst
                                closure
                                certificate
                                && closureTargetsRef closure ref
                        )
                        refinementLocalGammaClosures
                Nothing -> False
        closureTargetsRef closure ref =
            typeBinderRefIdentity ref
                == lgcConsumerIdentity closure
        localRefs =
            foldr insertDistinctRef []
                [ ref
                | ref <-
                    certifiedPendingLocalRefs
                        ++ ownerRoutedPlannedRefs
                        ++ ownerConstructedResultBinderRefs
                        ++ applicationBinderRefs
                        ++ map fst applicationConstructedSourceBinders
                        ++ ownerEmittedFreeBinderRefs
                , not
                    ( ownerConsumedLocalRef ref
                        || ownerDischargedLocalRef ref
                    )
                ]
        checkedAmbientRefs =
            foldr
                insertDistinctRef
                []
                ( ambientRootRefs
                    ++ concatMap
                        lgccUsedAmbientBinderRefs
                        localApplicationCertificates
                    ++ maybe
                        []
                        ofcUsedAmbientBinderRefs
                        mbProjectedOwnerFinalConstruction
                )
    case mbProjectedOwnerFinalConstruction of
        Just certificate
            | not
                ( ownerFinalConstructionMatchesLocalAuthority
                    mbOwnership
                    refinementLocalGammaClosures
                    certificate
                )
            , claimedLocalRefs@(_ : _) <-
                filter
                    (isJust . ownerFinalConstructionLocalRefFor certificate)
                    pendingLocalRefs ->
                Left
                    ( ValidationFailed
                        [ "owner-final construction certificate claims a local binder for a different source owner"
                        , "  certificate owner: " ++ show (ofcOwner certificate)
                        , "  packet ownership: " ++ show mbOwnership
                        , "  local Gamma closures: "
                            ++ show pendingLocalGammaClosures
                        , "  expected local refs: " ++ show pendingLocalRefs
                        , "  claimed local refs: " ++ show claimedLocalRefs
                        , "  certified local refs: "
                            ++ show (ofcLocallyEmittedBinderRefs certificate)
                        ]
                    )
        _ -> pure ()
    applicationSourceProjectedFullScheme <-
        projectApplicationLocalSourceAuthorities
            localApplicationCertificates
            unrefinedConstructedFullScheme
    applicationProjectedFullScheme <-
        projectApplicationLocalGraphAuthorities
            localApplicationCertificates
            applicationSourceProjectedFullScheme
    constructedFullScheme0 <-
        case mbProjectedOwnerFinalConstruction of
            Nothing -> pure applicationProjectedFullScheme
            Just certificate -> do
                let refinementLocalRefs =
                        foldr
                            insertDistinctRef
                            []
                            ( localRefs
                                ++ ofcLocallyEmittedBinderRefs certificate
                            )
                -- Source-annotation projection can quotient a graph
                -- declaration directly to the exact source binder that the
                -- root will emit.  Move the refinement proof through that
                -- same identity map before requiring its target in the final
                -- root spine.  Installing the stale graph declaration here
                -- would instead leave the owner's InstAbstrRef unscoped after
                -- compiler-exact closure.
                refinedScheme <-
                    projectCertifiedBodyConsumerRootScheme
                        retainedRootConsumers
                        refinementLocalGammaClosures
                        (ofcUsedAmbientBinderRefs certificate)
                        refinementLocalRefs
                        (ofcBodyConsumerBoundRefinements certificate)
                        applicationProjectedFullScheme
                -- The application certificate owns the Gamma emitted by the
                -- application itself, but it cannot replace the exact bound
                -- already emitted by its returned result owner.  Project that
                -- owner payload before partitioning the root spine, keeping
                -- the planner's binder identity, order, and body unchanged.
                projectOwnerConstructedLocalBinderBounds
                    certificate
                    refinedScheme
    let constructedFullScheme =
            projectVacuousRootConstructionBinders
                ( IntMap.elems sourceBinderRefs
                    ++ IntMap.elems certificateSourceBinderRefs
                    ++ localRefs
                    ++ checkedAmbientRefs
                )
                constructedFullScheme0
    closure <- case localRefs of
        -- A completed descendant owner can consume every local declaration
        -- while its certified result bound still mentions an enclosing
        -- lambda parameter.  In that no-local presentation the checked
        -- owner/application ambient set is the construction authority for
        -- the remaining free identities; retaining only the pre-owner root
        -- set would discard that proof and reject a well-scoped bound.
        [] ->
            pure
                ( PreparedWholeRootClosure
                    checkedAmbientRefs
                    constructedFullScheme
                )
        _
            | null localApplicationCertificates
            , Just ownerFinalConstruction <-
                mbProjectedOwnerFinalConstruction
            , ownerFinalConstructionMatchesLocalAuthority
                mbOwnership
                refinementLocalGammaClosures
                ownerFinalConstruction ->
                prepareCertifiedLocalRootClosure
                    ambientRootRefs
                    sourceBinderRefs
                    mbOwnership
                    refinementLocalGammaClosures
                    constructedFullScheme
                    localRefs
                    ownerFinalConstruction
        _
            | null localApplicationCertificates
            , Just ownerFinalConstruction <-
                mbProjectedOwnerFinalConstruction
            , any
                (isJust . ownerFinalConstructionLocalRefFor ownerFinalConstruction)
                localRefs ->
                Left
                    ( ValidationFailed
                        [ "owner-final construction certificate claims a local binder for a different source owner"
                        , "  certificate owner: " ++ show (ofcOwner ownerFinalConstruction)
                        , "  packet ownership: " ++ show mbOwnership
                        , "  local Gamma closures: "
                            ++ show pendingLocalGammaClosures
                        , "  expected local refs: " ++ show localRefs
                        , "  certified local refs: "
                            ++ show (ofcLocallyEmittedBinderRefs ownerFinalConstruction)
                        ]
                    )
        _ -> do
            let belongsToLocal (ref, _) =
                    any (typeBinderRefsSameIdentity ref) localRefs
                (localBinders, rootBinders) =
                    List.partition
                        belongsToLocal
                        (schemeBinderRefs constructedFullScheme)
                -- A local constructor emits 'localBinders'; root closure must
                -- emit only the ambient binders that remain free in that
                -- completed local scheme (plus their bound dependencies).
                -- This distinction is construction data, not a final-term
                -- cleanup: a reconstructed forall inside a local bound may
                -- share an identity with a redundant root candidate, while K's
                -- lexical parameter binder is genuinely free in the packet
                -- bound and therefore survives this closure.
                retainedRootBinders =
                    rootBinderDependencyClosure
                        rootBinders
                        ( freeTypeVarRefsType (schemeToType localScheme)
                            ++ checkedAmbientRefs
                        )
                missingLocalRefs =
                    [ expectedRef
                    | expectedRef <- localRefs
                    , not
                        ( any
                            (typeBinderRefsSameIdentity expectedRef . fst)
                            localBinders
                        )
                    ]
                ownerConstructedResultBoundMismatches =
                    [ (plannedBinder, constructedBinder)
                    | constructedBinder@(constructedRef, constructedBound) <-
                        ownerConstructedResultBinders
                    , Just plannedBinder <-
                        [ find
                            (typeBinderRefsSameIdentity constructedRef . fst)
                            localBinders
                        ]
                    , not
                        ( rootClosureBinderBoundAgrees
                            (schemeBinderRefs constructedFullScheme)
                            plannedBinder
                            constructedBound
                        )
                    ]
                closureScheme =
                    mkElabSchemeWithRefs
                        retainedRootBinders
                        (schemeToType localScheme)
                localScheme =
                    mkElabSchemeWithRefs
                        localBinders
                        (schemeBody constructedFullScheme)
                -- The planner orders the complete dependency set before
                -- source-tree ownership is applied.  Stable-partitioning it
                -- into root then local binders can therefore create a forward
                -- reference.  Keep the ordinary split when it is lexical;
                -- otherwise retain the planner's dependency order and record
                -- the exact producer-owned identities for interleaved
                -- publication.
                reorderedBinders = retainedRootBinders ++ localBinders
                forwardBoundDependencies =
                    binderForwardBoundDependencies reorderedBinders
                retainedBinderRefs =
                    map fst retainedRootBinders ++ map fst localBinders
                interleavedBinders =
                    [ binder
                    | binder@(ref, _) <-
                        schemeBinderRefs constructedFullScheme
                    , any
                        (typeBinderRefsSameIdentity ref)
                        retainedBinderRefs
                    ]
                interleavedForwardBoundDependencies =
                    binderForwardBoundDependencies interleavedBinders
                interleavedScheme =
                    mkElabSchemeWithRefs
                        interleavedBinders
                        (schemeBody constructedFullScheme)
                authority =
                    preparedLocalAuthority
                        mbOwnership
                        pendingLocalGammaClosures
                        localApplicationCertificates
                        checkedAmbientRefs
                        localScheme
                ownershipMatches =
                    null missingLocalRefs
                        && length localBinders == length localRefs
                        && null ownerConstructedResultBoundMismatches
                localOwnershipFailure =
                    Left
                        ( ValidationFailed
                            [ "local result ownership does not match the constructed root binder spine"
                            , "  packet binders: " ++ show packetBinderRefs
                            , "  Gamma binders: " ++ show gammaBinderRefs
                            , "  application binders: " ++ show applicationBinderRefs
                            , "  owner-routed planned binders: "
                                ++ show ownerRoutedPlannedRefs
                            , "  owner-emitted free binders: "
                                ++ show ownerEmittedFreeBinderRefs
                            , "  owner-constructed result binders: "
                                ++ show ownerConstructedResultBinders
                            , "  owner-constructed result bound mismatches: "
                                ++ show ownerConstructedResultBoundMismatches
                            , "  owner-constructed result closed-bound comparisons: "
                                ++ show
                                    [ ( plannedBinder
                                      , rootClosureClosedBinderBound
                                            (schemeBinderRefs constructedFullScheme)
                                            plannedBinder
                                      , constructedBinder
                                      )
                                    | (plannedBinder, constructedBinder) <-
                                        ownerConstructedResultBoundMismatches
                                    ]
                            , "  checked ambient binders: "
                                ++ show checkedAmbientRefs
                            , "  local application certificates: "
                                ++ show localApplicationCertificates
                            , "  owner-final construction: "
                                ++ show mbProjectedOwnerFinalConstruction
                            , "  matched local binders: " ++ show localBinders
                            , "  missing local refs: " ++ show missingLocalRefs
                            , "  forward bound dependencies after ownership partition: "
                                ++ show forwardBoundDependencies
                            , "  forward bound dependencies in the interleaved spine: "
                                ++ show interleavedForwardBoundDependencies
                            , "  packet ownership: " ++ show mbOwnership
                            , "  constructed full scheme: "
                                ++ show constructedFullScheme
                            , "  interleaved scheme: " ++ show interleavedScheme
                            ]
                        )
            if ownershipMatches && null forwardBoundDependencies
                then
                    pure
                        ( PreparedLocalRootClosure
                            authority
                            closureScheme
                        )
                else
                    if
                        ownershipMatches
                            && null interleavedForwardBoundDependencies
                            && length interleavedBinders
                                == length retainedRootBinders
                                    + length localBinders
                        then
                            pure
                                ( PreparedInterleavedLocalRootClosure
                                    authority
                                    (map fst localBinders)
                                    interleavedScheme
                                )
                        else localOwnershipFailure
    case validatePreparedRootClosure "prepared root closure" closure of
        Right validated -> pure validated
        Left cause ->
            Left
                ( ValidationFailed
                    [ "prepared root closure validation failed"
                    , "  ambient root refs: " ++ show ambientRootRefs
                    , "  checked ambient refs: " ++ show checkedAmbientRefs
                    , "  local refs: " ++ show localRefs
                    , "  owner has local authority: "
                        ++ show ownerHasLocalAuthority
                    , "  owner-emitted free binders: "
                        ++ show ownerEmittedFreeBinders
                    , "  planned full scheme: " ++ show fullScheme
                    , "  local Gamma closures: "
                        ++ show refinementLocalGammaClosures
                    , "  local application certificates: "
                        ++ show localApplicationCertificates
                    , "  owner-final construction: "
                        ++ show mbProjectedOwnerFinalConstruction
                    , "  root substitution: " ++ show fullSubst
                    , "  source binder refs: " ++ show sourceBinderRefs
                    , "  certificate source binder refs: "
                        ++ show certificateSourceBinderRefs
                    , "  root projection renames: "
                        ++ show rootProjectionRenames
                    , "  cause: " ++ show cause
                    ]
                )
  where
    -- Reifying a result whose live scheme scope differs from the current
    -- graph scope can put that scheme's forall spine directly in the body.
    -- At the expression root those binders still need an xMLF constructor
    -- unless a packet/Gamma owner below proves that it emits them locally.
    -- Normalize the complete leading spine into ownership candidates first;
    -- the partition above then moves exactly the proven local binders back
    -- into the body.  This preserves 'schemeToType' while making the owner of
    -- every type abstraction explicit before term closure is constructed.
    (leadingBodyBinders, rootBody) =
        splitForallsRefs (schemeBody fullScheme0)
    fullScheme =
        mkElabSchemeWithRefs
            (schemeBinderRefs fullScheme0 ++ leadingBodyBinders)
            rootBody
    rootConstructionBinderRefs =
        map fst (schemeBinderRefs fullScheme)
    rootProjectionRenames =
        [ (graphRef, projectedRef)
        | (graphKey, projectedRef) <- IntMap.toList fullSubst
        , any
            (typeBinderRefsSameIdentity projectedRef)
            rootConstructionBinderRefs
        , let graphIdentity =
                typeBinderIdentityFromNode (NodeId graphKey)
        , let graphRef =
                typeBinderRefFromIdentity
                    graphIdentity
                    (typeBinderIdentityStableName graphIdentity)
        , not
            (typeBinderRefsSameIdentity graphRef projectedRef)
        ]
    mbProjectedOwnerFinalConstruction =
        renameOwnerFinalConstructionBinderRefPayloads
            rootProjectionRenames
            <$> mbOwnerFinalConstruction

    -- A checked AApp constructor can emit a source-owned forall while the
    -- root plan still mentions graph copies of that exact lexical binder in
    -- its body.  The application certificate carries the graph occurrence to
    -- source-identity quotient used to check the constructor.  Apply that
    -- quotient only to free occurrences governed by one of the certificate's
    -- emitted binders or by an exact used-ambient source authority.
    -- Declarations keep their own owners; an ambient graph occurrence is
    -- projected to the already-owned source identity rather than declared
    -- again at this root.
    projectApplicationLocalSourceAuthorities certificates scheme = do
        routes <- foldM collectCertificateRoutes IntMap.empty certificates
        let freeRefs = freeTypeVarRefsType (schemeToType scheme)
            activeRoutes =
                [ (graphRef, sourceRef)
                | (nodeKey, sourceRef) <- IntMap.toList routes
                , let graphIdentity =
                        typeBinderIdentityFromNode (NodeId nodeKey)
                , let graphRef =
                        typeBinderRefFromIdentity
                            graphIdentity
                            (typeBinderIdentityStableName graphIdentity)
                , any (typeBinderRefsSameIdentity graphRef) freeRefs
                ]
            renameType ty0 =
                foldl
                    ( \ty (graphRef, sourceRef) ->
                        substTypeCaptureRef graphRef (TVarRef sourceRef) ty
                    )
                    ty0
                    activeRoutes
        pure
            ( mkElabSchemeWithRefs
                [ (ref, fmap (mapBoundType renameType) mbBound)
                | (ref, mbBound) <- schemeBinderRefs scheme
                ]
                (renameType (schemeBody scheme))
            )

    -- Application construction can route a result occurrence directly to a
    -- graph declaration emitted by that application.  Root reification may
    -- retain the occurrence node without putting it in the ordinary root
    -- substitution (the declaration is local, not a second root forall).
    -- Apply only routes already authorized by the exact application claim,
    -- root substitution, or the same owner's independently projected final
    -- construction.  This turns the certificate into the binding
    -- substitution it proves; merely allowing the old occurrence to remain
    -- free would leave the emitted ETyAbs unrelated to the root scheme.
    projectApplicationLocalGraphAuthorities certificates scheme = do
        routes <- foldM collectGraphRoutes IntMap.empty certificates
        let freeRefs = freeTypeVarRefsType (schemeToType scheme)
            activeRoutes =
                [ (graphRefForKey nodeKey, targetRef)
                | (nodeKey, targetRef) <- IntMap.toList routes
                , let graphRef = graphRefForKey nodeKey
                , not (typeBinderRefsSameIdentity graphRef targetRef)
                , any (typeBinderRefsSameIdentity graphRef) freeRefs
                ]
            projectType ty0 =
                foldl
                    ( \ty (graphRef, targetRef) ->
                        substTypeCaptureRef
                            graphRef
                            (TVarRef targetRef)
                            ty
                    )
                    ty0
                    activeRoutes
        pure
            ( mkElabSchemeWithRefs
                [ (ref, fmap (mapBoundType projectType) mbBound)
                | (ref, mbBound) <- schemeBinderRefs scheme
                ]
                (projectType (schemeBody scheme))
            )
      where
        collectGraphRoutes routes certificate =
            foldM
                (insertGraphRoute certificate)
                routes
                [ (nodeKey, routedRef)
                | (nodeKey, routedRef) <-
                    IntMap.toList (lgccLocalBinderRoutes certificate)
                , isJust (typeBinderRefNode routedRef)
                , any
                    (typeBinderRefsSameIdentity routedRef . fst)
                    ( localGammaEmittedBinders
                        (lgccConstruction certificate)
                    )
                , applicationGraphRouteIsAuthorized
                    certificate
                    routedRef
                    nodeKey
                ]

        insertGraphRoute certificate routes (nodeKey, targetRef) =
            case IntMap.lookup nodeKey routes of
                Nothing -> pure (IntMap.insert nodeKey targetRef routes)
                Just existingRef
                    | typeBinderRefsSameIdentity existingRef targetRef ->
                        pure routes
                    | otherwise ->
                        Left
                            ( ValidationFailed
                                [ "application Gamma certificates disagree on a graph occurrence route"
                                , "  graph node: " ++ show (NodeId nodeKey)
                                , "  existing target: " ++ show existingRef
                                , "  conflicting target: " ++ show targetRef
                                , "  certificate owner: "
                                    ++ show (lgccOwner certificate)
                                ]
                            )

        graphRefForKey nodeKey =
            let graphIdentity =
                    typeBinderIdentityFromNode (NodeId nodeKey)
            in typeBinderRefFromIdentity
                graphIdentity
                (typeBinderIdentityStableName graphIdentity)

    applicationConstructedSourceSuffix certificate =
        case localGammaEmittedBinders (lgccConstruction certificate) of
            [] -> pure []
            emittedBinders ->
                case
                    localGammaConstructionCertificateResidualType certificate
                of
                    Nothing ->
                        Left
                            ( ValidationFailed
                                [ "application Gamma certificate's checked type does not start with its emitted binder spine"
                                , "  owner: " ++ show (lgccOwner certificate)
                                , "  emitted binders: " ++ show emittedBinders
                                , "  constructed type: "
                                    ++ show (lgccConstructedType certificate)
                                ]
                            )
                    Just residualType ->
                        pure
                            ( takeWhile
                                (sourceBinderHasFreeGraphRoute . fst)
                                ( schemeBinderRefs
                                    (schemeFromType residualType)
                                )
                            )

    sourceBinderHasFreeGraphRoute constructedRef =
        any routeMatches (IntMap.toList certificateSourceBinderRefs)
      where
        freeGraphRefs =
            freeTypeVarRefsType (schemeToType fullScheme)

        routeMatches (nodeKey, sourceRef) =
            typeBinderRefsSameIdentity sourceRef constructedRef
                && any
                    (typeBinderRefsSameIdentity (graphRefForKey nodeKey))
                    freeGraphRefs

        graphRefForKey nodeKey =
            let graphIdentity =
                    typeBinderIdentityFromNode (NodeId nodeKey)
            in typeBinderRefFromIdentity
                graphIdentity
                (typeBinderIdentityStableName graphIdentity)

    collectCertificateRoutes routes certificate =
        foldM
            (insertCertificateRoute certificate)
            routes
            ( emittedSourceRoutes ++ usedAmbientSourceRoutes
            )
      where
        emittedSourceRoutes =
            [ (nodeKey, constructionRef)
            | (nodeKey, authority) <-
                IntMap.toList (lgccSourceBinderAuthorities certificate)
            , let constructionRef =
                    sourceBinderAuthorityConstructionRef authority
            , any
                (typeBinderRefsSameIdentity constructionRef . fst)
                ( localGammaEmittedBinders
                    (lgccConstruction certificate)
                )
            ]
        usedAmbientSourceRoutes =
            [ (nodeKey, constructionRef)
            | (nodeKey, authority) <-
                IntMap.toList
                    (lgccUsedSourceBinderAuthorities certificate)
            , let constructionRef =
                    sourceBinderAuthorityConstructionRef authority
            , any
                (typeBinderRefsSameIdentity constructionRef)
                (lgccUsedAmbientBinderRefs certificate)
            ]

    insertCertificateRoute certificate routes (nodeKey, sourceRef) =
        case IntMap.lookup nodeKey routes of
            Nothing -> pure (IntMap.insert nodeKey sourceRef routes)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef sourceRef ->
                    pure routes
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "application Gamma certificates disagree on a source-binder route"
                            , "  graph node: " ++ show (NodeId nodeKey)
                            , "  existing source binder: " ++ show existingRef
                            , "  conflicting source binder: " ++ show sourceRef
                            , "  certificate owner: "
                                ++ show (lgccOwner certificate)
                            ]
                        )

    gammaBinder closure = do
        ref <-
            case
                IntMap.lookup
                    (getNodeId (lgcExteriorNode closure))
                    fullSubst
            of
                Just routedRef -> pure routedRef
                Nothing ->
                    case certifiedOwnerRef closure of
                        Just certifiedRef -> pure certifiedRef
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "result-local Gamma has no root or owner-final substitution route"
                                    , "  closure: " ++ show closure
                                    ]
                                )
        case
                find
                    (typeBinderRefsSameIdentity ref . fst)
                    (schemeBinderRefs fullScheme)
            of
            Just (_, mbBound) -> pure (ref, mbBound)
            Nothing ->
                case certifiedOwnerBinder ref of
                    Just (_, mbBound) -> pure (ref, mbBound)
                    Nothing ->
                        case lgcOwnerPendingScheme closure of
                            Just ownerSchemeInfo ->
                                case
                                    [ binder
                                    | binder@(pendingRef, _) <-
                                        fst
                                            ( splitForallsRefs
                                                (schemeToType (siScheme ownerSchemeInfo))
                                            )
                                    , typeBinderRefsSameIdentity ref pendingRef
                                    ]
                                of
                                    [(_, Nothing)] ->
                                        -- The pending owner scheme is the typed proof
                                        -- of this exact unbounded local Gamma slot.
                                        -- Root generalization can expose that slot only
                                        -- through its substitution/body, so materialize
                                        -- the declaration before ownership partitioning.
                                        pure (ref, Nothing)
                                    [(_, Just staleBound)] ->
                                        Left
                                            ( ValidationFailed
                                                [ "result-local Gamma pending binder is already materialized"
                                                , "  binder: " ++ show ref
                                                , "  stale bound: " ++ show (tyToElab staleBound)
                                                , "  closure: " ++ show closure
                                                ]
                                            )
                                    []
                                        | any
                                            (typeBinderRefsSameIdentity ref)
                                            ( freeTypeVarRefsType
                                                (schemeToType (siScheme ownerSchemeInfo))
                                            ) ->
                                            -- A quotiented RaiseMerge can publish its
                                            -- exact exterior only as a free construction
                                            -- identity.  The owner still proves the
                                            -- local unbounded Gamma declaration.
                                            pure (ref, Nothing)
                                    _ -> missingConstructedBinder closure ref
                            _ ->
                                missingConstructedBinder closure ref

    certifiedOwnerRef closure = do
        certificate <- mbProjectedOwnerFinalConstruction
        if
            ownerFinalConstructionAuthorizesResultOwner
                certificate
                (lgcOwner closure)
            then
                ownerFinalConstructionLocalRefFor
                    certificate
                    ( typeBinderRefFromIdentity
                        (lgcConsumerIdentity closure)
                        ( typeBinderIdentityStableName
                            (lgcConsumerIdentity closure)
                        )
                    )
            else Nothing

    certifiedOwnerBinder ref = do
        certificate <- mbProjectedOwnerFinalConstruction
        find
            (typeBinderRefsSameIdentity ref . fst)
            ( schemeBinderRefs
                (schemeFromType (ofcConstructedType certificate))
            )

    validateApplicationCertificateRoutes certificate = do
        mapM_
            validateBinder
            ( localGammaEmittedBinders
                (lgccConstruction certificate)
            )
        unless
            (null invalidUsedSourceAuthorities)
            ( Left
                ( ValidationFailed
                    [ "application Gamma certificate has invalid nested source authority"
                    , "  owner: " ++ show (lgccOwner certificate)
                    , "  invalid authorities: "
                        ++ show invalidUsedSourceAuthorities
                    ]
                )
            )
        let knownRootRefs =
                ambientRootRefs
                    ++ map fst (schemeBinderRefs fullScheme)
                    ++ freeTypeVarRefsType (schemeToType fullScheme)
                    ++ IntMap.elems fullSubst
                    ++ ownerFinalAmbientAuthorityRefs
                    ++ nestedSourceAmbientRefs
            ownerFinalAmbientAuthorityRefs =
                case mbProjectedOwnerFinalConstruction of
                    Just ownerCertificate
                        | ownerFinalConstructionAuthorizesResultOwner
                            ownerCertificate
                            (lgccOwner certificate) ->
                            map
                                agaExactRef
                                ( ofcAmbientDeclarationAuthorities
                                    ownerCertificate
                                )
                    _ -> []
            nestedSourceAmbientRefs =
                map
                    sourceBinderAuthorityConstructionRef
                    ( IntMap.elems
                        (lgccUsedSourceBinderAuthorities certificate)
                    )
            certificateAmbientAuthorityRefs =
                map
                    agaExactRef
                    (lgccAmbientDeclarationAuthorities certificate)
            ambientRefKnown ambientRef =
                any
                    (typeBinderRefsSameIdentity ambientRef)
                    (knownRootRefs ++ certificateAmbientAuthorityRefs)
                    || case typeBinderRefNode ambientRef of
                        Just node ->
                            IntMap.member
                                (getNodeId node)
                                fullSubst
                        Nothing -> False
            foreignAmbientRefs =
                [ ambientRef
                | ambientRef <- lgccUsedAmbientBinderRefs certificate
                , not (ambientRefKnown ambientRef)
                ]
        unless
            (null foreignAmbientRefs)
            ( Left
                ( ValidationFailed
                    [ "application Gamma certificate claims ambient identities outside the prepared root"
                    , "  owner: " ++ show (lgccOwner certificate)
                    , "  foreign ambient refs: " ++ show foreignAmbientRefs
                    , "  known root refs: " ++ show knownRootRefs
                    , "  certificate: " ++ show certificate
                    , "  prepared root scheme: " ++ show fullScheme
                    , "  root substitution: " ++ show fullSubst
                    , "  source binder refs: " ++ show sourceBinderRefs
                    , "  ambient root refs: " ++ show ambientRootRefs
                    , "  owner-final construction: "
                        ++ show mbProjectedOwnerFinalConstruction
                    ]
                )
            )
      where
        invalidUsedSourceAuthorities =
            [ ( NodeId nodeKey
              , sourceBinderAuthoritySidecarRef authority
              , sourceBinderAuthorityConstructionRef authority
              , IntMap.lookup nodeKey certificateSourceBinderRefs
              )
            | (nodeKey, authority) <-
                IntMap.toList
                    (lgccUsedSourceBinderAuthorities certificate)
            , not
                ( any
                    ( typeBinderRefsSameIdentity
                        ( sourceBinderAuthorityConstructionRef
                            authority
                        )
                    )
                    (certificateAvailableAmbientBinderRefs certificate)
                )
                || case IntMap.lookup nodeKey certificateSourceBinderRefs of
                    Just preparedRef ->
                        not
                            ( typeBinderRefsSameIdentity
                                (sourceBinderAuthoritySidecarRef authority)
                                preparedRef
                            )
                    Nothing -> True
            ]

        validateBinder (emittedRef, _) =
            let graphRouteKeys =
                    [ nodeKey
                    | (nodeKey, routedRef) <-
                        IntMap.toList (lgccLocalBinderRoutes certificate)
                    , typeBinderRefsSameIdentity emittedRef routedRef
                    ]
                sourceAuthorityKeys =
                    [ nodeKey
                    | (nodeKey, authority) <-
                        IntMap.toList
                            (lgccSourceBinderAuthorities certificate)
                    , typeBinderRefsSameIdentity
                        emittedRef
                        (sourceBinderAuthorityConstructionRef authority)
                    ]
                rootedRefs =
                    [ rootedRef
                    | nodeKey <- graphRouteKeys
                    , Just rootedRef <- [IntMap.lookup nodeKey fullSubst]
                    ]
                graphRouteIsAuthorized nodeKey =
                    applicationGraphRouteIsAuthorized
                        certificate
                        emittedRef
                        nodeKey
                currentSourceAuthorities =
                    [ ( sidecarRef
                      , IntMap.lookup nodeKey certificateSourceBinderRefs
                      )
                    | nodeKey <- sourceAuthorityKeys
                    , Just authority <-
                        [ IntMap.lookup
                            nodeKey
                            (lgccSourceBinderAuthorities certificate)
                        ]
                    , let sidecarRef =
                            sourceBinderAuthoritySidecarRef authority
                    ]
                graphAuthorized =
                    not (null graphRouteKeys)
                        && all graphRouteIsAuthorized graphRouteKeys
                sourceAuthorized =
                    not (null sourceAuthorityKeys)
                        && length currentSourceAuthorities
                            == length sourceAuthorityKeys
                        && all
                            ( \(sidecarRef, mbPreparedRef) ->
                                case mbPreparedRef of
                                    Just preparedRef ->
                                        typeBinderRefsSameIdentity
                                            sidecarRef
                                            preparedRef
                                    Nothing -> False
                            )
                            currentSourceAuthorities
            in case (graphAuthorized, sourceAuthorized) of
                (True, False) -> pure ()
                (False, True) -> pure ()
                (True, True) ->
                    certificateBinderFailure
                        "binder claims both graph and source authority"
                        graphRouteKeys
                        sourceAuthorityKeys
                        rootedRefs
                        currentSourceAuthorities
                (False, False) ->
                    certificateBinderFailure
                        "binder has no matching graph or source authority"
                        graphRouteKeys
                        sourceAuthorityKeys
                        rootedRefs
                        currentSourceAuthorities
          where
            certificateBinderFailure detail graphRouteKeys sourceAuthorityKeys rootedRefs currentSourceAuthorities =
                Left
                    ( ValidationFailed
                        [ "application Gamma certificate binder authority is invalid"
                        , "  detail: " ++ detail
                        , "  owner: " ++ show (lgccOwner certificate)
                        , "  emitted binder: " ++ show emittedRef
                        , "  application construction: "
                            ++ show (lgccConstruction certificate)
                        , "  application constructed type: "
                            ++ show (lgccConstructedType certificate)
                        , "  graph route keys: "
                            ++ show (map NodeId graphRouteKeys)
                        , "  source authority keys: "
                            ++ show (map NodeId sourceAuthorityKeys)
                        , "  rooted refs: " ++ show rootedRefs
                        , "  current source authorities: "
                            ++ show currentSourceAuthorities
                        , "  root substitution: " ++ show fullSubst
                        , "  prepared root scheme: " ++ show fullScheme
                        , "  source binder sidecar: "
                            ++ show sourceBinderRefs
                        , "  direct claims: "
                            ++ show
                                (lgccDirectApplicationGammaClaims certificate)
                        , "  owner-final construction: "
                            ++ show mbProjectedOwnerFinalConstruction
                        ]
                    )

    applicationGraphRouteIsAuthorized certificate emittedRef nodeKey =
        IntSet.member nodeKey directClaimRouteKeys
            || IntSet.member nodeKey ownerFinalRouteKeys
            || case IntMap.lookup nodeKey fullSubst of
                Just rootedRef ->
                    typeBinderRefsSameIdentity emittedRef rootedRef
                Nothing ->
                    typeBinderRefNode emittedRef == Just (NodeId nodeKey)
      where
        directClaimRouteKeys =
            IntSet.fromList
                [ getNodeId node
                | claim <- lgccDirectApplicationGammaClaims certificate
                , typeBinderRefsSameIdentity
                    emittedRef
                    (dagcBinderRef claim)
                , node <- directClaimRouteNodes claim
                ]
        ownerFinalRouteKeys =
            IntSet.fromList
                [ routeNodeKey
                | ownerCertificate <-
                    maybeToList mbProjectedOwnerFinalConstruction
                , ownerFinalConstructionAuthorizesResultOwner
                    ownerCertificate
                    (lgccOwner certificate)
                , (routeNodeKey, routedRef) <-
                    IntMap.toList (ofcLocalBinderRoutes ownerCertificate)
                , typeBinderRefsSameIdentity emittedRef routedRef
                ]

    validateOwnerFinalCertificateRoutes certificate = do
        mapM_
            (validateRoutedBinder "locally emitted" (ofcLocalBinderRoutes certificate))
            (ofcLocallyEmittedBinderRefs certificate)
        mapM_ validateCarriedBinder (ofcCarriedResultBinderRefs certificate)
        unless
            ( all
                (`refMember` ofcCarriedResultBinderRefs certificate)
                (ofcCarriedResultTypeAbstractionRefs certificate)
            )
            ( Left
                ( ValidationFailed
                    [ "owner-final construction has type-abstraction evidence outside its carried result"
                    , "  owner: " ++ show (ofcOwner certificate)
                    , "  carried binders: "
                        ++ show (ofcCarriedResultBinderRefs certificate)
                    , "  type-abstraction evidence: "
                        ++ show (ofcCarriedResultTypeAbstractionRefs certificate)
                    ]
                )
            )
      where
        validateRoutedBinder role routes emittedRef =
            unless
                (hasRoute routes emittedRef)
                ( Left
                    ( ValidationFailed
                        [ "owner-final construction binder has no exact graph provenance route"
                        , "  owner: " ++ show (ofcOwner certificate)
                        , "  binder role: " ++ role
                        , "  emitted binder: " ++ show emittedRef
                        , "  routes: " ++ show routes
                        ]
                    )
                )

        validateCarriedBinder carriedRef =
            unless
                ( hasRoute
                    (ofcCarriedResultBinderRoutes certificate)
                    carriedRef
                    || refMember
                        carriedRef
                        (ofcCarriedResultTypeAbstractionRefs certificate)
                )
                ( Left
                    ( ValidationFailed
                        [ "owner-final carried binder has no construction provenance"
                        , "  owner: " ++ show (ofcOwner certificate)
                        , "  carried binder: " ++ show carriedRef
                        , "  graph routes: "
                            ++ show (ofcCarriedResultBinderRoutes certificate)
                        , "  type-abstraction evidence: "
                            ++ show (ofcCarriedResultTypeAbstractionRefs certificate)
                        ]
                    )
                )

        hasRoute routes ref =
            any
                (typeBinderRefsSameIdentity ref)
                (IntMap.elems routes)

        refMember ref = any (typeBinderRefsSameIdentity ref)

    missingConstructedBinder closure ref =
        Left
            ( ValidationFailed
                [ "result-local Gamma has no constructed root binder"
                , "  binder: " ++ show ref
                , "  closure: " ++ show closure
                ]
            )

    insertMissingGammaBinder existingBinders binders binder@(ref, _)
        | any (typeBinderRefsSameIdentity ref . fst) existingBinders = binders
        | any (typeBinderRefsSameIdentity ref . fst) binders = binders
        | otherwise = binders ++ [binder]

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

    rootClosureBoundsAgree left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

    -- A planned binder bound is checked under the forall declarations that
    -- precede it in the root spine.  The completed owner certificate records
    -- the same bound as a self-contained type, so compare both the direct
    -- forms and the planned bound closed over exactly its live dependencies.
    rootClosureBinderBoundAgrees binders plannedBinder@(_, plannedBound) constructedBound =
        rootClosureBoundsAgree plannedBound constructedBound
            || rootClosureTypeBoundsAgree
                (rootClosureClosedBinderBound binders plannedBinder)
                (maybe TBottom tyToElab constructedBound)

    rootClosureClosedBinderBound binders plannedBinder@(_, plannedBound) =
        schemeToType
            (mkElabSchemeWithRefs dependencyBinders plannedBoundTy)
      where
        precedingBinders =
            takeWhile
                ( not
                    . typeBinderRefsSameIdentity (fst plannedBinder)
                    . fst
                )
                binders
        plannedBoundTy = maybe TBottom tyToElab plannedBound
        dependencyBinders =
            rootBinderDependencyClosure
                precedingBinders
                (freeTypeVarRefsType plannedBoundTy)

    rootClosureTypeBoundsAgree left right =
        alphaEqType left right || churchAwareEqType left right

    binderForwardBoundDependencies binders =
        [ (binderRef, dependency)
        | (binderIndex, (binderRef, Just bound)) <-
            zip [0 :: Int ..] binders
        , dependency <- freeTypeVarRefsType (tyToElab bound)
        , laterRef <- map fst (drop (binderIndex + 1) binders)
        , typeBinderRefsSameIdentity dependency laterRef
        ]

    -- Eq-Free must run while root binder ownership is still explicit.  A
    -- graph-only declaration can become vacuous after a checked descendant
    -- supplies the result endpoint; retaining it can make its bound refer
    -- forward into a lambda-owned codomain.  Process inside-out so removing
    -- one administrative declaration can make its now-unused dependencies
    -- vacuous too.  Exact source and locally constructed identities remain
    -- observable publication ABI and are never removed here.
    projectVacuousRootConstructionBinders protectedRefs candidateScheme =
        mkElabSchemeWithRefs
            (project (schemeBinderRefs candidateScheme))
            (schemeBody candidateScheme)
      where
        project [] = []
        project (binder@(ref, _) : binders) =
            let retainedBinders = project binders
                retainedType =
                    schemeToType
                        ( mkElabSchemeWithRefs
                            retainedBinders
                            (schemeBody candidateScheme)
                        )
                protected =
                    any (typeBinderRefsSameIdentity ref) protectedRefs
                vacuous =
                    not
                        ( any
                            (typeBinderRefsSameIdentity ref)
                            (freeTypeVarRefsType retainedType)
                        )
            in if not protected && vacuous
                then retainedBinders
                else binder : retainedBinders

    rootBinderDependencyClosure binders initialRefs =
        [ binder
        | binder@(ref, _) <- binders
        , refMember ref closedRefs
        ]
      where
        closedRefs = close initialRefs

        close refs =
            let dependencies =
                    [ dependency
                    | (ref, Just bound) <- binders
                    , refMember ref refs
                    , dependency <- freeTypeVarRefsType (tyToElab bound)
                    , any (typeBinderRefsSameIdentity dependency . fst) binders
                    ]
                refs' = foldr insertDistinctRef refs dependencies
            in if length refs' == length refs then refs else close refs'

        refMember ref = any (typeBinderRefsSameIdentity ref)

-- | A detailed owner result is applicable only when every local closure in
-- this root slice belongs to that exact source constructor.  Packet
-- ownership does not retain the boundary edge, so its structural lambda node
-- is the additional provenance check in the packet-only case.
ownerFinalConstructionMatchesLocalAuthority
    :: Maybe SubtermResultOwnership
    -> [LocalGammaClosure]
    -> OwnerFinalConstruction
    -> Bool
ownerFinalConstructionMatchesLocalAuthority mbOwnership closures certificate =
    hasLocalAuthority
        && packetOwnerMatches
        && all
            ( ownerFinalConstructionAuthorizesResultOwner certificate
                . lgcOwner
            )
            closures
  where
    certificateOwner = ofcOwner certificate
    hasLocalAuthority = isJust mbOwnership || not (null closures)
    packetOwnerMatches =
        case closures of
            _ : _ -> True
            [] ->
                case mbOwnership of
                    Nothing -> True
                    Just ownership ->
                        subtermResultOwnershipLambdaNode ownership
                            == lgoTermNode certificateOwner

ownerFinalConstructionAuthorizesResultOwner
    :: OwnerFinalConstruction
    -> LocalGammaOwner
    -> Bool
ownerFinalConstructionAuthorizesResultOwner certificate owner =
    owner == ofcOwner certificate
        || owner `elem` ofcTransparentResultOwners certificate

ownerFinalConstructionLocalRefFor
    :: OwnerFinalConstruction
    -> TypeBinderRef
    -> Maybe TypeBinderRef
ownerFinalConstructionLocalRefFor certificate expectedRef =
    case
        find
            (typeBinderRefsSameIdentity expectedRef)
            (ofcLocallyEmittedBinderRefs certificate)
    of
        Just emittedRef -> Just emittedRef
        Nothing -> do
            expectedNode <- typeBinderRefNode expectedRef
            routedRef <-
                IntMap.lookup
                    (getNodeId expectedNode)
                    (ofcLocalBinderRoutes certificate)
            find
                (typeBinderRefsSameIdentity routedRef)
                (ofcLocallyEmittedBinderRefs certificate)

ownerFinalConstructionCarriedRefFor
    :: OwnerFinalConstruction
    -> TypeBinderRef
    -> Maybe TypeBinderRef
ownerFinalConstructionCarriedRefFor certificate expectedRef =
    case
        find
            (typeBinderRefsSameIdentity expectedRef)
            (ofcCarriedResultBinderRefs certificate)
    of
        Just carriedRef -> Just carriedRef
        Nothing -> do
            expectedNode <- typeBinderRefNode expectedRef
            routedRef <-
                IntMap.lookup
                    (getNodeId expectedNode)
                    (ofcCarriedResultBinderRoutes certificate)
            find
                (typeBinderRefsSameIdentity routedRef)
                (ofcCarriedResultBinderRefs certificate)

-- | Install only the binder bounds actually emitted by a checked local
-- owner.  The root plan continues to own binder identity, order, and body;
-- the owner-final certificate owns the payload of a declaration for which it
-- carries an exact local construction route.  Requiring one matching entry
-- in the certified construction spine makes duplicate or incomplete
-- construction evidence fail before root ownership is published.
projectOwnerConstructedLocalBinderBounds
    :: OwnerFinalConstruction
    -> ElabScheme
    -> Either ElabError ElabScheme
projectOwnerConstructedLocalBinderBounds certificate scheme = do
    projectedBinders <- traverse projectBinder (schemeBinderRefs scheme)
    pure
        ( mkElabSchemeWithRefs
            projectedBinders
            (schemeBody scheme)
        )
  where
    constructedBinders = ofcConstructedBinderSpine certificate
    localBinderRefs = ofcLocallyEmittedBinderRefs certificate

    projectBinder binder@(plannedRef, _) = do
        routedRefs <-
            case routedRefFor plannedRef of
                Nothing -> pure []
                Just routedRef ->
                    case
                        filter
                            (typeBinderRefsSameIdentity routedRef)
                            localBinderRefs
                    of
                        [] ->
                            projectionFailure
                                plannedRef
                                ( "the planned graph route targets no locally emitted declaration: "
                                    ++ show routedRef
                                )
                        matches -> pure matches
        case distinctRefs (directRefsFor plannedRef ++ routedRefs) of
            [] -> pure binder
            [constructedRef] ->
                case
                    filter
                        (typeBinderRefsSameIdentity constructedRef . fst)
                        constructedBinders
                of
                    [(_, constructedBound)] ->
                        pure (plannedRef, constructedBound)
                    [] ->
                        projectionFailure
                            plannedRef
                            "the routed declaration is absent from the certified construction spine"
                    matches ->
                        projectionFailure
                            plannedRef
                            ( "the certified construction spine contains duplicate declaration identities: "
                                ++ show matches
                            )
            conflictingRefs ->
                projectionFailure
                    plannedRef
                    ( "direct identity and graph provenance select different locally emitted declarations: "
                        ++ show conflictingRefs
                    )

    directRefsFor plannedRef =
        filter
            (typeBinderRefsSameIdentity plannedRef)
            localBinderRefs

    routedRefFor plannedRef = do
        plannedNode <- typeBinderRefNode plannedRef
        IntMap.lookup
            (getNodeId plannedNode)
            (ofcLocalBinderRoutes certificate)

    distinctRefs = foldr insertDistinct []
      where
        insertDistinct ref refs
            | any (typeBinderRefsSameIdentity ref) refs = refs
            | otherwise = ref : refs

    projectionFailure :: TypeBinderRef -> String -> Either ElabError a
    projectionFailure plannedRef detail =
        Left
            ( ValidationFailed
                [ "cannot project owner-emitted binder bound into root construction"
                , "  owner: " ++ show (ofcOwner certificate)
                , "  planned binder: " ++ show plannedRef
                , "  detail: " ++ detail
                , "  locally emitted binders: "
                    ++ show (ofcLocallyEmittedBinders certificate)
                , "  local binder routes: "
                    ++ show (ofcLocalBinderRoutes certificate)
                , "  certified construction spine: "
                    ++ show constructedBinders
                ]
            )

-- | Close a root from evidence produced by the local constructor itself.
-- The planner remains authoritative for binder order and candidate identities.
-- The owner certificate contributes exact bounds and liveness for its routed
-- local declarations: those identities are excluded from the root spine, and
-- its ambient-use certificate selects the still-needed root candidates.
prepareCertifiedLocalRootClosure
    :: [TypeBinderRef]
    -> IntMap.IntMap TypeBinderRef
    -> Maybe SubtermResultOwnership
    -> [LocalGammaClosure]
    -> ElabScheme
    -> [TypeBinderRef]
    -> OwnerFinalConstruction
    -> Either ElabError PreparedRootClosure
prepareCertifiedLocalRootClosure ambientRootRefs sourceBinderRefs mbOwnership closures plannedFullScheme expectedLocalRefs certificate = do
    ownerProjectedPlannedScheme <-
        projectOwnerConstructedLocalBinderBounds
            certificate
            plannedFullScheme
    let locallyEmittedInputBinders =
            ofcLocallyEmittedBinders certificate
        carriedResultInputBinders =
            ofcCarriedResultBinders certificate
        constructionInputBinders =
            locallyEmittedInputBinders ++ carriedResultInputBinders
        plannedBinders =
            schemeBinderRefs ownerProjectedPlannedScheme
        certifiedLocalRefs = distinctRefs (ofcLocallyEmittedBinderRefs certificate)
        certifiedCarriedRefs =
            distinctRefs (ofcCarriedResultBinderRefs certificate)
        rawCertifiedAmbientRefs =
            distinctRefs (ofcUsedAmbientBinderRefs certificate)
        -- A checked Church roll can expose the structural self binder in an
        -- owner-local bound even though the root plan names the ambient
        -- flexible declaration whose exact bound owns that self.  Recover
        -- that route only from the declaration certificate: matching a
        -- structural identity or a recursive type shape on its own is not
        -- authority to replace the checked carrier.
        recursiveCarrierAuthorityRoutes =
            [ (selfRef, agaExactRef ambientAuthority)
            | ambientAuthority <- ofcAmbientDeclarationAuthorities certificate
            , TMuRef selfRef _ <- [agaBound ambientAuthority]
            , refMember selfRef rawCertifiedAmbientRefs
            , refMember
                (agaExactRef ambientAuthority)
                rawCertifiedAmbientRefs
            ]
        recursiveCarrierRefs =
            distinctRefs (map fst recursiveCarrierAuthorityRoutes)
        recursiveCarrierTargets carrierRef =
            distinctRefs
                [ declarationRef
                | (candidateCarrierRef, declarationRef) <-
                    recursiveCarrierAuthorityRoutes
                , typeBinderRefsSameIdentity
                    carrierRef
                    candidateCarrierRef
                ]
        ambiguousRecursiveCarrierRoutes =
            [ (carrierRef, targets)
            | carrierRef <- recursiveCarrierRefs
            , let targets = recursiveCarrierTargets carrierRef
            , length targets /= 1
            ]
        recursiveCarrierRenames =
            [ (carrierRef, declarationRef)
            | carrierRef <- recursiveCarrierRefs
            , [declarationRef] <- [recursiveCarrierTargets carrierRef]
            ]
        recursivelyProjectedAmbientRefs =
            distinctRefs
                (map projectRecursiveCarrierRef rawCertifiedAmbientRefs)
        ambientSourceRenames =
            [ (ambientRef, sourceRef)
            | ambientRef <- recursivelyProjectedAmbientRefs
            , Just node <- [typeBinderRefNode ambientRef]
            , Just sourceRef <-
                [IntMap.lookup (getNodeId node) sourceBinderRefs]
            -- Only lexical generated binders are outward Gamma aliases.
            -- Structural self/result identities name declarations inside
            -- their owning mu/forall and must remain reconstruction
            -- metadata; projecting an ambient graph declaration to one would
            -- collapse the complete recursive owner to its internal carrier.
            , isJust
                ( typeBinderIdentityGeneratedUnique
                    (typeBinderRefIdentity sourceRef)
                )
            , not (typeBinderRefsSameIdentity ambientRef sourceRef)
            ]
        certifiedAmbientRefs =
            distinctRefs
                (map projectAmbientRef recursivelyProjectedAmbientRefs)
        certificateConstructedType =
            foldl
                ( \ty (ambientRef, sourceRef) ->
                    substTypeCaptureRef
                        ambientRef
                        (TVarRef sourceRef)
                        ty
                )
                recursivelyProjectedConstructedType
                ambientSourceRenames
        recursivelyProjectedConstructedType =
            foldl
                ( \ty (carrierRef, declarationRef) ->
                    substTypeCaptureRef
                        carrierRef
                        (TVarRef declarationRef)
                        ty
                )
                (ofcConstructedType certificate)
                recursiveCarrierRenames
        projectRecursiveCarrierRef ref =
            fromMaybe
                ref
                ( snd
                    <$> find
                        (typeBinderRefsSameIdentity ref . fst)
                        recursiveCarrierRenames
                )
        projectAmbientRef ref =
            fromMaybe
                ref
                ( snd
                    <$> find
                        (typeBinderRefsSameIdentity ref . fst)
                        ambientSourceRenames
                )
        duplicateLocalRefs = duplicateRefs (ofcLocallyEmittedBinderRefs certificate)
        duplicateCarriedRefs =
            duplicateRefs (ofcCarriedResultBinderRefs certificate)
        localCarriedOverlap =
            [ localRef
            | localRef <- certifiedLocalRefs
            , refMember localRef certifiedCarriedRefs
            ]
        duplicateAmbientRefs = duplicateRefs (ofcUsedAmbientBinderRefs certificate)
        localAmbientOverlap =
            [ localRef
            | localRef <- certifiedLocalRefs
            , refMember localRef certifiedAmbientRefs
            ]
        carriedAmbientOverlap =
            [ carriedRef
            | carriedRef <- certifiedCarriedRefs
            , refMember carriedRef certifiedAmbientRefs
            ]
        -- A closure can nominate the consumer identity before construction
        -- establishes whether this owner emits it or inherits it.  Exact
        -- ambient-use evidence resolves that choice; only the remaining
        -- nominated identities require a local construction route.
        expectedLocalRoutes =
            [ ( expectedRef
              , ownerFinalConstructionLocalRefFor certificate expectedRef
              )
            | expectedRef <- distinctRefs expectedLocalRefs
            , not (refMember expectedRef certifiedAmbientRefs)
            ]
        -- A local binder's declared bound can depend on another exact binder
        -- from the root plan.  When the owner has already emitted that
        -- dependency ahead of the local binder, it belongs to the same
        -- certified construction spine and must not be emitted again at the
        -- root.  Admit only the transitive dependency closure selected by the
        -- planner; unrelated extra binders remain invalid.
        plannedCarriedRoutes =
            [ (plannedRef, carriedRef)
            | (plannedRef, _) <- plannedBinders
            , Just carriedRef <-
                [ ownerFinalConstructionCarriedRefFor
                    certificate
                    plannedRef
                ]
            ]
        plannedOwnerBinders =
            rootDependencyClosure
                plannedBinders
                (expectedLocalRefs ++ map fst plannedCarriedRoutes)
        plannedOwnerRoutes =
            [ ( plannedRef
              , lookupByExpected plannedRef expectedLocalRoutes
                    <|> lookupByExpected
                        plannedRef
                        [ (candidate, Just carriedRef)
                        | (candidate, carriedRef) <- plannedCarriedRoutes
                        ]
                    <|> find
                        (typeBinderRefsSameIdentity plannedRef)
                        certifiedLocalRefs
                    <|> find
                        (typeBinderRefsSameIdentity plannedRef)
                        certifiedCarriedRefs
              )
            | (plannedRef, _) <- plannedOwnerBinders
            ]
        -- Several planned graph declarations can quotient to one binder
        -- emitted by the owner.  Validate that emitted declaration once, at
        -- the first planned position in the quotient, and use the planned
        -- declaration whose identity is the emitted identity when available.
        -- An interior source binder can otherwise precede its enclosing
        -- result binder and make the same checked binder appear twice with
        -- two different provisional bounds.
        selectedPlannedOwnerRoutes =
            foldl selectPlannedRoute []
                [ (plannedRef, plannedBound, certifiedRef)
                | (plannedRef, plannedBound) <- plannedOwnerBinders
                , Just certifiedRef <-
                    [lookupByExpected plannedRef plannedOwnerRoutes]
                ]
        selectPlannedRoute selected incoming@(plannedRef, _, certifiedRef) =
            case
                break
                    ( \(_, _, existingCertifiedRef) ->
                        typeBinderRefsSameIdentity
                            existingCertifiedRef
                            certifiedRef
                    )
                    selected
            of
                (_, []) -> selected ++ [incoming]
                (before, existing@(existingPlannedRef, _, _) : after)
                    | typeBinderRefsSameIdentity plannedRef certifiedRef
                    , not
                        ( typeBinderRefsSameIdentity
                            existingPlannedRef
                            certifiedRef
                        ) ->
                        before ++ (incoming : after)
                    | otherwise ->
                        before ++ (existing : after)
        certificateScheme = schemeFromType certificateConstructedType
        certificateBinders = schemeBinderRefs certificateScheme
        routedCertificateRefs =
            distinctRefs
                [ certifiedRef
                | (_, _, certifiedRef) <- selectedPlannedOwnerRoutes
                ]
        -- A checked owner can emit an earlier binder which the root planner
        -- sees only as a free dependency in a later planned bound.  The
        -- dependency is still construction-owned: it is present in the
        -- constructor input, the checked forall spine, and the exact bound
        -- that needs it.  Close backwards through that checked binder spine
        -- rather than requiring an artificial second graph route for the
        -- dependency.  An unrelated emitted binder is not reachable from a
        -- routed declaration and remains invalid.
        authorizedCertificateRefs =
            binderDependencyClosureRefs
                certificateBinders
                routedCertificateRefs
        certifiedPlannedOwnerRefs =
            [ plannedRef
            | (plannedRef, Just _) <- plannedOwnerRoutes
            ]
        missingCertifiedLocalRoutes =
            [ expectedRef
            | (expectedRef, Nothing) <- expectedLocalRoutes
            ]
        unexpectedCertifiedLocals =
            [ certifiedRef
            | certifiedRef <- certifiedLocalRefs
            , not (refMember certifiedRef authorizedCertificateRefs)
            ]
        unexpectedCertifiedCarriedRefs =
            [ certifiedRef
            | certifiedRef <- certifiedCarriedRefs
            , not (refMember certifiedRef authorizedCertificateRefs)
            ]
        plannedRootBinders =
            [ binder
            | binder@(ref, _) <- plannedBinders
            , not (refMember ref certifiedPlannedOwnerRefs)
            ]
        constructionInputBinderMismatches =
            [ "construction="
                ++ show constructionBinder
                ++ ", checked="
                ++ show checkedBinder
            | (constructionBinder, checkedBinder) <-
                zip constructionInputBinders certificateBinders
            , not (constructionBindersAgree constructionBinder checkedBinder)
            ]
                ++ [ "construction binder has no checked counterpart: "
                        ++ show constructionBinder
                   | constructionBinder <-
                        drop
                            (length certificateBinders)
                            constructionInputBinders
                   ]
                ++ [ "checked binder has no construction input: "
                        ++ show checkedBinder
                   | checkedBinder <-
                        drop
                            (length constructionInputBinders)
                            certificateBinders
                   ]
        unexpectedCertificateBinders =
            [ ref
            | (ref, _) <- certificateBinders
            , not (refMember ref certifiedLocalRefs)
            , not (refMember ref certifiedCarriedRefs)
            ]
        unownedCertificateBinders =
            [ binder
            | binder@(ref, _) <- certificateBinders
            , not (refMember ref authorizedCertificateRefs)
            ]
        retainedCertificateBinders =
            [ binder
            | binder@(ref, _) <- certificateBinders
            , refMember ref authorizedCertificateRefs
            ]
        plannedRoutedCertificateRefs =
            [ certifiedRef
            | (_, _, certifiedRef) <- selectedPlannedOwnerRoutes
            ]
        checkedRoutedCertificateRefs =
            [ ref
            | (ref, _) <- certificateBinders
            , refMember ref routedCertificateRefs
            ]
        certificateBinderOrderMismatch =
            not
                ( sameRefOrder
                    plannedRoutedCertificateRefs
                    checkedRoutedCertificateRefs
                )
        certificateBoundMismatches =
            [ (certifiedRef, plannedBound, certificateBound)
            | (_, plannedBound, certifiedRef) <- selectedPlannedOwnerRoutes
            , Just (_, certificateBound) <-
                [find (typeBinderRefsSameIdentity certifiedRef . fst) certificateBinders]
            , not
                ( certificateBoundAdmissible
                    certifiedAmbientRefs
                    plannedBound
                    certificateBound
                )
            ]
        certifiedLocalScheme =
            mkElabSchemeWithRefs
                retainedCertificateBinders
                (schemeBody certificateScheme)
        freeCertificateRefs =
            freeTypeVarRefsType (schemeToType certifiedLocalScheme)
        missingAmbientUses =
            [ freeRef
            | freeRef <- freeCertificateRefs
            , not (refMember freeRef certifiedAmbientRefs)
            ]
        retainedRootBinders =
            rootDependencyClosure
                plannedRootBinders
                certifiedAmbientRefs
        uncertifiedRetainedDependencies =
            [ ref
            | (ref, _) <- retainedRootBinders
            , not (refMember ref certifiedAmbientRefs)
            ]
        unownedCertifiedAmbientRefs =
            [ ref
            | ref <- certifiedAmbientRefs
            , not (refMember ref (map fst plannedBinders))
            , not
                ( refMember
                    ref
                    (freeTypeVarRefsType (schemeToType plannedFullScheme))
                )
            , not (refMember ref ambientRootRefs)
            ]
        closureScheme =
            mkElabSchemeWithRefs
                retainedRootBinders
                (schemeToType certifiedLocalScheme)
        reorderedBinders = retainedRootBinders ++ retainedCertificateBinders
        forwardBoundDependencies =
            [ (binderRef, dependency)
            | (binderIndex, (binderRef, Just bound)) <-
                zip [0 :: Int ..] reorderedBinders
            , dependency <- freeTypeVarRefsType (tyToElab bound)
            , laterRef <- map fst (drop (binderIndex + 1) reorderedBinders)
            , typeBinderRefsSameIdentity dependency laterRef
            ]
        authority =
            -- These refs were observed free in the checked owner result and
            -- matched above by exact binder identity.  They are lexical
            -- authority for the nested local scheme even when the root
            -- planner omits an inherited rigid declaration from its forall
            -- spine.
            preparedLocalAuthority
                mbOwnership
                closures
                []
                certifiedAmbientRefs
                certifiedLocalScheme
        failures =
            [ ("duplicate locally emitted binder identities", show duplicateLocalRefs)
            | not (null duplicateLocalRefs)
            ]
                ++ [ ("duplicate carried-result binder identities", show duplicateCarriedRefs)
                   | not (null duplicateCarriedRefs)
                   ]
                ++ [ ("local and carried-result certificates overlap", show localCarriedOverlap)
                   | not (null localCarriedOverlap)
                   ]
                ++ [ ("duplicate ambient-use binder identities", show duplicateAmbientRefs)
                   | not (null duplicateAmbientRefs)
                   ]
                ++ [ ("local and ambient certificates overlap", show localAmbientOverlap)
                   | not (null localAmbientOverlap)
                   ]
                ++ [ ("carried-result and ambient certificates overlap", show carriedAmbientOverlap)
                   | not (null carriedAmbientOverlap)
                   ]
                ++ [ ("recursive structural carrier has ambiguous ambient declaration authority", show ambiguousRecursiveCarrierRoutes)
                   | not (null ambiguousRecursiveCarrierRoutes)
                   ]
                ++ [ ("owner certificate has no construction route for locally planned binders", show missingCertifiedLocalRoutes)
                   | not (null missingCertifiedLocalRoutes)
                   ]
                ++ [ ("owner certificate emits binders outside its local plan", show unexpectedCertifiedLocals)
                   | not (null unexpectedCertifiedLocals)
                   ]
                ++ [ ("owner certificate carries result binders outside its checked child plan", show unexpectedCertifiedCarriedRefs)
                   | not (null unexpectedCertifiedCarriedRefs)
                   ]
                ++ [ ("constructed type binds an identity not emitted by the owner", show unexpectedCertificateBinders)
                   | not (null unexpectedCertificateBinders)
                   ]
                ++ [ ("checked owner binder spine disagrees with its construction input", show constructionInputBinderMismatches)
                   | not (null constructionInputBinderMismatches)
                   ]
                ++ [ ("constructed type binder is neither routed nor a checked dependency", show unownedCertificateBinders)
                   | not (null unownedCertificateBinders)
                   ]
                ++ [ ("constructed type routed-binder order disagrees with the planner", show (plannedRoutedCertificateRefs, checkedRoutedCertificateRefs))
                   | certificateBinderOrderMismatch
                   ]
                ++ [ ("constructed type binder bounds disagree with the planner", show certificateBoundMismatches)
                   | not (null certificateBoundMismatches)
                   ]
                ++ [ ("constructed type contains an uncertified ambient identity", show missingAmbientUses)
                   | not (null missingAmbientUses)
                   ]
                ++ [ ("ambient-use certificate is not dependency-closed", show uncertifiedRetainedDependencies)
                   | not (null uncertifiedRetainedDependencies)
                   ]
                ++ [ ("ambient-use certificate has no prepared root closure authority", show unownedCertifiedAmbientRefs)
                   | not (null unownedCertifiedAmbientRefs)
                   ]
                ++ [ ("planner order would put a bound dependency after its binder", show forwardBoundDependencies)
                   | not (null forwardBoundDependencies)
                   ]
    case failures of
        [] ->
            pure
                ( PreparedLocalRootClosure
                    authority
                    closureScheme
                )
        _ ->
            Left
                ( ValidationFailed
                    ( [ "owner-final construction certificate does not match root closure authority"
                      , "  owner: " ++ show (ofcOwner certificate)
                      , "  expected local refs: " ++ show expectedLocalRefs
                      , "  planned root scheme: " ++ show plannedFullScheme
                      , "  emitted binders: "
                          ++ show (ofcLocallyEmittedBinders certificate)
                      , "  local binder routes: "
                          ++ show (ofcLocalBinderRoutes certificate)
                      , "  carried result binders: "
                          ++ show (ofcCarriedResultBinders certificate)
                      , "  carried result routes: "
                          ++ show (ofcCarriedResultBinderRoutes certificate)
                      , "  ambient refs: "
                          ++ show (ofcUsedAmbientBinderRefs certificate)
                      , "  ambient declaration authorities: "
                          ++ show
                              (ofcAmbientDeclarationAuthorities certificate)
                      , "  body-consumer refinements: "
                          ++ show
                              (ofcBodyConsumerBoundRefinements certificate)
                      , "  source binder refs: " ++ show sourceBinderRefs
                      , "  constructed type: "
                          ++ show (ofcConstructedType certificate)
                      ]
                        ++ ["  " ++ label ++ ": " ++ detail | (label, detail) <- failures]
                    )
                )
  where
    refMember ref = any (typeBinderRefsSameIdentity ref)

    lookupByExpected _ [] = Nothing
    lookupByExpected expectedRef ((candidate, mbCertifiedRef) : rest)
        | typeBinderRefsSameIdentity expectedRef candidate = mbCertifiedRef
        | otherwise = lookupByExpected expectedRef rest

    distinctRefs = foldr insertDistinct []
    insertDistinct ref refs
        | refMember ref refs = refs
        | otherwise = ref : refs

    duplicateRefs refs =
        [ ref
        | (index, ref) <- zip [0 :: Int ..] refs
        , any (typeBinderRefsSameIdentity ref) (drop (index + 1) refs)
        ]

    sameRefOrder left right =
        length left == length right
            && and (zipWith typeBinderRefsSameIdentity left right)

    equivalentBounds left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

    constructionBindersAgree (leftRef, leftBound) (rightRef, rightBound) =
        typeBinderRefsSameIdentity leftRef rightRef
            && equivalentBounds leftBound rightBound

    -- A local owner is elaborated after Gen(Gamma, tau) has removed leading
    -- binders already opened by its enclosing construction environment.  The
    -- root planner still carries the pre-environment generalized bound.  The
    -- owner's checked bound may replace it only when structural matching maps
    -- every removed planner binder to an ambient identity explicitly recorded
    -- by the owner certificate.
    certificateBoundAdmissible certifiedAmbientRefs plannedBound certificateBound
        | equivalentBounds plannedBound certificateBound = True
        | otherwise =
            case (plannedBound, certificateBound) of
                (Just planned, Just constructed) ->
                    let plannedTy = tyToElab planned
                        constructedTy = tyToElab constructed
                        (plannedBinders, plannedBody) = splitForallsRefs plannedTy
                    in not (null plannedBinders)
                        && case
                            matchTypeRefs
                                (map fst plannedBinders)
                                plannedBody
                                constructedTy
                           of
                            Right matches ->
                                all
                                    (\matchedTy ->
                                        case matchedTy of
                                            TVarRef matchedRef ->
                                                refMember matchedRef certifiedAmbientRefs
                                            _ -> False
                                    )
                                    (Map.elems matches)
                            Left _ -> False
                _ -> False

    rootDependencyClosure binders initialRefs =
        [ binder
        | binder@(ref, _) <- binders
        , refMember ref (binderDependencyClosureRefs binders initialRefs)
        ]

    binderDependencyClosureRefs binders initialRefs =
        close (distinctRefs initialRefs)
      where
        close refs =
            let dependencies =
                    [ dependency
                    | (ref, Just bound) <- binders
                    , refMember ref refs
                    , dependency <- freeTypeVarRefsType (tyToElab bound)
                    , any (typeBinderRefsSameIdentity dependency . fst) binders
                    ]
                refs' = distinctRefs (refs ++ dependencies)
            in if length refs' == length refs then refs else close refs'

preparedLocalAuthority
    :: Maybe SubtermResultOwnership
    -> [LocalGammaClosure]
    -> [LocalGammaConstructionCertificate]
    -> [TypeBinderRef]
    -> ElabScheme
    -> PreparedLocalRootAuthority
preparedLocalAuthority mbOwnership closures applicationCertificates ambientRefs scheme =
    PreparedLocalRootAuthority
        { plraPacketOwnership = mbOwnership
        , plraGammaClosures = closures
        , plraApplicationCertificates = applicationCertificates
        , plraAmbientBinderRefs = ambientRefs
        , plraScheme = scheme
        }

-- | Retain only aliases whose outward identity is introduced by the root
-- closure spine.  A pending local-Gamma exterior is excluded even while the
-- root closure is still provisional: exposing that identity as ambient Gamma
-- before elaboration would suppress the exact lambda/application constructor
-- that must produce its binder.  The checked owner certificate decides final
-- closure placement after elaboration; this scope only prevents the root from
-- pre-empting that construction.
--
-- Application certificates own route keys, not the root substitution values
-- stored at those keys.  A key can still route an ambient dependency in the
-- root quotient, so it is excluded from root aliases without classifying that
-- dependency identity as locally constructed.
prepareRootConstructionScope
    :: IntMap.IntMap LocalGammaClosure
    -> IntSet.IntSet
    -> PreparedRootClosure
    -> IntMap.IntMap TypeBinderRef
    -> PreparedRootConstructionScope
prepareRootConstructionScope locallyClosedGammas locallyClosedApplicationNodes rootClosure fullSubst =
    PreparedRootConstructionScope
        { prcsBinders = rootBinders
        , prcsAliases =
            IntMap.filterWithKey
                aliasBelongsToRoot
                (siSubstRefs constructionSchemeInfo)
        , prcsBinderRenames = []
        , prcsLocallyClosedBinderRefs =
            foldr insertDistinctRef [] localClosedRefs
        , prcsLocallyClosedGammas = locallyClosedGammas
        , prcsLocallyClosedApplicationNodes = locallyClosedApplicationNodes
        }
  where
    closureScheme = preparedRootClosureScheme rootClosure
    constructionSchemeInfo =
        schemeInfoFromRefSubst closureScheme fullSubst
    localGammaExteriorKeys =
        IntSet.fromList
            [ getNodeId (lgcExteriorNode closure)
            | closure <- IntMap.elems locallyClosedGammas
            ]
    localAliasKeys =
        IntSet.union
            locallyClosedApplicationNodes
            localGammaExteriorKeys
    localConsumerIdentities =
        [ lgcConsumerIdentity closure
        | closure <- IntMap.elems locallyClosedGammas
        ]
    localExteriorRefs =
        [ ref
        | key <- IntSet.toList localGammaExteriorKeys
        , Just ref <- [IntMap.lookup key (siSubstRefs constructionSchemeInfo)]
        ]
    localClosedRefs =
        preparedRootClosureLocallyConstructedBinderRefs rootClosure
            ++ localExteriorRefs
            ++ [ typeBinderRefFromIdentity
                    identity
                    (typeBinderIdentityStableName identity)
               | identity <- localConsumerIdentities
               ]
    isLocallyClosedRef ref =
        any (typeBinderRefsSameIdentity ref) localClosedRefs
    rootBinders =
        [ binder
        | binder@(ref, _) <- schemeBinderRefs (siScheme constructionSchemeInfo)
        , not (isLocallyClosedRef ref)
        ]
    rootBinderRefs = map fst rootBinders
    aliasesRootBinder ref =
        any (typeBinderRefsSameIdentity ref) rootBinderRefs
    aliasBelongsToRoot key ref =
        not (IntSet.member key localAliasKeys)
            && not (isLocallyClosedRef ref)
            && aliasesRootBinder ref

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

-- | Rebuild the construction-only base routes carried by an inherited Gamma
-- certificate.  Source projection may legitimately map the same base key to
-- a generated source identity in the published root substitution; that route
-- must not replace the frozen graph capability used to close a descendant
-- local bound.  The overlay is used only for requirement/dependency
-- classification, and two certificates may share a base key only when their
-- exact binder identities agree.
overlayInheritedGammaDependencyRoutes
    :: Reify.InheritedGammaRoutes
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
overlayInheritedGammaDependencyRoutes routes subst = do
    exactBaseRoutes <-
        foldM
            insertBaseRoute
            IntMap.empty
            (Reify.inheritedGammaRoutesEntries routes)
    pure (IntMap.union exactBaseRoutes subst)
  where
    insertBaseRoute baseRoutes route =
        let baseNode = Reify.inheritedGammaRouteBaseNode route
            baseKey = getNodeId baseNode
            inheritedRef = Reify.inheritedGammaRouteRef route
        in case IntMap.lookup baseKey baseRoutes of
            Nothing ->
                pure (IntMap.insert baseKey inheritedRef baseRoutes)
            Just existing
                | typeBinderRefsSameIdentity existing inheritedRef ->
                    pure baseRoutes
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "inherited Gamma dependency base has conflicting exact identities"
                            , "  base node: " ++ show baseNode
                            , "  first identity: " ++ show existing
                            , "  second identity: " ++ show inheritedRef
                            ]
                        )

-- | Combine the closure-derived root scope with the requirement proof that
-- owns descendant construction endpoints and their inherited dependencies.
-- A requirement binder is retained only when it is already a root binder or
-- its exact identity occurs free in the remaining closure.  This is the
-- identity-only bridge needed for a local bound such as @c >= Box Graph0 ->
-- Bool@: @c@ remains local, while the certified inherited @Graph0@ capability
-- remains ambient.  Same-spelled peers are never authority.  When closure and
-- requirement aliases disagree, only the exact route in @fullSubst@ may
-- select the construction endpoint; an unproved conflict is rejected.
prepareRootConstructionScopeWithRequirementEvidence
    :: IntMap.IntMap LocalGammaClosure
    -> IntSet.IntSet
    -> [TypeBinderRef]
    -> [(TypeBinderRef, Maybe BoundType)]
    -> IntMap.IntMap TypeBinderRef
    -> PreparedRootClosure
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError PreparedRootConstructionScope
prepareRootConstructionScopeWithRequirementEvidence
    locallyClosedGammas
    locallyClosedApplicationNodes
    exactLocalRefs
    requirementBinders
    requirementAliases
    rootClosure
    fullSubst = do
        mergedBinders <-
            foldM
                insertEvidenceBinder
                baseBinders
                retainedEvidenceBinders
        mergedAliases <-
            foldM
                (insertEvidenceAlias (map fst mergedBinders))
                baseAliases
                (IntMap.toList requirementAliases)
        pure
            baseScope0
                { prcsBinders = mergedBinders
                , prcsAliases = mergedAliases
                , prcsLocallyClosedBinderRefs = localRefs
                }
      where
        baseScope0 =
            prepareRootConstructionScope
                locallyClosedGammas
                locallyClosedApplicationNodes
                rootClosure
                fullSubst
        localRefs =
            foldr insertDistinctRef []
                ( exactLocalRefs
                    ++ prcsLocallyClosedBinderRefs baseScope0
                )
        isLocal ref =
            any (typeBinderRefsSameIdentity ref) localRefs
        baseBinders =
            filter (not . isLocal . fst) (prcsBinders baseScope0)
        baseBinderRefs = map fst baseBinders
        baseAliases =
            IntMap.filter
                (\ref -> not (isLocal ref))
                (prcsAliases baseScope0)
        closureFreeRefs =
            freeTypeVarRefsType
                (schemeToType (preparedRootClosureScheme rootClosure))
        retainedEvidenceBinders =
            [ binder
            | binder@(ref, _) <- requirementBinders
            , not (isLocal ref)
            , refMember ref baseBinderRefs
                || refMember ref closureFreeRefs
            ]

        insertEvidenceBinder binders binder@(ref, mbBound) =
            case find (typeBinderRefsSameIdentity ref . fst) binders of
                Nothing -> pure (binders ++ [binder])
                Just (_, existingBound)
                    | boundsAgree existingBound mbBound -> pure binders
                    -- An unbounded dependency proves lexical availability;
                    -- it carries no refinement that could replace a bound
                    -- already constructed for the same exact identity.
                    | isNothing mbBound -> pure binders
                    -- Conversely, explicit requirement evidence completes a
                    -- root declaration that the closure still presents as
                    -- unbounded.  Refine that declaration in place so its
                    -- existing identity/order/aliases remain authoritative.
                    | isNothing existingBound ->
                        pure (map refineMatchingBinder binders)
                    | otherwise ->
                        Left
                            ( ValidationFailed
                                [ "root construction dependency disagrees with an existing binder bound"
                                , "  binder: " ++ show ref
                                , "  existing bound: " ++ show existingBound
                                , "  dependency bound: " ++ show mbBound
                                ]
                            )
          where
            refineMatchingBinder existing@(existingRef, _)
                | typeBinderRefsSameIdentity existingRef ref =
                    (existingRef, mbBound)
                | otherwise = existing

        insertEvidenceAlias binderRefs aliases (key, ref)
            | isLocal ref = pure aliases
            | not (refMember ref binderRefs) = pure aliases
            | otherwise =
                case IntMap.lookup key aliases of
                    Nothing -> pure (IntMap.insert key ref aliases)
                    Just existing
                        | typeBinderRefsSameIdentity existing ref ->
                            pure aliases
                        | Just routed <- IntMap.lookup key fullSubst
                        , typeBinderRefsSameIdentity routed existing ->
                            pure aliases
                        | Just routed <- IntMap.lookup key fullSubst
                        , typeBinderRefsSameIdentity routed ref ->
                            pure (IntMap.insert key ref aliases)
                        | otherwise ->
                            Left
                                ( ValidationFailed
                                    [ "root construction dependency alias targets conflicting identities"
                                    , "  graph node: " ++ show (NodeId key)
                                    , "  root target: " ++ show existing
                                    , "  dependency target: " ++ show ref
                                    ]
                                )

        boundsAgree left right =
            let leftTy = maybe TBottom tyToElab left
                rightTy = maybe TBottom tyToElab right
            in alphaEqType leftTy rightTy
                || churchAwareEqType leftTy rightTy

        refMember ref = any (typeBinderRefsSameIdentity ref)

        insertDistinctRef ref refs
            | refMember ref refs = refs
            | otherwise = ref : refs

-- | Prove that one complete root requirement is emitted by the exact local
-- constructor retained at its nested or enclosing construction placement.
-- The edge set, semantic exterior, consumer identity, lexical owner, and
-- either direct-application occurrence provenance or flexible binding-tree
-- path are all part of the proof; a matching name or quotient representative
-- is deliberately insufficient.
requiredGammaBinderClosedLocally
    :: GaBindParents p
    -> IntMap.IntMap LocalGammaClosure
    -> RequiredGammaBinder
    -> Either ElabError Bool
requiredGammaBinderClosedLocally ga locallyClosedGammas requirement =
    case closures of
        [] ->
            case rgbPlacement requirement of
                RequiredGammaAtCurrentScope -> pure False
                RequiredGammaAtConstructionScope _ -> pure False
                -- A structurally nested requirement is still constructed by
                -- this root planner unless a term-local constructor retained
                -- the corresponding LocalGammaClosure proof.
                RequiredGammaAtNestedScope _ -> pure False
        closure : rest
            | length closures /= length requirementEdges ->
                invalidLocalClosure
                    "only part of a root requirement is owned by a local constructor"
                    closures
            | any (/= closure) rest ->
                invalidLocalClosure
                    "one root requirement has conflicting local-constructor proofs"
                    closures
            | edgeKeySet (lgcEdgeIds closure)
                /= edgeKeySet (rgbEdgeIds requirement) ->
                invalidLocalClosure
                    "local constructor does not own the complete root requirement edge set"
                    closures
            | lgcExteriorNode closure /= rgbExteriorNode requirement ->
                invalidLocalClosure
                    "local constructor exterior disagrees with the root requirement"
                    closures
            | lgcConsumerIdentity closure
                /= typeBinderIdentityFromNode (rgbExteriorNode requirement) ->
                invalidLocalClosure
                    "local constructor consumer disagrees with the root requirement"
                    closures
            | not
                ( directApplicationEdgeKeySet closure
                    `IntSet.isSubsetOf` edgeKeySet (lgcEdgeIds closure)
                ) ->
                invalidLocalClosure
                    "direct application provenance names an edge outside the local closure"
                    closures
            | not
                ( directApplicationOwnsRequirement closure
                    || rootRaiseMergeExteriorOwnedByScope
                        ga
                        (localGammaOwnerScope (lgcOwner closure))
                        (lgcExteriorNode closure)
                ) ->
                invalidLocalClosure
                    "local constructor has neither direct-edge nor scope ownership of the root requirement"
                    closures
            | otherwise ->
                case rgbPlacement requirement of
                    RequiredGammaAtCurrentScope ->
                        invalidLocalClosure
                            "current-scope root requirement is also claimed by a local constructor"
                            closures
                    RequiredGammaAtConstructionScope owner
                        | owner == localGammaOwnerScope (lgcOwner closure) ->
                            pure True
                        | otherwise ->
                            invalidLocalClosure
                                "construction-scope root requirement placement disagrees with its local constructor owner"
                                closures
                    RequiredGammaAtNestedScope owner
                        | owner == localGammaOwnerScope (lgcOwner closure) ->
                            pure True
                        | otherwise ->
                            invalidLocalClosure
                                "nested root requirement placement disagrees with its local constructor owner"
                                closures
  where
    requirementEdges = NonEmpty.toList (rgbEdgeIds requirement)
    closures =
        [ closure
        | edgeId <- requirementEdges
        , Just closure <-
            [IntMap.lookup (getEdgeId edgeId) locallyClosedGammas]
        ]

    edgeKeySet =
        IntSet.fromList . map getEdgeId . NonEmpty.toList

    directApplicationEdgeKeySet =
        IntSet.fromList . map getEdgeId . lgcDirectApplicationEdgeIds

    directApplicationOwnsRequirement closure =
        directApplicationClosureOwnsEdges
            closure
            (rgbEdgeIds requirement)

    invalidLocalClosure reason found =
        Left
            ( ValidationFailed
                [ reason
                , "  requirement: " ++ show requirement
                , "  closures: " ++ show found
                ]
            )

-- | Select the outward identity constructed for one requirement.  Result
-- roots are the explicit post-RaiseMerge construction endpoints and therefore
-- take precedence over the operated source and semantic exterior.  Every
-- routed result root must agree on identity; list order must never arbitrate a
-- conflict.  Sparse plans may retain only the operated route, while older
-- leaf plans can expose only the exterior route, so those are exact fallbacks
-- in that order.
requiredGammaBinderConstructionRef
    :: IntMap.IntMap TypeBinderRef
    -> RequiredGammaBinder
    -> Either ElabError TypeBinderRef
requiredGammaBinderConstructionRef constructionSubst requirement =
    case routedResultRefs of
        firstResultRef : remainingResultRefs
            | all
                (typeBinderRefsSameIdentity firstResultRef)
                remainingResultRefs ->
                pure firstResultRef
            | otherwise ->
                Left
                    ( ValidationFailed
                        [ "ordinary root construction requirement has conflicting result endpoints"
                        , "  requirement: " ++ show requirement
                        , "  result refs: " ++ show routedResultRefs
                        , "  construction substitution: " ++ show constructionSubst
                        ]
                    )
        [] ->
            case
                IntMap.lookup
                    (getNodeId (rgbOperatedRoot requirement))
                    constructionSubst
                    <|> IntMap.lookup
                        (getNodeId (rgbExteriorNode requirement))
                        constructionSubst
            of
                Just ref -> pure ref
                Nothing ->
                    Left
                        ( ValidationFailed
                            [ "ordinary root construction requirement has no outward binder"
                            , "  requirement: " ++ show requirement
                            , "  construction substitution: " ++ show constructionSubst
                            ]
                        )
  where
    routedResultRefs =
        [ ref
        | resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
        , Just ref <-
            [IntMap.lookup (getNodeId resultRoot) constructionSubst]
        ]

-- | Select exactly the explicit root-RaiseMerge Gamma binders and the binder
-- dependencies of their S(operated) bounds.  The generalized anchor's result
-- type is intentionally discarded: it exists only to run the ordinary binder
-- planner over the already-validated requirements.
prepareRequiredRootConstructionScope
    :: PresolutionView 'Presolved
    -> GaBindParents 'Presolved
    -> [(TypeBinderRef, Maybe BoundType)]
    -> IntMap.IntMap LocalGammaClosure
    -> Reify.InheritedGammaRoutes
    -> GeneralizationRequirements
    -> ElabScheme
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError PreparedRootConstructionScope
prepareRequiredRootConstructionScope presolutionView ga ambientConstructionBinders locallyClosedGammas inheritedGammaRoutes requirements constructionScheme fullSubst =
    fst
        <$> prepareRequiredRootConstructionScopeDetailed
            presolutionView
            ga
            ambientConstructionBinders
            locallyClosedGammas
            inheritedGammaRoutes
            requirements
            constructionScheme
            fullSubst

-- | The exact-root path needs both halves of the ordinary construction
-- proof: the scope that retains inherited bound dependencies, and the routed
-- construction identities that a descendant constructor owns.  Returning
-- them together prevents later source projection from reconstructing either
-- fact from a graph key or display name.
prepareRequiredRootConstructionScopeDetailed
    :: PresolutionView 'Presolved
    -> GaBindParents 'Presolved
    -> [(TypeBinderRef, Maybe BoundType)]
    -> IntMap.IntMap LocalGammaClosure
    -> Reify.InheritedGammaRoutes
    -> GeneralizationRequirements
    -> ElabScheme
    -> IntMap.IntMap TypeBinderRef
    -> Either
        ElabError
        (PreparedRootConstructionScope, [TypeBinderRef])
prepareRequiredRootConstructionScopeDetailed presolutionView ga ambientConstructionBinders locallyClosedGammas inheritedGammaRoutes requirements constructionScheme fullSubst = do
    rigidBindParents <-
        bindingToElab
            ( Binding.canonicalizeBindParentsUnder
                (pvCanonical presolutionView)
                (pvConstraint presolutionView)
            )
    classifiedRequirements <-
        traverse
            (\requirement -> do
                closedLocally <-
                    requiredGammaBinderClosedLocally
                        ga
                        locallyClosedGammas
                        requirement
                pure (requirement, closedLocally)
            )
            allRequiredBinders
    let requiredBinders =
            [ requirement
            | (requirement, False) <- classifiedRequirements
            ]
        locallyClosedBinders =
            [ requirement
            | (requirement, True) <- classifiedRequirements
            ]
    requiredRefs <- traverse requiredRef requiredBinders
    localConstructionRefs <- traverse requiredRef locallyClosedBinders
    localConstructionBounds <-
        traverse
            localConstructionBound
            (zip locallyClosedBinders localConstructionRefs)
    ( localDependencyRefs
      , inheritedAmbientBinders
      , inheritedDependencyAliases
      ) <-
        foldM
            ( collectLocalDependency
                rigidBindParents
                localConstructionRefs
                inheritedGammaRoutes
            )
            ([], [], IntMap.empty)
            (concatMap freeTypeVarRefsType localConstructionBounds)
    requiredAliases <- foldM addRequirementAliases IntMap.empty requiredBinders
    let selectedRefs =
            dependencyClosure
                ( foldr
                    insertRef
                    (foldr insertRef [] localDependencyRefs)
                    requiredRefs
                )
        selectedSchemeBinders =
            [ binder
            | binder@(ref, _) <- schemeBinders
            , refMember ref selectedRefs
            ]
        -- The reification planner deliberately omits an inherited rigid Gamma
        -- declaration from the published scheme.  The certified API retains
        -- its frozen base-key route in the construction substitution, so
        -- reproduce that exact capability here as an ambient scope entry.  It
        -- is not a root-scheme binder and no ETyAbs is constructed for it at
        -- this stage.
        selectedBinders =
            inheritedAmbientBinders ++ selectedSchemeBinders
        selectedBinderRefs = map fst selectedBinders
        availableDependencyRefs =
            map fst ambientConstructionBinders ++ selectedBinderRefs
        missingRoots =
            [ ref
            | ref <- requiredRefs
            , not (refMember ref selectedBinderRefs)
            ]
        -- An omitted scheme bound denotes the paper's bottom bound.  Compare
        -- semantic bound types instead of treating 'Nothing' as missing: this
        -- accepts exactly the unbounded forall required by S(operated) = bottom
        -- while still rejecting an omitted non-bottom Gamma bound.
        mismatchedBounds =
            [ (ref, expectedBound, actualBound)
            | (requirement, ref) <- zip requiredBinders requiredRefs
            , Just (_, mbBound) <- [findBinder ref]
            , let expectedBound = rgbOperatedType requirement
                  actualBound = maybe TBottom tyToElab mbBound
            , not
                ( alphaEqType expectedBound actualBound
                    || churchAwareEqType expectedBound actualBound
                )
            ]
        missingDependencies =
            [ dependency
            | (_, Just bound) <- selectedBinders
            , dependency <- freeTypeVarRefsType (tyToElab bound)
            , not (refMember dependency availableDependencyRefs)
            ]
        retainedSchemeAliases =
            IntMap.filter
                (`refMember` selectedBinderRefs)
                constructionSubst
        constructionScope =
            PreparedRootConstructionScope
                { prcsBinders = selectedBinders
                , prcsAliases =
                    IntMap.unions
                        [ requiredAliases
                        , inheritedDependencyAliases
                        , retainedSchemeAliases
                        ]
                , prcsBinderRenames = []
                , prcsLocallyClosedBinderRefs = localConstructionRefs
                , prcsLocallyClosedGammas = locallyClosedGammas
                , prcsLocallyClosedApplicationNodes = IntSet.empty
                }
    if null missingRoots
        && null mismatchedBounds
        && null missingDependencies
        then pure (constructionScope, localConstructionRefs)
        else
            Left
                ( ValidationFailed
                    [ "ordinary root construction Gamma is not closed by its requirement anchor"
                    , "  requirements: " ++ show requiredBinders
                    , "  required refs: " ++ show requiredRefs
                    , "  selected binders: " ++ show selectedBinders
                    , "  missing roots: " ++ show missingRoots
                    , "  mismatched bounds: " ++ show mismatchedBounds
                    , "  missing dependencies: " ++ show missingDependencies
                    , "  anchor scheme: " ++ show (siScheme constructionSchemeInfo)
                    , "  anchor substitution: " ++ show constructionSubst
                    ]
                )
  where
    allRequiredBinders = grRequiredGammaBinders requirements
    constructionSchemeInfo =
        schemeInfoFromRefSubst constructionScheme fullSubst
    schemeBinders = schemeBinderRefs (siScheme constructionSchemeInfo)
    constructionSubst = siSubstRefs constructionSchemeInfo

    requiredRef requirement =
        case requiredGammaBinderConstructionRef constructionSubst requirement of
            Right primaryRef
                | requiredBoundAgrees requirement primaryRef ->
                    pure primaryRef
                | otherwise ->
                    case typedRoleFallbackRefs requirement primaryRef of
                        [] ->
                            -- Preserve the primary construction endpoint so
                            -- the ordinary bound validation below reports the
                            -- exact malformed declaration.
                            pure primaryRef
                        [fallbackRef] ->
                            pure fallbackRef
                        fallbackRefs ->
                            Left
                                ( ValidationFailed
                                    [ "ordinary root construction requirement has multiple typed role endpoints"
                                    , "  requirement: " ++ show requirement
                                    , "  primary result ref: " ++ show primaryRef
                                    , "  typed role refs: " ++ show fallbackRefs
                                    , "  anchor scheme: "
                                        ++ show (siScheme constructionSchemeInfo)
                                    , "  anchor substitution: "
                                        ++ show constructionSubst
                                    ]
                                )
            Left (ValidationFailed messages) ->
                Left
                    ( ValidationFailed
                        ( messages
                            ++ [ "  anchor scheme: "
                                    ++ show (siScheme constructionSchemeInfo)
                               ]
                        )
                    )
            Left err -> Left err

    -- A generalized anchor can share its final result with a later
    -- requirement while still retaining the earlier requirement's exact
    -- exterior declaration.  In that case the result-root substitution is an
    -- endpoint route, not the declaration carrying this requirement's
    -- S(operated) bound.  Select the unique exact role route whose published
    -- binder carries that bound.  Node role plus the typed declaration is the
    -- authority; equal type shape alone never identifies a binder.
    typedRoleFallbackRefs requirement primaryRef =
        foldr insertDistinctRef []
            [ candidateRef
            | candidateRef <-
                maybeToList (rgbExactOperatedOccurrenceRef requirement)
                    ++ mapMaybe
                        ( \node ->
                            IntMap.lookup
                                (getNodeId node)
                                constructionSubst
                        )
                        [ rgbExteriorNode requirement
                        , rgbOperatedRoot requirement
                        ]
            , not
                ( typeBinderRefsSameIdentity
                    candidateRef
                    primaryRef
                )
            , requiredBoundAgrees requirement candidateRef
            ]

    requiredBoundAgrees requirement ref =
        case findBinder ref of
            Nothing -> False
            Just (_, mbBound) ->
                let expectedBound = rgbOperatedType requirement
                    actualBound = maybe TBottom tyToElab mbBound
                in alphaEqType expectedBound actualBound
                    || churchAwareEqType expectedBound actualBound

    -- Once 'requiredGammaBinderClosedLocally' has matched a requirement to
    -- its exact local closure and 'requiredRef' has selected that closure's
    -- declaration, the anchor bound is the constructed S'(operated)
    -- endpoint.  It can complete a provisional graph variable to a ground or
    -- higher-rank type, so it need not be alpha-equivalent to the frozen
    -- pre-construction requirement.  Derive lexical dependencies from the
    -- certified constructed bound; using the stale graph view would demand an
    -- ambient route for a variable that no longer occurs in the construction.
    localConstructionBound (requirement, ref) =
        case findBinder ref of
            Just (_, mbBound) ->
                pure (maybe TBottom tyToElab mbBound)
            Nothing ->
                Left
                    ( ValidationFailed
                        [ "locally constructed Gamma has no anchor declaration"
                        , "  requirement: " ++ show requirement
                        , "  construction ref: " ++ show ref
                        , "  anchor scheme: "
                            ++ show (siScheme constructionSchemeInfo)
                        ]
                    )

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

    -- A nested Figure 15.3.5 constructor emits its own exterior binder, but
    -- Lemma 15.3.5 still requires every free ref of that binder's S'(operated)
    -- bound to be present in the enclosing Gamma.  Resolve those ambient refs
    -- through the same construction substitution used by the anchor scheme;
    -- never pre-bind the nested exterior itself.
    collectLocalDependency rigidParents localExteriorRefs certifiedRoutes (refs, ambientBinders, aliases) dependency
        | refMember dependency localExteriorRefs =
            pure (refs, ambientBinders, aliases)
        | Just (ambientRef, _) <-
            find
                (typeBinderRefsSameIdentity dependency . fst)
                ambientConstructionBinders =
            case typeBinderRefNode dependency of
                Just liveNode -> do
                    aliases' <-
                        insertInheritedAlias
                            liveNode
                            ambientRef
                            aliases
                    pure (refs, ambientBinders, aliases')
                Nothing ->
                    pure (refs, ambientBinders, aliases)
        | Just ambientRef <-
            find
                (typeBinderRefsSameIdentity dependency)
                (Reify.inheritedGammaRoutesLexicalRefs certifiedRoutes) =
            aliasesForAmbientRef ambientRef
        | Just graphNode <- typeBinderRefNode dependency =
            case certifiedInheritedRoute dependency graphNode certifiedRoutes of
                Just route -> do
                    ambientRef <-
                        case
                            IntMap.lookup
                                ( getNodeId
                                    (Reify.inheritedGammaRouteBaseNode route)
                                )
                                constructionSubst
                        of
                            Just ref -> pure ref
                            Nothing ->
                                missingCertifiedDependency dependency route
                    aliases' <-
                        insertInheritedAlias
                            (Reify.inheritedGammaRouteLiveNode route)
                            ambientRef
                            aliases
                    if refMember ambientRef localExteriorRefs
                        then pure (refs, ambientBinders, aliases')
                        else
                            case findBinder ambientRef of
                                Just (schemeRef, _) ->
                                    pure
                                        ( insertRef schemeRef refs
                                        , ambientBinders
                                        , aliases'
                                        )
                                Nothing ->
                                    pure
                                        ( insertRef ambientRef refs
                                        , insertAmbientBinder
                                            ambientRef
                                            ambientBinders
                                        , aliases'
                                        )
                Nothing ->
                    case findBinder dependency of
                        Just (schemeRef, _) ->
                            pure
                                ( insertRef schemeRef refs
                                , ambientBinders
                                , aliases
                                )
                        Nothing ->
                            case
                                IntMap.lookup
                                    (getNodeId graphNode)
                                    constructionSubst
                            of
                                Just routedRef
                                    | refMember routedRef localExteriorRefs ->
                                        pure (refs, ambientBinders, aliases)
                                    | Just (schemeRef, _) <-
                                        findBinder routedRef ->
                                            pure
                                                ( insertRef schemeRef refs
                                                , ambientBinders
                                                , aliases
                                                )
                                    | otherwise ->
                                        missingLocalDependency
                                            dependency
                                            (Just routedRef)
                                Nothing ->
                                    case
                                        inheritedRigidDependency
                                            rigidParents
                                            dependency
                                    of
                                        Just ambientRef ->
                                            pure
                                                ( insertRef ambientRef refs
                                                , insertAmbientBinder
                                                    ambientRef
                                                    ambientBinders
                                                , aliases
                                                )
                                        Nothing ->
                                            missingLocalDependency
                                                dependency
                                                Nothing
        | Just (schemeRef, _) <- findBinder dependency =
            pure (insertRef schemeRef refs, ambientBinders, aliases)
        | otherwise = pure (refs, ambientBinders, aliases)
      where
        aliasesForAmbientRef ambientRef =
            case typeBinderRefNode dependency of
                Just liveNode -> do
                    aliases' <-
                        insertInheritedAlias
                            liveNode
                            ambientRef
                            aliases
                    pure
                        ( insertRef ambientRef refs
                        , insertAmbientBinder ambientRef ambientBinders
                        , aliases'
                        )
                Nothing ->
                    pure
                        ( insertRef ambientRef refs
                        , insertAmbientBinder ambientRef ambientBinders
                        , aliases
                        )

    certifiedInheritedRoute dependency graphNode routes =
        find routeMatches (Reify.inheritedGammaRoutesEntries routes)
      where
        routeMatches route =
            Reify.inheritedGammaRouteLiveNode route == graphNode
                && typeBinderRefsSameIdentity
                    (Reify.inheritedGammaRouteRef route)
                    dependency

    insertInheritedAlias liveNode ambientRef aliases =
        case IntMap.lookup (getNodeId liveNode) aliases of
            Nothing ->
                pure
                    ( IntMap.insert
                        (getNodeId liveNode)
                        ambientRef
                        aliases
                    )
            Just existing
                | typeBinderRefsSameIdentity existing ambientRef ->
                    pure aliases
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "inherited Gamma live route targets multiple ambient identities"
                            , "  live node: " ++ show liveNode
                            , "  first ambient ref: " ++ show existing
                            , "  second ambient ref: " ++ show ambientRef
                            ]
                        )

    missingCertifiedDependency dependency route =
        Left
            ( ValidationFailed
                [ "certified inherited Gamma dependency has no ambient base route"
                , "  dependency: " ++ show dependency
                , "  certificate: " ++ show route
                , "  anchor substitution: " ++ show constructionSubst
                ]
            )

    -- Generalization's private InheritedGammaPlan authorizes exactly these
    -- nodes: a live, unbounded TyVar with an original rigid binding and frozen
    -- base provenance.  That authority is intentionally absent from the scheme
    -- substitution because the variable is inherited rather than generalized.
    -- Re-establish the same proof before installing the pre-elaboration root
    -- scope; never infer ambient ownership from a display name or a naked
    -- canonical representative.
    inheritedRigidDependency rigidParents dependency = do
        dependencyNode <- typeBinderRefNode dependency
        let liveNode = pvCanonical presolutionView dependencyNode
        guard (liveNode == dependencyNode)
        TyVar {} <- pvLookupNode presolutionView liveNode
        guard (pvLookupVarBound presolutionView liveNode == Nothing)
        (_parent, BindRigid) <-
            IntMap.lookup
                (nodeRefKey (typeRef liveNode))
                rigidParents
        baseNode <-
            case resolveGaSolvedToBase ga liveNode of
                SolvedToBaseMapped node -> Just node
                SolvedToBaseSameDomain node -> Just node
                SolvedToBaseMissing -> Nothing
        guard
            ( case lookupNodeIn (cNodes (gaBaseConstraint ga)) baseNode of
                Just TyVar {} -> True
                _ -> False
            )
        guard
            ( not
                ( any
                    sourceKeyIsStructural
                    [ dependencyNode
                    , liveNode
                    , baseNode
                    ]
                )
            )
        pure dependency

    sourceKeyIsStructural node =
        case
            IntMap.lookup
                (getNodeId node)
                (grSourceBinderRefs requirements)
        of
            Just sourceRef ->
                isJust
                    ( typeBinderIdentityStructural
                        (typeBinderRefIdentity sourceRef)
                    )
            Nothing -> False

    insertAmbientBinder ref binders
        | any (typeBinderRefsSameIdentity ref . fst) binders = binders
        | otherwise = binders ++ [(ref, Nothing)]

    missingLocalDependency dependency mbRoutedRef =
        Left
            ( ValidationFailed
                [ "locally constructed Gamma bound has no ambient dependency route"
                , "  dependency: " ++ show dependency
                , "  routed ref: " ++ show mbRoutedRef
                , "  anchor scheme: " ++ show (siScheme constructionSchemeInfo)
                , "  anchor substitution: " ++ show constructionSubst
                ]
            )

    addRequirementAliases aliases requirement = do
        outwardRef <- requiredRef requirement
        foldM
            (insertRequiredAlias outwardRef)
            aliases
            ( rgbOperatedRoot requirement
                : rgbExteriorNode requirement
                : NonEmpty.toList (rgbResultRoots requirement)
            )

    insertRequiredAlias outwardRef aliases node =
        let nodeKey = getNodeId node
        in case IntMap.lookup nodeKey aliases of
            Nothing -> pure (IntMap.insert nodeKey outwardRef aliases)
            Just existing
                | typeBinderRefsSameIdentity existing outwardRef -> pure aliases
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "ordinary root construction requirements disagree on one graph alias"
                            , "  node: " ++ show node
                            , "  first binder: " ++ show existing
                            , "  second binder: " ++ show outwardRef
                            ]
                        )

    dependencyClosure = go
      where
        go refs =
            let dependencies =
                    [ dependency
                    | (ref, Just bound) <- schemeBinders
                    , refMember ref refs
                    , dependency <- freeTypeVarRefsType (tyToElab bound)
                    , any (typeBinderRefsSameIdentity dependency . fst) schemeBinders
                    ]
                refs' = foldr insertRef refs dependencies
            in if length refs' == length refs then refs else go refs'

    findBinder ref =
        find (typeBinderRefsSameIdentity ref . fst) schemeBinders

    insertRef ref refs
        | refMember ref refs = refs
        | otherwise = ref : refs

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Select the gen node that owns the pipeline root scheme, rather than the
-- nearest gen ancestor of the root type node.  A top-level expression whose
-- outer constructor is @let@ deliberately records the same trivial result as
-- a scheme of both the let-expression gen and the enclosing definition gen.
-- The local @ALetF@ translation introduces the former Gamma; root closure must
-- introduce the latter.  Choosing the nearest ancestor silently drops named
-- nodes bound on the definition gen, leaving descendant Hyp computations out
-- of scope.
--
-- Scheme membership is construction authority here.  When the root is listed
-- by nested scheme owners, the outermost listed owner is the definition
-- boundary; it is the candidate with the shortest path to the binding-tree
-- root.  Roots without explicit scheme membership retain the ordinary
-- Definition 15.3.2 scope lookup.
preparedRootSchemeScope
    :: PreparedGeneralizationArtifact
    -> NodeId
    -> Either ElabError NodeRef
preparedRootSchemeScope artifact sourceRoot =
    case ownerPaths of
        [] ->
            bindingToElab $
                resolveCanonicalScope
                    baseConstraint
                    (pgaPresolutionView artifact)
                    (pgaRedirects artifact)
                    sourceRoot
        _ ->
            pure
                ( GenRef
                    ( fst
                        ( minimumBy
                            (comparing (\(owner, path) -> (length path, getGenNodeId owner)))
                            ownerPaths
                        )
                    )
                )
  where
    bindParents = gaBindParentsBase (pgaBindParentsGa artifact)
    baseConstraint = gaBaseConstraint (pgaBindParentsGa artifact)
    rootOwners =
        [ gnId gen
        | gen <- NodeAccess.allGenNodes baseConstraint
        , sourceRoot `elem` gnSchemes gen
        ]
    ownerPaths =
        [ (owner, path)
        | owner <- rootOwners
        , Right path <- [bindingPathToRootLocal bindParents (GenRef owner)]
        ]

preparedScopeRootForBoundary
    :: PreparedGeneralizationArtifact
    -> EdgeId
    -> NodeId
    -> Either ElabError NodeRef
preparedScopeRootForBoundary artifact edgeId fallbackNode =
    resolveConstructionScopeForBoundary
        (pgaAnnNodeCanonical artifact)
        (pgaBindParentsGa artifact)
        (pgaScopeOverrides artifact)
        edgeId
        fallbackNode

-- | Binder declarations owned by a source annotation that transparently
-- encloses the root currently being constructed.  These declarations are in
-- scope while the annotation's child is checked, so a local Gamma bound may
-- depend on them without promoting them to root-owned source binders.
--
-- Stop at every non-transparent constructor.  In particular, an annotation
-- in a lambda body or let sibling must be installed by that annotation's own
-- construction path and must never become ambient to the enclosing root.
preparedTransparentRootSourceAnnotationBinders
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
preparedTransparentRootSourceAnnotationBinders artifact = collect
  where
    collect ann =
        case ann of
            AAnn _ _ edgeId -> do
                expectedType <-
                    case
                        IntMap.lookup
                            (getEdgeId edgeId)
                            (preparedAnnotationExpectedTypesByEdge artifact)
                    of
                        Just ty -> pure ty
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "transparent root source annotation has no edge-owned expected type"
                                    , "  edge: " ++ show edgeId
                                    ]
                                )
                pure (schemeBinderRefs (schemeFromType expectedType))
            ALetScope inner _ _ -> collect inner
            _ -> pure []

-- | Extend the global source-binder carrier with only the compiler-exact
-- structural projections owned by the supplied annotated subtree.  Keeping
-- the edge-indexed projections on the prepared artifact lets root and subterm
-- generalization consume the same proof without making one occurrence's
-- instantiation visible to an unrelated sibling.
preparedSourceBinderRefsForAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
preparedSourceBinderRefsForAnn artifact ann = do
    plansByEdge <- pgaCompilerExactEdgePlans artifact
    localSourceDeclarations <-
        sourceAnnotationDeclarationIdentitiesForAnn artifact ann
    let inheritedSourceRefs =
            IntMap.filter
                ( \ref ->
                    let identity = typeBinderRefIdentity ref
                    in isJust (typeBinderIdentityStructural identity)
                        || Set.notMember identity localSourceDeclarations
                )
                (pgaSourceBinderRefs artifact)
    collect plansByEdge inheritedSourceRefs ann
  where
    collect plansByEdge refs current =
        case current of
            AExactAnn inner _ _ edgeId -> do
                edgePlan <-
                    case IntMap.lookup (getEdgeId edgeId) plansByEdge of
                        Just plan -> pure plan
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "compiler exact annotated scope has no edge-local plan"
                                    , "  edge: " ++ show edgeId
                                    ]
                                )
                refs' <-
                    mergeCompilerExactConstructionBinderRefs
                        refs
                        (ceepConstructionRefs edgePlan)
                collect plansByEdge refs' inner
            AAnn inner _ _ -> collect plansByEdge refs inner
            ALetScope inner _ _ -> collect plansByEdge refs inner
            AUnfold inner _ _ -> collect plansByEdge refs inner
            ALam{} -> pure refs
            AApp{} -> pure refs
            ALet{} -> pure refs
            AResolvedVar{} -> pure refs
            ALit{} -> pure refs

-- | Recover the occurrence-local routes proved by source annotation edges.
-- The annotation target carries free source variables, while the opened
-- source body identifies both free and locally declared occurrences in the
-- producer.  These routes are consumed only while planning nested construction
-- Gamma; they do not promote the annotation's local declarations to root
-- ambient binders.
preparedSourceAnnotationOccurrenceRefsForAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
preparedSourceAnnotationOccurrenceRefsForAnn artifact = collect
  where
    baseConstraint = gaBaseConstraint (pgaBindParentsGa artifact)

    collect ann =
        case ann of
            AResolvedVar{} -> pure IntMap.empty
            ALit{} -> pure IntMap.empty
            ALam _ _ _ _ body _ _ ->
                collect body
            AApp fun arg _ _ _ ->
                mergeChildren fun arg
            ALet _ _ _ _ _ _ rhs body _ ->
                mergeChildren rhs body
            AExactAnn inner _ _ _ ->
                collect inner
            AAnn inner annotationNode edgeId -> do
                nested <- collect inner
                expectedType <-
                    case
                        IntMap.lookup
                            (getEdgeId edgeId)
                            (preparedAnnotationExpectedTypesByEdge artifact)
                    of
                        Just ty -> pure ty
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "source annotation has no edge-owned expected type"
                                    , "  edge: " ++ show edgeId
                                    ]
                                )
                graphType <-
                    TypeReify.reifyTypeWithRefsNoFallbackOnConstraint
                        baseConstraint
                        IntMap.empty
                        annotationNode
                let producerNode =
                        pgaAnnNodeCanonical artifact (annNode inner)
                    producerTarget =
                        fromMaybe
                            producerNode
                            ( pvLookupVarBound
                                (pgaPresolutionView artifact)
                                producerNode
                            )
                producerTypeRaw <-
                    TypeReify.reifyTypeWithRefsNoFallback
                        (pgaPresolutionView artifact)
                        IntMap.empty
                        producerTarget
                let producerType =
                        inlineBoundVarsTypeWithCanonical
                            (pgaAnnNodeCanonical artifact)
                            (pgaPresolutionView artifact)
                            producerTypeRaw
                let targetOccurrencePairs =
                        fromMaybe
                            []
                            ( alignedFreeTypeOccurrencePairs
                                expectedType
                                graphType
                            )
                    openedExpectedType =
                        schemeBody (schemeFromType expectedType)
                    producerOccurrencePairs =
                        fromMaybe
                            []
                            ( alignedTypeOccurrencePairs
                                openedExpectedType
                                producerType
                            )
                targetRefs <-
                    foldM
                        insertOccurrenceRoute
                        IntMap.empty
                        targetOccurrencePairs
                producerRefs <-
                    foldM
                        insertOccurrenceRoute
                        IntMap.empty
                        producerOccurrencePairs
                let directRefs =
                        enterSourceAnnotationOccurrenceRefs
                            targetRefs
                            producerRefs
                let expandedRefs =
                        expandPreparedSourceBinderRefs
                            (pgaBindParentsGa artifact)
                            (preparedIdentityRepresentative artifact)
                            directRefs
                -- The inner annotation is the narrower lexical source
                -- domain. Each annotation has already proved its own
                -- one-to-one occurrence map, so an inner route shadows an
                -- outer route for a solved node without weakening
                -- same-domain conflict rejection.
                pure
                    ( enterSourceAnnotationOccurrenceRefs
                        expandedRefs
                        nested
                    )
            ALetScope inner _ _ ->
                collect inner
            AUnfold inner _ _ ->
                collect inner

    mergeChildren left right = do
        leftRefs <- collect left
        rightRefs <- collect right
        pure
            ( mergeSiblingSourceAnnotationOccurrenceRefs
                leftRefs
                rightRefs
            )

    insertOccurrenceRoute refs (sourceRef, graphRef) =
        case typeBinderRefNode graphRef of
            Nothing ->
                Left
                    ( ValidationFailed
                        [ "source annotation free occurrence has no graph identity"
                        , "  source binder: " ++ show sourceRef
                        , "  graph occurrence: " ++ show graphRef
                        ]
                    )
            Just graphNode ->
                mergeSourceAnnotationOccurrenceRefs
                    refs
                    (IntMap.singleton (getNodeId graphNode) sourceRef)

-- | Enter a narrower source-annotation occurrence domain. The target and
-- producer maps for each annotation are independently injective; a producer
-- annotation may nevertheless reuse a solved graph node also mentioned by
-- its enclosing target. The local identity is the lexical authority for that
-- subtree, just as an inner source binder shadows its outer declaration.
enterSourceAnnotationOccurrenceRefs
    :: IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
enterSourceAnnotationOccurrenceRefs inherited local =
    IntMap.union local inherited

-- | Join occurrence routes from sibling expression domains. A solved graph
-- node may be reused by distinct source binders in the two branches (for
-- example, two independently quantified higher-rank arguments). Neither
-- sibling is lexical authority outside its own branch, so retain only routes
-- on which both branches agree. Each branch has already rejected conflicts
-- inside its own lexical domain.
mergeSiblingSourceAnnotationOccurrenceRefs
    :: IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
mergeSiblingSourceAnnotationOccurrenceRefs left right =
    IntMap.mergeWithKey
        ( \_ leftRef rightRef ->
            if typeBinderRefsSameIdentity leftRef rightRef
                then Just leftRef
                else Nothing
        )
        id
        id
        left
        right

mergeSourceAnnotationOccurrenceRefs
    :: IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
mergeSourceAnnotationOccurrenceRefs existing projected =
    foldM insertProjected existing (IntMap.toList projected)
  where
    insertProjected refs (graphKey, sourceRef) =
        case IntMap.lookup graphKey refs of
            Nothing ->
                pure (IntMap.insert graphKey sourceRef refs)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef sourceRef ->
                    pure refs
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "source annotation occurrence maps to multiple source identities"
                            , "  graph node: " ++ show (NodeId graphKey)
                            , "  first source binder: " ++ show existingRef
                            , "  second source binder: " ++ show sourceRef
                            ]
                        )

-- | Direct source authority for one annotated root. Compiler-exact
-- construction routes are direct for their exact edge. Solved/copy aliases
-- introduced by 'expandPreparedSourceBinderRefs' are intentionally absent.
preparedDirectSourceBinderKeysForAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError IntSet.IntSet
preparedDirectSourceBinderKeysForAnn artifact ann = do
    plansByEdge <- pgaCompilerExactEdgePlans artifact
    collect plansByEdge (pgaDirectSourceBinderKeys artifact) ann
  where
    collect plansByEdge keys current =
        case current of
            AExactAnn inner _ _ edgeId -> do
                edgePlan <-
                    case IntMap.lookup (getEdgeId edgeId) plansByEdge of
                        Just plan -> pure plan
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "compiler exact annotated scope has no edge-local plan"
                                    , "  edge: " ++ show edgeId
                                    ]
                                )
                collect
                    plansByEdge
                    ( IntSet.union
                        keys
                        (IntMap.keysSet (ceepConstructionRefs edgePlan))
                    )
                    inner
            AAnn inner _ _ -> collect plansByEdge keys inner
            ALetScope inner _ _ -> collect plansByEdge keys inner
            AUnfold inner _ _ -> collect plansByEdge keys inner
            ALam{} -> pure keys
            AApp{} -> pure keys
            ALet{} -> pure keys
            AResolvedVar{} -> pure keys
            ALit{} -> pure keys

-- | Source kappa annotations own the declarations in their expected types.
-- Those identities are available globally as graph-to-source provenance, but
-- they are not inherited Gamma binders of an enclosing root.  Publishing one
-- through the ambient source sidecar would let root generalization capture a
-- locally quantified parameter before the annotation constructs its forall.
-- Structural self/result identities remain constructor evidence: they rebuild
-- their own mu/forall declarations and the reification plan excludes them from
-- ambient Gamma authority.
--
-- Compiler-exact annotations are different: their edge plan is the ABI proof
-- that introduces exact declarations at that boundary, so this traversal only
-- removes declarations owned by source 'AAnn' occurrences.  The caller then
-- merges compiler-exact construction refs back in while following the exact
-- wrapper path.
sourceAnnotationDeclarationIdentitiesForAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError (Set.Set TypeBinderIdentity)
sourceAnnotationDeclarationIdentitiesForAnn artifact = collect
  where
    collect ann =
        case ann of
            AResolvedVar{} -> pure Set.empty
            ALit{} -> pure Set.empty
            ALam _ _ _ _ body _ _ -> collect body
            AApp fun arg _ _ _ ->
                Set.union <$> collect fun <*> collect arg
            ALet _ _ _ _ _ _ rhs body _ ->
                Set.union <$> collect rhs <*> collect body
            AExactAnn inner _ _ _ -> collect inner
            AAnn inner _ edgeId -> do
                expectedType <-
                    case
                        IntMap.lookup
                            (getEdgeId edgeId)
                            (preparedAnnotationExpectedTypesByEdge artifact)
                    of
                        Just ty -> pure ty
                        Nothing ->
                            Left
                                ( ValidationFailed
                                    [ "source annotation has no edge-owned expected type"
                                    , "  edge: " ++ show edgeId
                                    ]
                                )
                nested <- collect inner
                pure
                    ( Set.union
                        ( Set.fromList
                            ( map
                                typeBinderRefIdentity
                                (typeBinderDeclarationRefs expectedType)
                            )
                        )
                        nested
                    )
            ALetScope inner _ _ -> collect inner
            AUnfold inner _ _ -> collect inner

-- | Keep freshly constructed Gamma consumers distinct from lexical source
-- binders even when an instantiated source occurrence shares their solved
-- alias class. The own-node exception is available only to a direct source
-- declaration key; solved/copy expansion is carrier evidence, not declaration
-- ownership.
--
-- A nested constructor's locally owned key is excluded even if it also has
-- direct source provenance.
projectPreparedSourceBinderSubstExceptWithLocalKeys
    :: Set.Set TypeBinderIdentity
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
projectPreparedSourceBinderSubstExceptWithLocalKeys
    protectedIdentities
    directSourceKeys
    locallyConstructedKeys
    preferredRefs
    existingSubst = do
    sourceProjection <-
        foldM
            insertSourceProjection
            Map.empty
            [ (graphRef, sourceRef, graphKey)
            | (graphKey, sourceRef) <- IntMap.toList preferredRefs
            , isJust
                ( typeBinderIdentityGeneratedUnique
                    (typeBinderRefIdentity sourceRef)
                )
            , Just graphRef <- [IntMap.lookup graphKey existingSubst]
            , Set.notMember
                (typeBinderRefIdentity graphRef)
                protectedIdentities
                || ( typeBinderRefNode graphRef == Just (NodeId graphKey)
                        && IntSet.member graphKey directSourceKeys
                        && IntSet.notMember graphKey locallyConstructedKeys
                   )
            ]
    let projectedExistingSubst =
            IntMap.map
                (\graphRef ->
                    Map.findWithDefault
                        graphRef
                        (typeBinderRefIdentity graphRef)
                        sourceProjection
                )
                existingSubst
    pure projectedExistingSubst
  where
    insertSourceProjection projections (graphRef, sourceRef, graphKey) =
        case Map.lookup graphIdentity projections of
            Nothing ->
                pure (Map.insert graphIdentity sourceRef projections)
            Just existingSourceRef
                | typeBinderRefsSameIdentity existingSourceRef sourceRef ->
                    pure projections
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "source-identity projection has conflicting identities for one substitution alias class"
                            , "  shared graph key: " ++ show (NodeId graphKey)
                            , "  graph-local identity: " ++ show graphIdentity
                            , "  first source identity: "
                                ++ show (typeBinderRefIdentity existingSourceRef)
                            , "  second source identity: "
                                ++ show (typeBinderRefIdentity sourceRef)
                            ]
                        )
      where
        graphIdentity = typeBinderRefIdentity graphRef

-- | Project only free occurrences backed by an exact source declaration copy.
-- Generalization intentionally omits inherited external binders from its local
-- forall spine, so such a copy can survive as a free graph identity even when
-- the source sidecar already records its lexical identity.  Restricting the
-- carrier to direct declaration keys and resolving with the identity
-- representative makes this an own-node proof: expanded aliases, canonical
-- peers, locally constructed Gamma slots, and locally bound declarations
-- cannot be rewritten here.
projectPreparedRootFreeSourceDeclarationCopies
    :: Set.Set TypeBinderIdentity
    -> IntSet.IntSet
    -> IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> ElabScheme
projectPreparedRootFreeSourceDeclarationCopies protectedIdentities directSourceKeys locallyConstructedKeys sourceBinderRefs =
    schemeFromType
        . resolveSourceBinderAliasesInType
            id
            exactDeclarationRefs
        . schemeToType
  where
    exactDeclarationRefs =
        IntMap.filterWithKey
            ( \key _ ->
                IntSet.member key directSourceKeys
                    && IntSet.notMember key locallyConstructedKeys
                    && Set.notMember
                        (typeBinderIdentityFromNode (NodeId key))
                        protectedIdentities
            )
            sourceBinderRefs

applyPreparedRootSourceTypeBinderIdentities
    :: PreparedRootGeneralization
    -> Either ElabError PreparedRootGeneralization
applyPreparedRootSourceTypeBinderIdentities rootGeneralization = do
    preferredSubst <-
        projectPreparedSourceBinderSubstExceptWithLocalKeys
            (prgConstructedGammaIdentities rootGeneralization)
            (prgDirectSourceBinderKeys rootGeneralization)
            locallyConstructedKeys
            (prgSourceBinderRefs rootGeneralization)
            (prgSubst rootGeneralization)
    applyPreparedRootIdentitySubst preferredSubst rootGeneralization
  where
    constructionScope = prgConstructionScope rootGeneralization
    locallyConstructedKeys =
        IntSet.union
            (prcsLocallyClosedApplicationNodes constructionScope)
            ( IntSet.fromList
                [ getNodeId (lgcExteriorNode closure)
                | closure <-
                    IntMap.elems
                        (prcsLocallyClosedGammas constructionScope)
                ]
            )

-- | Install a construction-authoritative root substitution in the result
-- scheme and its closure as one operation.  Term alias discovery can identify
-- a root placeholder with a binder already emitted by a local packet; updating
-- only 'prgScheme' would leave 'prgClosure' to emit the stale placeholder as a
-- second type abstraction.
applyPreparedRootBinderIdentities
    :: IntMap.IntMap TypeBinderRef
    -> PreparedRootGeneralization
    -> Either ElabError PreparedRootGeneralization
applyPreparedRootBinderIdentities = applyPreparedRootIdentitySubst

-- | Project a root-local graph binder to the canonical exact source identity
-- before root closure emits its type abstraction.  This covers the case where
-- generalization has introduced a vacuous graph binder at the exact binder's
-- lexical position while the scheme body already refers to the semantic
-- source identity.  The proof is computed from the prepared exact contract
-- and the root scheme, never from the finished term.
applyPreparedCompilerExactRootBinderIdentities
    :: PreparedGeneralizationArtifact
    -> EdgeId
    -> PreparedRootGeneralization
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError PreparedRootGeneralization
applyPreparedCompilerExactRootBinderIdentities artifact exactEdge rootGeneralization sourcePreferredSubst = do
    exactType <- preparedCompilerExactExpectedType artifact exactEdge
    preferredSubst <-
        prepareCompilerExactRootBinderSubst
            exactType
            (prgScheme rootGeneralization)
            sourcePreferredSubst
    applyPreparedRootIdentitySubst preferredSubst rootGeneralization

-- | Extend a root substitution only with captures proved by the exact/root
-- forall relation.  A captured graph binder may be absent from the existing
-- substitution because it is vacuous in the root body; the root scheme is the
-- domain proof that permits adding that one key.  Seeding its graph identity
-- before source projection also carries the exact identity through every
-- existing alias of the same graph binder.
prepareCompilerExactRootBinderSubst
    :: ElabType
    -> ElabScheme
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
prepareCompilerExactRootBinderSubst exactType rootScheme existingSubst = do
    directCaptures <-
        compilerExactRootCaptureBinderRefs
            exactType
            (schemeToType rootScheme)
    projectedCaptures <-
        compilerExactRootCaptureBinderRefs
            exactType
            ( schemeToType
                ( siScheme
                    (schemeInfoFromRefSubst rootScheme existingSubst)
                )
            )
    captures <-
        mergeCompilerExactConstructionBinderRefs
            directCaptures
            projectedCaptures
    seededSubst <-
        foldM
            seedCapture
            existingSubst
            (IntMap.toList captures)
    projectPreparedSourceBinderSubstExceptWithLocalKeys
        Set.empty
        IntSet.empty
        IntSet.empty
        captures
        seededSubst
  where
    seedCapture subst (graphKey, exactRef) =
        let graphNode = NodeId graphKey
            graphRef =
                typeBinderRefFromIdentity
                    (typeBinderIdentityFromNode graphNode)
                    (typeBinderRefName exactRef)
        in case IntMap.lookup graphKey subst of
            Nothing -> pure (IntMap.insert graphKey graphRef subst)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef graphRef
                    || typeBinderRefsSameIdentity existingRef exactRef ->
                        pure (IntMap.insert graphKey graphRef subst)
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "compiler exact root binder conflicts with existing root binder route"
                            , "  root binder: " ++ show graphNode
                            , "  existing binder: " ++ show existingRef
                            , "  exact binder: " ++ show exactRef
                            ]
                        )

-- | A generated exact binder can already occur free in the generalized root
-- body while a graph-local placeholder occupies the corresponding forall
-- slot.  The placeholder is authoritative only when it is vacuous, both
-- bounds agree, and the exact identity is present in the remainder.  These
-- conditions turn the positional relation into an identity proof rather than
-- a name- or index-based guess.
compilerExactRootCaptureBinderRefs
    :: ElabType
    -> ElabType
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
compilerExactRootCaptureBinderRefs exactType rootType =
    go IntMap.empty rootType exactType
  where
    go refs source target =
        case (source, target) of
            ( TForallRef sourceRef sourceBound sourceBody
              , TForallRef targetRef targetBound targetBody
              )
                | typeBinderRefsSameIdentity sourceRef targetRef ->
                    go refs sourceBody targetBody
                | Just sourceNode <- typeBinderRefNode sourceRef
                , isJust
                    ( typeBinderIdentityGeneratedUnique
                        (typeBinderRefIdentity targetRef)
                    )
                , mentions sourceRef sourceBody
                , mentions targetRef targetBody
                , equivalentBounds sourceBound targetBound
                , let projectedSourceBody =
                        substTypeCaptureRef
                            sourceRef
                            (TVarRef targetRef)
                            sourceBody
                , alphaEqType projectedSourceBody targetBody
                    || churchAwareEqType projectedSourceBody targetBody -> do
                    refs' <- insertCapture sourceNode targetRef refs
                    go refs' projectedSourceBody targetBody
                | Just sourceNode <- typeBinderRefNode sourceRef
                , isJust
                    ( typeBinderIdentityGeneratedUnique
                        (typeBinderRefIdentity targetRef)
                    )
                , not (mentions sourceRef sourceBody)
                , mentions targetRef sourceBody
                , equivalentBounds sourceBound targetBound -> do
                    refs' <- insertCapture sourceNode targetRef refs
                    go refs' sourceBody targetBody
            _ -> pure refs

    mentions ref =
        any (typeBinderRefsSameIdentity ref) . freeTypeVarRefsType

    equivalentBounds left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

    insertCapture sourceNode targetRef refs =
        let sourceKey = getNodeId sourceNode
        in case IntMap.lookup sourceKey refs of
            Nothing -> pure (IntMap.insert sourceKey targetRef refs)
            Just existingTarget
                | typeBinderRefsSameIdentity existingTarget targetRef ->
                    pure refs
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "compiler exact root binder has conflicting source identities"
                            , "  root binder: " ++ show sourceNode
                            , "  first exact binder: " ++ show existingTarget
                            , "  second exact binder: " ++ show targetRef
                            ]
                        )

applyPreparedRootIdentitySubst
    :: IntMap.IntMap TypeBinderRef
    -> PreparedRootGeneralization
    -> Either ElabError PreparedRootGeneralization
applyPreparedRootIdentitySubst preferredSubst rootGeneralization = do
    preferredScheme0 <-
        applyPreparedRootBinderSubst
            "result"
            preferredSubst
            (prgScheme rootGeneralization)
    preferredClosure <-
        quotientPreparedRootClosureIdentities
            preferredSubst
            (prgClosure rootGeneralization)
    preferredScheme <-
        validateSchemeClosure
            "source-projected root result"
            (preparedRootClosureSchemeAuthority preferredClosure)
            preferredScheme0
    preferredLocalGammaClosures <-
        traverse
            (projectPreparedLocalGammaClosure preferredSubst)
            ( prcsLocallyClosedGammas
                (prgConstructionScope rootGeneralization)
            )
    preferredConstructionScope0 <-
        prepareRootConstructionScopeWithRequirementEvidence
            preferredLocalGammaClosures
            ( prcsLocallyClosedApplicationNodes
                originalConstructionScope
            )
            (prcsLocallyClosedBinderRefs originalConstructionScope)
            (prcsBinders originalConstructionScope)
            (prcsAliases originalConstructionScope)
            preferredClosure
            preferredSubst
    preferredConstructionScope <-
        publishRootSourceBinderAliases
            (prgSubst rootGeneralization)
            (prgClosure rootGeneralization)
            preferredClosure
            (prgSourceBinderRefs rootGeneralization)
            preferredConstructionScope0
    pure
        rootGeneralization
        { prgScheme = preferredScheme
        , prgClosure = preferredClosure
        , prgSubst = preferredSubst
        , prgConstructionScope = preferredConstructionScope
        }
  where
    originalConstructionScope =
        prgConstructionScope rootGeneralization

-- | Publish every root-local graph occurrence of a source binder that the
-- root construction scope itself introduces.  Root generalization may select
-- one graph representative for the forall spine while a descendant evidence
-- binding still mentions another occurrence of that same lexical source
-- binder.  The source sidecar is the construction proof connecting those
-- occurrences; retaining only the spine representative would let the
-- descendant environment reintroduce a graph-local identity after the root
-- Gamma has already chosen the outward source identity.
--
-- Only identities present in the root binder spine are admitted.  Therefore
-- a source occurrence owned by a locally closed child construction cannot be
-- made visible early merely because it appears in the root-local sidecar.
publishRootSourceBinderAliases
    :: IntMap.IntMap TypeBinderRef
    -> PreparedRootClosure
    -> PreparedRootClosure
    -> IntMap.IntMap TypeBinderRef
    -> PreparedRootConstructionScope
    -> Either ElabError PreparedRootConstructionScope
publishRootSourceBinderAliases originalSubst originalClosure projectedClosure sourceBinderRefs scope = do
    aliases <-
        reconcileRootSourceBinderAliases
            (preparedRootClosureBinderRefs originalClosure)
            (preparedRootClosureBinderRefs projectedClosure)
            originalSubst
            (IntMap.filter rootBinderMember sourceBinderRefs)
            (prcsAliases scope)
    pure scope {prcsAliases = aliases}
  where
    rootBinderRefs = map fst (prcsBinders scope)
    rootBinderMember ref =
        any (typeBinderRefsSameIdentity ref) rootBinderRefs

    preparedRootClosureBinderRefs closure =
        case closure of
            PreparedWholeRootClosure _ scheme ->
                map fst (schemeBinderRefs scheme)
            PreparedTopologyPacketRootClosure _ inner ->
                preparedRootClosureBinderRefs inner
            PreparedLocalRootClosure authority scheme ->
                map fst (schemeBinderRefs scheme)
                    ++ map fst
                        ( schemeBinderRefs
                            (preparedLocalRootAuthorityScheme authority)
                        )
            PreparedInterleavedLocalRootClosure _ _ scheme ->
                map fst (schemeBinderRefs scheme)

-- | Reconcile graph aliases with the binder projection performed while
-- rebuilding a root closure.  A conflicting alias may be replaced only when
-- it names a binder owned by the pre-projection closure and the source
-- identity is now owned by the rebuilt closure.  Ordinarily the old identity
-- disappears from the rebuilt closure.  If its declaration is still present
-- as representation lag, replacement additionally requires either that
-- identity's own graph node or the original solver substitution to route the
-- source-sidecar key to it.  The latter is the construction proof for a
-- surviving quotient representative; the scoped alias map alone is not.
-- An independent surviving binder therefore remains a conflict.  No spelling
-- or positional fallback participates.
reconcileRootSourceBinderAliases
    :: [TypeBinderRef]
    -> [TypeBinderRef]
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
reconcileRootSourceBinderAliases originalBinderRefs projectedBinderRefs originalSubst sourceBinderRefs aliases =
    foldM
        insertSourceAlias
        aliases
        [ (nodeKey, sourceRef)
        | (nodeKey, sourceRef) <- IntMap.toList sourceBinderRefs
        , refMember sourceRef projectedBinderRefs
        ]
  where
    insertSourceAlias currentAliases (nodeKey, sourceRef) =
        case IntMap.lookup nodeKey currentAliases of
            Nothing -> pure (IntMap.insert nodeKey sourceRef currentAliases)
            Just existingRef
                | typeBinderRefsSameIdentity existingRef sourceRef ->
                    pure currentAliases
                | binderProjectionProved nodeKey existingRef sourceRef ->
                    pure (IntMap.insert nodeKey sourceRef currentAliases)
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "root construction source alias conflicts with its prepared Gamma"
                            , "  graph node: " ++ show (NodeId nodeKey)
                            , "  prepared binder: " ++ show existingRef
                            , "  source binder: " ++ show sourceRef
                            ]
                        )

    binderProjectionProved nodeKey existingRef sourceRef =
        refMember existingRef originalBinderRefs
            && refMember sourceRef projectedBinderRefs
            && ( not (refMember existingRef projectedBinderRefs)
                    || typeBinderRefNode existingRef == Just (NodeId nodeKey)
                    || originalSubstitutionRoutes nodeKey existingRef
               )

    originalSubstitutionRoutes nodeKey existingRef =
        maybe
            False
            (typeBinderRefsSameIdentity existingRef)
            (IntMap.lookup nodeKey originalSubst)

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Keep the edge-local packet proof in the same outward identity quotient as
-- the root it helps construct.  The closure's exterior and consumer identity
-- remain graph-domain ownership evidence; only its pending owner scheme is
-- projected.  Otherwise a prepared packet can still advertise @Graph n@
-- after the root and the actual owner have both been constructed with the
-- authoritative source identity for @n@.
projectPreparedLocalGammaClosure
    :: IntMap.IntMap TypeBinderRef
    -> LocalGammaClosure
    -> Either ElabError LocalGammaClosure
projectPreparedLocalGammaClosure preferredSubst closure = do
    ownerScheme <- traverse projectOwnerScheme (lgcOwnerPendingScheme closure)
    pure closure {lgcOwnerPendingScheme = ownerScheme}
  where
    projectOwnerScheme schemeInfo = do
        projectedSubst <-
            projectPreparedSourceBinderSubstExceptWithLocalKeys
                Set.empty
                IntSet.empty
                IntSet.empty
                preferredSubst
                (siSubstRefs schemeInfo)
        projectedScheme <-
            quotientPreparedBinderIdentities
                "local Gamma closure"
                ( siScheme
                    ( schemeInfoFromRefSubst
                        (siScheme schemeInfo)
                        projectedSubst
                    )
                )
        pure (schemeInfoFromRefSubst projectedScheme projectedSubst)

-- | Install an authoritative graph-to-source binder substitution before
-- quotienting repeated semantic identities.  This is the construction-time
-- proof used for the result scheme; neither the elaborated term nor its final
-- shape participates in the decision.
applyPreparedRootBinderSubst
    :: String
    -> IntMap.IntMap TypeBinderRef
    -> ElabScheme
    -> Either ElabError ElabScheme
applyPreparedRootBinderSubst role subst scheme = do
    projectedScheme <-
        quotientPreparedBinderIdentities
            role
            (siScheme (schemeInfoFromRefSubst scheme subst))
    case orderSourceProjectedSchemeBinders role projectedScheme of
        Right orderedScheme -> Right orderedScheme
        Left cause -> Left (ValidationFailed [cause])

-- | Collapse repeated binders only after graph nodes have been projected to
-- their authoritative source identities.  Two solved copies of one source
-- forall can otherwise produce @forall s s@ at the root, even though both
-- copies denote the same lexical binder.  The identity projection is the
-- proof that permits the quotient; final term shape is deliberately not
-- consulted.  Conflicting bounds fail while preparation still has enough
-- context to report the invalid construction.
quotientPreparedBinderIdentities
    :: String
    -> ElabScheme
    -> Either ElabError ElabScheme
quotientPreparedBinderIdentities role scheme = do
    retainedBinders <- foldM retainBinder [] (schemeBinderRefs scheme)
    pure (mkElabSchemeWithRefs retainedBinders (schemeBody scheme))
  where
    retainBinder retained binder@(ref, mbBound) =
        case find (typeBinderRefsSameIdentity ref . fst) retained of
            Nothing -> pure (retained ++ [binder])
            Just (representativeRef, representativeBound)
                | equivalentBounds representativeBound mbBound -> pure retained
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "source-identity root binder quotient has incompatible bounds"
                            , "  role: " ++ role
                            , "  representative: " ++ show representativeRef
                            , "  representative bound: " ++ show representativeBound
                            , "  duplicate: " ++ show ref
                            , "  duplicate bound: " ++ show mbBound
                            , "  scheme: " ++ show scheme
                            ]
                        )

    equivalentBounds left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

-- | Preserve the construction placement encoded by 'PreparedRootClosure'
-- while quotienting solved copies that project to one source identity.  For a
-- local construction, its existing leading 'ETyAbsRef' spine wins over a
-- duplicate root binder: retaining the root copy would emit the same semantic
-- binder a second time during final closure.  The authority carries the exact
-- local scheme selected before source-identity projection, so quotienting can
-- verify placement and bounds instead of reconstructing them from packet
-- shape.
quotientPreparedRootClosureIdentities
    :: IntMap.IntMap TypeBinderRef
    -> PreparedRootClosure
    -> Either ElabError PreparedRootClosure
quotientPreparedRootClosureIdentities preferredSubst closure = do
    projectedClosure <-
        case closure of
            PreparedWholeRootClosure ambientRefs scheme -> do
                projectedScheme <-
                    quotientPreparedBinderIdentities
                        "closure"
                        (projectScheme scheme)
                pure
                    ( PreparedWholeRootClosure
                        (projectAmbientBinderRefs projectedScheme ambientRefs)
                        projectedScheme
                    )
            PreparedTopologyPacketRootClosure consumers inner -> do
                projectedInner <-
                    quotientPreparedRootClosureIdentities
                        preferredSubst
                        inner
                pure
                    ( PreparedTopologyPacketRootClosure
                        consumers
                        projectedInner
                    )
            PreparedInterleavedLocalRootClosure authority localRefs scheme -> do
                projectedScheme <-
                    applyPreparedRootBinderSubst
                        "interleaved local root closure"
                        preferredSubst
                        scheme
                projectedAuthorityScheme <-
                    applyPreparedRootBinderSubst
                        "interleaved local construction authority"
                        preferredSubst
                        (preparedLocalRootAuthorityScheme authority)
                let projectedLocalCandidates =
                        foldr insertDistinctRef [] (map projectRef localRefs)
                    projectedLocalRefs =
                        [ ref
                        | (ref, _) <- schemeBinderRefs projectedScheme
                        , any
                            (typeBinderRefsSameIdentity ref)
                            projectedLocalCandidates
                        ]
                    missingProjectedLocalRefs =
                        [ ref
                        | ref <- projectedLocalCandidates
                        , not
                            ( any
                                (typeBinderRefsSameIdentity ref)
                                projectedLocalRefs
                            )
                        ]
                unless
                    (null missingProjectedLocalRefs)
                    ( Left
                        ( ValidationFailed
                            [ "source projection removed an interleaved local construction binder"
                            , "  local binders: " ++ show localRefs
                            , "  projected local binders: "
                                ++ show projectedLocalRefs
                            , "  missing projected binders: "
                                ++ show missingProjectedLocalRefs
                            , "  projected scheme: " ++ show projectedScheme
                            ]
                        )
                    )
                let projectedAuthority =
                        replacePreparedLocalRootAuthorityScheme
                            projectedAuthorityScheme
                            ( projectPreparedLocalRootAuthority
                                projectedScheme
                                authority
                            )
                pure
                    ( PreparedInterleavedLocalRootClosure
                        projectedAuthority
                        projectedLocalRefs
                        projectedScheme
                    )
            PreparedLocalRootClosure authority scheme -> do
                let authorityLocalScheme =
                        preparedLocalRootAuthorityScheme authority
                    localBinderCount =
                        length (schemeBinderRefs authorityLocalScheme)
                (unprojectedLocalBinders, unprojectedLocalBody) <-
                    takeLeadingForallBinders
                        localBinderCount
                        (schemeBody scheme)
                let unprojectedLocalScheme =
                        mkElabSchemeWithRefs
                            unprojectedLocalBinders
                            unprojectedLocalBody
                    projectedClosureScheme =
                        projectLocalClosureScheme
                            scheme
                            unprojectedLocalScheme
                    projectedAuthorityClosureScheme =
                        projectLocalClosureScheme
                            scheme
                            authorityLocalScheme
                (localBinders, localBody) <-
                    takeLeadingForallBinders
                        localBinderCount
                        (schemeBody projectedClosureScheme)
                (expectedLocalBinders, expectedLocalBody) <-
                    takeLeadingForallBinders
                        localBinderCount
                        (schemeBody projectedAuthorityClosureScheme)
                let localPlacementMatches =
                        length localBinders == length expectedLocalBinders
                            && and
                                ( zipWith
                                    bindersAgree
                                    localBinders
                                    expectedLocalBinders
                                )
                            && ( alphaEqType localBody expectedLocalBody
                                    || churchAwareEqType localBody expectedLocalBody
                               )
                if not localPlacementMatches
                    then
                        Left
                            ( ValidationFailed
                                [ "prepared local root closure lost its exact binder placement"
                                , "  expected local binders: " ++ show expectedLocalBinders
                                , "  actual local binders: " ++ show localBinders
                                , "  expected local body: " ++ show expectedLocalBody
                                , "  actual local body: " ++ show localBody
                                , "  closure scheme: " ++ show projectedClosureScheme
                                ]
                            )
                    else do
                        localScheme <-
                            quotientPreparedBinderIdentities
                                "local construction closure"
                                (mkElabSchemeWithRefs localBinders localBody)
                        let retainedLocalBinders = schemeBinderRefs localScheme
                            localIdentities = map fst retainedLocalBinders
                            (duplicateRootBinders, independentRootBinders) =
                                List.partition
                                    (\(ref, _) -> any (typeBinderRefsSameIdentity ref) localIdentities)
                                    (schemeBinderRefs projectedClosureScheme)
                        mapM_
                            (validateDuplicateRootBound retainedLocalBinders)
                            duplicateRootBinders
                        rootScheme <-
                            quotientPreparedBinderIdentities
                                "root closure"
                                ( mkElabSchemeWithRefs
                                    independentRootBinders
                                    (schemeToType localScheme)
                                )
                        pure
                            ( PreparedLocalRootClosure
                                ( replacePreparedLocalRootAuthorityScheme
                                    localScheme
                                    ( projectPreparedLocalRootAuthority
                                        rootScheme
                                        authority
                                    )
                                )
                                rootScheme
                            )
    validatePreparedRootClosure
        "source-projected prepared root closure"
        projectedClosure
  where
    projectPreparedLocalRootAuthority projectedScheme authority =
        authority
            { plraAmbientBinderRefs =
                projectAmbientBinderRefs
                    projectedScheme
                    (plraAmbientBinderRefs authority)
            }

    -- Binder projection only renames occurrences governed by a projected
    -- declaration.  A free ambient graph identity can therefore remain in the
    -- rebuilt closure even when the source substitution has a route at the
    -- same graph key.  Select authority from the rebuilt scheme itself, while
    -- accepting either the exact pre-projection identity or its exact projected
    -- identity as provenance.  Display names and solved representatives never
    -- participate.
    projectAmbientBinderRefs projectedScheme ambientRefs =
        foldr insertDistinctRef []
            [ freeRef
            | freeRef <-
                freeTypeVarRefsType
                    (schemeToType projectedScheme)
            , any (authorizesFreeRef freeRef) ambientRefs
            ]

    authorizesFreeRef freeRef ambientRef =
        typeBinderRefsSameIdentity freeRef ambientRef
            || typeBinderRefsSameIdentity freeRef (projectRef ambientRef)

    projectRef ref =
        case typeBinderRefNode ref of
            Just node ->
                IntMap.findWithDefault
                    ref
                    (getNodeId node)
                    preferredSubst
            Nothing -> ref

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

    projectScheme inputScheme =
        siScheme (schemeInfoFromRefSubst inputScheme preferredSubst)

    -- 'attachBinderRefsToScheme' projects only an 'ElabScheme's explicit
    -- binder list.  Local constructor binders live as leading foralls in the
    -- outer scheme body, so project that nested scheme first, rebuild the
    -- outer scheme, and only then project the root binders through it.
    projectLocalClosureScheme outerScheme localScheme =
        projectScheme
            ( mkElabSchemeWithRefs
                (schemeBinderRefs outerScheme)
                (schemeToType (projectScheme localScheme))
            )

    bindersAgree (leftRef, leftBound) (rightRef, rightBound) =
        typeBinderRefsSameIdentity leftRef rightRef
            && equivalentBounds leftBound rightBound

    validateDuplicateRootBound localBinders (rootRef, rootBound) =
        case find (typeBinderRefsSameIdentity rootRef . fst) localBinders of
            Just (_, localBound)
                | equivalentBounds rootBound localBound -> pure ()
            Just (_, localBound) ->
                Left
                    ( ValidationFailed
                        [ "root and local construction copies of one source binder have incompatible bounds"
                        , "  root binder: " ++ show rootRef
                        , "  root bound: " ++ show rootBound
                        , "  local bound: " ++ show localBound
                        ]
                    )
            Nothing ->
                Left
                    ( ValidationFailed
                        [ "local root closure quotient lost its identity representative"
                        , "  root binder: " ++ show rootRef
                        , "  local binders: " ++ show localBinders
                        ]
                    )

    equivalentBounds left right =
        let leftTy = maybe TBottom tyToElab left
            rightTy = maybe TBottom tyToElab right
        in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

takeLeadingForallBinders
    :: Int
    -> ElabType
    -> Either ElabError ([(TypeBinderRef, Maybe BoundType)], ElabType)
takeLeadingForallBinders remaining ty
    | remaining == 0 = pure ([], ty)
    | otherwise =
        case ty of
            TForallRef ref mbBound body -> do
                (binders, resultBody) <-
                    takeLeadingForallBinders (remaining - 1) body
                pure ((ref, mbBound) : binders, resultBody)
            _ ->
                Left
                    ( ValidationFailed
                        [ "prepared local root closure has a truncated constructor binder spine"
                        , "  remaining binders: " ++ show remaining
                        , "  body: " ++ show ty
                        ]
                    )

preparedSchemeBodyTarget :: PreparedGeneralizationArtifact -> NodeId -> NodeId
preparedSchemeBodyTarget artifact target =
    case pgaResultTypeView artifact of
        Right view -> View.rtvSchemeBodyTarget view target
        Left _ -> schemeBodyTarget (pgaPresolutionView artifact) target

computePreparedResultType
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError ElabType
computePreparedResultType artifact authoritativeAnnCanon authoritativeAnnPre =
    computePreparedResultTypeWithReadyView
        (pgaResultTypeView artifact)
        artifact
        authoritativeAnnCanon
        authoritativeAnnPre

computePreparedResultTypeWithRootGeneralization
    :: PreparedGeneralizationArtifact
    -> PreparedRootGeneralization
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError ElabType
computePreparedResultTypeWithRootGeneralization artifact rootGen authoritativeAnnCanon authoritativeAnnPre =
    let view =
            fmap
                ( View.rtvWithKnownGeneralization
                    (prgScopeRoot rootGen)
                    (prgTarget rootGen)
                    (prgScheme rootGen, prgSubst rootGen)
                )
                (pgaResultTypeView artifact)
    in computePreparedResultTypeWithReadyView view artifact authoritativeAnnCanon authoritativeAnnPre

computePreparedResultTypeWithReadyView
    :: Either ElabError (ResultTypeView 'Presolved)
    -> PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError ElabType
computePreparedResultTypeWithReadyView resultTypeView artifact authoritativeAnnCanon authoritativeAnnPre =
    case resultTypeView of
        Left err -> Left err
        Right view ->
            computeWithReadyView view
  where
    resultTypeInputs = pgaResultTypeInputs artifact

    computeWithReadyView view =
        case (authoritativeAnnCanon, authoritativeAnnPre) of
            (AAnn inner annNodeId eid, AAnn innerPre _ _) ->
                computeResultTypeFromAnnWithView resultTypeInputs view inner innerPre annNodeId eid
            (AExactAnn inner _ annNodeId eid, AExactAnn innerPre _ _ _) ->
                computeResultTypeFromAnnWithView resultTypeInputs view inner innerPre annNodeId eid
            (AUnfold inner annNodeId eid, _) ->
                let innerPre =
                        case authoritativeAnnPre of
                            AUnfold ip _ _ -> ip
                            AAnn ip _ _ -> ip
                            AExactAnn ip _ _ _ -> ip
                            other -> other
                 in computeResultTypeFromAnnWithView resultTypeInputs view inner innerPre annNodeId eid
            _ ->
                computeResultTypeFallbackWithView resultTypeInputs view authoritativeAnnCanon authoritativeAnnPre

stripWitnesslessAuthoritativeAnnWith
    :: IntMap.IntMap edgeWitness
    -> AnnExpr
    -> AnnExpr
    -> (AnnExpr, AnnExpr)
stripWitnesslessAuthoritativeAnnWith edgeWitnesses annCanon annPre =
    case annCanon of
        AAnn innerCanon _ eid
            | IntMap.notMember (getEdgeId eid) edgeWitnesses ->
                let innerPre =
                        case annPre of
                            AAnn inner _ _ -> inner
                            AExactAnn inner _ _ _ -> inner
                            AUnfold inner _ _ -> inner
                            other -> other
                 in stripWitnesslessAuthoritativeAnnWith edgeWitnesses innerCanon innerPre
        AExactAnn innerCanon _ _ eid
            | IntMap.notMember (getEdgeId eid) edgeWitnesses ->
                let innerPre =
                        case annPre of
                            AExactAnn inner _ _ _ -> inner
                            AAnn inner _ _ -> inner
                            AUnfold inner _ _ -> inner
                            other -> other
                 in stripWitnesslessAuthoritativeAnnWith edgeWitnesses innerCanon innerPre
        AUnfold innerCanon _ eid
            | IntMap.notMember (getEdgeId eid) edgeWitnesses ->
                let innerPre =
                        case annPre of
                            AAnn inner _ _ -> inner
                            AExactAnn inner _ _ _ -> inner
                            AUnfold inner _ _ -> inner
                            other -> other
                 in stripWitnesslessAuthoritativeAnnWith edgeWitnesses innerCanon innerPre
        _ -> (annCanon, annPre)

data TraceCopyArtifacts = TraceCopyArtifacts
    { tcaInstCopyNodes :: IntSet.IntSet
    , tcaInstCopyMapFull :: IntMap.IntMap NodeId
    , tcaExpansionConstructionPlacements :: ExpansionConstructionPlacements
    }

prepareTraceCopyArtifacts
    :: Constraint p
    -> PresolutionView q
    -> IntMap.IntMap NodeId
    -> Canonicalizer
    -> EdgeArtifacts
    -> Either SolveError TraceCopyArtifacts
prepareTraceCopyArtifacts baseConstraint presolutionView redirects canonNode edgeArtifacts =
    let edgeTraces = eaEdgeTraces edgeArtifacts
        edgeConstructions = eaEdgeExpansionConstructions edgeArtifacts
        adoptNode = canonicalizeNode canonNode
        baseNodes = cNodes baseConstraint
        edgeTracesForCopy =
            IntMap.filter
                ( \tr ->
                    case lookupNodeIn baseNodes (etRoot tr) of
                        Just _ -> True
                        Nothing -> False
                )
                edgeTraces
        instCopyNodes =
            instantiationCopyNodes presolutionView redirects edgeTracesForCopy
        instCopyMapFull =
            let baseNamedKeysAll = collectBaseNamedKeys baseConstraint
                traceMaps =
                    map
                        (buildTraceCopyMap baseConstraint baseNamedKeysAll adoptNode)
                        (IntMap.elems edgeTracesForCopy)
             in foldl' IntMap.union IntMap.empty traceMaps
    in do
        expansionConstructionPlacements <-
            prepareElaborationExpansionConstructionPlacements
                baseConstraint
                adoptNode
                (\parent node ->
                    case pvLookupNode presolutionView node of
                        Just TyVar{} ->
                            Binding.isUpper
                                (pvCanonicalConstraint presolutionView)
                                parent
                                (typeRef node)
                        _ -> False
                )
                instCopyMapFull
                ( IntMap.restrictKeys
                    edgeConstructions
                    (IntSet.fromAscList (IntMap.keys edgeTracesForCopy))
                )
        pure
            TraceCopyArtifacts
                { tcaInstCopyNodes = instCopyNodes
                , tcaInstCopyMapFull = instCopyMapFull
                , tcaExpansionConstructionPlacements = expansionConstructionPlacements
                }

data RawExpansionPlacement = RawExpansionPlacement
    { repNode :: !NodeId
    , repParent :: !NodeRef
    , repFlag :: !BindFlag
    , repIsArgument :: !Bool
    , repIsSemanticMeta :: !Bool
    , repIsSupport :: !Bool
    }

data ProjectedExpansionPlacement = ProjectedExpansionPlacement
    { pepNode :: !NodeId
    , pepParent :: !NodeRef
    , pepFlag :: !BindFlag
    , pepIsConstructionOwned :: !Bool
    , pepIsArgument :: !Bool
    , pepIsSemanticMeta :: !Bool
    }

-- | Build the elaboration view of the exact edits emitted by atomic chi_e
-- construction after solving has chosen a quotient.
--
-- This is deliberately not the live Rebind tree.  A representative with an
-- explicit source-copy projection delegates its argument occurrence back to
-- base ownership, while copied semantic-meta occurrences and the parent-chain
-- supports they require retain the exact construction path needed by
-- Gamma/alias reconstruction.  When several retained occurrences share a
-- representative, their one elaboration placement is their quotient LCA.
-- Keeping the role filter before that LCA prevents a base-owned argument
-- occurrence from flattening its semantic-meta occurrence out of the copied
-- result scheme.
prepareElaborationExpansionConstructionPlacements
    :: Constraint p
    -> (NodeId -> NodeId)
    -> (NodeRef -> NodeId -> Bool)
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap RawExpansionConstruction
    -> Either SolveError ExpansionConstructionPlacements
prepareElaborationExpansionConstructionPlacements baseConstraint adoptNode isRetainedPlacement instCopyMap constructions = do
    rawClaims <-
        concat
            <$> traverse
                claimsForConstruction
                (IntMap.toAscList constructions)
    rawPlacements <- foldM insertRawPlacement IntMap.empty rawClaims
    constructionParents <-
        foldM
            insertConstructionParent
            (cBindParents baseConstraint)
            (IntMap.elems rawPlacements)
    projectedMaybes <-
        traverse
            (projectClass constructionParents)
            (IntMap.toAscList (groupByRepresentative rawPlacements))
    let projected0 = [placement | Just placement <- projectedMaybes]
        projectedByNode =
            IntMap.fromList
                [ (getNodeId (pepNode placement), placement)
                | placement <- projected0
                , pepIsConstructionOwned placement
                ]
        candidateKeys =
            IntSet.fromList
                [ key
                | (key, placement) <- IntMap.toAscList projectedByNode
                , pepIsArgument placement || pepIsSemanticMeta placement
                ]
        requiredKeys = closeProjectedParents projectedByNode candidateKeys
        retained =
            IntMap.elems
                (IntMap.restrictKeys projectedByNode requiredKeys)
        argumentClaims =
            [ (pepNode placement, pepParent placement, pepFlag placement)
            | placement <- retained
            , pepIsArgument placement
            ]
        semanticMetaClaims =
            [ (pepNode placement, pepParent placement, pepFlag placement)
            | placement <- retained
            , pepIsSemanticMeta placement
            ]
        supportClaims =
            [ (pepNode placement, pepParent placement, pepFlag placement)
            | placement <- retained
            , not (pepIsArgument placement || pepIsSemanticMeta placement)
            ]
    case
        expansionConstructionPlacementsFromProjectedLists
            argumentClaims
            semanticMetaClaims
            supportClaims
      of
        Right placements -> Right placements
        Left conflict -> placementConflictError conflict
  where
    baseNodes = cNodes baseConstraint
    baseRepresentativeKeys =
        IntSet.fromList
            [ getNodeId (adoptNode baseNode)
            | (baseNode, _node) <- toListNode baseNodes
            ]

    claimsForConstruction (edgeKey, construction) =
        let parents = rawExpansionConstructionParents construction
            supportKeys =
                IntSet.fromList
                    [ getNodeId parent
                    | (_childKey, (TypeRef parent, _flag)) <- IntMap.toAscList parents
                    , IntMap.member (nodeRefKey (typeRef parent)) parents
                    ]
        in
        traverse
            (claimFromParent edgeKey construction supportKeys)
            (IntMap.toAscList parents)

    claimFromParent edgeKey construction supportKeys (childKey, (parent, flag)) =
        case nodeRefFromKey childKey of
            TypeRef child ->
                Right
                    RawExpansionPlacement
                        { repNode = child
                        , repParent = parent
                        , repFlag = flag
                        , repIsArgument =
                            IntSet.member
                                (getNodeId child)
                                (rawExpansionConstructionArgumentKeys construction)
                        , repIsSemanticMeta =
                            IntSet.member
                                (getNodeId child)
                                (rawExpansionConstructionSemanticMetaKeys construction)
                        , repIsSupport =
                            IntSet.member
                                (getNodeId child)
                                supportKeys
                        }
            child ->
                Left
                    ( Solve.ValidationFailed
                        [ "invalid raw expansion construction placement on edge "
                            ++ show (EdgeId edgeKey)
                        , "  expected a type child, got: "
                            ++ show (child, parent, flag)
                        ]
                    )

    insertRawPlacement placements placement =
        let key = getNodeId (repNode placement)
        in case IntMap.lookup key placements of
            Nothing -> Right (IntMap.insert key placement placements)
            Just existing
                | repParent existing == repParent placement
                    && repFlag existing == repFlag placement ->
                    Right
                        ( IntMap.insert
                            key
                            existing
                                { repIsArgument =
                                    repIsArgument existing
                                        || repIsArgument placement
                                , repIsSemanticMeta =
                                    repIsSemanticMeta existing
                                        || repIsSemanticMeta placement
                                , repIsSupport =
                                    repIsSupport existing
                                        || repIsSupport placement
                                }
                            placements
                        )
                | otherwise ->
                    Left
                        ( Solve.ValidationFailed
                            [ "conflicting raw creation-time placements for expansion node "
                                ++ show (repNode placement)
                            , "  first parent: " ++ show (repParent existing)
                            , "  second parent: " ++ show (repParent placement)
                            , "  first flag: " ++ show (repFlag existing)
                            , "  second flag: " ++ show (repFlag placement)
                            ]
                        )

    insertConstructionParent parents placement =
        let child = typeRef (repNode placement)
            key = nodeRefKey child
            parent = repParent placement
            flag = repFlag placement
        in case IntMap.lookup key parents of
            Nothing -> Right (IntMap.insert key (parent, flag) parents)
            Just (existingParent, existingFlag)
                | existingParent == parent && existingFlag == flag ->
                    Right parents
                | otherwise ->
                    Left
                        ( Solve.ValidationFailed
                            [ "raw expansion construction overlaps an incompatible base placement for "
                                ++ show child
                            , "  base: " ++ show (existingParent, existingFlag)
                            , "  construction: " ++ show (parent, flag)
                            ]
                        )

    groupByRepresentative placements =
        IntMap.foldl'
            ( \groups placement ->
                IntMap.insertWith
                    (++)
                    (getNodeId (adoptNode (repNode placement)))
                    [placement]
                    groups
            )
            IntMap.empty
            placements

    projectClass constructionParents (representativeKey, origins) =
        let solveCreated =
                IntSet.notMember representativeKey baseRepresentativeKeys
            sourceProjected = IntMap.member representativeKey instCopyMap
            retainedOrigins =
                if sourceProjected
                    then
                        filter
                            (\origin ->
                                repIsSemanticMeta origin
                                    || repIsSupport origin
                                    || not (repIsArgument origin)
                            )
                            origins
                    else origins
        in if not solveCreated
            then Right Nothing
            else
                case retainedOrigins of
                    [] -> Right Nothing
                    _ -> do
                        rawParent <-
                            lowestCommonConstructionParent
                                constructionParents
                                (map repParent retainedOrigins)
                        parentAboveRepresentative <-
                            firstParentAboveRepresentative
                                constructionParents
                                (NodeId representativeKey)
                                rawParent
                        let representative = NodeId representativeKey
                            parent = adoptRef parentAboveRepresentative
                            flag = maximum (map repFlag retainedOrigins)
                            retained =
                                isRetainedPlacement parent representative
                            argument =
                                retained
                                    && flag == BindFlex
                                    && not sourceProjected
                                    && any repIsArgument retainedOrigins
                            semanticMeta =
                                retained
                                    && flag == BindFlex
                                    && any repIsSemanticMeta retainedOrigins
                        pure
                            ( Just
                                ProjectedExpansionPlacement
                                    { pepNode = representative
                                    , pepParent = parent
                                    , pepFlag = flag
                                    , pepIsConstructionOwned = True
                                    , pepIsArgument = argument
                                    , pepIsSemanticMeta = semanticMeta
                                    }
                            )

    adoptRef ref =
        case ref of
            TypeRef node -> typeRef (adoptNode node)
            GenRef owner -> GenRef owner

    lowestCommonConstructionParent _ [] =
        Left
            (Solve.ValidationFailed ["empty expansion construction quotient class"])
    lowestCommonConstructionParent parents (firstParent : rest) =
        foldM (lowestCommonParent parents) firstParent rest

    lowestCommonParent parents firstParent secondParent = do
        firstPath <- constructionPath parents firstParent
        secondPath <- constructionPath parents secondParent
        let firstKeys =
                IntSet.fromList
                    (map (nodeRefKey . adoptRef) firstPath)
        case
            find
                (\ref -> IntSet.member (nodeRefKey (adoptRef ref)) firstKeys)
                secondPath
          of
            Just common -> Right common
            Nothing ->
                Left
                    ( Solve.ValidationFailed
                        [ "expansion construction parents have no quotient LCA"
                        , "  first: " ++ show firstParent
                        , "  second: " ++ show secondParent
                        ]
                    )

    firstParentAboveRepresentative parents representative parent = do
        path <- constructionPath parents parent
        case
            find
                ( \candidate ->
                    nodeRefKey (adoptRef candidate)
                        /= nodeRefKey (typeRef representative)
                )
                path
          of
            Just candidate -> Right candidate
            Nothing ->
                Left
                    ( Solve.ValidationFailed
                        [ "expansion construction parent collapses entirely into its child"
                        , "  representative: " ++ show representative
                        , "  parent: " ++ show parent
                        ]
                    )

    constructionPath parents ref =
        case bindingPathToRootLocal parents ref of
            Right path -> Right path
            Left err ->
                Left
                    ( Solve.ValidationFailed
                        [ "invalid raw expansion construction binding path"
                        , "  start: " ++ show ref
                        , "  error: " ++ show err
                        ]
                    )

    closeProjectedParents projectedByNode = go
      where
        go required =
            let parents =
                    IntSet.fromList
                        [ getNodeId parent
                        | key <- IntSet.toAscList required
                        , Just placement <- [IntMap.lookup key projectedByNode]
                        , TypeRef parent <- [pepParent placement]
                        , IntMap.member (getNodeId parent) projectedByNode
                        ]
                required' = IntSet.union required parents
            in if required' == required then required else go required'

    placementConflictError conflict =
        Left
            ( Solve.ValidationFailed
                [ "conflicting projected creation-time placements for expansion node "
                    ++ show (ecpcNode conflict)
                , "  first: "
                    ++ show (ecpcFirstParent conflict, ecpcFirstFlag conflict)
                , "  second: "
                    ++ show (ecpcSecondParent conflict, ecpcSecondFlag conflict)
                ]
            )
