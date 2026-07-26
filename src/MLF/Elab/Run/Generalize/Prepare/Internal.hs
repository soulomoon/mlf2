{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Generalize.Prepare.Internal (
    PreparedGeneralizationArtifact(..),
    PreparedRootGeneralization(..),
    PreparedRootClosure(..),
    preparedRootClosureScheme,
    preparedRootClosureAmbientBinderRefs,
    prepareRootClosureScheme,
    prepareRootClosureSchemeWithAmbient,
    PreparedRootConstructionScope,
    preparedRootConstructionScopeBinders,
    preparedRootConstructionScopeAliases,
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
    applicationCertificateTransfersRootRequirementOwnership,
    applicationCertificateDischargesRootClosure,
    rootRequirementOwnershipAllowsLocalGammaClosure,
    validateLocalApplicationCertificates,
    unclaimedEdgesOutsideLocalGammaClosures,
    placeNestedRootRequirements,
    preparedAnnotated,
    canonicalizePreparedAnn,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    preparedIdentityGenerator,
    applyPreparedTermSourceBinderAliases,
    preparedCompilerExactSourceResultBinderRoutes,
    insertPreparedTermSourceBinderAlias,
    completePreparedCompilerExactSubtermResults,
    preparedCompilerExactExpectedType,
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
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
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
    , completeCompilerExactSubtermResultsWithBounds
    , mkEnv
    )
import MLF.Elab.Elaborate.Algebra.ConstructionGamma
    ( completeUnboundedForallSpecializesTo
    , projectCertifiedBodyConsumerBoundsIfPresent
    , projectCertifiedBodyConsumerRootBounds
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
    , RootRaiseMergeAuthority(..)
    , SubtermPacketPlacement(..)
    , SubtermGeneralizations
    , SubtermResultOwnership
    , mergeSubtermGeneralizations
    , pairSubtermGeneralizationRoots
    , placeSubtermGeneralizationBindersWithRoutes
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
    , localGammaFrame
    , localGammaOwnerScope
    , selectLocalGammaEdgeOwnership
    , mkIdentityTopologyConsumerAuthority
    , generalizationRequirementsForEnclosingRootEdges
    , subtermGeneralizationsOwnedBy
    , scaConsumerIdentity
    , scaEdgeId
    , subtermConsumerAuthorityEnclosingOwner
    , subtermConsumerAuthorityIsTopology
    , subtermGeneralizationConsumerAuthority
    , subtermGeneralizationConsumerIdentity
    , subtermGeneralizationConsumerConstructionSchemeInfo
    , subtermGeneralizationCompilerExactBoundary
    , subtermGeneralizationCompilerExactExistingRef
    , subtermGeneralizationCompilerExactResultRef
    , subtermGeneralizationCompilerExactResultStage
    , subtermGeneralizationInheritedGammaRoutes
    , subtermGeneralizationGammaAuthority
    , subtermGeneralizationSchemeInfo
    , subtermGeneralizationOwnsGammaEdge
    , subtermGeneralizationOwnsGammaForEdge
    , subtermGeneralizationLocalConsumerClosure
    , subtermResultOwnershipFor
    , subtermResultOwnershipHasTransparentPath
    , subtermResultOwnershipLambdaNode
    , subtermResultOwnershipPacket
    , withCompilerExactSourceSubtermResult
    , withCompilerExactPacketSubtermResult
    , withCompilerExactEnclosingSubtermResult
    , withCompilerExactBinderRenames
    , withConstructionBinderRenames
    , withInheritedGammaRoutes
    )
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.ReadModel (ElabReadModel, buildElabReadModel)
import MLF.Elab.SourceBinder
    ( orderSourceProjectedSchemeBinders
    , resolveConstructionSourceBindersInSchemeInfoExcept
    , resolveSourceBinderAliasesInType
    , sourceBinderAliasSubstitution
    , sourceBinderConstructionRenames
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
    ( resolvedSourceApplicationArgumentEndpoint
    )
import MLF.Elab.Run.Generalize.Types
    ( DirectApplicationAmbientGammaClaim(..)
    , DirectApplicationGammaClaim(..)
    , ExpansionConstructionPlacementConflict(..)
    , ExpansionConstructionPlacements
    , LocalGammaConstruction(..)
    , LocalGammaConstructionCertificate(..)
    , expansionConstructionPlacementsFromProjectedLists
    , localGammaConstructionBinders
    , localGammaConsumedBinders
    , localGammaEmittedBinders
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
    )
import MLF.Elab.Types
    ( ElabError(..)
    , BoundType
    , ElabScheme
    , ElabType
    , XmlfTerm
    , SchemeClosureAuthority
    , SchemeInfo(..)
    , Ty(TArrow, TBaseWithIdentity, TBottom, TConWithIdentity, TForallRef, TMuRef, TVarAppRef, TVarRef)
    , TypeBinderRef
    , idDetailsIdentityKey
    , bindingToElab
    , mkElabSchemeWithRefs
    , ambientSchemeClosureAuthority
    , mapBoundType
    , schemeBinderRefs
    , schemeBody
    , schemeFromType
    , schemeInfoFromRefSubst
    , typeBinderRefFromIdentity
    , typeBinderIdentityFromNode
    , typeBinderRefIdentity
    , typeBinderRefName
    , typeBinderRefNode
    , typeBinderRefsSameIdentity
    , tyToElab
    , validateSchemeClosure
    )
import MLF.Frontend.ConstraintGen
    ( AnnExpr(..)
    , instantiationSiteEdgeId
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
    , pgaEdgeArtifacts :: EdgeArtifacts
    , pgaExactProducerTypes :: Either ElabError (IntMap.IntMap ElabType)
    -- Source annotations are occurrence-owned.  Solving may identify their
    -- graph result nodes, but it cannot identify the source coercion edges
    -- that carry their expected types.
    , pgaAnnotationExpectedTypesByEdge :: IntMap.IntMap ElabType
    , pgaAnnotationSourceNodeKeys :: IntSet.IntSet
    , pgaScopeOverrides :: ConstructionScopes
    , pgaSubtermGeneralizations :: Either ElabError SubtermGeneralizations
    , pgaIdentityGenerator :: Either ElabError IdentityGenerator
    , pgaAnnotated :: AnnExpr
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
    }

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
    | PreparedLocalRootClosure
        !PreparedLocalRootAuthority
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
        PreparedLocalRootClosure _ scheme -> scheme

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
        PreparedLocalRootClosure authority _ ->
            plraAmbientBinderRefs authority

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

preparedRootConstructionScopeLocalGammaClosures
    :: PreparedRootConstructionScope
    -> IntMap.IntMap LocalGammaClosure
preparedRootConstructionScopeLocalGammaClosures = prcsLocallyClosedGammas

emptyPreparedRootConstructionScope :: PreparedRootConstructionScope
emptyPreparedRootConstructionScope =
    PreparedRootConstructionScope
        { prcsBinders = []
        , prcsAliases = IntMap.empty
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

packetExpectedType :: PacketExpectedType -> ElabType
packetExpectedType expected =
    case expected of
        CompilerExactExpectedType _ ty _ -> ty
        SourceExpectedType ty _ -> ty

packetOperatedExpectedType :: PacketExpectedType -> ElabType
packetOperatedExpectedType expected =
    case expected of
        CompilerExactExpectedType _ ty enclosingBinders ->
            compilerExactOperatedType enclosingBinders ty
        SourceExpectedType ty _ -> ty

packetExpectedAmbientBinders
    :: PacketExpectedType
    -> [(TypeBinderRef, Maybe BoundType)]
packetExpectedAmbientBinders expected =
    case expected of
        CompilerExactExpectedType _ _ enclosingBinders -> enclosingBinders
        SourceExpectedType _ enclosingBinders -> enclosingBinders

sourcePacketExpectedType :: ElabType -> PacketExpectedType
sourcePacketExpectedType ty =
    SourceExpectedType
        ty
        [ (ref, Nothing)
        | ref <- distinctTypeBinderRefs (freeTypeVarRefsType ty)
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
    -> [(EdgeId, Maybe ElabType)]
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
-- type and declaration bound in the completed application's ambient-use set.
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
            ( \freeRef ->
                any
                    (typeBinderRefsSameIdentity freeRef)
                    (lgccUsedAmbientBinderRefs certificate)
            )
            ( freeTypeVarRefsType (daagcOperatedType claim)
                ++ freeTypeVarRefsType (daagcAmbientBound claim)
            )
  where
    ambientClaimBoundSatisfies ambientRef ambientBound operatedType =
        case operatedType of
            TVarRef operatedRef
                | typeBinderRefsSameIdentity ambientRef operatedRef -> True
            _ -> typesEquivalent ambientBound operatedType

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
        && constructionEndpointProvides
            (dagcOperatedType claim)
            (rgbOperatedType requirement)
        && directApplicationGammaClaimConstructionValid
            certificate
            claim
        && routesAllReachRef
            certificate
            (requirementRouteNodes requirement)
            (dagcBinderRef claim)
        && boundProvidesType
            (dagcConstructedBound claim)
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
-- source claim's operated root.  The transfer is nevertheless exact: one
-- validated non-bottom direct claim must name the same exterior and the same
-- complete result-node set, every one of those nodes must route to the
-- claim's binder, and the application term node must be that result
-- occurrence.  No representative or type-shape relation participates.
applicationCertificateCompletesProvisionalResultRequirement
    :: LocalGammaConstructionCertificate
    -> RequiredGammaBinder
    -> Bool
applicationCertificateCompletesProvisionalResultRequirement certificate requirement =
    lgoConstructor owner == LocalApplicationGamma
        && rgbOperatedType requirement == TBottom
        && not
            ( requirementUsesDirectApplicationSource
                certificate
                requirement
            )
        && case matchingClaims of
            [claim] ->
                directApplicationGammaClaimConstructionValid
                    certificate
                    claim
                    && dagcOperatedType claim /= TBottom
                    && routesAllReachRef
                        certificate
                        ( rgbExteriorNode requirement
                            : NonEmpty.toList
                                (rgbResultRoots requirement)
                        )
                        (dagcBinderRef claim)
            _ -> False
  where
    owner = lgccOwner certificate
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
                    && boundMatchesType
                        constructedBound
                        (dagcOperatedType claim)
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
        validateLocalApplicationCertificates
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
            explicitEdges
            expectedType
            ann
    let resultLocalEdges =
            [ EdgeId edgeKey
            | (edgeKey, closure) <-
                IntMap.toList (rbeLocallyClosedGammas boundaryEdges)
            , localGammaOwnerOnResultPath (lgcOwner closure) ann
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
            explicitEdges
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
                  grRequiredGammaBinders =
                    filter
                        (not . applicationOwnsPlanningRequirement)
                        (grRequiredGammaBinders requirements0)
                , grLocallyClosedGammaNodes =
                    IntSet.union
                        (grLocallyClosedGammaNodes requirements0)
                        locallyClosedApplicationNodes
                }
    unless
        (IntSet.null conflictingApplicationNodes)
        ( Left
            ( ValidationFailed
                [ "an emitted application Gamma is also required by the root scope"
                , "  conflicting nodes: "
                    ++ show (map NodeId (IntSet.toList conflictingApplicationNodes))
                , "  application certificates: " ++ show localApplicationCertificates
                , "  root requirements: " ++ show rootRequirements
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
            graphRouted ref =
                any
                    (typeBinderRefsSameIdentity ref)
                    (IntMap.elems routes)
            sourceAuthorized ref =
                any
                    (typeBinderRefsSameIdentity ref)
                    (IntMap.elems sourceAuthorities)
            ambientRefs = lgccUsedAmbientBinderRefs certificate
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
                [ sourceRef
                | sourceRef <- IntMap.elems sourceAuthorities
                , not
                    ( any
                        (typeBinderRefsSameIdentity sourceRef)
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
                [ (NodeId nodeKey, sourceRef, IntMap.lookup nodeKey sourceBinderRefs)
                | (nodeKey, sourceRef) <- IntMap.toList sourceAuthorities
                , case IntMap.lookup nodeKey sourceBinderRefs of
                    Just currentSourceRef ->
                        not
                            ( typeBinderRefsSameIdentity
                                sourceRef
                                currentSourceRef
                            )
                    Nothing -> True
                ]
            duplicateAmbientRefs =
                [ ref
                | (index, ref) <- zip [0 :: Int ..] ambientRefs
                , any
                    (typeBinderRefsSameIdentity ref)
                    (drop (index + 1) ambientRefs)
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
                        && boundMatchesType
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

-- | Attach the exact nested Figure 15.3.5 owner while construction evidence is
-- still available.  A term-local closure is the strongest authority.  If no
-- term constructor owns the edge, the frozen binding tree supplies the
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
                    pure
                        requirement
                            { rgbPlacement =
                                RequiredGammaAtNestedScope
                                    (localGammaOwnerScope (lgcOwner closure))
                            }
      where
        requirementEdges = NonEmpty.toList (rgbEdgeIds requirement)
        presentClosures =
            [ closure
            | edgeId <- requirementEdges
            , Just closure <- [closureForEdge edgeId]
            ]

    closureForEdge edgeId =
        IntMap.lookup (getEdgeId edgeId) locallyClosed

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
    -> [(EdgeId, Maybe ElabType)]
    -> Maybe ElabType
    -> AnnExpr
    -> Either ElabError GeneralizationRequirements
generalizationRequirementsForScopeEdges localEdges edgeSelector identityRepresentative constructionCanonical ga ownerScope presolutionView edgeArtifacts exactProducerTypes sourceBinderRefs subtermPackets explicitEdges expectedType ann = do
    (rootEdge, exactOperatedType) <- rootLambdaBodyRequirement expectedType ann
    selectedEdges <- edgeSelector ann
    ownedEdges <- filterM ownsEdge selectedEdges
    let inferredEdges =
            [ (edgeId, exactFor edgeId)
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
    generalizationRequirementsForEnclosingRootEdges
        identityRepresentative
        constructionCanonical
        ga
        presolutionView
        edgeArtifacts
        sourceBinderRefs
        subtermPackets
        (IntMap.elems edgesById)
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
    boundary <- go directApplicationOwners [] ann
    groupBoundaryClosures boundary
  where
    go directApplicationOwners localOwners expr = do
        frame <- localGammaFrame scopeForBoundary expr
        let localOwners' =
                maybe localOwners (: localOwners) (lgfOwner frame)
        collect
            directApplicationOwners
            localOwners'
            (map fst (lgfDirectEdgeSources frame))
            (lgfChildren frame)

    collect directApplicationOwners localOwners edges children = do
        edgeClaims <-
            traverse
                (\edgeId -> do
                    mbClosure <-
                        claimedClosure
                            directApplicationOwners
                            localOwners
                            edgeId
                    pure (edgeId, mbClosure)
                )
                edges
        descendants <-
            traverse
                (go directApplicationOwners localOwners)
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

    claimedClosure directApplicationOwners localOwners edgeId = do
        authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
        case authority of
            Just rootAuthority ->
                case closureOwner rootAuthority of
                    Nothing -> pure Nothing
                    Just (owner, directApplicationEdges) -> do
                        ownerScheme <- packetOwnerScheme owner
                        inheritedRoutes <-
                            packetInheritedGammaRoutes rootAuthority
                        pure
                            ( Just
                                ( LocalGammaClosure
                                    { lgcEdgeIds = NonEmpty.singleton edgeId
                                    , lgcDirectApplicationEdgeIds =
                                        directApplicationEdges
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

        closureOwner rootAuthority =
            case
                selectLocalGammaEdgeOwnership
                    directApplicationOwners
                    edgeId
                    localOwners
                    ownsExterior
            of
                Just (DirectApplicationEdgeOwnership owner) ->
                    -- Figure 15.3.5 sends each of an application's two
                    -- direct instantiation computations through its
                    -- edge-local construction lane.  The syntax-owned edge
                    -- is stable occurrence proof even when the frozen
                    -- exterior remains bound on an enclosing gen.
                    Just (owner, [edgeId])
                Just (FlexibleExteriorEdgeOwnership owner) ->
                    Just (owner, [])
                Nothing -> Nothing
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
            (before, existing : after)
                | lgcOwnerPendingScheme existing
                    == lgcOwnerPendingScheme closure ->
                    pure
                        ( before
                            ++ [ existing
                                    { lgcEdgeIds =
                                        foldl
                                            appendEdgeId
                                            (lgcEdgeIds existing)
                                            (NonEmpty.toList (lgcEdgeIds closure))
                                    , lgcDirectApplicationEdgeIds =
                                        foldl
                                            appendDirectApplicationEdgeId
                                            (lgcDirectApplicationEdgeIds existing)
                                            (lgcDirectApplicationEdgeIds closure)
                                    }
                               ]
                            ++ after
                        )
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "one local Gamma obligation has conflicting pending owner schemes"
                            , "  owner: " ++ show (lgcOwner closure)
                            , "  exterior: " ++ show (lgcExteriorNode closure)
                            , "  first scheme: " ++ show (lgcOwnerPendingScheme existing)
                            , "  second scheme: " ++ show (lgcOwnerPendingScheme closure)
                            ]
                        )

    sameClosureGroup left right =
        lgcExteriorNode left == lgcExteriorNode right
            && lgcConsumerIdentity left == lgcConsumerIdentity right
            && lgcOwner left == lgcOwner right

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
                (enclosingBinders ++ [(ref, mbBound)])
                body
        TArrow _ codomain -> Just (codomain, enclosingBinders)
        _ -> Nothing

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
                    ( sourceBinderConstructionRenames
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
            do
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
        _ -> pure (inferred, [], [])

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
            (declarationPairs sourceType inferredType)
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

    declarationPairs source target =
        case (source, target) of
            (TArrow sourceDomain sourceCodomain, TArrow targetDomain targetCodomain) ->
                declarationPairs sourceDomain targetDomain
                    ++ declarationPairs sourceCodomain targetCodomain
            (TConWithIdentity _ _ sourceArgs, TConWithIdentity _ _ targetArgs) ->
                concat
                    ( zipWith
                        declarationPairs
                        (NonEmpty.toList sourceArgs)
                        (NonEmpty.toList targetArgs)
                    )
            (TVarAppRef _ sourceArgs, TVarAppRef _ targetArgs) ->
                concat
                    ( zipWith
                        declarationPairs
                        (NonEmpty.toList sourceArgs)
                        (NonEmpty.toList targetArgs)
                    )
            (TForallRef sourceRef sourceBound sourceBody, TForallRef inferredRef inferredBound inferredBody) ->
                (sourceRef, inferredRef)
                    : boundPairs sourceBound inferredBound
                        ++ declarationPairs sourceBody inferredBody
            (TMuRef sourceRef sourceBody, TMuRef inferredRef inferredBody) ->
                (sourceRef, inferredRef)
                    : declarationPairs sourceBody inferredBody
            _ -> []

    boundPairs (Just sourceBound) (Just inferredBound) =
        declarationPairs (tyToElab sourceBound) (tyToElab inferredBound)
    boundPairs _ _ = []

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
                }

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
    let scopeOverrides = mconcat scopeOverrideParts
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
                        scopeOverrides
                        anns
                        annCanons
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
            , pgaEdgeArtifacts = edgeArtifacts
            , pgaExactProducerTypes = exactProducerTypes
            , pgaAnnotationExpectedTypesByEdge = annotationExpectedTypesByEdge
            , pgaAnnotationSourceNodeKeys = annotationSourceNodeKeys
            , pgaScopeOverrides = scopeOverrides
            , pgaSubtermGeneralizations = subtermGeneralizations
            , pgaIdentityGenerator = preparedGenerator
            , pgaAnnotated = annCanon
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
            (liveProjectionCandidates ++ baseAliasCandidates ++ copyCandidates)

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
    -> ConstructionScopes
    -> [AnnExpr]
    -> [AnnExpr]
    -> Either ElabError (SubtermGeneralizations, IdentityGenerator)
prepareSubtermGeneralizations identityGenerator identityRepresentative constructionCanonical baseConstraint presolutionView rawEdgeArtifacts edgeArtifacts exactProducerTypes annExpectedTypes redirects bindParentsGa sourceBinderRefs compilerExactConstructionRefs resultTypeView scopeOverrides sources canons = do
    localGammaClosures <- preparedLocalGammaClosures
    rootPairs <- pairSubtermGeneralizationRoots sources canons
    foldM (collectRoot localGammaClosures) (Map.empty, identityGenerator) rootPairs
  where
    preparedLocalGammaClosures = do
        boundaries <-
            traverse
                ( rootBoundaryInstantiationEdges
                    packetScopeRootForBoundary
                    bindParentsGa
                    edgeArtifacts
                    []
                )
                canons
        foldM
            mergePreparedClosureMap
            IntMap.empty
            (map rbeLocallyClosedGammas boundaries)

    mergePreparedClosureMap closures incoming =
        foldM insertPreparedClosure closures (IntMap.toList incoming)

    insertPreparedClosure closures (edgeKey, closure) =
        case IntMap.lookup edgeKey closures of
            Nothing -> pure (IntMap.insert edgeKey closure closures)
            Just existing
                | existing == closure -> pure closures
                | otherwise ->
                    Left
                        ( ValidationFailed
                            [ "one prepared edge has conflicting local Gamma owners"
                            , "  edge: " ++ show (EdgeId edgeKey)
                            , "  first: " ++ show existing
                            , "  second: " ++ show closure
                            ]
                        )

    packetScopeRootForBoundary edgeId fallbackNode =
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

    collectRoot localGammaClosures (packets, generator) (source, canon) = do
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
                    collect localGammaClosures localSourceBinderRefs boundOverlays expectedBodyType generator sourceBody canonBody
                let enclosingConsumerOwner =
                        LocalGammaOwner
                            { lgoConstructor = LocalLambdaGamma
                            , lgoBoundaryEdge = sourceBodyEdge
                            , lgoTermNode = canonLambdaNode
                            , lgoScope = GenRef canonScopeRoot
                            }
                    prepareBodyPacket ownerKey mbRequiredLambdaParam mbEnclosingParam mbConsumer mbConsumerOwner mbBodyGammaAuthority packetGammaAuthority expectedPacketType sourcePacketRoot canonPacketRoot = do
                        let ownedDescendants =
                                subtermGeneralizationsOwnedBy canonPacketRoot descendants
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
                            operatedExpectedType =
                                expectedPacketType <|> exactGammaOperatedType
                            exactOperatedDeclarationIdentities =
                                case operatedExpectedType of
                                    Just expected@CompilerExactExpectedType{} ->
                                        Set.fromList
                                            ( map
                                                typeBinderRefIdentity
                                                ( typeBinderDeclarationRefs
                                                    (packetOperatedExpectedType expected)
                                                )
                                            )
                                    _ -> Set.empty
                        ( _bodyTarget
                          , bodyPacketRaw0
                          , operatedPacketRaw0
                          , inheritedGammaRoutes
                          ) <-
                            generalizeBody
                                mbRequiredLambdaParam
                                mbEnclosingParam
                                localSourceBinderRefs
                                mbBodyGammaAuthority
                                operatedExpectedType
                                boundOverlays
                                ownedDescendants
                                sourcePacketRoot
                                canonPacketRoot
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
                                    exactOperatedDeclarationIdentities
                                    identityRepresentative
                                    localSourceBinderRefs
                                    bodyPacketRaw0
                                )
                        bodyPacketConstruction <-
                            publishTopologyConsumerRoutes
                                (gaConstructionRouteNodes constructionCanonical bindParentsGa)
                                ownedDescendants
                                bodyPacketConstruction0
                        bodySchemePlaced <-
                            placeSubtermGeneralizationBindersWithRoutes
                                (siSubstRefs bodyPacketConstruction)
                                ownedDescendants
                                (siScheme bodyPacketConstruction)
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
                        operatedScheme <-
                            placeSubtermGeneralizationBindersWithRoutes
                                (siSubstRefs operatedPacketWithConsumerRoutes)
                                ownedDescendants
                                operatedSchemeOrdered
                        let bodyPacketPlaced =
                                schemeInfoFromRefSubst
                                    bodySchemePlaced
                                    (siSubstRefs bodyPacketConstruction)
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
                                    exactOperatedDeclarationIdentities
                                    identityRepresentative
                                    localSourceBinderRefs
                                    bodyPacketPlaced
                                )
                        (operatedPacket, constructionBinderRenames, exactBinderRenames) <-
                            constructPacketOperatedScheme
                                identityRepresentative
                                constructionCanonical
                                (resultTypeViewWithOverlays boundOverlays)
                                localSourceBinderRefs
                                (Just bodyPacket)
                                operatedExpectedType
                                ( schemeInfoFromRefSubst
                                    operatedScheme
                                    (siSubstRefs operatedPacketWithConsumerRoutes)
                                )
                        let constructionPacket = bodyPacket
                        let bodyScheme = siScheme constructionPacket
                        let needsPacket =
                                isJust mbConsumer
                                    || not (Map.null ownedDescendants)
                                    || not (null (schemeBinderRefs bodyScheme))
                                    || not (null constructionBinderRenames)
                                    || not (null exactBinderRenames)
                        exactResult <-
                            compilerExactPacketResult
                                expectedPacketType
                                constructionPacket
                                operatedPacket
                        if not needsPacket
                            then pure (descendants, generatorAfterDescendants)
                            else do
                                (preparedBodyPacket, generator') <-
                                    case (mbConsumer, packetGammaAuthority) of
                                        (Just consumer, Just gammaAuthority) ->
                                            if gpaEdgeId gammaAuthority == sourceBodyEdge
                                                && consumerRequiresGamma
                                                then
                                                    if gpaConsumerIdentity gammaAuthority
                                                        == preparedLambdaBodyConsumerIdentity consumer
                                                        then
                                                            prepareSubtermGeneralizationPacket
                                                                generatorAfterDescendants
                                                                (GammaPacket gammaAuthority)
                                                                constructionPacket
                                                                operatedPacket
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
                                                    do
                                                        placement <-
                                                            consumerPlacement
                                                                consumer
                                                                (Just gammaAuthority)
                                                        prepareSubtermGeneralizationPacket
                                                            generatorAfterDescendants
                                                            placement
                                                            constructionPacket
                                                            operatedPacket
                                        (Just consumer, Nothing) -> do
                                            placement <- consumerPlacement consumer Nothing
                                            prepareSubtermGeneralizationPacket
                                                generatorAfterDescendants
                                                placement
                                                constructionPacket
                                                operatedPacket
                                        (Nothing, Just gammaAuthority) ->
                                            prepareSubtermGeneralizationPacket
                                                generatorAfterDescendants
                                                (GammaPacket gammaAuthority)
                                                constructionPacket
                                                operatedPacket
                                        (Nothing, _) ->
                                            prepareSubtermGeneralizationPacket
                                                generatorAfterDescendants
                                                DirectPacket
                                                constructionPacket
                                                operatedPacket
                                preparedBodyPacketWithInheritedRoutes <-
                                    withInheritedGammaRoutes
                                        inheritedGammaRoutes
                                        preparedBodyPacket
                                let preparedBodyPacketWithConstructionRenames =
                                        withConstructionBinderRenames
                                            constructionBinderRenames
                                            preparedBodyPacketWithInheritedRoutes
                                preparedBodyPacketWithBinderRenames <-
                                    withCompilerExactBinderRenames
                                        exactBinderRenames
                                        preparedBodyPacketWithConstructionRenames
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
                                                    preparedBodyPacketWithBinderRenames
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
                                                            preparedBodyPacketWithBinderRenames
                                                    Nothing ->
                                                        withCompilerExactPacketSubtermResult
                                                            exactEdge
                                                            packetResultRef
                                                            preparedBodyPacketWithBinderRenames
                                        Nothing -> pure preparedBodyPacketWithBinderRenames
                                packets <-
                                    mergeSubtermGeneralizations
                                        (Map.singleton ownerKey preparedBodyPacket')
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
                        (nestedOwnerKey, nestedParamNode, nestedBodyEdge) <-
                            case canonNestedLambda of
                                ALam _ nestedDetails nestedParam _ _ edgeId _ ->
                                    pure
                                        ( idDetailsIdentityKey nestedDetails
                                        , nestedParam
                                        , edgeId
                                        )
                                _ ->
                                    Left
                                        (ValidationFailed
                                            [ "administrative lambda body did not retain its lambda owner"
                                            , "  body: " ++ show canonNestedLambda
                                            ])
                        let packetGammaAuthority =
                                case Map.lookup nestedOwnerKey descendants of
                                    Just packet
                                        | subtermGeneralizationOwnsGammaForEdge nestedBodyEdge packet ->
                                            subtermGeneralizationGammaAuthority packet
                                    _ -> Nothing
                        prepareBodyPacket
                            nestedOwnerKey
                            (Just nestedParamNode)
                            (Just canonParam)
                            mbConsumer
                            consumerOwner
                            Nothing
                            packetGammaAuthority
                            expectedBodyType
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
                                    Nothing
                                    Nothing
                                    (Just consumer)
                                    consumerOwner
                                    mbGammaAuthority
                                    mbGammaAuthority
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
                    localSourceBinderRefs' <-
                        mergeCompilerExactConstructionBinderRefs
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
                collect
                    localGammaClosures
                    localSourceBinderRefs
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

    -- Construct the packet's binder scope while the source-owned lambda
    -- capability is still available.  The returned scheme is locally closed
    -- exactly once: an unused nested parameter is introduced by its own
    -- Var-Abs packet, while enclosing/source binders remain ambient and can
    -- never enter this packet's quantifier spine.
    constructPacketBodyScheme
        mbRequiredLambdaParam
        ambientBinderRefs
        packet = do
            packetWithRequiredParam <-
                case mbRequiredLambdaParam of
                    Nothing -> pure packet
                    Just paramNode -> ensureRequiredLambdaParameter paramNode packet
            let subst = siSubstRefs packetWithRequiredParam
                retainedBinders =
                    [ binding
                    | binding@(ref, _) <-
                        schemeBinderRefs (siScheme packetWithRequiredParam)
                    , not
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

    ensureRequiredLambdaParameter paramNode packet =
        let binders = schemeBinderRefs (siScheme packet)
            subst = siSubstRefs packet
            body = schemeBody (siScheme packet)
            paramRef =
                fromMaybe
                    ( typeBinderRefFromIdentity
                        (typeBinderIdentityFromNode paramNode)
                        ("t" ++ show (getNodeId paramNode))
                    )
                    (refForNode subst paramNode)
         in if any (typeBinderRefsSameIdentity paramRef . fst) binders
                then pure packet
                else
                    case snd (splitForallsRefs body) of
                        TArrow TBottom _ ->
                            pure
                                ( schemeInfoFromRefSubst
                                    ( mkElabSchemeWithRefs
                                        ((paramRef, Nothing) : binders)
                                        (restoreParameterDomain paramRef body)
                                    )
                                    (IntMap.insert (getNodeId paramNode) paramRef subst)
                                )
                        TArrow _ _ -> pure packet
                        _ ->
                            Left
                                ( ValidationFailed
                                    [ "source-owned lambda packet did not construct an arrow"
                                    , "  parameter: " ++ show paramNode
                                    , "  packet body: " ++ show body
                                    , "  packet binders: " ++ show binders
                                    ]
                                )

    -- A packet can already quantify its result before the enclosing Var-Abs
    -- restores the lambda domain.  Those quantifiers are part of the packet's
    -- constructed type, so inspect and rebuild beneath that exact spine rather
    -- than mistaking the forall itself for a non-function result.
    restoreParameterDomain paramRef ty =
        case ty of
            TForallRef ref mbBound body ->
                TForallRef ref mbBound (restoreParameterDomain paramRef body)
            TArrow TBottom cod -> TArrow (TVarRef paramRef) cod
            other -> other

    generalizeBody mbRequiredLambdaParam mbEnclosingParam localSourceBinderRefs mbAuthority expectedType boundOverlays ownedDescendants sourceBody canonBody = do
        baseScopeRoot <-
            case mbAuthority of
                Nothing ->
                    bindingToElab $
                        resolveCanonicalScope
                            baseConstraint
                            presolutionView
                            redirects
                            (annNode sourceBody)
                Just authority -> pure (genRef (gpaOwnerGen authority))
        let baseTarget = schemeBodyTarget presolutionView (annNode canonBody)
            expectedElabType = packetExpectedType <$> expectedType
            ambientBinderRefs =
                packetAmbientBinderRefs
                    localSourceBinderRefs
                    expectedType
                    mbEnclosingParam
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
                        ownedDescendants
                        []
                        expectedElabType
                        sourceBody
                let requirements =
                        requirements0
                            { grAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( ambientBinderRefs
                                        ++ grAmbientBinderRefs requirements0
                                    )
                            }
                (target, schemeRaw, subst, inheritedGammaRoutes) <-
                    generalizeTarget
                        boundOverlays
                        baseScopeRoot
                        baseTarget
                        requirements
                let schemeInfoRaw = schemeInfoFromRefSubst schemeRaw subst
                schemeInfoPrepared <-
                    prepareRootRaiseMergeScheme
                        edgeArtifacts
                        sourceBody
                        requirements
                        schemeInfoRaw
                bodySchemeInfo <-
                    constructPacketBodyScheme
                        mbRequiredLambdaParam
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
                        ownedDescendants
                        []
                        expectedElabType
                        sourceBody
                let operatedRequirements =
                        operatedRequirements0
                            { grAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( ambientBinderRefs
                                        ++ grAmbientBinderRefs operatedRequirements0
                                    )
                            }
                ( _operatedTarget
                  , operatedSchemeRaw
                  , operatedSubst
                  , operatedInheritedGammaRoutes
                  ) <-
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
                let operatedSchemeInfoRaw =
                        schemeInfoFromRefSubst operatedSchemeRaw operatedSubst
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
                        ownedDescendants
                        [ ( authorityEdge
                          , Just (schemeToType (siScheme operatedSchemeInfo))
                          )
                        ]
                        Nothing
                        sourceBody
                let requirements =
                        requirements0
                            { grAmbientBinderRefs =
                                distinctTypeBinderRefs
                                    ( ambientBinderRefs
                                        ++ grAmbientBinderRefs requirements0
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
                let constructionSchemeInfo =
                        schemeInfoFromRefSubst schemeRaw subst
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
                                , "  requirements: " ++ show requirements
                                , "  cause: " ++ show cause
                                ]
                            )
            pure (target, schemeRaw, subst, inheritedGammaRoutes)

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

    refForNode subst node =
        IntMap.lookup (getNodeId node) subst
            <|> find
                (\ref -> typeBinderRefNode ref == Just node)
                (IntMap.elems subst)

preparedAnnotated :: PreparedGeneralizationArtifact -> AnnExpr
preparedAnnotated = pgaAnnotated

canonicalizePreparedAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> Either ElabError AnnExpr
canonicalizePreparedAnn artifact =
    alignAnnInstantiationSites
        (pgaEdgeArtifacts artifact)
        . redirectAndCanonicalizeAnn
            (pgaAnnNodeCanonical artifact)
            (pgaRedirects artifact)

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
    -> IntMap.IntMap TypeBinderRef
    -> XmlfTerm
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
applyPreparedTermSourceBinderAliases artifact rootSubst term =
    do
        directSourceKeys <-
            preparedDirectSourceBinderKeysForAnn
                artifact
                (pgaAnnotated artifact)
        foldM
            ( insertPreparedTermSourceBinderAlias
                directSourceKeys
                (pgaSourceBinderRefs artifact)
            )
            rootSubst
            (IntMap.toList sourceAliases)
  where
    sourceAliases =
        sourceBinderAliasSubstitution
            (pgaCanonical artifact)
            (pgaSourceBinderRefs artifact)
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
-- graph placeholder may adopt a generated source identity only at the exact
-- key of a direct source declaration.  Expanded solved/copy routes are useful
-- for lookup, but are not declaration authority and therefore cannot resolve
-- a root identity conflict.
insertPreparedTermSourceBinderAlias
    :: IntSet.IntSet
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> (Int, TypeBinderRef)
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
insertPreparedTermSourceBinderAlias directSourceKeys sourceBinderRefs subst (nodeKey, sourceRef) =
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
            | otherwise ->
                Left
                    ( ValidationFailed
                        [ "prepared root and source-binder substitutions disagree"
                        , "  graph node: " ++ show (NodeId nodeKey)
                        , "  root binder: " ++ show existing
                        , "  source binder: " ++ show sourceRef
                        , "  direct source keys: "
                            ++ show (IntSet.toList directSourceKeys)
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
        , eeEdgeArtifacts = pgaEdgeArtifacts artifact
        , eeExactProducerTypes = pgaExactProducerTypes artifact
        , eeCompilerExactConstructionRefs =
            IntMap.map ceepConstructionRefs
                <$> pgaCompilerExactEdgePlans artifact
        , eeScopeOverrides = pgaScopeOverrides artifact
        , eeAnnotationExpectedTypesByEdge = pgaAnnotationExpectedTypesByEdge artifact
        , eeExactLambdaParamSourceTypes =
            canonicalizePreparedExactLambdaParamSourceTypes artifact annSourceTypes
        , eeSourceTypeHeadIdentities = Map.empty
        , eeSourceTypeBinderIdentities = Map.empty
        , eeSourceBinderRefs = pgaSourceBinderRefs artifact
        , eeDirectSourceBinderKeys = pgaDirectSourceBinderKeys artifact
        , eeSubtermGeneralizations = pgaSubtermGeneralizations artifact
        , eeInitialTermEnv = initialTermEnv
        }

canonicalizePreparedExactLambdaParamSourceTypes
    :: PreparedGeneralizationArtifact
    -> IntMap.IntMap NormSrcType
    -> IntMap.IntMap NormSrcType
canonicalizePreparedExactLambdaParamSourceTypes artifact annSourceTypes =
    IntMap.fromList
        [ (getNodeId (pgaAnnNodeCanonical artifact nid), ty)
        | (k, ty) <-
            IntMap.toList
                ( IntMap.withoutKeys
                    annSourceTypes
                    (pgaAnnotationSourceNodeKeys artifact)
                )
        , let nid = NodeId k
        ]

stripPreparedWitnesslessAuthoritativeAnn
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> (AnnExpr, AnnExpr)
stripPreparedWitnesslessAuthoritativeAnn artifact =
    stripWitnesslessAuthoritativeAnnWith
        (eaEdgeWitnesses (pgaEdgeArtifacts artifact))

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
    sourceBinderRefs <-
        preparedSourceBinderRefsForAnn artifact authoritativeAnnCanon
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
            (pgaEdgeArtifacts artifact)
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
            ( constructionScheme
              , constructionSubst0
              , constructionInheritedGammaRoutes
              ) <-
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
            inheritedGammaRoutes <-
                Reify.mergeInheritedGammaRoutes
                    (rbrInheritedGammaRoutes rootBoundary)
                    constructionInheritedGammaRoutes
            prepareRequiredRootConstructionScope
                (pgaPresolutionView artifact)
                (pgaBindParentsGa artifact)
                annotationConstructionBinders
                (rbrLocallyClosedGammas rootBoundary)
                inheritedGammaRoutes
                completeRequirements
                constructionScheme
                constructionSubst

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
    constructionSourceBinderRefs <-
        preparedSourceBinderRefsForAnn artifact constructionAnnCanon
    resultSourceBinderRefs0 <-
        preparedSourceBinderRefsForAnn artifact authoritativeResultAnnCanon
    constructionDirectSourceBinderKeys <-
        preparedDirectSourceBinderKeysForAnn artifact constructionAnnCanon
    resultDirectSourceBinderKeys0 <-
        preparedDirectSourceBinderKeysForAnn
            artifact
            authoritativeResultAnnCanon
    resultSourceBinderRefs <-
        mergeCompilerExactConstructionBinderRefs
            constructionSourceBinderRefs
            resultSourceBinderRefs0
    constructionCertificateSourceBinderRefs <-
        mergeCompilerExactConstructionBinderRefs
            (pgaSourceBinderRefs artifact)
            constructionSourceBinderRefs
    resultCertificateSourceBinderRefs <-
        mergeCompilerExactConstructionBinderRefs
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
            (pgaEdgeArtifacts artifact)
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
            , localApplicationOwnerOccursIn
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
            (pgaEdgeArtifacts artifact)
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
        constructionRequirements =
            withAnnotationAmbient
                constructionAnnotationBinders
                (rbrRequirements constructionBoundary)
        resultRequirements =
            withAnnotationAmbient
                resultAnnotationBinders
                (rbrRequirements resultBoundary)
        sourceAnnotationExpectedType =
            transparentRootSourceAnnotationExpectedType
                authoritativeResultAnnCanon
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
            any
                ( \candidateRef ->
                    any
                        (typeBinderRefsSameIdentity candidateRef)
                        expectedRefs
                )
                closureResultRefs
          where
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
        hasResultLocalConstruction =
            not (null resultLocalGammaClosures)
                || any
                    ( not
                        . null
                        . localGammaConstructionBinders
                        . lgccConstruction
                    )
                    resultLocalApplicationCertificates
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
    let rootTarget =
            case grRequiredGammaBinders resultRequirements of
                [] -> preparedSchemeBodyTarget artifact (annNode authoritativeResultAnnCanon)
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
    sourceAnnotationRootScheme <-
        case sourceAnnotationExpectedType of
            Nothing -> pure Nothing
            Just expectedType -> do
                (graphScheme, graphSubst) <-
                    generalizeAtWithBuilderRequired
                        (pgaPlanBuilder artifact)
                        constructionRequirements
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
                    rootedBinders =
                        [ binder
                        | binder@(ref, _) <-
                            selectRootBinderClosure
                                (schemeBinderRefs graphScheme)
                                rootedFreeRefs
                        , not
                            ( any
                                (typeBinderRefsSameIdentity ref . fst)
                                annotationBinders
                            )
                        ]
                    -- Free annotation refs owned by this root's graph
                    -- generalization acquire the graph-planned binder (and
                    -- any bound dependencies).  The remaining refs retain
                    -- their source-side identity as ambient authority; they
                    -- are recorded by 'inheritedRootRefs' below and must not
                    -- be manufactured into unrelated root binders merely
                    -- because the annotation mentions them.
                    rootedAnnotationScheme =
                        mkElabSchemeWithRefs
                            (rootedBinders ++ annotationBinders)
                            (schemeBody routedAnnotationScheme)
                pure
                    ( Just
                        ( rootedAnnotationScheme
                        , annotationSubst
                        )
                    )
    (scheme, subst) <-
        case (sourceAnnotationRootScheme, mbOwnerFinalConstruction) of
            (Just annotationScheme, _)
                | not hasResultLocalConstruction ->
                -- A source annotation has already constructed and checked
                -- its edge-owned expected type once all internal Gamma
                -- computations have their own construction authority.  That
                -- expected type owns its forall declarations; rebuilding only
                -- the graph body would expose those declarations as free
                -- variables and then try to recover them after the fact.
                pure annotationScheme
            (_, Just certificate)
                | null
                    (freeTypeVarRefsType (ofcConstructedType certificate))
                , localGammaOwnerOnResultPath
                    (ofcOwner certificate)
                    constructionAnnCanon ->
                    -- The exact source constructor on the transparent result
                    -- path has already emitted and checked its complete
                    -- Figure 15.3.5 Lambda(Gamma) prefix.  A surrounding let
                    -- may preserve a lambda/application certificate, so the
                    -- authority is the recorded owner path rather than the
                    -- wrapper constructor kind.  Reifying the
                    -- pre-construction graph here would expose those locally
                    -- owned binders as residual free refs.
                    pure
                        ( ownerFinalRootScheme certificate
                        , ofcLocalBinderRoutes certificate
                        )
            _ -> do
                generalized@(graphScheme, graphSubst) <-
                    generalizeAtWithBuilderRequired
                        (pgaPlanBuilder artifact)
                        constructionRequirements
                        (Just (pgaBindParentsGa artifact))
                        (pgaPresolutionView artifact)
                        rootScope
                        rootTarget
                case mbOwnerFinalConstruction of
                    Just certificate
                        | not hasResultLocalConstruction
                        , localGammaOwnerOnResultPath
                            (ofcOwner certificate)
                            constructionAnnCanon
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
    let unvalidatedRootSchemeInfo = schemeInfoFromRefSubst scheme subst
    mRootRaiseMergeAuthority <-
        rootRaiseMergeAuthorityForExpression
            (pgaEdgeArtifacts artifact)
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
                        (pgaEdgeArtifacts artifact)
                        authoritativeResultAnnCanon
                        constructionRequirements
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
                ownedSubtermPackets
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
            descendantPackets
            (schemeInfoFromRefSubst schemeForPlacement placementSubst)
    schemePlaced <-
        placeSubtermGeneralizationBindersWithRoutes
            (siSubstRefs placementInfo)
            descendantPackets
            (siScheme placementInfo)
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
                ( constructionScheme
                  , constructionSubst0
                  , constructionGeneralizationInheritedRoutes
                  ) <-
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
                  , refMember sourceRef freeRootRefs
                  ]
                    ++ [ dependencyRef
                       | (dependencyRef, _) <-
                            prcsBinders requirementConstructionScope
                       , refMember dependencyRef freeRootRefs
                       ]
                )
        freeRootRefs =
            freeTypeVarRefsType (schemeToType schemeConstructedUnique)
        refMember ref = any (typeBinderRefsSameIdentity ref)
        insertDistinctRootRef ref refs
            | any (typeBinderRefsSameIdentity ref) refs = refs
            | otherwise = ref : refs
    rootClosure <-
        prepareRootClosureSchemeWithAmbient
            inheritedRootRefs
            constructionSourceBinderRefs
            mbResultOwnership
            resultLocalGammaClosures
            resultLocalApplicationCertificates
            constructedSubst
            schemeConstructedUnique
            mbOwnerFinalConstruction
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
            , prgSourceBinderRefs = resultSourceBinderRefs
            , prgDirectSourceBinderKeys = resultDirectSourceBinderKeys
            , prgConstructionScope = rootConstructionScope
            , prgConstructedGammaIdentities = constructedGammaIdentities
            }
  where
    transparentRootSourceAnnotationExpectedType ann =
        case ann of
            AAnn _ _ edgeId ->
                IntMap.lookup
                    (getEdgeId edgeId)
                    (pgaAnnotationExpectedTypesByEdge artifact)
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
                ALet _ _ _ _ _ _ _ body _ -> go body
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

localApplicationOwnerOccursIn :: LocalGammaOwner -> AnnExpr -> Bool
localApplicationOwnerOccursIn owner = go
  where
    go ann =
        ownerMatches ann
            || case ann of
                AResolvedVar{} -> False
                ALit{} -> False
                ALam _ _ _ _ body _ _ -> go body
                AApp fun argument _ _ _ -> go fun || go argument
                ALet _ _ _ _ _ _ rhs body _ -> go rhs || go body
                AExactAnn inner _ _ _ -> go inner
                AAnn inner _ _ -> go inner
                ALetScope inner _ _ -> go inner
                AUnfold inner _ _ -> go inner

    ownerMatches ann =
        case ann of
            AApp _ _ funSite _ applicationNode ->
                lgoConstructor owner == LocalApplicationGamma
                    && lgoBoundaryEdge owner == instantiationSiteEdgeId funSite
                    && lgoTermNode owner == applicationNode
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
        case
            [ claim
            | claim <-
                lgccDirectApplicationAmbientGammaClaims certificate
            , edgeKeySet (daagcEdgeIds claim)
                == edgeKeySet (lgcEdgeIds closure)
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

-- | A checked owner can also prove that a planned local Gamma slot was
-- discharged without emitting a binder.  This is the construction produced by
-- an exact identity application: the owner result and its term use neither the
-- pending consumer nor an alias routed to a locally emitted binder.  Require
-- all three negative facts from the owner-final certificate; the absence of a
-- root substitution alone is never discharge evidence.
ownerFinalConstructionDischargesLocalGammaClosure
    :: LocalGammaClosure
    -> OwnerFinalConstruction
    -> Bool
ownerFinalConstructionDischargesLocalGammaClosure closure certificate =
    ofcOwner certificate == lgcOwner closure
        && null (ofcLocallyEmittedBinderRefs certificate)
        && IntMap.null (ofcLocalBinderRoutes certificate)
        && isNothing
            (ownerFinalConstructionLocalRefFor certificate consumerRef)
        && not (any isConsumerRef constructedFreeRefs)
        && not (any isConsumerRef (ofcUsedAmbientBinderRefs certificate))
  where
    consumerIdentity = lgcConsumerIdentity closure
    consumerRef =
        typeBinderRefFromIdentity
            consumerIdentity
            (typeBinderIdentityStableName consumerIdentity)
    constructedFreeRefs =
        freeTypeVarRefsType (ofcConstructedType certificate)
    isConsumerRef ref =
        typeBinderRefIdentity ref == consumerIdentity

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
prepareRootClosureSchemeWithAmbient ambientRootRefs sourceBinderRefs mbOwnership localGammaClosures localApplicationCertificates fullSubst fullScheme0 mbOwnerFinalConstruction = do
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
                            (ownerFinalConstructionDischargesLocalGammaClosure closure)
                            mbOwnerFinalConstruction
                        )
                )
                refinementLocalGammaClosures
    gammaBinders <- traverse gammaBinder pendingLocalGammaClosures
    mapM_ validateApplicationCertificateRoutes localApplicationCertificates
    let gammaBinderRefs = map fst gammaBinders
        applicationBinders =
            concatMap
                (localGammaEmittedBinders . lgccConstruction)
                localApplicationCertificates
        applicationBinderRefs = map fst applicationBinders
        existingFullBinders = schemeBinderRefs fullScheme
        missingLocalBinders =
            foldl
                (insertMissingGammaBinder existingFullBinders)
                []
                (gammaBinders ++ applicationBinders)
        unrefinedConstructedFullScheme =
            mkElabSchemeWithRefs
                (existingFullBinders ++ missingLocalBinders)
                (schemeBody fullScheme)
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
        -- Packet ownership and LocalGammaClosure describe where a binder is
        -- expected to be constructed; they are not evidence that an
        -- ETyAbsRef was actually emitted.  Exact-root preparation runs before
        -- term elaboration, so only the checked owner certificate may move
        -- these pending binders below the root closure.  Application
        -- certificates already are post-elaboration emission evidence.
        certifiedPendingLocalRefs =
            case mbOwnerFinalConstruction of
                Just certificate
                    | ownerFinalConstructionMatchesLocalAuthority
                        mbOwnership
                        pendingLocalGammaClosures
                        certificate ->
                        pendingLocalRefs
                _ -> []
        localRefs =
            foldr insertDistinctRef []
                (certifiedPendingLocalRefs ++ applicationBinderRefs)
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
                        mbOwnerFinalConstruction
                )
    case mbOwnerFinalConstruction of
        Just certificate
            | not
                ( ownerFinalConstructionMatchesLocalAuthority
                    mbOwnership
                    pendingLocalGammaClosures
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
    constructedFullScheme <-
        case mbOwnerFinalConstruction of
            Nothing -> pure unrefinedConstructedFullScheme
            Just certificate -> do
                let refinementLocalRefs =
                        foldr
                            insertDistinctRef
                            []
                            ( localRefs
                                ++ ofcLocallyEmittedBinderRefs certificate
                            )
                refinedBinders <-
                    projectCertifiedBodyConsumerRootBounds
                        refinementLocalGammaClosures
                        (ofcUsedAmbientBinderRefs certificate)
                        refinementLocalRefs
                        (ofcBodyConsumerBoundRefinements certificate)
                        (schemeBinderRefs unrefinedConstructedFullScheme)
                pure
                    ( mkElabSchemeWithRefs
                        refinedBinders
                        (schemeBody unrefinedConstructedFullScheme)
                    )
    closure <- case localRefs of
        [] -> pure (PreparedWholeRootClosure ambientRootRefs constructedFullScheme)
        _
            | null localApplicationCertificates
            , Just ownerFinalConstruction <- mbOwnerFinalConstruction
            , ownerFinalConstructionMatchesLocalAuthority
                mbOwnership
                pendingLocalGammaClosures
                ownerFinalConstruction ->
                prepareCertifiedLocalRootClosure
                    ambientRootRefs
                    sourceBinderRefs
                    mbOwnership
                    pendingLocalGammaClosures
                    constructedFullScheme
                    localRefs
                    ownerFinalConstruction
        _
            | null localApplicationCertificates
            , Just ownerFinalConstruction <- mbOwnerFinalConstruction
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
                -- into root then local binders can therefore repair a graph
                -- identity's forward reference; forall-by-forall alpha
                -- equality is intentionally too strict for that ownership
                -- move.  Identity-bearing references make the actual safety
                -- condition explicit: every bound dependency must precede
                -- its binder in the constructed root -> local spine.
                reorderedBinders = retainedRootBinders ++ localBinders
                forwardBoundDependencies =
                    [ (binderRef, dependency)
                    | (binderIndex, (binderRef, Just bound)) <-
                        zip [0 :: Int ..] reorderedBinders
                    , dependency <- freeTypeVarRefsType (tyToElab bound)
                    , laterRef <- map fst (drop (binderIndex + 1) reorderedBinders)
                    , typeBinderRefsSameIdentity dependency laterRef
                    ]
                authority =
                    preparedLocalAuthority
                        mbOwnership
                        pendingLocalGammaClosures
                        localApplicationCertificates
                        checkedAmbientRefs
                        localScheme
            if null missingLocalRefs
                && length localBinders == length localRefs
                && null forwardBoundDependencies
                then
                    pure
                        ( PreparedLocalRootClosure
                            authority
                            closureScheme
                        )
                else
                    Left
                        ( ValidationFailed
                            [ "local result ownership does not match the constructed root binder spine"
                            , "  packet binders: " ++ show packetBinderRefs
                            , "  Gamma binders: " ++ show gammaBinderRefs
                            , "  application binders: " ++ show applicationBinderRefs
                            , "  matched local binders: " ++ show localBinders
                            , "  missing local refs: " ++ show missingLocalRefs
                            , "  forward bound dependencies after ownership partition: "
                                ++ show forwardBoundDependencies
                            , "  incoming full scheme: " ++ show fullScheme
                            , "  materialized full scheme: " ++ show constructedFullScheme
                            , "  root-only closure scheme: " ++ show closureScheme
                            , "  local Gamma closures: "
                                ++ show pendingLocalGammaClosures
                            , "  local application certificates: " ++ show localApplicationCertificates
                            , "  packet ownership: " ++ show mbOwnership
                            ]
                    )
    validatePreparedRootClosure "prepared root closure" closure
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
                                    , "  root substitution: " ++ show fullSubst
                                    , "  owner-final construction: "
                                        ++ show mbOwnerFinalConstruction
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
        certificate <- mbOwnerFinalConstruction
        if ofcOwner certificate == lgcOwner closure
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
        certificate <- mbOwnerFinalConstruction
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
        let knownRootRefs =
                map fst (schemeBinderRefs fullScheme)
                    ++ freeTypeVarRefsType (schemeToType fullScheme)
                    ++ IntMap.elems fullSubst
            foreignAmbientRefs =
                [ ambientRef
                | ambientRef <- lgccUsedAmbientBinderRefs certificate
                , not
                    ( any
                        (typeBinderRefsSameIdentity ambientRef)
                        knownRootRefs
                    )
                ]
        unless
            (null foreignAmbientRefs)
            ( Left
                ( ValidationFailed
                    [ "application Gamma certificate claims ambient identities outside the prepared root"
                    , "  owner: " ++ show (lgccOwner certificate)
                    , "  foreign ambient refs: " ++ show foreignAmbientRefs
                    , "  known root refs: " ++ show knownRootRefs
                    ]
                )
            )
      where
        validateBinder (emittedRef, _) =
            let graphRouteKeys =
                    [ nodeKey
                    | (nodeKey, routedRef) <-
                        IntMap.toList (lgccLocalBinderRoutes certificate)
                    , typeBinderRefsSameIdentity emittedRef routedRef
                    ]
                sourceAuthorityKeys =
                    [ nodeKey
                    | (nodeKey, sourceRef) <-
                        IntMap.toList
                            (lgccSourceBinderAuthorities certificate)
                    , typeBinderRefsSameIdentity emittedRef sourceRef
                    ]
                rootedRefs =
                    [ rootedRef
                    | nodeKey <- graphRouteKeys
                    , Just rootedRef <- [IntMap.lookup nodeKey fullSubst]
                    ]
                currentSourceRefs =
                    [ currentSourceRef
                    | nodeKey <- sourceAuthorityKeys
                    , Just currentSourceRef <-
                        [IntMap.lookup nodeKey sourceBinderRefs]
                    ]
                graphAuthorized =
                    not (null rootedRefs)
                        && all
                            (typeBinderRefsSameIdentity emittedRef)
                            rootedRefs
                sourceAuthorized =
                    not (null sourceAuthorityKeys)
                        && length currentSourceRefs
                            == length sourceAuthorityKeys
                        && all
                            (typeBinderRefsSameIdentity emittedRef)
                            currentSourceRefs
            in case (graphAuthorized, sourceAuthorized) of
                (True, False) -> pure ()
                (False, True) -> pure ()
                (True, True) ->
                    certificateBinderFailure
                        "binder claims both graph and source authority"
                        graphRouteKeys
                        sourceAuthorityKeys
                        rootedRefs
                        currentSourceRefs
                (False, False) ->
                    certificateBinderFailure
                        "binder has no matching graph or source authority"
                        graphRouteKeys
                        sourceAuthorityKeys
                        rootedRefs
                        currentSourceRefs
          where
            certificateBinderFailure detail graphRouteKeys sourceAuthorityKeys rootedRefs currentSourceRefs =
                Left
                    ( ValidationFailed
                        [ "application Gamma certificate binder authority is invalid"
                        , "  detail: " ++ detail
                        , "  owner: " ++ show (lgccOwner certificate)
                        , "  emitted binder: " ++ show emittedRef
                        , "  graph route keys: "
                            ++ show (map NodeId graphRouteKeys)
                        , "  source authority keys: "
                            ++ show (map NodeId sourceAuthorityKeys)
                        , "  rooted refs: " ++ show rootedRefs
                        , "  current source refs: " ++ show currentSourceRefs
                        , "  root substitution: " ++ show fullSubst
                        , "  source binder sidecar: "
                            ++ show sourceBinderRefs
                        ]
                    )

    missingConstructedBinder closure ref =
        Left
            ( ValidationFailed
                [ "result-local Gamma has no constructed root binder"
                , "  binder: " ++ show ref
                , "  closure: " ++ show closure
                , "  full scheme: " ++ show fullScheme
                ]
            )

    insertMissingGammaBinder existingBinders binders binder@(ref, _)
        | any (typeBinderRefsSameIdentity ref . fst) existingBinders = binders
        | any (typeBinderRefsSameIdentity ref . fst) binders = binders
        | otherwise = binders ++ [binder]

    insertDistinctRef ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs

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
        && all ((== certificateOwner) . lgcOwner) closures
  where
    certificateOwner = ofcOwner certificate
    hasLocalAuthority = isJust mbOwnership || not (null closures)
    packetOwnerMatches =
        case mbOwnership of
            Nothing -> True
            Just ownership ->
                subtermResultOwnershipLambdaNode ownership
                    == lgoTermNode certificateOwner

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

-- | Close a root from evidence produced by the local constructor itself.
-- The planner remains authoritative for binder order, bounds, and candidate
-- identities.  The owner certificate contributes only liveness: its locally
-- emitted identities are excluded from the root spine, and its ambient-use
-- certificate selects the still-needed root candidates.
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
    let plannedBinders = schemeBinderRefs plannedFullScheme
        certifiedLocalRefs = distinctRefs (ofcLocallyEmittedBinderRefs certificate)
        rawCertifiedAmbientRefs =
            distinctRefs (ofcUsedAmbientBinderRefs certificate)
        ambientSourceRenames =
            [ (ambientRef, sourceRef)
            | ambientRef <- rawCertifiedAmbientRefs
            , Just node <- [typeBinderRefNode ambientRef]
            , Just sourceRef <-
                [IntMap.lookup (getNodeId node) sourceBinderRefs]
            , not (typeBinderRefsSameIdentity ambientRef sourceRef)
            ]
        certifiedAmbientRefs =
            distinctRefs
                (map projectAmbientRef rawCertifiedAmbientRefs)
        certificateConstructedType =
            foldl
                ( \ty (ambientRef, sourceRef) ->
                    substTypeCaptureRef
                        ambientRef
                        (TVarRef sourceRef)
                        ty
                )
                (ofcConstructedType certificate)
                ambientSourceRenames
        projectAmbientRef ref =
            fromMaybe
                ref
                ( snd
                    <$> find
                        (typeBinderRefsSameIdentity ref . fst)
                        ambientSourceRenames
                )
        duplicateLocalRefs = duplicateRefs (ofcLocallyEmittedBinderRefs certificate)
        duplicateAmbientRefs = duplicateRefs (ofcUsedAmbientBinderRefs certificate)
        localAmbientOverlap =
            [ localRef
            | localRef <- certifiedLocalRefs
            , refMember localRef certifiedAmbientRefs
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
        plannedOwnerBinders =
            rootDependencyClosure plannedBinders expectedLocalRefs
        plannedOwnerRoutes =
            [ ( plannedRef
              , lookupByExpected plannedRef expectedLocalRoutes
                    <|> find
                        (typeBinderRefsSameIdentity plannedRef)
                        certifiedLocalRefs
              )
            | (plannedRef, _) <- plannedOwnerBinders
            ]
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
            , not
                ( any
                    (maybe False (typeBinderRefsSameIdentity certifiedRef) . snd)
                    plannedOwnerRoutes
                )
            ]
        plannedRootBinders =
            [ binder
            | binder@(ref, _) <- plannedBinders
            , not (refMember ref certifiedPlannedOwnerRefs)
            ]
        certificateScheme = schemeFromType certificateConstructedType
        certificateBinders = schemeBinderRefs certificateScheme
        unexpectedCertificateBinders =
            [ ref
            | (ref, _) <- certificateBinders
            , not (refMember ref certifiedLocalRefs)
            ]
        missingPlannedCertificateRoutes =
            [ binder
            | binder@(ref, _) <- certificateBinders
            , not
                ( any
                    ( \(plannedRef, mbCertifiedRef) ->
                        maybe
                            False
                            (typeBinderRefsSameIdentity ref)
                            mbCertifiedRef
                            && any
                                (typeBinderRefsSameIdentity plannedRef . fst)
                                plannedOwnerBinders
                    )
                    plannedOwnerRoutes
                )
            ]
        retainedCertificateBinders =
            [ (certifiedRef, certificateBound)
            | (plannedRef, _plannedBound) <- plannedOwnerBinders
            , Just certifiedRef <-
                [lookupByExpected plannedRef plannedOwnerRoutes]
            , Just (_, certificateBound) <-
                [find (typeBinderRefsSameIdentity certifiedRef . fst) certificateBinders]
            ]
        certificateBinderOrderMismatch =
            not
                ( sameRefOrder
                    (map fst certificateBinders)
                    (map fst retainedCertificateBinders)
                )
        certificateBoundMismatches =
            [ (certifiedRef, plannedBound, certificateBound)
            | (plannedRef, plannedBound) <- plannedOwnerBinders
            , Just certifiedRef <-
                [lookupByExpected plannedRef plannedOwnerRoutes]
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
                ++ [ ("duplicate ambient-use binder identities", show duplicateAmbientRefs)
                   | not (null duplicateAmbientRefs)
                   ]
                ++ [ ("local and ambient certificates overlap", show localAmbientOverlap)
                   | not (null localAmbientOverlap)
                   ]
                ++ [ ("owner certificate has no construction route for locally planned binders", show missingCertifiedLocalRoutes)
                   | not (null missingCertifiedLocalRoutes)
                   ]
                ++ [ ("owner certificate emits binders outside its local plan", show unexpectedCertifiedLocals)
                   | not (null unexpectedCertifiedLocals)
                   ]
                ++ [ ("constructed type binds an identity not emitted by the owner", show unexpectedCertificateBinders)
                   | not (null unexpectedCertificateBinders)
                   ]
                ++ [ ("constructed type binder has no routed planner authority", show missingPlannedCertificateRoutes)
                   | not (null missingPlannedCertificateRoutes)
                   ]
                ++ [ ("constructed type binder order disagrees with the planner", show (certificateBinders, retainedCertificateBinders))
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
                      , "  packet ownership: " ++ show mbOwnership
                      , "  local Gamma closures: " ++ show closures
                      , "  expected local refs: " ++ show expectedLocalRefs
                      , "  planned full scheme: " ++ show plannedFullScheme
                      , "  constructed type: " ++ show (ofcConstructedType certificate)
                      , "  certified local refs: " ++ show certifiedLocalRefs
                      , "  certified local routes: " ++ show (ofcLocalBinderRoutes certificate)
                      , "  certified ambient refs: " ++ show certifiedAmbientRefs
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
        , refMember ref closedRefs
        ]
      where
        closedRefs = close (distinctRefs initialRefs)
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
        localExteriorRefs
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
-- remains ambient.  Same-spelled peers and conflicting alias routes are
-- rejected rather than selected by precedence.
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
                    | otherwise ->
                        Left
                            ( ValidationFailed
                                [ "root construction dependency disagrees with an existing binder bound"
                                , "  binder: " ++ show ref
                                , "  existing bound: " ++ show existingBound
                                , "  dependency bound: " ++ show mbBound
                                ]
                            )

        insertEvidenceAlias binderRefs aliases (key, ref)
            | isLocal ref = pure aliases
            | not (refMember ref binderRefs) = pure aliases
            | otherwise =
                case IntMap.lookup key aliases of
                    Nothing -> pure (IntMap.insert key ref aliases)
                    Just existing
                        | typeBinderRefsSameIdentity existing ref ->
                            pure aliases
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
-- constructor retained at its nested placement.  The edge set, semantic
-- exterior, consumer identity, lexical owner, and either direct-application
-- occurrence provenance or flexible binding-tree path are all part of the
-- proof; a matching name or quotient representative is deliberately
-- insufficient.
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
                    RequiredGammaAtConstructionScope _ ->
                        invalidLocalClosure
                            "exact-scope root requirement is also claimed by a local constructor"
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
            (concatMap (freeTypeVarRefsType . rgbOperatedType) locallyClosedBinders)
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

    requiredRef = requiredGammaBinderConstructionRef constructionSubst

    -- A nested Figure 15.3.5 constructor emits its own exterior binder, but
    -- Lemma 15.3.5 still requires every free ref of that binder's S'(operated)
    -- bound to be present in the enclosing Gamma.  Resolve those ambient refs
    -- through the same construction substitution used by the anchor scheme;
    -- never pre-bind the nested exterior itself.
    collectLocalDependency rigidParents localExteriorRefs certifiedRoutes (refs, ambientBinders, aliases) dependency
        | refMember dependency localExteriorRefs =
            pure (refs, ambientBinders, aliases)
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
                            (pgaAnnotationExpectedTypesByEdge artifact)
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
                            (pgaAnnotationExpectedTypesByEdge artifact)
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
    :: PreparedRootClosure
    -> PreparedRootClosure
    -> IntMap.IntMap TypeBinderRef
    -> PreparedRootConstructionScope
    -> Either ElabError PreparedRootConstructionScope
publishRootSourceBinderAliases originalClosure projectedClosure sourceBinderRefs scope = do
    aliases <-
        reconcileRootSourceBinderAliases
            (preparedRootClosureBinderRefs originalClosure)
            (preparedRootClosureBinderRefs projectedClosure)
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
            PreparedLocalRootClosure authority scheme ->
                map fst (schemeBinderRefs scheme)
                    ++ map fst
                        ( schemeBinderRefs
                            (preparedLocalRootAuthorityScheme authority)
                        )

-- | Reconcile graph aliases with the binder projection performed while
-- rebuilding a root closure.  A conflicting alias may be replaced only when
-- it names a binder owned by the pre-projection closure and the source
-- identity is now owned by the rebuilt closure.  Ordinarily the old identity
-- disappears from the rebuilt closure.  If its declaration is still present
-- as representation lag, replacement additionally requires that identity's
-- own graph node to equal the source-sidecar key.  An independent surviving
-- binder therefore remains a conflict.  No spelling or positional fallback
-- participates.
reconcileRootSourceBinderAliases
    :: [TypeBinderRef]
    -> [TypeBinderRef]
    -> IntMap.IntMap TypeBinderRef
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
reconcileRootSourceBinderAliases originalBinderRefs projectedBinderRefs sourceBinderRefs aliases =
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
               )

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
