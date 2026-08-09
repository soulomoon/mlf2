{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Elab.Generalize
-- Description : Apply generalization plans to produce elaborated types
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- This module applies generalization plans (produced by the presolution phase)
-- to produce elaborated types with explicit polymorphism. It coordinates the
-- binder naming, scheme reification, and finalization steps.
--
-- = Process
--
-- 1. Receive 'GeneralizePlan' from presolution
-- 2. Build 'BinderPlan' for naming quantified variables
-- 3. Reify the type using 'ReifyPlan'
-- 4. Finalize schemes with proper binders
--
-- See 'MLF.Constraint.Presolution.Plan' for the plan generation logic.
module MLF.Elab.Generalize
  ( GaBindParents (..),
    gaConstructionRouteNode,
    gaConstructionRouteNodes,
    GeneralizedResultRouteLocator (..),
    GeneralizedResultRouteRequest (..),
    GeneralizedResultRoute (..),
    sourceLambdaGeneralizedResultRouteRequest,
    certifyGeneralizedResultRoute,
    GammaPacketAuthority (..),
    LocalGammaConstructor (..),
    LocalGammaOwner (..),
    localGammaOwnerOccursIn,
    localGammaOwnerEncloses,
    LocalGammaFrame,
    lgfOwner,
    lgfDirectEdgeSources,
    lgfChildren,
    LocalGammaEdgeOwnership (..),
    localGammaDirectApplicationEdgeOwners,
    localGammaPreparedEnclosingEdgeOwners,
    selectLocalGammaEdgeOwnership,
    LocalGammaClosure (..),
    directApplicationClosureOwnsEdges,
    directLetBoundaryEdge,
    administrativeLambdaBody,
    localGammaFrame,
    localGammaOwnerScope,
    retainedDescendantGammaClosures,
    inheritDescendantGammaRequirements,
    IdentityTopologyConsumerAuthority,
    mkIdentityTopologyConsumerAuthority,
    itcaEdgeId,
    itcaSourceScopeRoot,
    itcaSourceBodyRoot,
    itcaBoundaryScopeRoot,
    itcaBoundaryBodyRoot,
    itcaFrozenResultRoot,
    itcaOwner,
    SubtermConsumerAuthority,
    SubtermConsumerKey,
    subtermConsumerKey,
    subtermConsumerAuthorityKey,
    scaEdgeId,
    scaConsumerIdentity,
    subtermConsumerAuthorityEnclosingOwner,
    subtermConsumerAuthorityIsTopology,
    subtermConsumerAuthorityIsRootGamma,
    CompilerExactResultStage (..),
    PreparedSubtermGeneralization,
    RootRaiseMergeAuthority (..),
    SubtermPacketPlacement (..),
    SubtermResultOwnership,
    SubtermGeneralizations,
    applyGeneralizePlan,
    inlineRigidTypes,
    mergeSubtermGeneralizations,
    pairSubtermGeneralizationRoots,
    packetTypeSpecializesToExactEndpoint,
    selectConstructionRequirementEndpoint,
    freshenSchemeInfoBinderNamesAgainst,
    placeSubtermGeneralizationBinders,
    placeSubtermGeneralizationBindersWithRoutes,
    publishPlacedSubtermConstructionBinderOrder,
    PlacedSubtermBinders,
    placedSubtermBinderScheme,
    placedSubtermCopiedBinderRoutes,
    placedSubtermConstructedConsumerIdentities,
    placeSubtermGeneralizationBindersWithRoutesAndProvenance,
    placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy,
    publishTopologyConsumerRoutes,
    publishSourceLambdaTopologyConsumerRoute,
    publishRootRaiseMergePacketResultRoute,
    prepareSubtermGeneralizationPacket,
    prepareRootRaiseMergeScheme,
    prepareRootRaiseMergeSchemeAtEdge,
    requiredGammaBinderForRootRaiseMerge,
    rootRaiseMergeAuthorityFor,
    rootRaiseMergeAuthorityForExpression,
    rootRaiseMergeExteriorIdentityFor,
    rootRaiseMergeExteriorOwnedByScope,
    RootEdgeExactEndpoint (..),
    rootEdgeExactEndpointType,
    mapRootEdgeExactEndpoint,
    generalizationRequirementsForRootEdges,
    LexicalTypeAbsClosure,
    mkLexicalTypeAbsClosure,
    generalizationRequirementsForRootEdgesInConstruction,
    generalizationRequirementsForRootEdgesInConstructionWithLexicalClosures,
    generalizationRequirementsForRootExactEdgesInConstruction,
    generalizationRequirementsForRootExactEdgesInConstructionWithLexicalClosures,
    generalizationRequirementsForEnclosingRootEdges,
    generalizationRequirementsForEnclosingRootExactEdges,
    resolveFrozenOperatedOccurrenceEndpoint,
    resolveAmbientGammaOperatedEndpoint,
    subtermGeneralizationSchemeInfo,
    subtermGeneralizationConsumerConstructionSchemeInfo,
    subtermGeneralizationConsumerProjectionSchemeInfo,
    subtermGeneralizationOperatedSchemeInfo,
    subtermGeneralizationCompilerExactBoundary,
    subtermGeneralizationCompilerExactResultRef,
    subtermGeneralizationCompilerExactExistingRef,
    subtermGeneralizationCompilerExactCompletionRef,
    subtermGeneralizationCompilerExactResultStage,
    subtermGeneralizationCompilerExactResultIsDelegated,
    subtermGeneralizationCompilerExactBinderRenames,
    subtermGeneralizationConstructionBinderRenames,
    subtermGeneralizationExactConsumerSpecialization,
    subtermGeneralizationSourceOwnerConsumerCompletion,
    subtermGeneralizationSourceOwnerFinalConsumerCompletion,
    subtermGeneralizationOpaqueResultConstruction,
    subtermGeneralizationOpaqueResultConstructionPlan,
    subtermGeneralizationOpaqueResultSourceLambdaCompletion,
    subtermGeneralizationCopiedBinderRoutes,
    subtermGeneralizationPlacedCopiedBinderRoutes,
    subtermGeneralizationInheritedGammaRoutes,
    withInheritedGammaRoutes,
    withConstructionBinderRenames,
    withPlacedCopiedBinderRoutes,
    withSourceLambdaParameter,
    withExactConsumerSpecialization,
    withSourceOwnerConsumerCompletion,
    withSourceOwnerFinalConsumerCompletion,
    withOpaqueResultConstruction,
    publishSubtermGammaConstructionSourceSchemeInfo,
    withCompilerExactBinderRenames,
    resolveSubtermLocalResultAtConstruction,
    withCompilerExactSourceSubtermResult,
    withCompilerExactPacketSubtermResult,
    withCompilerExactDescendantSubtermResult,
    withCompilerExactEnclosingSubtermResult,
    subtermGeneralizationConsumerIdentity,
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationLocalResultAuthority,
    subtermGeneralizationGammaAuthority,
    subtermGeneralizationResultAbstractionRef,
    subtermGeneralizationConstructionResultAbstractionRef,
    subtermGeneralizationGammaBoundScheme,
    subtermGeneralizationSourceLambdaParameter,
    subtermGeneralizationApplicationSourceLambdaParameter,
    subtermGeneralizationAdministrativeLambdaResultConstruction,
    subtermGeneralizationSourceStagedAdministrativeLambdaResultConstruction,
    subtermGeneralizationGammaBoundHasNestedSourceDeclaration,
    subtermGeneralizationSourceLambdaResultConstruction,
    subtermGeneralizationApplicationSourceLambdaResultConstruction,
    directAdministrativeLambdaApplicationResultConstructionFor,
    directAdministrativeLambdaApplicationSourceOwnerCompletionFor,
    subtermGeneralizationGammaBoundSchemeForConsumer,
    subtermGeneralizationOperatedSchemeForConsumer,
    subtermGeneralizationLocalConsumerClosure,
    subtermGeneralizationOwnsGammaEdge,
    subtermGeneralizationOwnsGammaForEdge,
    DirectLambdaApplicationResultConstruction (..),
    directLambdaApplicationResultConstructionType,
    directLambdaApplicationResultConstructionFor,
    subtermResultOwnershipFor,
    subtermResultOwnershipForResolvedLetAlias,
    subtermResultOwnershipConsumerClosedLocally,
    subtermResultOwnershipLocalSourceDeclarationRefs,
    subtermResultOwnershipHasTransparentPath,
    subtermResultOwnershipLambdaArity,
    subtermResultOwnershipLambdaNode,
    subtermResultOwnershipPacket,
    subtermGeneralizationsOwnedBy,
    shadowCompareTypes,
    selectSolvedOrderWithShadow,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import qualified Data.Set as Set
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution (EdgeTrace (..))
import MLF.Constraint.Presolution.Base
  ( EdgeArtifacts,
    edgeArtifactTrace,
    edgeArtifactWitness,
    lookupEdgeArtifact,
    rootRaiseMergeTraceAuthority,
    rootWeakenRaiseMergeTraceAuthority,
  )
import MLF.Constraint.Presolution.Plan
  ( GeneralizePlan (..),
    ReifyPlan (..),
  )
import MLF.Constraint.Presolution.Plan.BinderPlan (BinderPlan (..))
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    GeneralizeCtx (..),
    GeneralizeEnv (..),
    SolvedToBaseResolution (..),
    resolveGaSolvedToBase,
    traceGeneralizeM,
  )
import MLF.Constraint.Presolution.Plan.Finalize
  ( FinalizeInput (..),
    finalizeScheme,
    mkFinalizeBinderPlan,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( AmbientGammaAuthority (..),
    GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    RequiredGammaPlacement (..),
  )
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Constraint.Presolution.Plan.SchemeRoots
  ( SchemeRootInfo (..),
    SchemeRootsPlan (..),
    allowBoundTraversalFor,
  )
import MLF.Constraint.Presolution.Plan.Target
  ( ReifyRootSource (..),
    TypeRootPlan (..),
  )
import MLF.Constraint.Presolution.View (PresolutionView (..), pvCanonicalMap)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
  ( InstanceOp (..),
    ewWitness,
    getInstanceOps,
  )
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Types
import MLF.Elab.Inst
  ( applyInstantiation,
    instForLeadingTypeArgument,
    schemeToType,
  )
import MLF.Elab.Run.Instantiation
  ( ExactBinderSpinePlan,
    exactBinderSpineRenames,
    exactBinderSpineInstantiation,
    planExactBinderSpine,
  )
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Elab.SourceBinder
  ( orderSourceProjectedSchemeBinders,
    resolveConstructionSourceBindersInPacketAtExpected,
    resolveConstructionSourceBindersInTypeExcept,
    resolveConstructionSourceBindersInTypeAtExpected,
    typeBinderDeclarationRefs,
  )
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeWithCanonicalExcept)
import MLF.Frontend.ConstraintGen.Types
  ( AnnExpr (..),
    InstantiationSite (..),
    InstantiationTargetTopology (..),
    instantiationSiteEdgeId,
    instantiationSiteTargetTopology,
  )
import MLF.Reify.Core
  ( reifyBoundWithExternalRefs,
    reifyBoundWithExternalRefsOnConstraint,
    reifyBoundWithRefs,
    reifyBoundWithRefsOnConstraint,
    reifyTypeWithOuterBinderRefsNoFallback,
    reifyTypeWithOuterBinderRefsNoFallbackOnConstraint
  )
import qualified MLF.Reify.Core as ReifyCore
import
  MLF.Reify.TypeOps
    ( alphaEqType,
      alphaEqTypePreservingStructuralBinders,
      churchAwareEqType,
      churchRepresentationEqType,
      freeTypeVarRefsType,
      inlineAliasBoundsWithBy,
      splitForallsRefs,
      substTypeCaptureRef,
      typeHeadMatches,
    )
import MLF.Types.Identity
  ( IdDetails,
    IdentityGenerator,
    advanceIdentityGeneratorPastMany,
    typeBinderIdentityGeneratedUnique,
    typeBinderIdentityStableName,
    typeBinderIdentityStructural,
  )
import MLF.Util.Graph (reachableFromStop)
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Util.Names (alphaName)

-- | The exact lexical Gamma that owns one lambda-body construction edge.
-- Keeping the edge and gen together prevents later consumers from deriving a
-- different owner from a canonicalized result node.
data GammaPacketAuthority = GammaPacketAuthority
  { gpaEdgeId :: !EdgeId
  , gpaOwnerGen :: !GenNodeId
  , gpaConsumerIdentity :: !TypeBinderIdentity
  }
  deriving (Eq, Show)

-- | The source constructor that must emit one local Figure 15.3.5 Gamma.
-- Keeping this distinction typed prevents an application, let, and lambda
-- that happen to share a scope from discharging one another's obligation.
data LocalGammaConstructor
  = LocalLambdaGamma
  | LocalApplicationGamma
  | LocalLetGamma
  deriving (Eq, Show)

-- | Exact source-tree owner of a local construction Gamma.  The boundary edge
-- is the occurrence identity: canonicalization may merge constructor result
-- nodes, but it never merges their source instantiation edges.  The scope is
-- the paper's gen node at which this occurrence introduces its abstractions.
data LocalGammaOwner = LocalGammaOwner
  { lgoConstructor :: !LocalGammaConstructor,
    lgoBoundaryEdge :: !EdgeId,
    lgoTermNode :: !NodeId,
    lgoScope :: !NodeRef
  }
  deriving (Eq, Show)

localGammaOwnerScope :: LocalGammaOwner -> NodeRef
localGammaOwnerScope = lgoScope

-- | Whether the exact source constructor recorded by a local-Gamma
-- certificate occurs anywhere in an annotated expression.
--
-- Edge and term-node identity select the occurrence. Lambda scope is checked
-- as well because it is carried directly by the source constructor; an
-- application or let scope is recovered separately by boundary preparation.
localGammaOwnerOccursIn :: LocalGammaOwner -> AnnExpr -> Bool
localGammaOwnerOccursIn owner = go
  where
    go ann =
      localGammaOwnerMatches owner ann
        || case ann of
          AResolvedVar {} -> False
          ALit {} -> False
          ALam _ _ _ _ body _ _ -> go body
          AApp fun argument _ _ _ -> go fun || go argument
          ALet _ _ _ _ _ _ rhs body _ -> go rhs || go body
          AExactAnn inner _ _ _ -> go inner
          AAnn inner _ _ -> go inner
          ALetScope inner _ _ -> go inner
          AUnfold inner _ _ -> go inner

-- | Whether one exact source constructor lexically encloses another.  This is
-- stronger than sharing a graph scope: the outer occurrence must be found in
-- the annotated source tree, and the nested occurrence must occur in that
-- constructor's actual subtree.
localGammaOwnerEncloses
  :: LocalGammaOwner
  -> LocalGammaOwner
  -> AnnExpr
  -> Bool
localGammaOwnerEncloses outerOwner nestedOwner = go
  where
    go ann
      | localGammaOwnerMatches outerOwner ann =
          outerOwner /= nestedOwner
            && localGammaOwnerOccursIn nestedOwner ann
      | otherwise =
          case ann of
            AResolvedVar {} -> False
            ALit {} -> False
            ALam _ _ _ _ body _ _ -> go body
            AApp fun argument _ _ _ -> go fun || go argument
            ALet _ _ _ _ _ _ rhs body _ -> go rhs || go body
            AExactAnn inner _ _ _ -> go inner
            AAnn inner _ _ -> go inner
            ALetScope inner _ _ -> go inner
            AUnfold inner _ _ -> go inner

localGammaOwnerMatches :: LocalGammaOwner -> AnnExpr -> Bool
localGammaOwnerMatches owner ann =
  case ann of
    ALam _ _ _ lambdaScope _ bodyEdge lambdaNode ->
      lgoConstructor owner == LocalLambdaGamma
        && lgoBoundaryEdge owner == bodyEdge
        && lgoTermNode owner == lambdaNode
        && lgoScope owner == GenRef lambdaScope
    AApp _ _ funSite _ applicationNode ->
      lgoConstructor owner == LocalApplicationGamma
        && lgoBoundaryEdge owner == instantiationSiteEdgeId funSite
        && lgoTermNode owner == applicationNode
    ALet _ _ _ _ _ _ _ body resultNode ->
      lgoConstructor owner == LocalLetGamma
        && case body of
          ALetScope _ _ edgeId ->
            lgoBoundaryEdge owner == edgeId
              && lgoTermNode owner == resultNode
          _ -> False
    _ -> False

-- | Construction proof retained for the identity case of a lambda-body
-- instantiation edge.  The source scope/body/result roots are frozen before
-- quotienting, and the owner retains the exact source lambda that may consume
-- the proof.  The constructor is private: callers can only create a value
-- through 'mkIdentityTopologyConsumerAuthority', which makes a mismatched
-- edge, scope, or non-lambda owner unrepresentable.
data IdentityTopologyConsumerAuthority =
  IdentityTopologyConsumerAuthority
    { itcaEdgeId :: !EdgeId
    -- | Frozen pre-normalization scope used by the identity-topology proof.
    , itcaSourceScopeRoot :: !GenNodeId
    -- | Frozen edge-trace source root; this may be hidden by administrative
    -- source wrappers after canonical annotation pairing.
    , itcaSourceBodyRoot :: !NodeId
    -- | Scope retained by the paired canonical source lambda.
    , itcaBoundaryScopeRoot :: !GenNodeId
    -- | Result node of the paired canonical lambda body.
    , itcaBoundaryBodyRoot :: !NodeId
    , itcaFrozenResultRoot :: !NodeId
    , itcaOwner :: !LocalGammaOwner
    }
  deriving (Eq, Show)

mkIdentityTopologyConsumerAuthority
  :: IntMap.IntMap NodeId
  -> EdgeId
  -> GenNodeId
  -> NodeId
  -> GenNodeId
  -> NodeId
  -> NodeId
  -> LocalGammaOwner
  -> Either ElabError IdentityTopologyConsumerAuthority
mkIdentityTopologyConsumerAuthority
  restoredSchemeRootTargets
  edgeId
  sourceScopeRoot
  sourceBodyRoot
  boundaryScopeRoot
  boundaryBodyRoot
  frozenResultRoot
  owner = do
  unless
    (lgoConstructor owner == LocalLambdaGamma)
    (authorityFailure "owner is not a lambda constructor")
  unless
    (lgoBoundaryEdge owner == edgeId)
    (authorityFailure "owner boundary edge differs from the topology edge")
  unless
    (lgoScope owner == GenRef boundaryScopeRoot)
    (authorityFailure "owner scope differs from the paired source boundary")
  unless
    ( IntMap.lookup
        (getNodeId sourceBodyRoot)
        restoredSchemeRootTargets
        == Just boundaryBodyRoot
    )
    (authorityFailure "source body root is not restored to the paired boundary body root")
  pure
    IdentityTopologyConsumerAuthority
      { itcaEdgeId = edgeId
      , itcaSourceScopeRoot = sourceScopeRoot
      , itcaSourceBodyRoot = sourceBodyRoot
      , itcaBoundaryScopeRoot = boundaryScopeRoot
      , itcaBoundaryBodyRoot = boundaryBodyRoot
      , itcaFrozenResultRoot = frozenResultRoot
      , itcaOwner = owner
      }
  where
    authorityFailure detail =
      Left
        ( ValidationFailed
            [ "invalid identity-topology consumer authority"
            , "  detail: " ++ detail
            , "  edge: " ++ show edgeId
            , "  source scope: " ++ show sourceScopeRoot
            , "  source body root: " ++ show sourceBodyRoot
            , "  restored source target: "
                ++ show
                  ( IntMap.lookup
                      (getNodeId sourceBodyRoot)
                      restoredSchemeRootTargets
                  )
            , "  boundary scope: " ++ show boundaryScopeRoot
            , "  boundary body root: " ++ show boundaryBodyRoot
            , "  frozen result root: " ++ show frozenResultRoot
            , "  owner: " ++ show owner
            ]
        )

-- | One source-tree layer of local-Gamma ownership.  Both root preparation
-- and term construction must agree on which constructor owns a boundary, which
-- direct instantiation edges it covers, and which children inherit that owner.
-- Keeping that syntax-directed decomposition here prevents the two traversals
-- from independently choosing (for example) an application's argument edge as
-- its occurrence identity.
data LocalGammaFrame = LocalGammaFrame
  !(Maybe LocalGammaOwner)
  ![(EdgeId, AnnExpr)]
  ![AnnExpr]
  deriving (Eq, Show)

lgfOwner :: LocalGammaFrame -> Maybe LocalGammaOwner
lgfOwner (LocalGammaFrame owner _ _) = owner

lgfDirectEdgeSources :: LocalGammaFrame -> [(EdgeId, AnnExpr)]
lgfDirectEdgeSources (LocalGammaFrame _ edgeSources _) = edgeSources

lgfChildren :: LocalGammaFrame -> [AnnExpr]
lgfChildren (LocalGammaFrame _ _ children) = children

-- | Typed ownership decision for one instantiation edge named by a
-- 'LocalGammaFrame'.  A direct application source is an edge-local
-- Figure 15.3.5 obligation; it deliberately does not imply that the
-- application's scope flexibly owns the frozen exterior.  Every other frame
-- uses the nearest flexible-exterior owner.
--
-- Root preparation and term construction must consume this same decision.
-- In particular, an enclosing lambda must not claim an edge after the source
-- application frame selected the direct lane merely because the lambda owns
-- the exterior in the binding tree.
data LocalGammaEdgeOwnership
  = DirectApplicationEdgeOwnership !LocalGammaOwner
  | PreparedEnclosingEdgeOwnership !LocalGammaOwner
  | FlexibleExteriorEdgeOwnership !LocalGammaOwner
  deriving (Eq, Show)

selectLocalGammaEdgeOwnership
  :: IntMap.IntMap LocalGammaOwner
  -> IntMap.IntMap LocalGammaOwner
  -> EdgeId
  -> [LocalGammaOwner]
  -> (LocalGammaOwner -> Bool)
  -> Either ElabError (Maybe LocalGammaEdgeOwnership)
selectLocalGammaEdgeOwnership directOwners preparedOwners edgeId owners ownsExterior =
  case
      ( IntMap.lookup edgeKey directOwners
      , IntMap.lookup edgeKey preparedOwners
      )
    of
      (Just directOwner, Just preparedOwner)
        | directOwner /= preparedOwner ->
            Left
              ( ValidationFailed
                  [ "one local Gamma edge has conflicting exact source owners"
                  , "  edge: " ++ show edgeId
                  , "  direct application owner: " ++ show directOwner
                  , "  prepared enclosing owner: " ++ show preparedOwner
                  ]
              )
      (Just owner, _) ->
        pure (Just (DirectApplicationEdgeOwnership owner))
      (Nothing, Just owner) ->
        pure (Just (PreparedEnclosingEdgeOwnership owner))
      (Nothing, Nothing) ->
        pure
          ( FlexibleExteriorEdgeOwnership
              <$> find ownsExterior owners
          )
  where
    edgeKey = getEdgeId edgeId

-- | Collect the source-owned direct edge lane before either root preparation
-- or term construction selects flexible-exterior owners.  The same paper edge
-- can be visited again through a transparent operand wrapper; retaining the
-- edge-to-owner map makes direct precedence sticky across that subtree instead
-- of letting the wrapper reassign the edge to an enclosing lambda.
localGammaDirectApplicationEdgeOwners
  :: (EdgeId -> NodeId -> Either ElabError NodeRef)
  -> AnnExpr
  -> Either ElabError (IntMap.IntMap LocalGammaOwner)
localGammaDirectApplicationEdgeOwners scopeForBoundary =
  go IntMap.empty
  where
    go directOwners expr = do
      frame <- localGammaFrame scopeForBoundary expr
      directOwners' <-
        case lgfOwner frame of
          Just owner
            | lgoConstructor owner == LocalApplicationGamma -> do
                let directEdges =
                      map fst (lgfDirectEdgeSources frame)
                    directEdgeKeys =
                      IntSet.fromList (map getEdgeId directEdges)
                unless
                  (length directEdges == IntSet.size directEdgeKeys)
                  ( Left
                      ( ValidationFailed
                          [ "one application frame repeats a direct source edge"
                          , "  owner: " ++ show owner
                          , "  direct edges: " ++ show directEdges
                          ]
                      )
                  )
                foldM
                  (insertDirectOwner owner)
                  directOwners
                  directEdges
          _ -> pure directOwners
      foldM go directOwners' (lgfChildren frame)

    insertDirectOwner owner directOwners edgeId =
      case IntMap.lookup (getEdgeId edgeId) directOwners of
        Nothing ->
          pure
            ( IntMap.insert
                (getEdgeId edgeId)
                owner
                directOwners
            )
        Just existing ->
          Left
            ( ValidationFailed
                [ "one direct application edge is named by multiple source occurrences"
                , "  edge: " ++ show edgeId
                , "  first owner: " ++ show existing
                , "  second owner: " ++ show owner
                ]
            )

-- | Recover exact enclosing-constructor ownership retained by prepared
-- subterm packets.  This authority sits between syntax-owned direct
-- application edges and the frozen binding-tree fallback: it names the exact
-- source edge and constructor that already prepared its Figure 15.3.5 Gamma,
-- even when presolution later moves the shared exterior to an ancestor gen.
-- Conflicting packets are rejected here, before either root preparation or
-- term construction can make a traversal-order-dependent choice.
localGammaPreparedEnclosingEdgeOwners
  :: [PreparedSubtermGeneralization]
  -> Either ElabError (IntMap.IntMap LocalGammaOwner)
localGammaPreparedEnclosingEdgeOwners =
  foldM insertPacketOwner IntMap.empty
  where
    insertPacketOwner owners packet =
      case
          psgConsumerAuthority packet
            >>= \authority ->
              (,) (scaEdgeId authority)
                <$> subtermConsumerAuthorityEnclosingOwner authority
        of
          Nothing -> pure owners
          Just (edgeId, owner) ->
            case IntMap.lookup (getEdgeId edgeId) owners of
              Nothing ->
                pure
                  ( IntMap.insert
                      (getEdgeId edgeId)
                      owner
                      owners
                  )
              Just existing
                | existing == owner -> pure owners
                | otherwise ->
                    Left
                      ( ValidationFailed
                          [ "prepared packets assign one local Gamma edge to multiple enclosing owners"
                          , "  edge: " ++ show edgeId
                          , "  first owner: " ++ show existing
                          , "  second owner: " ++ show owner
                          ]
                      )

-- | The constraint-only edge wrapped immediately around a let body is the
-- exact occurrence boundary for that let's local Gamma.  A missing wrapper is
-- an invalid prepared annotation, not a reason to derive an owner from a node.
directLetBoundaryEdge :: AnnExpr -> Either ElabError EdgeId
directLetBoundaryEdge body =
  case body of
    ALetScope _ _ edgeId -> pure edgeId
    _ ->
      Left
        ( ValidationFailed
            [ "let construction is missing its constraint-only scope edge",
              "  body: " ++ show body
            ]
        )

-- | Recover the next value abstraction in the exact administrative shape
-- used by constraint generation for a source lambda spine.  An annotated
-- parameter is represented by a transparent let mediator followed by an
-- identity let-scope edge; that mediator is not a term boundary.  Keep this
-- owner check here so preparation and later topology publication count the
-- same source lambdas instead of independently searching an arrow type.
administrativeLambdaBody
  :: IdDetails
  -> AnnExpr
  -> Maybe AnnExpr
administrativeLambdaBody ownerDetails body =
  case body of
    nested@ALam {} -> Just nested
    _ -> do
      (mediatorDetails, innerBody) <-
        desugaredAnnotatedLambdaBody ownerDetails body
      guard
        ( idDetailsIdentityKey mediatorDetails
            /= idDetailsIdentityKey ownerDetails
        )
      case innerBody of
        ALetScope nested@ALam {} _ _ -> Just nested
        _ -> Nothing

-- | Recover the result-transparent let mediator introduced for an annotated
-- lambda parameter.  Its inner let-scope, rather than the administrative
-- let's result node, is the lambda's source construction boundary.
desugaredAnnotatedLambdaBody
  :: IdDetails
  -> AnnExpr
  -> Maybe (IdDetails, AnnExpr)
desugaredAnnotatedLambdaBody ownerDetails body =
  case body of
    ALet _ mediatorDetails _ _ _ _ rhs innerBody _
      | annotationRefersTo ownerDetails rhs ->
          Just (mediatorDetails, innerBody)
    _ -> Nothing
  where
    annotationRefersTo expectedDetails ann =
      case ann of
        AResolvedVar details _ _ ->
          idDetailsIdentityKey details
            == idDetailsIdentityKey expectedDetails
        AAnn inner _ _ -> annotationRefersTo expectedDetails inner
        AExactAnn inner _ _ _ -> annotationRefersTo expectedDetails inner
        ALetScope inner _ _ -> annotationRefersTo expectedDetails inner
        AUnfold inner _ _ -> annotationRefersTo expectedDetails inner
        _ -> False

-- | Whether a let's result is exactly its own resolved binding occurrence.
-- The constraint-only let-scope wrapper does not change that identity.  No
-- name comparison or type-shape comparison participates in this decision.
letBodyReturnsResolvedBinding :: IdDetails -> AnnExpr -> Bool
letBodyReturnsResolvedBinding expectedDetails = aliasBody
  where
    aliasBody ann =
      case ann of
        AResolvedVar actualDetails _ _ ->
          idDetailsIdentityKey actualDetails
            == idDetailsIdentityKey expectedDetails
        ALetScope inner _ _ -> aliasBody inner
        _ -> False

-- | Decompose exactly one annotated source layer into its local-Gamma owner,
-- direct edge sources, and recursive children.  Scope recovery is required
-- only for application and let owners; lambda scope is carried directly by its
-- source annotation.
localGammaFrame
  :: (EdgeId -> NodeId -> Either ElabError NodeRef)
  -> AnnExpr
  -> Either ElabError LocalGammaFrame
localGammaFrame scopeForBoundary ann =
  case ann of
    AResolvedVar {} -> pure emptyFrame
    ALit {} -> pure emptyFrame
    ALam _ _ _ lambdaScope body bodyEdge lambdaNode ->
      pure $
        LocalGammaFrame
          ( Just
              LocalGammaOwner
                { lgoConstructor = LocalLambdaGamma,
                  lgoBoundaryEdge = bodyEdge,
                  lgoTermNode = lambdaNode,
                  lgoScope = GenRef lambdaScope
                }
          )
          [(bodyEdge, body)]
          [body]
    AApp fun arg funSite argSite applicationNode -> do
      let applicationBoundary = instantiationSiteEdgeId funSite
      applicationScope <-
        scopeForBoundary applicationBoundary applicationNode
      pure $
        LocalGammaFrame
          ( Just
              LocalGammaOwner
                { lgoConstructor = LocalApplicationGamma,
                  lgoBoundaryEdge = applicationBoundary,
                  lgoTermNode = applicationNode,
                  lgoScope = applicationScope
                }
          )
          [ (applicationBoundary, fun),
            (instantiationSiteEdgeId argSite, arg)
          ]
          [fun, arg]
    ALet _ _ _ _ _ _ rhs body resultNode -> do
      letBoundary <- directLetBoundaryEdge body
      letScope <- scopeForBoundary letBoundary resultNode
      pure $
        LocalGammaFrame
          ( Just
              LocalGammaOwner
                { lgoConstructor = LocalLetGamma,
                  lgoBoundaryEdge = letBoundary,
                  lgoTermNode = resultNode,
                  lgoScope = letScope
                }
          )
          []
          [rhs, body]
    AExactAnn inner _ _ edgeId ->
      pure (wrapperFrame edgeId inner)
    AAnn inner _ edgeId ->
      pure (wrapperFrame edgeId inner)
    ALetScope inner _ edgeId ->
      pure (wrapperFrame edgeId inner)
    AUnfold inner _ edgeId ->
      pure (wrapperFrame edgeId inner)
  where
    emptyFrame = LocalGammaFrame Nothing [] []

    wrapperFrame edgeId inner =
      LocalGammaFrame Nothing [(edgeId, inner)] [inner]

-- | Pending proof that one complete set of instantiation edges is constructed
-- and closed below an enclosing root.  Every edge for the shared exterior is
-- kept with the exact source constructor and consumer identity, so the local
-- elaborator can discharge the whole obligation or none of it.
data LocalGammaClosure = LocalGammaClosure
  { lgcEdgeIds :: !(NonEmpty EdgeId),
    -- | Exact source edges covered immediately by an application frame.
    -- These edges are occurrence provenance, not a second ownership
    -- heuristic: 'localGammaFrame' is the sole constructor-side authority
    -- that may add them.  The subset is retained because the frozen exterior
    -- can belong to an enclosing graph scope even though Figure 15.3.5
    -- sends the direct requirement through the application's edge-local
    -- construction lane.
    lgcDirectApplicationEdgeIds :: ![EdgeId],
    -- | Edges contributed by enclosing constructors that were proved
    -- result-transparent while the annotated source tree was available.
    -- They are constructed by the direct application owner above, but remain
    -- distinct from its immediate occurrence edges.
    lgcForwardedResultEdgeIds :: ![EdgeId],
    lgcExteriorNode :: !NodeId,
    lgcConsumerIdentity :: !TypeBinderIdentity,
    lgcOwner :: !LocalGammaOwner,
    -- The pending local-constructor view of the packet consumer.  Its
    -- substitution proves the exact exterior-to-binder route, while its
    -- unbounded consumer slot proves that no stale S'(operated) presentation
    -- is being treated as the emitted bound.  The checked child materializes
    -- that bound later.  'Nothing' denotes an ordinary syntactic owner that
    -- must construct its Gamma through the normal local planner.
    lgcOwnerPendingScheme :: !(Maybe SchemeInfo)
  }
  deriving (Eq, Show)

-- | Validate the exact source occurrence proof for an application's
-- edge-local Figure 15.3.5 lane.  This does not establish ordinary Gamma_g
-- scope ownership: consumers may use it only where direct edge-local
-- requirements are expected.
directApplicationClosureOwnsEdges
  :: LocalGammaClosure
  -> NonEmpty EdgeId
  -> Bool
directApplicationClosureOwnsEdges closure expectedEdges =
    lgoConstructor (lgcOwner closure) == LocalApplicationGamma
    && not (IntSet.null directEdgeKeys)
    && coveredEdgeKeys == closureEdgeKeys
    && IntSet.null
      (IntSet.intersection directEdgeKeys forwardedEdgeKeys)
    && ( directEdgeKeys == expectedEdgeKeys
          || coveredEdgeKeys == expectedEdgeKeys
       )
  where
    edgeKeySet = IntSet.fromList . map getEdgeId
    directEdgeKeys = edgeKeySet (lgcDirectApplicationEdgeIds closure)
    forwardedEdgeKeys = edgeKeySet (lgcForwardedResultEdgeIds closure)
    coveredEdgeKeys = IntSet.union directEdgeKeys forwardedEdgeKeys
    expectedEdgeKeys = edgeKeySet (NonEmpty.toList expectedEdges)
    closureEdgeKeys = edgeKeySet (NonEmpty.toList (lgcEdgeIds closure))

-- | Select only the retained local-Gamma closures that are genuine
-- descendants of the construction currently being elaborated and whose exact
-- exterior occurrence is used by its result.  The annotated source tree proves
-- lexical ownership; raw graph reachability proves occurrence relevance.
-- Neither proof is reconstructed through a canonical representative.
retainedDescendantGammaClosures
  :: (EdgeId -> NodeId -> Either ElabError NodeRef)
  -> LocalGammaOwner
  -> IntSet.IntSet
  -> IntMap.IntMap LocalGammaClosure
  -> AnnExpr
  -> Either ElabError [LocalGammaClosure]
retainedDescendantGammaClosures scopeForBoundary currentOwner reachableNodes closures ann = do
  sourceOwners <- localGammaOwnersIn ann
  foldM
    (selectClosure sourceOwners)
    []
    (foldr insertDistinctClosure [] (IntMap.elems closures))
  where
    localGammaOwnersIn expr = do
      frame <- localGammaFrame scopeForBoundary expr
      childOwners <- concat <$> traverse localGammaOwnersIn (lgfChildren frame)
      pure (maybeToList (lgfOwner frame) ++ childOwners)

    selectClosure sourceOwners selected closure
      | closureOwner == currentOwner = pure selected
      | not
          ( IntSet.member
              (getNodeId (lgcExteriorNode closure))
              reachableNodes
          ) =
          pure selected
      | ownerOccurrences == 0 = pure selected
      | ownerOccurrences /= 1 =
          closureFailure
            closure
            ( "retained owner occurs "
                ++ show ownerOccurrences
                ++ " times in the current source subtree"
            )
      | storedEdgeKeys /= closureEdgeKeys =
          closureFailure
            closure
            "retained closure is not keyed by exactly its complete edge set"
      | IntSet.size closureEdgeKeys /= NonEmpty.length (lgcEdgeIds closure) =
          closureFailure
            closure
            "retained closure repeats an edge occurrence"
      | otherwise = pure (selected ++ [closure])
      where
        closureOwner = lgcOwner closure
        ownerOccurrences = length (filter (== closureOwner) sourceOwners)
        closureEdgeKeys = edgeKeySet (lgcEdgeIds closure)
        storedEdgeKeys =
          IntSet.fromList
            [ edgeKey
            | (edgeKey, storedClosure) <- IntMap.toList closures
            , storedClosure == closure
            ]

    insertDistinctClosure closure selected
      | closure `elem` selected = selected
      | otherwise = closure : selected

    edgeKeySet =
      IntSet.fromList . map getEdgeId . NonEmpty.toList

    closureFailure closure detail =
      Left
        ( ValidationFailed
            [ "invalid retained descendant Gamma closure"
            , "  detail: " ++ detail
            , "  current owner: " ++ show currentOwner
            , "  closure: " ++ show closure
            ]
        )

-- | Turn already-constructed descendant Gamma declarations into exact ambient
-- capabilities for an enclosing construction.  The regenerated requirement
-- supplies the declaration's checked bound, while retained closures prove its
-- source occurrences, complete edge partition, exterior, and consumer
-- identity.  Re-generation may coalesce adjacent closure edge groups for one
-- exterior; that is valid only when the retained groups are disjoint and their
-- union is the regenerated edge set.  The enclosing construction retains its
-- own required binders unchanged.
inheritDescendantGammaRequirements
  :: [LocalGammaClosure]
  -> [RequiredGammaBinder]
  -> GeneralizationRequirements
  -> Either ElabError GeneralizationRequirements
inheritDescendantGammaRequirements closures regenerated requirements = do
  unless
    (length closures == length (foldr insertDistinctClosure [] closures))
    ( inheritanceFailure
        "the same descendant closure was selected more than once"
        Nothing
        Nothing
    )
  matched <- traverse matchRequirement closures
  unless
    (all (`elem` matched) regenerated)
    ( inheritanceFailure
        "a regenerated descendant requirement has no retained closure evidence"
        Nothing
        Nothing
    )
  let matchedGroups =
        [ ( requirement
          , [ closure
            | (closure, matchedRequirement) <- zip closures matched
            , matchedRequirement == requirement
            ]
          )
        | requirement <- regenerated
        ]
  mapM_ validateMatchedGroup matchedGroups
  foldM inheritMatched requirements matchedGroups
  where
    matchRequirement closure =
      case filter (requirementMatchesClosure closure) regenerated of
        [requirement] -> pure requirement
        matches ->
          inheritanceFailure
            ( "retained closure matched "
                ++ show (length matches)
                ++ " regenerated requirements, expected exactly one"
            )
            (Just closure)
            Nothing

    validateMatchedGroup (requirement, matchedClosures) =
      case matchedClosures of
        [] ->
          inheritanceFailure
            "a regenerated descendant requirement has an empty closure group"
            Nothing
            (Just requirement)
        firstClosure : _ ->
          unless
            ( closureEdges == requirementEdges
                && totalClosureEdgeCount == IntSet.size closureEdges
            )
            ( inheritanceFailure
                "coalesced descendant closures do not form an exact disjoint edge partition"
                (Just firstClosure)
                (Just requirement)
            )
          where
            edgeSets = map (edgeKeySet . lgcEdgeIds) matchedClosures
            closureEdges = IntSet.unions edgeSets
            requirementEdges = edgeKeySet (rgbEdgeIds requirement)
            totalClosureEdgeCount = sum (map IntSet.size edgeSets)

    inheritMatched inherited (requirement, matchedClosures) =
      case matchedClosures of
        closure : _ -> inheritOne inherited (closure, requirement)
        [] ->
          inheritanceFailure
            "internal empty descendant closure group"
            Nothing
            (Just requirement)

    inheritOne inherited (closure, requirement)
      | rgbPlacement requirement /= RequiredGammaAtCurrentScope =
          inheritanceFailure
            "a descendant declaration was not regenerated at its owning scope"
            (Just closure)
            (Just requirement)
      | not (null overlappingCurrentRequirements) =
          inheritanceFailure
            "an enclosing construction overlaps a descendant Gamma edge set"
            (Just closure)
            (Just requirement)
      | not (null sameExteriorCurrentRequirements)
      , all
          ( equivalentTypes (rgbOperatedType requirement)
              . rgbOperatedType
          )
          sameExteriorCurrentRequirements =
          -- Disjoint descendant/current source occurrences can coalesce at
          -- the enclosing owner when they require the exact same declaration.
          -- Keep the current Gamma as the unique emitter; the descendant
          -- closure then consumes that declaration from ambient scope rather
          -- than re-emitting an identical binder inside the child.
          pure inherited
      | not (null sameExteriorCurrentRequirements)
      , rgbOperatedType requirement == TBottom
      , all
          ((/= TBottom) . rgbOperatedType)
          sameExteriorCurrentRequirements =
          -- The descendant has only the provisional unbounded slot produced
          -- before its enclosing result was known.  A current requirement
          -- for the same exterior carries the positive checked bound and is
          -- installed before the descendant term is elaborated.  Keep that
          -- exact current declaration; the descendant then consumes its
          -- Bottom slot as ambient instead of publishing a competing Gamma
          -- for the same graph identity.
          pure inherited
      | not (null sameExteriorCurrentRequirements) =
          inheritanceFailure
            "an enclosing construction tried to re-own a descendant Gamma declaration"
            (Just closure)
            (Just requirement)
      | not (null conflictingAmbientAuthorities) =
          inheritanceFailure
            "ambient Gamma declarations disagree on the descendant identity or bound"
            (Just closure)
            (Just requirement)
      | otherwise =
          case
              IntMap.lookup
                exteriorKey
                (grAmbientGammaAuthorities inherited)
            of
              Just existing
                | not (sameAmbientAuthority existing authority) ->
                    inheritanceFailure
                      "the descendant exterior already has a different ambient declaration"
                      (Just closure)
                      (Just requirement)
              _ ->
                pure
                  inherited
                    { grAmbientBinderRefs =
                        insertAmbientRef
                          exactRef
                          (grAmbientBinderRefs inherited)
                    , grAmbientGammaAuthorities =
                        IntMap.insert
                          exteriorKey
                          authority
                          (grAmbientGammaAuthorities inherited)
                    , grLocallyClosedGammaNodes =
                        IntSet.insert
                          exteriorKey
                          (grLocallyClosedGammaNodes inherited)
                    }
      where
        exteriorKey = getNodeId (rgbExteriorNode requirement)
        exactRef =
          typeBinderRefFromIdentity
            (lgcConsumerIdentity closure)
            (typeBinderIdentityStableName (lgcConsumerIdentity closure))
        authority =
          AmbientGammaAuthority
            { agaExactRef = exactRef
            , agaBound = rgbOperatedType requirement
            }
        requirementEdgeKeys = edgeKeySet (rgbEdgeIds requirement)
        sameExteriorCurrentRequirements =
          [ current
          | current <- grRequiredGammaBinders inherited
          , rgbExteriorNode current == rgbExteriorNode requirement
          ]
        overlappingCurrentRequirements =
          [ current
          | current <- grRequiredGammaBinders inherited
          , not
              ( IntSet.null
                  ( IntSet.intersection
                      requirementEdgeKeys
                      (edgeKeySet (rgbEdgeIds current))
                  )
              )
          ]
        conflictingAmbientAuthorities =
          [ existing
          | existing <- IntMap.elems (grAmbientGammaAuthorities inherited)
          , typeBinderRefsSameIdentity (agaExactRef existing) exactRef
          , not (sameAmbientAuthority existing authority)
          ]

    requirementMatchesClosure closure requirement =
      lgcExteriorNode closure == rgbExteriorNode requirement
        && lgcConsumerIdentity closure
          == typeBinderIdentityFromNode (rgbExteriorNode requirement)
        && edgeKeySet (lgcEdgeIds closure)
          `IntSet.isSubsetOf` edgeKeySet (rgbEdgeIds requirement)

    insertAmbientRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    sameAmbientAuthority left right =
      typeBinderRefsSameIdentity
        (agaExactRef left)
        (agaExactRef right)
        && equivalentTypes (agaBound left) (agaBound right)

    equivalentTypes left right =
      alphaEqType left right || churchAwareEqType left right

    insertDistinctClosure closure selected
      | closure `elem` selected = selected
      | otherwise = closure : selected

    edgeKeySet =
      IntSet.fromList . map getEdgeId . NonEmpty.toList

    inheritanceFailure
      :: String
      -> Maybe LocalGammaClosure
      -> Maybe RequiredGammaBinder
      -> Either ElabError a
    inheritanceFailure detail mbClosure mbRequirement =
      Left
        ( ValidationFailed
            ( [ "invalid inherited descendant Gamma authority"
              , "  detail: " ++ detail
              ]
                ++ maybe [] (pure . ("  closure: " ++) . show) mbClosure
                ++ maybe [] (pure . ("  requirement: " ++) . show) mbRequirement
                ++ [ "  regenerated requirements: " ++ show regenerated
                   , "  enclosing requirements: " ++ show requirements
                   ]
            )
        )

-- | Whether the delayed Hyp result is part of the compiler-exact source ABI
-- or is introduced solely by the packet construction.  Source-owned results
-- must exist while the exact boundary is checked; packet-only results are
-- applied afterwards so exact validation observes the operated source type.
data CompilerExactResultStage
  = CompleteBeforeCompilerExact
  | CompleteAfterCompilerExact
  deriving (Eq, Show)

-- | Delayed result ownership at a compiler-exact boundary.  The packet-owned
-- form deliberately carries only one reference: its semantic completion
-- identity is necessarily the packet result identity.  A distinct completion
-- identity can be recorded only by the source-owned constructor, whose source
-- binder is selected from the exact annotation's result position during
-- preparation.  Constructors remain private so callers cannot assemble an
-- illegal stage/reference combination.
data CompilerExactSubtermResult
  = CompilerExactSourceSubtermResult
      !EdgeId
      !TypeBinderRef
      !TypeBinderRef
  | CompilerExactPacketSubtermResult
      !EdgeId
      !TypeBinderRef
  -- A deeper packet in the same administrative lambda spine owns the exact
  -- result action.  Enclosing packets retain this marker so they neither
  -- publish nor consume the shared result binder locally.
  | CompilerExactDescendantSubtermResult
      !EdgeId
      !TypeBinderRef
  | CompilerExactEnclosingSubtermResult
      !EdgeId
      !TypeBinderRef
  deriving (Eq, Show)

-- | Lambda-body generalization packets keyed by the resolved identity of the
-- lambda that owns the body boundary.  Node ids are not suitable keys here:
-- canonicalization may merge nodes from sibling subtrees even though their
-- lexical ownership remains distinct.
-- | The edge-local consumer of a prepared packet.  A consumer-only packet is
-- placed under a specific enclosing local Gamma owner while a Gamma packet
-- owns its consumer at the packet's own edge.  Keeping those alternatives in
-- the authority prevents elaboration from reconstructing placement from a
-- missing binder or from the shape of a completed term.
data SubtermConsumerAuthority
  = EnclosingGammaConsumerAuthority
      !EdgeId
      !TypeBinderIdentity
      !LocalGammaOwner
  | TopologyConsumerAuthority !IdentityTopologyConsumerAuthority
  | RootGammaConsumerAuthority
      !EdgeId
      !TypeBinderIdentity
  | PacketGammaConsumerAuthority
      !EdgeId
      !TypeBinderIdentity
  deriving (Eq, Show)

-- | Exact lookup key for one prepared packet consumer.  Consumer identity
-- alone is insufficient: distinct instantiation edges can legitimately meet
-- at the same Γ exterior while only one packet is open in the current
-- construction.
data SubtermConsumerKey = SubtermConsumerKey
  !EdgeId
  !TypeBinderIdentity
  deriving (Eq, Ord, Show)

subtermConsumerKey :: EdgeId -> TypeBinderIdentity -> SubtermConsumerKey
subtermConsumerKey = SubtermConsumerKey

subtermConsumerAuthorityKey
  :: SubtermConsumerAuthority
  -> SubtermConsumerKey
subtermConsumerAuthorityKey authority =
  SubtermConsumerKey
    (scaEdgeId authority)
    (scaConsumerIdentity authority)

scaEdgeId :: SubtermConsumerAuthority -> EdgeId
scaEdgeId authority =
  case authority of
    EnclosingGammaConsumerAuthority edgeId _ _ -> edgeId
    TopologyConsumerAuthority topologyAuthority ->
      itcaEdgeId topologyAuthority
    RootGammaConsumerAuthority edgeId _ -> edgeId
    PacketGammaConsumerAuthority edgeId _ -> edgeId

scaConsumerIdentity :: SubtermConsumerAuthority -> TypeBinderIdentity
scaConsumerIdentity authority =
  case authority of
    EnclosingGammaConsumerAuthority _ identity _ -> identity
    TopologyConsumerAuthority topologyAuthority ->
      typeBinderIdentityFromNode
        (itcaFrozenResultRoot topologyAuthority)
    RootGammaConsumerAuthority _ identity -> identity
    PacketGammaConsumerAuthority _ identity -> identity

subtermConsumerAuthorityEnclosingOwner
  :: SubtermConsumerAuthority
  -> Maybe LocalGammaOwner
subtermConsumerAuthorityEnclosingOwner authority =
  case authority of
    EnclosingGammaConsumerAuthority _ _ owner -> Just owner
    TopologyConsumerAuthority topologyAuthority ->
      Just (itcaOwner topologyAuthority)
    RootGammaConsumerAuthority{} -> Nothing
    PacketGammaConsumerAuthority{} -> Nothing

subtermConsumerAuthorityIsTopology :: SubtermConsumerAuthority -> Bool
subtermConsumerAuthorityIsTopology authority =
  case authority of
    TopologyConsumerAuthority{} -> True
    _ -> False

subtermConsumerAuthorityIsRootGamma :: SubtermConsumerAuthority -> Bool
subtermConsumerAuthorityIsRootGamma authority =
  case authority of
    RootGammaConsumerAuthority{} -> True
    _ -> False

data PreparedSubtermGeneralization = PreparedSubtermGeneralization
  { psgConsumerAuthority :: Maybe SubtermConsumerAuthority
  -- An administrative source wrapper can place this packet under an
  -- enclosing consumer while the wrapped lambda still owns a distinct
  -- identity-topology result.  Keep that local result authority separate:
  -- overwriting it with the enclosing consumer loses the exact binder that
  -- the lambda body must construct.
  , psgLocalResultAuthority :: Maybe SubtermConsumerAuthority
  -- A topology result with no occurrence in the completed packet is
  -- administrative.  Retain the exact packet proof that discharged it so
  -- later shape-preserving rewrites can revalidate that decision.
  , psgLocalResultDischarge :: Maybe ClosedConsumerDischarge
  -- The completed packet closes every quantifier owned by this subterm.  An
  -- enclosing root RaiseMerge consumes this complete scheme as S'(operated).
  , psgSchemeInfo :: SchemeInfo
  -- The local constructor fills a packet-owned consumer bound from the
  -- recursively checked child.  Root planning still consumes 'psgSchemeInfo',
  -- whose graph-operated bound is complete; this view makes the pending local
  -- slot explicit instead of erasing information globally or overwriting a
  -- completed bound during elaboration.
  , psgConsumerConstructionSchemeInfo :: SchemeInfo
  -- The operated view remains open under those packet-owned quantifiers.  It
  -- is used only while constructing the packet itself and its exact
  -- endpoints, where the completed packet already supplies that scope.
  , psgOperatedSchemeInfo :: SchemeInfo
  -- The complete S'(operated) scheme consumed by an enclosing Gamma binder.
  -- Unlike the packet-local operated view, this value is materialized at
  -- compiler-exact construction arguments while the packet is prepared.  All
  -- later Gamma construction therefore observes the same bound; elaboration
  -- must not specialize an already-published environment after the fact.
  , psgGammaBoundScheme :: ElabScheme
  -- Source-tree authority for an otherwise-erased parameter of an
  -- administrative nested lambda.  This declaration is usable only while
  -- the completed construction remains the published enclosing Gamma bound;
  -- an exact specialization to the operated endpoint supersedes it.
  , psgSourceLambdaParameter
      :: Maybe SourceLambdaParameter
  , psgCompilerExactResult :: Maybe CompilerExactSubtermResult
  -- The exact source binder and the graph binder are both known while the
  -- packet's operated scheme is constructed.  Retain that quotient so source
  -- annotations owned by the packet are emitted directly in construction
  -- Gamma's identity domain instead of being repaired after term creation.
  , psgCompilerExactBinderRenames :: [(TypeBinderRef, TypeBinderRef)]
  -- Source-owned expected types can also require a source-to-graph quotient
  -- solely inside the packet construction.  Keep that route distinct from
  -- compiler-exact outward publication: the latter may instantiate a leading
  -- binder, while this route exists precisely to retain it locally.
  , psgConstructionBinderRenames :: [(TypeBinderRef, TypeBinderRef)]
  -- Reification is the last phase that owns both the live rigid node and its
  -- frozen base origin.  Preserve that capability independently of the
  -- packet's ordinary source/construction renames.
  , psgInheritedGammaRoutes :: Reify.InheritedGammaRoutes
  -- Exact original-to-copy routes already used while descendant packets were
  -- placed into this packet's complete Gamma bound.  The outer key is the
  -- declaration whose bound received those copies; routes for two distinct
  -- Gamma declarations must never be conflated.
  , psgPlacedCopiedBinderRefs ::
      Map.Map
        TypeBinderIdentity
        [(TypeBinderIdentity, TypeBinderRef)]
  , psgCopiedBinderRefs :: Map.Map TypeBinderIdentity TypeBinderRef
  , psgGammaAuthority :: Maybe GammaPacketAuthority
  -- A topology or enclosing consumer can become administrative when its
  -- complete packet is already a closed type and the consumer identity occurs
  -- nowhere in that packet. Record that fact while the complete and operated
  -- schemes are still owned by packet preparation. Enclosing placement may
  -- consume this certificate instead of manufacturing a vacuous binder.
  , psgClosedConsumerDischarge
      :: Maybe ClosedConsumerDischarge
  -- A source-owned exact endpoint may consume this packet's completed
  -- construction without retaining the intermediate enclosing Gamma
  -- declaration in a projected operated scheme.  Keep the checked leading-
  -- spine computation with the packet so that omission is certified while
  -- both endpoints are still available, rather than inferred later from a
  -- missing binder.
  , psgExactConsumerSpecialization
      :: Maybe ExactConsumerSpecialization
  -- A paired source/canonical owner walk can determine the exact endpoint of
  -- an enclosing lambda before its recursively elaborated child is checked.
  -- Retain that construction authority when the packet's frozen operated
  -- view places a returned result declaration at a different lexical depth.
  -- The child must later validate this endpoint with its owner-final
  -- construction; the certificate never derives an endpoint from a checked
  -- final type.
  , psgSourceOwnerConsumerCompletion
      :: Maybe SourceOwnerConsumerCompletion
  -- A paired source/canonical owner walk may also determine the complete
  -- result endpoint published by an enclosing owner.  This is distinct from
  -- the body endpoint above: root-edge grouping uses it only to select an
  -- already-present requirement with that exact endpoint.
  , psgSourceOwnerFinalConsumerCompletion
      :: Maybe SourceOwnerConsumerCompletion
  -- A source/canonical lockstep walk can prove that an opaque nested-lambda
  -- carrier has already been replaced by its completed descendant packet.
  -- Retain that exact endpoint so term elaboration reuses the construction
  -- instead of generalizing the graph's stale pre-composition carrier again.
  , psgOpaqueResultConstruction
      :: Maybe OpaqueResultConstruction
  }
  deriving (Eq, Show)

data SourceLambdaParameter =
  SourceLambdaParameter
    { slpLambdaNode :: !NodeId
    , slpParameterNode :: !NodeId
    , slpParameterType :: !ElabType
    , slpLocalBinderRef :: !(Maybe TypeBinderRef)
    }
  deriving (Eq, Show)

data ClosedConsumerDischarge =
  ClosedConsumerDischarge
    { ccdAuthority :: !SubtermConsumerAuthority
    , ccdSchemeInfo :: !SchemeInfo
    -- The exact pre-discharge construction. For a primary Gamma consumer this
    -- is its published Gamma bound; for a distinct local topology result it
    -- can be the packet's complete source-lambda construction.
    , ccdConstructionScheme :: !ElabScheme
    }
  deriving (Eq, Show)

data ExactConsumerSpecialization =
  ExactConsumerSpecialization
    { ecsAuthority :: !SubtermConsumerAuthority
    , ecsExpectedEndpoint :: !ElabType
    , ecsPlan :: !ExactBinderSpinePlan
    }
  deriving (Eq, Show)

data OpaqueResultConstruction =
  OpaqueResultConstruction
    { orcCarrierRefs :: ![TypeBinderRef]
    , orcConstructedType :: !ElabType
    , orcCompletionAuthority :: !OpaqueResultCompletionAuthority
    }
  deriving (Eq, Show)

data OpaqueResultCompletionAuthority
  = OpaqueResultBinderSpineCompletion !ExactBinderSpinePlan
  | OpaqueResultSourceLambdaCompletion
      ![(NodeId, ElabType)]
  deriving (Eq, Show)

type SubtermGeneralizations = Map.Map ResolvedTermIdentityKey PreparedSubtermGeneralization

-- | The legal ownership shapes for a prepared subterm packet. Keeping
-- the enclosing consumer and packet-owned Γ in one sum prevents callers
-- from collapsing two independently owned edges into one authority.
data SubtermPacketPlacement
  = EnclosingConsumerPacket
      !TypeBinderIdentity
      !EdgeId
      !LocalGammaOwner
  | EnclosingConsumerGammaPacket
      !TypeBinderIdentity
      !EdgeId
      !LocalGammaOwner
      !GammaPacketAuthority
  | TopologyConsumerPacket !IdentityTopologyConsumerAuthority
  | TopologyConsumerGammaPacket
      !IdentityTopologyConsumerAuthority
      !GammaPacketAuthority
  | RootConsumerPacket
      !TypeBinderIdentity
      !EdgeId
  | RootConsumerGammaPacket
      !TypeBinderIdentity
      !EdgeId
      !GammaPacketAuthority
  | GammaPacket !GammaPacketAuthority
  | WithLocalTopologyResult
      !SubtermPacketPlacement
      !IdentityTopologyConsumerAuthority
  | DirectPacket
  deriving (Eq, Show)

-- | Whether the lexical path from a queried root to its packet owner preserves
-- the result boundary.  The annotated source tree decides this once, while it
-- is authoritative; later phases cannot rebuild the answer from reified types
-- or from the incidental shape of the completed xMLF term.
data SubtermResultPath
  = TransparentSubtermResultPath
  | OpaqueSubtermResultPath
  deriving (Eq, Show)

-- | Structural proof that one prepared packet belongs at a particular lambda.
-- The path authority records exactly the distinction consumed by closure:
-- let/annotation wrappers preserve the result boundary, while crossing a
-- lambda or unfold does not.
data SubtermResultOwnership = SubtermResultOwnership
  { sroOwnerKey :: !ResolvedTermIdentityKey
  , sroLambdaNode :: !NodeId
  , sroLambdaScope :: !GenNodeId
  , sroLambdaEdge :: !EdgeId
  , sroLambdaArity :: !Int
  , sroPath :: !SubtermResultPath
  , sroPacket :: !PreparedSubtermGeneralization
  }
  deriving (Eq, Show)

data SourceOwnerConsumerCompletion =
  SourceOwnerConsumerCompletion
    { soccAuthority :: !SubtermConsumerAuthority
    , soccOwner :: !LocalGammaOwner
    , soccFrozenOperatedType :: !ElabType
    , soccExpectedEndpoint :: !ElabType
    }
  deriving (Eq, Show)

-- | Positive construction evidence for the result of a direct source-lambda
-- application.  A packet-local Gamma result and an enclosing-consumer
-- Eq-Free result have different consequences for the lambda child: only the
-- latter proves the complete returned-lambda parameter spine before that
-- child is checked.  Keep that distinction in the value instead of asking the
-- application elaborator to rediscover it from two equal-looking types.
data DirectLambdaApplicationResultConstruction
  = DirectLambdaPacketGammaResultConstruction !ElabType
  | DirectLambdaEnclosingConsumerEqFreeResultConstruction !ElabType
  deriving (Eq, Show)

directLambdaApplicationResultConstructionType
  :: DirectLambdaApplicationResultConstruction
  -> ElabType
directLambdaApplicationResultConstructionType construction =
  case construction of
    DirectLambdaPacketGammaResultConstruction resultTy -> resultTy
    DirectLambdaEnclosingConsumerEqFreeResultConstruction resultTy -> resultTy

-- | Construction-time authority for Figure 15.3.4's root
-- @RaiseMerge(r,m)@ case.  Both nodes stay in the frozen source-domain
-- identity space: final union-find representatives are deliberately absent.
data RootRaiseMergeAuthority = RootRaiseMergeAuthority
  { rrmaOperatedRoot :: !NodeId,
    rrmaExterior :: !NodeId,
    rrmaResultRoot :: !NodeId
  }
  deriving (Eq, Show)

-- | Read the exact root/exterior authority already validated by witness
-- normalization.  A root operation is usable for construction only when its
-- trace proves that the operated node is the frozen source root, the other
-- endpoint is outside that source interior, and no binder replay is involved.
rootRaiseMergeAuthorityFor :: EdgeArtifacts -> EdgeId -> Either ElabError (Maybe RootRaiseMergeAuthority)
rootRaiseMergeAuthorityFor artifacts eid =
  case lookupEdgeArtifact eid artifacts of
    Just artifact ->
      let witness = edgeArtifactWitness artifact
          traceInfo = edgeArtifactTrace artifact
       in case validAuthorities traceInfo (getInstanceOps (ewWitness witness)) of
            [] -> Right Nothing
            [authority] -> Right (Just authority)
            authorities ->
              Left
                ( ValidationFailed
                    [ "edge carries multiple root RaiseMerge authorities: " ++ show eid,
                      "  authorities: " ++ show authorities
                    ]
                )
    Nothing -> Right Nothing
  where
    validAuthorities traceInfo = go
      where
        go ops =
          case ops of
            OpWeaken weakened : OpRaiseMerge operated exterior : rest
              | weakened == operated,
                rootWeakenRaiseMergeTraceAuthority operated exterior traceInfo ->
                  -- The pair is the producer certificate for Lemma 11.5.3's
                  -- rigid-root lane.  Figure 15.3.4 translates it to the
                  -- identity, so it owns no exterior Gamma binder.
                  go rest
            OpRaiseMerge operated exterior : rest
              | exactProof traceInfo operated exterior ->
                  RootRaiseMergeAuthority operated exterior (etResultRoot traceInfo)
                    : go rest
            _ : rest -> go rest
            [] -> []

    exactProof traceInfo operated exterior =
      rootRaiseMergeTraceAuthority operated exterior traceInfo

rootRaiseMergeExteriorIdentityFor :: EdgeArtifacts -> EdgeId -> Either ElabError (Maybe TypeBinderIdentity)
rootRaiseMergeExteriorIdentityFor artifacts eid =
  fmap (typeBinderIdentityFromNode . rrmaExterior) <$> rootRaiseMergeAuthorityFor artifacts eid

-- | Decide which lexical Gamma owns the Hyp introduced by a root
-- @RaiseMerge(r,m)@.  In the thesis core, named exteriors are flexibly bound
-- directly on a gen node.  The repository's structural-recursive extension
-- can retain @m@ below a flexible @mu@/alias shell; Omega still emits the same
-- root Hyp, so the nearest gen reached through an entirely flexible path owns
-- its construction binder.  Crossing a rigid edge or another gen boundary is
-- never accepted.
rootRaiseMergeExteriorOwnedByScope
  :: GaBindParents p
  -> NodeRef
  -> NodeId
  -> Bool
rootRaiseMergeExteriorOwnedByScope ga ownerScope exterior =
  go IntSet.empty (typeRef exterior)
  where
    bindParents = gaBindParentsBase ga

    go seen child
      | IntSet.member childKey seen = False
      | otherwise =
          case IntMap.lookup childKey bindParents of
            Just (parent, BindFlex)
              | parent == ownerScope -> True
              | TypeRef {} <- parent ->
                  go (IntSet.insert childKey seen) parent
            _ -> False
      where
        childKey = nodeRefKey child

-- | Validate the exact exterior binder required by a terminal root
-- @RaiseMerge@.  Figure 15.3.4 and Lemmas 15.3.10--11 require planning to
-- construct @exterior > S'(operated)@ directly.  This boundary validates that
-- declaration and constructs the owned result endpoint before xMLF emission;
-- it never infers a binder from the finished term.
prepareRootRaiseMergeScheme ::
  EdgeArtifacts ->
  AnnExpr ->
  GeneralizationRequirements ->
  SchemeInfo ->
  Either ElabError SchemeInfo
prepareRootRaiseMergeScheme artifacts expr requirements schemeInfo =
  case rootLambdaBodyEdge expr of
    Nothing -> Right schemeInfo
    Just edgeId ->
      prepareRootRaiseMergeSchemeWithPlacement
        RaiseMergeUnderLambdaResult
        artifacts
        edgeId
        requirements
        schemeInfo

-- | Publish the exact result-to-exterior bridge already constructed by the
-- packet that owns one root @RaiseMerge@ edge.  Root generalization may reify
-- only the exterior representative after the packet has closed the source
-- result.  The packet still owns both source keys, so join those keys before
-- validating the root scheme instead of reconstructing the missing route from
-- the finished type.
publishRootRaiseMergePacketResultRoute
  :: EdgeId
  -> RootRaiseMergeAuthority
  -> SubtermGeneralizations
  -> SchemeInfo
  -> Either ElabError SchemeInfo
publishRootRaiseMergePacketResultRoute edgeId authority packets rootInfo = do
  mbPacket <-
    subtermGeneralizationForConsumerAtEdge
      edgeId
      exteriorIdentity
      packets
  case mbPacket of
    Nothing -> pure rootInfo
    Just packet -> do
      unless
        ( subtermGeneralizationOwnsGammaForEdge edgeId packet
            || maybe
              False
              ((== edgeId) . scaEdgeId)
              (psgConsumerAuthority packet)
        )
        (routeFailure "selected packet does not own the exact RaiseMerge edge" packet)
      packetExteriorRef <-
        requireRoute
          "packet exterior"
          exteriorKey
          (siSubstRefs (psgSchemeInfo packet))
      packetResultRef <-
        requireRoute
          "packet result"
          resultKey
          (siSubstRefs (psgSchemeInfo packet))
      (rootInfoWithExterior, rootExteriorRef) <-
        case IntMap.lookup exteriorKey (siSubstRefs rootInfo) of
          Just ref -> pure (rootInfo, ref)
          Nothing ->
            case
                find
                  (typeBinderRefsSameIdentity packetExteriorRef)
                  rootRepresentedRefs
              of
                -- The owner-final result can place this declaration below an
                -- outer arrow.  It is then outside SchemeInfo's leading
                -- binder spine, but the packet still carries the exact frozen
                -- graph-key route to that already-present declaration.
                -- Publish that positive route rather than requiring root
                -- generalization to manufacture a second outer binder.
                Just representedRef ->
                  pure
                    ( publishExteriorRoute representedRef rootInfo
                    , representedRef
                    )
                Nothing -> installPacketExteriorBinder packet packetExteriorRef
      -- The packet and root schemes are distinct construction domains.  The
      -- frozen exterior node is the transport key between them, so their
      -- outward binder identities may legitimately differ after a root
      -- quotient.  What the packet must prove locally is that its exact
      -- result key was joined to its exact exterior key.  The root's exterior
      -- route is then the authoritative outward identity installed below.
      unless
        (typeBinderRefsSameIdentity packetExteriorRef packetResultRef)
        (routeFailure "packet result does not join its exterior route" packet)
      case IntMap.lookup resultKey (siSubstRefs rootInfoWithExterior) of
        Nothing ->
          pure
            rootInfoWithExterior
              { siSubstRefs =
                  IntMap.insert
                    resultKey
                    rootExteriorRef
                    (siSubstRefs rootInfoWithExterior)
              }
        Just existing
          | typeBinderRefsSameIdentity existing rootExteriorRef ->
              pure rootInfoWithExterior
          | otherwise ->
              routeFailure
                "root result already has a conflicting route"
                packet
  where
    exteriorKey = getNodeId (rrmaExterior authority)
    resultKey = getNodeId (rrmaResultRoot authority)
    exteriorIdentity =
      typeBinderIdentityFromNode (rrmaExterior authority)
    rootRepresentedRefs =
      let rootTy = schemeToType (siScheme rootInfo)
       in typeBinderDeclarationRefs rootTy
            ++ freeTypeVarRefsType rootTy

    publishExteriorRoute exteriorRef info =
      info
        { siSubstRefs =
            IntMap.insert exteriorKey exteriorRef (siSubstRefs info)
        }

    installPacketExteriorBinder packet exteriorRef =
      case
          filter
            (typeBinderRefsSameIdentity exteriorRef . fst)
            (schemeBinderRefs (siScheme (psgSchemeInfo packet)))
        of
          [packetBinder] -> do
            orderedScheme <-
              either
                ( \cause ->
                    routeFailure
                      ( "packet exterior binder cannot enter the root construction order: "
                          ++ cause
                      )
                      packet
                )
                pure
                ( orderSourceProjectedSchemeBinders
                    "root RaiseMerge packet publication"
                    ( mkElabSchemeWithRefs
                        (schemeBinderRefs (siScheme rootInfo) ++ [packetBinder])
                        (schemeBody (siScheme rootInfo))
                    )
                )
            let publishedInfo =
                  rebuildSchemeInfoFromRefSubst
                    rootInfo
                    orderedScheme
                    ( IntMap.insert
                        exteriorKey
                        exteriorRef
                        (siSubstRefs rootInfo)
                    )
            pure (publishedInfo, exteriorRef)
          [] ->
            routeFailure
              ( "root exterior route is absent and the packet has no matching constructed binder"
                  ++ "; packet exterior: "
                  ++ show exteriorRef
                  ++ "; root represented refs: "
                  ++ show rootRepresentedRefs
              )
              packet
          binders ->
            routeFailure
              ( "root exterior route selects multiple packet binders: "
                  ++ show binders
              )
              packet

    requireRoute label key routes =
      case IntMap.lookup key routes of
        Just ref -> pure ref
        Nothing ->
          Left
            ( ValidationFailed
                [ "root RaiseMerge packet result route is incomplete"
                , "  missing route: " ++ label
                , "  edge: " ++ show edgeId
                , "  authority: " ++ show authority
                , "  root scheme: " ++ show (siScheme rootInfo)
                , "  root routes: " ++ show (siSubstRefs rootInfo)
                ]
            )

    routeFailure
      :: String
      -> PreparedSubtermGeneralization
      -> Either ElabError a
    routeFailure detail packet =
      Left
        ( ValidationFailed
            [ "invalid root RaiseMerge packet result route"
            , "  detail: " ++ detail
            , "  edge: " ++ show edgeId
            , "  authority: " ++ show authority
            , "  root routes: " ++ show (siSubstRefs rootInfo)
            , "  packet routes: "
                ++ show (siSubstRefs (psgSchemeInfo packet))
            , "  packet consumer authority: "
                ++ show (psgConsumerAuthority packet)
            , "  packet Gamma authority: "
                ++ show (psgGammaAuthority packet)
            ]
        )

data RootRaiseMergeResultPlacement
  = RaiseMergeAtExpressionResult
  | RaiseMergeUnderLambdaResult

rootRaiseMergeAuthorityForExpression
  :: EdgeArtifacts
  -> AnnExpr
  -> Either ElabError (Maybe (EdgeId, RootRaiseMergeAuthority))
rootRaiseMergeAuthorityForExpression artifacts expr =
  case rootLambdaBodyEdge expr of
    Nothing -> Right Nothing
    Just edgeId -> fmap ((,) edgeId) <$> rootRaiseMergeAuthorityFor artifacts edgeId

rootLambdaBodyEdge :: AnnExpr -> Maybe EdgeId
rootLambdaBodyEdge ann =
  case ann of
    ALam _ _ _ _ _ edgeId _ -> Just edgeId
    AAnn inner _ _ -> rootLambdaBodyEdge inner
    AExactAnn inner _ _ _ -> rootLambdaBodyEdge inner
    ALetScope inner _ _ -> rootLambdaBodyEdge inner
    AUnfold inner _ _ -> rootLambdaBodyEdge inner
    _ -> Nothing

-- | Validate a root RaiseMerge scheme against an explicitly owned edge.
-- This is used at a Var-Abs body boundary whose syntax is an application or
-- let: the parent lambda owns the edge, so recursing through the body to infer
-- authority would cross the lexical boundary.
--
-- The matching 'RequiredGammaBinder' carries the exact @S(operated)@ expected
-- at this edge.  An omitted scheme bound denotes bottom, so it is accepted
-- only when that expected type is semantically bottom too.  Keeping the
-- requirement at this boundary prevents a caller from bypassing the binder
-- planner and presenting an unbounded binder for a non-bottom obligation.
prepareRootRaiseMergeSchemeAtEdge ::
  EdgeArtifacts ->
  EdgeId ->
  GeneralizationRequirements ->
  SchemeInfo ->
  Either ElabError SchemeInfo
prepareRootRaiseMergeSchemeAtEdge =
  prepareRootRaiseMergeSchemeWithPlacement RaiseMergeAtExpressionResult

prepareRootRaiseMergeSchemeWithPlacement ::
  RootRaiseMergeResultPlacement ->
  EdgeArtifacts ->
  EdgeId ->
  GeneralizationRequirements ->
  SchemeInfo ->
  Either ElabError SchemeInfo
prepareRootRaiseMergeSchemeWithPlacement resultPlacement artifacts edgeId requirements schemeInfo = do
  mAuthority <- rootRaiseMergeAuthorityFor artifacts edgeId
  case mAuthority of
    Nothing -> pure schemeInfo
    Just authority -> do
      validateUnnormalizedExteriorProvenance authority schemeInfo
      let normalizedSchemeInfo =
            -- Opaque source constructions can expose a leading forall spine
            -- inside 'schemeBody'.  It is still the current constructor's
            -- Gamma, and Root RaiseMerge must see those declarations before it
            -- validates the exterior or selects the lambda codomain.  Normalize
            -- that representation at the typed Scheme boundary instead of
            -- searching through the finished type after construction.
            rebuildSchemeInfoFromRefSubst
              schemeInfo
              (schemeFromType (schemeToType (siScheme schemeInfo)))
              (siSubstRefs schemeInfo)
      requirement <- requiredGammaBinderForRootRaiseMerge edgeId authority requirements
      edgeLocalSchemeInfo <-
        localizeSharedResultRoute authority requirement normalizedSchemeInfo
      validated <- validateExteriorBinder authority edgeLocalSchemeInfo
      constructExteriorResult authority requirement validated
  where
    -- Several root RaiseMerge edges can solve to one outward monomorphic
    -- result while retaining distinct exterior declarations.  The enclosing
    -- scheme publishes one stable result alias, but the packet for each edge
    -- must construct that edge's result at its own exterior.  Rebase only
    -- when the existing result route is itself backed by an explicit sibling
    -- requirement for the same result; an arbitrary mismatched substitution
    -- still reaches 'validateExteriorBinder' and fails closed.
    localizeSharedResultRoute authority requirement info = do
      exteriorRef <- lookupPreparedRef "exterior" exterior info
      resultRef <- lookupPreparedRef "result root" resultRoot info
      if typeBinderRefsSameIdentity resultRef exteriorRef
        then pure info
        else
          if
              any
                (siblingOwnsPublishedResult resultRef)
                siblingRequirements
            then
              pure
                ( rebuildSchemeInfoFromRefSubst
                    info
                    (siScheme info)
                    ( foldr
                        (\root -> IntMap.insert (getNodeId root) exteriorRef)
                        (siSubstRefs info)
                        ( resultRoot
                            : NonEmpty.toList
                              (rgbResultRoots requirement)
                        )
                    )
                )
            else pure info
      where
        exterior = rrmaExterior authority
        resultRoot = rrmaResultRoot authority
        siblingRequirements =
          [ sibling
          | sibling <- grRequiredGammaBinders requirements
          , sibling /= requirement
          , rgbPlacement sibling == rgbPlacement requirement
          , resultRoot `elem` rgbResultRoots sibling
          ]
        siblingOwnsPublishedResult publishedResultRef sibling =
          case
              IntMap.lookup
                (getNodeId (rgbExteriorNode sibling))
                (siSubstRefs info)
            of
              Just siblingRef ->
                typeBinderRefsSameIdentity siblingRef publishedResultRef
              Nothing -> False

    -- Attaching the exterior substitution quotients graph binders by design.
    -- Reject contradictory input before that construction step can erase the
    -- evidence: an explicit exterior binder cannot coexist with a route that
    -- assigns the same graph node to a different semantic identity.
    validateUnnormalizedExteriorProvenance authority info =
      case IntMap.lookup exteriorKey (siSubstRefs info) of
        Just routedRef
          | typeBinderRefIdentity routedRef /= exteriorIdentity
          , any
              ((== exteriorIdentity) . typeBinderRefIdentity . fst)
              (schemeBinderRefs (siScheme info)) ->
              Left
                ( ValidationFailed
                    [ "root RaiseMerge Gamma alias was not quotiented to S(operated)"
                    , "  exterior: " ++ show exterior
                    , "  routed ref: " ++ show routedRef
                    , "  surviving exterior identity: " ++ show exteriorIdentity
                    ]
                )
        _ -> Right ()
      where
        exterior = rrmaExterior authority
        exteriorKey = getNodeId exterior
        exteriorIdentity = typeBinderIdentityFromNode exterior

    -- Figure 15.3.5 constructs the result of the edge-owned body computation
    -- at the fresh exterior, not at S'(operated).  The edge authority already
    -- identifies that position: it is the whole packet result at a body
    -- boundary, or the codomain of the lambda that owns the body edge.  Build
    -- that position directly, then retain only binders reachable from the
    -- constructed result and their bounds.  Do not search the reified type for
    -- a shape equal to S'(operated): a lambda parameter may have that same
    -- shape (paper g g), and inner Gamma construction may already have changed
    -- the stale pre-construction codomain.
    constructExteriorResult authority _requirement info = do
      exteriorRef <- lookupPreparedRef "exterior" exterior info
      let expectedExteriorRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode exterior)
              (typeBinderRefName exteriorRef)
      -- Section 15.6.2 can quotient a variable-valued requirement directly
      -- to an existing lexical binder.  That lane has no fresh exterior
      -- constructor and was fully validated above.
      if not (typeBinderRefsSameIdentity exteriorRef expectedExteriorRef)
        then pure info
        else do
          let scheme = siScheme info
              binders = schemeBinderRefs scheme
              (endpoint, rebuildBody) =
                case resultPlacement of
                  RaiseMergeAtExpressionResult ->
                    (schemeBody scheme, id)
                  RaiseMergeUnderLambdaResult ->
                    case schemeBody scheme of
                      TArrow domain codomain ->
                        (codomain, TArrow domain)
                      body ->
                        ( body
                        , const body
                        )
              endpointAlreadyConstructed =
                case endpoint of
                  TVarRef ref -> typeBinderRefsSameIdentity ref exteriorRef
                  _ -> False
          case resultPlacement of
            RaiseMergeUnderLambdaResult
              | not (isArrowBody (schemeBody scheme)) ->
                  Left
                    ( ValidationFailed
                        [ "root RaiseMerge owner did not reify as a lambda arrow"
                        , "  edge: " ++ show edgeId
                        , "  scheme: " ++ show scheme
                        ]
                    )
            _ -> pure ()
          let bodyConstructed =
                if endpointAlreadyConstructed
                  then schemeBody scheme
                  else rebuildBody (TVarRef exteriorRef)
              requiredConstructionRefs =
                [ ref
                | required <- grRequiredGammaBinders requirements
                , Just ref <-
                    [ IntMap.lookup
                        (getNodeId (rgbExteriorNode required))
                        (siSubstRefs info)
                    ]
                ]
                  ++ [ ref
                     | nodeKey <- IntSet.toList (grLocallyClosedGammaNodes requirements)
                     , Just ref <- [IntMap.lookup nodeKey (siSubstRefs info)]
                     ]
                  ++ concatMap
                    constructionUsedRefs
                    (grTermUsedRootBinderRefs requirements)
              retainedBinders =
                binderDependencyClosure
                  binders
                  ( freeTypeVarRefsType bodyConstructed
                      ++ requiredConstructionRefs
                  )
          pure
            ( rebuildSchemeInfoFromRefSubst
                info
                  { siConstructionBinderOrderRefs =
                      IntMap.insert
                        (getNodeId exterior)
                        exteriorRef
                        (siConstructionBinderOrderRefs info)
                  }
                (mkElabSchemeWithRefs retainedBinders bodyConstructed)
                (siSubstRefs info)
            )
      where
        exterior = rrmaExterior authority
        constructionUsedRefs ref =
          ref
            : case typeBinderRefNode ref of
              Just node ->
                maybeToList
                  (IntMap.lookup (getNodeId node) (siSubstRefs info))
              Nothing -> []

    isArrowBody ty =
      case ty of
        TArrow {} -> True
        _ -> False

    lookupPreparedRef label node info =
      case IntMap.lookup (getNodeId node) (siSubstRefs info) of
        Just ref -> pure ref
        Nothing ->
          Left
            ( ValidationFailed
                [ "root RaiseMerge " ++ label ++ " has no Γ substitution"
                , "  node: " ++ show node
                , "  scheme: " ++ show (siScheme info)
                ]
            )

    validateExteriorBinder authority info = do
      requirement <- requiredGammaBinderForRootRaiseMerge edgeId authority requirements
      exteriorRef <-
        requireSubstitution "exterior" exterior
      resultRef <-
        requireSubstitution "result root" resultRoot
      let expectedExteriorRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode exterior)
              (typeBinderRefName exteriorRef)
      if typeBinderRefsSameIdentity resultRef exteriorRef
        then pure ()
        else
          Left
            ( ValidationFailed
                [ "root RaiseMerge result bridge does not name the exterior Γ binder",
                  "  exterior binder: " ++ show exteriorRef,
                  "  result-root binder: " ++ show resultRef
                ]
            )
      if typeBinderRefsSameIdentity exteriorRef expectedExteriorRef
        then
          case find (typeBinderRefsSameIdentity exteriorRef . fst) binders of
            Nothing ->
              Left
                ( ValidationFailed
                    [ "root RaiseMerge exterior substitution does not name a Γ binder",
                      "  operated root: " ++ show (rrmaOperatedRoot authority),
                      "  exterior: " ++ show exterior,
                      "  result root: " ++ show resultRoot,
                      "  scheme: " ++ show (siScheme info)
                    ]
                )
            Just binder -> do
              validateBinderBound "exterior Gamma binder" requirement binder
              Right info
        else do
          let exteriorBinderSurvived =
                any
                  (typeBinderRefsSameIdentity expectedExteriorRef . fst)
                  binders
              aliasTargetBinder =
                find (typeBinderRefsSameIdentity exteriorRef . fst) binders
              aliasTargetIsFree =
                any
                    (typeBinderRefsSameIdentity exteriorRef)
                    (freeTypeVarRefsType (schemeToType (siScheme info)))
              aliasTargetInScope =
                maybe False (const True) aliasTargetBinder
                  || aliasTargetIsFree
              -- A root RaiseMerge may quotient its exterior directly to the
              -- lexical variable named by S(operated), while the completed
              -- result no longer mentions that variable.  In that case the
              -- binder is deliberately absent from the result scheme: the
              -- matched edge requirement is the ownership proof, and the
              -- exterior/result substitutions prove that this exact variable
              -- is the quotient target.  Do not require a vacuous occurrence
              -- merely to keep the alias visibly in the scheme.
              aliasTargetOwnedByRequirement =
                case rgbOperatedType requirement of
                  TVarRef operatedRef ->
                    typeBinderRefsSameIdentity exteriorRef operatedRef
                  _ -> False
          if
              (aliasTargetInScope || aliasTargetOwnedByRequirement)
                && not exteriorBinderSurvived
            then do
              case aliasTargetBinder of
                Just binder ->
                  validateAliasTargetBound requirement binder
                Nothing -> pure ()
              Right info
            else
              Left
                ( ValidationFailed
                    [ "root RaiseMerge Γ alias was not quotiented to S(operated)",
                      "  operated root: " ++ show (rrmaOperatedRoot authority),
                      "  exterior: " ++ show exterior,
                      "  exterior ref: " ++ show exteriorRef,
                      "  alias target in scope: " ++ show aliasTargetInScope,
                      "  exterior binder survived: " ++ show exteriorBinderSurvived
                    ]
                )
      where
        exterior = rrmaExterior authority
        resultRoot = rrmaResultRoot authority
        binders = schemeBinderRefs (siScheme info)

        validateBinderBound label requirement (binderRef, mbBound) =
          let expectedBound =
                foldl
                  projectRequirementRef
                  (rgbOperatedType requirement)
                  (freeTypeVarRefsType (rgbOperatedType requirement))
              actualBound = maybe TBottom tyToElab mbBound
              closesByExactOccurrenceIdentity =
                case
                    ( rgbExactOperatedOccurrenceRef requirement
                    , expectedBound
                    )
                  of
                    (Just exactRef, TVarRef operatedRef) ->
                      actualBound == TBottom
                        && typeBinderRefsSameIdentity binderRef exactRef
                        && typeBinderRefsSameIdentity binderRef operatedRef
                    _ -> False
              boundsAgree =
                closesByExactOccurrenceIdentity
                  || alphaEqType expectedBound actualBound
                  || churchAwareEqType expectedBound actualBound
                  || enclosingBinderContextConstructsExpectedBound
                    binderRef
                    expectedBound
                    actualBound
          in if boundsAgree
              then Right ()
              else
                Left
                  ( ValidationFailed
                      [ "root RaiseMerge " ++ label ++ " disagrees with S(operated)",
                        "  binder: " ++ show binderRef,
                        "  expected bound: " ++ show expectedBound,
                        "  actual bound: " ++ show actualBound
                      ]
                  )
          where
            -- The requirement is recorded in the graph occurrence domain,
            -- while packet preparation may already have projected two such
            -- occurrences to one source identity.  Compare S(operated) only
            -- after replaying this exact SchemeInfo substitution; otherwise
            -- a source lambda such as @\y -> y@ appears spuriously as
            -- @t6 -> t7@ versus @t6 -> t6@.
            projectRequirementRef ty ref =
              case typeBinderRefNode ref of
                Just node
                  | Just routedRef <-
                      IntMap.lookup
                        (getNodeId node)
                        (siSubstRefs info) ->
                      substTypeCaptureRef ref (TVarRef routedRef) ty
                _ -> ty

            -- Figure 15.3.4 translates a non-root RaiseMerge through the
            -- computation context C(r -> n).  Consequently, a leading forall
            -- of S(operated) can already be an adjacent declaration in the
            -- enclosing Gamma, while this exterior binder stores only the
            -- opened remainder.  Validate that construction directly: the
            -- exact dependency closure must be a contiguous suffix before
            -- the exterior, every declaration must retain its SchemeInfo
            -- route, and wrapping that suffix around the stored bound must
            -- reconstruct the complete expected type.
            enclosingBinderContextConstructsExpectedBound
              targetBinderRef
              expectedBound
              actualBound =
              not (null contextBinders)
                && contextBindersFormAdjacentSuffix
                && all contextBinderHasExactRoute contextBinders
                && ( alphaEqType expectedBound contextualActualBound
                      || churchAwareEqType
                        expectedBound
                        contextualActualBound
                   )
              where
                precedingBinders =
                  takeWhile
                    ( not
                        . typeBinderRefsSameIdentity targetBinderRef
                        . fst
                    )
                    binders
                contextBinders =
                  binderDependencyClosure
                    precedingBinders
                    (freeTypeVarRefsType actualBound)
                contextBindersFormAdjacentSuffix =
                  length contextBinders <= length precedingBinders
                    && and
                      ( zipWith
                          binderDeclarationsAgree
                          contextBinders
                          ( drop
                              (length precedingBinders - length contextBinders)
                              precedingBinders
                          )
                      )
                contextualActualBound =
                  schemeToType
                    ( mkElabSchemeWithRefs
                        contextBinders
                        actualBound
                    )

                contextBinderHasExactRoute (contextRef, _) =
                  case typeBinderRefNode contextRef of
                    Just node ->
                      maybe
                        False
                        (typeBinderRefsSameIdentity contextRef)
                        ( IntMap.lookup
                            (getNodeId node)
                            (siSubstRefs info)
                        )
                    Nothing -> False

                binderDeclarationsAgree
                  (leftRef, leftBound)
                  (rightRef, rightBound) =
                    typeBinderRefsSameIdentity leftRef rightRef
                      && boundsEquivalent
                        (maybe TBottom tyToElab leftBound)
                        (maybe TBottom tyToElab rightBound)

                boundsEquivalent left right =
                  alphaEqType left right || churchAwareEqType left right

        -- When generalization quotients the exterior directly to the lexical
        -- variable used by S(operated), that variable is the result of the
        -- RaiseMerge rather than a freshly constructed Gamma binder.  Its own
        -- lexical bound therefore remains unchanged; requiring it to equal
        -- itself would incorrectly turn an unbounded lexical variable into
        -- @a >= a@.  This exemption is identity-based and applies only to the
        -- exact variable named by S(operated).  Other aliases still have to
        -- carry the required bound.
        validateAliasTargetBound requirement binder@(binderRef, _) =
          case rgbOperatedType requirement of
            TVarRef operatedRef
              | typeBinderRefsSameIdentity binderRef operatedRef -> Right ()
            _ -> validateBinderBound "local Gamma alias target" requirement binder

        requireSubstitution label node =
          case IntMap.lookup (getNodeId node) (siSubstRefs info) of
            Just ref -> Right ref
            Nothing ->
              Left
                ( ValidationFailed
                    [ "root RaiseMerge " ++ label ++ " has no Γ substitution",
                      "  node: " ++ show node,
                      "  scheme: " ++ show (siScheme info)
                    ]
                )

-- | Select the unique paper-owned @S'(operated)@ declaration for an exact
-- terminal root 'RaiseMerge'.  Consumers use this certificate while the
-- witness, result root, and requirement are still in the same construction
-- domain; no term shape or reified-type equality grants Gamma authority.
requiredGammaBinderForRootRaiseMerge
  :: EdgeId
  -> RootRaiseMergeAuthority
  -> GeneralizationRequirements
  -> Either ElabError RequiredGammaBinder
requiredGammaBinderForRootRaiseMerge edgeId rootAuthority requirements =
  case matches of
    [requirement] -> Right requirement
    [] ->
      Left
        ( ValidationFailed
            [ "root RaiseMerge has no matching S(operated) requirement",
              "  edge: " ++ show edgeId,
              "  operated root: " ++ show (rrmaOperatedRoot rootAuthority),
              "  exterior: " ++ show (rrmaExterior rootAuthority),
              "  result root: " ++ show (rrmaResultRoot rootAuthority),
              "  requirements: " ++ show (grRequiredGammaBinders requirements)
            ]
        )
    _ ->
      Left
        ( ValidationFailed
            [ "root RaiseMerge has multiple matching S(operated) requirements",
              "  edge: " ++ show edgeId,
              "  exterior: " ++ show (rrmaExterior rootAuthority),
              "  result root: " ++ show (rrmaResultRoot rootAuthority),
              "  matches: " ++ show matches
            ]
        )
  where
    matches =
      [ requirement
      | requirement <- grRequiredGammaBinders requirements
      , edgeId `elem` rgbEdgeIds requirement
      , rgbExteriorNode requirement == rrmaExterior rootAuthority
      , rrmaResultRoot rootAuthority
          `elem` NonEmpty.toList (rgbResultRoots requirement)
      ]

subtermGeneralizationSchemeInfo :: PreparedSubtermGeneralization -> SchemeInfo
subtermGeneralizationSchemeInfo = psgSchemeInfo

subtermGeneralizationConsumerConstructionSchemeInfo
  :: PreparedSubtermGeneralization
  -> SchemeInfo
subtermGeneralizationConsumerConstructionSchemeInfo =
  psgConsumerConstructionSchemeInfo

-- | The packet view consumed while projecting a recursively checked child
-- into its enclosing construction Gamma.
--
-- An enclosing source constructor consumes the already selected complete
-- Gamma bound. Compiler-exact specialization is published only through the
-- packet's explicit exact-boundary certificate, never inferred from type
-- shape at this projection point.
-- Other consumers construct the packet locally and therefore still operate
-- on the packet-local operated view.
subtermGeneralizationConsumerProjectionSchemeInfo
  :: PreparedSubtermGeneralization
  -> SchemeInfo
subtermGeneralizationConsumerProjectionSchemeInfo packet =
  case psgConsumerAuthority packet of
    Just EnclosingGammaConsumerAuthority{} ->
      projected
        { siSourceBinderOrderRefs =
            retainedOrderRoutes
              ( IntMap.union
                  (siSourceBinderOrderRefs constructionInfo)
                  (siSourceBinderOrderRefs operatedInfo)
              )
        , siConstructionBinderOrderRefs =
            retainedOrderRoutes
              ( IntMap.union
                  (siConstructionBinderOrderRefs constructionInfo)
                  (siConstructionBinderOrderRefs operatedInfo)
              )
        }
    _ ->
      psgOperatedSchemeInfo packet
  where
    constructionInfo = psgSchemeInfo packet
    operatedInfo = psgOperatedSchemeInfo packet
    gammaType = schemeToType (psgGammaBoundScheme packet)
    projectionAuthority
      | alphaEqType gammaType (schemeToType (siScheme constructionInfo)) =
          constructionInfo
      | otherwise =
          operatedInfo
    projectionRefs =
      IntMap.union
        (siSubstRefs projectionAuthority)
        ( IntMap.union
            (siSubstRefs constructionInfo)
            (siSubstRefs operatedInfo)
        )
    projected =
      rebuildSchemeInfoFromRefSubst
        projectionAuthority
        (psgGammaBoundScheme packet)
        projectionRefs
    projectedDeclarationRefs =
      typeBinderDeclarationRefs gammaType
    retainedOrderRoutes =
      IntMap.filterWithKey
        ( \nodeKey orderedRef ->
            any
              (typeBinderRefsSameIdentity orderedRef)
              projectedDeclarationRefs
              && maybe
                False
                (typeBinderRefsSameIdentity orderedRef)
                (IntMap.lookup nodeKey (siSubstRefs projected))
        )

-- | Exact original-identity to fresh-copy routes allocated when this packet
-- is installed as an enclosing Gamma bound.
subtermGeneralizationCopiedBinderRoutes
  :: PreparedSubtermGeneralization
  -> [(TypeBinderIdentity, TypeBinderRef)]
subtermGeneralizationCopiedBinderRoutes =
  Map.toList . psgCopiedBinderRefs

-- | The bottom-up operated view used while constructing this packet.  It may
-- be open over quantifiers owned by the completed packet, so an enclosing
-- root RaiseMerge must consume 'subtermGeneralizationSchemeInfo' through
-- 'subtermGeneralizationGammaBoundSchemeForConsumer' instead.
subtermGeneralizationOperatedSchemeInfo :: PreparedSubtermGeneralization -> SchemeInfo
subtermGeneralizationOperatedSchemeInfo = psgOperatedSchemeInfo

-- | The compiler exact boundary that owns this packet's operated result.
-- Keeping the edge identity, rather than a Boolean marker, prevents an outer
-- exact annotation from completing a packet owned by a nested exact
-- annotation in the same term.
subtermGeneralizationCompilerExactBoundary :: PreparedSubtermGeneralization -> Maybe EdgeId
subtermGeneralizationCompilerExactBoundary packet =
  case psgCompilerExactResult packet of
    Just (CompilerExactSourceSubtermResult edge _ _) -> Just edge
    Just (CompilerExactPacketSubtermResult edge _) -> Just edge
    Just (CompilerExactDescendantSubtermResult edge _) -> Just edge
    Just (CompilerExactEnclosingSubtermResult edge _) -> Just edge
    Nothing -> Nothing

withCompilerExactSourceSubtermResult
  :: EdgeId
  -> TypeBinderRef
  -> TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withCompilerExactSourceSubtermResult exactEdge packetResultRef sourceResultRef =
  withCompilerExactSubtermResult
    (CompilerExactSourceSubtermResult exactEdge packetResultRef sourceResultRef)

withCompilerExactPacketSubtermResult
  :: EdgeId
  -> TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withCompilerExactPacketSubtermResult exactEdge packetResultRef =
  withCompilerExactSubtermResult
    (CompilerExactPacketSubtermResult exactEdge packetResultRef)

-- | Record that the exact result action is owned by a deeper packet in the
-- same source lambda spine.  This is distinct from an enclosing-Gamma result:
-- no ambient consumer declaration is required, and the terminal descendant
-- remains the unique delayed-Hyp owner.
withCompilerExactDescendantSubtermResult
  :: EdgeId
  -> TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withCompilerExactDescendantSubtermResult exactEdge packetResultRef =
  withCompilerExactSubtermResult
    (CompilerExactDescendantSubtermResult exactEdge packetResultRef)

-- | Record a packet-local result whose enclosing RaiseMerge consumer already
-- constructs the complete operated endpoint.  The marker suppresses the
-- packet's inner Hyp, but deliberately carries no completion action: emitting
-- both Hyp(result) and Hyp(consumer) would change the latter's source type.
withCompilerExactEnclosingSubtermResult
  :: EdgeId
  -> TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withCompilerExactEnclosingSubtermResult exactEdge packetResultRef =
  withCompilerExactSubtermResult
    (CompilerExactEnclosingSubtermResult exactEdge packetResultRef)

withCompilerExactSubtermResult
  :: CompilerExactSubtermResult
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withCompilerExactSubtermResult exactResult packet
  | any
      (typeBinderRefsSameIdentity packetResultRef . fst)
      (schemeBinderRefs (siScheme (psgSchemeInfo packet))) =
      Right packet {psgCompilerExactResult = Just exactResult}
  | otherwise =
      Left
        ( ValidationFailed
            [ "compiler exact result is not owned by the prepared packet"
            , "  packet result: " ++ show packetResultRef
            , "  exact result: " ++ show exactResult
            , "  packet: " ++ show packet
            ]
        )
  where
    packetResultRef =
      case exactResult of
        CompilerExactSourceSubtermResult _ ref _ -> ref
        CompilerExactPacketSubtermResult _ ref -> ref
        CompilerExactDescendantSubtermResult _ ref -> ref
        CompilerExactEnclosingSubtermResult _ ref -> ref

subtermGeneralizationConsumerIdentity :: PreparedSubtermGeneralization -> Maybe TypeBinderIdentity
subtermGeneralizationConsumerIdentity =
  fmap scaConsumerIdentity . psgConsumerAuthority

subtermGeneralizationConsumerAuthority
  :: PreparedSubtermGeneralization
  -> Maybe SubtermConsumerAuthority
subtermGeneralizationConsumerAuthority = psgConsumerAuthority

subtermGeneralizationLocalResultAuthority
  :: PreparedSubtermGeneralization
  -> Maybe SubtermConsumerAuthority
subtermGeneralizationLocalResultAuthority = psgLocalResultAuthority

-- | The graph-semantic result binder consumed by this subterm. A root
-- RaiseMerge or validated identity-topology consumer supplies its exact
-- identity from graph construction evidence.
subtermGeneralizationResultAbstractionRef :: PreparedSubtermGeneralization -> Maybe TypeBinderRef
subtermGeneralizationResultAbstractionRef packet =
  case psgLocalResultAuthority packet of
    Just authority ->
      Just (consumerRef (scaConsumerIdentity authority))
    Nothing ->
      case psgGammaAuthority packet of
        Just authority ->
          Just (consumerRef (gpaConsumerIdentity authority))
        Nothing ->
          case psgConsumerAuthority packet of
            Just authority@EnclosingGammaConsumerAuthority{} ->
              Just (consumerRef (scaConsumerIdentity authority))
            Just authority
              | subtermConsumerAuthorityIsTopology authority ->
                  Just (consumerRef (scaConsumerIdentity authority))
            Just authority
              | subtermConsumerAuthorityIsRootGamma authority ->
                  Just (consumerRef (scaConsumerIdentity authority))
            _ -> Nothing
  where
    consumerRef identity =
      typeBinderRefFromIdentity
        identity
        (typeBinderIdentityStableName identity)

-- | The result binder as represented by the packet's completed construction
-- scheme.  Source-binder projection may quotient the graph consumer directly
-- into an enclosing source ABI; construction placement must follow that
-- substitution, while edge translation must retain the graph-semantic result
-- returned by 'subtermGeneralizationResultAbstractionRef'.
subtermGeneralizationConstructionResultAbstractionRef
  :: PreparedSubtermGeneralization
  -> Maybe TypeBinderRef
subtermGeneralizationConstructionResultAbstractionRef packet =
  fmap constructionRef (subtermGeneralizationResultAbstractionRef packet)
  where
    constructionRef graphRef =
      case typeBinderRefNode graphRef of
        Just node ->
          IntMap.findWithDefault
            graphRef
            (getNodeId node)
            (siSubstRefs (psgSchemeInfo packet))
        Nothing -> graphRef

-- | The outgoing Gamma result abstraction delayed by a compiler exact
-- boundary. The prepared exact-result certificate retains the packet-owned
-- identity selected by witness or topology authority. The exact boundary
-- consumes this accessor before it completes the packet.
subtermGeneralizationCompilerExactResultRef :: PreparedSubtermGeneralization -> Maybe TypeBinderRef
subtermGeneralizationCompilerExactResultRef packet =
  case psgCompilerExactResult packet of
    Just (CompilerExactSourceSubtermResult _ packetRef _) -> Just packetRef
    Just (CompilerExactPacketSubtermResult _ packetRef) -> Just packetRef
    Just (CompilerExactDescendantSubtermResult _ packetRef) -> Just packetRef
    Just (CompilerExactEnclosingSubtermResult _ packetRef) -> Just packetRef
    Nothing -> Nothing

-- | Binder already present in the source construction at the exact boundary.
-- Source-owned completion retargets this lexical abstraction to the prepared
-- packet result identity; packet-owned completion uses the same identity for
-- both roles.
subtermGeneralizationCompilerExactExistingRef
  :: PreparedSubtermGeneralization
  -> Maybe TypeBinderRef
subtermGeneralizationCompilerExactExistingRef packet =
  case psgCompilerExactResult packet of
    Just (CompilerExactSourceSubtermResult _ _ sourceRef) -> Just sourceRef
    Just (CompilerExactPacketSubtermResult _ packetRef) -> Just packetRef
    Just (CompilerExactDescendantSubtermResult _ packetRef) -> Just packetRef
    Just CompilerExactEnclosingSubtermResult{} -> Nothing
    Nothing -> Nothing

-- | Semantic binder emitted at the exact boundary.  For a source-owned result
-- this is the source forall identity; the distinct packet result ref remains
-- the construction-local proof that the operated bound was prepared.
subtermGeneralizationCompilerExactCompletionRef
  :: PreparedSubtermGeneralization
  -> Maybe TypeBinderRef
subtermGeneralizationCompilerExactCompletionRef packet =
  case psgCompilerExactResult packet of
    Just (CompilerExactSourceSubtermResult _ packetRef _) -> Just packetRef
    Just (CompilerExactPacketSubtermResult _ packetRef) -> Just packetRef
    Just (CompilerExactDescendantSubtermResult _ packetRef) -> Just packetRef
    Just CompilerExactEnclosingSubtermResult{} -> Nothing
    Nothing -> Nothing

subtermGeneralizationCompilerExactResultStage
  :: PreparedSubtermGeneralization
  -> Maybe CompilerExactResultStage
subtermGeneralizationCompilerExactResultStage packet =
  case psgCompilerExactResult packet of
    Just CompilerExactSourceSubtermResult{} -> Just CompleteBeforeCompilerExact
    Just CompilerExactPacketSubtermResult{} -> Just CompleteAfterCompilerExact
    Just CompilerExactDescendantSubtermResult{} -> Nothing
    Just CompilerExactEnclosingSubtermResult{} -> Nothing
    Nothing -> Nothing

-- | Whether this packet carries only the transitive proof that a deeper
-- source-lambda packet owns the exact result action.
subtermGeneralizationCompilerExactResultIsDelegated
  :: PreparedSubtermGeneralization
  -> Bool
subtermGeneralizationCompilerExactResultIsDelegated packet =
  case psgCompilerExactResult packet of
    Just CompilerExactDescendantSubtermResult{} -> True
    _ -> False

subtermGeneralizationCompilerExactBinderRenames
  :: PreparedSubtermGeneralization
  -> [(TypeBinderRef, TypeBinderRef)]
subtermGeneralizationCompilerExactBinderRenames =
  psgCompilerExactBinderRenames

subtermGeneralizationConstructionBinderRenames
  :: PreparedSubtermGeneralization
  -> [(TypeBinderRef, TypeBinderRef)]
subtermGeneralizationConstructionBinderRenames =
  psgConstructionBinderRenames

-- | The exact source-owned computation that consumes this packet without
-- publishing its intermediate enclosing Gamma declaration.  Revalidate the
-- private certificate against the current packet before exposing its
-- endpoints to elaboration.
subtermGeneralizationExactConsumerSpecialization
  :: PreparedSubtermGeneralization
  -> Maybe
      ( SubtermConsumerAuthority
      , ElabType
      , [(TypeBinderRef, TypeBinderRef)]
      , Instantiation
      )
subtermGeneralizationExactConsumerSpecialization packet = do
  certificate <- psgExactConsumerSpecialization packet
  guard
    ( exactConsumerSpecializationFor
        (ecsExpectedEndpoint certificate)
        packet
        == Just certificate
    )
  pure
    ( ecsAuthority certificate
    , ecsExpectedEndpoint certificate
    , exactBinderSpineRenames (ecsPlan certificate)
    , exactBinderSpineInstantiation (ecsPlan certificate)
    )

-- | Revalidate the source-owner endpoint sealed during packet preparation.
-- The paired source/canonical walk supplies the endpoint and owner; this view
-- exposes them only while the packet still has the exact consumer authority
-- and frozen operated state against which the certificate was created.
subtermGeneralizationSourceOwnerConsumerCompletion
  :: PreparedSubtermGeneralization
  -> Maybe
      ( SubtermConsumerAuthority
      , LocalGammaOwner
      , ElabType
      , ElabType
      )
subtermGeneralizationSourceOwnerConsumerCompletion packet = do
  validatedSourceOwnerConsumerCompletion
    (psgSourceOwnerConsumerCompletion packet)
    packet

-- | Revalidate the complete source-owner result endpoint sealed during
-- packet preparation.  Unlike the body-completion view, this certificate is
-- usable only when the packet consumer is the exact result node of its owner.
-- Root-edge grouping may then use it to select an existing requirement; it
-- never manufactures a new operated root from the endpoint's type shape.
subtermGeneralizationSourceOwnerFinalConsumerCompletion
  :: PreparedSubtermGeneralization
  -> Maybe
      ( SubtermConsumerAuthority
      , LocalGammaOwner
      , ElabType
      , ElabType
      )
subtermGeneralizationSourceOwnerFinalConsumerCompletion packet = do
  completion@(_authority, owner, _frozenOperatedType, _expectedEndpoint) <-
    validatedSourceOwnerConsumerCompletion
      (psgSourceOwnerFinalConsumerCompletion packet)
      packet
  guard
    ( scaConsumerIdentity (completionAuthority completion)
        == typeBinderIdentityFromNode (lgoTermNode owner)
    )
  pure completion
  where
    completionAuthority (authority, _, _, _) = authority

validatedSourceOwnerConsumerCompletion
  :: Maybe SourceOwnerConsumerCompletion
  -> PreparedSubtermGeneralization
  -> Maybe
      ( SubtermConsumerAuthority
      , LocalGammaOwner
      , ElabType
      , ElabType
      )
validatedSourceOwnerConsumerCompletion mbCertificate packet = do
  certificate <- mbCertificate
  authority <- psgConsumerAuthority packet
  guard (authority == soccAuthority certificate)
  owner <- subtermConsumerAuthorityEnclosingOwner authority
  guard (owner == soccOwner certificate)
  guard (isNothing (psgGammaAuthority packet))
  let currentOperatedType =
        schemeToType (siScheme (psgOperatedSchemeInfo packet))
      frozenOperatedType = soccFrozenOperatedType certificate
      expectedEndpoint = soccExpectedEndpoint certificate
      consumerIdentity = scaConsumerIdentity authority
  guard (typesAgree currentOperatedType frozenOperatedType)
  guard
    ( all
        ((/= consumerIdentity) . typeBinderRefIdentity)
        ( typeBinderDeclarationRefs expectedEndpoint
            ++ freeTypeVarRefsType expectedEndpoint
        )
    )
  pure
    ( authority
    , owner
    , frozenOperatedType
    , expectedEndpoint
    )
  where
    typesAgree left right =
      alphaEqTypePreservingStructuralBinders left right
        || churchRepresentationEqType left right

subtermGeneralizationPlacedCopiedBinderRoutes
  :: PreparedSubtermGeneralization
  -> Map.Map
      TypeBinderIdentity
      [(TypeBinderIdentity, TypeBinderRef)]
subtermGeneralizationPlacedCopiedBinderRoutes =
  psgPlacedCopiedBinderRefs

subtermGeneralizationInheritedGammaRoutes
  :: PreparedSubtermGeneralization
  -> Reify.InheritedGammaRoutes
subtermGeneralizationInheritedGammaRoutes =
  psgInheritedGammaRoutes

withInheritedGammaRoutes
  :: Reify.InheritedGammaRoutes
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withInheritedGammaRoutes routes packet = do
  merged <-
    Reify.mergeInheritedGammaRoutes
      (psgInheritedGammaRoutes packet)
      routes
  pure packet {psgInheritedGammaRoutes = merged}

withPlacedCopiedBinderRoutes
  :: Map.Map
      TypeBinderIdentity
      [(TypeBinderIdentity, TypeBinderRef)]
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withPlacedCopiedBinderRoutes routes packet = do
  merged <-
    foldM
      insertTargetRoutes
      (psgPlacedCopiedBinderRefs packet)
      (Map.toList routes)
  pure packet {psgPlacedCopiedBinderRefs = merged}
  where
    insertTargetRoutes accumulated (targetIdentity, incomingRoutes) =
      case Map.lookup targetIdentity accumulated of
        Nothing ->
          pure (Map.insert targetIdentity incomingRoutes accumulated)
        Just existingRoutes
          | copiedRoutesAgree existingRoutes incomingRoutes ->
              pure accumulated
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "prepared packet has conflicting placed copied-binder routes"
                    , "  target identity: " ++ show targetIdentity
                    , "  first routes: " ++ show existingRoutes
                    , "  second routes: " ++ show incomingRoutes
                    ]
                )

    copiedRoutesAgree left right =
      length left == length right
        && all
          ( \(sourceIdentity, copiedRef) ->
              case lookup sourceIdentity right of
                Just otherCopiedRef ->
                  typeBinderRefsSameIdentity copiedRef otherCopiedRef
                Nothing -> False
          )
          left

withConstructionBinderRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> PreparedSubtermGeneralization
  -> PreparedSubtermGeneralization
withConstructionBinderRenames renames packet =
  packet
    { psgConstructionBinderRenames = renames
    , psgPlacedCopiedBinderRefs =
        renamePlacedCopiedBinderRefs
          renames
          (psgPlacedCopiedBinderRefs packet)
    , psgInheritedGammaRoutes =
        renameInheritedGammaRoutes
          renames
          (psgInheritedGammaRoutes packet)
    }

subtermGeneralizationOpaqueResultConstruction
  :: PreparedSubtermGeneralization
  -> Maybe ([TypeBinderRef], ElabType)
subtermGeneralizationOpaqueResultConstruction packet = do
  certificate <- psgOpaqueResultConstruction packet
  pure
    ( orcCarrierRefs certificate
    , orcConstructedType certificate
    )

subtermGeneralizationOpaqueResultConstructionPlan
  :: PreparedSubtermGeneralization
  -> Maybe ExactBinderSpinePlan
subtermGeneralizationOpaqueResultConstructionPlan packet = do
  certificate <- psgOpaqueResultConstruction packet
  case orcCompletionAuthority certificate of
    OpaqueResultBinderSpineCompletion plan -> Just plan
    OpaqueResultSourceLambdaCompletion {} -> Nothing

-- | Project the source-constructor authority sealed by
-- 'withOpaqueResultConstruction'.  This view is deliberately unavailable for
-- an ordinary binder-spine completion: consumers may use it only when every
-- provisional carrier was tied to the exact source lambda that constructs the
-- opaque endpoint.  A later checked owner must still match those lambda nodes
-- against its own result-construction certificate before using the endpoint.
subtermGeneralizationOpaqueResultSourceLambdaCompletion
  :: PreparedSubtermGeneralization
  -> Maybe (ElabType, [(NodeId, ElabType)])
subtermGeneralizationOpaqueResultSourceLambdaCompletion packet = do
  certificate <- psgOpaqueResultConstruction packet
  case orcCompletionAuthority certificate of
    OpaqueResultSourceLambdaCompletion authorities ->
      Just (orcConstructedType certificate, authorities)
    OpaqueResultBinderSpineCompletion {} -> Nothing

-- | Seal a source-proved opaque result composition.  The completed packet
-- must construct the exact source endpoint either by a replayable binder-spine
-- plan or by the source lambda that made the result path opaque.  The packet's
-- consumer-facing Gamma bound is a later publication state, not this
-- completion's target; requiring a second binder-spine plan to that bound
-- would discard valid completed-to-source plans.  None of the erased
-- provisional carriers may survive in the source endpoint.
withOpaqueResultConstruction
  :: [(TypeBinderRef, PreparedSubtermGeneralization)]
  -> ElabType
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withOpaqueResultConstruction carrierPackets constructedType packet
  | null distinctCarrierRefs =
      Left
        ( ValidationFailed
            [ "opaque result construction has no provisional carrier"
            ]
        )
  | not (null survivingCarriers) =
      Left
        ( ValidationFailed
            [ "opaque result construction retains a provisional carrier"
            , "  carriers: " ++ show distinctCarrierRefs
            , "  surviving carriers: " ++ show survivingCarriers
            , "  constructed type: " ++ show constructedType
            ]
        )
  | otherwise = do
      completionAuthority <-
        case
            planExactBinderSpine
              typesAgree
              completedConstructionType
              constructedType
          of
            Just completionPlan ->
              pure
                (OpaqueResultBinderSpineCompletion completionPlan)
            Nothing -> sourceLambdaCompletion
      pure
        packet
          { psgOpaqueResultConstruction =
              Just
                OpaqueResultConstruction
                  { orcCarrierRefs = distinctCarrierRefs
                  , orcConstructedType = constructedType
                  , orcCompletionAuthority = completionAuthority
                  }
          }
  where
    carrierRefs = map fst carrierPackets
    completedConstructionType =
      schemeToType (siScheme (psgSchemeInfo packet))
    consumerBoundType = schemeToType (psgGammaBoundScheme packet)
    survivingCarriers =
      [ freeRef
      | freeRef <- freeTypeVarRefsType constructedType
      , any
          (typeBinderRefsSameIdentity freeRef)
          distinctCarrierRefs
      ]
    distinctCarrierRefs = foldl insertDistinct [] carrierRefs
    insertDistinct refs ref
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = refs ++ [ref]
    typesAgree left right =
      alphaEqType left right || churchAwareEqType left right
    sourceLambdaCompletion =
      case traverse sourceLambdaAuthority carrierPackets of
        Just authorities ->
          pure
            (OpaqueResultSourceLambdaCompletion authorities)
        Nothing ->
          Left
            ( ValidationFailed
                [ "opaque source result has no exact completion authority"
                , "  completed construction: "
                    ++ show completedConstructionType
                , "  constructed source result: " ++ show constructedType
                , "  consumer bound: " ++ show consumerBoundType
                , "  carriers: " ++ show distinctCarrierRefs
                , "  source lambda parameter: "
                    ++ show (psgSourceLambdaParameter packet)
                ]
            )

    sourceLambdaAuthority (carrierRef, producerPacket) = do
      parameter <- psgSourceLambdaParameter producerPacket
      guard
        ( typeBinderRefNode carrierRef
            == Just (slpLambdaNode parameter)
        )
      guard
        ( sourceTypeConstructsParameter
            parameter
            constructedType
        )
      pure (slpLambdaNode parameter, slpParameterType parameter)

    sourceTypeConstructsParameter parameter ty =
      case splitForallsRefs ty of
        (binders, TArrow (TVarRef domainRef) codomain) ->
          ( sourceParameterDomainIsConstructed
              parameter
              binders
              (TVarRef domainRef)
          )
            || sourceTypeConstructsParameter parameter codomain
        (binders, TArrow domain codomain) ->
          sourceParameterDomainIsConstructed parameter binders domain
            || sourceTypeConstructsParameter parameter codomain
        _ -> False

    sourceParameterDomainIsConstructed parameter binders domain =
      typesAgree domain (slpParameterType parameter)
        && case slpLocalBinderRef parameter of
          Nothing -> True
          Just parameterRef ->
            case domain of
              TVarRef domainRef ->
                typeBinderRefsSameIdentity parameterRef domainRef
                  && any
                    ( \(binderRef, mbBound) ->
                        typeBinderRefsSameIdentity parameterRef binderRef
                          && isNothing mbBound
                    )
                    binders
              _ -> False

-- | Seal the first source-authorized endpoint that consumes an enclosing
-- packet.
--
-- This certificate is created while packet preparation still owns the source
-- expectation, the completed construction, and the operated endpoint.  A
-- later projected operated scheme may legitimately omit the intermediate
-- Gamma consumer, but only this stored xMLF computation authorizes that
-- omission.  Candidate order is source-authority order, but selection itself
-- is by construction: an endpoint is accepted only when its exact leading
-- binder-spine plan can be built from this packet.  If no candidate admits
-- such a plan, the packet remains on its ordinary lexical-Gamma route; no
-- partial or unchecked certificate is stored.
withExactConsumerSpecialization
  :: [ElabType]
  -> PreparedSubtermGeneralization
  -> PreparedSubtermGeneralization
withExactConsumerSpecialization expectedEndpoints packet =
  case
      [ certificate
      | expectedEndpoint <- expectedEndpoints
      , Just certificate <-
          [exactConsumerSpecializationFor expectedEndpoint packet]
      ]
  of
    [] -> packet
    certificate : _ ->
      packet
        { psgExactConsumerSpecialization = Just certificate
        }

-- | Seal the exact body endpoint obtained from the enclosing source owner.
-- The caller owns a lockstep source/canonical traversal of that owner; this
-- constructor checks the packet-local facts that make the endpoint usable by
-- the later body consumer.  In particular, it cannot be attached to a
-- packet-owned Gamma or to an endpoint that still mentions the provisional
-- consumer identity.
withSourceOwnerConsumerCompletion
  :: LocalGammaOwner
  -> ElabType
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withSourceOwnerConsumerCompletion owner expectedEndpoint packet = do
  certificate <-
    sourceOwnerConsumerCompletionCertificate
      owner
      expectedEndpoint
      packet
  pure
    packet
      { psgSourceOwnerConsumerCompletion = Just certificate
      }

-- | Seal the complete result endpoint of the enclosing source owner.  The
-- packet must consume that owner's exact result node; this prevents an
-- owner's final type from being reused as the bound of an unrelated nested
-- Gamma.  Later grouping still has to find a real requirement that already
-- publishes the endpoint.
withSourceOwnerFinalConsumerCompletion
  :: LocalGammaOwner
  -> ElabType
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withSourceOwnerFinalConsumerCompletion owner expectedEndpoint packet = do
  certificate <-
    sourceOwnerConsumerCompletionCertificate
      owner
      expectedEndpoint
      packet
  unless
    ( scaConsumerIdentity (soccAuthority certificate)
        == typeBinderIdentityFromNode (lgoTermNode owner)
    ) $
    completionFailure
      [ "the source-owner endpoint does not name the owner's exact result node"
      , "  owner result: " ++ show (lgoTermNode owner)
      , "  consumer identity: "
          ++ show (scaConsumerIdentity (soccAuthority certificate))
      ]
  pure
    packet
      { psgSourceOwnerFinalConsumerCompletion = Just certificate
      }
  where
    completionFailure details =
      Left
        ( ValidationFailed
            ( "invalid source-owner final consumer completion"
                : details
            )
        )

sourceOwnerConsumerCompletionCertificate
  :: LocalGammaOwner
  -> ElabType
  -> PreparedSubtermGeneralization
  -> Either ElabError SourceOwnerConsumerCompletion
sourceOwnerConsumerCompletionCertificate owner expectedEndpoint packet = do
  authority <-
    case psgConsumerAuthority packet of
      Just candidate -> pure candidate
      Nothing ->
        completionFailure
          ["the packet has no enclosing consumer authority"]
  actualOwner <-
    case subtermConsumerAuthorityEnclosingOwner authority of
      Just candidate -> pure candidate
      Nothing ->
        completionFailure
          [ "the packet consumer has no enclosing source owner"
          , "  authority: " ++ show authority
          ]
  unless (actualOwner == owner) $
    completionFailure
      [ "the paired source owner differs from the packet owner"
      , "  paired owner: " ++ show owner
      , "  packet owner: " ++ show actualOwner
      ]
  unless (isNothing (psgGammaAuthority packet)) $
    completionFailure
      [ "a packet-owned Gamma cannot use an enclosing source-owner completion"
      , "  Gamma authority: " ++ show (psgGammaAuthority packet)
      ]
  let consumerIdentity = scaConsumerIdentity authority
      endpointRefs =
        typeBinderDeclarationRefs expectedEndpoint
          ++ freeTypeVarRefsType expectedEndpoint
  unless
    ( all
        ((/= consumerIdentity) . typeBinderRefIdentity)
        endpointRefs
    ) $
    completionFailure
      [ "the source-owner endpoint retains the provisional consumer"
      , "  consumer identity: " ++ show consumerIdentity
      , "  expected endpoint: " ++ show expectedEndpoint
      ]
  case elabToBound expectedEndpoint of
    Right _ -> pure ()
    Left cause ->
      completionFailure
        [ "the source-owner endpoint is not a legal Gamma bound"
        , "  expected endpoint: " ++ show expectedEndpoint
        , "  cause: " ++ cause
        ]
  pure
    SourceOwnerConsumerCompletion
      { soccAuthority = authority
      , soccOwner = owner
      , soccFrozenOperatedType =
          schemeToType
            (siScheme (psgOperatedSchemeInfo packet))
      , soccExpectedEndpoint = expectedEndpoint
      }
  where
    completionFailure details =
      Left
        ( ValidationFailed
            ( "invalid source-owner consumer completion"
                : details
            )
        )

exactConsumerSpecializationFor
  :: ElabType
  -> PreparedSubtermGeneralization
  -> Maybe ExactConsumerSpecialization
exactConsumerSpecializationFor expectedEndpoint packet = do
  authority <- psgConsumerAuthority packet
  guard
    ( case authority of
        EnclosingGammaConsumerAuthority{} -> True
        TopologyConsumerAuthority{} -> True
        _ -> False
    )
  guard (psgGammaAuthority packet == Nothing)
  plan <-
    planExactBinderSpine
      exactTypesAgree
      constructionEndpoint
      expectedEndpoint
  guard
    ( -- A directly applied source lambda can consume the complete packet
      -- through a non-trivial leading-forall specialization while its
      -- operated endpoint is only the lambda body.  In that case the exact
      -- binder-spine plan above is the construction proof, and Gamma must
      -- still expose the complete construction that the plan consumes.
      exactTypesAgree gammaBoundEndpoint constructionEndpoint
        || ( exactTypesAgree expectedEndpoint operatedEndpoint
              && exactTypesAgree gammaBoundEndpoint expectedEndpoint
           )
        || ( exactTypesAgree expectedEndpoint constructionEndpoint
              && exactTypesAgree gammaBoundEndpoint expectedEndpoint
           )
        || maybe
          False
          ( \completedEndpoint ->
              exactTypesAgree expectedEndpoint completedEndpoint
                && exactTypesAgree gammaBoundEndpoint operatedEndpoint
          )
          sourceLambdaCompletedEndpoint
    )
  pure
    ExactConsumerSpecialization
      { ecsAuthority = authority
      , ecsExpectedEndpoint = expectedEndpoint
      , ecsPlan = plan
      }
  where
    constructionEndpoint =
      schemeToType (siScheme (psgSchemeInfo packet))
    operatedEndpoint =
      schemeToType (siScheme (psgOperatedSchemeInfo packet))
    sourceLambdaCompletedEndpoint = do
      parameter <- psgSourceLambdaParameter packet
      parameterRef <- slpLocalBinderRef parameter
      pure
        ( TForallRef
            parameterRef
            Nothing
            (TArrow (TVarRef parameterRef) operatedEndpoint)
        )
    gammaBoundEndpoint =
      schemeToType (psgGammaBoundScheme packet)
    exactTypesAgree left right =
      alphaEqTypePreservingStructuralBinders left right
        || churchRepresentationEqType left right

-- | Attach the source-tree proof for the lambda that constructs this packet's
-- value arrow after all identity projections have been applied.  A bare graph
-- parameter must still expose its exact unbounded declaration; a structured
-- source parameter must occur as the exact arrow domain.  The latter needs no
-- local type abstraction, but both forms own the same source-lambda result
-- construction used across an opaque result path.
withSourceLambdaParameter
  :: (NodeId -> [NodeId])
  -> NodeId
  -> NodeId
  -> Maybe ElabType
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withSourceLambdaParameter constructionRoutes lambdaNode parameterNode mbStructuredType packet =
  case mbStructuredType of
    Just parameterType ->
      case completedBody of
        TArrow domainType _
          | sourceTypesAgree domainType parameterType ->
              pure
                (recordSourceLambda parameterType Nothing)
        body ->
          sourceParameterFailure
            "structured parameter does not construct the value-arrow domain"
            [ "  certified parameter type: " ++ show parameterType
            , "  construction body: " ++ show body
            ]
    Nothing ->
      case
          IntMap.lookup
            (getNodeId parameterNode)
            (siSubstRefs completedInfo)
        of
          Nothing -> pure packet
          Just _ ->
            case completedBody of
              TArrow (TVarRef domainRef) _
                | [constructedParameterRef] <-
                    [ ref
                    | (ref, Nothing) <- completedBinders
                    , any
                        (typeBinderRefsSameIdentity ref)
                        parameterConstructionRefs
                    , typeBinderRefsSameIdentity ref domainRef
                    ] ->
                    pure
                      ( recordSourceLambda
                          (TVarRef constructedParameterRef)
                          (Just constructedParameterRef)
                      )
              body
                | [(_, Nothing)] <-
                    [ binder
                    | binder@(ref, _) <- completedBinders
                    , any
                        (typeBinderRefsSameIdentity ref)
                        parameterConstructionRefs
                    ] ->
                    sourceParameterFailure
                      "unbounded parameter route does not construct the value-arrow domain"
                      [ "  construction body: " ++ show body
                      , "  parameter construction refs: "
                          ++ show parameterConstructionRefs
                      , "  copied binder refs: "
                          ++ show (psgCopiedBinderRefs packet)
                      , "  placed copied-binder refs: "
                          ++ show (psgPlacedCopiedBinderRefs packet)
                      , "  construction binder renames: "
                          ++ show (psgConstructionBinderRenames packet)
                      , "  compiler-exact binder renames: "
                          ++ show (psgCompilerExactBinderRenames packet)
                      , "  inherited Gamma routes: "
                          ++ show (psgInheritedGammaRoutes packet)
                      ]
              _ -> pure packet
  where
    completedInfo = psgSchemeInfo packet
    completedScheme = siScheme completedInfo
    (completedBinders, completedBody) =
      splitForallsRefs (schemeToType completedScheme)
    parameterConstructionRefs =
      copiedClosure [] [parameterRef]
        ++ [ ref
           | (ref, _) <- completedBinders
           , Just node <- [typeBinderRefNode ref]
           , constructionRoutesIntersect node parameterNode
           ]
      where
        parameterRef =
          fromMaybe
            ( typeBinderRefFromIdentity
                (typeBinderIdentityFromNode parameterNode)
                ("t" ++ show (getNodeId parameterNode))
            )
            ( IntMap.lookup
                (getNodeId parameterNode)
                (siSubstRefs completedInfo)
            )
        copiedClosure _ [] = []
        copiedClosure seen (ref : pending)
          | typeBinderRefIdentity ref `elem` seen =
              copiedClosure seen pending
          | otherwise =
              ref
                : copiedClosure
                  (typeBinderRefIdentity ref : seen)
                  (directCopies ref ++ pending)
        directCopies ref =
          maybeToList
            ( Map.lookup
                (typeBinderRefIdentity ref)
                (psgCopiedBinderRefs packet)
            )
            ++ [ copiedRef
               | routes <- Map.elems (psgPlacedCopiedBinderRefs packet)
               , (sourceIdentity, copiedRef) <- routes
               , sourceIdentity == typeBinderRefIdentity ref
               ]
            ++ [ constructionRef
               | (sourceRef, constructionRef) <-
                   psgConstructionBinderRenames packet
                     ++ psgCompilerExactBinderRenames packet
               , typeBinderRefsSameIdentity sourceRef ref
               ]
        constructionRoutesIntersect left right =
          any
            (`elem` constructionRoutes right)
            (constructionRoutes left)
    sourceTypesAgree left right =
      alphaEqType left right || churchAwareEqType left right
    recordSourceLambda parameterType mbBinderRef =
      packet
        { psgSourceLambdaParameter =
            Just
              SourceLambdaParameter
                { slpLambdaNode = lambdaNode
                , slpParameterNode = parameterNode
                , slpParameterType = parameterType
                , slpLocalBinderRef = mbBinderRef
                }
        }
    sourceParameterFailure
      :: String
      -> [String]
      -> Either ElabError a
    sourceParameterFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid source lambda-parameter construction"
              , "  detail: " ++ detail
              , "  lambda: " ++ show lambdaNode
              , "  parameter: " ++ show parameterNode
              , "  completed scheme: " ++ show completedScheme
              , "  completed routes: " ++ show (siSubstRefs completedInfo)
              ]
                ++ context
            )
        )

-- | Publish the type constructed after this packet's own root RaiseMerge has
-- consumed its exact @S'(operated)@ bound.  The completed packet scheme is
-- the source before that computation (for example
-- @forall a > Int. Int -> a@); its stored Gamma bound is the exact producer
-- endpoint (@Int@).  Only the packet's edge/consumer/scope authority may join
-- those fields and eliminate the matching result binder.
--
-- Construction-source recovery calls this before an enclosing application
-- copies the producer into a new Gamma.  This keeps the new declaration
-- correct by construction instead of asking edge replay to repair a stale
-- completed-packet type after the child has already checked.
publishSubtermGammaConstructionSourceSchemeInfo
  :: EdgeId
  -> PreparedSubtermGeneralization
  -> SchemeInfo
  -> Either ElabError SchemeInfo
publishSubtermGammaConstructionSourceSchemeInfo producerEdge packet sourceSchemeInfo = do
  authority <-
    case psgGammaAuthority packet of
      Just packetAuthority
        | gpaEdgeId packetAuthority == producerEdge ->
            pure packetAuthority
      packetAuthority ->
        Left
          ( ValidationFailed
              [ "Gamma construction source publisher has no matching packet authority"
              , "  producer edge: " ++ show producerEdge
              , "  packet authority: " ++ show packetAuthority
              ]
          )
  let exactEndpoint = schemeToType (psgGammaBoundScheme packet)
  if any
      ( (== gpaConsumerIdentity authority)
          . typeBinderRefIdentity
      )
      (freeTypeVarRefsType exactEndpoint)
    then
      Left
        ( ValidationFailed
            [ "Gamma construction source endpoint retains its packet consumer"
            , "  packet authority: " ++ show authority
            , "  exact endpoint: " ++ show exactEndpoint
            ]
        )
    else do
      publishedScheme <-
        specializeSourceAtConsumer
          authority
          (gpaConsumerIdentity authority)
          exactEndpoint
          (siScheme sourceSchemeInfo)
      pure
        ( rebuildSchemeInfoFromRefSubst
            sourceSchemeInfo
            publishedScheme
            ( IntMap.filter
                ( (/= gpaConsumerIdentity authority)
                    . typeBinderRefIdentity
                )
                (siSubstRefs sourceSchemeInfo)
            )
        )
  where
    specializeSourceAtConsumer packetAuthority consumerIdentity exactTy scheme =
      case
          [ ref
          | (ref, _) <- leadingBinders
          , typeBinderRefIdentity ref == consumerIdentity
          ]
        of
        [] ->
          if any
              ((== consumerIdentity) . typeBinderRefIdentity)
              (freeTypeVarRefsType sourceTy)
            then
              Left
                ( ValidationFailed
                    [ "Gamma construction source consumer is free instead of quantified"
                    , "  producer edge: " ++ show producerEdge
                    , "  consumer identity: " ++ show consumerIdentity
                    , "  source scheme: " ++ show sourceTy
                    ]
                )
            else pure scheme
        [_] ->
          schemeFromType <$> specialize sourceTy
        matches ->
          Left
            ( ValidationFailed
                [ "Gamma construction source consumer is quantified more than once"
                , "  producer edge: " ++ show producerEdge
                , "  consumer identity: " ++ show consumerIdentity
                , "  matching binders: " ++ show matches
                , "  source scheme: " ++ show sourceTy
                ]
            )
      where
        sourceTy = schemeToType scheme
        (leadingBinders, _) = splitForallsRefs sourceTy

        specialize ty =
          case ty of
            TForallRef ref mbBound body
              | typeBinderRefIdentity ref == consumerIdentity ->
                  either
                    ( \cause ->
                        Left
                          ( ValidationFailed
                              [ "Gamma construction source endpoint does not satisfy its packet result bound"
                              , "  producer edge: " ++ show producerEdge
                              , "  packet authority: " ++ show packetAuthority
                              , "  consumer: " ++ show ref
                              , "  exact endpoint: " ++ show exactTy
                              , "  bound: " ++ show mbBound
                              , "  cause: " ++ show cause
                              ]
                          )
                    )
                    Right
                    ( applyInstantiation
                        ty
                        (instForLeadingTypeArgument ty exactTy)
                    )
              | otherwise ->
                  TForallRef ref mbBound <$> specialize body
            _ ->
              Left
                ( ValidationFailed
                    [ "Gamma construction source consumer disappeared during specialization"
                    , "  producer edge: " ++ show producerEdge
                    , "  packet authority: " ++ show packetAuthority
                    , "  source scheme: " ++ show sourceTy
                    ]
                )

renameInheritedGammaRoutes
  :: [(TypeBinderRef, TypeBinderRef)]
  -> Reify.InheritedGammaRoutes
  -> Reify.InheritedGammaRoutes
renameInheritedGammaRoutes renames =
  Reify.mapInheritedGammaRouteRefs renameRef
  where
    renameRef ref =
      case find (typeBinderRefsSameIdentity ref . fst) renames of
        Just (_, renamed) -> renamed
        Nothing -> ref

renamePlacedCopiedBinderRefs
  :: [(TypeBinderRef, TypeBinderRef)]
  -> Map.Map
      TypeBinderIdentity
      [(TypeBinderIdentity, TypeBinderRef)]
  -> Map.Map
      TypeBinderIdentity
      [(TypeBinderIdentity, TypeBinderRef)]
renamePlacedCopiedBinderRefs renames routes =
  Map.fromList
    [ ( typeBinderRefIdentity (renameRef targetRef)
      , [ ( typeBinderRefIdentity (renameRef sourceRef)
          , renameRef copiedRef
          )
        | (sourceIdentity, copiedRef) <- copiedRoutes
        , let sourceRef =
                typeBinderRefFromIdentity
                  sourceIdentity
                  (typeBinderIdentityStableName sourceIdentity)
        ]
      )
    | (targetIdentity, copiedRoutes) <- Map.toList routes
    , let targetRef =
            typeBinderRefFromIdentity
              targetIdentity
              (typeBinderIdentityStableName targetIdentity)
    ]
  where
    renameRef ref =
      case find (typeBinderRefsSameIdentity ref . fst) renames of
        Just (_, renamed) -> renamed
        Nothing -> ref

withCompilerExactBinderRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
withCompilerExactBinderRenames renames packet = do
  schemeInfo <- specializeConsumerScheme (psgSchemeInfo packet)
  consumerConstructionSchemeInfo <-
    specializeConsumerScheme (psgConsumerConstructionSchemeInfo packet)
  operatedSchemeInfo <-
    specializeConsumerScheme (psgOperatedSchemeInfo packet)
  gammaBoundScheme <-
    publishCompilerExactGammaBound (psgGammaBoundScheme packet)
  (localResultAuthority, localResultDischarge) <-
    classifyLocalResultAuthority
      (preparedLocalResultCandidate packet)
      schemeInfo
      gammaBoundScheme
  pure
    packet
      { psgSchemeInfo = schemeInfo
      , psgConsumerConstructionSchemeInfo = consumerConstructionSchemeInfo
      , psgOperatedSchemeInfo = operatedSchemeInfo
      , psgGammaBoundScheme = gammaBoundScheme
      , psgLocalResultAuthority = localResultAuthority
      , psgLocalResultDischarge = localResultDischarge
      , psgCompilerExactBinderRenames = renames
      , psgPlacedCopiedBinderRefs =
          renamePlacedCopiedBinderRefs
            renames
            (psgPlacedCopiedBinderRefs packet)
      , psgInheritedGammaRoutes =
          renameInheritedGammaRoutes
            renames
            (psgInheritedGammaRoutes packet)
      , psgClosedConsumerDischarge =
          closedConsumerDischarge
            (psgConsumerAuthority packet)
            (psgGammaAuthority packet)
            schemeInfo
            gammaBoundScheme
      }
  where
    consumerIdentity = scaConsumerIdentity <$> psgConsumerAuthority packet

    specializeConsumerScheme schemeInfo = do
      binders <- traverse specializeConsumerBinder (schemeBinderRefs scheme)
      pure
        ( rebuildSchemeInfoFromRefSubst
            schemeInfo
            (mkElabSchemeWithRefs binders (schemeBody scheme))
            (schemeInfoBinderRefSubst schemeInfo)
        )
      where
        scheme = siScheme schemeInfo

    specializeConsumerBinder binder@(ref, mbBound)
      | Just (typeBinderRefIdentity ref) == consumerIdentity = do
          bound <- traverse specializeConsumerBound mbBound
          pure (ref, bound)
      | otherwise = pure binder

    specializeConsumerBound bound = do
      specialized <-
        specializeCompilerExactType renames (tyToElab bound)
      either
        ( Left
            . ValidationFailed
            . pure
            . ("compiler-exact consumer specialization produced an invalid bound: " ++)
        )
        Right
        (elabToBound specialized)

    -- Internal packet views live in the construction identity domain.  The
    -- enclosing Gamma bound is the outward S'(operated) view, so publish its
    -- free construction references in the exact lexical domain.  A retained
    -- leading packet forall is instantiated only when that binder is the
    -- route's construction identity; unrelated leading binders retain their
    -- order and a route absent from this bound is irrelevant.
    publishCompilerExactGammaBound scheme =
      schemeFromType
        <$> foldM publishGammaRoute (schemeToType scheme) renames

    publishGammaRoute ty (sourceRef, constructionRef)
      | any
          (typeBinderRefsSameIdentity constructionRef)
          (freeTypeVarRefsType ty) =
          pure
            ( substTypeCaptureRef
                constructionRef
                (TVarRef sourceRef)
                ty
            )
      | any
          (typeBinderRefsSameIdentity sourceRef)
          (freeTypeVarRefsType ty) =
          pure ty
      | otherwise = do
          mbPublished <-
            instantiateMatchingLeadingBinderWith
              "compiler-exact Gamma-bound publication failed: "
              sourceRef
              constructionRef
              ty
          case mbPublished of
            Just published -> pure published
            Nothing -> pure ty

    instantiateMatchingLeadingBinderWith failurePrefix argumentRef binderRef ty =
      case ty of
        TForallRef ref mbBound body
          | typeBinderRefsSameIdentity ref binderRef ->
              Just
                <$> either
                  ( Left
                      . ValidationFailed
                      . pure
                      . (failurePrefix ++)
                      . show
                  )
                  Right
                  (applyInstantiation ty (InstApp (TVarRef argumentRef)))
          | otherwise -> do
              mbBody <-
                instantiateMatchingLeadingBinderWith
                  failurePrefix
                  argumentRef
                  binderRef
                  body
              pure (TForallRef ref mbBound <$> mbBody)
        _ -> pure Nothing

    specializeCompilerExactType exactRoutes ty =
      foldM specializeAtRoute ty exactRoutes

    specializeAtRoute ty (sourceRef, targetRef) =
      if any
          (typeBinderRefsSameIdentity targetRef)
          (freeTypeVarRefsType ty)
        then pure ty
        else
          if any
              (typeBinderRefsSameIdentity sourceRef)
              (freeTypeVarRefsType ty)
            then
              -- Source-binder projection may already have published this
              -- exact argument into S'(operated).  In that case there is no
              -- remaining construction-domain forall slot to instantiate.
              pure ty
            else do
              mbSpecialized <-
                instantiateMatchingLeadingBinderWith
                  "compiler-exact consumer specialization failed: "
                  targetRef
                  targetRef
                  ty
              case mbSpecialized of
                Just specialized -> pure specialized
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "compiler-exact consumer bound has no matching construction slot for its exact argument"
                        , "  bound: " ++ show ty
                        , "  source argument: " ++ show sourceRef
                        , "  construction argument: " ++ show targetRef
                        ]
                    )

subtermGeneralizationGammaAuthority
  :: PreparedSubtermGeneralization
  -> Maybe GammaPacketAuthority
subtermGeneralizationGammaAuthority = psgGammaAuthority

-- | Whether this packet owns the required Γ binders for an edge.  The marker
-- is provenance, not a type-shape inference: only the witness-authorized
-- non-administrative root RaiseMerge path sets it.
subtermGeneralizationOwnsGammaForEdge :: EdgeId -> PreparedSubtermGeneralization -> Bool
subtermGeneralizationOwnsGammaForEdge edgeId packet =
  case psgGammaAuthority packet of
    Just authority -> gpaEdgeId authority == edgeId
    Nothing -> False

-- | Recover the exact local-Gamma closure that is allowed to consume this
-- packet.  Consumer-only packets carry their enclosing constructor directly;
-- packet-owned Gamma consumers must instead agree with their own edge/scope
-- authority.  A caller that receives a closure from this function therefore
-- does not need to infer ownership from binder presence.
subtermGeneralizationLocalConsumerClosure
  :: IntMap.IntMap LocalGammaClosure
  -> PreparedSubtermGeneralization
  -> Maybe LocalGammaClosure
subtermGeneralizationLocalConsumerClosure closures packet = do
  consumerAuthority <- psgConsumerAuthority packet
  closure <-
    IntMap.lookup
      (getEdgeId (scaEdgeId consumerAuthority))
      closures
  if scaEdgeId consumerAuthority `elem` lgcEdgeIds closure
      && scaConsumerIdentity consumerAuthority == lgcConsumerIdentity closure
      && placementMatches consumerAuthority closure
    then Just closure
    else Nothing
  where
    placementMatches consumerAuthority closure
      | subtermConsumerAuthorityIsTopology consumerAuthority
          || subtermConsumerAuthorityIsRootGamma consumerAuthority = False
      | otherwise =
          case subtermConsumerAuthorityEnclosingOwner consumerAuthority of
            Just enclosingOwner ->
              enclosingOwner == lgcOwner closure
            Nothing ->
              case (lgcOwnerPendingScheme closure, psgGammaAuthority packet) of
                (Just _, Just gammaAuthority) ->
                  gpaEdgeId gammaAuthority `elem` lgcEdgeIds closure
                    && gpaConsumerIdentity gammaAuthority
                      == lgcConsumerIdentity closure
                    && lgoConstructor (lgcOwner closure) == LocalLambdaGamma
                    && genRef (gpaOwnerGen gammaAuthority)
                      == localGammaOwnerScope (lgcOwner closure)
                _ -> False

-- | Whether a packet carries the Γ authority for one lambda-body edge.
-- Such a packet is consumed at that lambda and must not also be installed as
-- a descendant bound by an enclosing generalization.
subtermGeneralizationOwnsGammaEdge :: PreparedSubtermGeneralization -> Bool
subtermGeneralizationOwnsGammaEdge packet =
  case psgGammaAuthority packet of
    Just _ -> True
    Nothing -> False

-- | Select the unique @S'(operated)@ bound consumed by an enclosing root
-- RaiseMerge exterior. Packet-owned quantifiers are closed here, while type
-- variables owned by the enclosing lexical Gamma deliberately remain free so
-- the binder planner records them as dependencies. Multiple packets for the
-- same exterior would make construction depend on traversal order, so reject
-- that shape at the ownership boundary.
subtermGeneralizationGammaBoundScheme
  :: PreparedSubtermGeneralization
  -> ElabScheme
subtermGeneralizationGammaBoundScheme = psgGammaBoundScheme

-- | Return the parameter type certified by an administrative source lambda
-- only when the packet still publishes its completed construction.  If an
-- exact xMLF specialization selected the operated endpoint instead, that
-- endpoint is the parameter authority and the graph declaration is dormant.
subtermGeneralizationSourceLambdaParameter
  :: PreparedSubtermGeneralization
  -> Maybe (NodeId, NodeId, ElabType)
subtermGeneralizationSourceLambdaParameter =
  sourceLambdaParameterWith
    (schemeToType . siScheme . psgSchemeInfo)

subtermGeneralizationApplicationSourceLambdaParameter
  :: (TypeBinderRef -> Bool)
  -> PreparedSubtermGeneralization
  -> Maybe (NodeId, NodeId, ElabType)
subtermGeneralizationApplicationSourceLambdaParameter canEliminate =
  sourceLambdaParameterWith
    (applicationConstructionTypeWithEliminableBinders canEliminate)

sourceLambdaParameterWith
  :: (PreparedSubtermGeneralization -> ElabType)
  -> PreparedSubtermGeneralization
  -> Maybe (NodeId, NodeId, ElabType)
sourceLambdaParameterWith completedConstruction packet = do
  parameter <- psgSourceLambdaParameter packet
  _ <- slpLocalBinderRef parameter
  guard
    ( alphaEqType
        (schemeToType (psgGammaBoundScheme packet))
        (completedConstruction packet)
    )
  pure
    ( slpLambdaNode parameter
    , slpParameterNode parameter
    , slpParameterType parameter
    )

-- | The exact term construction published for an administrative source
-- lambda.  Its consumer-facing Gamma bound may be a specialization of this
-- term (for example, the codomain beneath a directly applied wrapper), so the
-- result path must retain the packet's completed construction rather than
-- substituting the later consumer view for the source lambda itself.
subtermGeneralizationSourceLambdaResultConstruction
  :: PreparedSubtermGeneralization
  -> Maybe (NodeId, ElabType)
subtermGeneralizationSourceLambdaResultConstruction =
  sourceLambdaResultConstructionWith
    (schemeToType . siScheme . psgSchemeInfo)

subtermGeneralizationApplicationSourceLambdaResultConstruction
  :: (TypeBinderRef -> Bool)
  -> PreparedSubtermGeneralization
  -> Maybe (NodeId, ElabType)
subtermGeneralizationApplicationSourceLambdaResultConstruction canEliminate =
  sourceLambdaResultConstructionWith
    (applicationConstructionTypeWithEliminableBinders canEliminate)

sourceLambdaResultConstructionWith
  :: (PreparedSubtermGeneralization -> ElabType)
  -> PreparedSubtermGeneralization
  -> Maybe (NodeId, ElabType)
sourceLambdaResultConstructionWith completedConstruction packet = do
  parameter <- psgSourceLambdaParameter packet
  guard
    ( case slpLocalBinderRef parameter of
        Just _ -> True
        Nothing -> structuredParameterDependenciesOwned
    )
  pure
    ( slpLambdaNode parameter
    , completedConstruction packet
    )
  where
    completedType = schemeToType (siScheme (psgSchemeInfo packet))
    completedDeclarations = typeBinderDeclarationRefs completedType
    inheritedRefs =
      Reify.inheritedGammaRoutesLexicalRefs
        (psgInheritedGammaRoutes packet)
        ++ map
          Reify.inheritedGammaRouteRef
          ( Reify.inheritedGammaRoutesEntries
              (psgInheritedGammaRoutes packet)
          )
    structuredParameterDependenciesOwned =
      case psgSourceLambdaParameter packet of
        Nothing -> False
        Just parameter ->
          all
            ( \dependency ->
                any
                  (typeBinderRefsSameIdentity dependency)
                  (completedDeclarations ++ inheritedRefs)
            )
            (freeTypeVarRefsType (slpParameterType parameter))

-- | Select the completed result of a direct lambda application.  Consuming
-- the outer value arrow can make a construction-owned prefix binder vacuous
-- while retained binders beneath it remain live.  Source binders are part of
-- the source ABI and are therefore never removed.  This specialization is
-- intentionally application-local: the source lambda itself still publishes
-- the complete packet construction.  It is selected only when the packet's
-- typed consumer-facing Gamma already names that exact specialization; the
-- binder-spine plan alone is not authority to change an unrelated consumer.
-- A binder is eliminable only after its bound is fully constructed.  A
-- missing or Bottom-bearing bound is provisional checking topology, not
-- positive evidence for the application's completed endpoint.
applicationConstructionTypeWithEliminableBinders
  :: (TypeBinderRef -> Bool)
  -> PreparedSubtermGeneralization
  -> ElabType
applicationConstructionTypeWithEliminableBinders canEliminate packet
  | alphaEqType consumerTy completedTy = completedTy
  | alphaEqType consumerTy specializedTy
  , isJust
      ( planExactBinderSpine
          completedConstructionTypesAgree
          completedTy
          specializedTy
      ) =
      specializedTy
  | otherwise = completedTy
  where
    schemeInfo = psgSchemeInfo packet
    consumerTy = schemeToType (psgGammaBoundScheme packet)
    completedTy = schemeToType (siScheme schemeInfo)
    specializedTy = dropVacuousConstructionBinders completedTy
    sourceBinderRefs =
      IntMap.elems (siSourceBinderOrderRefs schemeInfo)
    constructionBinderRefs =
      IntMap.elems (siConstructionBinderOrderRefs schemeInfo)

    dropVacuousConstructionBinders ty =
      case ty of
        TForallRef ref mbBound body ->
          let specializedBody = dropVacuousConstructionBinders body
              retainedBinder = TForallRef ref mbBound specializedBody
           in if constructionOwns ref
                && not (sourceOwns ref)
                && canEliminate ref
                && maybe
                  False
                  (not . constructionEndpointContainsBottom . tyToElab)
                  mbBound
                && not (occursIn ref specializedBody)
                then specializedBody
                else retainedBinder
        _ -> ty

    constructionEndpointContainsBottom ty =
      case ty of
        TBottom -> True
        TArrow domainTy codomainTy ->
          constructionEndpointContainsBottom domainTy
            || constructionEndpointContainsBottom codomainTy
        TConWithIdentity _ _ args ->
          any constructionEndpointContainsBottom args
        TVarAppRef _ args ->
          any constructionEndpointContainsBottom args
        TForallRef _ mbBound bodyTy ->
          maybe
            False
            (constructionEndpointContainsBottom . tyToElab)
            mbBound
            || constructionEndpointContainsBottom bodyTy
        TMuRef _ bodyTy -> constructionEndpointContainsBottom bodyTy
        TVarRef {} -> False
        TBaseWithIdentity {} -> False

    constructionOwns ref =
      any (typeBinderRefsSameIdentity ref) constructionBinderRefs
    sourceOwns ref =
      any (typeBinderRefsSameIdentity ref) sourceBinderRefs
    occursIn ref =
      any
        (typeBinderRefsSameIdentity ref)
        . freeTypeVarRefsType

completedConstructionTypesAgree :: ElabType -> ElabType -> Bool
completedConstructionTypesAgree left right =
  alphaEqTypePreservingStructuralBinders left right
    || churchRepresentationEqType left right

subtermGeneralizationAdministrativeLambdaResultConstruction
  :: PreparedSubtermGeneralization
  -> Maybe (NodeId, ElabType)
subtermGeneralizationAdministrativeLambdaResultConstruction packet = do
  parameter <- psgSourceLambdaParameter packet
  _ <- slpLocalBinderRef parameter
  pure
    ( slpLambdaNode parameter
    , schemeToType (siScheme (psgSchemeInfo packet))
    )

-- | Select an administrative source-lambda result only when its returned
-- value contains a nested generated declaration owned by source provenance.
-- Such a declaration is not part of the lambda's leading graph Gamma and can
-- be alpha-freshened when the returned value is published.  The recursively
-- checked child must therefore confirm its final identity before the endpoint
-- becomes exact.  A packet containing only leading graph declarations has no
-- such claim; those declarations remain owned by the enclosing constructor.
subtermGeneralizationSourceStagedAdministrativeLambdaResultConstruction
  :: IntMap.IntMap TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Maybe (NodeId, ElabType)
subtermGeneralizationSourceStagedAdministrativeLambdaResultConstruction sourceSidecarRefs packet = do
  result@(_, resultTy) <-
    subtermGeneralizationAdministrativeLambdaResultConstruction packet
  guard
    ( administrativeLambdaResultNeedsOwnerConfirmation
        sourceSidecarRefs
        packet
        resultTy
    )
  pure result

-- | Whether the packet's consumer-facing bound carries a generated source
-- declaration beneath its leading Gamma spine.  This is the propagation
-- certificate used by enclosing lambda constructors: they may forward the
-- returned value without claiming the returned lambda's parameter ownership.
subtermGeneralizationGammaBoundHasNestedSourceDeclaration
  :: IntMap.IntMap TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Bool
subtermGeneralizationGammaBoundHasNestedSourceDeclaration sourceSidecarRefs packet =
  administrativeLambdaResultNeedsOwnerConfirmation
    sourceSidecarRefs
    packet
    (schemeToType (psgGammaBoundScheme packet))

schemeInPacketConstructionDomain
  :: PreparedSubtermGeneralization
  -> ElabScheme
  -> ElabScheme
schemeInPacketConstructionDomain packet scheme =
  mkElabSchemeWithRefs
    [ (ref, fmap (mapBoundType renameFreeType) mbBound)
    | (ref, mbBound) <- schemeBinderRefs scheme
    ]
    (renameFreeType (schemeBody scheme))
  where
    freeRefs = freeTypeVarRefsType (schemeToType scheme)
    constructionRenames =
      [ rename
      | rename@(sourceRef, _) <-
          subtermGeneralizationConstructionBinderRenames packet
      , any (typeBinderRefsSameIdentity sourceRef) freeRefs
      ]
    renameFreeType ty0 =
      foldl'
        (\ty (sourceRef, constructionRef) ->
          substTypeCaptureRef sourceRef (TVarRef constructionRef) ty
        )
        ty0
        constructionRenames

subtermGeneralizationGammaBoundSchemeForConsumer
  :: TypeBinderIdentity
  -> SubtermGeneralizations
  -> Either ElabError (Maybe ElabScheme)
subtermGeneralizationGammaBoundSchemeForConsumer consumerIdentity packets =
  fmap subtermGeneralizationGammaBoundScheme
    <$> subtermGeneralizationForConsumer consumerIdentity packets

subtermGeneralizationForConsumer
  :: TypeBinderIdentity
  -> SubtermGeneralizations
  -> Either ElabError (Maybe PreparedSubtermGeneralization)
subtermGeneralizationForConsumer consumerIdentity packets =
  case matchingPackets of
    [] -> Right Nothing
    [packet] -> Right (Just packet)
    matches ->
      Left
        ( ValidationFailed
            [ "multiple prepared subterm packets target one RaiseMerge exterior"
            , "  exterior identity: " ++ show consumerIdentity
            , "  packet count: " ++ show (length matches)
            , "  packet authorities: "
                ++ show
                  [ (psgConsumerAuthority packet, psgGammaAuthority packet)
                  | packet <- matches
                  ]
            ]
        )
  where
    matchingPackets =
      [ packet
      | packet <- Map.elems packets
      , subtermGeneralizationConsumerIdentity packet == Just consumerIdentity
      ]

-- | Select the packet that supplies one exact edge's RaiseMerge bound.
-- Multiple nested packets may legitimately quotient to the same exterior;
-- the witness edge, retained independently in the consumer/Gamma authority,
-- selects the construction owner without relying on traversal order.
subtermGeneralizationForConsumerAtEdge
  :: EdgeId
  -> TypeBinderIdentity
  -> SubtermGeneralizations
  -> Either ElabError (Maybe PreparedSubtermGeneralization)
subtermGeneralizationForConsumerAtEdge edgeId consumerIdentity packets =
  case edgePackets of
    -- Identity coincidence is not edge ownership.  Descendant packets can
    -- legitimately quotient to the same exterior while belonging to
    -- different lexical computations; borrowing either one here would make
    -- the root result route depend on traversal order.  A packet participates
    -- only when its consumer or Gamma authority names this exact edge.
    [] -> pure Nothing
    [packet] -> pure (Just packet)
    matches ->
      Left
        ( ValidationFailed
            [ "multiple prepared subterm packets own one RaiseMerge edge"
            , "  edge: " ++ show edgeId
            , "  exterior identity: " ++ show consumerIdentity
            , "  packet authorities: "
                ++ show
                  [ (psgConsumerAuthority packet, psgGammaAuthority packet)
                  | packet <- matches
                  ]
            ]
        )
  where
    edgePackets =
      [ packet
      | packet <- Map.elems packets
      , subtermGeneralizationConsumerIdentity packet == Just consumerIdentity
      , packetOwnsEdge packet
      ]

    packetOwnsEdge packet =
      subtermGeneralizationOwnsGammaForEdge edgeId packet
        || maybe
          False
          ((== edgeId) . scaEdgeId)
          (psgConsumerAuthority packet)

-- | Derive the exact scheme that this packet's consumer will copy.
--
-- An enclosing source constructor consumes the packet's completed
-- construction, including its lambda/application spine.  A packet-local,
-- topology, or root consumer instead names the result of the packet's own
-- operation and therefore consumes the operated endpoint.  The placement
-- authority decides between those two paper constructions while the source
-- boundary is still known; reconstructing the distinction later from the
-- resulting type shape loses administrative lambda arrows.
--
-- Packet preparation uses this same value to allocate copied binder
-- identities, so dependencies pulled from either view cannot appear later
-- without a corresponding fresh identity.
consumerGammaBoundSchemeFrom
  :: Maybe SubtermConsumerAuthority
  -> ElabScheme
  -> ElabScheme
  -> ElabScheme
consumerGammaBoundSchemeFrom consumerAuthority constructionScheme operatedScheme =
  case consumerAuthority of
    Just EnclosingGammaConsumerAuthority{} ->
      -- An enclosing source constructor consumes the packet's completed
      -- construction.  A proved xMLF specialization selects the operated
      -- endpoint, but it does not reopen dependencies already owned by the
      -- completed packet.  Close those dependencies here, while both schemes
      -- and their exact identities are present; a farther enclosing Gen must
      -- never have to recover them as ambient binders.
      case
          planExactBinderSpine
            alphaEqType
            (schemeToType constructionScheme)
            (schemeToType operatedScheme)
        of
          Just _ -> enclosingOperatedScheme
          Nothing -> constructionScheme
    _ ->
      mkElabSchemeWithRefs
        (selectedBinders ++ retainedOperatedBinders)
        (schemeBody operatedScheme)
  where
    constructionBinders = schemeBinderRefs constructionScheme
    -- A binder that remains free in the completed construction scheme belongs
    -- to the enclosing lexical Gamma.  Re-quantifying it inside the exterior
    -- bound would turn K's @forall beta. beta -> alpha@ into the incorrectly
    -- closed @forall alpha beta. beta -> alpha@ and leave no dependency for
    -- the outer @alpha@ binder.
    enclosingFreeRefs = freeTypeVarRefsType (schemeToType constructionScheme)
    retainedOperatedBinders =
      [ binder
      | binder@(ref, _) <- schemeBinderRefs operatedScheme
      , not (refMember ref enclosingFreeRefs)
      ]
    operatedType =
      schemeToType
        ( mkElabSchemeWithRefs
            retainedOperatedBinders
            (schemeBody operatedScheme)
        )
    rootRefs =
      [ constructionRef
      | freeRef <- freeTypeVarRefsType operatedType
      , not (refMember freeRef enclosingFreeRefs)
      , Just constructionRef <- [matchingConstructionRef freeRef]
      ]
    selectedRefs = dependencyClosure rootRefs
    selectedBinders =
      [ binder
      | binder@(ref, _) <- constructionBinders
      , refMember ref selectedRefs
      , not (refMember ref (map fst retainedOperatedBinders))
      ]

    -- Preserve the completed packet's dependency order while taking the
    -- selected operated declaration for binders common to both views.  The
    -- operated scheme can leave a construction-owned declaration free (for
    -- example an annotation binder closed by an intervening ETyAbs); that
    -- declaration is selected above and must remain inside this packet.
    enclosingOperatedScheme =
      mkElabSchemeWithRefs
        (constructionOrderedBinders ++ operatedOnlyBinders)
        (schemeBody operatedScheme)

    constructionOrderedBinders =
      [ fromMaybe constructionBinder (operatedBinderFor constructionRef)
      | constructionBinder@(constructionRef, _) <- constructionBinders
      , refMember constructionRef enclosingSelectedRefs
      ]

    operatedOnlyBinders =
      [ operatedBinder
      | operatedBinder@(operatedRef, _) <- retainedOperatedBinders
      , not (refMember operatedRef (map fst constructionBinders))
      ]

    enclosingSelectedRefs =
      selectedRefs ++ map fst retainedOperatedBinders

    operatedBinderFor constructionRef =
      find
        (typeBinderRefsSameIdentity constructionRef . fst)
        retainedOperatedBinders

    dependencyClosure refs =
      let dependencies =
            [ dependencyRef
            | (binderRef, Just bound) <- constructionBinders
            , refMember binderRef refs
            , freeRef <- freeTypeVarRefsType (tyToElab bound)
            , Just dependencyRef <- [matchingConstructionRef freeRef]
            ]
          refs' = foldr insertRef refs dependencies
       in if length refs' == length refs
            then refs
            else dependencyClosure refs'

    matchingConstructionRef ref =
      fst
        <$> find
          (typeBinderRefsSameIdentity ref . fst)
          constructionBinders

    refMember ref = any (typeBinderRefsSameIdentity ref)

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

-- | Select the packet-local operated view for a computation whose surrounding
-- construction Gamma already opens the packet's quantifiers.  This is not a
-- legal enclosing RaiseMerge bound; that boundary must use
-- 'subtermGeneralizationGammaBoundSchemeForConsumer' instead.
subtermGeneralizationOperatedSchemeForConsumer
  :: TypeBinderIdentity
  -> SubtermGeneralizations
  -> Either ElabError (Maybe ElabScheme)
subtermGeneralizationOperatedSchemeForConsumer consumerIdentity packets =
  fmap operatedScheme
    <$> subtermGeneralizationForConsumer consumerIdentity packets
  where
    operatedScheme packet =
      schemeInPacketConstructionDomain
        packet
        (siScheme (psgOperatedSchemeInfo packet))

-- | Build the exact application/root Γ obligations carried by a collection
-- of instantiation edges.  A terminal root RaiseMerge is the sole authority
-- for introducing its exterior Hyp binder; the binder's bound is the
-- bottom-up S'(operated) packet when one exists, otherwise the frozen source
-- graph.  The edge-local result root is only a substitution route after
-- presolution; it must not be mistaken for the operated type after a
-- RaiseMerge has made that result coincide with the exterior.
--
-- Combining edges here matters for Figure 15.3.5 applications: the function
-- and argument computations share one lexical Γ, so duplicate exterior
-- identities must agree on one bound and retain every edge-local result route
-- instead of being selected by traversal order.
generalizationRequirementsForRootEdges
  :: (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe ElabType)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootEdges identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges =
  generalizationRequirementsForRootEdgesAt
    PacketLocalRequirementBoundary
    []
    identityRepresentative
    constructionCanonical
    ga
    presolutionView
    edgeArtifacts
    sourceBinderRefs
    subtermPackets
    (map exactProducerEdge edges)

-- | The exact endpoint attached to a root instantiation edge carries one of
-- two distinct paper meanings.  A producer endpoint is @Typ(a')@ and still
-- has to be projected to @S'(operated)@ through its prepared packet.  An
-- operated endpoint was recovered from the checked source occurrence itself
-- and already /is/ the @S'(operated)@ used by the edge's Hyp declaration.
-- Keeping that distinction in the value prevents packet projection from
-- replacing an exact source construction with an older graph presentation.
data RootEdgeExactEndpoint
  = RootEdgeExactProducer !ElabType
  | RootEdgeExactOperated !ElabType
  deriving (Eq, Show)

rootEdgeExactEndpointType :: RootEdgeExactEndpoint -> ElabType
rootEdgeExactEndpointType endpoint =
  case endpoint of
    RootEdgeExactProducer ty -> ty
    RootEdgeExactOperated ty -> ty

mapRootEdgeExactEndpoint
  :: (ElabType -> ElabType)
  -> RootEdgeExactEndpoint
  -> RootEdgeExactEndpoint
mapRootEdgeExactEndpoint f endpoint =
  case endpoint of
    RootEdgeExactProducer ty -> RootEdgeExactProducer (f ty)
    RootEdgeExactOperated ty -> RootEdgeExactOperated (f ty)

exactProducerEdge
  :: (EdgeId, Maybe ElabType)
  -> (EdgeId, Maybe RootEdgeExactEndpoint)
exactProducerEdge (edgeId, mbEndpoint) =
  (edgeId, RootEdgeExactProducer <$> mbEndpoint)

-- | Positive evidence that a source-owned type binder was closed by an exact
-- lexical 'ETyAbsRef' before an enclosing construction consumes a prepared
-- packet.  The constructor is private: callers join the checked binder to at
-- least one source-sidecar occurrence with 'mkLexicalTypeAbsClosure'.  Packet
-- selection must independently retain the complete enclosing consumer route
-- before the binder may be closed inside its Gamma bound.
data LexicalTypeAbsClosure = LexicalTypeAbsClosure
  { ltacBinder :: !(TypeBinderRef, Maybe BoundType)
  , ltacSourceOccurrenceKeys :: !(NonEmpty Int)
  , ltacEnclosingConsumers :: !(NonEmpty SubtermConsumerKey)
  }
  deriving (Eq, Show)

-- | Join one checked lexical type abstraction to the exact source-sidecar
-- occurrences that used it and to the exact packet consumers whose source
-- constructors lexically enclose that abstraction.  A binder missing either
-- half of that construction proof is not packet-closure authority and is
-- deliberately ignored.
mkLexicalTypeAbsClosure
  :: (TypeBinderRef, Maybe BoundType)
  -> IntMap.IntMap TypeBinderRef
  -> [SubtermConsumerKey]
  -> Maybe LexicalTypeAbsClosure
mkLexicalTypeAbsClosure binder@(binderRef, _) sourceAuthorities enclosingConsumers = do
  occurrenceKeys <-
    NonEmpty.nonEmpty
      [ nodeKey
      | (nodeKey, sourceRef) <- IntMap.toList sourceAuthorities
      , typeBinderRefsSameIdentity binderRef sourceRef
      ]
  consumerKeys <-
    NonEmpty.nonEmpty
      (Set.toList (Set.fromList enclosingConsumers))
  pure
    LexicalTypeAbsClosure
      { ltacBinder = binder
      , ltacSourceOccurrenceKeys = occurrenceKeys
      , ltacEnclosingConsumers = consumerKeys
      }

-- | Build the obligations for a construction that may be running inside one
-- prepared packet.  Only that active packet has its quantifiers open in the
-- construction Gamma.  Every other matching packet is enclosed by this
-- construction and must contribute its closed operated scheme.  Making the
-- choice per edge prevents an enclosing application from interpreting a free
-- packet-local result variable through the solved graph, where its bound may
-- already have been erased to bottom.
generalizationRequirementsForRootEdgesInConstruction
  :: Set.Set SubtermConsumerKey
  -> [TypeBinderRef]
  -> IntMap.IntMap AmbientGammaAuthority
  -> (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe ElabType)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootEdgesInConstruction activeConsumerKeys ambientBinderRefs ambientGammaAuthorities identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  generalizationRequirementsForRootEdgesInConstructionWithLexicalClosures
    activeConsumerKeys
    ambientBinderRefs
    ambientGammaAuthorities
    []
    identityRepresentative
    constructionCanonical
    ga
    presolutionView
    edgeArtifacts
    sourceBinderRefs
    subtermPackets
    edges

-- | Construction-boundary requirements with exact lexical type-abstraction
-- closures produced by already checked descendants.  An inactive packet may
-- close one of its free source dependencies only when an enclosing completed
-- packet retains both the source occurrence and the selected packet's exact
-- consumer declaration.
generalizationRequirementsForRootEdgesInConstructionWithLexicalClosures
  :: Set.Set SubtermConsumerKey
  -> [TypeBinderRef]
  -> IntMap.IntMap AmbientGammaAuthority
  -> [LexicalTypeAbsClosure]
  -> (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe ElabType)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootEdgesInConstructionWithLexicalClosures activeConsumerKeys ambientBinderRefs ambientGammaAuthorities lexicalClosures identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  generalizationRequirementsForRootExactEdgesInConstructionWithLexicalClosures
    activeConsumerKeys
    ambientBinderRefs
    ambientGammaAuthorities
    lexicalClosures
    identityRepresentative
    constructionCanonical
    ga
    presolutionView
    edgeArtifacts
    sourceBinderRefs
    subtermPackets
    (map exactProducerEdge edges)

-- | Exact-endpoint form of
-- 'generalizationRequirementsForRootEdgesInConstructionWithLexicalClosures'.
generalizationRequirementsForRootExactEdgesInConstructionWithLexicalClosures
  :: Set.Set SubtermConsumerKey
  -> [TypeBinderRef]
  -> IntMap.IntMap AmbientGammaAuthority
  -> [LexicalTypeAbsClosure]
  -> (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe RootEdgeExactEndpoint)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootExactEdgesInConstructionWithLexicalClosures activeConsumerKeys ambientBinderRefs ambientGammaAuthorities lexicalClosures identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  requirements <-
    generalizationRequirementsForRootEdgesAt
      ( ConstructionRequirementBoundary
          activeConsumerKeys
          ambientGammaAuthorities
      )
      lexicalClosures
      identityRepresentative
      constructionCanonical
      ga
      presolutionView
      edgeArtifacts
      sourceBinderRefs
      subtermPackets
      edges
  pure
    requirements
      { grAmbientBinderRefs =
          foldr insertAmbientBinderRef
            (grAmbientBinderRefs requirements)
            ambientBinderRefs
      , grAmbientGammaAuthorities = ambientGammaAuthorities
      }
  where
    insertAmbientBinderRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

-- | Construction-boundary variant that preserves whether an exact edge value
-- is the producer @Typ(a')@ or an already checked source-occurrence
-- @S'(operated)@.  Callers should use this entrypoint only when they retain
-- that positive provenance from source recovery; ordinary exact-producer
-- tables continue through 'generalizationRequirementsForRootEdgesInConstruction'.
generalizationRequirementsForRootExactEdgesInConstruction
  :: Set.Set SubtermConsumerKey
  -> [TypeBinderRef]
  -> IntMap.IntMap AmbientGammaAuthority
  -> (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe RootEdgeExactEndpoint)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootExactEdgesInConstruction activeConsumerKeys ambientBinderRefs ambientGammaAuthorities identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  generalizationRequirementsForRootExactEdgesInConstructionWithLexicalClosures
    activeConsumerKeys
    ambientBinderRefs
    ambientGammaAuthorities
    []
    identityRepresentative
    constructionCanonical
    ga
    presolutionView
    edgeArtifacts
    sourceBinderRefs
    subtermPackets
    edges

-- | Build requirements at a boundary outside every supplied packet. The
-- packet's enclosing operated scheme closes packet-owned quantifiers but
-- leaves enclosing lexical identities free for the root binder planner. The
-- raw packet-local operated view can quantify those enclosing identities and
-- is valid only while constructing that packet.
generalizationRequirementsForEnclosingRootEdges
  :: (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe ElabType)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForEnclosingRootEdges identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges =
  generalizationRequirementsForEnclosingRootExactEdges
    identityRepresentative
    constructionCanonical
    ga
    presolutionView
    edgeArtifacts
    sourceBinderRefs
    subtermPackets
    (map exactProducerEdge edges)

-- | Exact-endpoint form of
-- 'generalizationRequirementsForEnclosingRootEdges'.  Use this while a
-- source constructor still retains positive provenance that an endpoint is
-- already @S'(operated)@ rather than the producer @Typ(a')@.
generalizationRequirementsForEnclosingRootExactEdges
  :: (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe RootEdgeExactEndpoint)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForEnclosingRootExactEdges identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges =
  generalizationRequirementsForRootEdgesAt
    EnclosingRequirementBoundary
    []
    identityRepresentative
    constructionCanonical
    ga
    presolutionView
    edgeArtifacts
    sourceBinderRefs
    subtermPackets
    edges

data RequirementSchemeBoundary
  = PacketLocalRequirementBoundary
  | EnclosingRequirementBoundary
  | ConstructionRequirementBoundary
      (Set.Set SubtermConsumerKey)
      (IntMap.IntMap AmbientGammaAuthority)

-- | Prove that a checked exact endpoint is a proper instantiation of a
-- completed descendant packet.  Packet selection remains the caller's
-- responsibility: it must first match the consumer identity and lexical
-- construction owner.  This helper validates only the type-level step.
--
-- The shared binder-spine planner retains target quantifiers in place and
-- emits 'InstUnder' when an intervening packet quantifier must be consumed.
-- A plan containing only alpha-renames and identity is not a specialization;
-- preserving that distinction keeps a body-only exact edge from stealing a
-- source-owned forall.
packetTypeSpecializesToExactEndpoint :: ElabType -> ElabType -> Bool
packetTypeSpecializesToExactEndpoint packetTy exactEndpoint =
  case planExactBinderSpine alphaEqType packetTy exactEndpoint of
    Just plan ->
      exactBinderSpineInstantiation plan /= InstId
    Nothing -> False

-- | Select the operated endpoint for a construction-owned Gamma requirement.
-- The caller supplies an exact endpoint only after proving it through either
-- the edge source or a direct ambient-Gamma node route.  A matching completed
-- packet still supplies placement ownership, but its closed forall must not
-- replace a checked specialization that this construction already owns.
selectConstructionRequirementEndpoint
  :: Maybe ElabType
  -> Maybe ElabScheme
  -> Maybe ElabType
selectConstructionRequirementEndpoint mbExactEndpoint mbPacketScheme =
  case (mbExactEndpoint, mbPacketScheme) of
    (Just exactEndpoint, Just packetScheme)
      | packetTypeSpecializesToExactEndpoint
          (schemeToType packetScheme)
          exactEndpoint ->
          Just exactEndpoint
    (_, Just packetScheme) -> Just (schemeToType packetScheme)
    (Just exactEndpoint, Nothing) -> Just exactEndpoint
    (Nothing, Nothing) -> Nothing

-- | Adopt the independently checked endpoint of a frozen occurrence only
-- when the raw packet has a direct identity route from that exact frozen TyVar
-- root.  The route is either the root identity itself, an exact construction
-- copy published by 'GaBindParents', or the root-keyed ambient declaration
-- produced before construction.  Its bound is deliberately not used here:
-- the endpoint comes from the independently checked edge/source path, while
-- this helper proves only that the raw packet is that occurrence.  No bare
-- quotient representative, source-binder sidecar, display name, or type shape
-- participates.
resolveFrozenOperatedOccurrenceEndpoint
  :: Constraint p
  -> IntMap.IntMap AmbientGammaAuthority
  -> [NodeId]
  -> NodeId
  -> ElabType
  -> ElabType
  -> Maybe ElabType
resolveFrozenOperatedOccurrenceEndpoint baseConstraint ambientAuthorities constructionRoutes operatedRoot exactEndpoint operatedType = do
  case lookupNodeIn (cNodes baseConstraint) operatedRoot of
    Just TyVar {} -> pure ()
    _ -> Nothing
  operatedRef <-
    case operatedType of
      TVarRef ref -> Just ref
      _ -> Nothing
  guard
    ( typeBinderRefNode operatedRef == Just operatedRoot
        || maybe False (`elem` constructionRoutes) (typeBinderRefNode operatedRef)
        || case IntMap.lookup (getNodeId operatedRoot) ambientAuthorities of
          Just authority ->
            typeBinderRefsSameIdentity operatedRef (agaExactRef authority)
          Nothing -> False
    )
  pure exactEndpoint

-- | Use a direct ambient-Gamma certificate to align one bare operated
-- occurrence with its independently checked exact endpoint.  The live node
-- is looked up exactly; the certificate producer has already joined that key
-- to an exact declaration in the ambient type environment.  Bound equality,
-- rather than representative or display-name equality, is the semantic
-- authority for adopting the endpoint.
resolveAmbientGammaOperatedEndpoint
  :: IntMap.IntMap AmbientGammaAuthority
  -> ElabType
  -> ElabType
  -> Maybe ElabType
resolveAmbientGammaOperatedEndpoint authorities exactEndpoint operatedType =
  case operatedType of
    TVarRef operatedRef -> do
      liveNode <- typeBinderRefNode operatedRef
      AmbientGammaAuthority {agaBound = ambientBound} <-
        IntMap.lookup (getNodeId liveNode) authorities
      guard
        ( alphaEqType ambientBound exactEndpoint
            || churchAwareEqType ambientBound exactEndpoint
        )
      pure exactEndpoint
    _ -> Nothing

generalizationRequirementsForRootEdgesAt
  :: RequirementSchemeBoundary
  -> [LexicalTypeAbsClosure]
  -> (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe RootEdgeExactEndpoint)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootEdgesAt boundary lexicalClosures identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  requirements <- traverse requirementForEdge edges
  packetDiagnostics <- traverse packetDiagnostic edges
  requiredBinders <-
    case mergeRequirementGroups (concat requirements) of
      Right binders -> pure binders
      Left cause ->
        Left
          ( ValidationFailed
              [ "root-edge Gamma requirements conflict before placement"
              , "  boundary: " ++ showRequirementBoundary boundary
              , "  input edges: " ++ show edges
              , "  packet selection: " ++ show packetDiagnostics
              , "  per-edge requirements: " ++ show requirements
              , "  per-edge trace summaries: "
                  ++ show
                    [ requirementTraceSummaries requirement
                    | requirement <- concat requirements
                    ]
              , "  cause: " ++ show cause
              ]
          )
  pure
    GeneralizationRequirements
      { grRequiredGammaBinders = requiredBinders,
        grSourceBinderRefs = sourceBinderRefs,
        grAmbientBinderRefs = [],
        grTermUsedRootBinderRefs = [],
        grAmbientGammaAuthorities =
          case boundary of
            ConstructionRequirementBoundary _ authorities -> authorities
            _ -> IntMap.empty,
        grLocallyClosedGammaNodes = IntSet.empty
      }
  where
    showRequirementBoundary requirementBoundary =
      case requirementBoundary of
        PacketLocalRequirementBoundary -> "packet-local"
        EnclosingRequirementBoundary -> "enclosing"
        ConstructionRequirementBoundary activeConsumerKeys authorities ->
          "construction(active="
            ++ show activeConsumerKeys
            ++ ", ambient="
            ++ show authorities
            ++ ")"

    -- Definition 15.3.1 gives one declaration to each named node in the
    -- owner's Gamma. Several nested computations can still RaiseMerge into
    -- that same exterior while carrying different intermediate S'(operated)
    -- presentations. Group first by the frozen exterior so the final
    -- presolution bound can select the declaration independently of edge
    -- traversal order.
    mergeRequirementGroups requirements0 =
      concat
        <$> traverse
          mergeRequirementGroup
          (foldl insertRequirementGroup [] requirements0)

    insertRequirementGroup groups requirement =
      case break (any (sameExterior requirement)) groups of
        (_, []) -> groups ++ [[requirement]]
        (before, matching : after) ->
          before ++ [matching ++ [requirement]] ++ after

    mergeRequirementGroup group = do
      mbActiveDeclaration <-
        sharedActiveConsumerDeclaration group
      case mbActiveDeclaration of
        Just declaration -> pure [declaration]
        Nothing -> do
          mbPacketOwnerDeclaration <-
            sharedPacketOwnerDeclaration group
          case mbPacketOwnerDeclaration of
            Just declaration -> pure [declaration]
            Nothing -> do
              mbSharedDeclaration <-
                sharedEnclosingConsumerDeclaration group
              case mbSharedDeclaration of
                Just declaration -> pure [declaration]
                Nothing -> foldM insertRequirement [] group

    -- Exactly one packet occurrence is open in a construction.  When every
    -- other requirement for the same exterior has a checked binder-spine
    -- specialization into that active endpoint, the active endpoint is the
    -- declaration materialized by the current source owner.  Retain all
    -- edge/result provenance on that declaration.  Consumer identity alone
    -- cannot make this choice because a closed descendant packet may share
    -- the same exterior.
    sharedActiveConsumerDeclaration group =
      case boundary of
        ConstructionRequirementBoundary activeConsumerKeys _ ->
          case viableActiveRequirements activeConsumerKeys group of
            selected : otherSelected
              | all
                  ( \other ->
                      alphaEqType
                        (rgbOperatedType selected)
                        (rgbOperatedType other)
                        || churchAwareEqType
                          (rgbOperatedType selected)
                          (rgbOperatedType other)
                  )
                  otherSelected ->
                  pure
                    ( Just
                        ( foldl
                            (flip mergeMatchingExterior)
                            selected
                            group
                        )
                    )
            _ -> pure Nothing
        _ -> pure Nothing

    viableActiveRequirements activeConsumerKeys group =
      [ candidate
      | candidate <- group
      , requirementIsActive activeConsumerKeys candidate
      , all
          (requirementConstructsEndpoint candidate)
          group
      ]

    -- A packet-local Gamma can be the exact lexical owner of earlier
    -- enclosing-consumer requirements for the same exterior.  Those earlier
    -- bounds are intermediate S'(operated) views built inside the packet;
    -- once the packet itself is present at the enclosing boundary, its
    -- completed declaration is the one Definition 15.3.1 publishes.  Match
    -- this ownership chain by edge, consumer identity, and generation scope,
    -- never by the coincidental shape or name of either bound.
    sharedPacketOwnerDeclaration group =
      case viablePacketOwners group of
        [selected] ->
          pure
            ( Just
                ( foldl
                    (flip mergeMatchingExterior)
                    selected
                    group
                )
            )
        _ -> pure Nothing

    viablePacketOwners group =
      [ candidate
      | candidate <- group
      , Just (candidateEdge, candidateIdentity, candidateScope) <-
          [packetOwnerAuthority candidate]
      , all
          (ownedByPacket candidateEdge candidateIdentity candidateScope)
          group
      ]

    packetOwnerAuthority requirement = do
      packet <- packetForRequirement requirement
      authority <- psgConsumerAuthority packet
      gammaAuthority <- psgGammaAuthority packet
      case authority of
        PacketGammaConsumerAuthority edgeId identity
          | edgeId == gpaEdgeId gammaAuthority
          , identity == gpaConsumerIdentity gammaAuthority ->
              Just (edgeId, identity, GenRef (gpaOwnerGen gammaAuthority))
        _ -> Nothing

    ownedByPacket packetEdge packetIdentity packetScope requirement =
      case packetForRequirement requirement >>= psgConsumerAuthority of
        Just authority
          | scaConsumerIdentity authority == packetIdentity ->
              case authority of
                PacketGammaConsumerAuthority edgeId _ ->
                  edgeId == packetEdge
                EnclosingGammaConsumerAuthority _ _ owner ->
                  lgoBoundaryEdge owner == packetEdge
                    && localGammaOwnerScope owner == packetScope
                _ -> False
        _ -> False

    packetForRequirement requirement =
      case NonEmpty.toList (rgbEdgeIds requirement) of
        [edgeId] ->
          either
            (const Nothing)
            id
            ( subtermGeneralizationForConsumerAtEdge
                edgeId
                ( typeBinderIdentityFromNode
                    (rgbExteriorNode requirement)
                )
                subtermPackets
            )
        _ -> Nothing

    requirementIsActive activeConsumerKeys requirement =
      any
        ( \edgeId ->
            Set.member
              ( subtermConsumerKey
                  edgeId
                  ( typeBinderIdentityFromNode
                      (rgbExteriorNode requirement)
                  )
              )
              activeConsumerKeys
        )
        (NonEmpty.toList (rgbEdgeIds requirement))

    requirementConstructsEndpoint target source =
      rgbPlacement source == rgbPlacement target
        && exactOccurrenceAuthoritiesAgree source target
        && ( alphaEqType
              (rgbOperatedType source)
              (rgbOperatedType target)
              || churchAwareEqType
                (rgbOperatedType source)
                (rgbOperatedType target)
              || exactBinderSpineEndpoint
                (rgbOperatedType source)
                (rgbOperatedType target)
           )

    -- A group of enclosing-consumer packets is a single construction only
    -- when every edge carries the same exact source owner and consumer
    -- identity. In that case the live presolution owns the final S(n) for the
    -- shared named exterior. Select the packet stage that publishes that
    -- bound and retain all edge/result provenance on that declaration.
    --
    -- This does not make arbitrary unlike bounds compatible. A missing owner
    -- certificate, a different placement, a conflicting exact occurrence, or
    -- the absence of a packet matching the final graph bound falls through to
    -- the strict pairwise merge below.
    sharedEnclosingConsumerDeclaration group =
      case group of
        [] -> pure Nothing
        firstRequirement : _ -> do
          mbOwners <- traverse enclosingOwnerForRequirement group
          let exterior = rgbExteriorNode firstRequirement
              expectedConsumer = typeBinderIdentityFromNode exterior
              samePlacement =
                all
                  ((== rgbPlacement firstRequirement) . rgbPlacement)
                  group
              exactOccurrencesAgree =
                all
                  (exactOccurrenceAuthoritiesAgree firstRequirement)
                  group
          case sequence mbOwners of
            Just (owner : owners)
              | all (== owner) owners
              , samePlacement
              , exactOccurrencesAgree
              , rootRaiseMergeExteriorOwnedByScope
                  ga
                  (localGammaOwnerScope owner)
                  exterior
              , all
                  (requirementConsumerIdentityIs expectedConsumer)
                  group ->
                  do
                    let mbSourceOwnerDeclaration =
                          sourceOwnerFinalDeclaration
                            exterior
                            owner
                            group
                            <|> sourceOwnerConsumerDeclaration
                              exterior
                              owner
                              group
                    mbFinalDeclaration <-
                      case mbSourceOwnerDeclaration of
                        Just declaration -> pure (Just declaration)
                        Nothing -> do
                          mbLiveDeclaration <-
                            finalExteriorDeclaration exterior group
                          pure
                            ( mbLiveDeclaration
                                <|> checkedOperatedDeclaration group
                            )
                    case mbFinalDeclaration of
                      Just selected ->
                        pure
                          ( Just
                              ( foldl
                                  (flip mergeMatchingExterior)
                                  selected
                                  group
                              )
                            )
                      Nothing -> pure Nothing
            _ -> pure Nothing

    -- A construction boundary can carry the checked source occurrence's
    -- exact S'(operated) for one forwarded edge while earlier edges for the
    -- same enclosing consumer still carry intermediate packet views.  Once
    -- the enclosing-owner proof above has established one owner, exterior,
    -- consumer identity, placement, and scope, that tagged endpoint is the
    -- declaration constructed by the source occurrence.  It is therefore a
    -- stronger final authority than comparing the intermediate type shapes.
    -- Multiple tagged endpoints are accepted only when they publish the same
    -- type; conflicting exact occurrences continue to the strict rejection.
    checkedOperatedDeclaration group =
      case filter carriesCheckedOperatedEndpoint group of
        [] -> Nothing
        selected : others
          | all
              (requirementPublishes (rgbOperatedType selected))
              others ->
              Just selected
        _ -> Nothing

    carriesCheckedOperatedEndpoint requirement =
      any edgePublishesRequirement
        (NonEmpty.toList (rgbEdgeIds requirement))
      where
        edgePublishesRequirement edgeId =
          case lookup edgeId edges of
            Just (Just (RootEdgeExactOperated exactTy)) ->
              requirementPublishes exactTy requirement
            _ -> False

    -- Packet preparation can recover the complete result of an enclosing
    -- source owner before recursive elaboration.  Every member of this group
    -- must carry that same exact certificate.  Prefer an edge that already
    -- publishes the certified endpoint; when every edge still exposes an
    -- intermediate S'(operated), construct the declaration at the sealed
    -- owner-final endpoint on one certified requirement and retain all real
    -- edge/result provenance during the enclosing merge.  This does not turn
    -- arbitrary unlike types into a graph requirement: each packet was sealed
    -- while the paired source owner, its frozen operated input, and the legal
    -- endpoint were simultaneously available.
    sourceOwnerFinalDeclaration exterior owner group = do
      completions <- traverse completionForRequirement group
      (template, (_, _, _, expectedEndpoint)) : remainingCompletions <-
        pure completions
      guard
        ( all
            ( \(_, (_, _, _, endpoint)) ->
                alphaEqType endpoint expectedEndpoint
                  || churchAwareEqType endpoint expectedEndpoint
            )
            remainingCompletions
        )
      pure
        ( fromMaybe
            template
              { rgbOperatedType = expectedEndpoint
              , rgbExactOperatedOccurrenceRef = Nothing
              }
            (find (requirementPublishes expectedEndpoint) group)
        )
      where
        expectedConsumer = typeBinderIdentityFromNode exterior

        completionForRequirement requirement = do
          packet <- packetForRequirement requirement
          completion@(authority, certifiedOwner, _, _) <-
            subtermGeneralizationSourceOwnerFinalConsumerCompletion packet
          guard (certifiedOwner == owner)
          guard (scaConsumerIdentity authority == expectedConsumer)
          guard
            ( scaEdgeId authority
                `elem` NonEmpty.toList (rgbEdgeIds requirement)
            )
          pure (requirement, completion)

    -- Several root edges can expose different construction stages of one
    -- enclosing lambda's Gamma declaration.  A source-owner consumer
    -- completion seals the exact frozen S'(operated) consumed by that lambda
    -- and the endpoint it constructs.  When every unlike stage carries such a
    -- certificate, all certificates agree on both endpoints, and one real
    -- requirement already publishes the frozen input, that requirement is the
    -- named-node declaration from Definition 15.3.1.  Select it by the
    -- edge/owner/consumer certificate; never infer this relation from the two
    -- type shapes.
    sourceOwnerConsumerDeclaration exterior owner group = do
      firstCompletion : remainingCompletions <- pure completions
      let (_, _, _, frozenOperatedType, expectedEndpoint) =
            firstCompletion
      guard
        ( all
            ( \(_, _, _, frozen, expected) ->
                typesAgree frozen frozenOperatedType
                  && typesAgree expected expectedEndpoint
            )
            remainingCompletions
        )
      selected : _ <-
        pure
          [ requirement
          | requirement <- group
          , requirementPublishes frozenOperatedType requirement
          ]
      guard
        ( all
            ( requirementIsPublishedInputOrCertified
                frozenOperatedType
            )
            group
        )
      pure selected
      where
        expectedConsumer = typeBinderIdentityFromNode exterior
        completions =
          [ (requirement, authority, certifiedOwner, frozen, expected)
          | requirement <- group
          , Just packet <- [packetForRequirement requirement]
          , Just (authority, certifiedOwner, frozen, expected) <-
              [subtermGeneralizationSourceOwnerConsumerCompletion packet]
          , certifiedOwner == owner
          , scaConsumerIdentity authority == expectedConsumer
          , scaEdgeId authority
              `elem` NonEmpty.toList (rgbEdgeIds requirement)
          ]

        requirementIsPublishedInputOrCertified frozen requirement =
          requirementPublishes frozen requirement
            || any
              (\(certified, _, _, _, _) -> sameRequirement certified requirement)
              completions

        sameRequirement left right =
          rgbEdgeIds left == rgbEdgeIds right
            && rgbOperatedRoot left == rgbOperatedRoot right

        typesAgree left right =
          alphaEqType left right || churchAwareEqType left right

    -- The live binding tree, rather than packet traversal order, identifies
    -- which frozen S'(operated) declaration is ultimately published for one
    -- shared exterior.  Ordinarily it must not replace that declaration:
    -- later graph solving can expose binders outside a bound whose
    -- packet-local coercion was constructed while those binders were still
    -- nested.  The vacuous-consumer case below is the narrow exception where
    -- every packet proves that its computation does not observe the bound.
    --
    -- Select the packet by its exact operated-root identity and retain its
    -- frozen type.  Type equality is only a fallback for cases where
    -- quotienting has moved the live bound away from that frozen root.  A
    -- second exact case is a packet whose frozen operated type is this
    -- requirement and whose validated completed scheme and Gamma bound are
    -- the live result.  That packet is the construction witness for the
    -- final state; no relationship is inferred from the two endpoint shapes.
    finalExteriorDeclaration exterior group =
      case pvLookupVarBound presolutionView exterior of
        Nothing -> pure Nothing
        Just boundRoot ->
          case find ((== boundRoot) . rgbOperatedRoot) group of
            Just requirement -> pure (Just requirement)
            Nothing ->
              case
                  reifyBoundWithRefs
                    presolutionView
                    IntMap.empty
                    exterior
                of
                Left _ -> pure Nothing
                Right finalExteriorBoundRaw ->
                  let finalExteriorBound =
                        normalizeFinalExteriorBound finalExteriorBoundRaw
                   in
                  case find
                    (requirementPublishes finalExteriorBound)
                    group
                  of
                    Just requirement -> pure (Just requirement)
                    Nothing ->
                      case
                          finalPacketCompletionDeclaration
                            boundRoot
                            finalExteriorBoundRaw
                            group
                        of
                          Just requirement -> pure (Just requirement)
                          Nothing ->
                            finalVacuousConsumerDeclaration
                              exterior
                              boundRoot
                              finalExteriorBound
                              group

    finalPacketCompletionDeclaration boundRoot liveBound group =
      case candidates of
        [(requirement, packetCompleted)] ->
          Just
            requirement
              { rgbOperatedRoot = boundRoot
              , rgbOperatedType = packetCompleted
              , rgbExactOperatedOccurrenceRef = Nothing
              }
        _ -> Nothing
      where
        candidates =
          [ (requirement, packetCompleted)
          | requirement <- group
          , boundRoot `elem` rgbResultRoots requirement
          , Just packet <- [packetForRequirement requirement]
          , let packetOperated =
                  schemeToType
                    (siScheme (psgOperatedSchemeInfo packet))
          , requirementPublishes packetOperated requirement
          , let packetCompleted =
                  schemeToType
                    (siScheme (psgSchemeInfo packet))
          , endpointTypesAgree liveBound packetCompleted
          , endpointTypesAgree
              packetCompleted
              (schemeToType (psgGammaBoundScheme packet))
          ]

        endpointTypesAgree left right =
          alphaEqType left right || churchAwareEqType left right

    -- The live exterior bound can still contain graph variables whose own
    -- solved lower bounds are structural.  Once the vacuous-consumer proof
    -- selects that live declaration, complete those graph occurrences before
    -- publishing the 'RequiredGammaBinder'.  Source and explicit ambient
    -- identities remain protected: they are declarations in the surrounding
    -- Gamma, whereas the other free graph variables are unfinished pieces of
    -- S(n), not independently bindable endpoints.
    normalizeFinalExteriorBound finalBound =
      inlineBoundVarsTypeWithCanonicalExcept
        protectedRefs
        constructionCanonical
        presolutionView
        finalBound
      where
        protectedRefs =
          [ ref
          | ref <- freeTypeVarRefsType finalBound
          , any
              (typeBinderRefsSameIdentity ref)
              protectedAuthorities
          ]
        protectedAuthorities =
          IntMap.elems sourceBinderRefs
            ++ case boundary of
              ConstructionRequirementBoundary _ authorities ->
                map agaExactRef (IntMap.elems authorities)
              _ -> []

    -- A shared exterior can advance past every packet-local operated snapshot
    -- when all of those packets erase the same unused consumer.  In that
    -- shape no packet computation observes the declaration's bound: each
    -- pending construction is exactly its completed packet after deleting
    -- one unbounded, vacuous consumer binder.  Definition 15.3.1 therefore
    -- takes the final live S(n) as the one declaration for the named
    -- exterior.  This is a construction proof, not a compatibility rule for
    -- unlike bounds; any used, bounded, duplicated, or incompletely routed
    -- consumer falls back to the strict pairwise rejection below.
    finalVacuousConsumerDeclaration exterior boundRoot finalBound group = do
      packetProofs <-
        traverse
          (requirementErasesVacuousConsumer exterior)
          group
      pure $ do
        guard (and packetProofs)
        guard
          (all
            (isNothing . rgbExactOperatedOccurrenceRef)
            group
          )
        guard
          (isJust
            ( lookupNodeIn
                (cNodes (gaBaseConstraint ga))
                boundRoot
            )
          )
        guard
          (not
            ( any
                ((== typeBinderIdentityFromNode exterior) . typeBinderRefIdentity)
                (freeTypeVarRefsType finalBound)
            )
          )
        template <- case group of
          firstRequirement : _ -> Just firstRequirement
          [] -> Nothing
        pure
          template
            { rgbOperatedRoot = boundRoot
            , rgbOperatedType = finalBound
            , rgbExactOperatedOccurrenceRef = Nothing
            }

    requirementErasesVacuousConsumer exterior requirement =
      case NonEmpty.toList (rgbEdgeIds requirement) of
        [edgeId] -> do
          mbPacket <-
            subtermGeneralizationForConsumerAtEdge
              edgeId
              (typeBinderIdentityFromNode exterior)
              subtermPackets
          pure
            ( maybe
                False
                (packetErasesVacuousConsumer exterior)
                mbPacket
            )
        _ -> pure False

    packetErasesVacuousConsumer exterior packet =
      case pendingConsumerBinders of
        [(pendingRef, Nothing)] ->
          routedConsumerIsPending pendingRef
            && not
              ( any
                  (typeBinderRefsSameIdentity pendingRef)
                  (freeTypeVarRefsType pendingTypeWithoutConsumer)
              )
            && null completedConsumerBinders
            && ( alphaEqType
                  pendingTypeWithoutConsumer
                  completedType
                   || churchAwareEqType
                        pendingTypeWithoutConsumer
                        completedType
               )
        _ -> False
      where
        consumerIdentity = typeBinderIdentityFromNode exterior
        pendingInfo =
          psgConsumerConstructionSchemeInfo packet
        completedInfo = psgSchemeInfo packet
        pendingScheme = siScheme pendingInfo
        completedScheme = siScheme completedInfo
        pendingConsumerRefs =
          consumerConstructionRefs consumerIdentity pendingInfo
        completedConsumerRefs =
          consumerConstructionRefs consumerIdentity completedInfo
        pendingConsumerBinders =
          consumerBinderCandidates pendingConsumerRefs pendingScheme
        completedConsumerBinders =
          consumerBinderCandidates completedConsumerRefs completedScheme
        pendingTypeWithoutConsumer =
          schemeToType
            ( mkElabSchemeWithRefs
                [ binder
                | binder@(ref, _) <- schemeBinderRefs pendingScheme
                , not
                    ( any
                        (typeBinderRefsSameIdentity ref)
                        pendingConsumerRefs
                    )
                ]
                (schemeBody pendingScheme)
            )
        completedType = schemeToType completedScheme
        routedConsumerIsPending pendingRef =
          case
              IntMap.lookup
                (getNodeId exterior)
                (schemeInfoBinderRefSubst pendingInfo)
            of
              Just routedRef ->
                typeBinderRefsSameIdentity pendingRef routedRef
              Nothing -> False

    consumerConstructionRefs consumerIdentity schemeInfo =
      directRef : maybeToList routedRef
      where
        directRef =
          typeBinderRefFromIdentity consumerIdentity "$consumer"
        routedRef = do
          consumerNode <- typeBinderRefNode directRef
          IntMap.lookup
            (getNodeId consumerNode)
            (schemeInfoBinderRefSubst schemeInfo)

    consumerBinderCandidates consumerRefs scheme =
      [ binder
      | binder@(ref, _) <- schemeBinderRefs scheme
      , any (typeBinderRefsSameIdentity ref) consumerRefs
      ]

    enclosingOwnerForRequirement requirement =
      case NonEmpty.toList (rgbEdgeIds requirement) of
        [edgeId] -> do
          mbPacket <-
            subtermGeneralizationForConsumerAtEdge
              edgeId
              (typeBinderIdentityFromNode (rgbExteriorNode requirement))
              subtermPackets
          pure $ do
            packet <- mbPacket
            authority <- psgConsumerAuthority packet
            owner <- subtermConsumerAuthorityEnclosingOwner authority
            guard (scaEdgeId authority == edgeId)
            guard
              ( scaConsumerIdentity authority
                  == typeBinderIdentityFromNode
                    (rgbExteriorNode requirement)
              )
            pure owner
        _ -> pure Nothing

    requirementConsumerIdentityIs expectedConsumer requirement =
      all
        (edgeConsumerIdentityIs expectedConsumer)
        (NonEmpty.toList (rgbEdgeIds requirement))

    edgeConsumerIdentityIs expectedConsumer edgeId =
      case
          subtermGeneralizationForConsumerAtEdge
            edgeId
            expectedConsumer
            subtermPackets
        of
        Right (Just packet) ->
          maybe
            False
            ((== expectedConsumer) . scaConsumerIdentity)
            (psgConsumerAuthority packet)
        _ -> False

    requirementPublishes finalExteriorBound requirement =
      alphaEqType
        finalExteriorBound
        (rgbOperatedType requirement)
        || churchAwareEqType
          finalExteriorBound
          (rgbOperatedType requirement)

    packetDiagnostic (edgeId, _) = do
      mbAuthority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
      case mbAuthority of
        Nothing -> pure (edgeId, Nothing)
        Just authority -> do
          mbPacket <-
            subtermGeneralizationForConsumerAtEdge
              edgeId
              (typeBinderIdentityFromNode (rrmaExterior authority))
              subtermPackets
          pure
            ( edgeId
            , ( \packet ->
                  ( psgConsumerAuthority packet
                  , psgGammaAuthority packet
                  , siScheme (psgSchemeInfo packet)
                  , siScheme (psgOperatedSchemeInfo packet)
                  , psgGammaBoundScheme packet
                  , siScheme (psgConsumerConstructionSchemeInfo packet)
                  , schemeInfoBinderRefSubst
                      (psgConsumerConstructionSchemeInfo packet)
                  , psgConstructionBinderRenames packet
                  , psgPlacedCopiedBinderRefs packet
                  )
              )
                <$> mbPacket
            )

    requirementForEdge (edgeId, exactEndpoint) = do
      authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
      case authority of
        Nothing -> pure []
        Just rootAuthority -> do
          selectedPacket <-
            subtermGeneralizationForConsumerAtEdge
              edgeId
              (typeBinderIdentityFromNode (rrmaExterior rootAuthority))
              subtermPackets
          packetOperatedScheme <-
            traverse
              (packetRequirementScheme rootAuthority)
              selectedPacket
          exactRequirementType <-
            traverse
              ( \endpoint ->
                  case endpoint of
                    RootEdgeExactOperated exactType -> pure exactType
                    RootEdgeExactProducer exactType ->
                      exactOperatedRequirementType
                        rootAuthority
                        selectedPacket
                        packetOperatedScheme
                        exactType
              )
              exactEndpoint
          let ambientRequirementType =
                constructionAmbientRequirementType rootAuthority
              selectedExactRequirementType =
                case exactRequirementType of
                  Just exactType -> Just exactType
                  Nothing -> ambientRequirementType
          operatedTypeRaw <-
            case
                ( boundary
                , selectedExactRequirementType
                , packetOperatedScheme
                )
              of
              ( ConstructionRequirementBoundary {}
                , Just exactType
                , _
                )
                  | exactEndpointIsOperated exactEndpoint ->
                      -- Source recovery has already checked this exact
                      -- occurrence as S'(operated).  It is the declaration
                      -- bound required by the local Hyp; an older packet is
                      -- placement/provenance evidence, not a competing type
                      -- endpoint.
                      pure exactType
              ( ConstructionRequirementBoundary activeConsumerKeys _
                , Just exactType
                , _
                )
                  | consumerPacketIsActive
                      activeConsumerKeys
                      rootAuthority ->
                      -- The active constructor has recursively built this
                      -- occurrence already.  Its checked source is therefore
                      -- the exact Typ(a) endpoint consumed by the local Hyp;
                      -- the packet still owns identity and placement, but its
                      -- earlier graph presentation must not replace that
                      -- construction-owned endpoint.
                      pure exactType
              (ConstructionRequirementBoundary {}, mbExactType, mbPacketScheme)
                | Just selected <-
                    selectConstructionRequirementEndpoint
                      mbExactType
                      mbPacketScheme ->
                    pure selected
              (_, _, Just scheme) -> pure (schemeToType scheme)
              (_, Just exactType, Nothing) -> pure exactType
              (_, Nothing, Nothing) ->
                case reifyFrozenOperatedRoot (rrmaOperatedRoot rootAuthority) of
                    Right ty -> pure ty
                    Left err ->
                      Left
                        ( ValidationFailed
                            [ "root RaiseMerge operated source has no reifiable S'(operated) bound",
                              "  edge: " ++ show edgeId,
                              "  authority: " ++ show rootAuthority,
                              "  operated root: " ++ show (rrmaOperatedRoot rootAuthority),
                              "  reify error: " ++ show err
                            ]
                        )
          sourceAlignedType <-
            either
              ( \cause ->
                  Left
                    ( ValidationFailed
                        [ "root RaiseMerge operated packet cannot be aligned to its construction Gamma"
                        , "  edge: " ++ show edgeId
                        , "  authority: " ++ show rootAuthority
                        , "  checked exact endpoint: "
                            ++ show exactEndpoint
                        , "  selected exact requirement type: "
                            ++ show selectedExactRequirementType
                        , "  selected packet: " ++ show selectedPacket
                        , "  packet operated scheme: "
                            ++ show packetOperatedScheme
                        , "  operated type: " ++ show operatedTypeRaw
                        , "  frozen operated node: "
                            ++ show
                              ( lookupNodeIn
                                  (cNodes baseConstraint)
                                  (rrmaOperatedRoot rootAuthority)
                              )
                        , "  source binder routes: " ++ show sourceBinderRefs
                        , "  direct ambient Gamma authorities: "
                            ++ show
                              ( case boundary of
                                  ConstructionRequirementBoundary _ authorities ->
                                    authorities
                                  _ -> IntMap.empty
                              )
                        , "  cause: " ++ cause
                        ]
                    )
              )
              Right
              ( case exactEndpoint of
                  Just (RootEdgeExactOperated exactType) ->
                    -- The endpoint constructor is the positive source
                    -- construction certificate.  No graph-shape comparison
                    -- or post-hoc source alias inference is needed here.
                    Right exactType
                  _ ->
                    case selectedExactRequirementType of
                      Just exactType ->
                        case boundary of
                          ConstructionRequirementBoundary _ ambientAuthorities
                            | Just aligned <-
                                resolveFrozenOperatedOccurrenceEndpoint
                                  baseConstraint
                                  ambientAuthorities
                                  ( gaConstructionRouteNodes
                                      constructionCanonical
                                      ga
                                      (rrmaOperatedRoot rootAuthority)
                                  )
                                  (rrmaOperatedRoot rootAuthority)
                                  exactType
                                  operatedTypeRaw ->
                                Right aligned
                          ConstructionRequirementBoundary {}
                            | Just packet <- selectedPacket
                            , Just packetScheme <- packetOperatedScheme
                            , subtermGeneralizationConsumerIdentity packet
                                == Just
                                  ( typeBinderIdentityFromNode
                                      (rrmaExterior rootAuthority)
                                  )
                            , schemeToType packetScheme == operatedTypeRaw ->
                                -- The selected packet and the checked edge meet
                                -- at the same exact RaiseMerge consumer identity.
                                -- That retained ownership route, rather than the
                                -- packet's possibly stale graph presentation,
                                -- authorizes the construction to use the checked
                                -- endpoint as its Gamma bound.
                                Right exactType
                          ConstructionRequirementBoundary _ ambientAuthorities
                            | Just aligned <-
                                resolveAmbientGammaOperatedEndpoint
                                  ambientAuthorities
                                  exactType
                                  operatedTypeRaw ->
                                Right aligned
                          EnclosingRequirementBoundary
                            | isJust selectedPacket -> do
                                -- The checked endpoint is Typ(a') after the outer
                                -- RaiseMerge computation, while an enclosing Gamma
                                -- entry must bind the selected packet's
                                -- S'(operated).  These need not have the same
                                -- surface shape: the endpoint can include lambda
                                -- domains outside the packet or consume packet
                                -- quantifiers.  Packet selection already proves
                                -- the exact edge, consumer identity, and lexical
                                -- owner, so preserve its operated scheme and use
                                -- the source sidecar only to align free identities.
                                resolveConstructionSourceBindersInTypeExcept
                                  ( ownedOperatedDeclarationIdentities
                                      rootAuthority
                                      packetOperatedScheme
                                      operatedTypeRaw
                                  )
                                  identityRepresentative
                                  sourceBinderRefs
                                  operatedTypeRaw
                          _
                            | isJust selectedPacket
                            , packetTypeSpecializesToExactEndpoint
                                operatedTypeRaw
                                exactType ->
                                -- The exact packet/consumer selection above owns
                                -- this edge.  When its closed S'(operated) packet
                                -- constructs the independently checked endpoint
                                -- through a non-identity binder-spine plan, retain
                                -- that endpoint as the Gamma bound.  Source-binder
                                -- alignment below proves identity equality only;
                                -- asking it to equate @sigma@ with the explicit
                                -- RaiseMerge result @forall (alpha > sigma). alpha@
                                -- would discard the paper computation that relates
                                -- them.
                                Right exactType
                          _ ->
                            resolveConstructionSourceBindersInPacketAtExpected
                              identityRepresentative
                              sourceBinderRefs
                              exactType
                              operatedTypeRaw
                      Nothing ->
                        resolveConstructionSourceBindersInTypeExcept
                          ( ownedOperatedDeclarationIdentities
                              rootAuthority
                              packetOperatedScheme
                              operatedTypeRaw
                          )
                          identityRepresentative
                          sourceBinderRefs
                          operatedTypeRaw
              )
          structurallyClosedType <-
            resolveStructuralSourceAliasesInType sourceAlignedType
          let constructionSourceRefs =
                [ freeRef
                | freeRef <- freeTypeVarRefsType structurallyClosedType
                , any
                    (typeBinderRefsSameIdentity freeRef)
                    (IntMap.elems sourceBinderRefs)
                ]
              producerRefProjection ref =
                inlineBoundVarsTypeWithCanonicalExcept
                  []
                  constructionCanonical
                  presolutionView
                  (TVarRef ref)
              producerRefProjectionIsClosed =
                null . freeTypeVarRefsType . producerRefProjection
              preparedPacketSchemes packet =
                [ siScheme (psgSchemeInfo packet)
                , siScheme (psgOperatedSchemeInfo packet)
                , psgGammaBoundScheme packet
                , siScheme (psgConsumerConstructionSchemeInfo packet)
                ]
              preparedPacketMentions ref =
                any
                  ( \packetScheme ->
                      let packetType = schemeToType packetScheme
                       in any
                            (typeBinderRefsSameIdentity ref)
                            ( typeBinderDeclarationRefs packetType
                                ++ freeTypeVarRefsType packetType
                            )
                  )
                  ( concatMap
                      preparedPacketSchemes
                      (Map.elems subtermPackets)
                  )
              selectedPacketCompletesProjection ref =
                case selectedPacket of
                  Nothing -> False
                  Just packet ->
                    case
                        [ bound
                        | packetScheme <-
                            [ siScheme (psgSchemeInfo packet)
                            , siScheme (psgOperatedSchemeInfo packet)
                            , psgGammaBoundScheme packet
                            ]
                        , (packetRef, Just bound) <-
                            schemeBinderRefs packetScheme
                        , typeBinderRefsSameIdentity packetRef ref
                        ]
                      of
                      [] -> False
                      bounds ->
                        all
                          ( \bound ->
                              alphaEqType
                                (tyToElab bound)
                                (producerRefProjection ref)
                          )
                          bounds
              exactRequirementProtectedRefs =
                case (exactEndpoint, selectedExactRequirementType) of
                  (Just (RootEdgeExactOperated exactType), _) ->
                    freeTypeVarRefsType exactType
                  (Just (RootEdgeExactProducer _), Just exactType@TVarRef {}) ->
                    freeTypeVarRefsType exactType
                  (Just (RootEdgeExactProducer _), Just exactType) ->
                    -- Typ(a') is exact as a whole, but a structured producer
                    -- can still contain result declarations owned by a child
                    -- construction.  Prepared packets are the positive
                    -- ownership authority for those nested identities.  Keep
                    -- any packet-owned occurrence unless the packet selected
                    -- by this exact edge publishes the same presolution bound
                    -- in all of its completed views.  A sibling or descendant
                    -- packet can prove ownership but cannot authorize this
                    -- edge to consume it.  An open projection must likewise
                    -- remain in place because moving its free dependencies
                    -- would cross a local Gamma/lambda boundary.  Projection
                    -- is therefore limited to closed, unowned occurrences or
                    -- closed completions certified by this edge's packet.
                    [ freeRef
                    | freeRef <- freeTypeVarRefsType exactType
                    , ( preparedPacketMentions freeRef
                          && not
                            (selectedPacketCompletesProjection freeRef)
                      )
                        || not (producerRefProjectionIsClosed freeRef)
                    ]
                  _ -> []
              operatedType =
                inlineBoundVarsTypeWithCanonicalExcept
                  (exactRequirementProtectedRefs ++ constructionSourceRefs)
                  constructionCanonical
                  presolutionView
                  structurallyClosedType
              exactOperatedOccurrenceRef =
                case (selectedExactRequirementType, operatedType) of
                  (Just (TVarRef exactRef), TVarRef operatedRef)
                    | typeBinderRefsSameIdentity exactRef operatedRef ->
                        Just operatedRef
                  _ -> Nothing
          pure
            [ RequiredGammaBinder
              { rgbEdgeIds = NonEmpty.singleton edgeId,
                rgbExteriorNode = rrmaExterior rootAuthority,
                rgbOperatedRoot = rrmaOperatedRoot rootAuthority,
                rgbResultRoots = NonEmpty.singleton (rrmaResultRoot rootAuthority),
                rgbOperatedType = operatedType,
                rgbExactOperatedOccurrenceRef =
                  exactOperatedOccurrenceRef,
                rgbPlacement = RequiredGammaAtCurrentScope
              }
            ]
      where
        packetRequirementScheme rootAuthority packet =
          case boundary of
            PacketLocalRequirementBoundary ->
              pure
                ( schemeInPacketConstructionDomain
                    packet
                    (siScheme (psgOperatedSchemeInfo packet))
                )
            EnclosingRequirementBoundary ->
              pure (subtermGeneralizationGammaBoundScheme packet)
            ConstructionRequirementBoundary activeConsumerKeys _
              | consumerPacketIsActive
                  activeConsumerKeys
                  rootAuthority ->
                  pure
                    ( schemeInPacketConstructionDomain
                        packet
                        (siScheme (psgOperatedSchemeInfo packet))
                    )
              | otherwise ->
                  closeLexicalPacketDependencies
                    packet
                    ( schemeInPacketConstructionDomain
                        packet
                        (subtermGeneralizationGammaBoundScheme packet)
                    )

        closeLexicalPacketDependencies packet scheme = do
          closedBinders <-
            foldM insertClosure [] lexicalClosures
          case closedBinders of
            [] -> pure scheme
            _ ->
              case
                  orderSourceProjectedSchemeBinders
                    "lexically closed packet Gamma bound"
                    ( mkElabSchemeWithRefs
                        (closedBinders ++ schemeBinderRefs scheme)
                        (schemeBody scheme)
                    )
                of
                  Right closedScheme -> pure closedScheme
                  Left cause ->
                    Left
                      ( ValidationFailed
                          [ "lexical type abstraction cannot close its prepared packet Gamma bound"
                          , "  packet authority: "
                              ++ show (psgConsumerAuthority packet)
                          , "  packet Gamma bound: " ++ show scheme
                          , "  lexical closures: " ++ show lexicalClosures
                          , "  cause: " ++ cause
                          ]
                      )
          where
            packetFreeRefs =
              freeTypeVarRefsType (schemeToType scheme)
            packetDeclaredRefs = map fst (schemeBinderRefs scheme)

            insertClosure closed closure
              | not (closureMatchesPacket closure) = pure closed
              | otherwise =
                  case find (sameBinder binder) closed of
                    Nothing -> pure (closed ++ [binder])
                    Just existing
                      | boundsAgree existing binder -> pure closed
                      | otherwise ->
                          Left
                            ( ValidationFailed
                                [ "one packet dependency is closed by conflicting lexical type abstractions"
                                , "  packet authority: "
                                    ++ show (psgConsumerAuthority packet)
                                , "  first declaration: " ++ show existing
                                , "  second declaration: " ++ show binder
                                ]
                            )
              where
                binder = ltacBinder closure

            closureMatchesPacket closure =
              let (binderRef, _) = ltacBinder closure
               in any (typeBinderRefsSameIdentity binderRef) packetFreeRefs
                    && not
                      ( any
                          (typeBinderRefsSameIdentity binderRef)
                          packetDeclaredRefs
                      )
                    && closureOwnsPacketConsumer closure

            closureOwnsPacketConsumer closure =
              case psgConsumerAuthority packet of
                Just authority ->
                  subtermConsumerAuthorityKey authority
                    `elem` NonEmpty.toList
                      (ltacEnclosingConsumers closure)
                Nothing -> False

            sameBinder (leftRef, _) (rightRef, _) =
              typeBinderRefsSameIdentity leftRef rightRef

            boundsAgree (_, leftBound) (_, rightBound) =
              let leftTy = maybe TBottom tyToElab leftBound
                  rightTy = maybe TBottom tyToElab rightBound
               in alphaEqType leftTy rightTy
                    || churchAwareEqType leftTy rightTy

        constructionAmbientRequirementType rootAuthority =
          case boundary of
            ConstructionRequirementBoundary activeConsumerKeys authorities
              | not
                  ( consumerPacketIsActive
                      activeConsumerKeys
                      rootAuthority
                  ) ->
                  agaBound
                    <$> IntMap.lookup
                      (getNodeId (rrmaExterior rootAuthority))
                      authorities
            _ -> Nothing

        closedPacketDeclarationIdentities rootAuthority mbPacketOperatedScheme =
          case mbPacketOperatedScheme of
            Just scheme
              | usesClosedOperatedScheme rootAuthority ->
                  Set.fromList
                    ( map
                        typeBinderRefIdentity
                        ( typeBinderDeclarationRefs
                            (schemeToType scheme)
                        )
                    )
            _ -> Set.empty

        -- Reifying a frozen operated bound can produce a leading forall whose
        -- child also has an exact source-sidecar identity.  The sidecar alone
        -- normally means that a leading unbounded forall is an inherited
        -- lexical capture and may be reopened.  A direct TyForall/TyMu
        -- binding-tree edge is stronger, local declaration authority: that
        -- binder must remain inside S'(operated), even when no descendant
        -- packet exists to publish the same fact.
        --
        -- Combine both construction proofs before source projection.  This
        -- keeps @pure :: forall a. a -> IO a@ closed at its required-Gamma
        -- bound while still reopening genuinely inherited source binders.
        ownedOperatedDeclarationIdentities rootAuthority mbPacketOperatedScheme operatedTy =
          Set.union
            ( closedPacketDeclarationIdentities
                rootAuthority
                mbPacketOperatedScheme
            )
            ( Set.fromList
                [ typeBinderRefIdentity declaration
                | declaration <- typeBinderDeclarationRefs operatedTy
                , frozenStructureOwnsDeclaration declaration
                ]
            )

        frozenStructureOwnsDeclaration declaration =
          case typeBinderRefNode declaration of
            Just node ->
              IntSet.member
                (getNodeId node)
                structuralSourceBinderKeys
            Nothing ->
              any
                ( \binderNode ->
                    maybe
                      False
                      (typeBinderRefsSameIdentity declaration)
                      (IntMap.lookup (getNodeId binderNode) sourceBinderRefs)
                )
                structuralSourceBinderNodes

        structuralSourceBinderNodes =
          concat (IntMap.elems structuralSourceBinderMap)

        -- A closed packet publishes S'(operated), whereas a compiler-exact
        -- lambda boundary records the source expression type.  When that
        -- source endpoint is the very lexical variable carried by the frozen
        -- operated root, the required Gamma bound is the variable's bound,
        -- not the variable itself.  The source sidecar is the construction
        -- proof connecting those two identity domains; reifying the operated
        -- root supplies the bound without guessing from a spelling.
        exactOperatedRequirementType rootAuthority mbPacket packetOperatedScheme exactType = do
          case boundary of
            ConstructionRequirementBoundary activeConsumerKeys _
              | consumerPacketIsActive
                  activeConsumerKeys
                  rootAuthority ->
                  -- This constructor has already checked the operated
                  -- occurrence at Typ(a).  Its packet still proves ownership
                  -- and placement, but an earlier S'(operated) snapshot (for
                  -- example Bottom before the lambda source is installed)
                  -- is not a second endpoint certificate.  Select Typ(a)
                  -- before packet publication validation, matching the
                  -- construction branch above.
                  pure
                    ( exactExteriorDeclarationBound
                        rootAuthority
                        exactType
                    )
            _ -> do
              packetEndpoint <-
                case (mbPacket, packetOperatedScheme) of
                  (Just packet, Just operatedScheme) ->
                    exactPacketOperatedEndpoint
                      rootAuthority
                      packet
                      operatedScheme
                      exactType
                  _ -> pure Nothing
              case packetEndpoint of
                Just operatedEndpoint -> pure operatedEndpoint
                Nothing ->
                  case exactType of
                    TVarRef exactRef
                      | usesClosedOperatedScheme rootAuthority
                      , not (packetAlreadyPublishesExactEndpoint packetOperatedScheme exactType)
                      , Just sourceRef <- sourceRefForNode (rrmaOperatedRoot rootAuthority)
                      , typeBinderRefsSameIdentity exactRef sourceRef ->
                          reifyFrozenOperatedSourceBound
                            exactRef
                            (rrmaOperatedRoot rootAuthority)
                    _ ->
                      pure
                        ( exactExteriorDeclarationBound
                            rootAuthority
                            exactType
                        )

        -- Typ(a') can already contain the exact declaration introduced by this
        -- root RaiseMerge, possibly beneath source-owned leading foralls.  In
        -- that case Gamma needs the declaration's bound S'(operated), not the
        -- whole completed endpoint.  Follow only the leading result spine and
        -- require the declared exterior to be returned as its own body; this is
        -- the identity-bearing construction form
        -- @forall (exterior >= S'(operated)). exterior@, not a type-shape
        -- approximation or forall-commuting rule.
        exactExteriorDeclarationBound rootAuthority exactType =
          fromMaybe exactType (go exactType)
          where
            exteriorIdentity =
              typeBinderIdentityFromNode (rrmaExterior rootAuthority)

            go ty =
              case ty of
                TForallRef ref mbBound body
                  | typeBinderRefIdentity ref == exteriorIdentity ->
                      case body of
                        TVarRef bodyRef
                          | typeBinderRefsSameIdentity ref bodyRef ->
                              Just (maybe TBottom tyToElab mbBound)
                        _ -> Nothing
                  | otherwise -> go body
                _ -> Nothing

        -- The exact-producer table records Typ(a'), whereas a root
        -- RaiseMerge Gamma binds S'(operated).  A prepared packet owns both
        -- endpoints and the consumer computation between them.  Join those
        -- fields only after the packet's complete producer has been aligned
        -- to the independently checked exact type; then replay the packet's
        -- own Gamma publication and require it to reconstruct the selected
        -- operated view.  This is the Figure 15.3.5 producer-to-edge
        -- transition, not a shape-based fallback.
        exactPacketOperatedEndpoint rootAuthority packet operatedScheme exactType =
          case psgGammaAuthority packet of
            Just packetAuthority
              | gpaEdgeId packetAuthority == edgeId
              , gpaConsumerIdentity packetAuthority
                  == typeBinderIdentityFromNode (rrmaExterior rootAuthority) ->
                  case
                      resolveConstructionSourceBindersInTypeAtExpected
                        identityRepresentative
                        sourceBinderRefs
                        exactType
                        packetProducerType
                    of
                      Left _ -> pure Nothing
                      Right _ -> do
                        published <-
                          publishSubtermGammaConstructionSourceSchemeInfo
                            edgeId
                            packet
                            (psgSchemeInfo packet)
                        let publishedType =
                              schemeToType
                                ( schemeInPacketConstructionDomain
                                    packet
                                    (siScheme published)
                                )
                            operatedType = schemeToType operatedScheme
                        case
                            resolveConstructionSourceBindersInTypeAtExpected
                              identityRepresentative
                              sourceBinderRefs
                              operatedType
                              publishedType
                          of
                            Right _ -> pure (Just operatedType)
                            Left cause ->
                              Left
                                ( ValidationFailed
                                    [ "prepared root RaiseMerge packet does not construct its selected operated endpoint"
                                    , "  edge: " ++ show edgeId
                                    , "  authority: " ++ show rootAuthority
                                    , "  exact producer: " ++ show exactType
                                    , "  packet producer: " ++ show packetProducerType
                                    , "  published producer: " ++ show publishedType
                                    , "  selected operated endpoint: " ++ show operatedType
                                    , "  cause: " ++ cause
                                    ]
                                )
            _ -> pure Nothing
          where
            packetProducerType =
              schemeToType
                ( schemeInPacketConstructionDomain
                    packet
                    (siScheme (psgSchemeInfo packet))
                )

        packetAlreadyPublishesExactEndpoint mbScheme exactType =
          case mbScheme of
            Nothing -> False
            Just scheme ->
              case
                  resolveConstructionSourceBindersInTypeAtExpected
                    identityRepresentative
                    sourceBinderRefs
                    exactType
                    (schemeToType scheme)
                of
                  Right _ -> True
                  Left _ -> False

        usesClosedOperatedScheme rootAuthority =
          case boundary of
            PacketLocalRequirementBoundary -> False
            EnclosingRequirementBoundary -> True
            ConstructionRequirementBoundary activeConsumerKeys _ ->
              not
                ( consumerPacketIsActive
                    activeConsumerKeys
                    rootAuthority
                )

        consumerPacketIsActive activeConsumerKeys rootAuthority =
          Set.member
            ( subtermConsumerKey
                edgeId
                (typeBinderIdentityFromNode (rrmaExterior rootAuthority))
            )
            activeConsumerKeys

        exactEndpointIsOperated mbEndpoint =
          case mbEndpoint of
            Just RootEdgeExactOperated {} -> True
            _ -> False

        sourceRefForNode node =
          case IntMap.lookup (getNodeId node) sourceBinderRefs of
            Just sourceRef -> Just sourceRef
            Nothing ->
              IntMap.lookup
                (getNodeId (identityRepresentative node))
                sourceBinderRefs

        reifyFrozenOperatedRoot operatedRoot =
          reifyBoundWithExternalRefsOnConstraint
            baseConstraint
            sourceVariableRefs
            externalSourceVariableKeys
            structuralSourceBinderMap
            operatedRoot

        reifyFrozenOperatedSourceBound sourceRef operatedRoot =
          reifyBoundWithExternalRefsOnConstraint
            baseConstraint
            sourceVariableRefsWithoutBinder
            (IntSet.fromList (IntMap.keys sourceVariableRefsWithoutBinder))
            structuralSourceBinderMap
            operatedRoot
          where
            sourceVariableRefsWithoutBinder =
              IntMap.filter
                (not . typeBinderRefsSameIdentity sourceRef)
                sourceVariableRefs

        baseConstraint = gaBaseConstraint ga

        -- Structural identities are evidence for rebuilding the binder at its
        -- owning mu/forall, not free variables inherited from construction
        -- Gamma.  Expanded aliases of a structural child therefore stay in
        -- the graph domain until 'resolveStructuralSourceAliasesInType'
        -- reifies their redirect owners in the frozen base graph.
        sourceVariableRefs =
          IntMap.filterWithKey
            (\nodeKey sourceRef ->
              not (IntSet.member nodeKey structuralSourceBinderKeys)
                && case typeBinderIdentityStructural (typeBinderRefIdentity sourceRef) of
                  Just _ -> False
                  Nothing -> True
                && case
                    lookupNodeIn
                      (cNodes (gaBaseConstraint ga))
                      (NodeId nodeKey)
                  of
                    Just TyVar {} -> True
                    _ -> False
            )
            sourceBinderRefs

        structuralSourceBinderMap =
          ReifyCore.structuralBinders
            baseConstraint
            (gaBindParentsBase ga)
            sourceBinderRefs

        structuralSourceBinderKeys =
          IntSet.fromList
            [ getNodeId binder
            | binders <- IntMap.elems structuralSourceBinderMap
            , binder <- binders
            ]

        externalSourceVariableKeys =
          IntSet.fromList (IntMap.keys sourceVariableRefs)
            `IntSet.difference` structuralSourceBinderKeys

        -- Only the actual structural binder children may carry structural
        -- source identities into owner reification.  Naming every expanded
        -- alias would turn the complete recursive owner back into a free self
        -- variable; naming none would replace recursive self occurrences with
        -- bottom.  Non-structural source variables remain external Gamma
        -- inputs to the reconstructed owner.
        sourceOwnerRefs =
          IntMap.union
            sourceVariableRefs
            (IntMap.restrictKeys sourceBinderRefs structuralSourceBinderKeys)

        resolveStructuralSourceAliasesInType ty =
          foldM resolveStructuralAlias ty (freeTypeVarRefsType ty)

        resolveStructuralAlias current graphRef =
          case structuralSourceRef graphRef of
            Nothing -> pure current
            Just sourceRef -> do
              graphNode <-
                case typeBinderRefNode graphRef of
                  Just node -> pure node
                  Nothing ->
                    Left
                      ( ValidationFailed
                          [ "structural source alias has no graph identity"
                          , "  edge: " ++ show edgeId
                          , "  alias: " ++ show graphRef
                          , "  source identity: " ++ show sourceRef
                          ]
                      )
              let redirectedOwner = constructionCanonical graphNode
              baseOwner <-
                -- Redirect targets are constructed in the frozen graph and
                -- may share their numeric key with a solved-to-base mapping.
                -- The target itself is the stronger owner proof when it is
                -- present; only a target outside the base graph needs the
                -- cross-phase projection.
                case lookupNodeIn (cNodes baseConstraint) redirectedOwner of
                  Just _ -> pure redirectedOwner
                  Nothing ->
                    case resolveGaSolvedToBase ga redirectedOwner of
                      SolvedToBaseMapped owner -> pure owner
                      SolvedToBaseSameDomain owner -> pure owner
                      SolvedToBaseMissing ->
                        Left
                          ( ValidationFailed
                              [ "structural source alias redirect owner has no frozen base node"
                              , "  edge: " ++ show edgeId
                              , "  alias: " ++ show graphRef
                              , "  source identity: " ++ show sourceRef
                              , "  redirected owner: " ++ show redirectedOwner
                              ]
                          )
              ownerType <-
                case
                    reifyBoundWithExternalRefsOnConstraint
                      baseConstraint
                      sourceOwnerRefs
                      externalSourceVariableKeys
                      structuralSourceBinderMap
                      baseOwner
                  of
                    Right reified -> pure reified
                    Left cause ->
                      Left
                        ( ValidationFailed
                            [ "structural source alias owner cannot be reified in the frozen base graph"
                            , "  edge: " ++ show edgeId
                            , "  alias: " ++ show graphRef
                            , "  source identity: " ++ show sourceRef
                            , "  redirected owner: " ++ show redirectedOwner
                            , "  base owner: " ++ show baseOwner
                            , "  cause: " ++ show cause
                            ]
                        )
              pure (substTypeCaptureRef graphRef ownerType current)

        structuralSourceRef graphRef = do
          graphNode <- typeBinderRefNode graphRef
          sourceRef <- IntMap.lookup (getNodeId graphNode) sourceBinderRefs
          case typeBinderIdentityStructural (typeBinderRefIdentity sourceRef) of
            Just _ -> Just sourceRef
            Nothing -> Nothing

    insertRequirement existing requirement =
      case find (sameExterior requirement) existing of
        Nothing -> pure (existing ++ [requirement])
        Just prior
          | not (exactOccurrenceAuthoritiesAgree prior requirement) ->
              Left
                ( ValidationFailed
                    [ "instantiation edges assign one Gamma exterior conflicting exact operated occurrences",
                      "  exterior: " ++ show (rgbExteriorNode requirement),
                      "  first occurrence: "
                        ++ show (rgbExactOperatedOccurrenceRef prior),
                      "  second occurrence: "
                        ++ show (rgbExactOperatedOccurrenceRef requirement)
                    ]
                )
          | rgbPlacement prior /= rgbPlacement requirement ->
              Left
                ( ValidationFailed
                    [ "instantiation edges assign one Gamma exterior to different construction scopes",
                      "  exterior: " ++ show (rgbExteriorNode requirement),
                      "  first placement: " ++ show (rgbPlacement prior),
                      "  second placement: " ++ show (rgbPlacement requirement)
                    ]
                )
          | alphaEqType
              (rgbOperatedType prior)
              (rgbOperatedType requirement) ->
              pure (map (mergeMatchingExterior requirement) existing)
          | exactExteriorSelfOccurrence prior ->
              -- An exact occurrence of the exterior at the exterior itself is
              -- the opened consumer endpoint, not a second lower bound.  The
              -- edge/occurrence certificate proves this is the paper's
              -- identity occurrence after N has opened the packet.  Retain the
              -- other edge's operated bound while preserving both provenance
              -- sets.
              pure (map (replaceMatchingExterior requirement) existing)
          | exactExteriorSelfOccurrence requirement ->
              pure (map (mergeMatchingExterior requirement) existing)
          | TBottom <- rgbOperatedType prior ->
              -- @exterior >= bottom@ contributes no lower-bound information.
              -- When another edge for the same exterior carries an exact
              -- non-bottom producer, retain that edge's operated-root
              -- provenance together with its stronger bound.
              pure (map (replaceMatchingExterior requirement) existing)
          | TBottom <- rgbOperatedType requirement ->
              pure (map (mergeMatchingExterior requirement) existing)
          | enclosingPacketPublishesExteriorBound prior requirement ->
              -- The enclosing owner's completed endpoint can already carry
              -- the exact Hyp declaration constructed by a descendant
              -- packet.  Its whole type is not a second lower bound for that
              -- same exterior: doing so would construct
              -- @exterior >= forall (exterior >= sigma). tau@.  The packet's
              -- edge, consumer identity, and enclosing-owner boundary prove
              -- that the nested declaration is this Gamma entry; publish its
              -- bound directly while both construction authorities coexist.
              pure (map (replaceMatchingExterior requirement) existing)
          | enclosingPacketPublishesExteriorBound requirement prior ->
              pure (map (mergeMatchingExterior requirement) existing)
          | exactFlexibleEndpoint
              (rgbOperatedType prior)
              (rgbOperatedType requirement) ->
              -- Two result-path edges can meet at one exterior before the
              -- later edge has opened its delayed flexible result.  The
              -- paper construction @forall (alpha > sigma). alpha@ is the
              -- shared declaration; its Hyp/Elim computation also supplies
              -- the earlier @sigma@ endpoint.  Retain that declaration and
              -- both edge authorities instead of treating the two stages as
              -- conflicting lower bounds.
              pure (map (replaceMatchingExterior requirement) existing)
          | exactFlexibleEndpoint
              (rgbOperatedType requirement)
              (rgbOperatedType prior) ->
              pure (map (mergeMatchingExterior requirement) existing)
          | exactBinderSpineEndpoint
              (rgbOperatedType prior)
              (rgbOperatedType requirement) ->
              -- Result-transparent constructors can forward several stages
              -- of one completed Gamma to the same enclosing owner.  Retain
              -- the declaration whose checked leading-binder computation
              -- constructs the incoming endpoint, while preserving every
              -- edge/result authority.  This covers ordinary unbounded
              -- specialization such as @forall a. a@ to @Bool@ without
              -- treating unrelated monomorphic bounds as compatible.
              pure (map (mergeMatchingExterior requirement) existing)
          | exactBinderSpineEndpoint
              (rgbOperatedType requirement)
              (rgbOperatedType prior) ->
              pure (map (replaceMatchingExterior requirement) existing)
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "instantiation edges require incompatible bounds for one Γ exterior",
                      "  exterior: " ++ show (rgbExteriorNode requirement),
                      "  first bound: " ++ show (rgbOperatedType prior),
                      "  second bound: " ++ show (rgbOperatedType requirement),
                      "  first edges: " ++ show (rgbEdgeIds prior),
                      "  second edges: " ++ show (rgbEdgeIds requirement),
                      "  first operated root: " ++ show (rgbOperatedRoot prior),
                      "  second operated root: " ++ show (rgbOperatedRoot requirement),
                      "  first frozen bounds: "
                        ++ show (frozenRequirementBounds prior),
                      "  second frozen bounds: "
                        ++ show (frozenRequirementBounds requirement),
                      "  first edge traces: "
                        ++ show (requirementTraceSummaries prior),
                      "  second edge traces: "
                        ++ show (requirementTraceSummaries requirement),
                      "  first enclosing-owner scope proof: "
                        ++ show (enclosingOwnerScopeProof prior),
                      "  second enclosing-owner scope proof: "
                        ++ show (enclosingOwnerScopeProof requirement),
                      "  first packet-completion proof: "
                        ++ show (packetCompletionProof prior),
                      "  second packet-completion proof: "
                        ++ show (packetCompletionProof requirement),
                      "  frozen exterior bound: "
                        ++ show
                          ( VarStore.lookupVarBound
                              (gaBaseConstraint ga)
                              (rgbExteriorNode requirement)
                          , reifyBoundWithRefsOnConstraint
                              (gaBaseConstraint ga)
                              IntMap.empty
                              (rgbExteriorNode requirement)
                          ),
                      "  live exterior bound: "
                        ++ show
                          ( pvLookupVarBound
                              presolutionView
                              (rgbExteriorNode requirement)
                          ),
                      "  live exterior reification: "
                        ++ show
                          ( reifyBoundWithRefs
                              presolutionView
                              IntMap.empty
                              (rgbExteriorNode requirement)
                          ),
                      "  first exact occurrence: "
                        ++ show (rgbExactOperatedOccurrenceRef prior),
                      "  second exact occurrence: "
                        ++ show (rgbExactOperatedOccurrenceRef requirement),
                      "  first placement: " ++ show (rgbPlacement prior),
                      "  second placement: " ++ show (rgbPlacement requirement)
                    ]
                )

    enclosingOwnerScopeProof requirement = do
      mbOwner <- enclosingOwnerForRequirement requirement
      pure
        ( (\owner ->
              ( owner
              , rootRaiseMergeExteriorOwnedByScope
                  ga
                  (localGammaOwnerScope owner)
                  (rgbExteriorNode requirement)
              )
          )
            <$> mbOwner
        )

    packetCompletionProof requirement = do
      boundRoot <-
        pvLookupVarBound
          presolutionView
          (rgbExteriorNode requirement)
      finalBoundRaw <-
        either
          (const Nothing)
          Just
          ( reifyBoundWithRefs
              presolutionView
              IntMap.empty
              (rgbExteriorNode requirement)
          )
      packet <- packetForRequirement requirement
      let finalBound = normalizeFinalExteriorBound finalBoundRaw
          packetOperated =
            schemeToType (siScheme (psgOperatedSchemeInfo packet))
          packetCompleted =
            schemeToType (siScheme (psgSchemeInfo packet))
          packetGammaBound = schemeToType (psgGammaBoundScheme packet)
          typesAgree left right =
            alphaEqType left right || churchAwareEqType left right
      pure
        ( boundRoot `elem` rgbResultRoots requirement
        , requirementPublishes packetOperated requirement
        , typesAgree finalBound packetCompleted
        , typesAgree finalBound packetGammaBound
        )

    sameExterior left right =
      rgbExteriorNode left == rgbExteriorNode right

    exactExteriorSelfOccurrence requirement =
      case
          ( rgbOperatedType requirement
          , rgbExactOperatedOccurrenceRef requirement
          )
        of
          (TVarRef operatedRef, Just exactRef) ->
            typeBinderRefsSameIdentity operatedRef exactRef
              && typeBinderRefIdentity exactRef
                == typeBinderIdentityFromNode
                  (rgbExteriorNode requirement)
          _ -> False

    exactFlexibleEndpoint source target =
      case target of
        TForallRef targetRef (Just targetBound) (TVarRef bodyRef) ->
          typeBinderRefsSameIdentity targetRef bodyRef
            && alphaEqType source (tyToElab targetBound)
            && isJust
              (planExactBinderSpine alphaEqType source target)
        _ -> False

    exactBinderSpineEndpoint source target =
      isJust (planExactBinderSpine alphaEqType source target)

    enclosingPacketPublishesExteriorBound ownerRequirement packetRequirement =
      case packetForRequirement packetRequirement >>= psgConsumerAuthority of
        Just
          ( EnclosingGammaConsumerAuthority
              consumerEdge
              consumerIdentity
              owner
            ) ->
            consumerEdge
              `elem` NonEmpty.toList (rgbEdgeIds packetRequirement)
              && lgoBoundaryEdge owner
                `elem` NonEmpty.toList (rgbEdgeIds ownerRequirement)
              && consumerIdentity == exteriorIdentity
              && case leadingExteriorBounds of
                [bound] ->
                  alphaEqType bound packetBound
                    || churchAwareEqType bound packetBound
                _ -> False
        _ -> False
      where
        exteriorIdentity =
          typeBinderIdentityFromNode
            (rgbExteriorNode ownerRequirement)
        packetBound = rgbOperatedType packetRequirement
        leadingExteriorBounds =
          go (rgbOperatedType ownerRequirement)

        go ty =
          case ty of
            TForallRef ref mbBound body ->
              [ tyToElab bound
              | typeBinderRefIdentity ref == exteriorIdentity
              , bound <- maybeToList mbBound
              ]
                ++ go body
            _ -> []

    frozenRequirementBounds requirement =
      [ ( ref
        , VarStore.lookupVarBound
            (gaBaseConstraint ga)
            node
        , reifyBoundWithRefsOnConstraint
            (gaBaseConstraint ga)
            subst
            node
        )
      | ref <- freeRefs
      , Just node <- [typeBinderRefNode ref]
      ]
      where
        freeRefs = freeTypeVarRefsType (rgbOperatedType requirement)
        subst =
          IntMap.fromList
            [ (getNodeId node, ref)
            | ref <- freeRefs
            , Just node <- [typeBinderRefNode ref]
            ]

    requirementTraceSummaries requirement =
      [ ( edgeId
        , etRoot traceInfo
        , etResultRoot traceInfo
        , etInterior traceInfo
        )
      | edgeId <- NonEmpty.toList (rgbEdgeIds requirement)
      , artifact <- maybeToList (lookupEdgeArtifact edgeId edgeArtifacts)
      , let traceInfo = edgeArtifactTrace artifact
      ]

    mergeMatchingExterior incoming prior
      | sameExterior incoming prior =
          prior
            { rgbEdgeIds =
                foldl
                  appendEdgeId
                  (rgbEdgeIds prior)
                  (NonEmpty.toList (rgbEdgeIds incoming)),
              rgbResultRoots =
                foldl
                  appendResultRoot
                  (rgbResultRoots prior)
                  (NonEmpty.toList (rgbResultRoots incoming)),
              rgbExactOperatedOccurrenceRef =
                case rgbExactOperatedOccurrenceRef prior of
                  Just ref -> Just ref
                  Nothing -> rgbExactOperatedOccurrenceRef incoming
            }
      | otherwise = prior

    replaceMatchingExterior incoming prior
      | sameExterior incoming prior =
          incoming
            { rgbEdgeIds =
                foldl
                  appendEdgeId
                  (rgbEdgeIds incoming)
                  (NonEmpty.toList (rgbEdgeIds prior)),
              rgbResultRoots =
                foldl
                  appendResultRoot
                  (rgbResultRoots incoming)
                  (NonEmpty.toList (rgbResultRoots prior))
            }
      | otherwise = prior

    appendResultRoot roots root
      | root `elem` roots = roots
      | otherwise = roots <> NonEmpty.singleton root

    appendEdgeId existingEdges edgeId
      | edgeId `elem` existingEdges = existingEdges
      | otherwise = existingEdges <> NonEmpty.singleton edgeId

    exactOccurrenceAuthoritiesAgree left right =
      case
          ( rgbExactOperatedOccurrenceRef left
          , rgbExactOperatedOccurrenceRef right
          )
        of
          (Just leftRef, Just rightRef) ->
            typeBinderRefsSameIdentity leftRef rightRef
          _ -> True

-- | Prepare one packet from an explicit legal placement. The construction
-- scheme wraps the completed lambda and closes its packet-owned quantifiers;
-- this is the complete @S'(operated)@ bound seen by an enclosing consumer.
-- The operated scheme is the open bottom-up view used by this packet's own
-- body computation. They deliberately remain distinct.
prepareSubtermGeneralizationPacket
  :: IdentityGenerator
  -> SubtermPacketPlacement
  -> SchemeInfo
  -> SchemeInfo
  -> Either ElabError (PreparedSubtermGeneralization, IdentityGenerator)
prepareSubtermGeneralizationPacket generator placement packet operatedPacket =
  do
    (consumerAuthority, gammaAuthority, localResultAuthority) <-
      packetPlacementAuthority placement
    prepareSubtermGeneralizationWithConsumer
      generator
      consumerAuthority
      gammaAuthority
      localResultAuthority
      packet
      operatedPacket

packetPlacementAuthority
  :: SubtermPacketPlacement
  -> Either
      ElabError
      ( Maybe SubtermConsumerAuthority
      , Maybe GammaPacketAuthority
      , Maybe SubtermConsumerAuthority
      )
packetPlacementAuthority placement =
  case placement of
    EnclosingConsumerPacket consumerIdentity consumerEdge owner ->
      pure
        ( Just
            ( EnclosingGammaConsumerAuthority
                consumerEdge
                consumerIdentity
                owner
            )
        , Nothing
        , Nothing
        )
    EnclosingConsumerGammaPacket consumerIdentity consumerEdge owner gammaAuthority ->
      pure
        ( Just
            ( EnclosingGammaConsumerAuthority
                consumerEdge
                consumerIdentity
                owner
            )
        , Just gammaAuthority
        , Nothing
        )
    TopologyConsumerPacket topologyAuthority ->
      pure
        ( Just (TopologyConsumerAuthority topologyAuthority)
        , Nothing
        , Nothing
        )
    TopologyConsumerGammaPacket topologyAuthority gammaAuthority ->
      pure
        ( Just (TopologyConsumerAuthority topologyAuthority)
        , Just gammaAuthority
        , Nothing
        )
    RootConsumerPacket consumerIdentity consumerEdge ->
      pure
        ( Just
            ( RootGammaConsumerAuthority
                consumerEdge
                consumerIdentity
            )
        , Nothing
        , Nothing
        )
    RootConsumerGammaPacket consumerIdentity consumerEdge gammaAuthority ->
      pure
        ( Just
            ( RootGammaConsumerAuthority
                consumerEdge
                consumerIdentity
            )
        , Just gammaAuthority
        , Nothing
        )
    GammaPacket gammaAuthority ->
      pure
        ( Just
            ( PacketGammaConsumerAuthority
                (gpaEdgeId gammaAuthority)
                (gpaConsumerIdentity gammaAuthority)
            )
        , Just gammaAuthority
        , Nothing
        )
    WithLocalTopologyResult base topologyAuthority -> do
      (consumerAuthority, gammaAuthority, localResultAuthority) <-
        packetPlacementAuthority base
      case (gammaAuthority, localResultAuthority) of
        (Nothing, Nothing) ->
          pure
            ( consumerAuthority
            , gammaAuthority
            , Just (TopologyConsumerAuthority topologyAuthority)
            )
        (Just gamma, _) ->
          Left
            ( ValidationFailed
                [ "local topology result cannot share a packet Gamma authority"
                , "  Gamma authority: " ++ show gamma
                , "  topology edge: " ++ show (itcaEdgeId topologyAuthority)
                ]
            )
        (_, Just existing) ->
          Left
            ( ValidationFailed
                [ "subterm packet has multiple local result authorities"
                , "  existing: " ++ show existing
                , "  incoming topology edge: "
                    ++ show (itcaEdgeId topologyAuthority)
                ]
            )
    DirectPacket ->
      pure (Nothing, Nothing, Nothing)

-- | Certify the sole legal no-binder presentation of a topology consumer.
-- The exact edge/owner authority proves which construction would
-- have consumed the result; the complete closed packet proves that the result
-- identity was eliminated rather than dropped; and either equality or an
-- exact non-trivial binder-spine specialization from the prepared
-- construction
-- endpoint proves that skipping placement cannot hide a pending computation.
--
-- The certificate is deliberately private and stores both prepared views.
-- Later packet transformations must recompute it instead of preserving a
-- Boolean whose construction evidence may have changed.
closedConsumerDischarge
  :: Maybe SubtermConsumerAuthority
  -> Maybe GammaPacketAuthority
  -> SchemeInfo
  -> ElabScheme
  -> Maybe ClosedConsumerDischarge
closedConsumerDischarge consumerAuthority gammaAuthority schemeInfo constructionScheme =
  case (consumerAuthority, gammaAuthority) of
    (Just authority, Nothing)
      | consumerCanBeAdministrativelyClosed authority
      , null consumerBinderOccurrences
      , ordinaryClosedDischarge
          || exactAmbientClosureAfterVacuousConsumer ->
          Just
            ClosedConsumerDischarge
              { ccdAuthority = authority
              , ccdSchemeInfo = schemeInfo
              , ccdConstructionScheme = constructionScheme
              }
      | otherwise -> Nothing
    _ -> Nothing
  where
    consumerCanBeAdministrativelyClosed authority =
      case authority of
        TopologyConsumerAuthority{} -> True
        EnclosingGammaConsumerAuthority{} -> False
        RootGammaConsumerAuthority{} -> False
        PacketGammaConsumerAuthority{} -> False

    completeType = schemeToType (siScheme schemeInfo)
    constructionType = schemeToType constructionScheme
    ordinaryClosedDischarge =
      null (freeTypeVarRefsType completeType)
        && ( alphaEqType completeType constructionType
              || packetTypeSpecializesToExactEndpoint
                constructionType
                completeType
           )
    -- A crossed lambda can consume its topology result while leaving an
    -- enclosing lambda parameter free in the emitted packet.  The original
    -- construction then contains two distinct facts: a vacuous declaration
    -- for the consumed result and an exact forall closure of every surviving
    -- ambient dependency.  Remove only that authority-selected vacuous
    -- declaration and require the remaining spine to close exactly the free
    -- identities in the packet.  This preserves the lexical construction
    -- proof without treating an arbitrary open packet as discharged.
    exactAmbientClosureAfterVacuousConsumer =
      case consumerDeclarations of
        [(consumerRef, _)]
          | not (consumerOccursAfterDeclaration consumerRef) ->
              exactIdentityClosure
                constructionWithoutConsumer
                completeType
        _ -> False
    constructionSchemeView = schemeFromType constructionType
    constructionBinders = schemeBinderRefs constructionSchemeView
    consumerDeclarations =
      [ binder
      | binder@(binderRef, _) <- constructionBinders
      , any (typeBinderRefsSameIdentity binderRef) consumerRefs
      ]
    constructionWithoutConsumer =
      schemeToType
        ( mkElabSchemeWithRefs
            [ binder
            | binder@(binderRef, _) <- constructionBinders
            , not
                ( any
                    (typeBinderRefsSameIdentity binderRef)
                    consumerRefs
                )
            ]
            (schemeBody constructionSchemeView)
        )
    consumerOccursAfterDeclaration consumerRef =
      any
        (typeBinderRefsSameIdentity consumerRef)
        (freeTypeVarRefsType constructionWithoutConsumer)
    exactIdentityClosure closed endpoint =
      peelClosure [] closed
      where
        endpointFreeRefs = freeTypeVarRefsType endpoint
        peelClosure closedRefs current
          | not (null closedRefs)
          , alphaEqType current endpoint =
              sameRefSet closedRefs endpointFreeRefs
          | TForallRef ref _ body <- current =
              peelClosure (ref : closedRefs) body
          | otherwise = False
        sameRefSet left right =
          all (\ref -> any (typeBinderRefsSameIdentity ref) right) left
            && all (\ref -> any (typeBinderRefsSameIdentity ref) left) right
    consumerIdentity = scaConsumerIdentity <$> consumerAuthority
    directConsumerRef = do
      identity <- consumerIdentity
      pure
        ( typeBinderRefFromIdentity
            identity
            (typeBinderIdentityStableName identity)
        )
    routedConsumerRef = do
      directRef <- directConsumerRef
      consumerNode <- typeBinderRefNode directRef
      IntMap.lookup
        (getNodeId consumerNode)
        (siSubstRefs schemeInfo)
    consumerRefs = maybeToList directConsumerRef ++ maybeToList routedConsumerRef
    consumerBinderOccurrences =
      [ binderRef
      | binderRef <- boundRefsInType completeType
      , any (typeBinderRefsSameIdentity binderRef) consumerRefs
      ]

preparedLocalResultCandidate
  :: PreparedSubtermGeneralization
  -> Maybe SubtermConsumerAuthority
preparedLocalResultCandidate packet =
  case psgLocalResultAuthority packet of
    Just authority -> Just authority
    Nothing ->
      ccdAuthority <$> psgLocalResultDischarge packet

classifyLocalResultAuthority
  :: Maybe SubtermConsumerAuthority
  -> SchemeInfo
  -> ElabScheme
  -> Either
      ElabError
      ( Maybe SubtermConsumerAuthority
      , Maybe ClosedConsumerDischarge
      )
classifyLocalResultAuthority mbAuthority schemeInfo dischargeConstructionScheme =
  case mbAuthority of
    Nothing -> pure (Nothing, Nothing)
    Just authority@TopologyConsumerAuthority{}
      | localResultIsRepresented authority ->
          pure (Just authority, Nothing)
      | Just discharge <-
          closedConsumerDischarge
            (Just authority)
            Nothing
            schemeInfo
            dischargeConstructionScheme ->
          pure (Nothing, Just discharge)
      | otherwise ->
          Left
            ( ValidationFailed
                [ "local topology result is neither present nor administratively discharged"
                , "  authority: " ++ show authority
                , "  packet scheme: " ++ show (siScheme schemeInfo)
                , "  discharge construction: "
                    ++ show dischargeConstructionScheme
                ]
            )
    Just authority ->
      Left
        ( ValidationFailed
            [ "local result authority is not identity topology"
            , "  authority: " ++ show authority
            ]
        )
  where
    -- Freshening against the lambda construction environment can move this
    -- declaration out of the packet while retaining its exact occurrence in
    -- the packet body.  That is not a discharge: the lambda constructor still
    -- needs the topology authority so it can validate and consume the ambient
    -- declaration.  Only a result identity absent from both declaration and
    -- occurrence positions may take the closed-consumer discharge path.
    localResultIsRepresented authority =
      any
        ( \packetRef ->
            any
              (typeBinderRefsSameIdentity packetRef)
              authorityRefs
        )
        ( boundRefsInType packetType
            ++ freeTypeVarRefsType packetType
        )
      where
        packetType = schemeToType (siScheme schemeInfo)
        directRef =
          typeBinderRefFromIdentity
            (scaConsumerIdentity authority)
            (typeBinderIdentityStableName (scaConsumerIdentity authority))
        routedRef = do
          node <- typeBinderRefNode directRef
          IntMap.lookup (getNodeId node) (siSubstRefs schemeInfo)
        authorityRefs = directRef : maybeToList routedRef

-- | Reclassify a packet's local topology result against the exact scheme that
-- its lambda constructor is about to emit.  Packet preparation retains the
-- result while it occurs in the complete graph construction, but a later
-- source-exact binder-spine specialization can consume that declaration
-- before term elaboration begins.  Record the existing closed-consumer proof
-- at that boundary so every later constructor sees the discharged state; do
-- not rediscover a missing binder after the lambda has been built.
resolveSubtermLocalResultAtConstruction
  :: SchemeInfo
  -> PreparedSubtermGeneralization
  -> Either ElabError PreparedSubtermGeneralization
resolveSubtermLocalResultAtConstruction constructionSchemeInfo packet = do
  (localResultAuthority, localResultDischarge) <-
    classifyLocalResultAuthority
      (preparedLocalResultCandidate packet)
      constructionSchemeInfo
      (siScheme (psgSchemeInfo packet))
  pure
    packet
      { psgLocalResultAuthority = localResultAuthority
      , psgLocalResultDischarge = localResultDischarge
      }

prepareSubtermGeneralizationWithConsumer
  :: IdentityGenerator
  -> Maybe SubtermConsumerAuthority
  -> Maybe GammaPacketAuthority
  -> Maybe SubtermConsumerAuthority
  -> SchemeInfo
  -> SchemeInfo
  -> Either ElabError (PreparedSubtermGeneralization, IdentityGenerator)
prepareSubtermGeneralizationWithConsumer generator consumerAuthority gammaAuthority localResultAuthority packet0 operatedPacket0 = do
  packetWithLocalResult <-
    ensurePacketLocalResultDeclaration localResultAuthority packet
  let gammaBoundScheme =
        projectVacuousConstructionBinders
          packetWithLocalResult
          ( consumerGammaBoundSchemeFrom
              consumerAuthority
              (siScheme packetWithLocalResult)
              (siScheme operatedPacket)
          )
      packetTy = schemeToType gammaBoundScheme
      packetBinderRefs =
        case scaConsumerIdentity <$> consumerAuthority of
          Nothing -> []
          -- Recursive data types reuse their structural self/result identities
          -- at every occurrence. Copy that semantic identity once and reuse
          -- it at each lexical occurrence.
          Just _ -> uniqueBinderRefs (boundRefsInType packetTy)
      generatorAfterPacket =
        advanceIdentityGeneratorPastMany
          ( generatedIdentitiesInSchemeInfo packetWithLocalResult
              ++ generatedIdentitiesInSchemeInfo operatedPacket
              ++ generatedIdentitiesInScheme gammaBoundScheme
          )
          generator
  packetWithPendingPrimaryConsumer <-
    deferPacketConsumerBounds
      consumerAuthority
      gammaAuthority
      packetWithLocalResult
  packetWithPendingConsumer <-
    deferPacketLocalResultBound
      localResultAuthority
      packetWithPendingPrimaryConsumer
  (retainedLocalResultAuthority, localResultDischarge) <-
    classifyLocalResultAuthority
      localResultAuthority
      packetWithLocalResult
      gammaBoundScheme
  (copiedRefs, generator') <-
    foldM allocateCopy (Map.empty, generatorAfterPacket) packetBinderRefs
  pure
    ( PreparedSubtermGeneralization
        { psgConsumerAuthority = consumerAuthority
        , psgLocalResultAuthority = retainedLocalResultAuthority
        , psgLocalResultDischarge = localResultDischarge
        , psgSchemeInfo = packetWithLocalResult
        , psgConsumerConstructionSchemeInfo = packetWithPendingConsumer
        , psgOperatedSchemeInfo = operatedPacket
        , psgGammaBoundScheme = gammaBoundScheme
        , psgSourceLambdaParameter = Nothing
        , psgCompilerExactResult = Nothing
        , psgCompilerExactBinderRenames = []
        , psgConstructionBinderRenames = []
        , psgInheritedGammaRoutes = Reify.emptyInheritedGammaRoutes
        , psgPlacedCopiedBinderRefs = Map.empty
        , psgCopiedBinderRefs = copiedRefs
        , psgGammaAuthority = gammaAuthority
        , psgClosedConsumerDischarge =
            closedConsumerDischarge
              consumerAuthority
              gammaAuthority
              packetWithLocalResult
              gammaBoundScheme
        , psgExactConsumerSpecialization = Nothing
        , psgSourceOwnerConsumerCompletion = Nothing
        , psgSourceOwnerFinalConsumerCompletion = Nothing
        , psgOpaqueResultConstruction = Nothing
        }
    , generator'
    )
  where
    packet = publishConstructionBinderOrder packet0
    operatedPacket = publishConstructionBinderOrder operatedPacket0

    publishConstructionBinderOrder schemeInfo =
      schemeInfo
        { siConstructionBinderOrderRefs =
            IntMap.union
              ( IntMap.fromList
                  [ (getNodeId node, ref)
                  | (ref, _) <-
                      schemeBinderRefs
                        ( schemeFromType
                            (schemeToType (siScheme schemeInfo))
                        )
                  , Just node <- [typeBinderRefNode ref]
                  ]
              )
              (siConstructionBinderOrderRefs schemeInfo)
        }

    -- Eq-Free is a construction step, not a repair performed after a bound
    -- has failed lexical validation.  A graph-only binder can become vacuous
    -- after a completed descendant is substituted into the packet.  Its
    -- provisional bound may mention a source binder that is scoped inside the
    -- retained packet body; publishing that dead declaration would therefore
    -- manufacture an ill-scoped forward reference.  Remove exactly the
    -- construction-owned, non-source binders whose identities do not occur in
    -- the retained suffix.  Processing inside-out also handles a chain of
    -- administrative binders made vacuous by the same completion.
    projectVacuousConstructionBinders schemeInfo candidateScheme =
      mkElabSchemeWithRefs
        (project (schemeBinderRefs candidateScheme))
        (schemeBody candidateScheme)
      where
        sourceRefs = IntMap.elems (siSourceBinderOrderRefs schemeInfo)
        constructionRefs =
          IntMap.elems (siConstructionBinderOrderRefs schemeInfo)

        project [] = []
        project (binder@(ref, _) : binders) =
          let retainedBinders = project binders
              retainedType =
                schemeToType
                  ( mkElabSchemeWithRefs
                      retainedBinders
                      (schemeBody candidateScheme)
                  )
              constructionOwned =
                any (typeBinderRefsSameIdentity ref) constructionRefs
              sourceOwned =
                any (typeBinderRefsSameIdentity ref) sourceRefs
              vacuous =
                not
                  ( any
                      (typeBinderRefsSameIdentity ref)
                      (freeTypeVarRefsType retainedType)
                  )
           in if constructionOwned && not sourceOwned && vacuous
                then retainedBinders
                else binder : retainedBinders

    allocateCopy (copies, nextGenerator) ref =
      let (copyRef, nextGenerator') =
            freshTypeBinderRef (typeBinderRefName ref) nextGenerator
       in Right
            ( Map.insert (typeBinderRefIdentity ref) copyRef copies
            , nextGenerator'
            )

    -- A distinct local topology result is a declaration owned by this packet,
    -- even when graph generalization leaves the rigid result free in the
    -- packet body. Introduce that pending declaration while the exact
    -- topology authority is still available. Later elaboration fills its
    -- bound from the recursively checked child.
    ensurePacketLocalResultDeclaration mbAuthority schemeInfo =
      case mbAuthority of
        Nothing -> pure schemeInfo
        Just authority
          | subtermConsumerAuthorityIsTopology authority ->
              ensureDeclaration
                (scaConsumerIdentity authority)
                (authorityConsumerRef (scaConsumerIdentity authority) schemeInfo)
        Just authority ->
          Left
            ( ValidationFailed
                [ "packet local result authority is not identity topology"
                , "  authority: " ++ show authority
                ]
            )
      where
        ensureDeclaration identity resultRef =
          case matchingBinders of
            [] ->
              pure
                ( rebuildSchemeInfoFromRefSubst
                    schemeInfo
                    ( mkElabSchemeWithRefs
                        ( schemeBinderRefs (siScheme schemeInfo)
                            ++ [(resultRef, Nothing)]
                        )
                        (schemeBody (siScheme schemeInfo))
                    )
                    resultSubst
                )
            [_] ->
              pure
                ( rebuildSchemeInfoFromRefSubst
                    schemeInfo
                    (siScheme schemeInfo)
                    resultSubst
                )
            matches ->
              Left
                ( ValidationFailed
                    [ "packet local topology result occurs more than once"
                    , "  consumer: " ++ show identity
                    , "  matches: " ++ show matches
                    ]
                )
          where
            matchingBinders =
              [ binder
              | binder@(ref, _) <- schemeBinderRefs (siScheme schemeInfo)
              , typeBinderRefsSameIdentity ref resultRef
              ]
            directRef = directPacketConsumerRef identity
            resultSubst =
              case typeBinderRefNode directRef of
                Just resultNode ->
                  IntMap.insert
                    (getNodeId resultNode)
                    resultRef
                    (siSubstRefs schemeInfo)
                Nothing -> siSubstRefs schemeInfo

    -- A packet-owned Gamma consumer is filled from the recursively constructed
    -- child type at its exact source constructor.  The graph-operated view is
    -- retained separately in 'psgOperatedSchemeInfo'; leaving its provisional
    -- bound in the construction scheme would force elaboration either to
    -- overwrite a completed bound after the fact or to emit an inadmissible
    -- Hyp from a specialized child.
    deferPacketConsumerBounds mbConsumerAuthority mbGammaAuthority schemeInfo = do
      gammaDeferred <-
        case mbGammaAuthority of
          Just packetGammaAuthority ->
            let identity = gpaConsumerIdentity packetGammaAuthority
             in deferConsumer
                  identity
                  (packetConsumerRef identity schemeInfo)
                  schemeInfo
          Nothing -> pure schemeInfo
      case mbConsumerAuthority of
        Just authority
          | ( subtermConsumerAuthorityIsTopology authority
                || isJust
                  (subtermConsumerAuthorityEnclosingOwner authority)
            )
          , let identity = scaConsumerIdentity authority
          , maybe True ((/= identity) . gpaConsumerIdentity) mbGammaAuthority ->
              deferConsumer
                identity
                (authorityConsumerRef identity gammaDeferred)
                gammaDeferred
        _ -> pure gammaDeferred

    -- A distinct topology result is completed by the recursively checked
    -- lambda body, just like the packet's primary topology consumer.  Keep
    -- the graph-computed bound in the complete packet, but leave the
    -- construction view pending.  In particular, a higher-rank body can
    -- complete @b >= a -> a@ as @b >= forall a. a -> a@ without first
    -- publishing the source-owned @a@ in the enclosing Gamma.
    deferPacketLocalResultBound mbAuthority schemeInfo =
      case mbAuthority of
        Nothing -> pure schemeInfo
        Just authority
          | subtermConsumerAuthorityIsTopology authority ->
              let identity = scaConsumerIdentity authority
               in deferConsumer
                    identity
                    (authorityConsumerRef identity schemeInfo)
                    schemeInfo
        Just authority ->
          Left
            ( ValidationFailed
                [ "packet local result authority is not identity topology"
                , "  authority: " ++ show authority
                ]
            )

    deferConsumer pendingIdentity consumerRef schemeInfo =
      case matchingBinders of
        [] -> do
          -- The root RaiseMerge is itself the construction-time authority for
          -- this exterior.  Generalization normally publishes a substitution
          -- route for it, but a quotient can legitimately leave no syntactic
          -- binder or route in the incoming scheme.  Introduce the pending
          -- slot from the frozen exterior identity instead of trying to
          -- rediscover it from the scheme shape.
          pure
            ( rebuildSchemeInfoFromRefSubst
                schemeInfo
                ( mkElabSchemeWithRefs
                    ( schemeBinderRefs (siScheme schemeInfo)
                        ++ [(consumerRef, Nothing)]
                    )
                    (schemeBody (siScheme schemeInfo))
                )
                consumerSubst
            )
        [(existingConsumerRef, _)] ->
          pure
            ( rebuildSchemeInfoFromRefSubst
                schemeInfo
                ( mkElabSchemeWithRefs
                    [ if typeBinderRefsSameIdentity ref existingConsumerRef
                        then (ref, Nothing)
                        else binder
                    | binder@(ref, _) <- schemeBinderRefs (siScheme schemeInfo)
                    ]
                    (schemeBody (siScheme schemeInfo))
                )
                consumerSubst
            )
        matches ->
          Left
            ( ValidationFailed
                [ "packet Gamma consumer occurs more than once in its construction scheme"
                , "  consumer: " ++ show pendingIdentity
                , "  matches: " ++ show matches
                ]
            )
      where
        directConsumerRef = directPacketConsumerRef pendingIdentity
        consumerSubst =
          case typeBinderRefNode directConsumerRef of
            Just consumerNode ->
              IntMap.insert
                (getNodeId consumerNode)
                consumerRef
                (siSubstRefs schemeInfo)
            Nothing -> siSubstRefs schemeInfo
        matchingBinders =
          [ binder
          | binder@(ref, _) <- schemeBinderRefs (siScheme schemeInfo)
          , typeBinderRefsSameIdentity ref consumerRef
          ]

    directPacketConsumerRef pendingIdentity =
      typeBinderRefFromIdentity
        pendingIdentity
        (typeBinderIdentityStableName pendingIdentity)

    -- The substitution map also carries source/reconstruction metadata; it
    -- is not by itself proof that a different identity owns this Gamma slot.
    -- The sole alias case is the paper's peer-variable quotient:
    -- S(operated) is exactly the routed variable.  Otherwise retain the
    -- frozen exterior identity.
    packetConsumerRef pendingIdentity schemeInfo =
      case (routedConsumerRef, schemeToType (siScheme operatedPacket)) of
        (Just routedRef, TVarRef operatedRef)
          | typeBinderRefsSameIdentity routedRef operatedRef -> routedRef
        _ -> directConsumerRef
      where
        directConsumerRef = directPacketConsumerRef pendingIdentity
        routedConsumerRef =
          case typeBinderRefNode directConsumerRef of
            Just consumerNode ->
              IntMap.lookup
                (getNodeId consumerNode)
                (siSubstRefs schemeInfo)
            Nothing ->
              find
                ( (== pendingIdentity)
                    . typeBinderRefIdentity
                )
                (freeTypeVarRefsType (schemeToType (siScheme schemeInfo)))

    -- Topology and enclosing-owner authorities are already exact consumer
    -- declarations. Preserve their routed construction identity while the
    -- bound is pending; unlike the Gamma peer-variable rule, no operated-type
    -- shape is needed to justify the route. If generalization omitted the
    -- route, the frozen exterior identity itself is the pending declaration.
    authorityConsumerRef pendingIdentity schemeInfo =
      case typeBinderRefNode directConsumerRef of
        Just consumerNode ->
          IntMap.findWithDefault
            directConsumerRef
            (getNodeId consumerNode)
            (siSubstRefs schemeInfo)
        Nothing -> directConsumerRef
      where
        directConsumerRef = directPacketConsumerRef pendingIdentity

    uniqueBinderRefs = go Set.empty
      where
        go _ [] = []
        go seen (ref : rest)
          | Set.member identity seen = go seen rest
          | otherwise = ref : go (Set.insert identity seen) rest
          where
            identity = typeBinderRefIdentity ref

-- | Pair source and canonical annotation roots without allowing 'zip' to
-- truncate either side.  Canonicalization may rewrite graph node ids, but it
-- must preserve the recursive expression shape and every resolved owner
-- identity used to key a prepared packet.
pairSubtermGeneralizationRoots
  :: [AnnExpr]
  -> [AnnExpr]
  -> Either ElabError [(AnnExpr, AnnExpr)]
pairSubtermGeneralizationRoots sources canons
  | sourceCount /= canonCount =
      Left
        ( ValidationFailed
            [ "prepared subterm annotation root count mismatch"
            , "  source roots: " ++ show sourceCount
            , "  canonical roots: " ++ show canonCount
            ]
        )
  | otherwise =
      traverse validateRoot (zip [0 :: Int ..] (zip sources canons))
  where
    sourceCount = length sources
    canonCount = length canons

    validateRoot (rootIndex, pair@(source, canon)) = do
      validateShape ("root[" ++ show rootIndex ++ "]") source canon
      pure pair

    validateShape path source canon =
      case (source, canon) of
        (AResolvedVar sourceDetails _ _, AResolvedVar canonDetails _ _) ->
          validateOwner path sourceDetails canonDetails
        (ALit sourceLit _, ALit canonLit _)
          | sourceLit == canonLit -> Right ()
          | otherwise -> shapeMismatch path source canon
        (ALam _ sourceDetails _ _ sourceBody _ _, ALam _ canonDetails _ _ canonBody _ _) -> do
          validateOwner path sourceDetails canonDetails
          validateShape (path ++ ".lambda-body") sourceBody canonBody
        (AApp sourceFun sourceArg _ _ _, AApp canonFun canonArg _ _ _) -> do
          validateShape (path ++ ".application-function") sourceFun canonFun
          validateShape (path ++ ".application-argument") sourceArg canonArg
        (ALet _ sourceDetails _ _ _ _ sourceRhs sourceBody _, ALet _ canonDetails _ _ _ _ canonRhs canonBody _) -> do
          validateOwner path sourceDetails canonDetails
          validateShape (path ++ ".let-rhs") sourceRhs canonRhs
          validateShape (path ++ ".let-body") sourceBody canonBody
        (AAnn sourceInner _ _, AAnn canonInner _ _) ->
          validateShape (path ++ ".annotation") sourceInner canonInner
        (AExactAnn sourceInner _ _ _, AExactAnn canonInner _ _ _) ->
          validateShape (path ++ ".exact-annotation") sourceInner canonInner
        (ALetScope sourceInner _ _, ALetScope canonInner _ _) ->
          validateShape (path ++ ".let-scope") sourceInner canonInner
        (AUnfold sourceInner _ _, AUnfold canonInner _ _) ->
          validateShape (path ++ ".unfold") sourceInner canonInner
        _ -> shapeMismatch path source canon

    validateOwner path sourceDetails canonDetails
      | idDetailsIdentityKey sourceDetails == idDetailsIdentityKey canonDetails = Right ()
      | otherwise =
          Left
            ( ValidationFailed
                [ "prepared subterm resolved owner identity mismatch at " ++ path
                , "  source owner: " ++ show (idDetailsIdentityKey sourceDetails)
                , "  canonical owner: " ++ show (idDetailsIdentityKey canonDetails)
                ]
            )

    shapeMismatch path source canon =
      Left
        ( ValidationFailed
            [ "prepared subterm annotation shape mismatch at " ++ path
            , "  source shape: " ++ annotationShape source
            , "  canonical shape: " ++ annotationShape canon
            ]
        )

    annotationShape ann =
      case ann of
        AResolvedVar {} -> "variable"
        ALit {} -> "literal"
        ALam {} -> "lambda"
        AApp {} -> "application"
        ALet {} -> "let"
        AAnn {} -> "annotation"
        AExactAnn {} -> "exact-annotation"
        ALetScope {} -> "let-scope"
        AUnfold {} -> "unfold"

-- | Combine independently prepared packet sets, rejecting duplicate lexical
-- owners.  A duplicate resolved identity means the source/canonical pairing
-- has lost the one-owner/one-boundary invariant; choosing either packet would
-- make elaboration depend on traversal order.
mergeSubtermGeneralizations
  :: SubtermGeneralizations
  -> SubtermGeneralizations
  -> Either ElabError SubtermGeneralizations
mergeSubtermGeneralizations left right =
  case Map.keys (Map.intersection left right) of
    [] -> Right (Map.union left right)
    duplicateOwners ->
      Left
        ( ValidationFailed
            ( "duplicate prepared subterm generalization owners"
                : map (("  owner: " ++) . show) duplicateOwners
            )
        )

-- | Recover the exact result constructed by applying one syntactic lambda.
--
-- The source lambda proves that one value application removes exactly its
-- outer arrow.  Starting the ownership walk at that lambda's body then makes
-- the packet usable only when let/annotation wrappers preserve the complete
-- body result boundary.  Paired Gamma authority proves that the packet's
-- nested result construction has been completed; requiring its consumer to
-- be absent from the completed endpoint proves that no outgoing Hyp remains.
-- The
-- administrative-parameter certificate proves that this exact packet restored
-- the result lambda crossed by the path.  An ordinary packet may still have
-- an outgoing consumer and is not an application result.  The certified
-- packet's completed scheme is already the sealed bottom-up construction of
-- the result, so using it here avoids first specializing the body to the
-- graph's provisional monomorphic view and trying to reconstruct the lost
-- quantifiers after 'EApp'.  Completion can make a construction-owned prefix
-- binder vacuous while retained binders around it remain live.  In that case
-- the packet also supplies the exact xMLF binder-spine computation that
-- consumes the stale declaration.  Source-declared binders are never removed:
-- their vacuity is part of the source ABI rather than an administrative
-- artifact.
directLambdaApplicationResultConstructionFor
  :: (TypeBinderRef -> Bool)
  -> ElabType
  -> AnnExpr
  -> SubtermGeneralizations
  -> Maybe DirectLambdaApplicationResultConstruction
directLambdaApplicationResultConstructionFor canEliminate graphResultTy ann packets =
  case ann of
    ALam _ _ _ _ body _ _ -> do
      ownership <- subtermResultOwnershipFor body packets
      guard (subtermResultOwnershipHasTransparentPath ownership)
      let packet = subtermResultOwnershipPacket ownership
          completedSchemeInfo =
            subtermGeneralizationSchemeInfo packet
          completedScheme =
            siScheme completedSchemeInfo
          completedOccurrenceRefs =
            freeTypeVarRefsType (schemeBody completedScheme)
              ++ concatMap
                ( maybe
                    []
                    (freeTypeVarRefsType . tyToElab)
                    . snd
                )
                (schemeBinderRefs completedScheme)
      case enclosingConsumerResult ownership packet of
        Just resultTy ->
          pure
            ( DirectLambdaEnclosingConsumerEqFreeResultConstruction
                resultTy
            )
        Nothing ->
          DirectLambdaPacketGammaResultConstruction
            <$> packetGammaResult ownership packet completedOccurrenceRefs
    _ -> Nothing
  where
    -- A consumer-only packet has no packet-local Gamma authority: its exact
    -- consumer-facing bound is constructed by the enclosing source owner.
    -- When a direct value application exposes that packet transparently, the
    -- prepared completed-to-bound binder-spine is the positive Eq-Free/N
    -- construction for the application's result.  Select that construction
    -- before checking the function child instead of retaining the completed
    -- packet's now-vacuous administrative forall and trying to remove it
    -- after EApp.
    enclosingConsumerResult ownership packet = do
      authority <- subtermGeneralizationConsumerAuthority packet
      case authority of
        EnclosingGammaConsumerAuthority{} -> pure ()
        _ -> Nothing
      guard (isNothing (subtermGeneralizationGammaAuthority packet))
      (certifiedLambdaNode, completedResultTy) <-
        subtermGeneralizationApplicationSourceLambdaResultConstruction
          canEliminate
          packet
      guard
        ( certifiedLambdaNode
            == subtermResultOwnershipLambdaNode ownership
        )
      let consumerResultTy =
            schemeToType
              (subtermGeneralizationGammaBoundScheme packet)
      guard
        ( all
            ( (/= scaConsumerIdentity authority)
                . typeBinderRefIdentity
            )
            (freeTypeVarRefsType consumerResultTy)
        )
      projectedResultTy <-
        projectVacuousUnboundedConstructionBinders
          (subtermGeneralizationSchemeInfo packet)
          completedResultTy
      guard
        ( completedConstructionTypesAgree
            projectedResultTy
            consumerResultTy
        )
      _ <-
        planExactBinderSpine
          completedConstructionTypesAgree
          completedResultTy
          consumerResultTy
      pure consumerResultTy

    -- The enclosing-consumer lane exists for the paper O step that becomes
    -- available only after the direct value arrow has been removed.  Build
    -- that projection explicitly: remove at least one unbounded, vacuous
    -- declaration whose provenance is construction-only.  A bounded
    -- declaration is an N obligation and remains owned by the ordinary
    -- packet/Gamma lane, even if packet preparation omitted it from its
    -- consumer-facing view.
    projectVacuousUnboundedConstructionBinders
      schemeInfo
      completedTy = do
        let completedScheme = schemeFromType completedTy
            (retainedBinders, removedBinders) =
              project
                (schemeBinderRefs completedScheme)
            projectedTy =
              schemeToType
                ( mkElabSchemeWithRefs
                    retainedBinders
                    (schemeBody completedScheme)
                )
        guard (not (null removedBinders))
        pure projectedTy
      where
        completedBody = schemeBody (schemeFromType completedTy)
        sourceRefs =
          IntMap.elems (siSourceBinderOrderRefs schemeInfo)
        constructionRefs =
          IntMap.elems (siConstructionBinderOrderRefs schemeInfo)

        project [] = ([], [])
        project (binder@(ref, mbBound) : binders) =
          let (retainedBinders, removedBinders) = project binders
              retainedTy =
                schemeToType
                  ( mkElabSchemeWithRefs
                      retainedBinders
                      completedBody
                  )
              constructionOwned =
                any (typeBinderRefsSameIdentity ref) constructionRefs
              sourceOwned =
                any (typeBinderRefsSameIdentity ref) sourceRefs
              vacuous =
                not
                  ( any
                      (typeBinderRefsSameIdentity ref)
                      (freeTypeVarRefsType retainedTy)
                  )
           in if isNothing mbBound
                && constructionOwned
                && not sourceOwned
                && vacuous
                then (retainedBinders, ref : removedBinders)
                else (binder : retainedBinders, removedBinders)

    packetGammaResult ownership packet completedOccurrenceRefs = do
      gammaAuthority <- subtermGeneralizationGammaAuthority packet
      (certifiedLambdaNode, _, _) <-
        subtermGeneralizationApplicationSourceLambdaParameter
          canEliminate
          packet
      guard
        ( certifiedLambdaNode
            == subtermResultOwnershipLambdaNode ownership
        )
      let completedResultTy =
            applicationConstructionTypeWithEliminableBinders
              canEliminate
              packet
          consumerIdentity = gpaConsumerIdentity gammaAuthority
          consumerAlreadyAbsent =
            all
              ((/= consumerIdentity) . typeBinderRefIdentity)
              completedOccurrenceRefs
          consumerClosedAtBound = do
            closedResultTy <-
              eliminateCompletedConsumerAtBound
                consumerIdentity
                completedResultTy
            guard
              ( alphaEqType
                  (schemeBody (schemeFromType closedResultTy))
                  (schemeBody (schemeFromType graphResultTy))
              )
            guard
              ( isJust
                  ( planExactBinderSpine
                      completedConstructionTypesAgree
                      completedResultTy
                      closedResultTy
                  )
              )
            pure closedResultTy
      if consumerAlreadyAbsent
        then pure completedResultTy
        else consumerClosedAtBound

    -- The packet's Gamma authority identifies the one bounded declaration
    -- consumed when the direct value lambda is applied.  Replay exactly that
    -- paper N step beneath any retained leading binders; no other declaration
    -- is inferred or removed here.  The caller's graph result is checked above
    -- only as the residual topology of this already-authorized computation.
    eliminateCompletedConsumerAtBound consumerIdentity ty =
      case ty of
        TForallRef ref mbBound bodyTy
          | typeBinderRefIdentity ref == consumerIdentity -> do
              bound <- mbBound
              pure
                ( substTypeCaptureRef
                    ref
                    (tyToElab bound)
                    bodyTy
                )
          | otherwise ->
              TForallRef ref mbBound
                <$> eliminateCompletedConsumerAtBound
                  consumerIdentity
                  bodyTy
        _ -> Nothing

-- | Recover the complete result published by an administrative source
-- lambda.  This is the pre-child counterpart of the checked owner-final
-- construction: the direct source lambda supplies the value arrow that one
-- application removes, while the returned-lambda packet supplies the complete
-- bottom-up codomain construction.
--
-- Unlike 'directLambdaApplicationResultConstructionFor', this lane does not
-- claim that a packet-local Gamma consumer has already been discharged.  It
-- therefore needs the returned-lambda packet to name the same exact lambda
-- owner, and is suitable only as an incoming construction endpoint whose
-- recursively checked lambda must still certify the completed result.  That
-- staging is observable exactly when the returned value contains a nested
-- generated declaration owned by the source sidecar: publication may
-- alpha-freshen that local declaration before the child certificate is
-- available.  Leading graph declarations belong to an enclosing Gamma and
-- do not create an application-result claim here.
directAdministrativeLambdaApplicationResultConstructionFor
  :: (TypeBinderRef -> Bool)
  -> IntMap.IntMap TypeBinderRef
  -> AnnExpr
  -> SubtermGeneralizations
  -> Maybe ElabType
directAdministrativeLambdaApplicationResultConstructionFor canEliminate sourceSidecarRefs ann packets =
  do
    (directOwner, ownership, packet) <-
      directAdministrativeLambdaOwnerPacket ann packets
    (snd <$> directOpaqueAdministrativeLambdaSourceOwnerCompletion directOwner packet)
      <|> opaqueSourceCompletedAdministrativeResult ownership packet
      <|> sourceStagedAdministrativeResult ownership packet
  where
    -- An opaque returned-lambda composition is already sealed against the
    -- exact source lambdas which replace every provisional carrier.  Keep
    -- that constructed placement (rather than the packet's pre-composition
    -- binder placement) as the staged result of applying the direct wrapper.
    -- The terminal packet owner ties it to this transparent result path; the
    -- application still passes it downward as an administrative marker so
    -- the recursively checked owners must construct the endpoint.
    opaqueSourceCompletedAdministrativeResult ownership packet = do
      (certifiedLambdaNode, _) <-
        subtermGeneralizationAdministrativeLambdaResultConstruction packet
      guard
        (certifiedLambdaNode == subtermResultOwnershipLambdaNode ownership)
      (constructedTy, sourceLambdaAuthorities) <-
        subtermGeneralizationOpaqueResultSourceLambdaCompletion packet
      guard (not (null sourceLambdaAuthorities))
      pure constructedTy

    -- Packet preparation has already paired this exact source lambda with its
    -- canonical body and opened the returned-lambda parameter by identity.
    -- The endpoint remains staged: the recursively elaborated direct lambda
    -- must still publish an owner-final certificate before the application
    -- may treat it as constructed.
    sourceStagedAdministrativeResult ownership packet = do
      (resultLambdaNode, resultTy) <-
        subtermGeneralizationApplicationSourceLambdaResultConstruction
          canEliminate
          packet
      guard
        (resultLambdaNode == subtermResultOwnershipLambdaNode ownership)
      guard
        ( administrativeLambdaResultNeedsOwnerConfirmation
            sourceSidecarRefs
            packet
            resultTy
        )
      pure resultTy

-- | Preserve the complete construction carried by the paired source-owner
-- certificate for a directly applied administrative lambda.  The first type
-- is the frozen operated Gamma; the second is the source-constructed result
-- endpoint.  The application elaborator keeps both in an administrative
-- expected-term marker so the child may use the endpoint before checking
-- while owner-final validation still owns the transition from the frozen
-- Gamma.
directAdministrativeLambdaApplicationSourceOwnerCompletionFor
  :: AnnExpr
  -> SubtermGeneralizations
  -> Maybe (ElabType, ElabType)
directAdministrativeLambdaApplicationSourceOwnerCompletionFor ann packets = do
  (directOwner, _ownership, packet) <-
    directAdministrativeLambdaOwnerPacket ann packets
  directOpaqueAdministrativeLambdaSourceOwnerCompletion directOwner packet

directOpaqueAdministrativeLambdaSourceOwnerCompletion
  :: LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> Maybe (ElabType, ElabType)
directOpaqueAdministrativeLambdaSourceOwnerCompletion directOwner packet = do
  (authority, certifiedOwner, frozenOperatedType, expectedEndpoint) <-
    subtermGeneralizationSourceOwnerConsumerCompletion packet
  guard (certifiedOwner == directOwner)
  guard (scaEdgeId authority == lgoBoundaryEdge directOwner)
  let frozenScheme = schemeFromType frozenOperatedType
  carrierRef <-
    case schemeBody frozenScheme of
      TVarRef ref -> pure ref
      _ -> Nothing
  guard
    ( any
        (typeBinderRefsSameIdentity carrierRef . fst)
        (schemeBinderRefs frozenScheme)
    )
  pure (frozenOperatedType, expectedEndpoint)

directAdministrativeLambdaOwnerPacket
  :: AnnExpr
  -> SubtermGeneralizations
  -> Maybe
      ( LocalGammaOwner
      , SubtermResultOwnership
      , PreparedSubtermGeneralization
      )
directAdministrativeLambdaOwnerPacket ann packets =
  case ann of
    ALam _ _ _ lambdaScope body bodyEdge lambdaNode -> do
      ownership <- subtermResultOwnershipFor body packets
      guard (subtermResultOwnershipHasTransparentPath ownership)
      let directOwner =
            LocalGammaOwner
              { lgoConstructor = LocalLambdaGamma
              , lgoBoundaryEdge = bodyEdge
              , lgoTermNode = lambdaNode
              , lgoScope = GenRef lambdaScope
              }
      pure
        ( directOwner
        , ownership
        , subtermResultOwnershipPacket ownership
        )
    _ -> Nothing

administrativeLambdaResultNeedsOwnerConfirmation
  :: IntMap.IntMap TypeBinderRef
  -> PreparedSubtermGeneralization
  -> ElabType
  -> Bool
administrativeLambdaResultNeedsOwnerConfirmation sourceSidecarRefs packet resultTy =
  any sourceOwnsNestedResultDeclaration nestedResultDeclarations
  where
    leadingResultDeclarations =
      map fst (schemeBinderRefs (schemeFromType resultTy))
    nestedResultDeclarations =
      [ declaration
      | declaration <- typeBinderDeclarationRefs resultTy
      , not
          ( any
              (typeBinderRefsSameIdentity declaration)
              leadingResultDeclarations
          )
      ]
    sourceOwnedDeclarations =
      IntMap.elems sourceSidecarRefs
        ++ IntMap.elems
          ( siSourceBinderOrderRefs
              (psgSchemeInfo packet)
          )
    sourceOwnsNestedResultDeclaration declaration =
      case
          typeBinderIdentityGeneratedUnique
            (typeBinderRefIdentity declaration)
        of
          Nothing -> False
          Just _ ->
            any
              (typeBinderRefsSameIdentity declaration)
              sourceOwnedDeclarations

-- | Recover the lexical path to the already-bottom-up packet that changes an
-- expression's result type.  The terminal constructor is always the lambda
-- that owns the packet.  Every traversed frame is recorded explicitly so a
-- caller can distinguish an immediate lambda from a lambda exposed through a
-- let or annotation without inspecting a reified type after the fact.
subtermResultOwnershipFor
  :: AnnExpr
  -> SubtermGeneralizations
  -> Maybe SubtermResultOwnership
subtermResultOwnershipFor =
  subtermResultOwnershipForWith
    (\_ _ -> False)

-- | The direct-application authority query may follow a let whose result is
-- exactly that let's resolved binding occurrence back to its RHS.  This is
-- intentionally separate from ordinary packet ownership: preparation and
-- closure must not globally move a descendant packet across a let merely
-- because the let returns the same value.
subtermResultOwnershipForResolvedLetAlias
  :: AnnExpr
  -> SubtermGeneralizations
  -> Maybe SubtermResultOwnership
subtermResultOwnershipForResolvedLetAlias =
  subtermResultOwnershipForWith letBodyReturnsResolvedBinding

subtermResultOwnershipForWith
  :: (IdDetails -> AnnExpr -> Bool)
  -> AnnExpr
  -> SubtermGeneralizations
  -> Maybe SubtermResultOwnership
subtermResultOwnershipForWith followsResolvedLetAlias =
  go TransparentSubtermResultPath
  where
    go path ann packets =
      case ann of
        ALam _ details _ lambdaScope body lambdaEdge lambdaNode ->
          case Map.lookup ownerKey packets of
            Just packet ->
              Just
                SubtermResultOwnership
                  { sroOwnerKey = ownerKey
                  , sroLambdaNode = lambdaNode
                  , sroLambdaScope = lambdaScope
                  , sroLambdaEdge = lambdaEdge
                  , sroLambdaArity = sourceLambdaArity details body
                  , sroPath = path
                  , sroPacket = packet
                  }
            -- A parent prepares the complete packet for a direct nested
            -- lambda. Crossing that lambda makes the remaining path opaque.
            Nothing -> go OpaqueSubtermResultPath body packets
          where
            ownerKey = idDetailsIdentityKey details
        ALet _ details _ _ _ _ rhs body _
          | followsResolvedLetAlias details body ->
              go path rhs packets
          | otherwise -> go path body packets
        AExactAnn inner _ _ _ -> go path inner packets
        AAnn inner _ _ -> go path inner packets
        ALetScope inner _ _ -> go path inner packets
        AUnfold inner _ _ -> go OpaqueSubtermResultPath inner packets
        _ -> Nothing

    sourceLambdaArity details body =
      1
        + case administrativeLambdaBody details body of
          Just (ALam _ nestedDetails _ _ nestedBody _ _) ->
            sourceLambdaArity nestedDetails nestedBody
          _ -> 0

-- | Whether the root-to-owner path preserves the result boundary. Let bodies
-- and annotation wrappers do; crossing another lambda adds an arrow, while an
-- unfold performs a computation. Those non-transparent frames need their own
-- owner construction and must not reuse the direct-lambda closure plan.
subtermResultOwnershipHasTransparentPath
  :: SubtermResultOwnership
  -> Bool
subtermResultOwnershipHasTransparentPath ownership =
  sroPath ownership == TransparentSubtermResultPath

-- | Whether the packet's exact enclosing-consumer authority proves that the
-- result lambda itself has already installed the packet.  This is stronger
-- than merely finding the consumer identity in a scheme: the recorded edge,
-- lambda node, and source owner must all be the same construction boundary.
-- Both ordinary enclosing-Gamma and topology consumers are handled by the
-- lambda constructor; root- and packet-owned consumers deliberately fail.
subtermResultOwnershipConsumerClosedLocally
  :: SubtermResultOwnership
  -> Bool
subtermResultOwnershipConsumerClosedLocally ownership =
  subtermResultOwnershipHasTransparentPath ownership
    && case psgConsumerAuthority (sroPacket ownership) of
      Just authority
        | Just owner <- subtermConsumerAuthorityEnclosingOwner authority ->
            lgoConstructor owner == LocalLambdaGamma
              && lgoBoundaryEdge owner == scaEdgeId authority
              && lgoBoundaryEdge owner == sroLambdaEdge ownership
              && lgoTermNode owner == sroLambdaNode ownership
              && lgoScope owner == GenRef (sroLambdaScope ownership)
      _ -> False

-- | Select declarations that the exact result lambda must emit itself.
-- Ownership supplies the source lambda's resolved owner, boundary edge, term
-- node, and scope; the direct source sidecar supplies declaration authority;
-- and the packet scheme proves that the identity is locally quantified by
-- that lambda.  An enclosing transparent constructor may use these refs as
-- declaration evidence, but must not prebind or quotient them into its Gamma
-- before the owning lambda emits its own type abstraction.
subtermResultOwnershipLocalSourceDeclarationRefs
  :: IntMap.IntMap TypeBinderRef
  -> SubtermResultOwnership
  -> [TypeBinderRef]
subtermResultOwnershipLocalSourceDeclarationRefs directSourceBinderRefs ownership
  | subtermResultOwnershipConsumerClosedLocally ownership =
      [ binderRef
      | (binderRef, _) <-
          schemeBinderRefs
            ( siScheme
                ( subtermGeneralizationConsumerConstructionSchemeInfo
                    (sroPacket ownership)
                )
            )
      , any
          (typeBinderRefsSameIdentity binderRef)
          (IntMap.elems directSourceBinderRefs)
      ]
  | otherwise = []

subtermResultOwnershipLambdaNode :: SubtermResultOwnership -> NodeId
subtermResultOwnershipLambdaNode = sroLambdaNode

subtermResultOwnershipLambdaArity :: SubtermResultOwnership -> Int
subtermResultOwnershipLambdaArity = sroLambdaArity

subtermResultOwnershipPacket
  :: SubtermResultOwnership
  -> PreparedSubtermGeneralization
subtermResultOwnershipPacket = sroPacket

-- | Compatibility view for consumers that need only the unique owned packet.
-- New construction code should retain 'SubtermResultOwnership' whenever
-- binder placement depends on how the owner is reached.
subtermGeneralizationsOwnedBy :: AnnExpr -> SubtermGeneralizations -> SubtermGeneralizations
subtermGeneralizationsOwnedBy ann packets =
  case subtermResultOwnershipFor ann packets of
    Just ownership ->
      Map.singleton
        (sroOwnerKey ownership)
        (sroPacket ownership)
    Nothing -> Map.empty

-- | Allocate capture-free display names for a prepared scheme without
-- changing binder identity.  The allocator is deliberately shared by local
-- packet construction and enclosing packet placement: if both stages reserve
-- the same lexical names, they choose the same paper-order @a, b, ...@
-- payloads instead of independently inventing suffixes such as @a1@.
freshenSchemeInfoBinderNamesAgainst
  :: Set.Set String
  -> SchemeInfo
  -> SchemeInfo
freshenSchemeInfoBinderNamesAgainst reservedNames schemeInfo
  | null refRenames = schemeInfo
  | otherwise =
      rebuildSchemeInfoFromRefSubst
        schemeInfo
        (mkElabSchemeWithRefs renamedBinders renamedBody)
        renamedSubst
  where
    scheme = siScheme schemeInfo
    binders = schemeBinderRefs scheme
    binderDomain =
      Set.unions (map (typeBinderRefAliasNames . fst) binders)
    (_, chosenRenames) =
      foldl'
        chooseBinderName
        (reservedNames, [])
        (map fst binders)
    refRenames =
      [ (ref, renameTypeBinderRef name ref)
      | (ref, name) <- chosenRenames
      , typeBinderRefName ref /= name
      ]
    renamedBinders = renameBinders [] binders
    renamedBody = renameType refRenames (schemeBody scheme)
    renamedSubst = IntMap.map (renameRef refRenames) (siSubstRefs schemeInfo)

    chooseBinderName (used, renames) ref =
      let originalName = typeBinderRefName ref
          chosenName
            | Set.member originalName used =
                freshAlphaName (Set.union used binderDomain)
            | otherwise = originalName
          ref' = renameTypeBinderRef chosenName ref
       in ( Set.union used (typeBinderRefAliasNames ref')
          , renames ++ [(ref, chosenName)]
          )

    renameBinders _ [] = []
    renameBinders previous ((ref, mbBound) : rest) =
      let ref' = renameRef refRenames ref
          mbBound' = fmap (mapBoundType (renameType previous)) mbBound
          previous'
            | typeBinderRefsSameIdentityAndName ref ref' = previous
            | otherwise = previous ++ [(ref, ref')]
       in (ref', mbBound') : renameBinders previous' rest

    renameType renames ty =
      foldl'
        ( \renamedTy (oldRef, newRef) ->
            if typeBinderRefsSameIdentityAndName oldRef newRef
              then renamedTy
              else substTypeCaptureRef oldRef (TVarRef newRef) renamedTy
        )
        ty
        renames

    renameRef [] ref = ref
    renameRef ((oldRef, newRef) : rest) ref
      | typeBinderRefsSameIdentity oldRef ref = newRef
      | otherwise = renameRef rest ref

    freshAlphaName used =
      go 0
      where
        go idx
          | Set.member candidate used = go (idx + 1)
          | otherwise = candidate
          where
            candidate = alphaName idx 0

-- | Move a prepared descendant scheme out of an enclosing scheme spine and
-- install that whole scheme as the bound of the unique later result binder
-- that consumes it.  Quantifiers nested in packet bounds count as packet-owned
-- too; this makes packet composition recursive instead of losing ownership
-- after one K-like level.
data PacketBoundMatch
  = WholePacketBound
  | PacketBodyBound
  | PacketConstructionBound

-- | A forall declaration below the outer 'ElabScheme' spine together with
-- the exact lexical binders visible at that declaration.  A topology
-- consumer can already have been constructed at this nested declaration by
-- the enclosing RaiseMerge.  Retaining the lexical scope here lets packet
-- placement validate that construction without flattening the declaration
-- into the outer scheme.
data NestedForallDeclaration = NestedForallDeclaration
  { nfdRef :: !TypeBinderRef
  , nfdBound :: !(Maybe BoundType)
  , nfdLexicalRefs :: ![TypeBinderRef]
  -- | The outer scheme declaration whose lower bound contains this nested
  -- declaration.  Placement replaces that lower bound atomically, so a
  -- matching nested identity owned by the selected outer consumer is part of
  -- the provisional construction being replaced, not a second consumer.
  , nfdOuterBoundOwner :: !(Maybe TypeBinderRef)
  }
  deriving (Show)

-- | Collect nested forall declarations without treating ordinary variable
-- occurrences as declarations.  Bounds see the binders outside their own
-- declaration; forall bodies additionally see the declared binder.
nestedForallDeclarationsInScheme
  :: ElabScheme
  -> [NestedForallDeclaration]
nestedForallDeclarationsInScheme scheme =
  collectOuterBinders [] (schemeBinderRefs scheme)
    ++ collectNestedForalls
      Nothing
      (map fst (schemeBinderRefs scheme))
      (schemeBody scheme)
  where
    collectOuterBinders _ [] = []
    collectOuterBinders lexicalRefs ((ref, mbBound) : rest) =
      maybe
        []
        (collectNestedForalls (Just ref) lexicalRefs)
        mbBound
        ++ collectOuterBinders (lexicalRefs ++ [ref]) rest

    collectNestedForalls
      :: Maybe TypeBinderRef
      -> [TypeBinderRef]
      -> Ty v
      -> [NestedForallDeclaration]
    collectNestedForalls outerBoundOwner lexicalRefs ty =
      case ty of
        TVarRef {} -> []
        TArrow domain codomain ->
          collectNestedForalls outerBoundOwner lexicalRefs domain
            ++ collectNestedForalls outerBoundOwner lexicalRefs codomain
        TConWithIdentity _ _ arguments ->
          concatMap
            (collectNestedForalls outerBoundOwner lexicalRefs)
            arguments
        TVarAppRef _ arguments ->
          concatMap
            (collectNestedForalls outerBoundOwner lexicalRefs)
            arguments
        TBaseWithIdentity {} -> []
        TForallRef ref mbBound body ->
          NestedForallDeclaration
            { nfdRef = ref
            , nfdBound = mbBound
            , nfdLexicalRefs = lexicalRefs
            , nfdOuterBoundOwner = outerBoundOwner
            }
            : maybe
              []
              (collectNestedForalls outerBoundOwner lexicalRefs)
              mbBound
              ++ collectNestedForalls
                outerBoundOwner
                (lexicalRefs ++ [ref])
                body
        TMuRef ref body ->
          collectNestedForalls
            outerBoundOwner
            (lexicalRefs ++ [ref])
            body
        TBottom -> []

-- | Retain exactly the declarations needed by a set of free references,
-- closing transitively over declaration bounds while preserving lexical
-- order.  This is the binder-context fragment used by the paper's
-- computation contexts; unrelated declarations are removable by Eq-Free.
binderDependencyClosure
  :: [(TypeBinderRef, Maybe BoundType)]
  -> [TypeBinderRef]
  -> [(TypeBinderRef, Maybe BoundType)]
binderDependencyClosure binders initialRefs =
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
          refs' = foldr insertRef refs dependencies
       in if length refs' == length refs then refs else close refs'

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Close a certified body endpoint with exactly the leading packet binders
-- that remain free in it.  The certificate proves the body computation; the
-- packet spine supplies the lexical declarations that computation occurred
-- beneath.  Taking their dependency closure prevents a local parameter from
-- escaping without retaining unrelated, already-consumed packet binders.
closeCompletedPacketEndpoint :: ElabType -> ElabType -> ElabType
closeCompletedPacketEndpoint packetTy endpoint =
  schemeToType
    ( mkElabSchemeWithRefs
        requiredBinders
        endpoint
    )
  where
    packetBinders = schemeBinderRefs (schemeFromType packetTy)
    requiredBinders =
      binderDependencyClosure
        packetBinders
        (freeTypeVarRefsType endpoint)

-- | Project a frozen/base graph node to the solved construction copy published
-- by generalization. Canonicalization alone does not cross this phase boundary:
-- 'gaBaseToSolved' is the provenance certificate that does.
gaConstructionRouteNode
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> NodeId
  -> NodeId
gaConstructionRouteNode canonical ga node =
  canonical
    ( case IntMap.lookup (getNodeId node) (gaBaseToSolved ga) of
        Just solvedNode -> solvedNode
        Nothing ->
          IntMap.findWithDefault
            nodeC
            (getNodeId nodeC)
            (gaBaseToSolved ga)
    )
  where
    nodeC = canonical node

-- | All exact construction copies published for one frozen/base node.
-- 'gaBaseToSolved' supplies the preferred forward projection.  Continue over
-- both directions of the retained Ga mapping until its provenance class is
-- closed: an aligned solved copy may point back to a base node that is only
-- quotient-equal to the preferred projection, and another exact copy may in
-- turn be aligned through that node.  Every step is certified by
-- 'gaBaseToSolved' or 'gaSolvedToBase'; canonicalization only joins the
-- endpoints of those certified steps.  Names and reified type shape never
-- participate.
gaConstructionRouteNodes
  :: (NodeId -> NodeId)
  -> GaBindParents p
  -> NodeId
  -> [NodeId]
gaConstructionRouteNodes canonical ga node =
  close IntSet.empty preferredNodes
  where
    preferredNodes =
      uniqueNodes
        [ gaConstructionRouteNode canonical ga node,
          gaConstructionRouteNode
            canonical
            ga
            (chaseRedirects (gaAnnotationNodeRedirects ga) node)
        ]

    close _ [] = []
    close seen (candidate : remaining)
      | IntSet.member candidateKey seen =
          close seen remaining
      | otherwise =
          candidateC
            : close
              (IntSet.insert candidateKey seen)
              (remaining ++ certifiedSuccessors candidateC)
      where
        candidateC = canonical candidate
        candidateKey = getNodeId candidateC

    certifiedSuccessors current =
      uniqueNodes
        ( IntMap.findWithDefault
            []
            (getNodeId current)
            successorsByBaseClass
            ++ [ redirected
               | let redirected =
                       canonical
                         ( chaseRedirects
                             (gaAnnotationNodeRedirects ga)
                             current
                         )
               , redirected /= current
               ]
        )

    successorsByBaseClass =
      foldl'
        insertSuccessor
        IntMap.empty
        ( [ (NodeId baseKey, solvedNode)
          | (baseKey, solvedNode) <- IntMap.toList (gaBaseToSolved ga)
          ]
            ++ [ (baseNode, NodeId solvedKey)
               | (solvedKey, baseNode) <- IntMap.toList (gaSolvedToBase ga)
               ]
        )

    insertSuccessor successors (baseNode, solvedNode) =
      IntMap.insertWith
        (\new old -> old ++ new)
        (getNodeId (canonical baseNode))
        [canonical solvedNode]
        successors

    uniqueNodes =
      foldl'
        ( \nodes candidate ->
            if candidate `elem` nodes
              then nodes
              else nodes ++ [candidate]
        )
        []

-- | Construction-side selector for the exact result owned by a source
-- lambda.  An application result carries the normalization artifact that
-- allocated its immediate codomain.  A result returned without such an
-- application is selected by the exact source-lambda arrow depth; the
-- generalization planner resolves that depth in its own graph domain.
data GeneralizedResultRouteLocator
  = ApplicationResultConstruction !NodeId
  | LambdaCodomainConstruction !Int
  deriving (Eq, Show)

-- | Construction-side question asked by a source lambda that owns an
-- identity-topology packet.  The request contains only producer-owned graph
-- facts.  Binder selection remains the responsibility of the same
-- generalization plan that finalizes the scheme.
data GeneralizedResultRouteRequest = GeneralizedResultRouteRequest
  { grrrOwnerTarget :: !NodeId
  , grrrFrozenConsumer :: !NodeId
  , grrrRouteLocator :: !GeneralizedResultRouteLocator
  }
  deriving (Eq, Show)

-- | Planner/finalizer certificate for the binder that owns an exact
-- application result.  This joins four authorities that coexist only while a
-- generalization plan is applied: the owner target, selected type root,
-- construction result root, and finalized binder substitution.
data GeneralizedResultRoute = GeneralizedResultRoute
  { grrOwnerTarget :: !NodeId
  , grrTypeRoot :: !NodeId
  , grrFrozenConsumer :: !NodeId
  , grrConstructionRoot :: !NodeId
  , grrBinderNode :: !NodeId
  , grrBinderRef :: !TypeBinderRef
  }
  deriving (Eq, Show)

-- | The exact source lambda at which a retained identity-topology proof can
-- publish its frozen result route.  Only result-transparent source wrappers
-- are removed while finding this boundary; an unfold or another expression
-- form must construct its own result.
data SourceLambdaTopologyBoundary = SourceLambdaTopologyBoundary
  { sltbBodyEdge :: !EdgeId
  , sltbScopeRoot :: !GenNodeId
  , sltbBodyRoot :: !NodeId
  , sltbConstructionRouteSites :: ![InstantiationSite]
  , sltbLambdaNode :: !NodeId
  , sltbLambdaArity :: !Int
  }
  deriving (Eq, Show)

sourceLambdaTopologyBoundary
  :: AnnExpr
  -> Maybe SourceLambdaTopologyBoundary
sourceLambdaTopologyBoundary sourceAnn =
  case sourceAnn of
    ALam _ details _ scopeRoot body bodyEdge lambdaNode ->
      let boundaryBody =
            maybe
              body
              snd
              (desugaredAnnotatedLambdaBody details body)
          boundaryBodyRoot = sourceExpressionNode boundaryBody
       in Just
            SourceLambdaTopologyBoundary
              { sltbBodyEdge = bodyEdge
              , sltbScopeRoot = scopeRoot
              , sltbBodyRoot = boundaryBodyRoot
              , sltbConstructionRouteSites =
                  sourceResultConstructionRouteSites
                    boundaryBodyRoot
                    boundaryBody
              , sltbLambdaNode = lambdaNode
              , sltbLambdaArity =
                  sourceLambdaArity details body
              }
    ALet _ _ _ _ _ _ _ body _ ->
      sourceLambdaTopologyBoundary body
    AAnn inner _ _ ->
      sourceLambdaTopologyBoundary inner
    AExactAnn inner _ _ _ ->
      sourceLambdaTopologyBoundary inner
    ALetScope inner _ _ ->
      sourceLambdaTopologyBoundary inner
    _ -> Nothing
  where
    sourceExpressionNode ann =
      case ann of
        AResolvedVar _ _ node -> node
        ALit _ node -> node
        ALam _ _ _ _ _ _ node -> node
        AApp _ _ _ _ node -> node
        ALet _ _ _ _ _ _ _ _ node -> node
        AAnn _ node _ -> node
        AExactAnn _ _ node _ -> node
        ALetScope _ node _ -> node
        AUnfold _ node _ -> node

    -- Count only the value abstractions proved to be one direct source
    -- lambda spine.  The shared administrative-lambda recognizer is also
    -- used during packet preparation, so this depth cannot drift into a
    -- generic search for the last arrow in a reified type.
    sourceLambdaArity details body =
      1
        + case administrativeLambdaBody details body of
          Just (ALam _ nestedDetails _ _ nestedBody _ _) ->
            sourceLambdaArity nestedDetails nestedBody
          _ -> 0

    -- Retain the exact function-instantiation occurrence whose result is
    -- returned through transparent wrappers.  The site selects one
    -- normalization-graft construction record; that record, rather than the
    -- target's fresh existential codomain, owns the result copy.
    sourceResultConstructionRouteSites expectedRoot ann
      | sourceExpressionNode ann /= expectedRoot = []
      | otherwise =
          case ann of
            AApp _ _ functionSite _ _
              | ArrowInstantiationTarget
                  { instantiationArrowCodomain = codomain
                  } <-
                  instantiationSiteTargetTopology functionSite
              , codomain == expectedRoot ->
                  [functionSite]
            ALet _ _ _ _ _ _ _ body _ ->
              sourceResultConstructionRouteSites expectedRoot body
            AAnn inner _ _ ->
              sourceResultConstructionRouteSites expectedRoot inner
            AExactAnn inner _ _ _ ->
              sourceResultConstructionRouteSites expectedRoot inner
            ALetScope inner _ _ ->
              sourceResultConstructionRouteSites expectedRoot inner
            _ -> []

-- | Select the exact source lambda named by a topology certificate along the
-- result path.  An enclosing lambda can own the overall expression while a
-- deeper returned lambda owns the packet.  Falling back to the first boundary
-- preserves a precise provenance error when a malformed certificate names no
-- source lambda at all.
sourceLambdaTopologyBoundaryFor
  :: IdentityTopologyConsumerAuthority
  -> AnnExpr
  -> Maybe SourceLambdaTopologyBoundary
sourceLambdaTopologyBoundaryFor authority sourceAnn =
  case findOwnerBoundary sourceAnn of
    Just boundary -> Just boundary
    Nothing -> sourceLambdaTopologyBoundary sourceAnn
  where
    ownerLambdaNode = lgoTermNode (itcaOwner authority)

    findOwnerBoundary ann =
      case ann of
        ALam _ _ _ _ body _ lambdaNode
          | lambdaNode == ownerLambdaNode ->
              sourceLambdaTopologyBoundary ann
          | otherwise -> findOwnerBoundary body
        ALet _ _ _ _ _ _ _ body _ -> findOwnerBoundary body
        AAnn inner _ _ -> findOwnerBoundary inner
        AExactAnn inner _ _ _ -> findOwnerBoundary inner
        ALetScope inner _ _ -> findOwnerBoundary inner
        _ -> Nothing

-- | Build the graph-side half of a generalized-result certificate for the
-- exact application returned by a source lambda.  One application consumes
-- one arrow, so the construction root is the copy of that arrow's immediate
-- codomain.  Walking to the last arrow or last variable would conflate the
-- application constructor with the returned function's own structure.
sourceLambdaGeneralizedResultRouteRequest
  :: GaBindParents p
  -> AnnExpr
  -> SubtermGeneralizations
  -> Either ElabError (Maybe GeneralizedResultRouteRequest)
sourceLambdaGeneralizedResultRouteRequest
  ga
  sourceAnn
  packets =
    case topologyAuthorities of
      [] -> pure Nothing
      [topologyAuthority] -> do
        boundary <-
          case
              sourceLambdaTopologyBoundaryFor
                topologyAuthority
                sourceAnn
            of
            Just present -> pure present
            Nothing ->
              requestFailure
                "owned topology packet is not at a source-lambda boundary"
                ["  authority: " ++ show topologyAuthority]
        validateSourceLambdaTopologyBoundary
          sourceAnn
          topologyAuthority
          boundary
        routeLocator <-
          case sltbConstructionRouteSites boundary of
            [] ->
              pure
                (LambdaCodomainConstruction (sltbLambdaArity boundary))
            [site] -> do
              constructionRoot <-
                case applicationResultConstructionRoot ga site of
                  Right root -> pure root
                  Left cause ->
                    requestFailure
                      "source-lambda result application has no exact construction route"
                      [ "  authority: " ++ show topologyAuthority
                      , "  site: " ++ show site
                      , "  cause: " ++ show cause
                      ]
              pure (ApplicationResultConstruction constructionRoot)
            sites ->
              requestFailure
                "source lambda returns multiple exact application results"
                ["  sites: " ++ show sites]
        pure
          ( Just
              GeneralizedResultRouteRequest
                { grrrOwnerTarget =
                    lgoTermNode (itcaOwner topologyAuthority)
                , grrrFrozenConsumer =
                    itcaFrozenResultRoot topologyAuthority
                , grrrRouteLocator = routeLocator
                }
          )
      authorities ->
        requestFailure
          "source lambda owns multiple identity-topology certificates"
          ["  authorities: " ++ show authorities]
  where
    topologyAuthorities =
      [ topologyAuthority
      | packet <- Map.elems packets
      , Just (TopologyConsumerAuthority topologyAuthority) <-
          [psgConsumerAuthority packet]
      ]

    requestFailure
      :: String
      -> [String]
      -> Either ElabError a
    requestFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid source-lambda generalized-result request"
              , "  detail: " ++ detail
              , "  source: " ++ show sourceAnn
              ]
                ++ context
            )
        )

applicationResultConstructionRoot
  :: GaBindParents p
  -> InstantiationSite
  -> Either ElabError NodeId
applicationResultConstructionRoot ga site =
  case
      IntMap.lookup
        edgeKey
        (cGraftResultConstructions baseConstraint)
    of
    Just construction -> graftedConstructionRoot construction
    Nothing ->
      case retainedArrowConstructionRoot of
        Just constructionRoot -> pure constructionRoot
        Nothing ->
          routeFailure
            "missing exact application graft construction"
            [ "  edge: " ++ show siteEdge
            , "  normalized target node: "
                    ++ show
                      ( lookupNodeIn
                          (cNodes baseConstraint)
                          (instantiationSiteTarget site)
                      )
                , "  allocated codomain construction routes: "
                    ++ show allocatedCodomainConstructionRoutes
                , "  base-to-solved routes: " ++ show (gaBaseToSolved ga)
            , "  solved-to-base routes: " ++ show (gaSolvedToBase ga)
            ]
  where
    baseConstraint = gaBaseConstraint ga
    siteEdge = instantiationSiteEdgeId site
    edgeKey = getEdgeId siteEdge

    -- Normalization records a fresh construction only when it grafts an
    -- arrow onto a variable.  If the edge instead canonicalizes directly
    -- onto an already allocated arrow, the normalized instantiation site is
    -- itself the construction certificate.  Its source and target are the
    -- same exact node, the allocation-side target retains the exact arrow,
    -- and the result endpoint must follow the allocation codomain's Ga route.
    -- The normalized domain is validated independently by application replay;
    -- it may be quotient-equal to the argument through ordinary unification
    -- without being a Ga construction copy of the fresh allocated domain.
    retainedArrowConstructionRoot =
      case instantiationSiteTargetTopology site of
        ArrowInstantiationTarget
          { instantiationArrowAllocatedDomain = allocatedDomain,
            instantiationArrowAllocatedCodomain = allocatedCodomain,
            instantiationArrowCodomain = codomain
          }
            | instantiationSiteSource site
                == instantiationSiteTarget site
            , Just TyArrow
                { tnDom = retainedDomain,
                  tnCod = retainedCodomain
                } <-
                lookupNodeIn
                  (cNodes baseConstraint)
                  (instantiationSiteAllocatedTarget site)
            , retainedDomain == allocatedDomain
            , retainedCodomain == allocatedCodomain
            , constructionNodesAgree allocatedCodomain codomain ->
                Just codomain
        _ -> Nothing

    constructionNodesAgree baseNode solvedNode =
      solvedNode
        `elem` gaConstructionRouteNodes id ga baseNode

    allocatedCodomainConstructionRoutes =
      case instantiationSiteTargetTopology site of
        ArrowInstantiationTarget
          { instantiationArrowAllocatedCodomain = codomain
          } ->
            gaConstructionRouteNodes id ga codomain
        AtomicInstantiationTarget -> []

    graftedConstructionRoot construction = do
      (targetDomain, targetCodomain) <-
        case instantiationSiteTargetTopology site of
          ArrowInstantiationTarget
            { instantiationArrowAllocatedDomain = domain,
              instantiationArrowAllocatedCodomain = codomain
            } -> pure (domain, codomain)
          AtomicInstantiationTarget ->
            routeFailure
              "exact application site has no allocated arrow topology"
              []
      unless
        ( grcEdgeId construction == siteEdge
            && grcSourceRoot construction
              == instantiationSiteAllocatedSource site
            && grcTargetRoot construction
              == instantiationSiteAllocatedTarget site
            && grcTargetDomain construction == targetDomain
            && grcTargetCodomain construction == targetCodomain
        )
        ( routeFailure
            "graft construction does not match the exact allocated application site"
            [ "  construction: " ++ show construction
            , "  allocated source: "
                ++ show (instantiationSiteAllocatedSource site)
            , "  allocated target: "
                ++ show (instantiationSiteAllocatedTarget site)
            , "  allocated target domain: " ++ show targetDomain
            , "  allocated target codomain: " ++ show targetCodomain
            ]
        )
      case
          ( grcSourceBoundRoot construction
          , grcSourceResultRoot construction
          )
        of
        (Just _, Nothing) ->
          routeFailure
            "graft construction has no source-bound result authority"
            [ "  construction: " ++ show construction
            ]
        (Nothing, Just _) ->
          routeFailure
            "unbound graft construction unexpectedly has source-result authority"
            [ "  construction: " ++ show construction
            ]
        _ -> do
          let constructionRoot = grcConstructionCodomain construction
          case lookupNodeIn (cNodes baseConstraint) constructionRoot of
            Just _ -> pure constructionRoot
            Nothing ->
              routeFailure
                "graft construction result is absent from the prepared graph"
                [ "  construction: " ++ show construction
                , "  result root: " ++ show constructionRoot
                ]

    routeFailure
      :: String
      -> [String]
      -> Either ElabError a
    routeFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid exact application result construction route"
              , "  detail: " ++ detail
              , "  site: " ++ show site
              ]
                ++ context
            )
        )

validateSourceLambdaTopologyBoundary
  :: AnnExpr
  -> IdentityTopologyConsumerAuthority
  -> SourceLambdaTopologyBoundary
  -> Either ElabError ()
validateSourceLambdaTopologyBoundary sourceAnn topologyAuthority boundary = do
  let owner = itcaOwner topologyAuthority
  unless
    (itcaEdgeId topologyAuthority == sltbBodyEdge boundary)
    (boundaryFailure "source body edge changed")
  unless
    (itcaBoundaryScopeRoot topologyAuthority == sltbScopeRoot boundary)
    (boundaryFailure "source lambda scope changed")
  unless
    (itcaBoundaryBodyRoot topologyAuthority == sltbBodyRoot boundary)
    (boundaryFailure "source body root changed")
  unless
    (lgoConstructor owner == LocalLambdaGamma)
    (boundaryFailure "recorded owner is not a lambda")
  unless
    (lgoBoundaryEdge owner == sltbBodyEdge boundary)
    (boundaryFailure "recorded owner edge changed")
  unless
    (lgoScope owner == GenRef (sltbScopeRoot boundary))
    (boundaryFailure "recorded owner scope changed")
  unless
    (lgoTermNode owner == sltbLambdaNode boundary)
    (boundaryFailure "recorded lambda node changed")
  where
    boundaryFailure
      :: String
      -> Either ElabError a
    boundaryFailure detail =
      Left
        ( ValidationFailed
            [ "invalid source-lambda identity-topology boundary"
            , "  detail: " ++ detail
            , "  source: " ++ show sourceAnn
            , "  authority: " ++ show topologyAuthority
            ]
        )

-- | Discharge an identity-topology certificate exactly where the annotated
-- source still identifies its owning lambda.  At this point the generalized
-- scheme has also published the binder selected by the planner from the exact
-- result node captured when normalization constructed the application arrow.
-- That construction provenance selects the consumer identity; the source
-- boundary and scheme only validate that it is the unique binder occurring in
-- the lambda's exact codomain.  No consumer is guessed from type shape.
publishSourceLambdaTopologyConsumerRoute
  :: Maybe GeneralizedResultRoute
  -> (NodeId -> [NodeId])
  -> AnnExpr
  -> SubtermGeneralizations
  -> SchemeInfo
  -> Either ElabError SchemeInfo
publishSourceLambdaTopologyConsumerRoute
  generalizedResultRoute
  constructionRouteNodes
  sourceAnn
  packets
  schemeInfo =
  case topologyAuthorities of
    [] -> pure schemeInfo
    [topologyAuthority] -> do
      boundary <-
        case
            sourceLambdaTopologyBoundaryFor
              topologyAuthority
              sourceAnn
          of
          Just present -> pure present
          Nothing ->
            publicationFailure
              topologyAuthority
              "owned topology packet is not at a source-lambda boundary"
              []
      case
          validateSourceLambdaTopologyBoundary
            sourceAnn
            topologyAuthority
            boundary
        of
          Right () -> pure ()
          Left cause ->
            publicationFailure
              topologyAuthority
              "source-lambda boundary does not match its topology authority"
              ["  cause: " ++ show cause]
      let generalizedBody = schemeBody (siScheme schemeInfo)
          (leadingTypeAbstractions, lambdaCore) =
            splitForallsRefs generalizedBody
      exactCodomain <-
        sourceLambdaCodomain
          topologyAuthority
          boundary
          leadingTypeAbstractions
          lambdaCore
      routedRef <-
        constructionConsumerRef topologyAuthority boundary
      publishedRef <-
        case exactConsumerBinders routedRef exactCodomain of
          [binderRef] -> pure binderRef
          binders ->
            publicationFailure
              topologyAuthority
              "construction consumer is not one unique free scheme binder"
              [ "  leading type abstractions: "
                  ++ show leadingTypeAbstractions
              , "  exact source-lambda arity: "
                  ++ show (sltbLambdaArity boundary)
              , "  exact codomain: " ++ show exactCodomain
              , "  routed construction consumer: "
                  ++ show routedRef
              , "  free codomain identities: "
                  ++ show (freeTypeVarRefsType exactCodomain)
              , "  matching binders: " ++ show binders
              , "  generalized body: " ++ show generalizedBody
              , "  generalized result certificate: "
                  ++ show generalizedResultRoute
              , "  generalized routes: " ++ show (siSubstRefs schemeInfo)
              ]
      let frozenResultKey =
            getNodeId (itcaFrozenResultRoot topologyAuthority)
      routes <-
        case IntMap.lookup frozenResultKey (siSubstRefs schemeInfo) of
          Nothing ->
            pure
              ( IntMap.insert
                  frozenResultKey
                  publishedRef
                  (siSubstRefs schemeInfo)
              )
          Just existingRef
            | typeBinderRefsSameIdentity existingRef publishedRef ->
                pure (siSubstRefs schemeInfo)
          Just existingRef ->
            publicationFailure
              topologyAuthority
              "frozen result already routes to a different scheme binder"
              [ "  existing route: " ++ show existingRef
              , "  source-lambda codomain: " ++ show publishedRef
              ]
      pure schemeInfo {siSubstRefs = routes}
    authorities ->
      Left
        ( ValidationFailed
            [ "source lambda owns multiple identity-topology certificates"
            , "  source: " ++ show sourceAnn
            , "  authorities: " ++ show authorities
            ]
        )
  where
    topologyAuthorities =
      [ topologyAuthority
      | packet <- Map.elems packets
      , Just (TopologyConsumerAuthority topologyAuthority) <-
          [psgConsumerAuthority packet]
      ]

    constructionConsumerRef topologyAuthority boundary =
      case generalizedResultRoute of
        Just route -> do
          validateCertifiedRoute topologyAuthority route
          pure (grrBinderRef route)
        Nothing
          | not (null exactConstructionSites) ->
              publicationFailure
                topologyAuthority
                "exact application result has no planner certificate"
                [ "  exact source construction sites: "
                    ++ show exactConstructionSites
                ]
          | otherwise ->
              directConstructionConsumerRef
      where
        sourceRoot = itcaSourceBodyRoot topologyAuthority
        exactConstructionSites =
          sltbConstructionRouteSites boundary

        routedCandidates =
          [ (constructionNode, routedRef)
          | constructionNode <- constructionRouteNodes sourceRoot
          , Just routedRef <-
              [ IntMap.lookup
                  (getNodeId constructionNode)
                  (siSubstRefs schemeInfo)
              ]
          ]

        directConstructionConsumerRef =
          case routedCandidates of
            [] ->
              publicationFailure
                topologyAuthority
                "certified source root has no generalized construction route"
                [ "  certified source root: " ++ show sourceRoot
                , "  construction route nodes: "
                    ++ show (constructionRouteNodes sourceRoot)
                , "  frozen result root: "
                    ++ show (itcaFrozenResultRoot topologyAuthority)
                , "  frozen construction route nodes: "
                    ++ show
                      ( constructionRouteNodes
                          (itcaFrozenResultRoot topologyAuthority)
                      )
                , "  topology packet routes: "
                    ++ show
                      [ siSubstRefs (psgSchemeInfo packet)
                      | packet <- Map.elems packets
                      , psgConsumerAuthority packet
                          == Just
                            (TopologyConsumerAuthority topologyAuthority)
                      ]
                , "  generalized routes: " ++ show (siSubstRefs schemeInfo)
                ]
            (_, firstRef) : remaining
              | all
                  (typeBinderRefsSameIdentity firstRef . snd)
                  remaining ->
                  pure firstRef
              | otherwise ->
                  publicationFailure
                    topologyAuthority
                    "certified source root has conflicting generalized construction routes"
                    [ "  certified source root: " ++ show sourceRoot
                    , "  routed candidates: " ++ show routedCandidates
                    ]

        validateCertifiedRoute authority route = do
          let owner = itcaOwner authority
          unless
            (grrOwnerTarget route == lgoTermNode owner)
            ( certifiedRouteFailure
                "certificate owner differs from the source lambda"
                route
            )
          unless
            (grrFrozenConsumer route
                == itcaFrozenResultRoot authority)
            ( certifiedRouteFailure
                "certificate frozen consumer differs from the topology authority"
                route
            )
          case
              IntMap.lookup
                (getNodeId (grrBinderNode route))
                (siSubstRefs schemeInfo)
            of
              Just published
                | typeBinderRefsSameIdentity
                    published
                    (grrBinderRef route) ->
                    pure ()
              published ->
                publicationFailure
                  authority
                  "planner certificate is absent from the generalized routes"
                  [ "  certificate: " ++ show route
                  , "  route at binder node: " ++ show published
                  , "  generalized routes: " ++ show (siSubstRefs schemeInfo)
                  ]

        certifiedRouteFailure detail route =
          publicationFailure
            topologyAuthority
            detail
            [ "  certificate: " ++ show route
            ]

    sourceLambdaCodomain
      topologyAuthority
      boundary
      leadingTypeAbstractions
      lambdaCore =
        go (sltbLambdaArity boundary) lambdaCore
      where
        go remaining ty
          | remaining <= 0 = pure ty
        go remaining (TArrow _ codomain) =
          go (remaining - 1) codomain
        go remaining ty =
          publicationFailure
            topologyAuthority
            "generalized lambda arrow spine ended before the exact source lambda"
            [ "  leading type abstractions: "
                ++ show leadingTypeAbstractions
            , "  exact source-lambda arity: "
                ++ show (sltbLambdaArity boundary)
            , "  remaining value abstractions: " ++ show remaining
            , "  non-arrow core: " ++ show ty
            ]

    -- The graph result projection, not the completed type shape, selects the
    -- consumer identity.  The source boundary then proves that this exact
    -- identity occurs freely in the body result and the generalized scheme
    -- proves that it denotes one binder.  The body may return a function; no
    -- arrow-tail search is performed here.
    exactConsumerBinders routedRef exactCodomain =
      [ binderRef
      | (binderRef, _) <-
          schemeBinderRefs (siScheme schemeInfo)
            ++ fst (splitForallsRefs (schemeBody (siScheme schemeInfo)))
      , typeBinderRefsSameIdentity binderRef routedRef
      , any (typeBinderRefsSameIdentity routedRef)
          (freeTypeVarRefsType exactCodomain)
      ]

    publicationFailure
      :: IdentityTopologyConsumerAuthority
      -> String
      -> [String]
      -> Either ElabError a
    publicationFailure topologyAuthority detail context =
      Left
        ( ValidationFailed
            ( [ "invalid source-lambda identity-topology publication"
              , "  detail: " ++ detail
              , "  source: " ++ show sourceAnn
              , "  authority: " ++ show topologyAuthority
              , "  scheme: " ++ show (siScheme schemeInfo)
              ]
                ++ context
            )
        )

-- | Publish the frozen consumer key retained by an identity-topology packet
-- through the enclosing construction's exact phase-crossing route.
--
-- Packet preparation records the result identity at the topology boundary,
-- while later generalization can expose a copied construction node in
-- 'SchemeInfo'. The caller supplies the base-to-solved provenance projection;
-- this function preserves that proof as an explicit route for placement. It
-- never guesses a binder from its name, shape, or position.
publishTopologyConsumerRoutes
  :: (NodeId -> [NodeId])
  -> SubtermGeneralizations
  -> SchemeInfo
  -> Either ElabError SchemeInfo
publishTopologyConsumerRoutes constructionRouteNodes packets schemeInfo = do
  routes <-
    foldM
      publishPacketRoute
      (siSubstRefs schemeInfo)
      (Map.elems packets)
  pure schemeInfo {siSubstRefs = routes}
  where
    publishPacketRoute routes packet =
      case subtermGeneralizationConsumerAuthority packet of
        Just authority
          | subtermConsumerAuthorityIsTopology authority ->
              case typeBinderIdentityNode (scaConsumerIdentity authority) of
                Just consumerNode -> do
                  consumerRoutes <-
                    publishConsumerRoute packet consumerNode routes
                  publishPacketBinderRoutes
                    packet
                    consumerNode
                    consumerRoutes
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "identity-topology packet consumer is not graph-backed"
                        , "  consumer identity: "
                            ++ show (scaConsumerIdentity authority)
                        , "  consumer edge: " ++ show (scaEdgeId authority)
                        ]
                    )
        _ -> pure routes

    -- The generalization plan can expose packet-owned declarations before the
    -- exact topology consumer and use them in that consumer's bound.  Pair the
    -- packet's declaration spine with the plan's non-ambient bound dependency
    -- spine while both are still explicit.  The checked specialization proves
    -- that this packet constructs the bound; binder order and lexical
    -- dependency then publish the identity routes consumed by placement.
    publishPacketBinderRoutes packet consumerNode routes =
      case IntMap.lookup (getNodeId consumerNode) routes of
        Nothing -> pure routes
        Just routedConsumerRef ->
          publishForConsumer
          where
            enclosingBinders = schemeBinderRefs (siScheme schemeInfo)
            packetTy =
              schemeToType
                (subtermGeneralizationGammaBoundScheme packet)
            packetBinderRefs = boundRefsInType packetTy
            packetLexicalRefs =
              freeTypeVarRefsType packetTy
                ++ [ routedRef
                   | packetRef <- freeTypeVarRefsType packetTy
                   , packetNode <- maybeToList (typeBinderRefNode packetRef)
                   , routedRef <-
                       maybeToList
                         (IntMap.lookup (getNodeId packetNode) routes)
                   ]

            publishForConsumer =
              case
                  find
                    (typeBinderRefsSameIdentity routedConsumerRef . fst)
                    enclosingBinders
                of
                  Just (_, Just consumerBound)
                    | let packetConstructionDependencies =
                            constructionDependencies consumerBound
                    , not (null packetBinderRefs)
                    , length packetBinderRefs
                        == length packetConstructionDependencies
                    , packetTypeSpecializesToExactEndpoint
                        packetTy
                        (tyToElab consumerBound) ->
                        foldM
                          publishPacketBinderRoute
                          routes
                          ( zip
                              packetBinderRefs
                              packetConstructionDependencies
                          )
                  _ -> pure routes

            constructionDependencies consumerBound =
              [ dependencyRef
              | (dependencyRef, _) <- bindersBeforeConsumer
              , any
                  (typeBinderRefsSameIdentity dependencyRef)
                  closedConsumerBoundDependencies
              , not
                  ( any
                      (typeBinderRefsSameIdentity dependencyRef)
                      packetLexicalRefs
                  )
              ]
              where
                bindersBeforeConsumer =
                  takeWhile
                    (not . typeBinderRefsSameIdentity routedConsumerRef . fst)
                    enclosingBinders
                closedConsumerBoundDependencies =
                  closeDependencies
                    (freeTypeVarRefsType (tyToElab consumerBound))
                closeDependencies dependencies =
                  let boundDependencies =
                        [ boundDependency
                        | (binderRef, Just binderBound) <-
                            bindersBeforeConsumer
                        , any
                            (typeBinderRefsSameIdentity binderRef)
                            dependencies
                        , boundDependency <-
                            freeTypeVarRefsType (tyToElab binderBound)
                        ]
                      dependencies' =
                        foldr insertDistinctRef dependencies boundDependencies
                   in if length dependencies' == length dependencies
                        then dependencies
                        else closeDependencies dependencies'
                insertDistinctRef ref refs
                  | any (typeBinderRefsSameIdentity ref) refs = refs
                  | otherwise = ref : refs

    publishPacketBinderRoute routes (packetRef, constructionRef) =
      case typeBinderRefNode packetRef of
        Nothing -> pure routes
        Just packetNode ->
          case IntMap.lookup packetKey routes of
            Nothing ->
              pure (IntMap.insert packetKey constructionRef routes)
            Just existingRef
              | typeBinderRefsSameIdentity existingRef constructionRef ->
                  pure routes
            Just existingRef ->
              Left
                ( ValidationFailed
                    [ "identity-topology packet binder route conflicts with its construction-plan dependency"
                    , "  packet binder: " ++ show packetRef
                    , "  existing route: " ++ show existingRef
                    , "  planned dependency: " ++ show constructionRef
                    ]
                )
          where
            packetKey = getNodeId packetNode

    publishConsumerRoute packet consumerNode routes =
      case candidateRoutes of
        [] -> pure routes
        (_, firstRef) : remaining
          | all
              (typeBinderRefsSameIdentity firstRef . snd)
              remaining ->
              pure (IntMap.insert consumerKey firstRef routes)
          | otherwise ->
              routeConflict candidateRoutes
      where
        consumerKey = getNodeId consumerNode
        candidateRoutes =
          maybeToList
            ( (\consumerRef -> (consumerNode, consumerRef))
                <$> IntMap.lookup consumerKey routes
            )
            ++ [ (constructionNode, constructionRef)
               | constructionNode <- packetConstructionRouteNodes
               , Just constructionRef <-
                   [ IntMap.lookup
                       (getNodeId constructionNode)
                       routes
                   ]
               ]
        -- Packet preparation can retain the frozen topology consumer under
        -- an intermediate graph identity before enclosing generalization
        -- creates its final construction copy.  Compose those two certified
        -- hops explicitly:
        --
        -- frozen consumer -> packet SchemeInfo ref -> Ga construction copy.
        --
        -- Every hop remains graph-backed and exact; a missing side simply
        -- contributes no candidate, while competing completed routes are
        -- rejected by the existing conflict check above.
        packetConsumerRouteNodes =
          maybeToList
            ( IntMap.lookup
                consumerKey
                (siSubstRefs (psgSchemeInfo packet))
                >>= typeBinderRefNode
            )
        packetConstructionRouteNodes =
          dedupeNodes
            ( constructionRouteNodes consumerNode
                ++ concatMap
                  constructionRouteNodes
                  packetConsumerRouteNodes
            )
        dedupeNodes =
          foldl'
            ( \nodes candidate ->
                if candidate `elem` nodes
                  then nodes
                  else nodes ++ [candidate]
            )
            []
        routeConflict conflictingRoutes =
          Left
            ( ValidationFailed
                [ "identity-topology consumer route conflicts with its construction provenance route"
                , "  frozen consumer: " ++ show consumerNode
                , "  routed construction candidates: "
                    ++ show conflictingRoutes
                ]
            )

placeSubtermGeneralizationBinders
  :: SubtermGeneralizations
  -> ElabScheme
  -> Either ElabError ElabScheme
placeSubtermGeneralizationBinders =
  placeSubtermGeneralizationBindersWithRoutes IntMap.empty

-- | Result of installing prepared subterm packets into an enclosing scheme.
-- The copied-binder routes are keyed by the exact consumer declaration that
-- received the packet bound.  They are construction provenance for that
-- bound, not a general alpha-equivalence map.
data PlacedSubtermBinders = PlacedSubtermBinders
  { placedSubtermBinderScheme :: !ElabScheme
  , placedSubtermCopiedBinderRoutes ::
      !(Map.Map TypeBinderIdentity [(TypeBinderIdentity, TypeBinderRef)])
  -- | Exact enclosing consumers whose bound was constructed by moving this
  -- packet's planned dependency spine into that bound.  This is positive
  -- construction provenance; ordinary whole-packet placement can also copy
  -- binders, so copied routes alone do not prove that the enclosing term must
  -- construct a flexible result.
  , placedSubtermConstructedConsumerIdentities ::
      !(Set.Set TypeBinderIdentity)
  }
  deriving (Eq, Show)

-- | Place packets using the identity-bearing node routes produced alongside
-- the enclosing scheme.  A consumer is recorded in the frozen graph domain;
-- generalization may publish a distinct construction identity for that node.
-- The route is the only authority for crossing those domains.  The route must
-- still land on exactly one binder in the scheme.  Ordinary consumers require
-- that binder to be bounded already.  A validated identity-topology packet may
-- construct its initially pending bound from its prepared S'(operated); the
-- child is checked independently against that same expectation before the
-- local term constructor is emitted.
placeSubtermGeneralizationBindersWithRoutes
  :: IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> ElabScheme
  -> Either ElabError ElabScheme
placeSubtermGeneralizationBindersWithRoutes consumerRoutes subtermPackets rootScheme =
  placedSubtermBinderScheme
    <$> placeSubtermGeneralizationBindersWithRoutesAndProvenance
      consumerRoutes
      subtermPackets
      rootScheme

-- | Carry the exact construction-order authority of packet binders that
-- survive placement into the enclosing 'SchemeInfo'.  Packet placement is a
-- type construction boundary: a fresh bounded exterior may deliberately lose
-- its graph 'BindParent' after completion, while its position remains fixed by
-- the prepared packet spine.  Rebuilding a bare scheme/substitution pair at
-- this boundary would discard that proof and make Phi consult a stale @<P@
-- relation.
--
-- Only a declaration retained in the final scheme and an exact final route
-- for that same semantic identity are published.  Renamed packet binders are
-- included only through the packet's explicit construction-renaming
-- certificate; unrelated graph binders continue to use ordinary @<P@ order.
publishPlacedSubtermConstructionBinderOrder
  :: SubtermGeneralizations
  -> SchemeInfo
  -> SchemeInfo
publishPlacedSubtermConstructionBinderOrder packets schemeInfo =
  schemeInfo
    { siConstructionBinderOrderRefs =
        IntMap.union
          placedPacketOrderRefs
          (siConstructionBinderOrderRefs schemeInfo)
    }
  where
    declaredRefs =
      typeBinderDeclarationRefs
        (schemeToType (siScheme schemeInfo))
    packetOrderRefs =
      distinctRefs
        ( concatMap
            packetConstructionRefs
            (Map.elems packets)
        )
    packetConstructionRefs packet =
      let ownedRefs =
            IntMap.elems
              ( siConstructionBinderOrderRefs
                  (subtermGeneralizationSchemeInfo packet)
              )
          renamedRefs =
            [ targetRef
            | (sourceRef, targetRef) <-
                subtermGeneralizationConstructionBinderRenames packet
            , any
                (typeBinderRefsSameIdentity sourceRef)
                ownedRefs
            ]
       in ownedRefs ++ renamedRefs
    retainedPacketRefs =
      [ declaredRef
      | declaredRef <- declaredRefs
      , any
          (typeBinderRefsSameIdentity declaredRef)
          packetOrderRefs
      ]
    placedPacketOrderRefs =
      IntMap.fromList
        [ (nodeKey, declaredRef)
        | declaredRef <- retainedPacketRefs
        , nodeKey <- orderRouteKeys declaredRef
        , Just routedRef <-
            [IntMap.lookup nodeKey (siSubstRefs schemeInfo)]
        , typeBinderRefsSameIdentity declaredRef routedRef
        ]
    orderRouteKeys ref =
      case typeBinderRefNode ref of
        Just node -> [getNodeId node]
        Nothing ->
          [ nodeKey
          | (nodeKey, routedRef) <-
              IntMap.toList (siSubstRefs schemeInfo)
          , typeBinderRefsSameIdentity ref routedRef
          ]
    distinctRefs = foldr insertDistinct []
    insertDistinct ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

placeSubtermGeneralizationBindersWithRoutesAndProvenance
  :: IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> ElabScheme
  -> Either ElabError PlacedSubtermBinders
placeSubtermGeneralizationBindersWithRoutesAndProvenance =
  placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy
    (\_ _ _ _ -> Nothing)

-- | Place prepared packets with an additional positive construction proof for
-- an already completed packet bound.  The callback receives the exact packet,
-- its selected consumer ref, the packet bound after construction-domain/free
-- reference alignment, and the currently constructed target bound.  Returning
-- a completed endpoint installs that exact bound.  Placement closes only the
-- packet declarations on which that endpoint is still free, including their
-- transitive bound dependencies; unrelated declarations from the stale packet
-- spine remain omitted.  This lets a source constructor publish the closed
-- endpoint it has already checked even when the frozen graph still exposes the
-- packet's open operated view.  Callers cannot use this hook to select a
-- consumer or perform alignment; those remain owned by the ordinary checked
-- placement path below.
placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy
  :: ( PreparedSubtermGeneralization
       -> TypeBinderRef
       -> ElabType
       -> ElabType
       -> Maybe ElabType
     )
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> ElabScheme
  -> Either ElabError PlacedSubtermBinders
placeSubtermGeneralizationBindersWithRoutesAndProvenanceBy completedBoundFor consumerRoutes subtermPackets rootScheme =
  placePackets
    rootScheme
    Map.empty
    Set.empty
    [ packet
    | packet <- Map.elems subtermPackets
    , not (subtermGeneralizationOwnsGammaEdge packet)
    ]
  where
    placePackets scheme copiedRoutes constructedConsumers [] =
      Right
        PlacedSubtermBinders
          { placedSubtermBinderScheme = scheme
          , placedSubtermCopiedBinderRoutes = copiedRoutes
          , placedSubtermConstructedConsumerIdentities =
              constructedConsumers
          }
    placePackets scheme copiedRoutes constructedConsumers (packet : rest) = do
      (scheme', mbCopiedRoutes, mbConstructedConsumer) <-
        placePacket scheme packet
      copiedRoutes' <-
        maybe
          (pure copiedRoutes)
          (insertCopiedRoutes copiedRoutes)
          mbCopiedRoutes
      let constructedConsumers' =
            maybe
              constructedConsumers
              (`Set.insert` constructedConsumers)
              mbConstructedConsumer
      placePackets
        scheme'
        copiedRoutes'
        constructedConsumers'
        rest

    insertCopiedRoutes copiedRoutes (targetRef, routes) =
      let targetIdentity = typeBinderRefIdentity targetRef
       in case Map.lookup targetIdentity copiedRoutes of
            Nothing ->
              pure (Map.insert targetIdentity routes copiedRoutes)
            Just existing
              | copiedRoutesAgree existing routes ->
                  pure copiedRoutes
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "one enclosing Gamma binder has conflicting packet-copy provenance"
                        , "  consumer: " ++ show targetRef
                        , "  first routes: " ++ show existing
                        , "  second routes: " ++ show routes
                        ]
                    )

    copiedRoutesAgree left right =
      length left == length right
        && all
          ( \(sourceIdentity, copiedRef) ->
              case lookup sourceIdentity right of
                Just otherCopiedRef ->
                  typeBinderRefsSameIdentity copiedRef otherCopiedRef
                Nothing -> False
          )
          left

    placePacket scheme0 packet =
      case subtermGeneralizationConsumerIdentity packet of
        Nothing -> Right (scheme0, Nothing, Nothing)
        Just consumerIdentity ->
          case targetRefs consumerIdentity of
            [] ->
              case nestedTargets consumerIdentity of
                [nestedTarget] ->
                  consumeNestedConsumerBound nestedTarget
                [] -> do
                  exactDischarged <- consumeExactConsumerSpecialization packet
                  topologyDischarged <-
                    if exactDischarged
                      then pure False
                      else consumeClosedConsumerDischarge packet
                  lexicalTopologyDischarged <-
                    if exactDischarged || topologyDischarged
                      then pure False
                      else consumeLexicallyClosedConsumerDischarge packet
                  topologyForwarded <-
                    if
                      exactDischarged
                        || topologyDischarged
                        || lexicalTopologyDischarged
                      then pure False
                      else consumeForwardedTopologyResult packet
                  if
                    exactDischarged
                      || topologyDischarged
                      || lexicalTopologyDischarged
                      || topologyForwarded
                    then Right (scheme0, Nothing, Nothing)
                    else
                      Left
                        ( InstantiationError
                            ( unlines
                                [ "prepared subterm scheme has no enclosing binder consumer"
                                , "  consumer identity: " ++ show consumerIdentity
                                , "  enclosing scheme: " ++ show scheme
                                , "  enclosing binders: " ++ show binders
                                , "  nested enclosing binders: "
                                    ++ show (nestedForallDeclarationsInScheme scheme)
                                , "  packet scheme: " ++ show (siScheme (psgSchemeInfo packet))
                                , "  packet scheme routes: "
                                    ++ show (siSubstRefs (psgSchemeInfo packet))
                                , "  packet source binder order routes: "
                                    ++ show
                                      ( siSourceBinderOrderRefs
                                          (psgSchemeInfo packet)
                                      )
                                , "  packet construction binder order routes: "
                                    ++ show
                                      ( siConstructionBinderOrderRefs
                                          (psgSchemeInfo packet)
                                      )
                                , "  packet copied-binder routes: "
                                    ++ show (psgCopiedBinderRefs packet)
                                , "  packet placed copied-binder routes: "
                                    ++ show (psgPlacedCopiedBinderRefs packet)
                                , "  packet inherited Gamma routes: "
                                    ++ show (psgInheritedGammaRoutes packet)
                                , "  packet Gamma bound: "
                                    ++ show (psgGammaBoundScheme packet)
                                , "  packet operated scheme: "
                                    ++ show (siScheme (psgOperatedSchemeInfo packet))
                                , "  packet exact consumer specialization: "
                                    ++ show (psgExactConsumerSpecialization packet)
                                , "  packet source lambda parameter: "
                                    ++ show (psgSourceLambdaParameter packet)
                                , "  packet Gamma authority: " ++ show (psgGammaAuthority packet)
                                , "  packet consumer authority: " ++ show (psgConsumerAuthority packet)
                                , "  packet local-result authority: "
                                    ++ show (psgLocalResultAuthority packet)
                                , "  packet local-result discharge: "
                                    ++ show (psgLocalResultDischarge packet)
                                , "  packet closed-topology discharge: "
                                    ++ show
                                      (psgClosedConsumerDischarge packet)
                                , "  recomputed closed-topology discharge: "
                                    ++ show
                                      ( closedConsumerDischarge
                                          (psgConsumerAuthority packet)
                                          (psgGammaAuthority packet)
                                          (psgSchemeInfo packet)
                                          (psgGammaBoundScheme packet)
                                      )
                                , "  completed packet free refs: "
                                    ++ show
                                      ( freeTypeVarRefsType
                                          ( schemeToType
                                              (siScheme (psgSchemeInfo packet))
                                          )
                                      )
                                , "  completed packet bound refs: "
                                    ++ show
                                      ( boundRefsInType
                                          ( schemeToType
                                              (siScheme (psgSchemeInfo packet))
                                          )
                                      )
                                , "  completed/Gamma alpha equality: "
                                    ++ show
                                      ( alphaEqType
                                          ( schemeToType
                                              (siScheme (psgSchemeInfo packet))
                                          )
                                          ( schemeToType
                                              (psgGammaBoundScheme packet)
                                          )
                                      )
                                , "  enclosing consumer routes: " ++ show consumerRoutes
                                , "  packet compiler-exact routes: "
                                    ++ show (subtermGeneralizationCompilerExactBinderRenames packet)
                                , "  packet construction routes: "
                                    ++ show (subtermGeneralizationConstructionBinderRenames packet)
                                ]
                            )
                        )
                nested ->
                  multipleConsumerFailure [] nested
            [targetRef]
              | let competingNestedTargets =
                      nestedTargetsOutsideTopLevelConsumer
                        targetRef
                        consumerIdentity
              , not (null competingNestedTargets) ->
                  multipleConsumerFailure
                    [targetRef]
                    competingNestedTargets
              | forwardsThroughExistingTopologyResult targetRef packet ->
                  Right (scheme0, Nothing, Nothing)
              | otherwise -> do
              validatePacketFreeRefSourceRoutes targetRef
              (provisionalTargetBound, mbUnboundedCompletedType) <-
                case find (typeBinderRefsSameIdentity targetRef . fst) binders of
                  Just (_, Just existingBound) ->
                    Right (existingBound, Nothing)
                  Just (_, Nothing)
                    | Just authority <- psgConsumerAuthority packet
                    , subtermConsumerAuthorityIsTopology authority ->
                        case elabToBound packetTyUnfreshened of
                          Right preparedBound ->
                            Right (preparedBound, Nothing)
                          Left cause ->
                            Left
                              ( ValidationFailed
                                  [ "identity-topology S'(operated) is not a legal prepared construction bound"
                                  , "  consumer: " ++ show targetRef
                                  , "  prepared bound: " ++ show packetTyUnfreshened
                                  , "  cause: " ++ cause
                                  ]
                              )
                  Just (_, Nothing)
                    | Just (_, _, frozenOperatedType, _) <-
                        subtermGeneralizationSourceOwnerConsumerCompletion packet
                    , Just completedType0 <-
                        completedBoundFor
                          packet
                          targetRef
                          packetTyUnfreshened
                          frozenOperatedType ->
                        let completedType =
                              alignPacketFreeRefs
                                ( closeCompletedPacketEndpoint
                                    packetTyUnfreshened
                                    completedType0
                                )
                         in case elabToBound completedType of
                              Right completedBound ->
                                Right
                                  ( completedBound
                                  , Just completedType
                                  )
                              Left cause ->
                                Left
                                  ( ValidationFailed
                                      [ "certified unbounded consumer endpoint is not a legal Gamma bound"
                                      , "  consumer: " ++ show targetRef
                                      , "  completed endpoint: "
                                          ++ show completedType
                                      , "  cause: " ++ cause
                                      ]
                                  )
                  _ ->
                    Left
                      ( ValidationFailed
                          [ "prepared subterm scheme consumer has no checked construction bound"
                          , "  consumer: " ++ show targetRef
                          , "  packet consumer authority: " ++ show (psgConsumerAuthority packet)
                          , "  packet Gamma authority: " ++ show (psgGammaAuthority packet)
                          , "  packet construction scheme: " ++ show (siScheme (psgConsumerConstructionSchemeInfo packet))
                          , "  enclosing scheme: " ++ show scheme0
                          ]
                      )
              let mbCertifiedCompletedType =
                    mbUnboundedCompletedType
                      <|> completedBoundFor
                        packet
                        targetRef
                        packetTyUnfreshened
                        (tyToElab provisionalTargetBound)
              mbCertifiedCompletedBound <-
                traverse
                  ( \completedType0 -> do
                      let completedType =
                            alignPacketFreeRefs
                              ( closeCompletedPacketEndpoint
                                  packetTyUnfreshened
                                  completedType0
                              )
                      case elabToBound completedType of
                        Right completedBound -> pure completedBound
                        Left cause ->
                          Left
                            ( ValidationFailed
                                [ "certified completed packet endpoint is not a legal Gamma bound"
                                , "  consumer: " ++ show targetRef
                                , "  completed endpoint: " ++ show completedType
                                , "  cause: " ++ cause
                                ]
                            )
                  )
                  mbCertifiedCompletedType
              let completedBoundAlreadyConstructed =
                    isJust mbCertifiedCompletedBound
              placementScheme <-
                case mbCertifiedCompletedBound of
                  Nothing -> pure scheme
                  Just completedBound ->
                    either
                      (Left . ValidationFailed . pure)
                      Right
                      ( orderSourceProjectedSchemeBinders
                          "certified completed packet construction"
                          ( mkElabSchemeWithRefs
                              [ if typeBinderRefsSameIdentity ref targetRef
                                  then (ref, Just completedBound)
                                  else binder
                              | binder@(ref, _) <- binders
                              ]
                              (schemeBody scheme)
                          )
                      )
              let placementBinders = schemeBinderRefs placementScheme
              case mbCertifiedCompletedBound of
                Nothing -> pure ()
                Just completedBound ->
                  validatePacketFreeRefsWithin
                    targetRef
                    (bindersBeforeIn placementBinders targetRef)
                    (freeTypeVarRefsType (tyToElab completedBound))
              let
                  targetBound =
                    fromMaybe
                      provisionalTargetBound
                      mbCertifiedCompletedBound
              unless completedBoundAlreadyConstructed $
                validatePacketFreeRefs targetRef
              let constructionTargetBoundInPacketDomain =
                    schemeToType
                      ( schemeInPacketConstructionDomain
                          packet
                          (schemeFromType (tyToElab targetBound))
                      )
                  constructionTargetBound =
                    alignPacketFreeRefs constructionTargetBoundInPacketDomain
                  packetOwnedTargetBound =
                    releasePacketFreeBinders constructionTargetBound
              let boundPlacement
                    | completedBoundAlreadyConstructed =
                        Just PacketBodyBound
                    | otherwise =
                        packetBoundMatch
                          targetRef
                          packetOwnedTargetBound
                          (tyToElab targetBound)
              (placedTargetBound, copiedBinderRoutes) <-
                case boundPlacement of
                  Just PacketBodyBound ->
                    -- The enclosing consumer already owns the exact
                    -- S'(operated) bound.  A body-only match proves that the
                    -- packet's outer binders may be removed, but it does not
                    -- authorize wrapping that exact bound in the packet's
                    -- copied forall spine.
                    pure (targetBound, [])
                  Just WholePacketBound -> do
                    packetTy <-
                      freshenPacketBoundRefs
                        packetReservedNames
                        packetTypeAtEnclosingSource
                    case elabToBound packetTy of
                      Right packetBound ->
                        pure
                          ( packetBound
                          , subtermGeneralizationCopiedBinderRoutes packet
                          )
                      Left err ->
                        Left
                          ( ValidationFailed
                              [ "prepared subterm scheme is not a legal Γ bound"
                              , "  consumer: " ++ show targetRef
                              , "  packet type: " ++ show packetTy
                              , "  error: " ++ err
                              ]
                          )
                  Just PacketConstructionBound -> do
                    packetTy <-
                      freshenPacketBoundRefs
                        packetReservedNames
                        ( packetConstructionTyUnfreshened
                            targetRef
                            targetBound
                        )
                    case elabToBound packetTy of
                      Right packetBound ->
                        pure
                          ( packetBound
                          , subtermGeneralizationCopiedBinderRoutes packet
                          )
                      Left err ->
                        Left
                          ( ValidationFailed
                              [ "prepared topology construction is not a legal Γ bound"
                              , "  consumer: " ++ show targetRef
                              , "  constructed packet type: " ++ show packetTy
                              , "  error: " ++ err
                              ]
                          )
                  Nothing ->
                    Left
                      ( ValidationFailed
                          [ "prepared subterm scheme disagrees with the constructed RaiseMerge bound"
                          , "  consumer: " ++ show targetRef
                          , "  published constructed bound: " ++ show targetBound
                          , "  raw construction-domain bound: "
                              ++ show constructionTargetBoundInPacketDomain
                          , "  construction-domain bound: " ++ show constructionTargetBound
                          , "  packet-owned projection: " ++ show packetOwnedTargetBound
                          , "  packet type: " ++ show packetTyUnfreshened
                          ]
                      )
              let retained =
                    [ ( ref
                      , if typeBinderRefsSameIdentity ref targetRef
                          then Just placedTargetBound
                          else mbBound
                      )
                    | (ref, mbBound) <- placementBinders
                    , typeBinderRefsSameIdentity ref targetRef
                        || isPacketSourceOwned ref
                        || not (isPacketOwned ref)
                    ]
                  placed =
                    mkElabSchemeWithRefs retained (schemeBody placementScheme)
                  placementProvenance
                    | null copiedBinderRoutes = Nothing
                    | otherwise = Just (targetRef, copiedBinderRoutes)
                  constructedConsumer =
                    case boundPlacement of
                      Just PacketConstructionBound ->
                        Just (typeBinderRefIdentity targetRef)
                      _ -> Nothing
              pure
                ( placed
                , placementProvenance
                , constructedConsumer
                )
            targets ->
              multipleConsumerFailure
                targets
                (nestedTargets consumerIdentity)
      where
        multipleConsumerFailure
          :: [TypeBinderRef]
          -> [NestedForallDeclaration]
          -> Either ElabError a
        multipleConsumerFailure topLevelTargets nested =
          Left
            ( InstantiationError
                ( unlines
                    [ "prepared subterm scheme is consumed by multiple enclosing binders"
                    , "  top-level consumers: " ++ show topLevelTargets
                    , "  nested consumers: " ++ show nested
                    ]
                )
            )

        consumeNestedConsumerBound declaration = do
            let targetRef = nfdRef declaration
                mbTargetBound = nfdBound declaration
                lexicalRefs = nfdLexicalRefs declaration
            case psgConsumerAuthority packet of
              Just authority
                | subtermConsumerAuthorityIsTopology authority ->
                    pure ()
              authority ->
                Left
                  ( ValidationFailed
                      [ "only an identity-topology packet may be consumed by a nested bound declaration"
                      , "  consumer: " ++ show targetRef
                      , "  packet consumer authority: " ++ show authority
                      ]
                  )
            validatePacketFreeRefSourceRoutes targetRef
            let nestedPacketTy =
                  alignPacketFreeRefsWithin
                    lexicalRefs
                    (schemeToType packetBoundScheme)
                nestedPacketBodyTy =
                  alignPacketFreeRefsWithin
                    lexicalRefs
                    (schemeBody packetBoundScheme)
                packetBindersRetainedByLexicalGamma =
                  case schemeBinderRefs packetBoundScheme of
                    [] -> False
                    packetBinders ->
                      all (isNothing . snd) packetBinders
                        && orderedIdentitySubsequence
                          (map fst packetBinders)
                          lexicalRefs
                nestedPacketFreeRefs =
                  freeTypeVarRefsType nestedPacketTy
            validatePacketFreeRefsWithin
              targetRef
              lexicalRefs
              nestedPacketFreeRefs
            (targetBound, placedScheme, constructedConsumer) <-
              case mbTargetBound of
                Just bound ->
                  pure (bound, scheme, Nothing)
                Nothing -> do
                  copiedPacketTy <-
                    freshenPacketBoundRefs
                      ( Set.unions
                          ( packetReservedNames
                              : map typeBinderRefAliasNames lexicalRefs
                          )
                      )
                      nestedPacketTy
                  copiedPacketBound <-
                    case elabToBound copiedPacketTy of
                      Right bound -> pure bound
                      Left cause ->
                        Left
                          ( ValidationFailed
                              [ "nested identity-topology S'(operated) is not a legal construction bound"
                              , "  consumer: " ++ show targetRef
                              , "  prepared bound: " ++ show copiedPacketTy
                              , "  cause: " ++ cause
                              ]
                          )
                  schemeWithNestedBound <-
                    installNestedForallBound
                      targetRef
                      copiedPacketBound
                      scheme
                  pure
                    ( copiedPacketBound
                    , schemeWithNestedBound
                    , Just (typeBinderRefIdentity targetRef)
                    )
            let constructionTargetBoundInPacketDomain =
                  schemeToType
                    ( schemeInPacketConstructionDomain
                        packet
                        (schemeFromType (tyToElab targetBound))
                    )
                constructionTargetBound =
                  alignPacketFreeRefsWithin
                    lexicalRefs
                    constructionTargetBoundInPacketDomain
                matchesNestedConstruction =
                  alphaEqType constructionTargetBound nestedPacketTy
                    -- The paper's @g g@ packet can already have moved its
                    -- complete unbounded forall spine into the surrounding
                    -- lexical Gamma.  In that case the nested consumer binds
                    -- the packet body under those exact declarations; it is
                    -- not an escaping self-instantiation @a := a@.  Accept
                    -- only the identity-ordered lexical spine prepared by
                    -- the enclosing scheme.
                    || ( packetBindersRetainedByLexicalGamma
                          && alphaEqType
                            constructionTargetBound
                            nestedPacketBodyTy
                       )
                    || packetTypeSpecializesToExactEndpoint
                      nestedPacketTy
                      constructionTargetBound
                    || maybe
                      False
                      ( \completedType ->
                          operationallyMatchesCompletedType
                            completedType
                            constructionTargetBound
                      )
                      ( completedBoundFor
                          packet
                          targetRef
                          nestedPacketTy
                          constructionTargetBound
                      )
            unless matchesNestedConstruction $
              Left
                ( ValidationFailed
                    [ "prepared subterm scheme disagrees with its exact nested topology bound"
                    , "  consumer: " ++ show targetRef
                    , "  lexical binders: " ++ show lexicalRefs
                    , "  published nested bound: " ++ show targetBound
                    , "  construction-domain bound: "
                        ++ show constructionTargetBound
                    , "  packet type: " ++ show nestedPacketTy
                    ]
                )
            pure
              ( placedScheme
              , Nothing
              , constructedConsumer
              )
          where
            -- A topology consumer may be declared below another flexible
            -- bound.  Install the prepared packet at that exact declaration
            -- instead of flattening it into the outer scheme spine.  Exact
            -- identities make the rewrite unambiguous; encountering zero or
            -- multiple declarations is a construction-provenance failure.
            installNestedForallBound
              :: TypeBinderRef
              -> BoundType
              -> ElabScheme
              -> Either ElabError ElabScheme
            installNestedForallBound consumerRef preparedBound candidateScheme =
              case replacedCount of
                1 ->
                  Right
                    ( mkElabSchemeWithRefs
                        replacedBinders
                        replacedBody
                    )
                _ ->
                  Left
                    ( ValidationFailed
                        [ "nested topology construction did not select exactly one forall declaration"
                        , "  consumer: " ++ show consumerRef
                        , "  matching declarations: " ++ show replacedCount
                        , "  enclosing scheme: " ++ show candidateScheme
                        ]
                    )
              where
                (replacedBinders, binderCounts) =
                  unzip
                    [ case mbBound of
                        Nothing -> ((ref, Nothing), 0)
                        Just bound ->
                          let (bound', count) = replaceInType bound
                           in ((ref, Just bound'), count)
                    | (ref, mbBound) <- schemeBinderRefs candidateScheme
                    ]
                (replacedBody, bodyCount) =
                  replaceInType (schemeBody candidateScheme)
                replacedCount = sum binderCounts + bodyCount

                replaceInType :: Ty v -> (Ty v, Int)
                replaceInType ty =
                  case ty of
                    TVarRef ref -> (TVarRef ref, 0)
                    TArrow domain codomain ->
                      let (domain', domainCount) =
                            replaceInType domain
                          (codomain', codomainCount) =
                            replaceInType codomain
                       in ( TArrow domain' codomain'
                          , domainCount + codomainCount
                          )
                    TConWithIdentity identity constructor arguments ->
                      let (arguments', counts) =
                            unzip (map replaceInType (NonEmpty.toList arguments))
                       in ( TConWithIdentity
                              identity
                              constructor
                              (NonEmpty.fromList arguments')
                          , sum counts
                          )
                    TVarAppRef ref arguments ->
                      let (arguments', counts) =
                            unzip (map replaceInType (NonEmpty.toList arguments))
                       in ( TVarAppRef
                              ref
                              (NonEmpty.fromList arguments')
                          , sum counts
                          )
                    TBaseWithIdentity identity base ->
                      (TBaseWithIdentity identity base, 0)
                    TBottom -> (TBottom, 0)
                    TForallRef ref mbBound body ->
                      let (mbBound', boundCount) =
                            case mbBound of
                              Nothing -> (Nothing, 0)
                              Just bound ->
                                let (bound', count) = replaceInType bound
                                 in (Just bound', count)
                          (body', bodyCount') = replaceInType body
                          isTarget =
                            typeBinderRefsSameIdentity ref consumerRef
                          installedBound
                            | isTarget = Just preparedBound
                            | otherwise = mbBound'
                       in ( TForallRef ref installedBound body'
                          , boundCount
                              + bodyCount'
                              + if isTarget then 1 else 0
                          )
                    TMuRef ref body ->
                      let (body', count) = replaceInType body
                       in (TMuRef ref body', count)

            orderedIdentitySubsequence [] _ = True
            orderedIdentitySubsequence _ [] = False
            orderedIdentitySubsequence
              sought@(packetRef : remainingPacketRefs)
              (lexicalRef : remainingLexicalRefs)
                | typeBinderRefsSameIdentity packetRef lexicalRef =
                    orderedIdentitySubsequence
                      remainingPacketRefs
                      remainingLexicalRefs
                | otherwise =
                    orderedIdentitySubsequence
                      sought
                      remainingLexicalRefs

        consumeExactConsumerSpecialization candidate =
          case psgExactConsumerSpecialization candidate of
            Nothing -> Right False
            Just certificate
              | exactConsumerSpecializationFor
                  (ecsExpectedEndpoint certificate)
                  candidate
                  == Just certificate ->
                  Right True
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "exact packet-consumer specialization no longer matches its prepared packet"
                        , "  certificate: " ++ show certificate
                        , "  packet construction: "
                            ++ show (siScheme (psgSchemeInfo candidate))
                        , "  packet operated endpoint: "
                            ++ show (siScheme (psgOperatedSchemeInfo candidate))
                        , "  packet Gamma bound: "
                            ++ show (psgGammaBoundScheme candidate)
                        , "  packet consumer authority: "
                            ++ show (psgConsumerAuthority candidate)
                        ]
                    )

        consumeClosedConsumerDischarge candidate =
          case psgClosedConsumerDischarge candidate of
            Nothing -> Right False
            Just certificate
              | closedConsumerDischarge
                  (psgConsumerAuthority candidate)
                  (psgGammaAuthority candidate)
                  (psgSchemeInfo candidate)
                  (psgGammaBoundScheme candidate)
                  == Just certificate ->
                  Right True
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "closed topology-consumer discharge no longer matches its prepared packet"
                        , "  certificate: " ++ show certificate
                        , "  packet scheme: " ++ show (psgSchemeInfo candidate)
                        , "  packet Gamma bound: " ++ show (psgGammaBoundScheme candidate)
                        , "  packet consumer authority: "
                            ++ show (psgConsumerAuthority candidate)
                        , "  packet Gamma authority: "
                            ++ show (psgGammaAuthority candidate)
                        ]
                    )

        -- A topology consumer can become vacuous only after an enclosing
        -- construction has moved the packet's surviving source declarations
        -- into its lexical Gamma.  Rebuild that exact computation context
        -- before asking the ordinary closed-consumer smart constructor to
        -- discharge the packet.  Every required declaration must be present
        -- in the enclosing scheme and in its published identity routes; a
        -- merely alpha-similar ambient type cannot create this proof.
        consumeLexicallyClosedConsumerDischarge candidate =
          Right
            ( not (null contextBinders)
                && contextOwnsEveryFreeRef
                && all contextBinderHasExactRoute contextBinders
                && isJust
                  ( closedConsumerDischarge
                      (psgConsumerAuthority candidate)
                      (psgGammaAuthority candidate)
                      contextualSchemeInfo
                      contextualConstructionScheme
                  )
            )
          where
            completedType =
              alignPacketFreeRefs
                (schemeToType (siScheme (psgSchemeInfo candidate)))
            constructionType = packetTyUnfreshened
            requiredContextRefs =
              foldr insertDistinctRef []
                ( freeTypeVarRefsType completedType
                    ++ freeTypeVarRefsType constructionType
                )
            contextBinders =
              binderDependencyClosure binders requiredContextRefs
            contextOwnsEveryFreeRef =
              all
                ( \requiredRef ->
                    any
                      (typeBinderRefsSameIdentity requiredRef . fst)
                      contextBinders
                )
                requiredContextRefs
            contextBinderHasExactRoute (contextRef, _) =
              any
                (typeBinderRefsSameIdentity contextRef)
                (IntMap.elems consumerRoutes)
            contextualSchemeInfo =
              (psgSchemeInfo candidate)
                { siScheme =
                    mkElabSchemeWithRefs
                      contextBinders
                      completedType
                }
            contextualConstructionScheme =
              mkElabSchemeWithRefs
                contextBinders
                constructionType

            insertDistinctRef ref refs
              | any (typeBinderRefsSameIdentity ref) refs = refs
              | otherwise = ref : refs

        -- A rigid topology result can remain free in the immediately
        -- enclosing scheme and be bound by a later Gamma.  This is not the
        -- closed administrative case above: forwarding is legal only when
        -- the exact consumer identity occurs in the enclosing result and the
        -- complete packet is precisely that result reference.  The topology
        -- authority and the identity-bearing scheme occurrence jointly carry
        -- the construction proof; no enclosing binder is manufactured here.
        consumeForwardedTopologyResult candidate =
          case
              ( psgConsumerAuthority candidate
              , psgGammaAuthority candidate
              , forwardedResultRefs
              )
            of
              (Just authority, Nothing, [resultRef])
                | subtermConsumerAuthorityIsTopology authority
                , alphaEqType
                    packetTyUnfreshened
                    (TVarRef resultRef) ->
                    Right True
              _ -> Right False
          where
            forwardedResultRefs =
              foldr insertDistinctRef []
                [ resultRef
                | resultRef <- freeTypeVarRefsType (schemeBody scheme)
                , forwardedConsumerTargetsRef
                    (scaConsumerIdentity <$> psgConsumerAuthority candidate)
                    resultRef
                ]

            forwardedConsumerTargetsRef Nothing _ = False
            forwardedConsumerTargetsRef (Just consumerIdentity) ref =
              typeBinderRefIdentity ref == consumerIdentity
                || any
                  (typeBinderRefsSameIdentity ref)
                  [ routedRef
                  | consumerNode <-
                      maybeToList
                        (typeBinderIdentityNode consumerIdentity)
                  , routedRef <-
                      maybeToList
                        ( IntMap.lookup
                            (getNodeId consumerNode)
                            consumerRoutes
                        )
                  ]

            insertDistinctRef ref refs
              | any (typeBinderRefsSameIdentity ref) refs = refs
              | otherwise = ref : refs

        -- The enclosing scheme can already expose the exact topology
        -- consumer as an unbounded result binder.  In that identity case the
        -- descendant packet is the body at that binder; manufacturing
        -- @consumer >= consumer@ would be both ill-scoped xMLF and a second
        -- construction of the same result.  Require the prepared topology
        -- authority, the unbounded declaration, and the exact result
        -- occurrence together before retaining the existing binder.
        forwardsThroughExistingTopologyResult targetRef candidate =
          case
              ( psgConsumerAuthority candidate
              , psgGammaAuthority candidate
              , find (typeBinderRefsSameIdentity targetRef . fst) binders
              )
            of
              (Just authority, Nothing, Just (_, Nothing)) ->
                subtermConsumerAuthorityIsTopology authority
                  && consumerTargetsRef
                    (scaConsumerIdentity authority)
                    targetRef
                  && any
                    (typeBinderRefsSameIdentity targetRef)
                    (freeTypeVarRefsType (schemeBody scheme))
                  && alphaEqType
                    packetTyUnfreshened
                    (TVarRef targetRef)
              _ -> False

        packetBoundMatch targetRef constructionTargetBound publishedTargetBound
          | alphaEqType constructionTargetBound packetBodyUnfreshened
          , targetBoundReusesSourceBinder publishedTargetBound =
              Just PacketBodyBound
          -- A body-shaped bound that still depends on a packet-local graph
          -- binder is not closed after that binder is removed.  Preserve the
          -- complete copied packet before considering broader compiler-exact
          -- authority; the latter may describe a different source binder and
          -- cannot make this graph-local dependency ambient.
          | alphaEqType constructionTargetBound packetBodyUnfreshened
          , targetBoundDependsOnRemovedPacketBinder targetRef publishedTargetBound =
              Just WholePacketBound
          | alphaEqType constructionTargetBound packetBodyUnfreshened =
              Just PacketBodyBound
          -- An enclosing generalization may construct the exact bound using
          -- copies of declarations owned by this topology packet.  The
          -- published base-to-construction routes identify those declarations
          -- positively.  Move that exact binder spine into the target bound;
          -- this is the construction planned by the graph, not a repair
          -- inferred from the resulting type.
          | packetHasTopologyConsumer
          , not (null (removedPacketBinders targetRef))
          , targetBoundDependsOnRemovedPacketBinder
              targetRef
              publishedTargetBound
          , packetTypeSpecializesToExactEndpoint
              packetTyUnfreshened
              constructionTargetBound =
              Just PacketConstructionBound
          -- A completed descendant packet may already have been instantiated
          -- in the ambient construction Gamma.  First replay the paper's
          -- exact @N@ construction: eliminating a bounded leading binder
          -- substitutes its declared bound, even when that binder is
          -- vacuous or its graph presentation no longer occurs in the
          -- specialized target.  The consumer identity/owner selected above
          -- is the placement authority; equality after this checked replay
          -- proves that the existing target is its already-specialized
          -- endpoint and must not be wrapped in the packet forall again.
          | packetTypeSpecializesToExactEndpoint
              packetTyUnfreshened
              constructionTargetBound =
              Just PacketBodyBound
          -- A source annotation can already have constructed the selected
          -- consumer inside its own published lower bound.  The leading
          -- source declarations stay in the enclosing Gamma, while the
          -- descendant packet supplies the exact lower bound of the nested
          -- occurrence of that same consumer.  Validate that ownership by
          -- identity before retaining the published bound; comparing the
          -- packet tail with the whole source-prefixed bound would require
          -- the child to construct declarations it does not own.
          | publishedBoundContainsPacketConsumer
              targetRef
              publishedTargetBound =
              Just PacketBodyBound
          -- Source-order declarations are already lexical in the enclosing
          -- scheme.  Remove only that certified prefix before replaying the
          -- packet consumer's N step; copying the source declaration into the
          -- consumer bound would both duplicate its scope and hide the exact
          -- source identity needed by the constructed bound.
          | not (null packetSourceBinderRefs)
          , packetTypeSpecializesToExactEndpoint
              packetTypeAtEnclosingSource
              constructionTargetBound =
              Just PacketBodyBound
          -- The enclosing Gamma can be strictly more general than this
          -- descendant occurrence.  The selected consumer identity fixes the
          -- declaration, and an exact reverse binder-spine plan proves the
          -- body endpoint obtained by specializing that declaration.
          | packetTypeSpecializesToExactEndpoint
              constructionTargetBound
              packetTyUnfreshened =
              Just PacketBodyBound
          -- The enclosing construction can already have published the
          -- packet's complete bound at this exact consumer.  Re-copying an
          -- alpha-equivalent packet would allocate a second binder spine for
          -- a declaration that is already constructed, and two descendants
          -- sharing the consumer would then report conflicting copy
          -- provenance.  Preserve the published declaration; an unbounded
          -- topology target still takes the WholePacket path below.
          | targetBoundWasPublished targetRef publishedTargetBound
          , alphaEqType constructionTargetBound packetTyUnfreshened =
              Just PacketBodyBound
          | alphaEqType constructionTargetBound packetTyUnfreshened =
              Just WholePacketBound
          | otherwise = Nothing

        publishedBoundContainsPacketConsumer targetRef publishedTargetBound =
          case consumerDeclarations [] publishedBinders of
            [(sourcePrefix, Just packetConsumerBound)] ->
              sourcePrefixOwnedByEnclosingGamma
                sourcePrefix
                enclosingPrefix
                && alphaEqType
                  (tyToElab packetConsumerBound)
                  packetTyUnfreshened
            _ -> False
          where
            publishedBinders =
              schemeBinderRefs (schemeFromType publishedTargetBound)
            enclosingPrefix =
              takeWhile
                (not . typeBinderRefsSameIdentity targetRef . fst)
                binders

            consumerDeclarations _ [] = []
            consumerDeclarations prefix (binder@(ref, mbBound) : rest)
              | typeBinderRefsSameIdentity targetRef ref =
                  (prefix, mbBound)
                    : consumerDeclarations (prefix ++ [binder]) rest
              | otherwise =
                  consumerDeclarations (prefix ++ [binder]) rest

            sourcePrefixOwnedByEnclosingGamma [] _ = True
            sourcePrefixOwnedByEnclosingGamma _ [] = False
            sourcePrefixOwnedByEnclosingGamma
              sourcePrefix@((sourceRef, sourceBound) : remainingSource)
              ((enclosingRef, enclosingBound) : remainingEnclosing)
                | typeBinderRefsSameIdentity sourceRef enclosingRef =
                    isGeneratedSourceRef sourceRef
                      && boundsAlphaEquivalent sourceBound enclosingBound
                      && sourcePrefixOwnedByEnclosingGamma
                        remainingSource
                        remainingEnclosing
                | otherwise =
                    sourcePrefixOwnedByEnclosingGamma
                      sourcePrefix
                      remainingEnclosing

            boundsAlphaEquivalent Nothing Nothing = True
            boundsAlphaEquivalent (Just left) (Just right) =
              alphaEqType (tyToElab left) (tyToElab right)
            boundsAlphaEquivalent _ _ = False

        targetBoundWasPublished targetRef publishedTargetBound =
          case find (typeBinderRefsSameIdentity targetRef . fst) binders of
            Just (_, Just _) ->
              -- A complete bound is already constructed only when its
              -- declaration spine is lexically distinct from the packet
              -- spine.  Reusing a packet binder identity inside the enclosing
              -- bound is precisely the pre-copy representation; accepting it
              -- here would declare one identity twice and skip allocation of
              -- the paper's copied binder.
              all
                ( \publishedRef ->
                    not
                      ( any
                          (typeBinderRefsSameIdentity publishedRef)
                          packetOwnedRefs
                      )
                )
                (typeBinderDeclarationRefs (tyToElab publishedTargetBound))
            _ -> False

        targetBoundReusesSourceBinder targetBound =
          any sourceBinderOccursFree packetOwnedRefs
          where
            targetFreeRefs = freeTypeVarRefsType targetBound
            sourceBinderOccursFree packetRef =
              case typeBinderIdentityGeneratedUnique (typeBinderRefIdentity packetRef) of
                Nothing -> False
                Just _ ->
                  any (typeBinderRefsSameIdentity packetRef) targetFreeRefs

        targetBoundDependsOnRemovedPacketBinder targetRef targetBound =
          any
            (\freeRef -> any (typeBinderRefsSameIdentity freeRef) removedPacketConstructionRefs)
            (freeTypeVarRefsType targetBound)
          where
            removedPacketRefs =
              map fst (removedPacketBinders targetRef)
            removedPacketConstructionRefs =
              removedPacketRefs
                ++ [ constructionRef
                   | removedRef <- removedPacketRefs
                   , (sourceRef, constructionRef) <-
                       subtermGeneralizationConstructionBinderRenames packet
                   , typeBinderRefsSameIdentity removedRef sourceRef
                   ]

        packetConstructionTyUnfreshened targetRef targetBound =
          foldr
            ( \(ref, mbBound) body ->
                TForallRef ref mbBound body
            )
            (tyToElab targetBound)
            (removedPacketBinders targetRef)

        removedPacketBinders targetRef =
          [ binder
          | binder@(ref, _) <- binders
          , not (typeBinderRefsSameIdentity ref targetRef)
          , isPacketOwned ref
          , not (isPacketSourceOwned ref)
          ]

        -- The descendant packet is constructed before the enclosing
        -- RaiseMerge consumer.  Allocate its display names in that semantic
        -- order as well: first the lexical binders visible to the packet,
        -- then the packet-local binders, and only then the remaining
        -- enclosing binders.  This is the paper's @a, b, c@ order for the K
        -- example.  Keeping the allocation here also lets the copied packet
        -- bound choose @d@ without sharing the local packet's @b@.
        packetLocalSchemeInfo =
          freshenSchemeInfoBinderNamesAgainst
            packetLexicalReservedNames
            (psgSchemeInfo packet)
        packetLocalBinderNames =
          Set.unions
            ( map
                (typeBinderRefAliasNames . fst)
                (schemeBinderRefs (siScheme packetLocalSchemeInfo))
            )
        scheme =
          siScheme
            ( freshenSchemeInfoBinderNamesAgainst
                packetLocalBinderNames
                (schemeInfoFromRefSubst scheme0 IntMap.empty)
            )

        packetLexicalReservedNames =
          Set.unions
            (map typeBinderRefAliasNames packetLexicalRefs)
        packetLexicalRefs =
          [ case find (typeBinderRefsSameIdentity packetRef) bindersBeforeConsumer of
              Just enclosingRef -> enclosingRef
              Nothing -> packetRef
          | packetRef <- freeTypeVarRefsType (schemeToType packetBoundScheme)
          , any (typeBinderRefsSameIdentity packetRef) bindersBeforeConsumer
              || not
                ( any
                    (typeBinderRefsSameIdentity packetRef . fst)
                    originalBinders
                )
          ]
        bindersBeforeConsumer =
          case subtermGeneralizationConsumerIdentity packet of
            Just consumerIdentity ->
              case targetRefsIn originalBinders consumerIdentity of
                  [consumerRef] ->
                    map fst
                      ( takeWhile
                          (not . typeBinderRefsSameIdentity consumerRef . fst)
                          originalBinders
                      )
                  _ -> []
            Nothing -> []
        originalBinders = schemeBinderRefs scheme0

        -- Packet preparation and enclosing-root generalization may choose
        -- different display payloads for the same graph identity. They may
        -- also publish an explicit graph-to-source route before placement.
        -- Project through that route here, while it is still producer-owned,
        -- so validation and packet/target matching consume one identity
        -- domain. Structural identities are never source aliases.
        --
        -- Once the packet becomes a bound in this scheme, its free references
        -- must use the enclosing binder's complete reference; otherwise
        -- name-based xMLF emission can print a free variable even though the
        -- identities agree (for example, the result of the inner K lambda).
        -- Packet-owned binders are then freshened away from the enclosing
        -- scheme's names so those aligned free occurrences cannot be captured
        -- when emitted.
        packetReservedNames =
          Set.unions
            ( map (typeBinderRefAliasNames . fst) binders
                ++ map typeBinderRefAliasNames (freeTypeVarRefsType packetTyUnfreshened)
                ++ [packetLocalBinderNames]
            )
        packetTyUnfreshened =
          alignPacketFreeRefs (schemeToType packetBoundScheme)
        packetTypeAtEnclosingSource =
          releasePacketSourceBinders packetTyUnfreshened
        packetBodyUnfreshened =
          alignPacketFreeRefs (schemeBody packetBoundScheme)
        -- The stored Gamma bound is the outward exact/source view.  Root
        -- placement itself still runs in the packet's construction domain,
        -- so consume the already-proved quotient in the opposite direction
        -- before checking lexical closure and copying the bound.
        packetBoundScheme =
          schemeInPacketConstructionDomain
            packet
            (subtermGeneralizationGammaBoundScheme packet)
        alignPacketFreeRefs packetTy0 =
          foldl' alignFreeRef packetTy0 (freeTypeVarRefsType packetTy0)
        alignPacketFreeRefsWithin lexicalRefs packetTy0 =
          foldl'
            alignScopedFreeRef
            packetTy0
            (freeTypeVarRefsType packetTy0)
          where
            alignScopedFreeRef packetTy packetRef =
              case
                  find
                    (typeBinderRefsSameIdentity packetRef)
                    lexicalRefs
                of
                  Just lexicalRef ->
                    substTypeCaptureRef
                      packetRef
                      (TVarRef lexicalRef)
                      packetTy
                  Nothing -> alignFreeRef packetTy packetRef
        alignFreeRef packetTy0 packetRef =
          let outwardRef =
                case enclosingRefFor packetRef of
                  Just enclosingRef -> enclosingRef
                  Nothing ->
                    case sourceRouteForPacketFreeRef packetRef of
                      Just sourceRef ->
                        case enclosingRefFor sourceRef of
                          Just enclosingRef -> enclosingRef
                          Nothing -> sourceRef
                      Nothing -> packetRef
           in substTypeCaptureRef packetRef (TVarRef outwardRef) packetTy0

        -- Placement already runs in the packet's validated construction
        -- quotient.  Prefer that exact identity whenever the enclosing scheme
        -- either declares it or carries it as a lexical free dependency.
        -- Falling through to the outward source route first would undo a
        -- proved @source -> construction@ transition and manufacture a free
        -- source identity that the enclosing construction never owned.
        enclosingRefFor ref =
          case find (typeBinderRefsSameIdentity ref . fst) binders of
            Just (enclosingRef, _) -> Just enclosingRef
            Nothing ->
              find
                (typeBinderRefsSameIdentity ref)
                enclosingFreeRefs

        sourceRouteForPacketFreeRef packetRef =
          case packetConstructionSourceRoutes packetRef of
            sourceRef : _ -> Just sourceRef
            [] -> consumerSourceRoute packetRef

        packetConstructionSourceRoutes packetRef =
          [ sourceRef
          | (sourceRef, constructionRef) <-
              subtermGeneralizationConstructionBinderRenames packet
          , typeBinderRefsSameIdentity constructionRef packetRef
          , isGeneratedSourceRef sourceRef
          ]

        consumerSourceRoute packetRef = do
          graphNode <- typeBinderRefNode packetRef
          sourceRef <-
            IntMap.lookup (getNodeId graphNode) consumerRoutes
          guard (isGeneratedSourceRef sourceRef)
          pure sourceRef

        isGeneratedSourceRef ref =
          isJust
            ( typeBinderIdentityGeneratedUnique
                (typeBinderRefIdentity ref)
            )

        validatePacketFreeRefSourceRoutes targetRef =
          case sourceRouteDisagreements of
            (packetRef, firstSourceRef, secondSourceRef) : _ ->
              Left
                ( ValidationFailed
                    [ "prepared subterm free reference has conflicting explicit source routes"
                    , "  packet free reference: " ++ show packetRef
                    , "  first source binder: " ++ show firstSourceRef
                    , "  second source binder: " ++ show secondSourceRef
                    , "  consumer: " ++ show targetRef
                    ]
                )
            [] ->
              case sourceRouteConflicts of
                [] -> Right ()
                (packetRef, graphBinder, sourceRef) : _ ->
                  Left
                    ( ValidationFailed
                        [ "prepared subterm source route conflicts with an enclosing graph binder"
                        , "  packet free reference: " ++ show packetRef
                        , "  enclosing graph binder: " ++ show graphBinder
                        , "  routed source binder: " ++ show sourceRef
                        , "  consumer: " ++ show targetRef
                        ]
                    )

        sourceRouteDisagreements =
          [ (packetRef, firstSourceRef, conflictingSourceRef)
          | packetRef <- rawPacketFreeRefs
          , let explicitRoutes =
                  packetConstructionSourceRoutes packetRef
                    ++ maybeToList (consumerSourceRoute packetRef)
          , firstSourceRef : otherSourceRefs <- [explicitRoutes]
          , conflictingSourceRef <- otherSourceRefs
          , not
              ( typeBinderRefsSameIdentity
                  firstSourceRef
                  conflictingSourceRef
              )
          ]

        sourceRouteConflicts =
          [ (packetRef, graphBinder, sourceRef)
          | packetRef <- rawPacketFreeRefs
          , Just sourceRef <- [sourceRouteForPacketFreeRef packetRef]
          , (graphBinder, _) <- binders
          , typeBinderRefsSameIdentity graphBinder packetRef
          , not (typeBinderRefsSameIdentity graphBinder sourceRef)
          , not
              ( any
                  ( \(explicitSourceRef, constructionRef) ->
                      typeBinderRefsSameIdentity explicitSourceRef sourceRef
                        && typeBinderRefsSameIdentity constructionRef graphBinder
                  )
                  (subtermGeneralizationConstructionBinderRenames packet)
              )
          ]

        -- Installing a packet as a bound copies its quantified type.  The
        -- copied binders must receive fresh identities, not merely fresh
        -- display names: the original packet binder still scopes the emitted
        -- descendant type abstraction (the paper's @b@), while this copied
        -- binder scopes only the enclosing bound (the paper's @d@).
        freshenPacketBoundRefs
          :: Set.Set String
          -> Ty v
          -> Either ElabError (Ty v)
        freshenPacketBoundRefs used ty =
          case ty of
            TVarRef ref -> Right (TVarRef ref)
            TArrow dom cod ->
              TArrow
                <$> freshenPacketBoundRefs used dom
                <*> freshenPacketBoundRefs used cod
            TConWithIdentity identity con args ->
              TConWithIdentity identity con
                <$> traverse (freshenPacketBoundRefs used) args
            TVarAppRef ref args ->
              TVarAppRef ref
                <$> traverse (freshenPacketBoundRefs used) args
            TBaseWithIdentity identity base ->
              Right (TBaseWithIdentity identity base)
            TBottom -> Right TBottom
            TForallRef ref mbBound body -> do
              mbBound' <- traverse (freshenPacketBoundRefs used) mbBound
              copiedRef <- copiedBinderRef ref
              let binderName = freshPacketBinderName used ref
                  ref' = renameTypeBinderRef binderName copiedRef
                  bodyForRef = substTypeCaptureRef ref (TVarRef ref') body
                  used' = typeBinderRefAliasNames ref' `Set.union` used
              body' <- freshenPacketBoundRefs used' bodyForRef
              pure (TForallRef ref' mbBound' body')
            TMuRef ref body -> do
              copiedRef <- copiedBinderRef ref
              let binderName = freshPacketBinderName used ref
                  ref' = renameTypeBinderRef binderName copiedRef
                  bodyForRef = substTypeCaptureRef ref (TVarRef ref') body
                  used' = typeBinderRefAliasNames ref' `Set.union` used
              body' <- freshenPacketBoundRefs used' bodyForRef
              pure (TMuRef ref' body')

        copiedBinderRef ref =
          case copiedRefFor ref of
            Just copiedRef -> Right copiedRef
            Nothing ->
              Left
                ( ValidationFailed
                    [ "prepared subterm scheme is missing its allocated copied-binder identity"
                    , "  binder: " ++ show ref
                    , "  allocated copies: " ++ show (psgCopiedBinderRefs packet)
                    , "  prepared Gamma bound: " ++ show (psgGammaBoundScheme packet)
                    ]
                )
          where
            copiedRefFor candidate =
              case
                  Map.lookup
                    (typeBinderRefIdentity candidate)
                    (psgCopiedBinderRefs packet)
                of
                  Just copiedRef -> Just copiedRef
                  Nothing -> do
                    packetRef <-
                      fst
                        <$> find
                          ( typeBinderRefsSameIdentity candidate
                              . snd
                          )
                          packetOwnedConstructionRoutes
                    Map.lookup
                      (typeBinderRefIdentity packetRef)
                      (psgCopiedBinderRefs packet)

        freshPacketBinderName used ref
          | Set.member (typeBinderRefName ref) used =
              freshAlphaName used
          | otherwise = typeBinderRefName ref

        freshAlphaName used =
          go 0
          where
            go idx
              | Set.member candidate used = go (idx + 1)
              | otherwise = candidate
              where
                candidate = alphaName idx 0
        -- The consumer identity is captured while the descendant packet is
        -- prepared.  Do not rediscover it by inspecting the enclosing bound:
        -- reification may represent an unused packet-owned parameter as
        -- bottom.  Removal still refers to the packet's original graph
        -- identities; the bound payload above is a fresh copy.
        packetOwnedRefs = boundRefsInType packetTyUnfreshened
        packetSourceBinderRefs =
          IntMap.elems
            ( siSourceBinderOrderRefs
                (psgConsumerConstructionSchemeInfo packet)
            )
            ++ IntMap.elems
              (siSourceBinderOrderRefs (psgSchemeInfo packet))
        binders = schemeBinderRefs scheme
        isPacketSourceOwned ref =
          any (typeBinderRefsSameIdentity ref) packetSourceBinderRefs
        isPacketOwned ref =
          any (typeBinderRefsSameIdentity ref) packetOwnedRefs
            || any
              (typeBinderRefsSameIdentity ref . snd)
              packetOwnedConstructionRoutes
        packetOwnedConstructionRoutes =
          [ (packetRef, constructionRef)
          | packetRef <- packetOwnedRefs
          , packetNode <- maybeToList (typeBinderRefNode packetRef)
          , constructionRef <-
              maybeToList
                ( IntMap.lookup
                    (getNodeId packetNode)
                    consumerRoutes
                )
          ]
        -- A root RaiseMerge packet can bind the same exterior identity that
        -- consumes it at the enclosing boundary.  The packet's copy is
        -- freshened above, so identity equality here selects the enclosing
        -- construction binder; treating it as merely packet-owned would lose
        -- the only paper-authorized Gamma entry.
        targetRefs = targetRefsIn binders
        nestedTargets consumerIdentity =
          [ declaration
          | declaration <-
              nestedForallDeclarationsInScheme scheme
          , isJust (nfdBound declaration) || packetHasTopologyConsumer
          , consumerTargetsRef consumerIdentity (nfdRef declaration)
          ]
        nestedTargetsOutsideTopLevelConsumer targetRef consumerIdentity =
          [ declaration
          | declaration <- nestedTargets consumerIdentity
          , not
              ( maybe
                  False
                  (typeBinderRefsSameIdentity targetRef)
                  (nfdOuterBoundOwner declaration)
              )
          ]
        targetRefsIn candidateBinders consumerIdentity =
          foldr insertDistinctRef [] (directTargets ++ routedTargets)
          where
            directTargets =
              [ ref
              | (ref, mbBound) <- candidateBinders
              , isJust mbBound || packetCanConstructUnboundedConsumer
              , typeBinderRefIdentity ref == consumerIdentity
              ]
            routedTargets =
              [ ref
              | consumerNode <- maybeToList (typeBinderIdentityNode consumerIdentity)
              , routedRef <-
                  maybeToList
                    (IntMap.lookup (getNodeId consumerNode) consumerRoutes)
              , (ref, mbBound) <- candidateBinders
              , isJust mbBound || packetCanConstructUnboundedConsumer
              , typeBinderRefsSameIdentity ref routedRef
              ]
            insertDistinctRef ref refs
              | any (typeBinderRefsSameIdentity ref) refs = refs
              | otherwise = ref : refs

        consumerTargetsRef consumerIdentity ref =
          typeBinderRefIdentity ref == consumerIdentity
            || any
              (typeBinderRefsSameIdentity ref)
              [ routedRef
              | consumerNode <-
                  maybeToList
                    (typeBinderIdentityNode consumerIdentity)
              , routedRef <-
                  maybeToList
                    ( IntMap.lookup
                        (getNodeId consumerNode)
                        consumerRoutes
                    )
              ]

        -- A topology packet is allowed to identify an unbounded candidate so
        -- placement can report a missing checked construction bound precisely.
        -- It never authorizes manufacturing that bound from the packet type.
        packetHasTopologyConsumer =
          case psgConsumerAuthority packet of
            Just TopologyConsumerAuthority{} -> True
            _ -> False

        -- A source/canonical owner walk can seal the exact endpoint that an
        -- enclosing source constructor will publish.  That positive
        -- certificate is the only non-topology authority allowed to select an
        -- unbounded top-level consumer.  The callback must still accept the
        -- certificate for the current construction owner before placement
        -- installs the bound.
        packetCanConstructUnboundedConsumer =
          packetHasTopologyConsumer
            || isJust
              (subtermGeneralizationSourceOwnerConsumerCompletion packet)

        validatePacketFreeRefs targetRef =
          validatePacketFreeRefsWithin
            targetRef
            (bindersBefore targetRef)
            packetFreeRefs

        validatePacketFreeRefsWithin
          targetRef
          lexicalRefs
          candidatePacketFreeRefs =
          case unexpectedPacketFreeRefsWithin lexicalRefs candidatePacketFreeRefs of
            [] -> Right ()
            unexpected ->
              Left
                ( ValidationFailed
                    ( "prepared subterm scheme has free references outside its enclosing lexical scheme"
                        : map (("  free reference: " ++) . show) unexpected
                            ++ [ "  consumer: " ++ show targetRef
                               , "  packet bound: " ++ show packetBoundScheme
                               , "  packet construction scheme: "
                                   ++ show (siScheme (psgSchemeInfo packet))
                               , "  packet construction substitution: "
                                   ++ show (siSubstRefs (psgSchemeInfo packet))
                               , "  packet operated scheme: "
                                   ++ show (siScheme (psgOperatedSchemeInfo packet))
                               , "  packet operated substitution: "
                                   ++ show
                                     (siSubstRefs (psgOperatedSchemeInfo packet))
                               , "  packet construction binder renames: "
                                   ++ show
                                     ( subtermGeneralizationConstructionBinderRenames
                                         packet
                                     )
                               , "  packet compiler-exact binder renames: "
                                   ++ show
                                     ( subtermGeneralizationCompilerExactBinderRenames
                                         packet
                                     )
                               , "  packet opaque result construction: "
                                   ++ show
                                     (psgOpaqueResultConstruction packet)
                               , "  packet source lambda parameter: "
                                   ++ show
                                     (psgSourceLambdaParameter packet)
                               , "  packet consumer authority: "
                                   ++ show (psgConsumerAuthority packet)
                               , "  packet Gamma authority: "
                                   ++ show (psgGammaAuthority packet)
                               , "  packet source-order routes: "
                                   ++ show
                                     ( siSourceBinderOrderRefs
                                         (psgSchemeInfo packet)
                                     )
                               , "  packet construction-order routes: "
                                   ++ show
                                     ( siConstructionBinderOrderRefs
                                         (psgSchemeInfo packet)
                                     )
                               , "  raw packet free references: "
                                   ++ show rawPacketFreeRefs
                               , "  aligned packet free references: "
                                   ++ show packetFreeRefs
                               , "  inherited packet references: "
                                   ++ show inheritedPacketRefs
                               , "  enclosing consumer routes: "
                                   ++ show consumerRoutes
                               , "  enclosing scheme: " ++ show scheme
                               , "  enclosing binder identities: "
                                   ++ show (map (typeBinderRefIdentity . fst) binders)
                               ]
                    )
                )

        unexpectedPacketFreeRefsWithin lexicalRefs candidatePacketFreeRefs =
          filter
            (not . isAllowedPacketFreeRefWithin lexicalRefs)
            candidatePacketFreeRefs

        isAllowedPacketFreeRefWithin lexicalRefs packetRef =
          any (typeBinderRefsSameIdentity packetRef) lexicalRefs
            || any (typeBinderRefsSameIdentity packetRef) enclosingFreeRefs
            || any
              (typeBinderRefsSameIdentity packetRef)
              inheritedPacketRefs

        -- Packet preparation records the exact inherited Gamma capability.
        -- Those references are lexical dependencies of the packet even when
        -- the enclosing scheme has already projected their graph occurrence
        -- away.  Admit only these identity-bearing routes; type shape or
        -- display names never create ambient authority.
        inheritedPacketRefs =
          Reify.inheritedGammaRoutesLexicalRefs
            (psgInheritedGammaRoutes packet)
            ++ map
              Reify.inheritedGammaRouteRef
              ( Reify.inheritedGammaRoutesEntries
                  (psgInheritedGammaRoutes packet)
              )

        bindersBefore targetRef =
          bindersBeforeIn binders targetRef

        bindersBeforeIn candidateBinders targetRef =
          map fst
            ( takeWhile
                (not . typeBinderRefsSameIdentity targetRef . fst)
                candidateBinders
            )

        -- A reference that is free in the whole enclosing scheme but also has
        -- a later binder in that scheme is not an outer lexical reference: it
        -- is an out-of-scope forward reference and must not be admitted into a
        -- newly installed bound.
        enclosingFreeRefs =
          filter
            (\freeRef -> not (any (typeBinderRefsSameIdentity freeRef . fst) binders))
            (freeTypeVarRefsType (schemeToType scheme))

        operationallyMatchesCompletedType completedType constructedType =
          alphaEqType completedType constructedType
            || packetTypeSpecializesToExactEndpoint
              completedType
              constructedType
            || packetTypeSpecializesToExactEndpoint
              constructedType
              completedType

        rawPacketFreeRefs =
          freeTypeVarRefsType (schemeToType packetBoundScheme)

        packetFreeRefs =
          freeTypeVarRefsType packetTyUnfreshened

        -- The enclosing generalization initially binds every flexible node in
        -- the descendant construction.  Packet ownership then distinguishes
        -- the binders copied into S'(operated) from lexical dependencies that
        -- must remain free in that bound.  Release exactly those validated
        -- packet-free identities from the leading constructed spine before
        -- comparing it with the prepared packet; this is K's transition from
        -- @forall b a. b -> a@ to @forall b. b -> a@, not a post-hoc type
        -- repair.
        releasePacketFreeBinders ty =
          case ty of
            TForallRef ref _ body
              | any (typeBinderRefsSameIdentity ref) packetFreeRefs ->
                  releasePacketFreeBinders body
            TForallRef ref mbBound body ->
              TForallRef ref mbBound (releasePacketFreeBinders body)
            _ -> ty

        releasePacketSourceBinders ty =
          case ty of
            TForallRef ref _ body
              | isPacketSourceOwned ref ->
                  releasePacketSourceBinders body
            TForallRef ref mbBound body ->
              TForallRef ref mbBound (releasePacketSourceBinders body)
            _ -> ty

boundRefsInType :: Ty v -> [TypeBinderRef]
boundRefsInType ty =
  case ty of
    TVarRef {} -> []
    TArrow dom cod -> boundRefsInType dom ++ boundRefsInType cod
    TConWithIdentity _ _ args -> concatMap boundRefsInType args
    TVarAppRef _ args -> concatMap boundRefsInType args
    TBaseWithIdentity {} -> []
    TForallRef ref mbBound body ->
      ref
        : maybe [] (boundRefsInType . tyToElab) mbBound
          ++ boundRefsInType body
    TMuRef ref body -> ref : boundRefsInType body
    TBottom -> []

-- | Generate a name for a rigid type variable based on its key.
rigidNameFor :: Int -> String
rigidNameFor key = "__rigid" ++ show key

-- | Rebuild a substitution that was synthesized purely from graph-local
-- nodes.  Never apply this to a 'ReifyRootChoice' substitution: those values
-- can intentionally carry an alias or source identity distinct from the map
-- key, and are already paired with the correct graph domain.
graphIdentitySubstRefs :: (NodeId -> NodeId) -> IntMap.IntMap TypeBinderRef -> IntMap.IntMap TypeBinderRef
graphIdentitySubstRefs canonical =
  IntMap.mapWithKey
    ( \key ref ->
        typeBinderRefFromIdentity
          (typeBinderIdentityFromNode (canonical (NodeId key)))
          (typeBinderRefName ref)
    )

-- | A reification root paired with the graph that owns it.
--
-- Keeping the graph and root in one value prevents a base-domain 'NodeId'
-- from being reified accidentally through the live solved view.  The two
-- graphs may legally reuse the same numeric key.
data ReifyRootDomain p
  = LiveReifyRootDomain (PresolutionView p) NodeId
  | BaseReifyRootDomain (Constraint p) NodeId

reifyRootDomain ::
  PresolutionView p ->
  Maybe (GaBindParents p) ->
  ReifyRootSource ->
  Either ElabError (ReifyRootDomain p)
reifyRootDomain liveView mbGa source =
  case source of
    ReifyLiveRoot root ->
      Right (LiveReifyRootDomain liveView root)
    ReifyBaseSchemeRoot root ->
      case mbGa of
        Just ga -> Right (BaseReifyRootDomain (gaBaseConstraint ga) root)
        Nothing ->
          Left $
            ValidationFailed
              [ "base-scheme reification root has no base graph",
                "  root: " ++ show root
              ]

reifyRootDomainNode :: ReifyRootDomain p -> NodeId
reifyRootDomainNode domain =
  case domain of
    LiveReifyRootDomain _ root -> root
    BaseReifyRootDomain _ root -> root

withReifyAliasNodes :: IntMap.IntMap TyNode -> ReifyRootDomain p -> ReifyRootDomain p
withReifyAliasNodes aliasNodes domain =
  case domain of
    LiveReifyRootDomain liveView root ->
      let liveConstraint = pvCanonicalConstraint liveView
          liveNodes =
            IntMap.fromList
              [ (getNodeId nid, node)
                | (nid, node) <- toListNode (cNodes liveConstraint)
              ]
          aliasedConstraint =
            liveConstraint {cNodes = NodeMap (IntMap.union aliasNodes liveNodes)}
          aliasedView =
            presolutionViewFromSnapshot aliasedConstraint (pvCanonicalMap liveView)
       in LiveReifyRootDomain aliasedView root
    BaseReifyRootDomain baseConstraint root ->
      let baseNodes =
            IntMap.fromList
              [ (getNodeId nid, node)
                | (nid, node) <- toListNode (cNodes baseConstraint)
              ]
          aliasedConstraint =
            baseConstraint {cNodes = NodeMap (IntMap.union aliasNodes baseNodes)}
       in BaseReifyRootDomain aliasedConstraint root

reifyTypeInRootDomain ::
  ReifyRootDomain p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  [TypeBinderRef] ->
  IntMap.IntMap [NodeId] ->
  Either ElabError ElabType
reifyTypeInRootDomain domain substMap externalKeys outerBinderRefs structuralBinders =
  -- 'ReifyRootChoice' already pairs this root with a substitution from the
  -- same graph domain.  Its values are identity-bearing alias authority: for
  -- example, a structural graph key can deliberately route to a different
  -- outer Gamma identity.  Rebuilding refs from their map keys here would
  -- erase that route before the reifier decides whether the outer scheme
  -- already owns a structural binder.
  case domain of
    LiveReifyRootDomain liveView root ->
      reifyTypeWithOuterBinderRefsNoFallback
        liveView
        substMap
        externalKeys
        outerBinderRefs
        structuralBinders
        root
    BaseReifyRootDomain baseConstraint root ->
      reifyTypeWithOuterBinderRefsNoFallbackOnConstraint
        baseConstraint
        substMap
        externalKeys
        outerBinderRefs
        structuralBinders
        root

reifyBoundInRootDomain ::
  ReifyRootDomain p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  IntMap.IntMap [NodeId] ->
  NodeId ->
  Either ElabError ElabType
reifyBoundInRootDomain domain substMap externalKeys structuralBinders boundRoot =
  -- Bounds consume the same identity-bearing, domain-matched substitution as
  -- result types.  Preserve its values so the binder declaration and its
  -- bound cannot be reified in different identity domains.
  case domain of
    LiveReifyRootDomain liveView _ ->
      reifyBoundWithExternalRefs
        liveView
        substMap
        externalKeys
        structuralBinders
        boundRoot
    BaseReifyRootDomain baseConstraint _ ->
      reifyBoundWithExternalRefsOnConstraint
        baseConstraint
        substMap
        externalKeys
        structuralBinders
        boundRoot

-- | Validate that solved-order and base-path shadow reification are semantically equivalent.
shadowCompareTypes :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypes context solvedTy baseTy =
  shadowCompareTypesWithDetails context defaultShadowDetails solvedTy baseTy

shadowCompareTypesWithDetails :: String -> [String] -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypesWithDetails context detailLines solvedTy baseTy
  | alphaEqType solvedTy baseTy || alphaEqTypeModuloVarRenaming solvedTy baseTy = Right ()
  | otherwise =
      Left $
        ValidationFailed
          ( [ "shadow reify mismatch",
              "context=" ++ context
            ]
              ++ detailLines
              ++ [ "solved=" ++ pretty solvedTy,
                   "base=" ++ pretty baseTy
                 ]
          )

data RenameEnv = RenameEnv
  { reForward :: [(TypeBinderRef, TypeBinderRef)],
    reBackward :: [(TypeBinderRef, TypeBinderRef)]
  }

alphaEqTypeModuloVarRenaming :: ElabType -> ElabType -> Bool
alphaEqTypeModuloVarRenaming tyL tyR =
  case goType (RenameEnv [] []) tyL tyR of
    Just _ -> True
    Nothing -> False
  where
    goType :: RenameEnv -> ElabType -> ElabType -> Maybe RenameEnv
    goType env t1 t2 = case (t1, t2) of
      (TVarRef ref1, TVarRef ref2) ->
        matchVar env ref1 ref2
      (TArrow a1 b1, TArrow a2 b2) -> do
        env' <- goType env a1 a2
        goType env' b1 b2
      (TConWithIdentity identity1 _ args1, TConWithIdentity identity2 _ args2)
        | typeHeadMatches identity1 identity2 ->
            goTypes env (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TBaseWithIdentity identity1 _, TBaseWithIdentity identity2 _)
        | typeHeadMatches identity1 identity2 ->
            Just env
      (TBottom, TBottom) ->
        Just env
      (TVarAppRef ref1 args1, TVarAppRef ref2 args2) -> do
        env' <- matchVar env ref1 ref2
        goTypes env' (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) -> do
        env' <- goMaybeBound env mb1 mb2
        withScopedVar env' ref1 ref2 (\scoped -> goType scoped body1 body2)
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        withScopedVar env ref1 ref2 (\scoped -> goType scoped body1 body2)
      _ ->
        Nothing

    goBound :: RenameEnv -> BoundType -> BoundType -> Maybe RenameEnv
    goBound env b1 b2 = case (b1, b2) of
      (TArrow a1 b1', TArrow a2 b2') -> do
        env' <- goType env a1 a2
        goType env' b1' b2'
      (TConWithIdentity identity1 _ args1, TConWithIdentity identity2 _ args2)
        | typeHeadMatches identity1 identity2 ->
            goTypes env (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TBaseWithIdentity identity1 _, TBaseWithIdentity identity2 _)
        | typeHeadMatches identity1 identity2 ->
            Just env
      (TBottom, TBottom) ->
        Just env
      (TVarAppRef ref1 args1, TVarAppRef ref2 args2) -> do
        env' <- matchVar env ref1 ref2
        goTypes env' (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) -> do
        env' <- goMaybeBound env mb1 mb2
        withScopedVar env' ref1 ref2 (\scoped -> goType scoped body1 body2)
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        withScopedVar env ref1 ref2 (\scoped -> goType scoped body1 body2)
      _ ->
        Nothing

    goTypes :: RenameEnv -> [ElabType] -> [ElabType] -> Maybe RenameEnv
    goTypes env left right = case (left, right) of
      ([], []) -> Just env
      (l : ls, r : rs) -> do
        env' <- goType env l r
        goTypes env' ls rs
      _ -> Nothing

    goMaybeBound :: RenameEnv -> Maybe BoundType -> Maybe BoundType -> Maybe RenameEnv
    goMaybeBound env mb1 mb2 = case (mb1, mb2) of
      (Nothing, Nothing) -> Just env
      (Just b1, Just b2) -> goBound env b1 b2
      _ -> Nothing

    matchVar :: RenameEnv -> TypeBinderRef -> TypeBinderRef -> Maybe RenameEnv
    matchVar env@RenameEnv {reForward = forward, reBackward = backward} v1 v2 =
      case (lookupRef v1 forward, lookupRef v2 backward) of
        (Just mappedV2, Just mappedV1)
          | typeBinderRefsSameIdentity mappedV2 v2 && typeBinderRefsSameIdentity mappedV1 v1 ->
              Just env
        (Just mappedV2, Nothing)
          | typeBinderRefsSameIdentity mappedV2 v2 ->
              Just env {reBackward = insertPair v2 v1 backward}
        (Nothing, Just mappedV1)
          | typeBinderRefsSameIdentity mappedV1 v1 ->
              Just env {reForward = insertPair v1 v2 forward}
        (Nothing, Nothing)
          | refsCanRename v1 v2 ->
          Just
            env
              { reForward = insertPair v1 v2 forward,
                reBackward = insertPair v2 v1 backward
              }
        _ ->
          Nothing

    withScopedVar ::
      RenameEnv ->
      TypeBinderRef ->
      TypeBinderRef ->
      (RenameEnv -> Maybe RenameEnv) ->
      Maybe RenameEnv
    withScopedVar env@RenameEnv {reForward = forward, reBackward = backward} v1 v2 runScoped = do
      let oldForward = lookupEntry v1 forward
          oldBackward = lookupEntry v2 backward
          scopedEnv =
            env
              { reForward = insertPair v1 v2 forward,
                reBackward = insertPair v2 v1 backward
              }
          restore key oldValue pairs = case oldValue of
            Just (_, value) -> insertPair key value pairs
            Nothing -> deleteRef key pairs
      scopedResult <- runScoped scopedEnv
      pure
        scopedResult
          { reForward = restore v1 oldForward (reForward scopedResult),
            reBackward = restore v2 oldBackward (reBackward scopedResult)
          }

    lookupRef ref =
      fmap snd . find (typeBinderRefsSameIdentity ref . fst)

    lookupEntry ref =
      find (typeBinderRefsSameIdentity ref . fst)

    insertPair key value pairs =
      (key, value) : deleteRef key pairs

    deleteRef key =
      filter (not . typeBinderRefsSameIdentity key . fst)

    refsCanRename left right =
      typeBinderRefsSameIdentity left right

selectSolvedOrderWithShadow :: String -> ElabType -> Maybe ElabType -> Either ElabError ElabType
selectSolvedOrderWithShadow context solvedTy mbBaseTy =
  selectSolvedOrderWithShadowWithDetails context defaultShadowDetails solvedTy mbBaseTy

selectSolvedOrderWithShadowWithDetails ::
  String ->
  [String] ->
  ElabType ->
  Maybe ElabType ->
  Either ElabError ElabType
selectSolvedOrderWithShadowWithDetails context detailLines solvedTy mbBaseTy =
  case mbBaseTy of
    Nothing -> Right solvedTy
    Just baseTy -> do
      shadowCompareTypesWithDetails context detailLines solvedTy baseTy
      Right solvedTy

defaultShadowDetails :: [String]
defaultShadowDetails =
  [ "scopeRootC=<unknown>",
    "typeRoot=<unknown>",
    "binders=[]"
  ]

-- | Inline rigid type variables by substituting them with their bounds.
-- Uses cycle detection to prevent infinite loops when bounds reference each other.
inlineRigidTypes :: Map.Map TypeBinderRef ElabType -> ElabType -> ElabType
inlineRigidTypes rigidBounds = go Set.empty Set.empty
  where
    go bound seen ty = case ty of
      TVarRef ref ->
        case Map.lookup ref rigidBounds of
          Just rigidTy
            | Set.notMember ref bound && Set.notMember ref seen ->
                go bound (Set.insert ref seen) rigidTy
          _ -> TVarRef ref
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (go bound seen) args)
      TVarAppRef ref args -> TVarAppRef ref (fmap (go bound seen) args)
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TArrow a b -> TArrow (go bound seen a) (go bound seen b)
      TForallRef ref mb body ->
        TForallRef ref (fmap (goBound bound seen) mb) (go (Set.insert ref bound) seen body)
      TMuRef ref body -> TMuRef ref (go (Set.insert ref bound) seen body)
    goBound bound seen = \case
      TArrow a b -> TArrow (go bound seen a) (go bound seen b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (go bound seen) args)
      TVarAppRef ref args -> TVarAppRef ref (fmap (go bound seen) args)
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body -> TForallRef ref (fmap (goBound bound seen) mb) (go (Set.insert ref bound) seen body)
      TMuRef ref body -> TMuRef ref (go (Set.insert ref bound) seen body)

-- | Join an exact application-result request with the binder route selected
-- by the same generalization plan and finalized scheme.  The construction
-- root narrows the planner's ordered binders by structural reachability; the
-- finalized substitution and outer scheme spine then certify the emitted
-- identity.  Reified type shape is deliberately not an input.
certifyGeneralizedResultRoute
  :: GeneralizedResultRouteRequest
  -> GeneralizePlan p
  -> ReifyPlan
  -> ElabScheme
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError GeneralizedResultRoute
certifyGeneralizedResultRoute request plan reifyPlanWrapper scheme finalizedSubst = do
  unless
    (grrrOwnerTarget request == target0)
    ( certificateFailure
        "request owner does not match the generalized target"
        [ "  request owner: " ++ show (grrrOwnerTarget request)
        , "  generalized target: " ++ show target0
        ]
    )
  (constructionRoot, mbReifiedResultRef) <- resolveConstructionRoot
  case mbReifiedResultRef of
    Nothing ->
      unless
        ( IntSet.member
            (getNodeId constructionRoot)
            (reachableFromWithBounds typeRoot)
        )
        ( certificateFailure
            "result construction root is outside the generalized type root"
            [ "  type root: " ++ show typeRoot
            , "  construction root: " ++ show constructionRoot
            ]
        )
    Just _ ->
      -- The exact traversal through the selected reify root below is already
      -- the reachability proof in that graph domain.  Rechecking a base-domain
      -- node through the live canonical graph would collapse an allocated
      -- source codomain into its enclosing application-result copy.
      pure ()
  let constructionReachable =
        reachableFromStructural constructionRoot
      candidateBinders =
        case mbReifiedResultRef of
          Nothing ->
            [ (binderKey, plannedRef)
            | (binderKey, plannedRef) <- orderedBinders
            , IntSet.member binderKey constructionReachable
            ]
          Just reifiedResultRef ->
            [ (binderKey, plannedRef)
            | (binderKey, plannedRef) <- orderedBinders
            , typeBinderRefsSameIdentity plannedRef reifiedResultRef
            ]
  (binderKey, plannedRef) <-
    case candidateBinders of
      [] ->
        certificateFailure
          "result construction root has no planned result binder"
          [ "  construction root: "
              ++ show constructionRoot
          , "  ordered binders: " ++ show orderedBinders
          ]
      first@(_, firstRef) : remaining
        | all
            (typeBinderRefsSameIdentity firstRef . snd)
            remaining ->
            pure first
        | otherwise ->
            certificateFailure
              "result construction root has ambiguous planned result binders"
              [ "  construction root: "
                  ++ show constructionRoot
              , "  candidates: " ++ show candidateBinders
              ]
  routedRef <-
    case IntMap.lookup binderKey binderRefRoutes of
      Just ref
        | typeBinderRefsSameIdentity ref plannedRef ->
            pure ref
      route ->
        certificateFailure
          "planned result binder has no matching planner route"
          [ "  binder: " ++ show (binderKey, plannedRef)
          , "  planner route: " ++ show route
          ]
  finalizedRef <-
    case IntMap.lookup binderKey finalizedSubst of
      Just ref
        | typeBinderRefsSameIdentity ref routedRef ->
            pure ref
      route ->
        certificateFailure
          "planned result binder has no matching finalized route"
          [ "  binder: " ++ show (binderKey, routedRef)
          , "  finalized route: " ++ show route
          , "  finalized substitution: " ++ show finalizedSubst
          ]
  unless
    ( any
        (typeBinderRefsSameIdentity finalizedRef . fst)
        (schemeSpineBinders scheme)
    )
    ( certificateFailure
        "finalized result route is absent from the scheme binder spine"
        [ "  result route: " ++ show finalizedRef
        , "  scheme binders: " ++ show (schemeSpineBinders scheme)
        ]
    )
  pure
    GeneralizedResultRoute
      { grrOwnerTarget = grrrOwnerTarget request
      , grrTypeRoot = typeRoot
      , grrFrozenConsumer = grrrFrozenConsumer request
      , grrConstructionRoot = constructionRoot
      , grrBinderNode = NodeId binderKey
      , grrBinderRef = finalizedRef
      }
  where
    GeneralizePlan
      { gpEnv =
          GeneralizeEnv
            { geNodes = planNodes
            , geCanonical = planCanonical
            , geBindParentsGa = mbPlanGa
            }
      , gpContext = GeneralizeCtx {gcTarget0 = target0}
      , gpTypeRootPlan = TypeRootPlan {trTypeRoot = typeRoot}
      , gpBinderPlan =
          BinderPlan
            { bpOrderedBinders = orderedBinders
            , bpBinderRefRoutes = binderRefRoutes
            }
      , gpReachableFromWithBounds = reachableFromWithBounds
      , gpReachableFromStructural = reachableFromStructural
      } = plan
    ReifyPlan
      { rpPlan = resultReifyPlan
      } = reifyPlanWrapper
    Reify.ReifyPlan
      { Reify.rpRootChoice = resultRootChoice
      } = resultReifyPlan
    resultReifyRootSource = Reify.rrcSource resultRootChoice
    resultReifySubst = Reify.rrcSubst resultRootChoice

    resolveConstructionRoot =
      case grrrRouteLocator request of
        ApplicationResultConstruction root -> pure (root, Nothing)
        LambdaCodomainConstruction arity
          | arity <= 0 ->
              certificateFailure
                "source-lambda result locator has non-positive arity"
                ["  lambda arity: " ++ show arity]
          | otherwise -> do
              constructionRoot <-
                descendReifiedLambdaCodomain
                  arity
                  resultReifyRootSource
              reifiedResultRef <-
                case
                    IntMap.lookup
                      (getNodeId constructionRoot)
                      resultReifySubst
                  of
                    Just ref -> pure ref
                    Nothing ->
                      certificateFailure
                        "source-lambda codomain has no selected reify substitution"
                        [ "  reify root source: "
                            ++ show resultReifyRootSource
                        , "  construction root: "
                            ++ show constructionRoot
                        , "  reify substitution: "
                            ++ show resultReifySubst
                        ]
              pure (constructionRoot, Just reifiedResultRef)

    descendReifiedLambdaCodomain remaining source =
      case source of
        ReifyLiveRoot root ->
          descendLambdaCodomain
            "live"
            (planCanonical root)
            planCanonical
            ( \node ->
                IntMap.lookup
                  (getNodeId (planCanonical node))
                  planNodes
            )
            remaining
        ReifyBaseSchemeRoot root ->
          case mbPlanGa of
            Nothing ->
              certificateFailure
                "base source-lambda reify root has no base graph"
                ["  reify root: " ++ show root]
            Just ga ->
              descendLambdaCodomain
                "base"
                root
                id
                (lookupNodeIn (cNodes (gaBaseConstraint ga)))
                remaining

    -- Follow the exact graph chosen by reification.  Leading graph foralls are
    -- the explicit type abstractions emitted before the source value lambda;
    -- the value-lambda arity then selects its immediate codomain.  This is the
    -- construction-side counterpart of 'sourceLambdaCodomain', not a search in
    -- the finished type.
    descendLambdaCodomain domainLabel root normalize lookupRoot remaining =
      descendValueArrows remaining (stripLeadingForalls root)
      where
        stripLeadingForalls node =
          let nodeN = normalize node
           in case lookupRoot nodeN of
                Just TyForall {tnBody = body} ->
                  stripLeadingForalls body
                _ -> nodeN

        descendValueArrows remainingArrows node
          | remainingArrows <= 0 = pure (normalize node)
          | otherwise =
              let nodeN = normalize node
               in case lookupRoot nodeN of
                    Just TyArrow {tnCod = codomain} ->
                      descendValueArrows (remainingArrows - 1) codomain
                    present ->
                      certificateFailure
                        "selected reify root ended before the exact source-lambda codomain"
                        [ "  lambda arity: "
                            ++ show
                              ( case grrrRouteLocator request of
                                  LambdaCodomainConstruction arity -> arity
                                  ApplicationResultConstruction _ -> 0
                              )
                        , "  remaining value abstractions: "
                            ++ show remainingArrows
                        , "  reify graph domain: " ++ domainLabel
                        , "  non-arrow node: " ++ show (nodeN, present)
                        ]

    schemeSpineBinders candidateScheme =
      schemeBinderRefs candidateScheme
        ++ leadingBodyBinders (schemeBody candidateScheme)

    leadingBodyBinders ty =
      case ty of
        TForallRef ref mbBound body ->
          (ref, mbBound) : leadingBodyBinders body
        _ -> []

    certificateFailure
      :: String
      -> [String]
      -> Either ElabError a
    certificateFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid generalized result certificate"
              , "  detail: " ++ detail
              , "  request: " ++ show request
              ]
                ++ context
            )
        )

applyGeneralizePlan ::
  GeneralizePlan p ->
  ReifyPlan ->
  Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
applyGeneralizePlan plan reifyPlanWrapper = do
  let GeneralizePlan
        { gpEnv = env,
          gpContext = ctx,
          gpSchemeRootsPlan = schemeRootsPlan,
          gpTypeRootPlan = typeRootPlan,
          gpBinderPlan = binderPlan,
          gpScopeHasStructuralScheme = scopeHasStructuralScheme,
          gpBinders0 = binders0,
          gpReachableFromWithBounds = reachableFromWithBounds,
          gpRigidBindParents = rigidBindParents,
          gpBindParents = bindParents
        } = plan
      GeneralizeEnv { geConstraint = constraint,
          geOriginalConstraint = originalConstraint,
          geNodes = nodes,
          geCanonical = canonical,
          geBindParentsGa = mbBindParentsGa
        } = env
      GeneralizeCtx
        { gcTarget0 = target0,
          gcScopeRootC = scopeRootC,
          gcScopeGen = scopeGen,
          gcFirstGenAncestor = firstGenAncestorGa,
          gcResForReify = resForReify,
          gcBindParentsGaInfo = mbBindParentsGaInfo
        } = ctx
      SchemeRootsPlan
        { srInfo = schemeRootInfo,
          srSchemeRootByBodyBase = schemeRootByBodyBase
        } = schemeRootsPlan
      SchemeRootInfo
        { sriRootKeySet = schemeRootKeySet,
          sriRootOwner = schemeRootOwner,
          sriRootByBody = schemeRootByBody
        } = schemeRootInfo
      TypeRootPlan
        { trUseBoundTypeRoot = useBoundTypeRoot,
          trTypeRoot0 = typeRoot0,
          trTypeRoot = typeRoot
        } = typeRootPlan
      BinderPlan
        { bpOrderedBinders = orderedBinderEntries,
          bpRootBodyClosureKeys = rootBodyClosureKeys,
          bpLocallyClosedGammaKeys = locallyClosedGammaKeys,
          bpGammaAlias = gammaAliasPlan,
          bpNamedUnderGaSet = namedUnderGaSetPlan,
          bpSolvedToBasePref = solvedToBasePrefPlan,
          bpAliasBinderBases = aliasBinderBasesPlan,
          bpRequiredGamma = requiredGammaPlan,
          bpSourceBinderRefs = binderSourceBinderRefs,
          bpAmbientBinderRefs = ambientBinderRefs,
          bpTermUsedRootBinderRefs = termUsedRootBinderRefs
        } = binderPlan
      -- A variable-shaped S(operated) is an alias only when the caller has
      -- supplied the corresponding lexical declaration.  Its shape alone is
      -- not scope authority: without an ambient or source-sidecar witness this
      -- boundary must still construct the required Gamma binder.
      requiredGammaAliasAuthorityRefs =
        ambientBinderRefs ++ IntMap.elems binderSourceBinderRefs
      orderedBinders = map fst orderedBinderEntries
      ReifyPlan
        { rpPlan = reifyPlan
        } = reifyPlanWrapper
      Reify.ReifyPlan
        { Reify.rpSubst = substRefs,
          Reify.rpRootChoice = rootChoice,
          Reify.rpSchemeTypeChoice = schemeTypeChoice,
          Reify.rpBindingScopeGen = bindingScopeGenPlan,
          Reify.rpHasExplicitBound = hasExplicitBoundPlan,
          Reify.rpIsTargetSchemeBinder = isTargetSchemeBinderPlan,
          Reify.rpBoundMentionsSelfAlias = boundMentionsSelfAliasPlan,
          Reify.rpContainsForall = containsForallPlan,
          Reify.rpSourceBinderRefs = sourceBinderRefsPlan,
          Reify.rpExternalSourceBinderKeys = externalSourceBinderKeys,
          Reify.rpExternalSourceBinderBaseKeys = externalSourceBinderBaseKeys,
          Reify.rpStructuralSourceBinders = structuralSourceBinders,
          Reify.rpStructuralSourceBaseBinders = structuralSourceBaseBinders,
          Reify.rpInheritedGammaPlan = inheritedGammaPlan
        } = reifyPlan
      reifyRootSource = Reify.rrcSource rootChoice
      substForReify = Reify.rrcSubst rootChoice
      locallyClosedGammaRefs =
        concat
          [ typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId key))
              ("t" ++ show key)
              : [ ref
                | candidateKey <-
                    key
                      : [ getNodeId baseNode
                        | Just baseNode <- [IntMap.lookup key solvedToBasePrefPlan]
                        ]
                , substMap <- [substForReify, sourceBinderRefsPlan]
                , Just ref <- [IntMap.lookup candidateKey substMap]
                ]
          | key <- IntSet.toList locallyClosedGammaKeys
          ]
      allowBoundTraversal =
        allowBoundTraversalFor schemeRootsPlan canonical scopeGen target0
      childrenWithBounds nid =
        case IntMap.lookup (getNodeId nid) nodes of
          Just node@TyVar {tnBound = Just bnd}
            | allowBoundTraversal bnd ->
                structuralChildrenWithBounds node
          Just node -> structuralChildren node
          Nothing -> []
  traceGeneralizeM
    env
    ( "generalizeAt: typeRootPlan useBound="
        ++ show useBoundTypeRoot
        ++ " typeRoot0="
        ++ show typeRoot0
        ++ " typeRoot="
        ++ show typeRoot
        ++ " reifyRoot="
        ++ show reifyRootSource
        ++ " externalSourceBinderKeys="
        ++ show (IntSet.toList externalSourceBinderKeys)
        ++ " externalSourceBinderBaseKeys="
        ++ show (IntSet.toList externalSourceBinderBaseKeys)
        ++ " structuralSourceBinders="
        ++ show structuralSourceBinders
        ++ " structuralSourceBaseBinders="
        ++ show structuralSourceBaseBinders
        ++ " ambientBinderRefs="
        ++ show ambientBinderRefs
    )
  let lookupCanonicalBound nid =
        case VarStore.lookupVarBound constraint (canonical nid) of
          Just bnd
            | Just _ <- NodeAccess.lookupNode constraint (canonical bnd) ->
                Just (canonical bnd)
          _ -> Nothing
      uniqueUnboundedName =
        case [ name
               | (nidInt, binderRef) <- orderedBinderEntries,
                 let name = typeBinderRefName binderRef,
                 Nothing <- [lookupCanonicalBound (NodeId nidInt)]
             ] of
          [nm] -> Just nm
          _ -> Nothing
  let binderSet = IntSet.fromList orderedBinders
      bindingEnv =
        Reify.ReifyBindingEnv
          { Reify.rbeConstraint = constraint,
            Reify.rbeNodes = nodes,
            Reify.rbeCanonical = canonical,
            Reify.rbeRigidBindParents = rigidBindParents,
            Reify.rbeBindParents = bindParents,
            Reify.rbeScopeGen = scopeGen,
            Reify.rbeSchemeRootOwner = schemeRootOwner,
            Reify.rbeSchemeRootByBody = schemeRootByBody,
            Reify.rbeSchemeRootByBodyBase = schemeRootByBodyBase,
            Reify.rbeSchemeRootKeySet = schemeRootKeySet,
            Reify.rbeGammaAlias = gammaAliasPlan,
            Reify.rbeAliasBinderBases = aliasBinderBasesPlan,
            Reify.rbeSolvedToBasePref = solvedToBasePrefPlan,
            Reify.rbeNamedUnderGaSet = namedUnderGaSetPlan,
            Reify.rbeBinderSet = binderSet,
            Reify.rbeUniqueUnboundedName = uniqueUnboundedName,
            Reify.rbeResForReify = resForReify,
            Reify.rbeBindParentsGa = mbBindParentsGaInfo,
            Reify.rbeBindingScopeGen = bindingScopeGenPlan,
            Reify.rbeHasExplicitBound = hasExplicitBoundPlan,
            Reify.rbeIsTargetSchemeBinder = isTargetSchemeBinderPlan,
            Reify.rbeBoundMentionsSelfAlias = boundMentionsSelfAliasPlan,
            Reify.rbeContainsForall = containsForallPlan,
            Reify.rbeFirstGenAncestor = firstGenAncestorGa,
            Reify.rbeTraceM = traceGeneralizeM env
          }
  -- Phase 8: construct per-binder bounds.
  let orderedBinderRefEntries =
        [ ( nidInt,
            case IntMap.lookup nidInt substRefs of
              Just ref -> ref
              Nothing ->
                typeBinderRefFromIdentity
                  (typeBinderIdentityFromNode (canonical (NodeId nidInt)))
                  (typeBinderRefName plannedRef)
          )
        | (nidInt, plannedRef) <- orderedBinderEntries
        ]
  bindings <-
    mapM
      (\(nidInt, ref) -> Reify.bindingFor bindingEnv reifyPlan (ref, nidInt))
      orderedBinderRefEntries
  finalizeBinderPlan <-
    mkFinalizeBinderPlan orderedBinderRefEntries bindings
  let outerBinderRefs = map fst bindings
  reachableType <- Right (reachableFromWithBounds typeRoot)
  selectedRootDomain <- reifyRootDomain resForReify mbBindParentsGa reifyRootSource

  -- Phase 9: scheme ownership and type reification.
  let typeRootC = canonical typeRoot
      Reify.SchemeTypeChoice
        { Reify.stcUseSchemeType = useSchemeTypeAdjusted,
          Reify.stcSchemeOwnerFromBody = schemeOwnerFromBody,
          Reify.stcSchemeOwnerFromBodyIsAlias = schemeOwnerFromBodyIsAlias,
          Reify.stcSchemeOwners = schemeOwners
        } = schemeTypeChoice
      ownersByRoot =
        [ gnId gen
          | gen <- NodeAccess.allGenNodes constraint,
            root <- gnSchemes gen,
            canonical root == typeRootC
        ]
  -- Thesis §15.2.5: rigid quantification is always inlined (no abstractions for rigid nodes).
  let externalKeysForDomain rootDomain =
        case rootDomain of
          LiveReifyRootDomain {} -> externalSourceBinderKeys
          BaseReifyRootDomain {} -> externalSourceBinderBaseKeys
      structuralBindersForDomain rootDomain =
        case rootDomain of
          LiveReifyRootDomain {} -> structuralSourceBinders
          BaseReifyRootDomain {} -> structuralSourceBaseBinders
      reifyTypeWithAliases rootDomain substBase binderPairs =
        if null aliasEntries
          then reifyAndInlineRigid rootDomain substBaseRigid
          else do
            let aliasNodes =
                  IntMap.fromList
                    [ (key, TyVar {tnId = NodeId key, tnBound = Nothing})
                      | (key, _) <- aliasEntries
                    ]
                aliasedDomain = withReifyAliasNodes aliasNodes rootDomain
                substAlias =
                  IntMap.union (IntMap.fromList aliasEntries) substBaseRigid
            ty <-
              reifyTypeInRootDomain
                aliasedDomain
                substAlias
                (externalKeysForDomain aliasedDomain)
                (outerBinderRefsFor substAlias)
                (structuralBindersForDomain aliasedDomain)
            inlineRigidInRootDomain aliasedDomain substAlias ty
        where
          -- Basic setup
          bodyRoot = reifyRootDomainNode rootDomain
          domainCanonical =
            case rootDomain of
              LiveReifyRootDomain liveView _ -> pvCanonical liveView
              BaseReifyRootDomain _ _ -> id
          canonicalKey = getNodeId . domainCanonical
          domainNodes =
            case rootDomain of
              LiveReifyRootDomain _ _ -> nodes
              BaseReifyRootDomain baseConstraint _ ->
                IntMap.fromList
                  [ (getNodeId nid, node)
                    | (nid, node) <- toListNode (cNodes baseConstraint)
                  ]
          domainBindParents =
            case rootDomain of
              LiveReifyRootDomain _ _ -> rigidBindParents
              BaseReifyRootDomain baseConstraint _ -> cBindParents baseConstraint
          domainChildrenWithBounds nid =
            case rootDomain of
              LiveReifyRootDomain _ _ -> childrenWithBounds nid
              BaseReifyRootDomain _ _ ->
                maybe [] structuralChildrenWithBounds (IntMap.lookup (getNodeId nid) domainNodes)
          domainReachableFromWithBounds root =
            case rootDomain of
              LiveReifyRootDomain _ _ -> reachableFromWithBounds root
              BaseReifyRootDomain _ _ ->
                reachableFromStop
                  getNodeId
                  id
                  domainChildrenWithBounds
                  (const False)
                  root
          bodyRootC = domainCanonical bodyRoot
          lookupBound nid =
            let nidC = domainCanonical nid
             in case rootDomain of
                  LiveReifyRootDomain liveView _ ->
                    case VarStore.lookupVarBound (pvCanonicalConstraint liveView) nidC of
                      Just bnd
                        | Just _ <- NodeAccess.lookupNode (pvCanonicalConstraint liveView) (domainCanonical bnd) ->
                            Just (domainCanonical bnd)
                      _ -> Nothing
                  BaseReifyRootDomain baseConstraint _ ->
                    case VarStore.lookupVarBound baseConstraint nidC of
                      Just bnd
                        | Just _ <- NodeAccess.lookupNode baseConstraint bnd -> Just bnd
                      _ -> Nothing

          -- Rigid type handling
          isReachableRigidVar nid =
            case IntMap.lookup (canonicalKey nid) domainNodes of
              Just TyVar {} ->
                let cidKey = canonicalKey nid
                 in IntSet.member cidKey (domainReachableFromWithBounds bodyRoot)
                      && not (isStructuralBinder nid)
              _ -> False

          isStructuralBinder nid =
            IntSet.member (canonicalKey nid) structuralBinderKeys
              || case IntMap.lookup (nodeRefKey (typeRef (domainCanonical nid))) domainBindParents of
                Just (TypeRef parent, _) ->
                  case IntMap.lookup (canonicalKey parent) domainNodes of
                    Just TyForall {} -> True
                    Just TyMu {} -> True
                    _ -> False
                _ -> False

          structuralBinderKeys =
            IntSet.fromList
              [ canonicalKey child
              | children <- IntMap.elems (structuralBindersForDomain rootDomain)
              , child <- children
              ]

          rigidNodeKeys =
            IntSet.toList $
              IntSet.fromList
                [ canonicalKey nid
                  | nid <- IntMapUtils.rigidTypeChildren domainBindParents,
                    isReachableRigidVar nid
                ]

          rigidSubstMap =
            IntMap.fromList
              [ ( key,
                  IntMap.findWithDefault
                    (typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) (rigidNameFor key))
                    key
                    domainInheritedGammaRoutes
                )
                | key <- rigidNodeKeys
              ]

          domainInheritedGammaRoutes =
            case rootDomain of
              LiveReifyRootDomain {} ->
                Reify.inheritedGammaPlanLiveRoutes inheritedGammaPlan
              BaseReifyRootDomain {} ->
                Reify.inheritedGammaPlanBaseRoutes inheritedGammaPlan

          -- A rigid node may still be listed among a scheme root's children,
          -- because that structural ownership is needed for ordering.  Tell
          -- the reifier that its declaration is owned by the subsequent
          -- rigid-inlining step, so it emits occurrences rather than first
          -- wrapping them in a local forall that would shadow the rewrite.
          outerBinderRefsFor substMap =
            [ ref
              | key <- rigidNodeKeys,
                Just ref <- [IntMap.lookup key substMap]
            ]
              ++ outerBinderRefs

          -- Alias handling
          reachableWithoutBound bnd =
            let shouldStop nid = getNodeId nid == getNodeId (domainCanonical bnd)
             in reachableFromStop
                  getNodeId
                  domainCanonical
                  domainChildrenWithBounds
                  shouldStop
                  bodyRoot

          aliasEntries =
            [ (getNodeId (domainCanonical bnd), ref)
              | (b, ref) <- binderPairs,
                Just bnd <- [lookupBound b],
                domainCanonical bnd /= bodyRootC,
                canonicalKey b `IntSet.notMember` reachableWithoutBound bnd
            ]

          substBaseRigid = IntMap.union substBase rigidSubstMap

          -- Main reification logic
          reifyAndInlineRigid domain substMap = do
            ty <-
              reifyTypeInRootDomain
                domain
                substMap
                (externalKeysForDomain domain)
                (outerBinderRefsFor substMap)
                (structuralBindersForDomain domain)
            inlineRigidInRootDomain domain substMap ty

          inlineRigidInRootDomain domain substMap ty
            | null rigidNodeKeys = pure ty
            | otherwise = do
                let computeRigidBound key = do
                      let nid = NodeId key
                          name = rigidNameFor key
                          rigidRef =
                            case IntMap.lookup key substMap of
                              Just substRef -> substRef
                              Nothing ->
                                typeBinderRefFromIdentity (typeBinderIdentityFromNode nid) name
                          fallbackTy =
                            case IntMap.lookup key domainInheritedGammaRoutes of
                              -- A validated inherited Gamma route proves that
                              -- this free rigid occurrence is already bound by
                              -- an enclosing lexical declaration.
                              Just inheritedRef -> TVarRef inheritedRef
                              -- A local rigid variable with no explicit bound
                              -- denotes its unbounded lower endpoint.  Inline
                              -- bottom now; manufacturing a free graph ref here
                              -- would ask finalization to repair missing scope.
                              Nothing -> TBottom
                      case lookupBound nid of
                        Nothing -> pure (rigidRef, fallbackTy)
                        Just bnd -> do
                          case
                              reifyBoundInRootDomain
                                domain
                                substMap
                                (externalKeysForDomain domain)
                                (structuralBindersForDomain domain)
                                (domainCanonical bnd)
                            of
                            Left (MissingNode _) -> pure (rigidRef, fallbackTy)
                            Left err -> Left err
                            Right bndTy -> pure (rigidRef, bndTy)
                rigidBounds <- mapM computeRigidBound rigidNodeKeys
                let rigidMap = Map.fromList rigidBounds
                pure (inlineRigidTypes rigidMap ty)

  let selectedSubstForReify = substForReify
      orderedBinderPairs =
        [ (NodeId key, ref)
          | key <- orderedBinders,
            Just ref <- [IntMap.lookup key substRefs]
        ]
      selectedBinderPairs =
        case selectedRootDomain of
          LiveReifyRootDomain _ _ -> orderedBinderPairs
          BaseReifyRootDomain _ _ ->
            [ (NodeId key, ref)
              | (key, ref) <- IntMap.toList selectedSubstForReify
            ]
      reifyTypeWithOrderedBinders =
        reifyTypeWithAliases
          selectedRootDomain
          selectedSubstForReify
          selectedBinderPairs

  let reifySchemeType
        | useSchemeTypeAdjusted = reifySchemeTypeAdjusted
        | otherwise = reifySchemeTypeExplicit
        where
          -- Adjusted scheme type: use scheme ownership to determine scope
          reifySchemeTypeAdjusted = do
            let reachableVars =
                  [ NodeId nid
                    | nid <- IntSet.toList reachableType,
                      nid /= getNodeId typeRootC,
                      Just TyVar {} <- [IntMap.lookup nid nodes]
                  ]
                hasReachableBinder gid =
                  any (\nid -> firstGenAncestorGa (typeRef nid) == Just gid) reachableVars
                schemeOwnerCandidates = filter hasReachableBinder schemeOwners
                schemeScope = case schemeOwnerFromBody of
                  Just _ | schemeOwnerFromBodyIsAlias, (owner : _) <- ownersByRoot -> genRef owner
                  Just gid -> genRef gid
                  Nothing -> case schemeOwnerCandidates ++ schemeOwners of
                    (gid : _) -> genRef gid
                    [] -> typeRef typeRootC
            if schemeScope == scopeRootC
              then do
                traceGeneralizeM
                  env
                  ( "generalizeAt: schemeScope equals scopeRootC; skipping recursive scheme-type fallback"
                      ++ " scopeRootC="
                      ++ show scopeRootC
                      ++ " typeRootC="
                      ++ show typeRootC
                  )
                reifyTypeWithOrderedBinders
              else do
                traceGeneralizeM
                  env
                  ( "generalizeAt: schemeScope differs from scopeRootC; using direct structural scheme reification"
                      ++ " scopeRootC="
                      ++ show scopeRootC
                      ++ " schemeScope="
                      ++ show schemeScope
                      ++ " typeRootC="
                      ++ show typeRootC
                  )
                reifySchemeTypeExplicit

          -- Explicit scheme type: use structural scheme if available
          reifySchemeTypeExplicit = do
            explicitSchemeTy <- explicitStructuralSchemeType
            case explicitSchemeTy of
              Just ty -> pure ty
              Nothing -> reifyTypeWithOrderedBinders

          explicitStructuralSchemeType
            | null bindings,
              scopeHasStructuralScheme,
              IntMap.null selectedSubstForReify,
              explicitBinders0@(_ : _) <- binders0 =
                case explicitSchemePlan explicitBinders0 of
                  Nothing -> pure Nothing
                  Just (binders, names, substExplicit, explicitBodyDomain) -> do
                    let binderRefs =
                          [ ref
                            | binder <- binders,
                              Just ref <- [IntMap.lookup (getNodeId binder) substExplicit]
                          ]
                    bodyTy <-
                      reifyTypeWithAliases
                        explicitBodyDomain
                        substExplicit
                        (zip binders binderRefs)
                    bounds <- explicitBounds binders names substExplicit
                    orderedScheme <-
                      either
                        (Left . ValidationFailed . pure)
                        Right
                        ( orderSourceProjectedSchemeBinders
                            "explicit structural scheme"
                            (mkElabSchemeWithRefs bounds bodyTy)
                        )
                    pure (Just (schemeToType orderedScheme))
            | otherwise = pure Nothing

          explicitSchemePlan explicitBinders0 =
            let binderKeysList =
                  IntSet.toList $
                    IntSet.fromList
                      [getNodeId (canonical b) | b <- explicitBinders0]
                names = zipWith alphaName [0 ..] binderKeysList
                refs =
                  [ typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name
                    | (key, name) <- zip binderKeysList names
                  ]
             in case binderKeysList of
                  [] -> Nothing
                  _ ->
                    Just
                      ( map NodeId binderKeysList,
                        names,
                        IntMap.fromList (zip binderKeysList refs),
                        case IntMap.lookup (getNodeId typeRootC) nodes of
                          Just TyVar {}
                            | Just bnd <- lookupCanonicalBound typeRootC ->
                                LiveReifyRootDomain resForReify (canonical bnd)
                          _ -> selectedRootDomain
                      )

          explicitBounds binders names substExplicit =
            let lookupBound nid =
                  case VarStore.lookupVarBound constraint (canonical nid) of
                    Just bnd
                      | Just _ <- NodeAccess.lookupNode constraint (canonical bnd) ->
                          Just (canonical bnd)
                    _ -> Nothing
                useConstraintBoundReify =
                  scopeHasStructuralScheme && null bindings
                reifyBoundForExplicit bndRoot
                  | useConstraintBoundReify =
                      reifyBoundWithRefsOnConstraint originalConstraint (graphIdentitySubstRefs id substExplicit) bndRoot
                  | otherwise =
                      reifyBoundWithRefs resForReify (graphIdentitySubstRefs (pvCanonical resForReify) substExplicit) bndRoot
                inlineNamedBounds = inlineNamedBoundsFor substExplicit
                computeBound (b, name) =
                  let ref = typeBinderRefFromIdentity (typeBinderIdentityFromNode b) name
                   in case lookupBound b of
                        Nothing -> pure (ref, Nothing)
                        Just bnd -> do
                          bndTy <- reifyBoundForExplicit (canonical bnd)
                          let bndTy' = inlineNamedBounds bndTy
                              mbBound = case bndTy' of
                                TBottom -> Nothing
                                TVarRef bndRef
                                  | typeBinderRefsSameIdentity bndRef ref -> Nothing
                                TVarRef {} -> Nothing
                                _ -> either (const Nothing) Just (elabToBound bndTy')
                          pure (ref, mbBound)
             in mapM computeBound (zip binders names)

          inlineNamedBoundsFor substExplicit =
            -- See Note [Scope-aware bound/alias inlining] in
            -- docs/notes/2026-01-27-elab-changes.md.
            let useConstraintBoundReify =
                  scopeHasStructuralScheme && null bindings
                reifyBoundForInline bndRoot
                  | useConstraintBoundReify =
                      reifyBoundWithRefsOnConstraint originalConstraint (graphIdentitySubstRefs id substExplicit) bndRoot
                  | otherwise =
                      reifyBoundWithRefs resForReify (graphIdentitySubstRefs (pvCanonical resForReify) substExplicit) bndRoot
             in inlineAliasBoundsWithBy
                  False
                  canonical
                  (NodeMap nodes)
                  (VarStore.lookupVarBound constraint)
                  reifyBoundForInline

  ty0Raw <- reifySchemeType
  finalizeScheme
    FinalizeInput
        { fiEnv = env,
          fiConstraint = constraint,
          fiCanonical = canonical,
          fiBindParents = bindParents,
          fiScopeRootC = scopeRootC,
          fiTypeRoot = typeRoot,
          fiTypeRootC = typeRootC,
          fiScopeGen = scopeGen,
          fiFirstGenAncestorGa = firstGenAncestorGa,
          fiBindParentsGa = mbBindParentsGa,
          fiSolvedToBasePref = solvedToBasePrefPlan,
          fiGammaAlias = gammaAliasPlan,
          fiNamedUnderGaSet = namedUnderGaSetPlan,
          fiRequiredGammaKeys = IntSet.fromList (IntMap.keys requiredGammaPlan),
          fiRootBodyClosureKeys = rootBodyClosureKeys,
          fiRequiredGammaAliases =
            IntMap.mapMaybe
              (\requirement ->
                case rgbOperatedType requirement of
                  TVarRef ref
                    | any
                        (typeBinderRefsSameIdentity ref)
                        requiredGammaAliasAuthorityRefs ->
                        Just ref
                  _ -> Nothing
              )
              requiredGammaPlan,
          fiInheritedGammaPlan = inheritedGammaPlan,
          fiAmbientBinderRefs = ambientBinderRefs,
          fiLocallyClosedGammaRefs = locallyClosedGammaRefs,
          fiTermUsedRootBinderRefs = termUsedRootBinderRefs,
          fiBinderPlan = finalizeBinderPlan,
          fiReifySubst = selectedSubstForReify,
          fiSubst = substRefs,
          fiTyRaw = ty0Raw
        }
