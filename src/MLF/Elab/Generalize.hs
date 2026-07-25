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
    GeneralizedResultRouteRequest (..),
    GeneralizedResultRoute (..),
    sourceLambdaGeneralizedResultRouteRequest,
    certifyGeneralizedResultRoute,
    GammaPacketAuthority (..),
    LocalGammaConstructor (..),
    LocalGammaOwner (..),
    LocalGammaFrame,
    lgfOwner,
    lgfDirectEdgeSources,
    lgfChildren,
    LocalGammaEdgeOwnership (..),
    localGammaDirectApplicationEdgeOwners,
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
    publishTopologyConsumerRoutes,
    publishSourceLambdaTopologyConsumerRoute,
    prepareSubtermGeneralizationPacket,
    prepareRootRaiseMergeScheme,
    prepareRootRaiseMergeSchemeAtEdge,
    requiredGammaBinderForRootRaiseMerge,
    rootRaiseMergeAuthorityFor,
    rootRaiseMergeAuthorityForExpression,
    rootRaiseMergeExteriorIdentityFor,
    rootRaiseMergeExteriorOwnedByScope,
    generalizationRequirementsForRootEdges,
    generalizationRequirementsForRootEdgesInConstruction,
    generalizationRequirementsForEnclosingRootEdges,
    resolveFrozenOperatedOccurrenceEndpoint,
    resolveAmbientGammaOperatedEndpoint,
    subtermGeneralizationSchemeInfo,
    subtermGeneralizationConsumerConstructionSchemeInfo,
    subtermGeneralizationOperatedSchemeInfo,
    subtermGeneralizationCompilerExactBoundary,
    subtermGeneralizationCompilerExactResultRef,
    subtermGeneralizationCompilerExactExistingRef,
    subtermGeneralizationCompilerExactCompletionRef,
    subtermGeneralizationCompilerExactResultStage,
    subtermGeneralizationCompilerExactBinderRenames,
    subtermGeneralizationConstructionBinderRenames,
    subtermGeneralizationInheritedGammaRoutes,
    withInheritedGammaRoutes,
    withConstructionBinderRenames,
    publishSubtermGammaConstructionSourceSchemeInfo,
    withCompilerExactBinderRenames,
    withCompilerExactSourceSubtermResult,
    withCompilerExactPacketSubtermResult,
    withCompilerExactEnclosingSubtermResult,
    subtermGeneralizationConsumerIdentity,
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationGammaAuthority,
    subtermGeneralizationResultAbstractionRef,
    subtermGeneralizationConstructionResultAbstractionRef,
    subtermGeneralizationGammaBoundScheme,
    subtermGeneralizationGammaBoundSchemeForConsumer,
    subtermGeneralizationOperatedSchemeForConsumer,
    subtermGeneralizationLocalConsumerClosure,
    subtermGeneralizationOwnsGammaEdge,
    subtermGeneralizationOwnsGammaForEdge,
    subtermResultOwnershipFor,
    subtermResultOwnershipConsumerClosedLocally,
    subtermResultOwnershipLocalSourceDeclarationRefs,
    subtermResultOwnershipHasTransparentPath,
    subtermResultOwnershipLambdaNode,
    subtermResultOwnershipPacket,
    subtermGeneralizationsOwnedBy,
    shadowCompareTypes,
    selectSolvedOrderWithShadow,
  )
where

import Control.Monad (foldM, guard, unless)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, maybeToList)
import qualified Data.Set as Set
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution (EdgeTrace (..))
import MLF.Constraint.Presolution.Base
  ( EdgeArtifacts (..),
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
      churchAwareEqType,
      freeTypeVarRefsType,
      inlineAliasBoundsWithBy,
      matchTypeRefs,
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
  | FlexibleExteriorEdgeOwnership !LocalGammaOwner
  deriving (Eq, Show)

selectLocalGammaEdgeOwnership
  :: IntMap.IntMap LocalGammaOwner
  -> EdgeId
  -> [LocalGammaOwner]
  -> (LocalGammaOwner -> Bool)
  -> Maybe LocalGammaEdgeOwnership
selectLocalGammaEdgeOwnership directOwners edgeId owners ownsExterior =
  case IntMap.lookup (getEdgeId edgeId) directOwners of
    Just owner ->
      Just (DirectApplicationEdgeOwnership owner)
    Nothing ->
      FlexibleExteriorEdgeOwnership <$> find ownsExterior owners

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
    ALet _ mediatorDetails _ _ _ _ rhs letBody _
      | annotationRefersTo ownerDetails rhs
      , idDetailsIdentityKey mediatorDetails
          /= idDetailsIdentityKey ownerDetails ->
          case letBody of
            ALetScope nested@ALam {} _ _ -> Just nested
            _ -> Nothing
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
    && directEdgeKeys == expectedEdgeKeys
    && directEdgeKeys `IntSet.isSubsetOf` closureEdgeKeys
  where
    edgeKeySet = IntSet.fromList . map getEdgeId
    directEdgeKeys = edgeKeySet (lgcDirectApplicationEdgeIds closure)
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
-- supplies the declaration's checked bound, while the retained closure proves
-- its source occurrence, complete edge group, exterior, and consumer identity.
-- The enclosing construction retains its own required binders unchanged.
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
    ( length matched == length regenerated
        && all (`elem` matched) regenerated
    )
    ( inheritanceFailure
        "regenerated descendant requirements are not in one-to-one correspondence with retained closures"
        Nothing
        Nothing
    )
  foldM inheritOne requirements (zip closures matched)
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

    inheritOne inherited (closure, requirement)
      | rgbPlacement requirement /= RequiredGammaAtCurrentScope =
          inheritanceFailure
            "a descendant declaration was not regenerated at its owning scope"
            (Just closure)
            (Just requirement)
      | not (null conflictingCurrentRequirements) =
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
        conflictingCurrentRequirements =
          [ current
          | current <- grRequiredGammaBinders inherited
          , rgbExteriorNode current == rgbExteriorNode requirement
              || not
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
          == edgeKeySet (rgbEdgeIds requirement)

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
  , psgCopiedBinderRefs :: Map.Map TypeBinderIdentity TypeBinderRef
  , psgGammaAuthority :: Maybe GammaPacketAuthority
  }
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
  , sroPath :: !SubtermResultPath
  , sroPacket :: !PreparedSubtermGeneralization
  }
  deriving (Eq, Show)

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
rootRaiseMergeAuthorityFor artifacts eid@(EdgeId edgeKey) =
  case (IntMap.lookup edgeKey (eaEdgeWitnesses artifacts), IntMap.lookup edgeKey (eaEdgeTraces artifacts)) of
    (Just witness, Just traceInfo) ->
      case validAuthorities traceInfo (getInstanceOps (ewWitness witness)) of
        [] -> Right Nothing
        [authority] -> Right (Just authority)
        authorities ->
          Left
            ( ValidationFailed
                [ "edge carries multiple root RaiseMerge authorities: " ++ show eid,
                  "  authorities: " ++ show authorities
                ]
            )
    _ -> Right Nothing
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
      validated <- validateExteriorBinder authority schemeInfo
      requirement <- requiredGammaBinderForRootRaiseMerge edgeId authority requirements
      constructExteriorResult authority requirement validated
  where
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
              retainedBinders =
                binderDependencyClosure
                  binders
                  ( freeTypeVarRefsType bodyConstructed
                      ++ requiredConstructionRefs
                  )
          pure
            ( schemeInfoFromRefSubst
                (mkElabSchemeWithRefs retainedBinders bodyConstructed)
                (siSubstRefs info)
            )
      where
        exterior = rrmaExterior authority

    isArrowBody ty =
      case ty of
        TArrow {} -> True
        _ -> False

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
          let expectedBound = rgbOperatedType requirement
              actualBound = maybe TBottom tyToElab mbBound
              boundsAgree =
                alphaEqType expectedBound actualBound
                  || churchAwareEqType expectedBound actualBound
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
        CompilerExactEnclosingSubtermResult _ ref -> ref

subtermGeneralizationConsumerIdentity :: PreparedSubtermGeneralization -> Maybe TypeBinderIdentity
subtermGeneralizationConsumerIdentity =
  fmap scaConsumerIdentity . psgConsumerAuthority

subtermGeneralizationConsumerAuthority
  :: PreparedSubtermGeneralization
  -> Maybe SubtermConsumerAuthority
subtermGeneralizationConsumerAuthority = psgConsumerAuthority

-- | The graph-semantic result binder consumed by this subterm. A root
-- RaiseMerge or validated identity-topology consumer supplies its exact
-- identity from graph construction evidence.
subtermGeneralizationResultAbstractionRef :: PreparedSubtermGeneralization -> Maybe TypeBinderRef
subtermGeneralizationResultAbstractionRef packet =
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
    Just CompilerExactEnclosingSubtermResult{} -> Nothing
    Nothing -> Nothing

subtermGeneralizationCompilerExactResultStage
  :: PreparedSubtermGeneralization
  -> Maybe CompilerExactResultStage
subtermGeneralizationCompilerExactResultStage packet =
  case psgCompilerExactResult packet of
    Just CompilerExactSourceSubtermResult{} -> Just CompleteBeforeCompilerExact
    Just CompilerExactPacketSubtermResult{} -> Just CompleteAfterCompilerExact
    Just CompilerExactEnclosingSubtermResult{} -> Nothing
    Nothing -> Nothing

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

withConstructionBinderRenames
  :: [(TypeBinderRef, TypeBinderRef)]
  -> PreparedSubtermGeneralization
  -> PreparedSubtermGeneralization
withConstructionBinderRenames renames packet =
  packet
    { psgConstructionBinderRenames = renames
    , psgInheritedGammaRoutes =
        renameInheritedGammaRoutes
          renames
          (psgInheritedGammaRoutes packet)
    }

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
        ( schemeInfoFromRefSubst
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
  pure
    packet
      { psgSchemeInfo = schemeInfo
      , psgConsumerConstructionSchemeInfo = consumerConstructionSchemeInfo
      , psgOperatedSchemeInfo = operatedSchemeInfo
      , psgGammaBoundScheme = gammaBoundScheme
      , psgCompilerExactBinderRenames = renames
      , psgInheritedGammaRoutes =
          renameInheritedGammaRoutes
            renames
            (psgInheritedGammaRoutes packet)
      }
  where
    consumerIdentity = scaConsumerIdentity <$> psgConsumerAuthority packet

    specializeConsumerScheme schemeInfo = do
      binders <- traverse specializeConsumerBinder (schemeBinderRefs scheme)
      pure
        ( schemeInfoFromRefSubst
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
            ]
        )
  where
    matchingPackets =
      [ packet
      | packet <- Map.elems packets
      , subtermGeneralizationConsumerIdentity packet == Just consumerIdentity
      ]

-- | Derive the exact scheme that an enclosing Gamma bound will copy.  Packet
-- preparation uses the same constructor to allocate copied binder identities,
-- so dependencies pulled from the construction scheme cannot appear later
-- without a corresponding fresh identity.
enclosingOperatedSchemeFrom :: ElabScheme -> ElabScheme -> ElabScheme
enclosingOperatedSchemeFrom constructionScheme operatedScheme =
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
generalizationRequirementsForRootEdges =
  generalizationRequirementsForRootEdgesAt PacketLocalRequirementBoundary

-- | Build the obligations for a construction that may be running inside one
-- prepared packet.  Only that active packet has its quantifiers open in the
-- construction Gamma.  Every other matching packet is enclosed by this
-- construction and must contribute its closed operated scheme.  Making the
-- choice per edge prevents an enclosing application from interpreting a free
-- packet-local result variable through the solved graph, where its bound may
-- already have been erased to bottom.
generalizationRequirementsForRootEdgesInConstruction
  :: Maybe TypeBinderIdentity
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
generalizationRequirementsForRootEdgesInConstruction activeConsumerIdentity ambientBinderRefs ambientGammaAuthorities identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  requirements <-
    generalizationRequirementsForRootEdgesAt
      ( ConstructionRequirementBoundary
          activeConsumerIdentity
          ambientGammaAuthorities
      )
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
generalizationRequirementsForEnclosingRootEdges =
  generalizationRequirementsForRootEdgesAt EnclosingRequirementBoundary

data RequirementSchemeBoundary
  = PacketLocalRequirementBoundary
  | EnclosingRequirementBoundary
  | ConstructionRequirementBoundary
      (Maybe TypeBinderIdentity)
      (IntMap.IntMap AmbientGammaAuthority)

-- | Prove that a checked exact endpoint is a proper instantiation of a
-- completed descendant packet.  Packet selection remains the caller's
-- responsibility: it must first match the consumer identity and lexical
-- construction owner.  This helper validates only the type-level step.
--
-- The first path is the paper's @N@ construction for explicit bounds.  The
-- second infers non-bound arguments from the packet body and replays them
-- through the same instantiation evaluator.  A replay that merely maps every
-- packet binder back to itself is not a specialization; preserving that
-- distinction keeps a body-only exact edge from stealing a source-owned
-- forall.
packetTypeSpecializesToExactEndpoint :: ElabType -> ElabType -> Bool
packetTypeSpecializesToExactEndpoint packetTy exactEndpoint =
  case splitForallsRefs packetTy of
    ([], _) -> False
    (leadingBinders, packetBody) ->
      boundEliminationMatches leadingBinders packetBody
        || inferredInstantiationMatches leadingBinders packetBody
  where
    boundEliminationMatches leadingBinders packetBody =
      dischargesPacketBinder leadingBinders packetBody
        && case
          foldM
            (\ty _ -> applyInstantiation ty InstElim)
            packetTy
            leadingBinders
          of
            Right specialized -> alphaEqType specialized exactEndpoint
            Left _ -> False

    inferredInstantiationMatches leadingBinders packetBody =
      case matchTypeRefs (map fst leadingBinders) packetBody exactEndpoint of
        Left _ -> False
        Right arguments ->
          hasProperArgument leadingBinders arguments
            && case
              foldM
                (applyLeadingArgument arguments)
                packetTy
                leadingBinders
              of
                Right specialized -> alphaEqType specialized exactEndpoint
                Left _ -> False

    applyLeadingArgument arguments ty (ref, _) =
      applyInstantiation
        ty
        (maybe InstElim InstApp (Map.lookup ref arguments))

    dischargesPacketBinder leadingBinders packetBody =
      any
        ( \(ref, _) ->
            refOccursFree ref packetBody
              && not (refOccursFree ref exactEndpoint)
        )
        leadingBinders

    hasProperArgument leadingBinders arguments =
      any
        ( \(ref, _) ->
            case Map.lookup ref arguments of
              Just (TVarRef argumentRef) ->
                not (typeBinderRefsSameIdentity ref argumentRef)
              Just _ -> True
              Nothing -> False
        )
        leadingBinders

    refOccursFree ref =
      any (typeBinderRefsSameIdentity ref) . freeTypeVarRefsType

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
  -> (NodeId -> NodeId)
  -> (NodeId -> NodeId)
  -> GaBindParents p
  -> PresolutionView p
  -> EdgeArtifacts
  -> IntMap.IntMap TypeBinderRef
  -> SubtermGeneralizations
  -> [(EdgeId, Maybe ElabType)]
  -> Either ElabError GeneralizationRequirements
generalizationRequirementsForRootEdgesAt boundary identityRepresentative constructionCanonical ga presolutionView edgeArtifacts sourceBinderRefs subtermPackets edges = do
  requirements <- traverse requirementForEdge edges
  requiredBinders <- foldM insertRequirement [] (concat requirements)
  pure
    GeneralizationRequirements
      { grRequiredGammaBinders = requiredBinders,
        grSourceBinderRefs = sourceBinderRefs,
        grAmbientBinderRefs = [],
        grAmbientGammaAuthorities =
          case boundary of
            ConstructionRequirementBoundary _ authorities -> authorities
            _ -> IntMap.empty,
        grLocallyClosedGammaNodes = IntSet.empty
      }
  where
    requirementForEdge (edgeId, exactOperatedType) = do
      authority <- rootRaiseMergeAuthorityFor edgeArtifacts edgeId
      case authority of
        Nothing -> pure []
        Just rootAuthority -> do
          selectedPacket <-
            subtermGeneralizationForConsumer
              (typeBinderIdentityFromNode (rrmaExterior rootAuthority))
              subtermPackets
          let packetOperatedScheme =
                packetRequirementScheme rootAuthority <$> selectedPacket
          exactRequirementType <-
            traverse
              (exactOperatedRequirementType rootAuthority packetOperatedScheme)
              exactOperatedType
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
              ( ConstructionRequirementBoundary activeConsumerIdentity _
                , Just exactType
                , _
                )
                  | activeConsumerIdentity
                      == Just (typeBinderIdentityFromNode (rrmaExterior rootAuthority)) ->
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
              ( case selectedExactRequirementType of
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
                      _ ->
                        resolveConstructionSourceBindersInPacketAtExpected
                          identityRepresentative
                          sourceBinderRefs
                          exactType
                          operatedTypeRaw
                  Nothing ->
                    resolveConstructionSourceBindersInTypeExcept
                      ( closedPacketDeclarationIdentities
                          rootAuthority
                          packetOperatedScheme
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
              operatedType =
                inlineBoundVarsTypeWithCanonicalExcept
                  ( maybe
                      []
                      freeTypeVarRefsType
                      selectedExactRequirementType
                      ++ constructionSourceRefs
                  )
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
              schemeInPacketConstructionDomain
                packet
                (siScheme (psgOperatedSchemeInfo packet))
            EnclosingRequirementBoundary ->
              subtermGeneralizationGammaBoundScheme packet
            ConstructionRequirementBoundary activeConsumerIdentity _
              | activeConsumerIdentity
                  == Just (typeBinderIdentityFromNode (rrmaExterior rootAuthority)) ->
                  schemeInPacketConstructionDomain
                    packet
                    (siScheme (psgOperatedSchemeInfo packet))
              | otherwise ->
                  schemeInPacketConstructionDomain
                    packet
                    (subtermGeneralizationGammaBoundScheme packet)

        constructionAmbientRequirementType rootAuthority =
          case boundary of
            ConstructionRequirementBoundary activeConsumerIdentity authorities
              | activeConsumerIdentity
                  /= Just
                    ( typeBinderIdentityFromNode
                        (rrmaExterior rootAuthority)
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

        -- A closed packet publishes S'(operated), whereas a compiler-exact
        -- lambda boundary records the source expression type.  When that
        -- source endpoint is the very lexical variable carried by the frozen
        -- operated root, the required Gamma bound is the variable's bound,
        -- not the variable itself.  The source sidecar is the construction
        -- proof connecting those two identity domains; reifying the operated
        -- root supplies the bound without guessing from a spelling.
        exactOperatedRequirementType rootAuthority packetOperatedScheme exactType =
          case exactType of
            TVarRef exactRef
              | usesClosedOperatedScheme rootAuthority
              , not (packetAlreadyPublishesExactEndpoint packetOperatedScheme exactType)
              , Just sourceRef <- sourceRefForNode (rrmaOperatedRoot rootAuthority)
              , typeBinderRefsSameIdentity exactRef sourceRef ->
                  reifyFrozenOperatedSourceBound
                    exactRef
                    (rrmaOperatedRoot rootAuthority)
            _ -> pure exactType

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
            ConstructionRequirementBoundary activeConsumerIdentity _ ->
              activeConsumerIdentity
                /= Just (typeBinderIdentityFromNode (rrmaExterior rootAuthority))

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
          | TBottom <- rgbOperatedType prior ->
              -- @exterior >= bottom@ contributes no lower-bound information.
              -- When another edge for the same exterior carries an exact
              -- non-bottom producer, retain that edge's operated-root
              -- provenance together with its stronger bound.
              pure (map (replaceMatchingExterior requirement) existing)
          | TBottom <- rgbOperatedType requirement ->
              pure (map (mergeMatchingExterior requirement) existing)
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "instantiation edges require incompatible bounds for one Γ exterior",
                      "  exterior: " ++ show (rgbExteriorNode requirement),
                      "  first bound: " ++ show (rgbOperatedType prior),
                      "  second bound: " ++ show (rgbOperatedType requirement)
                    ]
                )

    sameExterior left right =
      rgbExteriorNode left == rgbExteriorNode right

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
  let (consumerAuthority, gammaAuthority) =
        packetPlacementAuthority placement
   in prepareSubtermGeneralizationWithConsumer
        generator
        consumerAuthority
        gammaAuthority
        packet
        operatedPacket

packetPlacementAuthority
  :: SubtermPacketPlacement
  -> ( Maybe SubtermConsumerAuthority
     , Maybe GammaPacketAuthority
     )
packetPlacementAuthority placement =
  case placement of
    EnclosingConsumerPacket consumerIdentity consumerEdge owner ->
      ( Just
          ( EnclosingGammaConsumerAuthority
              consumerEdge
              consumerIdentity
              owner
          )
      , Nothing
      )
    EnclosingConsumerGammaPacket consumerIdentity consumerEdge owner gammaAuthority ->
      ( Just
          ( EnclosingGammaConsumerAuthority
              consumerEdge
              consumerIdentity
              owner
          )
      , Just gammaAuthority
      )
    TopologyConsumerPacket topologyAuthority ->
      (Just (TopologyConsumerAuthority topologyAuthority), Nothing)
    TopologyConsumerGammaPacket topologyAuthority gammaAuthority ->
      ( Just (TopologyConsumerAuthority topologyAuthority)
      , Just gammaAuthority
      )
    RootConsumerPacket consumerIdentity consumerEdge ->
      ( Just
          ( RootGammaConsumerAuthority
              consumerEdge
              consumerIdentity
          )
      , Nothing
      )
    RootConsumerGammaPacket consumerIdentity consumerEdge gammaAuthority ->
      ( Just
          ( RootGammaConsumerAuthority
              consumerEdge
              consumerIdentity
          )
      , Just gammaAuthority
      )
    GammaPacket gammaAuthority ->
      ( Just
          ( PacketGammaConsumerAuthority
              (gpaEdgeId gammaAuthority)
              (gpaConsumerIdentity gammaAuthority)
          )
      , Just gammaAuthority
      )
    DirectPacket ->
      (Nothing, Nothing)

prepareSubtermGeneralizationWithConsumer
  :: IdentityGenerator
  -> Maybe SubtermConsumerAuthority
  -> Maybe GammaPacketAuthority
  -> SchemeInfo
  -> SchemeInfo
  -> Either ElabError (PreparedSubtermGeneralization, IdentityGenerator)
prepareSubtermGeneralizationWithConsumer generator consumerAuthority gammaAuthority packet operatedPacket = do
  packetWithPendingConsumer <-
    deferPacketConsumerBounds consumerAuthority gammaAuthority packet
  (copiedRefs, generator') <-
    foldM allocateCopy (Map.empty, generatorAfterPacket) packetBinderRefs
  pure
    ( PreparedSubtermGeneralization
        { psgConsumerAuthority = consumerAuthority
        , psgSchemeInfo = packet
        , psgConsumerConstructionSchemeInfo = packetWithPendingConsumer
        , psgOperatedSchemeInfo = operatedPacket
        , psgGammaBoundScheme = gammaBoundScheme
        , psgCompilerExactResult = Nothing
        , psgCompilerExactBinderRenames = []
        , psgConstructionBinderRenames = []
        , psgInheritedGammaRoutes = Reify.emptyInheritedGammaRoutes
        , psgCopiedBinderRefs = copiedRefs
        , psgGammaAuthority = gammaAuthority
        }
    , generator'
    )
  where
    consumerIdentity = scaConsumerIdentity <$> consumerAuthority
    gammaBoundScheme =
      enclosingOperatedSchemeFrom
        (siScheme packet)
        (siScheme operatedPacket)
    packetTy =
      schemeToType gammaBoundScheme
    constructionTy = schemeToType (siScheme packet)
    packetBinderRefs =
      case consumerIdentity of
        Nothing -> []
        -- Recursive data types reuse their structural self/result identities
        -- at every occurrence (for example both arguments of @List a -> List
        -- a@).  Copy that semantic identity once and reuse it at each lexical
        -- occurrence; allocating repeatedly would merely overwrite the map
        -- entry and make the chosen fresh identity traversal-dependent.
        Just _ -> uniqueBinderRefs (boundRefsInType packetTy)
    generatorAfterPacket =
      advanceIdentityGeneratorPastMany
        (generatedIdentitiesInType constructionTy ++ generatedIdentitiesInType packetTy)
        generator

    allocateCopy (copies, nextGenerator) ref =
      let (copyRef, nextGenerator') =
            freshTypeBinderRef (typeBinderRefName ref) nextGenerator
       in Right
            ( Map.insert (typeBinderRefIdentity ref) copyRef copies
            , nextGenerator'
            )

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
          | subtermConsumerAuthorityIsTopology authority
          , let identity = scaConsumerIdentity authority
          , maybe True ((/= identity) . gpaConsumerIdentity) mbGammaAuthority ->
              deferConsumer
                identity
                (topologyConsumerRef identity gammaDeferred)
                gammaDeferred
        _ -> pure gammaDeferred

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
            ( schemeInfoFromRefSubst
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
            ( schemeInfoFromRefSubst
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

    -- Identity-topology authority is already routed through the packet's
    -- substitution.  Preserve that exact construction declaration while its
    -- bound is pending; unlike the Gamma peer-variable rule, no operated-type
    -- shape is needed to justify the route.
    topologyConsumerRef pendingIdentity schemeInfo =
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

-- | Recover the lexical path to the already-bottom-up packet that changes an
-- expression's result type.  The terminal constructor is always the lambda
-- that owns the packet.  Every traversed frame is recorded explicitly so a
-- caller can distinguish an immediate lambda from a lambda exposed through a
-- let or annotation without inspecting a reified type after the fact.
subtermResultOwnershipFor
  :: AnnExpr
  -> SubtermGeneralizations
  -> Maybe SubtermResultOwnership
subtermResultOwnershipFor = go TransparentSubtermResultPath
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
                  , sroPath = path
                  , sroPacket = packet
                  }
            -- A parent prepares the complete packet for a direct nested
            -- lambda. Crossing that lambda makes the remaining path opaque.
            Nothing -> go OpaqueSubtermResultPath body packets
          where
            ownerKey = idDetailsIdentityKey details
        ALet _ _ _ _ _ _ _ body _ -> go path body packets
        AExactAnn inner _ _ _ -> go path inner packets
        AAnn inner _ _ -> go path inner packets
        ALetScope inner _ _ -> go path inner packets
        AUnfold inner _ _ -> go OpaqueSubtermResultPath inner packets
        _ -> Nothing

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
      schemeInfoFromRefSubst
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
  close IntSet.empty [preferredNode]
  where
    preferredNode =
      gaConstructionRouteNode canonical ga node

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
      IntMap.findWithDefault
        []
        (getNodeId current)
        successorsByBaseClass

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

-- | Construction-side question asked by a source lambda whose body returns
-- one exact application result.  The request contains only producer-owned
-- graph facts.  Binder selection remains the responsibility of the
-- generalization planner.
data GeneralizedResultRouteRequest = GeneralizedResultRouteRequest
  { grrrOwnerTarget :: !NodeId
  , grrrFrozenConsumer :: !NodeId
  , grrrConstructionRoot :: !NodeId
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
      Just
        SourceLambdaTopologyBoundary
          { sltbBodyEdge = bodyEdge
          , sltbScopeRoot = scopeRoot
          , sltbBodyRoot = sourceExpressionNode body
          , sltbConstructionRouteSites =
              sourceResultConstructionRouteSites
                (sourceExpressionNode body)
                body
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
          case sourceLambdaTopologyBoundary sourceAnn of
            Just present -> pure present
            Nothing ->
              requestFailure
                "owned topology packet is not at a source-lambda boundary"
                ["  authority: " ++ show topologyAuthority]
        validateSourceLambdaTopologyBoundary
          sourceAnn
          topologyAuthority
          boundary
        case sltbConstructionRouteSites boundary of
          [] -> pure Nothing
          [site] -> do
            constructionRoot <-
              applicationResultConstructionRoot
                ga
                site
            pure
              ( Just
                  GeneralizedResultRouteRequest
                    { grrrOwnerTarget =
                        lgoTermNode (itcaOwner topologyAuthority)
                    , grrrFrozenConsumer =
                        itcaFrozenResultRoot topologyAuthority
                    , grrrConstructionRoot = constructionRoot
                    }
              )
          sites ->
            requestFailure
              "source lambda returns multiple exact application results"
              ["  sites: " ++ show sites]
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
applicationResultConstructionRoot ga site = do
  construction <-
    requireArtifact
      "graft construction"
      ( IntMap.lookup
          edgeKey
          (cGraftResultConstructions (gaBaseConstraint ga))
      )
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
  case grcSourceResultRoot construction of
    Nothing ->
      routeFailure
        "graft construction has no source-bound result authority"
        [ "  construction: " ++ show construction
        ]
    Just _ -> do
      let constructionRoot = grcConstructionCodomain construction
      case lookupNodeIn (cNodes (gaBaseConstraint ga)) constructionRoot of
        Just _ -> pure constructionRoot
        Nothing ->
          routeFailure
            "graft construction result is absent from the prepared graph"
            [ "  construction: " ++ show construction
            , "  result root: " ++ show constructionRoot
            ]
  where
    siteEdge = instantiationSiteEdgeId site
    edgeKey = getEdgeId siteEdge

    requireArtifact
      :: String
      -> Maybe a
      -> Either ElabError a
    requireArtifact label =
      maybe
        ( routeFailure
            ("missing exact application " ++ label)
            ["  edge: " ++ show siteEdge]
        )
        pure

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
        case sourceLambdaTopologyBoundary sourceAnn of
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
      | (binderRef, _) <- schemeBinderRefs (siScheme schemeInfo)
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
                Just consumerNode ->
                  publishConsumerRoute packet consumerNode routes
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
  placePackets
    rootScheme
    [ packet
    | packet <- Map.elems subtermPackets
    , not (subtermGeneralizationOwnsGammaEdge packet)
    ]
  where
    placePackets scheme [] = Right scheme
    placePackets scheme (packet : rest) = do
      scheme' <- placePacket scheme packet
      placePackets scheme' rest

    placePacket scheme0 packet =
      case subtermGeneralizationConsumerIdentity packet of
        Nothing -> Right scheme0
        Just consumerIdentity ->
          case targetRefs consumerIdentity of
            [] ->
              Left
                ( InstantiationError
                    ( unlines
                        [ "prepared subterm scheme has no enclosing binder consumer"
                        , "  consumer identity: " ++ show consumerIdentity
                        , "  enclosing binders: " ++ show binders
                        , "  packet scheme: " ++ show (siScheme (psgSchemeInfo packet))
                        , "  packet Gamma authority: " ++ show (psgGammaAuthority packet)
                        , "  packet consumer authority: " ++ show (psgConsumerAuthority packet)
                        , "  enclosing consumer routes: " ++ show consumerRoutes
                        , "  packet compiler-exact routes: "
                            ++ show (subtermGeneralizationCompilerExactBinderRenames packet)
                        , "  packet construction routes: "
                            ++ show (subtermGeneralizationConstructionBinderRenames packet)
                        ]
                    )
                )
            [targetRef] -> do
              validatePacketFreeRefSourceRoutes targetRef
              validatePacketFreeRefs targetRef
              targetBound <-
                case find (typeBinderRefsSameIdentity targetRef . fst) binders of
                  Just (_, Just existingBound) -> Right existingBound
                  Just (_, Nothing)
                    | Just authority <- psgConsumerAuthority packet
                    , subtermConsumerAuthorityIsTopology authority ->
                        case elabToBound packetTyUnfreshened of
                          Right preparedBound -> Right preparedBound
                          Left cause ->
                            Left
                              ( ValidationFailed
                                  [ "identity-topology S'(operated) is not a legal prepared construction bound"
                                  , "  consumer: " ++ show targetRef
                                  , "  prepared bound: " ++ show packetTyUnfreshened
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
              placedTargetBound <-
                case packetBoundMatch targetRef packetOwnedTargetBound (tyToElab targetBound) of
                  Just PacketBodyBound ->
                    -- The enclosing consumer already owns the exact
                    -- S'(operated) bound.  A body-only match proves that the
                    -- packet's outer binders may be removed, but it does not
                    -- authorize wrapping that exact bound in the packet's
                    -- copied forall spine.
                    pure targetBound
                  Just WholePacketBound -> do
                    packetTy <-
                      freshenPacketBoundRefs
                        packetReservedNames
                        packetTyUnfreshened
                    case elabToBound packetTy of
                      Right packetBound -> pure packetBound
                      Left err ->
                        Left
                          ( ValidationFailed
                              [ "prepared subterm scheme is not a legal Γ bound"
                              , "  consumer: " ++ show targetRef
                              , "  packet type: " ++ show packetTy
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
                    | (ref, mbBound) <- binders
                    , typeBinderRefsSameIdentity ref targetRef
                        || not (isPacketOwned ref)
                    ]
                  placed = mkElabSchemeWithRefs retained (schemeBody scheme)
              pure placed
            _ ->
              Left
                ( InstantiationError
                    "prepared subterm scheme is consumed by multiple enclosing binders"
                )
      where
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
          | alphaEqType constructionTargetBound packetTyUnfreshened =
              Just WholePacketBound
          | otherwise = Nothing

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
              [ ref
              | (ref, _) <- binders
              , not (typeBinderRefsSameIdentity ref targetRef)
              , isPacketOwned ref
              ]
            removedPacketConstructionRefs =
              removedPacketRefs
                ++ [ constructionRef
                   | removedRef <- removedPacketRefs
                   , (sourceRef, constructionRef) <-
                       subtermGeneralizationConstructionBinderRenames packet
                   , typeBinderRefsSameIdentity removedRef sourceRef
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
          case Map.lookup (typeBinderRefIdentity ref) (psgCopiedBinderRefs packet) of
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
        binders = schemeBinderRefs scheme
        isPacketOwned ref =
          any (typeBinderRefsSameIdentity ref) packetOwnedRefs
        -- A root RaiseMerge packet can bind the same exterior identity that
        -- consumes it at the enclosing boundary.  The packet's copy is
        -- freshened above, so identity equality here selects the enclosing
        -- construction binder; treating it as merely packet-owned would lose
        -- the only paper-authorized Gamma entry.
        targetRefs = targetRefsIn binders
        targetRefsIn candidateBinders consumerIdentity =
          foldr insertDistinctRef [] (directTargets ++ routedTargets)
          where
            directTargets =
              [ ref
              | (ref, mbBound) <- candidateBinders
              , isJust mbBound || packetHasTopologyConsumer
              , typeBinderRefIdentity ref == consumerIdentity
              ]
            routedTargets =
              [ ref
              | consumerNode <- maybeToList (typeBinderIdentityNode consumerIdentity)
              , routedRef <-
                  maybeToList
                    (IntMap.lookup (getNodeId consumerNode) consumerRoutes)
              , (ref, mbBound) <- candidateBinders
              , isJust mbBound || packetHasTopologyConsumer
              , typeBinderRefsSameIdentity ref routedRef
              ]
            insertDistinctRef ref refs
              | any (typeBinderRefsSameIdentity ref) refs = refs
              | otherwise = ref : refs

        -- A topology packet is allowed to identify an unbounded candidate so
        -- placement can report a missing checked construction bound precisely.
        -- It never authorizes manufacturing that bound from the packet type.
        packetHasTopologyConsumer =
          case psgConsumerAuthority packet of
            Just TopologyConsumerAuthority{} -> True
            _ -> False

        validatePacketFreeRefs targetRef =
          case unexpectedPacketFreeRefs targetRef of
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
                               , "  packet operated scheme: "
                                   ++ show (siScheme (psgOperatedSchemeInfo packet))
                               , "  enclosing scheme: " ++ show scheme
                               , "  enclosing binder identities: "
                                   ++ show (map (typeBinderRefIdentity . fst) binders)
                               ]
                    )
                )

        unexpectedPacketFreeRefs targetRef =
          filter (not . isAllowedPacketFreeRef targetRef) packetFreeRefs

        isAllowedPacketFreeRef targetRef packetRef =
          any (typeBinderRefsSameIdentity packetRef) (bindersBefore targetRef)
            || any (typeBinderRefsSameIdentity packetRef) enclosingFreeRefs

        bindersBefore targetRef =
          map fst
            ( takeWhile
                (not . typeBinderRefsSameIdentity targetRef . fst)
                binders
            )

        -- A reference that is free in the whole enclosing scheme but also has
        -- a later binder in that scheme is not an outer lexical reference: it
        -- is an out-of-scope forward reference and must not be admitted into a
        -- newly installed bound.
        enclosingFreeRefs =
          filter
            (\freeRef -> not (any (typeBinderRefsSameIdentity freeRef . fst) binders))
            (freeTypeVarRefsType (schemeToType scheme))

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
  -> ElabScheme
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError GeneralizedResultRoute
certifyGeneralizedResultRoute request plan scheme finalizedSubst = do
  unless
    (grrrOwnerTarget request == target0)
    ( certificateFailure
        "request owner does not match the generalized target"
        [ "  request owner: " ++ show (grrrOwnerTarget request)
        , "  generalized target: " ++ show target0
        ]
    )
  unless
    ( IntSet.member
        (getNodeId (grrrConstructionRoot request))
        (reachableFromWithBounds typeRoot)
    )
    ( certificateFailure
        "application construction root is outside the generalized type root"
        [ "  type root: " ++ show typeRoot
        , "  construction root: "
            ++ show (grrrConstructionRoot request)
        ]
    )
  (binderKey, plannedRef) <-
    case candidateBinders of
      [] ->
        certificateFailure
          "application construction root has no planned result binder"
          [ "  construction root: "
              ++ show (grrrConstructionRoot request)
          , "  ordered binders: " ++ show orderedBinders
          ]
      first@(_, firstRef) : remaining
        | all
            (typeBinderRefsSameIdentity firstRef . snd)
            remaining ->
            pure first
        | otherwise ->
            certificateFailure
              "application construction root has ambiguous planned result binders"
              [ "  construction root: "
                  ++ show (grrrConstructionRoot request)
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
        (schemeBinderRefs scheme)
    )
    ( certificateFailure
        "finalized result route is absent from the outer scheme binder spine"
        [ "  result route: " ++ show finalizedRef
        , "  scheme binders: " ++ show (schemeBinderRefs scheme)
        ]
    )
  pure
    GeneralizedResultRoute
      { grrOwnerTarget = grrrOwnerTarget request
      , grrTypeRoot = typeRoot
      , grrFrozenConsumer = grrrFrozenConsumer request
      , grrConstructionRoot = grrrConstructionRoot request
      , grrBinderNode = NodeId binderKey
      , grrBinderRef = finalizedRef
      }
  where
    GeneralizePlan
      { gpContext = GeneralizeCtx {gcTarget0 = target0}
      , gpTypeRootPlan = TypeRootPlan {trTypeRoot = typeRoot}
      , gpBinderPlan =
          BinderPlan
            { bpOrderedBinders = orderedBinders
            , bpBinderRefRoutes = binderRefRoutes
            }
      , gpReachableFromWithBounds = reachableFromWithBounds
      , gpReachableFromStructural = reachableFromStructural
      } = plan

    constructionReachable =
      reachableFromStructural (grrrConstructionRoot request)

    candidateBinders =
      [ (binderKey, plannedRef)
      | (binderKey, plannedRef) <- orderedBinders
      , IntSet.member binderKey constructionReachable
      ]

    certificateFailure
      :: String
      -> [String]
      -> Either ElabError a
    certificateFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid generalized application-result certificate"
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
          bpLocallyClosedGammaKeys = locallyClosedGammaKeys,
          bpGammaAlias = gammaAliasPlan,
          bpNamedUnderGaSet = namedUnderGaSetPlan,
          bpSolvedToBasePref = solvedToBasePrefPlan,
          bpAliasBinderBases = aliasBinderBasesPlan,
          bpRequiredGamma = requiredGammaPlan,
          bpSourceBinderRefs = binderSourceBinderRefs,
          bpAmbientBinderRefs = ambientBinderRefs
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
          fiBinderPlan = finalizeBinderPlan,
          fiReifySubst = selectedSubstForReify,
          fiSubst = substRefs,
          fiTyRaw = ty0Raw
        }
