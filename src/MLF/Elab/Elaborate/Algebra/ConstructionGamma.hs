{-# LANGUAGE GADTs #-}

module MLF.Elab.Elaborate.Algebra.ConstructionGamma
  ( FrozenEndpointCertificate (..),
    FrozenEndpointCertificates,
    ConstructionBinderBoundProvenance (..),
    DirectAmbientGammaAuthorityProvenance (..),
    PendingLocalResultSourcePacket,
    NestedApplicationResidualCertificate,
    LambdaParamBoundaryAuthority (..),
    LambdaParamBoundaryInstallation (..),
    LambdaParamBoundaryCertificate,
    lambdaParamBoundaryConstructedType,
    lambdaParamBoundarySourceBinderRefs,
    completeSchemeInfoRouteType,
    completeLambdaParamBoundarySchemeInfo,
    completeLambdaParamBoundarySchemeInfoInScope,
    completeLambdaParamBoundaryBound,
    completeLambdaParamBoundarySourceRootBound,
    constructLambdaParamBoundaryRequirement,
    completeLambdaParamBoundaryDeclarationBound,
    completeLambdaParamBoundaryDeclarationBoundInScope,
    completeLambdaParamBoundaryTypeInScope,
    completeLambdaParamBoundaryType,
    renameLambdaParamBoundaryCertificate,
    OrdinaryGammaPacketConstruction (..),
    ValidatedBodyConsumerProjection,
    BodyConsumerDeclarationAuthority,
    BodyConsumerBoundRefinementCertificate,
    CertifiedPacketConsumerBodyProjection,
    certifyPacketConsumerBodyProjection,
    attachCertifiedPacketConsumerBodyProjection,
    certifiedPacketConsumerBodyProjectionSourceRef,
    certifiedPacketConsumerBodyProjectionSourceType,
    certifiedPacketConsumerBodyProjectionTargetRef,
    certifiedPacketConsumerBodyProjectionTargetType,
    bodyConsumerBoundRefinementCertifiesTransition,
    bodyConsumerBoundRefinementsJointlyCertifyTransition,
    bodyConsumerBoundRefinementsAuthorizeDeclarationCompletion,
    bodyConsumerBoundRefinementAppliesToDeclarationState,
    bodyConsumerBoundRefinementTopologyResultRefAtConstruction,
    bodyConsumerBoundRefinementCompletedTopologyEndpoint,
    bodyConsumerBoundRefinementsCompletePacketBound,
    bodyConsumerBoundRefinementEmittedBy,
    bodyConsumerBoundRefinementCompletesOwnerEndpoint,
    bodyConsumerBoundRefinementCompletesExactEndpoint,
    bodyConsumerBoundRefinementCompletesSchemeDeclaration,
    bodyConsumerBoundRefinementFinalizedLocalRef,
    bodyConsumerDeclarationConstructsCompletion,
    bodyConsumerBoundRefinementHasSiblingCompletionAt,
    bodyConsumerBoundRefinementRequiresSchemeCompletion,
    bodyConsumerBoundRefinementRequiresOwnerEmission,
    bodyConsumerBoundRefinementExcludesAmbientRef,
    finalizeBodyConsumerBoundRefinementAtOwner,
    bodyConsumerBoundRefinementSurvivesOwnerBoundary,
    bodyConsumerBoundRefinementConsumesAny,
    bodyConsumerBoundRefinementConsumedDependencies,
    bodyConsumerBoundRefinementConsumedReplayRoutes,
    bodyConsumerBoundRefinementTargetsAny,
    bodyConsumerBoundRefinementHasSemanticRouteWithin,
    bodyConsumerBoundRefinementCompletesPreparedPacket,
    completeLambdaEndpointFromBodyConsumerRefinement,
    completeSelectedFreeLambdaEndpointFromBodyConsumerRefinement,
    completeConsumedResultOwnerEndpointFromBodyConsumerRefinement,
    completePreparedLambdaEndpointFromBodyConsumerRefinement,
    materializeLocalTopologyResultBound,
    certifyAmbientTopologyResultBoundRefinement,
    certifyLocalPacketBodyConsumerBoundRefinement,
    certifyAmbientPacketGammaConsumerBoundRefinement,
    certifyEnclosingPacketBodyConsumerBoundRefinement,
    CertifiedGammaBoundTransition,
    certifyGammaBoundTransition,
    certifyBodyConsumerProjectionGammaBoundTransition,
    certifiedBodyConsumerGammaBoundTransition,
    renameCertifiedGammaBoundTransition,
    advanceBodyConsumerBoundRefinementThroughCertifiedGammaBound,
    advanceBodyConsumerBoundRefinementsThroughCertifiedGammaBound,
    advanceBodyConsumerBoundRefinementThroughCheckedOwnerResultCompletion,
    advanceBodyConsumerBoundRefinementThroughOwnerEndpointCompletion,
    advanceBodyConsumerBoundRefinementsThroughValidatedLocalGamma,
    installBodyConsumerBoundRefinements,
    installBodyConsumerConstructionRoutes,
    installDescendantBodyConsumerBoundRefinements,
    installOwnedBodyConsumerBoundRefinements,
    installOwnedBodyConsumerBoundRefinementsWithClosures,
    completeOwnedBodyConsumerRequirementEndpoints,
    inheritOwnedBodyConsumerBoundRefinements,
    authorizeBodyConsumerDeclaration,
    authorizeBodyConsumerDeclarationWithValidatedLocalRequirements,
    authorizedBodyConsumerDeclarationBound,
    authorizedBodyConsumerRoute,
    bodyConsumerBoundRefinementRoute,
    buildAmbientGammaAuthorities,
    constructOrdinaryGammaPacket,
    certifiedSourcePacketOperatedOccurrenceRenames,
    certifiedSourcePacketOccurrenceRenames,
    selectTermSourcePacketOccurrenceRenames,
    certifiedSourceOccurrenceRenames,
    certifiedSourceOccurrenceRoutes,
    constructionProtectedIdentities,
    constructionRouteBoundCompatible,
    directSourceBinderConstructionRename,
    constructionRefAlreadyInGamma,
    inheritNestedApplicationResidualAuthority,
    inheritNestedApplicationResidualReplayAuthority,
    inferExactTransportArguments,
    installLambdaParamBoundary,
    lambdaBodyConstructionRenames,
    lambdaParamConstructionRenames,
    lambdaParamLocalGammaRenames,
    lambdaParamProtectedIdentities,
    lambdaParamBoundaryProtectedIdentities,
    lambdaParamBoundaryOccurrenceRenames,
    lambdaParamBoundaryConstructionRenames,
    lambdaParamBoundaryRecoveryRenames,
    protectedBoundaryOccurrenceRenames,
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
    forallClosurePresentsSameIdentityBody,
    exactIdentityForallClosureOf,
    CertifiedLambdaBodyConstruction,
    CertifiedPacketConsumerBinder,
    certifiedPacketConsumerBinderRef,
    certifyLambdaBodyConstruction,
    certifyPacketConsumerBinderFromBodyConstruction,
    CertifiedPacketSourceConsumerBinder,
    certifiedPacketSourceConsumerBinderRef,
    certifyPacketSourceConsumerBinder,
    CertifiedOpenValueLambdaParameterClosure,
    certifyOpenValueLambdaParameterClosure,
    certifyOpenValueLambdaParameterClosureAtBinders,
    ExactLambdaConstructionPlan,
    exactLambdaConstructionBinders,
    exactLambdaConstructionPublishedBinders,
    exactLambdaConstructionPublishedType,
    exactLambdaConstructionBinderRenames,
    exactLambdaConstructionParameterBinderCopies,
    exactLambdaConstructionBodyBinderRenames,
    exactLambdaConstructionResultBinderCopies,
    exactLambdaConstructionBodyAbstractions,
    exactLambdaConstructionBodyInstantiation,
    exactLambdaConstructionBodyType,
    exactLambdaConstructionPreservedBodyRefinements,
    exactLambdaConstructionAmbientBodyRefinement,
    exactLambdaConstructionIntroducedAmbientBodyDeclaration,
    exactLambdaConstructionAmbientBodyRefinementCertificate,
    exactLambdaConstructionCompletionInstantiation,
    exactLambdaConstructionCompletionPreservesBinderIdentities,
    certifyExactLambdaConstruction,
    certifyExactLambdaConstructionWithEndpointPlan,
    certifyExactLambdaEndpointConstruction,
    certifyExactLambdaEndpointConstructionWithCopies,
    exactLambdaEndpointTypesAgree,
    operationalEndpointTypesAgree,
    mkValidatedBodyConsumerProjection,
    validatedBodyConsumerProjectionSourceConstructionRenames,
    validatedBodyConsumerProjectionType,
    validatedBodyConsumerLeadingElimination,
    validatedBodyConsumerProjectionSpecialization,
    projectValidatedAmbientConsumerBound,
    projectValidatedAmbientConsumerBoundWithCertificate,
    consumeCertifiedBodyConsumerConstructionBindings,
    consumeCertifiedBodyConsumerConstructionScheme,
    consumeCertifiedBodyConsumerEndpointScheme,
    consumeCertifiedBodyConsumerConstructionType,
    advanceCertifiedBodyConsumerConstructionBindingsToScheme,
    projectCertifiedBodyConsumerRootScheme,
    projectCertifiedBodyConsumerRootBounds,
    completeCertifiedSourceOpenBodyConsumerBounds,
    completePreparedBodyConsumerConstructionBounds,
    projectCertifiedBodyConsumerBoundsIfPresent,
    alphaRenameBodyConsumerBoundRefinementCertificate,
    alignBodyConsumerBoundRefinementScopeDependencies,
    renameBodyConsumerBoundRefinementScopeDependencies,
    renameBodyConsumerBoundRefinementCertificate,
    publishFrozenEndpointCertificate,
    bodyConsumerProjectionProvenance,
    bodyConsumerRouteProjectionProvenance,
    directAmbientGammaAuthorityProvenance,
    selectLocalGammaClosureOwnerLane,
    selectDirectAmbientGammaAuthority,
    selectBoundaryConstructionRoute,
    selectDirectLocalApplicationArgumentTopology,
    sourceBinderAuthorityRefs,
    frozenEndpointCertificateTypes,
    transparentResultResolvedByEnclosingScheme,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find, nub)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph
  ( EdgeId,
    NodeId (..),
    genRef,
    getEdgeId,
    getNodeId,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( AmbientGammaAuthority (..),
    GeneralizationRequirements (..),
    RequiredGammaBinder (..),
    RequiredGammaPlacement (..),
    requiredGammaPlacementIsLocal,
  )
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Elab.Generalize
  ( GammaPacketAuthority (..),
    LocalGammaClosure (..),
    LocalGammaConstructor (..),
    LocalGammaOwner,
    PreparedSubtermGeneralization,
    SubtermConsumerAuthority,
    lgoBoundaryEdge,
    lgoConstructor,
    lgoTermNode,
    lgoScope,
    scaConsumerIdentity,
    scaEdgeId,
    subtermConsumerAuthorityEnclosingOwner,
    subtermConsumerAuthorityIsTopology,
    subtermGeneralizationCompilerExactBinderRenames,
    subtermGeneralizationCopiedBinderRoutes,
    subtermGeneralizationConstructionBinderRenames,
    subtermGeneralizationConstructionResultAbstractionRef,
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationConsumerConstructionSchemeInfo,
    subtermGeneralizationExactConsumerSpecialization,
    subtermGeneralizationSourceOwnerConsumerCompletion,
    subtermGeneralizationGammaAuthority,
    subtermGeneralizationGammaBoundScheme,
    subtermGeneralizationAdministrativeLambdaResultConstruction,
    subtermGeneralizationInheritedGammaRoutes,
    subtermGeneralizationLocalResultAuthority,
    subtermGeneralizationLocalConsumerClosure,
    subtermGeneralizationOperatedSchemeInfo,
    subtermGeneralizationOpaqueResultConstruction,
    subtermGeneralizationOpaqueResultSourceLambdaCompletion,
    subtermGeneralizationResultAbstractionRef,
    subtermGeneralizationSchemeInfo,
  )
import MLF.Elab.Elaborate.Algebra.BodyConsumerRoute
  ( BodyConsumerRoute (..),
  )
import MLF.Elab.Inst
  ( applyInstantiation,
    composeInst,
    renameInstBoundRef,
    schemeToType,
  )
import MLF.Elab.Run.Instantiation
  ( constructExactInstantiation,
    constructExactInstantiationAtSourceArguments,
    constructLexicalForallCopyInstantiation,
    exactBinderSpineInstantiation,
    exactBinderSpineRenames,
    inferInstAppArgsFromSchemeRefsExact,
    planExactBinderSpine,
  )
import MLF.Elab.SourceBinder
  ( orderSourceProjectedSchemeBinders,
    sourceBinderConstructionRenamesRetainingAmbiguousSources,
    typeBinderDeclarationRefs,
  )
import MLF.Elab.TermClosure
  ( alphaRenameTypeBinderScopes,
    renameBoundTypeBinderRefPayloads,
    renameTypeBinderRefPayloads,
  )
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Run.Generalize.Types
  ( LocalGammaConstructionCertificate (..),
    localGammaConstructionCertificateResidualType,
  )
import MLF.Elab.Types
  ( BoundType,
    ElabError (..),
    ElabScheme,
    ElabType,
    Instantiation (..),
    SchemeInfo (..),
    Ty (..),
    UniqueIdentity,
    identityGeneratorAfterType,
    mapBoundType,
    mkElabSchemeWithRefs,
    rebuildSchemeInfoFromRefSubst,
    schemeInfoFromRefSubst,
    schemeBinderRefs,
    schemeBody,
    schemeFromType,
    schemeInfoBinderRefSubst,
    elabToBound,
    tyToElab,
  )
import MLF.Reify.TypeOps
  ( alphaEqType,
    alphaEqTypePreservingRecursiveBinders,
    churchAwareEqType,
    churchRepresentationEqType,
    composeTypeHeadRef,
    freeTypeVarRefsType,
    matchTypeRefs,
    splitForallsRefs,
    substTypeCaptureRef,
  )
import MLF.Types.Elab
  ( TypeBinderIdentity,
    TypeBinderRef,
    typeBinderIdentityFromNode,
    typeBinderIdentityNode,
    typeBinderRefFromIdentity,
    typeBinderRefGraphOrigin,
    typeBinderRefIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    freshenTypeBinderRef,
  )
import MLF.Types.Identity
  ( IdentityGenerator,
    StructuralTypeBinderRole (StructuralSelfBinder),
    typeBinderIdentityGeneratedUnique,
    typeBinderIdentityIsCanonicalStructural,
    typeBinderIdentityStableName,
    typeBinderIdentityStructural,
  )

-- | One checked sibling-edge destination in the frozen witness domain.
-- Several lexical application boundaries may use the same graph result root
-- with different local Gamma consumers, so the provider edge remains part of
-- the certificate.
data FrozenEndpointCertificate = FrozenEndpointCertificate
  { fecProducerEdge :: !EdgeId
  , fecResultRoot :: !NodeId
  , fecEndpointType :: !ElabType
  }
  deriving (Eq, Show)

-- | Frozen providers at each graph result root, nearest lexical provider
-- first.  A nested application's direct argument sibling shadows inherited
-- providers for its function child without deleting their provenance.
type FrozenEndpointCertificates =
  IntMap.IntMap (NonEmpty.NonEmpty FrozenEndpointCertificate)

publishFrozenEndpointCertificate
  :: FrozenEndpointCertificate
  -> FrozenEndpointCertificates
  -> FrozenEndpointCertificates
publishFrozenEndpointCertificate certificate =
  IntMap.insertWith
    (<>)
    (getNodeId (fecResultRoot certificate))
    (certificate NonEmpty.:| [])

frozenEndpointCertificateTypes
  :: FrozenEndpointCertificates
  -> IntMap.IntMap ElabType
frozenEndpointCertificateTypes =
  IntMap.map (fecEndpointType . NonEmpty.head)

-- | Keep only graph routes whose outward references are binders emitted by
-- this exact constructor.  Every graph-backed forall is also the declaration
-- for its own frozen node, even when generalization did not need a separate
-- substitution entry for it.
localGammaConstructionRoutes
  :: String
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [IntMap.IntMap TypeBinderRef]
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
localGammaConstructionRoutes role binders routeMaps =
  fst
    <$> localGammaConstructionProvenance
      role
      binders
      routeMaps
      IntMap.empty

-- | Separate graph ownership from source dependency authority before a local
-- Gamma certificate is constructed.  A graph-backed binder normally
-- contributes its own node route.  When an exact supplied route delegates
-- that node to another emitted binder, the original binder is instead a
-- source dependency and must be independently present in the source sidecar;
-- one graph node can never name both declarations.
localGammaConstructionProvenance
  :: String
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [IntMap.IntMap TypeBinderRef]
  -> IntMap.IntMap TypeBinderRef
  -> Either
      ElabError
      (IntMap.IntMap TypeBinderRef, IntMap.IntMap TypeBinderRef)
localGammaConstructionProvenance role binders graphRouteMaps sourceBinderRefs = do
  routes <-
    foldM
      insertRoute
      IntMap.empty
      ( binderDeclarationRoutes
          ++ suppliedBinderRoutes
      )
  let graphRouted ref =
        any
          (typeBinderRefsSameIdentity ref)
          (IntMap.elems routes)
      sourceOnlyBinders =
        [ ref
        | (ref, _) <- binders
        , not (graphRouted ref)
        ]
      sourceAuthorities =
        IntMap.filter
          ( \sourceRef ->
              any
                (typeBinderRefsSameIdentity sourceRef)
                sourceOnlyBinders
          )
          sourceBinderRefs
      sourceAuthorized ref =
        any
          (typeBinderRefsSameIdentity ref)
          (IntMap.elems sourceAuthorities)
      missingBinders =
        [ ref
        | ref <- sourceOnlyBinders
        , not (sourceAuthorized ref)
        ]
  if null missingBinders
    then pure (routes, sourceAuthorities)
    else
      Left
        ( ValidationFailed
            [ "local Gamma construction has no authority for an emitted binder",
              "  role: " ++ role,
              "  emitted binders: " ++ show binders,
              "  missing binders: " ++ show missingBinders,
              "  candidate graph routes: " ++ show graphRouteMaps,
              "  source binder sidecar: " ++ show sourceBinderRefs
            ]
        )
  where
    binderDeclarationRoutes =
      [ (getNodeId node, ref)
      | (ref, _) <- binders
      , Just node <- [typeBinderRefNode ref]
      , IntSet.notMember
          (getNodeId node)
          delegatedDeclarationNodeKeys
      ]

    suppliedBinderRoutes =
      [ (nodeKey, routedRef)
      | routeMap <- graphRouteMaps
      , (nodeKey, routedRef) <- IntMap.toList routeMap
      , any (typeBinderRefsSameIdentity routedRef . fst) binders
      ]

    delegatedDeclarationNodeKeys =
      IntSet.fromList
        [ nodeKey
        | (nodeKey, routedRef) <- suppliedBinderRoutes
        , directRef <-
            [ ref
            | (ref, _) <- binders
            , typeBinderRefNode ref == Just (NodeId nodeKey)
            ]
        , not
            (typeBinderRefsSameIdentity directRef routedRef)
        ]

    insertRoute routes (nodeKey, routedRef) =
      case IntMap.lookup nodeKey routes of
        Nothing -> pure (IntMap.insert nodeKey routedRef routes)
        Just existingRef
          | typeBinderRefsSameIdentity existingRef routedRef -> pure routes
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "local Gamma construction has conflicting routes for one graph node",
                      "  role: " ++ role,
                      "  graph node: " ++ show (NodeId nodeKey),
                      "  first binder: " ++ show existingRef,
                      "  second binder: " ++ show routedRef,
                      "  candidate graph routes: " ++ show graphRouteMaps
                    ]
                )

-- | Why one construction surface carries a bound for a binder identity.
-- Figure 15.3.5 makes the recursively checked child the authority for the
-- local Gamma declaration.  A source annotation and an enclosing exact
-- endpoint constrain how that declaration is entered or consumed, but they
-- do not redefine its bound.
data ConstructionBinderBoundProvenance
  = ConstructionLocalGammaBound
  | ConstructionSourceAnnotationEndpoint
  | ConstructionExactEndpoint
  deriving (Eq, Show)

-- | Equality at an operational xMLF endpoint.  Church encodings may differ
-- only in the representation of a recursive type, but explicit outer
-- foralls and flexible bounds remain part of the endpoint ABI.  In
-- particular, 'churchAwareEqType' deliberately refuses to erase a forall
-- around a complete recursive type.
operationalEndpointTypesAgree :: ElabType -> ElabType -> Bool
operationalEndpointTypesAgree left right =
  alphaEqType left right || churchRepresentationEqType left right

-- | Equality for the exact lambda endpoint reconstructed from a compiler
-- packet.  A source recursive type can bind its structural self locally while
-- the packet retains the same structural self as an outer source declaration.
-- Normalize only that owner/role-certified alias back to the corresponding
-- local mu binder, then reuse the ordinary operational equality.  This keeps
-- the narrow Church-representation rule: unlike 'churchAwareEqType', it
-- cannot identify two different forall-preserving result algebras merely
-- because their result variables are matchable.
exactLambdaEndpointTypesAgree :: ElabType -> ElabType -> Bool
exactLambdaEndpointTypesAgree left right =
  operationalEndpointTypesAgree left right
    || case normalizeStructuralSelfAliasPair left right of
      Just (normalizedLeft, normalizedRight) ->
        operationalEndpointTypesAgree normalizedLeft normalizedRight
      Nothing -> False

normalizeStructuralSelfAliasPair
  :: ElabType
  -> ElabType
  -> Maybe (ElabType, ElabType)
normalizeStructuralSelfAliasPair left right =
  case (left, right) of
    (TVarRef _, TVarRef _) -> pure (left, right)
    (TArrow leftDomain leftCodomain, TArrow rightDomain rightCodomain) -> do
      (leftDomain', rightDomain') <-
        normalizeStructuralSelfAliasPair leftDomain rightDomain
      (leftCodomain', rightCodomain') <-
        normalizeStructuralSelfAliasPair leftCodomain rightCodomain
      pure
        ( TArrow leftDomain' leftCodomain'
        , TArrow rightDomain' rightCodomain'
        )
    ( TConWithIdentity leftIdentity leftBase leftArgs
      , TConWithIdentity rightIdentity rightBase rightArgs
      ) -> do
        (leftArgs', rightArgs') <-
          normalizeStructuralSelfAliasArgs leftArgs rightArgs
        pure
          ( TConWithIdentity leftIdentity leftBase leftArgs'
          , TConWithIdentity rightIdentity rightBase rightArgs'
          )
    (TVarAppRef leftRef leftArgs, TVarAppRef rightRef rightArgs) -> do
      (leftArgs', rightArgs') <-
        normalizeStructuralSelfAliasArgs leftArgs rightArgs
      pure
        ( TVarAppRef leftRef leftArgs'
        , TVarAppRef rightRef rightArgs'
        )
    (TBaseWithIdentity {}, TBaseWithIdentity {}) -> pure (left, right)
    (TBottom, TBottom) -> pure (TBottom, TBottom)
    ( TForallRef leftRef leftBound leftBody
      , TForallRef rightRef rightBound rightBody
      ) -> do
        (leftBound', rightBound') <-
          normalizeStructuralSelfAliasBounds leftBound rightBound
        (leftBody', rightBody') <-
          normalizeStructuralSelfAliasPair leftBody rightBody
        pure
          ( TForallRef leftRef leftBound' leftBody'
          , TForallRef rightRef rightBound' rightBody'
          )
    (TMuRef leftRef leftBody, TMuRef rightRef rightBody) -> do
      let mbOwner =
            commonStructuralSelfOwner leftRef rightRef
          leftBodyAtSelf =
            maybe
              leftBody
              (\owner -> normalizeStructuralSelfAliases owner leftRef leftBody)
              mbOwner
          rightBodyAtSelf =
            maybe
              rightBody
              (\owner -> normalizeStructuralSelfAliases owner rightRef rightBody)
              mbOwner
          (leftBody', rightBody') =
            fromMaybe
              (leftBodyAtSelf, rightBodyAtSelf)
              ( normalizeStructuralSelfAliasPair
                  leftBodyAtSelf
                  rightBodyAtSelf
              )
      pure
        ( TMuRef leftRef leftBody'
        , TMuRef rightRef rightBody'
        )
    _ -> Nothing

normalizeStructuralSelfAliasArgs
  :: NonEmpty.NonEmpty ElabType
  -> NonEmpty.NonEmpty ElabType
  -> Maybe
      ( NonEmpty.NonEmpty ElabType
      , NonEmpty.NonEmpty ElabType
      )
normalizeStructuralSelfAliasArgs leftArgs rightArgs = do
  guard (NonEmpty.length leftArgs == NonEmpty.length rightArgs)
  normalized <-
    traverse
      (uncurry normalizeStructuralSelfAliasPair)
      (zip (NonEmpty.toList leftArgs) (NonEmpty.toList rightArgs))
  pure
    ( NonEmpty.fromList (map fst normalized)
    , NonEmpty.fromList (map snd normalized)
    )

normalizeStructuralSelfAliasBounds
  :: Maybe BoundType
  -> Maybe BoundType
  -> Maybe (Maybe BoundType, Maybe BoundType)
normalizeStructuralSelfAliasBounds Nothing Nothing =
  pure (Nothing, Nothing)
normalizeStructuralSelfAliasBounds (Just leftBound) (Just rightBound) = do
  (leftBoundTy, rightBoundTy) <-
    normalizeStructuralSelfAliasPair
      (tyToElab leftBound)
      (tyToElab rightBound)
  leftBound' <- either (const Nothing) Just (elabToBound leftBoundTy)
  rightBound' <- either (const Nothing) Just (elabToBound rightBoundTy)
  pure (Just leftBound', Just rightBound')
normalizeStructuralSelfAliasBounds _ _ = Nothing

commonStructuralSelfOwner
  :: TypeBinderRef
  -> TypeBinderRef
  -> Maybe UniqueIdentity
commonStructuralSelfOwner leftRef rightRef =
  case
      ( structuralSelfOwner leftRef
      , structuralSelfOwner rightRef
      )
    of
      (Just leftOwner, Just rightOwner)
        | leftOwner == rightOwner -> Just leftOwner
      (Just owner, Nothing) -> Just owner
      (Nothing, Just owner) -> Just owner
      _ -> Nothing

structuralSelfOwner :: TypeBinderRef -> Maybe UniqueIdentity
structuralSelfOwner ref =
  case typeBinderIdentityStructural (typeBinderRefIdentity ref) of
    Just (owner, StructuralSelfBinder) -> Just owner
    _ -> Nothing

normalizeStructuralSelfAliases
  :: UniqueIdentity
  -> TypeBinderRef
  -> ElabType
  -> ElabType
normalizeStructuralSelfAliases owner localRef body =
  foldl
    ( \current aliasRef ->
        substTypeCaptureRef aliasRef (TVarRef localRef) current
    )
    body
    [ aliasRef
    | aliasRef <- freeTypeVarRefsType body
    , typeBinderIdentityStructural (typeBinderRefIdentity aliasRef)
        == Just (owner, StructuralSelfBinder)
    ]

-- | A checked local declaration may be more general than the operated
-- endpoint selected for one consumer.  It constructs that endpoint only when
-- exact source-scheme inference consumes its complete unbounded forall spine.
-- This is deliberately separate from operational endpoint equality: the
-- quantifiers remain part of the declaration, and the returned application
-- arguments are the construction evidence that specializes them.
completeUnboundedForallSpecializesTo :: ElabType -> ElabType -> Bool
completeUnboundedForallSpecializesTo source endpoint =
  not (null binders)
    && all (isNothing . snd) binders
    && case
      inferInstAppArgsFromSchemeRefsExact binders body endpoint
    of
      Just arguments -> length arguments == length binders
      Nothing -> False
  where
    sourceScheme = schemeFromType source
    binders = schemeBinderRefs sourceScheme
    body = schemeBody sourceScheme

-- | Whether opening the first type's complete leading forall spine exposes
-- the second type's exact identity-bearing body.  The closure may order the
-- same identities according to graph dependencies, while the checked endpoint
-- retains source lexical order.  This does not make the two types equal: it is
-- evidence only for replacing a provisional owner declaration with the
-- checked endpoint that constructs it.
forallClosurePresentsSameIdentityBody :: ElabType -> ElabType -> Bool
forallClosurePresentsSameIdentityBody closure endpoint =
  not (null closureBinderRefs)
    && all (`refOccursIn` endpointCarrierRefs) closureBinderRefs
    && all (`refOccursIn` closureBinderRefs) endpointBinderRefs
    && operationalEndpointTypesAgree
      (schemeBody closureScheme)
      (schemeBody endpointScheme)
  where
    closureScheme = schemeFromType closure
    endpointScheme = schemeFromType endpoint
    closureBinderRefs = map fst (schemeBinderRefs closureScheme)
    endpointBinderRefs = map fst (schemeBinderRefs endpointScheme)
    endpointCarrierRefs =
      endpointBinderRefs
        ++ freeTypeVarRefsType (schemeBody endpointScheme)
    refOccursIn ref =
      any (typeBinderRefsSameIdentity ref)

-- | Whether the first type is the exact universal closure of the second over
-- one or more of its free identities.  This is not an instantiation oracle:
-- it only peels an unbounded prefix and requires the remaining type to be the
-- recorded endpoint, with every peeled identity occurring free there.
--
-- Owner-final publication uses this relation after closing the free
-- dependencies of a descendant Gamma completion.  The closed binder and the
-- corresponding free dependency intentionally have the same stable identity;
-- asking the ordinary binder-spine planner to instantiate that binder would
-- reject the apparent self-reference because it has no declaration-authority
-- context.  Here the enclosing declaration certificate supplies that
-- authority, while this predicate supplies the exact structural closure
-- evidence.
exactUnboundedForallClosureOf :: ElabType -> ElabType -> Bool
exactUnboundedForallClosureOf closed endpoint =
  peel [] closed
  where
    endpointFreeRefs = freeTypeVarRefsType endpoint

    peel closedRefs current
      | not (null closedRefs)
      , operationalEndpointTypesAgree current endpoint =
          all
            ( \closedRef ->
                any
                  (typeBinderRefsSameIdentity closedRef)
                  endpointFreeRefs
            )
            closedRefs
      | TForallRef ref Nothing body <- current =
          peel (ref : closedRefs) body
      | otherwise = False

-- | Whether a checked source constructor has generalized an exact result
-- position of an earlier endpoint.  This is the type-level trace of commuting
-- a type abstraction through an already fixed value-lambda prefix: domains
-- must remain identical, and the changed result must be the exact unbounded
-- forall closure of the earlier result with the same binder identities.
--
-- The relation is intentionally covariant and one-way.  Later projection may
-- use it only through a private certificate created while the exact owner held
-- both declaration states; it is never an endpoint equality or authority that
-- root construction may infer from a final type.
exactResultForallClosureOf :: ElabType -> ElabType -> Bool
exactResultForallClosureOf closed endpoint =
  exactUnboundedForallClosureOf closed endpoint
    || case (closed, endpoint) of
      (TArrow closedDomain closedResult, TArrow endpointDomain endpointResult) ->
        operationalEndpointTypesAgree closedDomain endpointDomain
          && exactResultForallClosureOf closedResult endpointResult
      _ -> False

-- | Whether the first type is the exact universal closure of the second over
-- one or more free identities, including bounded declarations.  This is a
-- structural relation, not an instantiation rule: a bounded binder cannot be
-- applied to its own escaping identity.  Callers may use it only alongside a
-- positive construction certificate which proves that the enclosing term
-- emitted this exact forall spine before publishing the closed endpoint.
exactIdentityForallClosureOf :: ElabType -> ElabType -> Bool
exactIdentityForallClosureOf closed endpoint =
  peel [] closed
  where
    endpointFreeRefs = freeTypeVarRefsType endpoint

    peel closedRefs current
      | not (null closedRefs)
      , operationalEndpointTypesAgree current endpoint =
          all
            ( \closedRef ->
                any
                  (typeBinderRefsSameIdentity closedRef)
                  endpointFreeRefs
            )
            closedRefs
      | TForallRef ref _ body <- current =
          peel (ref : closedRefs) body
      | otherwise = False

-- | A recursively checked lambda body whose complete leading binder spine is
-- backed by its owner-final construction certificate.  The constructor is
-- private: callers can obtain this evidence only when the supplied spine is
-- exactly the leading declaration spine of the supplied checked endpoint.
-- The complete spine can be empty: that is the positive zero-step evidence
-- that a child owner constructed an exact monomorphic endpoint, rather than
-- an absence of construction evidence.
--
-- This small view keeps 'OwnerFinalConstruction' in its owning algebra module
-- while giving the exact lambda planner the positive evidence needed for the
-- paper's commuting construction
--
--   @\x. (Lambda a. e)  ==>  Lambda a. \x. (e [a])@.
--
-- In particular, the planner never rediscovers these binders from the final
-- body type: the caller must project the already certified owner spine and
-- this constructor verifies that projection before making it usable.
data CertifiedLambdaBodyConstruction =
  CertifiedLambdaBodyConstruction
    { certifiedLambdaBodyOwner :: !LocalGammaOwner
    , certifiedLambdaBodyConstructedType :: !ElabType
    , certifiedLambdaBodyConstructedBinders ::
        ![(TypeBinderRef, Maybe BoundType)]
    , certifiedLambdaBodyConsumedBinders ::
        ![(TypeBinderRef, Maybe BoundType)]
    , certifiedLambdaBodySourceRefs :: ![TypeBinderRef]
    , certifiedLambdaBodyParameterRefs :: ![TypeBinderRef]
    , -- | Parameter boundary installed by the recursively checked body owner
      -- when that owner is itself the returned value lambda.  This is kept
      -- separate from descendant returned-result boundaries: an enclosing
      -- lambda needs the immediate child's constructor evidence as well as
      -- the transitive returned-value chain.
      certifiedLambdaBodyOwnLambdaParameterBoundary ::
        !(Maybe LambdaParamBoundaryCertificate)
    , certifiedLambdaBodyAmbientDeclarations ::
        ![AmbientGammaAuthority]
    , certifiedLambdaBodyScopeDependencyRenames ::
        ![(TypeBinderRef, TypeBinderRef)]
    , certifiedLambdaBodyReturnedResults ::
        ![(LocalGammaOwner, ElabType)]
    , -- | Exact parameter boundaries paired with the returned value-lambda
      -- constructor that installed them.  The association is transported by
      -- the returned-result construction chain; it is not reconstructed by
      -- matching a completed arrow domain to a same-shaped certificate.
      certifiedLambdaBodyReturnedLambdaParameterBoundaries ::
        ![( LocalGammaOwner
          , LambdaParamBoundaryCertificate
          , ElabType
          )]
    }
  deriving (Eq, Show)

-- | Proof that a prepared packet has closed the exact type identity of a
-- checked value-lambda parameter while the recursively constructed body is
-- already inside that parameter's lexical scope.  The prepared declaration
-- supplies the forall, and the owner-final certificate supplies the lambda
-- constructor that introduced every captured identity.  The two endpoints
-- may have different result constructions: an xMLF computation cannot move
-- beneath an arrow, and the recursively checked body is the authority for
-- that result.  Therefore this certificate records only the exact parameter
-- identities whose matching lambda boundaries were verified on both sides.
newtype CertifiedOpenValueLambdaParameterClosure =
  CertifiedOpenValueLambdaParameterClosure [TypeBinderRef]
  deriving (Eq, Show)

certifyLambdaBodyConstruction
  :: LocalGammaOwner
  -> ElabType
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> Maybe LambdaParamBoundaryCertificate
  -> [AmbientGammaAuthority]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(LocalGammaOwner, ElabType)]
  -> [(LocalGammaOwner, LambdaParamBoundaryCertificate, ElabType)]
  -> Maybe CertifiedLambdaBodyConstruction
certifyLambdaBodyConstruction bodyOwner constructedTy certifiedBinders consumedBinders sourceRefs parameterRefs ownLambdaParameterBoundary ambientDeclarations scopeDependencyRenames returnedResults returnedLambdaParameterBoundaries = do
  guard (length certifiedBinders == length constructedBinders)
  guard (and (zipWith bindersAgree certifiedBinders constructedBinders))
  guard (distinctBinderIdentities certifiedBinders)
  guard (distinctBinderIdentities consumedBinders)
  guard
    ( allDistinctBy
        typeBinderRefsSameIdentity
        sourceRefs
    )
  guard
    ( allDistinctBy
        typeBinderRefsSameIdentity
        parameterRefs
    )
  guard
    ( maybe
        True
        ownLambdaParameterBoundaryAgrees
        ownLambdaParameterBoundary
    )
  guard
    ( allDistinctBy
        (\left right ->
            typeBinderRefsSameIdentity
              (agaExactRef left)
              (agaExactRef right)
        )
        ambientDeclarations
    )
  guard
    ( allDistinctBy
        (\(leftSource, leftTarget) (rightSource, rightTarget) ->
            typeBinderRefsSameIdentity leftSource rightSource
              && typeBinderRefsSameIdentity leftTarget rightTarget
        )
        scopeDependencyRenames
    )
  guard
    ( all
        ( \sourceRef ->
            any
              (typeBinderRefsSameIdentity sourceRef)
              constructedOccurrenceRefs
        )
        sourceRefs
    )
  guard
    ( all
        ( \(consumedRef, _) ->
            not
              ( any
                  (typeBinderRefsSameIdentity consumedRef)
                  constructedRefs
              )
        )
        consumedBinders
    )
  guard
    ( all
        ( \parameterRef ->
            any
              (typeBinderRefsSameIdentity parameterRef)
              (freeTypeVarRefsType constructedTy)
        )
        parameterRefs
    )
  guard
    ( allDistinctBy
        (\(leftOwner, _) (rightOwner, _) -> leftOwner == rightOwner)
        returnedResults
    )
  guard
    ( allDistinctBy
        ( \(leftOwner, _, _) (rightOwner, _, _) ->
            leftOwner == rightOwner
        )
        returnedLambdaParameterBoundaries
    )
  guard
    ( all returnedLambdaParameterBoundaryAgrees
        returnedLambdaParameterBoundaries
    )
  guard
    ( all returnedLambdaParameterBoundaryBelongsToResultChain
        returnedLambdaParameterBoundaries
    )
  guard
    ( returnedResultChainBelongsToConstructedType
        constructedTy
        returnedResults
    )
  pure
    CertifiedLambdaBodyConstruction
      { certifiedLambdaBodyOwner = bodyOwner
      , certifiedLambdaBodyConstructedType = constructedTy
      , certifiedLambdaBodyConstructedBinders = certifiedBinders
      , certifiedLambdaBodyConsumedBinders = consumedBinders
      , certifiedLambdaBodySourceRefs = sourceRefs
      , certifiedLambdaBodyParameterRefs = parameterRefs
      , certifiedLambdaBodyOwnLambdaParameterBoundary =
          ownLambdaParameterBoundary
      , certifiedLambdaBodyAmbientDeclarations = ambientDeclarations
      , certifiedLambdaBodyScopeDependencyRenames =
          scopeDependencyRenames
      , certifiedLambdaBodyReturnedResults = returnedResults
      , certifiedLambdaBodyReturnedLambdaParameterBoundaries =
          returnedLambdaParameterBoundaries
      }

  where
    constructedBinders = schemeBinderRefs (schemeFromType constructedTy)
    constructedRefs =
      typeBinderDeclarationRefs constructedTy
        ++ freeTypeVarRefsType constructedTy
    constructedOccurrenceRefs = constructedRefs

    bindersAgree (leftRef, leftBound) (rightRef, rightBound) =
      typeBinderRefsSameIdentity leftRef rightRef
        && case (leftBound, rightBound) of
          (Nothing, Nothing) -> True
          (Just left, Just right) ->
            operationalEndpointTypesAgree
              (tyToElab left)
              (tyToElab right)
          _ -> False

    returnedLambdaParameterBoundaryAgrees
      (returnedOwner, boundaryCertificate, returnedTy) =
        lgoConstructor returnedOwner == LocalLambdaGamma
          && case schemeBody (schemeFromType returnedTy) of
            TArrow returnedParamTy _ ->
              operationalEndpointTypesAgree
                returnedParamTy
                (lambdaParamBoundaryConstructedType boundaryCertificate)
            _ -> False

    ownLambdaParameterBoundaryAgrees boundaryCertificate =
      lgoConstructor bodyOwner == LocalLambdaGamma
        && case schemeBody (schemeFromType constructedTy) of
          TArrow constructedParamTy _ ->
            operationalEndpointTypesAgree
              constructedParamTy
              (lambdaParamBoundaryConstructedType boundaryCertificate)
          _ -> False

    returnedLambdaParameterBoundaryBelongsToResultChain
      (returnedOwner, _, returnedTy) =
        any
          ( \(resultOwner, resultTy) ->
              resultOwner == returnedOwner
                && operationalEndpointTypesAgree resultTy returnedTy
          )
          returnedResults

    distinctBinderIdentities binders =
      and
        [ not
            ( any
                (typeBinderRefsSameIdentity ref . fst)
                (drop (index + 1) binders)
            )
        | (index, (ref, _)) <- zip [0 :: Int ..] binders
        ]

    allDistinctBy same = go
      where
        go [] = True
        go (item : rest) = not (any (same item) rest) && go rest

    -- The returned-result certificate is an ordered owner chain.  Validate
    -- each child against the endpoint constructed by its immediate parent,
    -- rather than requiring every descendant to remain visible in the outer
    -- owner's final type.  A parent may publish the child through a flexible
    -- result declaration, in which case the child is the exact certified
    -- bound and the parent's visible result is that declaration.  The owner
    -- chain supplies positive provenance; this shape check only confirms the
    -- construction recorded by that provenance.
    returnedResultChainBelongsToConstructedType _ [] = True
    returnedResultChainBelongsToConstructedType parentTy ((_, returnedTy) : rest) =
      resultPathContains parentTy returnedTy
        && returnedResultChainBelongsToConstructedType returnedTy rest

    resultPathContains parentTy returnedTy = go parentTy
      where
        go currentTy
          | operationalEndpointTypesAgree currentTy returnedTy = True
        go (TForallRef ref mbBound bodyTy) =
          publishesReturnedBound ref mbBound bodyTy || go bodyTy
        go (TArrow _ resultTy) = go resultTy
        go _ = False

        publishesReturnedBound ref (Just bound) bodyTy =
          terminalFlexibleResultRef bodyTy == Just ref
            && operationalEndpointTypesAgree
              (tyToElab bound)
              returnedTy
        publishesReturnedBound _ Nothing _ = False

        terminalFlexibleResultRef currentTy =
          case currentTy of
            TVarRef ref -> Just ref
            TForallRef _ _ bodyTy -> terminalFlexibleResultRef bodyTy
            TArrow _ resultTy -> terminalFlexibleResultRef resultTy
            _ -> Nothing

-- | Proof that a packet's operated consumer is one declaration constructed
-- by the recursively checked child.  The constructor is private: callers can
-- inspect the selected ref for local-vs-ambient classification, but only the
-- certificate owner can recover its checked bound or turn the ref into a
-- cross-identity body-consumer projection.
data CertifiedPacketConsumerBinder =
  CertifiedPacketConsumerBinder
    !TypeBinderIdentity
    !TypeBinderRef
    !BoundType
  deriving (Eq, Show)

certifiedPacketConsumerBinderRef
  :: CertifiedPacketConsumerBinder
  -> TypeBinderRef
certifiedPacketConsumerBinderRef
  (CertifiedPacketConsumerBinder _ ref _) = ref

-- | Proof that a packet's exact exterior route selects one source declaration
-- emitted by the recursively checked child.  A lambda boundary deliberately
-- hides child-local aliases from its ambient environment, so the current
-- pending Gamma scheme can retain the declaration while losing the graph
-- route by which the packet selected it.  The private constructor records the
-- join of four positive authorities: the packet's unanimous route, its
-- consumer declaration and operated/completed occurrences, the current
-- declaration, and the checked child's exact binder spine and endpoint.  The
-- packet's staged SchemeInfos may legitimately retain both the provisional
-- graph route and its copied construction route.  They are alternatives, not
-- aliases to quotient eagerly: exactly one must survive the complete join.
newtype CertifiedPacketSourceConsumerBinder =
  CertifiedPacketSourceConsumerBinder TypeBinderRef
  deriving (Eq, Show)

certifiedPacketSourceConsumerBinderRef
  :: CertifiedPacketSourceConsumerBinder
  -> TypeBinderRef
certifiedPacketSourceConsumerBinderRef
  (CertifiedPacketSourceConsumerBinder ref) = ref

certifyPacketSourceConsumerBinder
  :: PreparedSubtermGeneralization
  -> SchemeInfo
  -> CertifiedLambdaBodyConstruction
  -> ElabType
  -> Either ElabError (Maybe CertifiedPacketSourceConsumerBinder)
certifyPacketSourceConsumerBinder packet currentSchemeInfo bodyConstruction sourceTy =
  case subtermGeneralizationGammaAuthority packet of
    Nothing -> pure Nothing
    Just gammaAuthority ->
      case typeBinderIdentityNode (gpaConsumerIdentity gammaAuthority) of
        Nothing -> pure Nothing
        Just exterior ->
          certifyUniqueRoute (distinctRefs (packetRoutes exterior))
  where
    certifyUniqueRoute routedRefs = do
      certifiedRoutes <-
        catMaybes <$> traverse certifyRoutedDeclaration routedRefs
      case distinctCertifiedRoutes certifiedRoutes of
        [] -> pure Nothing
        [certificate] -> pure (Just certificate)
        certificates ->
          Left
            ( ValidationFailed
                [ "packet source consumer has conflicting checked construction routes"
                , "  routes: " ++ show routedRefs
                , "  certificates: " ++ show certificates
                ]
            )

    distinctCertifiedRoutes = foldl' insertCertificate []
      where
        insertCertificate certificates certificate
          | certificate `elem` certificates = certificates
          | otherwise = certificates ++ [certificate]

    packetRoutes exterior =
      [ routedRef
      | packetInfo <- packetSchemeInfos
      , routedRef <-
          maybeToList
            ( IntMap.lookup
                (getNodeId exterior)
                (schemeInfoBinderRefSubst packetInfo)
            )
      ]

    packetSchemeInfos =
      [ subtermGeneralizationConsumerConstructionSchemeInfo packet
      , subtermGeneralizationOperatedSchemeInfo packet
      , subtermGeneralizationSchemeInfo packet
      ]

    certifyRoutedDeclaration routedRef =
      case
          ( currentCandidates routedRef
          , bodyCandidates routedRef
          , consumerConstructionCandidates routedRef
          )
        of
          ( [(currentRef, currentBound)]
            , [(_, bodyBound)]
            , [_]
            )
              | currentBoundCanBeCompleted currentBound bodyBound
              , packetEndpointMentions routedRef
              , operationalEndpointTypesAgree
                  (certifiedLambdaBodyConstructedType bodyConstruction)
                  sourceTy ->
                  pure
                    ( Just
                        (CertifiedPacketSourceConsumerBinder currentRef)
                    )
          ([], _, _) -> pure Nothing
          (_, [], _) -> pure Nothing
          (_, _, []) -> pure Nothing
          (currentBinders, bodyBinders, consumerBinders) ->
              Left
                ( ValidationFailed
                    [ "packet source consumer route is ambiguous in the checked child construction"
                    , "  route: " ++ show routedRef
                    , "  current binders: " ++ show currentBinders
                    , "  child binders: " ++ show bodyBinders
                    , "  packet consumer binders: " ++ show consumerBinders
                    ]
                )

    currentCandidates routedRef =
      [ binder
      | binder@(ref, _) <-
          schemeBinderRefs (siScheme currentSchemeInfo)
      , typeBinderRefsSameIdentity ref routedRef
      ]

    bodyCandidates routedRef =
      [ binder
      | binder@(ref, _) <-
          certifiedLambdaBodyConstructedBinders bodyConstruction
      , typeBinderRefsSameIdentity ref routedRef
      ]

    consumerConstructionCandidates routedRef =
      [ binder
      | binder@(ref, _) <-
          schemeBinderRefs
            ( siScheme
                (subtermGeneralizationConsumerConstructionSchemeInfo packet)
            )
      , typeBinderRefsSameIdentity ref routedRef
      ]

    packetEndpointMentions routedRef =
      all
        ( any (typeBinderRefsSameIdentity routedRef)
            . packetOccurrenceRefs
        )
        [ subtermGeneralizationOperatedSchemeInfo packet
        , subtermGeneralizationSchemeInfo packet
        ]

    packetOccurrenceRefs info =
      let packetTy = schemeToType (siScheme info)
       in typeBinderDeclarationRefs packetTy
            ++ freeTypeVarRefsType packetTy

    currentBoundCanBeCompleted Nothing _ = True
    currentBoundCanBeCompleted (Just currentBound) (Just bodyBound) =
      operationalEndpointTypesAgree
        (tyToElab currentBound)
        (tyToElab bodyBound)
    currentBoundCanBeCompleted (Just _) Nothing = False

    distinctRefs = foldl' insertRef []
      where
        insertRef refs ref
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = refs ++ [ref]

-- | Join a packet's exact operated consumer route with a recursively checked
-- child construction.  Child-local declarations are deliberately removed
-- from the enclosing ambient alias map, but a packet owned by the enclosing
-- lambda may still consume one of those declarations locally.  This
-- certificate recovers that classification without reintroducing an ambient
-- alias: the packet supplies the graph-exterior route, while the child
-- supplies the complete checked binder spine and endpoint.
certifyPacketConsumerBinderFromBodyConstruction
  :: PreparedSubtermGeneralization
  -> SchemeInfo
  -> CertifiedLambdaBodyConstruction
  -> Either ElabError (Maybe CertifiedPacketConsumerBinder)
certifyPacketConsumerBinderFromBodyConstruction packet currentSchemeInfo bodyConstruction =
  case subtermGeneralizationGammaAuthority packet of
    Nothing -> pure Nothing
    Just gammaAuthority ->
      case typeBinderIdentityNode (gpaConsumerIdentity gammaAuthority) of
        Nothing -> pure Nothing
        Just exterior ->
          case
              IntMap.lookup
                (getNodeId exterior)
                ( schemeInfoBinderRefSubst
                    (subtermGeneralizationOperatedSchemeInfo packet)
                )
            of
              Nothing -> pure Nothing
              Just operatedRef ->
                case
                    ( currentCandidates operatedRef
                    , bodyCandidates operatedRef
                    , operatedCandidates operatedRef
                    )
                  of
                  ( [currentRef]
                    , [(_, bodyBound)]
                    , [(_, Just operatedBound)]
                    )
                    | currentDeclarationAgrees currentRef bodyBound ->
                        pure
                          ( Just
                              ( CertifiedPacketConsumerBinder
                                  (gpaConsumerIdentity gammaAuthority)
                                  currentRef
                                  operatedBound
                              )
                          )
                  ([], _, _) -> pure Nothing
                  (_, [], _) -> pure Nothing
                  (currentRefs, bodyBinders, operatedBinders) ->
                    Left
                      ( ValidationFailed
                          [ "packet operated consumer route is ambiguous in the checked child construction"
                          , "  consumer: "
                              ++ show (gpaConsumerIdentity gammaAuthority)
                          , "  operated route: " ++ show operatedRef
                          , "  current refs: " ++ show currentRefs
                          , "  child binders: " ++ show bodyBinders
                          , "  operated binders: " ++ show operatedBinders
                          ]
                      )
  where
    currentCandidates operatedRef =
      distinctRefs
        [ ref
        | ref <-
            typeBinderDeclarationRefs currentType
              ++ freeTypeVarRefsType currentType
        , typeBinderRefsSameIdentity ref operatedRef
        ]
    bodyCandidates operatedRef =
      [ binder
      | binder@(ref, _) <-
          certifiedLambdaBodyConstructedBinders bodyConstruction
      , typeBinderRefsSameIdentity ref operatedRef
      ]
    operatedCandidates operatedRef =
      [ binder
      | binder@(ref, _) <-
          schemeBinderRefs
            ( siScheme
                (subtermGeneralizationOperatedSchemeInfo packet)
            )
      , typeBinderRefsSameIdentity ref operatedRef
      ]
    currentType = schemeToType (siScheme currentSchemeInfo)

    currentDeclarationAgrees currentRef bodyBound =
      case
          [ currentBound
          | (declaredRef, currentBound) <-
              schemeBinderRefs (siScheme currentSchemeInfo)
          , typeBinderRefsSameIdentity declaredRef currentRef
          ]
        of
          [] -> True
          [currentBound] -> declaredBoundsAgree currentBound bodyBound
          _ -> False

    distinctRefs = foldl' insertRef []
      where
        insertRef refs ref
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = refs ++ [ref]

    declaredBoundsAgree Nothing Nothing = True
    declaredBoundsAgree (Just left) (Just right) =
      operationalEndpointTypesAgree (tyToElab left) (tyToElab right)
    declaredBoundsAgree _ _ = False

certifyOpenValueLambdaParameterClosure
  :: ElabType
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
  -> Either [String] CertifiedOpenValueLambdaParameterClosure
certifyOpenValueLambdaParameterClosure closedTy openTy =
  certifyOpenValueLambdaParameterClosureAtBinders
    (schemeBinderRefs closedScheme)
    (schemeBody closedScheme)
    openTy
  where
    closedScheme = schemeFromType closedTy

-- | Certify the same value-lambda parameter closure when the newly closed
-- declarations are already known independently of the endpoint's remaining
-- leading forall spine.  This is needed when the returned body is itself
-- polymorphic: flattening the complete endpoint would otherwise mistake the
-- child's retained result binders for value-parameter closures.
certifyOpenValueLambdaParameterClosureAtBinders
  :: [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
  -> Either [String] CertifiedOpenValueLambdaParameterClosure
certifyOpenValueLambdaParameterClosureAtBinders closedBinders openedSource openTy mbBodyConstruction = do
  bodyConstruction <-
    maybe
      (Left ["the recursively checked body has no owner-final construction"])
      Right
      mbBodyConstruction
  let certifiedParameterRefs =
        certifiedLambdaBodyParameterRefs bodyConstruction
  require
    "the owner-final construction is not the open checked endpoint"
    ( operationalEndpointTypesAgree
        (certifiedLambdaBodyConstructedType bodyConstruction)
        openTy
    )
  require
    "the prepared endpoint does not close any value-lambda parameter"
    (not (null closedBinders))
  require
    "the prepared endpoint closes a non-parameter or bounded declaration"
    ( all
        ( \(closedRef, mbBound) ->
            isNothing mbBound
              && any
                (typeBinderRefsSameIdentity closedRef)
                certifiedParameterRefs
        )
        closedBinders
    )
  require
    "the prepared and checked endpoints do not share the certified value-lambda parameter boundary"
    ( all
        ( \(closedRef, _) ->
            matchingValueLambdaParameterBoundary
              closedRef
              openedSource
              openTy
        )
        closedBinders
    )
  pure
    (CertifiedOpenValueLambdaParameterClosure (map fst closedBinders))
  where
    -- The owner-final certificate is the positive construction authority;
    -- this walk only checks that the prepared closure and that construction
    -- refer to the same value-lambda parameter boundary.  It deliberately
    -- crosses independent result binders and stops once the requested
    -- parameter has been witnessed, so it does not pretend that a result
    -- refinement beneath an arrow is an instantiation.
    matchingValueLambdaParameterBoundary closedRef = go
      where
        go rawSource rawEndpoint =
          case (rawSource, rawEndpoint) of
              (TArrow sourceParam sourceBody, TArrow endpointParam endpointBody)
                | operationalEndpointTypesAgree sourceParam endpointParam ->
                    mentions closedRef sourceParam
                      && mentions closedRef endpointParam
                      || go sourceBody endpointBody
              ( TForallRef sourceRef sourceBound sourceBody
                , TForallRef endpointRef endpointBound endpointBody
                )
                  | typeBinderRefsSameIdentity sourceRef endpointRef
                  , boundsAgree sourceBound endpointBound ->
                      go sourceBody endpointBody
              (TForallRef sourceRef _ sourceBody, _)
                | not (typeBinderRefsSameIdentity sourceRef closedRef) ->
                    go sourceBody rawEndpoint
              (_, TForallRef endpointRef _ endpointBody)
                | not (typeBinderRefsSameIdentity endpointRef closedRef) ->
                    go rawSource endpointBody
              _ -> False

        mentions ref =
          any
            (typeBinderRefsSameIdentity ref)
            . freeTypeVarRefsType

        boundsAgree Nothing Nothing = True
        boundsAgree (Just sourceBound) (Just endpointBound) =
          operationalEndpointTypesAgree
            (tyToElab sourceBound)
            (tyToElab endpointBound)
        boundsAgree _ _ = False

    require message condition
      | condition = Right ()
      | otherwise = Left [message]

-- | Positive evidence that an opaque prepared endpoint and the recursively
-- checked body are the two sides of the same source-lambda construction.
--
-- A packet can freeze a source endpoint before an enclosing value lambda has
-- commuted its construction-owned forall spine through that lambda.  The two
-- presentations are not related by a plain xMLF instantiation, so comparing
-- their final types cannot establish the transition.  This certificate joins
-- instead:
--
-- * the packet's sealed source-lambda completion authority;
-- * the exact returned-result nodes carried by the checked child owner; and
-- * the current body edge plus its ambient/construction binder boundary.
--
-- The constructor is private so the enclosing-consumer transition can use
-- this case only after all three authorities agree.
data CertifiedOpaqueSourceLambdaBodyCompletion =
  CertifiedOpaqueSourceLambdaBodyCompletion
  deriving (Eq, Show)

certifyOpaqueSourceLambdaBodyCompletion
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> [LambdaParamBoundaryCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> ElabType
  -> ElabType
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
  -> Either [String] CertifiedOpaqueSourceLambdaBodyCompletion
certifyOpaqueSourceLambdaBodyCompletion
  currentOwner
  bodyEdge
  packet
  parameterBoundaryCertificates
  ambientBindings
  checkedBodyType
  completedGammaBound
  rawOperatedType
  mbBodyConstruction = do
  bodyConstruction <-
    requireValue
      "the recursively checked body has no owner-final construction"
      mbBodyConstruction
  require
    "the checked body construction does not belong beneath this lambda boundary"
    ( lgoConstructor currentOwner == LocalLambdaGamma
        && lgoBoundaryEdge currentOwner == bodyEdge
        && certifiedLambdaBodyOwner bodyConstruction /= currentOwner
    )
  let completedConstructedBodyType =
        completeBoundaryType
          (certifiedLambdaBodyConstructedType bodyConstruction)
  require
    "the checked body owner does not construct the completed enclosing Gamma bound"
    ( operationalEndpointTypesAgree
        completedConstructedBodyType
        checkedBodyType
        && operationalEndpointTypesAgree
          completedConstructedBodyType
          completedGammaBound
    )
  (opaqueConstructedType, sourceLambdaAuthorities) <-
    requireValue
      "the packet has no source-lambda opaque completion authority"
      (subtermGeneralizationOpaqueResultSourceLambdaCompletion packet)
  require
    "the packet's source-lambda opaque completion is empty"
    (not (null sourceLambdaAuthorities))
  unless
    ( operationalEndpointTypesAgree
        opaqueConstructedType
        rawOperatedType
    )
    ( Left
        [ "the packet's opaque source endpoint is not its frozen operated endpoint"
        , "  opaque source endpoint: " ++ show opaqueConstructedType
        , "  frozen operated endpoint: " ++ show rawOperatedType
        ]
    )
  let operatedScheme = schemeFromType rawOperatedType
      operatedLeadingBinders = schemeBinderRefs operatedScheme
      constructionRefs =
        IntMap.elems
          ( siConstructionBinderOrderRefs
              (subtermGeneralizationSchemeInfo packet)
          )
      sourceRefs =
        IntMap.elems
          ( siSourceBinderOrderRefs
              (subtermGeneralizationSchemeInfo packet)
          )
      constructionOwns (ref, _) =
        any (typeBinderRefsSameIdentity ref) constructionRefs
          && not (any (typeBinderRefsSameIdentity ref) sourceRefs)
      ambientOwns (ref, mbBound) =
        case
            [ bound
            | (ambientRef, bound) <- Map.toList ambientBindings
            , typeBinderRefsSameIdentity ref ambientRef
            ]
          of
            [ambientBound] ->
              operationalEndpointTypesAgree
                (maybe TBottom tyToElab mbBound)
                ambientBound
            _ -> False
      boundaryOwns binder =
        constructionOwns binder
          || ambientOwns binder
  require
    "the opaque operated prefix contains a binder owned by neither ambient Gamma nor construction"
    (all boundaryOwns operatedLeadingBinders)
  require
    "a source lambda sealed by the packet is absent from the checked result construction"
    ( all
        (sourceLambdaIsReturned bodyConstruction)
        sourceLambdaAuthorities
    )
  pure CertifiedOpaqueSourceLambdaBodyCompletion
  where
    completeBoundaryType =
      completeLambdaParamBoundaryType parameterBoundaryCertificates

    require message condition
      | condition = Right ()
      | otherwise = Left [message]

    requireValue message = maybe (Left [message]) Right

    sourceLambdaIsReturned
      bodyConstruction
      (lambdaNode, sourceParameterType) =
      case
          [ ()
          | (resultOwner, rawResultType) <-
              certifiedLambdaBodyReturnedResults bodyConstruction
          , lgoConstructor resultOwner == LocalLambdaGamma
          , lgoTermNode resultOwner == lambdaNode
          , let resultScheme =
                  schemeFromType (completeBoundaryType rawResultType)
          , TArrow resultParameterType _ <- [schemeBody resultScheme]
          , operationalEndpointTypesAgree
              resultParameterType
              (completeBoundaryType sourceParameterType)
          ]
        of
          [_] -> True
          _ -> False

-- | Positive evidence that packet preparation and recursive construction
-- agree on an enclosing source owner's body endpoint.  The source/canonical
-- lockstep walk seals the endpoint before the child is checked; the private
-- body-construction certificate then validates that exact endpoint.  This is
-- the commuting case where the frozen graph packet places a returned forall
-- above an arrow while the source owner keeps it in the arrow codomain.
data CertifiedSourceOwnerConsumerBodyCompletion =
  CertifiedSourceOwnerConsumerBodyCompletion
  deriving (Eq, Show)

certifySourceOwnerConsumerBodyCompletion
  :: TypeCheck.Env
  -> LocalGammaOwner
  -> EdgeId
  -> LocalGammaOwner
  -> SubtermConsumerAuthority
  -> PreparedSubtermGeneralization
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
  -> Either [String] CertifiedSourceOwnerConsumerBodyCompletion
certifySourceOwnerConsumerBodyCompletion
  typeEnv
  currentOwner
  bodyEdge
  enclosingOwner
  selectedAuthority
  packet
  parameterBoundaryCertificates
  checkedBodyType
  completedGammaBound
  rawOperatedType
  mbBodyConstruction = do
    (certifiedAuthority, certifiedOwner, frozenOperatedType, expectedEndpoint) <-
      requireValue
        "the packet has no source-owner consumer completion"
        (subtermGeneralizationSourceOwnerConsumerCompletion packet)
    require
      "the source-owner completion changed consumer authority"
      (certifiedAuthority == selectedAuthority)
    require
      "the source-owner completion belongs to a different enclosing owner"
      (certifiedOwner == enclosingOwner)
    require
      "the source-owner completion does not belong beneath this lambda boundary"
      ( lgoConstructor currentOwner == LocalLambdaGamma
          && lgoBoundaryEdge currentOwner == bodyEdge
      )
    require
      "the source-owner completion was sealed against a different operated endpoint"
      ( operationalEndpointTypesAgree
          frozenOperatedType
          rawOperatedType
      )
    require
      "the source-owner completion is not an actual lexical-depth transition"
      ( not
          ( operationalEndpointTypesAgree
              frozenOperatedType
              expectedEndpoint
          )
      )
    bodyConstruction <-
      requireValue
        "the recursively checked body has no owner-final construction"
        mbBodyConstruction
    require
      "the recursively checked construction is not owned by a strict child"
      ( certifiedLambdaBodyOwner bodyConstruction /= currentOwner
          && certifiedLambdaBodyOwner bodyConstruction /= enclosingOwner
      )
    let completedConstructedBodyType =
          completeLambdaParamBoundaryType
            parameterBoundaryCertificates
            (certifiedLambdaBodyConstructedType bodyConstruction)
        completedExpectedEndpoint =
          completeLambdaParamBoundaryType
            parameterBoundaryCertificates
            expectedEndpoint
        -- The checked child may commute a source-owned forall spine out of
        -- this value lambda.  Pair that leading child spine with the exact
        -- source codomain declarations, then explicitly apply every paired
        -- variable and N-eliminate every remaining bounded declaration.  The
        -- source/canonical packet certificate fixes the positional identity
        -- correspondence; the instantiation checker still validates the
        -- complete Figure 15.3.5 computation to the child codomain.
        sourceOwnerCodomainAtChildBinders = do
          let childScheme = schemeFromType completedConstructedBodyType
              expectedScheme = schemeFromType completedExpectedEndpoint
              childBinders = schemeBinderRefs childScheme
          TArrow childParameter childBody <-
            pure (schemeBody childScheme)
          TArrow expectedParameter expectedBody <-
            pure (schemeBody expectedScheme)
          guard
            ( operationalEndpointTypesAgree
                expectedParameter
                childParameter
            )
          let expectedBodyBinders =
                schemeBinderRefs (schemeFromType expectedBody)
          guard (length childBinders <= length expectedBodyBinders)
          let (pairedExpectedBinders, remainingExpectedBinders) =
                splitAt (length childBinders) expectedBodyBinders
              pairedBinders = zip pairedExpectedBinders childBinders
              binderRenames =
                [ (expectedRef, childRef)
                | ((expectedRef, _), (childRef, _)) <- pairedBinders
                ]
          guard
            ( all
                ( \((expectedRef, expectedBound), (childRef, childBound)) ->
                    certifiedScopeRefsCorrespond
                      (certifiedLambdaBodyScopeDependencyRenames bodyConstruction)
                      expectedRef
                      childRef
                      && case
                          ( fmap
                              ( renameBoundTypeBinderRefPayloads
                                  binderRenames
                              )
                              expectedBound
                          , childBound
                          )
                        of
                        (Nothing, Nothing) -> True
                        (Just expected, Just child) ->
                          operationalEndpointTypesAgree
                            (tyToElab expected)
                            (tyToElab child)
                        _ -> False
                )
                pairedBinders
            )
          let childTypeEnv =
                foldr
                  ( \(ref, mbBound) env ->
                      TypeCheck.insertTypeBindingRef
                        ref
                        (maybe TBottom tyToElab mbBound)
                        env
                  )
                  typeEnv
                  childBinders
              arguments =
                map (TVarRef . fst) childBinders
                  ++ [ renameTypeBinderRefPayloads
                         binderRenames
                         (tyToElab bound)
                     | (_, Just bound) <- remainingExpectedBinders
                     ]
          guard
            ( length arguments == length expectedBodyBinders
                && all (isJust . snd) remainingExpectedBinders
            )
          constructExactInstantiationAtSourceArguments
            childTypeEnv
            operationalEndpointTypesAgree
            expectedBody
            arguments
            childBody
        checkedChildConstructsSourceOwnerEndpoint =
          operationalEndpointTypesAgree
            completedExpectedEndpoint
            completedConstructedBodyType
            || isJust
              ( planExactBinderSpine
                  operationalEndpointTypesAgree
                  completedExpectedEndpoint
                  completedConstructedBodyType
              )
            || isJust sourceOwnerCodomainAtChildBinders
    unless
      ( checkedChildConstructsSourceOwnerEndpoint
          && operationalEndpointTypesAgree
            completedConstructedBodyType
            checkedBodyType
          && operationalEndpointTypesAgree
            completedConstructedBodyType
            completedGammaBound
      )
      ( Left
          [ "the checked child does not validate the source-owner endpoint"
          , "  raw child construction: "
              ++ show (certifiedLambdaBodyConstructedType bodyConstruction)
          , "  completed child construction: "
              ++ show completedConstructedBodyType
          , "  raw source-owner endpoint: " ++ show expectedEndpoint
          , "  completed source-owner endpoint: "
              ++ show completedExpectedEndpoint
          , "  certified child scope dependency renames: "
              ++ show
                (certifiedLambdaBodyScopeDependencyRenames bodyConstruction)
          , "  checked body type: " ++ show checkedBodyType
          , "  completed Gamma bound: " ++ show completedGammaBound
          , "  returned child constructions: "
              ++ show (certifiedLambdaBodyReturnedResults bodyConstruction)
          ]
      )
    pure CertifiedSourceOwnerConsumerBodyCompletion
  where
    require message condition
      | condition = Right ()
      | otherwise = Left [message]

    requireValue message = maybe (Left [message]) Right

    certifiedScopeRefsCorrespond scopeDependencyRenames sourceRef targetRef =
      typeBinderRefsSameIdentity sourceRef targetRef
        || any
          ( \(certifiedSourceRef, certifiedTargetRef) ->
              typeBinderRefsSameIdentity sourceRef certifiedSourceRef
                && typeBinderRefsSameIdentity targetRef certifiedTargetRef
          )
          scopeDependencyRenames

-- | Construction plan for the exact enclosing endpoint of Figure 15.3.5's
-- lambda rule.  The expected packet scheme owns both the complete
-- @Lambda(Gamma_g)@ spine and the terminal body computation.  Keeping that
-- authority together prevents the elaborator from first dropping a vacuous
-- Gamma binder and then trying to recover the published type by specializing
-- the already-built lambda.
data ExactLambdaConstructionPlan = ExactLambdaConstructionPlan
  { exactLambdaConstructionBinders ::
      ![(TypeBinderRef, Maybe BoundType)]
  , exactLambdaConstructionPublishedBinders ::
      ![(TypeBinderRef, Maybe BoundType)]
  , exactLambdaConstructionPublishedType :: !ElabType
  , exactLambdaConstructionBinderRenames ::
      ![(TypeBinderRef, TypeBinderRef)]
  , -- | Fresh lexical presentations allocated for forall declarations inside
    -- the value-lambda parameter when a Lambda(Gamma) declaration carries the
    -- same source identity.  These copies apply to the parameter and its body
    -- occurrences, but not to the enclosing Gamma binder.
    exactLambdaConstructionParameterBinderCopies ::
      ![(TypeBinderRef, TypeBinderRef)]
  , exactLambdaConstructionBodyBinderRenames ::
      ![(TypeBinderRef, TypeBinderRef)]
  , -- | Exact source-to-copy routes allocated by the body's explicit xMLF
    -- lexical-forall computation.  These are construction provenance: an
    -- enclosing result owner may use them to present the already checked
    -- returned value in the copied identity domain, but may not infer another
    -- route from alpha-equivalent endpoint types.
    exactLambdaConstructionResultBinderCopies ::
      ![(TypeBinderRef, TypeBinderRef)]
  , exactLambdaConstructionBodyAbstractions ::
      ![(TypeBinderRef, Maybe BoundType)]
  , exactLambdaConstructionBodyInstantiation :: !Instantiation
  , exactLambdaConstructionBodyType :: !ElabType
  , exactLambdaConstructionPreservedBodyRefinements ::
      ![BodyConsumerBoundRefinementCertificate]
  , exactLambdaConstructionAmbientBodyRefinement ::
      !(Maybe (TypeBinderRef, ElabType, ElabType))
  , exactLambdaConstructionIntroducedAmbientBodyDeclaration ::
      !(Maybe (TypeBinderRef, ElabType))
  , exactLambdaConstructionAmbientBodyRefinementCertificate ::
      !(Maybe BodyConsumerBoundRefinementCertificate)
  , exactLambdaConstructionPacketBodyProjection ::
      !(Maybe CertifiedPacketConsumerBodyProjection)
  , exactLambdaConstructionCompletionInstantiation :: !Instantiation
  }
  deriving (Eq, Show)

-- | Whether a computation allocates a new type binder with xMLF's O rule.
-- An O step can validly construct a type, but it cannot prove that an exact
-- declaration from a prepared packet was retained by the construction.
instantiationIntroducesFreshBinder :: Instantiation -> Bool
instantiationIntroducesFreshBinder inst =
  case inst of
    InstIntro -> True
    InstSeq left right ->
      instantiationIntroducesFreshBinder left
        || instantiationIntroducesFreshBinder right
    InstInside nested -> instantiationIntroducesFreshBinder nested
    InstUnderRef _ nested -> instantiationIntroducesFreshBinder nested
    _ -> False

-- | Whether the publication computation preserves the identity topology of
-- the construction spine.  In particular, @O@ constructs a fresh vacuous
-- declaration and therefore cannot certify retention of a prepared binder.
exactLambdaConstructionCompletionPreservesBinderIdentities
  :: ExactLambdaConstructionPlan
  -> Bool
exactLambdaConstructionCompletionPreservesBinderIdentities =
  not
    . instantiationIntroducesFreshBinder
    . exactLambdaConstructionCompletionInstantiation

-- | Recognize the paper's terminal-Hyp lambda publication boundary.  The
-- expected endpoint is the exact ambient declaration, while its installed
-- non-bottom bound is the value-lambda type that this constructor must build.
-- The declaration is deliberately not inferred from the bound's shape: both
-- its identity and its bound come from the active Gamma.
ambientLambdaPublicationTarget
  :: Map.Map TypeBinderRef ElabType
  -> ElabType
  -> Maybe (TypeBinderRef, ElabType)
ambientLambdaPublicationTarget ambientBindings expectedTy = do
  publicationRef <-
    case expectedTy of
      TVarRef ref -> Just ref
      _ -> Nothing
  publicationBound <-
    TypeCheck.lookupTypeBindingRef
      publicationRef
      (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] ambientBindings)
  guard
    ( not (operationalEndpointTypesAgree publicationBound TBottom)
        && not
          ( any
              (typeBinderRefsSameIdentity publicationRef)
              ( freeTypeVarRefsType publicationBound
                  ++ typeBinderDeclarationRefs publicationBound
              )
          )
    )
  case schemeBody (schemeFromType publicationBound) of
    TArrow _ _ -> pure (publicationRef, publicationBound)
    _ -> Nothing

-- | Append the exact terminal @Hyp(a)@ computation after a lambda plan has
-- constructed the installed bound of ambient @a@.  This is a positive
-- construction step: the complete combined instantiation is checked in the
-- same Gamma before the plan is returned, and @a@ is removed from the local
-- binder spine by the caller.
publishExactLambdaPlanAtAmbientReference
  :: Map.Map TypeBinderRef ElabType
  -> ElabType
  -> TypeBinderRef
  -> ExactLambdaConstructionPlan
  -> Either ElabError ExactLambdaConstructionPlan
publishExactLambdaPlanAtAmbientReference
  ambientBindings
  paramTy
  publicationRef
  plan = do
    publicationInstantiation <-
      case
          constructExactInstantiation
            constructionTypeEnv
            exactLambdaEndpointTypesAgree
            (exactLambdaConstructionPublishedType plan)
            publishedTy
        of
          Just inst -> pure inst
          Nothing ->
            failure
              "the constructed lambda bound cannot be published at its ambient declaration"
              [ "  ambient declaration: " ++ show publicationRef
              , "  installed bound: "
                  ++ show
                    ( TypeCheck.lookupTypeBindingRef
                        publicationRef
                        constructionTypeEnv
                    )
              , "  constructed bound: "
                  ++ show (exactLambdaConstructionPublishedType plan)
              ]
    let completionInstantiation =
          composeInst
            (exactLambdaConstructionCompletionInstantiation plan)
            publicationInstantiation
    completedTy <-
      case
          TypeCheck.checkInstantiation
            constructionTypeEnv
            constructionTy
            completionInstantiation
        of
          Right ty -> pure ty
          Left cause ->
            failure
              "the terminal ambient lambda publication does not typecheck"
              [ "  construction type: " ++ show constructionTy
              , "  completion: " ++ show completionInstantiation
              , "  cause: " ++ show cause
              ]
    unless
      (exactLambdaEndpointTypesAgree completedTy publishedTy)
      ( failure
          "the terminal ambient lambda publication reaches a different endpoint"
          [ "  constructed endpoint: " ++ show completedTy
          , "  expected endpoint: " ++ show publishedTy
          ]
      )
    pure
      plan
        { exactLambdaConstructionPublishedBinders = []
        , exactLambdaConstructionPublishedType = publishedTy
        , exactLambdaConstructionCompletionInstantiation =
            completionInstantiation
        }
  where
    publishedTy = TVarRef publicationRef
    constructionTy =
      schemeToType
        ( mkElabSchemeWithRefs
            (exactLambdaConstructionBinders plan)
            ( TArrow
                paramTy
                (exactLambdaConstructionBodyType plan)
            )
        )
    constructionTypeEnv =
      foldr
        ( \(ref, mbBound) typeEnv ->
            TypeCheck.insertTypeBindingRef
              ref
              (maybe TBottom tyToElab mbBound)
              typeEnv
        )
        (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] ambientBindings)
        (exactLambdaConstructionBinders plan)
    failure detail context =
      Left
        ( ValidationFailed
            ( "invalid exact ambient lambda publication"
                : ("  detail: " ++ detail)
                : context
            )
        )

-- | Construct a lambda at the expected endpoint, keeping terminal ambient
-- publication in one place for both packet-backed and inherited-endpoint
-- construction.  A bounded ambient result is not a local Lambda(Gamma)
-- candidate: construct its installed value-lambda bound first, then publish
-- the finished plan through the exact ambient declaration with @Hyp@.
certifyLambdaAtExpectedEndpoint
  :: Map.Map TypeBinderRef ElabType
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> ( [(TypeBinderRef, Maybe BoundType)]
       -> ElabType
       -> Either ElabError ExactLambdaConstructionPlan
     )
  -> Either ElabError ExactLambdaConstructionPlan
certifyLambdaAtExpectedEndpoint
  ambientBindings
  rawCandidates
  paramTy
  expectedTy
  certifyAtLambdaType =
    case ambientLambdaPublicationTarget ambientBindings expectedTy of
      Nothing -> certifyAtLambdaType rawCandidates expectedTy
      Just (publicationRef, publicationBound) -> do
        plan <-
          certifyAtLambdaType
            [ candidate
            | candidate@(candidateRef, _) <- rawCandidates
            , not
                (typeBinderRefsSameIdentity candidateRef publicationRef)
            ]
            publicationBound
        publishExactLambdaPlanAtAmbientReference
          ambientBindings
          paramTy
          publicationRef
          plan

-- | Make the lexical ownership chosen by an exact lambda plan explicit in
-- the body source before the plan leaves the constructor.  A recursively
-- checked body can bind a source forall whose graph identity is also used by
-- the enclosing lambda parameter or by Lambda(Gamma).  Keeping that shared
-- identity would place the body's 'ETyAbsRef' underneath a declaration that
-- already has the identity in scope.  The caller supplies the binders proved
-- to occur lexically in the checked term; foralls nested only inside a type
-- are not term scopes and must not be copied here.  Alpha-copy only those
-- source-owned lexical binders and check the already certified computation in the copied
-- domain.  The computation itself is alpha-invariant: its source-forall
-- operations are positional, and 'InstUnderRef' rebinds its placeholder to
-- the actual source binder when applied.  Rewriting free refs in 'InstApp',
-- 'InstBot', or 'InstAbstrRef' would instead capture ambient operands that
-- intentionally retain the enclosing identity.
--
-- The published endpoint is deliberately not renamed: its occurrences name
-- the enclosing declaration.  The copied binder belongs only to the checked
-- body operand which the endpoint computation consumes.
type TypeDeclarationScopeFresheningState =
  ( [TypeBinderRef]
  , IdentityGenerator
  , [(TypeBinderRef, TypeBinderRef)]
  )

-- | Copy lexical declarations in one type while retaining every free
-- reference.  The caller supplies the identities already owned by sibling or
-- enclosing scopes and threads the returned state through later siblings.
-- This is the common constructor used both for candidate bounds and for a
-- completed consumer bound inserted at more than one occurrence.
freshenTypeDeclarationScopesInBound
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeDeclarationScopeFresheningState
  -> BoundType
  -> (BoundType, TypeDeclarationScopeFresheningState)
freshenTypeDeclarationScopesInBound active state bound =
  case bound of
    TArrow domain codomain ->
      let (domain', state') =
            freshenTypeDeclarationScopesInType active state domain
          (codomain', state'') =
            freshenTypeDeclarationScopesInType active state' codomain
       in (TArrow domain' codomain', state'')
    TConWithIdentity identity constructor arguments ->
      let (arguments', state') =
            freshenTypeDeclarationScopesInNonEmpty active state arguments
       in (TConWithIdentity identity constructor arguments', state')
    TVarAppRef ref arguments ->
      let (arguments', state') =
            freshenTypeDeclarationScopesInNonEmpty active state arguments
       in (TVarAppRef (activeFreshenedTypeRef active ref) arguments', state')
    TBaseWithIdentity identity base ->
      (TBaseWithIdentity identity base, state)
    TForallRef ref mbNestedBound body ->
      let (mbNestedBound', state') =
            freshenOptionalTypeDeclarationScopes active state mbNestedBound
          (copiedRef, state'') =
            allocateTypeDeclarationScopeCopy ref state'
          bodyActive = enterFreshenedTypeRef ref copiedRef active
          (body', state''') =
            freshenTypeDeclarationScopesInType bodyActive state'' body
       in (TForallRef copiedRef mbNestedBound' body', state''')
    TMuRef ref body ->
      let (copiedRef, state') =
            allocateTypeDeclarationScopeCopy ref state
          bodyActive = enterFreshenedTypeRef ref copiedRef active
          (body', state'') =
            freshenTypeDeclarationScopesInType bodyActive state' body
       in (TMuRef copiedRef body', state'')
    TBottom -> (TBottom, state)

freshenTypeDeclarationScopesInType
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeDeclarationScopeFresheningState
  -> ElabType
  -> (ElabType, TypeDeclarationScopeFresheningState)
freshenTypeDeclarationScopesInType active state ty =
  case ty of
    TVarRef ref -> (TVarRef (activeFreshenedTypeRef active ref), state)
    TArrow domain codomain ->
      let (domain', state') =
            freshenTypeDeclarationScopesInType active state domain
          (codomain', state'') =
            freshenTypeDeclarationScopesInType active state' codomain
       in (TArrow domain' codomain', state'')
    TConWithIdentity identity constructor arguments ->
      let (arguments', state') =
            freshenTypeDeclarationScopesInNonEmpty active state arguments
       in (TConWithIdentity identity constructor arguments', state')
    TVarAppRef ref arguments ->
      let (arguments', state') =
            freshenTypeDeclarationScopesInNonEmpty active state arguments
       in (TVarAppRef (activeFreshenedTypeRef active ref) arguments', state')
    TBaseWithIdentity identity base ->
      (TBaseWithIdentity identity base, state)
    TForallRef ref mbNestedBound body ->
      let (mbNestedBound', state') =
            freshenOptionalTypeDeclarationScopes active state mbNestedBound
          (copiedRef, state'') =
            allocateTypeDeclarationScopeCopy ref state'
          bodyActive = enterFreshenedTypeRef ref copiedRef active
          (body', state''') =
            freshenTypeDeclarationScopesInType bodyActive state'' body
       in (TForallRef copiedRef mbNestedBound' body', state''')
    TMuRef ref body ->
      let (copiedRef, state') =
            allocateTypeDeclarationScopeCopy ref state
          bodyActive = enterFreshenedTypeRef ref copiedRef active
          (body', state'') =
            freshenTypeDeclarationScopesInType bodyActive state' body
       in (TMuRef copiedRef body', state'')
    TBottom -> (TBottom, state)

freshenTypeDeclarationScopesInList
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeDeclarationScopeFresheningState
  -> [ElabType]
  -> ([ElabType], TypeDeclarationScopeFresheningState)
freshenTypeDeclarationScopesInList _ state [] = ([], state)
freshenTypeDeclarationScopesInList active state (ty : types) =
  let (ty', state') =
        freshenTypeDeclarationScopesInType active state ty
      (types', state'') =
        freshenTypeDeclarationScopesInList active state' types
   in (ty' : types', state'')

freshenTypeDeclarationScopesInNonEmpty
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeDeclarationScopeFresheningState
  -> NonEmpty.NonEmpty ElabType
  -> (NonEmpty.NonEmpty ElabType, TypeDeclarationScopeFresheningState)
freshenTypeDeclarationScopesInNonEmpty
  active
  state
  (firstTy NonEmpty.:| remainingTys) =
    let (firstTy', state') =
          freshenTypeDeclarationScopesInType active state firstTy
        (remainingTys', state'') =
          freshenTypeDeclarationScopesInList active state' remainingTys
     in (firstTy' NonEmpty.:| remainingTys', state'')

freshenOptionalTypeDeclarationScopes
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeDeclarationScopeFresheningState
  -> Maybe BoundType
  -> (Maybe BoundType, TypeDeclarationScopeFresheningState)
freshenOptionalTypeDeclarationScopes _ state Nothing = (Nothing, state)
freshenOptionalTypeDeclarationScopes active state (Just bound) =
  let (bound', state') =
        freshenTypeDeclarationScopesInBound active state bound
   in (Just bound', state')

allocateTypeDeclarationScopeCopy
  :: TypeBinderRef
  -> TypeDeclarationScopeFresheningState
  -> (TypeBinderRef, TypeDeclarationScopeFresheningState)
allocateTypeDeclarationScopeCopy
  ref
  state@(reservedRefs, generator, copies)
  | typeBinderIdentityIsCanonicalStructural
      (typeBinderRefIdentity ref) =
      (ref, state)
  | any (typeBinderRefsSameIdentity ref) reservedRefs =
      let (copiedRef, nextGenerator) =
            freshenTypeBinderRef ref generator
       in ( copiedRef
          , ( copiedRef : reservedRefs
            , nextGenerator
            , copies ++ [(ref, copiedRef)]
            )
          )
  | otherwise =
      (ref, (ref : reservedRefs, generator, copies))

activeFreshenedTypeRef
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeBinderRef
  -> TypeBinderRef
activeFreshenedTypeRef active ref =
  fromMaybe
    ref
    ( snd
        <$> find
          (typeBinderRefsSameIdentity ref . fst)
          active
    )

enterFreshenedTypeRef
  :: TypeBinderRef
  -> TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
enterFreshenedTypeRef sourceRef targetRef active =
  (sourceRef, targetRef)
    : filter
      (not . typeBinderRefsSameIdentity sourceRef . fst)
      active

freshenExactLambdaBodyScopeCollisions
  :: Map.Map TypeBinderRef ElabType
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> ElabType
  -> ElabType
  -> ExactLambdaConstructionPlan
  -> Either ElabError ExactLambdaConstructionPlan
freshenExactLambdaBodyScopeCollisions
  ambientBindings
  lexicallyBoundBodyRefs
  reservedBodyBinderRefs
  paramTy
  bodySourceTy
  plan
  | null collisionRenames = pure plan
  | otherwise = do
      let bodyInstantiation =
            exactLambdaConstructionBodyInstantiation plan
      certifiedBodyComputationTy <-
        either
          ( \cause ->
              Left
                ( ValidationFailed
                    [ "exact lambda body alpha-copy invalidates its certified computation"
                    , "  checked body source: " ++ show bodySourceTy
                    , "  body source at existing copies: "
                        ++ show bodySourceAtExistingCopies
                    , "  body source at final copies: "
                        ++ show bodySourceAtFinalCopies
                    , "  outer lexical refs: " ++ show outerLexicalRefs
                    , "  collision renames: " ++ show collisionRenames
                    , "  body computation: " ++ show bodyInstantiation
                    , "  expected body endpoint: "
                        ++ show (exactLambdaConstructionBodyType plan)
                    , "  cause: " ++ show cause
                    ]
                )
          )
          pure
          ( TypeCheck.checkInstantiation
              bodyTypeEnv
              bodySourceAtFinalCopies
              bodyInstantiation
          )
      let certifiedBodyTy =
            schemeToType
              ( mkElabSchemeWithRefs
                  (exactLambdaConstructionBodyAbstractions plan)
                  certifiedBodyComputationTy
              )
      unless
        ( exactLambdaEndpointTypesAgree
            certifiedBodyTy
            (exactLambdaConstructionBodyType plan)
        )
        ( Left
            ( ValidationFailed
                [ "exact lambda body alpha-copy reaches a different endpoint"
                , "  checked body source: " ++ show bodySourceTy
                , "  body source at final copies: "
                    ++ show bodySourceAtFinalCopies
                , "  collision renames: " ++ show collisionRenames
                , "  constructed body residual: "
                    ++ show certifiedBodyComputationTy
                , "  constructed body endpoint: " ++ show certifiedBodyTy
                , "  expected body endpoint: "
                    ++ show (exactLambdaConstructionBodyType plan)
                ]
            )
        )
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              composedBodyBinderRenames
          , exactLambdaConstructionResultBinderCopies =
              [ ( renameCollisionRef sourceRef
                , renameCollisionRef targetRef
                )
              | (sourceRef, targetRef) <-
                  exactLambdaConstructionResultBinderCopies plan
              ]
          , exactLambdaConstructionBodyInstantiation =
              bodyInstantiation
          }
  where
    existingBodyBinderRenames =
      exactLambdaConstructionBodyBinderRenames plan
    bodySourceAtExistingCopies =
      alphaRenameTypeBinderScopes
        existingBodyBinderRenames
        bodySourceTy
    bodySourceBinderRefs =
      foldr insertBinderRef []
        (forallBinderRefs bodySourceAtExistingCopies)
    constructionBinders = exactLambdaConstructionBinders plan
    outerLexicalRefs =
      map fst constructionBinders
        ++ map fst (exactLambdaConstructionBodyAbstractions plan)
        ++ freeTypeVarRefsType paramTy
        ++ freeTypeVarRefsType
          (exactLambdaConstructionPublishedType plan)
        ++ concatMap freeTypeVarRefsType (Map.elems ambientBindings)
        ++ concatMap
          (maybe [] (freeTypeVarRefsType . tyToElab) . snd)
          constructionBinders
    collidingBodyBinderRefs =
      [ bodyRef
      | bodyRef <- bodySourceBinderRefs
      , not
          ( typeBinderIdentityIsCanonicalStructural
              (typeBinderRefIdentity bodyRef)
          )
      , any
          (typeBinderRefsSameIdentity bodyRef)
          lexicallyBoundBodyRefs
      , any (typeBinderRefsSameIdentity bodyRef) outerLexicalRefs
      ]
    insertBinderRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs
    forallBinderRefs ty =
      case ty of
        TBottom -> []
        TBaseWithIdentity _ _ -> []
        TVarRef _ -> []
        TArrow domain codomain ->
          forallBinderRefs domain ++ forallBinderRefs codomain
        TConWithIdentity _ _ arguments ->
          concatMap forallBinderRefs arguments
        TVarAppRef _ arguments ->
          concatMap forallBinderRefs arguments
        TForallRef ref mbBound body ->
          maybe [] (forallBinderRefs . tyToElab) mbBound
            ++ (ref : forallBinderRefs body)
        TMuRef _ body -> forallBinderRefs body
    identitySeedType =
      foldr
        TArrow
        ( TArrow
            bodySourceAtExistingCopies
            (exactLambdaConstructionPublishedType plan)
        )
        ( map TVarRef reservedBodyBinderRefs
            ++ [paramTy]
            ++ Map.elems ambientBindings
              ++ map (maybe TBottom tyToElab . snd) constructionBinders
        )
    (collisionRenames, _) =
      foldl
        freshenCollision
        ([], identityGeneratorAfterType identitySeedType)
        collidingBodyBinderRefs
    freshenCollision (renames, generator) sourceRef =
      let (freshRef, nextGenerator) =
            freshenTypeBinderRef sourceRef generator
       in (renames ++ [(sourceRef, freshRef)], nextGenerator)
    renameCollisionRef ref =
      fromMaybe
        ref
        ( snd
            <$> find
              (typeBinderRefsSameIdentity ref . fst)
              collisionRenames
        )
    composedExistingRenames =
      [ (sourceRef, renameCollisionRef targetRef)
      | (sourceRef, targetRef) <- existingBodyBinderRenames
      ]
    copiedExistingTargets = map snd existingBodyBinderRenames
    additionalCollisionRenames =
      [ rename
      | rename@(sourceRef, _) <- collisionRenames
      , not
          ( any
              (typeBinderRefsSameIdentity sourceRef)
              copiedExistingTargets
          )
      ]
    composedBodyBinderRenames =
      composedExistingRenames ++ additionalCollisionRenames
    bodySourceAtFinalCopies =
      alphaRenameTypeBinderScopes
        collisionRenames
        bodySourceAtExistingCopies
    bodyTypeEnv =
      foldr
        ( \(ref, mbBound) env ->
            TypeCheck.insertTypeBindingRef
              ref
              (maybe TBottom tyToElab mbBound)
              env
        )
        (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] ambientBindings)
        ( constructionBinders
            ++ exactLambdaConstructionBodyAbstractions plan
        )

-- | Construct a lambda directly at an inherited exact endpoint.  The endpoint
-- has already been certified by a source annotation or by projection from an
-- enclosing exact construction; unlike 'certifyExactLambdaConstruction', this
-- path therefore needs no packet-local result-completion evidence.  It still
-- constructs and checks both computations that Figure 15.3.5 requires: the
-- body source reaches the exact codomain, and the locally emitted
-- @Lambda(Gamma)@ spine reaches the published lambda type.
--
-- This is deliberately a downward construction.  In particular, a quantified
-- codomain inherited from an enclosing lambda is handed to the child lambda
-- before that child is built, so the child emits its own type abstractions;
-- the parent never tries to move those quantifiers across an already-built
-- arrow afterwards.
certifyExactLambdaEndpointConstruction
  :: Map.Map TypeBinderRef ElabType
  -> [TypeBinderRef]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaEndpointConstruction
  ambientBindings
  reservedBodyBinderRefs
  rawCandidates
  paramTy
  bodySourceTy
  expectedTy =
    certifyExactLambdaEndpointConstructionWithCopies
      ambientBindings
      reservedBodyBinderRefs
      []
      rawCandidates
      paramTy
      bodySourceTy
      expectedTy

-- | Variant for an enclosing constructor that allocated a second lexical
-- presentation of nested declarations before entering this lambda.  The
-- source-to-copy routes are construction authority; this function validates
-- and consumes them instead of inferring a correspondence from the final
-- alpha-equivalent types.
certifyExactLambdaEndpointConstructionWithCopies
  :: Map.Map TypeBinderRef ElabType
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaEndpointConstructionWithCopies
  ambientBindings
  reservedBodyBinderRefs
  inheritedBodyBinderCopies
  rawCandidates
  paramTy
  bodySourceTy
  expectedTy =
    certifyLambdaAtExpectedEndpoint
      ambientBindings
      rawCandidates
      paramTy
      expectedTy
      ( \constructionCandidates lambdaTy ->
        certifyExactLambdaEndpointConstructionAtLambdaType
          ambientBindings
          reservedBodyBinderRefs
          inheritedBodyBinderCopies
          constructionCandidates
          paramTy
          bodySourceTy
          lambdaTy
      )

-- | Certify an endpoint whose published type is already a value-lambda type.
-- The public wrapper above separately handles the paper's terminal @Hyp@
-- publication at an ambient bounded variable.  Keeping the two steps
-- separate prevents the ambient declaration from being mistaken for a local
-- member of this lambda's @Lambda(Gamma)@ spine.
certifyExactLambdaEndpointConstructionAtLambdaType
  :: Map.Map TypeBinderRef ElabType
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaEndpointConstructionAtLambdaType
  ambientBindings
  reservedBodyBinderRefs
  inheritedBodyBinderCopies0
  rawCandidates
  paramTy
  bodySourceTy
  expectedTy = do
    ensureDistinct "construction" (map fst constructionCandidates)
    ensureDistinct "endpoint" (map fst expectedBinders)
    (rawExpectedParamTy, expectedBodyTy) <-
      case schemeBody expectedScheme of
        TArrow expectedParamTy bodyTy
          | exactLambdaEndpointTypesAgree expectedParamTy paramTy ->
              pure (expectedParamTy, bodyTy)
        body ->
          constructionFailure
            "the inherited exact endpoint is not this lambda"
            ["  endpoint body: " ++ show body]
    let expectedBoundDeclarationRefs =
          [ boundRef
          | (_, Just expectedBound) <- expectedBinders
          , boundRef <- typeBinderDeclarationRefs (tyToElab expectedBound)
          ]
        -- A forall in the parameter and an alpha-equivalent forall nested in
        -- an enclosing result bound are different lexical declarations.  The
        -- paper's @forall (alpha >= sigma-id). sigma-id -> alpha@ endpoint is
        -- the minimal example: retaining the source @sigma-id@ identity in
        -- both positions makes the endpoint ill-scoped once it is embedded in
        -- a larger construction.  Keep the bound occurrence as the endpoint
        -- authority and construct a fresh sibling copy for the parameter.
        parameterCollisionRefs =
          distinctRefs
            [ parameterRef
            | parameterRef <- typeBinderDeclarationRefs paramTy
            , any
                (typeBinderRefsSameIdentity parameterRef . fst)
                expectedBinders
                || any
                  (typeBinderRefsSameIdentity parameterRef)
                  expectedBoundDeclarationRefs
            ]
        (parameterBinderCopies, _) =
          foldl
            freshenSourceBinder
            ([], identityGeneratorAfterType (TArrow paramTy expectedTy))
            parameterCollisionRefs
        paramTyAtLexicalCopies =
          alphaRenameTypeBinderScopes parameterBinderCopies paramTy
        expectedParamTy =
          alphaRenameTypeBinderScopes
            parameterBinderCopies
            rawExpectedParamTy
        expectedTyAtLexicalCopies =
          schemeToType
            ( mkElabSchemeWithRefs
                expectedBinders
                (TArrow expectedParamTy expectedBodyTy)
            )
        outerLexicalRefs =
          map fst expectedBinders
            ++ concatMap
              ( maybe
                  []
                  ( \bound ->
                      typeBinderDeclarationRefs (tyToElab bound)
                        ++ freeTypeVarRefsType (tyToElab bound)
                  )
                  . snd
              )
              expectedBinders
            ++ typeBinderDeclarationRefs expectedParamTy
            ++ freeTypeVarRefsType expectedParamTy
        sharedNestedDeclarationRefs =
          [ bodyRef
          | bodyRef <- typeBinderDeclarationRefs expectedBodyTy
          , any
              (typeBinderRefsSameIdentity bodyRef)
              outerLexicalRefs
          ]
        bodySourceTyAtParameterCopies =
          alphaRenameTypeBinderScopes
            parameterBinderCopies
            bodySourceTy
        sourceBinderRefs =
          distinctRefs
            (typeBinderDeclarationRefs bodySourceTyAtParameterCopies)
        targetFreeRefs = freeTypeVarRefsType expectedBodyTy
        collidingSourceBinderRefs =
          [ sourceRef
          | sourceRef <- sourceBinderRefs
          , any
              (typeBinderRefsSameIdentity sourceRef)
              targetFreeRefs
          ]
        (bodyBinderRenames, _) =
          foldl
            freshenSourceBinder
            ( [],
              identityGeneratorAfterType
                ( foldr
                    TArrow
                    (TArrow bodySourceTyAtParameterCopies expectedTyAtLexicalCopies)
                    (map TVarRef endpointReservedRefs)
                )
            )
            collidingSourceBinderRefs
        freshBodySourceTy =
          alphaRenameTypeBinderScopes
            bodyBinderRenames
            bodySourceTyAtParameterCopies
        renameBodySourceRef sourceRef =
          fromMaybe
            sourceRef
            ( snd
                <$> find
                  (typeBinderRefsSameIdentity sourceRef . fst)
                  bodyBinderRenames
            )
        inheritedBodyBinderCopies =
          [ (renameBodySourceRef sourceRef, targetRef)
          | (sourceRef, targetRef) <- inheritedBodyBinderCopies0
          ]
        constructionTypeEnv =
          foldr
            ( \(ref, mbBound) env ->
                TypeCheck.insertTypeBindingRef
                  ref
                  (maybe TBottom tyToElab mbBound)
                  env
            )
            (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] ambientBindings)
            constructionCandidates
    pendingInheritedBodyBinderCopies <-
      validateInheritedBodyBinderCopies
        inheritedBodyBinderCopies
        freshBodySourceTy
        expectedBodyTy
    let bodySourceTyAtInheritedCopies =
          alphaRenameTypeBinderScopes
            pendingInheritedBodyBinderCopies
            freshBodySourceTy
    (bodyAbstractions, lexicalBodyBinderCopies, bodyInstantiation0) <-
      case
          if null sharedNestedDeclarationRefs
            then Nothing
            else
              constructLexicalForallCopyInstantiation
                constructionTypeEnv
                exactLambdaEndpointTypesAgree
                bodySourceTyAtInheritedCopies
                expectedBodyTy
        of
          Just (copies, instantiation) ->
            pure ([], copies, instantiation)
          Nothing -> do
            -- The dedicated copy constructor changes one complete leading
            -- forall spine.  A body that must first introduce or specialize
            -- other endpoint structure is constructed normally here; the
            -- owner-final collision pass below then alpha-copies its lexical
            -- body declarations against the completed outer Gamma and records
            -- those routes on the plan.
            instantiation <-
              requireMaybe
                "the checked body cannot construct the inherited exact codomain"
                ( constructExactInstantiation
                    constructionTypeEnv
                    exactLambdaEndpointTypesAgree
                    bodySourceTyAtInheritedCopies
                    expectedBodyTy
                )
            if instantiationIntroducesFreshBinder instantiation
              then
                case
                    namedBodyIntroductionConstruction
                      constructionTypeEnv
                      inheritedBodyBinderCopies
                      bodySourceTyAtInheritedCopies
                      expectedBodyTy
                  of
                  Just (abstractions, namedInstantiation) ->
                    pure (abstractions, [], namedInstantiation)
                  Nothing -> pure ([], [], instantiation)
              else pure ([], [], instantiation)
    let bodySourceTyAtLexicalCopies =
          alphaRenameTypeBinderScopes
            lexicalBodyBinderCopies
            bodySourceTyAtInheritedCopies
        bodyComputationTypeEnv =
          foldl
            ( \env (ref, mbBound) ->
                TypeCheck.insertTypeBindingRef
                  ref
                  (maybe TBottom tyToElab mbBound)
                  env
            )
            constructionTypeEnv
            bodyAbstractions
    certifiedBodyComputationTy <-
      either
        ( \cause ->
            constructionFailure
              "the inherited exact body computation does not typecheck"
              ["  cause: " ++ show cause]
        )
        pure
        ( TypeCheck.checkInstantiation
            bodyComputationTypeEnv
            bodySourceTyAtLexicalCopies
            bodyInstantiation0
        )
    let certifiedBodyTy =
          schemeToType
            ( mkElabSchemeWithRefs
                bodyAbstractions
                certifiedBodyComputationTy
            )
    unless
      (exactLambdaEndpointTypesAgree certifiedBodyTy expectedBodyTy)
      ( constructionFailure
          "the inherited exact body computation reaches a different codomain"
          ["  constructed codomain: " ++ show certifiedBodyTy]
      )
    lexicalResultBinderCopies <-
      if null lexicalBodyBinderCopies
        then pure []
        else do
          let sourceBinders =
                schemeBinderRefs
                  (schemeFromType bodySourceTyAtLexicalCopies)
              copiedBinders =
                schemeBinderRefs (schemeFromType certifiedBodyTy)
              copyPairs = zip sourceBinders copiedBinders
          unless
            ( length sourceBinders == length copiedBinders
                && all
                  ( \((sourceRef, _), (copiedRef, _)) ->
                      not
                        ( typeBinderRefsSameIdentity
                            sourceRef
                            copiedRef
                        )
                  )
                  copyPairs
            )
            ( constructionFailure
                "the lexical forall computation did not allocate one fresh copy per source declaration"
                [ "  source binders: " ++ show sourceBinders
                , "  copied binders: " ++ show copiedBinders
                , "  computation: " ++ show bodyInstantiation0
                ]
            )
          pure
            [ (sourceRef, copiedRef)
            | ((sourceRef, _), (copiedRef, _)) <- copyPairs
            ]
    let expectedBodyTyForConstruction
          | null lexicalBodyBinderCopies = expectedBodyTy
          | otherwise = certifiedBodyTy
        expectedTyForConstruction =
          schemeToType
            ( mkElabSchemeWithRefs
                expectedBinders
                (TArrow expectedParamTy expectedBodyTyForConstruction)
            )
        constructionTy0 =
          schemeToType
            ( mkElabSchemeWithRefs
                constructionCandidates
                (TArrow paramTyAtLexicalCopies expectedBodyTyForConstruction)
            )
    spinePlan <-
      requireMaybe
        "the local lambda Gamma cannot construct the inherited exact endpoint"
        ( planExactBinderSpine
            exactLambdaEndpointTypesAgree
            constructionTy0
            expectedTyForConstruction
        )
    let binderRenames = exactBinderSpineRenames spinePlan
        alignType = renameTypeBinderRefPayloads binderRenames
        alignRef ref =
          case
              find
                (typeBinderRefsSameIdentity ref . fst)
                binderRenames
            of
              Just (_, targetRef) -> targetRef
              Nothing -> ref
        constructionBinders =
          [ ( alignRef ref
            , fmap
                (renameBoundTypeBinderRefPayloads binderRenames)
                mbBound
            )
          | (ref, mbBound) <- constructionCandidates
          ]
        alignedParamTy = alignType paramTyAtLexicalCopies
        alignedBodyTy = alignType expectedBodyTyForConstruction
        alignedPublishedTy = alignType expectedTyForConstruction
        alignedConstructionTy =
          schemeToType
            ( mkElabSchemeWithRefs
                constructionBinders
                (TArrow alignedParamTy alignedBodyTy)
            )
        completionInstantiation =
          exactBinderSpineInstantiation spinePlan
        bodyInstantiation =
          foldl
            ( \inst (sourceRef, targetRef) ->
                renameInstBoundRef sourceRef targetRef inst
            )
            bodyInstantiation0
            binderRenames
    completedTy <-
      either
        ( \cause ->
            constructionFailure
              "the exact lambda binder-spine computation does not apply"
              ["  cause: " ++ show cause]
        )
        pure
        (applyInstantiation alignedConstructionTy completionInstantiation)
    unless
      (exactLambdaEndpointTypesAgree completedTy alignedPublishedTy)
      ( constructionFailure
          "the exact lambda binder-spine reaches a different endpoint"
          ["  constructed endpoint: " ++ show completedTy]
      )
    let publishedBinders =
          schemeBinderRefs (schemeFromType alignedPublishedTy)
        directPublishedConstructionTy =
          schemeToType
            ( mkElabSchemeWithRefs
                publishedBinders
                (TArrow alignedParamTy alignedBodyTy)
            )
        constructsPublishedEndpointDirectly =
          exactLambdaEndpointTypesAgree
            directPublishedConstructionTy
            alignedPublishedTy
    freshenExactLambdaBodyScopeCollisions
      ambientBindings
      reservedBodyBinderRefs
      endpointReservedRefs
      paramTy
      bodySourceTy
      ExactLambdaConstructionPlan
        { exactLambdaConstructionBinders =
            if constructsPublishedEndpointDirectly
              then publishedBinders
              else constructionBinders
        , exactLambdaConstructionPublishedBinders = publishedBinders
        , exactLambdaConstructionPublishedType = alignedPublishedTy
        , exactLambdaConstructionBinderRenames = binderRenames
        , exactLambdaConstructionParameterBinderCopies =
            parameterBinderCopies
        , exactLambdaConstructionBodyBinderRenames =
            parameterBinderCopies
              ++ bodyBinderRenames
              ++ inheritedBodyBinderCopies
              ++ lexicalBodyBinderCopies
        , exactLambdaConstructionResultBinderCopies =
            [ (alignRef sourceRef, alignRef copiedRef)
            | (sourceRef, copiedRef) <-
                parameterBinderCopies
                  ++ inheritedBodyBinderCopies
                  ++ lexicalResultBinderCopies
                  ++ lexicalBodyBinderCopies
            ]
        , exactLambdaConstructionBodyAbstractions = bodyAbstractions
        , exactLambdaConstructionBodyInstantiation = bodyInstantiation
        , exactLambdaConstructionBodyType = alignedBodyTy
        , exactLambdaConstructionPreservedBodyRefinements = []
        , exactLambdaConstructionAmbientBodyRefinement = Nothing
        , exactLambdaConstructionIntroducedAmbientBodyDeclaration = Nothing
        , exactLambdaConstructionAmbientBodyRefinementCertificate = Nothing
        , exactLambdaConstructionPacketBodyProjection = Nothing
        , exactLambdaConstructionCompletionInstantiation =
            if constructsPublishedEndpointDirectly
              then InstId
              else completionInstantiation
        }
  where
    expectedScheme = schemeFromType expectedTy
    expectedBinders = schemeBinderRefs expectedScheme
    endpointReservedRefs =
      foldr insertReservedRef []
        ( reservedBodyBinderRefs
            ++ Map.keys ambientBindings
            ++ map fst rawCandidates
            ++ map fst expectedBinders
        )
    insertReservedRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs
    -- The inherited exact endpoint is the positive declaration authority for
    -- its leading Lambda(Gamma) spine.  Those binders can be absent from the
    -- graph-derived candidates precisely when this child lambda is where a
    -- parent-projected quantifier first becomes lexical.  Seed them here, then
    -- retain any additional graph candidates so the exact spine planner must
    -- still prove every required specialization.
    constructionCandidates =
      expectedBinders
        ++ [ candidate
           | candidate@(candidateRef, _) <- rawCandidates
           , not
               ( any
                   (typeBinderRefsSameIdentity candidateRef . fst)
                   expectedBinders
               )
           ]

    freshenSourceBinder (renames, generator) sourceRef =
      if typeBinderIdentityIsCanonicalStructural
          (typeBinderRefIdentity sourceRef)
        then (renames, generator)
        else
          let (freshRef, nextGenerator) =
                freshenTypeBinderRef sourceRef generator
           in (renames ++ [(sourceRef, freshRef)], nextGenerator)

    validateInheritedBodyBinderCopies copies sourceTy targetTy = do
      let sourceDeclarationRefs = typeBinderDeclarationRefs sourceTy
          targetDeclarationRefs = typeBinderDeclarationRefs targetTy
          parameterDeclarationRefs = typeBinderDeclarationRefs paramTy
          duplicateSources = duplicateRefs (map fst copies)
          duplicateTargets = duplicateRefs (map snd copies)
          invalidCopies =
            [ copy
            | copy@(sourceRef, targetRef) <- copies
            , typeBinderRefsSameIdentity sourceRef targetRef
                || not
                  ( any
                      (typeBinderRefsSameIdentity targetRef)
                      targetDeclarationRefs
                  )
                || not
                  ( any
                      (typeBinderRefsSameIdentity sourceRef)
                      sourceDeclarationRefs
                      || any
                        (typeBinderRefsSameIdentity targetRef)
                        sourceDeclarationRefs
                      || any
                        (typeBinderRefsSameIdentity sourceRef)
                        parameterDeclarationRefs
                  )
            ]
          pendingCopies =
            [ copy
            | copy@(sourceRef, targetRef) <- copies
            , any
                (typeBinderRefsSameIdentity sourceRef)
                sourceDeclarationRefs
            , not
                ( any
                    (typeBinderRefsSameIdentity targetRef)
                    sourceDeclarationRefs
                )
            ]
      unless
        ( null duplicateSources
            && null duplicateTargets
            && null invalidCopies
        )
        ( constructionFailure
            "the inherited lexical-copy certificate is not valid at this body boundary"
            [ "  inherited copies: " ++ show copies
            , "  duplicate sources: " ++ show duplicateSources
            , "  duplicate targets: " ++ show duplicateTargets
            , "  invalid copies: " ++ show invalidCopies
            , "  body source declarations: " ++ show sourceDeclarationRefs
            , "  parameter declarations: " ++ show parameterDeclarationRefs
            , "  expected body declarations: " ++ show targetDeclarationRefs
            ]
        )
      pure pendingCopies

    -- An inherited endpoint fixes the identities of every forall it exposes.
    -- The xMLF O computation ('InstIntro') only promises a fresh declaration;
    -- its evaluator therefore cannot construct a particular preallocated
    -- 'TypeBinderRef'.  Move the shortest sufficient leading target prefix to
    -- explicit 'ETyAbsRef' construction and retain only a computation which
    -- introduces no anonymous binder.  Any remaining presentation difference
    -- must be exactly the one named by the inherited lexical-copy certificate;
    -- the term constructor publishes that residual presentation under these
    -- explicit abstractions.
    namedBodyIntroductionConstruction
      constructionTypeEnv
      inheritedBodyBinderCopies
      sourceTy
      targetTy =
      firstNamedConstruction (leadingForallPrefixes targetTy)
      where
        firstNamedConstruction [] = Nothing
        firstNamedConstruction ((abstractions, residualTy) : rest) =
          case candidate abstractions residualTy of
            Just construction -> Just construction
            Nothing -> firstNamedConstruction rest

        candidate abstractions residualTy = do
          guard (not (null abstractions))
          let abstractionEnv =
                foldl
                  ( \env (ref, mbBound) ->
                      TypeCheck.insertTypeBindingRef
                        ref
                        (maybe TBottom tyToElab mbBound)
                        env
                  )
                  constructionTypeEnv
                  abstractions
          instantiation <-
            constructExactInstantiation
              abstractionEnv
              exactLambdaEndpointTypesAgree
              sourceTy
              residualTy
          guard (not (instantiationIntroducesFreshBinder instantiation))
          constructedTy <-
            either
              (const Nothing)
              Just
              ( TypeCheck.checkInstantiation
                  abstractionEnv
                  sourceTy
                  instantiation
              )
          guard
            ( constructedTy == residualTy
                || alphaRenameTypeBinderScopes
                  inheritedBodyBinderCopies
                  constructedTy
                  == residualTy
            )
          pure (abstractions, instantiation)

        leadingForallPrefixes = go []
          where
            go prefix (TForallRef ref mbBound body) =
              let prefix' = prefix ++ [(ref, mbBound)]
               in (prefix', body) : go prefix' body
            go _ _ = []

    ensureDistinct
      :: String
      -> [TypeBinderRef]
      -> Either ElabError ()
    ensureDistinct role refs =
      case duplicateRefs refs of
        [] -> pure ()
        duplicates ->
          constructionFailure
            (role ++ " binder spine repeats an identity")
            ["  duplicates: " ++ show duplicates]

    duplicateRefs :: [TypeBinderRef] -> [TypeBinderRef]
    duplicateRefs refs =
      [ ref
      | (index, ref) <- zip [0 :: Int ..] refs
      , any (typeBinderRefsSameIdentity ref) (drop (index + 1) refs)
      ]

    distinctRefs = foldl insertRef []
      where
        insertRef refs ref
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = refs ++ [ref]

    requireMaybe :: String -> Maybe a -> Either ElabError a
    requireMaybe detail =
      maybe
        (constructionFailure detail [])
        pure

    constructionFailure :: String -> [String] -> Either ElabError a
    constructionFailure detail extras =
      Left
        ( ValidationFailed
            ( [ "invalid inherited exact lambda construction"
              , "  detail: " ++ detail
              , "  expected type: " ++ show expectedTy
              , "  endpoint candidates: " ++ show expectedBinders
              , "  graph candidates: " ++ show rawCandidates
              , "  parameter: " ++ show paramTy
              , "  checked body source: " ++ show bodySourceTy
              ]
                ++ extras
            )
        )

-- | Certify and construct the exact lambda Gamma selected by an enclosing
-- prepared packet.  Candidate and expected declarations are compared only
-- after both sides have entered the final construction identity domain.  At
-- each leading candidate declaration, the complete expected type decides
-- whether the declaration is retained (and alpha-renamed to the next expected
-- declaration) or consumed by an exact xMLF specialization.  Retained
-- declarations are traversed with 'InstUnder'; consumed declarations use the
-- checked leading application/N computation.  This is the explicit
-- construction recorded by the paper, not a term-shape repair.
--
-- Once the binder spine is aligned, the body either already has the expected
-- codomain or reaches an exact bounded result binder by the paper's terminal
-- @Hyp@ computation.  The enclosing packet is prepared before the body is
-- checked, so its exact result declaration may still be unbounded.  When that
-- declaration is the published lambda codomain, materialize its bound from
-- the checked body before planning the spine.  No leading-forall elimination
-- is inferred from a type-shaped peer.  A bounded packet-body declaration is
-- different: its exact identity and bound are the frozen edge authority, so
-- the planner may construct the certified body-to-bound specialization before
-- the terminal Hyp.
certifyExactLambdaConstruction
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> Map.Map TypeBinderRef ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Maybe CertifiedLambdaBodyConstruction
  -> [TypeBinderRef]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> Instantiation
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaConstruction
  representative
  sourceSidecarRefs
  constructionIdentityRoutes
  owner
  packet
  ambientBindings0
  bodyRefinements
  certifiedBodyConstruction
  reservedBodyBinderRefs
  rawCandidates
  paramTy
  bodySourceTy
  checkedBodyInstantiation
  bodyResultTy
  expectedTy =
    certifyExactLambdaConstructionWithEndpointPlan
      representative
      sourceSidecarRefs
      constructionIdentityRoutes
      owner
      packet
      ambientBindings0
      bodyRefinements
      certifiedBodyConstruction
      reservedBodyBinderRefs
      rawCandidates
      paramTy
      bodySourceTy
      checkedBodyInstantiation
      bodyResultTy
      Nothing
      expectedTy

-- | Reuse an independently certified inherited-endpoint construction as one
-- candidate inside packet-aware planning.  The packet planner still consumes
-- every body refinement and revalidates both the construction and publication
-- schemes; the supplied plan contributes only its already checked lexical-copy
-- construction.  This joins the two positive authorities when a packet was
-- frozen before the checked body allocated its final forall presentation.
certifyExactLambdaConstructionWithEndpointPlan
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> Map.Map TypeBinderRef ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Maybe CertifiedLambdaBodyConstruction
  -> [TypeBinderRef]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> Instantiation
  -> ElabType
  -> Maybe ExactLambdaConstructionPlan
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaConstructionWithEndpointPlan
  representative
  sourceSidecarRefs
  constructionIdentityRoutes
  owner
  packet
  ambientBindings0
  bodyRefinements
  certifiedBodyConstruction
  reservedBodyBinderRefs
  rawCandidates
  paramTy
  bodySourceTy
  checkedBodyInstantiation
  bodyResultTy
  endpointPlan
  expectedTy =
    certifyLambdaAtExpectedEndpoint
      ambientBindings0
      rawCandidates
      paramTy
      expectedTy
      ( \constructionCandidates lambdaTy ->
        certifyExactLambdaConstructionAtLambdaType
          representative
          sourceSidecarRefs
          constructionIdentityRoutes
          owner
          packet
          ambientBindings0
          bodyRefinements
          certifiedBodyConstruction
          reservedBodyBinderRefs
          constructionCandidates
          paramTy
          bodySourceTy
          checkedBodyInstantiation
          bodyResultTy
          endpointPlan
          lambdaTy
      )

-- | Packet-aware lambda construction once the endpoint has been exposed as
-- a value-lambda type.  Ambient flexible publication is handled by the
-- wrapper above so the declaration remains in the enclosing Gamma and the
-- returned plan contains the terminal @Hyp@ step explicitly.
certifyExactLambdaConstructionAtLambdaType
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> Map.Map TypeBinderRef ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Maybe CertifiedLambdaBodyConstruction
  -> [TypeBinderRef]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> Instantiation
  -> ElabType
  -> Maybe ExactLambdaConstructionPlan
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaConstructionAtLambdaType
  representative
  sourceSidecarRefs
  constructionIdentityRoutes
  owner
  packet
  ambientBindings0
  bodyRefinements
  certifiedBodyConstruction
  reservedBodyBinderRefs
  rawCandidates
  paramTy
  bodySourceTy
  checkedBodyInstantiation
  bodyResultTy
  endpointPlan
  expectedTy = do
  case candidateOrdering of
    Right _ -> pure ()
    Left cause ->
      constructionFailure
        "the exact construction Gamma cannot enter the checked source identity domain"
        [ "  cause: " ++ cause
        , "  unordered candidates: "
            ++ show completedConstructionCandidates
        ]
  case introducedAmbientBodyDeclarations of
    [] -> pure ()
    [_] -> pure ()
    declarations ->
      constructionFailure
        "the exact packet introduces more than one ambient body declaration"
        ["  declarations: " ++ show declarations]
  ensureDistinct "construction" (map fst candidates)
  ensureDistinct "enclosing" (map fst expectedBinders)
  ensureDistinct
    "completed descendant endpoint"
    (map fst completedDescendantEndpointBindings)
  case
      map exactCandidate (maybeToList certifyParameterOwnedExactEndpoint)
        ++ map exactCandidate
          (maybeToList certifyVacuousSourceOwnedExpectedDeclarations)
        ++ map exactCandidate
          (maybeToList certifyPacketGammaBoundAmbientRefinement)
        ++ map exactCandidate
          (maybeToList certifyCertifiedBodyBinderSpineAcrossLambda)
        ++ map exactCandidate
          (maybeToList certifyBodyBinderSpineAcrossLambda)
        ++ map exactCandidate
          (maybeToList certifySelectedEndpointFromCertifiedBody)
        ++ map exactCandidate
          ( mapMaybe
              ( certifyBodyOption
                  bodySourceTy
                  checkedBodyInstantiation
                  candidates
                  expectedTy
              )
              bodyOptions
          )
        ++ map exactCandidate
          (maybeToList certifyGeneralizedExpectedBodyDeclaration)
        ++ map exactCandidate
          (maybeToList certifyOpenExpectedBodyAbstraction)
        ++ map exactCandidate
          (maybeToList certifyExactBodySourceEndpoint)
        ++ map exactCandidate
          [ plan
          | plan <- maybeToList endpointPlan
          , exactLambdaEndpointTypesAgree
              (exactLambdaConstructionPublishedType plan)
              expectedTy
          ]
        -- Prefer a construction that publishes the selected endpoint
        -- unchanged.  Only after every such branch has been tried may one of
        -- the explicitly certified completion constructors supersede the
        -- provisional packet endpoint.  This ordering prevents a valid exact
        -- plan from being shadowed by an earlier, more specialized sibling
        -- completion.
        ++ map completionCandidate
          (maybeToList certifyReturnedLambdaParameterCompletedEndpoint)
        ++ map completionCandidate
          (maybeToList certifyClosedSourceParameterResultEndpoint)
        ++ map completionCandidate
          (maybeToList certifyAlreadyAbstractedExpectedDeclaration)
        ++ map completionCandidate
          (maybeToList certifyBodyRefinedExpectedDeclaration)
        ++ map completionCandidate
          (maybeToList certifyFinalizedChildAmbientExpectedDeclaration)
        ++ map completionCandidate
          (maybeToList certifyPacketCompletedExpectedDeclaration)
        ++ map completionCandidate
          ( concatMap
              ( \completedExpectedTy ->
                  mapMaybe
                    ( certifyBodyOption
                        bodySourceTy
                        checkedBodyInstantiation
                        candidates
                        completedExpectedTy
                    )
                    bodyOptions
              )
              completedBottomEndpoints
          )
        ++ map completionCandidate
          (mapMaybe certifyCompletedBodyOption bodyOptions)
        ++ map completionCandidate
          (maybeToList certifySourceOpenedPacketBodySpine)
        ++ map completionCandidate
          (maybeToList certifyCompletedPacketBodySpine)
        ++ map completionCandidate
          (maybeToList certifyPendingPacketBodyBinder)
        ++ map completionCandidate
          (mapMaybe certifyCoalescedBodyRefinedExpectedBodyOption bodyOptions)
    of
    plans@(_ : _) -> selectExactExpectedPlan [] plans
    [] ->
      constructionFailure
        "no exact binder-spine and body computation reaches the enclosing Gamma"
        [ "  candidates: " ++ show candidates
        , "  parameter: " ++ show paramTy
        , "  checked body source/result: "
            ++ show (bodySourceTy, bodyResultTy)
        , "  certified body construction: "
            ++ show certifiedBodyConstruction
        , "  packet construction binders: "
            ++ show packetConstructionBinders
        , "  direct checked-body plan: "
            ++ show
              ( certifyBodyOption
                  bodySourceTy
                  checkedBodyInstantiation
                  candidates
                  expectedTy
                  (InstId, bodyResultTy, ambientPublishedBodyRefinement, [])
              )
        , "  expected binders: " ++ show expectedBinders
        , "  prepared packet binders: "
            ++ show
              ( schemeBinderRefs
                  ( siScheme
                      (subtermGeneralizationSchemeInfo packet)
                  )
              )
        , "  direct body refinement/environment: "
            ++ show
              ( ambientPublishedBodyRefinement
              , refinedAmbientBindingsForBody
                  ambientPublishedBodyRefinement
              )
        , "  direct body instantiation: "
            ++ show
              ( do
                  bodyAmbientBindings <-
                    refinedAmbientBindingsForBody
                      ambientPublishedBodyRefinement
                  pure
                    ( TypeCheck.checkInstantiation
                        ( constructionTypeEnvForAmbient
                            bodyAmbientBindings
                            candidates
                        )
                        bodySourceTy
                        checkedBodyInstantiation
                    )
              )
        , "  direct binder-spine plan: "
            ++ show
              ( planExactBinderSpine
                  exactLambdaEndpointTypesAgree
                  (constructionType candidates paramTy bodyResultTy)
                  expectedTy
              )
        , "  direct construction type: "
            ++ show (constructionType candidates paramTy bodyResultTy)
        , "  direct construction agrees: "
            ++ show
              ( exactLambdaEndpointTypesAgree
                  (constructionType candidates paramTy bodyResultTy)
                  expectedTy
              )
        ]
  where
    selectExactExpectedPlan rejected [] =
      constructionFailure
        "no certified lambda construction publishes the selected exact endpoint"
        [ "  selected endpoint: " ++ show expectedTy
        , "  rejected publication transitions: " ++ show (reverse rejected)
        , "  construction candidates: " ++ show candidates
        , "  packet construction binders: "
            ++ show packetConstructionBinders
        , "  expected binders: " ++ show expectedBinders
        , "  checked body source/result: "
            ++ show (bodySourceTy, bodyResultTy)
        , "  open body-abstraction candidate: "
            ++ show
              ( ( \plan ->
                    ( exactLambdaConstructionBinders plan
                    , exactLambdaConstructionBodyAbstractions plan
                    , exactLambdaConstructionPublishedType plan
                    , exactLambdaConstructionCompletionInstantiation plan
                    )
                )
                  <$> certifyOpenExpectedBodyAbstraction
              )
        ]
    selectExactExpectedPlan rejected ((completesSelectedEndpoint, plan0) : remainingPlans) = do
      plan <- completeConsumedBodyConsumerPlan plan0
      -- The selected endpoint is prepared before the recursively checked
      -- body has completed its consumer declarations.  A plan can therefore
      -- be born exactly at @expectedTy@ and, while replaying the certified
      -- body refinements above, publish the completed form of that same
      -- endpoint.  Requiring the completed form to equal the provisional
      -- input rejects the very construction that proves the transition.
      --
      -- Accept only an unchanged exact publication, a plan whose pre-replay
      -- publication was the selected endpoint, or one of the explicit
      -- completion branches above.  Those branches construct their completed
      -- endpoint from the selected packet identity (pending packet body,
      -- checked descendant completion, or a certified bound refinement), so
      -- the tag records a proof-producing constructor rather than a
      -- same-shaped type comparison.
      if null (duplicateTypeDeclarations plan0)
          && null (duplicateTypeDeclarations plan)
          && ( exactLambdaEndpointTypesAgree
                (exactLambdaConstructionPublishedType plan)
                expectedTy
                || exactLambdaEndpointTypesAgree
                  (exactLambdaConstructionPublishedType plan0)
                  expectedTy
                || completesSelectedEndpoint
             )
        then
          freshenExactLambdaBodyScopeCollisions
            ambientBindings
            reservedBodyBinderRefs
            exactConstructionReservedRefs
            paramTy
            bodySourceTy
            plan
        else
          selectExactExpectedPlan
            ( ( completesSelectedEndpoint
              , exactLambdaConstructionPublishedType plan0
              , exactLambdaConstructionPublishedType plan
              , duplicateTypeDeclarations plan0
              , duplicateTypeDeclarations plan
              , certifiedBodyScopeDependencyRenames
              )
                : rejected
            )
            remainingPlans

    duplicateTypeDeclarations plan =
      [ (leftRef, rightRef)
      | (index, leftRef) <- zip [0 :: Int ..] declarationRefs
      , rightRef <- drop (index + 1) declarationRefs
      , typeBinderRefsSameIdentity leftRef rightRef
      ]
      where
        declarationRefs =
          typeBinderDeclarationRefs
            (exactLambdaConstructionPublishedType plan)

    exactCandidate plan =
      (False, recordCandidateBoundScopeCopies plan)
    completionCandidate plan =
      (True, recordCandidateBoundScopeCopies plan)

    -- Candidate bounds and the recursively checked body share one lexical
    -- source domain.  Record any alpha-copies allocated while those bounds
    -- were constructed, so the bound, checked term, and body certificate
    -- cannot choose different fresh identities later.
    recordCandidateBoundScopeCopies plan =
      plan
        { exactLambdaConstructionBodyBinderRenames =
            existingCopies
              ++ [ copy
                 | copy@(sourceRef, _) <- bodyCandidateBoundScopeRenames
                 , not
                     ( any
                         (typeBinderRefsSameIdentity sourceRef . fst)
                         existingCopies
                     )
                 ]
        , exactLambdaConstructionResultBinderCopies =
            existingResultCopies
              ++ [ copy
                 | copy <- allCandidateBoundScopeRenames
                 , not
                     ( any
                         (sameCandidateScopeCopy copy)
                         existingResultCopies
                     )
                 ]
        , exactLambdaConstructionIntroducedAmbientBodyDeclaration =
            introducedAmbientBodyDeclaration
        }
      where
        existingCopies = exactLambdaConstructionBodyBinderRenames plan
        existingResultCopies =
          exactLambdaConstructionResultBinderCopies plan

        sameCandidateScopeCopy
          (sourceRef, copiedRef)
          (existingSourceRef, existingCopiedRef) =
            typeBinderRefsSameIdentity sourceRef existingSourceRef
              && typeBinderRefsSameIdentity copiedRef existingCopiedRef

    completeConsumedBodyConsumerPlan plan = do
      let planBindings =
            Map.fromList
              [ (ref, maybe TBottom tyToElab mbBound)
              | (ref, mbBound) <- exactLambdaConstructionBinders plan
              ]
          constructionRefinement certificate = do
            let renamedCertificate =
                  renameBodyConsumerBoundRefinementCertificate
                    (exactLambdaConstructionBinderRenames plan)
                    certificate
            -- Candidate bounds are sibling lexical scopes.  If constructing
            -- one of those bounds copied a dependency that also occurs in the
            -- checked body, select the copy owned by this exact declaration
            -- before moving the remaining certificate payload into the body
            -- presentation.  Applying the body copy first would erase the
            -- source identity and later substitute the body's private forall
            -- into a sibling bound, producing two declarations with one
            -- identity.
            candidateAlignedCertificate <-
              consumeBodyConsumerBoundRefinementScopeDependencies
                planBindings
                (exactLambdaConstructionResultBinderCopies plan)
                renamedCertificate
            pure
              ( if
                  certificate
                    `elem` exactLambdaConstructionPreservedBodyRefinements plan
                  then candidateAlignedCertificate
                  else
                    alphaRenameBodyConsumerBoundRefinementCertificate
                      (exactLambdaConstructionBodyBinderRenames plan)
                      candidateAlignedCertificate
              )
      constructionBodyRefinements <-
        traverse constructionRefinement bodyRefinements
      planBodyRefinements <-
        traverse
          ( consumeBodyConsumerBoundRefinementScopeDependencies
              planBindings
              certifiedBodyScopeDependencyRenames
          )
          constructionBodyRefinements
      completedConstructionScheme <-
        consumePlanRefinements
          "construction"
          plan
          planBodyRefinements
          ( mkElabSchemeWithRefs
              (exactLambdaConstructionBinders plan)
              ( TArrow
                  paramTy
                  (exactLambdaConstructionBodyType plan)
              )
          )
      completedPublishedScheme <-
        consumePlanRefinements
          "publication"
          plan
          planBodyRefinements
          ( schemeFromType
              (exactLambdaConstructionPublishedType plan)
          )
      (completedParamTy, completedBodyTy) <-
        case schemeBody completedConstructionScheme of
          TArrow completedParam completedBody ->
            pure (completedParam, completedBody)
          completedBody ->
            constructionFailure
              "consumed exact construction is no longer a lambda"
              [ "  completed construction body: " ++ show completedBody
              , "  original plan: " ++ show plan
              ]
      completedAmbientBindings <-
        case
            refinedAmbientBindingsForBody
              (exactLambdaConstructionAmbientBodyRefinement plan)
          of
          Just bindings -> pure bindings
          Nothing ->
            constructionFailure
              "the consumed exact construction lost its certified ambient body refinement"
              [ "  ambient refinement: "
                  ++ show
                    (exactLambdaConstructionAmbientBodyRefinement plan)
              ]
      unless
        (exactLambdaEndpointTypesAgree completedParamTy paramTy)
        ( constructionFailure
            "consumed exact construction changed the lambda parameter"
            [ "  completed parameter: " ++ show completedParamTy
            , "  original parameter: " ++ show paramTy
            ]
        )
      let completedBinders =
            schemeBinderRefs completedConstructionScheme
          completedPublishedTy =
            schemeToType completedPublishedScheme
          completedConstructionTy =
            schemeToType completedConstructionScheme
          completedTypeEnv =
            foldr
              ( \(ref, mbBound) typeEnv ->
                  TypeCheck.insertTypeBindingRef
                    ref
                    (maybe TBottom tyToElab mbBound)
                    typeEnv
              )
              ( TypeCheck.mkTypeCheckEnvWithResolvedTerms
                  []
                  completedAmbientBindings
              )
              completedBinders
          bodySourceAtPlanCopies =
            alphaRenameTypeBinderScopes
              (exactLambdaConstructionBodyBinderRenames plan)
              bodySourceTy
      (completedBodyAbstractions, completedBodyComputationTy) <-
        matchCompletedBodyAbstractions
          (exactLambdaConstructionBodyAbstractions plan)
          completedBodyTy
      completedBodyInstantiation <-
        retainOrConstructInstantiation
          "consumed exact lambda body"
          completedTypeEnv
          bodySourceAtPlanCopies
          completedBodyComputationTy
          (exactLambdaConstructionBodyInstantiation plan)
      completedCompletionInstantiation <-
        retainOrConstructInstantiation
          "consumed exact lambda publication"
          completedTypeEnv
          completedConstructionTy
          completedPublishedTy
          (exactLambdaConstructionCompletionInstantiation plan)
      pure
        plan
          { exactLambdaConstructionBinders = completedBinders
          , exactLambdaConstructionPublishedBinders =
              schemeBinderRefs completedPublishedScheme
          , exactLambdaConstructionPublishedType = completedPublishedTy
          , exactLambdaConstructionBodyAbstractions =
              completedBodyAbstractions
          , exactLambdaConstructionBodyInstantiation =
              completedBodyInstantiation
          , exactLambdaConstructionBodyType = completedBodyTy
          , exactLambdaConstructionCompletionInstantiation =
              completedCompletionInstantiation
          }

    consumePlanRefinements role plan refinements scheme =
      case
          consumeCertifiedBodyConsumerConstructionScheme
            refinements
            scheme
        of
          Right completed -> pure completed
          Left cause ->
            constructionFailure
              ("cannot consume the exact plan's " ++ role ++ " refinements")
              [ "  plan construction renames: "
                  ++ show (exactLambdaConstructionBinderRenames plan)
              , "  plan body renames: "
                  ++ show (exactLambdaConstructionBodyBinderRenames plan)
              , "  plan preserved refinements: "
                  ++ show
                    (exactLambdaConstructionPreservedBodyRefinements plan)
              , "  aligned refinements: " ++ show refinements
              , "  candidate scheme: " ++ show scheme
              , "  cause: " ++ show cause
              ]

    matchCompletedBodyAbstractions [] ty = pure ([], ty)
    matchCompletedBodyAbstractions
      ((expectedRef, expectedBound) : remaining)
      ty =
        case ty of
          TForallRef actualRef actualBound body
            | typeBinderRefsSameIdentity expectedRef actualRef
            , operationalEndpointTypesAgree
                (maybe TBottom tyToElab expectedBound)
                (maybe TBottom tyToElab actualBound) -> do
                (completedRemaining, residual) <-
                  matchCompletedBodyAbstractions remaining body
                pure
                  ( (actualRef, actualBound) : completedRemaining
                  , residual
                  )
          _ ->
            constructionFailure
              "consumed exact lambda body lost its certified abstractions"
              [ "  expected abstractions: "
                  ++ show ((expectedRef, expectedBound) : remaining)
              , "  completed body: " ++ show ty
              ]

    retainOrConstructInstantiation role typeEnv sourceTy targetTy existing =
      case TypeCheck.checkInstantiation typeEnv sourceTy existing of
        Right constructedTy
          | exactLambdaEndpointTypesAgree constructedTy targetTy ->
              pure existing
        _ ->
          case
              constructExactInstantiation
                typeEnv
                exactLambdaEndpointTypesAgree
                sourceTy
                targetTy
            of
              Just construction -> pure construction
              Nothing ->
                constructionFailure
                  (role ++ " has no exact computation after Gamma consumption")
                  [ "  source: " ++ show sourceTy
                  , "  target: " ++ show targetTy
                  , "  previous computation: " ++ show existing
                  ]

    expectedScheme = schemeFromType expectedTy
    expectedBinders = schemeBinderRefs expectedScheme
    certifiedBodyScopeDependencyRenames =
      maybe
        []
        certifiedLambdaBodyScopeDependencyRenames
        certifiedBodyConstruction
    expectedFreeRefs = freeTypeVarRefsType expectedTy
    installedAmbientBoundEndpointFreeRefs =
      [ freeRef
      | (_, ambientBound) <- Map.toList ambientBindings0
      , exactLambdaEndpointTypesAgree
          expectedTy
          ambientBound
      , freeRef <- expectedFreeRefs
      ]
    -- Exact construction allocates alpha-copies after several independently
    -- prepared source and graph packets have met.  Reserve every identity
    -- already owned by those inputs, including declarations that occur only as
    -- map keys and therefore are invisible to a type-only generator seed.
    exactConstructionReservedRefs =
      foldr insertExactConstructionRef []
        ( reservedBodyBinderRefs
            ++ IntMap.elems sourceSidecarRefs
            ++ bodyConstructionOuterRefs
        )
    insertExactConstructionRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs
    exactConstructionIdentityGenerator extraTypes =
      identityGeneratorAfterType
        ( foldr
            TArrow
            (TArrow bodySourceTy expectedTy)
            ( map TVarRef exactConstructionReservedRefs
                ++ [paramTy, bodyResultTy]
                ++ extraTypes
            )
        )
    -- A graph declaration that occurs free in the inherited exact endpoint
    -- belongs to the enclosing construction Gamma.  Quantifying it again in
    -- this lambda's local Lambda(Gamma) spine would capture the very endpoint
    -- occurrence that selects it.  A pending candidate is already sufficient
    -- authority for the Bottom declaration.  A completed candidate needs the
    -- independent owner-final body-consumer certificate below to prove the
    -- same identity and bound before it can be opened ambiently.  A checked
    -- source-body completion is the other constructive case: the packet owns
    -- the pending declaration, the child certificate owns Typ(body), and the
    -- checked InstAbstr computation installs the completed bound at the exact
    -- free endpoint.
    endpointAmbientCandidates =
      rawEndpointAmbientCandidates ++ packetEndpointAmbientCandidates
    rawEndpointAmbientCandidates =
      [ (expectedRef, candidateRef, ambientBound)
      | (candidateRef, candidateBound) <- rawCandidatesAtCopiedBounds
      , expectedRef <- expectedFreeRefs
      , typeBinderRefsSameIdentity candidateRef expectedRef
      , ambientBound <-
          maybeToList
            ( endpointCandidateAmbientBound
                expectedRef
                candidateBound
            )
      ]
    -- A completed packet declaration can occur free in the installed bound
    -- of a different ambient declaration.  Its prepared construction-order
    -- entry is already a checked Gamma declaration; move that exact entry to
    -- the ambient body scope instead of quantifying it inside the value
    -- lambda, which would capture the selected bound's free occurrence.
    packetEndpointAmbientCandidates =
      [ (expectedRef, packetRef, tyToElab packetBound)
      | expectedRef <- installedAmbientBoundEndpointFreeRefs
      , (packetRef, Just packetBound) <- preparedPacketConstructionBinders
      , typeBinderRefsSameIdentity expectedRef packetRef
      , constructionOwnsPacketBinder packetRef
      , not (sourceOwnsPacketBinder packetRef)
      , not
          ( any
              (typeBinderRefsSameIdentity packetRef)
              (typeBinderDeclarationRefs expectedTy)
          )
      , not
          ( any
              (typeBinderRefsSameIdentity packetRef)
              ( typeBinderDeclarationRefs (tyToElab packetBound)
                  ++ freeTypeVarRefsType (tyToElab packetBound)
              )
          )
      , not
          ( any
              ( \(_, rawCandidateRef, _) ->
                  typeBinderRefsSameIdentity packetRef rawCandidateRef
              )
              rawEndpointAmbientCandidates
          )
      ]
    endpointCandidateAmbientBound expectedRef candidateBound =
      case matchingAmbientBounds of
        []
          | isNothing candidateBound -> Just TBottom
          | Just bound <- candidateBound
          , Just completedBound <-
              certifiedSourceBodyAmbientBound
                expectedRef
                (tyToElab bound) ->
              Just completedBound
          | otherwise -> Nothing
        firstBound : remainingBounds
          | all
              (operationalEndpointTypesAgree firstBound)
              remainingBounds
          , maybe
              True
              ( \bound ->
                  operationalEndpointTypesAgree
                    (tyToElab bound)
                    firstBound
              )
              candidateBound ->
              Just firstBound
        _ -> Nothing
      where
        matchingAmbientBounds =
          [ ambientBound
          | (ambientRef, ambientBound) <-
              completedDescendantEndpointBindings
                ++ Map.toList ambientBindings0
          , typeBinderRefsSameIdentity expectedRef ambientRef
          ]

    certifiedSourceBodyAmbientBound expectedRef candidateBound = do
      bodyConstruction <- certifiedBodyConstruction
      guard (certifiedLambdaBodyOwner bodyConstruction /= owner)
      guard
        ( certifiedConstructionReachesCheckedBody
            bodyConstruction
            bodySourceTy
            && any
              ( \(packetRef, packetBound) ->
                  typeBinderRefsSameIdentity expectedRef packetRef
                    && isNothing packetBound
              )
              packetConstructionBinders
            && operationalEndpointTypesAgree
              candidateBound
              bodySourceAtRawCandidateCopies
        )
      TVarRef bodyResultRef <- pure bodyResultTy
      guard
        (typeBinderRefsSameIdentity expectedRef bodyResultRef)
      constructedBodyTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              ( TypeCheck.mkTypeCheckEnvWithResolvedTerms
                  []
                  ( installAmbientBinding
                      (expectedRef, candidateBound)
                      ambientBindings0
                  )
              )
              bodySourceAtRawCandidateCopies
              checkedBodyInstantiation
          )
      guard
        ( exactLambdaEndpointTypesAgree
            constructedBodyTy
            (TVarRef expectedRef)
        )
      pure candidateBound

    bodySourceAtRawCandidateCopies =
      alphaRenameTypeBinderScopes
        rawBodyCandidateBoundScopeRenames
        bodySourceTy

    introducedAmbientBodyDeclaration =
      case introducedAmbientBodyDeclarations of
        [declaration] -> Just declaration
        _ -> Nothing

    introducedAmbientBodyDeclarations =
      sourceBodyAmbientDeclarations
        ++ [ (expectedRef, completedBound)
           | (expectedRef, _packetRef, completedBound) <-
               packetEndpointAmbientCandidates
           , not
               ( any
                   (typeBinderRefsSameIdentity expectedRef . fst)
                   (Map.toList ambientBindings0)
               )
           ]

    sourceBodyAmbientDeclarations =
      [ (expectedRef, completedBound)
      | (expectedRef, candidateRef, completedBound) <-
          endpointAmbientCandidates
      , not
          ( any
              (typeBinderRefsSameIdentity expectedRef . fst)
              (Map.toList ambientBindings0)
          )
      , (_, Just candidateBound) <-
          [ candidate
          | candidate@(ref, Just _) <- rawCandidatesAtCopiedBounds
          , typeBinderRefsSameIdentity ref candidateRef
          ]
      , Just certifiedBound <-
          [ certifiedSourceBodyAmbientBound
              expectedRef
              (tyToElab candidateBound)
          ]
      , operationalEndpointTypesAgree completedBound certifiedBound
      ]
    ambientBindings =
      foldr
        installAmbientBinding
        ( foldr
            ( \(expectedRef, _, ambientBound) ->
                installAmbientBinding (expectedRef, ambientBound)
            )
            ambientBindings0
            endpointAmbientCandidates
        )
        completedDescendantEndpointBindings
    installAmbientBinding (exactRef, bound) bindings =
      Map.insert
        exactRef
        bound
        ( Map.filterWithKey
            (\ref _ -> not (typeBinderRefsSameIdentity ref exactRef))
            bindings
        )
    -- A recursively checked child can complete a declaration that the exact
    -- enclosing endpoint uses freely.  The child's finalized local-emission
    -- certificate is positive authority for that declaration and bound.  It
    -- lets this lambda alpha-freshen the child's ETyAbs and instantiate it at
    -- the now-ambient declaration, exactly as Figure 15.3.5 places T(e) at
    -- the lambda-body boundary.
    completedDescendantEndpointBindings =
      [ (expectedRef, bcbrCompletedBound certificate)
      | expectedRef <- expectedFreeRefs
      , certificate <- bodyRefinements
      , Just (route, declaredBound) <-
          [finalizedLocalBodyConsumerDeclaration certificate]
      , bcrOwner route /= owner
      , typeBinderRefsSameIdentity
          expectedRef
          (bcbrAmbientRef certificate)
      , typeBinderRefsSameIdentity
          (bcrConstructionRef route)
          (bcbrAmbientRef certificate)
      , operationalEndpointTypesAgree
          declaredBound
          (bcbrCompletedBound certificate)
      , operationalEndpointTypesAgree
          (bcrConstructionOperatedType route)
          (bcbrCompletedBound certificate)
      ]
    localRawCandidates =
      [ candidate
      | candidate@(candidateRef, _) <- rawCandidatesAtCopiedBounds
      , not
          ( any
              ( \(_, ambientCandidateRef, _) ->
                  typeBinderRefsSameIdentity
                    candidateRef
                    ambientCandidateRef
              )
              endpointAmbientCandidates
          )
      ]
    completedConstructionCandidates0 =
      map
        alignRetainedPacketBinderWithFrozenEndpoint
        ( map
            completeCheckedSourceBodyBinder
            ( map
                completePublishedBodyBinder
                ( packetExpectedConstructionCandidates
                    ++ remainingLocalRawCandidates
                )
            )
        )
    ( rawCandidatesAtCopiedBounds
      , rawCandidateBoundScopeCopies
      , candidateBoundScopeGeneratorAfterRaw
      , _candidateBoundReservedRefsAfterRaw
      ) =
        foldl
          freshenCandidateBoundScope
          ( []
          , []
          , candidateBoundScopeIdentityGenerator
          , candidateBoundInitialReservedRefs
          )
          rawCandidates
    ( completedConstructionCandidates
      , candidateBoundScopeCopies
      , _candidateBoundScopeGenerator
      , _candidateBoundReservedRefs
      ) =
        foldl
          freshenCandidateBoundScope
          ( []
          , rawCandidateBoundScopeCopies
          , candidateBoundScopeGeneratorAfterRaw
          , candidateBoundInitialReservedRefs
          )
          completedConstructionCandidates0

    -- Candidate bounds are sibling lexical scopes.  Allocate each nested
    -- declaration while threading the declarations already placed by the
    -- parameter, body result, earlier candidates, and the candidate's own
    -- outer binder.  A flat identity-keyed rename cannot do this: two later
    -- sibling bounds may each need a different copy of the same source
    -- declaration.
    freshenCandidateBoundScope
      (candidates0, copies0, generator0, reservedRefs0)
      candidate@(candidateRef, mbBound) =
        case mbBound of
          Nothing ->
            ( candidates0 ++ [candidate]
            , copies0
            , generator0
            , insertExactConstructionRef candidateRef reservedRefs0
            )
          Just candidateBound ->
            let originalBound = tyToElab candidateBound
                ( copiedBound
                  , (reservedRefs1, generator1, boundCopies)
                  ) =
                    freshenTypeDeclarationScopesInBound
                      []
                      ( insertExactConstructionRef
                          candidateRef
                          reservedRefs0
                      , generator0
                      , []
                      )
                      candidateBound
                copyRecords =
                  [ (candidateRef, originalBound, sourceRef, copiedRef)
                  | (sourceRef, copiedRef) <- boundCopies
                  ]
             in ( candidates0 ++ [(candidateRef, Just copiedBound)]
                , copies0 ++ copyRecords
                , generator1
                , reservedRefs1
                )

    candidateBoundInitialReservedRefs =
      foldr
        insertExactConstructionRef
        []
        ( map fst expectedBinders
            ++ typeBinderDeclarationRefs paramTy
            ++ typeBinderDeclarationRefs bodyResultTy
        )

    candidateBoundCopyAppliesToBody
      candidateRef
      originalBound =
        operationalEndpointTypesAgree originalBound bodySourceTy
          || case bodyResultTy of
            TVarRef bodyResultRef ->
              typeBinderRefsSameIdentity candidateRef bodyResultRef
            _ -> False

    rawBodyCandidateBoundScopeRenames =
      [ (sourceRef, copiedRef)
      | (candidateRef, originalBound, sourceRef, copiedRef) <-
          rawCandidateBoundScopeCopies
      , candidateBoundCopyAppliesToBody candidateRef originalBound
      ]

    bodyCandidateBoundScopeRenames =
      [ (sourceRef, copiedRef)
      | (candidateRef, originalBound, sourceRef, copiedRef) <-
          candidateBoundScopeCopies
      , candidateBoundCopyAppliesToBody candidateRef originalBound
      ]

    allCandidateBoundScopeRenames =
      [ (sourceRef, copiedRef)
      | (_, _, sourceRef, copiedRef) <- candidateBoundScopeCopies
      ]

    candidateBoundScopeIdentityGenerator =
      identityGeneratorAfterType
        ( foldr
            TArrow
            (TArrow bodySourceTy expectedTy)
            ( [paramTy, bodyResultTy]
                ++ Map.elems ambientBindings0
                ++ concatMap candidateSeedTypes rawCandidates
                ++ concatMap candidateSeedTypes packetConstructionBinders
            )
        )

    candidateSeedTypes (ref, mbBound) =
      TVarRef ref : maybe [] (pure . tyToElab) mbBound
    -- The selected exact endpoint fixes the lexical order of every retained
    -- declaration, including source-owned binders that are intentionally
    -- absent from the packet's local-emission order.  Merge ownership sources
    -- first, then project their common identities through that endpoint order;
    -- a dependency-only topological sort cannot decide where an otherwise
    -- independent source binder belongs relative to packet-local binders.
    endpointOrderedConstructionCandidates =
      [ candidate
      | (expectedRef, _) <- expectedBinders
      , candidate@(candidateRef, _) <- completedConstructionCandidates
      , typeBinderRefsSameIdentity expectedRef candidateRef
      ]
        ++ [ candidate
           | candidate@(candidateRef, _) <- completedConstructionCandidates
           , not
               ( any
                   (typeBinderRefsSameIdentity candidateRef . fst)
                   expectedBinders
               )
           ]
    -- The exact packet owns the complete Lambda(Gamma_g) spine.  A binder in
    -- its construction-order certificate can be absent from both the local
    -- candidates and the packet's ordinary scheme spine.  In particular, a
    -- vacuous declaration can remain only in a nested bound/body occurrence;
    -- 'siConstructionBinderOrderRefs' exists precisely so that Phi does not
    -- rediscover or discard such declarations from shape.  Reintroduce the
    -- exact declaration from the selected enclosing endpoint when that
    -- construction-order certificate owns its identity.  Traverse the
    -- selected endpoint spine so recovered declarations retain the order of
    -- the construction actually being certified; reuse the local candidate
    -- when it exists so body-completed bounds are not discarded.  The
    -- endpoint fixes the bound and spine, the order map fixes local-emission
    -- ownership, and the source order independently excludes source ABI
    -- binders.  A pending packet
    -- declaration can also be installed in the checking environment; that
    -- visibility is not ambient ownership.
    packetExpectedConstructionCandidates =
      foldl insertPacketEndpointCandidate []
        leadingPacketEndpointCandidates
    leadingPacketEndpointCandidates =
      [ fromMaybe
          expectedBinder
          ( find
              (typeBinderRefsSameIdentity expectedRef . fst)
              localRawCandidates
          )
      | expectedBinder@(expectedRef, _) <- expectedBinders
      , constructionOwnsPacketBinder expectedRef
      , not (sourceOwnsPacketBinder expectedRef)
      ]
    insertPacketEndpointCandidate accumulated candidate@(candidateRef, _)
      | any
          (typeBinderRefsSameIdentity candidateRef . fst)
          accumulated = accumulated
      | otherwise = accumulated ++ [candidate]
    remainingLocalRawCandidates =
      [ candidate
      | candidate@(candidateRef, _) <- localRawCandidates
      , not
          ( any
              (typeBinderRefsSameIdentity candidateRef . fst)
              packetExpectedConstructionCandidates
          )
      ]
    candidateOrdering = do
      sourceOccurrenceRenames <- exactSourceOccurrenceRenames
      sourceProjectedCandidates <-
        traverse
          (projectCandidateBoundSourceOccurrences sourceOccurrenceRenames)
          selectedConstructionCandidates
      orderSourceProjectedSchemeBinders
        "exact enclosing lambda construction"
        ( mkElabSchemeWithRefs
            sourceProjectedCandidates
            TBottom
        )
    candidates =
      either
        (const selectedConstructionCandidates)
        schemeBinderRefs
        candidateOrdering

    projectCandidateBoundSourceOccurrences _ (ref, Nothing) =
      Right (ref, Nothing)
    projectCandidateBoundSourceOccurrences sourceOccurrenceRenames (ref, Just bound) =
      case
          elabToBound
            ( sourceProjectCheckedBodyType
                ( renameTypeBinderRefPayloads
                    sourceOccurrenceRenames
                    (tyToElab bound)
                )
            )
        of
        Left cause ->
          Left
            ( "source projection produced an invalid exact construction bound: binder="
                ++ show ref
                ++ ", bound="
                ++ show bound
                ++ ", cause="
                ++ show cause
            )
        Right projectedBound ->
          Right (ref, Just projectedBound)

    exactSourceOccurrenceRenames = do
      sourceConstructionRenames <-
        sourceBinderConstructionRenamesRetainingAmbiguousSources
          representative
          checkedSourceSidecarRefs
          constructionIdentityRoutes
      foldM invertSourceConstructionRoute [] sourceConstructionRenames

    checkedSourceSidecarRefs =
      IntMap.filter
        ( \sourceRef ->
            any
              (typeBinderRefsSameIdentity sourceRef)
              checkedBodyDeclarationRefs
        )
        sourceSidecarRefs

    invertSourceConstructionRoute
      renames
      (sourceRef, constructionRef) =
        case
            [ existingSourceRef
            | (existingConstructionRef, existingSourceRef) <- renames
            , typeBinderRefsSameIdentity
                existingConstructionRef
                constructionRef
            ]
          of
          [] -> Right ((constructionRef, sourceRef) : renames)
          existingSources
            | all
                (typeBinderRefsSameIdentity sourceRef)
                existingSources ->
                Right renames
          existingSources ->
            Left
              ( "construction identity has multiple checked source authorities: construction="
                  ++ show constructionRef
                  ++ ", sources="
                  ++ show (sourceRef : existingSources)
              )
    selectedConstructionCandidates =
      case ambientPublishedBodyDeclarationRef of
        Nothing -> endpointOrderedConstructionCandidates
        Just ambientRef ->
          filter
            (not . typeBinderRefsSameIdentity ambientRef . fst)
            endpointOrderedConstructionCandidates
    packetConstructionBinders =
      schemeBinderRefs
        ( siScheme
            ( subtermGeneralizationConsumerConstructionSchemeInfo
                packet
            )
        )
    preparedPacketConstructionBinders =
      schemeBinderRefs
        ( siScheme
            (subtermGeneralizationSchemeInfo packet)
        )

    -- The enclosing packet fixes the source identity and lexical position of
    -- its result before Typ(body) is available.  A checked body computation
    -- that ends at that exact pending identity completes its declaration with
    -- the computation's source type.  The exact binder-spine plan below then
    -- proves how that source declaration is published at the enclosing
    -- codomain; no type-shape or display-name matching participates.
    completePublishedBodyBinder binder@(candidateRef, _)
      | Just pendingBodyRef <- packetBodyBinderRef
      , typeBinderRefsSameIdentity candidateRef pendingBodyRef
      , TVarRef bodyResultRef <- bodyResultTy
      , typeBinderRefsSameIdentity candidateRef bodyResultRef
      , any
          ( \(packetRef, packetBound) ->
              typeBinderRefsSameIdentity candidateRef packetRef
                && isNothing packetBound
          )
          packetConstructionBinders
      , Right checkedBodyBound <- elabToBound bodySourceTy =
          (candidateRef, Just checkedBodyBound)
      | Just completedBound <-
          certifiedBodyRefinementCompletion candidateRef (snd binder) =
          (candidateRef, Just completedBound)
      | Just completedBound <-
          certifiedBodyConstructionCompletion candidateRef =
          (candidateRef, Just completedBound)
      | Just completedBound <-
          certifiedBodyResultOwnerCompletion candidateRef =
          (candidateRef, Just completedBound)
      | Just completedBound <-
          certifiedDescendantCompletion candidateRef =
          (candidateRef, Just completedBound)
      | otherwise = binder

    -- A finalized body-consumer certificate is the positive construction
    -- evidence that a provisional declaration has advanced to its completed
    -- bound.  Prefer that exact transition over an older bound retained in a
    -- recursively checked owner's binder spine.  Identity alone is not
    -- enough: the certificate must target the construction declaration and
    -- accept the candidate's current, authority-recorded state.
    certifiedBodyRefinementCompletion candidateRef candidateBound = do
      [certificate] <-
        pure
          [ refinement
          | refinement <- bodyRefinements
          , bodyConsumerBoundRefinementOwnerFinalized refinement
          , bodyConsumerBoundRefinementAppliesToDeclarationState
              candidateRef
              currentBound
              refinement
          , let route =
                  authorizedBodyConsumerRoute
                    (bcbrDeclarationAuthority refinement)
          , typeBinderRefsSameIdentity
              candidateRef
              (bcrConstructionRef route)
          ]
      either
        (const Nothing)
        Just
        (elabToBound (bcbrCompletedBound certificate))
      where
        currentBound = maybe TBottom tyToElab candidateBound

    -- A recursively checked child publishes its complete leading declaration
    -- spine through a private owner-final certificate.  Complete the matching
    -- provisional parent candidate before planning the enclosing lambda: this
    -- lets the parent consume the exact bounded declaration with N instead of
    -- first erasing its bound and later recreating a vacuous forall with O.
    certifiedBodyConstructionCompletion candidateRef = do
      bodyConstruction <- certifiedBodyConstruction
      guard (certifiedLambdaBodyOwner bodyConstruction /= owner)
      guard
        ( certifiedConstructionReachesCheckedBody
            bodyConstruction
            bodySourceTy
            && certifiedConstructionReachesCheckedBody
              bodyConstruction
              bodyResultTy
        )
      [(_, Just completedBound)] <-
        pure
          [ binder
          | binder@(certifiedRef, Just _) <-
              certifiedLambdaBodyConstructedBinders bodyConstruction
                ++ certifiedLambdaBodyConsumedBinders bodyConstruction
          , typeBinderRefsSameIdentity candidateRef certifiedRef
          ]
      pure completedBound

    -- The recursively checked body can finish at a concrete application/let
    -- result while the enclosing packet still names that result by the graph
    -- declaration allocated for the exact owner node.  Complete that pending
    -- declaration before choosing the body computation, so Typ(body) emits
    -- @Hyp result@ inside the enclosing value lambda.  The owner-node match is
    -- the construction authority; neither the final type nor a display name
    -- is used to discover the declaration.
    certifiedBodyResultOwnerCompletion candidateRef = do
      bodyConstruction <- certifiedBodyConstruction
      guard (certifiedLambdaBodyOwner bodyConstruction /= owner)
      guard
        ( certifiedConstructionReachesCheckedBody
            bodyConstruction
            bodySourceTy
            && certifiedConstructionReachesCheckedBody
              bodyConstruction
              bodyResultTy
        )
      guard
        ( any
            ( \(packetRef, packetBound) ->
                typeBinderRefsSameIdentity candidateRef packetRef
                  && isNothing packetBound
            )
            packetConstructionBinders
        )
      candidateNode <- typeBinderRefNode candidateRef
      let exactResultEndpoints =
            [ certifiedLambdaBodyConstructedType bodyConstruction
            | candidateNode
                == lgoTermNode (certifiedLambdaBodyOwner bodyConstruction)
            ]
              ++ [ resultTy
                 | (resultOwner, resultTy) <-
                     certifiedLambdaBodyReturnedResults bodyConstruction
                 , candidateNode == lgoTermNode resultOwner
                 ]
          distinctResultEndpoints =
            foldr insertDistinctResultEndpoint [] exactResultEndpoints
      [completedTy] <- pure distinctResultEndpoints
      guard
        ( not
            ( any
                (typeBinderRefsSameIdentity candidateRef)
                ( typeBinderDeclarationRefs completedTy
                    ++ freeTypeVarRefsType completedTy
                )
            )
        )
      either (const Nothing) Just (elabToBound completedTy)
      where
        insertDistinctResultEndpoint resultTy resultTypes
          | any
              (operationalEndpointTypesAgree resultTy)
              resultTypes = resultTypes
          | otherwise = resultTy : resultTypes

    -- A source-owned declaration can enter packet preparation in its solved
    -- graph presentation before the recursively checked annotation restores
    -- the immutable source bound.  When the packet declaration, source
    -- sidecar, and checked body all name the same exact binder, and an xMLF
    -- computation constructs the checked source bound from the packet bound,
    -- use that checked declaration in Lambda(Gamma).  Doing this while the
    -- candidate spine is built keeps later exact planning in one identity and
    -- bound domain; it does not recover a binder from the finished body shape.
    completeCheckedSourceBodyBinder binder@(candidateRef, candidateBound) =
      fromMaybe binder $ do
        incomingBound <- candidateBound
        guard
          ( any
              (typeBinderRefsSameIdentity candidateRef)
              (IntMap.elems sourceSidecarRefs)
          )
        [(_, Just checkedBound)] <-
          pure
            [ bodyBinder
            | bodyBinder@(bodyRef, Just _) <-
                schemeBinderRefs (schemeFromType bodySourceTy)
            , typeBinderRefsSameIdentity candidateRef bodyRef
            ]
        let matchingPacketBounds =
              [ packetBound
              | (packetRef, Just packetBound) <- packetConstructionBinders
              , typeBinderRefsSameIdentity candidateRef packetRef
              ]
            incomingBoundTy =
              sourceProjectCheckedBodyType (tyToElab incomingBound)
        guard
          ( all
              ( operationalEndpointTypesAgree incomingBoundTy
                  . sourceProjectCheckedBodyType
                  . tyToElab
              )
              matchingPacketBounds
          )
        _ <-
          constructExactInstantiation
            (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] Map.empty)
            exactLambdaEndpointTypesAgree
            incomingBoundTy
            (tyToElab checkedBound)
        pure (candidateRef, Just checkedBound)

    -- A source annotation can be copied to several graph occurrences before
    -- the enclosing lambda construction is selected.  Project every binder
    -- declaration and free bound occurrence through the unique checked source
    -- declaration selected by the immutable sidecar, then form the
    -- declaration quotient.  Performing this at the bound-construction
    -- boundary means a graph presentation such as @forall p q. p -> q@ whose
    -- two declarations denote one source @a@ becomes @forall a. a -> a@
    -- before N/Hyp is planned.
    sourceProjectCheckedBodyType ty =
      schemeToType
        ( siScheme
            ( schemeInfoFromRefSubst
                (schemeFromType ty)
                ( IntMap.union
                    (sourceRepresentativeProjection ty)
                    (schemeInfoBinderRefSubst packetConstructionSchemeInfo)
                )
            )
        )

    sourceRepresentativeProjection ty =
      IntMap.fromList
        [ (getNodeId targetNode, sourceRef)
        | targetRef <-
            typeBinderDeclarationRefs ty
              ++ freeTypeVarRefsType ty
        , Just targetNode <- [typeBinderRefNode targetRef]
        , [sourceRef] <-
            [ distinctIdentityRefs
                [ candidateRef
                | (sourceNodeKey, candidateRef) <-
                    IntMap.toList sourceSidecarRefs
                , representative (NodeId sourceNodeKey)
                    == representative targetNode
                , any
                    (typeBinderRefsSameIdentity candidateRef)
                    checkedBodyDeclarationRefs
                ]
            ]
        ]

    checkedBodyDeclarationRefs =
      typeBinderDeclarationRefs bodyResultTy

    distinctIdentityRefs = foldr insertDistinctIdentity []

    insertDistinctIdentity ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    -- A completed local view can be reopened only when the exact body
    -- refinement proves that this owner consumes the packet completion and
    -- the inherited endpoint retains the same declaration unbounded.
    alignRetainedPacketBinderWithFrozenEndpoint
      (candidateRef, Just _)
      | expectedUsesCertifiedPacketCompletion
      , packetKeepsBinderUnbounded candidateRef
      , expectedKeepsBinderUnbounded candidateRef
      , not
          ( any
              (bodyConsumerBoundRefinementTargetsAny [candidateRef])
              bodyRefinements
          ) =
          (candidateRef, Nothing)
    alignRetainedPacketBinderWithFrozenEndpoint binder = binder

    packetKeepsBinderUnbounded candidateRef =
      any
        ( \(packetRef, packetBound) ->
            typeBinderRefsSameIdentity candidateRef packetRef
              && isNothing packetBound
        )
        packetConstructionBinders

    expectedKeepsBinderUnbounded candidateRef =
      any
        ( \(expectedRef, expectedBound) ->
            typeBinderRefsSameIdentity candidateRef expectedRef
              && isNothing expectedBound
        )
        expectedBinders

    expectedUsesCertifiedPacketCompletion =
      any expectedUsesCompletion bodyRefinements

    expectedUsesCompletion certificate =
      bodyConsumerBoundRefinementCompletesPreparedPacket
        owner
        packet
        certificate
        && case schemeBody expectedScheme of
          TArrow _ expectedBodyTy ->
            operationalEndpointTypesAgree
              expectedBodyTy
              (bcbrCompletedBound certificate)
          _ -> False

    -- A nested owner may complete the packet declaration before this exact
    -- lambda reconstructs and consumes its binder spine.  Accept that bound
    -- only when the private refinement certificate proves the transition
    -- from this packet's unbounded declaration and the active construction
    -- Gamma independently contains the same exact completed identity.
    certifiedDescendantCompletion candidateRef = do
      guard
        ( any
            ( \(packetRef, packetBound) ->
                typeBinderRefsSameIdentity candidateRef packetRef
                  && isNothing packetBound
            )
            packetConstructionBinders
        )
      [(ambientRef, ambientBound)] <-
        pure
          [ binding
          | binding@(ref, _) <- Map.toList ambientBindings
          , typeBinderRefsSameIdentity candidateRef ref
          ]
      guard (typeBinderRefsSameIdentity candidateRef ambientRef)
      [_certificate] <-
        pure
          [ certificate
          | certificate <- bodyRefinements
          , bodyConsumerBoundRefinementCertifiesTransition
              candidateRef
              TBottom
              ambientBound
              certificate
          ]
      either (const Nothing) Just (elabToBound ambientBound)

    expectedBodyBinderRef = do
      TArrow _ expectedBodyTy <- pure (schemeBody expectedScheme)
      case expectedBodyTy of
        TVarRef ref -> Just ref
        _ -> Nothing

    packetBodyBinderRef = do
      TArrow _ packetBodyTy <-
        pure
          ( schemeBody
              ( siScheme
                  ( subtermGeneralizationConsumerConstructionSchemeInfo
                      packet
                  )
              )
          )
      case packetBodyTy of
        TVarRef ref -> Just ref
        _ -> Nothing

    -- A prepared body packet can end in a provisional result declaration
    -- which the checked outgoing computation immediately consumes with Hyp,
    -- while an independently certified exact lambda endpoint quantifies the
    -- current value parameter.  Those declarations are not alpha peers: the
    -- packet body identity is a completed waypoint, whereas the endpoint
    -- binder is born at this lambda's Lambda(Gamma).  When the parameter,
    -- packet body, and terminal Hyp identities establish those two roles,
    -- discard only the consumed waypoint and construct the exact endpoint
    -- directly.  This prevents binder-spine alpha alignment from turning a
    -- body-result bound into the parameter binder's bound.
    certifyParameterOwnedExactEndpoint = do
      expectedParameterRef <-
        case
            [ expectedRef
            | (expectedRef, Nothing) <- expectedBinders
            , any
                (typeBinderRefsSameIdentity expectedRef)
                (freeTypeVarRefsType paramTy)
            ]
          of
            [ref] -> Just ref
            _ -> Nothing
      guard
        ( not
            ( any
                (typeBinderRefsSameIdentity expectedParameterRef . fst)
                candidates
            )
        )
      packetBodyRef <-
        packetBodyBinderRef
          <|> subtermGeneralizationConstructionResultAbstractionRef packet
      guard
        ( not
            ( typeBinderRefsSameIdentity
                packetBodyRef
                expectedParameterRef
            )
            && not
              ( any
                  (typeBinderRefsSameIdentity packetBodyRef)
                  (freeTypeVarRefsType expectedTy)
              )
        )
      guard
        ( any
            ( \(packetRef, packetBound) ->
                typeBinderRefsSameIdentity packetRef packetBodyRef
                  && isNothing packetBound
            )
            packetConstructionBinders
        )
      guard
        ( maybe
            False
            (typeBinderRefsSameIdentity packetBodyRef)
            (outgoingResultRef checkedBodyInstantiation)
            || packetOperatedEndpointDischarges packetBodyRef
        )
      let retainedCandidates =
            [ candidate
            | candidate@(candidateRef, _) <- candidates
            , not
                (typeBinderRefsSameIdentity candidateRef packetBodyRef)
            ]
      either
        (const Nothing)
        Just
        ( certifyExactLambdaEndpointConstruction
            ambientBindings
            exactConstructionReservedRefs
            retainedCandidates
            paramTy
            bodySourceTy
            expectedTy
        )

    -- A source annotation closes its own forall inside the value parameter,
    -- while a packet prepared before that boundary can still expose the same
    -- source identity as an outer declaration and use it as the provisional
    -- body result.  The paper @g g@ construction is the characteristic case:
    -- the checked body has already applied @Hyp(result)@, so the final lambda
    -- must retain the distinct packet result declaration
    --
    --   @Lambda(result > sigma-id). lambda (g : sigma-id). ... : result@
    --
    -- rather than re-emitting the annotation's @a@ outside @sigma-id@.  Build
    -- that endpoint only when the source-order certificate owns the closed
    -- parameter declaration, the packet owns one distinct result declaration,
    -- and the checked body computation ends with Hyp at that exact result.
    -- This is a constructor for the completed endpoint, not an alpha/shape
    -- repair of the already-built term.
    certifyClosedSourceParameterResultEndpoint = do
      [sourceParamRef] <- pure
        [ candidateRef
        | (candidateRef, Nothing) <- candidates
        , any
            (typeBinderRefsSameIdentity candidateRef)
            (IntMap.elems sourceSidecarRefs)
        , any
            (typeBinderRefsSameIdentity candidateRef . fst)
            (schemeBinderRefs (schemeFromType paramTy))
        ]
      packetResultRef <- outgoingResultRef checkedBodyInstantiation
      guard
        ( not
            (typeBinderRefsSameIdentity sourceParamRef packetResultRef)
        )
      (candidateResultRef, Just candidateResultBound) <-
        find
          (typeBinderRefsSameIdentity packetResultRef . fst)
          candidates
      guard
        ( constructionOwnsPacketBinder candidateResultRef
            && operationalEndpointTypesAgree
              (tyToElab candidateResultBound)
              paramTy
        )
      guard
        ( case bodyResultTy of
              TVarRef bodyResultRef ->
                typeBinderRefsSameIdentity
                  bodyResultRef
                  candidateResultRef
              _ -> False
        )
      TArrow expectedParamTy (TVarRef expectedResultRef) <-
        pure (schemeBody expectedScheme)
      parameterAlphaPlan <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          paramTy
          expectedParamTy
      guard
        ( exactBinderSpineInstantiation parameterAlphaPlan == InstId
            && typeBinderRefsSameIdentity
              expectedResultRef
              ( alphaRoutedParameterRef
                  parameterAlphaPlan
                  sourceParamRef
              )
        )
      guard
        (operationalEndpointTypesAgree expectedParamTy paramTy)
      let retainedCandidates =
            [ candidate
            | candidate@(candidateRef, _) <- candidates
            , not
                (typeBinderRefsSameIdentity candidateRef sourceParamRef)
            ]
          retainedExpectedRefs =
            [ expectedRef
            | (expectedRef, _) <- expectedBinders
            , not
                (typeBinderRefsSameIdentity expectedRef expectedResultRef)
            ]
      guard
        (length retainedCandidates == length retainedExpectedRefs)
      let completedEndpoint =
            schemeToType
              ( mkElabSchemeWithRefs
                  retainedCandidates
                  (TArrow expectedParamTy bodyResultTy)
              )
      certifyBodyOption
        bodySourceTy
        checkedBodyInstantiation
        retainedCandidates
        completedEndpoint
        (InstId, bodyResultTy, ambientPublishedBodyRefinement, [])

    -- The incoming endpoint can alpha-copy the source annotation's closed
    -- forall before this packet-aware constructor sees it.  A zero-step exact
    -- binder-spine plan is the positive correspondence between those lexical
    -- declarations: its payload rename is checked by replaying the complete
    -- parameter type, whereas a name or type-shape comparison would lose the
    -- source identity.  The routed copy may then be removed from the frozen
    -- outer spine by the paper g-g completion above.
    alphaRoutedParameterRef plan sourceRef =
      fromMaybe
        sourceRef
        ( snd
            <$> find
              (typeBinderRefsSameIdentity sourceRef . fst)
              (exactBinderSpineRenames plan)
        )

    outgoingResultRef inst =
      case inst of
        InstAbstrRef ref -> Just ref
        InstSeq _ suffix -> outgoingResultRef suffix
        _ -> Nothing

    -- A source annotation binder can remain in the packet's exact published
    -- spine even when it is shadowed inside the lambda parameter and is
    -- therefore absent from the local Gamma candidates.  Source order, not
    -- local-emission order, owns that declaration.  When every missing
    -- expected declaration is explicitly source-owned, has the same packet
    -- declaration, and is vacuous in the lambda body at this outer scope, the
    -- exact endpoint itself is the positive authority to emit it.  Delegate
    -- to the ordinary inherited-endpoint constructor, which constructs the
    -- complete expected spine before the value lambda and checks the body at
    -- its exact codomain.
    certifyVacuousSourceOwnedExpectedDeclarations = do
      let missingExpectedBinders =
            [ expectedBinder
            | expectedBinder@(expectedRef, _) <- expectedBinders
            , not
                ( any
                    (typeBinderRefsSameIdentity expectedRef . fst)
                    candidates
                )
            ]
          expectedBodyFreeRefs =
            freeTypeVarRefsType (schemeBody expectedScheme)
      guard (not (null missingExpectedBinders))
      guard
        ( all
            ( \(expectedRef, expectedBound) ->
                sourceOwnsPacketBinder expectedRef
                  && not
                    ( any
                        (typeBinderRefsSameIdentity expectedRef)
                        expectedBodyFreeRefs
                    )
                  && any
                    (samePacketDeclaration expectedRef expectedBound)
                    packetConstructionBinders
            )
            missingExpectedBinders
        )
      either
        (const Nothing)
        Just
        ( certifyExactLambdaEndpointConstruction
            ambientBindings
            exactConstructionReservedRefs
            candidates
            paramTy
            bodySourceTy
            expectedTy
        )
      where
        samePacketDeclaration expectedRef expectedBound (packetRef, packetBound) =
          typeBinderRefsSameIdentity expectedRef packetRef
            && case (expectedBound, packetBound) of
              (Nothing, Nothing) -> True
              (Just expected, Just packetBoundTy) ->
                operationalEndpointTypesAgree
                  (tyToElab expected)
                  (tyToElab packetBoundTy)
              _ -> False

    packetOperatedEndpointDischarges packetBodyRef =
      case
          ( [ mbBound
            | (completedRef, mbBound) <-
                schemeBinderRefs
                  (siScheme (subtermGeneralizationSchemeInfo packet))
            , typeBinderRefsSameIdentity completedRef packetBodyRef
            ]
          , [ mbBound
            | (candidateRef, mbBound) <- candidates
            , typeBinderRefsSameIdentity candidateRef packetBodyRef
            ]
          , schemeBody expectedScheme
          )
        of
          ([Just completedBound], [Just candidateBound], TArrow _ expectedBodyTy) ->
            let operatedTy =
                  schemeToType
                    ( siScheme
                        (subtermGeneralizationOperatedSchemeInfo packet)
                    )
             in operationalEndpointTypesAgree
                  (tyToElab completedBound)
                  (tyToElab candidateBound)
                  && operationalEndpointTypesAgree bodySourceTy operatedTy
                  && operationalEndpointTypesAgree expectedBodyTy operatedTy
          _ -> False

    -- An enclosing packet is frozen before its nested lambda body is checked.
    -- When its pending result declaration has already been eliminated, that
    -- frozen endpoint contains Bottom rather than the declaration identity.
    -- The same packet's operated scheme fixes the missing codomain, and the
    -- checked body must independently inhabit it.  Complete only this exact
    -- packet-authorized placeholder before planning the binder spine.
    completedBottomEndpoints =
      maybeToList completedProvisionalBottomEndpoint

    completedProvisionalBottomEndpoint = do
      guard
        ( any
            ( \(candidateRef, candidateBound) ->
                isNothing candidateBound
                  && any
                    ( \(packetRef, packetBound) ->
                        typeBinderRefsSameIdentity
                          candidateRef
                          packetRef
                          && isNothing packetBound
                    )
                    packetConstructionBinders
            )
            candidates
        )
      TArrow expectedParamTy TBottom <-
        pure (schemeBody expectedScheme)
      let operatedScheme =
            siScheme (subtermGeneralizationOperatedSchemeInfo packet)
      TArrow operatedParamTy operatedBodyTy <-
        pure (schemeBody operatedScheme)
      guard
        ( operationalEndpointTypesAgree
            expectedParamTy
            operatedParamTy
        )
      guard
        ( operationalEndpointTypesAgree
            bodyResultTy
            operatedBodyTy
        )
      pure
        ( schemeToType
            ( mkElabSchemeWithRefs
                expectedBinders
                (TArrow expectedParamTy operatedBodyTy)
            )
        )

    bodyOptions =
      (InstId, bodyResultTy, ambientPublishedBodyRefinement, [])
        : [ (InstAbstrRef ref, TVarRef ref, Nothing, [])
          | (ref, mbBound) <- candidates
          , operationalEndpointTypesAgree
              bodyResultTy
              (maybe TBottom tyToElab mbBound)
          ]
        ++ completedPacketBodyOptions
        ++ ambientBodyOptions

    -- The recursive body can already have applied Hyp for a bounded
    -- declaration before this enclosing Lambda(Gamma) is selected.  Its
    -- checked result then contains the flexible identity itself, while the
    -- frozen packet presentation still expands that occurrence to the
    -- declaration's bound.  Replaying the packet presentation would require
    -- the impossible reverse computation from the flexible identity back to
    -- its bound.
    --
    -- Retain the identity occurrence only when one exact bounded candidate
    -- and the enclosing endpoint declare the same identity and bound, and
    -- substituting that bound for the free checked-body occurrence recovers
    -- the frozen endpoint exactly.  This is the by-construction inverse of
    -- the already checked Hyp; no shape-only variable-to-bound rewrite is
    -- admitted.
    certifyAlreadyAbstractedExpectedDeclaration = do
      [ (candidateRef, candidateBound, expectedRef)
        ] <- pure
          [ (candidateRef, candidateBound, expectedRef)
          | (candidateRef, Just candidateBound) <- candidates
          , (expectedRef, Just expectedBound) <- expectedBinders
          , typeBinderRefsSameIdentity candidateRef expectedRef
          , operationalEndpointTypesAgree
              (tyToElab candidateBound)
              (tyToElab expectedBound)
          , any
              (typeBinderRefsSameIdentity candidateRef)
              (freeTypeVarRefsType bodyResultTy)
          ]
      TArrow expectedParamTy expectedBodyTy <-
        pure (schemeBody expectedScheme)
      guard
        ( operationalEndpointTypesAgree expectedParamTy paramTy
            && not
              ( any
                  (typeBinderRefsSameIdentity candidateRef)
                  (freeTypeVarRefsType paramTy)
              )
            && operationalEndpointTypesAgree
              ( substTypeCaptureRef
                  candidateRef
                  (tyToElab candidateBound)
                  bodyResultTy
              )
              expectedBodyTy
        )
      let retainedExpectedTy =
            schemeToType
              ( mkElabSchemeWithRefs
                  expectedBinders
                  (TArrow expectedParamTy bodyResultTy)
              )
      guard
        ( any
            (typeBinderRefsSameIdentity expectedRef . fst)
            (schemeBinderRefs (schemeFromType retainedExpectedTy))
        )
      certifyBodyOption
        bodySourceTy
        checkedBodyInstantiation
        candidates
        retainedExpectedTy
        (InstId, bodyResultTy, ambientPublishedBodyRefinement, [])

    -- A child owner-final certificate can complete a frozen enclosing
    -- endpoint whose body still contains the child's free graph presentation.
    -- The certificate supplies the complete declaration spine, including
    -- source-owned dependencies that do not occur in the frozen endpoint.
    -- Replace the frozen body only when the checked child residual is the same
    -- exact endpoint, then use the ordinary commuting constructor below.  The
    -- completed endpoint therefore comes from the child's construction input,
    -- never from peeling the checked body's final type.
    certifyCertifiedBodyBinderSpineAcrossLambda = do
      bodyConstruction <- certifiedBodyConstruction
      completedExpectedTy <- certifiedBodyCompletedExpectedType
      certifyBodyBinderSpineAcrossLambdaAt
        (certifiedLambdaBodyConstructedBinders bodyConstruction)
        completedExpectedTy

    certifiedBodyCompletedExpectedType = do
      bodyConstruction <- certifiedBodyConstruction
      let constructedTy =
            certifiedLambdaBodyConstructedType bodyConstruction
          constructedScheme = schemeFromType constructedTy
          constructedBinders =
            certifiedLambdaBodyConstructedBinders bodyConstruction
          constructedResidual = schemeBody constructedScheme
      guard
        ( certifiedConstructionReachesCheckedBody
            bodyConstruction
            bodySourceTy
            && certifiedConstructionReachesCheckedBody
              bodyConstruction
              bodyResultTy
        )
      TArrow expectedParamTy expectedBodyTy <-
        pure (schemeBody expectedScheme)
      guard
        ( operationalEndpointTypesAgree expectedParamTy paramTy
            && operationalEndpointTypesAgree
              expectedBodyTy
              constructedResidual
        )
      completedBinders <-
        foldM mergeCertifiedBodyBinder expectedBinders constructedBinders
      completedScheme <-
        either
          (const Nothing)
          Just
          ( orderSourceProjectedSchemeBinders
              "certified lambda-body construction"
              ( mkElabSchemeWithRefs
                  completedBinders
                  (TArrow expectedParamTy constructedResidual)
              )
          )
      pure (schemeToType completedScheme)

    -- The owner-final endpoint can retain a certified declaration which the
    -- checked body has already consumed by N.  Equality is the zero-step
    -- case; otherwise require the complete exact binder-spine computation to
    -- typecheck from that owner endpoint to the checked body view.
    certifiedConstructionReachesCheckedBody bodyConstruction checkedTy =
      operationalEndpointTypesAgree constructedTy checkedTy
        || isJust (do
          spinePlan <-
            planExactBinderSpine
              exactLambdaEndpointTypesAgree
              constructedTy
              checkedTy
          let alignedConstructedTy =
                renameTypeBinderRefPayloads
                  (exactBinderSpineRenames spinePlan)
                  constructedTy
          reachedTy <-
            either
              (const Nothing)
              Just
              ( TypeCheck.checkInstantiation
                  emptyConstructionTypeEnv
                  alignedConstructedTy
                  (exactBinderSpineInstantiation spinePlan)
              )
          guard (exactLambdaEndpointTypesAgree reachedTy checkedTy)
          )
      where
        constructedTy = certifiedLambdaBodyConstructedType bodyConstruction

    mergeCertifiedBodyBinder binders certifiedBinder@(certifiedRef, certifiedBound) =
      case
          [ existingBinder
          | existingBinder@(existingRef, _) <- binders
          , typeBinderRefsSameIdentity existingRef certifiedRef
          ]
        of
          [] -> Just (binders ++ [certifiedBinder])
          [(_, Nothing)]
            | Just _ <- certifiedBound ->
                -- The enclosing packet was frozen before Typ(body).  The
                -- checked owner-final certificate completes that exact
                -- pending declaration, preserving its identity and lexical
                -- position while replacing only the provisional bound.
                Just
                  [ if typeBinderRefsSameIdentity existingRef certifiedRef
                      then certifiedBinder
                      else existingBinder
                  | existingBinder@(existingRef, _) <- binders
                  ]
          [(_, existingBound)]
            | lambdaConstructionBinderBoundsAgree
                existingBound
                certifiedBound ->
                Just binders
          _ -> Nothing

    lambdaConstructionBinderBoundsAgree Nothing Nothing = True
    lambdaConstructionBinderBoundsAgree (Just left) (Just right) =
      let leftTy = sourceProjectCheckedBodyType (tyToElab left)
          rightTy = sourceProjectCheckedBodyType (tyToElab right)
       in operationalEndpointTypesAgree leftTy rightTy
            || isJust
              ( constructExactInstantiation
                  emptyConstructionTypeEnv
                  exactLambdaEndpointTypesAgree
                  leftTy
                  rightTy
              )
    lambdaConstructionBinderBoundsAgree _ _ = False

    emptyConstructionTypeEnv =
      TypeCheck.mkTypeCheckEnvWithResolvedTerms [] Map.empty

    -- A checked body can own a leading forall spine while an enclosing exact
    -- lambda endpoint places that same spine outside the value lambda.  This
    -- is the paper's ordinary M/N commuting construction:
    --
    --   \x. (Lambda a. e)  ==>  Lambda a. \x. (e [a])
    --
    -- The spine can mix packet-local declarations with source binders that
    -- are deliberately absent from local Gamma emission order.  Require each
    -- declaration to be owned by the corresponding packet order, require the
    -- checked body and exact endpoint to name the same identities and bounds,
    -- alpha-copy the complete inner spine, and typecheck one complete N
    -- specialization at the outer declarations before publishing the plan.
    certifyBodyBinderSpineAcrossLambda =
      certifyBodyBinderSpineAcrossLambdaAt [] expectedTy

    -- The selected packet endpoint can already own the complete outer
    -- Lambda(Gamma) spine while the recursively checked owner exposes only
    -- its value-lambda body.  When that private owner-final certificate
    -- independently reaches both checked body views, construct the selected
    -- endpoint directly with the inherited-endpoint constructor.  This is
    -- Figure 15.3.5's downward Var-Abs construction: the endpoint supplies
    -- the binder placement, the owner certificate supplies the child, and
    -- the constructor must still typecheck the complete O/M/N computation.
    certifySelectedEndpointFromCertifiedBody = do
      bodyConstruction <- certifiedBodyConstruction
      guard
        ( certifiedConstructionReachesCheckedBody
            bodyConstruction
            bodySourceTy
            && certifiedConstructionReachesCheckedBody
              bodyConstruction
              bodyResultTy
        )
      either
        (const Nothing)
        Just
        ( certifyExactLambdaEndpointConstruction
            ambientBindings
            exactConstructionReservedRefs
            candidates
            paramTy
            bodySourceTy
            expectedTy
        )

    -- A prepared graph endpoint can use one flexible declaration for both a
    -- returned source lambda's parameter and its result:
    --
    --   forall (b >= sigma). b -> b
    --
    -- Once the returned lambda constructor has installed its exact parameter
    -- boundary, the checked child is instead born at @sigma -> b@.  This is
    -- not an instantiation beneath an arrow and must not be recovered by a
    -- global @b := sigma@ rewrite.  The returned-result chain pairs each
    -- value-lambda owner with the exact boundary certificate it installed;
    -- use that association to construct the completed endpoint directly.
    certifyReturnedLambdaParameterCompletedEndpoint = do
      bodyConstruction <- certifiedBodyConstruction
      guard
        ( certifiedConstructionReachesCheckedBody
            bodyConstruction
            bodySourceTy
            && certifiedConstructionReachesCheckedBody
              bodyConstruction
              bodyResultTy
        )
      let completedTy = constructionType candidates paramTy bodyResultTy
      guard
        ( not
            (exactLambdaEndpointTypesAgree completedTy expectedTy)
        )
      completedReturnedOwners <-
        certifyReturnedLambdaParameterBodyCompletion
          bodyConstruction
          completedTy
      guard (not (null completedReturnedOwners))
      certifyBodyOption
        bodySourceTy
        checkedBodyInstantiation
        candidates
        completedTy
        (InstId, bodyResultTy, ambientPublishedBodyRefinement, [])

    certifyReturnedLambdaParameterBodyCompletion
      bodyConstruction
      completedTy = do
        let completedScheme = schemeFromType completedTy
            completedBinders = schemeBinderRefs completedScheme
        guard
          ( length completedBinders == length expectedBinders
              && and
                ( zipWith
                    constructionBinderAgrees
                    completedBinders
                    expectedBinders
                )
          )
        (expectedParamTy, expectedBodyTy) <-
          valueLambdaParts expectedScheme
        (completedParamTy, completedBodyTy) <-
          valueLambdaParts completedScheme
        guard
          ( operationalEndpointTypesAgree expectedParamTy paramTy
              && operationalEndpointTypesAgree completedParamTy paramTy
          )
        completedOwners <-
          matchReturnedLambdaParameterBodies
            bodyConstruction
            expectedBodyTy
            completedBodyTy
        guard (allDistinctBy (==) completedOwners)
        pure completedOwners

    valueLambdaParts scheme =
      case schemeBody scheme of
        TArrow parameterTy bodyTy -> Just (parameterTy, bodyTy)
        _ -> Nothing

    constructionBinderAgrees
      (leftRef, leftBound)
      (rightRef, rightBound) =
        typeBinderRefsSameIdentity leftRef rightRef
          && operationalEndpointTypesAgree
            (maybe TBottom tyToElab leftBound)
            (maybe TBottom tyToElab rightBound)

    matchReturnedLambdaParameterBodies
      _bodyConstruction
      expectedBodyTy
      completedBodyTy
        | operationalEndpointTypesAgree expectedBodyTy completedBodyTy =
            Just []
    matchReturnedLambdaParameterBodies
      bodyConstruction
      (TForallRef expectedRef expectedBound expectedBodyTy)
      (TForallRef completedRef completedBound completedBodyTy)
        | typeBinderRefsSameIdentity expectedRef completedRef
        , operationalEndpointTypesAgree
            (maybe TBottom tyToElab expectedBound)
            (maybe TBottom tyToElab completedBound) =
            matchReturnedLambdaParameterBodies
              bodyConstruction
              expectedBodyTy
              completedBodyTy
    matchReturnedLambdaParameterBodies
      bodyConstruction
      expectedSuffix@(TArrow expectedDomain expectedCodomain)
      completedSuffix@(TArrow completedDomain completedCodomain)
        | operationalEndpointTypesAgree expectedDomain completedDomain =
            matchReturnedLambdaParameterBodies
              bodyConstruction
              expectedCodomain
              completedCodomain
        | otherwise = do
            returnedOwner <-
              certifyReturnedLambdaParameterDomainCompletion
                bodyConstruction
                expectedSuffix
                completedSuffix
                expectedDomain
                expectedCodomain
                completedDomain
                completedCodomain
            remainingOwners <-
              matchReturnedLambdaParameterBodies
                bodyConstruction
                expectedCodomain
                completedCodomain
            pure (returnedOwner : remainingOwners)
    matchReturnedLambdaParameterBodies _ _ _ = Nothing

    certifyReturnedLambdaParameterDomainCompletion
      bodyConstruction
      expectedSuffix
      completedSuffix
      expectedDomain
      expectedCodomain
      completedDomain
      completedCodomain = do
        [(candidateRef, candidateBound, endpointRef, endpointBound)] <-
          pure
            [ (candidateRef, candidateBound, endpointRef, endpointBound)
            | (candidateRef, Just candidateBound) <- candidates
            , any
                (typeBinderRefsSameIdentity candidateRef)
                (freeTypeVarRefsType expectedDomain)
            , any
                (typeBinderRefsSameIdentity candidateRef)
                (freeTypeVarRefsType expectedCodomain)
            , any
                (typeBinderRefsSameIdentity candidateRef)
                (freeTypeVarRefsType completedCodomain)
            , (endpointRef, Just endpointBound) <- expectedBinders
            , typeBinderRefsSameIdentity candidateRef endpointRef
            ]
        guard
          ( typeBinderRefsSameIdentity candidateRef endpointRef
              && operationalEndpointTypesAgree
                (tyToElab candidateBound)
                completedDomain
              && operationalEndpointTypesAgree
                (tyToElab endpointBound)
                completedDomain
          )
        -- A provisional endpoint may place its flexible result beneath a
        -- source forall, as in the paper's returned @g g@ lambda.  The child
        -- lambda has already installed the completed parameter boundary, so
        -- require the ordinary xMLF constructor to produce the complete
        -- domain computation (for the characteristic case,
        -- @O; Under a (Hyp result)@).  This retains the direct-variable case
        -- without broadening it to an arrow-shape substitution.
        _ <-
          constructExactInstantiation
            (constructionTypeEnvFor candidates)
            exactLambdaEndpointTypesAgree
            completedDomain
            expectedDomain
        [(returnedOwner, _boundaryCertificate, returnedTy)] <-
          pure
            [ association
            | association@(ownerAtBoundary, certificate, returnedTy) <-
                ownBoundaryAssociation bodyConstruction
                  ++ certifiedLambdaBodyReturnedLambdaParameterBoundaries
                    bodyConstruction
            , lgoConstructor ownerAtBoundary == LocalLambdaGamma
            , operationalEndpointTypesAgree returnedTy completedSuffix
            , operationalEndpointTypesAgree
                (lambdaParamBoundaryConstructedType certificate)
                completedDomain
            , boundarySourceDeclarationsAgree certificate
            ]
        guard
          ( not
              (operationalEndpointTypesAgree expectedSuffix completedSuffix)
              && operationalEndpointTypesAgree returnedTy completedSuffix
              && lgoConstructor returnedOwner == LocalLambdaGamma
          )
        pure returnedOwner

    ownBoundaryAssociation bodyConstruction =
      case certifiedLambdaBodyOwnLambdaParameterBoundary bodyConstruction of
        Nothing -> []
        Just boundaryCertificate ->
          [ ( certifiedLambdaBodyOwner bodyConstruction
            , boundaryCertificate
            , certifiedLambdaBodyConstructedType bodyConstruction
            )
          ]

    boundarySourceDeclarationsAgree certificate =
      let sourceRefs = lambdaParamBoundarySourceBinderRefs certificate
          constructedRefs =
            map fst
              ( schemeBinderRefs
                  ( schemeFromType
                      (lambdaParamBoundaryConstructedType certificate)
                  )
              )
       in length sourceRefs == length constructedRefs
            && all
              ( \sourceRef ->
                  any
                    (typeBinderRefsSameIdentity sourceRef)
                    constructedRefs
              )
              sourceRefs

    certifyBodyBinderSpineAcrossLambdaAt certifiedBodyBinders publishedTy = do
      let bodyScheme = schemeFromType bodySourceTy
          bodyBinders = schemeBinderRefs bodyScheme
          bodyResidual = schemeBody bodyScheme
          publishedScheme = schemeFromType publishedTy
          publishedBinders = schemeBinderRefs publishedScheme
      rawProjectedExpectedBinders <-
        traverse sourceProjectExpectedBinder publishedBinders
      let ( projectedExpectedBinders
            , projectedExpectedBoundScopeCopies
            , _projectedExpectedBoundScopeGenerator
            , _projectedExpectedBoundReservedRefs
            ) =
              foldl
                freshenCandidateBoundScope
                ( []
                , []
                , _candidateBoundScopeGenerator
                , foldr
                    insertExactConstructionRef
                    []
                    ( map fst rawProjectedExpectedBinders
                        ++ typeBinderDeclarationRefs
                          (schemeBody publishedScheme)
                    )
                )
                rawProjectedExpectedBinders
          sourceProjectedExpectedScheme =
            mkElabSchemeWithRefs
              projectedExpectedBinders
              (schemeBody publishedScheme)
          sourceProjectedExpectedTy =
            schemeToType sourceProjectedExpectedScheme
          sourceProjectedExpectedBinders =
            schemeBinderRefs sourceProjectedExpectedScheme
      guard (not (null bodyBinders))
      TArrow expectedParamTy expectedBodyTy <-
        pure (schemeBody sourceProjectedExpectedScheme)
      let liftedPairsResult =
            traverse
              ( liftedBodyBinder
                  bodyResidual
                  expectedBodyTy
                  sourceProjectedExpectedBinders
              )
              bodyBinders
      liftedPairs <- liftedPairsResult
      guard
        ( allDistinctRefs
            [ expectedRef
            | (_, (expectedRef, _)) <- liftedPairs
            ]
        )
      guard
        ( all
            ( \(bodyRef, _) ->
                not
                  ( any
                      (typeBinderRefsSameIdentity bodyRef)
                      (freeTypeVarRefsType paramTy)
                  )
            )
            bodyBinders
        )
      let bodyResidualAtLiftedResults =
            foldl
              ( \residual ((bodyRef, _), (expectedRef, _)) ->
                  if typeBinderRefsSameIdentity bodyRef expectedRef
                    then residual
                    else
                      substTypeCaptureRef
                        bodyRef
                        (TVarRef expectedRef)
                        residual
              )
              bodyResidual
              liftedPairs
      guard
        ( operationalEndpointTypesAgree expectedParamTy paramTy
            && operationalEndpointTypesAgree
              bodyResidualAtLiftedResults
              expectedBodyTy
        )
      liftedCandidates <-
        traverse
          (selectExpectedCandidate liftedPairs)
          sourceProjectedExpectedBinders
      let remainingCandidates =
            [ candidate
            | candidate@(candidateRef, _) <- bodyCompletedCandidates
            , not
                ( any
                    (typeBinderRefsSameIdentity candidateRef . fst)
                    sourceProjectedExpectedBinders
                )
            ]
          constructionCandidates =
            liftedCandidates ++ remainingCandidates
          (bodyBinderRenames, _) =
            foldl
              ( \(renames, generator) (bodyRef, _) ->
                  freshenSourceBinder (renames, generator) bodyRef
              )
              ( []
              , exactConstructionIdentityGenerator
                  [sourceProjectedExpectedTy]
              )
              bodyBinders
          freshBodySourceTy =
            renameTypeBinderRefPayloads
              bodyBinderRenames
              bodySourceTy
          liftedTypeEnv =
            constructionTypeEnvFor constructionCandidates
          directPlan =
            constructExactInstantiationAtSourceArguments
              liftedTypeEnv
              exactLambdaEndpointTypesAgree
              freshBodySourceTy
              [ TVarRef expectedRef
              | (_, (expectedRef, _)) <- liftedPairs
              ]
              expectedBodyTy
      bodySpineSpecialization <- directPlan
      let certifiedPlanResult =
            certifyBodyOption
              freshBodySourceTy
              InstId
              constructionCandidates
              sourceProjectedExpectedTy
              ( bodySpineSpecialization
              , expectedBodyTy
              , ambientPublishedBodyRefinement
              , []
              )
      plan <- certifiedPlanResult
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              bodyBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          , exactLambdaConstructionResultBinderCopies =
              exactLambdaConstructionResultBinderCopies plan
                ++ [ (sourceRef, copiedRef)
                   | (_, _, sourceRef, copiedRef) <-
                       projectedExpectedBoundScopeCopies
                   ]
          }
      where
        liftedBodyBinder
          bodyResidual
          expectedBodyTy
          projectedExpectedBinders
          bodyBinder@(bodyRef, bodyBound) = do
          let exactExpectedMatches =
                [ binder
                | binder@(ref, bound) <- projectedExpectedBinders
                , typeBinderRefsSameIdentity bodyRef ref
                , lambdaConstructionBinderBoundsAgree bodyBound bound
                ]
              routedExpectedMatches =
                [ binder
                | binder@(ref, bound) <- projectedExpectedBinders
                , not (typeBinderRefsSameIdentity bodyRef ref)
                , lambdaConstructionBinderBoundsAgree bodyBound bound
                , bodyResultRefConstructsExpectedResult
                    bodyResidual
                    expectedBodyTy
                    bodyRef
                    ref
                ]
              expectedMatches =
                if null exactExpectedMatches
                  then routedExpectedMatches
                  else exactExpectedMatches
          [expectedBinder@(expectedRef, expectedBound)] <-
            pure expectedMatches
          let matchingCandidates =
                [ candidate
                | candidate@(candidateRef, _) <- bodyCompletedCandidates
                , typeBinderRefsSameIdentity expectedRef candidateRef
                ]
              matchingCertifiedBodyBinders =
                [ certifiedBinder
                | certifiedBinder@(certifiedRef, certifiedBound) <-
                    certifiedBodyBinders
                , typeBinderRefsSameIdentity expectedRef certifiedRef
                , lambdaConstructionBinderBoundsAgree
                    bodyBound
                    certifiedBound
                ]
          guard
            ( ( typeBinderRefsSameIdentity bodyRef expectedRef
                  || bodyResultRefConstructsExpectedResult
                    bodyResidual
                    expectedBodyTy
                    bodyRef
                    expectedRef
              )
                && (not (null matchingCandidates)
                      || sourceOwnsPacketBinder expectedRef
                      || length matchingCertifiedBodyBinders == 1
                   )
                && all
                  (lambdaConstructionBinderBoundsAgree expectedBound . snd)
                  matchingCandidates
                && ( not (null matchingCandidates)
                      || length matchingCertifiedBodyBinders == 1
                      || any
                        ( \(packetRef, packetBound) ->
                            typeBinderRefsSameIdentity packetRef expectedRef
                              && lambdaConstructionBinderBoundsAgree
                                packetBound
                                expectedBound
                        )
                        packetConstructionBinders
                   )
            )
          pure (bodyBinder, expectedBinder)

        -- The child and enclosing endpoint can allocate different flexible
        -- identities for the same value result.  Pair them only when this is
        -- the complete residual correspondence: replacing the child result
        -- occurrence by the enclosing declaration must reproduce the exact
        -- expected codomain.  The subsequent positional N construction is
        -- still checked in the full construction Gamma, so this relation
        -- selects an argument but never transports a term by type shape.
        bodyResultRefConstructsExpectedResult
          bodyResidual
          expectedBodyTy
          bodyRef
          expectedRef =
          any
            (typeBinderRefsSameIdentity bodyRef)
            (freeTypeVarRefsType bodyResidual)
            && any
              (typeBinderRefsSameIdentity expectedRef)
              (freeTypeVarRefsType expectedBodyTy)
            && operationalEndpointTypesAgree
              ( substTypeCaptureRef
                  bodyRef
                  (TVarRef expectedRef)
                  bodyResidual
              )
              expectedBodyTy

        allDistinctRefs [] = True
        allDistinctRefs (ref : refs) =
          not (any (typeBinderRefsSameIdentity ref) refs)
            && allDistinctRefs refs

        selectExpectedCandidate liftedPairs (expectedRef, _) =
          case matchingLifted of
            [liftedExpected@(_, liftedBound)]
              | all
                  (lambdaConstructionBinderBoundsAgree liftedBound . snd)
                  matchingCandidates ->
                  Just liftedExpected
            [] ->
              case matchingCandidates of
                [candidate] -> Just candidate
                _ -> Nothing
            _ -> Nothing
          where
            matchingCandidates =
              [ candidate
              | candidate@(candidateRef, _) <- bodyCompletedCandidates
              , typeBinderRefsSameIdentity candidateRef expectedRef
              ]
            matchingLifted =
              [ liftedExpected
              | (_, liftedExpected@(liftedRef, _)) <- liftedPairs
              , typeBinderRefsSameIdentity liftedRef expectedRef
              ]

        -- The recursively checked owner can complete a declaration after the
        -- packet's candidate spine was frozen.  Coalesce that exact bound in
        -- the same identity and lexical slot before selecting lifted
        -- candidates; otherwise a stale @Nothing@ candidate can mask the
        -- stronger child certificate and make the M/N construction appear
        -- impossible.
        bodyCompletedCandidates =
          fromMaybe candidates
            ( foldM
                mergeCertifiedBodyBinder
                candidates
                certifiedBodyBinders
            )

        sourceProjectExpectedBinder (ref, Nothing) =
          Just (ref, Nothing)
        sourceProjectExpectedBinder (ref, Just bound) = do
          let completedBound = completedCheckedSourceBound ref bound
              projectedBoundTy =
                fromMaybe
                  (sourceProjectCheckedBodyType (tyToElab bound))
                  completedBound
              convertedBound = elabToBound projectedBoundTy
          projectedBound <-
            either (const Nothing) Just convertedBound
          pure (ref, Just projectedBound)

        -- The packet is frozen in the graph presentation of a source
        -- binder's bound before the annotated body has completed that bound.
        -- Replace that presentation only when the same immutable source
        -- identity owns all three declarations (expected, packet, and checked
        -- body) and a checked xMLF computation constructs the body's source
        -- bound from the packet bound.  This is the bound-level M/N witness;
        -- it is intentionally stronger than representative or shape
        -- agreement.
        completedCheckedSourceBound expectedRef expectedBound = do
          guard
            ( any
                (typeBinderRefsSameIdentity expectedRef)
                (IntMap.elems sourceSidecarRefs)
            )
          [(_, Just bodyBound)] <-
            pure
              [ binder
              | binder@(bodyRef, Just _) <-
                  schemeBinderRefs (schemeFromType bodySourceTy)
              , typeBinderRefsSameIdentity bodyRef expectedRef
              ]
          let constructionDeclarations =
                [ declarationBound
                | (declarationRef, Just declarationBound) <-
                    candidates ++ packetConstructionBinders
                , typeBinderRefsSameIdentity declarationRef expectedRef
                ]
          guard (not (null constructionDeclarations))
          guard
            ( all
                ( \declarationBound ->
                    operationalEndpointTypesAgree
                      (tyToElab declarationBound)
                      (tyToElab expectedBound)
                      || isJust
                        ( constructExactInstantiation
                            emptyConstructionTypeEnv
                            exactLambdaEndpointTypesAgree
                            (tyToElab expectedBound)
                            (tyToElab declarationBound)
                        )
                )
                constructionDeclarations
            )
          _ <-
            constructExactInstantiation
              emptyConstructionTypeEnv
              exactLambdaEndpointTypesAgree
              (tyToElab expectedBound)
              (tyToElab bodyBound)
          pure (tyToElab bodyBound)

    -- The packet freezes the exact operated bound of its result declaration
    -- before the lambda body is checked.  The recursive body may initially
    -- close a wider source type around that bound (for example, a source-owned
    -- flexible forall outside the packet's own Gamma).  When the checked
    -- source constructs the packet bound exactly, specialize to that bound
    -- first and only then emit Hyp for the ambient result declaration.  This
    -- keeps the declaration born at S(operated), instead of installing the
    -- wider closed source and repairing it during root validation.
    certifyPacketGammaBoundAmbientRefinement = do
      expectedRef <- expectedBodyBinderRef
      [(ambientRef, ambientBound)] <- ambientExpectedMatches
      guard
        ( typeBinderRefsSameIdentity expectedRef ambientRef
            && operationalEndpointTypesAgree ambientBound TBottom
        )
      TVarRef bodyRef <- pure bodyResultTy
      guard (typeBinderRefsSameIdentity bodyRef ambientRef)
      gammaAuthority <- subtermGeneralizationGammaAuthority packet
      consumerAuthority <- subtermGeneralizationConsumerAuthority packet
      guard
        ( gpaConsumerIdentity gammaAuthority
            == typeBinderRefIdentity ambientRef
            && scaConsumerIdentity consumerAuthority
              == gpaConsumerIdentity gammaAuthority
            && scaEdgeId consumerAuthority == gpaEdgeId gammaAuthority
        )
      let completedBound =
            schemeToType (subtermGeneralizationGammaBoundScheme packet)
          sourceBinderRefs =
            map fst (schemeBinderRefs (schemeFromType bodySourceTy))
          targetFreeRefs = freeTypeVarRefsType completedBound
          collidingSourceBinderRefs =
            [ sourceRef
            | sourceRef <- sourceBinderRefs
            , any
                (typeBinderRefsSameIdentity sourceRef)
                targetFreeRefs
            ]
          (sourceBinderRenames, _) =
            foldl
              freshenSourceBinder
              ( []
              , exactConstructionIdentityGenerator []
              )
              collidingSourceBinderRefs
          freshBodySourceTy =
            renameTypeBinderRefPayloads
              sourceBinderRenames
              bodySourceTy
      _ <- either (const Nothing) Just (elabToBound completedBound)
      sourceSpecialization <-
        constructExactInstantiation
          constructionTypeEnv
          exactLambdaEndpointTypesAgree
          freshBodySourceTy
          completedBound
      let bodyConstruction =
            composeInst
              sourceSpecialization
              (InstAbstrRef ambientRef)
      plan <-
        certifyBodyOption
          freshBodySourceTy
          InstId
          candidates
          expectedTy
          ( bodyConstruction
          , TVarRef ambientRef
          , Just (ambientRef, ambientBound, completedBound)
          , []
          )
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              sourceBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          }

    -- The packet is frozen before the current lambda checks its body, so a
    -- bounded result declaration in the published endpoint can still carry
    -- its pre-body bound.  A private body-consumer refinement records the
    -- exact transition from that packet declaration to the bound constructed
    -- by this owner.  Apply that transition before building Lambda(Gamma):
    -- first construct the checked body at the completed bound, then emit Hyp
    -- for the same declaration identity.  Requiring the packet, expected
    -- endpoint, local candidate, route, and certificate to agree on the one
    -- identity keeps this distinct from completing a binder from final type
    -- shape.
    certifyCoalescedBodyRefinedExpectedBodyOption bodyOption = do
      [ (candidateRef, candidateBound, _completedBound, _certificate)
        ] <- pure bodyRefinedExpectedDeclarations
      let completedBound = tyToElab candidateBound
          coalescedRefinements =
            bodyRefinementsFor candidateRef completedBound
      guard
        ( any
            ( operationalEndpointTypesAgree completedBound
                . bcbrPreviousBound
            )
            coalescedRefinements
        )
      completedExpectedTy <-
        completedBodyRefinedExpectedType candidateRef candidateBound
      plan <-
        certifyBodyOption
          bodySourceTy
          checkedBodyInstantiation
          candidates
          completedExpectedTy
          bodyOption
      pure
        plan
          { exactLambdaConstructionPreservedBodyRefinements =
              nub
                ( coalescedRefinements
                    ++ exactLambdaConstructionPreservedBodyRefinements plan
                )
          }

    certifyBodyRefinedExpectedDeclaration = do
      [ (candidateRef, candidateBound, completedBound, _certificate)
        ] <- pure bodyRefinedExpectedDeclarations
      let sourceBinderRefs =
            map fst (schemeBinderRefs (schemeFromType bodySourceTy))
          completedBoundFreeRefs = freeTypeVarRefsType completedBound
          collidingSourceBinderRefs =
            [ sourceRef
            | sourceRef <- sourceBinderRefs
            , any
                (typeBinderRefsSameIdentity sourceRef)
                completedBoundFreeRefs
            ]
          (sourceBinderRenames, _) =
            foldl
              freshenSourceBinder
              ( []
              , exactConstructionIdentityGenerator []
              )
              collidingSourceBinderRefs
          freshBodySourceTy =
            renameTypeBinderRefPayloads sourceBinderRenames bodySourceTy
          freshBodyResultTy =
            renameTypeBinderRefPayloads sourceBinderRenames bodyResultTy
          freshCheckedBodyInstantiation =
            foldl
              ( \inst (sourceRef, freshRef) ->
                  renameInstBoundRef sourceRef freshRef inst
              )
              checkedBodyInstantiation
              sourceBinderRenames
      bodySpecialization <-
        constructExactInstantiation
          constructionTypeEnv
          exactLambdaEndpointTypesAgree
          freshBodyResultTy
          completedBound
      let bodyConstruction =
            composeInst
              bodySpecialization
              (InstAbstrRef candidateRef)
      completedExpectedTy <-
        completedBodyRefinedExpectedType candidateRef candidateBound
      let preservedDeclarationRefinements =
            bodyRefinementsFor candidateRef completedBound
      plan <-
        certifyBodyOption
          freshBodySourceTy
          freshCheckedBodyInstantiation
          candidates
          completedExpectedTy
          (bodyConstruction, TVarRef candidateRef, Nothing, [])
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              sourceBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          , exactLambdaConstructionPreservedBodyRefinements =
              nub
                ( preservedDeclarationRefinements
                    ++ exactLambdaConstructionPreservedBodyRefinements plan
                )
          }

    -- A nested checked owner can complete an exact ambient declaration while
    -- the enclosing packet still retains that declaration in its frozen
    -- unbounded spine.  At the next lambda boundary, publish the completed
    -- bound in the same lexical slot.  The finalized refinement supplies the
    -- state transition; the child owner-final certificate supplies the exact
    -- body construction; and the packet construction-order map proves that
    -- this declaration belongs to the inherited Lambda(Gamma) spine.
    -- Nothing is recovered from the final arrow shape.
    certifyFinalizedChildAmbientExpectedDeclaration = do
      bodyConstruction <- certifiedBodyConstruction
      [ (candidateRef, candidateBound, certificate)
        ] <- pure
          [ (candidateRef, candidateBound, certificate)
          | (candidateRef, Just candidateBound) <- candidates
          , (expectedRef, Nothing) <- expectedBinders
          , typeBinderRefsSameIdentity candidateRef expectedRef
          , certificate <- bodyRefinements
          , bodyConsumerBoundRefinementOwnerFinalized certificate
          , BodyConsumerEnclosingAmbient route declarationBound <-
              [bcbrDeclarationAuthority certificate]
          , bcrOwner route == certifiedLambdaBodyOwner bodyConstruction
          , typeBinderRefsSameIdentity
              (bcrConstructionRef route)
              candidateRef
          , typeBinderRefsSameIdentity
              (bcbrAmbientRef certificate)
              candidateRef
          , operationalEndpointTypesAgree declarationBound TBottom
          , operationalEndpointTypesAgree
              (bcbrPreviousBound certificate)
              TBottom
          , operationalEndpointTypesAgree
              (bcbrCompletedBound certificate)
              (tyToElab candidateBound)
          , operationalEndpointTypesAgree
              (bcrConstructionOperatedType route)
              (tyToElab candidateBound)
          , constructionOwnsPacketBinder candidateRef
          , not (sourceOwnsPacketBinder candidateRef)
          ]
      guard
        ( operationalEndpointTypesAgree
            (certifiedLambdaBodyConstructedType bodyConstruction)
            bodySourceTy
            && operationalEndpointTypesAgree
              (certifiedLambdaBodyConstructedType bodyConstruction)
              bodyResultTy
        )
      completedExpectedTy <-
        completedBodyRefinedExpectedType candidateRef candidateBound
      plan <-
        certifyBodyOption
          bodySourceTy
          checkedBodyInstantiation
          candidates
          completedExpectedTy
          (InstId, bodyResultTy, Nothing, [])
      pure
        plan
          { exactLambdaConstructionPreservedBodyRefinements =
              nub
                ( certificate
                    : exactLambdaConstructionPreservedBodyRefinements plan
                )
          }

    completedBodyRefinedExpectedType candidateRef candidateBound = do
      let dependencyBinders =
            completedDependencyBinders
              [(candidateRef, Just candidateBound)]
          updatedExpectedBinders =
            [ if typeBinderRefsSameIdentity expectedRef candidateRef
                then (expectedRef, Just candidateBound)
                else binder
            | binder@(expectedRef, _) <- expectedBinders
            ]
          completedPublishedBinders =
            [ fromMaybe candidateBinder
                ( find
                    (typeBinderRefsSameIdentity constructionRef . fst)
                    updatedExpectedBinders
                )
            | candidateBinder@(constructionRef, _) <- candidates
            , any
                (typeBinderRefsSameIdentity constructionRef . fst)
                (dependencyBinders ++ updatedExpectedBinders)
            ]
      guard
        ( length completedPublishedBinders
            == length dependencyBinders + length expectedBinders
        )
      pure
        ( schemeToType
            ( mkElabSchemeWithRefs
                completedPublishedBinders
                (schemeBody expectedScheme)
            )
        )

    bodyRefinementsFor candidateRef completedBound =
      [ refinement
      | refinement <- bodyRefinements
      , let refinementRoute =
              authorizedBodyConsumerRoute
                (bcbrDeclarationAuthority refinement)
      , bcrOwner refinementRoute == owner
      , typeBinderRefsSameIdentity
          (bcrConstructionRef refinementRoute)
          candidateRef
      , typeBinderRefsSameIdentity
          (bcbrAmbientRef refinement)
          candidateRef
      , operationalEndpointTypesAgree
          (bcbrCompletedBound refinement)
          completedBound
      ]

    bodyRefinedExpectedDeclarations =
      [ (candidateRef, candidateBound, completedBound, certificate)
      | (expectedRef, Just expectedBound) <- expectedBinders
      , (candidateRef, Just candidateBound) <- candidates
      , typeBinderRefsSameIdentity expectedRef candidateRef
      , certificate <- bodyRefinements
      , let route =
              authorizedBodyConsumerRoute
                (bcbrDeclarationAuthority certificate)
            previousBound = bcbrPreviousBound certificate
            completedBound = bcbrCompletedBound certificate
      , not (bodyConsumerBoundRefinementOwnerFinalized certificate)
      , bcrOwner route == owner
      , typeBinderRefsSameIdentity
          (bcrConstructionRef route)
          candidateRef
      , typeBinderRefsSameIdentity
          (bcbrAmbientRef certificate)
          candidateRef
      , operationalEndpointTypesAgree
          (bcrConstructionOperatedType route)
          completedBound
      , operationalEndpointTypesAgree
          (tyToElab expectedBound)
          previousBound
      , operationalEndpointTypesAgree
          (tyToElab candidateBound)
          completedBound
      , not
          ( operationalEndpointTypesAgree
              previousBound
              completedBound
          )
      , any
          ( \(packetRef, mbPacketBound) ->
              typeBinderRefsSameIdentity packetRef candidateRef
                && maybe
                  False
                  ( operationalEndpointTypesAgree previousBound
                      . tyToElab
                  )
                  mbPacketBound
          )
          packetConstructionBinders
      , constructionOwnsPacketBinder candidateRef
      , not (sourceOwnsPacketBinder candidateRef)
      ]

    -- A packet can freeze the final bound of a locally emitted result after
    -- the recursive body has already constructed the same declaration at a
    -- wider provisional bound.  The completed packet binder is the positive
    -- authority for the final declaration; its unbounded construction entry
    -- proves that the declaration is born at this lambda, rather than being
    -- inherited from ambient Gamma.  Reconstruct the body computation
    -- directly from its checked source to that frozen bound while retaining
    -- the packet declaration as the vacuous member of Lambda(Gamma).
    -- Replaying the provisional Hyp would make the body end at the flexible
    -- variable instead of the packet's exact operated endpoint.
    certifyPacketCompletedExpectedDeclaration = do
      [ (candidateRef, Just candidateBound)
        ] <- pure
          [ binder
          | binder@(_, Just _) <- candidates
          , any
              (typeBinderRefsSameIdentity (fst binder) . fst)
              expectedBinders
          ]
      (expectedRef, Just expectedBound) <-
        find
          (typeBinderRefsSameIdentity candidateRef . fst)
          expectedBinders
      (packetCompletedRef, Just packetCompletedBound) <-
        find
          (typeBinderRefsSameIdentity candidateRef . fst)
          ( schemeBinderRefs
              ( siScheme
                  (subtermGeneralizationSchemeInfo packet)
              )
          )
      guard
        ( typeBinderRefsSameIdentity candidateRef expectedRef
            && typeBinderRefsSameIdentity candidateRef packetCompletedRef
            && operationalEndpointTypesAgree
              (tyToElab expectedBound)
              (tyToElab packetCompletedBound)
            && not
              ( operationalEndpointTypesAgree
                  (tyToElab candidateBound)
                  (tyToElab expectedBound)
              )
            && any
              ( \(packetRef, packetBound) ->
                  typeBinderRefsSameIdentity candidateRef packetRef
                    && isNothing packetBound
              )
              packetConstructionBinders
            && constructionOwnsPacketBinder candidateRef
            && not (sourceOwnsPacketBinder candidateRef)
        )
      let completedCandidates =
            [ if typeBinderRefsSameIdentity ref candidateRef
                then (ref, Just expectedBound)
                else binder
            | binder@(ref, _) <- candidates
            ]
          expectedBoundTy = tyToElab expectedBound
          sourceBinderRefs =
            map fst (schemeBinderRefs (schemeFromType bodySourceTy))
          targetFreeRefs = freeTypeVarRefsType expectedBoundTy
          collidingSourceBinderRefs =
            [ sourceRef
            | sourceRef <- sourceBinderRefs
            , any
                (typeBinderRefsSameIdentity sourceRef)
                targetFreeRefs
            ]
          (sourceBinderRenames, _) =
            foldl
              freshenSourceBinder
              ( []
              , exactConstructionIdentityGenerator []
              )
              collidingSourceBinderRefs
          freshBodySourceTy =
            renameTypeBinderRefPayloads
              sourceBinderRenames
              bodySourceTy
          completedTypeEnv =
            constructionTypeEnvFor completedCandidates
      bodySpecialization <-
        constructExactInstantiation
          completedTypeEnv
          exactLambdaEndpointTypesAgree
          freshBodySourceTy
          expectedBoundTy
      plan <-
        certifyBodyOption
          freshBodySourceTy
          InstId
          completedCandidates
          expectedTy
          ( bodySpecialization
          , expectedBoundTy
          , Nothing
          , []
          )
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              sourceBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          }

    -- The checked body computation is provisional evidence for the result
    -- selected before the enclosing exact Gamma is known.  It must not become
    -- an irreversible prefix of the final xMLF construction.  When the exact
    -- codomain requires a different legal route (notably the paper's nested
    -- annotated self-application), construct the complete computation from
    -- the checked source type and certify that endpoint directly.
    certifyExactBodySourceEndpoint = do
      (sourceBinderRenames, freshBodySourceTy, expectedBodyTy) <-
        exactBodySourceEndpointInputs
      sourceInstantiation <-
        constructExactInstantiation
          constructionTypeEnv
          exactLambdaEndpointTypesAgree
          freshBodySourceTy
          expectedBodyTy
      plan <-
        certifyBodyOption
          freshBodySourceTy
          sourceInstantiation
          candidates
          expectedTy
          (InstId, expectedBodyTy, Nothing, [])
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              sourceBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          }

    -- A completed child can expose one open identity which the enclosing body
    -- packet generalizes.  It can originate either in a checked value-lambda
    -- parameter or in an immutable source annotation.  Figure 15.3.5
    -- constructs that declaration as part of the child's @Lambda(Gamma)@
    -- term, not with @O@: @O@ is necessarily vacuous and cannot capture the
    -- occurrence in the checked child type.  The private owner-final
    -- certificate records the exact origin in both cases.  The body may still
    -- have a leading bounded result computation, so construct its exact
    -- residual first and then emit the certified abstraction.
    certifyOpenExpectedBodyAbstraction = do
      TArrow expectedParamTy expectedBodyTy <-
        pure (schemeBody expectedScheme)
      guard (operationalEndpointTypesAgree expectedParamTy paramTy)
      let expectedBodyScheme = schemeFromType expectedBodyTy
      [(expectedRef, Nothing)] <-
        pure (schemeBinderRefs expectedBodyScheme)
      bodyConstruction <- certifiedBodyConstruction
      guard
        ( ( any
              (typeBinderRefsSameIdentity expectedRef)
              (certifiedLambdaBodySourceRefs bodyConstruction)
            || any
              (typeBinderRefsSameIdentity expectedRef)
              (certifiedLambdaBodyParameterRefs bodyConstruction)
          )
            && any
              (typeBinderRefsSameIdentity expectedRef)
              (freeTypeVarRefsType (schemeBody expectedBodyScheme))
            && operationalEndpointTypesAgree
              (certifiedLambdaBodyConstructedType bodyConstruction)
              bodySourceTy
        )
      let expectedBodyResidual = schemeBody expectedBodyScheme
      (sourceBinderRenames, freshBodySourceTy, sourceSpecialization) <-
        ( do
            spinePlan <-
              planExactBinderSpine
                exactLambdaEndpointTypesAgree
                bodySourceTy
                expectedBodyResidual
            let binderRenames = exactBinderSpineRenames spinePlan
            pure
              ( binderRenames
              , renameTypeBinderRefPayloads binderRenames bodySourceTy
              , exactBinderSpineInstantiation spinePlan
              )
        )
          <|> do
            specialization <-
              constructExactInstantiation
                constructionTypeEnv
                exactLambdaEndpointTypesAgree
                bodySourceTy
                expectedBodyResidual
            pure ([], bodySourceTy, specialization)
      let completedCandidateClaims =
            [ (candidateRef, expectedBound)
            | (candidateRef, Just candidateBound) <- candidates
            , (expectedCandidateRef, Just expectedBound) <- expectedBinders
            , typeBinderRefsSameIdentity
                candidateRef
                expectedCandidateRef
            , exactLambdaEndpointTypesAgree
                (tyToElab candidateBound)
                bodySourceTy
            , exactLambdaEndpointTypesAgree
                (tyToElab expectedBound)
                expectedBodyTy
            ]
      completedCandidates <-
        case completedCandidateClaims of
          [] -> pure candidates
          [(completedRef, completedBound)] ->
            pure
              [ if typeBinderRefsSameIdentity candidateRef completedRef
                  then (candidateRef, Just completedBound)
                  else candidate
              | candidate@(candidateRef, _) <- candidates
              ]
          _ -> Nothing
      plan <-
        certifyBodyOptionWithAbstractions
          [(expectedRef, Nothing)]
          freshBodySourceTy
          InstId
          completedCandidates
          expectedTy
          (sourceSpecialization, expectedBodyTy, Nothing, [])
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              sourceBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          }

    exactBodySourceEndpointInputs = do
      TArrow _ expectedBodyTy <- pure (schemeBody expectedScheme)
      let sourceBinderRefs =
            map fst (schemeBinderRefs (schemeFromType bodySourceTy))
          collidingSourceBinderRefs =
            [ sourceRef
            | sourceRef <- sourceBinderRefs
            , any
                (typeBinderRefsSameIdentity sourceRef)
                bodyConstructionOuterRefs
            ]
          (sourceBinderRenames, _) =
            foldl
              freshenSourceBinder
              ( []
              , exactConstructionIdentityGenerator []
              )
              collidingSourceBinderRefs
          freshBodySourceTy =
            renameTypeBinderRefPayloads
              sourceBinderRenames
              bodySourceTy
      pure (sourceBinderRenames, freshBodySourceTy, expectedBodyTy)

    -- The recursive body was checked before this Lambda(Gamma) spine was
    -- selected.  Its source forall binders therefore may reuse an identity
    -- which is now already open in the exact construction environment.  Such
    -- a declaration must be alpha-copied before the final body computation is
    -- planned; renaming it after planning is too late for InstApp to
    -- distinguish the lexical binder from its outer argument.
    bodyConstructionOuterRefs =
      foldr insertOuterRef []
        ( map fst candidates
            ++ Map.keys ambientBindings
            ++ freeTypeVarRefsType paramTy
            ++ freeTypeVarRefsType expectedTy
            ++ concatMap freeTypeVarRefsType (Map.elems ambientBindings)
            ++ concatMap
              (maybe [] (freeTypeVarRefsType . tyToElab) . snd)
              candidates
        )
      where
        insertOuterRef ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

    -- Opening a source-owned flexible quantifier at a free occurrence of that
    -- same source identity would otherwise turn one identity into both a
    -- lexical declaration and an ambient argument.  Give the lexical
    -- declaration a fresh alpha-copy first; the InstApp payload then denotes
    -- only the enclosing source identity.
    freshenSourceBinder (renames, generator) sourceRef =
      if typeBinderIdentityIsCanonicalStructural
          (typeBinderRefIdentity sourceRef)
        then (renames, generator)
        else
          let (freshRef, nextGenerator) =
                freshenTypeBinderRef sourceRef generator
           in (renames ++ [(sourceRef, freshRef)], nextGenerator)

    -- A completed packet-body declaration fixes both the exact result
    -- identity and the bound consumed by its terminal Hyp.  If the recursive
    -- body still publishes a more-general leading-forall construction (the
    -- paper's @g g@ value under an applied wrapper is the canonical case),
    -- build precisely the binder-spine specialization certified by that
    -- declaration, then emit Hyp for the same identity.  Requiring the
    -- candidate and frozen packet declarations to agree prevents a
    -- type-shaped ambient peer from nominating this path.
    completedPacketBodyOptions = do
      packetBodyRef <- maybeToList packetBodyBinderRef
      (candidateRef, Just candidateBound) <-
        [ binder
        | binder@(ref, Just _) <- candidates
        , typeBinderRefsSameIdentity ref packetBodyRef
        ]
      (_, Just packetBound) <-
        [ binder
        | binder@(ref, Just _) <- packetConstructionBinders
        , typeBinderRefsSameIdentity ref candidateRef
        ]
      guard
        ( operationalEndpointTypesAgree
            (tyToElab candidateBound)
            (tyToElab packetBound)
        )
      specialization <-
        maybeToList
          ( constructExactInstantiation
              constructionTypeEnv
              exactLambdaEndpointTypesAgree
              bodyResultTy
              (tyToElab candidateBound)
          )
      guard (specialization /= InstId)
      let completedInstantiation =
            composeInst
              specialization
              (InstAbstrRef candidateRef)
      completedTy <-
        either
          (const [])
          pure
          ( TypeCheck.checkInstantiation
              constructionTypeEnv
              bodyResultTy
              completedInstantiation
          )
      guard
        ( case completedTy of
            TVarRef completedRef ->
              typeBinderRefsSameIdentity completedRef candidateRef
            _ -> False
        )
      pure
        ( completedInstantiation
        , TVarRef candidateRef
        , Nothing
        , []
        )

    -- A sibling can reach an exact instance of the declaration that the
    -- enclosing lambda will publish.  The enclosing endpoint fixes the
    -- declaration identity and generalized bound; an exact binder-spine plan
    -- from that bound to the current candidate proves the relationship; and
    -- the checked body must independently construct the generalized bound
    -- before Hyp abstracts it to the shared declaration.
    certifyGeneralizedExpectedBodyDeclaration = do
      expectedBodyRef <- expectedBodyBinderRef
      (candidateRef, Just candidateBound) <-
        find
          (typeBinderRefsSameIdentity expectedBodyRef . fst)
          candidates
      (_, Just expectedBound) <-
        find
          (typeBinderRefsSameIdentity expectedBodyRef . fst)
          expectedBinders
      guard
        ( not
            ( operationalEndpointTypesAgree
                (tyToElab expectedBound)
                (tyToElab candidateBound)
            )
        )
      specializationPlan <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          (tyToElab expectedBound)
          (tyToElab candidateBound)
      guard
        (exactBinderSpineInstantiation specializationPlan /= InstId)
      let generalizedCandidates =
            [ if typeBinderRefsSameIdentity ref candidateRef
                then (ref, Just expectedBound)
                else binder
            | binder@(ref, _) <- candidates
            ]
          generalizedTypeEnv =
            constructionTypeEnvFor generalizedCandidates
      generalizeBody <-
        constructExactInstantiation
          generalizedTypeEnv
          exactLambdaEndpointTypesAgree
          bodyResultTy
          (tyToElab expectedBound)
      let bodyInstantiation =
            composeInst
              generalizeBody
              (InstAbstrRef candidateRef)
      constructedBodyTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              generalizedTypeEnv
              bodyResultTy
              bodyInstantiation
          )
      guard
        ( case constructedBodyTy of
            TVarRef constructedRef ->
              typeBinderRefsSameIdentity constructedRef candidateRef
            _ -> False
        )
      certifyBodyOption
        bodySourceTy
        checkedBodyInstantiation
        generalizedCandidates
        expectedTy
        (bodyInstantiation, TVarRef candidateRef, Nothing, [])

    constructionTypeEnv =
      constructionTypeEnvFor candidates

    constructionTypeEnvFor binders =
      constructionTypeEnvForAmbient ambientBindings binders

    constructionTypeEnvForAmbient ambient binders =
      foldr
        ( \(ref, mbBound) env ->
            TypeCheck.insertTypeBindingRef
              ref
              (maybe TBottom tyToElab mbBound)
              env
        )
        (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] ambient)
        binders

    -- A pending declaration is completed by this exact lambda construction,
    -- not after its body computation has already been accepted.  Validate the
    -- terminal Hyp in the completed Gamma selected by the same refinement
    -- proof that the plan returns to its caller.  In particular, checking
    -- @Hyp(a)@ while @a : Bottom@ is still installed would reject the very
    -- source-to-bound computation whose checked body determines @a@.
    refinedAmbientBindingsForBody Nothing =
      Just ambientBindings
    refinedAmbientBindingsForBody
      (Just (exactRef, previousBound, completedBound)) = do
        [(ambientRef, currentBound)] <-
          pure
            [ binding
            | binding@(ref, _) <- Map.toList ambientBindings
            , typeBinderRefsSameIdentity ref exactRef
            ]
        guard
          ( operationalEndpointTypesAgree currentBound previousBound
              && typeBinderRefsSameIdentity ambientRef exactRef
          )
        pure
          ( Map.insert
              ambientRef
              completedBound
              ( Map.filterWithKey
                  ( \ref _ ->
                      not
                        (typeBinderRefsSameIdentity ref ambientRef)
                  )
                  ambientBindings
              )
          )

    -- The ordinary lambda construction can complete a packet result that is
    -- already declared by the enclosing Gamma.  In that case the completed
    -- binder is not part of this lambda's local Lambda(Gamma) spine: the body
    -- has already reached the exact ambient identity, and this boundary only
    -- advances that declaration from its packet-authorized pending Bottom to
    -- the checked local bound.  Keep the refinement in the construction plan
    -- so the caller updates the ambient type environment before validating the
    -- completed lambda.
    ambientPublishedBodyRefinement = do
      expectedRef <- expectedBodyBinderRef
      [(ambientRef, ambientBound)] <- ambientExpectedMatches
      guard
        ( typeBinderRefsSameIdentity expectedRef ambientRef
            && operationalEndpointTypesAgree ambientBound TBottom
        )
      TVarRef bodyRef <- pure bodyResultTy
      guard (typeBinderRefsSameIdentity bodyRef ambientRef)
      (candidateRef, Just completedBound) <-
        case
            [ binder
            | binder@(ref, Just _) <- completedConstructionCandidates
            , typeBinderRefsSameIdentity ref ambientRef
            ]
          of
            [binder] -> Just binder
            _ -> Nothing
      guard
        ( any
            ( \(packetRef, packetBound) ->
                typeBinderRefsSameIdentity candidateRef packetRef
                  && isNothing packetBound
            )
            packetConstructionBinders
        )
      pure (ambientRef, ambientBound, tyToElab completedBound)

    -- A declaration already present in the enclosing construction Gamma is
    -- not emitted again by this lambda's local Gen(Gamma,tau) spine.  The
    -- exact expected codomain selects the declaration identity, while the
    -- ambient environment and the completed local candidate independently
    -- establish its bound.  A still-provisional ambient declaration follows
    -- the refinement path above; an already-completed one is simply consumed
    -- by the body's exact Hyp computation.
    ambientPublishedBodyDeclarationRef =
      ( \(ref, _, _) -> ref
      )
        <$> ambientPublishedBodyRefinement
        <|> do
          expectedRef <- expectedBodyBinderRef
          [(ambientRef, ambientBound)] <- ambientExpectedMatches
          guard (typeBinderRefsSameIdentity expectedRef ambientRef)
          [(_, Just candidateBound)] <-
            pure
              [ binder
              | binder@(candidateRef, Just _) <-
                  completedConstructionCandidates
              , typeBinderRefsSameIdentity candidateRef ambientRef
              ]
          guard
            ( operationalEndpointTypesAgree
                (tyToElab candidateBound)
                ambientBound
            )
          pure ambientRef

    -- The enclosing exact endpoint can expose a free declaration whose bound
    -- is still the packet's pre-body Bottom placeholder.  The endpoint fixes
    -- the declaration identity, the checked body fixes its bound, and the
    -- ambient construction environment proves that the declaration is
    -- already open.  Keep that three-part authority in the returned plan so
    -- the caller both emits Hyp and advances the exact declaration before
    -- typechecking the lambda.
    ambientBodyOptions =
      case ambientExpectedMatches of
        Nothing -> []
        Just matches ->
          case matches of
              [(ambientRef, ambientBound)]
                | operationalEndpointTypesAgree
                    bodyResultTy
                    ambientBound ->
                    [ ( InstAbstrRef ambientRef
                      , TVarRef ambientRef
                      , Nothing
                      , []
                      )
                    ]
                | operationalEndpointTypesAgree ambientBound TBottom
                , TVarRef bodyRef <- bodyResultTy
                , not
                    (typeBinderRefsSameIdentity ambientRef bodyRef)
                , Just exactBodyRef <- ambientPeerRef bodyRef ->
                    [ ( InstId
                      , TVarRef exactBodyRef
                      , Nothing
                      , [(ambientRef, exactBodyRef)]
                      )
                    ]
                | operationalEndpointTypesAgree ambientBound TBottom
                , Right _ <- elabToBound bodyResultTy ->
                    [ ( InstAbstrRef ambientRef
                      , TVarRef ambientRef
                      , Just (ambientRef, ambientBound, bodyResultTy)
                      , []
                      )
                    ]
              _ -> []

    -- Only a free result occurrence can denote a declaration already open in
    -- the enclosing construction Gamma.  The same graph identity can also be
    -- present in the ambient checking environment while the exact endpoint
    -- binds it locally; treating that shadowed occurrence as ambient would
    -- remove the binder from this lambda's certified Lambda(Gamma) spine.
    ambientExpectedMatches = do
      expectedRef <- expectedBodyBinderRef
      guard
        ( any
            (typeBinderRefsSameIdentity expectedRef)
            expectedFreeRefs
        )
      pure
        [ (ambientRef, ambientBound)
        | (ambientRef, ambientBound) <- Map.toList ambientBindings
        , typeBinderRefsSameIdentity ambientRef expectedRef
        ]

    ambientPeerRef soughtRef =
      case
          [ ambientRef
          | ambientRef <- Map.keys ambientBindings
          , typeBinderRefsSameIdentity ambientRef soughtRef
          ]
        of
          [ambientRef] -> Just ambientRef
          _ -> Nothing

    -- Packet preparation precedes Typ(body), so its raw consumer
    -- construction may still carry an unbounded placeholder for a declaration
    -- whose local Gamma bound is now known.  First prove that replacing only
    -- those packet-authorized declarations by their raw unbounded state
    -- reconstructs the exact enclosing endpoint.  The retained-binder
    -- correspondence from that proof then completes the endpoint with the
    -- checked local bounds, after which the ordinary exact construction
    -- planner validates the real xMLF computation.
    certifyCompletedBodyOption
      bodyOption@(_, constructedBodyTy0, _, bodyPeerRenames) = do
      guard (null bodyPeerRenames)
      let provisionalCandidates =
            map provisionalPacketBinder candidates
          completedCandidates =
            [ binder
            | (binder, provisional) <-
                zip candidates provisionalCandidates
            , binder /= provisional
            ]
      guard (not (null completedCandidates))
      completedExpectedTy <-
        firstCompletedExpectedType
          provisionalCandidates
          constructedBodyTy0
          completedCandidates
          ( maybeToList
              ( expectedWithCompletedDependencies
                  completedCandidates
              )
              ++ [expectedTy]
          )
      certifyBodyOption
        bodySourceTy
        checkedBodyInstantiation
        candidates
        completedExpectedTy
        bodyOption

    -- Typ(body) can complete a pending packet result with a bound that depends
    -- on declarations generalized at the same lambda.  Those declarations
    -- were absent when the enclosing endpoint was frozen, but they are part of
    -- the completed Gen(Gamma, tau) spine: for example,
    --
    --   forall a. forall (b >= a -> Bool). p -> b
    --
    -- completes a frozen @forall c. p -> c@ as
    --
    --   forall a. forall (c >= a -> Bool). p -> c.
    --
    -- Add only the exact dependency closure of the completed packet binders,
    -- preserving the already dependency-ordered candidate spine.  The two
    -- exact binder-spine checks below remain the authority that this expanded
    -- endpoint is constructible; no display name or type-only correspondence
    -- selects a declaration.
    expectedWithCompletedDependencies completedCandidates = do
      case completedDependencyBinders completedCandidates of
        [] -> Just expectedTy
        dependencyBinders -> do
          let completedEndpointBinders =
                [ fromMaybe
                    candidate
                    ( find
                        (typeBinderRefsSameIdentity candidateRef . fst)
                        expectedBinders
                    )
                | candidate@(candidateRef, _) <- candidates
                , any
                    (typeBinderRefsSameIdentity candidateRef . fst)
                    (dependencyBinders ++ expectedBinders)
                ]
          guard
            ( length completedEndpointBinders
                == length dependencyBinders + length expectedBinders
            )
          pure
            ( schemeToType
                ( mkElabSchemeWithRefs
                    completedEndpointBinders
                    (schemeBody expectedScheme)
                )
            )

    completedDependencyBinders completedCandidates =
      [ binder
      | binder@(candidateRef, _) <- candidates
      , dependencyRefMember candidateRef dependencyRefs
      , not
          ( any
              (typeBinderRefsSameIdentity candidateRef . fst)
              completedCandidates
          )
      , not
          ( any
              (typeBinderRefsSameIdentity candidateRef . fst)
              expectedBinders
          )
      ]
      where
        dependencyRefs =
          dependencyClosure
            [ dependencyRef
            | (_, Just completedBound) <- completedCandidates
            , dependencyRef <- freeTypeVarRefsType (tyToElab completedBound)
            ]

        dependencyClosure refs =
          let nextRefs =
                refs
                  ++ [ dependencyRef
                     | (candidateRef, Just candidateBound) <- candidates
                     , dependencyRefMember candidateRef refs
                     , dependencyRef <-
                         freeTypeVarRefsType (tyToElab candidateBound)
                     ]
              uniqueRefs = foldr insertDependencyRef [] nextRefs
           in if length uniqueRefs == length refs
                then uniqueRefs
                else dependencyClosure uniqueRefs

        insertDependencyRef ref refs
          | dependencyRefMember ref refs = refs
          | otherwise = ref : refs

    dependencyRefMember ref =
      any (typeBinderRefsSameIdentity ref)

    firstCompletedExpectedType _ _ _ [] = Nothing
    firstCompletedExpectedType
      provisionalCandidates
      constructedBodyTy0
      completedCandidates
      (publishedTy : remaining) =
      case
          planExactBinderSpine
            exactLambdaEndpointTypesAgree
            ( constructionType
                provisionalCandidates
                paramTy
                constructedBodyTy0
            )
            publishedTy
        of
          Just plan ->
            completeExpectedPacketBinders
              publishedTy
              completedCandidates
              plan
              <|> completeEliminatedPacketBodyBinder
                publishedTy
                provisionalCandidates
                constructedBodyTy0
                completedCandidates
                plan
              <|> tryRemaining
          Nothing ->
            tryRemaining
      where
        tryRemaining =
          firstCompletedExpectedType
            provisionalCandidates
            constructedBodyTy0
            completedCandidates
            remaining

    -- Packet preparation freezes the enclosing endpoint before the recursive
    -- body is checked.  If the packet's own arrow codomain is an unbounded
    -- declaration, the checked body completes that exact declaration.  First
    -- plan the unbounded binder spine, use its retained-binder correspondence
    -- to install the checked bound at the exact enclosing codomain, and then
    -- require a second exact plan from that completed construction.  The
    -- packet codomain identity and the two successful plans are the authority
    -- here, not the body's shape or a target binder's display name.
    certifyPendingPacketBodyBinder = do
      packetBodyRef <- packetBodyBinderRef
      (candidateRef, Nothing) <-
        find
          (typeBinderRefsSameIdentity packetBodyRef . fst)
          candidates
      guard
        ( any
            ( \(packetRef, packetBound) ->
                typeBinderRefsSameIdentity candidateRef packetRef
                  && isNothing packetBound
            )
            packetConstructionBinders
        )
      checkedBodyBound <-
        either (const Nothing) Just (elabToBound bodyResultTy)
      let completedBinder = (candidateRef, Just checkedBodyBound)
          completedCandidates =
            [ if typeBinderRefsSameIdentity ref candidateRef
                then completedBinder
                else binder
            | binder@(ref, _) <- candidates
            ]
          bodyOption =
            (InstAbstrRef candidateRef, TVarRef candidateRef, Nothing, [])
      provisionalPlan <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          ( constructionType
              candidates
              paramTy
              (TVarRef candidateRef)
          )
          expectedTy
      retainedTarget <-
        findRetainedTarget
          candidateRef
          (exactBinderSpineRenames provisionalPlan)
          expectedBinders
      expectedBodyRef <- expectedBodyBinderRef
      guard
        (typeBinderRefsSameIdentity retainedTarget expectedBodyRef)
      completedExpectedTy <-
        completeExpectedPacketBinders
          expectedTy
          [completedBinder]
          provisionalPlan
      certifyBodyOption
        bodySourceTy
        checkedBodyInstantiation
        completedCandidates
        completedExpectedTy
        bodyOption

    -- The checked body can already have published itself through the packet
    -- result's provisional Hyp before the enclosing lambda freezes the final
    -- result bound.  In that case @bodyResultTy@ is the packet variable and
    -- no longer exposes the source forall spine which constructs its bound.
    -- Rebuild that exact computation from @Typ(body)@: retain each
    -- source-owned dependency in the enclosing Lambda(Gamma), N-open the
    -- alpha-copied body forall at that declaration, then apply Hyp at the
    -- completed packet result.  The provisional unbounded packet plan proves
    -- which result slot is eliminated by the enclosing spine; completing that
    -- eliminated slot publishes its checked residual as the lambda codomain.
    certifySourceOpenedPacketBodySpine = do
      ( freshBodySourceTy
        , completedCandidates
        , completedExpectedTy
        , candidateRef
        , bodyInstantiation
        , bodyBinderRenames
        ) <- prepareSourceOpenedPacketBodySpine
      plan <-
        certifyBodyOption
          freshBodySourceTy
          InstId
          completedCandidates
          completedExpectedTy
          (bodyInstantiation, TVarRef candidateRef, Nothing, [])
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              bodyBinderRenames
          }

    prepareSourceOpenedPacketBodySpine = do
      checkedResultRef <- outgoingResultRef checkedBodyInstantiation
      packetResultRef <-
        subtermGeneralizationResultAbstractionRef packet
      guard
        (typeBinderRefsSameIdentity checkedResultRef packetResultRef)
      (candidateRef, Just candidateBound) <-
        find
          (typeBinderRefsSameIdentity checkedResultRef . fst)
          candidates
      guard
        ( not (sourceOwnsPacketBinder candidateRef)
            && any
              ( \(packetRef, packetBound) ->
                  typeBinderRefsSameIdentity candidateRef packetRef
                    && isNothing packetBound
              )
              packetConstructionBinders
            && operationalEndpointTypesAgree
              (tyToElab candidateBound)
              bodySourceTy
        )
      bodyResultRef <-
        case bodyResultTy of
          TVarRef ref -> Just ref
          _ -> Nothing
      guard (typeBinderRefsSameIdentity bodyResultRef candidateRef)
      checkedPublishedTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              (constructionTypeEnvFor candidates)
              bodySourceTy
              checkedBodyInstantiation
          )
      guard
        ( case checkedPublishedTy of
            TVarRef ref ->
              typeBinderRefsSameIdentity ref candidateRef
            _ -> False
        )
      let (checkedBodyBinders, _) = splitForallsRefs bodySourceTy
          candidatePrefix =
            takeWhile
              (not . typeBinderRefsSameIdentity candidateRef . fst)
              candidates
      guard (not (null checkedBodyBinders))
      dependencyCandidates <-
        traverse
          (checkedBodyDependencyCandidate candidatePrefix)
          checkedBodyBinders
      bodyBinderRenames <-
        traverse checkedBodyBinderCopy checkedBodyBinders
      let freshBodySourceTy =
            renameTypeBinderRefPayloads
              bodyBinderRenames
              bodySourceTy
          (freshBodyBinders, freshBodyResidual) =
            splitForallsRefs freshBodySourceTy
          freshToCandidateRenames =
            zipWith
              (\(freshRef, _) (candidateDependencyRef, _) ->
                  (freshRef, candidateDependencyRef)
              )
              freshBodyBinders
              dependencyCandidates
          completedDependencies =
            zipWith
              (completeCheckedBodyDependency freshToCandidateRenames)
              dependencyCandidates
              freshBodyBinders
          checkedBodyResidual =
            renameTypeBinderRefPayloads
              freshToCandidateRenames
              freshBodyResidual
      completedResidualBound <-
        either
          (const Nothing)
          Just
          (elabToBound checkedBodyResidual)
      let completedBodyCandidate =
            (candidateRef, Just completedResidualBound)
          completedBinders =
            completedDependencies ++ [completedBodyCandidate]
          completedCandidates =
            map
              (replaceCompletedBinder completedBinders)
              candidates
          provisionalCandidates = map provisionalPacketBinder candidates
      guard
        ( any
            (uncurry binderCompletionChanged)
            (zip candidates completedCandidates)
        )
      provisionalPlan <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          ( constructionType
              provisionalCandidates
              paramTy
              (TVarRef candidateRef)
          )
          expectedTy
      completedExpectedTy <-
        completeEliminatedPacketBodyBinder
          expectedTy
          provisionalCandidates
          (TVarRef candidateRef)
          [completedBodyCandidate]
          provisionalPlan
      let completedTypeEnv =
            constructionTypeEnvFor completedCandidates
      checkedBodySpecialization <-
        constructExactInstantiation
          completedTypeEnv
          exactLambdaEndpointTypesAgree
          freshBodySourceTy
          checkedBodyResidual
      let bodyInstantiation =
            composeInst
              checkedBodySpecialization
              (InstAbstrRef candidateRef)
      constructedBodyTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              completedTypeEnv
              freshBodySourceTy
              bodyInstantiation
          )
      guard
        ( case constructedBodyTy of
            TVarRef constructedRef ->
              typeBinderRefsSameIdentity constructedRef candidateRef
            _ -> False
        )
      pure
        ( freshBodySourceTy
        , completedCandidates
        , completedExpectedTy
        , candidateRef
        , bodyInstantiation
        , bodyBinderRenames
        )

    -- A descendant can finish a construction-owned packet body after the
    -- enclosing endpoint has already frozen an older bounded presentation.
    -- The checked body supplies an exact leading forall spine and residual;
    -- match those declarations to the packet candidates by identity, install
    -- their checked bounds, and use the residual as the exact packet-body
    -- bound.  The resulting @InstApp ...; Hyp(result)@ is then validated by
    -- the ordinary exact binder-spine planner.
    --
    -- The completed packet body itself must remain construction-owned.  A
    -- source-owned declaration may occur in the dependency prefix, but only
    -- at the identical packet source declaration and bound.  In that case it
    -- is retained unchanged and used as the explicit N argument which opens
    -- the checked body's source forall; its ABI bound and vacuity are never
    -- completed or inferred by this branch.
    certifyCompletedPacketBodySpine = do
      packetBodyRef <- packetBodyBinderRef
      (candidateRef, Just _) <-
        find
          (typeBinderRefsSameIdentity packetBodyRef . fst)
          candidates
      guard
        ( constructionOwnsPacketBinder candidateRef
            && not (sourceOwnsPacketBinder candidateRef)
        )
      let (checkedBodyBinders, _) =
            splitForallsRefs bodyResultTy
          candidatePrefix =
            takeWhile
              (not . typeBinderRefsSameIdentity candidateRef . fst)
              candidates
      dependencyCandidates <-
        traverse
          (checkedBodyDependencyCandidate candidatePrefix)
          checkedBodyBinders
      bodyBinderRenames <-
        traverse checkedBodyBinderCopy checkedBodyBinders
      let freshBodyResultTy =
            renameTypeBinderRefPayloads
              bodyBinderRenames
              bodyResultTy
          freshBodySourceTy =
            renameTypeBinderRefPayloads
              bodyBinderRenames
              bodySourceTy
          freshCheckedBodyInstantiation =
            foldl
              ( \inst (sourceRef, targetRef) ->
                  renameInstBoundRef sourceRef targetRef inst
              )
              checkedBodyInstantiation
              bodyBinderRenames
          (freshBodyBinders, freshBodyResidual) =
            splitForallsRefs freshBodyResultTy
          freshToCandidateRenames =
            zipWith
              (\(freshRef, _) (candidateDependencyRef, _) ->
                  (freshRef, candidateDependencyRef)
              )
              freshBodyBinders
              dependencyCandidates
          completedDependencies =
            zipWith
              (completeCheckedBodyDependency freshToCandidateRenames)
              dependencyCandidates
              freshBodyBinders
          checkedBodyResidual =
            renameTypeBinderRefPayloads
              freshToCandidateRenames
              freshBodyResidual
      completedResidualBound <-
        either
          (const Nothing)
          Just
          (elabToBound checkedBodyResidual)
      let completedBodyCandidate =
            (candidateRef, Just completedResidualBound)
          completedBinders =
            completedDependencies ++ [completedBodyCandidate]
          completedCandidates =
            map
              (replaceCompletedBinder completedBinders)
              candidates
      guard
        ( any
            (uncurry binderCompletionChanged)
            (zip candidates completedCandidates)
        )
      provisionalPlan <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          ( constructionType
              candidates
              paramTy
              (TVarRef candidateRef)
          )
          expectedTy
      completedExpectedTy <-
        completeExpectedPacketBinders
          expectedTy
          completedBinders
          provisionalPlan
      let completedTypeEnv =
            constructionTypeEnvFor completedCandidates
      checkedBodySpecialization <-
        constructExactInstantiation
          completedTypeEnv
          exactLambdaEndpointTypesAgree
          freshBodyResultTy
          checkedBodyResidual
      let bodyInstantiation =
            composeInst
              checkedBodySpecialization
              (InstAbstrRef candidateRef)
      constructedBodyTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              completedTypeEnv
              freshBodyResultTy
              bodyInstantiation
          )
      guard
        ( case constructedBodyTy of
            TVarRef constructedRef ->
              typeBinderRefsSameIdentity constructedRef candidateRef
            _ -> False
        )
      plan <-
        certifyBodyOption
          freshBodySourceTy
          freshCheckedBodyInstantiation
          completedCandidates
          completedExpectedTy
          (bodyInstantiation, TVarRef candidateRef, Nothing, [])
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              bodyBinderRenames
          }

    checkedBodyDependencyCandidate candidatePrefix (checkedRef, checkedBound) = do
      candidate@(candidateRef, candidateBound) <-
        find
          (typeBinderRefsSameIdentity checkedRef . fst)
          candidatePrefix
      guard
        ( if sourceOwnsPacketBinder candidateRef
            then
              sourceOwnsPacketBinder checkedRef
                && lambdaConstructionBinderBoundsAgree
                  checkedBound
                  candidateBound
                && any
                  ( \(packetRef, packetBound) ->
                      typeBinderRefsSameIdentity packetRef candidateRef
                        && lambdaConstructionBinderBoundsAgree
                          checkedBound
                          packetBound
                  )
                  packetConstructionBinders
            else constructionOwnsPacketBinder candidateRef
        )
      pure candidate

    checkedBodyBinderCopy (checkedRef, _) = do
      copyRef <-
        case
            [ candidate
            | (identity, candidate) <-
                subtermGeneralizationCopiedBinderRoutes packet
            , identity == typeBinderRefIdentity checkedRef
            ]
        of
          [candidate] -> Just candidate
          _ -> Nothing
      guard
        ( not (typeBinderRefsSameIdentity checkedRef copyRef)
            && not
              ( any
                  (typeBinderRefsSameIdentity copyRef . fst)
                  candidates
              )
        )
      pure (checkedRef, copyRef)

    completeCheckedBodyDependency
      freshToCandidateRenames
      (candidateRef, _)
      (_, freshBound) =
        ( candidateRef
        , fmap
            ( renameBoundTypeBinderRefPayloads
                freshToCandidateRenames
            )
            freshBound
        )

    replaceCompletedBinder replacements binder@(ref, _) =
      fromMaybe
        binder
        (find (typeBinderRefsSameIdentity ref . fst) replacements)

    binderCompletionChanged
      (leftRef, leftBound)
      (rightRef, rightBound) =
        typeBinderRefsSameIdentity leftRef rightRef
          && case (leftBound, rightBound) of
            (Nothing, Nothing) -> False
            (Just left, Just right) ->
              not
                ( operationalEndpointTypesAgree
                    (tyToElab left)
                    (tyToElab right)
                )
            _ -> True

    packetConstructionSchemeInfo =
      subtermGeneralizationConsumerConstructionSchemeInfo packet

    constructionOwnsPacketBinder ref =
      any
        (typeBinderRefsSameIdentity ref)
        (IntMap.elems (siConstructionBinderOrderRefs packetConstructionSchemeInfo))

    sourceOwnsPacketBinder ref =
      any
        (typeBinderRefsSameIdentity ref)
        (IntMap.elems (siSourceBinderOrderRefs packetConstructionSchemeInfo))

    provisionalPacketBinder binder@(candidateRef, mbBound)
      | isJust mbBound
      , any
          ( \(packetRef, packetBound) ->
              typeBinderRefsSameIdentity candidateRef packetRef
                && isNothing packetBound
          )
          packetConstructionBinders =
          (candidateRef, Nothing)
      | Just candidateBound <- mbBound
      , Just (_, Just preparedBound) <-
          find
            (typeBinderRefsSameIdentity candidateRef . fst)
            packetConstructionBinders
      , packetLocalResultOwns candidateRef
          || packetBodyResultOwns candidateRef
      , not
          ( operationalEndpointTypesAgree
              (tyToElab preparedBound)
              (tyToElab candidateBound)
          )
      , completeUnboundedForallSpecializesTo
          (tyToElab preparedBound)
          (tyToElab candidateBound) =
          (candidateRef, Just preparedBound)
      | otherwise = binder

    packetLocalResultOwns candidateRef =
      case subtermGeneralizationLocalResultAuthority packet of
        Just authority ->
          typeBinderRefIdentity candidateRef
            == scaConsumerIdentity authority
        Nothing -> False

    packetBodyResultOwns candidateRef =
      maybe
        False
        (typeBinderRefsSameIdentity candidateRef)
        packetBodyBinderRef

    completeExpectedPacketBinders publishedTy completedCandidates provisionalPlan = do
      replacements <-
        traverse completedExpectedBinder completedCandidates
      guard
        ( allDistinctBy
            ( \(leftRef, _) (rightRef, _) ->
                typeBinderRefsSameIdentity leftRef rightRef
            )
            replacements
        )
      let completedBinders =
            [ fromMaybe
                binder
                ( find
                    (typeBinderRefsSameIdentity publishedRef . fst)
                    replacements
                )
            | binder@(publishedRef, _) <- publishedBinders
            ]
      pure
        ( schemeToType
            ( mkElabSchemeWithRefs
                completedBinders
                (schemeBody publishedScheme)
            )
        )
      where
        publishedScheme = schemeFromType publishedTy
        publishedBinders = schemeBinderRefs publishedScheme
        binderRenames = exactBinderSpineRenames provisionalPlan

        completedExpectedBinder (candidateRef, mbBound) = do
          targetRef <-
            findRetainedTarget candidateRef binderRenames publishedBinders
          pure
            ( targetRef
            , fmap
                (renameBoundTypeBinderRefPayloads binderRenames)
                mbBound
            )

    -- A packet result can be consumed by the provisional binder-spine rather
    -- than retained as a binder in the frozen endpoint.  Once Typ(body) has
    -- completed that exact packet-owned declaration, replaying the old type
    -- application would publish its pre-body argument (and can be illegal for
    -- the new flexible bound).  The provisional spine proves that the unique
    -- packet body carrier was eliminated; replace that carrier's target
    -- codomain with its checked bound, then let the ordinary exact planner
    -- construct and validate the completed spine.  This is the eliminated
    -- counterpart of 'completeExpectedPacketBinders', which handles retained
    -- declarations.
    completeEliminatedPacketBodyBinder
      publishedTy
      provisionalCandidates
      constructedBodyTy
      completedCandidates
      provisionalPlan = do
        constructedBodyRef <-
          case constructedBodyTy of
            TVarRef ref -> Just ref
            _ -> Nothing
        [(completedRef, Just completedBound)] <-
          pure
            [ binder
            | binder@(ref, Just _) <- completedCandidates
            , typeBinderRefsSameIdentity ref constructedBodyRef
            ]
        guard
          ( length completedCandidates == 1
              && any
                ( \(ref, mbBound) ->
                    typeBinderRefsSameIdentity ref completedRef
                      && isNothing mbBound
                )
                provisionalCandidates
              && any
                ( \(ref, mbBound) ->
                    typeBinderRefsSameIdentity ref completedRef
                      && isNothing mbBound
                )
                packetConstructionBinders
              && isNothing
                ( findRetainedTarget
                    completedRef
                    binderRenames
                    publishedBinders
                )
              && not
                ( any
                    (typeBinderRefsSameIdentity completedRef)
                    (freeTypeVarRefsType publishedTy)
                )
          )
        (publishedParamTy, _provisionalBodyTy) <-
          case schemeBody publishedScheme of
            TArrow parameterTy bodyTy -> Just (parameterTy, bodyTy)
            _ -> Nothing
        let completedBodyTy =
              tyToElab
                ( renameBoundTypeBinderRefPayloads
                    binderRenames
                    completedBound
                )
        pure
          ( schemeToType
              ( mkElabSchemeWithRefs
                  publishedBinders
                  (TArrow publishedParamTy completedBodyTy)
              )
          )
      where
        publishedScheme = schemeFromType publishedTy
        publishedBinders = schemeBinderRefs publishedScheme
        binderRenames = exactBinderSpineRenames provisionalPlan

    findRetainedTarget candidateRef binderRenames binders =
      case
          [ targetRef
          | (sourceRef, targetRef) <- binderRenames
          , typeBinderRefsSameIdentity candidateRef sourceRef
          ]
            ++ [ expectedRef
               | (expectedRef, _) <- binders
               , typeBinderRefsSameIdentity candidateRef expectedRef
               ]
        of
          targetRef : _ -> Just targetRef
          [] -> Nothing

    allDistinctBy same = go
      where
        go [] = True
        go (item : rest) =
          not (any (same item) rest) && go rest

    constructionType constructionBinders constructionParamTy constructedBodyTy =
      schemeToType
        ( mkElabSchemeWithRefs
            constructionBinders
            (TArrow constructionParamTy constructedBodyTy)
        )

    certifyBodyOption = certifyBodyOptionWithAbstractions []

    certifyBodyOptionWithAbstractions
      bodyAbstractions0
      checkedBodySourceTy
      checkedBodyInstantiation0
      constructionCandidates
      publishedTy
      ( bodyCompletionInstantiation
        , constructedBodyTy0
        , ambientBodyRefinement0
        , bodyPeerRenames
        ) = do
      let publishedScheme0 = schemeFromType publishedTy
          publishedBinders0 = schemeBinderRefs publishedScheme0
          publishedBody0 = schemeBody publishedScheme0
          publishedParameterDeclarationRefs =
            case publishedBody0 of
              TArrow publishedParamTy _ ->
                typeBinderDeclarationRefs publishedParamTy
              _ -> []
          publishedOuterDeclarationRefs =
            map fst publishedBinders0
              ++ concatMap
                ( maybe
                    []
                    (typeBinderDeclarationRefs . tyToElab)
                    . snd
                )
                publishedBinders0
              ++ publishedParameterDeclarationRefs
              ++ map fst bodyAbstractions0
              ++ concatMap
                ( maybe
                    []
                    (typeBinderDeclarationRefs . tyToElab)
                    . snd
                )
                bodyAbstractions0
          bodyScopeCollisionRefs =
            foldr insertBodyScopeCollision []
              [ bodyRef
              | bodyRef <- typeBinderDeclarationRefs constructedBodyTy0
              , any
                  (typeBinderRefsSameIdentity bodyRef)
                  publishedOuterDeclarationRefs
              ]
          (bodyScopeCopies, _) =
            foldl
              freshenSourceBinder
              ( []
              , exactConstructionIdentityGenerator
                  [publishedTy, constructedBodyTy0]
              )
              bodyScopeCollisionRefs
          copyBodyScope =
            alphaRenameTypeBinderScopes bodyScopeCopies
          checkedBodySourceTyAtBodyScope =
            copyBodyScope checkedBodySourceTy
          checkedBodyInstantiationAtBodyScope =
            foldl
              ( \inst (sourceRef, copiedRef) ->
                  renameInstBoundRef sourceRef copiedRef inst
              )
              checkedBodyInstantiation0
              bodyScopeCopies
          bodyCompletionInstantiationAtBodyScope =
            foldl
              ( \inst (sourceRef, copiedRef) ->
                  renameInstBoundRef sourceRef copiedRef inst
              )
              bodyCompletionInstantiation
              bodyScopeCopies
          constructedBodyTy = copyBodyScope constructedBodyTy0
          publishedTyAtBodyScope =
            schemeToType
              ( mkElabSchemeWithRefs
                  publishedBinders0
                  ( case publishedBody0 of
                      TArrow publishedParamTy publishedBodyTy ->
                        TArrow
                          publishedParamTy
                          (copyBodyScope publishedBodyTy)
                      otherPublishedBody ->
                        copyBodyScope otherPublishedBody
                  )
              )
          bodyInstantiation0 =
            composeInst
              checkedBodyInstantiationAtBodyScope
              bodyCompletionInstantiationAtBodyScope
      bodyAmbientBindings <-
        refinedAmbientBindingsForBody ambientBodyRefinement0
      let bodyTypeEnv =
            constructionTypeEnvForAmbient
              bodyAmbientBindings
              constructionCandidates
      computedBodyTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              bodyTypeEnv
              checkedBodySourceTyAtBodyScope
              bodyInstantiation0
          )
      let certifiedBodyTy =
            schemeToType
              ( mkElabSchemeWithRefs
                  bodyAbstractions0
                  computedBodyTy
              )
      guard
        ( exactLambdaEndpointTypesAgree
            certifiedBodyTy
            constructedBodyTy
        )
      let alignPeerType =
            renameTypeBinderRefPayloads bodyPeerRenames
          alignPeerRef ref =
            case
                find
                  (typeBinderRefsSameIdentity ref . fst)
                  bodyPeerRenames
              of
                Just (_, targetRef) -> targetRef
                Nothing -> ref
          peerCandidates =
            [ ( alignPeerRef ref
              , fmap
                  (renameBoundTypeBinderRefPayloads bodyPeerRenames)
                  mbBound
              )
            | (ref, mbBound) <- constructionCandidates
            ]
          peerParamTy = alignPeerType paramTy
          peerBodyTy = alignPeerType constructedBodyTy
          peerPublishedTy0 = alignPeerType publishedTyAtBodyScope
          peerPublishedTy =
            fromMaybe
              peerPublishedTy0
              ( completePeerPublishedBodyBinder
                  peerCandidates
                  peerPublishedTy0
                  bodyPeerRenames
              )
          constructionTy0 =
            constructionType
              peerCandidates
              peerParamTy
              peerBodyTy
      spinePlan <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          constructionTy0
          peerPublishedTy
      let spineRenames = exactBinderSpineRenames spinePlan
          alignSpineRef ref =
            case
                find
                  (typeBinderRefsSameIdentity ref . fst)
                  spineRenames
              of
                Just (_, targetRef) -> targetRef
                Nothing -> ref
          composedPeerRenames =
            [ (sourceRef, alignSpineRef targetRef)
            | (sourceRef, targetRef) <- bodyPeerRenames
            ]
          binderRenames =
            composedPeerRenames
              ++ [ rename
                 | rename@(sourceRef, _) <- spineRenames
                 , not
                     ( any
                         (typeBinderRefsSameIdentity sourceRef . fst)
                         composedPeerRenames
                     )
                 ]
          completionInstantiation =
            exactBinderSpineInstantiation spinePlan
      let alignType =
            renameTypeBinderRefPayloads binderRenames
          alignRef ref =
            case
                find
                  (typeBinderRefsSameIdentity ref . fst)
                  binderRenames
              of
                Just (_, targetRef) -> targetRef
                Nothing -> ref
          constructionBinders =
            [ ( alignRef ref
              , fmap
                  (renameBoundTypeBinderRefPayloads binderRenames)
                  mbBound
              )
            | (ref, mbBound) <- constructionCandidates
            ]
          bodyAbstractions =
            [ ( alignRef ref
              , fmap
                  (renameBoundTypeBinderRefPayloads binderRenames)
                  mbBound
              )
            | (ref, mbBound) <- bodyAbstractions0
            ]
          alignedParamTy = alignType paramTy
          alignedBodyTy = alignType constructedBodyTy
          alignedConstructionTy =
            schemeToType
              ( mkElabSchemeWithRefs
                  constructionBinders
                  (TArrow alignedParamTy alignedBodyTy)
              )
          -- 'peerPublishedTy' may have completed a packet-owned provisional
          -- bound before the spine was planned.  Publish that constructed
          -- endpoint; re-aligning the original frozen type here would discard
          -- the completion after it had already justified the plan.
          alignedPublishedTy = alignType peerPublishedTy
          bodyInstantiation =
            foldl
              ( \inst (sourceRef, targetRef) ->
                  renameInstBoundRef sourceRef targetRef inst
              )
              bodyInstantiation0
              binderRenames
          ambientBodyRefinement =
            fmap
              ( \(ref, previousBound, completedBound) ->
                  ( alignRef ref
                  , alignType previousBound
                  , alignType completedBound
                  )
              )
              ambientBodyRefinement0
      completedTy <-
        either (const Nothing) Just
          (applyInstantiation alignedConstructionTy completionInstantiation)
      guard
        (exactLambdaEndpointTypesAgree completedTy alignedPublishedTy)
      let publishedBinders =
            schemeBinderRefs (schemeFromType alignedPublishedTy)
          directPublishedConstructionTy =
            schemeToType
              ( mkElabSchemeWithRefs
                  publishedBinders
                  (TArrow alignedParamTy alignedBodyTy)
              )
          retainsPreparedPacketDeclarations =
            all
              ( \(preparedRef, _) ->
                  any
                    (typeBinderRefsSameIdentity preparedRef . fst)
                    publishedBinders
              )
              ( schemeBinderRefs
                  ( siScheme
                      (subtermGeneralizationSchemeInfo packet)
                  )
              )
          constructsPublishedEndpointDirectly =
            retainsPreparedPacketDeclarations
              && exactLambdaEndpointTypesAgree
                directPublishedConstructionTy
                alignedPublishedTy
          emittedConstructionBinders =
            if constructsPublishedEndpointDirectly
              then publishedBinders
              else constructionBinders
          emittedCompletionInstantiation =
            if constructsPublishedEndpointDirectly
              then InstId
              else completionInstantiation
          ambientBodyRefinementCertificate =
            ambientBodyRefinement
              >>= exactAmbientBodyRefinementCertificate
          publishesAmbientBodyRefinement =
            isJust ambientBodyRefinement
          binderDeclarationsAgree
            (leftRef, leftBound)
            (rightRef, rightBound) =
              typeBinderRefsSameIdentity leftRef rightRef
                && operationalEndpointTypesAgree
                  (maybe TBottom tyToElab leftBound)
                  (maybe TBottom tyToElab rightBound)
          introducedPublishedBinders =
            [ publishedBinder
            | publishedBinder <- publishedBinders
            , not
                ( any
                    (binderDeclarationsAgree publishedBinder)
                    constructionBinders
                )
            ]
          introducedBindersHavePacketAuthority =
            not (null introducedPublishedBinders)
              && all
                ( \publishedBinder ->
                    any
                      (binderDeclarationsAgree publishedBinder)
                      packetConstructionBinders
                )
                introducedPublishedBinders
      -- A generic body option is not itself declaration authority.  It may
      -- use N to consume a bounded candidate.  Any O that follows must emit
      -- an exact declaration already owned by the packet's construction
      -- order; otherwise a consumed bounded identity could masquerade as a
      -- retained unbounded declaration.  Dedicated source/child branches
      -- carry their own authority, while direct construction emits the
      -- already-certified published spine and records InstId.
      guard
        ( constructsPublishedEndpointDirectly
            || not
              ( instantiationIntroducesFreshBinder
                  completionInstantiation
              )
            || introducedBindersHavePacketAuthority
        )
      guard
        ( not publishesAmbientBodyRefinement
            || isJust ambientBodyRefinementCertificate
        )
      pure
        ExactLambdaConstructionPlan
          { -- When all eliminated candidates are absent from the checked
            -- lambda endpoint, construct the already proved publication
            -- spine directly.  Building the larger candidate spine and then
            -- eliminating it would create an unnecessary redex; worse, a
            -- vacuous candidate bound can depend on a source binder scoped
            -- inside the lambda body and therefore has no legal outer
            -- ETyAbs placement.  Exact endpoint equality above is the
            -- construction proof that no eliminated declaration is needed.
            exactLambdaConstructionBinders = emittedConstructionBinders
          , exactLambdaConstructionPublishedBinders = publishedBinders
          , exactLambdaConstructionPublishedType = alignedPublishedTy
          , exactLambdaConstructionBinderRenames = binderRenames
          , exactLambdaConstructionParameterBinderCopies = []
          , exactLambdaConstructionBodyBinderRenames = bodyScopeCopies
          , exactLambdaConstructionResultBinderCopies = bodyScopeCopies
          , exactLambdaConstructionBodyAbstractions = bodyAbstractions
          , exactLambdaConstructionBodyInstantiation = bodyInstantiation
          , exactLambdaConstructionBodyType = alignedBodyTy
          , exactLambdaConstructionPreservedBodyRefinements = []
          , exactLambdaConstructionAmbientBodyRefinement =
              ambientBodyRefinement
          , exactLambdaConstructionIntroducedAmbientBodyDeclaration = Nothing
          , exactLambdaConstructionAmbientBodyRefinementCertificate =
              ambientBodyRefinementCertificate
          , exactLambdaConstructionPacketBodyProjection = Nothing
          , exactLambdaConstructionCompletionInstantiation =
              emittedCompletionInstantiation
          }
      where
        insertBodyScopeCollision ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

    -- A packet body slot can be copied to the checked body's exact result
    -- identity before Typ(body) completes its bound.  The peer route is
    -- packet provenance, the local candidate is the completed declaration,
    -- and an exact binder-spine plan from the prepared bound to that completed
    -- bound is the paper computation that justifies the specialization.
    -- Complete the published binder before selecting the lambda spine; doing
    -- it after term construction would leave the child built at a provisional
    -- endpoint it cannot inhabit.
    completePeerPublishedBodyBinder peerCandidates publishedTy peerRenames = do
      packetBodyRef <- packetBodyBinderRef
      [(packetRef, peerRef)] <-
        pure
          [ rename
          | rename@(sourceRef, _) <- peerRenames
          , typeBinderRefsSameIdentity sourceRef packetBodyRef
          ]
      guard
        ( any
            ( \(constructionRef, constructionBound) ->
                typeBinderRefsSameIdentity constructionRef packetRef
                  && isNothing constructionBound
            )
            packetConstructionBinders
        )
      [(candidateRef, Just completedBound)] <-
        pure
          [ binder
          | binder@(ref, Just _) <- peerCandidates
          , typeBinderRefsSameIdentity ref peerRef
          ]
      let publishedScheme = schemeFromType publishedTy
      [(publishedRef, Just preparedBound)] <-
        pure
          [ binder
          | binder@(ref, Just _) <- schemeBinderRefs publishedScheme
          , typeBinderRefsSameIdentity ref candidateRef
          ]
      _ <-
        planExactBinderSpine
          exactLambdaEndpointTypesAgree
          (tyToElab preparedBound)
          (tyToElab completedBound)
      pure
        ( schemeToType
            ( mkElabSchemeWithRefs
                [ if typeBinderRefsSameIdentity ref publishedRef
                    then (ref, Just completedBound)
                    else binder
                | binder@(ref, _) <- schemeBinderRefs publishedScheme
                ]
                (schemeBody publishedScheme)
            )
        )

    exactAmbientBodyRefinementCertificate
      refinement =
        packetGammaAmbientBodyRefinementCertificate refinement
          <|> checkedChildAmbientPacketBodyRefinementCertificate refinement

    packetGammaAmbientBodyRefinementCertificate
      (ambientRef, previousBound, completedBound) = do
        gammaAuthority <- subtermGeneralizationGammaAuthority packet
        consumerAuthority <- subtermGeneralizationConsumerAuthority packet
        guard
          ( gpaConsumerIdentity gammaAuthority
              == typeBinderRefIdentity ambientRef
          )
        guard
          ( scaConsumerIdentity consumerAuthority
              == gpaConsumerIdentity gammaAuthority
              && scaEdgeId consumerAuthority == gpaEdgeId gammaAuthority
          )
        guard
          ( operationalEndpointTypesAgree
              completedBound
              ( schemeToType
                  (subtermGeneralizationGammaBoundScheme packet)
              )
          )
        declarationOwner <-
          subtermConsumerAuthorityEnclosingOwner consumerAuthority
            <|> do
              -- A packet-owned consumer has no enclosing-owner field because
              -- the packet itself is the authority.  Join that authority to
              -- this exact lambda occurrence using the edge and generation
              -- scope that both records carry.  This makes the declaration
              -- locally emitted by construction instead of inferring an
              -- owner later from the completed type.
              guard
                ( lgoBoundaryEdge owner == gpaEdgeId gammaAuthority
                    && lgoScope owner == genRef (gpaOwnerGen gammaAuthority)
                )
              pure owner
        exterior <-
          typeBinderIdentityNode (gpaConsumerIdentity gammaAuthority)
        let semanticRef =
              typeBinderRefFromIdentity
                (gpaConsumerIdentity gammaAuthority)
                ( typeBinderIdentityStableName
                    (gpaConsumerIdentity gammaAuthority)
                )
            route =
              BodyConsumerRoute
                { bcrEdgeId = gpaEdgeId gammaAuthority
                , bcrOwner = declarationOwner
                , bcrExteriorNode = exterior
                , bcrSemanticRef = semanticRef
                , bcrConstructionRef = ambientRef
                , bcrOperatedType = completedBound
                , bcrConstructionOperatedType = completedBound
                }
        pure
          ( pendingBodyConsumerBoundRefinementCertificate
              (BodyConsumerLocallyEmitted route completedBound)
              ambientRef
              previousBound
              completedBound
          )

    -- An inherited exact endpoint can open a packet-owned result declaration
    -- before the lambda that consumes it is constructed.  This lambda does
    -- not acquire ownership of that declaration: Figure 15.3.5 constructs
    -- the checked child result first, applies Hyp at the already-open packet
    -- body identity, and carries the completed ambient declaration outward.
    --
    -- Packet Gamma authority is intentionally absent in this lane.  Instead,
    -- the complete proof is the conjunction of the packet's exact unbounded
    -- body slot, the inherited endpoint's free occurrence of that same
    -- identity, the ambient Bottom declaration, and a distinct child owner's
    -- final construction certificate.  Thus a ground body type alone can
    -- never refine an arbitrary ambient variable.
    checkedChildAmbientPacketBodyRefinementCertificate
      (ambientRef, previousBound, completedBound) = do
        guard
          ( isNothing (subtermGeneralizationGammaAuthority packet)
              && isNothing
                (subtermGeneralizationConsumerAuthority packet)
          )
        bodyConstruction <- certifiedBodyConstruction
        guard
          ( certifiedLambdaBodyOwner bodyConstruction /= owner
              && operationalEndpointTypesAgree
                (certifiedLambdaBodyConstructedType bodyConstruction)
                completedBound
              && operationalEndpointTypesAgree bodySourceTy completedBound
              && operationalEndpointTypesAgree bodyResultTy completedBound
          )
        expectedRef <- expectedBodyBinderRef
        guard
          ( typeBinderRefsSameIdentity expectedRef ambientRef
              && any
                (typeBinderRefsSameIdentity ambientRef)
                expectedFreeRefs
              && not
                ( any
                    (typeBinderRefsSameIdentity ambientRef . fst)
                    candidates
                )
          )
        [(installedRef, installedBound)] <- ambientExpectedMatches
        guard
          ( typeBinderRefsSameIdentity installedRef ambientRef
              && operationalEndpointTypesAgree
                installedBound
                previousBound
          )
        TArrow packetParamTy (TVarRef packetBodyRef) <-
          pure
            ( schemeBody
                ( siScheme
                    ( subtermGeneralizationConsumerConstructionSchemeInfo
                        packet
                    )
                )
            )
        guard
          ( operationalEndpointTypesAgree packetParamTy paramTy
              && typeBinderRefsSameIdentity packetBodyRef ambientRef
              && constructionOwnsPacketBinder ambientRef
              && not (sourceOwnsPacketBinder ambientRef)
          )
        [(packetRef, Nothing)] <-
          pure
            [ binder
            | binder@(ref, _) <- packetConstructionBinders
            , typeBinderRefsSameIdentity ref ambientRef
            ]
        guard (typeBinderRefsSameIdentity packetRef ambientRef)
        _ <- either (const Nothing) Just (elabToBound completedBound)
        exterior <- typeBinderIdentityNode (typeBinderRefIdentity ambientRef)
        let semanticRef =
              typeBinderRefFromIdentity
                (typeBinderRefIdentity ambientRef)
                ( typeBinderIdentityStableName
                    (typeBinderRefIdentity ambientRef)
                )
            route =
              BodyConsumerRoute
                { bcrEdgeId = lgoBoundaryEdge owner
                , bcrOwner = owner
                , bcrExteriorNode = exterior
                , bcrSemanticRef = semanticRef
                , bcrConstructionRef = ambientRef
                , bcrOperatedType =
                    certifiedLambdaBodyConstructedType bodyConstruction
                , bcrConstructionOperatedType = completedBound
                }
        pure
          ( pendingBodyConsumerBoundRefinementCertificate
              (BodyConsumerInheritedAmbient route previousBound)
              ambientRef
              previousBound
              completedBound
          )

    ensureDistinct role refs =
      case
          [ ref
          | (index, ref) <- zip [0 :: Int ..] refs
          , otherRef <- drop (index + 1) refs
          , typeBinderRefsSameIdentity ref otherRef
          ]
        of
          [] -> pure ()
          duplicate : _ ->
            constructionFailure
              ("the " ++ role ++ " Gamma repeats one binder identity")
              [ "  duplicate binder: " ++ show duplicate
              , "  binders: " ++ show refs
              ]

    constructionFailure
      :: String
      -> [String]
      -> Either ElabError a
    constructionFailure detail context =
      Left
        ( ValidationFailed
            ( [ "invalid exact enclosing lambda construction"
              , "  detail: " ++ detail
              , "  expected type: " ++ show expectedTy
              ]
                ++ context
            )
        )

-- | The source-oriented endpoint installed for one already validated
-- lambda-body consumer route.  Its constructor is private so later
-- specialization cannot confuse a raw packet endpoint with the endpoint that
-- 'Gen(Gamma, tau)' actually published.
data ValidatedBodyConsumerProjection =
  ValidatedBodyConsumerProjection
    { vbcpRoute :: !BodyConsumerRoute,
      vbcpSourceConstructionRenames ::
        ![(TypeBinderRef, TypeBinderRef)],
      vbcpProjectedType :: !ElabType,
      vbcpSourceProjectionInstantiation :: !Instantiation
    }
  deriving (Eq, Show)

-- | Proof that the construction endpoint of an exact lambda-body route is a
-- declaration available at this lambda boundary.  A declaration is either
-- emitted by this exact @Gen(Gamma,tau)@ construction or inherited from the
-- enclosing paper Gamma.  A descendant may also complete an enclosing
-- declaration before that enclosing owner has emitted it; that state has its
-- own constructor so owner handoff cannot mistake future local emission for
-- an already ambient declaration.  The constructors are private so a bare
-- route cannot be mistaken for declaration ownership.
data BodyConsumerDeclarationAuthority
  = BodyConsumerLocallyEmitted
      !BodyConsumerRoute
      !ElabType
  | BodyConsumerInheritedAmbient
      !BodyConsumerRoute
      !ElabType
  | BodyConsumerEnclosingAmbient
      !BodyConsumerRoute
      !ElabType
  | BodyConsumerPendingOwnerEmission
      !BodyConsumerRoute
      !OrdinaryOwnerEmissionProgress
  | BodyConsumerOrdinaryOwnerEmission
      !BodyConsumerRoute
      !OrdinaryOwnerEmissionProgress
  | BodyConsumerConsumedAtOwner
      !BodyConsumerRoute
      !ElabType
      !OrdinaryOwnerEmissionProgress
  deriving (Eq, Show)

-- | Construction-time evidence for the two ways an enclosing owner may later
-- expose a completed declaration.  A checked child can close the declaration
-- in a lexically copied scope, in which case the copied binders, checked bounds,
-- and open body remain one certificate.  An exact lambda owner can instead
-- consume the declaration while commuting a forall through the result of its
-- already fixed value-lambda prefix; that certificate retains the pre-closure
-- endpoint.  In both cases later projection only verifies the recorded
-- construction and never rediscovers an identity or transition from a final
-- type.
data CertifiedFutureOwnerScopeClosure
  = CertifiedFutureOwnerCopiedScopeClosure
      ![(TypeBinderRef, ElabType)]
      !ElabType
  | CertifiedFutureOwnerResultClosureSource !ElabType
  deriving (Eq, Show)

-- | Directional declaration progress while a descendant completion travels
-- toward an ordinary syntactic owner.  Checked child constructions and
-- validated local-Gamma binders are sources that construct this certificate's
-- completion; a child-owned lexical closure retains its own construction
-- certificate; and later xMLF Gamma steps retain their source and target as
-- opaque transition certificates.  Keeping the three forms separate prevents
-- a later state from being read as a transition back to an earlier generalized
-- declaration.
data OrdinaryOwnerEmissionProgress
  = OrdinaryOwnerEmissionProgress
      ![ElabType]
      ![CertifiedFutureOwnerScopeClosure]
      ![CertifiedGammaBoundTransition]
  deriving (Eq, Show)

emptyOrdinaryOwnerEmissionProgress :: OrdinaryOwnerEmissionProgress
emptyOrdinaryOwnerEmissionProgress =
  OrdinaryOwnerEmissionProgress [] [] []

recordOrdinaryOwnerPreparedSource
  :: ElabType
  -> OrdinaryOwnerEmissionProgress
  -> OrdinaryOwnerEmissionProgress
recordOrdinaryOwnerPreparedSource declarationBound progress@(OrdinaryOwnerEmissionProgress sources closures transitions)
  | any
      (operationalEndpointTypesAgree declarationBound)
      sources = progress
  | otherwise =
      OrdinaryOwnerEmissionProgress
        (sources ++ [declarationBound])
        closures
        transitions

recordOrdinaryOwnerScopeClosure
  :: CertifiedFutureOwnerScopeClosure
  -> OrdinaryOwnerEmissionProgress
  -> OrdinaryOwnerEmissionProgress
recordOrdinaryOwnerScopeClosure closure progress@(OrdinaryOwnerEmissionProgress sources closures transitions)
  | closure `elem` closures = progress
  | otherwise =
      OrdinaryOwnerEmissionProgress
        sources
        (closures ++ [closure])
        transitions

recordBodyConsumerOwnerScopeClosure
  :: CertifiedFutureOwnerScopeClosure
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
recordBodyConsumerOwnerScopeClosure closure certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerPendingOwnerEmission route progress ->
      certificate
        { bcbrDeclarationAuthority =
            BodyConsumerPendingOwnerEmission
              route
              (recordOrdinaryOwnerScopeClosure closure progress)
        }
    BodyConsumerOrdinaryOwnerEmission route progress ->
      certificate
        { bcbrDeclarationAuthority =
            BodyConsumerOrdinaryOwnerEmission
              route
              (recordOrdinaryOwnerScopeClosure closure progress)
        }
    BodyConsumerConsumedAtOwner route completedBound progress ->
      certificate
        { bcbrDeclarationAuthority =
            BodyConsumerConsumedAtOwner
              route
              completedBound
              (recordOrdinaryOwnerScopeClosure closure progress)
        }
    _ -> certificate

recordOrdinaryOwnerGammaTransition
  :: CertifiedGammaBoundTransition
  -> OrdinaryOwnerEmissionProgress
  -> OrdinaryOwnerEmissionProgress
recordOrdinaryOwnerGammaTransition transition progress@(OrdinaryOwnerEmissionProgress sources closures transitions)
  | transition `elem` transitions = progress
  | otherwise =
      OrdinaryOwnerEmissionProgress
        sources
        closures
        (transitions ++ [transition])

-- | Recover the declaration and completed bound only after its exact local
-- owner has finished emitting it.  Pending and ordinary owner-emission states
-- deliberately retain their authority constructor after finalization; the
-- certificate's finalized constructor proves that the future-owner state has
-- crossed its owner boundary.  Ambient and consumed states are excluded by
-- construction.
finalizedLocalBodyConsumerDeclaration
  :: BodyConsumerBoundRefinementCertificate
  -> Maybe (BodyConsumerRoute, ElabType)
finalizedLocalBodyConsumerDeclaration certificate
  | not (bodyConsumerBoundRefinementOwnerFinalized certificate) = Nothing
  | otherwise =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerLocallyEmitted route declaredBound
          | operationalEndpointTypesAgree declaredBound completedBound ->
              Just (route, declaredBound)
          | otherwise -> Nothing
        BodyConsumerPendingOwnerEmission route _ ->
          Just (route, completedBound)
        BodyConsumerOrdinaryOwnerEmission route _ ->
          Just (route, completedBound)
        _ -> Nothing
  where
    completedBound = bcbrCompletedBound certificate

-- | The exact declaration identity already emitted inside a checked child.
-- Enclosing constructors must keep this identity out of their own Gamma:
-- replaying it outside the child's lexical type abstraction would create a
-- second declaration.  Expose only the identity, while retaining the
-- authority and completed bound inside the opaque certificate.
bodyConsumerBoundRefinementFinalizedLocalRef
  :: BodyConsumerBoundRefinementCertificate
  -> Maybe TypeBinderRef
bodyConsumerBoundRefinementFinalizedLocalRef certificate = do
  _ <- finalizedLocalBodyConsumerDeclaration certificate
  pure (bcbrAmbientRef certificate)

-- | How the checked consumer declaration is represented at this boundary.
-- The ordinary case uses the ambient declaration itself.  A nested source
-- annotation can instead expose a child-local operated binder while the
-- enclosing Gamma still owns the graph declaration; its checked bound is
-- retained here so the enclosing Lambda(Gamma) candidate is completed from
-- construction evidence, not rediscovered from the final type.
data BodyConsumerConstructionProjection
  = DirectBodyConsumerConstructionProjection
  | PacketOperatedBodyConsumerConstructionProjection !BoundType
  deriving (Eq, Show)

-- | A construction proof that one provisional declaration was completed by
-- the exact lambda-body consumer that owns it.  Owner progress is represented
-- by the constructor, not by a Boolean that can disagree with declaration
-- authority.  A caller can therefore obtain a pending certificate only from a
-- validating construction seam, and can cross the owner boundary only through
-- 'finalizeBodyConsumerBoundRefinementAtOwner'.
data BodyConsumerBoundRefinementCertificate
  = PendingBodyConsumerBoundRefinementCertificate
      { bcbrDeclarationAuthority :: !BodyConsumerDeclarationAuthority,
        bcbrAmbientRef :: !TypeBinderRef,
        bcbrConstructionProjection :: !BodyConsumerConstructionProjection,
        bcbrPreviousBound :: !ElabType,
        bcbrCompletedBound :: !ElabType
      }
  | FinalizedBodyConsumerBoundRefinementCertificate
      { bcbrDeclarationAuthority :: !BodyConsumerDeclarationAuthority,
        bcbrAmbientRef :: !TypeBinderRef,
        bcbrConstructionProjection :: !BodyConsumerConstructionProjection,
        bcbrPreviousBound :: !ElabType,
        bcbrCompletedBound :: !ElabType
      }
  deriving (Eq, Show)

pendingBodyConsumerBoundRefinementCertificate
  :: BodyConsumerDeclarationAuthority
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
pendingBodyConsumerBoundRefinementCertificate authority ambientRef previousBound completedBound =
  PendingBodyConsumerBoundRefinementCertificate
    { bcbrDeclarationAuthority = authority,
      bcbrAmbientRef = ambientRef,
      bcbrConstructionProjection = DirectBodyConsumerConstructionProjection,
      bcbrPreviousBound = previousBound,
      bcbrCompletedBound = completedBound
    }

pendingPacketOperatedBodyConsumerBoundRefinementCertificate
  :: BodyConsumerDeclarationAuthority
  -> TypeBinderRef
  -> BoundType
  -> ElabType
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
pendingPacketOperatedBodyConsumerBoundRefinementCertificate
  authority
  ambientRef
  projectedBound
  previousBound
  completedBound =
    PendingBodyConsumerBoundRefinementCertificate
      { bcbrDeclarationAuthority = authority
      , bcbrAmbientRef = ambientRef
      , bcbrConstructionProjection =
          PacketOperatedBodyConsumerConstructionProjection projectedBound
      , bcbrPreviousBound = previousBound
      , bcbrCompletedBound = completedBound
      }

-- | A proof that one exact Gamma declaration crossed a checked construction
-- step.  Its constructor is private, and the declaration identity is kept
-- alongside both states so a later descendant certificate cannot borrow the
-- transition merely because another declaration has the same shape.
data CertifiedGammaBoundTransition =
  CertifiedGammaBoundTransition
    !TypeBinderRef
    !ElabType
    !ElabType
    !(Maybe BodyConsumerRoute)
  deriving (Eq, Show)

-- | Certify the exact state change performed by a construction boundary.
-- Scoped endpoint equality is already a proof that the two bounds are the
-- closed and opened presentations of the same lexical declaration.  All
-- other changes must be reproduced by an explicit xMLF instantiation and
-- checked in the construction Gamma.
certifyGammaBoundTransition
  :: (ElabType -> ElabType -> Bool)
  -> TypeCheck.Env
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> Maybe CertifiedGammaBoundTransition
certifyGammaBoundTransition typesAgree typeEnv declarationRef sourceBound targetBound
  | typesAgree sourceBound targetBound =
      Just transition
  | otherwise = do
      instantiation <-
        constructExactInstantiation
          typeEnv
          typesAgree
          sourceBound
          targetBound
      checkedTarget <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              typeEnv
              sourceBound
              instantiation
          )
      guard (typesAgree checkedTarget targetBound)
      pure transition
  where
    transition =
      CertifiedGammaBoundTransition
        declarationRef
        sourceBound
        targetBound
        Nothing

renameCertifiedGammaBoundTransition
  :: [(TypeBinderRef, TypeBinderRef)]
  -> CertifiedGammaBoundTransition
  -> CertifiedGammaBoundTransition
renameCertifiedGammaBoundTransition renames (CertifiedGammaBoundTransition ref sourceBound targetBound mbOriginRoute) =
  CertifiedGammaBoundTransition
    (renameRef ref)
    (renameTypeBinderRefPayloads renames sourceBound)
    (renameTypeBinderRefPayloads renames targetBound)
    (renameRoute <$> mbOriginRoute)
  where
    renameRoute route =
      route
        { bcrConstructionRef = renameRef (bcrConstructionRef route)
        , bcrOperatedType =
            renameTypeBinderRefPayloads renames (bcrOperatedType route)
        , bcrConstructionOperatedType =
            renameTypeBinderRefPayloads
              renames
              (bcrConstructionOperatedType route)
        }

    renameRef candidate =
      fromMaybe
        candidate
        ( snd
            <$> find
              (typeBinderRefsSameIdentity candidate . fst)
              renames
        )

-- | Seal the state transition performed by a validated lambda-body consumer
-- projection.  The route and projected endpoint were already typechecked by
-- 'mkValidatedBodyConsumerProjection'.  Here the exact declaration authority
-- must additionally prove that the published source state is either its own
-- declaration or the unique completion carried by one checked descendant.
certifyBodyConsumerProjectionGammaBoundTransition
  :: BodyConsumerDeclarationAuthority
  -> ValidatedBodyConsumerProjection
  -> ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Either ElabError CertifiedGammaBoundTransition
certifyBodyConsumerProjectionGammaBoundTransition
  declarationAuthority
  projection
  publishedBound
  childCertificates = do
  unless
    (authorizedBodyConsumerRoute declarationAuthority == route)
    (transitionFailure "declaration authority and projection select different routes")
  case matchingPublishedTransitions of
    _
      | operationalEndpointTypesAgree declarationBound publishedBound ->
          pure ()
    [_] -> pure ()
    matches ->
      transitionFailure
        ( "published source state has "
            ++ show (length matches)
            ++ " matching descendant completions, expected exactly one"
        )
  pure
    ( CertifiedGammaBoundTransition
        (bcrConstructionRef route)
        publishedBound
        (vbcpProjectedType projection)
        Nothing
    )
  where
    route = vbcpRoute projection
    declarationBound =
      authorizedBodyConsumerDeclarationBound declarationAuthority
    matchingPublishedTransitions =
      filter
        ( bodyConsumerBoundRefinementCertifiesTransition
            (bcrConstructionRef route)
            declarationBound
            publishedBound
        )
        childCertificates

    transitionFailure detail =
      Left
        ( ValidationFailed
            [ "cannot certify projected Gamma declaration transition"
            , "  detail: " ++ detail
            , "  route: " ++ show route
            , "  declaration bound: " ++ show declarationBound
            , "  published bound: " ++ show publishedBound
            , "  projected bound: " ++ show (vbcpProjectedType projection)
            , "  matching child refinements: "
                ++ show matchingPublishedTransitions
            ]
        )

-- | Recover the exact Gamma step already sealed by a body-consumer
-- certificate.  The route's operated type is the checked source-side view;
-- its construction-operated type is the bound installed for that same
-- declaration identity.  Certificate constructors validate this pair before
-- it becomes observable, so descendants may transport their own authority
-- across the step without re-deriving it from the resulting type shape.
certifiedBodyConsumerGammaBoundTransition
  :: BodyConsumerBoundRefinementCertificate
  -> Maybe CertifiedGammaBoundTransition
certifiedBodyConsumerGammaBoundTransition certificate = do
  let route =
        authorizedBodyConsumerRoute
          (bcbrDeclarationAuthority certificate)
      sourceBound = bcrOperatedType route
      targetBound = bcrConstructionOperatedType route
  guard
    ( typeBinderRefsSameIdentity
        (bcrConstructionRef route)
        (bcbrAmbientRef certificate)
    )
  guard
    ( operationalEndpointTypesAgree
        targetBound
        (bcbrCompletedBound certificate)
    )
  guard (not (operationalEndpointTypesAgree sourceBound targetBound))
  pure
    ( CertifiedGammaBoundTransition
        (bcrConstructionRef route)
        sourceBound
        targetBound
        (Just route)
    )

-- | Test one direction of the exact Gamma transition sealed by this
-- certificate.  In particular, a sibling certificate that has advanced past
-- its own completed bound cannot be read backwards merely because both states
-- occur in its owner-progress history.
bodyConsumerBoundRefinementCarriesGammaTransition
  :: ElabType
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementCarriesGammaTransition sourceBound targetBound certificate =
  any transitionMatches
    ( maybeToList
        (certifiedBodyConsumerGammaBoundTransition certificate)
        ++ bodyConsumerRecordedGammaTransitions certificate
    )
  where
    transitionMatches
      ( CertifiedGammaBoundTransition
          transitionRef
          transitionSource
          transitionTarget
          _
        ) =
        typeBinderRefsSameIdentity
          transitionRef
          (bcbrAmbientRef certificate)
          && operationalEndpointTypesAgree sourceBound transitionSource
          && operationalEndpointTypesAgree targetBound transitionTarget

-- | Carry a construction-validated Gamma transition into every matching
-- ordinary future-owner certificate.  The transition's source must already
-- be one of that certificate's recorded declaration states; this prevents a
-- same-identity sibling specialization from acquiring unrelated authority.
advanceBodyConsumerBoundRefinementsThroughCertifiedGammaBound
  :: CertifiedGammaBoundTransition
  -> [BodyConsumerBoundRefinementCertificate]
  -> [BodyConsumerBoundRefinementCertificate]
advanceBodyConsumerBoundRefinementsThroughCertifiedGammaBound transition =
  map
    ( advanceBodyConsumerBoundRefinementThroughCertifiedGammaBound
      transition
    )

-- | Advance a future-owner declaration through the exact result transition
-- already checked by its descendant term.  The caller supplies the source and
-- completed endpoints from that descendant's private owner-final certificate;
-- this smart constructor accepts them only for the pending declaration whose
-- owner, exterior, construction identity, and current completed state all
-- match.  The old state and the directional transition remain in the opaque
-- owner progress, so later requirement construction consumes positive history
-- instead of recovering a transition from the final type.
advanceBodyConsumerBoundRefinementThroughCheckedOwnerResultCompletion
  :: LocalGammaOwner
  -> NodeId
  -> ElabType
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
advanceBodyConsumerBoundRefinementThroughCheckedOwnerResultCompletion
  owner
  exteriorNode
  sourceEndpoint
  completedEndpoint
  certificate
    | bodyConsumerBoundRefinementOwnerFinalized certificate = certificate
    | not (bodyConsumerBoundRefinementEmittedBy owner certificate) =
        certificate
    | bcrExteriorNode route /= exteriorNode = certificate
    | not
        ( typeBinderRefsSameIdentity
            (bcrConstructionRef route)
            (bcbrAmbientRef certificate)
        ) =
        certificate
    | operationalEndpointTypesAgree sourceEndpoint completedEndpoint =
        certificate
    | not
        ( operationalEndpointTypesAgree
            (bcbrCompletedBound certificate)
            sourceEndpoint
        ) =
        certificate
    | otherwise =
        case bcbrDeclarationAuthority certificate of
          BodyConsumerPendingOwnerEmission pendingRoute progress ->
            advanceCertificate
              (BodyConsumerPendingOwnerEmission pendingRoute)
              progress
          BodyConsumerOrdinaryOwnerEmission ordinaryRoute progress ->
            advanceCertificate
              (BodyConsumerOrdinaryOwnerEmission ordinaryRoute)
              progress
          _ -> certificate
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)
    transition =
      CertifiedGammaBoundTransition
        (bcrConstructionRef route)
        sourceEndpoint
        completedEndpoint
        (Just route)
    advanceCertificate rebuildAuthority progress =
      certificate
        { bcbrDeclarationAuthority =
            rebuildAuthority
              ( recordOrdinaryOwnerGammaTransition
                  transition
                  ( recordOrdinaryOwnerPreparedSource
                      sourceEndpoint
                      progress
                  )
              )
        , bcbrCompletedBound = completedEndpoint
        }

-- | Advance a future-owner consumer after a checked child has replaced the
-- exact operated endpoint from which that consumer was constructed.  Both
-- states must already occur in the private certificate in the expected
-- direction: the old operated endpoint is its completion and the new checked
-- endpoint is its previous owner declaration.  This makes the update a
-- construction-state join, rather than a type-shaped rewrite after a failed
-- Hyp check.  Certificates for another owner, route, or construction state
-- are left unchanged.
advanceBodyConsumerBoundRefinementThroughOwnerEndpointCompletion
  :: LocalGammaOwner
  -> NodeId
  -> ElabType
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
advanceBodyConsumerBoundRefinementThroughOwnerEndpointCompletion
  owner
  exteriorNode
  sourceEndpoint
  completedEndpoint
  certificate
    | not (bodyConsumerBoundRefinementEmittedBy owner certificate) =
        certificate
    | bcrExteriorNode route /= exteriorNode = certificate
    | operationalEndpointTypesAgree sourceEndpoint completedEndpoint =
        certificate
    | not
        ( operationalEndpointTypesAgree
            (bcbrCompletedBound certificate)
            sourceEndpoint
        ) =
        certificate
    | not
        ( operationalEndpointTypesAgree
            (bcbrPreviousBound certificate)
            completedEndpoint
        ) =
        certificate
    | otherwise =
        certificate
          { bcbrPreviousBound = completedEndpoint
          , bcbrCompletedBound = completedEndpoint
          }
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

advanceBodyConsumerBoundRefinementThroughCertifiedGammaBound
  :: CertifiedGammaBoundTransition
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
advanceBodyConsumerBoundRefinementThroughCertifiedGammaBound
  transition@(CertifiedGammaBoundTransition declarationRef sourceBound _ mbOriginRoute)
  certificate =
    case bcbrDeclarationAuthority certificate of
      BodyConsumerPendingOwnerEmission route progress ->
        if transitionApplies
          then
            certificate
              { bcbrDeclarationAuthority =
                  BodyConsumerPendingOwnerEmission
                    route
                    (recordOrdinaryOwnerGammaTransition transition progress)
              }
          else certificate
      BodyConsumerOrdinaryOwnerEmission route progress ->
        if transitionApplies
          then
            certificate
              { bcbrDeclarationAuthority =
                  BodyConsumerOrdinaryOwnerEmission
                    route
                    (recordOrdinaryOwnerGammaTransition transition progress)
              }
          else certificate
      BodyConsumerConsumedAtOwner route completedBound progress ->
        if transitionMatches
          then
            certificate
              { bcbrDeclarationAuthority =
                  BodyConsumerConsumedAtOwner
                    route
                    completedBound
                    (recordOrdinaryOwnerGammaTransition transition progress)
              }
          else certificate
      _ -> certificate
  where
    transitionApplies =
      not (bodyConsumerBoundRefinementOwnerFinalized certificate)
        && transitionMatches

    transitionMatches =
      typeBinderRefsSameIdentity
        declarationRef
        (bcbrAmbientRef certificate)
        && sourceAccepted

    sourceAccepted =
      bodyConsumerBoundRefinementAcceptsDeclarationState
        certificate
        sourceBound
        || originatesFromCertificate

    originatesFromCertificate =
      maybe
        False
        ( == authorizedBodyConsumerRoute
            (bcbrDeclarationAuthority certificate)
        )
        mbOriginRoute

bodyConsumerBoundRefinementOwnerFinalized
  :: BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementOwnerFinalized certificate =
  case certificate of
    PendingBodyConsumerBoundRefinementCertificate {} -> False
    FinalizedBodyConsumerBoundRefinementCertificate {} -> True

finalizedBodyConsumerBoundRefinementCertificate
  :: BodyConsumerDeclarationAuthority
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
finalizedBodyConsumerBoundRefinementCertificate authority certificate =
  FinalizedBodyConsumerBoundRefinementCertificate
    { bcbrDeclarationAuthority = authority,
      bcbrAmbientRef = bcbrAmbientRef certificate,
      bcbrConstructionProjection =
        bcbrConstructionProjection certificate,
      bcbrPreviousBound = bcbrPreviousBound certificate,
      bcbrCompletedBound = bcbrCompletedBound certificate
    }

-- | Advance ordinary-owner certificates through declaration states selected
-- by an exact local-Gamma construction plan.  The caller supplies both halves
-- of the plan's evidence: the refs for which requirement validation succeeded
-- and the binders actually emitted into that construction Gamma.  Only an
-- identity present in both sets can extend the certificate's state chain.
--
-- This transition happens before the descendant is installed into the parent
-- environment.  The installer can consequently consume a recorded state
-- directly instead of recognizing a type relationship after a failed check.
advanceBodyConsumerBoundRefinementsThroughValidatedLocalGamma
  :: [TypeBinderRef]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [BodyConsumerBoundRefinementCertificate]
  -> Either ElabError [BodyConsumerBoundRefinementCertificate]
advanceBodyConsumerBoundRefinementsThroughValidatedLocalGamma
  validatedLocalRefs
  constructionBinders =
    traverse advanceCertificate
  where
    advanceCertificate certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerOrdinaryOwnerEmission route progress
          | not (bodyConsumerBoundRefinementOwnerFinalized certificate)
          , any
              (typeBinderRefsSameIdentity targetRef)
              validatedLocalRefs ->
              case
                  [ binder
                  | binder@(ref, _) <- constructionBinders
                  , typeBinderRefsSameIdentity ref targetRef
                  ]
                of
                  [(_, mbBound)] ->
                    pure
                      certificate
                        { bcbrDeclarationAuthority =
                            BodyConsumerOrdinaryOwnerEmission
                              route
                              (advanceProgress (maybe TBottom tyToElab mbBound) progress)
                        }
                  [] ->
                    transitionFailure
                      certificate
                      "validated local requirement has no emitted construction binder"
                  binders ->
                    transitionFailure
                      certificate
                      ( "validated local requirement has multiple emitted construction binders: "
                          ++ show binders
                      )
          where
            targetRef = bcbrAmbientRef certificate
        BodyConsumerConsumedAtOwner route completedBound progress
          | any
              (typeBinderRefsSameIdentity targetRef)
              validatedLocalRefs ->
              case
                  [ binder
                  | binder@(ref, _) <- constructionBinders
                  , typeBinderRefsSameIdentity ref targetRef
                  ]
                of
                  [(_, mbBound)] ->
                    pure
                      certificate
                        { bcbrDeclarationAuthority =
                            BodyConsumerConsumedAtOwner
                              route
                              completedBound
                              (advanceProgress (maybe TBottom tyToElab mbBound) progress)
                        }
                  [] ->
                    transitionFailure
                      certificate
                      "validated local requirement has no emitted construction binder"
                  binders ->
                    transitionFailure
                      certificate
                      ( "validated local requirement has multiple emitted construction binders: "
                          ++ show binders
                      )
          where
            targetRef = bcbrAmbientRef certificate
        _ -> pure certificate

    advanceProgress = recordOrdinaryOwnerPreparedSource

    transitionFailure certificate detail =
      Left
        ( ValidationFailed
            [ "cannot advance body-consumer refinement through validated local Gamma"
            , "  detail: " ++ detail
            , "  certificate: " ++ show certificate
            , "  validated local refs: " ++ show validatedLocalRefs
            , "  construction binders: " ++ show constructionBinders
            ]
        )

-- | A refinement is part of an owner's final construction certificate only
-- when its exact declaration identity remains live, either as a binder the
-- owner emits or as an ambient declaration it uses.  A body route may
-- complete a provisional waypoint that is subsequently consumed without
-- either role; publishing that dead waypoint to root planning would invent a
-- declaration.
bodyConsumerBoundRefinementTargetsAny
  :: [TypeBinderRef]
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementTargetsAny refs certificate =
  any
    (typeBinderRefsSameIdentity (bcbrAmbientRef certificate))
    refs

-- | Require the semantic half of a body-consumer route to occur in the
-- current construction authority.  Several graph occurrences may quotient
-- to one construction reference; the construction reference alone therefore
-- cannot decide whether a historical consumed declaration belongs to a
-- later requirement using that representative.
bodyConsumerBoundRefinementHasSemanticRouteWithin
  :: [NodeId]
  -> [TypeBinderRef]
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementHasSemanticRouteWithin nodes refs certificate =
  bcrExteriorNode route `elem` nodes
    || any
      (typeBinderRefsSameIdentity (bcrSemanticRef route))
      refs
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

-- | Join a private declaration-completion certificate to the exact prepared
-- packet whose provisional consumer it advances.  This is deliberately
-- stronger than owner equality: both the packet edge and construction identity
-- must denote the certificate's route, so callers cannot apply the refinement
-- to an unrelated endpoint candidate from the same lambda.
bodyConsumerBoundRefinementCompletesPreparedPacket
  :: LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementCompletesPreparedPacket owner packet certificate =
  case subtermGeneralizationGammaAuthority packet of
    Just authority ->
      bcrOwner route == owner
        && bcrEdgeId route == gpaEdgeId authority
        && typeBinderRefIdentity (bcrConstructionRef route)
          == gpaConsumerIdentity authority
    Nothing -> False
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

-- | Advance one inherited exact lambda endpoint through the body-consumer
-- completion owned by that same lambda.  The certificate fixes the owner,
-- body edge, exterior identity, operated endpoint, and completed bound; the
-- only admissible rewrite is therefore the lambda codomain selected by that
-- route.  This runs before exact binder-spine construction, so the lambda is
-- built at the completed endpoint rather than repaired after typechecking.
completeLambdaEndpointFromBodyConsumerRefinement
  :: LocalGammaOwner
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Either ElabError ElabType
completeLambdaEndpointFromBodyConsumerRefinement owner parameterTy certificate endpoint = do
  unless
    (bodyConsumerBoundRefinementCompletesOwnerEndpoint owner certificate)
    (completionFailure "certificate ownership or completed endpoint changed")
  if endpointConstructsCompletedBody
    then pure (TArrow parameterTy completedBound)
    else case schemeBody endpointScheme of
      TArrow endpointParameterTy bodyTy
        | exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
        , operationalEndpointTypesAgree bodyTy completedBound ->
            pure endpoint
        | exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
        , operationalEndpointTypesAgree bodyTy (bcrOperatedType route) ->
            pure
              ( schemeToType
                  ( mkElabSchemeWithRefs
                      (schemeBinderRefs endpointScheme)
                      (TArrow endpointParameterTy completedBound)
                  )
                )
        | exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
        , bodyConsumerBoundRefinementCarriesGammaTransition
            endpoint
            completedBound
            certificate ->
            -- The prepared packet can certify its whole source-oriented
            -- lambda endpoint as the Gamma state whose construction yields
            -- the completed body declaration.  The exact owner and value
            -- parameter supply Var-Abs; retain the endpoint's lexical forall
            -- spine and install that certified result as its codomain.
            pure
              ( schemeToType
                  ( mkElabSchemeWithRefs
                      (schemeBinderRefs endpointScheme)
                      (TArrow endpointParameterTy completedBound)
                  )
                )
        | exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
        , bodyBinderDeclarationReachesCompletedBound bodyTy ->
            -- The body can still name the local flexible declaration whose
            -- checked bound the owner certificate has just completed.  Use
            -- that exact declaration edge to construct the completed codomain
            -- now; retaining the (possibly vacuous) forall here leaves its
            -- later elimination to the binder-spine plan that owns it.
            pure
              ( schemeToType
                  ( mkElabSchemeWithRefs
                      (schemeBinderRefs endpointScheme)
                      (TArrow endpointParameterTy completedBound)
                  )
                )
        | operationalEndpointTypesAgree endpoint completedBound ->
            -- The incoming certificate can name the recursively checked body
            -- endpoint rather than the enclosing lambda endpoint.  The source
            -- lambda constructor owns the missing arrow, so construct it here
            -- from the already checked parameter and certified body instead of
            -- asking a later type mismatch repair to recover it.
            pure (TArrow parameterTy completedBound)
        | otherwise ->
            completionFailure
              "the inherited lambda endpoint is neither the certified body nor its completed enclosing arrow"
      _
        | operationalEndpointTypesAgree endpoint completedBound ->
            pure (TArrow parameterTy completedBound)
        | otherwise -> completionFailure "the inherited endpoint is not a lambda or its certified body"
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)
    completedBound = bcbrCompletedBound certificate
    endpointScheme = schemeFromType endpoint
    bodyBinderDeclarationReachesCompletedBound bodyTy =
      case bodyTy of
        TVarRef bodyRef ->
          any
            ( \(declaredRef, mbBound) ->
                typeBinderRefsSameIdentity declaredRef bodyRef
                  && maybe
                    False
                    ( operationalEndpointTypesAgree completedBound
                        . tyToElab
                    )
                    mbBound
            )
            (schemeBinderRefs endpointScheme)
        _ -> False

    endpointConstructsCompletedBody =
      case completedBound of
        TArrow completedParameterTy _ ->
          exactLambdaEndpointTypesAgree completedParameterTy parameterTy
            && completeUnboundedForallSpecializesTo endpoint completedBound
        _ -> False

    completionFailure :: String -> Either ElabError a
    completionFailure detail =
      Left
        ( ValidationFailed
            [ "cannot complete exact lambda endpoint from its body consumer"
            , "  detail: " ++ detail
            , "  owner: " ++ show owner
            , "  route: " ++ show route
            , "  parameter: " ++ show parameterTy
            , "  endpoint: " ++ show endpoint
            , "  completed bound: " ++ show completedBound
            ]
        )

-- | Consume the exact free result declaration of an endpoint only after that
-- endpoint has won the construction-certificate selection.  The earlier
-- candidate-admissibility query intentionally remains stricter: accepting
-- this transition there would promote provisional incoming topology merely
-- because a later local certificate could repair it.  Here the selected
-- endpoint, exact owner-boundary route, free declaration identity, and
-- completed bound jointly construct the final codomain.
completeSelectedFreeLambdaEndpointFromBodyConsumerRefinement
  :: LocalGammaOwner
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Either ElabError ElabType
completeSelectedFreeLambdaEndpointFromBodyConsumerRefinement
  owner
  parameterTy
  certificate
  endpoint = do
    unless
      (bodyConsumerBoundRefinementCompletesOwnerEndpoint owner certificate)
      (completionFailure "certificate does not complete this lambda owner")
    (endpointParameterTy, bodyRef) <-
      case schemeBody endpointScheme of
        TArrow endpointParameter (TVarRef ref) ->
          pure (endpointParameter, ref)
        body ->
          completionFailure
            ("selected endpoint has no free variable codomain: " ++ show body)
    unless
      ( exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
          && typeBinderRefsSameIdentity
            bodyRef
            (bcrConstructionRef route)
          && not
            ( any
                (typeBinderRefsSameIdentity bodyRef . fst)
                (schemeBinderRefs endpointScheme)
            )
      )
      (completionFailure "selected endpoint does not expose the certified declaration freely")
    pure
      ( schemeToType
          ( mkElabSchemeWithRefs
              (schemeBinderRefs endpointScheme)
              (TArrow endpointParameterTy (bcbrCompletedBound certificate))
          )
      )
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)
    endpointScheme = schemeFromType endpoint

    completionFailure :: String -> Either ElabError a
    completionFailure detail =
      Left
        ( ValidationFailed
            [ "cannot consume selected free lambda endpoint"
            , "  detail: " ++ detail
            , "  owner: " ++ show owner
            , "  route: " ++ show route
            , "  parameter: " ++ show parameterTy
            , "  endpoint: " ++ show endpoint
            , "  completed bound: "
                ++ show (bcbrCompletedBound certificate)
            ]
        )

-- | Project a result owner's finalized consumption into the provisional
-- endpoint seen by an enclosing lambda.  Unlike a locally emitted Gamma
-- binder, a 'BodyConsumerConsumedAtOwner' declaration no longer appears in
-- the owner's leading binder spine: its exact free occurrence has already
-- been discharged inside the checked owner.  The same provisional endpoint
-- can still contain the opened view of the owner's lambda parameter, so replay
-- the owner's parameter-boundary certificates at that exact arrow domain
-- before replacing the free result identity.  The two certified transitions
-- must produce exactly the owner's independently recorded construction.  This
-- is the owner-boundary construction witness; callers never infer either
-- replacement from the final type alone.
completeConsumedResultOwnerEndpointFromBodyConsumerRefinement
  :: LocalGammaOwner
  -> [LambdaParamBoundaryCertificate]
  -> BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> ElabType
  -> Maybe ElabType
completeConsumedResultOwnerEndpointFromBodyConsumerRefinement
  owner
  parameterCertificates
  certificate
  provisionalEndpoint
  ownerConstructedEndpoint = do
    guard (bodyConsumerBoundRefinementOwnerFinalized certificate)
    (route, declaredBound) <-
      case bcbrDeclarationAuthority certificate of
        BodyConsumerConsumedAtOwner consumedRoute bound _ ->
          Just (consumedRoute, bound)
        _ -> Nothing
    guard (bcrOwner route == owner)
    guard
      ( typeBinderRefsSameIdentity
          (bcrConstructionRef route)
          (bcbrAmbientRef certificate)
      )
    guard
      ( operationalEndpointTypesAgree
          declaredBound
          (bcbrCompletedBound certificate)
          && operationalEndpointTypesAgree
            (bcrConstructionOperatedType route)
            (bcbrCompletedBound certificate)
      )
    parameterCompletedEndpoint <-
      completeOwnerParameterBoundary provisionalEndpoint
    guard
      ( any
          ( typeBinderRefsSameIdentity
              (bcrConstructionRef route)
          )
          (freeTypeVarRefsType parameterCompletedEndpoint)
      )
    let completedEndpoint =
          substTypeCaptureRef
            (bcrConstructionRef route)
            (bcbrCompletedBound certificate)
            parameterCompletedEndpoint
    guard
      ( not
          ( operationalEndpointTypesAgree
              provisionalEndpoint
              completedEndpoint
          )
          && exactLambdaEndpointTypesAgree
            completedEndpoint
            ownerConstructedEndpoint
      )
    pure ownerConstructedEndpoint
  where
    completeOwnerParameterBoundary endpoint = do
      let endpointScheme = schemeFromType endpoint
          ownerScheme = schemeFromType ownerConstructedEndpoint
      (endpointParameter, endpointResult) <-
        case schemeBody endpointScheme of
          TArrow parameterTy resultTy -> Just (parameterTy, resultTy)
          _ -> Nothing
      ownerParameter <-
        case schemeBody ownerScheme of
          TArrow parameterTy _ -> Just parameterTy
          _ -> Nothing
      let completedParameter =
            completeLambdaParamBoundaryBound
              parameterCertificates
              endpointParameter
      guard
        ( exactLambdaEndpointTypesAgree
            completedParameter
            ownerParameter
        )
      pure
        ( schemeToType
            ( mkElabSchemeWithRefs
                (schemeBinderRefs endpointScheme)
                (TArrow completedParameter endpointResult)
            )
        )

-- | Advance the endpoint already selected from an exact prepared packet.  This
-- is intentionally separate from 'completeLambdaEndpointFromBodyConsumerRefinement':
-- that narrower operation is also used to decide whether an incoming endpoint
-- belongs to a body-consumer route, where accepting an arbitrary exact
-- specialization would keep stale candidates alive.  Here the packet edge and
-- consumer identity have first been joined to the private certificate, so the
-- paper's binder-spine computation may advance its frozen codomain before the
-- final lambda constructor is built and checked.
completePreparedLambdaEndpointFromBodyConsumerRefinement
  :: LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Either ElabError ElabType
completePreparedLambdaEndpointFromBodyConsumerRefinement
  owner
  packet
  parameterTy
  certificate
  endpoint = do
    unless
      ( bodyConsumerBoundRefinementCompletesPreparedPacket
          owner
          packet
          certificate
          && operationalEndpointTypesAgree
            (bcrConstructionOperatedType route)
            completedBound
      )
      (preparedCompletionFailure "packet authority or completed endpoint changed")
    case
        completeLambdaEndpointFromBodyConsumerRefinement
          owner
          parameterTy
          certificate
          endpoint
      of
        Right completed -> pure completed
        Left _ ->
          case schemeBody endpointScheme of
            TArrow endpointParameterTy bodyTy
              | exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
              , Just _ <-
                  planExactBinderSpine
                    exactLambdaEndpointTypesAgree
                    completedBound
                    bodyTy ->
                  -- The selected endpoint has already consumed this exact
                  -- packet completion through its binder-spine computation.
                  -- Keep that specialization at the lambda layer where it
                  -- was constructed; replacing the body with
                  -- @completedBound@ here would replay the certificate in
                  -- reverse and move its forall back across the value arrow.
                  pure endpoint
              | exactLambdaEndpointTypesAgree endpointParameterTy parameterTy
              , Just _ <-
                  planExactBinderSpine
                    exactLambdaEndpointTypesAgree
                    bodyTy
                    completedBound ->
                  pure
                    ( schemeToType
                        ( mkElabSchemeWithRefs
                            (schemeBinderRefs endpointScheme)
                            (TArrow endpointParameterTy completedBound)
                        )
                      )
            _ ->
              preparedCompletionFailure
                "the selected packet endpoint does not construct the completed body"
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)
    completedBound = bcbrCompletedBound certificate
    endpointScheme = schemeFromType endpoint

    preparedCompletionFailure :: String -> Either ElabError a
    preparedCompletionFailure detail =
      Left
        ( ValidationFailed
            [ "cannot complete selected exact packet lambda endpoint"
            , "  detail: " ++ detail
            , "  owner: " ++ show owner
            , "  route: " ++ show route
            , "  parameter: " ++ show parameterTy
            , "  endpoint: " ++ show endpoint
            , "  completed bound: " ++ show completedBound
            ]
        )

-- | Whether a finalized owner certificate proves that one of the supplied
-- declarations was consumed and therefore must not be emitted by an
-- enclosing root closure.
bodyConsumerBoundRefinementConsumesAny
  :: [TypeBinderRef]
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementConsumesAny refs certificate =
  bodyConsumerBoundRefinementOwnerFinalized certificate
    && bodyConsumerBoundRefinementConsumed certificate
    && bodyConsumerBoundRefinementTargetsAny refs certificate

-- | Exact construction identities exposed when a finalized owner consumes
-- the declaration and substitutes its completed bound into the result.  Root
-- preparation uses this construction evidence to place an owner-emitted
-- dependency before performing that substitution; it must not rediscover the
-- dependency from the already-projected result type.
bodyConsumerBoundRefinementConsumedDependencies
  :: BodyConsumerBoundRefinementCertificate
  -> [TypeBinderRef]
bodyConsumerBoundRefinementConsumedDependencies certificate
  | bodyConsumerBoundRefinementOwnerFinalized certificate
  , bodyConsumerBoundRefinementConsumed certificate =
      freeTypeVarRefsType (bcbrCompletedBound certificate)
  | otherwise = []

-- | Exact occurrence-replay routes left behind when a completed lambda
-- consumes its construction-local Gamma declaration.  The consumed
-- construction identity no longer denotes an emitted binder, while the
-- semantic exterior remains the occurrence identity recorded in the checked
-- child scheme.  Carry that quotient as a replay-only capability: publishing
-- it into the ordinary construction-alias graph would create the inverse of
-- the route used while the declaration was being built and can therefore
-- form a cycle.
--
-- The private refinement constructor is the authority for every route.  A
-- graph key may occur more than once through transparent wrappers, but all
-- copies must name the same semantic identity.
bodyConsumerBoundRefinementConsumedReplayRoutes
  :: [BodyConsumerBoundRefinementCertificate]
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
bodyConsumerBoundRefinementConsumedReplayRoutes =
  foldM insertCertificate IntMap.empty
  where
    insertCertificate routes certificate =
      case consumedReplayRoute certificate of
        Nothing -> pure routes
        Just (node, semanticRef) ->
          let nodeKey = getNodeId node
           in case IntMap.lookup nodeKey routes of
                Nothing -> pure (IntMap.insert nodeKey semanticRef routes)
                Just existingRef
                  | typeBinderRefsSameIdentity existingRef semanticRef ->
                      pure routes
                  | otherwise ->
                      Left
                        ( ValidationFailed
                            [ "consumed body-consumer replay routes disagree"
                            , "  graph node: " ++ show node
                            , "  existing semantic ref: " ++ show existingRef
                            , "  incoming semantic ref: " ++ show semanticRef
                            ]
                        )

    consumedReplayRoute certificate
      | bodyConsumerBoundRefinementOwnerFinalized certificate
      , BodyConsumerConsumedAtOwner route consumedBound _ <-
          bcbrDeclarationAuthority certificate
      , typeBinderRefsSameIdentity
          (bcbrAmbientRef certificate)
          (bcrConstructionRef route)
      , operationalEndpointTypesAgree
          consumedBound
          (bcbrCompletedBound certificate)
      , Just constructionNode <-
          typeBinderRefNode (bcrConstructionRef route) =
          Just (constructionNode, bcrSemanticRef route)
      | otherwise = Nothing

-- | Check the complete identity-bearing state transition recorded by a
-- refinement certificate without exposing its constructor.  This is used by
-- intermediate checked constructors whose ambient Gamma has already advanced
-- to the completed bound while their graph requirement still denotes the
-- provisional endpoint.
bodyConsumerBoundRefinementCertifiesTransition
  :: TypeBinderRef
  -> ElabType
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementCertifiesTransition targetRef previousBound completedBound certificate =
  typeBinderRefsSameIdentity
    targetRef
    (bcbrAmbientRef certificate)
    && operationalEndpointTypesAgree
      completedBound
      (bcbrCompletedBound certificate)
    && ( operationalEndpointTypesAgree
          previousBound
          (bcbrPreviousBound certificate)
          || any
            (operationalEndpointTypesAgree previousBound)
            (bodyConsumerPreparedCompletionSources certificate)
          || bodyConsumerBoundRefinementCarriesGammaTransition
            previousBound
            completedBound
            certificate
       )

-- | Several sibling edges may carry independent proof objects for one exact
-- declaration transition.  Treat them as one authority only when every
-- certificate proves the requested states and all routes retain the same
-- owner, exterior, semantic identity, and construction identity.  Edge ids
-- may differ: that is precisely the sibling-use case.
bodyConsumerBoundRefinementsJointlyCertifyTransition
  :: TypeBinderRef
  -> ElabType
  -> ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Bool
bodyConsumerBoundRefinementsJointlyCertifyTransition
  targetRef
  previousBound
  completedBound
  certificates =
    case certificates of
      [] -> False
      first : remaining ->
        all
          ( bodyConsumerBoundRefinementCertifiesTransition
              targetRef
              previousBound
              completedBound
          )
          certificates
          && all
            (bodyConsumerBoundRefinementsShareDeclaration first)
            remaining

-- | Authorize an owner-final declaration from the complete identity-bearing
-- refinement history.  The common case is one certificate whose declaration
-- state constructs the checked completion.  Nested consumers can instead
-- split that proof into a directed chain: the unique declaration is first
-- introduced from 'TBottom', and later sibling edges advance its completed
-- state to the declaration that the owner publishes.
--
-- The path is built only from private certificate states and routes for the
-- same owner, exterior, semantic identity, and construction identity.  A
-- type-shaped endpoint therefore cannot manufacture the missing link, and
-- traversal order cannot select a later instance as the declaration origin.
bodyConsumerBoundRefinementsAuthorizeDeclarationCompletion
  :: TypeBinderRef
  -> ElabType
  -> ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Bool
bodyConsumerBoundRefinementsAuthorizeDeclarationCompletion
  targetRef
  checkedCompletion
  ownerDeclaration
  certificates =
    case directCertificates of
      [_] -> True
      _ -> any originReachesDeclaration constructionOrigins
  where
    targetCertificates =
      [ certificate
      | certificate <- certificates
      , typeBinderRefsSameIdentity
          targetRef
          (bcbrAmbientRef certificate)
      ]

    directCertificates =
      [ certificate
      | certificate <- targetCertificates
      , bodyConsumerBoundRefinementCompletesExactEndpoint
          checkedCompletion
          certificate
      , bodyConsumerDeclarationConstructsCompletion
          certificate
          ownerDeclaration
      ]

    constructionOrigins =
      [ certificate
      | certificate <- targetCertificates
      , operationalEndpointTypesAgree
          (bcbrPreviousBound certificate)
          TBottom
      , bodyConsumerEndpointIsCertifiedCompletion
          certificate
          checkedCompletion
      ]

    originReachesDeclaration origin =
      bodyConsumerBoundRefinementsCertifyStatePathWithinRoute
        origin
        (bcbrCompletedBound origin)
        ownerDeclaration
        [ certificate
        | certificate <- targetCertificates
        , bodyConsumerBoundRefinementsShareDeclaration
            origin
            certificate
        ]

-- | Replay a directed state path for one private declaration route.  The
-- anchor supplies owner/exterior/identity authority; every traversed edge is
-- a transition sealed by a certificate for that same declaration.  This is
-- the sequential counterpart of
-- 'bodyConsumerBoundRefinementsJointlyCertifyTransition', which validates
-- several sibling proofs of one identical transition.
bodyConsumerBoundRefinementsCertifyStatePathWithinRoute
  :: BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Bool
bodyConsumerBoundRefinementsCertifyStatePathWithinRoute
  anchor
  sourceBound
  targetBound
  certificates =
    reachesTarget [sourceBound]
  where
    routeCertificates =
      filter
        (bodyConsumerBoundRefinementsShareDeclaration anchor)
        certificates

    reachesTarget reached
      | any (operationalEndpointTypesAgree targetBound) reached = True
      | null newlyReached = False
      | otherwise = reachesTarget (reached ++ newlyReached)
      where
        newlyReached =
          foldl'
            addFreshEndpoint
            []
            [ completedBound
            | certificate <- routeCertificates
            , (previousBound, completedBound) <-
                bodyConsumerBoundRefinementStateTransitions certificate
            , any
                (operationalEndpointTypesAgree previousBound)
                reached
            ]
        addFreshEndpoint endpoints endpoint
          | any (operationalEndpointTypesAgree endpoint) reached = endpoints
          | any (operationalEndpointTypesAgree endpoint) endpoints = endpoints
          | otherwise = endpoints ++ [endpoint]

bodyConsumerBoundRefinementStateTransitions
  :: BodyConsumerBoundRefinementCertificate
  -> [(ElabType, ElabType)]
bodyConsumerBoundRefinementStateTransitions certificate =
  (bcbrPreviousBound certificate, bcbrCompletedBound certificate)
    : [ (sourceBound, bcbrCompletedBound certificate)
      | sourceBound <- bodyConsumerPreparedCompletionSources certificate
      ]
    ++ [ (sourceBound, targetBound)
       | CertifiedGammaBoundTransition
          _
          sourceBound
          targetBound
          _ <- bodyConsumerRecordedGammaTransitions certificate
       ]

bodyConsumerBoundRefinementsShareDeclaration
  :: BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementsShareDeclaration first second =
  let firstRoute =
        authorizedBodyConsumerRoute
          (bcbrDeclarationAuthority first)
      secondRoute =
        authorizedBodyConsumerRoute
          (bcbrDeclarationAuthority second)
   in bcrOwner firstRoute == bcrOwner secondRoute
        && bcrExteriorNode firstRoute == bcrExteriorNode secondRoute
        && typeBinderRefsSameIdentity
          (bcrSemanticRef firstRoute)
          (bcrSemanticRef secondRoute)
        && typeBinderRefsSameIdentity
          (bcrConstructionRef firstRoute)
          (bcrConstructionRef secondRoute)

-- | Whether a certificate owns a legal transition from this exact Gamma
-- declaration state.  Descendant construction packets can carry independent
-- refinements for the same graph identity after different exact
-- specializations.  Identity alone therefore cannot select a certificate;
-- the declaration must also be at one of the authority-recorded states.
bodyConsumerBoundRefinementAppliesToDeclarationState
  :: TypeBinderRef
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementAppliesToDeclarationState targetRef currentBound certificate =
  typeBinderRefsSameIdentity
    targetRef
    (bcbrAmbientRef certificate)
    && bodyConsumerBoundRefinementAcceptsDeclarationState
      certificate
      currentBound

-- | The exact declaration states from which a checked refinement may be
-- replayed.  Most certificates observe the declaration at their previous
-- bound.  A descendant can, however, already observe the completed bound while
-- the future owner still carries the declaration bound frozen in its private
-- authority (notably the unbounded slot of
-- 'BodyConsumerPendingOwnerEmission').  Accepting that authority-owned state
-- is construction evidence; accepting an arbitrary lower bound would not be.
bodyConsumerBoundRefinementAcceptsDeclarationState
  :: BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Bool
bodyConsumerBoundRefinementAcceptsDeclarationState certificate currentBound =
  any
    (operationalEndpointTypesAgree currentBound)
    ( [ authorizedBodyConsumerDeclarationBound
          (bcbrDeclarationAuthority certificate)
      , bcbrPreviousBound certificate
      , bcbrCompletedBound certificate
      ]
        ++ bodyConsumerPreparedDeclarationStates certificate
    )
    || any
      (`certifiedFutureOwnerScopeClosureMatches` currentBound)
      (bodyConsumerPreparedOwnerScopeClosures certificate)

bodyConsumerPreparedDeclarationStates
  :: BodyConsumerBoundRefinementCertificate
  -> [ElabType]
bodyConsumerPreparedDeclarationStates certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerPendingOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress sources _ transitions) ->
        sources
          ++ concatMap transitionStates transitions
    BodyConsumerOrdinaryOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress sources _ transitions) ->
        sources
          ++ concatMap transitionStates transitions
    BodyConsumerConsumedAtOwner
      _
      _
      (OrdinaryOwnerEmissionProgress sources _ transitions) ->
        sources
          ++ concatMap transitionStates transitions
    _ -> []
  where
    transitionStates
      (CertifiedGammaBoundTransition _ sourceBound targetBound _) =
        [sourceBound, targetBound]

bodyConsumerPreparedOwnerScopeClosures
  :: BodyConsumerBoundRefinementCertificate
  -> [CertifiedFutureOwnerScopeClosure]
bodyConsumerPreparedOwnerScopeClosures certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerPendingOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress _ closures _) ->
        closures
    BodyConsumerOrdinaryOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress _ closures _) ->
        closures
    BodyConsumerConsumedAtOwner
      _
      _
      (OrdinaryOwnerEmissionProgress _ closures _) ->
        closures
    _ -> []

certifiedFutureOwnerScopeClosureMatches
  :: CertifiedFutureOwnerScopeClosure
  -> ElabType
  -> Bool
certifiedFutureOwnerScopeClosureMatches
  (CertifiedFutureOwnerCopiedScopeClosure copiedDeclarations openBody)
  currentEndpoint =
    matchCopiedDeclarations copiedDeclarations currentEndpoint
  where
    matchCopiedDeclarations [] endpoint =
      operationalEndpointTypesAgree endpoint openBody
    matchCopiedDeclarations remaining endpoint =
      case endpoint of
        TForallRef endpointRef mbEndpointBound endpointBody ->
          case
              break
                (typeBinderRefsSameIdentity endpointRef . fst)
                remaining
            of
              (before, (_, expectedBound) : after)
                | endpointBoundAgrees mbEndpointBound expectedBound ->
                    matchCopiedDeclarations
                      (before ++ after)
                      endpointBody
              _ -> False
        _ -> False

    endpointBoundAgrees Nothing expectedBound =
      operationalEndpointTypesAgree TBottom expectedBound
    endpointBoundAgrees (Just endpointBound) expectedBound =
      operationalEndpointTypesAgree
        (tyToElab endpointBound)
        expectedBound
certifiedFutureOwnerScopeClosureMatches
  (CertifiedFutureOwnerResultClosureSource sourceEndpoint)
  currentEndpoint =
    exactResultForallClosureOf currentEndpoint sourceEndpoint

bodyConsumerPreparedCompletionSources
  :: BodyConsumerBoundRefinementCertificate
  -> [ElabType]
bodyConsumerPreparedCompletionSources certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerPendingOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress sources _ _) ->
        sources
    BodyConsumerOrdinaryOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress sources _ _) ->
        sources
    BodyConsumerConsumedAtOwner
      _
      _
      (OrdinaryOwnerEmissionProgress sources _ _) ->
        sources
    _ -> []

bodyConsumerRecordedGammaTransitions
  :: BodyConsumerBoundRefinementCertificate
  -> [CertifiedGammaBoundTransition]
bodyConsumerRecordedGammaTransitions certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerPendingOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress _ _ transitions) ->
        transitions
    BodyConsumerOrdinaryOwnerEmission
      _
      (OrdinaryOwnerEmissionProgress _ _ transitions) ->
        transitions
    BodyConsumerConsumedAtOwner
      _
      _
      (OrdinaryOwnerEmissionProgress _ _ transitions) ->
        transitions
    _ -> []

-- | Whether an authority-recorded declaration state constructs the checked
-- completion carried by this exact certificate.  The declaration identity
-- and owner/edge route were sealed when the certificate was created; this
-- query additionally replays the exact Figure 15.3 binder computation.  It is
-- therefore safe for the owner to retain a closed, more-general declaration
-- instead of publishing an open child endpoint whose free value-lambda
-- parameter belongs to the child owner.
bodyConsumerDeclarationConstructsCompletion
  :: BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Bool
bodyConsumerDeclarationConstructsCompletion certificate declaration =
  any
    (`certifiedFutureOwnerScopeClosureMatches` declaration)
    (bodyConsumerPreparedOwnerScopeClosures certificate)
    || ( bodyConsumerBoundRefinementAcceptsDeclarationState
          certificate
          declaration
          && ( operationalEndpointTypesAgree declaration completedBound
                || bodyConsumerBoundRefinementCarriesGammaTransition
                  declaration
                  completedBound
                  certificate
                || isJust
                  ( planExactBinderSpine
                      operationalEndpointTypesAgree
                      declaration
                      completedBound
                  )
                || isJust
                  ( constructExactInstantiation
                      TypeCheck.emptyEnv
                      operationalEndpointTypesAgree
                      declaration
                      completedBound
                  )
             )
       )
  where
    completedBound = bcbrCompletedBound certificate

-- | Whether a second checked edge proves that the same owner declaration
-- constructs its own completion at this exact declaration state.  This is
-- the positive authority required to retain one generalized declaration for
-- several sibling uses; a single certificate must instead advance through
-- its ordinary completed-state transition.
bodyConsumerBoundRefinementHasSiblingCompletionAt
  :: LocalGammaOwner
  -> ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementHasSiblingCompletionAt owner declaration certificates certificate =
  bodyConsumerBoundRefinementEmittedBy owner certificate
    && bodyConsumerDeclarationConstructsCompletion certificate declaration
    && any isSibling certificates
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

    isSibling sibling =
      let siblingRoute =
            authorizedBodyConsumerRoute
              (bcbrDeclarationAuthority sibling)
       in bcrEdgeId siblingRoute /= bcrEdgeId route
            && bcrExteriorNode siblingRoute == bcrExteriorNode route
            && typeBinderRefsSameIdentity
              (bcbrAmbientRef sibling)
              (bcbrAmbientRef certificate)
            && bodyConsumerBoundRefinementEmittedBy owner sibling
            && bodyConsumerDeclarationConstructsCompletion
              sibling
              declaration

-- | A pending owner scheme or a sibling edge may already expose one exact
-- consumer endpoint of the declaration completed by this certificate.  The
-- certificate supplies the declaration identity; either its exact
-- identity-bearing closure or a checked xMLF computation must independently
-- prove the complete specialization.  The general computation is needed when
-- N consumes a bounded declaration and O/I reintroduce a vacuous retained
-- binder around the resulting endpoint; a leading-spine-only comparison
-- cannot express that paper computation.  This is intentionally private so a
-- type-shaped endpoint cannot manufacture declaration authority.
bodyConsumerCompletedDeclarationSpecializesToEndpoint
  :: BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Bool
bodyConsumerCompletedDeclarationSpecializesToEndpoint certificate endpoint =
  exactUnboundedForallClosureOf
    (bcbrCompletedBound certificate)
    endpoint
    || isJust
      ( constructExactInstantiation
          TypeCheck.emptyEnv
          operationalEndpointTypesAgree
          (bcbrCompletedBound certificate)
          endpoint
      )
    || case
        planExactBinderSpine
          alphaEqType
          (bcbrCompletedBound certificate)
          endpoint
      of
        Just plan -> exactBinderSpineInstantiation plan /= InstId
        Nothing -> False

-- | The source-oriented operated declaration can retain the bounded forall
-- that was opened when the body consumer entered its construction Gamma.  A
-- later owner requirement may expose one exact instance of that declaration
-- after producer projection has resolved the opened result variable to its
-- bound.  The private route proves both halves of this transition: its
-- construction-operated endpoint is the certificate's completed bound, and
-- an exact xMLF computation from the operated declaration reaches the selected
-- endpoint.  This is stronger than consulting the selected type or graph bound
-- alone and keeps the specialization correct by construction.
bodyConsumerOperatedDeclarationConstructsEndpoint
  :: BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Bool
bodyConsumerOperatedDeclarationConstructsEndpoint certificate endpoint =
  operationalEndpointTypesAgree
    (bcrConstructionOperatedType route)
    (bcbrCompletedBound certificate)
    && isJust
      ( constructExactInstantiation
          TypeCheck.emptyEnv
          operationalEndpointTypesAgree
          (bcrOperatedType route)
          endpoint
      )
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

-- | Whether an owner boundary exposes the recorded completion itself, one
-- exact instance of it, or its exact identity-bearing closure over free
-- dependencies.  The latter is produced when owner-final publication closes
-- a descendant completion before sibling requirements are combined.
--
-- This relation is certificate-private: the same type shapes without the
-- declaration identity and owner route are not Gamma construction evidence.
bodyConsumerEndpointIsCertifiedCompletion
  :: BodyConsumerBoundRefinementCertificate
  -> ElabType
  -> Bool
bodyConsumerEndpointIsCertifiedCompletion certificate endpoint =
  operationalEndpointTypesAgree
    endpoint
    (bcbrCompletedBound certificate)
    || bodyConsumerCompletedDeclarationSpecializesToEndpoint
      certificate
      endpoint
    || bodyConsumerOperatedDeclarationConstructsEndpoint
      certificate
      endpoint
    || exactUnboundedForallClosureOf
      endpoint
      (bcbrCompletedBound certificate)

-- | Validate an already constructed packet endpoint by replaying certified
-- descendant declaration completions through the packet's aligned bound.
-- A certificate may complete the packet's own consumer directly, or a
-- declaration quantified inside a larger packet bound.  In the latter case
-- the exact binder is substituted with its completed bound and now-dead packet
-- binders are removed by dependency closure.  The packet and target identities
-- are still selected by ordinary placement; this function only validates the
-- completed endpoint supplied by that checked path.
bodyConsumerBoundRefinementsCompletePacketBound
  :: LocalGammaOwner
  -> [TypeBinderRef]
  -> PreparedSubtermGeneralization
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> Bool
bodyConsumerBoundRefinementsCompletePacketBound owner ambientRefs packet targetRef packetBound constructedBound certificates =
  any directlyCompletesPacket certificates
    || case foldM projectCertificate (False, packetBound) certificates of
      Just (True, projectedBound) ->
        operationalEndpointTypesAgree
          (projectAmbientPacketBinders projectedBound)
          constructedBound
      _ -> False
  where
    directlyCompletesPacket certificate =
      typeBinderRefsSameIdentity
        targetRef
        (bcbrAmbientRef certificate)
        && bcrOwner route == owner
        && matchingPacketAuthority route
        && operationalEndpointTypesAgree
          (projectAmbientPacketBinders (bcbrCompletedBound certificate))
          constructedBound
      where
        route =
          authorizedBodyConsumerRoute
            (bcbrDeclarationAuthority certificate)

    matchingPacketAuthority route =
      case subtermGeneralizationConsumerAuthority packet of
        Just authority ->
          scaEdgeId authority == bcrEdgeId route
            && scaConsumerIdentity authority
              == typeBinderRefIdentity (bcrSemanticRef route)
            && subtermConsumerAuthorityEnclosingOwner authority
              == Just owner
        Nothing -> False

    projectCertificate state@(_changed, ty) certificate
      | bcrOwner route /= owner
          && not (bodyConsumerBoundRefinementConsumed certificate)
          && not (finalizedLocalBinder certificate) =
          Just state
      | otherwise =
          case matchingBinders of
            [] -> Just state
            [(packetRef, mbPreviousBound)]
              | certificateStartsAt mbPreviousBound ->
                  Just
                    ( True
                    , completedPacketBound packetRef
                    )
              | otherwise -> Nothing
            _ -> Nothing
      where
        route =
          authorizedBodyConsumerRoute
            (bcbrDeclarationAuthority certificate)
        scheme = schemeFromType ty
        binders = schemeBinderRefs scheme
        matchingBinders =
          filter
            ( typeBinderRefsSameIdentity
                (bcbrAmbientRef certificate)
                . fst
            )
            binders
        certificateStartsAt mbPreviousBound =
          bodyConsumerBoundRefinementAcceptsDeclarationState
            certificate
            (maybe TBottom tyToElab mbPreviousBound)
            || completeUnboundedForallSpecializesTo
              (maybe TBottom tyToElab mbPreviousBound)
              (bcbrPreviousBound certificate)
        completedPacketBound packetRef
          | finalizedLocalBinder certificate =
              schemeToType
                ( mkElabSchemeWithRefs
                    [ if typeBinderRefsSameIdentity ref packetRef
                        then
                          ( ref
                          , either
                              (const mbBound)
                              Just
                              (elabToBound (bcbrCompletedBound certificate))
                          )
                        else binder
                    | binder@(ref, mbBound) <- binders
                    ]
                    (schemeBody scheme)
                )
          | otherwise =
              schemeToType
                ( mkElabSchemeWithRefs
                    retainedBinders
                    completedBody
                )
          where
            completedBody =
              substTypeCaptureRef
                packetRef
                (bcbrCompletedBound certificate)
                (schemeBody scheme)
            substitutedBinders =
              [ ( ref
                , fmap
                    ( mapBoundType
                        ( substTypeCaptureRef
                            packetRef
                            (bcbrCompletedBound certificate)
                        )
                    )
                    mbBound
                )
              | (ref, mbBound) <- binders
              , not (typeBinderRefsSameIdentity ref packetRef)
              ]
            retainedBinders =
              fst
                ( foldr
                    retainRequiredBinder
                    ([], freeTypeVarRefsType completedBody)
                    substitutedBinders
                )

        finalizedLocalBinder refinement
          | not (bodyConsumerBoundRefinementOwnerFinalized refinement) = False
          | otherwise =
              case bcbrDeclarationAuthority refinement of
                BodyConsumerLocallyEmitted {} -> True
                _ -> False

    retainRequiredBinder binder@(ref, mbBound) (retained, requiredRefs)
      | any (typeBinderRefsSameIdentity ref) requiredRefs =
          ( binder : retained
          , foldr
              insertDistinctRef
              requiredRefs
              ( maybe
                  []
                  (freeTypeVarRefsType . tyToElab)
                  mbBound
              )
          )
      | otherwise = (retained, requiredRefs)

    insertDistinctRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    -- The packet was prepared outside the current construction Gamma and can
    -- therefore still quantify an identity that the checked owner has since
    -- inherited from an exact ambient declaration.  Project only those
    -- identity-equal binders out of the leading packet spine; their
    -- occurrences deliberately remain free in the constructed bound.
    projectAmbientPacketBinders ty =
      schemeToType
        ( mkElabSchemeWithRefs
            [ binder
            | binder@(ref, _) <- schemeBinderRefs scheme
            , not
                ( any
                    (typeBinderRefsSameIdentity ref)
                    ambientRefs
                )
            ]
            (schemeBody scheme)
        )
      where
        scheme = schemeFromType ty

-- | Whether this certificate completes a declaration that the exact owner
-- must still emit locally.  Descendant construction environments install the
-- completed bound so their terms can be checked, but the owner's outer
-- environment must not retain that future local declaration as ambient.
bodyConsumerBoundRefinementEmittedBy
  :: LocalGammaOwner
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementEmittedBy owner certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerLocallyEmitted route _ ->
      bcrOwner route == owner
    BodyConsumerPendingOwnerEmission route _ ->
      bcrOwner route == owner
    BodyConsumerOrdinaryOwnerEmission route _ ->
      bcrOwner route == owner
    _ -> False

-- | Whether a refinement is the terminal body-consumer transition of this
-- exact lambda owner.  Ordinary owner-emission certificates can name the same
-- owner while originating on a nested body edge; those refine Gamma but do
-- not authorize rewriting the lambda's published codomain.  Requiring the
-- owner boundary edge and completed construction endpoint separates the two
-- roles by construction.
bodyConsumerBoundRefinementCompletesOwnerEndpoint
  :: LocalGammaOwner
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementCompletesOwnerEndpoint owner certificate =
  bcrOwner route == owner
    && bcrEdgeId route == lgoBoundaryEdge owner
    && typeBinderRefsSameIdentity
      (bcrConstructionRef route)
      (bcbrAmbientRef certificate)
    && operationalEndpointTypesAgree
      (bcrConstructionOperatedType route)
      (bcbrCompletedBound certificate)
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

-- | Whether this exact certificate records the supplied endpoint as its
-- completed declaration state.  Keep the comparison beside the opaque
-- certificate so callers cannot accidentally join a route from one
-- transition to the completed bound of another transition for the same
-- owner and identity.
bodyConsumerBoundRefinementCompletesExactEndpoint
  :: ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementCompletesExactEndpoint endpoint certificate =
  operationalEndpointTypesAgree
    endpoint
    (bcbrCompletedBound certificate)

-- | Check that one construction scheme carries the unique declaration state
-- produced by this refinement.  Keeping this query beside the private
-- certificate constructor prevents callers from separately reading an
-- identity and a bound and accidentally joining them to different schemes.
bodyConsumerBoundRefinementCompletesSchemeDeclaration
  :: BodyConsumerBoundRefinementCertificate
  -> ElabScheme
  -> Bool
bodyConsumerBoundRefinementCompletesSchemeDeclaration certificate scheme =
  case
      [ tyToElab bound
      | (ref, Just bound) <- schemeBinderRefs scheme
      , typeBinderRefsSameIdentity ref (bcbrAmbientRef certificate)
      ]
    of
      [bound] ->
        operationalEndpointTypesAgree
          bound
          (bcbrCompletedBound certificate)
      _ -> False

-- | Whether a staged scheme carries this exact declaration at a state which
-- the certificate did not authorize.  Such a scheme may still be useful as
-- downward checking guidance, but it cannot be published for the owner: the
-- declaration must instead come from the owner-local completed scheme built
-- by the same refinement constructor.
bodyConsumerBoundRefinementRequiresSchemeCompletion
  :: BodyConsumerBoundRefinementCertificate
  -> ElabScheme
  -> Bool
bodyConsumerBoundRefinementRequiresSchemeCompletion certificate scheme =
  case
      [ (ref, maybe TBottom tyToElab mbBound)
      | (ref, mbBound) <- schemeBinderRefs scheme
      , typeBinderRefsSameIdentity ref (bcbrAmbientRef certificate)
      ]
    of
      [(ref, bound)] ->
        not
          ( bodyConsumerBoundRefinementAppliesToDeclarationState
              ref
              bound
              certificate
          )
      _ -> False

-- | Whether this refinement still denotes a declaration owned by some local
-- constructor rather than an ambient Gamma declaration.  Child construction
-- may temporarily install the completed bound in order to check descendants,
-- but an enclosing environment must wait for the exact route owner to emit
-- it.  The declaration-authority constructor is the proof; callers do not
-- infer ownership from the completed type.
bodyConsumerBoundRefinementRequiresOwnerEmission
  :: BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementRequiresOwnerEmission certificate =
  bodyConsumerBoundRefinementEmittedBy
    ( bcrOwner
        ( authorizedBodyConsumerRoute
            (bcbrDeclarationAuthority certificate)
        )
    )
    certificate

-- | Whether an exact reference belongs to a declaration that a future local
-- owner must still close and emit.  This includes both the provisional
-- declaration itself and the leading binders of its closed operated endpoint.
-- The latter can occur free in the intermediate completed bound, but they are
-- private to that bound rather than ambient Gamma dependencies.
bodyConsumerBoundRefinementExcludesAmbientRef
  :: TypeBinderRef
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementExcludesAmbientRef ref certificate =
  privateOperatedBinder
    || ( bodyConsumerBoundRefinementRequiresOwnerEmission certificate
          && bodyConsumerBoundRefinementTargetsAny [ref] certificate
       )
  where
    -- These binders remain lexical to the closed operated endpoint even after
    -- the route owner consumes the provisional declaration.  The sole
    -- exception is a finalized cross-identity packet projection: there the
    -- checked owner has consumed the old graph publication and exported the
    -- packet's exact construction ref as its ambient result endpoint.
    privateOperatedBinder =
      not projectedAmbientBinder
        && any
          (typeBinderRefsSameIdentity ref . fst)
          ( schemeBinderRefs
              ( schemeFromType
                  (bcrOperatedType route)
              )
          )

    projectedAmbientBinder =
      bodyConsumerBoundRefinementOwnerFinalized certificate
        && bodyConsumerBoundRefinementConsumed certificate
        && case bcbrConstructionProjection certificate of
          PacketOperatedBodyConsumerConstructionProjection _ ->
            typeBinderRefsSameIdentity
              ref
              (bcrConstructionRef route)
          DirectBodyConsumerConstructionProjection -> False

    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

-- | Advance an owner-targeted refinement after that owner has completed its
-- constructor.  Local and ambient liveness remain distinct: an exact
-- enclosing packet can consume a would-be local declaration into its ambient
-- Gamma, in which case later root validation must no longer require the
-- completed owner's local closure.  When an exact lambda consumes its own
-- locally emitted completion, retain the pre-closure endpoint as evidence for
-- the result-position forall introduced at that boundary.  A declaration that
-- remains locally emitted retains its original authority; a declaration absent
-- from both classes becomes historical completion proof only.
finalizeBodyConsumerBoundRefinementAtOwner
  :: LocalGammaOwner
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
finalizeBodyConsumerBoundRefinementAtOwner owner localRefs ambientRefs certificate
  | bodyConsumerBoundRefinementConsumed certificate =
      certificate
  | ownerHasCompleted
  , targetRemainsLocal
  , BodyConsumerInheritedAmbient {} <-
      bcbrDeclarationAuthority certificate =
      -- A declaration can be ambient while the lambda body is checked and
      -- nevertheless be owned by the enclosing lambda's exact
      -- Gen(Gamma,tau).  Once that owner publishes the declaration, record
      -- the ownership transition explicitly.  Otherwise an enclosing
      -- application will try to reinstall the now lexically bound identity as
      -- ambient Gamma.
      finalizedBodyConsumerBoundRefinementCertificate
        ( BodyConsumerLocallyEmitted
            route
            (bcbrCompletedBound certificate)
        )
        certificate
  | ownerHasCompleted
  , not targetRemainsLocal
  , targetRemainsAmbient =
      finalizedBodyConsumerBoundRefinementCertificate
        ( BodyConsumerEnclosingAmbient
            route
            ( authorizedBodyConsumerDeclarationBound
                (bcbrDeclarationAuthority certificate)
            )
        )
        certificate
  | ownerHasCompleted
  , not (targetRemainsLocal || targetRemainsAmbient) =
      finalizedBodyConsumerBoundRefinementCertificate
        ( BodyConsumerConsumedAtOwner
            route
            (bcbrCompletedBound certificate)
            consumedOwnerEmissionProgress
        )
        certificate
  | bcrOwner route == owner =
      finalizedBodyConsumerBoundRefinementCertificate
        (bcbrDeclarationAuthority certificate)
        certificate
  | otherwise = certificate
  where
    ownerHasCompleted =
      bodyConsumerBoundRefinementOwnerFinalized certificate
        || bcrOwner route == owner
    targetRemainsLocal =
      bodyConsumerBoundRefinementTargetsAny
        localRefs
        certificate
    targetRemainsAmbient =
      bodyConsumerBoundRefinementTargetsAny
        ambientRefs
        certificate
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

    ordinaryOwnerEmissionProgress currentCertificate =
      case bcbrDeclarationAuthority currentCertificate of
        BodyConsumerPendingOwnerEmission _ progress -> progress
        BodyConsumerOrdinaryOwnerEmission _ progress -> progress
        _ -> emptyOrdinaryOwnerEmissionProgress

    consumedOwnerEmissionProgress =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerLocallyEmitted declarationRoute declarationBound
          | lgoConstructor owner == LocalLambdaGamma
          , bcrOwner declarationRoute == owner
          , bcrEdgeId declarationRoute == lgoBoundaryEdge owner
          , operationalEndpointTypesAgree
              declarationBound
              (bcbrCompletedBound certificate) ->
              recordOrdinaryOwnerScopeClosure
                ( CertifiedFutureOwnerResultClosureSource
                    (bcbrPreviousBound certificate)
                )
                emptyOrdinaryOwnerEmissionProgress
        _ -> ordinaryOwnerEmissionProgress certificate

-- | Preserve a certified descendant completion until the exact route owner
-- has incorporated it into that owner's construction Gamma.  Before that
-- boundary the certificate is an accumulating construction effect, so an
-- intermediate application, let, annotation, or nested lambda cannot discard
-- it merely because the provisional declaration is absent from that
-- intermediate result type.  At the owning boundary the ordinary liveness
-- test applies: root planning may replay only a declaration the owner actually
-- emits or uses as ambient Gamma.
bodyConsumerBoundRefinementSurvivesOwnerBoundary
  :: LocalGammaOwner
  -> [TypeBinderRef]
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementSurvivesOwnerBoundary owner liveRefs certificate =
  bodyConsumerBoundRefinementConsumed certificate
    || bcrOwner route /= owner
    || bodyConsumerBoundRefinementTargetsAny liveRefs certificate
  where
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)

bodyConsumerBoundRefinementConsumed
  :: BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementConsumed certificate =
  case bcbrDeclarationAuthority certificate of
    BodyConsumerConsumedAtOwner {} -> True
    _ -> False

-- | The operated endpoint that packet preparation actually certified for one
-- consumer.  An exact consumer specialization supersedes the packet's raw
-- operated presentation; both the refinement constructor and every later
-- replay must therefore select that same sealed endpoint.
packetConsumerOperatedEndpoint
  :: SubtermConsumerAuthority
  -> PreparedSubtermGeneralization
  -> ElabType
packetConsumerOperatedEndpoint authority packet =
  fromMaybe rawOperatedEndpoint $ do
    (specializedAuthority, endpoint, _, _) <-
      subtermGeneralizationExactConsumerSpecialization packet
    guard (specializedAuthority == authority)
    pure endpoint
  where
    rawOperatedEndpoint =
      schemeToType
        (siScheme (subtermGeneralizationOperatedSchemeInfo packet))

-- | Recover the declaration state carried by the packet's construction view
-- for one exact consumer identity.  This joins a refinement's previous bound
-- to the identity-bearing packet declaration instead of comparing it with the
-- (potentially structured) operated endpoint.
packetConsumerDeclarationBound
  :: TypeBinderRef
  -> PreparedSubtermGeneralization
  -> Maybe ElabType
packetConsumerDeclarationBound consumerRef packet =
  case
      [ maybe TBottom tyToElab mbBound
      | (ref, mbBound) <-
          schemeBinderRefs
            ( siScheme
                (subtermGeneralizationConsumerConstructionSchemeInfo packet)
            )
      , typeBinderRefsSameIdentity ref consumerRef
      ]
    of
      [bound] -> Just bound
      _ -> Nothing

-- | Recover the exact result declaration selected by an identity-topology
-- packet while its lambda owner is still being constructed.  This is the
-- pre-owner counterpart of
-- 'bodyConsumerBoundRefinementCompletedTopologyEndpoint': the declaration
-- may still be pending owner emission, but the packet authority and the
-- refinement certificate must independently agree on its owner, body edge,
-- exterior identity, operated endpoint, and completed bound.
--
-- Keeping this query in the certificate owner prevents lambda construction
-- from inspecting private refinement fields or reconstructing authority from
-- a same-shaped type.  The returned reference is therefore safe to use as the
-- terminal @Hyp@ selected for the body before the lambda is typechecked.
bodyConsumerBoundRefinementTopologyResultRefAtConstruction
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> Maybe TypeBinderRef
bodyConsumerBoundRefinementTopologyResultRefAtConstruction owner bodyEdge packet expectedCompletedBound certificate = do
  authority <- subtermGeneralizationLocalResultAuthority packet
  guard (subtermConsumerAuthorityIsTopology authority)
  guard (subtermConsumerAuthorityEnclosingOwner authority == Just owner)
  guard (scaEdgeId authority == bodyEdge)
  packetResultRef <- subtermGeneralizationResultAbstractionRef packet
  let route =
        authorizedBodyConsumerRoute
          (bcbrDeclarationAuthority certificate)
      exterior = bcrExteriorNode route
      resultRef = bcbrAmbientRef certificate
  guard (bcrOwner route == owner)
  guard (bcrEdgeId route == bodyEdge)
  guard
    ( scaConsumerIdentity authority
        == typeBinderIdentityFromNode exterior
    )
  guard
    ( all
        (typeBinderRefsSameIdentity resultRef)
        [ packetResultRef
        , bcrSemanticRef route
        , bcrConstructionRef route
        ]
    )
  guard
    ( typeBinderRefIdentity resultRef
        == scaConsumerIdentity authority
    )
  guard
    ( operationalEndpointTypesAgree
        (packetConsumerOperatedEndpoint authority packet)
        (bcrOperatedType route)
    )
  packetPreviousBound <- packetConsumerDeclarationBound resultRef packet
  guard
    ( operationalEndpointTypesAgree
        packetPreviousBound
        (bcbrPreviousBound certificate)
    )
  guard
    ( operationalEndpointTypesAgree
        (bcrConstructionOperatedType route)
        (bcbrCompletedBound certificate)
    )
  guard
    ( operationalEndpointTypesAgree
        (bcbrCompletedBound certificate)
        expectedCompletedBound
    )
  pure resultRef

-- | Replay the completed endpoint of one identity-topology packet from the
-- exact post-construction certificate that consumed it.
--
-- The topology authority fixes the source edge, lambda owner, and frozen
-- exterior.  The packet route then has to identify that exterior with every
-- identity-bearing reference retained by the body-consumer certificate.  Only
-- an owner-finalized, consumed certificate whose operated and completed
-- endpoints still agree with the packet/construction pair is accepted.  Root
-- placement may use the returned endpoint to build an exact binder-spine
-- specialization; it need not and must not rediscover the endpoint from the
-- final root type.
bodyConsumerBoundRefinementCompletedTopologyEndpoint
  :: PreparedSubtermGeneralization
  -> BodyConsumerBoundRefinementCertificate
  -> Maybe ElabType
bodyConsumerBoundRefinementCompletedTopologyEndpoint packet certificate = do
  authority <- subtermGeneralizationConsumerAuthority packet
  guard (subtermConsumerAuthorityIsTopology authority)
  owner <- subtermConsumerAuthorityEnclosingOwner authority
  guard (bodyConsumerBoundRefinementOwnerFinalized certificate)
  guard (bodyConsumerBoundRefinementConsumed certificate)
  let route =
        authorizedBodyConsumerRoute
          (bcbrDeclarationAuthority certificate)
      exterior = bcrExteriorNode route
      completedEndpoint = bcbrCompletedBound certificate
  guard (bcrOwner route == owner)
  guard (bcrEdgeId route == scaEdgeId authority)
  guard
    ( scaConsumerIdentity authority
        == typeBinderIdentityFromNode exterior
    )
  let directConsumerRef =
        typeBinderRefFromIdentity
          (scaConsumerIdentity authority)
          (typeBinderIdentityStableName (scaConsumerIdentity authority))
      routedRef =
        fromMaybe
          directConsumerRef
          ( IntMap.lookup
              (getNodeId exterior)
              (siSubstRefs (subtermGeneralizationSchemeInfo packet))
          )
  guard
    ( all
        (typeBinderRefsSameIdentity routedRef)
        [ bcrSemanticRef route
        , bcrConstructionRef route
        , bcbrAmbientRef certificate
        ]
    )
  guard
    ( operationalEndpointTypesAgree
        (packetConsumerOperatedEndpoint authority packet)
        (bcrOperatedType route)
    )
  packetPreviousBound <- packetConsumerDeclarationBound routedRef packet
  guard
    ( operationalEndpointTypesAgree
        packetPreviousBound
        (bcbrPreviousBound certificate)
    )
  guard
    ( operationalEndpointTypesAgree
        (bcrConstructionOperatedType route)
        completedEndpoint
    )
  guard
    ( operationalEndpointTypesAgree
        ( authorizedBodyConsumerDeclarationBound
            (bcbrDeclarationAuthority certificate)
        )
        completedEndpoint
    )
  pure completedEndpoint

-- | Complete the result declaration selected by a packet's distinct local
-- identity-topology authority.  Such a packet can simultaneously be consumed
-- by an enclosing Gamma, so its primary consumer does not describe the
-- current lambda-body edge.  The topology authority fixes the exact result
-- identity and owner; the recursively checked body fixes its bound.
--
-- A prepared result bound may be an unbounded forall skeleton.  Replacing it
-- is permitted only when exact source-scheme inference proves that the checked
-- body is its instance.  The returned certificate carries that state
-- transition through enclosing packet placement, which may have copied the
-- result declaration before the body was checked.
materializeLocalTopologyResultBound
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> ElabType
  -> SchemeInfo
  -> Either
      ElabError
      (SchemeInfo, Maybe BodyConsumerBoundRefinementCertificate)
materializeLocalTopologyResultBound owner bodyEdge packet completedBound schemeInfo =
  case subtermGeneralizationLocalResultAuthority packet of
    Nothing -> pure (schemeInfo, Nothing)
    Just authority -> do
      unless
        ( scaEdgeId authority == bodyEdge
            && subtermConsumerAuthorityEnclosingOwner authority == Just owner
        )
        ( failure
            [ "local topology result belongs to a different lambda boundary"
            , "  authority edge: " ++ show (scaEdgeId authority)
            , "  authority owner: "
                ++ show (subtermConsumerAuthorityEnclosingOwner authority)
            ]
        )
      unless (isNothing (subtermGeneralizationGammaAuthority packet)) $
        failure
          [ "local topology result shares a packet-owned Gamma"
          , "  Gamma authority: "
              ++ show (subtermGeneralizationGammaAuthority packet)
          ]
      exterior <-
        case typeBinderIdentityNode consumerIdentity of
          Just node -> pure node
          Nothing ->
            failure
              [ "local topology result has no graph exterior"
              , "  consumer identity: " ++ show consumerIdentity
              ]
      routedRef <-
        case
            IntMap.lookup
              (getNodeId exterior)
              (schemeInfoBinderRefSubst schemeInfo)
          of
            Just ref -> pure ref
            Nothing ->
              failure
                [ "construction scheme has no exact topology-result route"
                , "  exterior: " ++ show exterior
                , "  routes: "
                    ++ show (schemeInfoBinderRefSubst schemeInfo)
                ]
      (resultRef, mbPreviousBound) <-
        case
            [ binder
            | binder@(ref, _) <- schemeBinderRefs scheme
            , typeBinderRefsSameIdentity ref routedRef
                || typeBinderRefsSameIdentity ref semanticRef
            ]
          of
            [binder] -> pure binder
            binders ->
              failure
                [ "construction scheme has no unique topology-result declaration"
                , "  semantic result: " ++ show semanticRef
                , "  routed result: " ++ show routedRef
                , "  declarations: " ++ show binders
                , "  scheme: " ++ show scheme
                ]
      unless (typeBinderRefsSameIdentity resultRef routedRef) $
        failure
          [ "topology-result declaration disagrees with its exact route"
          , "  declaration: " ++ show resultRef
          , "  route: " ++ show routedRef
          ]
      if
          case completedBound of
            TVarRef completedRef ->
              typeBinderRefsSameIdentity completedRef resultRef
            _ -> False
        then pure (schemeInfo, Nothing)
        else do
          completedBoundTy <-
            either
              ( \cause ->
                  failure
                    [ "checked body is not a legal topology-result bound"
                    , "  checked body: " ++ show completedBound
                    , "  cause: " ++ cause
                    ]
              )
              Right
              (elabToBound completedBound)
          let previousBound = maybe TBottom tyToElab mbPreviousBound
          unless
            ( operationalEndpointTypesAgree previousBound completedBound
                || previousBound == TBottom
                || completeUnboundedForallSpecializesTo
                  previousBound
                  completedBound
            )
            ( failure
                [ "checked body is not the exact completion of the prepared topology-result bound"
                , "  prepared bound: " ++ show previousBound
                , "  checked body: " ++ show completedBound
                , "  packet completed scheme: "
                    ++ show
                      (siScheme (subtermGeneralizationSchemeInfo packet))
                , "  packet construction scheme: "
                    ++ show
                      ( siScheme
                          ( subtermGeneralizationConsumerConstructionSchemeInfo
                              packet
                          )
                      )
                , "  packet operated scheme: "
                    ++ show
                      ( siScheme
                          (subtermGeneralizationOperatedSchemeInfo packet)
                      )
                , "  packet copied-binder routes: "
                    ++ show
                      (subtermGeneralizationCopiedBinderRoutes packet)
                , "  current construction scheme: " ++ show scheme
                ]
            )
          if operationalEndpointTypesAgree previousBound completedBound
            then pure (schemeInfo, Nothing)
            else do
              let completedScheme =
                    mkElabSchemeWithRefs
                      [ if typeBinderRefsSameIdentity ref resultRef
                          then (ref, Just completedBoundTy)
                          else binder
                      | binder@(ref, _) <- schemeBinderRefs scheme
                      ]
                      (schemeBody scheme)
                  completedSchemeInfo =
                    rebuildSchemeInfoFromRefSubst
                      schemeInfo
                      completedScheme
                      (schemeInfoBinderRefSubst schemeInfo)
                  route =
                    BodyConsumerRoute
                      { bcrEdgeId = bodyEdge
                      , bcrOwner = owner
                      , bcrExteriorNode = exterior
                      , bcrSemanticRef = semanticRef
                      , bcrConstructionRef = resultRef
                      , bcrOperatedType = previousBound
                      , bcrConstructionOperatedType = completedBound
                      }
                  certificate =
                    pendingBodyConsumerBoundRefinementCertificate
                      (BodyConsumerLocallyEmitted route completedBound)
                      resultRef
                      previousBound
                      completedBound
              pure (completedSchemeInfo, Just certificate)
      where
        consumerIdentity = scaConsumerIdentity authority
        semanticRef =
          typeBinderRefFromIdentity
            consumerIdentity
            (typeBinderIdentityStableName consumerIdentity)
        scheme = siScheme schemeInfo

        failure :: [String] -> Either ElabError a
        failure details =
          Left
            ( ValidationFailed
                ( [ "cannot materialize local topology-result bound"
                  , "  body edge: " ++ show bodyEdge
                  , "  owner: " ++ show owner
                  ]
                    ++ details
                )
            )

-- | Certify the exact specialization of a topology-result declaration that
-- is already present in the enclosing construction Gamma.  The packet fixes
-- the declaration identity and its lambda owner; the checked source-to-bound
-- computation proves the state transition before the lambda body is built.
-- This is the ambient counterpart of
-- 'materializeLocalTopologyResultBound': it advances the existing
-- declaration instead of emitting a second binder for the same identity.
certifyAmbientTopologyResultBoundRefinement
  :: TypeCheck.Env
  -> LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> Either ElabError BodyConsumerBoundRefinementCertificate
certifyAmbientTopologyResultBoundRefinement typeEnv owner bodyEdge packet ambientRef previousBound completedBound = do
  authority <-
    case subtermGeneralizationLocalResultAuthority packet of
      Just candidate
        | scaEdgeId candidate == bodyEdge
        , subtermConsumerAuthorityEnclosingOwner candidate == Just owner ->
            pure candidate
      candidate ->
        failure
          [ "packet has no local topology-result authority for this lambda"
          , "  authority: " ++ show candidate
          ]
  unless (isNothing (subtermGeneralizationGammaAuthority packet)) $
    failure
      [ "local topology result shares a packet-owned Gamma"
      , "  Gamma authority: "
          ++ show (subtermGeneralizationGammaAuthority packet)
      ]
  let consumerIdentity = scaConsumerIdentity authority
      semanticRef =
        typeBinderRefFromIdentity
          consumerIdentity
          (typeBinderIdentityStableName consumerIdentity)
  exterior <-
    case typeBinderIdentityNode consumerIdentity of
      Just node -> pure node
      Nothing ->
        failure
          [ "local topology result has no graph exterior"
          , "  consumer identity: " ++ show consumerIdentity
          ]
  unless (typeBinderRefsSameIdentity ambientRef semanticRef) $
    failure
      [ "ambient declaration changed the topology-result identity"
      , "  semantic result: " ++ show semanticRef
      , "  ambient result: " ++ show ambientRef
      ]
  case elabToBound completedBound of
    Right _ -> pure ()
    Left cause ->
      failure
        [ "checked body is not a legal topology-result bound"
        , "  checked body: " ++ show completedBound
        , "  cause: " ++ cause
        ]
  specialization <-
    case
        constructExactInstantiation
          typeEnv
          operationalEndpointTypesAgree
          previousBound
          completedBound
      of
      Just instantiation -> pure instantiation
      Nothing ->
        failure
          [ "ambient declaration cannot construct the checked topology-result bound"
          , "  previous bound: " ++ show previousBound
          , "  completed bound: " ++ show completedBound
          ]
  specializedBound <-
    either
      ( \cause ->
          failure
            [ "certified ambient topology-result specialization is not admissible"
            , "  specialization: " ++ show specialization
            , "  cause: " ++ show cause
            ]
      )
      pure
      (TypeCheck.checkInstantiation typeEnv previousBound specialization)
  unless
    (operationalEndpointTypesAgree specializedBound completedBound)
    ( failure
        [ "certified ambient topology-result specialization reached the wrong bound"
        , "  specialization result: " ++ show specializedBound
        , "  completed bound: " ++ show completedBound
        ]
    )
  let route =
        BodyConsumerRoute
          { bcrEdgeId = bodyEdge
          , bcrOwner = owner
          , bcrExteriorNode = exterior
          , bcrSemanticRef = semanticRef
          , bcrConstructionRef = ambientRef
          , bcrOperatedType = previousBound
          , bcrConstructionOperatedType = completedBound
          }
  pure
    ( pendingBodyConsumerBoundRefinementCertificate
        (BodyConsumerInheritedAmbient route previousBound)
        ambientRef
        previousBound
        completedBound
    )
  where
    failure :: [String] -> Either ElabError a
    failure details =
      Left
        ( ValidationFailed
            ( [ "cannot certify ambient topology-result bound refinement"
              , "  body edge: " ++ show bodyEdge
              , "  owner: " ++ show owner
              ]
                ++ details
            )
        )

-- | Certify a local Figure 15.3.5 consumer bound before the lambda term is
-- emitted.  The packet fixes the exact consumer edge, source owner, semantic
-- exterior, and pending declaration.  The current construction scheme must
-- contain one bounded declaration reached through that same identity route,
-- and the recursively checked child must agree with its bound.  The packet's
-- operated endpoint is retained as occurrence-routing evidence; it is not
-- itself the completed bound.  In particular, Var-Abs constructs
-- @parameter -> operated@, so requiring @operated@ to equal that result would
-- reject the paper's ordinary lambda construction.
--
-- This certificate is intentionally suitable for packet placement before the
-- lambda term exists.  A stale packet construction bound may still mention
-- graph-local occurrences that disappeared when the checked child completed
-- the declaration.  Placement may retain the already constructed bound only
-- through this proof, never by comparing the final scheme's shape.
certifyLocalPacketBodyConsumerBoundRefinement
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> [LambdaParamBoundaryCertificate]
  -> SchemeInfo
  -> ElabType
  -> Either ElabError BodyConsumerBoundRefinementCertificate
certifyLocalPacketBodyConsumerBoundRefinement
  owner
  bodyEdge
  packet
  paramBoundaryCertificates
  completedSchemeInfo
  completedBound = do
    unless
      ( lgoConstructor owner == LocalLambdaGamma
          && lgoBoundaryEdge owner == bodyEdge
      )
      ( failure
          [ "current owner is not the exact lambda boundary"
          , "  owner: " ++ show owner
          ]
      )
    authority <-
      case subtermGeneralizationConsumerAuthority packet of
        Just candidate
          | scaEdgeId candidate == bodyEdge
          , subtermConsumerAuthorityEnclosingOwner candidate == Just owner ->
              pure candidate
        candidate ->
          failure
            [ "packet has no local consumer authority for this lambda"
            , "  packet authority: " ++ show candidate
            ]
    unless (isNothing (subtermGeneralizationGammaAuthority packet)) $
      failure
        [ "packet also owns a Gamma; its consumer is not completed by this local declaration"
        , "  Gamma authority: "
            ++ show (subtermGeneralizationGammaAuthority packet)
        ]
    let consumerIdentity = scaConsumerIdentity authority
        semanticRef =
          typeBinderRefFromIdentity
            consumerIdentity
            (typeBinderIdentityStableName consumerIdentity)
        packetConstructionSchemeInfo =
          subtermGeneralizationConsumerConstructionSchemeInfo packet
    exterior <-
      case typeBinderIdentityNode consumerIdentity of
        Just node -> pure node
        Nothing ->
          failure
            [ "packet consumer has no graph exterior"
            , "  consumer identity: " ++ show consumerIdentity
            ]
    (pendingRef, previousBound) <-
      selectDeclaration
        "pending packet"
        False
        exterior
        semanticRef
        packetConstructionSchemeInfo
    (completedRef, completedDeclarationBound) <-
      selectDeclaration
        "completed construction"
        True
        exterior
        semanticRef
        completedSchemeInfo
    pendingRouteRef <-
      requireExteriorRoute
        "pending packet"
        exterior
        packetConstructionSchemeInfo
    completedRouteRef <-
      requireExteriorRoute
        "completed construction"
        exterior
        completedSchemeInfo
    let completeAtConstructionBoundary =
          completeSchemeInfoRouteType completedSchemeInfo
            . completeLambdaParamBoundaryBound paramBoundaryCertificates
        completedDeclarationBoundAtConstruction =
          completeSchemeInfoRouteType completedSchemeInfo
            ( completeLambdaParamBoundaryDeclarationBound
                paramBoundaryCertificates
                completedRef
                completedDeclarationBound
            )
        completedBoundAtConstruction =
          completeAtConstructionBoundary completedBound
    unless
      ( typeBinderRefsSameIdentity pendingRef pendingRouteRef
          && typeBinderRefsSameIdentity completedRef completedRouteRef
          && operationalEndpointTypesAgree
            completedDeclarationBoundAtConstruction
            completedBoundAtConstruction
      )
      ( failure
          [ "completed local declaration lost its exact exterior route or checked bound"
          , "  pending declaration: " ++ show pendingRef
          , "  completed declaration: " ++ show completedRef
          , "  pending exterior route: " ++ show pendingRouteRef
          , "  completed exterior route: " ++ show completedRouteRef
          , "  declaration bound: " ++ show completedDeclarationBound
          , "  declaration bound at construction boundary: "
              ++ show completedDeclarationBoundAtConstruction
          , "  checked child bound: " ++ show completedBound
          , "  checked child bound at construction boundary: "
              ++ show completedBoundAtConstruction
          , "  construction renames: "
              ++ show (subtermGeneralizationConstructionBinderRenames packet)
          , "  compiler-exact renames: "
              ++ show (subtermGeneralizationCompilerExactBinderRenames packet)
          , "  completed substitution: "
              ++ show (schemeInfoBinderRefSubst completedSchemeInfo)
          ]
      )
    case elabToBound completedBoundAtConstruction of
      Right _ -> pure ()
      Left cause ->
        failure
          [ "checked child is not a legal local Gamma bound"
          , "  checked child bound: " ++ show completedBoundAtConstruction
          , "  cause: " ++ cause
          ]
    let localResultCopyRenames =
          case subtermGeneralizationLocalResultAuthority packet of
            Just localAuthority ->
              [ ( typeBinderRefFromIdentity
                    sourceIdentity
                    (typeBinderIdentityStableName sourceIdentity)
                , copiedRef
                )
              | (sourceIdentity, copiedRef) <-
                  subtermGeneralizationCopiedBinderRoutes packet
              , sourceIdentity == scaConsumerIdentity localAuthority
              ]
            Nothing -> []
        preparedOperatedType =
          fromMaybe
            ( schemeToType
                (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
            )
            exactConsumerEndpoint
        exactConsumerEndpoint = do
          (consumerAuthority, endpoint, _, _) <-
            subtermGeneralizationExactConsumerSpecialization packet
          guard (scaEdgeId consumerAuthority == bodyEdge)
          pure endpoint
        operatedType =
          completeSchemeInfoRouteType completedSchemeInfo
            ( renameTypeBinderRefPayloads
                localResultCopyRenames
                ( completeLambdaParamBoundaryType
                    paramBoundaryCertificates
                    preparedOperatedType
                )
            )
    let route =
          BodyConsumerRoute
            { bcrEdgeId = bodyEdge
            , bcrOwner = owner
            , bcrExteriorNode = exterior
            , bcrSemanticRef = semanticRef
            , bcrConstructionRef = completedRef
            , bcrOperatedType = operatedType
            , bcrConstructionOperatedType = completedBoundAtConstruction
            }
    pure
      ( pendingBodyConsumerBoundRefinementCertificate
          ( BodyConsumerLocallyEmitted
              route
              completedBoundAtConstruction
          )
          completedRef
          previousBound
          completedBoundAtConstruction
      )
  where
    selectDeclaration label requireBound exterior semanticRef schemeInfo =
      case
          [ (ref, maybe TBottom tyToElab mbBound)
          | (ref, mbBound) <- schemeBinderRefs (siScheme schemeInfo)
          , any
              (typeBinderRefsSameIdentity ref)
              (consumerConstructionRefs exterior semanticRef schemeInfo)
          , not requireBound || isJust mbBound
          ]
        of
          [declaration] -> pure declaration
          declarations ->
            failure
              [ label
                  ++ " scheme has no unique "
                  ++ (if requireBound then "bounded " else "")
                  ++ "local consumer declaration"
              , "  semantic consumer: " ++ show semanticRef
              , "  declarations: " ++ show declarations
              , "  scheme: " ++ show (siScheme schemeInfo)
              , "  construction routes: "
                  ++ show (schemeInfoBinderRefSubst schemeInfo)
              ]

    consumerConstructionRefs exterior semanticRef schemeInfo =
      semanticRef
        : maybeToList
          ( IntMap.lookup
              (getNodeId exterior)
              (schemeInfoBinderRefSubst schemeInfo)
          )

    requireExteriorRoute label exterior schemeInfo =
      case
          IntMap.lookup
            (getNodeId exterior)
            (schemeInfoBinderRefSubst schemeInfo)
      of
        Just ref -> pure ref
        Nothing ->
          failure
            [ label ++ " has no exact exterior construction route"
            , "  exterior: " ++ show exterior
            , "  routes: " ++ show (schemeInfoBinderRefSubst schemeInfo)
            ]

    failure :: [String] -> Either ElabError a
    failure details =
      Left
        ( ValidationFailed
            ( [ "cannot certify local lambda packet consumer completion"
              , "  body edge: " ++ show bodyEdge
              , "  owner: " ++ show owner
              ]
                ++ details
            )
        )

-- | Install already certified descendant completions into the construction
-- environment used by an enclosing syntax node.  Certificates are
-- accumulating effects: intermediate applications and lets must typecheck
-- their children under the same completed Gamma bound even though the final
-- binder is emitted only by a later owner.
installBodyConsumerBoundRefinements
  :: [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installBodyConsumerBoundRefinements =
  installBodyConsumerBoundRefinementsWithOwner Nothing IntMap.empty

-- | Publish the exact graph-to-ambient route selected by an inherited packet
-- consumer after its certified bound has been installed.  The refinement
-- certificate is constructed from the packet's Gamma authority, its pending
-- exterior route, the recursively checked source, and the unique ambient
-- declaration.  Consequently this transition may replace only the packet's
-- own provisional graph route; it cannot redirect an unrelated alias merely
-- because the two declarations have compatible bounds.
--
-- This route must be available before construction-Gamma requirements are
-- planned.  Otherwise those requirements can rediscover the provisional
-- graph declaration at Bottom and emit @Hyp(graphRef)@ even though the packet
-- has already selected and completed a distinct ambient construction ref.
installBodyConsumerConstructionRoutes
  :: [BodyConsumerBoundRefinementCertificate]
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( IntMap.IntMap TypeBinderRef
      , [(TypeBinderRef, TypeBinderRef)]
      )
installBodyConsumerConstructionRoutes certificates initialAliases initialRenames bindings =
  foldM install (initialAliases, initialRenames) certificates
  where
    install (aliases, renames) certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerInheritedAmbient route declaredBound
          | not (bodyConsumerBoundRefinementOwnerFinalized certificate) -> do
              let semanticRef = bcrSemanticRef route
                  constructionRef = bcrConstructionRef route
                  targetRef = bcbrAmbientRef certificate
                  exterior = bcrExteriorNode route
                  exteriorKey = getNodeId exterior
                  constructionKeys =
                    nub
                      ( exteriorKey
                          : [ getNodeId targetNode
                            | targetNode <- maybeToList (typeBinderRefNode targetRef)
                            ]
                      )
              unless
                ( typeBinderRefIdentity semanticRef
                    == typeBinderIdentityFromNode exterior
                    && ( typeBinderRefsSameIdentity constructionRef targetRef
                          || case
                            certifiedPacketConsumerProjection certificate
                          of
                            Just (projectedRef, _) ->
                              typeBinderRefsSameIdentity
                                constructionRef
                                projectedRef
                            Nothing -> False
                       )
                    && operationalEndpointTypesAgree
                      declaredBound
                      (bcbrPreviousBound certificate)
                )
                ( aliasFailure
                    certificate
                    [ "certificate route no longer denotes its inherited ambient declaration"
                    , "  semantic ref: " ++ show semanticRef
                    , "  construction ref: " ++ show constructionRef
                    , "  target ref: " ++ show targetRef
                    , "  declared bound: " ++ show declaredBound
                    ]
                )
              requireUniqueBinding
                certificate
                "completed ambient target"
                targetRef
                (bcbrCompletedBound certificate)
              installedAliases <-
                foldM
                  (installConstructionKey certificate semanticRef targetRef)
                  aliases
                  constructionKeys
              installedRenames <-
                installConstructionRenames
                  certificate
                  semanticRef
                  targetRef
                  renames
              pure (installedAliases, installedRenames)
        _ -> pure (aliases, renames)

    installConstructionKey certificate semanticRef targetRef aliases nodeKey =
      case IntMap.lookup nodeKey aliases of
        Just established
          | typeBinderRefsSameIdentity established targetRef ->
              pure aliases
          | typeBinderRefsSameIdentity established semanticRef -> do
              requireUniqueBinding
                certificate
                "provisional semantic route"
                semanticRef
                (bcbrPreviousBound certificate)
              pure (IntMap.insert nodeKey targetRef aliases)
          | otherwise ->
              aliasFailure
                certificate
                [ "packet construction node already routes to a third declaration"
                , "  graph node: " ++ show (NodeId nodeKey)
                , "  established route: " ++ show established
                , "  semantic ref: " ++ show semanticRef
                , "  certified ambient ref: " ++ show targetRef
                ]
        Nothing
          | typeBinderRefsSameIdentity semanticRef targetRef -> do
              -- The packet can retain the same semantic identity as its
              -- ambient construction target.  Bound installation has
              -- already advanced that declaration from the certified
              -- previous state to the completed state; requiring Bottom
              -- again here would reject the transition after it succeeded.
              requireUniqueBinding
                certificate
                "completed identity route"
                targetRef
                (bcbrCompletedBound certificate)
              pure (IntMap.insert nodeKey targetRef aliases)
          | otherwise -> do
              requireUniqueBinding
                certificate
                "provisional semantic route"
                semanticRef
                (bcbrPreviousBound certificate)
              pure (IntMap.insert nodeKey targetRef aliases)

    -- An inherited ambient refinement reverses the provisional direction in
    -- which an earlier packet opened this occurrence.  Compose every route
    -- that ended at the semantic placeholder with the certified ambient
    -- target, retire the exact inverse route, and publish the forward route.
    -- The target is now an established declaration and may not itself point to
    -- any third identity.
    installConstructionRenames certificate semanticRef targetRef renames = do
      case
          [ rename
          | rename@(sourceRef, outwardRef) <- renames
          , typeBinderRefsSameIdentity sourceRef targetRef
          , not
              ( typeBinderRefsSameIdentity outwardRef semanticRef
                  || typeBinderRefsSameIdentity outwardRef targetRef
              )
          ]
        of
          [] -> pure ()
          conflicts ->
            aliasFailure
              certificate
              [ "certified ambient target already routes to a third declaration"
              , "  ambient target: " ++ show targetRef
              , "  conflicting renames: " ++ show conflicts
              ]
      foldM
        insertRename
        []
        ( mapMaybe advanceRename renames
            ++ [(semanticRef, targetRef)]
        )
      where
        advanceRename (sourceRef, outwardRef)
          | typeBinderRefsSameIdentity outwardRef semanticRef =
              if typeBinderRefsSameIdentity sourceRef targetRef
                then Nothing
                else Just (sourceRef, targetRef)
          | otherwise = Just (sourceRef, outwardRef)

        insertRename accumulated (sourceRef, outwardRef)
          | typeBinderRefsSameIdentity sourceRef outwardRef = pure accumulated
          | otherwise =
              case
                  find
                    (typeBinderRefsSameIdentity sourceRef . fst)
                    accumulated
                of
                  Nothing -> pure (accumulated ++ [(sourceRef, outwardRef)])
                  Just (_, establishedRef)
                    | typeBinderRefsSameIdentity establishedRef outwardRef ->
                        pure accumulated
                    | otherwise ->
                        aliasFailure
                          certificate
                          [ "packet refinement creates conflicting construction routes"
                          , "  route source: " ++ show sourceRef
                          , "  established target: " ++ show establishedRef
                          , "  certified target: " ++ show outwardRef
                          ]

    requireUniqueBinding certificate role ref expectedBound =
      case
          [ binding
          | binding@(candidateRef, _) <- Map.toList bindings
          , typeBinderRefsSameIdentity candidateRef ref
          ]
        of
          [(_, actualBound)]
            | operationalEndpointTypesAgree actualBound expectedBound ->
                pure ()
            | otherwise ->
                aliasFailure
                  certificate
                  [ role ++ " has a different bound"
                  , "  declaration: " ++ show ref
                  , "  expected bound: " ++ show expectedBound
                  , "  actual bound: " ++ show actualBound
                  ]
          [] ->
            aliasFailure
              certificate
              [ role ++ " is absent from construction Gamma"
              , "  declaration: " ++ show ref
              ]
          matches ->
            aliasFailure
              certificate
              [ role ++ " occurs more than once in construction Gamma"
              , "  declaration: " ++ show ref
              , "  matches: " ++ show matches
              ]

    aliasFailure certificate details =
      Left
        ( ValidationFailed
            ( [ "cannot install certified body-consumer construction route"
              , "  certificate: " ++ show certificate
              ]
                ++ details
            )
        )

-- | Install descendant completions while retaining the exact pending-owner
-- closures of the enclosing construction.  A future local owner may still
-- expose a graph-derived provisional bound after its checked descendant has
-- completed the owner's originally unbounded declaration.  Only the exact
-- owner/edge/exterior closure may authorize replacing that provisional view;
-- the ordinary installer deliberately has no such capability.
installDescendantBodyConsumerBoundRefinements
  :: IntMap.IntMap LocalGammaClosure
  -> [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installDescendantBodyConsumerBoundRefinements closures =
  installBodyConsumerBoundRefinementsWithOwner Nothing closures

-- | The only declaration states accepted while replaying a checked descendant
-- completion.  Keeping the selected output in the plan makes installation
-- mechanical: no caller can validate one relation and then write a different
-- bound.
data BodyConsumerBoundInstallation
  = InstallCompletedBodyConsumerBound !ElabType
  | ReplacePendingOwnerProvisionalBodyConsumerBound !ElabType
  | PreserveOwnerSpecializedBodyConsumerBound !ElabType
  | PreserveOwnerGeneralizedBodyConsumerBound !ElabType
  | PreserveEnclosingSpecializedBodyConsumerBound !ElabType
  | PreserveEnclosingGeneralizedBodyConsumerBound !ElabType

-- | Owner-aware installation for the exact constructor that will emit a
-- pending declaration.  If its checked boundary endpoint is an exact
-- specialization of the descendant's completion, or the exact forall closure
-- that constructs that opened completion, retain the owner's local bound.
-- Intermediate constructors still install the full descendant completion.
installOwnedBodyConsumerBoundRefinements
  :: LocalGammaOwner
  -> [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installOwnedBodyConsumerBoundRefinements owner =
  installBodyConsumerBoundRefinementsWithOwner (Just owner) IntMap.empty

-- | Owner-aware installation with the pending closures needed by foreign
-- descendant certificates that are merely passing through this owner.
installOwnedBodyConsumerBoundRefinementsWithClosures
  :: LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installOwnedBodyConsumerBoundRefinementsWithClosures owner closures =
  installBodyConsumerBoundRefinementsWithOwner (Just owner) closures

installBodyConsumerBoundRefinementsWithOwner
  :: Maybe LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installBodyConsumerBoundRefinementsWithOwner mbOwner closures certificates initialBindings =
  foldM install initialBindings certificates
  where
    install bindings certificate
      | bodyConsumerBoundRefinementConsumed certificate =
          pure bindings
      | finalizedLocalDeclaration certificate =
          -- The exact child owner has already closed this declaration in its
          -- emitted forall spine.  A same-identity entry still visible in an
          -- enclosing graph environment is not a second declaration to
          -- refine: replaying the child certificate here would re-open the
          -- binder outside its lexical ETyAbs.  Owner finalization is the
          -- construction proof, so discard the installation effect before
          -- inspecting the enclosing map.
          pure bindings
      | otherwise =
          case
          [ binding
          | binding@(ref, _) <- Map.toList bindings
          , typeBinderRefsSameIdentity
              ref
              (bcbrAmbientRef certificate)
          ]
        of
          [(ambientRef, currentBound)]
            | Just installation <-
                planBodyConsumerBoundInstallation
                  bindings
                  certificate
                  currentBound ->
                pure
                  ( Map.insert
                      ambientRef
                      (installedBodyConsumerBound installation)
                      ( Map.filterWithKey
                          ( \ref _ ->
                              not
                                ( typeBinderRefsSameIdentity
                                    ref
                                    ambientRef
                                )
                          )
                          bindings
                      )
                  )
            | otherwise ->
                installationFailure
                  certificate
                  [ "current ambient bound matches no certified declaration state"
                  , "  current bound: " ++ show currentBound
                  ]
          []
            | isJust (certifiedPacketConsumerProjection certificate) ->
                -- The enclosing constructor can consume the graph-owned
                -- declaration while retaining the packet's checked operated
                -- binder for its exact Lambda(Gamma) candidate.  That
                -- projection is construction evidence, not authority to
                -- recreate the now-absent ambient graph declaration.
                pure bindings
            | certificateCanInstallMissingDeclaration certificate ->
                pure
                  ( Map.insert
                      (bcbrAmbientRef certificate)
                      (bcbrCompletedBound certificate)
                      bindings
                  )
            | otherwise ->
                installationFailure
                  certificate
                  [ "certificate target is absent from ambient Gamma" ]
          matches ->
            installationFailure
              certificate
              [ "certificate target occurs more than once in ambient Gamma"
              , "  matches: " ++ show matches
              ]

    installedBodyConsumerBound installation =
      case installation of
        InstallCompletedBodyConsumerBound bound -> bound
        ReplacePendingOwnerProvisionalBodyConsumerBound bound -> bound
        PreserveOwnerSpecializedBodyConsumerBound bound -> bound
        PreserveOwnerGeneralizedBodyConsumerBound bound -> bound
        PreserveEnclosingSpecializedBodyConsumerBound bound -> bound
        PreserveEnclosingGeneralizedBodyConsumerBound bound -> bound

    planBodyConsumerBoundInstallation bindings certificate currentBound
      | ownerEmitsSpecializedEndpoint certificate currentBound =
          Just
            ( PreserveOwnerSpecializedBodyConsumerBound
                currentBound
            )
      | ownerEmitsGeneralizedEndpoint certificate currentBound =
          Just
            ( PreserveOwnerGeneralizedBodyConsumerBound
                currentBound
            )
      | bodyConsumerBoundRefinementAcceptsDeclarationState
          certificate
          currentBound =
          Just
            ( InstallCompletedBodyConsumerBound
                (bcbrCompletedBound certificate)
            )
      | pendingOwnerClosurePresentsCompletedBody
          certificate
          currentBound =
          Just
            ( InstallCompletedBodyConsumerBound
                (bcbrCompletedBound certificate)
            )
      | pendingOwnerClosureAuthorizesProvisionalReplacement certificate =
          Just
            ( ReplacePendingOwnerProvisionalBodyConsumerBound
                (bcbrCompletedBound certificate)
            )
      | openedFutureOwnerScopeRequiresClosure
          bindings
          certificate
          currentBound =
          Just
            ( InstallCompletedBodyConsumerBound
                (bcbrCompletedBound certificate)
            )
      | enclosingDeclarationSpecializesCompletion
          certificate
          currentBound =
          Just
            ( PreserveEnclosingSpecializedBodyConsumerBound
                currentBound
            )
      | enclosingDeclarationGeneralizesCompletion
          certificate
          currentBound =
          Just
            ( PreserveEnclosingGeneralizedBodyConsumerBound
                currentBound
            )
      | otherwise = Nothing

    -- A future exact owner can already have a provisional forall closure in
    -- the construction environment before its descendant finishes checking.
    -- That closure may quantify the same source identities in graph order,
    -- while the descendant certificate constructs them in lexical order.
    -- The two types are not interchangeable operational endpoints.  Replace
    -- the provisional declaration only when opening its complete leading
    -- spine exposes the certificate's exact identity-bearing body and every
    -- provisional binder is one of that completed body's identities.
    pendingOwnerClosurePresentsCompletedBody certificate currentBound =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerPendingOwnerEmission {} ->
          forallClosurePresentsSameIdentityBody
            currentBound
            (bcbrCompletedBound certificate)
        _ -> False

    -- The pending scheme is the source-constructor certificate that this
    -- owner began with an unbounded declaration.  Intermediate graph
    -- preparation can give the same exact declaration a non-Bottom bound
    -- before the recursively checked child reaches it.  That presentation is
    -- not another finalized declaration: the exact descendant completion is
    -- the construction result.  Require the complete closure tuple and its
    -- exact unbounded exterior route before allowing the replacement, so a
    -- same-shaped or same-representative binding cannot acquire authority.
    pendingOwnerClosureAuthorizesProvisionalReplacement certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerPendingOwnerEmission route _
          | not (bodyConsumerBoundRefinementOwnerFinalized certificate)
          , pendingOrForwardedCompletedState certificate ->
              case pendingOwnerClosuresFor route certificate of
                [closure] -> pendingClosureDeclaresExactRoute closure route certificate
                _ -> False
        _ -> False

    pendingOrForwardedCompletedState certificate =
      operationalEndpointTypesAgree
        (bcbrPreviousBound certificate)
        TBottom
        || operationalEndpointTypesAgree
          (bcbrPreviousBound certificate)
          (bcbrCompletedBound certificate)

    pendingOwnerClosuresFor route certificate =
      nub
        [ closure
        | closure <- IntMap.elems closures
        , lgcOwner closure == bcrOwner route
        , lgcExteriorNode closure == bcrExteriorNode route
        , lgcConsumerIdentity closure
            == typeBinderRefIdentity (bcbrAmbientRef certificate)
        , bcrEdgeId route `elem` NonEmpty.toList (lgcEdgeIds closure)
        ]

    pendingClosureDeclaresExactRoute closure route certificate =
      case lgcOwnerPendingScheme closure of
        Nothing -> False
        Just pendingScheme ->
          case
              ( IntMap.lookup
                  (getNodeId (bcrExteriorNode route))
                  (schemeInfoBinderRefSubst pendingScheme)
              , [ mbBound
                | (ref, mbBound) <-
                    schemeBinderRefs (siScheme pendingScheme)
                , typeBinderRefsSameIdentity
                    ref
                    (bcbrAmbientRef certificate)
                ]
              )
            of
              (Just routedRef, [Nothing]) ->
                typeBinderRefsSameIdentity
                  routedRef
                  (bcbrAmbientRef certificate)
                  && typeBinderRefsSameIdentity
                    (bcrConstructionRef route)
                    (bcbrAmbientRef certificate)
              _ -> False

    ownerEmitsSpecializedEndpoint certificate currentBound =
      case mbOwner of
        Just owner ->
          bodyConsumerBoundRefinementEmittedBy owner certificate
            && ( bodyConsumerCompletedDeclarationSpecializesToEndpoint
                  certificate
                  currentBound
                  || bodyConsumerOperatedDeclarationConstructsEndpoint
                    certificate
                    currentBound
               )
        Nothing -> False

    -- A descendant can expose an opened view of the declaration which its
    -- exact owner has already constructed as a complete forall closure.  At
    -- that owner the closed bound is the Lambda(Gamma) declaration; only an
    -- intermediate constructor, which has no matching owner authority, may
    -- replace a provisional closure with the descendant completion below.
    ownerEmitsGeneralizedEndpoint certificate currentBound =
      case mbOwner of
        Just owner ->
          bodyConsumerBoundRefinementEmittedBy owner certificate
            && enclosingDeclarationGeneralizesCompletion
              certificate
              currentBound
        Nothing -> False

    -- A prepared enclosing Gamma may already have closed the exact free
    -- dependencies of a descendant completion.  Preserve that declaration
    -- only when the certificate selects the same identity and either the
    -- declaration is the exact identity-bearing universal closure of the
    -- completion or Figure 15.3's binder-spine construction proves the
    -- corresponding instantiation.  This is the dual of an exact owner
    -- specialization above; arbitrary subtyping or same-shaped types do not
    -- enter this state transition.
    enclosingDeclarationGeneralizesCompletion certificate currentBound =
      declarationAuthorityMayBePreparedEnclosing
        (bcbrDeclarationAuthority certificate)
        && ( bodyConsumerDeclarationConstructsCompletion
              certificate
              currentBound
              || exactUnboundedForallClosureOf
              currentBound
              (bcbrCompletedBound certificate)
              || completeUnboundedForallSpecializesTo
              currentBound
              (bcbrCompletedBound certificate)
              || case
                  planExactBinderSpine
                    alphaEqType
                    currentBound
                    (bcbrCompletedBound certificate)
                of
                  Just plan ->
                    exactBinderSpineInstantiation plan /= InstId
                  Nothing -> False
           )

    declarationAuthorityMayBePreparedEnclosing authority =
      case authority of
        BodyConsumerEnclosingAmbient {} -> True
        BodyConsumerPendingOwnerEmission {} -> True
        BodyConsumerOrdinaryOwnerEmission {} -> True
        _ -> False

    -- A sibling can already have selected an exact instance of the completed
    -- declaration before the shared owner is entered.  Preserve that state
    -- only for a declaration whose authority is explicitly carried toward an
    -- enclosing owner and whose exact binder-spine computation is certified.
    enclosingDeclarationSpecializesCompletion certificate currentBound =
      declarationAuthorityMayBePreparedEnclosing
        (bcbrDeclarationAuthority certificate)
        && bodyConsumerCompletedDeclarationSpecializesToEndpoint
          certificate
          currentBound

    -- A lexical copy can open the exact bounded-forall completion while its
    -- binder is in the child constructor, then carry only the open body to an
    -- enclosing Gamma.  The copy consumer records the complete closure before
    -- crossing that boundary.  Close the body again only after every copied
    -- declaration has left ambient Gamma; otherwise the open endpoint still
    -- belongs to the constructor that owns those declarations.
    openedFutureOwnerScopeRequiresClosure bindings certificate endpoint =
      any closesEndpoint
        (bodyConsumerPreparedOwnerScopeClosures certificate)
        || any closesPreparedSource
          (bodyConsumerPreparedCompletionSources certificate)
      where
        closesEndpoint closure@(CertifiedFutureOwnerCopiedScopeClosure declarations openBody) =
          operationalEndpointTypesAgree endpoint openBody
            && certifiedFutureOwnerScopeClosureMatches
              closure
              (bcbrCompletedBound certificate)
            && all declarationIsNoLongerAmbient declarations
        closesEndpoint (CertifiedFutureOwnerResultClosureSource _) = False

        -- When the checked source is itself the completed forall closure, no
        -- separate lexical-copy record is necessary: the source stored in
        -- the progress certificate already fixes its complete binder spine.
        -- Opening that exact spine is admissible only after every binder it
        -- introduced is absent from ambient Gamma.
        closesPreparedSource source =
          operationalEndpointTypesAgree
            source
            (bcbrCompletedBound certificate)
            && not (null declarations)
            && operationalEndpointTypesAgree endpoint (schemeBody sourceScheme)
            && all declarationIsNoLongerAmbient declarations
          where
            sourceScheme = schemeFromType source
            declarations =
              [ (ref, maybe TBottom tyToElab mbBound)
              | (ref, mbBound) <- schemeBinderRefs sourceScheme
              ]

        declarationIsNoLongerAmbient (ref, _) =
          not
            ( any
                (typeBinderRefsSameIdentity ref)
                (Map.keys bindings)
            )

    installationFailure certificate details =
      Left
        ( ValidationFailed
            ( [ "cannot install descendant body-consumer refinement"
              , "  declaration authority: "
                  ++ show (bcbrDeclarationAuthority certificate)
              , "  target: " ++ show (bcbrAmbientRef certificate)
              , "  previous bound: "
                  ++ show (bcbrPreviousBound certificate)
              , "  completed bound: "
                  ++ show (bcbrCompletedBound certificate)
              ]
                ++ details
            )
        )

    certificateCanInstallMissingDeclaration certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerEnclosingAmbient route _ ->
          typeBinderRefsSameIdentity
            (bcrConstructionRef route)
            (bcbrAmbientRef certificate)
        BodyConsumerPendingOwnerEmission route _ ->
          typeBinderRefsSameIdentity
            (bcrConstructionRef route)
            (bcbrAmbientRef certificate)
        BodyConsumerOrdinaryOwnerEmission route _ ->
          typeBinderRefsSameIdentity
            (bcrConstructionRef route)
            (bcbrAmbientRef certificate)
        _ -> False

    finalizedLocalDeclaration certificate
      | not (bodyConsumerBoundRefinementOwnerFinalized certificate) = False
      | otherwise =
          case bcbrDeclarationAuthority certificate of
            BodyConsumerLocallyEmitted {} -> True
            BodyConsumerPendingOwnerEmission {} -> True
            BodyConsumerOrdinaryOwnerEmission {} -> True
            _ -> False

-- | Feed a checked descendant declaration into its exact owner's edge inputs
-- before those edges are combined into Gamma requirements.  The owner boundary
-- keeps either the complete declaration or its certified exact specialization;
-- every sibling edge for that exterior is advanced to the same endpoint.  The
-- same certificate is replayed later by
-- 'inheritOwnedBodyConsumerBoundRefinements' to publish the declaration, but
-- requirement construction must already see the owner-selected endpoint:
-- otherwise two edges for one exterior can be compared using one provisional
-- packet bound and one completed bound.
--
-- Selection is entirely by the certificate's owner and edge identity.  A
-- pre-existing exact endpoint must be either the certified starting point or
-- an exact specialization of the completed declaration; no type-shape or graph
-- representative fallback is accepted.
completeOwnedBodyConsumerRequirementEndpoints
  :: LocalGammaOwner
  -> IntMap.IntMap NodeId
  -> [BodyConsumerBoundRefinementCertificate]
  -> [(EdgeId, Maybe ElabType)]
  -> Either ElabError [(EdgeId, Maybe ElabType)]
completeOwnedBodyConsumerRequirementEndpoints owner edgeExteriors certificates edges = do
  let edgeKeys =
        IntSet.fromList
          [ getEdgeId edgeId
          | (edgeId, _) <- edges
          ]
  unless
    ( length edges == IntSet.size edgeKeys
        && IntMap.keysSet edgeExteriors == edgeKeys
    )
    ( completionFailure
        Nothing
        Nothing
        [ "the frozen edge-to-exterior authority is incomplete or ambiguous"
        , "  requirement inputs: " ++ show edges
        , "  edge exteriors: " ++ show edgeExteriors
        ]
    )
  unless
    ( length ownedCertificates
        == IntSet.size
          ( IntSet.fromList
              [ getEdgeId (bcrEdgeId route)
              | (route, _) <- ownedCertificates
              ]
          )
    )
    ( completionFailure
        Nothing
        Nothing
        [ "the owner has multiple completion certificates for one edge"
        , "  owned certificates: " ++ show ownedCertificates
        ]
    )
  foldM completeEdge edges ownedCertificates
  where
    ownedCertificates =
      [ (route, certificate)
      | certificate <- certificates
      , Just route <- [ownedRoute certificate]
      ]

    ownedRoute certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerEnclosingAmbient route _
          | bcrOwner route == owner -> Just route
        BodyConsumerPendingOwnerEmission route _
          | bcrOwner route == owner -> Just route
        BodyConsumerOrdinaryOwnerEmission route _
          | bcrOwner route == owner -> Just route
        _ -> Nothing

    completeEdge currentEdges (route, certificate) =
      case
          [ edge
          | edge@(edgeId, _) <- currentEdges
          , edgeId == bcrEdgeId route
          ]
        of
          [(edgeId, mbEndpoint)]
            | Just routeExterior <-
                IntMap.lookup (getEdgeId edgeId) edgeExteriors
            , routeExterior == bcrExteriorNode route -> do
                let sharedExteriorEdges =
                      [ candidate
                      | candidate@(candidateEdgeId, _) <- currentEdges
                      , IntMap.lookup
                          (getEdgeId candidateEdgeId)
                          edgeExteriors
                          == Just routeExterior
                      ]
                    incompatibleEndpoints =
                      [ candidate
                      | candidate@(_, candidateEndpoint) <-
                          sharedExteriorEdges
                      , not
                          ( endpointCanAdvance
                              route
                              certificate
                              candidateEndpoint
                          )
                      ]
                unless
                  (null incompatibleEndpoints)
                  ( completionFailure
                      (Just route)
                      (Just certificate)
                      [ "one edge for the certified exterior is not an exact instance of the completed declaration"
                      , "  shared exterior edges: "
                          ++ show sharedExteriorEdges
                      , "  incompatible edges: "
                          ++ show incompatibleEndpoints
                      ]
                  )
                ownerEndpoint <-
                  selectOwnerEndpoint
                    route
                    certificate
                    sharedExteriorEdges
                pure
                  [ if
                      IntMap.lookup
                        (getEdgeId candidateEdgeId)
                        edgeExteriors
                        == Just routeExterior
                      then
                        ( candidateEdgeId
                        , Just ownerEndpoint
                        )
                      else candidate
                  | candidate@(candidateEdgeId, _) <- currentEdges
                  ]
            | otherwise ->
                completionFailure
                  (Just route)
                  (Just certificate)
                  [ "the certificate edge does not carry its frozen exterior authority"
                  , "  current endpoint: " ++ show mbEndpoint
                  , "  edge exterior: "
                      ++ show
                        (IntMap.lookup (getEdgeId edgeId) edgeExteriors)
                  ]
          [] ->
            completionFailure
              (Just route)
              (Just certificate)
              [ "the certificate edge is absent from the owner's requirement inputs"
              , "  requirement inputs: " ++ show currentEdges
              ]
          matches ->
            completionFailure
              (Just route)
              (Just certificate)
              [ "the certificate edge occurs more than once in the owner's requirement inputs"
              , "  matches: " ++ show matches
              ]

    endpointCanAdvance _ _ Nothing = True
    endpointCanAdvance route certificate (Just endpoint) =
      bodyConsumerBoundRefinementAcceptsDeclarationState
        certificate
        endpoint
        || completeUnboundedForallSpecializesTo
          endpoint
          (bcbrPreviousBound certificate)
        || bodyConsumerEndpointIsCertifiedCompletion
          certificate
          endpoint
        || bodyConsumerDeclarationConstructsCompletion
          certificate
          endpoint
        || any
          ( bodyConsumerBoundRefinementCarriesGammaTransition
              endpoint
              (bcbrCompletedBound certificate)
          )
          [ siblingCertificate
          | (siblingRoute, siblingCertificate) <- ownedCertificates
          , bcrExteriorNode siblingRoute == bcrExteriorNode route
          , typeBinderRefsSameIdentity
              (bcbrAmbientRef siblingCertificate)
              (bcbrAmbientRef certificate)
          ]

    selectOwnerEndpoint route certificate sharedExteriorEdges =
      case
          [ mbEndpoint
          | (edgeId, mbEndpoint) <- sharedExteriorEdges
          , edgeId == lgoBoundaryEdge owner
          ]
        of
          [] ->
            selectCommonGeneralizedCompletion
              route
              certificate
              sharedExteriorEdges
          [Nothing] ->
            selectCommonGeneralizedCompletion
              route
              certificate
              sharedExteriorEdges
          [Just endpoint]
            | operationalEndpointTypesAgree
                endpoint
                (bcbrCompletedBound certificate) ->
                pure (bcbrCompletedBound certificate)
            | bodyConsumerEndpointIsCertifiedCompletion
                certificate
                endpoint ->
                pure endpoint
            | bodyConsumerDeclarationConstructsCompletion
                certificate
                endpoint ->
                pure endpoint
            | bodyConsumerBoundRefinementAcceptsDeclarationState
                certificate
                endpoint ->
                pure (bcbrCompletedBound certificate)
            | otherwise ->
                completionFailure
                  (Just route)
                  (Just certificate)
                  [ "the owner boundary endpoint is not constructed by the certified declaration"
                  , "  owner boundary: " ++ show (lgoBoundaryEdge owner)
                  , "  endpoint: " ++ show endpoint
                  ]
          matches ->
            completionFailure
              (Just route)
              (Just certificate)
              [ "the owner boundary edge occurs more than once for the certified exterior"
              , "  endpoints: " ++ show matches
              ]

    -- Several owned edges can complete the same exterior at different exact
    -- instances.  Figure 15.3.5 emits one declaration for that exterior, so
    -- choose the unique completion that constructs every sibling endpoint.
    -- Folding certificates and taking the last completed bound would make the
    -- result depend on edge traversal order (and can reopen a closed source
    -- scheme).  The declaration identity and each private certificate are the
    -- positive authority for this comparison.
    selectCommonGeneralizedCompletion route certificate sharedExteriorEdges =
      case mostGeneralCurrentDeclarations of
        [currentRepresentative] ->
          -- Preserve a declaration only when a live owned edge publishes it
          -- or every exact sibling certificate seals it as a previous
          -- declaration, and its recorded computation reaches every sibling
          -- completion.  More than one such state can be recorded when one
          -- sibling observes a declaration and another observes its exact
          -- instance; choose the unique declaration whose checked xMLF
          -- computation constructs every other candidate.  Bottom on a
          -- provisional edge is not a declaration and therefore cannot
          -- become the representative.
          pure currentRepresentative
        _ : _ ->
          completionFailure
            (Just route)
            (Just certificate)
            [ "sibling edges have multiple equally general declarations"
            , "  generalized edge candidates: "
                ++ show currentDeclarationRepresentatives
            , "  maximal candidates: "
                ++ show mostGeneralCurrentDeclarations
            , "  sibling certificates: " ++ show siblingCertificates
            ]
        [] ->
          if null currentDeclarationRepresentatives
            then case generalizedRepresentatives of
              [] ->
                completionFailure
                  (Just route)
                  (Just certificate)
                  [ "sibling completion certificates have no common generalized endpoint"
                  , "  sibling certificates: " ++ show siblingCertificates
                  ]
              representative : remainingRepresentatives
                | all
                    ( operationalEndpointTypesAgree
                        (bcbrCompletedBound representative)
                        . bcbrCompletedBound
                    )
                    remainingRepresentatives ->
                    pure (bcbrCompletedBound representative)
                | otherwise ->
                    completionFailure
                      (Just route)
                      (Just certificate)
                      [ "sibling completion certificates have multiple incomparable generalized endpoints"
                      , "  generalized candidates: "
                          ++ show generalizedRepresentatives
                      , "  sibling certificates: " ++ show siblingCertificates
                      ]
            else
              completionFailure
                (Just route)
                (Just certificate)
                [ "sibling edges have multiple incomparable generalized declarations"
                , "  generalized edge candidates: "
                    ++ show currentDeclarationRepresentatives
                , "  maximal candidates: []"
                , "  sibling certificates: " ++ show siblingCertificates
                ]
      where
        siblingCertificates =
          [ siblingCertificate
          | (siblingRoute, siblingCertificate) <- ownedCertificates
          , bcrExteriorNode siblingRoute == bcrExteriorNode route
          ]
        currentEndpoints =
          [ endpoint
          | (_, Just endpoint) <- sharedExteriorEdges
          , not (operationalEndpointTypesAgree endpoint TBottom)
          ]
        sealedPreviousDeclarations =
          [ previousBound
          | siblingCertificate <- siblingCertificates
          , let previousBound = bcbrPreviousBound siblingCertificate
          , not (operationalEndpointTypesAgree previousBound TBottom)
          ]
        declarationCandidates =
          currentEndpoints ++ sealedPreviousDeclarations
        currentDeclarationRepresentatives =
          [ candidate
          | candidate <- distinctEndpoints declarationCandidates
          , all
              ( \siblingCertificate ->
                  bodyConsumerDeclarationConstructsCompletion
                    siblingCertificate
                    candidate
              )
              siblingCertificates
          , all (declarationConstructs candidate) currentEndpoints
          ]
        mostGeneralCurrentDeclarations =
          [ candidate
          | candidate <- currentDeclarationRepresentatives
          , all
              (declarationConstructs candidate)
              currentDeclarationRepresentatives
          ]
        generalizedRepresentatives =
          [ candidate
          | candidate <- siblingCertificates
          , all
              (completedDeclarationConstructs candidate)
              siblingCertificates
          ]
        completedDeclarationConstructs candidate endpoint =
          operationalEndpointTypesAgree
            (bcbrCompletedBound candidate)
            (bcbrCompletedBound endpoint)
            || any
              ( bodyConsumerBoundRefinementCarriesGammaTransition
                  (bcbrCompletedBound candidate)
                  (bcbrCompletedBound endpoint)
              )
              siblingCertificates
            || bodyConsumerCompletedDeclarationSpecializesToEndpoint
              candidate
              (bcbrCompletedBound endpoint)

        declarationConstructs source endpoint =
          operationalEndpointTypesAgree source endpoint
            || isJust
              ( planExactBinderSpine
                  operationalEndpointTypesAgree
                  source
                  endpoint
              )
            || isJust
              ( constructExactInstantiation
                  TypeCheck.emptyEnv
                  operationalEndpointTypesAgree
                  source
                  endpoint
              )

        distinctEndpoints =
          foldl'
            ( \endpoints endpoint ->
                if any (operationalEndpointTypesAgree endpoint) endpoints
                  then endpoints
                  else endpoints ++ [endpoint]
            )
            []

    completionFailure
      :: Maybe BodyConsumerRoute
      -> Maybe BodyConsumerBoundRefinementCertificate
      -> [String]
      -> Either ElabError a
    completionFailure mbRoute mbCertificate details =
      Left
        ( ValidationFailed
            ( [ "cannot complete owned body-consumer requirement endpoints"
              , "  owner: " ++ show owner
              ]
                ++ maybe
                  []
                  (pure . ("  route: " ++) . show)
                  mbRoute
                ++ maybe
                  []
                  (pure . ("  certificate: " ++) . show)
                  mbCertificate
                ++ details
            )
        )

-- | Reclassify a descendant body-consumer obligation at the exact lambda
-- whose route owns it.  An open enclosing declaration moves into ambient
-- authority.  A deferred declaration instead remains an exact local
-- requirement with the endpoint selected above (the complete declaration or
-- its certified exact specialization), so this owner emits it.
-- The private declaration-state constructor decides the branch; neither
-- ambient absence nor type shape is rediscovered here.
inheritOwnedBodyConsumerBoundRefinements
  :: LocalGammaOwner
  -> [BodyConsumerBoundRefinementCertificate]
  -> Maybe SchemeInfo
  -> GeneralizationRequirements
  -> Either
      ElabError
      (Maybe SchemeInfo, GeneralizationRequirements)
inheritOwnedBodyConsumerBoundRefinements owner certificates initialOwnerScheme initialRequirements =
  foldM
    inheritCertificate
    (initialOwnerScheme, initialRequirements)
    certificates
  where
    inheritCertificate (ownerScheme, requirements) certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerEnclosingAmbient route previousBound
          | bcrOwner route == owner -> do
              requirement <-
                selectRequirement route certificate requirements
              validateRequirement
                route
                certificate
                previousBound
                requirement
              ambientAuthorities <-
                insertAmbientAuthority route certificate requirements
              pure
                ( ownerScheme
                , requirements
                    { grRequiredGammaBinders =
                        filter
                          (not . sameRequirement requirement)
                          (grRequiredGammaBinders requirements)
                    , grAmbientBinderRefs =
                        insertAmbientRef
                          (bcbrAmbientRef certificate)
                          (grAmbientBinderRefs requirements)
                    , grAmbientGammaAuthorities = ambientAuthorities
                    , grLocallyClosedGammaNodes =
                        IntSet.delete
                          (getNodeId (bcrExteriorNode route))
                          (grLocallyClosedGammaNodes requirements)
                    }
                )
        BodyConsumerPendingOwnerEmission route _
          | bcrOwner route == owner -> do
              requirement <-
                selectRequirement route certificate requirements
              (ownerScheme', ownerEndpoint) <-
                completePendingOwnerScheme
                  route
                  certificate
                  (rgbOperatedType requirement)
                  ownerScheme
              ownerRequirements <-
                emitDeferredRequirement
                  route
                  certificate
                  ownerEndpoint
                  requirements
              pure (Just ownerScheme', ownerRequirements)
        BodyConsumerOrdinaryOwnerEmission route _
          | bcrOwner route == owner -> do
              requirement <-
                selectRequirement route certificate requirements
              (ownerScheme', ownerEndpoint) <-
                completeOrdinaryOwnerScheme
                  route
                  certificate
                  (rgbOperatedType requirement)
                  ownerScheme
              ownerRequirements <-
                emitDeferredRequirement
                  route
                  certificate
                  ownerEndpoint
                  requirements
              pure (ownerScheme', ownerRequirements)
        _ -> pure (ownerScheme, requirements)

    emitDeferredRequirement route certificate ownerEndpoint requirements = do
      requirement <-
        selectRequirement route certificate requirements
      validateRequirement
        route
        certificate
        (bcbrPreviousBound certificate)
        requirement
      let targetRef = bcbrAmbientRef certificate
          exteriorKey = getNodeId (bcrExteriorNode route)
          ambientTargetRefs =
            filter
              (typeBinderRefsSameIdentity targetRef)
              (grAmbientBinderRefs requirements)
      unless
        (null ambientTargetRefs)
        ( inheritanceFailure
            route
            certificate
            [ "deferred declaration is already classified as ambient"
            , "  matching ambient refs: " ++ show ambientTargetRefs
            ]
        )
      case
          IntMap.lookup
            exteriorKey
            (grAmbientGammaAuthorities requirements)
        of
          Nothing -> pure ()
          Just authority ->
            inheritanceFailure
              route
              certificate
              [ "deferred declaration already has ambient Gamma authority"
              , "  authority: " ++ show authority
              ]
      let ownerRequirement =
            requirement
              { rgbOperatedType = ownerEndpoint
              , rgbExactOperatedOccurrenceRef =
                  completedExactOccurrence
                    route
                    ownerEndpoint
              }
      pure
        requirements
          { grRequiredGammaBinders =
              map
                (replaceRequirement requirement ownerRequirement)
                (grRequiredGammaBinders requirements)
          , grLocallyClosedGammaNodes =
              IntSet.delete
                exteriorKey
                (grLocallyClosedGammaNodes requirements)
          }

    completePendingOwnerScheme route certificate ownerEndpoint mbOwnerScheme = do
      unless
        ( bodyConsumerEndpointIsCertifiedCompletion
            certificate
            ownerEndpoint
            || bodyConsumerDeclarationConstructsCompletion
              certificate
              ownerEndpoint
            || siblingTransitionConstructsCompletion
              route
              certificate
              ownerEndpoint
        )
        ( inheritanceFailure
            route
            certificate
            [ "owner completion endpoint is not constructed by the certified declaration"
            , "  owner endpoint: " ++ show ownerEndpoint
            ]
        )
      ownerScheme <-
        case mbOwnerScheme of
          Just schemeInfo -> pure schemeInfo
          Nothing ->
            inheritanceFailure
              route
              certificate
              [ "pending owner emission has no construction scheme" ]
      let binders = schemeBinderRefs (siScheme ownerScheme)
          matchingBinders =
            filter
              ( typeBinderRefsSameIdentity
                  (bcbrAmbientRef certificate)
                  . fst
              )
              binders
      publicationEndpoint <-
        case matchingBinders of
          [(_, currentMbBound)] ->
            selectOwnerPublicationEndpoint
              (maybe TBottom tyToElab currentMbBound)
          [] ->
            inheritanceFailure
              route
              certificate
              [ "pending owner scheme does not declare the certified target"
              , "  scheme: " ++ show (siScheme ownerScheme)
              ]
          matches ->
            inheritanceFailure
              route
              certificate
              [ "pending owner scheme declares the certified target more than once"
              , "  matches: " ++ show matches
              ]
      ownerMbBound <-
        if operationalEndpointTypesAgree
            publicationEndpoint
            TBottom
          then pure Nothing
          else
            Just
              <$> either
                ( \cause ->
                    inheritanceFailure
                      route
                      certificate
                      [ "owner-selected declaration is not a legal bound"
                      , "  cause: " ++ cause
                      ]
                )
                Right
                (elabToBound publicationEndpoint)
      pure
        ( ownerScheme
            { siScheme =
                mkElabSchemeWithRefs
                  [ if
                      typeBinderRefsSameIdentity
                        ref
                        (bcbrAmbientRef certificate)
                      then (ref, ownerMbBound)
                      else binder
                  | binder@(ref, _) <- binders
                  ]
                  (schemeBody (siScheme ownerScheme))
            }
        , publicationEndpoint
        )
      where
        -- A pending owner scheme can already carry the exact specialization
        -- selected by a checked descendant.  Preserve that endpoint and
        -- publish the matching requirement; replacing it with the more
        -- general completed declaration would make the later Hyp impossible.
        -- Other authorized declaration states advance to the endpoint chosen
        -- from the owner's frozen edge inputs.
        selectOwnerPublicationEndpoint currentBound
          | operationalEndpointTypesAgree currentBound ownerEndpoint =
              pure ownerEndpoint
          | bodyConsumerEndpointIsCertifiedCompletion
              certificate
              currentBound =
              pure currentBound
          | bodyConsumerBoundRefinementAcceptsDeclarationState
              certificate
              currentBound =
              pure ownerEndpoint
          | completeUnboundedForallSpecializesTo
              currentBound
              (bcbrPreviousBound certificate) =
              pure ownerEndpoint
          | otherwise =
              inheritanceFailure
                route
                certificate
                [ "pending owner scheme carries a conflicting bound"
                , "  current bound: " ++ show currentBound
                ]

    completeOrdinaryOwnerScheme route certificate ownerEndpoint mbOwnerScheme =
      case mbOwnerScheme of
        Nothing -> pure (Nothing, ownerEndpoint)
        Just ownerScheme
          | any
              ( typeBinderRefsSameIdentity
                  (bcbrAmbientRef certificate)
                  . fst
              )
              (schemeBinderRefs (siScheme ownerScheme)) ->
              do
                (completedOwnerScheme, publicationEndpoint) <-
                  completePendingOwnerScheme
                    route
                    certificate
                    ownerEndpoint
                    (Just ownerScheme)
                pure (Just completedOwnerScheme, publicationEndpoint)
          | otherwise -> pure (Just ownerScheme, ownerEndpoint)

    selectRequirement route certificate requirements =
      case matchingRequirements route requirements of
        [candidate] -> pure candidate
        [] ->
          inheritanceFailure
            route
            certificate
            [ "certificate has no matching current-scope requirement"
            , "  requirements: "
                ++ show (grRequiredGammaBinders requirements)
            ]
        matches ->
          inheritanceFailure
            route
            certificate
            [ "certificate matches multiple current-scope requirements"
            , "  matches: " ++ show matches
            ]

    validateRequirement route certificate previousBound requirement =
      unless
        ( bodyConsumerBoundRefinementAppliesToDeclarationState
            (bcbrAmbientRef certificate)
            (rgbOperatedType requirement)
            certificate
            || pendingRequirementFor
              route
              previousBound
              (rgbOperatedType requirement)
            || bodyConsumerEndpointIsCertifiedCompletion
              certificate
              (rgbOperatedType requirement)
            || siblingTransitionConstructsCompletion
              route
              certificate
              (rgbOperatedType requirement)
        )
        ( inheritanceFailure
            route
            certificate
            [ "matched requirement is neither pending nor completed"
            , "  requirement bound: "
                ++ show (rgbOperatedType requirement)
            ]
        )

    matchingRequirements route requirements =
      [ requirement
      | requirement <- grRequiredGammaBinders requirements
      , rgbExteriorNode requirement == bcrExteriorNode route
      , bcrEdgeId route `elem` NonEmpty.toList (rgbEdgeIds requirement)
      , requiredGammaPlacementIsLocal (rgbPlacement requirement)
      ]

    sameRequirement expected actual =
      rgbExteriorNode expected == rgbExteriorNode actual
        && rgbEdgeIds expected == rgbEdgeIds actual
        && rgbPlacement expected == rgbPlacement actual

    replaceRequirement expected replacement actual
      | sameRequirement expected actual = replacement
      | otherwise = actual

    completedExactOccurrence route completedBound =
      case completedBound of
        TVarRef ref
          | typeBinderRefsSameIdentity
              ref
              (bcrConstructionRef route) ->
              Just ref
        _ -> Nothing

    pendingRequirementFor route previousBound requirementBound =
      operationalEndpointTypesAgree requirementBound previousBound
        || completeUnboundedForallSpecializesTo
          requirementBound
          previousBound
        || exactExteriorSelfOccurrence route requirementBound
        || pendingExteriorChoice route requirementBound

    siblingTransitionConstructsCompletion route certificate declaration =
      any
        ( bodyConsumerBoundRefinementCarriesGammaTransition
            declaration
            (bcbrCompletedBound certificate)
        )
        [ siblingCertificate
        | siblingCertificate <- certificates
        , let siblingRoute =
                authorizedBodyConsumerRoute
                  (bcbrDeclarationAuthority siblingCertificate)
        , bcrOwner siblingRoute == bcrOwner route
        , bcrExteriorNode siblingRoute == bcrExteriorNode route
        , typeBinderRefsSameIdentity
            (bcrSemanticRef siblingRoute)
            (bcrSemanticRef route)
        , typeBinderRefsSameIdentity
            (bcbrAmbientRef siblingCertificate)
            (bcbrAmbientRef certificate)
        ]

    exactExteriorSelfOccurrence route requirementBound =
      case requirementBound of
        TVarRef ref ->
          typeBinderRefsSameIdentity ref (bcrConstructionRef route)
        _ -> False

    pendingExteriorChoice route requirementBound =
      case requirementBound of
        TForallRef pendingRef Nothing _
          | typeBinderRefsSameIdentity
              pendingRef
              (bcrConstructionRef route) ->
              case applyInstantiation
                  requirementBound
                  (InstApp (TVarRef (bcrConstructionRef route)))
                of
                  Right opened ->
                    operationalEndpointTypesAgree
                      opened
                      (TVarRef (bcrConstructionRef route))
                  Left _ -> False
        _ -> False

    insertAmbientAuthority route certificate requirements =
      let key = getNodeId (bcrExteriorNode route)
          incoming =
            AmbientGammaAuthority
              { agaExactRef = bcbrAmbientRef certificate
              , agaBound = bcbrCompletedBound certificate
              }
       in case IntMap.lookup key (grAmbientGammaAuthorities requirements) of
            Nothing ->
              pure
                ( IntMap.insert
                    key
                    incoming
                    (grAmbientGammaAuthorities requirements)
                )
            Just established
              | typeBinderRefsSameIdentity
                  (agaExactRef established)
                  (agaExactRef incoming)
              , operationalEndpointTypesAgree
                  (agaBound established)
                  (bcbrPreviousBound certificate)
                  || operationalEndpointTypesAgree
                    (agaBound established)
                    (agaBound incoming) ->
                  pure
                    ( IntMap.insert
                        key
                        incoming
                        (grAmbientGammaAuthorities requirements)
                    )
              | otherwise ->
                  inheritanceFailure
                    route
                    certificate
                    [ "ambient authority conflicts with the inherited certificate"
                    , "  established authority: " ++ show established
                    , "  incoming authority: " ++ show incoming
                    ]

    insertAmbientRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = refs ++ [ref]

    inheritanceFailure
      :: BodyConsumerRoute
      -> BodyConsumerBoundRefinementCertificate
      -> [String]
      -> Either ElabError a
    inheritanceFailure route certificate details =
      Left
        ( ValidationFailed
            ( [ "cannot inherit owned descendant body-consumer refinement"
              , "  owner: " ++ show owner
              , "  route: " ++ show route
              , "  certificate: " ++ show certificate
              ]
                ++ details
            )
        )

-- | Complete a packet-owned lambda-body Gamma whose declaration has already
-- been opened by an enclosing exact construction.  In this representation,
-- @Gen(Gamma, tau)@ has correctly subtracted the declaration from the current
-- scheme, but the packet's pending construction scheme still records its
-- exact exterior and the enclosing Gamma still carries the unbounded slot.
-- The current scheme must be exactly that pending scheme projected through
-- the ambient declaration.  The packet already owns the Typexp/operated
-- endpoint; requiring the recursively checked body type itself to equal that
-- endpoint would confuse Typ(e) with phi_R;T(e).  The latter computation is
-- constructed and type-checked at the lambda boundary before the certified
-- slot is completed.
--
-- This is not a missing-binder fallback.  The packet authority, pending
-- declaration route, projected construction, current construction route, and
-- unique ambient Bottom declaration must all agree by identity before the
-- recursively checked @Typ(body)@ is installed as its completed bound.  The
-- operated packet remains the route for @phi_R;T(e)@; it is not the completed
-- declaration bound.
certifyAmbientPacketGammaConsumerBoundRefinement
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> SchemeInfo
  -> Maybe CertifiedPacketConsumerBinder
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( TypeBinderRef
      , ElabType
      , BodyConsumerBoundRefinementCertificate
      )
certifyAmbientPacketGammaConsumerBoundRefinement
  owner
  bodyEdge
  packet
  paramBoundaryCertificates
  checkedBodyType
  currentSchemeInfo
  certifiedCurrentBinder
  ambientBindings = do
    unless
      ( lgoConstructor owner == LocalLambdaGamma
          && lgoBoundaryEdge owner == bodyEdge
      )
      ( failure
          [ "current owner is not the exact lambda boundary"
          , "  owner: " ++ show owner
          ]
      )
    gammaAuthority <-
      case subtermGeneralizationGammaAuthority packet of
        Just authority
          | gpaEdgeId authority == bodyEdge
          , genRef (gpaOwnerGen authority) == lgoScope owner ->
              pure authority
        authority ->
          failure
            [ "packet has no local Gamma authority for this lambda"
            , "  packet authority: " ++ show authority
            ]
    let consumerIdentity = gpaConsumerIdentity gammaAuthority
        semanticRef =
          typeBinderRefFromIdentity
            consumerIdentity
            (typeBinderIdentityStableName consumerIdentity)
        pendingSchemeInfo =
          subtermGeneralizationConsumerConstructionSchemeInfo packet
    exterior <-
      case typeBinderIdentityNode consumerIdentity of
        Just node -> pure node
        Nothing ->
          failure
            [ "packet Gamma consumer has no graph exterior"
            , "  consumer identity: " ++ show consumerIdentity
            ]
    pendingRef <-
      requireExteriorRoute
        "pending packet"
        exterior
        pendingSchemeInfo
    case
        [ mbBound
        | (ref, mbBound) <-
            schemeBinderRefs (siScheme pendingSchemeInfo)
        , typeBinderRefsSameIdentity ref pendingRef
        ]
      of
        [Nothing] -> pure ()
        declarations ->
          failure
            [ "pending packet does not contain one unbounded consumer declaration"
            , "  pending consumer: " ++ show pendingRef
            , "  declarations: " ++ show declarations
            ]
    currentRef <-
      case certifiedCurrentBinder of
        Nothing ->
          requireExteriorRoute
            "current construction"
            exterior
            currentSchemeInfo
        Just (CertifiedPacketConsumerBinder certifiedIdentity certifiedRef _) -> do
          operatedRef <-
            requireExteriorRoute
              "operated packet"
              exterior
              (subtermGeneralizationOperatedSchemeInfo packet)
          unless
            ( certifiedIdentity == consumerIdentity
                && typeBinderRefsSameIdentity certifiedRef operatedRef
                && any
                  (typeBinderRefsSameIdentity certifiedRef)
                  ( freeTypeVarRefsType
                      (schemeToType (siScheme currentSchemeInfo))
                  )
            )
            ( failure
                [ "certified child consumer route does not occur in the current packet projection"
                , "  certified exterior: " ++ show certifiedIdentity
                , "  packet exterior: " ++ show consumerIdentity
                , "  certified consumer: " ++ show certifiedRef
                , "  operated consumer: " ++ show operatedRef
                , "  current scheme: "
                    ++ show (siScheme currentSchemeInfo)
                ]
            )
          pure certifiedRef
    unless
      (typeBinderRefIdentity pendingRef == consumerIdentity)
      ( failure
          [ "current construction lost the packet consumer's exact quotient route"
          , "  pending consumer: " ++ show pendingRef
          , "  current consumer: " ++ show currentRef
          ]
      )
    let currentScheme = siScheme currentSchemeInfo
        currentConsumerDeclarations =
          [ binder
          | binder@(ref, _) <- schemeBinderRefs currentScheme
          , typeBinderRefsSameIdentity ref currentRef
          ]
        currentConsumerOccurrences =
          [ ref
          | ref <- freeTypeVarRefsType (schemeToType currentScheme)
          , typeBinderRefsSameIdentity ref currentRef
          ]
    unless
      (null currentConsumerDeclarations)
      ( failure
          [ "current construction still declares the ambient consumer"
          , "  declarations: " ++ show currentConsumerDeclarations
          , "  free occurrences: " ++ show currentConsumerOccurrences
          ]
      )
    let ambientTargetRef =
          case certifiedCurrentBinder of
            Nothing -> currentRef
            Just _ -> pendingRef
    (ambientRef, previousBound) <-
      case
          [ binding
          | binding@(ref, _) <- Map.toList ambientBindings
          , typeBinderRefsSameIdentity ref ambientTargetRef
          ]
        of
          [binding] -> pure binding
          [] ->
            failure
              [ "enclosing Gamma has no exact pending consumer declaration"
              , "  consumer: " ++ show currentRef
              ]
          bindings ->
            failure
              [ "enclosing Gamma has multiple exact pending consumer declarations"
              , "  declarations: " ++ show bindings
              ]
    unless
      ( operationalEndpointTypesAgree previousBound TBottom
          && typeBinderRefsSameIdentity ambientRef ambientTargetRef
      )
      ( failure
          [ "ambient consumer is not the packet's pending Bottom declaration"
          , "  ambient consumer: " ++ show ambientRef
          , "  ambient bound: " ++ show previousBound
          ]
      )
    let constructionRenames =
          [ (pendingRef, currentRef)
          | not (typeBinderRefsSameIdentity pendingRef currentRef)
          ]
        pendingConsumerProjectionTy =
          dropVacuousUnboundedForalls
            ( renameTypeBinderRefPayloads
                constructionRenames
                ( schemeToType
                    ( mkElabSchemeWithRefs
                        [ binder
                        | binder@(ref, _) <-
                            schemeBinderRefs (siScheme pendingSchemeInfo)
                        , not
                            (typeBinderRefsSameIdentity ref pendingRef)
                        ]
                        (schemeBody (siScheme pendingSchemeInfo))
                    )
                )
            )
        currentTy =
          dropVacuousUnboundedForalls (schemeToType currentScheme)
    let pendingProjectionConstructsCurrent =
          operationalEndpointTypesAgree pendingConsumerProjectionTy currentTy
            || case
              planExactBinderSpine
                operationalEndpointTypesAgree
                pendingConsumerProjectionTy
                currentTy
            of
              Just plan ->
                exactBinderSpineInstantiation plan /= InstId
              Nothing -> False
    unless
      pendingProjectionConstructsCurrent
      ( failure
          [ "current construction is not the packet's ambient-consumer projection"
          , "  projected pending type: "
              ++ show pendingConsumerProjectionTy
          , "  current type: " ++ show currentTy
          ]
      )
    let operatedTy =
          completeLambdaParamBoundaryType
            paramBoundaryCertificates
            ( renameTypeBinderRefPayloads
                constructionRenames
                ( schemeToType
                    (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
                )
            )
        operatedAtBoundary =
          subtractAmbientLeadingBinders operatedTy
        completedBound =
          subtractAmbientLeadingBinders checkedBodyType
    case elabToBound completedBound of
      Right _ -> pure ()
      Left cause ->
        failure
          [ "the packet's operated endpoint is not a legal ambient Gamma bound"
          , "  operated endpoint: " ++ show completedBound
          , "  cause: " ++ cause
          ]
    let route =
          BodyConsumerRoute
            { bcrEdgeId = bodyEdge
            , bcrOwner = owner
            , bcrExteriorNode = exterior
            , bcrSemanticRef = semanticRef
            , bcrConstructionRef = currentRef
            , bcrOperatedType = operatedAtBoundary
            , bcrConstructionOperatedType = completedBound
            }
        certificate =
          case certifiedCurrentBinder of
            Just
              ( CertifiedPacketConsumerBinder
                  certifiedIdentity
                  certifiedRef
                  certifiedBound
                )
                | certifiedIdentity == consumerIdentity
                , typeBinderRefsSameIdentity certifiedRef currentRef
                , not
                    ( typeBinderRefsSameIdentity
                        currentRef
                        ambientRef
                    ) ->
                    pendingPacketOperatedBodyConsumerBoundRefinementCertificate
                      (BodyConsumerInheritedAmbient route previousBound)
                      ambientRef
                      certifiedBound
                      previousBound
                      completedBound
            _ ->
              pendingBodyConsumerBoundRefinementCertificate
                (BodyConsumerInheritedAmbient route previousBound)
                ambientRef
                previousBound
                completedBound
    pure (ambientRef, completedBound, certificate)
  where
    requireExteriorRoute label exterior schemeInfo =
      case
          IntMap.lookup
            (getNodeId exterior)
            (schemeInfoBinderRefSubst schemeInfo)
        of
          Just ref -> pure ref
          Nothing ->
            failure
              [ label ++ " has no exact packet-consumer route"
              , "  exterior: " ++ show exterior
              , "  routes: "
                  ++ show (schemeInfoBinderRefSubst schemeInfo)
              ]

    subtractAmbientLeadingBinders ty =
      case ty of
        TForallRef ref _ body
          | any
              (typeBinderRefsSameIdentity ref)
              (Map.keys ambientBindings) ->
              subtractAmbientLeadingBinders body
        _ -> ty

    dropVacuousUnboundedForalls ty =
      case ty of
        TForallRef ref Nothing body
          | not
              ( any
                  (typeBinderRefsSameIdentity ref)
                  (freeTypeVarRefsType body)
              ) ->
              dropVacuousUnboundedForalls body
        TForallRef ref mbBound body ->
          TForallRef
            ref
            mbBound
            (dropVacuousUnboundedForalls body)
        _ -> ty

    failure :: [String] -> Either ElabError a
    failure details =
      Left
        ( ValidationFailed
            ( [ "cannot certify ambient packet-owned Gamma completion"
              , "  body edge: " ++ show bodyEdge
              , "  owner: " ++ show owner
              ]
                ++ details
            )
        )

-- | Complete one descendant lambda-body consumer that is owned by an
-- enclosing lambda Gamma.  The packet freezes the exact consumer edge and
-- enclosing owner, the local-Gamma closure freezes the exterior occurrence
-- and its pending declaration route, and the recursively checked body
-- supplies the completed bound.  Joining those authorities here yields both
-- the ambient reference used by @Hyp@ and the certificate later replayed by
-- the enclosing root binder plan.
--
-- This is intentionally separate from ordinary current-scope body-consumer
-- projection.  A descendant edge need not equal its enclosing owner's
-- boundary edge, so treating it as the current lambda's ordinary
-- @Gen(Gamma,tau)@ requirement would either steal ownership or discard the
-- exact descendant edge.
certifyEnclosingPacketBodyConsumerBoundRefinement
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> TypeBinderRef
  -> SchemeInfo
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
  -> [CertifiedGammaBoundTransition]
  -> IntMap.IntMap LocalGammaClosure
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( TypeBinderRef
      , ElabType
      , [(TypeBinderRef, ElabType)]
      , Instantiation
      , BodyConsumerBoundRefinementCertificate
      )
certifyEnclosingPacketBodyConsumerBoundRefinement
  currentOwner
  bodyEdge
  packet
  selectedConsumerRef
  constructionSchemeInfo
  parameterBoundaryCertificates
  checkedBodyType
  certifiedBodyConstruction
  activeGammaTransitions
  closures
  ambientBindings = do
    unless
      ( lgoConstructor currentOwner == LocalLambdaGamma
          && lgoBoundaryEdge currentOwner == bodyEdge
      )
      ( failure
          [ "current body consumer is not the exact lambda boundary"
          , "  current owner: " ++ show currentOwner
          ]
      )
    authority <-
      case subtermGeneralizationConsumerAuthority packet of
        Just candidate
          | scaEdgeId candidate == bodyEdge
          , Just _ <- subtermConsumerAuthorityEnclosingOwner candidate ->
              pure candidate
        candidate ->
          failure
            [ "packet has no enclosing authority for this body edge"
            , "  packet authority: " ++ show candidate
            ]
    enclosingOwner <-
      case subtermConsumerAuthorityEnclosingOwner authority of
        Just owner -> pure owner
        Nothing -> failure ["packet consumer has no enclosing owner"]
    unless (isNothing (subtermGeneralizationGammaAuthority packet)) $
      failure
        [ "packet also owns a local Gamma; descendant completion is not its construction lane"
        , "  Gamma authority: "
            ++ show (subtermGeneralizationGammaAuthority packet)
        ]
    mbClosure <-
      case subtermGeneralizationLocalConsumerClosure closures packet of
        Just exactClosure -> pure (Just exactClosure)
        Nothing
          | subtermConsumerAuthorityIsTopology authority
          , currentOwner == enclosingOwner ->
              -- Identity-topology authority already freezes the owner,
              -- boundary edge, and exterior node.  Such a packet has no
              -- ordinary binding-tree closure; requiring one would replace
              -- its stronger topology proof with a fictitious lexical
              -- declaration.
              pure Nothing
        Nothing ->
          failure
            [ "packet has no exact enclosing local-Gamma closure"
            , "  enclosing owner: " ++ show enclosingOwner
            ]
    let consumerIdentity = scaConsumerIdentity authority
        semanticRef =
          typeBinderRefFromIdentity
            consumerIdentity
            (typeBinderIdentityStableName consumerIdentity)
        isTopologyConsumer =
          subtermConsumerAuthorityIsTopology authority
        ambientSelectionRef
          | isTopologyConsumer = selectedConsumerRef
          | otherwise = semanticRef
    exterior <-
      case mbClosure of
        Just closure -> pure (lgcExteriorNode closure)
        Nothing ->
          case typeBinderIdentityNode consumerIdentity of
            Just node -> pure node
            Nothing ->
              failure
                [ "topology consumer has no graph exterior"
                , "  consumer: " ++ show consumerIdentity
                ]
    case mbClosure of
      Just closure ->
        unless
          ( lgcOwner closure == enclosingOwner
              && bodyEdge `elem` NonEmpty.toList (lgcEdgeIds closure)
              && lgcConsumerIdentity closure == consumerIdentity
              && typeBinderIdentityFromNode exterior == consumerIdentity
          )
          ( failure
              [ "enclosing closure changed the packet's owner, edge, or exterior"
              , "  closure: " ++ show closure
              ]
          )
      Nothing ->
        unless
          ( subtermConsumerAuthorityIsTopology authority
              && currentOwner == enclosingOwner
              && scaEdgeId authority == bodyEdge
              && typeBinderIdentityFromNode exterior == consumerIdentity
          )
          ( failure
              [ "topology authority changed owner, edge, or exterior"
              , "  authority: " ++ show authority
              ]
          )
    let pendingOwnerEmission =
          isJust (mbClosure >>= lgcOwnerPendingScheme)
            && currentOwner /= enclosingOwner
    ( ambientRef
      , previousBound
      , deferredEnclosingDeclaration
      , deferredWithoutPendingRoute
      ) <-
      case
          [ binding
          | binding@(ref, _) <- Map.toList ambientBindings
          , typeBinderRefsSameIdentity ref ambientSelectionRef
          ]
        of
          [(ref, bound)] ->
            pure
              ( ref
              , bound
              , currentOwner /= enclosingOwner
              , currentOwner /= enclosingOwner
                  && isNothing (mbClosure >>= lgcOwnerPendingScheme)
              )
          []
            | currentOwner /= enclosingOwner ->
                -- A descendant can reach the completion before the enclosing
                -- owner's declaration is installed in this particular child
                -- environment.  A recorded pending owner scheme proves the
                -- declaration route; an ordinary owner without such a scheme
                -- is certified by the exact packet/closure pair.  Both are
                -- future local emissions, never ambient fallbacks.
                pure
                  ( semanticRef
                  , TBottom
                  , True
                  , isNothing (mbClosure >>= lgcOwnerPendingScheme)
                  )
          [] ->
            failure
              [ "enclosing Gamma has no exact provisional consumer binding"
              , "  consumer: " ++ show ambientSelectionRef
              , "  enclosing owner: " ++ show enclosingOwner
              , "  enclosing closure: " ++ show mbClosure
              , "  available Gamma bindings: "
                  ++ show (Map.toList ambientBindings)
              ]
          bindings ->
            failure
              [ "enclosing Gamma has multiple provisional consumer bindings"
              , "  bindings: " ++ show bindings
              ]
    let pendingEndpoint = TVarRef ambientRef
        gammaBoundScheme =
          subtermGeneralizationGammaBoundScheme packet
        completeDeclarationBound =
          completeLambdaParamBoundaryDeclarationBound
            parameterBoundaryCertificates
            ambientRef
        publishedBound =
          fromMaybe
            ( completeDeclarationBound
                ( subtractAmbientLeadingBinders
                    (schemeToType gammaBoundScheme)
                )
            )
            sourceOwnerPublishedBound
        sourceOwnerPublishedBound = do
          guard (isNothing mbClosure)
          ( completedAuthority
            , completedOwner
            , _
            , completedEndpoint
            ) <-
              subtermGeneralizationSourceOwnerConsumerCompletion packet
          guard
            ( completedAuthority == authority
                && completedOwner == enclosingOwner
            )
          pure (completeDeclarationBound completedEndpoint)
    completedBoundDependencies <-
      exactCompletedBoundDependencies
        ambientRef
        [ gammaBoundScheme
        , siScheme (subtermGeneralizationOperatedSchemeInfo packet)
        , siScheme
            (subtermGeneralizationConsumerConstructionSchemeInfo packet)
        , siScheme (subtermGeneralizationSchemeInfo packet)
        ]
        publishedBound
    let ambientTypeEnv =
          foldl'
            ( \typeEnv (ref, bound) ->
                TypeCheck.insertTypeBindingRef ref bound typeEnv
            )
            (TypeCheck.mkTypeCheckEnvWithResolvedTerms [] ambientBindings)
            completedBoundDependencies
        constructionDeclarationProvides source endpoint =
          operationalEndpointTypesAgree source endpoint
            || isJust
              ( planExactBinderSpine
                  operationalEndpointTypesAgree
                  source
                  endpoint
              )
            || isJust
              ( constructExactInstantiation
                  ambientTypeEnv
                  operationalEndpointTypesAgree
                  source
                  endpoint
              )
        checkedBodyToPublished =
          case publishedBound of
            TVarRef publishedRef
              | typeBinderRefsSameIdentity publishedRef ambientRef ->
                  Nothing
            _ ->
              constructExactInstantiation
                ambientTypeEnv
                operationalEndpointTypesAgree
                checkedBodyType
                publishedBound
        (completedGammaBound, sourceSpecialization) =
          case checkedBodyToPublished of
            Just specialization -> (publishedBound, specialization)
            Nothing -> (checkedBodyType, InstId)
        consumerCompletionAuthorized =
          deferredEnclosingDeclaration
            || operationalEndpointTypesAgree previousBound TBottom
            || ( constructionDeclarationProvides
                   publishedBound
                   completedGammaBound
                  && constructionDeclarationProvides
                    completedGammaBound
                    previousBound
               )
    case mbClosure >>= lgcOwnerPendingScheme of
      Just pending ->
        validatePendingRoute
          "enclosing closure"
          exterior
          ambientRef
          pending
      Nothing
        | currentOwner /= enclosingOwner -> pure ()
      Nothing
        | isJust mbClosure ->
        failure
          [ "enclosing closure has no pending construction scheme" ]
      Nothing -> pure ()
    unless deferredWithoutPendingRoute $
      if isTopologyConsumer
        then
          -- A topology consumer is ambient in the current construction even
          -- when an ordinary closure still records where its enclosing owner
          -- first opened the pending declaration.  The closure is validated
          -- above; the projected packet must now retain a free exact route,
          -- not redeclare that pending binder locally.
          validateTopologyAmbientRoute
            exterior
            ambientRef
            constructionSchemeInfo
        else
          validatePendingRoute
            "packet construction"
            exterior
            ambientRef
            (subtermGeneralizationConsumerConstructionSchemeInfo packet)
    let operatedSchemeInfo =
          subtermGeneralizationOperatedSchemeInfo packet
        operatedType =
          schemeToType (siScheme operatedSchemeInfo)
        -- Complete the exact parameter while its source forall and the
        -- ambient consumer's bound still occur in the same packet spine.
        -- Only then open declarations already owned by ambient Gamma;
        -- reversing this order loses the bound needed to construct the
        -- parameter domain in Figure 15.3.5.
        completedOperatedType =
          subtractAmbientLeadingBinders
            ( completeLambdaParamBoundaryOperatedType
                parameterBoundaryCertificates
                (schemeInfoBinderRefSubst operatedSchemeInfo)
                ambientProjectionRefs
                ambientRef
                completedGammaBound
                operatedType
            )
        checkedBodyConstructsOperated =
          constructionDeclarationProvides
            checkedBodyType
            completedOperatedType
        checkedBodyCompletesConsumer =
          checkedBodyConstructsOperated
            && constructionDeclarationProvides
              checkedBodyType
              completedGammaBound
        certifiedOperatedTransitionCompletesConsumer =
          any activeTransitionCompletesConsumer activeGammaTransitions
        -- The packet/closure pair has already selected this exact ambient
        -- declaration by identity.  Its current bound may contain several
        -- application-Gamma stages, so replay the complete xMLF
        -- instantiation instead of requiring one child certificate to own
        -- both the outer declaration and every nested stage.
        ambientDeclarationCompletesConsumer =
          constructionDeclarationProvides
            previousBound
            completedGammaBound
        activeTransitionCompletesConsumer
          ( CertifiedGammaBoundTransition
              transitionRef
              transitionSource
              transitionTarget
              _
            ) =
            typeBinderRefsSameIdentity transitionRef ambientRef
              && constructionDeclarationProvides
                operatedType
                transitionSource
              && constructionDeclarationProvides
                transitionTarget
                completedGammaBound
        opaqueSourceLambdaBodyCompletion =
          certifyOpaqueSourceLambdaBodyCompletion
            currentOwner
            bodyEdge
            packet
            parameterBoundaryCertificates
            ambientBindings
            checkedBodyType
            completedGammaBound
            operatedType
            certifiedBodyConstruction
        opaqueSourceLambdaBodyCompletionCertified =
          either (const False) (const True)
            opaqueSourceLambdaBodyCompletion
        sourceOwnerConsumerBodyCompletion =
          certifySourceOwnerConsumerBodyCompletion
            ambientTypeEnv
            currentOwner
            bodyEdge
            enclosingOwner
            authority
            packet
            parameterBoundaryCertificates
            checkedBodyType
            completedGammaBound
            operatedType
            certifiedBodyConstruction
        sourceOwnerConsumerBodyCompletionCertified =
          either (const False) (const True)
            sourceOwnerConsumerBodyCompletion
        openValueLambdaParameterClosure =
          certifyOpenValueLambdaParameterClosure
            publishedBound
            completedGammaBound
            certifiedBodyConstruction
        openValueLambdaParameterClosureCertified =
          either (const False) (const True)
            openValueLambdaParameterClosure
    operatedEndpoint <-
      case operatedType of
        TForallRef pendingRef Nothing _
          | typeBinderRefsSameIdentity pendingRef ambientRef ->
              either
                ( \cause ->
                    failure
                      [ "pending operated consumer cannot be opened at its ambient declaration"
                      , "  packet operated type: " ++ show operatedType
                      , "  cause: " ++ show cause
                      ]
                )
                pure
                ( applyInstantiation
                    operatedType
                    (InstApp (TVarRef ambientRef))
                )
        _
          | consumerCompletionAuthorized
          , constructionDeclarationProvides
                completedOperatedType
                completedGammaBound
              || checkedBodyCompletesConsumer
              || opaqueSourceLambdaBodyCompletionCertified
              || sourceOwnerConsumerBodyCompletionCertified
              || openValueLambdaParameterClosureCertified
              || certifiedOperatedTransitionCompletesConsumer
              || ambientDeclarationCompletesConsumer ->
              -- The packet may retain a completed forall spine.  Construct
              -- its exact Figure 15.3.5 instantiation plan (including N for
              -- bounded binders) and accept only a plan whose replay reaches
              -- the checked body endpoint.  A direct N step may choose an
              -- argument constructed from the binder's bound; that checked
              -- computation need not factor through the bound and then
              -- specialize beneath an arrow.  In that case the checked body
              -- must independently construct both the operated endpoint and
              -- the completed Gamma bound.  That bound may be the checked
              -- body itself when the provisional graph publication is no
              -- longer its endpoint; requiring a route back to that stale
              -- presentation would discard the positive body certificate.
              -- A source-lambda opaque completion is the commuting case: its
              -- packet authority and the checked child's exact returned
              -- result jointly certify the endpoint even though no plain
              -- instantiation relates the two presentations.  No final-type
              -- shape is used to invent a declaration or an equality.
              pure completedGammaBound
        _ ->
          failure
            [ "packet operated view is neither the exact pending consumer nor its certified completion"
            , "  packet operated type: " ++ show operatedType
            , "  completed packet operated type: "
                ++ show completedOperatedType
            , "  parameter boundary changed operated type: "
                ++ show
                  ( not
                      ( operationalEndpointTypesAgree
                          operatedType
                          completedOperatedType
                      )
                  )
            , "  checked body constructs completed operated type: "
                ++ show checkedBodyConstructsOperated
            , "  completed operated type constructs completed Gamma bound: "
                ++ show
                  ( constructionDeclarationProvides
                      completedOperatedType
                      completedGammaBound
                  )
            , "  packet operated routes: "
                ++ show
                  ( schemeInfoBinderRefSubst
                      (subtermGeneralizationOperatedSchemeInfo packet)
                  )
            , "  packet construction routes: "
                ++ show
                  ( schemeInfoBinderRefSubst
                      ( subtermGeneralizationConsumerConstructionSchemeInfo
                          packet
                      )
                  )
            , "  ambient consumer: " ++ show ambientRef
            , "  enclosing owner: " ++ show enclosingOwner
            , "  deferred enclosing declaration: "
                ++ show deferredEnclosingDeclaration
            , "  previous consumer bound: " ++ show previousBound
            , "  published consumer bound: " ++ show publishedBound
            , "  packet published construction: "
                ++ show
                  ( schemeToType
                      ( siScheme
                          (subtermGeneralizationSchemeInfo packet)
                      )
                  )
            , "  packet administrative result construction: "
                ++ show
                  ( subtermGeneralizationAdministrativeLambdaResultConstruction
                      packet
                  )
            , "  packet opaque result construction: "
                ++ show
                  (subtermGeneralizationOpaqueResultConstruction packet)
            , "  certified opaque source-lambda body completion: "
                ++ show opaqueSourceLambdaBodyCompletion
            , "  certified source-owner body completion: "
                ++ show sourceOwnerConsumerBodyCompletion
            , "  certified open value-lambda parameter closure: "
                ++ show openValueLambdaParameterClosure
            , "  source-owner consumer completion: "
                ++ show
                  (subtermGeneralizationSourceOwnerConsumerCompletion packet)
            , "  consumer completion authorized: "
                ++ show consumerCompletionAuthorized
            , "  parameter boundary certificates: "
                ++ show parameterBoundaryCertificates
            , "  checked body type: " ++ show checkedBodyType
            , "  completed Gamma bound: " ++ show completedGammaBound
            ]
    unless
      ( operationalEndpointTypesAgree operatedEndpoint pendingEndpoint
          || ( consumerCompletionAuthorized
                && operationalEndpointTypesAgree
                  operatedEndpoint
                  completedGammaBound
             )
      )
      ( failure
          [ "packet operated endpoint matches neither its pending nor completed state"
          , "  operated endpoint: " ++ show operatedEndpoint
          , "  pending endpoint: " ++ show pendingEndpoint
          , "  checked body type: " ++ show checkedBodyType
          , "  completed Gamma bound: " ++ show completedGammaBound
          ]
      )
    unless
      ( constructionDeclarationProvides publishedBound completedGammaBound
          || constructionDeclarationProvides
            checkedBodyType
            completedGammaBound
          || openValueLambdaParameterClosureCertified
          || ( operationalEndpointTypesAgree previousBound TBottom
                && case publishedBound of
                  TVarRef publishedRef ->
                    typeBinderRefsSameIdentity publishedRef ambientRef
                  _ -> False
             )
      )
      ( failure
          [ "checked body cannot complete the packet's published enclosing Gamma bound"
          , "  published bound: " ++ show publishedBound
          , "  pending endpoint: " ++ show pendingEndpoint
          , "  checked body type: " ++ show checkedBodyType
          , "  completed Gamma bound: " ++ show completedGammaBound
          ]
      )
    specializedBodyType <-
      either
        ( \cause ->
            failure
              [ "certified body-to-Gamma specialization does not typecheck"
              , "  checked body type: " ++ show checkedBodyType
              , "  specialization: " ++ show sourceSpecialization
              , "  cause: " ++ show cause
              ]
        )
        pure
        ( TypeCheck.checkInstantiation
            ambientTypeEnv
            checkedBodyType
            sourceSpecialization
        )
    unless
      (operationalEndpointTypesAgree specializedBodyType completedGammaBound)
      ( failure
          [ "certified body-to-Gamma specialization reaches a different endpoint"
          , "  checked body type: " ++ show checkedBodyType
          , "  specialization: " ++ show sourceSpecialization
          , "  specialized type: " ++ show specializedBodyType
          , "  completed Gamma bound: " ++ show completedGammaBound
          ]
      )
    case elabToBound completedGammaBound of
      Right _ -> pure ()
      Left cause ->
        failure
          [ "checked body is not a legal enclosing Gamma bound"
          , "  checked body type: " ++ show checkedBodyType
          , "  completed Gamma bound: " ++ show completedGammaBound
          , "  cause: " ++ cause
          ]
    let route =
          BodyConsumerRoute
            { bcrEdgeId = bodyEdge
            , bcrOwner = enclosingOwner
            , bcrExteriorNode = exterior
            , bcrSemanticRef = semanticRef
            , bcrConstructionRef = ambientRef
            , bcrOperatedType = checkedBodyType
            , bcrConstructionOperatedType = completedGammaBound
            }
        declarationAuthority
          | pendingOwnerEmission =
              BodyConsumerPendingOwnerEmission
                route
                futureOwnerEmissionProgress
          | deferredEnclosingDeclaration =
              BodyConsumerOrdinaryOwnerEmission
                route
                futureOwnerEmissionProgress
          | otherwise =
              BodyConsumerEnclosingAmbient route previousBound
        futureOwnerEmissionProgress =
          foldl'
            (flip recordOrdinaryOwnerScopeClosure)
            ( foldl'
                (flip recordOrdinaryOwnerPreparedSource)
                emptyOrdinaryOwnerEmissionProgress
                certifiedFutureOwnerSources
            )
            certifiedFutureOwnerScopeClosures
        certifiedFutureOwnerSources =
          [ checkedBodyType
          | constructionDeclarationProvides
              checkedBodyType
              completedGammaBound
          ]
            ++ [ publishedBound
               | publishedBoundIsConcrete
               , constructionDeclarationProvides
                   publishedBound
                   completedGammaBound
                   || futureOwnerConstructionCertified
               ]
            ++ [ constructedType
               | bodyConstruction <- maybeToList certifiedBodyConstruction
               , let constructedType =
                       certifiedLambdaBodyConstructedType bodyConstruction
               , constructionDeclarationProvides
                   constructedType
                   completedGammaBound
                   || futureOwnerConstructionCertified
               ]
        certifiedFutureOwnerScopeClosures =
          [ closure
          | source <- certifiedFutureOwnerSources
          , bodyConstruction <- maybeToList certifiedBodyConstruction
          , closure <-
              maybeToList
                ( certifyFutureOwnerScopeClosure
                    source
                    bodyConstruction
                )
          ]
        publishedBoundIsConcrete =
          case publishedBound of
            TVarRef publishedRef ->
              not (typeBinderRefsSameIdentity publishedRef ambientRef)
            _ -> True
        futureOwnerConstructionCertified =
          opaqueSourceLambdaBodyCompletionCertified
            || sourceOwnerConsumerBodyCompletionCertified
            || openValueLambdaParameterClosureCertified
        certificate =
          pendingBodyConsumerBoundRefinementCertificate
            declarationAuthority
            ambientRef
            previousBound
            completedGammaBound
    pure
      ( ambientRef
      , completedGammaBound
      , completedBoundDependencies
      , sourceSpecialization
      , certificate
      )
  where
    certifyFutureOwnerScopeClosure source bodyConstruction = do
      copiedDeclarations <-
        traverse
          copiedAmbientDeclaration
          relevantAmbientDeclarations
      guard (not (null copiedDeclarations))
      let openBody =
            renameTypeBinderRefPayloads
              unambiguousScopeRenames
              source
      guard
        ( all
            ( \(copiedRef, _) ->
                any
                  (typeBinderRefsSameIdentity copiedRef)
                  (freeTypeVarRefsType openBody)
            )
            copiedDeclarations
        )
      pure
        ( CertifiedFutureOwnerCopiedScopeClosure
            copiedDeclarations
            openBody
        )
      where
        scopeRenames =
          certifiedLambdaBodyScopeDependencyRenames
            bodyConstruction
        unambiguousScopeRenames =
          [ rename
          | rename@(sourceRef, _) <- scopeRenames
          , [_] <- [distinctCopyTargets sourceRef]
          ]
        relevantAmbientDeclarations =
          [ authority
          | authority <-
              certifiedLambdaBodyAmbientDeclarations
                bodyConstruction
          , any
              (typeBinderRefsSameIdentity (agaExactRef authority))
              (freeTypeVarRefsType source)
          , [_] <- [distinctCopyTargets (agaExactRef authority)]
          ]

        copiedAmbientDeclaration authority = do
          copiedRef <-
            case distinctCopyTargets (agaExactRef authority) of
              [candidate] -> Just candidate
              _ -> Nothing
          pure
            ( copiedRef
            , renameTypeBinderRefPayloads
                unambiguousScopeRenames
                (agaBound authority)
            )

        distinctCopyTargets sourceRef =
          foldl'
            insertDistinctRef
            []
            [ targetRef
            | (candidateSourceRef, targetRef) <- scopeRenames
            , typeBinderRefsSameIdentity
                sourceRef
                candidateSourceRef
            , not
                ( typeBinderRefsSameIdentity
                    sourceRef
                    targetRef
                )
            ]

        insertDistinctRef refs ref
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = refs ++ [ref]

    exactCompletedBoundDependencies
      consumerRef
      packetSchemes
      completedBound =
        go [] (freeTypeVarRefsType completedBound)
      where
        packetDeclarations =
          concatMap schemeBinderRefs packetSchemes

        certifiedChildAmbientBounds dependencyRef =
          [ agaBound authority
          | bodyConstruction <- maybeToList certifiedBodyConstruction
          , authority <-
              certifiedLambdaBodyAmbientDeclarations bodyConstruction
          , typeBinderRefsSameIdentity
              dependencyRef
              (agaExactRef authority)
          ]

        go dependencies [] = pure dependencies
        go dependencies (dependencyRef : rest)
          | typeBinderRefsSameIdentity dependencyRef consumerRef =
              go dependencies rest
          | any
              (typeBinderRefsSameIdentity dependencyRef . fst)
              dependencies =
              go dependencies rest
          | otherwise = do
              dependencyBound <-
                case
                    distinctBounds
                      ( [ maybe TBottom tyToElab mbBound
                        | (declaredRef, mbBound) <- packetDeclarations
                        , typeBinderRefsSameIdentity
                            declaredRef
                            dependencyRef
                        ]
                          ++ certifiedChildAmbientBounds dependencyRef
                      )
                  of
                    [bound] -> pure bound
                    [] ->
                      case
                          distinctBounds
                            [ bound
                            | (ambientRef, bound) <-
                                Map.toList ambientBindings
                            , typeBinderRefsSameIdentity
                                ambientRef
                                dependencyRef
                            ]
                        of
                          [bound] -> pure bound
                          [] ->
                            if
                                any
                                  ( typeBinderRefsSameIdentity
                                      dependencyRef
                                  )
                                  inheritedPacketRefs
                              then
                                -- Packet preparation proved that this exact
                                -- declaration belongs to an enclosing lexical
                                -- Gamma.  It need not be duplicated in the
                                -- packet scheme or the current child's
                                -- temporary environment; install its
                                -- unbounded declaration solely for checking
                                -- the completed bound's lexical closure.
                                pure TBottom
                              else
                                if
                                    any
                                      ( typeBinderRefsSameIdentity
                                          dependencyRef
                                      )
                                      certifiedOpenParameterRefs
                                  then
                                    -- The recursively checked owner records
                                    -- open value-lambda parameters separately
                                    -- from its emitted forall spine.  That
                                    -- owner-final certificate is the exact
                                    -- constructor of this free identity, so it
                                    -- supplies the unbounded lexical
                                    -- declaration needed while checking the
                                    -- descendant completion.  Do not infer the
                                    -- declaration from the completed type.
                                    pure TBottom
                              else
                                failure
                                  [ "completed enclosing Gamma bound has an unowned dependency"
                                  , "  completed bound: " ++ show completedBound
                                  , "  dependency: " ++ show dependencyRef
                                  ]
                          bounds ->
                            failure
                              [ "completed enclosing Gamma dependency has ambiguous ambient ownership"
                              , "  dependency: " ++ show dependencyRef
                              , "  bounds: " ++ show bounds
                              ]
                    bounds ->
                      failure
                        [ "completed enclosing Gamma dependency has conflicting packet declarations"
                        , "  dependency: " ++ show dependencyRef
                        , "  bounds: " ++ show bounds
                        ]
              let completedDependencyBound =
                    completeLambdaParamBoundaryDeclarationBound
                      parameterBoundaryCertificates
                      dependencyRef
                      dependencyBound
              go
                ( dependencies
                    ++ [(dependencyRef, completedDependencyBound)]
                )
                (freeTypeVarRefsType completedDependencyBound ++ rest)

        distinctBounds rawBounds =
          foldl'
            insertConstructionCompatibleBound
            []
            preferredBounds
          where
            informativeBounds =
              filter
                (not . (`operationalEndpointTypesAgree` TBottom))
                rawBounds
            preferredBounds
              | null informativeBounds = rawBounds
              | otherwise = informativeBounds

            -- A packet spells a declaration underneath its preceding source
            -- forall as the opened bound @a -> a@, while the recursively
            -- checked child records the self-contained ambient declaration
            -- @forall a. a -> a@.  They are one construction only when exact
            -- full-spine inference proves that the closed declaration opens
            -- to the packet presentation.  Prefer that closed authority so
            -- the dependency remains valid after leaving the packet scope;
            -- unrelated bounds remain distinct and fail closed above.
            insertConstructionCompatibleBound bounds incoming
              | any
                  (operationalEndpointTypesAgree incoming)
                  bounds = bounds
              | uniqueEquivalentBounds existingClosures = bounds
              | uniqueEquivalentBounds openedPresentations =
                  incoming
                    : [ existing
                      | existing <- bounds
                      , not
                          ( any
                              (operationalEndpointTypesAgree existing)
                              openedPresentations
                          )
                      ]
              | otherwise = bounds ++ [incoming]
              where
                existingClosures =
                  [ existing
                  | existing <- bounds
                  , completeUnboundedForallSpecializesTo
                      existing
                      incoming
                  ]
                openedPresentations =
                  [ existing
                  | existing <- bounds
                  , completeUnboundedForallSpecializesTo
                      incoming
                      existing
                  ]

            uniqueEquivalentBounds [] = False
            uniqueEquivalentBounds (first : rest) =
              all (operationalEndpointTypesAgree first) rest

    validatePendingRoute label exterior ambientRef schemeInfo = do
      routedRef <-
        case
            IntMap.lookup
              (getNodeId exterior)
              (schemeInfoBinderRefSubst schemeInfo)
          of
            Just ref
              | typeBinderRefsSameIdentity ref ambientRef ->
                  pure ref
            route ->
              failure
                [ label ++ " does not route the exact exterior to ambient Gamma"
                , "  exterior: " ++ show exterior
                , "  route: " ++ show route
                , "  ambient consumer: " ++ show ambientRef
                ]
      case
          [ mbBound
          | (ref, mbBound) <- schemeBinderRefs (siScheme schemeInfo)
          , typeBinderRefsSameIdentity ref routedRef
          ]
        of
          [Nothing] -> pure ()
          declarations ->
            failure
              [ label ++ " does not contain one pending consumer declaration"
              , "  consumer: " ++ show routedRef
              , "  declarations: " ++ show declarations
              ]

    validateTopologyAmbientRoute exterior ambientRef schemeInfo = do
      routedRef <-
        case
            IntMap.lookup
              (getNodeId exterior)
              (schemeInfoBinderRefSubst schemeInfo)
          of
            Just ref
              | typeBinderRefsSameIdentity ref ambientRef ->
                  pure ref
            route ->
              failure
                [ "topology packet does not route its exterior to the selected ambient declaration"
                , "  exterior: " ++ show exterior
                , "  route: " ++ show route
                , "  ambient consumer: " ++ show ambientRef
                ]
      let scheme = siScheme schemeInfo
          declarations =
            [ mbBound
            | (ref, mbBound) <- schemeBinderRefs scheme
            , typeBinderRefsSameIdentity ref routedRef
            ]
          freeOccurrences =
            [ ref
            | ref <- freeTypeVarRefsType (schemeToType scheme)
            , typeBinderRefsSameIdentity ref routedRef
            ]
      unless
        (null declarations && not (null freeOccurrences))
        ( failure
            [ "topology packet does not inherit exactly one ambient consumer route"
            , "  consumer: " ++ show routedRef
            , "  declarations: " ++ show declarations
            , "  free occurrences: " ++ show freeOccurrences
            ]
        )

    subtractAmbientLeadingBinders ty =
      case ty of
        TForallRef ref _ body
          | any
              (typeBinderRefsSameIdentity ref)
              ambientProjectionRefs ->
              subtractAmbientLeadingBinders body
        _ -> ty

    ambientProjectionRefs =
      Map.keys ambientBindings
        ++ inheritedPacketRefs
        ++ packetRoutedCheckedBodyDependencyRefs

    -- These refs are not necessarily installed in the current descendant's
    -- temporary environment, but packet preparation has already certified
    -- them as declarations of enclosing lexical Gamma.  A packet may close
    -- such a dependency in its standalone source view; the local consumer
    -- bound must project that forall back to the inherited declaration before
    -- constructing its own result.
    inheritedPacketRefs =
      Reify.inheritedGammaRoutesLexicalRefs inheritedRoutes
        ++ map
          Reify.inheritedGammaRouteRef
          (Reify.inheritedGammaRoutesEntries inheritedRoutes)
      where
        inheritedRoutes =
          subtermGeneralizationInheritedGammaRoutes packet

    certifiedOpenParameterRefs =
      maybe
        []
        certifiedLambdaBodyParameterRefs
        certifiedBodyConstruction

    -- A descendant may already use a packet binder from enclosing Gamma even
    -- when the legacy inherited-route summary is empty.  Accept that state
    -- only from the full identity-bearing certificate: the ref is free in the
    -- checked body, the packet declares it, and both the operated and consumer
    -- construction maps route its concrete graph node to that same identity.
    packetRoutedCheckedBodyDependencyRefs =
      [ checkedRef
      | checkedRef <- freeTypeVarRefsType checkedBodyType
      , any
          (typeBinderRefsSameIdentity checkedRef . fst)
          packetDeclarations
      , packetRouteMatches
          (subtermGeneralizationOperatedSchemeInfo packet)
          checkedRef
      , packetRouteMatches
          (subtermGeneralizationConsumerConstructionSchemeInfo packet)
          checkedRef
      ]
      where
        packetDeclarations =
          schemeBinderRefs
            (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
            ++ schemeBinderRefs
              ( siScheme
                  (subtermGeneralizationConsumerConstructionSchemeInfo packet)
              )
        packetRouteMatches schemeInfo ref =
          case typeBinderRefNode ref of
            Just node ->
              maybe
                False
                (typeBinderRefsSameIdentity ref)
                ( IntMap.lookup
                    (getNodeId node)
                    (schemeInfoBinderRefSubst schemeInfo)
                )
            Nothing -> False

    failure :: [String] -> Either ElabError a
    failure details =
      Left
        ( ValidationFailed
            ( [ "cannot certify enclosing lambda-body consumer completion"
              , "  body edge: " ++ show bodyEdge
              , "  current owner: " ++ show currentOwner
              ]
                ++ details
            )
        )

-- | Freeze declaration ownership for an already selected body-consumer
-- route.  The declaration occurs exactly once and its actual Gamma bound is
-- retained separately from the operated endpoint.  Usually those types agree;
-- for an unbounded named node they deliberately differ as @a > bottom@ versus
-- @S'(a) = a@.  Local ownership takes precedence because the lambda
-- constructor deliberately shadows a provisional enclosing slot.
authorizeBodyConsumerDeclaration
  :: IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [LambdaParamBoundaryCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Map.Map TypeBinderRef ElabType
  -> BodyConsumerRoute
  -> Either ElabError (Maybe BodyConsumerDeclarationAuthority)
authorizeBodyConsumerDeclaration constructionAliases constructionBinderRenames paramBoundaryCertificates localBinders ambientBindings =
  authorizeBodyConsumerDeclarationWithValidatedLocalRequirements
    []
    constructionAliases
    constructionBinderRenames
    paramBoundaryCertificates
    localBinders
    ambientBindings
    constructionBindings
  where
    constructionBindings =
      foldr insertLocalBinding ambientBindings localBinders

    insertLocalBinding (ref, mbBound) bindings =
      Map.insert
        ref
        (maybe TBottom tyToElab mbBound)
        ( Map.filterWithKey
            (\existingRef _ -> not (typeBinderRefsSameIdentity existingRef ref))
            bindings
        )

-- | Authorize a body-consumer declaration while preserving exact local
-- requirement evidence established by construction-Gamma planning.  The
-- planner has already checked each listed declaration against its required
-- @S(operated)@ bound in the active construction quotient, so consumers must
-- not discard that evidence and attempt a second, less informed comparison.
authorizeBodyConsumerDeclarationWithValidatedLocalRequirements
  :: [TypeBinderRef]
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [LambdaParamBoundaryCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Map.Map TypeBinderRef ElabType
  -> Map.Map TypeBinderRef ElabType
  -> BodyConsumerRoute
  -> Either ElabError (Maybe BodyConsumerDeclarationAuthority)
authorizeBodyConsumerDeclarationWithValidatedLocalRequirements validatedLocalRequirementRefs constructionAliases constructionBinderRenames paramBoundaryCertificates localBinders ambientBindings constructionBindings route =
  case exactLocalBinders of
    [(localRef, _)] -> do
      localBoundAtConstruction <-
        currentConstructionBound localRef
      if
          declarationMatchesOperated localBoundAtConstruction
            || routeOperatesOnOwnDeclaration
            || localRequirementAlreadyValidated localRef
        then
          pure
            ( Just
                ( BodyConsumerLocallyEmitted
                    route
                    localBoundAtConstruction
                )
            )
        else
          authorityFailure
            [ "local declaration disagrees with the route requirement"
            , "  local ref: " ++ show localRef
            , "  construction bound: "
                ++ show localBoundAtConstruction
            , "  route operated type: "
                ++ show (bcrConstructionOperatedType route)
            ]
    [] ->
      case exactAmbientBindings of
        [] -> pure Nothing
        [(ambientRef, _)] -> do
          ambientBoundAtConstruction <-
            currentConstructionBound ambientRef
          pure
            ( Just
                ( BodyConsumerInheritedAmbient
                    route
                    ambientBoundAtConstruction
                )
            )
        matches ->
          authorityFailure
            [ "ambient Gamma contains multiple exact construction declarations"
            , "  matches: " ++ show matches
            ]
    matches ->
      authorityFailure
        [ "local Gamma contains multiple exact construction declarations"
        , "  matches: " ++ show matches
        ]
  where
    exactLocalBinders =
      [ binder
      | binder@(ref, _) <- localBinders
      , typeBinderRefsSameIdentity ref (bcrConstructionRef route)
      ]

    exactAmbientBindings =
      [ binding
      | binding@(ref, _) <- Map.toList ambientBindings
      , typeBinderRefsSameIdentity ref (bcrConstructionRef route)
      ]

    currentConstructionBound ref =
      case
          [ (currentRef, currentBound)
          | (currentRef, currentBound) <- Map.toList constructionBindings
          , typeBinderRefsSameIdentity currentRef ref
          ]
        of
          [(currentRef, currentBound)] ->
            pure (projectDeclarationBound currentRef currentBound)
          [] ->
            authorityFailure
              [ "owned declaration is absent from the current construction Gamma"
              , "  declaration ref: " ++ show ref
              ]
          matches ->
            authorityFailure
              [ "owned declaration occurs more than once in the current construction Gamma"
              , "  declaration ref: " ++ show ref
              , "  matches: " ++ show matches
              ]

    declarationMatchesOperated declarationBound =
      operationalEndpointTypesAgree
        declarationBound
        (bcrConstructionOperatedType route)
        || completeUnboundedForallSpecializesTo
          declarationBound
          (bcrConstructionOperatedType route)

    -- The route's operated endpoint has already entered the exact
    -- construction quotient.  Compare a declaration bound in that same
    -- domain, using only graph-node aliases carried by the active
    -- construction Gamma; never infer a rename from matching type shape.
    projectDeclarationBound declarationRef declarationBound =
      completeLambdaParamBoundaryDeclarationBound
        paramBoundaryCertificates
        declarationRef
        ( foldr projectFreeRef declarationAtConstruction
            (freeTypeVarRefsType declarationAtConstruction)
        )
      where
        declarationAtConstruction =
          renameTypeBinderRefPayloads
            constructionBinderRenames
            declarationBound

    projectFreeRef ref ty =
      case
          typeBinderRefNode ref
            >>= \node -> IntMap.lookup (getNodeId node) constructionAliases
        of
        Just constructionRef
          | any
              (typeBinderRefsSameIdentity constructionRef)
              ( freeTypeVarRefsType
                  (bcrConstructionOperatedType route)
              ) ->
          substTypeCaptureRef ref (TVarRef constructionRef) ty
        Nothing -> ty
        _ -> ty

    routeOperatesOnOwnDeclaration =
      case bcrConstructionOperatedType route of
        TVarRef operatedRef ->
          typeBinderRefsSameIdentity
            operatedRef
            (bcrConstructionRef route)
        _ -> False

    localRequirementAlreadyValidated localRef =
      any
        (typeBinderRefsSameIdentity localRef)
        validatedLocalRequirementRefs

    authorityFailure :: [String] -> Either ElabError a
    authorityFailure details =
      Left
        ( ValidationFailed
            ( [ "cannot authorize lambda-body consumer declaration"
              , "  route: " ++ show route
              ]
                ++ details
            )
        )

authorizedBodyConsumerRoute
  :: BodyConsumerDeclarationAuthority
  -> BodyConsumerRoute
authorizedBodyConsumerRoute authority =
  case authority of
    BodyConsumerLocallyEmitted route _ -> route
    BodyConsumerInheritedAmbient route _ -> route
    BodyConsumerEnclosingAmbient route _ -> route
    BodyConsumerPendingOwnerEmission route _ -> route
    BodyConsumerOrdinaryOwnerEmission route _ -> route
    BodyConsumerConsumedAtOwner route _ _ -> route

-- | Exact route sealed inside a refinement certificate.  Keep the
-- declaration-authority constructor private while allowing consumers to join
-- a finalized transition to its owner and construction identity.
bodyConsumerBoundRefinementRoute
  :: BodyConsumerBoundRefinementCertificate
  -> BodyConsumerRoute
bodyConsumerBoundRefinementRoute =
  authorizedBodyConsumerRoute . bcbrDeclarationAuthority

authorizedBodyConsumerDeclarationBound
  :: BodyConsumerDeclarationAuthority
  -> ElabType
authorizedBodyConsumerDeclarationBound authority =
  case authority of
    BodyConsumerLocallyEmitted _ declarationBound ->
      declarationBound
    BodyConsumerInheritedAmbient _ declarationBound ->
      declarationBound
    BodyConsumerEnclosingAmbient _ declarationBound ->
      declarationBound
    BodyConsumerPendingOwnerEmission _ _ ->
      TBottom
    BodyConsumerOrdinaryOwnerEmission _ _ ->
      TBottom
    BodyConsumerConsumedAtOwner _ completedBound _ ->
      completedBound

-- | Finish construction of a body-consumer projection after the caller has
-- validated the route against its packet and Gamma.  The checked child may
-- still name an exact source-sidecar identity while the route's operated
-- endpoint has entered the construction Gamma.  Join those two
-- identity-bearing maps at the concrete graph node before comparing the
-- endpoints; a same-shaped peer cannot manufacture this certificate.
mkValidatedBodyConsumerProjection
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerRoute
  -> ElabType
  -> ElabType
  -> Either ElabError ValidatedBodyConsumerProjection
mkValidatedBodyConsumerProjection sourceBinderRefs constructionAliases constructionBinderRenames route checkedSource projectedType = do
  sourceConstructionRenames <-
    exactSourceConstructionRenames
  let checkedSourceAtConstruction =
        renameTypeBinderRefPayloads
          sourceConstructionRenames
          checkedSource
  ( exactProjectedType
    , sourceProjectionRenames
    , sourceProjectionInstantiation
    ) <-
    if operationalEndpointTypesAgree checkedSourceAtConstruction projectedType
      then pure (projectedType, [], InstId)
      else
        case
            planExactBinderSpine
              operationalEndpointTypesAgree
              checkedSourceAtConstruction
              projectedType
          of
            Just plan
              | exactBinderSpineInstantiation plan /= InstId ->
                  pure
                    ( projectedType
                    , exactBinderSpineRenames plan
                    , exactBinderSpineInstantiation plan
                    )
            _ ->
              case
                  planExactBinderSpine
                    operationalEndpointTypesAgree
                    projectedType
                    checkedSourceAtConstruction
                of
                  Just plan
                    | exactBinderSpineInstantiation plan /= InstId ->
                        -- The packet is a more general provisional view of
                        -- the already checked child.  Keep the checked source
                        -- as the exact endpoint; no computation is applied to
                        -- the child term in this direction.
                        pure (checkedSourceAtConstruction, [], InstId)
                  _ ->
                    projectionFailure
                      sourceConstructionRenames
                      checkedSourceAtConstruction
  pure
    ValidatedBodyConsumerProjection
      { vbcpRoute = route,
        vbcpSourceConstructionRenames =
          sourceConstructionRenames ++ sourceProjectionRenames,
        vbcpProjectedType = exactProjectedType,
        vbcpSourceProjectionInstantiation =
          sourceProjectionInstantiation
      }
  where
    projectionFailure
      :: [(TypeBinderRef, TypeBinderRef)]
      -> ElabType
      -> Either ElabError a
    projectionFailure sourceRenames endpoint =
      Left
        ( ValidationFailed
            [ "validated lambda-body consumer projection disagrees with its checked source"
            , "  route: " ++ show route
            , "  checked source: " ++ show checkedSource
            , "  source-to-construction renames: "
                ++ show sourceRenames
            , "  checked source at construction: "
                ++ show endpoint
            , "  projected operated type: " ++ show projectedType
            ]
        )

    checkedSourceRefs =
      freeTypeVarRefsType checkedSource

    projectedTypeRefs =
      freeTypeVarRefsType projectedType

    operatedGraphRefs =
      [ (getNodeId node, ref)
      | ref <- freeTypeVarRefsType (bcrOperatedType route)
      , Just node <- [typeBinderRefNode ref]
      ]

    exactSourceConstructionRenames =
      foldM insertExactRename [] candidateRenames

    candidateRenames =
      [ rename
      | rename@(sourceRef, constructionRef) <-
          constructionBinderRenames
      , any (typeBinderRefsSameIdentity sourceRef) checkedSourceRefs
      , any
          (typeBinderRefsSameIdentity constructionRef)
          projectedTypeRefs
      ]
        ++ [ (checkedRef, constructionRef)
           | checkedRef <- checkedSourceRefs
           , Just checkedNode <- [typeBinderRefGraphOrigin checkedRef]
           , Just constructionRef <-
               [ IntMap.lookup
                   (getNodeId checkedNode)
                   constructionAliases
               ]
           , any
               (typeBinderRefsSameIdentity constructionRef)
               projectedTypeRefs
           , not
               (typeBinderRefsSameIdentity checkedRef constructionRef)
           ]
        ++ [ (sourceRef, constructionRef)
      | (nodeKey, operatedRef) <- operatedGraphRefs
      , Just sourceRef <- [IntMap.lookup nodeKey sourceBinderRefs]
      , any (typeBinderRefsSameIdentity sourceRef) checkedSourceRefs
      , Just constructionRef <-
          [ constructionRefForOperatedOccurrence
              nodeKey
              operatedRef
          ]
      , any
          (typeBinderRefsSameIdentity constructionRef)
          projectedTypeRefs
      , not
          (typeBinderRefsSameIdentity sourceRef constructionRef)
      ]

    -- Structured operated endpoints use aliases already published by the
    -- active construction Gamma.  The packet selector may add one route only
    -- for a bare operated occurrence; that exact route is retained directly
    -- in 'BodyConsumerRoute'.
    constructionRefForOperatedOccurrence nodeKey operatedRef =
      IntMap.lookup nodeKey constructionAliases
        <|> case
            ( bcrOperatedType route
            , bcrConstructionOperatedType route
            )
          of
            (TVarRef routeOperatedRef, TVarRef routeConstructionRef)
              | typeBinderRefsSameIdentity routeOperatedRef operatedRef
              , typeBinderRefsSameIdentity
                  routeConstructionRef
                  (bcrConstructionRef route) ->
                  Just routeConstructionRef
            _ -> Nothing

    insertExactRename renames rename@(sourceRef, constructionRef) =
      case
          find
            (typeBinderRefsSameIdentity sourceRef . fst)
            renames
        of
        Nothing -> pure (renames ++ [rename])
        Just (_, existingConstructionRef)
          | typeBinderRefsSameIdentity
              existingConstructionRef
              constructionRef ->
              pure renames
        Just (_, existingConstructionRef) ->
          Left
            ( ValidationFailed
                [ "lambda-body source identity has conflicting construction routes"
                , "  route: " ++ show route
                , "  source ref: " ++ show sourceRef
                , "  first construction ref: "
                    ++ show existingConstructionRef
                , "  conflicting construction ref: "
                    ++ show constructionRef
                ]
            )

-- | The exact source-sidecar to construction-Gamma quotient that was checked
-- while building the projection.  Consumers use this route to move the term,
-- its checked source type, and its resolved lookup environment together
-- before applying the outgoing Phi computation.
validatedBodyConsumerProjectionSourceConstructionRenames
  :: ValidatedBodyConsumerProjection
  -> [(TypeBinderRef, TypeBinderRef)]
validatedBodyConsumerProjectionSourceConstructionRenames =
  vbcpSourceConstructionRenames

-- | The exact operated endpoint certified for the checked child.  A prepared
-- packet may be more general when its own binder spine is consumed by a
-- recorded 'InstUnder'/N computation; callers must install this endpoint,
-- not the pre-specialization packet presentation.
validatedBodyConsumerProjectionType
  :: ValidatedBodyConsumerProjection
  -> ElabType
validatedBodyConsumerProjectionType =
  vbcpProjectedType

-- | Return the construction recorded when the checked child was projected to
-- the selected consumer endpoint.  Both accepted references are exact
-- endpoints of the validated route; names, representatives, and type shape
-- are not consulted.
validatedBodyConsumerProjectionSpecialization
  :: TypeBinderRef
  -> ElabType
  -> ValidatedBodyConsumerProjection
  -> Maybe Instantiation
validatedBodyConsumerProjectionSpecialization resultRef publishedType projection
  | any
      (typeBinderRefsSameIdentity resultRef)
      [ bcrSemanticRef route,
        bcrConstructionRef route
      ],
    operationalEndpointTypesAgree
      publishedType
      (vbcpProjectedType projection)
      || projectedIsExactResult =
      Just (vbcpSourceProjectionInstantiation projection)
  | otherwise = Nothing
  where
    route = vbcpRoute projection
    projectedIsExactResult =
      case vbcpProjectedType projection of
        TVarRef projectedRef ->
          any
            (typeBinderRefsSameIdentity projectedRef)
            [ bcrSemanticRef route,
              bcrConstructionRef route
            ]
        _ -> False

-- | Select the leading N elimination for a lambda whose exact body consumer
-- has already been projected through 'Gen(Gamma, tau)'.  Before the final
-- construction quotient, the emitted flexible bound is intentionally still
-- source-oriented while the enclosing endpoint is construction-oriented.
-- Generic type-argument inference would interpret that identity difference as
-- an application and eventually manufacture 'InstBot'.  The projection
-- certificate instead proves that this exact result binder denotes the
-- checked source endpoint, while the construction-operated endpoint proves
-- what the same body becomes under the already validated quotient.
--
-- Keep this deliberately production-shaped: one exact result abstraction
-- around the lambda whose codomain is that abstraction.  Names,
-- representatives, and merely equal-shaped peer identities cannot select the
-- elimination.
validatedBodyConsumerLeadingElimination
  :: ElabType
  -> ElabType
  -> ElabType
  -> ValidatedBodyConsumerProjection
  -> Maybe Instantiation
validatedBodyConsumerLeadingElimination
  constructionOperatedType
  constructedLambdaType
  expectedLambdaType
  projection = do
  (resultRef, projectedBound, constructedParam, bodyResultRef) <-
    case constructedLambdaType of
      TForallRef ref (Just bound) (TArrow param (TVarRef result)) ->
        Just (ref, bound, param, result)
      _ -> Nothing
  (expectedParam, expectedBody) <-
    case expectedLambdaType of
      TArrow param body -> Just (param, body)
      _ -> Nothing
  guard (projectionOwnsRef resultRef)
  guard (typeBinderRefsSameIdentity resultRef bodyResultRef)
  guard
    ( operationalEndpointTypesAgree
        (tyToElab projectedBound)
        (vbcpProjectedType projection)
    )
  guard (operationalEndpointTypesAgree constructedParam expectedParam)
  guard
    ( operationalEndpointTypesAgree
        constructionOperatedType
        expectedBody
    )
  pure InstElim
  where
    route = vbcpRoute projection
    projectionOwnsRef ref =
      any
        (typeBinderRefsSameIdentity ref)
        [ bcrSemanticRef route,
          bcrConstructionRef route
        ]

-- | Install the checked body endpoint for one consumer whose complete direct
-- route has already been validated by the owning lambda construction.  This
-- is the final mechanical step of that proof: only the exact consumer
-- identity may be refined.  An established ambient declaration must still
-- carry the bound recorded when the route was selected; a different bound is
-- admitted only when the frozen local-Gamma closure classifies this exact slot
-- as a provisional nested result.  Names, solved representatives, and
-- equal-shaped peer types are deliberately not considered.
--
-- An absent identity means that the consumer is local to the current
-- construction and therefore needs no ambient projection.  More than one
-- binding for the same identity is an invariant violation rather than a
-- precedence choice.
projectValidatedAmbientConsumerBound
  :: DirectAmbientGammaAuthorityProvenance
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
projectValidatedAmbientConsumerBound provenance consumerRef declaredBound projectedBound bindings =
  fst
    <$> projectValidatedAmbientConsumerBoundDetailed
      provenance
      consumerRef
      declaredBound
      projectedBound
      bindings

-- | Project an already validated body endpoint and preserve the exact proof
-- needed by post-elaboration root planning.  Merely updating the local
-- elaboration environment is insufficient: the root closure is rebuilt from
-- its provisional graph plan after the lambda has been checked.  Returning
-- the certificate in the same successful branch that performs the update
-- keeps those two construction surfaces in lockstep.
projectValidatedAmbientConsumerBoundWithCertificate
  :: DirectAmbientGammaAuthorityProvenance
  -> BodyConsumerDeclarationAuthority
  -> ValidatedBodyConsumerProjection
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( Map.Map TypeBinderRef ElabType,
        Maybe BodyConsumerBoundRefinementCertificate
      )
projectValidatedAmbientConsumerBoundWithCertificate provenance declarationAuthority projection bindings = do
  unless
    (authorizedBodyConsumerRoute declarationAuthority == vbcpRoute projection)
    ( certificateFailure
        "declaration authority and validated projection select different routes"
    )
  when
    (provenance == DirectAmbientProvisionalNestedResult)
    ( unless
        ( operationalEndpointTypesAgree
            (bcrConstructionOperatedType route)
            (vbcpProjectedType projection)
        )
        ( certificateFailure
            "validated projection does not complete the selected operated endpoint"
        )
    )
  (projectedBindings, mbPreviousBinding) <-
    projectValidatedAmbientConsumerBoundDetailed
      provenance
      (bcrConstructionRef route)
      (authorizedBodyConsumerDeclarationBound declarationAuthority)
      (vbcpProjectedType projection)
      bindings
  pure
    ( projectedBindings,
      case provenance of
        DirectAmbientEstablished -> Nothing
        DirectAmbientProvisionalNestedResult ->
          case mbPreviousBinding of
            Just (ambientRef, previousBound) ->
              case declarationAuthority of
                BodyConsumerLocallyEmitted {} ->
                  -- Preserve the actual state transition observed at the
                  -- identity-coincident provisional slot.  The local
                  -- declaration supplies ownership, but its completed bound
                  -- must not overwrite the previous ambient state recorded
                  -- for root replay.
                  Just (certificate ambientRef previousBound)
                _
                  | operationalEndpointTypesAgree
                      previousBound
                      (vbcpProjectedType projection) ->
                      Nothing
                  | otherwise ->
                      Just (certificate ambientRef previousBound)
            Nothing ->
              case declarationAuthority of
                BodyConsumerLocallyEmitted _ localBound ->
                  -- The local constructor may already have materialized the
                  -- completed bound in its own candidate spine.  Root
                  -- planning still starts from the graph scheme, so publish
                  -- its exact identity-selected completion even when no
                  -- ambient map entry was present at this boundary.
                  Just
                    ( certificate
                        (bcrConstructionRef route)
                        localBound
                    )
                _ -> Nothing
    )
  where
    route = authorizedBodyConsumerRoute declarationAuthority
    certificate ambientRef previousBound =
      pendingBodyConsumerBoundRefinementCertificate
        declarationAuthority
        ambientRef
        previousBound
        (vbcpProjectedType projection)

    certificateFailure detail =
      Left
        ( ValidationFailed
            [ "cannot certify lambda-body consumer bound refinement",
              "  detail: " ++ detail,
              "  declaration route: " ++ show route,
              "  validated projection: " ++ show projection,
              "  provenance: " ++ show provenance
            ]
        )

projectValidatedAmbientConsumerBoundDetailed
  :: DirectAmbientGammaAuthorityProvenance
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( Map.Map TypeBinderRef ElabType,
        Maybe (TypeBinderRef, ElabType)
      )
projectValidatedAmbientConsumerBoundDetailed provenance consumerRef declaredBound projectedBound bindings =
  case matchingBindings of
    [] -> pure (bindings, Nothing)
    [(ambientRef, ambientBound)]
      | operationalEndpointTypesAgree ambientBound declaredBound
          || provenance == DirectAmbientProvisionalNestedResult ->
          pure
            ( Map.insert
                ambientRef
                projectedBound
                ( Map.filterWithKey
                    ( \existingRef _ ->
                        not
                          (typeBinderRefsSameIdentity existingRef ambientRef)
                    )
                    bindings
                ),
              Just (ambientRef, ambientBound)
            )
      | otherwise ->
          projectionFailure
            [ "ambient declaration disagrees with the validated consumer route"
            , "  ambient ref: " ++ show ambientRef
            , "  ambient bound: " ++ show ambientBound
            ]
    matches ->
      projectionFailure
        [ "ambient Gamma contains multiple bindings for one consumer identity"
        , "  matches: " ++ show matches
        ]
  where
    matchingBindings =
      [ binding
      | binding@(ref, _) <- Map.toList bindings
      , typeBinderRefsSameIdentity ref consumerRef
      ]

    projectionFailure details =
      Left
        ( ValidationFailed
            ( [ "cannot project validated lambda-body consumer bound into ambient Gamma"
              , "  consumer: " ++ show consumerRef
              , "  provenance: " ++ show provenance
              , "  declared bound: " ++ show declaredBound
              , "  projected bound: " ++ show projectedBound
              ]
                ++ details
            )
        )

-- | Advance an already prepared exact construction Gamma through the
-- finalized declarations consumed by its checked body.  The certificates,
-- rather than the body's final type, identify both the declarations removed
-- from Gamma and the completed bounds substituted into later declarations
-- and the construction body.
--
-- This is the intermediate-construction counterpart of
-- 'projectCertifiedBodyConsumerRootScheme'.  Non-consumed refinements belong
-- to the ordinary environment/root projection paths and are deliberately
-- ignored here.
consumeCertifiedBodyConsumerConstructionScheme
  :: [BodyConsumerBoundRefinementCertificate]
  -> ElabScheme
  -> Either ElabError ElabScheme
consumeCertifiedBodyConsumerConstructionScheme certificates scheme =
  projectCertifiedBodyConsumerScheme
    False
    Set.empty
    []
    []
    []
    consumedCertificates
    scheme
  where
    constructionRefs = map fst (schemeBinderRefs scheme)
    consumedCertificates =
      filter
        (bodyConsumerBoundRefinementConsumesAny constructionRefs)
        certificates

-- | Advance a frozen endpoint through declarations already consumed by the
-- exact checked owner that produced it.  Unlike an intermediate construction
-- Gamma, an owner-final endpoint can retain the consumed identity only as a
-- free occurrence: the local binder has disappeared at the owner boundary.
-- The caller must therefore supply the refinements carried by that same
-- 'OwnerFinalConstruction'; this function selects only finalized consumed
-- identities actually present in the endpoint and lets the common certified
-- projector perform the substitution.
consumeCertifiedBodyConsumerEndpointScheme
  :: [BodyConsumerBoundRefinementCertificate]
  -> ElabScheme
  -> Either ElabError ElabScheme
consumeCertifiedBodyConsumerEndpointScheme certificates scheme =
  projectCertifiedBodyConsumerScheme
    False
    Set.empty
    []
    []
    []
    consumedEndpointCertificates
    scheme
  where
    endpointTy = schemeToType scheme
    endpointRefs =
      typeBinderDeclarationRefs endpointTy
        ++ freeTypeVarRefsType endpointTy
    consumedEndpointCertificates =
      filter
        ( \certificate ->
            bodyConsumerBoundRefinementOwnerFinalized certificate
              && bodyConsumerBoundRefinementConsumed certificate
              && bodyConsumerBoundRefinementTargetsAny
                endpointRefs
                certificate
        )
        certificates

-- | Map form of 'consumeCertifiedBodyConsumerConstructionScheme' for an
-- enclosing construction environment.  The conversion is lossless for
-- Gamma: @TBottom@ is its unbounded declaration, and every other entry must
-- be a legal flexible bound.  This lets owner-final inheritance advance the
-- parent's frozen Gamma before comparing the child's completed ambient
-- authorities.
consumeCertifiedBodyConsumerConstructionBindings
  :: [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
consumeCertifiedBodyConsumerConstructionBindings certificates bindings = do
  targetBinders <-
    constructionConsumptionTargetBinders certificates bindings
  projectedBindings <-
    traverse
      (projectRetainedBinding targetBinders)
      [ binding
      | binding@(ref, _) <- Map.toList bindings
      , not
          ( any
              (typeBinderRefsSameIdentity ref . fst)
              targetBinders
          )
      ]
  pure (Map.fromList projectedBindings)
  where
    projectRetainedBinding targetBinders (ref, bound) = do
      projectedBound <-
        consumeCertifiedBodyConsumerConstructionTypeWithBinders
          certificates
          targetBinders
          bound
      pure (ref, projectedBound)

-- | Advance the retained declarations of a construction environment to the
-- exact bounds already selected by a completed construction scheme.  This is
-- deliberately narrower than descendant-refinement installation: a child
-- owner may already have finalized its local declaration, but an inherited
-- administrative construction can still carry that same declaration in its
-- certified Gamma spine.  In that case the scheme itself is the authority to
-- observe the completed state at this boundary; no ownership is inferred from
-- the ambient map and no unrelated certificate is replayed.
advanceCertifiedBodyConsumerConstructionBindingsToScheme
  :: [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
advanceCertifiedBodyConsumerConstructionBindingsToScheme certificates schemeBinders initialBindings =
  foldM advance initialBindings retainedCertificates
  where
    retainedCertificates =
      filter (not . bodyConsumerBoundRefinementConsumed) certificates

    advance bindings certificate =
      case matchingSchemeBinders certificate of
        [] -> pure bindings
        [(schemeRef, mbSchemeBound)] -> do
          let schemeBound = maybe TBottom tyToElab mbSchemeBound
          if
              not
                ( bodyConsumerBoundRefinementAcceptsDeclarationState
                    certificate
                    schemeBound
                )
            then
              -- One graph identity can have independent descendant
              -- refinements after different exact specializations.  The
              -- scheme's identity alone does not select this certificate;
              -- an unrelated bound denotes the other specialization and is
              -- left untouched.  A previous/completed/authority-owned state
              -- below still has to complete exactly.
              pure bindings
            else do
              unless
                ( operationalEndpointTypesAgree
                    schemeBound
                    (bcbrCompletedBound certificate)
                )
                ( transitionFailure
                    certificate
                    [ "the completed scheme retained a different bound"
                    , "  scheme declaration: "
                        ++ show (schemeRef, mbSchemeBound)
                    ]
                )
              case matchingBindings certificate bindings of
                [] -> pure bindings
                [(existingRef, currentBound)]
                  | bodyConsumerBoundRefinementAcceptsDeclarationState
                      certificate
                      currentBound ->
                      pure
                        ( Map.insert
                            existingRef
                            schemeBound
                            ( Map.filterWithKey
                                ( \ref _ ->
                                    not
                                      ( typeBinderRefsSameIdentity
                                          ref
                                          existingRef
                                      )
                                )
                                bindings
                            )
                        )
                  | otherwise ->
                      transitionFailure
                        certificate
                        [ "the construction Gamma is not at a certified declaration state"
                        , "  current declaration: "
                            ++ show (existingRef, currentBound)
                        ]
                matches ->
                  transitionFailure
                    certificate
                    [ "the construction Gamma contains duplicate declaration identities"
                    , "  matching declarations: " ++ show matches
                    ]
        matches ->
          transitionFailure
            certificate
            [ "the completed scheme contains duplicate declaration identities"
            , "  matching declarations: " ++ show matches
            ]

    matchingSchemeBinders certificate =
      filter
        ( typeBinderRefsSameIdentity
            (bcbrAmbientRef certificate)
            . fst
        )
        schemeBinders

    matchingBindings certificate bindings =
      filter
        ( typeBinderRefsSameIdentity
            (bcbrAmbientRef certificate)
            . fst
        )
        (Map.toList bindings)

    transitionFailure
      :: BodyConsumerBoundRefinementCertificate
      -> [String]
      -> Either ElabError a
    transitionFailure certificate details =
      Left
        ( ValidationFailed
            ( [ "cannot advance construction Gamma to its certified scheme"
              , "  certificate: " ++ show certificate
              , "  completed scheme binders: " ++ show schemeBinders
              ]
                ++ details
            )
        )

-- | Advance a type payload owned by an already prepared construction Gamma
-- through the same finalized declaration transition as that Gamma.  Keeping
-- the declarations and payload in one scheme ensures a consumed declaration
-- is substituted from its certificate before leaving scope.
consumeCertifiedBodyConsumerConstructionType
  :: [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> ElabType
  -> Either ElabError ElabType
consumeCertifiedBodyConsumerConstructionType certificates bindings ty = do
  targetBinders <-
    constructionConsumptionTargetBinders certificates bindings
  consumeCertifiedBodyConsumerConstructionTypeWithBinders
    certificates
    targetBinders
    ty

consumeCertifiedBodyConsumerConstructionTypeWithBinders
  :: [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> Either ElabError ElabType
consumeCertifiedBodyConsumerConstructionTypeWithBinders
  _
  []
  ty = pure ty
consumeCertifiedBodyConsumerConstructionTypeWithBinders
  certificates
  targetBinders
  ty =
    schemeBody
      <$> consumeCertifiedBodyConsumerConstructionScheme
        certificates
        (mkElabSchemeWithRefs targetBinders ty)

constructionConsumptionTargetBinders
  :: [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
constructionConsumptionTargetBinders certificates bindings = do
  targetBinders <- traverse selectTargetBinding targetRefs
  schemeBinderRefs
    <$> either
      (Left . ValidationFailed . pure)
      Right
      ( orderSourceProjectedSchemeBinders
          "body-consumer construction transition"
          (mkElabSchemeWithRefs targetBinders TBottom)
      )
  where
    targetRefs =
      foldr insertDistinctRef []
        [ ref
        | ref <- Map.keys bindings
        , any
            (bodyConsumerBoundRefinementConsumesAny [ref])
            certificates
        ]

    insertDistinctRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    selectTargetBinding targetRef =
      case
          [ binding
          | binding@(ref, _) <- Map.toList bindings
          , typeBinderRefsSameIdentity ref targetRef
          ]
        of
          [binding] -> constructionBindingToBinder binding
          matches ->
            Left
              ( ValidationFailed
                  [ "consumed construction identity has no unique Gamma declaration"
                  , "  target: " ++ show targetRef
                  , "  matches: " ++ show matches
                  ]
              )

constructionBindingToBinder
  :: (TypeBinderRef, ElabType)
  -> Either ElabError (TypeBinderRef, Maybe BoundType)
constructionBindingToBinder (ref, TBottom) = pure (ref, Nothing)
constructionBindingToBinder (ref, bound) =
  case elabToBound bound of
    Right boundTy -> pure (ref, Just boundTy)
    Left cause ->
      Left
        ( ValidationFailed
            [ "construction environment contains an illegal Gamma bound"
            , "  declaration: " ++ show ref
            , "  bound: " ++ show bound
            , "  cause: " ++ cause
            ]
        )

-- | Substitute one certified completed declaration into a construction
-- scheme while allocating a distinct lexical copy of the completion at each
-- occurrence.  A completed bound can itself declare foralls.  Reusing that
-- same closed type at two sibling occurrences would therefore emit the same
-- declaration identity twice, even though the two occurrences are distinct
-- lexical scopes.
--
-- The checked scheme body is visited first so its presentation remains the
-- authority used by the already constructed body computation.  Later sibling
-- bounds receive copies when they encounter identities already owned by that
-- body or by an earlier sibling.  Only declarations carried by the exact
-- certificate replacement are copied; this is not a general duplicate-type
-- normalizer.
substituteCertifiedCompletionWithLexicalCopies
  :: TypeBinderRef
  -> ElabType
  -> ElabScheme
  -> (ElabScheme, [(TypeBinderRef, TypeBinderRef)])
substituteCertifiedCompletionWithLexicalCopies targetRef replacement scheme =
  ( mkElabSchemeWithRefs substitutedBinders substitutedBody
  , finalCopies
  )
  where
    initialState =
      ( typeBinderDeclarationRefs (schemeToType scheme)
      , identityGeneratorAfterType
          (TArrow replacement (schemeToType scheme))
      , []
      )
    (substitutedBody, bodyState) =
      substituteType initialState (schemeBody scheme)
    (substitutedBinders, (_, _, finalCopies)) =
      substituteBinders
        bodyState
        [ binder
        | binder@(ref, _) <- schemeBinderRefs scheme
        , not (typeBinderRefsSameIdentity ref targetRef)
        ]
    replacementFreeRefs = freeTypeVarRefsType replacement

    targetOccursFreeIn ty =
      any
        (typeBinderRefsSameIdentity targetRef)
        (freeTypeVarRefsType ty)

    replacementMentionsRef ref =
      any (typeBinderRefsSameIdentity ref) replacementFreeRefs

    substituteBinders state [] = ([], state)
    substituteBinders state ((ref, mbBound) : binders) =
      let (mbBound', state') = substituteOptionalBound state mbBound
          (binders', state'') = substituteBinders state' binders
       in ((ref, mbBound') : binders', state'')

    substituteOptionalBound state Nothing = (Nothing, state)
    substituteOptionalBound state (Just bound) =
      let (bound', state') = substituteBound state bound
       in (Just bound', state')

    substituteBound state bound =
      case bound of
        TArrow domain codomain ->
          let (domain', state') = substituteType state domain
              (codomain', state'') = substituteType state' codomain
           in (TArrow domain' codomain', state'')
        TConWithIdentity identity constructor arguments ->
          let (arguments', state') =
                substituteNonEmpty state arguments
           in (TConWithIdentity identity constructor arguments', state')
        TVarAppRef ref arguments ->
          let (arguments', state') =
                substituteNonEmpty state arguments
           in if typeBinderRefsSameIdentity ref targetRef
                then
                  let (replacement', state'') =
                        copyReplacement state'
                   in ( composeTypeHeadRef ref replacement' arguments'
                      , state''
                      )
                else (TVarAppRef ref arguments', state')
        TBaseWithIdentity identity base ->
          (TBaseWithIdentity identity base, state)
        TForallRef ref mbNestedBound body ->
          let (mbNestedBound', state') =
                substituteOptionalBound state mbNestedBound
           in if typeBinderRefsSameIdentity ref targetRef
                then (TForallRef ref mbNestedBound' body, state')
                else
                  let (bodyRef, bodySource, state'') =
                        prepareBodyScope ref body state'
                      (body', state''') =
                        substituteType state'' bodySource
                   in (TForallRef bodyRef mbNestedBound' body', state''')
        TMuRef ref body
          | typeBinderRefsSameIdentity ref targetRef ->
              (TMuRef ref body, state)
          | otherwise ->
              let (bodyRef, bodySource, state') =
                    prepareBodyScope ref body state
                  (body', state'') = substituteType state' bodySource
               in (TMuRef bodyRef body', state'')
        TBottom -> (TBottom, state)

    substituteType state ty =
      case ty of
        TVarRef ref
          | typeBinderRefsSameIdentity ref targetRef ->
              copyReplacement state
          | otherwise -> (TVarRef ref, state)
        TArrow domain codomain ->
          let (domain', state') = substituteType state domain
              (codomain', state'') = substituteType state' codomain
           in (TArrow domain' codomain', state'')
        TConWithIdentity identity constructor arguments ->
          let (arguments', state') =
                substituteNonEmpty state arguments
           in (TConWithIdentity identity constructor arguments', state')
        TVarAppRef ref arguments ->
          let (arguments', state') =
                substituteNonEmpty state arguments
           in if typeBinderRefsSameIdentity ref targetRef
                then
                  let (replacement', state'') =
                        copyReplacement state'
                   in ( composeTypeHeadRef ref replacement' arguments'
                      , state''
                      )
                else (TVarAppRef ref arguments', state')
        TBaseWithIdentity identity base ->
          (TBaseWithIdentity identity base, state)
        TForallRef ref mbBound body ->
          let (mbBound', state') = substituteOptionalBound state mbBound
           in if typeBinderRefsSameIdentity ref targetRef
                then (TForallRef ref mbBound' body, state')
                else
                  let (bodyRef, bodySource, state'') =
                        prepareBodyScope ref body state'
                      (body', state''') =
                        substituteType state'' bodySource
                   in (TForallRef bodyRef mbBound' body', state''')
        TMuRef ref body
          | typeBinderRefsSameIdentity ref targetRef ->
              (TMuRef ref body, state)
          | otherwise ->
              let (bodyRef, bodySource, state') =
                    prepareBodyScope ref body state
                  (body', state'') = substituteType state' bodySource
               in (TMuRef bodyRef body', state'')
        TBottom -> (TBottom, state)

    substituteList state [] = ([], state)
    substituteList state (ty : types) =
      let (ty', state') = substituteType state ty
          (types', state'') = substituteList state' types
       in (ty' : types', state'')

    substituteNonEmpty state (firstTy NonEmpty.:| remainingTys) =
      let (firstTy', state') = substituteType state firstTy
          (remainingTys', state'') = substituteList state' remainingTys
       in (firstTy' NonEmpty.:| remainingTys', state'')

    copyReplacement state =
      freshenTypeDeclarationScopesInType [] state replacement

    prepareBodyScope ref body state
      | replacementMentionsRef ref
          && targetOccursFreeIn body =
          let (copiedRef, state') = copyCapturingBinder ref state
              renamedBody =
                substTypeCaptureRef ref (TVarRef copiedRef) body
           in (copiedRef, renamedBody, state')
      | otherwise = (ref, body, state)

    copyCapturingBinder
      ref
      (reservedRefs, generator, copies) =
        let (copiedRef, nextGenerator) =
              freshenTypeBinderRef ref generator
         in ( copiedRef
            , ( copiedRef : reservedRefs
              , nextGenerator
              , copies ++ [(ref, copiedRef)]
              )
            )

-- | Replay proof-bearing body-consumer refinements on the final root binder
-- plan.  This is deliberately stricter than the environment projection:
-- every certificate must identify one surviving root declaration and all
-- dependencies of the completed bound must already be available.  A local
-- owner-emission certificate refines the declaration in the owner's own
-- ETyAbs spine; an inherited certificate refines an ambient declaration.  An
-- ordinary certificate retains the matching local-Gamma closure.  The
-- distinct enclosing-ambient constructor instead proves that the exact
-- edge/exterior closure was checked before ownership moved to the root, so
-- that closure must now be absent.  Thus the final ETyAbs spine is correct by
-- construction rather than repaired after an InstAbstr failure.
projectCertifiedBodyConsumerRootScheme
  :: Set.Set TypeBinderIdentity
  -> [LocalGammaClosure]
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> [BodyConsumerBoundRefinementCertificate]
  -> ElabScheme
  -> Either ElabError ElabScheme
projectCertifiedBodyConsumerRootScheme =
  projectCertifiedBodyConsumerScheme True

projectCertifiedBodyConsumerScheme
  :: Bool
  -> Set.Set TypeBinderIdentity
  -> [LocalGammaClosure]
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> [BodyConsumerBoundRefinementCertificate]
  -> ElabScheme
  -> Either ElabError ElabScheme
projectCertifiedBodyConsumerScheme retainConsumedSpecializedEndpoint retainedRootConsumers closures ambientRefs localRefs certificates scheme0 = do
  projectedBinders <-
    projectCertifiedBodyConsumerRootBounds
      closures
      ambientRefs
      localRefs
      certificates
      (schemeBinderRefs scheme0)
  mapM_ validateConsumedAuthority allConsumedCertificates
  retainedProjectedBinders <-
    foldM
      retainConsumedDeclaration
      projectedBinders
      retainedConsumedCertificates
  consumedCertificates <-
    traverse selectConsumedRepresentative consumedCertificateGroups
  foldM
    projectConsumedCertificate
    (mkElabSchemeWithRefs retainedProjectedBinders (schemeBody scheme0))
    consumedCertificates
  where
    allConsumedCertificates =
      filter bodyConsumerBoundRefinementConsumed certificates

    retainedConsumedCertificates =
      filter retainedAtRoot allConsumedCertificates

    consumedCertificates0 =
      filter (not . retainedAtRoot) allConsumedCertificates

    retainedAtRoot certificate =
      Set.member
        (typeBinderRefIdentity (bcbrAmbientRef certificate))
        retainedRootConsumers
        || isJust (retainedPacketProjection certificate)

    retainedPacketProjection certificate = do
      guard retainConsumedSpecializedEndpoint
      projection@(projectionRef, _) <-
        bodyConsumerBoundRefinementEscapedPacketBinder certificate
      guard
        ( any
            (typeBinderRefsSameIdentity projectionRef . fst)
            (schemeBinderRefs scheme0)
        )
      pure projection

    consumedCertificateGroups =
      foldl insertConsumedCertificateGroup [] consumedCertificates0

    insertConsumedCertificateGroup groups certificate =
      case
          break
            ( any
                ( typeBinderRefsSameIdentity
                    (bcbrAmbientRef certificate)
                    . bcbrAmbientRef
                )
            )
            groups
        of
          (_, []) -> groups ++ [[certificate]]
          (before, matching : after) ->
            before ++ [matching ++ [certificate]] ++ after

    validateConsumedAuthority certificate =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerConsumedAtOwner _ authorityBound _
          | bodyConsumerBoundRefinementOwnerFinalized certificate
          , operationalEndpointTypesAgree
              authorityBound
              (bcbrCompletedBound certificate) ->
              pure ()
        authority ->
          projectionFailure
            "consumed refinement lacks finalized owner authority"
            [ "  target: " ++ show (bcbrAmbientRef certificate)
            , "  authority: " ++ show authority
            ]

    -- Packet placement can reconstruct the exact topology consumer at the
    -- enclosing root after its source lambda consumed the provisional local
    -- slot.  In that case the placement proof, not the historical local
    -- liveness state, owns the surviving declaration.  The graph can still
    -- spell the declaration's bound as an opened instance of the completed
    -- source endpoint (for @g g@, @a -> a@ under a preceding @forall a@).
    -- Close it with the certificate's self-contained endpoint now; Eq-Free
    -- can then remove the obsolete opened dependency.  Reification can also
    -- encode the packet's original Bottom slot as the exact universal
    -- eliminator @forall d. d@; the finalized certificate advances that
    -- identity to its completed bound before the root emits it.  These are
    -- certified construction transitions, not shape-driven final-type repairs.
    retainConsumedDeclaration projected certificate
      | Just (projectionRef, projectionBound) <-
          retainedPacketProjection certificate =
          case
              filter
                (typeBinderRefsSameIdentity projectionRef . fst)
                projected
            of
              [(_, Just currentBound)]
                | operationalEndpointTypesAgree
                    (tyToElab currentBound)
                    (tyToElab projectionBound) ->
                    pure projected
              matches ->
                projectionFailure
                  "escaped packet declaration does not retain its certified operated bound"
                  [ "  target: " ++ show projectionRef
                  , "  projected bound: " ++ show (tyToElab projectionBound)
                  , "  matching declarations: " ++ show matches
                  , "  certificate: " ++ show certificate
                  ]
      | otherwise =
          retainOrdinaryConsumedDeclaration projected certificate

    retainOrdinaryConsumedDeclaration projected certificate =
      case
          filter
            ( typeBinderRefsSameIdentity
                (bcbrAmbientRef certificate)
                . fst
            )
            projected
        of
          [(targetRef, Just bound)]
            | let currentBound = tyToElab bound
            , operationalEndpointTypesAgree
                currentBound
                (bcbrCompletedBound certificate)
                || completeUnboundedForallSpecializesTo
                  (bcbrCompletedBound certificate)
                  currentBound
                || ( operationalEndpointTypesAgree
                       (bcbrPreviousBound certificate)
                       TBottom
                       && completeUnboundedForallSpecializesTo
                         currentBound
                         (bcbrCompletedBound certificate)
                   ) -> do
                completedBound <-
                  either
                    ( \cause ->
                        projectionFailure
                          "retained packet completion is not a legal root bound"
                          [ "  target: " ++ show targetRef
                          , "  completed bound: "
                              ++ show (bcbrCompletedBound certificate)
                          , "  cause: " ++ cause
                          ]
                    )
                    pure
                    (elabToBound (bcbrCompletedBound certificate))
                pure
                  [ if typeBinderRefsSameIdentity ref targetRef
                      then (ref, Just completedBound)
                      else binder
                  | binder@(ref, _) <- projected
                  ]
            | certifiedForallSpineReachesCompletion
                (tyToElab bound)
                certificate ->
                -- The root already owns the complete closed declaration;
                -- the owner certificate and exact binder-spine plan prove
                -- only that the local consumer reaches its checked instance.
                -- Keep the declaration itself instead of leaking that
                -- owner-local open instance back into the root bound.
                pure projected
            | certifiedOperatedDeclaration
                (tyToElab bound)
                certificate ->
                -- Packet placement retained the exact operated declaration,
                -- while the local owner consumed its construction-oriented
                -- endpoint.  The private route records both sides of that
                -- computation, so the root keeps the already closed operated
                -- declaration instead of replacing it by the owner-local
                -- instance.
                pure projected
          matches ->
            projectionFailure
              "root packet placement did not retain a certified completable declaration"
              [ "  target: " ++ show (bcbrAmbientRef certificate)
              , "  completed bound: " ++ show (bcbrCompletedBound certificate)
              , "  matching declarations: " ++ show matches
              , "  certificate: " ++ show certificate
              , "  projected root binders: " ++ show projected
              ]

    -- Placement can retain a completed declaration with extra universal
    -- binders introduced by nested packet construction.  The consumed-owner
    -- certificate fixes the declaration identity and its completed endpoint;
    -- require the exact xMLF binder-spine planner to consume or retain every
    -- leading forall and replay to that endpoint.  Requiring a non-empty
    -- forall prefix prevents this lane from turning an unrelated Bottom slot
    -- into an arbitrary bound via InstBot.
    certifiedForallSpineReachesCompletion currentBound certificate =
      not
        ( null
            (schemeBinderRefs (schemeFromType currentBound))
        )
        && isJust
          ( planExactBinderSpine
              operationalEndpointTypesAgree
              currentBound
              (bcbrCompletedBound certificate)
          )

    certifiedOperatedDeclaration currentBound certificate =
      let route =
            authorizedBodyConsumerRoute
              (bcbrDeclarationAuthority certificate)
       in operationalEndpointTypesAgree
            currentBound
            (bcrOperatedType route)
            && operationalEndpointTypesAgree
              (bcrConstructionOperatedType route)
              (bcbrCompletedBound certificate)

    selectConsumedRepresentative [] =
      projectionFailure
        "internal empty consumed-certificate group"
        []
    selectConsumedRepresentative certificatesForTarget@(first : _) =
      case representatives of
        [] ->
          incompatibleConsumedCertificates
            "consumed certificates have no common generalized declaration"
        representative : remainingRepresentatives
          | all
              ( operationalEndpointTypesAgree
                  (bcbrCompletedBound representative)
                  . bcbrCompletedBound
              )
              remainingRepresentatives ->
              pure representative
          | otherwise ->
              case initialDeclarationRepresentatives of
                [initialDeclarationRepresentative] ->
                  -- Intro/Inside can make a specialized endpoint and a
                  -- vacuously generalized endpoint mutually constructible.
                  -- The unique Bottom-to-bound transition is the positive
                  -- evidence for which endpoint first entered this exact
                  -- Gamma declaration; later certificates only consume or
                  -- refine that declaration.
                  pure initialDeclarationRepresentative
                initialDeclarationRepresentative : remainingInitialRepresentatives
                  | all
                      ( operationalEndpointTypesAgree
                          (bcbrCompletedBound initialDeclarationRepresentative)
                          . bcbrCompletedBound
                      )
                      remainingInitialRepresentatives ->
                      pure initialDeclarationRepresentative
                _ -> selectOwnerBoundaryRepresentative
      where
        representatives =
          [ candidate
          | candidate <- certificatesForTarget
          , all
              (completedDeclarationConstructs candidate)
              certificatesForTarget
          ]

        completedDeclarationConstructs candidate endpoint =
          operationalEndpointTypesAgree
            (bcbrCompletedBound candidate)
            (bcbrCompletedBound endpoint)
            || any
              ( bodyConsumerBoundRefinementCarriesGammaTransition
                  (bcbrCompletedBound candidate)
                  (bcbrCompletedBound endpoint)
              )
              certificatesForTarget
            || bodyConsumerCompletedDeclarationSpecializesToEndpoint
              candidate
              (bcbrCompletedBound endpoint)

        ownerBoundaryRepresentatives =
          filter emittedAtOwnerBoundary representatives

        initialDeclarationRepresentatives =
          filter
            ( operationalEndpointTypesAgree TBottom
                . bcbrPreviousBound
            )
            representatives

        emittedAtOwnerBoundary certificate =
          let route =
                authorizedBodyConsumerRoute
                  (bcbrDeclarationAuthority certificate)
           in bcrEdgeId route == lgoBoundaryEdge (bcrOwner route)

        selectOwnerBoundaryRepresentative =
          case ownerBoundaryRepresentatives of
            [ownerBoundaryRepresentative] ->
              -- A retained, vacuous forall can instead be introduced on a
              -- descendant edge, or more than one sibling can observe the
              -- original Bottom slot.  The certificate emitted at the
              -- owner's boundary then records the endpoint that entered
              -- lexical Gamma.
              pure ownerBoundaryRepresentative
            ownerBoundaryRepresentative : remainingOwnerBoundaryRepresentatives
              | all
                  ( operationalEndpointTypesAgree
                      (bcbrCompletedBound ownerBoundaryRepresentative)
                      . bcbrCompletedBound
                  )
                  remainingOwnerBoundaryRepresentatives ->
                  pure ownerBoundaryRepresentative
            _ ->
              incompatibleConsumedCertificates
                "consumed certificates have multiple incomparable generalized declarations"

        incompatibleConsumedCertificates detail =
          projectionFailure
            detail
            [ "  target: "
                ++ show (bcbrAmbientRef first)
            , "  certificates: " ++ show certificatesForTarget
            ]

    projectConsumedCertificate scheme certificate = do
      sourceOpenedScheme <-
        openCertifiedConsumedDeclarationOccurrences
          certificate
          scheme
      let targetRef = bcbrAmbientRef certificate
          completedBound = bcbrCompletedBound certificate
          matchingBinders =
            filter
              (typeBinderRefsSameIdentity targetRef . fst)
              (schemeBinderRefs sourceOpenedScheme)
      projectedBound <-
        case matchingBinders of
          [] -> pure completedBound
          [(_, mbBound)]
            | bodyConsumerBoundRefinementAcceptsDeclarationState
                certificate
                (maybe TBottom tyToElab mbBound) ->
                pure completedBound
            | isNothing mbBound ->
                -- Root planning can still carry the packet's original pending
                -- declaration after the exact owner has completed and consumed
                -- it.  The finalized consumed-at-owner certificate, not the
                -- Bottom shape, authorizes advancing that exact identity to
                -- its completed bound before removing it.
                pure completedBound
            | operationalEndpointTypesAgree
                (bcbrPreviousBound certificate)
                TBottom
            , completeUnboundedForallSpecializesTo
                (maybe TBottom tyToElab mbBound)
                completedBound ->
                -- Reification may represent the packet's pending Bottom slot
                -- as an exact universal eliminator such as @forall d. d@.
                -- The certificate fixes the declaration identity and state
                -- transition; consuming the complete unbounded forall spine
                -- independently proves that this root representation reaches
                -- the certified completed bound.
                pure completedBound
            | certifiedForallSpineReachesCompletion
                (maybe TBottom tyToElab mbBound)
                certificate ->
                -- Nested packet construction can leave the consumed root
                -- declaration at a more general, bounded-forall closure of
                -- the endpoint that the owner actually consumed.  The
                -- declaration identity selects the certificate, and the exact
                -- binder-spine plan independently constructs its completed
                -- bound from that closure.  Once the declaration itself is
                -- removed, substitute the certificate's closed completion;
                -- retaining the opened closure would leak its owner-local
                -- binder state into the root.
                pure completedBound
            | bodyConsumerCompletedDeclarationSpecializesToEndpoint
                certificate
                (maybe TBottom tyToElab mbBound) ->
                -- A surviving root scheme denotes the exact instance at
                -- which the owner consumed this declaration.  An
                -- intermediate administrative Gamma is different: removing
                -- the declaration also removes the lexical context which
                -- made that opened instance meaningful, so substitute the
                -- certificate's self-contained completed endpoint there.
                pure
                  ( if retainConsumedSpecializedEndpoint
                      then maybe TBottom tyToElab mbBound
                      else completedBound
                  )
            | certifiedCompletionClosesAtDeclaredBounds
                sourceOpenedScheme
                certificate
                (maybe TBottom tyToElab mbBound) ->
                -- The checked owner can consume a flexible completion such
                -- as @b -> b@ while the root planner still carries its exact
                -- closed instance @sigma -> sigma@ and separately declares
                -- @b >= sigma@.  The finalized certificate owns @b -> b@;
                -- the identity-matched root declaration owns the only
                -- permitted substitution back to the stale closed state.
                -- Project the consumed declaration to the certified
                -- completion rather than requiring those two states to be
                -- equal after the owner has already entered Hyp(b).
                pure completedBound
            | otherwise ->
                projectionFailure
                  "consumed root binder does not carry a certified declaration state"
                  [ "  target: " ++ show targetRef
                  , "  current bound: "
                      ++ show (maybe TBottom tyToElab mbBound)
                  , "  completed bound: " ++ show completedBound
                  , "  certificate: " ++ show certificate
                  ]
          matches ->
            projectionFailure
              "consumed root identity has duplicate declarations"
              [ "  target: " ++ show targetRef
              , "  declarations: " ++ show matches
              ]
      unless
        ( not
            ( any
                (typeBinderRefsSameIdentity targetRef)
                (freeTypeVarRefsType projectedBound)
            )
        )
        ( projectionFailure
            "consumed declaration completion is recursively self-referential"
            [ "  target: " ++ show targetRef
            , "  projected bound: " ++ show projectedBound
            ]
        )
      let (projected, _lexicalCopies) =
            substituteCertifiedCompletionWithLexicalCopies
              targetRef
              projectedBound
              sourceOpenedScheme
          residualTargets =
            [ ref
            | ref <-
                map fst (schemeBinderRefs projected)
                  ++ freeTypeVarRefsType (schemeToType projected)
            , typeBinderRefsSameIdentity ref targetRef
            ]
      unless
        (null residualTargets)
        ( projectionFailure
            "consumed declaration remains in the projected root scheme"
            [ "  target: " ++ show targetRef
            , "  residual refs: " ++ show residualTargets
            , "  projected scheme: " ++ show projected
            ]
        )
      pure projected

    -- A consumed completion may mention exact flexible declarations that
    -- remain in the root scheme.  Closing precisely the completion refs that
    -- are absent from the planner state at their identity-matched declared
    -- bounds must reproduce that state.  This is the construction-time
    -- inverse of the owner's already checked Hyp steps; an arbitrary
    -- same-shaped bound, an unbounded declaration, or a missing/duplicate
    -- identity cannot authorize the projection.
    certifiedCompletionClosesAtDeclaredBounds scheme certificate currentBound =
      not (null refsToClose)
        && length declaredBounds == length refsToClose
        && operationalEndpointTypesAgree closedCompletion currentBound
      where
        completion = bcbrCompletedBound certificate
        currentFreeRefs = freeTypeVarRefsType currentBound
        refsToClose =
          foldr insertCompletionRef []
            [ ref
            | ref <- freeTypeVarRefsType completion
            , not
                ( any
                    (typeBinderRefsSameIdentity ref)
                    currentFreeRefs
                )
            ]
        declaredBounds =
          mapMaybe exactDeclaredBound refsToClose
        exactDeclaredBound ref =
          case
              filter
                (typeBinderRefsSameIdentity ref . fst)
                (schemeBinderRefs scheme)
            of
              [(_, Just bound)] -> Just (ref, tyToElab bound)
              _ -> Nothing
        closedCompletion =
          foldl
            ( \ty (ref, declaredBound) ->
                substTypeCaptureRef ref declaredBound ty
            )
            completion
            declaredBounds
        insertCompletionRef ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

    projectionFailure detail context =
      Left
        ( ValidationFailed
            ( [ "cannot project consumed body-consumer declaration into root construction"
              , "  detail: " ++ detail
              ]
                ++ context
            )
        )

-- | Open a consumed declaration's closed source-forall completion exactly at
-- occurrences which are already underneath those same source declarations.
-- A frozen construction can contain @forall a. b@ while the checked body
-- certificate consumes @b@ at @forall a. a -> a@.  Substituting that closed
-- bound directly would create @forall a. forall a. a -> a@: two lexical
-- declarations with one identity.
--
-- The finalized certificate selects @b@, and matching binder identity plus
-- bound proves that the surrounding @forall a@ is already the first
-- declaration of the completion.  Open that one layer before the ordinary
-- capture-avoiding substitution removes @b@.  This is intentionally not a
-- duplicate-forall normalizer: it starts only at the exact completed leading
-- spine, and a conflicting declaration after that point fails closed.
openCertifiedConsumedDeclarationOccurrences
  :: BodyConsumerBoundRefinementCertificate
  -> ElabScheme
  -> Either ElabError ElabScheme
openCertifiedConsumedDeclarationOccurrences certificate scheme = do
  binders <- traverse openBinder (schemeBinderRefs scheme)
  body <- openType Nothing (schemeBody scheme)
  pure (mkElabSchemeWithRefs binders body)
  where
    targetRef = bcbrAmbientRef certificate
    completedBound = bcbrCompletedBound certificate
    route =
      authorizedBodyConsumerRoute
        (bcbrDeclarationAuthority certificate)
    sourceOpenedCompletion = bcrOperatedType route
    constructionOpenedCompletion =
      bcrConstructionOperatedType route
    initialOpenedCompletion
      | alphaEqTypePreservingRecursiveBinders
          sourceOpenedCompletion
          constructionOpenedCompletion
          && operationalEndpointTypesAgree
            constructionOpenedCompletion
            completedBound =
          (sourceOpenedCompletion, completedBound)
      | otherwise = (completedBound, completedBound)

    openBinder (ref, mbBound) = do
      openedBound <- traverse (openBound Nothing) mbBound
      pure (ref, openedBound)

    openType mbOpened ty =
      case ty of
        TVarRef ref
          | Just (_, opened) <- mbOpened
          , typeBinderRefsSameIdentity ref targetRef ->
              pure opened
          | otherwise -> pure ty
        TArrow domain codomain ->
          TArrow
            <$> openType mbOpened domain
            <*> openType mbOpened codomain
        TConWithIdentity identity constructor args ->
          TConWithIdentity identity constructor
            <$> traverse (openType mbOpened) args
        TVarAppRef ref args -> do
          openedArgs <- traverse (openType mbOpened) args
          pure
            ( case mbOpened of
                Just (_, opened)
                  | typeBinderRefsSameIdentity ref targetRef ->
                      composeTypeHeadRef ref opened openedArgs
                _ -> TVarAppRef ref openedArgs
            )
        TBaseWithIdentity {} -> pure ty
        TBottom -> pure TBottom
        TForallRef ref mbBound body -> do
          (openedBound, openedBody) <-
            openForall mbOpened ref mbBound body
          pure (TForallRef ref openedBound openedBody)
        TMuRef ref body
          | typeBinderRefsSameIdentity ref targetRef ->
              pure ty
          | otherwise -> do
              rejectConflictingOpenedBinder mbOpened ref body
              TMuRef ref <$> openType mbOpened body

    openBound mbOpened bound =
      case bound of
        TArrow domain codomain ->
          TArrow
            <$> openType mbOpened domain
            <*> openType mbOpened codomain
        TConWithIdentity identity constructor args ->
          TConWithIdentity identity constructor
            <$> traverse (openType mbOpened) args
        TVarAppRef ref args -> do
          openedArgs <- traverse (openType mbOpened) args
          pure
            ( case mbOpened of
                Just (_, opened)
                  | typeBinderRefsSameIdentity ref targetRef ->
                      composeTypeHeadRef ref opened openedArgs
                _ -> TVarAppRef ref openedArgs
            )
        TBaseWithIdentity {} -> pure bound
        TBottom -> pure TBottom
        TForallRef ref mbBound body -> do
          (openedBound, openedBody) <-
            openForall mbOpened ref mbBound body
          pure (TForallRef ref openedBound openedBody)
        TMuRef ref body
          | typeBinderRefsSameIdentity ref targetRef ->
              pure bound
          | otherwise -> do
              rejectConflictingOpenedBinder mbOpened ref body
              TMuRef ref <$> openType mbOpened body

    openForall mbOpened ref mbBound body = do
      openedBound <- traverse (openBound mbOpened) mbBound
      if typeBinderRefsSameIdentity ref targetRef
        then pure (openedBound, body)
        else
          case matchingOpenedBody mbOpened ref mbBound of
            Just openedBodies
              | targetOccursFree body -> do
                  openedBody <- openType (Just openedBodies) body
                  pure (openedBound, openedBody)
            _ -> do
              rejectConflictingOpenedBinder mbOpened ref body
              openedBody <- openType mbOpened body
              pure (openedBound, openedBody)

    matchingOpenedBody mbOpened ref mbBound =
      case fromMaybe initialOpenedCompletion mbOpened of
        ( TForallRef sourceRef sourceBinderBound sourceBody
          , TForallRef completedRef completedBinderBound completedBody
          )
            | typeBinderRefsSameIdentity ref completedRef
            , binderBoundsAgree mbBound completedBinderBound ->
                Just (completedBody, completedBody)
            | typeBinderRefsSameIdentity ref sourceRef
            , binderBoundsAgree mbBound sourceBinderBound ->
                let completedBodyAtSourceScope =
                      alphaRenameTypeBinderScopes
                        [(completedRef, ref)]
                        completedBody
                 in Just (sourceBody, completedBodyAtSourceScope)
        _ -> Nothing

    binderBoundsAgree left right =
      operationalEndpointTypesAgree
        (maybe TBottom tyToElab left)
        (maybe TBottom tyToElab right)

    targetOccursFree =
      any (typeBinderRefsSameIdentity targetRef)
        . freeTypeVarRefsType

    rejectConflictingOpenedBinder Nothing _ _ = pure ()
    rejectConflictingOpenedBinder (Just (_, opened)) ref body
      | targetOccursFree body
      , any
          (typeBinderRefsSameIdentity ref)
          ( typeBinderDeclarationRefs opened
              ++ freeTypeVarRefsType opened
          ) =
          Left
            ( ValidationFailed
                [ "consumed source-forall completion has a conflicting lexical declaration"
                , "  certificate: " ++ show certificate
                , "  conflicting declaration: " ++ show ref
                , "  opened completion: " ++ show opened
                , "  occurrence body: " ++ show body
                ]
            )
      | otherwise = pure ()

projectCertifiedBodyConsumerRootBounds
  :: [LocalGammaClosure]
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
projectCertifiedBodyConsumerRootBounds closures ambientRefs localRefs certificates binders = do
  unless
    (null duplicateTargets)
    ( refinementFailure
        "multiple certificates target one ambient root declaration"
        ["  duplicate targets: " ++ show duplicateTargets]
    )
  projected <-
    foldM
      (projectCertifiedBodyConsumerBound True)
      binders
      declarationCertificates
  projectedOrdered <-
    either
      (Left . ValidationFailed . pure)
      (Right . schemeBinderRefs)
      ( orderSourceProjectedSchemeBinders
          "certified body-consumer root bounds"
          (mkElabSchemeWithRefs projected TBottom)
      )
  mapM_ (validateCertificate projectedOrdered) declarationCertificates
  pure projectedOrdered
  where
    declarationCertificates =
      filter
        (not . bodyConsumerBoundRefinementConsumed)
        certificates

    duplicateTargets =
      [ bcbrAmbientRef certificate
      | (index, certificate) <-
          zip [0 :: Int ..] declarationCertificates
      , any
          ( typeBinderRefsSameIdentity
              (bcbrAmbientRef certificate)
              . bcbrAmbientRef
          )
          (drop (index + 1) declarationCertificates)
      ]

    validateCertificate projected certificate = do
      let route =
            authorizedBodyConsumerRoute
              (bcbrDeclarationAuthority certificate)
          matchingClosures =
            [ closure
            | closure <- closures
            , lgcOwner closure == bcrOwner route
            , bcrEdgeId route `elem` NonEmpty.toList (lgcEdgeIds closure)
            , lgcExteriorNode closure == bcrExteriorNode route
            ]
          closureContext =
            [ "  route: " ++ show route
            , "  matching closures: " ++ show matchingClosures
            , "  available closures: " ++ show closures
            ]
          targetRef = bcbrAmbientRef certificate
          localOverlap =
            any (typeBinderRefsSameIdentity targetRef) localRefs
          ambientAuthorized =
            any (typeBinderRefsSameIdentity targetRef) ambientRefs
          declarationAuthorities =
            ambientRefs ++ if localOverlap then localRefs else []
          targetIndex =
            fst
              <$> find
                ( typeBinderRefsSameIdentity targetRef
                    . fst
                    . snd
                )
                (zip [0 :: Int ..] projected)
          completedDependencies =
            freeTypeVarRefsType (bcbrCompletedBound certificate)
          dependencyFailures =
            [ dependency
            | dependency <- completedDependencies
            , not (dependencyAvailableBefore targetIndex dependency projected)
                || not
                  ( any
                      (typeBinderRefsSameIdentity dependency)
                      declarationAuthorities
                  )
            ]
          validateLocalDeclaration = do
            unless
              (length matchingClosures == 1)
              ( refinementFailure
                  "certificate has no unique local-Gamma owner/edge/exterior closure"
                  closureContext
              )
          validateUniqueDeclaration =
            unless
              (localOverlap /= ambientAuthorized)
              ( refinementFailure
                  "certificate target is not owned by exactly one root declaration class"
                  [ "  target: " ++ show targetRef
                  , "  certificate: " ++ show certificate
                  , "  local authority: " ++ show localRefs
                  , "  ambient authority: " ++ show ambientRefs
                  ]
              )
      validateUniqueDeclaration
      case bcbrDeclarationAuthority certificate of
        BodyConsumerEnclosingAmbient {} ->
          do
            unless
              (ambientAuthorized && not localOverlap)
              ( refinementFailure
                  "enclosing certificate does not target one ambient declaration"
                  ( ("  target: " ++ show targetRef)
                      : ("  ambient authority: " ++ show ambientRefs)
                      : closureContext
                  )
              )
            unless
              (null matchingClosures)
              ( refinementFailure
                  "root-owned enclosing certificate still has a local-Gamma closure"
                  closureContext
              )
        BodyConsumerInheritedAmbient {} ->
          do
            validateLocalDeclaration
        BodyConsumerLocallyEmitted {} ->
          validateLocalDeclaration
        BodyConsumerPendingOwnerEmission {} ->
          validateLocalDeclaration
        BodyConsumerOrdinaryOwnerEmission {} ->
          validateLocalDeclaration
        BodyConsumerConsumedAtOwner {} ->
          pure ()
      unless
        (null dependencyFailures)
        ( refinementFailure
            "completed ambient bound has an unavailable or forward dependency"
            [ "  target: " ++ show targetRef,
              "  completed bound: "
                ++ show (bcbrCompletedBound certificate),
              "  unavailable dependencies: " ++ show dependencyFailures,
              "  declaration authority: " ++ show declarationAuthorities,
              "  projected binders: " ++ show projected
            ]
        )
    dependencyAvailableBefore Nothing _ _ = False
    dependencyAvailableBefore (Just targetIndex) dependency projected =
      case
          find
            (typeBinderRefsSameIdentity dependency . fst . snd)
            (zip [0 :: Int ..] projected)
        of
          Nothing -> True
          Just (dependencyIndex, _) -> dependencyIndex < targetIndex

    refinementFailure detail context =
      Left
        ( ValidationFailed
            ( [ "body-consumer bound refinement does not match final root construction",
                "  detail: " ++ detail
              ]
                ++ context
            )
        )

-- | Close the source-owned leading binder spine of a frozen Gamma bound when
-- a finalized child certificate records the exact closed operated type.
-- Packet preparation can observe the residual after N has opened that source
-- spine (for example @a -> a@), while the child-owned declaration is already
-- finalized at @forall a. a -> a@.  The certificate supplies the target
-- identity and closed type; the caller supplies the immutable source binder
-- identities.  Both must agree exactly before the declaration is advanced.
-- This is the source-boundary inverse of the certified N step, not a generic
-- type-shape completion.
completeCertifiedSourceOpenBodyConsumerBounds
  :: [TypeBinderRef]
  -> [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
completeCertifiedSourceOpenBodyConsumerBounds sourceRefs certificates initialBinders =
  foldM completeCertificate initialBinders certificates
  where
    completeCertificate currentBinders certificate =
      case matchingSourceOpenDeclarations currentBinders certificate of
        [] -> pure currentBinders
        [(targetRef, completedBound)] -> do
          completedFlexibleBound <-
            case elabToBound completedBound of
              Right bound -> pure bound
              Left cause ->
                completionFailure
                  certificate
                  [ "the completed source bound is not a legal flexible bound"
                  , "  declaration: " ++ show targetRef
                  , "  completed bound: " ++ show completedBound
                  , "  cause: " ++ show cause
                  ]
          pure
            [ if typeBinderRefsSameIdentity ref targetRef
                then (ref, Just completedFlexibleBound)
                else binder
            | binder@(ref, _) <- currentBinders
            ]
        matches ->
          completionFailure
            certificate
            [ "the source-open declaration is ambiguous"
            , "  matches: " ++ show matches
            ]

    matchingSourceOpenDeclarations currentBinders certificate =
      [ (ref, completedBound)
      | (ref, Just currentBound) <- currentBinders
      , typeBinderRefsSameIdentity ref (bcbrAmbientRef certificate)
      , not
          ( bodyConsumerBoundRefinementAppliesToDeclarationState
              ref
              (tyToElab currentBound)
              certificate
          )
      , bodyConsumerBoundRefinementOwnerFinalized certificate
      , BodyConsumerEnclosingAmbient route declaredBound <-
          [bcbrDeclarationAuthority certificate]
      , let completedBound = bcbrCompletedBound certificate
            sourceOperatedBound = bcrOperatedType route
            sourceOperatedScheme = schemeFromType sourceOperatedBound
            sourceOperatedBinders =
              map fst (schemeBinderRefs sourceOperatedScheme)
      , operationalEndpointTypesAgree
          (bcbrPreviousBound certificate)
          TBottom
      , operationalEndpointTypesAgree declaredBound completedBound
      , not (null sourceOperatedBinders)
      , all sourceOwns sourceOperatedBinders
      , all
          ( \sourceRef ->
              any
                (typeBinderRefsSameIdentity sourceRef)
                (freeTypeVarRefsType (tyToElab currentBound))
          )
          sourceOperatedBinders
      , operationalEndpointTypesAgree
          (schemeBody sourceOperatedScheme)
          (tyToElab currentBound)
      , operationalEndpointTypesAgree
          sourceOperatedBound
          declaredBound
      , operationalEndpointTypesAgree
          (bcrConstructionOperatedType route)
          completedBound
      ]

    sourceOwns ref =
      any (typeBinderRefsSameIdentity ref) sourceRefs

    completionFailure
      :: BodyConsumerBoundRefinementCertificate
      -> [String]
      -> Either ElabError a
    completionFailure certificate details =
      Left
        ( ValidationFailed
            ( [ "cannot close a certified source-open body-consumer declaration"
              , "  certificate: " ++ show certificate
              , "  source binder refs: " ++ show sourceRefs
              ]
                ++ details
            )
        )

-- | Advance declarations carried by one exact prepared packet through the
-- body-consumer certificates for that same packet.  Source projection can
-- leave a declaration at an earlier, more-general source bound even after
-- the checked body has published its completed construction bound.  The
-- packet owner/edge/identity tuple selects the certificate, and an executable
-- exact binder-spine computation or instantiation must connect the two bound
-- states before either is replaced or retained.
--
-- This is deliberately narrower than
-- 'bodyConsumerBoundRefinementAcceptsDeclarationState': unrelated packets may
-- reuse the same graph identity after different specializations, so identity
-- alone never licenses this transition.
completePreparedBodyConsumerConstructionBounds
  :: TypeCheck.Env
  -> LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
completePreparedBodyConsumerConstructionBounds typeEnv owner packet certificates initialBinders =
  foldM advanceCertificate initialBinders relevantCertificates
  where
    relevantCertificates =
      filter
        advancesPreparedConstruction
        certificates

    advancesPreparedConstruction certificate =
      bodyConsumerBoundRefinementCompletesPreparedPacket
        owner
        packet
        certificate
        || ( packetNamesExactLambdaOwner
              && bodyConsumerBoundRefinementCompletesOwnerEndpoint
                owner
                certificate
           )

    -- An administrative result packet can name the exact value-lambda
    -- constructor without owning a separate packet Gamma.  In that case the
    -- lambda owner's body-edge certificate is the positive link that advances
    -- the packet endpoint; accepting merely the same result identity would
    -- let a sibling specialization rewrite it.
    packetNamesExactLambdaOwner =
      case subtermGeneralizationAdministrativeLambdaResultConstruction packet of
        Just (lambdaNode, _) -> lambdaNode == lgoTermNode owner
        Nothing -> False

    advanceCertificate binders certificate =
      case matchingBinders binders certificate of
        [] -> pure binders
        [(targetRef, currentBound)] -> do
          replacement <-
            selectCompletedBound targetRef currentBound certificate
          pure
            [ if typeBinderRefsSameIdentity ref targetRef
                then (ref, replacement)
                else binder
            | binder@(ref, _) <- binders
            ]
        matches ->
          completionFailure
            certificate
            [ "the prepared construction contains duplicate target declarations"
            , "  matches: " ++ show matches
            ]

    matchingBinders binders certificate =
      filter
        ( typeBinderRefsSameIdentity
            (bcbrAmbientRef certificate)
            . fst
        )
        binders

    selectCompletedBound targetRef currentBound certificate
      | bodyConsumerBoundRefinementEmittedBy owner certificate
      , bodyConsumerDeclarationConstructsCompletion
          certificate
          currentBoundTy =
          pure currentBound
      | bodyConsumerBoundRefinementAppliesToDeclarationState
          targetRef
          currentBoundTy
          certificate =
          constructionBinderBound certificate completedBoundTy
      | isJust
          ( planExactBinderSpine
              exactLambdaEndpointTypesAgree
              currentBoundTy
              completedBoundTy
          ) =
          -- Packet, owner, edge, and declaration identity were selected
          -- above.  A retained/consumed forall spine is therefore a checked
          -- construction of this exact declaration state, even when that
          -- state was not one of the graph snapshots stored in the body
          -- certificate.  The plan constructor applies the complete xMLF
          -- computation and accepts it only when it reproduces the sealed
          -- completion.
          constructionBinderBound certificate completedBoundTy
      | isJust
          ( constructExactInstantiation
              typeEnv
              exactLambdaEndpointTypesAgree
              currentBoundTy
              completedBoundTy
          ) =
          constructionBinderBound certificate completedBoundTy
      | isJust
          ( constructExactInstantiation
              typeEnv
              exactLambdaEndpointTypesAgree
              completedBoundTy
              currentBoundTy
          ) =
          pure currentBound
      | otherwise =
          completionFailure
            certificate
            [ "the prepared source bound and checked completion are incomparable"
            , "  declaration: " ++ show targetRef
            , "  prepared bound: " ++ show currentBoundTy
            , "  completed bound: " ++ show completedBoundTy
            ]
      where
        currentBoundTy = maybe TBottom tyToElab currentBound
        completedBoundTy = bcbrCompletedBound certificate

    constructionBinderBound _ TBottom = pure Nothing
    constructionBinderBound certificate completedBound =
      case elabToBound completedBound of
        Right bound -> pure (Just bound)
        Left cause ->
          completionFailure
            certificate
            [ "the checked completion is not a legal flexible bound"
            , "  completed bound: " ++ show completedBound
            , "  cause: " ++ cause
            ]

    completionFailure
      :: BodyConsumerBoundRefinementCertificate
      -> [String]
      -> Either ElabError a
    completionFailure certificate details =
      Left
        ( ValidationFailed
            ( [ "cannot advance an exact prepared body-consumer declaration"
              , "  owner: " ++ show owner
              , "  certificate: " ++ show certificate
              ]
                ++ details
            )
        )

-- | Apply the same exact-identity update to a secondary construction binder
-- view.  Such a view may omit the target entirely, but if it contains the
-- declaration it must carry the same provisional or already-completed bound.
projectCertifiedBodyConsumerBoundsIfPresent
  :: [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
projectCertifiedBodyConsumerBoundsIfPresent certificates binders =
  foldM
    (projectCertifiedBodyConsumerBound False)
    binders
    ( filter
        (not . bodyConsumerBoundRefinementConsumed)
        certificates
    )

-- | Recover the child-local declaration only from the special packet
-- projection constructor.  Revalidating the operated binder and bound here
-- makes certificate renaming fail closed instead of allowing a stale ref to
-- complete a same-shaped candidate.
certifiedPacketConsumerProjection
  :: BodyConsumerBoundRefinementCertificate
  -> Maybe (TypeBinderRef, BoundType)
certifiedPacketConsumerProjection certificate = do
  projectedBound <-
    case bcbrConstructionProjection certificate of
      DirectBodyConsumerConstructionProjection -> Nothing
      PacketOperatedBodyConsumerConstructionProjection bound -> Just bound
  let authority = bcbrDeclarationAuthority certificate
      route = authorizedBodyConsumerRoute authority
      projectedRef = bcrConstructionRef route
      ambientRef = bcbrAmbientRef certificate
  guard
    ( case authority of
        BodyConsumerInheritedAmbient {} -> True
        BodyConsumerEnclosingAmbient {} -> True
        _ -> False
    )
  guard
    ( not (typeBinderRefsSameIdentity projectedRef ambientRef)
        && typeBinderRefIdentity (bcrSemanticRef route)
          == typeBinderIdentityFromNode (bcrExteriorNode route)
        && operationalEndpointTypesAgree
          (bcrConstructionOperatedType route)
          (bcbrCompletedBound certificate)
    )
  [(_, Just operatedBound)] <-
    pure
      [ binder
      | binder@(ref, _) <-
          schemeBinderRefs (schemeFromType (bcrOperatedType route))
      , typeBinderRefsSameIdentity ref projectedRef
      ]
  guard
    ( operationalEndpointTypesAgree
        (tyToElab projectedBound)
        (tyToElab operatedBound)
    )
  pure (projectedRef, projectedBound)

-- | A finalized cross-identity packet projection can outlive the graph
-- declaration consumed by its local owner.  The returned declaration is the
-- exact packet-operated binder that an enclosing construction boundary must
-- either already own or construct.  Requiring the finalized-and-consumed
-- state keeps an ordinary ambient use from manufacturing a root forall.
bodyConsumerBoundRefinementEscapedPacketBinder
  :: BodyConsumerBoundRefinementCertificate
  -> Maybe (TypeBinderRef, BoundType)
bodyConsumerBoundRefinementEscapedPacketBinder certificate = do
  guard
    ( bodyConsumerBoundRefinementOwnerFinalized certificate
        && bodyConsumerBoundRefinementConsumed certificate
    )
  projectedBound <-
    case bcbrConstructionProjection certificate of
      DirectBodyConsumerConstructionProjection -> Nothing
      PacketOperatedBodyConsumerConstructionProjection bound -> Just bound
  route <-
    case bcbrDeclarationAuthority certificate of
      BodyConsumerConsumedAtOwner consumedRoute authorityBound _
        | operationalEndpointTypesAgree
            authorityBound
            (bcbrCompletedBound certificate) ->
            Just consumedRoute
      _ -> Nothing
  let projectedRef = bcrConstructionRef route
  guard
    ( ( typeBinderRefIdentity (bcrSemanticRef route)
          == typeBinderIdentityFromNode (bcrExteriorNode route)
          || ( typeBinderRefsSameIdentity
                 (bcrSemanticRef route)
                 (bcbrAmbientRef certificate)
                 && typeBinderRefsSameIdentity
                   (bcbrAmbientRef certificate)
                   projectedRef
             )
      )
        && operationalEndpointTypesAgree
          (bcrConstructionOperatedType route)
          (bcbrCompletedBound certificate)
    )
  [(_, Just operatedBound)] <-
    pure
      [ binder
      | binder@(ref, _) <-
          schemeBinderRefs (schemeFromType (bcrOperatedType route))
      , typeBinderRefsSameIdentity ref projectedRef
      ]
  guard
    ( operationalEndpointTypesAgree
        (tyToElab projectedBound)
        (tyToElab operatedBound)
    )
  pure (projectedRef, projectedBound)

-- | Proof that one finalized packet-operated consumer joins an already
-- published ambient result to the exact child-local declaration selected by
-- an enclosing lambda.  The constructor stays private because the two
-- identities are intentionally not aliases: callers may only reuse the
-- ambient result's pre-publication producer after this exact certificate has
-- matched both endpoints and the target declaration bound.
data CertifiedPacketConsumerBodyProjection =
  CertifiedPacketConsumerBodyProjection
    !TypeBinderRef
    !ElabType
    !TypeBinderRef
    !ElabType
  deriving (Eq, Show)

certifiedPacketConsumerBodyProjectionSourceRef
  :: CertifiedPacketConsumerBodyProjection
  -> TypeBinderRef
certifiedPacketConsumerBodyProjectionSourceRef
  (CertifiedPacketConsumerBodyProjection sourceRef _ _ _) = sourceRef

certifiedPacketConsumerBodyProjectionSourceType
  :: CertifiedPacketConsumerBodyProjection
  -> ElabType
certifiedPacketConsumerBodyProjectionSourceType
  (CertifiedPacketConsumerBodyProjection _ sourceTy _ _) = sourceTy

certifiedPacketConsumerBodyProjectionTargetRef
  :: CertifiedPacketConsumerBodyProjection
  -> TypeBinderRef
certifiedPacketConsumerBodyProjectionTargetRef
  (CertifiedPacketConsumerBodyProjection _ _ targetRef _) = targetRef

certifiedPacketConsumerBodyProjectionTargetType
  :: CertifiedPacketConsumerBodyProjection
  -> ElabType
certifiedPacketConsumerBodyProjectionTargetType
  (CertifiedPacketConsumerBodyProjection _ _ _ targetTy) = targetTy

certifyPacketConsumerBodyProjection
  :: Map.Map TypeBinderRef ElabType
  -> [(TypeBinderRef, Maybe BoundType)]
  -> CertifiedLambdaBodyConstruction
  -> ElabType
  -> BodyConsumerBoundRefinementCertificate
  -> Maybe CertifiedPacketConsumerBodyProjection
certifyPacketConsumerBodyProjection ambientBindings candidates bodyConstruction targetTy certificate = do
  guard
    ( bodyConsumerBoundRefinementOwnerFinalized certificate
        && not (bodyConsumerBoundRefinementConsumed certificate)
    )
  sourceRef <-
    case certifiedLambdaBodyConstructedType bodyConstruction of
      TVarRef ref -> Just ref
      _ -> Nothing
  targetRef <-
    case targetTy of
      TVarRef ref -> Just ref
      _ -> Nothing
  (projectedRef, projectedBound) <-
    certifiedPacketConsumerProjection certificate
  guard
    ( typeBinderRefsSameIdentity sourceRef (bcbrAmbientRef certificate)
        && typeBinderRefsSameIdentity targetRef projectedRef
    )
  let matchingDeclarations =
        [ (ref, maybe TBottom tyToElab mbBound)
        | (ref, mbBound) <- candidates
        , typeBinderRefsSameIdentity ref projectedRef
        ]
          ++ [ (ref, bound)
             | (ref, bound) <- Map.toList ambientBindings
             , typeBinderRefsSameIdentity ref projectedRef
             ]
      declarationStateIsAdmissible (_, currentBound) =
        operationalEndpointTypesAgree
          currentBound
          (bcbrPreviousBound certificate)
          || operationalEndpointTypesAgree
            currentBound
            (tyToElab projectedBound)
  guard (not (null matchingDeclarations))
  guard (all declarationStateIsAdmissible matchingDeclarations)
  pure
    ( CertifiedPacketConsumerBodyProjection
        (bcbrAmbientRef certificate)
        (bcbrCompletedBound certificate)
        projectedRef
        (tyToElab projectedBound)
    )

-- | Attach the exact cross-identity body projection to the already checked
-- lambda plan.  The projection advances one ambient declaration for the
-- final lambda check; it never adds that declaration to the lambda's local
-- @Lambda(Gamma)@ binder spine.
attachCertifiedPacketConsumerBodyProjection
  :: CertifiedPacketConsumerBodyProjection
  -> ExactLambdaConstructionPlan
  -> Either ElabError ExactLambdaConstructionPlan
attachCertifiedPacketConsumerBodyProjection projection plan = do
  unless
    ( exactLambdaEndpointTypesAgree
        (exactLambdaConstructionBodyType plan)
        (TVarRef targetRef)
        && any
          (typeBinderRefsSameIdentity targetRef)
          ( freeTypeVarRefsType
              (exactLambdaConstructionPublishedType plan)
          )
        && not (typeBinderRefsSameIdentity sourceRef targetRef)
        && maybe True transitionAgrees existingTransition
        && isNothing
          (exactLambdaConstructionPacketBodyProjection plan)
    )
    ( Left
        ( ValidationFailed
            [ "cannot attach certified packet body projection to exact lambda plan"
            , "  projection: " ++ show projection
            , "  plan: " ++ show plan
            ]
        )
    )
  pure
    plan
      { exactLambdaConstructionAmbientBodyRefinement =
          Just transition
      , exactLambdaConstructionPacketBodyProjection =
          Just projection
      }
  where
    sourceRef =
      certifiedPacketConsumerBodyProjectionSourceRef projection
    sourceTy =
      certifiedPacketConsumerBodyProjectionSourceType projection
    targetRef =
      certifiedPacketConsumerBodyProjectionTargetRef projection
    targetTy =
      certifiedPacketConsumerBodyProjectionTargetType projection
    transition = (targetRef, sourceTy, targetTy)
    existingTransition =
      exactLambdaConstructionAmbientBodyRefinement plan
    transitionAgrees (existingRef, existingSource, existingTarget) =
      typeBinderRefsSameIdentity existingRef targetRef
        && exactLambdaEndpointTypesAgree existingSource sourceTy
        && exactLambdaEndpointTypesAgree existingTarget targetTy

projectCertifiedBodyConsumerBound
  :: Bool
  -> [(TypeBinderRef, Maybe BoundType)]
  -> BodyConsumerBoundRefinementCertificate
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
projectCertifiedBodyConsumerBound requireTarget binders certificate = do
  completedBinderBound <-
    if operationalEndpointTypesAgree completedBound TBottom
      then pure Nothing
      else
        Just
          <$> either
            ( \cause ->
                projectionFailure
                  "completed projection is not a legal flexible bound"
                  ["  cause: " ++ cause]
            )
            Right
            (elabToBound completedBound)
  case matchingBinders of
    []
      | requireTarget ->
          projectionFailure
            "certificate target is absent from the final root plan"
            []
      | otherwise -> pure binders
    [(_, currentBound)]
      | bodyConsumerBoundRefinementAcceptsDeclarationState
          certificate
          (maybe TBottom tyToElab currentBound) ->
          pure
            (replaceTarget completedBinderBound)
      | requireTarget
      , finalizedLocalDeclarationOwnsCompletion ->
          -- Packet placement can have installed a more-general provisional
          -- bound on this exact root slot before the lambda owner finishes.
          -- A finalized locally-emitted declaration is the construction
          -- authority for that slot: its exact owner/edge/exterior closure
          -- and dependency order are validated immediately after this pure
          -- projection.  Replace the planner candidate with the declaration
          -- the owner actually emitted instead of requiring the stale
          -- provisional shape to survive until root replay.
          pure
            (replaceTarget completedBinderBound)
      | otherwise ->
          projectionFailure
            "final binder does not carry the certified provisional bound"
            [ "  current bound: "
                ++ show (maybe TBottom tyToElab currentBound)
            ]
    matches ->
      projectionFailure
        "final root plan contains duplicate exact target declarations"
        ["  matches: " ++ show matches]
  where
    targetRef = bcbrAmbientRef certificate
    previousBound = bcbrPreviousBound certificate
    completedBound = bcbrCompletedBound certificate
    finalizedLocalDeclarationOwnsCompletion =
      bodyConsumerBoundRefinementOwnerFinalized certificate
        && case bcbrDeclarationAuthority certificate of
          BodyConsumerLocallyEmitted _ declarationBound ->
            operationalEndpointTypesAgree
              declarationBound
              completedBound
          _ -> False
    matchingBinders =
      [ binder
      | binder@(ref, _) <- binders
      , typeBinderRefsSameIdentity ref targetRef
      ]

    replaceTarget replacement =
      [ if typeBinderRefsSameIdentity ref targetRef
          then (ref, replacement)
          else binder
      | binder@(ref, _) <- binders
      ]

    projectionFailure :: String -> [String] -> Either ElabError a
    projectionFailure detail context =
      Left
        ( ValidationFailed
            ( [ "cannot project certified body-consumer bound into root construction",
                "  detail: " ++ detail,
                "  certificate: " ++ show certificate,
                "  target: " ++ show targetRef,
                "  provisional bound: " ++ show previousBound,
                "  completed bound: " ++ show completedBound,
                "  planned binders: " ++ show binders
              ]
                ++ context
            )
        )

-- | Alpha-copy lexical type declarations carried inside one refinement while
-- preserving free construction identities.  Exact lambda body copies are
-- scoped renames: an old identity can simultaneously name a body-local forall
-- and an enclosing operand supplied to 'InstApp'.  Replaying such a copy as a
-- global quotient would capture the latter and detach the certificate from the
-- published endpoint.
alphaRenameBodyConsumerBoundRefinementCertificate
  :: [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
alphaRenameBodyConsumerBoundRefinementCertificate renames certificate =
  certificate
    { bcbrDeclarationAuthority =
        alphaRenameDeclarationAuthority
          (bcbrDeclarationAuthority certificate)
    , bcbrConstructionProjection =
        alphaRenameConstructionProjection
          (bcbrConstructionProjection certificate)
    , bcbrPreviousBound = alphaRenameType (bcbrPreviousBound certificate)
    , bcbrCompletedBound = alphaRenameType (bcbrCompletedBound certificate)
    }
  where
    alphaRenameType = alphaRenameTypeBinderScopes renames

    alphaRenameDeclarationAuthority authority =
      case authority of
        BodyConsumerLocallyEmitted route declarationBound ->
          BodyConsumerLocallyEmitted
            (alphaRenameRoute route)
            (alphaRenameType declarationBound)
        BodyConsumerInheritedAmbient route declarationBound ->
          BodyConsumerInheritedAmbient
            (alphaRenameRoute route)
            (alphaRenameType declarationBound)
        BodyConsumerEnclosingAmbient route declarationBound ->
          BodyConsumerEnclosingAmbient
            (alphaRenameRoute route)
            (alphaRenameType declarationBound)
        BodyConsumerPendingOwnerEmission route progress ->
          BodyConsumerPendingOwnerEmission
            (alphaRenameRoute route)
            (alphaRenameOrdinaryOwnerProgress progress)
        BodyConsumerOrdinaryOwnerEmission route progress ->
          BodyConsumerOrdinaryOwnerEmission
            (alphaRenameRoute route)
            (alphaRenameOrdinaryOwnerProgress progress)
        BodyConsumerConsumedAtOwner route completedBound progress ->
          BodyConsumerConsumedAtOwner
            (alphaRenameRoute route)
            (alphaRenameType completedBound)
            (alphaRenameOrdinaryOwnerProgress progress)

    alphaRenameRoute route =
      route
        { bcrConstructionRef =
            alphaRenameProjectedConstructionRef
              (bcrConstructionRef route)
        , bcrOperatedType = alphaRenameType (bcrOperatedType route)
        , bcrConstructionOperatedType =
            alphaRenameType (bcrConstructionOperatedType route)
        }

    alphaRenameProjectedConstructionRef ref =
      case bcbrConstructionProjection certificate of
        DirectBodyConsumerConstructionProjection -> ref
        PacketOperatedBodyConsumerConstructionProjection _ ->
          renameRef ref

    alphaRenameConstructionProjection projection =
      case projection of
        DirectBodyConsumerConstructionProjection -> projection
        PacketOperatedBodyConsumerConstructionProjection bound ->
          PacketOperatedBodyConsumerConstructionProjection
            (renameBoundTypeBinderRefPayloads renames bound)

    alphaRenameOrdinaryOwnerProgress progress =
      case progress of
        OrdinaryOwnerEmissionProgress sources closures transitions ->
          OrdinaryOwnerEmissionProgress
            (map alphaRenameType sources)
            (map alphaRenameScopeClosure closures)
            (map alphaRenameTransition transitions)

    alphaRenameScopeClosure
      (CertifiedFutureOwnerCopiedScopeClosure declarations openBody) =
        CertifiedFutureOwnerCopiedScopeClosure
          [ (renameRef ref, alphaRenameType bound)
          | (ref, bound) <- declarations
          ]
          (alphaRenameType openBody)
    alphaRenameScopeClosure
      (CertifiedFutureOwnerResultClosureSource sourceEndpoint) =
        CertifiedFutureOwnerResultClosureSource
          (alphaRenameType sourceEndpoint)

    alphaRenameTransition
      ( CertifiedGammaBoundTransition
          ref
          sourceBound
          targetBound
          mbOriginRoute
        ) =
        CertifiedGammaBoundTransition
          ref
          (alphaRenameType sourceBound)
          (alphaRenameType targetBound)
          (alphaRenameRoute <$> mbOriginRoute)

    renameRef ref =
      fromMaybe
        ref
        ( snd
            <$> find
              (typeBinderRefsSameIdentity ref . fst)
              renames
        )

-- | Move the type payloads that depend on a lexically copied owner binder
-- while retaining the declaration route that the refinement completes.  A
-- body-local type abstraction may be copied from @a@ to @a'@ even though the
-- historical consumer declaration remains graph-owned @r@.  The bound of
-- @r@ must enter the copied dependency domain, but @r@ itself must not become
-- the body's fresh type abstraction.  This operation is therefore distinct
-- from both a scoped alpha-copy and a global construction quotient.
renameBodyConsumerBoundRefinementScopeDependencies
  :: [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
renameBodyConsumerBoundRefinementScopeDependencies =
  renameBodyConsumerBoundRefinementCertificateWith False

-- | Select the dependency half of a lexical body copy in the exact Gamma
-- where its consumer declaration is published.  The copy itself supplies the
-- only candidate identities; the current declaration bound decides which
-- presentation is active.  This never discovers a rename from type shape:
-- both refs come from the alpha-copy constructor, and the declaration is
-- joined by the certificate's exact target identity.
alignBodyConsumerBoundRefinementScopeDependencies
  :: Map.Map TypeBinderRef ElabType
  -> [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> Either ElabError BodyConsumerBoundRefinementCertificate
alignBodyConsumerBoundRefinementScopeDependencies =
  alignBodyConsumerBoundRefinementScopeDependenciesAt
    RetainClosedCopiedDependency

data CopiedDependencyAlignment
  = RetainClosedCopiedDependency
  | OpenClosedCopiedDependency

consumeBodyConsumerBoundRefinementScopeDependencies
  :: Map.Map TypeBinderRef ElabType
  -> [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> Either ElabError BodyConsumerBoundRefinementCertificate
consumeBodyConsumerBoundRefinementScopeDependencies =
  alignBodyConsumerBoundRefinementScopeDependenciesAt
    OpenClosedCopiedDependency

alignBodyConsumerBoundRefinementScopeDependenciesAt
  :: CopiedDependencyAlignment
  -> Map.Map TypeBinderRef ElabType
  -> [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> Either ElabError BodyConsumerBoundRefinementCertificate
alignBodyConsumerBoundRefinementScopeDependenciesAt alignment bindings renames certificate =
  foldM alignDependency certificate renames
  where
    alignDependency current rename@(sourceRef, targetRef)
      | typeBinderRefsSameIdentity sourceRef targetRef = pure current
      | not (mentionsRef sourceRef (bcbrCompletedBound current)) =
          alignClosedDependency current rename
      | mentionsRef targetRef (bcbrCompletedBound current) = pure current
      | otherwise =
          case matchingDeclarationBounds current of
            [] -> pure current
            [currentBound]
              | mentionsRef sourceRef currentBound
                  && not (mentionsRef targetRef currentBound) ->
                  pure current
              | mentionsRef targetRef currentBound
                  && not (mentionsRef sourceRef currentBound) ->
                  pure
                    ( renameBodyConsumerBoundRefinementScopeDependencies
                        [rename]
                        current
                    )
              | closesOpenCopiedDependency
                  rename
                  currentBound
                  (bcbrCompletedBound current) ->
                  -- The active declaration can close an otherwise free
                  -- copied dependency with @forall target@.  Its body is the
                  -- renamed open completion, so retain the source
                  -- certificate until the declaration is consumed.
                  pure
                    ( case alignment of
                        RetainClosedCopiedDependency -> current
                        OpenClosedCopiedDependency ->
                          recordOpenedCopiedScope
                            rename
                            currentBound
                            ( renameBodyConsumerBoundRefinementScopeDependencies
                                [rename]
                                current
                            )
                    )
              | [activeRef] <-
                  activeReachableCopiedDependencies
                    sourceRef
                    currentBound
                    (bcbrCompletedBound current) ->
                  -- The declaration can already be in a later lexical copy
                  -- than the edge currently visited by this fold.  Compose
                  -- the constructor-recorded copy path and move the proof
                  -- directly to that exact active identity.  This is still a
                  -- directional construction step: the declaration merely
                  -- selects one target that is reachable from the certified
                  -- source; its type shape contributes no rename.
                  pure
                    ( let aligned =
                            renameBodyConsumerBoundRefinementScopeDependencies
                              [(sourceRef, activeRef)]
                              current
                       in if
                            closesOpenCopiedDependency
                              (sourceRef, activeRef)
                              currentBound
                              (bcbrCompletedBound current)
                            then
                              recordOpenedCopiedScope
                                (sourceRef, activeRef)
                                currentBound
                                aligned
                            else aligned
                    )
              | activeRefs@(_ : _) <-
                  activeReachableCopiedDependencies
                    sourceRef
                    currentBound
                    (bcbrCompletedBound current) ->
                  alignmentFailure
                    current
                    rename
                    currentBound
                    ( "the active declaration selects multiple reachable copied dependency identities: "
                        ++ show activeRefs
                    )
              | anotherCopiedDependencyIsActive
                  sourceRef
                  targetRef
                  currentBound
                  (bcbrCompletedBound current) ->
                  -- Several sibling lexical scopes can copy the same source
                  -- identity.  A declaration selecting one sibling must not
                  -- be rejected while the fold visits another sibling's
                  -- route first; the matching route will move the proof when
                  -- it is visited.
                  pure current
              | operationalEndpointTypesAgree
                  currentBound
                  (bcbrPreviousBound current) ->
                  -- The dependency can be locally bound by the exact
                  -- previous endpoint (for example @forall a. a -> a@), so
                  -- neither copy identity is free in the declaration bound.
                  -- The certificate still proves that this is its source
                  -- side.  Retain the directional copy route until the
                  -- consumer transition selects the completed endpoint.
                  pure current
              | operationalEndpointTypesAgree currentBound TBottom ->
                  pure current
              | otherwise ->
                  alignmentFailure
                    current
                    rename
                    currentBound
                    "the active declaration does not select one copied dependency identity"
            currentBounds ->
              Left
                ( ValidationFailed
                    [ "cannot align a body-consumer refinement to its lexical dependency copy"
                    , "  detail: the exact consumer declaration is not unique"
                    , "  target: " ++ show (bcbrAmbientRef current)
                    , "  candidate rename: " ++ show rename
                    , "  matching bounds: " ++ show currentBounds
                    , "  certificate: " ++ show current
                    ]
                )

    matchingDeclarationBounds current =
      [ bound
      | (ref, bound) <- Map.toList bindings
      , typeBinderRefsSameIdentity ref (bcbrAmbientRef current)
      ]

    mentionsRef ref =
      any (typeBinderRefsSameIdentity ref) . freeTypeVarRefsType

    alignClosedDependency current rename@(sourceRef, targetRef)
      | not
          ( any
              (typeBinderRefsSameIdentity sourceRef)
              (typeBinderDeclarationRefs completedBound)
          ) =
          pure current
      | any
          (typeBinderRefsSameIdentity targetRef)
          ( typeBinderDeclarationRefs completedBound
              ++ freeTypeVarRefsType completedBound
          ) =
          pure current
      | otherwise =
          case matchingDeclarationBounds current of
            [currentBound]
              | currentBound == completedBound -> pure current
              | currentBound == copiedCompletedBound ->
                  -- A closed completion can carry its dependency only as a
                  -- forall declaration, so free-variable selection cannot
                  -- observe which lexical presentation is active.  The
                  -- constructor-provided route and exact identity-bearing
                  -- closed type select the copy.  Retain it while preparing
                  -- Gamma, then move the certificate only when that exact
                  -- consumer declaration is removed.
                  pure
                    ( case alignment of
                        RetainClosedCopiedDependency -> current
                        OpenClosedCopiedDependency ->
                          recordOpenedCopiedScope
                            rename
                            currentBound
                            ( renameBodyConsumerBoundRefinementScopeDependencies
                                [rename]
                                current
                            )
                    )
              | otherwise -> pure current
            _ -> pure current
      where
        completedBound = bcbrCompletedBound current
        copiedCompletedBound =
          renameTypeBinderRefPayloads [rename] completedBound

    closesOpenCopiedDependency
      (sourceRef, targetRef)
      currentBound
      completedBound =
        case currentBound of
          TForallRef closedRef _ closedBody ->
            typeBinderRefsSameIdentity closedRef targetRef
              && operationalEndpointTypesAgree
                closedBody
                ( renameTypeBinderRefPayloads
                    [(sourceRef, targetRef)]
                    completedBound
                )
          _ -> False

    anotherCopiedDependencyIsActive sourceRef targetRef currentBound completedBound =
      any
        ( \(otherSourceRef, otherTargetRef) ->
            typeBinderRefsSameIdentity sourceRef otherSourceRef
              && not
                ( typeBinderRefsSameIdentity
                    targetRef
                    otherTargetRef
                )
              && ( mentionsRef otherTargetRef currentBound
                    || closesOpenCopiedDependency
                      (otherSourceRef, otherTargetRef)
                      currentBound
                      completedBound
                 )
        )
        renames

    activeReachableCopiedDependencies sourceRef currentBound completedBound =
      distinctRefs
        [ candidateRef
        | (_, candidateRef) <- renames
        , Set.member
            (typeBinderRefIdentity candidateRef)
            (directedCopyTargets sourceRef)
        , mentionsRef candidateRef currentBound
            || closesOpenCopiedDependency
              (sourceRef, candidateRef)
              currentBound
              completedBound
        ]

    directedCopyTargets sourceRef = close initial
      where
        initial = Set.singleton (typeBinderRefIdentity sourceRef)
        close identities =
          let expanded = foldl' expand identities renames
           in if expanded == identities then identities else close expanded

        expand identities (copySourceRef, copyTargetRef)
          | Set.member
              (typeBinderRefIdentity copySourceRef)
              identities =
              Set.insert
                (typeBinderRefIdentity copyTargetRef)
                identities
          | otherwise = identities

    distinctRefs =
      foldl'
        ( \refs ref ->
            if any (typeBinderRefsSameIdentity ref) refs
              then refs
              else refs ++ [ref]
        )
        []

    -- The surrounding equality checks prove that @currentBound@ is either
    -- the exact copied completion or the exact closure of its open
    -- completion.  Preserve that construction event as one indivisible
    -- closure certificate before the copied binder is removed.
    recordOpenedCopiedScope (_, targetRef) currentBound current =
      case schemeBinderRefs currentScheme of
        [] -> current
        declarations
          | any
              (typeBinderRefsSameIdentity targetRef . fst)
              declarations ->
              recordBodyConsumerOwnerScopeClosure
                ( CertifiedFutureOwnerCopiedScopeClosure
                    [ (ref, maybe TBottom tyToElab mbBound)
                    | (ref, mbBound) <- declarations
                    ]
                    (schemeBody currentScheme)
                )
                current
          | otherwise -> current
      where
        currentScheme = schemeFromType currentBound

    alignmentFailure current rename currentBound detail =
      Left
        ( ValidationFailed
            [ "cannot align a body-consumer refinement to its lexical dependency copy"
            , "  detail: " ++ detail
            , "  target: " ++ show (bcbrAmbientRef current)
            , "  candidate rename: " ++ show rename
            , "  current declaration bound: " ++ show currentBound
            , "  certified completed bound: "
                ++ show (bcbrCompletedBound current)
            , "  certificate: " ++ show current
            ]
        )

-- | Rename every identity-bearing payload in one proof atomically with the
-- elaborated term that carries it.  This is a global construction quotient;
-- lexical alpha-copies use one of the two scope-specific operations above.
renameBodyConsumerBoundRefinementCertificate
  :: [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
renameBodyConsumerBoundRefinementCertificate =
  renameBodyConsumerBoundRefinementCertificateWith True

renameBodyConsumerBoundRefinementCertificateWith
  :: Bool
  -> [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
renameBodyConsumerBoundRefinementCertificateWith renameDeclarationRefs renames certificate =
  certificate
    { bcbrDeclarationAuthority =
        renameDeclarationAuthority
          (bcbrDeclarationAuthority certificate),
      bcbrAmbientRef = renameDeclarationRef (bcbrAmbientRef certificate),
      bcbrConstructionProjection =
        renameConstructionProjection
          (bcbrConstructionProjection certificate),
      bcbrPreviousBound =
        renameTypeBinderRefPayloads
          renames
          (bcbrPreviousBound certificate),
      bcbrCompletedBound =
        renameTypeBinderRefPayloads
          renames
          (bcbrCompletedBound certificate)
    }
  where
    renameDeclarationAuthority authority =
      case authority of
        BodyConsumerLocallyEmitted route declarationBound ->
          BodyConsumerLocallyEmitted
            (renameRoute route)
            (renameTypeBinderRefPayloads renames declarationBound)
        BodyConsumerInheritedAmbient route declarationBound ->
          BodyConsumerInheritedAmbient
            (renameRoute route)
            (renameTypeBinderRefPayloads renames declarationBound)
        BodyConsumerEnclosingAmbient route declarationBound ->
          BodyConsumerEnclosingAmbient
            (renameRoute route)
            (renameTypeBinderRefPayloads renames declarationBound)
        BodyConsumerPendingOwnerEmission route progress ->
          BodyConsumerPendingOwnerEmission
            (renameRoute route)
            (renameOrdinaryOwnerProgress progress)
        BodyConsumerOrdinaryOwnerEmission route progress ->
          BodyConsumerOrdinaryOwnerEmission
            (renameRoute route)
            (renameOrdinaryOwnerProgress progress)
        BodyConsumerConsumedAtOwner route completedBound progress ->
          BodyConsumerConsumedAtOwner
            (renameRoute route)
            (renameTypeBinderRefPayloads renames completedBound)
            (renameOrdinaryOwnerProgress progress)

    renameRoute route =
      route
        { -- The semantic endpoint is the immutable graph identity of
          -- 'bcrExteriorNode'.  A construction quotient moves only the
          -- routed declaration; renaming the semantic anchor would detach
          -- the certificate from the edge occurrence that created it.
          bcrConstructionRef =
            renameDeclarationRef (bcrConstructionRef route),
          bcrOperatedType =
            renameTypeBinderRefPayloads
              renames
              (bcrOperatedType route),
          bcrConstructionOperatedType =
            renameTypeBinderRefPayloads
              renames
              (bcrConstructionOperatedType route)
        }

    renameOrdinaryOwnerProgress progress =
      case progress of
        OrdinaryOwnerEmissionProgress sources closures transitions ->
          OrdinaryOwnerEmissionProgress
            (map (renameTypeBinderRefPayloads renames) sources)
            (map renameScopeClosure closures)
            (map renameTransition transitions)

    renameScopeClosure
      (CertifiedFutureOwnerCopiedScopeClosure declarations openBody) =
        CertifiedFutureOwnerCopiedScopeClosure
          [ ( renameRef ref
            , renameTypeBinderRefPayloads renames bound
            )
          | (ref, bound) <- declarations
          ]
          (renameTypeBinderRefPayloads renames openBody)
    renameScopeClosure
      (CertifiedFutureOwnerResultClosureSource sourceEndpoint) =
        CertifiedFutureOwnerResultClosureSource
          (renameTypeBinderRefPayloads renames sourceEndpoint)

    renameTransition
      ( CertifiedGammaBoundTransition
          ref
          sourceBound
          targetBound
          mbOriginRoute
        ) =
        CertifiedGammaBoundTransition
          (renameDeclarationRef ref)
          (renameTypeBinderRefPayloads renames sourceBound)
          (renameTypeBinderRefPayloads renames targetBound)
          (renameRoute <$> mbOriginRoute)

    renameConstructionProjection projection =
      case projection of
        DirectBodyConsumerConstructionProjection -> projection
        PacketOperatedBodyConsumerConstructionProjection bound ->
          PacketOperatedBodyConsumerConstructionProjection
            (renameBoundTypeBinderRefPayloads renames bound)

    renameRef ref =
      fromMaybe
        ref
        ( snd
            <$> find
              (typeBinderRefsSameIdentity ref . fst)
              renames
        )

    renameDeclarationRef
      | renameDeclarationRefs = renameRef
      | otherwise = id

-- | Checked result authority published by the exact nested application
-- constructor.  A prepared non-empty local Gamma proves the residual by
-- stripping its recorded emitted forall spine.  An application with no local
-- Gamma instead publishes its already typechecked final construction; that
-- path is admitted only when both local-binder collections are empty.
--
-- Keep the constructor private.  Enclosing applications may consume only
-- evidence built by one of the two validating smart constructors below, never
-- an arbitrary result type recovered after elaboration.
data NestedApplicationResidualCertificate =
  NestedApplicationResidualCertificate
    { narcOwner :: !LocalGammaOwner,
      narcResidualType :: !ElabType,
      narcProvenance :: !NestedApplicationResidualProvenance
    }
  deriving (Eq, Show)

data NestedApplicationResidualProvenance
  = NestedApplicationLocalGammaResidual
  | NestedApplicationZeroLocalConstructionResidual
  deriving (Eq, Show)

nestedApplicationResidualFromLocalGamma
  :: LocalGammaOwner
  -> LocalGammaConstructionCertificate
  -> Either ElabError NestedApplicationResidualCertificate
nestedApplicationResidualFromLocalGamma expectedOwner certificate = do
  unless
    (lgccOwner certificate == expectedOwner)
    (residualCertificateFailure "local Gamma certificate belongs to a different source owner")
  residualType <-
    maybe
      (residualCertificateFailure "local Gamma constructed type does not have its recorded emitted forall spine")
      pure
      (localGammaConstructionCertificateResidualType certificate)
  pure
    NestedApplicationResidualCertificate
      { narcOwner = expectedOwner,
        narcResidualType = residualType,
        narcProvenance = NestedApplicationLocalGammaResidual
      }
  where
    residualCertificateFailure
      :: String
      -> Either ElabError a
    residualCertificateFailure detail =
      Left
        ( ValidationFailed
            [ "invalid nested application local Gamma residual certificate",
              "  detail: " ++ detail,
              "  expected owner: " ++ show expectedOwner,
              "  local Gamma certificate: " ++ show certificate
            ]
        )

nestedApplicationResidualFromZeroLocalConstruction
  :: LocalGammaOwner
  -> LocalGammaOwner
  -> ElabType
  -> [TypeBinderRef]
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError NestedApplicationResidualCertificate
nestedApplicationResidualFromZeroLocalConstruction
  expectedOwner
  actualOwner
  constructedType
  locallyEmittedBinderRefs
  localBinderRoutes = do
    unless
      (actualOwner == expectedOwner)
      (residualCertificateFailure "final construction belongs to a different source owner")
    unless
      (null locallyEmittedBinderRefs)
      (residualCertificateFailure "zero-local construction claims locally emitted binders")
    unless
      (IntMap.null localBinderRoutes)
      (residualCertificateFailure "zero-local construction claims local binder routes")
    pure
      NestedApplicationResidualCertificate
        { narcOwner = expectedOwner,
          narcResidualType = constructedType,
          narcProvenance = NestedApplicationZeroLocalConstructionResidual
        }
  where
    residualCertificateFailure
      :: String
      -> Either ElabError a
    residualCertificateFailure detail =
      Left
        ( ValidationFailed
            [ "invalid nested application zero-local residual certificate",
              "  detail: " ++ detail,
              "  expected owner: " ++ show expectedOwner,
              "  actual owner: " ++ show actualOwner,
              "  constructed type: " ++ show constructedType,
              "  locally emitted binders: " ++ show locallyEmittedBinderRefs,
              "  local binder routes: " ++ show localBinderRoutes
            ]
        )

-- | Infer only the source applications fixed by an exact application
-- endpoint.  When that endpoint is an exact suffix beneath only vacuous
-- leading binders, an empty argument list is a positive result: those
-- abstractions are positional Gamma declarations and the caller must consume
-- them with their explicit N/bound applications.
inferExactTransportArguments
  :: (ElabType -> ElabType -> Bool)
  -> ElabScheme
  -> ElabType
  -> Maybe [ElabType]
inferExactTransportArguments endpointTypesAgree sourceScheme endpoint =
  if vacuousPrefixReachesEndpoint (schemeToType sourceScheme)
    then Just []
    else
      inferInstAppArgsFromSchemeRefsExact binders body endpoint
        <|> inferFromArrowDomain
  where
    binders = schemeBinderRefs sourceScheme
    body = schemeBody sourceScheme
    inferFromArrowDomain =
      case (body, endpoint) of
        (TArrow sourceDomain _, TArrow targetDomain _) ->
          inferInstAppArgsFromSchemeRefsExact
            binders
            sourceDomain
            targetDomain
        _ -> Nothing
    vacuousPrefixReachesEndpoint current
      | endpointTypesAgree current endpoint = True
      | TForallRef ref _ bodyTy <- current
      , not
          ( any
              (typeBinderRefsSameIdentity ref)
              (freeTypeVarRefsType bodyTy)
          ) =
          vacuousPrefixReachesEndpoint bodyTy
      | otherwise = False

-- | Select one graph-node construction route from recorded provenance.
-- A binder declared by this exact boundary shadows an ambient route.  When
-- neither route is boundary-local, the exact route already established in
-- the enclosing construction is the semantic authority; a scheme
-- substitution alone is only routing data.  Absence of either proof leaves
-- the conflict unresolved.
selectBoundaryConstructionRoute
  :: Maybe TypeBinderRef
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> TypeBinderRef
  -> Maybe TypeBinderRef
selectBoundaryConstructionRoute mbEstablishedAuthority boundaryLocalRefs enclosing local
  | typeBinderRefsSameIdentity enclosing local = Just enclosing
  | refIsBoundaryLocal local
  , not (refIsBoundaryLocal enclosing) =
      Just local
  | refIsBoundaryLocal enclosing
  , not (refIsBoundaryLocal local) =
      Just enclosing
  | Just establishedAuthority <- mbEstablishedAuthority
  , typeBinderRefsSameIdentity establishedAuthority enclosing =
      Just enclosing
  | Just establishedAuthority <- mbEstablishedAuthority
  , typeBinderRefsSameIdentity establishedAuthority local =
      Just local
  | otherwise = Nothing
  where
    refIsBoundaryLocal ref =
      any (typeBinderRefsSameIdentity ref) boundaryLocalRefs

-- | Merge declarations by semantic role, independently of input order.
-- Conflicting endpoint views are valid when a local Gamma declaration owns
-- the binder: an exact endpoint may specialize that declaration, and source
-- annotation closure may quantify identities that remain ambient to it.
-- Without local Gamma authority, endpoint-only declarations must agree.
mergeConstructionBinderBoundsByProvenance
  :: String
  -> [( ConstructionBinderBoundProvenance
      , (TypeBinderRef, Maybe BoundType)
      )]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
mergeConstructionBinderBoundsByProvenance context candidates =
  traverse selectAuthoritativeBinder (groupByIdentity candidates)
  where
    groupByIdentity = foldl insertCandidate []

    insertCandidate groups candidate@(_, (ref, _)) =
      case break (sameGroupIdentity ref) groups of
        (_, []) -> groups ++ [[candidate]]
        (before, group : after) -> before ++ ((group ++ [candidate]) : after)

    sameGroupIdentity ref group =
      case group of
        (_, (groupRef, _)) : _ -> typeBinderRefsSameIdentity ref groupRef
        [] -> False

    selectAuthoritativeBinder group = do
      localGamma <-
        validateOneRole ConstructionLocalGammaBound group
      sourceAnnotation <-
        validateOneRole ConstructionSourceAnnotationEndpoint group
      exactEndpoint <-
        validateOneRole ConstructionExactEndpoint group
      case localGamma of
        Just binder -> pure binder
        Nothing ->
          case (sourceAnnotation, exactEndpoint) of
            (Just sourceBinder@(_, sourceBound), Just (_, exactBound))
              | constructionBoundsAgree sourceBound exactBound ->
                  pure sourceBinder
              | otherwise ->
                  conflictingEndpointBounds group
            (Just binder, Nothing) -> pure binder
            (Nothing, Just binder) -> pure binder
            (Nothing, Nothing) ->
              Left
                ( ValidationFailed
                    [ "construction binder has no recorded provenance"
                    , "  context: " ++ context
                    , "  candidates: " ++ show group
                    ]
                )

    validateOneRole provenance group =
      case
          [ binder
          | (candidateProvenance, binder) <- group
          , candidateProvenance == provenance
          ]
        of
          [] -> pure Nothing
          binder@(_, bound) : remaining
            | all (constructionBoundsAgree bound . snd) remaining ->
                pure (Just binder)
            | otherwise ->
                Left
                  ( ValidationFailed
                      [ "one construction provenance disagrees on a binder bound"
                      , "  context: " ++ context
                      , "  provenance: " ++ show provenance
                      , "  candidates: " ++ show group
                      ]
                  )

    conflictingEndpointBounds group =
      Left
        ( ValidationFailed
            [ "construction endpoints disagree without a local Gamma authority"
            , "  context: " ++ context
            , "  candidates: " ++ show group
            ]
        )

    constructionBoundsAgree left right =
      let leftTy = maybe TBottom tyToElab left
          rightTy = maybe TBottom tyToElab right
       in alphaEqType leftTy rightTy || churchAwareEqType leftTy rightTy

-- | Refine one provisional ambient Gamma declaration from the checked
-- residual of the exact nested application that supplies an enclosing
-- argument occurrence.  The certificate has already established either the
-- child's emitted forall spine or its positive zero-local final construction.
inheritNestedApplicationResidualAuthority
  :: LocalGammaOwner
  -> NestedApplicationResidualCertificate
  -> GeneralizationRequirements
  -> Either ElabError GeneralizationRequirements
inheritNestedApplicationResidualAuthority expectedOwner certificate requirements = do
  unless
    (narcOwner certificate == expectedOwner)
    (inheritanceFailure "certificate belongs to a different source owner")
  let residualType = narcResidualType certificate
  requirement <-
    case matchingRequirements of
      [matched] -> pure matched
      matches ->
        inheritanceFailure
          ( "nested application result matched "
              ++ show (length matches)
              ++ " outer requirements, expected exactly one"
          )
  unless
    (requiredGammaPlacementIsLocal (rgbPlacement requirement))
    (inheritanceFailure "matching outer requirement is not owned by the current construction")
  resultAuthority <-
    requireAuthority "nested application result" (lgoTermNode expectedOwner)
  exteriorAuthority <-
    requireAuthority "outer requirement exterior" (rgbExteriorNode requirement)
  let exactRef = agaExactRef resultAuthority
      routeKeys =
        IntSet.fromList
          ( getNodeId (rgbExteriorNode requirement)
              : map getNodeId (NonEmpty.toList (rgbResultRoots requirement))
          )
      routedAuthorities =
        [ (nodeKey, authority)
        | nodeKey <- IntSet.toList routeKeys
        , Just authority <-
            [IntMap.lookup nodeKey (grAmbientGammaAuthorities requirements)]
        ]
      conflictingRoutes =
        [ (NodeId nodeKey, authority)
        | (nodeKey, authority) <- routedAuthorities
        , not (typeBinderRefsSameIdentity (agaExactRef authority) exactRef)
        ]
      incompatibleBounds =
        [ (NodeId nodeKey, agaBound authority)
        | (nodeKey, authority) <- routedAuthorities
        , agaBound authority /= TBottom
        , not (typesAgree (agaBound authority) residualType)
        ]
  unless
    (typeBinderRefsSameIdentity (agaExactRef exteriorAuthority) exactRef)
    (inheritanceFailure "argument result and requirement exterior name different ambient declarations")
  unless
    ( typeBinderRefIdentity exactRef
        == typeBinderIdentityFromNode (rgbExteriorNode requirement)
    )
    (inheritanceFailure "ambient declaration identity is not the exact requirement exterior")
  unless
    (any (typeBinderRefsSameIdentity exactRef) (grAmbientBinderRefs requirements))
    (inheritanceFailure "ambient declaration is absent from the enclosing binder scope")
  unless
    (typesAgree residualType (rgbOperatedType requirement))
    (inheritanceFailure "checked nested residual disagrees with the outer operated bound")
  unless
    (null conflictingRoutes)
    (inheritanceFailure ("requirement routes name other ambient declarations: " ++ show conflictingRoutes))
  unless
    (null incompatibleBounds)
    (inheritanceFailure ("requirement routes have incompatible established bounds: " ++ show incompatibleBounds))
  pure
    requirements
      { grAmbientGammaAuthorities =
          IntMap.mapWithKey
            ( \nodeKey authority ->
                if IntSet.member nodeKey routeKeys
                    && typeBinderRefsSameIdentity (agaExactRef authority) exactRef
                    && agaBound authority == TBottom
                  then authority {agaBound = residualType}
                  else authority
            )
            (grAmbientGammaAuthorities requirements)
      }
  where
    matchingRequirements =
      [ requirement
      | requirement <- grRequiredGammaBinders requirements
      , lgoTermNode expectedOwner
          `elem` NonEmpty.toList (rgbResultRoots requirement)
      ]

    requireAuthority label node =
      case
          IntMap.lookup
            (getNodeId node)
            (grAmbientGammaAuthorities requirements)
        of
          Just authority -> pure authority
          Nothing ->
            inheritanceFailure
              (label ++ " has no exact ambient Gamma route at " ++ show node)

    typesAgree = operationalEndpointTypesAgree

    inheritanceFailure
      :: String
      -> Either ElabError a
    inheritanceFailure detail =
      Left
        ( ValidationFailed
            [ "invalid nested application residual Gamma authority"
            , "  detail: " ++ detail
            , "  expected owner: " ++ show expectedOwner
            , "  certificate: " ++ show certificate
            , "  requirements: " ++ show requirements
            ]
        )

-- | Refine the owner-selected construction requirements and the independently
-- prepared edge-local replay requirements from one checked nested-application
-- certificate.  Replay planning is not allowed to resolve a disagreement by
-- precedence: every matching route must name the same exact declaration and
-- may only replace a provisional bottom bound with the checked residual.
inheritNestedApplicationResidualReplayAuthority
  :: LocalGammaOwner
  -> NestedApplicationResidualCertificate
  -> GeneralizationRequirements
  -> GeneralizationRequirements
  -> Either
      ElabError
      (GeneralizationRequirements, GeneralizationRequirements)
inheritNestedApplicationResidualReplayAuthority
  expectedOwner
  certificate
  ownerRequirements
  replayRequirements = do
    refinedOwnerRequirements <-
      inheritNestedApplicationResidualAuthority
        expectedOwner
        certificate
        ownerRequirements
    ownerRequirement <-
      case matchingOwnerRequirements of
        [matched] -> pure matched
        matches ->
          replayInheritanceFailure
            ( "validated owner result matched "
                ++ show (length matches)
                ++ " requirements, expected exactly one"
            )
    refinedAuthority <-
      case
          IntMap.lookup
            (getNodeId (lgoTermNode expectedOwner))
            (grAmbientGammaAuthorities refinedOwnerRequirements)
        of
          Just authority -> pure authority
          Nothing ->
            replayInheritanceFailure
              "validated owner result has no refined ambient Gamma authority"
    let routeKeys =
          IntSet.fromList
            ( getNodeId (rgbExteriorNode ownerRequirement)
                : map
                  getNodeId
                  (NonEmpty.toList (rgbResultRoots ownerRequirement))
            )
    replayAuthorities <-
      foldM
        (refineReplayRoute refinedAuthority)
        (grAmbientGammaAuthorities replayRequirements)
        (IntSet.toList routeKeys)
    pure
      ( refinedOwnerRequirements
      , replayRequirements
          { grAmbientGammaAuthorities = replayAuthorities
          }
      )
  where
    matchingOwnerRequirements =
      [ requirement
      | requirement <- grRequiredGammaBinders ownerRequirements
      , lgoTermNode expectedOwner
          `elem` NonEmpty.toList (rgbResultRoots requirement)
      ]

    refineReplayRoute refinedAuthority authorities nodeKey =
      case IntMap.lookup nodeKey authorities of
        Nothing -> pure authorities
        Just replayAuthority
          | not
              ( typeBinderRefsSameIdentity
                  (agaExactRef replayAuthority)
                  (agaExactRef refinedAuthority)
              ) ->
              replayInheritanceFailure
                ( "edge-local replay route names a different ambient declaration at "
                    ++ show (NodeId nodeKey)
                )
          | agaBound replayAuthority == TBottom ->
              pure
                ( IntMap.insert
                    nodeKey
                    replayAuthority
                      { agaBound = agaBound refinedAuthority
                      }
                    authorities
                )
          | operationalEndpointTypesAgree
              (agaBound replayAuthority)
              (agaBound refinedAuthority) ->
              pure authorities
          | otherwise ->
              replayInheritanceFailure
                ( "edge-local replay route has an incompatible established bound at "
                    ++ show (NodeId nodeKey)
                    ++ ": "
                    ++ show (agaBound replayAuthority)
                )

    replayInheritanceFailure
      :: String
      -> Either ElabError a
    replayInheritanceFailure detail =
      Left
        ( ValidationFailed
            [ "invalid nested application residual replay authority"
            , "  detail: " ++ detail
            , "  expected owner: " ++ show expectedOwner
            , "  certificate: " ++ show certificate
            , "  owner requirements: " ++ show ownerRequirements
            , "  replay requirements: " ++ show replayRequirements
            ]
        )

-- | The source of truth for one constructed lambda parameter.  Ordinary
-- lambdas may refine an existing Gamma binder from the enclosing construction
-- domain.  An exact source lambda owns the complete parameter type instead:
-- its graph root is not a type-variable occurrence and therefore cannot
-- authorize quotienting the whole domain into one outward Gamma binder.
data LambdaParamBoundaryAuthority
  = ConstructedLambdaParamBoundary !ElabType
  | ExactSourceLambdaParamBoundary
      !ElabType
      !(Maybe ElabType)
      ![TypeBinderRef]
  | ExactApplicationLambdaParamBoundary !ElabType
  deriving (Eq, Show)

-- | Construction proof that an exact lambda parameter node denotes its
-- source-owned type.  The graph node remains the stable presolution endpoint;
-- the exact type is the value installed before the lambda body is checked.
-- Enclosing Gamma completion may replay this substitution, but ordinary
-- construction aliases may not: an exact parameter root is not itself a
-- type-variable occurrence.
data LambdaParamBoundaryCertificate =
  LambdaParamBoundaryCertificate
    { lpbcParameterNode :: !NodeId,
      lpbcConstructedType :: !ElabType,
      lpbcSourceBinderRefs :: ![TypeBinderRef]
    }
  deriving (Eq, Show)

lambdaParamBoundaryConstructedType
  :: LambdaParamBoundaryCertificate
  -> ElabType
lambdaParamBoundaryConstructedType = lpbcConstructedType

-- | Immutable source declarations carried by an exact source parameter.
-- These identities are recorded when the source boundary is installed; a
-- later owner may move their abstraction across an enclosing value lambda,
-- but must not rediscover that authority from the parameter type's shape.
lambdaParamBoundarySourceBinderRefs
  :: LambdaParamBoundaryCertificate
  -> [TypeBinderRef]
lambdaParamBoundarySourceBinderRefs = lpbcSourceBinderRefs

data LambdaParamBoundaryInstallation = LambdaParamBoundaryInstallation
  { lambdaParamBoundaryType :: !ElabType,
    lambdaParamBoundaryLocalBinderIdentities :: !(Set.Set TypeBinderIdentity),
    lambdaParamBoundaryGammaAliases :: !(IntMap.IntMap TypeBinderRef),
    lambdaParamBoundaryTypeBindings :: !(Map.Map TypeBinderRef ElabType),
    lambdaParamBoundaryCertificate
      :: !LambdaParamBoundaryCertificate
  }
  deriving (Eq, Show)

-- | Project a recursively checked type through the exact graph-occurrence
-- routes carried by the construction scheme that will consume it.  These
-- routes are the typed quotient authority produced while the scheme is
-- built; using them here keeps a child result and its enclosing declaration
-- in one identity domain before a completion certificate is issued.
completeSchemeInfoRouteType :: SchemeInfo -> ElabType -> ElabType
completeSchemeInfoRouteType schemeInfo ty0 =
  foldl' project ty0 (freeTypeVarRefsType ty0)
  where
    project ty ref =
      case
          typeBinderRefNode ref
            >>= \node ->
              IntMap.lookup
                (getNodeId node)
                (schemeInfoBinderRefSubst schemeInfo)
        of
        Just routedRef
          | not (typeBinderRefsSameIdentity ref routedRef) ->
              substTypeCaptureRef ref (TVarRef routedRef) ty
        _ -> ty

-- | Replay exact parameter construction while comparing a graph-planned
-- Gamma bound with the recursively checked child that completed it.
completeLambdaParamBoundaryType
  :: [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryType =
  completeLambdaParamBoundaryTypeWithRootClosure False

-- | Replay exact parameter construction without closing source identities
-- that are already lexical in the recursively checked owner.  The caller
-- supplies those identities from owner-final provenance; this function never
-- infers lexical scope from the candidate type itself.
completeLambdaParamBoundaryTypeInScope
  :: [TypeBinderRef]
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryTypeInScope lexicalRefs =
  completeLambdaParamBoundaryTypeWithRootClosureInScope
    False
    lexicalRefs

-- | Complete a type stored as a Gamma declaration bound.  Unlike a general
-- endpoint, the root of a bound is itself in the lexical position authorized
-- by the certificate.  It may therefore be the opened body of the exact
-- parameter scheme and must be closed there, before an outgoing computation is
-- selected.
completeLambdaParamBoundaryBound
  :: [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryBound =
  completeLambdaParamBoundaryTypeWithRootClosure True

-- | Close an exact source parameter root after graph-to-source projection has
-- represented that root by one of the parameter scheme's lexical binders.
-- The route at the certified parameter node is the positive authority for
-- this substitution.  Merely finding a same-shaped free source binder is not
-- enough: source binders can occur in unrelated annotations, and their
-- identity must not be promoted to a complete parameter scheme without the
-- graph-occurrence sidecar that selected it.
--
-- First perform the ordinary opened-body completion.  This preserves the
-- important case where the whole @a -> a@ body denotes one
-- @forall a. a -> a@ value.  Any remaining free occurrence of the routed
-- source root then denotes the certified graph parameter occurrence and is
-- substituted by that complete value type.
completeLambdaParamBoundarySourceRootBound
  :: [LambdaParamBoundaryCertificate]
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
completeLambdaParamBoundarySourceRootBound certificates sourceRoutes ty0 =
  foldl' completeCertificate ty0 certificates
  where
    completeCertificate candidate certificate =
      case
          IntMap.lookup
            (getNodeId (lpbcParameterNode certificate))
            sourceRoutes
        of
          Just sourceRootRef
            | any
                (typeBinderRefsSameIdentity sourceRootRef)
                constructedBinderRefs ->
                substTypeCaptureRef
                  sourceRootRef
                  constructedType
                  openedBodyCompleted
          _ -> openedBodyCompleted
      where
        constructedType = lpbcConstructedType certificate
        constructedBinderRefs =
          map fst (schemeBinderRefs (schemeFromType constructedType))
        openedBodyCompleted =
          completeLambdaParamBoundaryTypeWithSiblingBlocking
            False
            True
            []
            [certificate]
            candidate

-- | Construct the completed presentation of a graph requirement whose exact
-- source lambda parameter has already been installed.  Reification can leave
-- the parameter's source forall at the front of the requirement,
--
--   @forall a. (a -> a) -> r@,
--
-- while the checked parameter boundary constructs
--
--   @(forall a. a -> a) -> r@.
--
-- The occurrence route is essential positive evidence: it proves that the
-- leading source declaration belongs to this exact parameter node, rather
-- than to another same-shaped source annotation.  Return 'Nothing' unless at
-- least one certified parameter boundary performs a real construction step.
constructLambdaParamBoundaryRequirement
  :: [LambdaParamBoundaryCertificate]
  -> IntMap.IntMap TypeBinderRef
  -> TypeBinderRef
  -> ElabType
  -> Maybe ElabType
constructLambdaParamBoundaryRequirement certificates occurrenceRoutes declarationRef ty0 = do
  guard constructedAny
  pure completedType
  where
    (constructedAny, completedType) =
      foldl' completeCertificate (False, ty0) certificates

    completeCertificate (alreadyConstructed, candidate) certificate =
      case
          IntMap.lookup
            (getNodeId (lpbcParameterNode certificate))
            occurrenceRoutes
        of
          Just occurrenceRef
            | any
                (typeBinderRefsSameIdentity occurrenceRef . fst)
                (schemeBinderRefs (schemeFromType constructedType)) ->
                case
                    moveExactLambdaParameterDeclarations
                      certificate
                      declarationRef
                      candidate
                  of
                    Just completed ->
                      ( True
                      , completeLambdaParamBoundaryDeclarationBound
                          [certificate]
                          declarationRef
                          completed
                      )
                    Nothing -> (alreadyConstructed, candidate)
          _ -> (alreadyConstructed, candidate)
      where
        constructedType = lpbcConstructedType certificate

-- | Complete a declaration bound without consuming a parameter certificate
-- whose endpoint depends on the declaration currently being emitted.  Such
-- an endpoint is available only after the owner publishes this binder; using
-- it in the binder's own bound would turn an acyclic Figure 15.3.5 Gamma
-- construction into an illegal recursive declaration.
completeLambdaParamBoundaryDeclarationBound
  :: [LambdaParamBoundaryCertificate]
  -> TypeBinderRef
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryDeclarationBound certificates declarationRef =
  completeLambdaParamBoundaryDeclarationBoundInScope
    certificates
    []
    declarationRef

-- | Complete a declaration bound while preserving the lexical scope of the
-- binders that precede it in the same forall spine.  An opened parameter body
-- such as @a -> a@ must not be closed back to @forall a. a -> a@ when @a@ is
-- already bound by an earlier declaration in that spine.
completeLambdaParamBoundaryDeclarationBoundInScope
  :: [LambdaParamBoundaryCertificate]
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryDeclarationBoundInScope
  certificates
  lexicalRefs
  declarationRef
  ty =
    let applicableCertificates =
          [ certificate
          | certificate <- certificates
          , not
              ( any
                  (typeBinderRefsSameIdentity declarationRef)
                  (freeTypeVarRefsType (lpbcConstructedType certificate))
              )
          ]
        completed =
          completeLambdaParamBoundaryBoundInScope
            lexicalRefs
            applicableCertificates
            ty
     in completed

-- Move the exact source declarations owned by one parameter certificate from
-- an operated forall spine into that parameter's arrow domain.  This is the
-- common Figure 15.3.5 construction step used both while completing an owner
-- packet and while publishing a direct ambient requirement.
moveExactLambdaParameterDeclarations
  :: LambdaParamBoundaryCertificate
  -> TypeBinderRef
  -> ElabType
  -> Maybe ElabType
moveExactLambdaParameterDeclarations certificate declarationRef candidate = do
  guard (not (null constructedBinders))
  guard
    ( allDistinctRefs (map fst constructedBinders)
        && allDistinctRefs (map fst candidateBinders)
    )
  matchedBinders <-
    traverse
      (matchingCandidateBinder candidateBinders)
      constructedBinders
  let movedRefs = map fst matchedBinders
      retainedBinders =
        [ binder
        | binder@(ref, _) <- candidateBinders
        , not (ref `anyRefIn` movedRefs)
        ]
  guard
    ( all
        (not . boundMentionsAny movedRefs . snd)
        retainedBinders
    )
  (openedDomain, codomain) <-
    case schemeBody candidateScheme of
      TArrow domain body -> Just (domain, body)
      _ -> Nothing
  guard
    ( not
        ( any
            (`anyRefIn` freeTypeVarRefsType codomain)
            movedRefs
        )
    )
  let completedDomain =
        completeLambdaParamBoundaryDeclarationBound
          [certificate]
          declarationRef
          openedDomain
  guard
    ( operationalEndpointTypesAgree
        completedDomain
        constructedType
    )
  pure
    ( schemeToType
        ( mkElabSchemeWithRefs
            retainedBinders
            (TArrow completedDomain codomain)
        )
    )
  where
    constructedType = lpbcConstructedType certificate
    constructedBinders =
      schemeBinderRefs (schemeFromType constructedType)
    candidateScheme = schemeFromType candidate
    candidateBinders = schemeBinderRefs candidateScheme

    matchingCandidateBinder binders (expectedRef, expectedBound) = do
      [candidateBinder@(_, candidateBound)] <-
        pure
          [ binder
          | binder@(candidateRef, _) <- binders
          , typeBinderRefsSameIdentity candidateRef expectedRef
          ]
      guard (binderBoundsAgree expectedBound candidateBound)
      pure candidateBinder

    binderBoundsAgree Nothing Nothing = True
    binderBoundsAgree (Just left) (Just right) =
      operationalEndpointTypesAgree (tyToElab left) (tyToElab right)
    binderBoundsAgree _ _ = False

    boundMentionsAny _ Nothing = False
    boundMentionsAny refs (Just bound) =
      any (`anyRefIn` freeTypeVarRefsType (tyToElab bound)) refs

    ref `anyRefIn` refs = any (typeBinderRefsSameIdentity ref) refs

    allDistinctRefs [] = True
    allDistinctRefs (ref : refs) =
      not (ref `anyRefIn` refs) && allDistinctRefs refs

-- | Move an exact source parameter's declarations from the packet-operated
-- spine into that lambda parameter before the enclosing Gamma bound is
-- certified.  Reification can expose the opened parameter body as
-- @forall a. (a -> a) -> r@, while the recursively constructed source lambda
-- has the paper's exact domain @(forall a. a -> a)@.  The parameter-boundary
-- certificate is the authority for that lexical move; without it, an outer
-- forall must never be pushed through an arrow.
completeLambdaParamBoundaryOperatedType
  :: [LambdaParamBoundaryCertificate]
  -> IntMap.IntMap TypeBinderRef
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryOperatedType certificates operatedRoutes projectedRefs declarationRef expectedType =
  \candidate -> foldl' completeCertificate candidate certificates
  where
    completeCertificate candidate certificate
      | operationalEndpointTypesAgree candidate expectedType =
          -- The enclosing owner has already published the exact completed
          -- endpoint.  Replaying this parameter certificate would move its
          -- source forall a second time and turn a completed paper g g bound
          -- back into an opened packet presentation.
          candidate
      | any
          (typeBinderRefsSameIdentity declarationRef)
          (freeTypeVarRefsType constructedType) =
          candidate
      | otherwise =
          completeLambdaParamBoundaryDeclarationBound
            [certificate]
            declarationRef
            ( fromMaybe
                candidate
                ( movedParameterDeclarations False
                    <|> exactBoundClosingMove
                    <|> paperSelfApplicationResultMove
                    <|> openedParameterProxyMove
                )
            )
      where
        constructedType = lpbcConstructedType certificate
        constructedBinders =
          schemeBinderRefs (schemeFromType constructedType)
        candidateScheme = schemeFromType candidate
        candidateBinders = schemeBinderRefs candidateScheme
        exactBoundClosingMove = do
          completed <- movedParameterDeclarations True
          guard
            ( operationalEndpointTypesAgree
                completed
                expectedType
                || projectedDeclarationsOpenToExpected completed
            )
          pure completed

        -- The parameter move can finish declarations already owned by the
        -- lexical Gamma before the caller projects those declarations out of
        -- the standalone packet view:
        --
        --   forall (b >= sigma). sigma -> b
        --     <= sigma -> b.
        --
        -- The exact projected identities, rather than the final arrow shape,
        -- are the authority for these remaining opening steps.  The caller
        -- supplies the same certified projection set that it uses after this
        -- construction, so a packet-local binder cannot be discarded merely
        -- because doing so happens to expose the expected endpoint.
        projectedDeclarationsOpenToExpected completed =
          operationalEndpointTypesAgree
            (projectLeadingDeclarations completed)
            expectedType

        projectLeadingDeclarations ty =
          case ty of
            TForallRef ref _ body
              | any
                  (typeBinderRefsSameIdentity ref)
                  projectedRefs ->
                  projectLeadingDeclarations body
            _ -> ty

        -- Section 15.3.8's annotated self-application can reach a lambda
        -- boundary in either of two partially completed packet
        -- presentations:
        --
        --   forall a. forall (b >= a -> a).
        --     (forall a. a -> a) -> a
        --
        -- or
        --
        --   forall a. forall (b >= a -> a). a -> b
        --
        -- The first has closed the exact parameter but still exposes its
        -- source result; the second has selected the flexible result but has
        -- not closed the parameter.  In both cases the checked application
        -- owns @b@ and constructs the same paper endpoint
        --
        --   forall (b >= forall a. a -> a).
        --     (forall a. a -> a) -> b
        --
        -- Build that intermediate endpoint while all construction authority
        -- is present.  A later explicit instantiation may validly specialize
        -- @b@ to its bound, so this boundary move must not require the
        -- intermediate endpoint to equal the consumer's final checked type.
        -- This is deliberately narrower than a type-shape rewrite: the
        -- packet must carry the exact source binder from its parameter
        -- boundary certificate, and the retained result declaration must be
        -- an identity-routed graph result with precisely the opened parameter
        -- body as its lower bound.  The ordinary construction checker then
        -- proves any transition from this completed intermediate type to the
        -- expected owner endpoint.
        paperSelfApplicationResultMove = do
          [(constructedRef, constructedBound)] <-
            pure constructedBinders
          [(openedRef, openedBound), (resultRef, Just openedResultBound)] <-
            pure candidateBinders
          guard
            ( typeBinderRefsSameIdentity openedRef constructedRef
                && binderBoundsAgree openedBound constructedBound
            )
          guard
            ( not
                (typeBinderRefsSameIdentity resultRef openedRef)
                && any
                  (typeBinderRefsSameIdentity resultRef)
                  (IntMap.elems operatedRoutes)
            )
          let constructedBody =
                schemeBody (schemeFromType constructedType)
          guard
            ( operationalEndpointTypesAgree
                (tyToElab openedResultBound)
                constructedBody
            )
          (openedDomain, openedCodomain) <-
            case schemeBody candidateScheme of
              TArrow domain codomain -> Just (domain, codomain)
              _ -> Nothing
          openedCodomainRef <-
            case openedCodomain of
              TVarRef ref -> Just ref
              _ -> Nothing
          let closedParameterWithOpenedSourceResult =
                operationalEndpointTypesAgree openedDomain constructedType
                  && typeBinderRefsSameIdentity openedCodomainRef openedRef
              openedParameterWithFlexibleResult =
                case
                    ( openedDomain
                    , IntMap.lookup
                        (getNodeId (lpbcParameterNode certificate))
                        operatedRoutes
                    )
                  of
                    (TVarRef openedDomainRef, Just routedParameterRef) ->
                      typeBinderRefsSameIdentity
                        openedDomainRef
                        routedParameterRef
                        && routedParameterRef
                          `anyRefIn` map fst constructedBinders
                        && typeBinderRefsSameIdentity
                          openedCodomainRef
                          resultRef
                    _ -> False
          guard
            ( closedParameterWithOpenedSourceResult
                || openedParameterWithFlexibleResult
            )
          completedBound <-
            either (const Nothing) Just (elabToBound constructedType)
          pure
            ( schemeToType
                ( mkElabSchemeWithRefs
                    [(resultRef, Just completedBound)]
                    (TArrow constructedType (TVarRef resultRef))
                )
            )

        openedParameterProxyMove = do
          let constructedRefs = map fst constructedBinders
              candidateRefs = map fst candidateBinders
              constructedBody = schemeBody (schemeFromType constructedType)
          guard (not (null constructedRefs))
          guard
            ( all
                (\constructedRef -> not (constructedRef `anyRefIn` candidateRefs))
                constructedRefs
            )
          routedParameterRef <-
            IntMap.lookup
              (getNodeId (lpbcParameterNode certificate))
              operatedRoutes
          guard (routedParameterRef `anyRefIn` constructedRefs)
          (openedDomain, codomain) <-
            case schemeBody candidateScheme of
              TArrow domain body -> Just (domain, body)
              _ -> Nothing
          proxyRef <-
            case openedDomain of
              TVarRef ref -> Just ref
              _ -> Nothing
          [(_, Just openedProxyBound)] <-
            pure
              [ binder
              | binder@(ref, Just bound) <- candidateBinders
              , typeBinderRefsSameIdentity ref proxyRef
              , operationalEndpointTypesAgree
                  (tyToElab bound)
                  constructedBody
              ]
          guard
            ( all
                (`anyRefIn` freeTypeVarRefsType (tyToElab openedProxyBound))
                constructedRefs
            )
          guard
            ( not
                ( any
                    (`anyRefIn` freeTypeVarRefsType codomain)
                    constructedRefs
                )
            )
          completedBinders <- traverse completeRetainedBinder candidateBinders
          [(_, Just completedProxyBound)] <-
            pure
              [ binder
              | binder@(ref, _) <- completedBinders
              , typeBinderRefsSameIdentity ref proxyRef
              ]
          guard
            ( operationalEndpointTypesAgree
                (tyToElab completedProxyBound)
                constructedType
            )
          guard
            ( all
                (not . boundMentionsAny constructedRefs . snd)
                completedBinders
            )
          let completed =
                schemeToType
                  ( mkElabSchemeWithRefs
                      completedBinders
                      (TArrow constructedType codomain)
                  )
          guard (operationalEndpointTypesAgree completed expectedType)
          pure completed
        movedParameterDeclarations False =
          moveExactLambdaParameterDeclarations
            certificate
            declarationRef
            candidate
        movedParameterDeclarations closeRetainedBounds = do
          guard (not (null constructedBinders))
          guard
            ( allDistinctRefs (map fst constructedBinders)
                && allDistinctRefs (map fst candidateBinders)
            )
          matchedBinders <-
            traverse
              (matchingCandidateBinder candidateBinders)
              constructedBinders
          let movedRefs = map fst matchedBinders
              retainedBinders0 =
                [ binder
                | binder@(ref, _) <- candidateBinders
                , not
                    (any (typeBinderRefsSameIdentity ref) movedRefs)
                ]
          retainedBinders <-
            if closeRetainedBounds
              then traverse completeRetainedBinder retainedBinders0
              else do
                guard
                  ( all
                      (not . boundMentionsAny movedRefs . snd)
                      retainedBinders0
                  )
                pure retainedBinders0
          guard
            ( all
                (not . boundMentionsAny movedRefs . snd)
                retainedBinders
            )
          (openedDomain, codomain) <-
            case schemeBody candidateScheme of
              TArrow domain body -> Just (domain, body)
              _ -> Nothing
          guard
            ( not
                ( any
                    (`anyRefIn` freeTypeVarRefsType codomain)
                    movedRefs
                )
            )

          let directlyCompletedDomain =
                completeLambdaParamBoundaryDeclarationBound
                  [certificate]
                  declarationRef
                  openedDomain
          completedDomain <-
            if
                operationalEndpointTypesAgree
                  directlyCompletedDomain
                  constructedType
              then pure directlyCompletedDomain
              else
                completedParameterProxyDomain
                  closeRetainedBounds
                  movedRefs
                  retainedBinders0
                  retainedBinders
                  openedDomain
          pure
            ( schemeToType
                ( mkElabSchemeWithRefs
                    retainedBinders
                    (TArrow completedDomain codomain)
                )
            )

        -- A flexible application result can also stand in the parameter
        -- position while its lower bound retains the opened exact parameter
        -- body:
        --
        --   forall a. forall (b >= a -> a). b -> b
        --
        -- Closing the moved @a@ declaration first yields the paper's exact
        -- result declaration @b >= forall a. a -> a@.  The parameter is then
        -- the complete source type certified at the lambda boundary, while
        -- the codomain remains the same flexible @b@.  Require the same
        -- identity-bearing declaration before and after bound completion;
        -- the expected endpoint merely validates the completed construction.
        completedParameterProxyDomain
          closeRetainedBounds
          movedRefs
          retainedBinders0
          retainedBinders
          openedDomain = do
            guard closeRetainedBounds
            proxyRef <-
              case openedDomain of
                TVarRef ref -> Just ref
                _ -> Nothing
            [(_, Just openedProxyBound)] <-
              pure
                [ binder
                | binder@(ref, _) <- retainedBinders0
                , typeBinderRefsSameIdentity ref proxyRef
                ]
            [(_, Just completedProxyBound)] <-
              pure
                [ binder
                | binder@(ref, _) <- retainedBinders
                , typeBinderRefsSameIdentity ref proxyRef
                ]
            guard (boundMentionsAny movedRefs (Just openedProxyBound))
            guard
              ( operationalEndpointTypesAgree
                  (tyToElab completedProxyBound)
                  constructedType
              )
            pure constructedType

        -- Moving an exact parameter's forall declarations out of the
        -- operated spine can close a dependency in a later declaration.  Do
        -- that while the parameter certificate and the declaration identity
        -- are both present.  Reject any payload that is not a legal xMLF
        -- bound; leaving the moved identity free would publish an ill-scoped
        -- Gamma rather than completing it.
        completeRetainedBinder (ref, Nothing) =
          Just (ref, Nothing)
        completeRetainedBinder (ref, Just bound) =
          case
              elabToBound
                ( completeLambdaParamBoundaryDeclarationBound
                    [certificate]
                    ref
                    (tyToElab bound)
                )
            of
              Right completedBound -> Just (ref, Just completedBound)
              Left _ -> Nothing

    matchingCandidateBinder candidateBinders (expectedRef, expectedBound) = do
      [candidate@(_, candidateBound)] <-
        pure
          [ binder
          | binder@(candidateRef, _) <- candidateBinders
          , typeBinderRefsSameIdentity candidateRef expectedRef
          ]
      guard (binderBoundsAgree expectedBound candidateBound)
      pure candidate

    binderBoundsAgree Nothing Nothing = True
    binderBoundsAgree (Just left) (Just right) =
      operationalEndpointTypesAgree (tyToElab left) (tyToElab right)
    binderBoundsAgree _ _ = False

    boundMentionsAny _ Nothing = False
    boundMentionsAny refs (Just bound) =
      any (`anyRefIn` freeTypeVarRefsType (tyToElab bound)) refs

    ref `anyRefIn` refs = any (typeBinderRefsSameIdentity ref) refs

    allDistinctRefs [] = True
    allDistinctRefs (ref : refs) =
      not (ref `anyRefIn` refs) && allDistinctRefs refs

completeLambdaParamBoundaryTypeWithRootClosure
  :: Bool
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryTypeWithRootClosure
  mayCloseRoot
  certificates
  ty0 =
    completeLambdaParamBoundaryTypeWithRootClosureInScope
      mayCloseRoot
      []
      certificates
      ty0

completeLambdaParamBoundaryBoundInScope
  :: [TypeBinderRef]
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryBoundInScope lexicalRefs =
  completeLambdaParamBoundaryTypeWithRootClosureInScope
    True
    lexicalRefs

completeLambdaParamBoundaryTypeWithRootClosureInScope
  :: Bool
  -> [TypeBinderRef]
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryTypeWithRootClosureInScope
  mayCloseRoot
  initialLexicalRefs
  certificates
  ty0 =
  completeLambdaParamBoundaryTypeWithSiblingBlocking
    True
    mayCloseRoot
    initialLexicalRefs
    certificates
    ty0

-- The ordinary closure path blocks a source binder when the same identity is
-- free in a sibling subtree: without occurrence evidence, closing only one
-- sibling would be ambiguous.  A source-root completion has stronger
-- authority.  Its graph parameter route proves that remaining sibling
-- occurrences denote the exact checked parameter, so it may first close an
-- opened source body and then substitute those routed occurrences.
completeLambdaParamBoundaryTypeWithSiblingBlocking
  :: Bool
  -> Bool
  -> [TypeBinderRef]
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryTypeWithSiblingBlocking
  blockSiblingOccurrences
  mayCloseRoot
  initialLexicalRefs
  certificates
  ty0 =
  let completed =
        foldl'
          completeCertificate
          ty0
          certificates
   in completed
  where
    completeCertificate ty certificate =
      closeOpenedEndpoint
        mayCloseRoot
        initialLexicalRefs
        ( substTypeCaptureRef
            ( typeBinderRefFromIdentity
                (typeBinderIdentityFromNode (lpbcParameterNode certificate))
                "$lambda-param-boundary"
            )
            constructedType
            ty
        )
      where
        constructedType = lpbcConstructedType certificate
        constructedScheme = schemeFromType constructedType
        constructedBinderRefs =
          map fst (schemeBinderRefs constructedScheme)
        openedBody = schemeBody constructedScheme
        mbConstructedBound =
          case elabToBound constructedType of
            Right constructedBound -> Just constructedBound
            Left _ -> Nothing

        -- A graph-planned bound can retain the opened body of an exact
        -- polymorphic lambda parameter rather than its parameter node.  Close
        -- that body at the certificate's complete source type, but only when
        -- every exact source binder occurs free at this lexical point.  Under
        -- the parameter's own forall those identities are already bound and
        -- therefore must not be wrapped again.
        closeOpenedEndpoint mayClose lexicalRefs candidate
          | mayClose
          , closesExactOpenedEndpoint lexicalRefs candidate =
              constructedType
          | otherwise =
              case candidate of
                TVarRef ref -> TVarRef ref
                TArrow domain codomain ->
                  TArrow
                    ( closeOpenedEndpoint
                        True
                        (addSiblingClosureBlockers lexicalRefs [codomain])
                        domain
                    )
                    ( closeOpenedEndpoint
                        True
                        (addSiblingClosureBlockers lexicalRefs [domain])
                        codomain
                    )
                TConWithIdentity identity constructor arguments ->
                  TConWithIdentity
                    identity
                    constructor
                    (closeOpenedArguments True lexicalRefs arguments)
                TVarAppRef ref arguments ->
                  TVarAppRef
                    ref
                    (closeOpenedArguments True lexicalRefs arguments)
                TBaseWithIdentity identity base ->
                  TBaseWithIdentity identity base
                TBottom -> TBottom
                TForallRef ref Nothing body
                  | typeBinderRefNode ref
                      == Just (lpbcParameterNode certificate)
                  , TVarRef sourceRef <- constructedType
                  , not (typeBinderRefsSameIdentity ref sourceRef)
                  , not
                      ( any
                          (typeBinderRefsSameIdentity ref)
                          lexicalRefs
                      ) ->
                      -- Reification can close the exact parameter occurrence
                      -- as a construction-only unbounded forall before its
                      -- source identity is installed.  The parameter-boundary
                      -- certificate is the declaration authority for that
                      -- exact graph node, so open the placeholder at the
                      -- checked source type here.  Descending under the
                      -- placeholder would shadow the occurrence and make a
                      -- later substitution necessarily too late.
                      closeOpenedEndpoint
                        mayClose
                        lexicalRefs
                        ( substTypeCaptureRef
                            ref
                            constructedType
                            body
                        )
                TForallRef ref mbBound body ->
                  let boundBlockers =
                        addSiblingClosureBlockers
                          lexicalRefs
                          [TForallRef ref Nothing body :: ElabType]
                      bodyBlockers =
                        addSiblingClosureBlockers
                          lexicalRefs
                          (map tyToElab (maybeToList mbBound))
                   in TForallRef
                        ref
                        (fmap (closeOpenedBound boundBlockers) mbBound)
                        ( closeOpenedEndpoint
                            True
                            (ref : bodyBlockers)
                            body
                        )
                TMuRef ref body ->
                  TMuRef
                    ref
                    ( closeOpenedEndpoint
                        True
                        (ref : lexicalRefs)
                        body
                    )

        closeOpenedBound lexicalRefs candidate
          | Just constructedBound <- mbConstructedBound
          , closesExactOpenedEndpoint
              lexicalRefs
              (tyToElab candidate) =
              constructedBound
          | otherwise =
              case candidate of
                TArrow domain codomain ->
                  TArrow
                    ( closeOpenedEndpoint
                        True
                        (addSiblingClosureBlockers lexicalRefs [codomain])
                        domain
                    )
                    ( closeOpenedEndpoint
                        True
                        (addSiblingClosureBlockers lexicalRefs [domain])
                        codomain
                    )
                TConWithIdentity identity constructor arguments ->
                  TConWithIdentity
                    identity
                    constructor
                    (closeOpenedArguments True lexicalRefs arguments)
                TVarAppRef ref arguments ->
                  TVarAppRef
                    ref
                    (closeOpenedArguments True lexicalRefs arguments)
                TBaseWithIdentity identity base ->
                  TBaseWithIdentity identity base
                TBottom -> TBottom
                TForallRef ref mbBound body ->
                  let boundBlockers =
                        addSiblingClosureBlockers
                          lexicalRefs
                          [TForallRef ref Nothing body :: ElabType]
                      bodyBlockers =
                        addSiblingClosureBlockers
                          lexicalRefs
                          (map tyToElab (maybeToList mbBound))
                   in TForallRef
                        ref
                        (fmap (closeOpenedBound boundBlockers) mbBound)
                        ( closeOpenedEndpoint
                            True
                            (ref : bodyBlockers)
                            body
                        )
                TMuRef ref body ->
                  TMuRef
                    ref
                    ( closeOpenedEndpoint
                        True
                        (ref : lexicalRefs)
                        body
                    )

        -- Closing one exact opened parameter body introduces binders around
        -- every occurrence of those identities in the selected subtree.  A
        -- sibling occurrence therefore prevents selecting only that subtree:
        -- otherwise one identity would become bound in one branch while
        -- remaining free in another.  The original sibling types are used for
        -- every branch so the decision is independent of traversal order.
        addSiblingClosureBlockers lexicalRefs siblings =
          if blockSiblingOccurrences
            then concatMap freeTypeVarRefsType siblings ++ lexicalRefs
            else lexicalRefs

        closeOpenedArguments
          mayClose
          lexicalRefs
          (firstArgument NonEmpty.:| remainingArguments) =
            closeArgument [] firstArgument remainingArguments
              NonEmpty.:| closeRemaining [firstArgument] remainingArguments
          where
            closeArgument preceding argument following =
              closeOpenedEndpoint
                mayClose
                ( addSiblingClosureBlockers
                    lexicalRefs
                    (preceding ++ following)
                )
                argument

            closeRemaining _ [] = []
            closeRemaining preceding (argument : following) =
              closeArgument preceding argument following
                : closeRemaining (argument : preceding) following

        closesExactOpenedEndpoint lexicalRefs candidate =
          not (null constructedBinderRefs)
            && constructedBindersAreDistinct constructedBinderRefs
            && alphaEqType candidate openedBody
            && all
              ( \constructedRef ->
                  any
                    (typeBinderRefsSameIdentity constructedRef)
                    candidateFreeRefs
              )
              constructedBinderRefs
            && all
              ( \constructedRef ->
                  not
                    ( any
                        (typeBinderRefsSameIdentity constructedRef)
                        lexicalRefs
                    )
              )
              constructedBinderRefs
          where
            candidateFreeRefs = freeTypeVarRefsType candidate

        constructedBindersAreDistinct [] = True
        constructedBindersAreDistinct (ref : refs) =
          not (any (typeBinderRefsSameIdentity ref) refs)
            && constructedBindersAreDistinct refs

-- | Complete every type payload in a construction scheme while retaining its
-- exact graph-node routing and binder-order authority.  This is the
-- by-construction transition from the provisional graph view to the final
-- @S(n)@ consumed by packet placement.
completeLambdaParamBoundarySchemeInfo
  :: [LambdaParamBoundaryCertificate]
  -> SchemeInfo
  -> SchemeInfo
completeLambdaParamBoundarySchemeInfo =
  completeLambdaParamBoundarySchemeInfoInScope []

-- | Complete a construction scheme without re-closing parameter identities
-- that are already lexical in the consuming Gamma.  The caller supplies that
-- scope from its checked environment; the scheme's type shape is not used to
-- infer which free identities are ambient.
completeLambdaParamBoundarySchemeInfoInScope
  :: [TypeBinderRef]
  -> [LambdaParamBoundaryCertificate]
  -> SchemeInfo
  -> SchemeInfo
completeLambdaParamBoundarySchemeInfoInScope lexicalRefs certificates schemeInfo =
  rebuildSchemeInfoFromRefSubst
    schemeInfo
    completedScheme
    completedRoutes
  where
    completedScheme0 =
      schemeFromType
        ( completeLambdaParamBoundaryTypeInScope
            lexicalRefs
            certificates
            (schemeToType (siScheme schemeInfo))
        )
    completedBinders0 = schemeBinderRefs completedScheme0
    parameterLocalRefs =
      concatMap
        ( map fst
            . schemeBinderRefs
            . schemeFromType
            . lpbcConstructedType
        )
        certificates
    usedOutsideOwnDeclaration ref =
      any
        (typeBinderRefsSameIdentity ref)
        ( freeTypeVarRefsType (schemeBody completedScheme0)
            ++ concat
              [ freeTypeVarRefsType (tyToElab bound)
              | (candidateRef, Just bound) <- completedBinders0
              , not
                  (typeBinderRefsSameIdentity candidateRef ref)
              ]
        )
    escapedParameterLocalRef ref =
      any (typeBinderRefsSameIdentity ref) parameterLocalRefs
        && not (usedOutsideOwnDeclaration ref)
    completedBinders =
      filter
        (not . escapedParameterLocalRef . fst)
        completedBinders0
    completedScheme =
      mkElabSchemeWithRefs
        completedBinders
        (schemeBody completedScheme0)
    completedRoutes =
      IntMap.filter
        (not . escapedParameterLocalRef)
        (schemeInfoBinderRefSubst schemeInfo)

-- | Keep a certificate in the same outward identity domain as the checked
-- owner construction that carries it.  Its graph node is historical
-- presolution provenance and therefore does not move.
renameLambdaParamBoundaryCertificate
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LambdaParamBoundaryCertificate
  -> LambdaParamBoundaryCertificate
renameLambdaParamBoundaryCertificate renames certificate =
  certificate
    { lpbcConstructedType =
        renameTypeBinderRefPayloads renames (lpbcConstructedType certificate)
    , lpbcSourceBinderRefs =
        map
          renameRef
          (lpbcSourceBinderRefs certificate)
    }
  where
    renameRef ref =
      fromMaybe ref
        ( snd
            <$> find
              (typeBinderRefsSameIdentity ref . fst)
              renames
        )

-- | Install the Gamma effect of a lambda parameter boundary.  The exact-source
-- constructor deliberately has no refinement path: after validating any
-- independently constructed domain, it removes the root alias and leaves all
-- real ambient declarations untouched.
installLambdaParamBoundary
  :: NodeId
  -> LambdaParamBoundaryAuthority
  -> IntMap.IntMap TypeBinderRef
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError LambdaParamBoundaryInstallation
installLambdaParamBoundary paramNode authority gammaAliases typeBindings =
  case authority of
    ExactSourceLambdaParamBoundary
      exactTy
      mbConstructionTy
      sourceBinderRefs -> do
      case mbConstructionTy of
        Just constructionTy
          | not (isPendingExactParameterRoot constructionTy)
          , not (constructedTypeAgreesWithExact exactTy constructionTy) ->
              Left
                ( ValidationFailed
                    [ "compiler exact lambda parameter source type disagrees with its constructed domain"
                    , "  parameter node: " ++ show paramNode
                    , "  exact source type: " ++ show exactTy
                    , "  constructed domain: " ++ show constructionTy
                    , "  completed constructed domain: "
                        ++ show (completeConstructedRoot constructionTy)
                    ]
                )
        _ -> pure ()
      pure (exactInstallation sourceBinderRefs exactTy)
    ExactApplicationLambdaParamBoundary exactTy ->
      -- Figure 15.3.5 has already checked the argument at this exact
      -- endpoint.  The direct identity/eta parameter therefore owns the
      -- complete value type just like a source-exact parameter: its graph
      -- root is not an occurrence of the provisional application Gamma
      -- variable.  Remove that root route instead of comparing the checked
      -- value type with, or overwriting it by, the provisional lower bound.
      pure (exactInstallation [] exactTy)
    ConstructedLambdaParamBoundary constructionTy ->
      case IntMap.lookup (getNodeId paramNode) identityRoutes of
        Nothing ->
          pure
            LambdaParamBoundaryInstallation
              { lambdaParamBoundaryType = constructionTy,
                lambdaParamBoundaryLocalBinderIdentities =
                  boundaryTypeIdentities constructionTy,
                lambdaParamBoundaryGammaAliases = gammaAliases,
                lambdaParamBoundaryTypeBindings = typeBindings,
                lambdaParamBoundaryCertificate =
                  boundaryCertificate [] constructionTy
              }
        Just outwardRef ->
          case constructionTy of
            TVarRef constructionRef
              | typeBinderRefsSameIdentity constructionRef outwardRef ->
                  -- Figure 15.3.5 may construct the function parameter at
                  -- the local flexible declaration itself.  Its existing
                  -- Gamma entry stores the lower bound; replacing that bound
                  -- by the self reference would destroy the declaration, and
                  -- comparing the two as endpoint types would reject the
                  -- paper's ordinary @a >= sigma@ construction.
                  pure
                    LambdaParamBoundaryInstallation
                      { lambdaParamBoundaryType = constructionTy,
                        lambdaParamBoundaryLocalBinderIdentities =
                          boundaryTypeIdentities constructionTy,
                        lambdaParamBoundaryGammaAliases = gammaAliases,
                        lambdaParamBoundaryTypeBindings = typeBindings,
                        lambdaParamBoundaryCertificate =
                          boundaryCertificate [] constructionTy
                      }
            _ -> do
              mapM_ (validateExisting constructionTy outwardRef) affectedBindings
              pure
                LambdaParamBoundaryInstallation
                  { lambdaParamBoundaryType = constructionTy,
                    lambdaParamBoundaryLocalBinderIdentities =
                      boundaryTypeIdentities constructionTy,
                    lambdaParamBoundaryGammaAliases = gammaAliases,
                    lambdaParamBoundaryTypeBindings =
                      Map.mapWithKey
                        ( \ref bound ->
                            if affected outwardRef ref
                              then constructionTy
                              else bound
                        )
                        typeBindings,
                    lambdaParamBoundaryCertificate =
                      boundaryCertificate [] constructionTy
                  }
  where
    exactInstallation sourceBinderRefs exactTy =
      let certificate = boundaryCertificate sourceBinderRefs exactTy
       in LambdaParamBoundaryInstallation
            { lambdaParamBoundaryType = exactTy,
              lambdaParamBoundaryLocalBinderIdentities =
                boundaryTypeIdentities exactTy,
              lambdaParamBoundaryGammaAliases =
                IntMap.delete (getNodeId paramNode) gammaAliases,
              -- Existing declarations keep their exact identities, but their
              -- bounds must enter the same completed parameter domain before
              -- the body is constructed.  Otherwise a dependent packet sees
              -- the opened @a -> a@ view here and is only repaired to
              -- @forall a. a -> a@ after it has already emitted InstApp.
              lambdaParamBoundaryTypeBindings =
                Map.mapWithKey
                  ( completeLambdaParamBoundaryDeclarationBound
                      [certificate]
                  )
                  typeBindings,
              lambdaParamBoundaryCertificate = certificate
            }

    boundaryCertificate sourceBinderRefs constructedTy =
      LambdaParamBoundaryCertificate
        { lpbcParameterNode = paramNode,
          lpbcConstructedType = constructedTy,
          lpbcSourceBinderRefs = sourceBinderRefs
        }

    boundaryTypeIdentities ty =
      Set.fromList
        ( map
            typeBinderRefIdentity
            ( map fst (schemeBinderRefs (schemeFromType ty))
                ++ freeTypeVarRefsType ty
            )
        )

    identityRoutes =
      IntMap.union
        gammaAliases
        ( IntMap.fromList
            [ (getNodeId node, ref)
            | ref <- Map.keys typeBindings,
              Just node <- [typeBinderRefNode ref]
            ]
        )

    affected outwardRef ref =
      typeBinderRefsSameIdentity ref outwardRef
        || case typeBinderRefNode ref of
          Just node ->
            maybe
              False
              (typeBinderRefsSameIdentity outwardRef)
              (IntMap.lookup (getNodeId node) gammaAliases)
          Nothing -> False

    affectedBindings =
      case authority of
        ConstructedLambdaParamBoundary _ ->
          case IntMap.lookup (getNodeId paramNode) identityRoutes of
            Nothing -> []
            Just outwardRef ->
              [ (ref, bound)
              | (ref, bound) <- Map.toList typeBindings,
                affected outwardRef ref
              ]
        ExactSourceLambdaParamBoundary {} -> []
        ExactApplicationLambdaParamBoundary {} -> []

    validateExisting constructionTy outwardRef (ref, bound)
      | bound == TBottom = pure ()
      | typesAgree bound constructionTy = pure ()
      | otherwise =
          Left
            ( ValidationFailed
                [ "constructed lambda domain disagrees with its published Gamma bound"
                , "  parameter node: " ++ show paramNode
                , "  outward binder: " ++ show outwardRef
                , "  binding: " ++ show ref
                , "  published bound: " ++ show bound
                , "  constructed domain: " ++ show constructionTy
                ]
            )

    typesAgree left right =
      alphaEqType left right || churchAwareEqType left right

    -- An exact source parameter can meet a provisional graph variable whose
    -- declaration has already been constructed at the exact source type.
    -- The Gamma binding is the positive construction certificate for that
    -- completion; comparing the raw graph occurrence would reject before the
    -- boundary gets the chance to install the source-owned type.
    constructedTypeAgreesWithExact exactTy constructionTy =
      typesAgree exactTy constructionTy
        || typesAgree exactTy (completeConstructedRoot constructionTy)

    completeConstructedRoot constructionTy =
      case constructionTy of
        TVarRef constructionRef ->
          fromMaybe constructionTy $ do
            (_, bound) <-
              find
                (typeBinderRefsSameIdentity constructionRef . fst)
                (Map.toList typeBindings)
            guard (bound /= TBottom)
            pure bound
        _ -> constructionTy

    -- The graph allocates the lambda parameter root before a source-exact
    -- domain is available.  A direct occurrence of that same root is the
    -- pending endpoint completed by this boundary, not an independently
    -- constructed domain to compare with the completed source type.  Any
    -- structured type, or a variable rooted at another node, remains subject
    -- to the disagreement check above.
    isPendingExactParameterRoot constructionTy =
      case constructionTy of
        TVarRef constructionRef ->
          typeBinderRefNode constructionRef == Just paramNode
        _ -> False

-- | Apply a packet quotient only outside the identities fixed by the installed
-- lambda boundary.  This protects source-owned forall declarations at exact
-- evidence parameters and the complete type of an ordinary parameter already
-- constructed by an enclosing body edge.  Re-routing either after boundary
-- installation would make the parameter and its recursively elaborated
-- occurrences enter different identity domains.
lambdaParamConstructionRenames
  :: Set.Set TypeBinderIdentity
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamConstructionRenames localBinderIdentities =
  filter
    ( \(sourceRef, _) ->
        Set.notMember
          (typeBinderRefIdentity sourceRef)
          localBinderIdentities
    )

-- | Enter the exact body packet in its certified source orientation.  Lexical
-- parameter declarations remain fixed as usual.  In addition, an exact
-- @construction -> source@ certificate removes only its matching
-- @source -> construction@ packet edge.
--
-- The certificate, rather than the child's starting side, fixes the
-- orientation.  A child already at the source endpoint remains there; a child
-- at the construction endpoint is recovered by the separately selected
-- inverse occurrence rename.  This makes the quotient idempotent instead of
-- turning a correct source occurrence back into its graph peer.
--
-- Comparing both endpoints is important: a same-named or representative peer
-- must not suppress an unrelated outgoing quotient edge.
lambdaBodyConstructionRenames
  :: Set.Set TypeBinderIdentity
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaBodyConstructionRenames lexicalBinderIdentities certifiedSourceRenames =
  filter admissible
  where
    admissible route@(sourceRef, _) =
      Set.notMember
        (typeBinderRefIdentity sourceRef)
        lexicalBinderIdentities
        && not (any (exactInverse route) certifiedSourceRenames)

    exactInverse
      (sourceRef, constructionRef)
      (recoveredConstructionRef, recoveredSourceRef) =
        typeBinderRefsSameIdentity sourceRef recoveredSourceRef
          && typeBinderRefsSameIdentity
            constructionRef
            recoveredConstructionRef

-- | Recover a free graph occurrence through the exact parameter certificate
-- that completed that same graph root.  This is the term-level counterpart of
-- 'completeLambdaParamBoundaryType' for the common variable endpoint: it
-- supplies an identity route that can be applied atomically to a recursively
-- elaborated term and all of its construction metadata.  A structured
-- parameter type is completed in type payloads directly and therefore does
-- not claim a binder-to-binder rename here.
lambdaParamBoundaryOccurrenceRenames
  :: [LambdaParamBoundaryCertificate]
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamBoundaryOccurrenceRenames certificates usedRefs locallyEmittedRefs =
  [ route
  | (index, route@(graphRef, sourceRef)) <-
      zip [0 :: Int ..] candidates
  , not
      ( any
          (typeBinderRefsSameIdentity graphRef . fst)
          (take index candidates)
      )
  , all
      (typeBinderRefsSameIdentity sourceRef . snd)
      ( candidatesForGraph graphRef
      )
  ]
  where
    candidates =
      [ route
      | route@(graphRef, _) <- lambdaParamBoundaryVariableRoutes certificates
      , any (typeBinderRefsSameIdentity graphRef) usedRefs
      , not
          ( any
              (typeBinderRefsSameIdentity graphRef)
              locallyEmittedRefs
          )
      ]
    candidatesForGraph graphRef =
      filter
        (typeBinderRefsSameIdentity graphRef . fst)
        candidates

-- | Keep a completed exact parameter boundary from being replayed backwards
-- by an enclosing packet or annotation.  The certificate proves
-- @Graph(parameter) -> Source(parameter)@; a later
-- @Source(parameter) -> Graph(parameter)@ route is therefore the inverse of a
-- construction already consumed at this lexical boundary.
lambdaParamBoundaryConstructionRenames
  :: [LambdaParamBoundaryCertificate]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamBoundaryConstructionRenames certificates =
  lambdaBodyConstructionRenames
    Set.empty
    (lambdaParamBoundaryVariableRoutes certificates)

-- | Identities already fixed inside a constructed parameter type.  A source
-- annotation below that lambda must not replay an inherited
-- @source -> graph@ route through any of these identities: the installed
-- parameter certificate is the more local construction authority.
lambdaParamBoundaryProtectedIdentities
  :: [LambdaParamBoundaryCertificate]
  -> Set.Set TypeBinderIdentity
lambdaParamBoundaryProtectedIdentities =
  Set.fromList
    . concatMap
      ( \certificate ->
          let ty = lpbcConstructedType certificate
           in map
                typeBinderRefIdentity
                ( map fst (schemeBinderRefs (schemeFromType ty))
                    ++ freeTypeVarRefsType ty
                )
      )

-- | Recover the source side of a construction route that an installed
-- parameter boundary has made lexical.  The inverse is justified by two
-- positive pieces of authority: the boundary certificate owns the source
-- identity, and the incoming construction route identifies its exact graph
-- peer.  This also handles structured parameter types such as @List a@, for
-- which the parameter root itself is not the graph node of @a@.
lambdaParamBoundaryRecoveryRenames
  :: [LambdaParamBoundaryCertificate]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamBoundaryRecoveryRenames certificates =
  mapMaybe recover
  where
    protectedIdentities =
      lambdaParamBoundaryProtectedIdentities certificates

    recover (sourceRef, constructionRef)
      | Set.member
          (typeBinderRefIdentity sourceRef)
          protectedIdentities
      , not (typeBinderRefsSameIdentity sourceRef constructionRef) =
          Just (constructionRef, sourceRef)
      | otherwise = Nothing

lambdaParamBoundaryVariableRoutes
  :: [LambdaParamBoundaryCertificate]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamBoundaryVariableRoutes certificates =
  [ (graphRef, sourceRef)
  | certificate <- certificates
  , TVarRef sourceRef <- [lpbcConstructedType certificate]
  , let graphRef =
          typeBinderRefFromIdentity
            (typeBinderIdentityFromNode (lpbcParameterNode certificate))
            (typeBinderRefName sourceRef)
  , not (typeBinderRefsSameIdentity graphRef sourceRef)
  ]

-- | Select the exact construction routes that return a recursively
-- elaborated child to its already installed lambda-boundary identity domain.
-- A route is admissible only when all of its authority is local and positive:
--
-- * the concrete graph node occurs in the child term or owner certificate;
-- * the route target is protected by the constructed parameter boundary; and
-- * the graph identity is not declared by the child itself.
--
-- The graph reference is reconstructed from the direct alias key.  Solved
-- representatives and same-shaped types are deliberately absent, so this
-- cannot manufacture a quotient when the body edge did not publish one.
protectedBoundaryOccurrenceRenames
  :: Set.Set TypeBinderIdentity
  -> IntMap.IntMap TypeBinderRef
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
protectedBoundaryOccurrenceRenames protectedIdentities directAliases usedRefs locallyEmittedRefs =
  [ (graphRef, outwardRef)
  | (nodeKey, outwardRef) <- IntMap.toList directAliases
  , Set.member
      (typeBinderRefIdentity outwardRef)
      protectedIdentities
  , let graphRef =
          typeBinderRefFromIdentity
            (typeBinderIdentityFromNode (NodeId nodeKey))
            (typeBinderRefName outwardRef)
  , any (typeBinderRefsSameIdentity graphRef) usedRefs
  , not
      ( any
          (typeBinderRefsSameIdentity graphRef)
          locallyEmittedRefs
      )
  , not (typeBinderRefsSameIdentity graphRef outwardRef)
  ]

-- | Select the identity rewrite that makes an ordinary lambda payload use
-- the Gamma binder emitted by that same Figure 15.3.5 boundary.  The route
-- must be keyed by the exact parameter node and end at a binder emitted by
-- this owner; a solved/source alias alone is not construction authority.
-- A structured parameter such as @Box a@ carries the same construction
-- obligation as a bare @a@: apply only validated construction-quotient routes
-- whose source occurs free in the parameter and whose target is emitted by
-- this owner.  The exact parameter-node route remains the graph-local fallback
-- when no direct source route was needed.
lambdaParamLocalGammaRenames
  :: NodeId
  -> ElabType
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamLocalGammaRenames paramNode paramTy emittedRefs constructionRenames localRoutes =
  case directRenames of
    _ : _ -> directRenames
    [] ->
      case
          ( freeRefs,
            IntMap.lookup (getNodeId paramNode) localRoutes
          )
        of
        ([sourceRef], Just localRef)
          | isEmitted localRef
          , not (typeBinderRefsSameIdentity sourceRef localRef) ->
              [(sourceRef, localRef)]
        _ -> []
  where
    freeRefs = freeTypeVarRefsType paramTy
    isFree ref = any (typeBinderRefsSameIdentity ref) freeRefs
    isEmitted ref = any (typeBinderRefsSameIdentity ref) emittedRefs
    directRenames =
      [ rename
      | rename@(sourceRef, localRef) <- constructionRenames
      , isFree sourceRef
      , isEmitted localRef
      , not (typeBinderRefsSameIdentity sourceRef localRef)
      ]

-- | Resolve the one non-orthogonal pair of application argument candidates.
-- A direct local Gamma declaration normally keeps the shared Figure 15.3.5
-- endpoint abstract.  When an independently constructed exact result and the
-- checked argument both prove that the declaration is consumed at its bound,
-- use that bound for both application children instead.  The caller supplies
-- the bound candidate only after checking that the result does not retain the
-- local identity.
selectDirectLocalApplicationArgumentTopology
  :: Maybe ElabType
  -> Maybe ElabType
  -> Maybe ElabType
  -> Maybe ElabType
selectDirectLocalApplicationArgumentTopology exactResult localBound localDeclaration =
  case localDeclaration of
    Nothing -> Nothing
    Just declaration ->
      case (exactResult, localBound) of
        (Just _, Just bound) -> Just bound
        _ -> Just declaration

-- | Exact-source parameter declarations remain lexical even when graph
-- construction has published an ambient route with the same identity.  That
-- route is the occurrence's eventual instantiation target; it does not turn
-- the parameter's forall declaration into ambient Gamma.
lambdaParamProtectedIdentities
  :: Set.Set TypeBinderIdentity
  -> (TypeBinderRef -> Bool)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Set.Set TypeBinderIdentity
  -> Set.Set TypeBinderIdentity
lambdaParamProtectedIdentities lexicalIdentities ambientOwnsExact constructionRoutes schemeInfo excludedIdentities =
  Set.union
    lexicalIdentities
    ( constructionProtectedIdentities
        ambientOwnsExact
        constructionRoutes
        schemeInfo
        excludedIdentities
    )

-- | A current-scope requirement is owned by this constructor even when a
-- provisional ambient slot has the same identity. Reuse that slot only once
-- its bound already satisfies S(operated). Nested requirements still need
-- their explicit local-closure proof before this constructor may claim them.
requirementNeedsLocalConstruction
  :: Bool
  -> RequiredGammaPlacement
  -> Bool
  -> Bool
requirementNeedsLocalConstruction closureOwned placement ambientSatisfied =
  (closureOwned || requiredGammaPlacementIsLocal placement)
    && not ambientSatisfied

-- | Select the single exact declaration carried by several direct graph
-- routes.  Bottom is an exact unbounded declaration unless the caller carries
-- a frozen nested-result certificate that explicitly marks that route as a
-- provisional result slot.  Keeping that provenance outside the type payload
-- prevents an ordinary @forall a@ declaration from being refined merely
-- because another route happens to carry a non-bottom bound.
data DirectAmbientGammaAuthorityProvenance
  = DirectAmbientEstablished
  | DirectAmbientProvisionalNestedResult
  deriving (Eq, Show)

-- | Exact construction-time source packet for one application occurrence.
-- The constructor is private: only the application that owns the function
-- boundary may publish its distinct argument edge together with the
-- identity-bearing scheme produced for that occurrence.
data PendingLocalResultSourcePacket =
  PendingLocalResultSourcePacket
    { plrspOwner :: !LocalGammaOwner,
      plrspArgumentEdge :: !EdgeId,
      plrspCertifiedResultEndpoints :: !IntSet.IntSet,
      plrspSchemeInfo :: !SchemeInfo
    }
  deriving (Eq, Show)

-- | Publish the identity-bearing packet produced by one exact application
-- source occurrence.  The function edge is already fixed by
-- 'LocalGammaOwner'; rejecting it here makes the supplied edge
-- argument-specific rather than an arbitrary member of the application.  The
-- constructor also freezes every exact edge-local endpoint proved by the same
-- root-RaiseMerge requirements.  A later requirement therefore cannot borrow
-- this packet merely because its scheme happens to route another graph node
-- to the same declaration identity.
mkApplicationPendingLocalResultSourcePacket
  :: LocalGammaOwner
  -> EdgeId
  -> [RequiredGammaBinder]
  -> SchemeInfo
  -> Either ElabError PendingLocalResultSourcePacket
mkApplicationPendingLocalResultSourcePacket owner argumentEdge requirements schemeInfo
  | lgoConstructor owner /= LocalApplicationGamma =
      Left
        ( ValidationFailed
            [ "pending local-result source packet requires an application owner"
            , "  owner: " ++ show owner
            ]
        )
  | lgoBoundaryEdge owner == argumentEdge =
      Left
        ( ValidationFailed
            [ "pending local-result source packet requires the argument-side edge"
            , "  owner: " ++ show owner
            , "  supplied edge: " ++ show argumentEdge
            ]
        )
  | otherwise =
      Right
        PendingLocalResultSourcePacket
          { plrspOwner = owner,
            plrspArgumentEdge = argumentEdge,
            plrspCertifiedResultEndpoints =
              IntSet.fromList
                [ getNodeId exterior
                | requirement <- requirements
                , rgbEdgeIds requirement
                    == (argumentEdge NonEmpty.:| [])
                , let exterior = rgbExteriorNode requirement
                , rgbResultRoots requirement
                    == (exterior NonEmpty.:| [])
                ],
            plrspSchemeInfo = schemeInfo
          }

-- | Classify one direct ambient declaration as provisional only when the
-- frozen construction provenance proves that this exact graph occurrence is
-- its pending result route.  The proof is deliberately conjunctive:
--
-- * either every required edge names the same exact frozen descendant closure,
--   or the sole required edge is the current application's certified
--   argument edge at its exact endpoint;
-- * the selected certificate carries the exact owner and source occurrence;
-- * its identity-bearing scheme directly routes both the exterior and this
--   result node to the exact ambient declaration; and
-- * the requirement is local and still has a non-bottom operated type to
--   materialize.
--
-- No canonical representative, display name, or type shape participates in
-- this decision.  A bottom-valued declaration without the complete frozen
-- proof is an established unbounded ambient declaration.
directAmbientGammaAuthorityProvenance
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> Maybe PendingLocalResultSourcePacket
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> NodeId
  -> AmbientGammaAuthority
  -> DirectAmbientGammaAuthorityProvenance
directAmbientGammaAuthorityProvenance refRenames owner sourcePacket closures requirement node authority
  | agaBound authority == TBottom
      && ( exactPendingLocalResultRoute
             refRenames
             owner
             closures
             requirement
             node
             authority
             || maybe
               False
               ( \packet ->
                   exactPendingSourcePacketResultRoute
                     refRenames
                     owner
                     packet
                     requirement
                     node
                     authority
               )
               sourcePacket
         ) =
      DirectAmbientProvisionalNestedResult
  | otherwise =
      DirectAmbientEstablished

-- | Prove the pending-scheme subcase of completed lambda-body projection.
-- Unlike direct-authority selection above, the old payload need not be
-- Bottom: exact root generalization can preserve an arrow-shaped skeleton
-- such as @Bottom -> Bottom@.  The replacement authority comes exclusively
-- from the frozen local-Gamma closure and its direct pending-scheme route; no
-- relation between the old and new type shapes is inferred here.
bodyConsumerProjectionProvenance
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> NodeId
  -> AmbientGammaAuthority
  -> DirectAmbientGammaAuthorityProvenance
bodyConsumerProjectionProvenance refRenames owner closures requirement node authority
  | exactPendingLocalConsumerCompletion
      refRenames
      owner
      closures
      requirement
      node
      authority =
      DirectAmbientProvisionalNestedResult
  | otherwise =
      DirectAmbientEstablished

-- | Classify the completed-body projection selected by one exact
-- 'BodyConsumerRoute'.  Two construction certificates are accepted:
--
-- * a pending owner scheme directly routes the exterior to its unmaterialized
--   consumer slot; or
-- * an ordinary frozen closure has no pending packet and either the exact
--   semantic exterior declaration is already present in ambient Gamma with
--   the completed bound, or this exact lambda emits the construction
--   declaration while the semantic exterior is deliberately absent from the
--   enclosing Gamma.
--
-- The latter ordinary lane is Figure 15.3.5's constructor-local
-- @Lambda(Gamma_g)@: root preparation excludes that exterior from ambient
-- ownership precisely so the lambda can emit it.  The private declaration
-- authority proves that local emission; the exact closure and validated
-- semantic-to-construction route prove which provisional ambient slot it may
-- replace.  A present semantic exterior still remains authoritative and must
-- agree with the completed bound.  Type equality validates identity-selected
-- endpoints but never selects an endpoint by shape.
bodyConsumerRouteProjectionProvenance
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> BodyConsumerDeclarationAuthority
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> DirectAmbientGammaAuthorityProvenance
bodyConsumerRouteProjectionProvenance refRenames owner closures requirement declarationAuthority projectedTy ambientBindings
  | not exactRoute =
      DirectAmbientEstablished
  | otherwise =
      case exactLocalGammaClosure owner closures requirement of
        Just closure
          | pendingSchemeAuthorizes closure
              || ordinaryExteriorAuthorizes closure
              || ordinaryLocalEmissionAuthorizes closure
              || ordinaryCoincidentLocalEmissionAuthorizes closure ->
              DirectAmbientProvisionalNestedResult
        _ -> DirectAmbientEstablished
  where
    route = authorizedBodyConsumerRoute declarationAuthority

    exactRoute =
      lgoConstructor owner == LocalLambdaGamma
        && rgbOperatedType requirement /= TBottom
        && requirementOwnedByRoute
        && bcrOwner route == owner
        && bcrEdgeId route == lgoBoundaryEdge owner
        && bcrEdgeId route
          `elem` NonEmpty.toList (rgbEdgeIds requirement)
        && bcrExteriorNode route == rgbExteriorNode requirement
        && typeBinderRefIdentity (bcrSemanticRef route)
          == typeBinderIdentityFromNode (rgbExteriorNode requirement)
        && operationalEndpointTypesAgree
          (bcrOperatedType route)
          (rgbOperatedType requirement)
        && operationalEndpointTypesAgree
          projectedTy
          (bcrConstructionOperatedType route)

    requirementOwnedByRoute =
      case rgbPlacement requirement of
        RequiredGammaAtCurrentScope -> True
        RequiredGammaAtConstructionScope scope ->
          scope == lgoScope owner
        RequiredGammaAtNestedScope _ -> False

    pendingSchemeAuthorizes closure =
      isJust (lgcOwnerPendingScheme closure)
        && case exactConstructionBindings of
          [(constructionRef, constructionBound)] ->
            any
              ( == DirectAmbientProvisionalNestedResult
              )
              [ bodyConsumerProjectionProvenance
                  refRenames
                  owner
                  closures
                  requirement
                  resultNode
                  ( AmbientGammaAuthority
                      constructionRef
                      constructionBound
                  )
              | resultNode <- NonEmpty.toList (rgbResultRoots requirement)
              ]
          _ -> False

    ordinaryExteriorAuthorizes closure =
      lgcOwnerPendingScheme closure == Nothing
        && not
          ( typeBinderRefsSameIdentity
              (bcrSemanticRef route)
              (bcrConstructionRef route)
          )
        && case (exactConstructionBindings, exactExteriorBindings) of
          ([_], [(_, exteriorBound)]) ->
            operationalEndpointTypesAgree
              exteriorBound
              (bcrConstructionOperatedType route)
              && operationalEndpointTypesAgree
                exteriorBound
                projectedTy
          _ -> False

    ordinaryLocalEmissionAuthorizes closure =
      case declarationAuthority of
        BodyConsumerLocallyEmitted _ _
          | lgcOwnerPendingScheme closure == Nothing
          , not
              ( typeBinderRefsSameIdentity
                  (bcrSemanticRef route)
                  (bcrConstructionRef route)
              )
          , null exactExteriorBindings ->
              case exactConstructionBindings of
                [_] -> True
                _ -> False
        _ -> False

    -- Generalization can retain the exact exterior graph identity for both
    -- the enclosing provisional slot and the declaration emitted by this
    -- lambda.  That is not the ordinary ambient-authority lane: the exact
    -- local declaration, owner/edge route, and frozen closure together prove
    -- that this constructor completes the coincident slot.  Without the
    -- local declaration authority the same ambient bottom remains
    -- established and cannot be refined from type shape alone.
    ordinaryCoincidentLocalEmissionAuthorizes closure =
      case declarationAuthority of
        BodyConsumerLocallyEmitted _ localBound
          | lgcOwnerPendingScheme closure == Nothing
          , typeBinderRefsSameIdentity
              (bcrSemanticRef route)
              (bcrConstructionRef route)
          , operationalEndpointTypesAgree localBound projectedTy ->
              case exactConstructionBindings of
                [(_, ambientBound)] ->
                  not
                    ( operationalEndpointTypesAgree
                        ambientBound
                        localBound
                    )
                _ -> False
        _ -> False

    exactConstructionBindings =
      exactBindingsFor (bcrConstructionRef route)

    exactExteriorBindings =
      exactBindingsFor (bcrSemanticRef route)

    exactBindingsFor requiredRef =
      [ binding
      | binding@(ref, _) <- Map.toList ambientBindings
      , typeBinderRefsSameIdentity ref requiredRef
      ]

-- | Prove the body-only completion of one pending local Gamma consumer.
-- The pending construction scheme routes the exact exterior occurrence to
-- the unmaterialized consumer slot.  Its result occurrence may legitimately
-- route to a different binder: the recursively checked body is precisely what
-- computes the consumer's bound from that result.  Requiring both routes to
-- alias the consumer would reject a structured body such as @c -> c@ whose
-- pending consumer is @b@.
--
-- The complete frozen closure still fixes the owner, edge set, exterior, and
-- consumer identity.  The pending scheme's direct exterior substitution must
-- name exactly one unbounded declaration before that declaration is projected
-- through the certified construction quotient.  A construction alias may
-- coexist in the scheme after projection, but a missing direct route, a peer
-- route, or an already materialized direct declaration cannot authorize
-- replacement.
exactPendingLocalConsumerCompletion
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> NodeId
  -> AmbientGammaAuthority
  -> Bool
exactPendingLocalConsumerCompletion refRenames owner closures requirement node authority =
  rgbOperatedType requirement /= TBottom
    && requiredGammaPlacementIsLocal (rgbPlacement requirement)
    && node `elem` NonEmpty.toList (rgbResultRoots requirement)
    && maybe False closureProvesConsumer exactClosure
  where
    exactClosure =
      exactLocalGammaClosure owner closures requirement

    closureProvesConsumer closure =
      maybe False pendingSchemeProvesConsumer (lgcOwnerPendingScheme closure)

    pendingSchemeProvesConsumer pendingSchemeInfo =
      case
          IntMap.lookup
            (getNodeId (rgbExteriorNode requirement))
            (schemeInfoBinderRefSubst pendingSchemeInfo)
      of
        Just exteriorRef
          | exactAuthorityRef exteriorRef ->
              case
                  [ mbBound
                  | (ref, mbBound) <-
                      schemeBinderRefs (siScheme pendingSchemeInfo)
                  , typeBinderRefsSameIdentity
                      ref
                      exteriorRef
                  ]
              of
                [Nothing] -> True
                _ -> False
        _ -> False

    exactAuthorityRef =
      typeBinderRefsSameIdentity (agaExactRef authority) . applyRefRenames

    applyRefRenames ref =
      maybe
        ref
        snd
        (find (typeBinderRefsSameIdentity ref . fst) refRenames)

exactPendingLocalResultRoute
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> NodeId
  -> AmbientGammaAuthority
  -> Bool
exactPendingLocalResultRoute refRenames owner closures requirement node authority =
  rgbOperatedType requirement /= TBottom
    && requiredGammaPlacementIsLocal (rgbPlacement requirement)
    && node `elem` NonEmpty.toList (rgbResultRoots requirement)
    && maybe False closureProvesResult exactClosure
  where
    exactClosure =
      exactLocalGammaClosure owner closures requirement

    closureProvesResult closure =
      maybe False pendingSchemeProvesResult (lgcOwnerPendingScheme closure)

    pendingSchemeProvesResult pendingSchemeInfo =
      case
          ( IntMap.lookup
              (getNodeId (rgbExteriorNode requirement))
              (schemeInfoBinderRefSubst pendingSchemeInfo)
          , IntMap.lookup
              (getNodeId node)
              (schemeInfoBinderRefSubst pendingSchemeInfo)
          )
      of
        (Just exteriorRef, Just resultRef) ->
          exactAuthorityRef exteriorRef
            && exactAuthorityRef resultRef
        _ -> False

    exactAuthorityRef =
      typeBinderRefsSameIdentity (agaExactRef authority) . applyRefRenames

    applyRefRenames ref =
      maybe
        ref
        snd
        (find (typeBinderRefsSameIdentity ref . fst) refRenames)

-- | Recover the one frozen closure that owns a complete requirement.  This is
-- the shared construction certificate for both direct authority selection
-- and completed-body projection; each consumer adds its own exact route
-- condition without weakening owner or occurrence identity.
exactLocalGammaClosure
  :: LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> Maybe LocalGammaClosure
exactLocalGammaClosure owner closures requirement =
  case selectLocalGammaClosureOwnerLane owner closures requirement of
    Right (Just closure)
      | lgcOwner closure == owner
      , edgeKeySet (lgcEdgeIds closure)
          == edgeKeySet (rgbEdgeIds requirement)
      , lgcExteriorNode closure == rgbExteriorNode requirement
      , lgcConsumerIdentity closure
          == typeBinderIdentityFromNode (rgbExteriorNode requirement) ->
          Just closure
    _ -> Nothing
  where
    edgeKeySet =
      IntSet.fromList . map getEdgeId . NonEmpty.toList

-- | Select closure authority only from the exact source-owner lane.  A direct
-- AApp closure may share an edge/exterior with an enclosing lambda
-- requirement without claiming that lambda's construction.  Such foreign
-- closures are therefore absence, not conflict.  Once this owner has claimed
-- any edge, however, it must own the complete requirement with one identical
-- closure record; partial or ambiguous claims are rejected within the lane.
selectLocalGammaClosureOwnerLane
  :: LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> Either ElabError (Maybe LocalGammaClosure)
selectLocalGammaClosureOwnerLane owner closures requirement =
  case ownerClosures of
    [] -> pure Nothing
    closure : rest
      | length ownerClosures /= NonEmpty.length requirementEdges ->
          laneFailure
            "only part of the requirement is claimed by this source owner"
            ownerClosures
      | any (/= closure) rest ->
          laneFailure
            "the requirement has conflicting closures in one source-owner lane"
            ownerClosures
      | otherwise -> pure (Just closure)
  where
    requirementEdges = rgbEdgeIds requirement
    ownerClosures =
      [ closure
      | edgeId <- NonEmpty.toList requirementEdges
      , Just closure <- [IntMap.lookup (getEdgeId edgeId) closures]
      , lgcOwner closure == owner
      ]

    laneFailure detail found =
      Left
        ( ValidationFailed
            [ "invalid local Gamma source-owner lane"
            , "  detail: " ++ detail
            , "  exact owner: " ++ show owner
            , "  requirement: " ++ show requirement
            , "  owner-lane closures: " ++ show found
            ]
        )

-- | Prove the corresponding current-application case directly from the
-- source packet created while constructing that application.  Unlike a
-- retained descendant closure, the current occurrence is not stored in
-- 'envLocalGammaClosures'; its function-edge owner, distinct argument edge,
-- and generalized scheme are available at the construction site, while the
-- typed requirement retains the exact edge-local endpoint.
exactPendingSourcePacketResultRoute
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> PendingLocalResultSourcePacket
  -> RequiredGammaBinder
  -> NodeId
  -> AmbientGammaAuthority
  -> Bool
exactPendingSourcePacketResultRoute refRenames owner packet requirement node authority =
  rgbOperatedType requirement /= TBottom
    && packetOwnsPlacement
    && plrspOwner packet == owner
    && rgbEdgeIds requirement
      == (plrspArgumentEdge packet NonEmpty.:| [])
    && rgbResultRoots requirement
      == (rgbExteriorNode requirement NonEmpty.:| [])
    && node == rgbExteriorNode requirement
    && IntSet.member
      (getNodeId node)
      (plrspCertifiedResultEndpoints packet)
    && pendingSchemeProvesResult (plrspSchemeInfo packet)
  where
    packetOwnsPlacement =
      case rgbPlacement requirement of
        RequiredGammaAtCurrentScope -> True
        RequiredGammaAtConstructionScope scope ->
          scope == lgoScope owner
        RequiredGammaAtNestedScope _ -> False

    pendingSchemeProvesResult pendingSchemeInfo =
      case
          ( IntMap.lookup
              (getNodeId (rgbExteriorNode requirement))
              (schemeInfoBinderRefSubst pendingSchemeInfo)
          , IntMap.lookup
              (getNodeId node)
              (schemeInfoBinderRefSubst pendingSchemeInfo)
          )
      of
        (Just exteriorRef, Just resultRef) ->
          exactAuthorityRef exteriorRef
            && exactAuthorityRef resultRef
        _ -> False

    exactAuthorityRef =
      typeBinderRefsSameIdentity (agaExactRef authority) . applyRefRenames

    applyRefRenames ref =
      maybe
        ref
        snd
        (find (typeBinderRefsSameIdentity ref . fst) refRenames)

selectDirectAmbientGammaAuthority
  :: TypeBinderRef
  -> [(DirectAmbientGammaAuthorityProvenance, AmbientGammaAuthority)]
  -> Either ElabError (Maybe AmbientGammaAuthority)
selectDirectAmbientGammaAuthority requiredRef authorities =
  case matchingAuthorities of
    [] -> pure Nothing
    (_, firstAuthority) : _ ->
      case establishedAuthorities of
        [] -> pure (Just firstAuthority)
        established : rest
          | all (sameEstablishedBound established) rest ->
              pure (Just established)
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "one exact ambient Gamma declaration has conflicting established bounds"
                    , "  required ref: " ++ show requiredRef
                    , "  direct authorities: " ++ show matchingAuthorities
                    ]
                )
  where
    matchingAuthorities =
      filter
        (typeBinderRefsSameIdentity requiredRef . agaExactRef . snd)
        authorities
    establishedAuthorities =
      [ authority
      | (DirectAmbientEstablished, authority) <- matchingAuthorities
      ]
    sameEstablishedBound left right =
      operationalEndpointTypesAgree
        (agaBound left)
        (agaBound right)

-- | Freeze the direct construction aliases whose outward identities already
-- have exact declarations in the ambient type environment.  The node key is
-- never canonicalized and declaration lookup is identity-only; aliases
-- without an exact binding are not authorities.
buildAmbientGammaAuthorities
  :: IntMap.IntMap TypeBinderRef
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (IntMap.IntMap AmbientGammaAuthority)
buildAmbientGammaAuthorities constructionAliases typeBindings =
  foldM addAuthority IntMap.empty (IntMap.toList constructionAliases)
  where
    bindings = Map.toList typeBindings

    addAuthority authorities (liveKey, outwardRef) =
      case
          [ binding
          | binding@(candidateRef, _) <- bindings
          , typeBinderRefsSameIdentity candidateRef outwardRef
          ]
      of
        [] -> pure authorities
        (exactRef, exactBound) : rest
          | all
              ( \(candidateRef, candidateBound) ->
                  typeBinderRefsSameIdentity candidateRef exactRef
                    && ( alphaEqType exactBound candidateBound
                           || churchAwareEqType exactBound candidateBound
                       )
              )
              rest ->
              pure
                ( IntMap.insert
                    liveKey
                    AmbientGammaAuthority
                      { agaExactRef = exactRef
                      , agaBound = exactBound
                      }
                    authorities
                )
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "direct construction alias has conflicting exact ambient bindings"
                    , "  live node: " ++ show (NodeId liveKey)
                    , "  outward ref: " ++ show outwardRef
                    , "  first exact binding: "
                        ++ show (exactRef, exactBound)
                    , "  conflicting bindings: " ++ show rest
                    ]
                )

-- | Construction proof for a body that has already opened the forall packet
-- carried by its ordinary Gamma consumer.  The routes align each Gamma-bound
-- binder with the source packet identity that the checked body already uses;
-- the outgoing computation then retains only the consumer's terminal HYP.
-- The edge may already consist solely of that HYP: closing the checked body
-- with the matched packet spine is still the construction that makes the HYP
-- admissible.
data OrdinaryGammaPacketConstruction = OrdinaryGammaPacketConstruction
  { ogpcBoundBinderRoutes :: ![(TypeBinderRef, TypeBinderRef)],
    ogpcScheme :: !ElabScheme,
    ogpcInstantiation :: !Instantiation
  }
  deriving (Eq, Show)

-- | Construct the missing packet spine from the ordinary body's current
-- Gamma bound.  This is intentionally structural: every non-vacuous bound
-- binder must match one free identity in the checked body, and rebuilding the
-- forall spine with those source identities must be alpha-equivalent to the
-- current bound.  Once that proof is available, the pre-HYP edge prefix
-- describes the packet opening already represented by the checked body and
-- must not be replayed a second time.
constructOrdinaryGammaPacket
  :: ElabType
  -> Maybe (TypeBinderRef, ElabType)
  -> Instantiation
  -> Maybe OrdinaryGammaPacketConstruction
constructOrdinaryGammaPacket bodySourceTy mbConsumerBound edgeInstantiation = do
  (consumerRef, currentBound) <- mbConsumerBound
  (_edgePrefix, resultRef) <- splitOutgoingGammaResult edgeInstantiation
  guard (typeBinderRefsSameIdentity consumerRef resultRef)
  let currentScheme = schemeFromType currentBound
      currentBinders = schemeBinderRefs currentScheme
      currentBody = schemeBody currentScheme
      currentBinderRefs = map fst currentBinders
  guard (not (null currentBinders))
  matched <-
    either
      (const Nothing)
      Just
      (matchTypeRefs currentBinderRefs currentBody bodySourceTy)
  binderRoutes <- traverse (binderRoute matched currentBody) currentBinderRefs
  let activeRoutes = [route | Just route <- binderRoutes]
      sourceIdentities =
        map (typeBinderRefIdentity . snd) activeRoutes
      sourceRefFor boundRef =
        fromMaybe
          boundRef
          ( snd
              <$> find
                (typeBinderRefsSameIdentity boundRef . fst)
                activeRoutes
          )
      renameBoundRefs ty =
        foldl
          (\current (boundRef, sourceRef) ->
              substTypeCaptureRef boundRef (TVarRef sourceRef) current
          )
          ty
          activeRoutes
      sourceBinders =
        [ ( sourceRefFor boundRef,
            fmap (mapBoundType renameBoundRefs) mbBound
          )
        | (boundRef, mbBound) <- currentBinders
        ]
      sourceScheme = mkElabSchemeWithRefs sourceBinders bodySourceTy
  guard (Set.size (Set.fromList sourceIdentities) == length sourceIdentities)
  guard (alphaEqType (schemeToType sourceScheme) currentBound)
  pure
    OrdinaryGammaPacketConstruction
      { ogpcBoundBinderRoutes = activeRoutes,
        ogpcScheme = sourceScheme,
        ogpcInstantiation = InstAbstrRef resultRef
      }
  where
    binderRoute matched currentBody ref
      | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType currentBody)) =
          Just Nothing
      | otherwise = do
          matchedTy <- Map.lookup ref matched
          sourceRef <-
            case matchedTy of
              TVarRef matchedRef -> Just matchedRef
              _ -> Nothing
          guard
            (any (typeBinderRefsSameIdentity sourceRef) (freeTypeVarRefsType bodySourceTy))
          pure (Just (ref, sourceRef))

    splitOutgoingGammaResult inst =
      case inst of
        InstAbstrRef ref -> Just (InstId, ref)
        InstSeq prefix suffix -> do
          (suffixPrefix, ref) <- splitOutgoingGammaResult suffix
          pure (composeInst prefix suffixPrefix, ref)
        _ -> Nothing

-- | Decide whether one construction reference is inherited from the ambient
-- paper Gamma.  Exact declarations owned by the current construction boundary
-- shadow representative-equivalent ambient aliases; all other references use
-- the ordinary identity-or-representative environment lookup.
constructionRefAlreadyInGamma
  :: (NodeId -> NodeId)
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> Bool
constructionRefAlreadyInGamma representative localRefs ambientRefs ref =
  not (any (typeBinderRefsSameIdentity ref) localRefs)
    && isJust (lookupRefByIdentityOrRepresentative representative ambientRefs ref)

-- | Enter an exact construction route only when the annotation sidecar proves
-- that this graph node is the direct declaration site for the outward source
-- identity.  Solved equivalence alone is deliberately insufficient: distinct
-- lexical source binders may share a representative.
directSourceBinderConstructionRename
  :: IntMap.IntMap TypeBinderRef
  -> Int
  -> TypeBinderRef
  -> Maybe (TypeBinderRef, TypeBinderRef)
directSourceBinderConstructionRename directSourceBinderRefs nodeKey outwardRef = do
  sourceRef <- IntMap.lookup nodeKey directSourceBinderRefs
  guard (typeBinderRefsSameIdentity sourceRef outwardRef)
  let graphRef =
        typeBinderRefFromIdentity
          (typeBinderIdentityFromNode (NodeId nodeKey))
          ("t" ++ show nodeKey)
  guard (not (typeBinderRefsSameIdentity graphRef outwardRef))
  pure (graphRef, outwardRef)

-- | Invert one packet construction quotient only when two independent,
-- exact-identity facts agree:
--
-- * the packet records @source -> construction@; and
-- * the direct source sidecar at the construction node records that same
--   source identity.
--
-- This is the child-boundary route needed when recursive elaboration returns
-- a packet-local graph binder (for example @Graph15@) but the enclosing exact
-- parameter has already fixed its semantic source identity (for example
-- @Generated15@).  The concrete target node is part of the certificate, so no
-- representative, display name, or type shape can nominate the inverse.
certifiedSourcePacketOccurrenceRenames
  :: IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
certifiedSourcePacketOccurrenceRenames sourceBinderRefs =
  mapMaybe certified
  where
    certified (sourceRef, constructionRef) = do
      constructionNode <- typeBinderRefNode constructionRef
      (directGraphRef, directSourceRef) <-
        directSourceBinderConstructionRename
          sourceBinderRefs
          (getNodeId constructionNode)
          sourceRef
      guard
        (typeBinderRefsSameIdentity directGraphRef constructionRef)
      pure (constructionRef, directSourceRef)

-- | Recover the exact source view of a packet's operated occurrence from the
-- two identity-bearing stages carried by that same packet.  The operated
-- substitution names the concrete graph occurrence used by the recursively
-- elaborated child; the completed construction substitution names the
-- outward source identity at the same node key.  The independent annotation
-- sidecar must agree with that outward identity.
--
-- This certificate is needed when the packet's explicit construction-renames
-- list routes a source declaration to a different construction node than the
-- copied operated occurrence.  Joining the two substitutions by their exact
-- node key recovers that occurrence before the body is checked against
-- S'(operated), without consulting representatives, display names, or type
-- shape.
certifiedSourcePacketOperatedOccurrenceRenames
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
certifiedSourcePacketOperatedOccurrenceRenames
  sourceBinderRefs
  completedPacketRefs
  operatedPacketRefs =
    mapMaybe certified (IntMap.toList operatedPacketRefs)
    where
      certified (nodeKey, operatedRef) = do
        operatedNode <- typeBinderRefNode operatedRef
        guard (getNodeId operatedNode == nodeKey)
        completedRef <- IntMap.lookup nodeKey completedPacketRefs
        sourceRef <- IntMap.lookup nodeKey sourceBinderRefs
        guard (typeBinderRefsSameIdentity completedRef sourceRef)
        guard (not (typeBinderRefsSameIdentity operatedRef sourceRef))
        pure (operatedRef, sourceRef)

-- | Select the direction of a certified packet/source quotient from the
-- recursively elaborated term, before owner-final and local-Gamma metadata are
-- projected.  Metadata shares the child's identity domain but is not evidence
-- that the term itself contains the graph occurrence.  Letting it choose the
-- direction would incorrectly invert a valid @source -> graph@ construction
-- when only the certificate mentions the graph endpoint.
selectTermSourcePacketOccurrenceRenames
  :: [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
selectTermSourcePacketOccurrenceRenames termRefs =
  filter
    ( \(graphRef, _) ->
        any (typeBinderRefsSameIdentity graphRef) termRefs
    )

-- | Select the source-identity routes that may rewrite occurrences in a
-- recursively constructed term. Unlike declaration ownership, occurrence
-- transport is not restricted to direct source declarations: a copied graph
-- occurrence can retain its concrete source-sidecar key. The route is valid
-- only when both sides independently name the same exact binder identity.
certifiedSourceOccurrenceRoutes
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
certifiedSourceOccurrenceRoutes sourceBinderRefs constructionRoutes =
  IntMap.mapMaybeWithKey certified constructionRoutes
  where
    certified nodeKey outwardRef = do
      sourceRef <- IntMap.lookup nodeKey sourceBinderRefs
      guard (typeBinderRefsSameIdentity sourceRef outwardRef)
      pure outwardRef

-- | Identity-bearing renames for entering the same certified occurrence
-- quotient before recursively elaborating a child.  The source sidecar proves
-- the outward identity; the graph reference is reconstructed solely from the
-- concrete node key that names the occurrence domain.
certifiedSourceOccurrenceRenames
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
certifiedSourceOccurrenceRenames sourceBinderRefs constructionRoutes =
  [ ( graphRef nodeKey outwardRef
    , outwardRef
    )
  | (nodeKey, outwardRef) <-
      IntMap.toList
        ( certifiedSourceOccurrenceRoutes
            sourceBinderRefs
            constructionRoutes
        )
  ]
  where
    graphRef nodeKey outwardRef =
      typeBinderRefFromIdentity
        (typeBinderIdentityFromNode (NodeId nodeKey))
        (typeBinderRefName outwardRef)

-- | Check whether entering one construction route is monotone with respect to
-- the routes already in scope.  Re-entering the exact same route is
-- idempotent, even before either bound is materialized.  Redirecting an
-- existing graph declaration to a different identity is stronger and
-- therefore requires both visible bounds to agree.
constructionRouteBoundCompatible
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeBinderRef
  -> TypeBinderRef
  -> Maybe ElabType
  -> Maybe ElabType
  -> Bool
constructionRouteBoundCompatible activeRenames graphRef outwardRef mbGraphBound mbOutwardBound =
  case find (typeBinderRefsSameIdentity graphRef . fst) activeRenames of
    Nothing -> boundsDoNotConflict
    Just (_, activeTarget)
      | typeBinderRefsSameIdentity activeTarget outwardRef ->
          boundsDoNotConflict
      | otherwise ->
          boundsAgree
  where
    boundsDoNotConflict =
      case (mbGraphBound, mbOutwardBound) of
        (Just graphBound, Just outwardBound) ->
          typesAgree graphBound outwardBound
        _ -> True
    boundsAgree =
      case (mbGraphBound, mbOutwardBound) of
        (Just graphBound, Just outwardBound) ->
          typesAgree graphBound outwardBound
        _ -> False
    typesAgree left right =
      alphaEqType left right || churchAwareEqType left right

-- | Select the declaration identities that a construction boundary may keep
-- local while computing @Gen(Gamma, tau)@.  The final construction route is
-- authoritative: an excluded graph identity routed to an exact ambient
-- identity is already free in Gamma and must not be protected from
-- subtraction.  That route may already be in the scheme substitution or may
-- still live in the ambient construction sidecar; the latter takes precedence
-- because it is what alignment will publish.  A genuinely local route protects
-- both the graph identity and its outward construction identity, so later
-- alignment cannot split one declaration into two binders.
constructionProtectedIdentities
  :: (TypeBinderRef -> Bool)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Set.Set TypeBinderIdentity
  -> Set.Set TypeBinderIdentity
constructionProtectedIdentities ambientOwnsExact constructionRoutes schemeInfo =
  Set.foldl' protect Set.empty
  where
    protect protected identity
      | ambientOwnsExact authoritativeRef = protected
      | otherwise =
          Set.insert
            (typeBinderRefIdentity authoritativeRef)
            (Set.insert identity protected)
      where
        originalRef =
          typeBinderRefFromIdentity
            identity
            (typeBinderIdentityStableName identity)
        authoritativeRef =
          fromMaybe schemeRef $ do
            sourceNode <- typeBinderIdentityNode identity
            IntMap.lookup (getNodeId sourceNode) constructionRoutes
        schemeRef =
          fromMaybe originalRef $ do
            sourceNode <- typeBinderIdentityNode identity
            IntMap.lookup (getNodeId sourceNode) (schemeInfoBinderRefSubst schemeInfo)

lookupRefByIdentityOrRepresentative
  :: (NodeId -> NodeId)
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> Maybe TypeBinderRef
lookupRefByIdentityOrRepresentative representative refs ref =
  find (typeBinderRefsSameIdentity ref) refs
    <|> find sameGraphIdentityRepresentative refs
  where
    sameGraphIdentityRepresentative candidate =
      case (typeBinderRefNode ref, typeBinderRefNode candidate) of
        (Just refNode, Just candidateNode) ->
          representative refNode == representative candidateNode
        _ -> False

-- | Decide whether a transparent RHS packet's open result is already
-- completed by its enclosing let scheme.  There are exactly two legal forms:
-- the enclosing scheme publishes an identity-equivalent binder/reference, or
-- its whole scheme is closed and therefore owns the specialization of this
-- exact RHS result.  The latter includes a graph-generalized closed scheme:
-- packet ownership plus the transparent source path, rather than incidental
-- binder identity, is the proof that it completes this result.
transparentResultResolvedByEnclosingScheme
  :: (NodeId -> NodeId)
  -> SchemeInfo
  -> TypeBinderRef
  -> SchemeInfo
  -> Bool
transparentResultResolvedByEnclosingScheme representative packetSchemeInfo resultRef enclosingSchemeInfo =
  packetOwnsResult
    && ( any
           (sameConstructionIdentity resultRef)
           enclosingRefs
           || null enclosingFreeRefs
       )
  where
    -- A completed packet can specialize its open result all the way to a
    -- closed type, so that identity no longer occurs free in the packet body.
    -- Its SchemeInfo substitution is the construction-time route that still
    -- proves ownership of the exact result occurrence.
    packetOwnsResult =
      refMember resultRef packetFreeRefs
        || any
          (typeBinderRefsSameIdentity resultRef)
          (IntMap.elems (schemeInfoBinderRefSubst packetSchemeInfo))
    packetFreeRefs =
      freeTypeVarRefsType (schemeToType (siScheme packetSchemeInfo))
    enclosingBinders = schemeBinderRefs (siScheme enclosingSchemeInfo)
    enclosingFreeRefs =
      freeTypeVarRefsType (schemeToType (siScheme enclosingSchemeInfo))
    enclosingRefs = map fst enclosingBinders ++ enclosingFreeRefs

    sameConstructionIdentity packetRef enclosingRef =
      typeBinderRefsSameIdentity packetRef enclosingRef
        || or
          [ representative packetNode == representative enclosingNode
          | packetNode <- constructionNodes packetSchemeInfo packetRef
          , enclosingNode <- constructionNodes enclosingSchemeInfo enclosingRef
          ]

    constructionNodes schemeInfo ref =
      foldr insertNode (maybeToList (typeBinderRefNode ref))
        [ NodeId nodeKey
        | (nodeKey, routedRef) <-
            IntMap.toList (schemeInfoBinderRefSubst schemeInfo)
        , typeBinderRefsSameIdentity ref routedRef
        ]

    insertNode node nodes
      | node `elem` nodes = nodes
      | otherwise = node : nodes

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Add the source identities currently available from construction Gamma.
-- A concrete alias is always authoritative for its own graph occurrence. Its
-- solved representative is also a usable route unless the prepared source
-- sidecar already proves that the representative belongs to a distinct
-- lexical source binder. In that case the solved class is an alpha-equivalent
-- graph quotient, not an identity quotient, so retain both concrete routes.
mergeConstructionSourceBinderRefs
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
mergeConstructionSourceBinderRefs representative sourceRefs constructionAliases =
  Right (IntMap.union (IntMap.fromList representativeRoutes) concreteRefs)
  where
    -- A boundary-local alias identifies this concrete occurrence even when a
    -- different source occurrence was solved to the same node and therefore
    -- occupies the global node-keyed sidecar.  Keep the foreign source route
    -- at the representative, but let the local construction own its concrete
    -- key.
    concreteAliases =
      IntMap.filter
        (isJust . typeBinderIdentityGeneratedUnique . typeBinderRefIdentity)
        constructionAliases
    concreteRefs = IntMap.union concreteAliases sourceRefs

    representativeRoutes =
      [ (representativeKey, outwardRef)
      | (representativeKey, candidates) <-
          IntMap.toList representativeCandidatesByNode
      , IntMap.notMember representativeKey concreteRefs
      , [outwardRef] <- [uniqueIdentityRefs candidates]
      ]

    representativeCandidatesByNode =
      IntMap.fromListWith (++)
        [ (representativeKey, [outwardRef])
        | (nodeKey, outwardRef) <- IntMap.toList concreteAliases
        , let representativeKey = getNodeId (representative (NodeId nodeKey))
        , representativeKey /= nodeKey
        ]

    -- Several concrete occurrences may share a solved graph representative.
    -- Publish the representative route only when all of them prove the same
    -- semantic binder identity; otherwise the representative is intentionally
    -- ambiguous and the concrete routes remain the sole authorities.
    uniqueIdentityRefs = foldr insertUnique []

    insertUnique ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

-- | Build the source-binder view that may decide local Gamma ownership.
-- Direct declaration keys are semantic authority; solved/copy aliases in the
-- expanded carrier are only lookup routes.  Exact ambient Gamma certificates
-- are admitted through their separately validated authority channel.
sourceBinderAuthorityRefs
  :: (NodeId -> NodeId)
  -> IntSet.IntSet
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap AmbientGammaAuthority
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
sourceBinderAuthorityRefs representative directSourceKeys sourceRefs ambientAuthorities =
  mergeConstructionSourceBinderRefs
    representative
    (IntMap.restrictKeys sourceRefs directSourceKeys)
    (IntMap.map agaExactRef ambientAuthorities)
