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
    completeLambdaParamBoundaryBound,
    completeLambdaParamBoundarySourceRootBound,
    completeLambdaParamBoundaryDeclarationBound,
    completeLambdaParamBoundaryDeclarationBoundInScope,
    completeLambdaParamBoundaryType,
    renameLambdaParamBoundaryCertificate,
    OrdinaryGammaPacketConstruction (..),
    ValidatedBodyConsumerProjection,
    BodyConsumerDeclarationAuthority,
    BodyConsumerBoundRefinementCertificate,
    bodyConsumerBoundRefinementCertifiesTransition,
    bodyConsumerBoundRefinementAppliesToDeclarationState,
    bodyConsumerBoundRefinementTopologyResultRefAtConstruction,
    bodyConsumerBoundRefinementCompletedTopologyEndpoint,
    bodyConsumerBoundRefinementsCompletePacketBound,
    bodyConsumerBoundRefinementEmittedBy,
    bodyConsumerBoundRefinementCompletesOwnerEndpoint,
    bodyConsumerBoundRefinementCompletesSchemeDeclaration,
    bodyConsumerBoundRefinementRequiresSchemeCompletion,
    bodyConsumerBoundRefinementRequiresOwnerEmission,
    bodyConsumerBoundRefinementExcludesAmbientRef,
    finalizeBodyConsumerBoundRefinementAtOwner,
    bodyConsumerBoundRefinementSurvivesOwnerBoundary,
    bodyConsumerBoundRefinementConsumesAny,
    bodyConsumerBoundRefinementConsumedDependencies,
    bodyConsumerBoundRefinementConsumedReplayRoutes,
    bodyConsumerBoundRefinementTargetsAny,
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
    certifyLambdaBodyConstruction,
    CertifiedOpenValueLambdaParameterClosure,
    certifyOpenValueLambdaParameterClosure,
    ExactLambdaConstructionPlan,
    exactLambdaConstructionBinders,
    exactLambdaConstructionPublishedBinders,
    exactLambdaConstructionPublishedType,
    exactLambdaConstructionBinderRenames,
    exactLambdaConstructionBodyBinderRenames,
    exactLambdaConstructionBodyAbstractions,
    exactLambdaConstructionBodyInstantiation,
    exactLambdaConstructionBodyType,
    exactLambdaConstructionPreservedBodyRefinements,
    exactLambdaConstructionAmbientBodyRefinement,
    exactLambdaConstructionAmbientBodyRefinementCertificate,
    exactLambdaConstructionCompletionInstantiation,
    exactLambdaConstructionCompletionPreservesBinderIdentities,
    certifyExactLambdaConstruction,
    certifyExactLambdaEndpointConstruction,
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
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
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
    churchAwareEqType,
    churchRepresentationEqType,
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
    typeBinderRefIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    freshTypeBinderRef,
  )
import MLF.Types.Identity
  ( StructuralTypeBinderRole (StructuralSelfBinder),
    typeBinderIdentityGeneratedUnique,
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
    , certifiedLambdaBodyReturnedResults ::
        ![(LocalGammaOwner, ElabType)]
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
  -> [(LocalGammaOwner, ElabType)]
  -> Maybe CertifiedLambdaBodyConstruction
certifyLambdaBodyConstruction bodyOwner constructedTy certifiedBinders consumedBinders sourceRefs parameterRefs returnedResults = do
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
    ( all
        ( returnedResultBelongsToConstructedType
            . snd
        )
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
      , certifiedLambdaBodyReturnedResults = returnedResults
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

    -- The returned-result certificate is projected from the checked owner,
    -- but keep the projection sealed only when its endpoint is actually on
    -- that owner's result path.  This validates provenance already carried by
    -- the owner certificate; it does not infer a result owner from type shape.
    returnedResultBelongsToConstructedType returnedTy =
      resultPathContains constructedTy
      where
        resultPathContains currentTy
          | operationalEndpointTypesAgree currentTy returnedTy = True
        resultPathContains (TForallRef _ _ bodyTy) =
          resultPathContains bodyTy
        resultPathContains (TArrow _ resultTy) =
          resultPathContains resultTy
        resultPathContains _ = False

certifyOpenValueLambdaParameterClosure
  :: ElabType
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
  -> Either [String] CertifiedOpenValueLambdaParameterClosure
certifyOpenValueLambdaParameterClosure closedTy openTy mbBodyConstruction = do
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
    closedScheme = schemeFromType closedTy
    closedBinders = schemeBinderRefs closedScheme
    openedSource = schemeBody closedScheme

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
  :: LocalGammaOwner
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
        sourceOwnerConstructsChild =
          operationalEndpointTypesAgree
            completedExpectedEndpoint
            completedConstructedBodyType
            || isJust
              ( planExactBinderSpine
                  operationalEndpointTypesAgree
                  completedExpectedEndpoint
                  completedConstructedBodyType
              )
    unless
      ( sourceOwnerConstructsChild
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
  , exactLambdaConstructionBodyBinderRenames ::
      ![(TypeBinderRef, TypeBinderRef)]
  , exactLambdaConstructionBodyAbstractions ::
      ![(TypeBinderRef, Maybe BoundType)]
  , exactLambdaConstructionBodyInstantiation :: !Instantiation
  , exactLambdaConstructionBodyType :: !ElabType
  , exactLambdaConstructionPreservedBodyRefinements ::
      ![BodyConsumerBoundRefinementCertificate]
  , exactLambdaConstructionAmbientBodyRefinement ::
      !(Maybe (TypeBinderRef, ElabType, ElabType))
  , exactLambdaConstructionAmbientBodyRefinementCertificate ::
      !(Maybe BodyConsumerBoundRefinementCertificate)
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
            freshTypeBinderRef
              (typeBinderRefName sourceRef)
              generator
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
        constructionBinders

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
    certifyLambdaAtExpectedEndpoint
      ambientBindings
      rawCandidates
      paramTy
      expectedTy
      ( \constructionCandidates lambdaTy ->
        certifyExactLambdaEndpointConstructionAtLambdaType
          ambientBindings
          reservedBodyBinderRefs
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
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaEndpointConstructionAtLambdaType
  ambientBindings
  reservedBodyBinderRefs
  rawCandidates
  paramTy
  bodySourceTy
  expectedTy = do
    ensureDistinct "construction" (map fst constructionCandidates)
    ensureDistinct "endpoint" (map fst expectedBinders)
    expectedBodyTy <-
      case schemeBody expectedScheme of
        TArrow expectedParamTy bodyTy
          | exactLambdaEndpointTypesAgree expectedParamTy paramTy ->
              pure bodyTy
        body ->
          constructionFailure
            "the inherited exact endpoint is not this lambda"
            ["  endpoint body: " ++ show body]
    let sourceBinderRefs =
          map fst (schemeBinderRefs (schemeFromType bodySourceTy))
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
                    (TArrow bodySourceTy expectedTy)
                    (map TVarRef endpointReservedRefs)
                )
            )
            collidingSourceBinderRefs
        freshBodySourceTy =
          renameTypeBinderRefPayloads bodyBinderRenames bodySourceTy
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
    bodyInstantiation0 <-
      requireMaybe
        "the checked body cannot construct the inherited exact codomain"
        ( constructExactInstantiation
            constructionTypeEnv
            exactLambdaEndpointTypesAgree
            freshBodySourceTy
            expectedBodyTy
        )
    certifiedBodyTy <-
      either
        ( \cause ->
            constructionFailure
              "the inherited exact body computation does not typecheck"
              ["  cause: " ++ show cause]
        )
        pure
        ( TypeCheck.checkInstantiation
            constructionTypeEnv
            freshBodySourceTy
            bodyInstantiation0
        )
    unless
      (exactLambdaEndpointTypesAgree certifiedBodyTy expectedBodyTy)
      ( constructionFailure
          "the inherited exact body computation reaches a different codomain"
          ["  constructed codomain: " ++ show certifiedBodyTy]
      )
    let constructionTy0 =
          schemeToType
            ( mkElabSchemeWithRefs
                constructionCandidates
                (TArrow paramTy expectedBodyTy)
            )
    spinePlan <-
      requireMaybe
        "the local lambda Gamma cannot construct the inherited exact endpoint"
        ( planExactBinderSpine
            exactLambdaEndpointTypesAgree
            constructionTy0
            expectedTy
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
        alignedParamTy = alignType paramTy
        alignedBodyTy = alignType expectedBodyTy
        alignedPublishedTy = alignType expectedTy
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
        , exactLambdaConstructionBodyBinderRenames = bodyBinderRenames
        , exactLambdaConstructionBodyAbstractions = []
        , exactLambdaConstructionBodyInstantiation = bodyInstantiation
        , exactLambdaConstructionBodyType = alignedBodyTy
        , exactLambdaConstructionPreservedBodyRefinements = []
        , exactLambdaConstructionAmbientBodyRefinement = Nothing
        , exactLambdaConstructionAmbientBodyRefinementCertificate = Nothing
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
      let (freshRef, nextGenerator) =
            freshTypeBinderRef
              (typeBinderRefName sourceRef)
              generator
       in (renames ++ [(sourceRef, freshRef)], nextGenerator)

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
        -- Prefer a construction that publishes the selected endpoint
        -- unchanged.  Only after every such branch has been tried may one of
        -- the explicitly certified completion constructors supersede the
        -- provisional packet endpoint.  This ordering prevents a valid exact
        -- plan from being shadowed by an earlier, more specialized sibling
        -- completion.
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
        ]
  where
    selectExactExpectedPlan rejected [] =
      constructionFailure
        "no certified lambda construction publishes the selected exact endpoint"
        [ "  selected endpoint: " ++ show expectedTy
        , "  rejected publication transitions: " ++ show (reverse rejected)
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
      if exactLambdaEndpointTypesAgree
          (exactLambdaConstructionPublishedType plan)
          expectedTy
          || exactLambdaEndpointTypesAgree
            (exactLambdaConstructionPublishedType plan0)
            expectedTy
          || completesSelectedEndpoint
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
              )
                : rejected
            )
            remainingPlans

    exactCandidate plan = (False, plan)
    completionCandidate plan = (True, plan)

    completeConsumedBodyConsumerPlan plan = do
      completedConstructionScheme <-
        consumeCertifiedBodyConsumerConstructionScheme
          bodyRefinements
          ( mkElabSchemeWithRefs
              (exactLambdaConstructionBinders plan)
              ( TArrow
                  paramTy
                  (exactLambdaConstructionBodyType plan)
              )
          )
      completedPublishedScheme <-
        consumeCertifiedBodyConsumerConstructionScheme
          bodyRefinements
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
    expectedFreeRefs = freeTypeVarRefsType expectedTy
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
    -- same identity and bound before it can be opened ambiently.
    endpointAmbientCandidates =
      [ (expectedRef, candidateRef, ambientBound)
      | (candidateRef, candidateBound) <- rawCandidates
      , expectedRef <- expectedFreeRefs
      , typeBinderRefsSameIdentity candidateRef expectedRef
      , ambientBound <-
          maybeToList
            ( endpointCandidateAmbientBound
                expectedRef
                candidateBound
            )
      ]
    endpointCandidateAmbientBound expectedRef candidateBound =
      case matchingAmbientBounds of
        []
          | isNothing candidateBound -> Just TBottom
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
      | candidate@(candidateRef, _) <- rawCandidates
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
    completedConstructionCandidates =
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
          certifiedBodyConstructionCompletion candidateRef =
          (candidateRef, Just completedBound)
      | Just completedBound <-
          certifiedBodyResultOwnerCompletion candidateRef =
          (candidateRef, Just completedBound)
      | Just completedBound <-
          certifiedDescendantCompletion candidateRef =
          (candidateRef, Just completedBound)
      | otherwise = binder

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
        ++ maybeToList completedEnclosingConsumerBottomEndpoint

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

    -- An administrative lambda can construct a pending consumer owned by its
    -- enclosing lambda rather than a declaration emitted by this local
    -- Gamma.  The packet records the enclosing consumer identity, its
    -- unbounded construction slot, and the exact Gamma-bound skeleton before
    -- Typ(body) is known.  Once the body has been checked, complete only that
    -- certified Bottom codomain; neither a candidate identity nor type shape
    -- alone is sufficient authority.
    completedEnclosingConsumerBottomEndpoint = do
      consumerAuthority <- subtermGeneralizationConsumerAuthority packet
      _enclosingOwner <-
        subtermConsumerAuthorityEnclosingOwner consumerAuthority
      [pendingConsumerRef] <-
        pure
          [ ref
          | (ref, Nothing) <- packetConstructionBinders
          , typeBinderRefIdentity ref
              == scaConsumerIdentity consumerAuthority
          ]
      guard
        ( typeBinderRefIdentity pendingConsumerRef
            == scaConsumerIdentity consumerAuthority
        )
      guard
        ( operationalEndpointTypesAgree
            ( schemeToType
                (subtermGeneralizationGammaBoundScheme packet)
            )
            expectedTy
        )
      TArrow expectedParamTy TBottom <-
        pure (schemeBody expectedScheme)
      guard (operationalEndpointTypesAgree expectedParamTy paramTy)
      pure
        ( schemeToType
            ( mkElabSchemeWithRefs
                expectedBinders
                (TArrow expectedParamTy bodyResultTy)
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

    certifyBodyBinderSpineAcrossLambdaAt certifiedBodyBinders publishedTy = do
      let bodyScheme = schemeFromType bodySourceTy
          bodyBinders = schemeBinderRefs bodyScheme
          bodyResidual = schemeBody bodyScheme
          publishedScheme = schemeFromType publishedTy
          publishedBinders = schemeBinderRefs publishedScheme
      projectedExpectedBinders <-
        traverse sourceProjectExpectedBinder publishedBinders
      let sourceProjectedExpectedScheme =
            mkElabSchemeWithRefs
              projectedExpectedBinders
              (schemeBody publishedScheme)
          sourceProjectedExpectedTy =
            schemeToType sourceProjectedExpectedScheme
          sourceProjectedExpectedBinders =
            schemeBinderRefs sourceProjectedExpectedScheme
      guard (not (null bodyBinders))
      liftedPairs <-
        traverse
          (liftedBodyBinder sourceProjectedExpectedBinders)
          bodyBinders
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
      TArrow expectedParamTy expectedBodyTy <-
        pure (schemeBody sourceProjectedExpectedScheme)
      guard
        ( operationalEndpointTypesAgree expectedParamTy paramTy
            && operationalEndpointTypesAgree bodyResidual expectedBodyTy
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
      bodySpineSpecialization <-
        directPlan
      plan <-
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
      pure
        plan
          { exactLambdaConstructionBodyBinderRenames =
              bodyBinderRenames
                ++ exactLambdaConstructionBodyBinderRenames plan
          }
      where
        liftedBodyBinder projectedExpectedBinders bodyBinder@(bodyRef, bodyBound) = do
          let expectedMatches =
                [ binder
                | binder@(ref, bound) <- projectedExpectedBinders
                , typeBinderRefsSameIdentity bodyRef ref
                , lambdaConstructionBinderBoundsAgree bodyBound bound
                ]
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
            ( typeBinderRefsSameIdentity bodyRef expectedRef
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
          , bcbrOwnerFinalized certificate
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
      , not (bcbrOwnerFinalized certificate)
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
      let (freshRef, nextGenerator) =
            freshTypeBinderRef
              (typeBinderRefName sourceRef)
              generator
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
          [ expectedWithCompletedDependencies completedCandidates
          , expectedTy
          ]
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
    expectedWithCompletedDependencies completedCandidates =
      case completedDependencyBinders completedCandidates of
        [] -> expectedTy
        dependencyBinders ->
          schemeToType
            ( mkElabSchemeWithRefs
                (dependencyBinders ++ expectedBinders)
                (schemeBody expectedScheme)
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

    -- A descendant can finish a construction-owned packet body after the
    -- enclosing endpoint has already frozen an older bounded presentation.
    -- The checked body supplies an exact leading forall spine and residual;
    -- match those declarations to the packet candidates by identity, install
    -- their checked bounds, and use the residual as the exact packet-body
    -- bound.  The resulting @InstApp ...; Hyp(result)@ is then validated by
    -- the ordinary exact binder-spine planner.
    --
    -- This completion is unavailable to source-owned binders.  A source
    -- forall's bound and vacuity are ABI, whereas the packet identities here
    -- are construction slots whose metadata explicitly records that
    -- ownership.
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
      where
        checkedBodyDependencyCandidate candidatePrefix (checkedRef, _) = do
          candidate <-
            find
              (typeBinderRefsSameIdentity checkedRef . fst)
              candidatePrefix
          guard
            ( constructionOwnsPacketBinder (fst candidate)
                && not (sourceOwnsPacketBinder (fst candidate))
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
      let bodyInstantiation0 =
            composeInst
              checkedBodyInstantiation0
              bodyCompletionInstantiation
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
              checkedBodySourceTy
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
            constructedBodyTy0
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
          peerBodyTy = alignPeerType constructedBodyTy0
          peerPublishedTy0 = alignPeerType publishedTy
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
          alignedBodyTy = alignType constructedBodyTy0
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
          , exactLambdaConstructionBodyBinderRenames = []
          , exactLambdaConstructionBodyAbstractions = bodyAbstractions
          , exactLambdaConstructionBodyInstantiation = bodyInstantiation
          , exactLambdaConstructionBodyType = alignedBodyTy
          , exactLambdaConstructionPreservedBodyRefinements = []
          , exactLambdaConstructionAmbientBodyRefinement =
              ambientBodyRefinement
          , exactLambdaConstructionAmbientBodyRefinementCertificate =
              ambientBodyRefinementCertificate
          , exactLambdaConstructionCompletionInstantiation =
              emittedCompletionInstantiation
          }

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
          BodyConsumerBoundRefinementCertificate
            { bcbrDeclarationAuthority =
                BodyConsumerLocallyEmitted route completedBound
            , bcbrOwnerFinalized = False
            , bcbrAmbientRef = ambientRef
            , bcbrPreviousBound = previousBound
            , bcbrCompletedBound = completedBound
            }

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
          BodyConsumerBoundRefinementCertificate
            { bcbrDeclarationAuthority =
                BodyConsumerInheritedAmbient route previousBound
            , bcbrOwnerFinalized = False
            , bcbrAmbientRef = ambientRef
            , bcbrPreviousBound = previousBound
            , bcbrCompletedBound = completedBound
            }

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
  | BodyConsumerOrdinaryOwnerEmission
      !BodyConsumerRoute
  | BodyConsumerConsumedAtOwner
      !BodyConsumerRoute
      !ElabType
  deriving (Eq, Show)

-- | Recover the declaration and completed bound only after its exact local
-- owner has finished emitting it.  Pending and ordinary owner-emission states
-- deliberately retain their provenance constructor after finalization; the
-- finalized bit proves that the future-owner state has crossed its owner
-- boundary.  Ambient and consumed states are excluded by construction.
finalizedLocalBodyConsumerDeclaration
  :: BodyConsumerBoundRefinementCertificate
  -> Maybe (BodyConsumerRoute, ElabType)
finalizedLocalBodyConsumerDeclaration certificate
  | not (bcbrOwnerFinalized certificate) = Nothing
  | otherwise =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerLocallyEmitted route declaredBound
          | operationalEndpointTypesAgree declaredBound completedBound ->
              Just (route, declaredBound)
          | otherwise -> Nothing
        BodyConsumerPendingOwnerEmission route ->
          Just (route, completedBound)
        BodyConsumerOrdinaryOwnerEmission route ->
          Just (route, completedBound)
        _ -> Nothing
  where
    completedBound = bcbrCompletedBound certificate

-- | A construction proof that one provisional declaration was completed by
-- the exact lambda-body consumer that owns it.  The constructor is private: a
-- caller can obtain this value only by joining either a validated body
-- projection with its declaration authority or a packet-local pending
-- declaration with the recursively checked child that completed it.  Root
-- planning and enclosing packet placement may therefore replay the completed
-- bound without inferring it from the finished term or from type shape.
data BodyConsumerBoundRefinementCertificate =
  BodyConsumerBoundRefinementCertificate
    { bcbrDeclarationAuthority :: !BodyConsumerDeclarationAuthority,
      -- | Whether the exact route owner has completed its constructor.
      -- Before this point an enclosing wrapper must preserve the certificate
      -- even when its own result does not mention the provisional
      -- declaration: the declaration still belongs to a future owner.  After
      -- this point the first boundary that no longer carries the identity may
      -- convert it to historical packet-completion provenance.
      bcbrOwnerFinalized :: !Bool,
      bcbrAmbientRef :: !TypeBinderRef,
      bcbrPreviousBound :: !ElabType,
      bcbrCompletedBound :: !ElabType
    }
  deriving (Eq, Show)

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
    guard (bcbrOwnerFinalized certificate)
    (route, declaredBound) <-
      case bcbrDeclarationAuthority certificate of
        BodyConsumerConsumedAtOwner consumedRoute bound ->
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
  bcbrOwnerFinalized certificate
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
  | bcbrOwnerFinalized certificate
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
      | bcbrOwnerFinalized certificate
      , BodyConsumerConsumedAtOwner route consumedBound <-
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
      previousBound
      (bcbrPreviousBound certificate)
    && operationalEndpointTypesAgree
      completedBound
      (bcbrCompletedBound certificate)

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
    [ authorizedBodyConsumerDeclarationBound
        (bcbrDeclarationAuthority certificate)
    , bcbrPreviousBound certificate
    , bcbrCompletedBound certificate
    ]

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
          | not (bcbrOwnerFinalized refinement) = False
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
    BodyConsumerPendingOwnerEmission route ->
      bcrOwner route == owner
    BodyConsumerOrdinaryOwnerEmission route ->
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
    -- the route owner consumes the provisional declaration.  Finalizing the
    -- owner changes the declaration's liveness, not the scope of its source
    -- forall spine.
    privateOperatedBinder =
      any
        (typeBinderRefsSameIdentity ref . fst)
        ( schemeBinderRefs
            ( schemeFromType
                ( bcrOperatedType
                    ( authorizedBodyConsumerRoute
                        (bcbrDeclarationAuthority certificate)
                    )
                )
            )
        )

-- | Advance an owner-targeted refinement after that owner has completed its
-- constructor.  Local and ambient liveness remain distinct: an exact
-- enclosing packet can consume a would-be local declaration into its ambient
-- Gamma, in which case later root validation must no longer require the
-- completed owner's local closure.  A declaration that remains locally
-- emitted retains its original authority; a declaration absent from both
-- classes becomes historical completion proof only.
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
      certificate
        { bcbrDeclarationAuthority =
            BodyConsumerLocallyEmitted
              route
              (bcbrCompletedBound certificate)
        , bcbrOwnerFinalized = True
        }
  | ownerHasCompleted
  , not targetRemainsLocal
  , targetRemainsAmbient =
      certificate
        { bcbrDeclarationAuthority =
            BodyConsumerEnclosingAmbient
              route
              ( authorizedBodyConsumerDeclarationBound
                  (bcbrDeclarationAuthority certificate)
              )
        , bcbrOwnerFinalized = True
        }
  | ownerHasCompleted
  , not (targetRemainsLocal || targetRemainsAmbient) =
      certificate
        { bcbrDeclarationAuthority =
            BodyConsumerConsumedAtOwner
              route
              (bcbrCompletedBound certificate)
        , bcbrOwnerFinalized = True
        }
  | bcrOwner route == owner =
      certificate {bcbrOwnerFinalized = True}
  | otherwise = certificate
  where
    ownerHasCompleted =
      bcbrOwnerFinalized certificate
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
  guard (bcbrOwnerFinalized certificate)
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
                    BodyConsumerBoundRefinementCertificate
                      { bcbrDeclarationAuthority =
                          BodyConsumerLocallyEmitted route completedBound
                      , bcbrOwnerFinalized = False
                      , bcbrAmbientRef = resultRef
                      , bcbrPreviousBound = previousBound
                      , bcbrCompletedBound = completedBound
                      }
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
    BodyConsumerBoundRefinementCertificate
      { bcbrDeclarationAuthority =
          BodyConsumerInheritedAmbient route previousBound
      , bcbrOwnerFinalized = False
      , bcbrAmbientRef = ambientRef
      , bcbrPreviousBound = previousBound
      , bcbrCompletedBound = completedBound
      }
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
      BodyConsumerBoundRefinementCertificate
        { bcbrDeclarationAuthority =
            BodyConsumerLocallyEmitted route completedBoundAtConstruction
        , bcbrOwnerFinalized = False
        , bcbrAmbientRef = completedRef
        , bcbrPreviousBound = previousBound
        , bcbrCompletedBound = completedBoundAtConstruction
        }
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
          | not (bcbrOwnerFinalized certificate) -> do
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
                    && typeBinderRefsSameIdentity constructionRef targetRef
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
        Nothing -> do
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

    planBodyConsumerBoundInstallation certificate currentBound
      | bodyConsumerBoundRefinementAcceptsDeclarationState
          certificate
          currentBound =
          Just
            ( InstallCompletedBodyConsumerBound
                (bcbrCompletedBound certificate)
            )
      | ordinaryOwnerCertificateClosesOpenedDeclaration
          certificate
          currentBound =
          Just
            ( InstallCompletedBodyConsumerBound
                (bcbrCompletedBound certificate)
            )
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

    -- An ordinary future owner has no pending scheme to replay, so its exact
    -- packet/closure pair is the declaration authority.  A nested application
    -- can enter that already checked bounded declaration by opening its
    -- leading binder into the construction Gamma before the future owner is
    -- reached.  Close that opened view again only when the certificate
    -- recorded the completed declaration as both its incoming and constructed
    -- endpoint.  Opening that exact identity-bearing declaration must produce
    -- the current bound.
    -- This is a construction state transition selected by owner/edge/exterior
    -- authority, not an equality inferred from the eventual term type.
    ordinaryOwnerCertificateClosesOpenedDeclaration certificate currentBound =
      case bcbrDeclarationAuthority certificate of
        BodyConsumerOrdinaryOwnerEmission route ->
          not (bcbrOwnerFinalized certificate)
            && operationalEndpointTypesAgree
              (bcbrPreviousBound certificate)
              completedBound
            && operationalEndpointTypesAgree
              (bcrOperatedType route)
              completedBound
            && operationalEndpointTypesAgree
              (bcrConstructionOperatedType route)
              completedBound
            && leadingBinderIsBounded
            && operationalEndpointTypesAgree openedDeclaration currentBound
            && any
              (\ref -> any (typeBinderRefsSameIdentity ref) openedBinderRefs)
              (freeTypeVarRefsType currentBound)
          where
            completedBound = bcbrCompletedBound certificate
            openedScheme = schemeFromType completedBound
            openedBinders = schemeBinderRefs openedScheme
            openedBinderRefs = map fst openedBinders
            openedDeclaration = schemeBody openedScheme
            leadingBinderIsBounded =
              case openedBinders of
                (_, Just _) : _ -> True
                _ -> False
        _ -> False

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
        BodyConsumerPendingOwnerEmission route
          | not (bcbrOwnerFinalized certificate)
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
        && ( exactUnboundedForallClosureOf
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
        BodyConsumerPendingOwnerEmission route ->
          typeBinderRefsSameIdentity
            (bcrConstructionRef route)
            (bcbrAmbientRef certificate)
        BodyConsumerOrdinaryOwnerEmission route ->
          typeBinderRefsSameIdentity
            (bcrConstructionRef route)
            (bcbrAmbientRef certificate)
        _ -> False

    finalizedLocalDeclaration certificate
      | not (bcbrOwnerFinalized certificate) = False
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
        BodyConsumerPendingOwnerEmission route
          | bcrOwner route == owner -> Just route
        BodyConsumerOrdinaryOwnerEmission route
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

    endpointCanAdvance _ Nothing = True
    endpointCanAdvance certificate (Just endpoint) =
      bodyConsumerBoundRefinementAcceptsDeclarationState
        certificate
        endpoint
        || completeUnboundedForallSpecializesTo
          endpoint
          (bcbrPreviousBound certificate)
        || bodyConsumerEndpointIsCertifiedCompletion
          certificate
          endpoint

    selectOwnerEndpoint route certificate sharedExteriorEdges =
      case
          [ mbEndpoint
          | (edgeId, mbEndpoint) <- sharedExteriorEdges
          , edgeId == lgoBoundaryEdge owner
          ]
        of
          [] -> selectCommonGeneralizedCompletion route certificate
          [Nothing] -> selectCommonGeneralizedCompletion route certificate
          [Just endpoint]
            | operationalEndpointTypesAgree
                endpoint
                (bcbrCompletedBound certificate) ->
                pure (bcbrCompletedBound certificate)
            | bodyConsumerEndpointIsCertifiedCompletion
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
    selectCommonGeneralizedCompletion route certificate =
      case generalizedRepresentatives of
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
      where
        siblingCertificates =
          [ siblingCertificate
          | (siblingRoute, siblingCertificate) <- ownedCertificates
          , bcrExteriorNode siblingRoute == bcrExteriorNode route
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
            || bodyConsumerCompletedDeclarationSpecializesToEndpoint
              candidate
              (bcbrCompletedBound endpoint)

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
        BodyConsumerPendingOwnerEmission route
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
        BodyConsumerOrdinaryOwnerEmission route
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
        ( pendingRequirementFor
            route
            previousBound
            (rgbOperatedType requirement)
            || bodyConsumerEndpointIsCertifiedCompletion
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
      requireExteriorRoute
        "current construction"
        exterior
        currentSchemeInfo
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
    (ambientRef, previousBound) <-
      case
          [ binding
          | binding@(ref, _) <- Map.toList ambientBindings
          , typeBinderRefsSameIdentity ref currentRef
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
          && typeBinderRefsSameIdentity ambientRef currentRef
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
            , bcrConstructionRef = ambientRef
            , bcrOperatedType = operatedAtBoundary
            , bcrConstructionOperatedType = completedBound
            }
        certificate =
          BodyConsumerBoundRefinementCertificate
            { bcbrDeclarationAuthority =
                BodyConsumerInheritedAmbient route previousBound
            , bcbrOwnerFinalized = False
            , bcbrAmbientRef = ambientRef
            , bcbrPreviousBound = previousBound
            , bcbrCompletedBound = completedBound
            }
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
  -> [LambdaParamBoundaryCertificate]
  -> ElabType
  -> Maybe CertifiedLambdaBodyConstruction
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
  parameterBoundaryCertificates
  checkedBodyType
  certifiedBodyConstruction
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
    closure <-
      case subtermGeneralizationLocalConsumerClosure closures packet of
        Just exactClosure -> pure exactClosure
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
        exterior = lgcExteriorNode closure
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
    let pendingOwnerEmission =
          isJust (lgcOwnerPendingScheme closure)
            && currentOwner /= enclosingOwner
    ( ambientRef
      , previousBound
      , deferredEnclosingDeclaration
      , deferredWithoutPendingRoute
      ) <-
      case
          [ binding
          | binding@(ref, _) <- Map.toList ambientBindings
          , typeBinderRefsSameIdentity ref semanticRef
          ]
        of
          [(ref, bound)] ->
            pure
              ( ref
              , bound
              , currentOwner /= enclosingOwner
              , currentOwner /= enclosingOwner
                  && isNothing (lgcOwnerPendingScheme closure)
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
                  , isNothing (lgcOwnerPendingScheme closure)
                  )
          [] ->
            failure
              [ "enclosing Gamma has no exact provisional consumer binding"
              , "  consumer: " ++ show semanticRef
              , "  enclosing owner: " ++ show enclosingOwner
              , "  enclosing closure: " ++ show closure
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
          completeDeclarationBound
            ( subtractAmbientLeadingBinders
                (schemeToType gammaBoundScheme)
            )
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
    case lgcOwnerPendingScheme closure of
      Just pending ->
        validatePendingRoute
          "enclosing closure"
          exterior
          ambientRef
          pending
      Nothing
        | currentOwner /= enclosingOwner -> pure ()
      Nothing ->
        failure
          [ "enclosing closure has no pending construction scheme" ]
    unless deferredWithoutPendingRoute $
      validatePendingRoute
        "packet construction"
        exterior
        ambientRef
        (subtermGeneralizationConsumerConstructionSchemeInfo packet)
    let operatedSchemeInfo =
          subtermGeneralizationOperatedSchemeInfo packet
        operatedType =
          schemeToType (siScheme operatedSchemeInfo)
        completedOperatedType =
          completeLambdaParamBoundaryOperatedType
            parameterBoundaryCertificates
            (schemeInfoBinderRefSubst operatedSchemeInfo)
            ambientRef
            completedGammaBound
            (subtractAmbientLeadingBinders operatedType)
        checkedBodyConstructsOperated =
          constructionDeclarationProvides
            checkedBodyType
            completedOperatedType
        checkedBodyCompletesConsumer =
          checkedBodyConstructsOperated
            && constructionDeclarationProvides
              checkedBodyType
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
              || openValueLambdaParameterClosureCertified ->
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
        declarationAuthority =
          if pendingOwnerEmission
            then BodyConsumerPendingOwnerEmission route
            else
              if deferredEnclosingDeclaration
                then BodyConsumerOrdinaryOwnerEmission route
                else BodyConsumerEnclosingAmbient route previousBound
        certificate =
          BodyConsumerBoundRefinementCertificate
            { bcbrDeclarationAuthority = declarationAuthority
            , bcbrOwnerFinalized = False
            , bcbrAmbientRef = ambientRef
            , bcbrPreviousBound = previousBound
            , bcbrCompletedBound = completedGammaBound
            }
    pure
      ( ambientRef
      , completedGammaBound
      , completedBoundDependencies
      , sourceSpecialization
      , certificate
      )
  where
    exactCompletedBoundDependencies
      consumerRef
      packetSchemes
      completedBound =
        go [] (freeTypeVarRefsType completedBound)
      where
        packetDeclarations =
          concatMap schemeBinderRefs packetSchemes

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
                      [ maybe TBottom tyToElab mbBound
                      | (declaredRef, mbBound) <- packetDeclarations
                      , typeBinderRefsSameIdentity
                          declaredRef
                          dependencyRef
                      ]
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
              go
                (dependencies ++ [(dependencyRef, dependencyBound)])
                (freeTypeVarRefsType dependencyBound ++ rest)

        distinctBounds rawBounds =
          foldl'
            ( \bounds bound ->
                if
                  any
                    (operationalEndpointTypesAgree bound)
                    bounds
                  then bounds
                  else bounds ++ [bound]
            )
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

    subtractAmbientLeadingBinders ty =
      case ty of
        TForallRef ref _ body
          | any
              (typeBinderRefsSameIdentity ref)
              ( Map.keys ambientBindings
                  ++ inheritedPacketRefs
                  ++ packetRoutedCheckedBodyDependencyRefs
              ) ->
              subtractAmbientLeadingBinders body
        _ -> ty

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
    BodyConsumerPendingOwnerEmission route -> route
    BodyConsumerOrdinaryOwnerEmission route -> route
    BodyConsumerConsumedAtOwner route _ -> route

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
    BodyConsumerPendingOwnerEmission _ ->
      TBottom
    BodyConsumerOrdinaryOwnerEmission _ ->
      TBottom
    BodyConsumerConsumedAtOwner _ completedBound ->
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
           , Just checkedNode <- [typeBinderRefNode checkedRef]
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
      BodyConsumerBoundRefinementCertificate
        { bcbrDeclarationAuthority = declarationAuthority,
          bcbrOwnerFinalized = False,
          bcbrAmbientRef = ambientRef,
          bcbrPreviousBound = previousBound,
          bcbrCompletedBound = vbcpProjectedType projection
        }

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
            bcbrOwnerFinalized certificate
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
        BodyConsumerConsumedAtOwner _ authorityBound
          | bcbrOwnerFinalized certificate
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
    retainConsumedDeclaration projected certificate =
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
      let targetRef = bcbrAmbientRef certificate
          completedBound = bcbrCompletedBound certificate
          matchingBinders =
            filter
              (typeBinderRefsSameIdentity targetRef . fst)
              (schemeBinderRefs scheme)
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
      let substitute =
            substTypeCaptureRef targetRef projectedBound
          projected =
            mkElabSchemeWithRefs
              [ (ref, fmap (mapBoundType substitute) mbBound)
              | (ref, mbBound) <- schemeBinderRefs scheme
              , not (typeBinderRefsSameIdentity ref targetRef)
              ]
              (substitute (schemeBody scheme))
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

    projectionFailure detail context =
      Left
        ( ValidationFailed
            ( [ "cannot project consumed body-consumer declaration into root construction"
              , "  detail: " ++ detail
              ]
                ++ context
            )
        )

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
      , bcbrOwnerFinalized certificate
      , BodyConsumerEnclosingAmbient route declaredBound <-
          [bcbrDeclarationAuthority certificate]
      , let completedBound = bcbrCompletedBound certificate
            completedScheme = schemeFromType completedBound
            completedSourceBinders =
              map fst (schemeBinderRefs completedScheme)
      , operationalEndpointTypesAgree
          (bcbrPreviousBound certificate)
          TBottom
      , operationalEndpointTypesAgree declaredBound completedBound
      , not (null completedSourceBinders)
      , all sourceOwns completedSourceBinders
      , all
          ( \sourceRef ->
              any
                (typeBinderRefsSameIdentity sourceRef)
                (freeTypeVarRefsType (tyToElab currentBound))
          )
          completedSourceBinders
      , operationalEndpointTypesAgree
          (schemeBody completedScheme)
          (tyToElab currentBound)
      , operationalEndpointTypesAgree
          (bcrOperatedType route)
          completedBound
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
-- exact instantiation must connect the two bound states before either is
-- replaced or retained.
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
      | bodyConsumerBoundRefinementAppliesToDeclarationState
          targetRef
          currentBoundTy
          certificate =
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
      bcbrOwnerFinalized certificate
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
        BodyConsumerPendingOwnerEmission route ->
          BodyConsumerPendingOwnerEmission (alphaRenameRoute route)
        BodyConsumerOrdinaryOwnerEmission route ->
          BodyConsumerOrdinaryOwnerEmission (alphaRenameRoute route)
        BodyConsumerConsumedAtOwner route completedBound ->
          BodyConsumerConsumedAtOwner
            (alphaRenameRoute route)
            (alphaRenameType completedBound)

    alphaRenameRoute route =
      route
        { bcrOperatedType = alphaRenameType (bcrOperatedType route)
        , bcrConstructionOperatedType =
            alphaRenameType (bcrConstructionOperatedType route)
        }

-- | Rename every identity-bearing payload in one proof atomically with the
-- elaborated term that carries it.  This is a global construction quotient;
-- lexical alpha-copies use
-- 'alphaRenameBodyConsumerBoundRefinementCertificate' instead.
renameBodyConsumerBoundRefinementCertificate
  :: [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerBoundRefinementCertificate
  -> BodyConsumerBoundRefinementCertificate
renameBodyConsumerBoundRefinementCertificate renames certificate =
  certificate
    { bcbrDeclarationAuthority =
        renameDeclarationAuthority
          (bcbrDeclarationAuthority certificate),
      bcbrAmbientRef = renameRef (bcbrAmbientRef certificate),
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
        BodyConsumerPendingOwnerEmission route ->
          BodyConsumerPendingOwnerEmission
            (renameRoute route)
        BodyConsumerOrdinaryOwnerEmission route ->
          BodyConsumerOrdinaryOwnerEmission
            (renameRoute route)
        BodyConsumerConsumedAtOwner route completedBound ->
          BodyConsumerConsumedAtOwner
            (renameRoute route)
            (renameTypeBinderRefPayloads renames completedBound)

    renameRoute route =
      route
        { bcrSemanticRef = renameRef (bcrSemanticRef route),
          bcrConstructionRef = renameRef (bcrConstructionRef route),
          bcrOperatedType =
            renameTypeBinderRefPayloads
              renames
              (bcrOperatedType route),
          bcrConstructionOperatedType =
            renameTypeBinderRefPayloads
              renames
              (bcrConstructionOperatedType route)
        }

    renameRef ref =
      fromMaybe
        ref
        ( snd
            <$> find
              (typeBinderRefsSameIdentity ref . fst)
              renames
        )

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
          completeLambdaParamBoundaryBound
            [certificate]
            candidate

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
  declarationRef =
    completeLambdaParamBoundaryBoundInScope
      lexicalRefs
    [ certificate
    | certificate <- certificates
    , not
        ( any
            (typeBinderRefsSameIdentity declarationRef)
            (freeTypeVarRefsType (lpbcConstructedType certificate))
        )
    ]

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
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryOperatedType certificates operatedRoutes declarationRef expectedType =
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
            )
          pure completed

        -- Section 15.3.8's annotated self-application reaches a lambda
        -- boundary with the exact parameter still open in the packet:
        --
        --   forall a. forall (b >= a -> a).
        --     (forall a. a -> a) -> a
        --
        -- The checked application, however, owns the flexible result @b@ and
        -- constructs the paper endpoint
        --
        --   forall (b >= forall a. a -> a).
        --     (forall a. a -> a) -> b
        --
        -- Build that endpoint while all construction authority is present.
        -- This is deliberately narrower than a type-shape rewrite: the
        -- packet must carry the exact source binder from its parameter
        -- boundary certificate, and the retained result declaration must be
        -- an identity-routed graph result with precisely the opened parameter
        -- body as its lower bound.  The expected owner endpoint validates the
        -- same result identity and completed construction, but does not supply
        -- any binder or payload used to construct it.
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
          guard
            ( operationalEndpointTypesAgree openedDomain constructedType
                && typeBinderRefsSameIdentity openedCodomainRef openedRef
            )
          completedBound <-
            either (const Nothing) Just (elabToBound constructedType)
          let expectedScheme = schemeFromType expectedType
          [(expectedResultRef, Just expectedResultBound)] <-
            pure (schemeBinderRefs expectedScheme)
          guard
            ( typeBinderRefsSameIdentity expectedResultRef resultRef
                && operationalEndpointTypesAgree
                  (tyToElab expectedResultBound)
                  constructedType
            )
          (expectedDomain, expectedCodomain) <-
            case schemeBody expectedScheme of
              TArrow domain codomain -> Just (domain, codomain)
              _ -> Nothing
          expectedCodomainRef <-
            case expectedCodomain of
              TVarRef ref -> Just ref
              _ -> Nothing
          guard
            ( operationalEndpointTypesAgree expectedDomain constructedType
                && typeBinderRefsSameIdentity expectedCodomainRef resultRef
            )
          let completed =
                schemeToType
                  ( mkElabSchemeWithRefs
                      [(resultRef, Just completedBound)]
                      (TArrow constructedType (TVarRef resultRef))
                  )
          guard (operationalEndpointTypesAgree completed expectedType)
          pure completed

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
  foldl'
    completeCertificate
    ty0
    certificates
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
          concatMap freeTypeVarRefsType siblings ++ lexicalRefs

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
completeLambdaParamBoundarySchemeInfo certificates schemeInfo =
  rebuildSchemeInfoFromRefSubst
    schemeInfo
    completedScheme
    completedRoutes
  where
    completedScheme0 =
      schemeFromType
        ( completeLambdaParamBoundaryType
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
-- consumer identity.  The pending scheme must contain exactly one unbounded
-- declaration for its direct exterior route; a missing route, a peer route,
-- or an already materialized bound cannot authorize replacement.
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
                      (applyRefRenames ref)
                      (applyRefRenames exteriorRef)
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
