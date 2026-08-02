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
    completeSchemeInfoRouteType,
    completeLambdaParamBoundarySchemeInfo,
    completeLambdaParamBoundaryBound,
    completeLambdaParamBoundaryDeclarationBound,
    completeLambdaParamBoundaryType,
    renameLambdaParamBoundaryCertificate,
    OrdinaryGammaPacketConstruction (..),
    ValidatedBodyConsumerProjection,
    BodyConsumerDeclarationAuthority,
    BodyConsumerBoundRefinementCertificate,
    bodyConsumerBoundRefinementCertifiesTransition,
    bodyConsumerBoundRefinementTopologyResultRefAtConstruction,
    bodyConsumerBoundRefinementCompletedTopologyEndpoint,
    bodyConsumerBoundRefinementsCompletePacketBound,
    bodyConsumerBoundRefinementEmittedBy,
    bodyConsumerBoundRefinementRequiresOwnerEmission,
    bodyConsumerBoundRefinementExcludesAmbientRef,
    finalizeBodyConsumerBoundRefinementAtOwner,
    bodyConsumerBoundRefinementSurvivesOwnerBoundary,
    bodyConsumerBoundRefinementConsumesAny,
    bodyConsumerBoundRefinementConsumedDependencies,
    bodyConsumerBoundRefinementTargetsAny,
    completeLambdaEndpointFromBodyConsumerRefinement,
    materializeLocalTopologyResultBound,
    certifyLocalPacketBodyConsumerBoundRefinement,
    certifyAmbientPacketGammaConsumerBoundRefinement,
    certifyEnclosingPacketBodyConsumerBoundRefinement,
    installBodyConsumerBoundRefinements,
    installOwnedBodyConsumerBoundRefinements,
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
    ExactLambdaConstructionPlan,
    exactLambdaConstructionBinders,
    exactLambdaConstructionPublishedBinders,
    exactLambdaConstructionPublishedType,
    exactLambdaConstructionBinderRenames,
    exactLambdaConstructionBodyBinderRenames,
    exactLambdaConstructionBodyInstantiation,
    exactLambdaConstructionBodyType,
    exactLambdaConstructionAmbientBodyRefinement,
    exactLambdaConstructionAmbientBodyRefinementCertificate,
    exactLambdaConstructionCompletionInstantiation,
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
    projectCertifiedBodyConsumerRootScheme,
    projectCertifiedBodyConsumerRootBounds,
    projectCertifiedBodyConsumerBoundsIfPresent,
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
import Data.List (find)
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
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationConsumerConstructionSchemeInfo,
    subtermGeneralizationExactConsumerSpecialization,
    subtermGeneralizationGammaAuthority,
    subtermGeneralizationGammaBoundScheme,
    subtermGeneralizationAdministrativeLambdaResultConstruction,
    subtermGeneralizationInheritedGammaRoutes,
    subtermGeneralizationLocalResultAuthority,
    subtermGeneralizationLocalConsumerClosure,
    subtermGeneralizationOperatedSchemeInfo,
    subtermGeneralizationOpaqueResultConstruction,
    subtermGeneralizationOpaqueResultConstructionPlan,
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
    exactBinderSpineInstantiation,
    exactBinderSpineRenames,
    inferInstAppArgsFromSchemeRefsExact,
    planExactBinderSpine,
  )
import MLF.Elab.SourceBinder
  ( orderSourceProjectedSchemeBinders,
  )
import MLF.Elab.TermClosure
  ( renameBoundTypeBinderRefPayloads,
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
  , exactLambdaConstructionBodyInstantiation :: !Instantiation
  , exactLambdaConstructionBodyType :: !ElabType
  , exactLambdaConstructionAmbientBodyRefinement ::
      !(Maybe (TypeBinderRef, ElabType, ElabType))
  , exactLambdaConstructionAmbientBodyRefinementCertificate ::
      !(Maybe BodyConsumerBoundRefinementCertificate)
  , exactLambdaConstructionCompletionInstantiation :: !Instantiation
  }
  deriving (Eq, Show)

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
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaEndpointConstruction
  ambientBindings
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
            ([], identityGeneratorAfterType (TArrow bodySourceTy expectedTy))
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
    pure
      ExactLambdaConstructionPlan
        { exactLambdaConstructionBinders =
            if constructsPublishedEndpointDirectly
              then publishedBinders
              else constructionBinders
        , exactLambdaConstructionPublishedBinders = publishedBinders
        , exactLambdaConstructionPublishedType = alignedPublishedTy
        , exactLambdaConstructionBinderRenames = binderRenames
        , exactLambdaConstructionBodyBinderRenames = bodyBinderRenames
        , exactLambdaConstructionBodyInstantiation = bodyInstantiation
        , exactLambdaConstructionBodyType = alignedBodyTy
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
  :: LocalGammaOwner
  -> PreparedSubtermGeneralization
  -> Map.Map TypeBinderRef ElabType
  -> [BodyConsumerBoundRefinementCertificate]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> Instantiation
  -> ElabType
  -> ElabType
  -> Either ElabError ExactLambdaConstructionPlan
certifyExactLambdaConstruction
  owner
  packet
  ambientBindings0
  bodyRefinements
  rawCandidates
  paramTy
  bodySourceTy
  checkedBodyInstantiation
  bodyResultTy
  expectedTy = do
  ensureDistinct "construction" (map fst candidates)
  ensureDistinct "enclosing" (map fst expectedBinders)
  case
      maybeToList certifyPacketGammaBoundAmbientRefinement
        ++ mapMaybe
        ( certifyBodyOption
            bodySourceTy
            checkedBodyInstantiation
            candidates
            expectedTy
        )
        bodyOptions
        ++ concatMap
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
        ++ mapMaybe certifyCompletedBodyOption bodyOptions
        ++ maybeToList certifyGeneralizedExpectedBodyDeclaration
        ++ maybeToList certifyCompletedPacketBodySpine
        ++ maybeToList certifyPendingPacketBodyBinder
        ++ maybeToList certifyExactBodySourceEndpoint
    of
    plan : _ -> pure plan
    [] ->
      constructionFailure
        "no exact binder-spine and body computation reaches the enclosing Gamma"
        [ "  candidates: " ++ show candidates
        , "  parameter: " ++ show paramTy
        , "  checked body source: " ++ show bodySourceTy
        , "  checked body result: " ++ show bodyResultTy
        , "  body options: " ++ show bodyOptions
        , "  checked body source plans: "
            ++ show
              [ ( expectedBodyTy
                , constructExactInstantiation
                    constructionTypeEnv
                    exactLambdaEndpointTypesAgree
                    bodySourceTy
                    expectedBodyTy
                )
              | TArrow _ expectedBodyTy <-
                  [schemeBody expectedScheme]
              ]
        , "  packet body binder: " ++ show packetBodyBinderRef
        , "  completed packet body plans: "
            ++ show
              [ ( candidateRef
                , constructExactInstantiation
                    constructionTypeEnv
                    exactLambdaEndpointTypesAgree
                    bodyResultTy
                    (tyToElab candidateBound)
                )
              | (candidateRef, Just candidateBound) <- candidates
              , maybe
                  False
                  (typeBinderRefsSameIdentity candidateRef)
                  packetBodyBinderRef
              ]
        , "  pending packet body spine: "
            ++ show
              [ ( candidateRef
                , planExactBinderSpine
                    exactLambdaEndpointTypesAgree
                    ( constructionType
                        candidates
                        paramTy
                        (TVarRef candidateRef)
                    )
                    expectedTy
                )
              | (candidateRef, Nothing) <- candidates
              , maybe
                  False
                  (typeBinderRefsSameIdentity candidateRef)
                  packetBodyBinderRef
              ]
        , "  completed packet body spine: "
            ++ show completedPacketBodySpineDiagnostic
        , "  ambient expected matches: " ++ show ambientExpectedMatches
        , "  ambient bindings: " ++ show (Map.toList ambientBindings)
        , "  packet source binder refs: "
            ++ show
              ( IntMap.elems
                  ( siSourceBinderOrderRefs
                      packetConstructionSchemeInfo
                  )
              )
        , "  packet construction binders: "
            ++ show packetConstructionBinders
        , "  packet construction-order refs: "
            ++ show
              ( IntMap.elems
                  ( siConstructionBinderOrderRefs
                      packetConstructionSchemeInfo
                  )
              )
        , "  opaque result construction: "
            ++ show
              (subtermGeneralizationOpaqueResultConstruction packet)
        , "  opaque result construction plan: "
            ++ show
              (subtermGeneralizationOpaqueResultConstructionPlan packet)
        , "  completed packet type: "
            ++ show
              (schemeToType (siScheme (subtermGeneralizationSchemeInfo packet)))
        , "  operated packet type: "
            ++ show
              (schemeToType (siScheme (subtermGeneralizationOperatedSchemeInfo packet)))
        , "  packet Gamma bound: "
            ++ show
              (schemeToType (subtermGeneralizationGammaBoundScheme packet))
        ]
  where
    expectedScheme = schemeFromType expectedTy
    expectedBinders = schemeBinderRefs expectedScheme
    expectedFreeRefs = freeTypeVarRefsType expectedTy
    -- A pending graph declaration that occurs free in the inherited exact
    -- endpoint belongs to the enclosing construction Gamma.  Quantifying it
    -- again in this lambda's local Lambda(Gamma) spine would capture the very
    -- endpoint occurrence that selects it.  The endpoint identity and the
    -- candidate's unbounded declaration together are the construction
    -- authority for opening it ambiently here.
    endpointAmbientCandidates =
      [ (expectedRef, candidateRef)
      | (candidateRef, Nothing) <- rawCandidates
      , expectedRef <- expectedFreeRefs
      , typeBinderRefsSameIdentity candidateRef expectedRef
      ]
    ambientBindings =
      foldr
        ( \(expectedRef, _) ->
            Map.insert expectedRef TBottom
        )
        ambientBindings0
        endpointAmbientCandidates
    localRawCandidates =
      [ candidate
      | candidate@(candidateRef, _) <- rawCandidates
      , not
          ( any
              (typeBinderRefsSameIdentity candidateRef . snd)
              endpointAmbientCandidates
          )
      ]
    completedConstructionCandidates =
      map
        completePublishedBodyBinder
        (packetExpectedConstructionCandidates ++ localRawCandidates)
    -- The exact packet owns the complete Lambda(Gamma_g) spine.  A binder in
    -- its construction-order certificate can be absent from the local
    -- candidates when an exact parameter boundary has already consumed the
    -- corresponding graph occurrence.  Reintroduce only the declaration
    -- positively owned by both the packet and the expected spine; ambient and
    -- source-owned declarations remain outside this lambda's local emission.
    packetExpectedConstructionCandidates =
      [ packetBinder
      | packetBinder@(packetRef, packetBound) <- packetConstructionBinders
      , constructionOwnsPacketBinder packetRef
      , not (sourceOwnsPacketBinder packetRef)
      , Just (_, expectedBound) <-
          [ find
              (typeBinderRefsSameIdentity packetRef . fst)
              expectedBinders
          ]
      , binderBoundsAgree packetBound expectedBound
      , not
          ( any
              (typeBinderRefsSameIdentity packetRef . fst)
              rawCandidates
          )
      , not
          ( any
              (typeBinderRefsSameIdentity packetRef . fst)
              (Map.toList ambientBindings)
          )
      ]
    binderBoundsAgree Nothing Nothing = True
    binderBoundsAgree (Just left) (Just right) =
      operationalEndpointTypesAgree (tyToElab left) (tyToElab right)
    binderBoundsAgree _ _ = False
    candidates =
      case ambientPublishedBodyDeclarationRef of
        Nothing -> completedConstructionCandidates
        Just ambientRef ->
          filter
            (not . typeBinderRefsSameIdentity ambientRef . fst)
            completedConstructionCandidates
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
          certifiedDescendantCompletion candidateRef =
          (candidateRef, Just completedBound)
      | otherwise = binder

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
              , identityGeneratorAfterType
                  (TArrow bodySourceTy expectedTy)
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

    -- The checked body computation is provisional evidence for the result
    -- selected before the enclosing exact Gamma is known.  It must not become
    -- an irreversible prefix of the final xMLF construction.  When the exact
    -- codomain requires a different legal route (notably the paper's nested
    -- annotated self-application), construct the complete computation from
    -- the checked source type and certify that endpoint directly.
    certifyExactBodySourceEndpoint = do
      TArrow _ expectedBodyTy <- pure (schemeBody expectedScheme)
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
          (sourceBinderRenames, _) =
            foldl
              freshenSourceBinder
              ( []
              , identityGeneratorAfterType
                  (TArrow bodySourceTy expectedTy)
              )
              collidingSourceBinderRefs
          freshBodySourceTy =
            renameTypeBinderRefPayloads
              sourceBinderRenames
              bodySourceTy
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

    ambientExpectedMatches =
      fmap
        ( \expectedRef ->
            [ (ambientRef, ambientBound)
            | (ambientRef, ambientBound) <- Map.toList ambientBindings
            , typeBinderRefsSameIdentity ambientRef expectedRef
            ]
        )
        expectedBodyBinderRef

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

    completedPacketBodySpineDiagnostic = do
      packetBodyRef <- maybeToList packetBodyBinderRef
      bodyCandidate <-
        [ binder
        | binder@(ref, Just _) <- candidates
        , typeBinderRefsSameIdentity ref packetBodyRef
        ]
      let (checkedBinders, checkedResidual) =
            splitForallsRefs bodyResultTy
      pure
        ( bodyCandidate
        , checkedBinders
        , checkedResidual
        , constructionOwnsPacketBinder (fst bodyCandidate)
        , sourceOwnsPacketBinder (fst bodyCandidate)
        )

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

    certifyBodyOption
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
      certifiedBodyTy <-
        either
          (const Nothing)
          Just
          ( TypeCheck.checkInstantiation
              bodyTypeEnv
              checkedBodySourceTy
              bodyInstantiation0
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
          peerPublishedTy = alignPeerType publishedTy
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
          alignedParamTy = alignType paramTy
          alignedBodyTy = alignType constructedBodyTy0
          alignedConstructionTy =
            schemeToType
              ( mkElabSchemeWithRefs
                  constructionBinders
                  (TArrow alignedParamTy alignedBodyTy)
              )
          alignedPublishedTy = alignType publishedTy
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
          constructsPublishedEndpointDirectly =
            exactLambdaEndpointTypesAgree
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
          , exactLambdaConstructionBodyInstantiation = bodyInstantiation
          , exactLambdaConstructionBodyType = alignedBodyTy
          , exactLambdaConstructionAmbientBodyRefinement =
              ambientBodyRefinement
          , exactLambdaConstructionAmbientBodyRefinementCertificate =
              ambientBodyRefinementCertificate
          , exactLambdaConstructionCompletionInstantiation =
              emittedCompletionInstantiation
          }

    exactAmbientBodyRefinementCertificate
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
    ( bcrOwner route == owner
        && bcrEdgeId route == lgoBoundaryEdge owner
        && typeBinderRefsSameIdentity
          (bcrConstructionRef route)
          (bcbrAmbientRef certificate)
        && operationalEndpointTypesAgree
          (bcrConstructionOperatedType route)
          completedBound
    )
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
  authority <- subtermGeneralizationConsumerAuthority packet
  guard (subtermConsumerAuthorityIsTopology authority)
  guard (subtermConsumerAuthorityEnclosingOwner authority == Just owner)
  guard (scaEdgeId authority == bodyEdge)
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
        [ bcrSemanticRef route
        , bcrConstructionRef route
        ]
    )
  guard
    ( typeBinderRefIdentity resultRef
        == scaConsumerIdentity authority
    )
  guard
    ( operationalEndpointTypesAgree
        ( schemeToType
            (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
        )
        (bcrOperatedType route)
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
        ( schemeToType
            (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
        )
        (bcrOperatedType route)
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
  installBodyConsumerBoundRefinementsWithOwner Nothing

-- | The only declaration states accepted while replaying a checked descendant
-- completion.  Keeping the selected output in the plan makes installation
-- mechanical: no caller can validate one relation and then write a different
-- bound.
data BodyConsumerBoundInstallation
  = InstallCompletedBodyConsumerBound !ElabType
  | PreserveOwnerSpecializedBodyConsumerBound !ElabType
  | PreserveEnclosingSpecializedBodyConsumerBound !ElabType
  | PreserveEnclosingGeneralizedBodyConsumerBound !ElabType

-- | Owner-aware installation for the exact constructor that will emit a
-- pending declaration.  If its checked boundary endpoint is an exact
-- specialization of the descendant's completed declaration, that endpoint
-- is retained as the owner's local bound.  Intermediate constructors still
-- install the full descendant completion.
installOwnedBodyConsumerBoundRefinements
  :: LocalGammaOwner
  -> [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installOwnedBodyConsumerBoundRefinements owner =
  installBodyConsumerBoundRefinementsWithOwner (Just owner)

installBodyConsumerBoundRefinementsWithOwner
  :: Maybe LocalGammaOwner
  -> [BodyConsumerBoundRefinementCertificate]
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
installBodyConsumerBoundRefinementsWithOwner mbOwner certificates initialBindings =
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
        PreserveOwnerSpecializedBodyConsumerBound bound -> bound
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
      | pendingOwnerClosurePresentsCompletedBody
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

    ownerEmitsSpecializedEndpoint certificate currentBound =
      case mbOwner of
        Just owner ->
          bodyConsumerBoundRefinementEmittedBy owner certificate
            && bodyConsumerCompletedDeclarationSpecializesToEndpoint
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
          [] -> pure (bcbrCompletedBound certificate)
          [Nothing] -> pure (bcbrCompletedBound certificate)
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
              (ownerRequirements, ownerEndpoint) <-
                emitDeferredRequirement
                  route
                  certificate
                  requirements
              ownerScheme' <-
                completePendingOwnerScheme
                  route
                  certificate
                  ownerEndpoint
                  ownerScheme
              pure (Just ownerScheme', ownerRequirements)
        BodyConsumerOrdinaryOwnerEmission route
          | bcrOwner route == owner -> do
              (ownerRequirements, ownerEndpoint) <-
                emitDeferredRequirement
                  route
                  certificate
                  requirements
              ownerScheme' <-
                completeOrdinaryOwnerScheme
                  route
                  certificate
                  ownerEndpoint
                  ownerScheme
              pure (ownerScheme', ownerRequirements)
        _ -> pure (ownerScheme, requirements)

    emitDeferredRequirement route certificate requirements = do
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
      let ownerEndpoint = rgbOperatedType requirement
          ownerRequirement =
            requirement
              { rgbExactOperatedOccurrenceRef =
                  completedExactOccurrence
                    route
                    ownerEndpoint
              }
      pure
        ( requirements
            { grRequiredGammaBinders =
                map
                  (replaceRequirement requirement ownerRequirement)
                  (grRequiredGammaBinders requirements)
            , grLocallyClosedGammaNodes =
                IntSet.delete
                  exteriorKey
                  (grLocallyClosedGammaNodes requirements)
            }
        , ownerEndpoint
        )

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
      ownerMbBound <-
        if operationalEndpointTypesAgree
            ownerEndpoint
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
                (elabToBound ownerEndpoint)
      let binders = schemeBinderRefs (siScheme ownerScheme)
          matchingBinders =
            filter
              ( typeBinderRefsSameIdentity
                  (bcbrAmbientRef certificate)
                  . fst
              )
              binders
      case matchingBinders of
        [(_, currentMbBound)]
          | ownerSchemeBoundCanAdvance currentMbBound ->
              pure
                ownerScheme
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
          | otherwise ->
              inheritanceFailure
                route
                certificate
                [ "pending owner scheme carries a conflicting bound"
                , "  current bound: "
                    ++ show (maybe TBottom tyToElab currentMbBound)
                ]
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
      where
        ownerSchemeBoundCanAdvance currentMbBound =
          let currentBound = maybe TBottom tyToElab currentMbBound
           in operationalEndpointTypesAgree
                currentBound
                ownerEndpoint
                || bodyConsumerBoundRefinementAcceptsDeclarationState
                certificate
                currentBound
                || completeUnboundedForallSpecializesTo
                  currentBound
                  (bcbrPreviousBound certificate)

    completeOrdinaryOwnerScheme route certificate ownerEndpoint mbOwnerScheme =
      case mbOwnerScheme of
        Nothing -> pure Nothing
        Just ownerScheme
          | any
              ( typeBinderRefsSameIdentity
                  (bcbrAmbientRef certificate)
                  . fst
              )
              (schemeBinderRefs (siScheme ownerScheme)) ->
              Just
                <$> completePendingOwnerScheme
                  route
                  certificate
                  ownerEndpoint
                  (Just ownerScheme)
          | otherwise -> pure (Just ownerScheme)

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
    let operatedType =
          schemeToType
            (siScheme (subtermGeneralizationOperatedSchemeInfo packet))
        completedOperatedType =
          completeLambdaParamBoundaryOperatedType
            parameterBoundaryCertificates
            ambientRef
            (subtractAmbientLeadingBinders operatedType)
        checkedBodyConstructsOperated =
          constructionDeclarationProvides
            checkedBodyType
            completedOperatedType
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
              || ( isJust checkedBodyToPublished
                    && checkedBodyConstructsOperated
                 ) ->
              -- The packet may retain a completed forall spine.  Construct
              -- its exact Figure 15.3.5 instantiation plan (including N for
              -- bounded binders) and accept only a plan whose replay reaches
              -- the checked body endpoint.  A direct N step may choose an
              -- argument constructed from the binder's bound; that checked
              -- computation need not factor through the bound and then
              -- specialize beneath an arrow.  In that case the checked body
              -- must independently construct both the operated endpoint and
              -- the published Gamma bound.  No final-type shape is used to
              -- invent a declaration or an equality.
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

        inheritedPacketRefs =
          Reify.inheritedGammaRoutesLexicalRefs inheritedRoutes
            ++ map
              Reify.inheritedGammaRouteRef
              (Reify.inheritedGammaRoutesEntries inheritedRoutes)
          where
            inheritedRoutes =
              subtermGeneralizationInheritedGammaRoutes packet

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
              (Map.keys ambientBindings) ->
              subtractAmbientLeadingBinders body
        _ -> ty

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
authorizeBodyConsumerDeclaration =
  authorizeBodyConsumerDeclarationWithValidatedLocalRequirements []

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
  -> BodyConsumerRoute
  -> Either ElabError (Maybe BodyConsumerDeclarationAuthority)
authorizeBodyConsumerDeclarationWithValidatedLocalRequirements validatedLocalRequirementRefs constructionAliases constructionBinderRenames paramBoundaryCertificates localBinders ambientBindings route =
  case exactLocalBinders of
    [(localRef, mbLocalBound)]
      | declarationMatchesOperated localRef localBound
          || routeOperatesOnOwnDeclaration
          || localRequirementAlreadyValidated localRef ->
          pure
            ( Just
                (BodyConsumerLocallyEmitted route localBound)
            )
      | otherwise ->
          authorityFailure
            [ "local declaration disagrees with the route requirement"
            , "  local ref: " ++ show localRef
            , "  local bound: "
                ++ show localBound
            , "  projected local bound: "
                ++ show (projectDeclarationBound localRef localBound)
            , "  route operated type: "
                ++ show (bcrConstructionOperatedType route)
            ]
      where
        localBound = maybe TBottom tyToElab mbLocalBound
    [] ->
      case exactAmbientBindings of
        [] -> pure Nothing
        [(_, ambientBound)] ->
          pure
            ( Just
                (BodyConsumerInheritedAmbient route ambientBound)
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

    declarationMatchesOperated declarationRef declarationBound =
      operationalEndpointTypesAgree
        (projectDeclarationBound declarationRef declarationBound)
        (bcrConstructionOperatedType route)
        || completeUnboundedForallSpecializesTo
          (projectDeclarationBound declarationRef declarationBound)
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
projectCertifiedBodyConsumerRootScheme retainedRootConsumers closures ambientRefs localRefs certificates scheme0 = do
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
              incompatibleConsumedCertificates
                "consumed certificates have multiple incomparable generalized declarations"
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
                -- The owner consumed this declaration at a certified exact
                -- instance of its completed bound.  The surviving root scheme
                -- therefore denotes that instance, not the more-general
                -- declaration that supplied it.
                pure (maybe TBottom tyToElab mbBound)
            | otherwise ->
                projectionFailure
                  "consumed root binder does not carry a certified declaration state"
                  [ "  target: " ++ show targetRef
                  , "  current bound: "
                      ++ show (maybe TBottom tyToElab mbBound)
                  , "  completed bound: " ++ show completedBound
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

-- | Rename every identity-bearing payload in one proof atomically with the
-- elaborated term that carries it.
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
  | ExactSourceLambdaParamBoundary !ElabType !(Maybe ElabType)
  | ExactApplicationArgumentLambdaParamBoundary !ElabType
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
      lpbcConstructedType :: !ElabType
    }
  deriving (Eq, Show)

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
  completeLambdaParamBoundaryBound
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
  -> TypeBinderRef
  -> ElabType
  -> ElabType
completeLambdaParamBoundaryOperatedType certificates declarationRef =
  \candidate -> foldl' completeCertificate candidate certificates
  where
    completeCertificate candidate certificate
      | any
          (typeBinderRefsSameIdentity declarationRef)
          (freeTypeVarRefsType constructedType) =
          candidate
      | otherwise =
          completeLambdaParamBoundaryDeclarationBound
            [certificate]
            declarationRef
            (fromMaybe candidate movedParameterDeclarations)
      where
        constructedType = lpbcConstructedType certificate
        constructedBinders =
          schemeBinderRefs (schemeFromType constructedType)
        candidateScheme = schemeFromType candidate
        candidateBinders = schemeBinderRefs candidateScheme
        movedParameterDeclarations = do
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
                , not
                    (any (typeBinderRefsSameIdentity ref) movedRefs)
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
  foldl'
    completeCertificate
    ty0
    certificates
  where
    completeCertificate ty certificate =
      closeOpenedEndpoint
        mayCloseRoot
        []
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
                    (closeOpenedEndpoint mayClose lexicalRefs domain)
                    (closeOpenedEndpoint mayClose lexicalRefs codomain)
                TConWithIdentity identity constructor arguments ->
                  TConWithIdentity
                    identity
                    constructor
                    ( fmap
                        (closeOpenedEndpoint mayClose lexicalRefs)
                        arguments
                    )
                TVarAppRef ref arguments ->
                  TVarAppRef
                    ref
                    ( fmap
                        (closeOpenedEndpoint mayClose lexicalRefs)
                        arguments
                    )
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
                  TForallRef
                    ref
                    (fmap (closeOpenedBound lexicalRefs) mbBound)
                    ( closeOpenedEndpoint
                        False
                        (ref : lexicalRefs)
                        body
                    )
                TMuRef ref body ->
                  TMuRef
                    ref
                    ( closeOpenedEndpoint
                        mayClose
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
                    (closeOpenedEndpoint True lexicalRefs domain)
                    (closeOpenedEndpoint True lexicalRefs codomain)
                TConWithIdentity identity constructor arguments ->
                  TConWithIdentity
                    identity
                    constructor
                    ( fmap
                        (closeOpenedEndpoint True lexicalRefs)
                        arguments
                    )
                TVarAppRef ref arguments ->
                  TVarAppRef
                    ref
                    ( fmap
                        (closeOpenedEndpoint True lexicalRefs)
                        arguments
                    )
                TBaseWithIdentity identity base ->
                  TBaseWithIdentity identity base
                TBottom -> TBottom
                TForallRef ref mbBound body ->
                  TForallRef
                    ref
                    (fmap (closeOpenedBound lexicalRefs) mbBound)
                    ( closeOpenedEndpoint
                        False
                        (ref : lexicalRefs)
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
    }

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
    ExactSourceLambdaParamBoundary exactTy mbConstructionTy -> do
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
      pure (exactInstallation exactTy)
    ExactApplicationArgumentLambdaParamBoundary exactTy ->
      -- Figure 15.3.5 has already checked the argument at this exact
      -- endpoint.  The direct identity/eta parameter therefore owns the
      -- complete value type just like a source-exact parameter: its graph
      -- root is not an occurrence of the provisional application Gamma
      -- variable.  Remove that root route instead of comparing the checked
      -- value type with, or overwriting it by, the provisional lower bound.
      pure (exactInstallation exactTy)
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
                  boundaryCertificate constructionTy
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
                          boundaryCertificate constructionTy
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
                      boundaryCertificate constructionTy
                  }
  where
    exactInstallation exactTy =
      let certificate = boundaryCertificate exactTy
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

    boundaryCertificate constructedTy =
      LambdaParamBoundaryCertificate
        { lpbcParameterNode = paramNode,
          lpbcConstructedType = constructedTy
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
        ExactApplicationArgumentLambdaParamBoundary {} -> []

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
