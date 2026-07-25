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
    OrdinaryGammaPacketConstruction (..),
    ValidatedBodyConsumerProjection,
    BodyConsumerDeclarationAuthority,
    BodyConsumerBoundRefinementCertificate,
    bodyConsumerBoundRefinementTargetsAny,
    authorizeBodyConsumerDeclaration,
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
    operationalEndpointTypesAgree,
    mkValidatedBodyConsumerProjection,
    validatedBodyConsumerProjectionSourceConstructionRenames,
    validatedBodyConsumerLeadingElimination,
    validatedBodyConsumerProjectionSpecialization,
    projectValidatedAmbientConsumerBound,
    projectValidatedAmbientConsumerBoundWithCertificate,
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
import MLF.Elab.Generalize
  ( LocalGammaClosure (..),
    LocalGammaConstructor (..),
    LocalGammaOwner,
    lgoBoundaryEdge,
    lgoConstructor,
    lgoTermNode,
    lgoScope,
  )
import MLF.Elab.Elaborate.Algebra.BodyConsumerRoute
  ( BodyConsumerRoute (..),
  )
import MLF.Elab.Inst (composeInst, schemeToType)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromSchemeRefsExact)
import MLF.Elab.TermClosure (renameTypeBinderRefPayloads)
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
    mapBoundType,
    mkElabSchemeWithRefs,
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
  )
import MLF.Types.Identity
  ( typeBinderIdentityGeneratedUnique,
    typeBinderIdentityStableName,
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

-- | The source-oriented endpoint installed for one already validated
-- lambda-body consumer route.  Its constructor is private so later
-- specialization cannot confuse a raw packet endpoint with the endpoint that
-- 'Gen(Gamma, tau)' actually published.
data ValidatedBodyConsumerProjection =
  ValidatedBodyConsumerProjection
    { vbcpRoute :: !BodyConsumerRoute,
      vbcpSourceConstructionRenames ::
        ![(TypeBinderRef, TypeBinderRef)],
      vbcpProjectedType :: !ElabType
    }
  deriving (Eq, Show)

-- | Proof that the construction endpoint of an exact lambda-body route is a
-- declaration available at this lambda boundary.  A declaration is either
-- emitted by this exact @Gen(Gamma,tau)@ construction or inherited from the
-- enclosing paper Gamma.  The constructor is private so a bare route cannot
-- be mistaken for declaration ownership.
data BodyConsumerDeclarationAuthority
  = BodyConsumerLocallyEmitted
      !BodyConsumerRoute
      !ElabType
  | BodyConsumerInheritedAmbient
      !BodyConsumerRoute
      !ElabType
  deriving (Eq, Show)

-- | A construction proof that one provisional ambient declaration was
-- completed by the exact lambda-body consumer that owns it.  The constructor
-- is private: a caller can obtain this value only by presenting the private
-- declaration authority and the validated body projection to the ambient
-- projector below.  Root planning may therefore replay the completed bound
-- without inferring it from the finished term or from type shape.
data BodyConsumerBoundRefinementCertificate =
  BodyConsumerBoundRefinementCertificate
    { bcbrDeclarationAuthority :: !BodyConsumerDeclarationAuthority,
      bcbrAmbientRef :: !TypeBinderRef,
      bcbrPreviousBound :: !ElabType,
      bcbrCompletedBound :: !ElabType
    }
  deriving (Eq, Show)

-- | A refinement is part of an owner's final construction certificate only
-- when its exact declaration identity remains ambiently free in that
-- construction.  A body route may complete a provisional waypoint that is
-- subsequently consumed by the same lambda; publishing that dead waypoint
-- to root planning would misclassify it as both local and ambient.
bodyConsumerBoundRefinementTargetsAny
  :: [TypeBinderRef]
  -> BodyConsumerBoundRefinementCertificate
  -> Bool
bodyConsumerBoundRefinementTargetsAny refs certificate =
  any
    (typeBinderRefsSameIdentity (bcbrAmbientRef certificate))
    refs

-- | Freeze declaration ownership for an already selected body-consumer
-- route.  The declaration occurs exactly once and its actual Gamma bound is
-- retained separately from the operated endpoint.  Usually those types agree;
-- for an unbounded named node they deliberately differ as @a > bottom@ versus
-- @S'(a) = a@.  Local ownership takes precedence because the lambda
-- constructor deliberately shadows a provisional enclosing slot.
authorizeBodyConsumerDeclaration
  :: IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> Map.Map TypeBinderRef ElabType
  -> BodyConsumerRoute
  -> Either ElabError (Maybe BodyConsumerDeclarationAuthority)
authorizeBodyConsumerDeclaration constructionAliases constructionBinderRenames localBinders ambientBindings route =
  case exactLocalBinders of
    [(localRef, mbLocalBound)]
      | declarationMatchesOperated localBound
          || routeOperatesOnOwnDeclaration ->
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

    declarationMatchesOperated declarationBound =
      operationalEndpointTypesAgree
        (projectDeclarationBound declarationBound)
        (bcrConstructionOperatedType route)
        || completeUnboundedForallSpecializesTo
          (projectDeclarationBound declarationBound)
          (bcrConstructionOperatedType route)

    -- The route's operated endpoint has already entered the exact
    -- construction quotient.  Compare a declaration bound in that same
    -- domain, using only graph-node aliases carried by the active
    -- construction Gamma; never infer a rename from matching type shape.
    projectDeclarationBound declarationBound =
      foldr projectFreeRef declarationAtConstruction
        (freeTypeVarRefsType declarationAtConstruction)
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

authorizedBodyConsumerDeclarationBound
  :: BodyConsumerDeclarationAuthority
  -> ElabType
authorizedBodyConsumerDeclarationBound authority =
  case authority of
    BodyConsumerLocallyEmitted _ declarationBound ->
      declarationBound
    BodyConsumerInheritedAmbient _ declarationBound ->
      declarationBound

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
  if operationalEndpointTypesAgree checkedSourceAtConstruction projectedType
    then
      Right
        ValidatedBodyConsumerProjection
          { vbcpRoute = route,
            vbcpSourceConstructionRenames =
              sourceConstructionRenames,
            vbcpProjectedType = projectedType
          }
    else
      Left
        ( ValidationFailed
            [ "validated lambda-body consumer projection disagrees with its checked source"
            , "  route: " ++ show route
            , "  checked source: " ++ show checkedSource
            , "  source-to-construction renames: "
                ++ show sourceConstructionRenames
            , "  checked source at construction: "
                ++ show checkedSourceAtConstruction
            , "  projected operated type: " ++ show projectedType
            ]
        )
  where
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

-- | Once the projection certificate exists, the selected consumer already
-- denotes the checked source endpoint.  Specialization is therefore the
-- identity computation.  Both accepted references are exact endpoints of the
-- validated route; names, representatives, and type shape are not consulted.
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
      Just InstId
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
          ( \(ambientRef, previousBound) ->
              if
                  operationalEndpointTypesAgree
                    previousBound
                    (vbcpProjectedType projection)
                then Nothing
                else
                  Just
                    BodyConsumerBoundRefinementCertificate
                      { bcbrDeclarationAuthority = declarationAuthority,
                        bcbrAmbientRef = ambientRef,
                        bcbrPreviousBound = previousBound,
                        bcbrCompletedBound = vbcpProjectedType projection
                      }
          )
            =<< mbPreviousBinding
    )
  where
    route = authorizedBodyConsumerRoute declarationAuthority

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
-- every certificate must identify one surviving ambient root declaration,
-- the matching local-Gamma closure must still own its edge/exterior, and all
-- dependencies of the completed bound must already be available.  Thus the
-- final ETyAbs spine is correct by construction rather than repaired after an
-- InstAbstr failure.
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
      certificates
  mapM_ (validateCertificate projected) certificates
  pure projected
  where
    duplicateTargets =
      [ bcbrAmbientRef certificate
      | (index, certificate) <- zip [0 :: Int ..] certificates
      , any
          ( typeBinderRefsSameIdentity
              (bcbrAmbientRef certificate)
              . bcbrAmbientRef
          )
          (drop (index + 1) certificates)
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
          targetRef = bcbrAmbientRef certificate
          localOverlap =
            any (typeBinderRefsSameIdentity targetRef) localRefs
          ambientAuthorized =
            any (typeBinderRefsSameIdentity targetRef) ambientRefs
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
                      ambientRefs
                  )
            ]
      unless
        (length matchingClosures == 1)
        ( refinementFailure
            "certificate has no unique local-Gamma owner/edge/exterior closure"
            [ "  route: " ++ show route,
              "  matching closures: " ++ show matchingClosures
            ]
        )
      when
        localOverlap
        ( refinementFailure
            "refined ambient declaration overlaps an owner-emitted binder"
            ["  target: " ++ show targetRef]
        )
      unless
        ambientAuthorized
        ( refinementFailure
            "refined declaration is absent from the owner's ambient-use authority"
            [ "  target: " ++ show targetRef,
              "  ambient authority: " ++ show ambientRefs
            ]
        )
      unless
        (null dependencyFailures)
        ( refinementFailure
            "completed ambient bound has an unavailable or forward dependency"
            [ "  target: " ++ show targetRef,
              "  completed bound: "
                ++ show (bcbrCompletedBound certificate),
              "  unavailable dependencies: " ++ show dependencyFailures,
              "  ambient authority: " ++ show ambientRefs,
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
    certificates

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
      | boundAgrees currentBound completedBound ->
          pure
            (replaceTarget completedBinderBound)
      | boundAgrees currentBound previousBound ->
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

    boundAgrees mbBound expected =
      operationalEndpointTypesAgree
        (maybe TBottom tyToElab mbBound)
        expected

    projectionFailure :: String -> [String] -> Either ElabError a
    projectionFailure detail context =
      Left
        ( ValidationFailed
            ( [ "cannot project certified body-consumer bound into root construction",
                "  detail: " ++ detail,
                "  certificate: " ++ show certificate,
                "  target: " ++ show targetRef,
                "  provisional bound: " ++ show previousBound,
                "  completed bound: " ++ show completedBound
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
  deriving (Eq, Show)

data LambdaParamBoundaryInstallation = LambdaParamBoundaryInstallation
  { lambdaParamBoundaryType :: !ElabType,
    lambdaParamBoundaryLocalBinderIdentities :: !(Set.Set TypeBinderIdentity),
    lambdaParamBoundaryGammaAliases :: !(IntMap.IntMap TypeBinderRef),
    lambdaParamBoundaryTypeBindings :: !(Map.Map TypeBinderRef ElabType)
  }
  deriving (Eq, Show)

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
          | not (typesAgree exactTy constructionTy) ->
              Left
                ( ValidationFailed
                    [ "compiler exact lambda parameter source type disagrees with its constructed domain"
                    , "  parameter node: " ++ show paramNode
                    , "  exact source type: " ++ show exactTy
                    , "  constructed domain: " ++ show constructionTy
                    ]
                )
        _ -> pure ()
      pure
        LambdaParamBoundaryInstallation
          { lambdaParamBoundaryType = exactTy,
            lambdaParamBoundaryLocalBinderIdentities =
              boundaryTypeIdentities exactTy,
            lambdaParamBoundaryGammaAliases =
              IntMap.delete (getNodeId paramNode) gammaAliases,
            lambdaParamBoundaryTypeBindings = typeBindings
          }
    ConstructedLambdaParamBoundary constructionTy ->
      case IntMap.lookup (getNodeId paramNode) identityRoutes of
        Nothing ->
          pure
            LambdaParamBoundaryInstallation
              { lambdaParamBoundaryType = constructionTy,
                lambdaParamBoundaryLocalBinderIdentities =
                  boundaryTypeIdentities constructionTy,
                lambdaParamBoundaryGammaAliases = gammaAliases,
                lambdaParamBoundaryTypeBindings = typeBindings
              }
        Just outwardRef -> do
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
                    typeBindings
              }
  where
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
              || ordinaryLocalEmissionAuthorizes closure ->
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
    Nothing -> True
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
