module MLF.Elab.Elaborate.Algebra.TestSupport
  ( ConstructionBinderBoundProvenance (..),
    BodyConsumerRouteTestView (..),
    DirectAmbientGammaAuthorityProvenance (..),
    DirectApplicationAmbientGammaClaim (..),
    DirectApplicationGammaClaim (..),
    PendingLocalResultSourcePacket,
    constructOrdinaryGammaPacketForTest,
    constructionBoundAfterScopeExtensionForTest,
    buildAmbientGammaAuthoritiesForTest,
    certifiedSourcePacketOperatedOccurrenceRenamesForTest,
    certifiedSourcePacketOccurrenceRenamesForTest,
    certifiedSourceOccurrenceRenamesForTest,
    certifiedSourceOccurrenceRoutesForTest,
    constructionProtectedIdentitiesForTest,
    constructionRefAlreadyInGammaForTest,
    constructionRouteBoundCompatibleForTest,
    closedTermTypeChecksForTest,
    directSourceBinderConstructionRenameForTest,
    directAmbientGammaAuthorityProvenanceForTest,
    bodyConsumerProjectionProvenanceForTest,
    bodyConsumerRouteProjectionProvenanceForTest,
    bodyConsumerLocallyEmittedRouteProjectionProvenanceForTest,
    frozenEndpointTypesByLexicalPublicationForTest,
    inheritNestedApplicationResidualAuthorityForTest,
    inheritNestedApplicationResidualReplayAuthorityForTest,
    inheritNestedApplicationZeroLocalResidualAuthorityForTest,
    inferExactTransportArgumentsForTest,
    inferInstAppArgsFromSchemeRefsForTest,
    installConstructedLambdaParamBoundaryForTest,
    installExactLambdaParamBoundaryForTest,
    lambdaBodyConstructionRenamesForTest,
    lambdaParamConstructionRenamesForTest,
    lambdaParamLocalGammaRenamesForTest,
    lambdaParamProtectedIdentitiesForTest,
    protectedBoundaryOccurrenceRenamesForTest,
    localGammaConstructionProvenanceForTest,
    mergeConstructionBinderBoundsByProvenanceForTest,
    mkApplicationPendingLocalResultSourcePacketForTest,
    operationalEndpointTypesAgreeForTest,
    validateBodyConsumerCheckedSourceProjectionForTest,
    validatedBodyConsumerLeadingEliminationForTest,
    validatedBodyConsumerProjectionSpecializationForTest,
    projectValidatedAmbientConsumerBoundForTest,
    projectBodyConsumerBoundWithCertificateForTest,
    attachBodyConsumerBoundRefinementForTest,
    selectBodyConsumerRouteForTest,
    selectBodyConsumerRouteWithPacketForTest,
    selectTermSourcePacketOccurrenceRenamesForTest,
    validateBodyConsumerRouteForTest,
    requirementNeedsLocalConstructionForTest,
    mergeConstructionSourceBinderRefsForTest,
    selectLocalGammaClosureOwnerLaneForTest,
    selectDirectAmbientGammaAuthorityForTest,
    selectDirectLocalApplicationArgumentTopologyForTest,
    selectBoundaryConstructionRouteForTest,
    sourceBinderAuthorityRefsForTest,
    transparentResultResolvedByEnclosingSchemeForTest,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (EdgeId, NodeId)
import MLF.Constraint.Presolution.Plan.Requirements
  ( AmbientGammaAuthority (..),
    GeneralizationRequirements,
    RequiredGammaBinder,
    RequiredGammaPlacement,
  )
import MLF.Elab.Elaborate.Algebra.ConstructionGamma
  ( FrozenEndpointCertificate (..),
    ConstructionBinderBoundProvenance (..),
    DirectAmbientGammaAuthorityProvenance (..),
    PendingLocalResultSourcePacket,
    LambdaParamBoundaryAuthority (..),
    LambdaParamBoundaryInstallation (..),
    OrdinaryGammaPacketConstruction (..),
    authorizeBodyConsumerDeclaration,
    buildAmbientGammaAuthorities,
    bodyConsumerProjectionProvenance,
    bodyConsumerRouteProjectionProvenance,
    certifiedSourcePacketOperatedOccurrenceRenames,
    certifiedSourcePacketOccurrenceRenames,
    certifiedSourceOccurrenceRenames,
    certifiedSourceOccurrenceRoutes,
    constructOrdinaryGammaPacket,
    constructionProtectedIdentities,
    constructionRefAlreadyInGamma,
    constructionRouteBoundCompatible,
    directSourceBinderConstructionRename,
    directAmbientGammaAuthorityProvenance,
    frozenEndpointCertificateTypes,
    inheritNestedApplicationResidualAuthority,
    inheritNestedApplicationResidualReplayAuthority,
    inferExactTransportArguments,
    installLambdaParamBoundary,
    lambdaBodyConstructionRenames,
    lambdaParamConstructionRenames,
    lambdaParamLocalGammaRenames,
    lambdaParamProtectedIdentities,
    protectedBoundaryOccurrenceRenames,
    selectTermSourcePacketOccurrenceRenames,
    localGammaConstructionProvenance,
    nestedApplicationResidualFromLocalGamma,
    nestedApplicationResidualFromZeroLocalConstruction,
    mergeConstructionBinderBoundsByProvenance,
    mkApplicationPendingLocalResultSourcePacket,
    requirementNeedsLocalConstruction,
    mergeConstructionSourceBinderRefs,
    operationalEndpointTypesAgree,
    mkValidatedBodyConsumerProjection,
    validatedBodyConsumerLeadingElimination,
    validatedBodyConsumerProjectionSpecialization,
    projectValidatedAmbientConsumerBound,
    projectValidatedAmbientConsumerBoundWithCertificate,
    publishFrozenEndpointCertificate,
    selectLocalGammaClosureOwnerLane,
    selectDirectAmbientGammaAuthority,
    selectDirectLocalApplicationArgumentTopology,
    selectBoundaryConstructionRoute,
    sourceBinderAuthorityRefs,
    transparentResultResolvedByEnclosingScheme,
  )
import MLF.Elab.Elaborate.Algebra.BodyConsumerRoute
  ( BodyConsumerRoute (..),
    selectBodyConsumerRoute,
    selectBodyConsumerRouteWithPacket,
    validateBodyConsumerRoute,
  )
import MLF.Elab.Elaborate.Algebra
  ( OwnerFinalConstruction (..),
    alignEnvToConstructionBinderRenames,
    extendEnvTypeScope,
    mkEnv,
    typeCheckEnvFrom,
  )
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Generalize
  ( LocalGammaClosure,
    LocalGammaOwner,
    PreparedSubtermGeneralization,
  )
import MLF.Elab.Run.Generalize.Types
  ( DirectApplicationAmbientGammaClaim (..),
    DirectApplicationGammaClaim (..),
    LocalGammaConstructionCertificate,
  )
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromSchemeRefs)
import MLF.Elab.Types
  ( BoundType,
    ElabError (..),
    ElabScheme,
    ElabType,
    Instantiation,
    SchemeInfo,
    XmlfTerm,
    elabToBound,
  )
import MLF.Types.Elab (TypeBinderIdentity, TypeBinderRef)

-- | Inspectable mirror of the private production route.  Keeping the mirror
-- in test support lets guard tests corrupt one fact at a time without making
-- the production constructor part of the exposed Algebra surface.
data BodyConsumerRouteTestView = BodyConsumerRouteTestView
  { bcrtvEdgeId :: !EdgeId
  , bcrtvOwner :: !LocalGammaOwner
  , bcrtvExteriorNode :: !NodeId
  , bcrtvSemanticRef :: !TypeBinderRef
  , bcrtvConstructionRef :: !TypeBinderRef
  , bcrtvOperatedType :: !ElabType
  , bcrtvConstructionOperatedType :: !ElabType
  }
  deriving (Eq, Show)

closedTermTypeChecksForTest :: XmlfTerm -> Bool
closedTermTypeChecksForTest term =
  case TypeCheck.typeCheck term of
    Right _ -> True
    Left _ -> False

selectBodyConsumerRouteForTest
  :: LocalGammaOwner
  -> EdgeId
  -> GeneralizationRequirements
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (Maybe BodyConsumerRouteTestView)
selectBodyConsumerRouteForTest owner edgeId requirements aliases =
  fmap (fmap bodyConsumerRouteTestView)
    (selectBodyConsumerRoute owner edgeId requirements aliases)

selectBodyConsumerRouteWithPacketForTest
  :: LocalGammaOwner
  -> EdgeId
  -> PreparedSubtermGeneralization
  -> GeneralizationRequirements
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (Maybe BodyConsumerRouteTestView)
selectBodyConsumerRouteWithPacketForTest owner edgeId packet requirements aliases =
  fmap (fmap bodyConsumerRouteTestView)
    ( selectBodyConsumerRouteWithPacket
        owner
        edgeId
        (Just packet)
        requirements
        aliases
    )

validateBodyConsumerRouteForTest
  :: LocalGammaOwner
  -> EdgeId
  -> IntMap.IntMap TypeBinderRef
  -> PreparedSubtermGeneralization
  -> BodyConsumerRouteTestView
  -> Either ElabError ()
validateBodyConsumerRouteForTest owner edgeId aliases packet routeView = do
  validateBodyConsumerRoute
    owner
    edgeId
    packet
    aliases
    (bodyConsumerRouteFromTestView routeView)

bodyConsumerRouteTestView
  :: BodyConsumerRoute
  -> BodyConsumerRouteTestView
bodyConsumerRouteTestView route =
  BodyConsumerRouteTestView
    { bcrtvEdgeId = bcrEdgeId route
    , bcrtvOwner = bcrOwner route
    , bcrtvExteriorNode = bcrExteriorNode route
    , bcrtvSemanticRef = bcrSemanticRef route
    , bcrtvConstructionRef = bcrConstructionRef route
    , bcrtvOperatedType = bcrOperatedType route
    , bcrtvConstructionOperatedType = bcrConstructionOperatedType route
    }

bodyConsumerRouteFromTestView
  :: BodyConsumerRouteTestView
  -> BodyConsumerRoute
bodyConsumerRouteFromTestView route =
  BodyConsumerRoute
    { bcrEdgeId = bcrtvEdgeId route
    , bcrOwner = bcrtvOwner route
    , bcrExteriorNode = bcrtvExteriorNode route
    , bcrSemanticRef = bcrtvSemanticRef route
    , bcrConstructionRef = bcrtvConstructionRef route
    , bcrOperatedType = bcrtvOperatedType route
    , bcrConstructionOperatedType = bcrtvConstructionOperatedType route
    }

constructOrdinaryGammaPacketForTest
  :: ElabType
  -> Maybe (TypeBinderRef, ElabType)
  -> Instantiation
  -> Maybe ([(TypeBinderRef, TypeBinderRef)], ElabScheme, Instantiation)
constructOrdinaryGammaPacketForTest bodySourceTy consumerBound inst = do
  construction <-
    constructOrdinaryGammaPacket bodySourceTy consumerBound inst
  pure
    ( ogpcBoundBinderRoutes construction,
      ogpcScheme construction,
      ogpcInstantiation construction
    )

constructionBoundAfterScopeExtensionForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, Maybe BoundType)]
  -> TypeBinderRef
  -> Either ElabError (Maybe ElabType)
constructionBoundAfterScopeExtensionForTest renames binders ref = do
  constructionEnv <-
    alignEnvToConstructionBinderRenames renames (mkEnv Map.empty)
  pure
    ( TypeCheck.lookupTypeBindingRef
        ref
        (typeCheckEnvFrom (extendEnvTypeScope binders constructionEnv))
    )

buildAmbientGammaAuthoritiesForTest
  :: IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, ElabType)]
  -> Either
      ElabError
      (IntMap.IntMap (TypeBinderRef, ElabType))
buildAmbientGammaAuthoritiesForTest aliases bindings =
  fmap
    ( IntMap.map
        (\authority -> (agaExactRef authority, agaBound authority))
    )
    (buildAmbientGammaAuthorities aliases (Map.fromList bindings))

constructionRefAlreadyInGammaForTest
  :: (NodeId -> NodeId)
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> Bool
constructionRefAlreadyInGammaForTest =
  constructionRefAlreadyInGamma

constructionRouteBoundCompatibleForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeBinderRef
  -> TypeBinderRef
  -> Maybe ElabType
  -> Maybe ElabType
  -> Bool
constructionRouteBoundCompatibleForTest =
  constructionRouteBoundCompatible

certifiedSourceOccurrenceRoutesForTest
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
certifiedSourceOccurrenceRoutesForTest =
  certifiedSourceOccurrenceRoutes

certifiedSourcePacketOccurrenceRenamesForTest
  :: IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
certifiedSourcePacketOccurrenceRenamesForTest =
  certifiedSourcePacketOccurrenceRenames

certifiedSourcePacketOperatedOccurrenceRenamesForTest
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
certifiedSourcePacketOperatedOccurrenceRenamesForTest =
  certifiedSourcePacketOperatedOccurrenceRenames

selectTermSourcePacketOccurrenceRenamesForTest
  :: [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
selectTermSourcePacketOccurrenceRenamesForTest =
  selectTermSourcePacketOccurrenceRenames

certifiedSourceOccurrenceRenamesForTest
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
certifiedSourceOccurrenceRenamesForTest =
  certifiedSourceOccurrenceRenames

directSourceBinderConstructionRenameForTest
  :: IntMap.IntMap TypeBinderRef
  -> Int
  -> TypeBinderRef
  -> Maybe (TypeBinderRef, TypeBinderRef)
directSourceBinderConstructionRenameForTest =
  directSourceBinderConstructionRename

directAmbientGammaAuthorityProvenanceForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> Maybe PendingLocalResultSourcePacket
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> NodeId
  -> TypeBinderRef
  -> ElabType
  -> DirectAmbientGammaAuthorityProvenance
directAmbientGammaAuthorityProvenanceForTest renames owner sourcePacket closures requirement node ref bound =
  directAmbientGammaAuthorityProvenance
    renames
    owner
    sourcePacket
    closures
    requirement
    node
    (AmbientGammaAuthority ref bound)

mkApplicationPendingLocalResultSourcePacketForTest
  :: LocalGammaOwner
  -> EdgeId
  -> [RequiredGammaBinder]
  -> SchemeInfo
  -> Either ElabError PendingLocalResultSourcePacket
mkApplicationPendingLocalResultSourcePacketForTest =
  mkApplicationPendingLocalResultSourcePacket

bodyConsumerProjectionProvenanceForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> NodeId
  -> TypeBinderRef
  -> ElabType
  -> DirectAmbientGammaAuthorityProvenance
bodyConsumerProjectionProvenanceForTest renames owner closures requirement node ref bound =
  bodyConsumerProjectionProvenance
    renames
    owner
    closures
    requirement
    node
    (AmbientGammaAuthority ref bound)

bodyConsumerRouteProjectionProvenanceForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> BodyConsumerRouteTestView
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> DirectAmbientGammaAuthorityProvenance
bodyConsumerRouteProjectionProvenanceForTest renames owner closures requirement route projectedTy bindings =
  classifyBodyConsumerRouteProjectionForTest
    renames
    owner
    closures
    requirement
    []
    route
    projectedTy
    bindings

bodyConsumerLocallyEmittedRouteProjectionProvenanceForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> TypeBinderRef
  -> ElabType
  -> BodyConsumerRouteTestView
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> DirectAmbientGammaAuthorityProvenance
bodyConsumerLocallyEmittedRouteProjectionProvenanceForTest renames owner closures requirement localRef localBound route projectedTy bindings =
  case elabToBound localBound of
    Left _ -> DirectAmbientEstablished
    Right bound ->
      classifyBodyConsumerRouteProjectionForTest
        renames
        owner
        closures
        requirement
        [(localRef, Just bound)]
        route
        projectedTy
        bindings

classifyBodyConsumerRouteProjectionForTest
  :: [(TypeBinderRef, TypeBinderRef)]
  -> LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> [(TypeBinderRef, Maybe BoundType)]
  -> BodyConsumerRouteTestView
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> DirectAmbientGammaAuthorityProvenance
classifyBodyConsumerRouteProjectionForTest renames owner closures requirement localBinders routeView projectedTy bindings =
  case
      authorizeBodyConsumerDeclaration
        IntMap.empty
        []
        localBinders
        bindings
        route
    of
      Right (Just declarationAuthority) ->
        bodyConsumerRouteProjectionProvenance
          renames
          owner
          closures
          requirement
          declarationAuthority
          projectedTy
          bindings
      _ -> DirectAmbientEstablished
  where
    route = bodyConsumerRouteFromTestView routeView

frozenEndpointTypesByLexicalPublicationForTest
  :: [(EdgeId, NodeId, ElabType)]
  -> IntMap.IntMap ElabType
frozenEndpointTypesByLexicalPublicationForTest =
  frozenEndpointCertificateTypes
    . foldl
      ( \certificates (edge, root, endpointType) ->
          publishFrozenEndpointCertificate
            (FrozenEndpointCertificate edge root endpointType)
            certificates
      )
      IntMap.empty

inheritNestedApplicationResidualAuthorityForTest
  :: LocalGammaOwner
  -> LocalGammaConstructionCertificate
  -> GeneralizationRequirements
  -> Either ElabError GeneralizationRequirements
inheritNestedApplicationResidualAuthorityForTest owner certificate requirements = do
  residualCertificate <-
    nestedApplicationResidualFromLocalGamma owner certificate
  inheritNestedApplicationResidualAuthority
    owner
    residualCertificate
    requirements

inheritNestedApplicationResidualReplayAuthorityForTest
  :: LocalGammaOwner
  -> LocalGammaConstructionCertificate
  -> GeneralizationRequirements
  -> GeneralizationRequirements
  -> Either
      ElabError
      (GeneralizationRequirements, GeneralizationRequirements)
inheritNestedApplicationResidualReplayAuthorityForTest
  owner
  certificate
  ownerRequirements
  replayRequirements = do
    residualCertificate <-
      nestedApplicationResidualFromLocalGamma owner certificate
    inheritNestedApplicationResidualReplayAuthority
      owner
      residualCertificate
      ownerRequirements
      replayRequirements

inheritNestedApplicationZeroLocalResidualAuthorityForTest
  :: LocalGammaOwner
  -> LocalGammaOwner
  -> ElabType
  -> [TypeBinderRef]
  -> IntMap.IntMap TypeBinderRef
  -> GeneralizationRequirements
  -> Either ElabError GeneralizationRequirements
inheritNestedApplicationZeroLocalResidualAuthorityForTest
  expectedOwner
  actualOwner
  constructedType
  locallyEmittedBinderRefs
  localBinderRoutes
  requirements = do
    residualCertificate <-
      nestedApplicationResidualFromZeroLocalConstruction
        expectedOwner
        actualOwner
        constructedType
        locallyEmittedBinderRefs
        localBinderRoutes
    inheritNestedApplicationResidualAuthority
      expectedOwner
      residualCertificate
      requirements

constructionProtectedIdentitiesForTest
  :: (TypeBinderRef -> Bool)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Set.Set TypeBinderIdentity
  -> Set.Set TypeBinderIdentity
constructionProtectedIdentitiesForTest =
  constructionProtectedIdentities

inferInstAppArgsFromSchemeRefsForTest
  :: [(TypeBinderRef, Maybe BoundType)]
  -> ElabType
  -> ElabType
  -> Maybe [ElabType]
inferInstAppArgsFromSchemeRefsForTest =
  inferInstAppArgsFromSchemeRefs

inferExactTransportArgumentsForTest
  :: (ElabType -> ElabType -> Bool)
  -> ElabScheme
  -> ElabType
  -> Maybe [ElabType]
inferExactTransportArgumentsForTest =
  inferExactTransportArguments

installExactLambdaParamBoundaryForTest
  :: NodeId
  -> ElabType
  -> Maybe ElabType
  -> IntMap.IntMap TypeBinderRef
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( ElabType
      , Set.Set TypeBinderIdentity
      , IntMap.IntMap TypeBinderRef
      , Map.Map TypeBinderRef ElabType
      )
installExactLambdaParamBoundaryForTest paramNode exactTy constructedTy aliases bindings = do
  installation <-
    installLambdaParamBoundary
      paramNode
      (ExactSourceLambdaParamBoundary exactTy constructedTy)
      aliases
      bindings
  pure
    ( lambdaParamBoundaryType installation
    , lambdaParamBoundaryLocalBinderIdentities installation
    , lambdaParamBoundaryGammaAliases installation
    , lambdaParamBoundaryTypeBindings installation
    )

installConstructedLambdaParamBoundaryForTest
  :: NodeId
  -> ElabType
  -> IntMap.IntMap TypeBinderRef
  -> Map.Map TypeBinderRef ElabType
  -> Either
      ElabError
      ( ElabType
      , Set.Set TypeBinderIdentity
      , IntMap.IntMap TypeBinderRef
      , Map.Map TypeBinderRef ElabType
      )
installConstructedLambdaParamBoundaryForTest paramNode constructionTy aliases bindings = do
  installation <-
    installLambdaParamBoundary
      paramNode
      (ConstructedLambdaParamBoundary constructionTy)
      aliases
      bindings
  pure
    ( lambdaParamBoundaryType installation
    , lambdaParamBoundaryLocalBinderIdentities installation
    , lambdaParamBoundaryGammaAliases installation
    , lambdaParamBoundaryTypeBindings installation
    )

lambdaParamConstructionRenamesForTest
  :: Set.Set TypeBinderIdentity
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamConstructionRenamesForTest =
  lambdaParamConstructionRenames

lambdaBodyConstructionRenamesForTest
  :: Set.Set TypeBinderIdentity
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaBodyConstructionRenamesForTest =
  lambdaBodyConstructionRenames

lambdaParamLocalGammaRenamesForTest
  :: NodeId
  -> ElabType
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
lambdaParamLocalGammaRenamesForTest =
  lambdaParamLocalGammaRenames

lambdaParamProtectedIdentitiesForTest
  :: Set.Set TypeBinderIdentity
  -> (TypeBinderRef -> Bool)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Set.Set TypeBinderIdentity
  -> Set.Set TypeBinderIdentity
lambdaParamProtectedIdentitiesForTest =
  lambdaParamProtectedIdentities

protectedBoundaryOccurrenceRenamesForTest
  :: Set.Set TypeBinderIdentity
  -> IntMap.IntMap TypeBinderRef
  -> [TypeBinderRef]
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
protectedBoundaryOccurrenceRenamesForTest =
  protectedBoundaryOccurrenceRenames

localGammaConstructionProvenanceForTest
  :: String
  -> [(TypeBinderRef, Maybe BoundType)]
  -> [IntMap.IntMap TypeBinderRef]
  -> IntMap.IntMap TypeBinderRef
  -> Either
      ElabError
      (IntMap.IntMap TypeBinderRef, IntMap.IntMap TypeBinderRef)
localGammaConstructionProvenanceForTest =
  localGammaConstructionProvenance

mergeConstructionBinderBoundsByProvenanceForTest
  :: String
  -> [( ConstructionBinderBoundProvenance
      , (TypeBinderRef, Maybe BoundType)
      )]
  -> Either ElabError [(TypeBinderRef, Maybe BoundType)]
mergeConstructionBinderBoundsByProvenanceForTest =
  mergeConstructionBinderBoundsByProvenance

operationalEndpointTypesAgreeForTest
  :: ElabType
  -> ElabType
  -> Bool
operationalEndpointTypesAgreeForTest =
  operationalEndpointTypesAgree

validateBodyConsumerCheckedSourceProjectionForTest
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> BodyConsumerRouteTestView
  -> ElabType
  -> ElabType
  -> Either ElabError ()
validateBodyConsumerCheckedSourceProjectionForTest sourceBinderRefs constructionAliases constructionBinderRenames route checkedSource projectedType = do
  _ <-
    mkValidatedBodyConsumerProjection
      sourceBinderRefs
      constructionAliases
      constructionBinderRenames
      (bodyConsumerRouteFromTestView route)
      checkedSource
      projectedType
  pure ()

validatedBodyConsumerLeadingEliminationForTest
  :: BodyConsumerRouteTestView
  -> ElabType
  -> ElabType
  -> ElabType
  -> ElabType
  -> ElabType
  -> Either ElabError (Maybe Instantiation)
validatedBodyConsumerLeadingEliminationForTest
  route
  checkedSource
  projectedType
  constructionOperatedType
  constructedLambdaType
  expectedLambdaType = do
  projection <-
    mkValidatedBodyConsumerProjection
      IntMap.empty
      IntMap.empty
      []
      (bodyConsumerRouteFromTestView route)
      checkedSource
      projectedType
  pure
    ( validatedBodyConsumerLeadingElimination
        constructionOperatedType
        constructedLambdaType
        expectedLambdaType
        projection
    )

validatedBodyConsumerProjectionSpecializationForTest
  :: BodyConsumerRouteTestView
  -> ElabType
  -> ElabType
  -> TypeBinderRef
  -> ElabType
  -> Either ElabError (Maybe Instantiation)
validatedBodyConsumerProjectionSpecializationForTest route checkedSource projectedType resultRef publishedType = do
  projection <-
    mkValidatedBodyConsumerProjection
      IntMap.empty
      IntMap.empty
      []
      (bodyConsumerRouteFromTestView route)
      checkedSource
      projectedType
  pure
    ( validatedBodyConsumerProjectionSpecialization
        resultRef
        publishedType
        projection
    )

projectValidatedAmbientConsumerBoundForTest
  :: DirectAmbientGammaAuthorityProvenance
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType)
projectValidatedAmbientConsumerBoundForTest =
  projectValidatedAmbientConsumerBound

projectBodyConsumerBoundWithCertificateForTest
  :: DirectAmbientGammaAuthorityProvenance
  -> [(TypeBinderRef, Maybe BoundType)]
  -> BodyConsumerRouteTestView
  -> ElabType
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> Either ElabError (Map.Map TypeBinderRef ElabType, Bool)
projectBodyConsumerBoundWithCertificateForTest provenance localBinders routeView checkedSource projectedType ambientBindings = do
  declarationAuthority <-
    authorizeBodyConsumerDeclaration
      IntMap.empty
      []
      localBinders
      ambientBindings
      route
      >>= maybe
        ( Left
            ( ValidationFailed
                ["test body-consumer route has no declaration authority"]
            )
        )
        Right
  projection <-
    mkValidatedBodyConsumerProjection
      IntMap.empty
      IntMap.empty
      []
      route
      checkedSource
      projectedType
  (projectedBindings, mbCertificate) <-
    projectValidatedAmbientConsumerBoundWithCertificate
      provenance
      declarationAuthority
      projection
      ambientBindings
  pure
    ( projectedBindings,
      case mbCertificate of
        Nothing -> False
        Just _ -> True
    )
  where
    route = bodyConsumerRouteFromTestView routeView

attachBodyConsumerBoundRefinementForTest
  :: DirectAmbientGammaAuthorityProvenance
  -> [(TypeBinderRef, Maybe BoundType)]
  -> BodyConsumerRouteTestView
  -> ElabType
  -> Map.Map TypeBinderRef ElabType
  -> OwnerFinalConstruction
  -> Either ElabError OwnerFinalConstruction
attachBodyConsumerBoundRefinementForTest provenance localBinders routeView projectedType ambientBindings ownerCertificate = do
  declarationAuthority <-
    authorizeBodyConsumerDeclaration
      IntMap.empty
      []
      localBinders
      ambientBindings
      route
      >>= maybe
        ( Left
            ( ValidationFailed
                ["test body-consumer route has no declaration authority"]
            )
        )
        Right
  projection <-
    mkValidatedBodyConsumerProjection
      IntMap.empty
      IntMap.empty
      []
      route
      projectedType
      projectedType
  (_, mbCertificate) <-
    projectValidatedAmbientConsumerBoundWithCertificate
      provenance
      declarationAuthority
      projection
      ambientBindings
  certificate <-
    maybe
      ( Left
          ( ValidationFailed
              ["test body-consumer route did not refine an ambient declaration"]
          )
      )
      Right
      mbCertificate
  pure
    ownerCertificate
      { ofcBodyConsumerBoundRefinements =
          ofcBodyConsumerBoundRefinements ownerCertificate
            ++ [certificate]
      }
  where
    route = bodyConsumerRouteFromTestView routeView

requirementNeedsLocalConstructionForTest
  :: Bool
  -> RequiredGammaPlacement
  -> Bool
  -> Bool
requirementNeedsLocalConstructionForTest =
  requirementNeedsLocalConstruction

selectDirectAmbientGammaAuthorityForTest
  :: TypeBinderRef
  -> [(DirectAmbientGammaAuthorityProvenance, TypeBinderRef, ElabType)]
  -> Either ElabError (Maybe (TypeBinderRef, ElabType))
selectDirectAmbientGammaAuthorityForTest requiredRef authorities =
  fmap
    (fmap (\authority -> (agaExactRef authority, agaBound authority)))
    ( selectDirectAmbientGammaAuthority
        requiredRef
        [ (provenance, AmbientGammaAuthority ref bound)
        | (provenance, ref, bound) <- authorities
        ]
    )

selectDirectLocalApplicationArgumentTopologyForTest
  :: Maybe ElabType
  -> Maybe ElabType
  -> Maybe ElabType
  -> Maybe ElabType
selectDirectLocalApplicationArgumentTopologyForTest =
  selectDirectLocalApplicationArgumentTopology

selectLocalGammaClosureOwnerLaneForTest
  :: LocalGammaOwner
  -> IntMap.IntMap LocalGammaClosure
  -> RequiredGammaBinder
  -> Either ElabError (Maybe LocalGammaClosure)
selectLocalGammaClosureOwnerLaneForTest =
  selectLocalGammaClosureOwnerLane

mergeConstructionSourceBinderRefsForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
mergeConstructionSourceBinderRefsForTest =
  mergeConstructionSourceBinderRefs

selectBoundaryConstructionRouteForTest
  :: Maybe TypeBinderRef
  -> [TypeBinderRef]
  -> TypeBinderRef
  -> TypeBinderRef
  -> Maybe TypeBinderRef
selectBoundaryConstructionRouteForTest =
  selectBoundaryConstructionRoute

sourceBinderAuthorityRefsForTest
  :: (NodeId -> NodeId)
  -> IntSet.IntSet
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap (TypeBinderRef, ElabType)
  -> Either ElabError (IntMap.IntMap TypeBinderRef)
sourceBinderAuthorityRefsForTest representative directKeys sourceRefs authorities =
  sourceBinderAuthorityRefs
    representative
    directKeys
    sourceRefs
    ( IntMap.map
        (\(ref, bound) -> AmbientGammaAuthority ref bound)
        authorities
    )

transparentResultResolvedByEnclosingSchemeForTest
  :: (NodeId -> NodeId)
  -> SchemeInfo
  -> TypeBinderRef
  -> SchemeInfo
  -> Bool
transparentResultResolvedByEnclosingSchemeForTest =
  transparentResultResolvedByEnclosingScheme
