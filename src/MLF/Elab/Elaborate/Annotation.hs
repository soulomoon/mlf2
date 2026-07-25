{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MLF.Elab.Elaborate.Annotation
  ( AnnotationContext (..),
    AnnotationBoundaryRole (..),
    closeTermForAnnotation,
    stripUnusedTopTyAbs,
    sourceAnnIsPolymorphicResolved,
    sourceAnnSchemeInfoResolved,
    annBinderKey,
    annExprReferenceKey,
    desugaredAnnLambdaInfo,
    elaborateAnnotationTerm,
    elaborateExactAnnotationTerm,
    elaborateClosedExactAnnotationTerm,
    elaborateClosedExactAnnotationTermAtType,
    constructExactTermAtType,
    validateAnnotationEdgeAuthority,
    validateElaborationEdgeAuthority,
    freshenTermTypeAbsAgainstEnv,
    reifyInst,
    reifyInstWithFrozenEndpoints,
    reifyInstWithFrozenEndpointsFromCheckedSource,
    reifyInstWithFrozenEndpointsFromCheckedSourceInConstructionGamma,
    reifyInstFromSourceScheme,
    reifyInstFromSourceSchemeInConstructionGamma,
    sourceSchemeInfoForConstruction,
    instSeqApps,
    sourceTypeToElabTypeWithIdentities,
    sourceTypeToElabTypeWithIdentitiesFromSupply,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard)
import Data.Functor.Foldable (Recursive (project))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import qualified Data.Set as Set
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base
  ( EdgeArtifacts (..),
    getCopyMapping,
    rootWeakenRaiseMergeTraceAuthority,
  )
import MLF.Constraint.Presolution.Plan.Requirements (GeneralizationRequirements (..))
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindingError (..),
    EdgeId (..),
    NodeId (..),
    genRef,
  )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion (..), InstanceOp (..), ReplayContract (..), ewEdgeId, ewForallIntros, ewLeft, ewRight, ewWitness, getInstanceOps)
import MLF.Elab.Elaborate.Scope
  ( GeneralizeAtWith,
    ScopeContext (..),
    generalizeAtNode,
    reifyNodeTypePreferringBound,
    reifyTargetNodeType,
    reifyTargetType,
    scopeRootForNode,
    scopeTypeBinderIdentityRepresentative,
  )
import MLF.Elab.Generalize
  ( GammaPacketAuthority (..),
    SubtermConsumerAuthority,
    SubtermGeneralizations,
    gaConstructionRouteNodes,
    generalizationRequirementsForEnclosingRootEdges,
    generalizationRequirementsForRootEdges,
    generalizationRequirementsForRootEdgesInConstruction,
    placeSubtermGeneralizationBindersWithRoutes,
    publishSourceLambdaTopologyConsumerRoute,
    publishTopologyConsumerRoutes,
    prepareRootRaiseMergeScheme,
    publishSubtermGammaConstructionSourceSchemeInfo,
    rootRaiseMergeAuthorityForExpression,
    scaConsumerIdentity,
    subtermConsumerAuthorityEnclosingOwner,
    subtermGeneralizationConsumerAuthority,
    subtermGeneralizationConstructionBinderRenames,
    subtermGeneralizationGammaAuthority,
    subtermGeneralizationSchemeInfo,
    subtermGeneralizationsOwnedBy,
    sourceLambdaGeneralizedResultRouteRequest,
  )
import MLF.Elab.Inst (applyInstantiation, composeInst, instForLeadingTypeArgument, schemeToType)
import MLF.Elab.Elaborate.Annotation.Construction
  ( checkedOccurrenceSchemeInfo,
    scopedAnnotationConstructionBinderRenames,
    strictReplayCheckedSchemeInfo,
  )
import MLF.Elab.Phi
  ( PhiEndpointShapeAuthority,
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints,
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor,
  )
import MLF.Elab.Phi.Omega.Normalize (normalizeInst)
import qualified MLF.Elab.Reduce as Reduce
import qualified MLF.Elab.Sigma as Sigma
import MLF.Elab.Run.Annotation (adjustAnnotationInst, annNode)
import MLF.Elab.Run.Scope (generalizeTargetNode)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromSchemeRefs)
import MLF.Elab.Run.TypeOps (inlineBoundVarsTypeWithContext)
import MLF.Elab.SourceBinder
  ( resolveConstructionSourceBindersInSchemeInfoExcept,
    sourceBinderAliasSubstitution,
    typeBinderDeclarationRefs,
  )
import MLF.Elab.TermClosure
  ( alignTermTypeVarsToScheme,
    alignTermTypeVarsToTopTyAbs,
    alignTopTyAbsToScheme,
    closeTermWithSchemeSubstRefsIfNeeded,
    refreshLocalResolvedVarType,
    renameTermTypeVars,
    substInTermRefs,
  )
import MLF.Elab.TypeCheck (typeCheck)
import qualified MLF.Elab.TypeCheck as TypeCheck
import MLF.Elab.Types
  ( BoundType,
    ElabError (..),
    ElabScheme,
    XmlfTerm (..),
    XmlfTermF (..),
    ElabType,
    Instantiation (..),
    InstantiationF (..),
    SchemeInfo (..),
    Ty (..),
    elabToBound,
    eTyAbsWithRef,
    sourceTypeBinderRefsFromIdentities,
    sourceTypeBinderRefOrFreshInScope,
    instAbstrWithRef,
    instUnderWithRef,
    mapResolvedVarType,
    mapBoundType,
    mkElabSchemeWithRefs,
    renameTypeBinderRef,
    resolvedVarType,
    schemeBinderRefs,
    schemeBody,
    schemeFromType,
    schemeInfoFromRefSubst,
    schemeInfoBinderRefSubst,
    TypeBinderRef,
    typeBinderRefAliasNames,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
    typeBinderRefsSameIdentityAndName,
    typeBinderIdentityFromNode,
    typeBinderIdentityNode,
    tyToElab,
  )
import MLF.Frontend.ConstraintGen.Types
  ( AnnExpr (..),
    BindingKey (..),
    instantiationSiteEdgeId,
    instantiationSiteSource,
    instantiationSiteTarget,
  )
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Program.Types (resolvedSourceTypeToElabType)
import MLF.Frontend.Symbol (SymbolIdentity, lookupSymbolIdentityAlias)
import MLF.Frontend.Syntax
  ( NormSrcType,
    ResolvedSrcType,
    SrcBound (..),
    SrcNorm (NormN),
    SrcTy (..),
    StructBound
  )
import MLF.Reify.Type (reifyTypeWithNamedSetRefsNoFallbackReadModel)
import MLF.Reify.TypeOps
  ( alphaEqType,
    churchAwareEqType,
    freeTypeVarAliasNamesType,
    freeTypeVarRefsType,
    freshNameLike,
    implicitForallClosureMatches,
    substTypeCaptureRef,
  )
import MLF.Types.Identity
  ( IdDetails,
    IdentityGenerator,
    TypeBinderIdentity,
    advanceIdentityGeneratorPastMany,
    idDetailsIdentityKey,
    identityGeneratorAfter,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
  )
import MLF.Util.Trace (TraceConfig, traceElab)

data AnnotationContext (p :: Phase) = AnnotationContext
  { acTraceConfig :: TraceConfig,
    acScopeContext :: ScopeContext p,
    acAnnotationExpectedTypesByEdge :: IntMap.IntMap ElabType,
    acSourceTypeHeadIdentities :: Map.Map String SymbolIdentity,
    acSourceTypeBinderIdentities :: Map.Map String TypeBinderIdentity,
    acSourceBinderRefs :: IntMap.IntMap TypeBinderRef,
    acDirectSourceBinderKeys :: IntSet.IntSet,
    acSubtermGeneralizations :: SubtermGeneralizations,
    acEdgeWitnesses :: IntMap.IntMap EdgeWitness,
    acEdgeTraces :: IntMap.IntMap EdgeTrace,
    acEdgeExpansions :: IntMap.IntMap Expansion,
    acIdentityEdges :: IntSet.IntSet
  }

-- | Source-annotation authority belongs to the coercion occurrence, not its
-- result graph node.  Distinct annotations may solve to one canonical node but
-- retain distinct source types and lexical binder identities.
annotationExpectedTypeForEdge :: AnnotationContext p -> EdgeId -> Maybe ElabType
annotationExpectedTypeForEdge annotationContext edgeId =
  IntMap.lookup
    (getEdgeId edgeId)
    (acAnnotationExpectedTypesByEdge annotationContext)

-- | Validate the replay artifacts owned by source annotations before
-- elaboration starts.  An 'AAnn' is the source-level coercion form from
-- thesis §12.3.2/§15.3.8, so its instantiation edge is never an optional
-- let-simplification edge: the presolution witness, replay trace, and explicit
-- expansion are the construction authority for the emitted xMLF computation.
-- 'ALetScope' is intentionally excluded because it represents the
-- constraint-only Var-Let identity edge whose artifacts are discarded after
-- presolution.
validateAnnotationEdgeAuthority ::
  IntMap.IntMap ElabType ->
  IntMap.IntMap EdgeWitness ->
  IntMap.IntMap EdgeTrace ->
  IntMap.IntMap Expansion ->
  AnnExpr ->
  Either ElabError ()
validateAnnotationEdgeAuthority sourceTypes witnesses traces expansions = go
  where
    go ann =
      case ann of
        AResolvedVar {} -> Right ()
        ALit {} -> Right ()
        ALam _ _ _ _ body _ _ -> go body
        AApp fun arg _ _ _ -> go fun >> go arg
        ALet _ _ _ _ _ _ rhs body _ -> go rhs >> go body
        AAnn inner _ eid -> do
          requireSourceType eid
          requireArtifact "witness" witnesses eid
          requireArtifact "trace" traces eid
          requireArtifact "expansion" expansions eid
          go inner
        ALetScope inner _ _ -> go inner
        AUnfold inner _ _ -> go inner

    requireArtifact label artifacts eid@(EdgeId edgeKey)
      | IntMap.member edgeKey artifacts = Right ()
      | otherwise =
          Left
            ( ValidationFailed
                ["missing edge " ++ label ++ " for annotation " ++ show eid]
            )

    requireSourceType eid@(EdgeId edgeKey)
      | IntMap.member edgeKey sourceTypes = Right ()
      | otherwise =
          Left
            ( ValidationFailed
                ["missing source type for annotation " ++ show eid]
            )

-- | Validate every instantiation edge consumed by term elaboration.  The
-- thesis translation (Definition 15.3.12 and Figure 15.3.5) obtains each
-- application computation from its edge witness.  An edge may omit replay
-- artifacts only when an earlier phase retained explicit provenance that the
-- edge was discharged as xMLF identity by construction.
validateElaborationEdgeAuthority ::
  (NodeId -> NodeId) ->
  IntMap.IntMap ElabType ->
  IntMap.IntMap EdgeWitness ->
  IntMap.IntMap EdgeTrace ->
  IntMap.IntMap Expansion ->
  IntSet.IntSet ->
  AnnExpr ->
  Either ElabError ()
validateElaborationEdgeAuthority canonical sourceTypes witnesses traces expansions identityEdges = go
  where
    go ann =
      case ann of
        AResolvedVar {} -> Right ()
        ALit {} -> Right ()
        ALam _ _ _ _ body bodyEid _ -> do
          requireReplayOrIdentity "lambda body" bodyEid
          go body
        AApp fun arg funSite argSite _ -> do
          requireApplicationSite "application function" funSite
          requireApplicationSite "application argument" argSite
          go fun
          go arg
        ALet _ _ _ _ _ _ rhs body _ -> go rhs >> go body
        AAnn inner _ eid -> do
          requireSourceType eid
          requireReplay "annotation" eid
          go inner
        ALetScope inner _ eid -> do
          requireIdentity "let scope" eid
          go inner
        AUnfold inner _ eid -> do
          requireReplay "unfold" eid
          go inner

    requireReplayOrIdentity label eid@(EdgeId edgeKey)
      | IntSet.member edgeKey identityEdges = Right ()
      | otherwise = requireReplay label eid

    requireApplicationSite label site =
      let eid@(EdgeId edgeKey) = instantiationSiteEdgeId site
       in if IntSet.member edgeKey identityEdges
            then Right ()
            else do
              requireReplay label eid
              witness <-
                case IntMap.lookup edgeKey witnesses of
                  Just value -> Right value
                  Nothing ->
                    Left
                      (ValidationFailed ["missing edge witness for " ++ label ++ " " ++ show eid])
              if ewEdgeId witness == eid
                then Right ()
                else
                  Left
                    ( ValidationFailed
                        [ label ++ " witness edge id does not match its artifact key: " ++ show eid]
                    )
              if canonical (ewLeft witness) == canonical (instantiationSiteSource site)
                then Right ()
                else
                  Left
                    ( ValidationFailed
                        [label ++ " witness source does not match its construction site: " ++ show eid]
                    )
              if canonical (ewRight witness) == canonical (instantiationSiteTarget site)
                then Right ()
                else
                  Left
                    ( ValidationFailed
                        [label ++ " witness destination does not match its construction site: " ++ show eid]
                    )

    requireReplay label eid = do
      requireArtifact label "witness" witnesses eid
      requireArtifact label "trace" traces eid
      requireArtifact label "expansion" expansions eid

    requireIdentity label eid@(EdgeId edgeKey)
      | IntSet.member edgeKey identityEdges = Right ()
      | otherwise =
          Left
            ( ValidationFailed
                [label ++ " edge is missing identity provenance: " ++ show eid]
            )

    requireArtifact owner artifact artifacts eid@(EdgeId edgeKey)
      | IntMap.member edgeKey artifacts = Right ()
      | otherwise =
          Left
            ( ValidationFailed
                [ "missing edge "
                    ++ artifact
                    ++ " for "
                    ++ owner
                    ++ " "
                    ++ show eid
                ]
            )

    requireSourceType eid@(EdgeId edgeKey)
      | IntMap.member edgeKey sourceTypes = Right ()
      | otherwise =
          Left
            ( ValidationFailed
                ["missing source type for annotation " ++ show eid]
            )

closeTermForAnnotation :: XmlfTerm -> XmlfTerm
closeTermForAnnotation term =
  case typeCheck term of
    Right ty ->
      let freeRefs = freeTypeVarRefsType ty
          scheme = mkElabSchemeWithRefs [(ref, Nothing) | ref <- freeRefs] ty
       in closeTermWithSchemeSubstRefsIfNeeded IntMap.empty scheme term
    Left _ -> term

stripUnusedTopTyAbs :: XmlfTerm -> XmlfTerm
stripUnusedTopTyAbs term =
  case term of
    ETyAbsRef ref mbBound body ->
      let body' = stripUnusedTopTyAbs body
          term' = ETyAbsRef ref mbBound body'
       in case typeCheck term' of
        Right (TForallRef _ _ bodyTy)
          | not (any (typeBinderRefsSameIdentity ref) (freeTypeVarRefsType bodyTy)),
            not (any (typeBinderRefsSameIdentity ref) (Reduce.freeTypeVarRefsTerm body')) -> body'
        _ -> term'
    _ -> term

expInstantiateArgsToInstNoFallback ::
  ScopeContext p ->
  IntSet.IntSet ->
  IntMap.IntMap ElabType ->
  [NodeId] ->
  Either ElabError Instantiation
expInstantiateArgsToInstNoFallback scopeContext namedSet frozenEndpointTypes args = do
  tys <- mapM reifyArg args
  instAppsFromTypes scopeContext tys
  where
    presolutionView = scPresolutionView scopeContext
    canonical = pvCanonical presolutionView
    reifyArg arg =
      case IntMap.lookup (getNodeId arg) frozenEndpointTypes of
        Just exactEndpoint -> pure exactEndpoint
        Nothing ->
          let argC = canonical arg
              readModel = scReadModel scopeContext
           in reifyTypeWithNamedSetRefsNoFallbackReadModel readModel IntMap.empty namedSet argC

instAppsFromTypes :: ScopeContext p -> [ElabType] -> Either ElabError Instantiation
instAppsFromTypes scopeContext tys =
  let tys' = map (inlineBoundVarsTypeWithContext (scInlineBoundVarsContext scopeContext)) tys
   in if null tys'
        then Right InstId
        else Right $ foldr1 InstSeq (map InstApp tys')

sourceAnnIsPolymorphicResolved :: (IdDetails -> Maybe SchemeInfo) -> AnnExpr -> Bool
sourceAnnIsPolymorphicResolved resolvedLookup sourceAnn =
  case sourceAnn of
    AResolvedVar details _ _ ->
      case resolvedLookup details of
        Just schemeInfo -> not (null (schemeBinderRefs (siScheme schemeInfo)))
        _ -> False
    AAnn inner _ _ -> sourceAnnIsPolymorphicResolved resolvedLookup inner
    ALetScope inner _ _ -> sourceAnnIsPolymorphicResolved resolvedLookup inner
    AUnfold inner _ _ -> sourceAnnIsPolymorphicResolved resolvedLookup inner
    _ -> False

sourceAnnSchemeInfoResolved :: (IdDetails -> Maybe SchemeInfo) -> AnnExpr -> Maybe SchemeInfo
sourceAnnSchemeInfoResolved resolvedLookup sourceAnn =
  case sourceAnn of
    AResolvedVar details _ _ -> resolvedLookup details
    AAnn inner _ _ -> sourceAnnSchemeInfoResolved resolvedLookup inner
    ALetScope inner _ _ -> sourceAnnSchemeInfoResolved resolvedLookup inner
    AUnfold inner _ _ -> sourceAnnSchemeInfoResolved resolvedLookup inner
    _ -> Nothing

-- | Semantic key for a binder represented in the annotated source tree.
-- Every annotated binder already carries its resolved identity.
annBinderKey :: IdDetails -> BindingKey
annBinderKey details =
  ResolvedBindingKey (idDetailsIdentityKey details)

-- | Semantic key for a direct annotated variable occurrence.
annExprReferenceKey :: AnnExpr -> Maybe BindingKey
annExprReferenceKey annExpr =
  case annExpr of
    AResolvedVar details _ _ -> Just (ResolvedBindingKey (idDetailsIdentityKey details))
    AAnn inner _ _ -> annExprReferenceKey inner
    ALetScope inner _ _ -> annExprReferenceKey inner
    AUnfold inner _ _ -> annExprReferenceKey inner
    _ -> Nothing

desugaredAnnLambdaInfo :: IdDetails -> AnnExpr -> Maybe (IdDetails, NodeId, EdgeId, AnnExpr)
desugaredAnnLambdaInfo mbParamDetails bodyAnn =
  case bodyAnn of
    ALet _letName mediatorDetails _ _ _ _ rhsAnn innerBodyAnn _ ->
      case rhsAnn of
        AAnn rhsInner annNodeId eid
          | annRefersToVar paramKey rhsInner ->
              Just (mediatorDetails, annNodeId, eid, innerBodyAnn)
        _ -> Nothing
    _ -> Nothing
  where
    paramKey = annBinderKey mbParamDetails

-- | Recover the scheme that the source expression constructs before an
-- annotation edge is applied.  Compound applications are composed from the
-- function scheme and their own edge computation because the annotation has
-- already constrained the application's graph result node to the target type.
-- Keeping this as source construction data lets both annotation closure and
-- witness translation use the same paper-facing scheme.
schemeInfoForInstantiation ::
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  AnnExpr ->
  Either ElabError (Maybe SchemeInfo)
schemeInfoForInstantiation =
  schemeInfoForInstantiationAt PacketLocalSchemeBoundary

data InstantiationSchemeBoundary
  = PacketLocalSchemeBoundary
  | EnclosingAnnotationSchemeBoundary
  -- The packet whose open quantifiers govern requirement recovery and the
  -- packet that lexically encloses it are intentionally distinct.  A nested
  -- topology packet uses the current authority for S'(operated), but its
  -- frozen consumer is placed through the enclosing authority's binder route.
  | ActiveConstructionSchemeBoundary
      { activeConstructionCurrentConsumer
          :: Maybe SubtermConsumerAuthority,
        activeConstructionEnclosingConsumer
          :: Maybe SubtermConsumerAuthority
      }

schemeInfoForAnnotationSource ::
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  AnnExpr ->
  Either ElabError (Maybe SchemeInfo)
schemeInfoForAnnotationSource =
  schemeInfoForInstantiationAt EnclosingAnnotationSchemeBoundary

-- | Recover an edge producer scheme at a construction boundary.  Only the
-- active packet has its local quantifiers open; all other matching packets
-- are observed through their closed enclosing scheme.
sourceSchemeInfoForConstruction
  :: Maybe SubtermConsumerAuthority
  -> Maybe SubtermConsumerAuthority
  -> AnnotationContext p
  -> IntSet.IntSet
  -> (IdDetails -> Maybe SchemeInfo)
  -> AnnExpr
  -> Either ElabError (Maybe SchemeInfo)
sourceSchemeInfoForConstruction currentConsumer enclosingConsumer =
  schemeInfoForInstantiationAt
    ActiveConstructionSchemeBoundary
      { activeConstructionCurrentConsumer = currentConsumer,
        activeConstructionEnclosingConsumer = enclosingConsumer
      }

schemeInfoForInstantiationAt ::
  InstantiationSchemeBoundary ->
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  AnnExpr ->
  Either ElabError (Maybe SchemeInfo)
schemeInfoForInstantiationAt boundary annotationContext namedSetReify resolvedLookup =
  go []
  where
    scopeContext = acScopeContext annotationContext
    subtermGeneralizations = acSubtermGeneralizations annotationContext

    go ambientBinderRefs annExpr = do
      synthetic <- syntheticLetSchemeInfo annExpr
      case synthetic of
        Just schemeInfo -> pure (Just schemeInfo)
        Nothing ->
          case annExpr of
            AResolvedVar details _ _ -> pure (resolvedLookup details)
            AExactAnn inner _ _ edgeId ->
              recurseUnderAnnotation ambientBinderRefs edgeId inner
            AAnn inner _ edgeId ->
              recurseUnderAnnotation ambientBinderRefs edgeId inner
            ALetScope inner _ _ -> go ambientBinderRefs inner
            AUnfold inner _ _ -> go ambientBinderRefs inner
            ALam {} -> generalizedSchemeInfo ambientBinderRefs annExpr
            AApp sourceFunAnn _ funSite _ _ -> do
              applicationScheme <-
                applicationResultSchemeInfo
                  ambientBinderRefs
                  sourceFunAnn
                  (instantiationSiteEdgeId funSite)
              case applicationScheme of
                Just schemeInfo -> pure (Just schemeInfo)
                Nothing -> generalizedSchemeInfo ambientBinderRefs annExpr
            _ -> generalizedSchemeInfo ambientBinderRefs annExpr

    recurseUnderAnnotation ambientBinderRefs edgeId inner =
      go
        ( distinctBinderRefs
            ( ambientBinderRefs
                ++ annotationAmbientBinderRefs edgeId
            )
        )
        inner

    annotationAmbientBinderRefs edgeId =
      case annotationExpectedTypeForEdge annotationContext edgeId of
        Nothing -> []
        Just expectedType ->
          let sourceRefs =
                distinctBinderRefs
                  ( typeBinderDeclarationRefs expectedType
                      ++ freeTypeVarRefsType expectedType
                  )
              graphRefs =
                [ typeBinderRefFromIdentity
                    (typeBinderIdentityFromNode (NodeId nodeKey))
                    ("t" ++ show nodeKey)
                | (nodeKey, sourceRef) <-
                    IntMap.toList (acSourceBinderRefs annotationContext)
                , any (typeBinderRefsSameIdentity sourceRef) sourceRefs
                ]
           in distinctBinderRefs (sourceRefs ++ graphRefs)

    distinctBinderRefs = foldr insertDistinctBinderRef []
    insertDistinctBinderRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    applicationResultSchemeInfo ambientBinderRefs sourceFunAnn funEid = do
      mFunScheme <-
        go ambientBinderRefs sourceFunAnn
      case mFunScheme of
        Nothing -> pure Nothing
        Just funScheme -> do
          funInst <-
            case
                reifyInst
                  annotationContext
                  namedSetReify
                  resolvedLookup
                  sourceFunAnn
                  funEid
              of
                Right inst -> pure inst
                Left cause ->
                  Left
                    ( ValidationFailed
                        [ "application source-scheme recovery failed during edge replay"
                        , "  boundary: " ++ showInstantiationSchemeBoundary boundary
                        , "  edge: " ++ show funEid
                        , "  function: " ++ show sourceFunAnn
                        , "  cause: " ++ show cause
                        ]
                    )
          pure $ do
            instantiatedFun <-
              either
                (const Nothing)
                Just
                (applyInstantiation (schemeToType (siScheme funScheme)) funInst)
            applicationFun <- exposeApplicationArrow instantiatedFun
            case applicationFun of
              TArrow _ resultTy ->
                Just (schemeInfoFromRefSubst (schemeFromType resultTy) IntMap.empty)
              _ -> Nothing

    exposeApplicationArrow ty =
      case ty of
        TForallRef _ (Just _) _ -> do
          eliminated <- either (const Nothing) Just (applyInstantiation ty InstElim)
          exposeApplicationArrow eliminated
        _ -> Just ty

    showInstantiationSchemeBoundary schemeBoundary =
      case schemeBoundary of
        PacketLocalSchemeBoundary -> "packet-local"
        EnclosingAnnotationSchemeBoundary -> "enclosing-annotation"
        ActiveConstructionSchemeBoundary currentConsumer enclosingConsumer ->
          "active-construction current="
            ++ show currentConsumer
            ++ " enclosing="
            ++ show enclosingConsumer

    generalizedSchemeInfo ambientBinderRefs sourceAnn = do
      let ownedPackets =
            subtermGeneralizationsOwnedBy sourceAnn subtermGeneralizations
          constructionRouteNodes =
            gaConstructionRouteNodes
              (scCanonical scopeContext)
              (scGaParents scopeContext)
          edgeArtifacts =
            EdgeArtifacts
              { eaEdgeExpansions = acEdgeExpansions annotationContext,
                eaEdgeWitnesses = acEdgeWitnesses annotationContext,
                eaEdgeTraces = acEdgeTraces annotationContext,
                eaIdentityEdges = acIdentityEdges annotationContext
              }
      generalizedResultRequest <-
        sourceLambdaGeneralizedResultRouteRequest
          (scGaParents scopeContext)
          sourceAnn
          ownedPackets
      let runGeneralization requirements mbGa scope target =
            case generalizedResultRequest of
              Nothing -> do
                (scheme, subst) <-
                  scGeneralizeAtWithRequirements scopeContext
                    requirements
                    mbGa
                    scope
                    target
                pure (scheme, subst, Nothing)
              Just request -> do
                (scheme, subst, certificate) <-
                  scGeneralizeAtWithResultCertificate scopeContext
                    request
                    requirements
                    mbGa
                    scope
                    target
                pure
                  ( scheme,
                    subst,
                    Just certificate
                  )
      mRootAuthority <-
        rootRaiseMergeAuthorityForExpression edgeArtifacts sourceAnn
      (requirements, generalizationResult) <-
        case mRootAuthority of
          Nothing ->
            let requirements =
                  GeneralizationRequirements
                  { grRequiredGammaBinders = [],
                    grSourceBinderRefs = acSourceBinderRefs annotationContext,
                    grAmbientBinderRefs = ambientBinderRefs,
                    grAmbientGammaAuthorities = IntMap.empty,
                    grLocallyClosedGammaNodes = mempty
                  }
                sourceNode = annNode sourceAnn
                target =
                  generalizeTargetNode
                    (scPresolutionView scopeContext)
                    (scCanonical scopeContext sourceNode)
             in do
                  scope <- scopeRootForNode scopeContext sourceNode
                  pure
                    ( requirements,
                      runGeneralization
                        requirements
                        (Just (scGaParents scopeContext))
                        scope
                        target
                    )
          Just (rootEdge, _) -> do
            gammaOwner <-
              rootGammaOwner sourceAnn rootEdge ownedPackets
            requirements0 <-
              rootRequirements edgeArtifacts ownedPackets rootEdge
            let requirements =
                  requirements0
                    { grAmbientBinderRefs =
                        distinctBinderRefs
                          ( ambientBinderRefs
                              ++ grAmbientBinderRefs requirements0
                          )
                    }
                sourceNode = annNode sourceAnn
                target =
                  generalizeTargetNode
                    (scPresolutionView scopeContext)
                    (scCanonical scopeContext sourceNode)
            pure
              ( requirements,
                runGeneralization
                  requirements
                  (Just (scGaParents scopeContext))
                  (genRef gammaOwner)
                  target
              )
      case generalizationResult of
        Right (scheme, subst, generalizedResultRoute) -> do
          let normalized = schemeInfoFromRefSubst scheme subst
          prepared <-
            case
                prepareRootRaiseMergeScheme
                  edgeArtifacts
                  sourceAnn
                  requirements
                  normalized
              of
                Right result -> pure result
                Left err ->
                  Left
                    ( ValidationFailed
                        [ "annotation source scheme failed root RaiseMerge validation"
                        , "  source: " ++ show sourceAnn
                        , "  generalized: " ++ show normalized
                        , "  owned packets: " ++ show ownedPackets
                        , "  cause: " ++ show err
                        ]
                    )
          preparedWithSourceTopologyRoute <-
            publishSourceLambdaTopologyConsumerRoute
              generalizedResultRoute
              constructionRouteNodes
              sourceAnn
              ownedPackets
              prepared
          preparedWithConsumerRoutes <-
            publishTopologyConsumerRoutes
              constructionRouteNodes
              ownedPackets
              preparedWithSourceTopologyRoute
          placed <-
            case
                placeSubtermGeneralizationBindersWithRoutes
                  (siSubstRefs preparedWithConsumerRoutes)
                  ownedPackets
                  (siScheme preparedWithConsumerRoutes)
              of
                Right placedScheme -> pure placedScheme
                Left cause ->
                  Left
                    ( ValidationFailed
                        [ "annotation source topology packet placement failed"
                        , "  boundary: "
                            ++ showInstantiationSchemeBoundary boundary
                        , "  active enclosing consumer: "
                            ++ show (activeEnclosingConsumer boundary)
                        , "  prepared binders: "
                            ++ show
                              (schemeBinderRefs (siScheme preparedWithConsumerRoutes))
                        , "  prepared routes: "
                            ++ show (siSubstRefs preparedWithConsumerRoutes)
                        , "  packet consumer route hops: "
                            ++ show
                              ( packetConsumerRouteHops
                                  constructionRouteNodes
                                  ownedPackets
                              )
                        , "  owned packets: " ++ show ownedPackets
                        , "  cause: " ++ show cause
                        ]
                    )
          let placedInfo =
                schemeInfoFromRefSubst
                  placed
                  (siSubstRefs preparedWithConsumerRoutes)
          publishedInfo <-
            case (boundary, mRootAuthority) of
              ( ActiveConstructionSchemeBoundary {}
                , Just (rootEdge, _)
                ) ->
                  case
                      [ packet
                      | packet <- Map.elems ownedPackets
                      , Just authority <-
                          [subtermGeneralizationGammaAuthority packet]
                      , gpaEdgeId authority == rootEdge
                      ]
                    of
                    [] -> pure placedInfo
                    [packet] ->
                      publishSubtermGammaConstructionSourceSchemeInfo
                        rootEdge
                        packet
                        placedInfo
                    packets ->
                      Left
                        ( ValidationFailed
                            [ "construction source has multiple Gamma packet publishers"
                            , "  edge: " ++ show rootEdge
                            , "  packets: " ++ show packets
                            ]
                        )
              _ -> pure placedInfo
          pure (Just publishedInfo)
        Left _ -> pure Nothing
      where
        rootGammaOwner ann edgeId ownedPackets =
          case
              [ authority
              | packet <- Map.elems ownedPackets
              , Just authority <- [subtermGeneralizationGammaAuthority packet]
              , gpaEdgeId authority == edgeId
              ]
          of
            [authority] -> pure (gpaOwnerGen authority)
            [] ->
              case directLambdaBodyGammaOwner edgeId ann of
                Just owner -> pure owner
                Nothing ->
                  Left
                    ( ValidationFailed
                        [ "annotation root RaiseMerge has no prepared Gamma owner"
                        , "  edge: " ++ show edgeId
                        , "  packet owners: " ++ show (Map.keys ownedPackets)
                        ]
                    )
            authorities ->
              Left
                ( ValidationFailed
                    [ "annotation root RaiseMerge has multiple prepared Gamma owners"
                    , "  edge: " ++ show edgeId
                    , "  authorities: " ++ show authorities
                    ]
                )

        -- An ordinary lambda constructs its body-edge Gamma at its recorded
        -- lexical scope.  Once that construction is owned directly by ALam,
        -- annotation-source recovery must not require a duplicate prepared
        -- subterm packet for the same edge.
        directLambdaBodyGammaOwner edgeId ann =
          case ann of
            ALam _ _ _ owner _ bodyEdge _
              | bodyEdge == edgeId -> Just owner
            AAnn inner _ _ -> directLambdaBodyGammaOwner edgeId inner
            ALetScope inner _ _ -> directLambdaBodyGammaOwner edgeId inner
            _ -> Nothing

        rootRequirements edgeArtifacts ownedPackets rootEdge =
          -- The shared requirement owner reifies S'(operated) from the frozen
          -- operated source root.  Reifying the edge-local result here would
          -- turn a freshly introduced exterior into bottom before the binder
          -- planner has constructed its required Gamma entry.
          requirementBuilder
            (scopeTypeBinderIdentityRepresentative scopeContext)
            (scCanonical scopeContext)
            (scGaParents scopeContext)
            (scPresolutionView scopeContext)
            edgeArtifacts
            (acSourceBinderRefs annotationContext)
            ownedPackets
            [(rootEdge, Nothing)]
          where
            requirementBuilder =
              case boundary of
                PacketLocalSchemeBoundary ->
                  generalizationRequirementsForRootEdges
                EnclosingAnnotationSchemeBoundary ->
                  generalizationRequirementsForEnclosingRootEdges
                ActiveConstructionSchemeBoundary
                  { activeConstructionCurrentConsumer = currentConsumer
                  } ->
                  generalizationRequirementsForRootEdgesInConstruction
                    (scaConsumerIdentity <$> currentConsumer)
                    ambientBinderRefs
                    IntMap.empty

        -- The construction caller supplies the active packet's opaque
        -- consumer authority, not an independently paired owner and identity.
        -- Retaining that certificate across source-scheme recovery lets a
        -- descendant topology packet publish its frozen consumer through the
        -- enclosing packet's exact consumer route without reconstructing
        -- ownership from binder shape, name, or graph coincidence.
        activeEnclosingConsumer schemeBoundary =
          case schemeBoundary of
            ActiveConstructionSchemeBoundary
              { activeConstructionEnclosingConsumer = Just authority
              } -> do
              owner <- subtermConsumerAuthorityEnclosingOwner authority
              pure (owner, scaConsumerIdentity authority)
            _ -> Nothing

        packetConsumerRouteHops constructionRouteNodes packets =
          [ ( consumerIdentity
            , packetConsumerRef
            , maybe
                []
                constructionRouteNodes
                (typeBinderRefNode packetConsumerRef)
            )
          | packet <- Map.elems packets
          , Just authority <-
              [subtermGeneralizationConsumerAuthority packet]
          , let consumerIdentity = scaConsumerIdentity authority
          , Just consumerNode <- [typeBinderIdentityNode consumerIdentity]
          , Just packetConsumerRef <-
              [ IntMap.lookup
                  (getNodeId consumerNode)
                  ( siSubstRefs
                      (subtermGeneralizationSchemeInfo packet)
                  )
              ]
          ]

    syntheticLetSchemeInfo sourceAnn =
      case sourceAnn of
        ALet _letName mbDetails _ schemeRootId _ _ rhsAnn bodyAnn _
          | annRefersToVar (annBinderKey mbDetails) bodyAnn ->
              firstJustE
                (explicitSourceAnnotatedScheme rhsAnn)
                ( firstJustE
                    (explicitSourceAnnotatedScheme sourceAnn)
                    ( pure
                        ( case generalizeAtNode scopeContext schemeRootId of
                            Right (scheme, subst) -> Just (schemeInfoFromRefSubst scheme subst)
                            Left _ -> Nothing
                        )
                    )
                )
        AAnn inner _ _ -> syntheticLetSchemeInfo inner
        ALetScope inner _ _ -> syntheticLetSchemeInfo inner
        AUnfold inner _ _ -> syntheticLetSchemeInfo inner
        _ -> pure Nothing

    explicitSourceAnnotatedScheme sourceAnn =
      case sourceAnn of
        AAnn inner _ edgeId ->
          case annotationExpectedTypeForEdge annotationContext edgeId of
            Just expectedTy ->
              pure
                ( Just
                    ( schemeInfoFromRefSubst
                        (schemeFromType expectedTy)
                        IntMap.empty
                    )
                )
            Nothing -> explicitSourceAnnotatedScheme inner
        ALetScope inner _ _ -> explicitSourceAnnotatedScheme inner
        ALam _ _ _ _ body _ _ -> explicitSourceAnnotatedScheme body
        AApp fun arg _ _ _ ->
          firstJustE (explicitSourceAnnotatedScheme fun) (explicitSourceAnnotatedScheme arg)
        ALet _ _ _ _ _ _ rhs body _ ->
          firstJustE (explicitSourceAnnotatedScheme rhs) (explicitSourceAnnotatedScheme body)
        AUnfold inner _ _ -> explicitSourceAnnotatedScheme inner
        _ -> pure Nothing

    firstJustE left right = do
      result <- left
      case result of
        Just _ -> pure result
        Nothing -> right

-- | Select the surface computation owned by an annotation boundary.  A
-- producer boundary retains Omega's primitive bounded elimination; an
-- application argument publishes the equivalent explicit bound application
-- required by Figure 15.3.5.  The same role is consumed by source and
-- compiler-exact annotations so transparent annotation layers cannot erase
-- the occurrence distinction.
data AnnotationBoundaryRole
  = AnnotationProducerBoundary
  | AnnotationApplicationArgumentBoundary

-- | Build the annotation computation from the source and target schemes when
-- they share a leading forall.  The shared binder is preserved with HYP while
-- the remaining source scheme is instantiated underneath it.  This is the
-- recursive form needed to specialize a prepared K-like bound without
-- eliminating the annotation's own outer quantifier.
inferPreservingAnnotationInst :: ElabType -> ElabType -> Maybe Instantiation
inferPreservingAnnotationInst =
  inferPreservingAnnotationInstFor AnnotationProducerBoundary

inferPreservingAnnotationInstFor ::
  AnnotationBoundaryRole ->
  ElabType ->
  ElabType ->
  Maybe Instantiation
inferPreservingAnnotationInstFor boundaryRole sourceTy targetTy
  | alphaEqType sourceTy targetTy = Just InstId
  | otherwise =
      case (sourceTy, targetTy) of
        (_, TForallRef targetRef Nothing targetBody)
          | not
              ( any
                  (typeBinderRefsSameIdentity targetRef)
                  (freeTypeVarRefsType targetBody)
              ) -> do
              -- Figure 15.3.5 permits the source annotation to publish a
              -- vacuous quantifier after completing the producer's existing
              -- computation.  Recurse first so a result-carrier elimination
              -- remains under its retained source binder; introducing the
              -- target forall first would instead move that elimination to
              -- the new outer binder's bound.
              inner <-
                inferPreservingAnnotationInstFor
                  boundaryRole
                  sourceTy
                  targetBody
              let candidate = composeInst inner InstIntro
              applied <- either (const Nothing) Just (applyInstantiation sourceTy candidate)
              if alphaEqType applied targetTy
                then Just candidate
                else Nothing
        (TForallRef sourceRef sourceBound sourceBody, TForallRef targetRef targetBound targetBody)
          | Just boundInst <-
              inferPreservingAnnotationInst
                (maybe TBottom tyToElab sourceBound)
                (maybe TBottom tyToElab targetBound) -> do
              let targetBody' =
                    substTypeCaptureRef targetRef (TVarRef sourceRef) targetBody
              bodyInst <-
                inferPreservingAnnotationInstFor
                  boundaryRole
                  sourceBody
                  targetBody'
              let boundStep =
                    case boundInst of
                      InstId -> InstId
                      _ -> InstInside boundInst
                  bodyStep =
                    case bodyInst of
                      InstId -> InstId
                      _ -> instUnderWithRef sourceRef bodyInst
                  candidate =
                    composeInst
                      boundStep
                      bodyStep
              applied <- either (const Nothing) Just (applyInstantiation sourceTy candidate)
              if alphaEqType applied targetTy
                then Just candidate
                else Nothing
        (TForallRef sourceRef _ sourceBody, _)
          | not
              ( any
                  (typeBinderRefsSameIdentity sourceRef)
                  (freeTypeVarRefsType sourceBody)
              ) -> do
              inner <-
                inferPreservingAnnotationInstFor
                  boundaryRole
                  sourceBody
                  targetTy
              let elimination = boundMatchingElimination sourceTy
                  candidate =
                    case inner of
                      InstId -> elimination
                      _ -> InstSeq elimination inner
              applied <- either (const Nothing) Just (applyInstantiation sourceTy candidate)
              if alphaEqType applied targetTy
                then Just candidate
                else Nothing
        _ ->
          inferredTypeApplication
            <|> canonicalBoundElimination sourceTy targetTy
  where
    inferredTypeApplication =
      inferredTypeApplicationFrom sourceTy targetTy

    inferredTypeApplicationFrom currentTy expectedTy = do
      let sourceScheme = schemeFromType currentTy
      args <-
        inferInstAppArgsFromSchemeRefs
          (schemeBinderRefs sourceScheme)
          (schemeBody sourceScheme)
          expectedTy
      candidate <- instForTypeArguments currentTy args
      applied <- either (const Nothing) Just (applyInstantiation currentTy candidate)
      if alphaEqType applied expectedTy
        then Just candidate
        else Nothing

    -- Bounded forall elimination uses the binder's known bounds.  A producer
    -- keeps the canonical N,N construction, while an application argument
    -- publishes the equivalent explicit bound applications.  Inference can
    -- determine only a proper prefix of the source arguments when a later
    -- binder is vacuous in the result, so continue until the complete
    -- candidate reaches the annotation target exactly.
    canonicalBoundElimination initialTy expectedTy =
      go initialTy InstId
      where
        go currentTy candidate
          | alphaEqType currentTy expectedTy = Just candidate
          | Just inferred <-
              inferredTypeApplicationFrom currentTy expectedTy =
              Just (composeInst candidate inferred)
          | otherwise =
              case currentTy of
                TForallRef _ (Just _) _ -> do
                  let elimination = boundMatchingElimination currentTy
                  nextTy <-
                    either
                      (const Nothing)
                      Just
                      (applyInstantiation currentTy elimination)
                  go nextTy (appendElimination candidate elimination)
                _ -> Nothing

        appendElimination InstId elimination = elimination
        appendElimination candidate elimination =
          InstSeq candidate elimination

    instForTypeArguments _ [] = Just InstId
    instForTypeArguments currentTy (arg : rest) = do
      step <- instForLeadingArgument currentTy arg
      nextTy <- either (const Nothing) Just (applyInstantiation currentTy step)
      remaining <- instForTypeArguments nextTy rest
      pure $
        case remaining of
          InstId -> step
          _ -> InstSeq step remaining

    instForLeadingArgument currentTy arg =
      case currentTy of
        TForallRef _ (Just bound) _
          | let boundTy = tyToElab bound ->
              if alphaEqType arg boundTy
                then Just (boundMatchingElimination currentTy)
                else do
                  boundInst <- inferPreservingAnnotationInst boundTy arg
                  pure (InstSeq (InstInside boundInst) InstElim)
        _ -> Just (instForLeadingTypeArgument currentTy arg)

    boundMatchingElimination currentTy =
      case (boundaryRole, currentTy) of
        (AnnotationApplicationArgumentBoundary, TForallRef _ (Just bound) _) ->
          InstApp (tyToElab bound)
        _ -> InstElim

-- | Elaborate a nested compiler-owned exact annotation.  The producer is a
-- proper subterm here, so construct its own Figure 15.3.5 abstractions before
-- applying the compiler-owned exact specialization.  A root exact annotation
-- is handled after root closure by 'elaborateClosedExactAnnotationTerm'.
elaborateExactAnnotationTerm ::
  AnnotationBoundaryRole ->
  AnnotationContext p ->
  TypeCheck.Env ->
  ElabType ->
  IntMap.IntMap TypeBinderRef ->
  NodeId ->
  EdgeId ->
  XmlfTerm ->
  Either ElabError XmlfTerm
elaborateExactAnnotationTerm boundaryRole annotationContext tcEnv expectedType edgeRefs _annNodeId eid expr' = do
  case annotationExpectedTypeForEdge annotationContext eid of
    Just _ -> pure ()
    Nothing ->
      Left
        ( ValidationFailed
            ["missing source type for exact annotation " ++ show eid]
        )
  sourceBinderRefs <-
    foldM
      insertEdgeRef
      (acSourceBinderRefs annotationContext)
      (IntMap.toList edgeRefs)
  let representative =
        scopeTypeBinderIdentityRepresentative (acScopeContext annotationContext)
      sourceAliasSubst =
        sourceBinderAliasSubstitution
          representative
          sourceBinderRefs
          (Reduce.freeTypeVarRefsTerm expr')
      sourceTerm =
        freshenTermTypeAbsAgainstEnv tcEnv
          (substInTermRefs sourceAliasSubst expr')
      expectedTy = openCompilerExactTypeAgainstEnv tcEnv expectedType
  elaborateClosedExactAnnotationTermAtTypeFor
    boundaryRole
    tcEnv
    expectedTy
    eid
    sourceTerm
  where
    insertEdgeRef refs (nodeKey, outwardRef) =
      case IntMap.lookup nodeKey refs of
        Nothing -> pure (IntMap.insert nodeKey outwardRef refs)
        Just existingRef
          | typeBinderRefsSameIdentity existingRef outwardRef -> pure refs
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "compiler exact edge route conflicts with a source binder"
                    , "  edge: " ++ show eid
                    , "  graph node: " ++ show (NodeId nodeKey)
                    , "  source binder: " ++ show existingRef
                    , "  exact binder: " ++ show outwardRef
                    ]
                )

-- | A nested compiler exact annotation can repeat a source forall whose
-- semantic binder is already owned by the surrounding lexical Gamma.  At
-- that boundary the paper construction is open in the existing binder; it
-- must not introduce a second type abstraction with the same identity.
openCompilerExactTypeAgainstEnv :: TypeCheck.Env -> ElabType -> ElabType
openCompilerExactTypeAgainstEnv tcEnv = go
  where
    lexicalRefs =
      Map.keys (TypeCheck.typeEnv tcEnv)
        ++ concatMap
          freeTypeVarRefsType
          ( Map.elems (TypeCheck.typeEnv tcEnv)
              ++ map snd
                ( TypeCheck.resolvedTermEnvEntries
                    (TypeCheck.resolvedTermEnv tcEnv)
                )
          )

    go ty =
      case ty of
        TForallRef ref _ body
          | any
              (typeBinderRefsSameIdentity ref)
              lexicalRefs ->
              go body
        _ -> ty

-- | Apply compiler-owned exact authority to a producer whose paper
-- abstractions have already been constructed.  Both source and target types
-- are supplied by construction; the two typechecks below only validate those
-- choices.
elaborateClosedExactAnnotationTerm ::
  TypeCheck.Env ->
  ResolvedSrcType ->
  EdgeId ->
  XmlfTerm ->
  Either ElabError XmlfTerm
elaborateClosedExactAnnotationTerm tcEnv exactSourceType eid sourceTerm = do
  expectedTy <-
    either (Left . InstantiationError) Right
      (resolvedSourceTypeToElabType exactSourceType)
  elaborateClosedExactAnnotationTermAtType tcEnv expectedTy eid sourceTerm

elaborateClosedExactAnnotationTermAtType ::
  TypeCheck.Env ->
  ElabType ->
  EdgeId ->
  XmlfTerm ->
  Either ElabError XmlfTerm
elaborateClosedExactAnnotationTermAtType =
  elaborateClosedExactAnnotationTermAtTypeFor AnnotationProducerBoundary

elaborateClosedExactAnnotationTermAtTypeFor ::
  AnnotationBoundaryRole ->
  TypeCheck.Env ->
  ElabType ->
  EdgeId ->
  XmlfTerm ->
  Either ElabError XmlfTerm
elaborateClosedExactAnnotationTermAtTypeFor boundaryRole tcEnv expectedTy eid sourceTerm = do
  sourceActual <-
    case TypeCheck.typeCheckWithEnv tcEnv sourceTerm of
      Left err ->
        exactFailure
          "producer construction is not typable"
          expectedTy
          ( show err
              ++ "; colliding abstractions="
              ++ show
                [ ref
                | ref <- typeAbstractionRefs sourceTerm
                , any (typeBinderRefsSameIdentity ref) envFreeRefs
                ]
              ++ "; environment free refs="
              ++ show envFreeRefs
              ++ "; term="
              ++ show sourceTerm
          )
      Right ty -> pure ty
  constructed <-
    if alphaEqType sourceActual expectedTy
      then
        alignExactRecursivePresentations
          eid
          sourceActual
          expectedTy
          sourceTerm
      else
        if churchAwareEqType sourceActual expectedTy
          then pure sourceTerm
          else
            constructExactAnnotationTermFor
              boundaryRole
              tcEnv
              sourceActual
              expectedTy
              sourceTerm
  case TypeCheck.typeCheckWithEnv tcEnv constructed of
    Left err -> exactFailure "specialized construction is not typable" expectedTy (show err)
    Right actualTy
      | alphaEqType actualTy expectedTy
          || churchAwareEqType actualTy expectedTy
          || implicitForallClosureMatches expectedTy actualTy ->
          pure constructed
      | otherwise ->
          exactFailure
            "specialized construction disagrees with exact type"
            expectedTy
            ( show actualTy
                ++ "; source-type="
                ++ show sourceActual
                ++ "; source-construction="
                ++ exactTermConstructionSummary sourceTerm
                ++ "; specialized-construction="
                ++ exactTermConstructionSummary constructed
            )
  where
    exactFailure :: String -> ElabType -> String -> Either ElabError a
    exactFailure label expected actual =
      Left
        ( PhiInvariantError
            ( "compiler exact annotation "
                ++ label
                ++ " for edge "
                ++ show eid
                ++ "; expected="
                ++ show expected
                ++ "; actual="
                ++ actual
            )
        )

    envFreeRefs =
      foldr
        unionRefs
        []
        ( Map.elems (TypeCheck.typeEnv tcEnv)
            ++ map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv tcEnv))
        )

    unionRefs ty refs =
      foldr
        (\ref acc ->
          if any (typeBinderRefsSameIdentity ref) acc
            then acc
            else ref : acc
        )
        refs
        (freeTypeVarRefsType ty)

    typeAbstractionRefs term =
      case term of
        EVarNode{} -> []
        ELit{} -> []
        ELam _ body -> typeAbstractionRefs body
        EApp fun arg -> typeAbstractionRefs fun ++ typeAbstractionRefs arg
        ELet _ _ rhs body -> typeAbstractionRefs rhs ++ typeAbstractionRefs body
        ETyAbsRef ref _ body -> ref : typeAbstractionRefs body
        ETyInst body _ -> typeAbstractionRefs body
        ERoll _ body -> typeAbstractionRefs body
        EUnroll body -> typeAbstractionRefs body

    exactTermConstructionSummary term =
      case term of
        EVarNode resolved ->
          "Var(" ++ show (resolvedVarType resolved) ++ ")"
        ELit{} -> "Lit"
        ELam resolved body ->
          "Lam("
            ++ show (resolvedVarType resolved)
            ++ ","
            ++ exactTermConstructionSummary body
            ++ ")"
        EApp fun arg ->
          "App("
            ++ exactTermConstructionSummary fun
            ++ ","
            ++ exactTermConstructionSummary arg
            ++ ")"
        ELet _ scheme rhs body ->
          "Let("
            ++ show (schemeToType scheme)
            ++ ","
            ++ exactTermConstructionSummary rhs
            ++ ","
            ++ exactTermConstructionSummary body
            ++ ")"
        ETyAbsRef ref mbBound body ->
          "TyAbs("
            ++ show (ref, mbBound)
            ++ ","
            ++ exactTermConstructionSummary body
            ++ ")"
        ETyInst inner inst ->
          "TyInst("
            ++ exactTermConstructionSummary inner
            ++ ","
            ++ show inst
            ++ ")"
        ERoll ty body ->
          "Roll("
            ++ show ty
            ++ ","
            ++ exactTermConstructionSummary body
            ++ ")"
        EUnroll body ->
          "Unroll(" ++ exactTermConstructionSummary body ++ ")"

-- | An exact boundary owns the source-facing presentation of its result.  The
-- graph pipeline can construct an alpha-equivalent recursive type whose local
-- @mu@ binder still carries a graph identity.  Leaving that presentation in
-- the completed term loses the data owner needed by later structural lowering.
--
-- This is not shape-based owner recovery: the exact source type and the
-- independently checked producer type first have to be alpha-equivalent.  We
-- then replace only the precise recursive subtrees found at corresponding
-- positions, and reject one producer presentation being routed to two source
-- presentations.  Explicit roll/unroll structure is retained because both
-- sides remain recursive types.
alignExactRecursivePresentations
  :: EdgeId
  -> ElabType
  -> ElabType
  -> XmlfTerm
  -> Either ElabError XmlfTerm
alignExactRecursivePresentations eid sourceTy targetTy term = do
  rewrites <-
    either
      ( \detail ->
          Left
            ( PhiInvariantError
                ( "compiler exact annotation has ambiguous recursive presentation for edge "
                    ++ show eid
                    ++ "; "
                    ++ detail
                )
            )
      )
      Right
      (collectRecursivePresentationRewrites sourceTy targetTy)
  pure (mapTermTypes (rewriteType rewrites) term)
  where
    rewriteType rewrites ty =
      case lookupExactType ty rewrites of
        Just replacement -> replacement
        Nothing ->
          case ty of
            TVarRef ref -> TVarRef ref
            TArrow domain codomain ->
              TArrow
                (rewriteType rewrites domain)
                (rewriteType rewrites codomain)
            TConWithIdentity identity con args ->
              TConWithIdentity identity con (fmap (rewriteType rewrites) args)
            TVarAppRef ref args ->
              TVarAppRef ref (fmap (rewriteType rewrites) args)
            TBaseWithIdentity identity base ->
              TBaseWithIdentity identity base
            TForallRef ref mbBound body ->
              TForallRef
                ref
                (fmap (mapBoundType (rewriteType rewrites)) mbBound)
                (rewriteType rewrites body)
            TMuRef ref body ->
              TMuRef ref (rewriteType rewrites body)
            TBottom -> TBottom

    lookupExactType _ [] = Nothing
    lookupExactType ty ((source, target) : rest)
      | ty == source = Just target
      | otherwise = lookupExactType ty rest

collectRecursivePresentationRewrites
  :: ElabType
  -> ElabType
  -> Either String [(ElabType, ElabType)]
collectRecursivePresentationRewrites = go []
  where
    go rewrites source target =
      case (source, target) of
        (TMuRef {}, TMuRef {})
          | source /= target
          , alphaEqType source target ->
              addRewrite source target rewrites
        (TArrow sourceDomain sourceCodomain, TArrow targetDomain targetCodomain) -> do
          rewrites' <- go rewrites sourceDomain targetDomain
          go rewrites' sourceCodomain targetCodomain
        (TConWithIdentity sourceIdentity _ sourceArgs, TConWithIdentity targetIdentity _ targetArgs)
          | sourceIdentity == targetIdentity
          , length sourceArgs == length targetArgs ->
              foldM
                (\current (sourceArg, targetArg) -> go current sourceArg targetArg)
                rewrites
                (zip (toList sourceArgs) (toList targetArgs))
        (TVarAppRef sourceRef sourceArgs, TVarAppRef targetRef targetArgs)
          | typeBinderRefsSameIdentity sourceRef targetRef
          , length sourceArgs == length targetArgs ->
              foldM
                (\current (sourceArg, targetArg) -> go current sourceArg targetArg)
                rewrites
                (zip (toList sourceArgs) (toList targetArgs))
        (TForallRef sourceRef sourceBound sourceBody, TForallRef targetRef targetBound targetBody) -> do
          let targetBody' =
                substTypeCaptureRef targetRef (TVarRef sourceRef) targetBody
              targetBound' =
                fmap
                  (mapBoundType (substTypeCaptureRef targetRef (TVarRef sourceRef)))
                  targetBound
          rewrites' <- goBounds rewrites sourceBound targetBound'
          go rewrites' sourceBody targetBody'
        _ -> pure rewrites

    goBounds rewrites Nothing Nothing = pure rewrites
    goBounds rewrites (Just sourceBound) (Just targetBound) =
      go rewrites (tyToElab sourceBound) (tyToElab targetBound)
    goBounds rewrites _ _ = pure rewrites

    addRewrite source target rewrites =
      case lookup source rewrites of
        Nothing -> pure ((source, target) : rewrites)
        Just existing
          | existing == target -> pure rewrites
          | otherwise ->
              Left
                ( "producer recursive type maps to both "
                    ++ show existing
                    ++ " and "
                    ++ show target
                )

    toList (first :| rest) = first : rest

mapTermTypes :: (ElabType -> ElabType) -> XmlfTerm -> XmlfTerm
mapTermTypes rewrite = go
  where
    go term =
      case term of
        EVarNode resolved ->
          EVarNode (mapResolvedVarType rewrite resolved)
        ELit literal -> ELit literal
        ELam resolved body ->
          ELam
            (mapResolvedVarType rewrite resolved)
            (go body)
        EApp fun arg -> EApp (go fun) (go arg)
        ELet resolved scheme rhs body ->
          let scheme' = schemeFromType (rewrite (schemeToType scheme))
           in ELet
                (mapResolvedVarType rewrite resolved)
                scheme'
                (go rhs)
                (go body)
        ETyAbsRef ref mbBound body ->
          ETyAbsRef
            ref
            (fmap (mapBoundType rewrite) mbBound)
            (go body)
        ETyInst inner inst ->
          ETyInst (go inner) (mapInstantiationTypes rewrite inst)
        ERoll ty body -> ERoll (rewrite ty) (go body)
        EUnroll body -> EUnroll (go body)

mapInstantiationTypes
  :: (ElabType -> ElabType)
  -> Instantiation
  -> Instantiation
mapInstantiationTypes rewrite inst =
  case inst of
    InstId -> InstId
    InstApp ty -> InstApp (rewrite ty)
    InstBot ty -> InstBot (rewrite ty)
    InstIntro -> InstIntro
    InstElim -> InstElim
    InstAbstrRef ref -> InstAbstrRef ref
    InstUnderRef ref inner ->
      InstUnderRef ref (mapInstantiationTypes rewrite inner)
    InstInside inner -> InstInside (mapInstantiationTypes rewrite inner)
    InstSeq left right ->
      InstSeq
        (mapInstantiationTypes rewrite left)
        (mapInstantiationTypes rewrite right)

-- | Construct an exact boundary in the producer's lexical order.  A
-- lambda-body generalization stays under that lambda; for example,
-- @lambda e. Lambda b. t@ is specialized inside the lambda and is never
-- flattened into @Lambda b. lambda e. t@.
constructExactAnnotationTerm ::
  TypeCheck.Env ->
  ElabType ->
  ElabType ->
  XmlfTerm ->
  Either ElabError XmlfTerm
constructExactAnnotationTerm =
  constructExactAnnotationTermFor AnnotationProducerBoundary

constructExactAnnotationTermFor ::
  AnnotationBoundaryRole ->
  TypeCheck.Env ->
  ElabType ->
  ElabType ->
  XmlfTerm ->
  Either ElabError XmlfTerm
constructExactAnnotationTermFor boundaryRole =
  constructExactAnnotationTermForWithIntroducedBinders boundaryRole []

constructExactAnnotationTermForWithIntroducedBinders ::
  AnnotationBoundaryRole ->
  [TypeBinderRef] ->
  TypeCheck.Env ->
  ElabType ->
  ElabType ->
  XmlfTerm ->
  Either ElabError XmlfTerm
constructExactAnnotationTermForWithIntroducedBinders boundaryRole introducedTargetRefs tcEnv sourceTy targetTy term
  | alphaEqType sourceTy targetTy || churchAwareEqType sourceTy targetTy =
      Right term
  | otherwise = do
      mbReordered <- exactLeadingBinderReordering sourceTy targetTy
      case mbReordered of
        Just (reordering, reorderedSourceTy) ->
          constructExactAnnotationTermForWithIntroducedBinders
            boundaryRole
            introducedTargetRefs
            tcEnv
            reorderedSourceTy
            targetTy
            (ETyInst term reordering)
        Nothing -> constructWithoutReordering
  where
    constructWithoutReordering =
      case (sourceTy, targetTy, term) of
        ( TForallRef sourceRef sourceBound sourceBody,
          TForallRef targetRef targetBound targetBody,
          ETyAbsRef termRef termBound body
          )
            | typeBinderRefsSameIdentity sourceRef termRef,
              not
                ( any
                    (typeBinderRefsSameIdentity targetRef)
                    (freeTypeVarRefsType sourceTy)
                ),
              boundsAgree sourceBound targetBound -> do
                let targetBody' =
                      substTypeCaptureRef targetRef (TVarRef sourceRef) targetBody
                    boundTy = maybe TBottom tyToElab termBound
                    tcEnv' = TypeCheck.insertTypeBindingRef sourceRef boundTy tcEnv
                body' <-
                  constructExactAnnotationTermForWithIntroducedBinders
                    boundaryRole
                    introducedTargetRefs
                    tcEnv'
                    sourceBody
                    targetBody'
                    body
                pure (ETyAbsRef sourceRef termBound body')
        ( TForallRef sourceRef _ sourceBody,
          _,
          ETyAbsRef termRef termBound body
          )
            | typeBinderRefsSameIdentity sourceRef termRef,
              any (typeBinderRefsSameIdentity sourceRef) (freeTypeVarRefsType targetTy) -> do
                let boundTy = maybe TBottom tyToElab termBound
                    tcEnv' = TypeCheck.insertTypeBindingRef sourceRef boundTy tcEnv
                body' <-
                  constructExactAnnotationTermForWithIntroducedBinders
                    boundaryRole
                    introducedTargetRefs
                    tcEnv'
                    sourceBody
                    targetTy
                    body
                pure (ETyAbsRef sourceRef termBound body')
        (_, TForallRef targetRef targetBound targetBody, _)
          | any
              (typeBinderRefsSameIdentity targetRef)
              (freeTypeVarRefsType sourceTy) -> do
              let boundTy = maybe TBottom tyToElab targetBound
                  tcEnv' = TypeCheck.insertTypeBindingRef targetRef boundTy tcEnv
              body' <-
                constructExactAnnotationTermForWithIntroducedBinders
                  boundaryRole
                  (targetRef : introducedTargetRefs)
                  tcEnv'
                  sourceTy
                  targetBody
                  term
              pure (ETyAbsRef targetRef targetBound body')
        (TArrow sourceDom sourceCod, TArrow targetDom targetCod, ELam resolved body)
          | alphaEqType sourceDom targetDom || churchAwareEqType sourceDom targetDom -> do
              let resolved' = mapResolvedVarType (const targetDom) resolved
                  bodyForTarget = refreshLocalResolvedVarType resolved' targetDom body
                  tcEnv' = TypeCheck.insertResolvedTermBinding resolved' targetDom tcEnv
              body' <-
                constructExactAnnotationTermForWithIntroducedBinders
                  boundaryRole
                  introducedTargetRefs
                  tcEnv'
                  sourceCod
                  targetCod
                  bodyForTarget
              pure (ELam resolved' body')
        (_, _, ELet resolved scheme rhs body) -> do
          let schemeTy = schemeToType scheme
              resolved' = mapResolvedVarType (const schemeTy) resolved
              tcEnv' = TypeCheck.insertResolvedTermBinding resolved' schemeTy tcEnv
          bodyTy <-
            case TypeCheck.typeCheckWithEnv tcEnv' body of
              Left err -> exactConstructionFailure sourceTy targetTy (show err)
              Right ty -> pure ty
          body' <-
            constructExactAnnotationTermForWithIntroducedBinders
              boundaryRole
              introducedTargetRefs
              tcEnv'
              bodyTy
              targetTy
              body
          pure (ELet resolved' scheme rhs body')
        _ ->
          case inferPreservingAnnotationInstFor boundaryRole sourceTy targetTy of
            Just inst ->
              let inst' = normalizeInst inst
               in case applyInstantiation sourceTy inst' of
                    Right appliedTy
                      | alphaEqType appliedTy targetTy
                          || churchAwareEqType appliedTy targetTy ->
                          -- Keep the paper translation compositional: a
                          -- positive root RaiseMerge first emits Hyp and this
                          -- exact boundary emits N.  Only after both typed
                          -- computations are present may their same-identity
                          -- type beta-redex be removed.
                          let constructed =
                                case inst' of
                                  InstId -> term
                                  _ -> ETyInst term inst'
                           in Right
                                (reduceOwnerAlignedLeadingTypeRedex constructed)
                    Right appliedTy ->
                      exactConstructionFailure
                        sourceTy
                        targetTy
                        ("instantiation produced " ++ show appliedTy)
                    Left err ->
                      exactConstructionFailure sourceTy targetTy (show err)
            Nothing ->
              exactConstructionFailure
                sourceTy
                targetTy
                "no preserving xMLF construction"
    boundsAgree Nothing Nothing = True
    boundsAgree (Just left) (Just right) =
      alphaEqType (tyToElab left) (tyToElab right)
        || churchAwareEqType (tyToElab left) (tyToElab right)
    boundsAgree _ _ = False

    -- A target binder introduced by this exact boundary is a distinct lexical
    -- owner.  Keep an unrelated producer abstraction and its N computation
    -- explicit underneath that binder; beta-reducing it would erase the
    -- producer-side construction history that established the ordering.
    reduceOwnerAlignedLeadingTypeRedex constructed =
      case constructed of
        ETyInst (ETyAbsRef sourceRef _ _) InstElim
          | any
              (not . typeBinderRefsSameIdentity sourceRef)
              introducedTargetRefs ->
              constructed
        _ ->
          fromMaybe
            constructed
            (Reduce.reduceLeadingTypeInstantiationRedexes constructed)

    exactConstructionFailure :: ElabType -> ElabType -> String -> Either ElabError a
    exactConstructionFailure source target reason =
      Left
        ( PhiInvariantError
            ( "compiler exact annotation has no lexical construction; source="
                ++ show source
                ++ "; expected="
                ++ show target
                ++ "; reason="
                ++ reason
                ++ "; term outer shape="
                ++ termOuterShape term
            )
        )

    termOuterShape outerTerm =
      case outerTerm of
        ETyInst inner inst -> "ETyInst " ++ show inst ++ " (" ++ termOuterShape inner ++ ")"
        ETyAbsRef ref _ inner -> "ETyAbs " ++ show ref ++ " (" ++ termOuterShape inner ++ ")"
        ELet _ _ _ inner -> "ELet (... " ++ termOuterShape inner ++ ")"
        ELam{} -> "ELam"
        EApp{} -> "EApp"
        EVarNode{} -> "EVarNode"
        ELit{} -> "ELit"
        ERoll _ inner -> "ERoll (" ++ termOuterShape inner ++ ")"
        EUnroll inner -> "EUnroll (" ++ termOuterShape inner ++ ")"

-- | Apply the thesis's @phi_R@ before an exact boundary consumes or preserves
-- any leading quantifier.  A generalized root follows the graph's @<P@ order,
-- which can differ from the source contract's lexical order.  Pairing those
-- binders positionally would exchange distinct source identities; the
-- explicit Sigma computation instead commutes the existing binders while
-- preserving which occurrences each binder owns.
exactLeadingBinderReordering
  :: ElabType
  -> ElabType
  -> Either ElabError (Maybe (Instantiation, ElabType))
exactLeadingBinderReordering sourceTy targetTy
  | null targetRefs = pure Nothing
  | not (all (`hasIdentityIn` sourceRefs) targetRefs) = pure Nothing
  | leadingIdentitiesAgree sourceRefs targetRefs = pure Nothing
  | hasDuplicateIdentities sourceRefs || hasDuplicateIdentities targetRefs =
      Left
        ( ValidationFailed
            [ "compiler exact quantifier reordering has duplicate binder identities"
            , "  source binders: " ++ show sourceRefs
            , "  target binders: " ++ show targetRefs
            ]
        )
  | otherwise = do
      reordering <- Sigma.sigmaReorder sourceTy targetTy
      reorderedSourceTy <- applyInstantiation sourceTy reordering
      pure (Just (reordering, reorderedSourceTy))
  where
    sourceRefs = map fst (schemeBinderRefs (schemeFromType sourceTy))
    targetRefs = map fst (schemeBinderRefs (schemeFromType targetTy))

    ref `hasIdentityIn` refs =
      any (typeBinderRefsSameIdentity ref) refs

    leadingIdentitiesAgree sourceRefs0 targetRefs0 =
      length sourceRefs0 >= length targetRefs0
        && and
          ( zipWith
              typeBinderRefsSameIdentity
              sourceRefs0
              targetRefs0
          )

    hasDuplicateIdentities = go []
      where
        go _ [] = False
        go seen (ref : rest)
          | ref `hasIdentityIn` seen = True
          | otherwise = go (ref : seen) rest

-- | Construct the paper's preserving xMLF computation from one already
-- checked producer type to an exact consumer type.  This is also the final
-- checked-binding seam for a principal producer that is more general than its
-- declared source contract.
constructExactTermAtType ::
  TypeCheck.Env ->
  ElabType ->
  ElabType ->
  XmlfTerm ->
  Either ElabError XmlfTerm
constructExactTermAtType = constructExactAnnotationTerm

-- | Records why an annotation instantiation was selected.  An identity
-- computation obtained because the already-constructed term has the exact
-- annotation type is authoritative; it must not later be mistaken for a
-- degenerate edge translation and reconstructed from an unrelated source
-- scheme.
data AnnotationInstantiationAuthority
  = ConstructedAnnotationInstantiation Instantiation
  | EdgeAnnotationInstantiation Instantiation

-- | Project only inherited source identities through the construction
-- quotient.  Binder declarations retain their owner identity; applying the
-- quotient to the whole scheme would turn an annotation-local forall into an
-- ambient binder.  The same filtered quotient is used for the term, its
-- expected source scheme, and its inferred source scheme below.
constructionBinderRenamesForRefs
  :: [(TypeBinderRef, TypeBinderRef)]
  -> [TypeBinderRef]
  -> [(TypeBinderRef, TypeBinderRef)]
constructionBinderRenamesForRefs renames refs =
  [ rename
  | rename@(sourceRef, _) <- renames
  , any (typeBinderRefsSameIdentity sourceRef) refs
  ]

applyFreeConstructionBinderRenamesToScheme
  :: [(TypeBinderRef, TypeBinderRef)]
  -> ElabScheme
  -> ElabScheme
applyFreeConstructionBinderRenamesToScheme renames scheme =
  mkElabSchemeWithRefs
    [ (ref, fmap (mapBoundType renameType) mbBound)
    | (ref, mbBound) <- schemeBinderRefs scheme
    ]
    (renameType (schemeBody scheme))
  where
    activeRenames =
      constructionBinderRenamesForRefs
        renames
        (freeTypeVarRefsType (schemeToType scheme))
    renameType ty0 =
      foldl'
        ( \ty (sourceRef, constructionRef) ->
            if typeBinderRefsSameIdentityAndName sourceRef constructionRef
              then ty
              else substTypeCaptureRef sourceRef (TVarRef constructionRef) ty
        )
        ty0
        activeRenames

applyFreeConstructionBinderRenamesToSchemeInfo
  :: [(TypeBinderRef, TypeBinderRef)]
  -> SchemeInfo
  -> SchemeInfo
applyFreeConstructionBinderRenamesToSchemeInfo renames schemeInfo =
  schemeInfoFromRefSubst
    (applyFreeConstructionBinderRenamesToScheme activeRenames (siScheme schemeInfo))
    (IntMap.map renameRef (schemeInfoBinderRefSubst schemeInfo))
  where
    activeRenames =
      constructionBinderRenamesForRefs
        renames
        (freeTypeVarRefsType (schemeToType (siScheme schemeInfo)))
    renameRef ref =
      case find (\(sourceRef, _) -> typeBinderRefsSameIdentity sourceRef ref) activeRenames of
        Just (_, constructionRef) -> constructionRef
        Nothing -> ref

elaborateAnnotationTerm ::
  AnnotationBoundaryRole ->
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  TypeCheck.Env ->
  [(TypeBinderRef, TypeBinderRef)] ->
  IntMap.IntMap TypeBinderRef ->
  AnnExpr ->
  NodeId ->
  EdgeId ->
  XmlfTerm ->
  Either ElabError XmlfTerm
elaborateAnnotationTerm boundaryRole annotationContext namedSetReify resolvedLookup tcEnv constructionBinderRenames constructionIdentityRoutes exprAnn _annNodeId eid expr' = do
  preparedAnnotationConstructionBinderRenames <-
    foldM
      insertConstructionBinderRename
      []
      ( constructionBinderRenames
          ++ concatMap
            subtermGeneralizationConstructionBinderRenames
            ( Map.elems
                ( subtermGeneralizationsOwnedBy
                    exprAnn
                    (acSubtermGeneralizations annotationContext)
                )
            )
      )
  annotationSourceBinderRefs <-
    foldM
      insertConstructionSourceRoute
      (acSourceBinderRefs annotationContext)
      (IntMap.toList constructionIdentityRoutes)
  annotationExpectedType <-
    case annotationExpectedTypeForEdge annotationContext eid of
      Just expectedTy -> pure expectedTy
      Nothing ->
        Left
          ( ValidationFailed
              ["missing source type for annotation " ++ show eid]
          )
  let expectedSourceScheme0 = schemeFromType annotationExpectedType
  let representative =
        scopeTypeBinderIdentityRepresentative
          (acScopeContext annotationContext)
  scopedConstructionBinderRenames <-
    either
      ( \cause ->
          Left
            ( ValidationFailed
                [ "annotation construction source-to-Gamma route is inconsistent",
                  "  edge: " ++ show eid,
                  "  cause: " ++ cause
                ]
            )
      )
      Right
      ( scopedAnnotationConstructionBinderRenames
          representative
          (acSourceBinderRefs annotationContext)
          constructionIdentityRoutes
          (schemeToType expectedSourceScheme0)
      )
  annotationConstructionBinderRenames <-
    foldM
      insertConstructionBinderRename
      preparedAnnotationConstructionBinderRenames
      scopedConstructionBinderRenames
  let
      expectedSourceScheme =
        applyFreeConstructionBinderRenamesToScheme
          annotationConstructionBinderRenames
          expectedSourceScheme0
      sourceTermSubst =
        sourceBinderAliasSubstitution
          representative
          annotationSourceBinderRefs
          (Reduce.freeTypeVarRefsTerm expr')
      sourceTerm0 = substInTermRefs sourceTermSubst expr'
      sourceTermConstructionRenames =
        constructionBinderRenamesForRefs
          annotationConstructionBinderRenames
          (Reduce.freeTypeVarRefsTerm sourceTerm0)
      exprFresh =
        freshenTermTypeAbsAgainstEnv tcEnv
          (renameTermTypeVars sourceTermConstructionRenames sourceTerm0)
      sourceTermConstruction =
        checkedCompositeConstruction
          <|> annotatedLambdaParamConstruction
      checkedCompositeConstruction =
        case annExprReferenceKey exprAnn of
          Just _ -> Nothing
          Nothing -> do
            sourceTy <-
              either
                (const Nothing)
                Just
                (TypeCheck.typeCheckWithEnv tcEnv exprFresh)
            sourceInst <-
              inferPreservingAnnotationInstFor
                boundaryRole
                sourceTy
                (schemeToType expectedSourceScheme)
            pure (exprFresh, sourceInst)
      annotatedLambdaParamConstruction = do
        closed <-
          closeAnnotatedLambdaParam
            annotationConstructionBinderRenames
            tcEnv
            (schemeToType expectedSourceScheme)
            exprFresh
        closedTy <-
          either
            (const Nothing)
            Just
            (TypeCheck.typeCheckWithEnv tcEnv closed)
        closedInst <-
          inferPreservingAnnotationInstFor
            boundaryRole
            closedTy
            (schemeToType expectedSourceScheme)
        pure (closed, closedInst)
  sourceSchemeInfo0 <-
    case sourceTermConstruction of
      -- A checked composite source term is the construction authority for
      -- its complete abstraction/parameter spine.  Once that spine admits a
      -- preserving computation to the annotation type, do not independently
      -- generalize the same source and reconstruct its locally owned Gamma a
      -- second time from the graph edge.
      Just _ -> pure Nothing
      Nothing ->
        schemeInfoForAnnotationSource
          annotationContext
          namedSetReify
          resolvedLookup
          exprAnn
  sourceSchemeInfo <-
    traverse
      ( \schemeInfo ->
          either
            ( \cause ->
                Left
                  ( ValidationFailed
                      [ "annotation source scheme has inconsistent source-binder provenance"
                      , "  edge: " ++ show eid
                      , "  cause: " ++ cause
                      ]
                  )
            )
            Right
            ( do
                resolvedSchemeInfo <-
                  resolveConstructionSourceBindersInSchemeInfoExcept
                    producerOwnedBinderIdentities
                    representative
                    annotationSourceBinderRefs
                    schemeInfo
                pure
                  ( applyFreeConstructionBinderRenamesToSchemeInfo
                      annotationConstructionBinderRenames
                      resolvedSchemeInfo
                  )
            )
      )
      sourceSchemeInfo0
  let freshenSchemeAgainstEnv scheme0 =
        let reserved =
              Set.unions
                ( map freeTypeVarAliasNamesType (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv tcEnv)))
                    ++ [typeVarRefAliasNames (Map.keys (TypeCheck.typeEnv tcEnv))]
                )
            go _ [] bodyAcc acc = (reverse acc, bodyAcc)
            go used ((ref, mb) : rest) bodyAcc acc =
              let name = typeBinderRefName ref
                  ref' = if Set.member name used then renameTypeBinderRef (freshNameLike name used) ref else ref
                  aliases' = typeBinderRefAliasNames ref'
                  renameTy = TVarRef ref'
                  bodyAcc' =
                    if typeBinderRefsSameIdentityAndName ref' ref
                      then bodyAcc
                      else substTypeCaptureRef ref renameTy bodyAcc
                  acc' = (ref', mb) : acc
               in go (aliases' `Set.union` used) rest bodyAcc' acc'
            (binds', body') = go reserved (schemeBinderRefs scheme0) (schemeBody scheme0) []
         in mkElabSchemeWithRefs binds' body'
      exprPrepared =
        case sourceSchemeInfo of
          Just schemeInfo ->
            closeTermWithSchemeSubstRefsIfNeeded
              (siSubstRefs schemeInfo)
              (freshenSchemeAgainstEnv (siScheme schemeInfo))
              exprFresh
          Nothing -> exprFresh
      sourceMatchesExplicitAnnotation =
        case sourceSchemeInfo of
          Just schemeInfo ->
            alphaEqType (schemeToType (siScheme schemeInfo)) (schemeToType expectedSourceScheme)
          _ -> False
      canReuseSourceScheme = sourceMatchesExplicitAnnotation
      requiresExplicitAnnotationInst =
        case sourceSchemeInfo of
          Just schemeInfo ->
            let srcScheme = siScheme schemeInfo
                sourcePoly = not (null (schemeBinderRefs srcScheme))
             in sourcePoly
                  && not sourceMatchesExplicitAnnotation
          _ -> False
      preservingAnnotationInst = do
        schemeInfo <- sourceSchemeInfo
        inferPreservingAnnotationInstFor
          boundaryRole
          (schemeToType (siScheme schemeInfo))
          (schemeToType expectedSourceScheme)
      preparedTermMatchesAnnotation =
        case (annExprReferenceKey exprAnn, TypeCheck.typeCheckWithEnv tcEnv exprPrepared) of
          (Nothing, Right ty) ->
            alphaEqType ty (schemeToType expectedSourceScheme)
              || churchAwareEqType ty (schemeToType expectedSourceScheme)
          _ -> False
  instAuthority <-
    case sourceTermConstruction of
      Just (_, closedInst) ->
        pure (ConstructedAnnotationInstantiation closedInst)
      Nothing ->
        case preservingAnnotationInst of
          -- Section 15.3.8 gives source annotations their own preserving
          -- coercion construction.  Select it before translating the generic
          -- occurrence edge: a degenerate graph expansion can legitimately have
          -- no active binders even when the reduced consumer scheme still has an
          -- explicit forall spine.
          Just preservingInst ->
            pure (ConstructedAnnotationInstantiation preservingInst)
          Nothing ->
            if preparedTermMatchesAnnotation || canReuseSourceScheme
              then
                -- The term has already been constructed at the annotation type.
                -- Preserve that equality as the authority for InstId instead of
                -- losing its provenance in the generic edge path below.
                pure (ConstructedAnnotationInstantiation InstId)
              else
                EdgeAnnotationInstantiation
                  <$> reifyInst annotationContext namedSetReify resolvedLookup exprAnn eid
  inst <-
    case instAuthority of
      ConstructedAnnotationInstantiation constructedInst ->
        pure constructedInst
      EdgeAnnotationInstantiation edgeInst ->
        case (edgeInst, sourceSchemeInfo) of
          (InstId, Just schemeInfo)
            | requiresExplicitAnnotationInst ->
                case
                  inferInstAppArgsFromSchemeRefs
                    (schemeBinderRefs (siScheme schemeInfo))
                    (schemeBody (siScheme schemeInfo))
                    (schemeToType expectedSourceScheme)
                  of
                  Just args -> pure (instSeqApps args)
                  Nothing ->
                    Left
                      ( PhiTranslatabilityError
                          [ "AAnnF: missing authoritative instantiation for annotation edge "
                              ++ show eid
                              ++ "; source="
                              ++ show (fmap (schemeToType . siScheme) sourceSchemeInfo)
                              ++ "; annotation="
                              ++ show (schemeToType expectedSourceScheme)
                          ]
                      )
          _ -> pure edgeInst
  let preservesForalls =
        not (null (schemeBinderRefs expectedSourceScheme))
      instAdjusted =
        case sourceTermConstruction of
          Just (_, closedInst) -> normalizeInst closedInst
          Nothing ->
            case preservingAnnotationInst of
              Just preservingInst -> normalizeInst preservingInst
              Nothing ->
                if preservesForalls
                  then normalizeInst (adjustAnnotationInst inst)
                  -- A monomorphic annotation consumes the witness's complete
                  -- quantifier spine.  In particular, a bounded vacuous
                  -- prefix contributes N before a later inferred InstApp;
                  -- erasing all eliminations would apply that argument to the
                  -- prefix bound instead of to the remaining binder.
                  else normalizeInst inst
  exprClosed0 <-
    case sourceTermConstruction of
      Just (closed, _) -> pure closed
      Nothing ->
        if instAdjusted == InstId
          then
            if canReuseSourceScheme && sourceAnnIsPolymorphicResolved resolvedLookup exprAnn
              then pure exprPrepared
              else
                let alignedExpr =
                      fromMaybe
                        exprPrepared
                        ( alignTopTyAbsToScheme expectedSourceScheme exprPrepared
                            <|> alignTermTypeVarsToScheme expectedSourceScheme exprPrepared
                            <|> alignTermTypeVarsToTopTyAbs exprPrepared
                        )
                    alignedExprMatchesExpected =
                      case TypeCheck.typeCheckWithEnv tcEnv alignedExpr of
                        Right tyExpr ->
                          alphaEqType tyExpr (schemeToType expectedSourceScheme)
                            || churchAwareEqType tyExpr (schemeToType expectedSourceScheme)
                        Left _ -> False
                 in case exprPrepared of
                      ETyAbsRef {}
                        | alignedExprMatchesExpected ->
                            pure alignedExpr
                        | otherwise ->
                            pure (closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (freshenSchemeAgainstEnv expectedSourceScheme) alignedExpr)
                      _ -> pure (closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (freshenSchemeAgainstEnv expectedSourceScheme) exprPrepared)
          else
            let instHasUnder inst0 =
                  case inst0 of
                    InstUnderRef {} -> True
                    InstSeq a b -> instHasUnder a || instHasUnder b
                    InstInside a -> instHasUnder a
                    _ -> False
                instLooksLikeApp inst0 =
                  case inst0 of
                    InstApp {} -> True
                    InstInside (InstBot _) -> True
                    InstInside (InstApp _) -> True
                    InstSeq (InstInside (InstBot _)) InstElim -> True
                    InstSeq (InstInside (InstApp _)) InstElim -> True
                    _ -> False
             in if sourceAnnIsPolymorphicResolved resolvedLookup exprAnn
                  then pure exprPrepared
                  else
                    if instLooksLikeApp instAdjusted
                      then case (annExprReferenceKey exprAnn, TypeCheck.typeCheckWithEnv tcEnv exprPrepared) of
                        (Nothing, Right TForallRef {}) ->
                          if instHasUnder instAdjusted
                            then pure (closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (freshenSchemeAgainstEnv expectedSourceScheme) exprPrepared)
                            else pure (closeTermForAnnotation exprPrepared)
                        (Nothing, Right _) -> pure exprPrepared
                        _ ->
                          if instHasUnder instAdjusted
                            then pure (closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (freshenSchemeAgainstEnv expectedSourceScheme) exprPrepared)
                            else pure (closeTermForAnnotation exprPrepared)
                      else
                        if instHasUnder instAdjusted
                          then pure (closeTermWithSchemeSubstRefsIfNeeded IntMap.empty (freshenSchemeAgainstEnv expectedSourceScheme) exprPrepared)
                          else pure (closeTermForAnnotation exprPrepared)
  let exprClosed =
        -- A source annotation owns an ordered forall/abstraction spine.
        -- Even a vacuous binder remains observable to positional xMLF
        -- computations such as quantification elimination (N), so erasing it
        -- here would publish a term with a different instantiation ABI.
        rollExplicitMuAnnotation
          annotationConstructionBinderRenames
          tcEnv
          expectedSourceScheme
          exprClosed0
  sourceClosedTy <-
    case TypeCheck.typeCheckWithEnv tcEnv exprClosed of
      Left tcError ->
        Left
          ( PhiInvariantError
              ( unlines
                  [ "annotation source term is not typable before applying authoritative edge " ++ show eid,
                    "typecheck=" ++ show tcError,
                    "source scheme=" ++ show (fmap (schemeToType . siScheme) sourceSchemeInfo),
                    "expected annotation=" ++ show (schemeToType expectedSourceScheme)
                  ]
              )
          )
      Right tyExpr -> pure tyExpr
  let expectedAnnotationTy = schemeToType expectedSourceScheme
      -- Closing the source term is part of constructing the annotation
      -- producer, so its checked type is the final authority for the
      -- annotation computation.  Reconstruct the complete positional
      -- computation from that type before emitting it.  In particular, a
      -- monomorphic target can require @N ; InstApp tau@: replacing that
      -- sequence with its final application changes which source binder the
      -- argument consumes.
      closedPreservingAnnotationInst =
        normalizeInst
          <$> inferPreservingAnnotationInstFor
            boundaryRole
            sourceClosedTy
            expectedAnnotationTy
      instFinal
        | alphaEqType sourceClosedTy expectedAnnotationTy
            || churchAwareEqType sourceClosedTy expectedAnnotationTy = InstId
        | Just constructedInst <- closedPreservingAnnotationInst = constructedInst
        | otherwise = instAdjusted
      annotatedTerm =
        case instFinal of
          InstId -> exprClosed
          _ -> ETyInst exprClosed instFinal
  annotatedTy <-
    case TypeCheck.typeCheckWithEnv tcEnv annotatedTerm of
      Left tcError ->
        Left
          ( PhiInvariantError
              ( "authoritative annotation instantiation for edge "
                  ++ show eid
                  ++ " does not typecheck: "
                  ++ show instFinal
                  ++ "; "
                  ++ show tcError
                  ++ "; source type="
                  ++ show sourceClosedTy
                  ++ "; expected="
                  ++ show expectedAnnotationTy
                  ++ "; source scheme="
                  ++ show (fmap (schemeToType . siScheme) sourceSchemeInfo)
                  ++ "; preserving computation="
                  ++ show preservingAnnotationInst
                  ++ "; closed preserving computation="
                  ++ show closedPreservingAnnotationInst
                  ++ "; edge computation="
                  ++ show inst
              )
          )
      Right ty -> pure ty
  if
      alphaEqType annotatedTy expectedAnnotationTy
        || churchAwareEqType annotatedTy expectedAnnotationTy
    then
      case instFinal of
        InstId -> pure exprClosed
        _ ->
          case applyInstantiation sourceClosedTy instFinal of
            Right tyApplied
              | alphaEqType tyApplied sourceClosedTy -> pure exprClosed
            _ -> pure annotatedTerm
    else
      Left
        ( PhiInvariantError
            ( "authoritative annotation instantiation for edge "
                ++ show eid
                ++ " with source type "
                ++ show sourceClosedTy
                ++ " and computation "
                ++ show instFinal
                ++ " from source scheme "
                ++ show (fmap (schemeToType . siScheme) sourceSchemeInfo)
                ++ " and preserving computation "
                ++ show preservingAnnotationInst
                ++ " and closed preserving computation "
                ++ show closedPreservingAnnotationInst
                ++ " has type "
                ++ show annotatedTy
                ++ " instead of its source annotation "
                ++ show expectedAnnotationTy
            )
        )
  where
    producerOwnedBinderIdentities =
      case sourceAnnSchemeInfoResolved resolvedLookup exprAnn of
        Nothing -> Set.empty
        Just schemeInfo ->
          Set.fromList
            ( map
                (typeBinderRefIdentity . fst)
                (schemeBinderRefs (siScheme schemeInfo))
            )

    sourceBinderIdentities =
      Map.elems (acSourceTypeBinderIdentities annotationContext)

    insertConstructionSourceRoute refs (nodeKey, outwardRef)
      | typeBinderRefIdentity outwardRef `notElem` sourceBinderIdentities =
          pure refs
      | otherwise =
          case IntMap.lookup nodeKey refs of
            Nothing -> pure (IntMap.insert nodeKey outwardRef refs)
            Just existingRef
              | typeBinderRefsSameIdentity existingRef outwardRef -> pure refs
              | otherwise ->
                  Left
                    ( ValidationFailed
                        [ "annotation construction route conflicts with source-binder provenance"
                        , "  edge: " ++ show eid
                        , "  graph node: " ++ show (NodeId nodeKey)
                        , "  source binder: " ++ show existingRef
                        , "  construction binder: " ++ show outwardRef
                        ]
                    )

    insertConstructionBinderRename renames rename@(sourceRef, constructionRef) =
      case find (typeBinderRefsSameIdentity sourceRef . fst) renames of
        Nothing -> pure (renames ++ [rename])
        Just (_, existingRef)
          | typeBinderRefsSameIdentity existingRef constructionRef -> pure renames
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "annotation construction quotient routes one source binder to multiple identities"
                    , "  edge: " ++ show eid
                    , "  source binder: " ++ show sourceRef
                    , "  first construction binder: " ++ show existingRef
                    , "  second construction binder: " ++ show constructionRef
                    ]
                )

    rollExplicitMuAnnotation :: [(TypeBinderRef, TypeBinderRef)] -> TypeCheck.Env -> ElabScheme -> XmlfTerm -> XmlfTerm
    rollExplicitMuAnnotation binderRenames checkEnv expectedScheme term =
      case schemeToType expectedScheme of
        muTy@TMuRef {} ->
          case TypeCheck.typeCheckWithEnv checkEnv term of
            Right termTy
              | alphaEqType termTy muTy -> term
              | Just unfoldedTy <- unfoldMuOnce muTy,
                alphaEqType termTy unfoldedTy ->
                  ERoll muTy term
              | churchAwareEqType termTy muTy -> term
            _ ->
              case unfoldMuOnce muTy of
                Just unfoldedTy ->
                  let aligned = alignTermAlongType binderRenames checkEnv unfoldedTy term
                   in case TypeCheck.typeCheckWithEnv tcEnv aligned of
                        Right alignedTy
                          | alphaEqType alignedTy unfoldedTy ->
                              ERoll muTy aligned
                        _ -> term
                Nothing -> term
        _ -> term

    unfoldMuOnce :: ElabType -> Maybe ElabType
    unfoldMuOnce ty =
      case ty of
        TMuRef ref body -> Just (substTypeCaptureRef ref ty body)
        _ -> Nothing

    alignTermAlongType :: [(TypeBinderRef, TypeBinderRef)] -> TypeCheck.Env -> ElabType -> XmlfTerm -> XmlfTerm
    alignTermAlongType binderRenames checkEnv targetTy term =
      case (targetTy, term) of
        (TForallRef targetRef _mbBound targetBody, ETyAbsRef termRef termBound body)
          | typeBinderRefsSameIdentity targetRef termRef ->
              let boundTy = maybe TBottom tyToElab termBound
                  checkEnv' = TypeCheck.insertTypeBindingRef termRef boundTy checkEnv
               in ETyAbsRef termRef termBound (alignTermAlongType binderRenames checkEnv' targetBody body)
        (TForallRef targetRef mbBound targetBody, _)
          | termDeclaresTypeAbstraction targetRef term -> term
          | otherwise ->
              let boundTy = maybe TBottom tyToElab mbBound
                  checkEnv' = TypeCheck.insertTypeBindingRef targetRef boundTy checkEnv
                  termForBody =
                    case TypeCheck.typeCheckWithEnv checkEnv term of
                      Right sourceTy ->
                        case constructionBinderForTarget targetRef sourceTy of
                          Just constructionRef
                            | typeBinderRefsSameIdentity constructionRef targetRef ->
                                term
                            | otherwise ->
                                renameTermTypeVars [(constructionRef, targetRef)] term
                          Nothing -> term
                      Left _ -> term
               in ETyAbsRef
                    targetRef
                    mbBound
                    (alignTermAlongType binderRenames checkEnv' targetBody termForBody)
        (TArrow dom cod, ELam resolved body) ->
          let resolved' = mapResolvedVarType (const dom) resolved
              body' = refreshLocalResolvedVarType resolved dom body
              checkEnv' = TypeCheck.insertResolvedTermBinding resolved' dom checkEnv
           in ELam resolved' (alignTermAlongType binderRenames checkEnv' cod body')
        (_, sourcePoly@ETyAbsRef {}) ->
          case TypeCheck.typeCheckWithEnv checkEnv sourcePoly of
            Right sourceTy
              | Just exactInst <- inferPreservingAnnotationInst sourceTy targetTy,
                Right appliedTy <- applyInstantiation sourceTy exactInst,
                alphaEqType appliedTy targetTy ->
                  ETyInst sourcePoly exactInst
            _ -> term
        _ -> term
      where
        -- Packet preparation records the source-to-construction quotient
        -- while both identity domains are present.  Consume that route in
        -- reverse when publishing the source forall; endpoint shape is only
        -- validation and is never authority to merge two free identities.
        constructionBinderForTarget targetRef sourceTy =
          case distinctRefs (directRefs ++ routedRefs) of
            [constructionRef] -> Just constructionRef
            _ -> Nothing
          where
            freeRefs = freeTypeVarRefsType sourceTy
            directRefs =
              [ freeRef
              | freeRef <- freeRefs
              , typeBinderRefsSameIdentity freeRef targetRef
              ]
            routedRefs =
              [ constructionRef
              | (sourceRef, constructionRef) <- binderRenames
              , typeBinderRefsSameIdentity sourceRef targetRef
              , any
                  (typeBinderRefsSameIdentity constructionRef)
                  freeRefs
              ]

        distinctRefs = foldr insertDistinctRef []

        insertDistinctRef ref refs
          | any (typeBinderRefsSameIdentity ref) refs = refs
          | otherwise = ref : refs

        -- Introducing a target forall around an existing abstraction with
        -- the same identity would capture any routed free occurrence below
        -- it.  Such a term has no valid construction at this boundary; leave
        -- it unchanged so the exact endpoint check rejects it.
        termDeclaresTypeAbstraction targetRef term0 =
          case term0 of
            EVarNode {} -> False
            ELit {} -> False
            ELam _ body -> termDeclaresTypeAbstraction targetRef body
            EApp fun arg ->
              termDeclaresTypeAbstraction targetRef fun
                || termDeclaresTypeAbstraction targetRef arg
            ELet _ _ rhs body ->
              termDeclaresTypeAbstraction targetRef rhs
                || termDeclaresTypeAbstraction targetRef body
            ETyAbsRef ref _ body ->
              typeBinderRefsSameIdentity ref targetRef
                || termDeclaresTypeAbstraction targetRef body
            ETyInst inner _ -> termDeclaresTypeAbstraction targetRef inner
            ERoll _ body -> termDeclaresTypeAbstraction targetRef body
            EUnroll body -> termDeclaresTypeAbstraction targetRef body

    closeAnnotatedLambdaParam :: [(TypeBinderRef, TypeBinderRef)] -> TypeCheck.Env -> ElabType -> XmlfTerm -> Maybe XmlfTerm
    closeAnnotatedLambdaParam binderRenames checkEnv annotationTy term =
      case annotationTy of
        TForallRef {} ->
          let aligned = alignTermAlongType binderRenames checkEnv annotationTy term
           in case TypeCheck.typeCheckWithEnv checkEnv aligned of
                Right alignedTy
                  | alphaEqType alignedTy annotationTy || churchAwareEqType alignedTy annotationTy ->
                      Just aligned
                _ -> Nothing
        _ ->
          case (annotationTy, leadingLambda term) of
            (TArrow dom _, Just (leadingRefs, resolved)) -> do
              (binderRef, binderBoundTy) <-
                case resolvedVarType resolved of
                  -- The lambda parameter itself supplies this binder.  Its
                  -- source annotation is therefore the authoritative bound.
                  TVarRef ref -> Just (ref, dom)
                  _ ->
                    case freeInstantiationAbstractionRefs term of
                      -- A free Hyp in the body belongs to the result Gamma,
                      -- not to the annotated parameter.  Figure 15.3.8 closes
                      -- that binder with Gamma's recorded type; borrowing the
                      -- parameter domain here can construct the wrong
                      -- forall even though the identities are distinct.
                      [ref] ->
                        (\boundTy -> (ref, boundTy))
                          <$> TypeCheck.lookupTypeBindingRef ref checkEnv
                      _ -> Nothing
              if any (typeBinderRefsSameIdentity binderRef) leadingRefs
                then Nothing
                else pure ()
              bound <- either (const Nothing) Just (elabToBound binderBoundTy)
              let closed = eTyAbsWithRef binderRef (Just bound) term
              case TypeCheck.typeCheckWithEnv checkEnv closed of
                Right _ -> Just closed
                Left _ -> Nothing
            _ -> Nothing
      where
        -- A lambda's result Gamma can already be constructed before its
        -- source parameter annotation is elaborated.  Those leading
        -- abstractions do not hide the lambda owner: the parameter bound must
        -- wrap the complete term so both the parameter and result binders are
        -- in scope for the preserving annotation computation.
        leadingLambda term0 = goLeading [] term0

        goLeading leadingRefs term0 =
          case term0 of
            ETyAbsRef ref _ body -> goLeading (ref : leadingRefs) body
            ELam resolved _ -> Just (leadingRefs, resolved)
            _ -> Nothing

        freeInstantiationAbstractionRefs = goTerm []

        goTerm bound term0 =
          case term0 of
            EVarNode {} -> []
            ELit {} -> []
            ELam _ body -> goTerm bound body
            EApp fun arg -> unionRefs (goTerm bound fun) (goTerm bound arg)
            ELet _ _ rhs body -> unionRefs (goTerm bound rhs) (goTerm bound body)
            ETyAbsRef ref _ body -> goTerm (ref : bound) body
            ETyInst inner inst -> unionRefs (goTerm bound inner) (goInst bound inst)
            ERoll _ body -> goTerm bound body
            EUnroll body -> goTerm bound body

        goInst bound inst =
          case inst of
            InstId -> []
            InstApp _ -> []
            InstBot _ -> []
            InstIntro -> []
            InstElim -> []
            InstAbstrRef ref
              | any (typeBinderRefsSameIdentity ref) bound -> []
              | otherwise -> [ref]
            InstUnderRef ref inner -> goInst (ref : bound) inner
            InstInside inner -> goInst bound inner
            InstSeq left right -> unionRefs (goInst bound left) (goInst bound right)

        unionRefs left right =
          foldr insertRef right left
          where
            insertRef ref refs
              | any (typeBinderRefsSameIdentity ref) refs = refs
              | otherwise = ref : refs

{- Note [Transport occurrence computations across annotation reduction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Figure 15.3.5 elaborates an occurrence with @[phi_R(a); T(e)]@, not with
the raw edge translation @T(e)@ alone.  There is one further boundary for
source annotations (section 15.3.8): the prepared 'SourceExpectedType' owns
the operated scheme directly; inference does not manufacture an auxiliary
@forall gamma >= sigma. gamma@ wrapper.

The source term can still construct its more-general principal scheme while
the annotation edge targets that explicit operated scheme.  Applying raw
@T(e)@ to the principal source scheme is therefore not valid in general.  The
expansion arguments and authoritative edge target transport the occurrence
computation between those two construction-owned endpoints: target equality
gives identity; otherwise 'ExpInstantiate' supplies the explicit applications
valid for the source scheme.  This is construction authority from presolution,
not a post-typecheck repair.
-}

reifyInst ::
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInst annotationContext namedSetReify resolvedLookup =
  reifyInstWithFrozenEndpoints
    annotationContext
    namedSetReify
    resolvedLookup
    IntMap.empty

-- | Translate an edge while retaining exact endpoints already constructed by
-- sibling occurrences.  Frozen node ids are deliberately not canonicalized:
-- the certificate belongs to one witness-domain occurrence, not to its final
-- union-find representative.
reifyInstWithFrozenEndpoints ::
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  IntMap.IntMap ElabType ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstWithFrozenEndpoints annotationContext namedSetReify resolvedLookup frozenEndpointTypes =
  reifyInstWithSourceSchemeUsing
    (scGeneralizeAtWith (acScopeContext annotationContext))
    (acSourceBinderRefs annotationContext)
    annotationContext
    namedSetReify
    resolvedLookup
    frozenEndpointTypes
    Nothing
    Nothing

-- | Translate an occurrence edge from the exact type that has already passed
-- the local xMLF type checker.  That checked type is required source
-- authority; syntactic environment recovery contributes compatible
-- identity-bearing graph routes only.
--
-- Deferred class-method occurrences may deliberately be absent from the
-- elaboration environment after evidence rewriting.  Requiring the checked
-- source here keeps witness replay total at the application boundary without
-- reconstructing a scheme from display names or graph shape.
reifyInstWithFrozenEndpointsFromCheckedSource ::
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  IntMap.IntMap ElabType ->
  ElabType ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstWithFrozenEndpointsFromCheckedSource annotationContext =
  reifyInstWithFrozenEndpointsFromCheckedSourceUsing
    ( \_edgeId checkedSchemeInfo ->
        pure
          ( scGeneralizeAtWith (acScopeContext annotationContext),
            acSourceBinderRefs annotationContext,
            checkedSchemeInfo
          )
    )
    Nothing
    annotationContext

-- | Translate a checked occurrence edge inside an already prepared
-- construction Gamma.  Phi replay may generalize intermediate roots while
-- rebuilding @T(e)@; selecting the requirements-aware generalizer here makes
-- that replay inherit the exact owner/edge Gamma authority instead of asking
-- finalization to recover its binders from residual free references.
reifyInstWithFrozenEndpointsFromCheckedSourceInConstructionGamma ::
  AnnotationContext p ->
  IntSet.IntSet ->
  IntMap.IntMap TypeBinderRef ->
  GeneralizationRequirements ->
  PhiEndpointShapeAuthority ->
  (IdDetails -> Maybe SchemeInfo) ->
  IntMap.IntMap ElabType ->
  ElabType ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstWithFrozenEndpointsFromCheckedSourceInConstructionGamma annotationContext namedSetReify constructionAliases requirements endpointShapeAuthority =
  reifyInstWithFrozenEndpointsFromCheckedSourceUsing
    ( \edgeId checkedSchemeInfo -> do
        (replayRequirements, replaySchemeInfo) <-
          mergeOccurrenceSchemeInfoIntoReplayRequirements
            edgeId
            ( IntMap.lookup
                (getEdgeId edgeId)
                (acEdgeTraces annotationContext)
            )
            constructionAliases
            requirements
            checkedSchemeInfo
        pure
          ( scGeneralizeAtWithRequirements
              (acScopeContext annotationContext)
              replayRequirements,
            grSourceBinderRefs replayRequirements,
            replaySchemeInfo
          )
    )
    (Just endpointShapeAuthority)
    annotationContext
    namedSetReify

reifyInstWithFrozenEndpointsFromCheckedSourceUsing ::
  ( EdgeId ->
    SchemeInfo ->
    Either
      ElabError
      (GeneralizeAtWith p, IntMap.IntMap TypeBinderRef, SchemeInfo)
  ) ->
  Maybe PhiEndpointShapeAuthority ->
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  IntMap.IntMap ElabType ->
  ElabType ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstWithFrozenEndpointsFromCheckedSourceUsing replayCapabilityFor endpointShapeAuthority annotationContext namedSetReify resolvedLookup frozenEndpointTypes checkedSourceType funAnn edgeId = do
  recoveredSchemeInfo <-
    schemeInfoForInstantiation
      annotationContext
      namedSetReify
      resolvedLookup
      funAnn
  checkedSchemeInfo <-
    case
        checkedOccurrenceSchemeInfo
          checkedSourceType
          recoveredSchemeInfo
      of
        Left cause ->
          Left
            ( PhiInvariantError
                ( unlines
                    [ "cannot construct checked occurrence source authority",
                      "edge: " ++ show edgeId,
                      "source type: " ++ show checkedSourceType,
                      "cause: " ++ cause
                    ]
                )
            )
        Right schemeInfo -> Right schemeInfo
  (generalizeAtWith, replaySourceBinderRefs, replaySchemeInfo) <-
    replayCapabilityFor edgeId checkedSchemeInfo
  reifyInstWithSourceSchemeUsing
    generalizeAtWith
    replaySourceBinderRefs
    annotationContext
    namedSetReify
    (const Nothing)
    frozenEndpointTypes
    (Just replaySchemeInfo)
    endpointShapeAuthority
    funAnn
    edgeId

-- | Translate an edge from the exact scheme constructed for its source term.
-- Subterm generalization prepares this scheme before term elaboration; passing
-- it through here keeps closure and Figure 15.3.5 translation on the same
-- construction-owned source.  This is not a fallback: when present, the
-- supplied scheme is the only source authority consulted for the edge.
reifyInstFromSourceScheme ::
  AnnotationContext p ->
  IntSet.IntSet ->
  SchemeInfo ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstFromSourceScheme annotationContext namedSetReify sourceScheme =
  reifyInstWithSourceSchemeUsing
    (scGeneralizeAtWith (acScopeContext annotationContext))
    (acSourceBinderRefs annotationContext)
    annotationContext
    namedSetReify
    (const Nothing)
    IntMap.empty
    (Just sourceScheme)
    Nothing

-- | Translate an occurrence whose source and enclosing construction Gamma
-- were prepared together. Witness replay may generalize an intermediate root
-- while reconstructing @T(e)@. Selecting the requirements-aware generalizer
-- at this boundary carries the prepared consumer's closure authority into
-- every such replay root by construction.
reifyInstFromSourceSchemeInConstructionGamma ::
  AnnotationContext p ->
  IntSet.IntSet ->
  IntMap.IntMap TypeBinderRef ->
  GeneralizationRequirements ->
  SchemeInfo ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstFromSourceSchemeInConstructionGamma annotationContext namedSetReify constructionAliases requirements sourceScheme funAnn edgeId = do
  (replayRequirements, replaySourceScheme) <-
    mergeOccurrenceSchemeInfoIntoReplayRequirements
      edgeId
      ( IntMap.lookup
          (getEdgeId edgeId)
          (acEdgeTraces annotationContext)
      )
      constructionAliases
      requirements
      sourceScheme
  reifyInstWithSourceSchemeUsing
    ( scGeneralizeAtWithRequirements
        (acScopeContext annotationContext)
        replayRequirements
    )
    (grSourceBinderRefs replayRequirements)
    annotationContext
    namedSetReify
    (const Nothing)
    IntMap.empty
    (Just replaySourceScheme)
    Nothing
    funAnn
    edgeId

-- | Extend an already prepared replay capability with the exact graph routes
-- retained by the checked occurrence scheme.  This merge happens before the
-- requirements-aware generalizer is closed over the capability, so strict
-- replay alignment and every producer-root generalization observe the same
-- identity evidence.  The frozen trace additionally proves which source nodes
-- were copied to the same replay binder; those exact copy-domain aliases carry
-- the occurrence identity into producer-root binder planning.  A graph key may
-- not be reassigned to a different semantic binder identity.
mergeOccurrenceSchemeInfoIntoReplayRequirements ::
  EdgeId ->
  Maybe EdgeTrace ->
  IntMap.IntMap TypeBinderRef ->
  GeneralizationRequirements ->
  SchemeInfo ->
  Either ElabError (GeneralizationRequirements, SchemeInfo)
mergeOccurrenceSchemeInfoIntoReplayRequirements edgeId mbTrace constructionAliases requirements schemeInfo = do
  directSourceBinderRefs <-
    foldM
      insertOccurrenceRoute
      (grSourceBinderRefs requirements)
      (IntMap.toList (siSubstRefs replaySchemeInfo))
  sourceBinderRefs <-
    foldM
      insertOccurrenceRoute
      directSourceBinderRefs
      (traceCopyDomainRoutes directSourceBinderRefs)
  pure
    ( requirements {grSourceBinderRefs = sourceBinderRefs},
      replaySchemeInfo
    )
  where
    -- Construction preparation can project the complete Scheme type before a
    -- metadata-only substitution entry is rewritten.  Such an entry no
    -- longer names any free or declared identity in the supplied occurrence,
    -- so it cannot override the exact route already carried by the prepared
    -- replay capability.  Normalize that stale key through the capability
    -- before merging and return the same normalized SchemeInfo to Phi.
    replaySchemeInfo =
      schemeInfoFromRefSubst
        (siScheme schemeInfo)
        ( IntMap.mapWithKey
            normalizeMetadataOnlyRoute
            (siSubstRefs schemeInfo)
        )

    normalizeMetadataOnlyRoute nodeKey incomingRef =
      case IntMap.lookup nodeKey (grSourceBinderRefs requirements) of
        Just preparedRef
          | not
              ( any
                  (typeBinderRefsSameIdentity incomingRef)
                  occurrenceTypeRefs
              )
          , any
              (typeBinderRefsSameIdentity preparedRef)
              occurrenceTypeRefs ->
              preparedRef
        _ -> incomingRef

    occurrenceTypeRefs =
      occurrenceDeclarationRefs
        ++ freeTypeVarRefsType
          (schemeToType (siScheme schemeInfo))

    traceCopyDomainRoutes sourceBinderRefs =
      case mbTrace of
        Nothing -> []
        Just traceInfo ->
          [ (sourceAliasKey, sourceRef)
          | (traceSourceKey, sourceRef) <- IntMap.toList sourceBinderRefs
          , Just replayTarget <-
              [IntMap.lookup traceSourceKey (etBinderReplayMap traceInfo)]
          , (sourceAliasKey, copiedTarget) <-
              IntMap.toList (getCopyMapping (etCopyMap traceInfo))
          , copiedTarget == replayTarget
          ]

    insertOccurrenceRoute routes (nodeKey, incomingRef) =
      case IntMap.lookup nodeKey routes of
        Nothing -> pure (IntMap.insert nodeKey incomingRef routes)
        Just existingRef
          | typeBinderRefsSameIdentity existingRef incomingRef -> pure routes
          | Just constructionRef <- IntMap.lookup nodeKey constructionAliases
          , typeBinderRefsSameIdentity constructionRef incomingRef ->
              -- The occurrence scheme was constructed under this exact
              -- graph-node alias.  Enter the same quotient in the replay
              -- capability before Phi consumes it; retaining the pre-Gamma
              -- graph/source endpoint here would make one checked
              -- construction appear as two incompatible declarations.
              pure (IntMap.insert nodeKey constructionRef routes)
          | any
              (typeBinderRefsSameIdentity incomingRef)
              occurrenceDeclarationRefs ->
              -- A checked forall/mu binder is owned by the occurrence
              -- scheme.  Its substitution key may coincide with an ambient
              -- source route, but that key overlap is not authority to
              -- replace either declaration.  Keep the prepared ambient route
              -- and let the supplied SchemeInfo retain its local binder.
              pure routes
          | Just replayTarget <- traceReplayTargetFor nodeKey incomingRef ->
              -- The strict replay trace is the construction proof that this
              -- graph-local occurrence is the copied producer binder for the
              -- already prepared source identity.  Retain the prepared
              -- identity at the producer key and publish it at the exact
              -- replay key; the graph-local replay identity is not a second
              -- semantic declaration.
              insertPreparedRoute
                (getNodeId replayTarget)
                existingRef
                routes
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "occurrence replay source route conflicts with its prepared construction capability",
                      "  edge: " ++ show edgeId,
                      "  graph node: " ++ show (NodeId nodeKey),
                      "  prepared ref: " ++ show existingRef,
                      "  occurrence ref: " ++ show incomingRef,
                      "  construction alias: "
                        ++ show (IntMap.lookup nodeKey constructionAliases),
                      "  occurrence scheme: "
                        ++ show (schemeToType (siScheme schemeInfo))
                    ]
                )

    occurrenceDeclarationRefs =
      typeBinderDeclarationRefs
        (schemeToType (siScheme schemeInfo))

    traceReplayTargetFor nodeKey incomingRef = do
      traceInfo <- mbTrace
      replayTarget <-
        IntMap.lookup nodeKey (etBinderReplayMap traceInfo)
      incomingNode <- typeBinderRefNode incomingRef
      guard (incomingNode == replayTarget)
      pure replayTarget

    insertPreparedRoute nodeKey preparedRef routes =
      case IntMap.lookup nodeKey routes of
        Nothing -> pure (IntMap.insert nodeKey preparedRef routes)
        Just existingRef
          | typeBinderRefsSameIdentity existingRef preparedRef -> pure routes
          | otherwise ->
              Left
                ( ValidationFailed
                    [ "occurrence replay target conflicts with its prepared construction capability",
                      "  edge: " ++ show edgeId,
                      "  replay node: " ++ show (NodeId nodeKey),
                      "  prepared ref: " ++ show preparedRef,
                      "  existing ref: " ++ show existingRef
                    ]
                )

reifyInstWithSourceSchemeUsing ::
  GeneralizeAtWith p ->
  IntMap.IntMap TypeBinderRef ->
  AnnotationContext p ->
  IntSet.IntSet ->
  (IdDetails -> Maybe SchemeInfo) ->
  IntMap.IntMap ElabType ->
  Maybe SchemeInfo ->
  Maybe PhiEndpointShapeAuthority ->
  AnnExpr ->
  EdgeId ->
  Either ElabError Instantiation
reifyInstWithSourceSchemeUsing generalizeAtWith replaySourceBinderRefs annotationContext namedSetReify resolvedLookup frozenEndpointTypes sourceAuthority endpointShapeAuthority funAnn (EdgeId eid) =
  debugGeneralize
    ( "reifyInst: edge="
        ++ show eid
        ++ " witness="
        ++ show (IntMap.member eid edgeWitnesses)
        ++ " trace="
        ++ show (IntMap.member eid edgeTraces)
        ++ " exp="
        ++ show (IntMap.member eid edgeExpansions)
    )
    ()
    `seq` case IntMap.lookup eid edgeWitnesses of
      Nothing ->
        case debugGeneralize
          ("reifyInst: missing witness for edge " ++ show eid)
          () of
          ()
            | IntSet.member eid identityEdges ->
                Right InstId
            | otherwise ->
                Left
                  ( ValidationFailed
                      [ "missing edge witness for instantiation "
                          ++ show (EdgeId eid)
                          ++ " at "
                          ++ show funAnn
                      ]
                  )
      Just edgeWitness
        | exactIdentityEdge edgeWitness -> Right InstId
        | rigidRootTransitionEdge edgeWitness -> Right InstId
        | otherwise -> do
        let mTrace = IntMap.lookup eid edgeTraces
        mSchemeInfoRaw <-
          case sourceAuthority of
            Just sourceScheme -> pure (Just sourceScheme)
            Nothing ->
              schemeInfoForInstantiation
                annotationContext
                namedSetReify
                resolvedLookup
                funAnn
        mSchemeInfo <-
          case (mTrace, mSchemeInfoRaw) of
            (Just traceInfo, Just schemeInfo) ->
              case
                  strictReplayCheckedSchemeInfo
                    replaySourceBinderRefs
                    traceInfo
                    schemeInfo
                of
                  Left cause ->
                    Left
                      ( PhiInvariantError
                          ( unlines
                              [ "strict replay cannot align the checked occurrence scheme"
                              , "edge: " ++ show (EdgeId eid)
                              , "cause: " ++ cause
                              ]
                          )
                      )
                  Right aligned -> pure (Just aligned)
            _ -> pure mSchemeInfoRaw
        let
            mExpansion = IntMap.lookup eid edgeExpansions
            mTraceArgs =
              case (mTrace, mSchemeInfo) of
                (Just traceInfo, Just schemeInfo)
                  | not (null (etBinderArgs traceInfo)) ->
                      reifyTraceBinderInstArgs
                        namedSetReify
                        schemeInfo
                        traceInfo
                _ -> Nothing
            mExpansionInst =
              case (mExpansion, mSchemeInfo, mTrace) of
                (Just (ExpInstantiate args), Just schemeInfo, Just traceInfo) ->
                  case
                    fullValidExpansionInstFor
                      namedSetReify
                      schemeInfo
                      traceInfo
                      frozenEndpointTypes
                      args
                  of
                    Left _ -> Nothing
                    Right inst -> Just inst
                _ -> Nothing
            frozenGraftOperandsAuthorizeWitnessReplay =
              case mExpansion of
                Just (ExpInstantiate args) ->
                  not (null args)
                    && args
                      == [ operated
                         | OpGraft operated _ <-
                            getInstanceOps (ewWitness edgeWitness)
                         ]
                    && all
                      (\node -> IntMap.member (getNodeId node) frozenEndpointTypes)
                      args
                _ -> False
        case debugGeneralize
          ( "reifyInst scheme edge="
              ++ show eid
              ++ " source="
              ++ show (fmap (schemeToType . siScheme) mSchemeInfo)
              ++ " subst="
              ++ show (fmap schemeInfoBinderRefSubst mSchemeInfo)
              ++ " expansionInst="
              ++ show mExpansionInst
          )
          () of
          () -> pure ()
        phi0 <-
          case mExpansionInst of
            Just expansionInst
              | not frozenGraftOperandsAuthorizeWitnessReplay ->
                  pure expansionInst
            _ ->
              case phiForOccurrenceEndpoint of
                Right phi0' -> pure phi0'
                Left err -> Left (edgeContextError err)
              where
                phiForOccurrenceEndpoint =
                  case endpointShapeAuthority of
                    Nothing ->
                      phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
                        traceCfg
                        generalizeAtWith
                        (scReadModel scopeContext)
                        (Just gaParents)
                        mSchemeInfo
                        frozenEndpointTypes
                        mTrace
                        edgeWitness
                    Just authority ->
                      phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor
                        traceCfg
                        generalizeAtWith
                        (scReadModel scopeContext)
                        (Just gaParents)
                        mSchemeInfo
                        frozenEndpointTypes
                        authority
                        mTrace
                        edgeWitness
        let substForPhi = maybe IntMap.empty schemeInfoBinderRefSubst mSchemeInfo
            resolvePhiVar ref = do
              nid <- typeBinderRefNode ref
              bnd <- pvLookupVarBound presolutionView (canonical nid)
              either
                (const Nothing)
                Just
                (reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) substForPhi namedSetReify bnd)
            normalizePhiInst inst0 = case inst0 of
              InstApp (TVarRef ref) -> maybe inst0 InstApp (resolvePhiVar ref)
              InstBot (TVarRef ref) -> maybe inst0 InstBot (resolvePhiVar ref)
              _ -> inst0
            phi = normalizePhiInst phi0
        case debugGeneralize
          ("reifyInst phi edge=" ++ show eid ++ " phi=" ++ show phi)
          () of
          () -> pure ()
        instFromAuthority <-
          if frozenGraftOperandsAuthorizeWitnessReplay
            then
              -- ExpInstantiate names only the operated operands of OpGraft.
              -- Once sibling occurrences have frozen every one of those
              -- endpoints, the witness translation owns the complete
              -- computation, including any following Merge/Raise operation.
              -- Replacing it with the expansion applications would discard
              -- that residual T(e) work.
              pure Nothing
            else
              case (mExpansion, mSchemeInfo) of
                (Just (ExpInstantiate args), Just schemeInfo) -> do
                  let schemeArity =
                        length (schemeBinderRefs (siScheme schemeInfo))
                      targetTy = authoritativeTargetType namedSetReify edgeWitness schemeInfo
                      traceArgs = mTraceArgs
                      expansionInstResult =
                        mTrace >>= \traceInfo ->
                          pure
                          ( fullValidExpansionInstFor
                              namedSetReify
                              schemeInfo
                              traceInfo
                              frozenEndpointTypes
                              args
                          )
                      expansionInst =
                        expansionInstResult >>= either (const Nothing) Just
                      expansionArgs =
                        expansionInst >>= collectApps >>= \appArgs ->
                          pure (schemeBinderRefs (siScheme schemeInfo), appArgs)
                      targetArgs =
                        if schemeArity == 0
                          then Nothing
                          else inferAuthoritativeInstArgs namedSetReify edgeWitness schemeInfo
                      needsExpansionAuthority =
                        instNeedsAuthoritativeRefinement phi
                          || case collectApps phi of
                            Just phiArgs ->
                              length phiArgs < schemeArity
                            Nothing -> False
                      -- The environment scheme can be a reduced presentation of
                      -- the graph source (see Note [Environment source schemes
                      -- can differ from witness roots]).  In that case replay and
                      -- expansion arguments still live in the graph source
                      -- domain, while the edge destination is the construction
                      -- authority for transporting the computation to the
                      -- environment scheme.  Prefer the exact destination
                      -- matching before consulting graph-domain arguments.
                      authoritativeArgs
                        | not (IntMap.null frozenEndpointTypes) =
                            expansionArgs <|> targetArgs <|> traceArgs
                        | otherwise =
                            targetArgs <|> traceArgs <|> expansionArgs
                      shouldRefine =
                        needsExpansionAuthority
                          || case targetTy of
                            Just ty -> not (alphaEqType ty (schemeToType (siScheme schemeInfo)))
                            Nothing -> phi == InstId
                      schemeTy = schemeToType (siScheme schemeInfo)
                  case debugGeneralize
                    ( "reifyInst authoritative edge="
                        ++ show eid
                        ++ " expansionArity="
                        ++ show (length args)
                        ++ " schemeArity="
                        ++ show schemeArity
                        ++ " targetTy="
                        ++ show targetTy
                        ++ " targetArgs="
                        ++ show targetArgs
                        ++ " traceArgs="
                        ++ show traceArgs
                        ++ " fullValidExpansionArgs="
                        ++ show expansionInst
                        ++ " shouldRefine="
                        ++ show shouldRefine
                    )
                    () of
                    () -> pure ()
                  case authoritativeArgs of
                    Just (_binds, inferred)
                      | shouldRefine,
                        schemeArity > 0,
                        length inferred == schemeArity,
                        Right _ <- applyInstantiation schemeTy (instSeqApps inferred) ->
                          pure (Just (instSeqApps inferred))
                    _
                      | needsExpansionAuthority,
                        schemeArity > 0 ->
                          case expansionInst of
                            Just validExpansionInst -> pure (Just validExpansionInst)
                            Nothing ->
                              Left
                                ( PhiTranslatabilityError
                                    [ "reifyInst: missing authoritative instantiation translation for edge " ++ show eid,
                                      "expansion args=" ++ show args,
                                      "scheme=" ++ show schemeTy,
                                      "raw phi=" ++ show phi,
                                      "trace args=" ++ show (fmap snd traceArgs),
                                      "target args=" ++ show (fmap snd targetArgs),
                                      "expansion authority=" ++ show expansionInstResult
                                    ]
                                )
                    _
                      | shouldRefine,
                        schemeArity > 0 ->
                          pure Nothing
                    _ -> pure Nothing
                _ -> pure Nothing
        case instFromAuthority of
          Just inst -> Right inst
          Nothing -> Right phi
  where
    traceCfg = acTraceConfig annotationContext
    scopeContext = acScopeContext annotationContext
    presolutionView = scPresolutionView scopeContext
    gaParents = scGaParents scopeContext
    edgeWitnesses = acEdgeWitnesses annotationContext
    edgeTraces = acEdgeTraces annotationContext
    edgeExpansions = acEdgeExpansions annotationContext
    identityEdges = acIdentityEdges annotationContext
    canonical = pvCanonical presolutionView
    debugGeneralize :: String -> a -> a
    debugGeneralize = traceElab traceCfg

    -- Figure 15.3.4 gives Tχ() = ε.  ExpIdentity plus the normalized empty
    -- witness/trace is the producer-owned proof that this edge is precisely
    -- that case.  In particular, do not run Σ over a scheme prepared at an
    -- enclosing subterm boundary: those binders are Γ, not work performed by
    -- this edge.
    exactIdentityEdge edgeWitness =
      ewLeft edgeWitness == ewRight edgeWitness
        && ewForallIntros edgeWitness == 0
        && null (getInstanceOps (ewWitness edgeWitness))
        && IntMap.lookup eid edgeExpansions == Just ExpIdentity
        && case IntMap.lookup eid edgeTraces of
          Just traceInfo ->
            null (etBinderArgs traceInfo)
              && IntMap.null (etBinderReplayMap traceInfo)
              && null (etReplayDomainBinders traceInfo)
              && IntMap.null (getCopyMapping (etCopyMap traceInfo))
              && etReplayContract traceInfo == ReplayContractNone
          Nothing -> False

    -- Lemma 11.5.3 constructs a flex-to-rigid terminal transition as
    -- Weaken(r); RaiseMerge(r,d).  Figure 15.3.4 translates both rigid
    -- operations to the identity, so a later arity-based refinement must not
    -- manufacture a type application for binders owned by the enclosing Γ.
    rigidRootTransitionEdge edgeWitness =
      case (getInstanceOps (ewWitness edgeWitness), IntMap.lookup eid edgeTraces) of
        ( [OpWeaken weakened, OpRaiseMerge operated exterior],
          Just traceInfo
          ) ->
            weakened == operated
              && rootWeakenRaiseMergeTraceAuthority operated exterior traceInfo
        _ -> False

    edgeContextError err =
      case err of
        BindingTreeError (InvalidBindingTree message) ->
          BindingTreeError
            ( InvalidBindingTree
                ( "reifyInst edge "
                    ++ show (EdgeId eid)
                    ++ " at "
                    ++ show funAnn
                    ++ ": "
                    ++ message
                )
            )
        _ -> err

    inferAuthoritativeInstArgs namedSet schemeInfoWitness schemeInfo =
      inferFromNode (ewRight schemeInfoWitness)
      where
        inferFromNode nodeId =
          case
            [ inferred
              | targetTy <- targetTypes nodeId,
                Just inferred <- [inferAgainstTarget targetTy]
            ]
            of
            [] -> Nothing
            first : rest -> Just (foldl preferLonger first rest)
        preferLonger current@(_, currentArgs) candidate@(_, candidateArgs)
          | length candidateArgs > length currentArgs = candidate
          | otherwise = current
        targetTypes nodeId =
          [ inlineBoundVarsTypeWithContext
              (scInlineBoundVarsContext scopeContext)
              ty
            | Right ty <-
                [ reifyTargetType scopeContext namedSet schemeInfo nodeId,
                  reifyTargetNodeType scopeContext namedSet schemeInfo nodeId,
                  reifyNodeTypePreferringBound scopeContext nodeId
                ]
          ]
        inferAgainstTarget targetTy =
          let binds = schemeBinderRefs (siScheme schemeInfo)
              body = schemeBody (siScheme schemeInfo)
              schemeTy = schemeToType (siScheme schemeInfo)
              targetHasVisibleForall = case targetTy of
                TForallRef {} -> True
                _ -> False
              isInternalTypeBinderRef ref =
                isJust (typeBinderRefNode ref)
              inferIdentityLikeTarget =
                case (binds, body) of
                  ([(binderRef, _)], TArrow (TVarRef domRef) (TVarRef codRef))
                    | typeBinderRefsSameIdentity binderRef domRef && typeBinderRefsSameIdentity binderRef codRef ->
                        let args = [TVarRef binderRef]
                         in case applyInstantiation schemeTy (instSeqApps args) of
                              Right tyApplied
                                | alphaEqType tyApplied targetTy ->
                                    Just args
                              _ -> Nothing
                  _ -> Nothing
              normalizeArgs inferred =
                let rewrite prefix remainingBinds remainingArgs =
                      case (remainingBinds, remainingArgs) of
                        ((binderRef, _) : restBinds, argTy : restArgs) ->
                          let normalizedArg =
                                case argTy of
                                  TVarRef argRef
                                    | targetHasVisibleForall,
                                      isInternalTypeBinderRef argRef ->
                                        let candidateArgs = prefix ++ [TVarRef binderRef] ++ restArgs
                                         in case applyInstantiation schemeTy (instSeqApps candidateArgs) of
                                              Right tyApplied
                                                | alphaEqType tyApplied targetTy ->
                                                    TVarRef binderRef
                                              _ -> argTy
                                  _ -> argTy
                           in normalizedArg : rewrite (prefix ++ [normalizedArg]) restBinds restArgs
                        (_, []) -> []
                        ([], restArgs) -> restArgs
                 in rewrite [] binds inferred
              inferredArgs =
                fmap
                  normalizeArgs
                  ( inferInstAppArgsFromSchemeRefs
                      (schemeBinderRefs (siScheme schemeInfo))
                      (schemeBody (siScheme schemeInfo))
                      targetTy
                  )
                  <|> inferIdentityLikeTarget
           in fmap ((,) binds) inferredArgs

    authoritativeTargetType namedSet edgeWitness schemeInfo =
      find (alphaEqType sourceSchemeTy) candidates <|> listToMaybe candidates
      where
        sourceSchemeTy = schemeToType (siScheme schemeInfo)
        candidates = targetTypes (ewRight edgeWitness)
        targetTypes nodeId =
          let frozenTy = IntMap.lookup (getNodeId nodeId) frozenEndpointTypes
              directTy = either (const Nothing) Just (reifyTargetType scopeContext namedSet schemeInfo nodeId)
              nodeTy = either (const Nothing) Just (reifyTargetNodeType scopeContext namedSet schemeInfo nodeId)
              boundTy = either (const Nothing) Just (reifyNodeTypePreferringBound scopeContext nodeId)
           in case directTy of
                Just TVarRef {} ->
                  concatMap maybeToList [frozenTy, boundTy, directTy, nodeTy]
                _ -> concatMap maybeToList [frozenTy, directTy, nodeTy, boundTy]

    reifyTraceBinderInstArgs namedSet schemeInfo traceInfo = do
      if length schemeBinders == length traceSources
        then pure ()
        else Nothing
      alignedArgs <- mapM alignedArgFor schemeBinders
      pure (schemeBinders, alignedArgs)
      where
        schemeBinders = schemeBinderRefs (siScheme schemeInfo)
        subst = schemeInfoBinderRefSubst schemeInfo
        replayMap = etBinderReplayMap traceInfo
        traceSources = uniqueTraceSources (etBinderArgs traceInfo)
        uniqueTraceSources = foldl' insertUnique []
        insertUnique sources pair@(sourceNode, _)
          | any (samePreparedNode sourceNode . fst) sources = sources
          | otherwise = sources ++ [pair]
        samePreparedNode left right =
          left == right || canonical left == canonical right
        alignedArgFor (binderRef, _) = do
          binderNode <- typeBinderRefNode binderRef
          (_, argNode) <-
            find
              (sourceMatchesBinder binderNode . fst)
              traceSources
          reifyArg argNode
        sourceMatchesBinder binderNode sourceNode =
          samePreparedNode sourceNode binderNode
            || case IntMap.lookup (getNodeId sourceNode) replayMap of
              Just replayNode -> samePreparedNode replayNode binderNode
              Nothing -> False
        reifyArg nodeId =
          case IntMap.lookup (getNodeId nodeId) frozenEndpointTypes of
            Just exactEndpoint -> Just exactEndpoint
            Nothing ->
              let nodeC = canonical nodeId
                  tyE =
                    case pvLookupVarBound presolutionView nodeC of
                      Just bnd -> reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) subst namedSet bnd
                      Nothing -> reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) subst namedSet nodeC
               in either (const Nothing) Just tyE

    instNeedsAuthoritativeRefinement inst =
      case collectApps inst of
        Just tys -> any isPlaceholderTy tys
        Nothing -> False

    isPlaceholderTy ty = case ty of
      TVarRef _ -> True
      _ -> False

    collectApps inner = case inner of
      InstId -> Just []
      InstApp ty -> Just [ty]
      InstSeq a b -> (++) <$> collectApps a <*> collectApps b
      _ -> Nothing

    fullValidExpansionInstFor namedSet schemeInfo traceInfo exactEndpoints nodeArgs = do
      schemeNodeArgs <- expansionArgsForScheme
      case expInstantiateArgsToInstNoFallback scopeContext namedSet exactEndpoints schemeNodeArgs of
        Right inst
          | Just appArgs <- collectApps inst
          , length appArgs == schemeArity
          , Right _ <- applyInstantiation schemeTy inst -> Right inst
        _ -> aliasBridge schemeNodeArgs
      where
        schemeTy = schemeToType (siScheme schemeInfo)
        schemeBinders = schemeBinderRefs (siScheme schemeInfo)
        schemeArity = length schemeBinders
        subst = schemeInfoBinderRefSubst schemeInfo
        traceArgs = etBinderArgs traceInfo
        replayMap = etBinderReplayMap traceInfo
        replayDomain = etReplayDomainBinders traceInfo
        copyMap = getCopyMapping (etCopyMap traceInfo)

        -- Expansion records the frozen graph-source quantifier domain.  The
        -- checked occurrence may expose a smaller, quotient-preserving source
        -- scheme (for example @apply@ has two source quantifiers while its
        -- graph occurrence also carries a result placeholder).  Project by
        -- the trace's exact binder identities; positional truncation would
        -- silently attach an argument to the wrong producer quantifier.
        expansionArgsForScheme
          | length nodeArgs == schemeArity = pure nodeArgs
          | otherwise = do
              if length traceArgs == length nodeArgs
                then pure ()
                else
                  Left
                    ( "expansion/trace arity mismatch: "
                        ++ show (length nodeArgs, length traceArgs)
                    )
              traverse expansionArgFor schemeBinders

        expansionArgFor (binderRef, _) =
          case
            [ expansionArg
            | ((sourceNode, _), expansionArg) <- zip traceArgs nodeArgs
            , sourceMatchesRef binderRef sourceNode
            ]
          of
            [expansionArg] -> Right expansionArg
            matches ->
              Left
                ( "expected one expansion argument for source ref "
                    ++ show binderRef
                    ++ ", got "
                    ++ show matches
                )

        aliasBridge schemeNodeArgs = do
          if etReplayContract traceInfo == ReplayContractStrict
            then pure ()
            else
              Left
                ( "alias bridge requires strict replay authority, got "
                    ++ show (etReplayContract traceInfo)
                )
          steps <- mapM stepFor (zip schemeBinders schemeNodeArgs)
          let candidate = normalizeInst (sequenceInstantiations steps)
          _ <-
            either
              (Left . ("constructed alias bridge is ill-typed: " ++) . show)
              Right
              (applyInstantiation schemeTy candidate)
          pure candidate

        stepFor ((binderRef, mbBound), expansionArg) = do
          (sourceNode, traceArg) <- uniqueTraceBinderFor binderRef
          if canonical traceArg == canonical expansionArg
            then pure ()
            else
              Left
                ( "trace argument does not match expansion argument: "
                    ++ show (traceArg, expansionArg)
                )
          copiedNode <- uniqueCopyFor sourceNode
          replayNode <-
            maybe
              (Left ("missing replay-map source " ++ show sourceNode))
              Right
              (IntMap.lookup (getNodeId sourceNode) replayMap)
          if canonical replayNode == canonical copiedNode
            && any ((== canonical copiedNode) . canonical) replayDomain
            then pure ()
            else
              Left
                ( "copy/replay authority mismatch: "
                    ++ show (sourceNode, copiedNode, replayNode, replayDomain)
                )
          case NodeAccess.lookupVarBound (pvConstraint presolutionView) copiedNode of
            Just finalBound
              | canonical finalBound == canonical expansionArg -> pure ()
              | otherwise ->
                  Left
                    ( "copied binder final bound does not match expansion argument: "
                        ++ show (copiedNode, finalBound, expansionArg)
                    )
            Nothing ->
              case NodeAccess.lookupNode (pvConstraint presolutionView) copiedNode of
                Nothing -> pure ()
                Just rawNode ->
                  Left
                    ( "live copied binder has no final bound: "
                        ++ show (copiedNode, rawNode)
                    )
          argTy <-
            case IntMap.lookup (getNodeId expansionArg) exactEndpoints of
              Just exactEndpoint -> pure exactEndpoint
              Nothing ->
                either
                  (Left . ("cannot reify expansion argument: " ++) . show)
                  Right
                  ( reifyTypeWithNamedSetRefsNoFallbackReadModel
                      (scReadModel scopeContext)
                      subst
                      namedSet
                      (canonical expansionArg)
                  )
          case mbBound of
            Nothing -> pure (InstApp argTy)
            Just sourceBound -> do
              boundComputation <-
                case argTy of
                  TVarRef argRef -> pure (instAbstrWithRef argRef)
                  _ ->
                    maybe
                      ( Left
                          ( "cannot construct bound computation: "
                              ++ show (tyToElab sourceBound, argTy)
                          )
                      )
                      Right
                      (inferPreservingAnnotationInst (tyToElab sourceBound) argTy)
              pure (InstSeq (InstInside boundComputation) InstElim)

        uniqueTraceBinderFor binderRef =
          case
            [ pair
            | pair@(sourceNode, _) <- traceArgs
            , sourceMatchesRef binderRef sourceNode
            ]
          of
            [pair] -> Right pair
            pairs ->
              Left
                ( "expected one trace binder for source ref "
                    ++ show binderRef
                    ++ ", got "
                    ++ show pairs
                )

        sourceMatchesRef binderRef sourceNode =
          any
            (\candidate ->
              canonical candidate == canonical sourceNode
                || copiedRepresentativesAgree candidate sourceNode
            )
            (binderSourceCandidates binderRef)

        copiedRepresentativesAgree left right =
          case
            ( IntMap.lookup (getNodeId left) copyMap
            , IntMap.lookup (getNodeId right) copyMap
            )
          of
            (Just leftCopy, Just rightCopy) ->
              canonical leftCopy == canonical rightCopy
            _ -> False

        binderSourceCandidates binderRef =
          dedupeCanonical
            ( maybeToList (typeBinderRefNode binderRef)
                ++ [ NodeId key
                   | (key, substRef) <- IntMap.toList subst
                   , typeBinderRefsSameIdentity binderRef substRef
                   ]
            )

        uniqueCopyFor sourceNode =
          case
            dedupeCanonical
              [ copiedNode
              | (sourceKey, copiedNode) <- IntMap.toList copyMap
              , canonical (NodeId sourceKey) == canonical sourceNode
              ]
          of
            [copiedNode] -> Right copiedNode
            copiedNodes ->
              Left
                ( "expected one copied binder for source "
                    ++ show sourceNode
                    ++ ", got "
                    ++ show copiedNodes
                )

        dedupeCanonical = foldl' insertCanonical []
        insertCanonical nodes node
          | any ((== canonical node) . canonical) nodes = nodes
          | otherwise = nodes ++ [node]

        sequenceInstantiations instantiations =
          case instantiations of
            [] -> InstId
            [inst] -> inst
            inst : rest -> InstSeq inst (sequenceInstantiations rest)

instSeqApps :: [ElabType] -> Instantiation
instSeqApps tys =
  case map InstApp tys of
    [] -> InstId
    [inst] -> inst
    insts -> foldr1 InstSeq insts

annRefersToVar :: BindingKey -> AnnExpr -> Bool
annRefersToVar key exprAnn =
  annExprReferenceKey exprAnn == Just key

freshenTermTypeAbsAgainstEnv :: TypeCheck.Env -> XmlfTerm -> XmlfTerm
freshenTermTypeAbsAgainstEnv env = go reserved
  where
    reserved =
      Set.unions
        ( map freeTypeVarAliasNamesType (map snd (TypeCheck.resolvedTermEnvEntries (TypeCheck.resolvedTermEnv env)))
            ++ [typeVarRefAliasNames (Map.keys (TypeCheck.typeEnv env))]
        )

    go used term = case term of
      ETyAbsRef ref mb body ->
        let name = typeBinderRefName ref
            usedForBinder = Set.union used (maybe Set.empty freeTypeVarAliasNamesType mb)
            (ref', body') =
              if Set.member name usedForBinder
                then
                  let fresh = freshNameLike name usedForBinder
                      freshRef = renameTypeBinderRef fresh ref
                   in (freshRef, renameTypeVarInTerm ref freshRef body)
                else (ref, body)
            used' = typeBinderRefAliasNames ref' `Set.union` usedForBinder
         in ETyAbsRef ref' mb (go used' body')
      ELam resolved body ->
        ELam resolved (go (Set.union used (freeTypeVarAliasNamesType (resolvedVarType resolved))) body)
      EApp f a -> EApp (go used f) (go used a)
      ELet resolved sch rhs body ->
        let used' = Set.union used (freeTypeVarAliasNamesType (schemeToType sch))
         in ELet resolved sch (go used' rhs) (go used' body)
      ETyInst t inst -> ETyInst (go used t) inst
      ERoll ty body -> ERoll ty (go used body)
      EUnroll body -> EUnroll (go used body)
      _ -> term

typeVarRefAliasNames :: [TypeBinderRef] -> Set.Set String
typeVarRefAliasNames =
  Set.unions . map typeBinderRefAliasNames

renameTypeVarInTerm :: TypeBinderRef -> TypeBinderRef -> XmlfTerm -> XmlfTerm
renameTypeVarInTerm oldRef newRef term =
  let renameTy = substTypeCaptureRef oldRef (TVarRef newRef)
      renameBound = mapBoundType renameTy
      renameScheme sch = schemeFromType (renameTy (schemeToType sch))
      renameRef ref
        | typeBinderRefsSameIdentity ref oldRef = newRef
        | otherwise = ref
      renameInst inst = case project inst of
        InstIdF -> InstId
        InstAppF ty -> InstApp (renameTy ty)
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstInsideF inner -> InstInside (renameInst inner)
        InstSeqF a b -> InstSeq (renameInst a) (renameInst b)
        InstUnderFRef ref inner -> instUnderWithRef (renameRef ref) (renameInst inner)
        InstBotF ty -> InstBot (renameTy ty)
        InstAbstrFRef ref -> instAbstrWithRef (renameRef ref)
   in case project term of
        EVarNodeF resolved -> EVarNode (mapResolvedVarType renameTy resolved)
        ELitF lit -> ELit lit
        ELamF resolved body ->
          ELam (mapResolvedVarType renameTy resolved) (renameTypeVarInTerm oldRef newRef body)
        EAppF f a -> EApp (renameTypeVarInTerm oldRef newRef f) (renameTypeVarInTerm oldRef newRef a)
        ELetF resolved sch rhs body ->
          ELet
            (mapResolvedVarType renameTy resolved)
            (renameScheme sch)
            (renameTypeVarInTerm oldRef newRef rhs)
            (renameTypeVarInTerm oldRef newRef body)
        ETyAbsFRef ref mb body
          | typeBinderRefsSameIdentity ref oldRef -> eTyAbsWithRef ref (fmap renameBound mb) body
          | otherwise -> eTyAbsWithRef ref (fmap renameBound mb) (renameTypeVarInTerm oldRef newRef body)
        ETyInstF t inst -> ETyInst (renameTypeVarInTerm oldRef newRef t) (renameInst inst)
        ERollF ty body -> ERoll (renameTy ty) (renameTypeVarInTerm oldRef newRef body)
        EUnrollF body -> EUnroll (renameTypeVarInTerm oldRef newRef body)

-- | Convert a normalized source annotation while preserving the semantic
-- identities chosen by source resolution.  Preparation uses the same helper
-- as term elaboration so an expected type cannot silently acquire a second
-- spelling-derived binder identity.
sourceTypeToElabTypeWithIdentities
  :: Map.Map String SymbolIdentity
  -> Map.Map String TypeBinderIdentity
  -> NormSrcType
  -> Either ElabError ElabType
sourceTypeToElabTypeWithIdentities headIdentities binderIdentities ty =
  fmap fst
    ( sourceTypeToElabTypeWithIdentitiesFromSupply
        (identityGeneratorAfter [])
        headIdentities
        binderIdentities
        ty
    )

-- | Convert a normalized source annotation by allocating every source-local
-- binder from the caller's identity supply.  Annotation conversion happens
-- immediately before packet preparation, so returning the advanced supply is
-- part of the construction contract: preparation must not reuse an identity
-- consumed by a lexical @forall@ or @mu@ here.
sourceTypeToElabTypeWithIdentitiesFromSupply
  :: IdentityGenerator
  -> Map.Map String SymbolIdentity
  -> Map.Map String TypeBinderIdentity
  -> NormSrcType
  -> Either ElabError (ElabType, IdentityGenerator)
sourceTypeToElabTypeWithIdentitiesFromSupply generator0 headIdentities binderIdentities ty =
  let generator1 =
        advanceSourceTypeIdentityGeneratorPast
          headIdentities
          binderIdentities
          ty
          generator0
      (refs, generator2) =
        sourceTypeBinderRefsFromIdentities
          binderIdentities
          (Set.toList (freeSrcTypeVars ty))
          generator1
   in srcTypeToElabTypeWith headIdentities binderIdentities refs generator2 ty

advanceSourceTypeIdentityGeneratorPast
  :: Map.Map String SymbolIdentity
  -> Map.Map String TypeBinderIdentity
  -> NormSrcType
  -> IdentityGenerator
  -> IdentityGenerator
advanceSourceTypeIdentityGeneratorPast sourceHeadIdentities sourceBinderIdentities ty =
  advanceIdentityGeneratorPastMany
    ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
        ++ concatMap typeBinderGeneratedIdentities (Map.elems sourceBinderIdentities)
    )
  where
    headIdentities =
      Map.union
        sourceHeadIdentities
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

requireSourceTypeHeadIdentity :: Map.Map String SymbolIdentity -> String -> Either ElabError SymbolIdentity
requireSourceTypeHeadIdentity headIdentities name =
  case lookupSymbolIdentityAlias headIdentities name <|> Builtins.builtinTypeHeadIdentity name of
    Just identity -> Right identity
    Nothing -> Left (InstantiationError ("unresolved source type head `" ++ name ++ "` reached annotation elaboration"))

srcTypeToElabTypeWith :: Map.Map String SymbolIdentity -> Map.Map String TypeBinderIdentity -> Map.Map String TypeBinderRef -> IdentityGenerator -> NormSrcType -> Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWith =
  srcTypeToElabTypeWithBound Set.empty

srcTypeToElabTypeWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator ty = case ty of
  STVar name -> do
    ref <- sourceTypeBinderRef refs name
    Right (TVarRef ref, generator)
  STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator1 cod
    Right (TArrow dom' cod', generator2)
  STCon name args -> do
    (args', generator') <- srcTypesToElabTypesWith boundNames refs generator args
    identity <- requireSourceTypeHeadIdentity headIdentities name
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
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator2 body
          Right (TForallRef ref mb' body', generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        boundNames' = Set.insert name boundNames
     in do
          (body', generator2) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities (Map.insert name ref refs) generator1 body
          Right (TMuRef ref body', generator2)
  STBase name -> do
    identity <- requireSourceTypeHeadIdentity headIdentities name
    Right (TBaseWithIdentity identity (builtinBaseTy name), generator)
  STBottom -> Right (TBottom, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InstantiationError ("unresolved source type binder `" ++ name ++ "` reached annotation elaboration"))

    srcTypesToElabTypesWith boundNames' refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

srcBoundToElabBoundWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  SrcBound 'NormN ->
  Either ElabError (Maybe BoundType, IdentityGenerator)
srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator bound = case bound of
  SrcBound ty -> structBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator ty

structBoundToElabBoundWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  StructBound ->
  Either ElabError (Maybe BoundType, IdentityGenerator)
structBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator bTy = case bTy of
  STArrow dom cod -> do
    (dom', generator1) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator dom
    (cod', generator2) <- srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator1 cod
    Right (Just (TArrow dom' cod'), generator2)
  STBase name -> do
    identity <- requireSourceTypeHeadIdentity headIdentities name
    Right (Just (TBaseWithIdentity identity (builtinBaseTy name)), generator)
  STCon name args -> do
    (args', generator1) <- srcTypesToElabTypesWith refs generator args
    identity <- requireSourceTypeHeadIdentity headIdentities name
    Right (Just (TConWithIdentity identity (builtinBaseTy name) args'), generator1)
  STVarApp name args -> do
    (args', generator1) <- srcTypesToElabTypesWith refs generator args
    ref <- sourceTypeBinderRef refs name
    Right (Just (TVarAppRef ref args'), generator1)
  STTyLam {} ->
    Left (InstantiationError "residual type lambda reached elaboration")
  STTyApp {} ->
    Left (InstantiationError "residual type application reached elaboration")
  STForall name mb body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        refs' = Map.insert name ref refs
        boundNames' = Set.insert name boundNames
     in do
          (mb', generator2) <- maybe (Right (Nothing, generator1)) (srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator1) mb
          (body', generator3) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities refs' generator2 body
          Right (Just (TForallRef ref mb' body'), generator3)
  STMu name body ->
    let (ref, generator1) = sourceTypeBinderRefOrFreshInScope (Set.member name boundNames) binderIdentities name generator
        boundNames' = Set.insert name boundNames
     in do
      (body', generator2) <- srcTypeToElabTypeWithBound boundNames' headIdentities binderIdentities (Map.insert name ref refs) generator1 body
      Right (Just (TMuRef ref body'), generator2)
  STBottom -> Right (Nothing, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing -> Left (InstantiationError ("unresolved source type binder `" ++ name ++ "` reached annotation elaboration"))

    srcTypesToElabTypesWith refs0 generator0 (arg :| args) = do
      (arg', generator1) <- srcTypeToElabTypeWith headIdentities binderIdentities refs0 generator0 arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <- srcTypeToElabTypeWith headIdentities binderIdentities refs0 gen next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

builtinBaseTy :: String -> BaseTy
builtinBaseTy =
  BaseTy . Builtins.normalizeBuiltinTypeReference
