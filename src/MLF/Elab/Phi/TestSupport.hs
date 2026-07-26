module MLF.Elab.Phi.TestSupport (
    VSpine,
    mkVSpine,
    reorderSpineRefsTo,
    orderPhiBindersByPrecForTest,
    assertSpineSync,
    vSpineBinderAt,
    vSpineBinderRefs,
    vSpineNameAt,
    normalizeInst,
    phiFromEdgeWitnessWithTraceForTest,
    phiOccurrenceFromEdgeWitnessWithTraceForTest,
    reifyInstWithSourceScheme,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import MLF.Constraint.Presolution (EdgeTrace)
import MLF.Constraint.Presolution.Base
    ( EdgeArtifacts(..)
    , PresolutionPlanBuilder
    )
import MLF.Constraint.Presolution.Plan.Context
    ( GaBindParents(..)
    , emptyExpansionConstructionPlacements
    )
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Types.Graph
    ( Constraint(..)
    , EdgeId
    , NodeId(..)
    , NodeRef
    , getEdgeId
    , getNodeId
    , toListNode
    )
import MLF.Constraint.Types.Witness (EdgeWitness, ewEdgeId)
import MLF.Elab.Elaborate.Annotation
    ( AnnotationContext(..)
    , reifyInstFromSourceScheme
    )
import MLF.Elab.Elaborate.Scope (ScopeContext(..))
import MLF.Elab.ReadModel (buildElabReadModel, ermNamedNodes)
import MLF.Elab.Run.Generalize
    ( generalizeAtWithBuilder
    , generalizeAtWithBuilderRequired
    , generalizeAtWithBuilderRequiredResultCertified
    )
import MLF.Elab.Run.TypeOps (mkInlineBoundVarsContextWithReadModel)
import qualified MLF.Elab.Sigma as Sigma
import MLF.Elab.Types
    ( BoundType
    , ElabScheme
    , ElabError(..)
    , Instantiation
    , SchemeInfo
    , TypeBinderRef
    )
import MLF.Elab.Phi.Omega.Normalize (normalizeInst)
import MLF.Elab.Phi.Omega.Interpret.Internal
    ( orderPhiBindersByPrec
    )
import MLF.Elab.Phi.Computation (OccurrenceComputation)
import qualified MLF.Elab.Phi.Translate as Translate
import MLF.Elab.Phi.VSpine
    ( VSpine
    , assertSpineSync
    , mkVSpine
    , vSpineBinderAt
    , vSpineBinderRefs
    , vSpineNameAt
    )
import MLF.Frontend.ConstraintGen.Types (AnnExpr)
import MLF.Util.Trace (TraceConfig)
import qualified MLF.Util.Order as Order

reorderSpineRefsTo
    :: Eq a
    => String
    -> [(TypeBinderRef, Maybe BoundType)]
    -> [a]
    -> [a]
    -> Either ElabError (Instantiation, [(TypeBinderRef, Maybe BoundType)], [a])
reorderSpineRefsTo = Sigma.bubbleReorderToFromSpineRefs

orderPhiBindersByPrecForTest
    :: IntMap.IntMap Order.OrderKey
    -> IntSet.IntSet
    -> [(TypeBinderRef, Maybe BoundType, Maybe NodeId)]
    -> Either ElabError [Maybe NodeId]
orderPhiBindersByPrecForTest orderKeys orderedBinderKeys =
    orderPhiBindersByPrec
        id
        (\nodeId -> IntSet.member (getNodeId nodeId) orderedBinderKeys)
        orderKeys

phiFromEdgeWitnessWithTraceForTest
    :: TraceConfig
    -> (Maybe (GaBindParents p) -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef))
    -> PresolutionView p
    -> Maybe (GaBindParents p)
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTraceForTest traceCfg generalizeAt presolutionView mbGaParents mSchemeInfo mTrace witness = do
    traceInfo <-
        case mTrace of
            Just trace -> Right trace
            Nothing -> Left (MissingEdgeTrace (ewEdgeId witness))
    replay <-
        Translate.mkPhiReplayCertificate
            (ewEdgeId witness)
            EdgeArtifacts
                { eaEdgeExpansions = IntMap.empty
                , eaEdgeWitnesses =
                    IntMap.singleton (getEdgeId (ewEdgeId witness)) witness
                , eaEdgeTraces =
                    IntMap.singleton (getEdgeId (ewEdgeId witness)) traceInfo
                , eaIdentityEdges = IntSet.empty
                }
    Translate.phiFromEdgeWitnessWithTrace
        traceCfg
        generalizeAt
        presolutionView
        (fromMaybe (gaBindParentsFromView presolutionView) mbGaParents)
        mSchemeInfo
        replay

phiOccurrenceFromEdgeWitnessWithTraceForTest
    :: TraceConfig
    -> (Maybe (GaBindParents p) -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef))
    -> PresolutionView p
    -> Maybe (GaBindParents p)
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError OccurrenceComputation
phiOccurrenceFromEdgeWitnessWithTraceForTest traceCfg generalizeAt presolutionView mbGaParents mSchemeInfo mTrace witness = do
    traceInfo <-
        case mTrace of
            Just trace -> Right trace
            Nothing -> Left (MissingEdgeTrace (ewEdgeId witness))
    replay <-
        Translate.mkPhiReplayCertificate
            (ewEdgeId witness)
            EdgeArtifacts
                { eaEdgeExpansions = IntMap.empty
                , eaEdgeWitnesses =
                    IntMap.singleton (getEdgeId (ewEdgeId witness)) witness
                , eaEdgeTraces =
                    IntMap.singleton (getEdgeId (ewEdgeId witness)) traceInfo
                , eaIdentityEdges = IntSet.empty
                }
    Translate.phiOccurrenceFromEdgeWitnessWithTrace
        traceCfg
        generalizeAt
        presolutionView
        (fromMaybe (gaBindParentsFromView presolutionView) mbGaParents)
        mSchemeInfo
        replay

-- | Low-level Phi fixtures that predate preserved source provenance receive a
-- complete identity certificate over their own graph.  This seam is test-only;
-- production translation must supply the actual pre-solve certificate.
gaBindParentsFromView :: PresolutionView p -> GaBindParents p
gaBindParentsFromView presolutionView =
    GaBindParents
        { gaBindParentsBase = cBindParents constraint
        , gaBaseConstraint = constraint
        , gaBaseToSolved = identityMap
        , gaSolvedToBase = identityMap
        , gaRestoredSchemeRootTargets = IntMap.empty
        , gaExpansionConstructionPlacements =
            emptyExpansionConstructionPlacements
        }
  where
    constraint = pvConstraint presolutionView
    identityMap =
        IntMap.fromList
            [ (getNodeId nodeId, nodeId)
            | (nodeId, _) <- toListNode (cNodes constraint)
            ]

-- | Exercise the edge-local @[phi_R; T(e)]@ computation without running the
-- elaboration algebra.  The caller supplies the source scheme, while edge
-- expansion, witness, trace, and identity authority come from presolution.
-- Figure 15.3.5 application endpoint construction is deliberately absent:
-- in thesis section 15.3.8 that additional authority is what turns the raw
-- graph-domain @g[t -> t]@ into the reduced @g[sigma-id]@ occurrence.
reifyInstWithSourceScheme
    :: TraceConfig
    -> PresolutionPlanBuilder
    -> PresolutionView p
    -> GaBindParents p
    -> EdgeArtifacts
    -> SchemeInfo
    -> AnnExpr
    -> EdgeId
    -> Either ElabError Instantiation
reifyInstWithSourceScheme traceCfg planBuilder presolutionView gaParents edgeArtifacts sourceScheme ann edgeId = do
    readModel <- buildElabReadModel presolutionView
    let namedSet = ermNamedNodes readModel
        generalizeAt mbGa =
            generalizeAtWithBuilder planBuilder mbGa presolutionView
        scopeContext =
            ScopeContext
                { scPresolutionView = presolutionView
                , scCanonical = pvCanonical presolutionView
                , scGaParents = gaParents
                , scScopeOverrides = mempty
                , scGeneralizeAtWith = generalizeAt
                , scGeneralizeAtWithRequirements =
                    \requirements mbGa ->
                        generalizeAtWithBuilderRequired
                            planBuilder
                            requirements
                            mbGa
                            presolutionView
                , scGeneralizeAtWithResultCertificate =
                    \request requirements mbGa ->
                        generalizeAtWithBuilderRequiredResultCertified
                            planBuilder
                            request
                            requirements
                            mbGa
                            presolutionView
                , scReadModel = readModel
                , scNamedSetReify = namedSet
                , scInlineBoundVarsContext = mkInlineBoundVarsContextWithReadModel readModel
                }
        annotationContext =
            AnnotationContext
                { acTraceConfig = traceCfg
                , acScopeContext = scopeContext
                , acAnnotationExpectedTypesByEdge = IntMap.empty
                , acSourceTypeHeadIdentities = Map.empty
                , acSourceTypeBinderIdentities = Map.empty
                , acSourceBinderRefs = IntMap.empty
                , acDirectSourceBinderKeys = IntSet.empty
                , acSubtermGeneralizations = Map.empty
                , acEdgeArtifacts = edgeArtifacts
                }
    reifyInstFromSourceScheme annotationContext namedSet sourceScheme ann edgeId
