{-# LANGUAGE DataKinds #-}

module MLF.Elab.Run.Generalize.Prepare.Internal (
    PreparedGeneralizationArtifact(..),
    PreparedRootGeneralization(..),
    prepareGeneralizationArtifact,
    prepareGeneralizationArtifactForRoots,
    preparedAnnotated,
    canonicalizePreparedAnn,
    preparedReadContextReady,
    preparedResultTypeViewReady,
    preparedElaborationConfig,
    preparedElaborationEnv,
    stripPreparedWitnesslessAuthoritativeAnn,
    generalizePreparedRoot,
    generalizePreparedRootDetailed,
    computePreparedResultType,
    computePreparedResultTypeWithRootGeneralization,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map

import MLF.Constraint.Canonicalizer (Canonicalizer, canonicalizeNode)
import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionPlanBuilder
    , PresolutionResult(..)
    )
import MLF.Constraint.Presolution.Base (EdgeArtifacts(..))
import MLF.Constraint.Presolution.View (PresolutionView, pvCanonical)
import MLF.Constraint.Solve (SolveError)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId(..)
    , NodeRef
    , cNodes
    , getEdgeId
    , getNodeId
    , lookupNodeIn
    , toPresolvedConstraint
    )
import MLF.Constraint.Types.Phase (Phase(Acyclic, Presolved))
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
import MLF.Elab.Elaborate (ElabConfig(..), ElabEnv(..))
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.ReadModel (ElabReadModel, buildElabReadModel)
import MLF.Elab.Run.Annotation (annNode, redirectAndCanonicalizeAnn)
import MLF.Elab.Run.Generalize
    ( GeneralizeAtView
    , constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
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
    ( letScopeOverrides
    , resolveCanonicalScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Elab.Types
    ( ElabError
    , ElabScheme
    , ElabType
    , SchemeInfo
    , bindingToElab
    )
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.Syntax (NormSrcType, VarName)
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
    , pgaGeneralizeAt :: GeneralizeAtView 'Presolved
    , pgaResultTypeInputs :: ResultTypeInputs 'Presolved
    , pgaReadModel :: Either ElabError (ElabReadModel 'Presolved)
    , pgaBaseReadModel :: Either ElabError (ElabReadModel 'Presolved)
    , pgaResultTypeView :: Either ElabError (ResultTypeView 'Presolved)
    , pgaEdgeArtifacts :: EdgeArtifacts
    , pgaScopeOverrides :: IntMap.IntMap NodeRef
    , pgaAnnotated :: AnnExpr
    , pgaAnnNodeCanonical :: NodeId -> NodeId
    , pgaCanonical :: NodeId -> NodeId
    , pgaPlanBuilder :: PresolutionPlanBuilder
    , pgaRedirects :: IntMap.IntMap NodeId
    }

data PreparedRootGeneralization = PreparedRootGeneralization
    { prgScopeRoot :: NodeRef
    , prgTarget :: NodeId
    , prgScheme :: ElabScheme
    , prgSubst :: IntMap.IntMap String
    }

prepareGeneralizationArtifact
    :: TraceConfig
    -> Constraint 'Acyclic
    -> PresolutionResult
    -> AnnExpr
    -> Either SolveError PreparedGeneralizationArtifact
prepareGeneralizationArtifact traceCfg acyclicBase pres ann =
    prepareGeneralizationArtifactForRoots traceCfg acyclicBase pres [ann]

prepareGeneralizationArtifactForRoots
    :: TraceConfig
    -> Constraint 'Acyclic
    -> PresolutionResult
    -> [AnnExpr]
    -> Either SolveError PreparedGeneralizationArtifact
prepareGeneralizationArtifactForRoots traceCfg acyclicBase pres anns0 = do
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
    let canonNode = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        acyclicBaseForGeneralization = toPresolvedConstraint acyclicBase
        planBuilder = prPlanBuilder pres
        TraceCopyArtifacts
            { tcaInstCopyNodes = instCopyNodes
            , tcaInstCopyMapFull = instCopyMapFull
            } =
            prepareTraceCopyArtifacts
                acyclicBase
                presolutionViewClean
                (prRedirects pres)
                canonNode
                (prEdgeTraces pres)
        (constraintForGen, bindParentsGa) =
            constraintForGeneralization
                traceCfg
                presolutionViewClean
                (prRedirects pres)
                instCopyNodes
                instCopyMapFull
                acyclicBaseForGeneralization
                annForGeneralization
    presolutionViewForGen <-
        Finalize.finalizePresolutionViewFromSnapshot
            constraintForGen
            (Solved.canonicalMap solvedClean)
    let annNodeCanonical = canonicalizeNode canonNode
        annCanons =
            map
                (redirectAndCanonicalizeAnn annNodeCanonical (prRedirects pres))
                anns
        annCanon =
            case annCanons of
                firstAnn : _ -> firstAnn
                [] -> snapshotAnnFallback
        edgeArtifacts =
            EdgeArtifacts
                { eaEdgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres)
                , eaEdgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres)
                , eaEdgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
                }
        scopeOverrides =
            IntMap.unions
                [ letScopeOverrides
                    acyclicBaseForGeneralization
                    constraintForGen
                    presolutionViewClean
                    (prRedirects pres)
                    annRoot
                | annRoot <- annCanons
                ]
        generalizeAtWithView mbGa =
            generalizeAtWithBuilder
                planBuilder
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
        resultTypeView =
            buildResultTypeView resultTypeInputsWithReadModels
    pure
        PreparedGeneralizationArtifact
            { pgaPresolutionView = presolutionViewForGen
            , pgaBindParentsGa = bindParentsGa
            , pgaGeneralizeAt = generalizeAtWithView
            , pgaResultTypeInputs = resultTypeInputsWithReadModels
            , pgaReadModel = readModel
            , pgaBaseReadModel = baseReadModel
            , pgaResultTypeView = resultTypeView
            , pgaEdgeArtifacts = edgeArtifacts
            , pgaScopeOverrides = scopeOverrides
            , pgaAnnotated = annCanon
            , pgaAnnNodeCanonical = annNodeCanonical
            , pgaCanonical = pvCanonical presolutionViewForGen
            , pgaPlanBuilder = planBuilder
            , pgaRedirects = prRedirects pres
            }

preparedAnnotated :: PreparedGeneralizationArtifact -> AnnExpr
preparedAnnotated = pgaAnnotated

canonicalizePreparedAnn :: PreparedGeneralizationArtifact -> AnnExpr -> AnnExpr
canonicalizePreparedAnn artifact =
    redirectAndCanonicalizeAnn (pgaAnnNodeCanonical artifact) (pgaRedirects artifact)

preparedReadContextReady :: PreparedGeneralizationArtifact -> Either ElabError ()
preparedReadContextReady artifact = do
    _ <- pgaReadModel artifact
    _ <- pgaBaseReadModel artifact
    pure ()

preparedResultTypeViewReady :: PreparedGeneralizationArtifact -> Either ElabError ()
preparedResultTypeViewReady artifact = do
    _ <- pgaResultTypeView artifact
    pure ()

preparedElaborationConfig :: TraceConfig -> PreparedGeneralizationArtifact -> ElabConfig 'Presolved
preparedElaborationConfig traceCfg artifact =
    ElabConfig
        { ecTraceConfig = traceCfg
        , ecGeneralizeAtWith = pgaGeneralizeAt artifact
        }

preparedElaborationEnv
    :: IntMap.IntMap NormSrcType
    -> Map.Map VarName SchemeInfo
    -> PreparedGeneralizationArtifact
    -> ElabEnv 'Presolved
preparedElaborationEnv annSourceTypes initialTermEnv artifact =
    ElabEnv
        { eePresolutionView = pgaPresolutionView artifact
        , eeReadModel = pgaReadModel artifact
        , eeGaParents = pgaBindParentsGa artifact
        , eeEdgeArtifacts = pgaEdgeArtifacts artifact
        , eeScopeOverrides = pgaScopeOverrides artifact
        , eeAnnSourceTypes = canonicalizePreparedAnnSourceTypes artifact annSourceTypes
        , eeInitialTermEnv = initialTermEnv
        }

canonicalizePreparedAnnSourceTypes
    :: PreparedGeneralizationArtifact
    -> IntMap.IntMap NormSrcType
    -> IntMap.IntMap NormSrcType
canonicalizePreparedAnnSourceTypes artifact annSourceTypes =
    IntMap.fromList
        [ (getNodeId (pgaAnnNodeCanonical artifact nid), ty)
        | (k, ty) <- IntMap.toList annSourceTypes
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

generalizePreparedRoot
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizePreparedRoot artifact authoritativeAnnCanon authoritativeAnnPre = do
    detailed <- generalizePreparedRootDetailed artifact authoritativeAnnCanon authoritativeAnnPre
    pure (prgScheme detailed, prgSubst detailed)

generalizePreparedRootDetailed
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError PreparedRootGeneralization
generalizePreparedRootDetailed artifact authoritativeAnnCanon authoritativeAnnPre = do
    rootScope <-
        bindingToElab $
            resolveCanonicalScope
                (gaBaseConstraint (pgaBindParentsGa artifact))
                (pgaPresolutionView artifact)
                (pgaRedirects artifact)
                (annNode authoritativeAnnPre)
    let rootTarget =
            preparedSchemeBodyTarget artifact (annNode authoritativeAnnCanon)
    (scheme, subst) <-
        pgaGeneralizeAt artifact (Just (pgaBindParentsGa artifact)) rootScope rootTarget
    pure
        PreparedRootGeneralization
            { prgScopeRoot = rootScope
            , prgTarget = rootTarget
            , prgScheme = scheme
            , prgSubst = subst
            }

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
            (AUnfold inner annNodeId eid, _) ->
                let innerPre =
                        case authoritativeAnnPre of
                            AUnfold ip _ _ -> ip
                            AAnn ip _ _ -> ip
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
                            AUnfold inner _ _ -> inner
                            other -> other
                 in stripWitnesslessAuthoritativeAnnWith edgeWitnesses innerCanon innerPre
        AUnfold innerCanon _ eid
            | IntMap.notMember (getEdgeId eid) edgeWitnesses ->
                let innerPre =
                        case annPre of
                            AAnn inner _ _ -> inner
                            AUnfold inner _ _ -> inner
                            other -> other
                 in stripWitnesslessAuthoritativeAnnWith edgeWitnesses innerCanon innerPre
        _ -> (annCanon, annPre)

data TraceCopyArtifacts = TraceCopyArtifacts
    { tcaInstCopyNodes :: IntSet.IntSet
    , tcaInstCopyMapFull :: IntMap.IntMap NodeId
    }

prepareTraceCopyArtifacts
    :: Constraint p
    -> PresolutionView q
    -> IntMap.IntMap NodeId
    -> Canonicalizer
    -> IntMap.IntMap EdgeTrace
    -> TraceCopyArtifacts
prepareTraceCopyArtifacts baseConstraint presolutionView redirects canonNode edgeTraces =
    let adoptNode = canonicalizeNode canonNode
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
     in TraceCopyArtifacts
            { tcaInstCopyNodes = instCopyNodes
            , tcaInstCopyMapFull = instCopyMapFull
            }
