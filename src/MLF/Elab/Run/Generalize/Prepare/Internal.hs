{-# LANGUAGE DataKinds #-}

module MLF.Elab.Run.Generalize.Prepare.Internal (
    PreparedGeneralizationArtifact(..),
    prepareGeneralizationArtifact,
    preparedAnnotated,
    preparedElaborationConfig,
    preparedElaborationEnv,
    stripPreparedWitnesslessAuthoritativeAnn,
    generalizePreparedRoot,
    computePreparedResultType,
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
import MLF.Elab.Run.Annotation (annNode, redirectAndCanonicalizeAnn)
import MLF.Elab.Run.Generalize
    ( GeneralizeAtView
    , constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType
    ( ResultTypeInputs
    , computeResultTypeFallback
    , computeResultTypeFromAnn
    , mkResultTypeInputs
    )
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
    , pgaEdgeArtifacts :: EdgeArtifacts
    , pgaScopeOverrides :: IntMap.IntMap NodeRef
    , pgaAnnotated :: AnnExpr
    , pgaAnnNodeCanonical :: NodeId -> NodeId
    , pgaCanonical :: NodeId -> NodeId
    , pgaPlanBuilder :: PresolutionPlanBuilder
    , pgaRedirects :: IntMap.IntMap NodeId
    }

prepareGeneralizationArtifact
    :: TraceConfig
    -> Constraint 'Acyclic
    -> PresolutionResult
    -> AnnExpr
    -> Either SolveError PreparedGeneralizationArtifact
prepareGeneralizationArtifact traceCfg acyclicBase pres ann = do
    let preRewrite = snapshotConstraint pres
    solvedClean <- Finalize.finalizeSolvedFromSnapshot preRewrite (snapshotUnionFind pres)
    presolutionViewClean <- Finalize.finalizePresolutionViewFromSnapshot preRewrite (snapshotUnionFind pres)
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
                ann
    presolutionViewForGen <-
        Finalize.finalizePresolutionViewFromSnapshot
            constraintForGen
            (Solved.canonicalMap solvedClean)
    let annNodeCanonical = canonicalizeNode canonNode
        annCanon = redirectAndCanonicalizeAnn annNodeCanonical (prRedirects pres) ann
        edgeArtifacts =
            EdgeArtifacts
                { eaEdgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres)
                , eaEdgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres)
                , eaEdgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
                }
        scopeOverrides =
            letScopeOverrides
                acyclicBaseForGeneralization
                constraintForGen
                presolutionViewClean
                (prRedirects pres)
                annCanon
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
    pure
        PreparedGeneralizationArtifact
            { pgaPresolutionView = presolutionViewForGen
            , pgaBindParentsGa = bindParentsGa
            , pgaGeneralizeAt = generalizeAtWithView
            , pgaResultTypeInputs = resultTypeInputs
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
    rootScope <-
        bindingToElab $
            resolveCanonicalScope
                (gaBaseConstraint (pgaBindParentsGa artifact))
                (pgaPresolutionView artifact)
                (pgaRedirects artifact)
                (annNode authoritativeAnnPre)
    let rootTarget =
            schemeBodyTarget
                (pgaPresolutionView artifact)
                (annNode authoritativeAnnCanon)
    pgaGeneralizeAt artifact (Just (pgaBindParentsGa artifact)) rootScope rootTarget

computePreparedResultType
    :: PreparedGeneralizationArtifact
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError ElabType
computePreparedResultType artifact authoritativeAnnCanon authoritativeAnnPre =
    case (authoritativeAnnCanon, authoritativeAnnPre) of
        (AAnn inner annNodeId eid, AAnn innerPre _ _) ->
            computeResultTypeFromAnn resultTypeInputs inner innerPre annNodeId eid
        (AUnfold inner annNodeId eid, _) ->
            let innerPre =
                    case authoritativeAnnPre of
                        AUnfold ip _ _ -> ip
                        AAnn ip _ _ -> ip
                        other -> other
             in computeResultTypeFromAnn resultTypeInputs inner innerPre annNodeId eid
        _ ->
            computeResultTypeFallback resultTypeInputs authoritativeAnnCanon authoritativeAnnPre
  where
    resultTypeInputs = pgaResultTypeInputs artifact

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
