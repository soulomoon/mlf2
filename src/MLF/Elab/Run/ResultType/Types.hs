{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module MLF.Elab.Run.ResultType.Types (
    ResultTypeInputs
        ( ResultTypeInputs
        , rtcCanonical
        , rtcEdgeArtifacts
        , rtcPresolutionView
        , rtcReadModel
        , rtcBaseReadModel
        , rtcBindParentsGa
        , rtcPlanBuilder
        , rtcBaseConstraint
        , rtcRedirects
        , rtcTraceConfig
        ),
    rtcEdgeWitnesses,
    rtcEdgeTraces,
    rtcEdgeExpansions,
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (Constraint, NodeId)
import MLF.Constraint.Presolution (EdgeTrace, PresolutionPlanBuilder)
import MLF.Constraint.Presolution.Base (EdgeArtifacts(..))
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion)
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.Generalize (GaBindParents)
import MLF.Elab.ReadModel (ElabReadModel)
import MLF.Elab.Types (ElabError)
import MLF.Util.Trace (TraceConfig)

-- | Context for result type computation, bundling shared state.
--
-- Phase-polymorphic: the result type machinery only reads phase-insensitive
-- graph data, so it works at any pipeline phase.
data ResultTypeInputs (p :: Phase) = ResultTypeInputs
    { rtcCanonical :: NodeId -> NodeId
    , rtcEdgeArtifacts :: EdgeArtifacts
    , rtcPresolutionView :: PresolutionView p
    , rtcReadModel :: Maybe (Either ElabError (ElabReadModel p))
    , rtcBaseReadModel :: Maybe (Either ElabError (ElabReadModel p))
    , rtcBindParentsGa :: GaBindParents p
    , rtcPlanBuilder :: PresolutionPlanBuilder
    , rtcBaseConstraint :: Constraint p
    , rtcRedirects :: IntMap.IntMap NodeId
    , rtcTraceConfig :: TraceConfig
    }

rtcEdgeWitnesses :: ResultTypeInputs p -> IntMap.IntMap EdgeWitness
rtcEdgeWitnesses = eaEdgeWitnesses . rtcEdgeArtifacts

rtcEdgeTraces :: ResultTypeInputs p -> IntMap.IntMap EdgeTrace
rtcEdgeTraces = eaEdgeTraces . rtcEdgeArtifacts

rtcEdgeExpansions :: ResultTypeInputs p -> IntMap.IntMap Expansion
rtcEdgeExpansions = eaEdgeExpansions . rtcEdgeArtifacts
