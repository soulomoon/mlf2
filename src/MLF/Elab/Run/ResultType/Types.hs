module MLF.Elab.Run.ResultType.Types (
    ResultTypeInputs
        ( ResultTypeInputs
        , rtcCanonical
        , rtcEdgeArtifacts
        , rtcPresolutionView
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
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion)
import MLF.Elab.Generalize (GaBindParents)
import MLF.Util.Trace (TraceConfig)

-- | Context for result type computation, bundling shared state.
data ResultTypeInputs = ResultTypeInputs
    { rtcCanonical :: NodeId -> NodeId
    , rtcEdgeArtifacts :: EdgeArtifacts
    , rtcPresolutionView :: PresolutionView
    , rtcBindParentsGa :: GaBindParents
    , rtcPlanBuilder :: PresolutionPlanBuilder
    , rtcBaseConstraint :: Constraint
    , rtcRedirects :: IntMap.IntMap NodeId
    , rtcTraceConfig :: TraceConfig
    }

rtcEdgeWitnesses :: ResultTypeInputs -> IntMap.IntMap EdgeWitness
rtcEdgeWitnesses = eaEdgeWitnesses . rtcEdgeArtifacts

rtcEdgeTraces :: ResultTypeInputs -> IntMap.IntMap EdgeTrace
rtcEdgeTraces = eaEdgeTraces . rtcEdgeArtifacts

rtcEdgeExpansions :: ResultTypeInputs -> IntMap.IntMap Expansion
rtcEdgeExpansions = eaEdgeExpansions . rtcEdgeArtifacts
