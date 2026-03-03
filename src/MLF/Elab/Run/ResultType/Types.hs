{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.Types (
    ResultTypeInputs(..),
    rtcSolveLike,
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (Constraint, NodeId(..))
import MLF.Constraint.Presolution
    ( PresolutionView(..)
    , EdgeTrace(..)
    , PresolutionPlanBuilder(..)
    )
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Witness (EdgeWitness(..), Expansion(..))
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Util.ElabError (ElabError)
import MLF.Util.Trace (TraceConfig)

-- | Context for result type computation, bundling shared state.
data ResultTypeInputs = ResultTypeInputs
    { rtcCanonical :: NodeId -> NodeId
    , rtcEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , rtcEdgeTraces :: IntMap.IntMap EdgeTrace
    , rtcEdgeExpansions :: IntMap.IntMap Expansion
    , rtcPresolutionView :: PresolutionView
    , rtcBindParentsGa :: GaBindParents
    , rtcPlanBuilder :: PresolutionPlanBuilder
    , rtcBaseConstraint :: Constraint
    , rtcRedirects :: IntMap.IntMap NodeId
    , rtcTraceConfig :: TraceConfig
    }

rtcSolveLike :: ResultTypeInputs -> Either ElabError Solved
rtcSolveLike ctx =
    let presolutionView = rtcPresolutionView ctx
        solved0 =
            Solved.fromConstraintAndUf
                (pvConstraint presolutionView)
                (pvCanonicalMap presolutionView)
        solved = Solved.rebuildWithConstraint solved0 (pvCanonicalConstraint presolutionView)
    in Right solved
