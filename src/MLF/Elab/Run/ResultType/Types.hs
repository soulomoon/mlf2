{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType.Types (
    ResultTypeContext(..),
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (Constraint, NodeId(..))
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionPlanBuilder(..)
    )
import MLF.Constraint.Solve (SolveResult(..))
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Constraint.Types.Witness (EdgeWitness(..), Expansion(..))
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Util.Trace (TraceConfig)

-- | Context for result type computation, bundling shared state.
data ResultTypeContext = ResultTypeContext
    { rtcCanonical :: NodeId -> NodeId
    , rtcEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , rtcEdgeTraces :: IntMap.IntMap EdgeTrace
    , rtcEdgeExpansions :: IntMap.IntMap Expansion
    , rtcSolvedForGen :: SolveResult
    , rtcSolvedClean :: SolveResult
    , rtcBindParentsGa :: GaBindParents
    , rtcPlanBuilder :: PresolutionPlanBuilder
    , rtcBaseConstraint :: Constraint
    , rtcRedirects :: IntMap.IntMap NodeId
    , rtcTraceConfig :: TraceConfig
    }
