{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MLF.Elab.Run.ResultType.Types (
    ResultTypeInputs
        ( ResultTypeInputs
        , rtcCanonical
        , rtcEdgeWitnesses
        , rtcEdgeTraces
        , rtcEdgeExpansions
        , rtcPresolutionView
        , rtcSolvedCompat
        , rtcBindParentsGa
        , rtcPlanBuilder
        , rtcBaseConstraint
        , rtcRedirects
        , rtcTraceConfig
        ),
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (Constraint, NodeId(..))
import MLF.Constraint.Presolution
    ( PresolutionView(..)
    , EdgeTrace(..)
    , PresolutionPlanBuilder(..)
    )
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as SolvedCompat
import MLF.Constraint.Types.Witness (EdgeWitness(..), Expansion(..))
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.ChiQuery
    ( chiCanonicalConstraint )
import MLF.Util.Trace (TraceConfig)

-- | Context for result type computation, bundling shared state.
data ResultTypeInputs = MkResultTypeInputs
    (NodeId -> NodeId)
    (IntMap.IntMap EdgeWitness)
    (IntMap.IntMap EdgeTrace)
    (IntMap.IntMap Expansion)
    PresolutionView
    GaBindParents
    PresolutionPlanBuilder
    Constraint
    (IntMap.IntMap NodeId)
    TraceConfig

-- | Compatibility constructor: keeps existing call sites compiling while
-- retiring solved-compat as stored state in the ResultType input boundary.
pattern ResultTypeInputs
    :: (NodeId -> NodeId)
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> PresolutionView
    -> Solved
    -> GaBindParents
    -> PresolutionPlanBuilder
    -> Constraint
    -> IntMap.IntMap NodeId
    -> TraceConfig
    -> ResultTypeInputs
pattern ResultTypeInputs
    { rtcCanonical
    , rtcEdgeWitnesses
    , rtcEdgeTraces
    , rtcEdgeExpansions
    , rtcPresolutionView
    , rtcSolvedCompat
    , rtcBindParentsGa
    , rtcPlanBuilder
    , rtcBaseConstraint
    , rtcRedirects
    , rtcTraceConfig
    } <- (toCompatFields ->
        ( rtcCanonical
        , rtcEdgeWitnesses
        , rtcEdgeTraces
        , rtcEdgeExpansions
        , rtcPresolutionView
        , rtcSolvedCompat
        , rtcBindParentsGa
        , rtcPlanBuilder
        , rtcBaseConstraint
        , rtcRedirects
        , rtcTraceConfig
        ))
  where
    ResultTypeInputs
        canonicalF
        edgeWitnessesF
        edgeTracesF
        edgeExpansionsF
        presolutionViewF
        _rtcSolvedCompat
        bindParentsGaF
        planBuilderF
        baseConstraintF
        redirectsF
        traceCfgF =
            MkResultTypeInputs
                canonicalF
                edgeWitnessesF
                edgeTracesF
                edgeExpansionsF
                presolutionViewF
                bindParentsGaF
                planBuilderF
                baseConstraintF
                redirectsF
                traceCfgF

{-# COMPLETE ResultTypeInputs #-}

toCompatFields
    :: ResultTypeInputs
    -> ( (NodeId -> NodeId)
       , IntMap.IntMap EdgeWitness
       , IntMap.IntMap EdgeTrace
       , IntMap.IntMap Expansion
       , PresolutionView
       , Solved
       , GaBindParents
       , PresolutionPlanBuilder
       , Constraint
       , IntMap.IntMap NodeId
       , TraceConfig
       )
toCompatFields
    (MkResultTypeInputs canonical edgeWitnesses edgeTraces edgeExpansions presolutionView bindParentsGa planBuilder baseConstraint redirects traceCfg) =
        ( canonical
        , edgeWitnesses
        , edgeTraces
        , edgeExpansions
        , presolutionView
        , compatSolved presolutionView baseConstraint
        , bindParentsGa
        , planBuilder
        , baseConstraint
        , redirects
        , traceCfg
        )

compatSolved :: PresolutionView -> Constraint -> Solved
compatSolved presolutionView baseConstraint =
    let solved0 = SolvedCompat.fromConstraintAndUf baseConstraint (pvCanonicalMap presolutionView)
    in SolvedCompat.rebuildWithConstraint solved0 (chiCanonicalConstraint presolutionView)
