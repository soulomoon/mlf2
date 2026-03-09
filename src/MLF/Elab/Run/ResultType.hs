{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType (
    ResultTypeInputs(..),
    generalizeWithPlan,
    inferInstAppArgsFromScheme,
    mkResultTypeInputs,
    computeResultTypeFromAnn,
    computeResultTypeFallback
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Presolution (EdgeTrace, PresolutionPlanBuilder)
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Types.Graph
    ( Constraint
    , EdgeId(..)
    , NodeId(..)
    )
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion)
import MLF.Elab.Generalize (GaBindParents)
import MLF.Elab.Types (ElabType, ElabError)
import MLF.Util.Trace (TraceConfig)
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.ResultType.Types (ResultTypeInputs(..))
import MLF.Elab.Run.ResultType.Util (generalizeWithPlan, resultTypeRoots)
import qualified MLF.Elab.Run.ResultType.View as View
import qualified MLF.Elab.Run.ResultType.Ann as Ann
import qualified MLF.Elab.Run.ResultType.Fallback as Fallback

mkResultTypeInputs
    :: (NodeId -> NodeId)
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> PresolutionView
    -> GaBindParents
    -> PresolutionPlanBuilder
    -> Constraint
    -> IntMap.IntMap NodeId
    -> TraceConfig
    -> ResultTypeInputs
mkResultTypeInputs canonical edgeWitnesses edgeTraces edgeExpansions presolutionView bindParentsGa planBuilder baseConstraint redirects traceCfg =
    ResultTypeInputs
        { rtcCanonical = canonical
        , rtcEdgeWitnesses = edgeWitnesses
        , rtcEdgeTraces = edgeTraces
        , rtcEdgeExpansions = edgeExpansions
        , rtcPresolutionView = presolutionView
        , rtcBindParentsGa = bindParentsGa
        , rtcPlanBuilder = planBuilder
        , rtcBaseConstraint = baseConstraint
        , rtcRedirects = redirects
        , rtcTraceConfig = traceCfg
        }

-- Re-export computeResultTypeFromAnn from Ann module
computeResultTypeFromAnn :: ResultTypeInputs -> AnnExpr -> AnnExpr -> NodeId -> EdgeId -> Either ElabError ElabType
computeResultTypeFromAnn ctx inner innerPre annNodeId eid = do
    view <- View.buildResultTypeView ctx
    Ann.computeResultTypeFromAnnWithView ctx view inner innerPre annNodeId eid

-- | Compute result type when there's no direct annotation (fallback path).
-- This is a facade that handles the AAnn case by delegating to computeResultTypeFromAnn,
-- and delegates the non-AAnn case to the Fallback submodule.
computeResultTypeFallback
    :: ResultTypeInputs
    -> AnnExpr      -- ^ annCanon (post-redirect)
    -> AnnExpr      -- ^ ann (pre-redirect)
    -> Either ElabError ElabType
computeResultTypeFallback ctx annCanon ann = do
    view <- View.buildResultTypeView ctx
    computeResultTypeDispatch ctx view annCanon ann

computeResultTypeDispatch
    :: ResultTypeInputs
    -> View.ResultTypeView
    -> AnnExpr
    -> AnnExpr
    -> Either ElabError ElabType
computeResultTypeDispatch ctx view annCanon ann = do
    -- First, determine the root (same logic as before to check for AAnn)
    let (rootForTypeAnn, rootForTypePreAnn) =
            resultTypeRoots
                (rtcCanonical ctx)
                (ChiQuery.chiConstraint (rtcPresolutionView ctx))
                (rtcBaseConstraint ctx)
                annCanon
                ann

    -- Dispatch based on the root type
    case rootForTypeAnn of
        AAnn inner annNodeId eid -> do
            let innerPre =
                    case rootForTypePreAnn of
                        AAnn innerPre0 _ _ -> innerPre0
                        _ -> rootForTypePreAnn
            Ann.computeResultTypeFromAnnWithView ctx view inner innerPre annNodeId eid
        _ ->
            Fallback.computeResultTypeFallback computeResultTypeDispatch ctx view annCanon ann
