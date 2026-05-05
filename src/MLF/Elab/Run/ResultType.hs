{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.ResultType
  ( ResultTypeInputs (..),
    rtcEdgeWitnesses,
    rtcEdgeTraces,
    rtcEdgeExpansions,
    generalizeWithPlan,
    inferInstAppArgsFromScheme,
    mkResultTypeInputs,
    computeResultTypeFromAnn,
    computeResultTypeFallback,
  )
where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution (PresolutionPlanBuilder)
import MLF.Constraint.Presolution.Base (EdgeArtifacts (..))
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Types.Graph
  ( Constraint,
    EdgeId (..),
    NodeId (..),
  )
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Elab.Generalize (GaBindParents)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Instantiation (inferInstAppArgsFromScheme)
import qualified MLF.Elab.Run.ResultType.Ann as Ann
import qualified MLF.Elab.Run.ResultType.Fallback as Fallback
import MLF.Elab.Run.ResultType.Types
  ( ResultTypeInputs (..),
    rtcEdgeExpansions,
    rtcEdgeTraces,
    rtcEdgeWitnesses,
  )
import MLF.Elab.Run.ResultType.Util (generalizeWithPlan, resultTypeRoots)
import qualified MLF.Elab.Run.ResultType.View as View
import MLF.Elab.Types (ElabError, ElabType)
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Util.Trace (TraceConfig)

mkResultTypeInputs ::
  (NodeId -> NodeId) ->
  EdgeArtifacts ->
  PresolutionView 'Raw ->
  GaBindParents 'Raw ->
  PresolutionPlanBuilder ->
  Constraint 'Raw ->
  IntMap.IntMap NodeId ->
  TraceConfig ->
  ResultTypeInputs
mkResultTypeInputs canonical edgeArtifacts presolutionView bindParentsGa planBuilder baseConstraint redirects traceCfg =
  ResultTypeInputs
    { rtcCanonical = canonical,
      rtcEdgeArtifacts = edgeArtifacts,
      rtcPresolutionView = presolutionView,
      rtcBindParentsGa = bindParentsGa,
      rtcPlanBuilder = planBuilder,
      rtcBaseConstraint = baseConstraint,
      rtcRedirects = redirects,
      rtcTraceConfig = traceCfg
    }

-- Re-export computeResultTypeFromAnn from Ann module
computeResultTypeFromAnn :: ResultTypeInputs -> AnnExpr -> AnnExpr -> NodeId -> EdgeId -> Either ElabError ElabType
computeResultTypeFromAnn ctx inner innerPre annNodeId eid = do
  view <- View.buildResultTypeView ctx
  Ann.computeResultTypeFromAnnWithView ctx view inner innerPre annNodeId eid

-- | Compute result type when there's no direct annotation (fallback path).
-- This is a facade that handles the AAnn case by delegating to computeResultTypeFromAnn,
-- and delegates the non-AAnn case to the Fallback submodule.
computeResultTypeFallback ::
  ResultTypeInputs ->
  -- | annCanon (post-redirect)
  AnnExpr ->
  -- | ann (pre-redirect)
  AnnExpr ->
  Either ElabError ElabType
computeResultTypeFallback ctx annCanon ann = do
  view <- View.buildResultTypeView ctx
  computeResultTypeDispatch ctx view annCanon ann

computeResultTypeDispatch ::
  ResultTypeInputs ->
  View.ResultTypeView ->
  AnnExpr ->
  AnnExpr ->
  Either ElabError ElabType
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
              AUnfold innerPre0 _ _ -> innerPre0
              _ -> rootForTypePreAnn
      Ann.computeResultTypeFromAnnWithView ctx view inner innerPre annNodeId eid
    AUnfold inner annNodeId eid -> do
      let innerPre =
            case rootForTypePreAnn of
              AAnn innerPre0 _ _ -> innerPre0
              AUnfold innerPre0 _ _ -> innerPre0
              _ -> rootForTypePreAnn
      Ann.computeResultTypeFromAnnWithView ctx view inner innerPre annNodeId eid
    _ ->
      Fallback.computeResultTypeFallback computeResultTypeDispatch ctx view annCanon ann
