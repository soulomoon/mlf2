-- |
-- Module      : MLF.Constraint.Presolution.Plan
-- Description : Build generalization plans from solved constraints
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- This module coordinates the generalization planning process, which determines
-- how to turn solved constraint graphs into polymorphic type schemes. It builds
-- plans that specify:
--
-- * Which variables to generalize as binders
-- * How to order those binders
-- * How to reify the type structure
-- * How to handle scheme roots
--
-- The planning process is split across submodules:
--
-- * 'BinderPlan' - Plan which variables become quantified binders
-- * 'Context' - Build the generalization context from scope info
-- * 'Target' - Determine the target type and its properties
-- * 'SchemeRoots' - Track scheme ownership for polymorphism
-- * 'ReifyPlan' - Plan the reification of types with bounds
-- * 'Finalize' - Finalize schemes with proper naming
-- * 'Env' - Environment construction for generalization planning
-- * 'Generalize' - Build a generalization plan for a single scope
-- * 'ReifyStep' - Build the reification plan for a generalized scope
module MLF.Constraint.Presolution.Plan
  ( GeneralizePlan (..),
    ReifyPlan (..),
    PresolutionEnv (..),
    planGeneralizeAt,
    planReify,
    buildGeneralizePlans,
  )
where

import MLF.Constraint.Presolution.Plan.Context (GaBindParents (..))
import MLF.Constraint.Presolution.Plan.Env (PresolutionEnv (..))
import MLF.Constraint.Presolution.Plan.Generalize (GeneralizePlan (..), planGeneralizeAt)
import MLF.Constraint.Presolution.Plan.ReifyStep (ReifyPlan (..), planReify)
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Types.Graph (NodeId, NodeRef, cBindParents)
import MLF.Util.ElabError (ElabError)
import MLF.Util.Trace (TraceConfig)

buildGeneralizePlans ::
  TraceConfig ->
  PresolutionView p ->
  Maybe (GaBindParents p) ->
  NodeRef ->
  NodeId ->
  Either ElabError (GeneralizePlan p, ReifyPlan)
buildGeneralizePlans traceCfg presolutionView mbBindParentsGa scopeRoot targetNode = do
  let constraint = pvCanonicalConstraint presolutionView
      canonical = pvCanonical presolutionView
      presEnv =
        PresolutionEnv
          { peConstraint = constraint,
            pePresolutionView = presolutionView,
            peCanonical = canonical,
            peBindParents = cBindParents constraint,
            peBindParentsGa = mbBindParentsGa,
            peScopeRoot = scopeRoot,
            peTargetNode = targetNode,
            peTraceConfig = traceCfg
          }
  genPlan <- planGeneralizeAt presEnv
  reifyPlan <- planReify presEnv genPlan
  pure (genPlan, reifyPlan)
