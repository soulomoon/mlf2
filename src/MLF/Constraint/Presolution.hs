{- |
Module      : MLF.Constraint.Presolution
Description : Phase 4 - Principal Presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module is the public entrypoint for Phase 4 (presolution). The
implementation is split into submodules under `MLF.Constraint.Presolution.*`
to keep responsibilities clearer.
-}
module MLF.Constraint.Presolution (
    -- * Main API
    computePresolution,
    PresolutionResult(..),
    PresolutionPlanBuilder(..),
    defaultPlanBuilder,
    PresolutionError(..),

    -- * Internal types (exported for testing)
    PresolutionState(..),
    EdgeTrace(..),
    runPresolutionM,

    -- * Building blocks (exported for testing)
    decideMinimalExpansion,
    processInstEdge,
    unifyAcyclicRawWithRaiseTrace,
    runEdgeUnifyForTest,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    applyExpansion
) where

import MLF.Constraint.Presolution.Base (
    EdgeTrace(..),
    PresolutionError(..),
    PresolutionPlanBuilder(..),
    PresolutionResult(..),
    PresolutionState(..),
    runPresolutionM
    )
import MLF.Constraint.Presolution.Core (
    applyExpansion,
    computePresolution,
    decideMinimalExpansion,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    processInstEdge,
    runEdgeUnifyForTest,
    unifyAcyclicRawWithRaiseTrace
    )
import MLF.Constraint.Presolution.Plan (buildGeneralizePlans)

defaultPlanBuilder :: PresolutionPlanBuilder
defaultPlanBuilder = PresolutionPlanBuilder buildGeneralizePlans
