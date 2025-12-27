{- |
Module      : MLF.Constraint.Presolution.Core
Description : Presolution compatibility facade

This module is a thin compatibility layer kept for tests and internal tooling.
The implementation lives in smaller submodules under `MLF.Constraint.Presolution.*`.
-}
module MLF.Constraint.Presolution.Core (
    computePresolution,
    decideMinimalExpansion,
    processInstEdge,
    unifyAcyclicRawWithRaiseTrace,
    runEdgeUnifyForTest,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    normalizeInstanceOps,
    applyExpansion
) where

import MLF.Constraint.Presolution.Driver (computePresolution, processInstEdge)
import MLF.Constraint.Presolution.EdgeUnify (runEdgeUnifyForTest)
import MLF.Constraint.Presolution.Expansion (
    applyExpansion,
    decideMinimalExpansion,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions
    )
import MLF.Constraint.Presolution.Unify (unifyAcyclicRawWithRaiseTrace)
import MLF.Constraint.Presolution.Witness (normalizeInstanceOps)
