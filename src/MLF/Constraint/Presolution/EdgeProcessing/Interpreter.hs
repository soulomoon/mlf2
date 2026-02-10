{-# LANGUAGE DataKinds #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Interpreter
Description : Pass B — execute typed edge plans
Copyright   : (c) 2024
License     : BSD-3-Clause

The interpreter (pass B of the two-pass architecture) executes a resolved
'EdgePlan' by dispatching to the appropriate code path based on the plan's
'EdgePlanMode':

* 'ExpansionMode': decideMinimalExpansion + unifyStructure (TyExp left)
* 'LegacyDirectMode': solveNonExpInstantiation (non-TyExp left)

Both paths record the same observable artifacts (expansion, witness, trace).
-}
module MLF.Constraint.Presolution.EdgeProcessing.Interpreter (
    executeEdgePlan,
) where

import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base (PresolutionM, emptyTrace)
import MLF.Constraint.Presolution.Ops (getCanonicalNode)
import MLF.Constraint.Presolution.Expansion (
    decideMinimalExpansion,
    getExpansion,
    mergeExpansions,
    recordEdgeExpansion,
    setExpansion
    )
import MLF.Constraint.Presolution.EdgeProcessing.Unify (
    EdgeExpansionResult(..),
    runExpansionUnify
    )
import MLF.Constraint.Presolution.EdgeProcessing.Witness (
    EdgeWitnessPlan(..),
    buildEdgeTrace,
    buildEdgeWitness,
    edgeWitnessPlan
    )
import MLF.Constraint.Presolution.EdgeProcessing (
    unifyStructure,
    solveNonExpInstantiation,
    recordEdgeWitness,
    recordEdgeTrace,
    canonicalizeEdgeTraceInteriorsM
    )
import MLF.Constraint.Presolution.EdgeProcessing.Plan

-- | Execute a resolved edge plan by dispatching on its mode.
executeEdgePlan :: EdgePlan 'StageResolved -> PresolutionM ()
executeEdgePlan plan = case eprMode plan of
    ExpansionMode    -> executeExpansionPath plan
    LegacyDirectMode -> executeLegacyDirectPath plan

-- | Expansion path: left node is TyExp.
-- Uses decideMinimalExpansion + mergeExpansions + runExpansionUnify.
executeExpansionPath :: EdgePlan 'StageResolved -> PresolutionM ()
executeExpansionPath plan = do
    let edge = eprEdge plan
        edgeId = instEdgeId edge
        n1Id = instLeft edge
        n2Id = instRight edge
        n1Raw = eprLeftNode plan
        n2 = eprRightNode plan
        TyExp { tnExpVar = s } = n1Raw

    currentExp <- getExpansion s
    (reqExp, unifications) <- decideMinimalExpansion (eprAllowTrivial plan) n1Raw n2
    finalExp <- mergeExpansions s currentExp reqExp

    setExpansion s finalExp
    recordEdgeExpansion edgeId finalExp

    mapM_ (uncurry unifyStructure) unifications

    witnessPlan <- edgeWitnessPlan (eprSuppressWeaken plan) n1Id n1Raw finalExp
    expansionResult <-
        if finalExp == ExpIdentity
            then pure EdgeExpansionResult { eerTrace = emptyTrace, eerExtraOps = [] }
            else runExpansionUnify edgeId n1Raw n2 finalExp (ewpBaseOps witnessPlan)

    let expTrace = eerTrace expansionResult
        extraOps = eerExtraOps expansionResult
    tr <- buildEdgeTrace edgeId n1Id n1Raw finalExp expTrace
    recordEdgeTrace edgeId tr
    w <- buildEdgeWitness edgeId n1Id n2Id n1Raw (ewpBaseSteps witnessPlan) extraOps
    recordEdgeWitness edgeId w
    canonicalizeEdgeTraceInteriorsM

-- | Legacy direct path: left node is non-TyExp.
-- Uses solveNonExpInstantiation with binding-permission + scheme-root logic.
executeLegacyDirectPath :: EdgePlan 'StageResolved -> PresolutionM ()
executeLegacyDirectPath plan = do
    let edge = eprEdge plan
        edgeId = instEdgeId edge
        n1Id = instLeft edge
        n2Id = instRight edge
        n1Raw = eprLeftNode plan

    n1 <- getCanonicalNode n1Id
    n2 <- getCanonicalNode n2Id
    recordEdgeExpansion edgeId ExpIdentity
    witnessPlan <- edgeWitnessPlan (eprSuppressWeaken plan) n1Id n1Raw ExpIdentity
    w <- buildEdgeWitness edgeId n1Id n2Id n1Raw (ewpBaseSteps witnessPlan) []
    recordEdgeWitness edgeId w
    solveNonExpInstantiation (tnId n1) (tnId n2)
    tr <- buildEdgeTrace edgeId n1Id n1Raw ExpIdentity emptyTrace
    recordEdgeTrace edgeId tr
    canonicalizeEdgeTraceInteriorsM