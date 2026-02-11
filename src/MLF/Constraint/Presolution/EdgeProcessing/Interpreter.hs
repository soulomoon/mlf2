{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Interpreter
Description : Pass B — execute typed edge plans
Copyright   : (c) 2024
License     : BSD-3-Clause

The interpreter (pass B of the two-pass architecture) executes a resolved
'EdgePlan' using a single expansion-oriented execution path.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Interpreter (
    executeEdgePlan,
) where

import Control.Monad.Except (catchError, throwError)

import MLF.Constraint.Presolution.Base (PresolutionError (..), PresolutionM, emptyTrace)
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Solve (
    canonicalizeEdgeTraceInteriorsM,
    recordEdgeTrace,
    recordEdgeWitness,
    solveNonExpInstantiation,
    unifyStructure
    )
import MLF.Constraint.Presolution.EdgeProcessing.Unify (
    EdgeExpansionResult (..),
    runExpansionUnify
    )
import MLF.Constraint.Presolution.EdgeProcessing.Witness (
    EdgeWitnessPlan (..),
    buildEdgeTrace,
    buildEdgeWitness,
    edgeWitnessPlan
    )
import MLF.Constraint.Presolution.Expansion (
    decideMinimalExpansion,
    getExpansion,
    mergeExpansions,
    recordEdgeExpansion,
    setExpansion
    )
import MLF.Constraint.Types
import MLF.Constraint.Types.SynthesizedExpVar (isSynthesizedExpVar)

-- | Execute a resolved edge plan.
executeEdgePlan :: EdgePlan -> PresolutionM ()
executeEdgePlan plan =
    catchError (executeUnifiedExpansionPath plan) (throwError . toExecError)

-- | Wrap non-tagged interpreter errors at the phase boundary.
toExecError :: PresolutionError -> PresolutionError
toExecError err@(ExecError _) = err
toExecError err = ExecError err

-- | Unified expansion-oriented execution path.
--
-- Frontend TyExp edges use minimal-expansion solving.
-- Synthesized wrappers still force identity expansion and apply
-- direct-instantiation semantics on the wrapper body.
executeUnifiedExpansionPath :: EdgePlan -> PresolutionM ()
executeUnifiedExpansionPath plan = do
    let leftTyExp = eprLeftTyExp plan
        edge = eprEdge plan
        edgeId = instEdgeId edge
        n1Id = instLeft edge
        n2Id = instRight edge
        n1Raw = resolvedTyExpNode leftTyExp
        n2 = eprRightNode plan
        s = rteExpVar leftTyExp
        bodyId = rteBodyId leftTyExp
        isSynth = isSynthesizedExpVar s

    currentExp <- getExpansion s
    (reqExp, unifications) <-
        if isSynth
            then pure (ExpIdentity, [(bodyId, n2Id)])
            else decideMinimalExpansion (eprAllowTrivial plan) n1Raw n2

    finalExp <-
        if isSynth
            then pure ExpIdentity
            else mergeExpansions s currentExp reqExp

    setExpansion s finalExp
    recordEdgeExpansion edgeId finalExp

    let applyInstantiationUnification =
            if isSynth
                then uncurry solveNonExpInstantiation
                else uncurry unifyStructure
    mapM_ applyInstantiationUnification unifications

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
