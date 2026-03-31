-- |
-- Module      : MLF.Constraint.Presolution.EdgeProcessing.Interpreter
-- Description : Pass B — execute typed edge plans
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- The interpreter (pass B of the two-pass architecture) executes a resolved
-- 'EdgePlan' using a single expansion-oriented execution path.
module MLF.Constraint.Presolution.EdgeProcessing.Interpreter
  ( executeEdgePlan,
  )
where

import Control.Monad.Except (catchError, throwError)
import MLF.Constraint.Presolution.Base (PresolutionError (..), PresolutionM, emptyTrace)
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Solve
  ( canonicalizeEdgeTraceInteriorsM,
    recordEdgeTrace,
    recordEdgeWitness,
    unifyStructure,
  )
import MLF.Constraint.Presolution.EdgeProcessing.Unify
  ( EdgeExpansionInput (..),
    EdgeExpansionResult (..),
    runExpansionUnify,
  )
import MLF.Constraint.Presolution.Expansion
  ( decideMinimalExpansion,
    getExpansion,
    mergeExpansions,
    recordEdgeExpansion,
    setExpansion,
  )
import MLF.Constraint.Presolution.Witness
  ( EdgeWitnessInput (..),
    EdgeWitnessPlan (..),
    buildEdgeTrace,
    buildEdgeWitness,
    edgeWitnessPlan,
  )
import MLF.Constraint.Types

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
-- Frontend TyExp edges all use the same minimal-expansion + unification flow.
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
      ownerGen = eprSchemeOwnerGen plan

  currentExp <- getExpansion s
  (reqExp, unifications) <- decideMinimalExpansion ownerGen (eprAllowTrivial plan) n1Raw n2
  finalExp <- mergeExpansions s currentExp reqExp

  setExpansion s finalExp
  recordEdgeExpansion edgeId finalExp

  mapM_ (uncurry unifyStructure) unifications

  witnessPlan <- edgeWitnessPlan ownerGen n1Id n1Raw finalExp
  let witnessInput =
        EdgeWitnessInput
          { ewiEdgeId = edgeId,
            ewiSrcNode = n1Id,
            ewiTgtNode = n2Id,
            ewiLeftRaw = n1Raw,
            ewiDepth = ewpForallIntros witnessPlan
          }
      expansionInput =
        EdgeExpansionInput
          { eeiGenId = ownerGen,
            eeiEdgeId = edgeId,
            eeiLeftRaw = n1Raw,
            eeiRightRaw = n2,
            eeiExpansion = finalExp
          }
  expansionResult <-
    if finalExp == ExpIdentity
      then pure EdgeExpansionResult {eerTrace = emptyTrace, eerExtraOps = []}
      else runExpansionUnify expansionInput (ewpBaseOps witnessPlan)

  let expTrace = eerTrace expansionResult
      extraOps = eerExtraOps expansionResult
  tr <- buildEdgeTrace witnessInput ownerGen finalExp expTrace
  recordEdgeTrace edgeId tr
  w <- buildEdgeWitness witnessInput (ewpBaseOps witnessPlan) extraOps
  recordEdgeWitness edgeId w
  canonicalizeEdgeTraceInteriorsM
