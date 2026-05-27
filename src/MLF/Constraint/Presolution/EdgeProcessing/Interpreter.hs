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
    executeEdgePlanWithoutTraceCanonicalization,
    EdgeExecutionDecision (..),
    EdgeExecutionWitnessContext (..),
    prepareEdgeExecutionDecision,
    recordEdgeExecutionExpansion,
    unifyEdgeExecutionStructure,
    prepareEdgeExecutionWitness,
    runEdgeExecutionExpansionUnify,
    recordEdgeExecutionTrace,
    recordEdgeExecutionWitness,
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
  ( MinimalExpansionDecision (..),
    decideMinimalExpansionDetailed,
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
    binderArgsFromKnownBinders,
    edgeWitnessPlanFromBinders,
    filterTyVarBinders,
  )
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness

data EdgeExecutionDecision = EdgeExecutionDecision
  { eedEdgeId :: EdgeId,
    eedLeftNodeId :: NodeId,
    eedRightNodeId :: NodeId,
    eedLeftRaw :: TyNode,
    eedRightRaw :: TyNode,
    eedOwnerGen :: GenNodeId,
    eedFinalExpansion :: Expansion,
    eedUnifications :: [(NodeId, NodeId)],
    eedBodyRoot :: NodeId,
    eedBoundVars :: [NodeId],
    eedBinderArgs :: [(NodeId, NodeId)]
  }

data EdgeExecutionWitnessContext = EdgeExecutionWitnessContext
  { eewDecision :: EdgeExecutionDecision,
    eewWitnessPlan :: EdgeWitnessPlan,
    eewWitnessInput :: EdgeWitnessInput,
    eewExpansionInput :: EdgeExpansionInput
  }

-- | Execute a resolved edge plan.
executeEdgePlan :: EdgePlan -> PresolutionM p ()
executeEdgePlan plan = do
  executeEdgePlanWithoutTraceCanonicalization plan
  canonicalizeEdgeTraceInteriorsM

executeEdgePlanWithoutTraceCanonicalization :: EdgePlan -> PresolutionM p ()
executeEdgePlanWithoutTraceCanonicalization plan =
  catchError (executeUnifiedExpansionPath plan) (throwError . toExecError)

-- | Wrap non-tagged interpreter errors at the phase boundary.
toExecError :: PresolutionError -> PresolutionError
toExecError err@(ExecError _) = err
toExecError err = ExecError err

-- | Unified expansion-oriented execution path.
--
-- Frontend TyExp edges all use the same minimal-expansion + unification flow.
executeUnifiedExpansionPath :: EdgePlan -> PresolutionM p ()
executeUnifiedExpansionPath plan = do
  decision <- prepareEdgeExecutionDecision plan
  recordEdgeExecutionExpansion decision
  unifyEdgeExecutionStructure decision
  witnessContext <- prepareEdgeExecutionWitness decision
  expansionResult <- runEdgeExecutionExpansionUnify witnessContext
  recordEdgeExecutionTrace witnessContext expansionResult
  recordEdgeExecutionWitness witnessContext expansionResult

prepareEdgeExecutionDecision :: EdgePlan -> PresolutionM p EdgeExecutionDecision
prepareEdgeExecutionDecision plan = do
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
  minimal <- decideMinimalExpansionDetailed ownerGen (eprAllowTrivial plan) n1Raw n2
  finalExp <- mergeExpansions s currentExp (medExpansion minimal)
  binderArgVars <- filterTyVarBinders (medBoundVars minimal)
  binderArgs <-
    binderArgsFromKnownBinders
      "prepareEdgeExecutionDecision/ExpInstantiate"
      binderArgVars
      finalExp
  pure
    EdgeExecutionDecision
      { eedEdgeId = edgeId,
        eedLeftNodeId = n1Id,
        eedRightNodeId = n2Id,
        eedLeftRaw = n1Raw,
        eedRightRaw = n2,
        eedOwnerGen = ownerGen,
        eedFinalExpansion = finalExp,
        eedUnifications = medUnifications minimal,
        eedBodyRoot = medBodyRoot minimal,
        eedBoundVars = medBoundVars minimal,
        eedBinderArgs = binderArgs
      }

recordEdgeExecutionExpansion :: EdgeExecutionDecision -> PresolutionM p ()
recordEdgeExecutionExpansion decision =
  case eedLeftRaw decision of
    TyExp {tnExpVar = s} -> do
      setExpansion s (eedFinalExpansion decision)
      recordEdgeExpansion (eedEdgeId decision) (eedFinalExpansion decision)
    _ ->
      throwError (InternalError ("recordEdgeExecutionExpansion: expected TyExp for edge " ++ show (eedEdgeId decision)))

unifyEdgeExecutionStructure :: EdgeExecutionDecision -> PresolutionM p ()
unifyEdgeExecutionStructure decision =
  mapM_ (uncurry unifyStructure) (eedUnifications decision)

prepareEdgeExecutionWitness :: EdgeExecutionDecision -> PresolutionM p EdgeExecutionWitnessContext
prepareEdgeExecutionWitness decision = do
  witnessPlan <- edgeWitnessPlanFromBinders (eedBoundVars decision) (eedFinalExpansion decision)
  let witnessInput =
        EdgeWitnessInput
          { ewiEdgeId = eedEdgeId decision,
            ewiSrcNode = eedLeftNodeId decision,
            ewiTgtNode = eedRightNodeId decision,
            ewiLeftRaw = eedLeftRaw decision,
            ewiDepth = ewpForallIntros witnessPlan
          }
      expansionInput =
        EdgeExpansionInput
          { eeiGenId = eedOwnerGen decision,
            eeiEdgeId = eedEdgeId decision,
            eeiLeftRaw = eedLeftRaw decision,
            eeiRightRaw = eedRightRaw decision,
            eeiExpansion = eedFinalExpansion decision,
            eeiBodyRoot = eedBodyRoot decision,
            eeiBoundVars = eedBoundVars decision,
            eeiBinderArgs = eedBinderArgs decision
          }
  pure
    EdgeExecutionWitnessContext
      { eewDecision = decision,
        eewWitnessPlan = witnessPlan,
        eewWitnessInput = witnessInput,
        eewExpansionInput = expansionInput
      }

runEdgeExecutionExpansionUnify :: EdgeExecutionWitnessContext -> PresolutionM p EdgeExpansionResult
runEdgeExecutionExpansionUnify context = do
  expansionResult <-
    if eedFinalExpansion (eewDecision context) == ExpIdentity
      then pure EdgeExpansionResult {eerTrace = emptyTrace, eerExtraOps = []}
      else runExpansionUnify (eewExpansionInput context) (ewpBaseOps (eewWitnessPlan context))
  pure expansionResult

recordEdgeExecutionTrace :: EdgeExecutionWitnessContext -> EdgeExpansionResult -> PresolutionM p ()
recordEdgeExecutionTrace context expansionResult = do
  let expTrace = eerTrace expansionResult
      decision = eewDecision context
  tr <-
    buildEdgeTrace
      (eewWitnessInput context)
      (eedOwnerGen decision)
      (eedBinderArgs decision)
      expTrace
  recordEdgeTrace (eedEdgeId decision) tr

recordEdgeExecutionWitness :: EdgeExecutionWitnessContext -> EdgeExpansionResult -> PresolutionM p ()
recordEdgeExecutionWitness context expansionResult = do
  let extraOps = eerExtraOps expansionResult
      decision = eewDecision context
      witnessPlan = eewWitnessPlan context
  w <- buildEdgeWitness (eewWitnessInput context) (ewpBaseOps witnessPlan) extraOps
  recordEdgeWitness (eedEdgeId decision) w
