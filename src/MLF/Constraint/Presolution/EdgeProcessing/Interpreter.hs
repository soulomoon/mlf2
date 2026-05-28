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
    executeEdgePlanWithoutTraceCanonicalizationWithOutcome,
    EdgeExecutionDecision (..),
    EdgeExecutionOutcome (..),
    EdgeExecutionReplay (..),
    EdgeExecutionWitnessContext (..),
    prepareEdgeExecutionDecision,
    recordEdgeExecutionExpansion,
    unifyEdgeExecutionStructure,
    prepareEdgeExecutionWitness,
    edgeExecutionReplay,
    recordEdgeExecutionReplayTraceFromDecision,
    recordEdgeExecutionReplayTrace,
    runEdgeExecutionExpansionUnify,
    recordEdgeExecutionTrace,
    recordEdgeExecutionWitness,
  )
where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Strict (gets)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.Presolution.Base
  ( EdgeTrace (..),
    PresolutionError (..),
    PresolutionM,
    PresolutionState (..),
    emptyTrace,
  )
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Solve
  ( canonicalizeEdgeTraceInteriorsWith,
    recordEdgeTrace,
    recordEdgeWitness,
    unifyStructure,
  )
import MLF.Constraint.Presolution.StateAccess (getCanonical)
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
    eedBinderArgs :: [(NodeId, NodeId)],
    eedReplayTrace :: Maybe EdgeTrace
  }

data EdgeExecutionWitnessContext = EdgeExecutionWitnessContext
  { eewDecision :: EdgeExecutionDecision,
    eewWitnessPlan :: EdgeWitnessPlan,
    eewWitnessInput :: EdgeWitnessInput,
    eewExpansionInput :: EdgeExpansionInput
  }

data EdgeExecutionReplay
  = EdgeExecutionFresh
  | EdgeExecutionReplay !EdgeTrace

data EdgeExecutionOutcome
  = EdgeExecutionFreshOutcome
  | EdgeExecutionReplayTraceRebuilt
  | EdgeExecutionReplayNoop
  deriving (Eq, Show)

-- | Execute a resolved edge plan.
executeEdgePlan :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p ()
executeEdgePlan canonical plan = do
  executeEdgePlanWithoutTraceCanonicalization canonical plan
  canonical' <- getCanonical
  canonicalizeEdgeTraceInteriorsWith canonical' (instEdgeId (eprEdge plan))

executeEdgePlanWithoutTraceCanonicalization :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p ()
executeEdgePlanWithoutTraceCanonicalization canonical plan =
  () <$ executeEdgePlanWithoutTraceCanonicalizationWithOutcome canonical plan

executeEdgePlanWithoutTraceCanonicalizationWithOutcome :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p EdgeExecutionOutcome
executeEdgePlanWithoutTraceCanonicalizationWithOutcome canonical plan =
  catchError (executeUnifiedExpansionPath canonical plan) (throwError . toExecError)

-- | Wrap non-tagged interpreter errors at the phase boundary.
toExecError :: PresolutionError -> PresolutionError
toExecError err@(ExecError _) = err
toExecError err = ExecError err

-- | Unified expansion-oriented execution path.
--
-- Frontend TyExp edges all use the same minimal-expansion + unification flow.
executeUnifiedExpansionPath :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p EdgeExecutionOutcome
executeUnifiedExpansionPath canonical plan = do
  decision <- prepareEdgeExecutionDecision canonical plan
  case eedReplayTrace decision of
    Just previousTrace -> do
      recordEdgeExecutionExpansion decision
      unifyEdgeExecutionStructure decision
      witnessContext <- prepareEdgeExecutionWitness decision
      recordEdgeExecutionReplayTrace witnessContext previousTrace
      pure EdgeExecutionReplayTraceRebuilt
    Nothing ->
      executeFreshDecision decision

executeFreshDecision :: EdgeExecutionDecision -> PresolutionM p EdgeExecutionOutcome
executeFreshDecision decision = do
  recordEdgeExecutionExpansion decision
  unifyEdgeExecutionStructure decision
  witnessContext <- prepareEdgeExecutionWitness decision
  expansionResult <- runEdgeExecutionExpansionUnify witnessContext
  recordEdgeExecutionTrace witnessContext expansionResult
  recordEdgeExecutionWitness witnessContext expansionResult
  pure EdgeExecutionFreshOutcome

{-# INLINABLE prepareEdgeExecutionDecision #-}
prepareEdgeExecutionDecision :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p EdgeExecutionDecision
prepareEdgeExecutionDecision canonical plan = do
  mbReplayDecision <- prepareRecordedEdgeExecutionDecision plan
  case mbReplayDecision of
    Just decision -> pure decision
    Nothing -> prepareFreshEdgeExecutionDecision canonical plan

prepareFreshEdgeExecutionDecision :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p EdgeExecutionDecision
prepareFreshEdgeExecutionDecision canonical plan = do
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
  minimal <- decideMinimalExpansionDetailed canonical ownerGen (eprAllowTrivial plan) n1Raw n2
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
        eedBinderArgs = binderArgs,
        eedReplayTrace = Nothing
      }

prepareRecordedEdgeExecutionDecision :: EdgePlan -> PresolutionM p (Maybe EdgeExecutionDecision)
prepareRecordedEdgeExecutionDecision plan = do
  let leftTyExp = eprLeftTyExp plan
      edge = eprEdge plan
      edgeId = instEdgeId edge
      edgeKey = getEdgeId edgeId
      n1Raw = resolvedTyExpNode leftTyExp
      n2 = eprRightNode plan
      s = rteExpVar leftTyExp
      ownerGen = eprSchemeOwnerGen plan
  st <- gets id
  currentExp <- getExpansion s
  case ( IntMap.lookup edgeKey (psEdgeExpansions st)
       , IntMap.member edgeKey (psEdgeWitnesses st)
       , IntMap.lookup edgeKey (psEdgeTraces st)
       ) of
    (Just recordedExp, True, Just previousTrace)
      | recordedExp /= ExpIdentity
      , recordedExp == currentExp -> do
          let bodyRoot =
                case n1Raw of
                  TyExp {tnBody = bodyId} -> bodyId
                  _ -> tnId n1Raw
              binderArgs = etBinderArgs previousTrace
              boundVars0 = map fst binderArgs
          pure $
            Just
              EdgeExecutionDecision
                { eedEdgeId = edgeId,
                  eedLeftNodeId = instLeft edge,
                  eedRightNodeId = instRight edge,
                  eedLeftRaw = n1Raw,
                  eedRightRaw = n2,
                  eedOwnerGen = ownerGen,
                  eedFinalExpansion = recordedExp,
                  eedUnifications = [],
                  eedBodyRoot = bodyRoot,
                  eedBoundVars = boundVars0,
                  eedBinderArgs = binderArgs,
                  eedReplayTrace = Just previousTrace
                }
    _ ->
      pure Nothing

{-# INLINE recordEdgeExecutionExpansion #-}
recordEdgeExecutionExpansion :: EdgeExecutionDecision -> PresolutionM p ()
recordEdgeExecutionExpansion decision =
  case eedLeftRaw decision of
    TyExp {tnExpVar = s} -> do
      setExpansion s (eedFinalExpansion decision)
      recordEdgeExpansion (eedEdgeId decision) (eedFinalExpansion decision)
    _ ->
      throwError (InternalError ("recordEdgeExecutionExpansion: expected TyExp for edge " ++ show (eedEdgeId decision)))

{-# INLINE unifyEdgeExecutionStructure #-}
unifyEdgeExecutionStructure :: EdgeExecutionDecision -> PresolutionM p ()
unifyEdgeExecutionStructure decision =
  mapM_ (uncurry unifyStructure) (eedUnifications decision)

{-# INLINABLE prepareEdgeExecutionWitness #-}
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

edgeExecutionReplay :: EdgeExecutionWitnessContext -> PresolutionM p EdgeExecutionReplay
edgeExecutionReplay context = do
  st <- gets id
  let decision = eewDecision context
      edgeKey = getEdgeId (eedEdgeId decision)
      mbExpansion = IntMap.lookup edgeKey (psEdgeExpansions st)
      hasWitness = IntMap.member edgeKey (psEdgeWitnesses st)
      mbTrace = IntMap.lookup edgeKey (psEdgeTraces st)
  pure $
    case (mbExpansion, hasWitness, mbTrace) of
      (Just expn, True, Just trace)
        | expn == eedFinalExpansion decision ->
            EdgeExecutionReplay trace
      _ ->
        EdgeExecutionFresh

runEdgeExecutionExpansionUnify :: EdgeExecutionWitnessContext -> PresolutionM p EdgeExpansionResult
runEdgeExecutionExpansionUnify context = do
  expansionResult <-
    if eedFinalExpansion (eewDecision context) == ExpIdentity
      then pure EdgeExpansionResult {eerTrace = emptyTrace, eerExtraOps = []}
      else runExpansionUnify (eewExpansionInput context) (ewpBaseOps (eewWitnessPlan context))
  pure expansionResult

recordEdgeExecutionReplayTrace :: EdgeExecutionWitnessContext -> EdgeTrace -> PresolutionM p ()
recordEdgeExecutionReplayTrace context previousTrace =
  recordEdgeExecutionReplayTraceFromDecision (eewDecision context) previousTrace

recordEdgeExecutionReplayTraceFromDecision :: EdgeExecutionDecision -> EdgeTrace -> PresolutionM p ()
recordEdgeExecutionReplayTraceFromDecision decision previousTrace = do
  let witnessInput =
        EdgeWitnessInput
          { ewiEdgeId = eedEdgeId decision,
            ewiSrcNode = eedLeftNodeId decision,
            ewiTgtNode = eedRightNodeId decision,
            ewiLeftRaw = eedLeftRaw decision,
            ewiDepth = length (eedBinderArgs decision)
          }
  tr <-
    buildEdgeTrace
      witnessInput
      (eedOwnerGen decision)
      (eedBinderArgs decision)
      (etCopyMap previousTrace, IntSet.empty, IntSet.empty)
  recordEdgeTrace (eedEdgeId decision) tr

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
