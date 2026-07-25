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
    EdgeExecutionWitnessContext (..),
    prepareEdgeExecutionDecision,
    recordEdgeExecutionExpansion,
    prepareEdgeExecutionWitness,
    runEdgeExecutionExpansionUnify,
    recordEdgeExecutionTrace,
    recordEdgeExecutionWitness,
    sourceRaiseAuthorityNodes,
  )
where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Strict (gets)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Base
  ( EdgeSourceInterior (..),
    EdgeTrace (..),
    InteriorNodes (..),
    PresolutionError (..),
    PresolutionM,
    PresolutionState (..),
    EdgeExecutionArtifacts (..),
    edgeInteriorExact,
    getConstraint,
    instantiationBindersM,
  )
import MLF.Constraint.Presolution.EdgeProcessing.Plan
import MLF.Constraint.Presolution.EdgeProcessing.Solve
  ( recordEdgeExecutionArtifacts
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
    setExpansion,
  )
import MLF.Constraint.Presolution.Witness
  ( EdgeWitnessInput (..),
    EdgeWitnessPlan (..),
    buildEdgeTrace,
    buildEdgeWitness,
    binderArgsFromKnownBinders,
    edgeWitnessInstanceOp,
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
    eedSourceInterior :: EdgeSourceInterior,
    eedLockedSourceNodes :: IntSet.IntSet,
    eedSourceRaiseAuthorityNodes :: IntSet.IntSet,
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

data EdgeExecutionOutcome
  = EdgeExecutionFreshOutcome
  | EdgeExecutionReplayNoop
  deriving (Eq, Show)

-- | Execute a resolved edge plan.
executeEdgePlan :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p ()
executeEdgePlan = executeEdgePlanWithoutTraceCanonicalization

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
    -- A replay decision is admitted only when the recorded expansion,
    -- endpoints, source root, witness, and trace still describe this exact
    -- edge.  Preserve those artifacts verbatim: rerunning identity structural
    -- equalities can erase a previously recorded source-domain Raise.
    Just _previousTrace -> pure EdgeExecutionReplayNoop
    Nothing ->
      executeFreshDecision decision

executeFreshDecision :: EdgeExecutionDecision -> PresolutionM p EdgeExecutionOutcome
executeFreshDecision decision = do
  recordEdgeExecutionExpansion decision
  witnessContext <- prepareEdgeExecutionWitness decision
  expansionResult <- runEdgeExecutionExpansionUnify witnessContext
  trace <- recordEdgeExecutionTrace witnessContext expansionResult
  recordEdgeExecutionWitness witnessContext expansionResult trace
  pure EdgeExecutionFreshOutcome

{-# INLINABLE prepareEdgeExecutionDecision #-}
prepareEdgeExecutionDecision :: (NodeId -> NodeId) -> EdgePlan -> PresolutionM p EdgeExecutionDecision
prepareEdgeExecutionDecision canonical plan = do
  mbReplayDecision <- prepareRecordedEdgeExecutionDecision plan
  case mbReplayDecision of
    Just decision -> pure decision
    Nothing -> prepareFreshEdgeExecutionDecision canonical plan

lockedNodesInSourceInterior
  :: EdgeSourceInterior
  -> PresolutionM p IntSet.IntSet
lockedNodesInSourceInterior (EdgeSourceInterior (InteriorNodes sourceKeys)) = do
  constraint <- getConstraint
  pure $
    IntSet.filter
      ( \sourceKey ->
          Binding.nodeKind constraint (typeRef (NodeId sourceKey))
            == Right Binding.NodeLocked
      )
      sourceKeys

sourceRaiseAuthorityNodes
  :: NodeId
  -> EdgeSourceInterior
  -> PresolutionM p IntSet.IntSet
sourceRaiseAuthorityNodes sourceRoot (EdgeSourceInterior (InteriorNodes sourceKeys)) = do
  constraint <- getConstraint
  pure $
    IntSet.filter
      ( \sourceKey ->
          let source = NodeId sourceKey
           in source /= sourceRoot
                && transitivelyFlexBoundTo constraint sourceRoot source
      )
      sourceKeys
  where
    -- Only semantic Raise work receives a certificate.  A restricted source
    -- is the identity case in the paper and must be removed by construction,
    -- not retained as though it had an all-flexible path.
    transitivelyFlexBoundTo constraint authorityRoot = go IntSet.empty
      where
        go visited source
          | source == authorityRoot = True
          | IntSet.member (getNodeId source) visited = False
          | otherwise =
              case Binding.lookupBindParent constraint (typeRef source) of
                Just (TypeRef parent, BindFlex) ->
                  go (IntSet.insert (getNodeId source) visited) parent
                _ -> False

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
  bodyInterior <- edgeInteriorExact (medBodyRoot minimal)
  let sourceInterior =
        EdgeSourceInterior . InteriorNodes $
          IntSet.union
            bodyInterior
            (IntSet.fromList (map getNodeId binderArgVars))
  lockedSourceNodes <- lockedNodesInSourceInterior sourceInterior
  raiseAuthorityNodes <-
    sourceRaiseAuthorityNodes (medBodyRoot minimal) sourceInterior
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
        eedSourceInterior = sourceInterior,
        eedLockedSourceNodes = lockedSourceNodes,
        eedSourceRaiseAuthorityNodes = raiseAuthorityNodes,
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
  canonical <- getCanonical
  currentExp <- getExpansion s
  (expectedBodyRoot, _expectedBinders) <-
    instantiationBindersM ownerGen (rteBodyId leftTyExp)
  case IntMap.lookup edgeKey (psEdgeExecutionArtifacts st) of
    Just artifacts
      | recordedExp == currentExp
      , ewEdgeId recordedWitness == edgeId
      , ewLeft recordedWitness == instLeft edge
      , ewRight recordedWitness == instRight edge
      , canonical (ewRoot recordedWitness) == canonical expectedBodyRoot
      , canonical (etRoot previousTrace) == canonical expectedBodyRoot -> do
          let bodyRoot = etRoot previousTrace
              binderArgs = etBinderArgs previousTrace
              boundVars0 = map fst binderArgs
          lockedSourceNodes <-
            lockedNodesInSourceInterior (etInterior previousTrace)
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
                  eedSourceInterior = etInterior previousTrace,
                  eedLockedSourceNodes = lockedSourceNodes,
                  eedSourceRaiseAuthorityNodes = raiseAuthorityNodes,
                  eedBoundVars = boundVars0,
                  eedBinderArgs = binderArgs,
                  eedReplayTrace = Just previousTrace
                }
      where
        recordedExp = eeaExpansion artifacts
        recordedWitness = eeaWitness artifacts
        raiseAuthorityNodes = eeaRaiseAuthorityNodes artifacts
        previousTrace = eeaTrace artifacts
    Just _ ->
      throwError
        ( InternalError
            ( "conflicting committed edge execution artifacts for replay "
                ++ show edgeId
            )
        )
    Nothing -> pure Nothing

{-# INLINE recordEdgeExecutionExpansion #-}
recordEdgeExecutionExpansion :: EdgeExecutionDecision -> PresolutionM p ()
recordEdgeExecutionExpansion decision =
  case eedLeftRaw decision of
    TyExp {tnExpVar = s} -> do
      setExpansion s (eedFinalExpansion decision)
    _ ->
      throwError (InternalError ("recordEdgeExecutionExpansion: expected TyExp for edge " ++ show (eedEdgeId decision)))

{-# INLINABLE prepareEdgeExecutionWitness #-}
prepareEdgeExecutionWitness :: EdgeExecutionDecision -> PresolutionM p EdgeExecutionWitnessContext
prepareEdgeExecutionWitness decision = do
  witnessPlan <- edgeWitnessPlanFromBinders (eedBoundVars decision) (eedFinalExpansion decision)
  constraint <- getConstraint
  let sourceNodeKeys =
        IntSet.fromList
          [ getNodeId (tnId node)
          | node <- NodeAccess.allNodes constraint
          ]
  let witnessInput =
        EdgeWitnessInput
          { ewiEdgeId = eedEdgeId decision,
            ewiSrcNode = eedLeftNodeId decision,
            ewiTgtNode = eedRightNodeId decision,
            ewiRoot = eedBodyRoot decision,
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
            eeiSourceInterior = eedSourceInterior decision,
            eeiLockedSourceNodes = eedLockedSourceNodes decision,
            eeiSourceRaiseAuthorityNodes = eedSourceRaiseAuthorityNodes decision,
            eeiSourceNodeKeys = sourceNodeKeys,
            eeiBoundVars = eedBoundVars decision,
            eeiBinderArgs = eedBinderArgs decision,
            eeiStructuralUnifications = eedUnifications decision
          }
  pure
    EdgeExecutionWitnessContext
      { eewDecision = decision,
        eewWitnessPlan = witnessPlan,
        eewWitnessInput = witnessInput,
        eewExpansionInput = expansionInput
      }

runEdgeExecutionExpansionUnify :: EdgeExecutionWitnessContext -> PresolutionM p EdgeExpansionResult
runEdgeExecutionExpansionUnify context =
  runExpansionUnify
    (eewExpansionInput context)
    (map edgeWitnessInstanceOp (ewpBaseOps (eewWitnessPlan context)))

recordEdgeExecutionTrace
  :: EdgeExecutionWitnessContext
  -> EdgeExpansionResult
  -> PresolutionM p EdgeTrace
recordEdgeExecutionTrace context expansionResult = do
  let decision = eewDecision context
  tr <-
    buildEdgeTrace
      (eedBodyRoot decision)
      (eedSourceInterior decision)
      (eedBinderArgs decision)
      (eerResultRoot expansionResult)
      (eerCopyMap expansionResult)
  pure tr

recordEdgeExecutionWitness
  :: EdgeExecutionWitnessContext
  -> EdgeExpansionResult
  -> EdgeTrace
  -> PresolutionM p ()
recordEdgeExecutionWitness context expansionResult tr = do
  let extraOps = eerExtraOps expansionResult
      decision = eewDecision context
      witnessPlan = eewWitnessPlan context
  (w, nonSourceOpOrigins) <-
    buildEdgeWitness (eewWitnessInput context) (ewpBaseOps witnessPlan) extraOps
  recordEdgeExecutionArtifacts
    (eedEdgeId decision)
    EdgeExecutionArtifacts
      { eeaExpansion = eedFinalExpansion decision,
        eeaWitness = w,
        eeaRaiseAuthorityNodes = eedSourceRaiseAuthorityNodes decision,
        eeaNonSourceOpOrigins = nonSourceOpOrigins,
        eeaExpansionConstruction = eerConstruction expansionResult,
        eeaTrace = tr
      }
