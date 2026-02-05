{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.WitnessNorm
Description : Witness normalization for presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module handles normalization of edge witnesses against the finalized
presolution constraint.
-}
module MLF.Constraint.Presolution.WitnessNorm (
    normalizeEdgeWitnessesM
) where

import Control.Monad.State
import Control.Monad.Except (throwError)
import Control.Monad (forM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Util.Order as Order
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)
import MLF.Constraint.Presolution.Validation (translatableWeakenedNodes)
import MLF.Constraint.Presolution.Witness (
    normalizeInstanceStepsFull,
    OmegaNormalizeEnv(OmegaNormalizeEnv, oneRoot),
    validateNormalizedWitness,
    )
import qualified MLF.Constraint.Presolution.Witness as Witness

-- | Normalize edge witnesses against the finalized presolution constraint.
normalizeEdgeWitnessesM :: PresolutionM ()
normalizeEdgeWitnessesM = do
    (c0, canonical) <- getConstraintAndCanonical
    traces <- gets psEdgeTraces
    witnesses0 <- gets psEdgeWitnesses
    let rewriteNodeWith copyMap nid =
            IntMap.findWithDefault nid (getNodeId nid) (getCopyMapping copyMap)
        weakenedOps =
            IntSet.fromList
                [ getNodeId (canonical (rewriteNodeWith copyMap n))
                | (eid, w0) <- IntMap.toList witnesses0
                , let copyMap = maybe mempty etCopyMap (IntMap.lookup eid traces)
                , StepOmega (OpWeaken n) <- ewSteps w0
                ]
        weakened =
            IntSet.union weakenedOps (translatableWeakenedNodes c0)
    witnesses <- forM (IntMap.toList witnesses0) $ \(eid, w0) -> do
        let (edgeRoot, copyMap, binderArgs0, traceInterior) =
                case IntMap.lookup eid traces of
                    Nothing -> (ewRoot w0, mempty, [], mempty)
                    Just tr -> (etRoot tr, etCopyMap tr, etBinderArgs tr, etInterior tr)
            rewriteNode = rewriteNodeWith copyMap
            binderArgs =
                IntMap.fromList
                    [ ( getNodeId (canonical (rewriteNode bv))
                      , canonical (rewriteNode arg)
                      )
                    | (bv, arg) <- binderArgs0
                    ]
            copyToOriginal =
                IntMap.fromListWith min
                    [ (getNodeId copy, NodeId orig)
                    | (orig, copy) <- IntMap.toList (getCopyMapping copyMap)
                    ]
            restoreNode nid =
                IntMap.findWithDefault nid (getNodeId nid) copyToOriginal
            rewriteOp op =
                case op of
                    OpGraft sigma n -> OpGraft (rewriteNode sigma) (rewriteNode n)
                    OpMerge n m -> OpMerge (rewriteNode n) (rewriteNode m)
                    OpRaise n -> OpRaise (rewriteNode n)
                    OpWeaken n -> OpWeaken (rewriteNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (rewriteNode n) (rewriteNode m)
            rewriteStep step =
                case step of
                    StepOmega op -> StepOmega (rewriteOp op)
                    StepIntro -> StepIntro
            restoreOp op =
                case op of
                    OpGraft sigma n -> OpGraft (restoreNode sigma) (restoreNode n)
                    OpMerge n m -> OpMerge (restoreNode n) (restoreNode m)
                    OpRaise n -> OpRaise (restoreNode n)
                    OpWeaken n -> OpWeaken (restoreNode n)
                    OpRaiseMerge n m -> OpRaiseMerge (restoreNode n) (restoreNode m)
            restoreStep step =
                case step of
                    StepOmega op -> StepOmega (restoreOp op)
                    StepIntro -> StepIntro
        let orderBase = edgeRoot
            orderRoot = orderBase
            traceInteriorKeys =
                case traceInterior of
                    InteriorNodes s -> s
        interiorExact <-
            if IntSet.null traceInteriorKeys
                then edgeInteriorExact edgeRoot
                else pure traceInteriorKeys
        let interiorNorm =
                -- Rewrite interior through copyMap so it's in the same node-id space
                -- as the rewritten witness steps (thesis-exact I(r) membership).
                IntSet.fromList
                    [ getNodeId (canonical (rewriteNode (NodeId n)))
                    | n <- IntSet.toList interiorExact
                    ]
        let steps0 = map rewriteStep (ewSteps w0)
            orderKeys0 = Order.orderKeysFromConstraintWith canonical c0 (canonical orderRoot) Nothing
            orderKeys = orderKeys0
            env =
                OmegaNormalizeEnv
                    { oneRoot = canonical edgeRoot
                    , Witness.interior = interiorNorm
                    , Witness.weakened = weakened
                    , Witness.orderKeys = orderKeys
                    , Witness.canonical = canonical
                    , Witness.constraint = c0
                    , Witness.binderArgs = binderArgs
                    }
        steps <- case normalizeInstanceStepsFull env steps0 of
            Right steps' -> pure steps'
            Left err ->
                throwError (WitnessNormalizationError (EdgeId eid) err)
        -- Validate normalized ops against Fig. 15.3.4 invariants (thesis-exact).
        -- IMPORTANT: Validate BEFORE restoring to original node space, since
        -- interiorNorm is in the rewritten node space (matching the normalized steps).
        let opsRewritten = [op | StepOmega op <- steps]
        case validateNormalizedWitness env opsRewritten of
            Left valErr -> throwError (WitnessNormalizationError (EdgeId eid) valErr)
            Right () -> pure ()
        let stepsFinal = map restoreStep steps
            ops = [op | StepOmega op <- stepsFinal]
        pure (eid, w0 { ewSteps = stepsFinal, ewWitness = InstanceWitness ops })
    modify' $ \st -> st { psEdgeWitnesses = IntMap.fromList witnesses }
