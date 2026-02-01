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
import Data.Maybe (mapMaybe)

import qualified MLF.Util.Order as Order
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.Validation (translatableWeakenedNodes)
import MLF.Constraint.Presolution.Witness (
    normalizeInstanceStepsFull,
    OmegaNormalizeEnv(OmegaNormalizeEnv, oneRoot),
    )
import qualified MLF.Constraint.Presolution.Witness as Witness

-- | Normalize edge witnesses against the finalized presolution constraint.
normalizeEdgeWitnessesM :: PresolutionM ()
normalizeEdgeWitnessesM = do
    c0 <- gets psConstraint
    traces <- gets psEdgeTraces
    witnesses0 <- gets psEdgeWitnesses
    let rewriteNodeWith copyMap nid =
            IntMap.findWithDefault nid (getNodeId nid) (getCopyMapping copyMap)
        weakenedOps =
            IntSet.fromList
                [ getNodeId (rewriteNodeWith copyMap n)
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
                    [ (getNodeId (rewriteNode bv), rewriteNode arg)
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
        let interiorRoot = typeRef edgeRoot
            orderBase = edgeRoot
            orderRoot = orderBase
            traceInteriorKeys =
                case traceInterior of
                    InteriorNodes s -> s
        interiorExact <-
            if IntSet.null traceInteriorKeys
                then case Binding.interiorOf c0 interiorRoot of
                    Left err -> throwError (BindingTreeError err)
                    Right s ->
                        pure $
                            IntSet.fromList
                                [ getNodeId nid
                                | key <- IntSet.toList s
                                , TypeRef nid <- [nodeRefFromKey key]
                                ]
                else pure traceInteriorKeys
        let interiorNorm =
                -- Normalize against an expansion-aware interior so ops on copied nodes
                -- (e.g., escaping binder metas) are retained for Raise/Merge witnesses.
                IntSet.union interiorExact $
                    IntSet.fromList (map getNodeId (copiedNodes copyMap))
        let steps0 = map rewriteStep (ewSteps w0)
            orderKeys0 = Order.orderKeysFromConstraintWith id c0 orderRoot Nothing
            opTargets = \case
                OpGraft sigma n -> [sigma, n]
                OpWeaken n -> [n]
                OpMerge n m -> [n, m]
                OpRaise n -> [n]
                OpRaiseMerge n m -> [n, m]
            missingKeys =
                IntSet.fromList
                    [ getNodeId n
                    | StepOmega op <- steps0
                    , n <- opTargets op
                    , not (IntMap.member (getNodeId n) orderKeys0)
                    ]
            orderKeys =
                if IntSet.null missingKeys
                    then orderKeys0
                    else
                        -- Defensive: if an op targets a node outside the ≺ traversal,
                        -- assign a stable ordering key so normalization can proceed.
                        let existingFirsts =
                                mapMaybe
                                    (\k -> case Order.okPath k of
                                        [] -> Nothing
                                        (x:_) -> Just x
                                    )
                                    (IntMap.elems orderKeys0)
                            base =
                                case existingFirsts of
                                    [] -> 0
                                    _ -> maximum existingFirsts + 1
                            extraKeys =
                                IntMap.fromList
                                    [ (nidInt, Order.OrderKey { Order.okDepth = 0, Order.okPath = [base, idx] })
                                    | (idx, nidInt) <- zip [0 ..] (IntSet.toList missingKeys)
                                    ]
                        in IntMap.union orderKeys0 extraKeys
            env =
                OmegaNormalizeEnv
                    { oneRoot = edgeRoot
                    , Witness.interior = interiorNorm
                    , Witness.weakened = weakened
                    , Witness.orderKeys = orderKeys
                    , Witness.canonical = id
                    , Witness.constraint = c0
                    , Witness.binderArgs = binderArgs
                    }
        steps <- case normalizeInstanceStepsFull env steps0 of
            Right steps' -> pure steps'
            Left err ->
                throwError (InternalError ("normalizeInstanceStepsFull failed: " ++ show err))
        let stepsFinal = map restoreStep steps
            ops = [op | StepOmega op <- stepsFinal]
        pure (eid, w0 { ewSteps = stepsFinal, ewWitness = InstanceWitness ops })
    modify' $ \st -> st { psEdgeWitnesses = IntMap.fromList witnesses }
