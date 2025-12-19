{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.OmegaExec
Description : Shared executor for base ω operations in χe
Copyright   : (c) 2024
License     : BSD-3-Clause

This module centralizes execution of the “base” instance operations induced by
`ExpInstantiate` on the expansion graph χe:

  - `Graft(σ, n)`  (recorded as `OpGraft sigmaRoot n`)
  - `Merge(n, m)`  (recorded as `OpMerge n m`)
  - `Weaken(n)`    (recorded as `OpWeaken n`)

Paper anchor: `papers/xmlf.txt` §3.4 (normalized witnesses, ω operations).

This executor is intentionally parametric in its effects: presolution provides
callbacks for “set binder bound”, “unify without emitting merge ops”, and the
bookkeeping for binder elimination.
-}
module MLF.OmegaExec (
    OmegaExecEnv(..),
    executeOmegaBaseOpsPre,
    executeOmegaBaseOpsPost,
) where

import Control.Monad (forM_, when)

import MLF.Types

data OmegaExecEnv m = OmegaExecEnv
    { omegaMetaFor :: NodeId -> m NodeId
    , omegaLookupMeta :: NodeId -> m (Maybe NodeId)
    , omegaSetVarBound :: NodeId -> Maybe NodeId -> m ()
    , omegaDropVarBind :: NodeId -> m ()
    , omegaUnifyNoMerge :: NodeId -> NodeId -> m ()
    , omegaRecordEliminate :: NodeId -> m ()
    , omegaIsEliminated :: NodeId -> m Bool
    , omegaEliminatedBinders :: m [NodeId]
    , omegaWeakenMeta :: NodeId -> m ()
    }

-- | Execute base ω ops in a “pre” phase (before edge-local unification).
--
-- Paper alignment (`papers/xmlf.txt` §3.4): weakening is delayed until after
-- other operations on nodes below the weakened binder, so this phase executes
-- only graft/merge-like base ops.
executeOmegaBaseOpsPre :: Monad m => OmegaExecEnv m -> [InstanceOp] -> m ()
executeOmegaBaseOpsPre env ops0 = go ops0
  where
    go = \case
        [] -> pure ()
        (OpGraft sigma bv : rest) -> do
            meta <- omegaMetaFor env bv
            omegaSetVarBound env meta (Just sigma)
            go rest
        (OpMerge bv bound : rest) -> do
            meta <- omegaMetaFor env bv
            metaBound <- omegaMetaFor env bound
            omegaUnifyNoMerge env meta metaBound
            omegaDropVarBind env meta
            omegaRecordEliminate env bv
            go rest
        (_ : rest) ->
            go rest

-- | Execute base ω ops in a “post” phase (after edge-local unification).
--
-- This phase performs `Weaken` and also applies binder elimination bookkeeping
-- for binders eliminated by merge-like ops.
executeOmegaBaseOpsPost :: Monad m => OmegaExecEnv m -> [InstanceOp] -> m ()
executeOmegaBaseOpsPost env ops0 = do
    -- Drop binder-metas eliminated by phase-2 merge-like ops, so they won't be
    -- re-quantified during reification/elaboration.
    elims <- omegaEliminatedBinders env
    forM_ elims $ \bid -> do
        mbMeta <- omegaLookupMeta env bid
        case mbMeta of
            Nothing -> pure ()
            Just meta -> omegaDropVarBind env meta

    forM_ ops0 $ \case
        OpWeaken bv -> do
            eliminated <- omegaIsEliminated env bv
            when (not eliminated) $ do
                meta <- omegaMetaFor env bv
                omegaWeakenMeta env meta
                omegaRecordEliminate env bv
        _ -> pure ()
