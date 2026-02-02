{- |
Module      : MLF.Elab.Phi.Env
Description : Environment helpers for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines the shared environment and accessors used by the
witness-to-instantiation translation (phi). It keeps the ReaderT plumbing
in one place so the main Phi module can remain a thin facade.
-}
module MLF.Elab.Phi.Env (
    PhiEnv(..),
    PhiM,
    askResult,
    askCanonical,
    askCopyMap,
    askInvCopyMap,
    askGaParents,
    askTrace
) where

import Control.Monad.Reader (ReaderT, asks)
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (NodeId)
import MLF.Constraint.Presolution (EdgeTrace)
import MLF.Constraint.Solve (SolveResult)
import MLF.Elab.Generalize (GaBindParents)
import MLF.Elab.Types (ElabError)

-- | Environment for Phi translation operations.
--
-- This record holds the shared context needed for translating graph witnesses
-- to xMLF instantiations. It enables future refactoring of the large
-- 'phiFromEdgeWitnessWithTrace' function to use explicit parameter passing
-- via a ReaderT monad.
--
-- Fields:
--   * peResult: The solve result containing the constraint and union-find
--   * peCanonical: Function to get the canonical node id
--   * peCopyMap: Copy mapping from edge trace (original -> copied)
--   * peInvCopyMap: Inverse copy mapping (copied -> original)
--   * peGaParents: Optional generalized binding parents
--   * peTrace: Optional edge trace with interior/copy info
data PhiEnv = PhiEnv
    { peResult :: SolveResult
    , peCanonical :: NodeId -> NodeId
    , peCopyMap :: IntMap.IntMap NodeId
    , peInvCopyMap :: IntMap.IntMap NodeId
    , peGaParents :: Maybe GaBindParents
    , peTrace :: Maybe EdgeTrace
    }

-- | Monad for Phi translation operations.
--
-- PhiM is a ReaderT over Either ElabError, providing access to the PhiEnv
-- and short-circuiting error handling.
type PhiM = ReaderT PhiEnv (Either ElabError)

-- | Get the SolveResult from the environment.
askResult :: PhiM SolveResult
askResult = asks peResult

-- | Get the canonical node function from the environment.
askCanonical :: PhiM (NodeId -> NodeId)
askCanonical = asks peCanonical

-- | Get the copy map from the environment.
askCopyMap :: PhiM (IntMap.IntMap NodeId)
askCopyMap = asks peCopyMap

-- | Get the inverse copy map (copied -> original) from the environment.
askInvCopyMap :: PhiM (IntMap.IntMap NodeId)
askInvCopyMap = asks peInvCopyMap

-- | Get the optional GaBindParents from the environment.
askGaParents :: PhiM (Maybe GaBindParents)
askGaParents = asks peGaParents

-- | Get the optional EdgeTrace from the environment.
askTrace :: PhiM (Maybe EdgeTrace)
askTrace = asks peTrace
