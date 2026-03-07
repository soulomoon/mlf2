{- |
Module      : MLF.Elab.Phi
Description : Facade for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module re-exports the live Phi translation helpers from the
split submodules under "MLF.Elab.Phi.*".
-}
module MLF.Elab.Phi (
    -- * Re-exports from Context
    contextToNodeBound,
    -- * Main entry point (chi-native callback: no solved-typed arg)
    phiFromEdgeWitnessWithTrace,
    -- * Phi environment
    PhiEnv(..),
    PhiM,
    askCanonical,
    askCopyMap,
    askGaParents,
    askTrace,
    -- * Shared helper
    canonicalNodeM
) where

import MLF.Elab.Phi.Context (contextToNodeBound)
import MLF.Elab.Phi.Env (PhiEnv(..), PhiM, askCanonical, askCopyMap, askGaParents, askTrace)
import MLF.Elab.Phi.Translate (canonicalNodeM, phiFromEdgeWitnessWithTrace)
