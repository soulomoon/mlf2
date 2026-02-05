{- |
Module      : MLF.Elab.Phi
Description : Facade for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module re-exports the public Phi translation helpers from the
split submodules under "MLF.Elab.Phi.*".
-}
module MLF.Elab.Phi (
    -- * Re-exports from Context
    contextToNodeBound,
    -- * Main entry points
    phiFromEdgeWitnessWithTrace,
    -- * Test/debug-only entry points (no trace required; not for production paths)
    phiFromEdgeWitnessNoTrace,
    -- * Legacy alias (deprecated)
    phiFromEdgeWitness,
    -- * Phi environment
    PhiEnv(..),
    PhiM,
    askResult,
    askCanonical,
    askCopyMap,
    askGaParents,
    askTrace,
    -- * Extracted helpers (using PhiM)
    canonicalNodeM,
    remapSchemeInfoM,
    isBinderNodeM,
    lookupBinderIndexM,
    binderIndexM,
    binderNameForM
) where

import MLF.Elab.Phi.Binder (binderIndexM, binderNameForM, isBinderNodeM, lookupBinderIndexM)
import MLF.Elab.Phi.Context (contextToNodeBound)
import MLF.Elab.Phi.Env (PhiEnv(..), PhiM, askCanonical, askCopyMap, askGaParents, askResult, askTrace)
import MLF.Elab.Phi.Translate (canonicalNodeM, phiFromEdgeWitness, phiFromEdgeWitnessNoTrace, phiFromEdgeWitnessWithTrace, remapSchemeInfoM)
