{- |
Module      : MLF.Constraint.Presolution
Description : Phase 4 - Principal Presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module is the public entrypoint for Phase 4 (presolution). The
implementation is split into submodules under `MLF.Constraint.Presolution.*`
to keep responsibilities clearer.
-}
module MLF.Constraint.Presolution (
    -- * Main API
    computePresolution,
    PresolutionResult(..),
    PresolutionView(..),
    PresolutionPlanBuilder(..),
    PresolutionError(..),
    EdgeTrace(..),
) where

import MLF.Constraint.Presolution.Base (
    EdgeTrace(..),
    PresolutionError(..),
    PresolutionPlanBuilder(..),
    PresolutionResult(..)
    )
import MLF.Constraint.Presolution.Driver (computePresolution)
import MLF.Constraint.Presolution.View
    ( PresolutionView(..)
    )
