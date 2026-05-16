{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Finalize.TestSupport (
    presolutionViewFromSolved,
    stepPruneSolvedBindParents
) where

import MLF.Constraint.Presolution.View (PresolutionView)
import qualified MLF.Constraint.Finalize.Internal as Internal
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Phase (Phase(Raw))

presolutionViewFromSolved :: Solved.Solved -> PresolutionView 'Raw
presolutionViewFromSolved = Internal.buildPresolutionViewFromSolved

stepPruneSolvedBindParents :: Solved.Solved -> Solved.Solved
stepPruneSolvedBindParents = Internal.pruneSolvedBindParents
