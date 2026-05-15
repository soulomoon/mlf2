{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Solve.TestSupport
    ( SolveOutput(..)
    , SolveResult(..)
    , SolveSnapshot(..)
    , solveUnifyResult
    , solveUnifyResultWithSnapshot
    , solveResultFromSnapshot
    ) where

import MLF.Constraint.Solve.Internal (SolveOutput(..), SolveResult(..), SolveSnapshot(..))
import MLF.Constraint.Solve.Finalize (finalizeConstraintWithUF)
import MLF.Constraint.Solve.Output (solveOutputWithSnapshot)
import MLF.Constraint.Solve.Worklist (SolveError)
import MLF.Constraint.Types.Graph (Constraint)
import MLF.Util.Trace (TraceConfig)

-- | Result-shaped helper for low-level solver tests.
solveUnifyResult :: TraceConfig -> Constraint p -> Either SolveError (SolveResult p)
solveUnifyResult traceCfg c0 = do
    out <- solveOutputWithSnapshot traceCfg c0
    solveResultFromSnapshot (soSnapshot out)

-- | Polymorphic snapshot helper for low-level tests that construct constraints
-- outside the presolved phase.
solveUnifyResultWithSnapshot :: TraceConfig -> Constraint p -> Either SolveError (SolveOutput p)
solveUnifyResultWithSnapshot = solveOutputWithSnapshot

-- | Rebuild a validated solve result from a pre-rewrite snapshot.
solveResultFromSnapshot :: SolveSnapshot p -> Either SolveError (SolveResult p)
solveResultFromSnapshot SolveSnapshot { snapUnionFind = uf, snapPreRewriteConstraint = preRewrite } =
    finalizeConstraintWithUF uf preRewrite
