{-# LANGUAGE DataKinds #-}
{-|
Phase 5 — Global unification (Rémy–Yakobowski, TLDI 2007; ICFP 2008 §3/§5)

`MLF.Constraint.Solve` stays the public façade/export owner for solve-phase
unification. The implementation is split across focused child modules:

* `MLF.Constraint.Solve.Worklist` — worklist draining, occurs-check, and per-edge
  unification decisions.
* `MLF.Constraint.Solve.Harmonize` — generalized equivalence-class harmonization
  via batch rebind.
* `MLF.Constraint.Solve.Finalize` — canonical rewrite, parent repair, eliminated
  binder rewriting, and solved-graph validation.
-}
module MLF.Constraint.Solve (
    SolveError(..),
    UnifyClosureResult(..),
    SolveOutput,
    runUnifyClosure,
    solveUnify,
    solveUnifyWithSnapshot,
    finalizeConstraintWithUF,
    validateSolvedGraph,
    validateSolvedGraphStrict,
    rewriteConstraintWithUF,
    repairNonUpperParents,
    frWith
) where

import MLF.Constraint.Solve.Finalize (finalizeConstraintWithUF, frWith, repairNonUpperParents, rewriteConstraintWithUF, validateSolvedGraph, validateSolvedGraphStrict)
import MLF.Constraint.Solve.Internal (SolveOutput)
import MLF.Constraint.Solve.Output (solveOutputWithSnapshot)
import MLF.Constraint.Solve.Worklist (SolveError(..), UnifyClosureResult(..), runUnifyClosure)
import MLF.Constraint.Types.Graph (Constraint)
import MLF.Constraint.Types.Phase (Phase(Presolved))
import qualified MLF.Constraint.Solved as Solved
import MLF.Util.Trace (TraceConfig)

-- | Drain all unification edges; assumes instantiation work was already done
-- by earlier phases. Returns the solved abstraction for post-solve queries.
solveUnify :: TraceConfig -> Constraint 'Presolved -> Either SolveError Solved.Solved
solveUnify traceCfg c0 = solveUnifyWithSnapshot traceCfg c0 >>= Solved.fromSolveOutput

-- | `solveUnify` plus a pre-rewrite snapshot for staged equivalence-backend construction.
--   Consumes a presolved constraint and returns a presolved output suitable for
--   'MLF.Constraint.Solved.fromSolveOutput'.
solveUnifyWithSnapshot :: TraceConfig -> Constraint 'Presolved -> Either SolveError (SolveOutput 'Presolved)
solveUnifyWithSnapshot = solveOutputWithSnapshot
