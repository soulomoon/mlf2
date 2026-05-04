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
    SolveSnapshot(..),
    SolveOutput(..),
    SolveResult(..),
    runUnifyClosure,
    solveUnify,
    solveUnifyWithSnapshot,
    solveResultFromSnapshot,
    finalizeConstraintWithUF,
    validateSolvedGraph,
    validateSolvedGraphStrict,
    rewriteConstraintWithUF,
    repairNonUpperParents,
    frWith
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve.Finalize (finalizeConstraintWithUF, frWith, repairNonUpperParents, rewriteConstraintWithUF, validateSolvedGraph, validateSolvedGraphStrict)
import MLF.Constraint.Solve.Internal (SolveResult(..))
import MLF.Constraint.Solve.Worklist (SolveError(..), UnifyClosureResult(..), runUnifyClosure)
import MLF.Constraint.Types.Graph (Constraint, NodeId(..), cBindParents, nodeRefKey, typeRef)
import MLF.Util.Trace (TraceConfig, traceBinding)

-- | Snapshot of solve state right before final canonical rewriting.
data SolveSnapshot = SolveSnapshot
    { snapUnionFind :: IntMap NodeId
    , snapPreRewriteConstraint :: Constraint
    }
    deriving (Eq, Show)

-- | Full solve output with:
--   * `soSnapshot`: primary data for staged `Solved` construction.
--   * `soResult`: legacy compatibility payload for `SolveResult` call sites.
data SolveOutput = SolveOutput
    { soResult :: SolveResult
    , soSnapshot :: SolveSnapshot
    }
    deriving (Eq, Show)

-- | Drain all unification edges; assumes instantiation work was already done
-- by earlier phases. Returns the rewritten constraint and the final UF map.
solveUnify :: TraceConfig -> Constraint -> Either SolveError SolveResult
solveUnify traceCfg c0 = soResult <$> solveUnifyWithSnapshot traceCfg c0

-- | Rebuild a validated legacy solve result from a pre-rewrite snapshot.
-- This keeps snapshot-driven consumers independent of `soResult.srConstraint`.
solveResultFromSnapshot :: SolveSnapshot -> Either SolveError SolveResult
solveResultFromSnapshot SolveSnapshot { snapUnionFind = uf, snapPreRewriteConstraint = preRewrite } =
    finalizeConstraintWithUF uf preRewrite

-- | `solveUnify` plus a pre-rewrite snapshot for staged equivalence-backend construction.
solveUnifyWithSnapshot :: TraceConfig -> Constraint -> Either SolveError SolveOutput
solveUnifyWithSnapshot traceCfg c0 = do
    let debugSolveBinding = traceBinding traceCfg
        c0' = repairNonUpperParents c0
        probeIds = [NodeId 2, NodeId 3]
        probeInfo =
            [ ( pid
              , NodeAccess.lookupNode c0' pid
              , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents c0')
              )
            | pid <- probeIds
            ]
    case debugSolveBinding ("solveUnify: pre-check probe " ++ show probeInfo) () of
        () -> pure ()
    closure <- runUnifyClosure traceCfg c0'
    let snapshot =
            SolveSnapshot
                { snapUnionFind = ucUnionFind closure
                , snapPreRewriteConstraint = ucConstraint closure
                }
    res <- solveResultFromSnapshot snapshot
    let solvedConstraint = srConstraint res
        probeInfo' =
            [ ( pid
              , NodeAccess.lookupNode solvedConstraint pid
              , IntMap.lookup (nodeRefKey (typeRef pid)) (cBindParents solvedConstraint)
              )
            | pid <- probeIds
            ]
    case Binding.checkBindingTree solvedConstraint of
        Left err -> Left (BindingTreeError err)
        Right () -> do
            case debugSolveBinding ("solveUnify: post-check probe " ++ show probeInfo') () of
                () -> pure ()
            pure SolveOutput { soResult = res, soSnapshot = snapshot }
