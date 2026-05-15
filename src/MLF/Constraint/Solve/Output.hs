{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Solve.Output
    ( solveOutputWithSnapshot
    ) where

import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve.Finalize (finalizeConstraintWithUF, repairNonUpperParents)
import MLF.Constraint.Solve.Internal (SolveOutput(..), SolveResult(..), SolveSnapshot(..))
import MLF.Constraint.Solve.Worklist (SolveError(..), runUnifyClosure, UnifyClosureResult(..))
import MLF.Constraint.Types.Graph (Constraint, NodeId(..), cBindParents, nodeRefKey, typeRef)
import MLF.Util.Trace (TraceConfig, traceBinding)

-- | Drain unification and keep the pre-rewrite snapshot needed to build the
-- solved backend.
solveOutputWithSnapshot :: TraceConfig -> Constraint p -> Either SolveError (SolveOutput p)
solveOutputWithSnapshot traceCfg c0 = do
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
    SolveResult { srConstraint = solvedConstraint } <-
        finalizeConstraintWithUF
            (snapUnionFind snapshot)
            (snapPreRewriteConstraint snapshot)
    let probeInfo' =
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
            pure SolveOutput { soSnapshot = snapshot }
