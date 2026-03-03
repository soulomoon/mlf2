module MLF.Elab.Run.PipelineBoundary (
    sanitizeSnapshotUf,
    solvedFromSnapshotReplay,
    rebuildSolvedForConstraint
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Solve (SolveError, SolveSnapshot(..), solveResultFromSnapshot)
import qualified MLF.Constraint.Solve as Solve
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types (Constraint, NodeId(..), cNodes, lookupNodeIn)

sanitizeSnapshotUf :: Constraint -> IntMap.IntMap NodeId -> IntMap.IntMap NodeId
sanitizeSnapshotUf preRewrite =
    IntMap.mapMaybeWithKey keepLive
  where
    isLive nid = case lookupNodeIn (cNodes preRewrite) nid of
        Just _ -> True
        Nothing -> False

    keepLive key rep =
        let keyNode = NodeId key
        in if isLive keyNode && isLive rep && keyNode /= rep
            then Just rep
            else Nothing

solvedFromSnapshotReplay :: Constraint -> IntMap.IntMap NodeId -> Either SolveError Solved.Solved
solvedFromSnapshotReplay preRewrite uf =
    Solved.fromPreRewriteState uf preRewrite

rebuildSolvedForConstraint :: Solved.Solved -> Constraint -> Either SolveError Solved.Solved
rebuildSolvedForConstraint solved c' = do
    replayed <-
        solveResultFromSnapshot
            SolveSnapshot
                { snapUnionFind = Solved.canonicalMap solved
                , snapPreRewriteConstraint = c'
                }
    let rebuilt = Solved.rebuildWithConstraint solved (Solve.srConstraint replayed)
    case Solved.validateCanonicalGraphStrict rebuilt of
        [] -> Right rebuilt
        vs -> Left (Solve.ValidationFailed vs)
