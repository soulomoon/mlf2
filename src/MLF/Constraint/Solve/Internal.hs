module MLF.Constraint.Solve.Internal (
    SolveSnapshot(..),
    SolveOutput(..),
    SolveResult(..)
) where

import Data.IntMap.Strict (IntMap)
import MLF.Constraint.Types.Graph (Constraint, NodeId)

-- | Snapshot of solve state right before final canonical rewriting.
data SolveSnapshot p = SolveSnapshot
    { snapUnionFind :: IntMap NodeId
    , snapPreRewriteConstraint :: Constraint p
    }
    deriving (Eq, Show)

-- | Successful unification result.
--
-- Internal representation module; most callers should use
-- 'MLF.Constraint.Solved' queries instead of direct field access.
data SolveResult p = SolveResult { srConstraint :: Constraint p   -- ^ Constraint rewritten to canonical node ids; unify edges drained.
    , srUnionFind :: IntMap NodeId -- ^ Final union-find parent map.
    }
    deriving (Eq, Show)

-- | Full solve output with:
--   * `soSnapshot`: primary data for staged `Solved` construction.
--   * `soResult`: legacy compatibility payload for `SolveResult p` call sites.
data SolveOutput p = SolveOutput
    { soResult :: SolveResult p
    , soSnapshot :: SolveSnapshot p
    }
    deriving (Eq, Show)
