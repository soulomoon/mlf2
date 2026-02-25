module MLF.Constraint.Solve.Internal (
    SolveResult(..)
) where

import Data.IntMap.Strict (IntMap)
import MLF.Constraint.Types (Constraint, NodeId)

-- | Successful unification result.
--
-- Internal representation module; most callers should use
-- 'MLF.Constraint.Solved' queries instead of direct field access.
data SolveResult = SolveResult
    { srConstraint :: Constraint   -- ^ Constraint rewritten to canonical node ids; unify edges drained.
    , srUnionFind :: IntMap NodeId -- ^ Final union-find parent map.
    }
    deriving (Eq, Show)
