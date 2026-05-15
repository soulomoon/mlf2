{-# LANGUAGE DataKinds #-}

module MLF.Constraint.Solved.TestSupport
    ( mkTestSolved
    ) where

import Data.IntMap.Strict (IntMap)

import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved.Internal as Internal
import MLF.Constraint.Types.Graph (Constraint, NodeId)
import MLF.Constraint.Types.Phase (Phase(Raw))

-- | Test-only constructor for low-level solved fixtures that deliberately do
-- not pass full solver finalization.
mkTestSolved :: Constraint 'Raw -> IntMap NodeId -> Solved
mkTestSolved = Internal.mkTestSolved
