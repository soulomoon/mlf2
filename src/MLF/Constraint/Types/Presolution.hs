{- |
Module      : MLF.Constraint.Types.Presolution
Description : Presolution-only constraint types
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines presolution-only constraint types so callers can depend on
the smaller presolution surface without importing the full constraint facade.
-}
module MLF.Constraint.Types.Presolution (
    Presolution(..),
    SolverState(..),
    DepGraph(..)
) where

import Data.IntMap.Strict (IntMap)

import MLF.Constraint.Types.Graph (Constraint, EdgeId, NodeId)
import MLF.Constraint.Types.Witness (Expansion)

-- | A presolution assignment map.
--
-- This maps each expansion variable to its corresponding expansion recipe.
--
-- Paper terminology: a presolution is a solved instance of a constraint (χᵖ)
-- where instantiation edges are solved by choosing expansions and performing
-- the required unifications. In this codebase, the solved *graph* lives in the
-- rewritten `Constraint`; this `Presolution` value is the persistent “recipe
-- assignment” slice that later phases (notably elaboration) may consult.
newtype Presolution = Presolution { getAssignments :: IntMap Expansion }
    deriving (Eq, Show)

-- | A convenience bundle for the “full pipeline” state.
--
-- This is not the only way to run phases, but it is a useful snapshot for
-- debugging and for pipeline-style entry points.
data SolverState = SolverState
    { -- | The current constraint graph.
      ssConstraint :: Constraint
    , -- | The presolution assignment map.
      ssPresolution :: Presolution
    , -- | The union-find map.
      ssUnionFind :: IntMap NodeId
    , -- | The dependency graph.
      ssDepGraph :: DepGraph EdgeId
    }
    deriving (Eq, Show)

-- | A directed dependency graph.
--
-- Used for ordering instantiation edges prior to presolution.
data DepGraph a = DepGraph
    { -- | The vertices of the graph.
      dgVertices :: [a]
    , -- | The edges of the graph.
      dgEdges :: IntMap [a]
    }
    deriving (Eq, Show)
