module MLF.Frontend.ConstraintGen.State (
    ScopeFrame(..),
    BuildState(..),
    ConstraintM,
    runConstraintM,
    mkInitialState,
    buildConstraint
) where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import MLF.Frontend.ConstraintGen.Types (ConstraintError)

data ScopeFrame = ScopeFrame
  { sfNodes :: !IntSet.IntSet
  }

data BuildState = BuildState
    { bsNextNode :: !Int           -- ^ Next available NodeId
    , bsNextExpVar :: !Int         -- ^ Next available ExpVarId
    , bsNextEdge :: !Int           -- ^ Next available EdgeId
    , bsNodes :: !(IntMap.IntMap TyNode) -- ^ Map of all allocated type nodes
    , bsInstEdges :: ![InstEdge]   -- ^ Instantiation edges (accumulated in reverse)
    , bsUnifyEdges :: ![UnifyEdge] -- ^ Unification edges (accumulated in reverse)
    , bsBindParents :: !BindParents -- ^ Binding edges: child -> (parent, flag)
    , bsVarBounds :: !(IntMap.IntMap (Maybe NodeId)) -- ^ Dedicated variable-bound store
    , bsScopes :: ![ScopeFrame]    -- ^ Stack of scopes tracking newly created nodes
    }

type ConstraintM = StateT BuildState (Except ConstraintError)

runConstraintM :: ConstraintM a -> BuildState -> Either ConstraintError (a, BuildState)
runConstraintM action st = runExcept (runStateT action st)

mkInitialState :: BuildState
mkInitialState = BuildState
    { bsNextNode = 0
    , bsNextExpVar = 0
    , bsNextEdge = 0
    , bsNodes = IntMap.empty
    , bsInstEdges = []
    , bsUnifyEdges = []
    , bsBindParents = IntMap.empty
    , bsVarBounds = IntMap.empty
    , bsScopes = [ScopeFrame IntSet.empty]
    }

buildConstraint :: BuildState -> Constraint
buildConstraint st = Constraint
    { cNodes = bsNodes st
    , cInstEdges = reverse (bsInstEdges st)
    , cUnifyEdges = reverse (bsUnifyEdges st)
    , cBindParents = bsBindParents st
    , cVarBounds = bsVarBounds st
    , cEliminatedVars = IntSet.empty
    }

