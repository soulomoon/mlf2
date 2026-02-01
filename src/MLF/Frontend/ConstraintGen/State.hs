module MLF.Frontend.ConstraintGen.State (
    ScopeFrame(..),
    BuildState(..),
    ConstraintM,
    runConstraintM,
    mkInitialState,
    mkInitialStateWithPolySyms,
    buildConstraint
) where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Constraint.Types
import MLF.Frontend.ConstraintGen.Types (ConstraintError)

data ScopeFrame = ScopeFrame
  { sfNodes :: !IntSet.IntSet
  }

data BuildState = BuildState
    { bsNextNode :: !Int           -- ^ Next available NodeId
    , bsNextGen :: !Int            -- ^ Next available GenNodeId
    , bsNextExpVar :: !Int         -- ^ Next available ExpVarId
    , bsNextEdge :: !Int           -- ^ Next available EdgeId
    , bsNodes :: !(IntMap.IntMap TyNode) -- ^ Map of all allocated type nodes
    , bsInstEdges :: ![InstEdge]   -- ^ Instantiation edges (accumulated in reverse)
    , bsUnifyEdges :: ![UnifyEdge] -- ^ Unification edges (accumulated in reverse)
    , bsBindParents :: !BindParents -- ^ Binding edges: child -> (parent, flag)
    , bsGenNodes :: !(GenNodeMap GenNode) -- ^ Gen nodes (paper G constructors)
    , bsPolySyms :: !PolySyms -- ^ Polymorphic type constructors (paper Poly)
    , bsScopes :: ![ScopeFrame]    -- ^ Stack of scopes tracking newly created nodes
    , bsLetEdges :: !IntSet.IntSet -- ^ Let-scope instantiation edges (body → trivial root)
    }

type ConstraintM = StateT BuildState (Except ConstraintError)

runConstraintM :: ConstraintM a -> BuildState -> Either ConstraintError (a, BuildState)
runConstraintM action st = runExcept (runStateT action st)

mkInitialState :: BuildState
mkInitialState = mkInitialStateWithPolySyms Set.empty

mkInitialStateWithPolySyms :: PolySyms -> BuildState
mkInitialStateWithPolySyms polySyms = BuildState
    { bsNextNode = 0
    , bsNextGen = 0
    , bsNextExpVar = 0
    , bsNextEdge = 0
    , bsNodes = IntMap.empty
    , bsInstEdges = []
    , bsUnifyEdges = []
    , bsBindParents = IntMap.empty
    , bsGenNodes = fromListGen []
    , bsPolySyms = polySyms
    , bsScopes = [ScopeFrame IntSet.empty]
    , bsLetEdges = IntSet.empty
    }

buildConstraint :: BuildState -> Constraint
buildConstraint st = Constraint
    { cNodes = NodeMap (bsNodes st)
    , cInstEdges = reverse (bsInstEdges st)
    , cUnifyEdges = reverse (bsUnifyEdges st)
    , cBindParents = bsBindParents st
    , cPolySyms = bsPolySyms st
    , cEliminatedVars = IntSet.empty
    , cWeakenedVars = IntSet.empty
    , cAnnEdges = IntSet.empty
    , cLetEdges = bsLetEdges st
    , cGenNodes = bsGenNodes st
    }
