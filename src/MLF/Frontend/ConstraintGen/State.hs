module MLF.Frontend.ConstraintGen.State
  ( ScopeFrame (..),
    BuildState (..),
    ConstraintM,
    runConstraintM,
    mkInitialState,
    mkInitialStateWithPolySyms,
    buildConstraint,
  )
where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph
import MLF.Frontend.ConstraintGen.Types (Binding, ConstraintError)
import MLF.Frontend.Syntax (NormSrcType)

data ScopeFrame = ScopeFrame
  { sfNodes :: !IntSet.IntSet
  }

data BuildState = BuildState
  { -- | Next available NodeId
    bsNextNode :: !Int,
    -- | Next available GenNodeId
    bsNextGen :: !Int,
    -- | Next available ExpVarId
    bsNextExpVar :: !Int,
    -- | Next available EdgeId
    bsNextEdge :: !Int,
    -- | Map of all allocated type nodes
    bsNodes :: !(IntMap.IntMap TyNode),
    -- | Instantiation edges (accumulated in reverse)
    bsInstEdges :: ![InstEdge],
    -- | Unification edges (accumulated in reverse)
    bsUnifyEdges :: ![UnifyEdge],
    -- | Binding edges: child -> (parent, flag)
    bsBindParents :: !BindParents,
    -- | Gen nodes (paper G constructors)
    bsGenNodes :: !(GenNodeMap GenNode),
    -- | Polymorphic type constructors (paper Poly)
    bsPolySyms :: !PolySyms,
    -- | Stack of scopes tracking newly created nodes
    bsScopes :: ![ScopeFrame],
    -- | Let-scope instantiation edges (body → trivial root)
    bsLetEdges :: !IntSet.IntSet,
    -- | Arity of each type constructor (paper Σ arity function)
    bsTyConArity :: !(Map.Map BaseTy Int),
    -- | Original source annotation types keyed by AAnn codomain NodeId.
    -- Preserves the exact lowered 'NormSrcType' from 'buildCoerce' so that
    -- elaboration can recover annotation types that presolution strips
    -- (e.g. TForall inside a μ body).
    bsAnnSourceTypes :: !(IntMap.IntMap NormSrcType),
    -- | External assumptions materialized selectively for referenced free
    -- variables.  The initial environment keeps compact lazy entries; once a
    -- variable is needed, its graph binding is cached here so later
    -- occurrences share the same scheme graph within this definition.
    bsExternalBindingCache :: !(Map.Map String Binding)
  }

type ConstraintM = StateT BuildState (Except ConstraintError)

runConstraintM :: ConstraintM a -> BuildState -> Either ConstraintError (a, BuildState)
runConstraintM action st = runExcept (runStateT action st)

mkInitialState :: BuildState
mkInitialState = mkInitialStateWithPolySyms Set.empty

mkInitialStateWithPolySyms :: PolySyms -> BuildState
mkInitialStateWithPolySyms polySyms =
  BuildState
    { bsNextNode = 0,
      bsNextGen = 0,
      bsNextExpVar = 0,
      bsNextEdge = 0,
      bsNodes = IntMap.empty,
      bsInstEdges = [],
      bsUnifyEdges = [],
      bsBindParents = IntMap.empty,
      bsGenNodes = fromListGen [],
      bsPolySyms = polySyms,
      bsScopes = [ScopeFrame IntSet.empty],
      bsLetEdges = IntSet.empty,
      bsTyConArity = Map.empty,
      bsAnnSourceTypes = IntMap.empty,
      bsExternalBindingCache = Map.empty
    }

buildConstraint :: BuildState -> Constraint p
buildConstraint st =
  Constraint
    { cNodes = NodeMap (bsNodes st),
      cInstEdges = reverse (bsInstEdges st),
      cUnifyEdges = reverse (bsUnifyEdges st),
      cBindParents = bsBindParents st,
      cPolySyms = bsPolySyms st,
      cEliminatedVars = IntSet.empty,
      cWeakenedVars = IntSet.empty,
      cAnnEdges = IntSet.empty,
      cLetEdges = bsLetEdges st,
      cGenNodes = bsGenNodes st
    }
