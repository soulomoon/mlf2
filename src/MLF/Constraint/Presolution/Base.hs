module MLF.Constraint.Presolution.Base (
    PresolutionResult(..),
    PresolutionError(..),
    PresolutionState(..),
    EdgeTrace(..),
    CopyMap,
    InteriorSet,
    emptyTrace,
    unionTrace,
    PresolutionM,
    runPresolutionM,
    requireValidBindingTree,
    edgeInteriorExact,
    orderedBindersM,
    forallSpecM
) where

import Control.Monad.State (StateT, gets, runStateT)
import Control.Monad.Except (throwError)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import qualified MLF.Util.UnionFind as UnionFind

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
    , prEdgeTraces :: IntMap EdgeTrace
    , prRedirects :: IntMap NodeId -- ^ Map from old TyExp IDs to their replacement IDs
    } deriving (Eq, Show)

-- | Errors that can occur during presolution.
data PresolutionError
    = UnmatchableTypes NodeId NodeId String  -- ^ Type mismatch during expansion
    | UnresolvedExpVar ExpVarId              -- ^ ExpVar couldn't be resolved
    | ArityMismatch String Int Int           -- ^ (context, expected, actual)
    | InstantiateOnNonForall NodeId          -- ^ Tried to instantiate a non-forall node
    | NodeLookupFailed NodeId                -- ^ Missing node in constraint
    | OccursCheckPresolution NodeId NodeId   -- ^ Unification would make node reachable from itself
    | BindingTreeError BindingError          -- ^ Invalid binding tree when binding edges are in use
    | InternalError String                   -- ^ Unexpected internal state
    deriving (Eq, Show)

-- | State maintained during the presolution process.
data PresolutionState = PresolutionState
    { psConstraint :: Constraint
    , psPresolution :: Presolution
    , psUnionFind :: IntMap NodeId
    , psNextNodeId :: Int
    , psPendingWeakens :: IntSet.IntSet
    , psEdgeExpansions :: IntMap Expansion
    , psEdgeWitnesses :: IntMap EdgeWitness
    , psEdgeTraces :: IntMap EdgeTrace
    }
    deriving (Eq, Show)

-- | Per-edge provenance for instantiation-related operations.
--
-- This is an internal aid for gradually aligning presolution witnesses with
-- `papers/xmlf.txt`’s normalized instance-operation language (Fig. 10). For now,
-- we only track the binder↦argument pairing chosen by `ExpInstantiate`.
data EdgeTrace = EdgeTrace
    { etRoot :: NodeId
    , etBinderArgs :: [(NodeId, NodeId)] -- ^ (binder node, instantiation argument node)
    , etInterior :: IntSet.IntSet -- ^ Nodes in I(r) (exact, from the binding tree).
    , etCopyMap :: IntMap NodeId -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

type CopyMap = IntMap NodeId
type InteriorSet = IntSet.IntSet

emptyTrace :: (CopyMap, InteriorSet)
emptyTrace = (IntMap.empty, IntSet.empty)

unionTrace :: (CopyMap, InteriorSet) -> (CopyMap, InteriorSet) -> (CopyMap, InteriorSet)
unionTrace (m1, s1) (m2, s2) = (IntMap.union m1 m2, IntSet.union s1 s2)

-- | The Presolution monad.
type PresolutionM = StateT PresolutionState (Either PresolutionError)

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM :: PresolutionState -> PresolutionM a -> Either PresolutionError (a, PresolutionState)
runPresolutionM st action = runStateT action st

requireValidBindingTree :: PresolutionM ()
requireValidBindingTree = do
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.checkBindingTreeUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

edgeInteriorExact :: NodeId -> PresolutionM IntSet.IntSet
edgeInteriorExact root0 = do
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.interiorOfUnder canonical c0 root0 of
        Left err -> throwError (BindingTreeError err)
        Right interior -> pure interior

orderedBindersM :: NodeId -> PresolutionM [NodeId]
orderedBindersM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.orderedBinders canonical c0 binder0 of
        Left err -> throwError (BindingTreeError err)
        Right binders -> pure binders

forallSpecM :: NodeId -> PresolutionM ForallSpec
forallSpecM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.forallSpecFromForall canonical c0 binder0 of
        Left err -> throwError (BindingTreeError err)
        Right fs -> pure fs
