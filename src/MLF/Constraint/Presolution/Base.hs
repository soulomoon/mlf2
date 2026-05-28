{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module MLF.Constraint.Presolution.Base (
    PresolutionUf(..),
    EdgeArtifacts(..),
    emptyEdgeArtifacts,
    PresolutionResult(..),
    PresolutionPlanBuilder(..),
    PresolutionError(..),
    TranslatabilityIssue(..),
    PresolutionState
        ( PresolutionState
        , PresolutionStateInternal
        , psConstraint
        , psPresolution
        , psUnionFind
        , psNextNodeId
        , psPendingWeakens
        , psPendingWeakenOwners
        , psBinderCache
        , psGraphVersion
        , psUnionFindVersion
        , psBindParentsVersion
        , psBindingModelCache
        , psBindingRepairCache
        , psBindingRepairDirty
        , psEdgeExpansions
        , psEdgeWitnesses
        , psEdgeTraces
        ),
    CachedBindingModel(..),
    CachedBindingRepairModel(..),
    emptyBindingRepairDirty,
    dirtyAllBindingRepair,
    modifyConstraintState,
    modifyConstraintDirtyTypesState,
    setConstraintDirtyBindRefsState,
    modifyBindParentsState,
    setBindParentState,
    setConstraintState,
    setUnionFindState,
    modifyUnionFindState,
    mergeUnionFindState,
    compressUnionFindState,
    setBindingModelCacheState,
    PendingWeakenOwner(..),
    pendingWeakenOwnerFromMaybe,
    pendingWeakenOwnerToMaybe,
    EdgeTrace(..),
    CopyMapping(..),
    CopyMap,
    lookupCopy,
    insertCopy,
    copiedNodes,
    originalNodes,
    InteriorNodes(..),
    FrontierNodes(..),
    InteriorSet,
    FrontierSet,
    memberInterior,
    memberFrontier,
    toListInterior,
    toListFrontier,
    fromListInterior,
    fromListFrontier,
    emptyTrace,
    unionTrace,
    PresolutionM,
    runPresolutionM,
    MonadPresolution(..),
    bindingPathToRootUnderM,
    cachedBindingModelM,
    ensureBindingParents,
    ensureBindingParentsWithOutcome,
    BindingRepairOutcome(..),
    requireValidBindingTree,
    edgeInteriorExact,
    interiorOfUnderCachedM,
    traceInteriorRootRef,
    instantiationBindersM,
    instantiationBindersFromGenM,
    forallSpecM,
    dropTrivialSchemeEdges
) where

import Control.Applicative ((<|>))
import Control.Monad.State (MonadState, StateT, get, gets, modify', put, runStateT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad (forM_, void, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import qualified MLF.Binding.Canonicalization as BindingCanonical
import qualified MLF.Binding.Path as BindingPath
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Phase (Phase(Presolved))
import qualified MLF.Constraint.Types.Graph as Types
import MLF.Constraint.Types.Witness (EdgeWitness, Expansion, ForallSpec, ReplayContract)
import MLF.Constraint.Types.Presolution (Presolution, PresolutionSnapshot (..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Util.Order as Order
import qualified MLF.Util.UnionFind as UnionFind
import MLF.Util.Trace (TraceConfig, traceBindingM)
import MLF.Constraint.Presolution.BindingRepair
    ( BindingRepairDirty
    , BindingRepairModel(..)
    , buildBindingRepairModel
    , dirtyAllBindingRepair
    , emptyBindingRepairDirty
    , repairBindingParentsWithModel
    )
import MLF.Constraint.Presolution.Plan (GeneralizePlan, ReifyPlan)
import MLF.Constraint.Presolution.Plan.Context (GaBindParents)
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeError)
import MLF.Util.ElabError (ElabError)

newtype PresolutionUf = PresolutionUf { getPresolutionUf :: IntMap NodeId }
    deriving (Eq, Show)

data EdgeArtifacts = EdgeArtifacts
    { eaEdgeExpansions :: IntMap Expansion
    , eaEdgeWitnesses :: IntMap EdgeWitness
    , eaEdgeTraces :: IntMap EdgeTrace
    }
    deriving (Eq, Show)

emptyEdgeArtifacts :: EdgeArtifacts
emptyEdgeArtifacts = EdgeArtifacts IntMap.empty IntMap.empty IntMap.empty

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint 'Presolved
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
    , prEdgeTraces :: IntMap EdgeTrace
    , prRedirects :: IntMap NodeId -- ^ Map from old TyExp IDs to their replacement IDs
    , prUnionFind :: PresolutionUf
    , prPlanBuilder :: PresolutionPlanBuilder
    } deriving (Eq, Show)

instance PresolutionSnapshot PresolutionResult where
    snapshotConstraint = prConstraint
    snapshotUnionFind = getPresolutionUf . prUnionFind

newtype PresolutionPlanBuilder = PresolutionPlanBuilder
    { ppbBuildGeneralizePlans
        :: forall p.
           PresolutionView p
        -> Maybe (GaBindParents p)
        -> NodeRef
        -> NodeId
        -> Either ElabError (GeneralizePlan p, ReifyPlan)
    }

instance Eq PresolutionPlanBuilder where
    _ == _ = True

instance Show PresolutionPlanBuilder where
    show _ = "<PresolutionPlanBuilder>"

-- | Errors that can occur during presolution.
data PresolutionError
    = UnmatchableTypes NodeId NodeId String  -- ^ Type mismatch during expansion
    | UnresolvedExpVar ExpVarId              -- ^ ExpVar couldn't be resolved
    | ArityMismatch String Int Int           -- ^ (context, expected, actual)
    | InstantiateOnNonForall NodeId          -- ^ Tried to instantiate a non-forall node
    | NodeLookupFailed NodeId                -- ^ Missing node in constraint
    | OccursCheckPresolution NodeId NodeId   -- ^ Unification would make node reachable from itself
    | BindingTreeError BindingError          -- ^ Invalid binding tree when binding edges are in use
    | NonTranslatablePresolution [TranslatabilityIssue]
    | WitnessNormalizationError EdgeId OmegaNormalizeError  -- ^ Normalized witness violates Fig. 15.3.4 invariants
    | ResidualUnifyEdges [UnifyEdge]         -- ^ Presolution artifact must not retain unification work.
    | ResidualInstEdges [InstEdge]           -- ^ Presolution artifact must not retain instantiation edges.
    | ResidualTyExpNodes [NodeId]            -- ^ Presolution artifact must be TyExp-free.
    | MissingEdgeWitnesses [EdgeId]          -- ^ Non-trivial instantiation edges missing witness entries.
    | MissingEdgeTraces [EdgeId]             -- ^ Non-trivial instantiation edges missing trace entries.
    | ExpectedTyExpLeftInPlanner EdgeId TyNode
      -- ^ Planner expected normalized `TyExp <= τ` but saw a different left node.
    | PlanError PresolutionError             -- ^ Error surfaced during planner pass
    | ExecError PresolutionError             -- ^ Error surfaced during interpreter pass
    | InternalError String                   -- ^ Unexpected internal state
    deriving (Eq, Show)

data TranslatabilityIssue
    = InertLockedNodes [NodeId]
    | SchemeRootNotRigid GenNodeId NodeId
    | ArrowNodeNotRigid NodeId
    | TyConNodeNotRigid NodeId
    | NonInteriorNodeNotRigid GenNodeId NodeId
    deriving (Eq, Show)

-- | State maintained during the presolution process.
data CachedBindingModel p = CachedBindingModel
    { cbmGraphVersion :: !Int
    , cbmUnionFindVersion :: !Int
    , cbmBindParentsVersion :: !Int
    , cbmConstraint :: Constraint p
    , cbmUnionFind :: IntMap NodeId
    , cbmQuotient :: Binding.QuotientBindParents
    }
    deriving (Eq, Show)

data CachedBindingRepairModel p = CachedBindingRepairModel
    { cbrmGraphVersion :: !Int
    , cbrmUnionFindVersion :: !Int
    , cbrmModel :: BindingRepairModel p
    }
    deriving (Eq, Show)

data BindingRepairOutcome
    = BindingRepairSkipped
    | BindingRepairFull
    | BindingRepairIncremental
    deriving (Eq, Show)

data PresolutionState p = PresolutionStateInternal
    { psConstraint :: Constraint p
    , psPresolution :: Presolution
    , psUnionFind :: IntMap NodeId
    , psNextNodeId :: Int
    , psPendingWeakens :: IntSet.IntSet
    , psPendingWeakenOwners :: IntMap PendingWeakenOwner
    , psBinderCache :: IntMap [NodeId]
    , psGraphVersion :: !Int
    , psUnionFindVersion :: !Int
    , psBindParentsVersion :: !Int
    , psBindingModelCache :: Maybe (CachedBindingModel p)
    , psBindingRepairCache :: Maybe (CachedBindingRepairModel p)
    , psBindingRepairDirty :: Maybe BindingRepairDirty
    , psEdgeExpansions :: IntMap Expansion
    , psEdgeWitnesses :: IntMap EdgeWitness
    , psEdgeTraces :: IntMap EdgeTrace
    }
    deriving (Eq, Show)

pattern PresolutionState
    :: Constraint p
    -> Presolution
    -> IntMap NodeId
    -> Int
    -> IntSet.IntSet
    -> IntMap PendingWeakenOwner
    -> IntMap [NodeId]
    -> IntMap Expansion
    -> IntMap EdgeWitness
    -> IntMap EdgeTrace
    -> PresolutionState p
pattern PresolutionState constraint presolution unionFind nextNodeId pendingWeakens pendingWeakenOwners binderCache edgeExpansions edgeWitnesses edgeTraces <-
    PresolutionStateInternal
        constraint
        presolution
        unionFind
        nextNodeId
        pendingWeakens
        pendingWeakenOwners
        binderCache
        _
        _
        _
        _
        _
        _
        edgeExpansions
        edgeWitnesses
        edgeTraces
  where
    PresolutionState constraint presolution unionFind nextNodeId pendingWeakens pendingWeakenOwners binderCache edgeExpansions edgeWitnesses edgeTraces =
        PresolutionStateInternal
            constraint
            presolution
            unionFind
            nextNodeId
            pendingWeakens
            pendingWeakenOwners
            binderCache
            0
            0
            0
            Nothing
            Nothing
            (Just dirtyAllBindingRepair)
            edgeExpansions
            edgeWitnesses
            edgeTraces

{-# COMPLETE PresolutionState #-}

invalidateBindingModelState :: PresolutionState p -> PresolutionState p
invalidateBindingModelState st = st { psBindingModelCache = Nothing }

invalidateBindingRepairModelState :: PresolutionState p -> PresolutionState p
invalidateBindingRepairModelState st =
    st
        { psBindingRepairCache = Nothing
        , psBindingRepairDirty = Just dirtyAllBindingRepair
        }

modifyConstraintState :: (Constraint p -> Constraint p) -> PresolutionState p -> PresolutionState p
modifyConstraintState f st =
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psConstraint = f (psConstraint st)
            , psGraphVersion = psGraphVersion st + 1
            , psBindParentsVersion = psBindParentsVersion st + 1
            }

-- | Modify constraint types/nodes without touching bind parents.
--
-- Only bumps 'psGraphVersion'.  The quotient binding-model cache depends on
-- 'psUnionFindVersion' and 'psBindParentsVersion' so it remains valid across
-- type-only mutations.  The repair model is also unaffected.
modifyConstraintDirtyTypesState
    :: IntSet.IntSet
    -> (Constraint p -> Constraint p)
    -> PresolutionState p
    -> PresolutionState p
modifyConstraintDirtyTypesState _dirtyTypes f st =
    st
        { psConstraint = f (psConstraint st)
        , psGraphVersion = psGraphVersion st + 1
        }

setConstraintDirtyBindRefsState
    :: IntSet.IntSet
    -> Constraint p
    -> PresolutionState p
    -> PresolutionState p
setConstraintDirtyBindRefsState _dirtyBindRefs constraint st =
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psConstraint = constraint
            , psBindParentsVersion = psBindParentsVersion st + 1
            }

modifyBindParentsState :: (BindParents -> BindParents) -> PresolutionState p -> PresolutionState p
modifyBindParentsState f st =
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psConstraint = (psConstraint st) { cBindParents = f (cBindParents (psConstraint st)) }
            , psBindParentsVersion = psBindParentsVersion st + 1
            }

setBindParentState :: NodeRef -> (NodeRef, BindFlag) -> PresolutionState p -> PresolutionState p
setBindParentState child parentInfo st =
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psConstraint =
                (psConstraint st)
                    { cBindParents =
                        IntMap.insert
                            (nodeRefKey child)
                            parentInfo
                            (cBindParents (psConstraint st))
                    }
            , psBindParentsVersion = psBindParentsVersion st + 1
            }

setConstraintState :: Constraint p -> PresolutionState p -> PresolutionState p
setConstraintState constraint =
    modifyConstraintState (const constraint)

setUnionFindState :: IntMap NodeId -> PresolutionState p -> PresolutionState p
setUnionFindState unionFind st =
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psUnionFind = unionFind
            , psUnionFindVersion = psUnionFindVersion st + 1
            }

modifyUnionFindState :: (IntMap NodeId -> IntMap NodeId) -> PresolutionState p -> PresolutionState p
modifyUnionFindState f st =
    setUnionFindState (f (psUnionFind st)) st

mergeUnionFindState :: NodeId -> NodeId -> PresolutionState p -> PresolutionState p
mergeUnionFindState fromRoot toRoot st =
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psUnionFind =
                IntMap.insert
                    (getNodeId fromRoot)
                    toRoot
                    (psUnionFind st)
            , psUnionFindVersion = psUnionFindVersion st + 1
            }

-- | Path compression does not change canonical representatives, so cached
-- quotient binding models remain semantically valid.
compressUnionFindState :: IntMap NodeId -> PresolutionState p -> PresolutionState p
compressUnionFindState unionFind st =
    st { psUnionFind = unionFind }

setBindingModelCacheState :: CachedBindingModel p -> PresolutionState p -> PresolutionState p
setBindingModelCacheState cache st =
    st { psBindingModelCache = Just cache }

cachedBindingModelM :: PresolutionM p (Constraint p, NodeId -> NodeId, Binding.QuotientBindParents)
cachedBindingModelM = do
    st <- get
    case psBindingModelCache st of
        Just cached
            | cbmUnionFindVersion cached == psUnionFindVersion st
            , cbmBindParentsVersion cached == psBindParentsVersion st ->
                -- Quotient is valid; return current constraint (types may have
                -- changed via modifyConstraintDirtyTypesState without bumping
                -- psBindParentsVersion).
                pure
                    ( psConstraint st
                    , UnionFind.frWith (cbmUnionFind cached)
                    , cbmQuotient cached
                    )
        _ -> do
            let c0 = psConstraint st
                uf = psUnionFind st
                canonical = UnionFind.frWith uf
            quotient <-
                case Binding.quotientBindParentsContextUnder canonical c0 of
                    Left err -> throwError (BindingTreeError err)
                    Right result -> pure result
            let cached =
                    CachedBindingModel
                        { cbmGraphVersion = psGraphVersion st
                        , cbmUnionFindVersion = psUnionFindVersion st
                        , cbmBindParentsVersion = psBindParentsVersion st
                        , cbmConstraint = c0
                        , cbmUnionFind = uf
                        , cbmQuotient = quotient
                        }
            modify' (setBindingModelCacheState cached)
            pure (c0, canonical, quotient)

-- | Ownership bucket used by owner-aware pending-weaken scheduling.
--
-- `PendingWeakenOwnerUnknown` represents a pending weaken target whose
-- enclosing scheme owner cannot be resolved.
data PendingWeakenOwner
    = PendingWeakenOwnerGen !GenNodeId
    | PendingWeakenOwnerUnknown
    deriving (Eq, Ord, Show)

pendingWeakenOwnerFromMaybe :: Maybe GenNodeId -> PendingWeakenOwner
pendingWeakenOwnerFromMaybe = maybe PendingWeakenOwnerUnknown PendingWeakenOwnerGen

pendingWeakenOwnerToMaybe :: PendingWeakenOwner -> Maybe GenNodeId
pendingWeakenOwnerToMaybe owner = case owner of
    PendingWeakenOwnerGen gid -> Just gid
    PendingWeakenOwnerUnknown -> Nothing

-- | Per-edge provenance for instantiation-related operations.
--
-- Source-ID contract (consumed by Φ):
--   * `EdgeWitness.ewWitness` operation node IDs
--   * `etBinderArgs`
--   * `etCopyMap` keys
--   * `etInterior`
-- all live in one source-ID domain.
--
-- Replay-map contract:
--   * `etBinderReplayMap` maps source binder keys to replay-domain binder
--     nodes selected during presolution normalization.
--   * `etReplayDomainBinders`, when non-empty, records the producer-approved
--     replay-domain TyVar binders that Phase 4 must validate against instead
--     of re-deriving binder scope from `etRoot`.
--   * The map is required, total over the source binder domain (`etBinderArgs`),
--     and injective over replay-domain TyVar binders.
--
-- Canonical IDs are derived locally at lookup sites. Global canonicalization
-- must not rewrite provenance collections across this boundary.
--
-- This is an internal aid for gradually aligning presolution witnesses with
-- `papers/these-finale-english.txt`’s normalized instance-operation language
-- (see `papers/xmlf.txt` Fig. 10). For now, we only track the binder↦argument
-- pairing chosen by `ExpInstantiate`.
data EdgeTrace = EdgeTrace
    { etRoot :: NodeId
    , etBinderArgs :: [(NodeId, NodeId)] -- ^ (binder node, instantiation argument node)
    , etInterior :: InteriorNodes -- ^ Nodes in I(r) (exact, from the binding tree).
    , etReplayContract :: ReplayContract -- ^ Producer-owned replay contract authority.
    , etBinderReplayMap :: IntMap NodeId -- ^ source binder key -> replay-domain binder node
    , etReplayDomainBinders :: [NodeId] -- ^ Explicit replay-domain binders for strict replay lanes.
    , etCopyMap :: CopyMapping -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

newtype CopyMapping = CopyMapping { getCopyMapping :: IntMap NodeId }
    deriving (Eq, Show)

instance Semigroup CopyMapping where
    CopyMapping a <> CopyMapping b = CopyMapping (IntMap.union a b)

instance Monoid CopyMapping where
    mempty = CopyMapping IntMap.empty

lookupCopy :: NodeId -> CopyMapping -> Maybe NodeId
lookupCopy nid (CopyMapping m) = IntMap.lookup (getNodeId nid) m

insertCopy :: NodeId -> NodeId -> CopyMapping -> CopyMapping
insertCopy src dst (CopyMapping m) = CopyMapping (IntMap.insert (getNodeId src) dst m)

originalNodes :: CopyMapping -> [NodeId]
originalNodes (CopyMapping m) = map NodeId (IntMap.keys m)

copiedNodes :: CopyMapping -> [NodeId]
copiedNodes (CopyMapping m) = IntMap.elems m

type CopyMap = CopyMapping
type InteriorSet = IntSet.IntSet
type FrontierSet = IntSet.IntSet

newtype InteriorNodes = InteriorNodes IntSet.IntSet
    deriving (Eq, Show)

instance Semigroup InteriorNodes where
    InteriorNodes a <> InteriorNodes b = InteriorNodes (IntSet.union a b)

instance Monoid InteriorNodes where
    mempty = InteriorNodes IntSet.empty

newtype FrontierNodes = FrontierNodes IntSet.IntSet
    deriving (Eq, Show)

instance Semigroup FrontierNodes where
    FrontierNodes a <> FrontierNodes b = FrontierNodes (IntSet.union a b)

instance Monoid FrontierNodes where
    mempty = FrontierNodes IntSet.empty

memberInterior :: NodeId -> InteriorNodes -> Bool
memberInterior nid (InteriorNodes s) = IntSet.member (getNodeId nid) s

memberFrontier :: NodeId -> FrontierNodes -> Bool
memberFrontier nid (FrontierNodes s) = IntSet.member (getNodeId nid) s

toListInterior :: InteriorNodes -> [NodeId]
toListInterior (InteriorNodes s) = map NodeId (IntSet.toList s)

toListFrontier :: FrontierNodes -> [NodeId]
toListFrontier (FrontierNodes s) = map NodeId (IntSet.toList s)

fromListInterior :: [NodeId] -> InteriorNodes
fromListInterior = InteriorNodes . IntSet.fromList . map getNodeId

fromListFrontier :: [NodeId] -> FrontierNodes
fromListFrontier = FrontierNodes . IntSet.fromList . map getNodeId

emptyTrace :: (CopyMap, InteriorSet, FrontierSet)
emptyTrace = (mempty, IntSet.empty, IntSet.empty)

unionTrace :: (CopyMap, InteriorSet, FrontierSet) -> (CopyMap, InteriorSet, FrontierSet) -> (CopyMap, InteriorSet, FrontierSet)
unionTrace (m1, s1, f1) (m2, s2, f2) =
    (m1 <> m2, IntSet.union s1 s2, IntSet.union f1 f2)

-- | The Presolution monad.
newtype PresolutionM p a = PresolutionM
    { unPresolutionM :: ReaderT TraceConfig (StateT (PresolutionState p) (Either PresolutionError)) a
    }
    deriving (Functor, Applicative, Monad, MonadReader TraceConfig, MonadState (PresolutionState p), MonadError PresolutionError)

-- | Run a PresolutionM action with an initial state (testing helper).
runPresolutionM
    :: TraceConfig
    -> PresolutionState p
    -> PresolutionM p a
    -> Either PresolutionError (a, PresolutionState p)
runPresolutionM cfg st action = runStateT (runReaderT (unPresolutionM action) cfg) st

{- Note [Presolution foundation]
Presolution state access is intentionally layered to keep the core algorithms
paper-faithful while avoiding ad-hoc state plumbing:

  * Preferred abstraction: use the `MonadPresolution` class for functions that
    should work across presolution sub-monads (e.g., `PresolutionM` and
    `EdgeUnifyM`).

  * Preferred helper modules:
      - `MLF.Constraint.Presolution.Ops` for low-level stateful primitives
        (fresh IDs, node registration, union-find roots, variable bounds).
      - `MLF.Constraint.Presolution.StateAccess` for canonical/constraint access
        and binding-tree queries via the shared `PresolutionM` helper style.

  * Avoid adding new direct uses of `gets psConstraint` / `gets psUnionFind`,
    ad-hoc `UnionFind.frWith`, or manual `Binding.*` error lifting in submodules.
    Instead, extend the helper modules above when a common access pattern is
    missing.

Layering plan: direct state access should continue moving toward the
foundation modules as presolution layers converge (see US-019). Once migrated,
redundant helpers will be removed.
-}

-- | Typeclass for monads that support presolution operations.
-- This allows functions to be polymorphic over the concrete monad stack,
-- reducing the need for explicit lift calls.
class Monad m => MonadPresolution m where
    type PresolutionPhaseOf m :: Phase
    -- | Get the current constraint.
    getConstraint :: m (Constraint (PresolutionPhaseOf m))
    -- | Modify the constraint with a function.
    modifyConstraint
        :: (Constraint (PresolutionPhaseOf m) -> Constraint (PresolutionPhaseOf m))
        -> m ()
    -- | Get the full presolution state.
    getPresolutionState :: m (PresolutionState (PresolutionPhaseOf m))
    -- | Put a new presolution state.
    putPresolutionState :: PresolutionState (PresolutionPhaseOf m) -> m ()
    -- | Throw a presolution error.
    throwPresolutionError :: PresolutionError -> m a
    -- | Modify the presolution state with a function.
    modifyPresolution :: (Presolution -> Presolution) -> m ()
    -- | Bind expansion arguments to the appropriate binder.
    -- Used during instantiation to bind copied argument nodes.
    bindExpansionArgs :: NodeId -> [(NodeId, NodeId)] -> m ()

-- | Instance for the concrete PresolutionM monad.
instance {-# OVERLAPPING #-} MonadPresolution (PresolutionM p) where
    type PresolutionPhaseOf (PresolutionM p) = p
    getConstraint = gets psConstraint
    modifyConstraint f = modify' (modifyConstraintState f)
    getPresolutionState = get
    putPresolutionState = put
    throwPresolutionError = throwError
    modifyPresolution f = modify' $ \st -> st { psPresolution = f (psPresolution st) }
    bindExpansionArgs expansionRoot pairs = do
        (c0, canonical, quotient) <- cachedBindingModelM
        let expansionRootC = canonical expansionRoot
            bindParents = Binding.qbpBindParents quotient
            rootGen =
                let genIds = IntMap.keys (getGenNodeMap (cGenNodes c0))
                    pickRoot acc gidInt =
                        case acc of
                            Just _ -> acc
                            Nothing ->
                                let gref = genRef (GenNodeId gidInt)
                                in case IntMap.lookup (nodeRefKey gref) bindParents of
                                    Nothing -> Just gref
                                    Just _ -> Nothing
                in foldl' pickRoot Nothing genIds
        forM_ pairs $ \(_bv, arg) -> do
            let argC = canonical arg
            case IntMap.lookup (nodeRefKey (typeRef argC)) bindParents of
                Just _ -> pure ()
                Nothing ->
                    case rootGen of
                        Just gref ->
                            modify' $
                                setBindParentState
                                    (typeRef argC)
                                    (gref, BindFlex)
                        Nothing ->
                            when (Binding.isUpper c0 (typeRef expansionRootC) (typeRef argC)) $
                                modify' $
                                    setBindParentState
                                        (typeRef argC)
                                        (typeRef expansionRootC, BindFlex)

bindingPathToRootUnderM
    :: (NodeId -> NodeId)
    -> Constraint q
    -> NodeRef
    -> PresolutionM p [NodeRef]
bindingPathToRootUnderM _canonical _c start = do
    (_c0, canonical, qbp) <- cachedBindingModelM
    let startC = case start of
            TypeRef nid -> TypeRef (canonical nid)
            GenRef _    -> start
    case BindingPath.bindingPathToRootLocal
            (Binding.qbpBindParents qbp) startC of
        Left err -> throwError (BindingTreeError err)
        Right path -> pure path

requireValidBindingTree :: PresolutionM p ()
requireValidBindingTree = do
    ensureBindingParents
    c0 <- gets psConstraint
    uf <- gets psUnionFind
    let canonical = UnionFind.frWith uf
    case Binding.checkBindingTreeUnder canonical c0 of
        Left err -> throwError (BindingTreeError err)
        Right () -> pure ()

ensureBindingParents :: PresolutionM p ()
ensureBindingParents =
    void ensureBindingParentsWithOutcome

ensureBindingParentsWithOutcome :: PresolutionM p BindingRepairOutcome
ensureBindingParentsWithOutcome = do
    st0 <- get
    let c0 = psConstraint st0
        uf = psUnionFind st0
        canonical = UnionFind.frWith uf
    qbp0 <-
        case Binding.quotientBindParentsContextUnder canonical c0 of
            Left err -> throwError (BindingTreeError err)
            Right result -> pure result
    let bp0 = Binding.qbpBindParents qbp0
        repairModel = buildBindingRepairModel canonical c0
        bp1 = repairBindingParentsWithModel repairModel bp0
        changed = bp1 /= cBindParents c0
        cFinal = c0 { cBindParents = bp1 }
        bindParentsVersion =
            if changed
                then psBindParentsVersion st0 + 1
                else psBindParentsVersion st0
        qbpFinal =
            qbp0
                { Binding.qbpBindParents = bp1
                , Binding.qbpChildrenByParent = quotientChildrenByParent bp1
                }
        bindingCache =
            CachedBindingModel
                { cbmGraphVersion = psGraphVersion st0
                , cbmUnionFindVersion = psUnionFindVersion st0
                , cbmBindParentsVersion = bindParentsVersion
                , cbmConstraint = cFinal
                , cbmUnionFind = uf
                , cbmQuotient = qbpFinal
                }
        stFinal =
            st0
                { psConstraint = cFinal
                , psBindParentsVersion = bindParentsVersion
                , psBindingModelCache = Just bindingCache
                , psBindingRepairCache = Nothing
                , psBindingRepairDirty = Nothing
                }
    put stFinal
    pure $
        if changed
            then BindingRepairFull
            else BindingRepairSkipped

quotientChildrenByParent :: BindParents -> IntMap [(Int, (NodeRef, BindFlag))]
quotientChildrenByParent =
    IntMap.map reverse
        . IntMap.foldlWithKey'
            ( \m childKey info@(parentRoot, _flag) ->
                IntMap.insertWith (++) (nodeRefKey parentRoot) [(childKey, info)] m
            )
            IntMap.empty

edgeInteriorExact :: NodeId -> PresolutionM p IntSet.IntSet
edgeInteriorExact root0 = do
    (c0, canonical, qbp) <- cachedBindingModelM
    let interiorRootRef = traceInteriorRootRef canonical c0 root0
    interiorOfUnderCachedM canonical interiorRootRef

-- | Compute interior I(r) using the cached quotient binding model.
interiorOfUnderCachedM :: (NodeId -> NodeId) -> NodeRef -> PresolutionM p IntSet.IntSet
interiorOfUnderCachedM canonical rootRef = do
    (_c0, _canonical, qbp) <- cachedBindingModelM
    let rootC = case rootRef of
            TypeRef nid -> TypeRef (canonical nid)
            r -> r
        rootKey = nodeRefKey rootC
        childrenByParent = Binding.qbpChildrenByParent qbp
        -- BFS from root through children
        go visited [] = visited
        go visited (nid : rest) =
            let kids = IntSet.fromList
                    [ ck | (ck, _) <- IntMap.findWithDefault [] nid childrenByParent ]
                newKids = IntSet.difference kids visited
            in go (IntSet.union visited newKids) (IntSet.toList newKids ++ rest)
    pure (go (IntSet.singleton rootKey) [rootKey])

-- | Choose the binding-tree root reference used for exact I(r) computation in
-- edge traces and post-rewrite trace refresh.
traceInteriorRootRef :: (NodeId -> NodeId) -> Constraint p -> NodeId -> NodeRef
traceInteriorRootRef canonical c0 root0 =
    let rootC = canonical root0
        schemeOwner =
            listToMaybe
                [ gnId gen
                | gen <- NodeAccess.allGenNodes c0
                , any (\r -> canonical r == rootC) (gnSchemes gen)
                ]
        schemeOwnerByBody =
            listToMaybe
                [ gnId gen
                | gen <- NodeAccess.allGenNodes c0
                , any
                    ( \r ->
                        case VarStore.lookupVarBound c0 (canonical r) of
                            Just bnd -> canonical bnd == rootC
                            Nothing -> False
                    )
                    (gnSchemes gen)
                ]
    in case schemeOwner <|> schemeOwnerByBody of
        Just gid -> genRef gid
        Nothing -> typeRef rootC

orderedBindersRawM :: NodeId -> PresolutionM p [NodeId]
orderedBindersRawM binder0 = do
    c0 <- gets psConstraint
    case Binding.orderedBinders id c0 (typeRef binder0) of
        Left err -> throwError (BindingTreeError err)
        Right binders -> pure binders

-- | Resolve the instantiation binders for a node, skipping vacuous ∀ nodes.
-- Returns the body root to instantiate and the ordered binder list.
instantiationBindersM :: GenNodeId -> NodeId -> PresolutionM p (NodeId, [NodeId])
instantiationBindersM gid nid0 = do
    st <- get
    let c0 = psConstraint st
        uf0 = psUnionFind st
        canonical = UnionFind.frWith uf0
        nid = canonical nid0
        cache0 = psBinderCache st
        nodes = cNodes c0
        nodeAtNid = Types.lookupNode nid nodes
    case IntMap.lookup (getNodeId nid) cache0 of
        Just binders ->
            if null binders
                then pure (nid, binders)
                else do
                    let root =
                            case nodeAtNid of
                                Just TyForall{ tnBody = inner } -> canonical inner
                                _ -> nid
                    let debugMsg =
                            "instantiationBindersM: nid="
                                ++ show nid
                                ++ " root="
                                ++ show root
                    debugBinders debugMsg
                    pure (root, binders)
        Nothing -> case nodeAtNid of
            Nothing -> throwError (NodeLookupFailed nid)
            Just node -> case node of
                TyForall { tnId = forallId, tnBody = inner } -> do
                    binders <- orderedBindersRawM forallId
                    if null binders
                        then instantiationBindersM gid inner
                        else do
                            when (not (null binders)) $
                                modify' $ \st1 ->
                                    let cache1 = psBinderCache st1
                                        cache2 = IntMap.insert (getNodeId nid) binders cache1
                                        cache3 = IntMap.insert (getNodeId inner) binders cache2
                                    in st1 { psBinderCache = cache3 }
                            pure (inner, binders)
                TyExp { tnBody = inner } ->
                    -- Follow through TyExp wrappers to the inner scheme body.
                    -- Nested TyExp arises from intermediate let bindings
                    -- (e.g. `let g = f in g g` where g's scheme wraps f's).
                    instantiationBindersM gid inner
                _ -> do
                    -- Explicit provenance: use gen node scope instead of heuristic
                    (bodyRoot, binders) <- instantiationBindersFromGenM gid nid
                    when (not (null binders)) $
                        modify' $ \st1 ->
                            let cache1 = psBinderCache st1
                                cache2 = IntMap.insert (getNodeId nid) binders cache1
                                cache3 = IntMap.insert (getNodeId bodyRoot) binders cache2
                            in st1 { psBinderCache = cache3 }
                    pure (bodyRoot, binders)

-- | Compute instantiation binders from explicit scheme provenance.
--
-- Given the owning gen node and the scheme body root, enumerate binders
-- using the binding tree scope of the gen node — matching the thesis
-- definition where binders come from the gen node's scope (s = hg·i).
--
-- This replaces the heuristic @implicitBindersM@ with explicit provenance.
instantiationBindersFromGenM :: GenNodeId -> NodeId -> PresolutionM p (NodeId, [NodeId])
instantiationBindersFromGenM gid bodyRoot0 = do
    st <- get
    let c0 = psConstraint st
        uf0 = psUnionFind st
        canonical = UnionFind.frWith uf0
        bodyC = canonical bodyRoot0
        nodes = cNodes c0

    -- 1. Get flex children under the gen node's scope
    bindersUnderGen <- case Binding.boundFlexChildrenUnder canonical c0 (genRef gid) of
        Left err -> throwError (BindingTreeError err)
        Right bs -> pure bs

    -- 2. Compute reachability from the body root
    let reachable =
            Traversal.reachableFromWithBounds
                canonical
                (lookupNodeIn nodes)
                bodyC

    -- 3. Filter to live TyVar nodes reachable from body
    let isLiveVar nid =
            case lookupNodeIn nodes nid of
                Just TyVar{} -> not (VarStore.isEliminatedVar c0 nid)
                _ -> False

        bindersReachable =
            [ canonical b
            | b <- bindersUnderGen
            , IntSet.member (getNodeId (canonical b)) reachable
            , isLiveVar (canonical b)
            ]

    -- 4. Deduplicate by canonical ID
    let bindersCanon =
            IntMap.elems $
                IntMap.fromList
                    [ (getNodeId b, b)
                    | b <- bindersReachable
                    ]

    -- 5. Sort by order keys (leftmost-lowermost, paper <P)
    let orderKeys = Order.orderKeysFromConstraintWith canonical c0 bodyC Nothing

    sorted <- case Order.sortByOrderKey orderKeys bindersCanon of
        Left err -> throwError $ InternalError ("instantiationBindersFromGenM: order key error: " ++ show err)
        Right s -> pure s

    -- 6. Exclude wrapper body if it's a bound variable
    let bodyIsWrapper =
            case lookupNodeIn nodes bodyC of
                Just TyVar{} ->
                    case VarStore.lookupVarBound c0 bodyC of
                        Just _ -> True
                        Nothing -> False
                _ -> False
        candidates =
            if bodyIsWrapper
                then filter (/= bodyC) sorted
                else sorted

    pure (bodyC, candidates)

forallSpecM :: NodeId -> PresolutionM p ForallSpec
forallSpecM binder0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
    case Binding.forallSpecFromForall canonical c0 binder0 of
        Left err -> throwError (BindingTreeError err)
        Right fs -> pure fs

-- | Debug binders using explicit trace config.
debugBinders :: String -> PresolutionM p ()
debugBinders msg = do
    cfg <- ask
    traceBindingM cfg msg

-- | Drop trivial scheme edges (let edges) from the result maps.
--
-- See Note [Constraint simplification: Var-Let (Ch 12.4.1)]
dropTrivialSchemeEdges
    :: Constraint p
    -> EdgeArtifacts
    -> EdgeArtifacts
dropTrivialSchemeEdges constraint edgeArtifacts =
    let dropEdgeIds = cLetEdges constraint
        keepEdge eid = not (IntSet.member eid dropEdgeIds)
        witnesses' = IntMap.filterWithKey (\eid _ -> keepEdge eid) (eaEdgeWitnesses edgeArtifacts)
        traces' = IntMap.filterWithKey (\eid _ -> keepEdge eid) (eaEdgeTraces edgeArtifacts)
        expansions' = IntMap.filterWithKey (\eid _ -> keepEdge eid) (eaEdgeExpansions edgeArtifacts)
    in EdgeArtifacts expansions' witnesses' traces'

{- Note [Constraint simplification: Var-Let (Ch 12.4.1)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis (§12.4.1, Lemma 12.4.2) defines the Var-Let simplification rule:
gen nodes introduced for let-bound variable occurrences are superfluous because
they only create indirections — instantiating a scheme and immediately using it.
Var-Let removes these intermediate gen nodes by connecting the instantiation
edge directly to the scheme root.

We apply Var-Let on-the-fly (§12.4.3): during constraint generation, let-bound
variable occurrences get expansion nodes (via `allocExpNode` in the `EVar` case
of `buildExprRaw` in Translate.hs) rather than full gen nodes. After presolution resolves expansions, this function
drops the trivial let-edge witnesses/traces/expansions from the result maps,
since those edges served only as indirections and carry no elaboration content.

See also Note [Lambda Translation] in ConstraintGen/Translate.hs for the
companion Var-Abs rule.
-}

{- Note [ML-Extrude omitted (Ch 12.4.2)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis (§12.4.2, Lemma 12.4.3) defines the ML-Extrude rule: in ML
constraints, binding edges can be raised from inside a gen node to an ancestor
without loss of generality. This is intentionally not implemented because it
only preserves solutions for ML constraints. In MLF, raising a node outside a
gen interior prevents it from being reset during expansion, resulting in either
untypable constraints or weaker principal solutions (thesis line ~13400:
"the equivalence only holds in ML").
-}

-- | MonadPresolution instance for ReaderT, allowing presolution operations
-- to be used within ReaderT transformers without explicit lift.
instance {-# OVERLAPPABLE #-} MonadPresolution m => MonadPresolution (ReaderT r m) where
    type PresolutionPhaseOf (ReaderT r m) = PresolutionPhaseOf m
    getConstraint = lift getConstraint
    modifyConstraint f = lift (modifyConstraint f)
    getPresolutionState = lift getPresolutionState
    putPresolutionState st = lift (putPresolutionState st)
    throwPresolutionError err = lift (throwPresolutionError err)
    modifyPresolution f = lift (modifyPresolution f)
    bindExpansionArgs root pairs = lift (bindExpansionArgs root pairs)

instance {-# OVERLAPPABLE #-} MonadPresolution m => MonadPresolution (StateT s m) where
    type PresolutionPhaseOf (StateT s m) = PresolutionPhaseOf m
    getConstraint = lift getConstraint
    modifyConstraint f = lift (modifyConstraint f)
    getPresolutionState = lift getPresolutionState
    putPresolutionState st = lift (putPresolutionState st)
    throwPresolutionError err = lift (throwPresolutionError err)
    modifyPresolution f = lift (modifyPresolution f)
    bindExpansionArgs root pairs = lift (bindExpansionArgs root pairs)
