{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module MLF.Constraint.Presolution.Base (
    PresolutionUf(..),
    ExpansionResultMap(..),
    emptyExpansionResultMap,
    lookupExpansionResult,
    lookupExpansionResultUnder,
    canonicalizeExpansionResultMap,
    EdgeArtifact,
    edgeArtifactExpansion,
    edgeArtifactWitness,
    edgeArtifactTrace,
    edgeArtifactExpansionConstruction,
    EdgeArtifacts,
    EdgeArtifactsError(..),
    edgeArtifactsFromExecutionArtifacts,
    lookupEdgeArtifact,
    edgeArtifactKeys,
    mapEdgeArtifacts,
    eaEdgeExpansions,
    eaEdgeWitnesses,
    eaEdgeTraces,
    eaEdgeExpansionConstructions,
    eaIdentityEdges,
    RawExpansionConstruction,
    emptyRawExpansionConstruction,
    mkRawExpansionConstruction,
    combineRawExpansionConstructions,
    rawExpansionConstructionParents,
    rawExpansionConstructionArgumentKeys,
    rawExpansionConstructionSemanticMetaKeys,
    PresolutionResult(..),
    prEdgeExpansions,
    prEdgeWitnesses,
    prEdgeTraces,
    prEdgeExpansionConstructions,
    prIdentityEdges,
    PresolutionPlanBuilder(..),
    PresolutionError(..),
    TranslatabilityIssue(..),
    EdgeWitnessNonSourceOrigin(..),
    EdgeExecutionArtifacts(..),
    PresolutionState
        ( psConstraint
        , psPresolution
        , psUnionFind
        , psNextNodeId
        , psPendingWeakens
        , psPendingWeakenOwners
        , psWeakenReplayCertificates
        , psBinderCache
        , psGraphVersion
        , psUnionFindVersion
        , psBindParentsVersion
        , psBindingModelCache
        , psEdgeLocalSnapshot
        , psBindingRepairCache
        , psBindingRepairDirty
        , psCachedRootGen
        , psExpansionResults
        , psEdgeExecutionArtifacts
        ),
    mkPresolutionState,
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
    setEdgeLocalSnapshot,
    clearEdgeLocalSnapshot,
    PendingWeakenOwner(..),
    pendingWeakenOwnerFromMaybe,
    pendingWeakenOwnerToMaybe,
    WeakenReplayCertificate,
    certifyAppliedNonRootWeakenReplay,
    certifyEliminatedNonRootWeakenReplay,
    weakenReplayCertificateSource,
    weakenReplayCertificateTarget,
    weakenReplayCertificateReplayBinder,
    weakenReplayCertificateRoot,
    weakenReplayCertificateFlexiblePath,
    weakenReplayCertificateDescendants,
    weakenReplayCertificateMatches,
    EdgeTrace(..),
    rootRaiseMergeTraceAuthority,
    rootRigidRaiseMergeTraceAuthority,
    rootWeakenRaiseMergeTraceAuthority,
    CopyMapping(..),
    CopyMap,
    lookupCopy,
    insertCopy,
    copiedNodes,
    originalNodes,
    InteriorNodes(..),
    EdgeSourceInterior(..),
    EdgeDestinationInterior(..),
    sourceInteriorFromList,
    sourceInteriorFromSet,
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
    recordExpansionResult,
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
import Control.Monad (void, when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import qualified MLF.Binding.Path as BindingPath
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Presolution.BoundScope as BoundScope
import MLF.Constraint.Presolution.Construction
    ( RawExpansionConstruction
    , combineRawExpansionConstructions
    , emptyRawExpansionConstruction
    , mkRawExpansionConstruction
    , rawExpansionConstructionArgumentKeys
    , rawExpansionConstructionParents
    , rawExpansionConstructionSemanticMetaKeys
    )
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Phase (Phase(Presolved))
import qualified MLF.Constraint.Types.Graph as Types
import MLF.Constraint.Types.Witness
    ( EdgeWitness
    , Expansion
    , ForallSpec
    , InstanceOp
    , ReplayContract(..)
    , ewEdgeId
    )
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
    , bindingRepairDirtyIsEmpty
    , brdDirtyAll
    , buildBindingRepairModel
    , dirtyBindingRepairBindRefs
    , dirtyBindingRepairTypes
    , dirtyAllBindingRepair
    , emptyBindingRepairDirty
    , mergeBindingRepairDirty
    , repairBindingParentsDirtyWithModel
    , repairBindingParentsWithModel
    )
import MLF.Constraint.Presolution.Plan (GeneralizePlan, ReifyPlan)
import MLF.Constraint.Presolution.Plan.Context (GaBindParents, GeneralizationRequirements)
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Presolution.WitnessValidation (OmegaNormalizeError)
import MLF.Util.ElabError (ElabError)

newtype PresolutionUf = PresolutionUf { getPresolutionUf :: IntMap NodeId }
    deriving (Eq, Show)

-- | Administrative replacement of an occurrence-site 'TyExp' wrapper by the
-- already-constructed root of its χe expansion.  This map is deliberately
-- separate from semantic union-find: wrappers do not occur in the paper graph
-- and must not induce Raise/Merge operations or quotient term-DAG edges.
newtype ExpansionResultMap = ExpansionResultMap
    { getExpansionResultMap :: IntMap NodeId
    }
    deriving (Eq, Show)

emptyExpansionResultMap :: ExpansionResultMap
emptyExpansionResultMap = ExpansionResultMap IntMap.empty

lookupExpansionResult :: NodeId -> ExpansionResultMap -> Maybe NodeId
lookupExpansionResult wrapper (ExpansionResultMap results) =
    IntMap.lookup (getNodeId wrapper) results

-- | Canonicalize administrative replacements by semantic union-find class.
-- A wrapper class has exactly one destination-scoped expansion result; unlike
-- ordinary unification, resolving a collision here must not emit witness work.
canonicalizeExpansionResultMap
    :: (NodeId -> NodeId)
    -> ExpansionResultMap
    -> Either PresolutionError ExpansionResultMap
canonicalizeExpansionResultMap canonical (ExpansionResultMap results) =
    ExpansionResultMap
        <$> IntMap.foldlWithKey' insertCanonical (Right IntMap.empty) results
  where
    insertCanonical accE wrapperKey result0 = do
        acc <- accE
        let wrapper = canonical (NodeId wrapperKey)
            result = canonical result0
            key = getNodeId wrapper
        case IntMap.lookup key acc of
            Nothing -> pure (IntMap.insert key result acc)
            Just existing
                | existing == result -> pure acc
                | otherwise ->
                    Left (ExpansionResultConflict wrapper existing result)

lookupExpansionResultUnder
    :: (NodeId -> NodeId)
    -> NodeId
    -> ExpansionResultMap
    -> Either PresolutionError (Maybe NodeId)
lookupExpansionResultUnder canonical wrapper results = do
    normalized <- canonicalizeExpansionResultMap canonical results
    pure (lookupExpansionResult (canonical wrapper) normalized)

-- | One construction-closed proof packet for an instantiation edge.
--
-- The constructor is private: an expansion choice, its normalized derivation
-- witness, frozen replay trace, and exact construction certificate are
-- committed together and can only be selected by the same semantic edge
-- identity.
data EdgeArtifact = EdgeArtifact
    { edgeArtifactExpansion :: !Expansion
    , edgeArtifactWitness :: !EdgeWitness
    , edgeArtifactTrace :: !EdgeTrace
    , edgeArtifactExpansionConstruction :: !RawExpansionConstruction
    }
    deriving (Eq, Show)

-- | Phase 4 artifacts consumed by Phase 6.  Keeping one map of complete
-- packets makes independently missing witness/trace/expansion/construction
-- state unrepresentable after construction.
data EdgeArtifacts = EdgeArtifactsInternal
    { edgeArtifactsByEdge :: !(IntMap EdgeArtifact)
    , eaIdentityEdges :: !IntSet.IntSet
    }
    deriving (Eq, Show)

data EdgeArtifactsError
    = EdgeArtifactKeyMismatch
        { edgeArtifactExpansionKeys :: !IntSet.IntSet
        , edgeArtifactWitnessKeys :: !IntSet.IntSet
        , edgeArtifactTraceKeys :: !IntSet.IntSet
        , edgeArtifactExpansionConstructionKeys :: !IntSet.IntSet
        }
    | EdgeArtifactWitnessIdMismatch
        { edgeArtifactMapKey :: !Int
        , edgeArtifactEmbeddedEdgeId :: !EdgeId
        }
    deriving (Eq, Show)

lookupEdgeArtifact :: EdgeId -> EdgeArtifacts -> Maybe EdgeArtifact
lookupEdgeArtifact edgeId =
    IntMap.lookup (getEdgeId edgeId) . edgeArtifactsByEdge

edgeArtifactKeys :: EdgeArtifacts -> IntSet.IntSet
edgeArtifactKeys =
    IntMap.keysSet . edgeArtifactsByEdge

filterEdgeArtifacts
    :: (EdgeId -> Bool)
    -> EdgeArtifacts
    -> EdgeArtifacts
filterEdgeArtifacts keep edgeArtifacts =
    edgeArtifacts
        { edgeArtifactsByEdge =
            IntMap.filterWithKey
                (\edgeKey _ -> keep (EdgeId edgeKey))
                (edgeArtifactsByEdge edgeArtifacts)
        }

mapEdgeArtifacts
    :: (Expansion -> Expansion)
    -> (EdgeWitness -> EdgeWitness)
    -> (EdgeTrace -> EdgeTrace)
    -> EdgeArtifacts
    -> Either EdgeArtifactsError EdgeArtifacts
mapEdgeArtifacts mapExpansion mapWitness mapTrace edgeArtifacts = do
    artifacts <-
        IntMap.traverseWithKey
            (\edgeKey artifact ->
                mkEdgeArtifact
                    edgeKey
                    (mapExpansion (edgeArtifactExpansion artifact))
                    (mapWitness (edgeArtifactWitness artifact))
                    (mapTrace (edgeArtifactTrace artifact))
                    (edgeArtifactExpansionConstruction artifact)
            )
            (edgeArtifactsByEdge edgeArtifacts)
    pure edgeArtifacts {edgeArtifactsByEdge = artifacts}

setEdgeArtifactsIdentityEdges
    :: IntSet.IntSet
    -> EdgeArtifacts
    -> EdgeArtifacts
setEdgeArtifactsIdentityEdges identityEdges edgeArtifacts =
    edgeArtifacts {eaIdentityEdges = identityEdges}

eaEdgeExpansions :: EdgeArtifacts -> IntMap Expansion
eaEdgeExpansions =
    IntMap.map edgeArtifactExpansion . edgeArtifactsByEdge

eaEdgeWitnesses :: EdgeArtifacts -> IntMap EdgeWitness
eaEdgeWitnesses =
    IntMap.map edgeArtifactWitness . edgeArtifactsByEdge

eaEdgeTraces :: EdgeArtifacts -> IntMap EdgeTrace
eaEdgeTraces =
    IntMap.map edgeArtifactTrace . edgeArtifactsByEdge

eaEdgeExpansionConstructions
    :: EdgeArtifacts
    -> IntMap RawExpansionConstruction
eaEdgeExpansionConstructions =
    IntMap.map edgeArtifactExpansionConstruction . edgeArtifactsByEdge

mkEdgeArtifact
    :: Int
    -> Expansion
    -> EdgeWitness
    -> EdgeTrace
    -> RawExpansionConstruction
    -> Either EdgeArtifactsError EdgeArtifact
mkEdgeArtifact edgeKey expansion witness traceInfo construction
    | ewEdgeId witness /= EdgeId edgeKey =
        Left
            EdgeArtifactWitnessIdMismatch
                { edgeArtifactMapKey = edgeKey
                , edgeArtifactEmbeddedEdgeId = ewEdgeId witness
                }
    | otherwise =
        Right
            EdgeArtifact
                { edgeArtifactExpansion = expansion
                , edgeArtifactWitness = witness
                , edgeArtifactTrace = traceInfo
                , edgeArtifactExpansionConstruction = construction
                }

-- | Exceptional operand origins for an edge-witness operation.  Source-only
-- operations need no entry.  A mixed Merge names its operated node in the
-- frozen source graph and its other node in the constructed destination graph.
-- An expansion Graft instead names its argument in the destination graph and
-- its quantified binder in the frozen source graph.  Keeping these cases
-- distinct prevents witness finalization from restoring both operands through
-- the same source-domain map.
data EdgeWitnessNonSourceOrigin
    = DestinationEdgeOperation
    | SourceDestinationMergeOperation
    | DestinationSourceGraftOperation
    | FlexibleTerminalSourceOperation
    deriving (Eq, Show)

-- | Result of the presolution phase.
data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint 'Presolved
    , prEdgeArtifacts :: EdgeArtifacts
    , prRedirects :: IntMap NodeId -- ^ Map from old TyExp IDs to their replacement IDs
    , prUnionFind :: PresolutionUf
    , prPlanBuilder :: PresolutionPlanBuilder
    } deriving (Eq, Show)

prEdgeExpansions :: PresolutionResult -> IntMap Expansion
prEdgeExpansions =
    eaEdgeExpansions . prEdgeArtifacts

prEdgeWitnesses :: PresolutionResult -> IntMap EdgeWitness
prEdgeWitnesses =
    eaEdgeWitnesses . prEdgeArtifacts

prEdgeTraces :: PresolutionResult -> IntMap EdgeTrace
prEdgeTraces =
    eaEdgeTraces . prEdgeArtifacts

prEdgeExpansionConstructions
    :: PresolutionResult
    -> IntMap RawExpansionConstruction
prEdgeExpansionConstructions =
    eaEdgeExpansionConstructions . prEdgeArtifacts

prIdentityEdges :: PresolutionResult -> IntSet.IntSet
prIdentityEdges =
    eaIdentityEdges . prEdgeArtifacts

instance PresolutionSnapshot PresolutionResult where
    snapshotConstraint = prConstraint
    snapshotUnionFind = getPresolutionUf . prUnionFind

newtype PresolutionPlanBuilder = PresolutionPlanBuilder
    { ppbBuildGeneralizePlans
        :: forall p.
           PresolutionView p
        -> Maybe (GaBindParents p)
        -> GeneralizationRequirements
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
    | InvalidEdgeArtifacts EdgeArtifactsError
      -- ^ Phase 4 attempted to publish a partial or mis-keyed edge packet.
    | UnresolvedExpVar ExpVarId              -- ^ ExpVar couldn't be resolved
    | ArityMismatch String Int Int           -- ^ (context, expected, actual)
    | InstantiateOnNonForall NodeId          -- ^ Tried to instantiate a non-forall node
    | NodeLookupFailed NodeId                -- ^ Missing node in constraint
    | BoundTargetNotTyVar NodeId TyNode      -- ^ Tried to install a lower bound on a non-variable node
    | OccursCheckPresolution NodeId NodeId   -- ^ Unification would make node reachable from itself
    | CopyBoundScopeRepairRequired NodeId [NodeId]
      -- ^ A copied bound needed Raise after binding reset; copying must
      -- construct the copied scope directly rather than hide witness work.
    | CopyBindingScopeRepairRequired NodeId [NodeId]
      -- ^ The complete copied binding projection still needed Raise.  A χe
      -- copy must publish its binding tree and copied bounds in one already
      -- well-scoped transaction.
    | CopySubstitutionConflict NodeId NodeId NodeId
      -- ^ Canonical aliases of one source binder selected distinct images.
    | CopyPendingBoundConflict NodeId NodeId NodeId
      -- ^ One copied variable was assigned two distinct lower-bound roots
      -- while constructing an atomic χe projection.
    | CopyBindingParentConflict NodeRef NodeRef NodeRef
      -- ^ One canonical copied child was assigned two distinct parents while
      -- constructing the atomic copied binding projection.
    | ExpansionArgumentScopeRepairRequired NodeId [NodeId]
      -- ^ Destination rebinding of expansion arguments would require hidden
      -- Raise work; edge-local execution must record it explicitly.
    | EdgeBoundRaiseOutsideInterior NodeId [NodeId]
      -- ^ Installing a bound during chi_e attempted to Raise nodes outside
      -- the edge's source interior.  Such a mutation cannot be represented by
      -- that edge's source-domain witness and must fail transactionally.
    | IdentityExpansionHasBaseOps EdgeId [InstanceOp]
      -- ^ ExpIdentity has no Omega construction steps.  Reaching execution
      -- with base operations means the witness plan and expansion disagree.
    | ExpansionResultConflict NodeId NodeId NodeId
      -- ^ One administrative TyExp wrapper was associated with two distinct
      -- expansion-result classes.
    | MissingExpansionResult NodeId ExpVarId
      -- ^ A non-identity TyExp reached finalization without χe having
      -- constructed its destination-scoped result.
    | BindingTreeError BindingError          -- ^ Invalid binding tree when binding edges are in use
    | NonTranslatablePresolution [TranslatabilityIssue]
    | WitnessNormalizationError EdgeId OmegaNormalizeError  -- ^ Normalized witness violates Fig. 15.3.4 invariants
    | ResidualUnifyEdges [UnifyEdge]         -- ^ Presolution artifact must not retain unification work.
    | ResidualInstEdges [InstEdge]           -- ^ Presolution artifact must not retain instantiation edges.
    | ResidualTyExpNodes [NodeId]            -- ^ Presolution artifact must be TyExp-free.
    | MissingEdgeArtifacts [EdgeId]
      -- ^ Non-trivial instantiation edges missing complete proof packets.
    | ExpansionDestinationConflict ExpVarId [GenNodeId]
      -- ^ One expansion variable is demanded at more than one propagation
      -- destination.  Its assignment may later acquire argument payloads, and
      -- one payload cannot be owned by two distinct n-hat gen nodes
      -- (Definition 10.3.2).
    | ExpansionArgumentDestinationConflict NodeId [GenNodeId]
      -- ^ Distinct expansion arguments collapsed to one canonical node while
      -- retaining incompatible destination-gen provenance.
    | NestedTyExpAuthorityMismatch NodeId GenNodeId ExpVarId GenNodeId ExpVarId (Maybe (NodeId, GenNodeId))
      -- ^ A leading source spine reached a nested expansion wrapper owned by
      -- another @(gen, expansion-variable)@ authority.  The optional pair is
      -- the already-materialized result and its owner when one existed.
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

-- | The complete immutable proof packet emitted by one edge execution.
-- Keeping expansion choice, witness authority, construction provenance, and
-- replay trace in one value makes partial execution evidence unrepresentable.
data EdgeExecutionArtifacts = EdgeExecutionArtifacts
    { eeaExpansion :: !Expansion
    , eeaWitness :: !EdgeWitness
    , eeaRaiseAuthorityNodes :: !IntSet.IntSet
    , eeaNonSourceOpOrigins :: !(IntMap EdgeWitnessNonSourceOrigin)
    , eeaExpansionConstruction :: !RawExpansionConstruction
    , eeaTrace :: !EdgeTrace
    }
    deriving (Eq, Show)

-- | Publish the construction-closed execution packets without first splitting
-- them into independently keyed component maps.
edgeArtifactsFromExecutionArtifacts
    :: IntMap EdgeExecutionArtifacts
    -> IntSet.IntSet
    -> Either EdgeArtifactsError EdgeArtifacts
edgeArtifactsFromExecutionArtifacts executionArtifacts identityEdges = do
    artifacts <-
        IntMap.traverseWithKey
            (\edgeKey executionArtifact ->
                mkEdgeArtifact
                    edgeKey
                    (eeaExpansion executionArtifact)
                    (eeaWitness executionArtifact)
                    (eeaTrace executionArtifact)
                    (eeaExpansionConstruction executionArtifact)
            )
            executionArtifacts
    pure
        EdgeArtifactsInternal
            { edgeArtifactsByEdge = artifacts
            , eaIdentityEdges = identityEdges
            }

data PresolutionState p = PresolutionStateInternal
    { psConstraint :: Constraint p
    , psPresolution :: Presolution
    , psUnionFind :: IntMap NodeId
    , psNextNodeId :: Int
    , psPendingWeakens :: IntSet.IntSet
    , psPendingWeakenOwners :: IntMap PendingWeakenOwner
    , psWeakenReplayCertificates :: IntMap (IntMap WeakenReplayCertificate)
    , psBinderCache :: IntMap [NodeId]
    , psGraphVersion :: !Int
    , psUnionFindVersion :: !Int
    , psBindParentsVersion :: !Int
    , psBindingModelCache :: Maybe (CachedBindingModel p)
    , psEdgeLocalSnapshot :: Maybe (CachedBindingModel p)
    , psBindingRepairCache :: Maybe (CachedBindingRepairModel p)
    , psBindingRepairDirty :: Maybe BindingRepairDirty
    , psCachedRootGen :: !(Maybe (Maybe NodeRef))
    , psExpansionResults :: ExpansionResultMap
    , psEdgeExecutionArtifacts :: IntMap EdgeExecutionArtifacts
    }
    deriving (Eq, Show)

-- | Construct mutable presolution state from complete edge-execution packets.
--
-- The packet map is the only edge-artifact input.  In particular, callers
-- cannot provide expansion, witness, trace, or construction maps separately
-- and rely on this boundary to reconnect them or manufacture missing
-- authority.  Cache/version fields are initialized owner-locally.
mkPresolutionState
    :: Constraint p
    -> Presolution
    -> IntMap NodeId
    -> Int
    -> IntSet.IntSet
    -> IntMap PendingWeakenOwner
    -> IntMap [NodeId]
    -> IntMap EdgeExecutionArtifacts
    -> PresolutionState p
mkPresolutionState
    constraint
    presolution
    unionFind
    nextNodeId
    pendingWeakens
    pendingWeakenOwners
    binderCache
    edgeExecutionArtifacts =
    PresolutionStateInternal
        { psConstraint = constraint
        , psPresolution = presolution
        , psUnionFind = unionFind
        , psNextNodeId = nextNodeId
        , psPendingWeakens = pendingWeakens
        , psPendingWeakenOwners = pendingWeakenOwners
        , psWeakenReplayCertificates = IntMap.empty
        , psBinderCache = binderCache
        , psGraphVersion = 0
        , psUnionFindVersion = 0
        , psBindParentsVersion = 0
        , psBindingModelCache = Nothing
        , psEdgeLocalSnapshot = Nothing
        , psBindingRepairCache = Nothing
        , psBindingRepairDirty = Just dirtyAllBindingRepair
        , psCachedRootGen = Nothing
        , psExpansionResults = emptyExpansionResultMap
        , psEdgeExecutionArtifacts = edgeExecutionArtifacts
        }

{-# INLINE invalidateBindingModelState #-}
invalidateBindingModelState :: PresolutionState p -> PresolutionState p
invalidateBindingModelState st = st { psBindingModelCache = Nothing, psEdgeLocalSnapshot = Nothing }

{-# INLINE invalidateBindingRepairModelState #-}
invalidateBindingRepairModelState :: PresolutionState p -> PresolutionState p
invalidateBindingRepairModelState st =
    st
        { psBindingRepairCache = Nothing
        , psBindingRepairDirty = Just dirtyAllBindingRepair
        }

-- | Binder discovery depends on the complete canonical type graph and binding
-- tree: reachability, eliminated-variable state, ownership, and semantic UF
-- representatives all participate in 'instantiationBindersFromGenM'.  Keep
-- entries only while those inputs are stable.
{-# INLINE invalidateBinderCacheState #-}
invalidateBinderCacheState :: PresolutionState p -> PresolutionState p
invalidateBinderCacheState st = st { psBinderCache = IntMap.empty }

{-# INLINE markBindingRepairDirtyState #-}
markBindingRepairDirtyState :: BindingRepairDirty -> PresolutionState p -> PresolutionState p
markBindingRepairDirtyState dirty st =
    st
        { psBindingRepairDirty =
            Just $
                maybe dirty (mergeBindingRepairDirty dirty) (psBindingRepairDirty st)
        }

{-# INLINE markBindingRepairDirtyTypesState #-}
markBindingRepairDirtyTypesState :: IntSet.IntSet -> PresolutionState p -> PresolutionState p
markBindingRepairDirtyTypesState dirtyTypes st =
    markBindingRepairDirtyState (dirtyBindingRepairTypes dirtyTypes) st

{-# INLINE markBindingRepairDirtyBindRefsState #-}
markBindingRepairDirtyBindRefsState :: IntSet.IntSet -> PresolutionState p -> PresolutionState p
markBindingRepairDirtyBindRefsState dirtyBindRefs st =
    markBindingRepairDirtyState (dirtyBindingRepairBindRefs dirtyBindRefs) st

{-# INLINE modifyConstraintState #-}
modifyConstraintState :: (Constraint p -> Constraint p) -> PresolutionState p -> PresolutionState p
modifyConstraintState f st =
    invalidateBinderCacheState $
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psConstraint = f (psConstraint st)
            , psGraphVersion = psGraphVersion st + 1
            , psBindParentsVersion = psBindParentsVersion st + 1
            , psCachedRootGen = Nothing
            }

-- | Modify constraint types/nodes without touching bind parents.
--
-- Only bumps 'psGraphVersion'.  The quotient binding-model cache depends on
-- 'psUnionFindVersion' and 'psBindParentsVersion' so it remains valid across
-- type-only mutations.  The repair model is also unaffected.
{-# INLINE modifyConstraintDirtyTypesState #-}
modifyConstraintDirtyTypesState
    :: IntSet.IntSet
    -> (Constraint p -> Constraint p)
    -> PresolutionState p
    -> PresolutionState p
modifyConstraintDirtyTypesState dirtyTypes f st =
    invalidateBinderCacheState $
    markBindingRepairDirtyTypesState dirtyTypes $
        st
            { psConstraint = f (psConstraint st)
            , psGraphVersion = psGraphVersion st + 1
            , psBindingRepairCache = Nothing
            }

{-# INLINE setConstraintDirtyBindRefsState #-}
setConstraintDirtyBindRefsState
    :: IntSet.IntSet
    -> Constraint p
    -> PresolutionState p
    -> PresolutionState p
setConstraintDirtyBindRefsState dirtyBindRefs constraint st =
    invalidateBinderCacheState $
    invalidateBindingModelState $
    markBindingRepairDirtyBindRefsState dirtyBindRefs $
        st
            { psConstraint = constraint
            , psBindParentsVersion = psBindParentsVersion st + 1
            , psCachedRootGen = Nothing
            }

{-# INLINE modifyBindParentsState #-}
modifyBindParentsState :: (BindParents -> BindParents) -> PresolutionState p -> PresolutionState p
modifyBindParentsState f st =
    invalidateBinderCacheState $
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psConstraint = (psConstraint st) { cBindParents = f (cBindParents (psConstraint st)) }
            , psBindParentsVersion = psBindParentsVersion st + 1
            , psCachedRootGen = Nothing
            }

{-# INLINE setBindParentState #-}
setBindParentState :: NodeRef -> (NodeRef, BindFlag) -> PresolutionState p -> PresolutionState p
setBindParentState child parentInfo st =
    invalidateBinderCacheState $
    invalidateBindingModelState $
    markBindingRepairDirtyBindRefsState (IntSet.singleton (nodeRefKey child)) $
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

{-# INLINE setConstraintState #-}
setConstraintState :: Constraint p -> PresolutionState p -> PresolutionState p
setConstraintState constraint =
    modifyConstraintState (const constraint)

{-# INLINE setUnionFindState #-}
setUnionFindState :: IntMap NodeId -> PresolutionState p -> PresolutionState p
setUnionFindState unionFind st =
    invalidateBinderCacheState $
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        st
            { psUnionFind = unionFind
            , psUnionFindVersion = psUnionFindVersion st + 1
            , psCachedRootGen = Nothing
            }

{-# INLINE modifyUnionFindState #-}
modifyUnionFindState :: (IntMap NodeId -> IntMap NodeId) -> PresolutionState p -> PresolutionState p
modifyUnionFindState f st =
    setUnionFindState (f (psUnionFind st)) st

{-# INLINE mergeUnionFindState #-}
mergeUnionFindState :: NodeId -> NodeId -> PresolutionState p -> PresolutionState p
mergeUnionFindState fromRoot toRoot st =
    invalidateBinderCacheState $
    invalidateBindingRepairModelState $
    invalidateBindingModelState $
        let unionFind =
                IntMap.insert
                    (getNodeId fromRoot)
                    toRoot
                    (psUnionFind st)
        in st
            { psUnionFind = unionFind
            , psUnionFindVersion = psUnionFindVersion st + 1
            , psCachedRootGen = Nothing
            }

-- | Path compression does not change canonical representatives, so cached
-- quotient binding models remain semantically valid.
{-# INLINE compressUnionFindState #-}
compressUnionFindState :: IntMap NodeId -> PresolutionState p -> PresolutionState p
compressUnionFindState unionFind st =
    st { psUnionFind = unionFind }

{-# INLINE setBindingModelCacheState #-}
setBindingModelCacheState :: CachedBindingModel p -> PresolutionState p -> PresolutionState p
setBindingModelCacheState cache st =
    st { psBindingModelCache = Just cache }

{-# INLINE setEdgeLocalSnapshot #-}
setEdgeLocalSnapshot :: CachedBindingModel p -> PresolutionState p -> PresolutionState p
setEdgeLocalSnapshot snap st = st { psEdgeLocalSnapshot = Just snap }

{-# INLINE clearEdgeLocalSnapshot #-}
clearEdgeLocalSnapshot :: PresolutionState p -> PresolutionState p
clearEdgeLocalSnapshot st = st { psEdgeLocalSnapshot = Nothing }

cachedBindingModelM :: PresolutionM p (Constraint p, NodeId -> NodeId, Binding.QuotientBindParents)
cachedBindingModelM = do
    st <- get
    case psEdgeLocalSnapshot st of
        Just frozen ->
            pure
                ( psConstraint st
                , UnionFind.frWith (cbmUnionFind frozen)
                , cbmQuotient frozen
                )
        Nothing ->
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

{-# INLINE pendingWeakenOwnerFromMaybe #-}
pendingWeakenOwnerFromMaybe :: Maybe GenNodeId -> PendingWeakenOwner
pendingWeakenOwnerFromMaybe = maybe PendingWeakenOwnerUnknown PendingWeakenOwnerGen

{-# INLINE pendingWeakenOwnerToMaybe #-}
pendingWeakenOwnerToMaybe :: PendingWeakenOwner -> Maybe GenNodeId
pendingWeakenOwnerToMaybe owner = case owner of
    PendingWeakenOwnerGen gid -> Just gid
    PendingWeakenOwnerUnknown -> Nothing

{- Note [Construction-time Weaken replay certificates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Figure 15.3.4 of the thesis translates a non-root @Weaken(n)@ as
@C(r -> n)(N)@.  The premise for constructing that computation context is
that @n@ is transitively flex-bound to the expansion root @r@ /before/ the
operation.  Applying the Weaken changes the first edge of that very path to
rigid, so the finalized binding tree cannot reconstruct the premise.

`applyPendingWeaken` is the one point that owns both states.  It creates this
opaque certificate only when the complete pre-state path is flexible and the
post-state has changed exactly the operated edge from flexible to rigid (or
when an earlier Merge in the same witness has already eliminated that copied
binder).  Besides the normalized operation target, the certificate retains
the exact operation-time copied binder needed by Φ; union-find may erase that
identity before witness normalization.  It also snapshots the strict
descendants needed by the delayed-Weaken ordering law.  Witness normalization
consumes these producer-owned fields; it must not infer replay from a final
type shape or treat an arbitrary invalid operation as exempt.

Root Weaken is the identity computation in Figure 15.3.4.  Likewise a
Graft/Weaken pair is one atomic instantiation step, so it does not publish a
second standalone-Weaken certificate.  Other operations without the checked
construction-time premise deliberately receive no certificate.
-}

-- | Evidence that one raw non-root Weaken had a valid computation context at
-- the instant its pending graph mutation was applied.  Keep the constructor
-- private: the pre/post checks below are the only production constructor.
data WeakenReplayCertificate = WeakenReplayCertificate
    { weakenReplayCertificateSource :: !NodeId
    , weakenReplayCertificateTarget :: !NodeId
    , weakenReplayCertificateReplayBinder :: !NodeId
    , weakenReplayCertificateRoot :: !NodeId
    , weakenReplayCertificateFlexiblePath :: ![NodeRef]
    , weakenReplayCertificateDescendants :: !IntSet.IntSet
    }
    deriving (Eq, Show)

-- | Certify the thesis premise for an applied non-root Weaken.
--
-- The path is recorded in target-to-root order.  Every edge on it is flexible
-- in the pre-state; in the post-state only the target's own edge is rigid.
certifyAppliedNonRootWeakenReplay
    :: Constraint p
    -> Constraint q
    -> (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> NodeId
    -> Maybe WeakenReplayCertificate
certifyAppliedNonRootWeakenReplay before after canonical source target root = do
    let targetC = canonical target
        rootC = canonical root
        interiorRoot = expansionInteriorRootRef before canonical rootC
    if targetC == rootC then Nothing else pure ()
    path <- flexibleRefPathToRoot before canonical (typeRef targetC) interiorRoot
    parent <- case path of
        _target : nextParent : _ -> Just nextParent
        _ -> Nothing
    case Binding.lookupBindParent after (typeRef targetC) of
        Just (parentAfter, BindRigid)
            | canonicalizeRef canonical parentAfter == parent -> pure ()
        _ -> Nothing
    if unchangedFlexibleSuffix before after canonical (drop 1 (dropLast path))
        then pure ()
        else Nothing
    descendants <- strictTypeDescendants before canonical targetC
    pure
        WeakenReplayCertificate
            { weakenReplayCertificateSource = source
            , weakenReplayCertificateTarget = targetC
            , weakenReplayCertificateReplayBinder = target
            , weakenReplayCertificateRoot = rootC
            , weakenReplayCertificateFlexiblePath = path
            , weakenReplayCertificateDescendants = descendants
            }

-- | Check the source/copy/root linkage owned by the edge artifact.  This does
-- not re-check the finalized binding shape; the opaque certificate already
-- records the operation-time proof from Note [Construction-time Weaken replay
-- certificates].
weakenReplayCertificateMatches
    :: (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> NodeId
    -> WeakenReplayCertificate
    -> Bool
weakenReplayCertificateMatches canonical source target root certificate =
    source == weakenReplayCertificateSource certificate
        && target == weakenReplayCertificateReplayBinder certificate
        && canonical target == canonical (weakenReplayCertificateTarget certificate)
        && canonical root == canonical (weakenReplayCertificateRoot certificate)
        && case weakenReplayCertificateFlexiblePath certificate of
            TypeRef pathTarget : _ -> canonical pathTarget == canonical target
            _ -> False

-- | Certify the no-mutation finalization of a Weaken whose copied binder was
-- already eliminated by an earlier Merge from the same raw witness.  UF
-- inequality is the construction-time evidence that changing the surviving
-- representative's binding edge would mutate the exterior type rather than
-- finish the copied binder's operation.
certifyEliminatedNonRootWeakenReplay
    :: Constraint p
    -> Constraint q
    -> (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> NodeId
    -> Maybe WeakenReplayCertificate
certifyEliminatedNonRootWeakenReplay before after canonical source target root = do
    let targetRepresentative = canonical target
        rootC = canonical root
        interiorRoot = expansionInteriorRootRef before canonical rootC
    if targetRepresentative == target || targetRepresentative == rootC
        then Nothing
        else pure ()
    path <- flexibleRefPathToRoot before canonical (typeRef target) interiorRoot
    if unchangedFlexibleSuffix before after canonical (dropLast path)
        then pure ()
        else Nothing
    descendants <- strictTypeDescendants before canonical target
    pure
        WeakenReplayCertificate
            { weakenReplayCertificateSource = source
            , weakenReplayCertificateTarget = targetRepresentative
            , weakenReplayCertificateReplayBinder = target
            , weakenReplayCertificateRoot = rootC
            , weakenReplayCertificateFlexiblePath = path
            , weakenReplayCertificateDescendants = descendants
            }

flexibleRefPathToRoot
    :: Constraint p
    -> (NodeId -> NodeId)
    -> NodeRef
    -> NodeRef
    -> Maybe [NodeRef]
flexibleRefPathToRoot constraint canonical target root =
    go IntSet.empty target []
  where
    go seen current acc
        | current == root = Just (reverse (current : acc))
        | IntSet.member (nodeRefKey current) seen = Nothing
        | otherwise =
            case Binding.lookupBindParent constraint current of
                Just (parent, BindFlex) ->
                    go
                        (IntSet.insert (nodeRefKey current) seen)
                        (canonicalizeRef canonical parent)
                        (current : acc)
                _ -> Nothing

unchangedFlexibleSuffix
    :: Constraint p
    -> Constraint q
    -> (NodeId -> NodeId)
    -> [NodeRef]
    -> Bool
unchangedFlexibleSuffix before after canonical =
    all unchanged
  where
    unchanged ref =
        let refC = canonicalizeRef canonical ref
        in case Binding.lookupBindParent before refC of
            Nothing -> Binding.lookupBindParent after refC == Nothing
            Just (parentBefore, BindFlex) ->
                case Binding.lookupBindParent after refC of
                    Just (parentAfter, BindFlex) ->
                        canonicalizeRef canonical parentBefore
                            == canonicalizeRef canonical parentAfter
                    _ -> False
            Just _ -> False

canonicalizeRef :: (NodeId -> NodeId) -> NodeRef -> NodeRef
canonicalizeRef canonical ref =
    case ref of
        TypeRef node -> TypeRef (canonical node)
        GenRef gid -> GenRef gid

dropLast :: [a] -> [a]
dropLast values =
    case values of
        [] -> []
        [_] -> []
        value : rest -> value : dropLast rest

-- | The expansion result and its copied binders are scheme siblings when the
-- result is directly owned by a gen node.  In that representation the gen
-- node, rather than the result type node, is the binding-tree authority for
-- I(r).  Otherwise the result node itself is the authority.
expansionInteriorRootRef
    :: Constraint p
    -> (NodeId -> NodeId)
    -> NodeId
    -> NodeRef
expansionInteriorRootRef constraint canonical root =
    let rootC = canonical root
    in case Binding.lookupBindParent constraint (typeRef rootC) of
        Just (owner@GenRef{}, _flag) -> owner
        _ -> typeRef rootC

strictTypeDescendants
    :: Constraint p
    -> (NodeId -> NodeId)
    -> NodeId
    -> Maybe IntSet.IntSet
strictTypeDescendants constraint canonical target = do
    raw <- either (const Nothing) Just (Binding.interiorOf constraint (typeRef target))
    pure $
        IntSet.delete (getNodeId target) $
            IntSet.fromList
                [ getNodeId (canonical node)
                | key <- IntSet.toList raw
                , TypeRef node <- [nodeRefFromKey key]
                ]

-- | Per-edge provenance for instantiation-related operations.
--
-- Source-ID contract (consumed by Φ):
--   * `EdgeWitness.ewWitness` operation node IDs
--   * `etRoot`
--   * the source side of `etBinderArgs`
--   * `etCopyMap` keys
--   * `etInterior`
-- all live in one source-ID domain.
--
-- The argument side of `etBinderArgs` is an instantiation/destination ID;
-- `etCopyMap` values are destination IDs; and replay-map values are replay
-- IDs.  These mappings are preserved verbatim across final materialization,
-- but their two sides intentionally inhabit different domains.  Consumers
-- canonicalize destination IDs locally when they need solved-graph lookup;
-- replacing them in the trace would discard the construction provenance.
--
-- Destination-ID contract (consumed by Ω normalization):
--   * `etResultRoot` is the authoritative root @r@ of the expansion at the
--     edge destination.  It is produced by edge expansion construction and
--     may be outside the binding-tree interior of the source `etRoot`.
--
-- Replay-map contract:
--   * `etBinderReplayMap` maps source binder keys to replay-domain binder
--     nodes selected during presolution normalization.
--   * `etReplayDomainBinders`, when non-empty, records the producer-approved
--     replay-domain TyVar binders that Phase 4 must validate against instead
--     of re-deriving binder scope from `etRoot`.
--   * The map is required, total over the source binder domain (`etBinderArgs`),
--     and injective over replay-domain TyVar binders.
--   * This operation-authority domain can be wider than the leading quantified
--     producer scheme after final unification.  It must not be used as a
--     quantifier count: Phase 4 classifies the actual replay spine from the
--     producer `SchemeInfo`, then matches those identities through this map.
--
-- Canonical IDs are derived locally at lookup sites. Global canonicalization
-- must not rewrite provenance collections across this boundary.
--
-- This is an internal aid for gradually aligning presolution witnesses with
-- `papers/these-finale-english.txt`’s normalized instance-operation language
-- (see `papers/xmlf.txt` Fig. 10). For now, we only track the binder↦argument
-- pairing chosen by `ExpInstantiate`.
data EdgeTrace = EdgeTrace
    { etRoot :: NodeId -- ^ Source scheme root used by Φ/replay provenance.
    , etResultRoot :: NodeId -- ^ Destination expansion root used to validate rewritten Ω.
    , etBinderArgs :: [(NodeId, NodeId)] -- ^ (binder node, instantiation argument node)
    , etInterior :: EdgeSourceInterior -- ^ Frozen source nodes in I(r).
    , etReplayContract :: ReplayContract -- ^ Producer-owned replay contract authority.
    , etBinderReplayMap :: IntMap NodeId -- ^ source binder key -> replay-domain binder node
    , etReplayDomainBinders :: [NodeId] -- ^ Explicit replay-domain binders for strict replay lanes.
    , etCopyMap :: CopyMapping -- ^ Provenance: original node -> copied/replaced node
    }
    deriving (Eq, Show)

-- | Decide Figure 15.3.4's flexible terminal @RaiseMerge(r,n')@ case
-- entirely from one edge's frozen source-domain trace.  The rigid-target lane
-- carries replay metadata for the exact source root and begins with
-- @Weaken(r)@; replay owned by another source binder on the same edge does not
-- classify the root transition.
rootRaiseMergeTraceAuthority :: NodeId -> NodeId -> EdgeTrace -> Bool
rootRaiseMergeTraceAuthority operated exterior traceInfo =
    operated == etRoot traceInfo
        && IntSet.member (getNodeId operated) sourceInterior
        && IntSet.notMember (getNodeId exterior) sourceInterior
        && not (sourceOwnsReplay operated)
        && replayContractLeavesRootFlexible
  where
    EdgeSourceInterior (InteriorNodes sourceInterior) = etInterior traceInfo
    sourceOwnsReplay source =
        any ((== source) . fst) (etBinderArgs traceInfo)
            || IntMap.member
                (getNodeId source)
                (etBinderReplayMap traceInfo)
    replayContractLeavesRootFlexible =
        case etReplayContract traceInfo of
            ReplayContractNone ->
                null (etBinderArgs traceInfo)
                    && IntMap.null (etBinderReplayMap traceInfo)
                    && null (etReplayDomainBinders traceInfo)
            ReplayContractStrict ->
                not (null (etBinderArgs traceInfo))
                    && not (IntMap.null (etBinderReplayMap traceInfo))
                    && not (null (etReplayDomainBinders traceInfo))

-- | Decide the rigid-target root lane from its exact producer bridge.  The
-- normalized operation sequence is @Weaken(r); RaiseMerge(r,n')@: weakening
-- changes the operation-time binding flag of @r@, so both computations are
-- identity.  This authority must never be used to emit @alpha_n' triangle@.
rootRigidRaiseMergeTraceAuthority :: NodeId -> NodeId -> EdgeTrace -> Bool
rootRigidRaiseMergeTraceAuthority operated exterior traceInfo =
    operated == etRoot traceInfo
        && IntSet.member (getNodeId operated) sourceInterior
        && IntSet.notMember (getNodeId exterior) sourceInterior
        && etReplayContract traceInfo == ReplayContractStrict
        && case
            ( etBinderArgs traceInfo,
              IntMap.toList (etBinderReplayMap traceInfo),
              etReplayDomainBinders traceInfo
            )
          of
            ( [(sourceRoot, _argument)],
              [(sourceKey, replayRoot)],
              [domainRoot]
              ) ->
                sourceRoot == operated
                    && sourceKey == getNodeId operated
                    && replayRoot == domainRoot
                    && lookupCopy operated (etCopyMap traceInfo) == Just replayRoot
            _ -> False
  where
    EdgeSourceInterior (InteriorNodes sourceInterior) = etInterior traceInfo

-- | Validate the trace half of an adjacent
-- @Weaken(r); RaiseMerge(r,n')@ construction certificate.  The operation
-- pair itself records that @r@ became rigid; the trace may come either from a
-- copied strict-replay expansion or from an uncopied/no-replay expansion.
-- Callers must check the adjacent same-root pair before using this predicate.
rootWeakenRaiseMergeTraceAuthority :: NodeId -> NodeId -> EdgeTrace -> Bool
rootWeakenRaiseMergeTraceAuthority operated exterior traceInfo =
    rootRigidRaiseMergeTraceAuthority operated exterior traceInfo
        || rootRaiseMergeTraceAuthority operated exterior traceInfo

newtype CopyMapping = CopyMapping { getCopyMapping :: IntMap NodeId }
    deriving (Eq, Show)

instance Semigroup CopyMapping where
    CopyMapping a <> CopyMapping b = CopyMapping (IntMap.union a b)

instance Monoid CopyMapping where
    mempty = CopyMapping IntMap.empty

{-# INLINE lookupCopy #-}
lookupCopy :: NodeId -> CopyMapping -> Maybe NodeId
lookupCopy nid (CopyMapping m) = IntMap.lookup (getNodeId nid) m

{-# INLINE insertCopy #-}
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

-- | Frozen source-domain membership for one instantiation edge.  These IDs
-- align with witness operands, trace roots/binder arguments, and copy-map
-- keys; they must not be rewritten when the destination graph is copied or
-- canonicalized.
newtype EdgeSourceInterior = EdgeSourceInterior
    { getEdgeSourceInterior :: InteriorNodes
    }
    deriving (Eq, Show)

instance Semigroup EdgeSourceInterior where
    EdgeSourceInterior a <> EdgeSourceInterior b = EdgeSourceInterior (a <> b)

instance Monoid EdgeSourceInterior where
    mempty = EdgeSourceInterior mempty

sourceInteriorFromList :: [NodeId] -> EdgeSourceInterior
sourceInteriorFromList = EdgeSourceInterior . fromListInterior

sourceInteriorFromSet :: IntSet.IntSet -> EdgeSourceInterior
sourceInteriorFromSet = EdgeSourceInterior . InteriorNodes

-- | Membership of the fresh/canonical graph constructed at an edge
-- destination.  This domain is used only while executing or reusing chi_e and
-- must never be stored in 'EdgeTrace.etInterior'.
newtype EdgeDestinationInterior = EdgeDestinationInterior
    { getEdgeDestinationInterior :: InteriorSet
    }
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

{-# INLINE memberInterior #-}
memberInterior :: NodeId -> InteriorNodes -> Bool
memberInterior nid (InteriorNodes s) = IntSet.member (getNodeId nid) s

{-# INLINE memberFrontier #-}
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

-- | Record the non-semantic collapse of a 'TyExp' occurrence wrapper onto the
-- root constructed for its propagation.  Replays may report the same result
-- class; distinct result classes are an invariant violation.
recordExpansionResult :: NodeId -> NodeId -> PresolutionM p ()
recordExpansionResult wrapper result0 = do
    st <- get
    let c = psConstraint st
        canonical = UnionFind.frWith (psUnionFind st)
        wrapperClass = canonical wrapper
        result = canonical result0
    case NodeAccess.lookupNode c wrapper of
        Just TyExp{} -> pure ()
        Just node ->
            throwError $
                InternalError $
                    "expansion result expected TyExp wrapper, got " ++ show node
        Nothing -> throwError (NodeLookupFailed wrapper)
    normalized <-
        either throwError pure $
            canonicalizeExpansionResultMap canonical (psExpansionResults st)
    case lookupExpansionResult wrapperClass normalized of
        Nothing ->
            put $
                st
                    { psExpansionResults =
                        ExpansionResultMap $
                            IntMap.insert
                                (getNodeId wrapperClass)
                                result
                                (getExpansionResultMap normalized)
                    }
        Just existing
            | existing == result ->
                put st { psExpansionResults = normalized }
            | otherwise ->
                throwError (ExpansionResultConflict wrapperClass existing result)

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
        (_c0, canonical, quotient) <- cachedBindingModelM
        let
            expansionRootC = canonical expansionRoot
            bindParents = Binding.qbpBindParents quotient
        destinationGen <-
            case BindingPath.bindingPathToRootLocal
                bindParents
                (typeRef expansionRootC) of
                Left err -> throwError (BindingTreeError err)
                Right path ->
                    case [gref | gref@GenRef{} <- path] of
                        (gref : _) -> pure gref
                        [] ->
                            throwError
                                (InternalError "expansion root has no destination gen binder")
        st0 <- get
        let cBefore = psConstraint st0
            -- Expansion ownership is defined when an argument is first
            -- allocated.  A reused argument may already have been Raised by
            -- an earlier edge-local operation; lowering it back to the
            -- destination would violate the monotone binding-tree semantics
            -- and immediately require the same Raise again.
            cCandidate =
                foldr
                    bindFreshArgument
                    cBefore
                    pairs
            bindFreshArgument (_binder, arg) c =
                let argC = canonical arg
                    existingParent =
                        Binding.lookupBindParent cBefore (typeRef arg)
                            <|> Binding.lookupBindParent cBefore (typeRef argC)
                    bindOne ownedArg =
                        Binding.setBindParent
                            (typeRef ownedArg)
                            (destinationGen, BindFlex)
                in case existingParent of
                    Just _ -> c
                    Nothing -> bindOne arg (bindOne argC c)
        (cScoped, raiseTrace) <-
            case BoundScope.repairAllVarBoundScopes canonical cCandidate of
                Left err -> throwError (BindingTreeError err)
                Right result -> pure result
        if null raiseTrace
            then do
                let dirtyBindRefs =
                        BoundScope.changedBindParentRefs cBefore cScoped
                if IntSet.null dirtyBindRefs
                    then pure ()
                    else put (setConstraintDirtyBindRefsState dirtyBindRefs cScoped st0)
            else
                throwError
                    (ExpansionArgumentScopeRepairRequired expansionRootC raiseTrace)

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
        repairCacheValid cached =
            cbrmGraphVersion cached == psGraphVersion st0
                && cbrmUnionFindVersion cached == psUnionFindVersion st0
        mbValidRepairCache =
            case psBindingRepairCache st0 of
                Just cached | repairCacheValid cached -> Just cached
                _ -> Nothing
        dirty0 =
            case psBindingRepairDirty st0 of
                Just dirty
                    | not (bindingRepairDirtyIsEmpty dirty) -> dirty
                _ ->
                    case mbValidRepairCache of
                        Just _ -> emptyBindingRepairDirty
                        Nothing -> dirtyAllBindingRepair
    case (mbValidRepairCache, psBindingModelCache st0, bindingRepairDirtyIsEmpty dirty0) of
        (Just _, Just cached, True)
            | cbmGraphVersion cached == psGraphVersion st0
            , cbmUnionFindVersion cached == psUnionFindVersion st0
            , cbmBindParentsVersion cached == psBindParentsVersion st0 ->
                pure BindingRepairSkipped
        _ -> repairBindingParentsFromScratchOrDirty c0 uf canonical mbValidRepairCache dirty0

repairBindingParentsFromScratchOrDirty
    :: Constraint p
    -> IntMap NodeId
    -> (NodeId -> NodeId)
    -> Maybe (CachedBindingRepairModel p)
    -> BindingRepairDirty
    -> PresolutionM p BindingRepairOutcome
repairBindingParentsFromScratchOrDirty c0 uf canonical mbValidRepairCache dirty0 = do
    st0 <- get
    qbp0 <-
        case Binding.quotientBindParentsContextUnder canonical c0 of
            Left err -> throwError (BindingTreeError err)
            Right result -> pure result
    let bp0 = Binding.qbpBindParents qbp0
        repairModel =
            case mbValidRepairCache of
                Just cached -> cbrmModel cached
                Nothing -> buildBindingRepairModel canonical c0
        repairCache =
            CachedBindingRepairModel
                { cbrmGraphVersion = psGraphVersion st0
                , cbrmUnionFindVersion = psUnionFindVersion st0
                , cbrmModel = repairModel
                }
        bp1 =
            if brdDirtyAll dirty0
                then repairBindingParentsWithModel repairModel bp0
                else repairBindingParentsDirtyWithModel dirty0 repairModel bp0
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
                , Binding.qbpRawParentAssignments = IntMap.map (: []) bp1
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
                , psBindingRepairCache = Just repairCache
                , psBindingRepairDirty = Nothing
                , psCachedRootGen = if changed then Nothing else psCachedRootGen st0
                , psBinderCache = if changed then IntMap.empty else psBinderCache st0
                }
    put stFinal
    pure
        ( if not changed
            then BindingRepairSkipped
            else
                if brdDirtyAll dirty0
                    then BindingRepairFull
                    else BindingRepairIncremental
        )

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
    (c0, canonical, _qbp) <- cachedBindingModelM
    let interiorRootRef = traceInteriorRootRef canonical c0 root0
    refs <- interiorOfUnderCachedM canonical interiorRootRef
    pure $
        IntSet.fromList
            [ getNodeId (canonical node)
            | key <- IntSet.toList refs
            , TypeRef node <- [nodeRefFromKey key]
            ]

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

-- | Resolve the instantiation binders for a node, skipping vacuous forall
-- nodes.  A single graph forall remains a single instantiation boundary.
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
                                Just TyForall {tnBody = inner} -> canonical inner
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
        schemeRoots =
            case NodeAccess.lookupGenNode c0 gid of
                Nothing -> []
                Just gen -> map canonical (gnSchemes gen)
        ownsBody root =
            root == bodyC
                || case VarStore.lookupVarBound c0 root of
                    Just bound -> canonical bound == bodyC
                    Nothing -> False
        hasSchemeProvenance = any ownsBody schemeRoots

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
            if not hasSchemeProvenance
                then []
                else if bodyIsWrapper
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
        identityEdgeIds = dropEdgeIds `IntSet.union` cGraftedEdges constraint
        identityEdges' = eaIdentityEdges edgeArtifacts `IntSet.union` identityEdgeIds
    in setEdgeArtifactsIdentityEdges
        identityEdges'
        ( filterEdgeArtifacts
            (\edgeId -> IntSet.notMember (getEdgeId edgeId) dropEdgeIds)
            edgeArtifacts
        )

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
