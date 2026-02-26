{- |
Module      : MLF.Constraint.Solved
Description : Opaque abstraction over solved constraint graphs
Copyright   : (c) 2024
License     : BSD-3-Clause

Provides an opaque 'Solved' type backed by an equivalence-class
representation and exposes read-only queries over the solved constraint
graph. This is the single entry point for post-solve access — downstream
phases (elaboration, phi translation, omega) should use these queries
instead of reaching into solver internals directly.

`fromSolveOutput` consumes `solveUnifyWithSnapshot` output and constructs
the equivalence-class backend from the pre-rewrite snapshot.
-}
module MLF.Constraint.Solved (
    -- * Opaque type
    Solved,
    fromSolveOutput,
    mkTestSolved,
    fromPreRewriteState,

    -- * Core queries
    canonical,
    canonicalMap,
    originalConstraint,
    solvedConstraint,
    lookupNode,
    allNodes,
    lookupBindParent,
    bindParents,
    instEdges,
    genNodes,
    lookupVarBound,

    -- * Constraint rebuilding
    rebuildWithConstraint,

    -- * Mutation helpers
    patchNode,
    pruneBindParentsSolved,
    rebuildWithNodes,
    rebuildWithBindParents,
    rebuildWithGenNodes,

    -- * Extended queries
    classMembers,
    originalNode,
    originalBindParent,
    wasOriginalBinder,

    -- * Canonical-domain queries
    weakenedVars,
    isEliminatedVar,
    canonicalBindParents,
    canonicalGenNodes,
    canonicalNodes,
    lookupCanonicalNode,
    lookupCanonicalVarBound,
) where

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import MLF.Constraint.Solve
    ( SolveError
    , SolveOutput(..)
    , SolveSnapshot(..)
    , solveResultFromSnapshot
    )
import MLF.Constraint.Solve.Internal (SolveResult(..))
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindParents
    , Constraint(..)
    , GenNode(..)
    , GenNodeMap(..)
    , InstEdge
    , NodeId(..)
    , NodeMap(..)
    , NodeRef(..)
    , TyNode(..)
    , genNodeKey
    , nodeRefFromKey
    )
import qualified MLF.Constraint.NodeAccess as NA
import qualified MLF.Constraint.VarStore as VarStore

-- -----------------------------------------------------------------
-- Opaque type
-- -----------------------------------------------------------------

-- | Opaque handle to a solved constraint graph, backed by an
-- equivalence-class representation.
data SolvedBackend
    = EquivBackend
        { ebCanonicalMap :: IntMap NodeId
        , ebCanonicalConstraint :: Constraint
        , ebEquivClasses :: IntMap [NodeId]
        , ebOriginalConstraint :: Constraint
        }
    deriving (Eq, Show)

newtype Solved = Solved { unSolved :: SolvedBackend }
    deriving (Eq, Show)

-- | Test helper: construct an EquivBackend from a constraint and
-- union-find map. The constraint serves as both original and canonical
-- (no pre-rewrite snapshot needed for tests with empty union-find).
mkTestSolved :: Constraint -> IntMap NodeId -> Solved
mkTestSolved c uf =
    let canonicalMap = buildCanonicalMap uf c
        equivClasses = buildEquivClasses canonicalMap c
    in Solved EquivBackend
        { ebCanonicalMap = canonicalMap
        , ebCanonicalConstraint = c
        , ebEquivClasses = equivClasses
        , ebOriginalConstraint = c
        }

-- | Build a staged equivalence backend from snapshot-enabled solve output.
fromSolveOutput :: SolveOutput -> Either SolveError Solved
fromSolveOutput out =
    let snapshot = soSnapshot out
    in fromPreRewriteStateStrict
        (snapUnionFind snapshot)
        (snapPreRewriteConstraint snapshot)

-- | Build a staged equivalence-backend snapshot from solver pre-rewrite state.
--
-- The input pair should come from solve after unification has converged:
-- a pre-rewrite constraint and the final union-find map.
fromPreRewriteState :: IntMap NodeId -> Constraint -> Either SolveError Solved
fromPreRewriteState = fromPreRewriteStateStrict

-- | Strict snapshot replay used by production solve output conversion.
fromPreRewriteStateStrict :: IntMap NodeId -> Constraint -> Either SolveError Solved
fromPreRewriteStateStrict uf preRewrite = do
    let snapshot =
            SolveSnapshot
                { snapUnionFind = uf
                , snapPreRewriteConstraint = preRewrite
                }
    replayed <- solveResultFromSnapshot snapshot
    let
        canonicalMap = buildCanonicalMap (srUnionFind replayed) preRewrite
        canonicalConstraint = srConstraint replayed
        equivClasses = buildEquivClasses canonicalMap preRewrite
    pure $ Solved EquivBackend
        { ebCanonicalMap = canonicalMap
        , ebCanonicalConstraint = canonicalConstraint
        , ebEquivClasses = equivClasses
        , ebOriginalConstraint = preRewrite
        }

buildCanonicalMap :: IntMap NodeId -> Constraint -> IntMap NodeId
buildCanonicalMap uf c =
    let nodeKeys = map (getNodeId . tnId) (NA.allNodes c)
        allKeys = IntSet.toList (IntSet.fromList (nodeKeys ++ IntMap.keys uf))
        -- Snapshot UF may contain parent cycles from intermediate states.
        -- Use cycle-safe chasing to keep staged reconstruction total.
        canonicalNode = chaseUfCanonical uf
    in IntMap.fromList
        [ (k, rep)
        | k <- allKeys
        , let rep = canonicalNode (NodeId k)
        , rep /= NodeId k
        ]

chaseUfCanonical :: IntMap NodeId -> NodeId -> NodeId
chaseUfCanonical uf = go IntSet.empty
  where
    step nid = IntMap.findWithDefault nid (nodeIdKey nid) uf

    go seen current =
        let next = step current
        in if next == current
            then current
            else if IntSet.member (nodeIdKey next) seen
                then cycleRepresentative next
                else go (IntSet.insert (nodeIdKey current) seen) next

    cycleRepresentative cycleStart = goCycle cycleStart cycleStart
      where
        goCycle minNode current =
            let next = step current
                minNode' = minByNodeId minNode next
            in if next == cycleStart
                then minNode'
                else goCycle minNode' next

    minByNodeId a b =
        if nodeIdKey a <= nodeIdKey b
            then a
            else b

buildEquivClasses :: IntMap NodeId -> Constraint -> IntMap [NodeId]
buildEquivClasses canonMap c =
    foldr addNode IntMap.empty (map tnId (NA.allNodes c))
  where
    addNode nid classes =
        let rep = equivCanonical canonMap nid
        in IntMap.insertWith (++) (nodeIdKey rep) [nid] classes

nodeIdKey :: NodeId -> Int
nodeIdKey (NodeId k) = k

-- | Chase Equiv canonical links to a fixed point.
--
-- Unlike legacy union-find canonicalization (`frWith`), Equiv canonical data is
-- stored as direct links in `ebCanonicalMap`. We chase those links to a stable
-- representative and break cycles deterministically by choosing the smallest
-- `NodeId` in the cycle.
equivCanonical :: IntMap NodeId -> NodeId -> NodeId
equivCanonical canonMap = go IntSet.empty
  where
    step nid = IntMap.findWithDefault nid (nodeIdKey nid) canonMap

    go seen current =
        let next = step current
        in if next == current
            then current
            else if IntSet.member (nodeIdKey next) seen
                then cycleRepresentative next
                else go (IntSet.insert (nodeIdKey current) seen) next

    cycleRepresentative cycleStart = goCycle cycleStart cycleStart
      where
        goCycle minNode current =
            let next = step current
                minNode' = minByNodeId minNode next
            in if next == cycleStart
                then minNode'
                else goCycle minNode' next

    minByNodeId a b =
        if nodeIdKey a <= nodeIdKey b
            then a
            else b

-- -----------------------------------------------------------------
-- Core queries
-- -----------------------------------------------------------------

-- | Chase the canonical map to the canonical representative.
canonical :: Solved -> NodeId -> NodeId
canonical (Solved EquivBackend { ebCanonicalMap = canonMap }) nid =
    equivCanonical canonMap nid

-- | The raw canonical map (equivalent to the old union-find accessor).
canonicalMap :: Solved -> IntMap NodeId
canonicalMap (Solved EquivBackend { ebCanonicalMap = cm }) = cm

-- | The original (pre-solving) constraint.  Primary accessor for all
-- post-solve operations (thesis-exact).
originalConstraint :: Solved -> Constraint
originalConstraint (Solved EquivBackend { ebOriginalConstraint = c }) = c

-- | The canonical (post-solving) constraint.
--
-- /Deprecated/: prefer the opaque queries ('lookupNode', 'allNodes',
-- 'lookupBindParent', 'bindParents', 'instEdges', 'genNodes',
-- 'lookupVarBound') which already read from the original constraint.
-- This accessor and the underlying 'ebCanonicalConstraint' field will
-- be removed once all external callers are migrated.
-- See Note [solvedConstraint migration status].
solvedConstraint :: Solved -> Constraint
solvedConstraint (Solved EquivBackend { ebCanonicalConstraint = c }) = c

-- | Look up a type node, canonicalizing the id first.
lookupNode :: Solved -> NodeId -> Maybe TyNode
lookupNode s@(Solved EquivBackend { ebOriginalConstraint = c }) nid =
    NA.lookupNode c (canonical s nid)

-- | All type nodes in the solved constraint.
allNodes :: Solved -> [TyNode]
allNodes (Solved EquivBackend { ebOriginalConstraint = c }) = NA.allNodes c

-- | Look up the binding parent of a node reference.
lookupBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent (Solved EquivBackend { ebOriginalConstraint = c }) ref =
    NA.lookupBindParent c ref

-- | The full bind-parents map.
bindParents :: Solved -> BindParents
bindParents (Solved EquivBackend { ebOriginalConstraint = c }) = cBindParents c

-- | All instantiation edges.
instEdges :: Solved -> [InstEdge]
instEdges (Solved EquivBackend { ebOriginalConstraint = c }) = cInstEdges c

-- | The gen-node map.
genNodes :: Solved -> GenNodeMap GenNode
genNodes (Solved EquivBackend { ebOriginalConstraint = c }) = cGenNodes c

-- | Look up the instance bound of a variable, canonicalizing the id first.
lookupVarBound :: Solved -> NodeId -> Maybe NodeId
lookupVarBound s@(Solved EquivBackend { ebOriginalConstraint = c }) nid =
    NA.lookupVarBound c (canonical s nid)

-- -----------------------------------------------------------------
-- Constraint rebuilding
-- -----------------------------------------------------------------

-- | Rebuild a Solved with a different constraint, preserving the
-- canonical map. Used by callers that modify the constraint
-- (e.g., alias insertion, canonicalization).
rebuildWithConstraint :: Solved -> Constraint -> Solved
rebuildWithConstraint (Solved EquivBackend { ebCanonicalMap = cm, ebEquivClasses = ec, ebOriginalConstraint = orig }) c =
    Solved EquivBackend
        { ebCanonicalMap = cm
        , ebCanonicalConstraint = c
        , ebEquivClasses = ec
        , ebOriginalConstraint = orig
        }

-- -----------------------------------------------------------------
-- Mutation helpers
-- -----------------------------------------------------------------

-- | Modify a node in the canonical constraint by its canonical ID.
-- Used by Fallback.hs to patch TyVar bounds.
patchNode :: Solved -> NodeId -> (TyNode -> TyNode) -> Solved
patchNode (Solved eb@EquivBackend { ebCanonicalConstraint = c }) nid f =
    let NodeMap nodes = cNodes c
        nodes' = NodeMap (IntMap.adjust f (getNodeId nid) nodes)
        c' = c { cNodes = nodes' }
    in Solved eb { ebCanonicalConstraint = c' }

-- | Prune dead bind-parent entries from the canonical constraint.
-- Used by Pipeline.hs.
pruneBindParentsSolved :: Solved -> Solved
pruneBindParentsSolved (Solved eb@EquivBackend { ebCanonicalConstraint = c }) =
    let liveNodes = getNodeMap (cNodes c)
        liveGens = getGenNodeMap (cGenNodes c)
        liveRef ref =
            case ref of
                TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
                GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        liveChild childKey = liveRef (nodeRefFromKey childKey)
        bindParents' =
            IntMap.filterWithKey
                (\childKey (parentRef, _flag) ->
                    liveChild childKey && liveRef parentRef
                )
                (cBindParents c)
    in Solved eb { ebCanonicalConstraint = c { cBindParents = bindParents' } }

-- | Replace the canonical node map.
-- Used by Generalize phases that build merged node maps.
rebuildWithNodes :: Solved -> NodeMap TyNode -> Solved
rebuildWithNodes (Solved eb@EquivBackend { ebCanonicalConstraint = c }) nodes =
    Solved eb { ebCanonicalConstraint = c { cNodes = nodes } }

-- | Replace the canonical bind parents.
rebuildWithBindParents :: Solved -> BindParents -> Solved
rebuildWithBindParents (Solved eb@EquivBackend { ebCanonicalConstraint = c }) bp =
    Solved eb { ebCanonicalConstraint = c { cBindParents = bp } }

-- | Replace the canonical gen nodes.
rebuildWithGenNodes :: Solved -> GenNodeMap GenNode -> Solved
rebuildWithGenNodes (Solved eb@EquivBackend { ebCanonicalConstraint = c }) gn =
    Solved eb { ebCanonicalConstraint = c { cGenNodes = gn } }

-- -----------------------------------------------------------------
-- Extended queries
-- -----------------------------------------------------------------

-- | Members of the equivalence class containing @nid@.
classMembers :: Solved -> NodeId -> [NodeId]
classMembers s@(Solved EquivBackend { ebEquivClasses = classes }) nid =
    let nidC = canonical s nid
    in IntMap.findWithDefault [] (nodeIdKey nidC) classes

-- | Look up the /original/ (pre-merge) node.
originalNode :: Solved -> NodeId -> Maybe TyNode
originalNode (Solved EquivBackend { ebOriginalConstraint = c }) nid =
    NA.lookupNode c nid

-- | Look up the /original/ (pre-merge) binding parent.
originalBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
originalBindParent (Solved EquivBackend { ebOriginalConstraint = c }) ref =
    NA.lookupBindParent c ref

-- | Was @nid@ a binder (forall body owner) before merges?
wasOriginalBinder :: Solved -> NodeId -> Bool
wasOriginalBinder s@(Solved EquivBackend { ebOriginalConstraint = c }) nid =
    any isForall (classMembers s nid)
  where
    isForall member =
        case NA.lookupNode c member of
            Just TyForall {} -> True
            _ -> False

-- -----------------------------------------------------------------
-- Canonical-domain queries
-- -----------------------------------------------------------------

-- | Weakened variable IDs from the canonical (post-solve) constraint.
weakenedVars :: Solved -> IntSet
weakenedVars (Solved EquivBackend { ebCanonicalConstraint = c }) = cWeakenedVars c

-- | Is the node an eliminated variable in the canonical constraint?
isEliminatedVar :: Solved -> NodeId -> Bool
isEliminatedVar (Solved EquivBackend { ebCanonicalConstraint = c }) nid =
    VarStore.isEliminatedVar c nid

-- | Bind parents from the canonical (post-solve) constraint.
canonicalBindParents :: Solved -> BindParents
canonicalBindParents (Solved EquivBackend { ebCanonicalConstraint = c }) = cBindParents c

-- | Gen nodes from the canonical constraint.
canonicalGenNodes :: Solved -> GenNodeMap GenNode
canonicalGenNodes (Solved EquivBackend { ebCanonicalConstraint = c }) = cGenNodes c

-- | The canonical node map (post-solve nodes keyed by canonical IDs).
canonicalNodes :: Solved -> NodeMap TyNode
canonicalNodes (Solved EquivBackend { ebCanonicalConstraint = c }) = cNodes c

-- | Look up a node in the canonical constraint by its canonical ID.
--
-- Unlike 'lookupNode', this does /not/ canonicalize the ID first —
-- the caller is expected to provide a canonical ID.
lookupCanonicalNode :: Solved -> NodeId -> Maybe TyNode
lookupCanonicalNode (Solved EquivBackend { ebCanonicalConstraint = c }) nid =
    NA.lookupNode c nid

-- | Look up the instance bound of a variable in the canonical constraint.
--
-- Unlike 'lookupVarBound', this does /not/ canonicalize the ID first
-- and reads from the canonical (post-solve) constraint rather than the
-- original. The caller is expected to provide a canonical ID.
lookupCanonicalVarBound :: Solved -> NodeId -> Maybe NodeId
lookupCanonicalVarBound (Solved EquivBackend { ebCanonicalConstraint = c }) nid =
    VarStore.lookupVarBound c nid

{- Note [solvedConstraint migration status]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tasks 14-17 migrated the core opaque queries (lookupNode, allNodes,
lookupBindParent, bindParents, instEdges, genNodes, lookupVarBound)
to read from ebOriginalConstraint instead of ebCanonicalConstraint.

However, ~20 external callers still use solvedConstraint to obtain the
raw canonical Constraint and then access its fields directly (cNodes,
cBindParents, cGenNodes, lookupVarBound, etc.). These callers fall
into two categories:

  (a) Read-only: use the canonical constraint for node lookup,
      var-bound chasing, bind-parent traversal, gen-node enumeration,
      order-key computation, reification, and validation.
      Files: Reify/Core.hs, Reify/TypeOps.hs, Elab/Legacy.hs,
      Elab/Elaborate.hs, Elab/Run/TypeOps.hs, Elab/Run/Scope.hs,
      Elab/Run/ResultType/Ann.hs, Elab/Run/ResultType/Fallback.hs,
      Util/Order.hs, Presolution/Plan.hs,
      Elab/Run/Generalize (Phase1-4, Finalize).

  (b) Read-write: obtain the canonical constraint, modify it (e.g.
      alias insertion, bind-parent pruning, node-map replacement),
      and store it back via rebuildWithConstraint.
      Files: Elab/Run/Pipeline.hs, Elab/Generalize.hs,
      Elab/Run/ResultType/Fallback.hs, Presolution/Plan/Context.hs.

  (c) Test: PipelineSpec.hs feeds solvedConstraint into
      validateSolvedGraphStrict via SolveResult.

To remove ebCanonicalConstraint, all category (a) callers must be
migrated to use the opaque queries or originalConstraint + canonical.
Category (b) callers need a replacement for the rebuild pattern
(possibly operating on originalConstraint directly). Category (c)
callers need a validation path that works with the original constraint.

This is tracked as future work beyond Task 18.
-}
