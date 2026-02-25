{- |
Module      : MLF.Constraint.Solved
Description : Opaque abstraction over solved constraint graphs
Copyright   : (c) 2024
License     : BSD-3-Clause

Provides an opaque 'Solved' type that wraps 'SolveResult' and exposes
read-only queries over the solved constraint graph. This is the single
entry point for post-solve access — downstream phases (elaboration, phi
translation, omega) should use these queries instead of reaching into
'SolveResult' internals directly.

= Runtime path

`fromSolveResult` and `mkSolved` still produce the legacy backend for
compatibility.

`fromSolveOutput` consumes `solveUnifyWithSnapshot` output and constructs
the equivalence-class backend from the pre-rewrite snapshot.
-}
module MLF.Constraint.Solved (
    -- * Opaque type
    Solved,
    fromSolveResult,
    fromSolveOutput,
    mkSolved,
    mkTestSolved,
    fromPreRewriteState,

    -- * Core queries
    canonical,
    lookupNode,
    allNodes,
    lookupBindParent,
    bindParents,
    instEdges,
    genNodes,
    lookupVarBound,

    -- * Constraint rebuilding
    rebuildWithConstraint,

    -- * Escape hatches (Phase 1 only — removed in Phase 2)
    toSolveResult,
    unionFind,
    solvedConstraint,

    -- * Extended queries (degraded stubs in Phase 1)
    classMembers,
    originalNode,
    originalBindParent,
    wasOriginalBinder,
) where

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Solve
    ( SolveError
    , SolveOutput(..)
    , SolveResult
    , SolveSnapshot(..)
    , frWith
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
    , NodeRef(..)
    , TyNode(..)
    )
import qualified MLF.Constraint.NodeAccess as NA

-- -----------------------------------------------------------------
-- Opaque type
-- -----------------------------------------------------------------

-- | Opaque handle to a solved constraint graph.
--
-- The runtime path still uses 'LegacyBackend' via 'fromSolveResult',
-- but 'EquivBackend' is available for staged migration.
data SolvedBackend
    = LegacyBackend
        { lbConstraint :: Constraint
        , lbUnionFind :: IntMap NodeId
        }
    | EquivBackend
        { ebCanonicalMap :: IntMap NodeId
        , ebCanonicalConstraint :: Constraint
        , ebEquivClasses :: IntMap [NodeId]
        , ebOriginalConstraint :: Constraint
        }
    deriving (Eq, Show)

newtype Solved = Solved { unSolved :: SolvedBackend }
    deriving (Eq, Show)

-- | Wrap a 'SolveResult' into the opaque 'Solved' handle.
fromSolveResult :: SolveResult -> Solved
fromSolveResult sr =
    Solved LegacyBackend
        { lbConstraint = srConstraint sr
        , lbUnionFind = srUnionFind sr
        }

-- | Test helper: construct an EquivBackend from a constraint and
-- union-find map. Same interface as 'mkSolved' but produces the
-- EquivBackend. The constraint serves as both original and canonical
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

-- | Construct a 'Solved' directly from a constraint and union-find map.
-- This avoids the need to import 'SolveResult(..)' in test code.
mkSolved :: Constraint -> IntMap NodeId -> Solved
mkSolved c uf =
    Solved LegacyBackend
        { lbConstraint = c
        , lbUnionFind = uf
        }

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

-- | Chase the union-find to the canonical representative.
canonical :: Solved -> NodeId -> NodeId
canonical (Solved LegacyBackend { lbUnionFind = uf }) nid = frWith uf nid
canonical (Solved EquivBackend { ebCanonicalMap = canonMap }) nid =
    equivCanonical canonMap nid

-- | Look up a type node, canonicalizing the id first.
lookupNode :: Solved -> NodeId -> Maybe TyNode
lookupNode s@(Solved LegacyBackend { lbConstraint = c }) nid =
    NA.lookupNode c (canonical s nid)
lookupNode s@(Solved EquivBackend { ebCanonicalConstraint = c }) nid =
    NA.lookupNode c (canonical s nid)

-- | All type nodes in the solved constraint.
allNodes :: Solved -> [TyNode]
allNodes (Solved LegacyBackend { lbConstraint = c }) = NA.allNodes c
allNodes (Solved EquivBackend { ebCanonicalConstraint = c }) = NA.allNodes c

-- | Look up the binding parent of a node reference.
lookupBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent (Solved LegacyBackend { lbConstraint = c }) ref =
    NA.lookupBindParent c ref
lookupBindParent (Solved EquivBackend { ebCanonicalConstraint = c }) ref =
    NA.lookupBindParent c ref

-- | The full bind-parents map.
bindParents :: Solved -> BindParents
bindParents (Solved LegacyBackend { lbConstraint = c }) = cBindParents c
bindParents (Solved EquivBackend { ebCanonicalConstraint = c }) = cBindParents c

-- | All instantiation edges.
instEdges :: Solved -> [InstEdge]
instEdges (Solved LegacyBackend { lbConstraint = c }) = cInstEdges c
instEdges (Solved EquivBackend { ebCanonicalConstraint = c }) = cInstEdges c

-- | The gen-node map.
genNodes :: Solved -> GenNodeMap GenNode
genNodes (Solved LegacyBackend { lbConstraint = c }) = cGenNodes c
genNodes (Solved EquivBackend { ebCanonicalConstraint = c }) = cGenNodes c

-- | Look up the instance bound of a variable, canonicalizing the id first.
lookupVarBound :: Solved -> NodeId -> Maybe NodeId
lookupVarBound s@(Solved LegacyBackend { lbConstraint = c }) nid =
    NA.lookupVarBound c (canonical s nid)
lookupVarBound s@(Solved EquivBackend { ebCanonicalConstraint = c }) nid =
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
rebuildWithConstraint (Solved LegacyBackend { lbUnionFind = uf }) c =
    Solved LegacyBackend
        { lbConstraint = c
        , lbUnionFind = uf
        }

-- -----------------------------------------------------------------
-- Escape hatches (Phase 1 only)
-- -----------------------------------------------------------------

-- | Extract the underlying 'SolveResult'. Escape hatch for callers
-- not yet migrated to 'Solved' queries. Will be removed in Phase 2.
toSolveResult :: Solved -> SolveResult
toSolveResult (Solved LegacyBackend { lbConstraint = c, lbUnionFind = uf }) =
    SolveResult { srConstraint = c, srUnionFind = uf }
toSolveResult (Solved EquivBackend { ebCanonicalConstraint = c, ebCanonicalMap = cm }) =
    SolveResult { srConstraint = c, srUnionFind = cm }

-- | Raw union-find map. Will be removed in Phase 2.
unionFind :: Solved -> IntMap NodeId
unionFind (Solved LegacyBackend { lbUnionFind = uf }) = uf
unionFind (Solved EquivBackend { ebCanonicalMap = cm }) = cm

-- | Raw constraint. Will be removed in Phase 2 once all callers
-- are migrated to 'Solved' queries.
solvedConstraint :: Solved -> Constraint
solvedConstraint (Solved LegacyBackend { lbConstraint = c }) = c
solvedConstraint (Solved EquivBackend { ebCanonicalConstraint = c }) = c

-- -----------------------------------------------------------------
-- Extended queries (degraded stubs — Phase 1)
-- -----------------------------------------------------------------

-- | Members of the equivalence class containing @nid@.
--
-- Phase 1: returns @[canonical s nid]@ (singleton).
-- Phase 2: returns the full equivalence class.
classMembers :: Solved -> NodeId -> [NodeId]
classMembers s@(Solved LegacyBackend {}) nid = [canonical s nid]
classMembers s@(Solved EquivBackend { ebEquivClasses = classes }) nid =
    let nidC = canonical s nid
    in IntMap.findWithDefault [] (nodeIdKey nidC) classes

-- | Look up the /original/ (pre-merge) node.
--
-- Phase 1: delegates to 'lookupNode' (no pre-merge snapshot yet).
-- Phase 2: returns the node as it existed before unification merges.
originalNode :: Solved -> NodeId -> Maybe TyNode
originalNode s@(Solved LegacyBackend {}) nid = lookupNode s nid
originalNode (Solved EquivBackend { ebOriginalConstraint = c }) nid =
    NA.lookupNode c nid

-- | Look up the /original/ (pre-merge) binding parent.
--
-- Phase 1: delegates to 'lookupBindParent'.
-- Phase 2: returns the binding parent from the pre-merge snapshot.
originalBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
originalBindParent s@(Solved LegacyBackend {}) ref = lookupBindParent s ref
originalBindParent (Solved EquivBackend { ebOriginalConstraint = c }) ref =
    NA.lookupBindParent c ref

-- | Was @nid@ a binder (forall body owner) before merges?
--
-- Phase 1: always returns 'False' (no pre-merge data).
-- Phase 2: returns the real answer from the snapshot.
wasOriginalBinder :: Solved -> NodeId -> Bool
wasOriginalBinder (Solved LegacyBackend {}) _nid = False
wasOriginalBinder s@(Solved EquivBackend { ebOriginalConstraint = c }) nid =
    any isForall (classMembers s nid)
  where
    isForall member =
        case NA.lookupNode c member of
            Just TyForall {} -> True
            _ -> False
