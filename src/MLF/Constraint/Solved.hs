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

= Phase 1 (current)

The internal representation is 'SolveResult'; all queries delegate directly.
Escape hatches ('unionFind', 'solvedConstraint') are provided for callers
not yet migrated.

= Phase 2 (planned)

The backend switches to equivalence classes. Escape hatches are removed,
and 'classMembers' / 'originalNode' / 'originalBindParent' /
'wasOriginalBinder' return real data instead of degraded stubs.
-}
module MLF.Constraint.Solved (
    -- * Opaque type
    Solved,
    fromSolveResult,
    mkSolved,

    -- * Core queries
    canonical,
    lookupNode,
    allNodes,
    lookupBindParent,
    bindParents,
    instEdges,
    genNodes,
    lookupVarBound,

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

import MLF.Constraint.Solve (SolveResult, frWith)
import MLF.Constraint.Solve.Internal (SolveResult(..))
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , Constraint(..)
    , GenNode
    , GenNodeMap
    , InstEdge
    , NodeId
    , NodeRef
    , TyNode
    )
import qualified MLF.Constraint.NodeAccess as NA

-- -----------------------------------------------------------------
-- Opaque type
-- -----------------------------------------------------------------

-- | Opaque handle to a solved constraint graph.
--
-- In Phase 1 this is a thin wrapper around 'SolveResult'.
-- In Phase 2 the internal representation will switch to equivalence
-- classes; the query interface stays the same.
newtype Solved = Solved { unSolved :: SolveResult }
    deriving (Eq, Show)

-- | Wrap a 'SolveResult' into the opaque 'Solved' handle.
fromSolveResult :: SolveResult -> Solved
fromSolveResult = Solved

-- | Construct a 'Solved' directly from a constraint and union-find map.
-- This avoids the need to import 'SolveResult(..)' in test code.
mkSolved :: Constraint -> IntMap NodeId -> Solved
mkSolved c uf = Solved SolveResult { srConstraint = c, srUnionFind = uf }

-- -----------------------------------------------------------------
-- Core queries
-- -----------------------------------------------------------------

-- | Chase the union-find to the canonical representative.
canonical :: Solved -> NodeId -> NodeId
canonical s = frWith (srUnionFind (unSolved s))

-- | Look up a type node, canonicalizing the id first.
lookupNode :: Solved -> NodeId -> Maybe TyNode
lookupNode s nid = NA.lookupNode (srConstraint (unSolved s)) (canonical s nid)

-- | All type nodes in the solved constraint.
allNodes :: Solved -> [TyNode]
allNodes s = NA.allNodes (srConstraint (unSolved s))

-- | Look up the binding parent of a node reference.
lookupBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent s ref = NA.lookupBindParent (srConstraint (unSolved s)) ref

-- | The full bind-parents map.
bindParents :: Solved -> BindParents
bindParents s = cBindParents (srConstraint (unSolved s))

-- | All instantiation edges.
instEdges :: Solved -> [InstEdge]
instEdges s = cInstEdges (srConstraint (unSolved s))

-- | The gen-node map.
genNodes :: Solved -> GenNodeMap GenNode
genNodes s = cGenNodes (srConstraint (unSolved s))

-- | Look up the instance bound of a variable, canonicalizing the id first.
lookupVarBound :: Solved -> NodeId -> Maybe NodeId
lookupVarBound s nid = NA.lookupVarBound (srConstraint (unSolved s)) (canonical s nid)

-- -----------------------------------------------------------------
-- Escape hatches (Phase 1 only)
-- -----------------------------------------------------------------

-- | Extract the underlying 'SolveResult'. Escape hatch for callers
-- not yet migrated to 'Solved' queries. Will be removed in Phase 2.
toSolveResult :: Solved -> SolveResult
toSolveResult = unSolved

-- | Raw union-find map. Will be removed in Phase 2.
unionFind :: Solved -> IntMap NodeId
unionFind s = srUnionFind (unSolved s)

-- | Raw constraint. Will be removed in Phase 2 once all callers
-- are migrated to 'Solved' queries.
solvedConstraint :: Solved -> Constraint
solvedConstraint s = srConstraint (unSolved s)

-- -----------------------------------------------------------------
-- Extended queries (degraded stubs — Phase 1)
-- -----------------------------------------------------------------

-- | Members of the equivalence class containing @nid@.
--
-- Phase 1: returns @[canonical s nid]@ (singleton).
-- Phase 2: returns the full equivalence class.
classMembers :: Solved -> NodeId -> [NodeId]
classMembers s nid = [canonical s nid]

-- | Look up the /original/ (pre-merge) node.
--
-- Phase 1: delegates to 'lookupNode' (no pre-merge snapshot yet).
-- Phase 2: returns the node as it existed before unification merges.
originalNode :: Solved -> NodeId -> Maybe TyNode
originalNode = lookupNode

-- | Look up the /original/ (pre-merge) binding parent.
--
-- Phase 1: delegates to 'lookupBindParent'.
-- Phase 2: returns the binding parent from the pre-merge snapshot.
originalBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
originalBindParent = lookupBindParent

-- | Was @nid@ a binder (forall body owner) before merges?
--
-- Phase 1: always returns 'False' (no pre-merge data).
-- Phase 2: returns the real answer from the snapshot.
wasOriginalBinder :: Solved -> NodeId -> Bool
wasOriginalBinder _s _nid = False
