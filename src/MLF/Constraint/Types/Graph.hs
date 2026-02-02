{- |
Module      : MLF.Constraint.Types.Graph
Description : Graph identifiers and core constraint graph types
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines the core data types for representing MLF type constraints
as a graph structure. The constraint graph consists of:

* 'TyNode' - Type nodes (variables, arrows, foralls, base types)
* 'GenNode' - Generalization nodes that own scheme roots
* 'InstEdge' - Instantiation edges between nodes
* 'Constraint' - The complete constraint graph

It is intentionally cohesive so callers can depend on the core graph types
without importing the full constraint type surface.

= Paper References

* Rémy & Yakobowski, "From ML to MLF" (ICFP 2008) - Constraint graph model
* Rémy & Yakobowski, "Graphic Type Constraints" - Binding tree structure

= Key Concepts

The constraint graph uses a /binding tree/ to track the scope of type variables.
Each node has a binding parent (either a type node or a gen node) and a binding
flag ('BindFlex' or 'BindRigid') that determines whether it can be generalized.

= Module Structure

Core node and edge definitions are in 'MLF.Constraint.Types.Graph.NodeEdge'
and re-exported here. This module adds binding-related types and the
'Constraint' record.
-}
module MLF.Constraint.Types.Graph (
    -- * Re-exports from NodeEdge
    NodeId (..),
    NodeMap (..),
    lookupNode,
    insertNode,
    deleteNode,
    fromListNode,
    toListNode,
    GenNodeId (..),
    GenNodeMap (..),
    lookupGen,
    insertGen,
    deleteGen,
    fromListGen,
    toListGen,
    NodeRef (..),
    typeRef,
    genRef,
    nodeRefKey,
    nodeRefFromKey,
    typeRefKey,
    genRefKey,
    genNodeKey,
    EdgeId (..),
    GenNode (..),
    ExpVarId (..),
    BaseTy (..),
    TyNode (..),
    structuralChildren,
    structuralChildrenWithBounds,
    lookupNodeIn,
    InstEdge (..),
    UnifyEdge (..),
    -- * Binding types
    BindFlag (..),
    BindParents,
    BindingError (..),
    EliminatedVars,
    WeakenedVars,
    PolySyms,
    -- * Constraint record
    Constraint (..),
    maxNodeIdKeyOr0,
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import Data.Set (Set)

import MLF.Constraint.Types.Graph.NodeEdge

-- | Flag indicating whether a binding edge is flexible or rigid.
--
-- Paper mapping (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1):
-- flexible binders can be raised/weakened
-- during instantiation; rigid binders are locked and cannot be modified.
data BindFlag = BindFlex | BindRigid
    deriving (Eq, Ord, Show)

-- | Binding-edge map: child NodeRef -> (parent NodeRef, flag).
--
-- This represents the paper's binding tree explicitly. Every non-root node
-- has exactly one binding parent. Roots are nodes that do not appear as keys.
type BindParents = IntMap (NodeRef, BindFlag)

-- | Errors that can occur when validating or manipulating the binding tree.
data BindingError
    = MissingBindParent NodeRef
        -- ^ A node that should have a binding parent does not have one.
    | BindingCycleDetected [NodeRef]
        -- ^ A cycle was detected in the binding-parent chain.
    | NoCommonAncestor NodeRef NodeRef
        -- ^ Two nodes have no common ancestor in the binding tree.
    | ParentNotUpper NodeRef NodeRef
        -- ^ The parent is not "upper" than the child in the term-DAG.
        -- First NodeId is the child, second is the parent.
    | OperationOnLockedNode NodeRef
        -- ^ Attempted to raise/weaken a node that is locked (rigidly bound path).
    | RaiseNotPossible NodeRef
        -- ^ Raise step not possible (e.g., parent is already a root).
    | GenFallbackRequired
        { fallbackBinder :: NodeId
        , fallbackGen :: GenNodeId
        , fallbackBinders :: [NodeId]
        }
        -- ^ Type-node binder enumeration would require a gen-ancestor fallback.
    | GenSchemeFreeVars
        { schemeRoot :: NodeId
        , schemeGen :: GenNodeId
        , freeNodes :: [NodeId]
        }
        -- ^ A scheme root reaches named nodes not bound under its gen node.
    | InvalidBindingTree String
        -- ^ Generic binding tree invariant violation with description.
    deriving (Eq, Show)

-- | Persistent marker for variables eliminated during ω execution / presolution.
type EliminatedVars = IntSet

-- | Variables whose binding edge was weakened by ω (OpWeaken).
type WeakenedVars = IntSet

-- | Polymorphic type constructor symbols (paper Poly set).
type PolySyms = Set BaseTy

{- Note [Binding tree]
~~~~~~~~~~~~~~~~~~~~~~
This implementation models scope using the paper-style binding tree, stored as
binding edges in `Constraint.cBindParents`.

Quantifier binders (paper Q(n)) are represented by the direct flexibly-bound
`TyVar` children of a binder node (`TyForall`), optionally carrying instance
bounds on the `TyVar` itself.
-}

-- | Snapshot of the entire graphic constraint graph produced by Phase 1. Each
-- solver phase mutates/consumes different slices of this structure, so we keep
-- them together instead of recomputing them:
--
--   * Phase 2 rewrites 'cNodes' via canonicalization rules.
--   * Phase 3 reads 'cInstEdges' to build the dependency graph.
--   * Phase 4 consumes 'cInstEdges' while extending 'cNodes'.
--   * Phase 5 consumes 'cUnifyEdges' to merge entries inside 'cNodes'.
--
-- Subsequent elaboration phases also read the same graph to reconstruct typed
-- terms, so this record is the single source of truth for the constraint state.
data Constraint = Constraint
        { cNodes :: NodeMap TyNode
            -- ^ The hash-consed DAG of graphic type nodes. Every structural operation
            --   (unification, grafting, elaboration) needs fast access to the canonical
            --   representation of each node, so we store them in a NodeMap keyed by
            --   'NodeId'. This is the core object manipulated by Phases 2–5.
        , cInstEdges :: [InstEdge]
            -- ^ Outstanding instantiation constraints T_left ≤ T_right. Phase 4 consumes
            --   this list (usually in dependency order) to determine minimal expansions
            --   for each expansion variable and to emit follow-up unification work.
        , cUnifyEdges :: [UnifyEdge]
            -- ^ Pending monotype equalities T1 = T2 produced either during constraint
            --   generation or as a side effect of instantiation. The graphic unifier in
            --   Phase 2 may reduce these locally; Phase 5 drains the remainder and
            --   detects inconsistencies.
        , cBindParents :: BindParents
            -- ^ Paper-style binding edges: child NodeRef -> (parent NodeRef, BindFlag).
            --
            --   This is the explicit representation of the paper's binding tree
            --   (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1).
            --   Every non-root node has exactly one binding
            --   parent. Roots are nodes that do not appear as keys in this map.
            --
            --   The binding tree is used for:
            --   - Raise/Weaken operations (ω in the paper)
            --   - Computing the interior I(r) of an expansion
            --   - Determining which nodes are "instantiable" vs "locked"
            --
            --   See Note [Binding Tree] in MLF.Binding.Tree for details.
        , cPolySyms :: PolySyms
            -- ^ Polymorphic symbols (paper Poly set), used when computing inert nodes.
        , cEliminatedVars :: EliminatedVars
            -- ^ Variables eliminated during presolution/ω execution.
            --
            -- Elaboration ignores eliminated vars when reifying quantifiers.
        , cWeakenedVars :: WeakenedVars
            -- ^ Variables weakened for translatability (inert-locked / rigidify passes).
            --
            -- These are treated as flexible during reification/generalization so
            -- Theorem 15.2.11 weakening does not change the elaborated type.
        , cAnnEdges :: IntSet
            -- ^ Instantiation edges introduced by term annotations (κσ).
            --
            -- These edges update bounds but should not eliminate binders.
        , cLetEdges :: IntSet
            -- ^ Instantiation edges introduced by let-scoping (body → trivial root).
            --
            -- These are internal edges for the alternative let-typing translation
            -- and should be ignored when translating witnesses.
        , cGenNodes :: GenNodeMap GenNode
            -- ^ Gen node map (paper G constructors).
            --
            -- This is separate from the term-DAG so we can follow the thesis
            -- notion of gen nodes as a distinct sort.
        }
    deriving (Eq, Show)

-- | Maximum NodeId key present in `cNodes`, defaulting to 0 for an empty map.
--
-- This is used to initialize fresh NodeId counters in phases that allocate new
-- nodes during rewriting (Normalize, Presolution).
maxNodeIdKeyOr0 :: Constraint -> Int
maxNodeIdKeyOr0 c =
    case lookupMaxNode (cNodes c) of
        Nothing -> 0
        Just (NodeId k, _node) -> k

lookupMaxNode :: NodeMap a -> Maybe (NodeId, a)
lookupMaxNode (NodeMap nodes) =
    fmap (\(k, v) -> (NodeId k, v)) (IntMap.lookupMax nodes)
