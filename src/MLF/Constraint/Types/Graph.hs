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

Core node and edge definitions are in 'MLF.Constraint.Types.Graph.NodeEdge'.
Binding-related types are in 'MLF.Constraint.Types.Graph.Binding'.
Accessor utilities are in 'MLF.Constraint.Types.Graph.Accessors'.
This module re-exports all submodules and adds the 'Constraint' record.
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
    -- * Re-exports from Binding
    BindFlag (..),
    BindParents,
    BindingError (..),
    EliminatedVars,
    WeakenedVars,
    PolySyms,
    -- * Accessors
    maxNodeIdKeyOr0,
    -- * Constraint record
    Constraint (..),
) where

import Data.IntSet (IntSet)

import qualified MLF.Constraint.Types.Graph.Accessors as Accessors
import MLF.Constraint.Types.Graph.Binding
import MLF.Constraint.Types.Graph.NodeEdge

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
maxNodeIdKeyOr0 c = Accessors.maxNodeIdKeyOr0 (cNodes c)
