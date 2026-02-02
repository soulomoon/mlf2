{-# LANGUAGE DeriveTraversable #-}
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
-}
module MLF.Constraint.Types.Graph (
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
    Constraint (..),
    maxNodeIdKeyOr0,
    BindFlag (..),
    BindParents,
    BindingError (..),
    EliminatedVars,
    WeakenedVars,
    PolySyms
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import Data.Set (Set)

-- | Stable identifier for a node in the term-DAG (`TyNode`).
--
-- This is an internal identifier (not a source-level variable name). It is the
-- key used for `Constraint.cNodes`.
newtype NodeId = NodeId { getNodeId :: Int }
    deriving (Eq, Ord, Show)

-- | Map keyed by `NodeId`s.
newtype NodeMap a = NodeMap { getNodeMap :: IntMap a }
    deriving (Eq, Show, Functor, Foldable, Traversable)

lookupNode :: NodeId -> NodeMap a -> Maybe a
lookupNode nid (NodeMap nodes) = IntMap.lookup (getNodeId nid) nodes

insertNode :: NodeId -> a -> NodeMap a -> NodeMap a
insertNode nid value (NodeMap nodes) = NodeMap (IntMap.insert (getNodeId nid) value nodes)

deleteNode :: NodeId -> NodeMap a -> NodeMap a
deleteNode nid (NodeMap nodes) = NodeMap (IntMap.delete (getNodeId nid) nodes)

fromListNode :: [(NodeId, a)] -> NodeMap a
fromListNode pairs =
    NodeMap (IntMap.fromList (map (\(NodeId k, v) -> (k, v)) pairs))

toListNode :: NodeMap a -> [(NodeId, a)]
toListNode (NodeMap nodes) =
    map (\(k, v) -> (NodeId k, v)) (IntMap.toList nodes)

-- | Identifier for a gen node (paper G constructor).
--
-- This is a distinct sort from type nodes in the thesis.
newtype GenNodeId = GenNodeId { getGenNodeId :: Int }
    deriving (Eq, Ord, Show)

-- | Map keyed by `GenNodeId`s.
newtype GenNodeMap a = GenNodeMap { getGenNodeMap :: IntMap a }
    deriving (Eq, Show, Functor, Foldable, Traversable)

lookupGen :: GenNodeId -> GenNodeMap a -> Maybe a
lookupGen gid (GenNodeMap nodes) = IntMap.lookup (getGenNodeId gid) nodes

insertGen :: GenNodeId -> a -> GenNodeMap a -> GenNodeMap a
insertGen gid value (GenNodeMap nodes) = GenNodeMap (IntMap.insert (getGenNodeId gid) value nodes)

deleteGen :: GenNodeId -> GenNodeMap a -> GenNodeMap a
deleteGen gid (GenNodeMap nodes) = GenNodeMap (IntMap.delete (getGenNodeId gid) nodes)

fromListGen :: [(GenNodeId, a)] -> GenNodeMap a
fromListGen pairs =
    GenNodeMap (IntMap.fromList (map (\(GenNodeId k, v) -> (k, v)) pairs))

toListGen :: GenNodeMap a -> [(GenNodeId, a)]
toListGen (GenNodeMap nodes) =
    map (\(k, v) -> (GenNodeId k, v)) (IntMap.toList nodes)

-- | References to nodes in the binding tree (Type or Gen).
data NodeRef
    = TypeRef NodeId
    | GenRef GenNodeId
    deriving (Eq, Ord, Show)

typeRef :: NodeId -> NodeRef
typeRef = TypeRef

genRef :: GenNodeId -> NodeRef
genRef = GenRef

nodeRefKey :: NodeRef -> Int
nodeRefKey ref = case ref of
    TypeRef (NodeId i) -> i * 2
    GenRef (GenNodeId i) -> i * 2 + 1

nodeRefFromKey :: Int -> NodeRef
nodeRefFromKey k
    | even k = TypeRef (NodeId (k `div` 2))
    | otherwise = GenRef (GenNodeId ((k - 1) `div` 2))

typeRefKey :: NodeId -> Int
typeRefKey = nodeRefKey . TypeRef

genRefKey :: GenNodeId -> Int
genRefKey = nodeRefKey . GenRef

genNodeKey :: GenNodeId -> Int
genNodeKey = getGenNodeId

-- | Identifier for an instantiation edge.
--
-- Used to attach presolution decisions and witnesses to a particular (≤) edge.
newtype EdgeId = EdgeId { getEdgeId :: Int }
    deriving (Eq, Ord, Show)

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

-- | Gen node record (paper G constructor).
--
-- Each gen node introduces zero or more scheme roots.
data GenNode = GenNode
    { gnId :: GenNodeId
    , gnSchemes :: [NodeId]
    }
    deriving (Eq, Show)

-- | Identifier for an expansion variable (paper: the expansion parameter `s`).
--
-- Presolution assigns an `Expansion` recipe to each `ExpVarId`.
newtype ExpVarId = ExpVarId { getExpVarId :: Int }
    deriving (Eq, Ord, Show)

-- | Base type constructor (opaque name, e.g. "Int", "Bool").
newtype BaseTy = BaseTy { getBaseName :: String }
    deriving (Eq, Ord, Show)

{- Note [Representation overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines the *data model* for the “graphic constraint” pipeline.

Paper context
-------------
In `papers/these-finale-english.txt` (see also `papers/xmlf.txt`), *graphic types*
are described as:

  - a **term-DAG** for type constructors / sharing (§"Graphic types"), and
  - a **binding tree** encoding scope and which variables may be generalized.

Our implementation keeps that split explicit:

  - The term-DAG is `cNodes :: NodeMap TyNode` (nodes referenced by `NodeId`).
  - The binding tree is `cBindParents :: BindParents` (child → parent + flag).

This separation is intentional: binding structure is not embedded in the
term-DAG constructors.

Phases at a glance
------------------
  - Phase 1 (ConstraintGen): builds `Constraint` (nodes, binding edges, edges).
  - Phase 2 (Normalize): local rewrites (grafting + merge/unify) on the graph.
  - Phase 3: checks/derives a dependency order for instantiation edges.
  - Phase 4 (Presolution): chooses minimal expansions, records witnesses.
  - Phase 5 (Solve): discharges remaining unifications; returns `SolveResult`.
  - Phase 6 (Elab): reifies solved graph and witnesses into xMLF terms/types.

The rest of this file documents the invariants that those phases rely on.
-}

-- | A node in the term-DAG of graphic types.
--
-- Nodes are stored in `Constraint.cNodes` and referred to by stable `NodeId`s.
-- We keep sharing explicitly: multiple parents may reference the same child.
data TyNode
    = TyVar
        { tnId :: NodeId
        , tnBound :: Maybe NodeId
        }
    -- | Bottom type node (⊥).
    --
    -- This is introduced by the omega-elimination rewrite pass to inline
    -- eliminated binders with no explicit bound.
    | TyBottom
        { tnId :: NodeId
        }
    -- | Arrow type node (τ₁ → τ₂).
    --
    -- In the paper’s term-DAG view, `TyArrow` is a constructor node with two
    -- children. Sharing is explicit: multiple arrows may point to the same
    -- `tnDom`/`tnCod` subgraphs.
    | TyArrow
        { tnId :: NodeId
        , tnDom :: NodeId
        , tnCod :: NodeId
        }
    -- | Base type constant (e.g. Int/Bool/String).
    | TyBase
        { tnId :: NodeId
        , tnBase :: BaseTy
        }
    -- | Explicit quantifier node (∀).
    --
    -- Quantifier binders are represented by binding edges (`Constraint.cBindParents`):
    -- the direct flexibly-bound children of this node (paper Q(n)).
    | TyForall
        { tnId :: NodeId
        , tnBody :: NodeId
        }
    -- | Expansion node (paper: “expansion variable” machinery).
    --
    -- This node represents “apply an expansion recipe `s` to the underlying type
    -- graph rooted at `tnBody`.” Presolution chooses an `Expansion` for each
    -- `ExpVarId` and later materializes it, eliminating all `TyExp` nodes.
    --
    -- See Note [Expansion nodes].
    | TyExp
        { tnId :: NodeId
        , tnExpVar :: ExpVarId
        , tnBody :: NodeId
        }
    deriving (Eq, Show)

-- | Immediate structural children of a term-DAG node.
--
-- Traversal order is stable and sometimes significant:
-- arrow domain before codomain; single-child constructors yield a singleton
-- list.
--
-- Note: instance bounds are not structural children; ≺ ordering accounts for
-- them separately when needed.
structuralChildren :: TyNode -> [NodeId]
structuralChildren TyVar{} = []
structuralChildren TyBottom{} = []
structuralChildren TyBase{} = []
structuralChildren TyArrow{ tnDom = d, tnCod = c } = [d, c]
structuralChildren TyForall{ tnBody = b } = [b]
structuralChildren TyExp{ tnBody = b } = [b]

-- | Structural children plus the bound child for TyVar nodes (when present).
structuralChildrenWithBounds :: TyNode -> [NodeId]
structuralChildrenWithBounds node =
    case node of
        TyVar{ tnBound = Just bnd } -> structuralChildren node ++ [bnd]
        _ -> structuralChildren node

-- | Lookup a node by `NodeId` in the constraint node map.
lookupNodeIn :: NodeMap a -> NodeId -> Maybe a
lookupNodeIn nodes = flip lookupNode nodes

{- Note [Expansion nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Paper link
----------
The thesis (`papers/these-finale-english.txt`; see `papers/xmlf.txt`) uses
*expansions* and *instantiation
constraints* to express and solve let-polymorphism and first-class polymorphism.
An instantiation edge is considered solved when the constraint graph is an
instance of the expansion graph; the paper then translates a *normalized*
instantiation witness into an xMLF instantiation (Figure 10, §3.4).

Terminology mapping
-------------------
The paper’s constraints are usually written χ; the “expansion of an edge e in χ”
is χₑ (built by copying the interior of a binding node and unifying it with the
edge target). We do not construct χₑ as a separate value. Instead, we keep a
first-class `TyExp` node at the would-be expansion root and let presolution
materialize the effect of χₑ by rewriting the graph according to the chosen
`Expansion`.

What we represent
-----------------
We model the paper’s “apply an expansion to a type graph” as an explicit node:

  TyExp { tnExpVar = s, tnBody = τ }

Intuitively: “the type of this occurrence is `E_s(τ)` for some expansion recipe
E_s that presolution will choose.”

Important: in the current implementation, `TyExp` nodes are introduced at
*occurrence sites* (e.g. `EVar` wraps the referenced node in a fresh `TyExp`).
This gives presolution a uniform place to attach per-occurrence expansion
decisions and to record per-edge witnesses for elaboration.

Shape
-----
* `TyExp` carries (s, τ) where `s :: ExpVarId` is the expansion variable and
  `τ :: NodeId` is the root of the underlying type graph.
* `Expansion` is the solver’s explicit *recipe* for `s` (chosen in presolution):
  identity, ∀-introduction, instantiation (substitution), and composition.
  These correspond to the xMLF instantiation constructs in
  `papers/these-finale-english.txt` (see `papers/xmlf.txt` for numbering):
    - identity instantiation `1`
    - quantifier introduction (`O` / `InstIntro` in our elaborator)
    - elimination + substitution (paper often abbreviates this as type app ⟨τ⟩)
    - sequential composition `φ; φ′`
  See the thesis section on instantiations and `papers/xmlf.txt` Figures 1–3.

Flow across phases
------------------
* Phase 1 (constraint gen): variable occurrences are wrapped in `TyExp`, and
  applications emit instantiation edges `left ≤ right` where `left` is often a
  `TyExp`.
* Phase 3 (acyclicity): orders these edges so presolution can process them
    without dependency cycles.
* Phase 4 (presolution): chooses minimal `Expansion`s, records per-edge
  witness data, eagerly performs necessary unifications, and finally rewrites
  the graph to eliminate all `TyExp` nodes.
* Phase 5 (solve): assumes `TyExp` nodes are gone; survivors are rejected
  (`UnexpectedExpNode`) because they would make reification/elaboration ill-defined.

Why explicit TyExp + Expansion
------------------------------
* Gives a concrete handle for the paper’s “expansion variable” mechanism.
* Keeps sharing explicit (term-DAG), matching the paper’s emphasis that copying
  must preserve sharing outside the expansion interior.
* Preserves enough provenance to later produce xMLF instantiations from
  witnesses (Φ/Σ; see `papers/these-finale-english.txt` and
  `papers/xmlf.txt` §3.4, Figure 10).
-}

{- Note [Binding tree]
~~~~~~~~~~~~~~~~~~~~~~
This implementation models scope using the paper-style binding tree, stored as
binding edges in `Constraint.cBindParents`.

Quantifier binders (paper Q(n)) are represented by the direct flexibly-bound
`TyVar` children of a binder node (`TyForall`), optionally carrying instance
bounds on the `TyVar` itself.
-}

-- | Instantiation edge (Tₗ ≤ Tᵣ) connecting a polymorphic binding to the
-- type shape it must instantiate to; Phase 4 consumes these edges when
-- computing minimal expansions.
--
-- Paper mapping (`papers/these-finale-english.txt`; see `papers/xmlf.txt`):
-- instantiation edges correspond to the
-- *instance* relation (≤) between types; solving an edge yields a witness Ω
-- that can be translated to an xMLF instantiation Φ (Figure 10, §3.4). In this
-- codebase, presolution records that information as an `EdgeWitness`.
data InstEdge = InstEdge
    { instEdgeId :: EdgeId
    , instLeft :: NodeId
    , instRight :: NodeId
    }
    deriving (Eq, Show)

{- Note [Unification edges (=)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unification edges (T₁ = T₂) are *pure equality* constraints between type nodes.
They correspond to the paper’s “graphical unification” component (TLDI 2007 §3)
and are conceptually distinct from instantiation edges (≤), which require
presolution choices.

Where they come from
--------------------
  1. Constraint generation: argument/parameter equality, lambda/application
     structure, etc.
  2. Normalization (grafting): decomposing an instantiation constraint against
     known structure may generate equalities on sub-components.
  3. Presolution: expansion decisions may request additional unifications so
     the expanded LHS matches the RHS.

How they are processed here
---------------------------
We process `UnifyEdge`s in two places:

  - Phase 2 (`MLF.Normalize.mergeUnifyEdges`) performs *local* incremental
    unification to keep the constraint in a locally-solved form.
  - Phase 5 (`MLF.Solve.solveUnify`) runs after presolution has eliminated
    `TyExp` and drained instantiation edges, and then discharges the remaining
    unification work, producing the final union-find map.

Both phases prefer structured representatives over variables when merging.

Errors
------
Constructor clashes (Arrow vs Base, Base mismatch, ∀-level mismatch) ultimately
surface as residual edges or `SolveError`s. (Normalization may keep impossible
edges to defer error reporting until a later phase with more context.)
-}

-- | Unification edge (T₁ = T₂) representing a monotype equality constraint.
-- These are processed by Phase 2 (Normalize) and Phase 5 (Solve) via union-find.
-- See Note [Unification edges (=)].
data UnifyEdge = UnifyEdge
    { uniLeft :: NodeId
    , uniRight :: NodeId
    }
    deriving (Eq, Show)

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
