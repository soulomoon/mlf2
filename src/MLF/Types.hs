module MLF.Types (
    NodeId (..),
    GNodeId (..),
    ExpVarId (..),
    EdgeId (..),
    BaseTy (..),
    TyNode (..),
    GNode (..),
    InstEdge (..),
    UnifyEdge (..),
    Constraint (..),
    -- * Variable bounds + elimination stores (scope-model retirement)
    VarBounds,
    EliminatedVars,
    BoundRef(..),
    ForallSpec(..),
    Expansion (..),
    InstanceOp(..),
    InstanceWitness(..),
    EdgeWitness(..),
    Presolution (..),
    SolverState (..),
    DepGraph (..),
    -- * Binding tree types
    BindFlag (..),
    BindParents,
    -- * Binding tree errors
    BindingError (..)
) where

import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.IntSet (IntSet)

-- | Flag indicating whether a binding edge is flexible or rigid.
--
-- Paper mapping (`papers/xmlf.txt` §3.1): flexible binders can be raised/weakened
-- during instantiation; rigid binders are locked and cannot be modified.
data BindFlag = BindFlex | BindRigid
    deriving (Eq, Ord, Show)

-- | Binding-edge map: child NodeId -> (parent NodeId, flag).
--
-- This represents the paper's binding tree explicitly. Every non-root node
-- has exactly one binding parent. Roots are nodes that do not appear as keys.
type BindParents = IntMap (NodeId, BindFlag)

-- | Dedicated variable-bound store (replaces `GNode.gBinds` bounds).
--
-- Missing keys are treated as `Nothing` (⊥).
type VarBounds = IntMap (Maybe NodeId)

-- | Persistent marker for variables eliminated during ω execution / presolution.
type EliminatedVars = IntSet

-- | Reference to a bound used in forall introductions.
--
-- BoundBinder uses a binder index so bounds can be remapped to fresh binders.
data BoundRef
    = BoundNode NodeId
    | BoundBinder Int
    deriving (Eq, Show)

-- | Binder specification for quantifier introduction.
data ForallSpec = ForallSpec
    { fsBinderCount :: Int
    , fsBounds :: [Maybe BoundRef]
    }
    deriving (Eq, Show)

-- | Errors that can occur when validating or manipulating the binding tree.
data BindingError
    = MissingBindParent NodeId
        -- ^ A node that should have a binding parent does not have one.
    | BindingCycleDetected [NodeId]
        -- ^ A cycle was detected in the binding-parent chain.
    | ParentNotUpper NodeId NodeId
        -- ^ The parent is not "upper" than the child in the term-DAG.
        -- First NodeId is the child, second is the parent.
    | OperationOnLockedNode NodeId
        -- ^ Attempted to raise/weaken a node that is locked (rigidly bound path).
    | RaiseNotPossible NodeId
        -- ^ Raise step not possible (e.g., parent is already a root).
    | InvalidBindingTree String
        -- ^ Generic binding tree invariant violation with description.
    deriving (Eq, Show)

-- | Stable identifier for a node in the term-DAG (`TyNode`).
--
-- This is an internal identifier (not a source-level variable name). It is the
-- key used for `Constraint.cNodes`.
newtype NodeId = NodeId { getNodeId :: Int }
    deriving (Eq, Ord, Show)

-- | Stable identifier for a generalization level (paper: a “G-node”).
--
-- Levels form a forest (`Constraint.cGForest`) with parent links (`GNode.gParent`).
newtype GNodeId = GNodeId { getGNodeId :: Int }
    deriving (Eq, Ord, Show)

-- | Identifier for an expansion variable (paper: the expansion parameter `s`).
--
-- Presolution assigns an `Expansion` recipe to each `ExpVarId`.
newtype ExpVarId = ExpVarId { getExpVarId :: Int }
    deriving (Eq, Ord, Show)

-- | Identifier for an instantiation edge.
--
-- Used to attach presolution decisions and witnesses to a particular (≤) edge.
newtype EdgeId = EdgeId { getEdgeId :: Int }
    deriving (Eq, Ord, Show)

-- | Base type constructor (opaque name, e.g. "Int", "Bool").
newtype BaseTy = BaseTy { getBaseName :: String }
    deriving (Eq, Ord, Show)

{- Note [Representation overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module defines the *data model* for the “graphic constraint” pipeline.

Paper context
-------------
In `papers/xmlf.txt` (Rémy & Yakobowski), *graphic types* are described as:

  - a **term-DAG** for type constructors / sharing (§"Graphic types"), and
  - a **binding tree** encoding levels and which variables may be generalized.

Our implementation keeps that split explicit:

  - The term-DAG is `cNodes :: IntMap TyNode` (nodes referenced by `NodeId`).
  - The binding tree (levels) is `cGNodes :: IntMap GNode` plus roots `cGForest`.

This separation is intentional: it enforces the paper’s “no nested binding
nodes” shape structurally (see Note [G-node Push/Pull Invariant]).

Phases at a glance
------------------
  - Phase 1 (ConstraintGen): builds `Constraint` (nodes, G-nodes, edges).
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
        , tnVarLevel :: GNodeId
            -- ^ The level that *owns* this variable (paper: which G-node binds it).
            --
            -- In the paper’s presentation, variables are anonymous and drawn as ⊥
            -- nodes; the `NodeId` is their identity and sharing key.
            --   Used for:
            --   - generalization (variables eligible at a let-level),
            --   - instantiation scoping (fresh vars allocated at the right level).
            --   See Note [G-nodes].
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
    -- This is the term-DAG counterpart of the paper’s binding information.
    -- We encode a quantifier by *pointing to a level* (`tnQuantLevel`) whose
    -- binders live in the `GNode` forest (see Note [G-nodes]).
    --
    -- Two distinct “levels” matter:
    --
    --   - `tnQuantLevel`: which level’s variables are being quantified.
    --   - `tnOwnerLevel`: which outer level owns/introduces this quantifier node.
    --
    -- This split removes the historical ambiguity of a single `tnLevel` field.
    | TyForall
        { tnId :: NodeId
        , tnQuantLevel :: GNodeId
            -- ^ Level whose binders are quantified by this node.
            --
            -- The actual binder variables (and their optional instance bounds)
            -- are stored in `GNode.gBinds` for this level.
        , tnOwnerLevel :: GNodeId
            -- ^ Level that owns this `TyForall` node (i.e. where it was introduced).
            -- Used when deciding whether a quantifier behaves “rigidly” vs
            -- “flexibly” during reification/elaboration.
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

{- Note [Expansion nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Paper link
----------
The xMLF paper (`papers/xmlf.txt`) uses *expansions* and *instantiation
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
  These correspond to the xMLF instantiation constructs in `xmlf.txt`:
    - identity instantiation `1`
    - quantifier introduction (`O` / `InstIntro` in our elaborator)
    - elimination + substitution (paper often abbreviates this as type app ⟨τ⟩)
    - sequential composition `φ; φ′`
  See §"Instantiations" and Figures 1–3 of `xmlf.txt`.

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
  witnesses (Φ/Σ, `xmlf.txt` §3.4, Figure 10).
-}

-- | Generalization nodes model the levels introduced by let-generalization in
-- the graphic constraint. Each level forms a tree (parent/children) so that we
-- know which type variables may be quantified at a given scope and where that
-- scope sits relative to its ancestors.
--
-- * 'gnodeId' is the stable identifier for this level.
-- * 'gParent' points to the enclosing level (Nothing for the root g₀).
-- * 'gBinds' lists the 'TyVar' node ids whose binding level is this node; they
--   are the candidates for ∀-introduction when solving. Each binder may carry
--   an optional *instance bound* (paper: ∀(α ⩾ τ). ...; `Nothing` corresponds
--   to ⊥ / an unbounded quantifier).
-- * 'gChildren' enumerates nested levels created by inner let-bindings, giving
--   the solver the forest structure it needs for scope checks.
--   See Note [G-nodes].
data GNode = GNode
    { gnodeId :: GNodeId
    , gParent :: Maybe GNodeId
    , gBinds :: [(NodeId, Maybe NodeId)]
        -- ^ List of (TyVar, Optional Bound) pairs bound at this level.
        -- The optional bound is the NodeId of the type τ in ∀(α ⩾ τ).
        -- `Nothing` corresponds to the paper’s default bound ⊥ (i.e. ∀(α ⩾ ⊥)).
    , gChildren :: [GNodeId]
    }
    deriving (Eq, Show)

{- Note [G-nodes]
~~~~~~~~~~~~~~~~~
Paper link
----------
In `xmlf.txt`, graphic types combine a term-DAG with a *binding tree* that
encodes scope and (flexible) bounds for quantified variables.

What a level means here
-----------------------
`GNode` values are our explicit representation of that binding tree:

  - a new child level is created for each let-binding scope
  - `TyVar` nodes record which level owns them (`tnVarLevel`)
  - `TyForall` nodes quantify *a level* (`tnQuantLevel`) and are owned by some
    outer level (`tnOwnerLevel`)

This matches how the paper speaks about nodes belonging to the interior of a
G-node and about edges crossing level boundaries.

Queries supported
-----------------
We encode that hierarchy so later phases can answer:

    * Which variables are allowed to generalize at this scope?  → 'gBinds'.
    * Does this scope sit under another generalization level?     → 'gParent'.
    * Which nested lets did this binding introduce?               → 'gChildren'.

How binders/quantifiers are represented
--------------------------------------
We do not store binder *names* inside `TyForall`. Instead:

  - `TyForall` stores a *level id* (`tnQuantLevel`).
  - The binders at that level are listed in `gBinds` of the corresponding `GNode`.
  - Each binder can carry an optional *instance bound* (paper: ∀(α ⩾ τ). ...).

Elaboration/reification then binds exactly the variables at `tnQuantLevel` that
are actually reachable in the `tnBody` subgraph (to avoid vacuous binders).

Why keep a forest of roots
--------------------------
`cGForest` allows multiple roots (e.g. multiple entry points / independent
top-level components) without forcing an artificial single root.

Note [G-node Push/Pull Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The paper (§4) describes a "push/pull G-nodes" transformation that ensures
G-nodes appear only at the top of types, not nested inside type constructors.
In our implementation, this invariant is enforced structurally by the data
types themselves:

  1. G-nodes live in a separate forest: 'cGNodes :: IntMap GNode'
  2. Type nodes only *reference* G-nodes via level fields ('tnVarLevel',
     'tnQuantLevel', 'tnOwnerLevel')
  3. Type constructors like 'TyArrow' contain 'NodeId' fields, not 'GNode's

This means we never embed the *binding tree* (levels / G-nodes) inside the
term-DAG: constructors only carry `NodeId`s plus level *references*. As a
result, there is nothing to “push” or “pull” — the binding structure is always
top-level in `cGNodes`.

Note: this does **not** restrict higher-rank polymorphism. A `TyArrow` may
reference a `TyForall` node as one of its children; what is ruled out is only
embedding `GNode` structure itself inside the term-DAG.
-}

-- | Instantiation edge (Tₗ ≤ Tᵣ) connecting a polymorphic binding to the
-- type shape it must instantiate to; Phase 4 consumes these edges when
-- computing minimal expansions.
--
-- Paper mapping (`papers/xmlf.txt`): instantiation edges correspond to the
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
--   * Phase 2 rewrites 'cGNodes'/'cNodes' via canonicalization rules.
--   * Phase 3 reads 'cInstEdges' to build the dependency graph.
--   * Phase 4 consumes 'cInstEdges' while extending 'cNodes'.
--   * Phase 5 consumes 'cUnifyEdges' to merge entries inside 'cNodes'.
--
-- Subsequent elaboration phases also read the same graph to reconstruct typed
-- terms, so this record is the single source of truth for the constraint state.
data Constraint = Constraint
        { cGForest :: [GNodeId]
            -- ^ Roots of the generalization forest (most programs yield a singleton [g₀]).
            --   Keeping track of the forest roots lets us traverse all scopes even when
            --   multiple disconnected components exist (e.g. for multiple top-level lets).
            --   See Note [G-nodes].
        , cGNodes :: IntMap GNode
            -- ^ The lookup table for G-nodes keyed by their 'GNodeId'. The solver uses
            --   this to answer queries like "what is the parent of this level?" or
            --   "which TyVar nodes are bound here?" whenever it decides whether a
            --   variable may be generalized or instantiated. See Note [G-nodes].
        , cNodes :: IntMap TyNode
            -- ^ The hash-consed DAG of graphic type nodes. Every structural operation
            --   (unification, grafting, elaboration) needs fast access to the canonical
            --   representation of each node, so we store them in an IntMap keyed by
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
            -- ^ Paper-style binding edges: child NodeId -> (parent NodeId, BindFlag).
            --
            --   This is the explicit representation of the paper's binding tree
            --   (`papers/xmlf.txt` §3.1). Every non-root node has exactly one binding
            --   parent. Roots are nodes that do not appear as keys in this map.
            --
            --   The binding tree is used for:
            --   - Raise/Weaken operations (ω in the paper)
            --   - Computing the interior I(r) of an expansion
            --   - Determining which nodes are "instantiable" vs "locked"
            --
            --   See Note [Binding Tree] in MLF.Binding for details.
        , cVarBounds :: VarBounds
            -- ^ Variable instance bounds (paper: ∀(α ⩾ τ). …).
            --
            -- This replaces using `GNode.gBinds` as the bound store. Missing keys
            -- are treated as ⊥ (`Nothing`).
        , cEliminatedVars :: EliminatedVars
            -- ^ Variables eliminated during presolution/ω execution.
            --
            -- Elaboration ignores eliminated vars when reifying quantifiers.
        }
    deriving (Eq, Show)

data Expansion
-- | A presolution “expansion recipe” assigned to an `ExpVarId`.
--
-- These recipes are the bridge between the graphic-constraint world and xMLF:
-- they encode the *shape* of the instantiation we need (identity / ∀-intro /
-- elimination+substitution / composition). See `papers/xmlf.txt` Figures 1–3
-- for the instantiation grammar and §3.4 + Figure 10 for witness translation.
    = ExpIdentity                    -- ^ Do nothing; reuse the underlying body.
    | ExpForall (NonEmpty GNodeId)
        -- ^ Introduce one or more ∀ levels around the body.
        -- Intended correspondence: repeated quantifier-introduction (`O`) / `InstIntro`.
    | ExpInstantiate [NodeId]
        -- ^ Eliminate outer quantifiers and substitute their binders.
        --
        -- The payload is a list of *NodeIds* (not yet reified types) that will be
        -- substituted for the binders, left-to-right. The arity is checked against
        -- the binders present at the quantified level(s).
        -- Intended correspondence: type application sugar ⟨τ⟩ (paper) which expands
        -- to (∀(⩾ τ); N) on types.
    | ExpCompose (NonEmpty Expansion)
        -- ^ Sequential composition of steps.
        -- Intended correspondence: `φ; φ′`.
    deriving (Eq, Show)

-- | Atomic instance operations used in instantiation witnesses.
--
-- In `papers/xmlf.txt` §3.4, solved instantiation edges come with a *normalized*
-- witness Ω (a sequence of operations such as graft/merge/raise/weaken).
-- Figure 10 then translates these operations to an xMLF instantiation (Φ),
-- possibly preceded by a quantifier-reordering instantiation Σ(g).
--
-- We store the operations on `NodeId`s of our constraint graph.
--
-- We store “graft σ at n” as `(OpGraft sigmaRoot n)` where `sigmaRoot` is the
-- root `NodeId` of a (possibly shared) type subgraph in the constraint.
data InstanceOp
    = OpGraft NodeId NodeId
    | OpMerge NodeId NodeId
    -- | Paper Raise(n): raising a binding edge for an interior node.
    --
    -- `papers/xmlf.txt` translates this operation to an instantiation that
    -- introduces a fresh quantifier one level higher and aliases/eliminates the
    -- original binder (Fig. 10, §3.4). The current presolution does not emit
    -- arbitrary interior Raise ops yet; it currently records Raise steps for
    -- binders during instantiation-edge solving via binding-edge harmonization.
    -- See
    -- `merge_raise_merge_plan.txt` for alignment details.
    | OpRaise NodeId
    | OpWeaken NodeId
    | OpRaiseMerge NodeId NodeId
    deriving (Eq, Show)

-- | A (normalized) instance-operation witness: a sequence of atomic operations.
--
-- Paper mapping (`papers/xmlf.txt` §3.4): this corresponds to an instantiation
-- witness Ω. The paper assumes witnesses can be normalized (Yakobowski 2008)
-- before translating them with Φ (Figure 10). Our presolution records witnesses
-- in a form intended to be suitable for that translation.
newtype InstanceWitness = InstanceWitness { getInstanceOps :: [InstanceOp] }
    deriving (Eq, Show)

-- | Per-instantiation-edge witness metadata.
--
-- This is the data that Phase 6 uses to reconstruct an xMLF instantiation for
-- an application site.
--
-- Paper mapping (`papers/xmlf.txt` §3.4):
--   - `ewWitness` corresponds to a (normalized) witness Ω.
--   - `ewRoot` corresponds to the expansion root `r` in χₑ.
--   - `ewLeft`/`ewRight` are the endpoints of the original instantiation edge.
--   - The paper’s Φ(e) is Σ(g); Φχe(Ω). Our elaborator computes the Σ(g) part
--     using scheme information (binder order), and then translates Ω.
data EdgeWitness = EdgeWitness
    { ewEdgeId :: EdgeId
    , ewLeft :: NodeId
    , ewRight :: NodeId
    , ewRoot :: NodeId
    , ewForallIntros :: Int
        -- ^ Number of xMLF quantifier-introduction steps (`O`) that come from the
        -- presolution expansion recipe for this edge (e.g. `ExpForall`).
        --
        -- Paper alignment: `papers/xmlf.txt` does not include `O` in the witness
        -- language Ω (Figure 10). We therefore keep Ω as `InstanceOp`s only and
        -- apply these `O` steps directly when constructing Φ(e).
    , ewWitness :: InstanceWitness
    }
    deriving (Eq, Show)

-- | A presolution assignment map.
--
-- This maps each expansion variable to its corresponding expansion recipe.
--
-- Paper terminology: a presolution is a solved instance of a constraint (χᵖ)
-- where instantiation edges are solved by choosing expansions and performing
-- the required unifications. In this codebase, the solved *graph* lives in the
-- rewritten `Constraint`; this `Presolution` value is the persistent “recipe
-- assignment” slice that later phases (notably elaboration) may consult.
newtype Presolution = Presolution { getAssignments :: IntMap Expansion }
    deriving (Eq, Show)

-- | A convenience bundle for the “full pipeline” state.
--
-- This is not the only way to run phases, but it is a useful snapshot for
-- debugging and for pipeline-style entry points.
data SolverState = SolverState
    { -- | The current constraint graph.
      ssConstraint :: Constraint
    , -- | The presolution assignment map.
      ssPresolution :: Presolution
    , -- | The union-find map.
      ssUnionFind :: IntMap NodeId
    , -- | The dependency graph.
      ssDepGraph :: DepGraph EdgeId
    }
    deriving (Eq, Show)

-- | A directed dependency graph.
--
-- Used for ordering instantiation edges prior to presolution.
data DepGraph a = DepGraph
    { -- | The vertices of the graph.
      dgVertices :: [a]
    , -- | The edges of the graph.
      dgEdges :: IntMap [a]
    }
    deriving (Eq, Show)
