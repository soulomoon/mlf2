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
    Expansion (..),
    Presolution (..),
    SolverState (..),
    DepGraph (..)
) where

import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)

newtype NodeId = NodeId { getNodeId :: Int }
    deriving (Eq, Ord, Show)

newtype GNodeId = GNodeId { getGNodeId :: Int }
    deriving (Eq, Ord, Show)

newtype ExpVarId = ExpVarId { getExpVarId :: Int }
    deriving (Eq, Ord, Show)

newtype EdgeId = EdgeId { getEdgeId :: Int }
    deriving (Eq, Ord, Show)

newtype BaseTy = BaseTy { getBaseName :: String }
    deriving (Eq, Ord, Show)

-- | Graphic type node as described in the roadmap; stored as a DAG node and
-- referenced by its stable 'NodeId'.
data TyNode
    = TyVar
        { tnId :: NodeId
        , tnLevel :: GNodeId
            -- ^ Binding level that owns this variable; see Note [G-nodes].
        }
    | TyArrow
        { tnId :: NodeId
        , tnDom :: NodeId
        , tnCod :: NodeId
        }
    | TyBase
        { tnId :: NodeId
        , tnBase :: BaseTy
        }
    | TyForall
        { tnId :: NodeId
        , tnQuantLevel :: GNodeId
        , tnBody :: NodeId
        }
    -- | Expansion node created for let-bindings; see Note [Expansion nodes].
    | TyExp
        { tnId :: NodeId
        , tnExpVar :: ExpVarId
        , tnBody :: NodeId
        }
    deriving (Eq, Show)

{- Note [Expansion nodes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Paper link (Rémy & Yakobowski, ICFP 2008, §3–§5): Expansion variables mediate
between a let-bound scheme and its uses. We mirror that mechanism explicitly:

Shape
-----
* 'TyExp' carries (s, τ) where s is an expansion variable and τ is the graphic
    type of the binding's RHS.
* 'Expansion' (Phase 4 output) captures the paper's lattice: identity,
    instantiation with fresh metas, ∀-introduction (possibly multiple levels),
    and explicit composition of steps. Keeping s ↦ Expansion separate from τ lets
    us remember the original scheme while still applying the minimal recipe per
    use site.

Flow across phases
------------------
* Phase 1 (constraint gen): for each application of a let-bound value, emit an
    instantiation edge  s · τ ≤ σ_use  where the LHS node is the 'TyExp'.
* Phase 3 (acyclicity): orders these edges so presolution can process them
    without dependency cycles.
* Phase 4 (presolution): decides and *materializes* the minimal expansion for s
    using the lattice above (identity / instantiate / add ∀ / compose). It may
    allocate fresh nodes for bound vars, wrap ∀ at target levels, and emit
    follow-up unifications so that E(τ) matches σ_use. Sharing is preserved:
    multiple uses of the same binding point to the same (s, τ) but can have
    distinct expansions.
* Phase 5 (solve): assumes TyExp nodes are gone; if any survive, they are
    rejected as `UnexpectedExpNode` while final unifications are discharged.

Why explicit TyExp + Expansion
------------------------------
* Distinguishes separate instantiations of the same let-binding without copying
    τ eagerly.
* Allows faithful implementation of the paper's minimal-expansion lattice and
    its composition rules (e.g., instantiate then re-generalize).
* Keeps scope information intact: instantiation only allocates fresh nodes for
    τ's bound vars; free/shared nodes remain shared, matching the paper's sharing
    discipline.
-}

-- | Generalization nodes model the levels introduced by let-generalization in
-- the graphic constraint. Each level forms a tree (parent/children) so that we
-- know which type variables may be quantified at a given scope and where that
-- scope sits relative to its ancestors.
--
-- * 'gnodeId' is the stable identifier for this level.
-- * 'gParent' points to the enclosing level (Nothing for the root g₀).
-- * 'gBinds' lists the 'TyVar' node ids whose binding level is this node; they
--   are the candidates for ∀-introduction when solving.
-- * 'gChildren' enumerates nested levels created by inner let-bindings, giving
--   the solver the forest structure it needs for scope checks.
--   See Note [G-nodes].
data GNode = GNode
    { gnodeId :: GNodeId
    , gParent :: Maybe GNodeId
    , gBinds :: [NodeId]
    , gChildren :: [GNodeId]
    }
    deriving (Eq, Show)

{- Note [G-nodes]
~~~~~~~~~~~~~~~~~
Rémy–Yakobowski levels form a tree where each let-binding introduces a child
scope. We encode that hierarchy explicitly so later phases can answer:

    * Which variables are allowed to generalize at this scope?  → 'gBinds'.
    * Does this scope sit under another generalization level?     → 'gParent'.
    * Which nested lets did this binding introduce?               → 'gChildren'.

During Phase 1 every TyVar records the G-node that owns it via 'tnLevel'. When
Phase 4 decides whether a variable may be quantified it consults 'gBinds' for
the child level introduced by a let, walks up the parent chain ('gParent') to
check that all uses stay within scope, and then records the chosen variables in
the presolution for that binding. This is how we decide which TyVars become
∀-bound in the let-generalized scheme. Keeping an explicit forest also lets us
describe programs with multiple roots (e.g. module initializers) without
imposing an artificial single g₀, and it ensures each 'TyVar' remembers exactly
which generalization level created it.

Note [G-node Push/Pull Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The paper (§4) describes a "push/pull G-nodes" transformation that ensures
G-nodes appear only at the top of types, not nested inside type constructors.
In our implementation, this invariant is enforced structurally by the data
types themselves:

  1. G-nodes live in a separate forest: 'cGNodes :: IntMap GNode'
  2. Type nodes only *reference* G-nodes via 'tnLevel :: GNodeId'
  3. Type constructors like 'TyArrow' contain 'NodeId' fields, not 'GNode's

This means it's impossible to construct a type like "(∀α.α) → Int" where the
quantifier is nested inside the arrow. The separation between the G-node forest
and the type graph makes the push/pull transformation unnecessary — we get the
invariant "for free" from the Haskell type system.
-}

-- | Instantiation edge (Tₗ ≤ Tᵣ) connecting a polymorphic binding to the
-- type shape it must instantiate to; Phase 4 consumes these edges when
-- computing minimal expansions.
data InstEdge = InstEdge
    { instEdgeId :: EdgeId
    , instLeft :: NodeId
    , instRight :: NodeId
    }
    deriving (Eq, Show)

{- Note [Unification Edges]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unification edges (T₁ = T₂) represent equality constraints between type nodes.
They arise from two sources:

  1. Direct generation: When the constraint generator determines that two types
     must be identical (e.g., function argument matches parameter type).

  2. Grafting output: When Phase 2 grafts structure onto a variable, the
     instantiation edge α ≤ τ is converted into unification edges:
       - α = (α₁ → α₂) where α₁, α₂ are fresh variables
       - α₁ = dom(τ), α₂ = cod(τ)
     See Note [Grafting] in MLF.Normalize.

Processing (TLDI 2007 §3 "Graphical Unification"):
  Phase 2's mergeUnifyEdges processes these via union-find:

    Var = Var       → Union the variables (one points to other)
    Var = Structure → Variable points to the structure
    Arrow = Arrow   → Union arrows, emit dom₁ = dom₂, cod₁ = cod₂
    Base = Base     → Check equality; error if different
    Arrow = Base    → Type error (constructor clash)

  The algorithm is incremental: new edges can be added during processing
  (e.g., Arrow = Arrow generates two child edges). We iterate until all
  edges are consumed or errors remain.

Unlike InstEdges which require presolution to determine *how* to instantiate,
UnifyEdges are resolved immediately by structural unification. The union-find
structure is applied to all node references at the end of each normalization
iteration, ensuring the constraint graph uses canonical representatives.

Remaining edges after normalization represent type errors (e.g., Int = Bool).
-}

-- | Unification edge (T₁ = T₂) representing a monotype equality constraint.
-- These are processed by Phase 2's merging algorithm via union-find.
-- See Note [Unification Edges].
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
            --   Phase 5 processes these to merge DAG nodes and detect inconsistencies.
        }
    deriving (Eq, Show)

data Expansion
-- | Presolution recipes the solver assigns to each expansion variable. See
-- Note [Expansion nodes] for why we keep these recipes explicit and how they
-- correspond to the operations from the paper (identity, add ∀ levels,
-- instantiate by grafting fresh metas, compose steps).
    = ExpIdentity                    -- ^ Do nothing; reuse the underlying body.
    | ExpForall (NonEmpty GNodeId)   -- ^ Introduce one or more ∀ levels around the body.
    | ExpInstantiate [NodeId]        -- ^ Substitute bound vars with fresh nodes (arity matches binders).
    | ExpCompose (NonEmpty Expansion) -- ^ Execute several steps in order (e.g., instantiate then ∀).
    deriving (Eq, Show)

newtype Presolution = Presolution { getAssignments :: IntMap Expansion }
    deriving (Eq, Show)

data SolverState = SolverState
    { ssConstraint :: Constraint
    , ssPresolution :: Presolution
    , ssUnionFind :: IntMap NodeId
    , ssDepGraph :: DepGraph EdgeId
    }
    deriving (Eq, Show)

data DepGraph a = DepGraph
    { dgVertices :: [a]
    , dgEdges :: IntMap [a]
    }
    deriving (Eq, Show)
