# Phase Notes

This document describes the invariants and properties of each phase in the MLF type inference pipeline.

## Overview

The pipeline consists of six phases:

1. **ConstraintGen** — Build constraint graph from surface syntax
2. **Normalize** — Local rewrites (grafting + merge/unify)
3. **Acyclicity** — Derive dependency order for instantiation edges
4. **Presolution** — Choose minimal expansions, record witnesses
5. **Solve** — Discharge remaining unifications via union-find
6. **Elab** — Reify solved graph and witnesses into xMLF terms/types

---

## Phase 1: Constraint Generation

**Module**: `MLF.Frontend.ConstraintGen`

### Purpose
Transform eMLF surface syntax (`Expr`) into an initial constraint graph (`Constraint`).

### Invariants
- Every type variable occurrence is wrapped in a `TyExp` node
- Instantiation edges (`≤`) connect polymorphic bindings to use sites
- Binding parents are established for all non-root nodes
- Gen nodes are created for let-binding sites

### Key Types
- `Expr` — surface syntax
- `Constraint` — initial constraint graph

### Tests
- `test/ConstraintGenSpec.hs` — constraint generation correctness

---

## Phase 2: Normalize

**Module**: `MLF.Constraint.Normalize`

### Purpose
Perform local rewrites to simplify the constraint graph before presolution.

### Invariants
- Grafting: When a `TyVar` unifies with structure, the variable node is updated
- Binding-edge Raise harmonization before unioning
- No `TyExp` nodes are eliminated (that's Phase 4's job)

### Key Functions
- `normalize` — main entry point
- `applyUnionFindToConstraint` — grafting logic

### Strategy
Uses `MLF.Constraint.Unify.Core` with a `UnifyStrategy` for phase-specific behavior.

### Tests
- `test/NormalizeSpec.hs` — normalization properties

---

## Phase 3: Acyclicity

**Module**: `MLF.Constraint.Acyclicity`

### Purpose
Check that instantiation edges form an acyclic graph and derive a dependency order.

### Invariants
- If cycles exist, presolution cannot proceed
- Edge order respects dependency direction (prerequisites before dependents)

### Key Functions
- `checkAcyclicity` — verify acyclicity and return processing order

### Tests
- `test/AcyclicitySpec.hs` — acyclicity checking

---

## Phase 4: Presolution

**Module**: `MLF.Constraint.Presolution`

### Purpose
Compute minimal expansions for each `TyExp` node and record per-edge witnesses.

### Invariants
- Each expansion variable gets a minimal `Expansion` recipe
- Per-edge witnesses (`EdgeWitness`) record Ω operations
- Witness steps are normalized (Raise+Merge coalesced, Weaken-last ordering)
- `TyExp` nodes are eliminated after materialization

### Key Types
- `Expansion` — expansion recipe (Identity, Forall, Instantiate, Compose)
- `EdgeWitness` — per-edge witness with Ω steps
- `EdgeTrace` — copy map and interior for witness translation

### Key Functions
- `computePresolution` — main entry point
- `processInstEdge` — process single instantiation edge
- `materializeExpansions` — eliminate `TyExp` nodes

### Submodules
- `EdgeProcessing` — edge-local logic with `EdgeCtx`
- `EdgeUnify` — edge-local unification
- `WitnessNorm` — witness normalization
- `Materialization` — expansion materialization
- `ForallIntro` — ∀-introduction handling

### Tests
- `test/PresolutionSpec.hs` — presolution correctness

---

## Phase 5: Solve

**Module**: `MLF.Constraint.Solve`

### Purpose
Discharge remaining unification edges after presolution has eliminated `TyExp`.

### Invariants
- No `TyExp` nodes remain (verified, survivors cause errors)
- Eliminated binders are rewritten to bounds or `TyBottom`
- Union-find map is extended with elimination substitution
- Binding-edge Raise harmonization before unioning

### Key Functions
- `solveUnify` — main entry point
- Uses `MLF.Constraint.Unify.Core` with Solve-specific strategy

### Tests
- `test/SolveSpec.hs` — solver correctness

---

## Phase 6: Elaboration

**Module**: `MLF.Elab.Pipeline`

### Purpose
Reify the solved constraint graph and witnesses into xMLF terms and types.

### Invariants
- Types are reified from the binding tree (not free-variable fallback)
- Variable names in terms match their type schemes (via `substInTerm`)
- Witnesses translate to valid xMLF instantiations
- Quantifier reordering uses adjacent swaps (Σ)

### Key Types
- `ElabType` — xMLF type
- `ElabTerm` — xMLF term
- `Instantiation` — xMLF instantiation witness

### Key Functions
- `elaborate` / `runPipelineElab` — main entry points
- `generalizeAt` — generalization at a gen node
- `phiFromEdgeWitness` — translate edge witness to instantiation
- `sigmaReorder` — quantifier reordering

### Submodules
- `Generalize` — generalization orchestration
- `Elaborate` — term elaboration
- `TypeCheck` — xMLF type checking
- `Reduce` — xMLF reduction

### Tests
- `test/ElaborationSpec.hs` — elaboration correctness
- `test/TypeCheckSpec.hs` — xMLF type checking
- `test/ReduceSpec.hs` — xMLF reduction

---

## Cross-Cutting Concerns

### Binding Tree

All phases rely on the binding tree (`cBindParents`) for scope tracking.

**Modules**: `MLF.Binding.Tree`, `MLF.Binding.Queries`, `MLF.Binding.Adjustment`

### Union-Find

Phases 2, 4, and 5 use union-find for unification.

**Module**: `MLF.Util.UnionFind`

### Shared Unification Core

Phases 2, 4, and 5 share structural unification logic via `MLF.Constraint.Unify.Core`.

**Module**: `MLF.Constraint.Unify.Core`

### Configuration

- **Normalize strategy**: `normalizeUnifyStrategy` in `Normalize`
- **Solve strategy**: `solveUnifyStrategy` in `Solve`
- **Elab config**: `ElabConfig` / `ElabEnv` records in `Elaborate`
