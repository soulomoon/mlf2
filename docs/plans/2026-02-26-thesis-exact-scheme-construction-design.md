# Thesis-Exact Scheme Construction Design

## Goal

Eliminate the DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP deviation by reifying
scheme types from the original (pre-solving) constraint. Remove the
LegacyBackend entirely, leaving EquivBackend as the sole `Solved`
representation. Remove all escape hatches.

This completes the equivalence-class abstraction layer: the solver
produces thesis-aligned output where node identity is fully preserved
and scheme types reflect the original type structure.

## Background

### The deviation

When a binder α is unified with a concrete type (e.g., Int) during
constraint solving, the binder disappears from the solved constraint
graph. The scheme type becomes `Int → Int` instead of `∀α. α → α`.
When the witness contains OpWeaken α, Omega can't find α in the VSpine
and skips it (no-op). The thesis expects InstElim.

### Root cause: solving before graph ops

The thesis does not rewrite the constraint graph before applying witness
operations. Solving produces equivalence classes (metadata about which
nodes are equal), but the graph structure is preserved. Witness
operations (OpWeaken, OpGraft, OpMerge, OpRaise) describe how to
transform the original type to the target type.

Our pipeline rewrites the graph via `applyUFConstraint`, then builds
schemes from the rewritten graph. By the time witness translation runs,
solved-away binders are gone. The fix: build schemes from the original
graph, as the thesis intends.

### The EquivBackend already has what we need

Milestone 4 built the EquivBackend which stores:
- `ebOriginalConstraint` — the pre-solving constraint (all binders intact)
- `ebCanonicalConstraint` — the post-solving constraint (for backward compat)
- `ebEquivClasses` — which nodes were unified
- `ebCanonicalMap` — original → canonical mapping

## Approach: Original Constraint as Primary, Everywhere

### Core principle

In the thesis, solving produces equivalence classes — metadata about
which nodes are equal. The constraint graph is NEVER rewritten. All
post-solve operations (elaboration, generalization, phi translation,
omega) work on the original graph structure + equivalence classes.

Our pipeline should do the same. The `ebOriginalConstraint` is the
primary constraint for ALL post-solve operations. The canonical
function (`Solved.canonical`) provides equivalence-class resolution.
The `ebCanonicalConstraint` exists only for backward compatibility
during migration and is removed with the LegacyBackend.

### What this means concretely

| Operation | Current | Thesis-exact |
|---|---|---|
| Scheme reification | `solvedConstraint` | `originalConstraint` |
| Bound reification | `solvedConstraint` | `originalConstraint` |
| Phi translation node lookups | `Solved.lookupNode` (canonical view) | `Solved.originalNode` or lookups on original constraint |
| Binding tree queries | `Solved.bindParents` (canonical) | `Solved.originalBindParent` / original binding tree |
| Omega node lookups | `Solved.lookupNode` (canonical view) | Original constraint + canonical for equivalence |
| VSpine construction | From solved scheme type | From original scheme type (all binders present) |

The canonical function is still used everywhere — it tells us which
nodes are equivalent. But the graph structure (node types, children,
binding parents) comes from the original constraint.

### How it works

1. All reification reads from `originalConstraint`
2. All binding tree queries read from `originalConstraint`
3. `Solved.canonical` resolves equivalence classes (unchanged)
4. Binder list from BinderPlan has all binders (presolution, pre-solve)
5. Scheme types include all original quantifiers
6. VSpine has slots for all binders
7. Witness operations (OpWeaken, OpGraft, etc.) find their targets
8. No deviations from thesis semantics

### Verification cases

| Original type | Solving | Scheme (thesis-exact) | Witness |
|---|---|---|---|
| `∀α. α → α` | α = Int | `∀α. α → α` | OpGraft α Int, OpWeaken α |
| `∀α. α → Int` | α = Int | `∀α. α → Int` | OpGraft α Int, OpWeaken α |
| `∀α. ∀β. α → β` | α = β = Int | `∀α. ∀β. α → β` | OpMerge α β, OpGraft α Int, OpWeaken α |
| `∀α. α → γ` | α = Int, γ free | `∀α. α → γ` | OpGraft α Int, OpWeaken α |

## Component Changes

### 1. Solved.hs — original constraint becomes primary

Migrate core queries to read from the original constraint:

```haskell
-- | The original (pre-solving) constraint. Primary constraint for
-- all post-solve operations (thesis-exact).
originalConstraint :: Solved -> Constraint
originalConstraint (Solved EquivBackend { ebOriginalConstraint = c }) = c
```

Existing queries change their source:
- `lookupNode` → look up in original constraint (not canonical)
- `lookupBindParent` → read from original binding tree
- `bindParents` → return original binding parents
- `allNodes` → return original nodes

The `canonical` function remains unchanged — it provides equivalence
class resolution. Consumers use `canonical` to determine which nodes
are equal, and the original constraint for graph structure.

### 2. Generalize.hs — pass original constraint to reification

Change the constraint passed to the reification environment from
`solvedConstraint solved` to `originalConstraint solved`.

The binder list from BinderPlan already has all binders (presolution
builds it before solving). No filtering or augmentation needed.

Bound reification also uses the original constraint: a solved-away
binder's bound is its original bound (e.g., `⊥` for unbounded).
The witness operations communicate the solved value.

### 3. Reify/Core.hs — reads original graph

The reification functions already accept a `Constraint` and a
`canonical` function. The only change is what constraint is passed:

- Before: `solvedConstraint solved` (post-solving, binders missing)
- After: `originalConstraint solved` (pre-solving, all binders present)

The `canonical` function is still `Solved.canonical solved` for
equivalence-class resolution during rendering.

### 4. Phi translation (Omega.hs, Translate.hs) — uses original

Node lookups in phi translation switch to original constraint.
Since `Solved.lookupNode` and `Solved.lookupBindParent` are migrated
at the Solved.hs level (change 1), phi translation code needs no
source-level changes — the API functions return original data.

### 5. Omega.hs OpWeaken — no changes

The VSpine is built from the scheme type via `mkVSpine`. Since the
scheme now includes all original quantifiers, the VSpine naturally
has slots for solved-away binders. OpWeaken finds the binder and
emits InstElim. The deviation disappears without touching Omega.

### 6. Binding/Tree.hs — no changes

`orderedBinders` is not called during elaboration for scheme
construction. The BinderPlan (from presolution) provides the binder
list.

### 7. finalizeScheme — no changes expected

`finalizeScheme` filters binders by usage in the type body. Since
the body is reified from the original constraint, all binder
variables appear in the body. `finalizeScheme` naturally keeps them.

## LegacyBackend Removal

### Why now

The EquivBackend is the runtime backend (wired in Milestone 4).
LegacyBackend exists only for:
- `fromSolveResult` / `mkSolved` (test helpers, some production code)
- `toSolveResult` escape hatch

Thesis-exact scheme construction requires the original constraint,
which LegacyBackend does not have. Keeping it means maintaining two
code paths with different semantics. Removing it simplifies the
codebase and enforces the thesis-aligned path.

### Migration path

1. `fromSolveResult` → remove or rewrite to produce EquivBackend
   (requires a pre-rewrite snapshot, which `solveUnifyWithSnapshot`
   already provides)
2. `mkSolved` → replace with `fromPreRewriteState` or a new
   constructor that takes both constraints
3. `toSolveResult` → remove (consumers use Solved API)
4. `unionFind` → remove (consumers use `canonical`)
5. `solvedConstraint` → remove (consumers use `originalConstraint`
   or specific queries)
6. Test code using `mkSolved emptyConstraint uf` → provide a test
   helper that constructs EquivBackend from minimal inputs

### Escape hatch removal

| Escape hatch | Current consumers | Migration |
|---|---|---|
| `unionFind` | Canonicalizer, Generalize, Pipeline, Plan/Context, Fallback, tests | Use `Solved.canonical` or add targeted queries |
| `toSolveResult` | Generalize test helper | Remove; tests use Solved API |
| `solvedConstraint` | Omega, Translate (via Binding.orderedBinders) | Use `originalConstraint` or pass Solved to Binding |

## Deviation Removal

Once scheme types are reified from the original constraint:
- All binders present in VSpine → OpWeaken emits InstElim
- DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP no longer applies
- Remove from `docs/thesis-deviations.yaml`
- Update `docs/thesis-claims.yaml` if any claims reference it
- Audit for other deviations resolved by original-constraint reification

## Milestones

### M5a: Remove LegacyBackend and escape hatches

Remove LegacyBackend first so all subsequent changes touch one code
path. The EquivBackend is already the runtime backend (Milestone 4).

1. Provide `mkTestSolved` helper that constructs EquivBackend from
   a constraint and union-find map (same interface as `mkSolved`)
2. Migrate all test code from `mkSolved`/`fromSolveResult` to new helpers
3. Migrate production callers of `unionFind` to `Solved.canonical`
4. Migrate production callers of `solvedConstraint` to Solved queries
5. Remove `LegacyBackend` constructor from `SolvedBackend`
6. Remove `fromSolveResult`, `mkSolved`
7. Remove `toSolveResult`, `unionFind`, `solvedConstraint` escape hatches

Test: full suite. No LegacyBackend references remain.

### M5b: Original constraint as primary in Solved.hs

With only EquivBackend remaining, migrate core queries to read from
`ebOriginalConstraint`:

1. Add `originalConstraint :: Solved -> Constraint` export
2. Switch `lookupNode` to use `ebOriginalConstraint`
   (look up by raw nid, not canonical — canonical is for equivalence only)
3. Switch `allNodes` to use `ebOriginalConstraint`
4. Switch `lookupBindParent` to use `ebOriginalConstraint`
5. Switch `bindParents` to use `ebOriginalConstraint`
6. Switch `instEdges` to use `ebOriginalConstraint`
7. Switch `genNodes` to use `ebOriginalConstraint`
8. Switch `lookupVarBound` to use `ebOriginalConstraint`

The `canonical` function remains unchanged — it provides equivalence
class resolution.

Test: full suite. Expect some failures from callers that assumed
canonical-view semantics — fix those callers.

### M5c: Original constraint in Generalize.hs and Reify

1. Change the constraint passed to the reification environment from
   `solvedConstraint solved` to `originalConstraint solved`
2. Verify BinderPlan already provides all binders (presolution)
3. Verify `finalizeScheme` keeps solved-away binders (they appear in
   the original-constraint type body)
4. Fix any test expectations that assumed solved-away binders were absent

Test: full suite. Scheme types should now include all original quantifiers.
VSpine gets slots for solved-away binders. OpWeaken finds its target.

### M5d: Remove deviation and update documentation

1. Remove DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP from `thesis-deviations.yaml`
2. Update `thesis-claims.yaml` if any claims reference the deviation
3. Update test expectations that assert the deviation behavior (e.g.,
   OpWeaken producing ε instead of InstElim)
4. Audit for other deviations resolved by original-constraint reification

## Risk Register

### R1: Test churn from LegacyBackend removal (M5a)

Many tests use `mkSolved emptyConstraint uf`. Removing LegacyBackend
requires migrating all of them to EquivBackend-based helpers.

Mitigation: Provide `mkTestSolved` that constructs a minimal
EquivBackend from a constraint and union-find map (same interface,
different backend). Migration is mechanical. Do this first (M5a)
so all subsequent work touches one code path.

### R2: lookupNode semantics change breaks phi translation (M5b)

Phi translation currently calls `Solved.lookupNode` expecting the
canonical (post-solve) view. Switching to original constraint means
lookups return the pre-solve node. If phi translation relies on
solved-away merges being visible in the node structure, it will break.

Mitigation: M5b runs the full test suite after each query migration.
If phi translation breaks, add `lookupCanonicalNode` that preserves
the old behavior for specific callers.

### R3: Binding tree queries return stale parents (M5b)

The original constraint's binding tree may have entries that were
invalidated by solving (e.g., a binder whose parent was merged).
Callers that walk the binding tree may hit unexpected structure.

Mitigation: The canonical function resolves equivalences. Callers
that need "who is the effective parent" should canonicalize the
parent after looking it up. This is the thesis model.

### R4: finalizeScheme drops solved-away binders (M5c)

If `finalizeScheme` filters binders by whether they appear as
`TForall` in the reified type, and the original constraint has
the binder as `TForall`, this should work. But if the binder was
unified with a concrete type and the reifier follows the canonical
link, the binder variable won't appear in the body.

Mitigation: The reifier must use the original constraint for
structure (binder is `TForall`) and canonical only for equivalence
resolution when rendering type bodies. Verify in M5c.
