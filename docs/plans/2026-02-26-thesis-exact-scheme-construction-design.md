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

## Codebase Audit Findings

### Two kinds of lookup

The thesis distinguishes two operations that our `lookupNode` conflates:

1. "What is this node's original structure?" → look up in original graph
2. "What is this node equivalent to?" → use canonical function

Our current `lookupNode` canonicalizes first, then looks up in the
solved constraint. This always returns the canonical representative's
post-solve node. The thesis model needs both operations available.

### Canonical-link problem in reification

`reifyType` accepts a constraint AND a canonical function. When given
the original constraint with the standard canonical function:

1. It encounters `∀α. α → α`
2. It follows `canonical(α) = Int` when rendering the body
3. It renders the body as `Int → Int`, not `α → α`
4. Binder α appears unused in the body
5. `finalizeScheme` filters it out

The thesis model says: reification for scheme construction must
preserve original structure. The canonical function is for equivalence
checking ("are these two nodes the same?"), not for collapsing binders
during rendering. Scheme reification needs `id` as its canonical
function, or a canonical function that only resolves non-binder
equivalences.

### Solved.hs API consumer audit

| API | Call sites | Needs canonical view? | Breakage risk |
|---|---|---|---|
| `lookupNode` | 14 (Omega, Elaborate) | Callers canonicalize first, then check node type. Works with original constraint since canonical reps exist in original. | LOW-MEDIUM |
| `lookupBindParent` | 8 (Omega) | Callers walk binding tree, check BindRigid flags. Original tree has all entries but flags may differ. | MEDIUM |
| `bindParents` | 2 (Omega) | Iterates full map to build child maps. Original tree has more entries (pre-merge). | MEDIUM |
| `lookupVarBound` | 8 (Omega, Elaborate) | Callers reify types from bounds. Original bounds are pre-solve (e.g., `⊥`). Thesis says this is correct — witness ops communicate solved values. | MEDIUM-HIGH |
| `allNodes` | 0 | Unused; callers use `solvedConstraint` escape hatch | N/A |
| `instEdges` | 0 | Unused; callers use escape hatch | N/A |
| `genNodes` | 0 | Unused; callers use escape hatch | N/A |
| `solvedConstraint` | 30+ | Main escape hatch. Each caller needs individual analysis. | CRITICAL |
| `unionFind` | 10 | Canonicalizer construction, test utilities | MEDIUM |
| `canonical` | 40+ | Independent of constraint view | NONE |

### `solvedConstraint` escape hatch breakdown

The 30+ callers of `solvedConstraint` are the real migration surface:

- Omega.hs (9 uses): `Binding.orderedBinders`, `NodeAccess` queries,
  `checkBindingTree`, order key computation
- Translate.hs (4 uses): `Binding.orderedBinders`, `namedNodes`,
  `reifyType`, `checkBindingTree`
- Pipeline.hs (2 uses): canonicalizer construction, constraint setup
- Generalize.hs (1 use): constraint reconstruction
- ResultType/Ann.hs (2 uses): result type computation
- ResultType/Fallback.hs (1 use): constraint reconstruction
- Plan/Context.hs (1 use): constraint reconstruction
- Order.hs (2 uses): order key computation
- Reify/Core.hs (1 use): reification setup
- Tests (10+ uses): various test utilities

### Second deviation may be resolved

DEV-PHI-KEEP-BINDER-WEAKEN-SUPPRESSION (Omega suppresses InstElim
for selected OpWeaken steps when binder is in `keepBinderKeys`) is a
workaround for lost binder identity. With original constraint as
primary, this may no longer be needed. Audit in M5d.

## Component Changes

### 1. Solved.hs — dual-view API

Rather than switching ALL queries to original constraint (which breaks
callers that need canonical-view semantics), provide both views:

```haskell
-- | The original (pre-solving) constraint. Primary for scheme
-- construction and thesis-exact operations.
originalConstraint :: Solved -> Constraint

-- | Look up a node in the original constraint by raw ID.
-- Does NOT canonicalize. Returns the pre-solve node structure.
originalNode :: Solved -> NodeId -> Maybe TyNode  -- already exists

-- | Look up a node, canonicalizing first. Returns the canonical
-- representative's node from the original constraint.
-- All nodes exist in the original constraint, so this is safe.
lookupNode :: Solved -> NodeId -> Maybe TyNode  -- keep canonicalizing
```

The key change: `lookupNode` still canonicalizes, but looks up in
`ebOriginalConstraint` instead of `ebCanonicalConstraint`. Since
solving via union-find doesn't create new nodes, all canonical
representatives exist in the original constraint.

Similarly for `lookupBindParent`, `bindParents`, `allNodes`: switch
to original constraint. The `canonical` function remains unchanged.

`lookupVarBound` switches to original constraint. Callers get
pre-solve bounds (e.g., `⊥` for unbounded). This matches the thesis:
witness operations communicate the solved value, not the bound.

### 2. Reify/Core.hs — identity canonical for scheme reification

The critical change. When reifying types for scheme construction,
pass `id` as the canonical function instead of `Solved.canonical`:

```haskell
-- Scheme reification: preserve original structure
reifyType originalConstraint id rootNode

-- Phi/omega reification: follow canonical links (unchanged)
reifyType originalConstraint (Solved.canonical solved) rootNode
```

With `id` as canonical, the reifier preserves `∀α. α → α` instead
of collapsing to `Int → Int`. Binder variables appear in the body.
`finalizeScheme` keeps them.

### 3. Generalize.hs — pass original constraint + identity canonical

Change the constraint passed to the reification environment from
`solvedConstraint solved` to `originalConstraint solved`, and pass
`id` as the canonical function for scheme type reification.

The binder list from BinderPlan already has all binders (presolution
builds it before solving). No filtering or augmentation needed.

Bound reification also uses the original constraint with `id`:
a solved-away binder's bound is its original bound (e.g., `⊥`).
The witness operations communicate the solved value.

### 4. Phi translation (Omega.hs, Translate.hs) — transparent

Since `Solved.lookupNode` and `Solved.lookupBindParent` are migrated
at the Solved.hs level (change 1), phi translation code needs no
source-level changes. Callers still canonicalize before lookup, and
the original constraint has all nodes.

The 30+ `solvedConstraint` escape hatch callers in Omega/Translate
migrate to `originalConstraint` or specific Solved queries as part
of M5a (escape hatch removal).

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
the body is reified from the original constraint with `id` canonical,
all binder variables appear in the body. `finalizeScheme` keeps them.

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

### R2: Reifier follows canonical links, drops solved-away binders (M5c)

This is the critical risk. `reifyType` uses a canonical function to
resolve equivalences during rendering. If it follows `canonical(α) =
Int`, the body becomes `Int → Int` and `finalizeScheme` drops α.

Mitigation: Pass `id` as the canonical function for scheme
reification. The reifier preserves original structure (`α → α`).
Phi/omega reification continues using `Solved.canonical`. This is
a targeted change in Generalize.hs, not a global reifier change.

### R3: lookupNode semantics change breaks phi translation (M5b)

Phi translation calls `Solved.lookupNode` expecting the canonical
(post-solve) view. Switching to original constraint means lookups
return the canonical representative's original node. Since solving
doesn't create new nodes, all canonical reps exist in the original
constraint, so lookups succeed. But node structure may differ if
`applyUFConstraint` modified nodes beyond merging.

Mitigation: M5b runs the full test suite after each query migration.
If phi translation breaks, add `lookupCanonicalNode` that preserves
the old behavior for specific callers.

### R4: lookupVarBound returns pre-solve bounds (M5b)

8 callers use `lookupVarBound` to reify types from bounds. Original
bounds are pre-solve (e.g., `⊥` for unbounded). The thesis says this
is correct — witness operations communicate the solved value. But
callers may have implicit assumptions about post-solve bounds.

Mitigation: Incremental migration with full test suite after each
change. If bound reification breaks, provide `lookupSolvedVarBound`
that preserves canonical-view semantics.

### R5: Binding tree queries return stale parents (M5b)

The original constraint's binding tree may have entries that were
invalidated by solving (e.g., a binder whose parent was merged).
Callers that walk the binding tree may hit unexpected structure.

Mitigation: The canonical function resolves equivalences. Callers
that need "who is the effective parent" should canonicalize the
parent after looking it up. This is the thesis model.

### R6: solvedConstraint escape hatch has 30+ callers (M5a)

The main escape hatch feeds all downstream queries. Each caller
needs individual analysis to determine whether it needs original
or canonical constraint semantics.

Mitigation: Categorize callers before migration:
- Binding tree queries → use `originalConstraint`
- Node lookups → use `Solved.lookupNode` (migrated to original)
- Order key computation → may need canonical constraint
- Constraint reconstruction → remove pattern (use Solved API)
Provide `canonicalConstraint` escape hatch during migration if
needed, remove after all callers are migrated.
