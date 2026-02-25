# Thesis-Exact Scheme Construction Design

## Goal

Eliminate the DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP deviation by including
solved-away binders in scheme types. Remove the LegacyBackend entirely,
leaving EquivBackend as the sole `Solved` representation. Remove all
escape hatches (`unionFind`, `toSolveResult`, `solvedConstraint`).

This completes the equivalence-class abstraction layer: the solver
produces thesis-aligned output where node identity is fully preserved.

## Background

### The deviation

When a binder α is unified with a concrete type (e.g., Int) during
constraint solving, the binder disappears from the solved constraint
graph. The scheme type becomes `Int → Int` instead of `∀α. α → α`.
When the witness contains OpWeaken α, Omega can't find α in the VSpine
and skips it (no-op). The thesis expects InstElim.

### Root cause

The drop happens in the generalization pipeline:

```
orderedBinders (Binding/Tree.hs:235)
  → queries cBindParents from SOLVED constraint
  → solved-away binders have no binding-parent entry → DROPPED
  → BinderPlan has only surviving binders
  → scheme type missing quantifiers for solved-away binders
  → VSpine has no slot → OpWeaken skips
```

### Why the fix is upstream

The thesis treats the scheme as the *original* type (pre-solving). The
witness operations describe how to transform it to the solved type.
Fixing Omega.hs is impossible — the VSpine is positionally indexed to
the scheme type, and the scheme doesn't have the quantifier. The fix
must ensure the scheme includes all original quantifiers.

## Approach: Hybrid Original/Solved Reification

### Key insight

The EquivBackend stores both the original constraint (`ebOriginalConstraint`)
and the solved constraint (`ebCanonicalConstraint`). The reification process
can walk the original constraint's node structure while using the solved
constraint for non-binder nodes.

### Reification rule

For each node encountered during type body reification:

1. If the node is a binder in the ordered binder list → render as `TVar name`
   (use original node identity, don't canonicalize)
2. If the node is NOT a binder → canonicalize and render from solved constraint

Example: `∀α. α → Int` where α (NodeId 5) solved to Int (NodeId 10):

- Original constraint: `Arrow(NodeId 5, NodeId 10)`
- Walk original: child 1 = NodeId 5 (binder) → `TVar "a"`
- Walk original: child 2 = NodeId 10 (not binder) → canonicalize → `TBase "Int"`
- Result: `∀a. a → Int` ✓

### BinderPlan preservation

The BinderPlan is built during presolution (before solving) and already
contains ALL binders, including ones that will be solved away. During
elaboration, `applyGeneralizePlan` currently canonicalizes binder IDs,
mapping solved-away binders to non-binder canonical representatives.

Fix: when a BinderPlan binder ID canonicalizes to a non-binder, detect
this via `wasOriginalBinder` and keep the original ID for reification.

### finalizeScheme interaction

`finalizeScheme` filters binders by usage in the type body. Since the
body is reified from the original constraint, solved-away binder
variables appear in the body. `finalizeScheme` naturally keeps them.
No change needed.

## Component Changes

### 1. Reify/Core.hs — dual-constraint reification

The reification functions need access to both constraints. Add a
`ReifyMode` or extend the existing reification environment:

```haskell
data ReifySource = ReifySource
    { rsCanonicalConstraint :: Constraint  -- solved view (for non-binder nodes)
    , rsOriginalConstraint  :: Constraint  -- original view (for binder nodes)
    , rsCanonical           :: NodeId -> NodeId
    , rsSolvedAwayBinders   :: IntSet      -- binder IDs that were solved away
    }
```

When walking the node graph:
- Use `rsOriginalConstraint` to get the node structure (children)
- For each child, check `rsSolvedAwayBinders`: if member, render as TVar
- Otherwise, canonicalize and render from `rsCanonicalConstraint`

When EquivBackend is not active (LegacyBackend during transition),
`rsOriginalConstraint == rsCanonicalConstraint` and
`rsSolvedAwayBinders == empty`, preserving current behavior.

### 2. Generalize.hs — detect solved-away binders

In `applyGeneralizePlan`, after receiving `bpOrderedBinderIds`:

```haskell
-- For each binder ID from the BinderPlan:
let binderIdC = canonical binderId
    isSolvedAway = wasOriginalBinder solved binderId
                   && not (isTyForall (lookupNode solved binderIdC))
```

Solved-away binders use their original ID (not canonical) for:
- Name assignment (from original node)
- Bound reification (from original constraint)
- Type body reification (original node structure)

### 3. Solved.hs — new query

```haskell
-- | Binder IDs from the original constraint that were solved away.
-- Returns empty for LegacyBackend.
solvedAwayBinders :: Solved -> IntSet -> IntSet
solvedAwayBinders solved binderPlanIds =
    IntSet.filter isSolvedAway binderPlanIds
  where
    isSolvedAway nid =
        wasOriginalBinder solved (NodeId nid)
        && canonical solved (NodeId nid) /= NodeId nid
```

### 4. Omega.hs — no changes

The VSpine is built from the scheme type via `mkVSpine`. Since the
scheme now includes solved-away quantifiers, the VSpine naturally has
slots for them. OpWeaken finds the binder and emits InstElim. The
deviation disappears without touching Omega.

### 5. Binding/Tree.hs — no changes

`orderedBinders` is not called during elaboration for scheme
construction. The BinderPlan (from presolution) provides the binder
list. No change needed.

## LegacyBackend Removal

### Why now

The EquivBackend is the runtime backend (wired in Milestone 4).
LegacyBackend exists only for:
- `fromSolveResult` / `mkSolved` (test helpers, some production code)
- `toSolveResult` escape hatch

With thesis-exact scheme construction requiring the original constraint,
LegacyBackend cannot support the new behavior (it has no original
constraint). Keeping it means maintaining two code paths with different
semantics. Removing it simplifies the codebase and enforces the
thesis-aligned path.

### Migration path

1. `fromSolveResult` → remove or rewrite to produce EquivBackend
   (requires a pre-rewrite snapshot, which `solveUnifyWithSnapshot`
   already provides)
2. `mkSolved` → replace with `fromPreRewriteState` or a new
   constructor that takes both constraints
3. `toSolveResult` → remove (consumers use Solved API)
4. `unionFind` → remove (consumers use `canonical`)
5. `solvedConstraint` → remove (consumers use specific queries)
6. Test code using `mkSolved emptyConstraint uf` → provide a test
   helper that constructs EquivBackend from minimal inputs

### Escape hatch removal

| Escape hatch | Current consumers | Migration |
|---|---|---|
| `unionFind` | Canonicalizer, Generalize, Pipeline, Plan/Context, Fallback, tests | Use `Solved.canonical` or add targeted queries |
| `toSolveResult` | Generalize test helper | Remove; tests use Solved API |
| `solvedConstraint` | Omega, Translate (via Binding.orderedBinders) | Add `Solved.orderedBinders` or pass Solved to Binding |

## Deviation Removal

Once solved-away binders appear in scheme types:
- OpWeaken finds them in the VSpine → emits InstElim
- The deviation DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP no longer applies
- Remove from `docs/thesis-deviations.yaml`
- Update `docs/thesis-claims.yaml` if any claims reference it

## Milestones

### M5a: Thesis-exact scheme construction (EquivBackend only)

1. Add `solvedAwayBinders` query to Solved.hs
2. Extend reification to accept dual-constraint source
3. Update `applyGeneralizePlan` to detect solved-away binders
4. Reify solved-away binder bounds from original constraint
5. Reify type body using hybrid original/solved approach
6. Verify: scheme types include solved-away quantifiers
7. Verify: OpWeaken emits InstElim (no Omega changes)
8. Update test expected outputs

### M5b: Remove LegacyBackend

1. Remove `LegacyBackend` constructor from `SolvedBackend`
2. Remove `fromSolveResult` (or rewrite to use snapshot)
3. Remove `mkSolved` (or rewrite)
4. Remove `toSolveResult`
5. Simplify all Solved API functions (no pattern match on backend)
6. Update all test helpers to construct EquivBackend
7. Verify: all tests pass with EquivBackend only

### M5c: Remove escape hatches

1. Migrate `unionFind` consumers to Solved queries
2. Remove `unionFind` export
3. Remove `solvedConstraint` export
4. Clean up Solved.hs API surface
5. Verify: no consumer accesses Solved internals

### M5d: Update documentation

1. Remove DEV-PHI-WEAKEN-SOLVED-BINDER-SKIP from thesis-deviations.yaml
2. Audit for other deviations resolved by node identity preservation
3. Update thesis-claims.yaml

## Risk Register

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Reification produces wrong type when canonical appears for non-unification reasons | Medium | High | Hybrid approach: only use original constraint for binder nodes; comparison tests |
| BinderPlan doesn't have solved-away binders | Low | High | Verify before implementing; fall back to augmenting BinderPlan from EquivBackend |
| Test suite breakage from changed scheme types | High (expected) | Medium | Intentional behavior change; update expected outputs systematically |
| `finalizeScheme` filters solved-away binders | Low | Medium | Body references binder variables; verify with test |
| LegacyBackend removal breaks test infrastructure | Medium | Medium | Provide EquivBackend test helpers first |
| Performance regression from dual-constraint reification | Low | Low | Original constraint already in memory; no extra allocation |

## Rollback Strategy

- **M5a**: Revert scheme construction changes; deviation returns
- **M5b**: Re-add LegacyBackend; `fromSolveResult` still works
- **M5c**: Re-export escape hatches
- Each milestone is independently revertible.
