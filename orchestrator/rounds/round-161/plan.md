# Plan — Round 161

## Roadmap Item

**item-2**: Fix BUG-2026-03-16-001 (InstBot replay mismatch)

## Current State Assessment

The `InstBot` precondition in `MLF.Elab.Inst.applyInstantiation` was already
partially fixed during earlier diagnostic rounds (R2/round-025). The
`allowReplayBoundMatch` function in `applyInstantiation` accepts non-bottom
types when the replay environment resolves `tArg` to match the bound. The
existing test "URI-R2-C1 witness replay stays alpha-equivalent to the locked
no-fallback shape" (line 1524 of `test/ElaborationSpec.hs`) passes.

However:
1. `Bugs.md` still lists BUG-2026-03-16-001 as **Open**.
2. There is no focused, minimal regression test that exercises the `InstBot`
   replay-bound-match path directly (the existing test uses the full pipeline
   fixture, which is ~70 lines of setup).
3. The `allowReplayBoundMatch` guard's `not (alphaEqType resolvedArg tArg)`
   condition is the paper-faithful safety check that prevents bare `InstBot`
   from accepting non-bottom types when no replay substitution is active.
   This is correct and should be preserved.

## Steps

### Step 1: Add a focused minimal regression test for the InstBot replay path

**File**: `test/ElaborationSpec.hs`

Add a new test in the "xMLF instantiation semantics (applyInstantiation)"
describe block that directly exercises the `allowReplayBoundMatch` path with
a minimal hand-constructed type and instantiation:

- Construct a type `∀(a ⩾ ⊥) ∀(b ⩾ a -> a) b`
- Construct an instantiation `∀(⩾ ⊲t9); N; (∀(⩾ ⊲(a -> a)); N)` that
  exercises the replay path where `a` is substituted by `t9` in the replay env
- Verify `applyInstantiation` succeeds and returns `t9 -> t9`
- Name the test: `"BUG-2026-03-16-001 regression: InstBot accepts replay-resolved bound match"`

This is a focused, direct exercise of the bug-path without the full pipeline
fixture setup.

**Verification**:
```
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-03-16-001 regression"'
```

### Step 2: Add a {- Note -} block documenting the replay-bound-match semantics

**File**: `src/MLF/Elab/Inst.hs`

Add a `{- Note [InstBot replay-bound match] -}` block near the
`allowReplayBoundMatch` function explaining:
- Why `InstBot` must accept non-⊥ types when replay variables resolve `tArg`
  to match the bound
- Why the `not (alphaEqType resolvedArg tArg)` guard is necessary (prevents
  accepting non-⊥ without any active replay substitution)
- Reference to BUG-2026-03-16-001 and the URI-R2-C1 replay path

### Step 3: Update `Bugs.md` to mark BUG-2026-03-16-001 as Resolved

**File**: `Bugs.md`

Move BUG-2026-03-16-001 from `## Open` to `## Resolved`. Add:
- Resolved date: 2026-03-30
- Fix description: `allowReplayBoundMatch` in `applyInstantiation` accepts
  non-⊥ bound when replay-variable resolution changes the argument to match
- Regression test paths:
  - `test/ElaborationSpec.hs` — "BUG-2026-03-16-001 regression: InstBot accepts
    replay-resolved bound match"
  - `test/ElaborationSpec.hs` — "URI-R2-C1 witness replay stays
    alpha-equivalent to the locked no-fallback shape"

### Step 4: Verify all gates pass

**Commands**:
```
cabal build all && cabal test
./scripts/thesis-conformance-gate.sh
```

Test count must be ≥ 1273 (current baseline). No existing tests may break.
