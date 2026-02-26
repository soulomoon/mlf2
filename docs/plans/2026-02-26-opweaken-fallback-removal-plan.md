# OpWeaken Fallback Removal Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove all `OpWeaken` no-op fallback exits in Φ Ω replay so non-root weaken always maps to thesis-shaped elimination (`N`) or fails fast.

**Architecture:** Keep witness generation/normalization unchanged, and make strictness explicit in `MLF.Elab.Phi.Omega`: recover binder via canonical/class aliases when possible, otherwise return structured error instead of identity/no-op. Lock behavior with targeted elaboration regressions for both currently-defensive branches.

**Tech Stack:** Haskell (`base`, `containers`, `hspec`), Cabal test/build pipeline, existing Φ/Ω modules.

---

### Task 1: Pin current fallback behavior with red tests

**Files:**
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add test for unrecoverable non-binder alias weaken target**

Add a spec near the existing alias-recovery tests (around the `"OpWeaken on an alias target..."` block) that constructs a witness with `OpWeaken alias` where:
- `alias` is not a scheme binder,
- `Solved.classMembers solved alias` contains no recoverable binder in current spine,
- current behavior is silent no-op.

Assert **new expected behavior**: `phiFromEdgeWitnessNoTrace ...` returns `Left (PhiTranslatabilityError ...)` (or `PhiInvariantError`, depending on Task 2 decision).

**Step 2: Add test for binder-node with missing spine index**

Add a second spec where `isBinderNode` is true but `lookupBinderIndex` fails (current branch at `Omega.hs` old lines ~811-813). Assert fail-fast error, not success/no-op.

**Step 3: Run only the new tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpWeaken"'`
Expected: FAIL (tests red before implementation).

**Step 4: Commit red tests**

```bash
git add test/ElaborationSpec.hs
git commit -m "test: add red regressions for OpWeaken fallback removal"
```

### Task 2: Replace fallback no-op paths with strict erroring in Omega

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`

**Step 1: Refactor OpWeaken branch into explicit resolution outcomes**

In `go` case for `OpWeaken`:
- keep root case (`bvC == rootC`) as identity,
- for non-root case, introduce helper that returns either:
  - resolved binder node + index,
  - or structured failure explaining why binder recovery failed.

**Step 2: Remove both no-op exits**

Delete/replace:
- `Nothing -> go ...` inside non-binder recovery case,
- `lookupBinderIndex ... Nothing -> go ...` inside binder case.

Both become `Left ...` with details including:
- op target/source target,
- canonical target,
- class members considered,
- current `vSpineIds`.

**Step 3: Keep successful path thesis-shaped**

When resolution succeeds, always emit `InstElim` via `atBinderWith False ... (pure InstElim)`.

**Step 4: Run focused tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpWeaken"'`
Expected: PASS for new fail-fast tests and existing alias-recovery tests.

**Step 5: Commit behavior change**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "fix: remove OpWeaken no-op fallbacks and fail fast on unresolved binders"
```

### Task 3: Make error classification consistent and reviewable

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `src/MLF/Elab/Types.hs` (only if adding a dedicated constructor)

**Step 1: Decide error class**

Choose one and apply consistently:
- `PhiTranslatabilityError` if treated as invalid witness for thesis translation,
- `PhiInvariantError` if treated as impossible under valid translatable presolutions.

Recommendation: use `PhiTranslatabilityError` for user-facing invalid replay shape; reserve `PhiInvariantError` for internal impossible states.

**Step 2: Normalize message format**

Ensure both former fallback sites emit the same field-rich format (`op`, `target`, `canonical`, `classMembers`, `ids`, replay map domains when relevant).

**Step 3: Run compile + targeted suite**

Run:
- `cabal build`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi|OpWeaken|alias target"'`

Expected: PASS.

**Step 4: Commit (if separate from Task 2)**

```bash
git add src/MLF/Elab/Phi/Omega.hs src/MLF/Elab/Types.hs test/ElaborationSpec.hs
git commit -m "refactor: unify OpWeaken unresolved-binder error semantics"
```

### Task 4: Validate no silent OpWeaken skip remains in Omega

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs` (only if grep reveals leftover skip)

**Step 1: Audit OpWeaken branch for identity exits**

Run: `rg -n "OpWeaken|go binderKeys .* accum rest|Nothing ->" src/MLF/Elab/Phi/Omega.hs`

Confirm only allowed identity paths remain:
- root weaken (`bvC == rootC`),
- rigid-node identities as thesis-defined elsewhere (not this fallback).

**Step 2: Add comment documenting strict invariant**

Add succinct note in `OpWeaken` case:
- non-root weaken must resolve to a replay binder,
- unresolved target is now explicit error.

**Step 3: Commit (if changed)**

```bash
git add src/MLF/Elab/Phi/Omega.hs
git commit -m "docs: state strict OpWeaken replay invariant in Omega"
```

### Task 5: Full regression verification (@verification-before-completion)

**Files:**
- No source edits expected

**Step 1: Run full project gate**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 2: Run direct-details test pass for evidence capture**

Run: `cabal test --test-show-details=direct`
Expected: PASS with all examples green.

### Task 6: Documentation and tracker sync

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md` (if roadmap priorities change)
- Modify: `Bugs.md` (only if this closes/updates a tracked bug)

**Step 1: Record thesis alignment change**

Add dated note:
- removed defensive `OpWeaken` no-op fallback,
- unresolved non-root weaken now fails fast,
- cite thesis anchor (Fig. 15.3.4 / §15.3.5).

**Step 2: Add changelog entry**

Concise entry for strictness hardening and test coverage.

**Step 3: Update bug/deviation status if applicable**

If there is a bug/deviation line for this caveat, mark resolved with regression test reference.

**Step 4: Commit docs**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md Bugs.md
git commit -m "docs: record strict OpWeaken thesis-exact closure"
```

### Task 7: Final quality check and handoff

**Files:**
- No edits expected

**Step 1: Show final diff summary**

Run:
- `git status --short`
- `git log --oneline -n 8`
- `git diff --stat HEAD~N..HEAD` (replace `N` with commit count)

**Step 2: Prepare review checklist**

Confirm:
- no non-root `OpWeaken` silent skip remains,
- alias recovery still works,
- unrecoverable targets fail fast,
- full gate green.

