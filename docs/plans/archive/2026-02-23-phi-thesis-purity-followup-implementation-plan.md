# Phi Thesis Purity Follow-up Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close the remaining actionable Phi/Witness deltas by documenting load-bearing deviations and aligning Omega translation closer to thesis-shape without breaking translatability.

**Architecture:** Keep the existing phase-typed `Σ; O; Ω` pipeline intact and make only local, behavior-preserving changes in `MLF.Elab.Phi.Omega` and witness/deviation docs. For code changes, prefer adding/adjusting targeted Phi translation tests first, then apply minimal edits that preserve current end-to-end elaboration behavior. Keep load-bearing behavior explicit in `thesis-deviations.yaml` + claim links so conformance tooling remains green.

**Tech Stack:** Haskell (`cabal`, `hspec`), YAML docs (`docs/thesis-claims.yaml`, `docs/thesis-deviations.yaml`), shell validation scripts.

---

### Task 1: Register the 3 Load-Bearing Deviations (4, 5, 6)

**Files:**
- Modify: `docs/thesis-deviations.yaml`
- Modify: `docs/thesis-claims.yaml`
- Test: `scripts/check-thesis-claims.sh`

**Step 1: Add deviation entries**

Add three `implementation-choice` entries under Chapter 15/Section 15.3 for:
1. witness-side `suppressWeaken`/`argIsGenBound` guard,
2. `keepBinderKeys` weaken suppression,
3. standalone `OpGraft` translation without paired `OpWeaken`.

Use `code_paths` at minimum:
- `src/MLF/Constraint/Presolution/Witness.hs`
- `src/MLF/Elab/Phi/Omega.hs`

**Step 2: Link deviations to claims**

Update `CLM-PHI-CORRECTNESS` in `docs/thesis-claims.yaml` so `deviations:` includes the 3 new IDs. This avoids orphan-failures in the claims checker.

**Step 3: Validate claim/deviation cross-links**

Run:
```bash
./scripts/check-thesis-claims.sh
```
Expected: PASS with no schema, orphan, or missing-reference errors.

**Step 4: Commit docs-only checkpoint**

```bash
git add docs/thesis-deviations.yaml docs/thesis-claims.yaml
git commit -m "Register Phi load-bearing implementation-choice deviations"
```

### Task 2: Add Focused Failing Tests for Omega Translation Changes

**Files:**
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add a failing test for bounded bound-match translation shape**

Add a case that builds a simple bounded binder scenario (`OpGraft arg bv` + `OpWeaken bv` where `argTy` matches explicit bound) and asserts Phi emits bounded application shape (`InstApp boundTy`) rather than direct elimination shape.

Suggested assertion pattern:
```haskell
phi <- requireRight (...)
Elab.pretty phi `shouldSatisfy` ("⟨" `isInfixOf`)
Elab.pretty phi `shouldNotSatisfy` (" N" `isInfixOf`)
```

**Step 2: Add a regression test for de-fused 3-op handling**

Add a case with `OpGraft; OpRaise; OpWeaken` on the same binder and assert translation still succeeds and the instantiated output type matches the expected type. Do not assert an exact instantiation string; assert semantic result.

**Step 3: Add a regression test for bottom-rescue behavior after reification move**

Use an existing BUG-003/BUG-004 style shape where the graft argument can reify as `⊥`; assert pipeline/elaboration still avoids bottom-collapse in the resulting function shape.

**Step 4: Run targeted tests to confirm failures pre-implementation**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "bounded bound-match|de-fused 3-op|bottom-collapse"'
```
Expected: new tests fail before code edits.

### Task 3: Remove `mergeIntoApp` 3-Op Fusion (Deviation 1)

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Delete fusion branch**

In the `OpGraft` branch (current `mergeIntoApp` logic around `src/MLF/Elab/Phi/Omega.hs:890`), remove the peephole that consumes `OpRaise` + `OpWeaken` and directly emits `InstApp`.

**Step 2: Always translate standalone `OpGraft` through its existing path**

Keep the current non-fusion path (`atBinderKeep ... InstInside (InstBot argTy)`) and continue replaying remaining ops normally.

**Step 3: Run focused tests**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "de-fused 3-op|Phase 6 — Elaborate \(xMLF\)"'
```
Expected: new de-fusion test passes and existing Phi/Omega tests remain green.

**Step 4: Commit checkpoint**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "Remove OpGraft-OpRaise-OpWeaken fusion in Omega translation"
```

### Task 4: Push `rescueBottomAtBinder` into Reification Path (Deviation 2)

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Move rescue logic into `reifyTypeArg`**

Refactor `reifyTypeArg` to apply binder-aware `⊥ -> TVar binderName` rescue internally when a binder target is provided. Keep caller-facing behavior the same.

**Step 2: Update call sites**

Replace explicit post-reify rescue calls with binder-aware calls, e.g.:
```haskell
reifyTypeArg namedSet' (Just bvReplay) (graftArgFor arg bv)
```
Then remove `rescueBottomAtBinder` helper from operation-level code.

**Step 3: Re-run regression slices**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-003|BUG-004|bottom-collapse|OpGraft\+OpWeaken"'
```
Expected: no regressions in historical bottom-collapse bug matrix.

**Step 4: Commit checkpoint**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "Move binder-bottom rescue into reify path"
```

### Task 5: Replace bound-match `InstElim` Shortcut with `InstApp boundTy` (Deviation 3)

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Implement literal thesis-shape emission**

In `OpGraft+OpWeaken` bounded-branch handling (current `src/MLF/Elab/Phi/Omega.hs:804-814`), replace `pure InstElim` shortcut with `pure (InstApp boundTy)` when arg matches explicit bound.

**Step 2: Keep the same translatability guard**

Do not relax guards for mismatched bounded grafts; preserve current error path when argument does not match explicit bound.

**Step 3: Run targeted tests**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "bounded bound-match|OpGraft\+OpWeaken|Phase 6 — Elaborate \(xMLF\)"'
```
Expected: new bounded-shape test passes; existing translatability checks still pass.

**Step 4: Commit checkpoint**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "Emit InstApp for bounded graft-weaken bound-match case"
```

### Task 6: Sync Project Docs and Final Verification

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Optional Modify (if priorities changed): `TODO.md`

**Step 1: Update implementation notes**

Add a dated note summarizing:
- new registered load-bearing deviations,
- removal of 3-op fusion,
- reification-layer move for bottom-rescue,
- bound-match emission change to `InstApp`.

**Step 2: Add changelog entry**

Add a concise entry describing thesis-purity alignment and preserved behavior guarantees.

**Step 3: Run full mandatory verification**

Run:
```bash
cabal build all && cabal test
./scripts/check-thesis-claims.sh
```
Expected: all PASS.

**Step 4: Final commit**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md
git commit -m "Document Phi thesis-purity follow-up and verification evidence"
```

### Task 7: Optional Safety Net (Recommended)

**Files:**
- Optional Modify: `test/PhiSoundnessSpec.hs`

**Step 1: Add one extra Phi soundness case**

Add one expression-level regression that exercises bounded-instantiation + let-polymorphism together. Assert `runPipelineElab` succeeds and resulting term type-checks.

**Step 2: Run only Phi soundness slice**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi soundness"'
```
Expected: PASS.
