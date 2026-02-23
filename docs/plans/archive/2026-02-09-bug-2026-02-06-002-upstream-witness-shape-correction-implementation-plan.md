# BUG-2026-02-06-002 Upstream Witness-Shape Correction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix `BUG-2026-02-06-002` by correcting witness shape upstream (presolution normalization path) so elaboration preserves polymorphic factory behavior and no longer collapses to `TBottom`/`Int` arrow artifacts.

**Architecture:** Drive the fix from witness normalization (`ewSteps` contract) instead of adding more Ω heuristics. Add RED tests for witness-shape + pipeline outcomes, implement canonical graft/weaken alignment in presolution normalization, then graduate the currently pending sentinel matrix into strict assertions. Keep strict translatability errors and reject malformed witness shape early.

**Tech Stack:** Haskell (GHC 9.12.2), Cabal, Hspec, `MLF.Constraint.Presolution.WitnessNorm`, `MLF.Constraint.Presolution.WitnessCanon`, `MLF.Elab.Phi.Omega`, `test/Presolution/WitnessSpec.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`.

---

### Task 1: Lock RED bug-target tests (without losing sentinel intent)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `mlf2.cabal` (only if a new spec module is introduced)
- Modify: `test/Main.hs` (only if a new spec module is introduced)

**Step 1: Keep the existing sentinel matrix pending as documentation.**
- Leave `describe "BUG-2026-02-06-002 sentinel matrix"` as `pendingWith` until finalization.

**Step 2: Add a new strict block for the same 4 shapes with explicit expected outcomes (RED now).**
```haskell
describe "BUG-2026-02-06-002 strict target matrix" $ do
  it "make-only elaborates polymorphic factory" $ ...
  it "make-app does not collapse to bottom-int arrow" $ ...
  it "let-c1-return keeps second binder polymorphic" $ ...
  it "let-c1-apply-bool typechecks to Int" $ ...
```

**Step 3: Run strict block to verify RED.**
Run: `cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002 strict target matrix"' --test-show-details=direct`
Expected: at least one FAIL on current baseline.

**Step 4: Re-run sentinel pending block for policy sanity.**
Run: `cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002 sentinel matrix"' --test-show-details=direct`
Expected: `4 pending`.

**Step 5: Commit RED checkpoint.**
```bash
git add test/PipelineSpec.hs
# git add mlf2.cabal test/Main.hs  # only if touched
git commit -m "test: add strict red matrix for BUG-2026-02-06-002"
```

---

### Task 2: Add witness-shape normalization regression tests (RED)

**Files:**
- Modify: `test/Presolution/WitnessSpec.hs`

**Step 1: Add a case for canonical graft→weaken alignment with binderArgs/copy-map correspondence.**
```haskell
it "normalizes graft-weaken pairs with canonical binder/arg alignment" $ do
  -- build env + ops that mirror copied binder ids
  normalizeInstanceStepsFull env steps0 `shouldBe` Right expected
```

**Step 2: Add malformed-ambiguity rejection case.**
```haskell
it "rejects ambiguous graft-weaken mapping after canonicalization" $ do
  normalizeInstanceStepsFull env badSteps
    `shouldSatisfy` isLeft
```

**Step 3: Add idempotence case focused on graft/weaken-heavy sequences.**
```haskell
it "is idempotent for graft-weaken normalization" $ ...
```

**Step 4: Run only witness tests.**
Run: `cabal test mlf2-test --test-options='--match "graft-weaken|idempotent|ambiguous"' --test-show-details=direct`
Expected: RED at least on the new behavior-driven cases.

**Step 5: Commit RED witness tests.**
```bash
git add test/Presolution/WitnessSpec.hs
git commit -m "test: add witness-shape red regressions for BUG-2026-02-06-002"
```

---

### Task 3: Add Φ translation guard tests tied to normalized witness shape (RED/GREEN gate)

**Files:**
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add a focused Φ case for `OpGraft ... OpWeaken` that previously collapsed.**
```haskell
it "translates normalized graft-weaken without bottom collapse" $ do
  phi <- requireRight (...)
  out <- requireRight (Elab.applyInstantiation ... phi)
  canonType out `shouldBe` canonType expected
```

**Step 2: Add a non-regression case for non-front binder targeting.**
- Reuse patterns near existing tests around non-front binder reorder (`OpGraft intNode binderB`, `OpWeaken binderB`).

**Step 3: Run targeted ElaborationSpec checks.**
Run: `cabal test mlf2-test --test-options='--match "graft-weaken|non-front binder|StepIntro"' --test-show-details=direct`
Expected: baseline may fail until implementation.

**Step 4: Ensure existing Φ tests still compile and execute.**
Run: `cabal test mlf2-test --test-options='--match "scheme-aware Φ"' --test-show-details=direct`
Expected: no unrelated regressions.

**Step 5: Commit Φ regression coverage.**
```bash
git add test/ElaborationSpec.hs
git commit -m "test: add phi translation regressions for upstream witness-shape fix"
```

---

### Task 4: Implement canonical graft/weaken alignment in witness normalization

**Files:**
- Modify: `src/MLF/Constraint/Presolution/WitnessCanon.hs`

**Step 1: Add a pure helper that aligns/validates graft→weaken relationships in normalized op blocks.**
- Keep helper local to `normalizeInstanceOpsFull` pipeline (or exported if reused by tests).

**Step 2: Integrate helper after canonicalization/coalescing and before final validation.**
- Preserve existing pass ordering unless a test proves reorder is required.

**Step 3: Represent malformed mapping as normalization error (no silent fallback).**
- Reuse existing `OmegaNormalizeError` constructors when possible.
- Add a new constructor only if diagnostics need finer distinction.

**Step 4: Run witness-focused tests from Tasks 2–3.**
Run: `cabal test mlf2-test --test-options='--match "witness|graft-weaken|BUG-2026-02-06-002 strict target matrix"' --test-show-details=direct`
Expected: witness tests move GREEN or fail in actionable ways.

**Step 5: Commit normalization implementation.**
```bash
git add src/MLF/Constraint/Presolution/WitnessCanon.hs
git commit -m "fix: canonicalize graft-weaken witness shape upstream"
```

---

### Task 5: Thread alignment through edge-witness normalization environment

**Files:**
- Modify: `src/MLF/Constraint/Presolution/WitnessNorm.hs`
- Modify: `src/MLF/Constraint/Presolution/Witness.hs` (only if exports/env wiring need extension)

**Step 1: Ensure binderArgs/copy-map rewriting provides unambiguous canonical inputs for the new helper.**
- Keep normalization in rewritten node space; validate before restoring ids.

**Step 2: Add/adjust env fields only if strictly needed.**
- Prefer reusing `binderArgs`, `orderKeys`, `interior`, `weakened`.

**Step 3: Maintain thesis-exact validation sequence.**
- Normalize → validate rewritten ops → restore original ids.

**Step 4: Run presolution normalization and elaboration targeted tests.**
Run: `cabal test mlf2-test --test-options='--match "Witness|Φ|BUG-2026-02-06-002 strict target matrix"' --test-show-details=direct`
Expected: strict matrix should start improving.

**Step 5: Commit env/wiring updates.**
```bash
git add src/MLF/Constraint/Presolution/WitnessNorm.hs
# git add src/MLF/Constraint/Presolution/Witness.hs  # if touched
git commit -m "fix: enforce upstream witness-shape contract during edge normalization"
```

---

### Task 6: Keep Ω translation strict; remove temporary probe behavior

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs` (only if required for compatibility with normalized shape)

**Step 1: Confirm no temporary H16 probe logic remains.**
- Ensure no speculative weaken/graft heuristics survive.

**Step 2: Make minimal compatibility edits only if new normalized shape requires it.**
- No behavior broadening; maintain invariant/translatability errors.

**Step 3: Run focused Ω/Φ translation tests.**
Run: `cabal test mlf2-test --test-options='--match "OpGraft|OpWeaken|OpRaise|scheme-aware Φ"' --test-show-details=direct`
Expected: GREEN for existing + newly added Φ cases.

**Step 4: Re-run H15 guard.**
Run: `cabal test mlf2-test --test-options='--match "does not leak solved-node names in make let mismatch"' --test-show-details=direct`
Expected: PASS.

**Step 5: Commit only if file changed.**
```bash
git add src/MLF/Elab/Phi/Omega.hs
git commit -m "refactor: keep omega strict with upstream-normalized witness shape"
```

---

### Task 7: Graduate sentinels to strict assertions (final bug closure)

**Files:**
- Modify: `test/PipelineSpec.hs`

**Step 1: Replace the 4 `pendingWith` sentinel examples with concrete assertions.**
- Keep case names stable so historical logs remain comparable.

**Step 2: Ensure new strict target matrix and sentinel matrix agree (dedupe if redundant).**
- Merge blocks if both now encode the same assertions.

**Step 3: Run all BUG-2026-02-06-002 tests.**
Run: `cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002"' --test-show-details=direct`
Expected: PASS, 0 pending for the bug matrix.

**Step 4: Run broader nearby regression pack.**
Run: `cabal test mlf2-test --test-options='--match "generalizes reused constructors via make const|redirected let-use sites keep polymorphic schemes|does not leak solved-node names in make let mismatch"' --test-show-details=direct`
Expected: PASS.

**Step 5: Commit closure tests.**
```bash
git add test/PipelineSpec.hs
git commit -m "test: graduate BUG-2026-02-06-002 sentinels to strict passing assertions"
```

---

### Task 8: Final verification and project records

**Files:**
- Modify: `tasks/todo/2026-02-08-investigate-bug-phase7/task_plan.md`
- Modify: `tasks/todo/2026-02-08-investigate-bug-phase7/findings.md`
- Modify: `tasks/todo/2026-02-08-investigate-bug-phase7/progress.md`
- Modify: `TODO.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `/Volumes/src/mlf4/Bugs.md`

**Step 1: Run full verification gate.**
Run: `cabal build all && cabal test`
Expected: PASS.

**Step 2: Update bug tracker status.**
- Move `BUG-2026-02-06-002` to resolved section in `/Volumes/src/mlf4/Bugs.md`.
- Link regression test paths.

**Step 3: Update thesis-faithfulness docs for behavior changes/deviations.**
- Add concise alignment note in `implementation_notes.md`.

**Step 4: Update changelog and task logs with evidence.**
- Include command outputs and exact impacted tests.

**Step 5: Close task folder if all phases complete.**
- Move `tasks/todo/2026-02-08-investigate-bug-phase7/` to `tasks/archive/2026-02-08-investigate-bug-phase7/` when done.

---

## Execution Sequence Notes

- Execute tasks in order; do not skip RED checkpoints.
- Keep each probe reversible; if a step regresses sentinel/hard-gate tests, revert immediately and record evidence.
- Prefer smallest patch that satisfies all strict gates.

## Done Criteria

1. `BUG-2026-02-06-002` matrix has strict assertions and passes (no pending placeholders).
2. H15 non-leak regression remains green.
3. Witness normalization tests for graft/weaken alignment and idempotence pass.
4. `cabal build all && cabal test` passes.
5. Docs/task/bug tracker/changelog updated in the same iteration.
