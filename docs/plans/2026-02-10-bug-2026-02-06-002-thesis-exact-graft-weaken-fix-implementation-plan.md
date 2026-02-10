# BUG-2026-02-06-002 Thesis-Exact Graft→Weaken Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the remaining `BUG-2026-02-06-002` failures by making `OpGraft`/`OpWeaken` handling thesis-exact (normalized delayed weakenings upstream, local Φ translation downstream) so `make-app`, `let-c1-return`, and `let-c1-apply-bool` stop collapsing to `TBottom`-driven shapes.

**Architecture:** Move delayed-graft/weaken handling out of downstream Ω look-ahead and into normalized witness operations that satisfy thesis delayed-weakening constraints (Definition 11.5.2 + §15.3.5 translation assumptions). Keep Φ translation local and deterministic: adjacent witness shape in, local instantiation out. Preserve existing invariants and focused regressions while turning strict bug matrix green.

**Tech Stack:** Haskell (GHC/Cabal), Hspec, `MLF.Constraint.Presolution.WitnessValidation`, `MLF.Constraint.Presolution.WitnessCanon`, `MLF.Constraint.Presolution.WitnessNorm`, `MLF.Elab.Phi.Omega`, `test/Presolution/WitnessSpec.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, `test/ThesisFixDirectionSpec.hs`.

**Execution discipline:** @haskell-pro @test-driven-development @systematic-debugging @verification-before-completion

---

### Task 1: Lock thesis-law RED tests for delayed weakenings

**Files:**
- Modify: `test/Presolution/WitnessSpec.hs`
- Test: `test/Presolution/WitnessSpec.hs`
- Test: `test/PipelineSpec.hs`

**Step 1: Add a failing validator-level test for delayed-weakening violation.**

```haskell
it "flags Weaken when later ops still transform strict descendants" $ do
  let env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId child])
      ops = [OpWeaken root, OpGraft arg child]
  validateNormalizedWitness env ops
    `shouldSatisfy` isLeft
```

**Step 2: Add a failing normalization test for non-adjacent delayed `Graft/Weaken` pairing.**

```haskell
it "pulls delayed weaken next to its matching graft when middle ops are unrelated" $ do
  let ops0 = [OpGraft arg binder, OpMerge x y, OpWeaken binder]
  normalizeInstanceOpsFull env ops0
    `shouldBe` Right [OpGraft arg binder, OpWeaken binder, OpMerge x y]
```

**Step 3: Run witness tests to confirm RED.**

Run:
`cabal test mlf2-test --test-options='--match "delayed weakenings|graft-weaken canonical alignment|flags Weaken|pulls delayed weaken"' --test-show-details=direct`

Expected: at least one failure on baseline.

**Step 4: Re-run strict bug matrix baseline gate.**

Run:
`cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002 strict target matrix"' --test-show-details=direct`

Expected: current known RED (before fix work).

**Step 5: Commit RED checkpoint.**

```bash
git add test/Presolution/WitnessSpec.hs
git commit -m "test: add thesis delayed-weaken red regressions"
```

---

### Task 2: Make delayed-weaken validation explicit and thesis-traceable

**Files:**
- Modify: `src/MLF/Constraint/Presolution/WitnessValidation.hs`
- Modify: `test/Presolution/WitnessSpec.hs`
- Test: `test/Presolution/WitnessSpec.hs`

**Step 1: Add a dedicated validation error for delayed-weaken violations.**

```haskell
data OmegaNormalizeError
  = ...
  | DelayedWeakenViolation NodeId NodeId
```

**Step 2: Return the new error from `checkWeakenOrdering` instead of overloaded errors.**

```haskell
case firstOffender desc rest of
  Just offender -> Left (DelayedWeakenViolation (canon n) offender)
  Nothing -> checkWeakenOrdering rest
```

**Step 3: Update tests to assert the exact constructor.**

```haskell
validateNormalizedWitness env ops
  `shouldBe` Left (DelayedWeakenViolation root child)
```

**Step 4: Run validator-focused tests.**

Run:
`cabal test mlf2-test --test-options='--match "delayed weakenings|DelayedWeakenViolation|flags Weaken"' --test-show-details=direct`

Expected: PASS for new validator expectations.

**Step 5: Commit validation clarity patch.**

```bash
git add src/MLF/Constraint/Presolution/WitnessValidation.hs test/Presolution/WitnessSpec.hs
git commit -m "fix: encode delayed-weaken thesis invariant with explicit error"
```

---

### Task 3: Normalize delayed graft→weaken pairs upstream (WitnessCanon)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/WitnessCanon.hs`
- Modify: `test/Presolution/WitnessSpec.hs`
- Test: `test/Presolution/WitnessSpec.hs`

**Step 1: Add a helper to rewrite safe delayed pairs into adjacent pairs.**

```haskell
coalesceDelayedGraftWeakenWithEnv
  :: OmegaNormalizeEnv -> [InstanceOp] -> Either OmegaNormalizeError [InstanceOp]
```

Algorithm rule (minimal):
- detect `OpGraft arg b ... OpWeaken b`
- allow rewrite only when middle ops do not transform `b` nor strict descendants of `b`
- preserve stable order for unaffected ops.

**Step 2: Integrate helper into normalization pipeline before final validation.**

```haskell
ops5 <- reorderWeakenWithEnv env ops4
ops6 <- coalesceDelayedGraftWeakenWithEnv env ops5
let ops7 = dropRedundantOps ops6
validateNormalizedWitness env ops7
pure ops7
```

**Step 3: Extend canonical alignment tests for interleaved unrelated ops and idempotence.**

```haskell
it "is idempotent after delayed graft-weaken coalescing" $ ...
```

**Step 4: Run witness normalization suite.**

Run:
`cabal test mlf2-test --test-options='--match "Phase 3 — Witness normalization|graft-weaken canonical alignment|delayed weakenings"' --test-show-details=direct`

Expected: PASS with no ambiguity regressions.

**Step 5: Commit upstream normalization fix.**

```bash
git add src/MLF/Constraint/Presolution/WitnessCanon.hs test/Presolution/WitnessSpec.hs
git commit -m "fix: normalize delayed graft-weaken pairs upstream"
```

---

### Task 4: Keep Ω translation local (remove delayed look-ahead heuristic)

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `test/ElaborationSpec.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Add a Φ regression test proving behavior depends only on local normalized shape.**

```haskell
it "does not require non-local look-ahead for delayed graft-weaken handling" $ do
  -- build witness with adjacent normalized pair and unrelated middle ops
  -- assert translated instantiation matches expected polymorphic shape
```

**Step 2: Remove non-local delayed-weaken scan in standalone `OpGraft` branch.**

Delete `splitDelayedWeaken`/`delayedWeaken` path in `(OpGraft arg bv : rest)` and keep local behavior:

```haskell
(inst, ids1) <- atBinderKeep binderKeys ids ty bv $ do
  argTy <- reifyTypeArg namedSet' (Just bv) arg
  pure (InstInside (InstBot argTy))
```

**Step 3: Keep adjacent pair path (`OpGraft : OpWeaken`) intact as the thesis-aligned translation target.**

```haskell
(OpGraft arg bv : OpWeaken bv' : rest) -> ...
```

**Step 4: Run Ω/Φ tests and focused bug guards.**

Run:
- `cabal test mlf2-test --test-options='--match "Witness translation (Φ/Σ)|non-front binder|graft-weaken"' --test-show-details=direct`
- `cabal test mlf2-test --test-options='--match "does not leak solved-node names in make let mismatch"' --test-show-details=direct`

Expected: all targeted tests PASS.

**Step 5: Commit Ω localization change.**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "refactor: remove omega delayed-lookahead and rely on normalized witness shape"
```

---

### Task 5: Tighten `reifyTypeArg` bottom rescue to thesis-backed context only

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `test/ElaborationSpec.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Add a regression test for over-broad `TBottom` rescue.**

```haskell
it "does not rescue TBottom to binder TVar outside graft-weaken context" $ do
  -- construct path without matching weaken
  -- expect unchanged TBottom behavior (or explicit translatability error)
```

**Step 2: Scope rescue logic to the adjacent graft/weaken translation path.**

```haskell
-- move rescue into OpGraft+OpWeaken case; remove global chosenTy2 fallback
```

**Step 3: Keep naming substitutions (`substSchemeNames`) unchanged.**

```haskell
chosenTy = substSchemeNames chosenTy1
```

**Step 4: Run Φ tests + strict matrix gate.**

Run:
- `cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002 strict target matrix"' --test-show-details=direct`
- `cabal test mlf2-test --test-options='--match "does not leak solved-node names in make let mismatch|redirected let-use sites keep polymorphic schemes"' --test-show-details=direct`

Expected: strict matrix improves or stays improved without focused regressions.

**Step 5: Commit narrowed rescue behavior.**

```bash
git add src/MLF/Elab/Phi/Omega.hs test/ElaborationSpec.hs
git commit -m "fix: scope reifyTypeArg bottom-rescue to thesis-backed graft-weaken context"
```

---

### Task 6: Turn BUG strict matrix green and graduate sentinel tests

**Files:**
- Modify: `test/PipelineSpec.hs`
- Test: `test/PipelineSpec.hs`
- Test: `test/ThesisFixDirectionSpec.hs`

**Step 1: Re-run strict matrix until all four cases are green.**

Run:
`cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002 strict target matrix"' --test-show-details=direct`

Expected: `4 examples, 0 failures`.

**Step 2: Replace sentinel `pendingWith` cases with strict assertions.**

```haskell
describe "BUG-2026-02-06-002 sentinel matrix" $ do
  it "make-only ..." $ ... -- concrete assertion
```

**Step 3: Run full bug matcher and thesis target spec.**

Run:
- `cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002"' --test-show-details=direct`
- `cabal test mlf2-test --test-options='--match "BUG-2026-02-06-002 thesis target"' --test-show-details=direct`

Expected: zero failures, zero pending for this bug family.

**Step 4: Run focused non-bug regressions.**

Run:
`cabal test mlf2-test --test-options='--match "generalizes reused constructors via make const|redirected let-use sites keep polymorphic schemes|does not leak solved-node names in make let mismatch"' --test-show-details=direct`

Expected: PASS.

**Step 5: Commit bug-closure tests.**

```bash
git add test/PipelineSpec.hs test/ThesisFixDirectionSpec.hs
git commit -m "test: graduate BUG-2026-02-06-002 sentinels after thesis-exact graft-weaken fix"
```

---

### Task 7: Final verification and project records

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-02-08-investigate-bug-phase7/task_plan.md`
- Modify: `tasks/todo/2026-02-08-investigate-bug-phase7/findings.md`
- Modify: `tasks/todo/2026-02-08-investigate-bug-phase7/progress.md`
- Modify: `/Volumes/src/mlf4/Bugs.md`

**Step 1: Run full project verification gate.**

Run:
`cabal build all && cabal test`

Expected: full pass.

**Step 2: Document thesis alignment and exact invariants in implementation notes.**

Add a note with references to:
- Definition 11.5.2 (delayed weakenings)
- Figure 15.3.4 / §15.3.5 translation assumptions.

**Step 3: Update changelog and bug tracker.**

Record:
- fixed bug ID,
- tests proving closure,
- retained behavior constraints.

**Step 4: Update task logs and TODO roadmap.**

Ensure findings/progress include:
- commands,
- before/after matrix,
- rollback history.

**Step 5: Commit docs + tracking updates.**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md \
  tasks/todo/2026-02-08-investigate-bug-phase7/task_plan.md \
  tasks/todo/2026-02-08-investigate-bug-phase7/findings.md \
  tasks/todo/2026-02-08-investigate-bug-phase7/progress.md \
  /Volumes/src/mlf4/Bugs.md
git commit -m "docs: record thesis-exact graft-weaken fix and verification evidence"
```

---

## Hard Gates (must stay true after every implementation task)

1. `BUG-2026-02-06-002 strict target matrix` never regresses in pass count.
2. Focused guards always PASS:
   - `generalizes reused constructors via make const`
   - `redirected let-use sites keep polymorphic schemes`
   - `does not leak solved-node names in make let mismatch`
3. Witness invariants stay valid under canonicalization and delayed-weakening checks.
4. No retained probe-only heuristics in Ω that depend on non-local future-op scanning.

## Rollback Rule

If a task fails hard gates, immediately revert that task’s patch, record the failure in `tasks/todo/2026-02-08-investigate-bug-phase7/progress.md`, and move to the next smallest hypothesis.

