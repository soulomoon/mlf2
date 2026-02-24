# Implementation Plan: Eliminate DEV-PHI-WITNESS-WEAKEN-SUPPRESSION

**Design:** `docs/plans/2026-02-24-phi-witness-weaken-suppression-elimination-design.md`
**Approach:** A — Remove emission guards, extend Omega to handle extra OpWeaken

## Prerequisites

- All 781+ tests green (`cabal build all && cabal test`)
- `./scripts/check-thesis-claims.sh` PASS
- `./scripts/thesis-conformance-gate.sh` PASS

---

## Task 1: Baseline Snapshot

**Goal:** Verify green baseline before any changes.

1. Run `cabal build all && cabal test` — record example count and confirm 0 failures.
2. Run `./scripts/check-thesis-claims.sh` — confirm PASS.
3. Run `./scripts/thesis-conformance-gate.sh` — confirm PASS.

**Verification:** All three commands green.

---

## Task 2: Characterization Tests (Tests-First)

**Goal:** Write failing tests that demonstrate thesis-exact emission behavior before removing suppression.

### 2a: Witness emission — suffix-forall case

Add test to `test/Presolution/WitnessSpec.hs`:
- "emits OpWeaken for unbounded binders even when suffix has forall (thesis-exact)"
- Construct `ExpCompose [ExpInstantiate [argId], ExpForall [fsSpec]]`
- Assert witness contains `OpGraft argId binderId` AND `OpWeaken binderId`
- This test will FAIL initially (current code suppresses the OpWeaken)

### 2b: Witness emission — argIsGenBound case

Add test to `test/Presolution/WitnessSpec.hs`:
- "emits OpWeaken for gen-bound args (thesis-exact)"
- Construct expansion with arg that is a scheme root flexibly bound to its gen node
- Assert witness contains `OpGraft argId binderId` AND `OpWeaken binderId`
- This test will FAIL initially

### 2c: Edge witness — annotation edge case

Add test to `test/Presolution/EdgePlannerSpec.hs` or `WitnessSpec.hs`:
- "annotation edges preserve OpWeaken in witness (thesis-exact)"
- Construct annotation edge (`cAnnEdges` membership) with `ExpInstantiate`
- Assert `edgeWitnessPlan` output contains `OpWeaken`
- This test will FAIL initially

### 2d: Update existing suppression test

Update the existing test in `WitnessSpec.hs`:
- "suppresses OpWeaken for rigid binders during ExpInstantiate witness emission"
- Rename to reflect thesis-exact behavior
- Update expected output to include `OpWeaken binderId`
- This test will FAIL initially

**Verification:** New tests fail with expected mismatch (OpWeaken missing). Existing tests still pass.

---

## Task 3: Remove Inner Suppression (Witness.hs)

**Goal:** Make `witnessFromExpansion` thesis-exact — always emit OpWeaken for unbounded binders.

### 3a: Remove `suppressWeaken` and `argIsGenBound` guards in `classify`

File: `src/MLF/Constraint/Presolution/Witness.hs`

Change `classify` (lines 145-165):
```haskell
-- BEFORE:
let weakenOp =
        if suppressWeaken || argIsGenBound
            then []
            else [OpWeaken bv]

-- AFTER:
let weakenOp = [OpWeaken bv]
```

Remove the `suppressWeaken` parameter from `classify` signature.

### 3b: Simplify `witnessAlg` ExpCompose case

The `ExpCompose` case (lines 128-137) currently computes suffix flags to pass as
`suppressWeaken` to child steppers. Since `classify` no longer uses `suppressWeaken`,
simplify the stepper signature from `Bool -> PresolutionM (Int, [InstanceOp])` to
`PresolutionM (Int, [InstanceOp])`.

Update:
- `witnessAlg` type signature: remove `Bool` from stepper
- `ExpIdentityF`: return `pure (0, [])` directly
- `ExpForallF`: return `pure (count, [])` directly
- `ExpInstantiateF`: remove `\suppressWeaken ->` lambda wrapper
- `ExpComposeF`: remove suffix flag computation, just sequence child steppers
- `witnessFromExpansion` call site (line 86): call stepper without `False` argument

### 3c: Remove `argIsGenBound` helper

Delete the `argIsGenBound` function (lines 182-198) — no longer called.

### 3d: Remove unused imports

Remove imports that were only used by `argIsGenBound`:
- `MLF.Constraint.Presolution.StateAccess (getConstraintAndCanonical)` — check if used elsewhere
- `qualified MLF.Binding.Tree as Binding` — check if used elsewhere
- `qualified Data.IntMap.Strict as IntMap` — check if used elsewhere
- `qualified MLF.Constraint.NodeAccess as NodeAccess` — check if used elsewhere

**Verification:** Tests from Task 2a and 2b now pass. Run `cabal build` to confirm compilation.

---

## Task 4: Remove Outer Suppression (EdgeProcessing)

**Goal:** Remove annotation-edge blanket weaken stripping.

### 4a: Remove `dropWeakenOps` call in `edgeWitnessPlan`

File: `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`

Change `edgeWitnessPlan` (lines 67-74):
```haskell
-- BEFORE:
edgeWitnessPlan gid suppressWeaken leftId leftRaw expn = do
    ...
    let baseOps = if suppressWeaken then dropWeakenOps baseOps0 else baseOps0
    ...

-- AFTER:
edgeWitnessPlan gid leftId leftRaw expn = do
    ...
    pure EdgeWitnessPlan { ewpForallIntros = introCount, ewpBaseOps = baseOps0 }
```

Remove the `suppressWeaken :: Bool` parameter.

### 4b: Remove `dropWeakenOps` function

Delete `dropWeakenOps` (lines 101-107) and remove from module exports (line 14).

### 4c: Remove Note [Weaken suppression on annotation edges]

Delete the note block (lines 53-59) — no longer applicable.

### 4d: Update call site in Interpreter.hs

File: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`

Change line 94:
```haskell
-- BEFORE:
witnessPlan <- edgeWitnessPlan schemeGen (eprSuppressWeaken plan) n1Id n1Raw finalExp

-- AFTER:
witnessPlan <- edgeWitnessPlan schemeGen n1Id n1Raw finalExp
```

### 4e: Remove `eprSuppressWeaken` from EdgePlan

File: `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs`

Remove `eprSuppressWeaken :: Bool` field from `EdgePlanResolved` (line 56).
Remove from `mkEmptyResolvedPlan` (line 82).

File: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`

Remove `suppressWeaken` computation (line 44) and field assignment (line 98).
Remove `cAnnEdges` usage if this was the only consumer — check first.

### 4f: Update EdgePlannerSpec

File: `test/Presolution/EdgePlannerSpec.hs`

- Remove `eprSuppressWeaken plan \`shouldBe\` False` assertion (line 95)
- Update the annotation-edge test (line 158): remove `eprSuppressWeaken plan \`shouldBe\` True`
  assertion, or replace with a test that verifies annotation edges still produce correct witnesses

**Verification:** Test from Task 2c now passes. `cabal build` compiles. Run targeted test:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan"'`

---

## Task 5: Fix Omega Regressions

**Goal:** Extend Omega to handle the extra OpWeaken operations that were previously suppressed.

Run `cabal test` and categorize failures into buckets:

### 5a: Bucket — OpWeaken targets non-binder node

If Omega errors with "OpWeaken targets non-binder node" for previously-suppressed weakens:
- The binder is not in the quantifier spine (e.g., gen-bound arg case)
- **Fix:** In the standalone `OpWeaken` case (Omega.hs:907-926), add a graceful skip
  when the binder is not in `binderKeys` but IS in the interior set. This means the
  binder was consumed by a prior OpGraft and is no longer a quantifier binder — the
  weaken is semantically redundant.

### 5b: Bucket — OpGraft+OpWeaken binder not found in quantifier spine

If the fused pattern errors with "binder not found in quantifier spine":
- The binder was already eliminated by a prior operation
- **Fix:** Skip the fused pair when the binder is no longer in the identity list.
  Fall through to standalone OpGraft + standalone OpWeaken handling.

### 5c: Bucket — keepBinderKeys coverage for annotation edges

If annotation-edge elaboration produces wrong types (binders eliminated that should be kept):
- `keepBinderKeys` may not cover all annotation binders
- **Fix:** Verify `computeTargetBinderKeys` in `Translate.hs` correctly identifies all
  annotation-edge target binders. Extend if needed.

### 5d: Bucket — Type state mismatch after extra weaken

If `applyInst "OpWeaken"` fails because the type doesn't have the expected quantifier:
- The weaken targets a binder that was already consumed
- **Fix:** Add a guard in the standalone OpWeaken case: if the binder is not in the
  current quantifier spine, skip silently (the binder was already eliminated).

**Verification:** All existing tests pass. New tests from Task 2 pass.

---

## Task 6: Full Verification

**Goal:** Confirm all gates are green.

1. `cabal build all && cabal test` — all examples pass, 0 failures
2. `./scripts/check-thesis-claims.sh` — PASS
3. `./scripts/thesis-conformance-gate.sh` — PASS

**Verification:** All three commands green. Record new example count.

---

## Task 7: Retire Deviation

**Goal:** Remove DEV-PHI-WITNESS-WEAKEN-SUPPRESSION from the deviation register.

### 7a: Remove from thesis-deviations.yaml

File: `docs/thesis-deviations.yaml`

Delete the `DEV-PHI-WITNESS-WEAKEN-SUPPRESSION` entry (lines 91-110).

### 7b: Remove from thesis-claims.yaml

File: `docs/thesis-claims.yaml`

Remove `DEV-PHI-WITNESS-WEAKEN-SUPPRESSION` from the `deviations` list under
`CLM-PHI-CORRECTNESS` (line 45). Keep the other two deviations.

### 7c: Update stale deviation count

File: `CLAUDE.md` or any docs referencing "7 deviations" — update count to 6.

### 7d: Update CHANGELOG.md

Add entry under `## Unreleased`:
```
* Eliminated DEV-PHI-WITNESS-WEAKEN-SUPPRESSION: witness emission now always emits
  OpWeaken for unbounded binders (thesis-exact Def. 15.3.4); removed `suppressWeaken`
  and `argIsGenBound` guards from `Witness.hs`, removed `dropWeakenOps` from annotation
  edge processing, extended Omega translation to handle previously-suppressed weakens;
  deviation register 7 → 6 entries.
```

### 7e: Update implementation_notes.md

Add dated entry documenting the change.

**Verification:**
- `./scripts/check-thesis-claims.sh` — PASS (no orphan deviations, cross-links valid)
- `cabal build all && cabal test` — PASS

---

## Task 8: Final Gate

**Goal:** Full end-to-end verification.

1. `cabal build all && cabal test` — record final example count
2. `./scripts/check-thesis-claims.sh` — PASS
3. `./scripts/thesis-conformance-gate.sh` — PASS
4. Verify deviation count in docs matches register

**Verification:** All gates green. Deviation retired.

---

## Risk Mitigation

- **TDD approach:** Failing tests written before any production changes (Task 2)
- **Incremental removal:** Inner suppression (Task 3) removed before outer (Task 4)
- **Bucket-driven fixes:** Omega regressions categorized and fixed per-bucket (Task 5)
- **Rollback point:** Each task is independently committable; revert to last green if stuck

## Files Changed (Summary)

| File | Nature |
|------|--------|
| `src/MLF/Constraint/Presolution/Witness.hs` | Remove guards, simplify stepper, delete `argIsGenBound` |
| `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs` | Remove `dropWeakenOps`, remove `suppressWeaken` param |
| `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs` | Remove `eprSuppressWeaken` field |
| `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs` | Remove `suppressWeaken` computation |
| `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs` | Update `edgeWitnessPlan` call |
| `src/MLF/Elab/Phi/Omega.hs` | Extend OpWeaken handling for new cases |
| `test/Presolution/WitnessSpec.hs` | Add thesis-exact tests, update existing |
| `test/Presolution/EdgePlannerSpec.hs` | Update annotation-edge test |
| `test/ElaborationSpec.hs` | Possible end-to-end test updates |
| `docs/thesis-deviations.yaml` | Retire deviation |
| `docs/thesis-claims.yaml` | Remove deviation link |
| `CHANGELOG.md` | Add entry |
| `implementation_notes.md` | Add entry |
