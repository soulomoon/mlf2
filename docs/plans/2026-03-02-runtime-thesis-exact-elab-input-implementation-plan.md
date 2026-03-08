# Runtime Thesis-Exact Elaboration Input (TMT Row 1) Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Make the elaboration input boundary thesis-exact by removing runtime mediation/reconstruction semantics and using presolution-authoritative inputs for elaboration.

**Architecture:** Keep the current single runtime path, but refactor the pipeline→elaboration boundary to a thesis-core input contract. Add temporary equivalence checks while migrating from solved-snapshot mediation, then remove mediation code once evidence is stable. Preserve strict trace-required Φ translation and checked-authoritative pipeline behavior.

**Tech Stack:** Haskell (Cabal + Hspec), `src/MLF/Elab/*`, `src/MLF/Constraint/*`, `test/*Spec.hs`.

## 2026-03-03 Strict Completion Checklist (5-item closeout)

1. [x] Remove row-1 pipeline direct replay mediation calls (`Solved.fromPreRewriteState`, `solveResultFromSnapshot`, `setSolvedConstraint`) from `Pipeline.hs` boundary assembly.
   Evidence:
   - `src/MLF/Elab/Run/Pipeline.hs` no longer calls replay APIs directly and now routes boundary finalization via dedicated helper module `src/MLF/Elab/Run/PipelineBoundary.hs`.
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 boundary uses thesis-core elaboration input contract"'` (PASS).
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 boundary validates-only and does not mediate input"'` (PASS).
2. [x] Remove result-type replay reconstruction (`rtcSolveLike -> fromPreRewriteState`) and switch to non-replay solved materialization.
   Evidence:
   - `src/MLF/Elab/Run/ResultType/Types.hs` now builds solved view via `Solved.fromConstraintAndUf` over `pvCanonicalConstraint`/`pvCanonicalMap` (no replay call).
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "migration guardrail: thesis-core boundary matches legacy outcome"'` (PASS).
3. [x] Complete elaboration boundary wiring on presolution-authoritative inputs (remove `ecSolved` boundary injection).
   Evidence:
   - `src/MLF/Elab/Elaborate.hs` `ElabConfig` no longer includes `ecSolved`.
   - `elaborateWithEnv` materializes internal solved state from `eePresolutionView`.
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborateWithEnv consumes thesis-core input"'` (PASS).
4. [x] Add exact plan-checklist tests and make them executable via `--match`.
   Evidence (all PASS):
   - `row1 boundary uses thesis-core elaboration input contract`
   - `elaborateWithEnv consumes thesis-core input`
   - `row1 boundary validates-only and does not mediate input`
   - `migration guardrail: thesis-core boundary matches legacy outcome`
   - `final row1 state uses single thesis-core boundary path`
   - `Dual-path verification`
5. [x] Docs/gate closeout: update docs and run final gate (`cabal build all && cabal test`).
   Evidence:
   - Updated closeout docs: `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`.
   - Targeted gates: `Phase 6 — Elaborate` (PASS), `Pipeline (Phases 1-5)` (PASS), `Dual-path verification` (PASS).
   - Full gate: `cabal build all && cabal test` (PASS).

---

### Task 1: Add a failing boundary contract test for thesis-core elaboration input

**Files:**
- Modify: `test/PipelineSpec.hs`
- Test: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add a focused test describing the new boundary contract (pipeline passes thesis-core elaboration input without boundary reconstruction):

```haskell
it "row1 boundary uses thesis-core elaboration input contract" $ do
  -- characterize current boundary shape and assert no reconstruction path is used
  pendingWith "red test placeholder until boundary contract refactor lands"
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 boundary uses thesis-core elaboration input contract"'`
Expected: FAIL (pending/failing characterization).

**Step 3: Write minimal implementation**

Implement only enough test plumbing/helpers in `PipelineSpec` to express the boundary assertion concretely (no production code yet).

**Step 4: Run test to verify it still fails for the right reason**

Run the same command.
Expected: FAIL due to current boundary behavior, not placeholder setup.

**Step 5: Commit**

```bash
git add test/PipelineSpec.hs
git commit -m "test: add red contract for thesis-core elaboration boundary"
```

### Task 2: Introduce thesis-core elaboration input type and wire ElabEnv to it

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `test/ElaborationSpec.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add/adjust a direct elaboration call test to require the new input contract in `ElaborationSpec`.

```haskell
it "elaborateWithEnv consumes thesis-core input" $ do
  -- direct call using target contract shape
  -- should fail to compile/runtime until contract migration is done
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaborateWithEnv consumes thesis-core input"'`
Expected: FAIL (signature/contract mismatch).

**Step 3: Write minimal implementation**

Refactor `ElabEnv`/entry wiring so elaboration consumes thesis-core input fields (presolution-authoritative solved/evidence inputs) without adding new fallback channels.

**Step 4: Run test to verify it passes**

Run the same command.
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Pipeline.hs test/ElaborationSpec.hs
git commit -m "refactor: wire elaboration to thesis-core input contract"
```

### Task 3: Remove row-1 boundary mediation/reconstruction semantics in pipeline

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Constraint/Solved.hs` (only if helper exposure is required)
- Test: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add a red test asserting no row-1 mediation/reprojection/reconstruction at pipeline boundary.

```haskell
it "row1 boundary validates-only and does not mediate input" $ do
  -- characterize absence of reconstruction path in elaboration handoff
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 boundary validates-only and does not mediate input"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

Update pipeline boundary assembly to validate/fail-fast only; remove mediation behavior at elaboration handoff.

**Step 4: Run test to verify it passes**

Run the same command.
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs src/MLF/Constraint/Solved.hs test/PipelineSpec.hs
git commit -m "refactor: remove row1 elaboration boundary mediation semantics"
```

### Task 4: Add temporary equivalence guardrails for migration safety

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs` (temporary instrumentation/hooks only if needed)
- Test: `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add equivalence characterization test(s): old and new boundary outcomes must agree on selected expressions.

```haskell
it "migration guardrail: thesis-core boundary matches legacy outcome" $ do
  -- compare outcomes for representative terms
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "migration guardrail: thesis-core boundary matches legacy outcome"'`
Expected: FAIL before equivalence is fully ensured.

**Step 3: Write minimal implementation**

Add minimal migration guardrails and fix mismatches in boundary wiring only (no algorithm redesign).

**Step 4: Run targeted tests to verify pass**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "migration guardrail: thesis-core boundary matches legacy outcome"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline \(Phases 1-5\)"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: add migration equivalence guardrails for row1 boundary"
```

### Task 5: Lock strict invariants and delete temporary migration scaffolding

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Test: `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add a test asserting final state: no temporary migration branch remains and strict boundary invariants hold.

```haskell
it "final row1 state uses single thesis-core boundary path" $ do
  -- assert only final path is active
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "final row1 state uses single thesis-core boundary path"'`
Expected: FAIL while temporary migration scaffolding still exists.

**Step 3: Write minimal implementation**

Remove temporary equivalence scaffolding; keep final single boundary path + invariant checks.

**Step 4: Run tests to verify pass**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "final row1 state uses single thesis-core boundary path"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: remove migration scaffolding and lock final row1 boundary"
```

### Task 6: Update docs + run full verification gate

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Optional Modify: `historical task-tracker path (not retained as a live folder)`

**Step 1: Write the failing check**

Add a local checklist in the task notes requiring all validation commands to pass before row-1 reclassification.

**Step 2: Run targeted checks**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline \(Phases 1-5\)"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`

Expected: PASS.

**Step 3: Write minimal doc updates**

- Update transformation table row 1 wording to reflect final runtime boundary semantics.
- Record migration decisions and evidence references in `implementation_notes.md`.
- Add concise changelog entry.

**Step 4: Run full gate**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 5: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md historical task-tracker path (not retained as a live folder)
git commit -m "docs: record row1 thesis-exact boundary migration and verification evidence"
```

---

## Definition of Done

- Pipeline→elaboration boundary is thesis-core and presolution-authoritative for row 1.
- No boundary mediation/reconstruction semantics remain for elaboration input.
- Strict trace-required Φ translation behavior remains intact.
- Migration equivalence was demonstrated before deleting temporary scaffolding.
- Targeted regression slices and full gate pass:
  - `Phase 6 — Elaborate`
  - `Pipeline (Phases 1-5)`
  - `Dual-path verification`
  - `cabal build all && cabal test`
- Transformation table/docs are updated with evidence-backed row-1 status.
