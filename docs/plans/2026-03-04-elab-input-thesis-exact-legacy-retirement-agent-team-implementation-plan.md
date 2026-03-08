# Elaboration Input Thesis-Exact Legacy Retirement Parallel Work Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Make the Transformation Mechanism Table row `Elaboration input` thesis-exact by removing remaining solved-typed elaboration/Phi APIs (including test-only surfaces) while preserving checked-authoritative behavior.

**Architecture:** Keep production elaboration/Phi strictly `χp`-native (`PresolutionView` + per-edge artifacts) and retire solved-typed compatibility entrypoints from elaboration-facing modules. Migrate tests to consume `χp`-native test helpers so strict row policy (`includes test-only code paths`) can be satisfied without runtime behavior drift.

**Tech Stack:** Haskell (`cabal`, `hspec`), `src/MLF/Elab/*`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, docs under `docs/notes/`.

---

## Thesis Contract (Row Target)

- Thesis anchor: `papers/these-finale-english.txt` Def. 15.3.12 / Fig. 15.3.5 / §15.3.6.
- Contract for `Elaboration input` thesis-exactness on this repo:
  1. Active elaboration/Phi runtime path is `χp`-native.
  2. No solved-typed elaboration/Phi compatibility APIs remain in production modules.
  3. No solved-typed elaboration/Phi compatibility APIs remain in test-only helper surfaces.
  4. Guard slices remain green: `elab-input thesis-exact guard`, `checked-authoritative`, `Dual-path verification`.

## Current Blocking Surfaces

1. `src/MLF/Elab/Elaborate.hs` still exposes `GeneralizeAtWithLegacy` and solved-typed compatibility entrypoints (`elaborate*`).
2. `src/MLF/Elab/Phi/Translate.hs` still exposes solved-typed `GeneralizeAtWithLegacy`, `phiFromEdgeWitnessNoTrace`, and deprecated `phiFromEdgeWitness`.
3. `src/MLF/Elab/Phi.hs` still re-exports legacy no-trace/deprecated helpers.
4. `src/MLF/Elab/Phi/TestOnly.hs` still exposes solved-typed callback shape in `phiFromEdgeWitnessAutoTrace`.
5. `test/ElaborationSpec.hs` still relies on solved-typed Phi test helper calls.

---

## Parallel Work Topology

| Team | Mission | File Ownership |
|---|---|---|
| Team A (`contracts`) | Strengthen guards and lock strict thesis-exact criterion | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`elab-api`) | Remove solved-typed compatibility from elaboration-facing runtime API | `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Pipeline.hs` |
| Team C (`phi-api`) | Remove solved-typed compatibility from Phi API and test-only helper surfaces | `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Phi/TestOnly.hs` |
| Team D (`tests-migration`) | Migrate tests to `χp`-native Phi helper/callback shapes | `test/ElaborationSpec.hs` |
| Team E (`closeout`) | Verify, update docs/table status, and close task artifacts | `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`, task files |

### Wave Order

- Wave 0 (serial): Task 1 (Team A).
- Wave 1 (parallel): Task 2 (Team B) + Task 3 (Team C).
- Wave 2 (serial): Task 4 (Team D) + Wave integration fixups.
- Wave 3 (serial): Task 5 (Team E) + final gates.

---

## Merge Gates

1. **Gate A (post-Wave 0)**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
   - Expected: **RED** on pre-migration branch.

2. **Gate B (post-Wave 1 integration)**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
   - Expected: compile-clean and closer to green; some test callsites may still fail before Wave 2.

3. **Gate C (post-Wave 2)**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
   - Expected: **PASS**.

4. **Final Gate**
   - `cabal build all && cabal test`
   - Expected: **PASS**.

---

### Task 1: Guard Hardening (Strict Criterion First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add failing guards for forbidden solved-typed API symbols**
- Add source guards that fail if these symbols remain exported in active modules:
  - `GeneralizeAtWithLegacy`
  - `phiFromEdgeWitnessNoTrace`
  - `phiFromEdgeWitness` (deprecated alias)
  - solved-typed callback parameter shape in `MLF.Elab.Phi.TestOnly`.

**Step 2: Confirm RED before migration**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
Expected: FAIL with newly added strict guard checks.

**Step 3: Characterize safety baseline**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
Expected: PASS.

**Step 4: Commit**
```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: harden elaboration-input thesis-exact strict legacy-API guards"
```

### Task 2: Retire Solved-Typed Elaborate API Compatibility

**Agent Owner:** Team B (`elab-api`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Pipeline.hs`

**Step 1: Remove solved-typed alias and compatibility entrypoints**
- Remove `GeneralizeAtWithLegacy` and solved-typed compatibility `elaborate*` entrypoints from `MLF.Elab.Elaborate`.
- Keep only `ElabConfig`/`ElabEnv` + `elaborateWithEnv` as active elaboration entry API.

**Step 2: Update public re-export surface**
- Ensure `MLF.Elab.Pipeline` exports only intended active elaboration API surface.
- If compatibility must be preserved for non-row callers, move it into `MLF.Elab.Legacy` (not elaboration-facing modules).

**Step 3: Build+slice check**
Run:
- `cabal build all`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
Expected: compile-clean for Team-B-owned surfaces.

**Step 4: Commit**
```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Pipeline.hs
git commit -m "refactor: retire solved-typed elaborate compatibility entrypoints"
```

### Task 3: Retire Solved-Typed Phi API/Test-Only Compatibility

**Agent Owner:** Team C (`phi-api`)

**Files:**
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi.hs`
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`

**Step 1: Remove solved-typed legacy aliases from Phi.Translate**
- Remove `GeneralizeAtWithLegacy`.
- Remove `phiFromEdgeWitnessNoTrace` and deprecated `phiFromEdgeWitness`.
- Keep `phiFromEdgeWitnessWithTrace` as the sole translation entrypoint.

**Step 2: Update Phi facade exports**
- Stop exporting removed no-trace/deprecated symbols from `MLF.Elab.Phi`.

**Step 3: Rework test-only helper to chi-native callback shape**
- Keep `phiFromEdgeWitnessAutoTrace`, but make callback shape `GeneralizeAtWith` (no `Solved` argument in callback contract).
- Keep missing-trace fail-fast semantics and witness-domain checks unchanged.

**Step 4: Build+slice check**
Run:
- `cabal build all`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
Expected: compile-clean for Phi surfaces; test callsite updates may still be pending until Task 4.

**Step 5: Commit**
```bash
git add src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi.hs src/MLF/Elab/Phi/TestOnly.hs
git commit -m "refactor: retire solved-typed phi compatibility entrypoints"
```

### Task 4: Migrate Elaboration Tests To Chi-Native Phi Test Helpers

**Agent Owner:** Team D (`tests-migration`)

**Files:**
- Modify: `test/ElaborationSpec.hs`

**Step 1: Replace solved-typed Phi helper callsites**
- Migrate callsites that currently pass solved-typed callbacks into `phiFromEdgeWitnessAutoTrace`.
- Use chi-native callback shape at each callsite.

**Step 2: Keep fixture semantics unchanged**
- Preserve existing witness fixtures and expected instantiation outcomes.
- Do not weaken fail-fast expectations around missing trace/witness evidence.

**Step 3: Run focused elaboration slices**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
Expected: PASS for migrated slices.

**Step 4: Commit**
```bash
git add test/ElaborationSpec.hs
git commit -m "test: migrate elaboration phi helpers to chi-native callback contracts"
```

### Task 5: Closeout Verification + Table/Docs Update

**Agent Owner:** Team E (`closeout`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `historical 2026-03-04 task_plan.md path (archive folder not retained)`
- Modify: `historical 2026-03-04 findings.md path (archive folder not retained)`
- Modify: `historical 2026-03-04 progress.md path (archive folder not retained)`
- Modify: `Bugs.md` only if a real defect is uncovered.

**Step 1: Run closeout gates**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`
Expected: PASS.

**Step 2: Flip row status only after green gates**
- Update TMT `Elaboration input` row to `Thesis-exact = Yes` with current file/line references.
- Keep `What to change` either `N/A` or follow-up-only work not required for row exactness.

**Step 3: Update implementation docs**
- Record retirement of solved-typed elaboration/Phi APIs and test-only helper compatibility.
- Capture verification evidence snippets.

**Step 4: Commit**
```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md historical 2026-03-04 task_plan.md path (archive folder not retained) historical 2026-03-04 findings.md path (archive folder not retained) historical 2026-03-04 progress.md path (archive folder not retained)
git commit -m "docs: close elaboration-input thesis-exact legacy-retirement with verification evidence"
```

---

## Agent Dispatch Procedure (Execution Guidance)

1. Create isolated team worktrees from the same base commit.
2. Dispatch Team A first; do not start Wave 1 until Gate A evidence is captured.
3. Dispatch Team B and Team C in parallel (independent file ownership).
4. Integrate Wave 1 results, then run Gate B.
5. Dispatch Team D after Wave 1 integration (depends on new callback contracts).
6. Run Gate C.
7. Dispatch Team E for docs/table closeout and final gate.

## Safety Rules

- Do not re-introduce solved-typed compatibility in active elaboration/Phi modules.
- Do not weaken trace fail-fast behavior to make tests pass.
- If `checked-authoritative` regresses, stop and triage before moving to doc/status updates.
- If a new defect is discovered, log it in `Bugs.md` in the same iteration.

## Success Criteria

- Zero solved-typed elaboration/Phi compatibility APIs remain in production or test-only helper surfaces.
- Guard + parity slices are green.
- TMT `Elaboration input` row is updated to `Thesis-exact = Yes` with current references.
