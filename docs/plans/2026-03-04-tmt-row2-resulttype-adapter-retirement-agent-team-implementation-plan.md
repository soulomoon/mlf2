# TMT Row 2 Result-Type/Elab Adapter Retirement Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Retire the remaining row-2 compatibility adapters (`rtcSolvedCompat`, `rtcSolveLike`, and `ElabConfig.ecSolved`) while preserving checked-authoritative output and thesis-faithful `χp`-first runtime behavior.

**Architecture:** Move row-2 boundary logic to `PresolutionView`/`ChiQuery`-first query surfaces and remove solved-compat fields/functions from result-type context construction. Keep any unavoidable solved materialization explicit, centralized, and adapter-local (never hidden in pipeline/elaboration entry config fields). Stage changes by guard-first TDD waves to keep parity constraints stable.

**Tech Stack:** Haskell (`cabal`, `hspec`), elaboration/result-type runtime modules under `src/MLF/Elab/*`, regression coverage in `test/PipelineSpec.hs` + `test/ElaborationSpec.hs`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`, `[$dispatching-parallel-agents](/Users/ares/.codex/superpowers/skills/dispatching-parallel-agents/SKILL.md)`.

---

## Team Topology (Parallel Work)

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | Row-2 guard tests + parity characterization | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`resulttype-view`) | Remove `rtcSolvedCompat` / `rtcSolveLike` from result-type boundary | `src/MLF/Elab/Run/ResultType/Types.hs`, `src/MLF/Elab/Run/ResultType/View.hs` |
| Team C (`elab-phi`) | Remove `ElabConfig.ecSolved` and keep elaboration/Phi behavior stable | `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/ChiQuery.hs`, `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/TestOnly.hs` |
| Team D (`pipeline-wire`) | Wire runtime call sites to new row-2 contracts | `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Util.hs` |
| Team E (`verifier`) | Integration verification + docs/task closeout | docs + verification + task tracking files |

## Wave Plan

- **Wave 0 (serial):** Task 1.
- **Wave 1 (parallel):** Task 2 (Team B) + Task 3 (Team C).
- **Wave 2 (serial):** Task 4 (Team D + Team A), then Task 5 (Team C + D).
- **Wave 3 (serial):** Task 6 (Team E).

## Merge Gates

1. **Gate A (post-Wave 0):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`
2. **Gate B (post-Wave 1):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 ResultType"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
3. **Gate C (post-Wave 2):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
4. **Final Gate:**
   - `cabal build all && cabal test`

> If any `--match` filter returns `0 examples`, run the gate's listed narrow commands exactly as fallback evidence; do not advance wave boundaries without non-empty evidence.

---

### Task 1: Add Row-2 Closeout Guards (Red First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Write failing characterization tests**

Add source-level guards that intentionally fail until row-2 retirement is done:
- `ResultTypeInputs` no longer exposes `rtcSolvedCompat`.
- `MLF.Elab.Run.ResultType.Types` no longer exposes `rtcSolveLike`.
- `ElabConfig` no longer includes `ecSolved`.

Add parity characterization:
- representative corpus remains checked-authoritative (`assertCheckedAuthoritative`).

**Step 2: Run and confirm red state**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`

Expected: FAIL on new source guards.

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`

Expected: PASS baseline parity characterization.

**Step 3: Commit red tests**

```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: add row2 closeout guards for result-type and elab adapters"
```

### Task 2: Remove `rtcSolvedCompat` / `rtcSolveLike` from Result-Type Boundary

**Agent Owner:** Team B (`resulttype-view`)

**Files:**
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs`
- Modify: `src/MLF/Elab/Run/ResultType/View.hs`

**Step 1: Confirm red guard(s)**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`

Expected: FAIL on `rtcSolvedCompat` / `rtcSolveLike` guards.

**Step 2: Implement minimal boundary refactor**

- Remove `rtcSolvedCompat` field from `ResultTypeInputs`.
- Remove `rtcSolveLike` and its export.
- Refactor `ResultTypeView` construction and overlay handling to be `χp`-native:
  - no solved-compat field/function dependency at construction time;
  - derive node/bound/bind-parent/gen-root reads from `PresolutionView`/canonical constraint + overlay map;
  - keep behavior equivalent for downstream `Ann`/`Fallback`.

**Step 3: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 ResultType"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Run/ResultType/Types.hs src/MLF/Elab/Run/ResultType/View.hs
git commit -m "refactor: retire rtcSolvedCompat and rtcSolveLike from result-type boundary"
```

### Task 3: Remove `ElabConfig.ecSolved` Compatibility Handle

**Agent Owner:** Team C (`elab-phi`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/ChiQuery.hs`
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`

**Step 1: Confirm red guard(s)**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`

Expected: FAIL on `ecSolved` guard.

**Step 2: Implement minimal API/adapter change**

- Remove `ecSolved` from `ElabConfig`.
- Keep `elaborateWithEnv` behavior stable by sourcing any required solved-domain operations from explicit `χp`-rooted adapters (centralized helper path, not config field plumbing).
- Update Phi/generalize callback plumbing if signature alignment is required; preserve fallback ladder semantics (`SchemeFreeVars` and `reifyType` fallback behavior).

**Step 3: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/ChiQuery.hs src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi/TestOnly.hs
git commit -m "refactor: remove ElabConfig ecSolved compatibility handle"
```

### Task 4: Wire Pipeline/Result-Type Call Sites to Row-2 Contracts

**Agent Owner:** Team D (`pipeline-wire`) with Team A support for test fixture updates

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Util.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Confirm red compile/test state**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 ResultType"'`

Expected: FAIL/compile-break until call sites are aligned.

**Step 2: Implement minimal wiring changes**

- Remove solved-compat argument plumbing from `mkResultTypeInputs` and pipeline construction.
- Update `Ann`/`Fallback`/`Util` call paths to the new row-2 view contracts.
- Update test fixtures (`ResultTypeInputs` builders) to match new structure.
- Keep checked-authoritative output policy unchanged.

**Step 3: Run integration slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 ResultType"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Util.hs test/ElaborationSpec.hs
git commit -m "refactor: wire row2 result-type contracts through pipeline and callsites"
```

### Task 5: Row-2 Cleanup Sweep + Gate C

**Agent Owner:** Team C + Team D

**Files:**
- Modify: any remaining row-2 files from Teams C/D if stragglers remain
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add/refresh hard guards**

Ensure source guards are explicit and green for:
- no `rtcSolvedCompat`;
- no `rtcSolveLike`;
- no `ElabConfig.ecSolved`.

**Step 2: Run Gate C**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`

Expected: PASS.

**Step 3: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Types.hs src/MLF/Elab/Run/ResultType/View.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Util.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: complete row2 adapter retirement in result-type and elab config"
```

### Task 6: Verification + Documentation + Task Closeout

**Agent Owner:** Team E (`verifier`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `historical task-tracker path (not retained as a live folder)`
- Modify: `historical task-tracker path (not retained as a live folder)`
- Modify: `historical task-tracker path (not retained as a live folder)`
- Modify: `Bugs.md` (only if a real defect is discovered)

**Step 1: Run final verification gates**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`

Expected: PASS.

**Step 2: Update docs + task evidence**

- Mark row-2 adapter retirement state in TMT notes.
- Update implementation notes/changelog with exact verification evidence.
- Update TODO next-step ordering for any post-row2 cleanup.
- Update task tracker files with per-wave evidence.
- Update `Bugs.md` only if a concrete defect was found.

**Step 3: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md historical task-tracker path (not retained as a live folder) historical task-tracker path (not retained as a live folder) historical task-tracker path (not retained as a live folder) Bugs.md
git commit -m "docs: record row2 adapter retirement and verification evidence"
```

---

## Definition of Done

- `ResultTypeInputs` no longer carries `rtcSolvedCompat`.
- `MLF.Elab.Run.ResultType.Types` no longer defines `rtcSolveLike`.
- `ElabConfig` no longer carries `ecSolved`.
- Row-2 runtime behavior remains checked-authoritative on representative corpus slices.
- Gate A/B/C and final gate (`cabal build all && cabal test`) are all green with non-empty evidence.
- TMT/implementation/changelog/TODO/task tracking docs are updated with concrete verification output.
