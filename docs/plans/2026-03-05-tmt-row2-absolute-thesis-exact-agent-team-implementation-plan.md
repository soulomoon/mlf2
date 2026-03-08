# TMT Row 2 Result-Type Context Wiring Absolute Thesis-Exact Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Make the Transformation Mechanism Table row `Result-type context wiring` absolute thesis-exact by removing ResultType-local solved-overlay materialization and finishing `χp`-native query wiring end-to-end.

**Architecture:** The thesis-side contract for subterm typing/result typing is defined over translated artifacts from the translatable presolution (`Def. 15.3.2`, `§15.3.2.2`, `§15.3.6`). The runtime row2 boundary already removed `rtcSolvedCompat`/`rtcSolveLike`, but `MLF.Elab.Run.ResultType.View` still reconstructs/rebuilds `Solved` (`rtvSolved`, `rtvOriginalConstraint`, `solveFromInputs`) and consumers still depend on that surface. This plan removes those residual surfaces and migrates Ann/Fallback/generalize/reify call chains to explicit `PresolutionView` + `ChiQuery`/view helpers while preserving checked-authoritative parity.

**Tech Stack:** Haskell (`cabal`, `hspec`), runtime elaboration modules under `src/MLF/Elab/Run/ResultType/*`, scope helpers in `src/MLF/Elab/Run/Scope.hs`, reify helpers in `src/MLF/Reify/Core.hs`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`, `@dispatching-parallel-agents`.

---

## Team Topology (Parallel Work)

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | RED->GREEN source guards + parity characterization | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`view-core`) | Remove ResultType local `Solved` overlay/materialization | `src/MLF/Elab/Run/ResultType/View.hs`, `src/MLF/Elab/Run/Scope.hs` |
| Team C (`resulttype-consumers`) | Migrate Ann/Fallback to view-native scope/reify/generalize calls | `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Util.hs` |
| Team D (`integration-wire`) | Compile-fix wiring and non-behavioral cleanups after B/C | `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Types.hs` |
| Team E (`verifier-docs`) | Gate execution, TMT/docs/TODO/task closeout | docs + task tracker files |

### Wave Order

- **Wave 0 (serial):** Task 1 (Team A)
- **Wave 1 (parallel):** Task 2 (Team B) + Task 3 (Team C)
- **Wave 2 (serial):** Task 4 (Team D integration pass)
- **Wave 3 (serial):** Task 5 (Team E verification gates)
- **Wave 4 (serial):** Task 6 (Team E docs/ledger closeout)

## Merge Gates

1. **Gate A (RED baseline):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
   - Expected: FAIL before Wave 1.
2. **Gate B (row2 surfaces retired):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
   - Expected: PASS.
3. **Gate C (behavior parity):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
   - Expected: PASS.
4. **Final Gate:**
   - `cabal build all && cabal test`

> If a regex matcher returns `0 examples`, run the narrow fallback matchers individually and record non-empty evidence before advancing the wave.

---

### Task 1: Add Row2 Absolute Guard (RED First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add strict source guards**

Add `row2 absolute thesis-exact guard` assertions that fail while residual surfaces remain:
- `src/MLF/Elab/Run/ResultType/View.hs` contains none of:
  - `rtvSolved ::`
  - `rtvOriginalConstraint ::`
  - `solveFromInputs ::`
  - `Solved.rebuildWithConstraint`
- `src/MLF/Elab/Run/ResultType/Ann.hs` contains no `View.rtvSolved`.
- `src/MLF/Elab/Run/ResultType/Fallback.hs` contains no `View.rtvSolved`.

**Step 2: Confirm RED baseline**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`

Expected: FAIL.

**Step 3: Keep parity characterization in place**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`

Expected: PASS baseline.

**Step 4: Commit**

```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: add row2 absolute thesis-exact RED guard"
```

### Task 2: Retire ResultType Local `Solved` Overlay

**Agent Owner:** Team B (`view-core`)

**Files:**
- Modify: `src/MLF/Elab/Run/ResultType/View.hs`
- Modify: `src/MLF/Elab/Run/Scope.hs`

**Step 1: Remove residual solved-backed view surfaces**

In `ResultType.View`:
- Remove exports/definitions for `rtvSolved`, `rtvOriginalConstraint`, and `solveFromInputs`.
- Stop importing/constructing `Solved` in this module.
- Keep `ResultTypeView` as `(inputs + bound overlay)` and serve read APIs via `rtcPresolutionView`/`ChiQuery` + overlay.

**Step 2: Preserve strict malformed-view failure semantics without local solved materialization**

- Add/route a view-level strict validation path so malformed canonical constraints still fail fast (preserve current regression expectation containing `Residual unification edge`).
- Keep failure shape in `Either ElabError` at `buildResultTypeView` boundary.

**Step 3: Use scope view helpers only**

- Ensure row2 call sites can use `schemeBodyTargetView`, `resolveCanonicalScopeView`, and `canonicalizeScopeRefView` directly from `PresolutionView`.

**Step 4: Run focused tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ResultType|Phase 6 — Elaborate|chi-first gate stays green"'`

Expected: first may still fail until Task 3; second should stay PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/ResultType/View.hs src/MLF/Elab/Run/Scope.hs
git commit -m "refactor: retire row2 ResultType solved-overlay view surfaces"
```

### Task 3: Migrate ResultType Consumers to View-Native APIs

**Agent Owner:** Team C (`resulttype-consumers`)

**Files:**
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Util.hs`

**Step 1: Remove direct `rtvSolved` dependencies**

- Replace `View.rtvSolved`-dependent flows with `PresolutionView`/view helpers.
- Replace solved-only scope calls with view variants:
  - `resolveCanonicalScopeView`
  - `schemeBodyTargetView`
  - `canonicalizeScopeRefView`

**Step 2: Move reify/generalize fallbacks to view-oriented helpers**

- In `ResultType.Util`, remove `Solved` requirement from `generalizeWithPlanView` fallback path by using `reifyTypeFromView`.
- In Ann/Fallback, switch to `namedNodesFromView` and `reifyTypeWithNamedSetNoFallbackFromView` where applicable.

**Step 3: Preserve behavior and keep normalization paths intact**

- Keep existing instantiation/normalization logic unchanged except for dependency migration.
- Do not change algorithmic behavior of annotation handling; only dependency boundary is in scope.

**Step 4: Run focused tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "result-type reconstruction fails on malformed PresolutionView materialization"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Util.hs
git commit -m "refactor: migrate row2 result-type consumers to presolution-view APIs"
```

### Task 4: Integration Wiring Pass

**Agent Owner:** Team D (`integration-wire`)

**Files:**
- Modify: `src/MLF/Elab/Run/ResultType.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs`

**Step 1: Reconcile signatures and imports after B/C**

- Remove dead imports/aliases left by `Solved` removal.
- Ensure `mkResultTypeInputs` and pipeline call chains stay `χp`-native and compile-clean.

**Step 2: Run integration slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`

Expected: PASS.

**Step 3: Commit**

```bash
git add src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Types.hs
git commit -m "refactor: integrate row2 absolute thesis-exact result-type wiring"
```

### Task 5: Verification Gates

**Agent Owner:** Team E (`verifier-docs`)

**Step 1: Execute Gate B/C and full gate in order**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 absolute thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row2 closeout guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`

Expected: PASS.

**Step 2: If any gate fails, log defect and stop wave advance**

- Add or update `Bugs.md` entry with reproducer and expected vs actual.
- Do not mark row2 absolute closeout as complete until all gates are green.

### Task 6: Docs and Ledger Closeout (Post-Green Only)

**Agent Owner:** Team E (`verifier-docs`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team/task_plan.md`
- Modify: `tasks/todo/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team/findings.md`
- Modify: `tasks/todo/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team/progress.md`
- Modify: `Bugs.md` (only if defects were discovered)

**Step 1: Update TMT row2 narrative after green gates**

- Revise row2 `Current codebase`/`What to change` text to reflect completed absolute hardening (if achieved).
- If any residual non-thesis-exact surfaces remain, keep `Thesis-exact` narrative conservative and list exact follow-ups.

**Step 2: Update implementation notes/changelog/TODO with concrete gate evidence**

- Record command outputs (`examples/failures`) for all required gates.
- Update next priorities only after row2 absolute status is proven by gates.

**Step 3: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team/task_plan.md tasks/todo/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team/findings.md tasks/todo/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team/progress.md Bugs.md
git commit -m "docs: close row2 absolute thesis-exact hardening evidence"
```

---

## Definition of Done

- `ResultType.View` no longer exposes local solved-overlay/materialization surfaces (`rtvSolved`, `rtvOriginalConstraint`, `solveFromInputs`).
- ResultType Ann/Fallback/Util no longer depend on `View.rtvSolved`.
- `row2 absolute thesis-exact guard` is green and non-empty.
- Existing row2/phase parity slices remain green: `row2 closeout guard`, `checked-authoritative`, `Dual-path verification`.
- Full validation gate is green: `cabal build all && cabal test`.
- TMT/docs/task artifacts are updated with concrete evidence.
