# TMT Elaboration Input Thesis-Exact Remediation Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Make the `Elaboration input` row thesis-exact by removing solved-typed compatibility from the active elaboration/Phi runtime path while preserving checked-authoritative behavior.

**Architecture:** Keep the runtime boundary rooted in `PresolutionView (χp)` plus per-edge artifacts, then migrate active callback/query contracts to `χp`-native types end-to-end. Use strict source guards first (RED) so residual solved-compat paths cannot hide behind stale assertions. Land the migration through wave-based agent teams with merge gates after each wave.

**Tech Stack:** Haskell (`cabal`, `hspec`), modules under `src/MLF/Elab/*`, `src/MLF/Reify/*`, and tests in `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`.

---

## Thesis Contract (Row Target)

- Primary anchor: `papers/these-finale-english.txt` §15.3.5 Def. 15.3.12 and §15.3.6.
- Implementation interpretation for this row:
  - Active elaboration input path is `χp`-native.
  - Active elaboration/Phi callback signatures are not solved-typed.
  - `elaborateWithEnv` does not materialize `chiSolvedCompat`/`chiSolved`.

## Current Gaps To Close

1. `src/MLF/Elab/Elaborate.hs` still materializes `solvedCompat = ChiQuery.chiSolvedCompat presolutionView`.
2. `src/MLF/Elab/Phi/Translate.hs` still exposes solved-typed compat callback aliases and takes `Solved` in active entrypoint signature.
3. Guard tests do not currently fail on those exact gaps.

## Parallel Work Topology

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | Strengthen guards + characterization (RED first) | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`chi-surface`) | Add/align `χp`-native query/helper surfaces | `src/MLF/Elab/Run/ChiQuery.hs`, `src/MLF/Reify/Core.hs`, `src/MLF/Elab/Run/Generalize.hs` |
| Team C (`phi-signature`) | Remove solved-typed compat from active Phi signatures | `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Phi/TestOnly.hs` |
| Team D (`elab-wire`) | Migrate Elaborate + runtime callsites to new contracts | `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs` |
| Team E (`verifier`) | Full verification + docs/task closeout | `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`, task trackers |

### Wave Sequencing

- Wave 0 (serial): Task 1.
- Wave 1 (parallel): Task 2 (Team B) + Task 3 (Team C).
- Wave 2 (serial): Task 4 (Team D).
- Wave 3 (serial): Task 5 (Team E).

## Merge Gates

1. Gate A (post-Wave 0):
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- Expected: RED (non-zero failures) on current branch.

2. Gate B (post-Wave 1 integration):
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- Expected: compile + targeted slices near-green; row guards may remain RED until Wave 2 wiring lands.

3. Gate C (post-Wave 2):
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- Expected: PASS.

4. Final Gate:
- `cabal build all && cabal test`
- Expected: PASS.

---

### Task 1: Harden Row Guards (RED First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add failing source guards for real gaps**
- Add a guard that fails if `src/MLF/Elab/Elaborate.hs` contains `chiSolvedCompat presolutionView`.
- Add a guard that fails if active Phi callback aliases include solved-typed compat signatures (match `GeneralizeAtWithCompat` and solved-typed `phiFromEdgeWitnessWithTrace` args).

**Step 2: Run guard slice to confirm RED**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
Expected: FAIL with the new strict guards.

**Step 3: Re-run characterization baseline**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`
Expected: PASS.

**Step 4: Commit**
```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: harden elaboration-input thesis-exact guards for solved-compat path"
```

### Task 2: Build χp-Native Query Surfaces For Active Elab/Phi Needs

**Agent Owner:** Team B (`chi-surface`)

**Files:**
- Modify: `src/MLF/Elab/Run/ChiQuery.hs`
- Modify: `src/MLF/Reify/Core.hs`
- Modify: `src/MLF/Elab/Run/Generalize.hs`

**Step 1: Add χp-native helper entrypoints (minimal)**
- Introduce `PresolutionView`-native wrappers for active operations currently needing `Solved` (named-node discovery/reify/generalization support), without changing semantics.

**Step 2: Keep backward-compat wrappers isolated**
- Keep legacy solved-typed helpers as thin wrappers for non-active callsites only.

**Step 3: Targeted verification**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
Expected: PASS or compile-clean for Team-B-owned modules.

**Step 4: Commit**
```bash
git add src/MLF/Elab/Run/ChiQuery.hs src/MLF/Reify/Core.hs src/MLF/Elab/Run/Generalize.hs
git commit -m "refactor: add chi-native helper surfaces for active elaboration input"
```

### Task 3: Remove Solved-Typed Compat From Active Phi Signatures

**Agent Owner:** Team C (`phi-signature`)

**Files:**
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi.hs`
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`

**Step 1: Switch active callback alias to χp-native shape**
- Make active exported entrypoint signature use `GeneralizeAtWith` without `Solved` argument.
- Keep any legacy no-trace/testing adapters explicit and non-active.

**Step 2: Keep fail-fast trace contract unchanged**
- Preserve missing-trace fail-fast behavior and existing witness-domain checks.

**Step 3: Targeted verification**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
Expected: PASS for Phi slice.

**Step 4: Commit**
```bash
git add src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi.hs src/MLF/Elab/Phi/TestOnly.hs
git commit -m "refactor: migrate active phi callback contracts to chi-native signatures"
```

### Task 4: Rewire Elaborate + Runtime Call Paths To χp-Native Contracts

**Agent Owner:** Team D (`elab-wire`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`

**Step 1: Remove active solved materialization in `elaborateWithEnv`**
- Delete `solvedCompat = ChiQuery.chiSolvedCompat presolutionView` from active path.
- Route all active generalize/reify/Phi calls through the new χp-native helpers from Task 2.

**Step 2: Update callsites to new Phi/generalize contracts**
- Update pipeline/result-type callsites to pass χp-native callbacks; do not reintroduce solved-typed compat at boundaries.

**Step 3: Preserve behavior contracts**
- Keep SchemeFreeVars fallback ladder behavior unchanged.
- Keep checked-authoritative type reporting unchanged.

**Step 4: Run Gate C slices**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
Expected: PASS.

**Step 5: Commit**
```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs
git commit -m "refactor: remove solved-compat from active elaboration input runtime path"
```

### Task 5: Verification + Row/Docs Closeout

**Agent Owner:** Team E (`verifier`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-03-04-tmt-elab-input-thesis-exact-remediation-plan/task_plan.md`
- Modify: `tasks/todo/2026-03-04-tmt-elab-input-thesis-exact-remediation-plan/findings.md`
- Modify: `tasks/todo/2026-03-04-tmt-elab-input-thesis-exact-remediation-plan/progress.md`
- Modify: `Bugs.md` (only if a concrete defect is found)

**Step 1: Final full verification**
Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`
Expected: PASS.

**Step 2: Update TMT row**
- Set `Elaboration input` row back to `Thesis-exact: Yes` only after verification passes.
- Cite precise updated code references.

**Step 3: Update implementation docs/changelog**
- Record active-path solved-compat retirement and guard evidence.

**Step 4: Commit**
```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-03-04-tmt-elab-input-thesis-exact-remediation-plan/task_plan.md tasks/todo/2026-03-04-tmt-elab-input-thesis-exact-remediation-plan/findings.md tasks/todo/2026-03-04-tmt-elab-input-thesis-exact-remediation-plan/progress.md
git commit -m "docs: close elaboration-input thesis-exact remediation with verification evidence"
```

---

## Parallel Dispatch Notes

- Dispatch Teams B and C in parallel only; all other tasks are serial.
- During integration, if Teams B/C touch overlapping exports, Team D owns the final contract reconciliation.
- Every team must run its task-local verification before handoff.

## Rollback / Safety Rules

- If `checked-authoritative` or `Dual-path verification` regress, stop and revert only the latest task commit in the team worktree, not unrelated branch history.
- Do not weaken fail-fast Phi trace invariants to force green tests.

