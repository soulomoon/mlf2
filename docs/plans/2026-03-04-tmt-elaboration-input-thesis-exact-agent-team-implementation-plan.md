# TMT Elaboration Input Thesis-Exact Agent-Team Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the **Elaboration input** mechanism thesis-exact by removing remaining internal solved-adapter dependency from the elaboration input path (while preserving checked-authoritative runtime behavior).

**Architecture:** Keep the elaboration boundary rooted in translatable presolution `χp` and edge artifacts (already in place), then migrate internal elaboration/generalize/reify call chains from `Solved`-typed plumbing to `PresolutionView`/`ChiQuery`-native query surfaces. Maintain behavior parity through guard-first TDD and checked-authoritative corpus slices.

**Tech Stack:** Haskell (`cabal`, `hspec`), elaboration/runtime modules under `src/MLF/Elab/*`, reify/generalize helpers under `src/MLF/Reify/*` and `src/MLF/Elab/Run/*`, regression slices in `test/PipelineSpec.hs` and `test/ElaborationSpec.hs`.

**Thesis anchors:** `papers/these-finale-english.txt` §15.3.5 Def. 15.3.12, §15.3.6 (elaboration from translatable presolution `χp` with per-edge witness-driven translation before term elaboration).

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`, `[$dispatching-parallel-agents](/Users/ares/.codex/superpowers/skills/dispatching-parallel-agents/SKILL.md)`.

---

## Team Topology (Agent Teams)

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | Thesis-exact guardrails + parity characterization | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`chi-generalize`) | χp-native generalize/reify/scope helper surfaces | `src/MLF/Elab/Run/Generalize.hs`, `src/MLF/Elab/Run/Scope.hs`, `src/MLF/Elab/Run/TypeOps.hs`, `src/MLF/Reify/Core.hs` |
| Team C (`elab-phi`) | Elaborate/Phi signature migration off solved-input adapter | `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/ChiQuery.hs`, `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi/TestOnly.hs` |
| Team D (`resulttype-wire`) | Result-type/pipeline callsite alignment to χp-native helper contracts | `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Util.hs` |
| Team E (`verifier`) | Integration verification + docs/task closeout | docs + task trackers + final verification |

## Wave Plan

- **Wave 0 (serial):** Task 1.
- **Wave 1 (parallel):** Task 2 (Team B) + Task 3 (Team C).
- **Wave 2 (serial):** Task 4 then Task 5.
- **Wave 3 (serial):** Task 6.

## Merge Gates

1. **Gate A (post-Wave 0):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`
2. **Gate B (post-Wave 1):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
3. **Gate C (post-Wave 2):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
4. **Final Gate:**
   - `cabal build all && cabal test`

> If any `--match` filter returns `0 examples`, run non-empty fallback matchers for the same gate before advancing:
> - fallback A/B: `--match "row2 closeout guard|Phase 6 — Elaborate|checked-authoritative"`
> - fallback C: `--match "Pipeline (Phases 1-5)|Dual-path verification|checked-authoritative"`

---

### Task 1: Add Elaboration-Input Thesis-Exact Guards (Red First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Write failing guards/characterization tests**

Add source-level guards under matcher text `elab-input thesis-exact guard` that fail until migration is complete:
- Elaborate path no longer depends on `ChiQuery.chiSolved` in `MLF.Elab.Elaborate`.
- Elaborate/Phi callback signatures in active runtime path do not require a `Solved` parameter for generalize-at entry callbacks.
- Keep representative checked-authoritative characterization in place (`assertCheckedAuthoritative`).

**Step 2: Run and confirm RED/PASS baseline**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: FAIL on new source guards.

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative keeps representative corpus parity"'`

Expected: PASS baseline parity characterization.

**Step 3: Commit red tests**

```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: add elaboration-input thesis-exact closeout guards"
```

### Task 2: Introduce χp-Native Helper Surfaces for Generalize/Reify/Scope

**Agent Owner:** Team B (`chi-generalize`)

**Files:**
- Modify: `src/MLF/Elab/Run/Generalize.hs`
- Modify: `src/MLF/Elab/Run/Scope.hs`
- Modify: `src/MLF/Elab/Run/TypeOps.hs`
- Modify: `src/MLF/Reify/Core.hs`

**Step 1: Confirm guard RED**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: FAIL.

**Step 2: Implement minimal χp-native helper migration**

- Add/extend view-native helper APIs so elaboration callers can avoid mandatory `Solved` handles in active path.
- Preserve legacy wrappers only where needed for compatibility call sites outside the row scope.
- Keep semantics unchanged for:
  - scope resolution (`ga′`-aligned behavior),
  - generalization fallback ladder (`SchemeFreeVars` + reify fallback),
  - annotation simplification/inlining behavior.

**Step 3: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: still RED overall until Task 3, but Team-B-owned guard slices should be green.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Run/Generalize.hs src/MLF/Elab/Run/Scope.hs src/MLF/Elab/Run/TypeOps.hs src/MLF/Reify/Core.hs
git commit -m "refactor: add chi-native helper surfaces for elaboration input path"
```

### Task 3: Remove Internal `chiSolved` Dependency from Elaborate Input Path

**Agent Owner:** Team C (`elab-phi`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/ChiQuery.hs`
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`

**Step 1: Confirm guard RED**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: FAIL on Elaborate/Phi signature/usage guards.

**Step 2: Implement minimal API/signature migration**

- Remove elaboration-path reliance on `solved = ChiQuery.chiSolved presolutionView` in `elaborateWithEnv`.
- Migrate `GeneralizeAtWith` runtime callback shape in active elaboration/Phi path to χp-native inputs.
- Keep term-shape translation behavior and fallback semantics unchanged.
- Keep fail-fast edge-trace requirements unchanged.

**Step 3: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/ChiQuery.hs src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi/TestOnly.hs
git commit -m "refactor: remove chiSolved dependency from elaboration input path"
```

### Task 4: Align Pipeline/ResultType Call Paths to New Helper Contracts

**Agent Owner:** Team D (`resulttype-wire`)

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Util.hs`

**Step 1: Confirm red/compile-break alignment state**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`

Expected: fail/compile-break until call sites are aligned.

**Step 2: Implement minimal wiring changes**

- Remove solved-only callback compatibility parameters where χp-native contract now applies.
- Keep result-type reconstruction behavior unchanged and checked-authoritative output policy intact.
- Preserve GA mapping behavior used by result-type guard-rail tests.

**Step 3: Run targeted slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Util.hs
git commit -m "refactor: align pipeline and result-type call paths to chi-native elaboration input"
```

### Task 5: Cleanup Sweep + Gate C

**Agent Owner:** Teams C + D

**Files:**
- Modify: any remaining files from Tasks 2–4 (scope-limited)
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Harden final guards**

Ensure green source guards for this row:
- no elaboration-path `ChiQuery.chiSolved` dependency in `MLF.Elab.Elaborate`;
- no solved-typed generalize callback requirement in the active elaboration/Phi path.

**Step 2: Run Gate C**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline (Phases 1-5)"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`

Expected: PASS.

**Step 3: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/ChiQuery.hs src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi/TestOnly.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Util.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: complete thesis-exact elaboration input migration"
```

### Task 6: Verification + TMT Row/Docs + Task Closeout

**Agent Owner:** Team E (`verifier`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team/task_plan.md`
- Modify: `tasks/todo/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team/findings.md`
- Modify: `tasks/todo/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team/progress.md`
- Modify: `Bugs.md` (only if concrete defect discovered)

**Step 1: Final verification gates**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`

Expected: PASS.

**Step 2: Update docs + tracker evidence**

- Update TMT **Elaboration input** row to `Thesis-exact: Yes` with precise code references.
- Record any intentional residual deviations (if any) in `implementation_notes.md`.
- Update changelog/TODO and task tracker files with concrete gate output.
- Update `Bugs.md` only for concrete defects discovered during execution.

**Step 3: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team/task_plan.md tasks/todo/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team/findings.md tasks/todo/2026-03-04-tmt-elaboration-input-thesis-exact-agent-team/progress.md Bugs.md
git commit -m "docs: record thesis-exact elaboration input closeout evidence"
```

---

## Definition of Done

- Elaboration input runtime path is thesis-exact against §15.3.5/§15.3.6:
  - active elaboration boundary remains `χp` + witness artifacts,
  - active elaboration/Phi generalize callback path no longer requires solved-typed adapter plumbing,
  - no internal `chiSolved` dependency remains in `MLF.Elab.Elaborate`.
- Checked-authoritative output policy remains unchanged on representative slices.
- Gate A/B/C and final gate are green with non-empty evidence (fallback commands used when any matcher is empty).
- TMT row, implementation notes, changelog, TODO, and task tracker files are updated with concrete evidence.
