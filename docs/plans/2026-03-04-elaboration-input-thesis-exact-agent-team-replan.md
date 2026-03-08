# Elaboration Input Thesis-Exact Parallel Work Replan Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Make the `Elaboration input` row thesis-exact by removing the active Elaborate -> Phi solved-adapter handoff and running elaboration-edge translation from `chi`-native inputs end-to-end.

**Architecture:** Keep pipeline boundaries rooted in `PresolutionView (chi_p)` and edge artifacts. Refactor active Phi translation entrypoints to consume `PresolutionView` directly, then migrate Elaborate/ResultType callsites off `ChiQuery.chiSolved` handoff. Lock the migration with stronger source guards that match the real call chain, then prove behavior parity via checked-authoritative and dual-path suites.

**Tech Stack:** Haskell (`cabal`, `hspec`), modules under `src/MLF/Elab/*` and `src/MLF/Elab/Run/*`, tests in `test/PipelineSpec.hs` and `test/ElaborationSpec.hs`.

---

## Thesis Contract and Live Gap

- Thesis anchors (source of truth):
  - `papers/these-finale-english.txt` Def. 15.3.12
  - `papers/these-finale-english.txt` §15.3.6 / Fig. 15.3.5
- Live runtime gap (current branch):
  - Active call chain still contains solved handoff:
    - `runPipelineElabWith` -> `elaborateWithEnv` -> `reifyInst` ->
      `phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView)`
      (`src/MLF/Elab/Elaborate.hs`)
  - Active Phi trace entrypoints still require `Solved` in signatures
    (`src/MLF/Elab/Phi/Translate.hs`).
- Resulting row status target:
  - Current: `Thesis-exact = No`
  - Target after this plan: `Thesis-exact = Yes`

---

## Parallel Work Topology

| Team | Focus | Ownership (no cross-edits) |
|---|---|---|
| Team A (`guards`) | Guard hardening + red/green characterization | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`phi-core`) | Phi API migration to `PresolutionView`-native active entrypoints | `src/MLF/Elab/Phi/Translate.hs`, `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Phi/TestOnly.hs` |
| Team C (`callsites`) | Elaborate/ResultType callsite rewiring off `ChiQuery.chiSolved` | `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/Pipeline.hs` |
| Team D (`verification`) | Integration verification + bug capture + drift checks | test execution + `Bugs.md` updates when defects are found |
| Team E (`docs-closeout`) | TMT row + implementation/task docs closeout | `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`, task trackers |

### Wave schedule

- Wave 0 (serial): Task 1 (Team A)
- Wave 1 (serial): Task 2 (Team B)
- Wave 2 (parallel): Task 3A (Team C elaboration callsites) + Task 3B (Team D verification harness prep)
- Wave 3 (serial): Task 4 (Team D full gates)
- Wave 4 (serial): Task 5 (Team E docs closeout)

---

## Gate Matrix (must be non-empty)

1. Gate A (post Task 1)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- Expected: FAIL (new strict guards are red before code migration).

2. Gate B (post Task 2 + Task 3A)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`
- Expected: PASS.

3. Gate C (post Task 4)
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- Expected: PASS.

4. Final gate
- `cabal build all && cabal test`
- Expected: PASS.

If any matcher returns `0 examples`, rerun with a narrower fallback matcher and record both commands/results in progress logs.

---

### Task 1: Harden Elaboration-Input Guards (RED First)

**Agent Owner:** Team A (`guards`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Add failing guards for the real active gap**

Implement guard assertions that fail when either of the following appears in active source:

```haskell
-- PipelineSpec guard intent
isInfixOf "ChiQuery.chiSolved presolutionView" elaborateSrc `shouldBe` False
```

```haskell
-- ElaborationSpec guard intent (active entrypoint)
isInfixOf "phiFromEdgeWitnessWithTrace\n    :: TraceConfig\n    -> GeneralizeAtWith\n    -> Solved" phiSrc `shouldBe` False
```

Also keep existing row guard names so CI matchers remain stable.

**Step 2: Confirm RED**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: FAIL with at least one assertion tied to current solved handoff.

**Step 3: Commit**

```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: harden elaboration-input thesis-exact guards for active solved handoff"
```

---

### Task 2: Migrate Active Phi Entry to `chi`-Native Contract

**Agent Owner:** Team B (`phi-core`)

**Files:**
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi.hs`
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`

**Step 1: Introduce `PresolutionView`-native active entrypoint**

Refactor active Phi trace function signatures so `Solved` is not required by the active runtime path.

Target shape:

```haskell
phiFromEdgeWitnessWithTrace
  :: TraceConfig
  -> GeneralizeAtWith
  -> PresolutionView
  -> Maybe GaBindParents
  -> Maybe SchemeInfo
  -> Maybe EdgeTrace
  -> EdgeWitness
  -> Either ElabError Instantiation
```

`phiFromEdgeWitnessCore` should consume the same `PresolutionView` handle; any `Solved` compatibility wrapper must be explicitly marked legacy/test-only.

**Step 2: Preserve runtime semantics**
- Keep fail-fast behavior for missing `EdgeTrace`.
- Keep witness-domain replay/bridge invariants unchanged.

**Step 3: Run focused tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`

Expected: may still fail on unresolved callsites until Task 3A.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi.hs src/MLF/Elab/Phi/TestOnly.hs
git commit -m "refactor: make active phi trace entrypoint chi-native"
```

---

### Task 3A: Rewire Elaborate/ResultType Callsites Off `chiSolved`

**Agent Owner:** Team C (`callsites`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs` (only if type wiring requires it)

**Step 1: Remove active solved handoff from Elaborate path**

Replace the active callsite:

```haskell
phiFromEdgeWitnessWithTrace ... (ChiQuery.chiSolved presolutionView) ...
```

with the new `PresolutionView`-native argument.

**Step 2: Align ResultType callsites to the same contract**
- Update all `phiFromEdgeWitnessWithTrace` callsites to pass `PresolutionView` (or a legacy adapter only where explicitly non-active).
- Keep checked-authoritative behavior unchanged.

**Step 3: Run Gate B commands**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "elab-input thesis-exact guard"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Pipeline.hs
git commit -m "refactor: remove chiSolved handoff from active elaborate/resulttype phi callsites"
```

---

### Task 3B: Verification Harness Prep (Parallel with 3A)

**Agent Owner:** Team D (`verification`)

**Files:**
- Modify only if needed: `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`

**Step 1: Prepare fallback matcher map**
- Ensure each gate has a non-empty fallback matcher command documented in task progress.

**Step 2: Dry-run fast slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`

Expected: PASS baseline.

**Step 3: Commit only if test harness code changed**

```bash
# If files changed
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: stabilize elaboration-input gate matcher coverage"
```

---

### Task 4: Integration Verification and Defect Capture

**Agent Owner:** Team D (`verification`)

**Files:**
- Modify: `Bugs.md` only if a real defect is discovered

**Step 1: Run Gate C and Final gate**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`
- `cabal build all && cabal test`

Expected: PASS.

**Step 2: Bug protocol**
- If any behavioral defect appears, add an entry to `Bugs.md` in the same iteration with reproducer and expected vs actual behavior.

**Step 3: Commit (if bug log changed)**

```bash
git add Bugs.md
git commit -m "docs: log elaboration-input migration defect discovered during gates"
```

---

### Task 5: Docs + Row Closeout

**Agent Owner:** Team E (`docs-closeout`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `historical task-tracker path (not retained as a live folder)`
- Modify: `historical task-tracker path (not retained as a live folder)`
- Modify: `historical task-tracker path (not retained as a live folder)`

**Step 1: Update table row only after green gates**
- Set `Elaboration input` row to `Thesis-exact = Yes` only when all gates pass.
- Cite precise updated code references for the active call chain.

**Step 2: Sync implementation notes/changelog/TODO**
- Record what changed and exact verification evidence.

**Step 3: Close task tracker**
- Mark all phases complete in task plan and archive task folder when implementation is done.

**Step 4: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md historical task-tracker path (not retained as a live folder) historical task-tracker path (not retained as a live folder) historical task-tracker path (not retained as a live folder)
git commit -m "docs: close elaboration-input thesis-exact migration after agent-team verification"
```

---

## Dispatch Rules (Parallel Work)

- Use isolated worktrees per team; no two teams edit the same file.
- Team C rebases/cherry-picks Team B's API commit before Task 3A.
- Team D runs verification only on integration head, not on partial team branches.
- If Gate B fails, return ownership to the team that owns the failing file and repeat only that gate before advancing.

## Done Criteria

- Active elaboration path has no `ChiQuery.chiSolved` handoff.
- Active Phi trace entrypoint signature is `PresolutionView`-native (no `Solved` argument).
- Guard slice `elab-input thesis-exact guard` is green and non-empty.
- `checked-authoritative`, `Dual-path verification`, and full suite are green.
- TMT row `Elaboration input` updated to `Thesis-exact = Yes` with current code references.
