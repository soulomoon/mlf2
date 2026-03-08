# TMT Row 1 χp-First Elaboration/Generalization Closeout Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Complete the Transformation Mechanism Table row-1 migration by moving solved-typed elaboration/generalization helper reads to direct `χp` (`PresolutionView`) queries, then remove `eeSolvedCompat` and entry-time `Solved.rebuildWithConstraint` without changing checked-authoritative output.

**Architecture:** Keep production behavior single-path and checked-authoritative. Introduce a view-first generalization/query contract, migrate helpers/call sites in waves, and keep any unavoidable solved-only behavior behind explicit boundary adapters (not hidden in `ElabEnv` entry reconstruction). Remove `eeSolvedCompat` and `elaborateWithEnv` entry-time rebuild only after characterization + parity gates are green.

**Tech Stack:** Haskell (`cabal`, `hspec`), modules in `src/MLF/Elab/*` and tests in `test/*Spec.hs`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`, `[$dispatching-parallel-agents](/Users/ares/.codex/superpowers/skills/dispatching-parallel-agents/SKILL.md)`.

---

## Team Topology (Parallel Work)

| Team | Scope | File Ownership |
|---|---|---|
| Team A (`contracts`) | Red/green migration guards and parity characterization | `test/PipelineSpec.hs`, `test/ElaborationSpec.hs` |
| Team B (`generalize-api`) | View-first generalization API and adapters | `src/MLF/Elab/Run/Generalize.hs`, `src/MLF/Elab/Run/ResultType/Util.hs` |
| Team C (`elab-core`) | Elaborate/Scope χp-first helper migration + `ElabEnv` cleanup | `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/Scope.hs`, `src/MLF/Elab/Run/ChiQuery.hs` |
| Team D (`pipeline-wire`) | Pipeline and result-type wiring to new contracts | `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Types.hs`, `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs` |
| Team E (`verifier`) | Cross-team integration review, docs, and full gates | docs + targeted/full verification |

## Wave Plan

- **Wave 0 (serial):** Task 1 (red guards + parity characterization).
- **Wave 1 (parallel):** Task 2 (Team B) + Task 3 (Team C).
- **Wave 2 (serial):** Task 4 (Team C + D), then Task 5 (Team D).
- **Wave 3 (serial):** Task 6 (Team E, with Team D support).

## Merge Gates

1. **Gate A (post-Wave 0):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first guard|checked-authoritative keeps representative corpus parity|row1 closeout guard"'`
2. **Gate B (post-Wave 1):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate|ResultType|chi-first"'`
3. **Gate C (post-Wave 2):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline \(Phases 1-5\)|Dual-path verification|checked-authoritative"'`
4. **Final Gate:**
   - `cabal build all && cabal test`

---

### Task 1: Add Row-1 Closeout Guards (Red First)

**Agent Owner:** Team A (`contracts`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Write failing characterization tests**

Add source-level guards that intentionally fail until migration is complete:
- `ElabEnv` no longer includes `eeSolvedCompat`.
- `MLF.Elab.Elaborate` no longer contains entry-time `Solved.rebuildWithConstraint` in `elaborateWithEnv`.

Add runtime parity characterization:
- representative corpus remains checked-authoritative (`assertCheckedAuthoritative`).

**Step 2: Run and confirm red state**

Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 closeout guard|checked-authoritative keeps representative corpus parity"'`

Expected: FAIL on new source guards, PASS on parity baseline.

**Step 3: Commit red tests**

```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: add row1 closeout guards for eeSolvedCompat and entry rebuild"
```

### Task 2: Introduce View-First Generalization Entry Points

**Agent Owner:** Team B (`generalize-api`)

**Files:**
- Modify: `src/MLF/Elab/Run/Generalize.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Util.hs`

**Step 1: Write failing tests/guards**

Add focused tests that fail until generalization can run from `PresolutionView` without a solved-only call signature in helper layers.

**Step 2: Implement minimal API additions**

- Add a view-first helper (example shape):
  - `generalizeAtWithBuilderView :: PresolutionPlanBuilder -> Maybe GaBindParents -> PresolutionView -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)`
- Keep compatibility wrapper temporarily:
  - existing solved-typed `generalizeAtWithBuilder` delegates via `fromSolved`.
- Update `generalizeWithPlan` to accept view-first inputs for primary path; keep explicit fallback behavior unchanged.

**Step 3: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalizeWithPlan|Phase 6 — Elaborate|ResultType"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Run/Generalize.hs src/MLF/Elab/Run/ResultType/Util.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: add view-first generalization helper entry points"
```

### Task 3: Migrate Elaborate/Scope Helper Reads to χp-First Queries

**Agent Owner:** Team C (`elab-core`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/Scope.hs`
- Modify: `src/MLF/Elab/Run/ChiQuery.hs`

**Step 1: Write failing tests/guards**

Add/extend guards that fail while elaboration helper reads still prefer direct `Solved.lookup*` in locations covered by this row.

**Step 2: Implement χp-first helper migration**

- Introduce/expand `ChiQuery` helper coverage needed by elaboration and scope-target helpers.
- Switch helper reads in `Elaborate`/`Scope` to `PresolutionView`-based query helpers where equivalent.
- Preserve behavior of fallback ladders (`SchemeFreeVars` and `reifyType` fallback) exactly.

**Step 3: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first Elaborate|Phase 6"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Scope.hs src/MLF/Elab/Run/ChiQuery.hs test/ElaborationSpec.hs
git commit -m "refactor: migrate elaborate/scope helper reads to chi-first queries"
```

### Task 4: Remove `eeSolvedCompat` and Entry-Time `Solved.rebuildWithConstraint`

**Agent Owner:** Team C (`elab-core`) + Team D (`pipeline-wire`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs` (only if signature alignment requires)

**Step 1: Remove compatibility field and entry rebuild**

- Remove `eeSolvedCompat` from `ElabEnv`.
- Remove `Solved.rebuildWithConstraint` entry reconstruction in `elaborateWithEnv`.
- Keep any remaining solved-only behavior behind explicit boundary wiring, not hidden entry-time rebuild.

**Step 2: Compile + targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 closeout guard|chi-first guard|Phase 6 — Elaborate"'`

Expected: PASS.

**Step 3: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Types.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: remove eeSolvedCompat and elaborate entry-time solved rebuild"
```

### Task 5: Migrate Remaining Generalization Call Sites to View-First Contracts

**Agent Owner:** Team D (`pipeline-wire`)

**Files:**
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/ResultType.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs`

**Step 1: Replace solved-typed helper calls where row-1 relevant**

- Route generalization helper call sites through view-first APIs.
- Keep checked-authoritative output policy unchanged.
- Avoid broad row-2 scope creep: only touch result-type code as required for row-1 contract removal.

**Step 2: Run integration slices**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ResultType|Pipeline \(Phases 1-5\)|Dual-path verification"'`

Expected: PASS.

**Step 3: Commit**

```bash
git add src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/Pipeline.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: migrate row1-relevant generalization callsites to chi-first contracts"
```

### Task 6: Verification + Documentation + TMT Status Update

**Agent Owner:** Team E (`verifier`) with Team D support

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `Bugs.md` (only if new defect discovered)

**Step 1: Run final gates**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "row1 closeout guard|checked-authoritative|Dual-path verification"'`
- `cabal build all && cabal test`

Expected: PASS.

**Step 2: Update docs with evidence**

- Document final row-1 runtime shape and remaining explicit adapters (if any).
- Record verification evidence and any intentional deviation notes.
- Update TODO next-step ordering for row-2 follow-up (`rtcSolvedCompat`/`rtcSolveLike`) if still outstanding.

**Step 3: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md Bugs.md
git commit -m "docs: record row1 chi-first closeout and verification evidence"
```

---

## Definition of Done

- `ElabEnv` no longer contains `eeSolvedCompat`.
- `elaborateWithEnv` no longer performs entry-time `Solved.rebuildWithConstraint`.
- Row-1 relevant elaboration/generalization helper reads are χp-first (`PresolutionView` / `ChiQuery`).
- Checked-authoritative representative corpus remains unchanged.
- Full verification passes: `cabal build all && cabal test`.
- TMT row-1 docs and changelog are updated with evidence.
