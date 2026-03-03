# χp-First Elaboration/ResultType Internal Cleanup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Reduce solved materialization/query dependence inside elaboration and result-type internals, and prefer direct `χp` (`PresolutionView`) queries where behavior allows.

**Architecture:** Keep production pipeline behavior and checked-authoritative output unchanged. Move solved reconstruction/materialization out of internal modules (`Elaborate`, `Run/ResultType/*`) and into explicit boundary/adapters only when required by legacy helper APIs. Introduce a shared χp query facade, migrate call sites in waves, and enforce guard tests so internals do not regress back to ad hoc solved reconstruction.

**Tech Stack:** Haskell (`cabal`, `hspec`), modules under `src/MLF/Elab/*` and `src/MLF/Constraint/Presolution/*`, tests in `test/*Spec.hs`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`.

---

## Agent Team Topology (tmux)

| Team | Ownership | Files | Parallel Window |
|---|---|---|---|
| Team A (`query-layer`) | Shared χp query facade + guard tests | `src/MLF/Elab/Run/ChiQuery.hs`, `test/PipelineSpec.hs`, `mlf2.cabal` | Wave 0 |
| Team B (`resulttype`) | Result-type internals migration to χp-first queries | `src/MLF/Elab/Run/ResultType/*` | Wave 1 |
| Team C (`elab-core`) | `Elaborate` migration to χp-first queries | `src/MLF/Elab/Elaborate.hs` | Wave 1 |
| Team D (`integrator`) | Pipeline boundary wiring + integration + docs/changelog | `src/MLF/Elab/Run/Pipeline.hs`, docs | Wave 2 |
| Team E (`reviewer`) | Spec/quality review + regression gate execution | `test/*`, docs review | All waves |

## Merge Gates

1. **Gate A (after Wave 0):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first guard"'`
2. **Gate B (after Wave 1):**
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ResultType|Phase 6 — Elaborate|chi-first"'`
3. **Gate C (final):**
   - `cabal build all && cabal test`

---

### Task 1: Add Red Guard Rails for Internal Solved Materialization

**Agent Owner:** Team A (`query-layer`)

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Test: `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add migration-guard tests that initially fail while internals still materialize solved locally:

```haskell
it "chi-first guard: Elaborate internals avoid local solved materialization" $ do
  src <- readFile "src/MLF/Elab/Elaborate.hs"
  src `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")
```

```haskell
it "chi-first guard: ResultType internals avoid local solved materialization" $ do
  src <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
  src `shouldSatisfy` (not . isInfixOf "Solved.fromConstraintAndUf")
```

**Step 2: Run test to verify it fails**

Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first guard"'`

Expected: FAIL.

**Step 3: Write minimal implementation**

Only commit test scaffolding and helper utilities required for these guard checks.

**Step 4: Run test to verify deterministic red state**

Run the same command.
Expected: deterministic FAIL on targeted assertions.

**Step 5: Commit**

```bash
git add test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "test: add chi-first guardrails for solved materialization in internals"
```

### Task 2: Introduce Shared χp Query Facade for Elab/ResultType

**Agent Owner:** Team A (`query-layer`)

**Files:**
- Create: `src/MLF/Elab/Run/ChiQuery.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs`
- Modify: `mlf2.cabal`
- Test: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add a guard asserting both internals import/use a shared χp query module:

```haskell
it "chi-first guard: internals use shared ChiQuery facade" $ do
  elabSrc <- readFile "src/MLF/Elab/Elaborate.hs"
  rtSrc <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
  elabSrc `shouldSatisfy` isInfixOf "MLF.Elab.Run.ChiQuery"
  rtSrc `shouldSatisfy` isInfixOf "MLF.Elab.Run.ChiQuery"
```

**Step 2: Run test to verify it fails**

Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "shared ChiQuery facade"'`

Expected: FAIL.

**Step 3: Write minimal implementation**

Create facade with presolution-view backed helpers:

```haskell
module MLF.Elab.Run.ChiQuery (
  chiCanonical,
  chiLookupNode,
  chiLookupVarBound,
  chiLookupBindParent,
  chiConstraint,
  chiCanonicalConstraint,
  chiBindParents
) where
```

Use `PresolutionView` as the source of truth (`pv*` accessors).

**Step 4: Run test to verify it passes**

Run the same command.
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/ChiQuery.hs src/MLF/Elab/Run/ResultType/Types.hs mlf2.cabal test/PipelineSpec.hs
git commit -m "feat: add chi-first query facade for elaboration/result-type"
```

### Task 3: Migrate ResultType Internals to χp-First Queries

**Agent Owner:** Team B (`resulttype`)

**Files:**
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs`
- Modify: `src/MLF/Elab/Run/ResultType/View.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Util.hs`
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add characterization test for runtime parity while replacing internal solved queries:

```haskell
it "chi-first ResultType keeps checked-authoritative behavior on representative corpus" $ do
  forM_ representativeMigrationCorpus assertCheckedAuthoritative
```

Add source-level guard to reduce `Solved.lookup*` usage inside result-type modules.

**Step 2: Run test to verify it fails**

Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first ResultType|checked-authoritative"'`

Expected: FAIL.

**Step 3: Write minimal implementation**

- Remove direct solved reconstruction from `rtcSolveLike` usage path where possible.
- Prefer `χp` query facade (`ChiQuery`) for canonical/node/bound/bind-parent reads.
- Keep solved compatibility only in a narrow adapter when unavoidable for existing reify/generalize helper signatures.

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ResultType"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "checked-authoritative"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/ResultType/Types.hs src/MLF/Elab/Run/ResultType/View.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Util.hs test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "refactor: migrate result-type internals to chi-first query usage"
```

### Task 4: Migrate Elaborate Internals to χp-First Queries

**Agent Owner:** Team C (`elab-core`)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Run/Scope.hs` (only if required for χp helper extraction)
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add characterization test:

```haskell
it "chi-first Elaborate keeps Phase 6 behavior" $ do
  -- run existing phase-6 corpus assertions unchanged
```

Add source-level guard that `elaborateWithEnv` no longer performs local solved reconstruction.

**Step 2: Run test to verify it fails**

Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first Elaborate|Phase 6"'`

Expected: FAIL.

**Step 3: Write minimal implementation**

- Replace local lookup/canonical/bound queries with `χp` queries via `ChiQuery`.
- Remove local solved reconstruction in `elaborateWithEnv`.
- Keep a narrow solved compatibility adapter only at points where current APIs still require `Solved` (document each retained call with a `{- Note -}` rationale).

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first Elaborate"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Scope.hs test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "refactor: move elaborate internals to chi-first query usage"
```

### Task 5: Integrate Pipeline Boundary and Remove Residual Internal Scaffolding

**Agent Owner:** Team D (`integrator`)

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType.hs`
- Modify: `src/MLF/Elab/Run/Generalize.hs` (only if wiring requires)
- Test: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add integration guard:

```haskell
it "chi-first integration: pipeline run path has no internal solved materialization in Elaborate/ResultType" $ do
  -- grep-based guard over target modules
```

**Step 2: Run test to verify it fails**

Run:
`cabal test mlf2-test --test-show-details=direct --test-options='--match "chi-first integration"'`

Expected: FAIL.

**Step 3: Write minimal implementation**

- Keep solved construction confined to explicit boundary points where still required.
- Ensure internal modules use χp query facade, not ad hoc solved reconstruction.
- Preserve current checked-authoritative return behavior.

**Step 4: Run integration tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline \(Phases 1-5\)|Dual-path verification|chi-first integration"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/Generalize.hs test/PipelineSpec.hs
git commit -m "refactor: integrate chi-first elab/result-type boundary wiring"
```

### Task 6: Final Verification, Documentation, and Closeout

**Agent Owner:** Team E (`reviewer`) + Team D (`integrator`)

**Files:**
- Modify: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md` (if priority/order changes)
- Modify: `Bugs.md` (only if new bug is discovered)

**Step 1: Write failing doc checklist (if absent)**

Add a short checklist in task notes documenting required verification evidence.

**Step 2: Run full verification gate**

Run:
`cabal build all && cabal test`

Expected: PASS.

**Step 3: Apply documentation updates**

Document:
- what moved to χp-first queries,
- what solved dependencies remain and why,
- where compatibility adapters still exist.

**Step 4: Re-run key targeted slices for evidence capture**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate|ResultType|Dual-path verification"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add docs/notes/2026-02-27-transformation-mechanism-table.md implementation_notes.md CHANGELOG.md TODO.md Bugs.md
git commit -m "docs: record chi-first elab/result-type cleanup and remaining solved adapters"
```

---

## Definition of Done

- No local solved reconstruction in `src/MLF/Elab/Elaborate.hs` and `src/MLF/Elab/Run/ResultType/Types.hs`.
- Internal elaboration/result-type lookups prefer χp (`PresolutionView`) through shared query facade.
- Checked-authoritative behavior and representative corpus parity remain intact.
- Full gate passes: `cabal build all && cabal test`.
- Docs/changelog updated with retained solved adapter rationale.
