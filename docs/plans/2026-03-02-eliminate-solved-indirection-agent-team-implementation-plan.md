# Eliminate Solved Indirection Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Remove production dependence on `Solved` as the elaboration/planning boundary by introducing `PresolutionView` and migrating all consumers to presolution-backed queries, while preserving thesis-aligned behavior and test parity.

**Architecture:** Keep query semantics stable by building a `PresolutionView` with the same read-only capability shape currently used from `Solved` (`canonical`, `lookupNode`, `lookupVarBound`, bind-parent access, canonical/original constraints). Migrate call sites in clustered waves (Phi/Omega, Reify/Generalize, Pipeline/Run/ResultType, Presolution/Plan), then remove legacy solved-builder/replay wiring and shrink `MLF.Constraint.Solved` to compatibility/test-only surface.

**Tech Stack:** Haskell (`cabal`, `hspec`), core modules under `src/MLF/Constraint/*`, `src/MLF/Elab/*`, `src/MLF/Reify/*`, and regression suites in `test/*`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@subagent-driven-development`, `@verification-before-completion`.

---

## Parallel Work Topology

| Team | Ownership | Parallel Window | Notes |
|---|---|---|---|
| Team A (Core View) | `PresolutionView` type + constructors + equivalence tests | Wave 0 (sequential prerequisite) | Must land before consumer migration |
| Team B (Phi/Omega) | `src/MLF/Elab/Phi/*` migration | Wave 1 (parallel) | Heaviest call-site count |
| Team C (Reify/Gen) | `src/MLF/Reify/*`, `src/MLF/Elab/Run/Generalize*` | Wave 1 (parallel) | Sensitive `canonicalConstraint` usage |
| Team D (Run/Pipeline) | `src/MLF/Elab/Run/*`, `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Pipeline*` | Wave 1 (parallel) | Removes solved-builder path |
| Team E (Plan + Cleanup) | `src/MLF/Constraint/Presolution/Plan*`, `Solved` reduction, docs/changelog | Wave 2 (after Waves 0-1) | Handles closure/signature breakage |

## Merge Gates

1. **Gate A (after Wave 0):** `cabal test mlf2-test --test-options='--match "Solved|Presolution|Pipeline"'`
2. **Gate B (after Wave 1):** `cabal test mlf2-test --test-options='--match "Phi|IdentityBridge|Generalize|ResultType|Pipeline"'`
3. **Gate C (final):** `cabal build all && cabal test`

---

### Task 1: Baseline and Migration Guard Rails

**Agent Owner:** Team A

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/Constraint/SolvedSpec.hs`
- Test: `test/PipelineSpec.hs`, `test/Constraint/SolvedSpec.hs`

**Step 1: Write the failing test**

Add red guards that encode migration intent:

```haskell
it "pipeline path can construct elaboration inputs without solved builder" $ do
  src <- readFile "src/MLF/Elab/Run/Pipeline.hs"
  src `shouldSatisfy` (not . isInfixOf "runPipelineElabWithSolvedBuilder")
```

```haskell
it "presolution view parity: canonical lookup agrees with solved on snapshot fixtures" $ do
  -- fixture scaffolding; initially fails until PresolutionView is added
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "without solved builder|presolution view parity"'`
Expected: FAIL (missing `PresolutionView`, legacy builder still present).

**Step 3: Write minimal implementation**

Add placeholder test scaffolding/helpers only; do not refactor production modules yet.

**Step 4: Run test to verify red state is stable**

Run: same command as Step 2.
Expected: deterministic FAIL at intended assertions.

**Step 5: Commit**

```bash
git add test/PipelineSpec.hs test/Constraint/SolvedSpec.hs
git commit -m "test: add solved-indirection migration guards"
```

### Task 2: Introduce `PresolutionView` Type and Constructors

**Agent Owner:** Team A

**Files:**
- Create: `src/MLF/Constraint/Presolution/View.hs`
- Modify: `src/MLF/Constraint/Presolution.hs`
- Modify: `src/MLF/Constraint/Types.hs` (re-export if needed)
- Modify: `mlf2.cabal`
- Test: `test/Constraint/SolvedSpec.hs` (or new `test/Constraint/PresolutionViewSpec.hs`)

**Step 1: Write the failing test**

Add parity checks on snapshot fixtures:

```haskell
it "PresolutionView mirrors solved canonical/node/bound queries" $ do
  -- run presolution fixture
  -- compare `pvCanonical`, `pvLookupNode`, `pvLookupVarBound` against Solved
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "PresolutionView mirrors solved"'`
Expected: FAIL (module/symbol not found).

**Step 3: Write minimal implementation**

Create `PresolutionView` record and constructor from presolution snapshot data:

```haskell
data PresolutionView = PresolutionView
  { pvConstraint :: Constraint
  , pvCanonicalMap :: IntMap NodeId
  , pvCanonical :: NodeId -> NodeId
  , pvLookupNode :: NodeId -> Maybe TyNode
  , pvLookupVarBound :: NodeId -> Maybe NodeId
  , pvLookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
  , pvBindParents :: BindParents
  , pvCanonicalConstraint :: Constraint
  }
```

Implement `fromPresolutionResult :: PresolutionResult -> Either SolveError PresolutionView` using `prConstraint` + `prUnionFind` and no replay solve.

**Step 4: Run test to verify it passes**

Run: same command as Step 2.
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/View.hs src/MLF/Constraint/Presolution.hs src/MLF/Constraint/Types.hs mlf2.cabal test/Constraint/SolvedSpec.hs
git commit -m "feat: add presolution view abstraction backed by presolution snapshot"
```

### Task 3: Migrate `ElabEnv` Boundary (`eeSolved` -> `eePresolutionView`)

**Agent Owner:** Team D

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/Phi/Env.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add a guard test:

```haskell
it "ElabEnv uses PresolutionView boundary" $ do
  src <- readFile "src/MLF/Elab/Elaborate.hs"
  src `shouldSatisfy` (isInfixOf "eePresolutionView")
  src `shouldSatisfy` (not . isInfixOf "eeSolved ::")
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "ElabEnv uses PresolutionView boundary"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

Change environment and constructors:

```haskell
data ElabEnv = ElabEnv
  { eePresolutionView :: PresolutionView
  , ...
  }
```

Thread `PresolutionView` from pipeline setup into elaboration entrypoints.

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ElabEnv uses PresolutionView boundary"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Elaborate.hs src/MLF/Elab/Phi/Env.hs src/MLF/Elab/Run/Pipeline.hs test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "refactor: switch elaboration env to presolution view"
```

### Task 4: Phi/Omega Cluster Migration

**Agent Owner:** Team B

**Files:**
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `src/MLF/Elab/Phi/Context.hs`
- Modify: `src/MLF/Elab/Phi/Translate.hs`
- Modify: `src/MLF/Elab/Phi/IdentityBridge.hs`
- Modify: `src/MLF/Elab/Phi/TestOnly.hs`
- Test: `test/Phi/IdentityBridgeSpec.hs`, `test/ElaborationSpec.hs`, `test/PhiSoundnessSpec.hs`

**Step 1: Write the failing test**

Add cluster guard:

```haskell
it "Phi modules no longer import MLF.Constraint.Solved directly" $ do
  -- scan src/MLF/Elab/Phi/*.hs for forbidden import
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi modules no longer import MLF.Constraint.Solved directly"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

Swap solved queries to view queries (`pvLookupNode`, `pvLookupBindParent`, `pvLookupVarBound`, `pvBindParents`, `pvConstraint`, `pvCanonical`).

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phi alignment|IdentityBridge|Phi soundness"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "OpRaise|OpWeaken|OpGraft"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Phi/Omega.hs src/MLF/Elab/Phi/Context.hs src/MLF/Elab/Phi/Translate.hs src/MLF/Elab/Phi/IdentityBridge.hs src/MLF/Elab/Phi/TestOnly.hs test/Phi/IdentityBridgeSpec.hs test/ElaborationSpec.hs test/PhiSoundnessSpec.hs
git commit -m "refactor: migrate phi/omega stack to presolution view queries"
```

### Task 5: Reify + Generalize Cluster Migration

**Agent Owner:** Team C

**Files:**
- Modify: `src/MLF/Reify/Core.hs`
- Modify: `src/MLF/Reify/TypeOps.hs`
- Modify: `src/MLF/Elab/Run/Generalize.hs`
- Modify: `src/MLF/Elab/Run/Generalize/Constraint.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs` (type boundary if needed)
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add guard for `Generalize` canonical dependency migration:

```haskell
it "generalize env derives canonical constraint from PresolutionView" $ do
  src <- readFile "src/MLF/Elab/Run/Generalize.hs"
  src `shouldSatisfy` (isInfixOf "pvCanonicalConstraint")
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalize env derives canonical constraint from PresolutionView"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

Replace `Solved` inputs with `PresolutionView` (or minimal view capability records) in Reify/Generalize call chains.

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Generalize shadow comparator|selectSolvedOrderWithShadow"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Paper alignment baselines"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Reify/Core.hs src/MLF/Reify/TypeOps.hs src/MLF/Elab/Run/Generalize.hs src/MLF/Elab/Run/Generalize/Constraint.hs src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "refactor: migrate reify and generalize to presolution view"
```

### Task 6: Pipeline/Run/ResultType Migration and Builder Removal

**Agent Owner:** Team D

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `src/MLF/Elab/Run/ResultType.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/Scope.hs`
- Modify: `src/MLF/Elab/Run/TypeOps.hs`
- Test: `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add two guard assertions:

```haskell
it "pipeline no longer defines buildSolvedFromPresolutionSnapshot" $ ...
it "result-type context no longer stores rtcSolved" $ ...
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "no longer defines buildSolvedFromPresolutionSnapshot|no longer stores rtcSolved"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

- Remove `SolvedBuilder` and `buildSolvedFromPresolutionSnapshot` path.
- Build `PresolutionView` directly from presolution output and thread through `ElabEnv`/result-type flows.
- Remove `solveResultFromSnapshot` replay in pipeline.

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline \(Phases 1-5\)|Phase 6 — Elaborate|D2: elaboration path is read-only on Solved"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/ResultType/Types.hs src/MLF/Elab/Run/ResultType/Ann.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Scope.hs src/MLF/Elab/Run/TypeOps.hs test/PipelineSpec.hs test/ElaborationSpec.hs
git commit -m "refactor: remove solved builder path and run pipeline on presolution view"
```

### Task 7: Presolution/Plan Closure Migration (`prPlanBuilder`)

**Agent Owner:** Team E

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Base.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan/Context.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan/BinderPlan/Types.hs`
- Modify: `src/MLF/Constraint/Presolution/Driver.hs`
- Test: `test/PresolutionSpec.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add boundary guard:

```haskell
it "PresolutionPlanBuilder closes over PresolutionView, not Solved" $ do
  src <- readFile "src/MLF/Constraint/Presolution/Base.hs"
  src `shouldSatisfy` (isInfixOf ":: PresolutionView ->")
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "PresolutionPlanBuilder closes over PresolutionView"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

Change plan-builder signature from `Solved -> ...` to `PresolutionView -> ...`, then migrate `buildGeneralizePlans` environment setup to consume `pvConstraint`/`pvCanonicalMap` derived canonical data.

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 — Principal Presolution|Presolution witness ops|Paper alignment baselines"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/Base.hs src/MLF/Constraint/Presolution/Plan.hs src/MLF/Constraint/Presolution/Plan/Context.hs src/MLF/Constraint/Presolution/Plan/BinderPlan/Types.hs src/MLF/Constraint/Presolution/Driver.hs test/PresolutionSpec.hs test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "refactor: migrate presolution plan builder closure to presolution view"
```

### Task 8: Reduce `MLF.Constraint.Solved` to Compatibility/Test Surface

**Agent Owner:** Team E

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`
- Modify: `src/MLF/Elab/Legacy.hs` (if compatibility path still needed)
- Modify: `test/Constraint/SolvedSpec.hs`
- Modify: `mlf2.cabal`
- Test: `test/Constraint/SolvedSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add API-surface guard:

```haskell
it "Solved production-only builders are absent" $ do
  src <- readFile "src/MLF/Constraint/Solved.hs"
  src `shouldSatisfy` (not . isInfixOf "fromPresolutionResult")
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Solved production-only builders are absent"'`
Expected: FAIL.

**Step 3: Write minimal implementation**

Either:
- move remaining `Solved` constructors/builders behind test-only modules, or
- keep a minimal compatibility shim with explicit deprecation comments and no production call sites.

**Step 4: Run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "MLF.Constraint.Solved|Frozen parity artifact baseline"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Solved.hs src/MLF/Elab/Legacy.hs test/Constraint/SolvedSpec.hs mlf2.cabal test/PipelineSpec.hs
git commit -m "cleanup: reduce solved module to compatibility and test surface"
```

### Task 9: Dead Export Cleanup, Docs, and Changelog

**Agent Owner:** Team E + Integrator

**Files:**
- Modify: `src/MLF/Elab/Run.hs`
- Modify: `src/MLF/Elab/Pipeline.hs`
- Modify: `src-public/MLF/API.hs`
- Modify: `src-public/MLF/Pipeline.hs`
- Modify: `implementation_notes.md`
- Modify: `TODO.md`
- Modify: `CHANGELOG.md`
- Modify: `tasks/todo/2026-03-02-eliminate-solved-indirection/{task_plan.md,findings.md,progress.md}`

**Step 1: Write the failing test**

Add one source-level hygiene guard:

```haskell
it "production src tree has no MLF.Constraint.Solved imports in elaboration path" $ ...
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "no MLF.Constraint.Solved imports in elaboration path"'`
Expected: FAIL until final cleanup is complete.

**Step 3: Write minimal implementation**

Remove stale exports/imports and update docs:
- thesis-alignment note for presolution-view boundary.
- changelog entry for solved-indirection removal.
- TODO next steps for any remaining compatibility deletion.

**Step 4: Run full verification**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run.hs src/MLF/Elab/Pipeline.hs src-public/MLF/API.hs src-public/MLF/Pipeline.hs implementation_notes.md TODO.md CHANGELOG.md tasks/todo/2026-03-02-eliminate-solved-indirection/task_plan.md tasks/todo/2026-03-02-eliminate-solved-indirection/findings.md tasks/todo/2026-03-02-eliminate-solved-indirection/progress.md test/PipelineSpec.hs
git commit -m "docs: finalize solved-indirection removal and cleanup exports"
```

---

## Orchestration Notes (Parallel Work Execution)

1. Wave 0 (Team A) must merge first: Tasks 1-2.
2. Wave 1 runs in parallel after rebase on Wave 0:
   - Team B: Task 4
   - Team C: Task 5
   - Team D: Tasks 3 and 6
3. Wave 2 starts only after Wave 1 green merge: Tasks 7-9.
4. Integrator runs merge gates A/B/C at each wave boundary and resolves conflicts in `Elab` shared files (`Elaborate.hs`, `Run/Pipeline.hs`, `Run/ResultType/*`).

## Risk Register (Implementation-Time Checks)

- `prPlanBuilder` closure signature change is global; do it once in Task 7 to avoid piecemeal breakage.
- `Omega.hs` is the largest consumer; keep behavior locked with targeted `OpRaise/OpWeaken/OpGraft` test slices before rebases.
- `canonicalConstraint` users in `Generalize`/`Reify` should move to explicit `pvConstraint` + `pvCanonical` derivation to avoid hidden replay assumptions.
- Any fallback dependence on `Solved.rebuildWithNodes` in result-type path should be replaced with explicit override structures, not reconstructed solved snapshots.
