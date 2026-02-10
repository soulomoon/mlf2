# Master 4-Phase Typed Two-Pass Edge DSL Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a thesis-faithful, type-guided two-pass presolution edge-processing architecture where Phase 2 guarantees paper-shaped `TyExp ≤ τ` instantiation edges and Phase 4 executes only typed, legal edge plans.

**Architecture (revised 2026-02-11):** Implement in four phases using an **atomic rollout** strategy. Build the planner/interpreter infrastructure first with a dual-mode execution split (`expansion-semantics` for TyExp edges, `legacy-direct-semantics` for non-TyExp edges). Then land wrapping + fail-fast atomically once equivalence is proven. This avoids the blocker where wrapping alone reroutes edges through semantically different code paths.

**Blocker context:** Naive wrapping of all residual inst edges with TyExp breaks 7 pipeline/elaboration tests because `processInstEdge`'s TyExp branch (`decideMinimalExpansion` + `unifyStructure`) is not equivalent to the non-TyExp branch (`solveNonExpInstantiation`). The non-TyExp branch has special binding-permission and scheme-root logic. See `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md` for details.

**Tech Stack:** Haskell (GHC2021 + selected extensions for typed plans), Cabal, Hspec, existing presolution/normalization modules.

---

## Execution Notes

- This plan assumes work occurs in this dedicated worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/codex/2026-02-09-new-worktree`.
- If any test fails unexpectedly, pause and use `@superpowers:systematic-debugging` before changing logic.
- Execute in phase order; do not start later phases before earlier phase verification passes.
- **Gate tests:** The 7 known-failing pipeline cases must be used as equivalence gates before landing wrapping + fail-fast (Phase 3).

---

## Phase 1 — Typed Plan Core + Dual-Mode Planner

### Task 1: [DONE] Add failing tests for paper-shaped residual instantiation edges

**Status:** Complete. 3 tests added to `test/NormalizeSpec.hs` under `describe "Paper-shaped residual inst edges"`. These tests intentionally fail until Phase 3 lands the wrapping implementation.

### Task 2: Create typed plan module with dual-mode discriminator

**Files:**
- Create: `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs`
- Create: `test/Presolution/EdgePlannerSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Add failing compile-time usage test**

Create `test/Presolution/EdgePlannerSpec.hs` with a smoke spec importing new plan API:

```haskell
module Presolution.EdgePlannerSpec (spec) where

import Test.Hspec
import MLF.Constraint.Presolution.EdgeProcessing.Plan

spec :: Spec
spec = describe "Edge plan types" $ do
    it "exposes stage-indexed plan constructors" $ do
        edgePlanStage (mkEmptyResolvedPlan undefined) `shouldBe` StageResolved
```

Wire into `test/Main.hs` and `mlf2.cabal`.

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types"'`

Expected: FAIL (module missing).

**Step 3: Implement typed plan model with dual-mode discriminator**

In `Plan.hs`, define:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Execution mode for an edge plan.
-- ExpansionMode: left node is TyExp, use decideMinimalExpansion + unifyStructure.
-- LegacyDirectMode: left node is non-TyExp, use solveNonExpInstantiation.
data EdgePlanMode = ExpansionMode | LegacyDirectMode

data EdgeStage = StageResolved | StageExpanded | StageUnified | StageCommitted

data EdgePlan (s :: EdgeStage) where
    EdgePlanResolved ::
        { eprEdge :: InstEdge
        , eprLeftNode :: TyNode       -- raw left node (TyExp or other)
        , eprRightNode :: TyNode      -- canonical right node
        , eprMode :: EdgePlanMode     -- which execution path to use
        , eprAllowTrivial :: Bool
        , eprSuppressWeaken :: Bool
        } -> EdgePlan 'StageResolved
```

Add small helpers used by tests (`edgePlanStage`, constructor helper).

**Step 4: Run targeted test/build**

Run:
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs test/Presolution/EdgePlannerSpec.hs test/Main.hs mlf2.cabal
git commit -m "feat: add stage-indexed edge plan core types with dual-mode discriminator"
```

### Task 3: Implement planner module (pass A)

**Files:**
- Create: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `test/Presolution/EdgePlannerSpec.hs`
- Modify: `mlf2.cabal`

**Step 1: Add failing planner behavior tests**

Add tests for:
- planner returns `EdgePlan 'StageResolved` with `ExpansionMode` when left is `TyExp`
- planner returns `EdgePlan 'StageResolved` with `LegacyDirectMode` when left is non-`TyExp`
- planner threads `let`/`ann` flags into plan

**Step 2: Run tests to verify failure**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match planner'`

Expected: FAIL (planner missing).

**Step 3: Implement planner**

`planEdge :: InstEdge -> PresolutionM (EdgePlan 'StageResolved)` should:
- read canonicalized nodes and flags (`cLetEdges`, `cAnnEdges`)
- classify left node: `TyExp` → `ExpansionMode`, otherwise → `LegacyDirectMode`
- package resolved operands, mode, and per-edge flags into `EdgePlanResolved`

**Step 4: Run planner tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match planner'`
- `cabal build mlf2-test`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgePlannerSpec.hs mlf2.cabal
git commit -m "feat: add edge planner pass with dual-mode classification"
```

---

## Phase 2 — Interpreter + Integration

### Task 4: Implement interpreter module with dual execution paths (pass B)

**Files:**
- Create: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
- Create: `test/Presolution/EdgeInterpreterSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Add failing interpreter smoke tests**

Add tests validating interpreter on:
- `ExpansionMode` edge (TyExp left): records expansion/witness/trace
- `LegacyDirectMode` edge (non-TyExp left): records identity expansion + direct solve

**Step 2: Run test to confirm fail**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "executes resolved plan"'`

Expected: FAIL (interpreter missing).

**Step 3: Implement interpreter with dual paths**

Core API:

```haskell
executeEdgePlan :: EdgePlan 'StageResolved -> PresolutionM ()
executeEdgePlan plan = case eprMode plan of
    ExpansionMode -> executeExpansionPath plan
    LegacyDirectMode -> executeLegacyDirectPath plan
```

- `executeExpansionPath`: uses existing TyExp branch logic (`decideMinimalExpansion`, `mergeExpansions`, `runExpansionUnify`, `buildEdgeTrace`, `buildEdgeWitness`)
- `executeLegacyDirectPath`: uses existing non-TyExp branch logic (`solveNonExpInstantiation`, `buildEdgeTrace`, `buildEdgeWitness`)

Both paths must produce identical observable artifacts for the same input.

**Step 4: Run interpreter tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match EdgeInterpreter'`
- `cabal build mlf2-test`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs test/Presolution/EdgeInterpreterSpec.hs test/Main.hs mlf2.cabal
git commit -m "feat: add dual-path edge plan interpreter"
```

### Task 5: Refactor `processInstEdge` to two-pass plan→execute

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `test/Presolution/EdgeTraceSpec.hs`
- Modify: `test/Presolution/ExpansionSpec.hs`

**Step 1: Add failing orchestration test**

Add a test that compares current observable artifacts before/after refactor for one fixture:
- same `EdgeExpansion` assigned
- same `EdgeWitness` edge ids and roots
- same `EdgeTrace` root and binder args

**Step 2: Run test to confirm fail**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "two-pass orchestration"'`

Expected: FAIL until orchestration path is updated.

**Step 3: Implement refactor**

In `EdgeProcessing.hs`:

```haskell
processInstEdge edge = do
    requireValidBindingTree
    plan <- planEdge edge
    executeEdgePlan plan
```

The dual-mode discriminator ensures TyExp edges go through expansion-semantics and non-TyExp edges go through legacy-direct-semantics, preserving exact behavior.

**Step 4: Run full regression**

Run: `cabal build all && cabal test`

Expected: PASS (all 609 tests, with only the 3 Task 1 NormalizeSpec tests failing as expected).

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgeTraceSpec.hs test/Presolution/ExpansionSpec.hs
git commit -m "refactor: route processInstEdge through typed planner and dual-path interpreter"
```

### Task 6: Remove obsolete branch-specific helpers and tighten module boundaries

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`

**Step 1: Confirm dead-code warnings**

Run: `cabal build mlf2-test`

Expected: WARN due to unused inline helpers that were replaced by planner/interpreter.

**Step 2: Remove dead helpers and redundant imports**

Delete inlined TyExp/non-TyExp branch glue from `EdgeProcessing.hs`. Keep `solveNonExpInstantiation` and related helpers accessible to the interpreter.

**Step 3: Rebuild and run focused tests**

Run: `cabal build all && cabal test`

Expected: PASS with clean warnings under `-Wall`.

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs
git commit -m "refactor: remove obsolete edge-processing inline branch logic"
```

---

## Phase 3 — Atomic Wrapping + Fail-Fast (gated by equivalence)

**Prerequisite:** Phase 2 must be complete with all existing tests passing.

### Task 7: Add equivalence gate tests for known-failing pipeline cases

**Files:**
- Modify: `test/PipelineSpec.hs`

**Step 1: Add explicit gate tests**

Add tests that capture the exact behavior of the 7 known-failing cases as equivalence assertions. These tests must pass on the current (pre-wrapping) codebase:
- `PipelineSpec`: "make let mismatch" node-name leakage
- `PipelineSpec`: "let-c1-apply-bool" sentinel matrix
- `PipelineSpec`: "let-c1-apply-bool" strict target matrix
- `PipelineSpec`: checked-authoritative invariant
- `ThesisFixDirectionSpec`: unchecked pipeline returns Int
- `ThesisFixDirectionSpec`: checked pipeline returns Int
- `ElaborationSpec`: `\y. let id = (\x. x) in id y` type

**Step 2: Verify gate tests pass on current codebase**

Run: `cabal build all && cabal test`

Expected: PASS (gate tests pass without wrapping).

**Step 3: Commit**

```bash
git add test/PipelineSpec.hs
git commit -m "test: add equivalence gate tests for atomic wrapping rollout"
```

### Task 8: Implement residual-edge wrapping + port legacy semantics

**Files:**
- Modify: `src/MLF/Constraint/Normalize.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`

**Step 1: Implement wrapping in normalization**

Re-apply the `enforcePaperShapedInstEdges` pass (post-loop) from the reverted Task 2 implementation:
- Extend `NormalizeState` with `nsNextExpVar`
- Add `maxExpVarKeyOrMinus1`, `freshExpVarIdNorm`
- Add `enforcePaperShapedInstEdges` and `wrapInstEdgeLeft`

**Step 2: Update interpreter to handle wrapped legacy edges**

In the interpreter, when a wrapped edge arrives with `ExpansionMode` (because the left is now TyExp), detect whether the body is a non-forall node (i.e., a synthesized wrapper around a plain variable/structure). For these cases, port the `solveNonExpInstantiation` logic into the expansion path so that:
- `decideMinimalExpansion` returns `ExpIdentity` + body-target unification
- The unification uses `solveNonExpInstantiation` semantics (binding-permission, scheme-root checks)
- Witness/trace recording matches the legacy path

**Step 3: Run equivalence gate tests**

Run: `cabal build all && cabal test`

Expected: PASS — all 7 gate tests and all existing tests pass. The 3 Task 1 NormalizeSpec tests now also pass.

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Normalize.hs src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs
git commit -m "feat: atomic wrapping + legacy-semantics port for synthesized TyExp edges"
```

### Task 9: Enforce Phase-4 fail-fast on non-TyExp left edges

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
- Modify: `test/Presolution/EdgePlannerSpec.hs`

**Step 1: Add failing presolution assertion test**

Add a test that calls `planEdge` with non-`TyExp` left edge and expects `InternalError` containing `expected TyExp-left`.

**Step 2: Implement fail-fast in planner**

Replace `LegacyDirectMode` classification with:

```haskell
_ -> throwError (InternalError
    ("planEdge: expected TyExp-left edge after normalize; edge="
        ++ show edgeId ++ " leftNode=" ++ nodeTag leftNode))
```

**Step 3: Run full regression**

Run: `cabal build all && cabal test`

Expected: PASS (all edges are now TyExp-left after normalization, so the fail-fast never triggers on valid input).

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs test/Presolution/EdgePlannerSpec.hs
git commit -m "feat: fail fast on non-TyExp phase-4 inst edges in planner"
```

### Task 10: Remove legacy-direct execution path

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`

**Step 1: Remove `LegacyDirectMode` and `executeLegacyDirectPath`**

Since all edges are now TyExp-left and the planner rejects non-TyExp, the legacy path is dead code. Remove it and simplify `EdgePlanMode` to a single mode (or remove the discriminator entirely).

**Step 2: Remove `solveNonExpInstantiation` and related helpers**

Delete dead code from `EdgeProcessing.hs`.

**Step 3: Run full regression**

Run: `cabal build all && cabal test`

Expected: PASS with clean `-Wall`.

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs src/MLF/Constraint/Presolution/EdgeProcessing.hs
git commit -m "refactor: remove legacy-direct execution path after TyExp-left invariant"
```

---

## Phase 4 — Error Model, Verification Matrix, and Documentation

### Task 11: Add phase-tagged error wrappers (`PlanError` vs `ExecError`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Base.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
- Modify: `test/Presolution/EdgePlannerSpec.hs`

**Step 1: Add failing error-tag test**

Add test asserting planner invariant violation surfaces with planner tag text.

**Step 2: Implement minimal tagging**

Add constructors in `PresolutionError`:

```haskell
| PlanError PresolutionError
| ExecError PresolutionError
```

Use `PlanError` in planner precondition errors and `ExecError` in interpreter runtime failures.

**Step 3: Run tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "planner tag"'`

Expected: PASS.

**Step 4: Commit**

```bash
git add src/MLF/Constraint/Presolution/Base.hs src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs test/Presolution/EdgePlannerSpec.hs
git commit -m "feat: phase-tag presolution edge planning/execution errors"
```

### Task 12: Add equivalence and regression matrix tests

**Files:**
- Modify: `test/Presolution/ExpansionSpec.hs`
- Modify: `test/Presolution/EdgeTraceSpec.hs`
- Modify: `test/Presolution/WitnessSpec.hs`
- Modify: `test/PipelineSpec.hs`

**Step 1: Add matrix tests**

Add explicit cases for:
- identity expansion path
- instantiate path
- forall-intro path
- compose path
- annotation-edge weaken suppression

Each should assert stable `prEdgeExpansions` + witness step shape.

**Step 2: Run matrix and broader tests**

Run: `cabal build all && cabal test`

Expected: PASS.

**Step 3: Commit**

```bash
git add test/Presolution/ExpansionSpec.hs test/Presolution/EdgeTraceSpec.hs test/Presolution/WitnessSpec.hs test/PipelineSpec.hs
git commit -m "test: add two-pass edge-processing regression matrix"
```

### Task 13: Documentation and task-tracking updates

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md`
- Modify: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md`
- Modify: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md`

**Step 1: Update docs**

Add concise entries:
- Phase 2 now enforces `TyExp-left` residual inst edges
- Phase 4 edge processing is planner→interpreter two-pass
- presolution edge failures are phase-tagged

Add `CHANGELOG.md` bullet for major refactor and test coverage.

**Step 2: Update task tracking files**

Record completed phases, key decisions, and verification outcomes.

**Step 3: Commit**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md
git commit -m "docs: record master 4-phase typed two-pass edge DSL refactor"
```

### Task 14: Full verification gate and completion handoff

**Files:**
- Verify only

**Step 1: Targeted verification**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match NormalizeSpec'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match EdgePlanner'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match EdgeInterpreter'`

Expected: PASS.

**Step 2: Repository validation command**

Run: `cabal build all && cabal test`

Expected: PASS.

**Step 3: Verify clean diff scope**

Run: `git status --short`

Expected: only intended files changed.

**Step 4: Final checkpoint summary**

Prepare evidence block with:
- tests run + outputs
- changed modules grouped by phase
- thesis-faithfulness impact

**Step 5: Complete branch workflow**

Use `@superpowers:finishing-a-development-branch` before merge/PR decision.
