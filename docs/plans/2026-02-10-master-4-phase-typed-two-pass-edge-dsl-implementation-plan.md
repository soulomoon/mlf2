# Master 4-Phase Typed Two-Pass Edge DSL Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a thesis-faithful, type-guided two-pass presolution edge-processing architecture where Phase 2 guarantees paper-shaped `TyExp ≤ τ` instantiation edges and Phase 4 executes only typed, legal edge plans.

**Architecture:** Implement this in four phases: (1) enforce invariant foundations (Phase-2 wrapper + Phase-4 fail-fast), (2) introduce typed edge-plan data model and planner, (3) introduce typed interpreter and migrate `processInstEdge` to plan→execute, and (4) tighten error model, add equivalence/regression tests, and update docs. Keep the external API stable (`processInstEdge`, `runPresolutionLoop`) while replacing internals incrementally.

**Tech Stack:** Haskell (GHC2021 + selected extensions for typed plans), Cabal, Hspec, existing presolution/normalization modules.

---

## Execution Notes

- This plan assumes work occurs in this dedicated worktree: `/Users/ares/.config/superpowers/worktrees/mlf4/codex/2026-02-09-new-worktree`.
- If any test fails unexpectedly, pause and use `@superpowers:systematic-debugging` before changing logic.
- Execute in phase order; do not start later phases before earlier phase verification passes.

---

## Phase 1 — Invariant Foundation (paper-shaped edges + fail-fast)

### Task 1: Add failing tests for paper-shaped residual instantiation edges

**Files:**
- Modify: `test/NormalizeSpec.hs`
- Test: `test/NormalizeSpec.hs`

**Step 1: Write the failing tests**

Add targeted specs under `describe "Phase 2 — Normalization"`:

```haskell
it "wraps residual Var <= Var edges with TyExp-left form" $ do
    let n0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
        n1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
        e = InstEdge (EdgeId 7) (NodeId 0) (NodeId 1)
        c0 = emptyConstraint { cNodes = nodeMapFromList [(0, n0), (1, n1)], cInstEdges = [e] }
        c1 = normalize c0
    let [InstEdge eid l r] = cInstEdges c1
    eid `shouldBe` EdgeId 7
    r `shouldBe` NodeId 1
    case lookupNodeMaybe (cNodes c1) l of
        Just TyExp { tnBody = b } -> b `shouldBe` NodeId 0
        other -> expectationFailure ("expected TyExp-left edge, got " ++ show other)

it "wraps residual type-error edges too" $ do
    let base = TyBase (NodeId 0) (BaseTy "Int")
        dom = TyVar { tnId = NodeId 2, tnBound = Nothing }
        cod = TyVar { tnId = NodeId 3, tnBound = Nothing }
        arr = TyArrow (NodeId 1) (NodeId 2) (NodeId 3)
        e = InstEdge (EdgeId 9) (NodeId 0) (NodeId 1)
        c0 = emptyConstraint
            { cNodes = nodeMapFromList [(0, base), (1, arr), (2, dom), (3, cod)]
            , cInstEdges = [e]
            }
        c1 = normalize c0
    let [InstEdge _ l _] = cInstEdges c1
    case lookupNodeMaybe (cNodes c1) l of
        Just TyExp {} -> pure ()
        other -> expectationFailure ("expected TyExp-left error edge, got " ++ show other)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match wraps residual"`

Expected: FAIL because residual edges are currently not normalized to `TyExp-left`.

**Step 3: Write minimal implementation placeholder**

In `src/MLF/Constraint/Normalize.hs`, add a TODO marker near `normalizeLoop` after `applyUnionFindToConstraint`:

```haskell
-- TODO(master-4phase): enforce paper-shaped TyExp-left form for remaining cInstEdges
```

**Step 4: Re-run focused tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match wraps residual"`

Expected: FAIL with assertion mismatch (not compile failure).

**Step 5: Commit**

```bash
git add test/NormalizeSpec.hs src/MLF/Constraint/Normalize.hs
git commit -m "test: add failing specs for paper-shaped residual inst edges"
```

### Task 2: Implement residual-edge wrapping in normalization

**Files:**
- Modify: `src/MLF/Constraint/Normalize.hs`
- Test: `test/NormalizeSpec.hs`

**Step 1: Add a failing freshness test**

Add this test:

```haskell
it "allocates distinct ExpVarIds for synthesized wrappers" $ do
    let n0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
        n1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
        n2 = TyVar { tnId = NodeId 2, tnBound = Nothing }
        e0 = InstEdge (EdgeId 1) (NodeId 0) (NodeId 1)
        e1 = InstEdge (EdgeId 2) (NodeId 0) (NodeId 2)
        c1 = normalize (emptyConstraint { cNodes = nodeMapFromList [(0,n0),(1,n1),(2,n2)], cInstEdges = [e0,e1] })
        expVars =
            [ s
            | InstEdge _ l _ <- cInstEdges c1
            , Just TyExp { tnExpVar = s } <- [lookupNodeMaybe (cNodes c1) l]
            ]
    length expVars `shouldBe` 2
    length (IntSet.fromList (map getExpVarId expVars)) `shouldBe` 2
```

**Step 2: Run targeted failing test**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match distinct ExpVarIds"`

Expected: FAIL.

**Step 3: Implement minimal wrapping pass**

In `NormalizeState`, add expansion-var allocation:

```haskell
data NormalizeState = NormalizeState
    { nsNextNodeId :: !Int
    , nsNextExpVar :: !Int
    , nsUnionFind :: !(IntMap NodeId)
    , nsConstraint :: !Constraint
    }

freshExpVarIdNorm :: NormalizeM ExpVarId
freshExpVarIdNorm = do
    n <- gets nsNextExpVar
    modify' $ \s -> s { nsNextExpVar = n + 1 }
    pure (ExpVarId n)
```

Add `maxExpVarKeyOrMinus1` seeding logic in `normalize` initialization.

Add pass:

```haskell
enforcePaperShapedInstEdges :: NormalizeM ()
enforcePaperShapedInstEdges = do
    edges <- gets (cInstEdges . nsConstraint)
    edges' <- mapM wrapInstEdgeLeft edges
    modify' $ \s ->
        let c0 = nsConstraint s
        in s { nsConstraint = c0 { cInstEdges = edges' } }
```

`wrapInstEdgeLeft` requirements:
- preserve `instEdgeId`
- if left root already `TyExp`, keep it
- else synthesize `TyExp` wrapper on left root
- inherit left-node binding parent for wrapper when present

**Step 4: Run target tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match wraps residual"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match distinct ExpVarIds"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Normalize.hs test/NormalizeSpec.hs
git commit -m "feat: normalize residual inst edges into TyExp-left form"
```

### Task 3: Enforce Phase-4 fail-fast on non-`TyExp` left edges

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `test/Presolution/EdgeTraceSpec.hs`
- Test: `test/Presolution/EdgeTraceSpec.hs`

**Step 1: Add failing presolution assertion test**

Add a test that calls `processInstEdge` with non-`TyExp` left edge and expects `InternalError` containing `expected TyExp-left`.

**Step 2: Run test to confirm failure**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match expected TyExp-left"`

Expected: FAIL (current fallback branch may succeed).

**Step 3: Implement fail-fast assertion**

Replace non-`TyExp` branch in `processInstEdge` with:

```haskell
_ ->
    throwError
        (InternalError
            ( "processInstEdge: expected TyExp-left edge after normalize; edge="
                ++ show edgeId
                ++ " leftNode="
                ++ nodeTag n1Raw
            )
        )
```

**Step 4: Re-run targeted tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match expected TyExp-left"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match EdgeTrace"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgeTraceSpec.hs
git commit -m "feat: fail fast on non-TyExp phase-4 inst edges"
```

---

## Phase 2 — Typed Two-Pass Plan Core (Section 1 + Section 2 foundations)

### Task 4: Create typed plan module and wire Cabal

**Files:**
- Create: `src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs`
- Modify: `mlf2.cabal`
- Test: `test/Presolution/EdgePlannerSpec.hs`

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

(Use a real constructor helper instead of `undefined` in implementation.)

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match Edge plan types"`

Expected: FAIL (module missing).

**Step 3: Implement minimal typed plan model**

In `Plan.hs`, define a stage-indexed model:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

data EdgeStage = StageResolved | StageExpanded | StageUnified | StageCommitted

data EdgePlan (s :: EdgeStage) where
    EdgePlanResolved ::
        { eprEdge :: InstEdge
        , eprLeftExp :: TyNode
        , eprRight :: TyNode
        , eprAllowTrivial :: Bool
        , eprSuppressWeaken :: Bool
        } -> EdgePlan 'StageResolved
```

Add small helpers used by tests (`edgePlanStage`, constructor helper).

Update `mlf2.cabal` `other-modules` and test-suite modules for the new files.

**Step 4: Run targeted test/build**

Run:
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match Edge plan types"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Plan.hs test/Presolution/EdgePlannerSpec.hs mlf2.cabal
git commit -m "feat: add stage-indexed edge plan core types"
```

### Task 5: Implement planner module (pass A)

**Files:**
- Create: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `test/Presolution/EdgePlannerSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Add failing planner behavior tests**

Add tests for:
- planner returns `EdgePlan 'StageResolved` when left is `TyExp`
- planner fails with `InternalError` for non-`TyExp`
- planner threads `let`/`ann` flags into plan

Example snippet:

```haskell
it "rejects non-TyExp left edges in planner" $ do
    case runPresolutionM defaultTraceConfig st0 (planEdge edgeNonExp) of
        Left (InternalError msg) -> msg `shouldContain` "expected TyExp-left"
        other -> expectationFailure ("unexpected planner result: " ++ show other)
```

**Step 2: Run tests to verify failure**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match planner"`

Expected: FAIL (planner missing).

**Step 3: Implement planner**

`planEdge :: InstEdge -> PresolutionM (EdgePlan 'StageResolved)` should:
- read canonicalized nodes and flags (`cLetEdges`, `cAnnEdges`)
- enforce `TyExp-left`
- package resolved operands and per-edge flags into `EdgePlanResolved`

**Step 4: Run planner tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match planner"`
- `cabal build mlf2-test`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgePlannerSpec.hs test/Main.hs mlf2.cabal
git commit -m "feat: add edge planner pass with TyExp-left enforcement"
```

---

## Phase 3 — Typed Interpreter + Integration (Section 1 + Section 2 migration)

### Task 6: Implement interpreter module (pass B)

**Files:**
- Create: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
- Create: `test/Presolution/EdgeInterpreterSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Add failing interpreter smoke tests**

Add tests validating interpreter on a minimal monomorphic edge:

```haskell
it "executes resolved plan and records expansion/witness/trace" $ do
    -- plan edge, execute plan, then assert maps contain edge id
```

Include assertion for `recordEdgeExpansion edgeId ExpIdentity` in simple case.

**Step 2: Run test to confirm fail**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match executes resolved plan"`

Expected: FAIL (interpreter missing).

**Step 3: Implement interpreter**

Core API:

```haskell
executeEdgePlan :: EdgePlan 'StageResolved -> PresolutionM ()
```

Implement using existing helpers (no semantics changes):
- `decideMinimalExpansion`
- `mergeExpansions`, `setExpansion`, `recordEdgeExpansion`
- `runExpansionUnify`
- `buildEdgeTrace`, `buildEdgeWitness`, `canonicalizeEdgeTraceInteriorsM`

**Step 4: Run interpreter tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match EdgeInterpreter"`
- `cabal build mlf2-test`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs test/Presolution/EdgeInterpreterSpec.hs test/Main.hs mlf2.cabal
git commit -m "feat: add edge plan interpreter using existing presolution ops"
```

### Task 7: Refactor `processInstEdge` to two-pass plan→execute

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

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match two-pass orchestration"`

Expected: FAIL until orchestration path is updated.

**Step 3: Implement refactor**

In `EdgeProcessing.hs`:

```haskell
processInstEdge edge = do
    requireValidBindingTree
    plan <- planEdge edge
    executeEdgePlan plan

runPresolutionLoop edges = forM_ edges processInstEdge
```

Keep helper functions only if still used; delete dead direct-branch logic.

**Step 4: Run focused regression tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match EdgeTrace"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match Expansion"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match two-pass orchestration"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs test/Presolution/EdgeTraceSpec.hs test/Presolution/ExpansionSpec.hs
git commit -m "refactor: route processInstEdge through typed planner and interpreter"
```

### Task 8: Remove obsolete branch-specific helpers and tighten module boundaries

**Files:**
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`

**Step 1: Add failing compile-time cleanliness check**

Run build and confirm dead-code warnings exist after Task 7:

Run: `cabal build mlf2-test`

Expected: WARN/FAIL due to unused fallback helpers.

**Step 2: Remove dead helpers and redundant imports**

Delete non-`TyExp` path glue in `EdgeProcessing.hs`, remove unused exports/imports while preserving semantics.

**Step 3: Keep single responsibility boundaries**

Ensure:
- planner handles classification + preconditions
- interpreter handles execution
- unify/witness modules remain focused primitives

**Step 4: Rebuild and run focused tests**

Run:
- `cabal build mlf2-test`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match processInstEdge"`

Expected: PASS with clean warnings under `-Wall`.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/EdgeProcessing.hs src/MLF/Constraint/Presolution/EdgeProcessing/Unify.hs src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs
git commit -m "refactor: remove obsolete edge-processing fallback paths"
```

---

## Phase 4 — Error Model, Verification Matrix, and Documentation

### Task 9: Add phase-tagged error wrappers (`PlanError` vs `ExecError`)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Base.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs`
- Modify: `test/Presolution/EdgePlannerSpec.hs`

**Step 1: Add failing error-tag test**

Add test asserting planner invariant violation surfaces with planner tag text.

**Step 2: Run targeted test (expect fail)**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match planner tag"`

Expected: FAIL.

**Step 3: Implement minimal tagging**

Add constructors (or wrapper payloads) in `PresolutionError`:

```haskell
| PlanError PresolutionError
| ExecError PresolutionError
```

Use `PlanError` in planner precondition errors and `ExecError` in interpreter runtime failures.

**Step 4: Re-run tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-options="--match planner tag"`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/Base.hs src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs src/MLF/Constraint/Presolution/EdgeProcessing/Interpreter.hs test/Presolution/EdgePlannerSpec.hs
git commit -m "feat: phase-tag presolution edge planning/execution errors"
```

### Task 10: Add equivalence and regression matrix tests

**Files:**
- Modify: `test/Presolution/ExpansionSpec.hs`
- Modify: `test/Presolution/EdgeTraceSpec.hs`
- Modify: `test/Presolution/WitnessSpec.hs`
- Modify: `test/PipelineSpec.hs`

**Step 1: Add failing matrix tests**

Add explicit cases for:
- identity expansion path
- instantiate path
- forall-intro path
- compose path
- annotation-edge weaken suppression

Each should assert stable `prEdgeExpansions` + witness step shape.

**Step 2: Run matrix tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match identity expansion path"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match compose path"`

Expected: FAIL before updates complete.

**Step 3: Adjust code only if semantic drift found**

If failures indicate drift, fix in planner/interpreter sequencing only. Do not change unrelated solver behavior.

**Step 4: Re-run matrix and broader presolution tests**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match Presolution"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match Pipeline"`

Expected: PASS.

**Step 5: Commit**

```bash
git add test/Presolution/ExpansionSpec.hs test/Presolution/EdgeTraceSpec.hs test/Presolution/WitnessSpec.hs test/PipelineSpec.hs
git commit -m "test: add two-pass edge-processing regression matrix"
```

### Task 11: Documentation and task-tracking updates

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`
- Modify: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md`
- Modify: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md`
- Modify: `tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md`

**Step 1: Add documentation TODO checks**

Run: `rg -n "TyExp-left|two-pass|planner|interpreter|PlanError|ExecError" implementation_notes.md TODO.md`

Expected: missing/outdated lines before edits.

**Step 2: Update docs**

Add concise entries:
- Phase 2 now enforces `TyExp-left` residual inst edges
- Phase 4 edge processing is planner→interpreter two-pass
- presolution edge failures are phase-tagged

Add `CHANGELOG.md` bullet for major refactor and test coverage.

**Step 3: Update task tracking files**

Record completed phases, key decisions, and verification outcomes.

**Step 4: Verify docs contain anchors**

Run: `rg -n "TyExp-left|two-pass|PlanError|ExecError" implementation_notes.md CHANGELOG.md TODO.md`

Expected: hits in all target docs.

**Step 5: Commit**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/task_plan.md tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/findings.md tasks/todo/2026-02-10-master-4-phase-typed-two-pass-edge-dsl/progress.md
git commit -m "docs: record master 4-phase typed two-pass edge DSL refactor"
```

### Task 12: Full verification gate and completion handoff

**Files:**
- Verify only

**Step 1: Targeted verification**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options="--match NormalizeSpec"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match EdgePlanner"`
- `cabal test mlf2-test --test-show-details=direct --test-options="--match EdgeInterpreter"`

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

