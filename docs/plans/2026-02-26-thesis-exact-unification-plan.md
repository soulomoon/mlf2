# Thesis-Exact Unification Pipeline Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make unification thesis-exact by implementing SolveConstraint ordering in Phase 4 (initial unify drain, then per-edge propagation+unify), preserving raw `χp` identity as translation input, and removing production dependence on post-presolution solve.

**Architecture:** Keep `prConstraint` as the raw translatable presolution graph and represent equivalence classes with a presolution-carried UF map. Extract one shared unification closure engine and reuse it from both Presolution and Solve to avoid semantic drift. Build `Solved` from presolution without calling solve replay/rewrite routines (`solveResultFromSnapshot`), so Φ is truly presolution-centric.

**Tech Stack:** Haskell (`base`, `containers`, `mtl`), Cabal, Hspec, existing modules under `src/MLF/Constraint/*` and `src/MLF/Elab/*`.

---

### Task 1: Add Red Thesis-Order And Translatability Tests

**Files:**
- Create: `test/Presolution/UnificationClosureSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`
- Test: `test/Presolution/UnificationClosureSpec.hs`

**Step 1: Write the failing tests**

```haskell
module Presolution.UnificationClosureSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Presolution (validateTranslatablePresolution)
import MLF.Constraint.Types (cUnifyEdges)
import SpecUtil (requireRight, runToPresolutionDefault)

spec :: Spec
spec = describe "Phase 4 thesis-exact unification closure" $ do
    it "drains pending unify edges by end of presolution" $ do
        pres <- requireRight (runToPresolutionDefault Set.empty (EApp (ELam "x" (EVar "x")) (ELit (LInt 1))))
        cUnifyEdges (prConstraint pres) `shouldBe` []

    it "re-validates translatable presolution after unification closure" $ do
        pres <- requireRight (
            runToPresolutionDefault
                Set.empty
                (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
            )
        validateTranslatablePresolution (prConstraint pres) `shouldBe` Right ()

    it "exposes presolution UF metadata without assuming non-empty UF" $ do
        pres <- requireRight (runToPresolutionDefault Set.empty (ELam "x" (EVar "x")))
        let _uf = prUnionFind pres
        pure ()
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
Expected: FAIL (missing `prUnionFind` and/or `cUnifyEdges` not fully drained).

**Step 3: Write minimal scaffolding**

```haskell
-- src/MLF/Constraint/Presolution/Base.hs
newtype PresolutionUf = PresolutionUf { getPresolutionUf :: IntMap NodeId }

data PresolutionResult = PresolutionResult
    { prConstraint :: Constraint
    , prEdgeExpansions :: IntMap Expansion
    , prEdgeWitnesses :: IntMap EdgeWitness
    , prEdgeTraces :: IntMap EdgeTrace
    , prRedirects :: IntMap NodeId
    , prUnionFind :: PresolutionUf
    , prPlanBuilder :: PresolutionPlanBuilder
    }
```

**Step 4: Run test to keep behavior red**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
Expected: FAIL on behavior until ordering/drain logic is implemented.

**Step 5: Commit**

```bash
git add test/Presolution/UnificationClosureSpec.hs test/Main.hs mlf2.cabal src/MLF/Constraint/Presolution/Base.hs
git commit -m "test: add red thesis-order and translatability checks for presolution unification"
```

### Task 2: Extract Shared Unification Closure Engine (No Rewrite Side Effects)

**Files:**
- Create: `src/MLF/Constraint/Unify/Closure.hs`
- Modify: `src/MLF/Constraint/Solve.hs`
- Modify: `mlf2.cabal`
- Test: `test/SolveSpec.hs`

**Step 1: Write the failing test**

```haskell
it "runUnifyClosure drains queue and returns UF without rewrite/elimination passes" $ do
    let c0 = ...
    out <- requireRight (runUnifyClosure defaultTraceConfig c0)
    cUnifyEdges (ucConstraint out) `shouldBe` []
    -- no solve replay pass here
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "runUnifyClosure drains queue"'`
Expected: FAIL (`runUnifyClosure` missing).

**Step 3: Write minimal implementation**

```haskell
-- src/MLF/Constraint/Unify/Closure.hs
data UnifyClosureResult = UnifyClosureResult
    { ucConstraint :: Constraint
    , ucUnionFind :: IntMap NodeId
    }

runUnifyClosure :: TraceConfig -> Constraint -> Either SolveError UnifyClosureResult
runUnifyClosure traceCfg c0 = do
    -- reuse Solve worklist + batchHarmonize semantics
    -- DO NOT call rewriteConstraintWithUF
    -- DO NOT call rewriteEliminatedBinders
```

Refactor `solveUnifyWithSnapshot` to consume this engine, then keep existing compatibility replay path unchanged for legacy callers.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "runUnifyClosure drains queue"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Unify/Closure.hs src/MLF/Constraint/Solve.hs mlf2.cabal test/SolveSpec.hs
git commit -m "refactor: extract shared unification closure engine without rewrite side effects"
```

### Task 3: Enforce Thesis SolveConstraint Order In Presolution

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Driver.hs`
- Modify: `src/MLF/Constraint/Presolution/EdgeProcessing.hs`
- Modify: `src/MLF/Constraint/Presolution/Base.hs`
- Modify: `src/MLF/Constraint/Presolution.hs`
- Modify: `src/MLF/Constraint/Types/Presolution.hs`
- Modify: `src/MLF/Constraint/Types.hs`
- Test: `test/Presolution/UnificationClosureSpec.hs`

**Step 1: Write the failing test**

Add one explicit ordering check:

```haskell
it "solves initial unify edges before inst-edge traversal effects are persisted" $ do
    -- construct a constraint with an initial unification clash and at least one inst edge
    -- expected: computePresolution fails from initial unify drain
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "solves initial unify edges before inst-edge traversal"'`
Expected: FAIL until initial drain is wired before edge loop.

**Step 3: Write minimal implementation**

```haskell
-- src/MLF/Constraint/Presolution/Driver.hs
-- 0) initial unify drain (thesis step 2)
pre <- liftEither (runUnifyClosure traceCfg constraint)
let initialState = ... { psConstraint = ucConstraint pre, psUnionFind = ucUnionFind pre }

-- 1) process inst edges in topological order (thesis step 3a + 3b)
-- ensure each edge processing iteration drains any newly-added cUnifyEdges
-- via runUnifyClosure on current presolution state

-- 2) final assertion + translatability validation
when (not (null (cUnifyEdges finalConstraint))) (throwError ...)
validateTranslatablePresolution finalConstraint
```

Store final UF map in `prUnionFind`.

**Step 4: Run tests to verify they pass**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "solves initial unify edges before inst-edge traversal"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/Driver.hs src/MLF/Constraint/Presolution/EdgeProcessing.hs src/MLF/Constraint/Presolution/Base.hs src/MLF/Constraint/Presolution.hs src/MLF/Constraint/Types/Presolution.hs src/MLF/Constraint/Types.hs test/Presolution/UnificationClosureSpec.hs
git commit -m "feat: enforce thesis SolveConstraint ordering inside presolution"
```

### Task 4: Add Presolution-Native Solved Constructor (No Solve Replay)

**Files:**
- Modify: `src/MLF/Constraint/Solved.hs`
- Modify: `test/Constraint/SolvedSpec.hs`

**Step 1: Write the failing test**

```haskell
it "builds Solved from PresolutionResult without solve replay" $ do
    pres <- requireRight (runToPresolutionDefault Set.empty (ELam "x" (EVar "x")))
    solved <- requireRight (Solved.fromPresolutionResult pres)
    Solved.originalConstraint solved `shouldBe` prConstraint pres
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "without solve replay"'`
Expected: FAIL (`fromPresolutionResult` missing).

**Step 3: Write minimal implementation**

```haskell
fromPresolutionResult :: PresolutionResult -> Either SolveError Solved
fromPresolutionResult pres =
    -- new constructor path:
    -- * uses prConstraint as original graph
    -- * uses prUnionFind for canonical map/equiv classes
    -- * does NOT call solveResultFromSnapshot
    fromConstraintAndUfNoReplay (prConstraint pres) (getPresolutionUf (prUnionFind pres))
```

Add `fromConstraintAndUfNoReplay` internal helper in `Solved.hs`. Do not route through `fromPreRewriteState` (that currently calls solve replay).

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "without solve replay"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Solved.hs test/Constraint/SolvedSpec.hs
git commit -m "feat: add presolution-native Solved constructor without solve replay"
```

### Task 5: Switch Production Pipeline To Presolution-Centric Solved Path

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `test/PipelineSpec.hs`
- Modify: `test/SpecUtil.hs`

**Step 1: Write the failing test**

```haskell
it "uses presolution-native solved path in production pipeline" $ do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty (ELam "x" (EVar "x")))
    cUnifyEdges (prConstraint (paPresolution artifacts)) `shouldBe` []
    runPipelineElab Set.empty (unsafeNormalizeExpr (ELam "x" (EVar "x"))) `shouldSatisfy` isRight
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "uses presolution-native solved path"'`
Expected: FAIL until pipeline stops calling `solveUnifyWithSnapshot`.

**Step 3: Write minimal implementation**

```haskell
-- src/MLF/Elab/Run/Pipeline.hs
pres <- fromPresolutionError (computePresolution traceCfg acyc c1)
solvedView <- fromSolveError (Solved.fromPresolutionResult pres)
-- remove production solveUnifyWithSnapshot invocation
```

```haskell
-- test/SpecUtil.hs
runPipelineArtifactsDefault ... = do
    ...
    pres <- ...
    solved <- firstShowE (Solved.fromPresolutionResult pres)
    ...
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "uses presolution-native solved path"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Pipeline.hs test/PipelineSpec.hs test/SpecUtil.hs
git commit -m "feat: switch production pipeline to presolution-native solved path"
```

### Task 6: Add Behavioral Parity Tests Versus Legacy Solve Path

**Files:**
- Modify: `test/Constraint/SolvedSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/SolveSpec.hs`
- Modify: `test/SpecUtil.hs`

**Step 1: Write the failing test**

```haskell
it "matches legacy solve path on elaborated type for thesis anchor expressions" $ do
    let expr = ...
    (termNew, tyNew) <- requireRight (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
    (termLegacy, tyLegacy) <- requireRight (runPipelineElabViaLegacySolve Set.empty expr)
    tyNew `shouldBe` tyLegacy
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "matches legacy solve path on elaborated type"'`
Expected: FAIL until residual parity issues are fixed.

**Step 3: Write minimal implementation**

Align presolution-native and legacy paths on:
- unification strategy details,
- bind-parent harmonization order,
- occurs-check behavior.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "matches legacy solve path on elaborated type"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/Constraint/SolvedSpec.hs test/ElaborationSpec.hs test/SolveSpec.hs test/SpecUtil.hs src/MLF/Constraint/Unify/Closure.hs src/MLF/Constraint/Presolution/Driver.hs
git commit -m "test: enforce behavioral parity between presolution-native and legacy solve paths"
```

### Task 7: Documentation And Tracker Sync

**Files:**
- Modify: `implementation_notes.md`
- Modify: `docs/paper-map.md`
- Modify: `TODO.md`
- Modify: `Bugs.md`
- Modify: `CHANGELOG.md`

**Step 1: Write docs updates (red draft)**

Add statements:
- Phase 4 now follows thesis SolveConstraint ordering (initial unify, then propagate+unify per edge).
- `χp` is translation input; equivalence map is carried separately.
- Production pipeline no longer depends on post-presolution solve.

**Step 2: Run checks before final edits**

Run:
- `./scripts/check-thesis-claims.sh`
- `./scripts/thesis-conformance-gate.sh`

Expected: may FAIL until references are synced.

**Step 3: Write final docs updates**

```markdown
- Divergence closed (2026-02-26): presolution now implements thesis SolveConstraint ordering and drives Φ without post-presolution solve.
```

**Step 4: Run checks again**

Run:
- `./scripts/check-thesis-claims.sh`
- `./scripts/thesis-conformance-gate.sh`

Expected: PASS.

**Step 5: Commit**

```bash
git add implementation_notes.md docs/paper-map.md TODO.md Bugs.md CHANGELOG.md
git commit -m "docs: record thesis-exact unification ordering and presolution-centric pipeline"
```

### Task 8: Full Verification And Finish (@verification-before-completion)

**Files:**
- No source edits expected

**Step 1: Run targeted suites**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 thesis-exact unification closure"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "without solve replay"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "uses presolution-native solved path"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "matches legacy solve path on elaborated type"'`

Expected: PASS.

**Step 2: Run full verification**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 3: Collect release artifacts**

Run:
- `git status --short`
- `git log --oneline -n 10`
- `git diff --stat origin/main...HEAD`

Expected: clean, reviewable diff.

**Step 4: Final integration commit if needed**

```bash
git add -A
git commit -m "chore: finalize thesis-exact unification migration verification"
```

**Step 5: Prepare review notes**

Record:
- guaranteed invariants,
- any remaining legacy Solve compatibility paths,
- exact verification commands/results.

---

## Execution Notes

- Use `@haskell-pro` for type/API decisions while extracting closure logic.
- Keep DRY/YAGNI: one unification semantics source shared by Solve and Presolution.
- Keep strict TDD cadence: red -> green -> commit each task.
- Execute in a dedicated worktree.
