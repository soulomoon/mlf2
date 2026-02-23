# A7 Group 3: Test Harness Dedup Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Centralize repeated test-only pipeline helpers (`unsafeNormalize`, `firstShow`, solve chain runners) into `SpecUtil` and migrate heavy specs to use them.

**Architecture:** Expand `test/SpecUtil.hs` with small, composable helpers that encode common pipeline setup and error conversion once. Replace local helper duplicates in `PipelineSpec`, `ElaborationSpec`, and `ConstraintGenSpec` with `SpecUtil` imports. Keep behavior identical; this is readability/maintainability refactor.

**Tech Stack:** Haskell test suite (`Hspec`, `QuickCheck`), existing pipeline APIs.

---

## Skills & Constraints
- Use @haskell-pro for helper signatures and module hygiene.
- Use @test-driven-development (red→green per migration chunk).
- Use @verification-before-completion before finish.

### Task 1: Add Red Tests/Usages for Missing Shared Helpers

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/ConstraintGenSpec.hs`

**Step 1: Write the failing test**

Replace one local helper usage in each file with new `SpecUtil` symbols before defining them in `SpecUtil`:

```haskell
import SpecUtil (unsafeNormalizeExpr, firstShowE, runToSolvedDefault)

-- example usage in PipelineSpec
case runToSolvedDefault defaultPolySyms expr of
  Left err -> expectationFailure err
  Right solved -> validateStrict solved
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline|Binding tree coverage|elimination rewrite'`
Expected: FAIL with missing identifiers in `SpecUtil`.

**Step 3: Write minimal implementation**

Add to `test/SpecUtil.hs`:

```haskell
unsafeNormalizeExpr :: SurfaceExpr -> NormSurfaceExpr
unsafeNormalizeExpr expr =
  case normalizeExpr expr of
    Left err -> error ("normalizeExpr failed in test: " ++ show err)
    Right out -> out

firstShowE :: Show e => Either e a -> Either String a
firstShowE = either (Left . show) Right

runToPresolutionDefault :: PolySyms -> SurfaceExpr -> Either String PresolutionResult
runToPresolutionDefault poly expr = do
  ConstraintResult{ crConstraint = c0 } <- firstShowE (generateConstraints poly (unsafeNormalizeExpr expr))
  let c1 = normalize c0
  acyc <- firstShowE (checkAcyclicity c1)
  firstShowE (computePresolution defaultTraceConfig acyc c1)

runToSolvedDefault :: PolySyms -> SurfaceExpr -> Either String SolveResult
runToSolvedDefault poly expr = do
  pres <- runToPresolutionDefault poly expr
  firstShowE (solveUnify defaultTraceConfig (prConstraint pres))
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline'`
Expected: PASS for migrated usages.

**Step 5: Commit**

```bash
git add test/SpecUtil.hs test/PipelineSpec.hs test/ElaborationSpec.hs test/ConstraintGenSpec.hs
git commit -m "refactor(test): add shared SpecUtil pipeline helpers"
```

### Task 2: Migrate PipelineSpec Completely

**Files:**
- Modify: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Delete local `unsafeNormalize` and helper wrappers in `PipelineSpec.hs`; switch all call sites to `SpecUtil` helpers.

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline'`
Expected: FAIL until all call sites/imports are updated.

**Step 3: Write minimal implementation**

Replace patterns:

```haskell
first show (generateConstraints defaultPolySyms (unsafeNormalize expr))
```

with:

```haskell
runToPresolutionDefault defaultPolySyms expr
```

and use dedicated helpers for solved + redirects where needed.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/PipelineSpec.hs
git commit -m "refactor(test): migrate PipelineSpec to SpecUtil helpers"
```

### Task 3: Migrate ElaborationSpec and ConstraintGenSpec Completely

**Files:**
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/ConstraintGenSpec.hs`

**Step 1: Write the failing test**

Remove local `firstShow` and local `runToPresolution` helper definitions.

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Elaboration|ConstraintGen'`
Expected: FAIL until imports/call sites use `SpecUtil` equivalents.

**Step 3: Write minimal implementation**

Migrate call sites to:
- `firstShowE`
- `runToPresolutionDefault`
- `runToSolvedDefault`
- `unsafeNormalizeExpr`

Keep local special-case helpers only when behavior is unique to the spec.

**Step 4: Run test to verify it passes**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Elaboration'`
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='ConstraintGen'`

Expected: PASS.

**Step 5: Commit**

```bash
git add test/ElaborationSpec.hs test/ConstraintGenSpec.hs
git commit -m "refactor(test): migrate Elaboration/ConstraintGen specs to shared harness"
```

### Task 4: Group Verification and Final Cleanup

**Files:**
- Modify: `CHANGELOG.md`
- Modify: `implementation_notes.md` (optional, if test harness patterns are documented)

**Step 1: Write the failing test**

No new test code.

**Step 2: Run full verification**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 3: Apply minimal cleanup**

- Remove dead imports from specs.
- Ensure no duplicate helper remains in touched specs.

**Step 4: Re-run focused suite**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline|Elaboration|ConstraintGen'`
Expected: PASS.

**Step 5: Commit**

```bash
git add CHANGELOG.md implementation_notes.md test/SpecUtil.hs test/PipelineSpec.hs test/ElaborationSpec.hs test/ConstraintGenSpec.hs
git commit -m "chore(test): finalize A7 shared harness dedup and docs"
```
