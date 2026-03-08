# Single-Solved Elaboration Input Implementation Plan

> **Execution Note:** Use `@executing-plans` to implement this plan task-by-task.

**Goal:** Collapse elaboration input wiring to one authoritative `Solved` snapshot while preserving checked-authoritative output behavior and existing regression expectations.

**Architecture:** Refactor `ElabEnv` and `ResultTypeContext` to carry one solved handle, and keep generalization differences explicit via `GaBindParents`, scope overrides, redirects, and plan-builder-driven generalization. Migrate call sites compile-first, then stabilize behavior with targeted regression slices (`ElaborationSpec`, `PipelineSpec`, `DualPathSpec`) and a final full gate.

**Tech Stack:** Haskell (`cabal`, `hspec`), modules in `src/MLF/Elab/*`, existing pipeline artifact test helpers in `test/SpecUtil.hs`, source grep checks with `rg`.

**Execution Discipline:** `@haskell-pro`, `@test-driven-development`, `@verification-before-completion`.

---

### Task 1: Lock the New Elaboration API Shape with a Red Test

**Files:**
- Modify: `test/ElaborationSpec.hs:1388-1400`
- Modify: `src/MLF/Elab/Elaborate.hs:103-178`
- Test: `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Update the direct `Elab.elaborate` call in the missing-trace test to the target single-solved signature:

```haskell
case Elab.elaborate
        defaultTraceConfig
        generalizeAtWith'
        solved
        (prEdgeWitnesses pres)
        edgeTraces'
        (prEdgeExpansions pres)
        ann of
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaboration fails when a witness has no trace entry"'`
Expected: FAIL to compile with an arity/type mismatch for `Elab.elaborate`.

**Step 3: Write minimal implementation**

Collapse the split solved arguments in `Elaborate.hs`:

```haskell
data ElabEnv = ElabEnv
    { eeSolved :: Solved
    , eeGaParents :: GaBindParents
    , eeEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , eeEdgeTraces :: IntMap.IntMap EdgeTrace
    , eeEdgeExpansions :: IntMap.IntMap Expansion
    , eeScopeOverrides :: IntMap.IntMap NodeRef
    }

elaborate
    :: TraceConfig
    -> GeneralizeAtWith
    -> Solved
    -> IntMap.IntMap EdgeWitness
    -> IntMap.IntMap EdgeTrace
    -> IntMap.IntMap Expansion
    -> AnnExpr
    -> Either ElabError ElabTerm

elaborateWithScope ... solved gaParents ... =
    elaborateWithEnv cfg ElabEnv
        { eeSolved = solved
        , eeGaParents = gaParents
        , ...
        }
        ann
```

Inside `elaborateWithEnv`, derive all solved operations from `eeSolved` (named sets, canonicalizer, reify/phi/generalize targets).

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "elaboration fails when a witness has no trace entry"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/ElaborationSpec.hs src/MLF/Elab/Elaborate.hs
git commit -m "refactor: collapse elaborate API to a single solved input"
```

### Task 2: Add a Red Guard for Legacy Split-Solved Field Names

**Files:**
- Modify: `test/PipelineSpec.hs` (new guard describe block near Integration tests)
- Test: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add a guard test that scans source text and rejects legacy split fields:

```haskell
it "single-solved migration removes eeRes* fields" $ do
    elaborateSrc <- readFile "src/MLF/Elab/Elaborate.hs"
    pipelineSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
    forM_ ["eeResPhi", "eeResReify", "eeResGen"] $ \needle -> do
        elaborateSrc `shouldSatisfy` (not . isInfixOf needle)
        pipelineSrc `shouldSatisfy` (not . isInfixOf needle)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "single-solved migration removes eeRes\* fields"'`
Expected: FAIL while legacy names still exist.

**Step 3: Write minimal implementation**

Update all remaining legacy field names/call sites and keep only the single solved handle:

```haskell
-- src/MLF/Elab/Run/Pipeline.hs
elabEnv = ElabEnv
    { eeSolved = solvedClean
    , eeGaParents = bindParentsGa
    , eeEdgeWitnesses = edgeWitnesses
    , eeEdgeTraces = edgeTraces
    , eeEdgeExpansions = edgeExpansions
    , eeScopeOverrides = scopeOverrides
    }
```

Also remove/rename any local identifiers that still encode split-solved semantics in elaboration wiring.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "single-solved migration removes eeRes\* fields"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/PipelineSpec.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Elaborate.hs
git commit -m "test: add guard against legacy eeRes* split-solved fields"
```

### Task 3: Collapse ResultType Context to One Solved Handle

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Types.hs:19-31`
- Modify: `src/MLF/Elab/Run/ResultType.hs:38-96`
- Modify: `src/MLF/Elab/Run/ResultType/Ann.hs`
- Modify: `src/MLF/Elab/Run/ResultType/Fallback.hs`
- Modify: `src/MLF/Elab/Run/Pipeline.hs:200-224`
- Test: `test/PipelineSpec.hs`

**Step 1: Write the failing test**

Add a second guard test for `ResultTypeContext` split fields:

```haskell
it "single-solved migration removes rtcSolvedForGen/rtcSolvedClean fields" $ do
    typesSrc <- readFile "src/MLF/Elab/Run/ResultType/Types.hs"
    fallbackSrc <- readFile "src/MLF/Elab/Run/ResultType/Fallback.hs"
    forM_ ["rtcSolvedForGen", "rtcSolvedClean"] $ \needle -> do
        typesSrc `shouldSatisfy` (not . isInfixOf needle)
        fallbackSrc `shouldSatisfy` (not . isInfixOf needle)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "single-solved migration removes rtcSolvedForGen/rtcSolvedClean fields"'`
Expected: FAIL while old fields are present.

**Step 3: Write minimal implementation**

Refactor `ResultTypeContext` and all consumers to a single solved field:

```haskell
-- src/MLF/Elab/Run/ResultType/Types.hs
data ResultTypeContext = ResultTypeContext
    { rtcCanonical :: NodeId -> NodeId
    , rtcEdgeWitnesses :: IntMap.IntMap EdgeWitness
    , rtcEdgeTraces :: IntMap.IntMap EdgeTrace
    , rtcEdgeExpansions :: IntMap.IntMap Expansion
    , rtcSolved :: Solved
    , rtcBindParentsGa :: GaBindParents
    , rtcPlanBuilder :: PresolutionPlanBuilder
    , rtcBaseConstraint :: Constraint
    , rtcRedirects :: IntMap.IntMap NodeId
    , rtcTraceConfig :: TraceConfig
    }
```

Then update `ResultType.hs`, `Ann.hs`, `Fallback.hs`, and `Run/Pipeline.hs` record construction/field access accordingly.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "single-solved migration removes rtcSolvedForGen/rtcSolvedClean fields"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/PipelineSpec.hs \
  src/MLF/Elab/Run/ResultType/Types.hs \
  src/MLF/Elab/Run/ResultType.hs \
  src/MLF/Elab/Run/ResultType/Ann.hs \
  src/MLF/Elab/Run/ResultType/Fallback.hs \
  src/MLF/Elab/Run/Pipeline.hs
git commit -m "refactor: use single solved handle in result-type context"
```

### Task 4: Preserve Behavior on Targeted Regression Slices

**Files:**
- Modify (as needed for regressions): `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/*`
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`, `test/DualPathSpec.hs`

**Step 1: Write the failing test**

Add one explicit characterization in `PipelineSpec` for checked-authoritative stability:

```haskell
it "single-solved refactor keeps checked pipeline authoritative" $ do
    let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
    case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
        Left err -> expectationFailure (renderPipelineError err)
        Right (_termUnchecked, tyUnchecked) ->
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure (renderPipelineError err)
                Right (_termChecked, tyChecked) -> tyUnchecked `shouldBe` tyChecked
```

**Step 2: Run test to verify it fails (if regression introduced)**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "single-solved refactor keeps checked pipeline authoritative"'`
Expected: initially FAIL if the refactor drifted behavior; otherwise PASS and keep as regression lock.

**Step 3: Write minimal implementation**

Fix only the mismatched solved/generalization wiring that causes drift (avoid algorithm rewrites). Typical fixes:

```haskell
-- prefer explicit context instead of extra solved slots
scopeRoot <- resolveCanonicalScope c1 rtcSolved redirects annNodeId
targetC <- pure (schemeBodyTarget rtcSolved annNodeId)
```

and keep `GaBindParents` + plan builder as the only generalization context channels.

**Step 4: Run targeted suites**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 6 — Elaborate"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Pipeline \(Phases 1-5\)"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "Dual-path verification"'`

Expected: PASS.

**Step 5: Commit**

```bash
git add test/ElaborationSpec.hs test/PipelineSpec.hs test/DualPathSpec.hs src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/*.hs
git commit -m "test: lock single-solved behavior across elaboration and pipeline slices"
```

### Task 5: Documentation + Final Verification Gate

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Optional modify (if priorities shift): `TODO.md`

**Step 1: Write the failing check**

Run compile guards from the design verification strategy:

```bash
rg -n "eeResPhi|eeResReify|eeResGen|rtcSolvedForGen|rtcSolvedClean" src test
```

Expected: non-empty output before all migration cleanup is complete.

**Step 2: Run check to verify it fails**

Run the `rg` command above.
Expected: FAIL condition (matches found) until cleanup is complete.

**Step 3: Write minimal documentation updates**

Add concise entries:

```markdown
# implementation_notes.md
- Elaboration input now uses a single solved snapshot (`ElabEnv.eeSolved`).
- Generalization differences remain explicit via `GaBindParents`, scope overrides, and plan-builder context.

# CHANGELOG.md
- Refactor elaboration/pipeline/result-type wiring to single solved input; preserve checked-authoritative output policy.
```

**Step 4: Run final gate**

Run: `cabal build all && cabal test`
Expected: PASS with zero failing examples.

Then re-run guard:

Run: `rg -n "eeResPhi|eeResReify|eeResGen|rtcSolvedForGen|rtcSolvedClean" src test`
Expected: no output.

**Step 5: Commit**

```bash
git add implementation_notes.md CHANGELOG.md TODO.md
git commit -m "docs: record single-solved elaboration input migration"
```

---

## Definition of Done

- `ElabEnv` has exactly one solved field and no `eeResPhi|eeResReify|eeResGen` references remain.
- `ResultTypeContext` has exactly one solved field and no `rtcSolvedForGen|rtcSolvedClean` references remain.
- `runPipelineElab`/`runPipelineElabChecked` behavior remains parity-checked on targeted slices.
- `Solved.fromPresolutionResult` remains the production solved baseline.
- `cabal build all && cabal test` passes.
