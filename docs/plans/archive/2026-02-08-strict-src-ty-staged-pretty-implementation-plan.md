# Strict SrcTy + Staged Pretty Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace split frontend type datatypes with a single indexed `SrcTy` model and make frontend pretty-printing staged/generic.

**Architecture:** Introduce one indexed AST in `MLF.Frontend.Syntax` with stage (`SrcNorm`) and root variable-policy (`SrcTopVar`) indices, then migrate parser/normalizer/constraintgen/pretty to aliases over that model. Preserve existing semantic behavior (normalization, normalized-only Phase 1 contract, witness behavior) while removing duplicate concrete type definitions. Keep constructor names stable to minimize churn and regression risk.

**Tech Stack:** Haskell (GHC 9.12.2), Cabal, Hspec, Megaparsec, recursion-schemes (`Recursive`/`Corecursive`).

---

### Task 1: Replace Frontend Type Declarations With Indexed `SrcTy`

**Files:**
- Modify: `src/MLF/Frontend/Syntax.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add a compile-level smoke test block in `test/ElaborationSpec.hs`:

```haskell
describe "SrcTy indexed aliases compile shape" $ do
  it "supports raw and normalized aliases from one SrcTy family" $ do
    let rawTy :: SrcType
        rawTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
        normTy :: NormSrcType
        normTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
    show rawTy `shouldNotBe` ""
    show normTy `shouldNotBe` ""
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "SrcTy indexed aliases compile shape"'`
Expected: FAIL at compile time once old constructors/types are removed (or until migration is complete).

**Step 3: Write minimal implementation**

In `src/MLF/Frontend/Syntax.hs`:

- Add:
  - `data SrcNorm = RawN | NormN`
  - `data SrcTopVar = TopVarAllowed | TopVarDisallowed`
  - `data SrcBound (n :: SrcNorm)` wrapper to avoid `Nothing` ambiguity in `STForall`.
  - `data SrcTy (n :: SrcNorm) (v :: SrcTopVar)` with constructors:
    - `STVar :: String -> SrcTy n 'TopVarAllowed`
    - `STArrow :: SrcTy n 'TopVarAllowed -> SrcTy n 'TopVarAllowed -> SrcTy n v`
    - `STBase :: String -> SrcTy n v`
    - `STCon :: String -> NonEmpty (SrcTy n 'TopVarAllowed) -> SrcTy n v`
    - `STForall :: String -> Maybe (SrcBound n) -> SrcTy n 'TopVarAllowed -> SrcTy n v`
    - `STBottom :: SrcTy n v`
- Replace old concrete data declarations for `SrcType` / `NormSrcType` / `StructBound` with aliases:
  - `type SrcType = SrcTy 'RawN 'TopVarAllowed`
  - `type NormSrcType = SrcTy 'NormN 'TopVarAllowed`
  - `type StructBound = SrcTy 'NormN 'TopVarDisallowed`
  - `type RawSrcType = SrcType`
- Update exports accordingly (`SrcTy`, `SrcNorm`, `SrcTopVar`, `SrcBound` + aliases).
- Keep `Expr` type parameter shape unchanged, but ensure aliases refer to `SrcTy` aliases.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "SrcTy indexed aliases compile shape"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Frontend/Syntax.hs test/ElaborationSpec.hs
git commit -m "refactor: replace split frontend types with indexed SrcTy"
```

### Task 2: Migrate Normalization to Indexed `SrcTy`

**Files:**
- Modify: `src/MLF/Frontend/Normalize.hs`
- Test: `test/FrontendNormalizeSpec.hs`

**Step 1: Write the failing test**

Add/adjust tests in `test/FrontendNormalizeSpec.hs` to assert normalized bound roots are structural:

```haskell
it "normalizes structural bound to StructBound alias over SrcTy" $ do
  normalizeType (STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a"))
    `shouldBe` Right (STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a"))
```

(Use the real constructor/helper names introduced in Task 1.)

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "StructBound alias over SrcTy"'`
Expected: FAIL due to outdated normalization types/helpers.

**Step 3: Write minimal implementation**

In `src/MLF/Frontend/Normalize.hs`:

- Update `NormalizationError` payloads and function signatures to `SrcTy` aliases.
- Rework free-var and substitution helpers over `SrcType = SrcTy 'RawN 'TopVarAllowed`.
- Implement helpers for bound wrappers:
  - unwrap and wrap `SrcBound n` when handling `STForall`.
- Keep behavior identical:
  - alias-bound inlining remains capture-avoiding
  - self-bound remains `SelfBoundVariable`
  - normalized bound root cannot be top var by construction (`StructBound` alias).
- Remove unreachable partials (`error`) where possible; return structured errors instead if needed.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "MLF.Frontend.Normalize"'`
Expected: PASS for normalization suite.

**Step 5: Commit**

```bash
git add src/MLF/Frontend/Normalize.hs test/FrontendNormalizeSpec.hs
git commit -m "refactor: migrate frontend normalization to indexed SrcTy"
```

### Task 3: Migrate Parser to `SrcTy` Aliases

**Files:**
- Modify: `src/MLF/Frontend/Parse.hs`
- Test: `test/FrontendParseSpec.hs`

**Step 1: Write the failing test**

Add parser assertions that raw and normalized paths still produce expected surface forms under new aliases:

```haskell
it "parses raw forall binder and keeps raw alias type" $ do
  parseRawEmlfType "forall a. a -> a"
    `shouldBe` Right (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "parses raw forall binder and keeps raw alias type"'`
Expected: FAIL until parser binder construction is updated for `SrcBound`.

**Step 3: Write minimal implementation**

In `src/MLF/Frontend/Parse.hs`:

- Update type imports/exports to new indexed names and aliases.
- Keep parser API names unchanged (`parseRaw*`, `parseNorm*`, legacy aliases).
- Update binder parsing/building:
  - raw binder bounds use `Maybe (SrcBound 'RawN)` where applicable via helper.
- Keep normalized parse as parse-then-normalize.
- Ensure error rendering behavior is unchanged.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "Frontend eMLF parser"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Frontend/Parse.hs test/FrontendParseSpec.hs
git commit -m "refactor: migrate frontend parser to SrcTy aliases"
```

### Task 4: Implement Staged/Generic Pretty Entry Points

**Files:**
- Modify: `src/MLF/Frontend/Pretty.hs`
- Test: `test/FrontendPrettySpec.hs`
- Optional callsite touch: `src-public/MLF/API.hs` (only if signatures or exports change textually)

**Step 1: Write the failing test**

Add normalized/staged pretty coverage in `test/FrontendPrettySpec.hs`:

```haskell
it "pretty-prints normalized staged types" $ do
  let ty :: NormSrcType
      ty = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
  prettyEmlfType ty `shouldBe` "∀a. a -> a"
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "pretty-prints normalized staged types"'`
Expected: FAIL because `prettyEmlfType` is currently raw-only.

**Step 3: Write minimal implementation**

In `src/MLF/Frontend/Pretty.hs`:

- Change signatures to staged/generic:
  - `prettyEmlfType :: SrcTy n v -> String`
  - `prettyEmlfExpr :: Expr 'Surface (SrcTy n v) -> String`
- Generalize helpers (`goType`, `goArg`, forall-collector, binder renderer) across indices.
- Preserve output format exactly for existing raw tests.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "Frontend eMLF pretty printer"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Frontend/Pretty.hs test/FrontendPrettySpec.hs src-public/MLF/API.hs
git commit -m "feat: make frontend pretty printers staged and generic"
```

### Task 5: Migrate ConstraintGen/Translate Type Signatures

**Files:**
- Modify: `src/MLF/Frontend/ConstraintGen/Translate.hs`
- Modify: `src/MLF/Frontend/ConstraintGen.hs`
- Test: `test/ConstraintGenSpec.hs`

**Step 1: Write the failing test**

Add a targeted coercion/type-internalization test in `test/ConstraintGenSpec.hs` ensuring normalized bound handling still compiles and behaves:

```haskell
it "internalizes normalized forall bounds using indexed StructBound alias" $ do
  let ann :: NormSrcType
      ann = STForall "a" (Just (mkNormBound (STBase "Int"))) (STVar "a")
  -- existing harness assertion for successful internalization
  ...
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "indexed StructBound alias"'`
Expected: FAIL until signatures/helpers are updated.

**Step 3: Write minimal implementation**

In `src/MLF/Frontend/ConstraintGen/Translate.hs` and `src/MLF/Frontend/ConstraintGen.hs`:

- Update signatures to `NormSrcType` alias over `SrcTy`.
- Replace `StructBound` conversion helpers with `SrcBound`/alias-aware helpers.
- Keep alias-bound-unreachable invariant behavior unchanged.
- Remove stale comments referencing now-deleted separate concrete types.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "ConstraintGen"'`
Expected: PASS for constraint generation suites.

**Step 5: Commit**

```bash
git add src/MLF/Frontend/ConstraintGen/Translate.hs src/MLF/Frontend/ConstraintGen.hs test/ConstraintGenSpec.hs
git commit -m "refactor: migrate constraint generation frontend types to SrcTy"
```

### Task 6: Update Public API, Helper Modules, and Test Utilities

**Files:**
- Modify: `src-public/MLF/API.hs`
- Modify: `src-public/MLF/Pipeline.hs`
- Modify: `test/SpecUtil.hs`
- Modify: other affected tests via compiler guidance (`test/*.hs`)

**Step 1: Write the failing test**

Add/update one API-level smoke test module (or existing API-integration tests) to compile and run staged type exports from `MLF.API`:

```haskell
it "exports staged SrcTy aliases for raw and normalized paths" $ do
  let _raw :: SrcType
      _raw = STBase "Int"
      _norm :: NormSrcType
      _norm = STBase "Int"
  True `shouldBe` True
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "exports staged SrcTy aliases"'`
Expected: FAIL if exports/imports are incomplete.

**Step 3: Write minimal implementation**

- Ensure `MLF.API` and `MLF.Pipeline` exports reflect indexed model (while retaining legacy alias names users rely on).
- Update `test/SpecUtil.hs` constructors/helpers (e.g., `mkForalls`) for `SrcBound` wrappers.
- Perform mechanical test migration across affected files.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "exports staged SrcTy aliases"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src-public/MLF/API.hs src-public/MLF/Pipeline.hs test/SpecUtil.hs test
git commit -m "refactor: align public API and test helpers with indexed SrcTy"
```

### Task 7: Documentation, Trackers, and Full Verification

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `Bugs.md` (if status changes)
- Modify: `tasks/todo/2026-02-08-staged-src-types-structural-raise-merge/progress.txt`
- Optional: `TODO.md` (if priorities changed)

**Step 1: Write/adjust failing tests first where behavior changed**

If any behavior drift appears during Task 1-6, add a regression test before doc updates.

**Step 2: Run targeted suite**

Run:

```bash
cabal test mlf2-test --test-options='--match "Frontend eMLF parser|Frontend eMLF pretty printer|MLF.Frontend.Normalize|ConstraintGen|Paper alignment baselines"'
```

Expected: PASS.

**Step 3: Update docs/trackers**

- Replace “separate concrete types” narrative with “single indexed `SrcTy` model”.
- Record strict staged pretty outcome and any deviations.

**Step 4: Run full verification**

Run: `cabal build all && cabal test`
Expected: PASS (all tests green).

**Step 5: Commit**

```bash
git add implementation_notes.md CHANGELOG.md Bugs.md tasks/todo/2026-02-08-staged-src-types-structural-raise-merge/progress.txt TODO.md
git commit -m "docs: record strict SrcTy model and staged pretty migration"
```

### Finalization Task: Branch Hygiene and Handoff

**Files:**
- No source changes expected unless verification reveals regressions.

**Step 1: Check git status**

Run: `git status --short --branch`
Expected: clean working tree on `codex/strict-src-ty-staged-pretty`.

**Step 2: Summarize validation evidence**

Capture:
- targeted test commands and outcomes
- full gate outcome
- any known residual risks (if any).

**Step 3: Prepare for review**

Run:

```bash
git log --oneline --decorate -n 10
```

Expected: task-structured commits in order.

**Step 4: Optional squash/rebase only if explicitly requested**

Do not amend history unless requested.
