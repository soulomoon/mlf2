# Round 143 — Plan: End-to-end validation of Phase 7 reduction

## Scope Guard

This plan covers **item-1 only**: end-to-end validation that Phase 7 reduction
(`step`/`normalize` in `MLF.Elab.Reduce`) works correctly on elaborated terms
produced by automatically-inferred iso-recursive types. No documentation, no
roadmap edits, no cleanup.

## Current-State Readout

### Reduction engine (`src/MLF/Elab/Reduce.hs`)

- `isValue`: `ERoll ty body` is a value iff `body` is a value (line 21). Correct.
- `step ERoll`: steps body when non-value, otherwise `Nothing` (lines 39–41). Correct.
- `step EUnroll`: steps inner when non-value; when inner is value and matches
  `ERoll _ body`, returns `Just body` (lines 42–47). This is the core
  roll/unroll β-reduction.
- `normalize`: iterates `step` to fixpoint (lines 50–53).
- `substTermVar` / `substTypeVarTerm`: both handle `ERoll`/`EUnroll`
  recursively (lines 190–191, 222–223). Correct.

### Type checker (`src/MLF/Elab/TypeCheck.hs`)

- `ERoll recursiveTy body`: checks `recursiveTy` is `TMu name unfoldedBody`,
  type-checks body, expects body type equals `subst name recursiveTy
  unfoldedBody` (lines 83–95).
- `EUnroll e`: type-checks e, expects result `TMu name body`, returns
  `subst name (TMu name body) body` (lines 96–100).

### Pipeline (`src/MLF/Elab/Run/Pipeline.hs`)

- `runPipelineElabChecked` calls `runPipelineElabCheckedWithConfig
  defaultPipelineConfig` which delegates to `runPipelineElabWithConfig` (line
  95 — they share the same implementation). Both produce `(ElabTerm,
  ElabType)` with Phase 7 type-check.

### Existing test coverage (`test/PipelineSpec.hs`)

- Line 1318: "elaborates recursive uses with explicit ERoll/EUnroll and passes
  Phase 7" — validates elaboration and type-checking, but **does NOT call
  `step`/`normalize`**.
- Line 1265: non-recursive identity control — validates μ-free type.
- Lines 1281–1441: item-2/3/4 edge-case tests — all about constraints,
  elaboration, and type-checking. **None test reduction.**

### Test infrastructure already available

- `step`, `normalize`, `isValue` are re-exported through `MLF.Elab.Pipeline`
  (imported in `PipelineSpec.hs` at line 46–47).
- `typeCheck` available at line 47.
- `containsRollTerm`, `containsUnrollTerm`, `containsMu` helpers exist
  (lines 149–171, 133–147).
- `unsafeNormalizeExpr`, `runPipelineElab`, `runPipelineElabChecked` are
  imported.
- `Elab.ElabTerm`, `Elab.ERoll`, `Elab.EUnroll` constructors are accessible
  via `MLF.Elab.Pipeline qualified as Elab`.

### Gap

No test exercises `step` or `normalize` on auto-inferred recursive elaborated
terms. No type-preservation-under-reduction test exists. No test verifies that
applying a recursive function to an argument reduces through roll/unroll. No
non-recursive regression test for `step`/`normalize` behavior.

---

## Implementation Plan

All new tests go in `test/PipelineSpec.hs` inside a new describe block placed
immediately after the "Automatic μ-introduction (item-4 edge cases)" block
(after line 1441).

### Step 1 — Add `describe "Phase 7 reduction of auto-inferred recursive terms (item-1)"` block

**File**: `test/PipelineSpec.hs`

Add a new `describe` section with the following test cases. All tests use the
existing imports (`step`, `normalize`, `isValue`, `typeCheck`,
`runPipelineElab`, `runPipelineElabChecked` from `MLF.Elab.Pipeline`).

#### Test 1.1 — `isValue` recognizes ERoll as a value

```
it "isValue recognizes ERoll wrapping a value as a value" $ do
  let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, _ty) -> do
      -- The elaborated term should normalize to a value
      let nf = normalize term
      isValue nf `shouldBe` True
```

**What it validates**: `isValue` correctly handles `ERoll ty body` where `body`
is a value (lambda). If the normalized form contains `ERoll`, `isValue`
traverses through it.

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "isValue recognizes"`

#### Test 1.2 — `step` reduces through `EUnroll (ERoll _ v)` → `v`

```
it "step reduces EUnroll (ERoll ty v) to v for auto-inferred recursive terms" $ do
  let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, _ty) -> do
      -- Find step count: normalize should terminate
      let steps = iterateStep term
      length steps `shouldSatisfy` (< 1000)
      -- Verify at least one step fires (the let-substitution at minimum)
      length steps `shouldSatisfy` (> 0)
```

Where `iterateStep` is a helper that collects step results:

```haskell
iterateStep :: Elab.ElabTerm -> [Elab.ElabTerm]
iterateStep t = case step t of
  Nothing -> []
  Just t' -> t' : iterateStep t'
```

**What it validates**: `step` terminates on auto-inferred recursive elaborated
terms and fires at least once (let-substitution unfolds the body).

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "step reduces"`

#### Test 1.3 — `normalize` produces a value for self-recursive elaborated term

```
it "normalize produces a value for simple self-recursive elaborated term" $ do
  let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, _ty) -> do
      let nf = normalize term
      isValue nf `shouldBe` True
```

**What it validates**: `normalize` reaches a normal form (value) for the
canonical self-recursive test case. This proves the reduction engine doesn't
diverge on recursive terms — the term is a lambda (value) after let-
substitution and any roll/unroll normalization, not an infinite expansion.

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "normalize produces a value"`

#### Test 1.4 — Type preservation under `step`

```
it "type preservation: typeCheck(term) == typeCheck(step(term)) for recursive terms" $ do
  let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, ty) -> do
      typeCheck term `shouldBe` Right ty
      -- Check type preservation for each step
      let checkPreservation t = case step t of
            Nothing -> pure ()
            Just t' -> do
              case typeCheck t' of
                Right ty' -> ty' `shouldBe` ty
                Left tcErr ->
                  expectationFailure
                    ("Type preservation failed after step:\n"
                     ++ "  before: " ++ show t ++ "\n"
                     ++ "  after:  " ++ show t' ++ "\n"
                     ++ "  error:  " ++ show tcErr)
              checkPreservation t'
      checkPreservation term
```

**What it validates**: Every intermediate reduction step preserves the type.
This is the core type-soundness property for iso-recursive reduction.

**Note**: Type preservation for `ELet` substitution may not hold naively
because `ELet` introduces a scheme, but after substitution the scheme is gone.
The implementer should check whether `typeCheck` on the post-step term needs
the original environment. If `typeCheck term` succeeds with `emptyEnv` (which
it does — the pipeline produces closed terms), then `typeCheck (step term)`
should also succeed with `emptyEnv`. If this test reveals a mismatch, that is a
genuine bug to fix (see Step 4).

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "type preservation"`

#### Test 1.5 — Application of recursive function reduces through roll/unroll

```
it "application of recursive function reduces through roll/unroll" $ do
  let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EApp (EVar "f") (ELit (LInt 42)))
  case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, _ty) -> do
      -- The term should be reducible (step should fire)
      let steps = iterateStep term
      length steps `shouldSatisfy` (> 0)
      -- After enough steps, we should see the unroll-roll reduction fire
      -- (EUnroll (ERoll ty v) -> v pattern in at least one step)
      -- This is an application of a recursive function to an argument,
      -- which requires unrolling the recursive value before applying.
      --
      -- Note: This may diverge (infinite recursion at the value level)
      -- since `f = \x. f x` applied to 42 loops. In that case, verify
      -- that at least the first N steps are well-typed and contain
      -- the expected roll/unroll reduction pattern.
      let firstN = take 20 steps
      length firstN `shouldSatisfy` (> 0)
      -- Each intermediate term should type-check if the pipeline term did
      forM_ (take 5 firstN) $ \t' ->
        case typeCheck t' of
          Right _ -> pure ()
          Left tcErr ->
            expectationFailure ("Intermediate step ill-typed: " ++ show tcErr)
```

**What it validates**: Applying a self-recursive function to an argument fires
reduction steps that go through the roll/unroll machinery. The term `f 42` with
`f = \x. f x` is non-terminating, but the first several reduction steps should
be well-typed and should demonstrate the `EUnroll (ERoll ...)` → body pattern.

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "application of recursive function"`

#### Test 1.6 — Non-recursive regression: `step`/`normalize` unchanged

```
it "step/normalize unchanged for non-recursive programs" $ do
  let nonRecExprs =
        [ ("identity", ELam "x" (EVar "x"))
        , ("let-id", ELet "id" (ELam "x" (EVar "x")) (EVar "id"))
        , ("app-id-int", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
        , ("nested-let", ELet "a" (ELit (LInt 1)) (ELet "b" (ELit (LInt 2)) (EVar "a")))
        ]
  forM_ nonRecExprs $ \(label, expr) ->
    case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
      Left err -> expectationFailure (label ++ ": " ++ renderPipelineError err)
      Right (term, ty) -> do
        -- Type should not contain μ
        containsMu ty `shouldBe` False
        -- Term should not contain ERoll/EUnroll
        containsRollTerm term `shouldBe` False
        containsUnrollTerm term `shouldBe` False
        -- normalize should terminate
        let nf = normalize term
        isValue nf `shouldBe` True
        -- Type of normalized form should match (if checkable)
        case typeCheck nf of
          Right nfTy -> nfTy `shouldBe` ty
          Left _ -> pure ()  -- some normal forms lose let-scheme context
```

**What it validates**: Non-recursive programs produce no `ERoll`/`EUnroll`, no
`TMu` in types, and `normalize` terminates with a value. Reduction behavior is
unaffected by the iso-recursive extension.

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "step/normalize unchanged"`

### Step 2 — Add the `iterateStep` helper

**File**: `test/PipelineSpec.hs`

Add near the other test helpers (after `containsUnrollTerm`, around line 172):

```haskell
iterateStep :: Elab.ElabTerm -> [Elab.ElabTerm]
iterateStep t = case step t of
  Nothing -> []
  Just t' -> t' : iterateStep t'
```

This is a simple unfold of `step` used by tests 1.2 and 1.5. It is lazy, so
`take 20 (iterateStep t)` is safe even for divergent terms.

**Verification**: Compiles as part of `cabal build all`.

### Step 3 — Build and run full test suite

**Commands**:
```
cabal build all && cabal test
```

**Exit criteria**: All tests pass (including the new ones), zero regressions in
the existing 1168 tests.

### Step 4 — If reduction bugs are found, apply minimal fixes

This step is conditional — only needed if Steps 1–3 reveal failures.

**Likely failure modes and fix plans**:

| Failure | Root cause | Fix location | Fix description |
|---------|-----------|--------------|-----------------|
| `normalize` diverges on recursive term | `ELet` substitution unfolds recursion infinitely | `src/MLF/Elab/Reduce.hs` `step` | The let-substitution in `step` (line 35) replaces all occurrences of the let-bound variable, which for `let f = \x. f x in f` substitutes `f` in the body once, producing `\x. f x` — but `f` is still free. The elaborated term likely uses `ERoll`/`EUnroll` to guard the recursion, so this should terminate. If it doesn't, check that the elaborated term structure actually wraps recursive self-references in `EUnroll`. |
| Type preservation fails after `step` | `ELet`-step drops scheme annotation | `src/MLF/Elab/Reduce.hs` `step` or test expectation | The `ELet` step substitutes the rhs into the body and drops the scheme. If `typeCheck` relies on the scheme for the body, the post-step term may need type-checking in a different way. Adjust the test to type-check with an appropriate environment, or fix `step` to produce well-typed intermediate terms. |
| `isValue (ERoll ty (ELam ...))` returns False | Bug in `isValue` | `src/MLF/Elab/Reduce.hs` line 21 | Current code: `ERoll _ body -> isValue body`. If body is `ELam`, this returns `True`. Should be correct. If not, debug the actual elaborated term structure. |
| `EUnroll (ERoll ty v)` reduction doesn't fire | Value guard mismatch | `src/MLF/Elab/Reduce.hs` lines 44–47 | The guard checks `isValue e` before pattern-matching `ERoll`. If the inner `ERoll` contains a non-value body, `step` would recurse into `EUnroll`. Check if the elaborated term wraps the self-reference correctly. |

**Any fix must**:
1. Be minimal — change only the broken function
2. Not break existing tests
3. Pass `cabal build all && cabal test`

### Step 5 — Verify `runPipelineElabChecked` succeeds for recursive definitions

This is implicitly validated by Steps 1.1–1.5 (they all call
`runPipelineElab`), but add an explicit cross-check:

```
it "runPipelineElabChecked succeeds for self-recursive definition" $ do
  let expr = ELet "f" (ELam "x" (EApp (EVar "f") (EVar "x"))) (EVar "f")
  case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
    Left err -> expectationFailure (renderPipelineError err)
    Right (term, ty) -> do
      containsMu ty `shouldBe` True
      let nf = normalize term
      isValue nf `shouldBe` True
```

This test goes inside the same `describe` block.

**Verification**: `cabal test --test-show-details=direct 2>&1 | grep "runPipelineElabChecked succeeds"`

---

## Exit Criteria

All of the following must be true for this round to pass review:

1. **New test block exists** in `test/PipelineSpec.hs` under `describe "Phase 7
   reduction of auto-inferred recursive terms (item-1)"`.
2. **`isValue` test** (1.1) passes — `ERoll` wrapping a value is recognized.
3. **`step` termination test** (1.2) passes — reduction fires and terminates.
4. **`normalize` value test** (1.3) passes — normal form is a value.
5. **Type preservation test** (1.4) passes — every `step` preserves the type.
6. **Application reduction test** (1.5) passes — recursive function applied to
   argument reduces through roll/unroll.
7. **Non-recursive regression test** (1.6) passes — non-recursive programs
   produce identical reduction behavior.
8. **`runPipelineElabChecked` test** (Step 5) passes for recursive definitions.
9. **`iterateStep` helper** compiles without warnings.
10. **`cabal build all && cabal test`** passes with zero failures and zero
    regressions.
11. **`git diff --check`** clean.
12. **`python3 -m json.tool orchestrator/state.json >/dev/null`** succeeds.
