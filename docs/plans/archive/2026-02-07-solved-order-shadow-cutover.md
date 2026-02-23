# Solved-Order Reification Shadow Cutover Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make solved-order reification the authoritative output path while keeping base-path reification as a temporary shadow comparator that hard-fails on semantic mismatch.

**Architecture:** Keep both reifiers temporarily, but invert ownership: solved-order always produces the runtime result and base-path runs only as an oracle when its preconditions hold. Compare both results with semantic type equality (alpha-equivalent binders, equivalent bounds/body) and fail immediately on divergence. After 5 consecutive green runs with zero mismatches, remove runtime dependence on the base path.

**Tech Stack:** Haskell (GHC2021/Haskell2010 modules), Cabal, Hspec, `mlf2` internal library (`MLF.Elab.Generalize`, presolution plan modules), existing `ValidationFailed` error channel.

---

## Execution Preconditions

- Run implementation in an isolated worktree (`@superpowers:using-git-worktrees`).
- Use TDD for each task (`@superpowers:test-driven-development`).
- Verify before completion (`@superpowers:verification-before-completion`).
- Keep behavior aligned with thesis elaboration from solved presolution (`papers/these-finale-english.txt`, §15.3.2).

### Task 1: Add Reification Shadow Comparator Unit Tests

**Files:**
- Create: `test/GeneralizeSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal` (test-suite `other-modules`)
- Modify: `src/MLF/Elab/Generalize.hs` (export testable comparator helper)

**Step 1: Write the failing test**

```haskell
module GeneralizeSpec (spec) where

import Test.Hspec
import MLF.Elab.Generalize (shadowCompareTypes)
import MLF.Elab.Types
import MLF.Util.ElabError (ElabError(..))

spec :: Spec
spec = describe "Generalize shadow comparator" $ do
  it "accepts alpha-equivalent types" $ do
    let solvedTy = TForall "a" Nothing (TVar "a")
        baseTy   = TForall "b" Nothing (TVar "b")
    shadowCompareTypes "ctx" solvedTy baseTy `shouldBe` Right ()

  it "rejects semantic mismatch" $ do
    let solvedTy = TForall "a" Nothing (TArrow (TVar "a") (TVar "a"))
        baseTy   = TForall "a" Nothing (TArrow (TVar "a") (TBase (BaseTy "Int")))
    shadowCompareTypes "ctx" solvedTy baseTy `shouldSatisfy` \case
      Left (ValidationFailed msgs) -> any ("shadow reify mismatch" `elem`) (map words msgs)
      _                            -> False
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "Generalize shadow comparator"'`
Expected: FAIL at compile time (`shadowCompareTypes` not exported / not defined).

**Step 3: Write minimal implementation**

```haskell
-- in MLF.Elab.Generalize exports:
--   shadowCompareTypes

import MLF.Reify.TypeOps (alphaEqType)

shadowCompareTypes :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypes ctx solvedTy baseTy
  | alphaEqType solvedTy baseTy = Right ()
  | otherwise =
      Left $
        ValidationFailed
          [ "shadow reify mismatch"
          , "context=" ++ ctx
          , "solved=" ++ pretty solvedTy
          , "base=" ++ pretty baseTy
          ]
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "Generalize shadow comparator"'`
Expected: PASS (2 examples).

**Step 5: Commit**

```bash
git add test/GeneralizeSpec.hs test/Main.hs mlf2.cabal src/MLF/Elab/Generalize.hs
git commit -m "test: add generalize shadow comparator unit coverage"
```

### Task 2: Add Solved-Authoritative Selection Helper (Pure, Test-First)

**Files:**
- Modify: `test/GeneralizeSpec.hs`
- Modify: `src/MLF/Elab/Generalize.hs`

**Step 1: Write the failing test**

```haskell
it "always returns solved type when comparison succeeds" $ do
  let solvedTy = TForall "a" Nothing (TVar "a")
      baseTy   = TForall "b" Nothing (TVar "b")
  selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy

it "fails hard on mismatch when base shadow is present" $ do
  let solvedTy = TVar "a"
      baseTy   = TBase (BaseTy "Int")
  selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldSatisfy` isMismatch
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "returns solved type"'`
Expected: FAIL (`selectSolvedOrderWithShadow` missing).

**Step 3: Write minimal implementation**

```haskell
selectSolvedOrderWithShadow
  :: String
  -> ElabType
  -> Maybe ElabType
  -> Either ElabError ElabType
selectSolvedOrderWithShadow _ solvedTy Nothing = Right solvedTy
selectSolvedOrderWithShadow ctx solvedTy (Just baseTy) = do
  shadowCompareTypes ctx solvedTy baseTy
  Right solvedTy
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "solved type" --match "fails hard on mismatch"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/GeneralizeSpec.hs src/MLF/Elab/Generalize.hs
git commit -m "feat: add solved-authoritative shadow selection helper"
```

### Task 3: Integrate Shadow Selection into `fallbackSchemeType`

**Files:**
- Modify: `src/MLF/Elab/Generalize.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Write the failing integration test**

Add/adjust a focused elaboration regression asserting Phase 6 succeeds in a base-mapping case and yields stable expected type:

```haskell
it "uses solved-authoritative reify and stays stable on mapped-base elaboration" $ do
  let expr = ELet "id" (ELam "x" (EVar "x"))
            (ELet "use" (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int"))
                          (EApp (EVar "f") (ELit (LInt 0))))
               (EApp (EVar "use") (EVar "id")))
  (_term, ty) <- requirePipeline expr
  ty `shouldBe` TBase (BaseTy "Int")
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "mapped-base elaboration"'`
Expected: FAIL initially if behavior changes are not yet wired / expected string absent.

**Step 3: Write minimal implementation**

Refactor local logic in `fallbackSchemeType`/`reifyWithGaBase`:

```haskell
-- solved is always primary result:
solvedTy <- reifyTypeWithOrderedBinders
mbBaseTy <- reifyBaseIfAvailable ga -- returns Maybe ElabType
selectSolvedOrderWithShadow "generalizeAt:fallbackSchemeType" solvedTy mbBaseTy
```

Rules:
- Keep existing base-node existence guard.
- If base preconditions fail, skip comparison (`Nothing`) and return solved type.
- Do not let base type override solved output.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "Phase 6" --match "mapped-base elaboration"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Generalize.hs test/ElaborationSpec.hs
git commit -m "feat: make solved-order authoritative with base shadow compare"
```

### Task 4: Improve Mismatch Diagnostics for Fast Triage

**Files:**
- Modify: `src/MLF/Elab/Generalize.hs`
- Modify: `test/GeneralizeSpec.hs`

**Step 1: Write the failing test**

```haskell
it "reports context and both normalized types on mismatch" $ do
  let Left (ValidationFailed msgs) =
        selectSolvedOrderWithShadow "generalizeAt:caseX" (TVar "a") (Just (TBase (BaseTy "Int")))
  msgs `shouldSatisfy` any ("context=generalizeAt:caseX" `isInfixOf`)
  msgs `shouldSatisfy` any ("solved=" `isInfixOf`)
  msgs `shouldSatisfy` any ("base=" `isInfixOf`)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "reports context and both normalized types"'`
Expected: FAIL until message payload is enriched.

**Step 3: Write minimal implementation**

Add stable diagnostic fields:

```haskell
ValidationFailed
  [ "shadow reify mismatch"
  , "context=" ++ ctx
  , "scopeRootC=" ++ show scopeRootC
  , "typeRoot=" ++ show typeRoot
  , "binders=" ++ show orderedBinders
  , "solved=" ++ pretty solvedTy
  , "base=" ++ pretty baseTy
  ]
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "shadow reify mismatch"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Generalize.hs test/GeneralizeSpec.hs
git commit -m "feat: enrich shadow mismatch diagnostics in generalization"
```

### Task 5: Keep Base-Path Only as Shadow (No Behavioral Ownership)

**Files:**
- Modify: `src/MLF/Elab/Generalize.hs`
- Modify: `test/GeneralizeSpec.hs`

**Step 1: Write the failing test**

```haskell
it "returns solved output even when base output is alpha-equivalent but syntactically different" $ do
  let solvedTy = TForall "a" Nothing (TVar "a")
      baseTy   = TForall "z" Nothing (TVar "z")
  selectSolvedOrderWithShadow "ctx" solvedTy (Just baseTy) `shouldBe` Right solvedTy
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-options='--match "returns solved output even when base output is alpha-equivalent"'`
Expected: FAIL if helper still returns base result anywhere.

**Step 3: Write minimal implementation**

Ensure every branch that currently does:

```haskell
if ... then pure tyBase else reifyTypeWithOrderedBinders
```

is rewritten to:

```haskell
solvedTy <- reifyTypeWithOrderedBinders
mbBaseTy <- ...
selectSolvedOrderWithShadow "..." solvedTy mbBaseTy
```

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-options='--match "Generalize shadow comparator"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Generalize.hs test/GeneralizeSpec.hs
git commit -m "refactor: demote base reification to shadow-only comparator"
```

### Task 6: Documentation Updates for New Runtime Semantics

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Optionally modify: `Bugs.md` (only if new mismatch bug discovered while implementing)

**Step 1: Write failing doc-check step**

Run a quick grep to confirm missing wording before edit:

```bash
rg -n "solved-authoritative|shadow compare|base shadow" implementation_notes.md CHANGELOG.md
```

Expected: no exact matches yet.

**Step 2: Add minimal doc updates**

Add concise bullets:
- solved-order is authoritative output in `Generalize`.
- base path is shadow comparator only.
- mismatch is hard-fail (`ValidationFailed`) with context payload.
- gate for full base-path removal is 5 consecutive green runs.

**Step 3: Run doc lint/consistency check**

Run: `rg -n "solved-order|shadow|ValidationFailed|5 consecutive" implementation_notes.md CHANGELOG.md`
Expected: matches present in both files.

**Step 4: Run focused tests as proof**

Run: `cabal test mlf2-test --test-options='--match "Generalize shadow comparator" --match "BUG-2026-02-06-001"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add implementation_notes.md CHANGELOG.md
git commit -m "docs: record solved-authoritative shadow-reify migration semantics"
```

### Task 7: Verification and Cutover Gate Tracking

**Files:**
- No new code required unless failures occur.
- Optional tracker update: `TODO.md` (if you track run count there).

**Step 1: Full verification run**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 2: Start consecutive-pass tracking**

Run this full command once per CI/local cycle:

```bash
cabal build all && cabal test
```

Expected: 5 consecutive PASS runs with zero shadow mismatch failures.

**Step 3: If mismatch appears, create/refresh bug entry**

Add/update `Bugs.md` with reproducer, expected/actual, owner modules, thesis impact.

**Step 4: After 5/5 green runs, remove dead runtime base selection code**

Modify:
- `src/MLF/Elab/Generalize.hs` (delete obsolete `tyBase` selection branches, keep only optional comparator path if still desired).

Re-run: `cabal build all && cabal test`
Expected: PASS.

**Step 5: Final commit**

```bash
git add src/MLF/Elab/Generalize.hs Bugs.md TODO.md
git commit -m "refactor: remove runtime base-path ownership after shadow gate passes"
```

## Final Verification Checklist

- `cabal build all && cabal test` passes.
- `GeneralizeSpec` comparator tests pass.
- Existing Phase 6 crash regression (`BUG-2026-02-06-001`) stays green.
- No shadow mismatch failures across 5 consecutive full runs.
- Docs updated to reflect solved-authoritative behavior and cutover gate.
