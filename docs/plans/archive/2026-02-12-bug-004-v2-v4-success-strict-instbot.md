# BUG-004 V2/V4 Strict-InstBot Success Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `BUG-004-V2` and `BUG-004-V4` elaborate successfully in checked and unchecked pipelines while preserving strict `InstBot` semantics (no non-⊥ `InstBot` acceptance).

**Architecture:** Keep strictness in `MLF.Elab.TypeCheck` and `MLF.Elab.Inst` unchanged, and fix the producer side of instantiations instead. First, relax `PhiReorder` identity requirements only for non-scheme-intro positions so explicit call-site monomorphic annotations can pass without inventing bogus binder IDs. Then, prevent target-derived annotation instantiation synthesis from emitting `InstBot` on non-⊥ arguments by preferring `InstApp`/`InstId` paths. Validate behavior via BUG matrix parity and strictness regression tests.

**Tech Stack:** Haskell (GHC 9.12), Cabal, Hspec, xMLF elaboration pipeline (`MLF.Elab.*`, `MLF.Elab.Phi.*`).

---

### Task 1: Convert BUG-004-V2/V4 to failing success tests (red first)

**Files:**
- Modify: `/Volumes/src/mlf4/test/ElaborationSpec.hs`
- Test: `/Volumes/src/mlf4/test/ElaborationSpec.hs`

**Step 1: Write the failing test updates**

Replace sentinel expectations with success assertions:

```haskell
it "BUG-004-V2: call-site annotation accepts explicit monomorphic instance" $ do
  ...
  assertBothPipelinesMono expr (Elab.TBase (BaseTy "Int"))

it "BUG-004-V4: annotated parameter + inner let preserves Int result" $ do
  ...
  assertBothPipelinesMono expr (Elab.TBase (BaseTy "Int"))
```

**Step 2: Run test to verify it fails**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2|BUG-004-V4"'
```

Expected: FAIL with current signatures similar to:
- `PhiInvariantError "PhiReorder: missing binder identity at positions [2]"`
- `TCInstantiationError ... "InstBot expects TBottom, got Int -> Int"`

**Step 3: Capture failure signatures in progress notes**

Record exact failing messages for V2 and V4 in the task progress log.

**Step 4: Confirm strictness guard still red-lines illegal InstBot**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "checker strictness|equals non-bottom input type|fails InstBot"'
```

Expected: PASS.

**Step 5: Commit test flip (red state commit)**

```bash
git add /Volumes/src/mlf4/test/ElaborationSpec.hs
git commit -m "test: flip BUG-004 V2/V4 to success expectations"
```

---

### Task 2: Fix BUG-004-V2 by narrowing PhiReorder missing-ID invariant to scheme-required positions

**Files:**
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`
- Test: `/Volumes/src/mlf4/test/ElaborationSpec.hs`

**Step 1: Write failing focused run (still red from Task 1)**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2"'
```

Expected: `PhiReorder: missing binder identity ...`.

**Step 2: Implement minimal code change in `reorderBindersByPrec`**

Update the missing-identity check to require identities only for scheme-introduced binders:

```haskell
let (qs, _) = splitForalls ty
    schemeArity = case siScheme si of
      Forall binds _ -> length binds
    missingRequiredIdPositions =
      [ i
      | (i, Nothing) <- zip [(0 :: Int) ..] ids
      , i < schemeArity
      ]
...
unless (null missingRequiredIdPositions) $
  Left $ PhiInvariantError $
    "PhiReorder: missing binder identity at positions " ++ show missingRequiredIdPositions
```

Do not relax any other invariant checks.

**Step 3: Run V2 and BUG-002 regression checks**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2|BUG-002-V"'
```

Expected: `BUG-004-V2` moves to green (or at least advances past previous `PhiReorder` failure), `BUG-002-V*` stays green.

**Step 4: Run strictness sanity checks**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "checker strictness|equals non-bottom input type"'
```

Expected: PASS.

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs
git commit -m "phi: require reorder identities only for scheme binder positions"
```

---

### Task 3: Fix BUG-004-V4 by preventing non-⊥ InstBot synthesis in target-derived annotation instantiation

**Files:**
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Run/Instantiation.hs`
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Ann.hs`
- Test: `/Volumes/src/mlf4/test/ElaborationSpec.hs`

**Step 1: Reproduce current V4 failure**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V4"'
```

Expected: strict `InstBot expects TBottom, got Int -> Int` failure.

**Step 2: Change instantiation synthesis to avoid illegal InstBot construction**

In `instInsideFromArgsWithBounds`, keep strict bounded behavior but switch unbounded non-⊥ arg case away from `InstBot`:

```haskell
instFor mbBound t = case mbBound of
  Nothing
    | alphaEqType t TBottom -> Just (InstInside (InstBot t))
    | otherwise -> Just (InstInside (InstApp t))
  Just bound
    | containsForallTy bound -> Just (InstInside (InstApp t))
    | alphaEqType boundTy TBottom -> Just (InstInside (InstBot t))
    | alphaEqType boundTy t -> Just InstId
    | otherwise -> Nothing
  where
    boundTy = tyToElab bound
```

**Step 3: Keep explicit-annotation fallback behavior strict and predictable**

In `computeResultTypeFromAnn` (`Ann.hs`), preserve existing explicit-annotation branching, but ensure `phiFromTarget` is used only when produced (no permissive synthetic fallback). If needed, add a tiny guard comment explaining why non-⊥ `InstBot` is never synthesized.

**Step 4: Run V4 and BUG-004 matrix**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V"'
```

Expected: all `BUG-004-V*` pass.

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/src/MLF/Elab/Run/Instantiation.hs /Volumes/src/mlf4/src/MLF/Elab/Run/ResultType/Ann.hs
git commit -m "elab: avoid non-bottom InstBot in target-derived annotation instantiation"
```

---

### Task 4: Lock strict InstBot semantics with focused regressions

**Files:**
- Modify: `/Volumes/src/mlf4/test/TypeCheckSpec.hs`
- Modify: `/Volumes/src/mlf4/test/ElaborationSpec.hs`

**Step 1: Add low-level regression for synthesis helper semantics**

Add tests that verify helper-generated instantiations do not include non-⊥ `InstBot` for unbounded binders (use exported `instInsideFromArgsWithBounds`).

```haskell
it "instInsideFromArgsWithBounds does not synthesize InstBot on non-bottom unbounded arg" $ do
  let binds = [("a", Nothing)]
      args = [Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int"))]
  instInsideFromArgsWithBounds binds args
    `shouldBe` Just (Elab.InstInside (Elab.InstApp (Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int")))))
```

**Step 2: Keep existing strict checker tests unchanged**

Do not weaken any existing strictness checks:
- `rejects InstBot on alpha-equal non-bottom type (checker strictness)`
- `fails InstBot when argument equals non-bottom input type`

**Step 3: Run targeted strictness set**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "InstBot|checker strictness|equals non-bottom input type"'
```

Expected: PASS.

**Step 4: Run BUG-002 and BUG-004 parity set**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V|BUG-004-V"'
```

Expected: PASS.

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/test/TypeCheckSpec.hs /Volumes/src/mlf4/test/ElaborationSpec.hs
git commit -m "test: add strict InstBot synthesis guardrails for BUG-004 success path"
```

---

### Task 5: Thesis-exact audit, docs, and final verification gate

**Files:**
- Modify: `/Volumes/src/mlf4/Bugs.md`
- Modify: `/Volumes/src/mlf4/CHANGELOG.md`
- Modify: `/Volumes/src/mlf4/implementation_notes.md`

**Step 1: Update bug tracker statuses and resolved links**

In `Bugs.md`, move or update BUG-004-V2/V4 entries to resolved status with regression test references.

**Step 2: Add changelog entry**

In `CHANGELOG.md` (`Unreleased`), add one concise bullet describing strict-InstBot-preserving BUG-004 success work.

**Step 3: Update thesis-alignment notes**

In `implementation_notes.md`, document why this is thesis-exact: strict `InstBot` is unchanged; only instantiation production is corrected.

**Step 4: Run full validation command**

```bash
cabal build all && cabal test
```

Expected: full build + full test suite pass.

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/Bugs.md /Volumes/src/mlf4/CHANGELOG.md /Volumes/src/mlf4/implementation_notes.md
git commit -m "docs: record strict-InstBot-preserving BUG-004 V2/V4 closure"
```

---

### Task 6: Final branch hygiene and handoff

**Files:**
- Modify: none (verification + summary)

**Step 1: Show final status and commit list**

```bash
git status --short
git log --oneline --decorate -n 10
```

Expected: clean tree (or only known unrelated files), clear task commits.

**Step 2: Re-run narrow smoke check**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2|BUG-004-V4|checker strictness"'
```

Expected: PASS.

**Step 3: Prepare merge summary**

List modified files, rationale, and strictness invariants preserved.

**Step 4: Commit any final mechanical adjustments**

If any non-behavioral cleanup is still pending, commit with a dedicated message.

**Step 5: Request review**

Use internal review flow and include exact commands run + outputs in summary.
