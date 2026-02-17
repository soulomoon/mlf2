# Selective Merge from codex/bug-2026-02-11-002-init (Thesis-Exact) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Merge only thesis-strengthening behavior from `codex/bug-2026-02-11-002-init` into `master` while preserving `master`’s thesis-exact elimination of elaboration fallback synthesis.

**Architecture:** Use a selective, test-driven merge. First lock strict `InstBot` semantics in checker/runtime. Then port only Ω translatability guardrails (non-binder hard rejection and bounded-binder strictness) from the branch, while explicitly forbidding reintroduction of `reifyInst` fallback synthesis (`allowFallbackFromTrace` / `instFromTrace`). Keep validation evidence at each checkpoint and update docs in the same iteration.

**Tech Stack:** Haskell (`ghc-9.12.2`), Cabal, Hspec, git (`worktree`, `checkout -p`, `cherry-pick -n`), ripgrep.

---

### Task 1: Enforce strict InstBot in checker and runtime

**Files:**
- Modify: `/Volumes/src/mlf4/test/TypeCheckSpec.hs`
- Modify: `/Volumes/src/mlf4/test/ElaborationSpec.hs`
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs`
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Inst.hs`

**Step 1: Write failing tests (red)**

Add this test to `/Volumes/src/mlf4/test/TypeCheckSpec.hs`:

```haskell
    it "rejects InstBot on alpha-equal non-bottom type (checker strictness)" $ do
        let poly = ETyAbs "a" (Just intTy) (ELam "x" (TVar "a") (EVar "x"))
            polyTy = TForall "a" (Just intTy) (TArrow (TVar "a") (TVar "a"))
        case typeCheck (ETyInst poly (InstBot polyTy)) of
            Left TCInstantiationError{} -> pure ()
            other -> expectationFailure ("Expected strict InstBot rejection, got: " ++ show other)
```

Add this test to `/Volumes/src/mlf4/test/ElaborationSpec.hs` near existing `applyInstantiation` tests:

```haskell
        it "fails InstBot when argument equals non-bottom input type" $ do
            let ty = Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int"))
            case Elab.applyInstantiation ty (Elab.InstBot ty) of
                Left Elab.InstantiationError{} -> pure ()
                other -> expectationFailure ("Expected strict InstBot failure, got: " ++ show other)
```

**Step 2: Run focused tests to confirm failure**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "checker strictness|equals non-bottom input type"'
```

Expected: FAIL because current code still accepts `InstBot` no-op when `t == tArg` or `alphaEqType t tArg`.

**Step 3: Write minimal implementation (green)**

In `/Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs`, change `instBot` to strict-only:

```haskell
        { instBot = \tArg (k, _env', t) -> case t of
            TBottom -> Right (k, tArg)
            _ -> Left (TCInstantiationError (InstBot tArg) t ("InstBot expects TBottom, got " ++ pretty t))
```

In `/Volumes/src/mlf4/src/MLF/Elab/Inst.hs`, change `instBot` to strict-only:

```haskell
        { instBot = \tArg (k, _env', t) -> case t of
            TBottom -> Right (k, tArg)
            _ -> Left (InstantiationError ("InstBot expects ⊥, got: " ++ pretty t))
```

**Step 4: Re-run focused tests to confirm pass**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "checker strictness|equals non-bottom input type|fails InstBot on a non-⊥ type"'
```

Expected: PASS.

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/test/TypeCheckSpec.hs \
        /Volumes/src/mlf4/test/ElaborationSpec.hs \
        /Volumes/src/mlf4/src/MLF/Elab/TypeCheck.hs \
        /Volumes/src/mlf4/src/MLF/Elab/Inst.hs
git commit -m "thesis: enforce strict InstBot in checker and runtime"
```

---

### Task 2: Port Ω non-binder translatability guardrails (selective merge only)

**Files:**
- Modify: `/Volumes/src/mlf4/test/ElaborationSpec.hs`
- Modify: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`

**Step 1: Write failing tests (red)**

In `/Volumes/src/mlf4/test/ElaborationSpec.hs`:

1. Reintroduce explicit out-of-scheme target rejection tests:

```haskell
it "rejects OpGraft+OpWeaken on out-of-scheme target (no non-binder recovery)" $ do
    -- same fixture shape used in branch bff1eb5
    -- assert error contains "OpGraft+OpWeaken targets non-binder node"
    -- assert error does NOT contain "InstBot expects"
```

```haskell
it "rejects OpGraft on out-of-scheme target (no InstBot/InstApp fallback)" $ do
    -- same fixture shape used in branch bff1eb5
    -- assert error contains "OpGraft targets non-binder node"
    -- assert error does NOT contain "InstBot expects"
```

2. Restore BUG-002 variants to target semantics (from branch):

```haskell
it "BUG-002-V1: factory twice with mixed instantiations elaborates to Int" $ do
    assertBothPipelinesMono expr (Elab.TBase (BaseTy "Int"))

it "BUG-002-V2: alias indirection elaborates to Int" $ do
    assertBothPipelinesMono expr (Elab.TBase (BaseTy "Int"))

it "BUG-002-V3: intermediate annotation elaborates to Int" $ do
    assertBothPipelinesMono expr (Elab.TBase (BaseTy "Int"))

it "BUG-002-V4: factory-under-lambda elaborates to ∀a. a -> a" $ do
    assertBothPipelinesAlphaEq expr (Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")))
```

**Step 2: Run focused tests to confirm failure**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V|out-of-scheme target"'
```

Expected: FAIL on current `master` due `OpGraft(non-binder,...)` fallback path and sentinel expectations.

**Step 3: Write minimal implementation (green)**

Port only Ω guardrail hunks from `codex/bug-2026-02-11-002-init` into `/Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs`:

1. Remove `isRootAdjacent` non-binder recovery path for both `OpGraft+OpWeaken` and `OpGraft`.
2. Keep non-binder behavior as direct translatability failure:

```haskell
else if not (isBinderNode binderKeys bv)
    then Left $ PhiTranslatabilityError
        [ "OpGraft+OpWeaken targets non-binder node"
        , "  target node: " ++ show bv
        , "  canonical: " ++ show bvC
        ]
```

```haskell
else if not (isBinderNode binderKeys bv)
    then Left $ PhiTranslatabilityError
        [ "OpGraft targets non-binder node"
        , "  target node: " ++ show bv
        , "  canonical: " ++ show bvC
        ]
```

3. Keep bounded-binder rule strict (no bound-match compatibility path):

```haskell
if mbBound /= Just TBottom && mbBound /= Nothing
    then Left $
        PhiTranslatabilityError
            [ "OpGraft requires target binder to be unbounded or ⊥-bounded"
            , "  target node: " ++ show bv
            , "  canonical: " ++ show bvC
            , "  binder bound: " ++ show mbBound
            ]
```

**Step 4: Re-run focused tests to confirm pass**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-002-V|out-of-scheme target|BUG-003-V"'
```

Expected: `BUG-002-V1..V4` pass, out-of-scheme rejection tests pass with translatability messages, BUG-003 remains tracked according to current open-bug expectation.

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/test/ElaborationSpec.hs \
        /Volumes/src/mlf4/src/MLF/Elab/Phi/Omega.hs
git commit -m "thesis: restore omega non-binder rejection and BUG-002 success semantics"
```

---

### Task 3: Keep master-only thesis-exact elaboration behavior (no fallback synthesis regression)

**Files:**
- Verify only: `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`
- Verify only: `/Volumes/src/mlf4/src/MLF/Elab/Phi/Translate.hs`

**Step 1: Add a red safety check command to the session checklist**

Use this command in progress log/checklist:

```bash
rg -n "allowFallbackFromTrace|instFromTrace" /Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs
```

Expected now: no matches.

**Step 2: Run branch-diff sanity to ensure only intended files changed**

```bash
git diff --name-only master...HEAD | sort
```

Expected includes only:
- `src/MLF/Elab/TypeCheck.hs`
- `src/MLF/Elab/Inst.hs`
- `src/MLF/Elab/Phi/Omega.hs`
- `test/TypeCheckSpec.hs`
- `test/ElaborationSpec.hs`
- docs files from Task 4

**Step 3: If fallback symbols appear, remove them immediately (minimal patch)**

Forbidden patterns in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`:

```haskell
allowFallbackFromTrace
instFromTrace <- case ...
```

If present, delete and return to direct `Right phi` behavior.

**Step 4: Re-run targeted BUG-004 checks to protect BUG-003 closure**

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V2|BUG-004-V4|strict"'
```

Expected: PASS.

**Step 5: Commit safety-only fix if any file changed**

```bash
git add /Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs
git commit -m "thesis: keep witness-only reify path (no elaboration fallback synthesis)"
```

If no change: skip commit.

---

### Task 4: Documentation and tracker sync for thesis-exact selective merge

**Files:**
- Modify: `/Volumes/src/mlf4/Bugs.md`
- Modify: `/Volumes/src/mlf4/implementation_notes.md`
- Modify: `/Volumes/src/mlf4/CHANGELOG.md`
- Modify: `/Volumes/src/mlf4/TODO.md`

**Step 1: Write failing doc consistency checks (red)**

Run:

```bash
rg -n "InstBotMode|allowFallbackFromTrace|BUG-002-V1..V4|OpGraft\(non-binder" /Volumes/src/mlf4/Bugs.md /Volumes/src/mlf4/implementation_notes.md /Volumes/src/mlf4/CHANGELOG.md /Volumes/src/mlf4/TODO.md
```

Expected: output reveals stale wording/sentinels to update.

**Step 2: Apply minimal doc updates (green)**

Update docs to state:
- strict-only `InstBot` in checker and runtime,
- Ω non-binder operations reject via `PhiTranslatabilityError` (not `InstBot` mismatch fallback),
- `BUG-002-V1..V4` expected success semantics,
- `BUG-003` status unchanged unless behavior changes during execution.

**Step 3: Re-run doc consistency checks**

```bash
rg -n "InstBotMode|allowFallbackFromTrace" /Volumes/src/mlf4/Bugs.md /Volumes/src/mlf4/implementation_notes.md
```

Expected: no stale references to removed compatibility behavior.

**Step 4: Run full validation gate**

```bash
cabal build all && cabal test
```

Expected: PASS (example count may change; failures must be zero).

**Step 5: Commit**

```bash
git add /Volumes/src/mlf4/Bugs.md \
        /Volumes/src/mlf4/implementation_notes.md \
        /Volumes/src/mlf4/CHANGELOG.md \
        /Volumes/src/mlf4/TODO.md
git commit -m "docs: sync thesis-exact selective merge outcomes and bug tracking"
```

---

### Task 5: Final integration checkpoint and merge summary

**Files:**
- Modify: `/Volumes/src/mlf4/tasks/todo/YYYY-MM-DD-selective-merge-thesis-exact/task_plan.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/YYYY-MM-DD-selective-merge-thesis-exact/findings.md`
- Modify: `/Volumes/src/mlf4/tasks/todo/YYYY-MM-DD-selective-merge-thesis-exact/progress.md`

**Step 1: Create task tracking folder (if absent)**

```bash
mkdir -p /Volumes/src/mlf4/tasks/todo/2026-02-12-selective-merge-thesis-exact
```

Create files:
- `/Volumes/src/mlf4/tasks/todo/2026-02-12-selective-merge-thesis-exact/task_plan.md`
- `/Volumes/src/mlf4/tasks/todo/2026-02-12-selective-merge-thesis-exact/findings.md`
- `/Volumes/src/mlf4/tasks/todo/2026-02-12-selective-merge-thesis-exact/progress.md`

**Step 2: Record executed commands and test evidence**

Log exact commands and PASS/FAIL results from Tasks 1-4.

**Step 3: Record thesis alignment decisions**

Document explicitly:
- merged from `codex/bug-2026-02-11-002-init`,
- intentionally not merged (fallback synthesis in `Elaborate`).

**Step 4: Archive task when complete**

```bash
mv /Volumes/src/mlf4/tasks/todo/2026-02-12-selective-merge-thesis-exact \
   /Volumes/src/mlf4/tasks/archive/2026-02-12-selective-merge-thesis-exact
```

**Step 5: Commit final task artifacts**

```bash
git add /Volumes/src/mlf4/tasks/archive/2026-02-12-selective-merge-thesis-exact
git commit -m "chore: archive selective thesis-exact merge execution records"
```

---

## Guardrails (Do Not Merge)

Do not merge these from `codex/bug-2026-02-11-002-init`:
- `allowFallbackFromTrace` and `instFromTrace` synthesis path in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`.
- Any permissive fallback that converts translatability failures into inferred instantiations.
- Any reintroduction of compatibility mode APIs for `InstBot`.

## Skills to apply during execution

- `@superpowers/test-driven-development`
- `@superpowers/systematic-debugging`
- `@superpowers/verification-before-completion`
- `@superpowers/requesting-code-review`

