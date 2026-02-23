# A5 Totality and Harness Hardening Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Close `A5 (P3)` by removing remaining partial/footgun behavior in coercion-copy translation and adding a hard guard so presolution umbrella wiring cannot be silently omitted.

**Architecture:** Do this in two lanes. Lane 1 hardens frontend coercion-copy handling by replacing stringly internal failures with typed `ConstraintError` constructors and by making the STCon copy path structurally total. Lane 2 hardens the test harness by making `PresolutionSpec` the single umbrella entrypoint and adding an execution guard that fails the test binary if the umbrella is not run.

**Tech Stack:** Haskell (GHC 9.12.2), Cabal, Hspec, `MLF.Frontend.ConstraintGen.Translate`, `MLF.Frontend.ConstraintGen.Types`, `test/ConstraintGenSpec.hs`, `test/PresolutionSpec.hs`, `test/Main.hs`, `mlf2.cabal`.

---

### Task 1: Lock RED tests for typed coercion-copy failures

**Files:**
- Modify: `test/ConstraintGenSpec.hs`

**Step 1: Add a RED test for bare coercion constants returning a typed constructor (not stringly internal text).**
```haskell
it "rejects bare ECoerceConst with typed error" $ do
  case inferConstraintGraphDefault (ECoerceConst (STBase "Int")) of
    Left UnexpectedBareCoercionConst -> pure ()
    other -> expectationFailure ("expected typed bare-coercion error, got: " ++ show other)
```

**Step 2: Add a RED regression for STCon coercion-copy traversal that asserts no internal-string error path is used.**
```haskell
it "STCon coercion-copy failures surface as typed errors" $ do
  let ann = STArrow (STCon "List" (STBase "Int" :| [])) (STCon "List" (STBase "Int" :| [STBase "Bool"]))
  case inferConstraintGraphDefault (EAnn (ELit (LInt 1)) ann) of
    Left (TypeConstructorArityMismatch _ _ _) -> pure ()
    Left (InternalConstraintError msg) -> expectationFailure ("unexpected internal path: " ++ msg)
    other -> expectationFailure ("unexpected result: " ++ show other)
```

**Step 3: Run the focused RED slice.**
Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "bare ECoerceConst|STCon coercion-copy failures surface as typed errors"'`
Expected: FAIL until new constructors/translation updates land.

**Step 4: Commit RED tests.**
```bash
git add test/ConstraintGenSpec.hs
git commit -m "test: add red coverage for coercion-copy typed failure paths"
```

---

### Task 2: Introduce explicit typed errors for coercion-copy footguns

**Files:**
- Modify: `src/MLF/Frontend/ConstraintGen/Types.hs`
- Modify: `src/MLF/Frontend/ConstraintGen/Translate.hs`

**Step 1: Add dedicated `ConstraintError` constructor(s) for coercion-copy invariants.**
```haskell
data ConstraintError
  = ...
  | UnexpectedBareCoercionConst
```

**Step 2: Replace `InternalConstraintError "buildExprRaw: unexpected bare ECoerceConst"` with the typed constructor.**
```haskell
ECoerceConst{} -> throwError UnexpectedBareCoercionConst
```

**Step 3: Keep error propagation unchanged through pipeline layers (`PipelineConstraintError`).**
- No behavior broadening, only error-shape hardening.

**Step 4: Run focused coercion tests.**
Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Coercion semantics|Constructor types|bare ECoerceConst"'`
Expected: PASS for updated typed-path assertions.

**Step 5: Commit typed-error hardening.**
```bash
git add src/MLF/Frontend/ConstraintGen/Types.hs src/MLF/Frontend/ConstraintGen/Translate.hs
git commit -m "fix: replace coercion-copy internal failure with typed constraint error"
```

---

### Task 3: Totalize STCon coercion-copy traversal shape

**Files:**
- Modify: `src/MLF/Frontend/ConstraintGen/Translate.hs`
- Modify: `test/ConstraintGenSpec.hs`

**Step 1: Refactor STCon branch to a total helper over `NonEmpty` args (no manual head/tail plumbing in branch body).**
```haskell
internalizeConArgs
  :: ...
  -> NonEmpty NormSrcType
  -> ConstraintM (NonEmpty NodeId, SharedEnv)
```

**Step 2: Replace inline STCon `NE.head`/`NE.tail` + ad hoc accumulator with helper-based traversal.**
- Preserve bind-parent and sharing behavior exactly.

**Step 3: Add a non-regression test for nested constructor shape and binding-tree validity after refactor.**
```haskell
it "nested STCon coercion-copy preserves binding-tree validity" $ ...
```

**Step 4: Run focused constructor + binding invariants slice.**
Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Constructor types|coercion and let scope wiring preserve single-parent invariant|nested STCon coercion-copy preserves binding-tree validity"'`
Expected: PASS.

**Step 5: Commit STCon totality refactor.**
```bash
git add src/MLF/Frontend/ConstraintGen/Translate.hs test/ConstraintGenSpec.hs
git commit -m "refactor: totalize STCon coercion-copy traversal"
```

---

### Task 4: Make presolution umbrella wiring explicit and single-sourced

**Files:**
- Modify: `test/PresolutionSpec.hs`
- Modify: `test/Main.hs`

**Step 1: Expand `PresolutionSpec` to include all presolution children currently wired directly in `Main` (including planner/interpreter specs).**
```haskell
spec = describe "Phase 4 — Principal Presolution" $ do
  Presolution.EnforcementSpec.spec
  ...
  Presolution.EdgePlannerSpec.spec
  Presolution.EdgeInterpreterSpec.spec
```

**Step 2: Replace direct `Presolution.*` imports/calls in `Main` with one umbrella import/call.**
```haskell
import qualified PresolutionSpec
...
PresolutionSpec.spec
```

**Step 3: Keep ordering deterministic and unchanged for non-presolution suites.**
- Preserve existing top-level suite sequence.

**Step 4: Run focused presolution harness slice.**
Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 4 — Principal Presolution|R-"'`
Expected: PASS with all presolution examples still discoverable.

**Step 5: Commit umbrella wiring consolidation.**
```bash
git add test/PresolutionSpec.hs test/Main.hs
git commit -m "test: single-source presolution suite wiring through umbrella spec"
```

---

### Task 5: Add hard harness guard against silent umbrella omission

**Files:**
- Modify: `test/PresolutionSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal` (if new guard module is introduced)

**Step 1: Add a runtime marker in `PresolutionSpec` that flips when umbrella spec executes.**
```haskell
specWithMarker :: IORef Bool -> Spec
specWithMarker ranRef = beforeAll_ (writeIORef ranRef True) spec
```

**Step 2: In `Main`, allocate marker, run `specWithMarker`, and fail after `hspec` if marker is still `False`.**
```haskell
main = do
  ranRef <- newIORef False
  hspec $ do
    ...
    PresolutionSpec.specWithMarker ranRef
    ...
  ran <- readIORef ranRef
  unless ran (die "Harness wiring error: PresolutionSpec umbrella was not executed")
```

**Step 3: (Optional but recommended) Add a lightweight static `HarnessWiringSpec` that checks `test/Main.hs` still references `PresolutionSpec.specWithMarker` and that `PresolutionSpec` remains listed in test modules.**
- If this module is added, register it in `mlf2.cabal` and `test/Main.hs`.

**Step 4: Run harness-focused verification.**
Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Harness wiring|Principal Presolution"'`
Expected: PASS.

**Step 5: Commit harness guard.**
```bash
git add test/PresolutionSpec.hs test/Main.hs
# git add mlf2.cabal test/HarnessWiringSpec.hs  # if optional static guard added
git commit -m "test: fail fast when presolution umbrella wiring is omitted"
```

---

### Task 6: Full verification and tracker/doc closure

**Files:**
- Modify: `TODO.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `tasks/todo/2026-02-18-a5-totality-harness-hardening/task_plan.md`
- Modify: `tasks/todo/2026-02-18-a5-totality-harness-hardening/findings.md`
- Modify: `tasks/todo/2026-02-18-a5-totality-harness-hardening/progress.md`

**Step 1: Run full gate.**
Run: `cabal build all && cabal test`
Expected: PASS.

**Step 2: Mark A5 checklist items closed in `TODO.md` lines 263 and 393 section.**
- Include brief status note with verification evidence.

**Step 3: Update `implementation_notes.md` with typed coercion-copy failure policy and harness guard contract.**
- Document any intentional deviation (if any).

**Step 4: Add a concise `CHANGELOG.md` entry for A5 closure.**
- Mention typed coercion-copy error and presolution harness guard.

**Step 5: Update task files with final evidence and archive task folder when complete.**
```bash
mv tasks/todo/2026-02-18-a5-totality-harness-hardening tasks/archive/2026-02-18-a5-totality-harness-hardening
```

---

## Execution Sequence Notes

- Keep TDD order strict: RED tests first, then minimal implementation.
- Do not reintroduce stringly `InternalConstraintError` in coercion-copy path.
- Prefer smallest harness change that makes omission fail loudly and deterministically.

## Done Criteria

1. Frontend coercion-copy path exposes typed failures for known footguns (no stringly internal branch for bare coercion const).
2. STCon coercion-copy traversal is structurally total and regression-covered.
3. Presolution tests are wired through the umbrella spec as the single entrypoint.
4. Omitting umbrella execution causes deterministic test-suite failure (guarded, not silent).
5. `cabal build all && cabal test` passes.
6. `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and task logs are updated in the same iteration.
