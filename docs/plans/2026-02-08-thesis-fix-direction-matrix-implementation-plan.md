# Thesis Fix Direction Matrix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Determine the correct thesis-faithful fix for `BUG-2026-02-06-002` by testing all plausible fix directions under a controlled experiment matrix.

**Architecture:** Lock thesis invariants in tests first, then run isolated direction experiments (one subsystem direction at a time) with identical diagnostics and regression gates. Record results in a shared matrix, reject regressive directions immediately, and select the smallest direction (or minimal combination) that satisfies all thesis and regression criteria.

**Tech Stack:** Haskell (GHC 9.12.2), Cabal, Hspec, `MLF.Constraint.Presolution.Plan.*`, `MLF.Elab.*`, thesis source `papers/these-finale-english.txt`.

---

## Brainstorming Outcome (Approaches)

### Option A: Isolated Direction Matrix (Recommended)
- Test each fix direction independently with strict red/green gates.
- Pros: highest causal confidence, easy rollback, clear evidence for thesis-faithfulness.
- Cons: more total iterations.

### Option B: Combined Patch Sweep
- Apply multiple suspected fixes at once and check if bug disappears.
- Pros: fastest path to a green result.
- Cons: cannot identify true root cause; high risk of accidental regressions.

### Option C: Paper-only Reasoning (No Experiment Matrix)
- Choose direction purely from thesis interpretation and code reading.
- Pros: low implementation churn.
- Cons: weak empirical confidence; easy to miss implementation interactions.

Use **Option A**.

---

## Thesis Decision Criteria (Hard Gates)

1. `BUG-2026-02-06-002` expression must pass in both `runPipelineElab` and `runPipelineElabChecked` with result `Int`.
2. Let-polymorphism must remain at binder-level (no hard specialization of `make` scheme to `... -> Int` in elaboration).
3. Known nearby regressions must stay green:
   - `generalizes reused constructors via make const`
   - `redirected let-use sites keep polymorphic schemes`
   - `BUG-2026-02-08-004` sentinel behavior (unless explicitly fixed with dedicated tests).
4. Full verification must pass: `cabal build all && cabal test`.
5. Behavior must be defensible against thesis text (`papers/these-finale-english.txt`), with any deviation documented.

---

## Direction Catalog

- `D1` Binder representative filtering in `src/MLF/Constraint/Presolution/Plan/BinderPlan/Build.hs`
- `D2` Let scheme-root/type-root selection in:
  - `src/MLF/Constraint/Presolution/Plan/Target/TargetPlan.hs`
  - `src/MLF/Constraint/Presolution/Plan/Target/TypeRootPlan.hs`
- `D3` Solved↔base copy/provenance mapping in:
  - `src/MLF/Elab/Run/Generalize/Phase2.hs`
  - `src/MLF/Elab/Run/Generalize/Finalize.hs`
  - `src/MLF/Elab/Run/Provenance.hs`
- `D4` Let scope/target routing in:
  - `src/MLF/Elab/Run/Scope.hs`
  - `src/MLF/Elab/Elaborate.hs`
- `D5` Let scheme closure/substitution boundary in:
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/TermClosure.hs`
- `D6` Generalization solve-state canonicalization in:
  - `src/MLF/Elab/Run/Pipeline.hs`

---

### Task 1: Lock Baseline Red Tests and Thesis Targets

**Files:**
- Create: `test/ThesisFixDirectionSpec.hs`
- Modify: `test/Main.hs`
- Modify: `mlf2.cabal`

**Step 1: Write failing bug-target integration tests**

```haskell
module ThesisFixDirectionSpec (spec) where

import qualified Data.Set as Set
import Test.Hspec
import MLF.API

bugExpr :: NormSurfaceExpr
bugExpr =
  ELet "make" (ELam "x" (ELam "y" (EVar "x")))
    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
      (EApp (EVar "c1") (ELit (LBool True))))

spec :: Spec
spec = describe "BUG-2026-02-06-002 thesis target" $ do
  it "unchecked pipeline returns Int" $ do
    (_tm, ty) <- runPipelineElab Set.empty bugExpr
    ty `shouldBe` TBase (BaseTy "Int")

  it "checked pipeline returns Int" $ do
    (_tm, ty) <- runPipelineElabChecked Set.empty bugExpr
    ty `shouldBe` TBase (BaseTy "Int")
```

**Step 2: Wire new spec into test suite**

Run: add `ThesisFixDirectionSpec` to `test/Main.hs` and `mlf2.cabal` test `other-modules`.

**Step 3: Run test to verify RED**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match="BUG-2026-02-06-002 thesis target"'`
Expected: FAIL with current `TCLetTypeMismatch`.

**Step 4: Capture baseline evidence in matrix log**

Run: append failure payload to `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`.
Expected: log contains baseline phase/error.

**Step 5: Commit baseline RED checkpoint**

```bash
git add test/ThesisFixDirectionSpec.hs test/Main.hs mlf2.cabal docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md
git commit -m "test: lock thesis target red baseline for BUG-2026-02-06-002"
```

---

### Task 2: Build Reusable Direction Diagnostics Runner

**Files:**
- Create: `scripts/bug-2026-02-06-002-diagnostics.hs`
- Create: `scripts/run-bug-2026-02-06-002-direction.sh`

**Step 1: Write diagnostics Haskell script (internal pipeline dump)**

```haskell
-- scripts/bug-2026-02-06-002-diagnostics.hs
-- Print: phase outcome, let scheme line, key generalizeAt trace lines,
-- s2b/b2s key mappings, and final checked/unchecked status.
main :: IO ()
main = putStrLn "DIAG|..."
```

**Step 2: Write runner shell script**

```bash
#!/usr/bin/env bash
set -euo pipefail
OUT_DIR="tmp/direction-matrix/$(date +%Y%m%d-%H%M%S)"
mkdir -p "$OUT_DIR"

cabal repl lib:mlf2 <<'TRACE_END' > "$OUT_DIR/repro.out" 2>&1
:l /tmp/repro-bug-2026-02-06-002.hs
:main
:q
TRACE_END

cabal repl lib:mlf2 <<'TRACE_END' > "$OUT_DIR/trace.out" 2>&1
:l /tmp/repro-bug-2026-02-06-002-trace.hs
:main
:q
TRACE_END

cabal test mlf2-test --test-show-details=direct \
  --test-options='--match="BUG-2026-02-06-002 thesis target|generalizes reused constructors via make const|redirected let-use"' \
  > "$OUT_DIR/tests.out" 2>&1 || true

echo "$OUT_DIR"
```

**Step 3: Run runner once on baseline**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: prints output directory with reproducible artifacts.

**Step 4: Verify artifact schema**

Run: `rg -n "Phase|TCLetTypeMismatch|elaborate let: scheme=|generalizeAt:" tmp/direction-matrix/*/trace.out`
Expected: all key lines present.

**Step 5: Commit diagnostics harness**

```bash
git add scripts/bug-2026-02-06-002-diagnostics.hs scripts/run-bug-2026-02-06-002-direction.sh
git commit -m "chore: add reusable direction diagnostics harness for BUG-2026-02-06-002"
```

---

### Task 3: Create Direction Matrix Log Template

**Files:**
- Create: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Add fixed table template**

```markdown
| Direction | Patch Summary | Bug Target | Thesis Gate | Focused Regression | Full Suite | Verdict | Notes |
|-----------|---------------|------------|-------------|--------------------|------------|---------|-------|
| Baseline  | none          | FAIL       | FAIL        | n/a                | n/a        | n/a     |       |
```

**Step 2: Add per-direction sections `D1`..`D6` with exact commands**

```markdown
### D1 Commands
- bash scripts/run-bug-2026-02-06-002-direction.sh
- cabal test ...
```

**Step 3: Add verdict rubric**

Run: include rule `PASS only if all hard gates pass`.
Expected: rubric explicit and binary.

**Step 4: Add rollback policy**

Run: include `if FAIL => revert patch immediately before next direction`.
Expected: one-direction-at-a-time isolation guaranteed.

**Step 5: Commit matrix template**

```bash
git add docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md
git commit -m "docs: add thesis-fix direction experiment matrix template"
```

---

### Task 4: Direction D1 Experiment (Binder Representative Filtering)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Plan/BinderPlan/Build.hs`
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Apply one minimal D1 patch only**

```haskell
-- Example: adjust rep suppression gate for reachable/in-scope binder retention.
```

**Step 2: Run diagnostics harness**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: new artifact folder.

**Step 3: Run focused regression pack**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match="BUG-2026-02-06-002 thesis target|generalizes reused constructors via make const|redirected let-use"'`
Expected: PASS only if D1 is viable.

**Step 4: Record verdict in matrix doc**

Run: mark PASS/FAIL + short reason.
Expected: table row completed.

**Step 5: Commit or rollback**

```bash
# if PASS
git add src/MLF/Constraint/Presolution/Plan/BinderPlan/Build.hs docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md
git commit -m "exp(d1): validate binder representative filtering direction"

# if FAIL
git restore --source=HEAD --worktree --staged src/MLF/Constraint/Presolution/Plan/BinderPlan/Build.hs
git add docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md
git commit -m "docs: record failed D1 direction"
```

---

### Task 5: Direction D2 Experiment (Target/TypeRoot Let Scheme Root)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Plan/Target/TargetPlan.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan/Target/TypeRootPlan.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan.hs`
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Apply one minimal D2 patch only**

```haskell
-- Example: explicit local let-scheme-root classification + bound-root gating.
```

**Step 2: Run diagnostics harness**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: artifact folder created.

**Step 3: Run focused regressions**

Run: same focused test command as Task 4.
Expected: PASS only if D2 is viable.

**Step 4: Record verdict in matrix doc**

Run: mark PASS/FAIL + thesis note.
Expected: row completed.

**Step 5: Commit or rollback**

```bash
# PASS path
git add src/MLF/Constraint/Presolution/Plan/Target/TargetPlan.hs src/MLF/Constraint/Presolution/Plan/Target/TypeRootPlan.hs src/MLF/Constraint/Presolution/Plan.hs docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md
git commit -m "exp(d2): validate target/type-root let-scheme direction"

# FAIL path
# revert only D2 source edits, keep matrix notes commit
```

---

### Task 6: Direction D3 Experiment (Solved/Base Mapping + Copy Provenance)

**Files:**
- Modify: `src/MLF/Elab/Run/Generalize/Phase2.hs`
- Modify: `src/MLF/Elab/Run/Generalize/Finalize.hs`
- Modify: `src/MLF/Elab/Run/Provenance.hs`
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Apply one minimal D3 patch only**

```haskell
-- Example: precedence/override policy for solvedToBase and copy map entries.
```

**Step 2: Run diagnostics harness**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: mapping lines (`s2b`, `b2s`) captured.

**Step 3: Run focused regressions**

Run: focused test command.
Expected: PASS only if D3 is viable.

**Step 4: Record verdict in matrix doc**

Run: fill row and include mapping deltas.
Expected: row completed.

**Step 5: Commit or rollback**

```bash
# PASS path: commit D3 patch + matrix
# FAIL path: revert D3 patch, commit matrix-only note
```

---

### Task 7: Direction D4 Experiment (Let Scope/Target Routing)

**Files:**
- Modify: `src/MLF/Elab/Run/Scope.hs`
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Apply one minimal D4 patch only**

```haskell
-- Example: let-only routing between scheme root vs body target.
```

**Step 2: Run diagnostics harness**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: target selection lines visible in trace.

**Step 3: Run focused regressions**

Run: focused test command.
Expected: reject if it triggers known regressions (`PhiInvariantError`, `TCTypeAbsBoundMentionsVar`).

**Step 4: Record verdict**

Run: update matrix row.
Expected: completed row.

**Step 5: Commit or rollback**

```bash
# PASS path: commit
# FAIL path: revert source, keep docs note
```

---

### Task 8: Direction D5 Experiment (Let Scheme Closure/Substitution Boundary)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Modify: `src/MLF/Elab/TermClosure.hs`
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Apply one minimal D5 patch only**

```haskell
-- Example: adjust closure trigger so let RHS and scheme are compared post-subst consistently.
```

**Step 2: Run diagnostics harness**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: let scheme vs RHS type lines updated.

**Step 3: Run focused regressions**

Run: focused test command.
Expected: PASS only if D5 viable.

**Step 4: Record verdict**

Run: update matrix row.
Expected: row completed.

**Step 5: Commit or rollback**

```bash
# PASS path: commit
# FAIL path: revert source, keep docs note
```

---

### Task 9: Direction D6 Experiment (Generalization Solve-State Canonicalization)

**Files:**
- Modify: `src/MLF/Elab/Run/Pipeline.hs`
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`

**Step 1: Apply one minimal D6 patch only**

```haskell
-- Example: alter solve-state canonicalization boundary for `solvedForGen`.
```

**Step 2: Run diagnostics harness**

Run: `bash scripts/run-bug-2026-02-06-002-direction.sh`
Expected: reproducible results captured.

**Step 3: Run focused regressions**

Run: focused test command.
Expected: PASS only if D6 viable.

**Step 4: Record verdict**

Run: update matrix row.
Expected: row completed.

**Step 5: Commit or rollback**

```bash
# PASS path: commit
# FAIL path: revert source, keep docs note
```

---

### Task 10: Select Winner and Prove Thesis-Correctness

**Files:**
- Modify: `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`
- Modify: `Bugs.md`
- Modify: `TODO.md`
- Modify: `implementation_notes.md`

**Step 1: Rank directions by hard gates**

Run: choose winner using strict order: correctness > thesis alignment > regressions > patch size.
Expected: one winner direction (or minimal pair) selected.

**Step 2: Keep only winner patch set in source tree**

Run: ensure non-winning experimental edits are reverted.
Expected: clean source diff with only final direction edits.

**Step 3: Run full verification**

Run: `cabal build all && cabal test`
Expected: all green.

**Step 4: Update docs and bug tracker**

Run: move bug status in `Bugs.md` only if all gates pass; otherwise keep open with precise next blocker.
Expected: docs reflect final truth.

**Step 5: Commit decision bundle**

```bash
git add docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md Bugs.md TODO.md implementation_notes.md
git commit -m "docs: record thesis-fix direction matrix verdict for BUG-2026-02-06-002"
```

---

## Execution Notes

- Use `@superpowers/systematic-debugging` throughout each direction iteration.
- Use `@superpowers/verification-before-completion` before claiming final resolution.
- Do not stack direction patches during evaluation; keep one active variable at a time.
- If two directions pass independently, test minimal combined patch and keep the smaller thesis-faithful set.
