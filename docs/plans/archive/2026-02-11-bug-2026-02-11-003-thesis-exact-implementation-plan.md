# BUG-2026-02-11-003 Thesis-Exact Follow-up Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove non-thesis fallback semantics introduced around BUG-004 V2/V4 while preserving green behavior via paper-faithful witness/annotation translation.

**Architecture:** Shift from consumer-side recovery (heuristic instantiation synthesis and relaxed `InstBot`) to producer-side correctness (paper-shaped Φ/Σ translation and annotation/coercion typing). Add a strict-theory test harness first, then remove compatibility behavior, then re-implement V2/V4 success through thesis-backed derivation paths only. Keep each change isolated and verified with narrow test slices before full-gate verification.

**Tech Stack:** Haskell (GHC/Cabal), Hspec, existing Phase-6/Phase-7 pipeline modules (`MLF.Elab.*`, `MLF.Constraint.Presolution.*`), thesis refs (`papers/these-finale-english.txt`, `papers/xmlf.txt`).

---

### Task 0: Preflight In Dedicated Worktree

**Files:**
- Modify: none (environment setup)

**Step 1: Create and switch to dedicated worktree**

Run:
```bash
git worktree add ../mlf4-thesis-exact-bug003 -b codex/bug003-thesis-exact
cd ../mlf4-thesis-exact-bug003
```
Expected: new worktree on branch `codex/bug003-thesis-exact`.

**Step 2: Baseline full verification**

Run:
```bash
cabal build all && cabal test
```
Expected: PASS (baseline before thesis-exact tightening).

**Step 3: Commit checkpoint marker**

Run:
```bash
git commit --allow-empty -m "chore: start thesis-exact BUG-2026-02-11-003 follow-up"
```

---

### Task 1: Add Strict-Theory Regression Harness (Red First)

**Files:**
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/TypeCheckSpec.hs`
- Modify: `test/SpecUtil.hs`

**Step 1: Write failing strict-instantiation tests**

Add in `test/TypeCheckSpec.hs`:
```haskell
it "strict InstBot rejects non-bottom equal bound" $ do
  let ty = TForall "a" (Just (TBase (BaseTy "Int"))) (TVar "a")
      term = ETyInst (ETyAbs "a" (Just (TBase (BaseTy "Int"))) (EVar "x")) (InstApp (TBase (BaseTy "Int")))
  -- Replace with concrete strict-check helper once introduced.
  pendingWith "typeCheckStrict helper not yet implemented"
```

Add BUG-004 strict shadow tests in `test/ElaborationSpec.hs` (new block):
```haskell
it "BUG-004-V2 strict checker shadow stays Int" $ pending
it "BUG-004-V4 strict checker shadow stays Int" $ pending
```

**Step 2: Run targeted tests to confirm RED**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "strict InstBot|BUG-004-V[24] strict"'
```
Expected: FAIL/PENDING due missing strict-check plumbing.

**Step 3: Commit failing-test scaffold**

Run:
```bash
git add test/TypeCheckSpec.hs test/ElaborationSpec.hs test/SpecUtil.hs
git commit -m "test: scaffold strict-theory harness for BUG-004"
```

---

### Task 2: Introduce Explicit Strict Policy API In TypeCheck/Inst

**Files:**
- Modify: `src/MLF/Elab/TypeCheck.hs`
- Modify: `src/MLF/Elab/Inst.hs`
- Modify: `src/MLF/Elab/Pipeline.hs` (exports if needed)
- Test: `test/TypeCheckSpec.hs`

**Step 1: Write failing API-usage tests**

In `test/TypeCheckSpec.hs`, call strict helpers directly:
```haskell
it "typeCheckStrict rejects InstBot on non-bottom" $ do
  -- expect Left TCInstantiationError ... "InstBot expects TBottom"
```

**Step 2: Run targeted tests (RED)**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "typeCheckStrict|strict InstBot"'
```
Expected: compile/test fail until API exists.

**Step 3: Implement minimal strict API**

In `src/MLF/Elab/TypeCheck.hs`, add:
```haskell
data InstBotMode = InstBotStrict | InstBotCompat

typeCheckWithMode :: InstBotMode -> ElabTerm -> Either TypeCheckError ElabType
checkInstantiationWithMode :: InstBotMode -> Env -> ElabType -> Instantiation -> Either TypeCheckError ElabType
```

In `src/MLF/Elab/Inst.hs`, add corresponding mode-aware evaluator entrypoint (or strict helper) so strict semantics is testable independently.

**Step 4: Re-run targeted tests (GREEN for mode plumbing)**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "typeCheckStrict|strict InstBot"'
```
Expected: strict-mode tests execute and fail/succeed according to current semantics assertions.

**Step 5: Commit**

Run:
```bash
git add src/MLF/Elab/TypeCheck.hs src/MLF/Elab/Inst.hs src/MLF/Elab/Pipeline.hs test/TypeCheckSpec.hs
git commit -m "refactor: add explicit strict vs compat InstBot policy"
```

---

### Task 3: Remove Runtime Compat Relaxation (Intentional RED For BUG-004)

**Files:**
- Modify: `src/MLF/Elab/TypeCheck.hs`
- Modify: `src/MLF/Elab/Inst.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Switch production path to strict-only `InstBot`**

Remove equality fallback branches:
```haskell
_ | alphaEqType t tArg -> ...
```
from both checker/runtime instantiation evaluation.

**Step 2: Run BUG-004 slices to confirm expected RED**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V[24]"'
```
Expected: FAIL (proves previous success depended on compat behavior).

**Step 3: Commit strict semantic cutover**

Run:
```bash
git add src/MLF/Elab/TypeCheck.hs src/MLF/Elab/Inst.hs
git commit -m "refactor: enforce strict InstBot semantics"
```

---

### Task 4: Remove Non-Thesis Reify Fallback Synthesis (Intentional RED)

**Files:**
- Modify: `src/MLF/Elab/Elaborate.hs`
- Test: `test/ElaborationSpec.hs`

**Step 1: Delete fallback instantiation synthesis block**

Remove logic that fabricates instantiations when `phi == InstId`:
- `ExpInstantiate` arg harvesting from traces
- `fallbackFromSchemeBound`
- single-binder var-name alignment (`substTypeCapture`)

Keep only paper path:
```haskell
phi <- phiFromEdgeWitnessWithTrace ...
pure phi
```

**Step 2: Run targeted RED check**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V[24]"'
```
Expected: FAIL until upstream producer-side fixes are implemented.

**Step 3: Commit fallback removal**

Run:
```bash
git add src/MLF/Elab/Elaborate.hs
git commit -m "refactor: remove non-thesis reify fallback synthesis"
```

---

### Task 5: Implement Thesis-Backed Producer Fix For V2/V4

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Plan/Finalize.hs`
- Modify: `src/MLF/Elab/Phi/Omega.hs`
- Modify: `src/MLF/Elab/Elaborate.hs`
- Possibly modify: `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`
- Possibly modify: `src/MLF/Elab/Run/Annotation.hs`
- Test: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`

**Step 1: Keep/confirm V2 paper-safe ownership fix**

Retain quantifier-identity ownership alignment:
- quantified names in finalize `usedNames`
- scheme-arity-aware reorder identity requirement in Omega.

**Step 2: Replace bounded-identity heuristic with explicit annotation/coercion derivation rule**

In desugared `ELamAnn` path, implement structure-based conversion only when annotation scheme is exactly coercion-domain form:
```haskell
coercionDomainTy (Forall [(v, Just bnd)] (TVar v')) | v == v' = Just (tyToElab bnd)
coercionDomainTy _ = Nothing
```
Use this instead of free-variable heuristic collapse.

**Step 3: Ensure BUG-004 terms elaborate to strict-checkable terms**

Add/adjust tests asserting:
- both pipelines return `Int`
- strict shadow checker (`typeCheckWithMode InstBotStrict`) also returns `Int`.

**Step 4: Run focused GREEN set**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V[24]|does not leak solved-node names in make let mismatch|id @ Int|interleaves StepIntro"'
```
Expected: PASS.

**Step 5: Commit producer-side thesis fix**

Run:
```bash
git add src/MLF/Constraint/Presolution/Plan/Finalize.hs src/MLF/Elab/Phi/Omega.hs src/MLF/Elab/Elaborate.hs src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs src/MLF/Elab/Run/Annotation.hs test/ElaborationSpec.hs test/PipelineSpec.hs
# If some optional files are untouched, omit them from git add
git commit -m "fix: restore BUG-004 via thesis-backed Φ/annotation derivation"
```

---

### Task 6: Drop Compat Mode + Lock Thesis-Exact Contract

**Files:**
- Modify: `src/MLF/Elab/TypeCheck.hs`
- Modify: `src/MLF/Elab/Inst.hs`
- Modify: `test/TypeCheckSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Remove compat API branches**

Make strict behavior the only behavior; delete temporary mode toggles if introduced.

**Step 2: Tighten tests to prevent regression**

Add negative assertions:
```haskell
it "rejects InstBot on non-bottom in final checker" $ ...
```

**Step 3: Run targeted suite**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "strict InstBot|BUG-004-V[24]"'
```
Expected: PASS.

**Step 4: Commit cleanup**

Run:
```bash
git add src/MLF/Elab/TypeCheck.hs src/MLF/Elab/Inst.hs test/TypeCheckSpec.hs test/ElaborationSpec.hs
git commit -m "cleanup: remove compat InstBot path and lock strict contract"
```

---

### Task 7: Docs + Bug Tracker + Full Verification

**Files:**
- Modify: `Bugs.md`
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`
- Modify: `TODO.md`

**Step 1: Update docs with explicit thesis mapping**

Document removed deviations and final exactness argument with code references.

**Step 2: Reclassify bug status if fully exact**

If strict checker + no fallback + full gate all green, mark `BUG-2026-02-11-003` as thesis-exact resolved (not just pragmatic resolved).

**Step 3: Full gate**

Run:
```bash
cabal build all && cabal test
```
Expected: PASS.

**Step 4: Commit docs/verification**

Run:
```bash
git add Bugs.md implementation_notes.md CHANGELOG.md TODO.md
git commit -m "docs: record thesis-exact BUG-2026-02-11-003 closure"
```

---

### Task 8: Final Audit Checklist

**Files:**
- Review-only: all touched files

**Step 1: Diff audit for non-thesis constructs**

Run:
```bash
rg -n "fallbackFromSchemeBound|allowFallbackFromTrace|alphaEqType t tArg|collapseClosedBoundedIdentity" src/MLF
```
Expected: no remaining forbidden compatibility paths.

**Step 2: Re-run key theorem-alignment slices**

Run:
```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-004-V|Phase 3 atomic wrapping equivalence gates|witness instantiation matches solved edge types|TypeCheck"'
```
Expected: PASS.

**Step 3: Commit final audit marker**

Run:
```bash
git commit --allow-empty -m "chore: finalize thesis-exact BUG-2026-02-11-003 audit"
```
