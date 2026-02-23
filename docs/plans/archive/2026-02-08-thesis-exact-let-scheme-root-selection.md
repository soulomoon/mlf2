# Thesis-Exact Let Scheme Root Selection Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore thesis-exact let generalization so `BUG-2026-02-06-002` elaborates/typechecks to `Int` by keeping let schemes polymorphic and pushing specialization to edge instantiations.

**Architecture:** Apply direction 1 first by fixing plan-time root selection in `TargetPlan` and `TypeRootPlan` so let-scheme generalization does not reify use-site-specialized bounds. Add red/green tests at both integration and plan-unit levels to prove behavior. Only execute direction 2 (`MLF.Elab.Run.Scope`) if post-fix evidence shows scope-owner drift.

**Tech Stack:** Haskell (GHC2021), Cabal, Hspec, `MLF.Constraint.Presolution.Plan.*`, `MLF.Elab.*`, thesis reference `papers/these-finale-english.txt`.

---

## Thesis Anchors (must remain true)

1. Let translation uses the alternative let constraint with trivial scheme to simplify scope handling (`papers/these-finale-english.txt:17287`).
2. For let, elaboration reads the let-bound variable’s scheme (`papers/these-finale-english.txt:17687`).
3. `T(let x = a in b)` coerces `b` to the root scheme type (`papers/these-finale-english.txt:18068`).
4. `Typ` vs `Typexp` may differ only by quantifier ordering (`papers/these-finale-english.txt:17605`).

---

### Task 1: Add Red Tests for Direction 1 (plan-level + integration)

**Files:**
- Create: `test/Presolution/TargetTypeRootPlanSpec.hs`
- Modify: `test/ElaborationSpec.hs`
- Modify: `mlf2.cabal`
- Modify: `test/Main.hs`

**Step 1: Write the failing plan-unit test module**

```haskell
module Presolution.TargetTypeRootPlanSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Test.Hspec

import MLF.Constraint.Presolution.Plan.Target.TargetPlan
import MLF.Constraint.Presolution.Plan.Target.TypeRootPlan
import MLF.Constraint.Types
import SpecUtil (emptyConstraint)

spec :: Spec
spec = describe "Target/TypeRoot let-scheme root handling" $ do
    it "marks a local scheme root as let-scheme-root (new flag)" $ do
        let gid = GenNodeId 1
            target = NodeId 10
            bound = NodeId 11
            dom = NodeId 12
            cod = NodeId 13
            nodes = IntMap.fromList
                [ (10, TyVar target (Just bound))
                , (11, TyArrow bound dom cod)
                , (12, TyVar dom Nothing)
                , (13, TyVar cod Nothing)
                ]
            c = emptyConstraint
                { cNodes = NodeMap nodes
                , cGenNodes = fromListGen [(gid, GenNode gid [target])]
                , cBindParents = IntMap.fromList
                    [ (nodeRefKey (typeRef target), (genRef gid, BindFlex))
                    , (nodeRefKey (typeRef bound), (typeRef target, BindFlex))
                    ]
                }
            tp = buildTargetPlan TargetPlanInput
                { tpiConstraint = c
                , tpiNodes = nodes
                , tpiCanonical = id
                , tpiCanonKey = getNodeId
                , tpiIsTyVarKey = \k -> case IntMap.lookup k nodes of
                    Just TyVar{} -> True
                    _ -> False
                , tpiScopeGen = Just gid
                , tpiScopeRootC = genRef gid
                , tpiBindParents = cBindParents c
                , tpiTarget0 = target
                , tpiSchemeRootKeySetRaw = IntSet.singleton (getNodeId target)
                , tpiSchemeRootKeySet = IntSet.singleton (getNodeId target)
                , tpiSchemeRootOwnerBase = IntMap.singleton (getNodeId target) gid
                , tpiSchemeRootByBodyBase = IntMap.empty
                , tpiContainsForallForTarget = const False
                , tpiFirstGenAncestor = const (Just gid)
                , tpiReachableFromWithBounds = const IntSet.empty
                , tpiBindParentsGa = Nothing
                }
        tpTargetIsSchemeRootForScope tp `shouldBe` True
        tpTargetIsLetSchemeRoot tp `shouldBe` True

    it "does not choose bound-derived type root for local let-scheme-root target" $ do
        let target = NodeId 10
            bound = NodeId 11
            out = buildTypeRootPlan TypeRootPlanInput
                { trpiNodes = IntMap.fromList
                    [ (10, TyVar target (Just bound))
                    , (11, TyArrow bound (NodeId 12) (NodeId 13))
                    ]
                , trpiCanonical = id
                , trpiCanonKey = getNodeId
                , trpiIsTyVarKey = (== 10)
                , trpiIsBaseLikeKey = const False
                , trpiBindParents = IntMap.empty
                , trpiScopeRootC = genRef (GenNodeId 1)
                , trpiScopeGen = Just (GenNodeId 1)
                , trpiTarget0 = target
                , trpiTargetBound = Just bound
                , trpiTargetIsSchemeRoot = True
                , trpiTargetIsSchemeRootForScope = True
                , trpiTargetIsTyVar = True
                , trpiTargetIsLetSchemeRoot = True
                , trpiTargetBoundUnderOtherGen = False
                , trpiNamedUnderGaSet = IntSet.singleton 10
                , trpiTypeRoot0 = target
                , trpiTypeRootFromBoundVar = Nothing
                , trpiTypeRootHasNamedOutsideGamma = False
                , trpiBoundHasForallForVar = const False
                , trpiSchemeRootByBody = IntMap.singleton (getNodeId bound) target
                , trpiSchemeRootOwner = IntMap.singleton (getNodeId target) (GenNodeId 1)
                , trpiLiftToForall = id
                }
        trUseBoundTypeRoot out `shouldBe` False
        trTypeRoot out `shouldBe` target
```

**Step 2: Wire test module into the suite**

```haskell
-- test/Main.hs
import qualified Presolution.TargetTypeRootPlanSpec
...
Presolution.TargetTypeRootPlanSpec.spec
```

```cabal
-- mlf2.cabal (test-suite mlf2-test other-modules)
Presolution.TargetTypeRootPlanSpec,
```

**Step 3: Write the failing integration test for BUG-2026-02-06-002**

```haskell
it "BUG-2026-02-06-002 polymorphic factory elaborates to Int (thesis-exact let scheme)" $ do
    let expr =
            ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                    (EApp (EVar "c1") (ELit (LBool True))))

    (_term, ty) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalize expr))
    ty `shouldBe` Elab.TBase (BaseTy "Int")

    (_checkedTerm, checkedTy) <- requireRight (Elab.runPipelineElabChecked Set.empty (unsafeNormalize expr))
    checkedTy `shouldBe` Elab.TBase (BaseTy "Int")
```

**Step 4: Run tests to verify RED**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match="Target/TypeRoot let-scheme root handling|BUG-2026-02-06-002"'`
Expected: FAIL (compile-time for new fields and/or runtime `TCLetTypeMismatch` for bug test).

**Step 5: Commit test-only RED checkpoint**

```bash
git add test/Presolution/TargetTypeRootPlanSpec.hs test/ElaborationSpec.hs test/Main.hs mlf2.cabal
git commit -m "test: add red regressions for thesis-exact let-scheme root selection"
```

---

### Task 2: Implement Direction 1 in TargetPlan/TypeRootPlan (minimal fix)

**Files:**
- Modify: `src/MLF/Constraint/Presolution/Plan/Target/TargetPlan.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan/Target/TypeRootPlan.hs`
- Modify: `src/MLF/Constraint/Presolution/Plan.hs`

**Step 1: Add explicit let-scheme-root classification in TargetPlan**

```haskell
-- TargetPlan.hs
data TargetPlan = TargetPlan
    { ...
    , tpTargetIsLetSchemeRoot :: Bool
    }

targetIsLetSchemeRootLocal :: Bool
targetIsLetSchemeRootLocal =
    targetIsTyVarLocal
        && targetIsSchemeRootForScopeLocal
```

**Step 2: Plumb the new flag through TypeRootPlanInput**

```haskell
-- TypeRootPlan.hs
data TypeRootPlanInput = TypeRootPlanInput
    { ...
    , trpiTargetIsLetSchemeRoot :: Bool
    }
```

```haskell
-- Plan.hs (TypeRootPlanInput call site)
, trpiTargetIsLetSchemeRoot = targetIsLetSchemeRoot
```

**Step 3: Gate bound-derived root selection for let-scheme-root targets**

```haskell
-- TypeRootPlan.hs
useBoundTypeRootLocal =
    not targetIsSchemeRoot
        && not targetIsLetSchemeRoot
        && case targetBound of
            Just bnd -> IntMap.member (getNodeId (canonical bnd)) schemeRootByBody
            Nothing -> False

typeRoot0Local =
    if targetIsLetSchemeRoot
        then typeRoot0
        else ...existing branches...
```

**Step 4: Run focused tests to verify GREEN for direction 1**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match="Target/TypeRoot let-scheme root handling|BUG-2026-02-06-002"'`
Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Constraint/Presolution/Plan/Target/TargetPlan.hs src/MLF/Constraint/Presolution/Plan/Target/TypeRootPlan.hs src/MLF/Constraint/Presolution/Plan.hs
git commit -m "fix: keep let scheme roots out of bound-derived type-root selection"
```

---

### Task 3: Add Thesis-Behavior Regression Coverage Around Let Polymorphism

**Files:**
- Modify: `test/ElaborationSpec.hs`
- Modify: `test/PipelineSpec.hs`

**Step 1: Add end-to-end test that let scheme stays polymorphic while use-site specializes**

```haskell
it "let polymorphism remains on binder while specialization happens at use-site" $ do
    let expr =
            ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                (ELet "c1" (EApp (EVar "make") (ELit (LInt 2)))
                    (EApp (EVar "c1") (ELit (LBool True))))
    (_term, ty) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalize expr))
    ty `shouldBe` Elab.TBase (BaseTy "Int")
```

**Step 2: Add checked-authoritative parity assertion for the same expression**

```haskell
let ...
(_uTerm, uTy) <- requireRight (Elab.runPipelineElab Set.empty (unsafeNormalize expr))
(_cTerm, cTy) <- requireRight (Elab.runPipelineElabChecked Set.empty (unsafeNormalize expr))
uTy `shouldBe` cTy
```

**Step 3: Run targeted suite**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match="BUG-2026-02-06-002|let polymorphism remains on binder"'`
Expected: PASS.

**Step 4: Run surrounding regression cluster**

Run: `cabal test mlf2-test --test-show-details=direct --test-options='--match="generalizes reused constructors via make const|redirected let-use sites keep polymorphic schemes|BUG-2026-02-08-004"'`
Expected: Existing known statuses remain unchanged except intended bug fix path.

**Step 5: Commit**

```bash
git add test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "test: lock thesis-exact let-polymorphism behavior for make factory path"
```

---

### Task 4: Evidence Gate for Direction 2 (scope-owner drift)

**Files:**
- No file changes unless gate fails.
- Conditional files if gate fails: `src/MLF/Elab/Run/Scope.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`.

**Step 1: Run post-fix reproducer with generalization traces**

Run:

```bash
cabal repl mlf2 <<'GHCI'
:set -ignore-dot-ghci
let cfg = defaultPipelineConfig { pcTraceConfig = defaultTraceConfig { tcGeneralize = True } }
let expr = ELet "make" (ELam "x" (ELam "y" (EVar "x"))) (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4)))) (EApp (EVar "c1") (ELit (LBool True))))
print (case normalizeExpr expr of Left e -> Left (show e); Right ne -> first renderPipelineError (runPipelineElabWithConfig cfg mempty ne))
:quit
GHCI
```

Expected: success result; no `TCLetTypeMismatch`.

**Step 2: Decide whether direction 2 is needed**

Gate criterion: If tests still fail and traces show let binder generalized under a non-owning scope root (`scopeRootC` mismatch for let scheme root generalization), proceed to conditional Step 3. Otherwise skip Step 3.

**Step 3 (conditional): Implement direction 2 scope-owner fix**

```haskell
-- Scope.hs (example target)
-- ensure letScopeOverrides preserves owning GenRef for scheme roots across canonicalization
```

Then rerun Task 3 tests and commit:

```bash
git add src/MLF/Elab/Run/Scope.hs test/ElaborationSpec.hs test/PipelineSpec.hs
git commit -m "fix: preserve owning let scope in generalization scope overrides"
```

---

### Task 5: Update Tracking and Documentation

**Files:**
- Modify: `Bugs.md`
- Modify: `CHANGELOG.md`
- Modify: `implementation_notes.md`
- Modify: `TODO.md`

**Step 1: Update bug record**

- Move `BUG-2026-02-06-002` from Open to Resolved if fixed.
- Include resolved date and regression test links:
  - `test/ElaborationSpec.hs`
  - `test/Presolution/TargetTypeRootPlanSpec.hs`

**Step 2: Add changelog entry**

- Add concise entry for thesis-exact let-scheme-root selection fix and bug ID.

**Step 3: Update implementation notes**

- Document that plan-level type root selection now preserves local let scheme roots per thesis sections on let translation.

**Step 4: Update TODO priorities**

- Remove or demote follow-up items covered by this fix.
- Keep direction 2 listed only if gate in Task 4 triggered.

**Step 5: Commit**

```bash
git add Bugs.md CHANGELOG.md implementation_notes.md TODO.md
git commit -m "docs: record thesis-exact let-scheme-root fix and close BUG-2026-02-06-002"
```

---

### Task 6: Final Verification Gate

**Files:**
- No code changes.

**Step 1: Run focused regression checks**

Run:

```bash
cabal test mlf2-test --test-show-details=direct --test-options='--match="BUG-2026-02-06-002|generalizes reused constructors via make const|redirected let-use sites keep polymorphic schemes"'
```

Expected: PASS.

**Step 2: Run full project validation (repo-required gate)**

Run: `cabal build all && cabal test`
Expected: exit code 0.

**Step 3: Inspect git diff for accidental edits**

Run: `git status --short`
Expected: only intended files are modified.

**Step 4: Record verification output in progress artifacts**

- Add exact commands and outcomes to task progress notes.

**Step 5: Final commit if needed**

```bash
git add -A
git commit -m "chore: verification run for thesis-exact let-scheme-root selection"
```

---

## Notes on DRY / YAGNI / Safety

- Keep direction 1 strictly limited to `TargetPlan` + `TypeRootPlan` + their plan wiring.
- Do not alter `MLF.Elab.Run.Scope` unless Task 4 gate shows scope-owner drift.
- Reuse existing helpers (`SpecUtil`, `unsafeNormalize`, `requireRight`) instead of creating parallel test utilities.
- Avoid unrelated refactors while touching plan modules.
