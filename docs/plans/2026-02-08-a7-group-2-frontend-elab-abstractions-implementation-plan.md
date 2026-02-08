# A7 Group 2: Frontend/Elab Abstractions Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove duplicated scope-wiring and annotation traversal logic in frontend constraint generation and elaboration-run utilities.

**Architecture:** Extract local combinators inside `Translate` for scope lifecycle and bind-parent attachment, then refactor repeated call sites (`let`, coercion, forall-internalization). In Elab run utilities, define one shared fold/traversal helper in `Annotation` and reuse it from both `Annotation` and `Debug` modules.

**Tech Stack:** Haskell, Cabal, Hspec, `recursion-schemes` (`cata`).

---

## Skills & Constraints
- Use @haskell-pro for helper API shape.
- Use @test-driven-development with focused failing tests first.
- Use @verification-before-completion before success claims.

### Task 1: Add Red Tests for Scope Wiring Invariants in Constraint Generation

**Files:**
- Modify: `test/ConstraintGenSpec.hs`

**Step 1: Write the failing test**

Add a focused test that constrains let/coercion scope parent relationships:

```haskell
it "coercion and let scope wiring preserve single-parent invariant" $ do
  let expr = ELet "id" (EAnn (ELam "x" (EVar "x")) (STArrow (STBase "Int") (STBase "Int"))) (EVar "id")
  result <- requireRight (inferConstraintGraphDefault expr)
  checkBindingTree (crConstraint result) `shouldBe` Right ()
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='scope wiring preserve single-parent invariant'`
Expected: FAIL if scope wiring currently drifts in one of the duplicated branches.

**Step 3: Write minimal implementation**

In `src/MLF/Frontend/ConstraintGen/Translate.hs`, add helpers:

```haskell
withScopedBuild
  :: ConstraintM a
  -> ConstraintM (a, Scope.ScopeFrame)
withScopedBuild action = do
  Scope.pushScope
  out <- action
  frame <- Scope.popScope
  pure (out, frame)

attachUnder :: NodeRef -> NodeRef -> BindFlag -> ConstraintM ()
attachUnder child parent flag = setBindParentOverride child parent flag

rebindScopeRoot :: NodeRef -> NodeId -> Scope.ScopeFrame -> ConstraintM ()
rebindScopeRoot binder root frame = Scope.rebindScopeNodes binder root frame
```

Then replace repeated push/pop/rebind/override blocks in let/coerce/forall paths with these helpers.

**Step 4: Run test to verify it passes**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='coercion and let scope wiring'`
Expected: PASS.

**Step 5: Commit**

```bash
git add test/ConstraintGenSpec.hs src/MLF/Frontend/ConstraintGen/Translate.hs
git commit -m "refactor(constraint-gen): deduplicate scope lifecycle and parent wiring helpers"
```

### Task 2: Add Red Tests for Shared AnnExpr Traversal Contract

**Files:**
- Modify: `test/PipelineSpec.hs`
- Modify: `src/MLF/Elab/Run/Annotation.hs`

**Step 1: Write the failing test**

Add tests that enforce node-rewrite consistency across annotation utilities:

```haskell
it "applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently" $ do
  let expr = unsafeNormalize (ELam "x" (EVar "x"))
  case runPipelineWithPresolution expr of
    Left err -> expectationFailure err
    Right (pres, ann) -> do
      let ann' = applyRedirectsToAnn (prRedirects pres) ann
      annNode ann' `shouldSatisfy` (\_ -> True)
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='rewrite every node occurrence consistently'`
Expected: FAIL while shared traversal helper is not yet wired (or exposes mismatch in one branch).

**Step 3: Write minimal implementation**

Refactor `src/MLF/Elab/Run/Annotation.hs`:

```haskell
mapAnnNodes :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
mapAnnNodes f = cata $ \case
  ALitF l n -> ALit l (f n)
  AVarF v n -> AVar v (f n)
  ALamF v p x b n -> ALam v (f p) x b (f n)
  AAppF fn arg fe ae n -> AApp fn arg fe ae (f n)
  ALetF v sg sr ev rg rhs body n -> ALet v sg (f sr) ev rg rhs body (f n)
  AAnnF inner n eid -> AAnn inner (f n) eid
```

Then define:
- `applyRedirectsToAnn redirects = mapAnnNodes (chaseRedirects redirects)`
- `canonicalizeAnn canonical = mapAnnNodes canonical`
- Refactor `edgeOrigins` in `Debug.hs` to reuse shared traversal shape helpers (single constructor walk source).

**Step 4: Run test to verify it passes**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='rewrite every node occurrence consistently'`
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='Pipeline'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Elab/Run/Annotation.hs src/MLF/Elab/Run/Debug.hs test/PipelineSpec.hs
git commit -m "refactor(elab-run): centralize AnnExpr node traversal and reuse across utilities"
```

### Task 3: Tighten Regression Coverage for Translate Refactor

**Files:**
- Modify: `test/ConstraintGenSpec.hs`
- Modify: `test/ElaborationSpec.hs`

**Step 1: Write the failing test**

Add a regression around nested forall/coercion in let where scope rebind is sensitive:

```haskell
it "nested forall coercion paths preserve valid binding tree" $ do
  let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
      expr = ELet "f" (EAnn (ELam "x" (EVar "x")) ann) (EApp (EVar "f") (ELit (LInt 1)))
  result <- requireRight (inferConstraintGraphDefault expr)
  checkBindingTree (crConstraint result) `shouldBe` Right ()
```

**Step 2: Run test to verify it fails**

Run: `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='nested forall coercion paths preserve valid binding tree'`
Expected: FAIL if any translated branch still uses diverging wiring behavior.

**Step 3: Write minimal implementation**

Apply helper combinators to all remaining duplicated regions in `Translate.hs`:
- `ELet` inferred + buildLet path
- `buildCoerce`
- `internalizeCoercionCopy` (`STArrow`, `STBase`, `STCon`, `STForall` branches)

Keep semantics unchanged.

**Step 4: Run test to verify it passes**

Run:
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='nested forall coercion paths preserve valid binding tree'`
- `cabal test mlf2-test --test-show-details=direct --test-option='-m' --test-option='ConstraintGen'`

Expected: PASS.

**Step 5: Commit**

```bash
git add src/MLF/Frontend/ConstraintGen/Translate.hs test/ConstraintGenSpec.hs test/ElaborationSpec.hs
git commit -m "test+refactor(constraint-gen): complete scope-wiring dedup with regressions"
```

### Task 4: Group Verification and Docs

**Files:**
- Modify: `implementation_notes.md`
- Modify: `CHANGELOG.md`

**Step 1: Write the failing test**

No new test code. Create a short checklist entry for the deduplicated regions.

**Step 2: Run verification gate**

Run: `cabal build all && cabal test`
Expected: PASS.

**Step 3: Write docs**

Add concise entries describing:
- new local translate combinators
- shared AnnExpr traversal helper

**Step 4: Re-run targeted tests**

Run: `cabal test mlf2-test --test-show-details=direct --test-option=-m --test-option='Pipeline (Phases 1-5)' --test-option=-m --test-option='Phase 1' --test-option=-m --test-option='Phase 6'`
Expected: PASS.

**Step 5: Commit**

```bash
git add implementation_notes.md CHANGELOG.md
git commit -m "docs(elab-frontend): document abstraction dedup for A7 group 2"
```
