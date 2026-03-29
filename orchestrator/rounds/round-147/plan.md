# Round 147 — Plan: Item-2 Phase 6 Fix Alias-Bound Resolution for Recursive Types

## Selected Item

- **Item id:** `item-2`
- **Title:** Phase 6: Fix alias-bound resolution for recursive types

## Roadmap Reference

- **`roadmap_id`:** `2026-03-29-02-iso-recursive-inference-gap-fixes`
- **`roadmap_revision`:** `rev-001`
- **`roadmap_dir`:** `orchestrator/roadmaps/2026-03-29-02-iso-recursive-inference-gap-fixes/rev-001`

## Scope & Constraints

The implementer works in worktree `orchestrator/worktrees/round-147/` on branch
`orchestrator/round-147-alias-bounds-fix`. The implementer must **not** directly
modify `orchestrator/state.json` (controller-owned).

## Root Cause Analysis

### Symptom

Two tests in `PipelineSpec.hs` fail with "alias bounds survived scheme
finalization" during Phase 6 (elaboration):

- **μ/∀ interaction** (`PipelineSpec.hs:1397`): `let id = λx.x in let f = λx. f (id x) in f`
- **Higher-order recursion** (`PipelineSpec.hs:1420`): `let f = λx.λy. f x y in f`

Both currently assert the Phase 6 error as expected behavior.

### The Roadmap's Stated Location Is Wrong

The roadmap and selection documents point to `Finalize.hs:502` as the error
source and `simplifySchemeBindings` / `substBound` as the functions to fix.
**This is incorrect.** Detailed analysis shows:

1. **`Finalize.hs:338-342`** computes `aliasBounds` by filtering for
   `isVarBound bound` where `bound :: BoundType = Ty 'NoTopVar`. But the GADT
   definition (`TVar :: String -> Ty 'AllowVar`, line 88 of `Elab.hs`) means
   `TVar` can **never** inhabit `Ty 'NoTopVar`. Therefore `isVarBound` always
   returns `False` for `BoundType`, and `aliasBounds` is always `[]`. The check
   at **`Finalize.hs:502-509` is dead code** — it can never fire.

2. **The real error source is `ReifyPlan.hs:626-632`**, inside the `bindingFor`
   function. Here, `mbBound :: Maybe ElabType` (which CAN contain `TVar` since
   `ElabType = Ty 'AllowVar`) is matched against `Just (TVar _)`, which
   triggers the same error message.

3. In `Generalize.hs`, `bindingFor` (line 376) runs **before**
   `finalizeScheme` (line 673), so the `ReifyPlan` error is hit first and
   prevents execution from ever reaching `Finalize`.

### Error Path in Detail

Inside `bindingFor` (`ReifyPlan.hs:284-639`):

1. For each binder in the scheme, the function computes `boundTy :: ElabType`
   through type reification + alias substitution (lines 465-530).

2. For recursive types, `boundTy` resolves to `TVar v` where `v` names another
   binder in the same scheme. This happens because the constraint graph cycle
   prevents resolution to a concrete type — the reification walks through the
   cycle and lands on a named binder node.

3. Several existing filters attempt to normalize problematic bounds to `Nothing`:
   - `boundIsFreeVar'` (line 591-592): Does NOT catch this case because the
     bound node IS in the `subst` map (it's a named binder, not truly free).
   - `boundIsSelfVar` (line 593-597): Does NOT catch this case because
     `v /= name` (it's a *different* binder, not the binder itself).
   - `boundMentionsBinderVar` (line 598-599): Does NOT catch this case because
     `TVar v` where `v /= name` has `freeTypeVarsType = {v}`, which does not
     contain `name`.
   - `boundAllowed` (line 604-616): PASSES because the bound node is in the
     `binderSet`.

4. So `mbBound = Just (TVar v)` survives all filters and reaches line 626.

5. At line 626-632, `mbBoundTyped` matches `Just (TVar _)` and hard-stops with
   `ValidationFailed ["alias bounds survived scheme finalization: " ++ ...]`.

### Why This Is Safe to Fix

The `TVar` hard-stop at line 626 exists because `elabToBound :: ElabType ->
Either String BoundType` (line 154 of `Elab.hs`) rejects `TVar` — it cannot
be converted to `BoundType` due to the GADT constraint. The hard-stop is a
pre-check that gives a better error message than `elabToBound`'s generic error.

For recursive types, the alias bound `∀(a ≤ b)(b ≤ T). body` with `a ≤ b`
dropped becomes `∀(a)(b ≤ T). body` — strictly more general. This is a safe
over-approximation: the binder `a` loses its bound but gains generality.
Non-recursive types are unaffected because they never produce `TVar`-valued
`mbBound` at this point (the existing filters handle non-recursive cases).

## Steps

### Step 1: Normalize inter-binder alias bounds to unbounded in `bindingFor`

**File:** `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`

**Location:** Lines 626-636 (the `mbBoundTyped` computation)

**Change:** Instead of hard-stopping on `Just (TVar v)`, check whether `v`
refers to another binder in the scheme's binder set. If so, normalize the
bound to `Nothing` (unbounded). If the `TVar` does NOT refer to a known
binder, preserve the existing hard-stop as a genuine invariant violation.

```haskell
-- BEFORE (lines 626-636):
        mbBoundTyped = case mbBound of
            Just (TVar _) ->
                Left $
                    ValidationFailed
                        [ "alias bounds survived scheme finalization: "
                            ++ show [name]
                        ]
            Nothing -> Right Nothing
            Just bnd -> case elabToBound bnd of
                Left err -> Left $ ValidationFailed [err]
                Right typed -> Right (Just typed)

-- AFTER:
        mbBoundTyped = case mbBound of
            Just (TVar v)
                | v `Set.member` binderNameSet ->
                    -- Inter-binder alias bound from recursive cycle;
                    -- normalize to unbounded (safe over-approximation).
                    Right Nothing
                | otherwise ->
                    Left $
                        ValidationFailed
                            [ "alias bounds survived scheme finalization: "
                                ++ show [name]
                            ]
            Nothing -> Right Nothing
            Just bnd -> case elabToBound bnd of
                Left err -> Left $ ValidationFailed [err]
                Right typed -> Right (Just typed)
```

This requires access to a `Set String` of binder names. The `bindingFor`
function already has `binderSet :: IntSet` (node IDs) and `bindings :: [(String, ...)]`
available from the `ReifyBindingEnv`. Construct the name set from the bindings
list.

**Auxiliary change (same function, near the top of `bindingFor`):** Add a
local binding:

```haskell
binderNameSet = Set.fromList [n | (n, _) <- bindings]
```

where `bindings` comes from the `ReifyBindingEnv` parameter. The exact field
name and extraction point should be determined from the function's existing
parameter destructuring (around lines 284-320).

**Rationale:** This is the minimal, targeted fix. It handles the two failing
test cases (both produce inter-binder `TVar` bounds due to recursive structure)
while preserving the hard-stop for unexpected `TVar` bounds that are NOT
inter-binder references — those would indicate a genuine bug elsewhere in the
pipeline.

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 2: Add a `{- Note [Inter-binder alias bounds in recursive types] -}` block

**File:** `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs`

**Location:** Near the `mbBoundTyped` computation (after line 636)

**Change:** Add a GHC-style Note block explaining:
- Why `TVar v` bounds arise for recursive types
- Why normalizing to unbounded is a safe over-approximation
- Reference to the GADT constraint that prevents `TVar` from inhabiting
  `BoundType`
- Reference to the dead-code check in `Finalize.hs:502-509`

```haskell
{- Note [Inter-binder alias bounds in recursive types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a recursive type introduces constraint graph cycles, type reification
may resolve a binder's bound to TVar v where v names another binder in the
same scheme. This happens because the cycle prevents resolution to a concrete
type.

The existing filters (boundIsFreeVar', boundIsSelfVar, boundMentionsBinderVar)
do not catch this case because:
  - The bound node IS in the subst map (not truly free)
  - v /= name (different binder, not self-referential)
  - TVar v has freeVars = {v}, which does not contain the current binder name

Since TVar cannot inhabit BoundType (GADT: TVar :: String -> Ty 'AllowVar,
BoundType = Ty 'NoTopVar), elabToBound would reject it. Rather than failing,
we normalize inter-binder alias bounds to Nothing (unbounded). This is a safe
over-approximation: ∀(a ≤ b)(b ≤ T). body becomes ∀(a)(b ≤ T). body, which
is strictly more general.

Note: Finalize.hs:502-509 has a parallel alias-bounds check using isVarBound
on BoundType values. That check is dead code because isVarBound matches TVar,
which can never inhabit BoundType. The real enforcement point is here.
-}
```

**Verification:**
```bash
cabal build mlf2-internal 2>&1 | grep -i error
```

### Step 3: Upgrade the μ/∀ interaction test at PipelineSpec.hs:1397

**File:** `test/PipelineSpec.hs`

**Location:** Lines 1397-1418

**Change:** Replace the "expects Phase 6 failure" test with a full pipeline
success assertion. The test currently:
1. Calls `expectAlignedPipelinePastPhase3 expr`
2. Verifies both pipeline runs return Phase 6 error with "alias bounds survived"
3. Asserts constraint contains `TyMu`

Replace with:
1. Keep the `expectAlignedPipelinePastPhase3` call and `constraintContainsTyMu`
   assertion (validates prerequisites)
2. Replace the `forM_ pipelineRuns` block with success assertions:

```haskell
-- Replace the current "expects failure" block with:
it "infers μ/∀ interaction types through full pipeline" $ do
    let expr =
          ELet
            "id"
            (ELam "x" (EVar "x"))
            (ELet "f" (ELam "x" (EApp (EVar "f") (EApp (EVar "id") (EVar "x")))) (EVar "f"))
    expectAlignedPipelinePastPhase3 expr
    cBroken <- automaticMuConstraint expr
    constraintContainsTyMu cBroken `shouldBe` True
    let pipelineRuns =
          [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr)),
            ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
          ]
    forM_ pipelineRuns $ \(label, result) ->
      case result of
        Left err ->
          expectationFailure
            (label ++ " failed: " ++ renderPipelineError err)
        Right (term, ty) ->
          typeCheck term `shouldBe` Right ty
```

**Verification:**
```bash
cabal test --test-show-details=direct --test-option='-m' --test-option='interaction'
```

### Step 4: Upgrade the higher-order recursion test at PipelineSpec.hs:1420

**File:** `test/PipelineSpec.hs`

**Location:** Lines 1420-1441

**Change:** Same pattern as Step 3. Replace the "expects Phase 6 failure"
assertions with full pipeline success assertions:

```haskell
it "infers higher-order recursion types through full pipeline" $ do
    let expr =
          ELet
            "f"
            (ELam "x" (ELam "y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))))
            (EVar "f")
    expectAlignedPipelinePastPhase3 expr
    cBroken <- automaticMuConstraint expr
    -- Note: this test currently asserts constraintContainsTyMu == False.
    -- Preserve this assertion — higher-order recursion may not introduce TyMu
    -- at the constraint level even though it triggers alias bounds.
    constraintContainsTyMu cBroken `shouldBe` False
    let pipelineRuns =
          [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr)),
            ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
          ]
    forM_ pipelineRuns $ \(label, result) ->
      case result of
        Left err ->
          expectationFailure
            (label ++ " failed: " ++ renderPipelineError err)
        Right (term, ty) ->
          typeCheck term `shouldBe` Right ty
```

**Verification:**
```bash
cabal test --test-show-details=direct --test-option='-m' --test-option='higher-order recursion'
```

### Step 5: Full build and test suite gate

**Commands:**
```bash
cabal build all && cabal test
```

**Verification:** Exit code 0, zero test failures. The entire 1175+ test suite
must pass, confirming:
- The μ/∀ interaction test now succeeds end-to-end
- The higher-order recursion test now succeeds end-to-end
- No regressions in non-recursive scheme finalization
- All other test families remain stable

### Step 6: Iterative debugging (conditional)

If Step 5 reveals failures:

1. **Regressions in existing tests** — The change in Step 1 only affects the
   `Just (TVar v)` code path. Non-recursive types should never produce
   `mbBound = Just (TVar v)` because the existing filters (`boundIsFreeVar'`,
   `boundIsSelfVar`, `boundMentionsBinderVar`) normalize those cases to
   `Nothing`. If a regression occurs, the `binderNameSet` membership check
   may be too broad. Tighten the guard by additionally checking whether the
   bound's constraint graph node participates in a TyMu cycle.

2. **The tests pass Phase 6 but fail in a later phase** — The pipeline may
   succeed through elaboration but produce a term that fails type checking.
   If `typeCheck term` fails:
   - The elaborated term's binder that lost its bound may cause the type
     checker to assign a more general type than expected. Check whether the
     xMLF type checker accepts this generalization.
   - If the type checker rejects, the bound normalization may need to produce
     a more specific fallback instead of `Nothing`.

3. **The tests fail with a DIFFERENT Phase 6 error** — If the `TVar` bound
   was masking a deeper issue (e.g., `elabToBound` fails on a different
   binder, or scheme finalization hits the dead-code `Finalize.hs` path for
   a non-`TVar` reason), investigate the new error message and determine
   whether additional `bindingFor` filter adjustments are needed.

4. **`constraintContainsTyMu` assertion changes** — The higher-order recursion
   test currently asserts `constraintContainsTyMu == False`. If the fix
   changes constraint generation behavior (unlikely, since Step 1 only affects
   reification), update this assertion.

**Verification:** Re-run `cabal build all && cabal test` after each fix.

## Deliverables

| Artifact | Path |
|----------|------|
| ReifyPlan fix (inter-binder alias bound normalization) | `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs` |
| Upgraded μ/∀ interaction test | `test/PipelineSpec.hs` |
| Upgraded higher-order recursion test | `test/PipelineSpec.hs` |

## Risk Assessment

- **Low risk:** Step 1 adds a guarded normalization to a single code path. The
  guard (`v ∈ binderNameSet`) ensures only inter-binder `TVar` bounds are
  normalized; all other `TVar` bounds still trigger the hard-stop. Non-recursive
  types never produce inter-binder `TVar` bounds at this point, so they are
  completely unaffected.

- **Low risk:** Steps 3-4 upgrade tests from asserting failure to asserting
  success. If the fix works, the tests pass. If the fix doesn't fully work,
  the tests fail clearly with the remaining error.

- **Medium risk:** The over-approximation (dropping the bound) may produce
  elaborated terms that are too general for the type checker. This would
  manifest as a type-checking failure in the upgraded tests, caught by the
  `typeCheck term \`shouldBe\` Right ty` assertion. Step 6 covers this case.

- **No risk to existing tests:** The 1175-test baseline has zero cases that
  produce `mbBound = Just (TVar v)` AND pass (they either don't produce it,
  or produce it and hit the hard-stop which is already tested as expected
  failure). The change only makes the hard-stop conditional.

## Non-Goals

- This plan does NOT modify `Finalize.hs`. The `aliasBounds` check at lines
  502-509 is dead code due to the GADT constraint. Cleaning it up is a
  separate hygiene task, not required for correctness.

- This plan does NOT modify `Normalize.hs` (`simplifySchemeBindings` /
  `substBound`). The roadmap mentions these functions, but they operate on
  `BoundType` values where `TVar` cannot appear. The actual fix target is
  `ReifyPlan.hs` where the type is `ElabType`.

- This plan does NOT modify constraint generation, acyclicity detection,
  or type reification. Those phases correctly produce the constraint graph
  and reify types; the issue is solely in the scheme finalization's handling
  of the reified result.
