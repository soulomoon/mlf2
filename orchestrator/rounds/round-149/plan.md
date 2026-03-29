# Round 149 — Implementation Plan: Open fallback reconstruction for recursive types

**Item**: roadmap item-4  
**Goal**: Make `Fallback.hs` recognize μ-types in non-local positions and return the actual recursive surface type instead of the quantified fail-closed shell.

---

## 1. Problem Summary

In `computeResultTypeFallbackCore` (Fallback.hs), non-local types always fall through to `rootFinal` at line 735:

```haskell
-- Lines 732-735 (current):
else
    if rootBindingIsLocalType
        then schemeBodyTarget targetPresolutionView rootC
        else rootFinal
```

`rootFinal` is a quantified variable. When the bound chain from `rootFinal` involves a `TyMu` node, generalization at that level loses the μ-type — producing a `∀a. ...` shell instead of preserving the recursive structure.

The fix: detect μ-involvement in the non-local bound chain and route through `schemeBodyTarget` (which unwraps the scheme root to its body/bound, exposing the μ-type) instead of using the raw quantified variable.

---

## 2. Strategy: Controlled μ-detection at the target selection

**Why minimal**: The non-local path currently has ONE decision point that loses μ (line 735). All other non-local paths (`rootNonLocalSchemeAliasBaseLike → baseC` at line 717) work correctly. The fix adds a single conditional branch at this one decision point.

**Why `presolutionViewFinal` is safe**: When `boundTarget = Nothing` (which it is for μ-types, since `resolveBaseBoundCanonical` does not traverse through TyMu), `viewFinalBounded = viewBase` (line 472-473), so `presolutionViewFinal = presolutionView`. The two views are identical for the μ case.

**Why `schemeBodyTarget` works**: For a scheme root TyVar whose bound is TyMu (or chains to TyMu through TyForall), `schemeBodyTarget` returns the bound canonical node (Scope.hs lines 154-156). This exposes the μ-type as the generalization target, so `generalizeWithPlan` at line 758-759 produces a scheme containing the recursive type.

---

## 3. Step-by-step changes

### Step 1: Add `rootFinalInvolvesMu` helper (Fallback.hs, after line 516)

**Location**: Insert after the `rootBoundIsBase` definition block (lines 509-516), before line 517 (`rootIsSchemeRoot`).

**Code to add**:

```haskell
                {- Note [Recursive type opening for non-local fallback]
                   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                   The result-type fallback is fail-closed for non-local types:
                   it returns the quantified rootFinal variable, producing a ∀a. …
                   shell that loses any μ-structure in the type's bound chain.

                   For well-formed recursive types (μ-types introduced by explicit
                   annotations), this quantified shell is too lossy — the surface
                   type should preserve the μ.  We detect this by walking the
                   rootFinal bound chain through TyVar bounds, TyForall/TyExp
                   bodies, looking for a TyMu node.  When found, we route through
                   schemeBodyTarget to unwrap the scheme root and expose the
                   actual recursive type for generalization.

                   This opening is controlled:
                   - Only the bound chain is walked (no arrow traversal)
                   - Only affects the non-local targetC fallback (line 735)
                   - Local-type behavior is completely unchanged
                   - Non-recursive non-local types still get rootFinal -}
                rootFinalInvolvesMu =
                    let go visited nid0 =
                            let nid = canonicalFinal nid0
                                key = getNodeId nid
                            in if IntSet.member key visited
                                then False
                                else case lookupNodeIn nodesFinal nid of
                                    Just TyMu{} -> True
                                    Just TyVar{ tnBound = Just bnd } ->
                                        go (IntSet.insert key visited) bnd
                                    Just TyForall{ tnBody = b } ->
                                        go (IntSet.insert key visited) b
                                    Just TyExp{ tnBody = b } ->
                                        go (IntSet.insert key visited) b
                                    _ -> False
                    in go IntSet.empty rootFinal
```

**Rationale**: Walk through TyVar bounds, TyForall bodies, and TyExp bodies (the "quantifier chain") but NOT through TyArrow or other structural nodes. This is a conservative check that recognizes μ-types wrapped in any number of quantifier/binding layers, without false-positiving on arrows that happen to have μ-typed components somewhere inside.

### Step 2: Modify the non-local targetC fallback (Fallback.hs, lines 732-735)

**Current code** (lines 732-735):

```haskell
                                else
                                    if rootBindingIsLocalType
                                        then schemeBodyTarget targetPresolutionView rootC
                                        else rootFinal
```

**Replace with**:

```haskell
                                else
                                    if rootBindingIsLocalType
                                        then schemeBodyTarget targetPresolutionView rootC
                                        else if rootFinalInvolvesMu
                                            then schemeBodyTarget presolutionViewFinal rootC
                                            else rootFinal
```

**What changes**:
- Non-local types where `rootFinal` bound chain contains TyMu → use `schemeBodyTarget presolutionViewFinal rootC` (unwraps to the actual μ-type)
- Non-local types without TyMu → unchanged (`rootFinal`)
- Local types → completely unchanged (takes the `then` branch as before)

**Why `presolutionViewFinal` instead of `targetPresolutionView`**: `targetPresolutionView` is set to `presolutionView` for non-local types (line 504). As discussed in §2, `presolutionViewFinal == presolutionView` when `boundTarget = Nothing`. Using `presolutionViewFinal` is consistent with the generalization call at line 758-759 which also uses `presolutionViewFinal`.

### Step 3: Add `rootFinalInvolvesMu` to the debug trace (Fallback.hs, around line 776)

**Current code** (lines 775-777):

```haskell
                    ++ " rootBindingIsLocalType="
                    ++ show rootBindingIsLocalType
                )
```

**Replace with**:

```haskell
                    ++ " rootBindingIsLocalType="
                    ++ show rootBindingIsLocalType
                    ++ " rootFinalInvolvesMu="
                    ++ show rootFinalInvolvesMu
                )
```

### Step 4: Update test 1 — nested forall boundary (PipelineSpec.hs, line 2115)

**Current** (line 2115):
```haskell
        containsMu fallbackTy `shouldBe` False
```

**Replace with**:
```haskell
        containsMu fallbackTy `shouldBe` True
```

**Test name**: "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary" (line 2101)

**Note**: The test name says "fail-closed" but the behavior is now open for μ-types. Consider renaming the test description to reflect the new behavior, e.g.:
```haskell
      it "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary" $ do
```

### Step 5: Update test 2 — non-local proxy fallback (PipelineSpec.hs, line 2242)

**Current** (line 2242):
```haskell
        containsMu fallbackTy `shouldBe` False
```

**Replace with**:
```haskell
        containsMu fallbackTy `shouldBe` True
```

**Test name**: "keeps non-local proxy fallback fail-closed in result-type reconstruction" (line 2232)

**Note**: Consider renaming:
```haskell
      it "keeps non-local proxy fallback open for recursive types in result-type reconstruction" $ do
```

### Step 6: Update test 3 — pipeline entrypoints (PipelineSpec.hs, lines 2303-2315)

**Current** (lines 2303-2315):
```haskell
      it "keeps the same non-local proxy wrapper fail-closed at pipeline entrypoints" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EApp (EVar "g") (EVar "g"))
            pipelineRuns =
              [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr)),
                ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
              ]
        forM_ pipelineRuns $ \(label, result) ->
          expectStrictPipelineFailure (label ++ " non-local proxy wrapper") result
```

**Replace with**:
```haskell
      it "keeps the same non-local proxy wrapper open for recursive types at pipeline entrypoints" $ do
        let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
            expr =
              ELet
                "g"
                (ELamAnn "x" recursiveAnn (EVar "x"))
                (EApp (EVar "g") (EVar "g"))
            pipelineRuns =
              [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr expr)),
                ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
              ]
        forM_ pipelineRuns $ \(label, result) ->
          case result of
            Left err ->
              expectationFailure
                (label ++ " non-local proxy wrapper: expected success, got " ++ show err)
            Right (_term, ty) ->
              containsMu ty `shouldBe` True
```

### Step 7: Update source-reading test pattern (PipelineSpec.hs, lines 2275-2277)

**Current** (lines 2275-2277):
```haskell
        fallbackSrc
          `shouldSatisfy` isInfixOf
            "then schemeBodyTarget targetPresolutionView rootC\n                                        else rootFinal"
```

**Replace with**:
```haskell
        fallbackSrc
          `shouldSatisfy` isInfixOf
            "then schemeBodyTarget targetPresolutionView rootC\n                                        else if rootFinalInvolvesMu\n                                            then schemeBodyTarget presolutionViewFinal rootC\n                                            else rootFinal"
```

**Why this is the only source-reading test that changes**: All other patterns in the two source-reading test blocks (lines 2083-2099 and 2244-2301) check definitions of `rootLocal*`, `keepTargetFinal`, `sameLaneLocalRetainedChildTarget`, `sameLocalTypeLane`, and the `keepTargetFinal=True` branch — none of which change. The ONLY substring that changes is the non-local else-branch at lines 734-735.

Verification that other source patterns still hold:
- `"then rootFinal"` (line 2301) — still present at line 724 and in new code
- `"keepTargetFinal =\n                    rootBindingIsLocalType\n..."` (lines 2096, 2280) — unchanged
- `"sameLaneLocalRetainedChildTarget =\n..."` (line 2093) — unchanged
- `"in if rootBindingIsLocalType\n                        then pickCandidate..."` (line 2090) — unchanged
- All `rootLocal*` definitions (lines 2248-2289) — unchanged

### Step 8: Update P5ClearBoundarySpec (potential cascade)

**Check**: The `nestedForallContrastExpr` test in `test/Research/P5ClearBoundarySpec.hs` (line 73-75) currently asserts:
```haskell
        it "fails closed once the same wrapper crosses a nested forall boundary" $ do
            fallbackTy <- fallbackType nestedForallContrastExpr
            containsMu fallbackTy `shouldBe` False
```

This uses the **exact same expression** as PipelineSpec test 1:
```haskell
nestedForallContrastExpr =
    ELet "id" (ELam "z" (EVar "z"))
        (ELet "k" (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
            (EApp (ELam "y" (EVar "y")) (EVar "k")))
```

**However**: This test uses `fallbackType` which calls `computeResultTypeFallback` on the FULL expression. PipelineSpec test 1 also calls `computeResultTypeFallback` on the full expression. Both should now return a μ-containing type.

**Action**: Change line 75 from `False` to `True` and update the test name.

Also check the pipeline entrypoint test at lines 91-117 which tests the same expression through `runPipelineElab`/`runPipelineElabChecked` and currently expects `PhiTranslatabilityError`. If the fallback now returns μ, the pipeline might succeed instead. **Implementer must run the test first** to check whether the pipeline succeeds or still fails for a different reason. If it succeeds, update the expectation to assert `containsMu ty == True`. If it still fails, leave it.

---

## 4. Files touched

| File | Change |
|------|--------|
| `src/MLF/Elab/Run/ResultType/Fallback.hs` | Add `rootFinalInvolvesMu` helper + modify non-local targetC branch + debug trace |
| `test/PipelineSpec.hs` | Update 3 test expectations (lines 2115, 2242, 2303-2315) + 1 source-reading pattern (lines 2275-2277) |
| `test/Research/P5ClearBoundarySpec.hs` | Update 1 test expectation (line 75) + possibly pipeline test (lines 91-117) |

---

## 5. Invariants preserved

- **Local-type fallback unchanged**: `rootBindingIsLocalType == True` takes the same `then` branch as before at every decision point. No local-type definitions or paths are modified.
- **Non-recursive non-local unchanged**: When `rootFinalInvolvesMu == False`, the non-local path still returns `rootFinal` exactly as before.
- **`keepTargetFinal` unchanged**: The definition and all uses of `keepTargetFinal` are untouched. The μ-opening only applies in the `keepTargetFinal == False` path.
- **`sameLaneLocalRetainedChildTarget` unchanged**: Not modified.
- **Candidate picker unchanged**: Not modified.
- **`targetPresolutionView` unchanged**: Not modified (the μ path explicitly uses `presolutionViewFinal`).

---

## 6. Potential risks and mitigations

1. **`schemeBodyTarget presolutionViewFinal rootC` returns `rootC` unchanged**: This happens if `rootC` is not a scheme root and its bound is not a scheme body (see Scope.hs lines 149-160). In that case, generalization at `rootC` might still lose the μ. **Mitigation**: If this happens during testing, the implementer should add a deeper unwrap that walks directly to the TyMu node. But based on the constraint graph structure for the test expressions, `rootC` should be a scheme root.

2. **P5 pipeline test cascade**: The pipeline entrypoint test for `nestedForallContrastExpr` currently expects `PhiTranslatabilityError`. With the fallback returning μ, the pipeline might succeed. **Mitigation**: Run the test, update the expectation to match actual behavior.

3. **Other tests regress**: The change is narrow (one new conditional branch), but μ-detection walks the bound chain which could theoretically match unexpected cases. **Mitigation**: The walk is conservative (no arrow traversal). Run `cabal test` with `--test-show-details=direct` to catch any regressions.

---

## 7. Verification

```bash
# Build
cabal build all

# Run full test suite
cabal test --test-show-details=direct

# Focused verification on the three target tests
cabal test --test-show-details=direct --test-option='-m' --test-option='non-local proxy'
cabal test --test-show-details=direct --test-option='-m' --test-option='nested forall boundary'
cabal test --test-show-details=direct --test-option='-m' --test-option='pipeline entrypoints'

# Check for warnings
cabal build all 2>&1 | grep -i warning
```

**Completion criteria**:
- `cabal build all` — zero warnings
- `cabal test` — all tests pass
- Tests 1-3 assert `containsMu == True` (or pipeline success with μ-type)
- Source-reading tests pass with updated patterns
- No regressions in local-type fallback tests
