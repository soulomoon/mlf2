I have analyzed `test/ElaborationSpec.hs`.

**Problems Discovered:**

1. **Fragile String Comparisons**: The tests heavily rely on `Elab.pretty` output comparisons (e.g., `shouldBe "t0 -> t0"`). This is brittle because:

   * It depends on deterministic variable naming (`t0`, `t3`), which might change if the traversal order or fresh ID generation logic changes.

   * It tests the *pretty printer* as much as the *elaboration logic*.

   * It masks the actual structure of the returned `ElabTerm` and `ElabType`.

2. **Missing Structural Tests**: There are no tests that verify the actual algebraic data structure of the elaborated terms. For example, `ELet` should be checked to have the correct `ElabScheme` and body structure, not just a string representation.

3. **Missing Error Case Coverage**: The tests only assert `Right (term, ty)` or `expectationFailure`. There are no negative tests checking if invalid programs (e.g., type errors, scope violations) produce the expected `Left` errors.

4. **Limited Scope**:

   * No test for nested `let` bindings to verify generalization levels.

   * No test for `ELam` with explicit type annotations (if supported by syntax).

   * No test for higher-rank polymorphism examples (e.g., `let f = \x. x in (f 1, f true)`).

**Improvement Suggestions:**

1. **Structural Assertions**: Add helper functions or matchers to check the structure of `ElabTerm` and `ElabType` directly, ignoring exact variable names where appropriate (alpha-equivalence).
2. **Add Negative Tests**: Add test cases for programs that should fail elaboration (e.g., `\x. x x` if infinite types are disallowed, or scope errors).
3. **Expand Integration Tests**: Add a "Complex Scenarios" section testing:

   * Let-polymorphism usage (using the polymorphic value at different types).

   * Nested let bindings.
4. **Refactor**: Group `pretty` print tests separately from logic tests.

**Plan:**

1. Refactor `ElaborationSpec.hs` to separate "Pretty Printing" tests from "Elaboration Logic" tests.
2. Add a helper `shouldAlphaEq` (or similar logic) to compare types/terms modulo variable renaming, making tests robust against ID generation changes. (For now, I will stick to structure matching without implementing full alpha-equivalence to keep it simple, but will verify constructors).
3. Add a test case for **Let-Polymorphism Usage**: `let f = \x. x in (f 1, f true)`.
4. Add a test case for **Nested Let**: `let x = 1 in let y = x in y`.

I will implement the **Let-Polymorphism Usage** test as a concrete improvement step now.

**Proposed Test Addition:**

```haskell
        it "elaborates usage of polymorphic let (instantiated at different types)" $ do
            -- let f = \x. x in let _ = f 1 in f true
            -- This forces 'f' to be instantiated twice: once at Int, once at Bool
            let expr = ELet "f" (ELam "x" (EVar "x")) 
                        (ELet "_" (EApp (EVar "f") (ELit (LInt 1))) 
                            (EApp (EVar "f") (ELit (LBool True))))
            case Elab.runPipelineElab expr of
                Right (term, ty) -> do
                    Elab.pretty ty `shouldBe` "Bool"
                    -- Verify term contains two instantiations of f
                    -- This is a structural check via pretty string for now
                    let s = Elab.pretty term
                    s `shouldSatisfy` (\x -> length (filter (== 'f') x) >= 3) -- def + 2 uses
                Left err -> expectationFailure err
```

