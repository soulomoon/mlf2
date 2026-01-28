I will update `ElaborationSpec.hs` to use strict `shouldBe` assertions by first capturing the exact output of the elaboration pipeline.

1. **Capture Exact Outputs**:
   Create a temporary script `debug_elab_values.hs` to run `Elab.runPipelineElab` on the test expressions and print the exact `pretty` strings for:

   * Lambda: `\x. x`

   * Application: `(\x. x) 42`

   * Polymorphic Let: `let id = \x. x in id`

   * Polymorphic Instantiation: `let id = \x. x in id 1`

   * Manual Let Scheme construction.

2. **Verify Determinism**:
   Run the script to ensure variable names (e.g., `t0`, `t1`) are generated deterministically.

3. **Update ElaborationSpec.hs**:
   Replace all instances of `shouldSatisfy` with `shouldBe` using the captured strings.

   * Refine "elaborates lambda" (Type)

   * Refine "elaborates application" (Type)

   * Refine "elaborates polymorphic let-binding" (Term & Type)

   * Refine "elaborates polymorphic instantiation" (Term)

   * Refine "pretty prints let with scheme" (Term)

4. **Verify Fixes**:
   Run `cabal test mlf2-test` to confirm all strict tests pass.

