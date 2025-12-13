I will implement the timeout using `System.Timeout` and `around_` directly within `ElaborationSpec.hs`, removing the separate `TestUtils` module to keep the project structure clean.

1. **Remove** **`test/TestUtils.hs`**: Delete the separate utility file.
2. **Update** **`mlf2.cabal`**: Remove `TestUtils` from the `other-modules` list of `mlf2-test`.
3. **Update** **`test/ElaborationSpec.hs`**:

   * Import `System.Timeout (timeout)`.

   * Define a local `withTimeout` helper function.

   * Apply the timeout using `around_` to the spec.
4. **Run Tests**: Verify that `ElaborationSpec` passes with the timeout configuration.

