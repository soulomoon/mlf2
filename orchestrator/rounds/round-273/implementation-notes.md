# Round 273 Implementation Notes

## Changes Made

- `test/BackendLLVMSpec.hs`: added the focused public behavior test `stringDrop slices Unicode scalar prefixes through native execution`, covering `stringDrop "λab" 1 == "ab"` and `stringDrop "aλb" 2 == "b"` through check-program, run-program, backend emission, backend object validation, native emission, native object validation, and linked native execution.
- `test/PrimitiveInventorySpec.hs`: extended the native-lowerable inventory expectation for the new string-drop primitive.
- `src/MLF/Primitive/Inventory.hs`: added `PrimitiveNativeStringDrop`, `stringDropPrimitiveName`, the `__string_drop : String -> Int -> String` primitive spec, and native-lowerable classification.
- `src/MLF/Frontend/Program/Prelude.hs`: exported public Prelude `stringDrop` and bound it to `__string_drop`.
- `src/MLF/Frontend/Program/Run.hs`: added interpreter/runtime support for dropping Unicode scalar prefixes by `Int` count.
- `src/MLF/Backend/LLVM/Lower.hs`: added LLVM/native lowering for `__string_drop`, including a native helper that advances by UTF-8 scalar boundaries rather than bytes.
- `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`: documented the narrow `stringDrop` tracer and kept adjacent out-of-scope items explicit.

## Tests

- `test/BackendLLVMSpec.hs`: added the focused public behavior test `stringDrop slices Unicode scalar prefixes through native execution`.

## TDD Evidence

Loaded TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.

RED command:

```bash
cabal test mlf2-test --test-options='--match "stringDrop slices Unicode scalar prefixes through native execution"'
```

RED result: FAIL as expected, 1 example. The test compiled and failed at the public source-checking boundary because `Prelude` does not export `stringDrop`:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `stringDrop` ..."
```

GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringDrop slices Unicode scalar prefixes through native execution"'
```

GREEN result: PASS, 1 example, 0 failures.

## Closeout Validation

- `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`: PASS, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'`: PASS, 8 examples, 0 failures.
- `rg -n 'stringDrop : String -> Int -> String|__string_drop|stringDropPrimitiveName|PrimitiveNativeStringDrop|RuntimeStringDrop' src test docs README.md CHANGELOG.md`: PASS.
- `rg -n 'stringDrop slices Unicode scalar prefixes through native execution|stringDrop "λab" 1|stringDrop "aλb" 2|NativeRunResult ExitSuccess "\\\"ab\\\"\\n"|NativeRunResult ExitSuccess "\\\"b\\\"\\n"' test/BackendLLVMSpec.hs`: PASS for test name and native result assertions.
- `rg -n 'stringDrop \\"λab\\" 1|stringDrop \\"aλb\\" 2' test/BackendLLVMSpec.hs`: PASS for source literals.
- `rg -n 'Unicode scalar|drop slicing|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|full slicing|formatting|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md`: PASS.
- `git diff --check`: PASS.
- `cabal build all`: PASS.
- `cabal test`: PASS, 2577 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: PASS, thesis conformance anchors green.

## Notes

- Scope is limited to `item-273-string-drop-native-tracer`.
- Did not widen to `stringTake`, arbitrary range slicing, String/List Char conversion, formatting, cursor APIs, parser parity, platform contracts, compiler package work, or proof records.
- `orchestrator/state.json` is controller-owned and was not edited.
- Broad validation rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` to worktree-local paths; the generated depfile line was restored to the canonical `/Volumes/src/mlf4/...` path.
- Blockers: none.
