# Round 274 Implementation Notes

## Changes Made

- Added public Prelude `stringTake : String -> Int -> String` as the narrow
  `__string_take` primitive wrapper.
- Added shared primitive inventory ownership for `__string_take`, including
  `PrimitiveNativeStringTake`, native lowerable classification, and inventory
  coverage.
- Added interpreter support for `stringTake` over Unicode scalar `String`
  values using the same escaped display path as existing string values.
- Added LLVM/backend/native lowering for `__string_take`, including native
  runtime declaration, helper emission, and UTF-8 scalar-boundary copying for
  non-negative prefix-take counts.
- Updated the language reference, backend/native pipeline notes, self-boot
  readiness ledger, and changelog to document the selected take-slicing tracer.

## Tests

- `test/BackendLLVMSpec.hs`: added the focused public behavior test `stringTake slices Unicode scalar prefixes through native execution`.
- `test/PrimitiveInventorySpec.hs`: extended native lowerable primitive
  inventory assertions for `stringTake`.

## TDD Evidence

Loaded TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.

RED command:

```bash
cabal test mlf2-test --test-options='--match "stringTake slices Unicode scalar prefixes through native execution"'
```

RED result: FAIL as expected, 1 example. The test compiled and failed at the public source-checking boundary because `Prelude` does not export `stringTake`:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `stringTake` ..."
```

GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringTake slices Unicode scalar prefixes through native execution"'
```

GREEN result: PASS, 1 example, 0 failures. The focused test covers
check-program, run-program, backend LLVM emission, backend object validation,
emit-native, native object validation, and linked native execution for both:

- `stringTake "λab" 1`, displayed as `"\\955"`
- `stringTake "aλb" 2`, displayed as `"a\\955"`

## Validation Evidence

- `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`: PASS, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'`: PASS, 9 examples, 0 failures.
- `rg -n 'stringTake : String -> Int -> String|__string_take|stringTakePrimitiveName|PrimitiveNativeStringTake|RuntimeStringTake' src test docs README.md CHANGELOG.md`: found the implementation, test, docs, and changelog evidence.
- `rg -n 'stringTake slices Unicode scalar prefixes through native execution|stringTake "λab" 1|stringTake "aλb" 2|NativeRunResult ExitSuccess "\\\"\\\\955\\\"\\n"|NativeRunResult ExitSuccess "\\\"a\\\\955\\\"\\n"' test/BackendLLVMSpec.hs`: found the focused test and native result expectations.
- `rg -n 'stringTake \\"λab\\" 1|stringTake \\"aλb\\" 2' test/BackendLLVMSpec.hs`: found the exact `.mlfp` source tracer lines.
- `rg -n 'Unicode scalar|prefix slicing|stringTake|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|full slicing|formatting|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md`: found the expected docs/changelog surface and out-of-scope boundaries.
- `git diff --check`: PASS.
- `cabal build all`: PASS.
- `cabal test`: PASS, 2578 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: PASS.

## Notes

- Scope is limited to `item-274-string-take-native-tracer`.
- `orchestrator/state.json` is controller-owned and was not edited.
- No roadmap status, parser parity, arbitrary range slicing, String/List Char
  conversion, formatting, cursor APIs, platform contracts, compiler package
  work, or proof records were changed.
- Broad validation rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` to the
  worktree-local path; it was restored to the repository-root generated path and
  left out of the final diff.
