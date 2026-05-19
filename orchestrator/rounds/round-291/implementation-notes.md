# Round 291 Implementation Notes

## Scope

- Selected item: `item-291-string-from-int-native-tracer`.
- Implemented public Prelude `stringFromInt : Int -> String` backed by reserved primitive `__string_from_int : Int -> String`.
- Scope stayed limited to decimal `Int` to `String` conversion through check/run/backend/object/native paths. No general `Show`, interpolation, printf-style source formatting, locale behavior, regex, parser parity, platform ABI contract, roadmap status, or proof-record claim was added.

## RED

- Command: `cabal test mlf2-test --test-options='--match "stringFromInt formats Int values as decimal strings through native execution"'`
- Result: failed as expected before production changes.
- Failure: `Prelude` did not export `stringFromInt`.

## GREEN

- Command: `cabal test mlf2-test --test-options='--match "stringFromInt formats Int values as decimal strings through native execution"'`
- Result: passed, 1 example, 0 failures.
- Evidence: the focused public `.mlfp` examples for `stringFromInt 42` and `stringFromInt 0` passed source checking, `run-program`, raw backend emission, backend object validation, `emit-native`, native object validation, and linked native execution.

## Files Changed

- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `src/MLF/Primitive/Inventory.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/mlfp-language-reference.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-self-boot-readiness.md`
- `CHANGELOG.md`
- `orchestrator/rounds/round-291/implementation-notes.md`

## Validation

- Primitive inventory matcher:
  `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  passed, 1 example, 0 failures.
- Neighbor matcher set:
  `cabal test mlf2-test --test-options='--match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "stringFromList converts List Char values to Unicode scalar strings through native execution" --match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringLength counts Unicode scalar values through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'`
  passed, 6 examples, 0 failures. The plan's `stringLength` label was stale, so the current equivalent
  `cabal test mlf2-test --test-options='--match "Unicode stringLength source checks, runs, emits backend, and executes natively"'`
  was also run and passed, 1 example, 0 failures.
- Evidence `rg` checks:
  `rg -n -e 'stringFromInt : Int -> String' -e '__string_from_int' -e 'stringFromIntPrimitiveName' -e 'PrimitiveNativeStringFromInt' -e 'RuntimeStringFromInt' src test docs CHANGELOG.md`
  passed with expected source, test, docs, and changelog matches.
  `rg -n -e 'stringFromInt formats Int values as decimal strings through native execution' -e 'stringFromInt 42' -e 'stringFromInt 0' test/BackendLLVMSpec.hs`
  passed with expected test-fixture matches.
  `rg -n 'Unicode scalar|stringFromInt|String/List Char|formatting|Explicit String Formatting|Show Int|printf|interpolation|locale|regex|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  passed with expected claim-audit matches and no widened claim.
- `git diff --check` passed.
- `cabal build all` passed.
- `CARGO_TARGET_DIR=/tmp/round291-cargo-target cabal test` passed, 2596 examples, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round291-cargo-target ./scripts/thesis-conformance-gate.sh` passed; final gate output ended with `[thesis-gate] PASS: thesis conformance anchors are green`.

## Generated Churn

- Restored validation churn in `runtime/mlfp_io/target/release/libmlfp_io.d`.
