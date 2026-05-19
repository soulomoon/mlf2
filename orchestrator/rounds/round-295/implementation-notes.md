# Round 295 Implementation Notes

## Scope

- Selected item: `item-295-string-replace-char-native-tracer`.
- Implemented public Prelude `stringReplaceChar : String -> Char -> Char -> String` backed by reserved primitive `__string_replace_char`.
- Kept scope limited to Unicode scalar `Char` replacement and no-match preservation. Did not add substring replacement, splitting, regex, formatting-family completion, case conversion, Unicode normalization, locale behavior, parser parity, platform ABI claims, roadmap status edits, or semantic roadmap updates.

## RED

- Command:
  `cabal test mlf2-test --test-options='--match "stringReplaceChar replaces Unicode scalar characters through native execution"'`
- Result: failed as expected before production changes.
- Failure evidence:
  `module 'Prelude' does not export 'stringReplaceChar'`.

## GREEN

- Command:
  `cabal test mlf2-test --test-options='--match "stringReplaceChar replaces Unicode scalar characters through native execution"'`
- Result: passed, 1 example, 0 failures.
- Evidence covered by the focused test:
  `.mlfp` source checking, `run-program`, `emit-backend`, backend object validation, `emit-native`, native object validation, and linked native execution for:
  - `stringReplaceChar "aλbλ" 'λ' 'x'` rendering `"axbx"`.
  - `stringReplaceChar "ab" 'λ' 'x'` rendering `"ab"`.

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
- `orchestrator/rounds/round-295/implementation-notes.md`

## Validation

- Focused RED: failed as expected; see RED.
- Focused GREEN: passed; see GREEN.
- Primitive inventory matcher:
  `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  passed, 1 example, 0 failures.
- Neighbor matcher set:
  `cabal test mlf2-test --test-options='--match "stringContainsChar searches Unicode scalars through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringFromUnit formats Unit as a string through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'`
  passed, 8 examples, 0 failures.
- Evidence `rg` checks:
  `rg -n -e 'stringReplaceChar : String -> Char -> Char -> String' -e '__string_replace_char' -e 'stringReplaceCharPrimitiveName' -e 'PrimitiveNativeStringReplaceChar' -e 'RuntimeStringReplaceChar' src test docs CHANGELOG.md`
  exited 0 with matches in the owned implementation, tests, docs, and changelog.
- Focused fixture evidence `rg` check:
  `rg -n -e 'stringReplaceChar replaces Unicode scalar characters through native execution' -e 'stringReplaceChar \\"aλbλ\\"' -e 'stringReplaceChar \\"ab\\"' test/BackendLLVMSpec.hs`
  exited 0 with matches for the public test and both source fixtures.
- Claim-audit `rg` check:
  `rg -n 'Unicode scalar|stringReplaceChar|stringContainsChar|stringContains|stringFromUnit|String/List Char|replacement|split|search|regex|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  exited 0 for audit review; new wording stays limited to Unicode scalar character replacement and does not claim substring replacement, splitting, regex, formatting-family completion, parser parity, platform contracts, self-boot proof, or milestone completion.
- `git diff --check`: passed.
- `cabal build all`: passed.
- `CARGO_TARGET_DIR=/tmp/round295-cargo-target cabal test`: passed, 2600 examples, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round295-cargo-target ./scripts/thesis-conformance-gate.sh`: passed.
- Generated validation churn: restored `runtime/mlfp_io/target/release/libmlfp_io.d` after broad validation dirtied it.

## Blockers

- None.
