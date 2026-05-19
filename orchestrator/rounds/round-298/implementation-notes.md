### Changes Made
- Added public Prelude `stringReplace : String -> String -> String -> String`
  backed by the shared primitive inventory name `__string_replace`.
- Added interpreter/runtime support for left-to-right non-overlapping
  substring replacement over Haskell `String` values. Empty needle returns the
  original string unchanged.
- Added LLVM native lowering for `__string_replace`. The native helper scans
  candidate positions at valid UTF-8 scalar starts, preserves no-match inputs,
  returns the original pointer for an empty needle, and copies replacement bytes
  into a freshly allocated result for non-empty needles.
- Added focused backend/native coverage for replacement, no-match, and
  empty-needle fixtures in `test/BackendLLVMSpec.hs`.
- Updated primitive inventory tests, language/backend/readiness docs, and
  `CHANGELOG.md` to describe exactly this substring replacement tracer without
  claiming split-family APIs, regex, normalization, locale, case conversion,
  formatting, parser parity, platform ABI work, or self-boot proof completion.

### Tests
- `test/BackendLLVMSpec.hs`: added `stringReplace replaces Unicode scalar substrings through native execution` before production changes. The example checks public `.mlfp` source checking, run-program output, backend LLVM/object validation, native emission/object validation, and linked native execution for replacement, no-match, and empty-needle fixtures.
- `test/PrimitiveInventorySpec.hs`: extended the native-lowerable primitive
  classification expectation to include `PrimitiveNativeStringReplace`.

### Notes
- Required TDD skill loaded from `/Users/ares/.agents/skills/tdd/SKILL.md`; Haskell style guidance loaded from `/Users/ares/.agents/skills/haskell-pro/SKILL.md`.
- RED command: `cabal test mlf2-test --test-options='--match "stringReplace replaces Unicode scalar substrings through native execution"'`.
- RED result: failed as expected before production changes. The first fixture failed at `checkProgramFile` with `module \`Prelude\` does not export \`stringReplace\``, proving the test reaches the intended public Prelude surface rather than failing due to malformed source.
- Focused GREEN command: `cabal test mlf2-test --test-options='--match "stringReplace replaces Unicode scalar substrings through native execution"'`.
- Focused GREEN result: PASS, 1 example, 0 failures. The command was rerun after the final fixture-marker edit and still passed.
- Inventory command: `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`.
- Inventory result: PASS, 1 example, 0 failures.
- Neighbor command: `cabal test mlf2-test --test-options='--match "stringIndexOf indexes Unicode scalar substrings through native execution" --match "stringIndexOfChar indexes Unicode scalar characters through native execution" --match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'`.
- Neighbor result: PASS, 10 examples, 0 failures.
- Evidence scan: `rg -n -e 'stringReplace : String -> String -> String -> String' -e '__string_replace' -e 'stringReplacePrimitiveName' -e 'PrimitiveNativeStringReplace' -e 'RuntimeStringReplace' src test docs CHANGELOG.md`.
- Evidence scan result: PASS; found the public type, native primitive name, inventory owner, runtime constructor, backend references, tests, docs, and changelog entries.
- Fixture scan: `rg -n -e 'stringReplace replaces Unicode scalar substrings through native execution' -e 'stringReplace "aλbλb" "λb" "WXYZ"' -e 'stringReplace "abc" "λ" "x"' -e 'stringReplace "abc" "" "x"' test/BackendLLVMSpec.hs`.
- Fixture scan result: PASS; found the focused test name and all three requested fixture expressions.
- Claim audit: `rg -n 'Unicode scalar|stringReplace|stringReplaceChar|stringIndexOf|Plain String Search|substring replacement|replacement|split|regex|formatting|classification|cursor|Unicode normalization|locale|case conversion|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`.
- Claim audit result: PASS after review; the new `stringReplace` claims stay limited to this exact substring replacement tracer and explicitly keep adjacent roadmap items out of scope.
- Closeout `git diff --check`: PASS.
- Closeout `CARGO_TARGET_DIR=/tmp/round298-cargo-target cabal build all`: PASS.
- Closeout `CARGO_TARGET_DIR=/tmp/round298-cargo-target cabal test`: PASS, 2603 examples, 0 failures.
- Closeout `CARGO_TARGET_DIR=/tmp/round298-cargo-target ./scripts/thesis-conformance-gate.sh`: PASS.
- Generated Rust depfile absolute-path churn from native test/build commands was restored to HEAD before final status.
- `orchestrator/state.json` was not edited by this implementer; the remaining diff is the controller-provided active round marker observed at start.
