### Changes Made
- `test/BackendLLVMSpec.hs`: added the focused public behavior test
  `stringAppend concatenates Unicode scalar strings through native execution`.
  The test covers `stringAppend "aλ" "b"`, `stringAppend "" "λ"`, and
  `stringAppend "λ" ""` through source checking, `run-program`, backend
  LLVM/object validation, `emit-native`/native-object validation, and linked
  native execution.
- `test/PrimitiveInventorySpec.hs`: added shared primitive inventory coverage
  for the new native-lowerable primitive.
- `src/MLF/Primitive/Inventory.hs`: added
  `PrimitiveNativeStringAppend`, `stringAppendPrimitiveName`, the reserved
  `__string_append : String -> String -> String` spec, and native support
  classification.
- `src/MLF/Frontend/Program/Prelude.hs`: exposed public Prelude
  `stringAppend : String -> String -> String` backed by `__string_append`.
- `src/MLF/Frontend/Program/Run.hs`: added run-program/interpreter behavior
  for `__string_append`, preserving Haskell `String` scalar contents with
  `(++)`.
- `src/MLF/Backend/LLVM/Lower.hs`: added native lowering and runtime support
  for `__string_append`, copying UTF-8 bytes from both null-terminated string
  operands into a newly allocated null-terminated string.
- `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`,
  `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`: added narrow
  user-facing and readiness evidence for the new string append tracer.

### Tests
- RED:
  `cabal test mlf2-test --test-options='--match "stringAppend concatenates Unicode scalar strings through native execution"'`
  failed as expected before production changes with the diagnostic that module
  `Prelude` does not export `stringAppend` at the focused test's
  `checkProgramFile` step.
- GREEN:
  `cabal test mlf2-test --test-options='--match "stringAppend concatenates Unicode scalar strings through native execution"'`
  passed with `1 example, 0 failures`.
- Primitive inventory:
  `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  passed with `1 example, 0 failures`.
- Neighbor matcher set:
  `cabal test mlf2-test --test-options='--match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution" --match "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" --match "charIsAsciiIdentifierContinue classifies ASCII identifier-continuation Char values through native execution" --match "charIsAsciiIdentifierStart classifies ASCII identifier-start Char values through native execution" --match "charIsAsciiAlphaNum classifies ASCII alphanumeric Char values through native execution" --match "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution" --match "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" --match "charIsAsciiLower classifies ASCII lowercase Char values through native execution" --match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'`
  passed with `22 examples, 0 failures`.
- Evidence rg checks:
  `rg -n 'stringAppend : String -> String -> String|__string_append|stringAppendPrimitiveName|PrimitiveNativeStringAppend|RuntimeStringAppend' src test docs CONTEXT.md README.md CHANGELOG.md`
  passed.
- Evidence rg checks:
  `rg -n -e 'stringAppend concatenates Unicode scalar strings through native execution' -e 'stringAppend "aλ" "b"' -e 'stringAppend "" "λ"' -e 'stringAppend "λ" ""' -e 'NativeRunResult ExitSuccess "\\"a\\\\955b\\"\\n"' -e 'NativeRunResult ExitSuccess "\\"\\\\955\\"\\n"' test/BackendLLVMSpec.hs`
  passed. The raw `.mlfp` source snippets are escaped inside Haskell string
  literals; the command succeeded on the test name and native render
  expectations.
- Evidence rg checks:
  `rg -n 'Unicode scalar|String append|stringAppend|broad string|String/List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  passed.
- `git diff --check` passed before broad validation and again after generated
  depfile restore.
- `cabal build all` passed.
- `CARGO_TARGET_DIR=/tmp/round287-cargo-target cabal test` passed with
  `2591 examples, 0 failures`.
- `CARGO_TARGET_DIR=/tmp/round287-cargo-target ./scripts/thesis-conformance-gate.sh`
  passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
- TDD skill loaded: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Generated validation churn in `runtime/mlfp_io/target/release/libmlfp_io.d`
  rewrote tracked parent-root paths to the round worktree path; it was restored
  with `git restore runtime/mlfp_io/target/release/libmlfp_io.d`.
- Scope boundaries: no roadmap status edits, no `orchestrator/state.json`
  edits, no parser parity, no formatting, no `String`/`List Char` conversion,
  no platform/proof work.
- Blocker: none.
