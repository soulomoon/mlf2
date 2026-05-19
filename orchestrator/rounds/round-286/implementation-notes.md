# Round 286 Implementation Notes

## Scope

- Selected item: `item-286-char-is-ascii-printable-native-tracer`.
- Target behavior: public `charIsAsciiPrintable : Char -> Bool` backed by reserved primitive `__char_is_ascii_printable`.
- Scope boundaries: no Unicode printability categories, locale, regex, parser parity, formatting, `String`/`List Char` conversion, platform/proof, roadmap status, or controller state edits.

## TDD Evidence

- TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- RED command:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiPrintable classifies ASCII printable Char values through native execution"'
```

- RED result: failed as expected before production changes.
- RED failure:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `charIsAsciiPrintable` ..."
```

- GREEN command:

```bash
CARGO_TARGET_DIR=/tmp/round286-cargo-target cabal test mlf2-test --test-options='--match "charIsAsciiPrintable classifies ASCII printable Char values through native execution"'
```

- GREEN result: passed, 1 example, 0 failures.

## Validation

- Focused inventory:

```bash
CARGO_TARGET_DIR=/tmp/round286-cargo-target cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

  Passed, 1 example, 0 failures.

- Focused neighbor set:

```bash
CARGO_TARGET_DIR=/tmp/round286-cargo-target cabal test mlf2-test --test-options='--match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution" --match "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" --match "charIsAsciiIdentifierContinue classifies ASCII identifier-continuation Char values through native execution" --match "charIsAsciiIdentifierStart classifies ASCII identifier-start Char values through native execution" --match "charIsAsciiAlphaNum classifies ASCII alphanumeric Char values through native execution" --match "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution" --match "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" --match "charIsAsciiLower classifies ASCII lowercase Char values through native execution" --match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

  Passed, 21 examples, 0 failures.

- Evidence `rg` checks: passed for primitive inventory names, runtime tags,
  Prelude export, docs references, fixed-string source snippets, and escaped
  tab/newline cases.
- `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d`: clean before and
  after the broad gate.
- `git diff --check`: passed.
- `cabal build all`: passed.
- `CARGO_TARGET_DIR=/tmp/round286-cargo-target cabal test`: passed, 2590
  examples, 0 failures.
- `CARGO_TARGET_DIR=/tmp/round286-cargo-target ./scripts/thesis-conformance-gate.sh`:
  passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

## Files Changed

- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `src/MLF/Primitive/Inventory.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `CHANGELOG.md`
- `orchestrator/rounds/round-286/implementation-notes.md`

## Blockers

- None at RED.
