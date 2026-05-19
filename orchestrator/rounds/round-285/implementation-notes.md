# Round 285 Implementation Notes

## Scope

- Selected item: `item-285-char-is-ascii-punctuation-native-tracer`.
- Target behavior: public `charIsAsciiPunctuation : Char -> Bool` backed by reserved primitive `__char_is_ascii_punctuation`.
- Scope boundaries: no Unicode punctuation categories, locale, regex, parser parity, formatting, `String`/`List Char` conversion, platform/proof, roadmap status, or controller state edits.

## TDD Evidence

- TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Implementer started the RED slice, then stalled; controller interrupted the
  implementer and completed the scoped implementation/validation in-thread.
- RED command:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution"'
```

- RED result: failed as expected after adding only the focused backend/native
  tracer test.
- RED failure:

```text
1) Backend LLVM parity, charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution
       Unknown backend LLVM function: __char_is_ascii_punctuation
```

- GREEN command:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution"'
```

- GREEN result: passed, `1 example, 0 failures`.

## Validation

- Focused inventory:

```bash
CARGO_TARGET_DIR=/tmp/round285-cargo-target cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

  Passed, `1 example, 0 failures`.

- Focused neighbor set:

```bash
CARGO_TARGET_DIR=/tmp/round285-cargo-target cabal test mlf2-test --test-options='--match "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" --match "charIsAsciiIdentifierContinue matches parser continuation characters through native execution" --match "charIsAsciiIdentifierStart matches parser start characters through native execution" --match "charIsAsciiAlphaNum classifies ASCII letters or digits through native execution" --match "charIsAsciiAlpha classifies ASCII letters through native execution" --match "charIsAsciiUpper classifies ASCII uppercase letters through native execution" --match "charIsAsciiLower classifies ASCII lowercase letters through native execution" --match "charIsDigit classifies ASCII decimal digits through native execution" --match "stringCharAt returns a Unicode scalar by index through native execution" --match "stringSlice keeps a Unicode scalar range through native execution" --match "stringTake keeps a Unicode scalar prefix through native execution" --match "stringDrop drops a Unicode scalar prefix through native execution" --match "stringEndsWith finds a Unicode scalar suffix through native execution" --match "stringStartsWith finds a Unicode scalar prefix through native execution" --match "stringContains finds a Unicode scalar substring through native execution" --match "stringContainsChar finds a Unicode scalar through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "stringLength counts Unicode scalar values through native execution" --match "emits native LLVM for compiler frontend seed package evidence" --match "runs compiler frontend seed package evidence through native execution"'
```

  Passed, `20 examples, 0 failures`.

- Evidence `rg` checks:

```bash
rg -n 'charIsAsciiPunctuation : Char -> Bool|__char_is_ascii_punctuation|charIsAsciiPunctuationPrimitiveName|PrimitiveNativeCharIsAsciiPunctuation|RuntimeCharIsAsciiPunctuation' src test docs CONTEXT.md README.md CHANGELOG.md
rg -n -e 'charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution' -e "charIsAsciiPunctuation '!'" -e "charIsAsciiPunctuation '_'" -e "charIsAsciiPunctuation '~'" -e "charIsAsciiPunctuation 'a'" -e "charIsAsciiPunctuation '7'" -e "charIsAsciiPunctuation ' '" -e "charIsAsciiPunctuation 'λ'" test/BackendLLVMSpec.hs
rg -n 'self-host|self host|self-hosting|self hosting|bootstrapped|bootstrap|complete parser|full parser|parser parity|full slicing|Unicode punctuation|locale|regex|formatting|String/List Char|platform/proof|broader classification' CHANGELOG.md docs/backend-native-pipeline.md docs/mlfp-language-reference.md docs/mlfp-self-boot-readiness.md CONTEXT.md README.md
```

  All passed after the docs were aligned to the narrow ASCII punctuation claim.

- `git diff --check`: passed.
- `cabal build all`: passed.
- `CARGO_TARGET_DIR=/tmp/round285-cargo-target cabal test`: passed,
  `2589 examples, 0 failures`.
- `CARGO_TARGET_DIR=/tmp/round285-cargo-target ./scripts/thesis-conformance-gate.sh`:
  passed with `[thesis-gate] PASS: thesis conformance anchors are green`.
- Runtime generated depfile drift check:
  `git diff -- runtime/mlfp_io/target/release/libmlfp_io.d` remained clean
  after the thesis gate.

## Files Changed

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Primitive/Inventory.hs`
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `orchestrator/rounds/round-285/implementation-notes.md`

## Blockers

- None.
