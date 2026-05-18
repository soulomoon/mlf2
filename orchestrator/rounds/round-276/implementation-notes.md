# Round 276 Implementation Notes

## Changes Made

- Added public Prelude `stringCharAt : String -> Int -> Char` backed by the
  shared primitive inventory name `__string_char_at`.
- Added `RuntimeStringCharAt` to `run-program`, indexing Haskell `String`
  values by Unicode scalar position and returning a `Char` for in-range input.
- Added LLVM/backend lowering for `__string_char_at`, with native execution
  advancing by UTF-8 scalar starts and decoding the scalar at the selected
  cursor position.
- Aligned native rendering for printable ASCII `Char` results with
  `run-program` output so the tracer can assert `'b'` instead of numeric
  escape output.
- Documented the narrow in-range cursor/index tracer in the language
  reference, backend-native pipeline contract, self-boot readiness ledger, and
  changelog.

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
- `orchestrator/rounds/round-276/implementation-notes.md`

## TDD Evidence

Loaded TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.

RED command:

```bash
cabal test mlf2-test --test-options='--match "stringCharAt indexes Unicode scalar cursor positions through native execution"'
```

RED result: failed as expected at the public import boundary:

```text
expected: Right "OK\n"
 but got: Left "... module `Prelude` does not export `stringCharAt` ..."
```

GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringCharAt indexes Unicode scalar cursor positions through native execution"'
```

GREEN result: passed, 1 example, 0 failures. The test covers both:

- `stringCharAt "aλb" 1` -> `Right "'\\955'\n"` and native
  `NativeRunResult ExitSuccess "'\\955'\n" ""`
- `stringCharAt "λab" 2` -> `Right "'b'\n"` and native
  `NativeRunResult ExitSuccess "'b'\n" ""`

An intermediate GREEN run reached native execution but rendered ASCII `b` as
`'\\98'`; the final GREEN includes the narrow native `Char` renderer alignment
needed for the public tracer output.

## Verification

- `cabal test mlf2-test --test-options='--match "stringCharAt indexes Unicode scalar cursor positions through native execution"'`
  - PASS: 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  - PASS: 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'`
  - PASS: 11 examples, 0 failures.
- `rg -n 'stringCharAt : String -> Int -> Char|__string_char_at|stringCharAtPrimitiveName|PrimitiveNativeStringCharAt|RuntimeStringCharAt' src test docs README.md CHANGELOG.md`
  - PASS: found expected implementation, docs, changelog, and test references.
- `rg -n 'stringCharAt indexes Unicode scalar cursor positions through native execution|stringCharAt "aλb" 1|stringCharAt "λab" 2|NativeRunResult ExitSuccess' test/BackendLLVMSpec.hs`
  - PASS: focused public behavior evidence found.
- `rg -n 'Unicode scalar|cursor|index|stringCharAt|stringSlice|stringTake|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|formatting|classification|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md`
  - PASS: docs record the new tracer and still state excluded scope.
- `git diff --check`
  - PASS.
- `cabal build all`
  - PASS.
- `cabal test`
  - PASS: 2580 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`
  - PASS: thesis conformance anchors are green.

## Scope Boundaries

- Scope is limited to `item-276-string-char-at-native-tracer`.
- The implementation does not add `String`/`List Char` conversion, formatting,
  complete cursor API semantics, parser parity, platform contracts, compiler
  package work, or proof records.
- `orchestrator/state.json` is controller-owned and was not edited.
- Cabal regenerated `runtime/mlfp_io/target/release/libmlfp_io.d` with the
  worktree path during validation; that generated depfile drift was restored
  to the pre-existing repository path and is clean in the final diff.
- No blockers remain.
