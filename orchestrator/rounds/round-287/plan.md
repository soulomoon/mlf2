### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-287-string-append-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable broad `String` tracer: public Prelude `stringAppend : String -> String -> String` should concatenate valid Unicode-scalar strings through source checking, `run-program`, backend LLVM emission, object-code validation, `emit-native`, native object validation, and linked native execution.

This advances the broad text substrate after the completed literal, length, empty/search/prefix/suffix, slicing/cursor, and ASCII classification tracers from rounds 265-286. It stays narrower than `String`/`List Char` conversion, formatting, broader Unicode category classification, complete cursor API design, parser combinators, parser parity, platform contracts, compiler package work, driver work, or proof records.

Round classification: behavior-changing implementation. The implementer must use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate because this changes public language/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Implementation ownership for this round is the single implementer in the canonical round-287 worktree. Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive inventory coverage: `test/PrimitiveInventorySpec.hs`.
- Primitive registry and native support classification: `src/MLF/Primitive/Inventory.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native LLVM lowering and runtime declarations/helpers: `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation: `orchestrator/rounds/round-287/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `stringAppend` concatenate Unicode-scalar strings by returning `"a\955b"` for `stringAppend "aλ" "b"` and preserving empty-side identity cases for `stringAppend "" "λ"` and `stringAppend "λ" ""`, with matching check, run-program, backend/object, emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `stringAppend concatenates Unicode scalar strings through native execution`. Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`, `emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`, `validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. Run the focused matcher before production changes and record the RED failure in `implementation-notes.md`; the expected failure should be missing public Prelude/native-capable `stringAppend` or missing backend lowering, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`, and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED -> GREEN -> refactor cycle. Do not batch broad text tests before implementation.
3. Write only the RED public behavior test first, using these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringAppend);
  def main : String = stringAppend "aλ" "b";
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringAppend);
  def main : String = stringAppend "" "λ";
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringAppend);
  def main : String = stringAppend "λ" "";
}
```

4. Prove RED with the focused command in Verification before production changes.
5. Implement only enough to pass:
   - add reserved primitive `__string_append : String -> String -> String` and native support classification in `MLF.Primitive.Inventory`;
   - export `stringAppend : String -> String -> String` from the built-in Prelude;
   - add `run-program` behavior by concatenating Haskell `String` values, preserving Unicode scalar values;
   - add backend/native lowering over the existing UTF-8 string representation, returning a newly allocated null-terminated `String` by copying left then right bytes;
   - preserve existing `Char`, `String`, slicing/search, `stringCharAt`, ASCII classification, IO, and native rendering behavior.
6. Update only narrow docs/changelog surfaces listed in the expected write scope. Docs must say this is an explicit native-capable string append tracer and must not claim `String`/`List Char` conversion, formatting, full slicing coverage, complete cursor APIs, parser parity, platform contracts, milestone-3 completion, or self-boot proof.
7. Re-run focused GREEN, inventory, neighbor, evidence, full build/test, and thesis-gate commands.
8. Record closeout evidence in `implementation-notes.md`: TDD skill path, RED/GREEN command results, files changed, native evidence by layer, docs updated, and scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringAppend concatenates Unicode scalar strings through native execution"'
```

Required focused inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution" --match "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" --match "charIsAsciiIdentifierContinue classifies ASCII identifier-continuation Char values through native execution" --match "charIsAsciiIdentifierStart classifies ASCII identifier-start Char values through native execution" --match "charIsAsciiAlphaNum classifies ASCII alphanumeric Char values through native execution" --match "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution" --match "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" --match "charIsAsciiLower classifies ASCII lowercase Char values through native execution" --match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringAppend : String -> String -> String|__string_append|stringAppendPrimitiveName|PrimitiveNativeStringAppend|RuntimeStringAppend' src test docs CONTEXT.md README.md CHANGELOG.md
```

```bash
rg -n -e 'stringAppend concatenates Unicode scalar strings through native execution' -e 'stringAppend "aλ" "b"' -e 'stringAppend "" "λ"' -e 'stringAppend "λ" ""' -e 'NativeRunResult ExitSuccess "\\"a\\\\955b\\"\\n"' -e 'NativeRunResult ExitSuccess "\\"\\\\955\\"\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|String append|stringAppend|broad string|String/List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
```

Required closeout checks:

```bash
git diff --check
```

```bash
cabal build all
```

```bash
cabal test
```

```bash
./scripts/thesis-conformance-gate.sh
```

### Review Acceptance Criteria
- `selection-record.json` and `round-plan-record.json` lineage matches active rev-003 and `item-287-string-append-native-tracer`.
- The focused test proves `stringAppend "aλ" "b"` returns `"a\955b"` and empty-side identity cases preserve `"λ"`, through check, run-program, backend LLVM/object validation, emit-native/native object validation, and linked native execution.
- `PrimitiveInventorySpec` confirms the new primitive is inventory-owned and native-lowerable.
- Neighbor text/char tracers from rounds 265-286 still pass.
- Docs and changelog record only the explicit native-capable string append tracer and do not claim `String`/`List Char` conversion, formatting completion, full slicing coverage, complete cursor APIs, parser parity, platform contracts, self-boot proof, roadmap status, or milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringAppend : String -> String -> String` broad string operation across source checking, interpreter/runtime, backend emission, object generation, native execution, inventory, narrow docs, and changelog.
- Out of scope: `String`/`List Char` conversion, formatting helpers, Unicode normalization, locale, regex, broader classification-family completion, case conversion, full slicing coverage, complete cursor API design, parser-owned combinator work, parser parity, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
