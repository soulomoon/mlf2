### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-288-string-from-char-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable text substrate tracer: public Prelude
`stringFromChar : Char -> String` should construct a singleton `String` from a
valid Unicode scalar `Char` through source checking, `run-program`, backend
LLVM emission, object-code validation, `emit-native`, native object
validation, and linked native execution.

This advances the broad text substrate after the completed literal, length,
empty/search/prefix/suffix, append, slicing/cursor, and ASCII classification
tracers from rounds 265-287. It is the first narrow public Char-to-String
conversion/formatting seed, but it stays narrower than full `String`/`List
Char` conversion, formatting family completion, broader Unicode category
classification, complete cursor API design, parser combinators, parser parity,
platform contracts, compiler package work, driver work, or proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Implementation ownership for this round is the single implementer in the
canonical round-288 worktree. Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive inventory coverage: `test/PrimitiveInventorySpec.hs`.
- Primitive registry and native support classification:
  `src/MLF/Primitive/Inventory.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native LLVM lowering and runtime declarations/helpers:
  `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and
  `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation:
  `orchestrator/rounds/round-288/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `stringFromChar` construct singleton
> strings from `Char` values by returning `"\955"` for `stringFromChar 'λ'`
> and `"A"` for `stringFromChar 'A'`, with matching check, run-program,
> backend/object, emit-native/native-object, and linked native execution
> evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringFromChar converts Unicode scalar Chars to singleton strings through
native execution`. Use the existing `withTempProgram`, `checkProgramFile`,
`runProgramFile`, `emitBackendFile`, `emitNativeFile`,
`validateLLVMAssembly`, `validateLLVMObjectCode`, and
`runLLVMNativeExecutable` helpers. Run the focused matcher before production
changes and record the RED failure in `implementation-notes.md`; the expected
failure should be missing public Prelude/native-capable `stringFromChar` or
missing backend lowering, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch broad text tests before
   implementation.
3. Write only the RED public behavior test first, using these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringFromChar);
  def main : String = stringFromChar 'λ';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringFromChar);
  def main : String = stringFromChar 'A';
}
```

4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add reserved primitive `__string_from_char : Char -> String` and native
     support classification in `MLF.Primitive.Inventory`;
   - export `stringFromChar : Char -> String` from the built-in Prelude;
   - add `run-program` behavior by converting `RuntimeLit (LChar scalar)` to
     `RuntimeLit (LString [scalar])`;
   - add backend/native lowering over the existing `Char` representation,
     returning a newly allocated null-terminated UTF-8 `String` for the scalar;
   - preserve existing `Char`, `String`, append, slicing/search,
     `stringCharAt`, ASCII classification, IO, and native rendering behavior.
6. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is an explicit native-capable singleton
   Char-to-String tracer and must not claim full `String`/`List Char`
   conversion, formatting completion, full slicing coverage, complete cursor
   APIs, parser parity, platform contracts, milestone-3 completion, or
   self-boot proof.
7. Re-run focused GREEN, inventory, neighbor, evidence, full build/test, and
   thesis-gate commands.
8. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer, docs
   updated, and scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution"'
```

Required focused inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringAppend concatenates Unicode scalar strings through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution" --match "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" --match "charIsAsciiIdentifierContinue classifies ASCII identifier-continuation Char values through native execution" --match "charIsAsciiIdentifierStart classifies ASCII identifier-start Char values through native execution" --match "charIsAsciiAlphaNum classifies ASCII alphanumeric Char values through native execution" --match "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution" --match "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" --match "charIsAsciiLower classifies ASCII lowercase Char values through native execution" --match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringFromChar : Char -> String|__string_from_char|stringFromCharPrimitiveName|PrimitiveNativeStringFromChar|RuntimeStringFromChar' src test docs CONTEXT.md README.md CHANGELOG.md
```

```bash
rg -n -e 'stringFromChar converts Unicode scalar Chars to singleton strings through native execution' -e "stringFromChar 'λ'" -e "stringFromChar 'A'" test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|stringFromChar|String/List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
- `selection-record.json` and `round-plan-record.json` lineage matches active
  rev-003 and `item-288-string-from-char-native-tracer`.
- The focused test proves `stringFromChar 'λ'` returns `"\955"` and
  `stringFromChar 'A'` returns `"A"`, through check, run-program, backend
  LLVM/object validation, emit-native/native object validation, and linked
  native execution.
- `PrimitiveInventorySpec` confirms the new primitive is inventory-owned and
  native-lowerable.
- Neighbor text/char tracers from rounds 265-287 still pass.
- Docs and changelog record only the explicit native-capable singleton
  Char-to-String tracer and do not claim full `String`/`List Char` conversion,
  formatting completion, full slicing coverage, complete cursor APIs, parser
  parity, platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringFromChar : Char -> String` operation across
  source checking, interpreter/runtime, backend emission, object generation,
  native execution, inventory, narrow docs, and changelog.
- Out of scope: full `String`/`List Char` conversion, formatting family
  completion, Unicode normalization, locale, regex, broader classification
  family completion, case conversion, full slicing coverage, complete cursor API
  design, parser-owned combinator work, parser parity, platform ABI/FFI/GC
  contracts, compiler source package implementation, driver work, proof records,
  roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps
this as a single serial implementation round with no worker fan-out.
