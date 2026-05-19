### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-289-string-from-list-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable broad text tracer: public Prelude
`stringFromList : List Char -> String` should construct a `String` from a
Unicode scalar `List Char` through source checking, `run-program`, backend
LLVM emission, object-code validation, `emit-native`, native object
validation, and linked native execution.

This advances the explicit `String`/`List Char` conversion part of milestone-3
after round-288 added `stringFromChar : Char -> String`. It proves only the
`List Char -> String` direction and stays narrower than reverse
`String -> List Char` conversion, formatting family completion, broader
Unicode category classification, complete cursor API design, parser
combinators, parser parity, platform contracts, compiler package work, driver
work, or proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Prefer a high-level Prelude definition composed from existing native-capable
building blocks instead of adding a new trusted primitive:

```mlfp
def stringFromList : List Char -> String =
  λchars case chars of {
    Nil -> "";
    Cons head tail -> stringAppend (stringFromChar head) (stringFromList tail)
  };
```

The implementer may touch backend/native lowering only if the focused RED test
proves an existing native lowering gap for this public behavior. Do not add
`__string_from_list`, `PrimitiveNativeStringFromList`, or
`RuntimeStringFromList` unless the high-level Prelude route is demonstrated
insufficient and the implementation notes explain why. The default target is a
Prelude-level library function over existing `List`, `Char`, `String`,
`stringFromChar`, and `stringAppend` support.

Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and
  `CHANGELOG.md`.
- Backend/native lowering only if needed for the high-level Prelude definition:
  `src/MLF/Backend/LLVM/Lower.hs`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation:
  `orchestrator/rounds/round-289/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `List(..)` and `stringFromList`
> construct strings from `List Char` values by returning `"a\955"` for
> `stringFromList (Cons 'a' (Cons 'λ' Nil))` and `""` for
> `stringFromList Nil`, with matching check, run-program, backend/object,
> emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringFromList converts List Char values to Unicode scalar strings through
native execution`. Use the existing `withTempProgram`, `checkProgramFile`,
`runProgramFile`, `emitBackendFile`, `emitNativeFile`,
`validateLLVMAssembly`, `validateLLVMObjectCode`, and
`runLLVMNativeExecutable` helpers. Run the focused matcher before production
changes and record the RED failure in `implementation-notes.md`; the expected
failure should be missing public Prelude `stringFromList` or a native lowering
gap in the high-level Prelude definition, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch broad text tests before
   implementation.
3. Write only the RED public behavior test first, using these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringFromList);
  def main : String = stringFromList (Cons 'a' (Cons 'λ' Nil));
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringFromList);
  def main : String = stringFromList Nil;
}
```

4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - export `stringFromList` from the built-in Prelude;
   - define `stringFromList : List Char -> String` in Prelude by pattern
     matching `Nil` and `Cons`, using `stringFromChar` and `stringAppend`;
   - if the focused GREEN fails only in backend/native lowering, fix the
     narrow native issue exposed by recursive Prelude List/Char/String
     composition without widening into a new text primitive family;
   - preserve existing `stringFromChar`, `stringAppend`, slicing/search,
     `stringCharAt`, ASCII classification, IO, and native rendering behavior.
6. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first native-capable `List Char -> String`
   conversion tracer and must not claim reverse `String -> List Char`
   conversion, formatting completion, full slicing coverage, complete cursor
   APIs, parser parity, platform contracts, milestone-3 completion, or
   self-boot proof.
7. Re-run focused GREEN, neighbor, evidence, primitive absence, full
   build/test, and thesis-gate commands.
8. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer, whether
   a backend lowering fix was needed, docs updated, and scope boundaries
   preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringFromList converts List Char values to Unicode scalar strings through native execution"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n -e 'stringFromList : List Char -> String' -e 'def stringFromList' -e 'stringFromList converts List Char values to Unicode scalar strings through native execution' -e 'stringFromList (Cons' src/MLF/Frontend/Program/Prelude.hs test/BackendLLVMSpec.hs docs CHANGELOG.md
```

```bash
rg -n 'Unicode scalar|stringFromList|String/List Char|List Char -> String|String -> List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
```

Primitive absence audit for the preferred high-level Prelude implementation:

```bash
rg -n '__string_from_list|PrimitiveNativeStringFromList|RuntimeStringFromList' src test
```

Expected result for the primitive absence audit: no matches. If there are
matches, `implementation-notes.md` must justify why the high-level Prelude
implementation could not satisfy the focused public behavior without a new
trusted primitive.

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
  rev-003 and `item-289-string-from-list-native-tracer`.
- The focused test proves `stringFromList (Cons 'a' (Cons 'λ' Nil))` returns
  `"a\955"` and `stringFromList Nil` returns `""`, through check,
  run-program, backend LLVM/object validation, emit-native/native object
  validation, and linked native execution.
- The implementation is a high-level Prelude function over existing
  native-capable `List`, `Char`, `String`, `stringFromChar`, and
  `stringAppend` behavior, unless a recorded RED/GREEN blocker justifies a
  narrower backend fix or new primitive.
- Neighbor text/char tracers from rounds 265-288 still pass.
- Docs and changelog record only the explicit native-capable
  `List Char -> String` conversion tracer and do not claim reverse
  `String -> List Char` conversion, formatting completion, full slicing
  coverage, complete cursor APIs, parser parity, platform contracts,
  self-boot proof, roadmap status, or milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringFromList : List Char -> String` conversion across
  source checking, interpreter/runtime, backend emission, object generation,
  native execution, narrow docs, and changelog.
- Out of scope: reverse `String -> List Char` conversion, formatting family
  completion, Unicode normalization, locale, regex, broader classification
  family completion, case conversion, full slicing coverage, complete cursor API
  design, parser-owned combinator work, parser parity, platform ABI/FFI/GC
  contracts, compiler source package implementation, driver work, proof records,
  roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps
this as a single serial implementation round with no worker fan-out.
