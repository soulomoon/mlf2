### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-294-string-from-unit-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable explicit formatting tracer after
`stringFromInt : Int -> String`, `stringFromBool : Bool -> String`, and
`stringFromNat : Nat -> String`: public Prelude
`stringFromUnit : Unit -> String` should format canonical Prelude `Unit` as
the ordinary `String` value `"Unit"` through source checking, `run-program`,
backend LLVM emission, object-code validation, `emit-native`, native object
validation, and linked native execution.

This advances the active milestone-3 formatting gap without claiming the
formatting family is complete. It proves only the narrow `Unit -> String`
value-to-string conversion and stays narrower than general `Show Unit`,
generic ADT formatting, format strings, interpolation, printf-style
formatting, locale behavior, regex, broader collection APIs, parser parity,
platform contracts, compiler package work, driver work, or proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior and readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Use a pure Prelude definition instead of adding a primitive:

```mlfp
def stringFromUnit : Unit -> String = \_ "Unit";
```

The active glossary defines explicit string formatting as value-to-string
conversions and string composition, not a formatting mini-language. This round
should therefore add a narrow public conversion for the existing Prelude
`Unit` type and prove that ordinary checked `.mlfp` code using that conversion
works through native execution. It must not add `__string_from_unit`, a runtime
primitive, a backend primitive lowering case, `Show Unit` claims beyond the
existing instance, or generic ADT rendering.

Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and
  `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation:
  `orchestrator/rounds/round-294/implementation-notes.md`.

Expected non-write scope:

- Do not edit `src/MLF/Primitive/Inventory.hs`,
  `test/PrimitiveInventorySpec.hs`, `src/MLF/Frontend/Program/Run.hs`, or
  `src/MLF/Backend/LLVM/Lower.hs` unless the focused RED failure proves that a
  pure Prelude definition cannot pass through the existing public checker,
  interpreter, and backend/native paths. If that happens, record the blocker
  before widening into primitive/native lowerer work.

First public-interface behavior:

> `.mlfp` source files importing Prelude `Unit(..)` and `stringFromUnit`
> format the canonical unit value as a string by returning `"Unit"` for
> `stringFromUnit Unit`, with matching check, run-program, backend/object,
> emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringFromUnit formats Unit as a string through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. Run the
focused matcher before production changes and record the RED failure in
`implementation-notes.md`; the expected failure should be missing public
Prelude `stringFromUnit`, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch broader formatting, classification,
   cursor, parser, platform, or docs changes before the focused behavior.
3. Write only the RED public behavior test first, using this source:

```mlfp
module Main export (main) {
  import Prelude exposing (Unit(..), stringFromUnit);
  def main : String = stringFromUnit Unit;
}
```

4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - export `stringFromUnit` from the built-in Prelude module header;
   - add `stringFromUnit : Unit -> String` as a pure Prelude definition that
     returns `"Unit"` for the sole canonical `Unit` value;
   - preserve existing `Show Unit`, `stringFromNat`, `stringFromBool`,
     `stringFromInt`, `stringToList`, `stringFromList`, `stringFromChar`,
     `stringAppend`, slicing/search, `stringCharAt`, ASCII classification, IO,
     primitive inventory, runtime primitive dispatch, and native lowering
     behavior.
6. Audit that no new primitive surface was introduced. `stringFromUnit` should
   rely on ordinary checked Prelude code and existing native string literal
   support.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is a next explicit native-capable formatting
   conversion tracer and must not claim `Show Unit`, generic ADT formatting,
   formatting completion, interpolation, printf, locale, regex, full slicing
   coverage, complete cursor APIs, parser parity, platform contracts,
   milestone-3 completion, or self-boot proof.
8. Re-run focused GREEN, neighbor, primitive-absence, evidence, claim-audit,
   full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer, pure
   Prelude implementation note, primitive absence audit, docs updated, and
   scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringFromUnit formats Unit as a string through native execution"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringFromNat formats Nat values as decimal strings through native execution" --match "stringFromBool formats Bool values as strings through native execution" --match "stringFromInt formats Int values as decimal strings through native execution" --match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'
```

Required primitive absence audit:

```bash
rg -n -e '__string_from_unit' -e 'stringFromUnitPrimitiveName' -e 'PrimitiveNativeStringFromUnit' -e 'RuntimeStringFromUnit' src test
```

Expected result for the primitive absence audit: no matches and exit code 1.

Required evidence checks:

```bash
rg -n -e 'stringFromUnit : Unit -> String' -e 'stringFromUnit Unit' -e 'stringFromUnit formats Unit as a string through native execution' src test docs CHANGELOG.md
```

```bash
rg -n 'Unicode scalar|stringFromUnit|stringFromNat|stringFromBool|stringFromInt|String/List Char|formatting|Explicit String Formatting|Show Unit|Show|generic ADT|printf|interpolation|locale|regex|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-294-string-from-unit-native-tracer`.
- The focused test proves `stringFromUnit Unit` returns `"Unit"` through
  check, run-program, backend LLVM/object validation, emit-native/native object
  validation, and linked native execution.
- The implementation adds a narrow pure Prelude public
  `stringFromUnit : Unit -> String` conversion.
- No `__string_from_unit`, `stringFromUnitPrimitiveName`,
  `PrimitiveNativeStringFromUnit`, `RuntimeStringFromUnit`, runtime primitive
  dispatch, or backend primitive lowering case is added.
- No formatting mini-language, interpolation, printf-facing source surface,
  locale behavior, `Show Unit` claim, or generic ADT rendering claim is added.
- Neighbor text/char tracers from rounds 265-293 still pass.
- Docs and changelog record only the explicit native-capable `Unit -> String`
  formatting conversion tracer and do not claim formatting completion, `Show`
  completion, full slicing coverage, complete cursor APIs, parser parity,
  platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringFromUnit : Unit -> String` conversion across
  source checking, interpreter/runtime, backend emission, object generation,
  native execution, narrow docs, and changelog.
- Out of scope: new string formatting primitives, primitive inventory changes,
  runtime primitive dispatch changes, backend primitive lowering changes,
  general `Show` instances, generic ADT rendering, formatting-family
  completion, interpolation, printf-style or format-string parsing, Unicode
  normalization, locale, regex, broader classification family completion, case
  conversion, full slicing coverage, complete cursor API design, broader
  collection APIs, parser-owned combinator work, parser parity, platform
  ABI/FFI/GC contracts, compiler source package implementation, driver work,
  proof records, roadmap status edits, controller state edits, or semantic
  roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps
this as a single serial implementation round with no worker fan-out.
