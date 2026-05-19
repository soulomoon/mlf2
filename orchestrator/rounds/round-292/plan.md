### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-292-string-from-bool-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable broad text tracer after
`stringFromInt : Int -> String`: public Prelude
`stringFromBool : Bool -> String` should format `Bool` values as ordinary
`String` values through source checking, `run-program`, backend LLVM emission,
object-code validation, `emit-native`, native object validation, and linked
native execution.

This advances the explicit formatting portion of milestone-3 without claiming
formatting-family completion. It proves only the narrow Boolean
value-to-string conversion and stays narrower than general `Show` support,
format strings, interpolation, printf-style formatting, locale behavior, regex,
broader collection APIs, parser parity, platform contracts, compiler package
work, driver work, or proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior and readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Use an explicit Prelude conversion rather than widening the existing `Show`
class in this formatting tracer:

```mlfp
def stringFromBool : Bool -> String = __string_from_bool;
```

The active glossary defines explicit string formatting as value-to-string
conversions and string composition, not a formatting mini-language. This round
should therefore add a narrow primitive-backed conversion and leave `Show Bool`,
generic formatting classes, interpolation, and printf-style formatting for
later selected work. The primitive must produce ordinary valid `.mlfp`
`String` values and must not expose or stabilize any native string heap layout
or C ABI contract.

Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive ownership and inventory coverage:
  `src/MLF/Primitive/Inventory.hs` and `test/PrimitiveInventorySpec.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native lowering for the new primitive:
  `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and
  `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation:
  `orchestrator/rounds/round-292/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `stringFromBool` format `Bool`
> values as strings by returning `"true"` for `stringFromBool true` and
> `"false"` for `stringFromBool false`, with matching check, run-program,
> backend/object, emit-native/native-object, and linked native execution
> evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringFromBool formats Bool values as strings through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. Run the
focused matcher before production changes and record the RED failure in
`implementation-notes.md`; the expected failure should be missing public
Prelude `stringFromBool` or missing primitive/native support, not a malformed
test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch broader formatting, classification,
   cursor, parser, platform, or docs changes before the focused behavior.
3. Write only the RED public behavior test first, using these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringFromBool);
  def main : String = stringFromBool true;
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringFromBool);
  def main : String = stringFromBool false;
}
```

4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add the primitive owner entry, exported primitive name, native support
     constructor, and type for `__string_from_bool : Bool -> String`;
   - update primitive inventory tests so native-lowerable support is derived
     from the shared primitive inventory owner;
   - expose `stringFromBool : Bool -> String` from the built-in Prelude;
   - implement `run-program` conversion from `Bool` to `"true"` or `"false"`
     with no locale or formatting mini-language behavior;
   - lower the primitive natively by producing normal valid `.mlfp` `String`
     values containing `true` or `false`; the lowerer may reuse existing
     private string-literal/global helpers, but that choice must remain a
     backend implementation detail and not a public platform contract;
   - preserve existing `stringFromInt`, `stringToList`, `stringFromList`,
     `stringFromChar`, `stringAppend`, slicing/search, `stringCharAt`, ASCII
     classification, IO, and native rendering behavior.
6. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the next explicit native-capable formatting
   conversion tracer and must not claim `Show Bool`, formatting completion,
   interpolation, printf, locale, regex, full slicing coverage, complete cursor
   APIs, parser parity, platform contracts, milestone-3 completion, or
   self-boot proof.
7. Re-run focused GREEN, primitive inventory, neighbor, evidence, claim-audit,
   full build/test, and thesis-gate commands.
8. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   primitive/runtime/native formatting notes, docs updated, and scope
   boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringFromBool formats Bool values as strings through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringFromInt formats Int values as decimal strings through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "stringFromList converts List Char values to Unicode scalar strings through native execution" --match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n -e 'stringFromBool : Bool -> String' -e '__string_from_bool' -e 'stringFromBoolPrimitiveName' -e 'PrimitiveNativeStringFromBool' -e 'RuntimeStringFromBool' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringFromBool formats Bool values as strings through native execution' -e 'stringFromBool true' -e 'stringFromBool false' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|stringFromBool|stringFromInt|String/List Char|formatting|Explicit String Formatting|Show Bool|Show|printf|interpolation|locale|regex|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-292-string-from-bool-native-tracer`.
- The focused test proves `stringFromBool true` returns `"true"` and
  `stringFromBool false` returns `"false"`, through check, run-program,
  backend LLVM/object validation, emit-native/native object validation, and
  linked native execution.
- The implementation adds a narrow primitive-backed public Prelude
  `stringFromBool : Bool -> String`; the primitive inventory, run-program
  runtime, and backend/native lowering agree on `__string_from_bool`.
- Boolean output is exact and lowercase: `"true"` and `"false"`.
- No formatting mini-language, interpolation, printf-facing source surface,
  locale behavior, or `Show Bool` claim is added.
- Neighbor text/char tracers from rounds 265-291 still pass.
- Docs and changelog record only the explicit native-capable `Bool -> String`
  formatting conversion tracer and do not claim formatting completion, `Show`
  completion, full slicing coverage, complete cursor APIs, parser parity,
  platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringFromBool : Bool -> String` conversion across
  source checking, interpreter/runtime, backend emission, object generation,
  native execution, primitive inventory ownership, narrow docs, and changelog.
- Out of scope: general `Show` instances, formatting-family completion,
  interpolation, printf-style or format-string parsing, Unicode normalization,
  locale, regex, broader classification family completion, case conversion,
  full slicing coverage, complete cursor API design, broader collection APIs,
  parser-owned combinator work, parser parity, platform ABI/FFI/GC contracts,
  compiler source package implementation, driver work, proof records, roadmap
  status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps
this as a single serial implementation round with no worker fan-out.
