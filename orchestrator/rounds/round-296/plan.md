### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-296-string-index-of-char-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable broad string tracer after
`stringReplaceChar`: public Prelude
`stringIndexOfChar : String -> Char -> Option Int` should return
`Some <zero-based Unicode-scalar index>` for the first matching `Char` in a
`String`, and `None` when the character is absent.

This advances the active milestone-3 Plain String Search and parser-needed
cursor gaps by adding the first public character index search operation. It
does not claim complete cursor APIs, substring search indexing, substring
replacement, splitting, regex, Unicode normalization, locale behavior,
formatting completion, parser parity, platform contracts, compiler package
work, driver work, or proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior and readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringIndexOfChar : String -> Char -> Option Int = __string_index_of_char;
```

Use `Option Int` for absence instead of inventing a negative sentinel. The
current Prelude already exports `Option(..)`, native execution already renders
first-order ADT values, and `stringToList` already proves native text
operations can construct Prelude data values. This slice should therefore stay
inside the broad String/Char substrate rather than adding parser-private cursor
state.

Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive ownership and inventory coverage: `src/MLF/Primitive/Inventory.hs`
  and `test/PrimitiveInventorySpec.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive dispatch: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native primitive lowering: `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and
  `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation:
  `orchestrator/rounds/round-296/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `Option(..)` and
> `stringIndexOfChar` can find the first zero-based Unicode scalar index of a
> non-ASCII `Char` in a `String`, returning `Some 1` for the first lambda in
> `"aλbλ"` and `None` when lambda is absent, with matching check,
> run-program, backend/object, emit-native/native-object, and linked native
> execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringIndexOfChar indexes Unicode scalar characters through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The focused
example should include both public source fixtures:

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringIndexOfChar);
  def main : Option Int = stringIndexOfChar "aλbλ" 'λ';
}
```

Expected observable output: `Some 1\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringIndexOfChar);
  def main : Option Int = stringIndexOfChar "ab" 'λ';
}
```

Expected observable output: `None\n`.

Run the focused matcher before production changes and record the RED failure in
`implementation-notes.md`; the expected failure should be missing public
Prelude `stringIndexOfChar` or missing trusted primitive support, not a
malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch substring indexing, splitting,
   replacement, formatting, parser, platform, or proof work before the focused
   behavior.
3. Write only the RED public behavior test first, using the source fixtures and
   expected outputs above. Keep the test public-interface oriented: check,
   run-program, backend LLVM/object validation, emit-native/native object
   validation, and linked native execution.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_index_of_char : String -> Char -> Option Int` and the
     corresponding primitive inventory constructor/name;
   - add an `Option` primitive type helper if needed by the inventory owner;
   - export `stringIndexOfChar` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that scans Unicode scalar `Char` values
     and constructs Prelude `Some index` or `None`;
   - add backend/native lowering that returns Prelude `Option Int` values using
     the existing first-order ADT/native value representation patterns;
   - preserve existing `stringContainsChar`, `stringContains`,
     `stringStartsWith`, `stringEndsWith`, `stringAppend`,
     `stringReplaceChar`, `stringFromChar`, `stringFromInt`,
     `stringFromBool`, `stringFromNat`, `stringFromUnit`, `stringFromList`,
     `stringToList`, slicing, `stringCharAt`, ASCII classification, IO, and
     native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first native-capable character index
   search tracer and must not claim substring indexing, splitting, replacement
   completion, regex, formatting completion, complete cursor APIs, parser
   parity, platform contracts, milestone-3 completion, or self-boot proof.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence,
   claim-audit, full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   primitive inventory/native-lowerability evidence, docs updated, and scope
   boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringIndexOfChar indexes Unicode scalar characters through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'
```

Required evidence checks:

```bash
rg -n -e 'stringIndexOfChar : String -> Char -> Option Int' -e '__string_index_of_char' -e 'stringIndexOfCharPrimitiveName' -e 'PrimitiveNativeStringIndexOfChar' -e 'RuntimeStringIndexOfChar' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringIndexOfChar indexes Unicode scalar characters through native execution' -e 'stringIndexOfChar \\"aλbλ\\"' -e 'stringIndexOfChar \\"ab\\"' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringIndexOfChar|Option Int|stringReplaceChar|stringContainsChar|stringContains|stringCharAt|Plain String Search|indexing|replacement|split|regex|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-296-string-index-of-char-native-tracer`.
- The focused test proves `stringIndexOfChar "aλbλ" 'λ'` returns `Some 1`
  and `stringIndexOfChar "ab" 'λ'` returns `None` through check, run-program,
  backend LLVM/object validation, emit-native/native object validation, and
  linked native execution.
- The implementation adds a narrow public
  `stringIndexOfChar : String -> Char -> Option Int` Prelude operation backed
  by `__string_index_of_char`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- No substring index operation, substring replacement, splitting,
  replacement-family completion, regex, formatting, case conversion, Unicode
  normalization, locale behavior, parser combinator, parser parity, platform
  contract, or proof surface is added.
- Neighbor text/char tracers from earlier milestone-3 rounds still pass.
- Docs and changelog record only the character index search tracer and do not
  claim substring indexing, splitting, replacement completion, formatting
  completion, full slicing coverage, complete cursor APIs, parser parity,
  platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringIndexOfChar : String -> Char -> Option Int`
  operation across source checking, interpreter/runtime, primitive inventory,
  backend emission, object generation, native execution, narrow docs, and
  changelog.
- Out of scope: substring index APIs, substring replacement, replace-all
  substring APIs, splitting, regex, formatting-family completion,
  interpolation, printf-style or format-string parsing, Unicode normalization,
  locale, case conversion, broader classification family completion, full
  slicing coverage, complete cursor API design, broader collection APIs,
  parser-owned combinator work, parser parity, platform ABI/FFI/GC contracts,
  compiler source package implementation, driver work, proof records, roadmap
  status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps
this as a single serial implementation round with no worker fan-out.
