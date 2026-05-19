### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-295-string-replace-char-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable broad string tracer after the current
search, slicing, conversion, classification, and narrow formatting tracers:
public Prelude `stringReplaceChar : String -> Char -> Char -> String` should
replace every matching Unicode scalar `Char` in a `String` and leave strings
unchanged when the target character is absent.

This advances the active milestone-3 plain search/replacement gap recorded for
the Broad String Library without claiming the replacement family is complete.
It proves only the narrow character replacement operation and stays narrower
than substring replacement, splitting, regex, case conversion, Unicode
normalization, locale behavior, formatting completion, complete cursor APIs,
parser parity, platform contracts, compiler package work, driver work, or
proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior and readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringReplaceChar : String -> Char -> Char -> String = __string_replace_char;
```

The active glossary names replacement as part of the non-regex plain search
surface over Unicode scalar strings. A character-level replacement tracer is a
lawful small vertical slice because it reuses the existing `String`/`Char`
substrate shape already proven by `stringContainsChar`, `stringFromChar`,
`stringCharAt`, and the string literal/native tracers.

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
  `orchestrator/rounds/round-295/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `stringReplaceChar` can replace every
> occurrence of a non-ASCII Unicode scalar character in a `String`, with
> matching check, run-program, backend/object, emit-native/native-object, and
> linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringReplaceChar replaces Unicode scalar characters through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The initial
positive case should use this public source:

```mlfp
module Main export (main) {
  import Prelude exposing (stringReplaceChar);
  def main : String = stringReplaceChar "aλbλ" 'λ' 'x';
}
```

The expected observable output is `"axbx"\n`. Run the focused matcher before
production changes and record the RED failure in `implementation-notes.md`; the
expected failure should be missing public Prelude `stringReplaceChar` or missing
trusted primitive support, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch broader replacement, splitting,
   formatting, classification, cursor, parser, platform, or proof work before
   the focused behavior.
3. Write only the RED public behavior test first. It should prove both the
   non-ASCII replacement case above and a no-match preservation case such as
   `stringReplaceChar "ab" 'λ' 'x' == "ab"` within the same public native
   tracer example only if doing so fits the local helper style without
   widening into a second feature.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_replace_char : String -> Char -> Char -> String` and the
     corresponding primitive inventory constructor/name;
   - export `stringReplaceChar` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that replaces matching Unicode scalar
     characters and preserves no-match inputs;
   - add backend/native lowering that matches the existing UTF-8 string and
     scalar `Char` helper patterns;
   - preserve existing `stringContainsChar`, `stringContains`,
     `stringStartsWith`, `stringEndsWith`, `stringAppend`, `stringFromChar`,
     `stringFromInt`, `stringFromBool`, `stringFromNat`, `stringFromUnit`,
     `stringFromList`, `stringToList`, slicing, `stringCharAt`, ASCII
     classification, IO, and native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first native-capable character replacement
   tracer and must not claim substring replacement, splitting, replacement
   completion, regex, formatting completion, parser parity, platform contracts,
   milestone-3 completion, or self-boot proof.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence,
   claim-audit, full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   primitive inventory/native-lowerability evidence, docs updated, and scope
   boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringReplaceChar replaces Unicode scalar characters through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringContainsChar searches Unicode scalars through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringFromUnit formats Unit as a string through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n -e 'stringReplaceChar : String -> Char -> Char -> String' -e '__string_replace_char' -e 'stringReplaceCharPrimitiveName' -e 'PrimitiveNativeStringReplaceChar' -e 'RuntimeStringReplaceChar' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringReplaceChar replaces Unicode scalar characters through native execution' -e 'stringReplaceChar "aλbλ"' -e 'stringReplaceChar "ab"' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringReplaceChar|stringContainsChar|stringContains|stringFromUnit|String/List Char|replacement|split|search|regex|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-295-string-replace-char-native-tracer`.
- The focused test proves `stringReplaceChar "aλbλ" 'λ' 'x'` returns
  `"axbx"` through check, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution.
- The implementation adds a narrow public
  `stringReplaceChar : String -> Char -> Char -> String` Prelude operation
  backed by `__string_replace_char`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- No substring replacement, replacement-family completion, splitting, regex,
  formatting, case conversion, Unicode normalization, locale behavior, parser
  combinator, parser parity, platform contract, or proof surface is added.
- Neighbor text/char tracers from earlier milestone-3 rounds still pass.
- Docs and changelog record only the character replacement tracer and do not
  claim substring replacement, splitting, replacement completion, formatting
  completion, full slicing coverage, complete cursor APIs, parser parity,
  platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringReplaceChar : String -> Char -> Char -> String`
  operation across source checking, interpreter/runtime, primitive inventory,
  backend emission, object generation, native execution, narrow docs, and
  changelog.
- Out of scope: substring replacement, replace-all substring APIs, splitting,
  regex, formatting-family completion, interpolation, printf-style or
  format-string parsing, Unicode normalization, locale, case conversion,
  broader classification family completion, full slicing coverage, complete
  cursor API design, broader collection APIs, parser-owned combinator work,
  parser parity, platform ABI/FFI/GC contracts, compiler source package
  implementation, driver work, proof records, roadmap status edits,
  controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps
this as a single serial implementation round with no worker fan-out.
