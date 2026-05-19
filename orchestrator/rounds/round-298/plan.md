### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-298-string-replace-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable Plain String Search/replacement tracer
after `stringIndexOf`: public Prelude
`stringReplace : String -> String -> String -> String` replaces every
non-overlapping occurrence of a non-empty needle with a replacement string,
preserves no-match inputs, and treats the empty needle as a no-op.

This advances the active milestone-3 replacement gap that remains after the
character replacement and substring index tracers. It does not claim splitting,
replace-all family completion beyond this exact operation, regex, Unicode
normalization, locale behavior, formatting completion, case conversion,
complete cursor APIs, parser parity, platform contracts, compiler package
work, driver work, proof records, or milestone-3 completion.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior and readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringReplace : String -> String -> String -> String = __string_replace;
```

Define replacement over Unicode scalar string boundaries, not UTF-8 byte
positions. Replacement is left-to-right and non-overlapping. The empty needle
returns the original string unchanged so the public API is total and avoids an
implicit insertion/formatting mini-language before parser parity.

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
  `orchestrator/rounds/round-298/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `stringReplace` can replace
> non-overlapping Unicode scalar substrings, preserve no-match inputs, and
> no-op on an empty needle, with matching check, run-program, backend/object,
> emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringReplace replaces Unicode scalar substrings through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The focused
example should include these public source fixtures:

```mlfp
module Main export (main) {
  import Prelude exposing (stringReplace);
  def main : String = stringReplace "aλbλb" "λb" "WXYZ";
}
```

Expected observable output: `"aWXYZWXYZ"\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (stringReplace);
  def main : String = stringReplace "abc" "λ" "x";
}
```

Expected observable output: `"abc"\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (stringReplace);
  def main : String = stringReplace "abc" "" "x";
}
```

Expected observable output: `"abc"\n`.

Run the focused matcher before production changes and record the RED failure
in `implementation-notes.md`; the expected failure should be missing public
Prelude `stringReplace` or missing trusted primitive support, not a malformed
test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch splitting, regex, formatting,
   cursor, parser, platform, driver, or proof work before the focused
   behavior.
3. Write only the RED public behavior test first, using the source fixtures and
   expected outputs above. Keep the test public-interface oriented: check,
   run-program, backend LLVM/object validation, emit-native/native object
   validation, and linked native execution.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_replace : String -> String -> String -> String` and the
     corresponding primitive inventory constructor/name;
   - export `stringReplace` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that walks by Unicode scalar values,
     replaces non-overlapping non-empty needles left-to-right, preserves
     no-match inputs, and returns the original string for an empty needle;
   - add backend/native lowering with the same semantics, including allocation
     that handles replacement strings longer than the matched needle;
   - preserve existing `stringReplaceChar`, `stringIndexOf`,
     `stringIndexOfChar`, `stringContainsChar`, `stringContains`,
     `stringStartsWith`, `stringEndsWith`, `stringAppend`, `stringFromChar`,
     `stringFromInt`, `stringFromBool`, `stringFromNat`, `stringFromUnit`,
     `stringFromList`, `stringToList`, slicing, `stringCharAt`, ASCII
     classification, IO, and native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first native-capable substring replacement
   tracer and must not claim splitting, regex, replacement-family completion,
   formatting completion, complete cursor APIs, parser parity, platform
   contracts, self-boot proof, roadmap status, or milestone-3 completion.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence, claim-audit,
   full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   primitive inventory/native-lowerability evidence, docs updated, and scope
   boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringReplace replaces Unicode scalar substrings through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringIndexOf indexes Unicode scalar substrings through native execution" --match "stringIndexOfChar indexes Unicode scalar characters through native execution" --match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'
```

Required evidence checks:

```bash
rg -n -e 'stringReplace : String -> String -> String -> String' -e '__string_replace' -e 'stringReplacePrimitiveName' -e 'PrimitiveNativeStringReplace' -e 'RuntimeStringReplace' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringReplace replaces Unicode scalar substrings through native execution' -e 'stringReplace "aλbλb" "λb" "WXYZ"' -e 'stringReplace "abc" "λ" "x"' -e 'stringReplace "abc" "" "x"' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringReplace|stringReplaceChar|stringIndexOf|Plain String Search|substring replacement|replacement|split|regex|formatting|classification|cursor|Unicode normalization|locale|case conversion|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-298-string-replace-native-tracer`.
- The focused test proves `stringReplace "aλbλb" "λb" "WXYZ"` returns
  `"aWXYZWXYZ"`, `stringReplace "abc" "λ" "x"` returns `"abc"`, and
  `stringReplace "abc" "" "x"` returns `"abc"` through check, run-program,
  backend LLVM/object validation, emit-native/native object validation, and
  linked native execution.
- The implementation adds a narrow public
  `stringReplace : String -> String -> String -> String` Prelude operation
  backed by `__string_replace`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- Replacement semantics are left-to-right, non-overlapping, Unicode scalar
  boundary aware, and empty-needle no-op.
- No splitting, regex, replacement-family completion beyond this exact
  operation, formatting, case conversion, Unicode normalization, locale
  behavior, parser combinator, parser parity, platform contract, compiler
  package, driver, proof, roadmap status, or controller state surface is
  added.
- Neighbor text/char tracers from earlier milestone-3 rounds still pass.
- Docs and changelog record only the substring replacement tracer and do not
  claim splitting, regex, formatting completion, full slicing coverage,
  complete cursor APIs, parser parity, platform contracts, self-boot proof,
  roadmap status, or milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public
  `stringReplace : String -> String -> String -> String` operation across
  source checking, interpreter/runtime, primitive inventory, backend emission,
  object generation, native execution, narrow docs, and changelog.
- Out of scope: splitting, split-family collection APIs, regex, replacement
  APIs beyond this exact substring replacement operation, interpolation,
  printf-style or format-string parsing, Unicode normalization, locale, case
  conversion, broader classification family completion, full slicing coverage,
  complete cursor API design, broader collection APIs, parser-owned combinator
  work, parser parity, platform ABI/FFI/GC contracts, compiler source package
  implementation, driver work, proof records, roadmap status edits, controller
  state edits, or semantic roadmap updates.
