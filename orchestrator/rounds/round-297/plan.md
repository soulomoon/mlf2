### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-297-string-index-of-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable Plain String Search tracer after
`stringIndexOfChar`: public Prelude
`stringIndexOf : String -> String -> Option Int` should return
`Some <zero-based Unicode-scalar index>` for the first matching substring in a
`String`, `None` when the substring is absent, and `Some 0` for the empty
needle.

This advances the active milestone-3 substring index gap that remains after
the character index tracer. It does not claim complete cursor APIs, splitting,
substring replacement, regex, Unicode normalization, locale behavior,
formatting completion, parser parity, platform contracts, compiler package
work, driver work, or proof records.

Round classification: behavior-changing implementation. The implementer must
use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate
because this changes public language/native behavior and readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringIndexOf : String -> String -> Option Int = __string_index_of;
```

Use `Option Int` for absence, matching the round-296 `stringIndexOfChar`
surface and avoiding a sentinel integer. Indexes are zero-based Unicode scalar
positions in the haystack, not UTF-8 byte offsets. The empty needle returns
`Some 0`, giving a deterministic public boundary before parser parity depends
on substring search behavior.

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
  `orchestrator/rounds/round-297/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `Option(..)` and `stringIndexOf` can
> find the first zero-based Unicode scalar index of a non-empty substring,
> return `None` for an absent substring, and return `Some 0` for an empty
> needle, with matching check, run-program, backend/object,
> emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringIndexOf indexes Unicode scalar substrings through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The focused
example should include these public source fixtures:

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringIndexOf);
  def main : Option Int = stringIndexOf "aλbcλ" "λb";
}
```

Expected observable output: `Some 1\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringIndexOf);
  def main : Option Int = stringIndexOf "abc" "λ";
}
```

Expected observable output: `None\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringIndexOf);
  def main : Option Int = stringIndexOf "λ" "";
}
```

Expected observable output: `Some 0\n`.

Run the focused matcher before production changes and record the RED failure in
`implementation-notes.md`; the expected failure should be missing public
Prelude `stringIndexOf` or missing trusted primitive support, not a malformed
test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch splitting, substring replacement,
   formatting, parser, platform, or proof work before the focused behavior.
3. Write only the RED public behavior test first, using the source fixtures and
   expected outputs above. Keep the test public-interface oriented: check,
   run-program, backend LLVM/object validation, emit-native/native object
   validation, and linked native execution.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_index_of : String -> String -> Option Int` and the
     corresponding primitive inventory constructor/name;
   - export `stringIndexOf` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that searches by Unicode scalar values
     and constructs Prelude `Some index` or `None`;
   - add backend/native lowering that returns Prelude `Option Int` values using
     the existing first-order ADT/native value representation and the
     `stringIndexOfChar` Option tag/layout pattern;
   - preserve existing `stringContainsChar`, `stringContains`,
     `stringStartsWith`, `stringEndsWith`, `stringAppend`,
     `stringReplaceChar`, `stringIndexOfChar`, `stringFromChar`,
     `stringFromInt`, `stringFromBool`, `stringFromNat`, `stringFromUnit`,
     `stringFromList`, `stringToList`, slicing, `stringCharAt`, ASCII
     classification, IO, and native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first native-capable substring index
   search tracer and must not claim splitting, substring replacement,
   replacement completion, regex, formatting completion, complete cursor APIs,
   parser parity, platform contracts, milestone-3 completion, or self-boot
   proof.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence,
   claim-audit, full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   primitive inventory/native-lowerability evidence, docs updated, and scope
   boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringIndexOf indexes Unicode scalar substrings through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringIndexOfChar indexes Unicode scalar characters through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'
```

Required evidence checks:

```bash
rg -n -e 'stringIndexOf : String -> String -> Option Int' -e '__string_index_of' -e 'stringIndexOfPrimitiveName' -e 'PrimitiveNativeStringIndexOf' -e 'RuntimeStringIndexOf' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringIndexOf indexes Unicode scalar substrings through native execution' -e 'stringIndexOf \\"aλbcλ\\" \\"λb\\"' -e 'stringIndexOf \\"abc\\" \\"λ\\"' -e 'stringIndexOf \\"λ\\" \\"\\"' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringIndexOf|stringIndexOfChar|Option Int|stringContains|stringReplaceChar|stringCharAt|Plain String Search|substring index|indexing|replacement|split|regex|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-297-string-index-of-native-tracer`.
- The focused test proves `stringIndexOf "aλbcλ" "λb"` returns `Some 1`,
  `stringIndexOf "abc" "λ"` returns `None`, and
  `stringIndexOf "λ" ""` returns `Some 0` through check, run-program,
  backend LLVM/object validation, emit-native/native object validation, and
  linked native execution.
- The implementation adds a narrow public
  `stringIndexOf : String -> String -> Option Int` Prelude operation backed by
  `__string_index_of`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- No splitting, substring replacement, replacement-family completion, regex,
  formatting, case conversion, Unicode normalization, locale behavior, parser
  combinator, parser parity, platform contract, or proof surface is added.
- Neighbor text/char tracers from earlier milestone-3 rounds still pass.
- Docs and changelog record only the substring index search tracer and do not
  claim splitting, substring replacement, replacement completion, formatting
  completion, full slicing coverage, complete cursor APIs, parser parity,
  platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringIndexOf : String -> String -> Option Int`
  operation across source checking, interpreter/runtime, primitive inventory,
  backend emission, object generation, native execution, narrow docs, and
  changelog.
- Out of scope: splitting, substring replacement, replace-all substring APIs,
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
