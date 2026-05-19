### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-299-string-split-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable Plain String Search tracer after
`stringReplace`: public Prelude
`stringSplit : String -> String -> List String` should split Unicode scalar
strings on a non-empty substring delimiter, preserve empty segments, return a
singleton list for no-match inputs, and treat an empty delimiter as a no-op
singleton list.

This closes only the first split-family tracer in milestone-3. It does not
claim split-family completion, regex, Unicode normalization, locale behavior,
case conversion, formatting completion, complete cursor APIs, parser parity,
platform contracts, compiler package work, driver work, proof records, or
milestone-3 completion.

Round classification: behavior-changing implementation. The implementer must
load and follow `/Users/ares/.agents/skills/tdd/SKILL.md` using a vertical
RED -> GREEN -> refactor cycle, and must run the thesis gate because this
changes public language/native behavior and self-boot readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringSplit : String -> String -> List String = __string_split;
```

Define splitting over Unicode scalar string boundaries, not UTF-8 byte
positions. The delimiter is matched left-to-right and non-overlapping. A
non-empty delimiter produces all segments, including leading and trailing empty
segments. A delimiter that is absent returns `Cons input Nil`. An empty
delimiter returns `Cons input Nil` so the API stays total without smuggling a
source cursor or character-explosion policy into this split tracer.

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
  `orchestrator/rounds/round-299/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `List(..)` and `stringSplit` can split
> a Unicode scalar string on a non-empty Unicode scalar substring delimiter,
> preserve leading/trailing empty segments, preserve no-match inputs as a
> singleton list, and no-op on an empty delimiter, with matching check,
> run-program, backend/object, emit-native/native-object, and linked native
> execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringSplit splits Unicode scalar substrings through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The focused
example should include these public source fixtures:

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringSplit);
  def main : List String = stringSplit "aλbλc" "λ";
}
```

Expected observable output:
`Cons "a" (Cons "b" (Cons "c" Nil))\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringSplit);
  def main : List String = stringSplit "abc" "λ";
}
```

Expected observable output:
`Cons "abc" Nil\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringSplit);
  def main : List String = stringSplit "abc" "";
}
```

Expected observable output:
`Cons "abc" Nil\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringSplit);
  def main : List String = stringSplit "λaλ" "λ";
}
```

Expected observable output:
`Cons "" (Cons "a" (Cons "" Nil))\n`.

Run the focused matcher before production changes and record the RED failure in
`implementation-notes.md`; the expected failure should be missing public
Prelude `stringSplit` or missing trusted primitive support, not a malformed
test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch broader split-family APIs,
   collection helpers, parser cursor work, parser parity, platform contracts,
   driver work, or proof records before the focused public behavior.
3. Write only the RED public behavior test first using the source fixtures and
   expected outputs above. Keep the test public-interface oriented: check,
   run-program, backend LLVM/object validation, emit-native/native object
   validation, and linked native execution.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_split : String -> String -> List String` and the
     corresponding primitive inventory constructor/name;
   - export `stringSplit` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that walks by Unicode scalar values,
     splits left-to-right on non-overlapping non-empty delimiters, preserves
     empty segments, returns `Cons input Nil` for absent delimiters, and returns
     `Cons input Nil` for an empty delimiter;
   - add backend/native lowering with the same semantics, constructing Prelude
     `List String` values using the existing first-order ADT/native value
     representation and existing string allocation/rendering conventions;
   - preserve existing `stringReplace`, `stringReplaceChar`, `stringIndexOf`,
     `stringIndexOfChar`, `stringContainsChar`, `stringContains`,
     `stringStartsWith`, `stringEndsWith`, `stringAppend`, `stringFromChar`,
     `stringFromInt`, `stringFromBool`, `stringFromNat`, `stringFromUnit`,
     `stringFromList`, `stringToList`, slicing, `stringCharAt`, ASCII
     classification, IO, and native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first native-capable split-family tracer
   and must not claim regex, Unicode normalization, locale behavior, case
   conversion, split-family completion beyond this exact operation, formatting
   completion, complete cursor APIs, parser parity, platform contracts,
   milestone-3 completion, or self-boot proof.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence, claim-audit,
   generated-artifact, full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   `List String` construction/rendering evidence, primitive
   inventory/native-lowerability evidence, docs updated, generated-artifact
   cleanup, and scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringSplit splits Unicode scalar substrings through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringReplace replaces Unicode scalar substrings through native execution" --match "stringIndexOf indexes Unicode scalar substrings through native execution" --match "stringIndexOfChar indexes Unicode scalar characters through native execution" --match "stringReplaceChar replaces Unicode scalar characters through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringFromList converts List Char values to Unicode scalar strings through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'
```

Required evidence checks:

```bash
rg -n -e 'stringSplit : String -> String -> List String' -e '__string_split' -e 'stringSplitPrimitiveName' -e 'PrimitiveNativeStringSplit' -e 'RuntimeStringSplit' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringSplit splits Unicode scalar substrings through native execution' -e 'stringSplit "aλbλc" "λ"' -e 'stringSplit "abc" "λ"' -e 'stringSplit "abc" ""' -e 'stringSplit "λaλ" "λ"' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringSplit|stringReplace|stringIndexOf|Plain String Search|split-family|splitting|List String|regex|Unicode normalization|locale|case conversion|formatting|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
```

Required generated-artifact audit:

```bash
git diff --name-only -- '*.d' '*.rlib' 'Cargo.lock' 'dist-newstyle/**' 'target/**'
```

Expected result: no generated artifact churn remains in the final diff. If
native validation rewrites `runtime/mlfp_io/target/release/libmlfp_io.d`,
restore or otherwise remove that generated depfile churn before review.

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
  rev-003 and `item-299-string-split-native-tracer`.
- The focused test proves `stringSplit "aλbλc" "λ"` returns
  `Cons "a" (Cons "b" (Cons "c" Nil))`,
  `stringSplit "abc" "λ"` returns `Cons "abc" Nil`,
  `stringSplit "abc" ""` returns `Cons "abc" Nil`, and
  `stringSplit "λaλ" "λ"` returns `Cons "" (Cons "a" (Cons "" Nil))`
  through check, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution.
- The implementation adds a narrow public
  `stringSplit : String -> String -> List String` Prelude operation backed by
  `__string_split`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- Split semantics are left-to-right, non-overlapping, Unicode scalar boundary
  aware, preserve empty segments for non-empty delimiters, return singleton
  input for absent delimiters, and no-op to singleton input for empty
  delimiters.
- No regex, Unicode normalization, locale behavior, case conversion,
  parser-combinator work, parser parity, platform contract, compiler package,
  driver, proof, roadmap status, or controller state surface is added.
- Neighbor text/char tracers from earlier milestone-3 rounds still pass.
- Docs and changelog record only the first split-family tracer and do not claim
  split-family completion beyond this exact operation, complete cursor APIs,
  parser parity, platform contracts, self-boot proof, roadmap status, or
  milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `stringSplit : String -> String -> List String`
  operation across source checking, interpreter/runtime, backend emission,
  object generation, native execution, narrow docs, and changelog.
- Out of scope: split-on-character aliases, lines/words/trim/reverse APIs,
  regex, Unicode normalization, locale behavior, case conversion, broader
  collection helpers, broader `List String` APIs, source cursor API design,
  parser-owned combinator work, parser parity, platform ABI/FFI/GC contracts,
  compiler source package implementation, driver work, proof records, roadmap
  status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json`
keeps this as a single serial implementation round with no worker fan-out.
