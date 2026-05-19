### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-301-string-equals-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable broad text comparison tracer after
`stringCharAtOption`: public Prelude
`stringEquals : String -> String -> Bool` compares two valid Unicode-scalar
strings for exact equality through source checking, `run-program`, backend
LLVM/object validation, `emit-native`/native-object validation, and linked
native execution.

This advances the milestone-3 comparison portion of the Broad String Library.
It does not claim ordering/collation, case conversion, Unicode normalization,
locale behavior, regex, `Eq String` typeclass instance support, parser
combinators, parser parity, platform contracts, compiler package work, driver
work, proof records, roadmap status, or milestone-3 completion.

Round classification: behavior-changing implementation. The implementer must
load and follow `/Users/ares/.agents/skills/tdd/SKILL.md` using a vertical
RED -> GREEN -> refactor cycle, and must run the thesis gate because this
changes public language/native behavior and self-boot readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringEquals : String -> String -> Bool = __string_equals;
```

Define equality as exact equality over valid Unicode scalar sequences. It is
not case-insensitive, locale-sensitive, normalization-aware, regex-like, or a
general collation API. It should agree between interpreter/runtime and native
execution for ASCII, non-ASCII, empty, and unequal-length examples.

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
  `orchestrator/rounds/round-301/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `stringEquals` can compare exact
> Unicode-scalar strings, including equal non-ASCII strings, unequal prefixes,
> and empty strings, with matching check, run-program, backend/object,
> emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringEquals compares Unicode scalar strings through native execution`. Use
the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The focused
example should include these public source fixtures:

```mlfp
module Main export (main) {
  import Prelude exposing (stringEquals);
  def main : Bool = stringEquals "aλ" "aλ";
}
```

Expected observable output: `true\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (stringEquals);
  def main : Bool = stringEquals "aλ" "a";
}
```

Expected observable output: `false\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (stringEquals);
  def main : Bool = stringEquals "" "";
}
```

Expected observable output: `true\n`.

Run the focused matcher before production changes and record the RED failure
in `implementation-notes.md`; the expected failure should be missing public
Prelude `stringEquals` or missing trusted primitive support, not a malformed
test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and the repo Haskell style
   guide at `/Users/ares/.agents/skills/haskell-pro/SKILL.md`. Follow one
   vertical RED -> GREEN -> refactor cycle. Do not batch comparison ordering,
   case conversion, normalization, locale behavior, regex, parser combinators,
   parser parity, platform, driver, or proof work before the focused public
   behavior.
3. Write only the RED public behavior test first using the source fixtures and
   expected outputs above. Keep the test public-interface oriented: check,
   run-program, backend LLVM/object validation, emit-native/native object
   validation, and linked native execution.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_equals : String -> String -> Bool` and the corresponding
     primitive inventory constructor/name;
   - export `stringEquals` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that compares exact Unicode scalar
     sequences and returns `true` only when both strings have the same scalar
     sequence;
   - add backend/native lowering with the same semantics, avoiding byte-index
     cursor shortcuts that would diverge from valid Unicode scalar behavior;
   - preserve existing `stringContains`, `stringStartsWith`, `stringEndsWith`,
     `stringIndexOfChar`, `stringIndexOf`, `stringReplace`,
     `stringReplaceChar`, `stringSplit`, `stringAppend`, `stringLength`,
     `stringCharAt`, `stringCharAtOption`, `stringToList`,
     `stringFromList`, ASCII classification, IO, and native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is the first exact native-capable string equality
   tracer and must not claim collation, ordering, `Eq String`, case conversion,
   Unicode normalization, locale behavior, regex, parser parity, platform
   contracts, driver work, proof records, roadmap status, or milestone-3
   completion.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence, claim-audit,
   generated-artifact, full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer,
   primitive inventory/native-lowerability evidence, docs updated, generated
   artifact cleanup, and scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringEquals compares Unicode scalar strings through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringIndexOf indexes Unicode scalar substrings through native execution" --match "stringSplit splits Unicode scalar substrings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "stringCharAtOption returns optional Unicode scalar cursor lookups through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'
```

Required evidence checks:

```bash
rg -n -e 'stringEquals : String -> String -> Bool' -e '__string_equals' -e 'stringEqualsPrimitiveName' -e 'PrimitiveNativeStringEquals' -e 'RuntimeStringEquals' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringEquals compares Unicode scalar strings through native execution' -e 'stringEquals "aλ" "aλ"' -e 'stringEquals "aλ" "a"' -e 'stringEquals "" ""' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringEquals|string equality|string comparison|collation|ordering|Eq String|case conversion|Unicode normalization|locale|regex|parser parity|platform contract|compiler package|driver|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
CARGO_TARGET_DIR=/tmp/round301-cargo-target cabal build all
```

```bash
CARGO_TARGET_DIR=/tmp/round301-cargo-target cabal test
```

```bash
CARGO_TARGET_DIR=/tmp/round301-cargo-target ./scripts/thesis-conformance-gate.sh
```

### Review Acceptance Criteria
- `selection-record.json` and `round-plan-record.json` lineage matches active
  rev-003 and `item-301-string-equals-native-tracer`.
- The focused public test proves `stringEquals "aλ" "aλ"` returns `true`,
  `stringEquals "aλ" "a"` returns `false`, and `stringEquals "" ""` returns
  `true` through check, run-program, backend LLVM/object validation,
  emit-native/native object validation, and linked native execution.
- The implementation adds a narrow public
  `stringEquals : String -> String -> Bool` Prelude operation backed by
  `__string_equals`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- Equality semantics are exact over valid Unicode scalar sequences. No
  collation, ordering, `Eq String` instance, case conversion, Unicode
  normalization, locale behavior, regex, parser combinator, parser parity,
  platform contract, compiler package, driver, proof, roadmap status, or
  controller state surface is added.
- Closeout evidence records RED -> GREEN -> refactor use of the TDD skill,
  focused and neighbor validation, broad `cabal build all` and `cabal test`,
  the thesis conformance gate, claim audit, and generated-artifact cleanup.
