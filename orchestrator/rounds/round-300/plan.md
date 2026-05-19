### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-300-string-char-at-option-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next bounded native-capable parser cursor tracer after `stringSplit`:
public Prelude `stringCharAtOption : String -> Int -> Option Char` should read
zero-based Unicode scalar cursor positions, return `Some` for in-range
characters, and return `None` at end-of-input or other out-of-range positions.

This closes only one safe cursor lookup in milestone-3. It does not claim full
cursor API completion, parser combinators, parser parity, split-family
completion, formatting completion, Unicode normalization, locale behavior,
regex, platform contracts, compiler package work, driver work, proof records,
or milestone-3 completion.

Round classification: behavior-changing implementation. The implementer must
load and follow `/Users/ares/.agents/skills/tdd/SKILL.md` using a vertical
RED -> GREEN -> refactor cycle, and must run the thesis gate because this
changes public language/native behavior and self-boot readiness claims.

### Approach
Keep the round serial. Do not use worker fan-out.

Add one trusted substrate primitive and one public Prelude wrapper:

```mlfp
def stringCharAtOption : String -> Int -> Option Char = __string_char_at_option;
```

Define the operation over Unicode scalar indexes, not UTF-8 byte positions.
Index `0` reads the first scalar, `1` reads the second scalar, and so on.
In-range lookups return `Some char`; end-of-input and out-of-range lookups
return `None`. If implementation paths already have a negative index case,
keep it fail-closed as `None`, but do not invent source syntax or arithmetic
scope just to test negative literals in this round.

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
  `orchestrator/rounds/round-300/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `Option(..)` and
> `stringCharAtOption` can safely inspect Unicode scalar cursor positions,
> returning `Some '\955'` for the lambda scalar in `"aλb"`, `Some 'b'` for
> index `2` in `"λab"`, and `None` for end-of-input/out-of-range indexes,
> with matching check, run-program, backend/object, emit-native/native-object,
> and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named
`stringCharAtOption returns optional Unicode scalar cursor lookups through native execution`.
Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`,
`emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`,
`validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. The focused
example should include these public source fixtures:

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringCharAtOption);
  def main : Option Char = stringCharAtOption "aλb" 1;
}
```

Expected observable output:
`Some '\955'\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringCharAtOption);
  def main : Option Char = stringCharAtOption "λab" 2;
}
```

Expected observable output:
`Some 'b'\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringCharAtOption);
  def main : Option Char = stringCharAtOption "λ" 1;
}
```

Expected observable output:
`None\n`.

```mlfp
module Main export (main) {
  import Prelude exposing (Option(..), stringCharAtOption);
  def main : Option Char = stringCharAtOption "" 0;
}
```

Expected observable output:
`None\n`.

Run the focused matcher before production changes and record the RED failure in
`implementation-notes.md`; the expected failure should be missing public
Prelude `stringCharAtOption` or missing trusted primitive support, not a
malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`,
   and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED
   -> GREEN -> refactor cycle. Do not batch parser combinators, broader cursor
   state APIs, split-family additions, formatting, platform, driver, or proof
   work before the focused public behavior.
3. Write only the RED public behavior test first using the source fixtures and
   expected outputs above. Keep the test public-interface oriented: check,
   run-program, backend LLVM/object validation, emit-native/native object
   validation, and linked native execution.
4. Prove RED with the focused command in Verification before production
   changes.
5. Implement only enough to pass:
   - add `__string_char_at_option : String -> Int -> Option Char` and the
     corresponding primitive inventory constructor/name;
   - export `stringCharAtOption` from the built-in Prelude module header;
   - add the public Prelude binding to the trusted primitive;
   - add interpreter/runtime behavior that walks by Unicode scalar values,
     returns `Some` for in-range positions, and returns `None` at
     end-of-input or other out-of-range positions;
   - add backend/native lowering with the same semantics, reusing the existing
     `Option` tag/layout construction pattern from `stringIndexOfChar` and
     `stringIndexOf` and the existing scalar cursor logic from `stringCharAt`;
   - preserve existing `stringCharAt`, `stringLength`, `stringSlice`,
     `stringDrop`, `stringTake`, `stringToList`, `stringFromList`,
     `stringIndexOfChar`, `stringIndexOf`, `stringSplit`, ASCII
     classification, IO, and native lowering behavior.
6. Update primitive inventory tests so the shared owner classifies the new
   primitive as native-lowerable.
7. Update only narrow docs/changelog surfaces listed in the expected write
   scope. Docs must say this is a safe optional cursor lookup tracer and must
   not claim full cursor APIs, parser combinators, parser parity, broader
   substring index APIs, split-family completion, formatting completion,
   Unicode normalization, locale behavior, regex, platform contracts, proof
   records, roadmap status, or milestone-3 completion.
8. Re-run focused GREEN, primitive inventory, neighbor, evidence, claim-audit,
   generated-artifact, full build/test, and thesis-gate commands.
9. Record closeout evidence in `implementation-notes.md`: TDD skill path,
   RED/GREEN command results, files changed, native evidence by layer, Option
   tag/layout evidence, cursor boundary semantics, primitive inventory/native
   lowerability evidence, docs updated, generated-artifact cleanup, and scope
   boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringCharAtOption returns optional Unicode scalar cursor lookups through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringIndexOfChar indexes Unicode scalar characters through native execution" --match "stringIndexOf indexes Unicode scalar substrings through native execution" --match "stringLength source checks, runs, emits backend, and executes natively" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "stringSplit splits Unicode scalar substrings through native execution" --match "prints nested first-order ADT values with ProgramSpec rendering"'
```

Required evidence checks:

```bash
rg -n -e 'stringCharAtOption : String -> Int -> Option Char' -e '__string_char_at_option' -e 'stringCharAtOptionPrimitiveName' -e 'PrimitiveNativeStringCharAtOption' -e 'RuntimeStringCharAtOption' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringCharAtOption returns optional Unicode scalar cursor lookups through native execution' -e 'stringCharAtOption "aλb" 1' -e 'stringCharAtOption "λab" 2' -e 'stringCharAtOption "λ" 1' -e 'stringCharAtOption "" 0' test/BackendLLVMSpec.hs
```

Required claim audit:

```bash
rg -n 'Unicode scalar|stringCharAtOption|Option Char|stringCharAt|cursor|end-of-input|out-of-range|parser combinator|parser parity|split-family|formatting|Unicode normalization|locale|regex|platform contract|compiler package|driver|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
  rev-003 and `item-300-string-char-at-option-native-tracer`.
- The focused test proves `stringCharAtOption "aλb" 1` returns `Some '\955'`,
  `stringCharAtOption "λab" 2` returns `Some 'b'`,
  `stringCharAtOption "λ" 1` returns `None`, and
  `stringCharAtOption "" 0` returns `None` through check, run-program,
  backend LLVM/object validation, emit-native/native object validation, and
  linked native execution.
- The implementation adds a narrow public
  `stringCharAtOption : String -> Int -> Option Char` Prelude operation backed
  by `__string_char_at_option`.
- The shared primitive inventory owns the primitive name/classification and
  classifies the new primitive as native-lowerable.
- Cursor semantics are zero-based over Unicode scalar values, not UTF-8 bytes;
  in-range positions return `Some Char`, and end-of-input/out-of-range
  positions return `None`.
- No parser-combinator work, parser parity, broader cursor state API,
  split-family API, formatting completion, Unicode normalization, locale
  behavior, regex, platform contract, compiler package, driver, proof, roadmap
  status, or controller state surface is added.
- Neighbor text/char tracers from earlier milestone-3 rounds still pass.
- Docs and changelog record only the safe optional cursor lookup tracer and do
  not claim full cursor APIs, parser parity, platform contracts, self-boot
  proof, roadmap status, or milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.
- Generated artifacts such as `runtime/mlfp_io/target/release/libmlfp_io.d`,
  `dist-newstyle/**`, `target/**`, `*.d`, `*.rlib`, or `Cargo.lock` do not
  remain in the final diff unless intentionally justified by the selected
  implementation.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json`
keeps this round serial with `worker_mode: "none"` because the selected
behavior crosses one public Prelude operation, one primitive inventory owner,
runtime dispatch, backend/native lowering, focused tests, and narrow docs; the
acceptance evidence depends on integrated behavior rather than disjoint worker
outputs.
