### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-273-string-drop-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the first native-capable broad text slicing tracer for `.mlfp`: a public Prelude `stringDrop : String -> Int -> String` operation should drop a non-negative count of Unicode scalar values and return the remaining `String` through source checking, interpreter/runtime, backend LLVM emission, object-code validation, and linked native execution.

This follows round-265's `Char` literal tracer, round-266's non-ASCII `String` literal tracer, round-267's `stringLength` Unicode-scalar counting tracer, round-268's `stringIsEmpty` classification tracer, round-269's `stringContainsChar` scalar search tracer, round-270's `stringContains` substring search tracer, round-271's `stringStartsWith` prefix search tracer, and round-272's `stringEndsWith` suffix search tracer. It advances milestone-3 with one observable slicing operation without claiming `String`/`List Char` conversion, full slicing API completion, formatting, broader classification predicates, full cursor APIs, parser parity, platform contracts, or milestone-3 completion.

Round classification: behavior-changing implementation. This is not a status-only round and not a semantic-roadmap-update round. The implementer must use the TDD skill path `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate because this touches language semantics plus backend/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Implementation ownership for this round is the single implementer in the canonical round-273 worktree. The owned write scope is:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive inventory coverage: `test/PrimitiveInventorySpec.hs`.
- Primitive registry and support classification: `src/MLF/Primitive/Inventory.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native LLVM lowering and runtime declarations/helpers: `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`.
- Round evidence only after implementation: `orchestrator/rounds/round-273/implementation-notes.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this first public-interface behavior:

> `.mlfp` source files importing Prelude `stringDrop` classify Unicode scalar drop counts by returning `stringDrop "λab" 1 == "ab"` and `stringDrop "aλb" 2 == "b"`, and both programs pass `check-program`, `run-program`, `emit-backend`, object validation, `emit-native`, and linked native execution with matching public `String` output.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `stringDrop slices Unicode scalar prefixes through native execution`. Use the existing public CLI helpers and LLVM test support from that spec: write each source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before adding the primitive, Prelude binding, interpreter behavior, or backend/native lowering, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show that `stringDrop` is not yet a public checked/native-capable Prelude operation, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is dependency-ready and in progress after approved rounds 265 through 272, and `direction-3a-broad-string-char-substrate` remains the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `stringDrop slices Unicode scalar prefixes through native execution`;
   - use these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringDrop);
  def main : String = stringDrop "λab" 1;
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringDrop);
  def main : String = stringDrop "aλb" 2;
}
```

   - assert each `checkProgramFile path == Right "OK\n"`;
   - assert the first program `runProgramFile path == Right "\"ab\"\n"` and the second program `runProgramFile path == Right "\"b\"\n"`;
   - assert each `emitBackendFile path` succeeds, contains a `Main__main` backend function returning `ptr`, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert each `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "\"ab\"\n" ""` or `NativeRunResult ExitSuccess "\"b\"\n" ""` respectively.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add one primitive inventory entry for the reserved operation, such as `__string_drop : String -> Int -> String`, with native support classified in `MLF.Primitive.Inventory`;
   - export `stringDrop` from the built-in Prelude and define `stringDrop : String -> Int -> String = __string_drop;`;
   - extend `run-program` primitive evaluation so `stringDrop "λab" 1` returns `"ab"` and `stringDrop "aλb" 2` returns `"b"`;
   - extend backend/native lowering so the primitive accepts the current runtime `String` pointer plus an `Int` count and advances by valid Unicode scalar boundaries, not bytes, escaped rendering, or display text;
   - preserve Unicode scalar semantics and do not introduce byte-indexed public APIs;
   - preserve existing ASCII `String`, non-ASCII `String`, `Char`, `stringLength`, `stringIsEmpty`, `stringContainsChar`, `stringContains`, `stringStartsWith`, `stringEndsWith`, IO, and native result-rendering behavior;
   - keep negative, out-of-range, `String`/`List Char` conversion, formatting, broader slicing, and cursor semantics out of this selected tracer unless the implementation must fail closed for correctness.
6. Update relevant docs without overclaiming:
   - `docs/mlfp-language-reference.md` should list `stringDrop : String -> Int -> String` as the first public non-negative Unicode-scalar drop slicing tracer;
   - `docs/backend-native-pipeline.md` should mention the native-capable `stringDrop` tracer if it lists native-supported text operations;
   - `docs/mlfp-self-boot-readiness.md` should record that a first drop-based slicing operation exists while `String`/`List Char` conversion, full slicing coverage, formatting, broader classification predicates, full cursor APIs, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringDrop slices Unicode scalar prefixes through native execution"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `stringDrop` behavior. Run it again after implementation; it must pass.

Required focused inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringDrop : String -> Int -> String|__string_drop|stringDropPrimitiveName|PrimitiveNativeStringDrop|RuntimeStringDrop' src test docs README.md CHANGELOG.md
```

```bash
rg -n 'stringDrop slices Unicode scalar prefixes through native execution|stringDrop "λab" 1|stringDrop "aλb" 2|NativeRunResult ExitSuccess "\\\"ab\\\"\\n"|NativeRunResult ExitSuccess "\\\"b\\\"\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|drop slicing|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|full slicing|formatting|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

Manual checks:

- Confirm this is a behavior-changing round, not status-only and not semantic-roadmap-update.
- Confirm `stringDrop "λab" 1` returns `"ab"` and `stringDrop "aλb" 2` returns `"b"`, proving non-negative Unicode scalar drop behavior over non-ASCII input.
- Confirm interpreter and native results match for both selected programs.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing round-265 `Char`, round-266 non-ASCII `String`, round-267 `stringLength`, round-268 `stringIsEmpty`, round-269 `stringContainsChar`, round-270 `stringContains`, round-271 `stringStartsWith`, and round-272 `stringEndsWith` tracers still pass.
- Confirm docs say only the first drop-based slicing tracer landed; they must not claim `String`/`List Char` conversion, complete slicing, formatting, full cursor APIs, parser parity, locale, regex, platform contracts, compiler package work, self-boot proof completion, or milestone-3 completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `stringDrop : String -> Int -> String` non-negative Unicode-scalar drop slicing tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: `String`/`List Char` conversion, `stringTake`, arbitrary range slicing, negative or out-of-range semantics beyond fail-closed correctness if required, formatting, broader character classification predicates, full cursor APIs, parser-owned combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades beyond the Prelude operation and reserved primitive, compatibility parser aliases, byte-indexed string semantics, broad fallback layers, or parser-private text helpers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
