### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-271-string-starts-with-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next native-capable broad text substrate tracer for `.mlfp`: a public Prelude `stringStartsWith : String -> String -> Bool` operation should classify a non-empty Unicode scalar prefix through source checking, interpreter/runtime, backend LLVM emission, object-code validation, and linked native execution.

This follows round-265's `Char` literal tracer, round-266's non-ASCII `String` literal tracer, round-267's `stringLength` Unicode-scalar counting tracer, round-268's `stringIsEmpty` classification tracer, round-269's `stringContainsChar` scalar search tracer, and round-270's `stringContains` substring search tracer. It advances milestone-3 with one observable prefix-search operation useful to future parser cursor work without claiming `String`/`List Char` conversion, slicing, formatting, broader classification predicates, full cursor APIs, parser parity, platform contracts, or milestone-3 completion.

### Approach
The implementer must load and use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this first public-interface behavior:

> `.mlfp` source files importing Prelude `stringStartsWith` classify `stringStartsWith "λab" "λ"` as `true` and `stringStartsWith "aλb" "λ"` as `false`, and both programs pass `check-program`, `run-program`, `emit-backend`, object validation, `emit-native`, and linked native execution with matching public Bool output.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `stringStartsWith classifies Unicode prefixes through native execution`. Use the existing public CLI helpers and LLVM test support from that spec: write each source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before adding the primitive, Prelude binding, interpreter behavior, or backend/native lowering, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show that `stringStartsWith` is not yet a public checked/native-capable Prelude operation, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is dependency-ready and in progress after approved rounds 265, 266, 267, 268, 269, and 270, and `direction-3a-broad-string-char-substrate` remains the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `stringStartsWith classifies Unicode prefixes through native execution`;
   - use these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringStartsWith);
  def main : Bool = stringStartsWith "λab" "λ";
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringStartsWith);
  def main : Bool = stringStartsWith "aλb" "λ";
}
```

   - assert each `checkProgramFile path == Right "OK\n"`;
   - assert the present-prefix program `runProgramFile path == Right "true\n"` and the absent-prefix program `runProgramFile path == Right "false\n"`;
   - assert each `emitBackendFile path` succeeds, contains a `Main__main` backend function returning `i1`, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert each `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "true\n" ""` or `NativeRunResult ExitSuccess "false\n" ""` respectively.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add one primitive inventory entry for the reserved operation, such as `__string_starts_with : String -> String -> Bool`, with native support classified in `MLF.Primitive.Inventory`;
   - export `stringStartsWith` from the built-in Prelude and define `stringStartsWith : String -> String -> Bool = __string_starts_with;`;
   - extend `run-program` primitive evaluation so `stringStartsWith "λab" "λ"` returns `true` and `stringStartsWith "aλb" "λ"` returns `false`;
   - extend backend/native lowering so the primitive accepts current runtime `String` pointers for haystack and prefix and compares valid UTF-8 scalar-sequence bytes from the start of the haystack, not escaped rendering or display text;
   - preserve Unicode scalar semantics and do not introduce byte-indexed public APIs;
   - preserve existing ASCII `String`, non-ASCII `String`, `Char`, `stringLength`, `stringIsEmpty`, `stringContainsChar`, `stringContains`, IO, and native result-rendering behavior;
   - keep unsupported future text operations fail-closed rather than adding broad fallback behavior.
6. Update relevant docs without overclaiming:
   - `docs/mlfp-language-reference.md` should list `stringStartsWith : String -> String -> Bool` as the first public non-empty prefix-search tracer;
   - `docs/backend-native-pipeline.md` should mention the native-capable `stringStartsWith` tracer if it lists native-supported text operations;
   - `docs/mlfp-self-boot-readiness.md` should record that a first prefix-search operation exists while `String`/`List Char` conversion, slicing, formatting, broader classification predicates, full cursor APIs, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringStartsWith classifies Unicode prefixes through native execution"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `stringStartsWith` behavior. Run it again after implementation; it must pass.

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringStartsWith : String -> String -> Bool|__string_starts_with|stringStartsWithPrimitiveName|PrimitiveNativeStringStartsWith|RuntimeStringStartsWith' src test docs README.md CHANGELOG.md
```

```bash
rg -n 'stringStartsWith classifies Unicode prefixes through native execution|stringStartsWith "λab"|stringStartsWith "aλb"|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|prefix search|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|slicing|formatting|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

- Confirm `stringStartsWith "λab" "λ"` returns `true` and `stringStartsWith "aλb" "λ"` returns `false`, proving present and absent non-empty Unicode prefix paths.
- Confirm interpreter and native results match for both selected programs.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing round-265 `Char`, round-266 non-ASCII `String`, round-267 `stringLength`, round-268 `stringIsEmpty`, round-269 `stringContainsChar`, and round-270 `stringContains` tracers still pass.
- Confirm docs say only the first non-empty prefix-search tracer landed; they must not claim `String`/`List Char` conversion, slicing, formatting, full cursor APIs, parser parity, locale, regex, platform contracts, compiler package work, self-boot proof completion, or milestone-3 completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `stringStartsWith : String -> String -> Bool` non-empty prefix-search tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: `String`/`List Char` conversion, slicing APIs, broader prefix/search semantics beyond the selected non-empty prefix tracer, broader character classification predicates, formatting, full cursor APIs, parser-owned combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades beyond the Prelude operation and reserved primitive, compatibility parser aliases, byte-indexed string semantics, broad fallback layers, or parser-private text helpers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
