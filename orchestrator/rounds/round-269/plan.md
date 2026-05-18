### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-269-string-contains-char-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next native-capable broad text substrate tracer for `.mlfp`: a public Prelude `stringContainsChar : String -> Char -> Bool` operation should search a `String` by Unicode scalar `Char` through source checking, interpreter/runtime, backend LLVM emission, object-code validation, and linked native execution.

This follows round-265's `Char` literal tracer, round-266's non-ASCII `String` literal tracer, round-267's `stringLength` Unicode-scalar counting tracer, and round-268's `stringIsEmpty` classification tracer. It advances milestone-3 into one observable `String`/`Char` search operation without claiming `String`/`List Char` conversion, substring search, slicing, formatting, parser cursor APIs, parser parity, platform contracts, or milestone-3 completion.

### Approach
The implementer must load and use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this public-interface behavior:

> `.mlfp` source files importing Prelude `stringContainsChar` classify `stringContainsChar "aλb" 'λ'` as `true` and `stringContainsChar "ab" 'λ'` as `false`, and both programs pass `check-program`, `run-program`, `emit-backend`, object validation, `emit-native`, and linked native execution with matching public Bool output.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `stringContainsChar searches Unicode scalars through native execution`. Use the existing public CLI helpers and LLVM test support from that spec: write each source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before adding the primitive, Prelude binding, interpreter behavior, or backend/native lowering, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show that `stringContainsChar` is not yet a public checked/native-capable Prelude operation, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is dependency-ready and in progress after approved rounds 265, 266, 267, and 268, and `direction-3a-broad-string-char-substrate` remains the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `stringContainsChar searches Unicode scalars through native execution`;
   - use these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringContainsChar);
  def main : Bool = stringContainsChar "aλb" 'λ';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringContainsChar);
  def main : Bool = stringContainsChar "ab" 'λ';
}
```

   - assert each `checkProgramFile path == Right "OK\n"`;
   - assert the present program `runProgramFile path == Right "true\n"` and the absent program `runProgramFile path == Right "false\n"`;
   - assert each `emitBackendFile path` succeeds, contains a `Main__main` backend function returning `i1`, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert each `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "true\n" ""` or `NativeRunResult ExitSuccess "false\n" ""` respectively.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add one primitive inventory entry for the reserved operation, such as `__string_contains_char : String -> Char -> Bool`, with native support classified in `MLF.Primitive.Inventory`;
   - export `stringContainsChar` from the built-in Prelude and define `stringContainsChar : String -> Char -> Bool = __string_contains_char;`;
   - extend `run-program` primitive evaluation so `stringContainsChar "aλb" 'λ'` returns `true` and `stringContainsChar "ab" 'λ'` returns `false`;
   - extend backend/native lowering so the primitive accepts the current runtime `String` pointer plus the current `Char` scalar value and compares Unicode scalar values, not bytes, rendered characters, or escaped display text;
   - preserve the runtime string as source text, not pre-rendered display text;
   - preserve existing ASCII `String`, non-ASCII `String`, `Char`, `stringLength`, `stringIsEmpty`, IO, and native result-rendering behavior;
   - keep unsupported future text operations fail-closed rather than adding broad fallback behavior.
6. Update relevant docs without overclaiming:
   - `docs/mlfp-language-reference.md` should list `stringContainsChar : String -> Char -> Bool` as the first public `String`/`Char` search operation;
   - `docs/backend-native-pipeline.md` should mention the native-capable `stringContainsChar` tracer if it lists native-supported text operations;
   - `docs/mlfp-self-boot-readiness.md` should record that a first `String`/`Char` search operation exists while `String`/`List Char` conversion, substring search, slicing, formatting, parser cursor APIs, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringContainsChar searches Unicode scalars through native execution"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `stringContainsChar` behavior. Run it again after implementation; it must pass.

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringContainsChar|__string_contains_char|PrimitiveNativeStringContainsChar|RuntimeStringContainsChar' src test docs README.md CHANGELOG.md
```

```bash
rg -n 'stringContainsChar searches Unicode scalars through native execution|stringContainsChar "aλb"|stringContainsChar "ab"|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|String/Char search|stringContainsChar|stringIsEmpty|stringLength|String/List Char|substring search|slicing|formatting|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

- Confirm `stringContainsChar "aλb" 'λ'` returns `true` and `stringContainsChar "ab" 'λ'` returns `false`, proving both present and absent paths through valid Unicode scalar text.
- Confirm interpreter and native results match for both selected programs.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing round-265 `Char`, round-266 non-ASCII `String`, round-267 `stringLength`, and round-268 `stringIsEmpty` tracers still pass.
- Confirm docs say only the first `String`/`Char` search tracer landed; they must not claim `String`/`List Char` conversion, substring search, slicing, formatting, cursor APIs, parser parity, locale, regex, platform contracts, compiler package work, self-boot proof completion, or milestone-3 completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `stringContainsChar : String -> Char -> Bool` search tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: `String`/`List Char` conversion, substring search, slicing APIs, broader character classification predicates, formatting, parser-owned cursor/combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades beyond the Prelude operation and reserved primitive, compatibility parser aliases, byte-indexed string semantics, broad fallback layers, or parser-private text helpers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
