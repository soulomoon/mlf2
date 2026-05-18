### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-270-string-contains-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next native-capable broad text substrate tracer for `.mlfp`: a public Prelude `stringContains : String -> String -> Bool` operation should search for a non-empty Unicode substring through source checking, interpreter/runtime, backend LLVM emission, object-code validation, and linked native execution.

This follows round-265's `Char` literal tracer, round-266's non-ASCII `String` literal tracer, round-267's `stringLength` Unicode-scalar counting tracer, round-268's `stringIsEmpty` classification tracer, and round-269's `stringContainsChar` single-character search tracer. It advances milestone-3 into one observable substring-search operation without claiming `String`/`List Char` conversion, slicing, formatting, parser cursor APIs, parser parity, platform contracts, or milestone-3 completion.

### Approach
The implementer must load and use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this first public-interface behavior:

> `.mlfp` source files importing Prelude `stringContains` classify `stringContains "aλb" "λ"` as `true` and `stringContains "ab" "λ"` as `false`, and both programs pass `check-program`, `run-program`, `emit-backend`, object validation, `emit-native`, and linked native execution with matching public Bool output.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `stringContains searches Unicode substrings through native execution`. Use the existing public CLI helpers and LLVM test support from that spec: write each source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before adding the primitive, Prelude binding, interpreter behavior, or backend/native lowering, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show that `stringContains` is not yet a public checked/native-capable Prelude operation, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is dependency-ready and in progress after approved rounds 265, 266, 267, 268, and 269, and `direction-3a-broad-string-char-substrate` remains the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `stringContains searches Unicode substrings through native execution`;
   - use these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (stringContains);
  def main : Bool = stringContains "aλb" "λ";
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (stringContains);
  def main : Bool = stringContains "ab" "λ";
}
```

   - assert each `checkProgramFile path == Right "OK\n"`;
   - assert the present-substring program `runProgramFile path == Right "true\n"` and the absent-substring program `runProgramFile path == Right "false\n"`;
   - assert each `emitBackendFile path` succeeds, contains a `Main__main` backend function returning `i1`, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert each `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "true\n" ""` or `NativeRunResult ExitSuccess "false\n" ""` respectively.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add one primitive inventory entry for the reserved operation, such as `__string_contains : String -> String -> Bool`, with native support classified in `MLF.Primitive.Inventory`;
   - export `stringContains` from the built-in Prelude and define `stringContains : String -> String -> Bool = __string_contains;`;
   - extend `run-program` primitive evaluation so `stringContains "aλb" "λ"` returns `true` and `stringContains "ab" "λ"` returns `false`;
   - extend backend/native lowering so the primitive accepts current runtime `String` pointers for haystack and needle and compares valid Unicode scalar-sequence slices, not display text or escaped rendering;
   - ensure native matching starts only at valid scalar boundaries and does not introduce a byte-indexed public API;
   - preserve the runtime string as source text, not pre-rendered display text;
   - preserve existing ASCII `String`, non-ASCII `String`, `Char`, `stringLength`, `stringIsEmpty`, `stringContainsChar`, IO, and native result-rendering behavior;
   - keep unsupported future text operations fail-closed rather than adding broad fallback behavior.
6. Update relevant docs without overclaiming:
   - `docs/mlfp-language-reference.md` should list `stringContains : String -> String -> Bool` as the first public non-empty substring-search operation;
   - `docs/backend-native-pipeline.md` should mention the native-capable `stringContains` tracer if it lists native-supported text operations;
   - `docs/mlfp-self-boot-readiness.md` should record that a first substring-search operation exists while `String`/`List Char` conversion, slicing, formatting, parser cursor APIs, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `stringContains` behavior. Run it again after implementation; it must pass.

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringContains : String -> String -> Bool|__string_contains"|stringContainsPrimitiveName|PrimitiveNativeStringContains|RuntimeStringContains' src test docs README.md CHANGELOG.md
```

```bash
rg -n 'stringContains searches Unicode substrings through native execution|stringContains "aλb"|stringContains "ab"|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|substring search|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|slicing|formatting|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

- Confirm `stringContains "aλb" "λ"` returns `true` and `stringContains "ab" "λ"` returns `false`, proving present and absent non-empty substring paths through valid Unicode scalar text.
- Confirm interpreter and native results match for both selected programs.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing round-265 `Char`, round-266 non-ASCII `String`, round-267 `stringLength`, round-268 `stringIsEmpty`, and round-269 `stringContainsChar` tracers still pass.
- Confirm docs say only the first non-empty substring-search tracer landed; they must not claim `String`/`List Char` conversion, slicing, formatting, cursor APIs, parser parity, locale, regex, platform contracts, compiler package work, self-boot proof completion, or milestone-3 completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `stringContains : String -> String -> Bool` non-empty substring-search tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: `String`/`List Char` conversion, slicing APIs, broader search semantics beyond the selected non-empty substring tracer, broader character classification predicates, formatting, parser-owned cursor/combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades beyond the Prelude operation and reserved primitive, compatibility parser aliases, byte-indexed string semantics, broad fallback layers, or parser-private text helpers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
