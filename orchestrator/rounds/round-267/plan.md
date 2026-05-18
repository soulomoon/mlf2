### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-267-unicode-string-length-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next native-capable broad text substrate tracer for `.mlfp`: a public Prelude `stringLength : String -> Int` operation should count Unicode scalar values, not UTF-8 bytes, through source checking, interpreter/runtime, backend LLVM emission, object-code validation, and linked native execution.

This follows round-265's `Char` literal tracer and round-266's non-ASCII `String` literal tracer. It advances milestone-3 from literal support into one observable Broad String Library operation without claiming `String`/`List Char` conversion, slicing, classification, search, formatting, cursor APIs, parser parity, platform contracts, or milestone-3 completion.

### Approach
The implementer must load and use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this public-interface behavior:

> A `.mlfp` source file importing Prelude `stringLength` with `def main : Int = stringLength "λa";` is accepted by `check-program`, rendered by `run-program` as `2\n`, emitted by `emit-backend` and `emit-native`, object-validated, and linked/native-executed with stdout `2\n`.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `Unicode stringLength source checks, runs, emits backend, and executes natively`. Use the existing public CLI helpers and LLVM test support from that spec: write the source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before adding the primitive, Prelude binding, interpreter behavior, or backend/native lowering, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show that `stringLength` is not yet a public checked/native-capable Prelude operation, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is dependency-ready and in progress after approved rounds 265 and 266, and `direction-3a-broad-string-char-substrate` is the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `Unicode stringLength source checks, runs, emits backend, and executes natively`;
   - use this source:

```mlfp
module Main export (main) {
  import Prelude exposing (stringLength);
  def main : Int = stringLength "λa";
}
```

   - assert `checkProgramFile path == Right "OK\n"`;
   - assert `runProgramFile path == Right "2\n"`;
   - assert `emitBackendFile path` succeeds, contains a `Main__main` backend function returning `i64`, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "2\n" ""`.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add a single primitive inventory entry for the reserved operation, such as `__string_length : String -> Int`, with native support classified in `MLF.Primitive.Inventory`;
   - export `stringLength` from the built-in Prelude and define `stringLength : String -> Int = __string_length;`;
   - extend `run-program` primitive evaluation so `stringLength "λa"` counts Haskell `Char` scalar values and returns `2`;
   - extend backend/native lowering so the primitive works for runtime `String` pointers and counts UTF-8 scalar starts rather than bytes;
   - preserve the runtime string as source text, not pre-rendered display text;
   - preserve existing ASCII `String`, non-ASCII `String`, `Char`, IO, and native result-rendering behavior;
   - keep unsupported future text operations fail-closed rather than adding broad fallback behavior.
6. Update relevant docs without overclaiming:
   - `docs/mlfp-language-reference.md` should list `stringLength : String -> Int` as the first broad string operation and state that it counts Unicode scalar values;
   - `README.md` or `docs/backend-native-pipeline.md` should mention the native-capable `stringLength` tracer if they describe native-supported text behavior;
   - `docs/mlfp-self-boot-readiness.md` should record that a first Unicode scalar `String` operation exists while `String`/`List Char` conversion, substring, search, formatting, slicing/classification, cursor APIs, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "Unicode stringLength source checks, runs, emits backend, and executes natively"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `stringLength` behavior. Run it again after implementation; it must pass.

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'stringLength|__string_length|PrimitiveNativeStringLength|RuntimeStringLength' src test docs README.md CHANGELOG.md
```

```bash
rg -n 'Unicode stringLength source checks, runs, emits backend, and executes natively|def main : Int = stringLength "λa";|runProgramFile|emitNativeFile|NativeRunResult ExitSuccess "2\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|String operation|stringLength|String/List Char|slicing|classification|search|formatting|parser parity' README.md docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

- Confirm `stringLength "λa"` returns `2`, proving Unicode scalar semantics rather than byte length.
- Confirm interpreter and native results match for the selected non-ASCII string literal.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing round-265 `Char` and round-266 non-ASCII `String` literal tracers still pass.
- Confirm docs say only the first `stringLength` tracer landed; they must not claim `String`/`List Char` conversion, substring, search, formatting, slicing/classification, cursor APIs, parser parity, locale, regex, platform contracts, compiler package work, self-boot proof completion, or milestone-3 completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `stringLength : String -> Int` Unicode-scalar operation tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: `String`/`List Char` conversion, substring/slicing APIs, classification predicates, search, formatting, parser-owned cursor/combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades beyond the Prelude operation and reserved primitive, compatibility parser aliases, byte-indexed string semantics, broad fallback layers, or parser-private text helpers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
