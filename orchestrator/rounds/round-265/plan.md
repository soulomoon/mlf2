### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-265-char-literal-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the first native-capable Unicode scalar `Char` tracer for `.mlfp`: a single-quoted `Char` literal should source-check as `Char`, run through the interpreter/runtime value path, emit backend LLVM, validate as object code, and execute through the native process entrypoint with the same public value text as `run-program`.

This starts milestone-3 after the approved milestone-2 closeout without claiming the Broad String Library is complete.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

First public-interface behavior:

> A `.mlfp` source file with `def main : Char = 'λ';` is accepted by `check-program`, rendered by `run-program` as `'\955'\n`, emitted by `emit-backend` and `emit-native`, object-validated, and linked/native-executed with stdout `'\955'\n`.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `Char literal source checks, runs, emits backend, and executes natively`. Use the existing public CLI helpers and LLVM test support from that spec: write the source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before changing literal syntax or backend code, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should be the public source/backend path rejecting or not supporting the single-quoted `Char` literal, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` depends on completed `milestone-2`, and `direction-3a-broad-string-char-substrate` is the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - extend `test/BackendLLVMSpec.hs` imports to include `checkProgramFile` and `runProgramFile` from `MLF.Program.CLI`;
   - add the single Hspec example named `Char literal source checks, runs, emits backend, and executes natively`;
   - use this source:

```mlfp
module Main export (main) {
  def main : Char = 'λ';
}
```

   - assert `checkProgramFile path == Right "OK\n"`;
   - assert `runProgramFile path == Right "'\\955'\n"`;
   - assert `emitBackendFile path` succeeds, contains a `Main__main` backend function for the `Char` result, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "'\\955'\n" ""`.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add an explicit `LChar Char` literal form instead of treating `Char` as a one-character `String` or integer codepoint source alias;
   - parse single-quoted character literals with the existing Megaparsec character-literal machinery, preserving double-quoted strings as `String`;
   - map `LChar` to source/checker/runtime/backend type `Char`;
   - pretty/render `LChar` consistently with the existing public value-text style, so interpreter output and native output match for the selected Unicode scalar;
   - lower backend/native `Char` as a scalar value and add native result rendering for `Char`;
   - keep unsupported future text operations fail-closed rather than adding broad fallback behavior.
6. Update relevant docs without overclaiming:
   - `docs/syntax.md` and `docs/mlfp-language-reference.md` should mention single-quoted `Char` literals alongside `Int`, `Bool`, and `String`;
   - `README.md` and `docs/backend-native-pipeline.md` should include `Char` in the supported native result-rendering subset;
   - `docs/mlfp-self-boot-readiness.md` should record that the first `Char` literal tracer exists while broad string operations, `String`/`List Char`, slicing/classification, search, formatting, cursor APIs, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "Char literal source checks, runs, emits backend, and executes natively"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `Char` literal/native support. Run it again after implementation; it must pass.

Required evidence checks:

```bash
rg -n 'LChar|pChar|STBase "Char"|BaseTy "Char"' src test docs README.md CHANGELOG.md
```

```bash
rg -n "Char literal source checks, runs, emits backend, and executes natively|def main : Char = 'λ';|runProgramFile|emitNativeFile|NativeRunResult ExitSuccess" test/BackendLLVMSpec.hs
```

```bash
rg -n 'Char|single-quoted|native result|String/List Char|slicing|classification|parser parity' README.md docs/syntax.md docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

- Confirm the implementation models `Char` as an explicit Unicode scalar literal/type path, not as a one-character string alias or parser-private spelling.
- Confirm interpreter and native value text match for the selected non-ASCII scalar.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm docs say only the `Char` literal tracer landed; they must not claim broad string operations, `String`/`List Char` conversion, slicing, classification, search, formatting, parser cursors, parser parity, locale, regex, platform contracts, compiler package work, or self-boot proof completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `Char` literal tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: Broad String Library operations, non-ASCII `String` lowering, `String`/`List Char` conversion, slicing, classification, search, formatting, parser-owned cursor/combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades, compatibility parser aliases, or broad fallback layers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
