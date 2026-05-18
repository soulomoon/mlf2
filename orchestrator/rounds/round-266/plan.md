### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-266-unicode-string-literal-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next native-capable Unicode scalar text tracer for `.mlfp`: a non-ASCII `String` literal should source-check as `String`, run through the interpreter/runtime value path, emit backend LLVM, validate as object code, and execute through the native process entrypoint with the same public value text as `run-program`.

This follows round-265's approved `Char` literal tracer. It advances milestone-3's `String` as Unicode-scalar sequence requirement without claiming the Broad String Library, `String`/`List Char` conversion, slicing, classification, search, formatting, parser cursors, or parser parity are complete.

### Approach
The implementer must load and use the TDD skill at `/Users/ares/.agents/skills/tdd/SKILL.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this public-interface behavior:

> A `.mlfp` source file with `def main : String = "λ";` is accepted by `check-program`, rendered by `run-program` as `"\\955"\n`, emitted by `emit-backend` and `emit-native`, object-validated, and linked/native-executed with stdout `"\\955"\n`.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `Unicode String literal source checks, runs, emits backend, and executes natively`. Use the existing public CLI helpers and LLVM test support from that spec: write the source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before changing backend/string code, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show the current backend/native path rejecting or mishandling the non-ASCII `String` literal, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is in progress after round-265, and `direction-3a-broad-string-char-substrate` is the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `Unicode String literal source checks, runs, emits backend, and executes natively`;
   - use this source:

```mlfp
module Main export (main) {
  def main : String = "λ";
}
```

   - assert `checkProgramFile path == Right "OK\n"`;
   - assert `runProgramFile path == Right "\"\\955\"\n"`;
   - assert `emitBackendFile path` succeeds, contains a `Main__main` backend function for the `String` result, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "\"\\955\"\n" ""`.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - remove the current ASCII-only backend rejection for `LString` values when the value is valid Unicode scalar text;
   - lower native string globals with byte-correct storage for the selected non-ASCII scalar rather than treating `length` or `ord` over Haskell `Char` as an emitted byte sequence;
   - keep the runtime string value as the source `String` value, not as pre-rendered display text such as a stored `\955` escape;
   - update native `String` result rendering so non-ASCII scalars are rendered with the same public value-text style as `run-program` for the selected literal;
   - preserve existing ASCII `String`, escaping, IO, and `Char` behavior;
   - keep unsupported future text operations fail-closed rather than adding broad fallback behavior.
6. Update relevant docs without overclaiming:
   - `docs/backend-native-pipeline.md` should mention that non-ASCII `String` literal native rendering has a first tracer;
   - `docs/mlfp-self-boot-readiness.md` should record that `Char` and a non-ASCII `String` literal tracer exist while broad string operations, `String`/`List Char`, slicing/classification, search, formatting, cursor APIs, and parser parity remain future work;
   - `docs/mlfp-language-reference.md` or `docs/syntax.md` should stay accurate about Unicode scalar `Char`/`String` literal behavior if the implementation changes any wording;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "Unicode String literal source checks, runs, emits backend, and executes natively"'
```

Run it once after adding the test and before implementation; it must fail for the missing public non-ASCII `String` literal backend/native support. Run it again after implementation; it must pass.

Required evidence checks:

```bash
rg -n 'BackendLLVMUnsupportedString|asciiString|stringByteLength|renderLLVMStringChar|lowerNativeStringRenderer|LString' src/MLF/Backend/LLVM test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode String literal source checks, runs, emits backend, and executes natively|def main : String = "λ";|runProgramFile|emitNativeFile|NativeRunResult ExitSuccess' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|String literal|String/List Char|slicing|classification|parser parity|non-ASCII' README.md docs/syntax.md docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
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

- Confirm the implementation models `String` literals as valid Unicode scalar text and does not store public display escapes as runtime data.
- Confirm interpreter and native value text match for the selected non-ASCII string literal.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing ASCII `String` native rendering and round-265 `Char` literal native rendering still pass.
- Confirm docs say only the non-ASCII `String` literal tracer landed; they must not claim broad string operations, `String`/`List Char` conversion, slicing, classification, search, formatting, parser cursors, parser parity, locale, regex, platform contracts, compiler package work, or self-boot proof completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public non-ASCII `String` literal tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: Broad String Library operations, `String`/`List Char` conversion, substring/slicing, classification, search, formatting, parser-owned cursor/combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades, compatibility parser aliases, lossy byte-indexed string semantics, or broad fallback layers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
