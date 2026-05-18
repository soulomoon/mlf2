### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-278-char-is-ascii-lower-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next native-capable parser-relevant `Char` classification tracer for `.mlfp`: a public Prelude `charIsAsciiLower : Char -> Bool` operation should classify ASCII lowercase identifier characters through source checking, interpreter/runtime, backend LLVM emission, object-code validation, and linked native execution.

This follows round-265's `Char` literal tracer, round-266's non-ASCII `String` literal tracer, round-267's `stringLength` Unicode-scalar counting tracer, round-268's `stringIsEmpty` classification tracer, round-269's `stringContainsChar` scalar search tracer, round-270's `stringContains` substring search tracer, round-271's `stringStartsWith` prefix search tracer, round-272's `stringEndsWith` suffix search tracer, round-273's `stringDrop` drop-slicing tracer, round-274's `stringTake` take-slicing tracer, round-275's `stringSlice` range-slicing tracer, round-276's `stringCharAt` cursor/index tracer, and round-277's `charIsDigit` decimal classifier. It advances milestone-3 with one explicitly named ASCII grammar helper needed by a future `.mlfp` parser, without claiming full Unicode classification, whitespace handling, `String`/`List Char` conversion, formatting, complete cursor APIs, parser parity, platform contracts, or milestone-3 completion.

Round classification: behavior-changing implementation. This is not a status-only round and not a semantic-roadmap-update round. The implementer must use the TDD skill path `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate because this touches language semantics plus backend/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Implementation ownership for this round is the single implementer in the canonical round-278 worktree. The owned write scope is:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive inventory coverage: `test/PrimitiveInventorySpec.hs`.
- Primitive registry and support classification: `src/MLF/Primitive/Inventory.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native LLVM lowering and runtime declarations/helpers: `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`.
- Claim-audit input, not an implementation write target for this round: `CONTEXT.md`.
- Round evidence only after implementation: `orchestrator/rounds/round-278/implementation-notes.md`.

Proceed behavior-first in one vertical RED -> GREEN -> refactor cycle from this first public-interface behavior:

> `.mlfp` source files importing Prelude `charIsAsciiLower` classify ASCII lowercase characters by returning `charIsAsciiLower 'a' == true`, `charIsAsciiLower 'A' == false`, and `charIsAsciiLower 'λ' == false`, and all programs pass `check-program`, `run-program`, `emit-backend`, object validation, `emit-native`, and linked native execution with matching public `Bool` output.

Focused RED test slice to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `charIsAsciiLower classifies ASCII lowercase Char values through native execution`. Use the existing public CLI helpers and LLVM test support from that spec: write each source through `withTempProgram`, call `checkProgramFile`, `runProgramFile`, `emitBackendFile`, and `emitNativeFile`, validate backend/native LLVM with `validateLLVMAssembly`, validate object generation with `validateLLVMObjectCode`, and run the emitted native LLVM with `runLLVMNativeExecutable`.

Before adding the primitive, Prelude binding, interpreter behavior, or backend/native lowering, run the focused matcher and record the RED failure in `implementation-notes.md`. The expected RED should show that `charIsAsciiLower` is not yet a public checked/native-capable Prelude operation, not an unwired test or compile error.

### Steps
1. Reconfirm the active rev-003 lineage and this selection record. `milestone-3` is dependency-ready and in progress after approved rounds 265 through 277, and `direction-3a-broad-string-char-substrate` remains the selected direction.
2. Load and use `/Users/ares/.agents/skills/tdd/SKILL.md`. Follow one vertical RED -> GREEN -> refactor cycle for this public behavior; do not batch broad text tests before implementation.
3. Write the RED test only:
   - add one Hspec example named `charIsAsciiLower classifies ASCII lowercase Char values through native execution`;
   - use these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiLower);
  def main : Bool = charIsAsciiLower 'a';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiLower);
  def main : Bool = charIsAsciiLower 'A';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiLower);
  def main : Bool = charIsAsciiLower 'λ';
}
```

   - assert each `checkProgramFile path == Right "OK\n"`;
   - assert `runProgramFile path == Right "true\n"` for `'a'` and `Right "false\n"` for `'A'` and `'λ'`;
   - assert each `emitBackendFile path` succeeds, contains a `Main__main` backend function returning `i1`, and passes `validateLLVMAssembly` plus `validateLLVMObjectCode`;
   - assert each `emitNativeFile path` succeeds, contains the native C ABI `main`, passes `validateLLVMAssembly` plus `validateLLVMObjectCode`, and linked execution returns `NativeRunResult ExitSuccess "true\n" ""` or `NativeRunResult ExitSuccess "false\n" ""` as appropriate.
4. Prove RED with the focused command in the Verification section before production changes.
5. Implement only enough to pass this behavior:
   - add one primitive inventory entry for the reserved operation, such as `__char_is_ascii_lower : Char -> Bool`, with native support classified in `MLF.Primitive.Inventory`;
   - export `charIsAsciiLower` from the built-in Prelude and define `charIsAsciiLower : Char -> Bool = __char_is_ascii_lower;`;
   - extend `run-program` primitive evaluation so `charIsAsciiLower 'a'` returns `true`, while `charIsAsciiLower 'A'` and `charIsAsciiLower 'λ'` return `false`;
   - extend backend/native lowering so the primitive accepts the existing `Char` scalar representation and returns `Bool` for ASCII lowercase code points `a` through `z`;
   - preserve Unicode scalar representation for `Char` and do not introduce byte-indexed or locale-sensitive public APIs;
   - preserve existing ASCII `String`, non-ASCII `String`, `Char`, `stringLength`, `stringIsEmpty`, `stringContainsChar`, `stringContains`, `stringStartsWith`, `stringEndsWith`, `stringDrop`, `stringTake`, `stringSlice`, `stringCharAt`, `charIsDigit`, IO, and native result-rendering behavior;
   - keep uppercase helpers, alphabetic aggregation, whitespace, punctuation, case conversion, locale, regex, formatting, `String`/`List Char` conversion, and parser combinator semantics out of this selected tracer.
6. Update relevant docs without overclaiming:
   - `docs/mlfp-language-reference.md` should list `charIsAsciiLower : Char -> Bool` as a public native-capable explicitly ASCII `Char` classification tracer and preserve the note that broader classification coverage remains incomplete;
   - `docs/backend-native-pipeline.md` should mention the native-capable `charIsAsciiLower` tracer if it lists native-supported text operations;
   - `docs/mlfp-self-boot-readiness.md` should record that one explicit ASCII lowercase `Char` classifier exists while `String`/`List Char` conversion, formatting, broader classification family coverage, complete cursor API semantics, and parser parity remain future work;
   - `CHANGELOG.md` should record this meaningful public language/native progress.
7. Re-run the focused matcher and keep it green. Refactor only while the focused test stays green.
8. Record reviewer-visible evidence in `implementation-notes.md`: loaded TDD skill path, RED failure command/result, GREEN command/result, files changed, focused behavior covered by layer, docs updated, scope boundaries preserved, and full closeout command results.

### Verification
Required focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiLower classifies ASCII lowercase Char values through native execution"'
```

Run it once after adding the test and before implementation; it must fail for the missing public `charIsAsciiLower` behavior. Run it again after implementation; it must pass.

Required focused inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'charIsAsciiLower : Char -> Bool|__char_is_ascii_lower|charIsAsciiLowerPrimitiveName|PrimitiveNativeCharIsAsciiLower|RuntimeCharIsAsciiLower' src test docs CONTEXT.md README.md CHANGELOG.md
```

```bash
rg -n 'charIsAsciiLower classifies ASCII lowercase Char values through native execution|charIsAsciiLower '\''a'\''|charIsAsciiLower '\''A'\''|charIsAsciiLower '\''λ'\''|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|ASCII lowercase|ASCII helper|Char classification|charIsAsciiLower|charIsDigit|stringCharAt|stringSlice|stringTake|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|formatting|classification|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
- Confirm `charIsAsciiLower 'a'` returns `true`, while `charIsAsciiLower 'A'` and `charIsAsciiLower 'λ'` return `false`, proving the selected explicit ASCII lowercase classifier over existing Unicode scalar `Char` values.
- Confirm interpreter and native results match for all selected programs.
- Confirm backend/native evidence includes assembly validation, object validation, and linked native execution.
- Confirm existing round-265 `Char`, round-266 non-ASCII `String`, round-267 `stringLength`, round-268 `stringIsEmpty`, round-269 `stringContainsChar`, round-270 `stringContains`, round-271 `stringStartsWith`, round-272 `stringEndsWith`, round-273 `stringDrop`, round-274 `stringTake`, round-275 `stringSlice`, round-276 `stringCharAt`, and round-277 `charIsDigit` tracers still pass.
- Confirm docs say only one explicit ASCII lowercase `Char` classifier landed; they must not claim full Unicode classification family coverage, uppercase/lowercase parser completion, `String`/`List Char` conversion, complete cursor semantics, formatting, parser parity, locale, regex, platform contracts, compiler package work, self-boot proof completion, or milestone-3 completion.
- Confirm no shared conformance expected output is regenerated dynamically and no milestone-2 corpus contract is weakened.

### Scope Boundaries
- In scope: one public `charIsAsciiLower : Char -> Bool` ASCII lowercase `Char` classification tracer across source checking, interpreter/runtime, backend emission, object generation, native execution, narrow docs, and changelog.
- Out of scope: uppercase helper, combined alphabetic helper, identifier-continuation helper, whitespace, punctuation, Unicode category family breadth, case conversion, locale, regex, formatting, `String`/`List Char` conversion, complete cursor API design, parser-owned combinator work, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out, new production public facades beyond the Prelude operation and reserved primitive, compatibility parser aliases, byte-indexed `Char` semantics, broad fallback layers, or parser-private text helpers.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
