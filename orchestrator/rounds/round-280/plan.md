### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-280-char-is-ascii-alpha-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next smallest native-capable parser-relevant `Char` classification tracer: public Prelude `charIsAsciiAlpha : Char -> Bool` should classify ASCII alphabetic `Char` values through source checking, `run-program`, backend LLVM emission, object-code validation, `emit-native`, native object validation, and linked native execution.

This follows round-278 `charIsAsciiLower` and round-279 `charIsAsciiUpper`, and deliberately stays smaller than identifier-start, identifier-continuation, underscore/apostrophe, whitespace, parser combinator, or parser parity semantics.

Round classification: behavior-changing implementation. This is not status-only and not semantic-roadmap-update. The implementer must use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate because this changes public language/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Implementation ownership for this round is the single implementer in the canonical round-280 worktree. Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive inventory coverage: `test/PrimitiveInventorySpec.hs`.
- Primitive registry and native support classification: `src/MLF/Primitive/Inventory.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native LLVM lowering and runtime declarations/helpers: `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation: `orchestrator/rounds/round-280/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `charIsAsciiAlpha` classify ASCII alphabetic characters by returning `charIsAsciiAlpha 'a' == true`, `charIsAsciiAlpha 'A' == true`, `charIsAsciiAlpha '7' == false`, and `charIsAsciiAlpha 'λ' == false`, with matching source checking, run-program, backend/object, emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution`. Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`, `emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`, `validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. Run the focused matcher before production changes and record the RED failure in `implementation-notes.md`; the expected failure should be missing public Prelude/native-capable `charIsAsciiAlpha`, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`, and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED -> GREEN -> refactor cycle. Do not batch broad text tests before implementation.
3. Write only the RED public behavior test first, using these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiAlpha);
  def main : Bool = charIsAsciiAlpha 'a';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiAlpha);
  def main : Bool = charIsAsciiAlpha 'A';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiAlpha);
  def main : Bool = charIsAsciiAlpha '7';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiAlpha);
  def main : Bool = charIsAsciiAlpha 'λ';
}
```

4. Prove RED with the focused command in Verification before production changes.
5. Implement only enough to pass:
   - add reserved primitive `__char_is_ascii_alpha : Char -> Bool` and native support classification in `MLF.Primitive.Inventory`;
   - export `charIsAsciiAlpha : Char -> Bool` from the built-in Prelude;
   - add `run-program` behavior for ASCII uppercase/lowercase true and digit/non-ASCII false;
   - add backend/native lowering over the existing Unicode scalar `Char` representation returning `Bool`;
   - preserve existing `Char`, `String`, slicing/search, `charIsDigit`, `charIsAsciiLower`, `charIsAsciiUpper`, IO, and native rendering behavior.
6. Update only narrow docs/changelog surfaces listed in the expected write scope. Docs must say this is an explicit ASCII alphabetic classifier and must not claim parser parity or the full classification family.
7. Re-run focused GREEN, inventory, neighbor, evidence, full build/test, and thesis-gate commands.
8. Record closeout evidence in `implementation-notes.md`: TDD skill path, RED/GREEN command results, files changed, native evidence by layer, docs updated, and scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution"'
```

Required focused inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" --match "charIsAsciiLower classifies ASCII lowercase Char values through native execution" --match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'charIsAsciiAlpha : Char -> Bool|__char_is_ascii_alpha|charIsAsciiAlphaPrimitiveName|PrimitiveNativeCharIsAsciiAlpha|RuntimeCharIsAsciiAlpha' src test docs CONTEXT.md README.md CHANGELOG.md
```

```bash
rg -n 'charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution|charIsAsciiAlpha '\''a'\''|charIsAsciiAlpha '\''A'\''|charIsAsciiAlpha '\''7'\''|charIsAsciiAlpha '\''λ'\''|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|ASCII alphabetic|ASCII helper|Char classification|charIsAsciiAlpha|charIsAsciiUpper|charIsAsciiLower|charIsDigit|stringCharAt|stringSlice|stringTake|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|formatting|classification|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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

### Review Acceptance Criteria
- `selection-record.json` and `round-plan-record.json` lineage matches active rev-003 and `item-280-char-is-ascii-alpha-native-tracer`.
- The focused test proves `charIsAsciiAlpha 'a'` and `'A'` return `true`, while `'7'` and `'λ'` return `false`, through check, run-program, backend LLVM/object validation, emit-native/native object validation, and linked native execution.
- `PrimitiveInventorySpec` confirms the new primitive is inventory-owned and native-lowerable.
- Neighbor text/char tracers from rounds 265-279 still pass.
- Docs and changelog record only the explicit ASCII alphabetic classifier and do not claim identifier-start, identifier-continuation, whitespace, full Unicode categories, `String`/`List Char` conversion, formatting, parser parity, platform contracts, self-boot proof, or milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `charIsAsciiAlpha : Char -> Bool` ASCII alphabetic `Char` classifier across source checking, interpreter/runtime, backend emission, object generation, native execution, inventory, narrow docs, and changelog.
- Out of scope: identifier-start helper, identifier-continuation helper, underscore/apostrophe handling, whitespace, punctuation, Unicode category family breadth, case conversion, locale, regex, formatting, `String`/`List Char` conversion, complete cursor API design, parser-owned combinator work, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
