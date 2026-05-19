### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-286-char-is-ascii-printable-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Add the next smallest native-capable broad `Char` classification tracer after `charIsAsciiPunctuation`: public Prelude `charIsAsciiPrintable : Char -> Bool` should classify the explicit ASCII printable range through source checking, `run-program`, backend LLVM emission, object-code validation, `emit-native`, native object validation, and linked native execution.

This advances the unfinished broader-classification and valid-text-boundary part of milestone-3 while staying narrower than Unicode printability categories, locale-sensitive classification, regex, parser combinators, full parser parity, `String`/`List Char` conversion, formatting, or complete cursor API design.

Round classification: behavior-changing implementation. This is not status-only and not semantic-roadmap-update. The implementer must use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the thesis gate because this changes public language/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Implementation ownership for this round is the single implementer in the canonical round-286 worktree. Expected write scope:

- Public behavior test and test helpers: `test/BackendLLVMSpec.hs`.
- Primitive inventory coverage: `test/PrimitiveInventorySpec.hs`.
- Primitive registry and native support classification: `src/MLF/Primitive/Inventory.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native LLVM lowering and runtime declarations/helpers: `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note: `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after implementation: `orchestrator/rounds/round-286/implementation-notes.md`.

First public-interface behavior:

> `.mlfp` source files importing Prelude `charIsAsciiPrintable` classify ASCII printable characters by returning `true` for `charIsAsciiPrintable ' '`, `charIsAsciiPrintable '!'`, `charIsAsciiPrintable 'A'`, `charIsAsciiPrintable '7'`, and `charIsAsciiPrintable '~'`, and returning `false` for `charIsAsciiPrintable '\t'`, `charIsAsciiPrintable '\n'`, and `charIsAsciiPrintable 'λ'`, with matching check, run-program, backend/object, emit-native/native-object, and linked native execution evidence.

Focused RED test to write first:

Add one Hspec example in `test/BackendLLVMSpec.hs` named `charIsAsciiPrintable classifies ASCII printable Char values through native execution`. Use the existing `withTempProgram`, `checkProgramFile`, `runProgramFile`, `emitBackendFile`, `emitNativeFile`, `validateLLVMAssembly`, `validateLLVMObjectCode`, and `runLLVMNativeExecutable` helpers. Run the focused matcher before production changes and record the RED failure in `implementation-notes.md`; the expected failure should be missing public Prelude/native-capable `charIsAsciiPrintable`, not a malformed test.

### Steps
1. Confirm `selection-record.json` lineage: active rev-003, `milestone-3`, and `direction-3a-broad-string-char-substrate`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and follow one vertical RED -> GREEN -> refactor cycle. Do not batch broad text tests before implementation.
3. Write only the RED public behavior test first, using these sources:

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable ' ';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable '!';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable 'A';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable '7';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable '~';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable '\t';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable '\n';
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (charIsAsciiPrintable);
  def main : Bool = charIsAsciiPrintable 'λ';
}
```

4. Prove RED with the focused command in Verification before production changes.
5. Implement only enough to pass:
   - add reserved primitive `__char_is_ascii_printable : Char -> Bool` and native support classification in `MLF.Primitive.Inventory`;
   - export `charIsAsciiPrintable : Char -> Bool` from the built-in Prelude;
   - add `run-program` behavior for exactly ASCII printable code points `0x20..0x7e`, with ASCII controls and non-ASCII scalars false;
   - add backend/native lowering over the existing Unicode scalar `Char` representation returning `Bool`;
   - preserve existing `Char`, `String`, slicing/search, punctuation, whitespace, identifier helper, alphanumeric helper, digit/lower/upper/alpha helpers, IO, and native rendering behavior.
6. Update only narrow docs/changelog surfaces listed in the expected write scope. Docs must say this is an explicit ASCII printable classifier and must not claim Unicode printability categories, locale/regex behavior, parser parity, full broader-classification completion, `String`/`List Char` conversion, formatting, or cursor API completion.
7. Re-run focused GREEN, inventory, neighbor, evidence, full build/test, and thesis-gate commands.
8. Record closeout evidence in `implementation-notes.md`: TDD skill path, RED/GREEN command results, files changed, native evidence by layer, docs updated, and scope boundaries preserved.

### Verification
Focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiPrintable classifies ASCII printable Char values through native execution"'
```

Required focused inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "charIsAsciiPunctuation classifies ASCII punctuation Char values through native execution" --match "charIsAsciiWhitespace classifies ASCII whitespace Char values through native execution" --match "charIsAsciiIdentifierContinue classifies ASCII identifier-continuation Char values through native execution" --match "charIsAsciiIdentifierStart classifies ASCII identifier-start Char values through native execution" --match "charIsAsciiAlphaNum classifies ASCII alphanumeric Char values through native execution" --match "charIsAsciiAlpha classifies ASCII alphabetic Char values through native execution" --match "charIsAsciiUpper classifies ASCII uppercase Char values through native execution" --match "charIsAsciiLower classifies ASCII lowercase Char values through native execution" --match "charIsDigit classifies decimal Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringEndsWith classifies Unicode suffixes through native execution" --match "stringStartsWith classifies Unicode prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required evidence checks:

```bash
rg -n 'charIsAsciiPrintable : Char -> Bool|__char_is_ascii_printable|charIsAsciiPrintablePrimitiveName|PrimitiveNativeCharIsAsciiPrintable|RuntimeCharIsAsciiPrintable' src test docs CONTEXT.md README.md CHANGELOG.md
```

```bash
rg -n -e 'charIsAsciiPrintable classifies ASCII printable Char values through native execution' -e "charIsAsciiPrintable ' '" -e "charIsAsciiPrintable '!'" -e "charIsAsciiPrintable 'A'" -e "charIsAsciiPrintable '7'" -e "charIsAsciiPrintable '~'" -e "charIsAsciiPrintable '\\t'" -e "charIsAsciiPrintable '\\n'" -e "charIsAsciiPrintable 'λ'" -e 'NativeRunResult ExitSuccess "true\\n"' -e 'NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|ASCII printable|ASCII helper|Char classification|charIsAsciiPrintable|charIsAsciiPunctuation|charIsAsciiWhitespace|charIsAsciiIdentifierContinue|charIsAsciiIdentifierStart|charIsAsciiAlphaNum|charIsAsciiAlpha|charIsAsciiUpper|charIsAsciiLower|charIsDigit|stringCharAt|stringSlice|stringTake|stringDrop|stringEndsWith|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|formatting|classification|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
- `selection-record.json` and `round-plan-record.json` lineage matches active rev-003 and `item-286-char-is-ascii-printable-native-tracer`.
- The focused test proves `charIsAsciiPrintable` returns `true` for space, `!`, `A`, `7`, and `~`, and `false` for tab, newline, and non-ASCII lambda, through check, run-program, backend LLVM/object validation, emit-native/native object validation, and linked native execution.
- `PrimitiveInventorySpec` confirms the new primitive is inventory-owned and native-lowerable.
- Neighbor text/char tracers from rounds 265-285 still pass.
- Docs and changelog record only the explicit ASCII printable classifier and do not claim Unicode printability categories, locale or regex behavior, full classification-family completion, `String`/`List Char` conversion, formatting, parser parity, platform contracts, self-boot proof, or milestone-3 completion.
- `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: one public `charIsAsciiPrintable : Char -> Bool` ASCII printable `Char` classifier across source checking, interpreter/runtime, backend emission, object generation, native execution, inventory, narrow docs, and changelog.
- Out of scope: Unicode printability/category semantics, locale, regex, broader classification-family completion, case conversion, formatting, `String`/`List Char` conversion, full slicing coverage, complete cursor API design, parser-owned combinator work, parser parity, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, roadmap status edits, controller state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json` keeps this as a single serial implementation round with no worker fan-out.
