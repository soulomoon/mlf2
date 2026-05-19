### Selected Extraction
- Milestone: Native-Capable Broad Text Substrate
- Milestone id: `milestone-3`
- Direction id: `direction-3a-broad-string-char-substrate`
- Extracted item id: `item-290-string-to-list-native-tracer`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-003`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003`

### Goal
Recover the selected `stringToList` round without changing roadmap lineage.
The round still delivers public Prelude
`stringToList : String -> List Char` as a native-capable broad text tracer, but
the current implementation evidence shows a prerequisite source-checking /
elaboration blocker:

```text
PhiInvariantError "PhiReorder: missing binder identity at positions [1]"
```

The recovery target is therefore two ordered public-interface behaviors:

1. Public `check-program` accepts a named top-level function application whose
   result type is `List Char`.
2. Public Prelude `stringToList : String -> List Char` then converts strings
   into ordinary Unicode scalar `List Char` values through source checking,
   `run-program`, backend LLVM emission, object-code validation,
   `emit-native`, native object validation, and linked native execution.

This remains within milestone-3 / `direction-3a-broad-string-char-substrate`
because the selected `String -> List Char` behavior cannot even reach
runtime/backend/native evidence until the source-checking layer accepts named
applications returning `List Char`. It does not resequence parser parity,
platform contracts, compiler package work, driver work, or proof records.

Round classification: behavior-changing recovery implementation. The
implementer must use `/Users/ares/.agents/skills/tdd/SKILL.md` and must run the
thesis gate because this changes source checking / elaboration and
language/native behavior.

### Approach
Keep the round serial. Do not use worker fan-out.

Preserve the existing lineage and current implementation evidence:

- `selection-record.json` still names
  `item-290-string-to-list-native-tracer`.
- Existing `implementation-notes.md` records the initial RED
  (`Prelude` missing `stringToList`), the attempted implementation, and the
  current source-checking blocker.
- Existing source/test edits are current work-in-progress evidence. Work with
  them; do not revert them merely to restart the round.

First fix the checker/elaboration prerequisite through a public regression.
The regression must not be a private invariant-only test and must not special
case `stringToList`, `__string_to_list`, or `List Char`. The implementation
should repair the missing binder identity propagation/root-cause path that
causes Φ reordering to fail for named applications returning a parameterized
ADT result.

Likely checker/elaboration write scope for the prerequisite fix:

- Public regression: `test/BackendLLVMSpec.hs` or another existing
  public-interface spec that already drives `checkProgramFile`.
- Narrow lower-level regression only if useful after the public RED:
  `test/PipelineSpec.hs` or `test/ElaborationSpec.hs`.
- Source-checking / elaboration owners as diagnosis requires:
  `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs`,
  `src/MLF/Elab/Run/Generalize/Prepare/Internal.hs`,
  `src/MLF/Elab/Run/Generalize/Phase1.hs`,
  `src/MLF/Elab/Run/Generalize/Phase2.hs`,
  `src/MLF/Constraint/Presolution/Plan/*`, and/or `src/MLF/Reify/*`.

Continue the original `stringToList` write scope only after the prerequisite
public regression is GREEN:

- Public behavior test and helpers: `test/BackendLLVMSpec.hs`.
- Primitive ownership and inventory coverage:
  `src/MLF/Primitive/Inventory.hs` and `test/PrimitiveInventorySpec.hs`.
- Built-in Prelude public surface: `src/MLF/Frontend/Program/Prelude.hs`.
- Interpreter/runtime primitive behavior: `src/MLF/Frontend/Program/Run.hs`.
- Backend/native lowering for the new primitive:
  `src/MLF/Backend/LLVM/Lower.hs`.
- Narrow support docs and progress note only after the end-to-end tracer is
  proven: `docs/mlfp-language-reference.md`,
  `docs/backend-native-pipeline.md`, `docs/mlfp-self-boot-readiness.md`, and
  `CHANGELOG.md`.
- Claim-audit input only, not an expected write target: `CONTEXT.md`.
- Round evidence after recovery: update
  `orchestrator/rounds/round-290/implementation-notes.md`.

First public-interface behavior for recovery:

> A `.mlfp` source file checked through the public `check-program` path can
> define `f : String -> List Char` and `main : List Char = f "aλ"` without
> failing Phase 6 elaboration with
> `PhiReorder: missing binder identity at positions [1]`.

Focused RED test to write first:

Add or keep one Hspec example named
`named functions returning List Char source check without PhiReorder binder identity failure`.
It should use `checkProgramFile` through a `.mlfp` source shaped like:

```mlfp
module Main export (main) {
  import Prelude exposing (List(..));
  def f : String -> List Char = λ(value : String) Nil;
  def main : List Char = f "aλ";
}
```

The initial RED must reproduce the current source-checking failure from
`implementation-notes.md`. After this regression is GREEN, resume the original
focused `stringToList` behavior:

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringToList);
  def main : List Char = stringToList "aλ";
}
```

```mlfp
module Main export (main) {
  import Prelude exposing (List(..), stringToList);
  def main : List Char = stringToList "";
}
```

### Steps
1. Confirm lineage remains active rev-003, `milestone-3`,
   `direction-3a-broad-string-char-substrate`, and
   `item-290-string-to-list-native-tracer`.
2. Load `/Users/ares/.agents/skills/tdd/SKILL.md` and continue as a vertical
   TDD recovery. Do not batch unrelated checker, primitive, backend, or docs
   changes ahead of the next public RED/GREEN slice.
3. Add the prerequisite public source-check regression named
   `named functions returning List Char source check without PhiReorder binder identity failure`.
   Prove RED with the focused command in Verification. The RED should match
   the current `PhiReorder` failure, not a malformed test or missing import.
4. Fix only the checker/elaboration root cause needed for that public
   regression. Do not suppress the invariant, special-case `List Char`,
   special-case `stringToList`, or add parser/platform/proof behavior.
5. Prove the prerequisite regression GREEN. If an internal regression is added
   to pin the exact binder-identity propagation fix, run it immediately after
   the public GREEN and record why it is a guard rather than the primary
   behavior test.
6. Re-run the original `stringToList converts Unicode scalar strings to List Char values through native execution`
   focused test. If source checking now passes but runtime/backend/native still
   fails, finish only the already-selected `stringToList` primitive/runtime/
   native path.
7. Update narrow docs/changelog only after `stringToList` is proven through all
   required layers. Docs must say this is the first native-capable
   `String -> List Char` conversion tracer and must not claim formatting
   completion, full slicing coverage, complete cursor APIs, parser parity,
   platform contracts, milestone-3 completion, or self-boot proof.
8. Re-run prerequisite, stringToList, primitive inventory, neighbor, evidence,
   full build/test, and thesis-gate commands.
9. Update `implementation-notes.md` with recovery evidence: TDD skill path,
   prerequisite RED/GREEN command results, root-cause files changed,
   stringToList RED/GREEN/native evidence, docs updated, and scope boundaries
   preserved.

### Verification
Prerequisite focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "named functions returning List Char source check without PhiReorder binder identity failure"'
```

Original `stringToList` focused RED/GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringToList converts Unicode scalar strings to List Char values through native execution"'
```

Required primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Required focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "stringFromList converts List Char values to Unicode scalar strings through native execution" --match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Required recovery evidence checks:

```bash
rg -n -e 'named functions returning List Char source check without PhiReorder binder identity failure' -e 'def f : String -> List Char' test
```

```bash
rg -n -e 'PhiReorder: missing binder identity' -e 'binder identity' src test orchestrator/rounds/round-290/implementation-notes.md
```

Required `stringToList` evidence checks:

```bash
rg -n -e 'stringToList : String -> List Char' -e '__string_to_list' -e 'stringToListPrimitiveName' -e 'PrimitiveNativeStringToList' -e 'RuntimeStringToList' src test docs CHANGELOG.md
```

```bash
rg -n -e 'stringToList converts Unicode scalar strings to List Char values through native execution' -e 'stringToList "aλ"' -e 'stringToList ""' -e "Cons 'a'" -e 'Nil' test/BackendLLVMSpec.hs
```

```bash
rg -n 'Unicode scalar|stringToList|String/List Char|String -> List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
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
- `selection-record.json` and `round-plan-record.json` preserve active rev-003
  lineage and `item-290-string-to-list-native-tracer`.
- The prerequisite public regression proves a named application returning
  `List Char` checks through the public `check-program` path without
  `PhiReorder: missing binder identity at positions [1]`.
- The checker/elaboration fix is root-cause oriented and does not special-case
  `stringToList`, `__string_to_list`, `List Char`, or the test fixture.
- The focused `stringToList` test proves `stringToList "aλ"` returns
  `Cons 'a' (Cons '\955' Nil)` and `stringToList ""` returns `Nil`, through
  check, run-program, backend LLVM/object validation, emit-native/native object
  validation, and linked native execution.
- The primitive inventory, run-program runtime, and backend/native lowering
  agree on `__string_to_list`; Unicode scalar order is preserved.
- Neighbor text/char tracers from rounds 265-289 still pass.
- Docs and changelog record only the explicit native-capable
  `String -> List Char` conversion tracer and do not claim formatting
  completion, full slicing coverage, complete cursor APIs, parser parity,
  platform contracts, self-boot proof, roadmap status, or milestone-3
  completion.
- `git diff --check`, `cabal build all`, `cabal test`, and
  `./scripts/thesis-conformance-gate.sh` pass.

### Scope Boundaries
- In scope: the prerequisite source-checking/elaboration fix for named
  applications returning parameterized ADT results as required by
  `stringToList`; one public `stringToList : String -> List Char` conversion
  across source checking, interpreter/runtime, backend emission, object
  generation, native execution, primitive inventory ownership, narrow docs, and
  changelog.
- Out of scope: parser syntax/parity work, formatting family completion,
  Unicode normalization, locale, regex, broader classification family
  completion, case conversion, full slicing coverage, complete cursor API
  design, platform ABI/FFI/GC contracts, compiler source package
  implementation, driver work, proof records, roadmap status edits, controller
  state edits, or semantic roadmap updates.
- Keep the round serial. Do not introduce worker fan-out.

### Round Plan Record
`selection-record.json` is the lineage authority. `round-plan-record.json`
continues to keep this as a single serial recovery implementation round with no
worker fan-out.
