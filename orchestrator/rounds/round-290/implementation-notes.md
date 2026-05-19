# Round 290 Implementation Notes

## Scope

- Selected item: `item-290-string-to-list-native-tracer`.
- Public behavior: `stringToList : String -> List Char`, backed by
  `__string_to_list`, preserves Unicode scalar order through source checking,
  `run-program`, backend LLVM/object validation, `emit-native`/native object
  validation, and linked native execution.
- Recovery prerequisite: fix the public checker regression
  `named functions returning List Char source check without PhiReorder binder identity failure`
  without special-casing `stringToList`, `__string_to_list`, `List Char`, or
  the fixture.
- Scope stayed out of roadmap/status updates and `orchestrator/state.json`.

## RED Evidence

Original focused RED before production implementation:

```sh
cabal test mlf2-test --test-options='--match "stringToList converts Unicode scalar strings to List Char values through native execution"'
```

Result: failed as expected because the Prelude did not export `stringToList`.

Recovery prerequisite RED:

```sh
cabal test mlf2-test --test-options='--match "named functions returning List Char source check without PhiReorder binder identity failure"'
```

Result: failed as expected during source checking/elaboration:

```text
error: pipeline error: Phase 6 (elaboration): PhiInvariantError "PhiReorder: missing binder identity at positions [1]"
```

The prerequisite fixture is:

```mlfp
module Main export (main) {
  import Prelude exposing (List(..));
  def f : String -> List Char = λ(value : String) Nil;
  def main : List Char = f "aλ";
}
```

## GREEN Implementation

- Added the prerequisite public checker regression in
  `test/BackendLLVMSpec.hs`.
- Fixed the Phi reorder root cause in
  `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs`: only graph-origin scheme
  binders whose names parse as graph binder IDs or appear in `siSubst` require
  binder identities during reorder. Synthetic residual/source binders in mixed
  schemes stay positional instead of causing an invariant failure.
- Added `stringToList` to the Prelude and primitive inventory:
  `stringToList : String -> List Char` and `__string_to_list`.
- Added `run-program` support that decodes a Haskell `String` into ordinary
  Prelude `Nil`/`Cons` `Char` cells.
- Added native LLVM lowering/runtime support that decodes valid UTF-8 scalars
  and constructs existing `List Char` ADT cells.
- Added generic backend structural ADT boundary matching needed by backend IR
  validation when primitive inventory uses nominal `List Char` while checked
  programs lower the Prelude list as structural recursive data.
- Updated narrow docs and changelog surfaces without claiming formatting,
  parser parity, platform contracts, broader collection APIs, or self-boot
  proof completion.

## GREEN Evidence

Prerequisite GREEN:

```sh
cabal test mlf2-test --test-options='--match "named functions returning List Char source check without PhiReorder binder identity failure"'
```

Result: passed, 1 example, 0 failures.

Original focused GREEN:

```sh
cabal test mlf2-test --test-options='--match "stringToList converts Unicode scalar strings to List Char values through native execution"'
```

Result: passed, 1 example, 0 failures.

Primitive inventory:

```sh
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Result: passed, 1 example, 0 failures.

Neighbor matcher set:

```sh
cabal test mlf2-test --test-options='--match "stringFromList converts List Char values to Unicode scalar strings through native execution" --match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "stringCharAt indexes Unicode scalar cursor positions through native execution" --match "stringSlice slices Unicode scalar ranges through native execution" --match "stringTake slices Unicode scalar prefixes through native execution" --match "stringDrop slices Unicode scalar prefixes through native execution" --match "stringContains searches Unicode substrings through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Result: passed, 11 examples, 0 failures.

## Evidence Scans

All required evidence scans passed:

```sh
rg -n -e 'named functions returning List Char source check without PhiReorder binder identity failure' -e 'def f : String -> List Char' test
rg -n -e 'PhiReorder: missing binder identity' -e 'binder identity' src test orchestrator/rounds/round-290/implementation-notes.md
rg -n -e 'stringToList : String -> List Char' -e '__string_to_list' -e 'stringToListPrimitiveName' -e 'PrimitiveNativeStringToList' -e 'RuntimeStringToList' src test docs CHANGELOG.md
rg -n -e 'stringToList converts Unicode scalar strings to List Char values through native execution' -e 'stringToList "aλ"' -e 'stringToList ""' -e "Cons 'a'" -e 'Nil' test/BackendLLVMSpec.hs
rg -n 'Unicode scalar|stringToList|String/List Char|String -> List Char|formatting|classification|cursor|parser parity|platform contract|self-boot proof' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md
```

Result: expected matches only; no stale `reverse String -> List Char` absence
claim remained in the updated docs/changelog surfaces.

## Broad Validation

```sh
git diff --check
```

Result: passed.

```sh
cabal build all
```

Result: passed.

```sh
CARGO_TARGET_DIR=/tmp/round290-cargo-target cabal test
```

Result: passed, 2595 examples, 0 failures.

```sh
CARGO_TARGET_DIR=/tmp/round290-cargo-target ./scripts/thesis-conformance-gate.sh
```

Result: passed; thesis conformance anchors are green.

Generated dependency churn:

- `runtime/mlfp_io/target/release/libmlfp_io.d` was dirtied by validation with
  worktree-local absolute paths.
- Restored with:

```sh
git restore runtime/mlfp_io/target/release/libmlfp_io.d
```

## Files Changed

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `src/MLF/Backend/IR.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Backend/StructuralRecursiveData.hs`
- `src/MLF/Elab/Phi/Omega/Interpret/Internal.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Primitive/Inventory.hs`
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `orchestrator/rounds/round-290/implementation-notes.md`

## Closeout

No blocker remains. The round is ready for controller review.
