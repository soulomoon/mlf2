# Round 275 Implementation Notes

## Changes Made

- Added the public Prelude binding
  `stringSlice : String -> Int -> Int -> String = __string_slice`.
- Added the inventory-owned primitive name, native support tag, lowerable
  classification, and inventory coverage for `__string_slice`.
- Added interpreter/runtime support for `RuntimeStringSlice` using the existing
  Unicode scalar drop-then-take behavior.
- Added LLVM/backend lowering for raw backend declarations and native runtime
  emission. The native helper composes the existing native `__string_drop` and
  `__string_take` scalar paths.
- Documented the new range slicing tracer in the language reference, native
  backend pipeline docs, self-boot readiness ledger, and changelog.

## Tests

- `test/BackendLLVMSpec.hs`: added the focused public behavior test
  `stringSlice slices Unicode scalar ranges through native execution`.
- `test/PrimitiveInventorySpec.hs`: extended the inventory-owned lowerable
  primitive classification assertions for `stringSlice`.

## Files Changed

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `orchestrator/rounds/round-275/implementation-notes.md`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Primitive/Inventory.hs`
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`

## TDD Evidence

Loaded TDD skill: `/Users/ares/.agents/skills/tdd/SKILL.md`.

RED command:

```bash
cabal test mlf2-test --test-options='--match "stringSlice slices Unicode scalar ranges through native execution"'
```

RED result: failed as expected at the public Prelude boundary.

```text
expected: Right "OK\n"
 but got: Left "... module `Prelude` does not export `stringSlice` ..."
```

GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringSlice slices Unicode scalar ranges through native execution"'
```

GREEN result: passed, 1 example, 0 failures.

## Validation

- `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  passed, 1 example, 0 failures.
- Neighbor text-substrate native matcher passed, 10 examples, 0 failures:
  `stringTake`, `stringDrop`, `stringEndsWith`, `stringStartsWith`,
  `stringContains`, `stringContainsChar`, `stringIsEmpty`, `stringLength`,
  Unicode `String`, and `Char` native tracers.
- Evidence scans confirmed `stringSlice : String -> Int -> Int -> String`,
  `__string_slice`, `PrimitiveNativeStringSlice`, `RuntimeStringSlice`, the
  focused test rows, source examples, and docs/changelog entries.
- `git diff --check` passed.
- `cabal build all` passed.
- `cabal test` passed, 2579 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh` passed.
- The broad gates rewrote
  `runtime/mlfp_io/target/release/libmlfp_io.d` to a worktree-local path; that
  generated drift was restored to the tracked repository-local path and is not
  part of the final diff.

## Scope Boundaries

- Scope is limited to `item-275-string-slice-native-tracer`.
- This does not add `String`/`List Char` conversion, formatting, full cursor
  APIs, parser parity, platform contracts, compiler package work, or proof
  records.
- `orchestrator/state.json` is controller-owned, was already dirty at start,
  and was not edited.

## Blockers

- None.
