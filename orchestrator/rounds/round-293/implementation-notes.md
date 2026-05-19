# Round 293 Implementation Notes

## Scope

- Selected item: `item-293-string-from-nat-native-tracer`.
- Target behavior: public Prelude `stringFromNat : Nat -> String` backed by
  reserved primitive `__string_from_nat : Nat -> String`.
- Scope is limited to canonical decimal formatting of Prelude `Nat` values
  built from `Zero` and `Succ`, through source checking, `run-program`,
  backend/object validation, emit-native/native-object validation, and linked
  native execution.
- Out of scope: general `Show`, `Show Nat`, generic ADT rendering,
  interpolation, printf-style formatting, locale behavior, regex, parser
  parity, platform ABI contracts, roadmap status edits, semantic roadmap
  updates, and proof-record claims.

## RED

- TDD skill path: `/Users/ares/.agents/skills/tdd/SKILL.md`.
- Command: `cabal test mlf2-test --test-options='--match "stringFromNat formats Nat values as decimal strings through native execution"'`
- Result: failed as expected before production changes.
- Failure: `Prelude` did not export `stringFromNat`.

## GREEN

- Command: `cabal test mlf2-test --test-options='--match "stringFromNat formats Nat values as decimal strings through native execution"'`
- Result: passed after implementation.
- Evidence: `1 example, 0 failures`.
- Public behavior covered:
  - `stringFromNat Zero` renders `"0"`.
  - `stringFromNat (Succ (Succ Zero))` renders `"2"`.
  - The focused test exercises `checkProgramFile`, `runProgramFile`,
    `emitBackendFile`, backend assembly/object validation, `emitNativeFile`,
    native assembly/object validation, and linked native execution.

## Files Changed

- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `src/MLF/Primitive/Inventory.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Backend/LLVM/Lower.hs`
- `docs/mlfp-language-reference.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-self-boot-readiness.md`
- `CHANGELOG.md`
- `orchestrator/rounds/round-293/implementation-notes.md`

## Validation

- Focused GREEN:
  - Command: `cabal test mlf2-test --test-options='--match "stringFromNat formats Nat values as decimal strings through native execution"'`
  - Result: passed, `1 example, 0 failures`.
- Primitive inventory matcher:
  - Command: `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'`
  - Result: passed, `1 example, 0 failures`.
- Focused neighbor set:
  - Command: `cabal test mlf2-test --test-options='--match "stringFromBool formats Bool values as strings through native execution" --match "stringFromInt formats Int values as decimal strings through native execution" --match "stringToList converts Unicode scalar strings to List Char values through native execution" --match "stringFromList converts List Char values to Unicode scalar strings through native execution" --match "stringFromChar converts Unicode scalar Chars to singleton strings through native execution" --match "stringAppend concatenates Unicode scalar strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "charIsAsciiPrintable classifies ASCII printable Char values through native execution" --match "Unicode String literal source checks, runs, emits backend, and executes natively"'`
  - Result: passed, `9 examples, 0 failures`.
- Evidence `rg` checks:
  - Command: `rg -n -e 'stringFromNat : Nat -> String' -e '__string_from_nat' -e 'stringFromNatPrimitiveName' -e 'PrimitiveNativeStringFromNat' -e 'RuntimeStringFromNat' src test docs CHANGELOG.md`
  - Result: passed; found the Prelude/docs/changelog surface, reserved
    primitive name, primitive inventory/native support, runtime primitive, and
    backend lowering evidence.
  - Command: `rg -n -e 'stringFromNat formats Nat values as decimal strings through native execution' -e 'stringFromNat Zero' -e 'stringFromNat (Succ (Succ Zero))' test/BackendLLVMSpec.hs`
  - Result: passed; found the focused test name and both public Nat examples.
  - Command: `rg -n 'Unicode scalar|stringFromNat|stringFromBool|stringFromInt|String/List Char|formatting|Explicit String Formatting|Show Nat|Show|generic ADT|printf|interpolation|locale|regex|classification|cursor|parser parity|platform contract|self-boot proof|milestone-3 completion|milestone completion' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CONTEXT.md CHANGELOG.md`
  - Result: passed for audit. The new `stringFromNat` docs describe a bounded
    explicit conversion tracer and keep broader Show, generic ADT rendering,
    formatting completion, locale, regex, parser parity, platform, milestone,
    and self-boot proof claims out of scope or in existing glossary/avoidance
    contexts.
- Command: `git diff --check`
  - Result: passed.
- Command: `cabal build all`
  - Result: passed.
- Command: `CARGO_TARGET_DIR=/tmp/round293-cargo-target cabal test`
  - Result: passed, `2598 examples, 0 failures`.
- Command: `CARGO_TARGET_DIR=/tmp/round293-cargo-target ./scripts/thesis-conformance-gate.sh`
  - Result: passed; ended with `PASS: thesis conformance anchors are green`.

## Generated Churn

- `runtime/mlfp_io/target/release/libmlfp_io.d` was dirtied by validation and
  restored with `git restore runtime/mlfp_io/target/release/libmlfp_io.d`.
- Controller-owned `orchestrator/state.json` was already dirty and was left
  untouched.
