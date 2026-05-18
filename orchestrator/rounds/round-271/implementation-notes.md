# Round 271 Implementation Notes

## Scope

- Selected slice: `item-271-string-starts-with-native-tracer`.
- In scope: one public Prelude operation, `stringStartsWith : String -> String -> Bool`, classified through `.mlfp` source checking, `run-program`, backend LLVM emission, object-code validation, native LLVM emission, and linked native execution.
- Out of scope and not claimed: `String`/`List Char` conversion, slicing APIs, broader prefix/search semantics beyond the selected non-empty prefix tracer, broader character classification predicates, formatting, full cursor APIs, parser-owned combinator work, locale, regex, platform ABI/FFI/GC contracts, compiler source package implementation, driver work, proof records, semantic roadmap updates, and milestone completion.
- `orchestrator/state.json` is controller-owned and was not edited.

## Loaded Instructions

- `AGENTS.md`
- `/Users/ares/.agents/skills/tdd/SKILL.md`
- `/Users/ares/.agents/skills/haskell-pro/SKILL.md`
- `orchestrator/role-contract.md`
- `orchestrator/roles/implementer.md`
- `orchestrator/active-roadmap-bundle.md`
- `orchestrator/project-contract.md`
- `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/verification.md`
- `orchestrator/selection-record-schema.md`
- `orchestrator/round-plan-record-schema.md`
- `orchestrator/rounds/round-271/selection-record.json`
- `orchestrator/rounds/round-271/round-plan-record.json`
- `orchestrator/rounds/round-271/plan.md`

## RED

Added the focused public behavior test in `test/BackendLLVMSpec.hs`, then ran:

```bash
cabal test mlf2-test --test-options='--match "stringStartsWith classifies Unicode prefixes through native execution"'
```

Result: expected RED failure. The test compiled, ran one example, and failed at the public source-checking boundary because `Prelude` did not export `stringStartsWith`:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `stringStartsWith` ..."
```

## GREEN

Implemented `stringStartsWith` as the next narrow text-substrate tracer:

- Added `stringStartsWith : String -> String -> Bool` to the source Prelude, backed by the owner primitive `__string_starts_with`.
- Added shared primitive-inventory ownership via `stringStartsWithPrimitiveName`, `PrimitiveNativeStringStartsWith`, and native-lowerable classification.
- Added interpreter support in `MLF.Frontend.Program.Run` using Haskell `isPrefixOf`, preserving source/run parity with backend/native.
- Added LLVM lowering for the native primitive by comparing UTF-8 string bytes from the haystack start against the prefix bytes; empty prefix returns `true`.
- Added public `.mlfp` coverage for `"λab"`/`"λ"` => `true` and `"aλb"`/`"λ"` => `false` through check/run/backend/object/native paths.
- Updated docs/changelog to record the narrow public prefix tracer and to keep broader string/cursor/parser work out of scope.

Re-ran the focused matcher:

```bash
cabal test mlf2-test --test-options='--match "stringStartsWith classifies Unicode prefixes through native execution"'
```

Result: PASS, 1 example, 0 failures.

## Files Changed

- `CHANGELOG.md` — progress entry for the new public/native text tracer.
- `docs/backend-native-pipeline.md` — backend/native primitive support note.
- `docs/mlfp-language-reference.md` — public `stringStartsWith` signature and current boundary.
- `docs/mlfp-self-boot-readiness.md` — readiness matrix updated to include the prefix tracer without claiming broad string support.
- `src/MLF/Backend/LLVM/Lower.hs` — LLVM declaration/runtime mapping and byte-prefix lowering.
- `src/MLF/Frontend/Program/Prelude.hs` — public Prelude binding.
- `src/MLF/Frontend/Program/Run.hs` — interpreter/runtime primitive semantics.
- `src/MLF/Primitive/Inventory.hs` — shared primitive ownership and native-lowerable classification.
- `test/BackendLLVMSpec.hs` — public source-to-native behavior.
- `test/PrimitiveInventorySpec.hs` — shared inventory classification expectation.
- `orchestrator/rounds/round-271/implementation-notes.md`

## Verification

RED:

```bash
cabal test mlf2-test --test-options='--match "stringStartsWith classifies Unicode prefixes through native execution"'
```

Result: FAIL as expected, 1 example. Public source checking rejected the import because `Prelude` did not yet export `stringStartsWith`.

Focused GREEN:

```bash
cabal test mlf2-test --test-options='--match "stringStartsWith classifies Unicode prefixes through native execution"'
```

Result: PASS, 1 example, 0 failures.

Focused neighbor text/native checks:

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution" --match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Result: PASS, 6 examples, 0 failures.

Primitive inventory check:

```bash
cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'
```

Result: PASS, 1 example, 0 failures.

Evidence scans:

```bash
rg -n 'stringStartsWith : String -> String -> Bool|__string_starts_with|stringStartsWithPrimitiveName|PrimitiveNativeStringStartsWith|RuntimeStringStartsWith' src test docs README.md CHANGELOG.md
rg -n 'stringStartsWith classifies Unicode prefixes through native execution|stringStartsWith "λab"|stringStartsWith "aλb"|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
rg -n 'stringStartsWith \\"λab\\"|stringStartsWith \\"aλb\\"' test/BackendLLVMSpec.hs
rg -n 'Unicode scalar|prefix search|stringStartsWith|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|slicing|formatting|cursor|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
```

Result: all scans exited 0 and found the expected implementation, public behavior, native-result, and boundary documentation evidence.

Closeout:

```bash
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Results:

- `git diff --check`: PASS.
- `cabal build all`: PASS.
- `cabal test`: PASS, 2575 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`: PASS, including obligation mapping, claims validation, translatability matrix rows, A6 regressions, theorem obligations, ga' redirect stability, translatable presolution invariant, Phi soundness, and expansion minimality.

No broad closeout command is blocked. The generated `runtime/mlfp_io/target/release/libmlfp_io.d` path churn from local build/test execution was restored after the gate. `orchestrator/state.json` remains controller-owned and was not edited by this implementer.
