# Round 270 Implementation Notes

## Scope

- Selected slice: `item-270-string-contains-native-tracer`.
- Implemented one public Prelude operation: `stringContains : String -> String -> Bool`.
- In-scope behavior: `.mlfp` programs classify `stringContains "aλb" "λ"` as `true` and `stringContains "ab" "λ"` as `false` through check, run, backend LLVM, object generation, native LLVM, and linked native execution.
- Out of scope and not claimed: `String`/`List Char` conversion, slicing, formatting, broader search APIs, cursor APIs, parser parity, locale, regex, platform ABI/FFI/GC contracts, compiler package work, self-boot proof completion, and milestone completion.
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
- `orchestrator/rounds/round-270/selection-record.json`
- `orchestrator/rounds/round-270/round-plan-record.json`
- `orchestrator/rounds/round-270/plan.md`

## RED

Added the focused public behavior test in `test/BackendLLVMSpec.hs`, then ran:

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution"'
```

Result: expected RED failure. The test compiled and failed at the public source-checking boundary because `Prelude` did not export `stringContains`:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `stringContains` ..."
```

## GREEN

Implemented the vertical path:

- `MLF.Primitive.Inventory`: added reserved `__string_contains : String -> String -> Bool` with native support `PrimitiveNativeStringContains`.
- `MLF.Frontend.Program.Prelude`: exported and defined `stringContains`.
- `MLF.Frontend.Program.Run`: interpreted `stringContains` over source `String` values.
- `MLF.Backend.LLVM.Lower`: lowered `__string_contains` in raw backend declarations and native runtime functions. The native helper compares UTF-8 byte sequences only from scalar boundaries, preserving source-text matching without exposing byte indexing.
- `test/BackendLLVMSpec.hs`: added present and absent Unicode substring programs with check/run/backend/object/native assertions.
- `test/PrimitiveInventorySpec.hs`: covered the new native-lowerable primitive inventory classification.
- Docs and changelog were updated without broadening the public contract.

Focused GREEN command:

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution"'
```

Result: PASS, 1 example, 0 failures. Both selected programs returned `true\n` or `false\n` through interpreter and linked native execution with empty stderr.

## Files Changed

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `orchestrator/rounds/round-270/implementation-notes.md`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Primitive/Inventory.hs`
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`

## Verification

```bash
cabal test mlf2-test --test-options='--match "stringContains searches Unicode substrings through native execution"'
```

PASS: 1 example, 0 failures.

```bash
cabal test mlf2-test --test-options='--match "stringContainsChar searches Unicode scalars through native execution" --match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution" --match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

PASS: 5 examples, 0 failures.

```bash
rg -n 'stringContains : String -> String -> Bool|__string_contains"|stringContainsPrimitiveName|PrimitiveNativeStringContains|RuntimeStringContains' src test docs README.md CHANGELOG.md
```

PASS: found the public signature, primitive inventory support, runtime support, docs, and changelog evidence.

```bash
rg -n 'stringContains searches Unicode substrings through native execution|stringContains "aλb"|stringContains "ab"|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

PASS: found the focused test and native-result expectations. Supplemental escaped-source search found the exact fixture program lines because the `.mlfp` source strings are Haskell string literals.

```bash
rg -n 'Unicode scalar|substring search|stringContains|stringContainsChar|stringIsEmpty|stringLength|String/List Char|slicing|formatting|parser parity' docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
```

PASS: found docs/changelog evidence and preserved future-work boundaries.

```bash
git diff --check
```

PASS before notes.

```bash
cabal build all
```

PASS.

```bash
cabal test
```

PASS: 2574 examples, 0 failures.

```bash
./scripts/thesis-conformance-gate.sh
```

PASS: thesis conformance anchors are green.

## Closeout

- The generated Rust dependency path churn in `runtime/mlfp_io/target/release/libmlfp_io.d` was restored to the pre-existing canonical root path.
- No blockers.
