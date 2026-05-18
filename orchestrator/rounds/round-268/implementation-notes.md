# round-268 implementation notes

## Loaded contracts

- `AGENTS.md`
- `/Users/ares/.agents/skills/tdd/SKILL.md`
- `/Users/ares/.agents/skills/haskell-pro/SKILL.md`
- `orchestrator/role-contract.md`
- `orchestrator/roles/implementer.md`
- `orchestrator/active-roadmap-bundle.md`
- `orchestrator/project-contract.md`
- `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-003/verification.md`
- `orchestrator/rounds/round-268/selection-record.json`
- `orchestrator/rounds/round-268/round-plan-record.json`
- `orchestrator/rounds/round-268/plan.md`

## Scope

Implemented only the selected serial slice:
`item-268-string-is-empty-native-tracer`.

The public behavior covered is `.mlfp` source importing Prelude
`stringIsEmpty`, classifying `""` as `true` and `"λ"` as `false`, through
check, `run-program`, backend LLVM, object validation, native LLVM, object
validation, and linked native execution.

Out of scope for this round: broader string classification predicates,
`String`/`List Char` conversion, substring, search, formatting, slicing, cursor
APIs, parser parity, or any merge/review/controller-state changes.
`orchestrator/state.json` was left controller-owned and was not edited.

## RED

Initial focused matcher after adding the vertical test:

```sh
cabal test mlf2-test --test-options='--match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution"'
```

Result: expected RED failure. The new source test failed at checking because
Prelude did not export `stringIsEmpty`:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `stringIsEmpty` ..."
```

## GREEN

Implemented `stringIsEmpty : String -> Bool` as an inventory-owned primitive
`__string_is_empty`, exposed it from Prelude, interpreted it in `run-program`,
and lowered it through backend/native LLVM as a native-capable string
classification helper.

Focused GREEN:

```sh
cabal test mlf2-test --test-options='--match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution"'
```

Result: PASS, `1 example, 0 failures`.

After tightening the native assertions to keep the two linked-native expected
results literal in the test file, the same focused matcher was run again:
PASS, `1 example, 0 failures`.

## Files changed

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Primitive/Inventory.hs`
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `orchestrator/rounds/round-268/implementation-notes.md`

Build-generated path churn in
`runtime/mlfp_io/target/release/libmlfp_io.d` was restored after verification.

## Verification

Focused matcher:

```sh
cabal test mlf2-test --test-options='--match "stringIsEmpty classifies empty and non-empty Unicode strings through native execution"'
```

Result: PASS, `1 example, 0 failures`.

Focused neighbor checks:

```sh
cabal test mlf2-test --test-options='--match "Unicode stringLength source checks, runs, emits backend, and executes natively" --match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Result: PASS, `3 examples, 0 failures`.

Evidence grep:

```sh
rg -n 'stringIsEmpty|__string_is_empty|PrimitiveNativeStringIsEmpty|RuntimeStringIsEmpty' src test docs README.md CHANGELOG.md
```

Result: PASS; matches cover inventory, Prelude, interpreter runtime, LLVM
lowering, focused tests, docs, and changelog.

Plan acceptance grep:

```sh
rg -n 'stringIsEmpty classifies empty and non-empty Unicode strings through native execution|def main : Bool = stringIsEmpty ""|def main : Bool = stringIsEmpty "λ"|NativeRunResult ExitSuccess "true\\n"|NativeRunResult ExitSuccess "false\\n"' test/BackendLLVMSpec.hs
```

Result: PASS; matched the Hspec example and literal linked-native expected
results. The `.mlfp` source strings contain escaped Haskell quotes, so the
source lines were also checked with:

```sh
rg -n 'def main : Bool = stringIsEmpty \\"\\"|def main : Bool = stringIsEmpty \\"λ\\"' test/BackendLLVMSpec.hs
```

Result: PASS; matched the empty and non-empty Unicode source fixtures.

Documentation/current-scope grep:

```sh
rg -n 'Unicode scalar|String classification|stringIsEmpty|stringLength|String/List Char|substring|search|formatting|slicing|parser parity' README.md docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
```

Result: PASS; docs record `stringIsEmpty` support and preserve future-work
boundaries.

Closeout commands:

```sh
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Results:

- `git diff --check`: PASS.
- `cabal build all`: PASS.
- `cabal test`: PASS, `2572 examples, 0 failures`.
- `./scripts/thesis-conformance-gate.sh`: PASS,
  `thesis conformance anchors are green`.

## Closeout

No blocker remains for this slice. The controller should advance round-268 to
review.
