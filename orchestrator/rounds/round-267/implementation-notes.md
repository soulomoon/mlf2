# round-267 implementation notes

## Loaded contracts

- Loaded `AGENTS.md`.
- Loaded `/Users/ares/.agents/skills/tdd/SKILL.md` and used one vertical RED -> GREEN -> refactor cycle.
- Loaded `/Users/ares/.agents/skills/haskell-pro/SKILL.md`.
- Loaded `orchestrator/role-contract.md`, `orchestrator/roles/implementer.md`,
  `orchestrator/active-roadmap-bundle.md`, `orchestrator/project-contract.md`,
  active rev-003 `verification.md`, `selection-record.json`,
  `round-plan-record.json`, and `plan.md`.

## Scope

Selected item: `item-267-unicode-string-length-native-tracer`.

Implemented only one public Prelude operation:

```mlfp
module Main export (main) {
  import Prelude exposing (stringLength);
  def main : Int = stringLength "λa";
}
```

Behavior covered:

- Source check accepts the program.
- `run-program` prints `2\n`.
- `emit-backend` emits LLVM containing `Main__main`, validates assembly, and
  validates object generation.
- `emit-native` emits a native C ABI `main`, validates assembly, validates
  object generation, links, and executes with stdout `2\n`.
- The interpreter counts Haskell `Char` scalar values.
- Native lowering counts UTF-8 scalar starts, not bytes.

Out of scope and not implemented:

- `String`/`List Char` conversion.
- Substring, search, formatting, slicing/classification, cursor APIs.
- Locale, regex, parser parity, platform ABI/FFI/GC contracts, driver work,
  compiler source package work, proof records, or milestone completion claims.
- No compatibility parser aliases, broad fallback text layer, or new worker
  fan-out.

`orchestrator/state.json` is controller-owned and was not edited by this
implementer.

## RED

Command:

```bash
cabal test mlf2-test --test-options='--match "Unicode stringLength source checks, runs, emits backend, and executes natively"'
```

Result before production changes: failed as expected because the public Prelude
operation did not exist.

Key failure:

```text
expected: Right "OK\n"
 but got: Left "... error: module `Prelude` does not export `stringLength` ..."
```

This proved the test was wired to the public source behavior and failed on the
missing `stringLength` surface, not on an unwired test or compile error.

## GREEN

Implemented:

- `MLF.Primitive.Inventory`: added `__string_length : String -> Int` as
  `PrimitiveNativeStringLength` and included it in native-lowerable support.
- Built-in Prelude: exported `stringLength` and defined it as
  `def stringLength : String -> Int = __string_length;`.
- `run-program`: added pure primitive evaluation for `stringLength`, returning
  the scalar count for `String` literals and preserving existing pure-main
  behavior elsewhere.
- LLVM lowering: added the native runtime helper and lowering path for
  `__string_length`; the helper loops over NUL-terminated UTF-8 and increments
  for non-continuation bytes.
- LLVM syntax/pretty-printer: added integer add instruction support needed by
  the loop.
- Tests: added the public end-to-end matcher and primitive inventory coverage.
- Docs/changelog: documented this as the first broad native-capable string
  operation without claiming the rest of milestone-3.

Focused GREEN command:

```bash
cabal test mlf2-test --test-options='--match "Unicode stringLength source checks, runs, emits backend, and executes natively"'
```

Result: PASS, 1 example, 0 failures.

## Files changed

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-language-reference.md`
- `docs/mlfp-self-boot-readiness.md`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Backend/LLVM/Ppr.hs`
- `src/MLF/Backend/LLVM/Syntax.hs`
- `src/MLF/Frontend/Program/Prelude.hs`
- `src/MLF/Frontend/Program/Run.hs`
- `src/MLF/Primitive/Inventory.hs`
- `test/BackendLLVMSpec.hs`
- `test/PrimitiveInventorySpec.hs`
- `orchestrator/rounds/round-267/implementation-notes.md`

## Verification

Focused matcher:

```bash
cabal test mlf2-test --test-options='--match "Unicode stringLength source checks, runs, emits backend, and executes natively"'
```

Result: PASS, 1 example, 0 failures.

Focused neighbor checks:

```bash
cabal test mlf2-test --test-options='--match "Unicode String literal source checks, runs, emits backend, and executes natively" --match "Char literal source checks, runs, emits backend, and executes natively"'
```

Result: PASS, 2 examples, 0 failures.

Evidence checks:

```bash
rg -n 'stringLength|__string_length|PrimitiveNativeStringLength|RuntimeStringLength' src test docs README.md CHANGELOG.md
```

Result: PASS, found implementation, tests, docs, and changelog references.

```bash
rg -n 'Unicode stringLength source checks, runs, emits backend, and executes natively|def main : Int = stringLength "λa";|runProgramFile|emitNativeFile|NativeRunResult ExitSuccess "2\\n"' test/BackendLLVMSpec.hs
```

Result: PASS, found the matcher and public CLI/native helper evidence.

```bash
rg -n 'Unicode scalar|String operation|stringLength|String/List Char|slicing|classification|search|formatting|parser parity' README.md docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
```

Result: PASS, docs/changelog mention the new scalar operation and preserved
future-work boundaries.

Closeout:

```bash
git diff --check
```

Result: PASS.

```bash
cabal build all
```

Result: PASS.

```bash
cabal test
```

Result: PASS, 2571 examples, 0 failures.

```bash
./scripts/thesis-conformance-gate.sh
```

Result: PASS, thesis conformance anchors are green.

## Closeout

The selected serial slice is implemented and verified. The controller can
advance round-267 to review.
