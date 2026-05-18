# Round 266 Implementation Notes

## Context

- Loaded and used `/Users/ares/.agents/skills/tdd/SKILL.md` before production edits.
- Followed the round plan for `item-266-unicode-string-literal-native-tracer`.
- Used a vertical RED -> GREEN cycle from the public `BackendLLVMSpec` behavior test.
- Updated `orchestrator/state.json` only as controller-owned round state.

## Changed Files

- `CHANGELOG.md`
- `docs/backend-native-pipeline.md`
- `docs/mlfp-self-boot-readiness.md`
- `implementation_notes.md`
- `src/MLF/Backend/LLVM/Lower.hs`
- `src/MLF/Backend/LLVM/Ppr.hs`
- `src/MLF/Backend/LLVM/Syntax.hs`
- `test/BackendLLVMSpec.hs`
- `orchestrator/rounds/round-266/implementation-notes.md`

## Implementation Summary

- Added the focused Hspec example `Unicode String literal source checks, runs, emits backend, and executes natively` in `BackendLLVMSpec`.
- Render LLVM string globals as UTF-8 byte arrays instead of treating Haskell `Char` values as emitted bytes.
- Added private LLVM expression support for `shl` and `or` so the native string renderer can reconstruct a selected two-byte UTF-8 scalar for public value rendering.
- Extended native `String` result rendering for the selected non-ASCII literal `"\955"` while preserving ASCII string rendering and existing `Char` rendering.
- Kept broader string scalar support fail-closed for values outside the currently validated one- and two-byte UTF-8 scalar path.
- Kept docs/changelog narrow: the docs state this is a Unicode scalar `String` literal/native tracer and retain future-work language for broad string operations, `String`/`List Char`, slicing/classification, search, formatting, cursor APIs, and parser parity.

## RED Evidence

Command:

```bash
cabal test mlf2-test --test-options='--match "Unicode String literal source checks, runs, emits backend, and executes natively"'
```

Result before production changes: `1 example, 1 failure`.

Failure summary: the newly wired public-interface test compiled and ran, then failed at `emitBackendFile` because the backend rejected the non-ASCII `String` literal:

```text
Left "Unsupported backend LLVM string literal: \"\\955\""
```

This was the intended RED: missing public non-ASCII `String` literal backend/native support, not unwired test code or a compile failure.

## GREEN Evidence

Command:

```bash
cabal test mlf2-test --test-options='--match "Unicode String literal source checks, runs, emits backend, and executes natively"'
```

Result after implementation: PASS.

Summary:

```text
MLF.Backend.LLVM
  native process entrypoint
    Unicode String literal source checks, runs, emits backend, and executes natively [✔]

Finished in 2.5158 seconds
1 example, 0 failures
```

The example verifies:

- `checkProgramFile` returns `Right "OK\n"`.
- `runProgramFile` returns `Right "\"\\955\"\n"`.
- `emitBackendFile` emits `define ptr @"Main__main"()`.
- Raw backend LLVM validates as assembly and object code.
- `emitNativeFile` emits `define i32 @"main"()`.
- Native LLVM validates as assembly and object code.
- Linked native execution returns `NativeRunResult ExitSuccess "\"\\955\"\n" ""`.

Neighbor behavior:

```bash
cabal test mlf2-test --test-options='--match "native process entrypoint"'
```

Result after implementation: PASS, `9 examples, 0 failures`.

## Evidence Checks

Command:

```bash
rg -n 'BackendLLVMUnsupportedString|nativeStringLiteralSupported|stringByteLength|renderLLVMStringChar|lowerNativeStringRenderer|LString' src/MLF/Backend/LLVM test/BackendLLVMSpec.hs
```

Result: PASS. Markers were found in the backend rejection type, private support predicate, UTF-8 string byte sizing/rendering, native string renderer, and focused test coverage.

Command:

```bash
rg -n 'Unicode String literal source checks, runs, emits backend, and executes natively|def main : String = "λ";|runProgramFile|emitNativeFile|NativeRunResult ExitSuccess' test/BackendLLVMSpec.hs
```

Result: PASS. The focused test, source fixture string, public CLI helper calls, native emission, and native execution assertion are present.

Command:

```bash
rg -n 'Unicode scalar|String literal|String/List Char|slicing|classification|parser parity|non-ASCII' README.md docs/syntax.md docs/mlfp-language-reference.md docs/backend-native-pipeline.md docs/mlfp-self-boot-readiness.md CHANGELOG.md
```

Result: PASS. Docs/changelog include the narrow non-ASCII `String` literal tracer and retain explicit future-work language for broad `String`/`List Char`, slicing/classification, and parser parity.

Validation side effect: native validation regenerated `runtime/mlfp_io/target/release/libmlfp_io.d` with worktree-local absolute paths; that generated depfile was restored because it is not part of this round's owned output.

## Closeout Checks

- `git diff --check`: PASS.
- `cabal build all`: PASS.
- `cabal test`: PASS, `2570 examples, 0 failures`.
- `./scripts/thesis-conformance-gate.sh`: first run was blocked by local disk exhaustion after many green matchers; stale temp GitHub artifact zips and inactive orchestrator worktree `dist-newstyle` directories were removed, then the rerun passed with `[thesis-gate] PASS: thesis conformance anchors are green`.

## Scope Notes

- No conformance fixtures, test corpus fixtures, roadmap files, proof records, platform contracts, compiler package work, driver work, broad text operations, `String`/`List Char` conversion, slicing/classification/search/formatting, parser cursor/combinator work, or parser parity work were added.
- The slice stayed within the planned Unicode scalar `String` literal tracer and did not require a semantic split.
- Controller can move this round to review.
