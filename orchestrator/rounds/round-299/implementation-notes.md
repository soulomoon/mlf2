### Changes Made
- `test/BackendLLVMSpec.hs`: added `stringSplit splits Unicode scalar substrings through native execution`, covering check, `run-program`, backend LLVM/object validation, native LLVM/object validation, and linked native execution for Unicode delimiter splitting, no-match singleton, empty-delimiter singleton, and leading/trailing empty segment preservation.
- `src/MLF/Primitive/Inventory.hs`: added `stringSplitPrimitiveName`, `PrimitiveNativeStringSplit`, and the shared `__string_split : String -> String -> List String` inventory entry.
- `test/PrimitiveInventorySpec.hs`: added `__string_split` to the native-lowerable primitive inventory assertion.
- `src/MLF/Frontend/Program/Prelude.hs`: exposed public `stringSplit : String -> String -> List String` backed by `__string_split`.
- `src/MLF/Frontend/Program/Run.hs`: added interpreter/runtime support for `RuntimeStringSplit`, including empty-delimiter/no-match singleton behavior and leading/trailing empty segment preservation.
- `src/MLF/Backend/LLVM/Lower.hs`: lowered `__string_split` to a native helper that scans UTF-8 strings at Unicode scalar boundaries, splits on non-empty substring delimiters left-to-right without overlap, and returns a `List String`.
- `src/MLF/Elab/Phi/Translate.hs`: aligned replay-domain validation with explicit producer replay binders. This file was outside the expected list, but the required direct two-argument `List`-return fixture exposed a pre-existing Phi validation bug before the new primitive could reach runtime/backend validation.
- `docs/mlfp-language-reference.md`, `docs/backend-native-pipeline.md`, and `docs/mlfp-self-boot-readiness.md`: documented the public API, native primitive coverage, exact semantics, and out-of-scope boundaries without claiming regex, normalization, locale, parser parity, platform-contract, or self-boot proof completion.
- `CHANGELOG.md`: recorded the public/native `stringSplit` progress note.

### Tests
- RED: `cabal test mlf2-test --test-options='--match "stringSplit splits Unicode scalar substrings through native execution"'` failed as expected before production changes. Failure: `module Prelude does not export stringSplit`.
- GREEN: `cabal test mlf2-test --test-options='--match "stringSplit splits Unicode scalar substrings through native execution"'` passed (`1 example, 0 failures`).
- `cabal test mlf2-test --test-options='--match "classifies native-lowerable primitive support from the shared primitive inventory owner"'` passed (`1 example, 0 failures`).
- Neighbor matcher for `stringReplace`, `stringIndexOf`, `stringIndexOfChar`, `stringReplaceChar`, `stringContains`, `stringAppend`, `stringFromList`, `stringToList`, and nested ADT rendering passed (`9 examples, 0 failures`).
- Required evidence scans for `stringSplit`, `__string_split`, inventory/runtime/lowering names, fixtures, and documentation claims were run and reviewed.
- Generated-artifact audit `git diff --name-only -- '*.d' '*.rlib' 'Cargo.lock' 'dist-newstyle/**' 'target/**'` is clean after restoring generated depfile churn from focused native tests.
- `git diff --check` passed.
- `CARGO_TARGET_DIR=/tmp/round299-cargo-target cabal build all` passed.
- `CARGO_TARGET_DIR=/tmp/round299-cargo-target cabal test` passed (`2604 examples, 0 failures`).
- First `CARGO_TARGET_DIR=/tmp/round299-cargo-target ./scripts/thesis-conformance-gate.sh` run hit a transient Cabal package database lookup failure at `O15-ENV-WF`; direct `O15-ENV-WF` rerun passed (`3 examples, 0 failures`).
- Required rerun `CARGO_TARGET_DIR=/tmp/round299-cargo-target ./scripts/thesis-conformance-gate.sh` passed with `PASS: thesis conformance anchors are green`.

### Notes
Loaded `/Users/ares/.agents/skills/tdd/SKILL.md` and `/Users/ares/.agents/skills/haskell-pro/SKILL.md`. The final implementation keeps the slice narrow: no split-on-character aliases, regex, Unicode normalization, locale behavior, case conversion, broader `List String` helpers, parser parity, platform ABI/FFI/GC contract work, proof records, controller-state edits, or roadmap status edits were added. `orchestrator/state.json` was left untouched as the controller-provided active-round marker.
