# Round 256 Implementation Notes

## Changes Made

- Updated `test/ProgramCompilerSeedSpec.hs` with a focused compiler-seed backend/native/object/native-run assertion over the existing CLI emission paths. The test asserts raw backend LLVM and native LLVM contain the reachable seed entry symbols and IO wrappers, validates LLVM assembly and object-code generation, and checks linked native execution preserves the same two evidence lines as `run-program`.
- Updated `docs/mlfp-self-boot-readiness.md` with a compiler frontend seed layer classification across source checking, interpreter/runtime, backend/native, object-code, and package-build layers. The classification records per-module support for the merged seed and the package root without claiming self-hosting, stable ABI/linking, package-manager behavior, separate compilation, or arbitrary compiler workload native support.
- No seed grammar, parser contract, primitive/stdlib surface, public facade, backend IR, native runtime, roadmap, or controller-state changes were made.

## Validation

- `cabal run mlf2 -- check-program test/programs/compiler-seed/frontend-contract`
  - Result: PASS.
  - Output: `OK`.
- `cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract`
  - Result: PASS.
  - Output:

    ```text
    lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol
    parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true
    ```

- `cabal run mlf2 -- emit-backend test/programs/compiler-seed/frontend-contract > /tmp/round256-compiler-seed-emit-backend.ll && wc -l /tmp/round256-compiler-seed-emit-backend.ll && rg -n 'define .*@"Main__main"|define .*@"SeedLexer__lexSeedInput"|define .*@"SeedParser__parseSeedTokens"|__io_putStrLn\.wrapper|__io_bind\.wrapper' /tmp/round256-compiler-seed-emit-backend.ll | head -20`
  - Result: PASS.
  - Output: 3008 LLVM lines; found `SeedLexer__lexSeedInput`, `SeedParser__parseSeedTokens`, `Main__main`, `__io_bind.wrapper`, and `__io_putStrLn.wrapper`.
- `cabal run mlf2 -- emit-native test/programs/compiler-seed/frontend-contract > /tmp/round256-compiler-seed-emit-native.ll && wc -l /tmp/round256-compiler-seed-emit-native.ll && rg -n 'define i32 @"main"|define ptr @"Main__main"|__io_putStrLn\.wrapper|__io_bind\.wrapper|__mlfp_native_render' /tmp/round256-compiler-seed-emit-native.ll | head -30`
  - Result: PASS.
  - Output: 3033 LLVM lines; found C ABI `main`, `Main__main`, `__io_bind.wrapper`, and `__io_putStrLn.wrapper`.
- `set -e; LLC=$(command -v llc); CC_BIN=$(command -v cc); $LLC -relocation-model=pic -filetype=obj -o /tmp/round256-compiler-seed.o /tmp/round256-compiler-seed-emit-native.ll; cargo build --release --manifest-path runtime/mlfp_io/Cargo.toml >/tmp/round256-cargo-build.log; $CC_BIN /tmp/round256-compiler-seed.o runtime/mlfp_io/target/release/libmlfp_io.a -o /tmp/round256-compiler-seed; /tmp/round256-compiler-seed`
  - Result: PASS.
  - Output:

    ```text
    lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol
    parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true
    ```

- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'`
  - Result: PASS.
  - Output summary: `Finished in 109.8186 seconds`; `3 examples, 0 failures`; `Test suite mlf2-test: PASS`.
- `git diff --check`
  - Result: PASS before full validation.
- `cabal build all`
  - Result: PASS.
- `cabal test`
  - Result: PASS.
  - Output summary: `Finished in 491.6256 seconds`; `2563 examples, 0 failures`; `Test suite mlf2-test: PASS`.
- `./scripts/thesis-conformance-gate.sh`
  - Result: PASS.
  - Output summary: `[thesis-obligations] PASS: all obligations are mapped and green`; `[thesis-claims] PASS: all validations green`; `[thesis-gate] PASS: thesis conformance anchors are green`.
- `git diff --check`
  - Result: PASS for tracked diffs after full validation, notes, and generated metadata cleanup.
- `rg -n "[ \t]+$" orchestrator/rounds/round-256/implementation-notes.md`
  - Result: PASS; no trailing whitespace in the new untracked implementation-notes file.

## Notes

- An initial parallel attempt to run the focused CLI commands hit Cabal package database contention: `ghc-pkg-9.14.1: cannot create: ... dist-newstyle/packagedb/ghc-9.14.1 already exists`. The focused CLI commands were rerun serially and passed.
- The current compiler seed does not exercise an unsupported backend/native fail-closed case because the merged seed fits the existing supported subset: ADTs, direct seed functions, `String` evidence, and `IO Unit` through `putStrLn`/`bind`. Unsupported future compiler shapes remain documented as fail-closed in `docs/backend-native-pipeline.md`.
- Native validation rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` to the round worktree path. That generated metadata was restored before final status.
