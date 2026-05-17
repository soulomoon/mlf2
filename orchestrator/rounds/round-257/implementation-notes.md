# Round 257 Implementation Notes

### Changes Made

- `README.md`: added a compiler frontend seed fixture section with the fixture path, focused check/run commands, asserted interpreter evidence, existing test coverage, and explicit non-goals.
- `docs/mlfp-language-reference.md`: clarified that compiler-source seed fixtures are ordinary local package roots and that the current frontend seed uses bounded symbolic input, seed-owned token streams, symbolic spans, and tiny parser diagnostics rather than source-text lexing or a new loader.
- `docs/architecture.md`: updated the compiler seed ownership note to include graph/source-path evidence and the existing backend/native/object/native-run proof while keeping that evidence under the existing package, CLI, backend IR, and LLVM owners.
- `docs/mlfp-self-boot-readiness.md`: added the final seed fixture handoff, recorded `ProgramCompilerSeedSpec` as the executable evidence owner, and expanded the next-family recommendation toward source-text lexer/parser expansion with concrete remaining blockers.
- No fixture, seed grammar, lexer/parser contract, primitive/stdlib, public API, backend implementation, roadmap, or controller-state changes were made.

### Tests

- `test/ProgramCompilerSeedSpec.hs`: existing coverage already verifies fixture discovery, module graph order, source paths, package checking, interpreter evidence, public CLI `check-program`/`run-program`, backend/native LLVM emission, object-code generation, and linked native execution for the bounded seed package.

### Validation

- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'`
  - Result: PASS.
  - Summary: `3 examples, 0 failures`; `Test suite mlf2-test: PASS`.
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
- `cabal run mlf2 -- emit-backend test/programs/compiler-seed/frontend-contract > /tmp/round257-compiler-seed-emit-backend.ll`
  - Result: PASS.
  - Follow-up checks: `wc -l /tmp/round257-compiler-seed-emit-backend.ll` reported `3008`; `rg` found `SeedLexer__lexSeedInput`, `SeedParser__parseSeedTokens`, `Main__main`, `__io_bind.wrapper`, and `__io_putStrLn.wrapper`.
- `cabal run mlf2 -- emit-native test/programs/compiler-seed/frontend-contract > /tmp/round257-compiler-seed-emit-native.ll`
  - Result: PASS.
  - Follow-up checks: `wc -l /tmp/round257-compiler-seed-emit-native.ll` reported `3033`; `rg` found native `main`, `Main__main`, `__io_bind.wrapper`, and `__io_putStrLn.wrapper`.
- `git diff --check`
  - Result: PASS.
- `cabal build all`
  - Result: PASS.
- `cabal test`
  - Result: PASS.
  - Summary: `2563 examples, 0 failures`; `Test suite mlf2-test: PASS`.
- `./scripts/thesis-conformance-gate.sh`
  - Result: PASS.
  - Final marker: `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes

- The docs intentionally do not claim self-hosting, checker/backend implementation in `.mlfp`, package-manager support, stable ABI/linker support, separate compilation, or arbitrary compiler workload native support.
- A requested Spark subagent discoverability pass could not start because the agent thread limit was already reached; the same scan was completed locally.
- Validation regenerated `runtime/mlfp_io/target/release/libmlfp_io.d` with a worktree-local absolute path. That generated-path drift was restored to the pre-existing repository-root path with no functional code change.
