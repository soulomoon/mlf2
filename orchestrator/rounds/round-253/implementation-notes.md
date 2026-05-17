### Changes Made
- `test/programs/compiler-seed/frontend-contract/SeedSource.mlfp`: added symbolic source-position/span labels, bounded input symbols, monomorphic seed input, and positive/negative lexer inputs.
- `test/programs/compiler-seed/frontend-contract/SeedToken.mlfp`: added token and token-stream ADTs for the bounded lexer seed.
- `test/programs/compiler-seed/frontend-contract/SeedDiagnostic.mlfp`: added lexer diagnostic kind and diagnostic ADTs carrying symbolic spans.
- `test/programs/compiler-seed/frontend-contract/SeedLexer.mlfp`: added the bounded `.mlfp` lexer, lexer-result/evidence ADTs, and `.mlfp` evidence rendering that checks the positive token stream and negative diagnostic span/position path.
- `test/programs/compiler-seed/frontend-contract/Main.mlfp`: changed the seed entrypoint to print aggregate lexer evidence through the interpreter IO path.
- `test/ProgramCompilerSeedSpec.hs`: updated the package graph, check/run, and CLI assertions for the expanded seed package and exact lexer evidence output.
- `docs/mlfp-self-boot-readiness.md`: recorded the bounded symbolic lexer seed evidence and the remaining source-text/character/byte/collection/parser gaps.
- `docs/architecture.md`: recorded the lexer seed fixture ownership and no-overclaim boundaries.
- `docs/mlfp-language-reference.md`: clarified that the lexer seed is symbolic and not a source-text, ABI, linker, native, or self-hosting contract.

### Tests
- `test/ProgramCompilerSeedSpec.hs`: verifies package discovery/order for the seed modules, package checking, interpreter output, and public CLI output for `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`.
- `cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract`: PASS, printed `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`.
- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'`: PASS (`Finished in 46.1553 seconds`, `2 examples, 0 failures`).
- `git diff --check`: PASS (no output).
- `cabal build all`: PASS.
- `cabal test`: PASS (`Finished in 430.9037 seconds`, `2562 examples, 0 failures`; `Test suite mlf2-test: PASS`; `1 of 1 test suites (1 of 1 test cases) passed`).
- `./scripts/thesis-conformance-gate.sh`: PASS (`[thesis-gate] PASS: thesis conformance anchors are green`).

### Notes
- The seed deliberately uses symbolic input and seed-owned monomorphic streams instead of raw `String`, bytes, characters, substrings, maps/sets, parser combinators, or general collection helpers.
- A broader recursive/generic stream shape and pure ADT rendering exposed existing frontend/runtime limitations during implementation; this round keeps the lexer proof bounded and records those as future gaps rather than expanding scope.
- Parser seed, AST contract, backend/native behavior, package manager, ABI, linker, and driver work remain out of scope.
