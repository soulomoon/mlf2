### Changes Made
- `test/programs/compiler-seed/frontend-contract/SeedAst.mlfp`: added the bounded parser AST contract for the accepted `def main = true` shape.
- `test/programs/compiler-seed/frontend-contract/SeedParser.mlfp`: added parser diagnostic/result/evidence ADTs, a bounded parser over `SeedTokenStream`, one accepted parser path from the lexer positive token stream, one rejected missing-equals token stream, and a `.mlfp` renderer that checks the expected AST and diagnostic span.
- `test/programs/compiler-seed/frontend-contract/Main.mlfp`: sequenced lexer and parser evidence through `Prelude.bind` so the interpreter/CLI prints both asserted lines from `.mlfp` code.
- `test/ProgramCompilerSeedSpec.hs`: updated package graph/source assertions and exact interpreter/CLI output assertions for the added AST/parser seed modules.
- `docs/mlfp-self-boot-readiness.md`: recorded the bounded parser/AST evidence and kept source-text parser, checker, backend, package manager, linker, native, and self-hosting claims out of scope.
- `docs/architecture.md`: recorded `SeedAst`/`SeedParser` as the parser seed owner beside the existing compiler-seed lexer fixture.
- `docs/mlfp-language-reference.md`: clarified that the compiler seed has only a bounded token-stream parser, not a source-text parser or native/backend guarantee.

### Tests
- `test/ProgramCompilerSeedSpec.hs`: verifies package discovery/order, package checking, interpreter output, and public CLI output for lexer and parser evidence.
- `cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'`: PASS (`Finished in 77.0140 seconds`, `2 examples, 0 failures`).
- `cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract`: PASS, printed:
  - `lexer-positive:def-main-equals-true;lexer-negative:unknown@span-unknown-symbol`
  - `parser-positive:ast-def-main-bool-true;parser-negative:expected-equals@span-bool-true`
- `git diff --check`: PASS (no output).
- `cabal build all`: PASS.
- `cabal test`: PASS (`Finished in 489.0299 seconds`, `2562 examples, 0 failures`; `Test suite mlf2-test: PASS`; `1 of 1 test suites (1 of 1 test cases) passed.`).
- `./scripts/thesis-conformance-gate.sh`: PASS (`[thesis-obligations] PASS: all obligations are mapped and green`; `[thesis-claims] PASS: all validations green`; `[thesis-gate] PASS: thesis conformance anchors are green`).

### Notes
- The parser consumes `SeedTokenStream` from `.mlfp` seed code; no Haskell helper constructs parser AST or diagnostics for the assertions.
- The negative parser path deliberately uses a bounded malformed token stream with `TokenBoolLiteral SpanBoolTrue BoolLiteralTrue` where `TokenEquals` is expected, producing `ExpectedEquals` at `SpanBoolTrue`.
- Milestone-4 primitive/stdlib gap-budget work, milestone-5 backend/native classification, source-text parsing, checker handoff, package manager, ABI, linker, and driver work remain out of scope.
