### Selected Extraction
- Milestone: Parser Seed And AST Contract
- Milestone id: `milestone-3`
- Direction id: `direction-3a-minimal-parser-seed`
- Extracted item id: `item-254-parser-seed-ast-contract`
- Roadmap id: `2026-05-17-01-mlfp-compiler-frontend-seed-roadmap`
- Roadmap revision: `rev-001`
- Roadmap dir: `orchestrator/roadmaps/2026-05-17-01-mlfp-compiler-frontend-seed-roadmap/rev-001`

### Goal
Add the first `.mlfp` parser seed on top of the merged lexer seed. The round should parse the existing bounded token stream for one tiny definition form, return a structured AST on success, return a parser diagnostic on a rejected token stream, and prove both paths through interpreter-run assertions.

### Approach
Keep the extraction serial and inside `milestone-3`. Reuse the existing compiler seed package at `test/programs/compiler-seed/frontend-contract/`, especially `SeedSource`, `SeedToken`, `SeedDiagnostic`, and `SeedLexer`. Do not add a real source-text parser, Haskell parser replacement, checker handoff, backend/native support, package manager, ABI, linker, driver, or broad standard-library work.

The parser should consume the lexer-owned `SeedTokenStream`, not raw `String`, bytes, or host-side token fixtures. A bounded contract is enough:
- add a parser-owned AST module such as `SeedAst` with one definition form, for example `def main = true`;
- add a parser-owned module such as `SeedParser` with `ParserResult`, `ParserDiagnosticKind`, `ParserDiagnostic`, positive and negative parser evidence, and a renderer used by `Main`;
- parse one accepted token stream from the lexer positive path into the AST;
- parse one deliberately malformed token stream, such as missing `TokenEquals` or ending after `TokenIdentifier`, into a diagnostic that carries a symbolic span/position;
- keep evidence values inspectable in `.mlfp` and assert exact rendered evidence from Haskell tests.

The round may adjust the seed module split if that keeps ownership clearer, but the parser result contract must remain tiny and explicit. If the current seed exposes support gaps, record them as future primitive/stdlib/parser-combinator gaps rather than widening this round.

### Steps
1. Inspect the current seed before editing: `test/programs/compiler-seed/frontend-contract/SeedSource.mlfp`, `SeedToken.mlfp`, `SeedDiagnostic.mlfp`, `SeedLexer.mlfp`, `Main.mlfp`, `test/ProgramCompilerSeedSpec.hs`, `docs/mlfp-self-boot-readiness.md`, `docs/mlfp-language-reference.md`, and `docs/architecture.md`.
2. Add `.mlfp` AST and parser-result data under the same compiler seed package. Keep exported constructors explicit and small: one definition AST shape, parser success, parser error, parser diagnostic kind, and parser diagnostic with symbolic span information.
3. Implement a minimal parser over `SeedTokenStream`. It should accept only the already-proven token sequence for the bounded definition form and reject at least one malformed stream with a structured diagnostic. Prefer simple case-analysis functions over general parser-combinator abstractions.
4. Add `.mlfp` parser evidence values and a render/classification function that returns one exact string only when both accepted and rejected parser paths match the expected AST/diagnostic. Avoid host-side helpers that manufacture AST or parser diagnostics outside `.mlfp`.
5. Update `Main.mlfp` and `ProgramCompilerSeedSpec` so the interpreter/CLI evidence includes parser seed output. The spec should still assert package discovery/order, check success, interpreter output, and CLI `run-program` output. If a new spec module is introduced, register it in both `mlf2.cabal` and `test/Main.hs`.
6. Update docs only as needed to record that the seed now has a parser/AST proof while preserving layer separation. The docs must still say this is not a source-text parser, checker, backend, package manager, ABI, linker, native guarantee, or self-hosting claim.
7. Review scope before validation: no milestone-4 primitive budget work, no milestone-5 backend/native classification work, no public facade widening unless forced by focused evidence, no duplicate package loader, no one-file compatibility path, and no smoke-only tests.

### Verification
Run focused validation first:

```sh
cabal test mlf2-test --test-options='--match=compiler-seed' --test-options='--fail-on=empty'
cabal run mlf2 -- run-program test/programs/compiler-seed/frontend-contract
```

Then run the behavior-changing gates:

```sh
git diff --check
cabal build all
cabal test
./scripts/thesis-conformance-gate.sh
```

Manual review should confirm the parser consumes the seed token stream, at least one accepted form and one rejected form have asserted evidence, AST and diagnostic values are produced by `.mlfp` code, docs do not overclaim beyond the bounded parser seed, and unsupported native/backend behavior remains fail-closed and out of scope.

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this plan. They are the machine authority for lineage and worker scheduling.
