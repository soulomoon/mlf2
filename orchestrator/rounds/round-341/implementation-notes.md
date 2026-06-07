### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`: added the owner-local `ParserExpectation` substrate, `parserDiagnosticForExpectation`, `parserFailExpected`, `parserFailExpectedAtCurrent`, and `labelExpected`; removed duplicated per-expectation current-token fail helpers and label helper exports.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: migrated parser-library call sites from the removed helper names to `ParserExpectation` constructors through `parserFailExpected`, `parserFailExpectedAtCurrent`, and `labelExpected`.
- `test/ProgramParserParitySpec.hs`: added focused static coverage requiring the expectation substrate and migrated call sites, and rejecting reintroduced compatibility aliases.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies the new substrate is present, parser call sites use it, and removed relabeling/current-token helper aliases are absent from the parser-library source.
- `git diff --check`: passed.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: passed; 68 examples, 0 failures. Log: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log`.
- Focused static guard: `rg` over `ParserParityParserCombinator.mlfp`, `ParserParityParser.mlfp`, and `ProgramParserParitySpec.hs` found `ParserExpectation`, `parserDiagnosticForExpectation`, `parserFailExpectedAtCurrent`, `labelExpected`, and migrated expectation call sites.
- Removed-alias guard: `rg` found no `parserFailExpected...AtCurrent` compatibility helper names and no removed `label...` compatibility aliases in the parser-library modules.
- Shortcut/overclaim guard: added-line scan over changed parser-library/spec/docs surfaces found no fixture-name shortcuts, pre-rendered projections, static negative evidence, canonical-parser bypasses, parser-private compiler-seed shortcuts, compiler-package/platform/proof hooks, native/backend/package-manager/linker claims, self-boot claims, or full parser parity claims.

### Notes
No repo-facing docs were updated because this round only adds bounded parser-library diagnostic expectation substrate. The change preserves the existing parser-choice behavior: retryable unexpected-source failures can be relabeled through `labelExpected`, while committed expected diagnostics keep their original diagnostic and remain non-backtracking.
