### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added the first-order `ProjectionRowParser` / `ProjectionRowsFinish` selector substrate and the shared `parseBoundedDelimitedProjectionRows*` bounded comma-list helper family; migrated export and import exposing projection lists onto it while preserving the existing final-budget close path.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: removed the migrated export/import numbered projection-list ladders and append continuations instead of leaving compatibility aliases.
- `test/ProgramParserParitySpec.hs`: added static coverage requiring the bounded projection-row substrate and export/import call sites, and rejecting reintroduced migrated projection-list helper names in parser-library source.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies the shared bounded projection-row helper exists, export/import projection list parsers use it, and removed projection-list ladder names are absent from parser-library modules.
- `git diff --check`: passed.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: passed; 69 examples, 0 failures; finished in 9513.0564 seconds. Log: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log`.
- Focused static guard: `rg` over `ParserParityParser.mlfp`, `ParserParityParserCombinator.mlfp`, and `ProgramParserParitySpec.hs` found `ProjectionRowParser`, `ProjectionRowsFinish`, `parseBoundedDelimitedProjectionRows`, the export/import bounded-list call sites, and the new Hspec guard phrases; the same guard found no migrated numbered projection-list helper aliases in parser-library source.
- Shortcut/claim guard: added-line scan over changed parser-library and spec lines passed for the plan's excluded shortcut and claim categories.

### Notes
An initial higher-order helper shape typechecked poorly in this parser-library path and failed a single shared-parser example with `WitnessNormalizationError`. The final implementation uses first-order selector data instead, keeping the reusable bounded-list substrate without widening into a generic combinator API.
