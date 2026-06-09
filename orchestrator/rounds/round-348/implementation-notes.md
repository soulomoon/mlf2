### Changes Made

- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`: added `ValueConstructorRows : String -> ParserValue` as the parser-library-owned constructor-row payload.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added constructor-row accumulator helpers for empty state, appending one rendered constructor row, and extracting accumulated rows into final data-row projection text. Exact four-, five-, and nine-constructor data-row families now use the accumulator; single, two, derived, nat-recursive, and expr-recursive families are unchanged.
- `test/ProgramParserParitySpec.hs`: added focused static coverage for payload/helper presence, exact four/five/nine accumulator use, and absence of removed tuple-threading continuation/finish helper names.

### Tests

- `git diff --check`: PASS.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: PASS; 75 examples, 0 failures.
- Focused static guard over `ParserParityParserCombinator.mlfp`, `ParserParityParser.mlfp`, and `ProgramParserParitySpec.hs`: PASS; required payload/helper/use phrases present and migrated continuation/finish helper names absent from the parser library.
- Shortcut/overclaim guard over changed parser-library/spec/docs lines: PASS.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`: PASS; 2722 examples, 0 failures.

### Notes

- The migrated exact-count parser families preserve source-type parsing, parsing order, separator/final semicolon behavior, source-span rendering, data-row span calculation, projection text shape, diagnostics, aggregate parser-parity outputs, canonical parser behavior, checker policy, and conformance fixture meaning.
- Removed migrated tuple-threading continuation/finish helper aliases without adding compatibility aliases.
- Non-claims: this round does not claim full parser parity, compiler-package progress, platform/proof progress, native/backend progress, package-manager/linker progress, or self-boot progress.
- `./scripts/thesis-conformance-gate.sh` was not run because this stayed within the parser-library ergonomics substrate and did not change thesis-facing semantics or self-boot/proof/package/platform/native/backend claims.
