### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added the owner-local bounded application argument helper family rooted at `parseBoundedApplicationArguments`, with focused one-argument and two-argument entry points for simple atom callers. The helper keeps left-associative accumulation through `finishApplicationExpression`, passes the atom parser as the parser boundary, and stops with the accumulated value when no next argument is present.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: routed `parseApplicationOrAtomExpression`, `parseApplicationOrSimpleAtomExpression`, and `parseApplicationOrTwoSimpleAtomExpression` through the bounded helper family, then removed the migrated numbered general and simple application continuation definitions.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: left the nested parenthesized depth family structure in place; only direct call sites that previously targeted the removed simple continuation now call the new single-argument entry point. That depth family remains the next candidate bounded surface.
- `test/ProgramParserParitySpec.hs`: added static coverage proving the bounded application helper family exists, the general and simple application paths use it, and the migrated numbered helper names are absent from parser-library sources.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies helper presence/use and absence of migrated numbered application helper definitions.
- `git diff --check`: passed.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded application arguments"'`: passed; 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: passed; 71 examples, 0 failures; finished in 9638.5404 seconds. Log: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log`.
- Focused static guard over parser-library/spec: passed; helper/call-site/spec evidence present and migrated numbered application helpers absent from parser-library source.
- Excluded-phrase guard over final changed parser-library/spec/notes lines: passed.

### Notes
The general entry point keeps the previous seven-argument budget by starting at `parseBoundedApplicationArgumentsMoreOrDone6`. The simple entry points reuse the same bounded continuation family at budgets one and two, preserving the existing direct caller behavior without introducing a wider parser framework.
