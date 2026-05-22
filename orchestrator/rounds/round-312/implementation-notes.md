### Changes Made

- Added the public parser-parity matcher
  `MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns`.
- Added canonical parser-parity conformance sources and expected projections for:
  - `case-expression-constructor-patterns`
  - `case-expression-nested-patterns`
- Added thin compiler-parser-parity fixture roots for both new fixtures. The fixture roots only provide `sourceFile` and `sourceText`, then call `renderParserParityProjectionFromSourceText`.
- Extended the shared parser-parity `.mlfp` parser library for the round-312 case-expression slice:
  - lexer tokens for `case`, `of`, `false`, wildcard `_`, identifier `n`, integer `0`, and existing punctuation/constructors needed by the fixtures;
  - parser-combinator label for case branch arrows;
  - parser paths for constructor application scrutinees, case/of blocks, constructor patterns, wildcard patterns, nested constructor patterns, parentheses, branch separators, EOF/end-state completion, and malformed branch-arrow diagnostics;
  - projection rendering for the two new case fixtures.
- Extended the Haskell canonical projection renderer used by the public parity spec to render `case` alternatives and patterns for the expected fixture comparison.
- Added a public malformed case-branch-arrow negative matcher through `run-program`.
- Updated bounded progress docs/changelog/readiness notes without claiming full parser parity, full compiler self-boot, or unrelated milestones.
- Restored validation-generated churn in `runtime/mlfp_io/target/release/libmlfp_io.d` after validation.

### Tests

- RED evidence:
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns/"'`
  - Result: failed as expected before implementation. The canonical projection matched the committed expected file, while the shared `.mlfp` parser returned `Right "parser-error\n"` for the new case fixture.
- Focused GREEN evidence:
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns/"'`
  - Result: passed, 1 example, 0 failures.
- Negative case-arrow diagnostic:
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed case branch arrows through public run-program/"'`
  - Result: passed, 1 example, 0 failures.
- Parser-parity group:
  - `perl -e 'alarm shift; exec @ARGV' 1800 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  - Result: passed, 19 examples, 0 failures.
- Direct fixture smokes:
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-constructor-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  - `perl -e 'alarm shift; exec @ARGV' 240 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-nested-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: all passed and printed parser projections.
- Thin fixture audit:
  - `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span'`
  - Result: no matches.
- Shortcut/static-token audit:
  - `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens)|case tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  - Result: no matches.
- Shared parser architecture audit:
  - `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput|ParserParityParser.mlfp' test/programs/compiler-parser-parity`
  - Result: no matches.
- Whitespace audit:
  - `git diff --check`
  - Result: passed.
- Build:
  - `perl -e 'alarm shift; exec @ARGV' 1800 cabal build all`
  - Result: passed.
- Full test suite:
  - `perl -e 'alarm shift; exec @ARGV' 3600 cabal test`
  - Result: passed, 2633 examples, 0 failures.
- Thesis gate:
  - `perl -e 'alarm shift; exec @ARGV' 1800 ./scripts/thesis-conformance-gate.sh`
  - Result: passed.

### Notes

- The implementation stays inside the shared parser-parity parser-library front door. The new fixture roots do not author `ParserSourceInput`, `SourceSymbol`, `SourceInputCons`, parser-library span constants, fixture-owned token streams, or fixture-specific parser logic.
- The parser still completes through the shared parser-combinator flow and EOF/end-state checks before projection rendering.
- The malformed case branch evidence enters through source text and the shared lexer/parser path.
- No blockers remain. The round is ready for review.
- `orchestrator/state.json` was dirty at handoff time and was not edited for this implementation.
