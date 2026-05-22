# Round 313 Implementation Notes

## Change Summary

- Added the public parser-parity matcher for
  `shared parser-owned .mlfp parser extends source-text grammar to typeclass, deriving, and instance declarations`
  and confirmed it failed before the shared parser-library implementation
  accepted the new fixtures.
- Added committed conformance source/projection fixtures for
  `typeclass-deriving-method` and `typeclass-instance-nullary-method`.
- Added thin compiler-parser-parity fixture roots for both fixtures; each root
  exposes only `sourceFile` and `sourceText` and calls
  `renderParserParityProjectionFromSourceText` from the shared parser library.
- Extended the shared parser-library lexer, parser combinator diagnostics,
  parser paths, source spans, and projection renderer for the selected
  class/method/deriving/instance grammar slice.
- Added a public malformed instance-method-definition matcher that enters via
  source text and renders parser-owned
  `expected-instance-method-equals` evidence through
  `renderParserNegativeEvidenceFromSourceText`.
- Updated bounded parser-parity progress docs without claiming full parser
  parity, checker/backend work, driver/platform work, proof work, or self-boot
  completion.

## TDD Evidence

- RED: the named positive matcher compiled after the test/fixture additions but
  failed with `Right "parser-error\n"` from the shared `.mlfp` parser-library
  before typeclass/instance grammar support was implemented.
- GREEN:
  `perl -e 'alarm shift; exec @ARGV' 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to typeclass, deriving, and instance declarations/"'`
  passed in `198.0841 seconds` with `1 example, 0 failures`.
- GREEN:
  `perl -e 'alarm shift; exec @ARGV' 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed instance method definitions through public run-program/"'`
  passed in `99.0690 seconds` with `1 example, 0 failures`.

## Validation

- Direct parser-parity fixture smokes:
  `cabal run mlf2 -- run-program <fixture> --search-path test/programs/compiler-parser-parity/parser-library`
  passed for `basic-module-def-bool`, `import-exposing-def-bool`,
  `value-def-list-int-ref`, `let-lambda-application`,
  `typed-annotation-types`, `data-declaration-constructor-spans`,
  `case-expression-constructor-patterns`, `case-expression-nested-patterns`,
  `typeclass-deriving-method`, and `typeclass-instance-nullary-method`.
- Parser-parity group:
  `perl -e 'alarm shift; exec @ARGV' 2400 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  passed in `2169.8417 seconds` with `21 examples, 0 failures`.
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span'`
  produced no matches.
- Exact-source/static-token shortcut audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens)|case tokens|class tokens|instance tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  produced no matches.
- Shared parser architecture audit:
  `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput|ParserParityParser.mlfp' test/programs/compiler-parser-parity`
  produced no matches.
- `git diff --check` passed with no output.
- `perl -e 'alarm shift; exec @ARGV' 2400 cabal build all` passed.
- `perl -e 'alarm shift; exec @ARGV' 7200 cabal test` passed in
  `3216.0459 seconds` with `2635 examples, 0 failures`.
- `perl -e 'alarm shift; exec @ARGV' 1800 ./scripts/thesis-conformance-gate.sh`
  passed with `[thesis-gate] PASS: thesis conformance anchors are green`.
- Validation-generated churn in
  `runtime/mlfp_io/target/release/libmlfp_io.d` was restored after the gates.

## Blockers And Assumptions

- No implementation blocker is currently known.
- `orchestrator/state.json` was already modified by the controller state for
  round-313 and was left untouched.
