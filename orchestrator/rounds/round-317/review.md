# Review: round-317

## Checks Run

- `timeout 900 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to qualified imports and references/"'`
  - Result: passed. The focused matcher completed with `1 example, 0 failures`.
- `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  - Result: timeout exit 124 after 3600 seconds. Before the timeout it had already passed the shared lexer, case-expression, typeclass/instance, higher-kinded/constraint, and closed type-family/type-level parser-parity matchers. I treated this as incomplete evidence and relied on the later full `timeout 10800 cabal test`, which completed and passed the full parser-parity group including the round-317 qualified-import matcher.
- Direct `run-program` smokes through `test/programs/compiler-parser-parity/parser-library`:
  - Command shape: `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/<fixture> --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed for all carried parser-parity fixtures and the two new fixtures. Strict smoke output directory: `/tmp/round317-parser-smokes-strict.MTKQIv`.
  - Passed fixtures: `basic-module-def-bool` (3 lines), `import-exposing-def-bool` (5), `value-def-list-int-ref` (6), `let-lambda-application` (5), `typed-annotation-types` (5), `data-declaration-constructor-spans` (7), `case-expression-constructor-patterns` (7), `case-expression-nested-patterns` (7), `typeclass-deriving-method` (12), `typeclass-instance-nullary-method` (16), `higher-kinded-class-data-params` (7), `multiparam-superclass-fundep` (7), `type-family-kind-lambda` (5), `type-family-apply-annotation` (5), `gadt-result-constructor-spans` (13), `existential-constructor-forall` (16), `qualified-import-alias-references` (15), and `qualified-import-alias-only` (5).
- Thin fixture audit:
  - `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span|higherKinded.*Span|fundep.*Span|constraint.*Span|typeFamily.*Span|family.*Span|typeLevel.*Span|gadt.*Span|existential.*Span|constructorForall.*Span|qualified.*Span|alias.*Span|importAlias.*Span'`
  - Result: no matches.
- Exact-source/static-token shortcut audit:
  - `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|HigherKindedTokens|ConstraintTokens|FundepTokens|TypeFamilyTokens|FamilyTokens|GadtTokens|ExistentialTokens|QualifiedImportTokens|AliasOnlyTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens|higherKindedTokens|constraintTokens|fundepTokens|typeFamilyTokens|familyTokens|gadtTokens|existentialTokens|qualifiedImportTokens|aliasOnlyTokens)|case tokens|class tokens|instance tokens|higher-kinded tokens|constraint tokens|fundep tokens|type-family tokens|family tokens|gadt tokens|existential tokens|qualified-import tokens|alias-only tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  - Result: no matches.
- Rejected-shape shortcut audit:
  - `rg -n 'parseQualifiedImportAliasModule|parseQualifiedAliasOnlyModule|completeModuleKey "qualified-import-alias-references"|completeModuleKey "qualified-import-alias-only"|moduleKey "qualified-import-alias-references"|moduleKey "qualified-import-alias-only"' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  - Result: no matches.
- Shared parser architecture audit:
  - `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput|ParserParityParser.mlfp' test/programs/compiler-parser-parity`
  - Result: no matches.
- `git diff --check`
  - Result: passed before writing review artifacts.
- `timeout 7200 cabal build all`
  - Result: passed.
- `timeout 10800 cabal test`
  - Result: passed with `2639 examples, 0 failures`. The parser-parity group passed inside the full suite, including `shared parser-owned .mlfp parser extends source-text grammar to qualified imports and references`.
- `timeout 7200 ./scripts/thesis-conformance-gate.sh`
  - Result: passed with `[thesis-gate] PASS: thesis conformance anchors are green`.
- Generated churn cleanup:
  - `runtime/mlfp_io/target/release/libmlfp_io.d` was modified by validation and was restored before finalizing this review.

## Plan Compliance

- Approved: the round extends the shared parser-library source-text path, not a test-owned parser. `ParserParityParser.mlfp` still routes through `parseCompleteModule`, tokenization, parser state, and parser-combinator/monadic helpers.
- Approved: the new positive fixtures `qualified-import-alias-references` and `qualified-import-alias-only` are thin roots. Each fixture keeps the source text in `ParserParityFixture.mlfp` and delegates parsing/rendering to the shared parser library.
- Approved: the round covers qualified import aliases and qualified value/type/constructor/class/method references in source-text parser parity. The new focused matcher compares canonical expected projections and shared parser output for both positive fixtures.
- Approved: malformed import alias behavior is represented by the planned `expected-import-alias` diagnostic and is exercised by the focused matcher.
- Approved: static audits found none of the rejected shortcut shapes: `parseQualifiedImportAliasModule`, `parseQualifiedAliasOnlyModule`, fixture-key `completeModuleKey`/`moduleKey` shortcuts for the new fixtures, exact source matching, static token streams, or equivalent fixture-key recognizers.
- Approved: docs and readiness notes describe a bounded parser-parity slice and do not claim checker, backend, driver, platform, or self-boot completion.

## Decision

APPROVED

## Evidence

The implementation satisfies the assigned round scope. The parser-library path now accepts the two new qualified import/reference fixture programs from source text, produces the expected canonical projections, and emits the planned malformed alias diagnostic. The fixtures remain thin source roots, and the parser code continues to use parser-combinator/parser-monad architecture rather than per-fixture parsers.

The focused matcher, strict direct smokes for all carried fixtures plus the two new fixtures, `git diff --check`, `cabal build all`, full `cabal test`, and the thesis conformance gate passed. The only incomplete check was the broad parser-parity matcher under its 3600-second timeout; full `cabal test` subsequently completed and passed the same parser-parity examples, including the round-317 matcher.
