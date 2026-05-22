# Review: round-312

Decision: APPROVED

Reviewed integrated result for `item-312-parser-library-case-pattern-extension`
against the round plan and the active rev-004 verification contract.

## Scope Review

- The two new fixture roots are thin source/evidence harnesses. Each
  `ParserParityFixture.mlfp` exports only `sourceFile` and `sourceText`, and
  each `Main.mlfp` calls the shared
  `renderParserParityProjectionFromSourceText` entrypoint.
- The case-expression evidence enters through the shared parser-library front
  door:
  `renderParserParityProjectionFromSourceText -> parseCompleteModule sourceText
  -> tokenizeCompleteModule -> parseTokens`.
- The parser library still has parser-combinator behavior through `Parser`,
  `parserBind`, `parserMap`, `parserChoice`, span capture, diagnostic labels,
  case-branch-arrow labeling, and `parserStateAtEnd` EOF/end-state checks.
- Static audits found no fixture-authored `ParserSourceInput`/
  `ParserSourceSymbol` streams, no fixture-owned parser entrypoint, no exact
  source recognizer, and no static token-stream shortcut shape from the plan.
- The parser remains bounded parser-parity evidence for selected fixtures. The
  round does not claim checker, backend, platform, driver, proof, full parser
  parity, or full self-boot completion.

Residual note: the parser-library grammar is still selected-corpus bounded, and
the new case branches are visibly shaped around the two selected case fixtures.
I am not treating that as a blocker for this round because the implementation
keeps those branches inside the shared parser library, consumes source text via
the shared lexer/token stream, sequences through `Parser` combinators, and does
not move grammar or tokens into fixture roots.

## Verification

- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns/"'`
  - Result: passed, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed case branch arrows through public run-program/"'`
  - Result: passed, 1 example, 0 failures.
- `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  - Result: passed, 19 examples, 0 failures.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the basic Bool parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the import-exposing Bool parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the value-definition-list Int/reference parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the let/lambda/application parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the typed-annotation parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the data-declaration/constructor-span parser projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-constructor-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the `case Succ Zero of { Zero -> 0; Succ _ -> 1 }` projection.
- `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-nested-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  - Result: passed; printed the `case Succ (Succ Zero) of { Succ Zero -> false; Succ (Succ n) -> true; _ -> false }` projection.
- `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span'`
  - Result: no matches; `rg` exited 1 because no banned fixture-root shape was found.
- `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens)|case tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  - Result: no matches; `rg` exited 1 because no exact-source/static-token shortcut shape was found.
- `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput|ParserParityParser.mlfp' test/programs/compiler-parser-parity`
  - Result: no matches; `rg` exited 1 because no fixture-owned parser entrypoint or public `ParserSourceInput` parser front door was found.
- `rg -n 'def renderParserParityProjectionFromSourceText : String -> String -> String|def parseCompleteModule : String -> ParserResult|def tokenizeCompleteModule : String -> LexerResult|def initialSourceCursor : String -> SourceCursor|data Parser a|def parserBind|def parserMap|def parserChoice|def captureSpan|def diagnosticLabel|def labelCaseBranchArrow|def parserStateAtEnd' test/programs/compiler-parser-parity/parser-library`
  - Result: found the expected shared source-text parser front door, lexer cursor entrypoint, parser type, combinator operations, diagnostic labeling, case-arrow label, and end-state check.
- `rg -n 'def sourceFile : String|def sourceText : String|renderParserParityProjectionFromSourceText' test/programs/compiler-parser-parity/case-expression-constructor-patterns test/programs/compiler-parser-parity/case-expression-nested-patterns`
  - Result: found only the intended thin fixture exports and shared projection calls in the two new fixture roots.
- `rg -n 'parsePattern|parseCaseAlternative|parseCaseAlt|parseCaseBranch|parseCaseConstructorType|parseCaseNestedType|completeModuleKey "case-' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  - Result: showed no generic `parsePattern`/`parseCaseAlternative` abstraction yet, and showed the two bounded shared-library case branches. This is recorded as boundedness, not a blocking shortcut, because the parser remains shared and source-text driven.
- `git diff --check`
  - Result: passed.
- `cabal build all`
  - Result: passed.
- `cabal test`
  - Result: passed, 2633 examples, 0 failures.
- `./scripts/thesis-conformance-gate.sh`
  - Result: passed; `[thesis-gate] PASS: thesis conformance anchors are green`.

Validation generated absolute-path churn in
`runtime/mlfp_io/target/release/libmlfp_io.d`; I restored that generated file
to its pre-validation content. `orchestrator/state.json` was already
controller-owned dirty state for the active review round and was not edited.

## Required Changes

None.

## Closeout Classification

Status-only. The approved result should add a compact completion pointer under
`milestone-4-completion`; milestone 4 remains `in-progress` and no semantic
roadmap update is required.
