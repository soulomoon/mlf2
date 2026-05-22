# Review: round-311

## Checks Run

- Command: `git status --short --branch`
  Result: PASS. Worktree is on `orchestrator/round-311-next-parser-parity-slice`; pre-existing controller state diff was left untouched. Validation-generated `runtime/mlfp_io/target/release/libmlfp_io.d` drift was restored after the gates.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser lexes carried fixtures from source text before grammar parsing/"'`
  Result: PASS. `1 example, 0 failures` in 98.9767s.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: PASS. `17 examples, 0 failures` in 812.0609s.
- Command: `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan'`
  Result: PASS. No matches; fixture roots no longer author parser input streams or parser-library span constants.
- Command: `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens)' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity`
  Result: PASS. No matches; parser-library and fixture code contain no exact-source or static-token success shortcut shapes from the recovery constraints.
- Command: `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput' test/programs/compiler-parser-parity/parser-library`
  Result: PASS. No matches; normal public parser entrypoints do not take `ParserSourceInput`.
- Command: `rg -n 'sourceContains|stringIndexOf.*sourceText|tokenInputBasicMissingEquals|tokenInputData|LexerOk basicModuleTokens|LexerOk dataDeclarationTokens|case tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity`
  Result: PASS. No matches for additional shortcut and stale token-input shapes.
- Command: `rg -n 'def tokenizeCompleteModule : String -> LexerResult|initialSourceCursor sourceText|parseCompleteModule : String -> ParserResult|renderParserParityProjectionFromSourceText|data Parser a|def parserBind|def parserMap|def parserChoice|def captureSpan|def diagnosticLabel|parserStateAtEnd state' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity/*/Main.mlfp`
  Result: PASS. Source-text lexer/parser entrypoints and parser-combinator functions are present; all six fixture `Main.mlfp` files call `renderParserParityProjectionFromSourceText sourceFile sourceText`.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  Result: PASS. Printed the expected basic Bool parser projection.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  Result: PASS. Printed the expected import-exposing parser projection.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  Result: PASS. Printed the expected value-definition-list parser projection.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  Result: PASS. Printed the expected let/lambda/application parser projection.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  Result: PASS. Printed the expected typed-annotation parser projection.
- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
  Result: PASS. Printed the expected data-declaration parser projection.
- Command: `git diff --check`
  Result: PASS.
- Command: `cabal build all`
  Result: PASS.
- Command: `cabal test`
  Result: PASS. `2631 examples, 0 failures` in 1807.2277s.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: PASS. Final output included `[thesis-gate] PASS: thesis conformance anchors are green`.

## Plan Compliance

- Step 1, focused source-text front-door matcher: met. The required matcher passes through public `run-program` and proves at least the basic Bool and data-declaration fixtures enter from source text before grammar parsing.
- Step 2, shared parser-library source-text front door: met. `ParserParityLexer.tokenizeCompleteModule : String -> LexerResult` starts from `initialSourceCursor sourceText` and scans source text into the parser-library internal token/input representation.
- Step 3, preserve parser-combinator grammar shape: met. The parser library still defines parser-owned `Parser a`, `parserBind`, `parserMap`, `parserChoice`, `captureSpan`, `diagnosticLabel`, token expectation, and EOF/end-state checking; grammar functions compose through that layer.
- Step 4, convert carried positive fixture roots: met. All six carried fixture roots expose `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText`.
- Step 5, convert negative evidence to source text: met. The parser-parity group includes the malformed import, value-definition, let, annotation, data-declaration, and tokenizer mismatch cases through shared source-text parser-library paths.
- Step 6, remove fixture-authored token/source-symbol streams: met. Static fixture-root audits found no `ParserSourceInput`, `ParserSourceSymbol`, `SourceSymbol`, `sourceInputCons`, or parser-library span constants in fixture `ParserParityFixture.mlfp` files.
- Step 7, bounded documentation: met. `implementation_notes.md` records the round-311 source-text front-door scope and explicitly avoids full parser parity, checker/backend, driver, platform, proof, and self-boot claims.
- Step 8, validation: met. Focused matcher, parser-parity group, direct package smokes, static audits, diff hygiene, full Cabal build/test, and thesis conformance gate all passed.

## Decision

**APPROVED.**

## Evidence

The integrated result satisfies the recovery constraints for item-311. Fixture roots provide source identity and exact source text only; the normal success path enters through `renderParserParityProjectionFromSourceText sourceFile sourceText`, `parseCompleteModule sourceText`, `tokenizeCompleteModule sourceText`, the parser-owned lexer, and the shared grammar parser. I found no fixture-authored `ParserSourceInput` / `SourceSymbol` streams, no exact full-source recognizer, no static fixture token-stream success path, and no one-parser-per-test fixture parser.

The parser library still has parser-monad behavior in the required bounded sense. It owns `Parser a`, parser state, success/error steps, `runParser`, explicit bind/map/choice combinators, span capture, diagnostic labeling, token expectation, and end-state checking. The carried fixture grammar functions are still composed through these parser functions rather than through fixture-level case trees.

Closeout classification: status-only. This round adds an approved completion pointer for `item-311-parser-source-text-front-door` under milestone 4, with no milestone status change and no semantic roadmap update. Milestone 4 remains `in-progress`.
