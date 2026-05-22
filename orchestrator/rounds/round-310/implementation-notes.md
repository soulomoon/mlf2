# Round 310 Implementation Notes

## Changes Made

- Added a shared parser-owned `.mlfp` parser library under `test/programs/compiler-parser-parity/parser-library/`.
- Routed `basic-module-def-bool`, `import-exposing-def-bool`, `value-def-list-int-ref`, `let-lambda-application`, `typed-annotation-types`, and `data-declaration-constructor-spans` through the shared `ParserParityParser.renderParserParityProjection` entrypoint with `--search-path`.
- Replaced per-fixture parser/AST/source/token support modules with small fixture source modules.
- Retry fix after review rejection: removed the positive fixture-level token-stream constructors (`BasicModuleTokens`, `ImportBoolTokens`, `ValueDefListTokens`, `LetLambdaApplicationTokens`, `TypedAnnotationTypesTokens`, and `DataDeclarationTokens`) and the parser switches over those complete constructors.
- Second retry fix after review rejection: removed fixed-offset `stringSlice source <offset> <length>` probes from the success path and made `ParserParityLexer` emit ordinary `ParserSourceSymbol` token sequences consumed by parser state.
- Third retry fix after review rejection: made `parseCompleteModule`/`parseTokens` return `ParserOk` only after the shared grammar consumes a complete carried module and `parserStateAtEnd` reports EOF.
- Replaced early projection-key returns with full grammar suffixes for the carried fixtures: Bool definitions now require `=`, expression, semicolon, and close brace; value-list modules parse the second definition and reference suffix; let/lambda/application modules parse the lambda, `in`, application, semicolon, and close brace; typed annotation modules parse annotation/lambda/application tails; data declarations parse constructor bodies plus the following `main` definition.
- Defined parser-local `.mlfp` type classes first in `ParserParityParserCombinator.mlfp`: `Functor`, `Applicative`, and `Monad`, with parser-shaped method signatures. The parser library exports those class names for parser-parity scope only.
- Changed the parser combinator shape to `data Parser a = Parser : (ParserState -> ParserStep) -> Parser a`, with parser-owned `runParser`, `parserPure`, `parserFail`, `parserBind`, `parserMap`, `parserChoice`, `captureSpan`, `diagnosticLabel`, and token expectation helpers.
- Made `ParserParityParser` compose the carried module/import/definition/type/expression/data-declaration grammar through `Parser a` state sequencing instead of selecting projections through complete fixture tokens or fixed source offsets.
- Removed exact-source token-stream recognition from the normal success path; source position and projection selection now come from consumed token cursor state and parsed module shape.
- Drove parser and lexer negative evidence through the same shared `tokenizeCompleteModule` / `parseCompleteModule` path using malformed `ParserSourceInput` values. `SourceUnknown` now produces `LexerError` through lexer validation instead of an unconditional `LexerOk`.
- Preserved parser projection outputs and negative evidence behavior through dynamically rendered shared parser diagnostics.
- Updated parser-parity tests and adjacent documentation/changelog notes for the shared-library shape.
- Aligned touched notes/docs with the actual parser-parity fixture contract: fixture roots provide source file identity and `ParserSourceInput` token/source-symbol streams, not source text.
- Fixed an ADT inference regression where parameterized constructor applications could leak identity substitutions and report a mismatch such as `TCArgumentMismatch (TVar "a") (TVar "a1")`; added focused program coverage for `Box value : Box a`.

## Tests

- RED: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint/"'`
  - Failed before implementation because `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` did not exist.
- GREEN: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint/"'`
  - Passed: `1 example, 0 failures`.
- RETRY RED: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser composes grammar without fixture-level token streams/"'`
  - Failed before the retry fix because the shared library still contained the banned complete-fixture token constructors and `LexerOk` values.
- RETRY GREEN: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser composes grammar without fixture-level token streams/"'`
  - Passed: `1 example, 0 failures`.
- SECOND RETRY RED: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser consumes tokens through parser-state grammar combinators/"'`
  - Failed before the second retry because the parser still contained fixed-offset source probes and the combinator library only had `parserPure`/`parserFail`.
- SECOND RETRY GREEN: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser consumes tokens through parser-state grammar combinators/"'`
  - Passed: `1 example, 0 failures`.
- THIRD RETRY RED: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'`
  - Failed before the third retry because the shared parser still had early projection-key success phrases and static negative evidence paths.
- THIRD RETRY GREEN: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'`
  - Passed: `1 example, 0 failures`.
- Shared entrypoint guard after third retry: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint/"'`
  - Passed: `1 example, 0 failures`.
- Focused structural checks after third retry: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser composes grammar without fixture-level token streams/" --match "/MLF.Program parser parity/shared parser-owned .mlfp parser consumes tokens through parser-state grammar combinators/" --match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'`
  - Passed: `3 examples, 0 failures`.
- ADT inference RED: the focused `Box value : Box a` repro failed before the inference fix with `TCArgumentMismatch (TVar "a") (TVar "a1")`.
- ADT inference GREEN: `cabal test mlf2-test --test-options='--match "/MLF.Program diagnostics/checks nullary constructors from wide ADTs without leaking handler result polymorphism/" --match "/MLF.Program diagnostics/checks parameterized constructor applications without leaking identity substitutions/"'`
  - Passed: `2 examples, 0 failures`.
- Parser parity group: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  - Passed: `16 examples, 0 failures`.
- Direct carried-fixture smoke loop:
  - `cabal run mlf2 -- run-program "test/programs/compiler-parser-parity/$fixture" --search-path test/programs/compiler-parser-parity/parser-library`
  - Passed for all six carried fixtures.
- Fixture-token classifier audit: `rg -n 'case tokens|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens)' test/programs/compiler-parser-parity/parser-library`
  - Passed with no matches.
- Fixed-offset source-probe audit: `rg -n 'stringSlice source [0-9]|stringIndexOf \(stringSlice source|stringIndexOf source .*SourceText|\b[a-z][A-Za-z0-9_]*SourceText\b|renderParserParityEvidence' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  - Passed with no matches.
- Complete-syntax/dynamic-diagnostics audit: `rg -n 'parserStateAtEnd|render[A-Za-z]*NegativeEvidence|parseCompleteModule|parseTokens|moduleKey|LexerError|validateSourceInput' test/programs/compiler-parser-parity/parser-library`
  - Passed review inspection: `parserStateAtEnd` is used by `ParserParityParser.mlfp`, negative evidence calls shared parse/tokenize paths, and lexer validation can emit `LexerError`.
- Parser module audit: `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityParser.mlfp -print | sort`
  - Passed; only the shared parser-library entrypoint remains.
- Exact-source success-path audit: `rg -n '\b[a-z][A-Za-z0-9_]*SourceText\b|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :' test/programs/compiler-parser-parity`
  - Passed with no matches.
- Whitespace audit: `git diff --check`
  - Passed.
- Build gate: `cabal build all`
  - Passed.
- Full test gate: `cabal test`
  - Passed: `2630 examples, 0 failures`.
- Thesis gate: `./scripts/thesis-conformance-gate.sh`
  - Passed: `[thesis-gate] PASS: thesis conformance anchors are green`.

## Notes

- No checker, resolver, backend, driver, platform, proof, generic Prelude parser, or public monad API scope was added.
- `Parser` has a type parameter and parser-local monad-shaped sequencing helpers, but parser parity does not define a public Prelude parser or monad instance surface.
- Parser parity still uses parser-library-owned `ParserSourceInput` token/source-symbol fixtures, not source-text fixtures.
- The lexer validator intentionally scans the bounded parser-parity source prefix with small helper functions to avoid the `.mlfp` runtime recursive top-level lookup failure observed with a recursive validator during this retry. Parser-level unknown tokens and full-module grammar errors still flow through the shared parser diagnostics.
- `runtime/mlfp_io/target/release/libmlfp_io.d` churn from full validation was restored before handoff.
- `orchestrator/state.json` was not edited.
- No validation commands were intentionally skipped.
