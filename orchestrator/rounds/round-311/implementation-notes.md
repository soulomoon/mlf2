### Changes Made
- `test/ProgramParserParitySpec.hs`: kept the public focused matcher for
  source-text front-door evidence and the source-text negative-evidence package
  shape from prior WIP.
- `test/programs/compiler-parser-parity/*/Main.mlfp`: fixture roots now call
  `renderParserParityProjectionFromSourceText sourceFile sourceText` through
  the shared parser library.
- `test/programs/compiler-parser-parity/*/ParserParityFixture.mlfp`: carried
  fixture modules expose only `sourceFile` and `sourceText`; they no longer
  hand-author `ParserSourceInput`, `SourceSymbol`, `sourceInputCons`, or
  parser-library span constants.
- `test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp`:
  replaced the exact full-source recognizer and prebuilt token-stream WIP with
  a shared source-text scanner. The scanner uses `initialSourceCursor`, bounded
  non-recursive scan/reverse steps, trivia skipping, and token recognition over
  the carried token set. It intentionally avoids recursive top-level lexer
  bindings because direct `run-program` validation rejected the earlier
  `lexRemaining -> lexNonEmpty -> lexRemaining` shape.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  routes `parseCompleteModule : String -> ParserResult` through the shared
  lexer, parser combinators, complete grammar consumption, and EOF/end-state
  checks for the six carried fixtures. Negative evidence now calls
  `parseCompleteModule sourceText` and renders the diagnostic span carried by
  the parser result.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`:
  kept `Parser a`, `parserBind`, `parserMap`, `parserChoice`, span capture,
  diagnostic labels, and EOF checks. `parserChoice` now backtracks on generic
  mismatch diagnostics but preserves labeled parser diagnostics once a branch
  reaches a committed negative checkpoint.
- `test/programs/compiler-parser-parity/parser-library/ParserParitySource.mlfp`:
  keeps parser-library span constants, including `lexerUnknownSpan`, for shared
  positive and negative evidence.
- `implementation_notes.md`: added a bounded round-311 note. Existing
  `CHANGELOG.md` and `test/conformance/mlfp/README.md` already describe the
  shared parser-library/source-text front-door scope without claiming full
  parser parity.

### Tests
- RED/minimization:
  - `/opt/homebrew/bin/timeout 90s cabal run mlf2 -- check-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
    timed out before the lexer reduction.
  - The same command after replacing the exact-source lexer with a recursive
    cursor lexer failed direct `run-program` with
    `recursive top-level binding lookup: ParserParityLexer__lexRemaining -> ParserParityLexer__lexNonEmpty -> ParserParityLexer__lexRemaining`.
  - A higher-order token-builder scanner minimized to
    `WitnessNormalizationError ... OpUnderRigid`; it was replaced with the
    first-order bounded scan/reverse implementation.
- Direct positive smokes:
  - `/opt/homebrew/bin/timeout 180s cabal run mlf2 -- check-program ... --search-path test/programs/compiler-parser-parity/parser-library`
    passed for `basic-module-def-bool`, `import-exposing-def-bool`,
    `value-def-list-int-ref`, `let-lambda-application`,
    `typed-annotation-types`, and `data-declaration-constructor-spans`.
  - `/opt/homebrew/bin/timeout 180s cabal run mlf2 -- run-program ... --search-path test/programs/compiler-parser-parity/parser-library`
    printed the expected committed projection for the same six fixtures.
- Direct negative smokes:
  - `dist-newstyle/parser-parity-basic-module-def-bool-retry-evidence` prints
    the expected token prefix, lexer mismatch, and
    `parser negative expected-equals@...:2:21-2:25`.
  - Generated negative evidence packages for import, value-definition list,
    let/lambda/application, typed annotation, and data declaration print the
    expected labeled diagnostics at `2:33-2:34`, `3:20-3:21`, `3:34-3:36`,
    `3:29-3:30`, and `3:12-3:13` respectively.
- Focused required matcher:
  - `/opt/homebrew/bin/timeout 240s cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser lexes carried fixtures from source text before grammar parsing/"'`
    passed: 1 example, 0 failures.
- Parser parity group:
  - `/opt/homebrew/bin/timeout 900s cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
    passed: 17 examples, 0 failures.
- Static/diff checks:
  - `git diff --check` passed.
  - `rg "sourceContains|stringIndexOf.*sourceText|tokenInputBasicMissingEquals|tokenInputData|LexerOk basicModuleTokens|LexerOk dataDeclarationTokens|case tokens" test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity -n`
    produced no matches.
  - `rg "ParserSourceInput|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataZeroColonSpan" test/programs/compiler-parser-parity/*/Main.mlfp test/programs/compiler-parser-parity/*/ParserParityFixture.mlfp -n`
    produced no matches.
- Broader gates:
  - `/opt/homebrew/bin/timeout 1200s cabal build all` passed.
  - `/opt/homebrew/bin/timeout 1800s cabal test` was terminated by the
    wrapper while still running after the parser-parity section had passed.
  - `/opt/homebrew/bin/timeout 5400s cabal test` passed: 2631 examples, 0
    failures.
  - `/opt/homebrew/bin/timeout 1200s ./scripts/thesis-conformance-gate.sh`
    passed: thesis conformance anchors are green.

### Notes
- `orchestrator/state.json` was already dirty on entry and was not edited by
  this implementer retry.
- The implementation keeps the round bounded to carried parser-parity fixtures;
  it does not claim a general `.mlfp` parser, checker/backend parity, driver
  completion, platform completion, proof completion, or self-boot completion.
