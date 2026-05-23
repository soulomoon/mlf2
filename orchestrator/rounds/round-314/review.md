# Review: round-314

Decision: APPROVED

Item reviewed: `item-314-parser-library-higher-kinded-constraint-extension`.

## Re-review Findings

No blocking findings.

The rejected round-314 fixture-shaped parser branches are gone from
`test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`.
The static rejected-shape audit found no `parseHigherKindedModule`, no
`parseMultiparam...`, no direct
`completeModuleKey "higher-kinded-class-data-params"` /
`completeModuleKey "multiparam-superclass-fundep"`, and no equivalent direct
round-314 fixture-key `moduleKey` success shortcut in the parser library.

The retry keeps the new fixture roots thin: each new `ParserParityFixture.mlfp`
only provides `sourceFile` and `sourceText`, and each `Main.mlfp` calls the
shared `renderParserParityProjectionFromSourceText` entrypoint through the
parser-library search path.

The replacement stays on the shared parser-library source-text path. The
parser continues to use `Parser`, `parserBind`, `parserChoice`,
`captureSpan`, `diagnosticLabel`, `parserStateAtEnd`, and the new
`labelFunctionalDependencyArrow`; the round-314 grammar is expressed through
shared parser-combinator functions for kinded declaration parameters, source
kinds, variable-headed type applications, superclass constraints, functional
dependencies, multi-parameter class heads, and empty instance bodies.

Guard coverage is present in the focused matcher:
`sharedParserRound314ShortcutPhrases` rejects the exact branch/key shortcut
shapes from the prior review, and the focused matcher reads
`ParserParityParser.mlfp` and asserts those phrases are absent. The retry notes
record the RED failure against the rejected branch/key shape before the fix.

Docs remain bounded to parser parity and explicitly do not claim type-family
parity, checker/resolver/backend/platform/driver/proof/full self-boot.

## Evidence

- Focused matcher:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to higher-kinded and constrained class syntax/"'`
  Result: passed, 1 example, 0 failures, finished in 388.5856 seconds.
- Malformed fundep/kind diagnostic matcher:
  No separate Hspec example is present. The malformed functional-dependency
  arrow evidence is embedded in the focused matcher through
  `higherKindedFundepNegativeEvidenceProjection`, expected output
  `expected-functional-dependency-arrow@test/conformance/mlfp/parser-parity/multiparam-superclass-fundep/src/Main.mlfp:2:60-2:61`.
- Parser parity group:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: passed, 22 examples, 0 failures, finished in 3183.1534 seconds.
- Direct `run-program` smokes:
  `basic-module-def-bool` passed, 316 bytes.
  `import-exposing-def-bool` passed, 546 bytes.
  `value-def-list-int-ref` passed, 643 bytes.
  `let-lambda-application` passed, 552 bytes.
  `typed-annotation-types` passed, 583 bytes.
  `data-declaration-constructor-spans` passed, 856 bytes.
  `case-expression-constructor-patterns` passed, 907 bytes.
  `case-expression-nested-patterns` passed, 911 bytes.
  `typeclass-deriving-method` passed, 1366 bytes.
  `typeclass-instance-nullary-method` passed, 2021 bytes.
  `higher-kinded-class-data-params` passed, 898 bytes.
  `multiparam-superclass-fundep` passed, 846 bytes.
- Static fixture-root shortcut audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span|higherKinded.*Span|fundep.*Span|constraint.*Span'`
  Result: no matches (`exit=1`).
- Static exact-source/static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|HigherKindedTokens|ConstraintTokens|FundepTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens|higherKindedTokens|constraintTokens|fundepTokens)|case tokens|class tokens|instance tokens|higher-kinded tokens|constraint tokens|fundep tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  Result: no matches (`exit=1`).
- Static shared parser architecture audit:
  `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput|ParserParityParser.mlfp' test/programs/compiler-parser-parity`
  Result: no matches (`exit=1`).
- Round-314 rejected-shape audit:
  `rg -n 'parseHigherKindedModule|parseMultiparam|completeModuleKey "higher-kinded-class-data-params"|completeModuleKey "multiparam-superclass-fundep"|moduleKey "higher-kinded-class-data-params"|moduleKey "multiparam-superclass-fundep"' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  Result: no matches (`exit=1`).
- Positive parser architecture audit:
  `rg -n 'def renderParserParityProjectionFromSourceText : String -> String -> String|def parseCompleteModule : String -> ParserResult|def tokenizeCompleteModule : String -> LexerResult|data Parser a|def parserBind|def parserMap|def parserChoice|def captureSpan|def diagnosticLabel|def labelFunctionalDependencyArrow|def parserStateAtEnd|def parseKindedConstraintModule|def parseKindedDeclarationParameter|def parseSourceKind|def parseFunctionalDependency|def parseSuperclassConstraint|def parseEmptyInstanceBody|def parseTypeApplication' test/programs/compiler-parser-parity/parser-library`
  Result: confirmed the shared source-text front door, parser monad/combinator pieces, diagnostic label, EOF/end-state check, and round-314 reusable grammar functions.
- `git diff --check`
  Result: passed with no output.
- `cabal build all`
  Result: passed.
- `cabal test`
  Result: passed, 2636 examples, 0 failures, finished in 4383.7799 seconds.
- `./scripts/thesis-conformance-gate.sh`
  Result: passed; final line `PASS: thesis conformance anchors are green`.

Validation rewrote `runtime/mlfp_io/target/release/libmlfp_io.d` to the
worktree path; that generated depfile was restored to the pre-validation
canonical path. `orchestrator/state.json` was observed dirty and left untouched
because it is controller-owned.

Closeout classification: approved; status-only roadmap closeout pointer is
authorized, with no semantic roadmap update required.
