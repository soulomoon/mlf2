# Review: round-310

## Checks Run

- Command: `git status --short --branch`
  Result: PASS. Worktree is on `orchestrator/round-310-parser-library-consolidation`; existing implementation/control-plane edits were left untouched.
- Command: `git diff --check`
  Result: PASS before and after validation/artifact writing.
- Command: `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityParser.mlfp -print | sort`
  Result: PASS. Only `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` remains.
- Command: `rg -n 'case tokens|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens)' test/programs/compiler-parser-parity/parser-library`
  Result: PASS. No fixture-token classifier or old fixture token-stream constructor remains in the shared parser library.
- Command: `rg -n 'stringSlice source [0-9]|stringIndexOf \(stringSlice source|stringIndexOf source .*SourceText|\b[a-z][A-Za-z0-9_]*SourceText\b|renderParserParityEvidence|TokenStream :' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  Result: PASS. No exact-source success recognizer, fixed source-offset probe, `SourceText` fixture path, old static renderer, or complete `TokenStream` route matched.
- Command: `rg -n 'parserStateAtEnd|render[A-Za-z]*NegativeEvidence|parseCompleteModule|parseTokens|moduleKey|LexerError|validateSourceInput' test/programs/compiler-parser-parity/parser-library`
  Result: PASS by inspection. `parseCompleteModule` routes through `tokenizeCompleteModule` and `parseTokens`; `parserReplyToResult` checks `parserStateAtEnd`; `completeModuleKey` is reached after `}`; negative evidence renderers call `parseCompleteModule`; lexer validation can emit `LexerError`.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser library routes carried parser fixtures through one entrypoint/"'`
  Result: PASS. `1 example, 0 failures` in 637.9337s.
- Command: `for fixture in basic-module-def-bool import-exposing-def-bool value-def-list-int-ref let-lambda-application typed-annotation-types data-declaration-constructor-spans; do cabal run mlf2 -- run-program "test/programs/compiler-parser-parity/$fixture" --search-path test/programs/compiler-parser-parity/parser-library; done`
  Result: PASS. All six carried fixture packages printed their expected parser projections through the shared search-path library.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program diagnostics/checks nullary constructors from wide ADTs without leaking handler result polymorphism/" --match "/MLF.Program diagnostics/checks parameterized constructor applications without leaking identity substitutions/"'`
  Result: PASS. `2 examples, 0 failures` in 31.4320s.
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: PASS. `16 examples, 0 failures` in 4593.8430s.
- Command: `cabal build all`
  Result: PASS.
- Command: `cabal test`
  Result: PASS. `2630 examples, 0 failures` in 5967.3996s.
- Command: `./scripts/thesis-conformance-gate.sh`
  Result: PASS. Final output included `[thesis-gate] PASS: thesis conformance anchors are green`.

## Plan Compliance

- Step 1, focused RED/GREEN shared-entrypoint matcher: met. The focused shared-entrypoint Hspec now passes and proves at least the basic Bool and import-exposing fixtures use `runProgramArgs [fixtureRoot, "--search-path", sharedParserLibraryRoot]`.
- Step 2, shared parser library root: met. `test/programs/compiler-parser-parity/parser-library/` owns shared source, token, AST/projection, lexer, diagnostic, parser-combinator, and parser modules.
- Step 3, basic/import thin harnesses: met. Both fixture roots now contain only `Main.mlfp` plus `ParserParityFixture.mlfp`, and both call `ParserParityParser.renderParserParityProjection` through the shared search path.
- Step 4, carried positive fixtures: met. `value-def-list-int-ref`, `let-lambda-application`, `typed-annotation-types`, and `data-declaration-constructor-spans` all route through the same shared entrypoint and preserve committed parser projections.
- Step 5, negative evidence through shared paths: met. Temporary negative packages provide `Main.mlfp` only and call shared parser-library renderers. The renderers now derive diagnostics from malformed `ParserSourceInput` values through `tokenizeCompleteModule` / `parseCompleteModule`, not static strings.
- Step 6, old per-fixture parser removal and exact-source removal: met. Old per-fixture parser/source/token/AST support modules are removed, only the shared parser module remains, and static audits found no exact-source success recognizer or complete fixture token-stream classifier.
- Step 7, bounded docs/changelog updates: met. Touched notes describe shared parser-library consolidation without claiming full parser parity, checker/backend, driver, platform, proof, or self-boot completion.
- Step 8, validation: met. Focused checks, parser-parity group, direct smokes, static audits, `git diff --check`, `cabal build all`, `cabal test`, and the thesis gate all passed.

## Decision

**APPROVED.**

## Evidence

The previous complete-syntax blocker is fixed in the current code. `ParserParityParser.parseCompleteModule` calls `tokenizeCompleteModule` and then `parseTokens`; `parserReplyToResult` returns `ParserOk` only when `parserStateAtEnd` reports `ParserAtEnd`; `completeModuleKey` is reached after the closing `}`. The carried grammar paths consume the relevant suffixes before projection: Bool definitions require `=`, `bool:true`, `;`, and `}`; value-list modules parse both definitions and reference suffix; let/lambda and typed-annotation modules parse the lambda/application tails; data declarations parse constructors plus the following `main` definition.

The previous negative-evidence blocker is fixed. `ParserParityLexer.validateSourceInput` can return `LexerError` for `SourceUnknown`, and parser negative evidence renderers call `parseCompleteModule` over malformed `ParserSourceInput` values. The static diagnostic-string bypasses from the stale rejection are gone.

The parser-combinator requirement is satisfied for this bounded item. `ParserParityParserCombinator.mlfp` defines parser-owned `Parser a`, parser state, `runParser`, `parserPure`, `parserFail`, `parserBind`, `parserMap`, `parserChoice`, span/label helpers, token expectation, and EOF checking. The grammar composes through that layer rather than per-fixture parser modules or complete fixture token-stream cases.

The included ADT inference support is in scope. It supports the parser-local `Parser a` / constructor-heavy fixture code, is covered by focused regressions for wide nullary ADTs and `Box value : Box a`, and the full suite plus thesis gate stayed green. I found no checker/backend/driver scope claim in the round docs.

Validation-generated churn in `runtime/mlfp_io/target/release/libmlfp_io.d` was restored after the Cabal gates. I did not edit implementation code or `orchestrator/state.json`.
