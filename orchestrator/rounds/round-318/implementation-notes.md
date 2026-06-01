### Second Rejected-Review Structural Retry Changes

- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  replaced the active multi-module fixed module/declaration/body span constants
  with spans derived from parsed tokens and parser state. The shared
  complete-program path now threads parsed module starts into body parsing,
  uses the next parsed module token or a line-after-close fallback for module
  spans, and derives `main`, `zero`, `succ`, `Eq.eq`, `Nat`, `Expr`, and
  constructor row spans from the tokens consumed by those declaration parsers.
- `ParserParityParser.mlfp`: removed the fixed-span helper family from the
  selected round-318 path, including `mainDefinitionSpanForType`,
  `zeroDefinitionSpan`, `natDataRowsForSpan`, `exprDataRowsForSpan`, and the
  earlier exact string matcher helper. The remaining `finishExactModuleBodyRows`
  and `finishImportedBodyRows` helpers now append parser-produced rows and call
  the token-derived module-row finisher instead of accepting fixture span
  constants.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`:
  added `parserCurrentTokenStartOr`, keeping parser-state inspection inside the
  combinator owner. This lets declaration and module row builders derive their
  closing span from the next token without constructing `Parser` directly in the
  grammar module.
- `test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp`:
  exported `nextLineNumber` so the parser can derive the final-module EOF
  line-start fallback from the parsed closing module token rather than from a
  fixture-family full span.
- `test/ProgramParserParitySpec.hs`: extended the round-318 static guard so it
  fails on the rejected fixed coordinate strings in the active multi-module
  path, not only retired helper or fixture-family names. I confirmed the guard
  failed before the parser changes with `1 example, 1 failure` and then passed
  after the structural fix.

### Retry Changes

- `test/programs/compiler-parser-parity/parser-library/ParserParitySource.mlfp`,
  `ParserParityToken.mlfp`, and `ParserParityLexer.mlfp`: changed lexer-owned
  `SourceSymbol` values to carry parsed token start, parsed token end, and
  token text. The lexer cursor now tracks a bounded line number and current
  line prefix so token spans are derived while scanning instead of coming from
  fixture-family span constants.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`:
  added `expectIdentifierToken`, a shared identifier-token parser used by the
  complete-program module/export/import path. It also owns current-token
  diagnostic parsers for import semicolon and import-exposing separator
  failures, so those spans are token-derived.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`:
  replaced the Core/User-only complete-program module branch with
  `parseSharedModuleName`, which accepts a parsed identifier token and renders
  module rows from the parsed module name.
- `ParserParityParser.mlfp`: removed the fixed `parseNatSurfaceExportRows`,
  `parseClassSurfaceExportRows`, `parseThreeItemImportRows`, and
  `parseFourItemImportRows` style branches from the active complete-program
  path. Export and import lists now accumulate `ValueProjectionRows` through
  shared item parsers and bounded generic list-budget steps.
- `ParserParityParser.mlfp`: export/import projection item names, ordering,
  `(..)` spans, plain type/value classification, and import-module spans are
  derived from parsed tokens. The import-module row span is derived from the
  parsed module-name token start through the parsed `exposing` token start,
  matching the canonical projection without a fixed Core/User span.
- `test/ProgramParserParitySpec.hs`: extended the round-318 static guard to
  catch the rejected equivalent shapes, including the fixed known-module,
  fixed export/import item, fixed span, and fixed row-family function names.

### Bounded Parser Notes

- Complete-program parsing remains bounded to four modules because the current
  `.mlfp` parser-library slice does not express an unbounded recursive module
  list here. The bound is independent of Core/User fixture-family dispatch:
  each slot calls the same shared module parser and appends parsed module rows
  in source order.
- Export/import projection list accumulation is also bounded by explicit
  generic budget steps rather than self-recursive parser combinators. A
  self-recursive parser-combinator attempt failed during presolution with
  `OperationOnLockedNode (TypeRef (NodeId {getNodeId = 738}))`; the retained
  bound is independent of Nat/Eq/main or Core/User fixture families and every
  item still flows through the same parsed item accumulator.
- Declaration and body-row accumulation for the two round-318 fixtures is now
  likewise independent of fixture-family span constants. The active path remains
  grammar-shaped and bounded, but every emitted module, import, export,
  declaration, constructor, and definition row is assembled from parsed tokens
  or the parser-owned current-token cursor.
- Scope remains parser parity only. This retry does not claim resolver,
  checker, backend, platform, driver, proof, full parser parity, or self-boot
  completion.

### Base Refresh

- Controller reported the branch was behind `origin/master` by 51 commits.
  I stopped the in-flight parser-parity group validation process before
  refreshing the base.
- Safety snapshot before replay:
  `/tmp/mlf4-round318-refresh-20260601165747` contains status, staged and
  unstaged patches, and an untracked-file tarball.
- I stashed the implementation changes, fast-forwarded
  `orchestrator/round-318-next-parser-parity-slice` from `3b38af28` to
  `1279adcd`, and verified `git rev-list --left-right --count HEAD...origin/master`
  returned `0 0`.
- The stash replay had one round-scope conflict in
  `test/ProgramParserParitySpec.hs`. I resolved it by preserving the refreshed
  `beforeAll loadParserParityBatchFixture` batch-driver structure and adding
  the round-318 multi-module and malformed-separator assertions against that
  single public batch run.
- `orchestrator/state.json` was already modified before the refresh and
  remains controller-owned. I excluded it from the implementation patch replay;
  its pre-existing staged state is still present and was not content-edited by
  this retry.

### Tests Run

- `MLF_PROGRAM_TIMING=1 MLF_PROGRAM_TIMING_DEF_DETAILS=1 timeout 900 cabal run mlf2 -- check-program test/programs/compiler-parser-parity/multi-module-abstract-export-import --search-path test/programs/compiler-parser-parity/parser-library`
  passed with `OK`. Parser-library check completed after the retry; the final
  `ParserParityParser.def-bindings` timing was `131527.618ms`.
- `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multi-module-abstract-export-import --search-path test/programs/compiler-parser-parity/parser-library`
  passed and emitted the committed abstract multi-module projection, including
  `import Core span=...:12:10-12:15`.
- `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multi-module-recursive-adt-export-import --search-path test/programs/compiler-parser-parity/parser-library`
  passed and emitted the committed recursive ADT multi-module projection,
  including parsed `Eq`, `Nat(..)`, `Expr(..)`, and `zero` import/export rows.
- Pre-refresh focused Hspec checks passed before the controller-directed base
  refresh:
  `shared parser-owned .mlfp parser parses multi-module source text and export/import surfaces`
  passed in `364.3608 seconds`, and
  `parser-owned .mlfp parser rejects malformed multi-module import exposing separators through public run-program`
  passed in `179.1827 seconds`.
- Post-refresh direct batch smoke passed:
  `timeout 900 cabal run mlf2 -- run-program dist-newstyle/parser-parity-batch --search-path test/programs/compiler-parser-parity/parser-library > /tmp/mlf4-round318-batch-actual-4.txt`.
  The batch output includes the new multi-module sections, the carried import
  negative now reports token-derived `expected-import-semicolon@...:3:3-3:6`,
  and the multi-module malformed import reports token-derived
  `expected-import-exposing-separator@...:12:29-12:33`.
- Post-refresh focused Hspec matcher passed:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses multi-module source text and export/import surfaces/"'`
  passed in `221.5416 seconds` with `1 example, 0 failures`.
- Post-refresh malformed-separator Hspec matcher passed:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed multi-module import exposing separators through public run-program/"'`
  passed in `221.8468 seconds` with `1 example, 0 failures`.
- Static shortcut guard passed:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  passed in `0.2928 seconds` with `1 example, 0 failures`.
- Broader parser-parity Hspec group passed:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  passed in `214.7520 seconds` with `9 examples, 0 failures`.
- Post-refresh direct new-fixture smokes passed:
  `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multi-module-abstract-export-import --search-path test/programs/compiler-parser-parity/parser-library`
  and
  `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multi-module-recursive-adt-export-import --search-path test/programs/compiler-parser-parity/parser-library`.
- Post-refresh carried import smoke passed:
  `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`.
- Static fixture-root audit passed with no matches:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|multiModule.*Span|abstract.*Span|recursive.*Span|moduleIntegrated.*Span|module.*ProgramKey|completeProgramKey'`.
- Exact-source/static-token audit passed with no matches:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|MultiModuleTokens|AbstractExportTokens|RecursiveAdtExportTokens|ModuleIntegratedTokens|LexerOk (multiModuleTokens|abstractExportTokens|recursiveAdtTokens|recursiveAdtExportTokens|moduleIntegratedTokens)|multi-module tokens|abstract-export tokens|recursive-adt tokens|module-integrated tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`.
- Rejected-shape audit passed with no matches:
  `rg -n 'parseCoreModuleName|parseUserModuleName|parseKnownModuleName|parseKnownImportModuleName|parseProgramModuleName|parseCoreProgramModule|parseUserProgramModule|parseNatSurfaceExportRows|parseClassSurfaceExportRows|parseThreeItemImportRows|parseFourItemImportRows|parseProjectionExportSecondOrDone|parseProjectionExportThirdOrDone|parseProjectionExportFourthOrDone|parseImportProjectionSecondOrClose|parseImportProjectionThirdOrClose|parseImportProjectionFourthOrClose|ExpectedImportExposingSeparator "12:29-12:33"|firstImportModuleSpan|exportSurfaceSpan|constructedExportSurfaceSpan|valueExportSurfaceSpan|importSurfaceSpan|constructedImportSurfaceSpan|valueImportSurfaceSpan|constructedSurfaceSpan|stringIndexOf importRows' test/programs/compiler-parser-parity/parser-library`.
- Complete-program rendered-row audit passed with no matches:
  `rg -n 'ValueProjectionRows .*render|render.*ValueProjectionRows|appendLine .*ValueProjectionRows' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`.
- Conflict-marker audit over touched round files passed with no matches:
  `rg -n '^(<<<<<<<|=======|>>>>>>>)' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs orchestrator/rounds/round-318 implementation_notes.md CHANGELOG.md docs/mlfp-self-boot-readiness.md test/conformance/mlfp/README.md`.
- `git rev-list --left-right --count HEAD...origin/master` returned `0 0`.
- `git diff --check` passed.

### Second Structural Retry Tests

- Static guard RED before the parser fix:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  failed in `0.3049 seconds` with `1 example, 1 failure`, listing the rejected
  fixed-coordinate phrases.
- Static guard after the parser fix:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  passed in `0.3342 seconds` with `1 example, 0 failures`.
- Fixed-coordinate structural audit passed with no matches:
  `rg -n 'parserStringExactMatch|mainDefinitionSpan|mainModuleSpan|zeroDefinitionSpan|natDataRowsForSpan|exprDataRowsForSpan|mainDefinitionSpanForType|finishExactModuleBodyRows sourceFile moduleName exportRows "1:1|finishImportedBodyRows sourceFile moduleName exportRows importRows "|dataRows sourceFile "Nat" "[0-9]|dataParamRows sourceFile "Expr" "a" "[0-9]|constructorRows sourceFile "(Zero|Succ|DoneNat|Step)" .* "[0-9]|defRows sourceFile "(zero|succ)" .* "[0-9]|classRows sourceFile "Eq" "a" "[0-9]|methodSignatureRows sourceFile "eq" "a -> a -> Bool" "[0-9]' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`.
- Named fixed-span audit passed with no matches:
  `rg -n '"(1:1-11:1|1:1-17:1|11:1-16:1|17:1-25:1|2:3-6:3|6:3-10:3|10:3-14:3|14:3-15:1|20:3-24:1|4:7-4:24|7:7-7:17|8:3-9:1|21:7-21:21|22:7-22:18)"|mainDefinitionSpanForType|zeroDefinitionSpan|natDataRowsForSpan|exprDataRowsForSpan' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`.
- Focused multi-module matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses multi-module source text and export/import surfaces/"'`
  passed in `216.9813 seconds` with `1 example, 0 failures`.
- Focused malformed import matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed multi-module import exposing separators through public run-program/"'`
  passed in `214.8513 seconds` with `1 example, 0 failures`.
- Parser-parity Hspec group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  passed in `217.9531 seconds` with `9 examples, 0 failures`.
- Direct expected-output smoke diffs passed for 20 parser-parity fixtures:
  `basic-module-def-bool`, `import-exposing-def-bool`,
  `value-def-list-int-ref`, `let-lambda-application`,
  `typed-annotation-types`, `data-declaration-constructor-spans`,
  `case-expression-constructor-patterns`, `case-expression-nested-patterns`,
  `typeclass-deriving-method`, `typeclass-instance-nullary-method`,
  `higher-kinded-class-data-params`, `multiparam-superclass-fundep`,
  `type-family-kind-lambda`, `type-family-apply-annotation`,
  `gadt-result-constructor-spans`, `existential-constructor-forall`,
  `qualified-import-alias-references`, `qualified-import-alias-only`,
  `multi-module-abstract-export-import`, and
  `multi-module-recursive-adt-export-import`. Outputs and empty diffs were
  written under `/tmp/mlf4-round318-smokes.xedQ46`.
- `git diff --check` passed.
- `git diff --cached --check` passed.
- `timeout 7200 cabal build all` passed.
- `timeout 10800 cabal test` passed in `578.0598 seconds` with
  `2656 examples, 0 failures`.
- `timeout 7200 ./scripts/thesis-conformance-gate.sh` passed with final line
  `[thesis-gate] PASS: thesis conformance anchors are green`.

### Remaining Notes

- `orchestrator/state.json` was already modified when this retry started. It
  is controller-owned and was not edited for this implementation retry.
- No implementation blocker remains from the second rejected review. Full
  `cabal build all`, `cabal test`, and the thesis conformance gate were rerun
  after the structural span fix.
