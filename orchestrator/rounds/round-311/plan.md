### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-311-parser-source-text-front-door`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Grow the round-310 shared parser-owned `.mlfp` parser library so the carried
parser-parity fixtures enter through one shared source-text lexer/cursor front
door. The existing shared grammar parser should continue to own syntax parsing,
but fixture roots should provide source text rather than pre-tokenized
`ParserSourceInput` / `SourceSymbol` streams.

This is a whole parser-library growth slice. It must not add another
fixture-owned parser, and it must not add one tiny grammar function as the
whole round.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that proves at least two carried fixtures run through a source-text parser
entrypoint, then grow the same source-text path across all carried
parser-parity fixtures before closeout.

Keep the implementation rooted in
`test/programs/compiler-parser-parity/parser-library/`. Add or extend
parser-owned source cursor, lexer/tokenizer, and span-rendering helpers that
consume a `String` source using the broad text substrate (`stringLength`,
`stringCharAtOption`, `stringSlice`, ASCII character classification, and
`stringFromInt` as needed). The lexer should emit the parser library's internal
token stream with source spans for the currently carried grammar: module
headers, exports, imports, definitions, data declarations, identifiers,
`true`, integer literals, punctuation, `->`, and `..`.

The shared grammar parser may continue to parse an internal token/input stream,
but that stream must be produced by the shared source-text lexer for normal
fixture execution. Fixture packages under
`test/programs/compiler-parser-parity/<fixture>/` should become thinner:
`Main.mlfp` and `ParserParityFixture.mlfp` may provide source file identity and
the exact source text, then call a shared parser-library entrypoint such as
`renderParserParityProjectionFromSourceText`. They must not hand-author
`ParserSourceInput`, `SourceSymbol`, span constants, token streams, or
fixture-specific lexer/parser logic.

Negative parser-parity evidence should also enter through source text and the
same shared lexer/parser path. Keep diagnostics stable enough for committed
evidence, but do not copy Megaparsec prose verbatim. If the source-text lexer
exposes a real checker/runtime bug needed to run the parser library, fix the
root cause with a focused regression and record it as parser-parity-enabling
support; do not widen into checker, backend, driver, platform, package
manager, generic Prelude parser/monad APIs, or proof work.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add a focused RED Hspec
   matcher in `test/ProgramParserParitySpec.hs`, for example
   `MLF.Program parser parity / shared parser-owned .mlfp parser lexes carried fixtures from source text before grammar parsing`. The first assertion should run the basic Bool fixture and one higher-coverage fixture such as `data-declaration-constructor-spans` through the public `run-program` path with `--search-path test/programs/compiler-parser-parity/parser-library`, and compare each output with the committed canonical projection.
2. Add the shared parser-library source-text front door. Implement a
   parser-owned cursor/lexer path that scans source text, skips whitespace,
   produces source spans, recognizes the carried keyword/identifier/literal and
   punctuation set, and returns lexer diagnostics for unknown or malformed text.
   Keep parser helpers parser-owned under the shared library.
3. Thread the new source-text entrypoint into the existing shared grammar
   parser. Preserve the round-310 parser-combinator shape: grammar functions
   should still compose through `Parser`, `parserBind`/`andThen`,
   `parserMap`, `parserChoice`, span capture, diagnostic labels, and EOF
   checking.
4. Convert every carried positive fixture root
   (`basic-module-def-bool`, `import-exposing-def-bool`,
   `value-def-list-int-ref`, `let-lambda-application`,
   `typed-annotation-types`, and `data-declaration-constructor-spans`) so its
   fixture module provides source text and calls the shared source-text
   entrypoint. Preserve all committed
   `test/conformance/mlfp/parser-parity/*/expected/parser-program.txt`
   projections.
5. Convert temporary negative-evidence packages in
   `test/ProgramParserParitySpec.hs` so their `Main.mlfp` values pass malformed
   source text to the same shared lexer/parser entrypoint. Keep the public
   malformed import, value-definition semicolon, let-expression, typed
   annotation, data-declaration constructor-colon, and tokenizer mismatch
   evidence green through the shared path.
6. Remove fixture-authored token/source-symbol streams from normal execution.
   The shared parser library may keep an internal token/input representation,
   but fixture roots must not import or construct `ParserSourceInput`,
   `ParserSourceSymbol`, `SourceSymbol`, `sourceInputCons`, or parser-library
   span constants.
7. Update bounded progress documentation that already tracks parser parity,
   such as `CHANGELOG.md`, `implementation_notes.md`,
   `docs/mlfp-self-boot-readiness.md`, and
   `test/conformance/mlfp/README.md`. State that the carried fixtures now enter
   through a shared source-text lexer/cursor front door, while still avoiding
   full parser parity, checker/backend, driver, platform, proof, or self-boot
   claims.
8. Run focused parser-parity checks, direct package smokes for all carried
   fixtures, static audits for no fixture-authored token streams or
   exact-source success recognizers, diff hygiene, full Cabal gates, and the
   thesis gate before review.

### Verification
- RED and focused GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser lexes carried fixtures from source text before grammar parsing/"'`
- Carried parser-parity group:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct package smokes:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
- Fixture front-door audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan'`
  should produce no matches.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens)' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  should produce no matches.
- Shared parser architecture audit:
  `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput' test/programs/compiler-parser-parity/parser-library`
  should produce no normal public entrypoint that fixture roots still call.
- Diff and full gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-311/selection-record.json`
- `orchestrator/rounds/round-311/round-plan-record.json`
