### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-312-parser-library-case-pattern-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text lexer/parser library with a
meaningful next grammar slice: `case ... of` expressions, constructor
application scrutinees, constructor patterns, wildcard patterns, nested
constructor patterns, branch separators, and the small literal/token additions
needed by those cases. The new coverage must compare the shared `.mlfp`
parser-library projection with the Haskell canonical parser projection and
must keep fixture roots as thin source/evidence harnesses.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that names the new case/pattern behavior before coding, then grow the same
source-text parser-library path across a grouped positive and negative slice.

This round builds on the approved round-310 shared parser-combinator library
and the approved round-311 source-text front door. Keep the implementation
rooted in `test/programs/compiler-parser-parity/parser-library/`. Add the
grammar through parser-owned lexer tokens, parser combinators, AST/projection
rendering, spans, and diagnostics. Do not create a fixture-owned
`ParserParityParser.mlfp`, a per-fixture token stream, an exact-source
recognizer, or a separate parser package per test fixture.

The first public behavior/focused failing test is:

`MLF.Program parser parity / shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns`

The first RED assertion should run at least two new public parser-parity
fixtures through `run-program ... --search-path test/programs/compiler-parser-parity/parser-library`
and compare each output with committed canonical projection files:

- `case-expression-constructor-patterns`: `data Nat`, `case Succ Zero of`,
  `Zero -> 0`, and `Succ _ -> 1`.
- `case-expression-nested-patterns`: `data Nat`, `case Succ (Succ Zero) of`,
  `Succ Zero -> false`, `Succ (Succ n) -> true`, and `_ -> false`.

Add one negative diagnostic path in the same round, such as a missing case
branch arrow or malformed case branch separator, and render it through the
same shared source-text lexer/parser path. Keep diagnostics stable and
parser-owned; do not copy Megaparsec prose verbatim.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec
   matcher named above in `test/ProgramParserParitySpec.hs`. The matcher
   should compare Haskell canonical projections and shared parser-library
   projections for both new case/pattern fixtures.
2. Add committed conformance sources and expected parser projection files under
   `test/conformance/mlfp/parser-parity/case-expression-constructor-patterns/`
   and `test/conformance/mlfp/parser-parity/case-expression-nested-patterns/`.
   Extend the projection renderer in `test/ProgramParserParitySpec.hs` only as
   needed to render observable canonical case expressions and patterns for this
   parser-parity evidence.
3. Add thin fixture roots under `test/programs/compiler-parser-parity/` for
   both new fixtures. Each root should have only `Main.mlfp` plus
   `ParserParityFixture.mlfp`, provide `sourceFile` and `sourceText`, and call
   `renderParserParityProjectionFromSourceText` from the shared parser library.
4. Extend the shared parser-library lexer/token layer to scan the new source
   text needed by the slice: `case`, `of`, `false`, `_`, branch arrows,
   constructor identifiers already in the fixture sources, parentheses, and
   branch separators. Preserve the round-311 source-text cursor path and avoid
   exact full-source matching.
5. Extend the parser-combinator grammar in
   `test/programs/compiler-parser-parity/parser-library/` for `case`
   expressions and constructor/wildcard/nested patterns. Grammar functions must
   compose through `Parser`, `parserBind` or equivalent sequencing,
   `parserChoice`, span capture, diagnostic labeling, and EOF/end-state checks.
6. Extend parser-library AST/projection rendering and span constants for the
   two new fixtures so the `.mlfp` parser projection matches the committed
   canonical projection. Keep this as parser-parity evidence, not a new
   checker/backend contract.
7. Add a public malformed case-expression diagnostic fixture in
   `test/ProgramParserParitySpec.hs` that calls the same
   `renderParserNegativeEvidenceFromSourceText` path. Use this to prove the
   shared parser library reports a parser-owned diagnostic for the selected
   malformed case syntax.
8. Update bounded progress docs that already track parser parity, such as
   `CHANGELOG.md`, `implementation_notes.md`,
   `docs/mlfp-self-boot-readiness.md`, and
   `test/conformance/mlfp/README.md`, without claiming full parser parity,
   checker/backend support, platform work, driver work, proof work, or
   self-boot completion.
9. Run the focused RED/GREEN check, the full parser-parity group, direct smokes
   for all carried and new parser-parity fixtures, static banned-shape audits,
   `git diff --check`, `cabal build all`, `cabal test`, and
   `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED and GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to case expressions and constructor patterns/"'`
- Parser-parity group:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct carried and new fixture smokes:
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-constructor-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-nested-patterns --search-path test/programs/compiler-parser-parity/parser-library`
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span'`
  should produce no matches in fixture roots.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens)|case tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Shared parser architecture audit:
  `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput|ParserParityParser.mlfp' test/programs/compiler-parser-parity`
  should show no fixture-owned parser entrypoint and no normal public parser
  entrypoint that accepts fixture-authored `ParserSourceInput`.
- Diff and full gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-312/selection-record.json`
- `orchestrator/rounds/round-312/round-plan-record.json`
