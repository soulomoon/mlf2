### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-313-parser-library-typeclass-instance-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text lexer/parser library with a
larger declaration-family grammar slice: class declarations, class method
signatures, `deriving` clauses, instance declarations, instance method
definitions, class/method export items, and the source tokens needed by the
selected fixtures. The round must compare the shared `.mlfp` parser-library
projection with the Haskell canonical parser projection and must keep fixture
roots as thin source/evidence harnesses.

This is still parser parity only. Do not claim checker, resolver, backend,
driver, platform, proof, or self-boot progress.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that names the grouped typeclass/instance behavior before coding, then grow
the same shared source-text parser-library path across positive and negative
evidence.

This round builds on the approved round-310 shared parser-combinator library,
the approved round-311 source-text front door, and the approved round-312
case/pattern extension. Keep the implementation rooted in
`test/programs/compiler-parser-parity/parser-library/`. Add grammar through
parser-owned lexer tokens, parser combinators, AST/projection rendering,
spans, and diagnostics. Do not create a fixture-owned `ParserParityParser.mlfp`,
a per-fixture token stream, an exact-source recognizer, or a separate parser
package per test fixture.

The first public behavior/focused failing test is:

`MLF.Program parser parity / shared parser-owned .mlfp parser extends source-text grammar to typeclass, deriving, and instance declarations`

The first RED assertion should run at least two new public parser-parity
fixtures through `run-program ... --search-path test/programs/compiler-parser-parity/parser-library`
and compare each output with committed canonical projection files:

- `typeclass-deriving-method`: a `Main` module exporting `Eq`, `Nat(..)`,
  `eq`, and `main`, with `class Eq a`, method signature `eq : a -> a -> Bool`,
  `data Nat` plus `deriving Eq`, and a definition that calls `eq`.
- `typeclass-instance-nullary-method`: a `Main` module exporting `Monoid`,
  `Nat(..)`, `mempty`, `append`, and `main`, with a two-method class, `data Nat`,
  `instance Monoid Nat`, method definitions for `mempty` and `append`, and a
  `main` expression that uses both instance methods.

Use the existing public sources as the fixture basis:
`test/programs/unified/authoritative-overloaded-method.mlfp` and
`test/programs/unified/authoritative-nullary-overloaded-method.mlfp`. Copy only
the needed source behavior into new parser-parity conformance fixtures; the
fixture roots must remain source/evidence harnesses that call the shared parser.

Add one negative diagnostic path in the same round, such as a missing class
method semicolon or missing instance method equals sign, and render it through
the same `renderParserNegativeEvidenceFromSourceText` path. Keep diagnostics
stable and parser-owned; do not copy Megaparsec prose verbatim.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec
   matcher named above in `test/ProgramParserParitySpec.hs`. The matcher
   should compare Haskell canonical projections and shared parser-library
   projections for both new fixtures before implementation starts.
2. Add committed conformance sources and expected parser projection files under
   `test/conformance/mlfp/parser-parity/typeclass-deriving-method/` and
   `test/conformance/mlfp/parser-parity/typeclass-instance-nullary-method/`.
   Extend the projection renderer in `test/ProgramParserParitySpec.hs` only as
   needed to render observable canonical class declarations, method signatures,
   deriving clauses, instance declarations, and instance method definitions.
3. Add thin fixture roots under `test/programs/compiler-parser-parity/` for
   both new fixtures. Each root should have only `Main.mlfp` plus
   `ParserParityFixture.mlfp`, provide `sourceFile` and `sourceText`, and call
   `renderParserParityProjectionFromSourceText` from the shared parser library.
4. Extend the shared parser-library lexer/token layer for the selected source:
   `class`, `deriving`, `instance`, class names `Eq` and `Monoid`, method names
   `eq`, `mempty`, and `append`, type variable `a`, lambda parameters used in
   the instance body, class/method export items, and any punctuation not already
   covered by the carried fixtures.
5. Extend the parser-combinator grammar in
   `test/programs/compiler-parser-parity/parser-library/` for class
   declarations, method signature lists, deriving clauses after data
   declarations, instance declarations, and instance method definition lists.
   Grammar functions must compose through `Parser`, `parserBind` or equivalent
   sequencing, `parserChoice`, span capture, diagnostic labeling, and
   EOF/end-state checks.
6. Extend parser-library AST/projection rendering and span constants for the
   two new fixtures so the `.mlfp` parser projection matches the committed
   canonical projection. Keep this as parser-parity evidence, not a new public
   syntax API or checker/backend contract.
7. Add a public malformed typeclass/instance diagnostic fixture in
   `test/ProgramParserParitySpec.hs` that calls the same
   `renderParserNegativeEvidenceFromSourceText` path. Use this to prove the
   shared parser library reports a parser-owned diagnostic for the selected
   malformed declaration syntax.
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
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to typeclass, deriving, and instance declarations/"'`
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
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typeclass-deriving-method --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typeclass-instance-nullary-method --search-path test/programs/compiler-parser-parity/parser-library`
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span'`
  should produce no matches in fixture roots.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens)|case tokens|class tokens|instance tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
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

- `orchestrator/rounds/round-313/selection-record.json`
- `orchestrator/rounds/round-313/round-plan-record.json`
