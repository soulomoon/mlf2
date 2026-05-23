### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-314-parser-library-higher-kinded-constraint-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text lexer/parser-combinator
library with the next larger canonical parser-parity syntax slice:
higher-kinded declaration parameter annotations, variable-headed type
applications in source types, constructor type applications, superclass
constraints, multi-parameter class heads, Unicode functional dependencies, and
empty instance bodies. The round must compare the shared `.mlfp`
parser-library projection with the Haskell canonical parser projection and
must keep fixture roots as thin source/evidence harnesses.

This is still parser parity only. Do not claim type-family parity, checker,
resolver, backend, driver, platform, proof, or self-boot progress.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that names the grouped higher-kinded/constraint behavior before coding, then
grow the same shared source-text parser-library path across positive and
negative evidence.

This round builds on the approved round-310 shared parser-combinator library,
round-311 source-text front door, round-312 case/pattern extension, and
round-313 typeclass/deriving/instance extension. Keep the implementation rooted
in `test/programs/compiler-parser-parity/parser-library/`. Add grammar through
parser-owned lexer tokens, parser combinators, AST/projection rendering, spans,
and diagnostics. Do not create a fixture-owned `ParserParityParser.mlfp`, a
per-fixture token stream, an exact-source recognizer, or a separate parser
package per test fixture.

The first public behavior/focused failing test is:

`MLF.Program parser parity / shared parser-owned .mlfp parser extends source-text grammar to higher-kinded and constrained class syntax`

The first RED assertion should run at least two new public parser-parity
fixtures through `run-program ... --search-path test/programs/compiler-parser-parity/parser-library`
and compare each output with committed canonical projection files:

- `higher-kinded-class-data-params`: a `Main` module exporting `Functor` and
  `Higher(..)`, with `class Functor (f :: * -> *)`, method signature
  `map : ∀ a b. (a -> b) -> f a -> f b`, and `data Higher (f :: * -> *) a`
  whose constructor type is `f a -> Higher f a`.
- `multiparam-superclass-fundep`: a `Main` module exporting `Monad`, with
  `class Functor f => Monad (m :: * -> *) (f :: * -> *) | m → f`, method
  signature `bind : ∀ a b. m a -> (a -> m b) -> m b`, and
  `instance Monad IO IO { }`.

Use the existing canonical parser snippets in `test/ProgramSpec.hs` as the
fixture basis. Copy only the needed source behavior into new parser-parity
conformance fixtures; fixture roots must expose `sourceFile` and `sourceText`
and call the shared `renderParserParityProjectionFromSourceText` entrypoint.

Add one negative diagnostic path in the same round, such as a missing Unicode
functional-dependency arrow in the `| m → f` clause, and render it through the
same `renderParserNegativeEvidenceFromSourceText` path. Keep diagnostics
stable and parser-owned; do not copy Megaparsec prose verbatim.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec
   matcher named above in `test/ProgramParserParitySpec.hs`. The matcher
   should compare Haskell canonical projections and shared parser-library
   projections for both new fixtures before implementation starts.
2. Add committed conformance sources and expected parser projection files under
   `test/conformance/mlfp/parser-parity/higher-kinded-class-data-params/` and
   `test/conformance/mlfp/parser-parity/multiparam-superclass-fundep/`.
   Extend the projection renderer in `test/ProgramParserParitySpec.hs` only as
   needed to render observable canonical higher-kinded type parameters,
   superclass constraints, functional dependencies, multi-parameter class and
   instance heads, `STVarApp`, and constructor type applications.
3. Add thin fixture roots under `test/programs/compiler-parser-parity/` for
   both new fixtures. Each root should have only `Main.mlfp` plus
   `ParserParityFixture.mlfp`, provide `sourceFile` and `sourceText`, and call
   `renderParserParityProjectionFromSourceText` from the shared parser library.
4. Extend the shared parser-library lexer/token layer for the selected source:
   `Functor`, `Higher`, `Monad`, `IO`, `map`, `bind`, `f`, `m`, `b`, `*`,
   `=>`, `|`, Unicode `→`, spaced `∀ a b.` binders, kind arrows, and any
   punctuation not already covered by the carried fixtures.
5. Extend the parser-combinator grammar in
   `test/programs/compiler-parser-parity/parser-library/` for kinded
   declaration parameters, source kind syntax, multi-binder `∀` source types,
   variable-headed type applications, constructor type applications,
   superclass constraint prefixes, functional-dependency clauses,
   multi-parameter class heads, and empty instance bodies. Grammar functions
   must compose through `Parser`, `parserBind` or equivalent sequencing,
   `parserChoice`, span capture, diagnostic labeling, and EOF/end-state checks.
6. Extend parser-library AST/projection rendering and span constants for the
   two new fixtures so the `.mlfp` parser projection matches the committed
   canonical projection. Keep this as parser-parity evidence, not a new public
   syntax API or checker/backend contract.
7. Add a public malformed higher-kinded/constraint diagnostic fixture in
   `test/ProgramParserParitySpec.hs` that calls the same
   `renderParserNegativeEvidenceFromSourceText` path. Use this to prove the
   shared parser library reports a parser-owned diagnostic for the selected
   malformed functional-dependency or kinded-parameter syntax.
8. Update bounded progress docs that already track parser parity, such as
   `CHANGELOG.md`, `implementation_notes.md`,
   `docs/mlfp-self-boot-readiness.md`, and
   `test/conformance/mlfp/README.md`, without claiming full parser parity,
   type-family parity, checker/backend support, platform work, driver work,
   proof work, or self-boot completion.
9. Run the focused RED/GREEN check, the full parser-parity group, direct smokes
   for all carried and new parser-parity fixtures, static banned-shape audits,
   `git diff --check`, `cabal build all`, `cabal test`, and
   `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED and GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to higher-kinded and constrained class syntax/"'`
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
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/higher-kinded-class-data-params --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multiparam-superclass-fundep --search-path test/programs/compiler-parser-parity/parser-library`
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span|higherKinded.*Span|fundep.*Span|constraint.*Span'`
  should produce no matches in fixture roots.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|HigherKindedTokens|ConstraintTokens|FundepTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens|higherKindedTokens|constraintTokens|fundepTokens)|case tokens|class tokens|instance tokens|higher-kinded tokens|constraint tokens|fundep tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
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

- `orchestrator/rounds/round-314/selection-record.json`
- `orchestrator/rounds/round-314/round-plan-record.json`
