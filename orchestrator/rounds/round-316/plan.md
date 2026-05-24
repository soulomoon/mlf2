### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-316-parser-library-gadt-existential-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text lexer/parser-combinator
library with the next larger canonical parser-parity syntax slice:
GADT-style constructor result heads, parameterized data declarations,
constructor-local existential `forall` signatures, and the related
constructor/case pattern syntax needed to parse representative recursive ADT
sources. The round must compare the shared `.mlfp` parser-library projection
with the Haskell canonical parser projection and keep fixture roots as thin
source/evidence harnesses.

This is parser parity only. Do not claim checker, resolver, backend, driver,
platform, proof, full parser parity, or self-boot progress. Existing checked
behavior for GADT-style and existential sources may be used as fixture
inspiration, but the round is accepted only on parser-owned source-text
parse/projection/diagnostic evidence.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md` for the
behavior-changing implementation. Start with one public-interface RED matcher
that names the grouped GADT/existential parser behavior before coding, then
grow the same shared source-text parser-library path across positive and
negative evidence.

This round builds on the approved round-310 shared parser-combinator library,
round-311 source-text front door, round-312 case/pattern extension, round-313
typeclass/deriving/instance extension, round-314 higher-kinded/constraint
extension, and round-315 type-family/type-level extension. Keep implementation
rooted in `test/programs/compiler-parser-parity/parser-library/`. Add grammar
through parser-owned lexer tokens, parser combinators, AST/projection
rendering, spans, and diagnostics. Do not create a fixture-owned
`ParserParityParser.mlfp`, a per-fixture token stream, an exact-source
recognizer, or a separate parser package per test fixture.

The first public behavior/focused failing test is:

`MLF.Program parser parity / shared parser-owned .mlfp parser extends source-text grammar to GADT-style results and existential constructors`

The first RED assertion should run at least two new public parser-parity
fixtures through `run-program ... --search-path test/programs/compiler-parser-parity/parser-library`
and compare each output with committed canonical projection files:

- `gadt-result-constructor-spans`: a `Main` module with `Nat`, parameterized
  `Expr a`, constructor result heads such as `DoneNat : Nat -> Expr Nat`,
  recursive constructor fields such as `Step : Expr a -> Expr a`, and a small
  case expression over the constructors.
- `existential-constructor-forall`: a `Main` module with `Nat`, `Expr a`,
  `SomeExpr`, constructor-local `forall` syntax such as
  `SomeExpr : forall a. Expr a -> SomeExpr`, and a nested case expression that
  proves constructor and pattern spans still flow through the shared parser
  library.

Use the documented syntax in `docs/mlfp-language-reference.md` and the current
fixtures `test/programs/recursive-adt/recursive-gadt.mlfp` and
`test/programs/recursive-adt/recursive-existential.mlfp` as source behavior
references. Copy only the needed source behavior into parser-parity
conformance fixtures. Fixture roots must expose `sourceFile` and `sourceText`
and call the shared `renderParserParityProjectionFromSourceText` entrypoint.

Add one negative diagnostic path in the same round, such as a constructor-local
`forall` missing its required dot before the body:

`SomeExpr : forall a Expr a -> SomeExpr;`

Render it through the same `renderParserNegativeEvidenceFromSourceText` path
with a stable parser-owned label such as
`expected-constructor-forall-dot@...` or a narrower existing parser-owned label
if the implementation can reuse one honestly. Do not copy Megaparsec prose
verbatim.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec
   matcher named above in `test/ProgramParserParitySpec.hs`. The matcher
   should compare Haskell canonical projections and shared parser-library
   projections for both new fixtures before implementation starts.
2. Add committed conformance sources and expected parser projection files under
   `test/conformance/mlfp/parser-parity/gadt-result-constructor-spans/` and
   `test/conformance/mlfp/parser-parity/existential-constructor-forall/`.
   Extend the projection renderer in `test/ProgramParserParitySpec.hs` only as
   needed to render observable canonical data declarations, constructor result
   heads, constructor-local `forall` source types, constructor references,
   case alternatives, and spans.
3. Add thin fixture roots under `test/programs/compiler-parser-parity/` for
   both new fixtures. Each root should have only `Main.mlfp` plus
   `ParserParityFixture.mlfp`, provide `sourceFile` and `sourceText`, and call
   `renderParserParityProjectionFromSourceText` from the shared parser library.
4. Extend the shared parser-library lexer/token layer for the selected source:
   `data`, `forall` and canonical `∀`, `case`, `of`, `Nat`, `Expr`,
   `SomeExpr`, `DoneNat`, `Step`, lower identifiers such as `a`, `expr`,
   `next`, punctuation for `:`, `.`, `=`, `|`, `->`, braces/parens,
   semicolons, and the tokens already carried by prior fixtures.
5. Extend the parser-combinator grammar in
   `test/programs/compiler-parser-parity/parser-library/` for reusable
   parameterized data heads, constructor signatures whose result type can
   refine the declared type head, constructor-local `forall` binders, nested
   source-type applications in constructor fields/results, and the related
   case-pattern coverage. Compose through `Parser`, `parserBind` or equivalent
   sequencing, `parserChoice`, span capture, diagnostic labeling, and
   EOF/end-state checks.
6. Extend parser-library AST/projection rendering and span constants for the
   two new fixtures so the `.mlfp` parser projection matches the committed
   canonical projection. Keep this as parser-parity evidence, not a new public
   syntax API or checker/backend contract.
7. Add a public malformed constructor-local `forall` diagnostic fixture in
   `test/ProgramParserParitySpec.hs` that calls the same
   `renderParserNegativeEvidenceFromSourceText` path. Use this to prove the
   shared parser library reports a parser-owned diagnostic for the selected
   malformed constructor signature.
8. Add guard coverage against returning to fixture-key success shortcuts. The
   focused matcher should fail if the shared parser contains direct
   fixture-key branches such as `parseGadtResultModule`,
   `parseExistentialConstructorModule`, `completeModuleKey
   "gadt-result-constructor-spans"`, or `completeModuleKey
   "existential-constructor-forall"`.
9. Update bounded progress docs that already track parser parity, such as
   `CHANGELOG.md`, `implementation_notes.md`,
   `docs/mlfp-self-boot-readiness.md`, and
   `test/conformance/mlfp/README.md`, without claiming full parser parity,
   checker/resolver behavior beyond existing compiler support, backend support,
   platform work, driver work, proof work, or self-boot completion.
10. Run the focused RED/GREEN check, the full parser-parity group, direct
    smokes for all carried and new parser-parity fixtures, static banned-shape
    audits, `git diff --check`, `cabal build all`, `cabal test`, and
    `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED and GREEN:
  `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to GADT-style results and existential constructors/"'`
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
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/type-family-kind-lambda --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/type-family-apply-annotation --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/gadt-result-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
  `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/existential-constructor-forall --search-path test/programs/compiler-parser-parity/parser-library`
- Thin fixture audit:
  `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span|higherKinded.*Span|fundep.*Span|constraint.*Span|typeFamily.*Span|family.*Span|typeLevel.*Span|gadt.*Span|existential.*Span|constructorForall.*Span'`
  should produce no matches in fixture roots.
- Exact-source and static-token audit:
  `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|HigherKindedTokens|ConstraintTokens|FundepTokens|TypeFamilyTokens|FamilyTokens|GadtTokens|ExistentialTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens|higherKindedTokens|constraintTokens|fundepTokens|typeFamilyTokens|familyTokens|gadtTokens|existentialTokens)|case tokens|class tokens|instance tokens|higher-kinded tokens|constraint tokens|fundep tokens|type-family tokens|family tokens|gadt tokens|existential tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Rejected-shape audit for this round:
  `rg -n 'parseGadtResultModule|parseExistentialConstructorModule|completeModuleKey "gadt-result-constructor-spans"|completeModuleKey "existential-constructor-forall"|moduleKey "gadt-result-constructor-spans"|moduleKey "existential-constructor-forall"' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  should produce no matches.
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

- `orchestrator/rounds/round-316/selection-record.json`
- `orchestrator/rounds/round-316/round-plan-record.json`
